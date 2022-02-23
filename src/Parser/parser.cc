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
#define YYLAST   19883

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  175
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  289
/* YYNRULES -- Number of rules.  */
#define YYNRULES  987
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  2002

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
     600,   602,   615,   616,   626,   631,   636,   637,   643,   649,
     655,   657,   659,   661,   663,   665,   667,   669,   671,   673,
     675,   677,   679,   681,   683,   685,   687,   697,   698,   702,
     703,   708,   711,   715,   716,   720,   721,   723,   725,   727,
     729,   731,   736,   738,   740,   748,   749,   757,   760,   761,
     763,   768,   784,   786,   788,   790,   792,   794,   796,   798,
     800,   808,   809,   811,   815,   816,   817,   818,   822,   823,
     825,   827,   829,   831,   833,   835,   837,   844,   845,   846,
     847,   851,   852,   856,   857,   862,   863,   865,   867,   872,
     873,   875,   880,   881,   883,   888,   889,   891,   893,   895,
     900,   901,   903,   908,   909,   914,   915,   920,   921,   926,
     927,   932,   933,   938,   939,   942,   947,   952,   953,   961,
     967,   968,   972,   973,   977,   978,   982,   983,   984,   985,
     986,   987,   988,   989,   990,   991,   992,  1002,  1004,  1009,
    1010,  1012,  1014,  1019,  1020,  1026,  1027,  1033,  1034,  1035,
    1036,  1037,  1038,  1039,  1040,  1041,  1042,  1043,  1045,  1046,
    1052,  1054,  1059,  1061,  1069,  1070,  1075,  1077,  1079,  1081,
    1083,  1087,  1088,  1090,  1095,  1097,  1104,  1106,  1108,  1118,
    1120,  1122,  1127,  1132,  1135,  1140,  1142,  1144,  1146,  1154,
    1155,  1157,  1161,  1163,  1167,  1169,  1170,  1172,  1174,  1179,
    1180,  1184,  1189,  1190,  1194,  1196,  1201,  1203,  1205,  1207,
    1209,  1211,  1213,  1215,  1217,  1222,  1223,  1245,  1247,  1249,
    1252,  1255,  1258,  1260,  1262,  1264,  1267,  1270,  1272,  1275,
    1282,  1284,  1286,  1288,  1290,  1295,  1297,  1299,  1301,  1306,
    1308,  1313,  1315,  1317,  1319,  1322,  1326,  1329,  1333,  1335,
    1337,  1339,  1341,  1343,  1345,  1347,  1349,  1351,  1353,  1358,
    1359,  1363,  1369,  1374,  1379,  1380,  1384,  1388,  1393,  1394,
    1400,  1404,  1406,  1408,  1410,  1413,  1415,  1420,  1422,  1427,
    1429,  1431,  1436,  1438,  1444,  1445,  1449,  1450,  1451,  1452,
    1456,  1461,  1462,  1464,  1466,  1468,  1472,  1476,  1477,  1481,
    1483,  1485,  1487,  1489,  1495,  1496,  1502,  1503,  1507,  1508,
    1513,  1515,  1521,  1522,  1524,  1529,  1534,  1545,  1546,  1550,
    1551,  1557,  1558,  1562,  1564,  1568,  1570,  1574,  1575,  1579,
    1580,  1584,  1585,  1586,  1590,  1592,  1607,  1608,  1609,  1610,
    1612,  1616,  1618,  1622,  1629,  1631,  1633,  1638,  1639,  1641,
    1643,  1645,  1677,  1680,  1685,  1687,  1693,  1698,  1703,  1714,
    1719,  1724,  1729,  1734,  1743,  1747,  1754,  1756,  1757,  1758,
    1764,  1766,  1771,  1772,  1773,  1782,  1783,  1784,  1788,  1789,
    1790,  1799,  1800,  1801,  1806,  1807,  1816,  1817,  1822,  1823,
    1827,  1829,  1831,  1833,  1835,  1839,  1844,  1845,  1847,  1857,
    1858,  1863,  1865,  1867,  1869,  1871,  1874,  1876,  1878,  1883,
    1885,  1887,  1889,  1891,  1893,  1895,  1897,  1899,  1901,  1903,
    1905,  1907,  1909,  1911,  1913,  1915,  1917,  1919,  1921,  1923,
    1925,  1927,  1929,  1931,  1933,  1935,  1937,  1942,  1943,  1947,
    1954,  1955,  1961,  1962,  1964,  1966,  1968,  1973,  1975,  1980,
    1981,  1983,  1985,  1990,  1992,  1994,  1996,  1998,  2000,  2005,
    2006,  2008,  2010,  2015,  2017,  2016,  2020,  2028,  2029,  2031,
    2033,  2038,  2039,  2041,  2046,  2047,  2049,  2051,  2056,  2057,
    2059,  2064,  2066,  2068,  2070,  2071,  2073,  2078,  2080,  2082,
    2087,  2088,  2092,  2093,  2098,  2097,  2102,  2101,  2109,  2108,
    2119,  2118,  2128,  2133,  2134,  2139,  2145,  2159,  2160,  2164,
    2166,  2168,  2174,  2176,  2178,  2180,  2182,  2184,  2186,  2188,
    2194,  2195,  2200,  2202,  2204,  2213,  2215,  2216,  2217,  2219,
    2221,  2222,  2227,  2228,  2229,  2234,  2236,  2239,  2246,  2247,
    2248,  2254,  2259,  2261,  2267,  2268,  2274,  2275,  2279,  2284,
    2287,  2286,  2290,  2293,  2299,  2298,  2307,  2313,  2317,  2319,
    2324,  2326,  2328,  2330,  2336,  2339,  2345,  2346,  2348,  2349,
    2350,  2352,  2354,  2361,  2362,  2364,  2366,  2371,  2372,  2378,
    2379,  2381,  2382,  2387,  2388,  2389,  2391,  2399,  2400,  2402,
    2405,  2407,  2411,  2412,  2413,  2415,  2417,  2422,  2424,  2429,
    2431,  2440,  2442,  2447,  2448,  2449,  2453,  2454,  2455,  2460,
    2461,  2466,  2467,  2468,  2469,  2473,  2474,  2479,  2480,  2481,
    2482,  2483,  2497,  2498,  2503,  2504,  2510,  2512,  2515,  2517,
    2519,  2542,  2543,  2549,  2550,  2556,  2555,  2565,  2564,  2568,
    2574,  2580,  2581,  2583,  2587,  2592,  2594,  2596,  2598,  2604,
    2605,  2609,  2610,  2615,  2617,  2624,  2626,  2627,  2629,  2634,
    2636,  2638,  2643,  2645,  2650,  2655,  2663,  2665,  2670,  2671,
    2676,  2677,  2681,  2682,  2683,  2688,  2690,  2696,  2698,  2703,
    2705,  2711,  2712,  2716,  2720,  2724,  2726,  2727,  2728,  2733,
    2736,  2735,  2747,  2746,  2758,  2757,  2769,  2768,  2780,  2779,
    2793,  2799,  2801,  2807,  2808,  2813,  2820,  2825,  2831,  2834,
    2837,  2841,  2847,  2850,  2853,  2858,  2859,  2860,  2864,  2870,
    2871,  2881,  2882,  2886,  2887,  2892,  2897,  2898,  2904,  2905,
    2907,  2912,  2913,  2914,  2915,  2916,  2918,  2953,  2955,  2960,
    2962,  2963,  2965,  2970,  2972,  2974,  2976,  2981,  2983,  2985,
    2987,  2989,  2991,  2993,  2998,  3000,  3002,  3004,  3013,  3015,
    3016,  3021,  3023,  3025,  3027,  3029,  3034,  3036,  3038,  3040,
    3045,  3047,  3049,  3051,  3053,  3055,  3067,  3068,  3069,  3073,
    3075,  3077,  3079,  3081,  3086,  3088,  3090,  3092,  3097,  3099,
    3101,  3103,  3105,  3107,  3122,  3127,  3132,  3134,  3135,  3137,
    3142,  3144,  3146,  3148,  3153,  3155,  3157,  3159,  3161,  3163,
    3165,  3170,  3172,  3174,  3176,  3178,  3188,  3190,  3192,  3193,
    3195,  3200,  3202,  3204,  3209,  3211,  3213,  3215,  3220,  3222,
    3224,  3238,  3240,  3242,  3243,  3245,  3250,  3252,  3257,  3259,
    3261,  3266,  3268,  3273,  3275,  3292,  3293,  3295,  3300,  3302,
    3304,  3306,  3308,  3313,  3314,  3316,  3318,  3323,  3325,  3327,
    3333,  3335,  3337,  3340,  3344,  3346,  3348,  3350,  3384,  3385,
    3387,  3389,  3394,  3396,  3398,  3400,  3402,  3407,  3408,  3410,
    3412,  3417,  3419,  3421,  3427,  3428,  3430,  3439,  3442,  3444,
    3447,  3449,  3451,  3465,  3466,  3468,  3473,  3475,  3477,  3479,
    3481,  3486,  3487,  3489,  3491,  3496,  3498,  3506,  3507,  3508,
    3513,  3514,  3519,  3521,  3523,  3525,  3527,  3529,  3536,  3538,
    3540,  3542,  3544,  3547,  3549,  3551,  3553,  3555,  3560,  3562,
    3564,  3569,  3595,  3596,  3598,  3602,  3603,  3607,  3609,  3611,
    3613,  3615,  3617,  3624,  3626,  3628,  3630,  3632,  3634,  3639,
    3641,  3643,  3650,  3652,  3670,  3672,  3677,  3678
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

#define YYPACT_NINF (-1647)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-868)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
      45, 11810,   171,   202, 16232,   137, -1647, -1647, -1647, -1647,
   -1647, -1647, -1647, -1647, -1647, -1647, -1647,   109,   902,   122,
   -1647, -1647, -1647, -1647, -1647, -1647, -1647, -1647, -1647, -1647,
   -1647, -1647, -1647, -1647, -1647, -1647, -1647, -1647, -1647, -1647,
   -1647, -1647, -1647, -1647, -1647, -1647, -1647,    10,   307, -1647,
   -1647, -1647, -1647, -1647, -1647,  3892,  3892,   270, 11810,   312,
     334, -1647, -1647,   340, -1647, -1647, -1647, -1647, -1647, -1647,
   -1647, -1647, -1647,  2318, -1647,    30,   364, -1647, -1647, -1647,
   -1647, -1647, 16082, -1647, -1647,   374,   406,   507,   439, -1647,
    3892,   406,   406,   406,   402,  3937,   582,   899, 11969, -1647,
   -1647, -1647, 15932,  1267, -1647, -1647, -1647,  1392,   599,  6387,
     872,   845,  1392,   965,   463, -1647, -1647, -1647, -1647,   557,
   -1647, -1647, -1647, -1647,   517, -1647, -1647, -1647, -1647, -1647,
     591,   540,   557, -1647,   557,   586, -1647, -1647, -1647, 16788,
    3892, -1647, -1647,  3892, -1647, 11810,   606, 16840, -1647, -1647,
    4025, 17852, -1647,   855,   855,   641,  2064, -1647, -1647, -1647,
   -1647,   437, 14542,  2662,   557, -1647, -1647, -1647, -1647, -1647,
   -1647,   651, -1647,   642,   681,   688, -1647,   725, 19358, 15162,
    3724,  2318,   387,   694,   715,   721,   737,   739,   747, -1647,
   -1647, 16990, 10693,   786, -1647, 16375, -1647, -1647, -1647, -1647,
     789, -1647, -1647,   838, -1647,  7821,   922, 18710, -1647,   858,
    3892,   540,   864,   840,   843,   875, -1647, -1647, -1647,  2896,
    1917,   890,   944,   107, -1647, -1647,   557,   557,   104,   231,
     123,   104, -1647,   557,   557, -1647,  2691, -1647, -1647,   908,
     913,   855, 14120, -1647, -1647, 16082, -1647, -1647,  1392, -1647,
    2470,   463,   891,   999,   231,  3892,   507, -1647, 13518, -1647,
     855,   855,   907,   999,   231,  3892, -1647, 13267, -1647, -1647,
     855, -1647,   855, -1647,   610,  4341,  3892, -1647,  1546,   941,
   -1647, -1647, -1647, 16534,   540,   257, -1647, -1647, 17902, -1647,
     944,    73, -1647, 19358, 17852,  3387,  2691, -1647,   237, -1647,
   -1647, -1647, 16840,  3892, -1647,   934, -1647, -1647, -1647, -1647,
    3892,  3662,   233,   659, -1647,  3892,   642, -1647,   616,   557,
     940, 17042,   781, 14700, 14278,  1392,  1392, -1647,  1392,   855,
    1392,   855, -1647, -1647,   557, -1647,   945, -1647, 17192, -1647,
   -1647, -1647, 17244,   789, -1647,   953,   309,  1308,   968,   463,
     970, -1647,  2064,   949,   642,  2064,  2103, -1647,   979,  1023,
   19430,  1015,  1026, 19358, 19502,  1029, -1647, -1647, -1647, -1647,
   -1647, -1647, -1647, 19574, 19574, 15008,  1028,  4157, -1647, -1647,
   -1647, -1647,  1047, -1647,  1052, -1647,   982, -1647, 19358, 19358,
   -1647,  1022,   605,   625,   701,   473,   728,  1054,  1060,  1055,
    1103,    97, -1647,   699, -1647,  1087, -1647,   762,  3890, 15470,
   -1647, -1647,   632,  1087, -1647, -1647,   740, -1647, -1647,  3724,
    1090,  1093,  1100,  1105,  1107,  1111, -1647, -1647,   290,  1115,
   -1647,   509,  1115, -1647, -1647, 16788, -1647,   802,  1110, 15624,
   -1647, -1647,  4312,  3790,  1152, 14700,  1165,   490,   805, -1647,
   -1647, -1647, -1647, -1647,  3892,  4446, -1647, -1647, -1647, -1647,
   -1647, -1647,  1142,  4257,  1028,  7821,  1148,  1150, -1647, -1647,
    1147, 18710,   691, -1647, -1647, -1647, 18782,  1167, -1647, -1647,
   -1647, -1647, -1647,  2896,   778,  1178,  1180,  1182,   885,  1194,
    1198,  1212,  1917, -1647, -1647,   557,  1171,   507,  1215, -1647,
   -1647,  1184, -1647, -1647,   540,   999, -1647, -1647, -1647,   540,
   -1647, -1647,  2691, -1647, 15470, 15470, -1647,   855,  4025, 18630,
   14542, -1647, -1647, -1647, -1647, -1647,   540,   999,    73, -1647,
   -1647,  1392,  1214,   999,   231, -1647,   540,   999, -1647, 13410,
   -1647,   855,   855, -1647, -1647,  1221,   383,  1223,   463,  1226,
   -1647, 18060, -1647,   746, -1647,  1322, 18526, -1647,  4025, 17403,
   14120, -1647, 16534, 19646, -1647, -1647, -1647, -1647, -1647,  3387,
     903,  2691, -1647, 14542,   944, 11810, -1647,  1222, -1647,  1256,
   -1647, -1647, -1647, -1647, -1647,  2064, -1647, -1647,  1347,  4408,
   17244, 10693, -1647, 17455, -1647,   855,   855, -1647, -1647,   789,
   -1647,   673,  1274,  1415, 19358,  1063,  1184,  1266, -1647,   557,
     557, -1647,  1115, -1647, 17042, -1647, -1647, 18341,   855,   855,
   -1647,  4408,   557, -1647, 17709, -1647, -1647, 17192, -1647,   437,
    1290,   225,  1303,  1308,   754, 16840,   771, -1647, -1647, -1647,
   -1647, -1647, -1647,   808, -1647,  1323,  1306, -1647, 15316, -1647,
   17507, 17507, -1647, 15316, -1647, 19358, 15316, -1647, -1647, 16586,
   17507, 17507,   762,  1183,  1454,   461,  1499, -1647,   837,  1326,
     812,  1330, -1647, 18782, 19358, 18854,  1325,  1546,  1546, -1647,
    2202, -1647, -1647, 18926,  2382, 19358, 18926,  1546, -1647, -1647,
   19358, 19358, 19358, 19358, 19358, 19358, 19358, 19358, 19358, 19358,
   19358, 19358, 19358, 19358, 19358, 19358, 19358, 19358, 19358, 18998,
    1316,   725,  3266, 10693, -1647, -1647, -1647, -1647, -1647, -1647,
   -1647, -1647, -1647, -1647, -1647,  1336, 19358, -1647, -1647,   632,
    1131, -1647, -1647,   557,   557, -1647, -1647, 15470, -1647,   296,
    1115, -1647,   850,  1115, -1647, -1647, -1647,  1184, -1647, -1647,
    1184, 19718, -1647, -1647, 10693,  1343,  1346,  2304,  1484,  2560,
     332,  1266, -1647,   557,   557,  1266,   343, -1647,   557,   557,
   19358,  3892,   831,   888,  1266,   261, 14068, 14068,  3892, -1647,
   -1647, 19358,  1147, -1647,  7821,  1355, -1647,  2540, -1647, -1647,
   -1647, -1647, -1647,   849, -1647, 14068,  1546,  4025,  1546,   859,
    1353,  1354,  1356,   866,  1358,  1368,  1369,   363,  1115, -1647,
   -1647,   370,  1115, -1647, -1647, -1647,  4025,   725, -1647,  1115,
   19718, -1647,   540, 18060, -1647, -1647,   874,  1370,   882,  1371,
   -1647,  1375, -1647,   540, -1647, -1647,   540,   999,  1375, -1647,
     540,  1349,  1373,  1384, -1647, -1647, 18341, -1647,  1377, -1647,
   -1647, -1647,  1546,  3892,  9852,  1474,  1372, 18428, -1647,  1110,
   -1647, 14068,   914, -1647, -1647,  1375, -1647, 16840, 15470,  1374,
   -1647,  1374, -1647, -1647, -1647, -1647, 17192, -1647, 10855, 15778,
   -1647, 18060,  1393,  1398,  1399, -1647, 11571,   557, -1647,  1063,
   -1647, -1647, -1647, -1647,  1184, -1647, -1647, -1647,   855, -1647,
    3803, -1647, -1647,   463,  1672,  1403, -1647, 18710, -1647,  1308,
    1290, -1647, -1647,  1396,  1406,  2103, 18926, -1647,  1417,   364,
    1411,  1418,  1419,  1420,  1422, 19358,  1423,  1427,  1428, 10693,
   19358, -1647, -1647,  1521, -1647, -1647, -1647, 19358, -1647,  1430,
    1431,  9480,   994, -1647, 18926, -1647, -1647, -1647,  3034, -1647,
   -1647,   900, -1647, -1647, -1647, -1647,  3034, -1647, -1647,  1012,
     228, -1647, -1647,  1022,  1022,  1022,   605,   605,   625,   625,
     701,   701,   701,   701,   473,   473,   728,  1054,  1060,  1055,
    1103, 19358,  1062, -1647,  1432,  3034, -1647, -1647,  7821, -1647,
   18060,  1433,  1435,  1436,  1131, -1647, -1647, -1647, -1647, -1647,
   -1647, -1647, -1647,  1184, -1647, -1647,  1184, 18060, 18060, -1647,
   -1647,  2304,   932,  1437,  1439,  1441,  1443,  2641,  2560, -1647,
   -1647, -1647, -1647, -1647, -1647, -1647, -1647, -1647, -1647, -1647,
   -1647, -1647, -1647,  1442, -1647,  1266, -1647, -1647, -1647, -1647,
   -1647, -1647, -1647, -1647,  1449,  1456, -1647,   507,  3034,  1075,
      25, -1647, -1647,  1457, -1647, 18710, -1647, 19358, -1647, 19070,
   14068, -1647, -1647, -1647,  1444,   410,  1115, -1647,   416,  1115,
   -1647, -1647, -1647, -1647,  1184, -1647, -1647, -1647,  1184,   944,
    1461,  1184, -1647, -1647, -1647, -1647, -1647, -1647, -1647,  1470,
   -1647, -1647,  1375, -1647,   540, -1647, -1647, -1647, -1647, -1647,
   12599,  1468,  1466, -1647,   249, -1647,   692,    32, 10531,  1473,
   13897,  1475,  1476,  2090,  2448,  1246, 19142,  1488, -1647, -1647,
    1489,  1490, -1647, -1647,   540, 19358, 19358,  1627,  1486,   426,
   -1647,  1572,  1493,  1491, -1647, -1647, -1647,  9678, -1647, -1647,
   -1647, -1647, -1647,  2179, -1647, -1647, -1647,  1577, -1647, -1647,
   -1647,  1546, -1647, -1647, 12446, 16082,  1511, -1647,  3892, -1647,
    1494,  1515,  1516, -1647,  1083, -1647, -1647, -1647, -1647,  4025,
   -1647, -1647,  1500,  1503,   937, 16840,   642,   642, -1647, -1647,
    1028,  1110, 15624, -1647,  1087, -1647, 11017, -1647,   455,  1115,
   -1647,   855, 11647, -1647, -1647,  1308,   557,   557,   437,   225,
   -1647, -1647,  1290,  1525,  1532, -1647, -1647,   956,   654, 10693,
    1546, -1647,   654, 16638,   654, -1647, 19358, 19358, 19358, -1647,
   -1647, -1647, -1647, 19358, 19358,  1527,  7821, -1647, -1647,  1530,
     634, -1647,  4095, -1647, -1647,  1099, -1647,   245, -1647, 18926,
    1132, -1647, 18782, -1647, -1647, 19358,  1512,  1140,  1146,  1147,
   -1647,   457,  1115, -1647, -1647, 18060, 18060, -1647, -1647,  1536,
     502,  1115, -1647,   516,  1453,   557,   557, -1647, -1647, 18060,
   18060, -1647,  1534, -1647, 14542, 14542,  1538,  1539,  1540,  1542,
   -1647,  1543, 19358, 19358,  1154,  1545, -1647, -1647, -1647, -1647,
   -1647, -1647,  1550, 19358, -1647, -1647, -1647,  1184, -1647, -1647,
   -1647,  1184, 18060, 18060,   507,   557,  1164,  1551,  1555, -1647,
   -1647,  1556, 12752, 12905, 13058, 16840, 17507, 17507,  1560, -1647,
    1531,  1537,  1522, 13360, -1647,   475,  3892, -1647, -1647,  3892,
   -1647, 18638,   291,   295, -1647, -1647, -1647, -1647, 19358,  1564,
    1638, 10368, 10024, -1647,  1548, -1647,  1557, 19358,  1559,  7821,
    1563, 19358, 18782, 19358,  1061, -1647,  1565,   -34, -1647,    34,
    1580, -1647, -1647,  1569, -1647,  1566, -1647,  1567,  1581, 13897,
     572, 13676,   557,   480, -1647, -1647, -1647,  1583, -1647,  1590,
   -1647,  1596, -1647,  1591, -1647,  1594, -1647, -1647, -1647, -1647,
   11179,  1600,  1602,  1603, -1647,  1607, -1647, -1647, -1647,  1184,
   19358, 19358,  1110,  1606, -1647,  1290, -1647,  1605,    69, -1647,
    1616, -1647, -1647, 16840, -1647,  1615,  1618,   964, -1647,  1619,
   -1647, -1647, -1647, -1647, -1647,  7821,  1147, 18782, -1647,  1667,
    3034, -1647,  1667,  1667, -1647,  3034,  4207,  4228, -1647, -1647,
    1169, -1647, -1647, -1647,  1641,  1635, -1647, -1647, -1647,  1184,
   -1647, -1647,  1639,  1643,   557, -1647, -1647, -1647,  1184, -1647,
   -1647, -1647,  1644, -1647, -1647, -1647, -1647, -1647, -1647, -1647,
   -1647, -1647, -1647, -1647, -1647, -1647,  1647, -1647, -1647, -1647,
   -1647,  1648,  1646,   557, -1647, 18060, 18060, -1647, -1647, -1647,
   -1647, 19358, -1647, -1647,  1650, -1647,  1560,  1560,  1560,   678,
    1629,   489, -1647,  3823,   519, 15470, -1647, -1647, -1647,  3558,
   19358,  2883,   547, -1647, -1647,     5,  1636,  1636,  3892, -1647,
   -1647, 18209, -1647, 19358,  1652,  1657, -1647, -1647, -1647, -1647,
     977,  1660, 13897,  1493,  1659, 19358,   374,  1656,   402, 13217,
   16840, 13897, 19358, 19358,   719,   494, -1647, 19358, -1647, -1647,
     548, -1647,  1147, -1647,   986,  1000,  1005, -1647, -1647, -1647,
   -1647,   540,  1061,  1663, -1647, -1647, 19358, -1647,  1669,   725,
   10531, -1647, -1647, -1647, -1647, 19358,  1710, -1647,  9397, -1647,
     557, 14542, -1647, -1647, 16840, -1647, -1647, -1647, -1647, -1647,
    1670, -1647, 18060, -1647, -1647,  1671, -1647,  1673,  1680,  1664,
    1308, -1647, -1647, -1647, -1647, 19358, -1647, 16638, 19358,  1147,
    1682,  1172, -1647,  1181, -1647,  3034, -1647,  3034, -1647, -1647,
   -1647, -1647, 18060,  1683,  1687, -1647, -1647, 18060, 18060,  1693,
    1697,  1186, 14226, 14384, -1647,  1698, -1647, -1647, -1647, -1647,
    1700,  1702,  1189, -1647, -1647, -1647, -1647,   678,  1233,   568,
   -1647, -1647, -1647, -1647,   557,   557, -1647, -1647, -1647,   571,
   -1647,  1010,  3558,   602, -1647,  2883,   557, -1647, -1647, -1647,
   -1647, -1647, -1647, -1647, -1647,   583, 13897,   397, 19214, -1647,
   13897,  1493, 14858, -1647,  1493,  1684, -1647, -1647, -1647, -1647,
    6120, 19358, 13897, 10196,  1685, -1647,  1709,   433, 13897, -1647,
   -1647,  1711, -1647, -1647,  1701,   725,   579,  1720,  1721,  1205,
    1773, -1647, -1647, -1647, -1647,  3892,  4025, -1647, -1647,  1723,
    1725, -1647, -1647, -1647,  1308,  1290,  1726, -1647, -1647, -1647,
    1732, -1647, -1647, -1647,  1211,  1219, -1647, -1647, -1647, -1647,
   -1647, -1647, -1647, -1647, -1647, -1647,  1730, -1647, -1647,  1731,
    1733, -1647, -1647, -1647,  1737,  1738,  1739,  1233, -1647,   557,
   -1647, -1647, -1647, -1647, -1647,  1740,  3823, -1647, -1647,  5953,
     114, 11344, -1647, 13779, -1647,  1722,  1025,  1810, 19358,  1743,
   19358,   846,  1729,   213,  1827, -1647, 19358,  1734, 11505, -1647,
   -1647, -1647, 17657, -1647,  1750,  1742,    59, 13897, -1647, 19358,
   18926,   456, -1647, -1647, -1647,  1756, -1647, -1647,  1290,  1763,
   -1647, -1647, -1647, -1647,  1766,  1769,  1770, 14542,  1779, -1647,
   -1647,   563,  1115, -1647, -1647,   678, -1647, -1647,    36, -1647,
      92, -1647, -1647, -1647,  1776, 12128, -1647, -1647, -1647,    -4,
   13897, -1647,  1493,  1785,  1786, 19358, 19358, 19358, 13897, -1647,
   -1647,  1796, 12128, 17657, -1647,  3904, 17455,  1546,  1789, -1647,
    1846,  1799,   588,  1794, -1647,  1878, -1647,  1030, 13897,  1804,
   13897, 13897, -1647,  1806, -1647, -1647, -1647, -1647, -1647, -1647,
   -1647, -1647,  1184, -1647, 19358, -1647, 19358, -1647, -1647,  1312,
   12287, -1647, 13897, -1647, -1647,  1792,  1797,   365, -1647,  1493,
   -1647, -1647,  1312, -1647,  1787,  3190,  3371, -1647, -1647, -1647,
      59,  1811, 19358,  1790,    59,    59, 13897, -1647, -1647, 19358,
    1862,  1863, -1647, 18060, -1647, -1647, 13779, -1647,  1312, -1647,
   -1647, 19358, 19286, 19358, -1647,  1787, 19358,  1820,  3371,  1817,
     725,  1824, -1647,   608, -1647, -1647,  1035,  1773,   470, -1647,
   -1647,  9063,  1828, 13779,  1493, -1647,  1493,  1493,  1830,  1829,
   -1647,   540,   725,  1833, -1647,  1805,   725, -1647, -1647, 13897,
    1912,  1835, -1647, -1647, -1647,  9270, -1647,   540, -1647, -1647,
    1255, 19358, -1647,  1038, -1647, 13897, -1647, -1647,   725,  1546,
    1838,  1816, -1647, -1647, -1647,  1051, -1647, -1647,  1819,  1546,
   -1647, -1647
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,   414,     0,     2,   414,   431,   432,   433,   434,   435,
     436,   437,   438,   420,   422,   421,   423,     0,     0,     0,
     439,   441,   462,   442,   463,   445,   446,   460,   461,   440,
     458,   459,   443,   444,   447,   448,   449,   450,   451,   452,
     453,   454,   455,   456,   457,   464,   465,   751,   467,   540,
     541,   544,   546,   542,   548,     0,     0,     0,   414,     0,
       0,    16,   511,   517,     9,    10,    11,    12,    13,    14,
      15,   715,    93,     0,    19,     0,     2,    91,    92,    17,
      18,   767,   414,   716,   363,     0,   366,   641,   368,   377,
       0,   367,   397,   398,     0,     0,     0,     0,   494,   416,
     418,   424,   414,   426,   429,   479,   466,   402,   472,   477,
     403,   489,   404,   504,   508,   514,   493,   520,   532,   751,
     537,   538,   521,   587,   369,   370,     3,   717,   730,   419,
       0,     0,   751,   789,   751,     2,   806,   807,   808,   414,
       0,   965,   966,     0,     1,   414,     0,   414,   386,   387,
       0,   494,   408,   409,   410,   720,     0,   543,   545,   547,
     549,     0,   414,     0,   752,   753,   539,   468,   634,   635,
     633,   694,   689,   679,     0,     0,   718,     0,     0,   414,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   512,
     515,   414,   414,     0,   967,   494,   796,   814,   971,   964,
     962,   969,   362,     0,   155,   647,   154,     0,   371,     0,
       0,     0,     0,     0,     0,     0,   361,   866,   867,     0,
       0,   396,   749,   751,   745,   770,   751,   751,   747,     2,
     751,   746,   827,   751,   751,   824,     0,   487,   488,     0,
       0,   414,   414,   431,     2,   414,   378,   417,   427,   480,
       0,   509,     0,   733,     2,     0,   641,   379,   494,   473,
     490,   505,     0,   733,     2,     0,   430,   474,   481,   482,
     491,   496,   506,   510,     0,   524,     0,   709,     2,     2,
     731,   788,   790,   414,     0,     2,     2,   975,   494,   978,
     749,   749,     3,     0,   494,     0,     0,   389,   751,   747,
     746,     2,   414,     0,   713,     0,   675,   677,   676,   678,
       0,     0,   671,     0,   661,     0,   670,   681,     0,   751,
       2,   414,   986,   415,   414,   426,   405,   472,   406,   497,
     407,   504,   501,   522,   751,   523,     0,   622,   414,   623,
     940,   941,   414,   624,   626,   511,   517,     0,   588,   589,
       0,   754,     0,   692,   680,     0,   758,    21,     0,    20,
       0,     0,     0,     0,     0,     0,    23,    25,     4,     8,
       5,     6,     7,     0,     0,   414,     2,     0,    94,    95,
      96,    97,    78,    24,    79,    36,    77,    98,     0,     0,
     113,   115,   119,   122,   125,   130,   133,   135,   137,   139,
     141,   143,   146,     0,    26,     0,   518,     2,    98,   414,
     147,   686,   637,   508,   639,   685,     0,   636,   640,     0,
       0,     0,     0,     0,     0,     0,   768,   794,   751,   804,
     812,   816,   822,     2,   973,   414,   976,     2,    91,   414,
       3,   621,     0,   986,     0,   415,   472,   497,   504,     3,
       3,   603,   607,   617,   623,   624,     2,   797,   815,   963,
       2,     2,    23,     0,     2,   647,    24,     0,   645,   648,
     984,     0,     0,   654,   643,   642,     0,     0,   735,     2,
       2,     2,     2,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   773,   830,   751,     0,   641,     2,   769,
     777,   893,   771,   772,     0,   733,     2,   826,   834,     0,
     828,   829,     0,   392,   414,   414,   478,   415,     0,   494,
     414,   968,   972,   970,   495,   713,     0,   733,   749,   372,
     380,   428,     0,   733,     2,   713,     0,   733,   690,   475,
     476,   492,   507,   513,   516,   511,   517,   535,   536,     0,
     691,   414,   631,     0,   192,   355,   414,     3,     0,   494,
     414,   732,   414,     0,   374,     2,   375,   710,   394,     0,
       0,     0,     2,   414,   749,   414,   713,     0,     2,     0,
     674,   673,   672,   667,   425,     0,   665,   682,   470,     0,
     414,   414,   942,   415,   411,   412,   413,   946,   937,   938,
     944,     2,     2,    92,     0,   902,   916,   986,   898,   751,
     751,   907,   914,   629,   414,   502,   625,   415,   498,   499,
     503,     0,   751,   952,   415,   957,   949,   414,   954,     0,
     984,   594,     0,     0,     0,   414,     0,   766,   765,   761,
     763,   764,   762,     0,   756,   759,     0,    22,   414,    85,
     414,   414,    80,   414,    87,     0,   414,    83,    84,   414,
     414,   414,     2,    94,    95,     0,     0,   173,     0,     0,
     538,     0,   962,     0,     0,     0,     0,     0,     0,    46,
       0,    52,    53,    57,     0,     0,    57,     0,    81,    82,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   414,   156,   157,   158,   159,   160,   161,
     162,   163,   164,   165,   166,   154,     0,   152,   153,     2,
     878,   638,   875,   751,   751,   883,   519,   414,   795,   751,
     805,   813,   817,   823,     2,   798,   800,   802,     2,   818,
     820,     0,   974,   977,   414,     0,     0,     2,    92,   902,
     751,   986,   848,   751,   751,   986,   751,   863,   751,   751,
       3,   625,     0,     0,   986,   986,   414,   414,     0,     2,
     656,     0,   984,   653,   985,     0,   649,     0,     2,   652,
     655,   170,   169,     0,     2,   414,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   751,   782,   786,
     825,   751,   839,   844,   774,   831,     0,     0,   400,   890,
       0,   736,     0,   414,   737,   393,     0,     0,     0,     0,
     391,     2,   738,     0,   376,   713,     0,   733,     2,   739,
       0,     0,     0,     0,   550,   610,   415,     3,     3,   614,
     613,   809,     0,     0,   414,   356,     0,   494,     3,    91,
       3,   414,     0,     3,   714,     2,   669,   414,   414,   663,
     662,   663,   471,   469,   588,   948,   414,   953,   415,   414,
     939,   414,     0,     0,     0,   917,     0,   751,   987,   903,
     904,   630,   900,   901,   915,   943,   947,   945,   500,   535,
       0,   951,   956,   591,   985,     0,   154,     0,   590,     0,
     984,   695,   693,     0,     0,   758,    57,   719,     0,     2,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   414,
       0,   112,   111,     0,   108,   107,    27,     0,    28,     0,
       0,     0,     0,     3,    57,    42,    43,    50,     0,    49,
      61,     0,    58,    59,    62,    45,     0,    44,    48,     0,
       0,    41,   114,   116,   117,   118,   120,   121,   123,   124,
     128,   129,   126,   127,   131,   132,   134,   136,   138,   140,
     142,     0,     0,   365,     0,     0,    29,     3,   647,   148,
     414,     0,     0,     0,   879,   880,   876,   877,   688,   687,
       2,   799,   801,   803,     2,   819,   821,   414,   414,   895,
     894,     2,     0,     0,     0,     0,     0,   751,   903,   851,
     868,     2,   846,   854,   627,   849,   850,   628,     2,   861,
     871,   864,   865,     0,     3,   986,   384,     2,   979,     2,
     618,   619,   597,     3,     3,     3,     3,   641,     0,   146,
       0,     3,     3,     0,   650,     0,   644,     0,   734,     0,
     414,     3,   388,   390,     0,   751,   783,   787,   751,   840,
     845,     2,   775,   778,   780,     2,   832,   835,   837,   749,
       0,   891,     3,   741,     3,   484,   483,   486,   485,     2,
     714,   742,     2,   740,     0,   714,   743,   550,   550,   550,
     414,     0,     0,   632,     0,   359,     0,     0,   414,     0,
       2,     0,     0,     0,     0,     0,   175,     0,   289,   290,
       0,     0,   328,   327,     0,   150,   150,   334,   511,   517,
     189,     0,   176,     0,   200,   177,   178,   414,   194,   179,
     180,   181,   182,     0,   183,   184,   295,     0,   185,   186,
     187,     0,   188,   196,   494,   414,     0,   198,     0,   353,
       0,     0,     0,     3,     0,   721,   714,   702,   703,     0,
       3,   698,     3,     3,     0,   414,   679,   679,   950,   955,
       2,    91,   414,     3,   509,     3,   415,     3,   751,   910,
     913,   414,     3,   899,   905,     0,   751,   751,     0,   594,
     579,   595,   984,     0,     2,   755,   757,     0,    86,   414,
       0,    90,    88,   414,     0,   102,     0,     0,     0,   106,
     110,   109,   174,     0,     0,     0,   647,    99,   167,     0,
       0,    75,     0,    75,    75,     0,    63,    65,    40,     0,
       0,    38,     0,    39,   145,     0,     0,     0,     0,   984,
       3,   751,   886,   889,   881,   414,   414,     3,     3,     0,
     751,   857,   860,   751,     0,   751,   751,   852,   869,   414,
     414,   980,     0,   620,   414,   414,     0,     0,     0,     0,
     373,     3,     0,     0,     0,     0,   646,   651,     3,   172,
     171,     3,     0,     0,     2,   776,   779,   781,     2,   833,
     836,   838,   414,   414,   641,   751,     0,     0,     0,   714,
     744,     0,   414,   414,   414,   414,   414,   414,   533,   561,
       3,     3,   562,   494,   551,     0,     0,   791,     2,     0,
     357,    57,     0,     0,   280,   281,   197,   199,     0,     0,
       0,   414,   414,   276,     0,   274,     0,     0,     0,   647,
       0,     0,     0,     0,     0,   151,     0,     0,   335,     0,
       0,     3,   204,     0,   195,     0,   271,     0,     0,     2,
       0,   494,   751,     0,   354,   897,   896,     0,     2,     0,
     705,     2,   700,     0,   701,     0,   683,   664,   668,   666,
     414,     0,     0,     0,     3,     0,     2,   906,   908,   909,
       0,     0,    91,     0,     3,   984,   584,     0,   594,   592,
       0,   582,   696,   414,   760,     0,     0,     0,    32,     0,
     103,   105,   104,   101,   100,   647,   984,     0,    56,    72,
       0,    66,    73,    74,    51,     0,     0,     0,    60,    47,
       0,   144,   364,    30,     0,     0,     2,   882,   884,   885,
       3,     3,     0,     0,   751,     2,   853,   855,   856,     2,
     870,   872,     0,   847,   862,     3,     3,   981,     3,   605,
     604,   608,   983,     2,     2,   982,     0,     3,   748,   657,
     658,     0,     0,   751,   395,   414,   414,     3,     3,   401,
     750,     0,   841,   725,     0,   727,   533,   533,   533,   568,
     538,     0,   574,   562,     0,   414,   525,   560,   556,     0,
       0,     0,     0,   563,   565,   751,   576,   576,     0,   557,
     572,   414,   360,     0,     0,    58,   284,   285,   282,   283,
       0,     0,     2,   215,     0,     0,   217,   368,   216,   494,
     414,     2,     0,   175,   250,     0,   245,   175,   277,   275,
       0,   269,   984,   278,     0,     0,     0,   316,   317,   318,
     319,     0,   309,     0,   310,   286,     0,   287,     0,     0,
     414,   206,   193,   273,   272,     0,   307,   326,     0,   358,
     751,   414,   723,   684,   414,     2,     2,   958,   959,   960,
       0,   911,   414,     3,     3,     0,   919,     0,     0,     0,
       0,   593,   581,     3,    89,     0,    31,   414,     0,   984,
       0,     0,    76,     0,    64,     0,    70,     0,    68,    37,
     149,   887,   414,     0,     0,   792,   810,   414,   414,     0,
       0,     0,   414,   414,   660,     0,   381,   383,     3,     3,
       0,     0,     0,   729,   529,   531,   527,     0,   926,     0,
     569,   931,   571,   923,   751,   751,   555,   575,   559,     0,
     558,     0,     0,     0,   578,     0,   751,   552,   566,   577,
     567,   573,   612,   616,   615,     0,     2,     0,     0,   236,
       2,   218,   494,   242,   251,     0,   266,   267,   268,   265,
     254,     0,     2,   414,     0,   270,     0,     0,     2,   293,
     320,     0,   311,     2,     0,     0,     0,     0,   298,     0,
     294,   191,   190,   382,   699,     0,     0,   961,     3,     0,
       0,   918,   920,   583,     0,   984,     2,    35,    33,    34,
       0,    54,   168,    67,     0,     0,     3,   793,   811,     3,
       3,   858,   873,   385,     2,   602,     3,   601,   659,     0,
       0,   784,   842,   892,     0,     0,     0,   927,   928,   751,
     554,   924,   925,   553,   534,     0,     0,   205,   292,     0,
       0,     0,   229,     2,   207,     0,     0,   237,   175,   259,
       0,   255,     0,   252,   243,   246,   175,     0,     0,   210,
     291,     2,   414,   288,     0,     0,   336,     2,   296,     0,
      57,     0,   308,   704,   706,     0,   921,   922,   984,     0,
     697,    55,    71,    69,     0,     0,     0,   414,     0,   785,
     843,   751,   934,   936,   929,     0,   564,   224,   219,   222,
       0,   221,   228,   227,     0,   414,   231,   230,   239,     0,
       2,   247,   256,   267,   265,     0,   175,     0,     2,   249,
     279,     0,   414,   414,     3,   321,   415,   325,     0,   329,
       0,     0,     0,   337,   338,   213,   299,     0,     2,     0,
       2,     2,   912,     0,   586,   888,   859,   874,   606,     2,
     930,   932,   933,   570,     0,   226,     0,   225,   209,   232,
     414,   349,     2,   240,   238,   261,   260,   257,   248,   253,
     244,   212,   232,     3,   314,     0,   926,   322,   323,   324,
     336,     0,     0,     0,   336,     0,     2,   297,   304,     0,
     301,   303,   585,   414,   220,   223,     2,     3,   233,   350,
     241,     0,     0,     0,     3,   314,     0,     0,   927,     0,
       0,     0,   330,     0,   339,   214,     0,   294,     0,     3,
     201,     0,     0,     2,   263,   264,   262,   258,     0,     0,
     315,     0,   342,     0,   340,     0,   342,   300,   302,     2,
       0,     0,   203,   202,   208,     0,   211,     0,   312,   343,
       0,     0,   331,     0,   305,     2,   935,   313,     0,     0,
       0,     0,   306,   344,   345,     0,   341,   332,     0,     0,
     333,   346
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1647,  5298,  5399, -1647,    -1,   381,  1668,  -147, -1647,  1614,
   -1647,   388, -1647,  -671,   666,   760,  -883, -1007, -1647,   197,
    6468,  1747, -1647,   925, -1647,  1337,   258,   743,   753,   788,
     750,  1297,  1298,  1301,  1302,  1296, -1647,  -165,  -159,  8072,
     886, -1647,  -390, -1647, -1647,  -647,  2941, -1068,  1339, -1647,
     265, -1647,   876,    61, -1647, -1647, -1647,   445,   131, -1647,
   -1574, -1528,   322,   128, -1647, -1647, -1647,   338,   253, -1647,
   -1647, -1647, -1647,    88, -1611,   243, -1647, -1647,    91, -1647,
   -1647, -1647,   110,   485,   486,   200, -1647, -1647, -1647, -1647,
    -703, -1647,   135,    89, -1647,   206, -1647,  -176, -1647, -1647,
   -1647,   896,  -808,  -799, -1227, -1647,    27, -1003,   230,  4294,
    -695,  -606, -1647,  -271, -1647,   174,  -135,   142,   -92,  -224,
    3620,  6516,  -634, -1647,   185,    50,   696,    54, -1647,  2008,
   -1647,   120,  3917, -1647, -1647, -1647,   106, -1647, -1647,  2030,
     267,  4088,  2535,   -37,  1809,  -300, -1647, -1647, -1647, -1647,
   -1647,  -809,  1861,  3432, -1647,  -347,   -26, -1647,   559,   297,
   -1647,   239,   755, -1647,   554,   -74, -1647, -1647, -1647,  4991,
    -629, -1149,  -665,  -480,   255,  1216, -1647, -1216,  -155,   199,
    1965,   923,  7218,   -22,  -474,  -235,  -136,  -460,  1292, -1647,
    1609,  -168,  1213,  1498, -1647, -1647, -1647, -1647,   299,  -171,
     -49,  -844, -1647,   316, -1647, -1647,   674,   504, -1647, -1647,
   -1647,  2091,  -761,  -384,  -959,   -19, -1647, -1647, -1647, -1647,
   -1647, -1647,   204,  -792,  -103, -1646,  -206,  7428,   -65,  5702,
   -1647,  1175, -1647,  1568,  -214,  -199,  -197,  -177,     8,   -72,
     -62,   -59,   505,    -9,    -5,     1,  -170,   -70,  -163,  -114,
     -82,  -715,  -721,  -670,  -662,  -707,   -35,  -644, -1647, -1647,
    -696,  1363,  1364,  1365,  1870,  7024,  -562,  -591,  -541,  -518,
    -649, -1647, -1475, -1584, -1582, -1571,  -585,   -83,  -305, -1647,
   -1647,   -47,    66,   -53, -1647,  7256,    38,  -503,  -315
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,  1131,   213,   382,   383,    80,    81,   384,   359,   385,
    1417,  1418,   386,   951,   952,   953,  1235,  1236,  1237,  1429,
     408,   388,   389,   390,   665,   666,   391,   392,   393,   394,
     395,   396,   397,   398,   399,   400,   401,   410,  1050,   667,
    1356,   726,   207,   728,   404,   793,  1132,  1133,  1134,  1135,
    1136,  1137,  1138,  1951,  1139,  1140,  1361,  1534,  1829,  1830,
    1772,  1773,  1774,  1927,  1928,  1141,  1545,  1546,  1691,  1142,
    1143,  1144,  1145,  1146,  1147,  1369,  1709,  1871,  1802,  1148,
    1149,  1562,  1937,  1563,  1564,  1854,  1150,  1151,  1152,  1359,
    1862,  1863,  1864,  1980,  1995,  1889,  1890,   284,   285,   854,
     855,  1104,    83,    84,    85,    86,    87,    88,   441,    90,
      91,    92,    93,    94,   221,   558,   443,   412,   444,    97,
     294,    99,   100,   101,   324,   325,   104,   105,   166,   106,
     873,   326,   152,   109,   241,   110,   153,   250,   328,   329,
     330,   154,   405,   115,   116,   332,   117,   549,   843,   841,
     842,  1506,   333,   334,   120,   121,  1100,  1324,  1512,  1513,
    1649,  1650,  1325,  1501,  1668,  1514,   122,   632,  1599,   335,
     630,   908,  1043,   449,   450,   847,   848,   451,   452,   849,
     337,   553,  1156,   414,   415,   208,   469,   470,   471,   472,
     473,   313,  1176,   314,   871,   869,   583,   315,   353,   316,
     317,   416,   124,   172,   173,   125,  1170,  1171,  1172,  1173,
       2,  1089,  1090,   575,  1165,   126,   304,   305,   252,   262,
     532,   127,   211,   128,   222,  1052,   834,   499,   164,   129,
     643,   644,   645,   130,   224,   225,   226,   227,   299,   132,
     133,   134,   135,   136,   137,   138,   230,   300,   232,   233,
     234,   761,   762,   763,   764,   765,   235,   767,   768,   769,
     731,   732,   733,   734,   500,   139,   607,   608,   609,   610,
     611,   612,  1652,  1653,  1654,  1655,   597,   454,   340,   341,
     342,   417,   199,   141,   142,   143,   344,   785,   613
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      79,   183,   354,    79,   910,   782,   493,   336,   181,   131,
     882,   184,   557,   402,   185,   960,   497,   516,   727,   403,
     485,   529,   486,   615,  1174,   231,   942,   322,   670,   896,
     358,   148,   935,   625,   995,   198,  1013,   628,   190,   176,
     827,   829,   487,   890,  1019,  -707,  1153,   297,  1350,   488,
    1409,   103,  1020,   505,    79,    79,   489,    79,  1469,  1470,
     883,  1566,  1157,  1754,   186,  1755,   131,   140,   187,  1892,
     140,   475,    79,  1240,   188,    57,  1756,  1095,   527,   339,
      57,    79,   493,   884,   564,   566,   289,  1014,   537,    79,
     196,  1332,  1333,  1885,    79,  1015,   485,    79,   486,  1801,
     198,    79,  1247,   228,  1166,   490,   253,   111,   103,   420,
     263,  1023,  1046,  1016,  1536,  1832,  1568,  1030,   487,   421,
     201,   107,   422,  1831,   140,   488,   292,   905,   350,   256,
    1061,  1308,   489,   513,   189,    63,  1311,   491,   436,    79,
    1567,   831,    79,    96,    79,   615,   150,   183,   248,    79,
     494,   838,   249,   131,   483,    79,  1282,   184,   498,   162,
     185,   912,    79,   269,   111,  1281,   882,  1884,   140,   496,
    1893,   144,   423,  1758,   357,    95,   424,    57,   107,    79,
      79,   490,   425,  1569,   634,   201,   102,   636,   196,   279,
    1283,   209,   865,    57,    79,   103,  1163,   890,   522,  1837,
      96,  1283,  -708,   457,   466,   249,  1334,  1379,   260,    79,
     186,   140,  1860,   491,   187,   204,   883,   418,    79,    79,
     188,   274,   565,   183,   194,  1431,   494,   708,   196,   592,
     569,    89,    95,   184,   149,    79,   185,   544,   598,   884,
    1831,   907,   906,   102,    79,  1207,   623,  1886,  1887,   249,
     626,   111,   155,   196,    79,   522,   278,    79,   156,  1601,
     498,  -733,   818,   999,    79,   107,   533,   587,   112,   709,
    1833,   161,   506,  1230,    79,    79,   498,    79,   814,  1053,
    1202,   287,  1824,   523,   800,   877,   801,    96,    89,   194,
    1013,   858,   891,   615,    79,    79,  1044,  1044,  1254,  1221,
    1336,  1319,    79,  1267,   196,   587,   802,    57,   531,    79,
      79,  1268,   249,   803,    79,  1044,  1337,   615,  -351,    95,
     804,  1754,   902,  1755,   615,   112,    19,  1194,   246,  1153,
     102,  1309,   257,   434,  1756,   786,  1801,   958,  1537,  1537,
     523,  1014,   249,  1536,  -352,  1157,    79,   592,   249,  1015,
    1494,    79,  1837,   671,    79,   642,   598,   814,   837,   600,
      57,  1926,   198,  1023,   580,  1436,    57,  1259,   937,   805,
     800,   204,   801,   248,  1926,    89,  1243,   249,  1847,  1837,
    1908,  1044,   753,  1239,   518,   146,   572,   521,  -351,   111,
     498,   453,   802,   581,   582,  1291,   280,  1437,   906,   803,
    1953,   806,    57,   107,  1329,  1320,   804,  1203,   766,   825,
     565,   420,   112,    57,  -352,   830,  1469,  1470,    79,   175,
     882,   421,   815,  1330,   422,    96,   190,   457,  1614,  1616,
    1618,  1758,   504,    57,   604,   509,   170,   170,   322,   744,
      57,    79,    79,   498,   521,  1000,  1024,   201,   279,   498,
    1027,  1092,   279,    79,    79,   805,  1194,   526,   179,  1040,
    1041,   177,    79,  1824,   466,  1526,  -867,   536,   560,  1528,
     883,   170,  1769,  1770,   423,  1685,   478,   600,   424,  1694,
      57,  1021,    79,   178,   425,   602,    57,   806,   530,   179,
     339,    79,  1028,   884,  1321,   248,   602,   420,   875,   249,
     457,   815,    13,    14,    15,    16,    17,   421,  1769,  1770,
     422,    79,  1071,  1319,  1319,  1319,   498,    79,   191,  1075,
     937,   170,   895,   498,   170,    57,   209,    57,  1249,  1868,
    1933,   598,   179,  1535,  1547,   901,   278,   170,   426,  1398,
    -530,    62,    63,  1969,   348,  1368,   615,  1613,   202,   561,
     112,   194,   418,   418,  1771,    79,   864,    79,  1121,  1294,
      57,    -3,  1869,   498,   984,  1298,  1405,  1537,    79,   498,
      79,  1179,    57,   249,    79,   179,   216,   752,   615,   457,
    1044,   538,  -635,   131,    79,   236,    57,   197,    79,    75,
    1788,   170,   550,   249,   210,  1440,  -733,   896,   698,   699,
     229,  1878,  -408,   254,  1396,   402,  1446,   264,   602,   248,
     498,  1034,   930,   249,  1461,   274,  1049,  1320,  1320,  1320,
      79,  -412,  1782,   931,   932,   103,   918,    57,   920,   921,
    1518,   922,    79,    57,   924,  1329,   170,   926,   927,   928,
     669,   140,   700,   701,  1657,  1692,   170,   249,  1054,  1519,
    1693,  1455,   897,   204,  1579,   498,   547,   170,   748,   552,
    1524,  1661,   498,  1658,   205,  1459,   260,   531,   111,   602,
    1080,   249,   276,   418,  1518,   544,    79,    79,   249,    79,
     206,   111,   107,    79,   170,   197,    79,  1644,  1645,  1646,
    1537,   170,   170,  1660,  1063,   107,   170,   279,   150,  1410,
     458,  1387,  1666,   937,    96,  1555,  1321,  1321,  1321,   822,
    1841,    79,  1879,  1079,   543,    63,   498,    96,  1849,   453,
    1273,  1667,  1695,  1759,   766,   197,  1666,   852,   170,   279,
    1795,   833,  1734,   170,  1735,  1796,   170,   836,   937,  1913,
     278,   840,  1760,  -351,  1914,  1763,  1444,   560,   157,    95,
     197,   158,   159,   426,   160,   498,    79,  1767,    79,  1965,
     102,    72,  1535,   534,  1966,   691,  1426,   588,   274,   821,
      79,  1201,   692,   693,   824,   418,   879,    79,  1898,   293,
    1611,   729,  1428,   466,  1178,   498,    79,   694,   695,  1239,
     453,   832,    77,    78,   322,    79,    79,    79,  -722,   266,
     352,   839,    72,   267,  1225,    89,   270,    72,   272,   907,
     584,  1226,  1280,   311,   585,    79,  1244,    13,    14,    15,
      16,    17,   601,   170,   696,   697,   602,  1647,   863,   112,
     355,   498,  1537,    77,    78,   170,   170,   356,    77,    78,
     357,  1331,   112,   787,   788,   427,   339,   789,  -409,  1537,
     710,    79,    79,   466,   711,   702,   703,    13,    14,    15,
      16,    17,  1686,  1687,  1688,  1197,   428,    13,    14,    15,
      16,    17,   429,  1304,   937,    57,   243,     6,     7,     8,
       9,    10,    11,    12,  1689,  1547,   615,  1537,   430,  1552,
     431,   736,  1049,  1690,  1167,   737,   248,   851,   432,    79,
     249,   852,  1598,    79,   103,   911,   418,   531,    79,   585,
      72,   249,   453,   669,   642,    57,   191,   673,   669,  1287,
     140,   669,   913,  1610,   458,    57,   585,   278,   248,   426,
     601,   498,   249,   140,   602,   456,  -413,   146,   460,   879,
     669,    77,   603,   474,   266,   237,   238,    79,   239,   963,
     964,   965,   240,   453,   604,    79,   590,   673,   157,   914,
     111,   158,   159,   915,   160,  1609,   170,  1400,  -410,  1500,
     874,  1725,   939,   940,   107,   453,   453,    13,    14,    15,
      16,    17,  1266,   766,    79,  1036,  1037,   466,   936,  1686,
    1843,  1688,   937,   461,   453,   480,    96,   458,   481,  1004,
    1058,   937,   899,   498,  1059,   354,   354,   476,   278,  1169,
      79,  1844,   498,   479,   170,   506,    79,    79,   907,   498,
    -176,   266,   267,  -399,   619,  1085,   272,  1891,    95,   937,
     482,  1045,  1045,  1087,   506,    57,   810,   937,   498,  1155,
     496,  1094,  1038,  1039,  1891,   495,  -399,    79,   525,  1696,
    1045,  1238,   572,  1326,   426,  1239,   498,   514,   945,   946,
     453,   949,   515,   572,   535,   957,  1380,   498,   961,  1489,
    1538,  1312,  1313,  1314,   322,    13,    14,    15,    16,    17,
    1441,  1021,  1929,   426,    89,   602,   209,  1083,  1386,   554,
     895,   576,   737,   986,   590,  1808,   622,  1168,  1091,   598,
     679,  1093,   680,   681,   682,  1096,  1730,  1414,  1516,   466,
    -866,  1239,    79,    79,    79,  1606,  1045,   402,   402,  1607,
    1471,   112,   635,  1477,  1478,  -580,   339,   633,  1677,  1867,
     646,   683,   937,    57,   684,   685,   466,  1697,   647,   686,
     687,   937,    79,    13,    14,    15,    16,    17,  1228,  1059,
      79,  1698,   170,    79,    79,  1059,  1699,    79,   103,   170,
     937,  1764,   253,   263,   650,   737,  1241,  1242,    79,  1557,
    1558,  1559,  1560,  1561,   140,   651,  1839,  1062,   655,  1064,
     937,  1917,   256,   673,   863,  1239,  1967,   103,   690,  1991,
     937,  1419,    72,  1988,    79,  -112,  -112,  -112,  -112,  -112,
    -112,    57,  1998,   140,   248,   677,  1999,  1939,   249,    79,
     678,  1943,   601,   418,   111,   704,   602,   937,  1245,   705,
     897,   140,  1809,    77,   603,   466,   706,   266,   107,  -147,
    -147,    79,   707,  1103,   170,   170,   531,  1038,  1378,   712,
     249,   738,  1322,   111,   739,    13,    14,    15,    16,    17,
      96,   740,  1517,  1434,  1435,   322,   741,   107,   742,   453,
      72,   260,   743,    79,    -3,  1326,  1326,  1326,   433,  1502,
    1326,   243,     6,     7,     8,     9,    10,    11,    12,    96,
     729,  1196,    95,   770,   498,   170,  1439,  1435,   652,  1516,
     170,    77,    78,  1155,  1443,  1435,  -411,   493,   -16,  1538,
    1010,  1427,   784,    57,   -17,  1873,   783,   339,  1479,  1427,
     485,    95,   486,   688,   689,  1045,   794,   629,  1010,  1491,
     817,    79,  1155,  1619,  1059,    79,  1732,  1059,    79,   807,
     265,   808,   487,   809,   688,  1733,  1435,   286,    89,   488,
    1743,  1744,   148,  1753,   937,   811,   489,    61,   466,   812,
     168,   169,    64,    65,    66,    67,    68,    69,    70,  1310,
    1799,  1800,    72,   813,   688,  1812,  1435,    89,   466,   819,
      79,   835,  1335,  1813,  1435,   112,   866,   249,  -528,   533,
    -526,   140,  1647,   844,   246,   257,   498,  1769,  1770,  1354,
     853,   103,   103,    77,    78,   490,   243,     6,     7,     8,
       9,    10,    11,    12,   112,   868,  1347,   140,   140,    61,
    1988,  1989,   168,   169,    64,    65,    66,    67,    68,    69,
      70,   531,  1706,   872,   466,   249,  1471,   491,   885,    79,
    1432,  1433,   887,  1517,    79,    79,    79,   966,   967,   604,
    1167,   494,  1669,  1669,  1662,   904,   322,   111,   111,   968,
     969,   814,   974,   975,  1322,  1322,  1322,   150,  1499,  1503,
     909,   107,   107,   800,   863,   801,  -111,  -111,  -111,  -111,
    -111,  -111,   916,   453,   453,  1388,  1389,   938,  1471,   140,
     917,   941,   944,    96,    96,   802,   970,   971,   972,   973,
     983,   170,   803,   988,   170,   170,   170,  1009,   339,   804,
    1010,  1017,    79,  1056,  1065,  1066,  1097,  1067,    79,  1068,
      79,    13,    14,    15,    16,    17,   934,    79,   170,  1069,
    1070,  1086,  1088,  -711,   170,   236,  1540,  1540,  -611,  1468,
    1098,   466,   552,    13,    14,    15,    16,    17,  1220,   170,
     466,  1099,  1158,   418,  1188,   149,  1159,  1175,   805,  1189,
    1190,  1200,  1516,  1204,    61,  1169,   615,  1205,  1794,    64,
      65,    66,    67,    68,    69,    70,  1210,   256,  1208,  1211,
    1212,    89,    89,  1214,  1216,  1213,   170,   466,  1217,  1218,
     806,  1223,  1224,  1246,  1251,  1419,  1252,  1253,  1260,   248,
    1261,  1415,  1262,   249,  1263,   815,  1271,  1527,  1529,    79,
    -599,   530,  1264,    74,   402,  1286,   140,  -598,   112,   112,
    1828,  1167,  1305,  1804,    79,   962,    79,  1293,  -712,  1327,
     103,  1328,  1338,    61,  1341,  1342,   217,   218,    64,    65,
      66,    67,    68,    69,    70,  1577,   140,  1351,  1352,  1353,
    1358,   182,  -634,  1168,  1360,   863,   260,    61,   937,  1861,
     140,    72,    64,    65,    66,    67,    68,    69,    70,   254,
     264,    79,  1471,   223,    79,  1362,  1368,  1372,  1374,  1375,
    1376,  1509,    74,  1411,  1382,   466,   111,  1384,  1510,   466,
    1412,  1198,    77,    78,  1425,  1427,  1442,  1454,  1467,  1472,
     107,   466,   493,  1475,  1473,  1474,  1517,   466,  1435,  1480,
    1855,  1483,  1492,  1493,  1495,  1507,   485,   170,   486,  1505,
     170,  1508,    96,  1331,    79,    79,  1531,  1572,   298,   402,
    1673,   402,  1548,    79,   814,  1924,  1169,  1828,   487,  1570,
    1575,  1549,   531,  1551,  1580,   488,   249,  1553,  1582,  1565,
    1573,  1574,   489,   103,  1583,  1857,  1585,   402,    82,  1586,
     170,   147,  1907,  1941,  1587,  1540,  1588,  1589,  1591,   140,
    1596,  1855,  1600,  1861,  1602,    79,  1604,  1861,  1861,   246,
     257,  1960,   466,    61,  1605,  1608,   168,   169,    64,    65,
      66,    67,    68,    69,    70,  1612,  1621,   484,   223,  1620,
    1625,   490,  1510,  1963,  1626,   426,   466,  1636,  1643,   111,
      89,  1634,  1479,  1656,   298,    82,  1857,  1239,  1676,  1678,
    1680,   210,  1703,   107,  1168,  1979,   402,  1710,  1705,  1979,
     180,  1724,  1990,   491,  1717,  1721,  1700,  1722,  1723,    82,
    1731,   453,   453,   183,  1737,    96,   494,   112,  1738,   466,
     569,  1993,   220,   184,  1741,   245,   185,   466,  1742,    82,
    1215,  1751,  1748,  1752,    79,  1219,    79,  1787,  1778,  1786,
    1791,  1121,   118,   570,   298,   118,  1227,   466,   815,   466,
     466,  1797,  1798,   467,  1810,  1793,   534,  1806,  1540,  1807,
    1811,  -600,  1819,  1840,  1820,   103,   147,  1746,  1821,  1822,
    1823,   466,    82,   498,   147,  -511,  1838,   296,   302,   170,
    1848,   140,   103,  1846,    79,    79,  1858,  1872,  1850,   321,
     249,  1874,   530,   170,   196,   466,  1859,  1875,   140,   118,
    1876,  1877,   170,    89,  1888,   466,   409,   180,   180,    13,
      14,    15,    16,    17,  1744,  1895,  1896,    79,   147,   439,
     103,   111,   245,   118,  1901,  1910,   457,  1911,  1912,  1915,
     466,  1916,   466,  1919,  1922,   107,   140,  1931,   111,   170,
     112,  1936,  1932,   118,  1942,  1940,   220,   220,   466,  1947,
    1948,  1961,   107,  1962,   466,  1964,  1974,    96,  1976,  1982,
    1977,   170,  1981,   296,   466,  1985,  1986,    57,    79,  1996,
    1997,   676,    82,  2000,    96,  1728,   111,  1525,    79,  1438,
     118,   976,   933,   977,   980,   245,   118,   978,   118,   979,
     107,   760,  1357,  1364,  1975,  1707,   453,  1925,    61,  1789,
    1540,   217,   218,    64,    65,    66,    67,    68,    69,    70,
    1934,  1785,    96,   652,  1845,   302,  1970,  1540,  1968,  1084,
     118,   302,   296,   296,  1870,  1959,    72,  1701,  1702,   147,
    1944,   799,   118,  1903,  1373,  1983,   167,   170,  1902,   524,
     223,   170,  1659,  1826,  1883,    89,   219,    74,   321,   605,
     614,  1670,  1504,   170,  1370,  1540,  1055,    77,    78,   170,
     298,   790,    89,   870,  1177,   321,   298,  1603,  1714,   321,
    1206,     3,   991,   992,   993,     0,   170,  1187,     0,     0,
     508,     0,   112,   118,     0,   170,   118,   688,     0,     0,
       0,   118,     0,     0,     0,   637,     0,     0,     0,   112,
      89,     0,   409,     0,     0,     0,   298,   306,   307,   308,
     309,   780,     0,   467,     0,     0,     0,   862,     0,   298,
       0,  1420,  1421,  1422,   118,     0,     0,     0,  1423,  1424,
       0,     0,     0,     0,   170,     0,   409,   112,     0,   730,
       0,     0,     0,   118,     0,    61,   180,     0,   168,   169,
      64,    65,    66,    67,    68,    69,    70,     0,   170,     0,
       0,     0,   147,     0,     0,     0,   439,     0,     0,   638,
     759,    61,   614,     0,   168,   169,    64,    65,    66,    67,
      68,    69,    70,     0,   639,     0,  1250,   640,   641,    64,
      65,    66,    67,    68,    69,    70,     0,   310,     0,     0,
       0,   170,   447,  1257,  1258,     0,  1978,     0,     0,   170,
     220,     0,     0,     0,     0,   311,   118,     0,  1909,   220,
       0,     0,  1987,     0,     0,     0,     0,     0,     0,   170,
       0,   170,   170,     0,     0,  1365,     0,     0,     0,   296,
       0,   409,   409,     0,  1343,   296,     0,   321,     0,     0,
     118,     0,     0,   170,     0,     0,     0,     0,     0,     0,
      61,     0,   735,   168,   169,    64,    65,    66,    67,    68,
      69,    70,     0,     0,     0,     0,   118,   170,   746,     0,
       0,   749,     0,    61,     0,   296,     0,   170,    64,    65,
      66,    67,    68,    69,    70,   947,   296,     0,   296,     0,
     321,     0,    82,     0,     0,  1012,     0,   760,     0,     0,
       0,     0,   170,     0,   170,     0,     0,   321,   439,     0,
     614,     0,     0,     0,     0,     0,     0,     0,   605,     0,
     170,   595,   605,  1366,   618,   948,   170,     0,   508,     0,
       0,   321,     0,     0,     0,   298,   170,     0,   595,     0,
    1994,   614,   595,     0,   321,   118,   118,     0,     0,     0,
    2001,     0,   147,     0,   298,     0,     0,     0,    57,     0,
       0,     0,     0,     0,     0,   409,     0,   147,   147,    18,
     409,     0,     0,   409,     0,    61,   147,   147,   147,     0,
      64,    65,    66,    67,    68,    69,    70,   118,     0,    61,
       0,   118,     0,   118,    64,    65,    66,    67,    68,    69,
      70,     0,     0,    72,     0,     0,   118,     0,     0,     0,
       0,    51,    52,    53,    54,     0,     0,    72,     0,  1340,
       0,     0,   467,  1011,    74,   780,     0,   602,     0,     0,
     439,     0,     0,     0,    77,    78,     0,    73,    74,   595,
       0,  1450,  1451,     0,     0,     0,   730,   730,    77,    78,
       0,     0,     0,    61,   409,  1465,  1466,    18,    64,    65,
      66,    67,    68,    69,    70,   955,   118,     0,     0,     0,
    1708,   439,     0,     0,   759,     0,   759,     0,     0,   118,
       0,   118,   118,     0,   118,     0,   850,   118,  1487,  1488,
     118,   118,   118,   321,   321,    47,    48,    49,    50,    51,
      52,    53,    54,     0,     0,   956,   114,     0,     0,   114,
       0,     0,   321,     0,   296,     0,     0,     0,     0,    61,
     447,     0,   168,   169,    64,    65,    66,    67,    68,    69,
      70,     0,     0,   296,     0,     0,     0,     0,     0,     0,
       0,     0,    13,    14,    15,    16,    17,     0,     0,  1012,
       0,     0,     0,     0,     0,  1265,   760,     0,     0,     0,
       0,     0,     0,   114,     0,     0,     0,     0,   118,   735,
     735,   409,     0,   447,     0,     0,     0,     0,   321,  1002,
       0,     0,  1005,     0,   147,   409,  1234,   114,     0,     0,
     595,   447,  1345,   321,  1234,  1182,     0,     0,     0,     0,
      57,     0,     0,   251,     0,     0,   605,   114,     0,     0,
       0,    61,     0,     0,   595,     0,    64,    65,    66,    67,
      68,    69,    70,  1234,     0,     0,   467,   595,     0,     0,
       0,    61,     0,     0,   217,   218,    64,    65,    66,    67,
      68,    69,    70,   508,   114,     0,   439,  1073,     0,     0,
     114,  1077,   114,     0,     0,     0,   251,     0,     0,    72,
      74,     0,     0,   779,     0,     0,   318,   114,   349,     0,
       0,  1638,  1639,    13,    14,    15,    16,    17,  1576,   757,
      74,    57,     0,   602,   413,   118,  1234,     0,     0,     0,
      77,   758,     0,     0,  1866,     0,   114,   413,   118,   118,
     251,     0,     0,     0,     0,     0,     0,   298,     0,     0,
       0,   730,    61,   447,     0,   217,   218,    64,    65,    66,
      67,    68,    69,    70,     0,     0,     0,     0,   759,     0,
       0,    57,     0,    61,     0,   759,   345,   346,    64,    65,
      66,    67,    68,    69,    70,     0,     0,   114,     0,     0,
     114,     0,     0,     0,   447,     0,     0,     0,   850,     0,
    1264,    74,    61,   251,     0,   217,   218,    64,    65,    66,
      67,    68,    69,    70,     0,     0,     0,   321,  1718,     0,
     548,     0,     0,     0,    75,     0,     0,     0,   114,   347,
      72,     0,     0,   251,     0,     0,     0,     0,     0,   251,
       0,     0,  1462,     0,     0,     0,     0,   114,  1736,     0,
     295,    74,     0,  1739,  1740,     0,   850,   147,     0,     0,
       0,    77,    78,     0,     0,   409,   114,     0,   251,   114,
       0,     0,     0,     0,   735,     0,     0,     0,     0,     0,
       0,  1679,     0,   114,     0,     0,     0,   114,     0,     0,
    1683,     0,     0,     0,   409,     0,     0,     0,     0,     0,
    1515,     0,     0,     0,   467,    13,    14,    15,    16,    17,
    1234,   245,    82,     0,     0,     0,   595,     0,     0,   618,
     413,     0,     0,     0,     0,     0,   296,  1712,     0,     0,
       0,     0,   147,     0,     0,     0,     0,     0,     0,   439,
       0,     0,     0,     0,     0,  1296,     0,     0,  1300,     0,
       0,     0,     0,     0,   413,     0,     0,     0,     0,     0,
       0,     0,     0,    57,     0,   850,   439,     0,     0,   447,
     147,   118,     0,     0,     0,     0,    57,     0,     0,   118,
     114,     0,   850,   850,   413,     0,     0,     0,     0,     0,
     251,     0,     0,     0,    61,     0,     0,   217,   218,    64,
      65,    66,    67,    68,    69,    70,     0,    61,   118,     0,
     217,   218,    64,    65,    66,    67,    68,    69,    70,     0,
       0,     0,    72,     0,     0,  1768,   118,   467,     0,  1777,
       0,   321,   321,     0,     0,    72,     0,     0,     0,     0,
       0,  1784,  1509,    74,     0,     0,   118,  1790,     0,     0,
       0,     0,     0,    77,    78,   219,    74,     0,     0,   413,
     413,     0,     0,     0,   251,   114,    77,    78,     0,   147,
     147,   147,   147,   147,   147,     0,     0,     0,     0,  1511,
     302,  1515,     0,     0,   118,     0,     0,  1663,     0,  1515,
       0,     0,     0,     0,     0,     0,   114,     0,   409,   409,
       0,   114,     0,   467,   251,   114,     0,   114,  1234,     0,
       0,     0,     0,  1234,  1234,  1234,     0,     0,   114,     0,
     114,     0,  1836,     0,     0,     0,     0,     0,   245,     0,
       0,  1448,     0,     0,   349,   114,   413,     0,   251,     0,
    1457,     0,     0,     0,     0,    61,  1865,   439,     0,  1949,
      64,    65,    66,    67,    68,    69,    70,  1231,     0,   114,
       0,  1232,   251,  1233,     0,     0,   548,     0,     0,   251,
     147,     0,   114,     0,   903,     0,     0,     0,     0,     0,
     114,     0,     0,   118,   118,   118,   118,   118,   118,  1894,
       0,     0,     0,   413,    74,   114,   114,  1900,   413,     0,
       0,   413,     0,     0,   114,   114,   114,     0,     0,     0,
       0,     0,   118,   118,     0,     0,     0,  1918,     0,  1920,
    1921,     0,   595,     0,     0,     0,     0,     0,     0,     0,
     850,   850,     0,     0,     0,     0,     0,     0,     0,     0,
    1765,  1930,     0,  1515,   850,   850,     0,     0,     0,   447,
       0,     0,     0,     0,     0,     0,  1648,     0,   413,     0,
    1511,     0,   409,     0,     0,  1945,  1511,     0,  1511,     0,
      57,     0,     0,     0,     0,  1950,     0,   850,   850,     0,
       0,     0,   413,     0,   118,     0,     0,     0,     0,     0,
       0,     0,     0,  1234,   298,  1234,   302,   147,     0,   413,
    1973,    61,  1950,     0,   217,   218,    64,    65,    66,    67,
      68,    69,    70,     0,     0,     0,     0,     0,  1984,     0,
       0,   114,   114,     0,  1973,     0,   668,   409,     0,    72,
       0,     0,     0,     0,  1992,     0,     0,     0,   321,     0,
     114,   147,     0,     0,  1515,     0,     0,     0,     0,  1905,
      74,     0,     0,   498,     0,     0,     0,     0,     0,     0,
      77,    78,     0,     0,   147,     0,     0,     0,   114,     0,
       0,     0,     0,     0,     0,     0,   118,    61,     0,  1651,
     543,    63,    64,    65,    66,    67,    68,    69,    70,   321,
     321,   251,     0,    13,    14,    15,    16,    17,     0,   413,
       0,     0,   251,     0,  1648,  1648,   114,     0,     0,     0,
       0,   118,   114,   413,     0,     0,     0,     0,     0,  1511,
       0,   114,  1511,  1184,   413,     0,   114,     0,     0,   985,
     447,     0,     0,   298,     0,     0,     0,     0,     0,   302,
       0,   118,     0,   119,     0,     0,   119,     0,     0,     0,
     409,    57,     0,     0,     0,   118,     0,     0,     0,     0,
     850,   850,     0,     0,     0,   826,   828,    57,     0,     0,
       0,     0,     0,   296,   413,     0,     0,     0,   118,     0,
       0,     0,    61,   570,   298,   217,   218,    64,    65,    66,
      67,    68,    69,    70,     0,     0,  1674,     0,    61,     0,
     119,   217,   218,    64,    65,    66,    67,    68,    69,    70,
      72,     0,     0,     0,  1648,     0,   298,     0,     0,     0,
       0,     0,     0,  1511,   119,     0,    72,  1651,  1651,     0,
    1905,    74,     0,     0,   498,   114,     0,     0,     0,     0,
       0,    77,    78,     0,   119,     0,   295,    74,     0,   147,
       0,     0,   114,   114,     0,     0,     0,    77,    78,     0,
       0,     0,     0,     0,   118,     0,     0,   850,     0,     0,
       0,     0,     0,     0,   321,     0,     0,     0,     0,     0,
       0,   119,  1648,     0,     0,     0,     0,   119,     0,   119,
       0,     0,   147,     0,     0,     0,     0,   850,     0,   668,
       0,     0,   850,   850,   668,   114,     0,   668,     0,   147,
     147,     0,  1906,   302,     0,     0,     0,     0,     0,     0,
       0,   119,     0,     0,     0,     0,   668,     0,     0,     0,
       0,    98,     0,   119,   151,     0,     0,  1651,    57,     0,
       0,     0,     0,     0,     0,   114,     0,   147,     0,     0,
       0,     0,     0,   413,     0,     0,     0,     0,     0,     0,
     982,     0,  1906,  1906,     0,     0,     0,     0,     0,    61,
       0,     0,   217,   218,    64,    65,    66,    67,    68,    69,
      70,     0,   413,     0,   119,     0,     0,   119,    98,     0,
       0,     0,   119,     0,     0,  1906,     0,    72,     0,   251,
     114,  1881,     0,     0,     0,  1651,   118,     0,     0,     0,
       0,     0,   195,     0,     0,     0,     0,  1509,    74,     0,
     114,     0,     0,   118,     0,   119,     0,   413,    77,    78,
       0,  1184,   258,     0,     0,  1651,     0,     0,     0,     0,
       0,     0,     0,  1408,   119,     0,    13,    14,    15,    16,
      17,     0,     0,     0,   413,     0,     0,     0,   114,     0,
       0,   118,     0,     0,     0,     0,     0,     0,     0,   288,
       0,     0,     0,    61,     0,    98,   168,   169,    64,    65,
      66,    67,    68,    69,    70,  1651,  1651,     0,     0,     0,
       0,     0,   323,     0,     0,     0,     0,     0,     0,     0,
     114,   114,     0,     0,    57,     0,     0,     0,     0,     0,
     419,     0,     0,     0,   114,   114,     0,   119,  1651,   114,
     114,   288,   445,     0,     0,     0,     0,     0,     0,   578,
       0,     0,   595,     0,     0,    61,     0,     0,     0,     0,
      64,    65,    66,    67,    68,    69,    70,   114,   114,     0,
     492,   119,     0,     0,     0,     0,     0,   114,   114,   114,
     114,   114,   114,    72,     0,     0,   512,     0,   251,     0,
       0,   517,   519,     0,     0,   195,     0,   119,     0,     0,
       0,     0,     0,    73,    74,     0,   413,   413,     0,     0,
       0,     0,     0,   595,    77,    78,     0,   539,   850,     0,
     541,    61,   542,     0,   217,   218,    64,    65,    66,    67,
      68,    69,    70,   559,    61,     0,   251,   217,   218,    64,
      65,    66,    67,    68,    69,    70,   571,     0,   108,    72,
       0,     0,     0,     0,    61,   413,     0,   217,   218,    64,
      65,    66,    67,    68,    69,    70,     0,     0,     0,   757,
      74,   593,     0,   602,   617,     0,   119,   119,   114,     0,
      77,   758,    72,     0,     0,     0,     0,     0,   624,     0,
    1195,     0,   624,   604,     0,     0,     0,     0,     0,     0,
       0,     0,  1509,    74,     0,   108,     0,     0,     0,  1510,
       0,     0,     0,    77,    78,     0,     0,     0,   119,     0,
       0,     0,   119,    61,   119,     0,   168,   169,    64,    65,
      66,    67,    68,    69,    70,    61,     0,   119,   217,   218,
      64,    65,    66,    67,    68,    69,    70,     0,     0,   259,
     114,   114,   714,   715,   716,   717,   718,   719,   720,   721,
     722,   723,   724,    72,     0,     0,   204,     0,    61,     0,
     413,   217,   218,    64,    65,    66,    67,    68,    69,    70,
       0,     0,     0,  1905,    74,   288,   114,   498,     0,   593,
       0,     0,   108,   725,    77,    78,    72,   119,     0,     0,
       0,     0,     0,     0,   251,   114,     0,     0,     0,   327,
     119,     0,   119,   119,     0,   119,   219,    74,   119,   113,
       0,   119,   119,   119,     0,     0,     0,    77,    78,     0,
       0,     0,     0,     0,     0,   413,     0,     0,     0,   446,
       0,     0,     0,     0,     0,     0,   114,     0,     0,   114,
       0,     0,     0,     0,     0,     0,    61,   114,     0,   217,
     218,    64,    65,    66,    67,    68,    69,    70,     0,     0,
     445,     0,   114,     0,     0,     0,   113,     0,     0,     0,
       0,     0,     0,     0,    72,     0,     0,   114,     0,     0,
       0,     0,   114,   114,     0,     0,     0,   114,   114,   119,
       0,   846,     0,     0,   295,    74,   519,     0,     0,     0,
     857,     0,   559,     0,   540,    77,    78,     0,     0,     0,
     261,     0,     0,   323,     0,    98,    61,     0,     0,     0,
     108,    64,    65,    66,    67,    68,    69,    70,  1231,     0,
     624,   878,  1232,     0,  1233,     0,     0,   251,     0,     0,
       0,     0,     0,     0,     0,   889,     0,     0,   413,     0,
       0,     0,     0,   113,   593,     0,     0,     0,   594,   898,
       0,   259,     0,     0,     0,    74,     0,   624,  1430,     0,
     331,     0,     0,     0,     0,   594,   365,     0,   366,   594,
     367,    62,    63,    64,    65,    66,    67,    68,    69,    70,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1530,
     448,     0,  1533,  1544,     0,     0,   119,     0,  1550,     0,
       0,     0,  1554,     0,  1556,     0,     0,     0,     0,   119,
     119,     0,     0,     0,     0,     0,   675,     0,    61,    75,
     376,     0,     0,    64,    65,    66,    67,    68,    69,    70,
    1231,     0,     0,     0,  1232,     0,  1233,   114,     0,    61,
       0,     0,     0,   445,    64,    65,    66,    67,    68,    69,
      70,  1231,     0,     0,     0,  1232,     0,  1233,     0,     0,
     994,     0,   114,     0,     0,     0,   594,    74,    61,     0,
    1615,   189,    63,    64,    65,    66,    67,    68,    69,    70,
     114,   113,     0,     0,   878,     0,   193,     0,    74,  1018,
       0,  1617,     0,     0,     0,     0,     0,   114,   114,     0,
       0,   251,     0,     0,     0,     0,   445,   445,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,   596,
     779,     0,   261,    61,     0,   445,   168,   169,    64,    65,
      66,    67,    68,    69,    70,   114,   596,     0,     0,     0,
     596,     0,  1642,     0,     0,     0,     0,   446,     0,     0,
       0,   193,    61,   846,     0,   545,   546,    64,    65,    66,
      67,    68,    69,    70,     0,     0,   193,     0,   114,     0,
       0,   456,     0,     0,  1675,     0,     0,     0,   327,     0,
       0,     0,     0,   193,  1154,     0,  1681,   259,     0,   108,
       0,   445,     0,  1684,     0,     0,   442,   151,     0,     0,
     446,     0,   108,    75,     0,     0,   624,     0,     0,  1186,
       0,   846,     0,     0,     0,     0,  1192,   594,   446,    61,
       0,  1533,    62,    63,    64,    65,    66,    67,    68,    69,
      70,     0,     0,     0,     0,     0,     0,   596,     0,     0,
       0,   594,   119,     0,     0,     0,     0,     0,     0,   193,
     119,     0,     0,     0,   594,     0,     0,    61,     0,   323,
     168,   169,    64,    65,    66,    67,    68,    69,    70,     0,
      75,     0,     0,     0,     0,     0,     0,     0,     0,   119,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   119,     0,     0,
       0,     0,     0,     0,     0,   460,   193,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   119,   448,     0,
     846,     0,     0,     0,     0,   193,     0,     0,     0,  1776,
       0,     0,     0,     0,     0,     0,     0,   846,   846,     0,
     446,  1781,  1783,     0,  1544,     0,     0,     0,     0,   331,
       0,     0,     0,     0,     0,   119,     0,     0,   261,     0,
     113,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   448,     0,   113,     0,     0,     0,     0,     0,     0,
       0,   446,     0,     0,     0,     0,     0,     0,   596,   448,
     445,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   327,   327,     0,     0,     0,     0,     0,
       0,     0,   596,   193,     0,     0,     0,     0,     0,     0,
       0,     0,   327,     0,     0,   596,     0,     0,     0,     0,
    1323,  1842,     0,     0,     0,     0,     0,     0,  1154,     0,
       0,     0,     0,   193,     0,     0,     0,     0,     0,     0,
     327,     0,     0,     0,   119,   119,   119,   119,   119,   119,
       0,     0,     0,     0,     0,     0,     0,  1154,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   108,     0,   119,   119,  1371,     0,     0,   327,     0,
       0,     0,     0,     0,     0,     0,  1897,     0,  1899,     0,
       0,     0,     0,   594,     0,     0,   259,     0,   327,     0,
       0,   448,   593,     0,     0,     0,     0,     0,   193,   193,
       0,   517,     0,     0,   442,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   323,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   448,     0,     0,   119,   446,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1946,     0,     0,     0,   331,   331,     0,   193,     0,     0,
       0,     0,  1954,  1956,  1957,   846,   846,     0,     0,     0,
       0,     0,     0,   331,     0,   442,     0,     0,     0,   846,
     846,     0,     0,     0,   445,   445,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   327,   193,     0,
       0,   331,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   846,   846,   327,   327,     0,     0,     0,   193,
       0,     0,  1323,  1323,  1323,   151,     0,   119,     0,     0,
       0,     0,   113,     0,     0,     0,     0,     0,     0,   331,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1539,  1539,     0,   596,     0,     0,   261,     0,   331,
       0,     0,   119,     0,     0,     0,     0,   327,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   123,     0,     0,   123,     0,     0,     0,     0,
       0,     0,   119,     0,     0,     0,     0,   442,     0,     0,
     323,     0,     0,     0,     0,     0,   119,   448,     0,     0,
       0,     0,     0,     0,     0,   108,     0,     0,     0,     0,
       0,   193,     0,   151,     0,     0,     0,     0,     0,   119,
       0,     0,     0,     0,     0,     0,     0,     0,   442,   123,
       0,     0,     0,     0,   108,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     442,   442,   259,   123,     0,     0,     0,     0,   331,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   442,
       0,     0,     0,   123,     0,   331,   331,     0,     0,   594,
       0,     0,     0,     0,     0,   846,   846,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   119,   446,     0,     0,     0,
     123,  1665,     0,     0,     0,     0,   123,     0,   123,     0,
       0,   846,     0,     0,     0,     0,     0,     0,   331,     0,
       0,     0,     0,     0,     0,   442,     0,     0,     0,     0,
    1682,     0,   193,     0,     0,     0,     0,     0,     0,     0,
     123,     0,   327,   327,     0,     0,     0,     0,     0,     0,
       0,     0,   123,     0,     0,     0,   327,   327,     0,     0,
    1539,   327,   327,     0,     0,     0,   113,     0,     0,     0,
       0,   323,     0,     0,   151,     0,     0,     0,     0,     0,
       0,     0,   846,     0,     0,     0,     0,     0,     0,   327,
     327,     0,     0,   193,     0,   113,     0,     0,     0,     0,
       0,     0,     0,   123,     0,     0,   123,     0,     0,     0,
       0,   123,   846,   261,     0,     0,     0,   846,   846,     0,
       0,     0,   445,   445,     0,     0,     0,     0,   108,   108,
       0,     0,     0,     0,     0,     0,     0,   119,  1757,     0,
     596,     0,     0,     0,   123,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   119,     0,     0,     0,     0,     0,
       0,     0,     0,   123,     0,     0,     0,   448,     1,     0,
       0,   145,     0,     0,     0,     0,     0,   446,     0,     0,
       0,     0,     0,  1539,     0,     0,     0,     0,     0,     0,
       0,     0,   119,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   331,   331,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   442,     0,     0,   331,   331,     0,
       0,     0,   331,   331,     0,     0,   123,     0,     0,     0,
       0,     0,     0,     0,   192,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     331,   331,     0,     0,     0,     0,     0,     0,     0,     0,
     123,     0,   327,   327,     0,     0,     0,     0,     0,     0,
       0,     0,  1856,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   123,     0,     0,   113,
     113,     0,     0,   283,     0,     0,     0,   445,   327,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1539,     0,   259,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   193,
       0,     0,  1539,  1856,     0,     0,   193,     0,   448,     0,
       0,     0,     0,     0,     0,   203,     0,   108,     0,     0,
       0,   214,   215,     0,     0,     0,     0,     0,   327,     0,
       0,     0,     0,   193,     0,   123,   123,     0,     0,   327,
    1539,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   277,  1938,   283,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   327,
       0,     0,   520,   846,   327,   327,     0,   123,     0,   327,
     327,   123,   283,   123,     0,     0,     0,     0,     0,     0,
       0,     0,   283,     0,     0,     0,   123,     0,   442,   442,
       0,     0,     0,   331,   331,     0,   551,   555,     0,     0,
       0,     0,     0,   562,   563,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   573,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   331,
     108,     0,     0,     0,     0,     0,     0,     0,   591,     0,
       0,     0,     0,     0,     0,     0,   123,     0,   261,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   123,
       0,   123,   123,     0,   123,     0,     0,   123,     0,     0,
     123,   123,   123,     0,     0,     0,     0,     0,   113,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   331,
       0,     0,     0,     0,   674,     0,     0,     0,     0,     0,
     331,     0,     0,     0,   193,     0,     0,     0,     0,     0,
       0,   567,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   713,     0,     0,     0,   594,
     331,     0,     0,     0,     0,   331,   331,     0,     0,     0,
     331,   331,     0,     0,     0,     0,     0,     0,   123,     0,
       0,   751,     0,     0,   327,   754,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   165,
       0,     0,   108,     0,   776,     0,     0,     0,   777,   778,
       0,     0,   781,     0,     0,     0,     0,     0,     0,   108,
     594,     0,     0,     0,     0,   165,     0,   795,   796,   797,
     798,   113,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   820,     0,     0,   193,
       0,     0,     0,     0,   823,     0,     0,   108,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   165,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   283,     0,   165,     0,   165,   755,     0,   756,
     327,     0,     0,     0,     0,   123,     0,     0,   772,   773,
       0,     0,     0,     0,     0,     0,     0,     0,   123,   123,
       0,     0,     0,   861,     0,     0,   351,     0,     0,     0,
     551,     0,     0,     0,     0,   193,   867,     0,     0,     0,
     596,     0,     0,   351,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   881,
     886,     0,     0,     0,     0,   331,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   165,     0,   113,     0,   165,   442,   442,   165,   165,
       0,     0,   165,     0,     0,   165,   165,     0,     0,     0,
     113,   596,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1827,     0,   856,     0,     0,     0,
     929,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   113,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   165,     0,     0,
     165,   360,     0,     0,     0,   361,     0,   362,     0,     0,
       0,   331,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   165,     0,     0,   363,     0,     0,   990,     0,     0,
       0,     0,     0,     0,     0,     0,   165,     0,     0,     0,
       0,     0,  1007,     0,     0,     0,  1008,     0,     0,     0,
       0,   364,   365,     0,   366,   881,   367,    62,    63,    64,
      65,    66,    67,    68,    69,    70,   368,   369,   357,     0,
     370,   371,   372,     0,   373,   374,     0,  1048,     0,     0,
       0,     0,    72,     0,     0,     0,  1057,     0,     0,     0,
       0,   123,  1060,     0,     0,     0,     0,     0,     0,   123,
       0,     0,   375,     0,     0,    75,   376,     0,     0,     0,
       0,   442,   377,    77,    78,   378,   379,   380,   381,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   123,     1,
     165,     0,     0,     0,     0,     0,     1,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   123,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     1,     0,     0,   123,     0,   360,  1033,
       0,     0,   361,     0,   362,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   351,     0,     0,     0,     0,
       0,   363,     0,     0,     0,     0,     0,   165,     0,     0,
       0,     0,     0,     0,   123,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1209,   364,   365,
       0,   366,     0,   367,  1779,    63,    64,    65,    66,    67,
      68,    69,    70,   368,   369,   357,     0,   370,   371,   372,
       0,   373,   374,     0,     0,     0,  1101,  1102,     0,    72,
       0,     0,     0,     0,     0,     0,     0,  1160,  1161,  1162,
       0,     0,  1164,     0,     0,     0,     0,     0,     0,   375,
       0,   351,    75,   376,     0,     0,     0,     0,     0,   377,
      77,    78,   378,   379,   380,   381,     0,     0,     0,     0,
       0,     0,     0,  1780,  -175,     0,     0,     0,  1255,     0,
       0,     0,  1256,   123,   123,   123,   123,   123,   123,   881,
       0,   165,   165,     0,     0,     0,     0,     0,     0,  1269,
       0,     0,     0,     0,   165,     0,  1270,     0,     0,     0,
       0,     0,   123,   123,     0,  1274,     0,  1275,     0,     0,
       0,     0,  1229,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1302,
       0,     0,     0,  1303,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1248,   145,     0,     0,
       1,     0,     0,     0,     0,     0,     0,     0,     0,    13,
      14,    15,    16,    17,   123,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,  1272,     0,   165,   165,    45,    46,     0,
       0,   165,  1276,  1277,  1278,  1279,     0,     0,     0,     0,
    1284,  1285,     0,     0,     0,     0,     0,    57,     0,     0,
    1292,     0,   165,     0,     0,   165,   165,     0,   165,     0,
     165,   165,     0,     0,     0,     0,     0,     0,  1390,     0,
       0,  1306,     0,  1307,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   123,     0,     0,     0,
       0,     0,  1413,     0,     0,     0,     0,     0,     0,   165,
       0,     0,     0,   165,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   123,     0,     0,     0,     0,  1363,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   123,  1377,     0,     0,     0,     0,     0,     0,  1381,
       0,  1383,  1385,     0,     0,   123,     0,     0,     0,     0,
    1391,     0,  1392,     0,  1393,     0,  1395,     0,     0,   165,
       0,  1403,  1485,     0,     0,     0,  1486,     0,   123,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   247,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   268,  1521,   271,     0,   273,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   387,     0,     0,  1445,
       0,     0,     0,     0,     0,     0,  1452,  1453,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   247,     0,   271,
     273,     0,     0,     0,     0,     0,  1581,     0,     0,  1584,
    1476,     0,     0,     0,   123,     0,     0,  1481,     0,     0,
    1482,     0,     0,     0,  1592,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   247,     0,     0,     0,     0,     0,     0,     0,   165,
     214,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1622,     0,     0,     0,     0,     0,
       0,     0,     0,  1627,     0,     0,     0,  1628,     0,     0,
    1571,     0,     0,     0,     0,     0,     0,   165,     0,     0,
     165,  1632,  1633,     0,   247,     0,   271,   273,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1590,     0,     0,     0,     0,     0,     0,
       0,  1595,     0,  1597,   247,     0,     0,     0,     0,     0,
     247,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   123,     0,   649,     0,
       0,   387,   654,     0,     0,     0,     0,     0,     0,   247,
       0,   657,   658,   123,     0,   620,     0,   273,     0,  1623,
    1624,     0,     0,     0,     0,     0,   387,   387,     0,     0,
       0,     0,     0,     0,  1629,  1630,     0,  1631,     0,     0,
       0,     0,     0,     0,     0,     0,  1635,   387,     0,     0,
       0,   123,     0,  1715,  1716,     0,  1640,  1641,     0,     0,
     165,     0,     0,     0,     0,     0,     0,     0,   165,   165,
       0,     0,     0,     0,     0,     0,     0,   387,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   247,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   165,     0,     0,     0,     0,     0,     0,
       0,   247,   165,   620,   273,   165,     0,   165,   165,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1719,  1720,     0,     0,     0,     0,     0,     0,
       0,  1792,  1726,     0,     0,     0,     0,   165,   247,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1584,     0,     0,     0,   247,     0,
       0,     0,     0,   247,     0,   247,     0,  1749,  1750,     0,
       0,     0,  1817,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   247,     0,   247,   247,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1835,
       0,     0,     0,     0,   165,   247,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1852,   247,     0,  1853,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   247,
       0,   620,   273,     0,     0,     0,     0,  1805,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   247,   620,  1814,     0,     0,  1815,  1816,
     247,     0,     0,     0,     0,  1818,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   165,     0,   387,   387,
     387,   387,   387,   387,   387,   387,   387,   387,   387,   387,
     387,   387,   387,   387,   387,   387,   387,  1923,     0,     0,
       0,     0,     0,     0,     0,   165,   338,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   165,     0,     0,     0,   435,   338,   165,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   387,     0,
       0,     0,     0,     0,     0,     0,     0,   501,     0,     0,
       0,     0,     0,  1904,   501,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   171,   174,     0,     0,     0,     0,     0,
       0,     0,   165,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1935,     0,     0,     0,     0,     0,   212,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   501,     0,     0,     0,  1952,     0,     0,     0,
       0,     0,     0,  1958,     0,     0,     0,     0,   200,     0,
       0,     0,     0,     0,     0,   338,   606,     0,  1971,     0,
       0,     0,     0,     0,   255,     0,   165,   165,   290,     0,
       0,   291,   247,     0,   351,     0,   627,     0,   165,     0,
       0,     0,     0,   247,   312,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   387,   247,     0,     0,     0,   387,     0,
       0,     0,     0,   200,     0,   247,     0,   303,     0,   387,
       0,     0,     0,     0,   247,     0,     0,     0,   343,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   477,     0,
       0,     0,     0,     0,     0,   200,   501,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   455,   387,
       0,   459,   501,   747,     0,   501,   750,     0,     0,     0,
       0,   165,     0,   338,     0,     0,     0,   606,     0,     0,
       0,     0,     0,   528,     0,   163,     0,     0,     0,     0,
       0,     0,     0,   171,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   171,     0,     0,     0,     0,     0,
       0,   200,     0,     0,     0,     0,     0,     0,   501,     0,
     247,     0,   501,     0,   255,     0,     0,     0,     0,     0,
       0,   574,     0,   165,     0,     0,     0,     0,   577,   579,
       0,     0,     0,   586,   247,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   338,     0,     0,   275,     0,     0,
     459,     0,     0,     0,     0,     0,     0,     0,   200,     0,
     281,     0,   282,     0,     0,   631,     0,     0,     0,     0,
     312,     0,     0,   312,     0,     0,   387,   599,     0,   616,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   501,     0,     0,   338,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   165,     0,     0,
       0,     0,     0,     0,   876,   338,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   606,     0,     0,     0,   606,
       0,   672,     0,     0,     0,     0,   894,     0,   338,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     387,     0,     0,     0,   502,   503,     0,     0,   507,     0,
     212,   510,   511,     0,     0,   200,     0,     0,     0,     0,
     247,     0,   774,   775,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   387,   387,   387,     0,     0,     0,
       0,   387,   387,     0,     0,   599,     0,     0,     0,     0,
       0,   771,   247,     0,     0,     0,     0,     0,   247,     0,
       0,     0,     0,   387,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   338,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   589,     0,     0,
     387,   387,     0,   501,   501,     0,     0,     0,     0,     0,
       0,     0,   621,   501,  1003,     0,   501,  1006,     0,     0,
     200,   200,     0,     0,     0,     0,   455,     0,   338,     0,
       0,   606,     0,   606,   606,     0,     0,     0,     0,     0,
     606,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     338,   338,     0,   312,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   338,
       0,     0,     0,   501,     0,     0,     0,   501,     0,   343,
       0,   501,  1074,     0,     0,   501,  1078,     0,     0,   247,
       0,     0,     0,  1081,     0,     0,     0,   455,     0,   880,
       0,   631,     0,     0,     0,     0,   745,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   360,
     599,     0,     0,   361,     0,   362,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   338,   501,   247,     0,     0,
       0,   200,   363,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   672,     0,   672,   672,     0,   672,
       0,     0,   672,   606,     0,   672,   672,   672,     0,   364,
     365,     0,   462,   816,   367,    62,    63,    64,    65,    66,
      67,    68,    69,    70,   368,   369,   357,     0,   370,   371,
     372,     0,   373,   374,     0,     0,     0,     0,     0,     0,
      72,     0,     0,   338,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   455,
     375,    74,     0,   463,   464,     0,     0,     0,   465,     0,
     377,    77,    78,   378,   379,   380,   381,     0,     0,  1035,
       0,     0,     0,   200,     0,     0,  1047,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     455,     0,     0,     0,     0,     0,     0,     0,   501,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   455,   455,     0,   606,   606,   892,   893,     0,
       0,     0,   606,   387,     0,     0,     0,     0,     0,     0,
     900,   455,     0,     0,     0,   247,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1105,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   338,     0,     0,     0,     0,   501,
    1297,     0,   501,  1301,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   455,     0,     0,
       0,     0,  1199,     0,   200,     0,     0,   631,     0,     0,
       0,     0,     0,     0,   771,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   996,   997,     0,     0,     0,     0,  1001,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   247,     0,     0,     0,   343,     0,     0,  1022,     0,
       0,  1025,  1026,     0,  1029,     0,  1031,  1032,   247,     0,
       0,     0,     0,     0,     0,     0,   338,     0,     0,     0,
       0,     0,   606,  1399,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   338,     0,  1072,     0,   387,     0,  1076,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   411,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   440,     0,     0,   387,     0,     0,
       0,     0,     0,   247,     0,   501,  1449,   468,     0,   468,
       0,     0,     0,     0,   501,  1458,     0,   606,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   338,   338,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1193,   455,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1344,  1346,  1348,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   387,     0,   387,     0,   672,     0,     0,     0,
       0,  1367,     0,     0,     0,   568,     0,     0,     0,     0,
       0,     0,   247,     0,     0,     0,  1105,     0,     0,     0,
     387,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   387,     0,     0,     0,     0,     0,
     255,     0,     0,   631,   338,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   200,     0,     0,     0,     0,     0,     0,   599,     0,
       0,     0,     0,     0,     0,  1193,     0,     0,     0,   387,
       0,     0,     0,     0,   247,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   343,     0,     0,     0,   672,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1295,     0,     0,  1299,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   501,     0,     0,     0,     0,     0,     0,
     455,   455,     0,     0,     0,     0,     0,   468,     0,   501,
       0,     0,     0,   468,  1520,     0,     0,  1522,   792,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   672,   672,
     672,     0,   672,   672,     0,     0,     0,     0,     0,   459,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   338,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1397,     0,     0,     0,
       0,     0,     0,     0,  1406,  1407,     0,   255,     0,     0,
       0,     0,     0,     0,     0,   860,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   343,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   338,   338,     0,     0,
       0,     0,     0,   440,     0,     0,     0,     0,     0,     0,
       0,   501,   501,     0,     0,     0,   888,     0,     0,  1447,
       0,     0,     0,     0,     0,     0,     0,   501,  1456,     0,
       0,  1460,     0,  1463,  1464,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   923,     0,     0,
       0,     0,     0,  1490,     0,     0,  1671,     0,     0,     0,
       0,     0,     0,     0,     0,   792,   943,     0,     0,     0,
       0,     0,     0,     0,     0,   954,     0,   959,   954,     0,
       0,   200,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   501,     0,     0,     0,   987,     0,     0,     0,   501,
       0,     0,     0,     0,     0,   255,     0,     0,   989,     0,
    1578,     0,     0,     0,     0,     0,     0,     0,     0,   998,
       0,     0,     0,     0,     0,     0,     0,     0,   631,     0,
       0,     0,     0,   440,     0,     0,   987,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   343,     0,     0,
       0,   338,     0,     0,     0,   501,  1882,     0,     0,   501,
       0,     0,     0,  1051,     0,     0,   468,     0,     0,     0,
       0,     0,     0,   672,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   501,
       0,     0,  1460,     0,     0,     0,     0,     0,   455,   455,
       0,     0,  1082,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1637,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   501,
     501,     0,     0,  1803,     0,     0,     0,     0,   255,     0,
     411,     0,   631,     0,     0,     0,     0,     0,     0,     0,
    1183,  1185,     0,     0,     0,     0,     0,     0,   440,     0,
       0,     0,   501,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   468,
       0,     0,     0,     0,     0,     0,     0,     0,   954,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   987,     0,     0,     0,     0,     0,     0,  1713,  1222,
       0,     0,     0,     0,     0,     0,   954,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   672,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     468,     0,     0,     0,  1972,     0,     0,     0,     0,     0,
       0,     0,     0,   455,     0,     0,     0,     0,     0,     0,
    1339,     0,  1761,  1762,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1766,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   672,
       0,   360,   459,     0,     0,   361,     0,   362,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   468,     0,  1288,
       0,  1290,  1107,     0,   363,    -2,     0,  1109,  -234,  -234,
    1110,  1111,  1112,  1113,  1114,  1115,  1116,  1117,  1118,  1119,
    1120,  1121,  -294,  1122,  1123,  1124,  1125,  1126,     0,  1127,
       0,   364,   365,     0,   462,     0,   367,  1128,  1129,    64,
      65,    66,    67,    68,    69,    70,   368,   369,   357,  1130,
     370,   371,   372,     0,   373,   374,     0,  1825,     0,     0,
       0,     0,    72,     0,     0,     0,     0,  1355,  1355,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -234,   375,     0,     0,    75,   376,     0,     0,     0,
     279,     0,   377,    77,    78,   378,   379,   380,   381,     0,
       0,     0,     0,     0,     0,     0,     0,  -175,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1880,
       0,     0,     0,     0,     0,     0,     0,     0,  1394,     0,
       0,     0,     0,     0,  1404,     0,     0,     0,     0,     0,
       0,  1972,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   440,     0,     0,     0,     0,     0,  1339,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   468,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   954,     0,     0,   792,     0,     0,     0,   360,     0,
       0,     0,   361,     0,   362,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1107,
       0,   363,    -2,     0,  1109,  -235,  -235,  1110,  1111,  1112,
    1113,  1114,  1115,  1116,  1117,  1118,  1119,  1120,  1121,  -294,
    1122,  1123,  1124,  1125,  1126,  1484,  1127,     0,   364,   365,
       0,   462,     0,   367,  1128,  1129,    64,    65,    66,    67,
      68,    69,    70,   368,   369,   357,  1130,   370,   371,   372,
       0,   373,   374,     0,     0,     0,     0,     0,  1711,    72,
       0,     0,     0,   954,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1339,     0,     0,     0,  -235,   375,
       0,   468,    75,   376,   792,     0,     0,   279,     0,   377,
      77,    78,   378,   379,   380,   381,     0,     0,     0,     0,
       0,     0,     0,     0,  -175,   360,     0,     0,     0,   361,
       0,   362,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   943,     0,     0,     0,  1107,     0,   363,    -2,
       0,  1109,  1593,  1594,  1110,  1111,  1112,  1113,  1114,  1115,
    1116,  1117,  1118,  1119,  1120,  1121,  -294,  1122,  1123,  1124,
    1125,  1126,     0,  1127,     0,   364,   365,   468,   462,   792,
     367,  1128,  1129,    64,    65,    66,    67,    68,    69,    70,
     368,   369,   357,  1130,   370,   371,   372,     0,   373,   374,
       0,     0,     0,     0,     0,     0,    72,     0,   360,     0,
       0,     0,   361,     0,   362,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   375,     0,     0,    75,
     376,   363,     0,     0,   279,     0,   377,    77,    78,   378,
     379,   380,   381,     0,     0,     0,     0,     0,     0,     0,
       0,  -175,     0,     0,     0,     0,     0,   411,   364,   365,
       0,   366,  1664,   367,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   368,   369,   357,     0,   370,   371,   372,
       0,   373,   374,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   375,
    1225,     0,    75,   376,     0,     0,     0,  1226,  1704,   377,
      77,    78,   378,   379,   380,   381,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1727,     0,     0,
    1729,     4,   243,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,  1106,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,   360,     0,    45,    46,
     361,     0,   362,    47,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,    56,     0,  1107,    57,  1108,
      -2,     0,  1109,     0,     0,  1110,  1111,  1112,  1113,  1114,
    1115,  1116,  1117,  1118,  1119,  1120,  1121,  -294,  1122,  1123,
    1124,  1125,  1126,     0,  1127,     0,   364,   365,    60,   462,
       0,   367,  1128,  1129,    64,    65,    66,    67,    68,    69,
      70,   368,   369,   357,  1130,   370,   371,   372,     0,   373,
     374,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    -3,   375,     0,     0,
      75,   407,     0,     0,     0,   279,     0,   377,    77,    78,
     378,   379,   380,   381,     0,     0,     0,     0,     0,     0,
       0,     0,  -175,     0,     0,     4,   243,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,  1106,
       0,    19,   954,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,     0,     0,
     360,     0,    45,    46,   361,     0,   362,    47,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,    56,
       0,  1107,    57,  1108,    -2,     0,  1109,     0,     0,  1110,
    1111,  1112,  1113,  1114,  1115,  1116,  1117,  1118,  1119,  1120,
    1121,  -294,  1122,  1123,  1124,  1125,  1126,     0,  1127,     0,
     364,   365,    60,   462,     0,   367,  1128,  1129,    64,    65,
      66,    67,    68,    69,    70,   368,   369,   357,  1130,   370,
     371,   372,     0,   373,   374,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   375,     0,     0,    75,   407,     0,     0,     0,   279,
       0,   377,    77,    78,   378,   379,   380,   381,     0,     0,
       0,     0,     0,     0,     0,     0,  -175,     4,   243,     6,
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
       0,     0,     0,   375,     0,  1541,    75,   407,     0,     0,
       0,     0,     0,   377,    77,    78,   378,   379,   380,   381,
       0,     0,     0,     0,     0,     0,     0,  1542,  1543,     4,
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
     380,   381,     0,     0,     0,     0,     0,     0,     0,  1542,
    1543,     4,   243,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,   360,     0,    45,    46,
     361,     0,   362,    47,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,    56,     0,     0,    57,   363,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   364,   365,    60,   366,
       0,   367,    62,    63,    64,    65,    66,    67,    68,    69,
      70,   368,   369,   357,     0,   370,   371,   372,     0,   373,
     374,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   375,     0,  1532,
      75,   407,     0,     0,     0,     0,     0,   377,    77,    78,
     378,   379,   380,   381,     4,   243,     6,     7,     8,     9,
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
     377,    77,    78,   378,   379,   380,   381,   243,     6,     7,
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
       0,     0,   375,     0,     0,    75,   437,     0,     0,     0,
       0,     0,   377,   438,    78,   378,   379,   380,   381,   243,
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
       0,     0,     0,     0,   375,     0,     0,    75,  1180,     0,
       0,     0,     0,     0,   377,  1181,    78,   378,   379,   380,
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
     376,     0,     0,     0,     0,     0,   377,    77,    78,   378,
     379,   380,   381,   243,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,     0,    19,     0,
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
      78,   378,   379,   380,   381,  1834,     0,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,     0,    -2,     0,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,     0,    -2,    -2,    -2,     0,    -2,     0,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,     0,
       0,    -2,     0,     0,    -2,     0,     0,     0,     0,    -2,
      -2,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    -2,     0,     0,     0,    -2,    -2,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    -2,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    -2,     0,     0,     0,    -2,    -2,     0,     0,
       0,     0,     0,     0,    -2,    -2,  1851,     0,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,     0,    -2,     0,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,     0,    -2,    -2,    -2,     0,    -2,     0,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,
       0,     0,    -2,     0,     0,    -2,  1191,     0,     0,     0,
      -2,    -2,     0,    13,    14,    15,    16,    17,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    -2,     0,     0,     0,    -2,
      -2,     0,     0,     0,     0,     0,     0,     0,     0,   360,
       0,     0,     0,   361,     0,   362,     0,     0,     0,     0,
       0,     0,     0,     0,    -2,     0,     0,     0,     0,     0,
       0,    57,   363,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1401,    -2,     0,     0,     0,    -2,    -2,    13,
      14,    15,    16,    17,     0,    -2,    -2,     0,     0,   364,
     365,     0,   366,     0,   367,    62,    63,    64,    65,    66,
      67,    68,    69,    70,   368,   369,   357,     0,   370,   371,
     372,     0,   373,   374,     0,   360,     0,     0,     0,   361,
      72,   362,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    57,   363,     0,
     375,     0,     0,    75,   376,     0,     0,     0,     0,     0,
     377,   438,    78,   378,   379,   380,   381,     0,     0,     0,
       0,     0,     0,     0,     0,   364,   365,     0,   366,     0,
     367,    62,    63,    64,    65,    66,    67,    68,    69,    70,
     368,   369,   357,     0,   370,   371,   372,     0,   373,   374,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   375,     0,     0,    75,
     376,     0,     0,     0,     0,     0,   377,  1402,    78,   378,
     379,   380,   381,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,     0,     0,
      45,    46,     0,     0,     0,    47,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,    56,     0,     0,
      57,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    59,     0,     0,     0,
      60,    61,     0,     0,    62,    63,    64,    65,    66,    67,
      68,    69,    70,     0,     0,     0,    71,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    73,
      74,     0,    75,    76,     0,     0,     0,     0,     0,     0,
      77,    78,   242,   243,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,     0,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,  -415,  -415,     0,  -415,    45,
      46,     0,  -415,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    57,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      61,     0,     0,    62,    63,    64,    65,    66,    67,    68,
      69,    70,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    73,    74,
       0,    75,   244,     0,     0,     0,  -724,     0,     0,    77,
      78,     4,   243,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,     0,     0,    45,    46,
       0,     0,     0,    47,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,    56,     0,     0,    57,     0,
       0,     0,     0,  -347,  -347,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    60,     0,
       0,     0,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -347,     0,     0,     0,
      75,    76,     0,     0,     0,     0,     0,     0,    77,    78,
       4,   243,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,     0,     0,     0,     0,    45,    46,     0,
       0,     0,    47,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,    56,     0,     0,    57,     0,     0,
       0,     0,  -348,  -348,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    60,     0,     0,
       0,    62,    63,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -348,     0,     0,     0,    75,
      76,     0,     0,     0,     0,     0,     0,    77,    78,   242,
     243,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,     0,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,  -415,  -415,     0,  -415,    45,    46,     0,  -415,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    57,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    61,     0,     0,
      62,    63,    64,    65,    66,    67,    68,    69,    70,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    73,    74,     0,    75,   244,
       0,     0,  1315,     0,     0,     0,    77,    78,  1316,     0,
       0,    13,    14,    15,    16,    17,    18,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,     0,     0,     0,     0,    45,
      46,     0,     0,     0,    47,    48,    49,    50,    51,    52,
      53,    54,     0,     0,     0,     0,     0,     0,     0,    57,
    1317,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    60,
       0,     0,     0,    62,    63,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1318,     0,     0,
       0,    75,   919,     0,     0,  1315,     0,     0,     0,    77,
      78,  1316,     0,     0,    13,    14,    15,    16,    17,    18,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,     0,     0,
       0,     0,    45,    46,     0,     0,     0,    47,    48,    49,
      50,    51,    52,    53,    54,     0,     0,     0,     0,     0,
       0,     0,    57,  1317,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    60,     0,     0,     0,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1496,     0,     0,     0,    75,   919,     0,     0,  1315,     0,
       0,     0,    77,    78,  1316,     0,     0,    13,    14,    15,
      16,    17,    18,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,     0,     0,     0,     0,    45,    46,     0,     0,     0,
      47,    48,    49,    50,    51,    52,    53,    54,     0,     0,
       0,     0,     0,     0,     0,    57,  1317,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    60,     0,     0,     0,    62,
      63,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1497,     0,     0,     0,    75,   919,     0,
       0,  1315,     0,     0,     0,    77,    78,  1316,     0,     0,
      13,    14,    15,    16,    17,    18,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,     0,     0,    45,    46,
       0,     0,     0,    47,    48,    49,    50,    51,    52,    53,
      54,     0,     0,     0,     0,     0,     0,     0,    57,  1317,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    60,     0,
       0,     0,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1498,     0,     0,     0,
      75,   919,     0,     0,     0,     0,     0,     0,    77,    78,
     242,   243,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,     0,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,  -415,  -415,     0,  -415,    45,    46,     0,
    -415,     0,     0,     0,     0,     0,     0,     0,     0,    13,
      14,    15,    16,    17,     0,     0,    19,    57,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,  -414,  -414,     0,  -414,    45,    46,     0,
    -414,    62,    63,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    57,     0,     0,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   242,     0,     0,     0,     0,     0,    75,
     244,     0,    13,    14,    15,    16,    17,    77,    78,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,  -415,  -415,     0,  -415,
      45,    46,     0,  -415,     0,     0,     0,     0,     0,     0,
       0,     0,    13,    14,    15,    16,    17,     0,     0,    19,
      57,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,  -415,  -415,     0,  -415,
      45,    46,     0,  -415,    62,    63,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      57,     0,     0,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    75,   301,     0,     0,     0,     0,     0,     0,
      77,    78,   243,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,     0,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,  -415,  -415,     0,  -415,    45,    46,
       0,  -415,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    57,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    61,
       0,     0,    62,    63,    64,    65,    66,    67,    68,    69,
      70,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    73,    74,     0,
      75,   244,     0,     0,     0,  -728,     0,     0,    77,    78,
     243,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,     0,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,  -415,  -415,     0,  -415,    45,    46,     0,  -415,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    57,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    61,     0,     0,
      62,    63,    64,    65,    66,    67,    68,    69,    70,     0,
       0,     0,     0,     0,     0,     0,  1339,     0,     0,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    73,    74,   360,    75,   244,
       0,   361,     0,   362,     0,     0,    77,    78,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1107,     0,
     363,     0,     0,  1109,  1769,  1770,  1110,  1111,  1112,  1113,
    1114,  1115,  1116,  1117,  1118,  1119,  1120,  1121,  -294,  1122,
    1123,  1124,  1125,  1126,     0,  1127,     0,   364,   365,     0,
     462,     0,   367,  1128,  1129,    64,    65,    66,    67,    68,
      69,    70,   368,   369,   357,  1130,   370,   371,   372,     0,
     373,   374,     0,     0,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,  1339,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   375,     0,
       0,    75,   376,     0,     0,     0,   279,     0,   377,    77,
      78,   378,   379,   380,   381,   360,     0,     0,     0,   361,
       0,   362,     0,  -175,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1107,     0,   363,     0,
       0,  1109,     0,     0,  1110,  1111,  1112,  1113,  1114,  1115,
    1116,  1117,  1118,  1119,  1120,  1121,  -294,  1122,  1123,  1124,
    1125,  1126,     0,  1127,     0,   364,   365,     0,   462,     0,
     367,  1128,  1129,    64,    65,    66,    67,    68,    69,    70,
     368,   369,   357,  1130,   370,   371,   372,     0,   373,   374,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   375,     0,     0,    75,
     376,     0,     0,     0,   279,     0,   377,    77,    78,   378,
     379,   380,   381,     0,     0,     0,     0,     0,     0,     0,
       0,  -175,   243,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,     0,     0,    45,    46,
       0,     0,     0,   319,    48,    49,    50,    51,    52,    53,
      54,     0,    13,    14,    15,    16,    17,    18,    57,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,     0,     0,
      45,    46,    62,    63,     0,    47,    48,    49,    50,    51,
      52,    53,    54,     0,     0,     0,     0,     0,     0,     0,
      57,     0,     0,     0,     0,     0,     0,    72,     0,  1042,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -596,
      75,   320,     0,     0,    62,    63,     0,     0,    77,    78,
     243,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    75,     0,     0,     0,    45,    46,     0,     0,
       0,   319,    48,    49,    50,    51,    52,    53,    54,     0,
      13,    14,    15,    16,    17,    18,    57,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,     0,     0,    45,    46,
      62,    63,     0,   319,    48,    49,    50,    51,    52,    53,
      54,     0,     0,     0,     0,     0,     0,     0,    57,     0,
       0,     0,     0,     0,     0,    72,     0,  1745,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    75,   320,
       0,     0,    62,    63,     0,     0,    77,    78,   243,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      75,     0,     0,     0,    45,    46,     0,     0,     0,   319,
      48,    49,    50,    51,    52,    53,    54,     0,     0,     0,
       0,     0,     0,     0,    57,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    62,    63,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    72,     0,  1747,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    75,   320,     0,     0,
       0,     0,     0,     0,    77,    78,   243,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,     0,     0,
       0,     0,    45,    46,     0,     0,     0,   319,    48,    49,
      50,    51,    52,    53,    54,     0,     0,     0,     0,     0,
       0,     0,    57,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    75,   320,     0,     0,     0,     0,
       0,     0,    77,    78,   243,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,     0,     0,
      45,    46,     0,     0,     0,   319,    48,    49,    50,    51,
      52,    53,    54,     0,     0,     0,     0,     0,     0,     0,
      57,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    62,    63,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    75,   301,     0,     0,     0,     0,     0,     0,
      77,    78,   243,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,     0,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,  -415,  -415,     0,  -415,    45,    46,
       0,  -415,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    57,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      75,   244,     0,     0,     0,     0,     0,     0,    77,    78,
      13,    14,    15,    16,    17,    18,   659,    19,   660,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,   360,     0,    45,    46,
     361,     0,   362,    47,    48,    49,    50,    51,    52,    53,
      54,     0,     0,     0,     0,     0,     0,     0,    57,   363,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   661,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   364,   365,     0,   366,
       0,   367,    62,    63,    64,    65,    66,    67,    68,    69,
      70,   368,   369,   357,     0,   370,   371,   372,     0,   373,
     374,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   375,     0,     0,
      75,   662,     0,     0,     0,   279,     0,   377,    77,    78,
     663,   664,   380,   381,    13,    14,    15,    16,    17,    18,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,     0,     0,
     360,     0,    45,    46,   361,     0,   362,    47,    48,    49,
      50,    51,    52,    53,    54,     0,     0,     0,     0,     0,
       0,     0,    57,   363,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     364,   365,     0,   366,     0,   367,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   368,   369,   357,     0,   370,
     371,   372,     0,   373,   374,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   375,     0,   406,    75,   407,     0,     0,     0,     0,
       0,   377,    77,    78,   378,   379,   380,   381,    13,    14,
      15,    16,    17,    18,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,   360,     0,    45,    46,   361,     0,
     362,    47,    48,    49,    50,    51,    52,    53,    54,     0,
       0,     0,     0,     0,     0,     0,    57,   363,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   364,   365,     0,   366,     0,   367,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   368,
     369,   357,     0,   370,   371,   372,     0,   373,   374,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   375,     0,     0,    75,   662,
       0,     0,     0,   279,     0,   377,    77,    78,   378,   379,
     380,   381,    13,    14,    15,    16,    17,    18,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,   360,     0,
      45,    46,   361,     0,   362,    47,    48,    49,    50,    51,
      52,    53,    54,     0,     0,     0,     0,     0,     0,     0,
      57,   363,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   364,   365,
       0,   366,     0,   367,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   368,   369,   357,     0,   370,   371,   372,
       0,   373,   374,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   375,
       0,     0,    75,   407,     0,     0,     0,     0,     0,   377,
      77,    78,   378,   379,   380,   381,    13,    14,    15,    16,
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
     378,   379,   380,   381,    13,    14,    15,    16,    17,    18,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,     0,     0,
       0,     0,    45,    46,     0,     0,     0,    47,    48,    49,
      50,    51,    52,    53,    54,     0,     0,     0,     0,     0,
       0,     0,    57,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    61,     0,     0,    62,    63,    64,    65,
      66,    67,    68,    69,    70,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    73,    74,     0,    75,    76,     0,     0,     0,  -726,
       0,     0,    77,    78,    13,    14,    15,    16,    17,    18,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,     0,     0,
       0,     0,    45,    46,     0,     0,     0,    47,    48,    49,
      50,    51,    52,    53,    54,     0,     0,     0,     0,     0,
       0,     0,    57,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    61,     0,     0,    62,    63,    64,    65,
      66,    67,    68,    69,    70,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    73,    74,     0,    75,    76,     0,     0,     0,     0,
       0,     0,    77,    78,    13,    14,    15,    16,    17,    18,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,     0,     0,
       0,     0,    45,    46,     0,     0,     0,    47,    48,    49,
      50,    51,    52,    53,    54,     0,     0,     0,     0,     0,
       0,     0,    57,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    61,     0,     0,    62,    63,    64,    65,
      66,    67,    68,    69,    70,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    75,    76,     0,    13,    14,    15,
      16,    17,    77,    78,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,  -415,  -415,     0,  -415,    45,    46,     0,  -415,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    57,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    61,     0,     0,    62,
      63,    64,    65,    66,    67,    68,    69,    70,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    73,    74,     0,    75,   301,     0,
       0,     0,     0,     0,     0,    77,    78,   556,   243,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
       0,     0,     0,     0,    45,    46,     0,     0,     0,    47,
      48,    49,    50,    51,    52,    53,    54,     0,    13,    14,
      15,    16,    17,    18,    57,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,     0,     0,    45,    46,    62,    63,
       0,    47,    48,    49,    50,    51,    52,    53,    54,     0,
      13,    14,    15,    16,    17,    18,    57,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,    75,     0,    45,    46,
      62,    63,     0,    47,    48,    49,    50,    51,    52,    53,
      54,     0,     0,     0,     0,     0,     0,     0,    57,     0,
       0,     0,     0,     0,  1416,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   925,    75,   919,
       0,     0,    62,    63,     0,     0,    77,    78,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      75,   919,     0,     0,     0,     0,     0,     0,    77,    78,
      13,    14,    15,    16,    17,    18,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,     0,     0,    45,    46,
       0,     0,     0,    47,    48,    49,    50,    51,    52,    53,
      54,     0,    13,    14,    15,    16,    17,    18,    57,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,     0,     0,
      45,    46,    62,    63,     0,    47,    48,    49,    50,    51,
      52,    53,    54,     0,     0,     0,     0,     0,     0,     0,
      57,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      75,   286,     0,     0,    62,    63,     0,     0,    77,    78,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    75,    76,     0,     0,     0,     0,     0,     0,
      77,    78,    13,    14,    15,    16,    17,    18,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,     0,     0,
      45,    46,     0,     0,     0,    47,    48,    49,    50,    51,
      52,    53,    54,     0,    13,    14,    15,    16,    17,    18,
      57,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,     0,     0,
       0,     0,    45,    46,    62,    63,     0,   319,    48,    49,
      50,    51,    52,    53,    54,     0,     0,     0,     0,     0,
       0,     0,    57,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    75,   433,     0,     0,    62,    63,     0,     0,
      77,    78,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    75,   320,     0,     0,     0,     0,
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
       0,     0,     0,     0,    75,   286,     0,     0,    62,    63,
       0,     0,    77,    78,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    75,   433,     0,     0,
       0,     0,     0,     0,    77,    78,   242,   243,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
       0,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,  -415,
    -415,     0,  -415,    45,    46,     0,  -415,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    13,    14,    15,
      16,    17,    18,    57,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,     0,     0,     0,     0,    45,    46,    62,    63,     0,
     319,    48,    49,    50,    51,    52,    53,    54,     0,    13,
      14,    15,    16,    17,    18,    57,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,     0,     0,    75,     0,    45,    46,    62,
      63,     0,    47,    48,    49,    50,    51,    52,    53,    54,
       0,     0,     0,     0,     0,     0,     0,    57,     0,     0,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    75,   301,     0,
       0,    62,    63,     0,     0,    77,    78,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    75,
     919,     0,     0,     0,     0,     0,     0,    77,    78,    13,
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
     919,     0,     0,    62,    63,     0,     0,    77,    78,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    75,     0,     0,    13,    14,    15,    16,    17,    77,
      78,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,  -415,  -415,
       0,  -415,    45,    46,     0,  -415,     0,     0,     0,     0,
       0,     0,     0,     0,    13,    14,    15,    16,    17,     0,
       0,    19,    57,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,  -415,  -415,
       0,  -415,    45,    46,     0,  -415,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    57,     0,     0,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    75,   301,    62,    63,     0,     0,
       0,     0,    77,    78,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    75,     0,     0,     0,     0,     0,
       0,     0,    77,    78,   243,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,     0,     0,
      45,    46,     0,     0,     0,   319,    48,    49,    50,    51,
      52,    53,    54,     0,     0,     0,     0,     0,     0,     0,
      57,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    62,    63,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   845,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -609,    75,   243,     6,     7,     8,     9,    10,    11,
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
    1672,     0,     0,     0,     0,   243,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
      19,    75,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,     0,     0,     0,
       0,    45,    46,     0,     0,     0,   319,    48,    49,    50,
      51,    52,    53,    54,     0,     0,     0,     0,     0,     0,
       0,    57,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   243,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    62,    63,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,  -415,  -415,     0,  -415,    45,    46,
       0,  -415,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    75,     0,     0,     0,     0,    57,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    62,    63,     0,     0,     0,     0,    13,    14,
      15,    16,    17,    18,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,     0,     0,    45,    46,     0,     0,
      75,    47,    48,    49,    50,    51,    52,    53,    54,     0,
       0,     0,     0,     0,     0,     0,    57,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    61,     0,     0,
      62,    63,    64,    65,    66,    67,    68,    69,    70,     0,
       0,     0,    13,    14,    15,    16,    17,     0,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,  -415,  -415,    75,  -415,
      45,    46,     0,  -415,     0,     0,   360,     0,     0,     0,
     361,     0,   362,     0,     0,     0,     0,     0,     0,     0,
      57,     0,     0,     0,     0,     0,     0,     0,     0,   363,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    62,    63,   364,   365,     0,   366,
       0,   367,    62,    63,    64,    65,    66,    67,    68,    69,
      70,   368,   369,   357,     0,   370,   371,   372,   360,   373,
     374,     0,   361,     0,   362,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   363,    75,     0,     0,     0,     0,   375,   950,  1523,
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
       0,   375,   791,     0,    75,   376,     0,     0,     0,     0,
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
       0,     0,     0,     0,     0,   375,   950,     0,    75,   376,
       0,     0,     0,     0,     0,   377,    77,    78,   378,   379,
     380,   381,     0,     0,     0,     0,   364,   365,     0,   366,
       0,   367,    62,    63,    64,    65,    66,    67,    68,    69,
      70,   368,   369,   357,     0,   370,   371,   372,   360,   373,
     374,     0,   361,     0,   362,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   363,     0,     0,     0,     0,     0,   375,     0,     0,
      75,   376,     0,     0,   981,     0,     0,   377,    77,    78,
     378,   379,   380,   381,     0,     0,     0,     0,   364,   365,
       0,   366,     0,   367,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   368,   369,   357,     0,   370,   371,   372,
     360,   373,   374,     0,   361,     0,   362,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   363,     0,     0,     0,     0,     0,   375,
    1289,     0,    75,   376,     0,     0,     0,     0,     0,   377,
      77,    78,   378,   379,   380,   381,     0,     0,     0,     0,
     364,   365,     0,   366,     0,   367,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   368,   369,   357,     0,   370,
     371,   372,   360,   373,   374,     0,   361,     0,   362,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   363,     0,     0,     0,     0,
       0,   375,     0,     0,    75,   376,     0,     0,     0,  1349,
       0,   377,    77,    78,   378,   379,   380,   381,     0,     0,
       0,     0,   364,   365,     0,   366,     0,   367,    62,    63,
      64,    65,    66,    67,    68,    69,    70,   368,   369,   357,
       0,   370,   371,   372,   360,   373,   374,     0,   361,     0,
     362,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   363,     0,     0,
       0,     0,     0,   375,     0,  1775,    75,   376,     0,     0,
       0,     0,     0,   377,    77,    78,   378,   379,   380,   381,
       0,     0,     0,     0,   364,   365,     0,   366,     0,   367,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   368,
     369,   357,     0,   370,   371,   372,   360,   373,   374,     0,
     361,     0,   362,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   363,
       0,     0,     0,     0,     0,   375,  1955,     0,    75,   376,
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
       0,     0,     0,   656,     0,     0,    75,   376,     0,     0,
       0,     0,     0,   377,    77,    78,   378,   379,   380,   381,
       0,     0,     0,     0,   364,   365,     0,   366,     0,   367,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   368,
     369,   357,     0,   370,   371,   372,   360,   373,   374,     0,
     361,     0,   362,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   363,
       0,     0,     0,     0,     0,   375,     0,     0,    75,   376,
       0,     0,     0,     0,     0,   377,   859,    78,   378,   379,
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
       1,    73,   173,     4,   633,   465,   220,   162,    73,     1,
     601,    73,   283,   178,    73,   686,   222,   241,   408,   178,
     219,   256,   219,   323,   868,    95,   673,   162,   375,   614,
     177,     4,   666,   338,   730,    82,   757,   342,    75,    58,
     514,   515,   219,   605,   759,     0,   854,   150,  1116,   219,
    1199,     1,   759,   229,    55,    56,   219,    58,  1274,  1275,
     601,    95,   854,  1647,    73,  1647,    58,     1,    73,    73,
       4,   207,    73,   956,    73,    70,  1647,   838,   254,   162,
      70,    82,   296,   601,   290,   291,   139,   757,   264,    90,
      82,    59,    60,     1,    95,   757,   295,    98,   295,  1710,
     147,   102,   985,    95,   865,   219,    98,     1,    58,   181,
     102,   760,   777,   757,  1341,     1,    82,   766,   295,   181,
      82,     1,   181,  1769,    58,   295,   145,   630,   163,   102,
     795,  1090,   295,   236,   104,   105,  1095,   219,   191,   140,
     174,   525,   143,     1,   145,   445,     4,   219,    98,   150,
     220,   535,    98,   145,   219,   156,   131,   219,   153,   149,
     219,   635,   163,   109,    58,  1048,   757,   131,   102,    96,
     174,     0,   181,  1648,   115,     1,   181,    70,    58,   180,
     181,   295,   181,   149,   352,   147,     1,   355,   180,   157,
     165,    87,   576,    70,   195,   145,   861,   759,   245,  1773,
      58,   165,     0,   195,   205,   151,   174,  1166,   102,   210,
     219,   145,   153,   295,   219,   146,   757,   179,   219,   220,
     219,   152,   149,   295,    82,  1232,   296,   130,   220,   321,
     295,     1,    58,   295,     4,   236,   295,   274,   321,   757,
    1886,   631,   173,    58,   245,   916,   338,   155,   156,   195,
     342,   145,   115,   245,   255,   302,   149,   258,   149,  1408,
     153,   157,   497,   737,   265,   145,   258,   316,     1,   172,
     156,   149,   149,   944,   275,   276,   153,   278,   492,   782,
     909,   139,  1757,   245,   483,   590,   483,   145,    58,   147,
    1011,   562,   607,   593,   295,   296,   776,   777,   994,   933,
    1108,  1100,   303,  1018,   296,   354,   483,    70,   258,   310,
     311,  1018,   258,   483,   315,   795,  1108,   617,    87,   145,
     483,  1905,   627,  1905,   624,    58,    19,   889,    98,  1137,
     145,  1092,   102,   191,  1905,   471,  1947,   684,  1341,  1342,
     302,  1011,   288,  1570,    87,  1137,   347,   439,   294,  1011,
    1309,   352,  1926,   375,   355,   356,   439,   571,   534,   321,
      70,  1889,   409,  1012,   131,   120,    70,  1011,   155,   483,
     569,   146,   569,   323,  1902,   145,   148,   323,   165,  1953,
    1855,   861,   435,   155,   242,     4,   149,   245,   157,   283,
     153,   192,   569,   160,   161,  1060,   131,   152,   173,   569,
    1928,   483,    70,   283,   155,  1100,   569,   910,   443,   512,
     149,   483,   145,    70,   157,   518,  1632,  1633,   419,   149,
    1011,   483,   492,   174,   483,   283,   463,   419,  1435,  1436,
    1437,  1906,   228,    70,   173,   231,    55,    56,   573,   149,
      70,   442,   443,   153,   302,   149,   761,   409,   157,   153,
     765,   835,   157,   454,   455,   569,  1018,   253,   149,   774,
     775,   149,   463,  1938,   465,   174,   157,   263,   283,   174,
    1011,    90,    75,    76,   483,  1543,   211,   439,   483,  1547,
      70,   149,   483,   149,   483,   153,    70,   569,   258,   149,
     573,   492,   149,  1011,  1100,   445,   153,   569,   590,   445,
     492,   571,    12,    13,    14,    15,    16,   569,    75,    76,
     569,   512,   149,  1312,  1313,  1314,   153,   518,   154,   149,
     155,   140,   614,   153,   143,    70,    87,    70,   988,    73,
     165,   614,   149,  1341,  1342,   627,   149,   156,   151,  1188,
     157,   104,   105,    73,   163,    89,   846,  1430,   174,   284,
     283,   409,   514,   515,   157,   556,   575,   558,    88,   149,
      70,   155,   106,   153,   711,   149,  1195,  1570,   569,   153,
     571,   876,    70,   519,   575,   149,   174,   435,   878,   571,
    1060,   265,   156,   575,   585,     3,    70,    82,   589,   152,
     157,   210,   276,   539,   155,  1242,   157,  1182,   125,   126,
      95,  1817,     3,    98,   149,   770,   149,   102,   153,   559,
     153,   770,   151,   559,  1263,   152,   781,  1312,  1313,  1314,
     621,   131,  1690,   162,   163,   575,   648,    70,   650,   651,
     155,   653,   633,    70,   656,   155,   255,   659,   660,   661,
     375,   575,   169,   170,   155,   151,   265,   593,   784,   174,
     156,   149,   614,   146,   174,   153,   275,   276,   149,   278,
    1331,  1505,   153,   174,   157,   149,   560,   617,   562,   153,
     817,   617,   155,   635,   155,   712,   677,   678,   624,   680,
     173,   575,   562,   684,   303,   180,   687,  1496,  1497,  1498,
    1693,   310,   311,   174,   797,   575,   315,   157,   556,  1202,
     195,  1175,   155,   155,   562,  1352,  1312,  1313,  1314,   505,
    1778,   712,   149,   816,   104,   105,   153,   575,  1786,   520,
    1035,   174,   174,   155,   759,   220,   155,   155,   347,   157,
     151,   527,  1615,   352,  1617,   156,   355,   533,   155,   151,
     149,   537,   174,   157,   156,   174,  1249,   562,    56,   575,
     245,    59,    60,   151,    62,   153,   757,   174,   759,   151,
     575,   129,  1570,   258,   156,   160,  1226,   151,   152,   504,
     771,   907,   167,   168,   509,   737,   591,   778,  1846,   173,
    1427,   149,   148,   784,   876,   153,   787,   162,   163,   155,
     591,   526,   160,   161,   929,   796,   797,   798,   157,   103,
     149,   536,   129,   107,   150,   575,   110,   129,   112,  1199,
     151,   157,  1047,   171,   155,   816,   981,    12,    13,    14,
      15,    16,   149,   442,   123,   124,   153,   149,   573,   562,
     149,   153,  1835,   160,   161,   454,   455,   149,   160,   161,
     115,   149,   575,   152,   153,   151,   929,   156,     3,  1852,
     151,   852,   853,   854,   155,   127,   128,    12,    13,    14,
      15,    16,   143,   144,   145,   900,   151,    12,    13,    14,
      15,    16,   151,  1079,   155,    70,     4,     5,     6,     7,
       8,     9,    10,    11,   165,  1693,  1186,  1890,   151,  1349,
     151,   151,  1057,   174,   867,   155,   846,   151,   151,   900,
     846,   155,  1405,   904,   854,   151,   868,   857,   909,   155,
     129,   857,   713,   648,   915,    70,   154,   155,   653,  1055,
     854,   656,   151,  1426,   419,    70,   155,   149,   878,   151,
     149,   153,   878,   867,   153,   149,   131,   556,   149,   754,
     675,   160,   161,    21,   248,    46,    47,   948,    49,   691,
     692,   693,    53,   754,   173,   956,   154,   155,    56,   151,
     854,    59,    60,   155,    62,  1425,   585,  1191,     3,  1316,
     589,  1600,   160,   161,   854,   776,   777,    12,    13,    14,
      15,    16,  1017,  1018,   985,   154,   155,   988,   151,   143,
     144,   145,   155,   155,   795,   155,   854,   492,   155,   149,
     151,   155,   621,   153,   155,  1176,  1177,   149,   149,   867,
    1011,   165,   153,   149,   633,   149,  1017,  1018,  1408,   153,
     174,   325,   326,   151,   328,   151,   330,  1835,   854,   155,
     155,   776,   777,   151,   149,    70,   151,   155,   153,   854,
      96,   837,   154,   155,  1852,   155,   174,  1048,   157,  1552,
     795,   151,   149,  1100,   151,   155,   153,   149,   677,   678,
     861,   680,   149,   149,   157,   684,  1169,   153,   687,  1304,
    1341,  1097,  1098,  1099,  1209,    12,    13,    14,    15,    16,
    1245,   149,  1890,   151,   854,   153,    87,   822,   151,   148,
    1182,   157,   155,   712,   154,  1724,   151,   867,   833,  1182,
     118,   836,   120,   121,   122,   840,  1609,   151,  1322,  1110,
     157,   155,  1113,  1114,  1115,   151,   861,  1282,  1283,   155,
    1275,   854,   173,  1282,  1283,   157,  1209,   157,   151,  1800,
     151,   149,   155,    70,   152,   153,  1137,   151,   115,   157,
     158,   155,  1143,    12,    13,    14,    15,    16,   154,   155,
    1151,   151,   771,  1154,  1155,   155,   151,  1158,  1108,   778,
     155,   151,  1154,  1155,   149,   155,   154,   155,  1169,   108,
     109,   110,   111,   112,  1108,   149,   151,   796,   149,   798,
     155,   151,  1155,   155,   929,   155,   151,  1137,   166,   151,
     155,  1213,   129,   155,  1195,    12,    13,    14,    15,    16,
      17,    70,   151,  1137,  1154,   158,   155,  1910,  1154,  1210,
     158,  1914,   149,  1175,  1108,   161,   153,   155,   156,   159,
    1182,  1155,  1725,   160,   161,  1226,   171,   531,  1108,   154,
     155,  1232,   129,   852,   853,   854,  1186,   154,   155,   152,
    1186,   151,  1100,  1137,   151,    12,    13,    14,    15,    16,
    1108,   151,  1322,   154,   155,  1390,   151,  1137,   151,  1060,
     129,  1155,   151,  1264,   154,  1312,  1313,  1314,   153,  1316,
    1317,     4,     5,     6,     7,     8,     9,    10,    11,  1137,
     149,   900,  1108,   131,   153,   904,   154,   155,   363,  1503,
     909,   160,   161,  1108,   154,   155,   131,  1511,   156,  1570,
     154,   155,   155,    70,   156,  1808,   156,  1390,   154,   155,
    1509,  1137,  1509,   388,   389,  1060,   149,     9,   154,   155,
     149,  1322,  1137,   154,   155,  1326,   154,   155,  1329,   151,
      63,   151,  1509,   151,   409,   154,   155,   153,  1108,  1509,
     154,   155,  1315,   154,   155,   151,  1509,   101,  1349,   151,
     104,   105,   106,   107,   108,   109,   110,   111,   112,  1094,
     155,   156,   129,   151,   439,   154,   155,  1137,  1369,   154,
    1371,   157,  1107,   154,   155,  1108,   154,  1323,   157,  1371,
     157,  1315,   149,   157,  1154,  1155,   153,    75,    76,  1124,
      68,  1341,  1342,   160,   161,  1509,     4,     5,     6,     7,
       8,     9,    10,    11,  1137,   149,   160,  1341,  1342,   101,
     155,   156,   104,   105,   106,   107,   108,   109,   110,   111,
     112,  1371,  1569,    76,  1425,  1371,  1581,  1509,   154,  1430,
    1233,  1234,    17,  1503,  1435,  1436,  1437,   694,   695,   173,
    1413,  1511,  1516,  1517,  1509,   155,  1581,  1341,  1342,   696,
     697,  1665,   702,   703,  1312,  1313,  1314,  1315,  1316,  1317,
     157,  1341,  1342,  1662,  1209,  1662,    12,    13,    14,    15,
      16,    17,   149,  1274,  1275,  1176,  1177,   151,  1633,  1413,
     174,   151,   157,  1341,  1342,  1662,   698,   699,   700,   701,
     174,  1110,  1662,   157,  1113,  1114,  1115,   154,  1581,  1662,
     154,    17,  1503,   148,   151,   151,   157,   151,  1509,   151,
    1511,    12,    13,    14,    15,    16,    17,  1518,  1137,   151,
     151,   151,   151,   148,  1143,     3,  1341,  1342,   151,  1274,
     157,  1532,  1151,    12,    13,    14,    15,    16,    17,  1158,
    1541,   157,    68,  1505,   151,  1315,   174,   173,  1662,   151,
     151,   148,  1766,   157,   101,  1413,  1856,   151,  1705,   106,
     107,   108,   109,   110,   111,   112,   155,  1540,   151,   151,
     151,  1341,  1342,   151,   151,   155,  1195,  1578,   151,   151,
    1662,   151,   151,   151,   151,  1607,   151,   151,   151,  1539,
     151,  1210,   151,  1539,   151,  1665,   154,  1332,  1333,  1600,
     151,  1371,   149,   150,  1769,   148,  1540,   151,  1341,  1342,
    1769,  1584,   151,  1716,  1615,   690,  1617,   173,   148,   151,
    1570,   155,   149,   101,   149,   149,   104,   105,   106,   107,
     108,   109,   110,   111,   112,  1370,  1570,   149,   149,   149,
      13,    73,   156,  1413,    72,  1390,  1540,   101,   155,  1796,
    1584,   129,   106,   107,   108,   109,   110,   111,   112,  1154,
    1155,  1662,  1817,    95,  1665,   174,    89,   156,   174,   154,
     154,   149,   150,   148,   174,  1676,  1570,   174,   156,  1680,
     148,     9,   160,   161,   157,   155,   174,   151,   154,   151,
    1570,  1692,  1906,   151,   155,   155,  1766,  1698,   155,   154,
    1792,   151,   151,   148,   148,   174,  1905,  1326,  1905,   149,
    1329,   174,  1570,   149,  1715,  1716,    78,   148,   150,  1884,
    1521,  1886,   174,  1724,  1938,  1884,  1584,  1886,  1905,   149,
     149,   174,  1682,   174,   151,  1905,  1682,   174,   148,   174,
     174,   174,  1905,  1693,   148,  1792,   155,  1912,     1,   155,
    1369,     4,  1855,  1912,   154,  1570,   154,   154,   151,  1693,
     154,  1853,   157,  1910,   148,  1766,   151,  1914,  1915,  1539,
    1540,  1936,  1773,   101,   156,   156,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   118,   151,   219,   220,   148,
     151,  1905,   156,  1940,   151,   151,  1797,   151,   148,  1693,
    1570,   154,   154,   174,   236,    58,  1853,   155,   151,   149,
     151,   155,   149,  1693,  1584,  1962,  1981,   107,   149,  1966,
      73,   157,  1981,  1905,   154,   154,  1561,   154,   148,    82,
     148,  1632,  1633,  1905,   151,  1693,  1906,  1570,   151,  1840,
    1905,  1988,    95,  1905,   151,    98,  1905,  1848,   151,   102,
     925,   151,   154,   151,  1855,   930,  1857,   148,   174,   174,
     149,    88,     1,   295,   296,     4,   941,  1868,  1938,  1870,
    1871,   151,   151,   205,   148,   174,  1371,   154,  1693,   154,
     148,   151,   151,    73,   151,  1835,   139,  1632,   151,   151,
     151,  1892,   145,   153,   147,   152,   174,   150,   151,  1518,
      73,  1835,  1852,   174,  1905,  1906,   156,   151,   174,   162,
    1856,   148,  1682,  1532,  1906,  1916,   174,   151,  1852,    58,
     151,   151,  1541,  1693,   148,  1926,   179,   180,   181,    12,
      13,    14,    15,    16,   155,   150,   150,  1938,   191,   192,
    1890,  1835,   195,    82,   148,   156,  1938,   101,   149,   155,
    1951,    73,  1953,   149,   148,  1835,  1890,   165,  1852,  1578,
    1693,   174,   165,   102,   174,   154,   219,   220,  1969,   107,
     107,   151,  1852,   156,  1975,   151,   148,  1835,   148,   174,
     151,  1600,   149,   236,  1985,    73,   151,    70,  1989,   151,
     174,   377,   245,   174,  1852,  1607,  1890,  1331,  1999,  1239,
     139,   704,   665,   705,   708,   258,   145,   706,   147,   707,
    1890,   443,  1126,  1137,  1953,  1570,  1817,  1886,   101,  1697,
    1835,   104,   105,   106,   107,   108,   109,   110,   111,   112,
    1902,  1693,  1890,  1108,  1781,   288,  1948,  1852,  1947,   823,
     179,   294,   295,   296,  1801,  1935,   129,  1562,  1562,   302,
    1915,   483,   191,  1853,  1158,  1966,    48,  1676,  1852,   250,
     492,  1680,  1503,  1766,  1825,  1835,   149,   150,   321,   322,
     323,  1517,  1317,  1692,  1151,  1890,   784,   160,   161,  1698,
     512,   472,  1852,   585,   871,   338,   518,  1413,  1584,   342,
     915,     0,   729,   729,   729,    -1,  1715,   881,    -1,    -1,
     230,    -1,  1835,   242,    -1,  1724,   245,  1182,    -1,    -1,
      -1,   250,    -1,    -1,    -1,    12,    -1,    -1,    -1,  1852,
    1890,    -1,   375,    -1,    -1,    -1,   558,    63,    64,    65,
      66,   463,    -1,   465,    -1,    -1,    -1,   569,    -1,   571,
      -1,  1216,  1217,  1218,   283,    -1,    -1,    -1,  1223,  1224,
      -1,    -1,    -1,    -1,  1773,    -1,   409,  1890,    -1,   412,
      -1,    -1,    -1,   302,    -1,   101,   419,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,    -1,  1797,    -1,
      -1,    -1,   435,    -1,    -1,    -1,   439,    -1,    -1,    86,
     443,   101,   445,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,    -1,   101,    -1,   990,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    -1,   153,    -1,    -1,
      -1,  1840,   192,  1007,  1008,    -1,  1961,    -1,    -1,  1848,
     483,    -1,    -1,    -1,    -1,   171,   375,    -1,  1857,   492,
      -1,    -1,  1977,    -1,    -1,    -1,    -1,    -1,    -1,  1868,
      -1,  1870,  1871,    -1,    -1,    76,    -1,    -1,    -1,   512,
      -1,   514,   515,    -1,   174,   518,    -1,   520,    -1,    -1,
     409,    -1,    -1,  1892,    -1,    -1,    -1,    -1,    -1,    -1,
     101,    -1,   412,   104,   105,   106,   107,   108,   109,   110,
     111,   112,    -1,    -1,    -1,    -1,   435,  1916,   428,    -1,
      -1,   431,    -1,   101,    -1,   558,    -1,  1926,   106,   107,
     108,   109,   110,   111,   112,   113,   569,    -1,   571,    -1,
     573,    -1,   575,    -1,    -1,   757,    -1,   759,    -1,    -1,
      -1,    -1,  1951,    -1,  1953,    -1,    -1,   590,   591,    -1,
     593,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   601,    -1,
    1969,   321,   605,   174,   324,   153,  1975,    -1,   488,    -1,
      -1,   614,    -1,    -1,    -1,   797,  1985,    -1,   338,    -1,
    1989,   624,   342,    -1,   627,   514,   515,    -1,    -1,    -1,
    1999,    -1,   635,    -1,   816,    -1,    -1,    -1,    70,    -1,
      -1,    -1,    -1,    -1,    -1,   648,    -1,   650,   651,    17,
     653,    -1,    -1,   656,    -1,   101,   659,   660,   661,    -1,
     106,   107,   108,   109,   110,   111,   112,   556,    -1,   101,
      -1,   560,    -1,   562,   106,   107,   108,   109,   110,   111,
     112,    -1,    -1,   129,    -1,    -1,   575,    -1,    -1,    -1,
      -1,    59,    60,    61,    62,    -1,    -1,   129,    -1,  1110,
      -1,    -1,   784,   149,   150,   787,    -1,   153,    -1,    -1,
     713,    -1,    -1,    -1,   160,   161,    -1,   149,   150,   439,
      -1,  1255,  1256,    -1,    -1,    -1,   729,   730,   160,   161,
      -1,    -1,    -1,   101,   737,  1269,  1270,    17,   106,   107,
     108,   109,   110,   111,   112,   113,   635,    -1,    -1,    -1,
    1575,   754,    -1,    -1,   757,    -1,   759,    -1,    -1,   648,
      -1,   650,   651,    -1,   653,    -1,   551,   656,  1302,  1303,
     659,   660,   661,   776,   777,    55,    56,    57,    58,    59,
      60,    61,    62,    -1,    -1,   153,     1,    -1,    -1,     4,
      -1,    -1,   795,    -1,   797,    -1,    -1,    -1,    -1,   101,
     520,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,    -1,    -1,   816,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    12,    13,    14,    15,    16,    -1,    -1,  1011,
      -1,    -1,    -1,    -1,    -1,  1017,  1018,    -1,    -1,    -1,
      -1,    -1,    -1,    58,    -1,    -1,    -1,    -1,   737,   729,
     730,   854,    -1,   573,    -1,    -1,    -1,    -1,   861,   739,
      -1,    -1,   742,    -1,   867,   868,   948,    82,    -1,    -1,
     590,   591,   174,   876,   956,   878,    -1,    -1,    -1,    -1,
      70,    -1,    -1,    98,    -1,    -1,   889,   102,    -1,    -1,
      -1,   101,    -1,    -1,   614,    -1,   106,   107,   108,   109,
     110,   111,   112,   985,    -1,    -1,   988,   627,    -1,    -1,
      -1,   101,    -1,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   803,   139,    -1,   929,   807,    -1,    -1,
     145,   811,   147,    -1,    -1,    -1,   151,    -1,    -1,   129,
     150,    -1,    -1,   153,    -1,    -1,   161,   162,   163,    -1,
      -1,  1485,  1486,    12,    13,    14,    15,    16,  1369,   149,
     150,    70,    -1,   153,   179,   854,  1048,    -1,    -1,    -1,
     160,   161,    -1,    -1,  1799,    -1,   191,   192,   867,   868,
     195,    -1,    -1,    -1,    -1,    -1,    -1,  1169,    -1,    -1,
      -1,   994,   101,   713,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,    -1,    -1,    -1,  1011,    -1,
      -1,    70,    -1,   101,    -1,  1018,   104,   105,   106,   107,
     108,   109,   110,   111,   112,    -1,    -1,   242,    -1,    -1,
     245,    -1,    -1,    -1,   754,    -1,    -1,    -1,   823,    -1,
     149,   150,   101,   258,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,    -1,    -1,  1060,  1592,    -1,
     275,    -1,    -1,    -1,   152,    -1,    -1,    -1,   283,   157,
     129,    -1,    -1,   288,    -1,    -1,    -1,    -1,    -1,   294,
      -1,    -1,  1264,    -1,    -1,    -1,    -1,   302,  1622,    -1,
     149,   150,    -1,  1627,  1628,    -1,   881,  1100,    -1,    -1,
      -1,   160,   161,    -1,    -1,  1108,   321,    -1,   323,   324,
      -1,    -1,    -1,    -1,   994,    -1,    -1,    -1,    -1,    -1,
      -1,  1532,    -1,   338,    -1,    -1,    -1,   342,    -1,    -1,
    1541,    -1,    -1,    -1,  1137,    -1,    -1,    -1,    -1,    -1,
    1322,    -1,    -1,    -1,  1226,    12,    13,    14,    15,    16,
    1232,  1154,  1155,    -1,    -1,    -1,   876,    -1,    -1,   879,
     375,    -1,    -1,    -1,    -1,    -1,  1169,  1578,    -1,    -1,
      -1,    -1,  1175,    -1,    -1,    -1,    -1,    -1,    -1,  1182,
      -1,    -1,    -1,    -1,    -1,  1065,    -1,    -1,  1068,    -1,
      -1,    -1,    -1,    -1,   409,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    70,    -1,   990,  1209,    -1,    -1,   929,
    1213,  1100,    -1,    -1,    -1,    -1,    70,    -1,    -1,  1108,
     435,    -1,  1007,  1008,   439,    -1,    -1,    -1,    -1,    -1,
     445,    -1,    -1,    -1,   101,    -1,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    -1,   101,  1137,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,    -1,
      -1,    -1,   129,    -1,    -1,  1676,  1155,  1349,    -1,  1680,
      -1,  1274,  1275,    -1,    -1,   129,    -1,    -1,    -1,    -1,
      -1,  1692,   149,   150,    -1,    -1,  1175,  1698,    -1,    -1,
      -1,    -1,    -1,   160,   161,   149,   150,    -1,    -1,   514,
     515,    -1,    -1,    -1,   519,   520,   160,   161,    -1,  1312,
    1313,  1314,  1315,  1316,  1317,    -1,    -1,    -1,    -1,  1322,
    1323,  1503,    -1,    -1,  1213,    -1,    -1,  1509,    -1,  1511,
      -1,    -1,    -1,    -1,    -1,    -1,   551,    -1,  1341,  1342,
      -1,   556,    -1,  1425,   559,   560,    -1,   562,  1430,    -1,
      -1,    -1,    -1,  1435,  1436,  1437,    -1,    -1,   573,    -1,
     575,    -1,  1773,    -1,    -1,    -1,    -1,    -1,  1371,    -1,
      -1,  1251,    -1,    -1,   589,   590,   591,    -1,   593,    -1,
    1260,    -1,    -1,    -1,    -1,   101,  1797,  1390,    -1,  1923,
     106,   107,   108,   109,   110,   111,   112,   113,    -1,   614,
      -1,   117,   617,   119,    -1,    -1,   621,    -1,    -1,   624,
    1413,    -1,   627,    -1,   629,    -1,    -1,    -1,    -1,    -1,
     635,    -1,    -1,  1312,  1313,  1314,  1315,  1316,  1317,  1840,
      -1,    -1,    -1,   648,   150,   650,   651,  1848,   653,    -1,
      -1,   656,    -1,    -1,   659,   660,   661,    -1,    -1,    -1,
      -1,    -1,  1341,  1342,    -1,    -1,    -1,  1868,    -1,  1870,
    1871,    -1,  1182,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1255,  1256,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1662,  1892,    -1,  1665,  1269,  1270,    -1,    -1,    -1,  1209,
      -1,    -1,    -1,    -1,    -1,    -1,  1499,    -1,   713,    -1,
    1503,    -1,  1505,    -1,    -1,  1916,  1509,    -1,  1511,    -1,
      70,    -1,    -1,    -1,    -1,  1926,    -1,  1302,  1303,    -1,
      -1,    -1,   737,    -1,  1413,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1615,  1716,  1617,  1539,  1540,    -1,   754,
    1951,   101,  1953,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,    -1,    -1,    -1,    -1,    -1,  1969,    -1,
      -1,   776,   777,    -1,  1975,    -1,   375,  1570,    -1,   129,
      -1,    -1,    -1,    -1,  1985,    -1,    -1,    -1,  1581,    -1,
     795,  1584,    -1,    -1,  1766,    -1,    -1,    -1,    -1,   149,
     150,    -1,    -1,   153,    -1,    -1,    -1,    -1,    -1,    -1,
     160,   161,    -1,    -1,  1607,    -1,    -1,    -1,   823,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1505,   101,    -1,  1499,
     104,   105,   106,   107,   108,   109,   110,   111,   112,  1632,
    1633,   846,    -1,    12,    13,    14,    15,    16,    -1,   854,
      -1,    -1,   857,    -1,  1647,  1648,   861,    -1,    -1,    -1,
      -1,  1540,   867,   868,    -1,    -1,    -1,    -1,    -1,  1662,
      -1,   876,  1665,   878,   879,    -1,   881,    -1,    -1,   153,
    1390,    -1,    -1,  1855,    -1,    -1,    -1,    -1,    -1,  1682,
      -1,  1570,    -1,     1,    -1,    -1,     4,    -1,    -1,    -1,
    1693,    70,    -1,    -1,    -1,  1584,    -1,    -1,    -1,    -1,
    1485,  1486,    -1,    -1,    -1,   514,   515,    70,    -1,    -1,
      -1,    -1,    -1,  1716,   929,    -1,    -1,    -1,  1607,    -1,
      -1,    -1,   101,  1905,  1906,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,    -1,  1521,    -1,   101,    -1,
      58,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     129,    -1,    -1,    -1,  1757,    -1,  1938,    -1,    -1,    -1,
      -1,    -1,    -1,  1766,    82,    -1,   129,  1647,  1648,    -1,
     149,   150,    -1,    -1,   153,   990,    -1,    -1,    -1,    -1,
      -1,   160,   161,    -1,   102,    -1,   149,   150,    -1,  1792,
      -1,    -1,  1007,  1008,    -1,    -1,    -1,   160,   161,    -1,
      -1,    -1,    -1,    -1,  1693,    -1,    -1,  1592,    -1,    -1,
      -1,    -1,    -1,    -1,  1817,    -1,    -1,    -1,    -1,    -1,
      -1,   139,  1825,    -1,    -1,    -1,    -1,   145,    -1,   147,
      -1,    -1,  1835,    -1,    -1,    -1,    -1,  1622,    -1,   648,
      -1,    -1,  1627,  1628,   653,  1060,    -1,   656,    -1,  1852,
    1853,    -1,  1855,  1856,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   179,    -1,    -1,    -1,    -1,   675,    -1,    -1,    -1,
      -1,     1,    -1,   191,     4,    -1,    -1,  1757,    70,    -1,
      -1,    -1,    -1,    -1,    -1,  1100,    -1,  1890,    -1,    -1,
      -1,    -1,    -1,  1108,    -1,    -1,    -1,    -1,    -1,    -1,
     709,    -1,  1905,  1906,    -1,    -1,    -1,    -1,    -1,   101,
      -1,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,    -1,  1137,    -1,   242,    -1,    -1,   245,    58,    -1,
      -1,    -1,   250,    -1,    -1,  1938,    -1,   129,    -1,  1154,
    1155,  1821,    -1,    -1,    -1,  1825,  1835,    -1,    -1,    -1,
      -1,    -1,    82,    -1,    -1,    -1,    -1,   149,   150,    -1,
    1175,    -1,    -1,  1852,    -1,   283,    -1,  1182,   160,   161,
      -1,  1186,   102,    -1,    -1,  1855,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1198,   302,    -1,    12,    13,    14,    15,
      16,    -1,    -1,    -1,  1209,    -1,    -1,    -1,  1213,    -1,
      -1,  1890,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   139,
      -1,    -1,    -1,   101,    -1,   145,   104,   105,   106,   107,
     108,   109,   110,   111,   112,  1905,  1906,    -1,    -1,    -1,
      -1,    -1,   162,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1255,  1256,    -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,
     180,    -1,    -1,    -1,  1269,  1270,    -1,   375,  1938,  1274,
    1275,   191,   192,    -1,    -1,    -1,    -1,    -1,    -1,   157,
      -1,    -1,  1792,    -1,    -1,   101,    -1,    -1,    -1,    -1,
     106,   107,   108,   109,   110,   111,   112,  1302,  1303,    -1,
     220,   409,    -1,    -1,    -1,    -1,    -1,  1312,  1313,  1314,
    1315,  1316,  1317,   129,    -1,    -1,   236,    -1,  1323,    -1,
      -1,   241,   242,    -1,    -1,   245,    -1,   435,    -1,    -1,
      -1,    -1,    -1,   149,   150,    -1,  1341,  1342,    -1,    -1,
      -1,    -1,    -1,  1853,   160,   161,    -1,   267,  1923,    -1,
     270,   101,   272,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   283,   101,    -1,  1371,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   296,    -1,     1,   129,
      -1,    -1,    -1,    -1,   101,  1390,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    -1,    -1,    -1,   149,
     150,   321,    -1,   153,   324,    -1,   514,   515,  1413,    -1,
     160,   161,   129,    -1,    -1,    -1,    -1,    -1,   338,    -1,
     157,    -1,   342,   173,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   149,   150,    -1,    58,    -1,    -1,    -1,   156,
      -1,    -1,    -1,   160,   161,    -1,    -1,    -1,   556,    -1,
      -1,    -1,   560,   101,   562,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   101,    -1,   575,   104,   105,
     106,   107,   108,   109,   110,   111,   112,    -1,    -1,   102,
    1485,  1486,   132,   133,   134,   135,   136,   137,   138,   139,
     140,   141,   142,   129,    -1,    -1,   146,    -1,   101,    -1,
    1505,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      -1,    -1,    -1,   149,   150,   435,  1521,   153,    -1,   439,
      -1,    -1,   145,   173,   160,   161,   129,   635,    -1,    -1,
      -1,    -1,    -1,    -1,  1539,  1540,    -1,    -1,    -1,   162,
     648,    -1,   650,   651,    -1,   653,   149,   150,   656,     1,
      -1,   659,   660,   661,    -1,    -1,    -1,   160,   161,    -1,
      -1,    -1,    -1,    -1,    -1,  1570,    -1,    -1,    -1,   192,
      -1,    -1,    -1,    -1,    -1,    -1,  1581,    -1,    -1,  1584,
      -1,    -1,    -1,    -1,    -1,    -1,   101,  1592,    -1,   104,
     105,   106,   107,   108,   109,   110,   111,   112,    -1,    -1,
     520,    -1,  1607,    -1,    -1,    -1,    58,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   129,    -1,    -1,  1622,    -1,    -1,
      -1,    -1,  1627,  1628,    -1,    -1,    -1,  1632,  1633,   737,
      -1,   551,    -1,    -1,   149,   150,   556,    -1,    -1,    -1,
     560,    -1,   562,    -1,   267,   160,   161,    -1,    -1,    -1,
     102,    -1,    -1,   573,    -1,   575,   101,    -1,    -1,    -1,
     283,   106,   107,   108,   109,   110,   111,   112,   113,    -1,
     590,   591,   117,    -1,   119,    -1,    -1,  1682,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   605,    -1,    -1,  1693,    -1,
      -1,    -1,    -1,   145,   614,    -1,    -1,    -1,   321,   619,
      -1,   324,    -1,    -1,    -1,   150,    -1,   627,   153,    -1,
     162,    -1,    -1,    -1,    -1,   338,    99,    -1,   101,   342,
     103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1338,
     192,    -1,  1341,  1342,    -1,    -1,   854,    -1,  1347,    -1,
      -1,    -1,  1351,    -1,  1353,    -1,    -1,    -1,    -1,   867,
     868,    -1,    -1,    -1,    -1,    -1,   149,    -1,   101,   152,
     153,    -1,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,    -1,    -1,    -1,   117,    -1,   119,  1792,    -1,   101,
      -1,    -1,    -1,   713,   106,   107,   108,   109,   110,   111,
     112,   113,    -1,    -1,    -1,   117,    -1,   119,    -1,    -1,
     730,    -1,  1817,    -1,    -1,    -1,   439,   150,   101,    -1,
     153,   104,   105,   106,   107,   108,   109,   110,   111,   112,
    1835,   283,    -1,    -1,   754,    -1,    82,    -1,   150,   759,
      -1,   153,    -1,    -1,    -1,    -1,    -1,  1852,  1853,    -1,
      -1,  1856,    -1,    -1,    -1,    -1,   776,   777,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   150,    -1,   321,
     153,    -1,   324,   101,    -1,   795,   104,   105,   106,   107,
     108,   109,   110,   111,   112,  1890,   338,    -1,    -1,    -1,
     342,    -1,  1491,    -1,    -1,    -1,    -1,   520,    -1,    -1,
      -1,   147,   101,   823,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,    -1,   162,    -1,  1923,    -1,
      -1,   149,    -1,    -1,  1523,    -1,    -1,    -1,   551,    -1,
      -1,    -1,    -1,   179,   854,    -1,  1535,   560,    -1,   562,
      -1,   861,    -1,  1542,    -1,    -1,   192,   867,    -1,    -1,
     573,    -1,   575,   152,    -1,    -1,   876,    -1,    -1,   879,
      -1,   881,    -1,    -1,    -1,    -1,   886,   590,   591,   101,
      -1,  1570,   104,   105,   106,   107,   108,   109,   110,   111,
     112,    -1,    -1,    -1,    -1,    -1,    -1,   439,    -1,    -1,
      -1,   614,  1100,    -1,    -1,    -1,    -1,    -1,    -1,   245,
    1108,    -1,    -1,    -1,   627,    -1,    -1,   101,    -1,   929,
     104,   105,   106,   107,   108,   109,   110,   111,   112,    -1,
     152,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1137,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1155,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   149,   302,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1175,   520,    -1,
     990,    -1,    -1,    -1,    -1,   321,    -1,    -1,    -1,  1678,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1007,  1008,    -1,
     713,  1690,  1691,    -1,  1693,    -1,    -1,    -1,    -1,   551,
      -1,    -1,    -1,    -1,    -1,  1213,    -1,    -1,   560,    -1,
     562,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   573,    -1,   575,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   754,    -1,    -1,    -1,    -1,    -1,    -1,   590,   591,
    1060,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   776,   777,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   614,   409,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   795,    -1,    -1,   627,    -1,    -1,    -1,    -1,
    1100,  1780,    -1,    -1,    -1,    -1,    -1,    -1,  1108,    -1,
      -1,    -1,    -1,   439,    -1,    -1,    -1,    -1,    -1,    -1,
     823,    -1,    -1,    -1,  1312,  1313,  1314,  1315,  1316,  1317,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1137,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   854,    -1,  1341,  1342,  1155,    -1,    -1,   861,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1845,    -1,  1847,    -1,
      -1,    -1,    -1,   876,    -1,    -1,   879,    -1,   881,    -1,
      -1,   713,  1182,    -1,    -1,    -1,    -1,    -1,   514,   515,
      -1,  1191,    -1,    -1,   520,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1209,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   754,    -1,    -1,  1413,   929,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1919,    -1,    -1,    -1,   776,   777,    -1,   573,    -1,    -1,
      -1,    -1,  1931,  1932,  1933,  1255,  1256,    -1,    -1,    -1,
      -1,    -1,    -1,   795,    -1,   591,    -1,    -1,    -1,  1269,
    1270,    -1,    -1,    -1,  1274,  1275,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   990,   614,    -1,
      -1,   823,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1302,  1303,  1007,  1008,    -1,    -1,    -1,   635,
      -1,    -1,  1312,  1313,  1314,  1315,    -1,  1505,    -1,    -1,
      -1,    -1,   854,    -1,    -1,    -1,    -1,    -1,    -1,   861,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1341,  1342,    -1,   876,    -1,    -1,   879,    -1,   881,
      -1,    -1,  1540,    -1,    -1,    -1,    -1,  1060,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     1,    -1,    -1,     4,    -1,    -1,    -1,    -1,
      -1,    -1,  1570,    -1,    -1,    -1,    -1,   713,    -1,    -1,
    1390,    -1,    -1,    -1,    -1,    -1,  1584,   929,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1108,    -1,    -1,    -1,    -1,
      -1,   737,    -1,  1413,    -1,    -1,    -1,    -1,    -1,  1607,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   754,    58,
      -1,    -1,    -1,    -1,  1137,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     776,   777,  1155,    82,    -1,    -1,    -1,    -1,   990,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   795,
      -1,    -1,    -1,   102,    -1,  1007,  1008,    -1,    -1,  1182,
      -1,    -1,    -1,    -1,    -1,  1485,  1486,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1693,  1209,    -1,    -1,    -1,
     139,  1511,    -1,    -1,    -1,    -1,   145,    -1,   147,    -1,
      -1,  1521,    -1,    -1,    -1,    -1,    -1,    -1,  1060,    -1,
      -1,    -1,    -1,    -1,    -1,   861,    -1,    -1,    -1,    -1,
    1540,    -1,   868,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     179,    -1,  1255,  1256,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   191,    -1,    -1,    -1,  1269,  1270,    -1,    -1,
    1570,  1274,  1275,    -1,    -1,    -1,  1108,    -1,    -1,    -1,
      -1,  1581,    -1,    -1,  1584,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1592,    -1,    -1,    -1,    -1,    -1,    -1,  1302,
    1303,    -1,    -1,   929,    -1,  1137,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   242,    -1,    -1,   245,    -1,    -1,    -1,
      -1,   250,  1622,  1155,    -1,    -1,    -1,  1627,  1628,    -1,
      -1,    -1,  1632,  1633,    -1,    -1,    -1,    -1,  1341,  1342,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1835,  1648,    -1,
    1182,    -1,    -1,    -1,   283,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1852,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   302,    -1,    -1,    -1,  1209,     0,    -1,
      -1,     3,    -1,    -1,    -1,    -1,    -1,  1390,    -1,    -1,
      -1,    -1,    -1,  1693,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1890,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1255,  1256,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1060,    -1,    -1,  1269,  1270,    -1,
      -1,    -1,  1274,  1275,    -1,    -1,   375,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    76,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1302,  1303,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     409,    -1,  1485,  1486,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1792,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   435,    -1,    -1,  1341,
    1342,    -1,    -1,   135,    -1,    -1,    -1,  1817,  1521,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1835,    -1,  1540,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1175,
      -1,    -1,  1852,  1853,    -1,    -1,  1182,    -1,  1390,    -1,
      -1,    -1,    -1,    -1,    -1,    86,    -1,  1570,    -1,    -1,
      -1,    92,    93,    -1,    -1,    -1,    -1,    -1,  1581,    -1,
      -1,    -1,    -1,  1209,    -1,   514,   515,    -1,    -1,  1592,
    1890,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   126,  1906,   229,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1622,
      -1,    -1,   244,  1923,  1627,  1628,    -1,   556,    -1,  1632,
    1633,   560,   254,   562,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   264,    -1,    -1,    -1,   575,    -1,  1274,  1275,
      -1,    -1,    -1,  1485,  1486,    -1,   278,   279,    -1,    -1,
      -1,    -1,    -1,   285,   286,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   301,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1521,
    1693,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   320,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   635,    -1,  1540,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   648,
      -1,   650,   651,    -1,   653,    -1,    -1,   656,    -1,    -1,
     659,   660,   661,    -1,    -1,    -1,    -1,    -1,  1570,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1581,
      -1,    -1,    -1,    -1,   376,    -1,    -1,    -1,    -1,    -1,
    1592,    -1,    -1,    -1,  1390,    -1,    -1,    -1,    -1,    -1,
      -1,   292,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   407,    -1,    -1,    -1,  1792,
    1622,    -1,    -1,    -1,    -1,  1627,  1628,    -1,    -1,    -1,
    1632,  1633,    -1,    -1,    -1,    -1,    -1,    -1,   737,    -1,
      -1,   433,    -1,    -1,  1817,   437,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    47,
      -1,    -1,  1835,    -1,   456,    -1,    -1,    -1,   460,   461,
      -1,    -1,   464,    -1,    -1,    -1,    -1,    -1,    -1,  1852,
    1853,    -1,    -1,    -1,    -1,    73,    -1,   479,   480,   481,
     482,  1693,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   498,    -1,    -1,  1505,
      -1,    -1,    -1,    -1,   506,    -1,    -1,  1890,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   119,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   534,    -1,   132,    -1,   134,   438,    -1,   440,
    1923,    -1,    -1,    -1,    -1,   854,    -1,    -1,   449,   450,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   867,   868,
      -1,    -1,    -1,   565,    -1,    -1,   164,    -1,    -1,    -1,
     572,    -1,    -1,    -1,    -1,  1581,   578,    -1,    -1,    -1,
    1792,    -1,    -1,   181,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   601,
     602,    -1,    -1,    -1,    -1,  1817,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   219,    -1,  1835,    -1,   223,  1632,  1633,   226,   227,
      -1,    -1,   230,    -1,    -1,   233,   234,    -1,    -1,    -1,
    1852,  1853,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     1,    -1,   557,    -1,    -1,    -1,
     662,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1890,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   295,    -1,    -1,
     298,    48,    -1,    -1,    -1,    52,    -1,    54,    -1,    -1,
      -1,  1923,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   319,    -1,    -1,    71,    -1,    -1,   729,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   334,    -1,    -1,    -1,
      -1,    -1,   744,    -1,    -1,    -1,   748,    -1,    -1,    -1,
      -1,    98,    99,    -1,   101,   757,   103,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,    -1,
     117,   118,   119,    -1,   121,   122,    -1,   779,    -1,    -1,
      -1,    -1,   129,    -1,    -1,    -1,   788,    -1,    -1,    -1,
      -1,  1100,   794,    -1,    -1,    -1,    -1,    -1,    -1,  1108,
      -1,    -1,   149,    -1,    -1,   152,   153,    -1,    -1,    -1,
      -1,  1817,   159,   160,   161,   162,   163,   164,   165,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1137,   831,
     428,    -1,    -1,    -1,    -1,    -1,   838,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1155,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   865,    -1,    -1,  1175,    -1,    48,   770,
      -1,    -1,    52,    -1,    54,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   483,    -1,    -1,    -1,    -1,
      -1,    71,    -1,    -1,    -1,    -1,    -1,   495,    -1,    -1,
      -1,    -1,    -1,    -1,  1213,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   919,    98,    99,
      -1,   101,    -1,   103,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,    -1,   117,   118,   119,
      -1,   121,   122,    -1,    -1,    -1,   847,   848,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   858,   859,   860,
      -1,    -1,   863,    -1,    -1,    -1,    -1,    -1,    -1,   149,
      -1,   569,   152,   153,    -1,    -1,    -1,    -1,    -1,   159,
     160,   161,   162,   163,   164,   165,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   173,   174,    -1,    -1,    -1,  1000,    -1,
      -1,    -1,  1004,  1312,  1313,  1314,  1315,  1316,  1317,  1011,
      -1,   609,   610,    -1,    -1,    -1,    -1,    -1,    -1,  1021,
      -1,    -1,    -1,    -1,   622,    -1,  1028,    -1,    -1,    -1,
      -1,    -1,  1341,  1342,    -1,  1037,    -1,  1039,    -1,    -1,
      -1,    -1,   943,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1071,
      -1,    -1,    -1,  1075,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   987,  1089,    -1,    -1,
    1092,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    12,
      13,    14,    15,    16,  1413,    -1,    19,    -1,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,  1034,    -1,   733,   734,    50,    51,    -1,
      -1,   739,  1043,  1044,  1045,  1046,    -1,    -1,    -1,    -1,
    1051,  1052,    -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,
    1061,    -1,   760,    -1,    -1,   763,   764,    -1,   766,    -1,
     768,   769,    -1,    -1,    -1,    -1,    -1,    -1,  1180,    -1,
      -1,  1082,    -1,  1084,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1505,    -1,    -1,    -1,
      -1,    -1,  1204,    -1,    -1,    -1,    -1,    -1,    -1,   807,
      -1,    -1,    -1,   811,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1540,    -1,    -1,    -1,    -1,  1137,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1570,  1163,    -1,    -1,    -1,    -1,    -1,    -1,  1170,
      -1,  1172,  1173,    -1,    -1,  1584,    -1,    -1,    -1,    -1,
    1181,    -1,  1183,    -1,  1185,    -1,  1187,    -1,    -1,   887,
      -1,  1192,  1294,    -1,    -1,    -1,  1298,    -1,  1607,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    98,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   109,  1328,   111,    -1,   113,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   178,    -1,    -1,  1250,
      -1,    -1,    -1,    -1,    -1,    -1,  1257,  1258,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,    -1,   153,
     154,    -1,    -1,    -1,    -1,    -1,  1378,    -1,    -1,  1381,
    1281,    -1,    -1,    -1,  1693,    -1,    -1,  1288,    -1,    -1,
    1291,    -1,    -1,    -1,  1396,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   195,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1017,
    1321,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1446,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1455,    -1,    -1,    -1,  1459,    -1,    -1,
    1361,    -1,    -1,    -1,    -1,    -1,    -1,  1065,    -1,    -1,
    1068,  1473,  1474,    -1,   258,    -1,   260,   261,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1394,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1402,    -1,  1404,   288,    -1,    -1,    -1,    -1,    -1,
     294,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1835,    -1,   360,    -1,
      -1,   363,   364,    -1,    -1,    -1,    -1,    -1,    -1,   323,
      -1,   373,   374,  1852,    -1,   329,    -1,   331,    -1,  1450,
    1451,    -1,    -1,    -1,    -1,    -1,   388,   389,    -1,    -1,
      -1,    -1,    -1,    -1,  1465,  1466,    -1,  1468,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1477,   409,    -1,    -1,
      -1,  1890,    -1,  1585,  1586,    -1,  1487,  1488,    -1,    -1,
    1188,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1196,  1197,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   439,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   419,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1251,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   445,  1260,   447,   448,  1263,    -1,  1265,  1266,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1593,  1594,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1703,  1603,    -1,    -1,    -1,    -1,  1305,   492,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1726,    -1,    -1,    -1,   512,    -1,
      -1,    -1,    -1,   517,    -1,   519,    -1,  1638,  1639,    -1,
      -1,    -1,  1744,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   539,    -1,   541,   542,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1771,
      -1,    -1,    -1,    -1,  1372,   559,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1788,   571,    -1,  1791,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   593,
      -1,   595,   596,    -1,    -1,    -1,    -1,  1718,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   617,   618,  1736,    -1,    -1,  1739,  1740,
     624,    -1,    -1,    -1,    -1,  1746,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1454,    -1,   690,   691,
     692,   693,   694,   695,   696,   697,   698,   699,   700,   701,
     702,   703,   704,   705,   706,   707,   708,  1879,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1483,   162,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1509,    -1,    -1,    -1,   191,   192,  1515,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   770,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   223,    -1,    -1,
      -1,    -1,    -1,  1854,   230,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    55,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1580,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1903,    -1,    -1,    -1,    -1,    -1,    90,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   298,    -1,    -1,    -1,  1927,    -1,    -1,    -1,
      -1,    -1,    -1,  1934,    -1,    -1,    -1,    -1,    82,    -1,
      -1,    -1,    -1,    -1,    -1,   321,   322,    -1,  1949,    -1,
      -1,    -1,    -1,    -1,    98,    -1,  1654,  1655,   140,    -1,
      -1,   143,   846,    -1,  1662,    -1,   342,    -1,  1666,    -1,
      -1,    -1,    -1,   857,   156,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   925,   878,    -1,    -1,    -1,   930,    -1,
      -1,    -1,    -1,   147,    -1,   889,    -1,   151,    -1,   941,
      -1,    -1,    -1,    -1,   898,    -1,    -1,    -1,   162,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   210,    -1,
      -1,    -1,    -1,    -1,    -1,   179,   412,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   192,   981,
      -1,   195,   428,   429,    -1,   431,   432,    -1,    -1,    -1,
      -1,  1759,    -1,   439,    -1,    -1,    -1,   443,    -1,    -1,
      -1,    -1,    -1,   255,    -1,    47,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   265,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   276,    -1,    -1,    -1,    -1,    -1,
      -1,   245,    -1,    -1,    -1,    -1,    -1,    -1,   484,    -1,
     994,    -1,   488,    -1,   258,    -1,    -1,    -1,    -1,    -1,
      -1,   303,    -1,  1821,    -1,    -1,    -1,    -1,   310,   311,
      -1,    -1,    -1,   315,  1018,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   520,    -1,    -1,   119,    -1,    -1,
     294,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   302,    -1,
     132,    -1,   134,    -1,    -1,   347,    -1,    -1,    -1,    -1,
     352,    -1,    -1,   355,    -1,    -1,  1108,   321,    -1,   323,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   570,    -1,    -1,   573,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1905,    -1,    -1,
      -1,    -1,    -1,    -1,   590,   591,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   601,    -1,    -1,    -1,   605,
      -1,   375,    -1,    -1,    -1,    -1,   612,    -1,   614,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1182,    -1,    -1,    -1,   226,   227,    -1,    -1,   230,    -1,
     442,   233,   234,    -1,    -1,   409,    -1,    -1,    -1,    -1,
    1154,    -1,   454,   455,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1216,  1217,  1218,    -1,    -1,    -1,
      -1,  1223,  1224,    -1,    -1,   439,    -1,    -1,    -1,    -1,
      -1,   445,  1186,    -1,    -1,    -1,    -1,    -1,  1192,    -1,
      -1,    -1,    -1,  1245,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   713,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   319,    -1,    -1,
    1282,  1283,    -1,   729,   730,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   334,   739,   740,    -1,   742,   743,    -1,    -1,
     514,   515,    -1,    -1,    -1,    -1,   520,    -1,   754,    -1,
      -1,   757,    -1,   759,   760,    -1,    -1,    -1,    -1,    -1,
     766,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     776,   777,    -1,   585,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   795,
      -1,    -1,    -1,   799,    -1,    -1,    -1,   803,    -1,   573,
      -1,   807,   808,    -1,    -1,   811,   812,    -1,    -1,  1323,
      -1,    -1,    -1,   819,    -1,    -1,    -1,   591,    -1,   593,
      -1,   633,    -1,    -1,    -1,    -1,   428,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,
     614,    -1,    -1,    52,    -1,    54,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   861,   862,  1371,    -1,    -1,
      -1,   635,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   648,    -1,   650,   651,    -1,   653,
      -1,    -1,   656,   889,    -1,   659,   660,   661,    -1,    98,
      99,    -1,   101,   495,   103,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,    -1,   117,   118,
     119,    -1,   121,   122,    -1,    -1,    -1,    -1,    -1,    -1,
     129,    -1,    -1,   929,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   713,
     149,   150,    -1,   152,   153,    -1,    -1,    -1,   157,    -1,
     159,   160,   161,   162,   163,   164,   165,    -1,    -1,   771,
      -1,    -1,    -1,   737,    -1,    -1,   778,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     754,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   994,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   776,   777,    -1,  1011,  1012,   609,   610,    -1,
      -1,    -1,  1018,  1575,    -1,    -1,    -1,    -1,    -1,    -1,
     622,   795,    -1,    -1,    -1,  1539,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   853,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1060,    -1,    -1,    -1,    -1,  1065,
    1066,    -1,  1068,  1069,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   861,    -1,    -1,
      -1,    -1,   904,    -1,   868,    -1,    -1,   909,    -1,    -1,
      -1,    -1,    -1,    -1,   878,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   733,   734,    -1,    -1,    -1,    -1,   739,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1665,    -1,    -1,    -1,   929,    -1,    -1,   760,    -1,
      -1,   763,   764,    -1,   766,    -1,   768,   769,  1682,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1182,    -1,    -1,    -1,
      -1,    -1,  1188,  1189,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1209,    -1,   807,    -1,  1769,    -1,   811,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   179,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   192,    -1,    -1,  1799,    -1,    -1,
      -1,    -1,    -1,  1757,    -1,  1251,  1252,   205,    -1,   207,
      -1,    -1,    -1,    -1,  1260,  1261,    -1,  1263,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1274,  1275,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   887,  1060,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1113,  1114,  1115,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1884,    -1,  1886,    -1,  1100,    -1,    -1,    -1,
      -1,  1143,    -1,    -1,    -1,   293,    -1,    -1,    -1,    -1,
      -1,    -1,  1856,    -1,    -1,    -1,  1158,    -1,    -1,    -1,
    1912,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1936,    -1,    -1,    -1,    -1,    -1,
    1154,    -1,    -1,  1195,  1390,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1175,    -1,    -1,    -1,    -1,    -1,    -1,  1182,    -1,
      -1,    -1,    -1,    -1,    -1,  1017,    -1,    -1,    -1,  1981,
      -1,    -1,    -1,    -1,  1938,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1209,    -1,    -1,    -1,  1213,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1065,    -1,    -1,  1068,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1499,    -1,    -1,    -1,    -1,    -1,    -1,
    1274,  1275,    -1,    -1,    -1,    -1,    -1,   465,    -1,  1515,
      -1,    -1,    -1,   471,  1326,    -1,    -1,  1329,   476,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1312,  1313,
    1314,    -1,  1316,  1317,    -1,    -1,    -1,    -1,    -1,  1323,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1581,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1188,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1196,  1197,    -1,  1371,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   563,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1390,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1632,  1633,    -1,    -1,
      -1,    -1,    -1,   591,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1647,  1648,    -1,    -1,    -1,   604,    -1,    -1,  1251,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1663,  1260,    -1,
      -1,  1263,    -1,  1265,  1266,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   655,    -1,    -1,
      -1,    -1,    -1,  1305,    -1,    -1,  1518,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   673,   674,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   683,    -1,   685,   686,    -1,
      -1,  1505,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1757,    -1,    -1,    -1,   713,    -1,    -1,    -1,  1765,
      -1,    -1,    -1,    -1,    -1,  1539,    -1,    -1,   726,    -1,
    1372,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   737,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1600,    -1,
      -1,    -1,    -1,   751,    -1,    -1,   754,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1581,    -1,    -1,
      -1,  1817,    -1,    -1,    -1,  1821,  1822,    -1,    -1,  1825,
      -1,    -1,    -1,   781,    -1,    -1,   784,    -1,    -1,    -1,
      -1,    -1,    -1,  1607,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1855,
      -1,    -1,  1454,    -1,    -1,    -1,    -1,    -1,  1632,  1633,
      -1,    -1,   820,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1483,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1905,
    1906,    -1,    -1,  1715,    -1,    -1,    -1,    -1,  1682,    -1,
     868,    -1,  1724,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     878,   879,    -1,    -1,    -1,    -1,    -1,    -1,   886,    -1,
      -1,    -1,  1938,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   907,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   916,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   929,    -1,    -1,    -1,    -1,    -1,    -1,  1580,   937,
      -1,    -1,    -1,    -1,    -1,    -1,   944,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1792,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     988,    -1,    -1,    -1,     1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1817,    -1,    -1,    -1,    -1,    -1,    -1,
      17,    -1,  1654,  1655,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1666,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1853,
      -1,    48,  1856,    -1,    -1,    52,    -1,    54,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1055,    -1,  1057,
      -1,  1059,    69,    -1,    71,    72,    -1,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    91,    92,    93,    94,    -1,    96,
      -1,    98,    99,    -1,   101,    -1,   103,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,    -1,   121,   122,    -1,  1759,    -1,    -1,
      -1,    -1,   129,    -1,    -1,    -1,    -1,  1125,  1126,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   148,   149,    -1,    -1,   152,   153,    -1,    -1,    -1,
     157,    -1,   159,   160,   161,   162,   163,   164,   165,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   174,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1821,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1186,    -1,
      -1,    -1,    -1,    -1,  1192,    -1,    -1,    -1,    -1,    -1,
      -1,     1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1209,    -1,    -1,    -1,    -1,    -1,    17,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1226,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1239,    -1,    -1,  1242,    -1,    -1,    -1,    48,    -1,
      -1,    -1,    52,    -1,    54,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    69,
      -1,    71,    72,    -1,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    92,    93,    94,  1293,    96,    -1,    98,    99,
      -1,   101,    -1,   103,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
      -1,   121,   122,    -1,    -1,    -1,    -1,    -1,     1,   129,
      -1,    -1,    -1,  1331,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    17,    -1,    -1,    -1,   148,   149,
      -1,  1349,   152,   153,  1352,    -1,    -1,   157,    -1,   159,
     160,   161,   162,   163,   164,   165,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   174,    48,    -1,    -1,    -1,    52,
      -1,    54,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1390,    -1,    -1,    -1,    69,    -1,    71,    72,
      -1,    74,  1400,  1401,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,    87,    88,    89,    90,    91,    92,
      93,    94,    -1,    96,    -1,    98,    99,  1425,   101,  1427,
     103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,    -1,   121,   122,
      -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    48,    -1,
      -1,    -1,    52,    -1,    54,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   149,    -1,    -1,   152,
     153,    71,    -1,    -1,   157,    -1,   159,   160,   161,   162,
     163,   164,   165,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   174,    -1,    -1,    -1,    -1,    -1,  1505,    98,    99,
      -1,   101,  1510,   103,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,    -1,   117,   118,   119,
      -1,   121,   122,    -1,    -1,    -1,    -1,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,
     150,    -1,   152,   153,    -1,    -1,    -1,   157,  1566,   159,
     160,   161,   162,   163,   164,   165,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1605,    -1,    -1,
    1608,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    -1,    19,    -1,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    -1,    -1,    48,    -1,    50,    51,
      52,    -1,    54,    55,    56,    57,    58,    59,    60,    61,
      62,    63,    -1,    -1,    -1,    67,    -1,    69,    70,    71,
      72,    -1,    74,    -1,    -1,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    91,
      92,    93,    94,    -1,    96,    -1,    98,    99,   100,   101,
      -1,   103,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,    -1,   121,
     122,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   148,   149,    -1,    -1,
     152,   153,    -1,    -1,    -1,   157,    -1,   159,   160,   161,
     162,   163,   164,   165,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   174,    -1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      -1,    19,  1800,    21,    22,    23,    24,    25,    26,    27,
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
      -1,   149,    -1,    -1,   152,   153,    -1,    -1,    -1,   157,
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
      -1,    -1,    -1,   149,    -1,   151,   152,   153,    -1,    -1,
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
     164,   165,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   173,
     174,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    -1,    19,    -1,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    -1,    -1,    48,    -1,    50,    51,
      52,    -1,    54,    55,    56,    57,    58,    59,    60,    61,
      62,    63,    -1,    -1,    -1,    67,    -1,    -1,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    98,    99,   100,   101,
      -1,   103,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,    -1,   117,   118,   119,    -1,   121,
     122,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,    -1,   151,
     152,   153,    -1,    -1,    -1,    -1,    -1,   159,   160,   161,
     162,   163,   164,   165,     3,     4,     5,     6,     7,     8,
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
     163,   164,   165,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    -1,    19,    -1,
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
     161,   162,   163,   164,   165,     1,    -1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    -1,    19,    -1,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    -1,    49,    50,    51,    -1,    53,    -1,    55,
      56,    57,    58,    59,    60,    61,    62,    63,    -1,    -1,
      -1,    67,    -1,    -1,    70,    -1,    -1,    -1,    -1,    75,
      76,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   100,    -1,    -1,    -1,   104,   105,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   148,    -1,    -1,    -1,   152,   153,    -1,    -1,
      -1,    -1,    -1,    -1,   160,   161,     1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    -1,    19,    -1,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    -1,    49,    50,    51,    -1,    53,    -1,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    -1,
      -1,    -1,    67,    -1,    -1,    70,     5,    -1,    -1,    -1,
      75,    76,    -1,    12,    13,    14,    15,    16,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   100,    -1,    -1,    -1,   104,
     105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,
      -1,    -1,    -1,    52,    -1,    54,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     5,   148,    -1,    -1,    -1,   152,   153,    12,
      13,    14,    15,    16,    -1,   160,   161,    -1,    -1,    98,
      99,    -1,   101,    -1,   103,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,    -1,   117,   118,
     119,    -1,   121,   122,    -1,    48,    -1,    -1,    -1,    52,
     129,    54,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    -1,
     149,    -1,    -1,   152,   153,    -1,    -1,    -1,    -1,    -1,
     159,   160,   161,   162,   163,   164,   165,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    98,    99,    -1,   101,    -1,
     103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,    -1,   117,   118,   119,    -1,   121,   122,
      -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   149,    -1,    -1,   152,
     153,    -1,    -1,    -1,    -1,    -1,   159,   160,   161,   162,
     163,   164,   165,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    -1,    19,
      -1,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    -1,    -1,    -1,    -1,
      50,    51,    -1,    -1,    -1,    55,    56,    57,    58,    59,
      60,    61,    62,    63,    -1,    -1,    -1,    67,    -1,    -1,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    96,    -1,    -1,    -1,
     100,   101,    -1,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,    -1,    -1,    -1,   116,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,
     150,    -1,   152,   153,    -1,    -1,    -1,    -1,    -1,    -1,
     160,   161,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    -1,    -1,    19,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    -1,    49,    50,
      51,    -1,    53,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     101,    -1,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,   150,
      -1,   152,   153,    -1,    -1,    -1,   157,    -1,    -1,   160,
     161,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    -1,    19,    -1,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    -1,    -1,    -1,    -1,    50,    51,
      -1,    -1,    -1,    55,    56,    57,    58,    59,    60,    61,
      62,    63,    -1,    -1,    -1,    67,    -1,    -1,    70,    -1,
      -1,    -1,    -1,    75,    76,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,    -1,
      -1,    -1,   104,   105,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   148,    -1,    -1,    -1,
     152,   153,    -1,    -1,    -1,    -1,    -1,    -1,   160,   161,
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
      14,    15,    16,    -1,    -1,    19,    -1,    21,    22,    23,
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
      -1,    -1,     3,    -1,    -1,    -1,   160,   161,     9,    -1,
      -1,    12,    13,    14,    15,    16,    17,    -1,    19,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    -1,    -1,    -1,    -1,    50,
      51,    -1,    -1,    -1,    55,    56,    57,    58,    59,    60,
      61,    62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,
      -1,    -1,    -1,   104,   105,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   148,    -1,    -1,
      -1,   152,   153,    -1,    -1,     3,    -1,    -1,    -1,   160,
     161,     9,    -1,    -1,    12,    13,    14,    15,    16,    17,
      -1,    19,    -1,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    -1,    -1,
      -1,    -1,    50,    51,    -1,    -1,    -1,    55,    56,    57,
      58,    59,    60,    61,    62,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   100,    -1,    -1,    -1,   104,   105,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     148,    -1,    -1,    -1,   152,   153,    -1,    -1,     3,    -1,
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
     152,   153,    -1,    -1,    -1,    -1,    -1,    -1,   160,   161,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    -1,    -1,    19,    -1,    21,    22,
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
      -1,    -1,    -1,     3,    -1,    -1,    -1,    -1,    -1,   152,
     153,    -1,    12,    13,    14,    15,    16,   160,   161,    19,
      -1,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    -1,    49,
      50,    51,    -1,    53,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    12,    13,    14,    15,    16,    -1,    -1,    19,
      70,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    -1,    49,
      50,    51,    -1,    53,   104,   105,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   152,   153,    -1,    -1,    -1,    -1,    -1,    -1,
     160,   161,     4,     5,     6,     7,     8,     9,    10,    11,
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
     152,   153,    -1,    -1,    -1,   157,    -1,    -1,   160,   161,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    -1,    -1,    19,    -1,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    -1,    49,    50,    51,    -1,    53,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    17,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   149,   150,    48,   152,   153,
      -1,    52,    -1,    54,    -1,    -1,   160,   161,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    69,    -1,
      71,    -1,    -1,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    90,
      91,    92,    93,    94,    -1,    96,    -1,    98,    99,    -1,
     101,    -1,   103,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,    -1,
     121,   122,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    17,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,    -1,
      -1,   152,   153,    -1,    -1,    -1,   157,    -1,   159,   160,
     161,   162,   163,   164,   165,    48,    -1,    -1,    -1,    52,
      -1,    54,    -1,   174,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    69,    -1,    71,    -1,
      -1,    74,    -1,    -1,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,    87,    88,    89,    90,    91,    92,
      93,    94,    -1,    96,    -1,    98,    99,    -1,   101,    -1,
     103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,    -1,   121,   122,
      -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   149,    -1,    -1,   152,
     153,    -1,    -1,    -1,   157,    -1,   159,   160,   161,   162,
     163,   164,   165,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   174,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    -1,    19,    -1,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    -1,    -1,    -1,    -1,    50,    51,
      -1,    -1,    -1,    55,    56,    57,    58,    59,    60,    61,
      62,    -1,    12,    13,    14,    15,    16,    17,    70,    19,
      -1,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    -1,    -1,    -1,    -1,
      50,    51,   104,   105,    -1,    55,    56,    57,    58,    59,
      60,    61,    62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      70,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,   131,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,
     152,   153,    -1,    -1,   104,   105,    -1,    -1,   160,   161,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    -1,    19,    -1,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,   152,    -1,    -1,    -1,    50,    51,    -1,    -1,
      -1,    55,    56,    57,    58,    59,    60,    61,    62,    -1,
      12,    13,    14,    15,    16,    17,    70,    19,    -1,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    -1,    -1,    -1,    -1,    50,    51,
     104,   105,    -1,    55,    56,    57,    58,    59,    60,    61,
      62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,
      -1,    -1,    -1,    -1,    -1,   129,    -1,   131,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,   153,
      -1,    -1,   104,   105,    -1,    -1,   160,   161,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    -1,    19,    -1,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
     152,    -1,    -1,    -1,    50,    51,    -1,    -1,    -1,    55,
      56,    57,    58,    59,    60,    61,    62,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,   105,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   129,    -1,   131,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   152,   153,    -1,    -1,
      -1,    -1,    -1,    -1,   160,   161,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      -1,    19,    -1,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    -1,    -1,
      -1,    -1,    50,    51,    -1,    -1,    -1,    55,    56,    57,
      58,    59,    60,    61,    62,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   104,   105,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      12,    13,    14,    15,    16,    -1,    -1,    19,    -1,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    -1,    49,    50,    51,
      -1,    53,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   104,   105,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     152,   153,    -1,    -1,    -1,    -1,    -1,    -1,   160,   161,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    -1,    -1,    48,    -1,    50,    51,
      52,    -1,    54,    55,    56,    57,    58,    59,    60,    61,
      62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    83,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    98,    99,    -1,   101,
      -1,   103,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,    -1,   117,   118,   119,    -1,   121,
     122,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,    -1,    -1,
     152,   153,    -1,    -1,    -1,   157,    -1,   159,   160,   161,
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
      -1,   149,    -1,   151,   152,   153,    -1,    -1,    -1,    -1,
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
      -1,    -1,   152,   153,    -1,    -1,    -1,    -1,    -1,   159,
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
      -1,    -1,    -1,   159,   160,   161,   162,   163,   164,   165,
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
      -1,    -1,    50,    51,    -1,    -1,    -1,    55,    56,    57,
      58,    59,    60,    61,    62,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   101,    -1,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   149,   150,    -1,   152,   153,    -1,    -1,    -1,   157,
      -1,    -1,   160,   161,    12,    13,    14,    15,    16,    17,
      -1,    19,    -1,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    -1,    -1,
      -1,    -1,    50,    51,    -1,    -1,    -1,    55,    56,    57,
      58,    59,    60,    61,    62,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   101,    -1,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   149,   150,    -1,   152,   153,    -1,    -1,    -1,    -1,
      -1,    -1,   160,   161,    12,    13,    14,    15,    16,    17,
      -1,    19,    -1,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    -1,    -1,
      -1,    -1,    50,    51,    -1,    -1,    -1,    55,    56,    57,
      58,    59,    60,    61,    62,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   101,    -1,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   152,   153,    -1,    12,    13,    14,
      15,    16,   160,   161,    19,    -1,    21,    22,    23,    24,
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
      -1,    -1,    -1,    -1,    -1,   160,   161,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
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
      12,    13,    14,    15,    16,    17,    70,    19,    -1,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    -1,    -1,   152,    -1,    50,    51,
     104,   105,    -1,    55,    56,    57,    58,    59,    60,    61,
      62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,
      -1,    -1,    -1,    -1,    76,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,   152,   153,
      -1,    -1,   104,   105,    -1,    -1,   160,   161,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     152,   153,    -1,    -1,    -1,    -1,    -1,    -1,   160,   161,
      12,    13,    14,    15,    16,    17,    -1,    19,    -1,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    -1,    -1,    -1,    -1,    50,    51,
      -1,    -1,    -1,    55,    56,    57,    58,    59,    60,    61,
      62,    -1,    12,    13,    14,    15,    16,    17,    70,    19,
      -1,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    -1,    -1,    -1,    -1,
      50,    51,   104,   105,    -1,    55,    56,    57,    58,    59,
      60,    61,    62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      70,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     152,   153,    -1,    -1,   104,   105,    -1,    -1,   160,   161,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   152,   153,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,   152,   153,    -1,    -1,
      -1,    -1,    -1,    -1,   160,   161,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      -1,    -1,    19,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    -1,    49,    50,    51,    -1,    53,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    12,    13,    14,
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
      -1,   152,    -1,    -1,    12,    13,    14,    15,    16,   160,
     161,    19,    -1,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      -1,    49,    50,    51,    -1,    53,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    12,    13,    14,    15,    16,    -1,
      -1,    19,    70,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      -1,    49,    50,    51,    -1,    53,   104,   105,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   152,   153,   104,   105,    -1,    -1,
      -1,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   152,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   151,   152,     4,     5,     6,     7,     8,     9,    10,
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
     131,    -1,    -1,    -1,    -1,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    -1,
      19,   152,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    -1,    -1,    -1,
      -1,    50,    51,    -1,    -1,    -1,    55,    56,    57,    58,
      59,    60,    61,    62,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,   104,   105,    19,    -1,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    -1,    49,    50,    51,
      -1,    53,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   152,    -1,    -1,    -1,    -1,    70,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   104,   105,    -1,    -1,    -1,    -1,    12,    13,
      14,    15,    16,    17,    -1,    19,    -1,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    -1,    -1,    -1,    -1,    50,    51,    -1,    -1,
     152,    55,    56,    57,    58,    59,    60,    61,    62,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,    -1,
      -1,    -1,    12,    13,    14,    15,    16,    -1,    -1,    19,
      -1,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,   152,    49,
      50,    51,    -1,    53,    -1,    -1,    48,    -1,    -1,    -1,
      52,    -1,    54,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   104,   105,    98,    99,    -1,   101,
      -1,   103,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,    -1,   117,   118,   119,    48,   121,
     122,    -1,    52,    -1,    54,    -1,    -1,   129,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    71,   152,    -1,    -1,    -1,    -1,   149,   150,   151,
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
     149,   149,   198,   149,   195,   149,   149,   195,   195,    18,
      20,    83,   153,   162,   163,   199,   200,   214,   221,   225,
     330,   358,   460,   155,   176,   149,   184,   158,   158,   118,
     120,   121,   122,   149,   152,   153,   157,   158,   198,   198,
     166,   160,   167,   168,   162,   163,   123,   124,   125,   126,
     169,   170,   127,   128,   161,   159,   171,   129,   130,   172,
     151,   155,   152,   176,   132,   133,   134,   135,   136,   137,
     138,   139,   140,   141,   142,   173,   216,   217,   218,   149,
     196,   435,   436,   437,   438,   439,   151,   155,   151,   151,
     151,   151,   151,   151,   149,   402,   439,   440,   149,   439,
     440,   176,   292,   458,   176,   177,   177,   149,   161,   196,
     408,   426,   427,   428,   429,   430,   431,   432,   433,   434,
     131,   460,   177,   177,   357,   357,   176,   176,   176,   153,
     181,   176,   362,   156,   155,   462,   361,   152,   153,   156,
     365,   150,   214,   220,   149,   176,   176,   176,   176,   408,
     410,   411,   412,   421,   423,   424,   425,   151,   151,   151,
     151,   151,   151,   151,   409,   422,   402,   149,   360,   154,
     176,   225,   397,   176,   225,   399,   221,   359,   221,   359,
     399,   388,   225,   397,   401,   157,   397,   272,   388,   225,
     397,   324,   325,   323,   157,   131,   295,   350,   351,   354,
     355,   151,   155,    68,   274,   275,   177,   295,   288,   160,
     214,   176,   408,   349,   390,   388,   154,   176,   149,   370,
     368,   369,    76,   305,   180,   293,   440,   453,   295,   299,
     460,   176,   442,   443,   444,   154,   176,    17,   214,   295,
     441,   463,   402,   402,   440,   293,   451,   461,   295,   180,
     402,   293,   453,   317,   155,   462,   173,   217,   346,   157,
     345,   151,   359,   151,   151,   155,   149,   174,   358,   153,
     358,   358,   358,   214,   358,   151,   358,   358,   358,   176,
     151,   162,   163,   200,    17,   297,   151,   155,   151,   160,
     161,   151,   220,   214,   157,   180,   180,   113,   153,   180,
     150,   188,   189,   190,   214,   113,   153,   180,   330,   214,
     188,   180,   198,   201,   201,   201,   202,   202,   203,   203,
     204,   204,   204,   204,   205,   205,   206,   207,   208,   209,
     210,   156,   221,   174,   182,   153,   180,   214,   157,   214,
     176,   436,   437,   438,   295,   435,   402,   402,   214,   359,
     149,   402,   439,   440,   149,   439,   440,   176,   176,   154,
     154,   149,   408,   427,   428,   429,   432,    17,   295,   426,
     430,   149,   402,   445,   463,   402,   402,   463,   149,   402,
     445,   402,   402,   177,   213,   357,   154,   155,   154,   155,
     463,   463,   131,   347,   348,   349,   347,   357,   176,   212,
     213,   214,   400,   462,   361,   363,   148,   176,   151,   155,
     176,   347,   180,   399,   180,   151,   151,   151,   151,   151,
     151,   149,   402,   439,   440,   149,   402,   439,   440,   399,
     182,   440,   214,   225,   350,   151,   151,   151,   151,   386,
     387,   225,   388,   225,   397,   387,   225,   157,   157,   157,
     331,   177,   177,   180,   276,   357,    17,    69,    71,    74,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    90,    91,    92,    93,    94,    96,   104,   105,
     116,   176,   221,   222,   223,   224,   225,   226,   227,   229,
     230,   240,   244,   245,   246,   247,   248,   249,   254,   255,
     261,   262,   263,   277,   295,   299,   357,   398,    68,   174,
     177,   177,   177,   347,   177,   389,   387,   281,   283,   292,
     381,   382,   383,   384,   376,   173,   367,   367,   293,   453,
     153,   160,   196,   214,   317,   214,   295,   350,   151,   151,
     151,     5,   295,   402,   441,   157,   180,   431,     9,   357,
     148,   361,   345,   462,   157,   151,   406,   188,   151,   176,
     155,   151,   151,   155,   151,   198,   151,   151,   151,   198,
      17,   297,   214,   151,   151,   150,   157,   198,   154,   177,
     188,   113,   117,   119,   181,   191,   192,   193,   151,   155,
     191,   154,   155,   148,   212,   156,   151,   191,   177,   362,
     350,   151,   151,   151,   435,   176,   176,   350,   350,   432,
     151,   151,   151,   151,   149,   408,   431,   426,   430,   176,
     176,   154,   177,   463,   176,   176,   177,   177,   177,   177,
     360,   191,   131,   165,   177,   177,   148,   361,   214,   150,
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
      76,   157,   235,   236,   237,   151,   221,   223,   174,   104,
     173,   221,   222,   221,   223,   242,   174,   148,   157,   237,
     223,   149,   176,   174,   182,   151,   156,   151,   151,   155,
     156,   249,   253,   357,   399,   177,   154,   154,   345,   462,
     148,   148,   154,   154,   177,   177,   177,   176,   177,   151,
     151,   151,   151,   151,   447,   402,   334,     1,   213,   233,
     234,   400,     1,   156,     1,   176,   223,   235,   174,   151,
      73,   222,   221,   144,   165,   243,   174,   165,    73,   222,
     174,     1,   176,   176,   260,   293,   295,   456,   156,   174,
     153,   182,   265,   266,   267,   223,   198,   188,    73,   106,
     250,   252,   151,   462,   148,   151,   151,   151,   352,   149,
     402,   439,   440,   336,   131,     1,   155,   156,   148,   270,
     271,   277,    73,   174,   223,   150,   150,   221,   222,   221,
     223,   148,   270,   260,   177,   149,   196,   399,   447,   180,
     156,   101,   149,   151,   156,   155,    73,   151,   223,   149,
     223,   223,   148,   176,   213,   233,   236,   238,   239,   277,
     223,   165,   165,   165,   238,   177,   174,   257,   295,   265,
     154,   213,   174,   265,   267,   223,   221,   107,   107,   350,
     223,   228,   177,   236,   221,   150,   221,   221,   177,   257,
     212,   151,   156,   182,   151,   151,   156,   151,   253,    73,
     248,   177,     1,   223,   148,   228,   148,   151,   225,   182,
     268,   149,   174,   268,   223,    73,   151,   225,   155,   156,
     213,   151,   223,   182,   180,   269,   151,   174,   151,   155,
     174,   180
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_int16 yyr1[] =
{
       0,   175,   176,   177,   178,   178,   178,   178,   178,   179,
     179,   179,   179,   179,   179,   179,   180,   180,   181,   181,
     182,   183,   183,   184,   184,   184,   184,   184,   184,   184,
     184,   184,   185,   185,   186,   186,   187,   187,   187,   187,
     187,   187,   187,   187,   187,   187,   187,   187,   187,   187,
     187,   187,   187,   187,   187,   187,   187,   188,   188,   189,
     189,   190,   190,   191,   191,   192,   192,   192,   192,   192,
     192,   192,   193,   193,   193,   194,   194,   195,   195,   195,
     195,   195,   195,   195,   195,   195,   195,   195,   195,   195,
     195,   196,   196,   196,   197,   197,   197,   197,   198,   198,
     198,   198,   198,   198,   198,   198,   198,   199,   199,   199,
     199,   200,   200,   201,   201,   202,   202,   202,   202,   203,
     203,   203,   204,   204,   204,   205,   205,   205,   205,   205,
     206,   206,   206,   207,   207,   208,   208,   209,   209,   210,
     210,   211,   211,   212,   212,   212,   213,   214,   214,   214,
     215,   215,   216,   216,   217,   217,   218,   218,   218,   218,
     218,   218,   218,   218,   218,   218,   218,   219,   219,   220,
     220,   220,   220,   221,   221,   222,   222,   223,   223,   223,
     223,   223,   223,   223,   223,   223,   223,   223,   223,   223,
     224,   224,   225,   225,   226,   226,   227,   227,   227,   227,
     227,   228,   228,   228,   229,   229,   230,   230,   230,   230,
     230,   230,   230,   231,   231,   232,   232,   232,   232,   233,
     233,   233,   234,   234,   235,   235,   235,   235,   235,   236,
     236,   237,   238,   238,   239,   239,   240,   240,   240,   240,
     240,   240,   240,   240,   240,   241,   241,   242,   242,   242,
     242,   242,   242,   242,   242,   242,   242,   242,   242,   242,
     242,   242,   242,   242,   242,   243,   243,   243,   243,   244,
     244,   244,   244,   244,   244,   244,   244,   244,   244,   244,
     244,   244,   244,   244,   244,   244,   244,   244,   244,   245,
     245,   246,   247,   248,   249,   249,   250,   250,   251,   251,
     252,   253,   253,   253,   253,   253,   253,   254,   254,   255,
     255,   255,   256,   256,   257,   257,   258,   258,   258,   258,
     259,   260,   260,   260,   260,   260,   261,   262,   262,   263,
     263,   263,   263,   263,   264,   264,   265,   265,   266,   266,
     267,   267,   268,   268,   268,   269,   269,   270,   270,   271,
     271,   272,   272,   273,   273,   274,   274,   275,   275,   276,
     276,   277,   277,   277,   278,   278,   279,   279,   279,   279,
     279,   280,   280,   280,   281,   281,   281,   282,   282,   282,
     282,   282,   283,   283,   284,   284,   285,   285,   285,   286,
     286,   286,   286,   286,   287,   287,   288,   288,   288,   288,
     289,   289,   290,   290,   290,   291,   291,   291,   292,   292,
     292,   293,   293,   293,   294,   294,   295,   295,   296,   296,
     297,   297,   297,   297,   297,   298,   299,   299,   299,   300,
     300,   301,   301,   301,   301,   301,   301,   301,   301,   302,
     302,   302,   302,   302,   302,   302,   302,   302,   302,   302,
     302,   302,   302,   302,   302,   302,   302,   302,   302,   302,
     302,   302,   302,   302,   302,   302,   302,   303,   303,   304,
     305,   305,   306,   306,   306,   306,   306,   307,   307,   308,
     308,   308,   308,   309,   309,   309,   309,   309,   309,   310,
     310,   310,   310,   311,   312,   311,   311,   313,   313,   313,
     313,   314,   314,   314,   315,   315,   315,   315,   316,   316,
     316,   317,   317,   317,   317,   317,   317,   318,   318,   318,
     319,   319,   320,   320,   322,   321,   323,   321,   324,   321,
     325,   321,   321,   326,   326,   327,   327,   328,   328,   329,
     329,   329,   330,   330,   330,   330,   330,   330,   330,   330,
     331,   331,   332,   332,   332,   332,   332,   332,   332,   332,
     332,   332,   333,   333,   333,   334,   334,   334,   335,   335,
     335,   336,   337,   337,   338,   338,   339,   339,   340,   341,
     342,   341,   341,   341,   343,   341,   341,   341,   344,   344,
     345,   345,   345,   345,   346,   346,   347,   347,   347,   347,
     347,   347,   347,   348,   348,   348,   348,   349,   349,   350,
     350,   350,   350,   351,   351,   351,   351,   352,   352,   352,
     352,   352,   353,   353,   353,   353,   353,   354,   354,   355,
     355,   356,   356,   357,   357,   357,   358,   358,   358,   359,
     359,   360,   360,   360,   360,   361,   361,   362,   362,   362,
     362,   362,   363,   363,   364,   364,   365,   365,   365,   365,
     365,   366,   366,   367,   367,   369,   368,   370,   368,   368,
     368,   371,   371,   371,   371,   372,   372,   372,   372,   373,
     373,   374,   374,   375,   375,   376,   376,   376,   376,   377,
     377,   377,   378,   378,   379,   379,   380,   380,   381,   381,
     382,   382,   383,   383,   383,   384,   384,   385,   385,   386,
     386,   387,   387,   388,   389,   390,   390,   390,   390,   390,
     391,   390,   392,   390,   393,   390,   394,   390,   395,   390,
     396,   396,   396,   397,   397,   398,   398,   398,   398,   398,
     398,   398,   398,   398,   398,   399,   399,   399,   400,   401,
     401,   402,   402,   403,   403,   404,   405,   405,   406,   406,
     406,   407,   407,   407,   407,   407,   407,   408,   408,   409,
     409,   409,   409,   410,   410,   410,   410,   411,   411,   411,
     411,   411,   411,   411,   412,   412,   412,   412,   413,   413,
     413,   414,   414,   414,   414,   414,   415,   415,   415,   415,
     416,   416,   416,   416,   416,   416,   417,   417,   417,   418,
     418,   418,   418,   418,   419,   419,   419,   419,   420,   420,
     420,   420,   420,   420,   421,   421,   422,   422,   422,   422,
     423,   423,   423,   423,   424,   424,   424,   424,   424,   424,
     424,   425,   425,   425,   425,   425,   426,   426,   426,   426,
     426,   427,   427,   427,   428,   428,   428,   428,   429,   429,
     429,   430,   430,   430,   430,   430,   431,   431,   432,   432,
     432,   433,   433,   434,   434,   435,   435,   435,   436,   436,
     436,   436,   436,   437,   437,   437,   437,   438,   438,   438,
     439,   439,   439,   439,   440,   440,   440,   440,   441,   441,
     441,   441,   442,   442,   442,   442,   442,   443,   443,   443,
     443,   444,   444,   444,   445,   445,   445,   446,   446,   446,
     446,   446,   446,   447,   447,   447,   448,   448,   448,   448,
     448,   449,   449,   449,   449,   450,   450,   451,   451,   451,
     452,   452,   453,   453,   453,   453,   453,   453,   454,   454,
     454,   454,   454,   454,   454,   454,   454,   454,   455,   455,
     455,   455,   456,   456,   456,   457,   457,   458,   458,   458,
     458,   458,   458,   459,   459,   459,   459,   459,   459,   460,
     460,   460,   461,   461,   462,   462,   463,   463
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     0,     0,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     2,     1,     1,     1,     1,     3,     3,     3,
       5,     6,     1,     3,     3,     3,     1,     6,     4,     4,
       4,     3,     3,     3,     3,     3,     2,     5,     3,     3,
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
       4,     4,     2,     6,     1,     2,     1,     2,     1,     2,
       1,     1,     2,     2,     2,     5,     3,     5,    10,     7,
       5,    10,     7,     5,     7,     1,     1,     1,     2,     1,
       3,     1,     1,     3,     2,     3,     3,     2,     2,     1,
       2,     2,     0,     1,     2,     3,     4,     5,     7,     6,
       7,     8,     4,     5,     7,     1,     3,     4,     5,     4,
       1,     2,     3,     5,     2,     3,     4,     5,     7,     3,
       5,     5,     7,     7,     7,     1,     1,     1,     1,     3,
       4,     2,     3,     3,     2,     3,     2,     3,     3,     6,
       2,     2,     3,     3,     3,     3,     3,     3,     5,     1,
       1,     5,     5,     4,     0,     1,     4,     6,     1,     3,
       4,     3,     5,     3,     3,     6,     7,     3,     5,     3,
       3,     4,     8,     9,     0,     2,     1,     1,     1,     1,
       2,     1,     2,     2,     2,     1,     3,     1,     1,     6,
       8,    10,    12,    14,     0,     1,     0,     1,     1,     3,
       4,     7,     0,     1,     3,     1,     3,     0,     1,     1,
       2,     0,     1,     4,     5,     0,     1,     3,     4,     1,
       3,     2,     2,     1,     7,     5,     1,     1,     1,     1,
       1,     2,     3,     6,     3,     3,     4,     1,     2,     2,
       3,     8,     8,     8,     5,     9,     2,     2,     5,     3,
       5,     4,     3,     4,     4,     7,     2,     1,     1,     1,
       3,     6,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     0,     1,     1,     2,     1,     1,
       1,     1,     1,     1,     1,     4,     1,     2,     3,     1,
       2,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     0,     1,     5,
       0,     1,     1,     2,     2,     3,     3,     1,     3,     1,
       2,     2,     2,     4,     4,     4,     4,     1,     1,     1,
       2,     2,     3,     1,     0,     3,     2,     1,     2,     2,
       3,     1,     2,     2,     1,     2,     2,     3,     1,     2,
       2,     1,     2,     3,     1,     2,     3,     1,     3,     4,
       1,     1,     1,     1,     0,     7,     0,     8,     0,     8,
       0,     8,     1,     0,     3,     3,     3,     1,     1,     2,
       1,     1,     1,     2,     1,     2,     1,     2,     1,     2,
       0,     2,     3,     4,     4,     3,     2,     2,     3,     3,
       2,     1,     0,     1,     4,     1,     2,     2,     0,     1,
       4,     1,     2,     3,     1,     2,     0,     1,     2,     6,
       0,     8,     7,     9,     0,    12,    11,     1,     3,     3,
       2,     2,     4,     5,     0,     2,     0,     1,     1,     1,
       5,     5,     5,     1,     5,     5,     9,     1,     5,     0,
       1,     1,     5,     1,     1,     5,     5,     1,     3,     3,
       4,     1,     1,     1,     1,     2,     1,     3,     3,     2,
       3,     1,     3,     1,     1,     1,     1,     1,     2,     1,
       1,     0,     2,     2,     4,     1,     4,     0,     1,     2,
       3,     4,     2,     2,     1,     2,     2,     5,     5,     7,
       6,     1,     3,     0,     2,     0,     5,     0,     5,     3,
       1,     0,     1,     1,     1,     1,     1,     1,     1,     0,
       1,     1,     2,     5,     6,     1,     1,     3,     3,     2,
       3,     3,     2,     4,     1,     4,     7,    10,     1,     4,
       2,     2,     1,     1,     5,     2,     5,     0,     1,     3,
       4,     0,     1,     0,     0,     1,     1,     1,     2,     5,
       0,     6,     0,     8,     0,     7,     0,     7,     0,     8,
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
#line 529 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.enterScope(); }
#line 6982 "Parser/parser.cc"
    break;

  case 3:
#line 533 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 6988 "Parser/parser.cc"
    break;

  case 4:
#line 540 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantInteger( *(yyvsp[0].tok) ) ); }
#line 6994 "Parser/parser.cc"
    break;

  case 5:
#line 541 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7000 "Parser/parser.cc"
    break;

  case 6:
#line 542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7006 "Parser/parser.cc"
    break;

  case 7:
#line 543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7012 "Parser/parser.cc"
    break;

  case 8:
#line 544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantChar( *(yyvsp[0].tok) ) ); }
#line 7018 "Parser/parser.cc"
    break;

  case 19:
#line 565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 7024 "Parser/parser.cc"
    break;

  case 20:
#line 569 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.constant) = build_constantStr( *(yyvsp[0].str) ); }
#line 7030 "Parser/parser.cc"
    break;

  case 21:
#line 573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 7036 "Parser/parser.cc"
    break;

  case 22:
#line 575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 7046 "Parser/parser.cc"
    break;

  case 23:
#line 586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 7052 "Parser/parser.cc"
    break;

  case 24:
#line 588 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 7058 "Parser/parser.cc"
    break;

  case 25:
#line 592 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_dimensionref( (yyvsp[0].tok) ) ); }
#line 7064 "Parser/parser.cc"
    break;

  case 27:
#line 595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 7070 "Parser/parser.cc"
    break;

  case 28:
#line 597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new StmtExpr( dynamic_cast<CompoundStmt *>(maybeMoveBuild<Statement>((yyvsp[-1].sn)) ) ) ); }
#line 7076 "Parser/parser.cc"
    break;

  case 29:
#line 599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7082 "Parser/parser.cc"
    break;

  case 30:
#line 601 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7088 "Parser/parser.cc"
    break;

  case 31:
#line 603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild<Expression>( (yyvsp[-3].en) );
			(yyval.en) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 7098 "Parser/parser.cc"
    break;

  case 33:
#line 617 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// steal the association node from the singleton and delete the wrapper
			(yyvsp[-2].genexpr)->associations.splice((yyvsp[-2].genexpr)->associations.end(), (yyvsp[0].genexpr)->associations);
			delete (yyvsp[0].genexpr);
			(yyval.genexpr) = (yyvsp[-2].genexpr);
		}
#line 7109 "Parser/parser.cc"
    break;

  case 34:
#line 627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuildType((yyvsp[-2].decl)), maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } );
		}
#line 7118 "Parser/parser.cc"
    break;

  case 35:
#line 632 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } ); }
#line 7124 "Parser/parser.cc"
    break;

  case 37:
#line 642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-5].en), new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) ) ) )) ) ); }
#line 7130 "Parser/parser.cc"
    break;

  case 38:
#line 648 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7136 "Parser/parser.cc"
    break;

  case 39:
#line 650 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.en) = new ExpressionNode( new ConstructorExpr( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) ) );
		}
#line 7146 "Parser/parser.cc"
    break;

  case 40:
#line 656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7152 "Parser/parser.cc"
    break;

  case 41:
#line 658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 7158 "Parser/parser.cc"
    break;

  case 42:
#line 660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 7164 "Parser/parser.cc"
    break;

  case 43:
#line 662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), new ExpressionNode( (yyvsp[-2].constant) ) ) ); }
#line 7170 "Parser/parser.cc"
    break;

  case 44:
#line 664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7176 "Parser/parser.cc"
    break;

  case 45:
#line 666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 7182 "Parser/parser.cc"
    break;

  case 46:
#line 668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-1].en), build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) ) ) ); }
#line 7188 "Parser/parser.cc"
    break;

  case 47:
#line 670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7194 "Parser/parser.cc"
    break;

  case 48:
#line 672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[0].aggKey), (yyvsp[-2].en) ) ); }
#line 7200 "Parser/parser.cc"
    break;

  case 49:
#line 674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7206 "Parser/parser.cc"
    break;

  case 50:
#line 676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 7212 "Parser/parser.cc"
    break;

  case 51:
#line 678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7218 "Parser/parser.cc"
    break;

  case 52:
#line 680 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::IncrPost, (yyvsp[-1].en) ) ); }
#line 7224 "Parser/parser.cc"
    break;

  case 53:
#line 682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::DecrPost, (yyvsp[-1].en) ) ); }
#line 7230 "Parser/parser.cc"
    break;

  case 54:
#line 684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].in), true ) ) ); }
#line 7236 "Parser/parser.cc"
    break;

  case 55:
#line 686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].in), true ))->set_maybeConstructed( false ) ) ); }
#line 7242 "Parser/parser.cc"
    break;

  case 56:
#line 688 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) );
		}
#line 7252 "Parser/parser.cc"
    break;

  case 57:
#line 697 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7258 "Parser/parser.cc"
    break;

  case 60:
#line 704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7264 "Parser/parser.cc"
    break;

  case 61:
#line 709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Default parameter for argument is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7270 "Parser/parser.cc"
    break;

  case 64:
#line 716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7276 "Parser/parser.cc"
    break;

  case 66:
#line 722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-1].tok) ) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7282 "Parser/parser.cc"
    break;

  case 67:
#line 724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-3].tok) ) ), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7288 "Parser/parser.cc"
    break;

  case 68:
#line 726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7294 "Parser/parser.cc"
    break;

  case 69:
#line 728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7300 "Parser/parser.cc"
    break;

  case 70:
#line 730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7306 "Parser/parser.cc"
    break;

  case 71:
#line 732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7312 "Parser/parser.cc"
    break;

  case 72:
#line 737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_constantInteger( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 7318 "Parser/parser.cc"
    break;

  case 73:
#line 739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_field_name_FLOATINGconstant( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 7324 "Parser/parser.cc"
    break;

  case 74:
#line 741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_varref( (yyvsp[-1].tok) ), (yyvsp[0].en) ) );
		}
#line 7332 "Parser/parser.cc"
    break;

  case 75:
#line 748 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7338 "Parser/parser.cc"
    break;

  case 76:
#line 750 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Expression * constant = build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) );
			(yyval.en) = (yyvsp[-1].en) != nullptr ? new ExpressionNode( build_fieldSel( (yyvsp[-1].en),  constant ) ) : new ExpressionNode( constant );
		}
#line 7347 "Parser/parser.cc"
    break;

  case 79:
#line 762 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 7353 "Parser/parser.cc"
    break;

  case 80:
#line 764 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en)->set_extension( true ); }
#line 7359 "Parser/parser.cc"
    break;

  case 81:
#line 769 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 7379 "Parser/parser.cc"
    break;

  case 82:
#line 785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_val( (yyvsp[-1].op), (yyvsp[0].en) ) ); }
#line 7385 "Parser/parser.cc"
    break;

  case 83:
#line 787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Incr, (yyvsp[0].en) ) ); }
#line 7391 "Parser/parser.cc"
    break;

  case 84:
#line 789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Decr, (yyvsp[0].en) ) ); }
#line 7397 "Parser/parser.cc"
    break;

  case 85:
#line 791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7403 "Parser/parser.cc"
    break;

  case 86:
#line 793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 7409 "Parser/parser.cc"
    break;

  case 87:
#line 795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7415 "Parser/parser.cc"
    break;

  case 88:
#line 797 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 7421 "Parser/parser.cc"
    break;

  case 89:
#line 799 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_offsetOf( (yyvsp[-3].decl), build_varref( (yyvsp[-1].tok) ) ) ); }
#line 7427 "Parser/parser.cc"
    break;

  case 90:
#line 801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.en) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 7436 "Parser/parser.cc"
    break;

  case 91:
#line 808 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::PointTo; }
#line 7442 "Parser/parser.cc"
    break;

  case 92:
#line 809 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AddressOf; }
#line 7448 "Parser/parser.cc"
    break;

  case 93:
#line 811 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::And; }
#line 7454 "Parser/parser.cc"
    break;

  case 94:
#line 815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::UnPlus; }
#line 7460 "Parser/parser.cc"
    break;

  case 95:
#line 816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::UnMinus; }
#line 7466 "Parser/parser.cc"
    break;

  case 96:
#line 817 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::Neg; }
#line 7472 "Parser/parser.cc"
    break;

  case 97:
#line 818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::BitNeg; }
#line 7478 "Parser/parser.cc"
    break;

  case 99:
#line 824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cast( (yyvsp[-2].decl), (yyvsp[0].en) ) ); }
#line 7484 "Parser/parser.cc"
    break;

  case 100:
#line 826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 7490 "Parser/parser.cc"
    break;

  case 101:
#line 828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 7496 "Parser/parser.cc"
    break;

  case 102:
#line 830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( nullptr ) ) ); }
#line 7502 "Parser/parser.cc"
    break;

  case 103:
#line 832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 7508 "Parser/parser.cc"
    break;

  case 104:
#line 834 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Return cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7514 "Parser/parser.cc"
    break;

  case 105:
#line 836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7520 "Parser/parser.cc"
    break;

  case 106:
#line 838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7526 "Parser/parser.cc"
    break;

  case 114:
#line 858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Exp, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7532 "Parser/parser.cc"
    break;

  case 116:
#line 864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mul, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7538 "Parser/parser.cc"
    break;

  case 117:
#line 866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Div, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7544 "Parser/parser.cc"
    break;

  case 118:
#line 868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mod, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7550 "Parser/parser.cc"
    break;

  case 120:
#line 874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Plus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7556 "Parser/parser.cc"
    break;

  case 121:
#line 876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Minus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7562 "Parser/parser.cc"
    break;

  case 123:
#line 882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7568 "Parser/parser.cc"
    break;

  case 124:
#line 884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::RShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7574 "Parser/parser.cc"
    break;

  case 126:
#line 890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7580 "Parser/parser.cc"
    break;

  case 127:
#line 892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7586 "Parser/parser.cc"
    break;

  case 128:
#line 894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7592 "Parser/parser.cc"
    break;

  case 129:
#line 896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7598 "Parser/parser.cc"
    break;

  case 131:
#line 902 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Eq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7604 "Parser/parser.cc"
    break;

  case 132:
#line 904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Neq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7610 "Parser/parser.cc"
    break;

  case 134:
#line 910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitAnd, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7616 "Parser/parser.cc"
    break;

  case 136:
#line 916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Xor, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7622 "Parser/parser.cc"
    break;

  case 138:
#line 922 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitOr, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7628 "Parser/parser.cc"
    break;

  case 140:
#line 928 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), true ) ); }
#line 7634 "Parser/parser.cc"
    break;

  case 142:
#line 934 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), false ) ); }
#line 7640 "Parser/parser.cc"
    break;

  case 144:
#line 940 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7646 "Parser/parser.cc"
    break;

  case 145:
#line 943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-3].en), (yyvsp[-3].en), (yyvsp[0].en) ) ); }
#line 7652 "Parser/parser.cc"
    break;

  case 148:
#line 954 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
//			if ( $2 == OperKinds::AtAssn ) {
//				SemanticError( yylloc, "C @= assignment is currently unimplemented." ); $$ = nullptr;
//			} else {
				(yyval.en) = new ExpressionNode( build_binary_val( (yyvsp[-1].op), (yyvsp[-2].en), (yyvsp[0].en) ) );
//			} // if
		}
#line 7664 "Parser/parser.cc"
    break;

  case 149:
#line 962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7670 "Parser/parser.cc"
    break;

  case 150:
#line 967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7676 "Parser/parser.cc"
    break;

  case 154:
#line 977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::Assign; }
#line 7682 "Parser/parser.cc"
    break;

  case 155:
#line 978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AtAssn; }
#line 7688 "Parser/parser.cc"
    break;

  case 156:
#line 982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ExpAssn; }
#line 7694 "Parser/parser.cc"
    break;

  case 157:
#line 983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MulAssn; }
#line 7700 "Parser/parser.cc"
    break;

  case 158:
#line 984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::DivAssn; }
#line 7706 "Parser/parser.cc"
    break;

  case 159:
#line 985 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ModAssn; }
#line 7712 "Parser/parser.cc"
    break;

  case 160:
#line 986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::PlusAssn; }
#line 7718 "Parser/parser.cc"
    break;

  case 161:
#line 987 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MinusAssn; }
#line 7724 "Parser/parser.cc"
    break;

  case 162:
#line 988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::LSAssn; }
#line 7730 "Parser/parser.cc"
    break;

  case 163:
#line 989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::RSAssn; }
#line 7736 "Parser/parser.cc"
    break;

  case 164:
#line 990 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AndAssn; }
#line 7742 "Parser/parser.cc"
    break;

  case 165:
#line 991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ERAssn; }
#line 7748 "Parser/parser.cc"
    break;

  case 166:
#line 992 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::OrAssn; }
#line 7754 "Parser/parser.cc"
    break;

  case 167:
#line 1003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)(new ExpressionNode( nullptr ) )->set_last( (yyvsp[-1].en) ) ) ); }
#line 7760 "Parser/parser.cc"
    break;

  case 168:
#line 1005 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-4].en)->set_last( (yyvsp[-1].en) ) ) )); }
#line 7766 "Parser/parser.cc"
    break;

  case 170:
#line 1011 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7772 "Parser/parser.cc"
    break;

  case 171:
#line 1013 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7778 "Parser/parser.cc"
    break;

  case 172:
#line 1015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7784 "Parser/parser.cc"
    break;

  case 174:
#line 1021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new CommaExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7790 "Parser/parser.cc"
    break;

  case 175:
#line 1026 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7796 "Parser/parser.cc"
    break;

  case 187:
#line 1044 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 7802 "Parser/parser.cc"
    break;

  case 189:
#line 1047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_directive( (yyvsp[0].tok) ) ); }
#line 7808 "Parser/parser.cc"
    break;

  case 190:
#line 1053 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[0].sn)->add_label( (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 7814 "Parser/parser.cc"
    break;

  case 191:
#line 1055 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "previous label must be associated with a statement (where a declaration is not a statement). Move the label or terminate with a semi-colon." ); (yyval.sn) = nullptr; }
#line 7820 "Parser/parser.cc"
    break;

  case 192:
#line 1060 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (StatementNode *)0 ) ); }
#line 7826 "Parser/parser.cc"
    break;

  case 193:
#line 1065 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (yyvsp[-2].sn) ) ); }
#line 7832 "Parser/parser.cc"
    break;

  case 195:
#line 1071 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 7838 "Parser/parser.cc"
    break;

  case 196:
#line 1076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7844 "Parser/parser.cc"
    break;

  case 197:
#line 1078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7850 "Parser/parser.cc"
    break;

  case 198:
#line 1080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7856 "Parser/parser.cc"
    break;

  case 199:
#line 1082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7862 "Parser/parser.cc"
    break;

  case 202:
#line 1089 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 7868 "Parser/parser.cc"
    break;

  case 203:
#line 1091 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "declarations only allowed at the start of the switch body, i.e., after the '{'." ); (yyval.sn) = nullptr; }
#line 7874 "Parser/parser.cc"
    break;

  case 204:
#line 1096 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_expr( (yyvsp[-1].en) ) ); }
#line 7880 "Parser/parser.cc"
    break;

  case 205:
#line 1098 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_mutex( nullptr, new StatementNode( build_expr( (yyvsp[-1].en) ) ) ) ); }
#line 7886 "Parser/parser.cc"
    break;

  case 206:
#line 1105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn); }
#line 7892 "Parser/parser.cc"
    break;

  case 207:
#line 1107 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( true, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 7898 "Parser/parser.cc"
    break;

  case 208:
#line 1109 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( true, (yyvsp[-7].en), (yyvsp[-2].sn) ) );
			// The semantics of the declaration list is changed to include associated initialization, which is performed
			// *before* the transfer to the appropriate case clause by hoisting the declarations into a compound
			// statement around the switch.  Statements after the initial declaration list can never be executed, and
			// therefore, are removed from the grammar even though C allows it. The change also applies to choose
			// statement.
			(yyval.sn) = (yyvsp[-3].decl) ? new StatementNode( build_compound( (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 7912 "Parser/parser.cc"
    break;

  case 209:
#line 1119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "only declarations can appear before the list of case clauses." ); (yyval.sn) = nullptr; }
#line 7918 "Parser/parser.cc"
    break;

  case 210:
#line 1121 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( false, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 7924 "Parser/parser.cc"
    break;

  case 211:
#line 1123 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( false, (yyvsp[-7].en), (yyvsp[-2].sn) ) );
			(yyval.sn) = (yyvsp[-3].decl) ? new StatementNode( build_compound( (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 7933 "Parser/parser.cc"
    break;

  case 212:
#line 1128 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "only declarations can appear before the list of case clauses." ); (yyval.sn) = nullptr; }
#line 7939 "Parser/parser.cc"
    break;

  case 213:
#line 1134 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-2].ifctl), maybe_build_compound( (yyvsp[0].sn) ), nullptr ) ); }
#line 7945 "Parser/parser.cc"
    break;

  case 214:
#line 1136 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-4].ifctl), maybe_build_compound( (yyvsp[-2].sn) ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 7951 "Parser/parser.cc"
    break;

  case 215:
#line 1141 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( nullptr, (yyvsp[0].en) ); }
#line 7957 "Parser/parser.cc"
    break;

  case 216:
#line 1143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 7963 "Parser/parser.cc"
    break;

  case 217:
#line 1145 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 7969 "Parser/parser.cc"
    break;

  case 218:
#line 1147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[-1].decl), (yyvsp[0].en) ); }
#line 7975 "Parser/parser.cc"
    break;

  case 219:
#line 1154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = (yyvsp[0].en); }
#line 7981 "Parser/parser.cc"
    break;

  case 220:
#line 1156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7987 "Parser/parser.cc"
    break;

  case 222:
#line 1161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.sn) = new StatementNode( build_case( (yyvsp[0].en) ) ); }
#line 7993 "Parser/parser.cc"
    break;

  case 223:
#line 1163 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)((yyvsp[-2].sn)->set_last( new StatementNode( build_case( (yyvsp[0].en) ) ) ) ); }
#line 7999 "Parser/parser.cc"
    break;

  case 224:
#line 1168 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "missing case list after case." ); (yyval.sn) = nullptr; }
#line 8005 "Parser/parser.cc"
    break;

  case 225:
#line 1169 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn); }
#line 8011 "Parser/parser.cc"
    break;

  case 226:
#line 1171 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "missing colon after case list." ); (yyval.sn) = nullptr; }
#line 8017 "Parser/parser.cc"
    break;

  case 227:
#line 1172 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.sn) = new StatementNode( build_default() ); }
#line 8023 "Parser/parser.cc"
    break;

  case 228:
#line 1175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "missing colon after default." ); (yyval.sn) = nullptr; }
#line 8029 "Parser/parser.cc"
    break;

  case 230:
#line 1180 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)( (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) )); }
#line 8035 "Parser/parser.cc"
    break;

  case 231:
#line 1184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( maybe_build_compound( (yyvsp[0].sn) ) ); }
#line 8041 "Parser/parser.cc"
    break;

  case 232:
#line 1189 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = nullptr; }
#line 8047 "Parser/parser.cc"
    break;

  case 234:
#line 1195 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ); }
#line 8053 "Parser/parser.cc"
    break;

  case 235:
#line 1197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)( (yyvsp[-2].sn)->set_last( (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ) ) ); }
#line 8059 "Parser/parser.cc"
    break;

  case 236:
#line 1202 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( new CondCtl( nullptr, new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8065 "Parser/parser.cc"
    break;

  case 237:
#line 1204 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( (yyvsp[-2].ifctl), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8071 "Parser/parser.cc"
    break;

  case 238:
#line 1206 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( (yyvsp[-4].ifctl), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[0].sn) ) ); }
#line 8077 "Parser/parser.cc"
    break;

  case 239:
#line 1208 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( new ExpressionNode( build_constantInteger( *new string( "1" ) ) ), maybe_build_compound( (yyvsp[-4].sn) ) ) ); }
#line 8083 "Parser/parser.cc"
    break;

  case 240:
#line 1210 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( (yyvsp[-2].en), maybe_build_compound( (yyvsp[-5].sn) ) ) ); }
#line 8089 "Parser/parser.cc"
    break;

  case 241:
#line 1212 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( (yyvsp[-3].en), maybe_build_compound( (yyvsp[-6].sn) ), (yyvsp[0].sn) ) ); }
#line 8095 "Parser/parser.cc"
    break;

  case 242:
#line 1214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( new ForCtrl( (ExpressionNode * )nullptr, (ExpressionNode * )nullptr, (ExpressionNode * )nullptr ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8101 "Parser/parser.cc"
    break;

  case 243:
#line 1216 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( (yyvsp[-2].fctl), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8107 "Parser/parser.cc"
    break;

  case 244:
#line 1218 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( (yyvsp[-4].fctl), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[0].sn) ) ); }
#line 8113 "Parser/parser.cc"
    break;

  case 246:
#line 1228 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 8132 "Parser/parser.cc"
    break;

  case 247:
#line 1246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (ExpressionNode * )nullptr, (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8138 "Parser/parser.cc"
    break;

  case 248:
#line 1248 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8144 "Parser/parser.cc"
    break;

  case 249:
#line 1250 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-3].decl), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8150 "Parser/parser.cc"
    break;

  case 250:
#line 1253 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8157 "Parser/parser.cc"
    break;

  case 251:
#line 1256 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LEThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8164 "Parser/parser.cc"
    break;

  case 252:
#line 1259 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].en)->clone(), (yyvsp[-1].compop), (yyvsp[0].en), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8170 "Parser/parser.cc"
    break;

  case 253:
#line 1261 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8176 "Parser/parser.cc"
    break;

  case 254:
#line 1263 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( new ExpressionNode( build_constantInteger( *new string( "0u" ) ) ), (yyvsp[-1].en), nullptr, OperKinds::LThan, nullptr, nullptr ); }
#line 8182 "Parser/parser.cc"
    break;

  case 255:
#line 1265 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-2].en), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8189 "Parser/parser.cc"
    break;

  case 256:
#line 1268 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-3].en), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LEThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8196 "Parser/parser.cc"
    break;

  case 257:
#line 1271 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), (yyvsp[-1].compop), (yyvsp[0].en), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8202 "Parser/parser.cc"
    break;

  case 258:
#line 1273 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8208 "Parser/parser.cc"
    break;

  case 259:
#line 1276 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Array interator is currently unimplemented." ); (yyval.fctl) = nullptr;
			(yyval.fctl) = forCtrl( new ExpressionNode( build_varref( (yyvsp[0].tok) ) ), (yyvsp[-2].en), nullptr, OperKinds::Range, nullptr, nullptr );
		}
#line 8217 "Parser/parser.cc"
    break;

  case 260:
#line 1283 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), OperKinds::LThan, nullptr, new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8223 "Parser/parser.cc"
    break;

  case 261:
#line 1285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), OperKinds::GThan, nullptr, new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8229 "Parser/parser.cc"
    break;

  case 262:
#line 1287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), OperKinds::LThan, nullptr, (yyvsp[0].en) ); }
#line 8235 "Parser/parser.cc"
    break;

  case 263:
#line 1289 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), OperKinds::GThan, nullptr, (yyvsp[0].en) ); }
#line 8241 "Parser/parser.cc"
    break;

  case 264:
#line 1291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), OperKinds::LThan, nullptr, nullptr ); }
#line 8247 "Parser/parser.cc"
    break;

  case 265:
#line 1296 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LThan; }
#line 8253 "Parser/parser.cc"
    break;

  case 266:
#line 1298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 8259 "Parser/parser.cc"
    break;

  case 267:
#line 1300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GThan; }
#line 8265 "Parser/parser.cc"
    break;

  case 268:
#line 1302 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GEThan; }
#line 8271 "Parser/parser.cc"
    break;

  case 269:
#line 1307 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Goto ) ); }
#line 8277 "Parser/parser.cc"
    break;

  case 270:
#line 1311 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_computedgoto( (yyvsp[-1].en) ) ); }
#line 8283 "Parser/parser.cc"
    break;

  case 271:
#line 1314 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThrough ) ); }
#line 8289 "Parser/parser.cc"
    break;

  case 272:
#line 1316 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::FallThrough ) ); }
#line 8295 "Parser/parser.cc"
    break;

  case 273:
#line 1318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThroughDefault ) ); }
#line 8301 "Parser/parser.cc"
    break;

  case 274:
#line 1321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Continue ) ); }
#line 8307 "Parser/parser.cc"
    break;

  case 275:
#line 1325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Continue ) ); }
#line 8313 "Parser/parser.cc"
    break;

  case 276:
#line 1328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Break ) ); }
#line 8319 "Parser/parser.cc"
    break;

  case 277:
#line 1332 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Break ) ); }
#line 8325 "Parser/parser.cc"
    break;

  case 278:
#line 1334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_return( (yyvsp[-1].en) ) ); }
#line 8331 "Parser/parser.cc"
    break;

  case 279:
#line 1336 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 8337 "Parser/parser.cc"
    break;

  case 280:
#line 1338 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr ) ); }
#line 8343 "Parser/parser.cc"
    break;

  case 281:
#line 1340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn) ) ); }
#line 8349 "Parser/parser.cc"
    break;

  case 282:
#line 1342 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Coroutine ) ); }
#line 8355 "Parser/parser.cc"
    break;

  case 283:
#line 1344 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Coroutine ) ); }
#line 8361 "Parser/parser.cc"
    break;

  case 284:
#line 1346 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Generator ) ); }
#line 8367 "Parser/parser.cc"
    break;

  case 285:
#line 1348 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Generator ) ); }
#line 8373 "Parser/parser.cc"
    break;

  case 286:
#line 1350 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_throw( (yyvsp[-1].en) ) ); }
#line 8379 "Parser/parser.cc"
    break;

  case 287:
#line 1352 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume( (yyvsp[-1].en) ) ); }
#line 8385 "Parser/parser.cc"
    break;

  case 288:
#line 1354 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume_at( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 8391 "Parser/parser.cc"
    break;

  case 291:
#line 1364 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_with( (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8397 "Parser/parser.cc"
    break;

  case 292:
#line 1370 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_mutex( (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8403 "Parser/parser.cc"
    break;

  case 293:
#line 1374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 8409 "Parser/parser.cc"
    break;

  case 294:
#line 1379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8415 "Parser/parser.cc"
    break;

  case 296:
#line 1385 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 8421 "Parser/parser.cc"
    break;

  case 297:
#line 1389 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) )); }
#line 8427 "Parser/parser.cc"
    break;

  case 299:
#line 1396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8433 "Parser/parser.cc"
    break;

  case 300:
#line 1400 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 8439 "Parser/parser.cc"
    break;

  case 301:
#line 1405 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8445 "Parser/parser.cc"
    break;

  case 302:
#line 1407 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-3].en), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[-4].en), (yyvsp[0].wfs) ); }
#line 8451 "Parser/parser.cc"
    break;

  case 303:
#line 1409 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8457 "Parser/parser.cc"
    break;

  case 304:
#line 1411 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( nullptr, maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8463 "Parser/parser.cc"
    break;

  case 305:
#line 1414 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 8469 "Parser/parser.cc"
    break;

  case 306:
#line 1416 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-5].en), maybe_build_compound( (yyvsp[-4].sn) ), (yyvsp[-6].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8475 "Parser/parser.cc"
    break;

  case 307:
#line 1421 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-1].en), (yyvsp[0].sn), (yyvsp[-2].en) ) ); }
#line 8481 "Parser/parser.cc"
    break;

  case 308:
#line 1423 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-3].en), (yyvsp[-2].sn), (yyvsp[-4].en), (yyvsp[0].wfs) ) ); }
#line 8487 "Parser/parser.cc"
    break;

  case 309:
#line 1428 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), (yyvsp[0].sn), 0 ) ); }
#line 8493 "Parser/parser.cc"
    break;

  case 310:
#line 1430 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), 0, (yyvsp[0].sn) ) ); }
#line 8499 "Parser/parser.cc"
    break;

  case 311:
#line 1432 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-2].sn), (yyvsp[-1].sn), (yyvsp[0].sn) ) ); }
#line 8505 "Parser/parser.cc"
    break;

  case 312:
#line 1437 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8511 "Parser/parser.cc"
    break;

  case 313:
#line 1439 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)(yyvsp[-8].sn)->set_last( new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ) ); }
#line 8517 "Parser/parser.cc"
    break;

  case 314:
#line 1444 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8523 "Parser/parser.cc"
    break;

  case 315:
#line 1445 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[0].en); }
#line 8529 "Parser/parser.cc"
    break;

  case 316:
#line 1449 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 8535 "Parser/parser.cc"
    break;

  case 317:
#line 1450 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 8541 "Parser/parser.cc"
    break;

  case 318:
#line 1451 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.catch_kind) = CatchStmt::Resume; }
#line 8547 "Parser/parser.cc"
    break;

  case 319:
#line 1452 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Resume; }
#line 8553 "Parser/parser.cc"
    break;

  case 320:
#line 1456 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = new StatementNode( build_finally( (yyvsp[0].sn) ) ); }
#line 8559 "Parser/parser.cc"
    break;

  case 322:
#line 1463 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 8565 "Parser/parser.cc"
    break;

  case 323:
#line 1465 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 8571 "Parser/parser.cc"
    break;

  case 324:
#line 1467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 8577 "Parser/parser.cc"
    break;

  case 329:
#line 1482 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-4].flag), (yyvsp[-2].constant), 0 ) ); }
#line 8583 "Parser/parser.cc"
    break;

  case 330:
#line 1484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-6].flag), (yyvsp[-4].constant), (yyvsp[-2].en) ) ); }
#line 8589 "Parser/parser.cc"
    break;

  case 331:
#line 1486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-8].flag), (yyvsp[-6].constant), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 8595 "Parser/parser.cc"
    break;

  case 332:
#line 1488 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-10].flag), (yyvsp[-8].constant), (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 8601 "Parser/parser.cc"
    break;

  case 333:
#line 1490 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-12].flag), (yyvsp[-9].constant), 0, (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].label) ) ); }
#line 8607 "Parser/parser.cc"
    break;

  case 334:
#line 1495 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = false; }
#line 8613 "Parser/parser.cc"
    break;

  case 335:
#line 1497 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = true; }
#line 8619 "Parser/parser.cc"
    break;

  case 336:
#line 1502 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8625 "Parser/parser.cc"
    break;

  case 339:
#line 1509 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 8631 "Parser/parser.cc"
    break;

  case 340:
#line 1514 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( nullptr, (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 8637 "Parser/parser.cc"
    break;

  case 341:
#line 1516 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( (yyvsp[-5].tok), (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 8643 "Parser/parser.cc"
    break;

  case 342:
#line 1521 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8649 "Parser/parser.cc"
    break;

  case 343:
#line 1523 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 8655 "Parser/parser.cc"
    break;

  case 344:
#line 1525 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( (yyvsp[0].constant) ) )); }
#line 8661 "Parser/parser.cc"
    break;

  case 345:
#line 1530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = new LabelNode(); (yyval.label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 8670 "Parser/parser.cc"
    break;

  case 346:
#line 1535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = (yyvsp[-2].label); (yyvsp[-2].label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 8679 "Parser/parser.cc"
    break;

  case 347:
#line 1545 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8685 "Parser/parser.cc"
    break;

  case 350:
#line 1552 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 8691 "Parser/parser.cc"
    break;

  case 351:
#line 1557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8697 "Parser/parser.cc"
    break;

  case 353:
#line 1563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 8703 "Parser/parser.cc"
    break;

  case 354:
#line 1565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-2].decl) ); }
#line 8709 "Parser/parser.cc"
    break;

  case 364:
#line 1591 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-4].en), (yyvsp[-2].constant) ); }
#line 8715 "Parser/parser.cc"
    break;

  case 365:
#line 1593 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-2].en), build_constantStr( *new string( "\"\"" ) ) ); }
#line 8721 "Parser/parser.cc"
    break;

  case 369:
#line 1611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 8727 "Parser/parser.cc"
    break;

  case 371:
#line 1617 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].in) ); }
#line 8733 "Parser/parser.cc"
    break;

  case 372:
#line 1621 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 8739 "Parser/parser.cc"
    break;

  case 373:
#line 1623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].in) ) ); }
#line 8745 "Parser/parser.cc"
    break;

  case 374:
#line 1630 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 8751 "Parser/parser.cc"
    break;

  case 375:
#line 1632 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 8757 "Parser/parser.cc"
    break;

  case 376:
#line 1634 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 8763 "Parser/parser.cc"
    break;

  case 378:
#line 1640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 8769 "Parser/parser.cc"
    break;

  case 379:
#line 1642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 8775 "Parser/parser.cc"
    break;

  case 380:
#line 1644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 8781 "Parser/parser.cc"
    break;

  case 381:
#line 1646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeClone( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->appendList( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 8792 "Parser/parser.cc"
    break;

  case 382:
#line 1679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), 0 )->addQualifiers( (yyvsp[0].decl) ); }
#line 8798 "Parser/parser.cc"
    break;

  case 383:
#line 1681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), 0 )->addQualifiers( (yyvsp[0].decl) ); }
#line 8804 "Parser/parser.cc"
    break;

  case 384:
#line 1686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 8810 "Parser/parser.cc"
    break;

  case 385:
#line 1689 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-6].decl)->appendList( (yyvsp[-2].decl) ) ); }
#line 8816 "Parser/parser.cc"
    break;

  case 386:
#line 1694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 8825 "Parser/parser.cc"
    break;

  case 387:
#line 1699 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 8834 "Parser/parser.cc"
    break;

  case 388:
#line 1704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "3" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 8843 "Parser/parser.cc"
    break;

  case 389:
#line 1715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "4" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef();
		}
#line 8852 "Parser/parser.cc"
    break;

  case 390:
#line 1720 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "5" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 8861 "Parser/parser.cc"
    break;

  case 391:
#line 1725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "6" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[-3].decl) )->addTypedef();
		}
#line 8870 "Parser/parser.cc"
    break;

  case 392:
#line 1730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "7" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-2].decl) )->addTypedef();
		}
#line 8879 "Parser/parser.cc"
    break;

  case 393:
#line 1735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "8" );
			(yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-3].decl) )->addTypedef()->addType( (yyvsp[-3].decl) );
		}
#line 8888 "Parser/parser.cc"
    break;

  case 394:
#line 1744 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 8896 "Parser/parser.cc"
    break;

  case 395:
#line 1748 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 8904 "Parser/parser.cc"
    break;

  case 396:
#line 1755 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 8910 "Parser/parser.cc"
    break;

  case 400:
#line 1765 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 8916 "Parser/parser.cc"
    break;

  case 401:
#line 1767 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ) ); }
#line 8922 "Parser/parser.cc"
    break;

  case 414:
#line 1806 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8928 "Parser/parser.cc"
    break;

  case 417:
#line 1818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 8934 "Parser/parser.cc"
    break;

  case 420:
#line 1828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Const ); }
#line 8940 "Parser/parser.cc"
    break;

  case 421:
#line 1830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Restrict ); }
#line 8946 "Parser/parser.cc"
    break;

  case 422:
#line 1832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Volatile ); }
#line 8952 "Parser/parser.cc"
    break;

  case 423:
#line 1834 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Atomic ); }
#line 8958 "Parser/parser.cc"
    break;

  case 425:
#line 1840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newForall( (yyvsp[-1].decl) ); }
#line 8964 "Parser/parser.cc"
    break;

  case 427:
#line 1846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 8970 "Parser/parser.cc"
    break;

  case 428:
#line 1848 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 8976 "Parser/parser.cc"
    break;

  case 430:
#line 1859 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 8982 "Parser/parser.cc"
    break;

  case 431:
#line 1864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Extern ); }
#line 8988 "Parser/parser.cc"
    break;

  case 432:
#line 1866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Static ); }
#line 8994 "Parser/parser.cc"
    break;

  case 433:
#line 1868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Auto ); }
#line 9000 "Parser/parser.cc"
    break;

  case 434:
#line 1870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Register ); }
#line 9006 "Parser/parser.cc"
    break;

  case 435:
#line 1872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Threadlocal ); }
#line 9012 "Parser/parser.cc"
    break;

  case 436:
#line 1875 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Inline ); }
#line 9018 "Parser/parser.cc"
    break;

  case 437:
#line 1877 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Fortran ); }
#line 9024 "Parser/parser.cc"
    break;

  case 438:
#line 1879 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Noreturn ); }
#line 9030 "Parser/parser.cc"
    break;

  case 439:
#line 1884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 9036 "Parser/parser.cc"
    break;

  case 440:
#line 1886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Bool ); }
#line 9042 "Parser/parser.cc"
    break;

  case 441:
#line 1888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Char ); }
#line 9048 "Parser/parser.cc"
    break;

  case 442:
#line 1890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int ); }
#line 9054 "Parser/parser.cc"
    break;

  case 443:
#line 1892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 ); }
#line 9060 "Parser/parser.cc"
    break;

  case 444:
#line 1894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 )->addType( DeclarationNode::newSignedNess( DeclarationNode::Unsigned ) ); }
#line 9066 "Parser/parser.cc"
    break;

  case 445:
#line 1896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Float ); }
#line 9072 "Parser/parser.cc"
    break;

  case 446:
#line 1898 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Double ); }
#line 9078 "Parser/parser.cc"
    break;

  case 447:
#line 1900 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat80 ); }
#line 9084 "Parser/parser.cc"
    break;

  case 448:
#line 1902 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat128 ); }
#line 9090 "Parser/parser.cc"
    break;

  case 449:
#line 1904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat16 ); }
#line 9096 "Parser/parser.cc"
    break;

  case 450:
#line 1906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32 ); }
#line 9102 "Parser/parser.cc"
    break;

  case 451:
#line 1908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32x ); }
#line 9108 "Parser/parser.cc"
    break;

  case 452:
#line 1910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64 ); }
#line 9114 "Parser/parser.cc"
    break;

  case 453:
#line 1912 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64x ); }
#line 9120 "Parser/parser.cc"
    break;

  case 454:
#line 1914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat128 ); }
#line 9126 "Parser/parser.cc"
    break;

  case 455:
#line 1916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9132 "Parser/parser.cc"
    break;

  case 456:
#line 1918 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9138 "Parser/parser.cc"
    break;

  case 457:
#line 1920 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9144 "Parser/parser.cc"
    break;

  case 458:
#line 1922 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Complex ); }
#line 9150 "Parser/parser.cc"
    break;

  case 459:
#line 1924 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Imaginary ); }
#line 9156 "Parser/parser.cc"
    break;

  case 460:
#line 1926 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Signed ); }
#line 9162 "Parser/parser.cc"
    break;

  case 461:
#line 1928 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Unsigned ); }
#line 9168 "Parser/parser.cc"
    break;

  case 462:
#line 1930 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Short ); }
#line 9174 "Parser/parser.cc"
    break;

  case 463:
#line 1932 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Long ); }
#line 9180 "Parser/parser.cc"
    break;

  case 464:
#line 1934 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Valist ); }
#line 9186 "Parser/parser.cc"
    break;

  case 465:
#line 1936 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::AutoType ); }
#line 9192 "Parser/parser.cc"
    break;

  case 467:
#line 1942 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9198 "Parser/parser.cc"
    break;

  case 469:
#line 1948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVtableType( (yyvsp[-2].decl) ); }
#line 9204 "Parser/parser.cc"
    break;

  case 470:
#line 1954 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9210 "Parser/parser.cc"
    break;

  case 471:
#line 1956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9216 "Parser/parser.cc"
    break;

  case 473:
#line 1963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9222 "Parser/parser.cc"
    break;

  case 474:
#line 1965 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9228 "Parser/parser.cc"
    break;

  case 475:
#line 1967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9234 "Parser/parser.cc"
    break;

  case 476:
#line 1969 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 9240 "Parser/parser.cc"
    break;

  case 478:
#line 1976 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9246 "Parser/parser.cc"
    break;

  case 480:
#line 1982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9252 "Parser/parser.cc"
    break;

  case 481:
#line 1984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9258 "Parser/parser.cc"
    break;

  case 482:
#line 1986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 9264 "Parser/parser.cc"
    break;

  case 483:
#line 1991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 9270 "Parser/parser.cc"
    break;

  case 484:
#line 1993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en) ); }
#line 9276 "Parser/parser.cc"
    break;

  case 485:
#line 1995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 9282 "Parser/parser.cc"
    break;

  case 486:
#line 1997 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en), true ); }
#line 9288 "Parser/parser.cc"
    break;

  case 487:
#line 1999 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Zero ); }
#line 9294 "Parser/parser.cc"
    break;

  case 488:
#line 2001 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::One ); }
#line 9300 "Parser/parser.cc"
    break;

  case 490:
#line 2007 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9306 "Parser/parser.cc"
    break;

  case 491:
#line 2009 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9312 "Parser/parser.cc"
    break;

  case 492:
#line 2011 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9318 "Parser/parser.cc"
    break;

  case 494:
#line 2017 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 9324 "Parser/parser.cc"
    break;

  case 495:
#line 2019 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 9330 "Parser/parser.cc"
    break;

  case 496:
#line 2021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 9339 "Parser/parser.cc"
    break;

  case 498:
#line 2030 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9345 "Parser/parser.cc"
    break;

  case 499:
#line 2032 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9351 "Parser/parser.cc"
    break;

  case 500:
#line 2034 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9357 "Parser/parser.cc"
    break;

  case 502:
#line 2040 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9363 "Parser/parser.cc"
    break;

  case 503:
#line 2042 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9369 "Parser/parser.cc"
    break;

  case 505:
#line 2048 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9375 "Parser/parser.cc"
    break;

  case 506:
#line 2050 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9381 "Parser/parser.cc"
    break;

  case 507:
#line 2052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9387 "Parser/parser.cc"
    break;

  case 509:
#line 2058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9393 "Parser/parser.cc"
    break;

  case 510:
#line 2060 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9399 "Parser/parser.cc"
    break;

  case 511:
#line 2065 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypedef( (yyvsp[0].tok) ); }
#line 9405 "Parser/parser.cc"
    break;

  case 512:
#line 2067 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 9411 "Parser/parser.cc"
    break;

  case 513:
#line 2069 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 9417 "Parser/parser.cc"
    break;

  case 515:
#line 2072 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), (yyvsp[0].decl) ); }
#line 9423 "Parser/parser.cc"
    break;

  case 516:
#line 2074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), (yyvsp[0].decl) ); }
#line 9429 "Parser/parser.cc"
    break;

  case 517:
#line 2079 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[0].tok), nullptr ); }
#line 9435 "Parser/parser.cc"
    break;

  case 518:
#line 2081 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-2].tok), nullptr ); }
#line 9441 "Parser/parser.cc"
    break;

  case 519:
#line 2083 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 9447 "Parser/parser.cc"
    break;

  case 524:
#line 2098 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 9453 "Parser/parser.cc"
    break;

  case 525:
#line 2100 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 9459 "Parser/parser.cc"
    break;

  case 526:
#line 2102 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 9468 "Parser/parser.cc"
    break;

  case 527:
#line 2107 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) ); }
#line 9474 "Parser/parser.cc"
    break;

  case 528:
#line 2109 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 9483 "Parser/parser.cc"
    break;

  case 529:
#line 2114 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypedef( (yyvsp[-5].tok) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 9492 "Parser/parser.cc"
    break;

  case 530:
#line 2119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 9501 "Parser/parser.cc"
    break;

  case 531:
#line 2124 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeGen( (yyvsp[-5].tok), nullptr );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 9510 "Parser/parser.cc"
    break;

  case 533:
#line 2133 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9516 "Parser/parser.cc"
    break;

  case 534:
#line 2135 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 9522 "Parser/parser.cc"
    break;

  case 535:
#line 2140 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 9532 "Parser/parser.cc"
    break;

  case 536:
#line 2146 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 9547 "Parser/parser.cc"
    break;

  case 539:
#line 2165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Struct; }
#line 9553 "Parser/parser.cc"
    break;

  case 540:
#line 2167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Union; }
#line 9559 "Parser/parser.cc"
    break;

  case 541:
#line 2169 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Exception; }
#line 9565 "Parser/parser.cc"
    break;

  case 542:
#line 2175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 9571 "Parser/parser.cc"
    break;

  case 543:
#line 2177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 9577 "Parser/parser.cc"
    break;

  case 544:
#line 2179 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Generator; }
#line 9583 "Parser/parser.cc"
    break;

  case 545:
#line 2181 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor generator is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 9589 "Parser/parser.cc"
    break;

  case 546:
#line 2183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Coroutine; }
#line 9595 "Parser/parser.cc"
    break;

  case 547:
#line 2185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor coroutine is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 9601 "Parser/parser.cc"
    break;

  case 548:
#line 2187 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Thread; }
#line 9607 "Parser/parser.cc"
    break;

  case 549:
#line 2189 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor thread is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 9613 "Parser/parser.cc"
    break;

  case 550:
#line 2194 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9619 "Parser/parser.cc"
    break;

  case 551:
#line 2196 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 9625 "Parser/parser.cc"
    break;

  case 552:
#line 2201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); }
#line 9631 "Parser/parser.cc"
    break;

  case 553:
#line 2203 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 9637 "Parser/parser.cc"
    break;

  case 554:
#line 2205 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-1].decl) ) {								// field declarator ?
				(yyvsp[-1].decl) = DeclarationNode::newName( nullptr );
			} // if
			(yyvsp[-1].decl)->inLine = true;
			(yyval.decl) = distAttr( (yyvsp[-2].decl), (yyvsp[-1].decl) );					// mark all fields in list
			distInl( (yyvsp[-1].decl) );
		}
#line 9650 "Parser/parser.cc"
    break;

  case 555:
#line 2214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9656 "Parser/parser.cc"
    break;

  case 558:
#line 2218 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 9662 "Parser/parser.cc"
    break;

  case 559:
#line 2220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 9668 "Parser/parser.cc"
    break;

  case 562:
#line 2227 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9674 "Parser/parser.cc"
    break;

  case 564:
#line 2230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 9680 "Parser/parser.cc"
    break;

  case 565:
#line 2235 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].en) ); }
#line 9686 "Parser/parser.cc"
    break;

  case 566:
#line 2238 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 9692 "Parser/parser.cc"
    break;

  case 567:
#line 2241 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 9698 "Parser/parser.cc"
    break;

  case 568:
#line 2246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9704 "Parser/parser.cc"
    break;

  case 570:
#line 2249 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 9710 "Parser/parser.cc"
    break;

  case 572:
#line 2260 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 9716 "Parser/parser.cc"
    break;

  case 573:
#line 2262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 9722 "Parser/parser.cc"
    break;

  case 575:
#line 2269 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 9728 "Parser/parser.cc"
    break;

  case 576:
#line 2274 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9734 "Parser/parser.cc"
    break;

  case 578:
#line 2280 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 9740 "Parser/parser.cc"
    break;

  case 579:
#line 2285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-4].decl) ); }
#line 9746 "Parser/parser.cc"
    break;

  case 580:
#line 2287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); }
#line 9752 "Parser/parser.cc"
    break;

  case 581:
#line 2289 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].tok), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) ); }
#line 9758 "Parser/parser.cc"
    break;

  case 582:
#line 2292 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-4].decl)->name, (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 9764 "Parser/parser.cc"
    break;

  case 583:
#line 2294 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-6].decl)->storageClasses.val != 0 || (yyvsp[-6].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			SemanticError( yylloc, "Typed enumeration is currently unimplemented." ); (yyval.decl) = nullptr;
		}
#line 9773 "Parser/parser.cc"
    break;

  case 584:
#line 2299 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].decl)->storageClasses.val != 0 || (yyvsp[-4].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			typedefTable.makeTypedef( *(yyvsp[-1].tok) );
		}
#line 9782 "Parser/parser.cc"
    break;

  case 585:
#line 2304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typed enumeration is currently unimplemented." ); (yyval.decl) = nullptr;
		}
#line 9790 "Parser/parser.cc"
    break;

  case 586:
#line 2308 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-8].decl)->storageClasses.val != 0 || (yyvsp[-8].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			typedefTable.makeTypedef( *(yyvsp[-5].decl)->name );
			SemanticError( yylloc, "Typed enumeration is currently unimplemented." ); (yyval.decl) = nullptr;
		}
#line 9800 "Parser/parser.cc"
    break;

  case 588:
#line 2318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); (yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), 0, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 9806 "Parser/parser.cc"
    break;

  case 589:
#line 2320 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].decl)->type->symbolic.name );	(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].decl)->type->symbolic.name, 0, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 9812 "Parser/parser.cc"
    break;

  case 590:
#line 2325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumConstant( (yyvsp[-1].tok), (yyvsp[0].en) ); }
#line 9818 "Parser/parser.cc"
    break;

  case 591:
#line 2327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumConstant( new string("inline"), nullptr ); }
#line 9824 "Parser/parser.cc"
    break;

  case 592:
#line 2329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( DeclarationNode::newEnumConstant( (yyvsp[-1].tok), (yyvsp[0].en) ) ); }
#line 9830 "Parser/parser.cc"
    break;

  case 593:
#line 2331 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( DeclarationNode::newEnumConstant( new string("inline"), nullptr ) ); }
#line 9836 "Parser/parser.cc"
    break;

  case 594:
#line 2336 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9842 "Parser/parser.cc"
    break;

  case 595:
#line 2340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].in)->get_expression(); }
#line 9848 "Parser/parser.cc"
    break;

  case 596:
#line 2345 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 9854 "Parser/parser.cc"
    break;

  case 597:
#line 2347 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9860 "Parser/parser.cc"
    break;

  case 600:
#line 2351 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9866 "Parser/parser.cc"
    break;

  case 601:
#line 2353 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 9872 "Parser/parser.cc"
    break;

  case 602:
#line 2355 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 9878 "Parser/parser.cc"
    break;

  case 604:
#line 2363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9884 "Parser/parser.cc"
    break;

  case 605:
#line 2365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9890 "Parser/parser.cc"
    break;

  case 606:
#line 2367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-8].decl)->appendList( (yyvsp[-4].decl) )->appendList( (yyvsp[0].decl) ); }
#line 9896 "Parser/parser.cc"
    break;

  case 608:
#line 2373 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9902 "Parser/parser.cc"
    break;

  case 609:
#line 2378 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9908 "Parser/parser.cc"
    break;

  case 610:
#line 2380 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9914 "Parser/parser.cc"
    break;

  case 612:
#line 2383 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 9920 "Parser/parser.cc"
    break;

  case 615:
#line 2390 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9926 "Parser/parser.cc"
    break;

  case 616:
#line 2392 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9932 "Parser/parser.cc"
    break;

  case 618:
#line 2401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 9938 "Parser/parser.cc"
    break;

  case 619:
#line 2404 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 9944 "Parser/parser.cc"
    break;

  case 620:
#line 2406 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 9950 "Parser/parser.cc"
    break;

  case 625:
#line 2416 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9956 "Parser/parser.cc"
    break;

  case 627:
#line 2423 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 9962 "Parser/parser.cc"
    break;

  case 628:
#line 2425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 9968 "Parser/parser.cc"
    break;

  case 629:
#line 2430 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 9974 "Parser/parser.cc"
    break;

  case 630:
#line 2432 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 9980 "Parser/parser.cc"
    break;

  case 631:
#line 2441 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 9986 "Parser/parser.cc"
    break;

  case 632:
#line 2443 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 9992 "Parser/parser.cc"
    break;

  case 638:
#line 2456 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 9998 "Parser/parser.cc"
    break;

  case 641:
#line 2466 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 10004 "Parser/parser.cc"
    break;

  case 642:
#line 2467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (yyvsp[-1].op) == OperKinds::Assign ? (yyvsp[0].in) : (yyvsp[0].in)->set_maybeConstructed( false ); }
#line 10010 "Parser/parser.cc"
    break;

  case 643:
#line 2468 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.in) = new InitializerNode( true ); }
#line 10016 "Parser/parser.cc"
    break;

  case 644:
#line 2469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 10022 "Parser/parser.cc"
    break;

  case 645:
#line 2473 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = new InitializerNode( (yyvsp[0].en) ); }
#line 10028 "Parser/parser.cc"
    break;

  case 646:
#line 2474 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 10034 "Parser/parser.cc"
    break;

  case 647:
#line 2479 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 10040 "Parser/parser.cc"
    break;

  case 649:
#line 2481 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ); }
#line 10046 "Parser/parser.cc"
    break;

  case 650:
#line 2482 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (InitializerNode *)( (yyvsp[-2].in)->set_last( (yyvsp[0].in) ) ); }
#line 10052 "Parser/parser.cc"
    break;

  case 651:
#line 2483 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.in) = (InitializerNode *)((yyvsp[-3].in)->set_last( (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ) )); }
#line 10058 "Parser/parser.cc"
    break;

  case 653:
#line 2499 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[-1].tok) ) ); }
#line 10064 "Parser/parser.cc"
    break;

  case 655:
#line 2505 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-1].en)->set_last( (yyvsp[0].en) )); }
#line 10070 "Parser/parser.cc"
    break;

  case 656:
#line 2511 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 10076 "Parser/parser.cc"
    break;

  case 657:
#line 2514 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 10082 "Parser/parser.cc"
    break;

  case 658:
#line 2516 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 10088 "Parser/parser.cc"
    break;

  case 659:
#line 2518 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-4].en) ), maybeMoveBuild<Expression>( (yyvsp[-2].en) ) ) ); }
#line 10094 "Parser/parser.cc"
    break;

  case 660:
#line 2520 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 10100 "Parser/parser.cc"
    break;

  case 662:
#line 2544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl) ); }
#line 10106 "Parser/parser.cc"
    break;

  case 663:
#line 2549 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10112 "Parser/parser.cc"
    break;

  case 664:
#line 2551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 10118 "Parser/parser.cc"
    break;

  case 665:
#line 2556 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "9" );
			if ( (yyvsp[-1].tclass) == TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 10129 "Parser/parser.cc"
    break;

  case 666:
#line 2563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 10135 "Parser/parser.cc"
    break;

  case 667:
#line 2565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "9" ); }
#line 10141 "Parser/parser.cc"
    break;

  case 668:
#line 2567 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 10147 "Parser/parser.cc"
    break;

  case 669:
#line 2569 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "9" );
			(yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 10156 "Parser/parser.cc"
    break;

  case 670:
#line 2575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 10162 "Parser/parser.cc"
    break;

  case 671:
#line 2580 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 10168 "Parser/parser.cc"
    break;

  case 672:
#line 2582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 10174 "Parser/parser.cc"
    break;

  case 673:
#line 2584 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::DStype; }
#line 10180 "Parser/parser.cc"
    break;

  case 674:
#line 2588 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 10186 "Parser/parser.cc"
    break;

  case 675:
#line 2593 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 10192 "Parser/parser.cc"
    break;

  case 676:
#line 2595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 10198 "Parser/parser.cc"
    break;

  case 677:
#line 2597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ftype; }
#line 10204 "Parser/parser.cc"
    break;

  case 678:
#line 2599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 10210 "Parser/parser.cc"
    break;

  case 679:
#line 2604 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10216 "Parser/parser.cc"
    break;

  case 682:
#line 2611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 10222 "Parser/parser.cc"
    break;

  case 683:
#line 2616 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 10228 "Parser/parser.cc"
    break;

  case 684:
#line 2618 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 10234 "Parser/parser.cc"
    break;

  case 685:
#line 2625 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 10240 "Parser/parser.cc"
    break;

  case 687:
#line 2628 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 10246 "Parser/parser.cc"
    break;

  case 688:
#line 2630 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)( (yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 10252 "Parser/parser.cc"
    break;

  case 689:
#line 2635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 10258 "Parser/parser.cc"
    break;

  case 690:
#line 2637 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10264 "Parser/parser.cc"
    break;

  case 691:
#line 2639 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 10270 "Parser/parser.cc"
    break;

  case 692:
#line 2644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 10276 "Parser/parser.cc"
    break;

  case 693:
#line 2646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 10282 "Parser/parser.cc"
    break;

  case 694:
#line 2651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "10" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), 0 );
		}
#line 10291 "Parser/parser.cc"
    break;

  case 695:
#line 2656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "11" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 10300 "Parser/parser.cc"
    break;

  case 696:
#line 2664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), 0 ); }
#line 10306 "Parser/parser.cc"
    break;

  case 697:
#line 2666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-8].tok), (yyvsp[-6].decl), (yyvsp[-2].decl) ); }
#line 10312 "Parser/parser.cc"
    break;

  case 699:
#line 2672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl) ); }
#line 10318 "Parser/parser.cc"
    break;

  case 704:
#line 2684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 10324 "Parser/parser.cc"
    break;

  case 705:
#line 2689 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10330 "Parser/parser.cc"
    break;

  case 706:
#line 2691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 10336 "Parser/parser.cc"
    break;

  case 708:
#line 2699 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl);	}
#line 10342 "Parser/parser.cc"
    break;

  case 709:
#line 2704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10348 "Parser/parser.cc"
    break;

  case 710:
#line 2706 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->appendList( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 10354 "Parser/parser.cc"
    break;

  case 711:
#line 2711 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10360 "Parser/parser.cc"
    break;

  case 713:
#line 2716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 10366 "Parser/parser.cc"
    break;

  case 714:
#line 2720 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 10372 "Parser/parser.cc"
    break;

  case 715:
#line 2725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( (yyvsp[0].tok) ) ) ); }
#line 10378 "Parser/parser.cc"
    break;

  case 718:
#line 2729 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 10387 "Parser/parser.cc"
    break;

  case 719:
#line 2734 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( false, (yyvsp[-2].constant), 0 ) ) ); }
#line 10393 "Parser/parser.cc"
    break;

  case 720:
#line 2736 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = LinkageSpec::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 10402 "Parser/parser.cc"
    break;

  case 721:
#line 2741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-1].decl);
		}
#line 10412 "Parser/parser.cc"
    break;

  case 722:
#line 2747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = LinkageSpec::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 10421 "Parser/parser.cc"
    break;

  case 723:
#line 2752 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10431 "Parser/parser.cc"
    break;

  case 724:
#line 2758 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 10440 "Parser/parser.cc"
    break;

  case 725:
#line 2763 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10450 "Parser/parser.cc"
    break;

  case 726:
#line 2769 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 10459 "Parser/parser.cc"
    break;

  case 727:
#line 2774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10469 "Parser/parser.cc"
    break;

  case 728:
#line 2780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.val) || (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 10478 "Parser/parser.cc"
    break;

  case 729:
#line 2785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10488 "Parser/parser.cc"
    break;

  case 731:
#line 2800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].sn) ); }
#line 10494 "Parser/parser.cc"
    break;

  case 732:
#line 2802 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].sn) ); }
#line 10500 "Parser/parser.cc"
    break;

  case 733:
#line 2807 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; forall = false; }
#line 10506 "Parser/parser.cc"
    break;

  case 734:
#line 2809 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); forall = false; }
#line 10512 "Parser/parser.cc"
    break;

  case 735:
#line 2814 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10523 "Parser/parser.cc"
    break;

  case 736:
#line 2821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 10532 "Parser/parser.cc"
    break;

  case 737:
#line 2826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 10541 "Parser/parser.cc"
    break;

  case 738:
#line 2832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 10547 "Parser/parser.cc"
    break;

  case 739:
#line 2835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 10553 "Parser/parser.cc"
    break;

  case 740:
#line 2838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10559 "Parser/parser.cc"
    break;

  case 741:
#line 2842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-4].decl) );
		}
#line 10568 "Parser/parser.cc"
    break;

  case 742:
#line 2848 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10574 "Parser/parser.cc"
    break;

  case 743:
#line 2851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10580 "Parser/parser.cc"
    break;

  case 744:
#line 2854 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 10586 "Parser/parser.cc"
    break;

  case 748:
#line 2865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 10592 "Parser/parser.cc"
    break;

  case 749:
#line 2870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10598 "Parser/parser.cc"
    break;

  case 750:
#line 2872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = (yyvsp[-2].constant);
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 10608 "Parser/parser.cc"
    break;

  case 751:
#line 2881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10614 "Parser/parser.cc"
    break;

  case 754:
#line 2888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10620 "Parser/parser.cc"
    break;

  case 755:
#line 2893 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 10626 "Parser/parser.cc"
    break;

  case 757:
#line 2899 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10632 "Parser/parser.cc"
    break;

  case 758:
#line 2904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10638 "Parser/parser.cc"
    break;

  case 759:
#line 2906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 10644 "Parser/parser.cc"
    break;

  case 760:
#line 2908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 10650 "Parser/parser.cc"
    break;

  case 765:
#line 2917 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 10656 "Parser/parser.cc"
    break;

  case 766:
#line 2919 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 10662 "Parser/parser.cc"
    break;

  case 767:
#line 2954 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 10668 "Parser/parser.cc"
    break;

  case 768:
#line 2956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10674 "Parser/parser.cc"
    break;

  case 769:
#line 2961 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10680 "Parser/parser.cc"
    break;

  case 771:
#line 2964 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10686 "Parser/parser.cc"
    break;

  case 772:
#line 2966 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10692 "Parser/parser.cc"
    break;

  case 773:
#line 2971 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10698 "Parser/parser.cc"
    break;

  case 774:
#line 2973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10704 "Parser/parser.cc"
    break;

  case 775:
#line 2975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10710 "Parser/parser.cc"
    break;

  case 776:
#line 2977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10716 "Parser/parser.cc"
    break;

  case 777:
#line 2982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 10722 "Parser/parser.cc"
    break;

  case 778:
#line 2984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10728 "Parser/parser.cc"
    break;

  case 779:
#line 2986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10734 "Parser/parser.cc"
    break;

  case 780:
#line 2988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10740 "Parser/parser.cc"
    break;

  case 781:
#line 2990 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10746 "Parser/parser.cc"
    break;

  case 782:
#line 2992 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10752 "Parser/parser.cc"
    break;

  case 783:
#line 2994 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10758 "Parser/parser.cc"
    break;

  case 784:
#line 2999 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10764 "Parser/parser.cc"
    break;

  case 785:
#line 3001 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 10770 "Parser/parser.cc"
    break;

  case 786:
#line 3003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10776 "Parser/parser.cc"
    break;

  case 787:
#line 3005 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10782 "Parser/parser.cc"
    break;

  case 788:
#line 3014 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10788 "Parser/parser.cc"
    break;

  case 790:
#line 3017 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10794 "Parser/parser.cc"
    break;

  case 791:
#line 3022 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10800 "Parser/parser.cc"
    break;

  case 792:
#line 3024 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10806 "Parser/parser.cc"
    break;

  case 793:
#line 3026 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 10812 "Parser/parser.cc"
    break;

  case 794:
#line 3028 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10818 "Parser/parser.cc"
    break;

  case 795:
#line 3030 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10824 "Parser/parser.cc"
    break;

  case 796:
#line 3035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10830 "Parser/parser.cc"
    break;

  case 797:
#line 3037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10836 "Parser/parser.cc"
    break;

  case 798:
#line 3039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10842 "Parser/parser.cc"
    break;

  case 799:
#line 3041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10848 "Parser/parser.cc"
    break;

  case 800:
#line 3046 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10854 "Parser/parser.cc"
    break;

  case 801:
#line 3048 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10860 "Parser/parser.cc"
    break;

  case 802:
#line 3050 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10866 "Parser/parser.cc"
    break;

  case 803:
#line 3052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10872 "Parser/parser.cc"
    break;

  case 804:
#line 3054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10878 "Parser/parser.cc"
    break;

  case 805:
#line 3056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10884 "Parser/parser.cc"
    break;

  case 809:
#line 3074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 10890 "Parser/parser.cc"
    break;

  case 810:
#line 3076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10896 "Parser/parser.cc"
    break;

  case 811:
#line 3078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 10902 "Parser/parser.cc"
    break;

  case 812:
#line 3080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10908 "Parser/parser.cc"
    break;

  case 813:
#line 3082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10914 "Parser/parser.cc"
    break;

  case 814:
#line 3087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10920 "Parser/parser.cc"
    break;

  case 815:
#line 3089 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10926 "Parser/parser.cc"
    break;

  case 816:
#line 3091 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10932 "Parser/parser.cc"
    break;

  case 817:
#line 3093 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10938 "Parser/parser.cc"
    break;

  case 818:
#line 3098 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10944 "Parser/parser.cc"
    break;

  case 819:
#line 3100 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10950 "Parser/parser.cc"
    break;

  case 820:
#line 3102 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10956 "Parser/parser.cc"
    break;

  case 821:
#line 3104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10962 "Parser/parser.cc"
    break;

  case 822:
#line 3106 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10968 "Parser/parser.cc"
    break;

  case 823:
#line 3108 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10974 "Parser/parser.cc"
    break;

  case 824:
#line 3123 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "ID" );
		}
#line 10983 "Parser/parser.cc"
    break;

  case 825:
#line 3128 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10989 "Parser/parser.cc"
    break;

  case 826:
#line 3133 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10995 "Parser/parser.cc"
    break;

  case 828:
#line 3136 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11001 "Parser/parser.cc"
    break;

  case 829:
#line 3138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11007 "Parser/parser.cc"
    break;

  case 830:
#line 3143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11013 "Parser/parser.cc"
    break;

  case 831:
#line 3145 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11019 "Parser/parser.cc"
    break;

  case 832:
#line 3147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11025 "Parser/parser.cc"
    break;

  case 833:
#line 3149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11031 "Parser/parser.cc"
    break;

  case 834:
#line 3154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11037 "Parser/parser.cc"
    break;

  case 835:
#line 3156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11043 "Parser/parser.cc"
    break;

  case 836:
#line 3158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11049 "Parser/parser.cc"
    break;

  case 837:
#line 3160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11055 "Parser/parser.cc"
    break;

  case 838:
#line 3162 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11061 "Parser/parser.cc"
    break;

  case 839:
#line 3164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11067 "Parser/parser.cc"
    break;

  case 840:
#line 3166 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11073 "Parser/parser.cc"
    break;

  case 841:
#line 3171 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11079 "Parser/parser.cc"
    break;

  case 842:
#line 3173 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11085 "Parser/parser.cc"
    break;

  case 843:
#line 3175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 11091 "Parser/parser.cc"
    break;

  case 844:
#line 3177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11097 "Parser/parser.cc"
    break;

  case 845:
#line 3179 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11103 "Parser/parser.cc"
    break;

  case 846:
#line 3189 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11109 "Parser/parser.cc"
    break;

  case 847:
#line 3191 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11115 "Parser/parser.cc"
    break;

  case 849:
#line 3194 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11121 "Parser/parser.cc"
    break;

  case 850:
#line 3196 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11127 "Parser/parser.cc"
    break;

  case 851:
#line 3201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11133 "Parser/parser.cc"
    break;

  case 852:
#line 3203 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11139 "Parser/parser.cc"
    break;

  case 853:
#line 3205 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11145 "Parser/parser.cc"
    break;

  case 854:
#line 3210 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11151 "Parser/parser.cc"
    break;

  case 855:
#line 3212 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11157 "Parser/parser.cc"
    break;

  case 856:
#line 3214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11163 "Parser/parser.cc"
    break;

  case 857:
#line 3216 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11169 "Parser/parser.cc"
    break;

  case 858:
#line 3221 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11175 "Parser/parser.cc"
    break;

  case 859:
#line 3223 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11181 "Parser/parser.cc"
    break;

  case 860:
#line 3225 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11187 "Parser/parser.cc"
    break;

  case 861:
#line 3239 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11193 "Parser/parser.cc"
    break;

  case 862:
#line 3241 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11199 "Parser/parser.cc"
    break;

  case 864:
#line 3244 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11205 "Parser/parser.cc"
    break;

  case 865:
#line 3246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11211 "Parser/parser.cc"
    break;

  case 866:
#line 3251 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11217 "Parser/parser.cc"
    break;

  case 867:
#line 3253 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11223 "Parser/parser.cc"
    break;

  case 868:
#line 3258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11229 "Parser/parser.cc"
    break;

  case 869:
#line 3260 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11235 "Parser/parser.cc"
    break;

  case 870:
#line 3262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11241 "Parser/parser.cc"
    break;

  case 871:
#line 3267 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11247 "Parser/parser.cc"
    break;

  case 872:
#line 3269 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11253 "Parser/parser.cc"
    break;

  case 873:
#line 3274 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11259 "Parser/parser.cc"
    break;

  case 874:
#line 3276 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11265 "Parser/parser.cc"
    break;

  case 876:
#line 3294 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11271 "Parser/parser.cc"
    break;

  case 877:
#line 3296 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11277 "Parser/parser.cc"
    break;

  case 878:
#line 3301 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( 0, (yyvsp[0].op) ); }
#line 11283 "Parser/parser.cc"
    break;

  case 879:
#line 3303 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 11289 "Parser/parser.cc"
    break;

  case 880:
#line 3305 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11295 "Parser/parser.cc"
    break;

  case 881:
#line 3307 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11301 "Parser/parser.cc"
    break;

  case 882:
#line 3309 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11307 "Parser/parser.cc"
    break;

  case 884:
#line 3315 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11313 "Parser/parser.cc"
    break;

  case 885:
#line 3317 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11319 "Parser/parser.cc"
    break;

  case 886:
#line 3319 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11325 "Parser/parser.cc"
    break;

  case 887:
#line 3324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 11331 "Parser/parser.cc"
    break;

  case 888:
#line 3326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11337 "Parser/parser.cc"
    break;

  case 889:
#line 3328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11343 "Parser/parser.cc"
    break;

  case 890:
#line 3334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false ); }
#line 11349 "Parser/parser.cc"
    break;

  case 891:
#line 3336 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false )->addArray( (yyvsp[0].decl) ); }
#line 11355 "Parser/parser.cc"
    break;

  case 892:
#line 3338 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-4].en), 0, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].en), 0, false ) ); }
#line 11361 "Parser/parser.cc"
    break;

  case 894:
#line 3345 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), 0, false ); }
#line 11367 "Parser/parser.cc"
    break;

  case 895:
#line 3347 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 11373 "Parser/parser.cc"
    break;

  case 896:
#line 3349 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newArray( (yyvsp[-2].en), 0, false ) ); }
#line 11379 "Parser/parser.cc"
    break;

  case 897:
#line 3351 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 11385 "Parser/parser.cc"
    break;

  case 899:
#line 3386 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 11391 "Parser/parser.cc"
    break;

  case 900:
#line 3388 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11397 "Parser/parser.cc"
    break;

  case 901:
#line 3390 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11403 "Parser/parser.cc"
    break;

  case 902:
#line 3395 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].op) ); }
#line 11409 "Parser/parser.cc"
    break;

  case 903:
#line 3397 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 11415 "Parser/parser.cc"
    break;

  case 904:
#line 3399 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 11421 "Parser/parser.cc"
    break;

  case 905:
#line 3401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11427 "Parser/parser.cc"
    break;

  case 906:
#line 3403 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11433 "Parser/parser.cc"
    break;

  case 908:
#line 3409 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11439 "Parser/parser.cc"
    break;

  case 909:
#line 3411 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11445 "Parser/parser.cc"
    break;

  case 910:
#line 3413 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11451 "Parser/parser.cc"
    break;

  case 911:
#line 3418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 11457 "Parser/parser.cc"
    break;

  case 912:
#line 3420 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11463 "Parser/parser.cc"
    break;

  case 913:
#line 3422 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11469 "Parser/parser.cc"
    break;

  case 915:
#line 3429 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11475 "Parser/parser.cc"
    break;

  case 917:
#line 3440 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false ); }
#line 11481 "Parser/parser.cc"
    break;

  case 918:
#line 3443 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 11487 "Parser/parser.cc"
    break;

  case 919:
#line 3445 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, (yyvsp[-2].decl), false ); }
#line 11493 "Parser/parser.cc"
    break;

  case 920:
#line 3448 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 11499 "Parser/parser.cc"
    break;

  case 921:
#line 3450 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 11505 "Parser/parser.cc"
    break;

  case 922:
#line 3452 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-4].decl), true ); }
#line 11511 "Parser/parser.cc"
    break;

  case 924:
#line 3467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11517 "Parser/parser.cc"
    break;

  case 925:
#line 3469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11523 "Parser/parser.cc"
    break;

  case 926:
#line 3474 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( 0, (yyvsp[0].op) ); }
#line 11529 "Parser/parser.cc"
    break;

  case 927:
#line 3476 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 11535 "Parser/parser.cc"
    break;

  case 928:
#line 3478 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11541 "Parser/parser.cc"
    break;

  case 929:
#line 3480 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11547 "Parser/parser.cc"
    break;

  case 930:
#line 3482 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11553 "Parser/parser.cc"
    break;

  case 932:
#line 3488 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11559 "Parser/parser.cc"
    break;

  case 933:
#line 3490 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11565 "Parser/parser.cc"
    break;

  case 934:
#line 3492 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11571 "Parser/parser.cc"
    break;

  case 935:
#line 3497 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11577 "Parser/parser.cc"
    break;

  case 936:
#line 3499 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11583 "Parser/parser.cc"
    break;

  case 939:
#line 3509 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11589 "Parser/parser.cc"
    break;

  case 942:
#line 3520 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11595 "Parser/parser.cc"
    break;

  case 943:
#line 3522 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11601 "Parser/parser.cc"
    break;

  case 944:
#line 3524 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11607 "Parser/parser.cc"
    break;

  case 945:
#line 3526 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11613 "Parser/parser.cc"
    break;

  case 946:
#line 3528 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11619 "Parser/parser.cc"
    break;

  case 947:
#line 3530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11625 "Parser/parser.cc"
    break;

  case 948:
#line 3537 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11631 "Parser/parser.cc"
    break;

  case 949:
#line 3539 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11637 "Parser/parser.cc"
    break;

  case 950:
#line 3541 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11643 "Parser/parser.cc"
    break;

  case 951:
#line 3543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 11649 "Parser/parser.cc"
    break;

  case 952:
#line 3545 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11655 "Parser/parser.cc"
    break;

  case 953:
#line 3548 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11661 "Parser/parser.cc"
    break;

  case 954:
#line 3550 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11667 "Parser/parser.cc"
    break;

  case 955:
#line 3552 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11673 "Parser/parser.cc"
    break;

  case 956:
#line 3554 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 11679 "Parser/parser.cc"
    break;

  case 957:
#line 3556 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11685 "Parser/parser.cc"
    break;

  case 958:
#line 3561 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 11691 "Parser/parser.cc"
    break;

  case 959:
#line 3563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 11697 "Parser/parser.cc"
    break;

  case 960:
#line 3568 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 11703 "Parser/parser.cc"
    break;

  case 961:
#line 3570 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl)->addQualifiers( (yyvsp[-4].decl) ), true ); }
#line 11709 "Parser/parser.cc"
    break;

  case 963:
#line 3597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11715 "Parser/parser.cc"
    break;

  case 967:
#line 3608 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11721 "Parser/parser.cc"
    break;

  case 968:
#line 3610 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11727 "Parser/parser.cc"
    break;

  case 969:
#line 3612 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11733 "Parser/parser.cc"
    break;

  case 970:
#line 3614 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11739 "Parser/parser.cc"
    break;

  case 971:
#line 3616 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11745 "Parser/parser.cc"
    break;

  case 972:
#line 3618 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11751 "Parser/parser.cc"
    break;

  case 973:
#line 3625 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11757 "Parser/parser.cc"
    break;

  case 974:
#line 3627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11763 "Parser/parser.cc"
    break;

  case 975:
#line 3629 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11769 "Parser/parser.cc"
    break;

  case 976:
#line 3631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11775 "Parser/parser.cc"
    break;

  case 977:
#line 3633 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11781 "Parser/parser.cc"
    break;

  case 978:
#line 3635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11787 "Parser/parser.cc"
    break;

  case 979:
#line 3640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 11793 "Parser/parser.cc"
    break;

  case 980:
#line 3642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11799 "Parser/parser.cc"
    break;

  case 981:
#line 3644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11805 "Parser/parser.cc"
    break;

  case 982:
#line 3651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 11811 "Parser/parser.cc"
    break;

  case 983:
#line 3653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 11817 "Parser/parser.cc"
    break;

  case 986:
#line 3677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 11823 "Parser/parser.cc"
    break;

  case 987:
#line 3679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 11829 "Parser/parser.cc"
    break;


#line 11833 "Parser/parser.cc"

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
#line 3682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "make install" //
// End: //
