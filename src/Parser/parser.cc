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
#define YYLAST   19324

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  175
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  289
/* YYNRULES -- Number of rules.  */
#define YYNRULES  989
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  2005

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
     657,   663,   669,   671,   673,   675,   677,   679,   681,   683,
     685,   687,   689,   691,   693,   695,   697,   699,   701,   711,
     712,   716,   717,   722,   725,   729,   730,   734,   735,   737,
     739,   741,   743,   745,   750,   752,   754,   762,   763,   771,
     774,   775,   777,   782,   798,   800,   802,   804,   806,   808,
     810,   812,   814,   822,   823,   825,   829,   830,   831,   832,
     836,   837,   839,   841,   843,   845,   847,   849,   851,   858,
     859,   860,   861,   865,   866,   870,   871,   876,   877,   879,
     881,   886,   887,   889,   894,   895,   897,   902,   903,   905,
     907,   909,   914,   915,   917,   922,   923,   928,   929,   934,
     935,   940,   941,   946,   947,   952,   953,   956,   961,   966,
     967,   975,   981,   982,   986,   987,   991,   992,   996,   997,
     998,   999,  1000,  1001,  1002,  1003,  1004,  1005,  1006,  1016,
    1018,  1023,  1024,  1026,  1028,  1033,  1034,  1040,  1041,  1047,
    1048,  1049,  1050,  1051,  1052,  1053,  1054,  1055,  1056,  1057,
    1059,  1060,  1066,  1068,  1078,  1080,  1088,  1089,  1094,  1096,
    1098,  1100,  1102,  1106,  1107,  1109,  1114,  1116,  1123,  1125,
    1127,  1137,  1139,  1141,  1146,  1151,  1154,  1159,  1161,  1163,
    1165,  1173,  1174,  1176,  1180,  1182,  1186,  1188,  1189,  1191,
    1193,  1198,  1199,  1203,  1208,  1209,  1213,  1215,  1220,  1222,
    1224,  1226,  1228,  1230,  1232,  1234,  1236,  1241,  1242,  1264,
    1266,  1268,  1271,  1274,  1277,  1279,  1281,  1283,  1286,  1289,
    1291,  1294,  1301,  1303,  1305,  1307,  1309,  1314,  1316,  1318,
    1320,  1325,  1327,  1332,  1334,  1336,  1338,  1341,  1345,  1348,
    1352,  1354,  1356,  1358,  1360,  1362,  1364,  1366,  1368,  1370,
    1372,  1377,  1378,  1382,  1388,  1393,  1398,  1399,  1403,  1407,
    1412,  1413,  1419,  1423,  1425,  1427,  1429,  1432,  1434,  1439,
    1441,  1446,  1448,  1450,  1455,  1457,  1463,  1464,  1468,  1469,
    1470,  1471,  1475,  1480,  1481,  1483,  1485,  1487,  1491,  1495,
    1496,  1500,  1502,  1504,  1506,  1508,  1514,  1515,  1521,  1522,
    1526,  1527,  1532,  1534,  1540,  1541,  1543,  1548,  1553,  1564,
    1565,  1569,  1570,  1576,  1577,  1581,  1583,  1587,  1589,  1593,
    1594,  1598,  1599,  1603,  1604,  1605,  1609,  1611,  1626,  1627,
    1628,  1629,  1631,  1635,  1637,  1641,  1648,  1650,  1652,  1657,
    1658,  1660,  1662,  1664,  1696,  1699,  1704,  1706,  1712,  1717,
    1722,  1733,  1738,  1743,  1748,  1753,  1762,  1766,  1773,  1775,
    1776,  1777,  1783,  1785,  1790,  1791,  1792,  1801,  1802,  1803,
    1807,  1808,  1809,  1818,  1819,  1820,  1825,  1826,  1835,  1836,
    1841,  1842,  1846,  1848,  1850,  1852,  1854,  1858,  1863,  1864,
    1866,  1876,  1877,  1882,  1884,  1886,  1888,  1890,  1893,  1895,
    1897,  1902,  1904,  1906,  1908,  1910,  1912,  1914,  1916,  1918,
    1920,  1922,  1924,  1926,  1928,  1930,  1932,  1934,  1936,  1938,
    1940,  1942,  1944,  1946,  1948,  1950,  1952,  1954,  1956,  1961,
    1962,  1966,  1973,  1974,  1980,  1981,  1983,  1985,  1987,  1992,
    1994,  1999,  2000,  2002,  2004,  2009,  2011,  2013,  2015,  2017,
    2019,  2024,  2025,  2027,  2029,  2034,  2036,  2035,  2039,  2047,
    2048,  2050,  2052,  2057,  2058,  2060,  2065,  2066,  2068,  2070,
    2075,  2076,  2078,  2083,  2085,  2087,  2089,  2090,  2092,  2097,
    2099,  2101,  2106,  2107,  2111,  2112,  2117,  2116,  2121,  2120,
    2128,  2127,  2138,  2137,  2147,  2152,  2153,  2158,  2164,  2178,
    2179,  2183,  2185,  2187,  2193,  2195,  2197,  2199,  2201,  2203,
    2205,  2207,  2213,  2214,  2219,  2221,  2223,  2232,  2234,  2235,
    2236,  2238,  2240,  2241,  2246,  2247,  2248,  2253,  2255,  2258,
    2265,  2266,  2267,  2273,  2278,  2280,  2286,  2287,  2293,  2294,
    2298,  2303,  2306,  2305,  2309,  2312,  2318,  2317,  2326,  2332,
    2336,  2338,  2343,  2345,  2347,  2349,  2355,  2358,  2364,  2365,
    2367,  2368,  2369,  2371,  2373,  2380,  2381,  2383,  2385,  2390,
    2391,  2397,  2398,  2400,  2401,  2406,  2407,  2408,  2410,  2418,
    2419,  2421,  2424,  2426,  2430,  2431,  2432,  2434,  2436,  2441,
    2443,  2448,  2450,  2459,  2461,  2466,  2467,  2468,  2472,  2473,
    2474,  2479,  2480,  2485,  2486,  2487,  2488,  2492,  2493,  2498,
    2499,  2500,  2501,  2502,  2516,  2517,  2522,  2523,  2529,  2531,
    2534,  2536,  2538,  2561,  2562,  2568,  2569,  2575,  2574,  2584,
    2583,  2587,  2593,  2599,  2600,  2602,  2606,  2611,  2613,  2615,
    2617,  2623,  2624,  2628,  2629,  2634,  2636,  2643,  2645,  2646,
    2648,  2653,  2655,  2657,  2662,  2664,  2669,  2674,  2682,  2684,
    2689,  2690,  2695,  2696,  2700,  2701,  2702,  2707,  2709,  2715,
    2717,  2722,  2724,  2730,  2731,  2735,  2739,  2743,  2745,  2746,
    2747,  2752,  2755,  2754,  2766,  2765,  2777,  2776,  2788,  2787,
    2799,  2798,  2812,  2818,  2820,  2826,  2827,  2832,  2839,  2844,
    2850,  2853,  2856,  2860,  2866,  2869,  2872,  2877,  2878,  2879,
    2883,  2889,  2890,  2900,  2901,  2905,  2906,  2911,  2916,  2917,
    2923,  2924,  2926,  2931,  2932,  2933,  2934,  2935,  2937,  2972,
    2974,  2979,  2981,  2982,  2984,  2989,  2991,  2993,  2995,  3000,
    3002,  3004,  3006,  3008,  3010,  3012,  3017,  3019,  3021,  3023,
    3032,  3034,  3035,  3040,  3042,  3044,  3046,  3048,  3053,  3055,
    3057,  3059,  3064,  3066,  3068,  3070,  3072,  3074,  3086,  3087,
    3088,  3092,  3094,  3096,  3098,  3100,  3105,  3107,  3109,  3111,
    3116,  3118,  3120,  3122,  3124,  3126,  3141,  3146,  3151,  3153,
    3154,  3156,  3161,  3163,  3165,  3167,  3172,  3174,  3176,  3178,
    3180,  3182,  3184,  3189,  3191,  3193,  3195,  3197,  3207,  3209,
    3211,  3212,  3214,  3219,  3221,  3223,  3228,  3230,  3232,  3234,
    3239,  3241,  3243,  3257,  3259,  3261,  3262,  3264,  3269,  3271,
    3276,  3278,  3280,  3285,  3287,  3292,  3294,  3311,  3312,  3314,
    3319,  3321,  3323,  3325,  3327,  3332,  3333,  3335,  3337,  3342,
    3344,  3346,  3352,  3354,  3356,  3359,  3363,  3365,  3367,  3369,
    3403,  3404,  3406,  3408,  3413,  3415,  3417,  3419,  3421,  3426,
    3427,  3429,  3431,  3436,  3438,  3440,  3446,  3447,  3449,  3458,
    3461,  3463,  3466,  3468,  3470,  3484,  3485,  3487,  3492,  3494,
    3496,  3498,  3500,  3505,  3506,  3508,  3510,  3515,  3517,  3525,
    3526,  3527,  3532,  3533,  3538,  3540,  3542,  3544,  3546,  3548,
    3555,  3557,  3559,  3561,  3563,  3566,  3568,  3570,  3572,  3574,
    3579,  3581,  3583,  3588,  3614,  3615,  3617,  3621,  3622,  3626,
    3628,  3630,  3632,  3634,  3636,  3643,  3645,  3647,  3649,  3651,
    3653,  3658,  3660,  3662,  3669,  3671,  3689,  3691,  3696,  3697
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

#define YYPACT_NINF (-1700)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-870)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
      78, 11650,   140,   220, 15770,   128, -1700, -1700, -1700, -1700,
   -1700, -1700, -1700, -1700, -1700, -1700, -1700,   187,   830,   258,
   -1700, -1700, -1700, -1700, -1700, -1700, -1700, -1700, -1700, -1700,
   -1700, -1700, -1700, -1700, -1700, -1700, -1700, -1700, -1700, -1700,
   -1700, -1700, -1700, -1700, -1700, -1700, -1700,   112,   404, -1700,
   -1700, -1700, -1700, -1700, -1700,  4831,  4831,   286, 11650,   299,
     323, -1700, -1700,   430, -1700, -1700, -1700, -1700, -1700, -1700,
   -1700, -1700, -1700,  2433, -1700,   613,   462, -1700, -1700, -1700,
   -1700, -1700, 15620, -1700, -1700,   463,   530,   606,   205, -1700,
    4831,   530,   530,   530,   491,  5204,   693,  1029,  8812, -1700,
   -1700, -1700, 15470,  1001, -1700, -1700, -1700,  2347,   698, 12179,
    1063,   810,  2347,   846,   579, -1700, -1700, -1700, -1700,   666,
   -1700, -1700, -1700, -1700,   591, -1700, -1700, -1700, -1700, -1700,
     618,   594,   666, -1700,   666,   659, -1700, -1700, -1700, 16326,
    4831, -1700, -1700,  4831, -1700, 11650,   671, 16378, -1700, -1700,
    5238, 17390, -1700,   778,   778,   695,  3003, -1700, -1700, -1700,
   -1700,   410, 14080,  3041,   666, -1700, -1700, -1700, -1700, -1700,
   -1700,   701, -1700,   692,   729,   734, -1700,   770, 18799, 14700,
    2417,  2433,   605,   757,   762,   764,   769,   775,   783, -1700,
   -1700, 16528, 10679,   780, -1700, 15913, -1700, -1700, -1700, -1700,
     791, -1700, -1700,   799, -1700,  8973,   925, 18151, -1700,   800,
    4831,   594,   802,   813,   819,   837, -1700, -1700, -1700,  3339,
    4379,   866,   907,   200, -1700, -1700,   666,   666,    35,    49,
     250,    35, -1700,   666,   666, -1700,  4529, -1700, -1700,   889,
     892,   778, 13658, -1700, -1700, 15620, -1700, -1700,  2347, -1700,
    3024,   579,   873,   962,    49,  4831,   606, -1700, 13056, -1700,
     778,   778,   926,   962,    49,  4831, -1700,  8401, -1700, -1700,
     778, -1700,   778, -1700,   796,  3864,  4831, -1700,  1448,   943,
   -1700, -1700, -1700, 16072,   594,    99, -1700, -1700, 17440, -1700,
     907,    42, -1700, 18799, 17390,  3585,  4529, -1700,   340, -1700,
   -1700, -1700, 16378,  4831, -1700,   940, -1700, -1700, -1700, -1700,
    4831,  3178,   391,   611, -1700,  4831,   692, -1700,   808,   666,
     954, 16580,   823, 14238, 13816,  2347,  2347, -1700,  2347,   778,
    2347,   778, -1700, -1700,   666, -1700,   951, -1700, 16730, -1700,
   -1700, -1700, 16782,   791, -1700,   969,    32,  1684,   995,   579,
    1006, -1700,  3003,   976,   692,  3003,  2639, -1700,  1018,  1057,
   18871,  1045,  1047, 18799, 18943,  1052,  7774, -1700, -1700, -1700,
   -1700, -1700, -1700, 19015, 19015, 14546,  1048,  3449, -1700, -1700,
   -1700, -1700,  1061, -1700,  1140, -1700,  1380, -1700, 18799, 18799,
   -1700,  1067,   566,   803,   857,   578,   953,  1070,  1087,  1077,
    1138,    16, -1700,   646, -1700,  1167, -1700,  1026,  4997, 15008,
   -1700, -1700,   616,  1167, -1700, -1700,   660, -1700, -1700,  2417,
    1171,  1174,  1176,  1178,  1183,  1185, -1700, -1700,   346,  1188,
   -1700,   715,  1188, -1700, -1700, 16326, -1700,  1056,  1199, 15162,
   -1700, -1700,  4511,  4805,  1239, 14238,  1241,   726,   923, -1700,
   -1700, -1700, -1700, -1700,  4831,  4698, -1700, -1700, -1700, -1700,
   -1700, -1700,  5666,  3484,  1048,  8973,  1218,  1220, -1700, -1700,
    1223, 18151,   632, -1700, -1700, -1700, 18223,  1235, -1700, -1700,
   -1700, -1700, -1700,  3339,   794,  1240,  1247,  1255,   848,  1258,
    1261,  1262,  4379, -1700, -1700,   666,  1237,   606,  1253, -1700,
   -1700,  1263, -1700, -1700,   594,   962, -1700, -1700, -1700,   594,
   -1700, -1700,  4529, -1700, 15008, 15008, -1700,   778,  5238,  6168,
   14080, -1700, -1700, -1700, -1700, -1700,   594,   962,    42, -1700,
   -1700,  2347,  1254,   962,    49, -1700,   594,   962, -1700, 12948,
   -1700,   778,   778, -1700, -1700,  1266,   580,  1267,   579,  1271,
   -1700, 17598, -1700,   740, -1700,  1349, 18064, -1700,  5238, 16941,
   13658, -1700, 16072, 19087, -1700, -1700, -1700, -1700, -1700,  3585,
     893,  4529, -1700, 14080,   907, 11650, -1700,  1265, -1700,  1281,
   -1700, -1700, -1700, -1700, -1700,  3003, -1700, -1700,  1355,  4175,
   16782, 10679, -1700, 16993, -1700,   778,   778, -1700, -1700,   791,
   -1700,   681,  1282,  1418, 18799,  1177,  1263,  1272, -1700,   666,
     666, -1700,  1188, -1700, 16580, -1700, -1700, 17879,   778,   778,
   -1700,  4175,   666, -1700, 17247, -1700, -1700, 16730, -1700,   410,
    1289,   321,  1291,  1684,   759, 16378,   776, -1700, -1700, -1700,
   -1700, -1700, -1700,   816, -1700,  1297,  1275, -1700, 14854, -1700,
   17045, 17045, -1700, 14854, -1700, 18799, -1700, 12179, 12179, 14854,
   -1700, -1700, 16124, 17045, 17045,  1026,  1511,  1848,   609,  1944,
   -1700,   831,  1301,  1065,  1302, -1700, 18223, 18799, 18295,  1298,
    1448,  1448, -1700,  1249, -1700, -1700, 18367,  2660, 18799, 18367,
    1448, -1700, -1700, 18799, 18799, 18799, 18799, 18799, 18799, 18799,
   18799, 18799, 18799, 18799, 18799, 18799, 18799, 18799, 18799, 18799,
   18799, 18799, 18439,  1285,   770,  3788, 10679, -1700, -1700, -1700,
   -1700, -1700, -1700, -1700, -1700, -1700, -1700, -1700,  1303, 18799,
   -1700, -1700,   616,  2018, -1700, -1700,   666,   666, -1700, -1700,
   15008, -1700,   381,  1188, -1700,   723,  1188, -1700, -1700, -1700,
    1263, -1700, -1700,  1263, 19159, -1700, -1700, 10679,  1308,  1309,
    1530,  1449,  2686,   389,  1272, -1700,   666,   666,  1272,   433,
   -1700,   666,   666, 18799,  4831,  1068,  1075,  1272,   -99, 13606,
   13606,  4831, -1700, -1700, 18799,  1223, -1700,  8973,  1319, -1700,
    2123, -1700, -1700, -1700, -1700, -1700,   834, -1700, 13606,  1448,
    5238,  1448,   779,  1317,  1320,  1321,   842,  1323,  1324,  1325,
     473,  1188, -1700, -1700,   493,  1188, -1700, -1700, -1700,  5238,
     770, -1700,  1188, 19159, -1700,   594, 17598, -1700, -1700,   881,
    1326,   882,  1328, -1700,  1332, -1700,   594, -1700, -1700,   594,
     962,  1332, -1700,   594,  1327,  1329,  1331, -1700, -1700, 17879,
   -1700,  1334, -1700, -1700, -1700,  1448,  4831,  9838,  1415,  1315,
   17966, -1700,  1199, -1700, 13606,   894, -1700, -1700,  1332, -1700,
   16378, 15008,  1322, -1700,  1322, -1700, -1700, -1700, -1700, 16730,
   -1700, 10841, 15316, -1700, 17598,  1339,  1342,  1345, -1700,  7515,
     666, -1700,  1177, -1700, -1700, -1700, -1700,  1263, -1700, -1700,
   -1700,   778, -1700,  3416, -1700, -1700,   579,  2204,  1356, -1700,
   18151, -1700,  1684,  1289, -1700, -1700,  1346,  1357,  2639, 18367,
   -1700,  1358,   462,  1351,  1359,  1363,  1352,  1364, 18799,  1366,
    1367,  1368, 10679, 18799, -1700, -1700,  2225, -1700, -1700, -1700,
   18799, -1700,  1370,  1384,  9251,  1088, -1700, 18367, -1700, -1700,
   -1700,  2047, -1700, -1700,   948, -1700, -1700, -1700, -1700,  2047,
   -1700, -1700,  1090,   602, -1700, -1700,  1067,  1067,  1067,   566,
     566,   803,   803,   857,   857,   857,   857,   578,   578,   953,
    1070,  1087,  1077,  1138, 18799,  1095, -1700,  1385,  2047, -1700,
   -1700,  8973, -1700, 17598,  1388,  1389,  1395,  2018, -1700, -1700,
   -1700, -1700, -1700, -1700, -1700, -1700,  1263, -1700, -1700,  1263,
   17598, 17598, -1700, -1700,  1530,   947,  1397,  1399,  1400,  1401,
    2532,  2686, -1700, -1700, -1700, -1700, -1700, -1700, -1700, -1700,
   -1700, -1700, -1700, -1700, -1700, -1700,  1377, -1700,  1272, -1700,
   -1700, -1700, -1700, -1700, -1700, -1700, -1700,  1410,  1412, -1700,
     606,  2047,  1100,   371, -1700, -1700,  1416, -1700, 18151, -1700,
   18799, -1700, 18511, 13606, -1700, -1700, -1700,  1392,   496,  1188,
   -1700,   510,  1188, -1700, -1700, -1700, -1700,  1263, -1700, -1700,
   -1700,  1263,   907,  1417,  1263, -1700, -1700, -1700, -1700, -1700,
   -1700, -1700,  1422, -1700, -1700,  1332, -1700,   594, -1700, -1700,
   -1700, -1700, -1700, 12280,  1420,  1419, -1700,   209, -1700,   541,
     450, 10517,  1423, 13435,  1424,  1428,  2448,  2715,  2217, 18583,
    1429, -1700, -1700,  1432,  1434, -1700, -1700,   594, 18799, 18799,
    1571,  1433,   658, -1700,  1519,  1438,  1425, -1700, -1700, -1700,
    9666, -1700, -1700, -1700, -1700, -1700,  3026, -1700, -1700, -1700,
    1505, -1700, -1700, -1700,  1448, -1700, -1700, 12127, 15620,  1440,
   -1700,  4831, -1700,  1427,  1443,  1451, -1700,  1103, -1700, -1700,
   -1700, -1700,  5238, -1700, -1700,  1435,  1436,   956, 16378,   692,
     692, -1700, -1700,  1048,  1199, 15162, -1700,  1167, -1700, 11003,
   -1700,   515,  1188, -1700,   778,  8474, -1700, -1700,  1684,   666,
     666,   410,   321, -1700, -1700,  1289,  1455,  1458, -1700, -1700,
     958,   625, 10679,  1448, -1700,   625, 16176,   625, -1700, 18799,
   18799, 18799, -1700, -1700, -1700, -1700, 18799, 18799,  1459,  8973,
   -1700, -1700,  1453,   672, -1700,  3620, -1700, -1700,  1105, -1700,
      13, -1700, 18367,  1109, -1700, 18223, -1700, -1700, 18799,  1444,
    1117,  1125,  1223, -1700,   521,  1188, -1700, -1700, 17598, 17598,
   -1700, -1700,  1471,   524,  1188, -1700,   549,  1912,   666,   666,
   -1700, -1700, 17598, 17598, -1700,  1469, -1700, 14080, 14080,  1473,
    1470,  1472,  1478, -1700,  1475, 18799, 18799,  1130,  1479, -1700,
   -1700, -1700, -1700, -1700, -1700,  1481, 18799, -1700, -1700, -1700,
    1263, -1700, -1700, -1700,  1263, 17598, 17598,   606,   666,  1133,
    1493,  1497, -1700, -1700,  1500, 12433, 12586, 12739, 16378, 17045,
   17045,  1501, -1700,  1482,  1487,  2285,  4988, -1700,   357,  4831,
   -1700, -1700,  4831, -1700, 18079,   313,   366, -1700, -1700, -1700,
   -1700, 18799,  1504,  1584, 10354, 10010, -1700,  1489, -1700,  1494,
   18799,  1498,  8973,  1499, 18799, 18223, 18799,   977, -1700,  1503,
     234, -1700,    53,  1522, -1700, -1700,  1526, -1700,  1507, -1700,
    1514,  1536, 13435,   674, 13214,   666,   361, -1700, -1700, -1700,
    1524, -1700,  1541, -1700,  1544, -1700,  1540, -1700,  1542, -1700,
   -1700, -1700, -1700, 11165,  1545,  1547,  1548, -1700,  1553, -1700,
   -1700, -1700,  1263, 18799, 18799,  1199,  1555, -1700,  1289, -1700,
    1539,   721, -1700,  1559, -1700, -1700, 16378, -1700,  1560,  1558,
     959, -1700,  1564, -1700, -1700, -1700, -1700, -1700,  8973,  1223,
   18223, -1700,  1580,  2047, -1700,  1580,  1580, -1700,  2047,  3727,
    3886, -1700, -1700,  1137, -1700, -1700, -1700,  1567,  1565, -1700,
   -1700, -1700,  1263, -1700, -1700,  1572,  1576,   666, -1700, -1700,
   -1700,  1263, -1700, -1700, -1700,  1577, -1700, -1700, -1700, -1700,
   -1700, -1700, -1700, -1700, -1700, -1700, -1700, -1700, -1700,  1575,
   -1700, -1700, -1700, -1700,  1578,  1582,   666, -1700, 17598, 17598,
   -1700, -1700, -1700, -1700, 18799, -1700, -1700,  1583, -1700,  1501,
    1501,  1501,   902,  1561,   374, -1700,  4261,   470, 15008, -1700,
   -1700, -1700,  4117, 18799,  4672,   474, -1700, -1700,   277,  1581,
    1581,  4831, -1700, -1700, 17747, -1700, 18799,  1586,  1585, -1700,
   -1700, -1700, -1700,   973,  1593, 13435,  1438,  1592, 18799,   463,
    1590,   491, 12898, 16378, 13435, 18799, 18799,  1062,   228, -1700,
   18799, -1700, -1700,   479, -1700,  1223, -1700,   980,   981,   986,
   -1700, -1700, -1700, -1700,   594,   977,  1597, -1700, -1700, 18799,
   -1700,  1598,   770, 10517, -1700, -1700, -1700, -1700, 18799,  1641,
   -1700,  9494, -1700,   666, 14080, -1700, -1700, 16378, -1700, -1700,
   -1700, -1700, -1700,  1595, -1700, 17598, -1700, -1700,  1600, -1700,
    1601,  1604,  1599,  1684, -1700, -1700, -1700, -1700, 18799, -1700,
   16176, 18799,  1223,  1615,  1139, -1700,  1142, -1700,  2047, -1700,
    2047, -1700, -1700, -1700, -1700, 17598,  1613,  1616, -1700, -1700,
   17598, 17598,  1619,  1625,  1146, 13764, 13922, -1700,  1612, -1700,
   -1700, -1700, -1700,  1626,  1629,  1150, -1700, -1700, -1700, -1700,
     902,  2059,   488, -1700, -1700, -1700, -1700,   666,   666, -1700,
   -1700, -1700,   497, -1700,   989,  4117,   685, -1700,  4672,   666,
   -1700, -1700, -1700, -1700, -1700, -1700, -1700, -1700,   520, 13435,
     316, 18655, -1700, 13435,  1438, 14396, -1700,  1438,  1607, -1700,
   -1700, -1700, -1700,  8560, 18799, 13435, 10182,  1608, -1700,  1652,
     552, 13435, -1700, -1700,  1653, -1700, -1700,  1632,   770,   325,
    1656,  1657,  1152,  1715, -1700, -1700, -1700, -1700,  4831,  5238,
   -1700, -1700,  1658,  1661, -1700, -1700, -1700,  1684,  1289,  1668,
   -1700, -1700, -1700,  1672, -1700, -1700, -1700,  1155,  1160, -1700,
   -1700, -1700, -1700, -1700, -1700, -1700, -1700, -1700, -1700,  1671,
   -1700, -1700,  1673,  1674, -1700, -1700, -1700,  1676,  1677,  1678,
    2059, -1700,   666, -1700, -1700, -1700, -1700, -1700,  1679,  4261,
   -1700, -1700,  7934,   150, 11330, -1700, 13317, -1700,  1659,   991,
    1758, 18799,  1682, 18799,  1260,  1664,   440,  1762, -1700, 18799,
    1670, 11491, -1700, -1700, -1700, 17195, -1700,  1697,  1680,    10,
   13435, -1700, 18799, 18367,   610, -1700, -1700, -1700,  1705, -1700,
   -1700,  1289,  1719, -1700, -1700, -1700, -1700,  1717,  1718,  1720,
   14080,  1721, -1700, -1700,   570,  1188, -1700, -1700,   902, -1700,
   -1700,   447, -1700,    58, -1700, -1700, -1700,  1726, 11809, -1700,
   -1700, -1700,    72, 13435, -1700,  1438,  1725,  1728, 18799, 18799,
   18799, 13435, -1700, -1700,  1731, 11809, 17195, -1700,  4601, 16993,
    1448,  1727, -1700,  1780,  1733,   334,  1729, -1700,  1812, -1700,
     992, 13435,  1740, 13435, 13435, -1700,  1742, -1700, -1700, -1700,
   -1700, -1700, -1700, -1700, -1700,  1263, -1700, 18799, -1700, 18799,
   -1700, -1700,  1257, 11968, -1700, 13435, -1700, -1700,  1730,  1732,
     550, -1700,  1438, -1700, -1700,  1257, -1700,  1722,  3144,  2868,
   -1700, -1700, -1700,    10,  1737, 18799,  1735,    10,    10, 13435,
   -1700, -1700, 18799,  1785,  1793, -1700, 17598, -1700, -1700, 13317,
   -1700,  1257, -1700, -1700, 18799, 18727, 18799, -1700,  1722, 18799,
    1748,  2868,  1745,   770,  1751, -1700,   398, -1700, -1700,   999,
    1715,   293, -1700, -1700,  9179,  1756, 13317,  1438, -1700,  1438,
    1438,  1763,  1755, -1700,   594,   770,  1764, -1700,  1738,   770,
   -1700, -1700, 13435,  1842,  1765, -1700, -1700, -1700,  9375, -1700,
     594, -1700, -1700,  1184, 18799, -1700,  1007, -1700, 13435, -1700,
   -1700,   770,  1448,  1770,  1749, -1700, -1700, -1700,  1010, -1700,
   -1700,  1750,  1448, -1700, -1700
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,   416,     0,     2,   416,   433,   434,   435,   436,   437,
     438,   439,   440,   422,   424,   423,   425,     0,     0,     0,
     441,   443,   464,   444,   465,   447,   448,   462,   463,   442,
     460,   461,   445,   446,   449,   450,   451,   452,   453,   454,
     455,   456,   457,   458,   459,   466,   467,   753,   469,   542,
     543,   546,   548,   544,   550,     0,     0,     0,   416,     0,
       0,    16,   513,   519,     9,    10,    11,    12,    13,    14,
      15,   717,    95,     0,    19,     0,     2,    93,    94,    17,
      18,   769,   416,   718,   365,     0,   368,   643,   370,   379,
       0,   369,   399,   400,     0,     0,     0,     0,   496,   418,
     420,   426,   416,   428,   431,   481,   468,   404,   474,   479,
     405,   491,   406,   506,   510,   516,   495,   522,   534,   753,
     539,   540,   523,   589,   371,   372,     3,   719,   732,   421,
       0,     0,   753,   791,   753,     2,   808,   809,   810,   416,
       0,   967,   968,     0,     1,   416,     0,   416,   388,   389,
       0,   496,   410,   411,   412,   722,     0,   545,   547,   549,
     551,     0,   416,     0,   754,   755,   541,   470,   636,   637,
     635,   696,   691,   681,     0,     0,   720,     0,     0,   416,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   514,
     517,   416,   416,     0,   969,   496,   798,   816,   973,   966,
     964,   971,   364,     0,   157,   649,   156,     0,   373,     0,
       0,     0,     0,     0,     0,     0,   363,   868,   869,     0,
       0,   398,   751,   753,   747,   772,   753,   753,   749,     2,
     753,   748,   829,   753,   753,   826,     0,   489,   490,     0,
       0,   416,   416,   433,     2,   416,   380,   419,   429,   482,
       0,   511,     0,   735,     2,     0,   643,   381,   496,   475,
     492,   507,     0,   735,     2,     0,   432,   476,   483,   484,
     493,   498,   508,   512,     0,   526,     0,   711,     2,     2,
     733,   790,   792,   416,     0,     2,     2,   977,   496,   980,
     751,   751,     3,     0,   496,     0,     0,   391,   753,   749,
     748,     2,   416,     0,   715,     0,   677,   679,   678,   680,
       0,     0,   673,     0,   663,     0,   672,   683,     0,   753,
       2,   416,   988,   417,   416,   428,   407,   474,   408,   499,
     409,   506,   503,   524,   753,   525,     0,   624,   416,   625,
     942,   943,   416,   626,   628,   513,   519,     0,   590,   591,
       0,   756,     0,   694,   682,     0,   760,    21,     0,    20,
       0,     0,     0,     0,     0,     0,    23,    25,     4,     8,
       5,     6,     7,     0,     0,   416,     2,     0,    96,    97,
      98,    99,    80,    24,    81,    38,    79,   100,     0,     0,
     115,   117,   121,   124,   127,   132,   135,   137,   139,   141,
     143,   145,   148,     0,    26,     0,   520,     2,   100,   416,
     149,   688,   639,   510,   641,   687,     0,   638,   642,     0,
       0,     0,     0,     0,     0,     0,   770,   796,   753,   806,
     814,   818,   824,     2,   975,   416,   978,     2,    93,   416,
       3,   623,     0,   988,     0,   417,   474,   499,   506,     3,
       3,   605,   609,   619,   625,   626,     2,   799,   817,   965,
       2,     2,    23,     0,     2,   649,    24,     0,   647,   650,
     986,     0,     0,   656,   645,   644,     0,     0,   737,     2,
       2,     2,     2,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   775,   832,   753,     0,   643,     2,   771,
     779,   895,   773,   774,     0,   735,     2,   828,   836,     0,
     830,   831,     0,   394,   416,   416,   480,   417,     0,   496,
     416,   970,   974,   972,   497,   715,     0,   735,   751,   374,
     382,   430,     0,   735,     2,   715,     0,   735,   692,   477,
     478,   494,   509,   515,   518,   513,   519,   537,   538,     0,
     693,   416,   633,     0,   194,   357,   416,     3,     0,   496,
     416,   734,   416,     0,   376,     2,   377,   712,   396,     0,
       0,     0,     2,   416,   751,   416,   715,     0,     2,     0,
     676,   675,   674,   669,   427,     0,   667,   684,   472,     0,
     416,   416,   944,   417,   413,   414,   415,   948,   939,   940,
     946,     2,     2,    94,     0,   904,   918,   988,   900,   753,
     753,   909,   916,   631,   416,   504,   627,   417,   500,   501,
     505,     0,   753,   954,   417,   959,   951,   416,   956,     0,
     986,   596,     0,     0,     0,   416,     0,   768,   767,   763,
     765,   766,   764,     0,   758,   761,     0,    22,   416,    87,
     416,   416,    82,   416,    89,     0,    32,     0,    33,   416,
      85,    86,   416,   416,   416,     2,    96,    97,     0,     0,
     175,     0,     0,   540,     0,   964,     0,     0,     0,     0,
       0,     0,    48,     0,    54,    55,    59,     0,     0,    59,
       0,    83,    84,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   416,   158,   159,   160,
     161,   162,   163,   164,   165,   166,   167,   168,   156,     0,
     154,   155,     2,   880,   640,   877,   753,   753,   885,   521,
     416,   797,   753,   807,   815,   819,   825,     2,   800,   802,
     804,     2,   820,   822,     0,   976,   979,   416,     0,     0,
       2,    94,   904,   753,   988,   850,   753,   753,   988,   753,
     865,   753,   753,     3,   627,     0,     0,   988,   988,   416,
     416,     0,     2,   658,     0,   986,   655,   987,     0,   651,
       0,     2,   654,   657,   172,   171,     0,     2,   416,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     753,   784,   788,   827,   753,   841,   846,   776,   833,     0,
       0,   402,   892,     0,   738,     0,   416,   739,   395,     0,
       0,     0,     0,   393,     2,   740,     0,   378,   715,     0,
     735,     2,   741,     0,     0,     0,     0,   552,   612,   417,
       3,     3,   616,   615,   811,     0,     0,   416,   358,     0,
     496,     3,    93,     3,   416,     0,     3,   716,     2,   671,
     416,   416,   665,   664,   665,   473,   471,   590,   950,   416,
     955,   417,   416,   941,   416,     0,     0,     0,   919,     0,
     753,   989,   905,   906,   632,   902,   903,   917,   945,   949,
     947,   502,   537,     0,   953,   958,   593,   987,     0,   156,
       0,   592,     0,   986,   697,   695,     0,     0,   760,    59,
     721,     0,     2,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   416,     0,   114,   113,     0,   110,   109,    27,
       0,    28,     0,     0,     0,     0,     3,    59,    44,    45,
      52,     0,    51,    63,     0,    60,    61,    64,    47,     0,
      46,    50,     0,     0,    43,   116,   118,   119,   120,   122,
     123,   125,   126,   130,   131,   128,   129,   133,   134,   136,
     138,   140,   142,   144,     0,     0,   367,     0,     0,    29,
       3,   649,   150,   416,     0,     0,     0,   881,   882,   878,
     879,   690,   689,     2,   801,   803,   805,     2,   821,   823,
     416,   416,   897,   896,     2,     0,     0,     0,     0,     0,
     753,   905,   853,   870,     2,   848,   856,   629,   851,   852,
     630,     2,   863,   873,   866,   867,     0,     3,   988,   386,
       2,   981,     2,   620,   621,   599,     3,     3,     3,     3,
     643,     0,   148,     0,     3,     3,     0,   652,     0,   646,
       0,   736,     0,   416,     3,   390,   392,     0,   753,   785,
     789,   753,   842,   847,     2,   777,   780,   782,     2,   834,
     837,   839,   751,     0,   893,     3,   743,     3,   486,   485,
     488,   487,     2,   716,   744,     2,   742,     0,   716,   745,
     552,   552,   552,   416,     0,     0,   634,     0,   361,     0,
       0,   416,     0,     2,     0,     0,     0,     0,     0,   177,
       0,   291,   292,     0,     0,   330,   329,     0,   152,   152,
     336,   513,   519,   191,     0,   178,     0,   202,   179,   180,
     416,   196,   181,   182,   183,   184,     0,   185,   186,   297,
       0,   187,   188,   189,     0,   190,   198,   496,   416,     0,
     200,     0,   355,     0,     0,     0,     3,     0,   723,   716,
     704,   705,     0,     3,   700,     3,     3,     0,   416,   681,
     681,   952,   957,     2,    93,   416,     3,   511,     3,   417,
       3,   753,   912,   915,   416,     3,   901,   907,     0,   753,
     753,     0,   596,   581,   597,   986,     0,     2,   757,   759,
       0,    88,   416,     0,    92,    90,   416,     0,   104,     0,
       0,     0,   108,   112,   111,   176,     0,     0,     0,   649,
     101,   169,     0,     0,    77,     0,    77,    77,     0,    65,
      67,    42,     0,     0,    40,     0,    41,   147,     0,     0,
       0,     0,   986,     3,   753,   888,   891,   883,   416,   416,
       3,     3,     0,   753,   859,   862,   753,     0,   753,   753,
     854,   871,   416,   416,   982,     0,   622,   416,   416,     0,
       0,     0,     0,   375,     3,     0,     0,     0,     0,   648,
     653,     3,   174,   173,     3,     0,     0,     2,   778,   781,
     783,     2,   835,   838,   840,   416,   416,   643,   753,     0,
       0,     0,   716,   746,     0,   416,   416,   416,   416,   416,
     416,   535,   563,     3,     3,   564,   496,   553,     0,     0,
     793,     2,     0,   359,    59,     0,     0,   282,   283,   199,
     201,     0,     0,     0,   416,   416,   278,     0,   276,     0,
       0,     0,   649,     0,     0,     0,     0,     0,   153,     0,
       0,   337,     0,     0,     3,   206,     0,   197,     0,   273,
       0,     0,     2,     0,   496,   753,     0,   356,   899,   898,
       0,     2,     0,   707,     2,   702,     0,   703,     0,   685,
     666,   670,   668,   416,     0,     0,     0,     3,     0,     2,
     908,   910,   911,     0,     0,    93,     0,     3,   986,   586,
       0,   596,   594,     0,   584,   698,   416,   762,     0,     0,
       0,    34,     0,   105,   107,   106,   103,   102,   649,   986,
       0,    58,    74,     0,    68,    75,    76,    53,     0,     0,
       0,    62,    49,     0,   146,   366,    30,     0,     0,     2,
     884,   886,   887,     3,     3,     0,     0,   753,     2,   855,
     857,   858,     2,   872,   874,     0,   849,   864,     3,     3,
     983,     3,   607,   606,   610,   985,     2,     2,   984,     0,
       3,   750,   659,   660,     0,     0,   753,   397,   416,   416,
       3,     3,   403,   752,     0,   843,   727,     0,   729,   535,
     535,   535,   570,   540,     0,   576,   564,     0,   416,   527,
     562,   558,     0,     0,     0,     0,   565,   567,   753,   578,
     578,     0,   559,   574,   416,   362,     0,     0,    60,   286,
     287,   284,   285,     0,     0,     2,   217,     0,     0,   219,
     370,   218,   496,   416,     2,     0,   177,   252,     0,   247,
     177,   279,   277,     0,   271,   986,   280,     0,     0,     0,
     318,   319,   320,   321,     0,   311,     0,   312,   288,     0,
     289,     0,     0,   416,   208,   195,   275,   274,     0,   309,
     328,     0,   360,   753,   416,   725,   686,   416,     2,     2,
     960,   961,   962,     0,   913,   416,     3,     3,     0,   921,
       0,     0,     0,     0,   595,   583,     3,    91,     0,    31,
     416,     0,   986,     0,     0,    78,     0,    66,     0,    72,
       0,    70,    39,   151,   889,   416,     0,     0,   794,   812,
     416,   416,     0,     0,     0,   416,   416,   662,     0,   383,
     385,     3,     3,     0,     0,     0,   731,   531,   533,   529,
       0,   928,     0,   571,   933,   573,   925,   753,   753,   557,
     577,   561,     0,   560,     0,     0,     0,   580,     0,   753,
     554,   568,   579,   569,   575,   614,   618,   617,     0,     2,
       0,     0,   238,     2,   220,   496,   244,   253,     0,   268,
     269,   270,   267,   256,     0,     2,   416,     0,   272,     0,
       0,     2,   295,   322,     0,   313,     2,     0,     0,     0,
       0,   300,     0,   296,   193,   192,   384,   701,     0,     0,
     963,     3,     0,     0,   920,   922,   585,     0,   986,     2,
      37,    35,    36,     0,    56,   170,    69,     0,     0,     3,
     795,   813,     3,     3,   860,   875,   387,     2,   604,     3,
     603,   661,     0,     0,   786,   844,   894,     0,     0,     0,
     929,   930,   753,   556,   926,   927,   555,   536,     0,     0,
     207,   294,     0,     0,     0,   231,     2,   209,     0,     0,
     239,   177,   261,     0,   257,     0,   254,   245,   248,   177,
       0,     0,   212,   293,     2,   416,   290,     0,     0,   338,
       2,   298,     0,    59,     0,   310,   706,   708,     0,   923,
     924,   986,     0,   699,    57,    73,    71,     0,     0,     0,
     416,     0,   787,   845,   753,   936,   938,   931,     0,   566,
     226,   221,   224,     0,   223,   230,   229,     0,   416,   233,
     232,   241,     0,     2,   249,   258,   269,   267,     0,   177,
       0,     2,   251,   281,     0,   416,   416,     3,   323,   417,
     327,     0,   331,     0,     0,     0,   339,   340,   215,   301,
       0,     2,     0,     2,     2,   914,     0,   588,   890,   861,
     876,   608,     2,   932,   934,   935,   572,     0,   228,     0,
     227,   211,   234,   416,   351,     2,   242,   240,   263,   262,
     259,   250,   255,   246,   214,   234,     3,   316,     0,   928,
     324,   325,   326,   338,     0,     0,     0,   338,     0,     2,
     299,   306,     0,   303,   305,   587,   416,   222,   225,     2,
       3,   235,   352,   243,     0,     0,     0,     3,   316,     0,
       0,   929,     0,     0,     0,   332,     0,   341,   216,     0,
     296,     0,     3,   203,     0,     0,     2,   265,   266,   264,
     260,     0,     0,   317,     0,   344,     0,   342,     0,   344,
     302,   304,     2,     0,     0,   205,   204,   210,     0,   213,
       0,   314,   345,     0,     0,   333,     0,   307,     2,   937,
     315,     0,     0,     0,     0,   308,   346,   347,     0,   343,
     334,     0,     0,   335,   348
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1700,  5577,  5303, -1700,    -1,   122,  1256,  -139, -1700,  1549,
   -1700,   317, -1700,  -674,   598,   691,  -927,  -879, -1700,   106,
    6290,  1847, -1700,  1153, -1700,  1268,   320,   648,   649,   482,
     677,  1230,  1231,  1229,  1233,  1236, -1700,  -118,  -165,  7817,
     817, -1700,  -383, -1700, -1700,  -643,  1550, -1054,  2106, -1700,
     105, -1700,   812,    -2, -1700, -1700, -1700,   390,    81, -1700,
   -1554, -1364,   267,    67, -1700, -1700, -1700,   278,   191, -1700,
   -1700, -1700, -1700,    25, -1598,   175, -1700, -1700,    30, -1700,
   -1700, -1700,    43,   418,   419,   132, -1700, -1700, -1700, -1700,
    -743, -1700,    71,    33, -1700,   135, -1700,   -56, -1700, -1700,
   -1700,   838,  -799,  -972, -1276, -1700,    83, -1282,   777,  2996,
    -896,  -796, -1700,  -278, -1700,     8,  -128,    48,  -210,  -233,
    3580,  6596,  -633, -1700,    76,   113,   130,   730, -1700,  1956,
   -1700,   270,  3987,  -321, -1700, -1700,    55, -1700, -1700,  2083,
     307,  4330,  2751,   -57,  1757,  -149, -1700, -1700, -1700, -1700,
   -1700,  -441,   442,  4414, -1700,  -364,    98, -1700,   499,   239,
   -1700,   182,   694, -1700,   492,  -130, -1700, -1700, -1700,  5224,
    -551, -1160,  -700,  -694,  -314,  1292, -1700, -1110,  -155,  -144,
    1792,   862,  2320,  -329,  -331,  -242,  -188,  -459,  1224, -1700,
    1563,  -103,  1151,  1452, -1700, -1700, -1700, -1700,   217,  -157,
     -78,  -851, -1700,   162, -1700, -1700,   620,   453, -1700, -1700,
   -1700,  2044,  -797,  -433,  -883,   -28, -1700, -1700, -1700, -1700,
   -1700, -1700,     0,  -829,  -124, -1699,  -183,  3986,   -69,  6197,
   -1700,  1127, -1700,  2442,  -203,  -196,  -195,  -168,    21,   -72,
     -71,   -61,   626,   -44,   -20,    86,  -143,   -64,  -135,   -98,
     -90,  -741,  -713,  -696,  -677,  -721,     6,  -642, -1700, -1700,
    -698,  1316,  1318,  1336,   476,  7127,  -535,  -564,  -513,  -510,
    -673, -1700, -1370, -1640, -1583, -1581,  -587,    -8,  -166, -1700,
   -1700,   -42,   268,   -96, -1700,  7589,  1073,  -555,  -382
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,  1134,   213,   382,   383,    80,    81,   384,   359,   385,
    1420,  1421,   386,   954,   955,   956,  1238,  1239,  1240,  1432,
     408,   388,   389,   390,   668,   669,   391,   392,   393,   394,
     395,   396,   397,   398,   399,   400,   401,   410,  1053,   670,
    1359,   729,   207,   731,   404,   796,  1135,  1136,  1137,  1138,
    1139,  1140,  1141,  1954,  1142,  1143,  1364,  1537,  1832,  1833,
    1775,  1776,  1777,  1930,  1931,  1144,  1548,  1549,  1694,  1145,
    1146,  1147,  1148,  1149,  1150,  1372,  1712,  1874,  1805,  1151,
    1152,  1565,  1940,  1566,  1567,  1857,  1153,  1154,  1155,  1362,
    1865,  1866,  1867,  1983,  1998,  1892,  1893,   284,   285,   857,
     858,  1107,    83,    84,    85,    86,    87,    88,   441,    90,
      91,    92,    93,    94,   221,   558,   443,   412,   444,    97,
     294,    99,   100,   101,   324,   325,   104,   105,   166,   106,
     876,   326,   152,   109,   241,   110,   153,   250,   328,   329,
     330,   154,   405,   115,   116,   332,   117,   549,   846,   844,
     845,  1509,   333,   334,   120,   121,  1103,  1327,  1515,  1516,
    1652,  1653,  1328,  1504,  1671,  1517,   122,   632,  1602,   335,
     630,   911,  1046,   449,   450,   850,   851,   451,   452,   852,
     337,   553,  1159,   414,   415,   208,   469,   470,   471,   472,
     473,   313,  1179,   314,   874,   872,   583,   315,   353,   316,
     317,   416,   124,   172,   173,   125,  1173,  1174,  1175,  1176,
       2,  1092,  1093,   575,  1168,   126,   304,   305,   252,   262,
     532,   127,   211,   128,   222,  1055,   837,   499,   164,   129,
     643,   644,   645,   130,   224,   225,   226,   227,   299,   132,
     133,   134,   135,   136,   137,   138,   230,   300,   232,   233,
     234,   764,   765,   766,   767,   768,   235,   770,   771,   772,
     734,   735,   736,   737,   500,   139,   607,   608,   609,   610,
     611,   612,  1655,  1656,  1657,  1658,   597,   454,   340,   341,
     342,   417,   199,   141,   142,   143,   344,   788,   613
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      79,   183,   184,    79,   181,   557,   785,   336,   516,    95,
    1757,   673,   185,   403,   529,   963,   354,   493,   190,   475,
    1177,  1022,   131,   485,   486,   730,   297,   899,  1160,   186,
     176,   231,  1243,   945,   322,   998,   938,   885,   358,   497,
     198,  1023,  1412,   289,  1098,   658,   674,  1016,   453,    96,
     565,   487,   150,   187,    79,    79,   111,    79,  1156,  1888,
     402,  1250,  1540,  1540,  1017,  1353,    95,  1758,  1539,  1759,
     893,  1169,    79,  1834,   604,   908,   488,   102,  -709,   131,
    1049,    79,   913,  1018,   489,  1047,  1047,   148,   886,    79,
    1026,   887,   834,   493,    79,   436,  1033,    79,  1064,   485,
     486,    79,   841,   196,  1047,   198,    96,   564,   566,   420,
     421,   592,   513,   111,   103,  1804,   228,   292,  1019,   253,
     422,   490,   209,   263,  1284,   357,   146,   487,   623,   491,
     194,  1322,   626,  1439,   102,  1571,  -353,   423,   496,    79,
     144,   658,    79,   868,    79,  1895,   711,   183,   184,    79,
     483,  1835,   488,    95,   339,    79,   494,   260,   185,   188,
     489,   424,    79,  1863,  1166,  1440,   131,  1472,  1473,   350,
    1047,   103,   625,   505,   615,   186,   628,   170,   170,    79,
      79,   179,    57,   830,   832,   256,  -354,   287,   712,  -869,
    1834,   565,  -735,    96,    79,   194,   885,   490,   527,   187,
     111,   196,  1572,   522,   466,   491,  -353,  1323,   537,    79,
    1311,   248,   170,  1889,  1890,  1314,   457,   544,    79,    79,
    -710,   102,  1840,   183,   184,   894,   569,   893,   504,   592,
    1056,   509,   494,   266,   185,    79,   280,   267,   587,   434,
     270,   196,   272,   155,    79,  1210,  1896,   886,   910,   634,
     887,  1604,   636,   526,    79,   821,  -354,    79,   103,   866,
     522,   162,   170,   536,    79,   170,   196,   425,  1757,   140,
      57,   107,   140,  1233,    79,    79,   587,    79,   170,   533,
    1270,  1761,  1340,   789,   861,   348,  1382,   803,   804,   817,
     518,  1540,   209,   521,    79,    79,   615,  1539,  1312,  1257,
    1271,  1016,    79,  1224,   915,   188,  1836,  1324,   112,    79,
      79,  1160,  1339,   598,    79,   805,   478,   196,  1017,   921,
      57,   923,   924,   961,   925,  1758,   140,  1759,   107,  1569,
     927,    96,   170,   929,   930,   931,   156,  1018,   111,   756,
     806,  1156,  1026,  1322,  1322,  1322,    79,    57,   807,   278,
     521,    79,  1804,   498,    79,   642,  1434,  1197,  1206,   560,
     210,  1205,  -735,  1294,  1332,   112,  1972,   198,   817,  1047,
     140,   531,  1262,   803,   804,  1840,   453,   170,   266,  1695,
     878,  1124,  1027,  1333,  1696,   808,  1030,   170,   828,   561,
    1827,  1772,  1773,   809,   833,  1043,  1044,   547,   170,   506,
     552,   805,  1840,   498,   898,  1095,   190,   161,  1570,  1002,
      57,   420,   421,   140,  1540,   107,    57,   904,    79,  1323,
    1323,  1323,   422,    19,   880,   170,   806,   538,   818,  1497,
     498,   598,   170,   170,   807,   175,   248,   170,   550,   423,
     457,    79,    79,   118,   615,   322,   118,   453,   177,   769,
     885,    57,   112,    79,    79,   266,   267,   194,   619,    57,
     272,   905,    79,   424,   466,  1048,  1048,   204,   615,   170,
     279,   808,   178,  1774,   170,   615,  1798,   170,   840,   809,
     672,  1799,    79,   755,  1048,  1916,  1197,  1529,  1911,   572,
    1917,    79,  1688,   498,   909,   747,  1697,   420,   421,   498,
     118,   886,  1285,    57,   887,   825,  1616,   818,   422,  1335,
    1336,    79,  1521,   457,    62,    63,  1332,    79,  1401,  1324,
    1324,  1324,   580,   279,   118,  1472,  1473,   836,  1929,  1660,
    1003,  1522,  1252,   839,   498,  1582,  1286,   843,  1024,  1761,
    1531,  1929,   602,    57,   118,  1538,  1550,   867,  1661,  1968,
    1048,   581,   582,   107,  1969,    79,  1540,    79,   248,  1617,
    1619,  1621,    75,    57,   170,   339,    57,  1956,    79,   425,
      79,  1827,   453,  1540,    79,   987,   170,   170,  1887,   179,
      57,   118,  1031,    95,    79,    57,   602,   118,    79,   118,
     112,    57,   457,  1464,    57,   940,   131,   157,   899,  1057,
     158,   159,  1443,   160,   150,  1850,   598,   279,  1037,   824,
      96,  1540,  1286,   453,   827,   260,   191,   111,   866,    57,
      79,   118,  1074,    96,  1337,  1521,   498,  1772,  1773,  1669,
     111,   835,    79,   118,   940,   453,   453,   202,   560,  1785,
      57,   842,  1078,  1762,  1663,  1297,   498,  1408,  1670,   498,
    1413,   102,  1669,  1698,   453,   402,  1276,  1664,   544,  1301,
    1527,   266,  1763,   498,  1399,   216,  1052,   882,   602,  1181,
    1449,  1766,   248,  1458,   498,   940,  1066,   498,   146,    79,
      79,  1083,    79,  1871,   118,    -3,    79,   118,   103,    79,
    1334,  1737,   118,  1738,  1770,  1082,   236,  1447,  1462,  1371,
     615,  -410,   602,   701,   702,   940,   508,   170,   197,  1791,
    1881,   877,  1558,  1182,    79,  1936,  1872,   189,    63,  1882,
     453,   229,  1204,   498,   254,   118,   694,  1844,   264,   179,
     531,   274,   615,   695,   696,  1852,    57,  -532,    13,    14,
      15,    16,    17,   902,   118,    72,   276,   703,   704,  1048,
    1246,   279,   204,   672,   278,   170,   426,  1242,   672,    79,
     933,    79,   584,   205,   672,   732,   585,   278,   769,   498,
    1429,   934,   935,    79,  1538,  1228,    77,    78,    89,   206,
      79,   149,  1229,   672,   790,   791,   466,  1614,   792,    79,
      13,    14,    15,    16,    17,  1901,    57,   713,    79,    79,
      79,   714,   948,   949,   322,   952,   197,   179,  1283,   960,
      72,   739,   964,  -411,  -637,   740,  -353,   118,    79,   910,
    1431,   458,    13,    14,    15,    16,    17,  1242,   249,   855,
     601,   279,   107,   882,   602,    89,   426,   989,   498,   269,
    1097,    77,    78,   140,   293,   107,   197,  1390,    57,  -412,
     352,   118,  -724,  1601,    79,    79,   466,  -414,    13,    14,
      15,    16,    17,   311,   751,    95,  1247,   204,   498,   112,
    1290,   197,  1007,   274,  1613,   246,   498,   118,   355,   257,
      57,   249,   112,   356,   534,   357,   157,  1422,   738,   158,
     159,   854,   160,  1555,   909,   855,   170,  1550,   866,  1307,
     543,    63,    79,   170,   749,    96,    79,   752,   427,  1200,
     914,    79,   111,   428,   585,   429,    57,   642,  1172,   453,
     430,  1065,    89,  1067,   339,   249,   431,   916,   278,   456,
    1086,   585,   498,  1158,   432,    13,    14,    15,    16,    17,
     460,  1094,  1052,   278,  1096,   426,   474,   498,  1099,   476,
      79,   479,    72,  1170,   461,  1503,   118,   118,    79,   588,
     274,  1403,   248,  1471,   508,   697,   698,   917,   480,  1612,
     103,   918,   601,   531,   481,   898,   602,  1106,   170,   170,
     699,   700,   939,    77,   603,  1061,   940,    79,   249,  1062,
     466,   506,   482,    57,   248,   498,   604,   506,   118,   813,
    1699,   498,   118,   496,   118,   243,     6,     7,     8,     9,
      10,    11,    12,    79,   966,   967,   968,   118,   249,    79,
      79,   495,   354,   354,   249,  1199,  1269,   769,   910,   170,
     525,    72,  1088,  1090,   170,   530,   940,   940,   514,  1894,
     615,   515,   572,   572,   426,   458,   498,   498,  1383,   209,
      79,  1650,  1728,   249,  -415,   498,  1894,  1733,  1647,  1648,
    1649,  1329,    77,    78,   265,  1492,  1541,   243,     6,     7,
       8,     9,    10,    11,    12,   237,   238,   118,   239,   866,
     705,   706,   240,   535,   322,  1560,  1561,  1562,  1563,  1564,
     118,   554,   118,   118,  1932,   118,  1024,   576,   426,  1241,
     602,   118,   622,  1242,   118,   118,   118,  1389,   590,  1417,
    1609,   740,   466,  1242,  1610,    79,    79,    79,   458,    95,
    1480,  1481,  1519,  1474,  1680,   140,  -868,   107,   940,  1870,
    1444,  1700,  1701,   453,   453,   940,  1062,  1702,   140,   466,
    1767,   940,  1842,  1920,   740,    79,   940,  1242,    95,   635,
    1970,  1325,  -582,    79,   940,   201,    79,    79,  1994,    96,
      79,  2001,  1991,   633,   112,  2002,   111,   402,   402,   646,
    1942,    79,   647,  1812,  1946,   249,  1811,   598,   253,   263,
     191,   676,   118,   973,   974,   975,   976,  1158,    96,    13,
      14,    15,    16,    17,   650,   111,   651,    79,  1315,  1316,
    1317,   655,  1313,   676,   339,  1689,  1690,  1691,   738,   738,
     590,   676,    79,   260,  -401,  1338,  1158,   940,  1005,   680,
     201,  1008,  1039,  1040,   103,   942,   943,  1692,   466,  1041,
    1042,   707,  1357,   693,    79,   170,  1693,  -401,   170,   170,
     170,   256,  1231,  1062,  1244,  1245,   708,    57,   709,   249,
     940,  1248,   418,   103,  -149,  -149,  1876,  1041,  1381,  1437,
    1438,  1520,   170,  1442,  1438,   322,    79,   710,   170,   249,
     248,  1446,  1438,  1329,  1329,  1329,   552,  1505,  1329,  1013,
    1430,  1422,   508,   170,  1482,  1430,  1076,  1013,  1494,   249,
    1080,  1622,  1062,  1735,  1062,  1541,  1736,  1438,   681,   118,
    1746,  1747,   531,  1519,  1756,   940,    72,  1802,  1803,  1815,
    1438,   493,   118,   118,  1816,  1438,   485,   486,   523,   715,
     170,  1749,   741,   249,    79,   742,   601,   743,    79,   744,
     602,    79,  1772,  1773,   745,  1418,   746,    77,   603,  1991,
    1992,   433,  1435,  1436,   487,   969,   970,   249,   971,   972,
      61,   466,    89,    -3,   249,    64,    65,    66,    67,    68,
      69,    70,   950,  1325,  1325,  1325,   150,  1502,  1506,   488,
     773,   466,  -413,    79,   -17,   523,   786,   489,   787,   140,
    1676,   107,   977,   978,   797,   339,   820,   249,   269,  1672,
    1672,   810,    96,    96,   600,   533,  1391,  1392,   811,   111,
     111,   148,   951,  1689,  1846,  1691,   812,   822,   140,   814,
     107,   838,   815,   816,   490,   940,   286,   856,   112,   869,
    1543,  1543,   491,  -530,  -528,  1847,   140,   466,   847,  1474,
     871,   875,    79,  1709,  -178,   890,   888,    79,    79,    79,
    1530,  1532,  1520,  1665,   907,   604,   919,   112,   912,   920,
     494,   170,   941,   944,   170,   947,   322,   103,   103,   986,
     991,   467,  1012,  1013,  1172,   817,  1020,  1059,  1068,   803,
     804,  1069,  1070,   738,  1071,  1072,  1073,  1089,  1580,  1091,
    -713,  1474,   201,  1161,  1100,  -613,  1101,   531,  1102,  1162,
    1191,   453,   453,  1192,   170,  1178,  1193,   805,   682,  1170,
     683,   684,   685,  1207,  1203,    79,  1213,  1216,  1208,  1211,
    1214,    79,   600,    79,  1215,  1217,   652,  1219,  1220,  1221,
      79,  1226,   806,  -114,  -114,  -114,  -114,  -114,  -114,   686,
     807,  1274,   687,   688,   466,  1227,  1249,   689,   690,  1254,
    1255,   691,   692,   466,  1299,   118,  1256,  1303,  1263,    61,
    1264,  1265,  1266,   118,    64,    65,    66,    67,    68,    69,
      70,  -601,   691,  -600,  1289,  1296,  1519,   808,  1308,  1797,
    -714,  1330,  1341,  1344,  1331,   809,   339,  1345,  1354,   249,
     466,  1355,   118,  1356,  1361,  1858,   140,   418,   418,  -636,
     249,  1363,   691,   940,  1371,  1807,  1375,  1378,   260,  1365,
     118,  1377,    79,  1414,   818,  1379,  1415,  1831,  1430,  1385,
    1387,   249,   140,   140,   107,   107,  1428,    79,  1445,    79,
     118,    96,  1457,  1470,  1475,  1476,   256,  1477,   111,  1478,
    1438,    61,  1486,  1483,    89,  1172,    64,    65,    66,    67,
      68,    69,    70,   170,  1495,  1496,  1858,  1171,  1498,  1543,
    1508,   112,   112,  1334,   402,   248,  1510,   170,   118,    72,
    1864,  1511,  1534,  1551,    79,  1474,   170,    79,  1552,  1703,
    1170,  1573,  1554,  1556,  1575,  1583,   453,  1568,   466,  1014,
      74,  1576,   466,   602,   140,  1578,   103,   900,  1577,  1585,
      77,    78,  1586,   629,   466,  1588,  1603,  1589,  1615,  1590,
     466,  1591,  1592,   170,  1594,  1520,   493,  1605,   418,  1599,
     615,  1607,   485,   486,  1608,  1623,  1624,    79,    79,   783,
    1611,   467,  1927,  1628,  1831,   170,    79,  1629,   426,  1637,
    1451,  1646,  1482,  1639,  1910,  1659,  1679,  1513,   817,  1460,
     487,  1242,  1681,  1683,    96,   210,  1706,  1708,  1713,  1720,
    1944,   111,  1726,  1860,  1724,  1725,  1727,   118,   118,   118,
     118,   118,   118,  1734,  1740,   488,  1751,  1741,    79,   402,
    1744,   402,  1543,   489,  1864,   466,  1745,  1754,  1864,  1864,
    1755,  1781,  1789,   254,   264,    61,   118,   118,   168,   169,
      64,    65,    66,    67,    68,    69,    70,   402,   531,   466,
    1790,   170,  1794,  1124,  1966,   170,  1796,  1800,  1801,   103,
     490,   140,  1809,   418,  1860,  1810,  1813,   170,   491,  1993,
    1814,  1963,  -602,   170,  1822,  1823,  1982,  1824,  1825,  1826,
    1982,  1843,   498,  1841,  -513,  1851,   183,   184,  1849,   569,
     170,   140,   466,   107,  1853,   494,   965,   185,    82,   170,
     466,   147,  1996,  1861,  1862,   140,  1875,    79,   118,    79,
    -113,  -113,  -113,  -113,  -113,  -113,   402,  1877,  1878,  1879,
     466,  1880,   466,   466,  1891,  1898,  1747,   818,  1899,  1904,
     112,  1914,  1915,  1913,  1918,  1919,    96,   249,    89,  1922,
    1925,  1943,  1950,   111,   466,  1934,  1939,  1935,   170,  1964,
    1951,  1965,  1967,    96,  1977,    82,  1980,    79,    79,  1945,
     111,  1979,  1985,  1984,  1543,  1988,  1989,    89,   466,   249,
     180,  1999,   170,  2000,  2003,   671,   679,  1731,   466,    82,
     196,  1543,  1528,  1441,   246,   257,   936,   979,   981,   980,
      79,    96,   220,   982,   418,   245,  1360,   983,   111,    82,
     118,   103,  1367,   466,  1978,   466,    13,    14,    15,    16,
      17,   937,   457,  1710,   140,   170,   107,  1792,   103,  1543,
    1928,   466,  1937,   170,  1788,  1848,  1973,   466,  1654,  1873,
    1971,  1962,  1912,  1704,  1705,   118,   147,   466,  1906,  1947,
    1905,    79,    82,   170,   147,   170,   170,   296,   302,  1376,
     534,    79,  1986,   112,   167,  1662,   103,   524,  1829,   321,
    1886,  1058,  1673,    61,  1507,   118,  1373,   170,    64,    65,
      66,    67,    68,    69,    70,  1180,   409,   180,   180,   118,
      13,    14,    15,    16,    17,   793,  1606,   873,   147,   439,
    1717,   170,   245,   467,     3,  1209,   783,     0,   994,     0,
     995,   170,   118,     0,     0,     0,   249,     0,     0,     0,
       0,  1267,    74,     0,   829,   831,   220,   220,   996,  1981,
       0,    13,    14,    15,    16,    17,   170,     0,   170,     0,
       0,  1218,     0,   296,     0,  1990,  1222,     0,    57,     0,
       0,     0,    82,     0,   170,   149,     0,  1230,     0,     0,
     170,     0,     0,     0,   249,   245,   140,     0,   107,     0,
     170,     0,     0,     0,  1997,     0,     0,     0,  1087,     0,
       0,    89,    89,   140,  2004,   107,  1654,  1654,     0,    57,
       0,     0,     0,     0,     0,   302,     0,     0,   118,     0,
       0,   302,   296,   296,     0,   112,     0,    72,    61,   147,
       0,   530,     0,    64,    65,    66,    67,    68,    69,    70,
    1234,   140,   112,   107,  1235,     0,  1236,   732,   321,   605,
     614,   498,     0,     0,     0,     0,  1190,     0,    77,    78,
       0,     0,     0,     0,     0,   321,     0,     0,    72,   321,
       0,     0,     0,  1171,     0,     0,     0,    74,   671,     0,
     112,     0,     0,   671,     0,     0,     0,  1237,  1650,   671,
       0,     0,   498,  1201,     0,  1237,     0,     0,     0,    77,
      78,     0,   409,     0,    61,     0,     0,     0,   671,    64,
      65,    66,    67,    68,    69,    70,  1654,    13,    14,    15,
      16,    17,  1223,     0,  1237,     0,     0,   467,     0,     0,
       0,   418,     0,     0,     0,     0,   409,     0,   900,   733,
       0,     0,   985,     0,   652,     0,   180,     0,     0,     0,
       0,     0,   249,    74,     0,   447,   782,     0,     0,     0,
     118,     0,   147,     0,     0,  1253,   439,     0,   236,     0,
     762,     0,   614,     0,     0,     0,     0,   118,     0,     0,
    1884,     0,  1260,  1261,  1654,    61,     0,  1237,   168,   169,
      64,    65,    66,    67,    68,    69,    70,     0,    61,   246,
     257,   168,   169,    64,    65,    66,    67,    68,    69,    70,
     220,     0,     0,     0,  1654,   118,     0,     0,   691,   220,
       0,     0,     0,   853,     0,     0,     0,     0,     0,     0,
      89,   243,     6,     7,     8,     9,    10,    11,    12,   296,
       0,   409,   409,     0,  1171,   296,     0,   321,     0,     0,
       0,     0,  1423,  1424,  1425,   171,   174,  1350,     0,  1426,
    1427,     0,     0,     0,  1654,  1654,    61,     0,     0,   217,
     218,    64,    65,    66,    67,    68,    69,    70,     0,     0,
       0,     0,     0,     0,   595,   296,     0,   618,     0,     0,
     212,     0,     0,     0,    72,   249,   296,  1654,   296,     0,
     321,   595,    82,     0,     0,   595,     0,     0,     0,    13,
      14,    15,    16,    17,  1512,    74,     0,   321,   439,     0,
     614,  1513,     0,     0,     0,    77,    78,     0,   605,     0,
       0,     0,   605,     0,     0,     0,     0,     0,     0,     0,
     290,   321,   530,   291,     0,     0,     0,     0,     0,     0,
       0,   614,     0,    89,   321,     0,   312,     0,     0,     0,
       0,     0,   147,     0,     0,   467,     0,    57,     0,     0,
       0,  1237,     0,     0,     0,   409,     0,   147,   147,     0,
     409,     0,     0,    57,     0,     0,   409,     0,     0,   147,
     147,   147,     0,     0,     0,   182,     0,     0,    61,     0,
       0,     0,   595,    64,    65,    66,    67,    68,    69,    70,
     477,     0,     0,     0,    61,     0,     0,   223,     0,    64,
      65,    66,    67,    68,    69,    70,    72,     0,     0,    61,
    1453,  1454,   168,   169,    64,    65,    66,    67,    68,    69,
      70,     0,    72,   439,  1468,  1469,    73,    74,     0,     0,
       0,     0,     0,     0,     0,   528,     0,    77,    78,   733,
     733,   418,    73,    74,     0,   171,     0,   409,     0,   249,
       0,     0,   298,    77,    78,     0,   171,  1490,  1491,     0,
       0,     0,    57,   447,   439,     0,     0,   762,   467,   762,
       0,     0,     0,     0,     0,    89,     0,     0,   853,     0,
       0,     0,  1346,   574,     0,     0,   321,   321,     0,     0,
     577,   579,    89,    61,     0,   586,   217,   218,    64,    65,
      66,    67,    68,    69,    70,   321,     0,   296,     0,     0,
       0,   637,     0,     0,     0,     0,   447,     0,     0,     0,
       0,   484,   223,     0,     0,     0,   296,   631,     0,     0,
      89,     0,   312,   595,   447,   312,   853,    18,   298,     0,
       0,  1267,    74,     0,   467,     0,     0,     0,     0,  1237,
       0,     0,     0,     0,  1237,  1237,  1237,   595,    13,    14,
      15,    16,    17,     0,   409,     0,     0,     0,     0,     0,
     595,   321,     0,     0,     0,     0,     0,   147,   409,    51,
      52,    53,    54,     0,     0,   638,   321,     0,  1185,     0,
       0,  1711,     0,     0,     0,     0,     0,   570,   298,   605,
     639,     0,     0,   640,   641,    64,    65,    66,    67,    68,
      69,    70,   114,     0,     0,   114,    57,     0,     0,     0,
       0,    61,   212,     0,     0,     0,    64,    65,    66,    67,
      68,    69,    70,   958,   777,   778,     0,     0,     0,   439,
    1641,  1642,     0,     0,     0,   853,     0,    61,     0,     0,
     217,   218,    64,    65,    66,    67,    68,    69,    70,   447,
       0,     0,   853,   853,     0,     0,     0,     0,     0,   114,
       0,     0,     0,   959,     0,    72,    61,     0,     0,   168,
     169,    64,    65,    66,    67,    68,    69,    70,     0,     0,
       0,     0,     0,   114,     0,   760,    74,     0,     0,   602,
     447,     0,     0,     0,   733,     0,    77,   761,     0,   251,
       0,     0,     0,   114,     0,     0,     0,     0,     0,     0,
       0,   762,     0,     0,     0,     0,     0,     0,   762,     0,
       0,     0,     0,     0,  1237,     0,  1237,     0,     0,     0,
      13,    14,    15,    16,    17,   763,     0,  1721,     0,  1348,
     114,  1533,     0,     0,  1536,  1547,   114,     0,   114,     0,
    1553,     0,   251,     0,  1557,   312,  1559,     0,     0,     0,
     321,     0,   318,   114,   349,     0,     0,  1739,     0,     0,
       0,     0,  1742,  1743,     0,   802,     0,     0,     0,     0,
     413,     0,     0,     0,   223,     0,     0,     0,    57,     0,
       0,     0,   114,   413,     0,     0,   251,     0,     0,     0,
     147,     0,     0,   631,   298,  1869,     0,     0,   409,     0,
     298,     0,   595,     0,     0,   618,     0,     0,     0,    61,
       0,     0,   217,   218,    64,    65,    66,    67,    68,    69,
      70,     0,     0,     0,     0,     0,     0,   409,     0,     0,
       0,     0,     0,   114,     0,     0,   114,    72,     0,     0,
     298,     0,     0,     0,   245,    82,     0,     0,     0,   251,
       0,   865,     0,   298,     0,   447,     0,  1908,    74,   296,
       0,   498,     0,     0,     0,   147,   548,     0,    77,    78,
       0,     0,   439,     0,   114,     0,     0,     0,     0,   251,
       0,    18,     0,     0,  1645,   251,     0,     0,     0,     0,
     853,   853,     0,   114,     0,     0,     0,     0,     0,   439,
       0,     0,     0,   147,   853,   853,   306,   307,   308,   309,
       0,     0,   114,     0,   251,   114,  1678,     0,   193,    47,
      48,    49,    50,    51,    52,    53,    54,     0,  1684,   114,
       0,     0,     0,   114,  1038,  1687,     0,   853,   853,     0,
       0,  1050,  1368,     0,    61,     0,     0,   168,   169,    64,
      65,    66,    67,    68,    69,    70,     0,     0,     0,     0,
       0,     0,     0,  1536,   321,   321,   413,    61,     0,     0,
     168,   169,    64,    65,    66,    67,    68,    69,    70,     0,
       0,     0,    61,   193,     0,   345,   346,    64,    65,    66,
      67,    68,    69,    70,     0,     0,   310,     0,   193,     0,
     413,     0,   147,   147,   147,   147,   147,   147,     0,     0,
       0,     0,  1514,   302,   311,   193,  1108,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   114,     0,   442,     0,
     413,   409,   409,    75,     0,     0,   251,     0,   347,     0,
    1369,     0,  1015,     0,   763,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    57,     0,     0,     0,  1952,  1343,
       0,   245,     0,     0,     0,     0,     0,  1202,     0,     0,
       0,  1779,   631,     0,     0,     0,     0,     0,     0,     0,
     439,   193,   298,  1784,  1786,    61,  1547,     0,   217,   218,
      64,    65,    66,    67,    68,    69,    70,     0,     0,     0,
       0,   298,     0,   147,     0,   413,   413,     0,   595,     0,
     251,   114,     0,    72,     0,     0,     0,     0,     0,    61,
     853,   853,   168,   169,    64,    65,    66,    67,    68,    69,
      70,     0,     0,  1908,    74,   447,     0,   498,   193,     0,
       0,     0,   114,     0,    77,    78,     0,   114,     0,     0,
     251,   114,     0,   114,     0,     0,  1677,   193,     0,     0,
       0,     0,     0,     0,   114,     0,   114,     0,     0,     0,
       0,     0,     0,  1845,     0,   578,     0,     0,     0,     0,
     349,   114,   413,     0,   251,     0,     0,     0,     0,  1651,
       0,     0,     0,  1514,     0,   409,     0,     0,     0,  1514,
       0,  1514,     0,     0,     0,   114,     0,     0,   251,     0,
       0,     0,   548,     0,     0,   251,     0,     0,   114,     0,
     906,     0,     0,     0,     0,     0,   114,   853,     0,   302,
     147,     0,     0,     0,     0,     0,     0,     0,  1900,   413,
    1902,   114,   114,     0,   413,   193,     0,     0,     0,    57,
     413,     0,     0,   114,   114,   114,     0,   853,     0,     0,
     409,     0,   853,   853,     0,     0,     0,     0,     0,     0,
       0,   321,     0,     0,   147,   193,  1347,  1349,  1351,     0,
      61,     0,     0,   217,   218,    64,    65,    66,    67,    68,
      69,    70,     0,     0,     0,     0,  1015,   147,     0,     0,
       0,     0,  1268,   763,     0,     0,  1370,   413,    72,     0,
       0,     0,  1949,     0,     0,     0,   447,     0,  1579,     0,
       0,  1108,   321,   321,  1957,  1959,  1960,     0,   219,    74,
       0,   413,     0,     0,     0,     0,     0,  1651,  1651,    77,
      78,     0,     0,     0,     0,     0,     0,     0,   413,     0,
     193,   193,  1514,     0,     0,  1514,   442,    61,   631,     0,
     217,   218,    64,    65,    66,    67,    68,    69,    70,     0,
     114,   114,   302,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   409,     0,     0,     0,     0,   365,   114,
     366,     0,   367,    62,    63,    64,    65,    66,    67,    68,
      69,    70,     0,     0,     0,     0,   296,     0,     0,   193,
       0,     0,     0,  1198,     0,     0,     0,   114,     0,     0,
       0,    98,     0,     0,   151,    61,     0,   442,   189,    63,
      64,    65,    66,    67,    68,    69,    70,     0,   678,     0,
     251,    75,   376,     0,     0,     0,     0,  1651,   413,     0,
     193,   251,     0,     0,   298,   114,  1514,     0,     0,     0,
       0,   114,   413,     0,     0,     0,     0,     0,     0,     0,
     114,   193,  1187,   413,    74,   114,     0,   782,    98,     0,
       0,  1682,   147,     0,     0,     0,     0,     0,     0,  1523,
    1686,     0,  1525,     0,     0,    57,     0,     0,     0,     0,
       0,     0,   195,     0,     0,     0,     0,   321,     0,     0,
       0,     0,     0,     0,     0,  1651,     0,     0,     0,     0,
       0,     0,   258,   413,     0,   147,    61,  1715,     0,   217,
     218,    64,    65,    66,    67,    68,    69,    70,     0,     0,
       0,     0,   147,   147,     0,  1909,   302,     0,     0,  1465,
       0,     0,   442,     0,    72,     0,     0,     0,   853,   288,
       0,    61,     0,     0,     0,    98,    64,    65,    66,    67,
      68,    69,    70,  1234,   295,    74,   193,  1235,     0,  1236,
     147,     0,   323,     0,   114,    77,    78,     0,     0,     0,
       0,     0,     0,   442,     0,  1909,  1909,     0,     0,     0,
     419,   114,   114,     0,     0,     0,     0,  1518,     0,     0,
      74,   288,   445,  1433,     0,   442,   442,     0,     0,     0,
       0,     0,     0,     0,     0,  1771,     0,     0,  1909,  1780,
       0,     0,     0,     0,   442,     0,     0,     0,     0,     0,
     492,  1787,     0,     0,     0,     0,     0,  1793,     0,     0,
       0,     0,     0,     0,   114,     0,   512,     0,     0,     0,
       0,   517,   519,     0,     0,   195,     0,     0,    61,     0,
       0,     0,     0,    64,    65,    66,    67,    68,    69,    70,
    1234,  1674,     0,     0,  1235,     0,  1236,   539,     0,     0,
     541,     0,   542,     0,   114,     0,     0,     0,     0,     0,
     442,     0,   413,   559,     0,     0,     0,   193,     0,     0,
       0,     0,     0,     0,     0,     0,   571,    74,   595,     0,
    1618,     0,  1839,     0,     0,     0,     0,     0,     0,    61,
       0,   413,   543,    63,    64,    65,    66,    67,    68,    69,
      70,   593,     0,     0,   617,     0,  1868,     0,   251,   114,
       0,     0,     0,     0,     0,     0,     0,     0,   624,     0,
       0,     0,   624,   631,     0,     0,     0,     0,   193,   114,
       0,     0,     0,     0,     0,     0,   413,     0,     0,   595,
    1187,   988,     0,     0,     0,     0,   657,     0,  1518,  1897,
       0,     0,  1411,     0,  1666,     0,  1518,  1903,     0,     0,
       0,     0,     0,   413,     0,    61,     0,   114,   545,   546,
      64,    65,    66,    67,    68,    69,    70,  1921,     0,  1923,
    1924,     0,     0,     0,     0,     0,     0,    61,   108,     0,
       0,     0,    64,    65,    66,    67,    68,    69,    70,  1234,
       0,  1933,     0,  1235,     0,  1236,     0,     0,     0,   114,
     114,     0,     0,     0,     0,   288,    75,     0,     0,   593,
       0,     0,     0,   114,   114,  1948,     0,     0,   114,   114,
       0,     0,     0,   163,     0,  1953,    74,     0,  1806,  1620,
       0,     0,   657,     0,     0,   108,     0,   631,     0,     0,
       0,     0,     0,     0,     0,     0,   114,   114,     0,   442,
    1976,     0,  1953,     0,     0,     0,   114,   114,   114,   114,
     114,   114,     0,     0,     0,     0,     0,   251,  1987,     0,
       0,     0,     0,     0,  1976,     0,     0,     0,     0,   259,
       0,     0,     0,     0,  1995,   413,   413,     0,     0,     0,
     445,     0,     0,     0,     0,   275,     0,  1768,     0,     0,
    1518,     0,     0,     0,     0,     0,     0,     0,   281,     0,
     282,     0,     0,     0,     0,   251,     0,     0,     0,     0,
       0,   849,   108,     0,     0,     0,   519,     0,     0,     0,
     860,     0,   559,     0,   413,     0,     0,     0,     0,   327,
       0,     0,     0,   323,     0,    98,     0,     0,     0,     0,
       0,   298,     0,     0,     0,     0,     0,   114,     0,     0,
     624,   881,     0,     0,   193,     0,     0,     0,     0,   446,
       0,   193,     0,     0,     0,   892,     0,    57,     0,     0,
       0,     0,     0,     0,   593,     0,     0,     0,     0,   901,
       0,     0,     0,     0,     0,     0,     0,   624,   193,     0,
       0,  1518,   502,   503,     0,     0,   507,     0,    61,   510,
     511,   217,   218,    64,    65,    66,    67,    68,    69,    70,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   114,
     114,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,   540,     0,     0,     0,     0,   413,
       0,     0,     0,     0,     0,     0,  1512,    74,     0,     0,
     108,     0,     0,   442,   442,   114,    61,    77,    78,    62,
      63,    64,    65,    66,    67,    68,    69,    70,     0,     0,
       0,     0,     0,   251,   114,     0,   445,     0,     0,     0,
     298,     0,     0,     0,     0,   589,     0,     0,   594,     0,
       0,   259,     0,   997,     0,     0,     0,     0,     0,     0,
     621,     0,     0,     0,   413,   594,     0,    75,     0,   594,
       0,   113,     0,     0,     0,   114,     0,   881,   114,     0,
       0,     0,  1021,     0,     0,     0,   114,     0,     0,     0,
     570,   298,     0,     0,     0,     0,     0,     0,     0,   445,
     445,   114,    61,     0,     0,   217,   218,    64,    65,    66,
      67,    68,    69,    70,     0,     0,   114,     0,   445,     0,
       0,   114,   114,   298,     0,     0,   114,   114,   113,   193,
      72,    13,    14,    15,    16,    17,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   849,     0,     0,     0,
    1512,    74,     0,     0,   748,   119,     0,  1513,   119,     0,
       0,    77,    78,     0,     0,     0,   594,     0,     0,     0,
       0,     0,   261,     0,     0,     0,   251,  1157,     0,     0,
       0,     0,     0,     0,   445,     0,     0,   413,     0,    57,
     151,     0,     0,     0,     0,     0,     0,     0,     0,   624,
       0,     0,  1189,     0,   849,     0,     0,     0,     0,  1195,
       0,     0,   119,     0,     0,   113,     0,     0,     0,     0,
      61,   819,     0,   217,   218,    64,    65,    66,    67,    68,
      69,    70,   331,     0,     0,     0,   119,     0,     0,     0,
       0,     0,     0,     0,   193,     0,     0,   446,    72,     0,
       0,     0,   323,     0,     0,     0,   119,     0,     0,     0,
       0,     0,   448,     0,     0,     0,     0,     0,   219,    74,
       0,     0,     0,     0,     0,     0,     0,     0,   327,    77,
      78,    13,    14,    15,    16,    17,   114,   259,     0,   108,
       0,     0,     0,   119,     0,     0,     0,     0,     0,   119,
     446,   119,   108,     0,     0,     0,     0,     0,     0,     0,
       0,   114,     0,   849,     0,     0,     0,   594,   446,     0,
     193,     0,     0,     0,     0,     0,     0,     0,     0,   114,
     849,   849,     0,   119,     0,   895,   896,     0,     0,    57,
       0,   594,     0,     0,     0,   119,   114,   114,   903,     0,
     251,     0,    61,   113,   594,   168,   169,    64,    65,    66,
      67,    68,    69,    70,     0,     0,     0,     0,     0,     0,
      61,   442,   442,   217,   218,    64,    65,    66,    67,    68,
      69,    70,     0,   445,   114,     0,     0,     0,     0,     0,
       0,   596,     0,     0,   261,     0,   119,     0,    72,   119,
     456,     0,     0,     0,   119,     0,     0,     0,   596,     0,
       0,     0,   596,     0,     0,     0,     0,   114,   295,    74,
       0,     0,     0,  1326,    13,    14,    15,    16,    17,    77,
      78,  1157,     0,     0,     0,     0,     0,   119,     0,     0,
       0,     0,    61,   446,     0,   217,   218,    64,    65,    66,
      67,    68,    69,    70,     0,     0,   119,     0,     0,     0,
    1157,     0,   999,  1000,     0,     0,     0,     0,  1004,     0,
      72,     0,     0,     0,     0,     0,     0,     0,  1374,     0,
       0,     0,    57,     0,   446,     0,     0,     0,     0,  1025,
    1908,    74,  1028,  1029,   498,  1032,     0,  1034,  1035,     0,
       0,    77,    78,     0,     0,   593,   327,   327,     0,   596,
       0,     0,     0,    61,   517,     0,   217,   218,    64,    65,
      66,    67,    68,    69,    70,   327,     0,     0,     0,   119,
       0,     0,   323,     0,     0,     0,  1075,     0,     0,    61,
    1079,    72,   168,   169,    64,    65,    66,    67,    68,    69,
      70,     0,     0,   327,     0,     0,   442,     0,     0,     0,
       0,  1512,    74,   119,     0,     0,     0,     0,     0,     0,
       0,     0,    77,    78,     0,     0,     0,     0,   849,   849,
       0,     0,     0,     0,   108,     0,     0,   460,     0,   119,
     448,   327,   849,   849,     0,     0,     0,   445,   445,     0,
       0,     0,     0,     0,     0,     0,   594,     0,     0,   259,
       0,   327,     0,     0,     0,     0,  1196,     0,     0,     0,
       0,   331,     0,     0,     0,   849,   849,     0,     0,     0,
     261,     0,   113,     0,     0,  1326,  1326,  1326,   151,     0,
       0,     0,     0,   448,     0,   113,    61,     0,     0,   217,
     218,    64,    65,    66,    67,    68,    69,    70,     0,   446,
     596,   448,     0,     0,  1542,  1542,     0,     0,   119,   119,
       0,     0,    61,     0,    72,   168,   169,    64,    65,    66,
      67,    68,    69,    70,   596,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   760,    74,     0,   596,   602,     0,
       0,     0,     0,     0,     0,    77,   761,     0,     0,     0,
     119,     0,     0,   323,   119,     0,   119,     0,   604,     0,
     327,     0,     0,     0,     0,     0,     0,     0,     0,   119,
       0,   242,     0,     0,     0,     0,   151,   327,   327,     0,
      13,    14,    15,    16,    17,     0,  1196,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,  -417,  -417,     0,  -417,    45,    46,
       0,  -417,     0,     0,     0,     0,   448,     0,     0,   119,
     327,     0,     0,     0,  1298,     0,     0,  1302,    57,     0,
       0,     0,   119,     0,   119,   119,     0,   119,   849,   849,
       0,     0,     0,   119,     0,     0,   119,   119,   119,     0,
       0,     0,     0,     0,     0,     0,     0,   448,     0,     0,
       0,     0,    62,    63,  1668,     0,     0,     0,   108,     0,
       0,     0,     0,     0,   849,     0,     0,     0,     0,   331,
     331,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,  1685,     0,     0,     0,   108,   331,   717,
     718,   719,   720,   721,   722,   723,   724,   725,   726,   727,
      75,   301,     0,   204,     0,   259,     0,     0,    77,    78,
       0,     0,     0,  1542,   119,     0,   331,     0,     0,     0,
       0,     0,     0,     0,   323,     0,     0,   151,     0,     0,
     728,     0,   594,     0,     0,   849,     0,  1400,     0,     0,
       0,     0,     0,     0,     0,  1409,  1410,   113,     0,     0,
       0,     0,     0,     0,   331,     0,     0,     0,     0,   446,
       0,     0,     0,     0,     0,   849,     0,     0,     0,   596,
     849,   849,   261,     0,   331,   445,   445,     0,     0,     0,
       0,     0,     0,     0,     0,   123,     0,     0,   123,     0,
       0,  1760,     0,     0,     0,     0,     0,     0,     0,     0,
    1450,     0,     0,     0,     0,   327,   327,     0,     0,  1459,
       0,     0,  1463,     0,  1466,  1467,     0,     0,     0,   327,
     327,     0,   448,     0,   327,   327,     0,     0,     0,     0,
       0,   119,     0,     0,     0,     0,  1542,     0,     0,     0,
       0,     0,   123,     0,   119,   119,     0,     0,     0,     0,
       0,     0,   327,   327,  1493,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    61,   123,     0,   217,   218,
      64,    65,    66,    67,    68,    69,    70,     0,     0,     0,
       0,     0,     0,   331,     0,     0,   123,     0,     0,     0,
       0,   108,   108,    72,     0,     0,     0,     0,     0,    61,
     331,   331,   217,   218,    64,    65,    66,    67,    68,    69,
      70,     0,     0,   219,    74,     0,     0,     0,     0,     0,
       0,  1581,     0,   123,    77,    78,     0,    72,     0,   123,
       0,   123,     0,     0,     0,  1859,     0,     0,     0,     0,
     446,     0,     0,     0,     0,     0,     0,   295,    74,   203,
       0,     0,     0,   331,     0,   214,   215,     0,    77,    78,
     445,     0,     0,   123,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   123,     0,     0,  1542,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   277,
       0,     0,     0,     0,     0,  1542,  1859,     0,     0,     0,
       0,   113,     0,  1463,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   123,     0,     0,   123,
     113,     0,  1640,  1542,   123,   327,   327,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   261,  1941,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   849,   123,     0,     0,
       0,   327,     0,     0,     0,   596,     0,   119,     0,     0,
       0,     0,     0,     0,     0,   119,   123,     0,     0,     0,
     259,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   448,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   119,     0,     0,     0,     0,     0,
     108,     0,     0,     0,     0,     0,     0,     0,     0,  1716,
       0,   327,   119,     0,     0,     0,     0,     1,     0,     0,
     145,     0,   327,     0,     0,     0,     0,     0,   331,   331,
       0,     0,   119,     0,     0,   567,     0,     0,     0,   123,
       0,     0,   331,   331,     0,     0,     0,   331,   331,     0,
       0,     0,   327,     0,     0,     0,     0,   327,   327,     0,
       0,     0,   327,   327,     0,     0,     0,     0,     0,     0,
     119,     0,     0,   123,     0,   331,   331,     0,     0,     0,
       0,     0,     0,  1764,  1765,     0,     0,     0,     0,     0,
       0,     0,     0,   192,     0,  1769,     0,     0,     0,   123,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   113,   113,     0,     0,    13,    14,
      15,    16,    17,   108,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,   283,     0,     0,     0,    45,    46,     0,     0,
       0,     0,     0,   448,     0,     0,     0,     0,     0,   119,
     119,   119,   119,   119,   119,     0,    57,     0,   123,   123,
       0,   758,     0,   759,     0,     0,     0,     0,  1828,     0,
       0,     0,   775,   776,     0,     0,     0,     0,   119,   119,
       0,     0,     0,     0,     0,     0,     0,   656,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     123,     0,   594,     0,   123,     0,   123,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   123,
       0,     0,     0,     0,     0,     0,   283,   327,     0,     0,
    1883,     0,     0,     0,     0,     0,     0,     0,   331,   331,
       0,   520,   -16,     0,     0,   108,     0,     0,     0,     0,
     119,   283,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   283,   108,   594,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   331,   551,   555,     0,     0,   123,
     859,     0,   562,   563,     0,     0,     0,     0,     0,     0,
       0,     0,   123,   261,   123,   123,     0,   123,   573,     0,
     108,     0,     0,   123,     0,     0,   123,   123,   123,     0,
       0,     0,     0,     0,     0,     0,     0,   591,     0,     0,
       0,     0,     0,   113,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   327,   331,     0,     0,     0,     0,     0,
       0,     0,   119,     0,     0,   331,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   677,     0,   331,     0,   119,     0,     0,
     331,   331,     0,     0,   123,   331,   331,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   716,     0,     0,   119,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   119,     0,     0,     0,     0,     0,     0,     0,     0,
     754,     0,     0,     0,   757,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   119,     0,   113,     0,     0,     0,
       0,     0,     0,   779,     0,     0,     0,   780,   781,     0,
       0,   784,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   798,   799,   800,   801,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   823,  1036,     0,     0,     0,
       0,   123,     0,   826,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   123,   123,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     119,   283,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   596,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   864,     0,     0,     0,     0,     0,     0,   551,
     331,     0,     0,  1104,  1105,   870,     0,     0,     0,     0,
       0,     0,     0,     0,  1163,  1164,  1165,     0,   113,  1167,
       0,     0,     0,     0,     0,     0,     0,     0,   884,   889,
      13,    14,    15,    16,    17,   113,   596,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,  -417,  -417,     0,  -417,    45,    46,
       0,  -417,     0,   113,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    57,     0,
       0,     0,   932,     0,   165,     0,     0,     0,     0,  1232,
       0,     0,   119,     0,     0,     0,   331,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   119,
     165,     0,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1251,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   119,     0,   993,
       0,     0,     0,     0,     0,     0,   165,     0,     0,     0,
      75,     0,     0,     0,  1010,     0,     0,   123,  1011,   165,
       0,   165,     0,     0,     0,   123,     0,   884,     0,     0,
    1275,     0,     0,     0,     0,     0,     0,     0,     0,  1279,
    1280,  1281,  1282,     0,     0,     0,     0,  1287,  1288,  1051,
       0,   351,     0,     0,   123,     0,     0,  1295,  1060,     0,
       0,     0,     0,     0,  1063,     0,     0,     0,   351,     0,
       0,     0,   123,     0,     0,     0,     0,     0,  1309,     0,
    1310,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   123,     0,     0,     0,     0,     0,     0,     0,
       0,     1,     0,     0,     0,     0,   165,     0,     1,     0,
     165,     0,     0,   165,   165,     0,     0,   165,     0,     0,
     165,   165,     0,     0,     0,     0,     0,     0,     0,     0,
     123,     0,     0,  1366,     0,     1,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   387,  1380,
       0,     0,     0,     0,     0,     0,  1384,     0,  1386,  1388,
       0,     0,     0,     0,     0,     0,     0,  1394,     0,  1395,
       0,  1396,   165,  1398,     0,   165,     0,     0,  1406,  1212,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   165,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   165,     0,     0,     0,     0,     0,     0,     0,   123,
     123,   123,   123,   123,   123,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1448,     0,     0,     0,
       0,     0,     0,  1455,  1456,     0,     0,     0,   123,   123,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1258,     0,     0,     0,  1259,     0,     0,  1479,     0,     0,
       0,   884,     0,     0,  1484,     0,     0,  1485,     0,     0,
       0,  1272,     0,     0,     0,     0,     0,     0,  1273,     0,
       0,     0,     0,     0,     0,     0,     0,  1277,     0,  1278,
       0,     0,     0,     0,     0,   165,     0,   214,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     123,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     649,  1305,     0,   387,   654,  1306,     0,     0,     0,     0,
       0,     0,     0,   660,   661,     0,     0,  1574,     0,   145,
       0,     0,     1,     0,     0,     0,     0,     0,   387,   387,
     351,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   165,     0,   247,     0,     0,     0,     0,   387,
    1593,     0,     0,     0,     0,   268,     0,   271,  1598,   273,
    1600,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   387,
       0,     0,   123,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   247,     0,   271,
     273,     0,     0,     0,     0,     0,  1626,  1627,     0,     0,
    1393,     0,     0,     0,     0,     0,   351,   123,     0,     0,
       0,  1632,  1633,     0,  1634,     0,     0,     0,     0,     0,
       0,     0,     0,  1638,  1416,     0,     0,     0,     0,     0,
       0,   247,     0,  1643,  1644,     0,     0,   123,     0,     0,
       0,     0,     0,     0,     0,     0,   165,   165,     0,     0,
       0,   123,     0,     0,     0,     0,     0,     0,     0,   165,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   123,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   247,     0,   271,   273,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1488,     0,     0,     0,  1489,     0,
       0,     0,     0,     0,   247,     0,     0,     0,     0,     0,
     247,     0,     0,     0,     0,     0,     0,     0,     0,  1722,
    1723,     0,     0,     0,     0,     0,     0,     0,  1524,  1729,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   247,
     123,     0,     0,     0,     0,   620,     0,   273,     0,     0,
       0,     0,     0,   165,   165,     0,     0,     0,     0,   165,
       0,     0,     0,     0,  1752,  1753,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1584,     0,
     165,  1587,     0,   165,   165,     0,   165,     0,   165,   165,
       0,     0,     0,     0,     0,     0,  1595,     0,     0,     0,
       0,     0,     0,   387,   387,   387,   387,   387,   387,   387,
     387,   387,   387,   387,   387,   387,   387,   387,   387,   387,
     387,   387,     0,     0,     0,     0,     0,   165,     0,     0,
       0,   165,     0,     0,     0,   247,     0,     0,     0,     0,
       0,     0,     0,     0,  1808,     0,  1625,     0,     0,     0,
       0,     0,     0,     0,     0,  1630,     0,     0,     0,  1631,
       0,   247,  1817,   620,   273,  1818,  1819,     0,     0,     0,
       0,     0,  1821,  1635,  1636,     0,     0,     0,     0,     0,
       0,     0,   123,   387,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   123,
       0,     0,     0,     0,     0,     0,     0,   165,   247,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   247,     0,
       0,     0,     0,   247,     0,   247,     0,   123,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   247,     0,   247,   247,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   247,     0,     0,     0,     0,
    1907,     0,     0,     0,     0,  1718,  1719,   247,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   247,
       0,   620,   273,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1938,
       0,     0,     0,   247,   620,     0,     0,   165,   387,     0,
     247,     0,     0,   387,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1955,   387,     0,     0,     0,     0,     0,
    1961,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   247,   268,  1974,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   165,     0,     0,   165,     0,
       0,     0,     0,     0,   387,     0,     0,     0,     0,     0,
       0,     0,     0,  1795,     0,     0,     0,     0,     0,   338,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1587,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   435,   338,
       0,     0,     0,     0,  1820,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     501,  1838,     0,     0,     0,     0,     0,   501,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1855,     0,
       0,  1856,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   165,     0,
       0,     0,     0,     0,     0,     0,   165,   165,     0,     0,
       0,   387,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   501,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   247,     0,     0,   338,   606,
       0,   165,     0,     0,     0,     0,   247,     0,     0,  1926,
     165,     0,     0,   165,     0,   165,   165,     0,     0,   627,
       0,     0,     0,     0,     0,   387,     0,   247,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   247,     0,
       0,     0,     0,     0,     0,     0,     0,   247,     0,     0,
       0,     0,     0,     0,     0,   165,     0,     0,     0,   387,
     387,   387,     0,     0,     0,     0,   387,   387,     0,     0,
    1194,     0,     0,     0,     0,     0,     0,    13,    14,    15,
      16,    17,     0,     0,     0,     0,     0,     0,   387,   501,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   501,   750,     0,   501,   753,
       0,     0,     0,   360,     0,     0,   338,   361,     0,   362,
     606,     0,   165,     0,     0,   387,   387,     0,     0,     0,
       0,     0,     0,     0,     0,    57,   363,     0,     0,     0,
       0,     0,     0,   247,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   501,     0,   364,   365,   501,   366,   247,   367,    62,
      63,    64,    65,    66,    67,    68,    69,    70,   368,   369,
     357,     0,   370,   371,   372,     0,   373,   374,     0,     0,
       0,     0,     0,     0,    72,     0,     0,   338,     0,     0,
       0,     0,     0,     0,   165,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   375,     0,     0,    75,   376,     0,
       0,   200,     0,     0,   377,   438,    78,   378,   379,   380,
     381,     0,     0,   165,     0,     0,     0,   255,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   501,     0,     0,
     338,     0,     0,     0,     0,     0,     0,     0,     0,   165,
       0,     0,     0,     0,     0,   165,     0,   879,   338,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   606,     0,
       0,     0,   606,     0,     0,     0,   200,     0,     0,   897,
     303,   338,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   343,     0,   247,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   200,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     165,   455,     0,     0,   459,   247,    13,    14,    15,    16,
      17,   247,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
       0,     0,     0,     0,    45,    46,     0,     0,     0,     0,
       0,     0,     0,     0,   200,     0,     0,     0,     0,     0,
       0,     0,     0,   338,    57,     0,     0,   255,     0,     0,
       0,     0,     0,     0,   165,   165,     0,     0,     0,   501,
     501,     0,   351,     0,     0,     0,   165,     0,   387,   501,
    1006,     0,   501,  1009,     0,   656,     0,     0,     0,     0,
       0,     0,     0,   459,   338,     0,     0,   606,     0,   606,
     606,   200,     0,     0,     0,     0,   606,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   338,   338,     0,     0,
     599,     0,   616,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   247,     0,     0,   338,     0,     0,     0,   501,
       0,     0,     0,   501,     0,  1830,     0,   501,  1077,     0,
       0,   501,  1081,     0,     0,     0,     0,     0,     0,  1084,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   165,
       0,     0,     0,     0,   675,     0,     0,     0,     0,     0,
     247,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   360,     0,     0,     0,   361,     0,   362,     0,
       0,   338,   501,     0,     0,     0,   411,     0,   200,     0,
       0,     0,     0,     0,     0,   363,     0,     0,     0,   440,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   606,
       0,   165,   468,     0,   468,     0,     0,     0,   599,     0,
       0,     0,   364,   365,   774,   366,     0,   367,    62,    63,
      64,    65,    66,    67,    68,    69,    70,   368,   369,   357,
       0,   370,   371,   372,     0,   373,   374,     0,     0,   338,
       0,     0,   387,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   375,     0,     0,    75,   376,     0,     0,
       0,     0,   387,   377,    77,    78,   378,   379,   380,   381,
       0,     0,     0,   200,   200,   165,     0,     0,     0,   455,
     568,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   501,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   247,     0,
       0,   606,   606,     0,     0,     0,     0,     0,   606,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   343,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   387,     0,   387,
     455,     0,   883,     0,     0,     0,     0,     0,     0,     0,
     338,     0,     0,     0,     0,   501,  1300,     0,   501,  1304,
       0,     0,     0,   599,     0,   387,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   200,     0,     0,     0,     0,   387,
       0,     0,     0,     0,     0,     0,     0,   675,     0,   675,
     675,     0,   675,     0,     0,     0,     0,     0,   675,     0,
       0,   675,   675,   675,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   247,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   387,     0,     0,     0,     0,     0,
       0,   247,   468,     0,     0,     0,     0,     0,   468,     0,
       0,     0,     0,   795,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   455,     0,     0,     0,     0,
       0,     0,   338,     0,     0,     0,     0,     0,   606,  1402,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   200,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   338,
       0,     0,     0,     0,     0,     0,   455,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   247,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   455,   455,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     863,   501,  1452,     0,     0,     0,     0,   455,     0,     0,
     501,  1461,     0,   606,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   338,   338,     0,     0,   440,     0,
       0,     0,     0,    13,    14,    15,    16,    17,     0,     0,
      19,   891,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,  -416,  -416,     0,
    -416,    45,    46,   455,  -416,   247,     0,     0,     0,     0,
     200,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     774,    57,   926,     0,     0,     0,     0,     0,     0,  1404,
       0,     0,     0,     0,     0,     0,    13,    14,    15,    16,
      17,     0,     0,   795,   946,     0,     0,     0,     0,     0,
       0,     0,     0,   957,     0,   962,   957,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     338,   343,   360,     0,     0,     0,   361,     0,   362,     0,
       0,     0,     0,   990,     0,     0,     0,   247,     0,     0,
       0,     0,     0,     0,    57,   363,   992,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1001,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   440,   364,   365,   990,   366,     0,   367,    62,    63,
      64,    65,    66,    67,    68,    69,    70,   368,   369,   357,
       0,   370,   371,   372,     0,   373,   374,     0,     0,     0,
       0,  1054,     0,    72,   468,     0,     0,     0,   360,     0,
       0,     0,   361,     0,   362,     0,     0,     0,     0,     0,
       0,     0,     0,   375,     0,     0,    75,   376,     0,   501,
       0,   363,     0,   377,  1405,    78,   378,   379,   380,   381,
    1085,     0,     0,     0,     0,   501,     0,     0,     0,     0,
       0,     0,   455,     0,     0,     0,     0,     0,   364,   365,
       0,   366,     0,   367,  1782,    63,    64,    65,    66,    67,
      68,    69,    70,   368,   369,   357,     0,   370,   371,   372,
       0,   373,   374,     0,     0,     0,     0,     0,   411,    72,
       0,     0,   675,     0,     0,     0,     0,     0,  1186,  1188,
       0,     0,     0,     0,     0,     0,   440,     0,     0,   375,
       0,   338,    75,   376,     0,     0,     0,     0,     0,   377,
      77,    78,   378,   379,   380,   381,     0,   468,     0,     0,
       0,     0,     0,  1783,  -177,     0,   957,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   255,     0,     0,   990,
       0,     0,     0,     0,     0,     0,     0,  1225,     0,     0,
       0,     0,   338,   338,   957,     0,     0,   200,     0,     0,
       0,     0,     0,     0,   599,     0,     0,   501,   501,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   501,     0,     0,     0,     0,     0,     0,
       0,   343,     0,     0,     0,   675,     0,     0,   468,     0,
       0,     0,     0,     0,     0,   242,   243,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,     0,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,  -417,  -417,
       0,  -417,    45,    46,     0,  -417,   455,   455,     0,     0,
       0,     0,     0,     0,     0,   468,     0,  1291,     0,  1293,
       0,     0,    57,     0,     0,     0,     0,   501,     0,     0,
       0,     0,     0,     0,     0,   501,     0,     0,     0,     0,
       0,     0,     0,     0,   675,   675,   675,     0,   675,   675,
       0,     0,     0,    61,     0,   459,    62,    63,    64,    65,
      66,    67,    68,    69,    70,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,  1358,  1358,   338,     0,     0,
       0,   501,  1885,     0,     0,   501,     0,     0,     0,     0,
       0,    73,    74,   255,    75,   244,     0,     0,     0,  -726,
       0,     0,    77,    78,     0,     0,     0,     0,     0,     0,
       0,     0,   343,     0,     0,   501,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1397,     0,     0,     0,
       0,     0,  1407,     0,     0,     0,     0,     0,     0,     0,
       0,   360,     0,     0,     0,   361,     0,   362,     0,   440,
       0,     0,     0,     0,     0,   501,   501,     0,     0,     0,
       0,     0,     0,     0,   363,     0,   468,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   957,
       0,     0,   795,     0,     0,     0,     0,     0,   501,     0,
       0,   364,   365,     0,   462,     0,   367,    62,    63,    64,
      65,    66,    67,    68,    69,    70,   368,   369,   357,     0,
     370,   371,   372,     0,   373,   374,     0,   200,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1487,     0,     0,     0,     0,     0,     0,
       0,     0,   375,    74,     0,   463,   464,     0,     0,     0,
     465,   255,   377,    77,    78,   378,   379,   380,   381,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   957,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   468,
       0,     0,   795,   343,     0,     0,     0,     0,     0,     0,
    1975,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1342,     0,     0,   675,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     946,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1596,  1597,     0,     0,   455,   455,     0,   360,     0,     0,
       0,   361,     0,   362,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   468,     0,   795,  1110,     0,
     363,    -2,     0,  1112,  -236,  -236,  1113,  1114,  1115,  1116,
    1117,  1118,  1119,  1120,  1121,  1122,  1123,  1124,  -296,  1125,
    1126,  1127,  1128,  1129,   255,  1130,     0,   364,   365,     0,
     462,     0,   367,  1131,  1132,    64,    65,    66,    67,    68,
      69,    70,   368,   369,   357,  1133,   370,   371,   372,   360,
     373,   374,     0,   361,     0,   362,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   363,     0,     0,   411,     0,  -236,   375,     0,
    1667,    75,   376,     0,     0,     0,   279,     0,   377,    77,
      78,   378,   379,   380,   381,     0,     0,     0,     0,   364,
     365,     0,   366,  -177,   367,    62,    63,    64,    65,    66,
      67,    68,    69,    70,   368,   369,   357,     0,   370,   371,
     372,     0,   373,   374,     0,     0,  1975,     0,     0,     0,
      72,     0,     0,     0,   675,     0,  1707,     0,     0,     0,
       0,     0,  1342,     0,     0,     0,     0,     0,     0,     0,
     375,  1228,     0,    75,   376,     0,     0,     0,  1229,   455,
     377,    77,    78,   378,   379,   380,   381,     0,     0,     0,
       0,     0,     0,   360,     0,  1730,     0,   361,  1732,   362,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1110,   675,   363,    -2,   459,  1112,
    -237,  -237,  1113,  1114,  1115,  1116,  1117,  1118,  1119,  1120,
    1121,  1122,  1123,  1124,  -296,  1125,  1126,  1127,  1128,  1129,
       0,  1130,     0,   364,   365,     0,   462,     0,   367,  1131,
    1132,    64,    65,    66,    67,    68,    69,    70,   368,   369,
     357,  1133,   370,   371,   372,  1714,   373,   374,     0,     0,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,  1342,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -237,   375,     0,     0,    75,   376,     0,
       0,     0,   279,     0,   377,    77,    78,   378,   379,   380,
     381,     0,   360,     0,     0,     0,   361,     0,   362,  -177,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1110,     0,   363,    -2,     0,  1112,     0,
       0,  1113,  1114,  1115,  1116,  1117,  1118,  1119,  1120,  1121,
    1122,  1123,  1124,  -296,  1125,  1126,  1127,  1128,  1129,     0,
    1130,     0,   364,   365,     0,   462,     0,   367,  1131,  1132,
      64,    65,    66,    67,    68,    69,    70,   368,   369,   357,
    1133,   370,   371,   372,     0,   373,   374,     0,     0,     0,
     957,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   375,     0,     0,    75,   376,     0,     0,
       0,   279,     0,   377,    77,    78,   378,   379,   380,   381,
       0,     0,     0,     0,     0,     0,     0,     0,  -177,     4,
     243,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,  1109,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,   360,     0,    45,    46,   361,     0,
     362,    47,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,    56,     0,  1110,    57,  1111,    -2,     0,
    1112,     0,     0,  1113,  1114,  1115,  1116,  1117,  1118,  1119,
    1120,  1121,  1122,  1123,  1124,  -296,  1125,  1126,  1127,  1128,
    1129,     0,  1130,     0,   364,   365,    60,   462,     0,   367,
    1131,  1132,    64,    65,    66,    67,    68,    69,    70,   368,
     369,   357,  1133,   370,   371,   372,     0,   373,   374,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    -3,   375,     0,     0,    75,   407,
       0,     0,     0,   279,     0,   377,    77,    78,   378,   379,
     380,   381,     0,     0,     0,     0,     0,     0,     0,     0,
    -177,     4,   243,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,  1109,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,   360,     0,    45,    46,
     361,     0,   362,    47,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,    56,     0,  1110,    57,  1111,
      -2,     0,  1112,     0,     0,  1113,  1114,  1115,  1116,  1117,
    1118,  1119,  1120,  1121,  1122,  1123,  1124,  -296,  1125,  1126,
    1127,  1128,  1129,     0,  1130,     0,   364,   365,    60,   462,
       0,   367,  1131,  1132,    64,    65,    66,    67,    68,    69,
      70,   368,   369,   357,  1133,   370,   371,   372,     0,   373,
     374,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   375,     0,     0,
      75,   407,     0,     0,     0,   279,     0,   377,    77,    78,
     378,   379,   380,   381,     0,     0,     0,     0,     0,     0,
       0,     0,  -177,     4,   243,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,   360,     0,
      45,    46,   361,     0,   362,    47,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,    56,     0,     0,
      57,   363,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   364,   365,
      60,   366,     0,   367,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   368,   369,   357,     0,   370,   371,   372,
       0,   373,   374,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   375,
       0,  1544,    75,   407,     0,     0,     0,     0,     0,   377,
      77,    78,   378,   379,   380,   381,     0,     0,     0,     0,
       0,     0,     0,  1545,  1546,     4,   243,     6,     7,     8,
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
       0,   377,    77,    78,   378,   379,   380,   381,     0,     0,
       0,     0,     0,     0,     0,  1545,  1546,     4,   243,     6,
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
       0,     0,     0,   375,     0,  1535,    75,   407,     0,     0,
       0,     0,     0,   377,    77,    78,   378,   379,   380,   381,
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
       0,    75,   437,     0,     0,     0,     0,     0,   377,   438,
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
     375,     0,     0,    75,  1183,     0,     0,     0,     0,     0,
     377,  1184,    78,   378,   379,   380,   381,   243,     6,     7,
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
       0,     0,   375,     0,     0,    75,   376,     0,     0,     0,
       0,     0,   377,    77,    78,   378,   379,   380,   381,   243,
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
       0,     0,     0,     0,   375,     0,     0,    75,   437,     0,
       0,     0,     0,     0,   377,    77,    78,   378,   379,   380,
     381,  1837,     0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
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
      -2,    -2,  1854,     0,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,
      -2,     0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,
      -2,    -2,    -2,     0,    -2,     0,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,     0,     0,     0,    -2,     0,
       0,    -2,     0,     0,     0,     0,    -2,    -2,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    -2,     0,     0,     0,    -2,    -2,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      -2,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    -2,
       0,     0,     0,    -2,    -2,     0,     0,     0,     0,     0,
       0,    -2,    -2,     4,     5,     6,     7,     8,     9,    10,
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
      77,    78,     4,   243,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,     0,     0,     0,     0,    45,
      46,     0,     0,     0,    47,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,    56,     0,     0,    57,
       0,     0,     0,     0,  -349,  -349,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    60,
       0,     0,     0,    62,    63,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -349,     0,     0,
       0,    75,    76,     0,     0,     0,     0,     0,     0,    77,
      78,     4,   243,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,     0,     0,    45,    46,
       0,     0,     0,    47,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,    56,     0,     0,    57,     0,
       0,     0,     0,  -350,  -350,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    60,     0,
       0,     0,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -350,     0,     0,     0,
      75,    76,     0,     0,     0,     0,     0,     0,    77,    78,
     242,   243,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,     0,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,  -417,  -417,     0,  -417,    45,    46,     0,
    -417,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    13,    14,    15,    16,    17,     0,    57,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,     0,     0,     0,    61,    45,
      46,    62,    63,    64,    65,    66,    67,    68,    69,    70,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    57,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    73,    74,     0,    75,
     244,     0,     0,  1318,     0,     0,     0,    77,    78,  1319,
       0,     0,    13,    14,    15,    16,    17,    18,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,     0,     0,
      45,    46,     0,     0,     0,    47,    48,    49,    50,    51,
      52,    53,    54,     0,     0,     0,     0,     0,     0,     0,
      57,  1320,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      60,     0,     0,     0,    62,    63,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1321,     0,
       0,     0,    75,   922,     0,     0,  1318,     0,     0,     0,
      77,    78,  1319,     0,     0,    13,    14,    15,    16,    17,
      18,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,     0,
       0,     0,     0,    45,    46,     0,     0,     0,    47,    48,
      49,    50,    51,    52,    53,    54,     0,     0,     0,     0,
       0,     0,     0,    57,  1320,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    60,     0,     0,     0,    62,    63,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1499,     0,     0,     0,    75,   922,     0,     0,  1318,
       0,     0,     0,    77,    78,  1319,     0,     0,    13,    14,
      15,    16,    17,    18,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,     0,     0,    45,    46,     0,     0,
       0,    47,    48,    49,    50,    51,    52,    53,    54,     0,
       0,     0,     0,     0,     0,     0,    57,  1320,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    60,     0,     0,     0,
      62,    63,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1500,     0,     0,     0,    75,   922,
       0,     0,  1318,     0,     0,     0,    77,    78,  1319,     0,
       0,    13,    14,    15,    16,    17,    18,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,     0,     0,     0,     0,    45,
      46,     0,     0,     0,    47,    48,    49,    50,    51,    52,
      53,    54,     0,     0,     0,     0,     0,     0,     0,    57,
    1320,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    60,
       0,     0,     0,    62,    63,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1501,     0,     0,
       0,    75,   922,     0,     0,     0,     0,     0,     0,    77,
      78,   242,   243,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,     0,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,  -417,  -417,     0,  -417,    45,    46,
       0,  -417,     0,     0,     0,     0,     0,     0,     0,     0,
      13,    14,    15,    16,    17,     0,     0,    19,    57,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,  -417,  -417,     0,  -417,    45,    46,
       0,  -417,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    57,     0,
       0,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      75,   244,     0,     0,     0,     0,     0,     0,    77,    78,
     243,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,     0,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,  -417,  -417,     0,  -417,    45,    46,     0,  -417,
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
       0,     0,     0,  -730,     0,     0,    77,    78,   243,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,     0,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
    -417,  -417,     0,  -417,    45,    46,     0,  -417,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    57,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    61,     0,     0,    62,    63,
      64,    65,    66,    67,    68,    69,    70,     0,     0,     0,
       0,     0,     0,     0,  1342,     0,     0,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    73,    74,   360,    75,   244,     0,   361,
       0,   362,     0,     0,    77,    78,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1110,     0,   363,     0,
       0,  1112,  1772,  1773,  1113,  1114,  1115,  1116,  1117,  1118,
    1119,  1120,  1121,  1122,  1123,  1124,  -296,  1125,  1126,  1127,
    1128,  1129,     0,  1130,     0,   364,   365,     0,   462,     0,
     367,  1131,  1132,    64,    65,    66,    67,    68,    69,    70,
     368,   369,   357,  1133,   370,   371,   372,     0,   373,   374,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,  1342,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   375,     0,     0,    75,
     376,     0,     0,     0,   279,     0,   377,    77,    78,   378,
     379,   380,   381,   360,     0,     0,     0,   361,     0,   362,
       0,  -177,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1110,     0,   363,     0,     0,  1112,
       0,     0,  1113,  1114,  1115,  1116,  1117,  1118,  1119,  1120,
    1121,  1122,  1123,  1124,  -296,  1125,  1126,  1127,  1128,  1129,
       0,  1130,     0,   364,   365,     0,   462,     0,   367,  1131,
    1132,    64,    65,    66,    67,    68,    69,    70,   368,   369,
     357,  1133,   370,   371,   372,     0,   373,   374,     0,     0,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   375,     0,     0,    75,   376,     0,
       0,     0,   279,     0,   377,    77,    78,   378,   379,   380,
     381,     0,     0,     0,     0,     0,     0,     0,     0,  -177,
     243,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,     0,     0,    45,    46,     0,     0,
       0,   319,    48,    49,    50,    51,    52,    53,    54,     0,
      13,    14,    15,    16,    17,    18,    57,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,     0,     0,    45,    46,
      62,    63,     0,    47,    48,    49,    50,    51,    52,    53,
      54,     0,     0,     0,     0,     0,     0,     0,    57,     0,
       0,     0,     0,     0,     0,    72,     0,  1045,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -598,    75,   320,
       0,     0,    62,    63,     0,     0,    77,    78,   243,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      75,     0,     0,     0,    45,    46,     0,     0,     0,   319,
      48,    49,    50,    51,    52,    53,    54,     0,    13,    14,
      15,    16,    17,    18,    57,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,     0,     0,    45,    46,    62,    63,
       0,   319,    48,    49,    50,    51,    52,    53,    54,     0,
       0,     0,     0,     0,     0,     0,    57,     0,     0,     0,
       0,     0,     0,    72,     0,  1748,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    75,   320,     0,     0,
      62,    63,     0,     0,    77,    78,   243,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    75,     0,
       0,     0,    45,    46,     0,     0,     0,   319,    48,    49,
      50,    51,    52,    53,    54,     0,     0,     0,     0,     0,
       0,     0,    57,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    72,     0,  1750,     0,     0,     0,     0,     0,     0,
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
       0,     0,    75,   320,     0,     0,     0,     0,     0,     0,
      77,    78,   243,     6,     7,     8,     9,    10,    11,    12,
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
       0,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      75,   301,     0,     0,     0,     0,     0,     0,    77,    78,
     243,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,     0,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,  -417,  -417,     0,  -417,    45,    46,     0,  -417,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    57,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      62,    63,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    75,   244,
       0,     0,     0,     0,     0,     0,    77,    78,    13,    14,
      15,    16,    17,    18,   662,    19,   663,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,   360,     0,    45,    46,   361,     0,
     362,    47,    48,    49,    50,    51,    52,    53,    54,     0,
       0,     0,     0,     0,     0,     0,    57,   363,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   664,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   364,   365,     0,   366,     0,   367,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   368,
     369,   357,     0,   370,   371,   372,     0,   373,   374,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   375,     0,     0,    75,   665,
       0,     0,     0,   279,     0,   377,    77,    78,   666,   667,
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
       0,   406,    75,   407,     0,     0,     0,     0,     0,   377,
      77,    78,   378,   379,   380,   381,    13,    14,    15,    16,
      17,    18,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
       0,     0,   360,     0,    45,    46,   361,     0,   362,    47,
      48,    49,    50,    51,    52,    53,    54,     0,     0,     0,
       0,     0,     0,     0,    57,   363,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   364,   365,     0,   366,     0,   367,    62,    63,
      64,    65,    66,    67,    68,    69,    70,   368,   369,   357,
       0,   370,   371,   372,     0,   373,   374,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   375,     0,     0,    75,   665,     0,     0,
       0,   279,     0,   377,    77,    78,   378,   379,   380,   381,
      13,    14,    15,    16,    17,    18,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,   360,     0,    45,    46,
     361,     0,   362,    47,    48,    49,    50,    51,    52,    53,
      54,     0,     0,     0,     0,     0,     0,     0,    57,   363,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   364,   365,     0,   366,
       0,   367,    62,    63,    64,    65,    66,    67,    68,    69,
      70,   368,   369,   357,     0,   370,   371,   372,     0,   373,
     374,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   375,     0,     0,
      75,   407,     0,     0,     0,     0,     0,   377,    77,    78,
     378,   379,   380,   381,    13,    14,    15,    16,    17,    18,
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
       0,   375,     0,     0,    75,   437,     0,     0,     0,     0,
       0,   377,    77,    78,   378,   379,   380,   381,    13,    14,
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
       0,     0,     0,     0,     0,   375,     0,     0,    75,   376,
       0,     0,     0,     0,     0,   377,    77,    78,   378,   379,
     380,   381,    13,    14,    15,    16,    17,    18,     0,    19,
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
      74,     0,    75,    76,     0,     0,     0,  -728,     0,     0,
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
       0,     0,    75,    76,     0,    13,    14,    15,    16,    17,
      77,    78,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,  -417,
    -417,     0,  -417,    45,    46,     0,  -417,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    57,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    61,     0,     0,    62,    63,    64,
      65,    66,    67,    68,    69,    70,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    73,    74,     0,    75,   301,     0,     0,     0,
       0,     0,     0,    77,    78,   556,   243,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,     0,     0,
       0,     0,    45,    46,     0,     0,     0,    47,    48,    49,
      50,    51,    52,    53,    54,     0,    13,    14,    15,    16,
      17,    18,    57,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
       0,     0,     0,     0,    45,    46,    62,    63,     0,    47,
      48,    49,    50,    51,    52,    53,    54,     0,    13,    14,
      15,    16,    17,    18,    57,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,    75,     0,    45,    46,    62,    63,
       0,    47,    48,    49,    50,    51,    52,    53,    54,     0,
       0,     0,     0,     0,     0,     0,    57,     0,     0,     0,
       0,     0,  1419,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   928,    75,   922,     0,     0,
      62,    63,     0,     0,    77,    78,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    75,   922,
       0,     0,     0,     0,     0,     0,    77,    78,    13,    14,
      15,    16,    17,    18,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,     0,     0,    45,    46,     0,     0,
       0,    47,    48,    49,    50,    51,    52,    53,    54,     0,
      13,    14,    15,    16,    17,    18,    57,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,     0,     0,    45,    46,
      62,    63,     0,    47,    48,    49,    50,    51,    52,    53,
      54,     0,     0,     0,     0,     0,     0,     0,    57,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    75,   286,
       0,     0,    62,    63,     0,     0,    77,    78,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      75,    76,     0,     0,     0,     0,     0,     0,    77,    78,
      13,    14,    15,    16,    17,    18,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,     0,     0,    45,    46,
       0,     0,     0,    47,    48,    49,    50,    51,    52,    53,
      54,     0,    13,    14,    15,    16,    17,    18,    57,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,     0,     0,
      45,    46,    62,    63,     0,   319,    48,    49,    50,    51,
      52,    53,    54,     0,     0,     0,     0,     0,     0,     0,
      57,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      75,   433,     0,     0,    62,    63,     0,     0,    77,    78,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    75,   320,     0,     0,     0,     0,     0,     0,
      77,    78,    13,    14,    15,    16,    17,    18,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,     0,     0,
      45,    46,     0,     0,     0,   319,    48,    49,    50,    51,
      52,    53,    54,     0,    13,    14,    15,    16,    17,    18,
      57,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,     0,     0,
       0,     0,    45,    46,    62,    63,     0,   319,    48,    49,
      50,    51,    52,    53,    54,     0,     0,     0,     0,     0,
       0,     0,    57,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    75,   286,     0,     0,    62,    63,     0,     0,
      77,    78,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    75,   433,     0,     0,     0,     0,
       0,     0,    77,    78,   242,   243,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,     0,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,  -417,  -417,     0,
    -417,    45,    46,     0,  -417,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    13,    14,    15,    16,    17,
      18,    57,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,     0,
       0,     0,     0,    45,    46,    62,    63,     0,   319,    48,
      49,    50,    51,    52,    53,    54,     0,    13,    14,    15,
      16,    17,    18,    57,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,     0,     0,    75,     0,    45,    46,    62,    63,     0,
      47,    48,    49,    50,    51,    52,    53,    54,     0,     0,
       0,     0,     0,     0,     0,    57,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    75,   301,     0,     0,    62,
      63,     0,     0,    77,    78,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    75,   922,     0,
       0,     0,     0,     0,     0,    77,    78,    13,    14,    15,
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
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    75,   922,     0,
       0,    62,    63,     0,     0,    77,    78,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    75,
       0,     0,    13,    14,    15,    16,    17,    77,    78,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,  -417,  -417,     0,  -417,
      45,    46,     0,  -417,     0,     0,     0,     0,     0,     0,
       0,     0,    13,    14,    15,    16,    17,     0,     0,    19,
      57,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,  -417,  -417,     0,  -417,
      45,    46,     0,  -417,    62,    63,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      57,     0,     0,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    75,   301,    62,    63,     0,     0,     0,     0,
      77,    78,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    75,     0,     0,     0,     0,     0,     0,     0,
      77,    78,   243,     6,     7,     8,     9,    10,    11,    12,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,   848,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -611,
      75,   243,     6,     7,     8,     9,    10,    11,    12,    13,
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
       0,     0,     0,     0,     0,     0,     0,     0,  1675,     0,
       0,     0,     0,   243,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,     0,    19,    75,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,     0,     0,     0,     0,    45,
      46,     0,     0,     0,   319,    48,    49,    50,    51,    52,
      53,    54,     0,     0,     0,     0,     0,     0,     0,    57,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     243,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    62,    63,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,  -417,  -417,     0,  -417,    45,    46,     0,  -417,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    75,     0,     0,     0,     0,    57,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      62,    63,     0,     0,     0,     0,    13,    14,    15,    16,
      17,    18,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
       0,     0,     0,     0,    45,    46,     0,     0,    75,    47,
      48,    49,    50,    51,    52,    53,    54,   360,     0,     0,
       0,   361,     0,   362,    57,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     363,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    61,     0,     0,    62,    63,
      64,    65,    66,    67,    68,    69,    70,   364,   365,     0,
     366,     0,   367,    62,    63,    64,    65,    66,    67,    68,
      69,    70,   368,   369,   357,     0,   370,   371,   372,   360,
     373,   374,     0,   361,     0,   362,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,    75,     0,     0,     0,
       0,     0,   363,     0,     0,     0,     0,     0,   375,   953,
    1526,    75,   376,     0,     0,     0,     0,     0,   377,    77,
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
       0,     0,   375,   794,     0,    75,   376,     0,     0,     0,
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
     363,     0,     0,     0,     0,     0,   375,   953,     0,    75,
     376,     0,     0,     0,     0,     0,   377,    77,    78,   378,
     379,   380,   381,     0,     0,     0,     0,   364,   365,     0,
     366,     0,   367,    62,    63,    64,    65,    66,    67,    68,
      69,    70,   368,   369,   357,     0,   370,   371,   372,   360,
     373,   374,     0,   361,     0,   362,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   363,     0,     0,     0,     0,     0,   375,     0,
       0,    75,   376,     0,     0,   984,     0,     0,   377,    77,
      78,   378,   379,   380,   381,     0,     0,     0,     0,   364,
     365,     0,   366,     0,   367,    62,    63,    64,    65,    66,
      67,    68,    69,    70,   368,   369,   357,     0,   370,   371,
     372,   360,   373,   374,     0,   361,     0,   362,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   363,     0,     0,     0,     0,     0,
     375,  1292,     0,    75,   376,     0,     0,     0,     0,     0,
     377,    77,    78,   378,   379,   380,   381,     0,     0,     0,
       0,   364,   365,     0,   366,     0,   367,    62,    63,    64,
      65,    66,    67,    68,    69,    70,   368,   369,   357,     0,
     370,   371,   372,   360,   373,   374,     0,   361,     0,   362,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   363,     0,     0,     0,
       0,     0,   375,     0,     0,    75,   376,     0,     0,     0,
    1352,     0,   377,    77,    78,   378,   379,   380,   381,     0,
       0,     0,     0,   364,   365,     0,   366,     0,   367,    62,
      63,    64,    65,    66,    67,    68,    69,    70,   368,   369,
     357,     0,   370,   371,   372,   360,   373,   374,     0,   361,
       0,   362,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   363,     0,
       0,     0,     0,     0,   375,     0,  1778,    75,   376,     0,
       0,     0,     0,     0,   377,    77,    78,   378,   379,   380,
     381,     0,     0,     0,     0,   364,   365,     0,   366,     0,
     367,    62,    63,    64,    65,    66,    67,    68,    69,    70,
     368,   369,   357,     0,   370,   371,   372,   360,   373,   374,
       0,   361,     0,   362,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     363,     0,     0,     0,     0,     0,   375,  1958,     0,    75,
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
     376,     0,     0,     0,     0,     0,   377,   862,    78,   378,
     379,   380,   381,     0,     0,     0,     0,   364,   365,     0,
     366,     0,   367,    62,    63,    64,    65,    66,    67,    68,
      69,    70,   368,   369,   357,     0,   370,   371,   372,     0,
     373,   374,     0,     0,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   375,     0,
       0,    75,   376,     0,     0,     0,     0,     0,   377,   438,
      78,   378,   379,   380,   381
};

static const yytype_int16 yycheck[] =
{
       1,    73,    73,     4,    73,   283,   465,   162,   241,     1,
    1650,   375,    73,   178,   256,   689,   173,   220,    75,   207,
     871,   762,     1,   219,   219,   408,   150,   614,   857,    73,
      58,    95,   959,   676,   162,   733,   669,   601,   177,   222,
      82,   762,  1202,   139,   841,   366,   375,   760,   192,     1,
     149,   219,     4,    73,    55,    56,     1,    58,   857,     1,
     178,   988,  1344,  1345,   760,  1119,    58,  1650,  1344,  1650,
     605,   868,    73,  1772,   173,   630,   219,     1,     0,    58,
     780,    82,   633,   760,   219,   779,   780,     4,   601,    90,
     763,   601,   525,   296,    95,   191,   769,    98,   798,   295,
     295,   102,   535,    82,   798,   147,    58,   290,   291,   181,
     181,   321,   236,    58,     1,  1713,    95,   145,   760,    98,
     181,   219,    87,   102,  1051,   115,     4,   295,   338,   219,
      82,  1103,   342,   120,    58,    82,    87,   181,    96,   140,
       0,   462,   143,   576,   145,    73,   130,   219,   219,   150,
     219,     1,   295,   145,   162,   156,   220,   102,   219,    73,
     295,   181,   163,   153,   864,   152,   145,  1277,  1278,   163,
     864,    58,   338,   229,   323,   219,   342,    55,    56,   180,
     181,   149,    70,   514,   515,   102,    87,   139,   172,   157,
    1889,   149,   157,   145,   195,   147,   760,   295,   254,   219,
     145,   180,   149,   245,   205,   295,   157,  1103,   264,   210,
    1093,    98,    90,   155,   156,  1098,   195,   274,   219,   220,
       0,   145,  1776,   295,   295,   607,   295,   762,   228,   439,
     785,   231,   296,   103,   295,   236,   131,   107,   316,   191,
     110,   220,   112,   115,   245,   919,   174,   760,   631,   352,
     760,  1411,   355,   253,   255,   497,   157,   258,   145,   573,
     302,   149,   140,   263,   265,   143,   245,   181,  1908,     1,
      70,     1,     4,   947,   275,   276,   354,   278,   156,   258,
    1021,  1651,  1111,   471,   562,   163,  1169,   483,   483,   492,
     242,  1573,    87,   245,   295,   296,   445,  1573,  1095,   997,
    1021,  1014,   303,   936,   635,   219,   156,  1103,     1,   310,
     311,  1140,  1111,   321,   315,   483,   211,   296,  1014,   648,
      70,   650,   651,   687,   653,  1908,    58,  1908,    58,    95,
     659,   283,   210,   662,   663,   664,   149,  1014,   283,   435,
     483,  1140,  1015,  1315,  1316,  1317,   347,    70,   483,   149,
     302,   352,  1950,   153,   355,   356,  1235,   892,   913,   283,
     155,   912,   157,  1063,   155,    58,    73,   409,   571,  1063,
     102,   258,  1014,   569,   569,  1929,   520,   255,   248,   151,
     590,    88,   764,   174,   156,   483,   768,   265,   512,   284,
    1760,    75,    76,   483,   518,   777,   778,   275,   276,   149,
     278,   569,  1956,   153,   614,   838,   463,   149,   174,   740,
      70,   483,   483,   145,  1696,   145,    70,   627,   419,  1315,
    1316,  1317,   483,    19,   590,   303,   569,   265,   492,  1312,
     153,   439,   310,   311,   569,   149,   323,   315,   276,   483,
     419,   442,   443,     1,   593,   573,     4,   591,   149,   443,
    1014,    70,   145,   454,   455,   325,   326,   409,   328,    70,
     330,   627,   463,   483,   465,   779,   780,   146,   617,   347,
     157,   569,   149,   157,   352,   624,   151,   355,   534,   569,
     375,   156,   483,   435,   798,   151,  1021,   174,  1858,   149,
     156,   492,  1546,   153,   173,   149,  1550,   569,   569,   153,
      58,  1014,   131,    70,  1014,   505,  1433,   571,   569,    59,
      60,   512,   155,   492,   104,   105,   155,   518,  1191,  1315,
    1316,  1317,   131,   157,    82,  1635,  1636,   527,  1892,   155,
     149,   174,   991,   533,   153,   174,   165,   537,   149,  1909,
     174,  1905,   153,    70,   102,  1344,  1345,   575,   174,   151,
     864,   160,   161,   283,   156,   556,  1838,   558,   445,  1438,
    1439,  1440,   152,    70,   442,   573,    70,  1931,   569,   483,
     571,  1941,   716,  1855,   575,   714,   454,   455,   131,   149,
      70,   139,   149,   575,   585,    70,   153,   145,   589,   147,
     283,    70,   571,  1266,    70,   155,   575,    56,  1185,   787,
      59,    60,  1245,    62,   556,   165,   614,   157,   773,   504,
     562,  1893,   165,   757,   509,   560,   154,   562,   932,    70,
     621,   179,   149,   575,   174,   155,   153,    75,    76,   155,
     575,   526,   633,   191,   155,   779,   780,   174,   562,  1693,
      70,   536,   149,   155,   174,   149,   153,  1198,   174,   153,
    1205,   575,   155,   174,   798,   773,  1038,  1508,   715,   149,
    1334,   531,   174,   153,   149,   174,   784,   591,   153,   879,
     149,   174,   559,   149,   153,   155,   800,   153,   556,   680,
     681,   820,   683,    73,   242,   155,   687,   245,   575,   690,
     149,  1618,   250,  1620,   174,   819,     3,  1252,   149,    89,
     849,     3,   153,   125,   126,   155,   230,   585,    82,   157,
    1820,   589,  1355,   879,   715,   165,   106,   104,   105,   149,
     864,    95,   910,   153,    98,   283,   160,  1781,   102,   149,
     617,   152,   881,   167,   168,  1789,    70,   157,    12,    13,
      14,    15,    16,   621,   302,   129,   155,   169,   170,  1063,
     148,   157,   146,   648,   149,   633,   151,   155,   653,   760,
     151,   762,   151,   157,   659,   149,   155,   149,   762,   153,
    1229,   162,   163,   774,  1573,   150,   160,   161,     1,   173,
     781,     4,   157,   678,   152,   153,   787,  1430,   156,   790,
      12,    13,    14,    15,    16,  1849,    70,   151,   799,   800,
     801,   155,   680,   681,   932,   683,   180,   149,  1050,   687,
     129,   151,   690,     3,   156,   155,   157,   375,   819,  1202,
     148,   195,    12,    13,    14,    15,    16,   155,    98,   155,
     149,   157,   562,   757,   153,    58,   151,   715,   153,   109,
     840,   160,   161,   575,   173,   575,   220,  1178,    70,     3,
     149,   409,   157,  1408,   855,   856,   857,   131,    12,    13,
      14,    15,    16,   171,   149,   857,   984,   146,   153,   562,
    1058,   245,   149,   152,  1429,    98,   153,   435,   149,   102,
      70,   151,   575,   149,   258,   115,    56,  1216,   412,    59,
      60,   151,    62,  1352,   173,   155,   774,  1696,  1212,  1082,
     104,   105,   903,   781,   428,   857,   907,   431,   151,   903,
     151,   912,   857,   151,   155,   151,    70,   918,   870,  1063,
     151,   799,   145,   801,   932,   195,   151,   151,   149,   149,
     825,   155,   153,   857,   151,    12,    13,    14,    15,    16,
     149,   836,  1060,   149,   839,   151,    21,   153,   843,   149,
     951,   149,   129,   870,   155,  1319,   514,   515,   959,   151,
     152,  1194,   849,  1277,   488,   162,   163,   151,   155,  1428,
     857,   155,   149,   860,   155,  1185,   153,   855,   856,   857,
     123,   124,   151,   160,   161,   151,   155,   988,   258,   155,
     991,   149,   155,    70,   881,   153,   173,   149,   556,   151,
    1555,   153,   560,    96,   562,     4,     5,     6,     7,     8,
       9,    10,    11,  1014,   694,   695,   696,   575,   288,  1020,
    1021,   155,  1179,  1180,   294,   903,  1020,  1021,  1411,   907,
     157,   129,   151,   151,   912,   258,   155,   155,   149,  1838,
    1189,   149,   149,   149,   151,   419,   153,   153,  1172,    87,
    1051,   149,  1603,   323,   131,   153,  1855,  1612,  1499,  1500,
    1501,  1103,   160,   161,    63,  1307,  1344,     4,     5,     6,
       7,     8,     9,    10,    11,    46,    47,   635,    49,  1393,
     127,   128,    53,   157,  1212,   108,   109,   110,   111,   112,
     648,   148,   650,   651,  1893,   653,   149,   157,   151,   151,
     153,   659,   151,   155,   662,   663,   664,   151,   154,   151,
     151,   155,  1113,   155,   155,  1116,  1117,  1118,   492,  1111,
    1285,  1286,  1325,  1278,   151,   857,   157,   857,   155,  1803,
    1248,   151,   151,  1277,  1278,   155,   155,   151,   870,  1140,
     151,   155,   151,   151,   155,  1146,   155,   155,  1140,   173,
     151,  1103,   157,  1154,   155,    82,  1157,  1158,   151,  1111,
    1161,   151,   155,   157,   857,   155,  1111,  1285,  1286,   151,
    1913,  1172,   115,  1728,  1917,   445,  1727,  1185,  1157,  1158,
     154,   155,   740,   701,   702,   703,   704,  1111,  1140,    12,
      13,    14,    15,    16,   149,  1140,   149,  1198,  1100,  1101,
    1102,   149,  1097,   155,  1212,   143,   144,   145,   732,   733,
     154,   155,  1213,  1158,   151,  1110,  1140,   155,   742,   158,
     147,   745,   154,   155,  1111,   160,   161,   165,  1229,   154,
     155,   161,  1127,   166,  1235,  1113,   174,   174,  1116,  1117,
    1118,  1158,   154,   155,   154,   155,   159,    70,   171,   519,
     155,   156,   179,  1140,   154,   155,  1811,   154,   155,   154,
     155,  1325,  1140,   154,   155,  1393,  1267,   129,  1146,   539,
    1157,   154,   155,  1315,  1316,  1317,  1154,  1319,  1320,   154,
     155,  1610,   806,  1161,   154,   155,   810,   154,   155,   559,
     814,   154,   155,   154,   155,  1573,   154,   155,   158,   857,
     154,   155,  1189,  1506,   154,   155,   129,   155,   156,   154,
     155,  1514,   870,   871,   154,   155,  1512,  1512,   245,   152,
    1198,  1635,   151,   593,  1325,   151,   149,   151,  1329,   151,
     153,  1332,    75,    76,   151,  1213,   151,   160,   161,   155,
     156,   153,  1236,  1237,  1512,   697,   698,   617,   699,   700,
     101,  1352,   575,   154,   624,   106,   107,   108,   109,   110,
     111,   112,   113,  1315,  1316,  1317,  1318,  1319,  1320,  1512,
     131,  1372,   131,  1374,   156,   302,   156,  1512,   155,  1111,
    1524,  1111,   705,   706,   149,  1393,   149,   657,   658,  1519,
    1520,   151,  1344,  1345,   321,  1374,  1179,  1180,   151,  1344,
    1345,  1318,   153,   143,   144,   145,   151,   154,  1140,   151,
    1140,   157,   151,   151,  1512,   155,   153,    68,  1111,   154,
    1344,  1345,  1512,   157,   157,   165,  1158,  1428,   157,  1584,
     149,    76,  1433,  1572,   174,    17,   154,  1438,  1439,  1440,
    1335,  1336,  1506,  1512,   155,   173,   149,  1140,   157,   174,
    1514,  1329,   151,   151,  1332,   157,  1584,  1344,  1345,   174,
     157,   205,   154,   154,  1416,  1668,    17,   148,   151,  1665,
    1665,   151,   151,   997,   151,   151,   151,   151,  1373,   151,
     148,  1636,   409,    68,   157,   151,   157,  1374,   157,   174,
     151,  1635,  1636,   151,  1372,   173,   151,  1665,   118,  1416,
     120,   121,   122,   157,   148,  1506,   155,   155,   151,   151,
     151,  1512,   439,  1514,   151,   151,   363,   151,   151,   151,
    1521,   151,  1665,    12,    13,    14,    15,    16,    17,   149,
    1665,   154,   152,   153,  1535,   151,   151,   157,   158,   151,
     151,   388,   389,  1544,  1068,  1103,   151,  1071,   151,   101,
     151,   151,   151,  1111,   106,   107,   108,   109,   110,   111,
     112,   151,   409,   151,   148,   173,  1769,  1665,   151,  1708,
     148,   151,   149,   149,   155,  1665,  1584,   149,   149,   849,
    1581,   149,  1140,   149,    13,  1795,  1318,   514,   515,   156,
     860,    72,   439,   155,    89,  1719,   156,   154,  1543,   174,
    1158,   174,  1603,   148,  1668,   154,   148,  1772,   155,   174,
     174,   881,  1344,  1345,  1344,  1345,   157,  1618,   174,  1620,
    1178,  1573,   151,   154,   151,   155,  1543,   155,  1573,   151,
     155,   101,   151,   154,   857,  1587,   106,   107,   108,   109,
     110,   111,   112,  1521,   151,   148,  1856,   870,   148,  1573,
     149,  1344,  1345,   149,  1772,  1542,   174,  1535,  1216,   129,
    1799,   174,    78,   174,  1665,  1820,  1544,  1668,   174,  1564,
    1587,   149,   174,   174,   148,   151,  1820,   174,  1679,   149,
     150,   174,  1683,   153,  1416,   149,  1573,   614,   174,   148,
     160,   161,   148,     9,  1695,   155,   157,   155,   118,   154,
    1701,   154,   154,  1581,   151,  1769,  1909,   148,   635,   154,
    1859,   151,  1908,  1908,   156,   148,   151,  1718,  1719,   463,
     156,   465,  1887,   151,  1889,  1603,  1727,   151,   151,   154,
    1254,   148,   154,   151,  1858,   174,   151,   156,  1941,  1263,
    1908,   155,   149,   151,  1696,   155,   149,   149,   107,   154,
    1915,  1696,   148,  1795,   154,   154,   157,  1315,  1316,  1317,
    1318,  1319,  1320,   148,   151,  1908,   154,   151,  1769,  1887,
     151,  1889,  1696,  1908,  1913,  1776,   151,   151,  1917,  1918,
     151,   174,   174,  1157,  1158,   101,  1344,  1345,   104,   105,
     106,   107,   108,   109,   110,   111,   112,  1915,  1685,  1800,
     148,  1679,   149,    88,  1943,  1683,   174,   151,   151,  1696,
    1908,  1543,   154,   740,  1856,   154,   148,  1695,  1908,  1984,
     148,  1939,   151,  1701,   151,   151,  1965,   151,   151,   151,
    1969,    73,   153,   174,   152,    73,  1908,  1908,   174,  1908,
    1718,  1573,  1843,  1573,   174,  1909,   693,  1908,     1,  1727,
    1851,     4,  1991,   156,   174,  1587,   151,  1858,  1416,  1860,
      12,    13,    14,    15,    16,    17,  1984,   148,   151,   151,
    1871,   151,  1873,  1874,   148,   150,   155,  1941,   150,   148,
    1573,   101,   149,   156,   155,    73,  1838,  1157,  1111,   149,
     148,   154,   107,  1838,  1895,   165,   174,   165,  1776,   151,
     107,   156,   151,  1855,   148,    58,   151,  1908,  1909,   174,
    1855,   148,   174,   149,  1838,    73,   151,  1140,  1919,  1189,
      73,   151,  1800,   174,   174,   375,   377,  1610,  1929,    82,
    1909,  1855,  1334,  1242,  1157,  1158,   668,   707,   709,   708,
    1941,  1893,    95,   710,   871,    98,  1129,   711,  1893,   102,
    1508,  1838,  1140,  1954,  1956,  1956,    12,    13,    14,    15,
      16,    17,  1941,  1573,  1696,  1843,  1696,  1700,  1855,  1893,
    1889,  1972,  1905,  1851,  1696,  1784,  1951,  1978,  1502,  1804,
    1950,  1938,  1860,  1565,  1565,  1543,   139,  1988,  1856,  1918,
    1855,  1992,   145,  1871,   147,  1873,  1874,   150,   151,  1161,
    1374,  2002,  1969,  1696,    48,  1506,  1893,   250,  1769,   162,
    1828,   787,  1520,   101,  1320,  1573,  1154,  1895,   106,   107,
     108,   109,   110,   111,   112,   874,   179,   180,   181,  1587,
      12,    13,    14,    15,    16,   472,  1416,   585,   191,   192,
    1587,  1919,   195,   787,     0,   918,   790,    -1,   732,    -1,
     732,  1929,  1610,    -1,    -1,    -1,  1326,    -1,    -1,    -1,
      -1,   149,   150,    -1,   514,   515,   219,   220,   732,  1964,
      -1,    12,    13,    14,    15,    16,  1954,    -1,  1956,    -1,
      -1,   928,    -1,   236,    -1,  1980,   933,    -1,    70,    -1,
      -1,    -1,   245,    -1,  1972,  1318,    -1,   944,    -1,    -1,
    1978,    -1,    -1,    -1,  1374,   258,  1838,    -1,  1838,    -1,
    1988,    -1,    -1,    -1,  1992,    -1,    -1,    -1,   826,    -1,
      -1,  1344,  1345,  1855,  2002,  1855,  1650,  1651,    -1,    70,
      -1,    -1,    -1,    -1,    -1,   288,    -1,    -1,  1696,    -1,
      -1,   294,   295,   296,    -1,  1838,    -1,   129,   101,   302,
      -1,  1374,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,  1893,  1855,  1893,   117,    -1,   119,   149,   321,   322,
     323,   153,    -1,    -1,    -1,    -1,   884,    -1,   160,   161,
      -1,    -1,    -1,    -1,    -1,   338,    -1,    -1,   129,   342,
      -1,    -1,    -1,  1416,    -1,    -1,    -1,   150,   648,    -1,
    1893,    -1,    -1,   653,    -1,    -1,    -1,   951,   149,   659,
      -1,    -1,   153,     9,    -1,   959,    -1,    -1,    -1,   160,
     161,    -1,   375,    -1,   101,    -1,    -1,    -1,   678,   106,
     107,   108,   109,   110,   111,   112,  1760,    12,    13,    14,
      15,    16,    17,    -1,   988,    -1,    -1,   991,    -1,    -1,
      -1,  1178,    -1,    -1,    -1,    -1,   409,    -1,  1185,   412,
      -1,    -1,   712,    -1,  1111,    -1,   419,    -1,    -1,    -1,
      -1,    -1,  1542,   150,    -1,   192,   153,    -1,    -1,    -1,
    1838,    -1,   435,    -1,    -1,   993,   439,    -1,     3,    -1,
     443,    -1,   445,    -1,    -1,    -1,    -1,  1855,    -1,    -1,
    1824,    -1,  1010,  1011,  1828,   101,    -1,  1051,   104,   105,
     106,   107,   108,   109,   110,   111,   112,    -1,   101,  1542,
    1543,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     483,    -1,    -1,    -1,  1858,  1893,    -1,    -1,  1185,   492,
      -1,    -1,    -1,   551,    -1,    -1,    -1,    -1,    -1,    -1,
    1573,     4,     5,     6,     7,     8,     9,    10,    11,   512,
      -1,   514,   515,    -1,  1587,   518,    -1,   520,    -1,    -1,
      -1,    -1,  1219,  1220,  1221,    55,    56,   160,    -1,  1226,
    1227,    -1,    -1,    -1,  1908,  1909,   101,    -1,    -1,   104,
     105,   106,   107,   108,   109,   110,   111,   112,    -1,    -1,
      -1,    -1,    -1,    -1,   321,   558,    -1,   324,    -1,    -1,
      90,    -1,    -1,    -1,   129,  1685,   569,  1941,   571,    -1,
     573,   338,   575,    -1,    -1,   342,    -1,    -1,    -1,    12,
      13,    14,    15,    16,   149,   150,    -1,   590,   591,    -1,
     593,   156,    -1,    -1,    -1,   160,   161,    -1,   601,    -1,
      -1,    -1,   605,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     140,   614,  1685,   143,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   624,    -1,  1696,   627,    -1,   156,    -1,    -1,    -1,
      -1,    -1,   635,    -1,    -1,  1229,    -1,    70,    -1,    -1,
      -1,  1235,    -1,    -1,    -1,   648,    -1,   650,   651,    -1,
     653,    -1,    -1,    70,    -1,    -1,   659,    -1,    -1,   662,
     663,   664,    -1,    -1,    -1,    73,    -1,    -1,   101,    -1,
      -1,    -1,   439,   106,   107,   108,   109,   110,   111,   112,
     210,    -1,    -1,    -1,   101,    -1,    -1,    95,    -1,   106,
     107,   108,   109,   110,   111,   112,   129,    -1,    -1,   101,
    1258,  1259,   104,   105,   106,   107,   108,   109,   110,   111,
     112,    -1,   129,   716,  1272,  1273,   149,   150,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   255,    -1,   160,   161,   732,
     733,  1508,   149,   150,    -1,   265,    -1,   740,    -1,  1859,
      -1,    -1,   150,   160,   161,    -1,   276,  1305,  1306,    -1,
      -1,    -1,    70,   520,   757,    -1,    -1,   760,  1352,   762,
      -1,    -1,    -1,    -1,    -1,  1838,    -1,    -1,   826,    -1,
      -1,    -1,   174,   303,    -1,    -1,   779,   780,    -1,    -1,
     310,   311,  1855,   101,    -1,   315,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   798,    -1,   800,    -1,    -1,
      -1,    12,    -1,    -1,    -1,    -1,   573,    -1,    -1,    -1,
      -1,   219,   220,    -1,    -1,    -1,   819,   347,    -1,    -1,
    1893,    -1,   352,   590,   591,   355,   884,    17,   236,    -1,
      -1,   149,   150,    -1,  1428,    -1,    -1,    -1,    -1,  1433,
      -1,    -1,    -1,    -1,  1438,  1439,  1440,   614,    12,    13,
      14,    15,    16,    -1,   857,    -1,    -1,    -1,    -1,    -1,
     627,   864,    -1,    -1,    -1,    -1,    -1,   870,   871,    59,
      60,    61,    62,    -1,    -1,    86,   879,    -1,   881,    -1,
      -1,  1578,    -1,    -1,    -1,    -1,    -1,   295,   296,   892,
     101,    -1,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,     1,    -1,    -1,     4,    70,    -1,    -1,    -1,
      -1,   101,   442,    -1,    -1,    -1,   106,   107,   108,   109,
     110,   111,   112,   113,   454,   455,    -1,    -1,    -1,   932,
    1488,  1489,    -1,    -1,    -1,   993,    -1,   101,    -1,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   716,
      -1,    -1,  1010,  1011,    -1,    -1,    -1,    -1,    -1,    58,
      -1,    -1,    -1,   153,    -1,   129,   101,    -1,    -1,   104,
     105,   106,   107,   108,   109,   110,   111,   112,    -1,    -1,
      -1,    -1,    -1,    82,    -1,   149,   150,    -1,    -1,   153,
     757,    -1,    -1,    -1,   997,    -1,   160,   161,    -1,    98,
      -1,    -1,    -1,   102,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1014,    -1,    -1,    -1,    -1,    -1,    -1,  1021,    -1,
      -1,    -1,    -1,    -1,  1618,    -1,  1620,    -1,    -1,    -1,
      12,    13,    14,    15,    16,   443,    -1,  1595,    -1,   174,
     139,  1341,    -1,    -1,  1344,  1345,   145,    -1,   147,    -1,
    1350,    -1,   151,    -1,  1354,   585,  1356,    -1,    -1,    -1,
    1063,    -1,   161,   162,   163,    -1,    -1,  1625,    -1,    -1,
      -1,    -1,  1630,  1631,    -1,   483,    -1,    -1,    -1,    -1,
     179,    -1,    -1,    -1,   492,    -1,    -1,    -1,    70,    -1,
      -1,    -1,   191,   192,    -1,    -1,   195,    -1,    -1,    -1,
    1103,    -1,    -1,   633,   512,  1802,    -1,    -1,  1111,    -1,
     518,    -1,   879,    -1,    -1,   882,    -1,    -1,    -1,   101,
      -1,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,    -1,    -1,    -1,    -1,    -1,    -1,  1140,    -1,    -1,
      -1,    -1,    -1,   242,    -1,    -1,   245,   129,    -1,    -1,
     558,    -1,    -1,    -1,  1157,  1158,    -1,    -1,    -1,   258,
      -1,   569,    -1,   571,    -1,   932,    -1,   149,   150,  1172,
      -1,   153,    -1,    -1,    -1,  1178,   275,    -1,   160,   161,
      -1,    -1,  1185,    -1,   283,    -1,    -1,    -1,    -1,   288,
      -1,    17,    -1,    -1,  1494,   294,    -1,    -1,    -1,    -1,
    1258,  1259,    -1,   302,    -1,    -1,    -1,    -1,    -1,  1212,
      -1,    -1,    -1,  1216,  1272,  1273,    63,    64,    65,    66,
      -1,    -1,   321,    -1,   323,   324,  1526,    -1,    82,    55,
      56,    57,    58,    59,    60,    61,    62,    -1,  1538,   338,
      -1,    -1,    -1,   342,   774,  1545,    -1,  1305,  1306,    -1,
      -1,   781,    76,    -1,   101,    -1,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1573,  1277,  1278,   375,   101,    -1,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,    -1,
      -1,    -1,   101,   147,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,    -1,   153,    -1,   162,    -1,
     409,    -1,  1315,  1316,  1317,  1318,  1319,  1320,    -1,    -1,
      -1,    -1,  1325,  1326,   171,   179,   856,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   435,    -1,   192,    -1,
     439,  1344,  1345,   152,    -1,    -1,   445,    -1,   157,    -1,
     174,    -1,   760,    -1,   762,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,  1926,  1113,
      -1,  1374,    -1,    -1,    -1,    -1,    -1,   907,    -1,    -1,
      -1,  1681,   912,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1393,   245,   800,  1693,  1694,   101,  1696,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,    -1,    -1,    -1,
      -1,   819,    -1,  1416,    -1,   514,   515,    -1,  1185,    -1,
     519,   520,    -1,   129,    -1,    -1,    -1,    -1,    -1,   101,
    1488,  1489,   104,   105,   106,   107,   108,   109,   110,   111,
     112,    -1,    -1,   149,   150,  1212,    -1,   153,   302,    -1,
      -1,    -1,   551,    -1,   160,   161,    -1,   556,    -1,    -1,
     559,   560,    -1,   562,    -1,    -1,  1524,   321,    -1,    -1,
      -1,    -1,    -1,    -1,   573,    -1,   575,    -1,    -1,    -1,
      -1,    -1,    -1,  1783,    -1,   157,    -1,    -1,    -1,    -1,
     589,   590,   591,    -1,   593,    -1,    -1,    -1,    -1,  1502,
      -1,    -1,    -1,  1506,    -1,  1508,    -1,    -1,    -1,  1512,
      -1,  1514,    -1,    -1,    -1,   614,    -1,    -1,   617,    -1,
      -1,    -1,   621,    -1,    -1,   624,    -1,    -1,   627,    -1,
     629,    -1,    -1,    -1,    -1,    -1,   635,  1595,    -1,  1542,
    1543,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1848,   648,
    1850,   650,   651,    -1,   653,   409,    -1,    -1,    -1,    70,
     659,    -1,    -1,   662,   663,   664,    -1,  1625,    -1,    -1,
    1573,    -1,  1630,  1631,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1584,    -1,    -1,  1587,   439,  1116,  1117,  1118,    -1,
     101,    -1,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,    -1,    -1,    -1,    -1,  1014,  1610,    -1,    -1,
      -1,    -1,  1020,  1021,    -1,    -1,  1146,   716,   129,    -1,
      -1,    -1,  1922,    -1,    -1,    -1,  1393,    -1,  1372,    -1,
      -1,  1161,  1635,  1636,  1934,  1935,  1936,    -1,   149,   150,
      -1,   740,    -1,    -1,    -1,    -1,    -1,  1650,  1651,   160,
     161,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   757,    -1,
     514,   515,  1665,    -1,    -1,  1668,   520,   101,  1198,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,    -1,
     779,   780,  1685,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1696,    -1,    -1,    -1,    -1,    99,   798,
     101,    -1,   103,   104,   105,   106,   107,   108,   109,   110,
     111,   112,    -1,    -1,    -1,    -1,  1719,    -1,    -1,   573,
      -1,    -1,    -1,   157,    -1,    -1,    -1,   826,    -1,    -1,
      -1,     1,    -1,    -1,     4,   101,    -1,   591,   104,   105,
     106,   107,   108,   109,   110,   111,   112,    -1,   149,    -1,
     849,   152,   153,    -1,    -1,    -1,    -1,  1760,   857,    -1,
     614,   860,    -1,    -1,  1172,   864,  1769,    -1,    -1,    -1,
      -1,   870,   871,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     879,   635,   881,   882,   150,   884,    -1,   153,    58,    -1,
      -1,  1535,  1795,    -1,    -1,    -1,    -1,    -1,    -1,  1329,
    1544,    -1,  1332,    -1,    -1,    70,    -1,    -1,    -1,    -1,
      -1,    -1,    82,    -1,    -1,    -1,    -1,  1820,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1828,    -1,    -1,    -1,    -1,
      -1,    -1,   102,   932,    -1,  1838,   101,  1581,    -1,   104,
     105,   106,   107,   108,   109,   110,   111,   112,    -1,    -1,
      -1,    -1,  1855,  1856,    -1,  1858,  1859,    -1,    -1,  1267,
      -1,    -1,   716,    -1,   129,    -1,    -1,    -1,  1926,   139,
      -1,   101,    -1,    -1,    -1,   145,   106,   107,   108,   109,
     110,   111,   112,   113,   149,   150,   740,   117,    -1,   119,
    1893,    -1,   162,    -1,   993,   160,   161,    -1,    -1,    -1,
      -1,    -1,    -1,   757,    -1,  1908,  1909,    -1,    -1,    -1,
     180,  1010,  1011,    -1,    -1,    -1,    -1,  1325,    -1,    -1,
     150,   191,   192,   153,    -1,   779,   780,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1679,    -1,    -1,  1941,  1683,
      -1,    -1,    -1,    -1,   798,    -1,    -1,    -1,    -1,    -1,
     220,  1695,    -1,    -1,    -1,    -1,    -1,  1701,    -1,    -1,
      -1,    -1,    -1,    -1,  1063,    -1,   236,    -1,    -1,    -1,
      -1,   241,   242,    -1,    -1,   245,    -1,    -1,   101,    -1,
      -1,    -1,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,  1521,    -1,    -1,   117,    -1,   119,   267,    -1,    -1,
     270,    -1,   272,    -1,  1103,    -1,    -1,    -1,    -1,    -1,
     864,    -1,  1111,   283,    -1,    -1,    -1,   871,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   296,   150,  1795,    -1,
     153,    -1,  1776,    -1,    -1,    -1,    -1,    -1,    -1,   101,
      -1,  1140,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   321,    -1,    -1,   324,    -1,  1800,    -1,  1157,  1158,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   338,    -1,
      -1,    -1,   342,  1603,    -1,    -1,    -1,    -1,   932,  1178,
      -1,    -1,    -1,    -1,    -1,    -1,  1185,    -1,    -1,  1856,
    1189,   153,    -1,    -1,    -1,    -1,   366,    -1,  1506,  1843,
      -1,    -1,  1201,    -1,  1512,    -1,  1514,  1851,    -1,    -1,
      -1,    -1,    -1,  1212,    -1,   101,    -1,  1216,   104,   105,
     106,   107,   108,   109,   110,   111,   112,  1871,    -1,  1873,
    1874,    -1,    -1,    -1,    -1,    -1,    -1,   101,     1,    -1,
      -1,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
      -1,  1895,    -1,   117,    -1,   119,    -1,    -1,    -1,  1258,
    1259,    -1,    -1,    -1,    -1,   435,   152,    -1,    -1,   439,
      -1,    -1,    -1,  1272,  1273,  1919,    -1,    -1,  1277,  1278,
      -1,    -1,    -1,    47,    -1,  1929,   150,    -1,  1718,   153,
      -1,    -1,   462,    -1,    -1,    58,    -1,  1727,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1305,  1306,    -1,  1063,
    1954,    -1,  1956,    -1,    -1,    -1,  1315,  1316,  1317,  1318,
    1319,  1320,    -1,    -1,    -1,    -1,    -1,  1326,  1972,    -1,
      -1,    -1,    -1,    -1,  1978,    -1,    -1,    -1,    -1,   102,
      -1,    -1,    -1,    -1,  1988,  1344,  1345,    -1,    -1,    -1,
     520,    -1,    -1,    -1,    -1,   119,    -1,  1665,    -1,    -1,
    1668,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,
     134,    -1,    -1,    -1,    -1,  1374,    -1,    -1,    -1,    -1,
      -1,   551,   145,    -1,    -1,    -1,   556,    -1,    -1,    -1,
     560,    -1,   562,    -1,  1393,    -1,    -1,    -1,    -1,   162,
      -1,    -1,    -1,   573,    -1,   575,    -1,    -1,    -1,    -1,
      -1,  1719,    -1,    -1,    -1,    -1,    -1,  1416,    -1,    -1,
     590,   591,    -1,    -1,  1178,    -1,    -1,    -1,    -1,   192,
      -1,  1185,    -1,    -1,    -1,   605,    -1,    70,    -1,    -1,
      -1,    -1,    -1,    -1,   614,    -1,    -1,    -1,    -1,   619,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   627,  1212,    -1,
      -1,  1769,   226,   227,    -1,    -1,   230,    -1,   101,   233,
     234,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1488,
    1489,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   267,    -1,    -1,    -1,    -1,  1508,
      -1,    -1,    -1,    -1,    -1,    -1,   149,   150,    -1,    -1,
     283,    -1,    -1,  1277,  1278,  1524,   101,   160,   161,   104,
     105,   106,   107,   108,   109,   110,   111,   112,    -1,    -1,
      -1,    -1,    -1,  1542,  1543,    -1,   716,    -1,    -1,    -1,
    1858,    -1,    -1,    -1,    -1,   319,    -1,    -1,   321,    -1,
      -1,   324,    -1,   733,    -1,    -1,    -1,    -1,    -1,    -1,
     334,    -1,    -1,    -1,  1573,   338,    -1,   152,    -1,   342,
      -1,     1,    -1,    -1,    -1,  1584,    -1,   757,  1587,    -1,
      -1,    -1,   762,    -1,    -1,    -1,  1595,    -1,    -1,    -1,
    1908,  1909,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   779,
     780,  1610,   101,    -1,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,    -1,  1625,    -1,   798,    -1,
      -1,  1630,  1631,  1941,    -1,    -1,  1635,  1636,    58,  1393,
     129,    12,    13,    14,    15,    16,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   826,    -1,    -1,    -1,
     149,   150,    -1,    -1,   428,     1,    -1,   156,     4,    -1,
      -1,   160,   161,    -1,    -1,    -1,   439,    -1,    -1,    -1,
      -1,    -1,   102,    -1,    -1,    -1,  1685,   857,    -1,    -1,
      -1,    -1,    -1,    -1,   864,    -1,    -1,  1696,    -1,    70,
     870,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   879,
      -1,    -1,   882,    -1,   884,    -1,    -1,    -1,    -1,   889,
      -1,    -1,    58,    -1,    -1,   145,    -1,    -1,    -1,    -1,
     101,   495,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   162,    -1,    -1,    -1,    82,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1508,    -1,    -1,   520,   129,    -1,
      -1,    -1,   932,    -1,    -1,    -1,   102,    -1,    -1,    -1,
      -1,    -1,   192,    -1,    -1,    -1,    -1,    -1,   149,   150,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   551,   160,
     161,    12,    13,    14,    15,    16,  1795,   560,    -1,   562,
      -1,    -1,    -1,   139,    -1,    -1,    -1,    -1,    -1,   145,
     573,   147,   575,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1820,    -1,   993,    -1,    -1,    -1,   590,   591,    -1,
    1584,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1838,
    1010,  1011,    -1,   179,    -1,   609,   610,    -1,    -1,    70,
      -1,   614,    -1,    -1,    -1,   191,  1855,  1856,   622,    -1,
    1859,    -1,   101,   283,   627,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,    -1,    -1,    -1,    -1,    -1,
     101,  1635,  1636,   104,   105,   106,   107,   108,   109,   110,
     111,   112,    -1,  1063,  1893,    -1,    -1,    -1,    -1,    -1,
      -1,   321,    -1,    -1,   324,    -1,   242,    -1,   129,   245,
     149,    -1,    -1,    -1,   250,    -1,    -1,    -1,   338,    -1,
      -1,    -1,   342,    -1,    -1,    -1,    -1,  1926,   149,   150,
      -1,    -1,    -1,  1103,    12,    13,    14,    15,    16,   160,
     161,  1111,    -1,    -1,    -1,    -1,    -1,   283,    -1,    -1,
      -1,    -1,   101,   716,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,    -1,   302,    -1,    -1,    -1,
    1140,    -1,   736,   737,    -1,    -1,    -1,    -1,   742,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1158,    -1,
      -1,    -1,    70,    -1,   757,    -1,    -1,    -1,    -1,   763,
     149,   150,   766,   767,   153,   769,    -1,   771,   772,    -1,
      -1,   160,   161,    -1,    -1,  1185,   779,   780,    -1,   439,
      -1,    -1,    -1,   101,  1194,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   798,    -1,    -1,    -1,   375,
      -1,    -1,  1212,    -1,    -1,    -1,   810,    -1,    -1,   101,
     814,   129,   104,   105,   106,   107,   108,   109,   110,   111,
     112,    -1,    -1,   826,    -1,    -1,  1820,    -1,    -1,    -1,
      -1,   149,   150,   409,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   160,   161,    -1,    -1,    -1,    -1,  1258,  1259,
      -1,    -1,    -1,    -1,   857,    -1,    -1,   149,    -1,   435,
     520,   864,  1272,  1273,    -1,    -1,    -1,  1277,  1278,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   879,    -1,    -1,   882,
      -1,   884,    -1,    -1,    -1,    -1,   890,    -1,    -1,    -1,
      -1,   551,    -1,    -1,    -1,  1305,  1306,    -1,    -1,    -1,
     560,    -1,   562,    -1,    -1,  1315,  1316,  1317,  1318,    -1,
      -1,    -1,    -1,   573,    -1,   575,   101,    -1,    -1,   104,
     105,   106,   107,   108,   109,   110,   111,   112,    -1,   932,
     590,   591,    -1,    -1,  1344,  1345,    -1,    -1,   514,   515,
      -1,    -1,   101,    -1,   129,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   614,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   149,   150,    -1,   627,   153,    -1,
      -1,    -1,    -1,    -1,    -1,   160,   161,    -1,    -1,    -1,
     556,    -1,    -1,  1393,   560,    -1,   562,    -1,   173,    -1,
     993,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   575,
      -1,     3,    -1,    -1,    -1,    -1,  1416,  1010,  1011,    -1,
      12,    13,    14,    15,    16,    -1,  1020,    19,    -1,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    -1,    49,    50,    51,
      -1,    53,    -1,    -1,    -1,    -1,   716,    -1,    -1,   635,
    1063,    -1,    -1,    -1,  1068,    -1,    -1,  1071,    70,    -1,
      -1,    -1,   648,    -1,   650,   651,    -1,   653,  1488,  1489,
      -1,    -1,    -1,   659,    -1,    -1,   662,   663,   664,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   757,    -1,    -1,
      -1,    -1,   104,   105,  1514,    -1,    -1,    -1,  1111,    -1,
      -1,    -1,    -1,    -1,  1524,    -1,    -1,    -1,    -1,   779,
     780,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,
      -1,    -1,    -1,  1543,    -1,    -1,    -1,  1140,   798,   132,
     133,   134,   135,   136,   137,   138,   139,   140,   141,   142,
     152,   153,    -1,   146,    -1,  1158,    -1,    -1,   160,   161,
      -1,    -1,    -1,  1573,   740,    -1,   826,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1584,    -1,    -1,  1587,    -1,    -1,
     173,    -1,  1185,    -1,    -1,  1595,    -1,  1191,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1199,  1200,   857,    -1,    -1,
      -1,    -1,    -1,    -1,   864,    -1,    -1,    -1,    -1,  1212,
      -1,    -1,    -1,    -1,    -1,  1625,    -1,    -1,    -1,   879,
    1630,  1631,   882,    -1,   884,  1635,  1636,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     1,    -1,    -1,     4,    -1,
      -1,  1651,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1254,    -1,    -1,    -1,    -1,  1258,  1259,    -1,    -1,  1263,
      -1,    -1,  1266,    -1,  1268,  1269,    -1,    -1,    -1,  1272,
    1273,    -1,   932,    -1,  1277,  1278,    -1,    -1,    -1,    -1,
      -1,   857,    -1,    -1,    -1,    -1,  1696,    -1,    -1,    -1,
      -1,    -1,    58,    -1,   870,   871,    -1,    -1,    -1,    -1,
      -1,    -1,  1305,  1306,  1308,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   101,    82,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,    -1,    -1,    -1,
      -1,    -1,    -1,   993,    -1,    -1,   102,    -1,    -1,    -1,
      -1,  1344,  1345,   129,    -1,    -1,    -1,    -1,    -1,   101,
    1010,  1011,   104,   105,   106,   107,   108,   109,   110,   111,
     112,    -1,    -1,   149,   150,    -1,    -1,    -1,    -1,    -1,
      -1,  1375,    -1,   139,   160,   161,    -1,   129,    -1,   145,
      -1,   147,    -1,    -1,    -1,  1795,    -1,    -1,    -1,    -1,
    1393,    -1,    -1,    -1,    -1,    -1,    -1,   149,   150,    86,
      -1,    -1,    -1,  1063,    -1,    92,    93,    -1,   160,   161,
    1820,    -1,    -1,   179,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   191,    -1,    -1,  1838,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   126,
      -1,    -1,    -1,    -1,    -1,  1855,  1856,    -1,    -1,    -1,
      -1,  1111,    -1,  1457,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   242,    -1,    -1,   245,
    1140,    -1,  1486,  1893,   250,  1488,  1489,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1158,  1909,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1926,   283,    -1,    -1,
      -1,  1524,    -1,    -1,    -1,  1185,    -1,  1103,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1111,   302,    -1,    -1,    -1,
    1543,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1212,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1140,    -1,    -1,    -1,    -1,    -1,
    1573,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1583,
      -1,  1584,  1158,    -1,    -1,    -1,    -1,     0,    -1,    -1,
       3,    -1,  1595,    -1,    -1,    -1,    -1,    -1,  1258,  1259,
      -1,    -1,  1178,    -1,    -1,   292,    -1,    -1,    -1,   375,
      -1,    -1,  1272,  1273,    -1,    -1,    -1,  1277,  1278,    -1,
      -1,    -1,  1625,    -1,    -1,    -1,    -1,  1630,  1631,    -1,
      -1,    -1,  1635,  1636,    -1,    -1,    -1,    -1,    -1,    -1,
    1216,    -1,    -1,   409,    -1,  1305,  1306,    -1,    -1,    -1,
      -1,    -1,    -1,  1657,  1658,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    76,    -1,  1669,    -1,    -1,    -1,   435,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1344,  1345,    -1,    -1,    12,    13,
      14,    15,    16,  1696,    -1,    19,    -1,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,   135,    -1,    -1,    -1,    50,    51,    -1,    -1,
      -1,    -1,    -1,  1393,    -1,    -1,    -1,    -1,    -1,  1315,
    1316,  1317,  1318,  1319,  1320,    -1,    70,    -1,   514,   515,
      -1,   438,    -1,   440,    -1,    -1,    -1,    -1,  1762,    -1,
      -1,    -1,   449,   450,    -1,    -1,    -1,    -1,  1344,  1345,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     556,    -1,  1795,    -1,   560,    -1,   562,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   575,
      -1,    -1,    -1,    -1,    -1,    -1,   229,  1820,    -1,    -1,
    1824,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1488,  1489,
      -1,   244,   156,    -1,    -1,  1838,    -1,    -1,    -1,    -1,
    1416,   254,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   264,  1855,  1856,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1524,   278,   279,    -1,    -1,   635,
     557,    -1,   285,   286,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   648,  1543,   650,   651,    -1,   653,   301,    -1,
    1893,    -1,    -1,   659,    -1,    -1,   662,   663,   664,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   320,    -1,    -1,
      -1,    -1,    -1,  1573,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1926,  1584,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1508,    -1,    -1,  1595,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   376,    -1,  1625,    -1,  1543,    -1,    -1,
    1630,  1631,    -1,    -1,   740,  1635,  1636,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   407,    -1,    -1,  1573,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1587,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     433,    -1,    -1,    -1,   437,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1610,    -1,  1696,    -1,    -1,    -1,
      -1,    -1,    -1,   456,    -1,    -1,    -1,   460,   461,    -1,
      -1,   464,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   479,   480,   481,   482,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   498,   773,    -1,    -1,    -1,
      -1,   857,    -1,   506,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   870,   871,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1696,   534,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1795,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   565,    -1,    -1,    -1,    -1,    -1,    -1,   572,
    1820,    -1,    -1,   850,   851,   578,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   861,   862,   863,    -1,  1838,   866,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   601,   602,
      12,    13,    14,    15,    16,  1855,  1856,    19,    -1,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    -1,    49,    50,    51,
      -1,    53,    -1,  1893,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,
      -1,    -1,   665,    -1,    47,    -1,    -1,    -1,    -1,   946,
      -1,    -1,  1838,    -1,    -1,    -1,  1926,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1855,
      73,    -1,   104,   105,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   990,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1893,    -1,   732,
      -1,    -1,    -1,    -1,    -1,    -1,   119,    -1,    -1,    -1,
     152,    -1,    -1,    -1,   747,    -1,    -1,  1103,   751,   132,
      -1,   134,    -1,    -1,    -1,  1111,    -1,   760,    -1,    -1,
    1037,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1046,
    1047,  1048,  1049,    -1,    -1,    -1,    -1,  1054,  1055,   782,
      -1,   164,    -1,    -1,  1140,    -1,    -1,  1064,   791,    -1,
      -1,    -1,    -1,    -1,   797,    -1,    -1,    -1,   181,    -1,
      -1,    -1,  1158,    -1,    -1,    -1,    -1,    -1,  1085,    -1,
    1087,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1178,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   834,    -1,    -1,    -1,    -1,   219,    -1,   841,    -1,
     223,    -1,    -1,   226,   227,    -1,    -1,   230,    -1,    -1,
     233,   234,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1216,    -1,    -1,  1140,    -1,   868,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   178,  1166,
      -1,    -1,    -1,    -1,    -1,    -1,  1173,    -1,  1175,  1176,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1184,    -1,  1186,
      -1,  1188,   295,  1190,    -1,   298,    -1,    -1,  1195,   922,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   319,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   334,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1315,
    1316,  1317,  1318,  1319,  1320,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1253,    -1,    -1,    -1,
      -1,    -1,    -1,  1260,  1261,    -1,    -1,    -1,  1344,  1345,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1003,    -1,    -1,    -1,  1007,    -1,    -1,  1284,    -1,    -1,
      -1,  1014,    -1,    -1,  1291,    -1,    -1,  1294,    -1,    -1,
      -1,  1024,    -1,    -1,    -1,    -1,    -1,    -1,  1031,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1040,    -1,  1042,
      -1,    -1,    -1,    -1,    -1,   428,    -1,  1324,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1416,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     360,  1074,    -1,   363,   364,  1078,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   373,   374,    -1,    -1,  1364,    -1,  1092,
      -1,    -1,  1095,    -1,    -1,    -1,    -1,    -1,   388,   389,
     483,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   495,    -1,    98,    -1,    -1,    -1,    -1,   409,
    1397,    -1,    -1,    -1,    -1,   109,    -1,   111,  1405,   113,
    1407,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   439,
      -1,    -1,  1508,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,    -1,   153,
     154,    -1,    -1,    -1,    -1,    -1,  1453,  1454,    -1,    -1,
    1183,    -1,    -1,    -1,    -1,    -1,   569,  1543,    -1,    -1,
      -1,  1468,  1469,    -1,  1471,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1480,  1207,    -1,    -1,    -1,    -1,    -1,
      -1,   195,    -1,  1490,  1491,    -1,    -1,  1573,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   609,   610,    -1,    -1,
      -1,  1587,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   622,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1610,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   258,    -1,   260,   261,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1297,    -1,    -1,    -1,  1301,    -1,
      -1,    -1,    -1,    -1,   288,    -1,    -1,    -1,    -1,    -1,
     294,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1596,
    1597,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1331,  1606,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   323,
    1696,    -1,    -1,    -1,    -1,   329,    -1,   331,    -1,    -1,
      -1,    -1,    -1,   736,   737,    -1,    -1,    -1,    -1,   742,
      -1,    -1,    -1,    -1,  1641,  1642,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1381,    -1,
     763,  1384,    -1,   766,   767,    -1,   769,    -1,   771,   772,
      -1,    -1,    -1,    -1,    -1,    -1,  1399,    -1,    -1,    -1,
      -1,    -1,    -1,   693,   694,   695,   696,   697,   698,   699,
     700,   701,   702,   703,   704,   705,   706,   707,   708,   709,
     710,   711,    -1,    -1,    -1,    -1,    -1,   810,    -1,    -1,
      -1,   814,    -1,    -1,    -1,   419,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1721,    -1,  1449,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1458,    -1,    -1,    -1,  1462,
      -1,   445,  1739,   447,   448,  1742,  1743,    -1,    -1,    -1,
      -1,    -1,  1749,  1476,  1477,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1838,   773,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1855,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   890,   492,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   512,    -1,
      -1,    -1,    -1,   517,    -1,   519,    -1,  1893,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   539,    -1,   541,   542,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   559,    -1,    -1,    -1,    -1,
    1857,    -1,    -1,    -1,    -1,  1588,  1589,   571,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   593,
      -1,   595,   596,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1906,
      -1,    -1,    -1,   617,   618,    -1,    -1,  1020,   928,    -1,
     624,    -1,    -1,   933,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1930,   944,    -1,    -1,    -1,    -1,    -1,
    1937,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   657,   658,  1952,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1068,    -1,    -1,  1071,    -1,
      -1,    -1,    -1,    -1,   984,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1706,    -1,    -1,    -1,    -1,    -1,   162,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1729,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   191,   192,
      -1,    -1,    -1,    -1,  1747,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     223,  1774,    -1,    -1,    -1,    -1,    -1,   230,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1791,    -1,
      -1,  1794,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1191,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1199,  1200,    -1,    -1,
      -1,  1111,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   298,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   849,    -1,    -1,   321,   322,
      -1,  1254,    -1,    -1,    -1,    -1,   860,    -1,    -1,  1882,
    1263,    -1,    -1,  1266,    -1,  1268,  1269,    -1,    -1,   342,
      -1,    -1,    -1,    -1,    -1,  1185,    -1,   881,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   892,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   901,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1308,    -1,    -1,    -1,  1219,
    1220,  1221,    -1,    -1,    -1,    -1,  1226,  1227,    -1,    -1,
       5,    -1,    -1,    -1,    -1,    -1,    -1,    12,    13,    14,
      15,    16,    -1,    -1,    -1,    -1,    -1,    -1,  1248,   412,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   428,   429,    -1,   431,   432,
      -1,    -1,    -1,    48,    -1,    -1,   439,    52,    -1,    54,
     443,    -1,  1375,    -1,    -1,  1285,  1286,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,   997,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   484,    -1,    98,    99,   488,   101,  1021,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,   117,   118,   119,    -1,   121,   122,    -1,    -1,
      -1,    -1,    -1,    -1,   129,    -1,    -1,   520,    -1,    -1,
      -1,    -1,    -1,    -1,  1457,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   149,    -1,    -1,   152,   153,    -1,
      -1,    82,    -1,    -1,   159,   160,   161,   162,   163,   164,
     165,    -1,    -1,  1486,    -1,    -1,    -1,    98,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   570,    -1,    -1,
     573,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1512,
      -1,    -1,    -1,    -1,    -1,  1518,    -1,   590,   591,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   601,    -1,
      -1,    -1,   605,    -1,    -1,    -1,   147,    -1,    -1,   612,
     151,   614,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   162,    -1,  1157,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   179,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1583,   192,    -1,    -1,   195,  1189,    12,    13,    14,    15,
      16,  1195,    -1,    19,    -1,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      -1,    -1,    -1,    -1,    50,    51,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   245,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   716,    70,    -1,    -1,   258,    -1,    -1,
      -1,    -1,    -1,    -1,  1657,  1658,    -1,    -1,    -1,   732,
     733,    -1,  1665,    -1,    -1,    -1,  1669,    -1,  1578,   742,
     743,    -1,   745,   746,    -1,   101,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   294,   757,    -1,    -1,   760,    -1,   762,
     763,   302,    -1,    -1,    -1,    -1,   769,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   779,   780,    -1,    -1,
     321,    -1,   323,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1326,    -1,    -1,   798,    -1,    -1,    -1,   802,
      -1,    -1,    -1,   806,    -1,     1,    -1,   810,   811,    -1,
      -1,   814,   815,    -1,    -1,    -1,    -1,    -1,    -1,   822,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1762,
      -1,    -1,    -1,    -1,   375,    -1,    -1,    -1,    -1,    -1,
    1374,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    48,    -1,    -1,    -1,    52,    -1,    54,    -1,
      -1,   864,   865,    -1,    -1,    -1,   179,    -1,   409,    -1,
      -1,    -1,    -1,    -1,    -1,    71,    -1,    -1,    -1,   192,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   892,
      -1,  1824,   205,    -1,   207,    -1,    -1,    -1,   439,    -1,
      -1,    -1,    98,    99,   445,   101,    -1,   103,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
      -1,   117,   118,   119,    -1,   121,   122,    -1,    -1,   932,
      -1,    -1,  1772,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   149,    -1,    -1,   152,   153,    -1,    -1,
      -1,    -1,  1802,   159,   160,   161,   162,   163,   164,   165,
      -1,    -1,    -1,   514,   515,  1908,    -1,    -1,    -1,   520,
     293,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   997,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1542,    -1,
      -1,  1014,  1015,    -1,    -1,    -1,    -1,    -1,  1021,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   573,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1887,    -1,  1889,
     591,    -1,   593,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1063,    -1,    -1,    -1,    -1,  1068,  1069,    -1,  1071,  1072,
      -1,    -1,    -1,   614,    -1,  1915,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   635,    -1,    -1,    -1,    -1,  1939,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   648,    -1,   650,
     651,    -1,   653,    -1,    -1,    -1,    -1,    -1,   659,    -1,
      -1,   662,   663,   664,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1668,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1984,    -1,    -1,    -1,    -1,    -1,
      -1,  1685,   465,    -1,    -1,    -1,    -1,    -1,   471,    -1,
      -1,    -1,    -1,   476,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   716,    -1,    -1,    -1,    -1,
      -1,    -1,  1185,    -1,    -1,    -1,    -1,    -1,  1191,  1192,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   740,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1212,
      -1,    -1,    -1,    -1,    -1,    -1,   757,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1760,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   779,   780,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     563,  1254,  1255,    -1,    -1,    -1,    -1,   798,    -1,    -1,
    1263,  1264,    -1,  1266,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1277,  1278,    -1,    -1,   591,    -1,
      -1,    -1,    -1,    12,    13,    14,    15,    16,    -1,    -1,
      19,   604,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    -1,
      49,    50,    51,   864,    53,  1859,    -1,    -1,    -1,    -1,
     871,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     881,    70,   655,    -1,    -1,    -1,    -1,    -1,    -1,     5,
      -1,    -1,    -1,    -1,    -1,    -1,    12,    13,    14,    15,
      16,    -1,    -1,   676,   677,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   686,    -1,   688,   689,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1393,   932,    48,    -1,    -1,    -1,    52,    -1,    54,    -1,
      -1,    -1,    -1,   716,    -1,    -1,    -1,  1941,    -1,    -1,
      -1,    -1,    -1,    -1,    70,    71,   729,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   740,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   754,    98,    99,   757,   101,    -1,   103,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
      -1,   117,   118,   119,    -1,   121,   122,    -1,    -1,    -1,
      -1,   784,    -1,   129,   787,    -1,    -1,    -1,    48,    -1,
      -1,    -1,    52,    -1,    54,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   149,    -1,    -1,   152,   153,    -1,  1502,
      -1,    71,    -1,   159,   160,   161,   162,   163,   164,   165,
     823,    -1,    -1,    -1,    -1,  1518,    -1,    -1,    -1,    -1,
      -1,    -1,  1063,    -1,    -1,    -1,    -1,    -1,    98,    99,
      -1,   101,    -1,   103,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,    -1,   117,   118,   119,
      -1,   121,   122,    -1,    -1,    -1,    -1,    -1,   871,   129,
      -1,    -1,  1103,    -1,    -1,    -1,    -1,    -1,   881,   882,
      -1,    -1,    -1,    -1,    -1,    -1,   889,    -1,    -1,   149,
      -1,  1584,   152,   153,    -1,    -1,    -1,    -1,    -1,   159,
     160,   161,   162,   163,   164,   165,    -1,   910,    -1,    -1,
      -1,    -1,    -1,   173,   174,    -1,   919,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1157,    -1,    -1,   932,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   940,    -1,    -1,
      -1,    -1,  1635,  1636,   947,    -1,    -1,  1178,    -1,    -1,
      -1,    -1,    -1,    -1,  1185,    -1,    -1,  1650,  1651,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1666,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1212,    -1,    -1,    -1,  1216,    -1,    -1,   991,    -1,
      -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    -1,
      -1,    19,    -1,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      -1,    49,    50,    51,    -1,    53,  1277,  1278,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1058,    -1,  1060,    -1,  1062,
      -1,    -1,    70,    -1,    -1,    -1,    -1,  1760,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1768,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1315,  1316,  1317,    -1,  1319,  1320,
      -1,    -1,    -1,   101,    -1,  1326,   104,   105,   106,   107,
     108,   109,   110,   111,   112,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   129,    -1,    -1,    -1,  1128,  1129,  1820,    -1,    -1,
      -1,  1824,  1825,    -1,    -1,  1828,    -1,    -1,    -1,    -1,
      -1,   149,   150,  1374,   152,   153,    -1,    -1,    -1,   157,
      -1,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1393,    -1,    -1,  1858,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1189,    -1,    -1,    -1,
      -1,    -1,  1195,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    48,    -1,    -1,    -1,    52,    -1,    54,    -1,  1212,
      -1,    -1,    -1,    -1,    -1,  1908,  1909,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    71,    -1,  1229,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1242,
      -1,    -1,  1245,    -1,    -1,    -1,    -1,    -1,  1941,    -1,
      -1,    98,    99,    -1,   101,    -1,   103,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,    -1,
     117,   118,   119,    -1,   121,   122,    -1,  1508,    -1,    -1,
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1296,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   149,   150,    -1,   152,   153,    -1,    -1,    -1,
     157,  1542,   159,   160,   161,   162,   163,   164,   165,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1334,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1352,
      -1,    -1,  1355,  1584,    -1,    -1,    -1,    -1,    -1,    -1,
       1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    17,    -1,    -1,  1610,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1393,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1403,  1404,    -1,    -1,  1635,  1636,    -1,    48,    -1,    -1,
      -1,    52,    -1,    54,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1428,    -1,  1430,    69,    -1,
      71,    72,    -1,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    90,
      91,    92,    93,    94,  1685,    96,    -1,    98,    99,    -1,
     101,    -1,   103,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,    48,
     121,   122,    -1,    52,    -1,    54,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    71,    -1,    -1,  1508,    -1,   148,   149,    -1,
    1513,   152,   153,    -1,    -1,    -1,   157,    -1,   159,   160,
     161,   162,   163,   164,   165,    -1,    -1,    -1,    -1,    98,
      99,    -1,   101,   174,   103,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,    -1,   117,   118,
     119,    -1,   121,   122,    -1,    -1,     1,    -1,    -1,    -1,
     129,    -1,    -1,    -1,  1795,    -1,  1569,    -1,    -1,    -1,
      -1,    -1,    17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     149,   150,    -1,   152,   153,    -1,    -1,    -1,   157,  1820,
     159,   160,   161,   162,   163,   164,   165,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    -1,  1608,    -1,    52,  1611,    54,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    69,  1856,    71,    72,  1859,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      -1,    96,    -1,    98,    99,    -1,   101,    -1,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,     1,   121,   122,    -1,    -1,
      -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   148,   149,    -1,    -1,   152,   153,    -1,
      -1,    -1,   157,    -1,   159,   160,   161,   162,   163,   164,
     165,    -1,    48,    -1,    -1,    -1,    52,    -1,    54,   174,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    69,    -1,    71,    72,    -1,    74,    -1,
      -1,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    91,    92,    93,    94,    -1,
      96,    -1,    98,    99,    -1,   101,    -1,   103,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,    -1,   121,   122,    -1,    -1,    -1,
    1803,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   149,    -1,    -1,   152,   153,    -1,    -1,
      -1,   157,    -1,   159,   160,   161,   162,   163,   164,   165,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   174,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    -1,    19,    -1,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    -1,    -1,    48,    -1,    50,    51,    52,    -1,
      54,    55,    56,    57,    58,    59,    60,    61,    62,    63,
      -1,    -1,    -1,    67,    -1,    69,    70,    71,    72,    -1,
      74,    -1,    -1,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,    87,    88,    89,    90,    91,    92,    93,
      94,    -1,    96,    -1,    98,    99,   100,   101,    -1,   103,
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
      92,    93,    94,    -1,    96,    -1,    98,    99,   100,   101,
      -1,   103,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,    -1,   121,
     122,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,    -1,    -1,
     152,   153,    -1,    -1,    -1,   157,    -1,   159,   160,   161,
     162,   163,   164,   165,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   174,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    -1,    19,
      -1,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    -1,    -1,    48,    -1,
      50,    51,    52,    -1,    54,    55,    56,    57,    58,    59,
      60,    61,    62,    63,    -1,    -1,    -1,    67,    -1,    -1,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    98,    99,
     100,   101,    -1,   103,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,    -1,   117,   118,   119,
      -1,   121,   122,    -1,    -1,    -1,    -1,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,
      -1,   151,   152,   153,    -1,    -1,    -1,    -1,    -1,   159,
     160,   161,   162,   163,   164,   165,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   173,   174,     3,     4,     5,     6,     7,
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
      -1,    -1,    -1,   149,    -1,   151,   152,   153,    -1,    -1,
      -1,    -1,    -1,   159,   160,   161,   162,   163,   164,   165,
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
     165,     1,    -1,     3,     4,     5,     6,     7,     8,     9,
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
     160,   161,     1,    -1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    -1,
      19,    -1,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    -1,
      49,    50,    51,    -1,    53,    -1,    55,    56,    57,    58,
      59,    60,    61,    62,    63,    -1,    -1,    -1,    67,    -1,
      -1,    70,    -1,    -1,    -1,    -1,    75,    76,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   100,    -1,    -1,    -1,   104,   105,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   148,
      -1,    -1,    -1,   152,   153,    -1,    -1,    -1,    -1,    -1,
      -1,   160,   161,     3,     4,     5,     6,     7,     8,     9,
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
      13,    14,    15,    16,    -1,    -1,    19,    -1,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    -1,    49,    50,    51,    -1,
      53,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    12,    13,    14,    15,    16,    -1,    70,    19,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    -1,    -1,    -1,   101,    50,
      51,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,
      -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   149,   150,    -1,   152,
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
      -1,   152,   153,    -1,    -1,    -1,    -1,    -1,    -1,   160,
     161,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    -1,    -1,    19,    -1,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    -1,    49,    50,    51,
      -1,    53,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      12,    13,    14,    15,    16,    -1,    -1,    19,    70,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    -1,    49,    50,    51,
      -1,    53,   104,   105,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   149,   150,    -1,   152,   153,
      -1,    -1,    -1,   157,    -1,    -1,   160,   161,     4,     5,
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
      -1,    -1,    -1,    -1,    17,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   149,   150,    48,   152,   153,    -1,    52,
      -1,    54,    -1,    -1,   160,   161,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    69,    -1,    71,    -1,
      -1,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,    87,    88,    89,    90,    91,    92,
      93,    94,    -1,    96,    -1,    98,    99,    -1,   101,    -1,
     103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,    -1,   121,   122,
      -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   149,    -1,    -1,   152,
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
      -1,    -1,    -1,    -1,   149,    -1,    -1,   152,   153,    -1,
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
      -1,    -1,   152,   153,    -1,    12,    13,    14,    15,    16,
     160,   161,    19,    -1,    21,    22,    23,    24,    25,    26,
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
      -1,    -1,    -1,   160,   161,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      -1,    19,    -1,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    -1,    -1,
      -1,    -1,    50,    51,    -1,    -1,    -1,    55,    56,    57,
      58,    59,    60,    61,    62,    -1,    12,    13,    14,    15,
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
      -1,    -1,    76,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   151,   152,   153,    -1,    -1,
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
      -1,    -1,   160,   161,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    -1,    -1,
      19,    -1,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    -1,
      49,    50,    51,    -1,    53,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    12,    13,    14,    15,    16,
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
      -1,    -1,    12,    13,    14,    15,    16,   160,   161,    19,
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
      -1,    -1,   152,   153,   104,   105,    -1,    -1,    -1,    -1,
     160,   161,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   152,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   131,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,
     152,     4,     5,     6,     7,     8,     9,    10,    11,    12,
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
      -1,    -1,    -1,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    -1,    19,   152,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    -1,    -1,    -1,    -1,    50,
      51,    -1,    -1,    -1,    55,    56,    57,    58,    59,    60,
      61,    62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,   104,   105,    19,    -1,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    -1,    49,    50,    51,    -1,    53,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   152,    -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     104,   105,    -1,    -1,    -1,    -1,    12,    13,    14,    15,
      16,    17,    -1,    19,    -1,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      -1,    -1,    -1,    -1,    50,    51,    -1,    -1,   152,    55,
      56,    57,    58,    59,    60,    61,    62,    48,    -1,    -1,
      -1,    52,    -1,    54,    70,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,    98,    99,    -1,
     101,    -1,   103,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,   117,   118,   119,    48,
     121,   122,    -1,    52,    -1,    54,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   152,    -1,    -1,    -1,
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
     161,   162,   163,   164,   165
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
     158,   158,   118,   120,   121,   122,   149,   152,   153,   157,
     158,   198,   198,   166,   160,   167,   168,   162,   163,   123,
     124,   125,   126,   169,   170,   127,   128,   161,   159,   171,
     129,   130,   172,   151,   155,   152,   176,   132,   133,   134,
     135,   136,   137,   138,   139,   140,   141,   142,   173,   216,
     217,   218,   149,   196,   435,   436,   437,   438,   439,   151,
     155,   151,   151,   151,   151,   151,   151,   149,   402,   439,
     440,   149,   439,   440,   176,   292,   458,   176,   177,   177,
     149,   161,   196,   408,   426,   427,   428,   429,   430,   431,
     432,   433,   434,   131,   460,   177,   177,   357,   357,   176,
     176,   176,   153,   181,   176,   362,   156,   155,   462,   361,
     152,   153,   156,   365,   150,   214,   220,   149,   176,   176,
     176,   176,   408,   410,   411,   412,   421,   423,   424,   425,
     151,   151,   151,   151,   151,   151,   151,   409,   422,   402,
     149,   360,   154,   176,   225,   397,   176,   225,   399,   221,
     359,   221,   359,   399,   388,   225,   397,   401,   157,   397,
     272,   388,   225,   397,   324,   325,   323,   157,   131,   295,
     350,   351,   354,   355,   151,   155,    68,   274,   275,   177,
     295,   288,   160,   214,   176,   408,   349,   390,   388,   154,
     176,   149,   370,   368,   369,    76,   305,   180,   293,   440,
     453,   295,   299,   460,   176,   442,   443,   444,   154,   176,
      17,   214,   295,   441,   463,   402,   402,   440,   293,   451,
     461,   295,   180,   402,   293,   453,   317,   155,   462,   173,
     217,   346,   157,   345,   151,   359,   151,   151,   155,   149,
     174,   358,   153,   358,   358,   358,   214,   358,   151,   358,
     358,   358,   176,   151,   162,   163,   200,    17,   297,   151,
     155,   151,   160,   161,   151,   220,   214,   157,   180,   180,
     113,   153,   180,   150,   188,   189,   190,   214,   113,   153,
     180,   330,   214,   188,   180,   198,   201,   201,   201,   202,
     202,   203,   203,   204,   204,   204,   204,   205,   205,   206,
     207,   208,   209,   210,   156,   221,   174,   182,   153,   180,
     214,   157,   214,   176,   436,   437,   438,   295,   435,   402,
     402,   214,   359,   149,   402,   439,   440,   149,   439,   440,
     176,   176,   154,   154,   149,   408,   427,   428,   429,   432,
      17,   295,   426,   430,   149,   402,   445,   463,   402,   402,
     463,   149,   402,   445,   402,   402,   177,   213,   357,   154,
     155,   154,   155,   463,   463,   131,   347,   348,   349,   347,
     357,   176,   212,   213,   214,   400,   462,   361,   363,   148,
     176,   151,   155,   176,   347,   180,   399,   180,   151,   151,
     151,   151,   151,   151,   149,   402,   439,   440,   149,   402,
     439,   440,   399,   182,   440,   214,   225,   350,   151,   151,
     151,   151,   386,   387,   225,   388,   225,   397,   387,   225,
     157,   157,   157,   331,   177,   177,   180,   276,   357,    17,
      69,    71,    74,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,    87,    88,    90,    91,    92,    93,    94,
      96,   104,   105,   116,   176,   221,   222,   223,   224,   225,
     226,   227,   229,   230,   240,   244,   245,   246,   247,   248,
     249,   254,   255,   261,   262,   263,   277,   295,   299,   357,
     398,    68,   174,   177,   177,   177,   347,   177,   389,   387,
     281,   283,   292,   381,   382,   383,   384,   376,   173,   367,
     367,   293,   453,   153,   160,   196,   214,   317,   214,   295,
     350,   151,   151,   151,     5,   295,   402,   441,   157,   180,
     431,     9,   357,   148,   361,   345,   462,   157,   151,   406,
     188,   151,   176,   155,   151,   151,   155,   151,   198,   151,
     151,   151,   198,    17,   297,   214,   151,   151,   150,   157,
     198,   154,   177,   188,   113,   117,   119,   181,   191,   192,
     193,   151,   155,   191,   154,   155,   148,   212,   156,   151,
     191,   177,   362,   350,   151,   151,   151,   435,   176,   176,
     350,   350,   432,   151,   151,   151,   151,   149,   408,   431,
     426,   430,   176,   176,   154,   177,   463,   176,   176,   177,
     177,   177,   177,   360,   191,   131,   165,   177,   177,   148,
     361,   214,   150,   214,   347,   177,   173,   149,   402,   439,
     440,   149,   402,   439,   440,   176,   176,   401,   151,   177,
     177,   389,   387,   225,   389,   331,   331,   331,     3,     9,
      71,   148,   278,   285,   286,   292,   295,   332,   337,   456,
     151,   155,   155,   174,   149,    59,    60,   174,   225,   277,
     398,   149,    17,   223,   149,   149,   174,   357,   174,   357,
     160,   357,   157,   222,   149,   149,   149,   225,   214,   215,
     215,    13,   264,    72,   231,   174,   177,   227,    76,   174,
     357,    89,   250,   356,   295,   156,   276,   174,   154,   154,
     177,   155,   389,   399,   177,   174,   177,   174,   177,   151,
     359,   373,   373,   176,   177,   177,   177,   214,   177,   149,
     402,   445,   440,   294,     5,   160,   177,   214,   345,   402,
     402,   317,   346,   462,   148,   148,   176,   151,   180,    76,
     185,   186,   358,   198,   198,   198,   198,   198,   157,   362,
     155,   148,   194,   153,   192,   194,   194,   154,   155,   120,
     152,   190,   154,   220,   212,   174,   154,   462,   177,   149,
     402,   439,   440,   350,   350,   177,   177,   151,   149,   402,
     439,   440,   149,   402,   445,   408,   402,   402,   350,   350,
     154,   349,   352,   352,   353,   151,   155,   155,   151,   177,
     213,   213,   154,   154,   177,   177,   151,   214,   176,   176,
     350,   350,   360,   402,   155,   151,   148,   389,   148,   148,
     148,   148,   292,   330,   338,   456,   292,   337,   149,   326,
     174,   174,   149,   156,   196,   333,   334,   340,   408,   409,
     422,   155,   174,   357,   176,   357,   151,   188,   189,   174,
     225,   174,   225,   221,    78,   151,   221,   232,   277,   279,
     282,   288,   295,   299,   151,   173,   174,   221,   241,   242,
     277,   174,   174,   221,   174,   362,   174,   221,   220,   221,
     108,   109,   110,   111,   112,   256,   258,   259,   174,    95,
     174,    82,   149,   149,   177,   148,   174,   174,   149,   223,
     225,   402,   174,   151,   176,   148,   148,   176,   155,   155,
     154,   154,   154,   177,   151,   176,   214,   214,   177,   154,
     177,   462,   343,   157,   346,   148,   381,   151,   156,   151,
     155,   156,   362,   462,   220,   118,   191,   192,   153,   192,
     153,   192,   154,   148,   151,   176,   177,   177,   151,   151,
     176,   176,   177,   177,   177,   176,   176,   154,   177,   151,
     402,   350,   350,   177,   177,   221,   148,   326,   326,   326,
     149,   196,   335,   336,   439,   447,   448,   449,   450,   174,
     155,   174,   333,   174,   376,   403,   408,   214,   295,   155,
     174,   339,   340,   339,   357,   131,   354,   355,   221,   151,
     151,   149,   223,   151,   221,   295,   223,   221,   222,   143,
     144,   145,   165,   174,   243,   151,   156,   222,   174,   462,
     151,   151,   151,   225,   258,   259,   149,   214,   149,   182,
     232,   198,   251,   107,     1,   223,   402,   382,   176,   176,
     154,   350,   177,   177,   154,   154,   148,   157,   345,   177,
     214,   186,   214,   462,   148,   154,   154,   191,   191,   350,
     151,   151,   350,   350,   151,   151,   154,   155,   131,   349,
     131,   154,   177,   177,   151,   151,   154,   448,   449,   450,
     295,   447,   155,   174,   402,   402,   174,   151,   408,   402,
     174,   223,    75,    76,   157,   235,   236,   237,   151,   221,
     223,   174,   104,   173,   221,   222,   221,   223,   242,   174,
     148,   157,   237,   223,   149,   176,   174,   182,   151,   156,
     151,   151,   155,   156,   249,   253,   357,   399,   177,   154,
     154,   345,   462,   148,   148,   154,   154,   177,   177,   177,
     176,   177,   151,   151,   151,   151,   151,   447,   402,   334,
       1,   213,   233,   234,   400,     1,   156,     1,   176,   223,
     235,   174,   151,    73,   222,   221,   144,   165,   243,   174,
     165,    73,   222,   174,     1,   176,   176,   260,   293,   295,
     456,   156,   174,   153,   182,   265,   266,   267,   223,   198,
     188,    73,   106,   250,   252,   151,   462,   148,   151,   151,
     151,   352,   149,   402,   439,   440,   336,   131,     1,   155,
     156,   148,   270,   271,   277,    73,   174,   223,   150,   150,
     221,   222,   221,   223,   148,   270,   260,   177,   149,   196,
     399,   447,   180,   156,   101,   149,   151,   156,   155,    73,
     151,   223,   149,   223,   223,   148,   176,   213,   233,   236,
     238,   239,   277,   223,   165,   165,   165,   238,   177,   174,
     257,   295,   265,   154,   213,   174,   265,   267,   223,   221,
     107,   107,   350,   223,   228,   177,   236,   221,   150,   221,
     221,   177,   257,   212,   151,   156,   182,   151,   151,   156,
     151,   253,    73,   248,   177,     1,   223,   148,   228,   148,
     151,   225,   182,   268,   149,   174,   268,   223,    73,   151,
     225,   155,   156,   213,   151,   223,   182,   180,   269,   151,
     174,   151,   155,   174,   180
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_int16 yyr1[] =
{
       0,   175,   176,   177,   178,   178,   178,   178,   178,   179,
     179,   179,   179,   179,   179,   179,   180,   180,   181,   181,
     182,   183,   183,   184,   184,   184,   184,   184,   184,   184,
     184,   184,   184,   184,   185,   185,   186,   186,   187,   187,
     187,   187,   187,   187,   187,   187,   187,   187,   187,   187,
     187,   187,   187,   187,   187,   187,   187,   187,   187,   188,
     188,   189,   189,   190,   190,   191,   191,   192,   192,   192,
     192,   192,   192,   192,   193,   193,   193,   194,   194,   195,
     195,   195,   195,   195,   195,   195,   195,   195,   195,   195,
     195,   195,   195,   196,   196,   196,   197,   197,   197,   197,
     198,   198,   198,   198,   198,   198,   198,   198,   198,   199,
     199,   199,   199,   200,   200,   201,   201,   202,   202,   202,
     202,   203,   203,   203,   204,   204,   204,   205,   205,   205,
     205,   205,   206,   206,   206,   207,   207,   208,   208,   209,
     209,   210,   210,   211,   211,   212,   212,   212,   213,   214,
     214,   214,   215,   215,   216,   216,   217,   217,   218,   218,
     218,   218,   218,   218,   218,   218,   218,   218,   218,   219,
     219,   220,   220,   220,   220,   221,   221,   222,   222,   223,
     223,   223,   223,   223,   223,   223,   223,   223,   223,   223,
     223,   223,   224,   224,   225,   225,   226,   226,   227,   227,
     227,   227,   227,   228,   228,   228,   229,   229,   230,   230,
     230,   230,   230,   230,   230,   231,   231,   232,   232,   232,
     232,   233,   233,   233,   234,   234,   235,   235,   235,   235,
     235,   236,   236,   237,   238,   238,   239,   239,   240,   240,
     240,   240,   240,   240,   240,   240,   240,   241,   241,   242,
     242,   242,   242,   242,   242,   242,   242,   242,   242,   242,
     242,   242,   242,   242,   242,   242,   242,   243,   243,   243,
     243,   244,   244,   244,   244,   244,   244,   244,   244,   244,
     244,   244,   244,   244,   244,   244,   244,   244,   244,   244,
     244,   245,   245,   246,   247,   248,   249,   249,   250,   250,
     251,   251,   252,   253,   253,   253,   253,   253,   253,   254,
     254,   255,   255,   255,   256,   256,   257,   257,   258,   258,
     258,   258,   259,   260,   260,   260,   260,   260,   261,   262,
     262,   263,   263,   263,   263,   263,   264,   264,   265,   265,
     266,   266,   267,   267,   268,   268,   268,   269,   269,   270,
     270,   271,   271,   272,   272,   273,   273,   274,   274,   275,
     275,   276,   276,   277,   277,   277,   278,   278,   279,   279,
     279,   279,   279,   280,   280,   280,   281,   281,   281,   282,
     282,   282,   282,   282,   283,   283,   284,   284,   285,   285,
     285,   286,   286,   286,   286,   286,   287,   287,   288,   288,
     288,   288,   289,   289,   290,   290,   290,   291,   291,   291,
     292,   292,   292,   293,   293,   293,   294,   294,   295,   295,
     296,   296,   297,   297,   297,   297,   297,   298,   299,   299,
     299,   300,   300,   301,   301,   301,   301,   301,   301,   301,
     301,   302,   302,   302,   302,   302,   302,   302,   302,   302,
     302,   302,   302,   302,   302,   302,   302,   302,   302,   302,
     302,   302,   302,   302,   302,   302,   302,   302,   302,   303,
     303,   304,   305,   305,   306,   306,   306,   306,   306,   307,
     307,   308,   308,   308,   308,   309,   309,   309,   309,   309,
     309,   310,   310,   310,   310,   311,   312,   311,   311,   313,
     313,   313,   313,   314,   314,   314,   315,   315,   315,   315,
     316,   316,   316,   317,   317,   317,   317,   317,   317,   318,
     318,   318,   319,   319,   320,   320,   322,   321,   323,   321,
     324,   321,   325,   321,   321,   326,   326,   327,   327,   328,
     328,   329,   329,   329,   330,   330,   330,   330,   330,   330,
     330,   330,   331,   331,   332,   332,   332,   332,   332,   332,
     332,   332,   332,   332,   333,   333,   333,   334,   334,   334,
     335,   335,   335,   336,   337,   337,   338,   338,   339,   339,
     340,   341,   342,   341,   341,   341,   343,   341,   341,   341,
     344,   344,   345,   345,   345,   345,   346,   346,   347,   347,
     347,   347,   347,   347,   347,   348,   348,   348,   348,   349,
     349,   350,   350,   350,   350,   351,   351,   351,   351,   352,
     352,   352,   352,   352,   353,   353,   353,   353,   353,   354,
     354,   355,   355,   356,   356,   357,   357,   357,   358,   358,
     358,   359,   359,   360,   360,   360,   360,   361,   361,   362,
     362,   362,   362,   362,   363,   363,   364,   364,   365,   365,
     365,   365,   365,   366,   366,   367,   367,   369,   368,   370,
     368,   368,   368,   371,   371,   371,   371,   372,   372,   372,
     372,   373,   373,   374,   374,   375,   375,   376,   376,   376,
     376,   377,   377,   377,   378,   378,   379,   379,   380,   380,
     381,   381,   382,   382,   383,   383,   383,   384,   384,   385,
     385,   386,   386,   387,   387,   388,   389,   390,   390,   390,
     390,   390,   391,   390,   392,   390,   393,   390,   394,   390,
     395,   390,   396,   396,   396,   397,   397,   398,   398,   398,
     398,   398,   398,   398,   398,   398,   398,   399,   399,   399,
     400,   401,   401,   402,   402,   403,   403,   404,   405,   405,
     406,   406,   406,   407,   407,   407,   407,   407,   407,   408,
     408,   409,   409,   409,   409,   410,   410,   410,   410,   411,
     411,   411,   411,   411,   411,   411,   412,   412,   412,   412,
     413,   413,   413,   414,   414,   414,   414,   414,   415,   415,
     415,   415,   416,   416,   416,   416,   416,   416,   417,   417,
     417,   418,   418,   418,   418,   418,   419,   419,   419,   419,
     420,   420,   420,   420,   420,   420,   421,   421,   422,   422,
     422,   422,   423,   423,   423,   423,   424,   424,   424,   424,
     424,   424,   424,   425,   425,   425,   425,   425,   426,   426,
     426,   426,   426,   427,   427,   427,   428,   428,   428,   428,
     429,   429,   429,   430,   430,   430,   430,   430,   431,   431,
     432,   432,   432,   433,   433,   434,   434,   435,   435,   435,
     436,   436,   436,   436,   436,   437,   437,   437,   437,   438,
     438,   438,   439,   439,   439,   439,   440,   440,   440,   440,
     441,   441,   441,   441,   442,   442,   442,   442,   442,   443,
     443,   443,   443,   444,   444,   444,   445,   445,   445,   446,
     446,   446,   446,   446,   446,   447,   447,   447,   448,   448,
     448,   448,   448,   449,   449,   449,   449,   450,   450,   451,
     451,   451,   452,   452,   453,   453,   453,   453,   453,   453,
     454,   454,   454,   454,   454,   454,   454,   454,   454,   454,
     455,   455,   455,   455,   456,   456,   456,   457,   457,   458,
     458,   458,   458,   458,   458,   459,   459,   459,   459,   459,
     459,   460,   460,   460,   461,   461,   462,   462,   463,   463
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     0,     0,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     2,     1,     1,     1,     1,     3,     3,     3,
       5,     6,     2,     2,     1,     3,     3,     3,     1,     6,
       4,     4,     4,     3,     3,     3,     3,     3,     2,     5,
       3,     3,     3,     5,     2,     2,     7,     8,     5,     0,
       1,     1,     3,     1,     1,     1,     3,     1,     2,     4,
       3,     5,     3,     5,     2,     2,     2,     0,     2,     1,
       1,     1,     2,     2,     2,     2,     2,     2,     4,     2,
       4,     6,     4,     1,     1,     1,     1,     1,     1,     1,
       1,     4,     5,     5,     4,     5,     5,     5,     4,     2,
       2,     3,     3,     1,     1,     1,     3,     1,     3,     3,
       3,     1,     3,     3,     1,     3,     3,     1,     3,     3,
       3,     3,     1,     3,     3,     1,     3,     1,     3,     1,
       3,     1,     3,     1,     3,     1,     5,     4,     1,     1,
       3,     6,     0,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     4,
       7,     1,     1,     3,     3,     1,     3,     0,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     4,     4,     2,     6,     1,     2,     1,     2,
       1,     2,     1,     1,     2,     2,     2,     5,     3,     5,
      10,     7,     5,    10,     7,     5,     7,     1,     1,     1,
       2,     1,     3,     1,     1,     3,     2,     3,     3,     2,
       2,     1,     2,     2,     0,     1,     2,     3,     4,     5,
       7,     6,     7,     8,     4,     5,     7,     1,     3,     4,
       5,     4,     1,     2,     3,     5,     2,     3,     4,     5,
       7,     3,     5,     5,     7,     7,     7,     1,     1,     1,
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
       1,     1,     3,     6,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     0,     1,     1,     2,
       1,     1,     1,     1,     1,     1,     1,     4,     1,     2,
       3,     1,     2,     1,     1,     1,     1,     1,     1,     1,
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
       1,     2,     0,     2,     3,     4,     4,     3,     2,     2,
       3,     3,     2,     1,     0,     1,     4,     1,     2,     2,
       0,     1,     4,     1,     2,     3,     1,     2,     0,     1,
       2,     6,     0,     8,     7,     9,     0,    12,    11,     1,
       3,     3,     2,     2,     4,     5,     0,     2,     0,     1,
       1,     1,     5,     5,     5,     1,     5,     5,     9,     1,
       5,     0,     1,     1,     5,     1,     1,     5,     5,     1,
       3,     3,     4,     1,     1,     1,     1,     2,     1,     3,
       3,     2,     3,     1,     3,     1,     1,     1,     1,     1,
       2,     1,     1,     0,     2,     2,     4,     1,     4,     0,
       1,     2,     3,     4,     2,     2,     1,     2,     2,     5,
       5,     7,     6,     1,     3,     0,     2,     0,     5,     0,
       5,     3,     1,     0,     1,     1,     1,     1,     1,     1,
       1,     0,     1,     1,     2,     5,     6,     1,     1,     3,
       3,     2,     3,     3,     2,     4,     1,     4,     7,    10,
       1,     4,     2,     2,     1,     1,     5,     2,     5,     0,
       1,     3,     4,     0,     1,     0,     0,     1,     1,     1,
       2,     5,     0,     6,     0,     8,     0,     7,     0,     7,
       0,     8,     1,     2,     3,     0,     4,     3,     4,     4,
       4,     4,     5,     5,     5,     5,     6,     1,     1,     1,
       3,     0,     5,     0,     1,     1,     2,     6,     1,     3,
       0,     1,     4,     1,     1,     1,     1,     1,     1,     1,
       3,     2,     1,     2,     2,     2,     3,     4,     5,     2,
       4,     5,     4,     5,     3,     4,     8,     9,     3,     4,
       2,     1,     2,     6,     8,     9,     3,     4,     2,     3,
       4,     5,     4,     5,     4,     5,     3,     4,     1,     1,
       1,     4,     8,     9,     3,     4,     2,     3,     3,     4,
       4,     5,     4,     5,     3,     4,     1,     3,     2,     1,
       2,     2,     2,     3,     4,     5,     2,     4,     5,     4,
       5,     3,     4,     6,     8,     9,     3,     4,     2,     4,
       1,     2,     2,     2,     3,     4,     2,     4,     4,     3,
       6,     8,     3,     2,     4,     1,     2,     2,     1,     1,
       2,     3,     4,     2,     4,     6,     8,     1,     2,     2,
       1,     2,     2,     3,     4,     1,     4,     4,     3,     5,
       8,     3,     2,     3,     7,     1,     5,     5,     6,     6,
       1,     3,     2,     2,     1,     2,     2,     3,     4,     1,
       4,     4,     3,     5,     8,     3,     1,     2,     1,     2,
       6,     5,     6,     7,     7,     1,     2,     2,     1,     2,
       2,     3,     4,     1,     4,     4,     3,     8,     3,     1,
       1,     2,     1,     1,     2,     3,     2,     3,     2,     3,
       3,     2,     4,     3,     2,     3,     2,     4,     3,     2,
       6,     6,     6,     7,     1,     2,     1,     1,     1,     2,
       3,     2,     3,     2,     3,     3,     4,     2,     3,     4,
       2,     5,     6,     7,     6,     6,     0,     1,     0,     2
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
#line 6870 "Parser/parser.cc"
    break;

  case 3:
#line 533 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 6876 "Parser/parser.cc"
    break;

  case 4:
#line 540 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantInteger( *(yyvsp[0].tok) ) ); }
#line 6882 "Parser/parser.cc"
    break;

  case 5:
#line 541 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 6888 "Parser/parser.cc"
    break;

  case 6:
#line 542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 6894 "Parser/parser.cc"
    break;

  case 7:
#line 543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 6900 "Parser/parser.cc"
    break;

  case 8:
#line 544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantChar( *(yyvsp[0].tok) ) ); }
#line 6906 "Parser/parser.cc"
    break;

  case 19:
#line 565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 6912 "Parser/parser.cc"
    break;

  case 20:
#line 569 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.constant) = build_constantStr( *(yyvsp[0].str) ); }
#line 6918 "Parser/parser.cc"
    break;

  case 21:
#line 573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 6924 "Parser/parser.cc"
    break;

  case 22:
#line 575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 6934 "Parser/parser.cc"
    break;

  case 23:
#line 586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 6940 "Parser/parser.cc"
    break;

  case 24:
#line 588 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 6946 "Parser/parser.cc"
    break;

  case 25:
#line 592 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_dimensionref( (yyvsp[0].tok) ) ); }
#line 6952 "Parser/parser.cc"
    break;

  case 27:
#line 595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 6958 "Parser/parser.cc"
    break;

  case 28:
#line 597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new StmtExpr( dynamic_cast<CompoundStmt *>(maybeMoveBuild<Statement>((yyvsp[-1].sn)) ) ) ); }
#line 6964 "Parser/parser.cc"
    break;

  case 29:
#line 599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.en) = nullptr; }
#line 6970 "Parser/parser.cc"
    break;

  case 30:
#line 601 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.en) = nullptr; }
#line 6976 "Parser/parser.cc"
    break;

  case 31:
#line 603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild<Expression>( (yyvsp[-3].en) );
			(yyval.en) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 6986 "Parser/parser.cc"
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
#line 6998 "Parser/parser.cc"
    break;

  case 33:
#line 621 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "Identifier \"", *(yyvsp[-1].tok).str, "\" cannot appear before a type. "
											   "Possible problem is misspelled storage or CV qualifier." ) );
			(yyval.en) = nullptr;
		}
#line 7008 "Parser/parser.cc"
    break;

  case 35:
#line 631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// steal the association node from the singleton and delete the wrapper
			(yyvsp[-2].genexpr)->associations.splice((yyvsp[-2].genexpr)->associations.end(), (yyvsp[0].genexpr)->associations);
			delete (yyvsp[0].genexpr);
			(yyval.genexpr) = (yyvsp[-2].genexpr);
		}
#line 7019 "Parser/parser.cc"
    break;

  case 36:
#line 641 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuildType((yyvsp[-2].decl)), maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } );
		}
#line 7028 "Parser/parser.cc"
    break;

  case 37:
#line 646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } ); }
#line 7034 "Parser/parser.cc"
    break;

  case 39:
#line 656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-5].en), new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) ) ) )) ) ); }
#line 7040 "Parser/parser.cc"
    break;

  case 40:
#line 662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7046 "Parser/parser.cc"
    break;

  case 41:
#line 664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.en) = new ExpressionNode( new ConstructorExpr( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) ) );
		}
#line 7056 "Parser/parser.cc"
    break;

  case 42:
#line 670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7062 "Parser/parser.cc"
    break;

  case 43:
#line 672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 7068 "Parser/parser.cc"
    break;

  case 44:
#line 674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 7074 "Parser/parser.cc"
    break;

  case 45:
#line 676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), new ExpressionNode( (yyvsp[-2].constant) ) ) ); }
#line 7080 "Parser/parser.cc"
    break;

  case 46:
#line 678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7086 "Parser/parser.cc"
    break;

  case 47:
#line 680 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 7092 "Parser/parser.cc"
    break;

  case 48:
#line 682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-1].en), build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) ) ) ); }
#line 7098 "Parser/parser.cc"
    break;

  case 49:
#line 684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7104 "Parser/parser.cc"
    break;

  case 50:
#line 686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[0].aggKey), (yyvsp[-2].en) ) ); }
#line 7110 "Parser/parser.cc"
    break;

  case 51:
#line 688 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7116 "Parser/parser.cc"
    break;

  case 52:
#line 690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 7122 "Parser/parser.cc"
    break;

  case 53:
#line 692 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7128 "Parser/parser.cc"
    break;

  case 54:
#line 694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::IncrPost, (yyvsp[-1].en) ) ); }
#line 7134 "Parser/parser.cc"
    break;

  case 55:
#line 696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::DecrPost, (yyvsp[-1].en) ) ); }
#line 7140 "Parser/parser.cc"
    break;

  case 56:
#line 698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].in), true ) ) ); }
#line 7146 "Parser/parser.cc"
    break;

  case 57:
#line 700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].in), true ))->set_maybeConstructed( false ) ) ); }
#line 7152 "Parser/parser.cc"
    break;

  case 58:
#line 702 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) );
		}
#line 7162 "Parser/parser.cc"
    break;

  case 59:
#line 711 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7168 "Parser/parser.cc"
    break;

  case 62:
#line 718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7174 "Parser/parser.cc"
    break;

  case 63:
#line 723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Default parameter for argument is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7180 "Parser/parser.cc"
    break;

  case 66:
#line 730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7186 "Parser/parser.cc"
    break;

  case 68:
#line 736 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-1].tok) ) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7192 "Parser/parser.cc"
    break;

  case 69:
#line 738 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-3].tok) ) ), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7198 "Parser/parser.cc"
    break;

  case 70:
#line 740 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7204 "Parser/parser.cc"
    break;

  case 71:
#line 742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7210 "Parser/parser.cc"
    break;

  case 72:
#line 744 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7216 "Parser/parser.cc"
    break;

  case 73:
#line 746 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7222 "Parser/parser.cc"
    break;

  case 74:
#line 751 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_constantInteger( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 7228 "Parser/parser.cc"
    break;

  case 75:
#line 753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_field_name_FLOATINGconstant( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 7234 "Parser/parser.cc"
    break;

  case 76:
#line 755 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_varref( (yyvsp[-1].tok) ), (yyvsp[0].en) ) );
		}
#line 7242 "Parser/parser.cc"
    break;

  case 77:
#line 762 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7248 "Parser/parser.cc"
    break;

  case 78:
#line 764 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Expression * constant = build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) );
			(yyval.en) = (yyvsp[-1].en) != nullptr ? new ExpressionNode( build_fieldSel( (yyvsp[-1].en),  constant ) ) : new ExpressionNode( constant );
		}
#line 7257 "Parser/parser.cc"
    break;

  case 81:
#line 776 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 7263 "Parser/parser.cc"
    break;

  case 82:
#line 778 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en)->set_extension( true ); }
#line 7269 "Parser/parser.cc"
    break;

  case 83:
#line 783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 7289 "Parser/parser.cc"
    break;

  case 84:
#line 799 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_val( (yyvsp[-1].op), (yyvsp[0].en) ) ); }
#line 7295 "Parser/parser.cc"
    break;

  case 85:
#line 801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Incr, (yyvsp[0].en) ) ); }
#line 7301 "Parser/parser.cc"
    break;

  case 86:
#line 803 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Decr, (yyvsp[0].en) ) ); }
#line 7307 "Parser/parser.cc"
    break;

  case 87:
#line 805 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7313 "Parser/parser.cc"
    break;

  case 88:
#line 807 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 7319 "Parser/parser.cc"
    break;

  case 89:
#line 809 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7325 "Parser/parser.cc"
    break;

  case 90:
#line 811 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 7331 "Parser/parser.cc"
    break;

  case 91:
#line 813 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_offsetOf( (yyvsp[-3].decl), build_varref( (yyvsp[-1].tok) ) ) ); }
#line 7337 "Parser/parser.cc"
    break;

  case 92:
#line 815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.en) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 7346 "Parser/parser.cc"
    break;

  case 93:
#line 822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::PointTo; }
#line 7352 "Parser/parser.cc"
    break;

  case 94:
#line 823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AddressOf; }
#line 7358 "Parser/parser.cc"
    break;

  case 95:
#line 825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::And; }
#line 7364 "Parser/parser.cc"
    break;

  case 96:
#line 829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::UnPlus; }
#line 7370 "Parser/parser.cc"
    break;

  case 97:
#line 830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::UnMinus; }
#line 7376 "Parser/parser.cc"
    break;

  case 98:
#line 831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::Neg; }
#line 7382 "Parser/parser.cc"
    break;

  case 99:
#line 832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::BitNeg; }
#line 7388 "Parser/parser.cc"
    break;

  case 101:
#line 838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cast( (yyvsp[-2].decl), (yyvsp[0].en) ) ); }
#line 7394 "Parser/parser.cc"
    break;

  case 102:
#line 840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 7400 "Parser/parser.cc"
    break;

  case 103:
#line 842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 7406 "Parser/parser.cc"
    break;

  case 104:
#line 844 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( nullptr ) ) ); }
#line 7412 "Parser/parser.cc"
    break;

  case 105:
#line 846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 7418 "Parser/parser.cc"
    break;

  case 106:
#line 848 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Return cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7424 "Parser/parser.cc"
    break;

  case 107:
#line 850 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7430 "Parser/parser.cc"
    break;

  case 108:
#line 852 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7436 "Parser/parser.cc"
    break;

  case 116:
#line 872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Exp, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7442 "Parser/parser.cc"
    break;

  case 118:
#line 878 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mul, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7448 "Parser/parser.cc"
    break;

  case 119:
#line 880 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Div, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7454 "Parser/parser.cc"
    break;

  case 120:
#line 882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mod, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7460 "Parser/parser.cc"
    break;

  case 122:
#line 888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Plus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7466 "Parser/parser.cc"
    break;

  case 123:
#line 890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Minus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7472 "Parser/parser.cc"
    break;

  case 125:
#line 896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7478 "Parser/parser.cc"
    break;

  case 126:
#line 898 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::RShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7484 "Parser/parser.cc"
    break;

  case 128:
#line 904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7490 "Parser/parser.cc"
    break;

  case 129:
#line 906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7496 "Parser/parser.cc"
    break;

  case 130:
#line 908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7502 "Parser/parser.cc"
    break;

  case 131:
#line 910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7508 "Parser/parser.cc"
    break;

  case 133:
#line 916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Eq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7514 "Parser/parser.cc"
    break;

  case 134:
#line 918 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Neq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7520 "Parser/parser.cc"
    break;

  case 136:
#line 924 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitAnd, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7526 "Parser/parser.cc"
    break;

  case 138:
#line 930 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Xor, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7532 "Parser/parser.cc"
    break;

  case 140:
#line 936 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitOr, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7538 "Parser/parser.cc"
    break;

  case 142:
#line 942 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), true ) ); }
#line 7544 "Parser/parser.cc"
    break;

  case 144:
#line 948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), false ) ); }
#line 7550 "Parser/parser.cc"
    break;

  case 146:
#line 954 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7556 "Parser/parser.cc"
    break;

  case 147:
#line 957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-3].en), (yyvsp[-3].en), (yyvsp[0].en) ) ); }
#line 7562 "Parser/parser.cc"
    break;

  case 150:
#line 968 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
//			if ( $2 == OperKinds::AtAssn ) {
//				SemanticError( yylloc, "C @= assignment is currently unimplemented." ); $$ = nullptr;
//			} else {
				(yyval.en) = new ExpressionNode( build_binary_val( (yyvsp[-1].op), (yyvsp[-2].en), (yyvsp[0].en) ) );
//			} // if
		}
#line 7574 "Parser/parser.cc"
    break;

  case 151:
#line 976 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7580 "Parser/parser.cc"
    break;

  case 152:
#line 981 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7586 "Parser/parser.cc"
    break;

  case 156:
#line 991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::Assign; }
#line 7592 "Parser/parser.cc"
    break;

  case 157:
#line 992 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AtAssn; }
#line 7598 "Parser/parser.cc"
    break;

  case 158:
#line 996 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ExpAssn; }
#line 7604 "Parser/parser.cc"
    break;

  case 159:
#line 997 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MulAssn; }
#line 7610 "Parser/parser.cc"
    break;

  case 160:
#line 998 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::DivAssn; }
#line 7616 "Parser/parser.cc"
    break;

  case 161:
#line 999 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ModAssn; }
#line 7622 "Parser/parser.cc"
    break;

  case 162:
#line 1000 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::PlusAssn; }
#line 7628 "Parser/parser.cc"
    break;

  case 163:
#line 1001 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MinusAssn; }
#line 7634 "Parser/parser.cc"
    break;

  case 164:
#line 1002 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::LSAssn; }
#line 7640 "Parser/parser.cc"
    break;

  case 165:
#line 1003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::RSAssn; }
#line 7646 "Parser/parser.cc"
    break;

  case 166:
#line 1004 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AndAssn; }
#line 7652 "Parser/parser.cc"
    break;

  case 167:
#line 1005 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ERAssn; }
#line 7658 "Parser/parser.cc"
    break;

  case 168:
#line 1006 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::OrAssn; }
#line 7664 "Parser/parser.cc"
    break;

  case 169:
#line 1017 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)(new ExpressionNode( nullptr ) )->set_last( (yyvsp[-1].en) ) ) ); }
#line 7670 "Parser/parser.cc"
    break;

  case 170:
#line 1019 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-4].en)->set_last( (yyvsp[-1].en) ) ) )); }
#line 7676 "Parser/parser.cc"
    break;

  case 172:
#line 1025 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7682 "Parser/parser.cc"
    break;

  case 173:
#line 1027 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7688 "Parser/parser.cc"
    break;

  case 174:
#line 1029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7694 "Parser/parser.cc"
    break;

  case 176:
#line 1035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new CommaExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7700 "Parser/parser.cc"
    break;

  case 177:
#line 1040 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7706 "Parser/parser.cc"
    break;

  case 189:
#line 1058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 7712 "Parser/parser.cc"
    break;

  case 191:
#line 1061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_directive( (yyvsp[0].tok) ) ); }
#line 7718 "Parser/parser.cc"
    break;

  case 192:
#line 1067 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[0].sn)->add_label( (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 7724 "Parser/parser.cc"
    break;

  case 193:
#line 1069 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "Label \"", *(yyvsp[-3].tok).str, "\" must be associated with a statement, "
											   "where a declaration, case, or default is not a statement. "
											   "Move the label or terminate with a semi-colon." ) );
			(yyval.sn) = nullptr;
		}
#line 7735 "Parser/parser.cc"
    break;

  case 194:
#line 1079 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (StatementNode *)0 ) ); }
#line 7741 "Parser/parser.cc"
    break;

  case 195:
#line 1084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (yyvsp[-2].sn) ) ); }
#line 7747 "Parser/parser.cc"
    break;

  case 197:
#line 1090 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 7753 "Parser/parser.cc"
    break;

  case 198:
#line 1095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7759 "Parser/parser.cc"
    break;

  case 199:
#line 1097 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7765 "Parser/parser.cc"
    break;

  case 200:
#line 1099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7771 "Parser/parser.cc"
    break;

  case 201:
#line 1101 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7777 "Parser/parser.cc"
    break;

  case 204:
#line 1108 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 7783 "Parser/parser.cc"
    break;

  case 205:
#line 1110 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Declarations only allowed at the start of the switch body, i.e., after the '{'." ); (yyval.sn) = nullptr; }
#line 7789 "Parser/parser.cc"
    break;

  case 206:
#line 1115 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_expr( (yyvsp[-1].en) ) ); }
#line 7795 "Parser/parser.cc"
    break;

  case 207:
#line 1117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_mutex( nullptr, new StatementNode( build_expr( (yyvsp[-1].en) ) ) ) ); }
#line 7801 "Parser/parser.cc"
    break;

  case 208:
#line 1124 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn); }
#line 7807 "Parser/parser.cc"
    break;

  case 209:
#line 1126 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( true, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 7813 "Parser/parser.cc"
    break;

  case 210:
#line 1128 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( true, (yyvsp[-7].en), (yyvsp[-2].sn) ) );
			// The semantics of the declaration list is changed to include associated initialization, which is performed
			// *before* the transfer to the appropriate case clause by hoisting the declarations into a compound
			// statement around the switch.  Statements after the initial declaration list can never be executed, and
			// therefore, are removed from the grammar even though C allows it. The change also applies to choose
			// statement.
			(yyval.sn) = (yyvsp[-3].decl) ? new StatementNode( build_compound( (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 7827 "Parser/parser.cc"
    break;

  case 211:
#line 1138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Only declarations can appear before the list of case clauses." ); (yyval.sn) = nullptr; }
#line 7833 "Parser/parser.cc"
    break;

  case 212:
#line 1140 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( false, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 7839 "Parser/parser.cc"
    break;

  case 213:
#line 1142 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( false, (yyvsp[-7].en), (yyvsp[-2].sn) ) );
			(yyval.sn) = (yyvsp[-3].decl) ? new StatementNode( build_compound( (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 7848 "Parser/parser.cc"
    break;

  case 214:
#line 1147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Only declarations can appear before the list of case clauses." ); (yyval.sn) = nullptr; }
#line 7854 "Parser/parser.cc"
    break;

  case 215:
#line 1153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-2].ifctl), maybe_build_compound( (yyvsp[0].sn) ), nullptr ) ); }
#line 7860 "Parser/parser.cc"
    break;

  case 216:
#line 1155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-4].ifctl), maybe_build_compound( (yyvsp[-2].sn) ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 7866 "Parser/parser.cc"
    break;

  case 217:
#line 1160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( nullptr, (yyvsp[0].en) ); }
#line 7872 "Parser/parser.cc"
    break;

  case 218:
#line 1162 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 7878 "Parser/parser.cc"
    break;

  case 219:
#line 1164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 7884 "Parser/parser.cc"
    break;

  case 220:
#line 1166 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[-1].decl), (yyvsp[0].en) ); }
#line 7890 "Parser/parser.cc"
    break;

  case 221:
#line 1173 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = (yyvsp[0].en); }
#line 7896 "Parser/parser.cc"
    break;

  case 222:
#line 1175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7902 "Parser/parser.cc"
    break;

  case 224:
#line 1180 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.sn) = new StatementNode( build_case( (yyvsp[0].en) ) ); }
#line 7908 "Parser/parser.cc"
    break;

  case 225:
#line 1182 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)((yyvsp[-2].sn)->set_last( new StatementNode( build_case( (yyvsp[0].en) ) ) ) ); }
#line 7914 "Parser/parser.cc"
    break;

  case 226:
#line 1187 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing case list after case." ); (yyval.sn) = nullptr; }
#line 7920 "Parser/parser.cc"
    break;

  case 227:
#line 1188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn); }
#line 7926 "Parser/parser.cc"
    break;

  case 228:
#line 1190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing colon after case list." ); (yyval.sn) = nullptr; }
#line 7932 "Parser/parser.cc"
    break;

  case 229:
#line 1191 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.sn) = new StatementNode( build_default() ); }
#line 7938 "Parser/parser.cc"
    break;

  case 230:
#line 1194 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing colon after default." ); (yyval.sn) = nullptr; }
#line 7944 "Parser/parser.cc"
    break;

  case 232:
#line 1199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)( (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) )); }
#line 7950 "Parser/parser.cc"
    break;

  case 233:
#line 1203 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( maybe_build_compound( (yyvsp[0].sn) ) ); }
#line 7956 "Parser/parser.cc"
    break;

  case 234:
#line 1208 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = nullptr; }
#line 7962 "Parser/parser.cc"
    break;

  case 236:
#line 1214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ); }
#line 7968 "Parser/parser.cc"
    break;

  case 237:
#line 1216 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)( (yyvsp[-2].sn)->set_last( (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ) ) ); }
#line 7974 "Parser/parser.cc"
    break;

  case 238:
#line 1221 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( new CondCtl( nullptr, new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 7980 "Parser/parser.cc"
    break;

  case 239:
#line 1223 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( (yyvsp[-2].ifctl), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 7986 "Parser/parser.cc"
    break;

  case 240:
#line 1225 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( (yyvsp[-4].ifctl), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[0].sn) ) ); }
#line 7992 "Parser/parser.cc"
    break;

  case 241:
#line 1227 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( new ExpressionNode( build_constantInteger( *new string( "1" ) ) ), maybe_build_compound( (yyvsp[-4].sn) ) ) ); }
#line 7998 "Parser/parser.cc"
    break;

  case 242:
#line 1229 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( (yyvsp[-2].en), maybe_build_compound( (yyvsp[-5].sn) ) ) ); }
#line 8004 "Parser/parser.cc"
    break;

  case 243:
#line 1231 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( (yyvsp[-3].en), maybe_build_compound( (yyvsp[-6].sn) ), (yyvsp[0].sn) ) ); }
#line 8010 "Parser/parser.cc"
    break;

  case 244:
#line 1233 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( new ForCtrl( (ExpressionNode * )nullptr, (ExpressionNode * )nullptr, (ExpressionNode * )nullptr ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8016 "Parser/parser.cc"
    break;

  case 245:
#line 1235 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( (yyvsp[-2].fctl), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8022 "Parser/parser.cc"
    break;

  case 246:
#line 1237 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( (yyvsp[-4].fctl), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[0].sn) ) ); }
#line 8028 "Parser/parser.cc"
    break;

  case 248:
#line 1247 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 8047 "Parser/parser.cc"
    break;

  case 249:
#line 1265 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (ExpressionNode * )nullptr, (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8053 "Parser/parser.cc"
    break;

  case 250:
#line 1267 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8059 "Parser/parser.cc"
    break;

  case 251:
#line 1269 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-3].decl), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8065 "Parser/parser.cc"
    break;

  case 252:
#line 1272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8072 "Parser/parser.cc"
    break;

  case 253:
#line 1275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LEThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8079 "Parser/parser.cc"
    break;

  case 254:
#line 1278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].en)->clone(), (yyvsp[-1].compop), (yyvsp[0].en), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8085 "Parser/parser.cc"
    break;

  case 255:
#line 1280 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8091 "Parser/parser.cc"
    break;

  case 256:
#line 1282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( new ExpressionNode( build_constantInteger( *new string( "0u" ) ) ), (yyvsp[-1].en), nullptr, OperKinds::LThan, nullptr, nullptr ); }
#line 8097 "Parser/parser.cc"
    break;

  case 257:
#line 1284 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-2].en), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8104 "Parser/parser.cc"
    break;

  case 258:
#line 1287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-3].en), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LEThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8111 "Parser/parser.cc"
    break;

  case 259:
#line 1290 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), (yyvsp[-1].compop), (yyvsp[0].en), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8117 "Parser/parser.cc"
    break;

  case 260:
#line 1292 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8123 "Parser/parser.cc"
    break;

  case 261:
#line 1295 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Array interator is currently unimplemented." ); (yyval.fctl) = nullptr;
			(yyval.fctl) = forCtrl( new ExpressionNode( build_varref( (yyvsp[0].tok) ) ), (yyvsp[-2].en), nullptr, OperKinds::Range, nullptr, nullptr );
		}
#line 8132 "Parser/parser.cc"
    break;

  case 262:
#line 1302 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), OperKinds::LThan, nullptr, new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8138 "Parser/parser.cc"
    break;

  case 263:
#line 1304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), OperKinds::GThan, nullptr, new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8144 "Parser/parser.cc"
    break;

  case 264:
#line 1306 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), OperKinds::LThan, nullptr, (yyvsp[0].en) ); }
#line 8150 "Parser/parser.cc"
    break;

  case 265:
#line 1308 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), OperKinds::GThan, nullptr, (yyvsp[0].en) ); }
#line 8156 "Parser/parser.cc"
    break;

  case 266:
#line 1310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), OperKinds::LThan, nullptr, nullptr ); }
#line 8162 "Parser/parser.cc"
    break;

  case 267:
#line 1315 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LThan; }
#line 8168 "Parser/parser.cc"
    break;

  case 268:
#line 1317 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 8174 "Parser/parser.cc"
    break;

  case 269:
#line 1319 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GThan; }
#line 8180 "Parser/parser.cc"
    break;

  case 270:
#line 1321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GEThan; }
#line 8186 "Parser/parser.cc"
    break;

  case 271:
#line 1326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Goto ) ); }
#line 8192 "Parser/parser.cc"
    break;

  case 272:
#line 1330 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_computedgoto( (yyvsp[-1].en) ) ); }
#line 8198 "Parser/parser.cc"
    break;

  case 273:
#line 1333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThrough ) ); }
#line 8204 "Parser/parser.cc"
    break;

  case 274:
#line 1335 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::FallThrough ) ); }
#line 8210 "Parser/parser.cc"
    break;

  case 275:
#line 1337 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThroughDefault ) ); }
#line 8216 "Parser/parser.cc"
    break;

  case 276:
#line 1340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Continue ) ); }
#line 8222 "Parser/parser.cc"
    break;

  case 277:
#line 1344 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Continue ) ); }
#line 8228 "Parser/parser.cc"
    break;

  case 278:
#line 1347 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Break ) ); }
#line 8234 "Parser/parser.cc"
    break;

  case 279:
#line 1351 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Break ) ); }
#line 8240 "Parser/parser.cc"
    break;

  case 280:
#line 1353 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_return( (yyvsp[-1].en) ) ); }
#line 8246 "Parser/parser.cc"
    break;

  case 281:
#line 1355 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 8252 "Parser/parser.cc"
    break;

  case 282:
#line 1357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr ) ); }
#line 8258 "Parser/parser.cc"
    break;

  case 283:
#line 1359 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn) ) ); }
#line 8264 "Parser/parser.cc"
    break;

  case 284:
#line 1361 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Coroutine ) ); }
#line 8270 "Parser/parser.cc"
    break;

  case 285:
#line 1363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Coroutine ) ); }
#line 8276 "Parser/parser.cc"
    break;

  case 286:
#line 1365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Generator ) ); }
#line 8282 "Parser/parser.cc"
    break;

  case 287:
#line 1367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Generator ) ); }
#line 8288 "Parser/parser.cc"
    break;

  case 288:
#line 1369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_throw( (yyvsp[-1].en) ) ); }
#line 8294 "Parser/parser.cc"
    break;

  case 289:
#line 1371 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume( (yyvsp[-1].en) ) ); }
#line 8300 "Parser/parser.cc"
    break;

  case 290:
#line 1373 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume_at( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 8306 "Parser/parser.cc"
    break;

  case 293:
#line 1383 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_with( (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8312 "Parser/parser.cc"
    break;

  case 294:
#line 1389 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_mutex( (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8318 "Parser/parser.cc"
    break;

  case 295:
#line 1393 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 8324 "Parser/parser.cc"
    break;

  case 296:
#line 1398 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8330 "Parser/parser.cc"
    break;

  case 298:
#line 1404 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 8336 "Parser/parser.cc"
    break;

  case 299:
#line 1408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) )); }
#line 8342 "Parser/parser.cc"
    break;

  case 301:
#line 1415 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8348 "Parser/parser.cc"
    break;

  case 302:
#line 1419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 8354 "Parser/parser.cc"
    break;

  case 303:
#line 1424 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8360 "Parser/parser.cc"
    break;

  case 304:
#line 1426 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-3].en), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[-4].en), (yyvsp[0].wfs) ); }
#line 8366 "Parser/parser.cc"
    break;

  case 305:
#line 1428 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8372 "Parser/parser.cc"
    break;

  case 306:
#line 1430 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( nullptr, maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8378 "Parser/parser.cc"
    break;

  case 307:
#line 1433 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 8384 "Parser/parser.cc"
    break;

  case 308:
#line 1435 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-5].en), maybe_build_compound( (yyvsp[-4].sn) ), (yyvsp[-6].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8390 "Parser/parser.cc"
    break;

  case 309:
#line 1440 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-1].en), (yyvsp[0].sn), (yyvsp[-2].en) ) ); }
#line 8396 "Parser/parser.cc"
    break;

  case 310:
#line 1442 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-3].en), (yyvsp[-2].sn), (yyvsp[-4].en), (yyvsp[0].wfs) ) ); }
#line 8402 "Parser/parser.cc"
    break;

  case 311:
#line 1447 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), (yyvsp[0].sn), 0 ) ); }
#line 8408 "Parser/parser.cc"
    break;

  case 312:
#line 1449 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), 0, (yyvsp[0].sn) ) ); }
#line 8414 "Parser/parser.cc"
    break;

  case 313:
#line 1451 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-2].sn), (yyvsp[-1].sn), (yyvsp[0].sn) ) ); }
#line 8420 "Parser/parser.cc"
    break;

  case 314:
#line 1456 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8426 "Parser/parser.cc"
    break;

  case 315:
#line 1458 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)(yyvsp[-8].sn)->set_last( new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ) ); }
#line 8432 "Parser/parser.cc"
    break;

  case 316:
#line 1463 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8438 "Parser/parser.cc"
    break;

  case 317:
#line 1464 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[0].en); }
#line 8444 "Parser/parser.cc"
    break;

  case 318:
#line 1468 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 8450 "Parser/parser.cc"
    break;

  case 319:
#line 1469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 8456 "Parser/parser.cc"
    break;

  case 320:
#line 1470 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.catch_kind) = CatchStmt::Resume; }
#line 8462 "Parser/parser.cc"
    break;

  case 321:
#line 1471 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Resume; }
#line 8468 "Parser/parser.cc"
    break;

  case 322:
#line 1475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = new StatementNode( build_finally( (yyvsp[0].sn) ) ); }
#line 8474 "Parser/parser.cc"
    break;

  case 324:
#line 1482 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 8480 "Parser/parser.cc"
    break;

  case 325:
#line 1484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 8486 "Parser/parser.cc"
    break;

  case 326:
#line 1486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 8492 "Parser/parser.cc"
    break;

  case 331:
#line 1501 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-4].flag), (yyvsp[-2].constant), 0 ) ); }
#line 8498 "Parser/parser.cc"
    break;

  case 332:
#line 1503 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-6].flag), (yyvsp[-4].constant), (yyvsp[-2].en) ) ); }
#line 8504 "Parser/parser.cc"
    break;

  case 333:
#line 1505 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-8].flag), (yyvsp[-6].constant), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 8510 "Parser/parser.cc"
    break;

  case 334:
#line 1507 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-10].flag), (yyvsp[-8].constant), (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 8516 "Parser/parser.cc"
    break;

  case 335:
#line 1509 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-12].flag), (yyvsp[-9].constant), 0, (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].label) ) ); }
#line 8522 "Parser/parser.cc"
    break;

  case 336:
#line 1514 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = false; }
#line 8528 "Parser/parser.cc"
    break;

  case 337:
#line 1516 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = true; }
#line 8534 "Parser/parser.cc"
    break;

  case 338:
#line 1521 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8540 "Parser/parser.cc"
    break;

  case 341:
#line 1528 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 8546 "Parser/parser.cc"
    break;

  case 342:
#line 1533 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( nullptr, (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 8552 "Parser/parser.cc"
    break;

  case 343:
#line 1535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( (yyvsp[-5].tok), (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 8558 "Parser/parser.cc"
    break;

  case 344:
#line 1540 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8564 "Parser/parser.cc"
    break;

  case 345:
#line 1542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 8570 "Parser/parser.cc"
    break;

  case 346:
#line 1544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( (yyvsp[0].constant) ) )); }
#line 8576 "Parser/parser.cc"
    break;

  case 347:
#line 1549 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = new LabelNode(); (yyval.label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 8585 "Parser/parser.cc"
    break;

  case 348:
#line 1554 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = (yyvsp[-2].label); (yyvsp[-2].label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 8594 "Parser/parser.cc"
    break;

  case 349:
#line 1564 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8600 "Parser/parser.cc"
    break;

  case 352:
#line 1571 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 8606 "Parser/parser.cc"
    break;

  case 353:
#line 1576 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8612 "Parser/parser.cc"
    break;

  case 355:
#line 1582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 8618 "Parser/parser.cc"
    break;

  case 356:
#line 1584 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-2].decl) ); }
#line 8624 "Parser/parser.cc"
    break;

  case 366:
#line 1610 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-4].en), (yyvsp[-2].constant) ); }
#line 8630 "Parser/parser.cc"
    break;

  case 367:
#line 1612 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-2].en), build_constantStr( *new string( "\"\"" ) ) ); }
#line 8636 "Parser/parser.cc"
    break;

  case 371:
#line 1630 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 8642 "Parser/parser.cc"
    break;

  case 373:
#line 1636 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].in) ); }
#line 8648 "Parser/parser.cc"
    break;

  case 374:
#line 1640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 8654 "Parser/parser.cc"
    break;

  case 375:
#line 1642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].in) ) ); }
#line 8660 "Parser/parser.cc"
    break;

  case 376:
#line 1649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 8666 "Parser/parser.cc"
    break;

  case 377:
#line 1651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 8672 "Parser/parser.cc"
    break;

  case 378:
#line 1653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 8678 "Parser/parser.cc"
    break;

  case 380:
#line 1659 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 8684 "Parser/parser.cc"
    break;

  case 381:
#line 1661 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 8690 "Parser/parser.cc"
    break;

  case 382:
#line 1663 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 8696 "Parser/parser.cc"
    break;

  case 383:
#line 1665 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeClone( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->appendList( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 8707 "Parser/parser.cc"
    break;

  case 384:
#line 1698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), 0 )->addQualifiers( (yyvsp[0].decl) ); }
#line 8713 "Parser/parser.cc"
    break;

  case 385:
#line 1700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), 0 )->addQualifiers( (yyvsp[0].decl) ); }
#line 8719 "Parser/parser.cc"
    break;

  case 386:
#line 1705 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 8725 "Parser/parser.cc"
    break;

  case 387:
#line 1708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-6].decl)->appendList( (yyvsp[-2].decl) ) ); }
#line 8731 "Parser/parser.cc"
    break;

  case 388:
#line 1713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 8740 "Parser/parser.cc"
    break;

  case 389:
#line 1718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 8749 "Parser/parser.cc"
    break;

  case 390:
#line 1723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "3" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 8758 "Parser/parser.cc"
    break;

  case 391:
#line 1734 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "4" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef();
		}
#line 8767 "Parser/parser.cc"
    break;

  case 392:
#line 1739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "5" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 8776 "Parser/parser.cc"
    break;

  case 393:
#line 1744 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "6" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[-3].decl) )->addTypedef();
		}
#line 8785 "Parser/parser.cc"
    break;

  case 394:
#line 1749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "7" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-2].decl) )->addTypedef();
		}
#line 8794 "Parser/parser.cc"
    break;

  case 395:
#line 1754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "8" );
			(yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-3].decl) )->addTypedef()->addType( (yyvsp[-3].decl) );
		}
#line 8803 "Parser/parser.cc"
    break;

  case 396:
#line 1763 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 8811 "Parser/parser.cc"
    break;

  case 397:
#line 1767 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 8819 "Parser/parser.cc"
    break;

  case 398:
#line 1774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 8825 "Parser/parser.cc"
    break;

  case 402:
#line 1784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 8831 "Parser/parser.cc"
    break;

  case 403:
#line 1786 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ) ); }
#line 8837 "Parser/parser.cc"
    break;

  case 416:
#line 1825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8843 "Parser/parser.cc"
    break;

  case 419:
#line 1837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 8849 "Parser/parser.cc"
    break;

  case 422:
#line 1847 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Const ); }
#line 8855 "Parser/parser.cc"
    break;

  case 423:
#line 1849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Restrict ); }
#line 8861 "Parser/parser.cc"
    break;

  case 424:
#line 1851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Volatile ); }
#line 8867 "Parser/parser.cc"
    break;

  case 425:
#line 1853 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Atomic ); }
#line 8873 "Parser/parser.cc"
    break;

  case 427:
#line 1859 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newForall( (yyvsp[-1].decl) ); }
#line 8879 "Parser/parser.cc"
    break;

  case 429:
#line 1865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 8885 "Parser/parser.cc"
    break;

  case 430:
#line 1867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 8891 "Parser/parser.cc"
    break;

  case 432:
#line 1878 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 8897 "Parser/parser.cc"
    break;

  case 433:
#line 1883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Extern ); }
#line 8903 "Parser/parser.cc"
    break;

  case 434:
#line 1885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Static ); }
#line 8909 "Parser/parser.cc"
    break;

  case 435:
#line 1887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Auto ); }
#line 8915 "Parser/parser.cc"
    break;

  case 436:
#line 1889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Register ); }
#line 8921 "Parser/parser.cc"
    break;

  case 437:
#line 1891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Threadlocal ); }
#line 8927 "Parser/parser.cc"
    break;

  case 438:
#line 1894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Inline ); }
#line 8933 "Parser/parser.cc"
    break;

  case 439:
#line 1896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Fortran ); }
#line 8939 "Parser/parser.cc"
    break;

  case 440:
#line 1898 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Noreturn ); }
#line 8945 "Parser/parser.cc"
    break;

  case 441:
#line 1903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 8951 "Parser/parser.cc"
    break;

  case 442:
#line 1905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Bool ); }
#line 8957 "Parser/parser.cc"
    break;

  case 443:
#line 1907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Char ); }
#line 8963 "Parser/parser.cc"
    break;

  case 444:
#line 1909 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int ); }
#line 8969 "Parser/parser.cc"
    break;

  case 445:
#line 1911 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 ); }
#line 8975 "Parser/parser.cc"
    break;

  case 446:
#line 1913 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 )->addType( DeclarationNode::newSignedNess( DeclarationNode::Unsigned ) ); }
#line 8981 "Parser/parser.cc"
    break;

  case 447:
#line 1915 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Float ); }
#line 8987 "Parser/parser.cc"
    break;

  case 448:
#line 1917 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Double ); }
#line 8993 "Parser/parser.cc"
    break;

  case 449:
#line 1919 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat80 ); }
#line 8999 "Parser/parser.cc"
    break;

  case 450:
#line 1921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat128 ); }
#line 9005 "Parser/parser.cc"
    break;

  case 451:
#line 1923 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat16 ); }
#line 9011 "Parser/parser.cc"
    break;

  case 452:
#line 1925 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32 ); }
#line 9017 "Parser/parser.cc"
    break;

  case 453:
#line 1927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32x ); }
#line 9023 "Parser/parser.cc"
    break;

  case 454:
#line 1929 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64 ); }
#line 9029 "Parser/parser.cc"
    break;

  case 455:
#line 1931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64x ); }
#line 9035 "Parser/parser.cc"
    break;

  case 456:
#line 1933 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat128 ); }
#line 9041 "Parser/parser.cc"
    break;

  case 457:
#line 1935 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9047 "Parser/parser.cc"
    break;

  case 458:
#line 1937 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9053 "Parser/parser.cc"
    break;

  case 459:
#line 1939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9059 "Parser/parser.cc"
    break;

  case 460:
#line 1941 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Complex ); }
#line 9065 "Parser/parser.cc"
    break;

  case 461:
#line 1943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Imaginary ); }
#line 9071 "Parser/parser.cc"
    break;

  case 462:
#line 1945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Signed ); }
#line 9077 "Parser/parser.cc"
    break;

  case 463:
#line 1947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Unsigned ); }
#line 9083 "Parser/parser.cc"
    break;

  case 464:
#line 1949 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Short ); }
#line 9089 "Parser/parser.cc"
    break;

  case 465:
#line 1951 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Long ); }
#line 9095 "Parser/parser.cc"
    break;

  case 466:
#line 1953 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Valist ); }
#line 9101 "Parser/parser.cc"
    break;

  case 467:
#line 1955 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::AutoType ); }
#line 9107 "Parser/parser.cc"
    break;

  case 469:
#line 1961 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9113 "Parser/parser.cc"
    break;

  case 471:
#line 1967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVtableType( (yyvsp[-2].decl) ); }
#line 9119 "Parser/parser.cc"
    break;

  case 472:
#line 1973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9125 "Parser/parser.cc"
    break;

  case 473:
#line 1975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9131 "Parser/parser.cc"
    break;

  case 475:
#line 1982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9137 "Parser/parser.cc"
    break;

  case 476:
#line 1984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9143 "Parser/parser.cc"
    break;

  case 477:
#line 1986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9149 "Parser/parser.cc"
    break;

  case 478:
#line 1988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 9155 "Parser/parser.cc"
    break;

  case 480:
#line 1995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9161 "Parser/parser.cc"
    break;

  case 482:
#line 2001 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9167 "Parser/parser.cc"
    break;

  case 483:
#line 2003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9173 "Parser/parser.cc"
    break;

  case 484:
#line 2005 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 9179 "Parser/parser.cc"
    break;

  case 485:
#line 2010 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 9185 "Parser/parser.cc"
    break;

  case 486:
#line 2012 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en) ); }
#line 9191 "Parser/parser.cc"
    break;

  case 487:
#line 2014 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 9197 "Parser/parser.cc"
    break;

  case 488:
#line 2016 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en), true ); }
#line 9203 "Parser/parser.cc"
    break;

  case 489:
#line 2018 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Zero ); }
#line 9209 "Parser/parser.cc"
    break;

  case 490:
#line 2020 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::One ); }
#line 9215 "Parser/parser.cc"
    break;

  case 492:
#line 2026 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9221 "Parser/parser.cc"
    break;

  case 493:
#line 2028 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9227 "Parser/parser.cc"
    break;

  case 494:
#line 2030 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9233 "Parser/parser.cc"
    break;

  case 496:
#line 2036 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 9239 "Parser/parser.cc"
    break;

  case 497:
#line 2038 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 9245 "Parser/parser.cc"
    break;

  case 498:
#line 2040 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 9254 "Parser/parser.cc"
    break;

  case 500:
#line 2049 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9260 "Parser/parser.cc"
    break;

  case 501:
#line 2051 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9266 "Parser/parser.cc"
    break;

  case 502:
#line 2053 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9272 "Parser/parser.cc"
    break;

  case 504:
#line 2059 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9278 "Parser/parser.cc"
    break;

  case 505:
#line 2061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9284 "Parser/parser.cc"
    break;

  case 507:
#line 2067 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9290 "Parser/parser.cc"
    break;

  case 508:
#line 2069 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9296 "Parser/parser.cc"
    break;

  case 509:
#line 2071 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9302 "Parser/parser.cc"
    break;

  case 511:
#line 2077 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9308 "Parser/parser.cc"
    break;

  case 512:
#line 2079 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9314 "Parser/parser.cc"
    break;

  case 513:
#line 2084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypedef( (yyvsp[0].tok) ); }
#line 9320 "Parser/parser.cc"
    break;

  case 514:
#line 2086 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 9326 "Parser/parser.cc"
    break;

  case 515:
#line 2088 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 9332 "Parser/parser.cc"
    break;

  case 517:
#line 2091 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), (yyvsp[0].decl) ); }
#line 9338 "Parser/parser.cc"
    break;

  case 518:
#line 2093 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), (yyvsp[0].decl) ); }
#line 9344 "Parser/parser.cc"
    break;

  case 519:
#line 2098 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[0].tok), nullptr ); }
#line 9350 "Parser/parser.cc"
    break;

  case 520:
#line 2100 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-2].tok), nullptr ); }
#line 9356 "Parser/parser.cc"
    break;

  case 521:
#line 2102 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 9362 "Parser/parser.cc"
    break;

  case 526:
#line 2117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 9368 "Parser/parser.cc"
    break;

  case 527:
#line 2119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 9374 "Parser/parser.cc"
    break;

  case 528:
#line 2121 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 9383 "Parser/parser.cc"
    break;

  case 529:
#line 2126 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) ); }
#line 9389 "Parser/parser.cc"
    break;

  case 530:
#line 2128 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 9398 "Parser/parser.cc"
    break;

  case 531:
#line 2133 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypedef( (yyvsp[-5].tok) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 9407 "Parser/parser.cc"
    break;

  case 532:
#line 2138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 9416 "Parser/parser.cc"
    break;

  case 533:
#line 2143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeGen( (yyvsp[-5].tok), nullptr );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 9425 "Parser/parser.cc"
    break;

  case 535:
#line 2152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9431 "Parser/parser.cc"
    break;

  case 536:
#line 2154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 9437 "Parser/parser.cc"
    break;

  case 537:
#line 2159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 9447 "Parser/parser.cc"
    break;

  case 538:
#line 2165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 9462 "Parser/parser.cc"
    break;

  case 541:
#line 2184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Struct; }
#line 9468 "Parser/parser.cc"
    break;

  case 542:
#line 2186 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Union; }
#line 9474 "Parser/parser.cc"
    break;

  case 543:
#line 2188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Exception; }
#line 9480 "Parser/parser.cc"
    break;

  case 544:
#line 2194 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 9486 "Parser/parser.cc"
    break;

  case 545:
#line 2196 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 9492 "Parser/parser.cc"
    break;

  case 546:
#line 2198 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Generator; }
#line 9498 "Parser/parser.cc"
    break;

  case 547:
#line 2200 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor generator is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 9504 "Parser/parser.cc"
    break;

  case 548:
#line 2202 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Coroutine; }
#line 9510 "Parser/parser.cc"
    break;

  case 549:
#line 2204 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor coroutine is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 9516 "Parser/parser.cc"
    break;

  case 550:
#line 2206 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Thread; }
#line 9522 "Parser/parser.cc"
    break;

  case 551:
#line 2208 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor thread is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 9528 "Parser/parser.cc"
    break;

  case 552:
#line 2213 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9534 "Parser/parser.cc"
    break;

  case 553:
#line 2215 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 9540 "Parser/parser.cc"
    break;

  case 554:
#line 2220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); }
#line 9546 "Parser/parser.cc"
    break;

  case 555:
#line 2222 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 9552 "Parser/parser.cc"
    break;

  case 556:
#line 2224 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-1].decl) ) {								// field declarator ?
				(yyvsp[-1].decl) = DeclarationNode::newName( nullptr );
			} // if
			(yyvsp[-1].decl)->inLine = true;
			(yyval.decl) = distAttr( (yyvsp[-2].decl), (yyvsp[-1].decl) );					// mark all fields in list
			distInl( (yyvsp[-1].decl) );
		}
#line 9565 "Parser/parser.cc"
    break;

  case 557:
#line 2233 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9571 "Parser/parser.cc"
    break;

  case 560:
#line 2237 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 9577 "Parser/parser.cc"
    break;

  case 561:
#line 2239 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 9583 "Parser/parser.cc"
    break;

  case 564:
#line 2246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9589 "Parser/parser.cc"
    break;

  case 566:
#line 2249 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 9595 "Parser/parser.cc"
    break;

  case 567:
#line 2254 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].en) ); }
#line 9601 "Parser/parser.cc"
    break;

  case 568:
#line 2257 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 9607 "Parser/parser.cc"
    break;

  case 569:
#line 2260 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 9613 "Parser/parser.cc"
    break;

  case 570:
#line 2265 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9619 "Parser/parser.cc"
    break;

  case 572:
#line 2268 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 9625 "Parser/parser.cc"
    break;

  case 574:
#line 2279 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 9631 "Parser/parser.cc"
    break;

  case 575:
#line 2281 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 9637 "Parser/parser.cc"
    break;

  case 577:
#line 2288 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 9643 "Parser/parser.cc"
    break;

  case 578:
#line 2293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9649 "Parser/parser.cc"
    break;

  case 580:
#line 2299 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 9655 "Parser/parser.cc"
    break;

  case 581:
#line 2304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-4].decl) ); }
#line 9661 "Parser/parser.cc"
    break;

  case 582:
#line 2306 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); }
#line 9667 "Parser/parser.cc"
    break;

  case 583:
#line 2308 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].tok), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) ); }
#line 9673 "Parser/parser.cc"
    break;

  case 584:
#line 2311 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-4].decl)->name, (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 9679 "Parser/parser.cc"
    break;

  case 585:
#line 2313 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-6].decl)->storageClasses.val != 0 || (yyvsp[-6].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			SemanticError( yylloc, "Typed enumeration is currently unimplemented." ); (yyval.decl) = nullptr;
		}
#line 9688 "Parser/parser.cc"
    break;

  case 586:
#line 2318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].decl)->storageClasses.val != 0 || (yyvsp[-4].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			typedefTable.makeTypedef( *(yyvsp[-1].tok) );
		}
#line 9697 "Parser/parser.cc"
    break;

  case 587:
#line 2323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typed enumeration is currently unimplemented." ); (yyval.decl) = nullptr;
		}
#line 9705 "Parser/parser.cc"
    break;

  case 588:
#line 2327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-8].decl)->storageClasses.val != 0 || (yyvsp[-8].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			typedefTable.makeTypedef( *(yyvsp[-5].decl)->name );
			SemanticError( yylloc, "Typed enumeration is currently unimplemented." ); (yyval.decl) = nullptr;
		}
#line 9715 "Parser/parser.cc"
    break;

  case 590:
#line 2337 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); (yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), 0, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 9721 "Parser/parser.cc"
    break;

  case 591:
#line 2339 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].decl)->type->symbolic.name );	(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].decl)->type->symbolic.name, 0, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 9727 "Parser/parser.cc"
    break;

  case 592:
#line 2344 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumConstant( (yyvsp[-1].tok), (yyvsp[0].en) ); }
#line 9733 "Parser/parser.cc"
    break;

  case 593:
#line 2346 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumConstant( new string("inline"), nullptr ); }
#line 9739 "Parser/parser.cc"
    break;

  case 594:
#line 2348 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( DeclarationNode::newEnumConstant( (yyvsp[-1].tok), (yyvsp[0].en) ) ); }
#line 9745 "Parser/parser.cc"
    break;

  case 595:
#line 2350 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( DeclarationNode::newEnumConstant( new string("inline"), nullptr ) ); }
#line 9751 "Parser/parser.cc"
    break;

  case 596:
#line 2355 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9757 "Parser/parser.cc"
    break;

  case 597:
#line 2359 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].in)->get_expression(); }
#line 9763 "Parser/parser.cc"
    break;

  case 598:
#line 2364 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 9769 "Parser/parser.cc"
    break;

  case 599:
#line 2366 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9775 "Parser/parser.cc"
    break;

  case 602:
#line 2370 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9781 "Parser/parser.cc"
    break;

  case 603:
#line 2372 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 9787 "Parser/parser.cc"
    break;

  case 604:
#line 2374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 9793 "Parser/parser.cc"
    break;

  case 606:
#line 2382 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9799 "Parser/parser.cc"
    break;

  case 607:
#line 2384 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9805 "Parser/parser.cc"
    break;

  case 608:
#line 2386 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-8].decl)->appendList( (yyvsp[-4].decl) )->appendList( (yyvsp[0].decl) ); }
#line 9811 "Parser/parser.cc"
    break;

  case 610:
#line 2392 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9817 "Parser/parser.cc"
    break;

  case 611:
#line 2397 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9823 "Parser/parser.cc"
    break;

  case 612:
#line 2399 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9829 "Parser/parser.cc"
    break;

  case 614:
#line 2402 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 9835 "Parser/parser.cc"
    break;

  case 617:
#line 2409 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9841 "Parser/parser.cc"
    break;

  case 618:
#line 2411 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9847 "Parser/parser.cc"
    break;

  case 620:
#line 2420 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 9853 "Parser/parser.cc"
    break;

  case 621:
#line 2423 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 9859 "Parser/parser.cc"
    break;

  case 622:
#line 2425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 9865 "Parser/parser.cc"
    break;

  case 627:
#line 2435 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9871 "Parser/parser.cc"
    break;

  case 629:
#line 2442 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 9877 "Parser/parser.cc"
    break;

  case 630:
#line 2444 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 9883 "Parser/parser.cc"
    break;

  case 631:
#line 2449 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 9889 "Parser/parser.cc"
    break;

  case 632:
#line 2451 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 9895 "Parser/parser.cc"
    break;

  case 633:
#line 2460 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 9901 "Parser/parser.cc"
    break;

  case 634:
#line 2462 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 9907 "Parser/parser.cc"
    break;

  case 640:
#line 2475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 9913 "Parser/parser.cc"
    break;

  case 643:
#line 2485 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 9919 "Parser/parser.cc"
    break;

  case 644:
#line 2486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (yyvsp[-1].op) == OperKinds::Assign ? (yyvsp[0].in) : (yyvsp[0].in)->set_maybeConstructed( false ); }
#line 9925 "Parser/parser.cc"
    break;

  case 645:
#line 2487 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.in) = new InitializerNode( true ); }
#line 9931 "Parser/parser.cc"
    break;

  case 646:
#line 2488 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 9937 "Parser/parser.cc"
    break;

  case 647:
#line 2492 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = new InitializerNode( (yyvsp[0].en) ); }
#line 9943 "Parser/parser.cc"
    break;

  case 648:
#line 2493 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 9949 "Parser/parser.cc"
    break;

  case 649:
#line 2498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 9955 "Parser/parser.cc"
    break;

  case 651:
#line 2500 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ); }
#line 9961 "Parser/parser.cc"
    break;

  case 652:
#line 2501 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (InitializerNode *)( (yyvsp[-2].in)->set_last( (yyvsp[0].in) ) ); }
#line 9967 "Parser/parser.cc"
    break;

  case 653:
#line 2502 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.in) = (InitializerNode *)((yyvsp[-3].in)->set_last( (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ) )); }
#line 9973 "Parser/parser.cc"
    break;

  case 655:
#line 2518 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[-1].tok) ) ); }
#line 9979 "Parser/parser.cc"
    break;

  case 657:
#line 2524 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-1].en)->set_last( (yyvsp[0].en) )); }
#line 9985 "Parser/parser.cc"
    break;

  case 658:
#line 2530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 9991 "Parser/parser.cc"
    break;

  case 659:
#line 2533 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 9997 "Parser/parser.cc"
    break;

  case 660:
#line 2535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 10003 "Parser/parser.cc"
    break;

  case 661:
#line 2537 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-4].en) ), maybeMoveBuild<Expression>( (yyvsp[-2].en) ) ) ); }
#line 10009 "Parser/parser.cc"
    break;

  case 662:
#line 2539 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 10015 "Parser/parser.cc"
    break;

  case 664:
#line 2563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl) ); }
#line 10021 "Parser/parser.cc"
    break;

  case 665:
#line 2568 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10027 "Parser/parser.cc"
    break;

  case 666:
#line 2570 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 10033 "Parser/parser.cc"
    break;

  case 667:
#line 2575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "9" );
			if ( (yyvsp[-1].tclass) == TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 10044 "Parser/parser.cc"
    break;

  case 668:
#line 2582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 10050 "Parser/parser.cc"
    break;

  case 669:
#line 2584 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "9" ); }
#line 10056 "Parser/parser.cc"
    break;

  case 670:
#line 2586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 10062 "Parser/parser.cc"
    break;

  case 671:
#line 2588 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "9" );
			(yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 10071 "Parser/parser.cc"
    break;

  case 672:
#line 2594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 10077 "Parser/parser.cc"
    break;

  case 673:
#line 2599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 10083 "Parser/parser.cc"
    break;

  case 674:
#line 2601 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 10089 "Parser/parser.cc"
    break;

  case 675:
#line 2603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::DStype; }
#line 10095 "Parser/parser.cc"
    break;

  case 676:
#line 2607 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 10101 "Parser/parser.cc"
    break;

  case 677:
#line 2612 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 10107 "Parser/parser.cc"
    break;

  case 678:
#line 2614 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 10113 "Parser/parser.cc"
    break;

  case 679:
#line 2616 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ftype; }
#line 10119 "Parser/parser.cc"
    break;

  case 680:
#line 2618 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 10125 "Parser/parser.cc"
    break;

  case 681:
#line 2623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10131 "Parser/parser.cc"
    break;

  case 684:
#line 2630 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 10137 "Parser/parser.cc"
    break;

  case 685:
#line 2635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 10143 "Parser/parser.cc"
    break;

  case 686:
#line 2637 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 10149 "Parser/parser.cc"
    break;

  case 687:
#line 2644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 10155 "Parser/parser.cc"
    break;

  case 689:
#line 2647 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 10161 "Parser/parser.cc"
    break;

  case 690:
#line 2649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)( (yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 10167 "Parser/parser.cc"
    break;

  case 691:
#line 2654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 10173 "Parser/parser.cc"
    break;

  case 692:
#line 2656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10179 "Parser/parser.cc"
    break;

  case 693:
#line 2658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 10185 "Parser/parser.cc"
    break;

  case 694:
#line 2663 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 10191 "Parser/parser.cc"
    break;

  case 695:
#line 2665 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 10197 "Parser/parser.cc"
    break;

  case 696:
#line 2670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "10" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), 0 );
		}
#line 10206 "Parser/parser.cc"
    break;

  case 697:
#line 2675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "11" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 10215 "Parser/parser.cc"
    break;

  case 698:
#line 2683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), 0 ); }
#line 10221 "Parser/parser.cc"
    break;

  case 699:
#line 2685 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-8].tok), (yyvsp[-6].decl), (yyvsp[-2].decl) ); }
#line 10227 "Parser/parser.cc"
    break;

  case 701:
#line 2691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl) ); }
#line 10233 "Parser/parser.cc"
    break;

  case 706:
#line 2703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 10239 "Parser/parser.cc"
    break;

  case 707:
#line 2708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10245 "Parser/parser.cc"
    break;

  case 708:
#line 2710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 10251 "Parser/parser.cc"
    break;

  case 710:
#line 2718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl);	}
#line 10257 "Parser/parser.cc"
    break;

  case 711:
#line 2723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10263 "Parser/parser.cc"
    break;

  case 712:
#line 2725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->appendList( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 10269 "Parser/parser.cc"
    break;

  case 713:
#line 2730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10275 "Parser/parser.cc"
    break;

  case 715:
#line 2735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 10281 "Parser/parser.cc"
    break;

  case 716:
#line 2739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 10287 "Parser/parser.cc"
    break;

  case 717:
#line 2744 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( (yyvsp[0].tok) ) ) ); }
#line 10293 "Parser/parser.cc"
    break;

  case 720:
#line 2748 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 10302 "Parser/parser.cc"
    break;

  case 721:
#line 2753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( false, (yyvsp[-2].constant), 0 ) ) ); }
#line 10308 "Parser/parser.cc"
    break;

  case 722:
#line 2755 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = LinkageSpec::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 10317 "Parser/parser.cc"
    break;

  case 723:
#line 2760 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-1].decl);
		}
#line 10327 "Parser/parser.cc"
    break;

  case 724:
#line 2766 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = LinkageSpec::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 10336 "Parser/parser.cc"
    break;

  case 725:
#line 2771 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10346 "Parser/parser.cc"
    break;

  case 726:
#line 2777 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 10355 "Parser/parser.cc"
    break;

  case 727:
#line 2782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10365 "Parser/parser.cc"
    break;

  case 728:
#line 2788 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 10374 "Parser/parser.cc"
    break;

  case 729:
#line 2793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10384 "Parser/parser.cc"
    break;

  case 730:
#line 2799 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.val) || (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 10393 "Parser/parser.cc"
    break;

  case 731:
#line 2804 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10403 "Parser/parser.cc"
    break;

  case 733:
#line 2819 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].sn) ); }
#line 10409 "Parser/parser.cc"
    break;

  case 734:
#line 2821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].sn) ); }
#line 10415 "Parser/parser.cc"
    break;

  case 735:
#line 2826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; forall = false; }
#line 10421 "Parser/parser.cc"
    break;

  case 736:
#line 2828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); forall = false; }
#line 10427 "Parser/parser.cc"
    break;

  case 737:
#line 2833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10438 "Parser/parser.cc"
    break;

  case 738:
#line 2840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 10447 "Parser/parser.cc"
    break;

  case 739:
#line 2845 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 10456 "Parser/parser.cc"
    break;

  case 740:
#line 2851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 10462 "Parser/parser.cc"
    break;

  case 741:
#line 2854 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 10468 "Parser/parser.cc"
    break;

  case 742:
#line 2857 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10474 "Parser/parser.cc"
    break;

  case 743:
#line 2861 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-4].decl) );
		}
#line 10483 "Parser/parser.cc"
    break;

  case 744:
#line 2867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10489 "Parser/parser.cc"
    break;

  case 745:
#line 2870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10495 "Parser/parser.cc"
    break;

  case 746:
#line 2873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 10501 "Parser/parser.cc"
    break;

  case 750:
#line 2884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 10507 "Parser/parser.cc"
    break;

  case 751:
#line 2889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10513 "Parser/parser.cc"
    break;

  case 752:
#line 2891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = (yyvsp[-2].constant);
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 10523 "Parser/parser.cc"
    break;

  case 753:
#line 2900 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10529 "Parser/parser.cc"
    break;

  case 756:
#line 2907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10535 "Parser/parser.cc"
    break;

  case 757:
#line 2912 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 10541 "Parser/parser.cc"
    break;

  case 759:
#line 2918 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10547 "Parser/parser.cc"
    break;

  case 760:
#line 2923 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10553 "Parser/parser.cc"
    break;

  case 761:
#line 2925 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 10559 "Parser/parser.cc"
    break;

  case 762:
#line 2927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 10565 "Parser/parser.cc"
    break;

  case 767:
#line 2936 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 10571 "Parser/parser.cc"
    break;

  case 768:
#line 2938 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 10577 "Parser/parser.cc"
    break;

  case 769:
#line 2973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 10583 "Parser/parser.cc"
    break;

  case 770:
#line 2975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10589 "Parser/parser.cc"
    break;

  case 771:
#line 2980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10595 "Parser/parser.cc"
    break;

  case 773:
#line 2983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10601 "Parser/parser.cc"
    break;

  case 774:
#line 2985 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10607 "Parser/parser.cc"
    break;

  case 775:
#line 2990 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10613 "Parser/parser.cc"
    break;

  case 776:
#line 2992 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10619 "Parser/parser.cc"
    break;

  case 777:
#line 2994 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10625 "Parser/parser.cc"
    break;

  case 778:
#line 2996 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10631 "Parser/parser.cc"
    break;

  case 779:
#line 3001 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 10637 "Parser/parser.cc"
    break;

  case 780:
#line 3003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10643 "Parser/parser.cc"
    break;

  case 781:
#line 3005 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10649 "Parser/parser.cc"
    break;

  case 782:
#line 3007 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10655 "Parser/parser.cc"
    break;

  case 783:
#line 3009 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10661 "Parser/parser.cc"
    break;

  case 784:
#line 3011 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10667 "Parser/parser.cc"
    break;

  case 785:
#line 3013 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10673 "Parser/parser.cc"
    break;

  case 786:
#line 3018 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10679 "Parser/parser.cc"
    break;

  case 787:
#line 3020 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 10685 "Parser/parser.cc"
    break;

  case 788:
#line 3022 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10691 "Parser/parser.cc"
    break;

  case 789:
#line 3024 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10697 "Parser/parser.cc"
    break;

  case 790:
#line 3033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10703 "Parser/parser.cc"
    break;

  case 792:
#line 3036 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10709 "Parser/parser.cc"
    break;

  case 793:
#line 3041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10715 "Parser/parser.cc"
    break;

  case 794:
#line 3043 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10721 "Parser/parser.cc"
    break;

  case 795:
#line 3045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 10727 "Parser/parser.cc"
    break;

  case 796:
#line 3047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10733 "Parser/parser.cc"
    break;

  case 797:
#line 3049 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10739 "Parser/parser.cc"
    break;

  case 798:
#line 3054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10745 "Parser/parser.cc"
    break;

  case 799:
#line 3056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10751 "Parser/parser.cc"
    break;

  case 800:
#line 3058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10757 "Parser/parser.cc"
    break;

  case 801:
#line 3060 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10763 "Parser/parser.cc"
    break;

  case 802:
#line 3065 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10769 "Parser/parser.cc"
    break;

  case 803:
#line 3067 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10775 "Parser/parser.cc"
    break;

  case 804:
#line 3069 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10781 "Parser/parser.cc"
    break;

  case 805:
#line 3071 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10787 "Parser/parser.cc"
    break;

  case 806:
#line 3073 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10793 "Parser/parser.cc"
    break;

  case 807:
#line 3075 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10799 "Parser/parser.cc"
    break;

  case 811:
#line 3093 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 10805 "Parser/parser.cc"
    break;

  case 812:
#line 3095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10811 "Parser/parser.cc"
    break;

  case 813:
#line 3097 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 10817 "Parser/parser.cc"
    break;

  case 814:
#line 3099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10823 "Parser/parser.cc"
    break;

  case 815:
#line 3101 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10829 "Parser/parser.cc"
    break;

  case 816:
#line 3106 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10835 "Parser/parser.cc"
    break;

  case 817:
#line 3108 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10841 "Parser/parser.cc"
    break;

  case 818:
#line 3110 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10847 "Parser/parser.cc"
    break;

  case 819:
#line 3112 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10853 "Parser/parser.cc"
    break;

  case 820:
#line 3117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10859 "Parser/parser.cc"
    break;

  case 821:
#line 3119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10865 "Parser/parser.cc"
    break;

  case 822:
#line 3121 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10871 "Parser/parser.cc"
    break;

  case 823:
#line 3123 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10877 "Parser/parser.cc"
    break;

  case 824:
#line 3125 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10883 "Parser/parser.cc"
    break;

  case 825:
#line 3127 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10889 "Parser/parser.cc"
    break;

  case 826:
#line 3142 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "ID" );
		}
#line 10898 "Parser/parser.cc"
    break;

  case 827:
#line 3147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10904 "Parser/parser.cc"
    break;

  case 828:
#line 3152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10910 "Parser/parser.cc"
    break;

  case 830:
#line 3155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10916 "Parser/parser.cc"
    break;

  case 831:
#line 3157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10922 "Parser/parser.cc"
    break;

  case 832:
#line 3162 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10928 "Parser/parser.cc"
    break;

  case 833:
#line 3164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10934 "Parser/parser.cc"
    break;

  case 834:
#line 3166 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10940 "Parser/parser.cc"
    break;

  case 835:
#line 3168 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10946 "Parser/parser.cc"
    break;

  case 836:
#line 3173 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 10952 "Parser/parser.cc"
    break;

  case 837:
#line 3175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10958 "Parser/parser.cc"
    break;

  case 838:
#line 3177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10964 "Parser/parser.cc"
    break;

  case 839:
#line 3179 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10970 "Parser/parser.cc"
    break;

  case 840:
#line 3181 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10976 "Parser/parser.cc"
    break;

  case 841:
#line 3183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10982 "Parser/parser.cc"
    break;

  case 842:
#line 3185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10988 "Parser/parser.cc"
    break;

  case 843:
#line 3190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10994 "Parser/parser.cc"
    break;

  case 844:
#line 3192 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11000 "Parser/parser.cc"
    break;

  case 845:
#line 3194 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 11006 "Parser/parser.cc"
    break;

  case 846:
#line 3196 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11012 "Parser/parser.cc"
    break;

  case 847:
#line 3198 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11018 "Parser/parser.cc"
    break;

  case 848:
#line 3208 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11024 "Parser/parser.cc"
    break;

  case 849:
#line 3210 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11030 "Parser/parser.cc"
    break;

  case 851:
#line 3213 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11036 "Parser/parser.cc"
    break;

  case 852:
#line 3215 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11042 "Parser/parser.cc"
    break;

  case 853:
#line 3220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11048 "Parser/parser.cc"
    break;

  case 854:
#line 3222 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11054 "Parser/parser.cc"
    break;

  case 855:
#line 3224 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11060 "Parser/parser.cc"
    break;

  case 856:
#line 3229 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11066 "Parser/parser.cc"
    break;

  case 857:
#line 3231 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11072 "Parser/parser.cc"
    break;

  case 858:
#line 3233 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11078 "Parser/parser.cc"
    break;

  case 859:
#line 3235 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11084 "Parser/parser.cc"
    break;

  case 860:
#line 3240 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11090 "Parser/parser.cc"
    break;

  case 861:
#line 3242 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11096 "Parser/parser.cc"
    break;

  case 862:
#line 3244 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11102 "Parser/parser.cc"
    break;

  case 863:
#line 3258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11108 "Parser/parser.cc"
    break;

  case 864:
#line 3260 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11114 "Parser/parser.cc"
    break;

  case 866:
#line 3263 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11120 "Parser/parser.cc"
    break;

  case 867:
#line 3265 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11126 "Parser/parser.cc"
    break;

  case 868:
#line 3270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11132 "Parser/parser.cc"
    break;

  case 869:
#line 3272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11138 "Parser/parser.cc"
    break;

  case 870:
#line 3277 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11144 "Parser/parser.cc"
    break;

  case 871:
#line 3279 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11150 "Parser/parser.cc"
    break;

  case 872:
#line 3281 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11156 "Parser/parser.cc"
    break;

  case 873:
#line 3286 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11162 "Parser/parser.cc"
    break;

  case 874:
#line 3288 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11168 "Parser/parser.cc"
    break;

  case 875:
#line 3293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11174 "Parser/parser.cc"
    break;

  case 876:
#line 3295 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11180 "Parser/parser.cc"
    break;

  case 878:
#line 3313 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11186 "Parser/parser.cc"
    break;

  case 879:
#line 3315 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11192 "Parser/parser.cc"
    break;

  case 880:
#line 3320 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( 0, (yyvsp[0].op) ); }
#line 11198 "Parser/parser.cc"
    break;

  case 881:
#line 3322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 11204 "Parser/parser.cc"
    break;

  case 882:
#line 3324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11210 "Parser/parser.cc"
    break;

  case 883:
#line 3326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11216 "Parser/parser.cc"
    break;

  case 884:
#line 3328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11222 "Parser/parser.cc"
    break;

  case 886:
#line 3334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11228 "Parser/parser.cc"
    break;

  case 887:
#line 3336 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11234 "Parser/parser.cc"
    break;

  case 888:
#line 3338 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11240 "Parser/parser.cc"
    break;

  case 889:
#line 3343 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 11246 "Parser/parser.cc"
    break;

  case 890:
#line 3345 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11252 "Parser/parser.cc"
    break;

  case 891:
#line 3347 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11258 "Parser/parser.cc"
    break;

  case 892:
#line 3353 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false ); }
#line 11264 "Parser/parser.cc"
    break;

  case 893:
#line 3355 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false )->addArray( (yyvsp[0].decl) ); }
#line 11270 "Parser/parser.cc"
    break;

  case 894:
#line 3357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-4].en), 0, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].en), 0, false ) ); }
#line 11276 "Parser/parser.cc"
    break;

  case 896:
#line 3364 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), 0, false ); }
#line 11282 "Parser/parser.cc"
    break;

  case 897:
#line 3366 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 11288 "Parser/parser.cc"
    break;

  case 898:
#line 3368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newArray( (yyvsp[-2].en), 0, false ) ); }
#line 11294 "Parser/parser.cc"
    break;

  case 899:
#line 3370 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 11300 "Parser/parser.cc"
    break;

  case 901:
#line 3405 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 11306 "Parser/parser.cc"
    break;

  case 902:
#line 3407 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11312 "Parser/parser.cc"
    break;

  case 903:
#line 3409 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11318 "Parser/parser.cc"
    break;

  case 904:
#line 3414 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].op) ); }
#line 11324 "Parser/parser.cc"
    break;

  case 905:
#line 3416 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 11330 "Parser/parser.cc"
    break;

  case 906:
#line 3418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 11336 "Parser/parser.cc"
    break;

  case 907:
#line 3420 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11342 "Parser/parser.cc"
    break;

  case 908:
#line 3422 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11348 "Parser/parser.cc"
    break;

  case 910:
#line 3428 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11354 "Parser/parser.cc"
    break;

  case 911:
#line 3430 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11360 "Parser/parser.cc"
    break;

  case 912:
#line 3432 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11366 "Parser/parser.cc"
    break;

  case 913:
#line 3437 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 11372 "Parser/parser.cc"
    break;

  case 914:
#line 3439 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11378 "Parser/parser.cc"
    break;

  case 915:
#line 3441 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11384 "Parser/parser.cc"
    break;

  case 917:
#line 3448 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11390 "Parser/parser.cc"
    break;

  case 919:
#line 3459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false ); }
#line 11396 "Parser/parser.cc"
    break;

  case 920:
#line 3462 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 11402 "Parser/parser.cc"
    break;

  case 921:
#line 3464 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, (yyvsp[-2].decl), false ); }
#line 11408 "Parser/parser.cc"
    break;

  case 922:
#line 3467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 11414 "Parser/parser.cc"
    break;

  case 923:
#line 3469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 11420 "Parser/parser.cc"
    break;

  case 924:
#line 3471 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-4].decl), true ); }
#line 11426 "Parser/parser.cc"
    break;

  case 926:
#line 3486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11432 "Parser/parser.cc"
    break;

  case 927:
#line 3488 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11438 "Parser/parser.cc"
    break;

  case 928:
#line 3493 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( 0, (yyvsp[0].op) ); }
#line 11444 "Parser/parser.cc"
    break;

  case 929:
#line 3495 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 11450 "Parser/parser.cc"
    break;

  case 930:
#line 3497 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11456 "Parser/parser.cc"
    break;

  case 931:
#line 3499 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11462 "Parser/parser.cc"
    break;

  case 932:
#line 3501 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11468 "Parser/parser.cc"
    break;

  case 934:
#line 3507 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11474 "Parser/parser.cc"
    break;

  case 935:
#line 3509 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11480 "Parser/parser.cc"
    break;

  case 936:
#line 3511 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11486 "Parser/parser.cc"
    break;

  case 937:
#line 3516 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11492 "Parser/parser.cc"
    break;

  case 938:
#line 3518 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11498 "Parser/parser.cc"
    break;

  case 941:
#line 3528 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11504 "Parser/parser.cc"
    break;

  case 944:
#line 3539 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11510 "Parser/parser.cc"
    break;

  case 945:
#line 3541 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11516 "Parser/parser.cc"
    break;

  case 946:
#line 3543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11522 "Parser/parser.cc"
    break;

  case 947:
#line 3545 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11528 "Parser/parser.cc"
    break;

  case 948:
#line 3547 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11534 "Parser/parser.cc"
    break;

  case 949:
#line 3549 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11540 "Parser/parser.cc"
    break;

  case 950:
#line 3556 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11546 "Parser/parser.cc"
    break;

  case 951:
#line 3558 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11552 "Parser/parser.cc"
    break;

  case 952:
#line 3560 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11558 "Parser/parser.cc"
    break;

  case 953:
#line 3562 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 11564 "Parser/parser.cc"
    break;

  case 954:
#line 3564 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11570 "Parser/parser.cc"
    break;

  case 955:
#line 3567 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11576 "Parser/parser.cc"
    break;

  case 956:
#line 3569 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11582 "Parser/parser.cc"
    break;

  case 957:
#line 3571 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11588 "Parser/parser.cc"
    break;

  case 958:
#line 3573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 11594 "Parser/parser.cc"
    break;

  case 959:
#line 3575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11600 "Parser/parser.cc"
    break;

  case 960:
#line 3580 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 11606 "Parser/parser.cc"
    break;

  case 961:
#line 3582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 11612 "Parser/parser.cc"
    break;

  case 962:
#line 3587 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 11618 "Parser/parser.cc"
    break;

  case 963:
#line 3589 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl)->addQualifiers( (yyvsp[-4].decl) ), true ); }
#line 11624 "Parser/parser.cc"
    break;

  case 965:
#line 3616 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11630 "Parser/parser.cc"
    break;

  case 969:
#line 3627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11636 "Parser/parser.cc"
    break;

  case 970:
#line 3629 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11642 "Parser/parser.cc"
    break;

  case 971:
#line 3631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11648 "Parser/parser.cc"
    break;

  case 972:
#line 3633 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11654 "Parser/parser.cc"
    break;

  case 973:
#line 3635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11660 "Parser/parser.cc"
    break;

  case 974:
#line 3637 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11666 "Parser/parser.cc"
    break;

  case 975:
#line 3644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11672 "Parser/parser.cc"
    break;

  case 976:
#line 3646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11678 "Parser/parser.cc"
    break;

  case 977:
#line 3648 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11684 "Parser/parser.cc"
    break;

  case 978:
#line 3650 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11690 "Parser/parser.cc"
    break;

  case 979:
#line 3652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11696 "Parser/parser.cc"
    break;

  case 980:
#line 3654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11702 "Parser/parser.cc"
    break;

  case 981:
#line 3659 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 11708 "Parser/parser.cc"
    break;

  case 982:
#line 3661 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11714 "Parser/parser.cc"
    break;

  case 983:
#line 3663 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11720 "Parser/parser.cc"
    break;

  case 984:
#line 3670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 11726 "Parser/parser.cc"
    break;

  case 985:
#line 3672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 11732 "Parser/parser.cc"
    break;

  case 988:
#line 3696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 11738 "Parser/parser.cc"
    break;

  case 989:
#line 3698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 11744 "Parser/parser.cc"
    break;


#line 11748 "Parser/parser.cc"

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
#line 3701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "make install" //
// End: //
