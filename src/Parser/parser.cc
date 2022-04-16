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
#define YYLAST   19943

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  175
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  289
/* YYNRULES -- Number of rules.  */
#define YYNRULES  991
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  2011

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
    1223,  1225,  1227,  1229,  1231,  1233,  1235,  1237,  1239,  1244,
    1245,  1267,  1269,  1271,  1274,  1277,  1280,  1282,  1284,  1286,
    1289,  1292,  1294,  1297,  1304,  1306,  1308,  1310,  1312,  1317,
    1319,  1321,  1323,  1328,  1330,  1335,  1337,  1339,  1341,  1344,
    1348,  1351,  1355,  1357,  1359,  1361,  1363,  1365,  1367,  1369,
    1371,  1373,  1375,  1380,  1381,  1385,  1391,  1396,  1401,  1402,
    1406,  1410,  1415,  1416,  1422,  1426,  1428,  1430,  1432,  1435,
    1437,  1442,  1444,  1449,  1451,  1453,  1458,  1460,  1466,  1467,
    1471,  1472,  1473,  1474,  1478,  1483,  1484,  1486,  1488,  1490,
    1494,  1498,  1499,  1503,  1505,  1507,  1509,  1511,  1517,  1518,
    1524,  1525,  1529,  1530,  1535,  1537,  1543,  1544,  1546,  1551,
    1556,  1567,  1568,  1572,  1573,  1579,  1580,  1584,  1586,  1590,
    1592,  1596,  1597,  1601,  1602,  1606,  1607,  1608,  1612,  1614,
    1629,  1630,  1631,  1632,  1634,  1638,  1640,  1644,  1651,  1653,
    1655,  1660,  1661,  1663,  1665,  1667,  1699,  1702,  1707,  1709,
    1715,  1720,  1725,  1736,  1741,  1746,  1751,  1756,  1765,  1769,
    1776,  1778,  1779,  1780,  1786,  1788,  1793,  1794,  1795,  1804,
    1805,  1806,  1810,  1811,  1812,  1821,  1822,  1823,  1828,  1829,
    1838,  1839,  1844,  1845,  1849,  1851,  1853,  1855,  1857,  1861,
    1866,  1867,  1869,  1879,  1880,  1885,  1887,  1889,  1891,  1893,
    1896,  1898,  1900,  1905,  1907,  1909,  1911,  1913,  1915,  1917,
    1919,  1921,  1923,  1925,  1927,  1929,  1931,  1933,  1935,  1937,
    1939,  1941,  1943,  1945,  1947,  1949,  1951,  1953,  1955,  1957,
    1959,  1964,  1965,  1969,  1976,  1977,  1983,  1984,  1986,  1988,
    1990,  1995,  1997,  2002,  2003,  2005,  2007,  2012,  2014,  2016,
    2018,  2020,  2022,  2027,  2028,  2030,  2032,  2037,  2039,  2038,
    2042,  2050,  2051,  2053,  2055,  2060,  2061,  2063,  2068,  2069,
    2071,  2073,  2078,  2079,  2081,  2086,  2088,  2090,  2092,  2093,
    2095,  2100,  2102,  2104,  2109,  2110,  2114,  2115,  2120,  2119,
    2124,  2123,  2131,  2130,  2141,  2140,  2150,  2155,  2156,  2161,
    2167,  2181,  2182,  2186,  2188,  2190,  2196,  2198,  2200,  2202,
    2204,  2206,  2208,  2210,  2216,  2217,  2222,  2224,  2226,  2235,
    2237,  2238,  2239,  2241,  2243,  2244,  2249,  2250,  2251,  2256,
    2258,  2261,  2268,  2269,  2270,  2276,  2281,  2283,  2289,  2290,
    2296,  2297,  2301,  2306,  2309,  2308,  2312,  2315,  2325,  2324,
    2334,  2341,  2345,  2347,  2352,  2354,  2356,  2358,  2364,  2367,
    2373,  2374,  2376,  2377,  2378,  2380,  2382,  2389,  2390,  2392,
    2394,  2399,  2400,  2406,  2407,  2409,  2410,  2415,  2416,  2417,
    2419,  2427,  2428,  2430,  2433,  2435,  2439,  2440,  2441,  2443,
    2445,  2450,  2452,  2457,  2459,  2468,  2470,  2475,  2476,  2477,
    2481,  2482,  2483,  2488,  2489,  2494,  2495,  2496,  2497,  2501,
    2502,  2507,  2508,  2509,  2510,  2511,  2525,  2526,  2531,  2532,
    2538,  2540,  2543,  2545,  2547,  2570,  2571,  2577,  2578,  2584,
    2583,  2593,  2592,  2596,  2602,  2608,  2609,  2611,  2615,  2620,
    2622,  2624,  2626,  2632,  2633,  2637,  2638,  2643,  2645,  2652,
    2654,  2655,  2657,  2662,  2664,  2666,  2671,  2673,  2678,  2683,
    2691,  2693,  2698,  2699,  2704,  2705,  2709,  2710,  2711,  2716,
    2718,  2724,  2726,  2731,  2733,  2739,  2740,  2744,  2748,  2752,
    2754,  2755,  2756,  2761,  2764,  2763,  2775,  2774,  2786,  2785,
    2797,  2796,  2808,  2807,  2821,  2827,  2829,  2835,  2836,  2841,
    2848,  2853,  2859,  2862,  2865,  2869,  2875,  2878,  2881,  2886,
    2887,  2888,  2892,  2898,  2899,  2909,  2910,  2914,  2915,  2920,
    2925,  2926,  2932,  2933,  2935,  2940,  2941,  2942,  2943,  2944,
    2946,  2981,  2983,  2988,  2990,  2991,  2993,  2998,  3000,  3002,
    3004,  3009,  3011,  3013,  3015,  3017,  3019,  3021,  3026,  3028,
    3030,  3032,  3041,  3043,  3044,  3049,  3051,  3053,  3055,  3057,
    3062,  3064,  3066,  3068,  3073,  3075,  3077,  3079,  3081,  3083,
    3095,  3096,  3097,  3101,  3103,  3105,  3107,  3109,  3114,  3116,
    3118,  3120,  3125,  3127,  3129,  3131,  3133,  3135,  3150,  3155,
    3160,  3162,  3163,  3165,  3170,  3172,  3174,  3176,  3181,  3183,
    3185,  3187,  3189,  3191,  3193,  3198,  3200,  3202,  3204,  3206,
    3216,  3218,  3220,  3221,  3223,  3228,  3230,  3232,  3237,  3239,
    3241,  3243,  3248,  3250,  3252,  3266,  3268,  3270,  3271,  3273,
    3278,  3280,  3285,  3287,  3289,  3294,  3296,  3301,  3303,  3320,
    3321,  3323,  3328,  3330,  3332,  3334,  3336,  3341,  3342,  3344,
    3346,  3351,  3353,  3355,  3361,  3363,  3365,  3368,  3372,  3374,
    3376,  3378,  3412,  3413,  3415,  3417,  3422,  3424,  3426,  3428,
    3430,  3435,  3436,  3438,  3440,  3445,  3447,  3449,  3455,  3456,
    3458,  3467,  3470,  3472,  3475,  3477,  3479,  3493,  3494,  3496,
    3501,  3503,  3505,  3507,  3509,  3514,  3515,  3517,  3519,  3524,
    3526,  3534,  3535,  3536,  3541,  3542,  3547,  3549,  3551,  3553,
    3555,  3557,  3564,  3566,  3568,  3570,  3572,  3575,  3577,  3579,
    3581,  3583,  3588,  3590,  3592,  3597,  3623,  3624,  3626,  3630,
    3631,  3635,  3637,  3639,  3641,  3643,  3645,  3652,  3654,  3656,
    3658,  3660,  3662,  3667,  3669,  3671,  3678,  3680,  3698,  3700,
    3705,  3706
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

#define YYPACT_NINF (-1690)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-872)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
      71, 12139,   177,   276, 16418,   268, -1690, -1690, -1690, -1690,
   -1690, -1690, -1690, -1690, -1690, -1690, -1690,   254,   901,   263,
   -1690, -1690, -1690, -1690, -1690, -1690, -1690, -1690, -1690, -1690,
   -1690, -1690, -1690, -1690, -1690, -1690, -1690, -1690, -1690, -1690,
   -1690, -1690, -1690, -1690, -1690, -1690, -1690,    54,   455, -1690,
   -1690, -1690, -1690, -1690, -1690,  3751,  3751,   328, 12139,   404,
     440, -1690, -1690,   466, -1690, -1690, -1690, -1690, -1690, -1690,
   -1690, -1690, -1690,  3018, -1690,   416,   238, -1690, -1690, -1690,
   -1690, -1690, 16268, -1690, -1690,   432,   505,   533,    74, -1690,
    3751,   505,   505,   505,   497,  3939,   708,   775, 12298, -1690,
   -1690, -1690, 16118,  1273, -1690, -1690, -1690,  1560,   738, 12350,
    1022,   885,  1560,   937,   546, -1690, -1690, -1690, -1690,   651,
   -1690, -1690, -1690, -1690,   617, -1690, -1690, -1690, -1690, -1690,
     657,   652,   651, -1690,   651,   661, -1690, -1690, -1690, 16922,
    3751, -1690, -1690,  3751, -1690, 12139,   678, 16974, -1690, -1690,
    4014, 17925, -1690,   799,   799,   727,  2274, -1690, -1690, -1690,
   -1690,   277, 14728,  3224,   651, -1690, -1690, -1690, -1690, -1690,
   -1690,   714, -1690,   722,   742,   762, -1690,   805, 19359, 15348,
    2885,  3018,   499,   787,   790,   813,   828,   830,   846, -1690,
   -1690, 17124, 11168,   777, -1690, 16561, -1690, -1690, -1690, -1690,
     807, -1690, -1690,   848, -1690,  8589,   989, 18711, -1690,   886,
    3751,   652,   903,   915,   918,   920, -1690, -1690, -1690,  2859,
    2353,   944,  1011,   196, -1690, -1690,   651,   651,   154,   165,
     302,   154, -1690,   651,   651, -1690,  2712, -1690, -1690,   908,
     966,   799, 14306, -1690, -1690, 16268, -1690, -1690,  1560, -1690,
    2189,   546,   965,  1037,   165,  3751,   533, -1690, 13704, -1690,
     799,   799,   970,  1037,   165,  3751, -1690, 15094, -1690, -1690,
     799, -1690,   799, -1690,   854,  4211,  3751, -1690,  1201,   988,
   -1690, -1690, -1690, 16720,   652,   174, -1690, -1690, 17975, -1690,
    1011,   119, -1690, 19359, 17925,  3411,  2712, -1690,   327, -1690,
   -1690, -1690, 16974,  3751, -1690,   993, -1690, -1690, -1690, -1690,
    3751,  3551,   498,   -22, -1690,  3751,   722, -1690,   720,   651,
    1002, 17176,   761, 14886, 14464,  1560,  1560, -1690,  1560,   799,
    1560,   799, -1690, -1690,   651, -1690,  1007, -1690, 17326, -1690,
   -1690, -1690, 17378,   807, -1690,  1040,    88,  1943,  1043,   546,
    1046, -1690,  2274,   995,   722,  2274,  2196, -1690,  1028,  1111,
   19431,  1085,  1092, 19359, 19503,  1102, 13596, -1690, -1690, -1690,
   -1690, -1690, -1690, 19575, 19575, 15194,  1101,  4116, -1690, -1690,
   -1690, -1690,   408, -1690,   552, -1690,  1086, -1690, 19359, 19359,
   -1690,  1093,   610,   857,   923,   456,  1059,  1100,  1114,  1128,
    1156,    86, -1690,   435, -1690,  1174, -1690,   910,  4371, 15656,
   -1690, -1690,   700,  1174, -1690, -1690,   467, -1690, -1690,  2885,
    1166,  1182,  1190,  1196,  1198,  1225, -1690, -1690,   366,  1229,
   -1690,   525,  1229, -1690, -1690, 16922, -1690,  1064,  1224, 15810,
   -1690, -1690,  4263,  3664,  1260, 14886,  1268,   723,   771, -1690,
   -1690, -1690, -1690, -1690,  3751,  4281, -1690, -1690, -1690, -1690,
   -1690, -1690,  8221,  4048,  1101,  8589,  1247,  1250, -1690, -1690,
    1264, 18711,   691, -1690, -1690, -1690, 18783,  1288, -1690, -1690,
   -1690, -1690, -1690,  2859,   734,  1291,  1297,  1299,   860,  1301,
    1303,  1313,  2353, -1690, -1690,   651,  1333,   533,  1336, -1690,
   -1690,  1338, -1690, -1690,   652,  1037, -1690, -1690, -1690,   652,
   -1690, -1690,  2712, -1690, 15656, 15656, -1690,   799,  4014, 18703,
   14728, -1690, -1690, -1690, -1690, -1690,   652,  1037,   119, -1690,
   -1690,  1560,  1346,  1037,   165, -1690,   652,  1037, -1690, 19873,
   -1690,   799,   799, -1690, -1690,  1353,   288,  1361,   546,  1363,
   -1690, 18133, -1690,   682, -1690,  1453, 18599, -1690,  4014,  9581,
   14306, -1690, 16720, 19647, -1690, -1690, -1690, -1690, -1690,  3411,
     865,  2712, -1690, 14728,  1011, 12139, -1690,  1369, -1690,  1375,
   -1690, -1690, -1690, -1690, -1690,  2274, -1690, -1690,  1455,  4226,
   17378, 11168, -1690, 17528, -1690,   799,   799, -1690, -1690,   807,
   -1690,   706,  1378,  1512, 19359,   930,  1338,  1362, -1690,   651,
     651, -1690,  1229, -1690, 17176, -1690, -1690, 18414,   799,   799,
   -1690,  4226,   651, -1690, 17782, -1690, -1690, 17326, -1690,   277,
    1379,   171,  1380,  1943,   695, 16974,   713, -1690, -1690, -1690,
   -1690, -1690, -1690,   815, -1690,  1387,  1364, -1690, 15502, -1690,
   17580, 17580, -1690, 15502, -1690, 19359, -1690, 12350, 12350, 15502,
   -1690, -1690,  5906, 17580, 17580,   910,   961,   972,   582,  1072,
   -1690,   816,  1391,  1068,  1392, -1690, 18783, 19359, 18855,  1382,
   19359,  1201, 19359,  1201, -1690,  1388, -1690, -1690, 18927,  2012,
   19359, 18927,  1201, -1690, -1690, 19359, 19359, 19359, 19359, 19359,
   19359, 19359, 19359, 19359, 19359, 19359, 19359, 19359, 19359, 19359,
   19359, 19359, 19359, 19359, 18999,  1370,   805,  4144, 11168, -1690,
   -1690, -1690, -1690, -1690, -1690, -1690, -1690, -1690, -1690, -1690,
    1389, 19359, -1690, -1690,   700,  1251, -1690, -1690,   651,   651,
   -1690, -1690, 15656, -1690,   382,  1229, -1690,   843,  1229, -1690,
   -1690, -1690,  1338, -1690, -1690,  1338, 19719, -1690, -1690, 11168,
    1396,  1399,  3239,  1537,  2481,   393,  1362, -1690,   651,   651,
    1362,   426, -1690,   651,   651, 19359,  3751,  1094,  1116,  1362,
     221, 14254, 14254,  3751, -1690, -1690, 19359,  1264, -1690,  8589,
    1407, -1690,  1083, -1690, -1690, -1690, -1690, -1690,   847, -1690,
   14254,  1201,  4014,  1201,   891,  1406,  1409,  1411,   902,  1422,
    1423,  1424,   458,  1229, -1690, -1690,   490,  1229, -1690, -1690,
   -1690,  4014,   805, -1690,  1229, 19719, -1690,   652, 18133, -1690,
   -1690,   949,  1425,   950,  1426, -1690,  1433, -1690,   652, -1690,
   -1690,   652,  1037,  1433, -1690,   652,  1427,  1428,  1430, -1690,
   -1690, 18414, -1690,  1437, -1690, -1690, -1690,  1201,  3751, 10327,
    1522,  1417, 18501, -1690,  1224, -1690, 14254,   927, -1690, -1690,
    1433, -1690, 16974, 15656,  1420, -1690,  1420, -1690, -1690, -1690,
   -1690, 17326, -1690, 11330, 15964, -1690, 18133,  1443,  1446,  1447,
   -1690,  8448,   651, -1690,   930, -1690, -1690, -1690, -1690,  1338,
   -1690, -1690, -1690,   799, -1690,  3566, -1690, -1690,   546,  2157,
    1451, -1690, 18711, -1690,  1943,  1379, -1690, -1690,  1448,  1449,
    2196, 18927, -1690,  1452,   238,  1454,  1456,  1460,  1463,  1468,
   19359,  1469,  1470,  1473, 11168, 19359, -1690, -1690,  1356, -1690,
   -1690, -1690, 19359, -1690,  1476,  1477,  8974,  1165, -1690, 18927,
    1450, -1690,  1475, -1690, -1690,  3887, -1690, -1690,   962, -1690,
   -1690, -1690, -1690,  3887, -1690, -1690,  1168,    35, -1690, -1690,
    1093,  1093,  1093,   610,   610,   857,   857,   923,   923,   923,
     923,   456,   456,  1059,  1100,  1114,  1128,  1156, 19359,  1173,
   -1690,  1480,  3887, -1690, -1690,  8589, -1690, 18133,  1481,  1483,
    1488,  1251, -1690, -1690, -1690, -1690, -1690, -1690, -1690, -1690,
    1338, -1690, -1690,  1338, 18133, 18133, -1690, -1690,  3239,   945,
    1490,  1494,  1498,  1499,  2557,  2481, -1690, -1690, -1690, -1690,
   -1690, -1690, -1690, -1690, -1690, -1690, -1690, -1690, -1690, -1690,
    1503, -1690,  1362, -1690, -1690, -1690, -1690, -1690, -1690, -1690,
   -1690,  1501,  1508, -1690,   533,  3887,  1189,    55, -1690, -1690,
    1458, -1690, 18711, -1690, 19359, -1690, 19071, 14254, -1690, -1690,
   -1690,  1487,   540,  1229, -1690,   551,  1229, -1690, -1690, -1690,
   -1690,  1338, -1690, -1690, -1690,  1338,  1011,  1513,  1338, -1690,
   -1690, -1690, -1690, -1690, -1690, -1690,  1515, -1690, -1690,  1433,
   -1690,   652, -1690, -1690, -1690, -1690, -1690, 12928,  1516,  1511,
   -1690,    19, -1690,   538,   248, 11006,  1519, 14083,  1520,  1523,
    1570,  3201,  2947, 19143,  1534, -1690, -1690,  1538,  1543, -1690,
   -1690,   652, 19359, 19359,  1673,  1540,   356, -1690,  1621,  1539,
    1524, -1690, -1690, -1690, 10155, -1690, -1690, -1690, -1690, -1690,
    2508, -1690, -1690, -1690,  1608, -1690, -1690, -1690,  1201, -1690,
   -1690, 12775, 16268,  1547, -1690,  3751, -1690,  1530,  1553,  1554,
   -1690,  1211, -1690, -1690, -1690, -1690,  4014, -1690, -1690,  1535,
    1541,   974, 16974,   722,   722, -1690, -1690,  1101,  1224, 15810,
   -1690,  1174, -1690, 11492, -1690,   574,  1229, -1690,   799,  8852,
   -1690, -1690,  1943,   651,   651,   277,   171, -1690, -1690,  1379,
    1563,  1564, -1690, -1690,   979,   373, 11168,  1201, -1690,   373,
   16772,   373, -1690, 19359, 19359, 19359, -1690, -1690, -1690, -1690,
   19359, 19359,  1557,  8589, -1690, -1690,  1562,   389, -1690, -1690,
   -1690,  3578, -1690, -1690,  1231, -1690,   314, -1690, 18927,  1235,
   -1690, 18783, -1690, -1690, 19359,  1544,  1239,  1242,  1264, -1690,
     576,  1229, -1690, -1690, 18133, 18133, -1690, -1690,  1569,   593,
    1229, -1690,   607,  1916,   651,   651, -1690, -1690, 18133, 18133,
   -1690,  1568, -1690, 14728, 14728,  1574,  1571,  1572,  1577, -1690,
    1575, 19359, 19359,  1255,  1580, -1690, -1690, -1690, -1690, -1690,
   -1690,  1585, 19359, -1690, -1690, -1690,  1338, -1690, -1690, -1690,
    1338, 18133, 18133,   533,   651,  1259,  1586,  1583, -1690, -1690,
    1590, 13081, 13234, 13387, 16974, 17580, 17580,  1591, -1690,  1573,
    1576,  2123,  7584, -1690,   335,  3751, -1690, -1690,  3751, -1690,
    9680,   193,   310, -1690, -1690, -1690, -1690, 19359,  1593,  1667,
   10843, 10499, -1690,  1578, -1690,  1584, 19359,  1588,  8589,  1589,
   19359, 18783, 19359,  1103, -1690,  1598,    58, -1690,   211,  1597,
   -1690, -1690,  1600, -1690,  1601, -1690,  1602,  1610, 14083,   624,
   13862,   651,   351, -1690, -1690, -1690,  1609, -1690,  1616, -1690,
    1630, -1690,  1594, -1690,  1624, -1690, -1690, -1690, -1690, 11654,
    1626,  1632,  1635, -1690,  1633, -1690, -1690, -1690,  1338, 19359,
   19359,  1224,  1636, -1690,  1379, -1690,  1637,   621, -1690,  1644,
   -1690, -1690, 16974, -1690,  1645,  1639,   982, -1690,  1641, -1690,
   -1690, -1690, -1690, -1690,  8589,  1264, 18783, -1690,  1680,  3887,
   -1690,  1680,  1680, -1690,  3887,  3693,  3798, -1690, -1690,  1263,
   -1690, -1690, -1690,  1653,  1651, -1690, -1690, -1690,  1338, -1690,
   -1690,  1652,  1655,   651, -1690, -1690, -1690,  1338, -1690, -1690,
   -1690,  1656, -1690, -1690, -1690, -1690, -1690, -1690, -1690, -1690,
   -1690, -1690, -1690, -1690, -1690,  1660, -1690, -1690, -1690, -1690,
    1661,  1658,   651, -1690, 18133, 18133, -1690, -1690, -1690, -1690,
   19359, -1690, -1690,  1662, -1690,  1591,  1591,  1591,   776,  1642,
     496, -1690,  3783,   512, 15656, -1690, -1690, -1690,  3475, 19359,
    3340,   518, -1690, -1690,    46,  1664,  1664,  3751, -1690, -1690,
   18282, -1690, 19359,  1663,  1670, -1690, -1690, -1690, -1690,   987,
    1675, 14083,  1539,  1671, 19359,   432,  1679,   497, 13546, 16974,
   14083, 19359, 19359,   893,   564, -1690, 19359, -1690, -1690,   521,
   -1690,  1264, -1690,   990,  1000,  1016, -1690, -1690, -1690, -1690,
     652,  1103,  1686, -1690, -1690, 19359, -1690,  1688,   805, 11006,
   -1690, -1690, -1690, -1690, 19359,  1733, -1690,  9385, -1690,   651,
   14728, -1690, -1690, 16974, -1690, -1690, -1690, -1690, -1690,  1687,
   -1690, 18133, -1690, -1690,  1689, -1690,  1696,  1705,  1697,  1943,
   -1690, -1690, -1690, -1690, 19359, -1690, 16772, 19359,  1264,  1709,
    1277, -1690,  1280, -1690,  3887, -1690,  3887, -1690, -1690, -1690,
   -1690, 18133,  1710,  1711, -1690, -1690, 18133, 18133,  1715,  1717,
    1286, 14412, 14570, -1690,  1716, -1690, -1690, -1690, -1690,  1718,
    1721,  1308, -1690, -1690, -1690, -1690,   776,  1493,   557, -1690,
   -1690, -1690, -1690,   651,   651, -1690, -1690, -1690,   573, -1690,
    1047,  3475,   644, -1690,  3340,   651, -1690, -1690, -1690, -1690,
   -1690, -1690, -1690, -1690,   588, 14083,    81, 19215, -1690, 14083,
    1539, 15044, -1690,  1539,  1699, -1690, -1690, -1690, -1690,  6513,
   19359, 14083, 10671,  1701, -1690,  1732,   457, 14083, -1690, -1690,
    1734, -1690, -1690,  1708,   805,   675,  1735,  1736,  1200,  1796,
   -1690, -1690, -1690, -1690,  3751,  4014, -1690, -1690,  1731,  1737,
   -1690, -1690, -1690,  1943,  1379,  1740, -1690, -1690, -1690,  1741,
   -1690, -1690, -1690,  1312,  1314, -1690, -1690, -1690, -1690, -1690,
   -1690, -1690, -1690, -1690, -1690,  1742, -1690, -1690,  1744,  1745,
   -1690, -1690, -1690,  1746,  1748,  1750,  1493, -1690,   651, -1690,
   -1690, -1690, -1690, -1690,  1739,  3783, -1690, -1690,  4703,    97,
   11819, -1690, 13965, -1690,  1729,  1069,  1832, 19359,  1754, 19359,
     898,  1738,   169,  1834, -1690, 19359,  1743, 11980, -1690, -1690,
   -1690, 17730, -1690,  1752,  1751,   243, 14083, -1690, 19359, 18927,
     192, -1690, -1690, -1690,  1759, -1690, -1690,  1379,  1763, -1690,
   -1690, -1690, -1690,  1764,  1768,  1769, 14728,  1773, -1690, -1690,
     627,  1229, -1690, -1690,   776, -1690, -1690,   161, -1690,    42,
   -1690, -1690, -1690,  1783, 12457, -1690, -1690, -1690,    68, 14083,
   -1690,  1539,  1785,  1787, 19359, 19359, 19359, 14083, -1690, -1690,
    1790, 12457, 17730, -1690,  3310, 17528,  1201,  1777, -1690,  1840,
    1794,   676,  1789, -1690,  1872, -1690,  1076, 14083,  1798, 14083,
   14083, -1690,  1803, -1690, -1690, -1690, -1690, -1690, -1690, -1690,
   -1690,  1338, -1690, 19359, -1690, 19359, -1690, -1690,  1395, 12616,
   -1690, 14083, -1690, -1690,  1788,  1791,   251, -1690,  1539, -1690,
   -1690,  1395, -1690,  1781,  2801,  2584, -1690, -1690, -1690,   243,
    1804, 19359,  1786,   243,   243, 14083, -1690, -1690, 19359,  1850,
    1855, -1690, 18133, -1690, -1690, 13965, -1690,  1395, -1690, -1690,
   19359, 19287, 19359, -1690,  1781, 19359,  1812,  2584,  1809,   805,
    1815, -1690,   683, -1690, -1690,  1091,  1796,   395, -1690, -1690,
    9834,  1821, 13965,  1539, -1690,  1539,  1539,  1822,  1823, -1690,
     652,   805,  1824, -1690,  1802,   805, -1690, -1690, 14083,  1899,
    1828, -1690, -1690, -1690,  9983, -1690,   652, -1690, -1690,  1319,
   19359, -1690,  1136, -1690, 14083, -1690, -1690,   805,  1201,  1829,
    1808, -1690, -1690, -1690,  1146, -1690, -1690,  1810,  1201, -1690,
   -1690
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,   418,     0,     2,   418,   435,   436,   437,   438,   439,
     440,   441,   442,   424,   426,   425,   427,     0,     0,     0,
     443,   445,   466,   446,   467,   449,   450,   464,   465,   444,
     462,   463,   447,   448,   451,   452,   453,   454,   455,   456,
     457,   458,   459,   460,   461,   468,   469,   755,   471,   544,
     545,   548,   550,   546,   552,     0,     0,     0,   418,     0,
       0,    16,   515,   521,     9,    10,    11,    12,    13,    14,
      15,   719,    97,     0,    19,     0,     2,    95,    96,    17,
      18,   771,   418,   720,   367,     0,   370,   645,   372,   381,
       0,   371,   401,   402,     0,     0,     0,     0,   498,   420,
     422,   428,   418,   430,   433,   483,   470,   406,   476,   481,
     407,   493,   408,   508,   512,   518,   497,   524,   536,   755,
     541,   542,   525,   591,   373,   374,     3,   721,   734,   423,
       0,     0,   755,   793,   755,     2,   810,   811,   812,   418,
       0,   969,   970,     0,     1,   418,     0,   418,   390,   391,
       0,   498,   412,   413,   414,   724,     0,   547,   549,   551,
     553,     0,   418,     0,   756,   757,   543,   472,   638,   639,
     637,   698,   693,   683,     0,     0,   722,     0,     0,   418,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   516,
     519,   418,   418,     0,   971,   498,   800,   818,   975,   968,
     966,   973,   366,     0,   159,   651,   158,     0,   375,     0,
       0,     0,     0,     0,     0,     0,   365,   870,   871,     0,
       0,   400,   753,   755,   749,   774,   755,   755,   751,     2,
     755,   750,   831,   755,   755,   828,     0,   491,   492,     0,
       0,   418,   418,   435,     2,   418,   382,   421,   431,   484,
       0,   513,     0,   737,     2,     0,   645,   383,   498,   477,
     494,   509,     0,   737,     2,     0,   434,   478,   485,   486,
     495,   500,   510,   514,     0,   528,     0,   713,     2,     2,
     735,   792,   794,   418,     0,     2,     2,   979,   498,   982,
     753,   753,     3,     0,   498,     0,     0,   393,   755,   751,
     750,     2,   418,     0,   717,     0,   679,   681,   680,   682,
       0,     0,   675,     0,   665,     0,   674,   685,     0,   755,
       2,   418,   990,   419,   418,   430,   409,   476,   410,   501,
     411,   508,   505,   526,   755,   527,     0,   626,   418,   627,
     944,   945,   418,   628,   630,   515,   521,     0,   592,   593,
       0,   758,     0,   696,   684,     0,   762,    21,     0,    20,
       0,     0,     0,     0,     0,     0,    23,    25,     4,     8,
       5,     6,     7,     0,     0,   418,     2,     0,    98,    99,
     100,   101,    82,    24,    83,    38,    81,   102,     0,     0,
     117,   119,   123,   126,   129,   134,   137,   139,   141,   143,
     145,   147,   150,     0,    26,     0,   522,     2,   102,   418,
     151,   690,   641,   512,   643,   689,     0,   640,   644,     0,
       0,     0,     0,     0,     0,     0,   772,   798,   755,   808,
     816,   820,   826,     2,   977,   418,   980,     2,    95,   418,
       3,   625,     0,   990,     0,   419,   476,   501,   508,     3,
       3,   607,   611,   621,   627,   628,     2,   801,   819,   967,
       2,     2,    23,     0,     2,   651,    24,     0,   649,   652,
     988,     0,     0,   658,   647,   646,     0,     0,   739,     2,
       2,     2,     2,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   777,   834,   755,     0,   645,     2,   773,
     781,   897,   775,   776,     0,   737,     2,   830,   838,     0,
     832,   833,     0,   396,   418,   418,   482,   419,     0,   498,
     418,   972,   976,   974,   499,   717,     0,   737,   753,   376,
     384,   432,     0,   737,     2,   717,     0,   737,   694,   479,
     480,   496,   511,   517,   520,   515,   521,   539,   540,     0,
     695,   418,   635,     0,   196,   359,   418,     3,     0,   498,
     418,   736,   418,     0,   378,     2,   379,   714,   398,     0,
       0,     0,     2,   418,   753,   418,   717,     0,     2,     0,
     678,   677,   676,   671,   429,     0,   669,   686,   474,     0,
     418,   418,   946,   419,   415,   416,   417,   950,   941,   942,
     948,     2,     2,    96,     0,   906,   920,   990,   902,   755,
     755,   911,   918,   633,   418,   506,   629,   419,   502,   503,
     507,     0,   755,   956,   419,   961,   953,   418,   958,     0,
     988,   598,     0,     0,     0,   418,     0,   770,   769,   765,
     767,   768,   766,     0,   760,   763,     0,    22,   418,    89,
     418,   418,    84,   418,    91,     0,    32,     0,    33,   418,
      87,    88,   418,   418,   418,     2,    98,    99,     0,     0,
     177,     0,     0,   542,     0,   966,     0,     0,     0,     0,
       0,     0,     0,     0,    50,     0,    56,    57,    61,     0,
       0,    61,     0,    85,    86,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   418,   160,
     161,   162,   163,   164,   165,   166,   167,   168,   169,   170,
     158,     0,   156,   157,     2,   882,   642,   879,   755,   755,
     887,   523,   418,   799,   755,   809,   817,   821,   827,     2,
     802,   804,   806,     2,   822,   824,     0,   978,   981,   418,
       0,     0,     2,    96,   906,   755,   990,   852,   755,   755,
     990,   755,   867,   755,   755,     3,   629,     0,     0,   990,
     990,   418,   418,     0,     2,   660,     0,   988,   657,   989,
       0,   653,     0,     2,   656,   659,   174,   173,     0,     2,
     418,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   755,   786,   790,   829,   755,   843,   848,   778,
     835,     0,     0,   404,   894,     0,   740,     0,   418,   741,
     397,     0,     0,     0,     0,   395,     2,   742,     0,   380,
     717,     0,   737,     2,   743,     0,     0,     0,     0,   554,
     614,   419,     3,     3,   618,   617,   813,     0,     0,   418,
     360,     0,   498,     3,    95,     3,   418,     0,     3,   718,
       2,   673,   418,   418,   667,   666,   667,   475,   473,   592,
     952,   418,   957,   419,   418,   943,   418,     0,     0,     0,
     921,     0,   755,   991,   907,   908,   634,   904,   905,   919,
     947,   951,   949,   504,   539,     0,   955,   960,   595,   989,
       0,   158,     0,   594,     0,   988,   699,   697,     0,     0,
     762,    61,   723,     0,     2,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   418,     0,   116,   115,     0,   112,
     111,    27,     0,    28,     0,     0,     0,     0,     3,    61,
       0,    46,     0,    47,    54,     0,    53,    65,     0,    62,
      63,    66,    49,     0,    48,    52,     0,     0,    45,   118,
     120,   121,   122,   124,   125,   127,   128,   132,   133,   130,
     131,   135,   136,   138,   140,   142,   144,   146,     0,     0,
     369,     0,     0,    29,     3,   651,   152,   418,     0,     0,
       0,   883,   884,   880,   881,   692,   691,     2,   803,   805,
     807,     2,   823,   825,   418,   418,   899,   898,     2,     0,
       0,     0,     0,     0,   755,   907,   855,   872,     2,   850,
     858,   631,   853,   854,   632,     2,   865,   875,   868,   869,
       0,     3,   990,   388,     2,   983,     2,   622,   623,   601,
       3,     3,     3,     3,   645,     0,   150,     0,     3,     3,
       0,   654,     0,   648,     0,   738,     0,   418,     3,   392,
     394,     0,   755,   787,   791,   755,   844,   849,     2,   779,
     782,   784,     2,   836,   839,   841,   753,     0,   895,     3,
     745,     3,   488,   487,   490,   489,     2,   718,   746,     2,
     744,     0,   718,   747,   554,   554,   554,   418,     0,     0,
     636,     0,   363,     0,     0,   418,     0,     2,     0,     0,
       0,     0,     0,   179,     0,   293,   294,     0,     0,   332,
     331,     0,   154,   154,   338,   515,   521,   193,     0,   180,
       0,   204,   181,   182,   418,   198,   183,   184,   185,   186,
       0,   187,   188,   299,     0,   189,   190,   191,     0,   192,
     200,   498,   418,     0,   202,     0,   357,     0,     0,     0,
       3,     0,   725,   718,   706,   707,     0,     3,   702,     3,
       3,     0,   418,   683,   683,   954,   959,     2,    95,   418,
       3,   513,     3,   419,     3,   755,   914,   917,   418,     3,
     903,   909,     0,   755,   755,     0,   598,   583,   599,   988,
       0,     2,   759,   761,     0,    90,   418,     0,    94,    92,
     418,     0,   106,     0,     0,     0,   110,   114,   113,   178,
       0,     0,     0,   651,   103,   171,     0,     0,    41,    42,
      79,     0,    79,    79,     0,    67,    69,    44,     0,     0,
      40,     0,    43,   149,     0,     0,     0,     0,   988,     3,
     755,   890,   893,   885,   418,   418,     3,     3,     0,   755,
     861,   864,   755,     0,   755,   755,   856,   873,   418,   418,
     984,     0,   624,   418,   418,     0,     0,     0,     0,   377,
       3,     0,     0,     0,     0,   650,   655,     3,   176,   175,
       3,     0,     0,     2,   780,   783,   785,     2,   837,   840,
     842,   418,   418,   645,   755,     0,     0,     0,   718,   748,
       0,   418,   418,   418,   418,   418,   418,   537,   565,     3,
       3,   566,   498,   555,     0,     0,   795,     2,     0,   361,
      61,     0,     0,   284,   285,   201,   203,     0,     0,     0,
     418,   418,   280,     0,   278,     0,     0,     0,   651,     0,
       0,     0,     0,     0,   155,     0,     0,   339,     0,     0,
       3,   208,     0,   199,     0,   275,     0,     0,     2,     0,
     498,   755,     0,   358,   901,   900,     0,     2,     0,   709,
       2,   704,     0,   705,     0,   687,   668,   672,   670,   418,
       0,     0,     0,     3,     0,     2,   910,   912,   913,     0,
       0,    95,     0,     3,   988,   588,     0,   598,   596,     0,
     586,   700,   418,   764,     0,     0,     0,    34,     0,   107,
     109,   108,   105,   104,   651,   988,     0,    60,    76,     0,
      70,    77,    78,    55,     0,     0,     0,    64,    51,     0,
     148,   368,    30,     0,     0,     2,   886,   888,   889,     3,
       3,     0,     0,   755,     2,   857,   859,   860,     2,   874,
     876,     0,   851,   866,     3,     3,   985,     3,   609,   608,
     612,   987,     2,     2,   986,     0,     3,   752,   661,   662,
       0,     0,   755,   399,   418,   418,     3,     3,   405,   754,
       0,   845,   729,     0,   731,   537,   537,   537,   572,   542,
       0,   578,   566,     0,   418,   529,   564,   560,     0,     0,
       0,     0,   567,   569,   755,   580,   580,     0,   561,   576,
     418,   364,     0,     0,    62,   288,   289,   286,   287,     0,
       0,     2,   219,     0,     0,   221,   372,   220,   498,   418,
       2,     0,   179,   254,     0,   249,   179,   281,   279,     0,
     273,   988,   282,     0,     0,     0,   320,   321,   322,   323,
       0,   313,     0,   314,   290,     0,   291,     0,     0,   418,
     210,   197,   277,   276,     0,   311,   330,     0,   362,   755,
     418,   727,   688,   418,     2,     2,   962,   963,   964,     0,
     915,   418,     3,     3,     0,   923,     0,     0,     0,     0,
     597,   585,     3,    93,     0,    31,   418,     0,   988,     0,
       0,    80,     0,    68,     0,    74,     0,    72,    39,   153,
     891,   418,     0,     0,   796,   814,   418,   418,     0,     0,
       0,   418,   418,   664,     0,   385,   387,     3,     3,     0,
       0,     0,   733,   533,   535,   531,     0,   930,     0,   573,
     935,   575,   927,   755,   755,   559,   579,   563,     0,   562,
       0,     0,     0,   582,     0,   755,   556,   570,   581,   571,
     577,   616,   620,   619,     0,     2,     0,     0,   240,     2,
     222,   498,   246,   255,     0,   270,   271,   272,   269,   258,
       0,     2,   418,     0,   274,     0,     0,     2,   297,   324,
       0,   315,     2,     0,     0,     0,     0,   302,     0,   298,
     195,   194,   386,   703,     0,     0,   965,     3,     0,     0,
     922,   924,   587,     0,   988,     2,    37,    35,    36,     0,
      58,   172,    71,     0,     0,     3,   797,   815,     3,     3,
     862,   877,   389,     2,   606,     3,   605,   663,     0,     0,
     788,   846,   896,     0,     0,     0,   931,   932,   755,   558,
     928,   929,   557,   538,     0,     0,   209,   296,     0,     0,
       0,   233,     2,   211,     0,     0,   241,   179,   263,     0,
     259,     0,   256,   247,   250,   179,     0,     0,   214,   295,
       2,   418,   292,     0,     0,   340,     2,   300,     0,    61,
       0,   312,   708,   710,     0,   925,   926,   988,     0,   701,
      59,    75,    73,     0,     0,     0,   418,     0,   789,   847,
     755,   938,   940,   933,     0,   568,   228,   223,   226,     0,
     225,   232,   231,     0,   418,   235,   234,   243,     0,     2,
     251,   260,   271,   269,     0,   179,     0,     2,   253,   283,
       0,   418,   418,     3,   325,   419,   329,     0,   333,     0,
       0,     0,   341,   342,   217,   303,     0,     2,     0,     2,
       2,   916,     0,   590,   892,   863,   878,   610,     2,   934,
     936,   937,   574,     0,   230,     0,   229,   213,   236,   418,
     353,     2,   244,   242,   265,   264,   261,   252,   257,   248,
     216,   236,     3,   318,     0,   930,   326,   327,   328,   340,
       0,     0,     0,   340,     0,     2,   301,   308,     0,   305,
     307,   589,   418,   224,   227,     2,     3,   237,   354,   245,
       0,     0,     0,     3,   318,     0,     0,   931,     0,     0,
       0,   334,     0,   343,   218,     0,   298,     0,     3,   205,
       0,     0,     2,   267,   268,   266,   262,     0,     0,   319,
       0,   346,     0,   344,     0,   346,   304,   306,     2,     0,
       0,   207,   206,   212,     0,   215,     0,   316,   347,     0,
       0,   335,     0,   309,     2,   939,   317,     0,     0,     0,
       0,   310,   348,   349,     0,   345,   336,     0,     0,   337,
     350
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1690,  5975,  5926, -1690,    -1,   493,  1322,  -158, -1690,  1611,
   -1690,   369, -1690,  -678,   647,   741,  -917,  -792, -1690,   234,
    6770,  1917, -1690,   115, -1690,  1323,    57,   780,   783,   718,
     806,  1283,  1284,  1285,  1289,  1282, -1690,  -152,  -138,  8466,
     867, -1690,  -383, -1690, -1690,  -647,  2491, -1038,  1402, -1690,
    -119, -1690,   862,    36, -1690, -1690, -1690,   425,   113, -1690,
   -1477, -1546,   304,   100, -1690, -1690, -1690,   312,   226, -1690,
   -1690, -1690, -1690,    61, -1689,   220, -1690, -1690,    75, -1690,
   -1690, -1690,    77,   462,   464,   175, -1690, -1690, -1690, -1690,
    -605, -1690,   112,    63, -1690,   178, -1690,   -63, -1690, -1690,
   -1690,   875,  -599,  -935, -1317, -1690,    43, -1216,   311,  6090,
    -890,  -883, -1690,  -273, -1690,    44,  -104,    37,  -206,  -226,
    3738,  3109,  -609, -1690,    33,   142,   767,  2212, -1690,  1994,
   -1690,    65,  3864,  -260, -1690, -1690,   106, -1690, -1690,  1209,
     110,  4604,  2602,   -52,  1795,  -287, -1690, -1690, -1690, -1690,
   -1690,  -741,  4962,  4993, -1690,  -357,  -173, -1690,   534,   282,
   -1690,   224,   733, -1690,   535,   -38, -1690, -1690, -1690,  5334,
    -601, -1132,  -696,  -594,  -293,   925, -1690, -1263,  -161,   646,
    -384,   905,  8031,  -112,  -464,  -247,  -180,  -428,  1271, -1690,
    1603,   283,  1193,  1485, -1690, -1690, -1690, -1690,   332,  -162,
      45,  -845, -1690,     8, -1690, -1690,   656,   488, -1690, -1690,
   -1690,  2083,  -780,  -417,  -738,   -19, -1690, -1690, -1690, -1690,
   -1690, -1690,   197,  -811,  -108, -1661,  -170,  8047,   -65,  6519,
   -1690,  1164, -1690,   307,  -196,  -213,  -143,  -126,     1,   -68,
     -59,   -56,   397,    -8,   102,   122,  -110,   -73,   -88,   -82,
     -69,  -702,  -683,  -675,  -652,  -697,  -107,  -650, -1690, -1690,
    -700,  1351,  1354,  1357,  1342,  7334,  -517,  -570,  -557,  -548,
    -701, -1690, -1517, -1587, -1576, -1572,  -598,   126,  -265, -1690,
   -1690,   -33,     3,   -61, -1690,  7710,   760,  -562,  -480
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,  1138,   213,   382,   383,    80,    81,   384,   359,   385,
    1426,  1427,   386,   958,   959,   960,  1244,  1245,  1246,  1438,
     408,   388,   389,   390,   668,   669,   391,   392,   393,   394,
     395,   396,   397,   398,   399,   400,   401,   410,  1057,   670,
    1365,   731,   207,   733,   404,   798,  1139,  1140,  1141,  1142,
    1143,  1144,  1145,  1960,  1146,  1147,  1370,  1543,  1838,  1839,
    1781,  1782,  1783,  1936,  1937,  1148,  1554,  1555,  1700,  1149,
    1150,  1151,  1152,  1153,  1154,  1378,  1718,  1880,  1811,  1155,
    1156,  1571,  1946,  1572,  1573,  1863,  1157,  1158,  1159,  1368,
    1871,  1872,  1873,  1989,  2004,  1898,  1899,   284,   285,   859,
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
      79,   336,   131,    79,   140,   183,   485,   140,   181,   529,
     557,   354,   280,   967,   184,   516,   901,   185,   673,   358,
    1478,  1479,   231,   190,   493,   732,   402,   475,  1181,   947,
    1810,   887,   915,  1545,   102,  1002,   615,   787,    96,   176,
     403,   150,   297,  1894,   888,    95,  1249,   148,  1164,   198,
     832,   834,   497,   889,    79,    79,   350,    79,   322,   131,
     940,   140,  1026,  1102,  1030,   186,   107,  1027,   910,  1763,
    1037,  -711,    79,   625,  1418,  1256,   486,   628,   289,  1020,
    1764,    79,   485,   196,  1765,  1359,  1053,  1021,   895,    79,
    1173,   102,   478,   487,    79,    96,   228,    79,  1841,   253,
     493,    79,    95,   263,  1068,   140,   658,   111,   836,   488,
    1022,   112,  1023,   420,   198,   592,    57,  1840,   843,   194,
     564,   566,   421,   107,    57,   422,   292,   896,   513,   584,
     436,   489,   623,   585,  1546,  1546,   626,   490,  1290,    79,
    1767,  1901,    79,   103,    79,   256,   131,   494,   140,    79,
     491,   183,   486,  1575,   483,    79,  1778,  1779,   615,   870,
     184,   209,    79,   185,   111,   561,   505,   855,   112,   487,
    1170,   917,  1328,   423,  1338,   187,   287,   144,   102,    79,
      79,   196,    96,  1252,   194,   488,  1291,  1051,  1051,    95,
    1248,   527,   887,  1339,    79,   188,   457,  1895,  1896,   498,
     103,   537,   658,   162,   466,   888,  1051,   489,   260,    79,
     107,   186,   522,   490,   889,   496,   713,  1329,    79,    79,
    1292,   196,   544,   494,  1330,  1060,   491,   183,   434,   210,
     569,  -737,  1576,   592,  1840,    79,   184,   179,  1780,   185,
     248,   209,  1902,  1214,    79,  -871,   196,   895,   912,  1833,
     823,   111,  -355,  1842,    79,   112,   672,    79,   714,   533,
    1160,  -356,  1545,   674,    79,  1877,    57,  1810,   565,   522,
     805,  1237,  1051,   538,    79,    79,  -712,    79,  1006,   518,
     868,  1377,   521,   424,   550,  1610,  1031,   103,   339,   863,
    1034,   791,  1893,  1577,    79,    79,   819,   196,  1878,  1047,
    1048,  1263,    79,   425,  1346,  1846,   615,  1341,  1342,    79,
      79,  -737,    89,  1209,    79,   149,   560,   204,  1030,  1318,
      96,   187,  -355,  1276,   942,   882,  1292,  1763,  1277,  1228,
     615,  -356,   965,  1164,  1856,  1020,   771,   615,  1764,   521,
     806,   188,  1765,  1021,   911,   278,    79,  1917,   107,   498,
     279,    79,  1935,  1210,    79,   642,   805,   807,   357,  1317,
    1578,   587,   907,  1546,  1320,  1935,  1022,  1535,  1268,    89,
     565,  1300,    57,   808,   758,   819,   198,  1201,  1478,  1479,
     182,    62,    63,   155,   880,   826,  1328,  1328,  1328,   111,
     829,  1962,   191,   112,   604,   809,  1869,    57,  1767,   587,
     531,   810,   223,   156,   830,   279,   942,   837,   900,   246,
     835,   190,   161,   257,   811,   420,  1942,   844,    79,   820,
     457,   906,  1343,  1099,   421,   504,   806,   422,   509,    75,
    1833,  1329,  1329,  1329,  1445,  1388,    57,   179,  1330,  1330,
    1330,    79,    79,   807,   855,  -534,   194,   598,   887,  1440,
     526,   506,    57,    79,    79,   498,    89,   298,  1846,   808,
     536,   888,    79,    57,   466,   248,  1446,   279,  1978,   322,
     889,   842,   757,  1051,    19,   423,   572,   175,   652,   197,
     498,   809,    79,  1128,  1537,  1846,  1546,   810,  1052,  1052,
    1527,    79,   229,   457,  1407,   254,    57,   146,   820,   264,
     811,   420,   855,   693,   694,   179,  1338,  1052,  1201,  1528,
     421,    79,  -639,   422,  1694,   749,  1345,    79,  1703,   498,
     189,    63,  1622,  1232,   693,  1588,   484,   223,    57,   672,
    1233,  1007,  1778,  1779,   672,   498,   923,  1437,   925,   926,
     672,   927,  1028,   298,  1248,  1160,   602,   929,   170,   170,
     931,   932,   933,   177,   693,    79,   869,    79,   991,   672,
      57,   680,  1282,  1887,   615,   598,   681,  1258,    79,   530,
      79,  1470,   457,  1052,    79,  1035,   131,   197,   140,   602,
    1503,   703,   704,   170,    79,   424,   715,   248,    79,   178,
     716,   901,   458,   150,   157,   560,   615,   158,   159,    96,
     160,  1414,   570,   298,  1449,   425,   202,  1078,   102,  1061,
      57,   498,    96,   855,  1797,   179,  1186,   197,   741,    95,
      79,    57,   742,   402,   884,   705,   706,   107,  1546,   580,
     855,   855,    79,   170,  1056,   634,   170,  1041,   636,  1082,
     107,   868,   197,   498,    57,  1546,    57,  1419,   278,   170,
     426,  1666,  1623,  1625,  1627,   534,   348,   771,   581,   582,
      -3,  1791,  1533,    57,  1087,   544,   260,  1527,   111,  1670,
    1667,   216,   112,  1675,   753,  1185,   942,    57,   498,   204,
      79,   111,    79,  1546,    79,   112,  1669,  1340,    79,  1303,
     205,    79,  1676,   498,  1070,  1704,  1453,    57,   274,   339,
    1307,   248,   827,   170,   498,   682,   206,  1743,  1090,  1744,
     683,   236,  1768,  1086,  1564,  1701,    79,   103,  1396,  1098,
    1702,    57,  1100,  1405,   838,  1455,  1103,   602,  1675,   498,
     841,  1769,  1208,   935,   845,    13,    14,    15,    16,    17,
     598,  -412,  1464,   942,   936,   937,   498,  1772,   170,  1850,
     765,  1544,  1556,   970,   971,   972,  1468,  1858,   170,   531,
     602,    79,  1776,    79,  1653,  1654,  1655,   204,   547,   170,
     696,   552,   276,   274,  1052,    79,  1888,   697,   698,   857,
     498,   279,    79,    13,    14,    15,    16,    17,   466,  1620,
     804,    79,   884,    57,   911,   426,   170,   498,  1204,   223,
      79,    79,    79,   170,   170,  1435,   278,  1289,   170,   279,
     969,    13,    14,    15,    16,    17,   458,  1907,  -355,   298,
      79,   237,   238,   912,   239,   298,  1804,  1922,   240,    72,
     322,  1805,  1923,   856,  1974,    72,  1253,   857,   453,  1975,
     170,    57,   201,   792,   793,   170,   916,   794,   170,   734,
     585,   293,  1607,   498,  -416,   601,    79,    79,   466,   602,
      77,    78,   140,   352,   918,   298,    77,    78,   585,    57,
     266,   588,   274,  1619,   267,   140,   867,   270,   298,   272,
     855,   855,  1296,   278,  -726,   426,    89,   498,  -413,   458,
      72,   355,  1162,   311,   855,   855,    96,    13,    14,    15,
      16,    17,  -417,    95,    79,    72,   615,   201,    79,  1176,
     601,   356,  1056,    79,   602,  1174,  1313,  1275,   771,   642,
     357,    77,   603,   868,   107,  1656,   456,   855,   855,   498,
    1561,  1321,  1322,  1323,   604,   170,    77,    78,   427,   418,
    -414,   428,    13,    14,    15,    16,    17,   170,   170,    13,
      14,    15,    16,    17,    79,    57,   460,   157,   543,    63,
     158,   159,    79,   160,   429,   111,   919,   941,  1509,   112,
     920,   942,  1409,  -116,  -116,  -116,  -116,  -116,  -116,   430,
    1544,   431,  1319,   900,  -115,  -115,  -115,  -115,  -115,  -115,
    1477,    79,  1011,   248,   466,  1344,   498,   432,  1065,  1705,
      57,   103,  1066,   461,   531,   523,  1618,    57,  1734,   506,
     474,   815,  1363,   498,   572,   266,   426,    79,   498,   699,
     700,   354,   354,    79,    79,   248,   243,     6,     7,     8,
       9,    10,    11,    12,   912,   476,  1695,  1696,  1697,  1101,
     278,  1695,  1852,  1697,   498,  1222,   701,   702,   942,   146,
    1226,   506,   479,   942,    79,   498,  1739,   514,  1698,    72,
     339,  1234,   523,  1853,   191,   676,  1498,  1699,  1389,  1019,
     480,   765,  -180,   481,  1335,   482,   572,  1547,   170,   601,
     498,   600,   879,   602,    13,    14,    15,    16,    17,   939,
      77,   603,   266,   267,  1028,   619,   426,   272,   602,   495,
    1092,  1094,  1450,  1556,   942,   942,   868,   496,  1428,   298,
     855,   855,   322,  1247,   904,   515,   466,  1248,   140,    79,
      79,    79,   525,  1480,   209,  1395,   170,   535,   298,   742,
    1423,  1876,  1817,  1615,  1248,  1525,   554,  1616,  1686,   402,
     402,  1706,   942,   466,  1331,   942,  1683,   140,  1162,    79,
     576,  1707,    96,  1486,  1487,  1066,   590,    79,   622,    95,
      79,    79,   253,   263,    79,   140,   453,  1708,   635,   201,
      89,   942,  1818,  -403,   951,    79,   953,  1162,   956,   646,
     107,    96,   964,  1175,    61,   968,   707,   708,    95,    64,
      65,    66,    67,    68,    69,    70,  -403,  -870,  1773,   600,
    -584,    79,   742,   633,   684,   256,   685,   686,   687,   107,
     993,  1566,  1567,  1568,  1569,  1570,    79,   855,   590,   676,
    1848,   111,  1536,  1538,   942,   112,   647,  1926,   944,   945,
     652,  1248,   466,    74,   650,   688,   784,   453,   689,   690,
      79,   651,  1976,   691,   692,  1900,   942,   855,  1043,  1044,
     111,   655,   855,   855,   112,  1882,   676,   103,  1526,   695,
    1586,   709,  1900,    13,    14,    15,    16,    17,   260,   170,
    1045,  1046,    79,   710,   418,   418,   170,   243,     6,     7,
       8,     9,    10,    11,    12,   712,   103,  2000,  1335,  1335,
    1335,  1997,  1511,  1335,  1069,   322,  1071,  2007,   266,   711,
    1938,  2008,    61,   248,   693,   485,  1547,    64,    65,    66,
      67,    68,    69,    70,  1948,   598,  1525,   743,  1952,  1235,
    1066,    57,  1250,  1251,   493,  1019,   717,   140,   942,  1254,
      79,  1274,   765,   744,    79,   531,   265,    79,  1429,  1430,
    1431,   745,   339,  -151,  -151,  1432,  1433,   746,  1755,   747,
    1110,   170,   170,   140,   140,  1808,  1809,   466,  1331,  1331,
    1331,   150,  1508,  1512,   453,  1045,  1387,   148,    13,    14,
      15,    16,    17,  1227,   902,   486,   748,   466,    -3,    79,
      72,   533,   433,  1549,  1549,  1443,  1444,    96,    96,  1448,
    1444,   775,   487,  1452,  1444,   418,  1017,  1436,  1203,  -415,
     734,   447,   170,   -17,   498,   453,   788,   170,   488,  1488,
    1436,    77,    78,  1017,  1500,   107,   107,  1628,  1066,   789,
    1715,   977,   978,   979,   980,   140,    89,   453,   453,  1480,
     489,  1741,  1066,   466,  1742,  1444,   490,   799,    79,  1526,
    1752,  1753,   812,    79,    79,    79,   453,   494,   813,   491,
     814,  1709,   816,  1671,   817,    89,   111,   111,   805,  1176,
     112,   112,  1762,   942,   818,  1174,  1821,  1444,  1822,  1444,
    1778,  1779,   246,   257,  1997,  1998,  1441,  1442,   819,   973,
     974,  1480,   822,   298,   975,   976,   322,  1678,  1678,    61,
     824,   286,   103,   103,    64,    65,    66,    67,    68,    69,
      70,   954,   418,   840,  1428,    13,    14,    15,    16,    17,
    -532,    79,   453,   981,   982,  1397,  1398,    79,  -530,    79,
     849,   858,   531,   871,   873,   339,    79,   467,   806,   892,
     595,   877,   890,   618,   909,   604,   921,   914,   922,   949,
     466,   955,   943,   946,   990,   807,   995,   595,   855,   466,
    1016,   595,   140,  1017,  1024,  1063,  1803,  1072,   254,   264,
    1073,   808,  1074,    57,   243,     6,     7,     8,     9,    10,
      11,    12,   508,  1075,  1076,  1077,  1093,  1095,   615,  1525,
    1471,  -715,   140,   809,  1104,  1105,   466,  1106,  -615,   810,
    1165,  1166,   256,  1182,  1195,  1864,   140,  1196,  1197,  1207,
    1212,   820,   811,  1215,  1238,  1211,  1295,  1218,    79,  1217,
     170,  1219,  1549,   170,   170,   170,    96,  1813,  1220,  1221,
    1223,  1224,    72,    79,  1225,    79,   402,  1230,  1231,  1239,
    1176,  1255,  1260,   418,  1261,   149,  1174,   170,  1524,  1262,
    1837,  1269,  1656,   170,   107,  1270,   498,  1870,   595,  1271,
    1272,   552,  -603,    77,    78,   260,  1864,  1280,   170,  -602,
    1302,    89,    89,  -716,  1314,  1480,  1337,  1336,  1347,  1350,
      79,    61,  1351,    79,   168,   169,    64,    65,    66,    67,
      68,    69,    70,  1360,   466,   111,  1367,  1361,   466,   112,
     248,   530,  1362,  1369,   942,   170,  -638,  1377,  1371,  1717,
     466,   485,  1526,  1381,  1383,   140,   466,  1384,  1385,  1391,
    1424,  1420,  1421,   453,  1434,  1393,   339,  1436,  1451,   493,
    1463,   103,  1476,    79,    79,  1481,  1482,  1483,  1484,   447,
    1444,  1502,    79,  1175,  1489,  1549,  1492,  1501,  1504,    96,
    1514,   402,  1340,   402,  1352,  1540,  1579,  1516,  1581,  1594,
    1517,   819,  1557,  1091,   740,  1933,  1916,  1837,  1558,  1584,
    1589,  1870,  1560,  1562,  1591,  1870,  1870,   107,  1866,   402,
     751,   486,  1574,   754,    79,  1582,  1583,   534,  1592,  1595,
    1596,   466,   447,  1950,  1600,   785,  1597,   467,   487,  1598,
    1605,  1972,  1611,  1969,  1609,  1614,  1613,  1617,  1621,   595,
     447,  1629,  1630,  1634,   488,   466,  1635,   426,   111,  1645,
    1652,  1194,   112,  1988,  1643,  1488,  1665,  1988,  1248,  1524,
    1519,  1685,  1689,   595,  1687,  1672,   489,  1524,   170,  1866,
     508,   170,   490,   531,   210,  1712,   595,  1714,   402,  2002,
    1719,  1726,   494,  1730,   103,   491,   183,   140,   466,   569,
    1731,  1987,  1999,  1732,  1733,   184,   466,  1740,   185,   246,
     257,  1746,  1747,    79,   140,    79,  1750,  1996,  1751,  1760,
    1757,   170,  1761,  1787,   820,  1795,   466,  1549,   466,   466,
    1796,    96,  1802,  1800,  1128,  1815,  1806,  1807,  1819,  1820,
      89,  1816,   498,  -604,  1549,  1828,  1829,  1830,    96,  1831,
     466,  1832,   140,  1847,  1175,  1849,  -515,  1857,  1867,   107,
    1881,  1883,  1855,    79,    79,  1884,   196,  1859,    82,  1885,
    1886,   147,  1259,  1875,   466,  1868,   107,   447,  1753,   453,
     453,  1897,  1549,  1919,   466,  1904,    96,  1905,  1910,  1266,
    1267,  1920,   418,  1921,  1924,  1925,    79,  1928,   457,   902,
     111,  1931,   629,  1940,   112,  1945,  1941,  1956,  1949,   466,
    1951,   466,  1957,  1970,   107,  1971,  1973,   111,   447,  1983,
    1985,   112,  1994,  1990,  1986,    82,  1991,   466,  1774,  1995,
    2005,  1524,  2006,   466,  2009,  1737,   103,  1534,   679,  1447,
     180,   938,   983,   466,   984,   987,   985,    79,  1984,    82,
    1366,   986,   530,   103,  1716,   111,  1373,    79,  1934,   112,
    1798,  1943,   220,    89,  1794,   245,  1854,    61,  1979,    82,
     170,  1968,    64,    65,    66,    67,    68,    69,    70,    18,
    1879,  1977,   298,  1710,   170,  1711,  1953,  1912,  1992,  1911,
    1382,   103,   167,   170,    61,   524,  1668,   168,   169,    64,
      65,    66,    67,    68,    69,    70,   147,  1835,  1892,  1513,
    1062,  1679,    82,  1379,   147,  1273,    74,   296,   302,  1184,
     875,    51,    52,    53,    54,   795,   740,   740,  1612,   321,
     170,  1723,  1524,     3,  1213,   998,  1009,     0,   999,  1012,
     595,  1000,     0,   618,     0,     0,   409,   180,   180,     0,
       0,     0,   170,     0,     0,     0,     0,     0,   147,   439,
       0,   467,   245,    61,   785,     0,     0,     0,    64,    65,
      66,    67,    68,    69,    70,   962,   236,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   220,   220,     0,     0,
       0,     0,     0,   447,     0,     0,     0,     0,     0,     0,
     508,     0,     0,   296,  1080,    89,     0,     0,  1084,     0,
       0,     0,    82,     0,     0,   963,  1205,     0,     0,     0,
       0,   298,    89,     0,     0,   245,  1682,     0,   170,     0,
       0,     0,   170,     0,     0,     0,     0,     0,     0,  1459,
    1460,     0,     0,     0,   170,     0,     0,     0,     0,     0,
     170,     0,     0,  1474,  1475,   302,    18,     0,   637,     0,
      89,   302,   296,   296,     0,     0,     0,   170,     0,   147,
       0,   570,   298,     0,    61,     0,   170,   217,   218,    64,
      65,    66,    67,    68,    69,    70,  1496,  1497,   321,   605,
     614,     0,     0,     0,    47,    48,    49,    50,    51,    52,
      53,    54,    72,     0,   298,   321,     0,     0,    61,   321,
       0,   168,   169,    64,    65,    66,    67,    68,    69,    70,
       0,     0,  1518,    74,   418,   170,     0,  1243,     0,  1519,
       0,     0,   638,    77,    78,  1243,     0,   453,   453,     0,
       0,     0,   409,     0,     0,     0,     0,   639,     0,   170,
     640,   641,    64,    65,    66,    67,    68,    69,    70,     0,
     249,     0,     0,     0,  1243,     0,     0,   467,     0,     0,
       0,   269,     0,     0,     0,     0,   409,     0,     0,   735,
       0,     0,     0,     0,     0,     0,   180,   306,   307,   308,
     309,     0,   170,   740,     0,     0,     0,     0,     0,     0,
     170,     0,   147,     0,     0,     0,   439,     0,     0,  1918,
     764,     0,   614,   249,     0,    13,    14,    15,    16,    17,
     170,     0,   170,   170,     0,    61,     0,  1243,   168,   169,
      64,    65,    66,    67,    68,    69,    70,     0,     0,     0,
       0,     0,     0,     0,   170,     0,     0,     0,   595,     0,
     220,     0,     0,     0,     0,     0,     0,   249,     0,   220,
       0,     0,     0,     0,  1305,     0,     0,  1309,   170,  1647,
    1648,     0,     0,    57,     0,   447,     0,   310,   170,   296,
       0,   409,   409,     0,     0,   296,     0,   321,     0,     0,
       0,     0,     0,     0,     0,   311,     0,     0,     0,     0,
       0,     0,     0,   170,    61,   170,     0,   217,   218,    64,
      65,    66,    67,    68,    69,    70,     0,     0,     0,     0,
     249,   170,   453,     0,     0,   296,     0,   170,     0,     0,
       0,     0,    72,     0,     0,     0,   296,   170,   296,     0,
     321,  2003,    82,    13,    14,    15,    16,    17,     0,     0,
     249,  2010,   219,    74,     0,     0,   249,   321,   439,     0,
     614,     0,     0,    77,    78,     0,     0,     0,   605,  1349,
       0,     0,   605,     0,     0,     0,  1727,     0,     0,     0,
       0,   321,     0,     0,     0,   249,     0,     0,     0,     0,
       0,   614,     0,     0,   321,     0,     0,     0,     0,     0,
       0,    57,   147,     0,     0,   467,  1745,     0,     0,     0,
       0,  1748,  1749,  1243,     0,   409,     0,   147,   147,     0,
     409,     0,     0,     0,     0,     0,   409,     0,     0,   147,
     147,   147,    61,     0,  1374,   217,   218,    64,    65,    66,
      67,    68,    69,    70,     0,     0,    13,    14,    15,    16,
      17,     0,  1457,   114,     0,     0,   114,     0,   447,    61,
      72,  1466,   168,   169,    64,    65,    66,    67,    68,    69,
      70,     0,     0,     0,     0,     0,     0,    57,     0,     0,
     762,    74,     0,     0,   602,   439,     0,     0,     0,     0,
       0,    77,   763,     0,     0,     0,     0,     0,     0,     0,
       0,   735,   735,     0,    57,     0,     0,   249,    61,   409,
     114,   217,   218,    64,    65,    66,    67,    68,    69,    70,
       0,     0,     0,     0,     0,     0,   439,     0,     0,   764,
     467,   764,  1375,     0,   114,    61,     0,     0,   217,   218,
      64,    65,    66,    67,    68,    69,    70,     0,   321,   321,
     251,     0,     0,     0,   114,     0,  1273,    74,     0,     0,
       0,     0,     0,    72,     0,     0,     0,   321,     0,   296,
       0,     0,     0,     0,    13,    14,    15,    16,    17,     0,
       0,   249,     0,  1914,    74,     0,     0,   498,   296,     0,
       0,   114,     0,     0,    77,    78,     0,   114,     0,   114,
       0,   249,     0,   251,     0,     0,   467,     0,     0,     0,
       0,  1243,     0,   318,   114,   349,  1243,  1243,  1243,     0,
       0,   249,     0,     0,     0,     0,   409,     0,     0,     0,
    1585,   413,    57,   321,     0,     0,     0,     0,     0,   147,
     409,     0,     0,   114,   413,     0,     0,   251,   321,     0,
    1189,     0,     0,     0,     0,   249,     0,     0,     0,     0,
       0,   605,     0,    61,     0,     0,   217,   218,    64,    65,
      66,    67,    68,    69,    70,     0,     0,     0,     0,   249,
       0,     0,     0,     0,     0,     0,   249,     0,     0,     0,
       0,    72,     0,     0,   114,     0,     0,   114,     0,     0,
    1660,   439,     0,     0,     0,     0,     0,  1958,     0,     0,
     251,   295,    74,     0,     0,     0,   671,     0,     0,   249,
     269,    57,    77,    78,     0,     0,     0,   548,     0,     0,
       0,     0,     0,     0,     0,   114,     0,     0,     0,     0,
     251,     0,     0,     0,     0,     0,   251,    13,    14,    15,
      16,    17,    61,     0,   114,   217,   218,    64,    65,    66,
      67,    68,    69,    70,     0,     0,     0,     0,   735,     0,
       0,     0,     0,   114,     0,   251,   114,     0,     0,    57,
      72,     0,     0,     0,     0,   764,     0,     0,     0,     0,
     114,     0,   764,  1688,   114,     0,  1243,     0,  1243,     0,
    1914,    74,  1692,     0,   498,    57,     0,     0,     0,     0,
      61,    77,    78,   217,   218,    64,    65,    66,    67,    68,
      69,    70,     0,     0,     0,     0,     0,   413,     0,     0,
       0,     0,     0,     0,   321,     0,    61,     0,    72,  1721,
       0,    64,    65,    66,    67,    68,    69,    70,  1660,  1660,
       0,     0,     0,     0,     0,   831,   833,     0,   219,    74,
     595,   413,     0,     0,    72,     0,     0,     0,     0,    77,
      78,     0,     0,     0,   147,     0,     0,     0,     0,     0,
       0,     0,   409,     0,    73,    74,     0,   114,     0,     0,
       0,   413,     0,     0,     0,    77,    78,   251,    61,     0,
       0,   168,   169,    64,    65,    66,    67,    68,    69,    70,
       0,   409,     0,   249,     0,     0,     0,     0,     0,     0,
       0,   595,     0,     0,   249,     0,     0,     0,   245,    82,
       0,     0,     0,     0,     0,     0,     0,  1777,    57,     0,
       0,  1786,     0,   296,     0,   249,     0,     0,     0,   147,
       0,     0,     0,  1793,     0,     0,   439,  1356,  1660,  1799,
       0,     0,     0,     0,     0,     0,   413,   413,     0,    61,
       0,   251,   114,     0,    64,    65,    66,    67,    68,    69,
      70,     0,     0,   439,     0,     0,     0,   147,     0,   671,
       0,     0,     0,     0,   671,     0,     0,    72,     0,     0,
     671,     0,     0,   114,     0,     0,     0,     0,   114,     0,
       0,   251,   114,     0,   114,     0,     0,    73,    74,   671,
       0,     0,  1890,     0,     0,   114,  1660,   114,    77,    78,
       0,     0,     0,     0,  1845,     0,     0,     0,     0,     0,
       0,   349,   114,   413,     0,   251,     0,     0,     0,     0,
     321,   321,     0,     0,     0,   989,  1660,   247,  1874,     0,
       0,     0,     0,     0,     0,     0,   114,     0,   268,   251,
     271,     0,   273,   548,     0,     0,   251,     0,     0,   114,
       0,   908,     0,     0,     0,     0,     0,   114,   147,   147,
     147,   147,   147,   147,     0,     0,     0,     0,  1520,   302,
     413,  1903,   114,   114,     0,   413,  1660,  1660,     0,  1909,
     247,   413,   271,   273,   114,   114,   114,   409,   409,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1927,
       0,  1929,  1930,     0,     0,     0,     0,     0,     0,  1660,
       0,     0,     0,     0,     0,     0,     0,   245,     0,     0,
       0,     0,    61,  1939,   247,   168,   169,    64,    65,    66,
      67,    68,    69,    70,     0,     0,   439,     0,     0,     0,
     413,     0,     0,     0,     0,    61,     0,  1954,   345,   346,
      64,    65,    66,    67,    68,    69,    70,  1959,     0,   147,
      61,     0,     0,     0,   413,    64,    65,    66,    67,    68,
      69,    70,    13,    14,    15,    16,    17,     0,     0,     0,
       0,   413,  1982,     0,  1959,     0,     0,   247,    72,   271,
     273,     0,     0,   249,     0,  1354,    75,     0,     0,     0,
    1993,   347,     0,   114,   114,     0,  1982,     0,  1018,    74,
       0,     0,   602,     0,     0,     0,  2001,   247,     0,    77,
      78,     0,   114,   247,     0,   249,     0,     0,     0,     0,
      57,    61,     0,     0,   217,   218,    64,    65,    66,    67,
      68,    69,    70,     0,     0,  1657,     0,     0,     0,  1520,
     114,   409,   247,     0,     0,  1520,     0,  1520,   620,    72,
     273,    61,     0,     0,   217,   218,    64,    65,    66,    67,
      68,    69,    70,   251,     0,     0,     0,     0,     0,  1914,
      74,   413,     0,   498,   251,   302,   147,     0,   114,    72,
      77,    78,     0,     0,   114,   413,     0,     0,     0,     0,
       0,    57,     0,   114,     0,  1191,   413,     0,   114,  1518,
      74,     0,     0,     0,     0,     0,   409,     0,     0,     0,
      77,    78,     0,     0,     0,     0,     0,   321,     0,     0,
     147,     0,    61,     0,     0,   217,   218,    64,    65,    66,
      67,    68,    69,    70,     0,     0,     0,     0,   247,     0,
       0,     0,     0,   147,     0,     0,   413,     0,     0,     0,
      72,     0,     0,     0,   249,    57,     0,     0,     0,     0,
       0,     0,     0,     0,   247,     0,   620,   273,   321,   321,
     295,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    77,    78,  1657,  1657,     0,    61,     0,     0,   217,
     218,    64,    65,    66,    67,    68,    69,    70,  1520,     0,
       0,  1520,   249,     0,     0,     0,     0,     0,     0,   114,
       0,   247,     0,     0,    72,     0,     0,     0,   302,     0,
       0,     0,     0,     0,     0,     0,   114,   114,     0,   409,
       0,   247,     0,     0,  1518,    74,   247,     0,   247,     0,
       0,     0,     0,     0,     0,    77,    78,     0,     0,     0,
       0,     0,   296,     0,     0,     0,     0,     0,   247,     0,
     247,   247,    61,     0,     0,   168,   169,    64,    65,    66,
      67,    68,    69,    70,     0,     0,     0,    61,   247,   114,
     217,   218,    64,    65,    66,    67,    68,    69,    70,    61,
     247,     0,     0,  1657,    64,    65,    66,    67,    68,    69,
      70,  1240,  1520,     0,     0,  1241,     0,  1242,     0,     0,
       0,     0,   247,     0,   620,   273,     0,     0,   578,   114,
       0,     0,     0,     0,     0,     0,     0,   413,   147,     0,
       0,     0,     0,  1202,     0,     0,   247,   620,    74,     0,
       0,  1439,     0,   247,     0,     0,     0,     0,     0,    98,
       0,     0,   151,   321,     0,     0,   413,     0,     0,     0,
       0,  1657,     0,     0,     0,     0,     0,     0,     0,     0,
     249,   147,     0,   251,   114,    61,   247,   268,   217,   218,
      64,    65,    66,    67,    68,    69,    70,     0,   147,   147,
       0,  1915,   302,     0,   114,     0,     0,     0,     0,     0,
       0,   413,     0,    72,    61,  1191,    98,     0,     0,    64,
      65,    66,    67,    68,    69,    70,  1240,  1417,     0,     0,
    1241,     0,  1242,   762,    74,     0,   147,   602,   413,     0,
     195,     0,   114,     0,    77,   763,     0,     0,     0,     0,
       0,  1915,  1915,     0,     0,     0,     0,   604,  1539,     0,
     258,  1542,  1553,    74,     0,     0,  1624,  1559,     0,     0,
       0,  1563,    61,  1565,     0,   168,   169,    64,    65,    66,
      67,    68,    69,    70,  1915,   108,   114,   114,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   288,     0,     0,
     114,   114,     0,    98,    61,   114,   114,   217,   218,    64,
      65,    66,    67,    68,    69,    70,     0,     0,     0,    61,
     323,     0,     0,   249,    64,    65,    66,    67,    68,    69,
      70,  1240,    72,   114,   114,  1241,     0,  1242,   419,     0,
       0,     0,   108,   114,   114,   114,   114,   114,   114,   288,
     445,     0,  1518,    74,   251,     0,     0,     0,     0,  1519,
       0,     0,     0,    77,    78,     0,     0,     0,    74,     0,
       0,  1626,   413,   413,     0,     0,     0,     0,   492,     0,
     247,     0,     0,     0,     0,     0,   259,     0,     0,     0,
       0,   247,     0,     0,   512,     0,     0,     0,     0,   517,
     519,     0,   251,   195,     0,     0,     0,     0,    61,     0,
       0,  1651,   247,    64,    65,    66,    67,    68,    69,    70,
    1240,   413,     0,   247,  1241,   539,  1242,     0,   541,   108,
     542,     0,   247,     0,     0,     0,     0,     0,     0,     0,
       0,   559,     0,  1684,   114,     0,   327,     0,     0,     0,
       0,     0,     0,     0,   571,  1690,     0,    74,     0,     0,
      61,     0,  1693,   217,   218,    64,    65,    66,    67,    68,
      69,    70,     0,     0,     0,     0,   446,     0,     0,   593,
       0,     0,   617,     0,     0,     0,     0,     0,    72,     0,
    1542,     0,     0,     0,     0,     0,   624,   249,     0,     0,
     624,     0,     0,     0,     0,     0,     0,     0,   219,    74,
       0,     0,     0,     0,     0,     0,   114,   114,     0,    77,
      78,     0,     0,     0,   657,     0,     0,     0,     0,     0,
     247,     0,     0,     0,     0,    61,   413,     0,   217,   218,
      64,    65,    66,    67,    68,    69,    70,     0,     0,     0,
       0,   540,   114,     0,   247,     0,     0,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,   108,     0,    61,
     251,   114,   189,    63,    64,    65,    66,    67,    68,    69,
      70,     0,     0,   295,    74,     0,     0,     0,     0,     0,
       0,     0,     0,   288,    77,    78,     0,   593,  1785,     0,
       0,   413,     0,     0,     0,   594,     0,     0,   259,     0,
    1790,  1792,   114,  1553,     0,   114,     0,     0,    74,     0,
     657,   784,   594,   114,     0,     0,   594,     0,     0,     0,
       0,     0,     0,     0,     0,   365,     0,   366,   114,   367,
      62,    63,    64,    65,    66,    67,    68,    69,    70,     0,
       0,     0,     0,   114,     0,     0,     0,     0,   114,   114,
       0,     0,     0,   114,   114,    61,     0,     0,   543,    63,
      64,    65,    66,    67,    68,    69,    70,     0,   445,     0,
       0,     0,     0,     0,     0,   678,     0,     0,    75,   376,
     247,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1851,     0,     0,     0,     0,     0,     0,     0,     0,   851,
       0,     0,     0,   251,   519,     0,     0,   992,   862,     0,
     559,     0,   247,   594,   413,     0,     0,     0,   247,     0,
       0,   323,    61,    98,     0,   545,   546,    64,    65,    66,
      67,    68,    69,    70,     0,     0,     0,    61,   624,   883,
      62,    63,    64,    65,    66,    67,    68,    69,    70,     0,
       0,     0,     0,   894,     0,  1906,     0,  1908,     0,     0,
       0,     0,   593,     0,     0,     0,     0,   903,     0,     0,
       0,     0,     0,    75,    61,   624,     0,   168,   169,    64,
      65,    66,    67,    68,    69,    70,     0,     0,    75,     0,
       0,     0,    61,     0,   446,   168,   169,    64,    65,    66,
      67,    68,    69,    70,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   114,     0,     0,     0,     0,     0,     0,
       0,     0,   456,     0,     0,   327,     0,     0,     0,  1955,
       0,     0,     0,     0,   259,     0,   108,     0,   114,     0,
     460,  1963,  1965,  1966,     0,     0,     0,   446,     0,   108,
       0,   247,     0,     0,     0,     0,   114,     0,     0,     0,
       0,     0,     0,     0,   594,   446,   445,     0,     0,     0,
       0,     0,     0,   114,   114,     0,     0,   251,     0,     0,
       0,     0,     0,  1001,     0,     0,     0,     0,   594,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   247,
       0,   594,     0,     0,     0,     0,     0,   883,     0,     0,
       0,   114,  1025,   719,   720,   721,   722,   723,   724,   725,
     726,   727,   728,   729,     0,     0,     0,   204,     0,   445,
     445,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   114,     0,     0,     0,   445,     0,
       0,     0,     0,     0,   730,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   851,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   446,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1161,     0,     0,
       0,     0,     0,     0,   445,   113,     0,     0,     0,     0,
     151,     0,     0,     0,     0,     0,     0,     0,     0,   624,
       0,     0,  1193,   446,   851,     0,     0,     0,     0,  1199,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   327,   327,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   247,     0,     0,
       0,     0,   113,     0,   327,     0,     0,     0,     0,     0,
       0,     0,   323,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   327,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1836,     0,   261,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   108,     0,     0,     0,     0,     0,     0,
     327,     0,     0,     0,     0,   851,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   594,     0,     0,   259,   113,
     327,   360,   851,   851,     0,   361,     0,   362,     0,     0,
       0,     0,     0,     0,     0,     0,   331,     0,     0,     0,
       0,     0,     0,     0,   363,     0,     0,     0,     0,     0,
       0,     0,     0,   247,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   448,     0,   446,     0,
     247,   364,   365,     0,   366,   445,   367,    62,    63,    64,
      65,    66,    67,    68,    69,    70,   368,   369,   357,     0,
     370,   371,   372,     0,   373,   374,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1332,     0,     0,     0,     0,
       0,     0,   375,  1161,     0,    75,   376,     0,     0,     0,
       0,   327,   377,    77,    78,   378,   379,   380,   381,     0,
       0,     0,     0,     0,     0,   247,     0,     0,   327,   327,
       0,     0,  1161,     0,     0,     0,     0,   113,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1380,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   596,     0,   593,   261,     0,
       0,   327,     0,     0,     0,     0,   517,     0,     0,     0,
       0,     0,   596,     0,     0,     0,   596,     0,     0,     0,
       0,     0,     0,     0,   323,     0,     0,     0,     0,     0,
       0,     0,     0,   118,     0,     0,   118,     0,     0,     0,
       0,     0,     0,     0,   247,     0,     0,     0,     0,   108,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   119,     0,     0,   119,     0,     0,
       0,     0,   851,   851,     0,     0,     0,     0,   108,     0,
       0,     0,     0,     0,     0,     0,   851,   851,     0,     0,
     118,   445,   445,     0,     0,     0,   259,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   596,   118,     0,     0,     0,     0,   851,
     851,   119,     0,   594,     0,     0,   247,     0,     0,  1332,
    1332,  1332,   151,     0,   118,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   119,     0,     0,     0,     0,
     446,     0,     0,     0,     0,     0,     0,     0,  1548,  1548,
       0,     0,     0,     0,     0,   119,     0,     0,     0,     0,
       0,   118,     0,     0,     0,     0,     0,   118,     0,   118,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   448,     0,     0,     0,   327,   327,
       0,     0,   119,     0,     0,     0,     0,   323,   119,     0,
     119,   118,   327,   327,     0,     0,     0,   327,   327,     0,
       0,     0,     0,   118,     0,   331,     0,     0,     0,     0,
     151,     0,     0,     0,   261,     0,   113,     0,     0,     0,
       0,     0,   119,     0,     0,   327,   327,   448,     0,   113,
       0,     0,     0,     0,   119,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   596,   448,     0,     0,     0,     0,
       0,     0,     0,     0,   118,     0,     0,   118,     0,     0,
       0,     0,   118,     0,   108,   108,     0,     0,   596,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   596,   851,   851,     0,   119,     0,     0,   119,     0,
       0,     0,     0,   119,     0,   118,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1674,     0,
       0,     0,     0,   446,   118,     0,     0,     0,   851,     0,
       0,     0,     0,     0,     0,     0,   119,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1691,     0,     0,
       0,     0,     0,     0,     0,   119,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1548,     0,     0,
       0,     0,   448,     0,     0,     0,     0,     0,   323,     0,
       0,   151,     0,     0,     0,   123,     0,   118,   123,   851,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   327,   327,
       0,     0,     0,   448,     0,     0,     0,     0,   119,   851,
       0,   118,     0,     0,   851,   851,     0,     0,     0,   445,
     445,     0,     0,     0,     0,   331,   331,     0,     0,     0,
       0,     0,   123,     0,   327,  1766,     0,   118,     0,     0,
       0,     0,   119,     0,   331,     0,     0,     0,     0,     0,
       0,     0,     0,   259,     0,     0,   123,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   119,     0,
       0,     0,   331,     0,     0,     0,   123,     0,     0,     0,
    1548,     0,     0,   108,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   327,     0,     0,     0,     0,     0,
       0,     0,     0,   113,     0,   327,     0,     0,     0,     0,
     331,     0,     0,   123,     0,     0,   118,   118,     0,   123,
       0,   123,     0,     0,     0,   596,     0,     0,   261,     0,
     331,     0,     0,     0,     0,   327,     0,     0,     0,     0,
     327,   327,     0,     0,     0,   327,   327,   119,   119,     0,
       0,     0,     0,   123,     0,     0,     0,     0,   118,     0,
       0,     0,   118,     0,   118,   123,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   118,   448,  1865,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   119,
       0,     0,     0,   119,     0,   119,     0,     0,     0,     0,
       0,     0,     0,     0,   445,     0,   108,     0,   119,     0,
       0,     0,     0,     0,     0,     0,   123,     0,     0,   123,
       0,     0,  1548,     0,   123,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   118,     0,  1548,
    1865,   331,     0,     0,     0,     0,     0,     0,     0,     0,
     118,     0,   118,   118,     0,   118,     0,   123,   331,   331,
       0,   118,     0,     0,   118,   118,   118,     0,   119,     0,
       0,     0,     0,     0,     0,     0,   123,  1548,     0,     0,
       0,   119,     0,   119,   119,     0,   119,     0,     0,     0,
       0,     0,   119,  1947,     0,   119,   119,   119,     0,     0,
       0,     0,     0,     0,     0,   594,     0,     0,     0,     0,
     851,   331,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     327,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   118,     0,     0,     0,   108,   123,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   113,
       0,     0,     0,     0,     0,   108,   594,     0,     0,     0,
       0,     0,     0,     0,     0,   119,     0,     0,     0,     0,
       0,     0,     0,   123,     0,     0,     0,     0,   113,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   108,     0,     0,   261,     0,     0,   123,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   596,     0,     0,   327,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     448,   118,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   118,   118,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   123,   123,
       0,     0,   119,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   119,   119,     0,   331,   331,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   331,   331,     0,     0,     0,   331,   331,     0,
     123,     0,     0,     0,   123,     0,   123,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   123,
       0,     0,     0,     0,     0,   331,   331,     0,    13,    14,
      15,    16,    17,    18,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,   113,   113,    45,    46,     0,     0,
       0,    47,    48,    49,    50,    51,    52,    53,    54,   123,
       0,     0,     0,     0,     0,     1,    57,     0,   145,     0,
       0,     0,   123,     0,   123,   123,     0,   123,     0,     0,
       0,     0,     0,   123,     0,     0,   123,   123,   123,     0,
       0,     0,     0,   448,     0,     0,     0,     0,     0,     0,
      62,    63,   203,     0,     0,     0,     0,     0,   214,   215,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   192,   277,     0,     0,     0,     0,   930,    75,   924,
       0,     0,     0,     0,     0,     0,    77,    78,     0,   118,
       0,     0,     0,     0,     0,     0,   123,   118,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   331,   331,
     119,     0,     0,     0,     0,     0,   118,     0,   119,     0,
     283,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   118,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   331,     0,     0,   119,     0,     0,
       0,     0,     0,     0,   118,     0,     0,     0,     0,     0,
       0,     0,     0,   261,     0,   119,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   193,     0,     0,   119,     0,     0,     0,     0,
       0,     0,   118,   113,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   123,   331,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   283,   331,   123,   123,     0,     0,
       0,     0,     0,   119,     0,     0,     0,     0,   567,   520,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   283,
       0,     0,     0,     0,     0,   331,     0,   193,     0,   283,
     331,   331,     0,     0,     0,   331,   331,     0,     0,     0,
       0,     0,   193,   551,   555,     0,     0,     0,     0,     0,
     562,   563,     0,     0,     0,     0,     0,     0,     0,   193,
       0,     0,     0,     0,     0,     0,   573,     0,     0,     0,
       0,     0,   442,   118,   118,   118,   118,   118,   118,     0,
       0,     0,     0,     0,     0,   591,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   113,     0,     0,     0,
       0,     0,   118,   118,   119,   119,   119,   119,   119,   119,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   193,     0,     0,     0,     0,
       0,     0,     0,   119,   119,     0,     0,     0,     0,     0,
       0,   677,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   760,     0,   761,     0,     0,     0,
       0,     0,     0,     0,     0,   777,   778,     0,     0,     0,
       0,     0,   718,     0,   118,     0,     0,     0,     0,     0,
       0,     0,   193,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   596,     0,     0,   756,     0,
       0,   193,   759,     0,     0,   119,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     331,   781,     0,     0,     0,   782,   783,     0,     0,   786,
       0,   123,     0,     0,     0,     0,     0,     0,   113,   123,
       0,     0,     0,     0,   800,   801,   802,   803,     0,     0,
       0,     0,     0,     0,     0,   113,   596,     0,     0,     0,
       0,     0,     0,   825,     0,     0,   118,     0,   123,     0,
       0,   828,     0,   861,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   123,     0,     0,   193,
       0,     0,     0,   113,     0,     0,     0,   119,     0,   283,
       0,   118,     0,     0,     0,     0,   123,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   193,
       0,     0,     0,     0,     0,     0,   331,     0,     0,     0,
     866,   118,   119,     0,     0,     0,     0,   551,     0,     0,
       0,     0,     0,   872,   123,   118,     0,     0,     0,     0,
       0,   360,     0,     0,     0,   361,   165,   362,     0,     0,
       0,     0,   119,     0,     0,     0,   886,   891,   118,     0,
       0,     0,     0,     0,   363,     0,   119,     0,     0,     0,
       0,     0,   165,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   193,   193,     0,     0,     0,   119,
     442,   364,   365,     0,   366,     0,   367,  1788,    63,    64,
      65,    66,    67,    68,    69,    70,   368,   369,   357,     0,
     370,   371,   372,     0,   373,   374,     0,     0,   165,     0,
     934,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,   165,     0,   165,     0,   123,   123,   123,   123,   123,
     123,     0,   375,   193,   118,    75,   376,     0,     0,     0,
       0,     0,   377,    77,    78,   378,   379,   380,   381,     0,
       0,   442,     0,   351,   123,   123,  1789,  -179,     0,     0,
       0,     0,     0,     0,     0,   119,     0,     0,     0,     0,
     351,  1040,     0,     0,   193,     0,     0,     0,     0,   997,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1014,   193,     0,     0,  1015,     0,
       0,     0,     0,     0,     0,     0,     0,   886,   165,     0,
       0,     0,   165,     0,     0,   165,   165,     0,     0,   165,
       0,     0,   165,   165,     0,     0,   123,     0,     0,  1055,
       0,     0,     0,     0,     0,     0,     0,     0,  1064,     0,
       0,     0,     0,     0,  1067,     0,     0,     0,  1108,  1109,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1167,
    1168,  1169,     0,     0,  1171,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   118,     0,   442,     0,
       0,     1,     0,     0,   165,     0,     0,   165,     1,     0,
       0,     0,     0,   118,     0,     0,     0,     0,     0,     0,
       0,     0,   193,     0,     0,     0,     0,   119,   165,     0,
       0,     0,     0,     0,     0,     1,     0,     0,   123,   442,
       0,     0,     0,   165,   119,     0,     0,     0,     0,     0,
       0,   118,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   442,   442,     0,  1236,     0,     0,     0,     0,     0,
       0,     0,     0,   123,     0,     0,     0,     0,     0,     0,
     442,     0,   119,     0,     0,     0,     0,     0,     0,  1216,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   123,     0,     0,     0,     0,     0,     0,
    1257,     0,     0,     0,     0,     0,     0,   123,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   165,   387,     0,
     123,     0,     0,     0,     0,     0,   442,     0,     0,     0,
       0,     0,     0,   193,     0,     0,     0,  1281,     0,     0,
       0,     0,     0,     0,     0,     0,  1285,  1286,  1287,  1288,
       0,     0,  1264,     0,  1293,  1294,  1265,     0,     0,     0,
       0,     0,     0,   886,  1301,     0,     0,     0,     0,     0,
       0,     0,   351,  1278,     0,     0,     0,     0,     0,     0,
    1279,     0,     0,     0,   165,  1315,     0,  1316,     0,  1283,
       0,  1284,     0,     0,   193,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   123,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1311,     0,     0,     0,  1312,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1372,   145,     0,     0,     1,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   351,     0,
       0,     0,     0,     0,     0,     0,  1386,     0,     0,     0,
       0,     0,     0,  1390,     0,  1392,  1394,     0,     0,     0,
       0,     0,     0,     0,  1400,     0,  1401,     0,  1402,     0,
    1404,     0,     0,     0,     0,  1412,     0,     0,   165,   165,
     649,     0,     0,   387,   654,     0,     0,     0,     0,     0,
       0,   165,     0,   660,   661,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   442,   387,   387,
       0,     0,  1399,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   123,   387,
       0,     0,     0,     0,     0,  1454,  1422,     0,     0,     0,
       0,     0,  1461,  1462,     0,   123,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   387,
       0,     0,     0,     0,     0,     0,  1485,     0,     0,     0,
       0,     0,     0,  1490,     0,     0,  1491,     0,     0,     0,
       0,     0,     0,   123,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   214,   165,   165,     0,
       0,     0,     0,   165,     0,     0,     0,     0,     0,     0,
       0,     0,   193,     0,     0,     0,     0,     0,  1494,   193,
       0,     0,  1495,     0,   165,     0,     0,   165,   165,     0,
     165,     0,   165,   165,     0,     0,  1580,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   193,     0,     0,     0,
       0,     0,  1530,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1599,
       0,   165,     0,     0,     0,   165,     0,  1604,     0,  1606,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1590,     0,     0,  1593,     0,     0,     0,     0,
       0,     0,     0,   442,   442,     0,     0,     0,     0,     0,
    1601,     0,     0,     0,     0,  1632,  1633,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1638,  1639,     0,  1640,     0,     0,     0,     0,     0,     0,
       0,   165,  1644,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1649,  1650,     0,     0,     0,     0,     0,     0,
    1631,     0,     0,     0,     0,     0,     0,     0,     0,  1636,
       0,     0,     0,  1637,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1641,  1642,     0,
       0,     0,     0,     0,     0,   387,   387,   387,   387,   387,
     387,   387,   387,   387,   387,   387,   387,   387,   387,   387,
     387,   387,   387,   387,     0,     0,     0,     0,     0,   193,
       0,     0,     0,     0,     0,     0,   338,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   435,   338,     0,  1728,  1729,
       0,     0,     0,     0,     0,     0,     0,     0,  1735,     0,
       0,     0,     0,   165,     0,   387,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   501,     0,     0,
       0,     0,     0,     0,   501,     0,     0,     0,     0,  1724,
    1725,     0,     0,  1758,  1759,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   242,     0,     0,
       0,   165,     0,     0,   165,     0,    13,    14,    15,    16,
      17,     0,     0,    19,   193,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
    -419,  -419,   501,  -419,    45,    46,     0,  -419,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1814,    57,   338,   606,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1823,     0,     0,  1824,  1825,   627,     0,     0,     0,
     193,  1827,     0,     0,     0,     0,     0,  1801,    62,    63,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     387,     0,     0,     0,     0,   387,     0,     0,     0,     0,
    1593,     0,     0,    72,   165,     0,   387,     0,     0,     0,
       0,     0,   165,   165,     0,     0,     0,     0,  1826,     0,
       0,   442,   442,     0,     0,     0,    75,   301,     0,     0,
       0,     0,     0,     0,    77,    78,   501,     0,     0,     0,
       0,     0,     0,     0,     0,  1844,     0,     0,   387,     0,
       0,     0,   501,   752,     0,   501,   755,     0,     0,     0,
       0,     0,  1861,   338,     0,  1862,     0,   606,     0,   165,
       0,     0,     0,     0,     0,     0,     0,     0,   165,  1913,
       0,   165,   200,   165,   165,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   255,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   501,     0,
       0,     0,   501,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   165,     0,     0,     0,     0,  1944,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   338,     0,     0,   200,     0,     0,
       0,   303,  1961,  1932,     0,     0,     0,     0,     0,  1967,
       0,     0,   343,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1980,   387,     0,     0,     0,   200,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     165,     0,   455,     0,   501,   459,     0,   338,     0,     0,
       0,     0,     0,     0,     0,     0,   442,     0,     0,     0,
       0,     0,     0,     0,   881,   338,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   606,     0,     0,     0,   606,
       0,     0,     0,     0,     0,     0,   899,     0,   338,     0,
       0,     0,     0,     0,     0,   200,     0,     0,     0,   387,
       0,     0,     0,     0,     0,     0,     0,     0,   255,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   165,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   387,   387,   387,     0,     0,     0,     0,
     387,   387,     0,     0,   459,     0,     0,     0,     0,     0,
       0,   165,   200,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   387,     0,     0,     0,     0,     0,
       0,   599,     0,   616,     0,     0,     0,   165,     0,     0,
       0,     0,     0,   165,     0,     0,     0,     0,     0,     0,
       0,     0,   338,     0,     0,     0,     0,     0,     0,     0,
       0,   387,   387,     0,     0,     0,     0,     0,   501,   501,
       0,     0,     0,     0,     0,     0,     0,     0,   501,  1010,
       0,   501,  1013,     0,     0,   675,   171,   174,     0,     0,
       0,     0,     0,   338,   163,     0,   606,     0,   606,   606,
       0,     0,     0,     0,     0,   606,     0,     0,   165,     0,
       0,     0,     0,     0,     0,   338,   338,     0,     0,   200,
       0,   212,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   338,     0,     0,     0,   501,     0,
       0,     0,   501,     0,     0,     0,   501,  1081,     0,   599,
     501,  1085,     0,     0,     0,   776,     0,     0,  1088,     0,
       0,     0,     0,     0,     0,     0,   275,     0,     0,     0,
       0,   290,     0,     0,   291,     0,     0,     0,     0,   281,
       0,   282,   165,   165,     0,     0,     0,   312,     0,     0,
     351,     0,     0,     0,   165,     0,     0,     0,     0,     0,
     338,   501,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   200,   200,     0,     0,   606,     0,
     455,     0,     0,    13,    14,    15,    16,    17,     0,     0,
      19,   477,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,     0,   338,     0,
       0,    45,    46,   502,   503,     0,     0,   507,     0,     0,
     510,   511,     0,   343,     0,     0,   528,   165,     0,     0,
       0,    57,     0,     0,     0,     0,   171,     0,     0,     0,
       0,   455,     0,   885,     0,     0,     0,   171,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   656,     0,   599,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   574,   501,     0,     0,     0,     0,
       0,   577,   579,     0,     0,   200,   586,     0,     0,   165,
       0,     0,   606,   606,   387,     0,     0,     0,   675,   606,
     675,   675,     0,   675,     0,     0,   589,     0,     0,   675,
       0,     0,   675,   675,   675,     0,     0,   -16,   631,     0,
       0,   621,     0,   312,     0,     0,   312,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   338,     0,     0,     0,     0,   501,  1306,     0,   501,
    1310,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   455,     0,
       0,     0,     0,   165,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   200,  1198,     0,     0,     0,     0,     0,     0,
      13,    14,    15,    16,    17,     0,     0,     0,     0,   455,
       0,     0,     0,   212,     0,   750,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   779,   780,     0,     0,     0,
       0,   455,   455,     0,     0,     0,   360,     0,     0,     0,
     361,     0,   362,     0,     0,     0,     0,     0,     0,     0,
     455,     0,     0,     0,     0,     0,     0,     0,    57,   363,
       0,     0,     0,   338,     0,     0,     0,     0,     0,   606,
    1408,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   821,     0,     0,     0,   364,   365,   387,   366,
     338,   367,    62,    63,    64,    65,    66,    67,    68,    69,
      70,   368,   369,   357,     0,   370,   371,   372,     0,   373,
     374,     0,     0,     0,     0,     0,   455,    72,   387,     0,
       0,     0,     0,   200,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   776,   501,  1458,     0,   375,     0,     0,
      75,   376,     0,   501,  1467,     0,   606,   377,   438,    78,
     378,   379,   380,   381,     0,     0,   312,   338,   338,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   360,     0,     0,
       0,   361,     0,   362,   343,   411,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   897,   898,   440,     0,
     363,     0,     0,   387,   631,   387,     0,     0,     0,   905,
       0,   468,     0,   468,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   364,   365,     0,
     462,   387,   367,    62,    63,    64,    65,    66,    67,    68,
      69,    70,   368,   369,   357,     0,   370,   371,   372,     0,
     373,   374,     0,     0,     0,   387,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   338,     0,     0,     0,     0,   375,    74,
       0,   463,   464,     0,     0,     0,   465,     0,   377,    77,
      78,   378,   379,   380,   381,     0,     0,     0,     0,   568,
     387,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   455,     0,     0,
       0,     0,     0,     0,     0,  1003,  1004,     0,     0,     0,
       0,  1008,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1042,     0,     0,
       0,     0,  1029,     0,  1054,  1032,  1033,   675,  1036,     0,
    1038,  1039,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   501,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1410,   501,  1079,
       0,     0,     0,  1083,    13,    14,    15,    16,    17,     0,
       0,   255,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1112,
       0,     0,   200,     0,     0,     0,     0,     0,     0,   599,
     360,     0,     0,     0,   361,     0,   362,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    57,   363,   338,     0,   343,     0,     0,     0,
     675,   468,     0,     0,     0,     0,     0,   468,     0,  1200,
    1206,     0,   797,     0,     0,   631,     0,     0,     0,     0,
     364,   365,     0,   366,     0,   367,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   368,   369,   357,     0,   370,
     371,   372,     0,   373,   374,   338,   338,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
     501,   501,     0,   455,   455,     0,     0,     0,     0,     0,
       0,   375,     0,     0,    75,   376,   501,     0,     0,     0,
       0,   377,  1411,    78,   378,   379,   380,   381,     0,     0,
       0,     0,   360,     0,     0,     0,   361,     0,   362,   865,
       0,   675,   675,   675,     0,   675,   675,     0,     0,     0,
       0,     0,   459,     0,     0,   363,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   440,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     893,  1200,   364,   365,     0,   366,     0,   367,    62,    63,
      64,    65,    66,    67,    68,    69,    70,   368,   369,   357,
     255,   370,   371,   372,     0,   373,   374,     0,     0,     0,
     501,     0,     0,    72,     0,     0,     0,     0,   501,   343,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1304,
       0,   928,  1308,   375,  1232,     0,    75,   376,     0,     0,
       0,  1233,     0,   377,    77,    78,   378,   379,   380,   381,
       0,     0,   797,   948,     0,     0,   950,     0,   952,     0,
       0,  1353,  1355,  1357,   961,     0,   966,   961,     0,     0,
     338,     0,     0,     0,   501,  1891,     0,     0,   501,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1376,     0,     0,   994,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1112,   996,   501,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1005,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   440,     0,   200,   994,     0,     0,     0,     0,
       0,     0,     0,   631,     0,     0,     0,     0,     0,     0,
       0,     0,  1406,     0,     0,     0,     0,     0,   501,   501,
    1415,  1416,  1058,     0,     0,   468,     0,     0,   255,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   501,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1089,     0,     0,     0,     0,     0,     0,     0,     0,
     343,     0,     0,     0,     0,     0,     0,  1456,     0,     0,
       0,     0,     0,     0,     0,     0,  1465,     0,     0,  1469,
       0,  1472,  1473,     0,     0,     0,   675,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   411,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1190,
    1192,   455,   455,     0,     0,     0,     0,   440,     0,     0,
       0,  1499,     0,     0,     0,     0,  1529,     0,     0,  1531,
       0,     0,     0,     0,     0,     0,     0,     0,   468,     0,
       0,     0,     0,     0,     0,     0,  1720,   961,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     994,   255,  1348,     0,     0,     0,     0,     0,  1229,     0,
       0,     0,     0,     0,     0,   961,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1587,     0,
       0,     0,     0,   360,     0,     0,     0,   361,     0,   362,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1114,     0,   363,    -2,     0,  1116,
       0,   468,  1117,  1118,  1119,  1120,  1121,  1122,  1123,  1124,
    1125,  1126,  1127,  1128,  -298,  1129,  1130,  1131,  1132,  1133,
       0,  1134,     0,   364,   365,     0,   462,     0,   367,  1135,
    1136,    64,    65,    66,    67,    68,    69,    70,   368,   369,
     357,  1137,   370,   371,   372,     0,   373,   374,     0,     0,
    1469,   675,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   468,     0,
    1297,     0,  1299,     0,   375,     0,   455,    75,   376,  1646,
       0,     0,   279,     0,   377,    77,    78,   378,   379,   380,
     381,     0,     0,     0,     0,     0,     0,     0,  1680,  -179,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   675,     0,     0,   459,     0,     0,     0,     0,
       0,     0,     0,     0,   242,   243,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,  1364,  1364,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,  -419,  -419,     0,
    -419,    45,    46,     0,  -419,     0,  1722,     0,     0,     0,
     631,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    57,     0,     0,     0,     0,     0,     0,     0,  1403,
       0,     0,     0,     0,     0,  1413,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   440,     0,     0,    62,    63,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   468,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1770,  1771,     0,     0,   961,     0,     0,   797,     0,     0,
       0,     0,  1775,     0,     0,     0,     0,     0,   360,     0,
       0,     0,   361,    75,   362,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   363,     0,     0,     0,  1812,     0,     0,     0,     0,
       0,     0,     0,     0,   631,     0,     0,     0,  1493,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   364,   365,
       0,   366,     0,   367,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   368,   369,   357,     0,   370,   371,   372,
       0,   373,   374,     0,     0,     0,   961,     0,     0,    72,
       0,     0,     0,     0,     0,  1834,     0,     0,     0,     0,
       0,     0,     0,     0,   468,     0,     0,   797,     0,   375,
     957,  1532,    75,   376,     0,  1981,     0,     0,     0,   377,
      77,    78,   378,   379,   380,   381,     0,     0,     0,     0,
       0,  1348,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   948,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1602,  1603,  1889,     0,     0,
       0,     0,   360,     0,     0,     0,   361,     0,   362,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     468,     0,   797,  1114,     0,   363,    -2,     0,  1116,  -238,
    -238,  1117,  1118,  1119,  1120,  1121,  1122,  1123,  1124,  1125,
    1126,  1127,  1128,  -298,  1129,  1130,  1131,  1132,  1133,     0,
    1134,     0,   364,   365,     0,   462,     0,   367,  1135,  1136,
      64,    65,    66,    67,    68,    69,    70,   368,   369,   357,
    1137,   370,   371,   372,     0,   373,   374,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     411,     0,  -238,   375,  1981,  1673,    75,   376,     0,     0,
       0,   279,     0,   377,    77,    78,   378,   379,   380,   381,
    1348,     0,     0,     0,     0,     0,     0,     0,  -179,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   360,     0,     0,     0,   361,     0,   362,     0,     0,
       0,  1713,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1114,     0,   363,    -2,     0,  1116,  -239,  -239,
    1117,  1118,  1119,  1120,  1121,  1122,  1123,  1124,  1125,  1126,
    1127,  1128,  -298,  1129,  1130,  1131,  1132,  1133,     0,  1134,
    1736,   364,   365,  1738,   462,     0,   367,  1135,  1136,    64,
      65,    66,    67,    68,    69,    70,   368,   369,   357,  1137,
     370,   371,   372,     0,   373,   374,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -239,   375,     0,     0,    75,   376,     0,     0,     0,
     279,     0,   377,    77,    78,   378,   379,   380,   381,     0,
       0,     0,     0,     0,     0,     0,     0,  -179,     4,   243,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,  1113,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,     0,     0,   360,     0,    45,    46,   361,     0,   362,
      47,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,    56,     0,  1114,    57,  1115,    -2,     0,  1116,
       0,     0,  1117,  1118,  1119,  1120,  1121,  1122,  1123,  1124,
    1125,  1126,  1127,  1128,  -298,  1129,  1130,  1131,  1132,  1133,
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
    1123,  1124,  1125,  1126,  1127,  1128,  -298,  1129,  1130,  1131,
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
    1843,     0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
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
      -2,  1860,     0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
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
      41,    42,    43,    44,  -419,  -419,     0,  -419,    45,    46,
       0,  -419,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    13,    14,    15,    16,    17,     0,    57,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,     0,    61,
      45,    46,    62,    63,    64,    65,    66,    67,    68,    69,
      70,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      57,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    73,    74,     0,
      75,   244,     0,     0,     0,  -728,     0,     0,    77,    78,
       4,   243,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,     0,     0,     0,     0,    45,    46,     0,
       0,     0,    47,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,    56,     0,     0,    57,     0,     0,
       0,     0,  -351,  -351,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    60,     0,     0,
       0,    62,    63,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -351,     0,     0,     0,    75,
      76,     0,     0,     0,     0,     0,     0,    77,    78,     4,
     243,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,     0,     0,    45,    46,     0,     0,
       0,    47,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,    56,     0,     0,    57,     0,     0,     0,
       0,  -352,  -352,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    60,     0,     0,     0,
      62,    63,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -352,     0,     0,     0,    75,    76,
       0,     0,     0,     0,     0,     0,    77,    78,   242,   243,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,     0,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,  -419,  -419,     0,  -419,    45,    46,     0,  -419,     0,
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
     924,     0,     0,     0,     0,     0,     0,    77,    78,   242,
     243,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,     0,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,  -419,  -419,     0,  -419,    45,    46,     0,  -419,
       0,     0,     0,     0,     0,     0,     0,     0,    13,    14,
      15,    16,    17,     0,     0,    19,    57,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,     0,     0,    45,    46,     0,     0,
      62,    63,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    57,     0,     0,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   656,    75,   244,
       0,     0,     0,     0,     0,     0,    77,    78,   243,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,     0,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
    -419,  -419,     0,  -419,    45,    46,     0,  -419,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    57,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    61,     0,     0,    62,    63,
      64,    65,    66,    67,    68,    69,    70,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    73,    74,     0,    75,   244,     0,     0,
       0,  -732,     0,     0,    77,    78,   243,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,     0,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,  -419,  -419,
       0,  -419,    45,    46,     0,  -419,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    57,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    61,     0,     0,    62,    63,    64,    65,
      66,    67,    68,    69,    70,     0,     0,     0,     0,     0,
       0,     0,  1348,     0,     0,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    73,    74,   360,    75,   244,     0,   361,     0,   362,
       0,     0,    77,    78,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1114,     0,   363,     0,     0,  1116,
    1778,  1779,  1117,  1118,  1119,  1120,  1121,  1122,  1123,  1124,
    1125,  1126,  1127,  1128,  -298,  1129,  1130,  1131,  1132,  1133,
       0,  1134,     0,   364,   365,     0,   462,     0,   367,  1135,
    1136,    64,    65,    66,    67,    68,    69,    70,   368,   369,
     357,  1137,   370,   371,   372,     0,   373,   374,     0,     0,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
    1348,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   375,     0,     0,    75,   376,     0,
       0,     0,   279,     0,   377,    77,    78,   378,   379,   380,
     381,   360,     0,     0,     0,   361,     0,   362,     0,  -179,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1114,     0,   363,     0,     0,  1116,     0,     0,
    1117,  1118,  1119,  1120,  1121,  1122,  1123,  1124,  1125,  1126,
    1127,  1128,  -298,  1129,  1130,  1131,  1132,  1133,     0,  1134,
       0,   364,   365,     0,   462,     0,   367,  1135,  1136,    64,
      65,    66,    67,    68,    69,    70,   368,   369,   357,  1137,
     370,   371,   372,     0,   373,   374,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   375,     0,     0,    75,   376,     0,     0,     0,
     279,     0,   377,    77,    78,   378,   379,   380,   381,     0,
       0,     0,     0,     0,     0,     0,     0,  -179,   243,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
       0,     0,     0,     0,    45,    46,     0,     0,     0,   319,
      48,    49,    50,    51,    52,    53,    54,     0,    13,    14,
      15,    16,    17,    18,    57,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,     0,     0,    45,    46,    62,    63,
       0,    47,    48,    49,    50,    51,    52,    53,    54,     0,
       0,     0,     0,     0,     0,     0,    57,     0,     0,     0,
       0,     0,     0,    72,     0,  1049,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -600,    75,   320,     0,     0,
      62,    63,     0,     0,    77,    78,   243,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    75,     0,
       0,     0,    45,    46,     0,     0,     0,   319,    48,    49,
      50,    51,    52,    53,    54,     0,    13,    14,    15,    16,
      17,    18,    57,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
       0,     0,     0,     0,    45,    46,    62,    63,     0,   319,
      48,    49,    50,    51,    52,    53,    54,     0,     0,     0,
       0,     0,     0,     0,    57,     0,     0,     0,     0,     0,
       0,    72,     0,  1754,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    75,   320,     0,     0,    62,    63,
       0,     0,    77,    78,   243,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    75,     0,     0,     0,
      45,    46,     0,     0,     0,   319,    48,    49,    50,    51,
      52,    53,    54,     0,     0,     0,     0,     0,     0,     0,
      57,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    62,    63,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    72,
       0,  1756,     0,     0,     0,     0,     0,     0,     0,     0,
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
      75,   320,     0,     0,     0,     0,     0,     0,    77,    78,
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
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    75,   301,
       0,     0,     0,     0,     0,     0,    77,    78,   243,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,     0,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
    -419,  -419,     0,  -419,    45,    46,     0,  -419,     0,     0,
       0,     0,     0,     0,     0,     0,    13,    14,    15,    16,
      17,     0,     0,    19,    57,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
    -418,  -418,     0,  -418,    45,    46,     0,  -418,    62,    63,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    57,     0,     0,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    75,   244,     0,     0,
       0,     0,     0,     0,    77,    78,    13,    14,    15,    16,
      17,    18,   662,    19,   663,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
       0,     0,   360,     0,    45,    46,   361,     0,   362,    47,
      48,    49,    50,    51,    52,    53,    54,     0,     0,     0,
       0,     0,     0,     0,    57,   363,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   664,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   364,   365,     0,   366,     0,   367,    62,    63,
      64,    65,    66,    67,    68,    69,    70,   368,   369,   357,
       0,   370,   371,   372,     0,   373,   374,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   375,     0,     0,    75,   665,     0,     0,
       0,   279,     0,   377,    77,    78,   666,   667,   380,   381,
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
       0,     0,     0,     0,     0,     0,     0,   375,     0,   406,
      75,   407,     0,     0,     0,     0,     0,   377,    77,    78,
     378,   379,   380,   381,    13,    14,    15,    16,    17,    18,
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
       0,   375,     0,     0,    75,   665,     0,     0,     0,   279,
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
       0,     0,     0,     0,     0,   375,     0,     0,    75,   407,
       0,     0,     0,     0,     0,   377,    77,    78,   378,   379,
     380,   381,    13,    14,    15,    16,    17,    18,     0,    19,
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
       0,     0,     0,   375,     0,     0,    75,   376,     0,     0,
       0,     0,     0,   377,    77,    78,   378,   379,   380,   381,
      13,    14,    15,    16,    17,    18,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,     0,     0,    45,    46,
       0,     0,     0,    47,    48,    49,    50,    51,    52,    53,
      54,     0,     0,     0,     0,     0,     0,     0,    57,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    61,
       0,     0,    62,    63,    64,    65,    66,    67,    68,    69,
      70,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    73,    74,     0,
      75,    76,     0,     0,     0,  -730,     0,     0,    77,    78,
      13,    14,    15,    16,    17,    18,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,     0,     0,    45,    46,
       0,     0,     0,    47,    48,    49,    50,    51,    52,    53,
      54,     0,     0,     0,     0,     0,     0,     0,    57,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    61,
       0,     0,    62,    63,    64,    65,    66,    67,    68,    69,
      70,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    73,    74,     0,
      75,    76,     0,     0,     0,     0,     0,     0,    77,    78,
      13,    14,    15,    16,    17,    18,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,     0,     0,    45,    46,
       0,     0,     0,    47,    48,    49,    50,    51,    52,    53,
      54,     0,     0,     0,     0,     0,     0,     0,    57,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    61,
       0,     0,    62,    63,    64,    65,    66,    67,    68,    69,
      70,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      75,    76,     0,    13,    14,    15,    16,    17,    77,    78,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,  -419,  -419,     0,
    -419,    45,    46,     0,  -419,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    57,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    61,     0,     0,    62,    63,    64,    65,    66,
      67,    68,    69,    70,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      73,    74,     0,    75,   301,     0,     0,     0,     0,     0,
       0,    77,    78,   556,   243,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     0,    19,
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
       0,     0,    57,     0,     0,     0,     0,     0,  1425,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    75,     0,     0,     0,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    75,   924,     0,     0,     0,     0,
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
       0,     0,     0,     0,    75,   286,     0,     0,    62,    63,
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
       0,   319,    48,    49,    50,    51,    52,    53,    54,     0,
       0,     0,     0,     0,     0,     0,    57,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    75,   433,     0,     0,
      62,    63,     0,     0,    77,    78,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    75,   320,
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
       0,     0,     0,     0,     0,     0,     0,     0,    75,   286,
       0,     0,    62,    63,     0,     0,    77,    78,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      75,   433,     0,     0,     0,     0,     0,     0,    77,    78,
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
      57,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      75,   301,     0,     0,    62,    63,     0,     0,    77,    78,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    75,   924,     0,     0,     0,     0,     0,     0,
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
       0,     0,    75,   924,     0,     0,    62,    63,     0,     0,
      77,    78,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    75,     0,     0,    13,    14,    15,
      16,    17,    77,    78,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,  -419,  -419,     0,  -419,    45,    46,     0,  -419,     0,
       0,     0,     0,     0,     0,     0,     0,    13,    14,    15,
      16,    17,     0,     0,    19,    57,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,  -419,  -419,     0,  -419,    45,    46,     0,  -419,    62,
      63,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    57,     0,     0,     0,     0,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    75,   301,    62,
      63,     0,     0,     0,     0,    77,    78,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    75,     0,     0,
       0,     0,     0,     0,     0,    77,    78,   243,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,     0,
       0,     0,     0,    45,    46,     0,     0,     0,   319,    48,
      49,    50,    51,    52,    53,    54,     0,     0,     0,     0,
       0,     0,     0,    57,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    62,    63,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   850,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -613,    75,   243,     6,     7,     8,
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
       0,     0,     0,  1681,     0,     0,     0,     0,   243,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,    19,    75,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
       0,     0,     0,     0,    45,    46,     0,     0,     0,   319,
      48,    49,    50,    51,    52,    53,    54,     0,     0,     0,
       0,     0,     0,     0,    57,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   243,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    62,    63,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,  -419,  -419,     0,
    -419,    45,    46,     0,  -419,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    75,     0,     0,     0,
       0,    57,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    62,    63,     0,     0,     0,
       0,    13,    14,    15,    16,    17,    18,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,     0,     0,     0,     0,    45,
      46,     0,     0,    75,    47,    48,    49,    50,    51,    52,
      53,    54,     0,     0,     0,     0,     0,     0,     0,    57,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      61,     0,     0,    62,    63,    64,    65,    66,    67,    68,
      69,    70,     0,     0,     0,    13,    14,    15,    16,    17,
       0,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,  -419,
    -419,    75,  -419,    45,    46,     0,  -419,     0,     0,   360,
       0,     0,     0,   361,     0,   362,     0,     0,     0,     0,
       0,     0,     0,    57,     0,     0,     0,     0,     0,     0,
       0,     0,   363,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    62,    63,   364,
     365,     0,   366,     0,   367,    62,    63,    64,    65,    66,
      67,    68,    69,    70,   368,   369,   357,     0,   370,   371,
     372,   360,   373,   374,     0,   361,     0,   362,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   363,    75,     0,     0,     0,     0,
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
     363,     0,     0,     0,     0,     0,   375,  1964,     0,    75,
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
      36,    37,    38,    39,    40,    41,    42,    43,    44,  -419,
    -419,     0,  -419,    45,    46,     0,  -419,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    57
};

static const yytype_int16 yycheck[] =
{
       1,   162,     1,     4,     1,    73,   219,     4,    73,   256,
     283,   173,   131,   691,    73,   241,   614,    73,   375,   177,
    1283,  1284,    95,    75,   220,   408,   178,   207,   873,   676,
    1719,   601,   633,  1350,     1,   735,   323,   465,     1,    58,
     178,     4,   150,     1,   601,     1,   963,     4,   859,    82,
     514,   515,   222,   601,    55,    56,   163,    58,   162,    58,
     669,    58,   764,   843,   765,    73,     1,   764,   630,  1656,
     771,     0,    73,   338,  1206,   992,   219,   342,   139,   762,
    1656,    82,   295,    82,  1656,  1123,   782,   762,   605,    90,
     870,    58,   211,   219,    95,    58,    95,    98,     1,    98,
     296,   102,    58,   102,   800,   102,   366,     1,   525,   219,
     762,     1,   762,   181,   147,   321,    70,  1778,   535,    82,
     290,   291,   181,    58,    70,   181,   145,   607,   236,   151,
     191,   219,   338,   155,  1350,  1351,   342,   219,  1055,   140,
    1657,    73,   143,     1,   145,   102,   145,   220,   145,   150,
     219,   219,   295,    95,   219,   156,    75,    76,   445,   576,
     219,    87,   163,   219,    58,   284,   229,   551,    58,   295,
     866,   635,  1107,   181,   155,    73,   139,     0,   145,   180,
     181,   180,   145,   148,   147,   295,   131,   781,   782,   145,
     155,   254,   762,   174,   195,    73,   195,   155,   156,   153,
      58,   264,   462,   149,   205,   762,   800,   295,   102,   210,
     145,   219,   245,   295,   762,    96,   130,  1107,   219,   220,
     165,   220,   274,   296,  1107,   787,   295,   295,   191,   155,
     295,   157,   174,   439,  1895,   236,   295,   149,   157,   295,
      98,    87,   174,   921,   245,   157,   245,   764,   631,  1766,
     497,   145,    87,   156,   255,   145,   375,   258,   172,   258,
     859,    87,  1579,   375,   265,    73,    70,  1956,   149,   302,
     483,   949,   866,   265,   275,   276,     0,   278,   742,   242,
     573,    89,   245,   181,   276,  1417,   766,   145,   162,   562,
     770,   471,   131,    82,   295,   296,   492,   296,   106,   779,
     780,  1001,   303,   181,  1115,  1782,   593,    59,    60,   310,
     311,   157,     1,   914,   315,     4,   283,   146,  1019,  1099,
     283,   219,   157,  1025,   155,   590,   165,  1914,  1025,   938,
     617,   157,   689,  1144,   165,  1018,   443,   624,  1914,   302,
     483,   219,  1914,  1018,   173,   149,   347,  1864,   283,   153,
     157,   352,  1898,   915,   355,   356,   569,   483,   115,  1097,
     149,   316,   627,  1579,  1102,  1911,  1018,   174,  1018,    58,
     149,  1067,    70,   483,   435,   571,   409,   894,  1641,  1642,
      73,   104,   105,   115,   590,   504,  1321,  1322,  1323,   283,
     509,  1937,   154,   283,   173,   483,   153,    70,  1915,   354,
     258,   483,    95,   149,   512,   157,   155,   526,   614,    98,
     518,   463,   149,   102,   483,   483,   165,   536,   419,   492,
     419,   627,   174,   840,   483,   228,   569,   483,   231,   152,
    1947,  1321,  1322,  1323,   120,  1173,    70,   149,  1321,  1322,
    1323,   442,   443,   569,   828,   157,   409,   321,  1018,  1241,
     253,   149,    70,   454,   455,   153,   145,   150,  1935,   569,
     263,  1018,   463,    70,   465,   323,   152,   157,    73,   573,
    1018,   534,   435,  1067,    19,   483,   149,   149,   363,    82,
     153,   569,   483,    88,   174,  1962,  1702,   569,   781,   782,
     155,   492,    95,   492,  1195,    98,    70,     4,   571,   102,
     569,   569,   886,   388,   389,   149,   155,   800,  1025,   174,
     569,   512,   156,   569,  1552,   149,  1115,   518,  1556,   153,
     104,   105,  1439,   150,   409,   174,   219,   220,    70,   648,
     157,   149,    75,    76,   653,   153,   648,   148,   650,   651,
     659,   653,   149,   236,   155,  1144,   153,   659,    55,    56,
     662,   663,   664,   149,   439,   556,   575,   558,   716,   678,
      70,   153,  1042,  1826,   851,   439,   158,   995,   569,   258,
     571,  1272,   571,   866,   575,   149,   575,   180,   575,   153,
    1318,   125,   126,    90,   585,   483,   151,   445,   589,   149,
     155,  1189,   195,   556,    56,   562,   883,    59,    60,   562,
      62,  1202,   295,   296,  1251,   483,   174,   149,   575,   789,
      70,   153,   575,   997,   157,   149,   881,   220,   151,   575,
     621,    70,   155,   775,   591,   169,   170,   562,  1844,   131,
    1014,  1015,   633,   140,   786,   352,   143,   775,   355,   149,
     575,   934,   245,   153,    70,  1861,    70,  1209,   149,   156,
     151,   155,  1444,  1445,  1446,   258,   163,   764,   160,   161,
     155,  1699,  1340,    70,   822,   717,   560,   155,   562,  1514,
     174,   174,   562,   155,   149,   881,   155,    70,   153,   146,
     681,   575,   683,  1899,   685,   575,   174,   149,   689,   149,
     157,   692,   174,   153,   802,   174,  1258,    70,   152,   573,
     149,   559,   505,   210,   153,   153,   173,  1624,   827,  1626,
     158,     3,   155,   821,  1361,   151,   717,   575,  1182,   838,
     156,    70,   841,   149,   527,   149,   845,   153,   155,   153,
     533,   174,   912,   151,   537,    12,    13,    14,    15,    16,
     614,     3,   149,   155,   162,   163,   153,   174,   255,  1787,
     443,  1350,  1351,   696,   697,   698,   149,  1795,   265,   617,
     153,   762,   174,   764,  1505,  1506,  1507,   146,   275,   276,
     160,   278,   155,   152,  1067,   776,   149,   167,   168,   155,
     153,   157,   783,    12,    13,    14,    15,    16,   789,  1436,
     483,   792,   759,    70,   173,   151,   303,   153,   905,   492,
     801,   802,   803,   310,   311,  1233,   149,  1054,   315,   157,
     695,    12,    13,    14,    15,    16,   419,  1855,   157,   512,
     821,    46,    47,  1206,    49,   518,   151,   151,    53,   129,
     934,   156,   156,   151,   151,   129,   988,   155,   192,   156,
     347,    70,    82,   152,   153,   352,   151,   156,   355,   149,
     155,   173,  1414,   153,   131,   149,   857,   858,   859,   153,
     160,   161,   859,   149,   151,   558,   160,   161,   155,    70,
     103,   151,   152,  1435,   107,   872,   569,   110,   571,   112,
    1264,  1265,  1062,   149,   157,   151,   575,   153,     3,   492,
     129,   149,   859,   171,  1278,  1279,   859,    12,    13,    14,
      15,    16,   131,   859,   905,   129,  1193,   147,   909,   872,
     149,   149,  1064,   914,   153,   872,  1086,  1024,  1025,   920,
     115,   160,   161,  1216,   859,   149,   149,  1311,  1312,   153,
    1358,  1104,  1105,  1106,   173,   442,   160,   161,   151,   179,
       3,   151,    12,    13,    14,    15,    16,   454,   455,    12,
      13,    14,    15,    16,   955,    70,   149,    56,   104,   105,
      59,    60,   963,    62,   151,   859,   151,   151,  1325,   859,
     155,   155,  1198,    12,    13,    14,    15,    16,    17,   151,
    1579,   151,  1101,  1189,    12,    13,    14,    15,    16,    17,
    1283,   992,   149,   851,   995,  1114,   153,   151,   151,  1561,
      70,   859,   155,   155,   862,   245,  1434,    70,  1609,   149,
      21,   151,  1131,   153,   149,   248,   151,  1018,   153,   162,
     163,  1183,  1184,  1024,  1025,   883,     4,     5,     6,     7,
       8,     9,    10,    11,  1417,   149,   143,   144,   145,   842,
     149,   143,   144,   145,   153,   930,   123,   124,   155,   556,
     935,   149,   149,   155,  1055,   153,  1618,   149,   165,   129,
     934,   946,   302,   165,   154,   155,  1313,   174,  1176,   762,
     155,   764,   174,   155,  1107,   155,   149,  1350,   585,   149,
     153,   321,   589,   153,    12,    13,    14,    15,    16,    17,
     160,   161,   325,   326,   149,   328,   151,   330,   153,   155,
     151,   151,  1254,  1702,   155,   155,  1399,    96,  1220,   802,
    1494,  1495,  1216,   151,   621,   149,  1117,   155,  1115,  1120,
    1121,  1122,   157,  1284,    87,   151,   633,   157,   821,   155,
     151,  1809,  1733,   151,   155,  1331,   148,   155,   151,  1291,
    1292,   151,   155,  1144,  1107,   155,  1530,  1144,  1115,  1150,
     157,   151,  1115,  1291,  1292,   155,   154,  1158,   151,  1115,
    1161,  1162,  1161,  1162,  1165,  1162,   520,   151,   173,   409,
     859,   155,  1734,   151,   681,  1176,   683,  1144,   685,   151,
    1115,  1144,   689,   872,   101,   692,   127,   128,  1144,   106,
     107,   108,   109,   110,   111,   112,   174,   157,   151,   439,
     157,  1202,   155,   157,   118,  1162,   120,   121,   122,  1144,
     717,   108,   109,   110,   111,   112,  1217,  1601,   154,   155,
     151,  1115,  1341,  1342,   155,  1115,   115,   151,   160,   161,
    1115,   155,  1233,   150,   149,   149,   153,   591,   152,   153,
    1241,   149,   151,   157,   158,  1844,   155,  1631,   154,   155,
    1144,   149,  1636,  1637,  1144,  1817,   155,  1115,  1331,   166,
    1379,   161,  1861,    12,    13,    14,    15,    16,  1162,   776,
     154,   155,  1273,   159,   514,   515,   783,     4,     5,     6,
       7,     8,     9,    10,    11,   129,  1144,   151,  1321,  1322,
    1323,   155,  1325,  1326,   801,  1399,   803,   151,   531,   171,
    1899,   155,   101,  1161,  1189,  1518,  1579,   106,   107,   108,
     109,   110,   111,   112,  1919,  1189,  1512,   151,  1923,   154,
     155,    70,   154,   155,  1520,  1018,   152,  1324,   155,   156,
    1331,  1024,  1025,   151,  1335,  1193,    63,  1338,  1223,  1224,
    1225,   151,  1216,   154,   155,  1230,  1231,   151,  1641,   151,
     857,   858,   859,  1350,  1351,   155,   156,  1358,  1321,  1322,
    1323,  1324,  1325,  1326,   718,   154,   155,  1324,    12,    13,
      14,    15,    16,    17,   614,  1518,   151,  1378,   154,  1380,
     129,  1380,   153,  1350,  1351,   154,   155,  1350,  1351,   154,
     155,   131,  1518,   154,   155,   635,   154,   155,   905,   131,
     149,   192,   909,   156,   153,   759,   156,   914,  1518,   154,
     155,   160,   161,   154,   155,  1350,  1351,   154,   155,   155,
    1578,   703,   704,   705,   706,  1422,  1115,   781,   782,  1590,
    1518,   154,   155,  1434,   154,   155,  1518,   149,  1439,  1512,
     154,   155,   151,  1444,  1445,  1446,   800,  1520,   151,  1518,
     151,  1570,   151,  1518,   151,  1144,  1350,  1351,  1671,  1422,
    1350,  1351,   154,   155,   151,  1422,   154,   155,   154,   155,
      75,    76,  1161,  1162,   155,   156,  1242,  1243,  1674,   699,
     700,  1642,   149,  1176,   701,   702,  1590,  1525,  1526,   101,
     154,   153,  1350,  1351,   106,   107,   108,   109,   110,   111,
     112,   113,   742,   157,  1616,    12,    13,    14,    15,    16,
     157,  1512,   866,   707,   708,  1183,  1184,  1518,   157,  1520,
     157,    68,  1380,   154,   149,  1399,  1527,   205,  1671,    17,
     321,    76,   154,   324,   155,   173,   149,   157,   174,   157,
    1541,   153,   151,   151,   174,  1671,   157,   338,  1932,  1550,
     154,   342,  1549,   154,    17,   148,  1714,   151,  1161,  1162,
     151,  1671,   151,    70,     4,     5,     6,     7,     8,     9,
      10,    11,   230,   151,   151,   151,   151,   151,  1865,  1775,
    1273,   148,  1579,  1671,   157,   157,  1587,   157,   151,  1671,
      68,   174,  1549,   173,   151,  1801,  1593,   151,   151,   148,
     151,  1674,  1671,   151,   154,   157,   148,   151,  1609,   155,
    1117,   151,  1579,  1120,  1121,  1122,  1579,  1725,   155,   151,
     151,   151,   129,  1624,   151,  1626,  1778,   151,   151,   154,
    1593,   151,   151,   873,   151,  1324,  1593,  1144,  1331,   151,
    1778,   151,   149,  1150,  1579,   151,   153,  1805,   439,   151,
     151,  1158,   151,   160,   161,  1549,  1862,   154,  1165,   151,
     173,  1350,  1351,   148,   151,  1826,   155,   151,   149,   149,
    1671,   101,   149,  1674,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   149,  1685,  1579,    13,   149,  1689,  1579,
    1548,  1380,   149,    72,   155,  1202,   156,    89,   174,  1584,
    1701,  1914,  1775,   156,   174,  1702,  1707,   154,   154,   174,
    1217,   148,   148,  1067,   157,   174,  1590,   155,   174,  1915,
     151,  1579,   154,  1724,  1725,   151,   155,   155,   151,   520,
     155,   148,  1733,  1422,   154,  1702,   151,   151,   148,  1702,
     149,  1893,   149,  1895,   174,    78,   149,   174,   148,   155,
     174,  1947,   174,   828,   412,  1893,  1864,  1895,   174,   149,
     151,  1919,   174,   174,   148,  1923,  1924,  1702,  1801,  1921,
     428,  1914,   174,   431,  1775,   174,   174,  1380,   148,   155,
     154,  1782,   573,  1921,   151,   463,   154,   465,  1914,   154,
     154,  1949,   148,  1945,   157,   156,   151,   156,   118,   590,
     591,   148,   151,   151,  1914,  1806,   151,   151,  1702,   151,
     148,   886,  1702,  1971,   154,   154,   174,  1975,   155,  1512,
     156,   151,   151,   614,   149,  1518,  1914,  1520,  1335,  1862,
     488,  1338,  1914,  1691,   155,   149,   627,   149,  1990,  1997,
     107,   154,  1915,   154,  1702,  1914,  1914,  1844,  1849,  1914,
     154,  1970,  1990,   148,   157,  1914,  1857,   148,  1914,  1548,
    1549,   151,   151,  1864,  1861,  1866,   151,  1986,   151,   151,
     154,  1378,   151,   174,  1947,   174,  1877,  1844,  1879,  1880,
     148,  1844,   174,   149,    88,   154,   151,   151,   148,   148,
    1579,   154,   153,   151,  1861,   151,   151,   151,  1861,   151,
    1901,   151,  1899,   174,  1593,    73,   152,    73,   156,  1844,
     151,   148,   174,  1914,  1915,   151,  1915,   174,     1,   151,
     151,     4,   997,  1808,  1925,   174,  1861,   718,   155,  1283,
    1284,   148,  1899,   156,  1935,   150,  1899,   150,   148,  1014,
    1015,   101,  1182,   149,   155,    73,  1947,   149,  1947,  1189,
    1844,   148,     9,   165,  1844,   174,   165,   107,   154,  1960,
     174,  1962,   107,   151,  1899,   156,   151,  1861,   759,   148,
     148,  1861,    73,   149,   151,    58,   174,  1978,  1671,   151,
     151,  1674,   174,  1984,   174,  1616,  1844,  1340,   377,  1248,
      73,   668,   709,  1994,   710,   713,   711,  1998,  1962,    82,
    1133,   712,  1691,  1861,  1579,  1899,  1144,  2008,  1895,  1899,
    1706,  1911,    95,  1702,  1702,    98,  1790,   101,  1957,   102,
    1527,  1944,   106,   107,   108,   109,   110,   111,   112,    17,
    1810,  1956,  1725,  1571,  1541,  1571,  1924,  1862,  1975,  1861,
    1165,  1899,    48,  1550,   101,   250,  1512,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   139,  1775,  1834,  1326,
     789,  1526,   145,  1158,   147,   149,   150,   150,   151,   876,
     585,    59,    60,    61,    62,   472,   734,   735,  1422,   162,
    1587,  1593,  1775,     0,   920,   734,   744,    -1,   734,   747,
     881,   734,    -1,   884,    -1,    -1,   179,   180,   181,    -1,
      -1,    -1,  1609,    -1,    -1,    -1,    -1,    -1,   191,   192,
      -1,   789,   195,   101,   792,    -1,    -1,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,     3,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   219,   220,    -1,    -1,
      -1,    -1,    -1,   934,    -1,    -1,    -1,    -1,    -1,    -1,
     808,    -1,    -1,   236,   812,  1844,    -1,    -1,   816,    -1,
      -1,    -1,   245,    -1,    -1,   153,     9,    -1,    -1,    -1,
      -1,  1864,  1861,    -1,    -1,   258,  1530,    -1,  1685,    -1,
      -1,    -1,  1689,    -1,    -1,    -1,    -1,    -1,    -1,  1264,
    1265,    -1,    -1,    -1,  1701,    -1,    -1,    -1,    -1,    -1,
    1707,    -1,    -1,  1278,  1279,   288,    17,    -1,    12,    -1,
    1899,   294,   295,   296,    -1,    -1,    -1,  1724,    -1,   302,
      -1,  1914,  1915,    -1,   101,    -1,  1733,   104,   105,   106,
     107,   108,   109,   110,   111,   112,  1311,  1312,   321,   322,
     323,    -1,    -1,    -1,    55,    56,    57,    58,    59,    60,
      61,    62,   129,    -1,  1947,   338,    -1,    -1,   101,   342,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      -1,    -1,   149,   150,  1514,  1782,    -1,   955,    -1,   156,
      -1,    -1,    86,   160,   161,   963,    -1,  1641,  1642,    -1,
      -1,    -1,   375,    -1,    -1,    -1,    -1,   101,    -1,  1806,
     104,   105,   106,   107,   108,   109,   110,   111,   112,    -1,
      98,    -1,    -1,    -1,   992,    -1,    -1,   995,    -1,    -1,
      -1,   109,    -1,    -1,    -1,    -1,   409,    -1,    -1,   412,
      -1,    -1,    -1,    -1,    -1,    -1,   419,    63,    64,    65,
      66,    -1,  1849,  1001,    -1,    -1,    -1,    -1,    -1,    -1,
    1857,    -1,   435,    -1,    -1,    -1,   439,    -1,    -1,  1866,
     443,    -1,   445,   151,    -1,    12,    13,    14,    15,    16,
    1877,    -1,  1879,  1880,    -1,   101,    -1,  1055,   104,   105,
     106,   107,   108,   109,   110,   111,   112,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1901,    -1,    -1,    -1,  1189,    -1,
     483,    -1,    -1,    -1,    -1,    -1,    -1,   195,    -1,   492,
      -1,    -1,    -1,    -1,  1072,    -1,    -1,  1075,  1925,  1494,
    1495,    -1,    -1,    70,    -1,  1216,    -1,   153,  1935,   512,
      -1,   514,   515,    -1,    -1,   518,    -1,   520,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   171,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1960,   101,  1962,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    -1,    -1,    -1,    -1,
     258,  1978,  1826,    -1,    -1,   558,    -1,  1984,    -1,    -1,
      -1,    -1,   129,    -1,    -1,    -1,   569,  1994,   571,    -1,
     573,  1998,   575,    12,    13,    14,    15,    16,    -1,    -1,
     288,  2008,   149,   150,    -1,    -1,   294,   590,   591,    -1,
     593,    -1,    -1,   160,   161,    -1,    -1,    -1,   601,  1117,
      -1,    -1,   605,    -1,    -1,    -1,  1601,    -1,    -1,    -1,
      -1,   614,    -1,    -1,    -1,   323,    -1,    -1,    -1,    -1,
      -1,   624,    -1,    -1,   627,    -1,    -1,    -1,    -1,    -1,
      -1,    70,   635,    -1,    -1,  1233,  1631,    -1,    -1,    -1,
      -1,  1636,  1637,  1241,    -1,   648,    -1,   650,   651,    -1,
     653,    -1,    -1,    -1,    -1,    -1,   659,    -1,    -1,   662,
     663,   664,   101,    -1,    76,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,    -1,    12,    13,    14,    15,
      16,    -1,  1260,     1,    -1,    -1,     4,    -1,  1399,   101,
     129,  1269,   104,   105,   106,   107,   108,   109,   110,   111,
     112,    -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,
     149,   150,    -1,    -1,   153,   718,    -1,    -1,    -1,    -1,
      -1,   160,   161,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   734,   735,    -1,    70,    -1,    -1,   445,   101,   742,
      58,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      -1,    -1,    -1,    -1,    -1,    -1,   759,    -1,    -1,   762,
    1358,   764,   174,    -1,    82,   101,    -1,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,    -1,   781,   782,
      98,    -1,    -1,    -1,   102,    -1,   149,   150,    -1,    -1,
      -1,    -1,    -1,   129,    -1,    -1,    -1,   800,    -1,   802,
      -1,    -1,    -1,    -1,    12,    13,    14,    15,    16,    -1,
      -1,   519,    -1,   149,   150,    -1,    -1,   153,   821,    -1,
      -1,   139,    -1,    -1,   160,   161,    -1,   145,    -1,   147,
      -1,   539,    -1,   151,    -1,    -1,  1434,    -1,    -1,    -1,
      -1,  1439,    -1,   161,   162,   163,  1444,  1445,  1446,    -1,
      -1,   559,    -1,    -1,    -1,    -1,   859,    -1,    -1,    -1,
    1378,   179,    70,   866,    -1,    -1,    -1,    -1,    -1,   872,
     873,    -1,    -1,   191,   192,    -1,    -1,   195,   881,    -1,
     883,    -1,    -1,    -1,    -1,   593,    -1,    -1,    -1,    -1,
      -1,   894,    -1,   101,    -1,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,    -1,    -1,    -1,    -1,   617,
      -1,    -1,    -1,    -1,    -1,    -1,   624,    -1,    -1,    -1,
      -1,   129,    -1,    -1,   242,    -1,    -1,   245,    -1,    -1,
    1508,   934,    -1,    -1,    -1,    -1,    -1,  1932,    -1,    -1,
     258,   149,   150,    -1,    -1,    -1,   375,    -1,    -1,   657,
     658,    70,   160,   161,    -1,    -1,    -1,   275,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   283,    -1,    -1,    -1,    -1,
     288,    -1,    -1,    -1,    -1,    -1,   294,    12,    13,    14,
      15,    16,   101,    -1,   302,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,    -1,    -1,    -1,  1001,    -1,
      -1,    -1,    -1,   321,    -1,   323,   324,    -1,    -1,    70,
     129,    -1,    -1,    -1,    -1,  1018,    -1,    -1,    -1,    -1,
     338,    -1,  1025,  1541,   342,    -1,  1624,    -1,  1626,    -1,
     149,   150,  1550,    -1,   153,    70,    -1,    -1,    -1,    -1,
     101,   160,   161,   104,   105,   106,   107,   108,   109,   110,
     111,   112,    -1,    -1,    -1,    -1,    -1,   375,    -1,    -1,
      -1,    -1,    -1,    -1,  1067,    -1,   101,    -1,   129,  1587,
      -1,   106,   107,   108,   109,   110,   111,   112,  1656,  1657,
      -1,    -1,    -1,    -1,    -1,   514,   515,    -1,   149,   150,
    1801,   409,    -1,    -1,   129,    -1,    -1,    -1,    -1,   160,
     161,    -1,    -1,    -1,  1107,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1115,    -1,   149,   150,    -1,   435,    -1,    -1,
      -1,   439,    -1,    -1,    -1,   160,   161,   445,   101,    -1,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      -1,  1144,    -1,   851,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1862,    -1,    -1,   862,    -1,    -1,    -1,  1161,  1162,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1685,    70,    -1,
      -1,  1689,    -1,  1176,    -1,   883,    -1,    -1,    -1,  1182,
      -1,    -1,    -1,  1701,    -1,    -1,  1189,   160,  1766,  1707,
      -1,    -1,    -1,    -1,    -1,    -1,   514,   515,    -1,   101,
      -1,   519,   520,    -1,   106,   107,   108,   109,   110,   111,
     112,    -1,    -1,  1216,    -1,    -1,    -1,  1220,    -1,   648,
      -1,    -1,    -1,    -1,   653,    -1,    -1,   129,    -1,    -1,
     659,    -1,    -1,   551,    -1,    -1,    -1,    -1,   556,    -1,
      -1,   559,   560,    -1,   562,    -1,    -1,   149,   150,   678,
      -1,    -1,  1830,    -1,    -1,   573,  1834,   575,   160,   161,
      -1,    -1,    -1,    -1,  1782,    -1,    -1,    -1,    -1,    -1,
      -1,   589,   590,   591,    -1,   593,    -1,    -1,    -1,    -1,
    1283,  1284,    -1,    -1,    -1,   714,  1864,    98,  1806,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   614,    -1,   109,   617,
     111,    -1,   113,   621,    -1,    -1,   624,    -1,    -1,   627,
      -1,   629,    -1,    -1,    -1,    -1,    -1,   635,  1321,  1322,
    1323,  1324,  1325,  1326,    -1,    -1,    -1,    -1,  1331,  1332,
     648,  1849,   650,   651,    -1,   653,  1914,  1915,    -1,  1857,
     151,   659,   153,   154,   662,   663,   664,  1350,  1351,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1877,
      -1,  1879,  1880,    -1,    -1,    -1,    -1,    -1,    -1,  1947,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1380,    -1,    -1,
      -1,    -1,   101,  1901,   195,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,    -1,  1399,    -1,    -1,    -1,
     718,    -1,    -1,    -1,    -1,   101,    -1,  1925,   104,   105,
     106,   107,   108,   109,   110,   111,   112,  1935,    -1,  1422,
     101,    -1,    -1,    -1,   742,   106,   107,   108,   109,   110,
     111,   112,    12,    13,    14,    15,    16,    -1,    -1,    -1,
      -1,   759,  1960,    -1,  1962,    -1,    -1,   258,   129,   260,
     261,    -1,    -1,  1161,    -1,   174,   152,    -1,    -1,    -1,
    1978,   157,    -1,   781,   782,    -1,  1984,    -1,   149,   150,
      -1,    -1,   153,    -1,    -1,    -1,  1994,   288,    -1,   160,
     161,    -1,   800,   294,    -1,  1193,    -1,    -1,    -1,    -1,
      70,   101,    -1,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,    -1,    -1,  1508,    -1,    -1,    -1,  1512,
     828,  1514,   323,    -1,    -1,  1518,    -1,  1520,   329,   129,
     331,   101,    -1,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   851,    -1,    -1,    -1,    -1,    -1,   149,
     150,   859,    -1,   153,   862,  1548,  1549,    -1,   866,   129,
     160,   161,    -1,    -1,   872,   873,    -1,    -1,    -1,    -1,
      -1,    70,    -1,   881,    -1,   883,   884,    -1,   886,   149,
     150,    -1,    -1,    -1,    -1,    -1,  1579,    -1,    -1,    -1,
     160,   161,    -1,    -1,    -1,    -1,    -1,  1590,    -1,    -1,
    1593,    -1,   101,    -1,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,    -1,    -1,    -1,   419,    -1,
      -1,    -1,    -1,  1616,    -1,    -1,   934,    -1,    -1,    -1,
     129,    -1,    -1,    -1,  1332,    70,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   445,    -1,   447,   448,  1641,  1642,
     149,   150,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   160,   161,  1656,  1657,    -1,   101,    -1,    -1,   104,
     105,   106,   107,   108,   109,   110,   111,   112,  1671,    -1,
      -1,  1674,  1380,    -1,    -1,    -1,    -1,    -1,    -1,   997,
      -1,   492,    -1,    -1,   129,    -1,    -1,    -1,  1691,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1014,  1015,    -1,  1702,
      -1,   512,    -1,    -1,   149,   150,   517,    -1,   519,    -1,
      -1,    -1,    -1,    -1,    -1,   160,   161,    -1,    -1,    -1,
      -1,    -1,  1725,    -1,    -1,    -1,    -1,    -1,   539,    -1,
     541,   542,   101,    -1,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,    -1,    -1,   101,   559,  1067,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   101,
     571,    -1,    -1,  1766,   106,   107,   108,   109,   110,   111,
     112,   113,  1775,    -1,    -1,   117,    -1,   119,    -1,    -1,
      -1,    -1,   593,    -1,   595,   596,    -1,    -1,   157,  1107,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1115,  1801,    -1,
      -1,    -1,    -1,   157,    -1,    -1,   617,   618,   150,    -1,
      -1,   153,    -1,   624,    -1,    -1,    -1,    -1,    -1,     1,
      -1,    -1,     4,  1826,    -1,    -1,  1144,    -1,    -1,    -1,
      -1,  1834,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1548,  1844,    -1,  1161,  1162,   101,   657,   658,   104,   105,
     106,   107,   108,   109,   110,   111,   112,    -1,  1861,  1862,
      -1,  1864,  1865,    -1,  1182,    -1,    -1,    -1,    -1,    -1,
      -1,  1189,    -1,   129,   101,  1193,    58,    -1,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,  1205,    -1,    -1,
     117,    -1,   119,   149,   150,    -1,  1899,   153,  1216,    -1,
      82,    -1,  1220,    -1,   160,   161,    -1,    -1,    -1,    -1,
      -1,  1914,  1915,    -1,    -1,    -1,    -1,   173,  1347,    -1,
     102,  1350,  1351,   150,    -1,    -1,   153,  1356,    -1,    -1,
      -1,  1360,   101,  1362,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,  1947,     1,  1264,  1265,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   139,    -1,    -1,
    1278,  1279,    -1,   145,   101,  1283,  1284,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    -1,    -1,    -1,   101,
     162,    -1,    -1,  1691,   106,   107,   108,   109,   110,   111,
     112,   113,   129,  1311,  1312,   117,    -1,   119,   180,    -1,
      -1,    -1,    58,  1321,  1322,  1323,  1324,  1325,  1326,   191,
     192,    -1,   149,   150,  1332,    -1,    -1,    -1,    -1,   156,
      -1,    -1,    -1,   160,   161,    -1,    -1,    -1,   150,    -1,
      -1,   153,  1350,  1351,    -1,    -1,    -1,    -1,   220,    -1,
     851,    -1,    -1,    -1,    -1,    -1,   102,    -1,    -1,    -1,
      -1,   862,    -1,    -1,   236,    -1,    -1,    -1,    -1,   241,
     242,    -1,  1380,   245,    -1,    -1,    -1,    -1,   101,    -1,
      -1,  1500,   883,   106,   107,   108,   109,   110,   111,   112,
     113,  1399,    -1,   894,   117,   267,   119,    -1,   270,   145,
     272,    -1,   903,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   283,    -1,  1532,  1422,    -1,   162,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   296,  1544,    -1,   150,    -1,    -1,
     101,    -1,  1551,   104,   105,   106,   107,   108,   109,   110,
     111,   112,    -1,    -1,    -1,    -1,   192,    -1,    -1,   321,
      -1,    -1,   324,    -1,    -1,    -1,    -1,    -1,   129,    -1,
    1579,    -1,    -1,    -1,    -1,    -1,   338,  1865,    -1,    -1,
     342,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,   150,
      -1,    -1,    -1,    -1,    -1,    -1,  1494,  1495,    -1,   160,
     161,    -1,    -1,    -1,   366,    -1,    -1,    -1,    -1,    -1,
    1001,    -1,    -1,    -1,    -1,   101,  1514,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,    -1,    -1,    -1,
      -1,   267,  1530,    -1,  1025,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   129,    -1,    -1,    -1,   283,    -1,   101,
    1548,  1549,   104,   105,   106,   107,   108,   109,   110,   111,
     112,    -1,    -1,   149,   150,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   435,   160,   161,    -1,   439,  1687,    -1,
      -1,  1579,    -1,    -1,    -1,   321,    -1,    -1,   324,    -1,
    1699,  1700,  1590,  1702,    -1,  1593,    -1,    -1,   150,    -1,
     462,   153,   338,  1601,    -1,    -1,   342,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    99,    -1,   101,  1616,   103,
     104,   105,   106,   107,   108,   109,   110,   111,   112,    -1,
      -1,    -1,    -1,  1631,    -1,    -1,    -1,    -1,  1636,  1637,
      -1,    -1,    -1,  1641,  1642,   101,    -1,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,    -1,   520,    -1,
      -1,    -1,    -1,    -1,    -1,   149,    -1,    -1,   152,   153,
    1161,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1789,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   551,
      -1,    -1,    -1,  1691,   556,    -1,    -1,   153,   560,    -1,
     562,    -1,  1193,   439,  1702,    -1,    -1,    -1,  1199,    -1,
      -1,   573,   101,   575,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,    -1,    -1,   101,   590,   591,
     104,   105,   106,   107,   108,   109,   110,   111,   112,    -1,
      -1,    -1,    -1,   605,    -1,  1854,    -1,  1856,    -1,    -1,
      -1,    -1,   614,    -1,    -1,    -1,    -1,   619,    -1,    -1,
      -1,    -1,    -1,   152,   101,   627,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    -1,    -1,   152,    -1,
      -1,    -1,   101,    -1,   520,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1801,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   149,    -1,    -1,   551,    -1,    -1,    -1,  1928,
      -1,    -1,    -1,    -1,   560,    -1,   562,    -1,  1826,    -1,
     149,  1940,  1941,  1942,    -1,    -1,    -1,   573,    -1,   575,
      -1,  1332,    -1,    -1,    -1,    -1,  1844,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   590,   591,   718,    -1,    -1,    -1,
      -1,    -1,    -1,  1861,  1862,    -1,    -1,  1865,    -1,    -1,
      -1,    -1,    -1,   735,    -1,    -1,    -1,    -1,   614,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1380,
      -1,   627,    -1,    -1,    -1,    -1,    -1,   759,    -1,    -1,
      -1,  1899,   764,   132,   133,   134,   135,   136,   137,   138,
     139,   140,   141,   142,    -1,    -1,    -1,   146,    -1,   781,
     782,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1932,    -1,    -1,    -1,   800,    -1,
      -1,    -1,    -1,    -1,   173,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   828,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   718,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   859,    -1,    -1,
      -1,    -1,    -1,    -1,   866,     1,    -1,    -1,    -1,    -1,
     872,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   881,
      -1,    -1,   884,   759,   886,    -1,    -1,    -1,    -1,   891,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   781,   782,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1548,    -1,    -1,
      -1,    -1,    58,    -1,   800,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   934,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   828,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     1,    -1,   102,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   859,    -1,    -1,    -1,    -1,    -1,    -1,
     866,    -1,    -1,    -1,    -1,   997,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   881,    -1,    -1,   884,   145,
     886,    48,  1014,  1015,    -1,    52,    -1,    54,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   162,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1674,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   192,    -1,   934,    -1,
    1691,    98,    99,    -1,   101,  1067,   103,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,    -1,
     117,   118,   119,    -1,   121,   122,    -1,    -1,    -1,    -1,
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1107,    -1,    -1,    -1,    -1,
      -1,    -1,   149,  1115,    -1,   152,   153,    -1,    -1,    -1,
      -1,   997,   159,   160,   161,   162,   163,   164,   165,    -1,
      -1,    -1,    -1,    -1,    -1,  1766,    -1,    -1,  1014,  1015,
      -1,    -1,  1144,    -1,    -1,    -1,    -1,   283,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1162,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   321,    -1,  1189,   324,    -1,
      -1,  1067,    -1,    -1,    -1,    -1,  1198,    -1,    -1,    -1,
      -1,    -1,   338,    -1,    -1,    -1,   342,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1216,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     1,    -1,    -1,     4,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1865,    -1,    -1,    -1,    -1,  1115,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     1,    -1,    -1,     4,    -1,    -1,
      -1,    -1,  1264,  1265,    -1,    -1,    -1,    -1,  1144,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1278,  1279,    -1,    -1,
      58,  1283,  1284,    -1,    -1,    -1,  1162,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   439,    82,    -1,    -1,    -1,    -1,  1311,
    1312,    58,    -1,  1189,    -1,    -1,  1947,    -1,    -1,  1321,
    1322,  1323,  1324,    -1,   102,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,
    1216,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1350,  1351,
      -1,    -1,    -1,    -1,    -1,   102,    -1,    -1,    -1,    -1,
      -1,   139,    -1,    -1,    -1,    -1,    -1,   145,    -1,   147,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   520,    -1,    -1,    -1,  1264,  1265,
      -1,    -1,   139,    -1,    -1,    -1,    -1,  1399,   145,    -1,
     147,   179,  1278,  1279,    -1,    -1,    -1,  1283,  1284,    -1,
      -1,    -1,    -1,   191,    -1,   551,    -1,    -1,    -1,    -1,
    1422,    -1,    -1,    -1,   560,    -1,   562,    -1,    -1,    -1,
      -1,    -1,   179,    -1,    -1,  1311,  1312,   573,    -1,   575,
      -1,    -1,    -1,    -1,   191,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   590,   591,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   242,    -1,    -1,   245,    -1,    -1,
      -1,    -1,   250,    -1,  1350,  1351,    -1,    -1,   614,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   627,  1494,  1495,    -1,   242,    -1,    -1,   245,    -1,
      -1,    -1,    -1,   250,    -1,   283,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1520,    -1,
      -1,    -1,    -1,  1399,   302,    -1,    -1,    -1,  1530,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   283,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1549,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   302,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1579,    -1,    -1,
      -1,    -1,   718,    -1,    -1,    -1,    -1,    -1,  1590,    -1,
      -1,  1593,    -1,    -1,    -1,     1,    -1,   375,     4,  1601,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1494,  1495,
      -1,    -1,    -1,   759,    -1,    -1,    -1,    -1,   375,  1631,
      -1,   409,    -1,    -1,  1636,  1637,    -1,    -1,    -1,  1641,
    1642,    -1,    -1,    -1,    -1,   781,   782,    -1,    -1,    -1,
      -1,    -1,    58,    -1,  1530,  1657,    -1,   435,    -1,    -1,
      -1,    -1,   409,    -1,   800,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1549,    -1,    -1,    82,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   435,    -1,
      -1,    -1,   828,    -1,    -1,    -1,   102,    -1,    -1,    -1,
    1702,    -1,    -1,  1579,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1590,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   859,    -1,  1601,    -1,    -1,    -1,    -1,
     866,    -1,    -1,   139,    -1,    -1,   514,   515,    -1,   145,
      -1,   147,    -1,    -1,    -1,   881,    -1,    -1,   884,    -1,
     886,    -1,    -1,    -1,    -1,  1631,    -1,    -1,    -1,    -1,
    1636,  1637,    -1,    -1,    -1,  1641,  1642,   514,   515,    -1,
      -1,    -1,    -1,   179,    -1,    -1,    -1,    -1,   556,    -1,
      -1,    -1,   560,    -1,   562,   191,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   575,   934,  1801,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   556,
      -1,    -1,    -1,   560,    -1,   562,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1826,    -1,  1702,    -1,   575,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   242,    -1,    -1,   245,
      -1,    -1,  1844,    -1,   250,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   635,    -1,  1861,
    1862,   997,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     648,    -1,   650,   651,    -1,   653,    -1,   283,  1014,  1015,
      -1,   659,    -1,    -1,   662,   663,   664,    -1,   635,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   302,  1899,    -1,    -1,
      -1,   648,    -1,   650,   651,    -1,   653,    -1,    -1,    -1,
      -1,    -1,   659,  1915,    -1,   662,   663,   664,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1801,    -1,    -1,    -1,    -1,
    1932,  1067,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1826,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   742,    -1,    -1,    -1,  1844,   375,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1115,
      -1,    -1,    -1,    -1,    -1,  1861,  1862,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   742,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   409,    -1,    -1,    -1,    -1,  1144,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1899,    -1,    -1,  1162,    -1,    -1,   435,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1189,    -1,    -1,  1932,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1216,   859,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   872,   873,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   514,   515,
      -1,    -1,   859,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   872,   873,    -1,  1264,  1265,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1278,  1279,    -1,    -1,    -1,  1283,  1284,    -1,
     556,    -1,    -1,    -1,   560,    -1,   562,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   575,
      -1,    -1,    -1,    -1,    -1,  1311,  1312,    -1,    12,    13,
      14,    15,    16,    17,    -1,    19,    -1,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    -1,    -1,  1350,  1351,    50,    51,    -1,    -1,
      -1,    55,    56,    57,    58,    59,    60,    61,    62,   635,
      -1,    -1,    -1,    -1,    -1,     0,    70,    -1,     3,    -1,
      -1,    -1,   648,    -1,   650,   651,    -1,   653,    -1,    -1,
      -1,    -1,    -1,   659,    -1,    -1,   662,   663,   664,    -1,
      -1,    -1,    -1,  1399,    -1,    -1,    -1,    -1,    -1,    -1,
     104,   105,    86,    -1,    -1,    -1,    -1,    -1,    92,    93,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    76,   126,    -1,    -1,    -1,    -1,   151,   152,   153,
      -1,    -1,    -1,    -1,    -1,    -1,   160,   161,    -1,  1107,
      -1,    -1,    -1,    -1,    -1,    -1,   742,  1115,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1494,  1495,
    1107,    -1,    -1,    -1,    -1,    -1,  1144,    -1,  1115,    -1,
     135,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1162,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1530,    -1,    -1,  1144,    -1,    -1,
      -1,    -1,    -1,    -1,  1182,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1549,    -1,  1162,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    82,    -1,    -1,  1182,    -1,    -1,    -1,    -1,
      -1,    -1,  1220,  1579,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   859,  1590,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   229,  1601,   872,   873,    -1,    -1,
      -1,    -1,    -1,  1220,    -1,    -1,    -1,    -1,   292,   244,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   254,
      -1,    -1,    -1,    -1,    -1,  1631,    -1,   147,    -1,   264,
    1636,  1637,    -1,    -1,    -1,  1641,  1642,    -1,    -1,    -1,
      -1,    -1,   162,   278,   279,    -1,    -1,    -1,    -1,    -1,
     285,   286,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   179,
      -1,    -1,    -1,    -1,    -1,    -1,   301,    -1,    -1,    -1,
      -1,    -1,   192,  1321,  1322,  1323,  1324,  1325,  1326,    -1,
      -1,    -1,    -1,    -1,    -1,   320,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1702,    -1,    -1,    -1,
      -1,    -1,  1350,  1351,  1321,  1322,  1323,  1324,  1325,  1326,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   245,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1350,  1351,    -1,    -1,    -1,    -1,    -1,
      -1,   376,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   438,    -1,   440,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   449,   450,    -1,    -1,    -1,
      -1,    -1,   407,    -1,  1422,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   302,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1801,    -1,    -1,   433,    -1,
      -1,   321,   437,    -1,    -1,  1422,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1826,   456,    -1,    -1,    -1,   460,   461,    -1,    -1,   464,
      -1,  1107,    -1,    -1,    -1,    -1,    -1,    -1,  1844,  1115,
      -1,    -1,    -1,    -1,   479,   480,   481,   482,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1861,  1862,    -1,    -1,    -1,
      -1,    -1,    -1,   498,    -1,    -1,  1514,    -1,  1144,    -1,
      -1,   506,    -1,   557,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1162,    -1,    -1,   409,
      -1,    -1,    -1,  1899,    -1,    -1,    -1,  1514,    -1,   534,
      -1,  1549,    -1,    -1,    -1,    -1,  1182,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   439,
      -1,    -1,    -1,    -1,    -1,    -1,  1932,    -1,    -1,    -1,
     565,  1579,  1549,    -1,    -1,    -1,    -1,   572,    -1,    -1,
      -1,    -1,    -1,   578,  1220,  1593,    -1,    -1,    -1,    -1,
      -1,    48,    -1,    -1,    -1,    52,    47,    54,    -1,    -1,
      -1,    -1,  1579,    -1,    -1,    -1,   601,   602,  1616,    -1,
      -1,    -1,    -1,    -1,    71,    -1,  1593,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   514,   515,    -1,    -1,    -1,  1616,
     520,    98,    99,    -1,   101,    -1,   103,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,    -1,
     117,   118,   119,    -1,   121,   122,    -1,    -1,   119,    -1,
     665,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   132,    -1,   134,    -1,  1321,  1322,  1323,  1324,  1325,
    1326,    -1,   149,   573,  1702,   152,   153,    -1,    -1,    -1,
      -1,    -1,   159,   160,   161,   162,   163,   164,   165,    -1,
      -1,   591,    -1,   164,  1350,  1351,   173,   174,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1702,    -1,    -1,    -1,    -1,
     181,   775,    -1,    -1,   614,    -1,    -1,    -1,    -1,   734,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   749,   635,    -1,    -1,   753,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   762,   219,    -1,
      -1,    -1,   223,    -1,    -1,   226,   227,    -1,    -1,   230,
      -1,    -1,   233,   234,    -1,    -1,  1422,    -1,    -1,   784,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   793,    -1,
      -1,    -1,    -1,    -1,   799,    -1,    -1,    -1,   852,   853,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   863,
     864,   865,    -1,    -1,   868,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1844,    -1,   718,    -1,
      -1,   836,    -1,    -1,   295,    -1,    -1,   298,   843,    -1,
      -1,    -1,    -1,  1861,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   742,    -1,    -1,    -1,    -1,  1844,   319,    -1,
      -1,    -1,    -1,    -1,    -1,   870,    -1,    -1,  1514,   759,
      -1,    -1,    -1,   334,  1861,    -1,    -1,    -1,    -1,    -1,
      -1,  1899,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   781,   782,    -1,   948,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1549,    -1,    -1,    -1,    -1,    -1,    -1,
     800,    -1,  1899,    -1,    -1,    -1,    -1,    -1,    -1,   924,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1579,    -1,    -1,    -1,    -1,    -1,    -1,
     994,    -1,    -1,    -1,    -1,    -1,    -1,  1593,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   428,   178,    -1,
    1616,    -1,    -1,    -1,    -1,    -1,   866,    -1,    -1,    -1,
      -1,    -1,    -1,   873,    -1,    -1,    -1,  1041,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1050,  1051,  1052,  1053,
      -1,    -1,  1007,    -1,  1058,  1059,  1011,    -1,    -1,    -1,
      -1,    -1,    -1,  1018,  1068,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   483,  1028,    -1,    -1,    -1,    -1,    -1,    -1,
    1035,    -1,    -1,    -1,   495,  1089,    -1,  1091,    -1,  1044,
      -1,  1046,    -1,    -1,   934,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1702,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1078,    -1,    -1,    -1,  1082,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1144,  1096,    -1,    -1,  1099,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   569,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1170,    -1,    -1,    -1,
      -1,    -1,    -1,  1177,    -1,  1179,  1180,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1188,    -1,  1190,    -1,  1192,    -1,
    1194,    -1,    -1,    -1,    -1,  1199,    -1,    -1,   609,   610,
     360,    -1,    -1,   363,   364,    -1,    -1,    -1,    -1,    -1,
      -1,   622,    -1,   373,   374,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1067,   388,   389,
      -1,    -1,  1187,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1844,   409,
      -1,    -1,    -1,    -1,    -1,  1259,  1211,    -1,    -1,    -1,
      -1,    -1,  1266,  1267,    -1,  1861,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   439,
      -1,    -1,    -1,    -1,    -1,    -1,  1290,    -1,    -1,    -1,
      -1,    -1,    -1,  1297,    -1,    -1,  1300,    -1,    -1,    -1,
      -1,    -1,    -1,  1899,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1330,   738,   739,    -1,
      -1,    -1,    -1,   744,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1182,    -1,    -1,    -1,    -1,    -1,  1303,  1189,
      -1,    -1,  1307,    -1,   765,    -1,    -1,   768,   769,    -1,
     771,    -1,   773,   774,    -1,    -1,  1370,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1216,    -1,    -1,    -1,
      -1,    -1,  1337,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1403,
      -1,   812,    -1,    -1,    -1,   816,    -1,  1411,    -1,  1413,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1387,    -1,    -1,  1390,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1283,  1284,    -1,    -1,    -1,    -1,    -1,
    1405,    -1,    -1,    -1,    -1,  1459,  1460,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1474,  1475,    -1,  1477,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   892,  1486,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1496,  1497,    -1,    -1,    -1,    -1,    -1,    -1,
    1455,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1464,
      -1,    -1,    -1,  1468,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1482,  1483,    -1,
      -1,    -1,    -1,    -1,    -1,   695,   696,   697,   698,   699,
     700,   701,   702,   703,   704,   705,   706,   707,   708,   709,
     710,   711,   712,   713,    -1,    -1,    -1,    -1,    -1,  1399,
      -1,    -1,    -1,    -1,    -1,    -1,   162,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   191,   192,    -1,  1602,  1603,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1612,    -1,
      -1,    -1,    -1,  1024,    -1,   775,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   223,    -1,    -1,
      -1,    -1,    -1,    -1,   230,    -1,    -1,    -1,    -1,  1594,
    1595,    -1,    -1,  1647,  1648,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,    -1,    -1,
      -1,  1072,    -1,    -1,  1075,    -1,    12,    13,    14,    15,
      16,    -1,    -1,    19,  1514,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,   298,    49,    50,    51,    -1,    53,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1727,    70,   321,   322,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1745,    -1,    -1,  1748,  1749,   342,    -1,    -1,    -1,
    1590,  1755,    -1,    -1,    -1,    -1,    -1,  1712,   104,   105,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     930,    -1,    -1,    -1,    -1,   935,    -1,    -1,    -1,    -1,
    1735,    -1,    -1,   129,  1195,    -1,   946,    -1,    -1,    -1,
      -1,    -1,  1203,  1204,    -1,    -1,    -1,    -1,  1753,    -1,
      -1,  1641,  1642,    -1,    -1,    -1,   152,   153,    -1,    -1,
      -1,    -1,    -1,    -1,   160,   161,   412,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1780,    -1,    -1,   988,    -1,
      -1,    -1,   428,   429,    -1,   431,   432,    -1,    -1,    -1,
      -1,    -1,  1797,   439,    -1,  1800,    -1,   443,    -1,  1260,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1269,  1863,
      -1,  1272,    82,  1274,  1275,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    98,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   484,    -1,
      -1,    -1,   488,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1314,    -1,    -1,    -1,    -1,  1912,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   520,    -1,    -1,   147,    -1,    -1,
      -1,   151,  1936,  1888,    -1,    -1,    -1,    -1,    -1,  1943,
      -1,    -1,   162,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1958,  1115,    -1,    -1,    -1,   179,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1381,    -1,   192,    -1,   570,   195,    -1,   573,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1826,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   590,   591,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   601,    -1,    -1,    -1,   605,
      -1,    -1,    -1,    -1,    -1,    -1,   612,    -1,   614,    -1,
      -1,    -1,    -1,    -1,    -1,   245,    -1,    -1,    -1,  1189,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   258,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1463,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1223,  1224,  1225,    -1,    -1,    -1,    -1,
    1230,  1231,    -1,    -1,   294,    -1,    -1,    -1,    -1,    -1,
      -1,  1492,   302,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1254,    -1,    -1,    -1,    -1,    -1,
      -1,   321,    -1,   323,    -1,    -1,    -1,  1518,    -1,    -1,
      -1,    -1,    -1,  1524,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   718,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1291,  1292,    -1,    -1,    -1,    -1,    -1,   734,   735,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   744,   745,
      -1,   747,   748,    -1,    -1,   375,    55,    56,    -1,    -1,
      -1,    -1,    -1,   759,    47,    -1,   762,    -1,   764,   765,
      -1,    -1,    -1,    -1,    -1,   771,    -1,    -1,  1589,    -1,
      -1,    -1,    -1,    -1,    -1,   781,   782,    -1,    -1,   409,
      -1,    90,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   800,    -1,    -1,    -1,   804,    -1,
      -1,    -1,   808,    -1,    -1,    -1,   812,   813,    -1,   439,
     816,   817,    -1,    -1,    -1,   445,    -1,    -1,   824,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   119,    -1,    -1,    -1,
      -1,   140,    -1,    -1,   143,    -1,    -1,    -1,    -1,   132,
      -1,   134,  1663,  1664,    -1,    -1,    -1,   156,    -1,    -1,
    1671,    -1,    -1,    -1,  1675,    -1,    -1,    -1,    -1,    -1,
     866,   867,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   514,   515,    -1,    -1,   894,    -1,
     520,    -1,    -1,    12,    13,    14,    15,    16,    -1,    -1,
      19,   210,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    -1,   934,    -1,
      -1,    50,    51,   226,   227,    -1,    -1,   230,    -1,    -1,
     233,   234,    -1,   573,    -1,    -1,   255,  1768,    -1,    -1,
      -1,    70,    -1,    -1,    -1,    -1,   265,    -1,    -1,    -1,
      -1,   591,    -1,   593,    -1,    -1,    -1,   276,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   101,    -1,   614,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   303,  1001,    -1,    -1,    -1,    -1,
      -1,   310,   311,    -1,    -1,   635,   315,    -1,    -1,  1830,
      -1,    -1,  1018,  1019,  1584,    -1,    -1,    -1,   648,  1025,
     650,   651,    -1,   653,    -1,    -1,   319,    -1,    -1,   659,
      -1,    -1,   662,   663,   664,    -1,    -1,   156,   347,    -1,
      -1,   334,    -1,   352,    -1,    -1,   355,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1067,    -1,    -1,    -1,    -1,  1072,  1073,    -1,  1075,
    1076,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   718,    -1,
      -1,    -1,    -1,  1914,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   742,     5,    -1,    -1,    -1,    -1,    -1,    -1,
      12,    13,    14,    15,    16,    -1,    -1,    -1,    -1,   759,
      -1,    -1,    -1,   442,    -1,   428,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   454,   455,    -1,    -1,    -1,
      -1,   781,   782,    -1,    -1,    -1,    48,    -1,    -1,    -1,
      52,    -1,    54,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     800,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,
      -1,    -1,    -1,  1189,    -1,    -1,    -1,    -1,    -1,  1195,
    1196,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   495,    -1,    -1,    -1,    98,    99,  1778,   101,
    1216,   103,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,    -1,   117,   118,   119,    -1,   121,
     122,    -1,    -1,    -1,    -1,    -1,   866,   129,  1808,    -1,
      -1,    -1,    -1,   873,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   883,  1260,  1261,    -1,   149,    -1,    -1,
     152,   153,    -1,  1269,  1270,    -1,  1272,   159,   160,   161,
     162,   163,   164,   165,    -1,    -1,   585,  1283,  1284,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    -1,    -1,
      -1,    52,    -1,    54,   934,   179,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   609,   610,   192,    -1,
      71,    -1,    -1,  1893,   633,  1895,    -1,    -1,    -1,   622,
      -1,   205,    -1,   207,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    98,    99,    -1,
     101,  1921,   103,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,   117,   118,   119,    -1,
     121,   122,    -1,    -1,    -1,  1945,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1399,    -1,    -1,    -1,    -1,   149,   150,
      -1,   152,   153,    -1,    -1,    -1,   157,    -1,   159,   160,
     161,   162,   163,   164,   165,    -1,    -1,    -1,    -1,   293,
    1990,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1067,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   738,   739,    -1,    -1,    -1,
      -1,   744,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   776,    -1,    -1,
      -1,    -1,   765,    -1,   783,   768,   769,  1107,   771,    -1,
     773,   774,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1508,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     5,  1524,   812,
      -1,    -1,    -1,   816,    12,    13,    14,    15,    16,    -1,
      -1,  1161,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   858,
      -1,    -1,  1182,    -1,    -1,    -1,    -1,    -1,    -1,  1189,
      48,    -1,    -1,    -1,    52,    -1,    54,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    70,    71,  1590,    -1,  1216,    -1,    -1,    -1,
    1220,   465,    -1,    -1,    -1,    -1,    -1,   471,    -1,   892,
     909,    -1,   476,    -1,    -1,   914,    -1,    -1,    -1,    -1,
      98,    99,    -1,   101,    -1,   103,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,    -1,   117,
     118,   119,    -1,   121,   122,  1641,  1642,    -1,    -1,    -1,
      -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1656,  1657,    -1,  1283,  1284,    -1,    -1,    -1,    -1,    -1,
      -1,   149,    -1,    -1,   152,   153,  1672,    -1,    -1,    -1,
      -1,   159,   160,   161,   162,   163,   164,   165,    -1,    -1,
      -1,    -1,    48,    -1,    -1,    -1,    52,    -1,    54,   563,
      -1,  1321,  1322,  1323,    -1,  1325,  1326,    -1,    -1,    -1,
      -1,    -1,  1332,    -1,    -1,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   591,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     604,  1024,    98,    99,    -1,   101,    -1,   103,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
    1380,   117,   118,   119,    -1,   121,   122,    -1,    -1,    -1,
    1766,    -1,    -1,   129,    -1,    -1,    -1,    -1,  1774,  1399,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1072,
      -1,   655,  1075,   149,   150,    -1,   152,   153,    -1,    -1,
      -1,   157,    -1,   159,   160,   161,   162,   163,   164,   165,
      -1,    -1,   676,   677,    -1,    -1,   680,    -1,   682,    -1,
      -1,  1120,  1121,  1122,   688,    -1,   690,   691,    -1,    -1,
    1826,    -1,    -1,    -1,  1830,  1831,    -1,    -1,  1834,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1150,    -1,    -1,   718,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1165,   731,  1864,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   742,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   756,    -1,  1514,   759,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1202,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1195,    -1,    -1,    -1,    -1,    -1,  1914,  1915,
    1203,  1204,   786,    -1,    -1,   789,    -1,    -1,  1548,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1947,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   825,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1590,    -1,    -1,    -1,    -1,    -1,    -1,  1260,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1269,    -1,    -1,  1272,
      -1,  1274,  1275,    -1,    -1,    -1,  1616,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   873,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   883,
     884,  1641,  1642,    -1,    -1,    -1,    -1,   891,    -1,    -1,
      -1,  1314,    -1,    -1,    -1,    -1,  1335,    -1,    -1,  1338,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   912,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     1,   921,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     934,  1691,    17,    -1,    -1,    -1,    -1,    -1,   942,    -1,
      -1,    -1,    -1,    -1,    -1,   949,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1381,    -1,
      -1,    -1,    -1,    48,    -1,    -1,    -1,    52,    -1,    54,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    69,    -1,    71,    72,    -1,    74,
      -1,   995,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      -1,    96,    -1,    98,    99,    -1,   101,    -1,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,    -1,   121,   122,    -1,    -1,
    1463,  1801,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1062,    -1,
    1064,    -1,  1066,    -1,   149,    -1,  1826,   152,   153,  1492,
      -1,    -1,   157,    -1,   159,   160,   161,   162,   163,   164,
     165,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1527,   174,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1862,    -1,    -1,  1865,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,  1132,  1133,
      19,    -1,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    -1,
      49,    50,    51,    -1,    53,    -1,  1589,    -1,    -1,    -1,
    1609,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1193,
      -1,    -1,    -1,    -1,    -1,  1199,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1216,    -1,    -1,   104,   105,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1233,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1663,  1664,    -1,    -1,  1248,    -1,    -1,  1251,    -1,    -1,
      -1,    -1,  1675,    -1,    -1,    -1,    -1,    -1,    48,    -1,
      -1,    -1,    52,   152,    54,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    71,    -1,    -1,    -1,  1724,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1733,    -1,    -1,    -1,  1302,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    98,    99,
      -1,   101,    -1,   103,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,    -1,   117,   118,   119,
      -1,   121,   122,    -1,    -1,    -1,  1340,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,  1768,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1358,    -1,    -1,  1361,    -1,   149,
     150,   151,   152,   153,    -1,     1,    -1,    -1,    -1,   159,
     160,   161,   162,   163,   164,   165,    -1,    -1,    -1,    -1,
      -1,    17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1399,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1409,  1410,  1830,    -1,    -1,
      -1,    -1,    48,    -1,    -1,    -1,    52,    -1,    54,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1434,    -1,  1436,    69,    -1,    71,    72,    -1,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    91,    92,    93,    94,    -1,
      96,    -1,    98,    99,    -1,   101,    -1,   103,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,    -1,   121,   122,    -1,    -1,    -1,
      -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1514,    -1,   148,   149,     1,  1519,   152,   153,    -1,    -1,
      -1,   157,    -1,   159,   160,   161,   162,   163,   164,   165,
      17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   174,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    48,    -1,    -1,    -1,    52,    -1,    54,    -1,    -1,
      -1,  1575,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    69,    -1,    71,    72,    -1,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    91,    92,    93,    94,    -1,    96,
    1614,    98,    99,  1617,   101,    -1,   103,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,    -1,   121,   122,    -1,    -1,    -1,    -1,
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   148,   149,    -1,    -1,   152,   153,    -1,    -1,    -1,
     157,    -1,   159,   160,   161,   162,   163,   164,   165,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   174,     3,     4,
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
     115,   116,   117,   118,   119,  1809,   121,   122,    -1,    -1,
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
      44,    45,    -1,    -1,    -1,    -1,    50,    51,    -1,    -1,
     104,   105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,   152,   153,
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
      -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   149,   150,    -1,   152,   153,    -1,    -1,
      -1,   157,    -1,    -1,   160,   161,     4,     5,     6,     7,
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
      -1,    -1,    17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   149,   150,    48,   152,   153,    -1,    52,    -1,    54,
      -1,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    69,    -1,    71,    -1,    -1,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      -1,    96,    -1,    98,    99,    -1,   101,    -1,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,    -1,   121,   122,    -1,    -1,
      -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   149,    -1,    -1,   152,   153,    -1,
      -1,    -1,   157,    -1,   159,   160,   161,   162,   163,   164,
     165,    48,    -1,    -1,    -1,    52,    -1,    54,    -1,   174,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    69,    -1,    71,    -1,    -1,    74,    -1,    -1,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    91,    92,    93,    94,    -1,    96,
      -1,    98,    99,    -1,   101,    -1,   103,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,    -1,   121,   122,    -1,    -1,    -1,    -1,
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   149,    -1,    -1,   152,   153,    -1,    -1,    -1,
     157,    -1,   159,   160,   161,   162,   163,   164,   165,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   174,     4,     5,
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
      -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,
      -1,    -1,    -1,   129,    -1,   131,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   151,   152,   153,    -1,    -1,
     104,   105,    -1,    -1,   160,   161,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      -1,    19,    -1,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,   152,    -1,
      -1,    -1,    50,    51,    -1,    -1,    -1,    55,    56,    57,
      58,    59,    60,    61,    62,    -1,    12,    13,    14,    15,
      16,    17,    70,    19,    -1,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      -1,    -1,    -1,    -1,    50,    51,   104,   105,    -1,    55,
      56,    57,    58,    59,    60,    61,    62,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,
      -1,   129,    -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   152,   153,    -1,    -1,   104,   105,
      -1,    -1,   160,   161,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    -1,    19,
      -1,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,   152,    -1,    -1,    -1,
      50,    51,    -1,    -1,    -1,    55,    56,    57,    58,    59,
      60,    61,    62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   104,   105,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,
      -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,   153,
      -1,    -1,    -1,    -1,    -1,    -1,   160,   161,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    -1,    -1,    19,    -1,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    -1,    49,    50,    51,    -1,    53,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    12,    13,    14,    15,
      16,    -1,    -1,    19,    70,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    -1,    49,    50,    51,    -1,    53,   104,   105,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   152,   153,    -1,    -1,
      -1,    -1,    -1,    -1,   160,   161,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      -1,    -1,    48,    -1,    50,    51,    52,    -1,    54,    55,
      56,    57,    58,    59,    60,    61,    62,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    83,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,    -1,   151,
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
      -1,   149,    -1,    -1,   152,   153,    -1,    -1,    -1,   157,
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
      42,    43,    44,    45,    -1,    -1,    -1,    -1,    50,    51,
      -1,    -1,    -1,    55,    56,    57,    58,    59,    60,    61,
      62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,
      -1,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,   150,    -1,
     152,   153,    -1,    -1,    -1,   157,    -1,    -1,   160,   161,
      12,    13,    14,    15,    16,    17,    -1,    19,    -1,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    -1,    -1,    -1,    -1,    50,    51,
      -1,    -1,    -1,    55,    56,    57,    58,    59,    60,    61,
      62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,
      -1,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,   150,    -1,
     152,   153,    -1,    -1,    -1,    -1,    -1,    -1,   160,   161,
      12,    13,    14,    15,    16,    17,    -1,    19,    -1,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    -1,    -1,    -1,    -1,    50,    51,
      -1,    -1,    -1,    55,    56,    57,    58,    59,    60,    61,
      62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,
      -1,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     152,   153,    -1,    12,    13,    14,    15,    16,   160,   161,
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
     149,   150,    -1,   152,   153,    -1,    -1,    -1,    -1,    -1,
      -1,   160,   161,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    -1,    19,
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
      -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,    76,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   152,    -1,    -1,    -1,   104,   105,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,   152,    -1,    -1,    12,    13,    14,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,   153,   104,
     105,    -1,    -1,    -1,    -1,   160,   161,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,    -1,    -1,
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
      -1,    -1,    -1,    -1,   131,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   151,   152,     4,     5,     6,     7,
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
      -1,    -1,    -1,   131,    -1,    -1,    -1,    -1,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    -1,    19,   152,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      -1,    -1,    -1,    -1,    50,    51,    -1,    -1,    -1,    55,
      56,    57,    58,    59,    60,    61,    62,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,   104,   105,
      19,    -1,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    -1,
      49,    50,    51,    -1,    53,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   152,    -1,    -1,    -1,
      -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   104,   105,    -1,    -1,    -1,
      -1,    12,    13,    14,    15,    16,    17,    -1,    19,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    -1,    -1,    -1,    -1,    50,
      51,    -1,    -1,   152,    55,    56,    57,    58,    59,    60,
      61,    62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     101,    -1,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,    -1,    -1,    -1,    12,    13,    14,    15,    16,
      -1,    -1,    19,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,   152,    49,    50,    51,    -1,    53,    -1,    -1,    48,
      -1,    -1,    -1,    52,    -1,    54,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,   105,    98,
      99,    -1,   101,    -1,   103,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,    -1,   117,   118,
     119,    48,   121,   122,    -1,    52,    -1,    54,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    71,   152,    -1,    -1,    -1,    -1,
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
     157,   235,   236,   237,   151,   221,   223,   174,   104,   173,
     221,   222,   221,   223,   242,   174,   148,   157,   237,   223,
     149,   176,   174,   182,   151,   156,   151,   151,   155,   156,
     249,   253,   357,   399,   177,   154,   154,   345,   462,   148,
     148,   154,   154,   177,   177,   177,   176,   177,   151,   151,
     151,   151,   151,   447,   402,   334,     1,   213,   233,   234,
     400,     1,   156,     1,   176,   223,   235,   174,   151,    73,
     222,   221,   144,   165,   243,   174,   165,    73,   222,   174,
       1,   176,   176,   260,   293,   295,   456,   156,   174,   153,
     182,   265,   266,   267,   223,   198,   188,    73,   106,   250,
     252,   151,   462,   148,   151,   151,   151,   352,   149,   402,
     439,   440,   336,   131,     1,   155,   156,   148,   270,   271,
     277,    73,   174,   223,   150,   150,   221,   222,   221,   223,
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
     240,   240,   240,   240,   240,   240,   240,   240,   240,   241,
     241,   242,   242,   242,   242,   242,   242,   242,   242,   242,
     242,   242,   242,   242,   242,   242,   242,   242,   242,   243,
     243,   243,   243,   244,   244,   244,   244,   244,   244,   244,
     244,   244,   244,   244,   244,   244,   244,   244,   244,   244,
     244,   244,   244,   245,   245,   246,   247,   248,   249,   249,
     250,   250,   251,   251,   252,   253,   253,   253,   253,   253,
     253,   254,   254,   255,   255,   255,   256,   256,   257,   257,
     258,   258,   258,   258,   259,   260,   260,   260,   260,   260,
     261,   262,   262,   263,   263,   263,   263,   263,   264,   264,
     265,   265,   266,   266,   267,   267,   268,   268,   268,   269,
     269,   270,   270,   271,   271,   272,   272,   273,   273,   274,
     274,   275,   275,   276,   276,   277,   277,   277,   278,   278,
     279,   279,   279,   279,   279,   280,   280,   280,   281,   281,
     281,   282,   282,   282,   282,   282,   283,   283,   284,   284,
     285,   285,   285,   286,   286,   286,   286,   286,   287,   287,
     288,   288,   288,   288,   289,   289,   290,   290,   290,   291,
     291,   291,   292,   292,   292,   293,   293,   293,   294,   294,
     295,   295,   296,   296,   297,   297,   297,   297,   297,   298,
     299,   299,   299,   300,   300,   301,   301,   301,   301,   301,
     301,   301,   301,   302,   302,   302,   302,   302,   302,   302,
     302,   302,   302,   302,   302,   302,   302,   302,   302,   302,
     302,   302,   302,   302,   302,   302,   302,   302,   302,   302,
     302,   303,   303,   304,   305,   305,   306,   306,   306,   306,
     306,   307,   307,   308,   308,   308,   308,   309,   309,   309,
     309,   309,   309,   310,   310,   310,   310,   311,   312,   311,
     311,   313,   313,   313,   313,   314,   314,   314,   315,   315,
     315,   315,   316,   316,   316,   317,   317,   317,   317,   317,
     317,   318,   318,   318,   319,   319,   320,   320,   322,   321,
     323,   321,   324,   321,   325,   321,   321,   326,   326,   327,
     327,   328,   328,   329,   329,   329,   330,   330,   330,   330,
     330,   330,   330,   330,   331,   331,   332,   332,   332,   332,
     332,   332,   332,   332,   332,   332,   333,   333,   333,   334,
     334,   334,   335,   335,   335,   336,   337,   337,   338,   338,
     339,   339,   340,   341,   342,   341,   341,   341,   343,   341,
     341,   341,   344,   344,   345,   345,   345,   345,   346,   346,
     347,   347,   347,   347,   347,   347,   347,   348,   348,   348,
     348,   349,   349,   350,   350,   350,   350,   351,   351,   351,
     351,   352,   352,   352,   352,   352,   353,   353,   353,   353,
     353,   354,   354,   355,   355,   356,   356,   357,   357,   357,
     358,   358,   358,   359,   359,   360,   360,   360,   360,   361,
     361,   362,   362,   362,   362,   362,   363,   363,   364,   364,
     365,   365,   365,   365,   365,   366,   366,   367,   367,   369,
     368,   370,   368,   368,   368,   371,   371,   371,   371,   372,
     372,   372,   372,   373,   373,   374,   374,   375,   375,   376,
     376,   376,   376,   377,   377,   377,   378,   378,   379,   379,
     380,   380,   381,   381,   382,   382,   383,   383,   383,   384,
     384,   385,   385,   386,   386,   387,   387,   388,   389,   390,
     390,   390,   390,   390,   391,   390,   392,   390,   393,   390,
     394,   390,   395,   390,   396,   396,   396,   397,   397,   398,
     398,   398,   398,   398,   398,   398,   398,   398,   398,   399,
     399,   399,   400,   401,   401,   402,   402,   403,   403,   404,
     405,   405,   406,   406,   406,   407,   407,   407,   407,   407,
     407,   408,   408,   409,   409,   409,   409,   410,   410,   410,
     410,   411,   411,   411,   411,   411,   411,   411,   412,   412,
     412,   412,   413,   413,   413,   414,   414,   414,   414,   414,
     415,   415,   415,   415,   416,   416,   416,   416,   416,   416,
     417,   417,   417,   418,   418,   418,   418,   418,   419,   419,
     419,   419,   420,   420,   420,   420,   420,   420,   421,   421,
     422,   422,   422,   422,   423,   423,   423,   423,   424,   424,
     424,   424,   424,   424,   424,   425,   425,   425,   425,   425,
     426,   426,   426,   426,   426,   427,   427,   427,   428,   428,
     428,   428,   429,   429,   429,   430,   430,   430,   430,   430,
     431,   431,   432,   432,   432,   433,   433,   434,   434,   435,
     435,   435,   436,   436,   436,   436,   436,   437,   437,   437,
     437,   438,   438,   438,   439,   439,   439,   439,   440,   440,
     440,   440,   441,   441,   441,   441,   442,   442,   442,   442,
     442,   443,   443,   443,   443,   444,   444,   444,   445,   445,
     445,   446,   446,   446,   446,   446,   446,   447,   447,   447,
     448,   448,   448,   448,   448,   449,   449,   449,   449,   450,
     450,   451,   451,   451,   452,   452,   453,   453,   453,   453,
     453,   453,   454,   454,   454,   454,   454,   454,   454,   454,
     454,   454,   455,   455,   455,   455,   456,   456,   456,   457,
     457,   458,   458,   458,   458,   458,   458,   459,   459,   459,
     459,   459,   459,   460,   460,   460,   461,   461,   462,   462,
     463,   463
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
       4,     5,     7,     6,     7,     8,     4,     5,     7,     1,
       3,     4,     5,     4,     1,     2,     3,     5,     2,     3,
       4,     5,     7,     3,     5,     5,     7,     7,     7,     1,
       1,     1,     1,     3,     4,     2,     3,     3,     2,     3,
       2,     3,     3,     6,     2,     2,     3,     3,     3,     3,
       3,     3,     5,     1,     1,     5,     5,     4,     0,     1,
       4,     6,     1,     3,     4,     3,     5,     3,     3,     6,
       7,     3,     5,     3,     3,     4,     8,     9,     0,     2,
       1,     1,     1,     1,     2,     1,     2,     2,     2,     1,
       3,     1,     1,     6,     8,    10,    12,    14,     0,     1,
       0,     1,     1,     3,     4,     7,     0,     1,     3,     1,
       3,     0,     1,     1,     2,     0,     1,     4,     5,     0,
       1,     3,     4,     1,     3,     2,     2,     1,     7,     5,
       1,     1,     1,     1,     1,     2,     3,     6,     3,     3,
       4,     1,     2,     2,     3,     8,     8,     8,     5,     9,
       2,     2,     5,     3,     5,     4,     3,     4,     4,     7,
       2,     1,     1,     1,     3,     6,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     0,     1,
       1,     2,     1,     1,     1,     1,     1,     1,     1,     4,
       1,     2,     3,     1,     2,     1,     1,     1,     1,     1,
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
       1,     1,     2,     1,     1,     0,     2,     2,     4,     1,
       4,     0,     1,     2,     3,     4,     2,     2,     1,     2,
       2,     5,     5,     7,     6,     1,     3,     0,     2,     0,
       5,     0,     5,     3,     1,     0,     1,     1,     1,     1,
       1,     1,     1,     0,     1,     1,     2,     5,     6,     1,
       1,     3,     3,     2,     3,     3,     2,     4,     1,     4,
       7,    10,     1,     4,     2,     2,     1,     1,     5,     2,
       5,     0,     1,     3,     4,     0,     1,     0,     0,     1,
       1,     1,     2,     5,     0,     6,     0,     8,     0,     7,
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
#line 7000 "Parser/parser.cc"
    break;

  case 3:
#line 533 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 7006 "Parser/parser.cc"
    break;

  case 4:
#line 540 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantInteger( *(yyvsp[0].tok) ) ); }
#line 7012 "Parser/parser.cc"
    break;

  case 5:
#line 541 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7018 "Parser/parser.cc"
    break;

  case 6:
#line 542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7024 "Parser/parser.cc"
    break;

  case 7:
#line 543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7030 "Parser/parser.cc"
    break;

  case 8:
#line 544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantChar( *(yyvsp[0].tok) ) ); }
#line 7036 "Parser/parser.cc"
    break;

  case 19:
#line 565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 7042 "Parser/parser.cc"
    break;

  case 20:
#line 569 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.constant) = build_constantStr( *(yyvsp[0].str) ); }
#line 7048 "Parser/parser.cc"
    break;

  case 21:
#line 573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 7054 "Parser/parser.cc"
    break;

  case 22:
#line 575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 7064 "Parser/parser.cc"
    break;

  case 23:
#line 586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 7070 "Parser/parser.cc"
    break;

  case 24:
#line 588 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 7076 "Parser/parser.cc"
    break;

  case 25:
#line 592 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_dimensionref( (yyvsp[0].tok) ) ); }
#line 7082 "Parser/parser.cc"
    break;

  case 27:
#line 595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 7088 "Parser/parser.cc"
    break;

  case 28:
#line 597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new StmtExpr( dynamic_cast<CompoundStmt *>(maybeMoveBuild<Statement>((yyvsp[-1].sn)) ) ) ); }
#line 7094 "Parser/parser.cc"
    break;

  case 29:
#line 599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7100 "Parser/parser.cc"
    break;

  case 30:
#line 601 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7106 "Parser/parser.cc"
    break;

  case 31:
#line 603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild<Expression>( (yyvsp[-3].en) );
			(yyval.en) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 7116 "Parser/parser.cc"
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
#line 7128 "Parser/parser.cc"
    break;

  case 33:
#line 621 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "Identifier \"", *(yyvsp[-1].tok).str, "\" cannot appear before a type. "
											   "Possible problem is misspelled storage or CV qualifier." ) );
			(yyval.en) = nullptr;
		}
#line 7138 "Parser/parser.cc"
    break;

  case 35:
#line 631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// steal the association node from the singleton and delete the wrapper
			(yyvsp[-2].genexpr)->associations.splice((yyvsp[-2].genexpr)->associations.end(), (yyvsp[0].genexpr)->associations);
			delete (yyvsp[0].genexpr);
			(yyval.genexpr) = (yyvsp[-2].genexpr);
		}
#line 7149 "Parser/parser.cc"
    break;

  case 36:
#line 641 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuildType((yyvsp[-2].decl)), maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } );
		}
#line 7158 "Parser/parser.cc"
    break;

  case 37:
#line 646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } ); }
#line 7164 "Parser/parser.cc"
    break;

  case 39:
#line 655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-5].en), new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) ) ) )) ) ); }
#line 7170 "Parser/parser.cc"
    break;

  case 40:
#line 661 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7176 "Parser/parser.cc"
    break;

  case 41:
#line 663 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7182 "Parser/parser.cc"
    break;

  case 42:
#line 665 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, new ExpressionNode( (yyvsp[-3].constant) ), (yyvsp[-1].en) ) ); }
#line 7188 "Parser/parser.cc"
    break;

  case 43:
#line 667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.en) = new ExpressionNode( new ConstructorExpr( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) ) );
		}
#line 7198 "Parser/parser.cc"
    break;

  case 44:
#line 673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7204 "Parser/parser.cc"
    break;

  case 45:
#line 675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 7210 "Parser/parser.cc"
    break;

  case 46:
#line 677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 7216 "Parser/parser.cc"
    break;

  case 47:
#line 679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), new ExpressionNode( (yyvsp[-2].constant) ) ) ); }
#line 7222 "Parser/parser.cc"
    break;

  case 48:
#line 681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7228 "Parser/parser.cc"
    break;

  case 49:
#line 683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 7234 "Parser/parser.cc"
    break;

  case 50:
#line 685 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-1].en), build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) ) ) ); }
#line 7240 "Parser/parser.cc"
    break;

  case 51:
#line 687 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7246 "Parser/parser.cc"
    break;

  case 52:
#line 689 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[0].aggKey), (yyvsp[-2].en) ) ); }
#line 7252 "Parser/parser.cc"
    break;

  case 53:
#line 691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7258 "Parser/parser.cc"
    break;

  case 54:
#line 693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 7264 "Parser/parser.cc"
    break;

  case 55:
#line 695 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7270 "Parser/parser.cc"
    break;

  case 56:
#line 697 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::IncrPost, (yyvsp[-1].en) ) ); }
#line 7276 "Parser/parser.cc"
    break;

  case 57:
#line 699 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::DecrPost, (yyvsp[-1].en) ) ); }
#line 7282 "Parser/parser.cc"
    break;

  case 58:
#line 701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].in), true ) ) ); }
#line 7288 "Parser/parser.cc"
    break;

  case 59:
#line 703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].in), true ))->set_maybeConstructed( false ) ) ); }
#line 7294 "Parser/parser.cc"
    break;

  case 60:
#line 705 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) );
		}
#line 7304 "Parser/parser.cc"
    break;

  case 61:
#line 714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7310 "Parser/parser.cc"
    break;

  case 64:
#line 721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7316 "Parser/parser.cc"
    break;

  case 65:
#line 726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Default parameter for argument is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7322 "Parser/parser.cc"
    break;

  case 68:
#line 733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7328 "Parser/parser.cc"
    break;

  case 70:
#line 739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-1].tok) ) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7334 "Parser/parser.cc"
    break;

  case 71:
#line 741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-3].tok) ) ), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7340 "Parser/parser.cc"
    break;

  case 72:
#line 743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7346 "Parser/parser.cc"
    break;

  case 73:
#line 745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7352 "Parser/parser.cc"
    break;

  case 74:
#line 747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7358 "Parser/parser.cc"
    break;

  case 75:
#line 749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7364 "Parser/parser.cc"
    break;

  case 76:
#line 754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_constantInteger( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 7370 "Parser/parser.cc"
    break;

  case 77:
#line 756 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_field_name_FLOATINGconstant( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 7376 "Parser/parser.cc"
    break;

  case 78:
#line 758 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_varref( (yyvsp[-1].tok) ), (yyvsp[0].en) ) );
		}
#line 7384 "Parser/parser.cc"
    break;

  case 79:
#line 765 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7390 "Parser/parser.cc"
    break;

  case 80:
#line 767 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Expression * constant = build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) );
			(yyval.en) = (yyvsp[-1].en) != nullptr ? new ExpressionNode( build_fieldSel( (yyvsp[-1].en),  constant ) ) : new ExpressionNode( constant );
		}
#line 7399 "Parser/parser.cc"
    break;

  case 83:
#line 779 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 7405 "Parser/parser.cc"
    break;

  case 84:
#line 781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en)->set_extension( true ); }
#line 7411 "Parser/parser.cc"
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
#line 7431 "Parser/parser.cc"
    break;

  case 86:
#line 802 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_val( (yyvsp[-1].op), (yyvsp[0].en) ) ); }
#line 7437 "Parser/parser.cc"
    break;

  case 87:
#line 804 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Incr, (yyvsp[0].en) ) ); }
#line 7443 "Parser/parser.cc"
    break;

  case 88:
#line 806 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Decr, (yyvsp[0].en) ) ); }
#line 7449 "Parser/parser.cc"
    break;

  case 89:
#line 808 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7455 "Parser/parser.cc"
    break;

  case 90:
#line 810 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 7461 "Parser/parser.cc"
    break;

  case 91:
#line 812 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7467 "Parser/parser.cc"
    break;

  case 92:
#line 814 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 7473 "Parser/parser.cc"
    break;

  case 93:
#line 816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_offsetOf( (yyvsp[-3].decl), build_varref( (yyvsp[-1].tok) ) ) ); }
#line 7479 "Parser/parser.cc"
    break;

  case 94:
#line 818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.en) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 7488 "Parser/parser.cc"
    break;

  case 95:
#line 825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::PointTo; }
#line 7494 "Parser/parser.cc"
    break;

  case 96:
#line 826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AddressOf; }
#line 7500 "Parser/parser.cc"
    break;

  case 97:
#line 828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::And; }
#line 7506 "Parser/parser.cc"
    break;

  case 98:
#line 832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::UnPlus; }
#line 7512 "Parser/parser.cc"
    break;

  case 99:
#line 833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::UnMinus; }
#line 7518 "Parser/parser.cc"
    break;

  case 100:
#line 834 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::Neg; }
#line 7524 "Parser/parser.cc"
    break;

  case 101:
#line 835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::BitNeg; }
#line 7530 "Parser/parser.cc"
    break;

  case 103:
#line 841 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cast( (yyvsp[-2].decl), (yyvsp[0].en) ) ); }
#line 7536 "Parser/parser.cc"
    break;

  case 104:
#line 843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 7542 "Parser/parser.cc"
    break;

  case 105:
#line 845 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 7548 "Parser/parser.cc"
    break;

  case 106:
#line 847 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( nullptr ) ) ); }
#line 7554 "Parser/parser.cc"
    break;

  case 107:
#line 849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 7560 "Parser/parser.cc"
    break;

  case 108:
#line 851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Return cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7566 "Parser/parser.cc"
    break;

  case 109:
#line 853 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7572 "Parser/parser.cc"
    break;

  case 110:
#line 855 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7578 "Parser/parser.cc"
    break;

  case 118:
#line 875 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Exp, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7584 "Parser/parser.cc"
    break;

  case 120:
#line 881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mul, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7590 "Parser/parser.cc"
    break;

  case 121:
#line 883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Div, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7596 "Parser/parser.cc"
    break;

  case 122:
#line 885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mod, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7602 "Parser/parser.cc"
    break;

  case 124:
#line 891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Plus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7608 "Parser/parser.cc"
    break;

  case 125:
#line 893 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Minus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7614 "Parser/parser.cc"
    break;

  case 127:
#line 899 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7620 "Parser/parser.cc"
    break;

  case 128:
#line 901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::RShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7626 "Parser/parser.cc"
    break;

  case 130:
#line 907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7632 "Parser/parser.cc"
    break;

  case 131:
#line 909 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7638 "Parser/parser.cc"
    break;

  case 132:
#line 911 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7644 "Parser/parser.cc"
    break;

  case 133:
#line 913 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7650 "Parser/parser.cc"
    break;

  case 135:
#line 919 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Eq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7656 "Parser/parser.cc"
    break;

  case 136:
#line 921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Neq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7662 "Parser/parser.cc"
    break;

  case 138:
#line 927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitAnd, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7668 "Parser/parser.cc"
    break;

  case 140:
#line 933 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Xor, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7674 "Parser/parser.cc"
    break;

  case 142:
#line 939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitOr, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7680 "Parser/parser.cc"
    break;

  case 144:
#line 945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), true ) ); }
#line 7686 "Parser/parser.cc"
    break;

  case 146:
#line 951 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), false ) ); }
#line 7692 "Parser/parser.cc"
    break;

  case 148:
#line 957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7698 "Parser/parser.cc"
    break;

  case 149:
#line 960 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-3].en), (yyvsp[-3].en), (yyvsp[0].en) ) ); }
#line 7704 "Parser/parser.cc"
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
#line 7716 "Parser/parser.cc"
    break;

  case 153:
#line 979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7722 "Parser/parser.cc"
    break;

  case 154:
#line 984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7728 "Parser/parser.cc"
    break;

  case 158:
#line 994 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::Assign; }
#line 7734 "Parser/parser.cc"
    break;

  case 159:
#line 995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AtAssn; }
#line 7740 "Parser/parser.cc"
    break;

  case 160:
#line 999 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ExpAssn; }
#line 7746 "Parser/parser.cc"
    break;

  case 161:
#line 1000 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MulAssn; }
#line 7752 "Parser/parser.cc"
    break;

  case 162:
#line 1001 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::DivAssn; }
#line 7758 "Parser/parser.cc"
    break;

  case 163:
#line 1002 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ModAssn; }
#line 7764 "Parser/parser.cc"
    break;

  case 164:
#line 1003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::PlusAssn; }
#line 7770 "Parser/parser.cc"
    break;

  case 165:
#line 1004 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MinusAssn; }
#line 7776 "Parser/parser.cc"
    break;

  case 166:
#line 1005 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::LSAssn; }
#line 7782 "Parser/parser.cc"
    break;

  case 167:
#line 1006 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::RSAssn; }
#line 7788 "Parser/parser.cc"
    break;

  case 168:
#line 1007 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AndAssn; }
#line 7794 "Parser/parser.cc"
    break;

  case 169:
#line 1008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ERAssn; }
#line 7800 "Parser/parser.cc"
    break;

  case 170:
#line 1009 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::OrAssn; }
#line 7806 "Parser/parser.cc"
    break;

  case 171:
#line 1020 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)(new ExpressionNode( nullptr ) )->set_last( (yyvsp[-1].en) ) ) ); }
#line 7812 "Parser/parser.cc"
    break;

  case 172:
#line 1022 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-4].en)->set_last( (yyvsp[-1].en) ) ) )); }
#line 7818 "Parser/parser.cc"
    break;

  case 174:
#line 1028 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7824 "Parser/parser.cc"
    break;

  case 175:
#line 1030 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7830 "Parser/parser.cc"
    break;

  case 176:
#line 1032 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7836 "Parser/parser.cc"
    break;

  case 178:
#line 1038 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new CommaExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7842 "Parser/parser.cc"
    break;

  case 179:
#line 1043 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7848 "Parser/parser.cc"
    break;

  case 191:
#line 1061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 7854 "Parser/parser.cc"
    break;

  case 193:
#line 1064 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_directive( (yyvsp[0].tok) ) ); }
#line 7860 "Parser/parser.cc"
    break;

  case 194:
#line 1070 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[0].sn)->add_label( (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 7866 "Parser/parser.cc"
    break;

  case 195:
#line 1072 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "Label \"", *(yyvsp[-3].tok).str, "\" must be associated with a statement, "
											   "where a declaration, case, or default is not a statement. "
											   "Move the label or terminate with a semi-colon." ) );
			(yyval.sn) = nullptr;
		}
#line 7877 "Parser/parser.cc"
    break;

  case 196:
#line 1082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (StatementNode *)0 ) ); }
#line 7883 "Parser/parser.cc"
    break;

  case 197:
#line 1087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (yyvsp[-2].sn) ) ); }
#line 7889 "Parser/parser.cc"
    break;

  case 199:
#line 1093 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 7895 "Parser/parser.cc"
    break;

  case 200:
#line 1098 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7901 "Parser/parser.cc"
    break;

  case 201:
#line 1100 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7907 "Parser/parser.cc"
    break;

  case 202:
#line 1102 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7913 "Parser/parser.cc"
    break;

  case 203:
#line 1104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7919 "Parser/parser.cc"
    break;

  case 206:
#line 1111 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 7925 "Parser/parser.cc"
    break;

  case 207:
#line 1113 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Declarations only allowed at the start of the switch body, i.e., after the '{'." ); (yyval.sn) = nullptr; }
#line 7931 "Parser/parser.cc"
    break;

  case 208:
#line 1118 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_expr( (yyvsp[-1].en) ) ); }
#line 7937 "Parser/parser.cc"
    break;

  case 209:
#line 1120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_mutex( nullptr, new StatementNode( build_expr( (yyvsp[-1].en) ) ) ) ); }
#line 7943 "Parser/parser.cc"
    break;

  case 210:
#line 1127 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn); }
#line 7949 "Parser/parser.cc"
    break;

  case 211:
#line 1129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( true, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 7955 "Parser/parser.cc"
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
#line 7969 "Parser/parser.cc"
    break;

  case 213:
#line 1141 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Only declarations can appear before the list of case clauses." ); (yyval.sn) = nullptr; }
#line 7975 "Parser/parser.cc"
    break;

  case 214:
#line 1143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( false, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 7981 "Parser/parser.cc"
    break;

  case 215:
#line 1145 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( false, (yyvsp[-7].en), (yyvsp[-2].sn) ) );
			(yyval.sn) = (yyvsp[-3].decl) ? new StatementNode( build_compound( (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 7990 "Parser/parser.cc"
    break;

  case 216:
#line 1150 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Only declarations can appear before the list of case clauses." ); (yyval.sn) = nullptr; }
#line 7996 "Parser/parser.cc"
    break;

  case 217:
#line 1156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-2].ifctl), maybe_build_compound( (yyvsp[0].sn) ), nullptr ) ); }
#line 8002 "Parser/parser.cc"
    break;

  case 218:
#line 1158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-4].ifctl), maybe_build_compound( (yyvsp[-2].sn) ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8008 "Parser/parser.cc"
    break;

  case 219:
#line 1163 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( nullptr, (yyvsp[0].en) ); }
#line 8014 "Parser/parser.cc"
    break;

  case 220:
#line 1165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 8020 "Parser/parser.cc"
    break;

  case 221:
#line 1167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 8026 "Parser/parser.cc"
    break;

  case 222:
#line 1169 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[-1].decl), (yyvsp[0].en) ); }
#line 8032 "Parser/parser.cc"
    break;

  case 223:
#line 1176 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = (yyvsp[0].en); }
#line 8038 "Parser/parser.cc"
    break;

  case 224:
#line 1178 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 8044 "Parser/parser.cc"
    break;

  case 226:
#line 1183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.sn) = new StatementNode( build_case( (yyvsp[0].en) ) ); }
#line 8050 "Parser/parser.cc"
    break;

  case 227:
#line 1185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)((yyvsp[-2].sn)->set_last( new StatementNode( build_case( (yyvsp[0].en) ) ) ) ); }
#line 8056 "Parser/parser.cc"
    break;

  case 228:
#line 1190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing case list after case." ); (yyval.sn) = nullptr; }
#line 8062 "Parser/parser.cc"
    break;

  case 229:
#line 1191 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn); }
#line 8068 "Parser/parser.cc"
    break;

  case 230:
#line 1193 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing colon after case list." ); (yyval.sn) = nullptr; }
#line 8074 "Parser/parser.cc"
    break;

  case 231:
#line 1194 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.sn) = new StatementNode( build_default() ); }
#line 8080 "Parser/parser.cc"
    break;

  case 232:
#line 1197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing colon after default." ); (yyval.sn) = nullptr; }
#line 8086 "Parser/parser.cc"
    break;

  case 234:
#line 1202 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)( (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) )); }
#line 8092 "Parser/parser.cc"
    break;

  case 235:
#line 1206 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( maybe_build_compound( (yyvsp[0].sn) ) ); }
#line 8098 "Parser/parser.cc"
    break;

  case 236:
#line 1211 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = nullptr; }
#line 8104 "Parser/parser.cc"
    break;

  case 238:
#line 1217 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ); }
#line 8110 "Parser/parser.cc"
    break;

  case 239:
#line 1219 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)( (yyvsp[-2].sn)->set_last( (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ) ) ); }
#line 8116 "Parser/parser.cc"
    break;

  case 240:
#line 1224 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( new CondCtl( nullptr, new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8122 "Parser/parser.cc"
    break;

  case 241:
#line 1226 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( (yyvsp[-2].ifctl), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8128 "Parser/parser.cc"
    break;

  case 242:
#line 1228 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( (yyvsp[-4].ifctl), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[0].sn) ) ); }
#line 8134 "Parser/parser.cc"
    break;

  case 243:
#line 1230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( new ExpressionNode( build_constantInteger( *new string( "1" ) ) ), maybe_build_compound( (yyvsp[-4].sn) ) ) ); }
#line 8140 "Parser/parser.cc"
    break;

  case 244:
#line 1232 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( (yyvsp[-2].en), maybe_build_compound( (yyvsp[-5].sn) ) ) ); }
#line 8146 "Parser/parser.cc"
    break;

  case 245:
#line 1234 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( (yyvsp[-3].en), maybe_build_compound( (yyvsp[-6].sn) ), (yyvsp[0].sn) ) ); }
#line 8152 "Parser/parser.cc"
    break;

  case 246:
#line 1236 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( new ForCtrl( (ExpressionNode * )nullptr, (ExpressionNode * )nullptr, (ExpressionNode * )nullptr ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8158 "Parser/parser.cc"
    break;

  case 247:
#line 1238 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( (yyvsp[-2].fctl), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8164 "Parser/parser.cc"
    break;

  case 248:
#line 1240 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( (yyvsp[-4].fctl), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[0].sn) ) ); }
#line 8170 "Parser/parser.cc"
    break;

  case 250:
#line 1250 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 8189 "Parser/parser.cc"
    break;

  case 251:
#line 1268 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (ExpressionNode * )nullptr, (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8195 "Parser/parser.cc"
    break;

  case 252:
#line 1270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8201 "Parser/parser.cc"
    break;

  case 253:
#line 1272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-3].decl), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8207 "Parser/parser.cc"
    break;

  case 254:
#line 1275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8214 "Parser/parser.cc"
    break;

  case 255:
#line 1278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LEThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8221 "Parser/parser.cc"
    break;

  case 256:
#line 1281 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].en)->clone(), (yyvsp[-1].compop), (yyvsp[0].en), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8227 "Parser/parser.cc"
    break;

  case 257:
#line 1283 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8233 "Parser/parser.cc"
    break;

  case 258:
#line 1285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( new ExpressionNode( build_constantInteger( *new string( "0u" ) ) ), (yyvsp[-1].en), nullptr, OperKinds::LThan, nullptr, nullptr ); }
#line 8239 "Parser/parser.cc"
    break;

  case 259:
#line 1287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-2].en), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8246 "Parser/parser.cc"
    break;

  case 260:
#line 1290 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-3].en), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LEThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8253 "Parser/parser.cc"
    break;

  case 261:
#line 1293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), (yyvsp[-1].compop), (yyvsp[0].en), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8259 "Parser/parser.cc"
    break;

  case 262:
#line 1295 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8265 "Parser/parser.cc"
    break;

  case 263:
#line 1298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Array interator is currently unimplemented." ); (yyval.fctl) = nullptr;
			(yyval.fctl) = forCtrl( new ExpressionNode( build_varref( (yyvsp[0].tok) ) ), (yyvsp[-2].en), nullptr, OperKinds::Range, nullptr, nullptr );
		}
#line 8274 "Parser/parser.cc"
    break;

  case 264:
#line 1305 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), OperKinds::LThan, nullptr, new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8280 "Parser/parser.cc"
    break;

  case 265:
#line 1307 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), OperKinds::GThan, nullptr, new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8286 "Parser/parser.cc"
    break;

  case 266:
#line 1309 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), OperKinds::LThan, nullptr, (yyvsp[0].en) ); }
#line 8292 "Parser/parser.cc"
    break;

  case 267:
#line 1311 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), OperKinds::GThan, nullptr, (yyvsp[0].en) ); }
#line 8298 "Parser/parser.cc"
    break;

  case 268:
#line 1313 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), OperKinds::LThan, nullptr, nullptr ); }
#line 8304 "Parser/parser.cc"
    break;

  case 269:
#line 1318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LThan; }
#line 8310 "Parser/parser.cc"
    break;

  case 270:
#line 1320 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 8316 "Parser/parser.cc"
    break;

  case 271:
#line 1322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GThan; }
#line 8322 "Parser/parser.cc"
    break;

  case 272:
#line 1324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GEThan; }
#line 8328 "Parser/parser.cc"
    break;

  case 273:
#line 1329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Goto ) ); }
#line 8334 "Parser/parser.cc"
    break;

  case 274:
#line 1333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_computedgoto( (yyvsp[-1].en) ) ); }
#line 8340 "Parser/parser.cc"
    break;

  case 275:
#line 1336 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThrough ) ); }
#line 8346 "Parser/parser.cc"
    break;

  case 276:
#line 1338 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::FallThrough ) ); }
#line 8352 "Parser/parser.cc"
    break;

  case 277:
#line 1340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThroughDefault ) ); }
#line 8358 "Parser/parser.cc"
    break;

  case 278:
#line 1343 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Continue ) ); }
#line 8364 "Parser/parser.cc"
    break;

  case 279:
#line 1347 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Continue ) ); }
#line 8370 "Parser/parser.cc"
    break;

  case 280:
#line 1350 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Break ) ); }
#line 8376 "Parser/parser.cc"
    break;

  case 281:
#line 1354 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Break ) ); }
#line 8382 "Parser/parser.cc"
    break;

  case 282:
#line 1356 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_return( (yyvsp[-1].en) ) ); }
#line 8388 "Parser/parser.cc"
    break;

  case 283:
#line 1358 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 8394 "Parser/parser.cc"
    break;

  case 284:
#line 1360 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr ) ); }
#line 8400 "Parser/parser.cc"
    break;

  case 285:
#line 1362 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn) ) ); }
#line 8406 "Parser/parser.cc"
    break;

  case 286:
#line 1364 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Coroutine ) ); }
#line 8412 "Parser/parser.cc"
    break;

  case 287:
#line 1366 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Coroutine ) ); }
#line 8418 "Parser/parser.cc"
    break;

  case 288:
#line 1368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Generator ) ); }
#line 8424 "Parser/parser.cc"
    break;

  case 289:
#line 1370 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Generator ) ); }
#line 8430 "Parser/parser.cc"
    break;

  case 290:
#line 1372 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_throw( (yyvsp[-1].en) ) ); }
#line 8436 "Parser/parser.cc"
    break;

  case 291:
#line 1374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume( (yyvsp[-1].en) ) ); }
#line 8442 "Parser/parser.cc"
    break;

  case 292:
#line 1376 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume_at( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 8448 "Parser/parser.cc"
    break;

  case 295:
#line 1386 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_with( (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8454 "Parser/parser.cc"
    break;

  case 296:
#line 1392 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_mutex( (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8460 "Parser/parser.cc"
    break;

  case 297:
#line 1396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 8466 "Parser/parser.cc"
    break;

  case 298:
#line 1401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8472 "Parser/parser.cc"
    break;

  case 300:
#line 1407 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 8478 "Parser/parser.cc"
    break;

  case 301:
#line 1411 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) )); }
#line 8484 "Parser/parser.cc"
    break;

  case 303:
#line 1418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8490 "Parser/parser.cc"
    break;

  case 304:
#line 1422 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 8496 "Parser/parser.cc"
    break;

  case 305:
#line 1427 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8502 "Parser/parser.cc"
    break;

  case 306:
#line 1429 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-3].en), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[-4].en), (yyvsp[0].wfs) ); }
#line 8508 "Parser/parser.cc"
    break;

  case 307:
#line 1431 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8514 "Parser/parser.cc"
    break;

  case 308:
#line 1433 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( nullptr, maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8520 "Parser/parser.cc"
    break;

  case 309:
#line 1436 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 8526 "Parser/parser.cc"
    break;

  case 310:
#line 1438 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-5].en), maybe_build_compound( (yyvsp[-4].sn) ), (yyvsp[-6].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8532 "Parser/parser.cc"
    break;

  case 311:
#line 1443 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-1].en), (yyvsp[0].sn), (yyvsp[-2].en) ) ); }
#line 8538 "Parser/parser.cc"
    break;

  case 312:
#line 1445 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-3].en), (yyvsp[-2].sn), (yyvsp[-4].en), (yyvsp[0].wfs) ) ); }
#line 8544 "Parser/parser.cc"
    break;

  case 313:
#line 1450 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), (yyvsp[0].sn), 0 ) ); }
#line 8550 "Parser/parser.cc"
    break;

  case 314:
#line 1452 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), 0, (yyvsp[0].sn) ) ); }
#line 8556 "Parser/parser.cc"
    break;

  case 315:
#line 1454 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-2].sn), (yyvsp[-1].sn), (yyvsp[0].sn) ) ); }
#line 8562 "Parser/parser.cc"
    break;

  case 316:
#line 1459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8568 "Parser/parser.cc"
    break;

  case 317:
#line 1461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)(yyvsp[-8].sn)->set_last( new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ) ); }
#line 8574 "Parser/parser.cc"
    break;

  case 318:
#line 1466 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8580 "Parser/parser.cc"
    break;

  case 319:
#line 1467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[0].en); }
#line 8586 "Parser/parser.cc"
    break;

  case 320:
#line 1471 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 8592 "Parser/parser.cc"
    break;

  case 321:
#line 1472 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 8598 "Parser/parser.cc"
    break;

  case 322:
#line 1473 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.catch_kind) = CatchStmt::Resume; }
#line 8604 "Parser/parser.cc"
    break;

  case 323:
#line 1474 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Resume; }
#line 8610 "Parser/parser.cc"
    break;

  case 324:
#line 1478 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = new StatementNode( build_finally( (yyvsp[0].sn) ) ); }
#line 8616 "Parser/parser.cc"
    break;

  case 326:
#line 1485 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 8622 "Parser/parser.cc"
    break;

  case 327:
#line 1487 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 8628 "Parser/parser.cc"
    break;

  case 328:
#line 1489 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 8634 "Parser/parser.cc"
    break;

  case 333:
#line 1504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-4].flag), (yyvsp[-2].constant), 0 ) ); }
#line 8640 "Parser/parser.cc"
    break;

  case 334:
#line 1506 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-6].flag), (yyvsp[-4].constant), (yyvsp[-2].en) ) ); }
#line 8646 "Parser/parser.cc"
    break;

  case 335:
#line 1508 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-8].flag), (yyvsp[-6].constant), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 8652 "Parser/parser.cc"
    break;

  case 336:
#line 1510 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-10].flag), (yyvsp[-8].constant), (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 8658 "Parser/parser.cc"
    break;

  case 337:
#line 1512 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-12].flag), (yyvsp[-9].constant), 0, (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].label) ) ); }
#line 8664 "Parser/parser.cc"
    break;

  case 338:
#line 1517 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = false; }
#line 8670 "Parser/parser.cc"
    break;

  case 339:
#line 1519 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = true; }
#line 8676 "Parser/parser.cc"
    break;

  case 340:
#line 1524 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8682 "Parser/parser.cc"
    break;

  case 343:
#line 1531 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 8688 "Parser/parser.cc"
    break;

  case 344:
#line 1536 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( nullptr, (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 8694 "Parser/parser.cc"
    break;

  case 345:
#line 1538 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( (yyvsp[-5].tok), (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 8700 "Parser/parser.cc"
    break;

  case 346:
#line 1543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8706 "Parser/parser.cc"
    break;

  case 347:
#line 1545 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 8712 "Parser/parser.cc"
    break;

  case 348:
#line 1547 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( (yyvsp[0].constant) ) )); }
#line 8718 "Parser/parser.cc"
    break;

  case 349:
#line 1552 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = new LabelNode(); (yyval.label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 8727 "Parser/parser.cc"
    break;

  case 350:
#line 1557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = (yyvsp[-2].label); (yyvsp[-2].label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 8736 "Parser/parser.cc"
    break;

  case 351:
#line 1567 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8742 "Parser/parser.cc"
    break;

  case 354:
#line 1574 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 8748 "Parser/parser.cc"
    break;

  case 355:
#line 1579 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8754 "Parser/parser.cc"
    break;

  case 357:
#line 1585 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 8760 "Parser/parser.cc"
    break;

  case 358:
#line 1587 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-2].decl) ); }
#line 8766 "Parser/parser.cc"
    break;

  case 368:
#line 1613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-4].en), (yyvsp[-2].constant) ); }
#line 8772 "Parser/parser.cc"
    break;

  case 369:
#line 1615 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-2].en), build_constantStr( *new string( "\"\"" ) ) ); }
#line 8778 "Parser/parser.cc"
    break;

  case 373:
#line 1633 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 8784 "Parser/parser.cc"
    break;

  case 375:
#line 1639 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].in) ); }
#line 8790 "Parser/parser.cc"
    break;

  case 376:
#line 1643 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 8796 "Parser/parser.cc"
    break;

  case 377:
#line 1645 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].in) ) ); }
#line 8802 "Parser/parser.cc"
    break;

  case 378:
#line 1652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 8808 "Parser/parser.cc"
    break;

  case 379:
#line 1654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 8814 "Parser/parser.cc"
    break;

  case 380:
#line 1656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 8820 "Parser/parser.cc"
    break;

  case 382:
#line 1662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 8826 "Parser/parser.cc"
    break;

  case 383:
#line 1664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 8832 "Parser/parser.cc"
    break;

  case 384:
#line 1666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 8838 "Parser/parser.cc"
    break;

  case 385:
#line 1668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeClone( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->appendList( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 8849 "Parser/parser.cc"
    break;

  case 386:
#line 1701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), 0 )->addQualifiers( (yyvsp[0].decl) ); }
#line 8855 "Parser/parser.cc"
    break;

  case 387:
#line 1703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), 0 )->addQualifiers( (yyvsp[0].decl) ); }
#line 8861 "Parser/parser.cc"
    break;

  case 388:
#line 1708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 8867 "Parser/parser.cc"
    break;

  case 389:
#line 1711 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-6].decl)->appendList( (yyvsp[-2].decl) ) ); }
#line 8873 "Parser/parser.cc"
    break;

  case 390:
#line 1716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 8882 "Parser/parser.cc"
    break;

  case 391:
#line 1721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 8891 "Parser/parser.cc"
    break;

  case 392:
#line 1726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "3" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 8900 "Parser/parser.cc"
    break;

  case 393:
#line 1737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "4" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef();
		}
#line 8909 "Parser/parser.cc"
    break;

  case 394:
#line 1742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "5" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 8918 "Parser/parser.cc"
    break;

  case 395:
#line 1747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "6" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[-3].decl) )->addTypedef();
		}
#line 8927 "Parser/parser.cc"
    break;

  case 396:
#line 1752 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "7" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-2].decl) )->addTypedef();
		}
#line 8936 "Parser/parser.cc"
    break;

  case 397:
#line 1757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "8" );
			(yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-3].decl) )->addTypedef()->addType( (yyvsp[-3].decl) );
		}
#line 8945 "Parser/parser.cc"
    break;

  case 398:
#line 1766 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 8953 "Parser/parser.cc"
    break;

  case 399:
#line 1770 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 8961 "Parser/parser.cc"
    break;

  case 400:
#line 1777 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 8967 "Parser/parser.cc"
    break;

  case 404:
#line 1787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 8973 "Parser/parser.cc"
    break;

  case 405:
#line 1789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ) ); }
#line 8979 "Parser/parser.cc"
    break;

  case 418:
#line 1828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8985 "Parser/parser.cc"
    break;

  case 421:
#line 1840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 8991 "Parser/parser.cc"
    break;

  case 424:
#line 1850 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Const ); }
#line 8997 "Parser/parser.cc"
    break;

  case 425:
#line 1852 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Restrict ); }
#line 9003 "Parser/parser.cc"
    break;

  case 426:
#line 1854 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Volatile ); }
#line 9009 "Parser/parser.cc"
    break;

  case 427:
#line 1856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Atomic ); }
#line 9015 "Parser/parser.cc"
    break;

  case 429:
#line 1862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newForall( (yyvsp[-1].decl) ); }
#line 9021 "Parser/parser.cc"
    break;

  case 431:
#line 1868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9027 "Parser/parser.cc"
    break;

  case 432:
#line 1870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9033 "Parser/parser.cc"
    break;

  case 434:
#line 1881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9039 "Parser/parser.cc"
    break;

  case 435:
#line 1886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Extern ); }
#line 9045 "Parser/parser.cc"
    break;

  case 436:
#line 1888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Static ); }
#line 9051 "Parser/parser.cc"
    break;

  case 437:
#line 1890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Auto ); }
#line 9057 "Parser/parser.cc"
    break;

  case 438:
#line 1892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Register ); }
#line 9063 "Parser/parser.cc"
    break;

  case 439:
#line 1894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Threadlocal ); }
#line 9069 "Parser/parser.cc"
    break;

  case 440:
#line 1897 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Inline ); }
#line 9075 "Parser/parser.cc"
    break;

  case 441:
#line 1899 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Fortran ); }
#line 9081 "Parser/parser.cc"
    break;

  case 442:
#line 1901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Noreturn ); }
#line 9087 "Parser/parser.cc"
    break;

  case 443:
#line 1906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 9093 "Parser/parser.cc"
    break;

  case 444:
#line 1908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Bool ); }
#line 9099 "Parser/parser.cc"
    break;

  case 445:
#line 1910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Char ); }
#line 9105 "Parser/parser.cc"
    break;

  case 446:
#line 1912 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int ); }
#line 9111 "Parser/parser.cc"
    break;

  case 447:
#line 1914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 ); }
#line 9117 "Parser/parser.cc"
    break;

  case 448:
#line 1916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 )->addType( DeclarationNode::newSignedNess( DeclarationNode::Unsigned ) ); }
#line 9123 "Parser/parser.cc"
    break;

  case 449:
#line 1918 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Float ); }
#line 9129 "Parser/parser.cc"
    break;

  case 450:
#line 1920 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Double ); }
#line 9135 "Parser/parser.cc"
    break;

  case 451:
#line 1922 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat80 ); }
#line 9141 "Parser/parser.cc"
    break;

  case 452:
#line 1924 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat128 ); }
#line 9147 "Parser/parser.cc"
    break;

  case 453:
#line 1926 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat16 ); }
#line 9153 "Parser/parser.cc"
    break;

  case 454:
#line 1928 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32 ); }
#line 9159 "Parser/parser.cc"
    break;

  case 455:
#line 1930 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32x ); }
#line 9165 "Parser/parser.cc"
    break;

  case 456:
#line 1932 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64 ); }
#line 9171 "Parser/parser.cc"
    break;

  case 457:
#line 1934 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64x ); }
#line 9177 "Parser/parser.cc"
    break;

  case 458:
#line 1936 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat128 ); }
#line 9183 "Parser/parser.cc"
    break;

  case 459:
#line 1938 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9189 "Parser/parser.cc"
    break;

  case 460:
#line 1940 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9195 "Parser/parser.cc"
    break;

  case 461:
#line 1942 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9201 "Parser/parser.cc"
    break;

  case 462:
#line 1944 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Complex ); }
#line 9207 "Parser/parser.cc"
    break;

  case 463:
#line 1946 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Imaginary ); }
#line 9213 "Parser/parser.cc"
    break;

  case 464:
#line 1948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Signed ); }
#line 9219 "Parser/parser.cc"
    break;

  case 465:
#line 1950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Unsigned ); }
#line 9225 "Parser/parser.cc"
    break;

  case 466:
#line 1952 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Short ); }
#line 9231 "Parser/parser.cc"
    break;

  case 467:
#line 1954 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Long ); }
#line 9237 "Parser/parser.cc"
    break;

  case 468:
#line 1956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Valist ); }
#line 9243 "Parser/parser.cc"
    break;

  case 469:
#line 1958 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::AutoType ); }
#line 9249 "Parser/parser.cc"
    break;

  case 471:
#line 1964 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9255 "Parser/parser.cc"
    break;

  case 473:
#line 1970 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVtableType( (yyvsp[-2].decl) ); }
#line 9261 "Parser/parser.cc"
    break;

  case 474:
#line 1976 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9267 "Parser/parser.cc"
    break;

  case 475:
#line 1978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9273 "Parser/parser.cc"
    break;

  case 477:
#line 1985 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9279 "Parser/parser.cc"
    break;

  case 478:
#line 1987 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9285 "Parser/parser.cc"
    break;

  case 479:
#line 1989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9291 "Parser/parser.cc"
    break;

  case 480:
#line 1991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 9297 "Parser/parser.cc"
    break;

  case 482:
#line 1998 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9303 "Parser/parser.cc"
    break;

  case 484:
#line 2004 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9309 "Parser/parser.cc"
    break;

  case 485:
#line 2006 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9315 "Parser/parser.cc"
    break;

  case 486:
#line 2008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 9321 "Parser/parser.cc"
    break;

  case 487:
#line 2013 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 9327 "Parser/parser.cc"
    break;

  case 488:
#line 2015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en) ); }
#line 9333 "Parser/parser.cc"
    break;

  case 489:
#line 2017 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 9339 "Parser/parser.cc"
    break;

  case 490:
#line 2019 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en), true ); }
#line 9345 "Parser/parser.cc"
    break;

  case 491:
#line 2021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Zero ); }
#line 9351 "Parser/parser.cc"
    break;

  case 492:
#line 2023 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::One ); }
#line 9357 "Parser/parser.cc"
    break;

  case 494:
#line 2029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9363 "Parser/parser.cc"
    break;

  case 495:
#line 2031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9369 "Parser/parser.cc"
    break;

  case 496:
#line 2033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9375 "Parser/parser.cc"
    break;

  case 498:
#line 2039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 9381 "Parser/parser.cc"
    break;

  case 499:
#line 2041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 9387 "Parser/parser.cc"
    break;

  case 500:
#line 2043 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 9396 "Parser/parser.cc"
    break;

  case 502:
#line 2052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9402 "Parser/parser.cc"
    break;

  case 503:
#line 2054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9408 "Parser/parser.cc"
    break;

  case 504:
#line 2056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9414 "Parser/parser.cc"
    break;

  case 506:
#line 2062 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9420 "Parser/parser.cc"
    break;

  case 507:
#line 2064 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9426 "Parser/parser.cc"
    break;

  case 509:
#line 2070 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9432 "Parser/parser.cc"
    break;

  case 510:
#line 2072 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9438 "Parser/parser.cc"
    break;

  case 511:
#line 2074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9444 "Parser/parser.cc"
    break;

  case 513:
#line 2080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9450 "Parser/parser.cc"
    break;

  case 514:
#line 2082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9456 "Parser/parser.cc"
    break;

  case 515:
#line 2087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypedef( (yyvsp[0].tok) ); }
#line 9462 "Parser/parser.cc"
    break;

  case 516:
#line 2089 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 9468 "Parser/parser.cc"
    break;

  case 517:
#line 2091 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 9474 "Parser/parser.cc"
    break;

  case 519:
#line 2094 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), (yyvsp[0].decl) ); }
#line 9480 "Parser/parser.cc"
    break;

  case 520:
#line 2096 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), (yyvsp[0].decl) ); }
#line 9486 "Parser/parser.cc"
    break;

  case 521:
#line 2101 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[0].tok), nullptr ); }
#line 9492 "Parser/parser.cc"
    break;

  case 522:
#line 2103 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-2].tok), nullptr ); }
#line 9498 "Parser/parser.cc"
    break;

  case 523:
#line 2105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 9504 "Parser/parser.cc"
    break;

  case 528:
#line 2120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 9510 "Parser/parser.cc"
    break;

  case 529:
#line 2122 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 9516 "Parser/parser.cc"
    break;

  case 530:
#line 2124 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 9525 "Parser/parser.cc"
    break;

  case 531:
#line 2129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) ); }
#line 9531 "Parser/parser.cc"
    break;

  case 532:
#line 2131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 9540 "Parser/parser.cc"
    break;

  case 533:
#line 2136 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypedef( (yyvsp[-5].tok) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 9549 "Parser/parser.cc"
    break;

  case 534:
#line 2141 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 9558 "Parser/parser.cc"
    break;

  case 535:
#line 2146 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeGen( (yyvsp[-5].tok), nullptr );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 9567 "Parser/parser.cc"
    break;

  case 537:
#line 2155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9573 "Parser/parser.cc"
    break;

  case 538:
#line 2157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 9579 "Parser/parser.cc"
    break;

  case 539:
#line 2162 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 9589 "Parser/parser.cc"
    break;

  case 540:
#line 2168 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 9604 "Parser/parser.cc"
    break;

  case 543:
#line 2187 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Struct; }
#line 9610 "Parser/parser.cc"
    break;

  case 544:
#line 2189 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Union; }
#line 9616 "Parser/parser.cc"
    break;

  case 545:
#line 2191 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Exception; }
#line 9622 "Parser/parser.cc"
    break;

  case 546:
#line 2197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 9628 "Parser/parser.cc"
    break;

  case 547:
#line 2199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 9634 "Parser/parser.cc"
    break;

  case 548:
#line 2201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Generator; }
#line 9640 "Parser/parser.cc"
    break;

  case 549:
#line 2203 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor generator is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 9646 "Parser/parser.cc"
    break;

  case 550:
#line 2205 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Coroutine; }
#line 9652 "Parser/parser.cc"
    break;

  case 551:
#line 2207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor coroutine is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 9658 "Parser/parser.cc"
    break;

  case 552:
#line 2209 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Thread; }
#line 9664 "Parser/parser.cc"
    break;

  case 553:
#line 2211 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor thread is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 9670 "Parser/parser.cc"
    break;

  case 554:
#line 2216 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9676 "Parser/parser.cc"
    break;

  case 555:
#line 2218 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 9682 "Parser/parser.cc"
    break;

  case 556:
#line 2223 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); }
#line 9688 "Parser/parser.cc"
    break;

  case 557:
#line 2225 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 9694 "Parser/parser.cc"
    break;

  case 558:
#line 2227 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-1].decl) ) {								// field declarator ?
				(yyvsp[-1].decl) = DeclarationNode::newName( nullptr );
			} // if
			(yyvsp[-1].decl)->inLine = true;
			(yyval.decl) = distAttr( (yyvsp[-2].decl), (yyvsp[-1].decl) );					// mark all fields in list
			distInl( (yyvsp[-1].decl) );
		}
#line 9707 "Parser/parser.cc"
    break;

  case 559:
#line 2236 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9713 "Parser/parser.cc"
    break;

  case 562:
#line 2240 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 9719 "Parser/parser.cc"
    break;

  case 563:
#line 2242 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 9725 "Parser/parser.cc"
    break;

  case 566:
#line 2249 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9731 "Parser/parser.cc"
    break;

  case 568:
#line 2252 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 9737 "Parser/parser.cc"
    break;

  case 569:
#line 2257 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].en) ); }
#line 9743 "Parser/parser.cc"
    break;

  case 570:
#line 2260 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 9749 "Parser/parser.cc"
    break;

  case 571:
#line 2263 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 9755 "Parser/parser.cc"
    break;

  case 572:
#line 2268 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9761 "Parser/parser.cc"
    break;

  case 574:
#line 2271 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 9767 "Parser/parser.cc"
    break;

  case 576:
#line 2282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 9773 "Parser/parser.cc"
    break;

  case 577:
#line 2284 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 9779 "Parser/parser.cc"
    break;

  case 579:
#line 2291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 9785 "Parser/parser.cc"
    break;

  case 580:
#line 2296 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9791 "Parser/parser.cc"
    break;

  case 582:
#line 2302 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 9797 "Parser/parser.cc"
    break;

  case 583:
#line 2307 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, false )->addQualifiers( (yyvsp[-4].decl) ); }
#line 9803 "Parser/parser.cc"
    break;

  case 584:
#line 2309 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); }
#line 9809 "Parser/parser.cc"
    break;

  case 585:
#line 2311 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].tok), (yyvsp[-2].decl), true, false )->addQualifiers( (yyvsp[-6].decl) ); }
#line 9815 "Parser/parser.cc"
    break;

  case 586:
#line 2314 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-4].decl)->name, (yyvsp[-2].decl), true, false )->addQualifiers( (yyvsp[-5].decl) ); }
#line 9821 "Parser/parser.cc"
    break;

  case 587:
#line 2316 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-6].decl)->storageClasses.val != 0 || (yyvsp[-6].decl)->type->qualifiers.val != 0 ) 
			{ SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			// SemanticError( yylloc, "Typed enumeration is currently unimplemented." ); $$ = nullptr;

			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true ) ->addQualifiers( (yyvsp[-4].decl) )  -> addEnumBase( (yyvsp[-6].decl) );
			// $$ = DeclarationNode::newEnum( nullptr, $7, true, true ) ->addQualifiers( $5 );
		}
#line 9834 "Parser/parser.cc"
    break;

  case 588:
#line 2325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].decl)->storageClasses.val != 0 || (yyvsp[-4].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			typedefTable.makeTypedef( *(yyvsp[-1].tok) );
		}
#line 9843 "Parser/parser.cc"
    break;

  case 589:
#line 2330 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, true ) -> addQualifiers( (yyvsp[-7].decl) ) -> addQualifiers( (yyvsp[-5].decl) ) -> addEnumBase( (yyvsp[-9].decl) );
			// $$ = DeclarationNode::newEnum( $6, $10, true, true ) -> addQualifiers( $5 ) -> addQualifiers( $7 );
		}
#line 9852 "Parser/parser.cc"
    break;

  case 590:
#line 2335 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-8].decl)->storageClasses.val != 0 || (yyvsp[-8].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			typedefTable.makeTypedef( *(yyvsp[-5].decl)->name );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].decl)->name, (yyvsp[-2].decl), true, true ) -> addQualifiers( (yyvsp[-6].decl) ) -> addQualifiers( (yyvsp[-4].decl) ) -> addEnumBase( (yyvsp[-8].decl) );
			// $$ = DeclarationNode::newEnum( $6->name, $9, true, true ) -> addQualifiers( $5 ) -> addQualifiers( $7 );
		}
#line 9863 "Parser/parser.cc"
    break;

  case 592:
#line 2346 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); (yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), 0, false, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 9869 "Parser/parser.cc"
    break;

  case 593:
#line 2348 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].decl)->type->symbolic.name );	(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].decl)->type->symbolic.name, 0, false, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 9875 "Parser/parser.cc"
    break;

  case 594:
#line 2353 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumConstant( (yyvsp[-1].tok), (yyvsp[0].en) ); }
#line 9881 "Parser/parser.cc"
    break;

  case 595:
#line 2355 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumConstant( new string("inline"), nullptr ); }
#line 9887 "Parser/parser.cc"
    break;

  case 596:
#line 2357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( DeclarationNode::newEnumConstant( (yyvsp[-1].tok), (yyvsp[0].en) ) ); }
#line 9893 "Parser/parser.cc"
    break;

  case 597:
#line 2359 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( DeclarationNode::newEnumConstant( new string("inline"), nullptr ) ); }
#line 9899 "Parser/parser.cc"
    break;

  case 598:
#line 2364 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9905 "Parser/parser.cc"
    break;

  case 599:
#line 2368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].in)->get_expression(); }
#line 9911 "Parser/parser.cc"
    break;

  case 600:
#line 2373 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 9917 "Parser/parser.cc"
    break;

  case 601:
#line 2375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9923 "Parser/parser.cc"
    break;

  case 604:
#line 2379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9929 "Parser/parser.cc"
    break;

  case 605:
#line 2381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 9935 "Parser/parser.cc"
    break;

  case 606:
#line 2383 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 9941 "Parser/parser.cc"
    break;

  case 608:
#line 2391 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9947 "Parser/parser.cc"
    break;

  case 609:
#line 2393 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9953 "Parser/parser.cc"
    break;

  case 610:
#line 2395 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-8].decl)->appendList( (yyvsp[-4].decl) )->appendList( (yyvsp[0].decl) ); }
#line 9959 "Parser/parser.cc"
    break;

  case 612:
#line 2401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9965 "Parser/parser.cc"
    break;

  case 613:
#line 2406 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9971 "Parser/parser.cc"
    break;

  case 614:
#line 2408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9977 "Parser/parser.cc"
    break;

  case 616:
#line 2411 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 9983 "Parser/parser.cc"
    break;

  case 619:
#line 2418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9989 "Parser/parser.cc"
    break;

  case 620:
#line 2420 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9995 "Parser/parser.cc"
    break;

  case 622:
#line 2429 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 10001 "Parser/parser.cc"
    break;

  case 623:
#line 2432 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 10007 "Parser/parser.cc"
    break;

  case 624:
#line 2434 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 10013 "Parser/parser.cc"
    break;

  case 629:
#line 2444 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10019 "Parser/parser.cc"
    break;

  case 631:
#line 2451 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 10025 "Parser/parser.cc"
    break;

  case 632:
#line 2453 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 10031 "Parser/parser.cc"
    break;

  case 633:
#line 2458 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 10037 "Parser/parser.cc"
    break;

  case 634:
#line 2460 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 10043 "Parser/parser.cc"
    break;

  case 635:
#line 2469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 10049 "Parser/parser.cc"
    break;

  case 636:
#line 2471 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 10055 "Parser/parser.cc"
    break;

  case 642:
#line 2484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10061 "Parser/parser.cc"
    break;

  case 645:
#line 2494 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 10067 "Parser/parser.cc"
    break;

  case 646:
#line 2495 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (yyvsp[-1].op) == OperKinds::Assign ? (yyvsp[0].in) : (yyvsp[0].in)->set_maybeConstructed( false ); }
#line 10073 "Parser/parser.cc"
    break;

  case 647:
#line 2496 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.in) = new InitializerNode( true ); }
#line 10079 "Parser/parser.cc"
    break;

  case 648:
#line 2497 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 10085 "Parser/parser.cc"
    break;

  case 649:
#line 2501 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = new InitializerNode( (yyvsp[0].en) ); }
#line 10091 "Parser/parser.cc"
    break;

  case 650:
#line 2502 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 10097 "Parser/parser.cc"
    break;

  case 651:
#line 2507 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 10103 "Parser/parser.cc"
    break;

  case 653:
#line 2509 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ); }
#line 10109 "Parser/parser.cc"
    break;

  case 654:
#line 2510 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (InitializerNode *)( (yyvsp[-2].in)->set_last( (yyvsp[0].in) ) ); }
#line 10115 "Parser/parser.cc"
    break;

  case 655:
#line 2511 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.in) = (InitializerNode *)((yyvsp[-3].in)->set_last( (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ) )); }
#line 10121 "Parser/parser.cc"
    break;

  case 657:
#line 2527 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[-1].tok) ) ); }
#line 10127 "Parser/parser.cc"
    break;

  case 659:
#line 2533 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-1].en)->set_last( (yyvsp[0].en) )); }
#line 10133 "Parser/parser.cc"
    break;

  case 660:
#line 2539 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 10139 "Parser/parser.cc"
    break;

  case 661:
#line 2542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 10145 "Parser/parser.cc"
    break;

  case 662:
#line 2544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 10151 "Parser/parser.cc"
    break;

  case 663:
#line 2546 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-4].en) ), maybeMoveBuild<Expression>( (yyvsp[-2].en) ) ) ); }
#line 10157 "Parser/parser.cc"
    break;

  case 664:
#line 2548 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 10163 "Parser/parser.cc"
    break;

  case 666:
#line 2572 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl) ); }
#line 10169 "Parser/parser.cc"
    break;

  case 667:
#line 2577 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10175 "Parser/parser.cc"
    break;

  case 668:
#line 2579 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 10181 "Parser/parser.cc"
    break;

  case 669:
#line 2584 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "9" );
			if ( (yyvsp[-1].tclass) == TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 10192 "Parser/parser.cc"
    break;

  case 670:
#line 2591 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 10198 "Parser/parser.cc"
    break;

  case 671:
#line 2593 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "9" ); }
#line 10204 "Parser/parser.cc"
    break;

  case 672:
#line 2595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 10210 "Parser/parser.cc"
    break;

  case 673:
#line 2597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "9" );
			(yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 10219 "Parser/parser.cc"
    break;

  case 674:
#line 2603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 10225 "Parser/parser.cc"
    break;

  case 675:
#line 2608 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 10231 "Parser/parser.cc"
    break;

  case 676:
#line 2610 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 10237 "Parser/parser.cc"
    break;

  case 677:
#line 2612 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::DStype; }
#line 10243 "Parser/parser.cc"
    break;

  case 678:
#line 2616 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 10249 "Parser/parser.cc"
    break;

  case 679:
#line 2621 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 10255 "Parser/parser.cc"
    break;

  case 680:
#line 2623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 10261 "Parser/parser.cc"
    break;

  case 681:
#line 2625 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ftype; }
#line 10267 "Parser/parser.cc"
    break;

  case 682:
#line 2627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 10273 "Parser/parser.cc"
    break;

  case 683:
#line 2632 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10279 "Parser/parser.cc"
    break;

  case 686:
#line 2639 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 10285 "Parser/parser.cc"
    break;

  case 687:
#line 2644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 10291 "Parser/parser.cc"
    break;

  case 688:
#line 2646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 10297 "Parser/parser.cc"
    break;

  case 689:
#line 2653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 10303 "Parser/parser.cc"
    break;

  case 691:
#line 2656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 10309 "Parser/parser.cc"
    break;

  case 692:
#line 2658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)( (yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 10315 "Parser/parser.cc"
    break;

  case 693:
#line 2663 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 10321 "Parser/parser.cc"
    break;

  case 694:
#line 2665 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10327 "Parser/parser.cc"
    break;

  case 695:
#line 2667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 10333 "Parser/parser.cc"
    break;

  case 696:
#line 2672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 10339 "Parser/parser.cc"
    break;

  case 697:
#line 2674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 10345 "Parser/parser.cc"
    break;

  case 698:
#line 2679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "10" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), 0 );
		}
#line 10354 "Parser/parser.cc"
    break;

  case 699:
#line 2684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "11" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 10363 "Parser/parser.cc"
    break;

  case 700:
#line 2692 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), 0 ); }
#line 10369 "Parser/parser.cc"
    break;

  case 701:
#line 2694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-8].tok), (yyvsp[-6].decl), (yyvsp[-2].decl) ); }
#line 10375 "Parser/parser.cc"
    break;

  case 703:
#line 2700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl) ); }
#line 10381 "Parser/parser.cc"
    break;

  case 708:
#line 2712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 10387 "Parser/parser.cc"
    break;

  case 709:
#line 2717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10393 "Parser/parser.cc"
    break;

  case 710:
#line 2719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 10399 "Parser/parser.cc"
    break;

  case 712:
#line 2727 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl);	}
#line 10405 "Parser/parser.cc"
    break;

  case 713:
#line 2732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10411 "Parser/parser.cc"
    break;

  case 714:
#line 2734 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->appendList( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 10417 "Parser/parser.cc"
    break;

  case 715:
#line 2739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10423 "Parser/parser.cc"
    break;

  case 717:
#line 2744 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 10429 "Parser/parser.cc"
    break;

  case 718:
#line 2748 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 10435 "Parser/parser.cc"
    break;

  case 719:
#line 2753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( (yyvsp[0].tok) ) ) ); }
#line 10441 "Parser/parser.cc"
    break;

  case 722:
#line 2757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 10450 "Parser/parser.cc"
    break;

  case 723:
#line 2762 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( false, (yyvsp[-2].constant), 0 ) ) ); }
#line 10456 "Parser/parser.cc"
    break;

  case 724:
#line 2764 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = LinkageSpec::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 10465 "Parser/parser.cc"
    break;

  case 725:
#line 2769 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-1].decl);
		}
#line 10475 "Parser/parser.cc"
    break;

  case 726:
#line 2775 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = LinkageSpec::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 10484 "Parser/parser.cc"
    break;

  case 727:
#line 2780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10494 "Parser/parser.cc"
    break;

  case 728:
#line 2786 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 10503 "Parser/parser.cc"
    break;

  case 729:
#line 2791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10513 "Parser/parser.cc"
    break;

  case 730:
#line 2797 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 10522 "Parser/parser.cc"
    break;

  case 731:
#line 2802 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10532 "Parser/parser.cc"
    break;

  case 732:
#line 2808 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.val) || (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 10541 "Parser/parser.cc"
    break;

  case 733:
#line 2813 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10551 "Parser/parser.cc"
    break;

  case 735:
#line 2828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].sn) ); }
#line 10557 "Parser/parser.cc"
    break;

  case 736:
#line 2830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].sn) ); }
#line 10563 "Parser/parser.cc"
    break;

  case 737:
#line 2835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; forall = false; }
#line 10569 "Parser/parser.cc"
    break;

  case 738:
#line 2837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); forall = false; }
#line 10575 "Parser/parser.cc"
    break;

  case 739:
#line 2842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10586 "Parser/parser.cc"
    break;

  case 740:
#line 2849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 10595 "Parser/parser.cc"
    break;

  case 741:
#line 2854 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 10604 "Parser/parser.cc"
    break;

  case 742:
#line 2860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 10610 "Parser/parser.cc"
    break;

  case 743:
#line 2863 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 10616 "Parser/parser.cc"
    break;

  case 744:
#line 2866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10622 "Parser/parser.cc"
    break;

  case 745:
#line 2870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-4].decl) );
		}
#line 10631 "Parser/parser.cc"
    break;

  case 746:
#line 2876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10637 "Parser/parser.cc"
    break;

  case 747:
#line 2879 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10643 "Parser/parser.cc"
    break;

  case 748:
#line 2882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 10649 "Parser/parser.cc"
    break;

  case 752:
#line 2893 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 10655 "Parser/parser.cc"
    break;

  case 753:
#line 2898 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10661 "Parser/parser.cc"
    break;

  case 754:
#line 2900 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = (yyvsp[-2].constant);
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 10671 "Parser/parser.cc"
    break;

  case 755:
#line 2909 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10677 "Parser/parser.cc"
    break;

  case 758:
#line 2916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10683 "Parser/parser.cc"
    break;

  case 759:
#line 2921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 10689 "Parser/parser.cc"
    break;

  case 761:
#line 2927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10695 "Parser/parser.cc"
    break;

  case 762:
#line 2932 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10701 "Parser/parser.cc"
    break;

  case 763:
#line 2934 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 10707 "Parser/parser.cc"
    break;

  case 764:
#line 2936 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 10713 "Parser/parser.cc"
    break;

  case 769:
#line 2945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 10719 "Parser/parser.cc"
    break;

  case 770:
#line 2947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 10725 "Parser/parser.cc"
    break;

  case 771:
#line 2982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 10731 "Parser/parser.cc"
    break;

  case 772:
#line 2984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10737 "Parser/parser.cc"
    break;

  case 773:
#line 2989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10743 "Parser/parser.cc"
    break;

  case 775:
#line 2992 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10749 "Parser/parser.cc"
    break;

  case 776:
#line 2994 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10755 "Parser/parser.cc"
    break;

  case 777:
#line 2999 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10761 "Parser/parser.cc"
    break;

  case 778:
#line 3001 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10767 "Parser/parser.cc"
    break;

  case 779:
#line 3003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10773 "Parser/parser.cc"
    break;

  case 780:
#line 3005 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10779 "Parser/parser.cc"
    break;

  case 781:
#line 3010 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 10785 "Parser/parser.cc"
    break;

  case 782:
#line 3012 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10791 "Parser/parser.cc"
    break;

  case 783:
#line 3014 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10797 "Parser/parser.cc"
    break;

  case 784:
#line 3016 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10803 "Parser/parser.cc"
    break;

  case 785:
#line 3018 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10809 "Parser/parser.cc"
    break;

  case 786:
#line 3020 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10815 "Parser/parser.cc"
    break;

  case 787:
#line 3022 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10821 "Parser/parser.cc"
    break;

  case 788:
#line 3027 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10827 "Parser/parser.cc"
    break;

  case 789:
#line 3029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 10833 "Parser/parser.cc"
    break;

  case 790:
#line 3031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10839 "Parser/parser.cc"
    break;

  case 791:
#line 3033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10845 "Parser/parser.cc"
    break;

  case 792:
#line 3042 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10851 "Parser/parser.cc"
    break;

  case 794:
#line 3045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10857 "Parser/parser.cc"
    break;

  case 795:
#line 3050 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10863 "Parser/parser.cc"
    break;

  case 796:
#line 3052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10869 "Parser/parser.cc"
    break;

  case 797:
#line 3054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 10875 "Parser/parser.cc"
    break;

  case 798:
#line 3056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10881 "Parser/parser.cc"
    break;

  case 799:
#line 3058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10887 "Parser/parser.cc"
    break;

  case 800:
#line 3063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10893 "Parser/parser.cc"
    break;

  case 801:
#line 3065 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10899 "Parser/parser.cc"
    break;

  case 802:
#line 3067 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10905 "Parser/parser.cc"
    break;

  case 803:
#line 3069 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10911 "Parser/parser.cc"
    break;

  case 804:
#line 3074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10917 "Parser/parser.cc"
    break;

  case 805:
#line 3076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10923 "Parser/parser.cc"
    break;

  case 806:
#line 3078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10929 "Parser/parser.cc"
    break;

  case 807:
#line 3080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10935 "Parser/parser.cc"
    break;

  case 808:
#line 3082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10941 "Parser/parser.cc"
    break;

  case 809:
#line 3084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10947 "Parser/parser.cc"
    break;

  case 813:
#line 3102 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 10953 "Parser/parser.cc"
    break;

  case 814:
#line 3104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10959 "Parser/parser.cc"
    break;

  case 815:
#line 3106 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 10965 "Parser/parser.cc"
    break;

  case 816:
#line 3108 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10971 "Parser/parser.cc"
    break;

  case 817:
#line 3110 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10977 "Parser/parser.cc"
    break;

  case 818:
#line 3115 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10983 "Parser/parser.cc"
    break;

  case 819:
#line 3117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10989 "Parser/parser.cc"
    break;

  case 820:
#line 3119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10995 "Parser/parser.cc"
    break;

  case 821:
#line 3121 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11001 "Parser/parser.cc"
    break;

  case 822:
#line 3126 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11007 "Parser/parser.cc"
    break;

  case 823:
#line 3128 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11013 "Parser/parser.cc"
    break;

  case 824:
#line 3130 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11019 "Parser/parser.cc"
    break;

  case 825:
#line 3132 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11025 "Parser/parser.cc"
    break;

  case 826:
#line 3134 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11031 "Parser/parser.cc"
    break;

  case 827:
#line 3136 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11037 "Parser/parser.cc"
    break;

  case 828:
#line 3151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "ID" );
		}
#line 11046 "Parser/parser.cc"
    break;

  case 829:
#line 3156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11052 "Parser/parser.cc"
    break;

  case 830:
#line 3161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11058 "Parser/parser.cc"
    break;

  case 832:
#line 3164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11064 "Parser/parser.cc"
    break;

  case 833:
#line 3166 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11070 "Parser/parser.cc"
    break;

  case 834:
#line 3171 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11076 "Parser/parser.cc"
    break;

  case 835:
#line 3173 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11082 "Parser/parser.cc"
    break;

  case 836:
#line 3175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11088 "Parser/parser.cc"
    break;

  case 837:
#line 3177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11094 "Parser/parser.cc"
    break;

  case 838:
#line 3182 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11100 "Parser/parser.cc"
    break;

  case 839:
#line 3184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11106 "Parser/parser.cc"
    break;

  case 840:
#line 3186 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11112 "Parser/parser.cc"
    break;

  case 841:
#line 3188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11118 "Parser/parser.cc"
    break;

  case 842:
#line 3190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11124 "Parser/parser.cc"
    break;

  case 843:
#line 3192 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11130 "Parser/parser.cc"
    break;

  case 844:
#line 3194 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11136 "Parser/parser.cc"
    break;

  case 845:
#line 3199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11142 "Parser/parser.cc"
    break;

  case 846:
#line 3201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11148 "Parser/parser.cc"
    break;

  case 847:
#line 3203 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 11154 "Parser/parser.cc"
    break;

  case 848:
#line 3205 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11160 "Parser/parser.cc"
    break;

  case 849:
#line 3207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11166 "Parser/parser.cc"
    break;

  case 850:
#line 3217 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11172 "Parser/parser.cc"
    break;

  case 851:
#line 3219 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11178 "Parser/parser.cc"
    break;

  case 853:
#line 3222 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11184 "Parser/parser.cc"
    break;

  case 854:
#line 3224 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11190 "Parser/parser.cc"
    break;

  case 855:
#line 3229 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11196 "Parser/parser.cc"
    break;

  case 856:
#line 3231 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11202 "Parser/parser.cc"
    break;

  case 857:
#line 3233 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11208 "Parser/parser.cc"
    break;

  case 858:
#line 3238 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11214 "Parser/parser.cc"
    break;

  case 859:
#line 3240 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11220 "Parser/parser.cc"
    break;

  case 860:
#line 3242 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11226 "Parser/parser.cc"
    break;

  case 861:
#line 3244 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11232 "Parser/parser.cc"
    break;

  case 862:
#line 3249 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11238 "Parser/parser.cc"
    break;

  case 863:
#line 3251 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11244 "Parser/parser.cc"
    break;

  case 864:
#line 3253 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11250 "Parser/parser.cc"
    break;

  case 865:
#line 3267 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11256 "Parser/parser.cc"
    break;

  case 866:
#line 3269 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11262 "Parser/parser.cc"
    break;

  case 868:
#line 3272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11268 "Parser/parser.cc"
    break;

  case 869:
#line 3274 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11274 "Parser/parser.cc"
    break;

  case 870:
#line 3279 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11280 "Parser/parser.cc"
    break;

  case 871:
#line 3281 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11286 "Parser/parser.cc"
    break;

  case 872:
#line 3286 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11292 "Parser/parser.cc"
    break;

  case 873:
#line 3288 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11298 "Parser/parser.cc"
    break;

  case 874:
#line 3290 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11304 "Parser/parser.cc"
    break;

  case 875:
#line 3295 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11310 "Parser/parser.cc"
    break;

  case 876:
#line 3297 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11316 "Parser/parser.cc"
    break;

  case 877:
#line 3302 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11322 "Parser/parser.cc"
    break;

  case 878:
#line 3304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11328 "Parser/parser.cc"
    break;

  case 880:
#line 3322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11334 "Parser/parser.cc"
    break;

  case 881:
#line 3324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11340 "Parser/parser.cc"
    break;

  case 882:
#line 3329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( 0, (yyvsp[0].op) ); }
#line 11346 "Parser/parser.cc"
    break;

  case 883:
#line 3331 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 11352 "Parser/parser.cc"
    break;

  case 884:
#line 3333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11358 "Parser/parser.cc"
    break;

  case 885:
#line 3335 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11364 "Parser/parser.cc"
    break;

  case 886:
#line 3337 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11370 "Parser/parser.cc"
    break;

  case 888:
#line 3343 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11376 "Parser/parser.cc"
    break;

  case 889:
#line 3345 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11382 "Parser/parser.cc"
    break;

  case 890:
#line 3347 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11388 "Parser/parser.cc"
    break;

  case 891:
#line 3352 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 11394 "Parser/parser.cc"
    break;

  case 892:
#line 3354 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11400 "Parser/parser.cc"
    break;

  case 893:
#line 3356 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11406 "Parser/parser.cc"
    break;

  case 894:
#line 3362 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false ); }
#line 11412 "Parser/parser.cc"
    break;

  case 895:
#line 3364 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false )->addArray( (yyvsp[0].decl) ); }
#line 11418 "Parser/parser.cc"
    break;

  case 896:
#line 3366 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-4].en), 0, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].en), 0, false ) ); }
#line 11424 "Parser/parser.cc"
    break;

  case 898:
#line 3373 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), 0, false ); }
#line 11430 "Parser/parser.cc"
    break;

  case 899:
#line 3375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 11436 "Parser/parser.cc"
    break;

  case 900:
#line 3377 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newArray( (yyvsp[-2].en), 0, false ) ); }
#line 11442 "Parser/parser.cc"
    break;

  case 901:
#line 3379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 11448 "Parser/parser.cc"
    break;

  case 903:
#line 3414 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 11454 "Parser/parser.cc"
    break;

  case 904:
#line 3416 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11460 "Parser/parser.cc"
    break;

  case 905:
#line 3418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11466 "Parser/parser.cc"
    break;

  case 906:
#line 3423 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].op) ); }
#line 11472 "Parser/parser.cc"
    break;

  case 907:
#line 3425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 11478 "Parser/parser.cc"
    break;

  case 908:
#line 3427 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 11484 "Parser/parser.cc"
    break;

  case 909:
#line 3429 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11490 "Parser/parser.cc"
    break;

  case 910:
#line 3431 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11496 "Parser/parser.cc"
    break;

  case 912:
#line 3437 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11502 "Parser/parser.cc"
    break;

  case 913:
#line 3439 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11508 "Parser/parser.cc"
    break;

  case 914:
#line 3441 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11514 "Parser/parser.cc"
    break;

  case 915:
#line 3446 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 11520 "Parser/parser.cc"
    break;

  case 916:
#line 3448 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11526 "Parser/parser.cc"
    break;

  case 917:
#line 3450 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11532 "Parser/parser.cc"
    break;

  case 919:
#line 3457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11538 "Parser/parser.cc"
    break;

  case 921:
#line 3468 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false ); }
#line 11544 "Parser/parser.cc"
    break;

  case 922:
#line 3471 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 11550 "Parser/parser.cc"
    break;

  case 923:
#line 3473 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, (yyvsp[-2].decl), false ); }
#line 11556 "Parser/parser.cc"
    break;

  case 924:
#line 3476 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 11562 "Parser/parser.cc"
    break;

  case 925:
#line 3478 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 11568 "Parser/parser.cc"
    break;

  case 926:
#line 3480 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-4].decl), true ); }
#line 11574 "Parser/parser.cc"
    break;

  case 928:
#line 3495 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11580 "Parser/parser.cc"
    break;

  case 929:
#line 3497 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11586 "Parser/parser.cc"
    break;

  case 930:
#line 3502 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( 0, (yyvsp[0].op) ); }
#line 11592 "Parser/parser.cc"
    break;

  case 931:
#line 3504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 11598 "Parser/parser.cc"
    break;

  case 932:
#line 3506 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11604 "Parser/parser.cc"
    break;

  case 933:
#line 3508 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11610 "Parser/parser.cc"
    break;

  case 934:
#line 3510 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11616 "Parser/parser.cc"
    break;

  case 936:
#line 3516 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11622 "Parser/parser.cc"
    break;

  case 937:
#line 3518 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11628 "Parser/parser.cc"
    break;

  case 938:
#line 3520 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11634 "Parser/parser.cc"
    break;

  case 939:
#line 3525 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11640 "Parser/parser.cc"
    break;

  case 940:
#line 3527 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11646 "Parser/parser.cc"
    break;

  case 943:
#line 3537 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11652 "Parser/parser.cc"
    break;

  case 946:
#line 3548 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11658 "Parser/parser.cc"
    break;

  case 947:
#line 3550 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11664 "Parser/parser.cc"
    break;

  case 948:
#line 3552 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11670 "Parser/parser.cc"
    break;

  case 949:
#line 3554 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11676 "Parser/parser.cc"
    break;

  case 950:
#line 3556 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11682 "Parser/parser.cc"
    break;

  case 951:
#line 3558 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11688 "Parser/parser.cc"
    break;

  case 952:
#line 3565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11694 "Parser/parser.cc"
    break;

  case 953:
#line 3567 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11700 "Parser/parser.cc"
    break;

  case 954:
#line 3569 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11706 "Parser/parser.cc"
    break;

  case 955:
#line 3571 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 11712 "Parser/parser.cc"
    break;

  case 956:
#line 3573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11718 "Parser/parser.cc"
    break;

  case 957:
#line 3576 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11724 "Parser/parser.cc"
    break;

  case 958:
#line 3578 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11730 "Parser/parser.cc"
    break;

  case 959:
#line 3580 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11736 "Parser/parser.cc"
    break;

  case 960:
#line 3582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 11742 "Parser/parser.cc"
    break;

  case 961:
#line 3584 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11748 "Parser/parser.cc"
    break;

  case 962:
#line 3589 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 11754 "Parser/parser.cc"
    break;

  case 963:
#line 3591 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 11760 "Parser/parser.cc"
    break;

  case 964:
#line 3596 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 11766 "Parser/parser.cc"
    break;

  case 965:
#line 3598 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl)->addQualifiers( (yyvsp[-4].decl) ), true ); }
#line 11772 "Parser/parser.cc"
    break;

  case 967:
#line 3625 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11778 "Parser/parser.cc"
    break;

  case 971:
#line 3636 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11784 "Parser/parser.cc"
    break;

  case 972:
#line 3638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11790 "Parser/parser.cc"
    break;

  case 973:
#line 3640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11796 "Parser/parser.cc"
    break;

  case 974:
#line 3642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11802 "Parser/parser.cc"
    break;

  case 975:
#line 3644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11808 "Parser/parser.cc"
    break;

  case 976:
#line 3646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11814 "Parser/parser.cc"
    break;

  case 977:
#line 3653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11820 "Parser/parser.cc"
    break;

  case 978:
#line 3655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11826 "Parser/parser.cc"
    break;

  case 979:
#line 3657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11832 "Parser/parser.cc"
    break;

  case 980:
#line 3659 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11838 "Parser/parser.cc"
    break;

  case 981:
#line 3661 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11844 "Parser/parser.cc"
    break;

  case 982:
#line 3663 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11850 "Parser/parser.cc"
    break;

  case 983:
#line 3668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 11856 "Parser/parser.cc"
    break;

  case 984:
#line 3670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11862 "Parser/parser.cc"
    break;

  case 985:
#line 3672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11868 "Parser/parser.cc"
    break;

  case 986:
#line 3679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 11874 "Parser/parser.cc"
    break;

  case 987:
#line 3681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 11880 "Parser/parser.cc"
    break;

  case 990:
#line 3705 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 11886 "Parser/parser.cc"
    break;

  case 991:
#line 3707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 11892 "Parser/parser.cc"
    break;


#line 11896 "Parser/parser.cc"

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
#line 3710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "make install" //
// End: //
