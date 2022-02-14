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
#define YYLAST   19087

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  175
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  289
/* YYNRULES -- Number of rules.  */
#define YYNRULES  980
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  1993

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
    1052,  1057,  1059,  1067,  1068,  1073,  1075,  1077,  1079,  1081,
    1085,  1086,  1091,  1093,  1101,  1103,  1105,  1115,  1117,  1125,
    1128,  1133,  1135,  1137,  1139,  1147,  1148,  1150,  1154,  1156,
    1160,  1161,  1172,  1173,  1177,  1182,  1183,  1187,  1189,  1194,
    1196,  1198,  1200,  1202,  1204,  1206,  1208,  1210,  1215,  1216,
    1238,  1240,  1242,  1245,  1248,  1251,  1253,  1255,  1257,  1260,
    1263,  1265,  1268,  1275,  1277,  1279,  1281,  1283,  1288,  1290,
    1292,  1294,  1299,  1301,  1306,  1308,  1310,  1312,  1315,  1319,
    1322,  1326,  1328,  1330,  1332,  1334,  1336,  1338,  1340,  1342,
    1344,  1346,  1351,  1352,  1356,  1362,  1367,  1372,  1373,  1377,
    1381,  1386,  1387,  1393,  1397,  1399,  1401,  1403,  1406,  1408,
    1413,  1415,  1420,  1422,  1424,  1429,  1431,  1437,  1438,  1442,
    1443,  1444,  1445,  1449,  1454,  1455,  1457,  1459,  1461,  1465,
    1469,  1470,  1474,  1476,  1478,  1480,  1482,  1488,  1489,  1495,
    1496,  1500,  1501,  1506,  1508,  1514,  1515,  1517,  1522,  1527,
    1538,  1539,  1543,  1544,  1550,  1551,  1555,  1557,  1561,  1563,
    1567,  1568,  1572,  1573,  1577,  1578,  1579,  1583,  1585,  1600,
    1601,  1602,  1603,  1605,  1609,  1611,  1615,  1622,  1624,  1626,
    1631,  1632,  1634,  1636,  1638,  1670,  1673,  1678,  1680,  1686,
    1691,  1696,  1707,  1712,  1717,  1722,  1727,  1736,  1740,  1747,
    1749,  1750,  1751,  1757,  1759,  1764,  1765,  1766,  1775,  1776,
    1777,  1781,  1782,  1783,  1792,  1793,  1794,  1799,  1800,  1809,
    1810,  1815,  1816,  1820,  1822,  1824,  1826,  1828,  1832,  1837,
    1838,  1840,  1850,  1851,  1856,  1858,  1860,  1862,  1864,  1867,
    1869,  1871,  1876,  1878,  1880,  1882,  1884,  1886,  1888,  1890,
    1892,  1894,  1896,  1898,  1900,  1902,  1904,  1906,  1908,  1910,
    1912,  1914,  1916,  1918,  1920,  1922,  1924,  1926,  1928,  1930,
    1935,  1936,  1940,  1947,  1948,  1954,  1955,  1957,  1959,  1961,
    1966,  1968,  1973,  1974,  1976,  1978,  1983,  1985,  1987,  1989,
    1991,  1993,  1998,  1999,  2001,  2003,  2008,  2010,  2009,  2013,
    2021,  2022,  2024,  2026,  2031,  2032,  2034,  2039,  2040,  2042,
    2044,  2049,  2050,  2052,  2057,  2059,  2061,  2063,  2064,  2066,
    2071,  2073,  2075,  2080,  2081,  2085,  2086,  2091,  2090,  2095,
    2094,  2102,  2101,  2112,  2111,  2121,  2126,  2127,  2132,  2138,
    2152,  2153,  2157,  2159,  2161,  2167,  2169,  2171,  2173,  2175,
    2177,  2179,  2181,  2187,  2188,  2193,  2195,  2197,  2206,  2208,
    2209,  2210,  2212,  2214,  2215,  2220,  2221,  2222,  2227,  2229,
    2232,  2239,  2240,  2241,  2247,  2252,  2254,  2260,  2261,  2267,
    2268,  2272,  2277,  2280,  2279,  2283,  2286,  2292,  2291,  2300,
    2306,  2310,  2312,  2317,  2319,  2321,  2323,  2329,  2332,  2338,
    2339,  2341,  2342,  2343,  2345,  2347,  2354,  2355,  2357,  2359,
    2364,  2365,  2371,  2372,  2374,  2375,  2380,  2381,  2382,  2384,
    2392,  2393,  2395,  2398,  2400,  2404,  2405,  2406,  2408,  2410,
    2415,  2417,  2422,  2424,  2433,  2435,  2440,  2441,  2442,  2446,
    2447,  2448,  2453,  2454,  2459,  2460,  2461,  2462,  2466,  2467,
    2472,  2473,  2474,  2475,  2476,  2490,  2491,  2496,  2497,  2503,
    2505,  2508,  2510,  2512,  2535,  2536,  2542,  2543,  2549,  2548,
    2558,  2557,  2561,  2567,  2573,  2574,  2576,  2580,  2585,  2587,
    2589,  2591,  2597,  2598,  2602,  2603,  2608,  2610,  2617,  2619,
    2620,  2622,  2627,  2629,  2631,  2636,  2638,  2643,  2648,  2656,
    2658,  2663,  2664,  2669,  2670,  2674,  2675,  2676,  2681,  2683,
    2689,  2691,  2696,  2698,  2704,  2705,  2709,  2713,  2717,  2719,
    2720,  2721,  2726,  2729,  2728,  2740,  2739,  2751,  2750,  2762,
    2761,  2773,  2772,  2786,  2792,  2794,  2800,  2801,  2806,  2813,
    2818,  2824,  2827,  2830,  2834,  2840,  2843,  2846,  2851,  2852,
    2853,  2857,  2863,  2864,  2874,  2875,  2879,  2880,  2885,  2890,
    2891,  2897,  2898,  2900,  2905,  2906,  2907,  2908,  2909,  2911,
    2946,  2948,  2953,  2955,  2956,  2958,  2963,  2965,  2967,  2969,
    2974,  2976,  2978,  2980,  2982,  2984,  2986,  2991,  2993,  2995,
    2997,  3006,  3008,  3009,  3014,  3016,  3018,  3020,  3022,  3027,
    3029,  3031,  3033,  3038,  3040,  3042,  3044,  3046,  3048,  3060,
    3061,  3062,  3066,  3068,  3070,  3072,  3074,  3079,  3081,  3083,
    3085,  3090,  3092,  3094,  3096,  3098,  3100,  3115,  3120,  3125,
    3127,  3128,  3130,  3135,  3137,  3139,  3141,  3146,  3148,  3150,
    3152,  3154,  3156,  3158,  3163,  3165,  3167,  3169,  3171,  3181,
    3183,  3185,  3186,  3188,  3193,  3195,  3197,  3202,  3204,  3206,
    3208,  3213,  3215,  3217,  3231,  3233,  3235,  3236,  3238,  3243,
    3245,  3250,  3252,  3254,  3259,  3261,  3266,  3268,  3285,  3286,
    3288,  3293,  3295,  3297,  3299,  3301,  3306,  3307,  3309,  3311,
    3316,  3318,  3320,  3326,  3328,  3330,  3333,  3337,  3339,  3341,
    3343,  3377,  3378,  3380,  3382,  3387,  3389,  3391,  3393,  3395,
    3400,  3401,  3403,  3405,  3410,  3412,  3414,  3420,  3421,  3423,
    3432,  3435,  3437,  3440,  3442,  3444,  3458,  3459,  3461,  3466,
    3468,  3470,  3472,  3474,  3479,  3480,  3482,  3484,  3489,  3491,
    3499,  3500,  3501,  3506,  3507,  3512,  3514,  3516,  3518,  3520,
    3522,  3529,  3531,  3533,  3535,  3537,  3540,  3542,  3544,  3546,
    3548,  3553,  3555,  3557,  3562,  3588,  3589,  3591,  3595,  3596,
    3600,  3602,  3604,  3606,  3608,  3610,  3617,  3619,  3621,  3623,
    3625,  3627,  3632,  3634,  3636,  3643,  3645,  3663,  3665,  3670,
    3671
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

#define YYPACT_NINF (-1787)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-861)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
      91, 10734,   172,   186, 15274,   133, -1787, -1787, -1787, -1787,
   -1787, -1787, -1787, -1787, -1787, -1787, -1787,   135,   720,   231,
   -1787, -1787, -1787, -1787, -1787, -1787, -1787, -1787, -1787, -1787,
   -1787, -1787, -1787, -1787, -1787, -1787, -1787, -1787, -1787, -1787,
   -1787, -1787, -1787, -1787, -1787, -1787, -1787,    35,   288, -1787,
   -1787, -1787, -1787, -1787, -1787,  3977,  3977,   265, 10734,   321,
     346, -1787, -1787,   356, -1787, -1787, -1787, -1787, -1787, -1787,
   -1787, -1787, -1787,  1528, -1787,   649,   307, -1787, -1787, -1787,
   -1787, -1787, 15124, -1787, -1787,   333,   387,   249,    86, -1787,
    3977,   387,   387,   387,   384,  4389,   589,   778, 10893, -1787,
   -1787, -1787, 14974,  1809, -1787, -1787, -1787,  2616,   598,  6578,
    1095,  1020,  2616,  1188,   428, -1787, -1787, -1787, -1787,   537,
   -1787, -1787, -1787, -1787,   460, -1787, -1787, -1787, -1787, -1787,
     469,   492,   537, -1787,   537,   495, -1787, -1787, -1787, 15830,
    3977, -1787, -1787,  3977, -1787, 10734,   461, 15882, -1787, -1787,
    4612, 16833, -1787,   752,   752,   509,  2486, -1787, -1787, -1787,
   -1787,   106, 13584,  3139,   537, -1787, -1787, -1787, -1787, -1787,
   -1787,   532, -1787,   519,   556,   564, -1787,   584, 18562, 14204,
    2865,  1528,   -25,   567,   594,   609,   612,   620,   622, -1787,
   -1787, 16032, 10085,   586, -1787, 15417, -1787, -1787, -1787, -1787,
     629, -1787, -1787,   626, -1787, 17698,   771, 17914, -1787,   659,
    3977,   492,   665,   643,   679,   708, -1787, -1787, -1787,  2974,
    3725,   710,   758,    12, -1787, -1787,   537,   537,    58,    65,
     263,    58, -1787,   537,   537, -1787,  4353, -1787, -1787,   724,
     748,   752, 13162, -1787, -1787, 15124, -1787, -1787,  2616, -1787,
    2923,   428,   700,   798,    65,  3977,   249, -1787, 12442, -1787,
     752,   752,   743,   798,    65,  3977, -1787,  6418, -1787, -1787,
     752, -1787,   752, -1787,   696,  3368,  3977, -1787,  2382,   777,
   -1787, -1787, -1787, 15576,   492,    95, -1787, -1787, 16883, -1787,
     758,   101, -1787, 18562, 16833,  3229,  4353, -1787,   279, -1787,
   -1787, -1787, 15882,  3977, -1787,   786, -1787, -1787, -1787, -1787,
    3977,  2173,   512,   527, -1787,  3977,   519, -1787,   591,   537,
     781, 16084,   800, 13742, 13320,  2616,  2616, -1787,  2616,   752,
    2616,   752, -1787, -1787,   537, -1787,   803, -1787, 16234, -1787,
   -1787, -1787, 16286,   629, -1787,   791,   391,  1305,   794,   428,
     802, -1787,  2486,   801,   519,  2486,  2363, -1787,   811,   868,
   18634,   816,   836, 18562, 18706,   839, -1787, -1787, -1787, -1787,
   -1787, -1787, -1787, 18778, 18778, 14050,   857,  4686, -1787, -1787,
   -1787, -1787,   867, -1787,   881, -1787,  2066, -1787, 18562, 18562,
   -1787,   849,   409,   654,   747,   562,   784,   896,   893,   879,
     940,    26, -1787,   638, -1787,   944, -1787,   817,  3316, 14512,
   -1787, -1787,   902,   944, -1787, -1787,   677, -1787, -1787,  2865,
     957,   959,   965,   967,   974,   981, -1787, -1787,   296,   990,
   -1787,   684,   990, -1787, -1787, 15830, -1787,   912,   983, 14666,
   -1787, -1787,  4208,  4000,  1015, 13742,  1018,   735,   984, -1787,
   -1787, -1787, -1787, -1787,  3977,  4721, -1787, -1787, -1787, -1787,
   -1787, -1787,   995,  3075,   857, 17698,  1002,  1011, -1787, -1787,
    1027, 17914,   635, -1787, -1787, -1787, 17986,  1026, -1787, -1787,
   -1787, -1787, -1787,  2974,   709,  1033,  1044,  1054,   733,  1091,
    1097,  1119,  3725, -1787, -1787,   537,  1083,   249,  1126, -1787,
   -1787,  1131, -1787, -1787,   492,   798, -1787, -1787, -1787,   492,
   -1787, -1787,  4353, -1787, 14512, 14512, -1787,   752,  4612, 17611,
   13584, -1787, -1787, -1787, -1787, -1787,   492,   798,   101, -1787,
   -1787,  2616,  1129,   798,    65, -1787,   492,   798, -1787,  9138,
   -1787,   752,   752, -1787, -1787,  1137,   414,  1144,   428,  1149,
   -1787, 17041, -1787,   713, -1787,  1223, 17507, -1787,  4612,  9088,
   13162, -1787, 15576, 18850, -1787, -1787, -1787, -1787, -1787,  3229,
     756,  4353, -1787, 13584,   758, 10734, -1787,  1161, -1787,  1173,
   -1787, -1787, -1787, -1787, -1787,  2486, -1787, -1787,  1250,  3710,
   16286, 10085, -1787, 16436, -1787,   752,   752, -1787, -1787,   629,
   -1787,  1012,  1175,  1314, 18562,   797,  1131,  1172, -1787,   537,
     537, -1787,   990, -1787, 16084, -1787, -1787, 17322,   752,   752,
   -1787,  3710,   537, -1787, 16690, -1787, -1787, 16234, -1787,   106,
    1194,   256,  1195,  1305,   739, 15882,   769, -1787, -1787, -1787,
   -1787, -1787, -1787,   783, -1787,  1205,  1182, -1787, 14358, -1787,
   16488, 16488, -1787, 14358, -1787, 18562, 14358, -1787, -1787, 15628,
   16488, 16488,   817,  1571,  1760,   491,  1854, -1787,   789,  1224,
     918,  1231, -1787, 17986, 18562, 18058,  1228,  2382,  2382, -1787,
    1678, -1787, -1787, 18130,  1910, 18562, 18130,  2382, -1787, -1787,
   18562, 18562, 18562, 18562, 18562, 18562, 18562, 18562, 18562, 18562,
   18562, 18562, 18562, 18562, 18562, 18562, 18562, 18562, 18562, 18202,
    1214,   584,  3152, 10085, -1787, -1787, -1787, -1787, -1787, -1787,
   -1787, -1787, -1787, -1787, -1787,  1243, 18562, -1787, -1787,   902,
     978, -1787, -1787,   537,   537, -1787, -1787, 14512, -1787,   328,
     990, -1787,   792,   990, -1787, -1787, -1787,  1131, -1787, -1787,
    1131, 18922, -1787, -1787, 10085,  1215,  1269,  2244,  1411,  3125,
     355,  1172, -1787,   537,   537,  1172,   365, -1787,   537,   537,
   18562,  3977,  1035,  1043,  1172,   188, 13110, 13110,  3977, -1787,
   -1787, 18562,  1027, -1787, 17698,  1289, -1787,   738, -1787, -1787,
   -1787, -1787, -1787,   850, -1787, 13110,  2382,  4612,  2382,   854,
    1290,  1291,  1294,   855,  1295,  1296,  1297,   376,   990, -1787,
   -1787,   390,   990, -1787, -1787, -1787,  4612,   584, -1787,   990,
   18922, -1787,   492, 17041, -1787, -1787,   858,  1303,   875,  1306,
   -1787,  1312, -1787,   492, -1787, -1787,   492,   798,  1312, -1787,
     492,  1307,  1308,  1309, -1787, -1787, 17322, -1787,  1318, -1787,
   -1787, -1787,  2382,  3977,  9416,  1395,  1298, 17409, -1787,   983,
   -1787, 13110,   888, -1787, -1787,  1312, -1787, 15882, 14512,  1301,
   -1787,  1301, -1787, -1787, -1787, -1787, 16234, -1787, 10247, 14820,
   -1787, 17041,  1324,  1325,  1326, -1787,  8365,   537, -1787,   797,
   -1787, -1787, -1787, -1787,  1131, -1787, -1787, -1787,   752, -1787,
    3042, -1787, -1787,   428,  2021,  1331, -1787, 17914, -1787,  1305,
    1194, -1787, -1787,  1323,  1332,  2363, 18130, -1787,  1333,   307,
    1327,  1337,  1338,  1342,  1341, 18562,  1347,  1348,  1349, 10085,
   18562, -1787, -1787,  1970, -1787, -1787, -1787, 18562, -1787,  1350,
    1354, 17770,  1069, -1787, 18130, -1787, -1787, -1787,  3529, -1787,
   -1787,   889, -1787, -1787, -1787, -1787,  3529, -1787, -1787,  1075,
     442, -1787, -1787,   849,   849,   849,   409,   409,   654,   654,
     747,   747,   747,   747,   562,   562,   784,   896,   893,   879,
     940, 18562,  1081, -1787,  1355,  3529, -1787, -1787, 17698, -1787,
   17041,  1358,  1360,  1361,   978, -1787, -1787, -1787, -1787, -1787,
   -1787, -1787, -1787,  1131, -1787, -1787,  1131, 17041, 17041, -1787,
   -1787,  2244,   779,  1362,  1363,  1364,  1368,  2414,  3125, -1787,
   -1787, -1787, -1787, -1787, -1787, -1787, -1787, -1787, -1787, -1787,
   -1787, -1787, -1787,  1339, -1787,  1172, -1787, -1787, -1787, -1787,
   -1787, -1787, -1787, -1787,  1370,  1374, -1787,   249,  3529,  1100,
     102, -1787, -1787,  1378, -1787, 17914, -1787, 18562, -1787, 18274,
   13110, -1787, -1787, -1787,  1356,   397,   990, -1787,   429,   990,
   -1787, -1787, -1787, -1787,  1131, -1787, -1787, -1787,  1131,   758,
    1376,  1131, -1787, -1787, -1787, -1787, -1787, -1787, -1787,  1382,
   -1787, -1787,  1312, -1787,   492, -1787, -1787, -1787, -1787, -1787,
   11523,  1383,  1380, -1787,    87, -1787,   396,    99,  9923,  1384,
   12939,  1388,  1404,  2307,  2655,  1440, 18346,  1406, -1787, -1787,
    1407,  1409, -1787, -1787,   492, 18562, 18562,  1547,  1405,   477,
   -1787,  1491,  1412,  1390, -1787, -1787, -1787,  9243, -1787, -1787,
   -1787, -1787, -1787,  2060, -1787, -1787, -1787,  1479, -1787, -1787,
   -1787,  2382, -1787, -1787, 11370, 15124,  1416, -1787,  3977, -1787,
    1399,  1420,  1421, -1787,  1102, -1787, -1787, -1787, -1787,  4612,
   -1787, -1787,  1422,  1423,   891, 15882,   519,   519, -1787, -1787,
     857,   983, 14666, -1787,   944, -1787, 10409, -1787,   436,   990,
   -1787,   752,  8800, -1787, -1787,  1305,   537,   537,   106,   256,
   -1787, -1787,  1194,  1431,  1446, -1787, -1787,   894,   488, 10085,
    2382, -1787,   488, 15680,   488, -1787, 18562, 18562, 18562, -1787,
   -1787, -1787, -1787, 18562, 18562,  1444, 17698, -1787, -1787,  1450,
     552, -1787,  3445, -1787, -1787,  1107, -1787,   230, -1787, 18130,
    1118, -1787, 17986, -1787, -1787, 18562,  1433,  1121,  1124,  1027,
   -1787,   464,   990, -1787, -1787, 17041, 17041, -1787, -1787,  1457,
     540,   990, -1787,   555,  1252,   537,   537, -1787, -1787, 17041,
   17041, -1787,  1458, -1787, 13584, 13584,  1460,  1462,  1464,  1470,
   -1787,  1468, 18562, 18562,  1128,  1459, -1787, -1787, -1787, -1787,
   -1787, -1787,  1475, 18562, -1787, -1787, -1787,  1131, -1787, -1787,
   -1787,  1131, 17041, 17041,   249,   537,  1134,  1480,  1482, -1787,
   -1787,  1494, 11676, 11829, 11982, 15882, 16488, 16488,  1495, -1787,
    1469,  1471,  2667, 12284, -1787,    96,  3977, -1787, -1787,  3977,
   -1787, 17842,   311,   379, -1787, -1787, -1787, -1787, 18562,  1497,
    1570,  9760,  8628, -1787,  1477, -1787,  1484, 18562,  1485, 17698,
    1488, 18562, 17986, 18562,   768, -1787,  1492,   -17, -1787,   113,
    1503, -1787, -1787,  1519, -1787,  1500, -1787,  1502,  1533, 12939,
      45, 12600,   537,   222, -1787, -1787, -1787,  1532, -1787,  1536,
   -1787,  1538, -1787,  1540, -1787,  1541, -1787, -1787, -1787, -1787,
   10571,  1539,  1544,  1545, -1787,  1549, -1787, -1787, -1787,  1131,
   18562, 18562,   983,  1548, -1787,  1194, -1787,  1530,   247, -1787,
    1555, -1787, -1787, 15882, -1787,  1557,  1556,   925, -1787,  1559,
   -1787, -1787, -1787, -1787, -1787, 17698,  1027, 17986, -1787,  1586,
    3529, -1787,  1586,  1586, -1787,  3529,  3881,  4115, -1787, -1787,
    1145, -1787, -1787, -1787,  1561,  1565, -1787, -1787, -1787,  1131,
   -1787, -1787,  1568,  1573,   537, -1787, -1787, -1787,  1131, -1787,
   -1787, -1787,  1575, -1787, -1787, -1787, -1787, -1787, -1787, -1787,
   -1787, -1787, -1787, -1787, -1787, -1787,  1567, -1787, -1787, -1787,
   -1787,  1574,  1578,   537, -1787, 17041, 17041, -1787, -1787, -1787,
   -1787, 18562, -1787, -1787,  1582, -1787,  1495,  1495,  1495,  1092,
    1566,   310, -1787,  3811,   341, 14512, -1787, -1787, -1787,  3695,
   18562,  5022,   354, -1787, -1787,   107,  1585,  1585,  3977, -1787,
   -1787, 17190, -1787, 18562,  1591,  1596, -1787, -1787, -1787, -1787,
     934,  1599, 12939,  1412,  1598, 18562,   333,  1597,   384, 12141,
   15882, 12939, 18562, 18562,   496,   323, -1787, 18562, -1787, -1787,
     361, -1787,  1027, -1787,   943,   973,   979, -1787, -1787, -1787,
   -1787,   492,   768,  1602, -1787, -1787, 18562, -1787,  1605,   584,
    9923, -1787, -1787, -1787, -1787, 18562,  1649, -1787, 12939, -1787,
     537, 13584, -1787, -1787, 15882, -1787, -1787, -1787, -1787, -1787,
    1603, -1787, 17041, -1787, -1787,  1604, -1787,  1611,  1622,  1623,
    1305, -1787, -1787, -1787, -1787, 18562, -1787, 15680, 18562,  1027,
    1645,  1155, -1787,  1158, -1787,  3529, -1787,  3529, -1787, -1787,
   -1787, -1787, 17041,  1608,  1643, -1787, -1787, 17041, 17041,  1646,
    1648,  1180, 13268, 13426, -1787,  1642, -1787, -1787, -1787, -1787,
    1651,  1653,  1184, -1787, -1787, -1787, -1787,  1092,  1059,   367,
   -1787, -1787, -1787, -1787,   537,   537, -1787, -1787, -1787,   399,
   -1787,   989,  3695,   576, -1787,  5022,   537, -1787, -1787, -1787,
   -1787, -1787, -1787, -1787, -1787,   401, 12939,   215, 18418, -1787,
   12939,  1412, 13900, -1787,  1412,  1631, -1787, -1787, -1787, -1787,
   17619, 18562, 12939,  9588,  1632, -1787,  1659,   437, 12939, -1787,
   -1787,  1662, -1787, -1787,  1634,   584,   382,  1671,  1673,  1185,
    1738, -1787, -1787, -1787,  3977,  4612, -1787, -1787,  1676,  1680,
   -1787, -1787, -1787,  1305,  1194,  1679, -1787, -1787, -1787,  1684,
   -1787, -1787, -1787,  1189,  1212, -1787, -1787, -1787, -1787, -1787,
   -1787, -1787, -1787, -1787, -1787,  1690, -1787, -1787,  1691,  1694,
   -1787, -1787, -1787,  1695,  1699,  1701,  1059, -1787,   537, -1787,
   -1787, -1787, -1787, -1787,  1702,  3811, -1787, -1787, 18562,  1698,
   -1787, -1787, 12703, -1787,  1685,  1005,  1785, 18562,  1708, 18562,
    1288,  1687,   283,  1801, -1787, 18562,  1703, -1787, -1787, -1787,
   -1787, 16638, -1787,  1719,  1706,   203, 12939, -1787, 18562, 18130,
     298, -1787, -1787, -1787,  1725, -1787, -1787,  1194,  1739, -1787,
   -1787, -1787, -1787,  1735,  1737,  1741, 13584,  1734, -1787, -1787,
     577,   990, -1787, -1787,  1092, -1787,   116, -1787,  1218, -1787,
   -1787, 11052, -1787, -1787, -1787,    11, 12939, -1787,  1412,  1740,
    1744, 18562, 18562, 18562, 12939, -1787, -1787, 11052, 16638, -1787,
    4080, 16436,  2382,  1742, -1787,  1794,  1751,   550,  1747, -1787,
    1830, -1787,  1025, 12939,  1755, 12939, 12939, -1787,  1757, -1787,
   -1787, -1787, -1787, -1787, -1787, -1787, -1787,  1131, -1787, 18562,
   18562, -1787,  1302, 11211, -1787, 12939, -1787, -1787,  1743,  1749,
     318, -1787,  1412, -1787,  1302, -1787,  1745,  3618,  3431, -1787,
   -1787, -1787,   203,  1758, 18562,  1750,   203,   203, 12939, -1787,
   -1787, 18562,  1808,  1811, -1787, 17041, -1787, -1787, 12703, -1787,
    1302, -1787, -1787, 18562, 18490, 18562, -1787,  1745, 18562,  1769,
    3431,  1767,   584,  1775, -1787,   590, -1787, -1787,  1032,  1738,
     271, -1787, -1787, 12821,  1780, 12703,  1412, -1787,  1412,  1412,
    1782,  1784, -1787,   492,   584,  1787, -1787,  1759,   584, -1787,
   -1787, 12939,  1858,  1786, -1787, -1787, 12821, -1787,   492, -1787,
   -1787,  1225, 18562, -1787,  1041, -1787, 12939, -1787, -1787,   584,
    2382,  1789,  1772, -1787, -1787, -1787,  1055, -1787, -1787,  1774,
    2382, -1787, -1787
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,   407,     0,     2,   407,   424,   425,   426,   427,   428,
     429,   430,   431,   413,   415,   414,   416,     0,     0,     0,
     432,   434,   455,   435,   456,   438,   439,   453,   454,   433,
     451,   452,   436,   437,   440,   441,   442,   443,   444,   445,
     446,   447,   448,   449,   450,   457,   458,   744,   460,   533,
     534,   537,   539,   535,   541,     0,     0,     0,   407,     0,
       0,    16,   504,   510,     9,    10,    11,    12,    13,    14,
      15,   708,    93,     0,    19,     0,     2,    91,    92,    17,
      18,   760,   407,   709,   356,     0,   359,   634,   361,   370,
       0,   360,   390,   391,     0,     0,     0,     0,   487,   409,
     411,   417,   407,   419,   422,   472,   459,   395,   465,   470,
     396,   482,   397,   497,   501,   507,   486,   513,   525,   744,
     530,   531,   514,   580,   362,   363,     3,   710,   723,   412,
       0,     0,   744,   782,   744,     2,   799,   800,   801,   407,
       0,   958,   959,     0,     1,   407,     0,   407,   379,   380,
       0,   487,   401,   402,   403,   713,     0,   536,   538,   540,
     542,     0,   407,     0,   745,   746,   532,   461,   627,   628,
     626,   687,   682,   672,     0,     0,   711,     0,     0,   407,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   505,
     508,   407,   407,     0,   960,   487,   789,   807,   964,   957,
     955,   962,   355,     0,   155,   640,   154,     0,   364,     0,
       0,     0,     0,     0,     0,     0,   354,   859,   860,     0,
       0,   389,   742,   744,   738,   763,   744,   744,   740,     2,
     744,   739,   820,   744,   744,   817,     0,   480,   481,     0,
       0,   407,   407,   424,     2,   407,   371,   410,   420,   473,
       0,   502,     0,   726,     2,     0,   634,   372,   487,   466,
     483,   498,     0,   726,     2,     0,   423,   467,   474,   475,
     484,   489,   499,   503,     0,   517,     0,   702,     2,     2,
     724,   781,   783,   407,     0,     2,     2,   968,   487,   971,
     742,   742,     3,     0,   487,     0,     0,   382,   744,   740,
     739,     2,   407,     0,   706,     0,   668,   670,   669,   671,
       0,     0,   664,     0,   654,     0,   663,   674,     0,   744,
       2,   407,   979,   408,   407,   419,   398,   465,   399,   490,
     400,   497,   494,   515,   744,   516,     0,   615,   407,   616,
     933,   934,   407,   617,   619,   504,   510,     0,   581,   582,
       0,   747,     0,   685,   673,     0,   751,    21,     0,    20,
       0,     0,     0,     0,     0,     0,    23,    25,     4,     8,
       5,     6,     7,     0,     0,   407,     2,     0,    94,    95,
      96,    97,    78,    24,    79,    36,    77,    98,     0,     0,
     113,   115,   119,   122,   125,   130,   133,   135,   137,   139,
     141,   143,   146,     0,    26,     0,   511,     2,    98,   407,
     147,   679,   630,   501,   632,   678,     0,   629,   633,     0,
       0,     0,     0,     0,     0,     0,   761,   787,   744,   797,
     805,   809,   815,     2,   966,   407,   969,     2,    91,   407,
       3,   614,     0,   979,     0,   408,   465,   490,   497,     3,
       3,   596,   600,   610,   616,   617,     2,   790,   808,   956,
       2,     2,    23,     0,     2,   640,    24,     0,   638,   641,
     977,     0,     0,   647,   636,   635,     0,     0,   728,     2,
       2,     2,     2,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   766,   823,   744,     0,   634,     2,   762,
     770,   886,   764,   765,     0,   726,     2,   819,   827,     0,
     821,   822,     0,   385,   407,   407,   471,   408,     0,   487,
     407,   961,   965,   963,   488,   706,     0,   726,   742,   365,
     373,   421,     0,   726,     2,   706,     0,   726,   683,   468,
     469,   485,   500,   506,   509,   504,   510,   528,   529,     0,
     684,   407,   624,     0,   191,   348,   407,     3,     0,   487,
     407,   725,   407,     0,   367,     2,   368,   703,   387,     0,
       0,     0,     2,   407,   742,   407,   706,     0,     2,     0,
     667,   666,   665,   660,   418,     0,   658,   675,   463,     0,
     407,   407,   935,   408,   404,   405,   406,   939,   930,   931,
     937,     2,     2,    92,     0,   895,   909,   979,   891,   744,
     744,   900,   907,   622,   407,   495,   618,   408,   491,   492,
     496,     0,   744,   945,   408,   950,   942,   407,   947,     0,
     977,   587,     0,     0,     0,   407,     0,   759,   758,   754,
     756,   757,   755,     0,   749,   752,     0,    22,   407,    85,
     407,   407,    80,   407,    87,     0,   407,    83,    84,   407,
     407,   407,     2,    94,    95,     0,     0,   173,     0,     0,
     531,     0,   955,     0,     0,     0,     0,     0,     0,    46,
       0,    52,    53,    57,     0,     0,    57,     0,    81,    82,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   407,   156,   157,   158,   159,   160,   161,
     162,   163,   164,   165,   166,   154,     0,   152,   153,     2,
     871,   631,   868,   744,   744,   876,   512,   407,   788,   744,
     798,   806,   810,   816,     2,   791,   793,   795,     2,   811,
     813,     0,   967,   970,   407,     0,     0,     2,    92,   895,
     744,   979,   841,   744,   744,   979,   744,   856,   744,   744,
       3,   618,     0,     0,   979,   979,   407,   407,     0,     2,
     649,     0,   977,   646,   978,     0,   642,     0,     2,   645,
     648,   170,   169,     0,     2,   407,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   744,   775,   779,
     818,   744,   832,   837,   767,   824,     0,     0,   393,   883,
       0,   729,     0,   407,   730,   386,     0,     0,     0,     0,
     384,     2,   731,     0,   369,   706,     0,   726,     2,   732,
       0,     0,     0,     0,   543,   603,   408,     3,     3,   607,
     606,   802,     0,     0,   407,   349,     0,   487,     3,    91,
       3,   407,     0,     3,   707,     2,   662,   407,   407,   656,
     655,   656,   464,   462,   581,   941,   407,   946,   408,   407,
     932,   407,     0,     0,     0,   910,     0,   744,   980,   896,
     897,   623,   893,   894,   908,   936,   940,   938,   493,   528,
       0,   944,   949,   584,   978,     0,   154,     0,   583,     0,
     977,   688,   686,     0,     0,   751,    57,   712,     0,     2,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   407,
       0,   112,   111,     0,   108,   107,    27,     0,    28,     0,
       0,     0,     0,     3,    57,    42,    43,    50,     0,    49,
      61,     0,    58,    59,    62,    45,     0,    44,    48,     0,
       0,    41,   114,   116,   117,   118,   120,   121,   123,   124,
     128,   129,   126,   127,   131,   132,   134,   136,   138,   140,
     142,     0,     0,   358,     0,     0,    29,     3,   640,   148,
     407,     0,     0,     0,   872,   873,   869,   870,   681,   680,
       2,   792,   794,   796,     2,   812,   814,   407,   407,   888,
     887,     2,     0,     0,     0,     0,     0,   744,   896,   844,
     861,     2,   839,   847,   620,   842,   843,   621,     2,   854,
     864,   857,   858,     0,     3,   979,   377,     2,   972,     2,
     611,   612,   590,     3,     3,     3,     3,   634,     0,   146,
       0,     3,     3,     0,   643,     0,   637,     0,   727,     0,
     407,     3,   381,   383,     0,   744,   776,   780,   744,   833,
     838,     2,   768,   771,   773,     2,   825,   828,   830,   742,
       0,   884,     3,   734,     3,   477,   476,   479,   478,     2,
     707,   735,     2,   733,     0,   707,   736,   543,   543,   543,
     407,     0,     0,   625,     0,   352,     0,     0,   407,     0,
       2,     0,     0,     0,     0,     0,   175,     0,   282,   283,
       0,     0,   321,   320,     0,   150,   150,   327,   504,   510,
     189,     0,   176,     0,   199,   177,   178,   407,   193,   179,
     180,   181,   182,     0,   183,   184,   288,     0,   185,   186,
     187,     0,   188,   195,   487,   407,     0,   197,     0,   346,
       0,     0,     0,     3,     0,   714,   707,   695,   696,     0,
       3,   691,     3,     3,     0,   407,   672,   672,   943,   948,
       2,    91,   407,     3,   502,     3,   408,     3,   744,   903,
     906,   407,     3,   892,   898,     0,   744,   744,     0,   587,
     572,   588,   977,     0,     2,   748,   750,     0,    86,   407,
       0,    90,    88,   407,     0,   102,     0,     0,     0,   106,
     110,   109,   174,     0,     0,     0,   640,    99,   167,     0,
       0,    75,     0,    75,    75,     0,    63,    65,    40,     0,
       0,    38,     0,    39,   145,     0,     0,     0,     0,   977,
       3,   744,   879,   882,   874,   407,   407,     3,     3,     0,
     744,   850,   853,   744,     0,   744,   744,   845,   862,   407,
     407,   973,     0,   613,   407,   407,     0,     0,     0,     0,
     366,     3,     0,     0,     0,     0,   639,   644,     3,   172,
     171,     3,     0,     0,     2,   769,   772,   774,     2,   826,
     829,   831,   407,   407,   634,   744,     0,     0,     0,   707,
     737,     0,   407,   407,   407,   407,   407,   407,   526,   554,
       3,     3,   555,   487,   544,     0,     0,   784,     2,     0,
     350,    57,     0,     0,   273,   274,   196,   198,     0,     0,
       0,   407,   407,   269,     0,   267,     0,     0,     0,   640,
       0,     0,     0,     0,     0,   151,     0,     0,   328,     0,
       0,     3,   202,     0,   194,     0,   264,     0,     0,     2,
       0,   487,   744,     0,   347,   890,   889,     0,     2,     0,
     698,     2,   693,     0,   694,     0,   676,   657,   661,   659,
     407,     0,     0,     0,     3,     0,     2,   899,   901,   902,
       0,     0,    91,     0,     3,   977,   577,     0,   587,   585,
       0,   575,   689,   407,   753,     0,     0,     0,    32,     0,
     103,   105,   104,   101,   100,   640,   977,     0,    56,    72,
       0,    66,    73,    74,    51,     0,     0,     0,    60,    47,
       0,   144,   357,    30,     0,     0,     2,   875,   877,   878,
       3,     3,     0,     0,   744,     2,   846,   848,   849,     2,
     863,   865,     0,   840,   855,     3,     3,   974,     3,   598,
     597,   601,   976,     2,     2,   975,     0,     3,   741,   650,
     651,     0,     0,   744,   388,   407,   407,     3,     3,   394,
     743,     0,   834,   718,     0,   720,   526,   526,   526,   561,
     531,     0,   567,   555,     0,   407,   518,   553,   549,     0,
       0,     0,     0,   556,   558,   744,   569,   569,     0,   550,
     565,   407,   353,     0,     0,    58,   277,   278,   275,   276,
       0,     0,     2,   211,     0,     0,   213,   361,   212,   487,
     407,     2,     0,   175,   243,     0,   238,   175,   270,   268,
       0,   262,   977,   271,     0,     0,     0,   309,   310,   311,
     312,     0,   302,     0,   303,   279,     0,   280,     0,     0,
     407,   204,   192,   266,   265,     0,   300,   319,     2,   351,
     744,   407,   716,   677,   407,     2,     2,   951,   952,   953,
       0,   904,   407,     3,     3,     0,   912,     0,     0,     0,
       0,   586,   574,     3,    89,     0,    31,   407,     0,   977,
       0,     0,    76,     0,    64,     0,    70,     0,    68,    37,
     149,   880,   407,     0,     0,   785,   803,   407,   407,     0,
       0,     0,   407,   407,   653,     0,   374,   376,     3,     3,
       0,     0,     0,   722,   522,   524,   520,     0,   919,     0,
     562,   924,   564,   916,   744,   744,   548,   568,   552,     0,
     551,     0,     0,     0,   571,     0,   744,   545,   559,   570,
     560,   566,   605,   609,   608,     0,     2,     0,     0,   229,
       2,   214,   487,   235,   244,     0,   259,   260,   261,   258,
     247,     0,     2,   407,     0,   263,     0,     0,     2,   286,
     313,     0,   304,     2,     0,     0,     0,     0,   291,     0,
     287,   190,   375,   692,     0,     0,   954,     3,     0,     0,
     911,   913,   576,     0,   977,     2,    35,    33,    34,     0,
      54,   168,    67,     0,     0,     3,   786,   804,     3,     3,
     851,   866,   378,     2,   595,     3,   594,   652,     0,     0,
     777,   835,   885,     0,     0,     0,   920,   921,   744,   547,
     917,   918,   546,   527,     0,     0,   203,   285,     0,     0,
       2,   222,     2,   205,     0,     0,   230,   175,   252,     0,
     248,     0,   245,   236,   239,   175,     0,     2,   207,   284,
       2,   407,   281,     0,     0,   329,     2,   289,     0,    57,
       0,   301,   697,   699,     0,   914,   915,   977,     0,   690,
      55,    71,    69,     0,     0,     0,   407,     0,   778,   836,
     744,   927,   929,   922,     0,   557,   215,   218,     0,   217,
     221,   407,   224,   223,   232,     0,     2,   240,   249,   260,
     258,     0,   175,     0,     2,   242,   272,   407,   407,     3,
     314,   408,   318,     0,   322,     0,     0,     0,   330,   331,
     209,   292,     0,     2,     0,     2,     2,   905,     0,   579,
     881,   852,   867,   599,     2,   923,   925,   926,   563,     0,
       0,   220,   225,   407,   342,     2,   233,   231,   254,   253,
     250,   241,   246,   237,   225,     3,   307,     0,   919,   315,
     316,   317,   329,     0,     0,     0,   329,     0,     2,   290,
     297,     0,   294,   296,   578,   407,   216,   219,     2,     3,
     226,   343,   234,     0,     0,     0,     3,   307,     0,     0,
     920,     0,     0,     0,   323,     0,   332,   210,     0,   287,
       0,     3,   200,   227,     0,     2,   256,   257,   255,   251,
       0,     0,   308,     0,   335,     0,   333,     0,   335,   293,
     295,     2,     0,     0,   201,   206,   228,   208,     0,   305,
     336,     0,     0,   324,     0,   298,     2,   928,   306,     0,
       0,     0,     0,   299,   337,   338,     0,   334,   325,     0,
       0,   326,   339
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1787,  5472,  5237, -1787,    -1,   381,   775,   158, -1787,  1581,
   -1787,   336, -1787,  -676,   621,   712,  -920, -1061, -1787,   157,
    6344,  1852, -1787,  1746, -1787,  1299,   149,   699,   702,   515,
     705,  1251,  1257,  1260,  1256,  1259, -1787,  -162,  -135,  7637,
     847, -1787,  -397, -1787, -1787,  -642,   972, -1016,  2980, -1787,
     248, -1787,   837,    33, -1787, -1787, -1787,   406,   100, -1787,
   -1680, -1786,   291,    98, -1787, -1787, -1787,   301,   220, -1787,
   -1787, -1787, -1787,    55, -1660,   206, -1787, -1787,    69, -1787,
   -1787, -1787,    82,   448,   450,   165, -1787, -1787, -1787, -1787,
    -695, -1787,   108,    67, -1787,   179, -1787,   -40, -1787, -1787,
   -1787,   869,  -714,  -752, -1282, -1787,    31, -1167,   110,  6928,
    -709,  -683, -1787,  -277, -1787,    18,  -123,   420,  -226,  -224,
    3424,  5698,  -614, -1787,   268,    85,  1553,  2251, -1787,  1980,
   -1787,   312,  4209, -1787, -1787, -1787,    62, -1787, -1787,   148,
     331,  4289,  2753,   -54,  1779,  -297, -1787, -1787, -1787, -1787,
   -1787,  -579,   736,  2556, -1787,  -348,   129, -1787,   531,   272,
   -1787,   212,   721, -1787,   522,   -98, -1787, -1787, -1787,  4340,
    -631, -1128,  -670,  -494,  -454,  1375, -1787, -1145,  -111,  1478,
    2165,   895,  7426,    64,  -500,  -248,  -163,  -436,  1261, -1787,
    1583,    88,  1169,  1473, -1787, -1787, -1787, -1787,   245,  -148,
     -95,  -836, -1787,    22, -1787, -1787,   641,   472, -1787, -1787,
   -1787,  2062,  -711,  -477,  -949,   -28, -1787, -1787, -1787, -1787,
   -1787, -1787,   790,  -809,  -149, -1608,  -169,  3901,   -69,  5930,
   -1787,  1151, -1787,  1024,  -187,  -212,  -207,  -191,     8,   -68,
     -50,   -49,   514,    -3,     6,    20,  -177,   -73,  -172,  -159,
    -158,  -718,  -708,  -680,  -677,  -703,   -96,  -672, -1787, -1787,
    -656,  1345,  1351,  1352,  3609,  7178,  -592,  -581,  -567,  -564,
    -691, -1787, -1467, -1585, -1545, -1532,  -596,  -122,  -274, -1787,
   -1787,    -9,   228,   -93, -1787,  6761,    84,   551,  -569
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,  1131,   213,   382,   383,    80,    81,   384,   359,   385,
    1417,  1418,   386,   951,   952,   953,  1235,  1236,  1237,  1429,
     408,   388,   389,   390,   665,   666,   391,   392,   393,   394,
     395,   396,   397,   398,   399,   400,   401,   410,  1050,   667,
    1356,   726,   207,   728,   404,   793,  1132,  1133,  1134,  1135,
    1136,  1137,  1138,  1943,  1139,  1140,  1361,  1534,  1827,  1828,
    1771,  1772,  1773,  1919,  1920,  1141,  1545,  1546,  1691,  1142,
    1143,  1144,  1145,  1146,  1147,  1369,  1709,  1866,  1801,  1148,
    1149,  1562,  1929,  1563,  1564,  1849,  1150,  1151,  1152,  1359,
    1857,  1858,  1859,  1971,  1986,  1882,  1883,   284,   285,   854,
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
      79,   297,   910,    79,   181,   183,   557,   485,   529,   131,
     960,   727,   486,   890,   827,   829,   402,   516,   896,    95,
     882,   190,   231,   184,   185,   354,   615,   670,   487,   782,
     176,   942,  1174,   493,   883,   148,  1240,   884,   891,   322,
     339,  1019,   488,   403,   475,  1157,   289,   489,   831,  1013,
    1800,   336,   935,   497,    79,    79,  1020,    79,   838,  1536,
     490,   491,  1753,   111,   625,  1247,   131,   350,   628,  1023,
     186,  1409,    79,   198,   995,  1030,    95,  1014,  1566,   187,
    1015,    79,    57,   485,  1885,  1016,   103,   513,   486,    79,
     196,  -700,  1833,   188,    79,   592,  1918,    79,   436,   865,
    1350,    79,  1754,   228,   487,    57,   253,  1046,  1918,   493,
     263,    89,   623,   420,   149,  1755,   626,   292,   488,   863,
     111,   564,   566,   489,   278,  1061,   426,  1095,  1281,  1469,
    1470,   421,   422,   256,  1945,   912,   490,   491,   198,    79,
    1153,  1308,    79,   103,    79,   209,  1311,   494,   615,    79,
     483,   183,  -344,   131,  1166,    79,   708,  1567,  1332,  1333,
    1829,   278,    79,    95,   260,   498,   201,   890,    89,   184,
     185,  1431,   144,   209,  1537,  1537,   882,    57,   423,    79,
      79,  1757,  -345,   248,   162,  1886,  -701,   424,   196,   505,
     883,  1163,  1024,   884,    79,  1568,  1027,   496,   709,   598,
     852,   425,   279,   457,   466,  1040,  1041,   111,   246,    79,
      62,    63,   257,   592,   527,  -726,   186,  1379,    79,    79,
     544,   587,  -344,   494,   537,   187,   569,   183,   196,   140,
     103,   201,   140,  1282,   907,    79,   522,   999,  1833,   188,
    1207,   210,  1329,  -726,    79,   184,   185,  1879,   155,   818,
     565,  1518,  -345,   196,    79,    89,   279,    79,    75,   587,
     498,  1330,  1569,   418,    79,  1833,   533,  1283,  1230,   102,
    1519,   800,  1829,  1334,    79,    79,   801,    79,  1202,  1800,
    1601,  1283,  1044,  1044,   156,   858,   140,   538,  1536,  1823,
    1768,  1769,   802,   522,    79,    79,   615,  1194,   550,  1337,
    1267,  1044,    79,  1013,   196,   814,   803,    19,   786,    79,
      79,   804,  1753,   107,    79,  1268,   877,   598,   357,  1221,
     615,  1023,  1045,  1045,   805,   806,   102,   615,  1157,   523,
     140,  1014,   112,    57,  1015,   358,   958,   565,  1254,  1259,
     447,  1045,   753,   531,  1961,   111,    79,   766,  1319,    57,
    1436,    79,  1754,   902,    79,   642,  1855,   800,  1092,  1121,
    1494,   604,   801,   825,   875,  1755,    57,  1044,   530,   830,
     107,  1863,  1770,   140,  1614,  1616,  1618,  1329,   802,   280,
     161,  1309,  1437,  1900,   814,   146,   523,  1368,   895,   112,
    1291,  1320,   803,   204,  1336,   204,  1579,   804,    57,   274,
     198,   901,   204,  1537,  1864,   600,   205,  1045,   248,   190,
     805,   806,   506,   102,   175,   420,   498,  1321,    79,   815,
     906,    96,   206,  1153,   150,    57,  1194,   457,   572,   906,
     882,  1757,   498,   421,   422,    57,   170,   170,   937,   671,
     634,    79,    79,   636,   883,   744,    57,   884,  1843,   498,
     322,   339,   157,    79,    79,   158,   159,   107,   160,   478,
      57,   191,    79,  1823,   466,  1657,  1273,    57,   279,   595,
     177,   170,   618,   937,  1692,   863,   112,  1000,    96,  1693,
     423,   498,    79,  1925,  1658,  1526,   595,  1469,  1470,   424,
     595,    79,   598,   201,   837,   178,  1518,  1398,   815,    57,
     457,   420,   194,   425,  1021,   179,    57,   202,   602,  1666,
    1613,    79,  1768,  1769,  1028,  1660,   937,    79,   602,   421,
     422,   170,  1758,   600,   170,  1071,  1537,  1685,  1667,   498,
     248,  1694,   561,  1794,    57,  1695,   279,   170,  1795,  1075,
     179,  1759,    -3,   498,   348,  1331,  1294,   864,  -860,   615,
     498,   560,  1249,  1528,  1666,    79,   937,    79,   216,   287,
    1319,  1319,  1319,   179,  1405,    96,  1044,   194,    79,   691,
      79,  -523,  1461,  1762,    79,  1766,   692,   693,  1298,   457,
     274,   615,   498,   131,    79,  1396,   896,   595,    79,   602,
    1243,   170,   236,    95,  1787,   107,   197,  1239,   418,   418,
    1440,  -401,  1179,  1320,  1320,  1320,  1045,    57,   402,   229,
      57,   434,   254,  1446,   112,   276,   264,   498,   278,  1049,
      79,  1054,   260,   669,   111,    57,   179,  1535,  1547,  1321,
    1321,  1321,    79,  -628,   293,  1034,   170,   111,  1225,  1686,
    1687,  1688,   930,   580,   248,  1226,   170,    57,  1063,   279,
    1178,   937,  -344,   931,   932,  1524,   547,   170,   544,   552,
     103,  1689,   518,   766,  1537,   521,  -715,  1079,   447,  1661,
    1690,  1873,   581,   582,  1781,  1387,    79,    79,   584,    79,
    1537,   352,   585,    79,   170,    89,    79,   698,   699,  1455,
     311,   170,   170,   498,   197,  1733,   170,  1734,   897,   357,
    1428,  1905,   531,    96,  1459,   355,  1906,  1239,   602,   458,
    1555,    79,   918,   356,   920,   921,  1537,   922,   427,   418,
     924,   447,   521,   926,   927,   928,  1874,   426,   170,   498,
     498,   700,   701,   170,   197,   456,   170,   118,   595,   447,
     118,  1957,   588,   274,  1201,   428,  1958,    13,    14,    15,
      16,    17,   821,   189,    63,   863,    79,   824,    79,   197,
     429,  1837,   595,   430,    13,    14,    15,    16,    17,  1845,
      79,   431,   534,   432,   832,   595,   157,    79,   460,   158,
     159,   461,   160,   466,   839,  1611,    79,   787,   788,   710,
    1426,   789,   474,   711,   118,    79,    79,    79,   480,  1280,
     543,    63,   907,   140,  1197,    57,   322,   339,   476,    13,
      14,    15,    16,    17,   479,    79,   694,   695,   118,  1244,
    1468,   418,    57,   170,   237,   238,  1891,   239,   736,   194,
     560,   240,   737,   748,   481,   170,   170,   498,   118,    61,
     963,   964,   965,   102,    64,    65,    66,    67,    68,    69,
      70,    79,    79,   466,   496,   752,  1535,   525,   278,   879,
     426,   447,   498,   482,   851,   495,  -405,    57,   852,   984,
     696,   697,    95,   514,   107,   118,  1557,  1558,  1559,  1560,
    1561,   118,   506,   118,   810,   209,   498,   107,    74,   615,
     911,   779,  1287,   112,   585,  1049,   669,   515,  1167,    79,
     535,   669,   447,    79,   669,   572,   112,   426,    79,   498,
    1304,   702,   703,  1552,   642,   118,   111,  1644,  1645,  1646,
     913,  1266,   766,   669,   585,   554,    72,   118,  1021,    72,
     426,   248,   602,   458,   914,   590,   863,   146,   915,   103,
     936,  1004,   531,   576,   937,   498,   601,    79,  -859,   601,
     602,  -573,   418,   602,   622,    79,   895,    77,   603,   633,
      77,   603,   646,   248,    89,   650,   170,  1400,  1500,  1724,
     874,   191,   673,   604,   635,  1080,   150,  1168,   118,  1547,
     467,   118,    96,   647,    79,   651,   118,   466,   655,  1609,
      13,    14,    15,    16,    17,    96,    13,    14,    15,    16,
      17,  1058,   899,   278,   506,  1059,   458,   498,   498,  1085,
      79,   907,   673,   937,   170,   690,    79,    79,   504,   118,
    1380,   509,   879,  -402,   595,   677,  1087,   618,   354,   354,
     937,    72,    13,    14,    15,    16,    17,   572,   118,   678,
    1238,   498,  1386,   526,  1239,  1414,   737,    79,    57,  1239,
     706,   729,   705,   536,    57,   498,  1489,   704,   945,   946,
     598,   949,    77,    78,  1538,   957,   590,   673,   961,   707,
    1083,    13,    14,    15,    16,    17,  1606,   447,   939,   940,
    1607,  1091,   140,  1441,  1093,  1677,   322,   339,  1096,   937,
      57,  1326,  1807,   986,  1697,   140,   712,   182,   937,   243,
       6,     7,     8,     9,    10,    11,    12,    72,   738,   466,
     739,   118,    79,    79,    79,  -406,   740,  1884,   741,   223,
     402,   402,  1155,  1862,  1698,   742,    95,   729,  1059,    57,
    1699,   498,   743,  1884,   937,  1516,   466,    -3,    77,    78,
    1763,    72,    79,   433,   737,   118,   770,  1477,  1478,  -404,
      79,   -16,   170,    79,    79,    95,  1835,    79,   -17,   170,
     937,   601,   253,   263,  1471,   602,   107,   783,    79,  1921,
     111,   118,    77,    78,   298,   794,  1909,  1062,  1745,  1064,
    1239,   905,   784,  1959,   807,   112,   256,   937,    72,  1036,
    1037,  -403,  1982,   103,    79,   808,  1979,  1038,  1039,   111,
      13,    14,    15,    16,    17,   809,  1989,  1931,  1647,    79,
    1990,  1935,   498,   970,   971,   972,   973,   260,    89,    77,
      78,    72,   103,  1228,  1059,   466,  1312,  1313,  1314,  1241,
    1242,    79,   817,  1103,   170,   170,   937,  1245,   780,   248,
     467,  1647,   811,   484,   223,   498,  -392,    89,   812,  1517,
     118,   118,    77,    78,  -147,  -147,  1038,  1378,    57,   418,
     298,  1434,  1435,    79,   246,   257,   897,   322,   339,  -392,
     813,   531,  1439,  1435,    96,  1443,  1435,  1419,  1010,  1427,
     819,  1196,  1479,  1427,   286,   170,   835,  1169,  1010,  1491,
     170,   853,   118,  1538,  -521,   822,   118,   485,   118,  1619,
    1059,  -519,   486,  1326,  1326,  1326,   844,  1502,  1326,  1731,
    1059,   118,  1732,  1435,   629,   866,  1516,   833,   487,   570,
     298,    79,   868,   836,   493,    79,   872,   840,    79,   885,
     595,   887,   488,  1053,  1742,  1743,   140,   489,  1752,   937,
    1798,  1799,  1310,  1811,  1435,   604,   148,   668,   466,   904,
     490,   491,   909,    61,   916,  1335,   917,   447,    64,    65,
      66,    67,    68,    69,    70,   140,  1812,  1435,   466,  1009,
      79,   118,  1354,  1880,  1881,   938,  1155,  1768,  1769,   533,
    1979,  1980,   941,   140,   118,   944,   118,   118,   983,   118,
    1432,  1433,   118,   966,   967,   118,   118,   118,   968,   969,
     988,  1264,    74,   111,   111,  1155,    61,   974,   975,   168,
     169,    64,    65,    66,    67,    68,    69,    70,  1669,  1669,
     107,  1388,  1389,  1010,   466,   149,   103,   103,  1017,    79,
    1517,  1686,  1839,  1688,    79,    79,    79,  1056,   494,   112,
    1662,  1065,  1066,   937,  1167,  1067,  1068,  1069,  1070,   107,
     800,    89,    89,  1840,  1086,   801,   531,  1088,   322,   339,
    -704,  1203,  -176,  1158,  1097,  1098,  1099,   760,   112,  -604,
    1471,   802,  1159,   118,  1175,  1188,  1189,  1190,   814,  1200,
    1204,   530,  1210,  1205,  1208,   803,   826,   828,  1211,  1212,
     804,   170,  1214,  1271,   170,   170,   170,  1213,  1216,  1217,
    1218,  1223,    79,   805,   806,  1224,  1246,   799,    79,  1251,
      79,  1252,  1253,  1260,  1261,  1262,   223,    79,   170,  1263,
    1322,  -592,  1471,  1168,   170,  -591,  1286,  1305,    96,  1293,
    -705,   466,   552,  1338,  1327,  1328,   298,  1341,   447,   170,
     466,    61,   298,   140,   168,   169,    64,    65,    66,    67,
      68,    69,    70,  1342,   615,  1351,  1352,    96,  1353,   467,
    1358,  -627,   780,  1360,  1362,  1850,  1803,   937,  1368,   140,
     140,   256,  1372,  1374,  1375,  1376,   170,   466,  1516,  1411,
    1527,  1529,   298,  -112,  -112,  -112,  -112,  -112,  -112,   418,
     118,  1415,   815,   862,  1412,   298,  1382,  1384,    57,    79,
    1347,  1425,   260,   118,   118,  1427,   402,  1442,  1454,  1540,
    1540,  1472,  1467,  1480,    79,  1167,    79,  1473,  1577,  1474,
     668,  1475,  1850,  1435,   248,   668,  1483,  1094,   668,    61,
    1493,  1492,   111,  1826,    64,    65,    66,    67,    68,    69,
      70,   140,  1495,  1507,  1505,  1508,  1331,   668,  1531,   246,
     257,  1548,  1570,   107,   107,   103,   266,    72,  1549,  1551,
     267,    79,  1553,   270,    79,   272,  1565,  1572,   254,   264,
     453,  1419,   112,   112,  1573,   466,  1574,    73,    74,   466,
      89,   982,  1575,  1580,  1582,   485,  1583,  1600,    77,    78,
     486,   466,  1517,  1587,  1168,  1585,  1586,   466,  1588,  1589,
    1591,  1899,  1596,  1602,  1612,  1471,   487,   170,  1604,  1620,
     170,   493,  1605,    79,    79,  1608,  1621,   402,   402,  1625,
     488,  1634,    79,  1234,  1626,   489,   426,  1706,  1479,  1636,
    1643,  1234,  1322,  1322,  1322,   150,  1499,  1503,   490,   491,
    1656,  1510,   402,   814,  1916,  1826,  1239,  1676,  1678,  1680,
     170,  1703,   210,  1410,  1705,   111,  1710,  1716,  1720,  1736,
    1234,    96,    96,   467,    79,  1721,  1952,   531,   140,  1933,
    1722,   466,  -111,  -111,  -111,  -111,  -111,  -111,   103,    61,
    1723,  1012,  1852,   760,    64,    65,    66,    67,    68,    69,
      70,   947,   530,  1730,  1737,   466,  1747,  1740,   140,  1741,
    1444,   266,  1750,    89,  1751,  1777,  1785,  1786,  1792,  1700,
     402,  1790,   140,   243,     6,     7,     8,     9,    10,    11,
      12,   298,  1796,  1234,  1797,   494,  1121,  1809,   569,   183,
    1805,   948,  1810,  1169,  1806,   466,   118,  1981,  1540,  1852,
     298,  -593,  1818,   466,   118,  1819,  1820,   184,   185,    79,
    1821,    79,  1822,    82,  1830,   498,   147,   815,  1836,  1834,
    -504,  1842,   466,  1793,   466,   466,    13,    14,    15,    16,
      17,   934,   265,   118,  1844,  1853,  1867,  1846,   266,   267,
    1854,   619,   107,   272,   466,   534,  1870,  1869,  1871,  1743,
    1888,   118,  1872,   111,  1889,  1903,    79,    79,  1902,   170,
    1904,   112,  1907,  1908,  1911,  1914,   196,   466,  1923,   111,
      82,   118,  1932,   170,  1924,  1939,   103,   466,  1940,  1928,
    1953,   140,   170,  1954,  1934,   180,  1956,    18,  1965,    79,
    1967,  1976,   103,  1973,    82,  1968,  1972,  1977,   457,   595,
    1987,    89,   466,  1727,   466,   111,  1988,   220,  1991,   118,
     245,  1438,  1525,  1856,    82,   976,  1598,    89,   676,   170,
     466,  1540,   977,   979,   933,   466,   978,   980,   103,    51,
      52,    53,    54,  1357,  1364,   466,  1707,  1610,  1966,    79,
    1917,   170,    13,    14,    15,    16,    17,  1220,  1788,    79,
      96,   147,  1926,    89,  1784,  1962,   595,    82,   453,   147,
    1841,   467,   296,   302,  1169,   107,  1865,  1234,  1960,  1951,
    1701,    61,  1702,  1895,   321,  1936,    64,    65,    66,    67,
      68,    69,    70,   955,   112,  1974,  1894,  1373,   167,   524,
    1198,   409,   180,   180,  1659,  1012,  1878,  1825,  1504,  1670,
    1177,  1265,   760,   147,   439,  1055,  1370,   245,   118,   118,
     118,   118,   118,   118,  1603,   790,  1713,   170,   870,   140,
    1856,   170,     3,   956,  1856,  1856,  1206,     0,     0,   453,
       0,   220,   220,   170,   991,   140,     0,   118,   118,   170,
     992,   993,     0,     0,   266,     0,     0,     0,   296,     0,
    1955,     0,     0,     0,     0,   170,     0,    82,     0,  1540,
       0,     0,     0,  1696,   170,     0,     0,     0,     0,   652,
     245,   140,  1970,    96,     0,  1540,  1970,     0,     0,     0,
       0,     0,    61,     0,   467,   168,   169,    64,    65,    66,
      67,    68,    69,    70,   688,   689,  1365,  1984,     0,     0,
     302,     0,     0,   107,     0,     0,   302,   296,   296,   118,
       0,  1540,     0,   170,   147,   688,     0,     0,     0,   107,
    1729,    61,   112,     0,   168,   169,    64,    65,    66,    67,
      68,    69,    70,   321,   605,   614,     0,   170,   112,     0,
       0,     0,     0,     0,   679,   688,   680,   681,   682,     0,
     321,   453,     0,   298,   321,   107,     0,     0,  1084,     0,
     467,  1969,     0,     0,     0,  1234,     0,     0,     0,     0,
    1234,  1234,  1234,     0,   112,   683,  1978,   170,   684,   685,
       0,     0,     0,   686,   687,   170,     0,   409,     0,     0,
       0,     0,   453,  1901,  1366,     0,     0,     0,     0,     0,
       0,   118,     0,     0,   170,     0,   170,   170,     0,     0,
       0,    96,     0,     0,   453,   453,  1187,     0,     0,     0,
       0,   409,     0,     0,   730,     0,   170,    96,     0,     0,
       0,   180,     0,   453,    61,  1808,   118,   168,   169,    64,
      65,    66,    67,    68,    69,    70,     0,   147,  1462,   170,
       0,   439,     0,     0,     0,   759,     0,   614,     0,   170,
       0,     0,     0,    96,     0,     0,   118,     0,     0,     0,
    1530,     0,     0,  1533,  1544,     0,     0,     0,     0,  1550,
     118,     0,     0,  1554,   170,  1556,   170,     0,     0,     0,
     578,     0,     0,     0,     0,   220,     0,     0,     0,   453,
       0,     0,   170,   118,   220,    61,  1515,   170,     0,   249,
      64,    65,    66,    67,    68,    69,    70,   170,  1868,     0,
     269,  1985,     0,     0,   296,  1250,   409,   409,     0,     0,
     296,  1992,   321,    72,     0,   637,     0,     0,     0,     0,
       0,     0,  1257,  1258,     0,     0,     0,     0,     0,     0,
    1234,     0,  1234,  1011,    74,     0,     0,   602,     0,     0,
       0,     0,   249,     0,    77,    78,     0,     0,    61,     0,
     296,   168,   169,    64,    65,    66,    67,    68,    69,    70,
       0,   296,     0,   296,     0,   321,     0,    82,     0,   118,
       0,     0,     0,     0,     0,     0,   962,     0,     0,     0,
       0,     0,   321,   439,     0,   614,   249,     0,     0,   638,
       0,     0,     0,   605,     0,     0,     0,   605,     0,     0,
       0,     0,     0,  1642,   639,     0,   321,   640,   641,    64,
      65,    66,    67,    68,    69,    70,   614,     0,     0,   321,
       0,  1343,     0,    61,    57,     0,     0,   147,    64,    65,
      66,    67,    68,    69,    70,  1675,     0,     0,     0,     0,
     409,     0,   147,   147,     0,   409,     0,  1681,   409,   249,
       0,   147,   147,   147,  1684,    61,     0,     0,   217,   218,
      64,    65,    66,    67,    68,    69,    70,  1515,     0,     0,
       0,     0,     0,  1663,     0,  1515,     0,     0,   453,   249,
       0,     0,  1533,     0,     0,   249,     0,     0,     0,   306,
     307,   308,   309,     0,     0,     0,     0,   119,     0,     0,
     119,     0,     0,  1264,    74,   439,     0,   118,     0,     0,
       0,     0,     0,     0,   249,     0,     0,     0,     0,     0,
       0,   730,   730,   118,     0,     0,     0,    61,     0,   409,
     168,   169,    64,    65,    66,    67,    68,    69,    70,     0,
       0,     0,     0,     0,     0,     0,   439,     0,     0,   759,
       0,   759,     0,     0,   119,     0,     0,     0,     0,   118,
     243,     6,     7,     8,     9,    10,    11,    12,   321,   321,
    1450,  1451,     0,     0,     0,     0,     0,     0,   119,   310,
       0,     0,     0,     0,  1465,  1466,     0,   321,     0,   296,
    1775,     0,     0,     0,     0,     0,     0,   311,   119,     0,
       0,     0,  1780,  1782,     0,  1544,     0,     0,   296,     0,
     236,  1215,     0,     0,     0,     0,  1219,  1487,  1488,     0,
       0,     0,     0,     0,     0,     0,  1764,  1227,     0,  1515,
       0,     0,     0,     0,     0,   119,   249,     0,     0,     0,
       0,   119,     0,   119,     0,     0,   409,     0,     0,     0,
       0,     0,     0,   321,     0,     0,   850,     0,     0,   147,
     409,     0,     0,     0,     0,     0,     0,     0,   321,     0,
    1182,     0,     0,     0,     0,   119,     0,     0,     0,   298,
       0,   605,     0,     0,     0,     0,     0,   119,     0,     0,
       0,  1838,   453,   453,   114,     0,    61,   114,     0,   168,
     169,    64,    65,    66,    67,    68,    69,    70,    61,     0,
     249,   217,   218,    64,    65,    66,    67,    68,    69,    70,
       0,   439,     0,     0,     0,     0,     0,     0,     0,  1515,
     249,     0,     0,     0,     0,     0,    72,     0,   119,     0,
       0,   119,     0,     0,     0,     0,   119,     0,     0,     0,
     249,   114,     0,  1890,     0,  1892,  1509,    74,     0,     0,
       0,     0,     0,  1510,     0,     0,     0,    77,    78,  1345,
       0,     0,     0,     0,     0,   114,     0,     0,     0,   119,
       0,     0,     0,     0,   249,     0,   730,     0,     0,     0,
       0,   251,     0,     0,   652,   114,     0,     0,   119,     0,
    1638,  1639,     0,   759,     0,     0,     0,     0,   249,     0,
     759,     0,     0,     0,   298,   249,     0,    13,    14,    15,
      16,    17,     0,  1938,     0,     0,     0,     0,     0,     0,
       0,     0,   114,     0,     0,  1946,  1948,  1949,   114,     0,
     114,     0,     0,     0,   251,     0,     0,     0,     0,     0,
       0,     0,   321,     0,   318,   114,   349,     0,     0,     0,
       0,   570,   298,     0,     0,     0,     0,     0,   688,     0,
       0,   119,   413,     0,     0,    57,     0,     0,     0,     0,
      18,     0,     0,     0,   114,   413,     0,     0,   251,     0,
       0,     0,   147,     0,   298,     0,     0,     0,     0,     0,
     409,     0,  1420,  1421,  1422,   119,    61,  1717,     0,  1423,
    1424,    64,    65,    66,    67,    68,    69,    70,    47,    48,
      49,    50,    51,    52,    53,    54,     0,     0,   850,   409,
       0,   119,     0,     0,    72,   114,     0,  1735,   114,  1673,
       0,     0,  1738,  1739,     0,     0,   245,    82,     0,     0,
       0,   251,     0,     0,    73,    74,     0,     0,     0,     0,
       0,   296,     0,     0,     0,    77,    78,   147,   548,     0,
       0,     0,     0,     0,   439,     0,   114,     0,     0,     0,
       0,   251,     0,     0,    57,     0,   850,   251,     0,     0,
       0,     0,     0,     0,     0,   114,     0,     0,     0,     0,
       0,   439,     0,     0,     0,   147,     0,     0,     0,     0,
     119,   119,     0,     0,   114,    61,   251,   114,   217,   218,
      64,    65,    66,    67,    68,    69,    70,     0,     0,     0,
       0,   114,     0,     0,     0,   114,     0,   249,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,   249,     0,
     453,   453,   119,     0,     0,     0,   119,     0,   119,     0,
       0,     0,     0,   219,    74,     0,   321,   321,   413,   249,
       0,   119,     0,     0,    77,    78,     0,    13,    14,    15,
      16,    17,     0,    61,     0,     0,   217,   218,    64,    65,
      66,    67,    68,    69,    70,   850,     0,     0,     0,     0,
       0,     0,   413,     0,   147,   147,   147,   147,   147,   147,
       0,     0,   850,   850,  1511,   302,    61,     0,     0,   189,
      63,    64,    65,    66,    67,    68,    69,    70,   114,     0,
       0,   119,   413,   409,   409,    57,     0,     0,   251,  1195,
       0,     0,     0,     0,   119,     0,   119,   119,     0,   119,
       0,     0,   119,     0,     0,   119,   119,   119,     0,     0,
       0,     0,     0,   245,     0,    74,    61,     0,   779,   217,
     218,    64,    65,    66,    67,    68,    69,    70,     0,     0,
      61,     0,   439,   345,   346,    64,    65,    66,    67,    68,
      69,    70,     0,    61,    72,     0,   543,    63,    64,    65,
      66,    67,    68,    69,    70,   147,     0,   413,   413,     0,
       0,     0,   251,   114,   757,    74,     0,     0,   602,     0,
       0,     0,     0,     0,     0,    77,   758,     0,     0,     0,
    1941,    75,     0,   119,   453,     0,   347,     0,     0,    57,
       0,     0,     0,     0,   114,   985,     0,     0,     0,   114,
       0,     0,   251,   114,     0,   114,     0,     0,     0,     0,
       0,  1708,     0,     0,     0,     0,   114,     0,   114,     0,
      61,     0,     0,   217,   218,    64,    65,    66,    67,    68,
      69,    70,   349,   114,   413,     0,   251,     0,     0,     0,
       0,  1648,     0,     0,     0,  1511,     0,   409,    72,     0,
       0,  1511,     0,  1511,     0,     0,     0,   114,     0,     0,
     251,     0,     0,     0,   548,     0,     0,   251,   295,    74,
     114,     0,   903,     0,     0,     0,     0,     0,   114,    77,
      78,   302,   147,     0,     0,     0,     0,     0,     0,     0,
       0,   413,     0,   114,   114,   249,   413,     0,     0,   413,
     119,     0,   114,   114,   114,     0,     0,     0,     0,     0,
     850,   850,   409,   119,   119,    98,     0,     0,   151,     0,
       0,     0,     0,   321,   850,   850,   147,   249,     0,     0,
       0,     0,     0,    13,    14,    15,    16,    17,   714,   715,
     716,   717,   718,   719,   720,   721,   722,   723,   724,   147,
       0,     0,   204,     0,     0,     0,   413,   850,   850,    61,
       0,     0,   545,   546,    64,    65,    66,    67,    68,    69,
      70,     0,    98,     0,   321,   321,     0,     0,     0,   725,
     413,     0,     0,     0,     0,     0,     0,     0,     0,  1648,
    1648,    57,     0,     0,     0,     0,   195,   413,     0,     0,
       0,     0,     0,     0,  1511,     0,     0,  1511,     0,     0,
      75,     0,     0,     0,     0,     0,   258,     0,     0,   114,
     114,     0,    61,     0,   302,   217,   218,    64,    65,    66,
      67,    68,    69,    70,  1861,   409,    61,     0,   114,     0,
       0,    64,    65,    66,    67,    68,    69,    70,  1231,     0,
      72,     0,  1232,   288,  1233,     0,     0,   296,     0,    98,
       0,     0,     0,     0,   249,     0,   114,     0,     0,     0,
    1897,    74,     0,     0,   498,     0,   323,     0,     0,     0,
       0,    77,    78,     0,     0,    74,     0,     0,  1430,   251,
       0,     0,     0,     0,   419,     0,     0,   413,  1648,     0,
     251,     0,     0,     0,   114,   288,   445,  1511,     0,     0,
     114,   413,   249,     0,     0,     0,     0,     0,     0,   114,
      61,  1184,   413,     0,   114,    64,    65,    66,    67,    68,
      69,    70,  1231,   147,   492,     0,  1232,     0,  1233,     0,
     850,   850,     0,     0,     0,     0,   119,     0,     0,     0,
     512,     0,     0,     0,   119,   517,   519,     0,   321,   195,
       0,     0,     0,     0,     0,     0,  1648,     0,     0,    74,
       0,     0,   413,   147,     0,     0,  1674,     0,    57,     0,
       0,   539,     0,   119,   541,     0,   542,     0,     0,   147,
     147,     0,  1898,   302,     0,     0,     0,   559,     0,     0,
       0,   119,     0,     0,     0,     0,     0,     0,     0,    61,
     571,     0,   217,   218,    64,    65,    66,    67,    68,    69,
      70,   119,     0,     0,     0,   147,     0,    13,    14,    15,
      16,    17,     0,   114,     0,   593,     0,    72,   617,  1898,
    1898,     0,     0,     0,     0,     0,     0,   850,     0,     0,
     114,   114,   624,     0,     0,    57,   624,  1897,    74,   119,
       0,   498,     0,     0,     0,     0,     0,     0,    77,    78,
       0,     0,  1898,     0,     0,     0,     0,   850,     0,     0,
     249,     0,   850,   850,     0,    57,    61,     0,     0,   217,
     218,    64,    65,    66,    67,    68,    69,    70,     0,     0,
       0,    61,     0,   114,    62,    63,    64,    65,    66,    67,
      68,    69,    70,     0,    72,     0,    61,     0,     0,   217,
     218,    64,    65,    66,    67,    68,    69,    70,     0,   508,
       0,     0,     0,     0,  1509,    74,     0,     0,     0,     0,
       0,     0,     0,   114,    72,    77,    78,     0,     0,   288,
       0,   413,    75,   593,     0,     0,     0,     0,   119,   119,
     119,   119,   119,   119,   219,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    77,    78,     0,     0,     0,
     413,     0,     0,     0,     0,     0,     0,   119,   119,     0,
       0,     0,     0,     0,     0,     0,     0,   251,   114,     0,
       0,     0,    61,     0,     0,   217,   218,    64,    65,    66,
      67,    68,    69,    70,     0,     0,     0,     0,   114,     0,
       0,     0,     0,   249,     0,   413,     0,     0,     0,  1184,
      72,     0,     0,     0,   445,     0,     0,     0,   163,     0,
       0,  1408,     0,     0,     0,     0,     0,     0,     0,     0,
    1509,    74,   413,     0,     0,     0,   114,  1510,     0,   119,
       0,    77,    78,     0,     0,   846,     0,     0,     0,     0,
     519,     0,    61,     0,   857,     0,   559,    64,    65,    66,
      67,    68,    69,    70,  1231,     0,     0,   323,  1232,    98,
    1233,     0,     0,     0,     0,     0,     0,     0,   114,   114,
       0,     0,     0,     0,   624,   878,     0,     0,     0,     0,
     275,   735,   114,   114,     0,     0,     0,   114,   114,   889,
       0,    74,     0,   281,  1615,   282,     0,   746,   593,     0,
     749,     0,     0,   898,     0,     0,     0,     0,     0,     0,
       0,   624,     0,     0,     0,   114,   114,     0,     0,     0,
       0,   119,     0,     0,     0,   114,   114,   114,   114,   114,
     114,     0,     0,     0,     0,     0,   251,     0,    61,     0,
     850,   168,   169,    64,    65,    66,    67,    68,    69,    70,
    1340,     0,     0,     0,   413,   413,   119,   508,     0,     0,
       0,    61,   249,     0,   217,   218,    64,    65,    66,    67,
      68,    69,    70,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   251,     0,   119,   502,   503,    72,
       0,   507,     0,     0,   510,   511,     0,   445,     0,     0,
     119,     0,     0,   413,     0,     0,     0,     0,     0,   757,
      74,     0,     0,   602,   994,     0,     0,     0,     0,     0,
      77,   758,     0,   119,     0,     0,   114,     0,     0,     0,
       0,     0,     0,   604,     0,     0,     0,     0,   878,     0,
       0,    61,     0,  1018,   217,   218,    64,    65,    66,    67,
      68,    69,    70,     0,     0,     0,     0,     0,     0,     0,
     445,   445,     0,     0,     0,     0,     0,     0,     0,    72,
     108,     0,     0,     0,     0,     0,    61,     0,     0,   445,
     589,    64,    65,    66,    67,    68,    69,    70,  1231,  1897,
      74,     0,  1232,   498,  1233,   621,     0,     0,   114,   114,
      77,    78,     0,     0,     0,     0,     0,   846,     0,   119,
       0,     0,     0,     0,     0,     0,     0,     0,   413,     0,
       0,     0,     0,     0,     0,    74,     0,   108,  1617,     0,
       0,     0,     0,     0,   114,     0,     0,     0,  1154,     0,
       0,     0,     0,     0,     0,   445,     0,     0,     0,     0,
     113,   151,   251,   114,     0,     0,     0,     0,     0,     0,
     624,     0,     0,  1186,     0,   846,     0,     0,     0,    61,
    1192,   259,   168,   169,    64,    65,    66,    67,    68,    69,
      70,     0,     0,   413,     0,     0,     0,     0,     0,   745,
       0,     0,     0,     0,   114,     0,     0,   114,   735,   735,
       0,   123,     0,     0,   123,   114,     0,   113,  1002,  1576,
       0,  1005,     0,   323,   108,     0,     0,   456,     0,     0,
     114,     0,     0,     0,     0,    13,    14,    15,    16,    17,
       0,   327,     0,     0,     0,   114,     0,     0,     0,     0,
     114,   114,     0,     0,     0,   114,   114,   119,     0,     0,
       0,   261,     0,     0,     0,     0,   816,     0,   123,     0,
       0,   446,     0,   119,     0,     0,     0,     0,     0,     0,
       0,     0,   508,     0,   846,     0,  1073,     0,     0,     0,
    1077,     0,   123,    57,     0,     0,     0,     0,     0,     0,
       0,   846,   846,     0,   113,   251,     0,     0,     0,   119,
       0,     0,   123,     0,     0,     0,   413,     0,     0,     0,
       0,   331,     0,     0,    61,     0,     0,   217,   218,    64,
      65,    66,    67,    68,    69,    70,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   540,     0,     0,   123,
       0,   448,    72,     0,   445,   123,     0,   123,     0,     0,
      61,     0,   108,   217,   218,    64,    65,    66,    67,    68,
      69,    70,   295,    74,     0,     0,     0,     0,     0,     0,
     892,   893,  1679,    77,    78,     0,     0,     0,    72,   123,
       0,  1683,     0,   900,  1323,     0,     0,     0,     0,     0,
     594,   123,  1154,   259,     0,     0,     0,     0,   219,    74,
       0,     0,     0,     0,   114,     0,     0,   594,     0,    77,
      78,   594,     0,     0,     0,     0,     0,     0,  1711,     0,
       0,  1154,     0,     0,     0,     0,     0,     0,     0,   114,
       0,     0,   113,     0,     0,     0,     0,     0,     0,  1371,
       0,     0,   123,     0,   114,   123,     0,     0,     0,     0,
     123,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     114,   114,     0,   735,   251,     0,   593,     0,     0,     0,
     596,     0,     0,   261,     0,   517,     0,     0,     0,     0,
       0,     0,     0,   123,     0,     0,     0,   596,     0,     0,
       0,   596,     0,   323,   996,   997,   114,     0,     0,     0,
    1001,     0,   123,     0,     0,     0,     0,     0,   594,     0,
       0,     0,     0,     0,     0,     0,  1767,     0,     0,     0,
    1776,  1022,     0,     0,  1025,  1026,     0,  1029,   114,  1031,
    1032,     0,  1783,     0,  1296,     0,     0,  1300,  1789,   846,
     846,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   846,   846,     0,     0,     0,   445,   445,
       0,     0,     0,     0,     0,     0,     0,     0,  1072,     0,
       0,     0,  1076,    61,     0,   123,   217,   218,    64,    65,
      66,    67,    68,    69,    70,     0,   846,   846,   596,   446,
       0,     0,     0,     0,     0,     0,  1323,  1323,  1323,   151,
       0,    72,     0,     0,     0,     0,     0,     0,     0,   123,
       0,     0,  1832,     0,     0,     0,     0,     0,     0,     0,
     327,   295,    74,     0,     0,  1539,  1539,     0,     0,   259,
       0,   108,    77,    78,     0,   123,  1860,     0,     0,     0,
       0,     0,   446,     0,   108,   365,     0,   366,  1193,   367,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   594,
     446,     0,     0,     0,     0,     0,     0,     0,     0,   448,
       0,     0,     0,     0,   323,     0,  1887,     0,     0,     0,
       0,     0,    61,   594,  1893,   168,   169,    64,    65,    66,
      67,    68,    69,    70,     0,   675,   594,   151,    75,   376,
     331,     0,     0,  1910,     0,  1912,  1913,     0,     0,   261,
       0,   113,     0,     0,   123,   123,     0,     0,     0,     0,
    1448,     0,   448,     0,   113,  1922,     0,     0,     0,  1457,
     460,     0,     0,     0,     0,     0,     0,     0,     0,   596,
     448,     0,     0,     0,     0,     0,     0,     0,  1937,     0,
       0,     0,     0,     0,     0,     0,   123,     0,  1942,     0,
     123,     0,   123,   596,     0,     0,     0,     0,     0,   846,
     846,     0,     0,     0,     0,   123,   596,     0,  1193,     0,
       0,     0,   446,  1964,     0,  1942,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1665,     0,     0,     0,     0,
       0,  1975,     0,     0,     0,   846,  1964,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1983,     0,     0,     0,
       0,     0,     0,   446,  1682,     0,  1295,     0,     0,  1299,
       0,     0,     0,     0,     0,   123,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   327,   327,     0,   123,     0,
     123,   123,     0,   123,  1539,     0,   123,     0,     0,   123,
     123,   123,   448,     0,   327,   323,     0,     0,   151,     0,
       0,     0,     0,     0,     0,     0,   846,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   327,     0,    13,    14,    15,    16,    17,     0,
       0,     0,     0,   448,     0,     0,   846,     0,     0,     0,
       0,   846,   846,     0,     0,     0,   445,   445,     0,     0,
       0,     0,     0,   108,     0,   331,   331,     0,     0,     0,
     327,     0,  1756,     0,     0,     0,     0,   123,     0,     0,
       0,     0,     0,     0,   331,   594,     0,     0,   259,  1397,
     327,     0,    57,     0,     0,     0,     0,  1406,  1407,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1651,     0,
       0,     0,   331,     0,     0,     0,     0,  1539,     0,     0,
       0,     0,     0,    61,     0,     0,   217,   218,    64,    65,
      66,    67,    68,    69,    70,     0,     0,     0,   446,     0,
       0,     0,     0,   113,     0,     0,     0,     0,     0,     0,
     331,    72,  1447,     0,     0,     0,     0,     0,     0,     0,
       0,  1456,     0,     0,  1460,   596,  1463,  1464,   261,     0,
     331,  1509,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    77,    78,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   123,     0,     0,     0,     0,   327,
       0,     0,     0,     0,     0,     0,  1490,   123,   123,     0,
       0,     0,     0,     0,     0,  1851,   327,   327,   448,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     445,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1539,  1651,  1651,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   327,
       0,  1539,  1851,  1578,     0,     0,     0,     0,     0,   331,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   331,   331,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1539,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   108,     0,     0,
       0,     0,  1930,   203,     0,     0,     0,     0,     0,   214,
     215,     0,     0,     0,     0,     0,     0,     0,     0,   846,
       0,     0,     0,     0,     0,     0,   108,     0,     0,   331,
       0,     0,     0,     0,     0,  1460,     0,     0,     0,     0,
       0,     0,     0,   277,   259,  1651,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1637,     0,     0,     0,     0,     0,
       0,   594,     0,     0,     0,     0,     0,   113,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   446,     0,
       0,     0,     0,     0,     0,     0,   113,     0,     0,  1876,
       0,     0,     0,  1651,     0,     0,     0,     0,     0,     0,
     123,     0,     0,     0,   261,     0,     0,     0,   123,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1651,
       0,     0,     0,     0,   327,   327,     0,     0,     0,     0,
       0,   596,     1,     0,     0,   145,     0,   123,   327,   327,
       0,  1712,     0,   327,   327,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   123,     0,     0,   448,     0,
       0,     0,     0,     0,     0,     0,  1651,  1651,     0,     0,
       0,   327,   327,     0,     0,   123,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   567,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1651,
       0,     0,     0,     0,   331,   331,     0,     0,   192,     0,
     108,   108,     0,   123,     0,  1760,  1761,     0,   331,   331,
       0,     0,     0,   331,   331,     0,     0,  1765,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   331,   331,     0,     0,     0,     0,     0,     0,   446,
       0,     0,     0,     0,     0,     0,     0,   283,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     113,   113,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   123,   123,   123,   123,   123,   123,     0,  1824,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   755,     0,   756,     0,   448,
       0,   123,   123,     0,     0,     0,   772,   773,     0,     0,
       0,     0,     0,     0,   327,   327,     0,     0,     0,     0,
       0,   283,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   520,     0,     0,     0,
       0,  1875,     0,     0,     0,     0,   283,     0,     0,     0,
     327,     0,     0,     0,     0,     0,   283,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   259,
     551,   555,     0,   123,     0,     0,     0,   562,   563,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   573,   331,   331,     0,     0,     0,   108,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     327,     0,   591,     0,   856,     0,   247,     0,     0,     0,
       0,   327,     0,     0,     0,     0,     0,   268,     0,   271,
     331,   273,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   261,
       0,   327,     0,     0,     0,     0,   327,   327,     0,     0,
       0,   327,   327,     0,     0,   123,     0,     0,   674,   247,
       0,   271,   273,     0,     0,     0,     0,     0,     0,   113,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     331,     0,     0,     0,     0,     0,     0,     0,     0,   713,
     123,   331,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   247,     0,     0,     0,     0,     0,     0,
       0,     0,   108,     0,     0,   751,     0,     0,     0,   754,
     123,   331,     0,     0,     0,     0,   331,   331,     0,     0,
       0,   331,   331,     0,   123,     0,     0,     0,   776,     0,
       0,     0,   777,   778,     0,     0,   781,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   123,     0,     0,
       0,   795,   796,   797,   798,     0,   247,     0,   271,   273,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     820,     0,     0,     0,     0,     0,     0,   165,   823,     0,
       0,     0,   113,     0,     0,     0,   247,     0,     0,     0,
       0,     0,   247,     0,     0,     0,     0,     0,     0,     0,
     594,     0,     0,   165,     0,     0,   283,  1033,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   247,     0,     0,     0,   327,     0,   620,     0,   273,
       0,     0,     0,   123,     0,     0,     0,   861,     0,     0,
     108,     0,     0,     0,   551,     0,     0,     0,     0,   165,
     867,     0,     0,     0,     0,     0,   108,   594,     0,     0,
       0,     0,   165,     0,   165,     0,     0,     0,     0,     0,
       0,     0,     0,   881,   886,     0,     0,     0,     0,     0,
     596,     0,     0,     0,  1101,  1102,     0,     0,     0,     0,
       0,     0,   108,     0,   351,  1160,  1161,  1162,     0,     0,
    1164,     0,     0,     0,     0,   331,     0,     0,     0,     0,
       0,   351,     0,     0,     0,     0,     0,   247,     0,     0,
     113,     0,     0,     0,   327,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   929,     0,   113,   596,     0,     0,
       0,     0,     0,   247,     0,   620,   273,     0,     0,   165,
       0,     0,     0,   165,     0,     0,   165,   165,     0,     0,
     165,     0,     0,   165,   165,     0,     0,     0,     0,     0,
       0,   123,   113,     0,     0,     0,     0,     0,     0,     0,
    1229,     0,     0,     0,     0,     0,     0,   123,     0,     0,
     247,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   990,     0,     0,   331,     0,     0,     0,     0,     0,
     247,     0,     0,     0,     0,   247,  1007,   247,     0,     0,
    1008,     0,     0,   123,  1248,   165,     0,     0,   165,   881,
       0,     0,     0,     0,     0,     0,     0,   247,     0,   247,
     247,     0,     0,     0,     0,     0,     0,     0,     0,   165,
       0,  1048,     0,     0,     0,     0,     0,   247,     0,     0,
    1057,     0,     0,     0,   165,     0,  1060,     0,     0,   247,
       0,  1272,     0,     0,     0,     0,     0,     0,     0,     0,
    1276,  1277,  1278,  1279,     0,     0,     0,     0,  1284,  1285,
       0,   247,     0,   620,   273,     0,     0,     0,  1292,     0,
       0,     0,     0,     1,     0,     0,     0,     0,     0,     0,
       1,     0,     0,     0,     0,   247,   620,     0,     0,  1306,
       0,  1307,   247,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     1,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   165,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1363,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1209,     0,     0,     0,     0,     0,     0,     0,     0,
    1377,     0,     0,     0,     0,     0,     0,  1381,     0,  1383,
    1385,     0,     0,   351,     0,     0,     0,     0,  1391,     0,
    1392,     0,  1393,     0,  1395,   165,     0,     0,     0,  1403,
      13,    14,    15,    16,    17,     0,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,  -407,  -407,     0,  -407,    45,    46,
       0,  -407,  1255,     0,     0,     0,  1256,     0,     0,     0,
       0,     0,     0,   881,     0,     0,     0,  1445,    57,     0,
       0,     0,     0,  1269,  1452,  1453,     0,     0,     0,   351,
    1270,     0,     0,     0,     0,     0,     0,     0,     0,  1274,
       0,  1275,     0,     0,     0,     0,     0,     0,  1476,     0,
       0,     0,   387,     0,     0,  1481,     0,     0,  1482,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   165,
     165,     0,     0,  1302,   247,     0,     0,  1303,     0,     0,
       0,     0,   165,     0,     0,   247,     0,     0,   214,     0,
       0,   145,     0,     0,     1,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   247,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   247,     0,     0,
      13,    14,    15,    16,    17,     0,   247,    19,  1571,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,     0,     0,    45,    46,
       0,  1590,     0,     0,     0,     0,     0,     0,     0,  1595,
       0,  1597,     0,     0,     0,     0,     0,     0,    57,     0,
       0,     0,  1390,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   165,   165,     0,     0,     0,     0,   165,
       0,     0,     0,     0,     0,     0,  1413,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1623,  1624,     0,
     165,     0,   247,   165,   165,     0,   165,     0,   165,   165,
       0,     0,  1629,  1630,   649,  1631,     0,   387,   654,     0,
       0,     0,     0,     0,  1635,     0,   247,   657,   658,     0,
       0,     0,     0,     0,  1640,  1641,     0,     0,     0,     0,
       0,     0,   387,   387,     0,     0,     0,   165,     0,     0,
       0,   165,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   387,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1485,     0,     0,     0,
    1486,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   387,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1521,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   165,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1718,  1719,     0,     0,     0,     0,     0,     0,     0,     0,
    1725,     0,     0,   200,     0,     0,     0,     0,     0,     0,
    1581,     0,   247,  1584,     0,     0,     0,     0,     0,   255,
       0,     0,     0,     0,     0,     0,     0,     0,  1592,     0,
       0,     0,     0,     0,     0,  1748,  1749,     0,     0,     0,
       0,     0,     0,     0,   247,     0,     0,     0,     0,     0,
     247,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   200,     0,
       0,     0,   303,     0,     0,     0,     0,     0,  1622,     0,
       0,     0,     0,   343,     0,     0,     0,  1627,     0,     0,
       0,  1628,     0,     0,     0,     0,     0,     0,     0,     0,
     200,     0,     0,     0,     0,  1632,  1633,   165,     0,     0,
       0,     0,     0,   455,  1804,     0,   459,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1813,     0,     0,  1814,  1815,     0,     0,     0,
       0,     0,  1817,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   165,     0,     0,   165,     0,
       0,     0,     0,     0,     0,     0,   200,     0,     0,     0,
     193,     0,     0,     0,     0,     0,     0,     0,     0,   255,
       0,   247,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   387,   387,   387,   387,   387,   387,
     387,   387,   387,   387,   387,   387,   387,   387,   387,   387,
     387,   387,   387,     0,     0,   459,     0,  1714,  1715,     0,
       0,     0,     0,   200,     0,     0,     0,     0,     0,   247,
       0,     0,     0,     0,     0,   193,     0,     0,     0,     0,
       0,     0,   599,     0,   616,     0,  1896,     0,     0,     0,
     193,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   193,     0,     0,
       0,     0,     0,     0,   387,     0,     0,     0,   165,     0,
     442,     0,     0,     0,     0,     0,   165,   165,     0,     0,
       0,     0,  1927,     0,     0,     0,   672,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1944,     0,     0,     0,
       0,     0,     0,  1950,     0,     0,     0,     0,     0,     0,
     200,     0,     0,   193,     0,  1791,     0,     0,  1963,     0,
       0,   165,     0,     0,     0,     0,     0,     0,     0,     0,
     165,     0,     0,   165,     0,   165,   165,  1584,     0,     0,
     599,     0,     0,     0,     0,     0,   771,     0,     0,     0,
       0,     0,     0,     0,     0,  1816,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     193,     0,     0,     0,     0,   165,     0,   247,     0,     0,
       0,     0,  1831,     0,     0,     0,     0,     0,     0,   193,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1847,
       0,     0,  1848,     0,     0,     0,     0,     0,     0,   387,
       0,     0,     0,     0,   387,   200,   200,     0,     0,     0,
       0,   455,     0,     0,     0,   387,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   165,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   387,     0,     0,     0,     0,
       0,     0,     0,     0,   343,     0,     0,   193,     0,     0,
     338,     0,     0,     0,     0,     0,  1915,     0,     0,     0,
       0,     0,   455,     0,   880,     0,     0,     0,     0,     0,
       0,     0,     0,   247,     0,     0,     0,   193,     0,   435,
     338,     0,     0,     0,     0,   599,     0,     0,     0,     0,
     247,     0,     0,     0,   165,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   200,     0,     0,     0,
       0,   501,     0,     0,     0,     0,     0,     0,   501,   672,
       0,   672,   672,   165,   672,     0,     0,   672,     0,     0,
     672,   672,   672,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   165,
       0,     0,   193,   193,     0,   165,     0,     0,   442,     0,
       0,     0,   387,     0,   247,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   455,     0,   501,     0,     0,     0,
       0,   171,   174,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   200,   338,
     606,   193,     0,     0,     0,     0,     0,     0,     0,     0,
     165,     0,     0,     0,     0,   455,   212,     0,     0,   442,
     627,     0,     0,     0,     0,     0,   387,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   455,   455,     0,
       0,     0,   193,     0,     0,     0,     0,     0,     0,   247,
       0,     0,     0,     0,     0,     0,   455,     0,     0,     0,
     387,   387,   387,   193,     0,     0,   290,   387,   387,   291,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   312,     0,   165,   165,     0,     0,     0,   387,
     501,     0,   351,     0,     0,     0,   165,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   501,   747,     0,   501,
     750,     0,     0,     0,     0,     0,     0,   338,     0,     0,
       0,   606,   455,     0,     0,     0,   387,   387,   247,   200,
       0,     0,     0,     0,     0,     0,   477,     0,     0,   771,
       0,   442,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   501,     0,     0,   193,   501,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   528,   442,     0,     0,     0,     0,     0,   165,     0,
     343,   171,     0,     0,     0,     0,     0,     0,   338,     0,
       0,     0,   171,     0,   442,   442,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   442,     0,     0,     0,     0,     0,   574,
       0,     0,     0,     0,     0,     0,   577,   579,     0,     0,
       0,   586,     0,     0,     0,     0,     0,     0,   501,     0,
     165,   338,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   876,   338,
       0,     0,     0,   631,     0,     0,     0,     0,   312,   606,
       0,   312,     0,   606,     0,     0,     0,     0,     0,   442,
     894,     0,   338,     0,     0,     0,   193,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   411,     0,     0,     0,
       0,   455,     0,     0,     0,     0,     0,   165,     0,   440,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   468,     0,   468,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   193,     0,     0,
       0,   672,     0,     0,     0,     0,     0,     0,   212,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     774,   775,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   338,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   501,   501,     0,
       0,     0,     0,     0,     0,   255,     0,   501,  1003,   387,
     501,  1006,     0,     0,     0,     0,     0,     0,     0,     0,
     568,     0,   338,     0,     0,   606,   200,   606,   606,     0,
       0,     0,     0,   599,   606,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   338,   338,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     343,     0,     0,   338,   672,     0,     0,   501,     0,     0,
       0,   501,     0,     0,     0,   501,  1074,     0,   442,   501,
    1078,     0,     0,     0,     0,     0,     0,  1081,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   312,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   455,   455,     0,     0,   338,
     501,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   631,
       0,     0,     0,     0,     0,     0,     0,   606,     0,     0,
       0,     0,     0,   672,   672,   672,     0,   672,   672,     0,
       0,     0,     0,     0,   459,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   468,   193,     0,     0,     0,   338,   468,     0,
     193,     0,   387,   792,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   255,     0,     0,     0,     0,   193,     0,     0,
       0,     0,   387,     0,     0,     0,     0,     0,     0,     0,
       0,   343,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   501,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   606,
     606,     0,     0,     0,     0,     0,   606,  1035,     0,     0,
     860,     0,   442,   442,  1047,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   387,   387,     0,     0,     0,   440,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   338,     0,
       0,   888,     0,   501,  1297,     0,   501,  1301,   387,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   200,     0,     0,     0,
       0,     0,   387,     0,     0,     0,     0,     0,     0,  1105,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   923,     0,     0,     0,     0,     0,     0,     0,
     255,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     792,   943,     0,     0,     0,     0,   387,     0,   193,     0,
     954,     0,   959,   954,     0,     0,     0,     0,     0,     0,
    1199,     0,     0,     0,     0,   631,     0,     0,     0,     0,
       0,     0,   343,     0,     0,     0,     0,     0,     0,     0,
     987,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     338,     0,     0,   989,     0,     0,   606,  1399,   672,     0,
    1191,     0,     0,     0,   998,     0,     0,    13,    14,    15,
      16,    17,     0,     0,     0,     0,     0,   338,   440,     0,
       0,   987,     0,   455,   455,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   360,     0,     0,     0,   361,  1051,   362,
       0,   468,     0,     0,     0,     0,     0,     0,     0,   501,
    1449,     0,     0,   193,     0,    57,   363,     0,   501,  1458,
       0,   606,     0,   255,     0,     0,     0,     0,     0,     0,
       0,     0,   338,   338,     0,     0,     0,  1082,     0,     0,
       0,     0,     0,   364,   365,     0,   366,     0,   367,    62,
      63,    64,    65,    66,    67,    68,    69,    70,   368,   369,
     357,     0,   370,   371,   372,     0,   373,   374,     0,     0,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   411,     0,     0,     0,   193,
       0,     0,     0,     0,   375,  1183,  1185,    75,   376,     0,
       0,     0,     0,   440,   377,   438,    78,   378,   379,   380,
     381,     0,     0,     0,     0,     0,     0,     0,     0,  1344,
    1346,  1348,     0,     0,   468,     0,     0,     0,     0,     0,
       0,     0,   672,   954,     0,     0,     0,     0,     0,     0,
     442,   442,     0,     0,     0,     0,   987,     0,   338,  1367,
       0,     0,     0,     0,  1222,     0,     0,   455,     0,     0,
       0,   954,     0,     0,  1105,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   672,
       0,     0,   459,     0,     0,     0,     0,     0,     0,     0,
       0,   631,     0,     0,     0,   468,     0,     0,     0,     0,
       0,     4,   243,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,   360,   501,    45,    46,
     361,     0,   362,    47,    48,    49,    50,    51,    52,    53,
      54,    55,   468,   501,  1288,    56,  1290,     0,    57,   363,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   364,   365,    60,   366,
       0,   367,    62,    63,    64,    65,    66,    67,    68,    69,
      70,   368,   369,   357,   442,   370,   371,   372,     0,   373,
     374,     0,  1520,     0,     0,  1522,     0,    72,     0,   338,
       0,     0,  1355,  1355,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   375,     0,  1541,
      75,   407,     0,     0,     0,     0,     0,   377,    77,    78,
     378,   379,   380,   381,     0,     0,     0,     0,     0,     0,
       0,  1542,  1543,     0,     0,  1401,     0,     0,     0,     0,
     338,   338,    13,    14,    15,    16,    17,     0,     0,     0,
       0,     0,     0,  1394,     0,   501,   501,     0,     0,  1404,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   501,     0,     0,     0,     0,   440,     0,   360,     0,
       0,     0,   361,     0,   362,     0,     0,     0,     0,     0,
       0,     0,     0,   468,     0,     0,     0,     0,     0,     0,
      57,   363,     0,     0,     0,     0,   954,     0,     0,   792,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   364,   365,
       0,   366,     0,   367,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   368,   369,   357,     0,   370,   371,   372,
       0,   373,   374,     0,     0,     0,     0,     0,     0,    72,
    1484,     0,     0,     0,   501,     0,     0,     0,     0,     0,
       0,     0,   501,     0,  1671,     0,     0,     0,     0,   375,
       0,     0,    75,   376,     0,     0,     0,     0,     0,   377,
    1402,    78,   378,   379,   380,   381,     0,     0,   954,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   468,     0,     0,   792,
       0,     0,     0,     0,   338,     0,     0,     0,   501,  1877,
       0,     0,   501,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   631,   943,   501,     0,
       0,     0,     0,     0,     0,     0,     0,  1593,  1594,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   468,     0,   792,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   501,   501,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   242,   243,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,     0,     0,    19,   501,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,  -408,  -408,     0,  -408,    45,    46,
    1802,  -408,   411,     0,     0,     0,     0,  1664,     0,   631,
      13,    14,    15,    16,    17,     0,     0,    19,    57,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,  -408,  -408,     0,  -408,    45,    46,
       0,  -408,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1704,     0,     0,     0,     0,    57,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      75,     0,  1726,     0,     0,  1728,     4,   243,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
    1106,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,     0,
       0,   360,     0,    45,    46,   361,     0,   362,    47,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
      56,     0,  1107,    57,  1108,    -2,     0,  1109,     0,     0,
    1110,  1111,  1112,  1113,  1114,  1115,  1116,  1117,  1118,  1119,
    1120,  1121,  -287,  1122,  1123,  1124,  1125,  1126,     0,  1127,
       0,   364,   365,    60,   462,     0,   367,  1128,  1129,    64,
      65,    66,    67,    68,    69,    70,   368,   369,   357,  1130,
     370,   371,   372,     0,   373,   374,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    -3,   375,     0,     0,    75,   407,     0,     0,     0,
     279,     0,   377,    77,    78,   378,   379,   380,   381,     0,
       0,     0,     0,     0,     0,     0,     0,  -175,     0,     4,
     243,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,  1106,     0,    19,   954,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,   360,     0,    45,    46,   361,     0,
     362,    47,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,    56,     0,  1107,    57,  1108,    -2,     0,
    1109,     0,     0,  1110,  1111,  1112,  1113,  1114,  1115,  1116,
    1117,  1118,  1119,  1120,  1121,  -287,  1122,  1123,  1124,  1125,
    1126,     0,  1127,     0,   364,   365,    60,   462,     0,   367,
    1128,  1129,    64,    65,    66,    67,    68,    69,    70,   368,
     369,   357,  1130,   370,   371,   372,     0,   373,   374,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   375,     0,     0,    75,   407,
       0,     0,     0,   279,     0,   377,    77,    78,   378,   379,
     380,   381,     0,     0,     0,     0,     0,     0,     0,     0,
    -175,     4,   243,     6,     7,     8,     9,    10,    11,    12,
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
       0,     0,     0,     0,     0,     0,     0,   375,     0,     0,
      75,   407,     0,     0,     0,     0,     0,   377,    77,    78,
     378,   379,   380,   381,     0,     0,     0,     0,     0,     0,
       0,  1542,  1543,     4,   243,     6,     7,     8,     9,    10,
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
       0,  1532,    75,   407,     0,     0,     0,     0,     0,   377,
      77,    78,   378,   379,   380,   381,     4,   243,     6,     7,
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
       0,     0,   375,     0,     0,    75,   407,     0,     0,     0,
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
       0,     0,     0,     0,   377,   438,    78,   378,   379,   380,
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
    1180,     0,     0,     0,     0,     0,   377,  1181,    78,   378,
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
       0,    75,   376,     0,     0,     0,     0,     0,   377,    77,
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
     377,    77,    78,   378,   379,   380,   381,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
       0,     0,     0,     0,    45,    46,     0,     0,     0,    47,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,    56,     0,     0,    57,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      59,     0,     0,     0,    60,    61,     0,     0,    62,    63,
      64,    65,    66,    67,    68,    69,    70,     0,     0,     0,
      71,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    73,    74,     0,    75,    76,     0,     0,
       0,     0,     0,     0,    77,    78,   242,   243,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
       0,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,  -408,
    -408,     0,  -408,    45,    46,     0,  -408,     0,     0,     0,
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
    -717,     0,     0,    77,    78,     4,   243,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,     0,     0,
       0,     0,    45,    46,     0,     0,     0,    47,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,    56,
       0,     0,    57,     0,     0,     0,     0,  -340,  -340,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    60,     0,     0,     0,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -340,     0,     0,     0,    75,    76,     0,     0,     0,     0,
       0,     0,    77,    78,     4,   243,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,     0,     0,     0,
       0,    45,    46,     0,     0,     0,    47,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,    56,     0,
       0,    57,     0,     0,     0,     0,  -341,  -341,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    60,     0,     0,     0,    62,    63,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -341,
       0,     0,     0,    75,    76,     0,     0,     0,     0,     0,
       0,    77,    78,   242,   243,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,     0,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,  -408,  -408,     0,  -408,
      45,    46,     0,  -408,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      57,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    61,     0,     0,    62,    63,    64,    65,    66,    67,
      68,    69,    70,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    73,
      74,     0,    75,   244,     0,     0,  1315,     0,     0,     0,
      77,    78,  1316,     0,     0,    13,    14,    15,    16,    17,
      18,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,     0,
       0,     0,     0,    45,    46,     0,     0,     0,    47,    48,
      49,    50,    51,    52,    53,    54,     0,     0,     0,     0,
       0,     0,     0,    57,  1317,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    60,     0,     0,     0,    62,    63,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1318,     0,     0,     0,    75,   919,     0,     0,  1315,
       0,     0,     0,    77,    78,  1316,     0,     0,    13,    14,
      15,    16,    17,    18,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,     0,     0,    45,    46,     0,     0,
       0,    47,    48,    49,    50,    51,    52,    53,    54,     0,
       0,     0,     0,     0,     0,     0,    57,  1317,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    60,     0,     0,     0,
      62,    63,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1496,     0,     0,     0,    75,   919,
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
       0,     0,     0,     0,     0,     0,     0,  1497,     0,     0,
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
    1498,     0,     0,     0,    75,   919,     0,     0,     0,     0,
       0,     0,    77,    78,   242,   243,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,     0,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,  -408,  -408,     0,
    -408,    45,    46,     0,  -408,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    57,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    62,    63,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   242,     0,     0,
       0,     0,     0,    75,   244,     0,    13,    14,    15,    16,
      17,    77,    78,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
    -408,  -408,     0,  -408,    45,    46,     0,  -408,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    57,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    62,    63,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    75,   301,     0,     0,
       0,     0,     0,     0,    77,    78,   243,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,     0,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,  -408,  -408,
       0,  -408,    45,    46,     0,  -408,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    57,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    61,     0,     0,    62,    63,    64,    65,
      66,    67,    68,    69,    70,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    73,    74,     0,    75,   244,     0,     0,     0,  -721,
       0,     0,    77,    78,   243,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,     0,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,  -408,  -408,     0,  -408,
      45,    46,     0,  -408,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      57,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    61,     0,     0,    62,    63,    64,    65,    66,    67,
      68,    69,    70,     0,     0,     0,     0,     0,     0,     0,
    1339,     0,     0,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    73,
      74,   360,    75,   244,     0,   361,     0,   362,     0,     0,
      77,    78,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1107,     0,   363,     0,     0,  1109,  1768,  1769,
    1110,  1111,  1112,  1113,  1114,  1115,  1116,  1117,  1118,  1119,
    1120,  1121,  -287,  1122,  1123,  1124,  1125,  1126,     0,  1127,
       0,   364,   365,     0,   462,     0,   367,  1128,  1129,    64,
      65,    66,    67,    68,    69,    70,   368,   369,   357,  1130,
     370,   371,   372,     0,   373,   374,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,  1339,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   375,     0,     0,    75,   376,     0,     0,     0,
     279,     0,   377,    77,    78,   378,   379,   380,   381,   360,
       0,     0,     0,   361,     0,   362,     0,  -175,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1107,     0,   363,    -2,     0,  1109,     0,     0,  1110,  1111,
    1112,  1113,  1114,  1115,  1116,  1117,  1118,  1119,  1120,  1121,
    -287,  1122,  1123,  1124,  1125,  1126,     0,  1127,     0,   364,
     365,     0,   462,     0,   367,  1128,  1129,    64,    65,    66,
      67,    68,    69,    70,   368,   369,   357,  1130,   370,   371,
     372,     0,   373,   374,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,  1339,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     375,     0,     0,    75,   376,     0,     0,     0,   279,     0,
     377,    77,    78,   378,   379,   380,   381,   360,     0,     0,
       0,   361,     0,   362,     0,  -175,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1107,     0,
     363,     0,     0,  1109,     0,     0,  1110,  1111,  1112,  1113,
    1114,  1115,  1116,  1117,  1118,  1119,  1120,  1121,  -287,  1122,
    1123,  1124,  1125,  1126,     0,  1127,     0,   364,   365,     0,
     462,     0,   367,  1128,  1129,    64,    65,    66,    67,    68,
      69,    70,   368,   369,   357,  1130,   370,   371,   372,     0,
     373,   374,     0,     0,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   375,     0,
       0,    75,   376,     0,     0,     0,   279,     0,   377,    77,
      78,   378,   379,   380,   381,     0,     0,     0,     0,     0,
       0,     0,     0,  -175,   243,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,     0,     0,
      45,    46,     0,     0,     0,   319,    48,    49,    50,    51,
      52,    53,    54,     0,    13,    14,    15,    16,    17,    18,
      57,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,     0,     0,
       0,     0,    45,    46,    62,    63,     0,    47,    48,    49,
      50,    51,    52,    53,    54,     0,     0,     0,     0,     0,
       0,     0,    57,     0,     0,     0,     0,     0,     0,    72,
       0,  1042,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -589,    75,   320,     0,     0,    62,    63,     0,     0,
      77,    78,   243,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    75,     0,     0,     0,    45,    46,
       0,     0,     0,   319,    48,    49,    50,    51,    52,    53,
      54,     0,    13,    14,    15,    16,    17,    18,    57,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,     0,     0,
      45,    46,    62,    63,     0,   319,    48,    49,    50,    51,
      52,    53,    54,     0,     0,     0,     0,     0,     0,     0,
      57,     0,     0,     0,     0,     0,     0,    72,     0,  1744,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      75,   320,     0,     0,    62,    63,     0,     0,    77,    78,
     243,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    75,     0,     0,     0,    45,    46,     0,     0,
       0,   319,    48,    49,    50,    51,    52,    53,    54,     0,
       0,     0,     0,     0,     0,     0,    57,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      62,    63,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    72,     0,  1746,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    75,   320,
       0,     0,     0,     0,     0,     0,    77,    78,   243,     6,
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
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
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
       0,     0,     0,     0,    75,   301,     0,     0,     0,     0,
       0,     0,    77,    78,   243,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,     0,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,  -408,  -408,     0,  -408,
      45,    46,     0,  -408,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      57,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    62,    63,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    75,   244,     0,     0,     0,     0,     0,     0,
      77,    78,    13,    14,    15,    16,    17,    18,   659,    19,
     660,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,   360,     0,
      45,    46,   361,     0,   362,    47,    48,    49,    50,    51,
      52,    53,    54,     0,     0,     0,     0,     0,     0,     0,
      57,   363,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   661,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   364,   365,
       0,   366,     0,   367,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   368,   369,   357,     0,   370,   371,   372,
       0,   373,   374,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   375,
       0,     0,    75,   662,     0,     0,     0,   279,     0,   377,
      77,    78,   663,   664,   380,   381,    13,    14,    15,    16,
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
       0,     0,     0,   375,     0,   406,    75,   407,     0,     0,
       0,     0,     0,   377,    77,    78,   378,   379,   380,   381,
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
      75,   662,     0,     0,     0,   279,     0,   377,    77,    78,
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
       0,   375,     0,     0,    75,   407,     0,     0,     0,     0,
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
       0,     0,     0,     0,     0,   375,     0,     0,    75,   437,
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
       0,     0,    75,   376,     0,     0,     0,     0,     0,   377,
      77,    78,   378,   379,   380,   381,    13,    14,    15,    16,
      17,    18,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
       0,     0,     0,     0,    45,    46,     0,     0,     0,    47,
      48,    49,    50,    51,    52,    53,    54,     0,     0,     0,
       0,     0,     0,     0,    57,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    61,     0,     0,    62,    63,
      64,    65,    66,    67,    68,    69,    70,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    73,    74,     0,    75,    76,     0,     0,
       0,  -719,     0,     0,    77,    78,    13,    14,    15,    16,
      17,    18,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
       0,     0,     0,     0,    45,    46,     0,     0,     0,    47,
      48,    49,    50,    51,    52,    53,    54,     0,     0,     0,
       0,     0,     0,     0,    57,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    61,     0,     0,    62,    63,
      64,    65,    66,    67,    68,    69,    70,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    73,    74,     0,    75,    76,     0,     0,
       0,     0,     0,     0,    77,    78,    13,    14,    15,    16,
      17,    18,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
       0,     0,     0,     0,    45,    46,     0,     0,     0,    47,
      48,    49,    50,    51,    52,    53,    54,     0,     0,     0,
       0,     0,     0,     0,    57,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    61,     0,     0,    62,    63,
      64,    65,    66,    67,    68,    69,    70,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    75,    76,     0,    13,
      14,    15,    16,    17,    77,    78,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,  -408,  -408,     0,  -408,    45,    46,     0,
    -408,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    57,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    61,     0,
       0,    62,    63,    64,    65,    66,    67,    68,    69,    70,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    73,    74,     0,    75,
     301,     0,     0,     0,     0,     0,     0,    77,    78,   556,
     243,     6,     7,     8,     9,    10,    11,    12,    13,    14,
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
      54,     0,    13,    14,    15,    16,    17,    18,    57,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,    75,     0,
      45,    46,    62,    63,     0,    47,    48,    49,    50,    51,
      52,    53,    54,     0,     0,     0,     0,     0,     0,     0,
      57,     0,     0,     0,     0,     0,  1416,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   925,
      75,   919,     0,     0,    62,    63,     0,     0,    77,    78,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    75,   919,     0,     0,     0,     0,     0,     0,
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
       0,     0,    57,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    75,   286,     0,     0,    62,    63,     0,     0,
      77,    78,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    75,    76,     0,     0,     0,     0,
       0,     0,    77,    78,    13,    14,    15,    16,    17,    18,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,     0,     0,
       0,     0,    45,    46,     0,     0,     0,    47,    48,    49,
      50,    51,    52,    53,    54,     0,    13,    14,    15,    16,
      17,    18,    57,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
       0,     0,     0,     0,    45,    46,    62,    63,     0,   319,
      48,    49,    50,    51,    52,    53,    54,     0,     0,     0,
       0,     0,     0,     0,    57,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    75,   433,     0,     0,    62,    63,
       0,     0,    77,    78,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    75,   320,     0,     0,
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
       0,     0,     0,     0,     0,     0,    75,   286,     0,     0,
      62,    63,     0,     0,    77,    78,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    75,   433,
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
      62,    63,     0,    47,    48,    49,    50,    51,    52,    53,
      54,     0,     0,     0,     0,     0,     0,     0,    57,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    75,   301,
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
       0,     0,     0,   319,    48,    49,    50,    51,    52,    53,
      54,     0,    13,    14,    15,    16,    17,    18,    57,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,     0,     0,
      45,    46,    62,    63,     0,   319,    48,    49,    50,    51,
      52,    53,    54,     0,     0,     0,     0,     0,     0,     0,
      57,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      75,   919,     0,     0,    62,    63,     0,     0,    77,    78,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    75,     0,     0,    13,    14,    15,    16,    17,
      77,    78,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,  -408,
    -408,     0,  -408,    45,    46,     0,  -408,     0,     0,     0,
       0,     0,     0,     0,     0,    13,    14,    15,    16,    17,
       0,     0,    19,    57,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,  -408,
    -408,     0,  -408,    45,    46,     0,  -408,    62,    63,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    57,     0,     0,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    75,   301,    62,    63,     0,
       0,     0,     0,    77,    78,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    75,     0,     0,     0,     0,
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
       0,     0,   845,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -602,    75,   243,     6,     7,     8,     9,    10,
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
       0,  1672,     0,     0,     0,     0,   243,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,    19,    75,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,     0,     0,
       0,     0,    45,    46,     0,     0,     0,   319,    48,    49,
      50,    51,    52,    53,    54,     0,     0,     0,     0,     0,
       0,     0,    57,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   243,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    62,    63,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,  -408,  -408,     0,  -408,    45,
      46,     0,  -408,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    75,     0,     0,     0,     0,    57,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    62,    63,     0,     0,     0,     0,    13,
      14,    15,    16,    17,    18,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,     0,     0,     0,     0,    45,    46,     0,
       0,    75,    47,    48,    49,    50,    51,    52,    53,    54,
       0,     0,     0,     0,     0,     0,     0,    57,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    61,     0,
       0,    62,    63,    64,    65,    66,    67,    68,    69,    70,
       0,     0,     0,    13,    14,    15,    16,    17,     0,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,  -408,  -408,    75,
    -408,    45,    46,     0,  -408,     0,     0,   360,     0,     0,
       0,   361,     0,   362,     0,     0,     0,     0,     0,     0,
       0,    57,     0,     0,     0,     0,     0,     0,     0,     0,
     363,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    62,    63,   364,   365,     0,
     366,     0,   367,  1778,    63,    64,    65,    66,    67,    68,
      69,    70,   368,   369,   357,     0,   370,   371,   372,     0,
     373,   374,     0,     0,     0,     0,   360,     0,    72,     0,
     361,     0,   362,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    75,     0,     0,     0,     0,   375,   363,
       0,    75,   376,     0,     0,     0,     0,     0,   377,    77,
      78,   378,   379,   380,   381,     0,     0,     0,     0,     0,
       0,     0,  1779,  -175,     0,     0,   364,   365,     0,   462,
       0,   367,    62,    63,    64,    65,    66,    67,    68,    69,
      70,   368,   369,   357,     0,   370,   371,   372,   360,   373,
     374,     0,   361,     0,   362,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   363,     0,     0,     0,     0,     0,   375,    74,     0,
     463,   464,     0,     0,     0,   465,     0,   377,    77,    78,
     378,   379,   380,   381,     0,     0,     0,     0,   364,   365,
       0,   366,     0,   367,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   368,   369,   357,     0,   370,   371,   372,
     360,   373,   374,     0,   361,     0,   362,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   363,     0,     0,     0,     0,     0,   375,
    1225,     0,    75,   376,     0,     0,     0,  1226,     0,   377,
      77,    78,   378,   379,   380,   381,     0,     0,     0,     0,
     364,   365,     0,   366,     0,   367,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   368,   369,   357,     0,   370,
     371,   372,   360,   373,   374,     0,   361,     0,   362,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   363,     0,     0,     0,     0,
       0,   375,   950,  1523,    75,   376,     0,     0,     0,     0,
       0,   377,    77,    78,   378,   379,   380,   381,     0,     0,
       0,     0,   364,   365,     0,   366,     0,   367,    62,    63,
      64,    65,    66,    67,    68,    69,    70,   368,   369,   357,
       0,   370,   371,   372,   360,   373,   374,     0,   361,     0,
     362,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   363,     0,     0,
       0,     0,     0,   375,     0,     0,    75,   376,     0,     0,
       0,   465,     0,   377,    77,    78,   378,   379,   380,   381,
       0,     0,     0,     0,   364,   365,     0,   366,     0,   367,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   368,
     369,   357,     0,   370,   371,   372,   360,   373,   374,     0,
     361,     0,   362,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   363,
       0,     0,     0,     0,     0,   375,   791,     0,    75,   376,
       0,     0,     0,     0,     0,   377,    77,    78,   378,   379,
     380,   381,     0,     0,     0,     0,   364,   365,     0,   366,
       0,   367,    62,    63,    64,    65,    66,    67,    68,    69,
      70,   368,   369,   357,     0,   370,   371,   372,   360,   373,
     374,     0,   361,     0,   362,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   363,     0,     0,     0,     0,     0,   375,     0,     0,
      75,   376,     0,     0,     0,   279,     0,   377,    77,    78,
     378,   379,   380,   381,     0,     0,     0,     0,   364,   365,
       0,   366,     0,   367,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   368,   369,   357,     0,   370,   371,   372,
     360,   373,   374,     0,   361,     0,   362,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   363,     0,     0,     0,     0,     0,   375,
     950,     0,    75,   376,     0,     0,     0,     0,     0,   377,
      77,    78,   378,   379,   380,   381,     0,     0,     0,     0,
     364,   365,     0,   366,     0,   367,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   368,   369,   357,     0,   370,
     371,   372,   360,   373,   374,     0,   361,     0,   362,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   363,     0,     0,     0,     0,
       0,   375,     0,     0,    75,   376,     0,     0,   981,     0,
       0,   377,    77,    78,   378,   379,   380,   381,     0,     0,
       0,     0,   364,   365,     0,   366,     0,   367,    62,    63,
      64,    65,    66,    67,    68,    69,    70,   368,   369,   357,
       0,   370,   371,   372,   360,   373,   374,     0,   361,     0,
     362,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   363,     0,     0,
       0,     0,     0,   375,  1289,     0,    75,   376,     0,     0,
       0,     0,     0,   377,    77,    78,   378,   379,   380,   381,
       0,     0,     0,     0,   364,   365,     0,   366,     0,   367,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   368,
     369,   357,     0,   370,   371,   372,   360,   373,   374,     0,
     361,     0,   362,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   363,
       0,     0,     0,     0,     0,   375,     0,     0,    75,   376,
       0,     0,     0,  1349,     0,   377,    77,    78,   378,   379,
     380,   381,     0,     0,     0,     0,   364,   365,     0,   366,
       0,   367,    62,    63,    64,    65,    66,    67,    68,    69,
      70,   368,   369,   357,     0,   370,   371,   372,   360,   373,
     374,     0,   361,     0,   362,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   363,     0,     0,     0,     0,     0,   375,     0,  1774,
      75,   376,     0,     0,     0,     0,     0,   377,    77,    78,
     378,   379,   380,   381,     0,     0,     0,     0,   364,   365,
       0,   366,     0,   367,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   368,   369,   357,     0,   370,   371,   372,
     360,   373,   374,     0,   361,     0,   362,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   363,     0,     0,     0,     0,     0,   375,
    1947,     0,    75,   376,     0,     0,     0,     0,     0,   377,
      77,    78,   378,   379,   380,   381,     0,     0,     0,     0,
     364,   365,     0,   366,     0,   367,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   368,   369,   357,     0,   370,
     371,   372,   360,   373,   374,     0,   361,     0,   362,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   363,     0,     0,     0,     0,
       0,   375,     0,     0,    75,   376,     0,     0,     0,     0,
       0,   377,    77,    78,   378,   379,   380,   381,     0,     0,
       0,     0,   364,   365,     0,   366,     0,   367,    62,    63,
      64,    65,    66,    67,    68,    69,    70,   368,   369,   357,
       0,   370,   371,   372,   360,   373,   374,     0,   361,     0,
     362,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   363,     0,     0,
       0,     0,     0,   648,     0,     0,    75,   376,     0,     0,
       0,     0,     0,   377,    77,    78,   378,   379,   380,   381,
       0,     0,     0,     0,   364,   365,     0,   366,     0,   367,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   368,
     369,   357,     0,   370,   371,   372,   360,   373,   374,     0,
     361,     0,   362,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   363,
       0,     0,     0,     0,     0,   653,     0,     0,    75,   376,
       0,     0,     0,     0,     0,   377,    77,    78,   378,   379,
     380,   381,     0,     0,     0,     0,   364,   365,     0,   366,
       0,   367,    62,    63,    64,    65,    66,    67,    68,    69,
      70,   368,   369,   357,     0,   370,   371,   372,   360,   373,
     374,     0,   361,     0,   362,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   363,     0,     0,     0,     0,     0,   656,     0,     0,
      75,   376,     0,     0,     0,     0,     0,   377,    77,    78,
     378,   379,   380,   381,     0,     0,     0,     0,   364,   365,
       0,   366,     0,   367,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   368,   369,   357,     0,   370,   371,   372,
     360,   373,   374,     0,   361,     0,   362,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   363,     0,     0,     0,     0,     0,   375,
       0,     0,    75,   376,     0,     0,     0,     0,     0,   377,
     859,    78,   378,   379,   380,   381,     0,     0,     0,     0,
     364,   365,     0,   366,     0,   367,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   368,   369,   357,     0,   370,
     371,   372,     0,   373,   374,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   375,     0,     0,    75,   376,     0,     0,     0,     0,
       0,   377,   438,    78,   378,   379,   380,   381
};

static const yytype_int16 yycheck[] =
{
       1,   150,   633,     4,    73,    73,   283,   219,   256,     1,
     686,   408,   219,   605,   514,   515,   178,   241,   614,     1,
     601,    75,    95,    73,    73,   173,   323,   375,   219,   465,
      58,   673,   868,   220,   601,     4,   956,   601,   607,   162,
     162,   759,   219,   178,   207,   854,   139,   219,   525,   757,
    1710,   162,   666,   222,    55,    56,   759,    58,   535,  1341,
     219,   219,  1647,     1,   338,   985,    58,   163,   342,   760,
      73,  1199,    73,    82,   730,   766,    58,   757,    95,    73,
     757,    82,    70,   295,    73,   757,     1,   236,   295,    90,
      82,     0,  1772,    73,    95,   321,  1882,    98,   191,   576,
    1116,   102,  1647,    95,   295,    70,    98,   777,  1894,   296,
     102,     1,   338,   181,     4,  1647,   342,   145,   295,   573,
      58,   290,   291,   295,   149,   795,   151,   838,  1048,  1274,
    1275,   181,   181,   102,  1920,   635,   295,   295,   147,   140,
     854,  1090,   143,    58,   145,    87,  1095,   220,   445,   150,
     219,   219,    87,   145,   865,   156,   130,   174,    59,    60,
    1768,   149,   163,   145,   102,   153,    82,   759,    58,   219,
     219,  1232,     0,    87,  1341,  1342,   757,    70,   181,   180,
     181,  1648,    87,    98,   149,   174,     0,   181,   180,   229,
     757,   861,   761,   757,   195,    82,   765,    96,   172,   321,
     155,   181,   157,   195,   205,   774,   775,   145,    98,   210,
     104,   105,   102,   439,   254,   157,   219,  1166,   219,   220,
     274,   316,   157,   296,   264,   219,   295,   295,   220,     1,
     145,   147,     4,   131,   631,   236,   245,   737,  1918,   219,
     916,   155,   155,   157,   245,   295,   295,   131,   115,   497,
     149,   155,   157,   245,   255,   145,   157,   258,   152,   354,
     153,   174,   149,   179,   265,  1945,   258,   165,   944,     1,
     174,   483,  1880,   174,   275,   276,   483,   278,   909,  1939,
    1408,   165,   776,   777,   149,   562,    58,   265,  1570,  1756,
      75,    76,   483,   302,   295,   296,   593,   889,   276,  1108,
    1018,   795,   303,  1011,   296,   492,   483,    19,   471,   310,
     311,   483,  1897,     1,   315,  1018,   590,   439,   115,   933,
     617,  1012,   776,   777,   483,   483,    58,   624,  1137,   245,
     102,  1011,     1,    70,  1011,   177,   684,   149,   994,  1011,
     192,   795,   435,   258,    73,   283,   347,   443,  1100,    70,
     120,   352,  1897,   627,   355,   356,   153,   569,   835,    88,
    1309,   173,   569,   512,   590,  1897,    70,   861,   258,   518,
      58,    73,   157,   145,  1435,  1436,  1437,   155,   569,   131,
     149,  1092,   152,  1850,   571,     4,   302,    89,   614,    58,
    1060,  1100,   569,   146,  1108,   146,   174,   569,    70,   152,
     409,   627,   146,  1570,   106,   321,   157,   861,   323,   463,
     569,   569,   149,   145,   149,   483,   153,  1100,   419,   492,
     173,     1,   173,  1137,     4,    70,  1018,   419,   149,   173,
    1011,  1898,   153,   483,   483,    70,    55,    56,   155,   375,
     352,   442,   443,   355,  1011,   149,    70,  1011,   165,   153,
     573,   573,    56,   454,   455,    59,    60,   145,    62,   211,
      70,   154,   463,  1930,   465,   155,  1035,    70,   157,   321,
     149,    90,   324,   155,   151,   929,   145,   149,    58,   156,
     483,   153,   483,   165,   174,   174,   338,  1632,  1633,   483,
     342,   492,   614,   409,   534,   149,   155,  1188,   571,    70,
     492,   569,    82,   483,   149,   149,    70,   174,   153,   155,
    1430,   512,    75,    76,   149,   174,   155,   518,   153,   569,
     569,   140,   155,   439,   143,   149,  1693,  1543,   174,   153,
     445,  1547,   284,   151,    70,   174,   157,   156,   156,   149,
     149,   174,   155,   153,   163,   149,   149,   575,   157,   846,
     153,   283,   988,   174,   155,   556,   155,   558,   174,   139,
    1312,  1313,  1314,   149,  1195,   145,  1060,   147,   569,   160,
     571,   157,  1263,   174,   575,   174,   167,   168,   149,   571,
     152,   878,   153,   575,   585,   149,  1182,   439,   589,   153,
     148,   210,     3,   575,   157,   283,    82,   155,   514,   515,
    1242,     3,   876,  1312,  1313,  1314,  1060,    70,   770,    95,
      70,   191,    98,   149,   283,   155,   102,   153,   149,   781,
     621,   784,   560,   375,   562,    70,   149,  1341,  1342,  1312,
    1313,  1314,   633,   156,   173,   770,   255,   575,   150,   143,
     144,   145,   151,   131,   559,   157,   265,    70,   797,   157,
     876,   155,   157,   162,   163,  1331,   275,   276,   712,   278,
     575,   165,   242,   759,  1831,   245,   157,   816,   520,  1505,
     174,  1816,   160,   161,  1690,  1175,   677,   678,   151,   680,
    1847,   149,   155,   684,   303,   575,   687,   125,   126,   149,
     171,   310,   311,   153,   180,  1615,   315,  1617,   614,   115,
     148,   151,   617,   283,   149,   149,   156,   155,   153,   195,
    1352,   712,   648,   149,   650,   651,  1883,   653,   151,   635,
     656,   573,   302,   659,   660,   661,   149,   151,   347,   153,
     153,   169,   170,   352,   220,   149,   355,     1,   590,   591,
       4,   151,   151,   152,   907,   151,   156,    12,    13,    14,
      15,    16,   504,   104,   105,  1209,   757,   509,   759,   245,
     151,  1777,   614,   151,    12,    13,    14,    15,    16,  1785,
     771,   151,   258,   151,   526,   627,    56,   778,   149,    59,
      60,   155,    62,   784,   536,  1427,   787,   152,   153,   151,
    1226,   156,    21,   155,    58,   796,   797,   798,   155,  1047,
     104,   105,  1199,   575,   900,    70,   929,   929,   149,    12,
      13,    14,    15,    16,   149,   816,   162,   163,    82,   981,
    1274,   737,    70,   442,    46,    47,  1842,    49,   151,   409,
     562,    53,   155,   149,   155,   454,   455,   153,   102,   101,
     691,   692,   693,   575,   106,   107,   108,   109,   110,   111,
     112,   852,   853,   854,    96,   435,  1570,   157,   149,   591,
     151,   713,   153,   155,   151,   155,   131,    70,   155,   711,
     123,   124,   854,   149,   562,   139,   108,   109,   110,   111,
     112,   145,   149,   147,   151,    87,   153,   575,   150,  1186,
     151,   153,  1055,   562,   155,  1057,   648,   149,   867,   900,
     157,   653,   754,   904,   656,   149,   575,   151,   909,   153,
    1079,   127,   128,  1349,   915,   179,   854,  1496,  1497,  1498,
     151,  1017,  1018,   675,   155,   148,   129,   191,   149,   129,
     151,   846,   153,   419,   151,   154,  1390,   556,   155,   854,
     151,   149,   857,   157,   155,   153,   149,   948,   157,   149,
     153,   157,   868,   153,   151,   956,  1182,   160,   161,   157,
     160,   161,   151,   878,   854,   149,   585,  1191,  1316,  1600,
     589,   154,   155,   173,   173,   817,   556,   867,   242,  1693,
     205,   245,   562,   115,   985,   149,   250,   988,   149,  1425,
      12,    13,    14,    15,    16,   575,    12,    13,    14,    15,
      16,   151,   621,   149,   149,   155,   492,   153,   153,   151,
    1011,  1408,   155,   155,   633,   166,  1017,  1018,   228,   283,
    1169,   231,   754,     3,   876,   158,   151,   879,  1176,  1177,
     155,   129,    12,    13,    14,    15,    16,   149,   302,   158,
     151,   153,   151,   253,   155,   151,   155,  1048,    70,   155,
     171,   149,   159,   263,    70,   153,  1304,   161,   677,   678,
    1182,   680,   160,   161,  1341,   684,   154,   155,   687,   129,
     822,    12,    13,    14,    15,    16,   151,   929,   160,   161,
     155,   833,   854,  1245,   836,   151,  1209,  1209,   840,   155,
      70,  1100,  1723,   712,   151,   867,   152,    73,   155,     4,
       5,     6,     7,     8,     9,    10,    11,   129,   151,  1110,
     151,   375,  1113,  1114,  1115,   131,   151,  1831,   151,    95,
    1282,  1283,   854,  1799,   151,   151,  1108,   149,   155,    70,
     151,   153,   151,  1847,   155,  1322,  1137,   154,   160,   161,
     151,   129,  1143,   153,   155,   409,   131,  1282,  1283,   131,
    1151,   156,   771,  1154,  1155,  1137,   151,  1158,   156,   778,
     155,   149,  1154,  1155,  1275,   153,   854,   156,  1169,  1883,
    1108,   435,   160,   161,   150,   149,   151,   796,  1632,   798,
     155,   630,   155,   151,   151,   854,  1155,   155,   129,   154,
     155,     3,   151,  1108,  1195,   151,   155,   154,   155,  1137,
      12,    13,    14,    15,    16,   151,   151,  1902,   149,  1210,
     155,  1906,   153,   698,   699,   700,   701,  1155,  1108,   160,
     161,   129,  1137,   154,   155,  1226,  1097,  1098,  1099,   154,
     155,  1232,   149,   852,   853,   854,   155,   156,   463,  1154,
     465,   149,   151,   219,   220,   153,   151,  1137,   151,  1322,
     514,   515,   160,   161,   154,   155,   154,   155,    70,  1175,
     236,   154,   155,  1264,  1154,  1155,  1182,  1390,  1390,   174,
     151,  1186,   154,   155,   854,   154,   155,  1213,   154,   155,
     154,   900,   154,   155,   153,   904,   157,   867,   154,   155,
     909,    68,   556,  1570,   157,   505,   560,  1509,   562,   154,
     155,   157,  1509,  1312,  1313,  1314,   157,  1316,  1317,   154,
     155,   575,   154,   155,     9,   154,  1503,   527,  1509,   295,
     296,  1322,   149,   533,  1511,  1326,    76,   537,  1329,   154,
    1182,    17,  1509,   782,   154,   155,  1108,  1509,   154,   155,
     155,   156,  1094,   154,   155,   173,  1315,   375,  1349,   155,
    1509,  1509,   157,   101,   149,  1107,   174,  1209,   106,   107,
     108,   109,   110,   111,   112,  1137,   154,   155,  1369,   154,
    1371,   635,  1124,   155,   156,   151,  1108,    75,    76,  1371,
     155,   156,   151,  1155,   648,   157,   650,   651,   174,   653,
    1233,  1234,   656,   694,   695,   659,   660,   661,   696,   697,
     157,   149,   150,  1341,  1342,  1137,   101,   702,   703,   104,
     105,   106,   107,   108,   109,   110,   111,   112,  1516,  1517,
    1108,  1176,  1177,   154,  1425,  1315,  1341,  1342,    17,  1430,
    1503,   143,   144,   145,  1435,  1436,  1437,   148,  1511,  1108,
    1509,   151,   151,   155,  1413,   151,   151,   151,   151,  1137,
    1662,  1341,  1342,   165,   151,  1662,  1371,   151,  1581,  1581,
     148,   910,   174,    68,   157,   157,   157,   443,  1137,   151,
    1581,  1662,   174,   737,   173,   151,   151,   151,  1665,   148,
     157,  1371,   155,   151,   151,  1662,   514,   515,   151,   151,
    1662,  1110,   151,   154,  1113,  1114,  1115,   155,   151,   151,
     151,   151,  1503,  1662,  1662,   151,   151,   483,  1509,   151,
    1511,   151,   151,   151,   151,   151,   492,  1518,  1137,   151,
    1100,   151,  1633,  1413,  1143,   151,   148,   151,  1108,   173,
     148,  1532,  1151,   149,   151,   155,   512,   149,  1390,  1158,
    1541,   101,   518,  1315,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   149,  1851,   149,   149,  1137,   149,   784,
      13,   156,   787,    72,   174,  1791,  1715,   155,    89,  1341,
    1342,  1540,   156,   174,   154,   154,  1195,  1578,  1765,   148,
    1332,  1333,   558,    12,    13,    14,    15,    16,    17,  1505,
     854,  1210,  1665,   569,   148,   571,   174,   174,    70,  1600,
     160,   157,  1540,   867,   868,   155,  1768,   174,   151,  1341,
    1342,   151,   154,   154,  1615,  1584,  1617,   155,  1370,   155,
     648,   151,  1848,   155,  1539,   653,   151,   837,   656,   101,
     148,   151,  1570,  1768,   106,   107,   108,   109,   110,   111,
     112,  1413,   148,   174,   149,   174,   149,   675,    78,  1539,
    1540,   174,   149,  1341,  1342,  1570,   103,   129,   174,   174,
     107,  1662,   174,   110,  1665,   112,   174,   148,  1154,  1155,
     192,  1607,  1341,  1342,   174,  1676,   174,   149,   150,  1680,
    1570,   709,   149,   151,   148,  1897,   148,   157,   160,   161,
    1897,  1692,  1765,   154,  1584,   155,   155,  1698,   154,   154,
     151,  1850,   154,   148,   118,  1816,  1897,  1326,   151,   148,
    1329,  1898,   156,  1714,  1715,   156,   151,  1879,  1880,   151,
    1897,   154,  1723,   948,   151,  1897,   151,  1569,   154,   151,
     148,   956,  1312,  1313,  1314,  1315,  1316,  1317,  1897,  1897,
     174,   156,  1904,  1930,  1879,  1880,   155,   151,   149,   151,
    1369,   149,   155,  1202,   149,  1693,   107,   154,   154,   151,
     985,  1341,  1342,   988,  1765,   154,  1928,  1682,  1540,  1904,
     148,  1772,    12,    13,    14,    15,    16,    17,  1693,   101,
     157,   757,  1791,   759,   106,   107,   108,   109,   110,   111,
     112,   113,  1682,   148,   151,  1796,   154,   151,  1570,   151,
    1249,   248,   151,  1693,   151,   174,   174,   148,   174,  1561,
    1972,   149,  1584,     4,     5,     6,     7,     8,     9,    10,
      11,   797,   151,  1048,   151,  1898,    88,   148,  1897,  1897,
     154,   153,   148,  1413,   154,  1836,  1100,  1972,  1570,  1848,
     816,   151,   151,  1844,  1108,   151,   151,  1897,  1897,  1850,
     151,  1852,   151,     1,   156,   153,     4,  1930,    73,   174,
     152,   174,  1863,  1705,  1865,  1866,    12,    13,    14,    15,
      16,    17,    63,  1137,    73,   156,   151,   174,   325,   326,
     174,   328,  1570,   330,  1885,  1371,   151,   148,   151,   155,
     150,  1155,   151,  1831,   150,   101,  1897,  1898,   156,  1518,
     149,  1570,   155,    73,   149,   148,  1898,  1908,   165,  1847,
      58,  1175,   154,  1532,   165,   107,  1831,  1918,   107,   174,
     151,  1693,  1541,   156,   174,    73,   151,    17,   148,  1930,
     148,    73,  1847,   174,    82,   151,   149,   151,  1930,  1791,
     151,  1831,  1943,  1607,  1945,  1883,   174,    95,   174,  1213,
      98,  1239,  1331,  1795,   102,   704,  1405,  1847,   377,  1578,
    1961,  1693,   705,   707,   665,  1966,   706,   708,  1883,    59,
      60,    61,    62,  1126,  1137,  1976,  1570,  1426,  1945,  1980,
    1880,  1600,    12,    13,    14,    15,    16,    17,  1697,  1990,
    1570,   139,  1894,  1883,  1693,  1940,  1848,   145,   520,   147,
    1780,  1226,   150,   151,  1584,  1693,  1800,  1232,  1939,  1927,
    1562,   101,  1562,  1848,   162,  1907,   106,   107,   108,   109,
     110,   111,   112,   113,  1693,  1958,  1847,  1158,    48,   250,
       9,   179,   180,   181,  1503,  1011,  1824,  1765,  1317,  1517,
     871,  1017,  1018,   191,   192,   784,  1151,   195,  1312,  1313,
    1314,  1315,  1316,  1317,  1413,   472,  1584,  1676,   585,  1831,
    1902,  1680,     0,   153,  1906,  1907,   915,    -1,    -1,   591,
      -1,   219,   220,  1692,   729,  1847,    -1,  1341,  1342,  1698,
     729,   729,    -1,    -1,   531,    -1,    -1,    -1,   236,    -1,
    1932,    -1,    -1,    -1,    -1,  1714,    -1,   245,    -1,  1831,
      -1,    -1,    -1,  1552,  1723,    -1,    -1,    -1,    -1,   363,
     258,  1883,  1954,  1693,    -1,  1847,  1958,    -1,    -1,    -1,
      -1,    -1,   101,    -1,  1349,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   388,   389,    76,  1979,    -1,    -1,
     288,    -1,    -1,  1831,    -1,    -1,   294,   295,   296,  1413,
      -1,  1883,    -1,  1772,   302,   409,    -1,    -1,    -1,  1847,
    1609,   101,  1831,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   321,   322,   323,    -1,  1796,  1847,    -1,
      -1,    -1,    -1,    -1,   118,   439,   120,   121,   122,    -1,
     338,   713,    -1,  1169,   342,  1883,    -1,    -1,   823,    -1,
    1425,  1953,    -1,    -1,    -1,  1430,    -1,    -1,    -1,    -1,
    1435,  1436,  1437,    -1,  1883,   149,  1968,  1836,   152,   153,
      -1,    -1,    -1,   157,   158,  1844,    -1,   375,    -1,    -1,
      -1,    -1,   754,  1852,   174,    -1,    -1,    -1,    -1,    -1,
      -1,  1505,    -1,    -1,  1863,    -1,  1865,  1866,    -1,    -1,
      -1,  1831,    -1,    -1,   776,   777,   881,    -1,    -1,    -1,
      -1,   409,    -1,    -1,   412,    -1,  1885,  1847,    -1,    -1,
      -1,   419,    -1,   795,   101,  1724,  1540,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    -1,   435,  1264,  1908,
      -1,   439,    -1,    -1,    -1,   443,    -1,   445,    -1,  1918,
      -1,    -1,    -1,  1883,    -1,    -1,  1570,    -1,    -1,    -1,
    1338,    -1,    -1,  1341,  1342,    -1,    -1,    -1,    -1,  1347,
    1584,    -1,    -1,  1351,  1943,  1353,  1945,    -1,    -1,    -1,
     157,    -1,    -1,    -1,    -1,   483,    -1,    -1,    -1,   861,
      -1,    -1,  1961,  1607,   492,   101,  1322,  1966,    -1,    98,
     106,   107,   108,   109,   110,   111,   112,  1976,  1807,    -1,
     109,  1980,    -1,    -1,   512,   990,   514,   515,    -1,    -1,
     518,  1990,   520,   129,    -1,    12,    -1,    -1,    -1,    -1,
      -1,    -1,  1007,  1008,    -1,    -1,    -1,    -1,    -1,    -1,
    1615,    -1,  1617,   149,   150,    -1,    -1,   153,    -1,    -1,
      -1,    -1,   151,    -1,   160,   161,    -1,    -1,   101,    -1,
     558,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      -1,   569,    -1,   571,    -1,   573,    -1,   575,    -1,  1693,
      -1,    -1,    -1,    -1,    -1,    -1,   690,    -1,    -1,    -1,
      -1,    -1,   590,   591,    -1,   593,   195,    -1,    -1,    86,
      -1,    -1,    -1,   601,    -1,    -1,    -1,   605,    -1,    -1,
      -1,    -1,    -1,  1491,   101,    -1,   614,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   624,    -1,    -1,   627,
      -1,   174,    -1,   101,    70,    -1,    -1,   635,   106,   107,
     108,   109,   110,   111,   112,  1523,    -1,    -1,    -1,    -1,
     648,    -1,   650,   651,    -1,   653,    -1,  1535,   656,   258,
      -1,   659,   660,   661,  1542,   101,    -1,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,  1503,    -1,    -1,
      -1,    -1,    -1,  1509,    -1,  1511,    -1,    -1,  1060,   288,
      -1,    -1,  1570,    -1,    -1,   294,    -1,    -1,    -1,    63,
      64,    65,    66,    -1,    -1,    -1,    -1,     1,    -1,    -1,
       4,    -1,    -1,   149,   150,   713,    -1,  1831,    -1,    -1,
      -1,    -1,    -1,    -1,   323,    -1,    -1,    -1,    -1,    -1,
      -1,   729,   730,  1847,    -1,    -1,    -1,   101,    -1,   737,
     104,   105,   106,   107,   108,   109,   110,   111,   112,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   754,    -1,    -1,   757,
      -1,   759,    -1,    -1,    58,    -1,    -1,    -1,    -1,  1883,
       4,     5,     6,     7,     8,     9,    10,    11,   776,   777,
    1255,  1256,    -1,    -1,    -1,    -1,    -1,    -1,    82,   153,
      -1,    -1,    -1,    -1,  1269,  1270,    -1,   795,    -1,   797,
    1678,    -1,    -1,    -1,    -1,    -1,    -1,   171,   102,    -1,
      -1,    -1,  1690,  1691,    -1,  1693,    -1,    -1,   816,    -1,
       3,   925,    -1,    -1,    -1,    -1,   930,  1302,  1303,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1662,   941,    -1,  1665,
      -1,    -1,    -1,    -1,    -1,   139,   445,    -1,    -1,    -1,
      -1,   145,    -1,   147,    -1,    -1,   854,    -1,    -1,    -1,
      -1,    -1,    -1,   861,    -1,    -1,   551,    -1,    -1,   867,
     868,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   876,    -1,
     878,    -1,    -1,    -1,    -1,   179,    -1,    -1,    -1,  1715,
      -1,   889,    -1,    -1,    -1,    -1,    -1,   191,    -1,    -1,
      -1,  1779,  1274,  1275,     1,    -1,   101,     4,    -1,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   101,    -1,
     519,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      -1,   929,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1765,
     539,    -1,    -1,    -1,    -1,    -1,   129,    -1,   242,    -1,
      -1,   245,    -1,    -1,    -1,    -1,   250,    -1,    -1,    -1,
     559,    58,    -1,  1841,    -1,  1843,   149,   150,    -1,    -1,
      -1,    -1,    -1,   156,    -1,    -1,    -1,   160,   161,   174,
      -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,   283,
      -1,    -1,    -1,    -1,   593,    -1,   994,    -1,    -1,    -1,
      -1,    98,    -1,    -1,  1108,   102,    -1,    -1,   302,    -1,
    1485,  1486,    -1,  1011,    -1,    -1,    -1,    -1,   617,    -1,
    1018,    -1,    -1,    -1,  1850,   624,    -1,    12,    13,    14,
      15,    16,    -1,  1911,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   139,    -1,    -1,  1923,  1924,  1925,   145,    -1,
     147,    -1,    -1,    -1,   151,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1060,    -1,   161,   162,   163,    -1,    -1,    -1,
      -1,  1897,  1898,    -1,    -1,    -1,    -1,    -1,  1182,    -1,
      -1,   375,   179,    -1,    -1,    70,    -1,    -1,    -1,    -1,
      17,    -1,    -1,    -1,   191,   192,    -1,    -1,   195,    -1,
      -1,    -1,  1100,    -1,  1930,    -1,    -1,    -1,    -1,    -1,
    1108,    -1,  1216,  1217,  1218,   409,   101,  1592,    -1,  1223,
    1224,   106,   107,   108,   109,   110,   111,   112,    55,    56,
      57,    58,    59,    60,    61,    62,    -1,    -1,   823,  1137,
      -1,   435,    -1,    -1,   129,   242,    -1,  1622,   245,  1521,
      -1,    -1,  1627,  1628,    -1,    -1,  1154,  1155,    -1,    -1,
      -1,   258,    -1,    -1,   149,   150,    -1,    -1,    -1,    -1,
      -1,  1169,    -1,    -1,    -1,   160,   161,  1175,   275,    -1,
      -1,    -1,    -1,    -1,  1182,    -1,   283,    -1,    -1,    -1,
      -1,   288,    -1,    -1,    70,    -1,   881,   294,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   302,    -1,    -1,    -1,    -1,
      -1,  1209,    -1,    -1,    -1,  1213,    -1,    -1,    -1,    -1,
     514,   515,    -1,    -1,   321,   101,   323,   324,   104,   105,
     106,   107,   108,   109,   110,   111,   112,    -1,    -1,    -1,
      -1,   338,    -1,    -1,    -1,   342,    -1,   846,    -1,    -1,
      -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,   857,    -1,
    1632,  1633,   556,    -1,    -1,    -1,   560,    -1,   562,    -1,
      -1,    -1,    -1,   149,   150,    -1,  1274,  1275,   375,   878,
      -1,   575,    -1,    -1,   160,   161,    -1,    12,    13,    14,
      15,    16,    -1,   101,    -1,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   990,    -1,    -1,    -1,    -1,
      -1,    -1,   409,    -1,  1312,  1313,  1314,  1315,  1316,  1317,
      -1,    -1,  1007,  1008,  1322,  1323,   101,    -1,    -1,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   435,    -1,
      -1,   635,   439,  1341,  1342,    70,    -1,    -1,   445,   157,
      -1,    -1,    -1,    -1,   648,    -1,   650,   651,    -1,   653,
      -1,    -1,   656,    -1,    -1,   659,   660,   661,    -1,    -1,
      -1,    -1,    -1,  1371,    -1,   150,   101,    -1,   153,   104,
     105,   106,   107,   108,   109,   110,   111,   112,    -1,    -1,
     101,    -1,  1390,   104,   105,   106,   107,   108,   109,   110,
     111,   112,    -1,   101,   129,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,  1413,    -1,   514,   515,    -1,
      -1,    -1,   519,   520,   149,   150,    -1,    -1,   153,    -1,
      -1,    -1,    -1,    -1,    -1,   160,   161,    -1,    -1,    -1,
    1915,   152,    -1,   737,  1816,    -1,   157,    -1,    -1,    70,
      -1,    -1,    -1,    -1,   551,   153,    -1,    -1,    -1,   556,
      -1,    -1,   559,   560,    -1,   562,    -1,    -1,    -1,    -1,
      -1,  1575,    -1,    -1,    -1,    -1,   573,    -1,   575,    -1,
     101,    -1,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   589,   590,   591,    -1,   593,    -1,    -1,    -1,
      -1,  1499,    -1,    -1,    -1,  1503,    -1,  1505,   129,    -1,
      -1,  1509,    -1,  1511,    -1,    -1,    -1,   614,    -1,    -1,
     617,    -1,    -1,    -1,   621,    -1,    -1,   624,   149,   150,
     627,    -1,   629,    -1,    -1,    -1,    -1,    -1,   635,   160,
     161,  1539,  1540,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   648,    -1,   650,   651,  1154,   653,    -1,    -1,   656,
     854,    -1,   659,   660,   661,    -1,    -1,    -1,    -1,    -1,
    1255,  1256,  1570,   867,   868,     1,    -1,    -1,     4,    -1,
      -1,    -1,    -1,  1581,  1269,  1270,  1584,  1186,    -1,    -1,
      -1,    -1,    -1,    12,    13,    14,    15,    16,   132,   133,
     134,   135,   136,   137,   138,   139,   140,   141,   142,  1607,
      -1,    -1,   146,    -1,    -1,    -1,   713,  1302,  1303,   101,
      -1,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,    -1,    58,    -1,  1632,  1633,    -1,    -1,    -1,   173,
     737,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1647,
    1648,    70,    -1,    -1,    -1,    -1,    82,   754,    -1,    -1,
      -1,    -1,    -1,    -1,  1662,    -1,    -1,  1665,    -1,    -1,
     152,    -1,    -1,    -1,    -1,    -1,   102,    -1,    -1,   776,
     777,    -1,   101,    -1,  1682,   104,   105,   106,   107,   108,
     109,   110,   111,   112,  1798,  1693,   101,    -1,   795,    -1,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,    -1,
     129,    -1,   117,   139,   119,    -1,    -1,  1715,    -1,   145,
      -1,    -1,    -1,    -1,  1323,    -1,   823,    -1,    -1,    -1,
     149,   150,    -1,    -1,   153,    -1,   162,    -1,    -1,    -1,
      -1,   160,   161,    -1,    -1,   150,    -1,    -1,   153,   846,
      -1,    -1,    -1,    -1,   180,    -1,    -1,   854,  1756,    -1,
     857,    -1,    -1,    -1,   861,   191,   192,  1765,    -1,    -1,
     867,   868,  1371,    -1,    -1,    -1,    -1,    -1,    -1,   876,
     101,   878,   879,    -1,   881,   106,   107,   108,   109,   110,
     111,   112,   113,  1791,   220,    -1,   117,    -1,   119,    -1,
    1485,  1486,    -1,    -1,    -1,    -1,  1100,    -1,    -1,    -1,
     236,    -1,    -1,    -1,  1108,   241,   242,    -1,  1816,   245,
      -1,    -1,    -1,    -1,    -1,    -1,  1824,    -1,    -1,   150,
      -1,    -1,   929,  1831,    -1,    -1,  1521,    -1,    70,    -1,
      -1,   267,    -1,  1137,   270,    -1,   272,    -1,    -1,  1847,
    1848,    -1,  1850,  1851,    -1,    -1,    -1,   283,    -1,    -1,
      -1,  1155,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,
     296,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,  1175,    -1,    -1,    -1,  1883,    -1,    12,    13,    14,
      15,    16,    -1,   990,    -1,   321,    -1,   129,   324,  1897,
    1898,    -1,    -1,    -1,    -1,    -1,    -1,  1592,    -1,    -1,
    1007,  1008,   338,    -1,    -1,    70,   342,   149,   150,  1213,
      -1,   153,    -1,    -1,    -1,    -1,    -1,    -1,   160,   161,
      -1,    -1,  1930,    -1,    -1,    -1,    -1,  1622,    -1,    -1,
    1539,    -1,  1627,  1628,    -1,    70,   101,    -1,    -1,   104,
     105,   106,   107,   108,   109,   110,   111,   112,    -1,    -1,
      -1,   101,    -1,  1060,   104,   105,   106,   107,   108,   109,
     110,   111,   112,    -1,   129,    -1,   101,    -1,    -1,   104,
     105,   106,   107,   108,   109,   110,   111,   112,    -1,   230,
      -1,    -1,    -1,    -1,   149,   150,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1100,   129,   160,   161,    -1,    -1,   435,
      -1,  1108,   152,   439,    -1,    -1,    -1,    -1,  1312,  1313,
    1314,  1315,  1316,  1317,   149,   150,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   160,   161,    -1,    -1,    -1,
    1137,    -1,    -1,    -1,    -1,    -1,    -1,  1341,  1342,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1154,  1155,    -1,
      -1,    -1,   101,    -1,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,    -1,    -1,    -1,  1175,    -1,
      -1,    -1,    -1,  1682,    -1,  1182,    -1,    -1,    -1,  1186,
     129,    -1,    -1,    -1,   520,    -1,    -1,    -1,    47,    -1,
      -1,  1198,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     149,   150,  1209,    -1,    -1,    -1,  1213,   156,    -1,  1413,
      -1,   160,   161,    -1,    -1,   551,    -1,    -1,    -1,    -1,
     556,    -1,   101,    -1,   560,    -1,   562,   106,   107,   108,
     109,   110,   111,   112,   113,    -1,    -1,   573,   117,   575,
     119,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1255,  1256,
      -1,    -1,    -1,    -1,   590,   591,    -1,    -1,    -1,    -1,
     119,   412,  1269,  1270,    -1,    -1,    -1,  1274,  1275,   605,
      -1,   150,    -1,   132,   153,   134,    -1,   428,   614,    -1,
     431,    -1,    -1,   619,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   627,    -1,    -1,    -1,  1302,  1303,    -1,    -1,    -1,
      -1,  1505,    -1,    -1,    -1,  1312,  1313,  1314,  1315,  1316,
    1317,    -1,    -1,    -1,    -1,    -1,  1323,    -1,   101,    -1,
    1915,   104,   105,   106,   107,   108,   109,   110,   111,   112,
    1110,    -1,    -1,    -1,  1341,  1342,  1540,   488,    -1,    -1,
      -1,   101,  1851,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1371,    -1,  1570,   226,   227,   129,
      -1,   230,    -1,    -1,   233,   234,    -1,   713,    -1,    -1,
    1584,    -1,    -1,  1390,    -1,    -1,    -1,    -1,    -1,   149,
     150,    -1,    -1,   153,   730,    -1,    -1,    -1,    -1,    -1,
     160,   161,    -1,  1607,    -1,    -1,  1413,    -1,    -1,    -1,
      -1,    -1,    -1,   173,    -1,    -1,    -1,    -1,   754,    -1,
      -1,   101,    -1,   759,   104,   105,   106,   107,   108,   109,
     110,   111,   112,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     776,   777,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,
       1,    -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,   795,
     319,   106,   107,   108,   109,   110,   111,   112,   113,   149,
     150,    -1,   117,   153,   119,   334,    -1,    -1,  1485,  1486,
     160,   161,    -1,    -1,    -1,    -1,    -1,   823,    -1,  1693,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1505,    -1,
      -1,    -1,    -1,    -1,    -1,   150,    -1,    58,   153,    -1,
      -1,    -1,    -1,    -1,  1521,    -1,    -1,    -1,   854,    -1,
      -1,    -1,    -1,    -1,    -1,   861,    -1,    -1,    -1,    -1,
       1,   867,  1539,  1540,    -1,    -1,    -1,    -1,    -1,    -1,
     876,    -1,    -1,   879,    -1,   881,    -1,    -1,    -1,   101,
     886,   102,   104,   105,   106,   107,   108,   109,   110,   111,
     112,    -1,    -1,  1570,    -1,    -1,    -1,    -1,    -1,   428,
      -1,    -1,    -1,    -1,  1581,    -1,    -1,  1584,   729,   730,
      -1,     1,    -1,    -1,     4,  1592,    -1,    58,   739,  1369,
      -1,   742,    -1,   929,   145,    -1,    -1,   149,    -1,    -1,
    1607,    -1,    -1,    -1,    -1,    12,    13,    14,    15,    16,
      -1,   162,    -1,    -1,    -1,  1622,    -1,    -1,    -1,    -1,
    1627,  1628,    -1,    -1,    -1,  1632,  1633,  1831,    -1,    -1,
      -1,   102,    -1,    -1,    -1,    -1,   495,    -1,    58,    -1,
      -1,   192,    -1,  1847,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   803,    -1,   990,    -1,   807,    -1,    -1,    -1,
     811,    -1,    82,    70,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1007,  1008,    -1,   145,  1682,    -1,    -1,    -1,  1883,
      -1,    -1,   102,    -1,    -1,    -1,  1693,    -1,    -1,    -1,
      -1,   162,    -1,    -1,   101,    -1,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   267,    -1,    -1,   139,
      -1,   192,   129,    -1,  1060,   145,    -1,   147,    -1,    -1,
     101,    -1,   283,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   149,   150,    -1,    -1,    -1,    -1,    -1,    -1,
     609,   610,  1532,   160,   161,    -1,    -1,    -1,   129,   179,
      -1,  1541,    -1,   622,  1100,    -1,    -1,    -1,    -1,    -1,
     321,   191,  1108,   324,    -1,    -1,    -1,    -1,   149,   150,
      -1,    -1,    -1,    -1,  1791,    -1,    -1,   338,    -1,   160,
     161,   342,    -1,    -1,    -1,    -1,    -1,    -1,  1578,    -1,
      -1,  1137,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1816,
      -1,    -1,   283,    -1,    -1,    -1,    -1,    -1,    -1,  1155,
      -1,    -1,   242,    -1,  1831,   245,    -1,    -1,    -1,    -1,
     250,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1847,  1848,    -1,   994,  1851,    -1,  1182,    -1,    -1,    -1,
     321,    -1,    -1,   324,    -1,  1191,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   283,    -1,    -1,    -1,   338,    -1,    -1,
      -1,   342,    -1,  1209,   733,   734,  1883,    -1,    -1,    -1,
     739,    -1,   302,    -1,    -1,    -1,    -1,    -1,   439,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1676,    -1,    -1,    -1,
    1680,   760,    -1,    -1,   763,   764,    -1,   766,  1915,   768,
     769,    -1,  1692,    -1,  1065,    -1,    -1,  1068,  1698,  1255,
    1256,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1269,  1270,    -1,    -1,    -1,  1274,  1275,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   807,    -1,
      -1,    -1,   811,   101,    -1,   375,   104,   105,   106,   107,
     108,   109,   110,   111,   112,    -1,  1302,  1303,   439,   520,
      -1,    -1,    -1,    -1,    -1,    -1,  1312,  1313,  1314,  1315,
      -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   409,
      -1,    -1,  1772,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     551,   149,   150,    -1,    -1,  1341,  1342,    -1,    -1,   560,
      -1,   562,   160,   161,    -1,   435,  1796,    -1,    -1,    -1,
      -1,    -1,   573,    -1,   575,    99,    -1,   101,   887,   103,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   590,
     591,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   520,
      -1,    -1,    -1,    -1,  1390,    -1,  1836,    -1,    -1,    -1,
      -1,    -1,   101,   614,  1844,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,   149,   627,  1413,   152,   153,
     551,    -1,    -1,  1863,    -1,  1865,  1866,    -1,    -1,   560,
      -1,   562,    -1,    -1,   514,   515,    -1,    -1,    -1,    -1,
    1251,    -1,   573,    -1,   575,  1885,    -1,    -1,    -1,  1260,
     149,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   590,
     591,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1908,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   556,    -1,  1918,    -1,
     560,    -1,   562,   614,    -1,    -1,    -1,    -1,    -1,  1485,
    1486,    -1,    -1,    -1,    -1,   575,   627,    -1,  1017,    -1,
      -1,    -1,   713,  1943,    -1,  1945,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1511,    -1,    -1,    -1,    -1,
      -1,  1961,    -1,    -1,    -1,  1521,  1966,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1976,    -1,    -1,    -1,
      -1,    -1,    -1,   754,  1540,    -1,  1065,    -1,    -1,  1068,
      -1,    -1,    -1,    -1,    -1,   635,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   776,   777,    -1,   648,    -1,
     650,   651,    -1,   653,  1570,    -1,   656,    -1,    -1,   659,
     660,   661,   713,    -1,   795,  1581,    -1,    -1,  1584,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1592,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   823,    -1,    12,    13,    14,    15,    16,    -1,
      -1,    -1,    -1,   754,    -1,    -1,  1622,    -1,    -1,    -1,
      -1,  1627,  1628,    -1,    -1,    -1,  1632,  1633,    -1,    -1,
      -1,    -1,    -1,   854,    -1,   776,   777,    -1,    -1,    -1,
     861,    -1,  1648,    -1,    -1,    -1,    -1,   737,    -1,    -1,
      -1,    -1,    -1,    -1,   795,   876,    -1,    -1,   879,  1188,
     881,    -1,    70,    -1,    -1,    -1,    -1,  1196,  1197,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1499,    -1,
      -1,    -1,   823,    -1,    -1,    -1,    -1,  1693,    -1,    -1,
      -1,    -1,    -1,   101,    -1,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,    -1,    -1,    -1,   929,    -1,
      -1,    -1,    -1,   854,    -1,    -1,    -1,    -1,    -1,    -1,
     861,   129,  1251,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1260,    -1,    -1,  1263,   876,  1265,  1266,   879,    -1,
     881,   149,   150,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   854,    -1,    -1,    -1,    -1,   990,
      -1,    -1,    -1,    -1,    -1,    -1,  1305,   867,   868,    -1,
      -1,    -1,    -1,    -1,    -1,  1791,  1007,  1008,   929,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1816,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1831,  1647,  1648,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1060,
      -1,  1847,  1848,  1372,    -1,    -1,    -1,    -1,    -1,   990,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1007,  1008,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1883,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1108,    -1,    -1,
      -1,    -1,  1898,    86,    -1,    -1,    -1,    -1,    -1,    92,
      93,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1915,
      -1,    -1,    -1,    -1,    -1,    -1,  1137,    -1,    -1,  1060,
      -1,    -1,    -1,    -1,    -1,  1454,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   126,  1155,  1756,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1483,    -1,    -1,    -1,    -1,    -1,
      -1,  1182,    -1,    -1,    -1,    -1,    -1,  1108,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1209,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1137,    -1,    -1,  1820,
      -1,    -1,    -1,  1824,    -1,    -1,    -1,    -1,    -1,    -1,
    1100,    -1,    -1,    -1,  1155,    -1,    -1,    -1,  1108,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1850,
      -1,    -1,    -1,    -1,  1255,  1256,    -1,    -1,    -1,    -1,
      -1,  1182,     0,    -1,    -1,     3,    -1,  1137,  1269,  1270,
      -1,  1580,    -1,  1274,  1275,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1155,    -1,    -1,  1209,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1897,  1898,    -1,    -1,
      -1,  1302,  1303,    -1,    -1,  1175,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   292,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1930,
      -1,    -1,    -1,    -1,  1255,  1256,    -1,    -1,    76,    -1,
    1341,  1342,    -1,  1213,    -1,  1654,  1655,    -1,  1269,  1270,
      -1,    -1,    -1,  1274,  1275,    -1,    -1,  1666,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1302,  1303,    -1,    -1,    -1,    -1,    -1,    -1,  1390,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1341,  1342,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1312,  1313,  1314,  1315,  1316,  1317,    -1,  1758,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   438,    -1,   440,    -1,  1390,
      -1,  1341,  1342,    -1,    -1,    -1,   449,   450,    -1,    -1,
      -1,    -1,    -1,    -1,  1485,  1486,    -1,    -1,    -1,    -1,
      -1,   229,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   244,    -1,    -1,    -1,
      -1,  1820,    -1,    -1,    -1,    -1,   254,    -1,    -1,    -1,
    1521,    -1,    -1,    -1,    -1,    -1,   264,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1540,
     278,   279,    -1,  1413,    -1,    -1,    -1,   285,   286,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   301,  1485,  1486,    -1,    -1,    -1,  1570,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1581,    -1,   320,    -1,   557,    -1,    98,    -1,    -1,    -1,
      -1,  1592,    -1,    -1,    -1,    -1,    -1,   109,    -1,   111,
    1521,   113,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1540,
      -1,  1622,    -1,    -1,    -1,    -1,  1627,  1628,    -1,    -1,
      -1,  1632,  1633,    -1,    -1,  1505,    -1,    -1,   376,   151,
      -1,   153,   154,    -1,    -1,    -1,    -1,    -1,    -1,  1570,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1581,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   407,
    1540,  1592,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   195,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1693,    -1,    -1,   433,    -1,    -1,    -1,   437,
    1570,  1622,    -1,    -1,    -1,    -1,  1627,  1628,    -1,    -1,
      -1,  1632,  1633,    -1,  1584,    -1,    -1,    -1,   456,    -1,
      -1,    -1,   460,   461,    -1,    -1,   464,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1607,    -1,    -1,
      -1,   479,   480,   481,   482,    -1,   258,    -1,   260,   261,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     498,    -1,    -1,    -1,    -1,    -1,    -1,    47,   506,    -1,
      -1,    -1,  1693,    -1,    -1,    -1,   288,    -1,    -1,    -1,
      -1,    -1,   294,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1791,    -1,    -1,    73,    -1,    -1,   534,   770,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   323,    -1,    -1,    -1,  1816,    -1,   329,    -1,   331,
      -1,    -1,    -1,  1693,    -1,    -1,    -1,   565,    -1,    -1,
    1831,    -1,    -1,    -1,   572,    -1,    -1,    -1,    -1,   119,
     578,    -1,    -1,    -1,    -1,    -1,  1847,  1848,    -1,    -1,
      -1,    -1,   132,    -1,   134,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   601,   602,    -1,    -1,    -1,    -1,    -1,
    1791,    -1,    -1,    -1,   847,   848,    -1,    -1,    -1,    -1,
      -1,    -1,  1883,    -1,   164,   858,   859,   860,    -1,    -1,
     863,    -1,    -1,    -1,    -1,  1816,    -1,    -1,    -1,    -1,
      -1,   181,    -1,    -1,    -1,    -1,    -1,   419,    -1,    -1,
    1831,    -1,    -1,    -1,  1915,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   662,    -1,  1847,  1848,    -1,    -1,
      -1,    -1,    -1,   445,    -1,   447,   448,    -1,    -1,   219,
      -1,    -1,    -1,   223,    -1,    -1,   226,   227,    -1,    -1,
     230,    -1,    -1,   233,   234,    -1,    -1,    -1,    -1,    -1,
      -1,  1831,  1883,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     943,    -1,    -1,    -1,    -1,    -1,    -1,  1847,    -1,    -1,
     492,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   729,    -1,    -1,  1915,    -1,    -1,    -1,    -1,    -1,
     512,    -1,    -1,    -1,    -1,   517,   744,   519,    -1,    -1,
     748,    -1,    -1,  1883,   987,   295,    -1,    -1,   298,   757,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   539,    -1,   541,
     542,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   319,
      -1,   779,    -1,    -1,    -1,    -1,    -1,   559,    -1,    -1,
     788,    -1,    -1,    -1,   334,    -1,   794,    -1,    -1,   571,
      -1,  1034,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1043,  1044,  1045,  1046,    -1,    -1,    -1,    -1,  1051,  1052,
      -1,   593,    -1,   595,   596,    -1,    -1,    -1,  1061,    -1,
      -1,    -1,    -1,   831,    -1,    -1,    -1,    -1,    -1,    -1,
     838,    -1,    -1,    -1,    -1,   617,   618,    -1,    -1,  1082,
      -1,  1084,   624,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   865,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   428,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1137,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   919,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1163,    -1,    -1,    -1,    -1,    -1,    -1,  1170,    -1,  1172,
    1173,    -1,    -1,   483,    -1,    -1,    -1,    -1,  1181,    -1,
    1183,    -1,  1185,    -1,  1187,   495,    -1,    -1,    -1,  1192,
      12,    13,    14,    15,    16,    -1,    -1,    19,    -1,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    -1,    49,    50,    51,
      -1,    53,  1000,    -1,    -1,    -1,  1004,    -1,    -1,    -1,
      -1,    -1,    -1,  1011,    -1,    -1,    -1,  1250,    70,    -1,
      -1,    -1,    -1,  1021,  1257,  1258,    -1,    -1,    -1,   569,
    1028,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1037,
      -1,  1039,    -1,    -1,    -1,    -1,    -1,    -1,  1281,    -1,
      -1,    -1,   178,    -1,    -1,  1288,    -1,    -1,  1291,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   609,
     610,    -1,    -1,  1071,   846,    -1,    -1,  1075,    -1,    -1,
      -1,    -1,   622,    -1,    -1,   857,    -1,    -1,  1321,    -1,
      -1,  1089,    -1,    -1,  1092,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   878,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   889,    -1,    -1,
      12,    13,    14,    15,    16,    -1,   898,    19,  1361,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    -1,    -1,    -1,    -1,    50,    51,
      -1,  1394,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1402,
      -1,  1404,    -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,
      -1,    -1,  1180,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   733,   734,    -1,    -1,    -1,    -1,   739,
      -1,    -1,    -1,    -1,    -1,    -1,  1204,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1450,  1451,    -1,
     760,    -1,   994,   763,   764,    -1,   766,    -1,   768,   769,
      -1,    -1,  1465,  1466,   360,  1468,    -1,   363,   364,    -1,
      -1,    -1,    -1,    -1,  1477,    -1,  1018,   373,   374,    -1,
      -1,    -1,    -1,    -1,  1487,  1488,    -1,    -1,    -1,    -1,
      -1,    -1,   388,   389,    -1,    -1,    -1,   807,    -1,    -1,
      -1,   811,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   409,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1294,    -1,    -1,    -1,
    1298,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   439,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1328,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   887,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1593,  1594,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1603,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    -1,
    1378,    -1,  1154,  1381,    -1,    -1,    -1,    -1,    -1,    98,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1396,    -1,
      -1,    -1,    -1,    -1,    -1,  1638,  1639,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1186,    -1,    -1,    -1,    -1,    -1,
    1192,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   147,    -1,
      -1,    -1,   151,    -1,    -1,    -1,    -1,    -1,  1446,    -1,
      -1,    -1,    -1,   162,    -1,    -1,    -1,  1455,    -1,    -1,
      -1,  1459,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     179,    -1,    -1,    -1,    -1,  1473,  1474,  1017,    -1,    -1,
      -1,    -1,    -1,   192,  1717,    -1,   195,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1735,    -1,    -1,  1738,  1739,    -1,    -1,    -1,
      -1,    -1,  1745,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1065,    -1,    -1,  1068,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   245,    -1,    -1,    -1,
      82,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   258,
      -1,  1323,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   690,   691,   692,   693,   694,   695,
     696,   697,   698,   699,   700,   701,   702,   703,   704,   705,
     706,   707,   708,    -1,    -1,   294,    -1,  1585,  1586,    -1,
      -1,    -1,    -1,   302,    -1,    -1,    -1,    -1,    -1,  1371,
      -1,    -1,    -1,    -1,    -1,   147,    -1,    -1,    -1,    -1,
      -1,    -1,   321,    -1,   323,    -1,  1849,    -1,    -1,    -1,
     162,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   179,    -1,    -1,
      -1,    -1,    -1,    -1,   770,    -1,    -1,    -1,  1188,    -1,
     192,    -1,    -1,    -1,    -1,    -1,  1196,  1197,    -1,    -1,
      -1,    -1,  1895,    -1,    -1,    -1,   375,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1919,    -1,    -1,    -1,
      -1,    -1,    -1,  1926,    -1,    -1,    -1,    -1,    -1,    -1,
     409,    -1,    -1,   245,    -1,  1703,    -1,    -1,  1941,    -1,
      -1,  1251,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1260,    -1,    -1,  1263,    -1,  1265,  1266,  1725,    -1,    -1,
     439,    -1,    -1,    -1,    -1,    -1,   445,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1743,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     302,    -1,    -1,    -1,    -1,  1305,    -1,  1539,    -1,    -1,
      -1,    -1,  1770,    -1,    -1,    -1,    -1,    -1,    -1,   321,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1787,
      -1,    -1,  1790,    -1,    -1,    -1,    -1,    -1,    -1,   925,
      -1,    -1,    -1,    -1,   930,   514,   515,    -1,    -1,    -1,
      -1,   520,    -1,    -1,    -1,   941,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1372,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   981,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   573,    -1,    -1,   409,    -1,    -1,
     162,    -1,    -1,    -1,    -1,    -1,  1874,    -1,    -1,    -1,
      -1,    -1,   591,    -1,   593,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1665,    -1,    -1,    -1,   439,    -1,   191,
     192,    -1,    -1,    -1,    -1,   614,    -1,    -1,    -1,    -1,
    1682,    -1,    -1,    -1,  1454,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   635,    -1,    -1,    -1,
      -1,   223,    -1,    -1,    -1,    -1,    -1,    -1,   230,   648,
      -1,   650,   651,  1483,   653,    -1,    -1,   656,    -1,    -1,
     659,   660,   661,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1509,
      -1,    -1,   514,   515,    -1,  1515,    -1,    -1,   520,    -1,
      -1,    -1,  1108,    -1,  1756,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   713,    -1,   298,    -1,    -1,    -1,
      -1,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   737,   321,
     322,   573,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1580,    -1,    -1,    -1,    -1,   754,    90,    -1,    -1,   591,
     342,    -1,    -1,    -1,    -1,    -1,  1182,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   776,   777,    -1,
      -1,    -1,   614,    -1,    -1,    -1,    -1,    -1,    -1,  1851,
      -1,    -1,    -1,    -1,    -1,    -1,   795,    -1,    -1,    -1,
    1216,  1217,  1218,   635,    -1,    -1,   140,  1223,  1224,   143,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   156,    -1,  1654,  1655,    -1,    -1,    -1,  1245,
     412,    -1,  1662,    -1,    -1,    -1,  1666,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   428,   429,    -1,   431,
     432,    -1,    -1,    -1,    -1,    -1,    -1,   439,    -1,    -1,
      -1,   443,   861,    -1,    -1,    -1,  1282,  1283,  1930,   868,
      -1,    -1,    -1,    -1,    -1,    -1,   210,    -1,    -1,   878,
      -1,   713,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   484,    -1,    -1,   737,   488,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   255,   754,    -1,    -1,    -1,    -1,    -1,  1758,    -1,
     929,   265,    -1,    -1,    -1,    -1,    -1,    -1,   520,    -1,
      -1,    -1,   276,    -1,   776,   777,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   795,    -1,    -1,    -1,    -1,    -1,   303,
      -1,    -1,    -1,    -1,    -1,    -1,   310,   311,    -1,    -1,
      -1,   315,    -1,    -1,    -1,    -1,    -1,    -1,   570,    -1,
    1820,   573,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   590,   591,
      -1,    -1,    -1,   347,    -1,    -1,    -1,    -1,   352,   601,
      -1,   355,    -1,   605,    -1,    -1,    -1,    -1,    -1,   861,
     612,    -1,   614,    -1,    -1,    -1,   868,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   179,    -1,    -1,    -1,
      -1,  1060,    -1,    -1,    -1,    -1,    -1,  1897,    -1,   192,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   205,    -1,   207,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   929,    -1,    -1,
      -1,  1100,    -1,    -1,    -1,    -1,    -1,    -1,   442,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     454,   455,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   713,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   729,   730,    -1,
      -1,    -1,    -1,    -1,    -1,  1154,    -1,   739,   740,  1575,
     742,   743,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     293,    -1,   754,    -1,    -1,   757,  1175,   759,   760,    -1,
      -1,    -1,    -1,  1182,   766,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   776,   777,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1209,    -1,    -1,   795,  1213,    -1,    -1,   799,    -1,    -1,
      -1,   803,    -1,    -1,    -1,   807,   808,    -1,  1060,   811,
     812,    -1,    -1,    -1,    -1,    -1,    -1,   819,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   585,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1274,  1275,    -1,    -1,   861,
     862,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   633,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   889,    -1,    -1,
      -1,    -1,    -1,  1312,  1313,  1314,    -1,  1316,  1317,    -1,
      -1,    -1,    -1,    -1,  1323,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   465,  1175,    -1,    -1,    -1,   929,   471,    -1,
    1182,    -1,  1768,   476,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1371,    -1,    -1,    -1,    -1,  1209,    -1,    -1,
      -1,    -1,  1798,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1390,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   994,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1011,
    1012,    -1,    -1,    -1,    -1,    -1,  1018,   771,    -1,    -1,
     563,    -1,  1274,  1275,   778,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1879,  1880,    -1,    -1,    -1,   591,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1060,    -1,
      -1,   604,    -1,  1065,  1066,    -1,  1068,  1069,  1904,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1505,    -1,    -1,    -1,
      -1,    -1,  1928,    -1,    -1,    -1,    -1,    -1,    -1,   853,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   655,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1539,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     673,   674,    -1,    -1,    -1,    -1,  1972,    -1,  1390,    -1,
     683,    -1,   685,   686,    -1,    -1,    -1,    -1,    -1,    -1,
     904,    -1,    -1,    -1,    -1,   909,    -1,    -1,    -1,    -1,
      -1,    -1,  1581,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     713,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1182,    -1,    -1,   726,    -1,    -1,  1188,  1189,  1607,    -1,
       5,    -1,    -1,    -1,   737,    -1,    -1,    12,    13,    14,
      15,    16,    -1,    -1,    -1,    -1,    -1,  1209,   751,    -1,
      -1,   754,    -1,  1632,  1633,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    -1,    -1,    -1,    52,   781,    54,
      -1,   784,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1251,
    1252,    -1,    -1,  1505,    -1,    70,    71,    -1,  1260,  1261,
      -1,  1263,    -1,  1682,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1274,  1275,    -1,    -1,    -1,   820,    -1,    -1,
      -1,    -1,    -1,    98,    99,    -1,   101,    -1,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,   117,   118,   119,    -1,   121,   122,    -1,    -1,
      -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   868,    -1,    -1,    -1,  1581,
      -1,    -1,    -1,    -1,   149,   878,   879,   152,   153,    -1,
      -1,    -1,    -1,   886,   159,   160,   161,   162,   163,   164,
     165,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1113,
    1114,  1115,    -1,    -1,   907,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1791,   916,    -1,    -1,    -1,    -1,    -1,    -1,
    1632,  1633,    -1,    -1,    -1,    -1,   929,    -1,  1390,  1143,
      -1,    -1,    -1,    -1,   937,    -1,    -1,  1816,    -1,    -1,
      -1,   944,    -1,    -1,  1158,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1848,
      -1,    -1,  1851,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1195,    -1,    -1,    -1,   988,    -1,    -1,    -1,    -1,
      -1,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    -1,    19,    -1,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    -1,    -1,    48,  1499,    50,    51,
      52,    -1,    54,    55,    56,    57,    58,    59,    60,    61,
      62,    63,  1055,  1515,  1057,    67,  1059,    -1,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    98,    99,   100,   101,
      -1,   103,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,  1816,   117,   118,   119,    -1,   121,
     122,    -1,  1326,    -1,    -1,  1329,    -1,   129,    -1,  1581,
      -1,    -1,  1125,  1126,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,    -1,   151,
     152,   153,    -1,    -1,    -1,    -1,    -1,   159,   160,   161,
     162,   163,   164,   165,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   173,   174,    -1,    -1,     5,    -1,    -1,    -1,    -1,
    1632,  1633,    12,    13,    14,    15,    16,    -1,    -1,    -1,
      -1,    -1,    -1,  1186,    -1,  1647,  1648,    -1,    -1,  1192,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1663,    -1,    -1,    -1,    -1,  1209,    -1,    48,    -1,
      -1,    -1,    52,    -1,    54,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1226,    -1,    -1,    -1,    -1,    -1,    -1,
      70,    71,    -1,    -1,    -1,    -1,  1239,    -1,    -1,  1242,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    98,    99,
      -1,   101,    -1,   103,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,    -1,   117,   118,   119,
      -1,   121,   122,    -1,    -1,    -1,    -1,    -1,    -1,   129,
    1293,    -1,    -1,    -1,  1756,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1764,    -1,  1518,    -1,    -1,    -1,    -1,   149,
      -1,    -1,   152,   153,    -1,    -1,    -1,    -1,    -1,   159,
     160,   161,   162,   163,   164,   165,    -1,    -1,  1331,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1349,    -1,    -1,  1352,
      -1,    -1,    -1,    -1,  1816,    -1,    -1,    -1,  1820,  1821,
      -1,    -1,  1824,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1600,  1390,  1850,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1400,  1401,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1425,    -1,  1427,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1897,  1898,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    -1,    -1,    19,  1930,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    -1,    49,    50,    51,
    1714,    53,  1505,    -1,    -1,    -1,    -1,  1510,    -1,  1723,
      12,    13,    14,    15,    16,    -1,    -1,    19,    70,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    -1,    49,    50,    51,
      -1,    53,   104,   105,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1566,    -1,    -1,    -1,    -1,    70,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     152,    -1,  1605,    -1,    -1,  1608,     3,     4,     5,     6,
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
      -1,   148,   149,    -1,    -1,   152,   153,    -1,    -1,    -1,
     157,    -1,   159,   160,   161,   162,   163,   164,   165,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   174,    -1,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    -1,    19,  1799,    21,    22,    23,
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
      -1,    -1,    -1,    -1,    -1,   149,    -1,    -1,   152,   153,
      -1,    -1,    -1,   157,    -1,   159,   160,   161,   162,   163,
     164,   165,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,    -1,    -1,
     152,   153,    -1,    -1,    -1,    -1,    -1,   159,   160,   161,
     162,   163,   164,   165,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   173,   174,     3,     4,     5,     6,     7,     8,     9,
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
     160,   161,   162,   163,   164,   165,     3,     4,     5,     6,
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
     159,   160,   161,   162,   163,   164,   165,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    -1,    19,    -1,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      -1,    -1,    -1,    -1,    50,    51,    -1,    -1,    -1,    55,
      56,    57,    58,    59,    60,    61,    62,    63,    -1,    -1,
      -1,    67,    -1,    -1,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      96,    -1,    -1,    -1,   100,   101,    -1,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,    -1,    -1,    -1,
     116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   149,   150,    -1,   152,   153,    -1,    -1,
      -1,    -1,    -1,    -1,   160,   161,     3,     4,     5,     6,
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
     157,    -1,    -1,   160,   161,     3,     4,     5,     6,     7,
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
       9,    10,    11,    12,    13,    14,    15,    16,    17,    -1,
      19,    -1,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    -1,    -1,    -1,
      -1,    50,    51,    -1,    -1,    -1,    55,    56,    57,    58,
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
      10,    11,    12,    13,    14,    15,    16,    -1,    -1,    19,
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
     150,    -1,   152,   153,    -1,    -1,     3,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,   104,   105,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,    -1,    -1,
      -1,    -1,    -1,   152,   153,    -1,    12,    13,    14,    15,
      16,   160,   161,    19,    -1,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    -1,    49,    50,    51,    -1,    53,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,   105,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   152,   153,    -1,    -1,
      -1,    -1,    -1,    -1,   160,   161,     4,     5,     6,     7,
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
      -1,   149,   150,    -1,   152,   153,    -1,    -1,    -1,   157,
      -1,    -1,   160,   161,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    -1,    -1,    19,
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
      17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,
     150,    48,   152,   153,    -1,    52,    -1,    54,    -1,    -1,
     160,   161,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    69,    -1,    71,    -1,    -1,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    91,    92,    93,    94,    -1,    96,
      -1,    98,    99,    -1,   101,    -1,   103,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,    -1,   121,   122,    -1,    -1,    -1,    -1,
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    17,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   149,    -1,    -1,   152,   153,    -1,    -1,    -1,
     157,    -1,   159,   160,   161,   162,   163,   164,   165,    48,
      -1,    -1,    -1,    52,    -1,    54,    -1,   174,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      69,    -1,    71,    72,    -1,    74,    -1,    -1,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,    87,    88,
      89,    90,    91,    92,    93,    94,    -1,    96,    -1,    98,
      99,    -1,   101,    -1,   103,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,    -1,   121,   122,    -1,    -1,    -1,    -1,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    17,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     149,    -1,    -1,   152,   153,    -1,    -1,    -1,   157,    -1,
     159,   160,   161,   162,   163,   164,   165,    48,    -1,    -1,
      -1,    52,    -1,    54,    -1,   174,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    69,    -1,
      71,    -1,    -1,    74,    -1,    -1,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    90,
      91,    92,    93,    94,    -1,    96,    -1,    98,    99,    -1,
     101,    -1,   103,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,    -1,
     121,   122,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,    -1,
      -1,   152,   153,    -1,    -1,    -1,   157,    -1,   159,   160,
     161,   162,   163,   164,   165,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   174,     4,     5,     6,     7,     8,     9,
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
      -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,   129,
      -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   151,   152,   153,    -1,    -1,   104,   105,    -1,    -1,
     160,   161,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    -1,    19,    -1,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,   152,    -1,    -1,    -1,    50,    51,
      -1,    -1,    -1,    55,    56,    57,    58,    59,    60,    61,
      62,    -1,    12,    13,    14,    15,    16,    17,    70,    19,
      -1,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    -1,    -1,    -1,    -1,
      50,    51,   104,   105,    -1,    55,    56,    57,    58,    59,
      60,    61,    62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      70,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,   131,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     152,   153,    -1,    -1,   104,   105,    -1,    -1,   160,   161,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    -1,    19,    -1,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,   152,    -1,    -1,    -1,    50,    51,    -1,    -1,
      -1,    55,    56,    57,    58,    59,    60,    61,    62,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     104,   105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   129,    -1,   131,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,   153,
      -1,    -1,    -1,    -1,    -1,    -1,   160,   161,     4,     5,
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
      -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
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
      10,    11,    12,    13,    14,    15,    16,    -1,    -1,    19,
      -1,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    -1,    49,
      50,    51,    -1,    53,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   104,   105,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   152,   153,    -1,    -1,    -1,    -1,    -1,    -1,
     160,   161,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    -1,    -1,    48,    -1,
      50,    51,    52,    -1,    54,    55,    56,    57,    58,    59,
      60,    61,    62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    83,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    98,    99,
      -1,   101,    -1,   103,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,    -1,   117,   118,   119,
      -1,   121,   122,    -1,    -1,    -1,    -1,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,
      -1,    -1,   152,   153,    -1,    -1,    -1,   157,    -1,   159,
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
      -1,    -1,    -1,   149,    -1,   151,   152,   153,    -1,    -1,
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
      -1,    -1,    -1,    -1,    50,    51,    -1,    -1,    -1,    55,
      56,    57,    58,    59,    60,    61,    62,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   149,   150,    -1,   152,   153,    -1,    -1,
      -1,   157,    -1,    -1,   160,   161,    12,    13,    14,    15,
      16,    17,    -1,    19,    -1,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      -1,    -1,    -1,    -1,    50,    51,    -1,    -1,    -1,    55,
      56,    57,    58,    59,    60,    61,    62,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   149,   150,    -1,   152,   153,    -1,    -1,
      -1,    -1,    -1,    -1,   160,   161,    12,    13,    14,    15,
      16,    17,    -1,    19,    -1,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      -1,    -1,    -1,    -1,    50,    51,    -1,    -1,    -1,    55,
      56,    57,    58,    59,    60,    61,    62,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   152,   153,    -1,    12,
      13,    14,    15,    16,   160,   161,    19,    -1,    21,    22,
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
     153,    -1,    -1,    -1,    -1,    -1,    -1,   160,   161,     3,
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
      62,    -1,    12,    13,    14,    15,    16,    17,    70,    19,
      -1,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    -1,    -1,   152,    -1,
      50,    51,   104,   105,    -1,    55,    56,    57,    58,    59,
      60,    61,    62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      70,    -1,    -1,    -1,    -1,    -1,    76,   129,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,
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
      -1,    -1,   152,    -1,    -1,    12,    13,    14,    15,    16,
     160,   161,    19,    -1,    21,    22,    23,    24,    25,    26,
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
      -1,    -1,    -1,    -1,    -1,   152,   153,   104,   105,    -1,
      -1,    -1,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   152,    -1,    -1,    -1,    -1,
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
      -1,    -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   151,   152,     4,     5,     6,     7,     8,     9,
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
      -1,   131,    -1,    -1,    -1,    -1,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      -1,    19,   152,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    -1,    -1,
      -1,    -1,    50,    51,    -1,    -1,    -1,    55,    56,    57,
      58,    59,    60,    61,    62,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,   104,   105,    19,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    -1,    49,    50,
      51,    -1,    53,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   152,    -1,    -1,    -1,    -1,    70,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   104,   105,    -1,    -1,    -1,    -1,    12,
      13,    14,    15,    16,    17,    -1,    19,    -1,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    -1,    -1,    -1,    -1,    50,    51,    -1,
      -1,   152,    55,    56,    57,    58,    59,    60,    61,    62,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      -1,    -1,    -1,    12,    13,    14,    15,    16,    -1,    -1,
      19,    -1,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,   152,
      49,    50,    51,    -1,    53,    -1,    -1,    48,    -1,    -1,
      -1,    52,    -1,    54,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   104,   105,    98,    99,    -1,
     101,    -1,   103,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,   117,   118,   119,    -1,
     121,   122,    -1,    -1,    -1,    -1,    48,    -1,   129,    -1,
      52,    -1,    54,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   152,    -1,    -1,    -1,    -1,   149,    71,
      -1,   152,   153,    -1,    -1,    -1,    -1,    -1,   159,   160,
     161,   162,   163,   164,   165,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   173,   174,    -1,    -1,    98,    99,    -1,   101,
      -1,   103,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,    -1,   117,   118,   119,    48,   121,
     122,    -1,    52,    -1,    54,    -1,    -1,   129,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    71,    -1,    -1,    -1,    -1,    -1,   149,   150,    -1,
     152,   153,    -1,    -1,    -1,   157,    -1,   159,   160,   161,
     162,   163,   164,   165,    -1,    -1,    -1,    -1,    98,    99,
      -1,   101,    -1,   103,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,    -1,   117,   118,   119,
      48,   121,   122,    -1,    52,    -1,    54,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,   149,
     150,    -1,   152,   153,    -1,    -1,    -1,   157,    -1,   159,
     160,   161,   162,   163,   164,   165,    -1,    -1,    -1,    -1,
      98,    99,    -1,   101,    -1,   103,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,    -1,   117,
     118,   119,    48,   121,   122,    -1,    52,    -1,    54,    -1,
      -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    71,    -1,    -1,    -1,    -1,
      -1,   149,   150,   151,   152,   153,    -1,    -1,    -1,    -1,
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
     152,   153,    -1,    -1,    -1,   157,    -1,   159,   160,   161,
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
      -1,   149,    -1,    -1,   152,   153,    -1,    -1,   156,    -1,
      -1,   159,   160,   161,   162,   163,   164,   165,    -1,    -1,
      -1,    -1,    98,    99,    -1,   101,    -1,   103,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
      -1,   117,   118,   119,    48,   121,   122,    -1,    52,    -1,
      54,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    -1,    -1,
      -1,    -1,    -1,   149,   150,    -1,   152,   153,    -1,    -1,
      -1,    -1,    -1,   159,   160,   161,   162,   163,   164,   165,
      -1,    -1,    -1,    -1,    98,    99,    -1,   101,    -1,   103,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,   117,   118,   119,    48,   121,   122,    -1,
      52,    -1,    54,    -1,    -1,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,
      -1,    -1,    -1,    -1,    -1,   149,    -1,    -1,   152,   153,
      -1,    -1,    -1,   157,    -1,   159,   160,   161,   162,   163,
     164,   165,    -1,    -1,    -1,    -1,    98,    99,    -1,   101,
      -1,   103,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,    -1,   117,   118,   119,    48,   121,
     122,    -1,    52,    -1,    54,    -1,    -1,   129,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    71,    -1,    -1,    -1,    -1,    -1,   149,    -1,   151,
     152,   153,    -1,    -1,    -1,    -1,    -1,   159,   160,   161,
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
     118,   119,    -1,   121,   122,    -1,    -1,    -1,    -1,    -1,
      -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   149,    -1,    -1,   152,   153,    -1,    -1,    -1,    -1,
      -1,   159,   160,   161,   162,   163,   164,   165
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
     107,   223,   402,   382,   176,   176,   154,   350,   177,   177,
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
     151,   151,   151,   447,   402,   334,   213,   233,   234,   400,
     156,   176,   223,   235,   174,   151,    73,   222,   221,   144,
     165,   243,   174,   165,    73,   222,   174,   176,   176,   260,
     293,   295,   456,   156,   174,   153,   182,   265,   266,   267,
     223,   198,   188,    73,   106,   250,   252,   151,   462,   148,
     151,   151,   151,   352,   149,   402,   439,   440,   336,   131,
     155,   156,   270,   271,   277,    73,   174,   223,   150,   150,
     221,   222,   221,   223,   270,   260,   177,   149,   196,   399,
     447,   180,   156,   101,   149,   151,   156,   155,    73,   151,
     223,   149,   223,   223,   148,   176,   213,   233,   236,   238,
     239,   277,   223,   165,   165,   165,   238,   177,   174,   257,
     295,   265,   154,   213,   174,   265,   267,   223,   221,   107,
     107,   350,   223,   228,   177,   236,   221,   150,   221,   221,
     177,   257,   212,   151,   156,   182,   151,   151,   156,   151,
     253,    73,   248,   177,   223,   148,   228,   148,   151,   225,
     182,   268,   149,   174,   268,   223,    73,   151,   225,   155,
     156,   213,   151,   223,   182,   180,   269,   151,   174,   151,
     155,   174,   180
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
     224,   225,   225,   226,   226,   227,   227,   227,   227,   227,
     228,   228,   229,   229,   230,   230,   230,   230,   230,   231,
     231,   232,   232,   232,   232,   233,   233,   233,   234,   234,
     235,   235,   236,   236,   237,   238,   238,   239,   239,   240,
     240,   240,   240,   240,   240,   240,   240,   240,   241,   241,
     242,   242,   242,   242,   242,   242,   242,   242,   242,   242,
     242,   242,   242,   242,   242,   242,   242,   242,   243,   243,
     243,   243,   244,   244,   244,   244,   244,   244,   244,   244,
     244,   244,   244,   244,   244,   244,   244,   244,   244,   244,
     244,   244,   245,   245,   246,   247,   248,   249,   249,   250,
     250,   251,   251,   252,   253,   253,   253,   253,   253,   253,
     254,   254,   255,   255,   255,   256,   256,   257,   257,   258,
     258,   258,   258,   259,   260,   260,   260,   260,   260,   261,
     262,   262,   263,   263,   263,   263,   263,   264,   264,   265,
     265,   266,   266,   267,   267,   268,   268,   268,   269,   269,
     270,   270,   271,   271,   272,   272,   273,   273,   274,   274,
     275,   275,   276,   276,   277,   277,   277,   278,   278,   279,
     279,   279,   279,   279,   280,   280,   280,   281,   281,   281,
     282,   282,   282,   282,   282,   283,   283,   284,   284,   285,
     285,   285,   286,   286,   286,   286,   286,   287,   287,   288,
     288,   288,   288,   289,   289,   290,   290,   290,   291,   291,
     291,   292,   292,   292,   293,   293,   293,   294,   294,   295,
     295,   296,   296,   297,   297,   297,   297,   297,   298,   299,
     299,   299,   300,   300,   301,   301,   301,   301,   301,   301,
     301,   301,   302,   302,   302,   302,   302,   302,   302,   302,
     302,   302,   302,   302,   302,   302,   302,   302,   302,   302,
     302,   302,   302,   302,   302,   302,   302,   302,   302,   302,
     303,   303,   304,   305,   305,   306,   306,   306,   306,   306,
     307,   307,   308,   308,   308,   308,   309,   309,   309,   309,
     309,   309,   310,   310,   310,   310,   311,   312,   311,   311,
     313,   313,   313,   313,   314,   314,   314,   315,   315,   315,
     315,   316,   316,   316,   317,   317,   317,   317,   317,   317,
     318,   318,   318,   319,   319,   320,   320,   322,   321,   323,
     321,   324,   321,   325,   321,   321,   326,   326,   327,   327,
     328,   328,   329,   329,   329,   330,   330,   330,   330,   330,
     330,   330,   330,   331,   331,   332,   332,   332,   332,   332,
     332,   332,   332,   332,   332,   333,   333,   333,   334,   334,
     334,   335,   335,   335,   336,   337,   337,   338,   338,   339,
     339,   340,   341,   342,   341,   341,   341,   343,   341,   341,
     341,   344,   344,   345,   345,   345,   345,   346,   346,   347,
     347,   347,   347,   347,   347,   347,   348,   348,   348,   348,
     349,   349,   350,   350,   350,   350,   351,   351,   351,   351,
     352,   352,   352,   352,   352,   353,   353,   353,   353,   353,
     354,   354,   355,   355,   356,   356,   357,   357,   357,   358,
     358,   358,   359,   359,   360,   360,   360,   360,   361,   361,
     362,   362,   362,   362,   362,   363,   363,   364,   364,   365,
     365,   365,   365,   365,   366,   366,   367,   367,   369,   368,
     370,   368,   368,   368,   371,   371,   371,   371,   372,   372,
     372,   372,   373,   373,   374,   374,   375,   375,   376,   376,
     376,   376,   377,   377,   377,   378,   378,   379,   379,   380,
     380,   381,   381,   382,   382,   383,   383,   383,   384,   384,
     385,   385,   386,   386,   387,   387,   388,   389,   390,   390,
     390,   390,   390,   391,   390,   392,   390,   393,   390,   394,
     390,   395,   390,   396,   396,   396,   397,   397,   398,   398,
     398,   398,   398,   398,   398,   398,   398,   398,   399,   399,
     399,   400,   401,   401,   402,   402,   403,   403,   404,   405,
     405,   406,   406,   406,   407,   407,   407,   407,   407,   407,
     408,   408,   409,   409,   409,   409,   410,   410,   410,   410,
     411,   411,   411,   411,   411,   411,   411,   412,   412,   412,
     412,   413,   413,   413,   414,   414,   414,   414,   414,   415,
     415,   415,   415,   416,   416,   416,   416,   416,   416,   417,
     417,   417,   418,   418,   418,   418,   418,   419,   419,   419,
     419,   420,   420,   420,   420,   420,   420,   421,   421,   422,
     422,   422,   422,   423,   423,   423,   423,   424,   424,   424,
     424,   424,   424,   424,   425,   425,   425,   425,   425,   426,
     426,   426,   426,   426,   427,   427,   427,   428,   428,   428,
     428,   429,   429,   429,   430,   430,   430,   430,   430,   431,
     431,   432,   432,   432,   433,   433,   434,   434,   435,   435,
     435,   436,   436,   436,   436,   436,   437,   437,   437,   437,
     438,   438,   438,   439,   439,   439,   439,   440,   440,   440,
     440,   441,   441,   441,   441,   442,   442,   442,   442,   442,
     443,   443,   443,   443,   444,   444,   444,   445,   445,   445,
     446,   446,   446,   446,   446,   446,   447,   447,   447,   448,
     448,   448,   448,   448,   449,   449,   449,   449,   450,   450,
     451,   451,   451,   452,   452,   453,   453,   453,   453,   453,
     453,   454,   454,   454,   454,   454,   454,   454,   454,   454,
     454,   455,   455,   455,   455,   456,   456,   456,   457,   457,
     458,   458,   458,   458,   458,   458,   459,   459,   459,   459,
     459,   459,   460,   460,   460,   461,   461,   462,   462,   463,
     463
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
       4,     2,     6,     1,     2,     1,     2,     1,     2,     1,
       1,     2,     2,     5,     3,     5,    10,     5,    10,     5,
       7,     1,     1,     1,     2,     1,     3,     1,     1,     3,
       3,     2,     1,     2,     2,     0,     1,     2,     3,     4,
       5,     7,     6,     7,     8,     4,     5,     7,     1,     3,
       4,     5,     4,     1,     2,     3,     5,     2,     3,     4,
       5,     7,     3,     5,     5,     7,     7,     7,     1,     1,
       1,     1,     3,     4,     2,     3,     3,     2,     3,     2,
       3,     3,     6,     2,     2,     3,     3,     3,     3,     3,
       3,     5,     1,     1,     5,     5,     4,     0,     1,     4,
       6,     1,     3,     4,     3,     5,     3,     3,     6,     7,
       3,     5,     3,     3,     4,     8,     9,     0,     2,     1,
       1,     1,     1,     2,     1,     2,     2,     2,     1,     3,
       1,     1,     6,     8,    10,    12,    14,     0,     1,     0,
       1,     1,     3,     4,     7,     0,     1,     3,     1,     3,
       0,     1,     1,     2,     0,     1,     4,     5,     0,     1,
       3,     4,     1,     3,     2,     2,     1,     7,     5,     1,
       1,     1,     1,     1,     2,     3,     6,     3,     3,     4,
       1,     2,     2,     3,     8,     8,     8,     5,     9,     2,
       2,     5,     3,     5,     4,     3,     4,     4,     7,     2,
       1,     1,     1,     3,     6,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     0,     1,     1,
       2,     1,     1,     1,     1,     1,     1,     1,     4,     1,
       2,     3,     1,     2,     1,     1,     1,     1,     1,     1,
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
       1,     2,     1,     1,     0,     2,     2,     4,     1,     4,
       0,     1,     2,     3,     4,     2,     2,     1,     2,     2,
       5,     5,     7,     6,     1,     3,     0,     2,     0,     5,
       0,     5,     3,     1,     0,     1,     1,     1,     1,     1,
       1,     1,     0,     1,     1,     2,     5,     6,     1,     1,
       3,     3,     2,     3,     3,     2,     4,     1,     4,     7,
      10,     1,     4,     2,     2,     1,     1,     5,     2,     5,
       0,     1,     3,     4,     0,     1,     0,     0,     1,     1,
       1,     2,     5,     0,     6,     0,     8,     0,     7,     0,
       7,     0,     8,     1,     2,     3,     0,     4,     3,     4,
       4,     4,     4,     5,     5,     5,     5,     6,     1,     1,
       1,     3,     0,     5,     0,     1,     1,     2,     6,     1,
       3,     0,     1,     4,     1,     1,     1,     1,     1,     1,
       1,     3,     2,     1,     2,     2,     2,     3,     4,     5,
       2,     4,     5,     4,     5,     3,     4,     8,     9,     3,
       4,     2,     1,     2,     6,     8,     9,     3,     4,     2,
       3,     4,     5,     4,     5,     4,     5,     3,     4,     1,
       1,     1,     4,     8,     9,     3,     4,     2,     3,     3,
       4,     4,     5,     4,     5,     3,     4,     1,     3,     2,
       1,     2,     2,     2,     3,     4,     5,     2,     4,     5,
       4,     5,     3,     4,     6,     8,     9,     3,     4,     2,
       4,     1,     2,     2,     2,     3,     4,     2,     4,     4,
       3,     6,     8,     3,     2,     4,     1,     2,     2,     1,
       1,     2,     3,     4,     2,     4,     6,     8,     1,     2,
       2,     1,     2,     2,     3,     4,     1,     4,     4,     3,
       5,     8,     3,     2,     3,     7,     1,     5,     5,     6,
       6,     1,     3,     2,     2,     1,     2,     2,     3,     4,
       1,     4,     4,     3,     5,     8,     3,     1,     2,     1,
       2,     6,     5,     6,     7,     7,     1,     2,     2,     1,
       2,     2,     3,     4,     1,     4,     4,     3,     8,     3,
       1,     1,     2,     1,     1,     2,     3,     2,     3,     2,
       3,     3,     2,     4,     3,     2,     3,     2,     4,     3,
       2,     6,     6,     6,     7,     1,     2,     1,     1,     1,
       2,     3,     2,     3,     2,     3,     3,     4,     2,     3,
       4,     2,     5,     6,     7,     6,     6,     0,     1,     0,
       2
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
#line 6819 "Parser/parser.cc"
    break;

  case 3:
#line 533 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 6825 "Parser/parser.cc"
    break;

  case 4:
#line 540 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantInteger( *(yyvsp[0].tok) ) ); }
#line 6831 "Parser/parser.cc"
    break;

  case 5:
#line 541 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 6837 "Parser/parser.cc"
    break;

  case 6:
#line 542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 6843 "Parser/parser.cc"
    break;

  case 7:
#line 543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 6849 "Parser/parser.cc"
    break;

  case 8:
#line 544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantChar( *(yyvsp[0].tok) ) ); }
#line 6855 "Parser/parser.cc"
    break;

  case 19:
#line 565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 6861 "Parser/parser.cc"
    break;

  case 20:
#line 569 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.constant) = build_constantStr( *(yyvsp[0].str) ); }
#line 6867 "Parser/parser.cc"
    break;

  case 21:
#line 573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 6873 "Parser/parser.cc"
    break;

  case 22:
#line 575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 6883 "Parser/parser.cc"
    break;

  case 23:
#line 586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 6889 "Parser/parser.cc"
    break;

  case 24:
#line 588 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 6895 "Parser/parser.cc"
    break;

  case 25:
#line 592 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_dimensionref( (yyvsp[0].tok) ) ); }
#line 6901 "Parser/parser.cc"
    break;

  case 27:
#line 595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 6907 "Parser/parser.cc"
    break;

  case 28:
#line 597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new StmtExpr( dynamic_cast<CompoundStmt *>(maybeMoveBuild<Statement>((yyvsp[-1].sn)) ) ) ); }
#line 6913 "Parser/parser.cc"
    break;

  case 29:
#line 599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.en) = nullptr; }
#line 6919 "Parser/parser.cc"
    break;

  case 30:
#line 601 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.en) = nullptr; }
#line 6925 "Parser/parser.cc"
    break;

  case 31:
#line 603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild<Expression>( (yyvsp[-3].en) );
			(yyval.en) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 6935 "Parser/parser.cc"
    break;

  case 33:
#line 617 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// steal the association node from the singleton and delete the wrapper
			(yyvsp[-2].genexpr)->associations.splice((yyvsp[-2].genexpr)->associations.end(), (yyvsp[0].genexpr)->associations);
			delete (yyvsp[0].genexpr);
			(yyval.genexpr) = (yyvsp[-2].genexpr);
		}
#line 6946 "Parser/parser.cc"
    break;

  case 34:
#line 627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuildType((yyvsp[-2].decl)), maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } );
		}
#line 6955 "Parser/parser.cc"
    break;

  case 35:
#line 632 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } ); }
#line 6961 "Parser/parser.cc"
    break;

  case 37:
#line 642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-5].en), new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) ) ) )) ) ); }
#line 6967 "Parser/parser.cc"
    break;

  case 38:
#line 648 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 6973 "Parser/parser.cc"
    break;

  case 39:
#line 650 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.en) = new ExpressionNode( new ConstructorExpr( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) ) );
		}
#line 6983 "Parser/parser.cc"
    break;

  case 40:
#line 656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 6989 "Parser/parser.cc"
    break;

  case 41:
#line 658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 6995 "Parser/parser.cc"
    break;

  case 42:
#line 660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 7001 "Parser/parser.cc"
    break;

  case 43:
#line 662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), new ExpressionNode( (yyvsp[-2].constant) ) ) ); }
#line 7007 "Parser/parser.cc"
    break;

  case 44:
#line 664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7013 "Parser/parser.cc"
    break;

  case 45:
#line 666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 7019 "Parser/parser.cc"
    break;

  case 46:
#line 668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-1].en), build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) ) ) ); }
#line 7025 "Parser/parser.cc"
    break;

  case 47:
#line 670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7031 "Parser/parser.cc"
    break;

  case 48:
#line 672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[0].aggKey), (yyvsp[-2].en) ) ); }
#line 7037 "Parser/parser.cc"
    break;

  case 49:
#line 674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7043 "Parser/parser.cc"
    break;

  case 50:
#line 676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 7049 "Parser/parser.cc"
    break;

  case 51:
#line 678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7055 "Parser/parser.cc"
    break;

  case 52:
#line 680 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::IncrPost, (yyvsp[-1].en) ) ); }
#line 7061 "Parser/parser.cc"
    break;

  case 53:
#line 682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::DecrPost, (yyvsp[-1].en) ) ); }
#line 7067 "Parser/parser.cc"
    break;

  case 54:
#line 684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].in), true ) ) ); }
#line 7073 "Parser/parser.cc"
    break;

  case 55:
#line 686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].in), true ))->set_maybeConstructed( false ) ) ); }
#line 7079 "Parser/parser.cc"
    break;

  case 56:
#line 688 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) );
		}
#line 7089 "Parser/parser.cc"
    break;

  case 57:
#line 697 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7095 "Parser/parser.cc"
    break;

  case 60:
#line 704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7101 "Parser/parser.cc"
    break;

  case 61:
#line 709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Default parameter for argument is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7107 "Parser/parser.cc"
    break;

  case 64:
#line 716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7113 "Parser/parser.cc"
    break;

  case 66:
#line 722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-1].tok) ) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7119 "Parser/parser.cc"
    break;

  case 67:
#line 724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-3].tok) ) ), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7125 "Parser/parser.cc"
    break;

  case 68:
#line 726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7131 "Parser/parser.cc"
    break;

  case 69:
#line 728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7137 "Parser/parser.cc"
    break;

  case 70:
#line 730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7143 "Parser/parser.cc"
    break;

  case 71:
#line 732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7149 "Parser/parser.cc"
    break;

  case 72:
#line 737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_constantInteger( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 7155 "Parser/parser.cc"
    break;

  case 73:
#line 739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_field_name_FLOATINGconstant( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 7161 "Parser/parser.cc"
    break;

  case 74:
#line 741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_varref( (yyvsp[-1].tok) ), (yyvsp[0].en) ) );
		}
#line 7169 "Parser/parser.cc"
    break;

  case 75:
#line 748 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7175 "Parser/parser.cc"
    break;

  case 76:
#line 750 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Expression * constant = build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) );
			(yyval.en) = (yyvsp[-1].en) != nullptr ? new ExpressionNode( build_fieldSel( (yyvsp[-1].en),  constant ) ) : new ExpressionNode( constant );
		}
#line 7184 "Parser/parser.cc"
    break;

  case 79:
#line 762 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 7190 "Parser/parser.cc"
    break;

  case 80:
#line 764 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en)->set_extension( true ); }
#line 7196 "Parser/parser.cc"
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
#line 7216 "Parser/parser.cc"
    break;

  case 82:
#line 785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_val( (yyvsp[-1].op), (yyvsp[0].en) ) ); }
#line 7222 "Parser/parser.cc"
    break;

  case 83:
#line 787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Incr, (yyvsp[0].en) ) ); }
#line 7228 "Parser/parser.cc"
    break;

  case 84:
#line 789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Decr, (yyvsp[0].en) ) ); }
#line 7234 "Parser/parser.cc"
    break;

  case 85:
#line 791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7240 "Parser/parser.cc"
    break;

  case 86:
#line 793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 7246 "Parser/parser.cc"
    break;

  case 87:
#line 795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7252 "Parser/parser.cc"
    break;

  case 88:
#line 797 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 7258 "Parser/parser.cc"
    break;

  case 89:
#line 799 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_offsetOf( (yyvsp[-3].decl), build_varref( (yyvsp[-1].tok) ) ) ); }
#line 7264 "Parser/parser.cc"
    break;

  case 90:
#line 801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.en) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 7273 "Parser/parser.cc"
    break;

  case 91:
#line 808 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::PointTo; }
#line 7279 "Parser/parser.cc"
    break;

  case 92:
#line 809 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AddressOf; }
#line 7285 "Parser/parser.cc"
    break;

  case 93:
#line 811 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::And; }
#line 7291 "Parser/parser.cc"
    break;

  case 94:
#line 815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::UnPlus; }
#line 7297 "Parser/parser.cc"
    break;

  case 95:
#line 816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::UnMinus; }
#line 7303 "Parser/parser.cc"
    break;

  case 96:
#line 817 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::Neg; }
#line 7309 "Parser/parser.cc"
    break;

  case 97:
#line 818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::BitNeg; }
#line 7315 "Parser/parser.cc"
    break;

  case 99:
#line 824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cast( (yyvsp[-2].decl), (yyvsp[0].en) ) ); }
#line 7321 "Parser/parser.cc"
    break;

  case 100:
#line 826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 7327 "Parser/parser.cc"
    break;

  case 101:
#line 828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 7333 "Parser/parser.cc"
    break;

  case 102:
#line 830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( nullptr ) ) ); }
#line 7339 "Parser/parser.cc"
    break;

  case 103:
#line 832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 7345 "Parser/parser.cc"
    break;

  case 104:
#line 834 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Return cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7351 "Parser/parser.cc"
    break;

  case 105:
#line 836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7357 "Parser/parser.cc"
    break;

  case 106:
#line 838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7363 "Parser/parser.cc"
    break;

  case 114:
#line 858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Exp, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7369 "Parser/parser.cc"
    break;

  case 116:
#line 864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mul, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7375 "Parser/parser.cc"
    break;

  case 117:
#line 866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Div, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7381 "Parser/parser.cc"
    break;

  case 118:
#line 868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mod, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7387 "Parser/parser.cc"
    break;

  case 120:
#line 874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Plus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7393 "Parser/parser.cc"
    break;

  case 121:
#line 876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Minus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7399 "Parser/parser.cc"
    break;

  case 123:
#line 882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7405 "Parser/parser.cc"
    break;

  case 124:
#line 884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::RShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7411 "Parser/parser.cc"
    break;

  case 126:
#line 890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7417 "Parser/parser.cc"
    break;

  case 127:
#line 892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7423 "Parser/parser.cc"
    break;

  case 128:
#line 894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7429 "Parser/parser.cc"
    break;

  case 129:
#line 896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7435 "Parser/parser.cc"
    break;

  case 131:
#line 902 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Eq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7441 "Parser/parser.cc"
    break;

  case 132:
#line 904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Neq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7447 "Parser/parser.cc"
    break;

  case 134:
#line 910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitAnd, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7453 "Parser/parser.cc"
    break;

  case 136:
#line 916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Xor, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7459 "Parser/parser.cc"
    break;

  case 138:
#line 922 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitOr, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7465 "Parser/parser.cc"
    break;

  case 140:
#line 928 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), true ) ); }
#line 7471 "Parser/parser.cc"
    break;

  case 142:
#line 934 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), false ) ); }
#line 7477 "Parser/parser.cc"
    break;

  case 144:
#line 940 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7483 "Parser/parser.cc"
    break;

  case 145:
#line 943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-3].en), (yyvsp[-3].en), (yyvsp[0].en) ) ); }
#line 7489 "Parser/parser.cc"
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
#line 7501 "Parser/parser.cc"
    break;

  case 149:
#line 962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7507 "Parser/parser.cc"
    break;

  case 150:
#line 967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7513 "Parser/parser.cc"
    break;

  case 154:
#line 977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::Assign; }
#line 7519 "Parser/parser.cc"
    break;

  case 155:
#line 978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AtAssn; }
#line 7525 "Parser/parser.cc"
    break;

  case 156:
#line 982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ExpAssn; }
#line 7531 "Parser/parser.cc"
    break;

  case 157:
#line 983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MulAssn; }
#line 7537 "Parser/parser.cc"
    break;

  case 158:
#line 984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::DivAssn; }
#line 7543 "Parser/parser.cc"
    break;

  case 159:
#line 985 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ModAssn; }
#line 7549 "Parser/parser.cc"
    break;

  case 160:
#line 986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::PlusAssn; }
#line 7555 "Parser/parser.cc"
    break;

  case 161:
#line 987 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MinusAssn; }
#line 7561 "Parser/parser.cc"
    break;

  case 162:
#line 988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::LSAssn; }
#line 7567 "Parser/parser.cc"
    break;

  case 163:
#line 989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::RSAssn; }
#line 7573 "Parser/parser.cc"
    break;

  case 164:
#line 990 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AndAssn; }
#line 7579 "Parser/parser.cc"
    break;

  case 165:
#line 991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ERAssn; }
#line 7585 "Parser/parser.cc"
    break;

  case 166:
#line 992 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::OrAssn; }
#line 7591 "Parser/parser.cc"
    break;

  case 167:
#line 1003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)(new ExpressionNode( nullptr ) )->set_last( (yyvsp[-1].en) ) ) ); }
#line 7597 "Parser/parser.cc"
    break;

  case 168:
#line 1005 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-4].en)->set_last( (yyvsp[-1].en) ) ) )); }
#line 7603 "Parser/parser.cc"
    break;

  case 170:
#line 1011 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7609 "Parser/parser.cc"
    break;

  case 171:
#line 1013 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7615 "Parser/parser.cc"
    break;

  case 172:
#line 1015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7621 "Parser/parser.cc"
    break;

  case 174:
#line 1021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new CommaExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7627 "Parser/parser.cc"
    break;

  case 175:
#line 1026 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7633 "Parser/parser.cc"
    break;

  case 187:
#line 1044 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 7639 "Parser/parser.cc"
    break;

  case 189:
#line 1047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_directive( (yyvsp[0].tok) ) ); }
#line 7645 "Parser/parser.cc"
    break;

  case 190:
#line 1053 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[0].sn)->add_label( (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 7651 "Parser/parser.cc"
    break;

  case 191:
#line 1058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (StatementNode *)0 ) ); }
#line 7657 "Parser/parser.cc"
    break;

  case 192:
#line 1063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (yyvsp[-2].sn) ) ); }
#line 7663 "Parser/parser.cc"
    break;

  case 194:
#line 1069 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 7669 "Parser/parser.cc"
    break;

  case 195:
#line 1074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7675 "Parser/parser.cc"
    break;

  case 196:
#line 1076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7681 "Parser/parser.cc"
    break;

  case 197:
#line 1078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7687 "Parser/parser.cc"
    break;

  case 198:
#line 1080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7693 "Parser/parser.cc"
    break;

  case 201:
#line 1087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 7699 "Parser/parser.cc"
    break;

  case 202:
#line 1092 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_expr( (yyvsp[-1].en) ) ); }
#line 7705 "Parser/parser.cc"
    break;

  case 203:
#line 1094 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_mutex( nullptr, new StatementNode( build_expr( (yyvsp[-1].en) ) ) ) ); }
#line 7711 "Parser/parser.cc"
    break;

  case 204:
#line 1102 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn); }
#line 7717 "Parser/parser.cc"
    break;

  case 205:
#line 1104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( true, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 7723 "Parser/parser.cc"
    break;

  case 206:
#line 1106 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( true, (yyvsp[-7].en), (yyvsp[-2].sn) ) );
			// The semantics of the declaration list is changed to include associated initialization, which is performed
			// *before* the transfer to the appropriate case clause by hoisting the declarations into a compound
			// statement around the switch.  Statements after the initial declaration list can never be executed, and
			// therefore, are removed from the grammar even though C allows it. The change also applies to choose
			// statement.
			(yyval.sn) = (yyvsp[-3].decl) ? new StatementNode( build_compound( (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 7737 "Parser/parser.cc"
    break;

  case 207:
#line 1116 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( false, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 7743 "Parser/parser.cc"
    break;

  case 208:
#line 1118 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( false, (yyvsp[-7].en), (yyvsp[-2].sn) ) );
			(yyval.sn) = (yyvsp[-3].decl) ? new StatementNode( build_compound( (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 7752 "Parser/parser.cc"
    break;

  case 209:
#line 1127 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-2].ifctl), maybe_build_compound( (yyvsp[0].sn) ), nullptr ) ); }
#line 7758 "Parser/parser.cc"
    break;

  case 210:
#line 1129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-4].ifctl), maybe_build_compound( (yyvsp[-2].sn) ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 7764 "Parser/parser.cc"
    break;

  case 211:
#line 1134 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( nullptr, (yyvsp[0].en) ); }
#line 7770 "Parser/parser.cc"
    break;

  case 212:
#line 1136 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 7776 "Parser/parser.cc"
    break;

  case 213:
#line 1138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 7782 "Parser/parser.cc"
    break;

  case 214:
#line 1140 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[-1].decl), (yyvsp[0].en) ); }
#line 7788 "Parser/parser.cc"
    break;

  case 215:
#line 1147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = (yyvsp[0].en); }
#line 7794 "Parser/parser.cc"
    break;

  case 216:
#line 1149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7800 "Parser/parser.cc"
    break;

  case 218:
#line 1154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.sn) = new StatementNode( build_case( (yyvsp[0].en) ) ); }
#line 7806 "Parser/parser.cc"
    break;

  case 219:
#line 1156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)((yyvsp[-2].sn)->set_last( new StatementNode( build_case( (yyvsp[0].en) ) ) ) ); }
#line 7812 "Parser/parser.cc"
    break;

  case 220:
#line 1160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn); }
#line 7818 "Parser/parser.cc"
    break;

  case 221:
#line 1161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.sn) = new StatementNode( build_default() ); }
#line 7824 "Parser/parser.cc"
    break;

  case 223:
#line 1173 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)( (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) )); }
#line 7830 "Parser/parser.cc"
    break;

  case 224:
#line 1177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( maybe_build_compound( (yyvsp[0].sn) ) ); }
#line 7836 "Parser/parser.cc"
    break;

  case 225:
#line 1182 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = nullptr; }
#line 7842 "Parser/parser.cc"
    break;

  case 227:
#line 1188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ); }
#line 7848 "Parser/parser.cc"
    break;

  case 228:
#line 1190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)( (yyvsp[-2].sn)->set_last( (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ) ) ); }
#line 7854 "Parser/parser.cc"
    break;

  case 229:
#line 1195 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( new CondCtl( nullptr, new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 7860 "Parser/parser.cc"
    break;

  case 230:
#line 1197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( (yyvsp[-2].ifctl), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 7866 "Parser/parser.cc"
    break;

  case 231:
#line 1199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( (yyvsp[-4].ifctl), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[0].sn) ) ); }
#line 7872 "Parser/parser.cc"
    break;

  case 232:
#line 1201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( new ExpressionNode( build_constantInteger( *new string( "1" ) ) ), maybe_build_compound( (yyvsp[-4].sn) ) ) ); }
#line 7878 "Parser/parser.cc"
    break;

  case 233:
#line 1203 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( (yyvsp[-2].en), maybe_build_compound( (yyvsp[-5].sn) ) ) ); }
#line 7884 "Parser/parser.cc"
    break;

  case 234:
#line 1205 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( (yyvsp[-3].en), maybe_build_compound( (yyvsp[-6].sn) ), (yyvsp[0].sn) ) ); }
#line 7890 "Parser/parser.cc"
    break;

  case 235:
#line 1207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( new ForCtrl( (ExpressionNode * )nullptr, (ExpressionNode * )nullptr, (ExpressionNode * )nullptr ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 7896 "Parser/parser.cc"
    break;

  case 236:
#line 1209 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( (yyvsp[-2].fctl), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 7902 "Parser/parser.cc"
    break;

  case 237:
#line 1211 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( (yyvsp[-4].fctl), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[0].sn) ) ); }
#line 7908 "Parser/parser.cc"
    break;

  case 239:
#line 1221 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 7927 "Parser/parser.cc"
    break;

  case 240:
#line 1239 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (ExpressionNode * )nullptr, (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 7933 "Parser/parser.cc"
    break;

  case 241:
#line 1241 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 7939 "Parser/parser.cc"
    break;

  case 242:
#line 1243 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-3].decl), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 7945 "Parser/parser.cc"
    break;

  case 243:
#line 1246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7952 "Parser/parser.cc"
    break;

  case 244:
#line 1249 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LEThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7959 "Parser/parser.cc"
    break;

  case 245:
#line 1252 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].en)->clone(), (yyvsp[-1].compop), (yyvsp[0].en), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7965 "Parser/parser.cc"
    break;

  case 246:
#line 1254 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 7971 "Parser/parser.cc"
    break;

  case 247:
#line 1256 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( new ExpressionNode( build_constantInteger( *new string( "0u" ) ) ), (yyvsp[-1].en), nullptr, OperKinds::LThan, nullptr, nullptr ); }
#line 7977 "Parser/parser.cc"
    break;

  case 248:
#line 1258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-2].en), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7984 "Parser/parser.cc"
    break;

  case 249:
#line 1261 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-3].en), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LEThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7991 "Parser/parser.cc"
    break;

  case 250:
#line 1264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), (yyvsp[-1].compop), (yyvsp[0].en), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7997 "Parser/parser.cc"
    break;

  case 251:
#line 1266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8003 "Parser/parser.cc"
    break;

  case 252:
#line 1269 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Array interator is currently unimplemented." ); (yyval.fctl) = nullptr;
			(yyval.fctl) = forCtrl( new ExpressionNode( build_varref( (yyvsp[0].tok) ) ), (yyvsp[-2].en), nullptr, OperKinds::Range, nullptr, nullptr );
		}
#line 8012 "Parser/parser.cc"
    break;

  case 253:
#line 1276 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), OperKinds::LThan, nullptr, new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8018 "Parser/parser.cc"
    break;

  case 254:
#line 1278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), OperKinds::GThan, nullptr, new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8024 "Parser/parser.cc"
    break;

  case 255:
#line 1280 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), OperKinds::LThan, nullptr, (yyvsp[0].en) ); }
#line 8030 "Parser/parser.cc"
    break;

  case 256:
#line 1282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), OperKinds::GThan, nullptr, (yyvsp[0].en) ); }
#line 8036 "Parser/parser.cc"
    break;

  case 257:
#line 1284 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), OperKinds::LThan, nullptr, nullptr ); }
#line 8042 "Parser/parser.cc"
    break;

  case 258:
#line 1289 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LThan; }
#line 8048 "Parser/parser.cc"
    break;

  case 259:
#line 1291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 8054 "Parser/parser.cc"
    break;

  case 260:
#line 1293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GThan; }
#line 8060 "Parser/parser.cc"
    break;

  case 261:
#line 1295 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GEThan; }
#line 8066 "Parser/parser.cc"
    break;

  case 262:
#line 1300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Goto ) ); }
#line 8072 "Parser/parser.cc"
    break;

  case 263:
#line 1304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_computedgoto( (yyvsp[-1].en) ) ); }
#line 8078 "Parser/parser.cc"
    break;

  case 264:
#line 1307 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThrough ) ); }
#line 8084 "Parser/parser.cc"
    break;

  case 265:
#line 1309 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::FallThrough ) ); }
#line 8090 "Parser/parser.cc"
    break;

  case 266:
#line 1311 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThroughDefault ) ); }
#line 8096 "Parser/parser.cc"
    break;

  case 267:
#line 1314 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Continue ) ); }
#line 8102 "Parser/parser.cc"
    break;

  case 268:
#line 1318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Continue ) ); }
#line 8108 "Parser/parser.cc"
    break;

  case 269:
#line 1321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Break ) ); }
#line 8114 "Parser/parser.cc"
    break;

  case 270:
#line 1325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Break ) ); }
#line 8120 "Parser/parser.cc"
    break;

  case 271:
#line 1327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_return( (yyvsp[-1].en) ) ); }
#line 8126 "Parser/parser.cc"
    break;

  case 272:
#line 1329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 8132 "Parser/parser.cc"
    break;

  case 273:
#line 1331 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr ) ); }
#line 8138 "Parser/parser.cc"
    break;

  case 274:
#line 1333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn) ) ); }
#line 8144 "Parser/parser.cc"
    break;

  case 275:
#line 1335 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Coroutine ) ); }
#line 8150 "Parser/parser.cc"
    break;

  case 276:
#line 1337 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Coroutine ) ); }
#line 8156 "Parser/parser.cc"
    break;

  case 277:
#line 1339 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Generator ) ); }
#line 8162 "Parser/parser.cc"
    break;

  case 278:
#line 1341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Generator ) ); }
#line 8168 "Parser/parser.cc"
    break;

  case 279:
#line 1343 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_throw( (yyvsp[-1].en) ) ); }
#line 8174 "Parser/parser.cc"
    break;

  case 280:
#line 1345 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume( (yyvsp[-1].en) ) ); }
#line 8180 "Parser/parser.cc"
    break;

  case 281:
#line 1347 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume_at( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 8186 "Parser/parser.cc"
    break;

  case 284:
#line 1357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_with( (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8192 "Parser/parser.cc"
    break;

  case 285:
#line 1363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_mutex( (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8198 "Parser/parser.cc"
    break;

  case 286:
#line 1367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 8204 "Parser/parser.cc"
    break;

  case 287:
#line 1372 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8210 "Parser/parser.cc"
    break;

  case 289:
#line 1378 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 8216 "Parser/parser.cc"
    break;

  case 290:
#line 1382 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) )); }
#line 8222 "Parser/parser.cc"
    break;

  case 292:
#line 1389 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8228 "Parser/parser.cc"
    break;

  case 293:
#line 1393 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 8234 "Parser/parser.cc"
    break;

  case 294:
#line 1398 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8240 "Parser/parser.cc"
    break;

  case 295:
#line 1400 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-3].en), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[-4].en), (yyvsp[0].wfs) ); }
#line 8246 "Parser/parser.cc"
    break;

  case 296:
#line 1402 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8252 "Parser/parser.cc"
    break;

  case 297:
#line 1404 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( nullptr, maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8258 "Parser/parser.cc"
    break;

  case 298:
#line 1407 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 8264 "Parser/parser.cc"
    break;

  case 299:
#line 1409 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-5].en), maybe_build_compound( (yyvsp[-4].sn) ), (yyvsp[-6].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8270 "Parser/parser.cc"
    break;

  case 300:
#line 1414 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-1].en), (yyvsp[0].sn), (yyvsp[-2].en) ) ); }
#line 8276 "Parser/parser.cc"
    break;

  case 301:
#line 1416 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-3].en), (yyvsp[-2].sn), (yyvsp[-4].en), (yyvsp[0].wfs) ) ); }
#line 8282 "Parser/parser.cc"
    break;

  case 302:
#line 1421 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), (yyvsp[0].sn), 0 ) ); }
#line 8288 "Parser/parser.cc"
    break;

  case 303:
#line 1423 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), 0, (yyvsp[0].sn) ) ); }
#line 8294 "Parser/parser.cc"
    break;

  case 304:
#line 1425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-2].sn), (yyvsp[-1].sn), (yyvsp[0].sn) ) ); }
#line 8300 "Parser/parser.cc"
    break;

  case 305:
#line 1430 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8306 "Parser/parser.cc"
    break;

  case 306:
#line 1432 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)(yyvsp[-8].sn)->set_last( new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ) ); }
#line 8312 "Parser/parser.cc"
    break;

  case 307:
#line 1437 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8318 "Parser/parser.cc"
    break;

  case 308:
#line 1438 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[0].en); }
#line 8324 "Parser/parser.cc"
    break;

  case 309:
#line 1442 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 8330 "Parser/parser.cc"
    break;

  case 310:
#line 1443 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 8336 "Parser/parser.cc"
    break;

  case 311:
#line 1444 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.catch_kind) = CatchStmt::Resume; }
#line 8342 "Parser/parser.cc"
    break;

  case 312:
#line 1445 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Resume; }
#line 8348 "Parser/parser.cc"
    break;

  case 313:
#line 1449 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = new StatementNode( build_finally( (yyvsp[0].sn) ) ); }
#line 8354 "Parser/parser.cc"
    break;

  case 315:
#line 1456 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 8360 "Parser/parser.cc"
    break;

  case 316:
#line 1458 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 8366 "Parser/parser.cc"
    break;

  case 317:
#line 1460 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 8372 "Parser/parser.cc"
    break;

  case 322:
#line 1475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-4].flag), (yyvsp[-2].constant), 0 ) ); }
#line 8378 "Parser/parser.cc"
    break;

  case 323:
#line 1477 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-6].flag), (yyvsp[-4].constant), (yyvsp[-2].en) ) ); }
#line 8384 "Parser/parser.cc"
    break;

  case 324:
#line 1479 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-8].flag), (yyvsp[-6].constant), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 8390 "Parser/parser.cc"
    break;

  case 325:
#line 1481 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-10].flag), (yyvsp[-8].constant), (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 8396 "Parser/parser.cc"
    break;

  case 326:
#line 1483 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-12].flag), (yyvsp[-9].constant), 0, (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].label) ) ); }
#line 8402 "Parser/parser.cc"
    break;

  case 327:
#line 1488 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = false; }
#line 8408 "Parser/parser.cc"
    break;

  case 328:
#line 1490 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = true; }
#line 8414 "Parser/parser.cc"
    break;

  case 329:
#line 1495 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8420 "Parser/parser.cc"
    break;

  case 332:
#line 1502 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 8426 "Parser/parser.cc"
    break;

  case 333:
#line 1507 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( nullptr, (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 8432 "Parser/parser.cc"
    break;

  case 334:
#line 1509 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( (yyvsp[-5].tok), (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 8438 "Parser/parser.cc"
    break;

  case 335:
#line 1514 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8444 "Parser/parser.cc"
    break;

  case 336:
#line 1516 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 8450 "Parser/parser.cc"
    break;

  case 337:
#line 1518 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( (yyvsp[0].constant) ) )); }
#line 8456 "Parser/parser.cc"
    break;

  case 338:
#line 1523 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = new LabelNode(); (yyval.label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 8465 "Parser/parser.cc"
    break;

  case 339:
#line 1528 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = (yyvsp[-2].label); (yyvsp[-2].label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 8474 "Parser/parser.cc"
    break;

  case 340:
#line 1538 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8480 "Parser/parser.cc"
    break;

  case 343:
#line 1545 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 8486 "Parser/parser.cc"
    break;

  case 344:
#line 1550 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8492 "Parser/parser.cc"
    break;

  case 346:
#line 1556 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 8498 "Parser/parser.cc"
    break;

  case 347:
#line 1558 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-2].decl) ); }
#line 8504 "Parser/parser.cc"
    break;

  case 357:
#line 1584 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-4].en), (yyvsp[-2].constant) ); }
#line 8510 "Parser/parser.cc"
    break;

  case 358:
#line 1586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-2].en), build_constantStr( *new string( "\"\"" ) ) ); }
#line 8516 "Parser/parser.cc"
    break;

  case 362:
#line 1604 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 8522 "Parser/parser.cc"
    break;

  case 364:
#line 1610 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].in) ); }
#line 8528 "Parser/parser.cc"
    break;

  case 365:
#line 1614 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 8534 "Parser/parser.cc"
    break;

  case 366:
#line 1616 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].in) ) ); }
#line 8540 "Parser/parser.cc"
    break;

  case 367:
#line 1623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 8546 "Parser/parser.cc"
    break;

  case 368:
#line 1625 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 8552 "Parser/parser.cc"
    break;

  case 369:
#line 1627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 8558 "Parser/parser.cc"
    break;

  case 371:
#line 1633 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 8564 "Parser/parser.cc"
    break;

  case 372:
#line 1635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 8570 "Parser/parser.cc"
    break;

  case 373:
#line 1637 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 8576 "Parser/parser.cc"
    break;

  case 374:
#line 1639 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeClone( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->appendList( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 8587 "Parser/parser.cc"
    break;

  case 375:
#line 1672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), 0 )->addQualifiers( (yyvsp[0].decl) ); }
#line 8593 "Parser/parser.cc"
    break;

  case 376:
#line 1674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), 0 )->addQualifiers( (yyvsp[0].decl) ); }
#line 8599 "Parser/parser.cc"
    break;

  case 377:
#line 1679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 8605 "Parser/parser.cc"
    break;

  case 378:
#line 1682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-6].decl)->appendList( (yyvsp[-2].decl) ) ); }
#line 8611 "Parser/parser.cc"
    break;

  case 379:
#line 1687 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 8620 "Parser/parser.cc"
    break;

  case 380:
#line 1692 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 8629 "Parser/parser.cc"
    break;

  case 381:
#line 1697 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "3" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 8638 "Parser/parser.cc"
    break;

  case 382:
#line 1708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "4" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef();
		}
#line 8647 "Parser/parser.cc"
    break;

  case 383:
#line 1713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "5" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 8656 "Parser/parser.cc"
    break;

  case 384:
#line 1718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "6" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[-3].decl) )->addTypedef();
		}
#line 8665 "Parser/parser.cc"
    break;

  case 385:
#line 1723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "7" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-2].decl) )->addTypedef();
		}
#line 8674 "Parser/parser.cc"
    break;

  case 386:
#line 1728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "8" );
			(yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-3].decl) )->addTypedef()->addType( (yyvsp[-3].decl) );
		}
#line 8683 "Parser/parser.cc"
    break;

  case 387:
#line 1737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 8691 "Parser/parser.cc"
    break;

  case 388:
#line 1741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 8699 "Parser/parser.cc"
    break;

  case 389:
#line 1748 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 8705 "Parser/parser.cc"
    break;

  case 393:
#line 1758 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 8711 "Parser/parser.cc"
    break;

  case 394:
#line 1760 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ) ); }
#line 8717 "Parser/parser.cc"
    break;

  case 407:
#line 1799 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8723 "Parser/parser.cc"
    break;

  case 410:
#line 1811 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 8729 "Parser/parser.cc"
    break;

  case 413:
#line 1821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Const ); }
#line 8735 "Parser/parser.cc"
    break;

  case 414:
#line 1823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Restrict ); }
#line 8741 "Parser/parser.cc"
    break;

  case 415:
#line 1825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Volatile ); }
#line 8747 "Parser/parser.cc"
    break;

  case 416:
#line 1827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Atomic ); }
#line 8753 "Parser/parser.cc"
    break;

  case 418:
#line 1833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newForall( (yyvsp[-1].decl) ); }
#line 8759 "Parser/parser.cc"
    break;

  case 420:
#line 1839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 8765 "Parser/parser.cc"
    break;

  case 421:
#line 1841 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 8771 "Parser/parser.cc"
    break;

  case 423:
#line 1852 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 8777 "Parser/parser.cc"
    break;

  case 424:
#line 1857 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Extern ); }
#line 8783 "Parser/parser.cc"
    break;

  case 425:
#line 1859 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Static ); }
#line 8789 "Parser/parser.cc"
    break;

  case 426:
#line 1861 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Auto ); }
#line 8795 "Parser/parser.cc"
    break;

  case 427:
#line 1863 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Register ); }
#line 8801 "Parser/parser.cc"
    break;

  case 428:
#line 1865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Threadlocal ); }
#line 8807 "Parser/parser.cc"
    break;

  case 429:
#line 1868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Inline ); }
#line 8813 "Parser/parser.cc"
    break;

  case 430:
#line 1870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Fortran ); }
#line 8819 "Parser/parser.cc"
    break;

  case 431:
#line 1872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Noreturn ); }
#line 8825 "Parser/parser.cc"
    break;

  case 432:
#line 1877 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 8831 "Parser/parser.cc"
    break;

  case 433:
#line 1879 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Bool ); }
#line 8837 "Parser/parser.cc"
    break;

  case 434:
#line 1881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Char ); }
#line 8843 "Parser/parser.cc"
    break;

  case 435:
#line 1883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int ); }
#line 8849 "Parser/parser.cc"
    break;

  case 436:
#line 1885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 ); }
#line 8855 "Parser/parser.cc"
    break;

  case 437:
#line 1887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 )->addType( DeclarationNode::newSignedNess( DeclarationNode::Unsigned ) ); }
#line 8861 "Parser/parser.cc"
    break;

  case 438:
#line 1889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Float ); }
#line 8867 "Parser/parser.cc"
    break;

  case 439:
#line 1891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Double ); }
#line 8873 "Parser/parser.cc"
    break;

  case 440:
#line 1893 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat80 ); }
#line 8879 "Parser/parser.cc"
    break;

  case 441:
#line 1895 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat128 ); }
#line 8885 "Parser/parser.cc"
    break;

  case 442:
#line 1897 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat16 ); }
#line 8891 "Parser/parser.cc"
    break;

  case 443:
#line 1899 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32 ); }
#line 8897 "Parser/parser.cc"
    break;

  case 444:
#line 1901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32x ); }
#line 8903 "Parser/parser.cc"
    break;

  case 445:
#line 1903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64 ); }
#line 8909 "Parser/parser.cc"
    break;

  case 446:
#line 1905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64x ); }
#line 8915 "Parser/parser.cc"
    break;

  case 447:
#line 1907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat128 ); }
#line 8921 "Parser/parser.cc"
    break;

  case 448:
#line 1909 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 8927 "Parser/parser.cc"
    break;

  case 449:
#line 1911 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 8933 "Parser/parser.cc"
    break;

  case 450:
#line 1913 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 8939 "Parser/parser.cc"
    break;

  case 451:
#line 1915 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Complex ); }
#line 8945 "Parser/parser.cc"
    break;

  case 452:
#line 1917 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Imaginary ); }
#line 8951 "Parser/parser.cc"
    break;

  case 453:
#line 1919 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Signed ); }
#line 8957 "Parser/parser.cc"
    break;

  case 454:
#line 1921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Unsigned ); }
#line 8963 "Parser/parser.cc"
    break;

  case 455:
#line 1923 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Short ); }
#line 8969 "Parser/parser.cc"
    break;

  case 456:
#line 1925 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Long ); }
#line 8975 "Parser/parser.cc"
    break;

  case 457:
#line 1927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Valist ); }
#line 8981 "Parser/parser.cc"
    break;

  case 458:
#line 1929 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::AutoType ); }
#line 8987 "Parser/parser.cc"
    break;

  case 460:
#line 1935 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8993 "Parser/parser.cc"
    break;

  case 462:
#line 1941 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVtableType( (yyvsp[-2].decl) ); }
#line 8999 "Parser/parser.cc"
    break;

  case 463:
#line 1947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9005 "Parser/parser.cc"
    break;

  case 464:
#line 1949 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9011 "Parser/parser.cc"
    break;

  case 466:
#line 1956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9017 "Parser/parser.cc"
    break;

  case 467:
#line 1958 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9023 "Parser/parser.cc"
    break;

  case 468:
#line 1960 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9029 "Parser/parser.cc"
    break;

  case 469:
#line 1962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 9035 "Parser/parser.cc"
    break;

  case 471:
#line 1969 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9041 "Parser/parser.cc"
    break;

  case 473:
#line 1975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9047 "Parser/parser.cc"
    break;

  case 474:
#line 1977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9053 "Parser/parser.cc"
    break;

  case 475:
#line 1979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 9059 "Parser/parser.cc"
    break;

  case 476:
#line 1984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 9065 "Parser/parser.cc"
    break;

  case 477:
#line 1986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en) ); }
#line 9071 "Parser/parser.cc"
    break;

  case 478:
#line 1988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 9077 "Parser/parser.cc"
    break;

  case 479:
#line 1990 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en), true ); }
#line 9083 "Parser/parser.cc"
    break;

  case 480:
#line 1992 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Zero ); }
#line 9089 "Parser/parser.cc"
    break;

  case 481:
#line 1994 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::One ); }
#line 9095 "Parser/parser.cc"
    break;

  case 483:
#line 2000 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9101 "Parser/parser.cc"
    break;

  case 484:
#line 2002 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9107 "Parser/parser.cc"
    break;

  case 485:
#line 2004 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9113 "Parser/parser.cc"
    break;

  case 487:
#line 2010 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 9119 "Parser/parser.cc"
    break;

  case 488:
#line 2012 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 9125 "Parser/parser.cc"
    break;

  case 489:
#line 2014 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 9134 "Parser/parser.cc"
    break;

  case 491:
#line 2023 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9140 "Parser/parser.cc"
    break;

  case 492:
#line 2025 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9146 "Parser/parser.cc"
    break;

  case 493:
#line 2027 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9152 "Parser/parser.cc"
    break;

  case 495:
#line 2033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9158 "Parser/parser.cc"
    break;

  case 496:
#line 2035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9164 "Parser/parser.cc"
    break;

  case 498:
#line 2041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9170 "Parser/parser.cc"
    break;

  case 499:
#line 2043 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9176 "Parser/parser.cc"
    break;

  case 500:
#line 2045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9182 "Parser/parser.cc"
    break;

  case 502:
#line 2051 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9188 "Parser/parser.cc"
    break;

  case 503:
#line 2053 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9194 "Parser/parser.cc"
    break;

  case 504:
#line 2058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypedef( (yyvsp[0].tok) ); }
#line 9200 "Parser/parser.cc"
    break;

  case 505:
#line 2060 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 9206 "Parser/parser.cc"
    break;

  case 506:
#line 2062 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 9212 "Parser/parser.cc"
    break;

  case 508:
#line 2065 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), (yyvsp[0].decl) ); }
#line 9218 "Parser/parser.cc"
    break;

  case 509:
#line 2067 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), (yyvsp[0].decl) ); }
#line 9224 "Parser/parser.cc"
    break;

  case 510:
#line 2072 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[0].tok), nullptr ); }
#line 9230 "Parser/parser.cc"
    break;

  case 511:
#line 2074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-2].tok), nullptr ); }
#line 9236 "Parser/parser.cc"
    break;

  case 512:
#line 2076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 9242 "Parser/parser.cc"
    break;

  case 517:
#line 2091 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 9248 "Parser/parser.cc"
    break;

  case 518:
#line 2093 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 9254 "Parser/parser.cc"
    break;

  case 519:
#line 2095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 9263 "Parser/parser.cc"
    break;

  case 520:
#line 2100 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) ); }
#line 9269 "Parser/parser.cc"
    break;

  case 521:
#line 2102 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 9278 "Parser/parser.cc"
    break;

  case 522:
#line 2107 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypedef( (yyvsp[-5].tok) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 9287 "Parser/parser.cc"
    break;

  case 523:
#line 2112 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 9296 "Parser/parser.cc"
    break;

  case 524:
#line 2117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeGen( (yyvsp[-5].tok), nullptr );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 9305 "Parser/parser.cc"
    break;

  case 526:
#line 2126 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9311 "Parser/parser.cc"
    break;

  case 527:
#line 2128 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 9317 "Parser/parser.cc"
    break;

  case 528:
#line 2133 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 9327 "Parser/parser.cc"
    break;

  case 529:
#line 2139 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 9342 "Parser/parser.cc"
    break;

  case 532:
#line 2158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Struct; }
#line 9348 "Parser/parser.cc"
    break;

  case 533:
#line 2160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Union; }
#line 9354 "Parser/parser.cc"
    break;

  case 534:
#line 2162 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Exception; }
#line 9360 "Parser/parser.cc"
    break;

  case 535:
#line 2168 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 9366 "Parser/parser.cc"
    break;

  case 536:
#line 2170 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 9372 "Parser/parser.cc"
    break;

  case 537:
#line 2172 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Generator; }
#line 9378 "Parser/parser.cc"
    break;

  case 538:
#line 2174 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor generator is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 9384 "Parser/parser.cc"
    break;

  case 539:
#line 2176 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Coroutine; }
#line 9390 "Parser/parser.cc"
    break;

  case 540:
#line 2178 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor coroutine is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 9396 "Parser/parser.cc"
    break;

  case 541:
#line 2180 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Thread; }
#line 9402 "Parser/parser.cc"
    break;

  case 542:
#line 2182 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor thread is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 9408 "Parser/parser.cc"
    break;

  case 543:
#line 2187 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9414 "Parser/parser.cc"
    break;

  case 544:
#line 2189 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 9420 "Parser/parser.cc"
    break;

  case 545:
#line 2194 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); }
#line 9426 "Parser/parser.cc"
    break;

  case 546:
#line 2196 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 9432 "Parser/parser.cc"
    break;

  case 547:
#line 2198 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-1].decl) ) {								// field declarator ?
				(yyvsp[-1].decl) = DeclarationNode::newName( nullptr );
			} // if
			(yyvsp[-1].decl)->inLine = true;
			(yyval.decl) = distAttr( (yyvsp[-2].decl), (yyvsp[-1].decl) );					// mark all fields in list
			distInl( (yyvsp[-1].decl) );
		}
#line 9445 "Parser/parser.cc"
    break;

  case 548:
#line 2207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9451 "Parser/parser.cc"
    break;

  case 551:
#line 2211 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 9457 "Parser/parser.cc"
    break;

  case 552:
#line 2213 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 9463 "Parser/parser.cc"
    break;

  case 555:
#line 2220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9469 "Parser/parser.cc"
    break;

  case 557:
#line 2223 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 9475 "Parser/parser.cc"
    break;

  case 558:
#line 2228 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].en) ); }
#line 9481 "Parser/parser.cc"
    break;

  case 559:
#line 2231 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 9487 "Parser/parser.cc"
    break;

  case 560:
#line 2234 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 9493 "Parser/parser.cc"
    break;

  case 561:
#line 2239 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9499 "Parser/parser.cc"
    break;

  case 563:
#line 2242 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 9505 "Parser/parser.cc"
    break;

  case 565:
#line 2253 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 9511 "Parser/parser.cc"
    break;

  case 566:
#line 2255 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 9517 "Parser/parser.cc"
    break;

  case 568:
#line 2262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 9523 "Parser/parser.cc"
    break;

  case 569:
#line 2267 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9529 "Parser/parser.cc"
    break;

  case 571:
#line 2273 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 9535 "Parser/parser.cc"
    break;

  case 572:
#line 2278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-4].decl) ); }
#line 9541 "Parser/parser.cc"
    break;

  case 573:
#line 2280 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); }
#line 9547 "Parser/parser.cc"
    break;

  case 574:
#line 2282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].tok), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) ); }
#line 9553 "Parser/parser.cc"
    break;

  case 575:
#line 2285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-4].decl)->name, (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 9559 "Parser/parser.cc"
    break;

  case 576:
#line 2287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-6].decl)->storageClasses.val != 0 || (yyvsp[-6].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			SemanticError( yylloc, "Typed enumeration is currently unimplemented." ); (yyval.decl) = nullptr;
		}
#line 9568 "Parser/parser.cc"
    break;

  case 577:
#line 2292 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].decl)->storageClasses.val != 0 || (yyvsp[-4].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			typedefTable.makeTypedef( *(yyvsp[-1].tok) );
		}
#line 9577 "Parser/parser.cc"
    break;

  case 578:
#line 2297 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typed enumeration is currently unimplemented." ); (yyval.decl) = nullptr;
		}
#line 9585 "Parser/parser.cc"
    break;

  case 579:
#line 2301 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-8].decl)->storageClasses.val != 0 || (yyvsp[-8].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			typedefTable.makeTypedef( *(yyvsp[-5].decl)->name );
			SemanticError( yylloc, "Typed enumeration is currently unimplemented." ); (yyval.decl) = nullptr;
		}
#line 9595 "Parser/parser.cc"
    break;

  case 581:
#line 2311 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); (yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), 0, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 9601 "Parser/parser.cc"
    break;

  case 582:
#line 2313 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].decl)->type->symbolic.name );	(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].decl)->type->symbolic.name, 0, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 9607 "Parser/parser.cc"
    break;

  case 583:
#line 2318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumConstant( (yyvsp[-1].tok), (yyvsp[0].en) ); }
#line 9613 "Parser/parser.cc"
    break;

  case 584:
#line 2320 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumConstant( new string("inline"), nullptr ); }
#line 9619 "Parser/parser.cc"
    break;

  case 585:
#line 2322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( DeclarationNode::newEnumConstant( (yyvsp[-1].tok), (yyvsp[0].en) ) ); }
#line 9625 "Parser/parser.cc"
    break;

  case 586:
#line 2324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( DeclarationNode::newEnumConstant( new string("inline"), nullptr ) ); }
#line 9631 "Parser/parser.cc"
    break;

  case 587:
#line 2329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9637 "Parser/parser.cc"
    break;

  case 588:
#line 2333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].in)->get_expression(); }
#line 9643 "Parser/parser.cc"
    break;

  case 589:
#line 2338 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 9649 "Parser/parser.cc"
    break;

  case 590:
#line 2340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9655 "Parser/parser.cc"
    break;

  case 593:
#line 2344 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9661 "Parser/parser.cc"
    break;

  case 594:
#line 2346 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 9667 "Parser/parser.cc"
    break;

  case 595:
#line 2348 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 9673 "Parser/parser.cc"
    break;

  case 597:
#line 2356 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9679 "Parser/parser.cc"
    break;

  case 598:
#line 2358 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9685 "Parser/parser.cc"
    break;

  case 599:
#line 2360 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-8].decl)->appendList( (yyvsp[-4].decl) )->appendList( (yyvsp[0].decl) ); }
#line 9691 "Parser/parser.cc"
    break;

  case 601:
#line 2366 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9697 "Parser/parser.cc"
    break;

  case 602:
#line 2371 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9703 "Parser/parser.cc"
    break;

  case 603:
#line 2373 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9709 "Parser/parser.cc"
    break;

  case 605:
#line 2376 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 9715 "Parser/parser.cc"
    break;

  case 608:
#line 2383 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9721 "Parser/parser.cc"
    break;

  case 609:
#line 2385 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9727 "Parser/parser.cc"
    break;

  case 611:
#line 2394 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 9733 "Parser/parser.cc"
    break;

  case 612:
#line 2397 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 9739 "Parser/parser.cc"
    break;

  case 613:
#line 2399 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 9745 "Parser/parser.cc"
    break;

  case 618:
#line 2409 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9751 "Parser/parser.cc"
    break;

  case 620:
#line 2416 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 9757 "Parser/parser.cc"
    break;

  case 621:
#line 2418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 9763 "Parser/parser.cc"
    break;

  case 622:
#line 2423 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 9769 "Parser/parser.cc"
    break;

  case 623:
#line 2425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 9775 "Parser/parser.cc"
    break;

  case 624:
#line 2434 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 9781 "Parser/parser.cc"
    break;

  case 625:
#line 2436 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 9787 "Parser/parser.cc"
    break;

  case 631:
#line 2449 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 9793 "Parser/parser.cc"
    break;

  case 634:
#line 2459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 9799 "Parser/parser.cc"
    break;

  case 635:
#line 2460 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (yyvsp[-1].op) == OperKinds::Assign ? (yyvsp[0].in) : (yyvsp[0].in)->set_maybeConstructed( false ); }
#line 9805 "Parser/parser.cc"
    break;

  case 636:
#line 2461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.in) = new InitializerNode( true ); }
#line 9811 "Parser/parser.cc"
    break;

  case 637:
#line 2462 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 9817 "Parser/parser.cc"
    break;

  case 638:
#line 2466 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = new InitializerNode( (yyvsp[0].en) ); }
#line 9823 "Parser/parser.cc"
    break;

  case 639:
#line 2467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 9829 "Parser/parser.cc"
    break;

  case 640:
#line 2472 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 9835 "Parser/parser.cc"
    break;

  case 642:
#line 2474 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ); }
#line 9841 "Parser/parser.cc"
    break;

  case 643:
#line 2475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (InitializerNode *)( (yyvsp[-2].in)->set_last( (yyvsp[0].in) ) ); }
#line 9847 "Parser/parser.cc"
    break;

  case 644:
#line 2476 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.in) = (InitializerNode *)((yyvsp[-3].in)->set_last( (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ) )); }
#line 9853 "Parser/parser.cc"
    break;

  case 646:
#line 2492 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[-1].tok) ) ); }
#line 9859 "Parser/parser.cc"
    break;

  case 648:
#line 2498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-1].en)->set_last( (yyvsp[0].en) )); }
#line 9865 "Parser/parser.cc"
    break;

  case 649:
#line 2504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 9871 "Parser/parser.cc"
    break;

  case 650:
#line 2507 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 9877 "Parser/parser.cc"
    break;

  case 651:
#line 2509 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 9883 "Parser/parser.cc"
    break;

  case 652:
#line 2511 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-4].en) ), maybeMoveBuild<Expression>( (yyvsp[-2].en) ) ) ); }
#line 9889 "Parser/parser.cc"
    break;

  case 653:
#line 2513 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 9895 "Parser/parser.cc"
    break;

  case 655:
#line 2537 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl) ); }
#line 9901 "Parser/parser.cc"
    break;

  case 656:
#line 2542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9907 "Parser/parser.cc"
    break;

  case 657:
#line 2544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 9913 "Parser/parser.cc"
    break;

  case 658:
#line 2549 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "9" );
			if ( (yyvsp[-1].tclass) == TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 9924 "Parser/parser.cc"
    break;

  case 659:
#line 2556 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 9930 "Parser/parser.cc"
    break;

  case 660:
#line 2558 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "9" ); }
#line 9936 "Parser/parser.cc"
    break;

  case 661:
#line 2560 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 9942 "Parser/parser.cc"
    break;

  case 662:
#line 2562 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "9" );
			(yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 9951 "Parser/parser.cc"
    break;

  case 663:
#line 2568 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 9957 "Parser/parser.cc"
    break;

  case 664:
#line 2573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 9963 "Parser/parser.cc"
    break;

  case 665:
#line 2575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 9969 "Parser/parser.cc"
    break;

  case 666:
#line 2577 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::DStype; }
#line 9975 "Parser/parser.cc"
    break;

  case 667:
#line 2581 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 9981 "Parser/parser.cc"
    break;

  case 668:
#line 2586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 9987 "Parser/parser.cc"
    break;

  case 669:
#line 2588 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 9993 "Parser/parser.cc"
    break;

  case 670:
#line 2590 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ftype; }
#line 9999 "Parser/parser.cc"
    break;

  case 671:
#line 2592 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 10005 "Parser/parser.cc"
    break;

  case 672:
#line 2597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10011 "Parser/parser.cc"
    break;

  case 675:
#line 2604 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 10017 "Parser/parser.cc"
    break;

  case 676:
#line 2609 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 10023 "Parser/parser.cc"
    break;

  case 677:
#line 2611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 10029 "Parser/parser.cc"
    break;

  case 678:
#line 2618 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 10035 "Parser/parser.cc"
    break;

  case 680:
#line 2621 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 10041 "Parser/parser.cc"
    break;

  case 681:
#line 2623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)( (yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 10047 "Parser/parser.cc"
    break;

  case 682:
#line 2628 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 10053 "Parser/parser.cc"
    break;

  case 683:
#line 2630 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10059 "Parser/parser.cc"
    break;

  case 684:
#line 2632 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 10065 "Parser/parser.cc"
    break;

  case 685:
#line 2637 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 10071 "Parser/parser.cc"
    break;

  case 686:
#line 2639 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 10077 "Parser/parser.cc"
    break;

  case 687:
#line 2644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "10" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), 0 );
		}
#line 10086 "Parser/parser.cc"
    break;

  case 688:
#line 2649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "11" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 10095 "Parser/parser.cc"
    break;

  case 689:
#line 2657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), 0 ); }
#line 10101 "Parser/parser.cc"
    break;

  case 690:
#line 2659 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-8].tok), (yyvsp[-6].decl), (yyvsp[-2].decl) ); }
#line 10107 "Parser/parser.cc"
    break;

  case 692:
#line 2665 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl) ); }
#line 10113 "Parser/parser.cc"
    break;

  case 697:
#line 2677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 10119 "Parser/parser.cc"
    break;

  case 698:
#line 2682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10125 "Parser/parser.cc"
    break;

  case 699:
#line 2684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 10131 "Parser/parser.cc"
    break;

  case 701:
#line 2692 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl);	}
#line 10137 "Parser/parser.cc"
    break;

  case 702:
#line 2697 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10143 "Parser/parser.cc"
    break;

  case 703:
#line 2699 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->appendList( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 10149 "Parser/parser.cc"
    break;

  case 704:
#line 2704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10155 "Parser/parser.cc"
    break;

  case 706:
#line 2709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 10161 "Parser/parser.cc"
    break;

  case 707:
#line 2713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 10167 "Parser/parser.cc"
    break;

  case 708:
#line 2718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( (yyvsp[0].tok) ) ) ); }
#line 10173 "Parser/parser.cc"
    break;

  case 711:
#line 2722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 10182 "Parser/parser.cc"
    break;

  case 712:
#line 2727 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( false, (yyvsp[-2].constant), 0 ) ) ); }
#line 10188 "Parser/parser.cc"
    break;

  case 713:
#line 2729 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = LinkageSpec::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 10197 "Parser/parser.cc"
    break;

  case 714:
#line 2734 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-1].decl);
		}
#line 10207 "Parser/parser.cc"
    break;

  case 715:
#line 2740 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = LinkageSpec::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 10216 "Parser/parser.cc"
    break;

  case 716:
#line 2745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10226 "Parser/parser.cc"
    break;

  case 717:
#line 2751 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 10235 "Parser/parser.cc"
    break;

  case 718:
#line 2756 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10245 "Parser/parser.cc"
    break;

  case 719:
#line 2762 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 10254 "Parser/parser.cc"
    break;

  case 720:
#line 2767 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10264 "Parser/parser.cc"
    break;

  case 721:
#line 2773 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.val) || (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 10273 "Parser/parser.cc"
    break;

  case 722:
#line 2778 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10283 "Parser/parser.cc"
    break;

  case 724:
#line 2793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].sn) ); }
#line 10289 "Parser/parser.cc"
    break;

  case 725:
#line 2795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].sn) ); }
#line 10295 "Parser/parser.cc"
    break;

  case 726:
#line 2800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; forall = false; }
#line 10301 "Parser/parser.cc"
    break;

  case 727:
#line 2802 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); forall = false; }
#line 10307 "Parser/parser.cc"
    break;

  case 728:
#line 2807 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10318 "Parser/parser.cc"
    break;

  case 729:
#line 2814 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 10327 "Parser/parser.cc"
    break;

  case 730:
#line 2819 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 10336 "Parser/parser.cc"
    break;

  case 731:
#line 2825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 10342 "Parser/parser.cc"
    break;

  case 732:
#line 2828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 10348 "Parser/parser.cc"
    break;

  case 733:
#line 2831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10354 "Parser/parser.cc"
    break;

  case 734:
#line 2835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-4].decl) );
		}
#line 10363 "Parser/parser.cc"
    break;

  case 735:
#line 2841 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10369 "Parser/parser.cc"
    break;

  case 736:
#line 2844 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10375 "Parser/parser.cc"
    break;

  case 737:
#line 2847 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 10381 "Parser/parser.cc"
    break;

  case 741:
#line 2858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 10387 "Parser/parser.cc"
    break;

  case 742:
#line 2863 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10393 "Parser/parser.cc"
    break;

  case 743:
#line 2865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = (yyvsp[-2].constant);
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 10403 "Parser/parser.cc"
    break;

  case 744:
#line 2874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10409 "Parser/parser.cc"
    break;

  case 747:
#line 2881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10415 "Parser/parser.cc"
    break;

  case 748:
#line 2886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 10421 "Parser/parser.cc"
    break;

  case 750:
#line 2892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10427 "Parser/parser.cc"
    break;

  case 751:
#line 2897 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10433 "Parser/parser.cc"
    break;

  case 752:
#line 2899 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 10439 "Parser/parser.cc"
    break;

  case 753:
#line 2901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 10445 "Parser/parser.cc"
    break;

  case 758:
#line 2910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 10451 "Parser/parser.cc"
    break;

  case 759:
#line 2912 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 10457 "Parser/parser.cc"
    break;

  case 760:
#line 2947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 10463 "Parser/parser.cc"
    break;

  case 761:
#line 2949 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10469 "Parser/parser.cc"
    break;

  case 762:
#line 2954 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10475 "Parser/parser.cc"
    break;

  case 764:
#line 2957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10481 "Parser/parser.cc"
    break;

  case 765:
#line 2959 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10487 "Parser/parser.cc"
    break;

  case 766:
#line 2964 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10493 "Parser/parser.cc"
    break;

  case 767:
#line 2966 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10499 "Parser/parser.cc"
    break;

  case 768:
#line 2968 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10505 "Parser/parser.cc"
    break;

  case 769:
#line 2970 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10511 "Parser/parser.cc"
    break;

  case 770:
#line 2975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 10517 "Parser/parser.cc"
    break;

  case 771:
#line 2977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10523 "Parser/parser.cc"
    break;

  case 772:
#line 2979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10529 "Parser/parser.cc"
    break;

  case 773:
#line 2981 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10535 "Parser/parser.cc"
    break;

  case 774:
#line 2983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10541 "Parser/parser.cc"
    break;

  case 775:
#line 2985 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10547 "Parser/parser.cc"
    break;

  case 776:
#line 2987 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10553 "Parser/parser.cc"
    break;

  case 777:
#line 2992 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10559 "Parser/parser.cc"
    break;

  case 778:
#line 2994 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 10565 "Parser/parser.cc"
    break;

  case 779:
#line 2996 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10571 "Parser/parser.cc"
    break;

  case 780:
#line 2998 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10577 "Parser/parser.cc"
    break;

  case 781:
#line 3007 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10583 "Parser/parser.cc"
    break;

  case 783:
#line 3010 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10589 "Parser/parser.cc"
    break;

  case 784:
#line 3015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10595 "Parser/parser.cc"
    break;

  case 785:
#line 3017 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10601 "Parser/parser.cc"
    break;

  case 786:
#line 3019 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 10607 "Parser/parser.cc"
    break;

  case 787:
#line 3021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10613 "Parser/parser.cc"
    break;

  case 788:
#line 3023 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10619 "Parser/parser.cc"
    break;

  case 789:
#line 3028 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10625 "Parser/parser.cc"
    break;

  case 790:
#line 3030 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10631 "Parser/parser.cc"
    break;

  case 791:
#line 3032 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10637 "Parser/parser.cc"
    break;

  case 792:
#line 3034 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10643 "Parser/parser.cc"
    break;

  case 793:
#line 3039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10649 "Parser/parser.cc"
    break;

  case 794:
#line 3041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10655 "Parser/parser.cc"
    break;

  case 795:
#line 3043 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10661 "Parser/parser.cc"
    break;

  case 796:
#line 3045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10667 "Parser/parser.cc"
    break;

  case 797:
#line 3047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10673 "Parser/parser.cc"
    break;

  case 798:
#line 3049 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10679 "Parser/parser.cc"
    break;

  case 802:
#line 3067 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 10685 "Parser/parser.cc"
    break;

  case 803:
#line 3069 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10691 "Parser/parser.cc"
    break;

  case 804:
#line 3071 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 10697 "Parser/parser.cc"
    break;

  case 805:
#line 3073 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10703 "Parser/parser.cc"
    break;

  case 806:
#line 3075 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10709 "Parser/parser.cc"
    break;

  case 807:
#line 3080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10715 "Parser/parser.cc"
    break;

  case 808:
#line 3082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10721 "Parser/parser.cc"
    break;

  case 809:
#line 3084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10727 "Parser/parser.cc"
    break;

  case 810:
#line 3086 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10733 "Parser/parser.cc"
    break;

  case 811:
#line 3091 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10739 "Parser/parser.cc"
    break;

  case 812:
#line 3093 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10745 "Parser/parser.cc"
    break;

  case 813:
#line 3095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10751 "Parser/parser.cc"
    break;

  case 814:
#line 3097 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10757 "Parser/parser.cc"
    break;

  case 815:
#line 3099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10763 "Parser/parser.cc"
    break;

  case 816:
#line 3101 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10769 "Parser/parser.cc"
    break;

  case 817:
#line 3116 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "ID" );
		}
#line 10778 "Parser/parser.cc"
    break;

  case 818:
#line 3121 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10784 "Parser/parser.cc"
    break;

  case 819:
#line 3126 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10790 "Parser/parser.cc"
    break;

  case 821:
#line 3129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10796 "Parser/parser.cc"
    break;

  case 822:
#line 3131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10802 "Parser/parser.cc"
    break;

  case 823:
#line 3136 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10808 "Parser/parser.cc"
    break;

  case 824:
#line 3138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10814 "Parser/parser.cc"
    break;

  case 825:
#line 3140 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10820 "Parser/parser.cc"
    break;

  case 826:
#line 3142 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10826 "Parser/parser.cc"
    break;

  case 827:
#line 3147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 10832 "Parser/parser.cc"
    break;

  case 828:
#line 3149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10838 "Parser/parser.cc"
    break;

  case 829:
#line 3151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10844 "Parser/parser.cc"
    break;

  case 830:
#line 3153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10850 "Parser/parser.cc"
    break;

  case 831:
#line 3155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10856 "Parser/parser.cc"
    break;

  case 832:
#line 3157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10862 "Parser/parser.cc"
    break;

  case 833:
#line 3159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10868 "Parser/parser.cc"
    break;

  case 834:
#line 3164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10874 "Parser/parser.cc"
    break;

  case 835:
#line 3166 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10880 "Parser/parser.cc"
    break;

  case 836:
#line 3168 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 10886 "Parser/parser.cc"
    break;

  case 837:
#line 3170 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10892 "Parser/parser.cc"
    break;

  case 838:
#line 3172 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10898 "Parser/parser.cc"
    break;

  case 839:
#line 3182 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10904 "Parser/parser.cc"
    break;

  case 840:
#line 3184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10910 "Parser/parser.cc"
    break;

  case 842:
#line 3187 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10916 "Parser/parser.cc"
    break;

  case 843:
#line 3189 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10922 "Parser/parser.cc"
    break;

  case 844:
#line 3194 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10928 "Parser/parser.cc"
    break;

  case 845:
#line 3196 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10934 "Parser/parser.cc"
    break;

  case 846:
#line 3198 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10940 "Parser/parser.cc"
    break;

  case 847:
#line 3203 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 10946 "Parser/parser.cc"
    break;

  case 848:
#line 3205 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10952 "Parser/parser.cc"
    break;

  case 849:
#line 3207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10958 "Parser/parser.cc"
    break;

  case 850:
#line 3209 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10964 "Parser/parser.cc"
    break;

  case 851:
#line 3214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10970 "Parser/parser.cc"
    break;

  case 852:
#line 3216 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10976 "Parser/parser.cc"
    break;

  case 853:
#line 3218 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10982 "Parser/parser.cc"
    break;

  case 854:
#line 3232 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10988 "Parser/parser.cc"
    break;

  case 855:
#line 3234 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10994 "Parser/parser.cc"
    break;

  case 857:
#line 3237 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11000 "Parser/parser.cc"
    break;

  case 858:
#line 3239 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11006 "Parser/parser.cc"
    break;

  case 859:
#line 3244 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11012 "Parser/parser.cc"
    break;

  case 860:
#line 3246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11018 "Parser/parser.cc"
    break;

  case 861:
#line 3251 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11024 "Parser/parser.cc"
    break;

  case 862:
#line 3253 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11030 "Parser/parser.cc"
    break;

  case 863:
#line 3255 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11036 "Parser/parser.cc"
    break;

  case 864:
#line 3260 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11042 "Parser/parser.cc"
    break;

  case 865:
#line 3262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11048 "Parser/parser.cc"
    break;

  case 866:
#line 3267 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11054 "Parser/parser.cc"
    break;

  case 867:
#line 3269 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11060 "Parser/parser.cc"
    break;

  case 869:
#line 3287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11066 "Parser/parser.cc"
    break;

  case 870:
#line 3289 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11072 "Parser/parser.cc"
    break;

  case 871:
#line 3294 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( 0, (yyvsp[0].op) ); }
#line 11078 "Parser/parser.cc"
    break;

  case 872:
#line 3296 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 11084 "Parser/parser.cc"
    break;

  case 873:
#line 3298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11090 "Parser/parser.cc"
    break;

  case 874:
#line 3300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11096 "Parser/parser.cc"
    break;

  case 875:
#line 3302 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11102 "Parser/parser.cc"
    break;

  case 877:
#line 3308 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11108 "Parser/parser.cc"
    break;

  case 878:
#line 3310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11114 "Parser/parser.cc"
    break;

  case 879:
#line 3312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11120 "Parser/parser.cc"
    break;

  case 880:
#line 3317 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 11126 "Parser/parser.cc"
    break;

  case 881:
#line 3319 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11132 "Parser/parser.cc"
    break;

  case 882:
#line 3321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11138 "Parser/parser.cc"
    break;

  case 883:
#line 3327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false ); }
#line 11144 "Parser/parser.cc"
    break;

  case 884:
#line 3329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false )->addArray( (yyvsp[0].decl) ); }
#line 11150 "Parser/parser.cc"
    break;

  case 885:
#line 3331 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-4].en), 0, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].en), 0, false ) ); }
#line 11156 "Parser/parser.cc"
    break;

  case 887:
#line 3338 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), 0, false ); }
#line 11162 "Parser/parser.cc"
    break;

  case 888:
#line 3340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 11168 "Parser/parser.cc"
    break;

  case 889:
#line 3342 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newArray( (yyvsp[-2].en), 0, false ) ); }
#line 11174 "Parser/parser.cc"
    break;

  case 890:
#line 3344 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 11180 "Parser/parser.cc"
    break;

  case 892:
#line 3379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 11186 "Parser/parser.cc"
    break;

  case 893:
#line 3381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11192 "Parser/parser.cc"
    break;

  case 894:
#line 3383 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11198 "Parser/parser.cc"
    break;

  case 895:
#line 3388 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].op) ); }
#line 11204 "Parser/parser.cc"
    break;

  case 896:
#line 3390 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 11210 "Parser/parser.cc"
    break;

  case 897:
#line 3392 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 11216 "Parser/parser.cc"
    break;

  case 898:
#line 3394 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11222 "Parser/parser.cc"
    break;

  case 899:
#line 3396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11228 "Parser/parser.cc"
    break;

  case 901:
#line 3402 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11234 "Parser/parser.cc"
    break;

  case 902:
#line 3404 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11240 "Parser/parser.cc"
    break;

  case 903:
#line 3406 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11246 "Parser/parser.cc"
    break;

  case 904:
#line 3411 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 11252 "Parser/parser.cc"
    break;

  case 905:
#line 3413 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11258 "Parser/parser.cc"
    break;

  case 906:
#line 3415 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11264 "Parser/parser.cc"
    break;

  case 908:
#line 3422 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11270 "Parser/parser.cc"
    break;

  case 910:
#line 3433 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false ); }
#line 11276 "Parser/parser.cc"
    break;

  case 911:
#line 3436 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 11282 "Parser/parser.cc"
    break;

  case 912:
#line 3438 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, (yyvsp[-2].decl), false ); }
#line 11288 "Parser/parser.cc"
    break;

  case 913:
#line 3441 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 11294 "Parser/parser.cc"
    break;

  case 914:
#line 3443 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 11300 "Parser/parser.cc"
    break;

  case 915:
#line 3445 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-4].decl), true ); }
#line 11306 "Parser/parser.cc"
    break;

  case 917:
#line 3460 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11312 "Parser/parser.cc"
    break;

  case 918:
#line 3462 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11318 "Parser/parser.cc"
    break;

  case 919:
#line 3467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( 0, (yyvsp[0].op) ); }
#line 11324 "Parser/parser.cc"
    break;

  case 920:
#line 3469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 11330 "Parser/parser.cc"
    break;

  case 921:
#line 3471 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11336 "Parser/parser.cc"
    break;

  case 922:
#line 3473 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11342 "Parser/parser.cc"
    break;

  case 923:
#line 3475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11348 "Parser/parser.cc"
    break;

  case 925:
#line 3481 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11354 "Parser/parser.cc"
    break;

  case 926:
#line 3483 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11360 "Parser/parser.cc"
    break;

  case 927:
#line 3485 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11366 "Parser/parser.cc"
    break;

  case 928:
#line 3490 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11372 "Parser/parser.cc"
    break;

  case 929:
#line 3492 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11378 "Parser/parser.cc"
    break;

  case 932:
#line 3502 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11384 "Parser/parser.cc"
    break;

  case 935:
#line 3513 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11390 "Parser/parser.cc"
    break;

  case 936:
#line 3515 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11396 "Parser/parser.cc"
    break;

  case 937:
#line 3517 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11402 "Parser/parser.cc"
    break;

  case 938:
#line 3519 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11408 "Parser/parser.cc"
    break;

  case 939:
#line 3521 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11414 "Parser/parser.cc"
    break;

  case 940:
#line 3523 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11420 "Parser/parser.cc"
    break;

  case 941:
#line 3530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11426 "Parser/parser.cc"
    break;

  case 942:
#line 3532 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11432 "Parser/parser.cc"
    break;

  case 943:
#line 3534 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11438 "Parser/parser.cc"
    break;

  case 944:
#line 3536 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 11444 "Parser/parser.cc"
    break;

  case 945:
#line 3538 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11450 "Parser/parser.cc"
    break;

  case 946:
#line 3541 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11456 "Parser/parser.cc"
    break;

  case 947:
#line 3543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11462 "Parser/parser.cc"
    break;

  case 948:
#line 3545 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11468 "Parser/parser.cc"
    break;

  case 949:
#line 3547 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 11474 "Parser/parser.cc"
    break;

  case 950:
#line 3549 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11480 "Parser/parser.cc"
    break;

  case 951:
#line 3554 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 11486 "Parser/parser.cc"
    break;

  case 952:
#line 3556 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 11492 "Parser/parser.cc"
    break;

  case 953:
#line 3561 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 11498 "Parser/parser.cc"
    break;

  case 954:
#line 3563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl)->addQualifiers( (yyvsp[-4].decl) ), true ); }
#line 11504 "Parser/parser.cc"
    break;

  case 956:
#line 3590 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11510 "Parser/parser.cc"
    break;

  case 960:
#line 3601 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11516 "Parser/parser.cc"
    break;

  case 961:
#line 3603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11522 "Parser/parser.cc"
    break;

  case 962:
#line 3605 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11528 "Parser/parser.cc"
    break;

  case 963:
#line 3607 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11534 "Parser/parser.cc"
    break;

  case 964:
#line 3609 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11540 "Parser/parser.cc"
    break;

  case 965:
#line 3611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11546 "Parser/parser.cc"
    break;

  case 966:
#line 3618 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11552 "Parser/parser.cc"
    break;

  case 967:
#line 3620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11558 "Parser/parser.cc"
    break;

  case 968:
#line 3622 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11564 "Parser/parser.cc"
    break;

  case 969:
#line 3624 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11570 "Parser/parser.cc"
    break;

  case 970:
#line 3626 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11576 "Parser/parser.cc"
    break;

  case 971:
#line 3628 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11582 "Parser/parser.cc"
    break;

  case 972:
#line 3633 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 11588 "Parser/parser.cc"
    break;

  case 973:
#line 3635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11594 "Parser/parser.cc"
    break;

  case 974:
#line 3637 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11600 "Parser/parser.cc"
    break;

  case 975:
#line 3644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 11606 "Parser/parser.cc"
    break;

  case 976:
#line 3646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 11612 "Parser/parser.cc"
    break;

  case 979:
#line 3670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 11618 "Parser/parser.cc"
    break;

  case 980:
#line 3672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 11624 "Parser/parser.cc"
    break;


#line 11628 "Parser/parser.cc"

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
#line 3675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "make install" //
// End: //
