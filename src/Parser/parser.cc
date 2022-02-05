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
#define YYLAST   19473

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  175
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  288
/* YYNRULES -- Number of rules.  */
#define YYNRULES  978
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  1989

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
    1196,  1198,  1201,  1203,  1205,  1208,  1210,  1212,  1218,  1219,
    1241,  1243,  1245,  1248,  1251,  1254,  1256,  1258,  1260,  1263,
    1266,  1268,  1271,  1278,  1280,  1282,  1284,  1286,  1291,  1293,
    1295,  1297,  1302,  1304,  1309,  1311,  1313,  1315,  1318,  1322,
    1325,  1329,  1331,  1333,  1335,  1337,  1339,  1341,  1343,  1345,
    1347,  1349,  1354,  1355,  1359,  1365,  1370,  1375,  1376,  1380,
    1384,  1389,  1390,  1396,  1400,  1402,  1404,  1406,  1409,  1411,
    1416,  1418,  1423,  1425,  1427,  1432,  1434,  1440,  1441,  1445,
    1446,  1447,  1448,  1452,  1457,  1458,  1460,  1462,  1464,  1468,
    1472,  1473,  1477,  1479,  1481,  1483,  1485,  1491,  1492,  1498,
    1499,  1503,  1504,  1509,  1511,  1517,  1518,  1520,  1525,  1530,
    1541,  1542,  1546,  1547,  1553,  1554,  1558,  1560,  1564,  1566,
    1570,  1571,  1575,  1576,  1580,  1581,  1582,  1586,  1588,  1603,
    1604,  1605,  1606,  1608,  1612,  1614,  1618,  1625,  1627,  1629,
    1634,  1635,  1637,  1639,  1641,  1673,  1676,  1681,  1683,  1689,
    1694,  1699,  1710,  1715,  1720,  1725,  1730,  1739,  1743,  1750,
    1752,  1753,  1754,  1760,  1762,  1767,  1768,  1769,  1778,  1779,
    1780,  1784,  1785,  1786,  1795,  1796,  1797,  1802,  1803,  1812,
    1813,  1818,  1819,  1823,  1825,  1827,  1829,  1831,  1835,  1840,
    1841,  1843,  1853,  1854,  1859,  1861,  1863,  1865,  1867,  1870,
    1872,  1874,  1879,  1881,  1883,  1885,  1887,  1889,  1891,  1893,
    1895,  1897,  1899,  1901,  1903,  1905,  1907,  1909,  1911,  1913,
    1915,  1917,  1919,  1921,  1923,  1925,  1927,  1929,  1931,  1933,
    1938,  1939,  1943,  1950,  1951,  1957,  1958,  1960,  1962,  1964,
    1969,  1971,  1976,  1977,  1979,  1981,  1986,  1988,  1990,  1992,
    1994,  1996,  2001,  2002,  2004,  2006,  2011,  2013,  2012,  2016,
    2024,  2025,  2027,  2029,  2034,  2035,  2037,  2042,  2043,  2045,
    2047,  2052,  2053,  2055,  2060,  2062,  2064,  2066,  2067,  2069,
    2074,  2076,  2078,  2083,  2084,  2088,  2089,  2094,  2093,  2098,
    2097,  2105,  2104,  2115,  2114,  2124,  2129,  2130,  2135,  2141,
    2155,  2156,  2160,  2162,  2164,  2170,  2172,  2174,  2176,  2178,
    2180,  2182,  2184,  2190,  2191,  2196,  2198,  2200,  2209,  2211,
    2212,  2213,  2215,  2217,  2218,  2223,  2224,  2225,  2230,  2232,
    2235,  2242,  2243,  2244,  2250,  2255,  2257,  2263,  2264,  2270,
    2271,  2275,  2280,  2283,  2282,  2286,  2289,  2295,  2294,  2303,
    2309,  2313,  2315,  2320,  2322,  2324,  2326,  2332,  2335,  2341,
    2342,  2344,  2345,  2346,  2348,  2350,  2357,  2358,  2360,  2362,
    2367,  2368,  2374,  2375,  2377,  2378,  2383,  2384,  2385,  2387,
    2395,  2396,  2398,  2401,  2403,  2407,  2408,  2409,  2411,  2413,
    2418,  2420,  2425,  2427,  2436,  2438,  2443,  2444,  2445,  2449,
    2450,  2451,  2456,  2457,  2462,  2463,  2464,  2465,  2469,  2470,
    2475,  2476,  2477,  2478,  2479,  2493,  2494,  2499,  2500,  2506,
    2508,  2511,  2513,  2515,  2538,  2539,  2545,  2546,  2552,  2551,
    2561,  2560,  2564,  2570,  2576,  2577,  2579,  2583,  2588,  2590,
    2592,  2594,  2600,  2601,  2605,  2606,  2611,  2613,  2620,  2622,
    2623,  2625,  2630,  2632,  2634,  2639,  2641,  2646,  2651,  2659,
    2661,  2666,  2667,  2672,  2673,  2677,  2678,  2679,  2684,  2686,
    2692,  2694,  2699,  2701,  2707,  2708,  2712,  2716,  2720,  2722,
    2723,  2724,  2729,  2732,  2731,  2743,  2742,  2754,  2753,  2765,
    2764,  2778,  2784,  2786,  2792,  2793,  2798,  2805,  2810,  2816,
    2819,  2822,  2826,  2832,  2835,  2838,  2843,  2844,  2845,  2849,
    2855,  2856,  2866,  2867,  2871,  2872,  2877,  2882,  2883,  2889,
    2890,  2892,  2897,  2898,  2899,  2900,  2901,  2903,  2938,  2940,
    2945,  2947,  2948,  2950,  2955,  2957,  2959,  2961,  2966,  2968,
    2970,  2972,  2974,  2976,  2978,  2983,  2985,  2987,  2989,  2998,
    3000,  3001,  3006,  3008,  3010,  3012,  3014,  3019,  3021,  3023,
    3025,  3030,  3032,  3034,  3036,  3038,  3040,  3052,  3053,  3054,
    3058,  3060,  3062,  3064,  3066,  3071,  3073,  3075,  3077,  3082,
    3084,  3086,  3088,  3090,  3092,  3107,  3112,  3117,  3119,  3120,
    3122,  3127,  3129,  3131,  3133,  3138,  3140,  3142,  3144,  3146,
    3148,  3150,  3155,  3157,  3159,  3161,  3163,  3173,  3175,  3177,
    3178,  3180,  3185,  3187,  3189,  3194,  3196,  3198,  3200,  3205,
    3207,  3209,  3223,  3225,  3227,  3228,  3230,  3235,  3237,  3242,
    3244,  3246,  3251,  3253,  3258,  3260,  3277,  3278,  3280,  3285,
    3287,  3289,  3291,  3293,  3298,  3299,  3301,  3303,  3308,  3310,
    3312,  3318,  3320,  3322,  3325,  3329,  3331,  3333,  3335,  3369,
    3370,  3372,  3374,  3379,  3381,  3383,  3385,  3387,  3392,  3393,
    3395,  3397,  3402,  3404,  3406,  3412,  3413,  3415,  3424,  3427,
    3429,  3432,  3434,  3436,  3450,  3451,  3453,  3458,  3460,  3462,
    3464,  3466,  3471,  3472,  3474,  3476,  3481,  3483,  3491,  3492,
    3493,  3498,  3499,  3504,  3506,  3508,  3510,  3512,  3514,  3521,
    3523,  3525,  3527,  3529,  3532,  3534,  3536,  3538,  3540,  3545,
    3547,  3549,  3554,  3580,  3581,  3583,  3587,  3588,  3592,  3594,
    3596,  3598,  3600,  3602,  3609,  3611,  3613,  3615,  3617,  3619,
    3624,  3626,  3628,  3635,  3637,  3655,  3657,  3662,  3663
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

#define YYPACT_NINF (-1622)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-859)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
      71, 11363,   118,   140, 15750,    46, -1622, -1622, -1622, -1622,
   -1622, -1622, -1622, -1622, -1622, -1622, -1622,   200,   812,   223,
   -1622, -1622, -1622, -1622, -1622, -1622, -1622, -1622, -1622, -1622,
   -1622, -1622, -1622, -1622, -1622, -1622, -1622, -1622, -1622, -1622,
   -1622, -1622, -1622, -1622, -1622, -1622, -1622,    82,   248, -1622,
   -1622, -1622, -1622, -1622, -1622,  4460,  4460,   278, 11363,   313,
     343, -1622, -1622,   364, -1622, -1622, -1622, -1622, -1622, -1622,
   -1622, -1622, -1622,  3725, -1622,   693,   240, -1622, -1622, -1622,
   -1622, -1622, 15600, -1622, -1622,   303,   389,   285,   450, -1622,
    4460,   389,   389,   389,   379,  4123,   561,   784, 11522, -1622,
   -1622, -1622, 15450,  1302, -1622, -1622, -1622,  2116,   570,  5667,
    1175,   872,  2116,  1088,   458, -1622, -1622, -1622, -1622,   549,
   -1622, -1622, -1622, -1622,   479, -1622, -1622, -1622, -1622, -1622,
     494,   511,   549, -1622,   549,   535, -1622, -1622, -1622, 16306,
    4460, -1622, -1622,  4460, -1622, 11363,   475, 16358, -1622, -1622,
    4222, 17370, -1622,   977,   977, -1622,  2533, -1622, -1622, -1622,
   -1622,   231, 14060,  2873,   549, -1622, -1622, -1622, -1622, -1622,
   -1622,   551, -1622,   541,   583,   586, -1622,   567, 18948, 14680,
    3461,  3725,   444,   589,   608,   623,   636,   683,   685, -1622,
   -1622, 16508, 10714,   618, -1622, 15893, -1622, -1622, -1622, -1622,
     715, -1622, -1622,   711, -1622,  9516,   860, 18300, -1622,   743,
    4460,   511,   758,   757,   760,   763, -1622, -1622, -1622,  3133,
    2979,   775,   814,   133, -1622, -1622,   549,   549,   183,   205,
     179,   183, -1622,   549,   549, -1622,  3528, -1622, -1622,   789,
     796,   977, 13638, -1622, -1622, 15600, -1622, -1622,  2116, -1622,
    2701,   458,   803,   908,   205,  4460,   285, -1622, 12918, -1622,
     977,   977,   864,   908,   205,  4460, -1622, 12667, -1622, -1622,
     977, -1622,   977, -1622,   718,  3383,  4460, -1622,  2554,   852,
   -1622, -1622, -1622, 16052,   511,   218, -1622, -1622, 17420, -1622,
     814,   177, -1622, 18948, 17370,  3278,  3528, -1622,   214, -1622,
   -1622, -1622, 16358,  4460,   884, -1622, -1622, -1622, -1622,  4460,
    3145,   245,   480, -1622,  4460,   541, -1622,    65,   549,   889,
   16560,   788, 14218, 13796,  2116,  2116, -1622,  2116,   977,  2116,
     977, -1622, -1622,   549, -1622,   899, -1622, 16710, -1622, -1622,
   -1622, 16762,   715, -1622,   903,   327,  1458,   911,   458,   919,
   -1622,  2533,   906,   541,  2533,  2393, -1622,   956,   997, 19020,
     972,   974, 18948, 19092,   976, -1622, -1622, -1622, -1622, -1622,
   -1622, -1622, 19164, 19164, 14526,  1000,  4320, -1622, -1622, -1622,
   -1622,  1003, -1622,  1005, -1622,   853, -1622, 18948, 18948, -1622,
     963,   658,   815,   862,   647,   881,  1004,  1012,  1007,  1045,
      43, -1622,   505, -1622,  1061, -1622,   932,  3442, 14988, -1622,
   -1622,   616,  1061, -1622, -1622,   563, -1622, -1622,  3461,  1050,
    1066,  1071,  1073,  1075,  1101, -1622, -1622,   286,  1105, -1622,
     588,  1105, -1622, -1622, 16306, -1622,   960,  1118, 15142, -1622,
   -1622,  2359,  3752,  1145, 14218,  1153,   674,   736, -1622, -1622,
   -1622, -1622, -1622,  4460,  4448, -1622, -1622, -1622, -1622, -1622,
   -1622,  1140,  3114,  1000,  9516,  1148,  1177, -1622, -1622,  1133,
   18300,   630, -1622, -1622, -1622, 18372,  1171, -1622, -1622, -1622,
   -1622, -1622,  3133,   611,  1151,  1184,  1186,   695,  1197,  1204,
    1210,  2979, -1622, -1622,   549,  1196,   285,  1213, -1622, -1622,
    1220, -1622, -1622,   511,   908, -1622, -1622, -1622,   511, -1622,
   -1622,  3528, -1622, 14988, 14988, -1622,   977,  4222, 18148, 14060,
   -1622, -1622, -1622, -1622, -1622,   511,   908,   177, -1622, -1622,
    2116,  1234,   908,   205, -1622,   511,   908, -1622, 12810, -1622,
     977,   977, -1622, -1622,  1236,   381,  1239,   458,  1240, -1622,
   17578, -1622,   602, -1622,  1330, 18044, -1622,  4222, 16921, 13638,
   -1622, 16052, 19236, -1622, -1622, -1622, -1622, -1622,  3278,   729,
    3528, -1622, 14060,   814, -1622,  1246, -1622,  1252, -1622, -1622,
   -1622, -1622, -1622,  2533, -1622, -1622,  1327,  4391, 16762, 10714,
   -1622, 16973, -1622,   977,   977, -1622, -1622,   715, -1622,   679,
    1250,  1390, 18948,   910,  1220,  1235, -1622,   549,   549, -1622,
    1105, -1622, 16560, -1622, -1622, 17859,   977,   977, -1622,  4391,
     549, -1622, 17227, -1622, -1622, 16710, -1622,   231,  1254,   116,
    1256,  1458,   656, 16358,   692, -1622, -1622, -1622, -1622, -1622,
   -1622,   702, -1622,  1261,  1237, -1622, 14834, -1622, 17025, 17025,
   -1622, 14834, -1622, 18948, 14834, -1622, -1622, 16104, 17025, 17025,
     932,  1251,  1278,   629,  1373, -1622,   703,  1263,   971,  1264,
   -1622, 18372, 18948, 18444,  1260,  2554,  2554, -1622,  1541, -1622,
   -1622, 18516,  2173, 18948, 18516,  2554, -1622, -1622, 18948, 18948,
   18948, 18948, 18948, 18948, 18948, 18948, 18948, 18948, 18948, 18948,
   18948, 18948, 18948, 18948, 18948, 18948, 18948, 18588,  1244,   567,
    3918, 10714, -1622, -1622, -1622, -1622, -1622, -1622, -1622, -1622,
   -1622, -1622, -1622,  1262, 18948, -1622, -1622,   616,  2065, -1622,
   -1622,   549,   549, -1622, -1622, 14988, -1622,   322,  1105, -1622,
     720,  1105, -1622, -1622, -1622,  1220, -1622, -1622,  1220, 19308,
   -1622, -1622, 10714,  1267,  1268,  4137,  1406,  2328,   346,  1235,
   -1622,   549,   549,  1235,   387, -1622,   549,   549, 18948,  4460,
     980,   986,  1235,   242, 13586, 13586,  4460, -1622, -1622, 18948,
    1133, -1622,  9516,  1280, -1622,  1943, -1622, -1622, -1622, -1622,
   -1622,   740, -1622, 13586,  2554,  4222,  2554,   748,  1275,  1284,
    1285,   749,  1287,  1293,  1295,   394,  1105, -1622, -1622,   397,
    1105, -1622, -1622, -1622,  4222,   567, -1622,  1105, 19308, -1622,
     511, 17578, -1622, -1622,   778,  1298,   785,  1301, -1622,  1294,
   -1622,   511, -1622, -1622,   511,   908,  1294, -1622,   511,  1296,
    1297,  1299, -1622, -1622, 17859, -1622,  1306, -1622, -1622, -1622,
    2554,  4460,  9873,  1393,  1288, 17946, -1622,  1118, -1622, 13586,
     798, -1622,  1294, -1622, 16358, 14988,  1290, -1622,  1290, -1622,
   -1622, -1622, -1622, 16710, -1622, 10876, 15296, -1622, 17578,  1313,
    1317,  1319, -1622,  8909,   549, -1622,   910, -1622, -1622, -1622,
   -1622,  1220, -1622, -1622, -1622,   977, -1622,  3636, -1622, -1622,
     458,  2047,  1326, -1622, 18300, -1622,  1458,  1254, -1622, -1622,
    1318,  1328,  2393, 18516, -1622,  1332,   240,  1329,  1335,  1338,
    1337,  1339, 18948,  1342,  1343,  1344, 10714, 18948, -1622, -1622,
    1533, -1622, -1622, -1622, 18948, -1622,  1346,  1348, 18156,   994,
   -1622, 18516, -1622, -1622, -1622,  4072, -1622, -1622,   808, -1622,
   -1622, -1622, -1622,  4072, -1622, -1622,   998,   230, -1622, -1622,
     963,   963,   963,   658,   658,   815,   815,   862,   862,   862,
     862,   647,   647,   881,  1004,  1012,  1007,  1045, 18948,  1041,
   -1622,  1349,  4072, -1622, -1622,  9516, -1622, 17578,  1350,  1351,
    1354,  2065, -1622, -1622, -1622, -1622, -1622, -1622, -1622, -1622,
    1220, -1622, -1622,  1220, 17578, 17578, -1622, -1622,  4137,   755,
    1356,  1357,  1359,  1360,  3046,  2328, -1622, -1622, -1622, -1622,
   -1622, -1622, -1622, -1622, -1622, -1622, -1622, -1622, -1622, -1622,
    1358, -1622,  1235, -1622, -1622, -1622, -1622, -1622, -1622, -1622,
   -1622,  1364,  1365, -1622,   285,  4072,  1056,   -81, -1622, -1622,
    1333, -1622, 18300, -1622, 18948, -1622, 18660, 13586, -1622, -1622,
   -1622,  1352,   399,  1105, -1622,   410,  1105, -1622, -1622, -1622,
   -1622,  1220, -1622, -1622, -1622,  1220,   814,  1368,  1220, -1622,
   -1622, -1622, -1622, -1622, -1622, -1622,  1374, -1622, -1622,  1294,
   -1622,   511, -1622, -1622, -1622, -1622, -1622,  9432,  1370,  1369,
   -1622,   416, -1622,   518,   170, 10552,  1379, 13415,  1380,  1381,
    2440,  2994,  3026, 18732,  1382, -1622, -1622,  1383,  1384, -1622,
   -1622,   511, 18948, 18948,  1510,  1385,   272, -1622,  1463,  1387,
    1366, -1622, -1622, -1622,  9701, -1622, -1622, -1622, -1622, -1622,
    2830, -1622, -1622, -1622,  1449, -1622, -1622, -1622,  2554, -1622,
   -1622, 11999, 15600,  1397, -1622,  4460, -1622,  1398,  1389,  1402,
   -1622,  1074, -1622, -1622, -1622,  4222, -1622, -1622,  1401,  1403,
     843, 16358,   541,   541, -1622, -1622,  1000,  1118, 15142, -1622,
    1061, -1622, 11038, -1622,   451,  1105, -1622,   977,  9146, -1622,
   -1622,  1458,   549,   549,   231,   116, -1622, -1622,  1254,  1423,
    1428, -1622, -1622,   876,   431, 10714,  2554, -1622,   431, 16156,
     431, -1622, 18948, 18948, 18948, -1622, -1622, -1622, -1622, 18948,
   18948,  1421,  9516, -1622, -1622,  1424,   525, -1622,  3826, -1622,
   -1622,  1076, -1622,   330, -1622, 18516,  1080, -1622, 18372, -1622,
   -1622, 18948,  1407,  1082,  1092,  1133, -1622,   492,  1105, -1622,
   -1622, 17578, 17578, -1622, -1622,  1431,   500,  1105, -1622,   506,
    2091,   549,   549, -1622, -1622, 17578, 17578, -1622,  1429, -1622,
   14060, 14060,  1433,  1430,  1435,  1437, -1622,  1439, 18948, 18948,
    1096,  1432, -1622, -1622, -1622, -1622, -1622, -1622,  1441, 18948,
   -1622, -1622, -1622,  1220, -1622, -1622, -1622,  1220, 17578, 17578,
     285,   549,  1106,  1451,  1452, -1622, -1622,  1455, 12152, 12305,
   12458, 16358, 17025, 17025,  1457, -1622,  1434,  1442,  2250, 12760,
   -1622,   424,  4460, -1622, -1622,  4460, -1622, 18228,   165,   328,
   -1622, -1622, -1622, -1622, 18948,  1460,  1537, 10389, 10045, -1622,
    1443, -1622,  1445, 18948,  1446,  9516,  1447, 18948, 18372, 18948,
     907, -1622,  1448,   113, -1622,   235,  1475, -1622, -1622,  1477,
   -1622,  1453, -1622,  1456,  1480, 13415,   564, 13076,   549,   434,
   -1622, -1622, -1622,  1481, -1622,  1478, -1622,  1483, -1622,  1484,
   -1622,  1485, -1622, -1622, -1622, -1622, 11200,  1479,  1482,  1489,
   -1622,  1487, -1622, -1622, -1622,  1220, 18948, 18948,  1118,  1490,
   -1622,  1254, -1622,  1505,    87, -1622,  1517, -1622, -1622, 16358,
   -1622,  1513,  1512,   877, -1622,  1521, -1622, -1622, -1622, -1622,
   -1622,  9516,  1133, 18372, -1622,  1554,  4072, -1622,  1554,  1554,
   -1622,  4072,  3944,  4341, -1622, -1622,  1116, -1622, -1622, -1622,
    1526,  1529, -1622, -1622, -1622,  1220, -1622, -1622,  1530,  1532,
     549, -1622, -1622, -1622,  1220, -1622, -1622, -1622,  1535, -1622,
   -1622, -1622, -1622, -1622, -1622, -1622, -1622, -1622, -1622, -1622,
   -1622, -1622,  1534, -1622, -1622, -1622, -1622,  1538,  1549,   549,
   -1622, 17578, 17578, -1622, -1622, -1622, -1622, 18948, -1622, -1622,
    1557, -1622,  1457,  1457,  1457,   805,  1461,   442, -1622,  4001,
     447, 14988, -1622, -1622, -1622,  3396, 18948,  3589,   470, -1622,
   -1622,    73,  1556,  1556,  4460, -1622, -1622, 17727, -1622, 18948,
    1562,  1568, -1622, -1622, -1622, -1622,   883,  1571, 13415,  1387,
    1570, 18948,   303,  1573,   379, 12617, 16358, 13415, 18948, 18948,
     581,   102, -1622, 18948, -1622, -1622,   487, -1622,  1133, -1622,
     891,   897,   900, -1622, -1622, -1622, -1622,   511,   907,  1574,
   -1622, -1622, 18948, -1622,  1588,   567, 10552, -1622, -1622, -1622,
   -1622, 18948,  1631, -1622, 13415, -1622,   549, 14060, -1622, -1622,
   16358, -1622, -1622, -1622, -1622, -1622,  1585, -1622, 17578, -1622,
   -1622,  1586, -1622,  1587,  1595,  1590,  1458, -1622, -1622, -1622,
   -1622, 18948, -1622, 16156, 18948,  1133,  1597,  1120, -1622,  1132,
   -1622,  4072, -1622,  4072, -1622, -1622, -1622, -1622, 17578,  1599,
    1600, -1622, -1622, 17578, 17578,  1602,  1603,  1144, 13744, 13902,
   -1622,  1608, -1622, -1622, -1622, -1622,  1612,  1613,  1146, -1622,
   -1622, -1622, -1622,   805,  2301,   491, -1622, -1622, -1622, -1622,
     549,   549, -1622, -1622, -1622,   542, -1622,   902,  3396,   661,
   -1622,  3589,   549, -1622, -1622, -1622, -1622, -1622, -1622, -1622,
   -1622,   543, 13415,    54, 18804, -1622, 13415,  1387, 14376, -1622,
    1387,  1591, -1622, -1622, -1622, -1622,  7957, 18948, 13415, 10217,
    1592, -1622,  1620,    90, 13415, -1622, -1622,  1621, -1622, -1622,
    1601,   567,   356,  1622,  1625,  1086,  1683, -1622, -1622, -1622,
    4460,  4222, -1622, -1622,  1623,  1624, -1622, -1622, -1622,  1458,
    1254,  1633, -1622, -1622, -1622,  1634, -1622, -1622, -1622,  1160,
    1164, -1622, -1622, -1622, -1622, -1622, -1622, -1622, -1622, -1622,
   -1622,  1628, -1622, -1622,  1632,  1636, -1622, -1622, -1622,  1637,
    1638,  1639,  2301, -1622,   549, -1622, -1622, -1622, -1622, -1622,
    1640,  4001, -1622, -1622, 18948,  1629, -1622, -1622, 13179, -1622,
    1610,   930,  1719, 18948,  1642, 18948,  1049,  1626,   396,  1722,
   -1622, 18948,  1627, -1622, -1622, -1622, -1622, 17175, -1622,  1641,
    1635,   123, 13415, -1622, 18948, 18516,   533, -1622, -1622, -1622,
    1654, -1622, -1622,  1254,  1658, -1622, -1622, -1622, -1622,  1656,
    1657,  1663, 14060,  1660, -1622, -1622,   558,  1105, -1622, -1622,
     805, -1622,    33, -1622,  1167, -1622, -1622, 11681, -1622, -1622,
   -1622,    72, 13415, -1622,  1387,  1666,  1669, 18948, 18948, 18948,
   13415, -1622, -1622, 11681, 17175, -1622,  4015, 16973,  2554,  1664,
   -1622,  1725,  1674,   373,  1672, -1622,  1756, -1622,   933, 13415,
    1684, 13415, 13415, -1622,  1686, -1622, -1622, -1622, -1622, -1622,
   -1622, -1622, -1622,  1220, -1622, 18948, 18948, -1622,  1253, 11840,
   -1622, 13415, -1622, -1622,  1670,  1675,   429, -1622,  1387, -1622,
    1253, -1622,  1668,  2847,  2732, -1622, -1622, -1622,   123,  1690,
   18948,  1677,   123,   123, 13415, -1622, -1622, 18948,  1739,  1741,
   -1622, 17578, -1622, -1622, 13179, -1622,  1253, -1622, -1622, 18948,
   18876, 18948, -1622,  1668, 18948,  1698,  2732,  1700,   567,  1708,
   -1622,   552, -1622, -1622,   941,  1683,   363, -1622, -1622, 13297,
    1715, 13179,  1387, -1622,  1387,  1387,  1718,  1716, -1622,   511,
     567,  1720, -1622,  1696,   567, -1622, -1622, 13415,  1798,  1721,
   -1622, -1622, 13297, -1622,   511, -1622, -1622,  1187, 18948, -1622,
     942, -1622, 13415, -1622, -1622,   567,  2554,  1723,  1699, -1622,
   -1622, -1622,   943, -1622, -1622,  1703,  2554, -1622, -1622
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
     446,   447,   448,   449,   450,   457,   458,   742,   460,   533,
     534,   537,   539,   535,   541,     0,     0,     0,   407,     0,
       0,    16,   504,   510,     9,    10,    11,    12,    13,    14,
      15,   708,    93,     0,    19,     0,     2,    91,    92,    17,
      18,   758,   407,   709,   356,     0,   359,   634,   361,   370,
       0,   360,   390,   391,     0,     0,     0,     0,   487,   409,
     411,   417,   407,   419,   422,   472,   459,   395,   465,   470,
     396,   482,   397,   497,   501,   507,   486,   513,   525,   742,
     530,   531,   514,   580,   362,   363,     3,   710,   721,   412,
       0,     0,   742,   780,   742,     2,   797,   798,   799,   407,
       0,   956,   957,     0,     1,   407,     0,   407,   379,   380,
       0,   487,   401,   402,   403,   713,     0,   536,   538,   540,
     542,     0,   407,     0,   743,   744,   532,   461,   627,   628,
     626,   687,   682,   672,     0,     0,   711,     0,     0,   407,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   505,
     508,   407,   407,     0,   958,   487,   787,   805,   962,   955,
     953,   960,   355,     0,   155,   640,   154,     0,   364,     0,
       0,     0,     0,     0,     0,     0,   354,   857,   858,     0,
       0,   389,   740,   742,   736,   761,   742,   742,   738,     2,
     742,   737,   818,   742,   742,   815,     0,   480,   481,     0,
       0,   407,   407,   424,     2,   407,   371,   410,   420,   473,
       0,   502,     0,   724,     2,     0,   634,   372,   487,   466,
     483,   498,     0,   724,     2,     0,   423,   467,   474,   475,
     484,   489,   499,   503,     0,   517,     0,   702,     2,     2,
     722,   779,   781,   407,     0,     2,     2,   966,   487,   969,
     740,   740,     3,     0,   487,     0,     0,   382,   742,   738,
     737,     2,   407,     0,     0,   668,   670,   669,   671,     0,
       0,   664,     0,   654,     0,   663,   674,     0,   742,     2,
     407,   977,   408,   407,   419,   398,   465,   399,   490,   400,
     497,   494,   515,   742,   516,     0,   615,   407,   616,   931,
     932,   407,   617,   619,   504,   510,     0,   581,   582,     0,
     745,     0,   685,   673,     0,   749,    21,     0,    20,     0,
       0,     0,     0,     0,     0,    23,    25,     4,     8,     5,
       6,     7,     0,     0,   407,     2,     0,    94,    95,    96,
      97,    78,    24,    79,    36,    77,    98,     0,     0,   113,
     115,   119,   122,   125,   130,   133,   135,   137,   139,   141,
     143,   146,     0,    26,     0,   511,     2,    98,   407,   147,
     679,   630,   501,   632,   678,     0,   629,   633,     0,     0,
       0,     0,     0,     0,     0,   759,   785,   742,   795,   803,
     807,   813,     2,   964,   407,   967,     2,    91,   407,     3,
     614,     0,   977,     0,   408,   465,   490,   497,     3,     3,
     596,   600,   610,   616,   617,     2,   788,   806,   954,     2,
       2,    23,     0,     2,   640,    24,     0,   638,   641,   975,
       0,     0,   647,   636,   635,     0,     0,   726,     2,     2,
       2,     2,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   764,   821,   742,     0,   634,     2,   760,   768,
     884,   762,   763,     0,   724,     2,   817,   825,     0,   819,
     820,     0,   385,   407,   407,   471,   408,     0,   487,   407,
     959,   963,   961,   488,   706,     0,   724,   740,   365,   373,
     421,     0,   724,     2,   706,     0,   724,   683,   468,   469,
     485,   500,   506,   509,   504,   510,   528,   529,     0,   684,
     407,   624,     0,   191,   348,   407,     3,     0,   487,   407,
     723,   407,     0,   367,     2,   368,   703,   387,     0,     0,
       0,     2,   407,   740,   706,     0,     2,     0,   667,   666,
     665,   660,   418,     0,   658,   675,   463,     0,   407,   407,
     933,   408,   404,   405,   406,   937,   928,   929,   935,     2,
       2,    92,     0,   893,   907,   977,   889,   742,   742,   898,
     905,   622,   407,   495,   618,   408,   491,   492,   496,     0,
     742,   943,   408,   948,   940,   407,   945,     0,   975,   587,
       0,     0,     0,   407,     0,   757,   756,   752,   754,   755,
     753,     0,   747,   750,     0,    22,   407,    85,   407,   407,
      80,   407,    87,     0,   407,    83,    84,   407,   407,   407,
       2,    94,    95,     0,     0,   173,     0,     0,   531,     0,
     953,     0,     0,     0,     0,     0,     0,    46,     0,    52,
      53,    57,     0,     0,    57,     0,    81,    82,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   407,   156,   157,   158,   159,   160,   161,   162,   163,
     164,   165,   166,   154,     0,   152,   153,     2,   869,   631,
     866,   742,   742,   874,   512,   407,   786,   742,   796,   804,
     808,   814,     2,   789,   791,   793,     2,   809,   811,     0,
     965,   968,   407,     0,     0,     2,    92,   893,   742,   977,
     839,   742,   742,   977,   742,   854,   742,   742,     3,   618,
       0,     0,   977,   977,   407,   407,     0,     2,   649,     0,
     975,   646,   976,     0,   642,     0,     2,   645,   648,   170,
     169,     0,     2,   407,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   742,   773,   777,   816,   742,
     830,   835,   765,   822,     0,     0,   393,   881,     0,   727,
       0,   407,   728,   386,     0,     0,     0,     0,   384,     2,
     729,     0,   369,   706,     0,   724,     2,   730,     0,     0,
       0,     0,   543,   603,   408,     3,     3,   607,   606,   800,
       0,     0,   407,   349,     0,   487,     3,    91,     3,   407,
       0,     3,     2,   662,   407,   407,   656,   655,   656,   464,
     462,   581,   939,   407,   944,   408,   407,   930,   407,     0,
       0,     0,   908,     0,   742,   978,   894,   895,   623,   891,
     892,   906,   934,   938,   936,   493,   528,     0,   942,   947,
     584,   976,     0,   154,     0,   583,     0,   975,   688,   686,
       0,     0,   749,    57,   712,     0,     2,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   407,     0,   112,   111,
       0,   108,   107,    27,     0,    28,     0,     0,     0,     0,
       3,    57,    42,    43,    50,     0,    49,    61,     0,    58,
      59,    62,    45,     0,    44,    48,     0,     0,    41,   114,
     116,   117,   118,   120,   121,   123,   124,   128,   129,   126,
     127,   131,   132,   134,   136,   138,   140,   142,     0,     0,
     358,     0,     0,    29,     3,   640,   148,   407,     0,     0,
       0,   870,   871,   867,   868,   681,   680,     2,   790,   792,
     794,     2,   810,   812,   407,   407,   886,   885,     2,     0,
       0,     0,     0,     0,   742,   894,   842,   859,     2,   837,
     845,   620,   840,   841,   621,     2,   852,   862,   855,   856,
       0,     3,   977,   377,     2,   970,     2,   611,   612,   590,
       3,     3,     3,     3,   634,     0,   146,     0,     3,     3,
       0,   643,     0,   637,     0,   725,     0,   407,     3,   381,
     383,     0,   742,   774,   778,   742,   831,   836,     2,   766,
     769,   771,     2,   823,   826,   828,   740,     0,   882,     3,
     732,     3,   477,   476,   479,   478,     2,   707,   733,     2,
     731,     0,   707,   734,   543,   543,   543,   407,     0,     0,
     625,     0,   352,     0,     0,   407,     0,     2,     0,     0,
       0,     0,     0,   175,     0,   282,   283,     0,     0,   321,
     320,     0,   150,   150,   327,   504,   510,   189,     0,   176,
       0,   199,   177,   178,   407,   193,   179,   180,   181,   182,
       0,   183,   184,   288,     0,   185,   186,   187,     0,   188,
     195,   487,   407,     0,   197,     0,   346,     0,     0,     0,
       3,     0,   707,   695,   696,     0,     3,   691,     3,     3,
       0,   407,   672,   672,   941,   946,     2,    91,   407,     3,
     502,     3,   408,     3,   742,   901,   904,   407,     3,   890,
     896,     0,   742,   742,     0,   587,   572,   588,   975,     0,
       2,   746,   748,     0,    86,   407,     0,    90,    88,   407,
       0,   102,     0,     0,     0,   106,   110,   109,   174,     0,
       0,     0,   640,    99,   167,     0,     0,    75,     0,    75,
      75,     0,    63,    65,    40,     0,     0,    38,     0,    39,
     145,     0,     0,     0,     0,   975,     3,   742,   877,   880,
     872,   407,   407,     3,     3,     0,   742,   848,   851,   742,
       0,   742,   742,   843,   860,   407,   407,   971,     0,   613,
     407,   407,     0,     0,     0,     0,   366,     3,     0,     0,
       0,     0,   639,   644,     3,   172,   171,     3,     0,     0,
       2,   767,   770,   772,     2,   824,   827,   829,   407,   407,
     634,   742,     0,     0,     0,   707,   735,     0,   407,   407,
     407,   407,   407,   407,   526,   554,     3,     3,   555,   487,
     544,     0,     0,   782,     2,     0,   350,    57,     0,     0,
     273,   274,   196,   198,     0,     0,     0,   407,   407,   269,
       0,   267,     0,     0,     0,   640,     0,     0,     0,     0,
       0,   151,     0,     0,   328,     0,     0,     3,   202,     0,
     194,     0,   264,     0,     0,     2,     0,   487,   742,     0,
     347,   888,   887,     0,     2,     0,   698,     2,   693,     0,
     694,     0,   676,   657,   661,   659,   407,     0,     0,     0,
       3,     0,     2,   897,   899,   900,     0,     0,    91,     0,
       3,   975,   577,     0,   587,   585,     0,   575,   689,   407,
     751,     0,     0,     0,    32,     0,   103,   105,   104,   101,
     100,   640,   975,     0,    56,    72,     0,    66,    73,    74,
      51,     0,     0,     0,    60,    47,     0,   144,   357,    30,
       0,     0,     2,   873,   875,   876,     3,     3,     0,     0,
     742,     2,   844,   846,   847,     2,   861,   863,     0,   838,
     853,     3,     3,   972,     3,   598,   597,   601,   974,     2,
       2,   973,     0,     3,   739,   650,   651,     0,     0,   742,
     388,   407,   407,     3,     3,   394,   741,     0,   832,   716,
       0,   718,   526,   526,   526,   561,   531,     0,   567,   555,
       0,   407,   518,   553,   549,     0,     0,     0,     0,   556,
     558,   742,   569,   569,     0,   550,   565,   407,   353,     0,
       0,    58,   277,   278,   275,   276,     0,     0,     2,   211,
       0,     0,   213,   361,   212,   487,   407,     2,     0,   175,
     243,     0,   238,   175,   270,   268,     0,   262,   975,   271,
       0,     0,     0,   309,   310,   311,   312,     0,   302,     0,
     303,   279,     0,   280,     0,     0,   407,   204,   192,   266,
     265,     0,   300,   319,     2,   351,   742,   407,   714,   677,
     407,     2,     2,   949,   950,   951,     0,   902,   407,     3,
       3,     0,   910,     0,     0,     0,     0,   586,   574,     3,
      89,     0,    31,   407,     0,   975,     0,     0,    76,     0,
      64,     0,    70,     0,    68,    37,   149,   878,   407,     0,
       0,   783,   801,   407,   407,     0,     0,     0,   407,   407,
     653,     0,   374,   376,     3,     3,     0,     0,     0,   720,
     522,   524,   520,     0,   917,     0,   562,   922,   564,   914,
     742,   742,   548,   568,   552,     0,   551,     0,     0,     0,
     571,     0,   742,   545,   559,   570,   560,   566,   605,   609,
     608,     0,     2,     0,     0,   229,     2,   214,   487,   235,
     244,     0,   259,   260,   261,   258,   247,     0,     2,   407,
       0,   263,     0,     0,     2,   286,   313,     0,   304,     2,
       0,     0,     0,     0,   291,     0,   287,   190,   375,   692,
       0,     0,   952,     3,     0,     0,   909,   911,   576,     0,
     975,     2,    35,    33,    34,     0,    54,   168,    67,     0,
       0,     3,   784,   802,     3,     3,   849,   864,   378,     2,
     595,     3,   594,   652,     0,     0,   775,   833,   883,     0,
       0,     0,   918,   919,   742,   547,   915,   916,   546,   527,
       0,     0,   203,   285,     0,     0,     2,   222,     2,   205,
       0,     0,   230,   175,   252,     0,   248,     0,   245,   236,
     239,   175,     0,     2,   207,   284,     2,   407,   281,     0,
       0,   329,     2,   289,     0,    57,     0,   301,   697,   699,
       0,   912,   913,   975,     0,   690,    55,    71,    69,     0,
       0,     0,   407,     0,   776,   834,   742,   925,   927,   920,
       0,   557,   215,   218,     0,   217,   221,   407,   224,   223,
     232,     0,     2,   240,   249,   260,   258,     0,   175,     0,
       2,   242,   272,   407,   407,     3,   314,   408,   318,     0,
     322,     0,     0,     0,   330,   331,   209,   292,     0,     2,
       0,     2,     2,   903,     0,   579,   879,   850,   865,   599,
       2,   921,   923,   924,   563,     0,     0,   220,   225,   407,
     342,     2,   233,   231,   254,   253,   250,   241,   246,   237,
     225,     3,   307,     0,   917,   315,   316,   317,   329,     0,
       0,     0,   329,     0,     2,   290,   297,     0,   294,   296,
     578,   407,   216,   219,     2,     3,   226,   343,   234,     0,
       0,     0,     3,   307,     0,     0,   918,     0,     0,     0,
     323,     0,   332,   210,     0,   287,     0,     3,   200,   227,
       0,     2,   256,   257,   255,   251,     0,     0,   308,     0,
     335,     0,   333,     0,   335,   293,   295,     2,     0,     0,
     201,   206,   228,   208,     0,   305,   336,     0,     0,   324,
       0,   298,     2,   926,   306,     0,     0,     0,     0,   299,
     337,   338,     0,   334,   325,     0,     0,   326,   339
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1622,  5592,  5666, -1622,    -1,   348,  1194,  -100, -1622,  1502,
   -1622,   276, -1622,  -673,   555,   648,  -697, -1051, -1622,   121,
    6399,  1516, -1622,   127, -1622,  1223,   498,   660,   663,   471,
     659,  1189,  1185,  1190,  1191,  1183, -1622,    91,  -151,  8003,
     774, -1622,  -392, -1622, -1622,  -653,  2671, -1034,   839, -1622,
     112, -1622,   767,   -39, -1622, -1622, -1622,   340,    31, -1622,
   -1572, -1418,   217,    22, -1622, -1622, -1622,   225,   139, -1622,
   -1622, -1622, -1622,   -20, -1598,   122, -1622, -1622,   -16, -1622,
   -1622, -1622,    -3,   365,   368,    84, -1622, -1622, -1622, -1622,
    -930, -1622,    27,   -22, -1622,    92, -1622,  -162, -1622, -1622,
   -1622,   786,  -844,  -921, -1202, -1622,   105,    41,    25,  4747,
    -910,  -900, -1622,  -263, -1622,    61,  -106,   421,  -142,  -234,
    3213,  5824,  -633, -1622,    38,   220,   753,   261, -1622,  1885,
   -1622,    68,  3529, -1622, -1622, -1622,   110, -1622, -1622,   501,
     135,  4097,  2414,   -38,  1692,  -303, -1622, -1622, -1622, -1622,
   -1622,  -286,  4674,  4756, -1622,  -357,   124, -1622,   452,   182,
   -1622,   129,   639, -1622,   446,  -134, -1622, -1622, -1622,  4995,
    -599, -1090,  -671,  -637,  -248,  1087, -1622, -1180,  -153,   -32,
    1291,   807,  7386,  -126,  -470,  -244,  -191,  -451,  1181, -1622,
    1486,   376,  1100,  1386, -1622, -1622, -1622, -1622,   210,  -169,
      42,  -851, -1622,   241, -1622, -1622,   565,   384, -1622, -1622,
   -1622,  1970,  -748,  -499,  -903,   -33, -1622, -1622, -1622, -1622,
   -1622,   202,  -814,  -104, -1610,  -157,  6691,   -72,  6432, -1622,
    1060, -1622,  2299,  -210,  -189,  -178,  -171,     5,   -71,   -68,
     -50,   538,   -28,   -14,    -9,  -167,   -62,  -149,  -124,  -121,
    -677,  -702,  -662,  -635,  -672,   -87,  -634, -1622, -1622,  -688,
    1255,  1257,  1259,  1518,  7793,  -556,  -571,  -565,  -563,  -583,
   -1622, -1595, -1621, -1583, -1577,  -591,  -138,  -259, -1622, -1622,
     -31,    57,   -66, -1622,  7115,   817,  -560,  -531
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,  1128,   213,   381,   382,    80,    81,   383,   358,   384,
    1413,  1414,   385,   948,   949,   950,  1231,  1232,  1233,  1425,
     407,   387,   388,   389,   663,   664,   390,   391,   392,   393,
     394,   395,   396,   397,   398,   399,   400,   409,  1047,   665,
    1352,   724,   207,   726,   403,   791,  1129,  1130,  1131,  1132,
    1133,  1134,  1135,  1939,  1136,  1137,  1357,  1530,  1823,  1824,
    1767,  1768,  1769,  1915,  1916,  1138,  1541,  1542,  1687,  1139,
    1140,  1141,  1142,  1143,  1144,  1365,  1705,  1862,  1797,  1145,
    1146,  1558,  1925,  1559,  1560,  1845,  1147,  1148,  1149,  1355,
    1853,  1854,  1855,  1967,  1982,  1878,  1879,   284,   285,   852,
     853,  1101,    83,    84,    85,    86,    87,  1533,   440,    90,
      91,    92,    93,    94,   221,   557,   442,   411,   443,    97,
     294,    99,   100,   101,   323,   324,   104,   105,   166,   106,
     870,   325,   152,   109,   241,   110,   153,   250,   327,   328,
     329,   154,   404,   115,   116,   331,   117,   548,   841,   839,
     840,  1502,   332,   333,   120,   121,  1097,  1320,  1508,  1509,
    1645,  1646,  1321,  1497,  1664,  1510,   122,   630,  1595,   334,
     628,   905,  1040,   448,   449,   845,   846,   450,   451,   847,
     336,   552,  1153,   413,   414,   208,   468,   469,   470,   471,
     472,   312,  1172,   313,   868,   866,   581,   314,   352,   315,
     316,   415,   124,   172,   173,   125,  1166,  1167,  1168,  1169,
       2,  1086,  1087,   829,  1304,   126,   304,   252,   262,   531,
     127,   211,   128,   222,  1049,   832,   498,   164,   129,   641,
     642,   643,   130,   224,   225,   226,   227,   299,   132,   133,
     134,   197,   136,   137,   138,   230,   300,   232,   233,   234,
     759,   760,   761,   762,   763,   235,   765,   766,   767,   729,
     730,   731,   732,   499,   139,   605,   606,   607,   608,   609,
     610,  1648,  1649,  1650,  1651,   595,   453,   339,   340,   341,
     416,   199,   141,   142,   143,   343,   783,   611
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      79,   181,   183,    79,   353,   184,   131,   515,  1150,   335,
     492,   957,   528,   780,  1170,   725,   474,   668,   939,   613,
     556,   893,  1749,   185,   338,   176,    89,   402,   879,   149,
     484,   932,   907,   231,   880,   836,   881,   190,  1154,   102,
     992,   485,    88,   825,   827,   186,   297,   887,   486,  1753,
    1278,   198,   487,  1010,    79,    79,   321,    79,   140,   187,
    1750,   140,    95,   131,   188,   496,  1751,   504,   902,   107,
     488,  -700,    79,   289,   888,   862,   349,   357,   623,  1346,
    1016,    79,   626,    89,  1279,  1017,   492,   196,  1092,    79,
    1465,  1466,   526,  1011,    79,   489,   102,    79,   490,    88,
     228,    79,   536,   253,  1043,  1405,   484,   263,  1796,   148,
     419,   111,   292,   420,  1162,   140,   198,   485,   144,    95,
    1012,  1013,  1058,   246,   486,   435,   107,   257,   487,  1764,
    1765,   421,   512,   563,   565,  1532,   112,  1041,  1041,    79,
    -701,   613,    79,    57,    79,  1881,   488,   482,   183,    79,
     131,   184,    57,   422,  1825,    79,  1041,  1819,   493,   140,
     452,   155,    79,   909,  1875,  1764,  1765,   423,   111,   185,
      89,   489,   424,   706,   490,  1020,  1315,  1427,   590,    79,
      79,  1027,   596,   102,   879,   196,    88,  1316,  1160,  1307,
     880,   186,   881,   112,    79,   621,  1829,  1317,  1279,   624,
     456,   887,   140,    57,   465,   187,    95,   256,  1562,    79,
     188,  1766,   260,   107,   521,   707,   586,   274,    79,    79,
    1050,   103,  1041,   568,   183,   196,   497,   184,  1021,  1328,
    1329,   162,  1024,   204,   493,    79,   543,   904,   356,   274,
    1203,  1037,  1038,   280,    79,   185,  1882,  1783,   669,    57,
     196,  1896,   816,  1688,    79,   111,  1236,    79,  1689,  1375,
     903,  1332,   204,   532,    79,   996,  1825,    19,  1226,   401,
     209,   521,  1749,   495,    79,    79,  1851,    79,   103,   784,
     112,   812,   278,   529,    57,  1243,   497,  1563,   613,   903,
    1150,  1333,  -344,   798,    79,    79,   590,  1217,   856,  1753,
     596,   196,    79,  1250,   799,  -345,  1010,  1198,    79,    79,
    1750,   800,   613,    79,  1597,   801,  1751,  1564,   248,   613,
    1154,   559,   279,   477,   861,   955,   564,   279,   505,   874,
    1190,  1819,   497,   802,  1089,    62,    63,  1796,  1263,  1522,
    -724,  1305,  1829,  1264,  1330,    79,  1011,  1199,  1277,   156,
      79,   107,   146,    79,   640,   764,    57,   585,   803,   249,
     812,   804,  -344,   571,  1532,   103,   899,   497,   751,  1829,
     269,   835,   161,  1012,  1255,  -345,   578,   198,  1239,   798,
    1610,  1612,  1614,    75,  1565,  1235,  1287,  1315,  1315,  1315,
     799,   564,    57,   111,   191,   585,   560,   800,  1316,  1316,
    1316,   801,  1490,   170,   170,   579,   580,   823,  1317,  1317,
    1317,   419,   249,   828,   420,   602,    57,    79,   112,   802,
    1041,   179,    96,   456,   190,   150,  1020,   175,  -628,   813,
     503,   204,   421,   508,   338,   742,  1957,   879,   170,   497,
      79,    79,   205,   880,   803,   881,   872,   804,  1465,  1466,
    1432,  1118,    79,    79,   422,   525,   249,    57,   206,  1190,
    1914,    79,   177,   465,    57,   535,   321,    57,   423,    57,
     892,   997,  1914,   424,   596,   497,   179,   202,   530,    96,
      57,    79,  1433,   898,  -858,   279,   667,   452,   170,   650,
      79,   170,   178,  1531,  1543,  1018,   456,   419,  1941,   600,
     420,  1269,  1524,   194,   170,  1681,   537,  1790,   813,  1690,
      79,   347,  1791,   179,   686,   687,    79,   549,   421,   249,
     915,    57,   917,   918,  1901,   919,  1042,  1042,   921,  1902,
     179,   923,   924,   925,  1245,   686,  1025,   209,  -523,   135,
     600,   613,   248,  1068,    -3,  1042,  1072,   497,  1290,   249,
     497,   934,   497,   216,    79,   249,    79,   452,   170,  1294,
     287,  1839,    57,   497,   236,   686,    96,    79,   194,    79,
      57,  1325,   613,  -401,   157,   456,    57,   158,   159,  1514,
     160,  1221,    79,   249,   934,  1436,    79,   893,  1222,  1325,
    1326,  1051,  1401,   278,  1921,   425,   135,  1653,  1515,   559,
    1392,  1394,  1514,   170,   600,   210,  1859,  -724,  1575,   981,
     274,  1042,   433,   170,  1175,   819,  1654,  1031,    79,    57,
     822,  1656,  1364,   546,   170,  1662,   551,   876,    57,   107,
      79,   582,  1869,   229,   276,   583,   254,   830,  1406,  1860,
     264,  1442,   934,   278,  1663,   497,  1754,   837,   293,  1451,
    1657,   170,  1777,   497,  1520,  1455,   708,   170,   170,   600,
     709,  1691,   170,   517,   248,  1755,   520,  1327,   279,   260,
     764,   111,   543,  1424,    79,    79,  1457,    79,   861,   452,
    1235,    79,   356,   135,    79,  1440,    13,    14,    15,    16,
      17,  1060,  -344,   446,   170,  1551,   112,  1662,   934,   170,
     351,  1383,   170,  1953,    96,   249,   820,  1870,  1954,    79,
    1076,   497,   310,  1197,   734,  1077,  1758,  1762,   735,   850,
     452,   279,  1531,   520,  1682,  1683,  1684,   632,   831,  1609,
     634,  1174,   354,   457,   834,   355,   934,   746,   838,  1833,
     426,   497,   452,   452,    57,    72,  1685,  1841,    13,    14,
      15,    16,    17,   849,    79,  1686,    79,   850,   667,   427,
     278,   452,   425,   667,   497,   727,   667,   455,    79,   497,
    1607,  1422,   696,   697,   428,    79,    77,    78,   248,   249,
     927,   465,   785,   786,    79,   667,   787,   429,   338,   170,
     876,   928,   929,    79,    79,    79,   533,   189,    63,   249,
    1276,   170,   170,   904,  1887,  -405,    57,   908,    72,  1042,
    1193,   583,   425,    79,   497,   959,   698,   699,   689,   249,
     321,   593,   542,    63,   616,   690,   691,   452,   599,   194,
     237,   238,   600,   239,   430,   530,   431,   240,   593,    77,
      78,  1594,   593,   910,   505,  1543,   808,   583,   497,    79,
      79,   465,   249,   911,   933,   750,   266,   912,   934,   401,
     267,  1283,  1606,   270,   459,   272,   460,  -406,   157,  1001,
    1046,   158,   159,   497,   160,  -402,   249,    89,   571,   613,
     425,   473,   497,   249,    13,    14,    15,    16,    17,  1164,
    1152,  1055,   475,    88,  1548,  1056,    79,   278,   505,   201,
      79,   497,   497,   146,  1018,    79,   425,   478,   600,   140,
     495,   640,   479,    95,  1729,   480,  1730,    72,   481,  1300,
     107,   140,    13,    14,    15,    16,    17,  1262,   764,  1082,
     494,   170,  1080,   934,    72,   871,  1084,   599,   513,   593,
     934,   600,    57,  1088,    79,   514,  1090,   571,    77,   601,
    1093,   497,    79,  1396,  1643,  1496,   457,   861,   497,  1234,
     524,   602,   111,  1235,   201,    77,    78,   896,  1927,  1163,
    1605,   677,  1931,   678,   679,   680,   150,   692,   693,   170,
      57,    79,    96,  1880,   465,   694,   695,   112,  1692,    13,
      14,    15,    16,    17,  1382,   209,   417,  1720,   735,  1880,
     553,   266,   681,   353,   353,   682,   683,    79,   700,   701,
     684,   685,   904,    79,    79,  1553,  1554,  1555,  1556,  1557,
     446,   534,  1464,   942,   943,   452,   946,  1410,  1602,   457,
     954,  1235,  1603,   958,  1673,  1917,   892,  1091,   934,    72,
     596,   574,  1693,   588,    79,  1725,   934,    57,  1694,  1211,
     620,  1695,  1056,  1759,  1215,   934,  1485,   735,   983,   599,
    -857,  1376,   522,   600,   248,  1223,  1322,   338,  -573,  1240,
      77,   601,   103,   446,  1534,   530,   631,   266,   267,   633,
     617,  1831,   272,  1415,  1905,   934,   191,   671,  1235,   593,
     446,  -403,  1955,  1978,  1985,   248,   934,  1975,  1986,   321,
      13,    14,    15,    16,    17,   249,   465,   644,  1512,    79,
      79,    79,   645,   593,   588,   671,   249,   170,  1467,   522,
    1803,   648,  1858,   649,   170,   653,   593,  1473,  1474,   688,
      89,   936,   937,   465,  1033,  1034,   249,   598,   861,    79,
    1035,  1036,  1059,  1152,  1061,  1046,    88,    79,  1224,  1056,
      79,    79,  1237,  1238,    79,   671,   253,   263,    57,    89,
    1804,   675,   140,   676,    79,   702,    95,   967,   968,   969,
     970,   703,  1152,   107,   705,    88,   246,   257,   704,   243,
       6,     7,     8,     9,    10,    11,    12,   960,   961,   962,
      79,   140,  1682,  1835,  1684,    95,   934,  1241,  1100,   170,
     170,   736,   107,  1306,   934,    79,  1640,  1641,  1642,   140,
    -147,  -147,   446,   710,  1836,   111,  1331,   737,  1308,  1309,
    1310,   465,   738,  -176,   739,   201,   740,    79,  1035,  1374,
    1430,  1431,   650,  1350,  1435,  1431,  1439,  1431,   452,   452,
     112,  1794,  1795,  1864,   111,  1192,  1007,  1423,   338,   170,
    1475,  1423,   741,   446,   170,   598,  1513,   256,   432,    79,
    1007,  1487,   260,  -112,  -112,  -112,  -112,  -112,  -112,   112,
    1615,  1056,    -3,    96,  1727,  1056,   768,  1322,  1322,  1322,
     321,  1498,  1322,   266,  -404,  1165,  1728,  1431,   782,  1512,
    -111,  -111,  -111,  -111,  -111,  -111,   -16,   492,  1738,  1739,
    1748,   934,   805,  1534,   -17,   686,   243,     6,     7,     8,
       9,    10,    11,    12,  1807,  1431,   484,    79,  1808,  1431,
     792,    79,  1876,  1877,    79,   103,  -392,   485,  1764,  1765,
     417,   417,  1437,   781,   486,   806,   149,   807,   487,  1416,
    1417,  1418,  1975,  1976,   465,   815,  1419,  1420,   809,  -392,
    1428,  1429,   963,   964,   103,   810,   488,   965,   966,   971,
     972,   811,    89,    89,   465,   265,    79,   817,   140,   401,
     401,   248,   532,   286,   593,  1536,  1536,   616,  1665,  1665,
    1741,   489,  1384,  1385,   490,    13,    14,    15,    16,    17,
     931,   833,   529,  -521,   140,   140,  -519,   842,   851,   466,
     863,   865,   530,   869,   882,   107,   107,   884,   602,   901,
     913,   914,   249,   906,   935,   938,   148,   941,   980,   985,
     465,  1006,  1007,  1014,  1467,    79,  1062,   446,  1053,   894,
      79,    79,    79,  1658,  1164,  1063,  1064,  1513,  1065,   338,
    1523,  1525,  -704,   249,  1066,   493,  1067,   111,   111,  1083,
     417,   812,  1085,  1094,  1095,   170,  1096,  -604,   170,   170,
     170,  1155,  1156,  1171,  1184,  1702,   140,   627,  1185,   798,
    1186,   321,   112,   112,  1196,  1200,  1467,  1415,  1573,  1201,
     799,  1282,   170,  1204,  1206,  1669,  1207,   800,   170,  1208,
    1210,   801,  1209,  1212,  1213,  1214,   551,  1219,    79,  1220,
    1242,  1247,  1248,   170,    79,  1249,    79,  1256,  1257,   802,
    1258,  1259,  1267,    79,  1163,  -592,  -591,    82,  1318,  1301,
     147,  1323,  -705,  1354,  1324,  1289,    96,   465,  1334,  1337,
    1338,  1347,  1348,  1349,   803,  1356,   465,   804,  1364,   170,
    1358,  -627,   934,  1371,   613,    13,    14,    15,    16,    17,
    1216,  1512,   417,  1368,  1411,    96,  1372,   103,   103,    61,
     246,   257,   168,   169,    64,    65,    66,    67,    68,    69,
      70,  1407,  1370,   465,    82,  1378,  1408,  1380,  1421,  1423,
     249,  1438,  1450,  1463,  1468,  1469,  1476,   530,  1471,   180,
    1470,    89,  1479,   140,  1431,    79,   452,   452,    82,   813,
    1489,  1789,  1488,  1491,  1536,  1164,  1501,  1799,  1503,  1327,
      79,   220,    79,  1822,   245,  1527,  1504,  1544,    82,  1545,
    1547,  1549,  1561,   140,  1566,  1568,  1578,  1569,   249,  1571,
    1570,  1579,  1576,  1583,   107,  1652,  1584,   140,  1587,  1581,
    1582,   256,    61,  1585,  1592,  1846,   260,    64,    65,    66,
      67,    68,    69,    70,   944,   147,   778,    79,   466,  1467,
      79,    82,  1596,   147,  1600,  1598,   296,   302,  1601,  1696,
     170,   465,  1608,   170,  1616,   465,   111,  1604,   320,   593,
    1617,  1621,   417,  1622,   492,  1163,   425,   465,  1630,   254,
     264,  1852,  1475,   465,   945,   408,   180,   180,  1704,  1513,
    1632,   112,  1846,   529,   484,  1639,   446,   147,   438,    79,
      79,   245,  1506,   170,    89,   485,   812,  1235,    79,  1672,
    1674,  1676,   486,  1699,  1912,  1822,   487,  1536,   210,  1318,
    1318,  1318,   150,  1495,  1499,   220,   220,  1701,  1706,  1712,
    1716,  1717,  1895,  1718,   488,  1726,   140,  1719,   507,  1929,
    1732,  1733,   296,  1736,  1737,   248,  1848,   107,    96,    96,
      79,    82,  1743,  1746,  1747,  1773,  1781,   465,  1782,   489,
    1786,  1118,   490,  1792,   245,  1788,  1793,  1801,  1802,  -593,
     452,  1805,  1806,  1814,  1830,  1826,   103,  1815,  1816,  1817,
    1818,   465,  1832,   497,  -504,  1840,   249,  1849,  1852,   111,
    1838,  1842,  1852,  1852,   302,  1863,  1865,  1866,  1867,  1850,
     302,   296,   296,  1848,  1868,  1739,  1884,  1977,   147,  1885,
    1898,   568,   183,  1900,   112,   184,  1899,  1903,  1951,  1904,
    1165,   465,   493,  1907,  1910,  1919,   320,   603,   612,   465,
    1920,   848,  1924,   185,  1928,    79,  1935,    79,  1936,  1949,
    1966,  1930,    89,   320,  1966,   401,  1950,   320,   465,  1952,
     465,   465,   170,  1961,   813,  1536,  1963,  1964,    89,  1968,
    1969,  1972,  1973,  1984,  1983,  1980,   170,  1987,   674,  1723,
     465,  1536,  1521,  1434,   140,   170,   930,   446,   974,   977,
     408,   973,    79,    79,   975,   107,   976,  1353,   530,   196,
     140,  1360,  1962,   465,    89,   533,  1703,  1913,  1081,   103,
    1784,   107,  1922,   465,  1780,  1837,  1958,  1536,  1861,  1956,
    1947,  1857,   170,  1697,   408,    79,  1698,   728,  1891,   733,
    1932,   456,  1970,   167,   180,  1890,   140,   111,   465,   249,
     465,  1369,   523,  1821,   170,   744,  1336,   107,   747,  1874,
     147,  1655,  1500,   111,   438,  1366,   465,   788,   757,  1666,
     612,   465,   112,  1052,  1709,  1183,   401,   401,  1173,   867,
       3,   465,  1202,     0,  1599,    79,   466,     0,   112,   778,
       0,     0,   988,     0,   989,    79,   990,    96,   417,   111,
       0,   401,     0,     0,     0,   894,     0,     0,   220,     0,
       0,  1165,     0,     0,     0,   507,     0,   220,     0,     0,
       0,     0,     0,     0,   112,  1948,     0,     0,     0,     0,
     170,     0,     0,     0,   170,     0,     0,   296,     0,   408,
     408,     0,     0,   296,     0,   320,   170,     0,     0,     0,
       0,     0,   170,     0,    61,     0,     0,   103,     0,    64,
      65,    66,    67,    68,    69,    70,  1194,     0,   170,   401,
       0,  1965,     0,   103,     0,     0,     0,   170,     0,     0,
       0,     0,     0,   296,  1246,     0,  1974,    13,    14,    15,
      16,    17,     0,     0,   296,     0,   296,     0,   320,     0,
       0,  1253,  1254,    74,     0,     0,   777,     0,     0,   103,
       0,     0,     0,     0,   320,   438,     0,   612,   249,     0,
      96,     0,   848,     0,     0,   603,   170,     0,     0,   603,
     243,     6,     7,     8,     9,    10,    11,    12,   320,     0,
       0,     0,     0,     0,     0,    57,     0,     0,   612,  1230,
     170,   320,     0,     0,     0,     0,     0,  1230,    61,   147,
       0,   168,   169,    64,    65,    66,    67,    68,    69,    70,
       0,     0,   408,     0,   147,   147,     0,   408,     0,   848,
     408,     0,     0,   147,   147,   147,  1230,     0,     0,   466,
     170,     0,     0,     0,     0,     0,     0,     0,   170,     0,
      18,     0,    61,     0,    72,     0,  1897,    64,    65,    66,
      67,    68,    69,    70,  1572,     0,     0,   170,     0,   170,
     170,     0,     0,     0,   727,     0,     0,     0,   497,     0,
       0,     0,     0,     0,     0,    77,    78,   438,     0,   170,
       0,     0,    51,    52,    53,    54,     0,     0,     0,  1230,
    1260,    74,     0,   728,   728,   733,   733,     0,    96,     0,
       0,   408,   170,   236,     0,   999,     0,     0,  1002,     0,
       0,     0,   170,     0,    96,     0,     0,     0,   438,     0,
       0,   757,     0,   757,    61,     0,     0,     0,   848,    64,
      65,    66,    67,    68,    69,    70,   952,   170,   593,   170,
     320,   320,     0,     0,     0,   848,   848,     0,     0,     0,
      96,     0,     0,     0,     0,   170,     0,     0,     0,   320,
     170,   296,     0,    13,    14,    15,    16,    17,   417,   507,
     170,     0,     0,  1070,  1981,     0,   953,  1074,     0,     0,
     296,     0,     0,     0,  1988,     0,     0,     0,  1446,  1447,
      13,    14,    15,    16,    17,   593,     0,     0,     0,     0,
       0,    61,  1461,  1462,   217,   218,    64,    65,    66,    67,
      68,    69,    70,     0,     0,     0,     0,  1675,   408,     0,
       0,    57,   182,     0,     0,   320,  1679,     0,     0,    72,
     147,   408,     0,     0,     0,  1483,  1484,     0,     0,   320,
       0,  1178,     0,     0,   223,     0,     0,     0,    57,  1505,
      74,     0,   603,     0,     0,   635,  1506,     0,     0,     0,
      77,    78,     0,  1707,     0,   114,   466,     0,   114,     0,
       0,     0,  1230,     0,     0,     0,     0,     0,     0,    61,
      72,     0,   217,   218,    64,    65,    66,    67,    68,    69,
      70,     0,   438,     0,     0,     0,     0,     0,     0,   298,
    1643,     0,     0,     0,   497,     0,     0,    72,     0,     0,
      61,    77,    78,   168,   169,    64,    65,    66,    67,    68,
      69,    70,   114,     0,     0,     0,     0,   755,    74,   636,
       0,   600,     0,     0,     0,     0,     0,     0,    77,   756,
       0,     0,     0,     0,   637,     0,   114,   638,   639,    64,
      65,    66,    67,    68,    69,    70,     0,   728,   455,   733,
       0,  1763,   251,     0,     0,  1772,   114,     0,   483,   223,
       0,     0,     0,     0,   757,     0,     0,  1779,     0,     0,
       0,   757,     0,  1785,     0,   298,     0,     0,     0,   466,
       0,    61,   848,   848,   168,   169,    64,    65,    66,    67,
      68,    69,    70,   114,     0,     0,   848,   848,     0,   114,
       0,   114,     0,     0,     0,   251,     0,     0,  1634,  1635,
       0,     0,     0,   320,     0,   317,   114,   348,     0,     0,
    1292,     0,     0,  1296,     0,     0,     0,     0,     0,   848,
     848,     0,     0,   412,   569,   298,   305,   306,   307,   308,
       0,     0,     0,     0,     0,   114,   412,  1828,     0,   251,
       0,     0,     0,   147,  1339,   466,     0,     0,     0,     0,
    1230,   408,     0,     0,     0,  1230,  1230,  1230,     0,     0,
       0,  1856,     0,     0,    61,     0,     0,   168,   169,    64,
      65,    66,    67,    68,    69,    70,     0,     0,     0,     0,
     408,     0,     0,     0,     0,    61,   114,     0,     0,   114,
      64,    65,    66,    67,    68,    69,    70,   245,    82,     0,
       0,  1883,   251,     0,     0,  1713,     0,     0,     0,  1889,
       0,   296,     0,     0,     0,     0,   309,   147,     0,   547,
       0,     0,     0,     0,   438,     0,     0,   114,  1906,     0,
    1908,  1909,   251,     0,   310,  1731,     0,     0,   251,     0,
    1734,  1735,     0,     0,     0,     0,   114,     0,    18,     0,
    1918,   438,     0,     0,     0,   147,     0,     0,     0,     0,
       0,     0,     0,     0,   114,     0,   251,   114,     0,     0,
       0,   758,     0,  1933,    13,    14,    15,    16,    17,     0,
       0,   114,     0,  1938,     0,   114,    47,    48,    49,    50,
      51,    52,    53,    54,     0,  1444,     0,     0,     0,     0,
       0,     0,   848,   848,  1453,     0,     0,     0,  1960,     0,
    1938,   797,     0,     0,     0,     0,   320,   320,   412,     0,
     223,     0,     0,     0,     0,     0,  1971,     0,     0,     0,
       0,  1960,    57,     0,     0,  1230,     0,  1230,  1670,     0,
     298,  1979,     0,     0,     0,     0,   298,     0,     0,     0,
       0,     0,   412,     0,   147,   147,   147,   147,   147,   147,
       0,     0,     0,    61,  1507,   302,   217,   218,    64,    65,
      66,    67,    68,    69,    70,     0,     0,     0,   114,     0,
       0,     0,   412,   408,   408,     0,   298,     0,   251,     0,
       0,    72,     0,     0,     0,     0,     0,   860,     0,   298,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   848,
       0,  1893,    74,   245,     0,   497,     0,     0,     0,     0,
       0,     0,    77,    78,     0,     0,     0,     0,     0,     0,
       0,     0,   438,     0,     0,     0,  1361,     0,     0,   848,
       0,     0,     0,     0,   848,   848,     0,    57,     0,     0,
       0,     0,     0,     0,     0,   147,     0,   412,   412,     0,
       0,    61,   251,   114,   168,   169,    64,    65,    66,    67,
      68,    69,    70,     0,     0,     0,     0,     0,    61,     0,
       0,   217,   218,    64,    65,    66,    67,    68,    69,    70,
       0,     0,     0,     0,   114,     0,     0,     0,     0,   114,
       0,     0,   251,   114,    61,   114,    72,   344,   345,    64,
      65,    66,    67,    68,    69,    70,   114,     0,     0,     0,
       0,    13,    14,    15,    16,    17,  1893,    74,  1937,     0,
     497,   348,   114,   412,  1362,   251,     0,    77,    78,     0,
       0,  1644,     0,  1647,     0,  1507,     0,   408,     0,     0,
       0,  1507,     0,  1507,     0,    75,   114,     0,     0,   251,
     346,     0,     0,   547,     0,     0,   251,     0,     0,   114,
       0,   900,     0,     0,     0,   666,     0,   114,     0,    57,
       0,   302,   147,     0,  1009,     0,   758,     0,     0,     0,
     412,     0,   114,   114,     0,   412,     0,     0,   412,     0,
       0,   114,   114,   114,     0,     0,     0,     0,     0,     0,
      61,     0,   408,   217,   218,    64,    65,    66,    67,    68,
      69,    70,     0,   320,   298,    61,   147,     0,   168,   169,
      64,    65,    66,    67,    68,    69,    70,     0,    72,     0,
       0,     0,     0,   298,     0,     0,    57,     0,     0,   147,
       0,     0,     0,     0,     0,   412,     0,    61,   219,    74,
     168,   169,    64,    65,    66,    67,    68,    69,    70,    77,
      78,     0,     0,     0,   320,   320,     0,    61,     0,   412,
     217,   218,    64,    65,    66,    67,    68,    69,    70,  1644,
    1644,  1647,  1647,     0,     0,     0,   412,     0,  1341,     0,
       0,     0,     0,     0,  1507,     0,     0,  1507,     0,     0,
       0,     0,     0,     0,   824,   826,  1343,     0,   114,   114,
       0,     0,     0,     0,   302,  1260,    74,     0,     0,     0,
       0,     0,   848,    57,     0,   408,     0,   114,     0,     0,
       0,     0,     0,     0,    98,    61,     0,   151,   189,    63,
      64,    65,    66,    67,    68,    69,    70,   296,     0,     0,
       0,     0,     0,     0,    61,   114,     0,   217,   218,    64,
      65,    66,    67,    68,    69,    70,    61,     0,     0,   168,
     169,    64,    65,    66,    67,    68,    69,    70,   251,     0,
       0,     0,    72,     0,    74,     0,   412,   777,  1644,   251,
    1647,    98,     0,   114,     0,     0,     0,  1507,   114,   412,
       0,     0,   219,    74,     0,     0,     0,   114,     0,  1180,
     412,     0,   114,    77,    78,   195,     0,     0,     0,     0,
       0,     0,   576,   147,     0,     0,     0,  1009,     0,     0,
       0,     0,     0,  1261,   758,   258,     0,   666,     0,     0,
       0,     0,   666,     0,     0,   666,     0,     0,   320,     0,
       0,     0,     0,     0,  1872,     0,  1644,     0,  1647,     0,
     412,     0,     0,   147,   666,     0,     0,     0,    57,     0,
       0,     0,   288,     0,     0,     0,     0,     0,    98,   147,
     147,     0,  1894,   302,  1647,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   322,     0,     0,   979,    61,
       0,     0,   217,   218,    64,    65,    66,    67,    68,    69,
      70,     0,     0,   418,     0,   147,     0,     0,     0,     0,
       0,   114,     0,     0,   288,   444,     0,    72,     0,  1894,
    1894,  1647,  1647,     0,     0,     0,     0,     0,   114,   114,
       0,     0,     0,     0,     0,     0,     0,   295,    74,     0,
       0,     0,     0,   491,     0,     0,     0,     0,    77,    78,
       0,     0,  1894,     0,  1647,     0,     0,     0,     0,   511,
       0,     0,     0,     0,   516,   518,     0,     0,   195,     0,
       0,     0,     0,     0,   298,     0,    57,     0,     0,     0,
       0,   114,     0,    13,    14,    15,    16,    17,     0,     0,
     538,     0,     0,   540,    61,   541,     0,   544,   545,    64,
      65,    66,    67,    68,    69,    70,   558,    61,     0,     0,
     217,   218,    64,    65,    66,    67,    68,    69,    70,   570,
       0,   114,     0,     0,     0,     0,     0,     0,     0,   412,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
     108,    57,     0,   591,     0,    75,   615,     0,     0,     0,
      13,    14,    15,    16,    17,  1505,    74,     0,   412,     0,
     622,     0,     0,     0,   622,     0,    77,    78,     0,  1458,
       0,     0,    61,     0,     0,   251,   114,    64,    65,    66,
      67,    68,    69,    70,   712,   713,   714,   715,   716,   717,
     718,   719,   720,   721,   722,   114,     0,   108,   204,     0,
      72,     0,   412,     0,     0,     0,  1180,     0,    57,     0,
       0,    13,    14,    15,    16,    17,     0,     0,  1404,     0,
      73,    74,     0,     0,     0,   723,     0,  1511,     0,   412,
       0,    77,    78,   114,     0,     0,     0,     0,     0,    61,
       0,   259,   217,   218,    64,    65,    66,    67,    68,    69,
      70,     0,     0,     0,     0,     0,     0,   288,     0,     0,
       0,   591,     0,     0,     0,     0,     0,    72,     0,    57,
       0,     0,     0,     0,     0,   114,   114,     0,     0,     0,
       0,     0,     0,     0,   108,     0,     0,   295,    74,   114,
     114,     0,     0,     0,   114,   114,     0,     0,    77,    78,
      61,   326,     0,   217,   218,    64,    65,    66,    67,    68,
      69,    70,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   114,   114,     0,     0,     0,     0,    72,     0,
       0,   445,   114,   114,   114,   114,   114,   114,     0,     0,
       0,     0,   444,   251,     0,     0,     0,    61,  1505,    74,
     217,   218,    64,    65,    66,    67,    68,    69,    70,    77,
      78,   412,   412,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   844,     0,     0,     0,     0,   518,     0,
       0,     0,   855,     0,   558,     0,     0,     0,     0,     0,
       0,   251,     0,     0,     0,   322,     0,     0,     0,     0,
       0,     0,     0,  1191,     0,    57,   539,     0,  1511,     0,
     412,   622,   875,     0,  1659,     0,  1511,     0,     0,     0,
       0,     0,   108,     0,     0,     0,   886,     0,     0,     0,
       0,     0,     0,   114,     0,   591,    61,     0,     0,     0,
     895,    64,    65,    66,    67,    68,    69,    70,   622,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   592,
       0,     0,   259,    61,    72,     0,   217,   218,    64,    65,
      66,    67,    68,    69,    70,     0,   592,     0,     0,     0,
     592,     0,     0,     0,    73,    74,     0,     0,     0,     0,
       0,    72,     0,     0,     0,    77,    78,     0,     0,     0,
       0,     0,     0,     0,     0,   114,   114,     0,     0,     0,
       0,   755,    74,     0,     0,   600,     0,     0,     0,     0,
       0,     0,    77,   756,     0,   412,     0,     0,     0,     0,
       0,     0,     0,     0,   444,   602,     0,    61,     0,     0,
       0,   114,    64,    65,    66,    67,    68,    69,    70,  1227,
       0,   991,     0,  1228,     0,  1229,     0,     0,     0,   251,
     114,     0,     0,     0,     0,     0,     0,  1760,     0,     0,
    1511,     0,     0,     0,     0,   875,     0,   592,     0,     0,
    1015,     0,     0,     0,     0,     0,    74,     0,     0,  1426,
     412,     0,     0,     0,     0,     0,     0,   444,   444,     0,
       0,   114,     0,     0,   114,     0,     0,     0,     0,     0,
       0,     0,   114,     0,     0,  1526,   444,     0,  1529,  1540,
     298,     0,     0,     0,  1546,     0,     0,   114,  1550,    61,
    1552,     0,   542,    63,    64,    65,    66,    67,    68,    69,
      70,     0,   114,     0,   844,     0,     0,   114,   114,     0,
       0,     0,   114,   114,     0,    61,     0,     0,   445,     0,
      64,    65,    66,    67,    68,    69,    70,  1227,     0,     0,
    1511,  1228,     0,  1229,     0,  1151,     0,     0,     0,     0,
       0,   982,   444,     0,     0,     0,     0,   151,     0,   326,
       0,     0,     0,     0,     0,     0,   622,     0,   259,  1182,
     108,   844,   251,     0,    74,     0,  1188,  1611,   113,     0,
       0,   445,    61,   412,     0,   217,   218,    64,    65,    66,
      67,    68,    69,    70,     0,     0,    61,   592,   445,   217,
     218,    64,    65,    66,    67,    68,    69,    70,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,   322,
       0,   592,     0,     0,    72,   298,     0,     0,     0,     0,
    1505,    74,     0,     0,   592,   113,     0,  1506,  1638,     0,
       0,    77,    78,     0,  1893,    74,     0,     0,   497,     0,
       0,     0,     0,    61,     0,    77,    78,     0,    64,    65,
      66,    67,    68,    69,    70,  1227,     0,     0,     0,  1228,
    1671,  1229,   569,   298,     0,     0,     0,     0,     0,   261,
     844,   114,  1677,     0,     0,     0,     0,     0,     0,  1680,
       0,     0,     0,     0,     0,     0,     0,   844,   844,     0,
       0,     0,    74,     0,    61,   298,   114,   217,   218,    64,
      65,    66,    67,    68,    69,    70,     0,  1529,    61,     0,
     445,   114,   113,    64,    65,    66,    67,    68,    69,    70,
       0,     0,    72,     0,     0,     0,     0,   114,   114,   330,
       0,   251,     0,     0,     0,     0,    72,     0,     0,     0,
     444,     0,   219,    74,     0,     0,     0,     0,     0,     0,
       0,   445,     0,    77,    78,     0,  1008,    74,     0,   447,
     600,     0,     0,   114,     0,     0,     0,    77,    78,     0,
       0,     0,     0,   326,   326,     0,     0,     0,     0,     0,
    1319,     0,     0,     0,     0,     0,     0,     0,  1151,     0,
       0,     0,   326,    61,     0,   114,   217,   218,    64,    65,
      66,    67,    68,    69,    70,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1771,     0,  1151,     0,     0,
     326,    72,     0,     0,     0,     0,     0,  1776,  1778,     0,
    1540,     0,     0,     0,     0,  1367,     0,     0,     0,     0,
       0,   295,    74,     0,     0,     0,     0,     0,     0,     0,
     113,   108,    77,    78,     0,     0,     0,     0,   326,     0,
       0,   591,     0,     0,     0,     0,     0,     0,     0,     0,
     516,     0,   592,     0,     0,   259,     0,   326,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   594,   322,   364,
     261,   365,     0,   366,    62,    63,    64,    65,    66,    67,
      68,    69,    70,     0,   594,     0,     0,     0,   594,     0,
       0,     0,    61,     0,     0,     0,  1834,    64,    65,    66,
      67,    68,    69,    70,  1227,   445,     0,     0,  1228,     0,
    1229,     0,     0,     0,   844,   844,     0,     0,     0,   673,
       0,     0,    75,   375,     0,     0,     0,     0,   844,   844,
       0,     0,     0,   444,   444,     0,     0,     0,     0,     0,
       0,    74,    61,     0,  1613,    62,    63,    64,    65,    66,
      67,    68,    69,    70,     0,     0,     0,     0,  1886,     0,
    1888,   844,   844,     0,     0,     0,   326,     0,     0,     0,
       0,  1319,  1319,  1319,   151,     0,     0,     0,     0,     0,
       0,     0,     0,   326,   326,   594,     0,     0,     0,     0,
       0,     0,     0,    75,     0,     0,     0,     0,     0,    61,
    1535,  1535,   168,   169,    64,    65,    66,    67,    68,    69,
      70,    61,     0,     0,   168,   169,    64,    65,    66,    67,
      68,    69,    70,     0,     0,     0,     0,     0,  1934,     0,
       0,     0,     0,     0,     0,     0,   326,     0,     0,     0,
    1942,  1944,  1945,     0,     0,     0,     0,   459,     0,   322,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   447,     0,     0,     0,
       0,     0,   151,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   108,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   330,     0,     0,
       0,     0,     0,     0,     0,     0,   261,     0,   113,     0,
       0,     0,     0,   108,     0,     0,     0,     0,     0,   447,
       0,     0,     0,     0,     0,   118,     0,     0,   118,     0,
       0,   259,     0,     0,     0,   594,   447,     0,     0,     0,
       0,     0,     0,     0,   844,   844,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   592,     0,   594,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1661,     0,   594,     0,     0,     0,     0,     0,     0,     0,
     844,     0,   118,     0,   445,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1678,
       0,     0,     0,     0,     0,     0,   118,   119,     0,     0,
     119,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   118,     0,     0,  1535,
     326,   326,     0,     0,     0,     0,     0,     0,     0,     0,
     322,     0,     0,   151,   326,   326,     0,     0,     0,   326,
     326,   844,     0,     0,     0,     0,     0,     0,   447,     0,
       0,     0,     0,   118,   119,     0,     0,     0,     0,   118,
       0,   118,     0,     0,     0,     0,     0,   326,   326,   193,
       0,   844,     0,     0,     0,     0,   844,   844,   119,     0,
       0,   444,   444,     0,     0,     0,     0,     0,     0,   447,
       0,     0,     0,   118,     0,     0,     0,  1752,   119,     0,
       0,     0,     0,     0,     0,   118,   108,   108,     0,     0,
       0,   330,   330,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     330,     0,     0,     0,   193,   119,     0,     0,     0,     0,
       0,   119,  1535,   119,     0,     0,     0,     0,     0,   193,
       0,     0,     0,     0,     0,   445,   118,     0,   330,   118,
       0,     0,     0,     0,   118,     0,   193,     0,     0,     0,
       0,     0,     0,     0,     0,   119,     0,     0,     0,   441,
       0,     0,     0,     0,     0,     0,     0,   119,     0,   113,
       0,     0,     0,     0,     0,     0,   330,   118,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     594,     0,     0,   261,     0,   330,   118,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   193,     0,     0,     0,   123,     0,   119,   123,
    1847,   119,     0,     0,     0,     0,   119,     0,     0,     0,
     326,   326,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   447,     0,   444,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   119,
    1535,     0,     0,     0,     0,     0,   326,     0,   118,   193,
       0,     0,     0,   123,     0,     0,  1535,  1847,   119,     0,
       0,     0,     0,     0,     0,   259,     0,   193,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   123,     0,     0,
       0,     0,   118,     0,   330,     0,     0,     0,     0,     0,
       0,     0,  1535,     0,     0,   108,     0,   123,     0,     0,
       0,   330,   330,     0,     0,     0,   326,  1926,   118,     0,
       0,     0,     0,     0,     0,     0,     0,   326,     0,     0,
       0,     0,     0,     0,   844,     0,     0,     0,     0,     0,
     119,     0,     0,     0,   123,     0,     0,     0,     0,     0,
     123,     0,   123,     0,     0,     0,     0,   326,     0,     0,
       0,     0,   326,   326,   330,   193,     0,   326,   326,     0,
       0,     0,     0,     0,   119,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   123,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   193,   123,   118,   118,     0,
     119,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   113,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   108,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   118,
       0,   113,     0,   118,     0,   118,     0,   123,     0,     0,
     123,     0,     0,     0,     0,   123,     0,     0,     0,   261,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     193,   193,     0,     0,     0,     0,   441,     0,     0,   119,
     119,     0,     0,     0,     0,   594,     0,     0,   123,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   123,     0,     0,
       0,     0,   447,     0,     0,     0,     0,   118,     0,     0,
       0,   119,     0,     0,     0,   119,   592,   119,     0,   193,
     118,     0,   118,   118,     0,   118,     0,     0,   118,     0,
       0,   118,   118,   118,     0,     0,   441,     0,     0,     0,
       0,   326,     0,     0,     0,     0,     0,     0,   330,   330,
       0,     0,     0,     0,     0,     0,   108,     0,     0,   193,
       0,     0,   330,   330,     0,     0,     0,   330,   330,   123,
       0,     0,   108,   592,     0,     0,     0,     0,     0,     0,
     193,     0,     0,     0,     0,     0,     0,     0,     0,   119,
       0,     0,     0,     0,     0,   330,   330,     0,     0,     0,
       0,     0,   119,   123,   119,   119,     0,   119,   108,   118,
     119,     0,     0,   119,   119,   119,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   123,
       0,     0,     0,     0,   113,   113,     0,     0,     0,     0,
     326,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   441,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   193,   447,     0,     0,     0,     0,     0,     0,
       0,   119,     0,     0,     0,     0,     0,     0,     0,   441,
       0,     0,     0,     0,     0,     0,     0,     0,   123,   123,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   441,   441,     0,     0,     0,   118,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   118,   118,
     441,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     123,     0,     0,     0,   123,     0,   123,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   330,   330,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     1,     0,     0,   145,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   441,     0,   119,     0,
       0,     0,   193,     0,   330,     0,     0,     0,     0,     0,
     119,   119,     0,     0,     0,     0,     0,     0,   123,     0,
       0,     0,     0,   261,     0,     0,     0,     0,     0,     0,
       0,   123,     0,   123,   123,     0,   123,     0,     0,   123,
       0,     0,   123,   123,   123,     0,     0,     0,     0,     0,
       0,     0,     0,   113,     0,     0,     0,     0,   192,     0,
       0,     0,     0,   193,   330,     0,     0,     0,     0,    13,
      14,    15,    16,    17,     0,   330,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,     0,     0,   330,     0,    45,    46,     0,
     330,   330,     0,     0,     0,   330,   330,   283,     0,     0,
     123,     0,     0,     0,     0,     0,     0,    57,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   203,     0,     0,     0,     0,     0,   214,   215,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   118,     0,     0,     0,     0,     0,     0,     0,   118,
       0,     0,     0,     0,     0,     0,   113,     0,     0,     0,
       0,     0,   277,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   441,     0,     0,     0,   118,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   283,     0,     0,     0,     0,   118,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   519,     0,     0,     0,
       0,     0,     0,     0,     0,   118,   283,   123,     0,     0,
       0,     0,     0,   119,     0,     0,   283,     0,     0,   123,
     123,   119,     0,     0,     0,     0,     0,     0,     0,     0,
     550,   554,     0,     0,     0,     0,     0,   561,   562,     0,
       0,     0,     0,   118,   594,     0,     0,     0,     0,     0,
     119,     0,     0,   572,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   119,   330,
       0,   589,     0,     0,     0,     0,     0,     0,   193,     0,
       0,     0,   247,     0,   113,   193,     0,   119,     0,     0,
       0,     0,     0,   268,     0,   271,     0,   273,     0,     0,
     113,   594,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   193,     0,     0,     0,     0,     0,   566,     0,
       0,     0,     0,     0,     0,   119,     0,   672,     0,     0,
       0,     0,     0,     0,     0,   247,   113,   271,   273,     0,
       0,     0,   118,   118,   118,   118,   118,   118,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   711,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   330,     0,
       0,   118,   118,     0,     0,     0,     0,   441,   441,   247,
       0,     0,     0,     0,   749,     0,     0,     0,   752,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   774,     0,     0,
       0,   775,   776,     0,     0,   779,     0,     0,     0,     0,
       0,     0,     0,     0,   119,   119,   119,   119,   119,   119,
     793,   794,   795,   796,     0,     0,     0,     0,     0,     0,
       0,     0,   247,   118,   271,   273,     0,     0,     0,   818,
       0,     0,   123,   119,   119,     0,     0,   821,     0,     0,
     123,     0,     0,   753,     0,   754,     0,     0,     0,     0,
       0,     0,   247,     0,   770,   771,     0,     0,   247,     0,
       0,     0,     0,     0,     0,   283,     0,     0,     0,   123,
       0,     0,     0,   193,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   247,   123,     0,     0,
       0,     0,   618,     0,   273,     0,   859,     0,     0,     0,
       0,     0,     0,   550,     0,   119,   123,     0,   864,     0,
       0,     0,     0,     0,     0,   118,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   878,   883,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   123,     0,     0,     0,     0,     0,
     118,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   854,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     118,     0,   247,     0,     0,     0,     0,     0,   193,     0,
       0,     0,   926,     0,   118,     0,     0,   119,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   247,     0,
     618,   273,     0,     0,     0,     0,     0,   118,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   119,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   123,   123,   123,   123,   123,   123,     0,
       0,     0,     0,     0,     0,   247,     0,     0,     0,   987,
       0,     0,   119,     0,   193,     0,     0,     0,     0,     0,
       0,     0,   123,   123,  1004,   247,   119,     0,  1005,     0,
     247,     0,   247,     0,     0,     0,     0,   878,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   119,
       0,     0,   247,   118,   247,   247,     0,     0,     0,  1045,
       0,     0,     0,     0,     0,   441,   441,     0,  1054,     0,
       0,     0,   247,     0,  1057,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   247,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   123,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   247,     0,   618,   273,     0,
       0,     1,     0,     0,     0,     0,     0,     0,     1,     0,
       0,     0,     0,     0,  1030,     0,     0,     0,     0,   247,
     618,     0,     0,     0,     0,   119,   247,     0,     0,     0,
       0,     0,     0,     0,     1,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   165,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   123,     0,     0,     0,
       0,   118,     0,     0,     0,   165,     0,     0,  1205,     0,
       0,  1098,  1099,     0,     0,     0,     0,   118,     0,     0,
       0,     0,  1157,  1158,  1159,     0,     0,  1161,     0,     0,
       0,   123,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   165,     0,   118,     0,     0,     0,     0,     0,   441,
       0,   123,     0,     0,   165,     0,   165,     0,     0,     0,
       0,     0,     0,     0,     0,   123,     0,   386,     0,     0,
       0,     0,     0,   119,     0,     0,     0,     0,     0,  1251,
       0,     0,     0,  1252,     0,     0,   350,     0,   123,   119,
     878,     0,     0,     0,     0,     0,  1225,     0,     0,     0,
    1265,     0,     0,   350,     0,     0,     0,  1266,     0,     0,
       0,     0,     0,     0,     0,     0,  1270,     0,  1271,     0,
       0,     0,     0,     0,     0,   119,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1244,   165,     0,     0,     0,   165,     0,     0,   165,   165,
    1298,     0,   165,     0,  1299,   165,   165,     0,   247,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   145,   247,
       0,     1,     0,     0,   123,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1268,     0,   247,
       0,     0,     0,     0,     0,     0,  1272,  1273,  1274,  1275,
     247,     0,     0,     0,  1280,  1281,     0,     0,     0,   247,
       0,     0,     0,     0,  1288,     0,     0,   165,     0,     0,
     165,     0,     0,     0,     0,     0,     0,     0,   163,     0,
       0,     0,     0,     0,     0,  1302,     0,  1303,     0,     0,
     165,     0,     0,     0,     0,     0,     0,     0,   647,     0,
       0,   386,   652,     0,     0,   165,     0,     0,  1386,     0,
       0,   655,   656,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   386,   386,     0,     0,
       0,     0,  1409,     0,     0,     0,     0,     0,     0,     0,
    1359,     0,     0,     0,     0,     0,     0,   386,     0,     0,
     275,     0,     0,     0,     0,   247,     0,     0,     0,     0,
       0,     0,   123,   281,     0,   282,  1373,     0,     0,     0,
       0,     0,  1377,     0,  1379,  1381,     0,   386,   123,   247,
       0,     0,     0,  1387,     0,  1388,     0,  1389,     0,  1391,
       0,     0,     0,     0,  1399,     0,     0,     0,     0,   165,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   123,     0,     0,     0,     0,     0,
       0,     0,  1481,     0,     0,     0,  1482,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1441,     0,   350,     0,  1517,   501,   502,  1448,
    1449,   506,     0,     0,   509,   510,   165,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1472,     0,     0,     0,     0,     0,     0,
    1477,     0,     0,  1478,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1577,     0,     0,  1580,
       0,     0,     0,     0,     0,   247,     0,     0,     0,     0,
       0,     0,     0,   214,  1588,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     350,     0,     0,     0,     0,     0,   247,     0,     0,   587,
       0,     0,   247,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1567,   619,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1618,     0,     0,     0,     0,   165,
     165,     0,     0,  1623,     0,     0,     0,  1624,     0,     0,
       0,     0,   165,     0,     0,     0,  1586,     0,     0,     0,
       0,  1628,  1629,     0,  1591,     0,  1593,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   386,   386,   386,
     386,   386,   386,   386,   386,   386,   386,   386,   386,   386,
     386,   386,   386,   386,   386,   386,     0,     0,     0,     0,
       0,     0,  1619,  1620,     0,     0,     0,     0,   743,     0,
       0,     0,     0,     0,     0,     0,     0,  1625,  1626,     0,
    1627,     0,     0,     0,     0,     0,     0,     0,     0,  1631,
       0,     0,     0,   247,     0,     0,     0,     0,     0,  1636,
    1637,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   165,   165,     0,     0,   386,     0,   165,
       0,     0,     0,  1710,  1711,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   814,     0,     0,     0,     0,
     165,   247,     0,   165,   165,     0,   165,   200,   165,   165,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   255,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   165,     0,     0,
       0,   165,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1714,  1715,     0,     0,     0,
       0,     0,   200,     0,     0,  1721,   303,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   342,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1787,     0,     0,   200,     0,     0,     0,   889,   890,
    1744,  1745,     0,     0,     0,     0,     0,   454,     0,     0,
     458,   897,     0,  1580,     0,     0,   165,     0,     0,     0,
       0,   386,     0,     0,     0,     0,   386,     0,     0,     0,
       0,  1812,     0,     0,     0,     0,     0,   386,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1827,   247,
     200,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   255,     0,  1843,     0,   386,  1844,  1800,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1809,     0,     0,
    1810,  1811,     0,     0,     0,     0,     0,  1813,     0,   458,
       0,     0,     0,     0,     0,     0,     0,   200,     0,     0,
       0,     0,   993,   994,     0,     0,     0,     0,   998,     0,
       0,     0,     0,     0,     0,   597,     0,   614,     0,     0,
       0,   171,   174,     0,     0,     0,   165,     0,     0,  1019,
       0,     0,  1022,  1023,     0,  1026,     0,  1028,  1029,     0,
       0,     0,  1911,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   212,     0,     0,     0,
       0,     0,     0,     0,     0,   247,     0,     0,     0,   670,
       0,     0,     0,     0,   165,     0,  1069,   165,     0,     0,
    1073,     0,   247,     0,   386,     0,     0,     0,     0,     0,
       0,  1892,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   200,     0,     0,   290,     0,     0,   291,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   311,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   597,     0,     0,     0,  1923,     0,   769,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1189,   247,   386,     0,     0,
       0,  1940,     0,     0,     0,     0,     0,     0,  1946,     0,
       0,     0,     0,     0,     0,     0,   476,     0,     0,     0,
       0,     0,     0,  1959,     0,     0,     0,     0,     0,     0,
       0,   386,   386,   386,     0,     0,   165,     0,   386,   386,
       0,     0,     0,     0,   165,   165,     0,     0,   200,   200,
       0,     0,     0,     0,   454,     0,     0,     0,     0,     0,
     386,   527,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   171,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   171,     0,     0,     0,     0,     0,     0,     0,
       0,   247,     0,     0,     0,     0,     0,   386,   386,   165,
       0,     0,     0,     0,     0,     0,     0,   342,   165,   573,
       0,   165,     0,   165,   165,   575,   577,     0,     0,     0,
     584,     0,     0,     0,   454,  1189,   877,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   597,     0,     0,
       0,     0,   629,   165,     0,     0,     0,   311,     0,     0,
     311,     0,     0,     0,     0,     0,     0,     0,   200,     0,
     247,     0,     0,  1291,     0,     0,  1295,     0,     0,     0,
       0,   670,     0,   670,   670,     0,   670,     0,     0,   670,
       0,     0,   670,   670,   670,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     165,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   454,   212,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   772,
     773,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     200,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   454,     0,     0,
       0,     0,     0,     0,     0,  1393,     0,     0,     0,     0,
       0,     0,   165,  1402,  1403,     0,     0,     0,     0,   454,
     454,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   454,     0,
       0,   165,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   165,  1443,     0,
       0,     0,     0,   165,     0,     0,     0,  1452,     0,     0,
    1456,     0,  1459,  1460,     0,   337,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   311,
     386,     0,     0,     0,   454,     0,     0,     0,     0,     0,
     200,     0,     0,     0,   434,   337,     0,     0,     0,     0,
     769,     0,  1486,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   359,     0,     0,   165,   360,
       0,   361,     0,     0,     0,     0,   500,   629,     0,     0,
       0,     0,     0,   500,     0,     0,     0,     0,   362,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   342,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   363,   364,     0,   365,  1574,
     366,  1774,    63,    64,    65,    66,    67,    68,    69,    70,
     367,   368,   356,     0,   369,   370,   371,     0,   372,   373,
       0,     0,   165,   165,     0,     0,    72,     0,     0,     0,
     350,   500,     0,     0,   165,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   374,     0,     0,    75,
     375,     0,     0,   337,   604,     0,   376,    77,    78,   377,
     378,   379,   380,     0,     0,     0,     0,     0,     0,     0,
    1775,  -175,     0,     0,   625,     0,     0,     0,     0,     0,
       0,  1456,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1032,     0,     0,     0,     0,
       0,     0,  1044,   386,     0,     0,     0,     0,     0,     0,
    1633,     0,   454,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   410,     0,     0,     0,   165,     0,     0,     0,
       0,     0,     0,   386,     0,   439,     0,     0,     0,     0,
       0,     0,     0,     0,   500,     0,     0,     0,   467,     0,
     467,     0,   670,     0,     0,     0,     0,     0,     0,     0,
     500,   745,     0,   500,   748,     0,     0,     0,     0,     0,
       0,   337,     0,     0,     0,   604,     0,  1102,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   165,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   255,  1708,     0,     0,
       0,     0,     0,     0,   386,   386,   500,     0,     0,     0,
     500,     0,     0,     0,     0,     0,   200,  1195,     0,     0,
       0,     0,   629,   597,     0,     0,   567,     0,     0,   386,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   337,     0,     0,     0,     0,     0,     0,     0,
     342,     0,     0,   386,   670,   165,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1756,  1757,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1761,     0,     0,     0,     0,     0,     0,
       0,     0,   500,     0,     0,   337,     0,   386,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   873,   337,     0,     0,   454,   454,     0,     0,     0,
       0,     0,   604,     0,     0,     0,   604,     0,     0,     0,
       0,     0,     0,   891,     0,   337,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   670,   670,   670,     0,   670,   670,     0,
       0,     0,     0,     0,   458,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1820,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   467,     0,     0,
       0,     0,     0,   467,     0,     0,     0,     0,   790,     0,
       0,     0,   255,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1340,  1342,  1344,     0,
       0,   342,     0,     0,   337,     0,     0,  1871,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     500,   500,     0,     0,     0,     0,  1363,     0,     0,     0,
     500,  1000,     0,   500,  1003,     0,     0,     0,     0,     0,
       0,  1102,     0,     0,     0,   337,     0,     0,   604,     0,
     604,   604,     0,     0,     0,     0,     0,   604,     0,     0,
       0,     0,     0,     0,     0,   858,     0,   337,   337,     0,
       0,     0,     0,     0,     0,     0,     0,   629,     0,     0,
       0,     0,     0,     0,     0,     0,   337,     0,     0,     0,
     500,     0,   439,     0,   500,     0,     0,     0,   500,  1071,
       0,     0,   500,  1075,     0,   885,     0,     0,     0,     0,
    1078,     0,     0,     0,     0,     0,   200,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     255,     0,   337,   500,     0,     0,   920,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   790,   940,     0,     0,     0,   604,
       0,     0,     0,     0,   951,     0,   956,   951,     0,     0,
       0,     0,   342,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1516,     0,
       0,  1518,     0,     0,   984,     0,     0,     0,   670,   337,
       0,     0,     0,     0,     0,     0,     0,   986,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   995,     0,
       0,     0,     0,   454,   454,     0,     0,     0,     0,     0,
       0,     0,   439,     0,     0,   984,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1048,     0,   500,   467,     0,     0,     0,     0,
       0,     0,     0,   255,     0,     0,     0,     0,     0,     0,
       0,   604,   604,     0,     0,     0,     0,     0,   604,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1079,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     337,     0,     0,     0,     0,   500,  1293,     0,   500,  1297,
       0,     0,     0,     0,     0,     0,     0,     0,   410,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1179,  1181,
       0,     0,     0,     0,     0,     0,   439,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1667,     0,   670,     0,     0,     0,     0,   467,     0,     0,
       0,     0,     0,     0,  1187,     0,   951,     0,     0,     0,
       0,    13,    14,    15,    16,    17,     0,   454,     0,   984,
       0,     0,     0,     0,     0,     0,     0,  1218,     0,     0,
       0,     0,     0,     0,   951,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   359,     0,   670,
       0,   360,   458,   361,     0,     0,     0,     0,     0,     0,
       0,   337,     0,     0,     0,     0,     0,   604,  1395,    57,
     362,     0,   629,     0,     0,     0,     0,     0,   467,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   337,     0,
       0,     0,     0,     0,     0,     0,     0,   363,   364,     0,
     365,     0,   366,    62,    63,    64,    65,    66,    67,    68,
      69,    70,   367,   368,   356,     0,   369,   370,   371,     0,
     372,   373,     0,     0,     0,     0,     0,     0,    72,     0,
     500,  1445,     0,     0,     0,     0,     0,     0,     0,   500,
    1454,     0,   604,     0,     0,   467,     0,  1284,   374,  1286,
       0,    75,   375,   337,   337,     0,     0,     0,   376,   437,
      78,   377,   378,   379,   380,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1798,     0,     0,     0,
       0,     0,     0,     0,     0,   629,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1351,  1351,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1397,     0,     0,     0,     0,     0,     0,    13,    14,
      15,    16,    17,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   337,
       0,     0,     0,     0,     0,  1390,     0,     0,     0,     0,
       0,  1400,     0,     0,   359,     0,     0,     0,   360,     0,
     361,     0,     0,     0,     0,     0,     0,     0,   439,     0,
       0,     0,     0,     0,     0,     0,    57,   362,     0,     0,
       0,     0,     0,     0,     0,   467,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   951,     0,
       0,   790,     0,     0,   363,   364,     0,   365,     0,   366,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   367,
     368,   356,     0,   369,   370,   371,     0,   372,   373,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   500,     0,
       0,     0,  1480,     0,     0,   374,     0,     0,    75,   375,
       0,     0,     0,     0,   500,   376,  1398,    78,   377,   378,
     379,   380,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     951,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   467,     0,
       0,   790,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     337,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   940,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1589,
    1590,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   337,   337,     0,   467,     0,   790,     0,     0,     0,
       0,     0,     0,     0,     0,  1311,   500,   500,     0,     0,
       0,  1312,     0,     0,    13,    14,    15,    16,    17,    18,
       0,    19,   500,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,     0,     0,
       0,     0,    45,    46,     0,     0,     0,    47,    48,    49,
      50,    51,    52,    53,    54,     0,     0,     0,     0,     0,
       0,     0,    57,  1313,   410,     0,     0,     0,     0,  1660,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    60,     0,     0,     0,    62,    63,     0,     0,
       0,     0,     0,     0,     0,   500,     0,     0,     0,     0,
       0,     0,     0,   500,     0,     0,     0,     0,     0,     0,
       0,    72,     0,     0,   359,  1700,     0,     0,   360,     0,
     361,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1314,     0,     0,     0,    75,   916,     0,   362,     0,     0,
       0,     0,    77,    78,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1722,   337,     0,  1724,     0,   500,
    1873,     0,     0,   500,   363,   364,     0,   461,     0,   366,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   367,
     368,   356,     0,   369,   370,   371,     0,   372,   373,   500,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   374,    74,     0,   462,   463,
       0,     0,     0,   464,     0,   376,    77,    78,   377,   378,
     379,   380,     0,     0,     0,     0,   500,   500,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     4,   243,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,  1103,   500,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,     0,     0,   359,
       0,    45,    46,   360,     0,   361,    47,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,    56,     0,
    1104,    57,  1105,    -2,     0,  1106,     0,     0,  1107,  1108,
    1109,  1110,  1111,  1112,  1113,  1114,  1115,  1116,  1117,  1118,
    -287,  1119,  1120,  1121,  1122,  1123,     0,  1124,   951,   363,
     364,    60,   461,     0,   366,  1125,  1126,    64,    65,    66,
      67,    68,    69,    70,   367,   368,   356,  1127,   369,   370,
     371,     0,   372,   373,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    -3,
     374,     0,     0,    75,   406,     0,     0,     0,   279,     0,
     376,    77,    78,   377,   378,   379,   380,     0,     0,     0,
       0,     0,     0,     0,     0,  -175,     4,   243,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
    1103,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,     0,
       0,   359,     0,    45,    46,   360,     0,   361,    47,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
      56,     0,  1104,    57,  1105,    -2,     0,  1106,     0,     0,
    1107,  1108,  1109,  1110,  1111,  1112,  1113,  1114,  1115,  1116,
    1117,  1118,  -287,  1119,  1120,  1121,  1122,  1123,     0,  1124,
       0,   363,   364,    60,   461,     0,   366,  1125,  1126,    64,
      65,    66,    67,    68,    69,    70,   367,   368,   356,  1127,
     369,   370,   371,     0,   372,   373,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   374,     0,     0,    75,   406,     0,     0,     0,
     279,     0,   376,    77,    78,   377,   378,   379,   380,     0,
       0,     0,     0,     0,     0,     0,     0,  -175,     4,   243,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,     0,     0,   359,     0,    45,    46,   360,     0,   361,
      47,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,    56,     0,     0,    57,   362,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   363,   364,    60,   365,     0,   366,    62,
      63,    64,    65,    66,    67,    68,    69,    70,   367,   368,
     356,     0,   369,   370,   371,     0,   372,   373,     0,     0,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   374,     0,  1537,    75,   406,     0,
       0,     0,     0,     0,   376,    77,    78,   377,   378,   379,
     380,     0,     0,     0,     0,     0,     0,     0,  1538,  1539,
       4,   243,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,     0,     0,   359,     0,    45,    46,   360,
       0,   361,    47,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,    56,     0,     0,    57,   362,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   363,   364,    60,   365,     0,
     366,    62,    63,    64,    65,    66,    67,    68,    69,    70,
     367,   368,   356,     0,   369,   370,   371,     0,   372,   373,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   374,     0,     0,    75,
     406,     0,     0,     0,     0,     0,   376,    77,    78,   377,
     378,   379,   380,     0,     0,     0,     0,     0,     0,     0,
    1538,  1539,     4,   243,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,     0,     0,   359,     0,    45,
      46,   360,     0,   361,    47,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,    56,     0,     0,    57,
     362,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   363,   364,    60,
     365,     0,   366,    62,    63,    64,    65,    66,    67,    68,
      69,    70,   367,   368,   356,     0,   369,   370,   371,     0,
     372,   373,     0,     0,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   374,     0,
    1528,    75,   406,     0,     0,     0,     0,     0,   376,    77,
      78,   377,   378,   379,   380,     4,   243,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,     0,     0,
     359,     0,    45,    46,   360,     0,   361,    47,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,    56,
       0,     0,    57,   362,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     363,   364,    60,   365,     0,   366,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   367,   368,   356,     0,   369,
     370,   371,     0,   372,   373,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   374,     0,     0,    75,   406,     0,     0,     0,     0,
       0,   376,    77,    78,   377,   378,   379,   380,   243,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
       0,     0,   359,     0,    45,    46,   360,     0,   361,   318,
      48,    49,    50,    51,    52,    53,    54,     0,     0,     0,
       0,     0,     0,     0,    57,   362,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   363,   364,     0,   365,     0,   366,    62,    63,
      64,    65,    66,    67,    68,    69,    70,   367,   368,   356,
       0,   369,   370,   371,     0,   372,   373,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   374,     0,     0,    75,   436,     0,     0,
       0,     0,     0,   376,   437,    78,   377,   378,   379,   380,
     243,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,   359,     0,    45,    46,   360,     0,
     361,   318,    48,    49,    50,    51,    52,    53,    54,     0,
       0,     0,     0,     0,     0,     0,    57,   362,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   363,   364,     0,   365,     0,   366,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   367,
     368,   356,     0,   369,   370,   371,     0,   372,   373,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   374,     0,     0,    75,  1176,
       0,     0,     0,     0,     0,   376,  1177,    78,   377,   378,
     379,   380,   243,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,   359,     0,    45,    46,
     360,     0,   361,   318,    48,    49,    50,    51,    52,    53,
      54,     0,     0,     0,     0,     0,     0,     0,    57,   362,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   363,   364,     0,   365,
       0,   366,    62,    63,    64,    65,    66,    67,    68,    69,
      70,   367,   368,   356,     0,   369,   370,   371,     0,   372,
     373,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   374,     0,     0,
      75,   375,     0,     0,     0,     0,     0,   376,    77,    78,
     377,   378,   379,   380,   243,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,   359,     0,
      45,    46,   360,     0,   361,   318,    48,    49,    50,    51,
      52,    53,    54,     0,     0,     0,     0,     0,     0,     0,
      57,   362,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   363,   364,
       0,   365,     0,   366,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   367,   368,   356,     0,   369,   370,   371,
       0,   372,   373,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   374,
       0,     0,    75,   436,     0,     0,     0,     0,     0,   376,
      77,    78,   377,   378,   379,   380,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,     0,
       0,     0,     0,    45,    46,     0,     0,     0,    47,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
      56,     0,     0,    57,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    59,
       0,     0,     0,    60,    61,     0,     0,    62,    63,    64,
      65,    66,    67,    68,    69,    70,     0,     0,     0,    71,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    73,    74,     0,    75,    76,     0,     0,     0,
       0,     0,     0,    77,    78,   242,   243,     6,     7,     8,
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
       0,    73,    74,     0,    75,   244,     0,     0,     0,  -715,
       0,     0,    77,    78,     4,   243,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,     0,     0,     0,
       0,    45,    46,     0,     0,     0,    47,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,    56,     0,
       0,    57,     0,     0,     0,     0,  -340,  -340,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    60,     0,     0,     0,    62,    63,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -340,
       0,     0,     0,    75,    76,     0,     0,     0,     0,     0,
       0,    77,    78,     4,   243,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,     0,     0,
      45,    46,     0,     0,     0,    47,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,    56,     0,     0,
      57,     0,     0,     0,     0,  -341,  -341,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      60,     0,     0,     0,    62,    63,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -341,     0,
       0,     0,    75,    76,     0,     0,     0,     0,     0,     0,
      77,    78,   242,   243,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,     0,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,  -408,  -408,     0,  -408,    45,
      46,     0,  -408,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    57,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      61,     0,     0,    62,    63,    64,    65,    66,    67,    68,
      69,    70,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    73,    74,
       0,    75,   244,     0,     0,  1311,     0,     0,     0,    77,
      78,  1312,     0,     0,    13,    14,    15,    16,    17,    18,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,     0,     0,
       0,     0,    45,    46,     0,     0,     0,    47,    48,    49,
      50,    51,    52,    53,    54,     0,     0,     0,     0,     0,
       0,     0,    57,  1313,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    60,     0,     0,     0,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1492,     0,     0,     0,    75,   916,     0,     0,  1311,     0,
       0,     0,    77,    78,  1312,     0,     0,    13,    14,    15,
      16,    17,    18,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,     0,     0,     0,     0,    45,    46,     0,     0,     0,
      47,    48,    49,    50,    51,    52,    53,    54,     0,     0,
       0,     0,     0,     0,     0,    57,  1313,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    60,     0,     0,     0,    62,
      63,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1493,     0,     0,     0,    75,   916,     0,
       0,  1311,     0,     0,     0,    77,    78,  1312,     0,     0,
      13,    14,    15,    16,    17,    18,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,     0,     0,    45,    46,
       0,     0,     0,    47,    48,    49,    50,    51,    52,    53,
      54,     0,     0,     0,     0,     0,     0,     0,    57,  1313,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    60,     0,
       0,     0,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1494,     0,     0,     0,
      75,   916,     0,     0,     0,     0,     0,     0,    77,    78,
     242,   243,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,     0,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,  -408,  -408,     0,  -408,    45,    46,     0,
    -408,     0,     0,     0,     0,     0,     0,     0,     0,    13,
      14,    15,    16,    17,     0,     0,    19,    57,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,  -407,  -407,     0,  -407,    45,    46,     0,
    -407,    62,    63,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    57,     0,     0,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   242,     0,     0,     0,     0,     0,    75,
     244,     0,    13,    14,    15,    16,    17,    77,    78,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,  -408,  -408,     0,  -408,
      45,    46,     0,  -408,     0,     0,     0,     0,     0,     0,
       0,     0,    13,    14,    15,    16,    17,     0,     0,    19,
      57,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,  -408,  -408,     0,  -408,
      45,    46,     0,  -408,    62,    63,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      57,     0,     0,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    75,   301,     0,     0,     0,     0,     0,     0,
      77,    78,   243,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,     0,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,  -408,  -408,     0,  -408,    45,    46,
       0,  -408,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    57,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    61,
       0,     0,    62,    63,    64,    65,    66,    67,    68,    69,
      70,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    73,    74,     0,
      75,   244,     0,     0,     0,  -719,     0,     0,    77,    78,
     243,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,     0,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,  -408,  -408,     0,  -408,    45,    46,     0,  -408,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    57,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    61,     0,     0,
      62,    63,    64,    65,    66,    67,    68,    69,    70,     0,
       0,     0,     0,     0,     0,     0,  1335,     0,     0,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    73,    74,   359,    75,   244,
       0,   360,     0,   361,     0,     0,    77,    78,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1104,     0,
     362,     0,     0,  1106,  1764,  1765,  1107,  1108,  1109,  1110,
    1111,  1112,  1113,  1114,  1115,  1116,  1117,  1118,  -287,  1119,
    1120,  1121,  1122,  1123,     0,  1124,     0,   363,   364,     0,
     461,     0,   366,  1125,  1126,    64,    65,    66,    67,    68,
      69,    70,   367,   368,   356,  1127,   369,   370,   371,     0,
     372,   373,     0,     0,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,  1335,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   374,     0,
       0,    75,   375,     0,     0,     0,   279,     0,   376,    77,
      78,   377,   378,   379,   380,   359,     0,     0,     0,   360,
       0,   361,     0,  -175,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1104,     0,   362,    -2,
       0,  1106,     0,     0,  1107,  1108,  1109,  1110,  1111,  1112,
    1113,  1114,  1115,  1116,  1117,  1118,  -287,  1119,  1120,  1121,
    1122,  1123,     0,  1124,     0,   363,   364,     0,   461,     0,
     366,  1125,  1126,    64,    65,    66,    67,    68,    69,    70,
     367,   368,   356,  1127,   369,   370,   371,     0,   372,   373,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,  1335,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   374,     0,     0,    75,
     375,     0,     0,     0,   279,     0,   376,    77,    78,   377,
     378,   379,   380,   359,     0,     0,     0,   360,     0,   361,
       0,  -175,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1104,     0,   362,     0,     0,  1106,
       0,     0,  1107,  1108,  1109,  1110,  1111,  1112,  1113,  1114,
    1115,  1116,  1117,  1118,  -287,  1119,  1120,  1121,  1122,  1123,
       0,  1124,     0,   363,   364,     0,   461,     0,   366,  1125,
    1126,    64,    65,    66,    67,    68,    69,    70,   367,   368,
     356,  1127,   369,   370,   371,     0,   372,   373,     0,     0,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   374,     0,     0,    75,   375,     0,
       0,     0,   279,     0,   376,    77,    78,   377,   378,   379,
     380,     0,     0,     0,     0,     0,     0,     0,     0,  -175,
     243,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,     0,     0,    45,    46,     0,     0,
       0,   318,    48,    49,    50,    51,    52,    53,    54,     0,
      13,    14,    15,    16,    17,    18,    57,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,     0,     0,    45,    46,
      62,    63,     0,    47,    48,    49,    50,    51,    52,    53,
      54,     0,     0,     0,     0,     0,     0,     0,    57,     0,
       0,     0,     0,     0,     0,    72,     0,  1039,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -589,    75,   319,
       0,     0,    62,    63,     0,     0,    77,    78,   243,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      75,     0,     0,     0,    45,    46,     0,     0,     0,   318,
      48,    49,    50,    51,    52,    53,    54,     0,    13,    14,
      15,    16,    17,    18,    57,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,     0,     0,    45,    46,    62,    63,
       0,   318,    48,    49,    50,    51,    52,    53,    54,     0,
       0,     0,     0,     0,     0,     0,    57,     0,     0,     0,
       0,     0,     0,    72,     0,  1740,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    75,   319,     0,     0,
      62,    63,     0,     0,    77,    78,   243,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    75,     0,
       0,     0,    45,    46,     0,     0,     0,   318,    48,    49,
      50,    51,    52,    53,    54,     0,     0,     0,     0,     0,
       0,     0,    57,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    72,     0,  1742,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    75,   319,     0,     0,     0,     0,
       0,     0,    77,    78,   243,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,     0,     0,
      45,    46,     0,     0,     0,   318,    48,    49,    50,    51,
      52,    53,    54,     0,     0,     0,     0,     0,     0,     0,
      57,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    62,    63,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    75,   319,     0,     0,     0,     0,     0,     0,
      77,    78,   243,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,     0,     0,    45,    46,
       0,     0,     0,   318,    48,    49,    50,    51,    52,    53,
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
      43,    44,  -408,  -408,     0,  -408,    45,    46,     0,  -408,
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
      15,    16,    17,    18,   657,    19,   658,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,   359,     0,    45,    46,   360,     0,
     361,    47,    48,    49,    50,    51,    52,    53,    54,     0,
       0,     0,     0,     0,     0,     0,    57,   362,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   659,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   363,   364,     0,   365,     0,   366,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   367,
     368,   356,     0,   369,   370,   371,     0,   372,   373,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   374,     0,     0,    75,   660,
       0,     0,     0,   279,     0,   376,    77,    78,   661,   662,
     379,   380,    13,    14,    15,    16,    17,    18,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,   359,     0,
      45,    46,   360,     0,   361,    47,    48,    49,    50,    51,
      52,    53,    54,     0,     0,     0,     0,     0,     0,     0,
      57,   362,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   363,   364,
       0,   365,     0,   366,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   367,   368,   356,     0,   369,   370,   371,
       0,   372,   373,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   374,
       0,   405,    75,   406,     0,     0,     0,     0,     0,   376,
      77,    78,   377,   378,   379,   380,    13,    14,    15,    16,
      17,    18,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
       0,     0,   359,     0,    45,    46,   360,     0,   361,    47,
      48,    49,    50,    51,    52,    53,    54,     0,     0,     0,
       0,     0,     0,     0,    57,   362,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   363,   364,     0,   365,     0,   366,    62,    63,
      64,    65,    66,    67,    68,    69,    70,   367,   368,   356,
       0,   369,   370,   371,     0,   372,   373,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   374,     0,     0,    75,   660,     0,     0,
       0,   279,     0,   376,    77,    78,   377,   378,   379,   380,
      13,    14,    15,    16,    17,    18,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,   359,     0,    45,    46,
     360,     0,   361,    47,    48,    49,    50,    51,    52,    53,
      54,     0,     0,     0,     0,     0,     0,     0,    57,   362,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   363,   364,     0,   365,
       0,   366,    62,    63,    64,    65,    66,    67,    68,    69,
      70,   367,   368,   356,     0,   369,   370,   371,     0,   372,
     373,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   374,     0,     0,
      75,   406,     0,     0,     0,     0,     0,   376,    77,    78,
     377,   378,   379,   380,    13,    14,    15,    16,    17,    18,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,     0,     0,
     359,     0,    45,    46,   360,     0,   361,   318,    48,    49,
      50,    51,    52,    53,    54,     0,     0,     0,     0,     0,
       0,     0,    57,   362,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     363,   364,     0,   365,     0,   366,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   367,   368,   356,     0,   369,
     370,   371,     0,   372,   373,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   374,     0,     0,    75,   436,     0,     0,     0,     0,
       0,   376,    77,    78,   377,   378,   379,   380,    13,    14,
      15,    16,    17,    18,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,   359,     0,    45,    46,   360,     0,
     361,   318,    48,    49,    50,    51,    52,    53,    54,     0,
       0,     0,     0,     0,     0,     0,    57,   362,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   363,   364,     0,   365,     0,   366,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   367,
     368,   356,     0,   369,   370,   371,     0,   372,   373,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   374,     0,     0,    75,   375,
       0,     0,     0,     0,     0,   376,    77,    78,   377,   378,
     379,   380,    13,    14,    15,    16,    17,    18,     0,    19,
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
      74,     0,    75,    76,     0,     0,     0,  -717,     0,     0,
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
       0,     0,    73,    74,     0,    75,   301,     0,     0,     0,
       0,     0,     0,    77,    78,   555,   243,     6,     7,     8,
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
       0,     0,  1412,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   922,    75,   916,     0,     0,
      62,    63,     0,     0,    77,    78,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    75,   916,
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
      45,    46,    62,    63,     0,   318,    48,    49,    50,    51,
      52,    53,    54,     0,     0,     0,     0,     0,     0,     0,
      57,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      75,   432,     0,     0,    62,    63,     0,     0,    77,    78,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    75,   319,     0,     0,     0,     0,     0,     0,
      77,    78,    13,    14,    15,    16,    17,    18,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,     0,     0,
      45,    46,     0,     0,     0,   318,    48,    49,    50,    51,
      52,    53,    54,     0,    13,    14,    15,    16,    17,    18,
      57,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,     0,     0,
       0,     0,    45,    46,    62,    63,     0,   318,    48,    49,
      50,    51,    52,    53,    54,     0,     0,     0,     0,     0,
       0,     0,    57,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    75,   286,     0,     0,    62,    63,     0,     0,
      77,    78,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    75,   432,     0,     0,     0,     0,
       0,     0,    77,    78,   242,   243,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,     0,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,  -408,  -408,     0,
    -408,    45,    46,     0,  -408,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    13,    14,    15,    16,    17,
      18,    57,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,     0,
       0,     0,     0,    45,    46,    62,    63,     0,   318,    48,
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
       0,     0,     0,     0,     0,     0,     0,    75,   916,     0,
       0,     0,     0,     0,     0,    77,    78,    13,    14,    15,
      16,    17,    18,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,     0,     0,     0,     0,    45,    46,     0,     0,     0,
     318,    48,    49,    50,    51,    52,    53,    54,     0,    13,
      14,    15,    16,    17,    18,    57,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,     0,     0,     0,     0,    45,    46,    62,
      63,     0,   318,    48,    49,    50,    51,    52,    53,    54,
       0,     0,     0,     0,     0,     0,     0,    57,     0,     0,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    75,   916,     0,
       0,    62,    63,     0,     0,    77,    78,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    75,
       0,     0,    13,    14,    15,    16,    17,    77,    78,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,  -408,  -408,     0,  -408,
      45,    46,     0,  -408,     0,     0,     0,     0,     0,     0,
       0,     0,    13,    14,    15,    16,    17,     0,     0,    19,
      57,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,  -408,  -408,     0,  -408,
      45,    46,     0,  -408,    62,    63,     0,     0,     0,     0,
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
       0,     0,     0,   318,    48,    49,    50,    51,    52,    53,
      54,     0,     0,     0,     0,     0,     0,     0,    57,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   843,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -602,
      75,   243,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,     0,     0,     0,     0,    45,    46,     0,
       0,     0,   318,    48,    49,    50,    51,    52,    53,    54,
       0,     0,     0,     0,     0,     0,     0,    57,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    62,    63,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1668,     0,
       0,     0,     0,   243,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,     0,    19,    75,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,     0,     0,     0,     0,    45,
      46,     0,     0,     0,   318,    48,    49,    50,    51,    52,
      53,    54,     0,     0,     0,     0,     0,     0,     0,    57,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     243,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    62,    63,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,  -408,  -408,     0,  -408,    45,    46,     0,  -408,
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
      48,    49,    50,    51,    52,    53,    54,     0,     0,     0,
       0,     0,     0,     0,    57,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    61,     0,     0,    62,    63,
      64,    65,    66,    67,    68,    69,    70,     0,     0,     0,
      13,    14,    15,    16,    17,     0,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,  -408,  -408,    75,  -408,    45,    46,
       0,  -408,     0,     0,   359,     0,     0,     0,   360,     0,
     361,     0,     0,     0,     0,     0,     0,     0,    57,     0,
       0,     0,     0,     0,     0,     0,     0,   362,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    62,    63,   363,   364,     0,   365,     0,   366,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   367,
     368,   356,     0,   369,   370,   371,   359,   372,   373,     0,
     360,     0,   361,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   362,
      75,     0,     0,     0,     0,   374,  1221,     0,    75,   375,
       0,     0,     0,  1222,     0,   376,    77,    78,   377,   378,
     379,   380,     0,     0,     0,     0,   363,   364,     0,   365,
       0,   366,    62,    63,    64,    65,    66,    67,    68,    69,
      70,   367,   368,   356,     0,   369,   370,   371,   359,   372,
     373,     0,   360,     0,   361,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   362,     0,     0,     0,     0,     0,   374,   947,  1519,
      75,   375,     0,     0,     0,     0,     0,   376,    77,    78,
     377,   378,   379,   380,     0,     0,     0,     0,   363,   364,
       0,   365,     0,   366,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   367,   368,   356,     0,   369,   370,   371,
     359,   372,   373,     0,   360,     0,   361,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   362,     0,     0,     0,     0,     0,   374,
       0,     0,    75,   375,     0,     0,     0,   464,     0,   376,
      77,    78,   377,   378,   379,   380,     0,     0,     0,     0,
     363,   364,     0,   365,     0,   366,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   367,   368,   356,     0,   369,
     370,   371,   359,   372,   373,     0,   360,     0,   361,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   362,     0,     0,     0,     0,
       0,   374,   789,     0,    75,   375,     0,     0,     0,     0,
       0,   376,    77,    78,   377,   378,   379,   380,     0,     0,
       0,     0,   363,   364,     0,   365,     0,   366,    62,    63,
      64,    65,    66,    67,    68,    69,    70,   367,   368,   356,
       0,   369,   370,   371,   359,   372,   373,     0,   360,     0,
     361,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   362,     0,     0,
       0,     0,     0,   374,     0,     0,    75,   375,     0,     0,
       0,   279,     0,   376,    77,    78,   377,   378,   379,   380,
       0,     0,     0,     0,   363,   364,     0,   365,     0,   366,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   367,
     368,   356,     0,   369,   370,   371,   359,   372,   373,     0,
     360,     0,   361,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   362,
       0,     0,     0,     0,     0,   374,   947,     0,    75,   375,
       0,     0,     0,     0,     0,   376,    77,    78,   377,   378,
     379,   380,     0,     0,     0,     0,   363,   364,     0,   365,
       0,   366,    62,    63,    64,    65,    66,    67,    68,    69,
      70,   367,   368,   356,     0,   369,   370,   371,   359,   372,
     373,     0,   360,     0,   361,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   362,     0,     0,     0,     0,     0,   374,     0,     0,
      75,   375,     0,     0,   978,     0,     0,   376,    77,    78,
     377,   378,   379,   380,     0,     0,     0,     0,   363,   364,
       0,   365,     0,   366,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   367,   368,   356,     0,   369,   370,   371,
     359,   372,   373,     0,   360,     0,   361,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   362,     0,     0,     0,     0,     0,   374,
    1285,     0,    75,   375,     0,     0,     0,     0,     0,   376,
      77,    78,   377,   378,   379,   380,     0,     0,     0,     0,
     363,   364,     0,   365,     0,   366,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   367,   368,   356,     0,   369,
     370,   371,   359,   372,   373,     0,   360,     0,   361,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   362,     0,     0,     0,     0,
       0,   374,     0,     0,    75,   375,     0,     0,     0,  1345,
       0,   376,    77,    78,   377,   378,   379,   380,     0,     0,
       0,     0,   363,   364,     0,   365,     0,   366,    62,    63,
      64,    65,    66,    67,    68,    69,    70,   367,   368,   356,
       0,   369,   370,   371,   359,   372,   373,     0,   360,     0,
     361,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   362,     0,     0,
       0,     0,     0,   374,     0,  1770,    75,   375,     0,     0,
       0,     0,     0,   376,    77,    78,   377,   378,   379,   380,
       0,     0,     0,     0,   363,   364,     0,   365,     0,   366,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   367,
     368,   356,     0,   369,   370,   371,   359,   372,   373,     0,
     360,     0,   361,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   362,
       0,     0,     0,     0,     0,   374,  1943,     0,    75,   375,
       0,     0,     0,     0,     0,   376,    77,    78,   377,   378,
     379,   380,     0,     0,     0,     0,   363,   364,     0,   365,
       0,   366,    62,    63,    64,    65,    66,    67,    68,    69,
      70,   367,   368,   356,     0,   369,   370,   371,   359,   372,
     373,     0,   360,     0,   361,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   362,     0,     0,     0,     0,     0,   374,     0,     0,
      75,   375,     0,     0,     0,     0,     0,   376,    77,    78,
     377,   378,   379,   380,     0,     0,     0,     0,   363,   364,
       0,   365,     0,   366,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   367,   368,   356,     0,   369,   370,   371,
     359,   372,   373,     0,   360,     0,   361,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   362,     0,     0,     0,     0,     0,   646,
       0,     0,    75,   375,     0,     0,     0,     0,     0,   376,
      77,    78,   377,   378,   379,   380,     0,     0,     0,     0,
     363,   364,     0,   365,     0,   366,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   367,   368,   356,     0,   369,
     370,   371,   359,   372,   373,     0,   360,     0,   361,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   362,     0,     0,     0,     0,
       0,   651,     0,     0,    75,   375,     0,     0,     0,     0,
       0,   376,    77,    78,   377,   378,   379,   380,     0,     0,
       0,     0,   363,   364,     0,   365,     0,   366,    62,    63,
      64,    65,    66,    67,    68,    69,    70,   367,   368,   356,
       0,   369,   370,   371,   359,   372,   373,     0,   360,     0,
     361,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   362,     0,     0,
       0,     0,     0,   654,     0,     0,    75,   375,     0,     0,
       0,     0,     0,   376,    77,    78,   377,   378,   379,   380,
       0,     0,     0,     0,   363,   364,     0,   365,     0,   366,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   367,
     368,   356,     0,   369,   370,   371,   359,   372,   373,     0,
     360,     0,   361,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   362,
       0,     0,     0,     0,     0,   374,     0,     0,    75,   375,
       0,     0,     0,     0,     0,   376,   857,    78,   377,   378,
     379,   380,     0,     0,     0,     0,   363,   364,     0,   365,
       0,   366,    62,    63,    64,    65,    66,    67,    68,    69,
      70,   367,   368,   356,     0,   369,   370,   371,     0,   372,
     373,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   374,     0,     0,
      75,   375,     0,     0,     0,     0,     0,   376,   437,    78,
     377,   378,   379,   380
};

static const yytype_int16 yycheck[] =
{
       1,    73,    73,     4,   173,    73,     1,   241,   852,   162,
     220,   684,   256,   464,   865,   407,   207,   374,   671,   322,
     283,   612,  1643,    73,   162,    58,     1,   178,   599,     4,
     219,   664,   631,    95,   599,   534,   599,    75,   852,     1,
     728,   219,     1,   513,   514,    73,   150,   603,   219,  1644,
     131,    82,   219,   755,    55,    56,   162,    58,     1,    73,
    1643,     4,     1,    58,    73,   222,  1643,   229,   628,     1,
     219,     0,    73,   139,   605,   574,   163,   177,   337,  1113,
     757,    82,   341,    58,   165,   757,   296,    82,   836,    90,
    1270,  1271,   254,   755,    95,   219,    58,    98,   219,    58,
      95,   102,   264,    98,   775,  1195,   295,   102,  1706,     4,
     181,     1,   145,   181,   862,    58,   147,   295,     0,    58,
     755,   755,   793,    98,   295,   191,    58,   102,   295,    75,
      76,   181,   236,   290,   291,  1337,     1,   774,   775,   140,
       0,   444,   143,    70,   145,    73,   295,   219,   219,   150,
     145,   219,    70,   181,  1764,   156,   793,  1752,   220,   102,
     192,   115,   163,   633,   131,    75,    76,   181,    58,   219,
     145,   295,   181,   130,   295,   758,  1097,  1228,   320,   180,
     181,   764,   320,   145,   755,   180,   145,  1097,   859,  1092,
     755,   219,   755,    58,   195,   337,  1768,  1097,   165,   341,
     195,   757,   145,    70,   205,   219,   145,   102,    95,   210,
     219,   157,   102,   145,   245,   172,   151,   152,   219,   220,
     780,     1,   859,   295,   295,   220,   153,   295,   759,    59,
      60,   149,   763,   146,   296,   236,   274,   629,   115,   152,
     913,   772,   773,   131,   245,   295,   174,   157,   374,    70,
     245,  1846,   496,   151,   255,   145,   953,   258,   156,  1162,
     173,  1105,   146,   258,   265,   735,  1876,    19,   941,   178,
      87,   302,  1893,    96,   275,   276,   153,   278,    58,   470,
     145,   491,   149,   258,    70,   982,   153,   174,   591,   173,
    1134,  1105,    87,   482,   295,   296,   438,   930,   561,  1894,
     438,   296,   303,   991,   482,    87,  1008,   906,   309,   310,
    1893,   482,   615,   314,  1404,   482,  1893,    82,    98,   622,
    1134,   283,   157,   211,   572,   682,   149,   157,   149,   588,
     886,  1926,   153,   482,   833,   104,   105,  1935,  1015,   174,
     157,  1089,  1914,  1015,   174,   346,  1008,   907,  1045,   149,
     351,   283,     4,   354,   355,   442,    70,   315,   482,    98,
     570,   482,   157,   149,  1566,   145,   625,   153,   434,  1941,
     109,   533,   149,  1008,  1008,   157,   131,   408,   148,   568,
    1431,  1432,  1433,   152,   149,   155,  1057,  1308,  1309,  1310,
     568,   149,    70,   283,   154,   353,   284,   568,  1308,  1309,
    1310,   568,  1305,    55,    56,   160,   161,   511,  1308,  1309,
    1310,   482,   151,   517,   482,   173,    70,   418,   283,   568,
    1057,   149,     1,   418,   462,     4,  1009,   149,   156,   491,
     228,   146,   482,   231,   572,   149,    73,  1008,    90,   153,
     441,   442,   157,  1008,   568,  1008,   588,   568,  1628,  1629,
     120,    88,   453,   454,   482,   253,   195,    70,   173,  1015,
    1878,   462,   149,   464,    70,   263,   572,    70,   482,    70,
     612,   149,  1890,   482,   612,   153,   149,   174,   258,    58,
      70,   482,   152,   625,   157,   157,   374,   519,   140,   362,
     491,   143,   149,  1337,  1338,   149,   491,   568,  1916,   153,
     568,  1032,   174,    82,   156,  1539,   265,   151,   570,  1543,
     511,   163,   156,   149,   387,   388,   517,   276,   568,   258,
     646,    70,   648,   649,   151,   651,   774,   775,   654,   156,
     149,   657,   658,   659,   985,   408,   149,    87,   157,     1,
     153,   844,   322,   149,   155,   793,   149,   153,   149,   288,
     153,   155,   153,   174,   555,   294,   557,   589,   210,   149,
     139,   165,    70,   153,     3,   438,   145,   568,   147,   570,
      70,   155,   875,     3,    56,   570,    70,    59,    60,   155,
      62,   150,   583,   322,   155,  1238,   587,  1178,   157,   155,
     174,   782,  1191,   149,   165,   151,    58,   155,   174,   561,
     149,  1184,   155,   255,   153,   155,    73,   157,   174,   709,
     152,   859,   191,   265,   873,   503,   174,   768,   619,    70,
     508,   174,    89,   275,   276,   155,   278,   589,    70,   561,
     631,   151,  1812,    95,   155,   155,    98,   525,  1198,   106,
     102,   149,   155,   149,   174,   153,   155,   535,   173,   149,
    1501,   303,  1686,   153,  1327,   149,   151,   309,   310,   153,
     155,   174,   314,   242,   444,   174,   245,   149,   157,   559,
     757,   561,   710,   148,   675,   676,  1259,   678,   926,   711,
     155,   682,   115,   145,   685,  1245,    12,    13,    14,    15,
      16,   795,   157,   192,   346,  1348,   561,   155,   155,   351,
     149,  1171,   354,   151,   283,   444,   504,   149,   156,   710,
     814,   153,   171,   904,   151,   815,   174,   174,   155,   155,
     752,   157,  1566,   302,   143,   144,   145,   351,   526,  1426,
     354,   873,   149,   195,   532,   149,   155,   149,   536,  1773,
     151,   153,   774,   775,    70,   129,   165,  1781,    12,    13,
      14,    15,    16,   151,   755,   174,   757,   155,   646,   151,
     149,   793,   151,   651,   153,   149,   654,   149,   769,   153,
    1423,  1222,   125,   126,   151,   776,   160,   161,   558,   518,
     151,   782,   152,   153,   785,   673,   156,   151,   926,   441,
     752,   162,   163,   794,   795,   796,   258,   104,   105,   538,
    1044,   453,   454,  1195,  1838,   131,    70,   151,   129,  1057,
     897,   155,   151,   814,   153,   688,   169,   170,   160,   558,
     926,   320,   104,   105,   323,   167,   168,   859,   149,   408,
      46,    47,   153,    49,   151,   615,   151,    53,   337,   160,
     161,  1401,   341,   151,   149,  1689,   151,   155,   153,   850,
     851,   852,   591,   151,   151,   434,   103,   155,   155,   768,
     107,  1052,  1422,   110,   149,   112,   155,   131,    56,   149,
     779,    59,    60,   153,    62,     3,   615,   852,   149,  1182,
     151,    21,   153,   622,    12,    13,    14,    15,    16,   864,
     852,   151,   149,   852,  1345,   155,   897,   149,   149,    82,
     901,   153,   153,   555,   149,   906,   151,   149,   153,   852,
      96,   912,   155,   852,  1611,   155,  1613,   129,   155,  1076,
     852,   864,    12,    13,    14,    15,    16,  1014,  1015,   151,
     155,   583,   820,   155,   129,   587,   151,   149,   149,   438,
     155,   153,    70,   831,   945,   149,   834,   149,   160,   161,
     838,   153,   953,  1187,   149,  1312,   418,  1205,   153,   151,
     157,   173,   852,   155,   147,   160,   161,   619,  1898,   864,
    1421,   118,  1902,   120,   121,   122,   555,   162,   163,   631,
      70,   982,   561,  1827,   985,   123,   124,   852,  1548,    12,
      13,    14,    15,    16,   151,    87,   179,  1596,   155,  1843,
     148,   248,   149,  1172,  1173,   152,   153,  1008,   127,   128,
     157,   158,  1404,  1014,  1015,   108,   109,   110,   111,   112,
     519,   157,  1270,   675,   676,  1057,   678,   151,   151,   491,
     682,   155,   155,   685,   151,  1879,  1178,   835,   155,   129,
    1178,   157,   151,   154,  1045,  1605,   155,    70,   151,   922,
     151,   151,   155,   151,   927,   155,  1300,   155,   710,   149,
     157,  1165,   245,   153,   844,   938,  1097,  1205,   157,   978,
     160,   161,   852,   572,  1337,   855,   157,   324,   325,   173,
     327,   151,   329,  1209,   151,   155,   154,   155,   155,   588,
     589,     3,   151,   151,   151,   875,   155,   155,   155,  1205,
      12,    13,    14,    15,    16,   844,  1107,   151,  1318,  1110,
    1111,  1112,   115,   612,   154,   155,   855,   769,  1271,   302,
    1719,   149,  1795,   149,   776,   149,   625,  1278,  1279,   166,
    1105,   160,   161,  1134,   154,   155,   875,   320,  1386,  1140,
     154,   155,   794,  1105,   796,  1054,  1105,  1148,   154,   155,
    1151,  1152,   154,   155,  1155,   155,  1151,  1152,    70,  1134,
    1720,   158,  1105,   158,  1165,   161,  1105,   696,   697,   698,
     699,   159,  1134,  1105,   129,  1134,  1151,  1152,   171,     4,
       5,     6,     7,     8,     9,    10,    11,   689,   690,   691,
    1191,  1134,   143,   144,   145,  1134,   155,   156,   850,   851,
     852,   151,  1134,  1091,   155,  1206,  1492,  1493,  1494,  1152,
     154,   155,   711,   152,   165,  1105,  1104,   151,  1094,  1095,
    1096,  1222,   151,   174,   151,   408,   151,  1228,   154,   155,
     154,   155,  1105,  1121,   154,   155,   154,   155,  1270,  1271,
    1105,   155,   156,  1803,  1134,   897,   154,   155,  1386,   901,
     154,   155,   151,   752,   906,   438,  1318,  1152,   153,  1260,
     154,   155,  1152,    12,    13,    14,    15,    16,    17,  1134,
     154,   155,   154,   852,   154,   155,   131,  1308,  1309,  1310,
    1386,  1312,  1313,   530,   131,   864,   154,   155,   155,  1499,
      12,    13,    14,    15,    16,    17,   156,  1507,   154,   155,
     154,   155,   151,  1566,   156,  1178,     4,     5,     6,     7,
       8,     9,    10,    11,   154,   155,  1505,  1318,   154,   155,
     149,  1322,   155,   156,  1325,  1105,   151,  1505,    75,    76,
     513,   514,  1241,   156,  1505,   151,  1311,   151,  1505,  1212,
    1213,  1214,   155,   156,  1345,   149,  1219,  1220,   151,   174,
    1229,  1230,   692,   693,  1134,   151,  1505,   694,   695,   700,
     701,   151,  1337,  1338,  1365,    63,  1367,   154,  1311,  1278,
    1279,  1151,  1367,   153,   873,  1337,  1338,   876,  1512,  1513,
    1628,  1505,  1172,  1173,  1505,    12,    13,    14,    15,    16,
      17,   157,  1367,   157,  1337,  1338,   157,   157,    68,   205,
     154,   149,  1182,    76,   154,  1337,  1338,    17,   173,   155,
     149,   174,  1151,   157,   151,   151,  1311,   157,   174,   157,
    1421,   154,   154,    17,  1577,  1426,   151,   926,   148,   612,
    1431,  1432,  1433,  1505,  1409,   151,   151,  1499,   151,  1577,
    1328,  1329,   148,  1182,   151,  1507,   151,  1337,  1338,   151,
     633,  1661,   151,   157,   157,  1107,   157,   151,  1110,  1111,
    1112,    68,   174,   173,   151,  1565,  1409,     9,   151,  1658,
     151,  1577,  1337,  1338,   148,   157,  1629,  1603,  1366,   151,
    1658,   148,  1134,   151,   155,  1517,   151,  1658,  1140,   151,
     151,  1658,   155,   151,   151,   151,  1148,   151,  1499,   151,
     151,   151,   151,  1155,  1505,   151,  1507,   151,   151,  1658,
     151,   151,   154,  1514,  1409,   151,   151,     1,  1097,   151,
       4,   151,   148,    13,   155,   173,  1105,  1528,   149,   149,
     149,   149,   149,   149,  1658,    72,  1537,  1658,    89,  1191,
     174,   156,   155,   154,  1847,    12,    13,    14,    15,    16,
      17,  1761,   735,   156,  1206,  1134,   154,  1337,  1338,   101,
    1535,  1536,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   148,   174,  1574,    58,   174,   148,   174,   157,   155,
    1319,   174,   151,   154,   151,   155,   154,  1367,   151,    73,
     155,  1566,   151,  1536,   155,  1596,  1628,  1629,    82,  1661,
     148,  1701,   151,   148,  1566,  1580,   149,  1711,   174,   149,
    1611,    95,  1613,  1764,    98,    78,   174,   174,   102,   174,
     174,   174,   174,  1566,   149,   148,   148,   174,  1367,   149,
     174,   148,   151,   154,  1566,   174,   154,  1580,   151,   155,
     155,  1536,   101,   154,   154,  1787,  1536,   106,   107,   108,
     109,   110,   111,   112,   113,   139,   462,  1658,   464,  1812,
    1661,   145,   157,   147,   151,   148,   150,   151,   156,  1557,
    1322,  1672,   118,  1325,   148,  1676,  1566,   156,   162,  1178,
     151,   151,   865,   151,  1894,  1580,   151,  1688,   154,  1151,
    1152,  1791,   154,  1694,   153,   179,   180,   181,  1571,  1761,
     151,  1566,  1844,  1678,  1893,   148,  1205,   191,   192,  1710,
    1711,   195,   156,  1365,  1689,  1893,  1926,   155,  1719,   151,
     149,   151,  1893,   149,  1875,  1876,  1893,  1689,   155,  1308,
    1309,  1310,  1311,  1312,  1313,   219,   220,   149,   107,   154,
     154,   154,  1846,   148,  1893,   148,  1689,   157,   230,  1900,
     151,   151,   236,   151,   151,  1535,  1787,  1689,  1337,  1338,
    1761,   245,   154,   151,   151,   174,   174,  1768,   148,  1893,
     149,    88,  1893,   151,   258,   174,   151,   154,   154,   151,
    1812,   148,   148,   151,   174,   156,  1566,   151,   151,   151,
     151,  1792,    73,   153,   152,    73,  1535,   156,  1898,  1689,
     174,   174,  1902,  1903,   288,   151,   148,   151,   151,   174,
     294,   295,   296,  1844,   151,   155,   150,  1968,   302,   150,
     156,  1893,  1893,   149,  1689,  1893,   101,   155,  1928,    73,
    1409,  1832,  1894,   149,   148,   165,   320,   321,   322,  1840,
     165,   550,   174,  1893,   154,  1846,   107,  1848,   107,   151,
    1950,   174,  1827,   337,  1954,  1764,   156,   341,  1859,   151,
    1861,  1862,  1514,   148,  1926,  1827,   148,   151,  1843,   149,
     174,    73,   151,   174,   151,  1975,  1528,   174,   376,  1603,
    1881,  1843,  1327,  1235,  1827,  1537,   663,  1386,   703,   706,
     374,   702,  1893,  1894,   704,  1827,   705,  1123,  1678,  1894,
    1843,  1134,  1941,  1904,  1879,  1367,  1566,  1876,   821,  1689,
    1693,  1843,  1890,  1914,  1689,  1776,  1936,  1879,  1796,  1935,
    1923,  1794,  1574,  1558,   408,  1926,  1558,   411,  1844,   411,
    1903,  1926,  1954,    48,   418,  1843,  1879,  1827,  1939,  1678,
    1941,  1155,   250,  1761,  1596,   427,  1107,  1879,   430,  1820,
     434,  1499,  1313,  1843,   438,  1148,  1957,   471,   442,  1513,
     444,  1962,  1827,   782,  1580,   878,  1875,  1876,   868,   583,
       0,  1972,   912,    -1,  1409,  1976,   782,    -1,  1843,   785,
      -1,    -1,   727,    -1,   727,  1986,   727,  1566,  1171,  1879,
      -1,  1900,    -1,    -1,    -1,  1178,    -1,    -1,   482,    -1,
      -1,  1580,    -1,    -1,    -1,   487,    -1,   491,    -1,    -1,
      -1,    -1,    -1,    -1,  1879,  1924,    -1,    -1,    -1,    -1,
    1672,    -1,    -1,    -1,  1676,    -1,    -1,   511,    -1,   513,
     514,    -1,    -1,   517,    -1,   519,  1688,    -1,    -1,    -1,
      -1,    -1,  1694,    -1,   101,    -1,    -1,  1827,    -1,   106,
     107,   108,   109,   110,   111,   112,     9,    -1,  1710,  1968,
      -1,  1949,    -1,  1843,    -1,    -1,    -1,  1719,    -1,    -1,
      -1,    -1,    -1,   557,   987,    -1,  1964,    12,    13,    14,
      15,    16,    -1,    -1,   568,    -1,   570,    -1,   572,    -1,
      -1,  1004,  1005,   150,    -1,    -1,   153,    -1,    -1,  1879,
      -1,    -1,    -1,    -1,   588,   589,    -1,   591,  1847,    -1,
    1689,    -1,   821,    -1,    -1,   599,  1768,    -1,    -1,   603,
       4,     5,     6,     7,     8,     9,    10,    11,   612,    -1,
      -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,   622,   945,
    1792,   625,    -1,    -1,    -1,    -1,    -1,   953,   101,   633,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      -1,    -1,   646,    -1,   648,   649,    -1,   651,    -1,   878,
     654,    -1,    -1,   657,   658,   659,   982,    -1,    -1,   985,
    1832,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1840,    -1,
      17,    -1,   101,    -1,   129,    -1,  1848,   106,   107,   108,
     109,   110,   111,   112,  1365,    -1,    -1,  1859,    -1,  1861,
    1862,    -1,    -1,    -1,   149,    -1,    -1,    -1,   153,    -1,
      -1,    -1,    -1,    -1,    -1,   160,   161,   711,    -1,  1881,
      -1,    -1,    59,    60,    61,    62,    -1,    -1,    -1,  1045,
     149,   150,    -1,   727,   728,   727,   728,    -1,  1827,    -1,
      -1,   735,  1904,     3,    -1,   737,    -1,    -1,   740,    -1,
      -1,    -1,  1914,    -1,  1843,    -1,    -1,    -1,   752,    -1,
      -1,   755,    -1,   757,   101,    -1,    -1,    -1,   987,   106,
     107,   108,   109,   110,   111,   112,   113,  1939,  1787,  1941,
     774,   775,    -1,    -1,    -1,  1004,  1005,    -1,    -1,    -1,
    1879,    -1,    -1,    -1,    -1,  1957,    -1,    -1,    -1,   793,
    1962,   795,    -1,    12,    13,    14,    15,    16,  1501,   801,
    1972,    -1,    -1,   805,  1976,    -1,   153,   809,    -1,    -1,
     814,    -1,    -1,    -1,  1986,    -1,    -1,    -1,  1251,  1252,
      12,    13,    14,    15,    16,  1844,    -1,    -1,    -1,    -1,
      -1,   101,  1265,  1266,   104,   105,   106,   107,   108,   109,
     110,   111,   112,    -1,    -1,    -1,    -1,  1528,   852,    -1,
      -1,    70,    73,    -1,    -1,   859,  1537,    -1,    -1,   129,
     864,   865,    -1,    -1,    -1,  1298,  1299,    -1,    -1,   873,
      -1,   875,    -1,    -1,    95,    -1,    -1,    -1,    70,   149,
     150,    -1,   886,    -1,    -1,    12,   156,    -1,    -1,    -1,
     160,   161,    -1,  1574,    -1,     1,  1222,    -1,     4,    -1,
      -1,    -1,  1228,    -1,    -1,    -1,    -1,    -1,    -1,   101,
     129,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,    -1,   926,    -1,    -1,    -1,    -1,    -1,    -1,   150,
     149,    -1,    -1,    -1,   153,    -1,    -1,   129,    -1,    -1,
     101,   160,   161,   104,   105,   106,   107,   108,   109,   110,
     111,   112,    58,    -1,    -1,    -1,    -1,   149,   150,    86,
      -1,   153,    -1,    -1,    -1,    -1,    -1,    -1,   160,   161,
      -1,    -1,    -1,    -1,   101,    -1,    82,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    -1,   991,   149,   991,
      -1,  1672,    98,    -1,    -1,  1676,   102,    -1,   219,   220,
      -1,    -1,    -1,    -1,  1008,    -1,    -1,  1688,    -1,    -1,
      -1,  1015,    -1,  1694,    -1,   236,    -1,    -1,    -1,  1345,
      -1,   101,  1251,  1252,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   139,    -1,    -1,  1265,  1266,    -1,   145,
      -1,   147,    -1,    -1,    -1,   151,    -1,    -1,  1481,  1482,
      -1,    -1,    -1,  1057,    -1,   161,   162,   163,    -1,    -1,
    1062,    -1,    -1,  1065,    -1,    -1,    -1,    -1,    -1,  1298,
    1299,    -1,    -1,   179,   295,   296,    63,    64,    65,    66,
      -1,    -1,    -1,    -1,    -1,   191,   192,  1768,    -1,   195,
      -1,    -1,    -1,  1097,   174,  1421,    -1,    -1,    -1,    -1,
    1426,  1105,    -1,    -1,    -1,  1431,  1432,  1433,    -1,    -1,
      -1,  1792,    -1,    -1,   101,    -1,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    -1,    -1,    -1,    -1,
    1134,    -1,    -1,    -1,    -1,   101,   242,    -1,    -1,   245,
     106,   107,   108,   109,   110,   111,   112,  1151,  1152,    -1,
      -1,  1832,   258,    -1,    -1,  1588,    -1,    -1,    -1,  1840,
      -1,  1165,    -1,    -1,    -1,    -1,   153,  1171,    -1,   275,
      -1,    -1,    -1,    -1,  1178,    -1,    -1,   283,  1859,    -1,
    1861,  1862,   288,    -1,   171,  1618,    -1,    -1,   294,    -1,
    1623,  1624,    -1,    -1,    -1,    -1,   302,    -1,    17,    -1,
    1881,  1205,    -1,    -1,    -1,  1209,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   320,    -1,   322,   323,    -1,    -1,
      -1,   442,    -1,  1904,    12,    13,    14,    15,    16,    -1,
      -1,   337,    -1,  1914,    -1,   341,    55,    56,    57,    58,
      59,    60,    61,    62,    -1,  1247,    -1,    -1,    -1,    -1,
      -1,    -1,  1481,  1482,  1256,    -1,    -1,    -1,  1939,    -1,
    1941,   482,    -1,    -1,    -1,    -1,  1270,  1271,   374,    -1,
     491,    -1,    -1,    -1,    -1,    -1,  1957,    -1,    -1,    -1,
      -1,  1962,    70,    -1,    -1,  1611,    -1,  1613,  1517,    -1,
     511,  1972,    -1,    -1,    -1,    -1,   517,    -1,    -1,    -1,
      -1,    -1,   408,    -1,  1308,  1309,  1310,  1311,  1312,  1313,
      -1,    -1,    -1,   101,  1318,  1319,   104,   105,   106,   107,
     108,   109,   110,   111,   112,    -1,    -1,    -1,   434,    -1,
      -1,    -1,   438,  1337,  1338,    -1,   557,    -1,   444,    -1,
      -1,   129,    -1,    -1,    -1,    -1,    -1,   568,    -1,   570,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1588,
      -1,   149,   150,  1367,    -1,   153,    -1,    -1,    -1,    -1,
      -1,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1386,    -1,    -1,    -1,    76,    -1,    -1,  1618,
      -1,    -1,    -1,    -1,  1623,  1624,    -1,    70,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1409,    -1,   513,   514,    -1,
      -1,   101,   518,   519,   104,   105,   106,   107,   108,   109,
     110,   111,   112,    -1,    -1,    -1,    -1,    -1,   101,    -1,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      -1,    -1,    -1,    -1,   550,    -1,    -1,    -1,    -1,   555,
      -1,    -1,   558,   559,   101,   561,   129,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   572,    -1,    -1,    -1,
      -1,    12,    13,    14,    15,    16,   149,   150,  1911,    -1,
     153,   587,   588,   589,   174,   591,    -1,   160,   161,    -1,
      -1,  1495,    -1,  1495,    -1,  1499,    -1,  1501,    -1,    -1,
      -1,  1505,    -1,  1507,    -1,   152,   612,    -1,    -1,   615,
     157,    -1,    -1,   619,    -1,    -1,   622,    -1,    -1,   625,
      -1,   627,    -1,    -1,    -1,   374,    -1,   633,    -1,    70,
      -1,  1535,  1536,    -1,   755,    -1,   757,    -1,    -1,    -1,
     646,    -1,   648,   649,    -1,   651,    -1,    -1,   654,    -1,
      -1,   657,   658,   659,    -1,    -1,    -1,    -1,    -1,    -1,
     101,    -1,  1566,   104,   105,   106,   107,   108,   109,   110,
     111,   112,    -1,  1577,   795,   101,  1580,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,    -1,   129,    -1,
      -1,    -1,    -1,   814,    -1,    -1,    70,    -1,    -1,  1603,
      -1,    -1,    -1,    -1,    -1,   711,    -1,   101,   149,   150,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   160,
     161,    -1,    -1,    -1,  1628,  1629,    -1,   101,    -1,   735,
     104,   105,   106,   107,   108,   109,   110,   111,   112,  1643,
    1644,  1643,  1644,    -1,    -1,    -1,   752,    -1,   174,    -1,
      -1,    -1,    -1,    -1,  1658,    -1,    -1,  1661,    -1,    -1,
      -1,    -1,    -1,    -1,   513,   514,   160,    -1,   774,   775,
      -1,    -1,    -1,    -1,  1678,   149,   150,    -1,    -1,    -1,
      -1,    -1,  1911,    70,    -1,  1689,    -1,   793,    -1,    -1,
      -1,    -1,    -1,    -1,     1,   101,    -1,     4,   104,   105,
     106,   107,   108,   109,   110,   111,   112,  1711,    -1,    -1,
      -1,    -1,    -1,    -1,   101,   821,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   101,    -1,    -1,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   844,    -1,
      -1,    -1,   129,    -1,   150,    -1,   852,   153,  1752,   855,
    1752,    58,    -1,   859,    -1,    -1,    -1,  1761,   864,   865,
      -1,    -1,   149,   150,    -1,    -1,    -1,   873,    -1,   875,
     876,    -1,   878,   160,   161,    82,    -1,    -1,    -1,    -1,
      -1,    -1,   157,  1787,    -1,    -1,    -1,  1008,    -1,    -1,
      -1,    -1,    -1,  1014,  1015,   102,    -1,   646,    -1,    -1,
      -1,    -1,   651,    -1,    -1,   654,    -1,    -1,  1812,    -1,
      -1,    -1,    -1,    -1,  1816,    -1,  1820,    -1,  1820,    -1,
     926,    -1,    -1,  1827,   673,    -1,    -1,    -1,    70,    -1,
      -1,    -1,   139,    -1,    -1,    -1,    -1,    -1,   145,  1843,
    1844,    -1,  1846,  1847,  1846,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   162,    -1,    -1,   707,   101,
      -1,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,    -1,    -1,   180,    -1,  1879,    -1,    -1,    -1,    -1,
      -1,   987,    -1,    -1,   191,   192,    -1,   129,    -1,  1893,
    1894,  1893,  1894,    -1,    -1,    -1,    -1,    -1,  1004,  1005,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,   150,    -1,
      -1,    -1,    -1,   220,    -1,    -1,    -1,    -1,   160,   161,
      -1,    -1,  1926,    -1,  1926,    -1,    -1,    -1,    -1,   236,
      -1,    -1,    -1,    -1,   241,   242,    -1,    -1,   245,    -1,
      -1,    -1,    -1,    -1,  1165,    -1,    70,    -1,    -1,    -1,
      -1,  1057,    -1,    12,    13,    14,    15,    16,    -1,    -1,
     267,    -1,    -1,   270,   101,   272,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   283,   101,    -1,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   296,
      -1,  1097,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1105,
      -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,
       1,    70,    -1,   320,    -1,   152,   323,    -1,    -1,    -1,
      12,    13,    14,    15,    16,   149,   150,    -1,  1134,    -1,
     337,    -1,    -1,    -1,   341,    -1,   160,   161,    -1,  1260,
      -1,    -1,   101,    -1,    -1,  1151,  1152,   106,   107,   108,
     109,   110,   111,   112,   132,   133,   134,   135,   136,   137,
     138,   139,   140,   141,   142,  1171,    -1,    58,   146,    -1,
     129,    -1,  1178,    -1,    -1,    -1,  1182,    -1,    70,    -1,
      -1,    12,    13,    14,    15,    16,    -1,    -1,  1194,    -1,
     149,   150,    -1,    -1,    -1,   173,    -1,  1318,    -1,  1205,
      -1,   160,   161,  1209,    -1,    -1,    -1,    -1,    -1,   101,
      -1,   102,   104,   105,   106,   107,   108,   109,   110,   111,
     112,    -1,    -1,    -1,    -1,    -1,    -1,   434,    -1,    -1,
      -1,   438,    -1,    -1,    -1,    -1,    -1,   129,    -1,    70,
      -1,    -1,    -1,    -1,    -1,  1251,  1252,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   145,    -1,    -1,   149,   150,  1265,
    1266,    -1,    -1,    -1,  1270,  1271,    -1,    -1,   160,   161,
     101,   162,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1298,  1299,    -1,    -1,    -1,    -1,   129,    -1,
      -1,   192,  1308,  1309,  1310,  1311,  1312,  1313,    -1,    -1,
      -1,    -1,   519,  1319,    -1,    -1,    -1,   101,   149,   150,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   160,
     161,  1337,  1338,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   550,    -1,    -1,    -1,    -1,   555,    -1,
      -1,    -1,   559,    -1,   561,    -1,    -1,    -1,    -1,    -1,
      -1,  1367,    -1,    -1,    -1,   572,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   157,    -1,    70,   267,    -1,  1499,    -1,
    1386,   588,   589,    -1,  1505,    -1,  1507,    -1,    -1,    -1,
      -1,    -1,   283,    -1,    -1,    -1,   603,    -1,    -1,    -1,
      -1,    -1,    -1,  1409,    -1,   612,   101,    -1,    -1,    -1,
     617,   106,   107,   108,   109,   110,   111,   112,   625,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   320,
      -1,    -1,   323,   101,   129,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,    -1,   337,    -1,    -1,    -1,
     341,    -1,    -1,    -1,   149,   150,    -1,    -1,    -1,    -1,
      -1,   129,    -1,    -1,    -1,   160,   161,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1481,  1482,    -1,    -1,    -1,
      -1,   149,   150,    -1,    -1,   153,    -1,    -1,    -1,    -1,
      -1,    -1,   160,   161,    -1,  1501,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   711,   173,    -1,   101,    -1,    -1,
      -1,  1517,   106,   107,   108,   109,   110,   111,   112,   113,
      -1,   728,    -1,   117,    -1,   119,    -1,    -1,    -1,  1535,
    1536,    -1,    -1,    -1,    -1,    -1,    -1,  1658,    -1,    -1,
    1661,    -1,    -1,    -1,    -1,   752,    -1,   438,    -1,    -1,
     757,    -1,    -1,    -1,    -1,    -1,   150,    -1,    -1,   153,
    1566,    -1,    -1,    -1,    -1,    -1,    -1,   774,   775,    -1,
      -1,  1577,    -1,    -1,  1580,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1588,    -1,    -1,  1334,   793,    -1,  1337,  1338,
    1711,    -1,    -1,    -1,  1343,    -1,    -1,  1603,  1347,   101,
    1349,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,    -1,  1618,    -1,   821,    -1,    -1,  1623,  1624,    -1,
      -1,    -1,  1628,  1629,    -1,   101,    -1,    -1,   519,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,    -1,    -1,
    1761,   117,    -1,   119,    -1,   852,    -1,    -1,    -1,    -1,
      -1,   153,   859,    -1,    -1,    -1,    -1,   864,    -1,   550,
      -1,    -1,    -1,    -1,    -1,    -1,   873,    -1,   559,   876,
     561,   878,  1678,    -1,   150,    -1,   883,   153,     1,    -1,
      -1,   572,   101,  1689,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,    -1,   101,   588,   589,   104,
     105,   106,   107,   108,   109,   110,   111,   112,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   926,
      -1,   612,    -1,    -1,   129,  1846,    -1,    -1,    -1,    -1,
     149,   150,    -1,    -1,   625,    58,    -1,   156,  1487,    -1,
      -1,   160,   161,    -1,   149,   150,    -1,    -1,   153,    -1,
      -1,    -1,    -1,   101,    -1,   160,   161,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,    -1,    -1,    -1,   117,
    1519,   119,  1893,  1894,    -1,    -1,    -1,    -1,    -1,   102,
     987,  1787,  1531,    -1,    -1,    -1,    -1,    -1,    -1,  1538,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1004,  1005,    -1,
      -1,    -1,   150,    -1,   101,  1926,  1812,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    -1,  1566,   101,    -1,
     711,  1827,   145,   106,   107,   108,   109,   110,   111,   112,
      -1,    -1,   129,    -1,    -1,    -1,    -1,  1843,  1844,   162,
      -1,  1847,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,
    1057,    -1,   149,   150,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   752,    -1,   160,   161,    -1,   149,   150,    -1,   192,
     153,    -1,    -1,  1879,    -1,    -1,    -1,   160,   161,    -1,
      -1,    -1,    -1,   774,   775,    -1,    -1,    -1,    -1,    -1,
    1097,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1105,    -1,
      -1,    -1,   793,   101,    -1,  1911,   104,   105,   106,   107,
     108,   109,   110,   111,   112,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1674,    -1,  1134,    -1,    -1,
     821,   129,    -1,    -1,    -1,    -1,    -1,  1686,  1687,    -1,
    1689,    -1,    -1,    -1,    -1,  1152,    -1,    -1,    -1,    -1,
      -1,   149,   150,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     283,   852,   160,   161,    -1,    -1,    -1,    -1,   859,    -1,
      -1,  1178,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1187,    -1,   873,    -1,    -1,   876,    -1,   878,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   320,  1205,    99,
     323,   101,    -1,   103,   104,   105,   106,   107,   108,   109,
     110,   111,   112,    -1,   337,    -1,    -1,    -1,   341,    -1,
      -1,    -1,   101,    -1,    -1,    -1,  1775,   106,   107,   108,
     109,   110,   111,   112,   113,   926,    -1,    -1,   117,    -1,
     119,    -1,    -1,    -1,  1251,  1252,    -1,    -1,    -1,   149,
      -1,    -1,   152,   153,    -1,    -1,    -1,    -1,  1265,  1266,
      -1,    -1,    -1,  1270,  1271,    -1,    -1,    -1,    -1,    -1,
      -1,   150,   101,    -1,   153,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,    -1,    -1,    -1,  1837,    -1,
    1839,  1298,  1299,    -1,    -1,    -1,   987,    -1,    -1,    -1,
      -1,  1308,  1309,  1310,  1311,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1004,  1005,   438,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   152,    -1,    -1,    -1,    -1,    -1,   101,
    1337,  1338,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   101,    -1,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,    -1,    -1,    -1,    -1,    -1,  1907,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1057,    -1,    -1,    -1,
    1919,  1920,  1921,    -1,    -1,    -1,    -1,   149,    -1,  1386,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   519,    -1,    -1,    -1,
      -1,    -1,  1409,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1105,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   550,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   559,    -1,   561,    -1,
      -1,    -1,    -1,  1134,    -1,    -1,    -1,    -1,    -1,   572,
      -1,    -1,    -1,    -1,    -1,     1,    -1,    -1,     4,    -1,
      -1,  1152,    -1,    -1,    -1,   588,   589,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1481,  1482,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1178,    -1,   612,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1507,    -1,   625,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1517,    -1,    58,    -1,  1205,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1536,
      -1,    -1,    -1,    -1,    -1,    -1,    82,     1,    -1,    -1,
       4,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   102,    -1,    -1,  1566,
    1251,  1252,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1577,    -1,    -1,  1580,  1265,  1266,    -1,    -1,    -1,  1270,
    1271,  1588,    -1,    -1,    -1,    -1,    -1,    -1,   711,    -1,
      -1,    -1,    -1,   139,    58,    -1,    -1,    -1,    -1,   145,
      -1,   147,    -1,    -1,    -1,    -1,    -1,  1298,  1299,    82,
      -1,  1618,    -1,    -1,    -1,    -1,  1623,  1624,    82,    -1,
      -1,  1628,  1629,    -1,    -1,    -1,    -1,    -1,    -1,   752,
      -1,    -1,    -1,   179,    -1,    -1,    -1,  1644,   102,    -1,
      -1,    -1,    -1,    -1,    -1,   191,  1337,  1338,    -1,    -1,
      -1,   774,   775,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     793,    -1,    -1,    -1,   147,   139,    -1,    -1,    -1,    -1,
      -1,   145,  1689,   147,    -1,    -1,    -1,    -1,    -1,   162,
      -1,    -1,    -1,    -1,    -1,  1386,   242,    -1,   821,   245,
      -1,    -1,    -1,    -1,   250,    -1,   179,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   179,    -1,    -1,    -1,   192,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   191,    -1,   852,
      -1,    -1,    -1,    -1,    -1,    -1,   859,   283,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     873,    -1,    -1,   876,    -1,   878,   302,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   245,    -1,    -1,    -1,     1,    -1,   242,     4,
    1787,   245,    -1,    -1,    -1,    -1,   250,    -1,    -1,    -1,
    1481,  1482,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   926,    -1,  1812,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   283,
    1827,    -1,    -1,    -1,    -1,    -1,  1517,    -1,   374,   302,
      -1,    -1,    -1,    58,    -1,    -1,  1843,  1844,   302,    -1,
      -1,    -1,    -1,    -1,    -1,  1536,    -1,   320,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,
      -1,    -1,   408,    -1,   987,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1879,    -1,    -1,  1566,    -1,   102,    -1,    -1,
      -1,  1004,  1005,    -1,    -1,    -1,  1577,  1894,   434,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1588,    -1,    -1,
      -1,    -1,    -1,    -1,  1911,    -1,    -1,    -1,    -1,    -1,
     374,    -1,    -1,    -1,   139,    -1,    -1,    -1,    -1,    -1,
     145,    -1,   147,    -1,    -1,    -1,    -1,  1618,    -1,    -1,
      -1,    -1,  1623,  1624,  1057,   408,    -1,  1628,  1629,    -1,
      -1,    -1,    -1,    -1,   408,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   179,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   438,   191,   513,   514,    -1,
     434,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1689,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   555,
      -1,  1134,    -1,   559,    -1,   561,    -1,   242,    -1,    -1,
     245,    -1,    -1,    -1,    -1,   250,    -1,    -1,    -1,  1152,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     513,   514,    -1,    -1,    -1,    -1,   519,    -1,    -1,   513,
     514,    -1,    -1,    -1,    -1,  1178,    -1,    -1,   283,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   302,    -1,    -1,
      -1,    -1,  1205,    -1,    -1,    -1,    -1,   633,    -1,    -1,
      -1,   555,    -1,    -1,    -1,   559,  1787,   561,    -1,   572,
     646,    -1,   648,   649,    -1,   651,    -1,    -1,   654,    -1,
      -1,   657,   658,   659,    -1,    -1,   589,    -1,    -1,    -1,
      -1,  1812,    -1,    -1,    -1,    -1,    -1,    -1,  1251,  1252,
      -1,    -1,    -1,    -1,    -1,    -1,  1827,    -1,    -1,   612,
      -1,    -1,  1265,  1266,    -1,    -1,    -1,  1270,  1271,   374,
      -1,    -1,  1843,  1844,    -1,    -1,    -1,    -1,    -1,    -1,
     633,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   633,
      -1,    -1,    -1,    -1,    -1,  1298,  1299,    -1,    -1,    -1,
      -1,    -1,   646,   408,   648,   649,    -1,   651,  1879,   735,
     654,    -1,    -1,   657,   658,   659,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   434,
      -1,    -1,    -1,    -1,  1337,  1338,    -1,    -1,    -1,    -1,
    1911,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   711,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   735,  1386,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   735,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   752,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   513,   514,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   774,   775,    -1,    -1,    -1,   852,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   864,   865,
     793,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     555,    -1,    -1,    -1,   559,    -1,   561,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1481,  1482,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     0,    -1,    -1,     3,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   859,    -1,   852,    -1,
      -1,    -1,   865,    -1,  1517,    -1,    -1,    -1,    -1,    -1,
     864,   865,    -1,    -1,    -1,    -1,    -1,    -1,   633,    -1,
      -1,    -1,    -1,  1536,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   646,    -1,   648,   649,    -1,   651,    -1,    -1,   654,
      -1,    -1,   657,   658,   659,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1566,    -1,    -1,    -1,    -1,    76,    -1,
      -1,    -1,    -1,   926,  1577,    -1,    -1,    -1,    -1,    12,
      13,    14,    15,    16,    -1,  1588,    19,    -1,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    -1,    -1,  1618,    -1,    50,    51,    -1,
    1623,  1624,    -1,    -1,    -1,  1628,  1629,   135,    -1,    -1,
     735,    -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    86,    -1,    -1,    -1,    -1,    -1,    92,    93,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1097,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1105,
      -1,    -1,    -1,    -1,    -1,    -1,  1689,    -1,    -1,    -1,
      -1,    -1,   126,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1057,    -1,    -1,    -1,  1134,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   229,    -1,    -1,    -1,    -1,  1152,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   244,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1171,   254,   852,    -1,    -1,
      -1,    -1,    -1,  1097,    -1,    -1,   264,    -1,    -1,   864,
     865,  1105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     278,   279,    -1,    -1,    -1,    -1,    -1,   285,   286,    -1,
      -1,    -1,    -1,  1209,  1787,    -1,    -1,    -1,    -1,    -1,
    1134,    -1,    -1,   301,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1152,  1812,
      -1,   319,    -1,    -1,    -1,    -1,    -1,    -1,  1171,    -1,
      -1,    -1,    98,    -1,  1827,  1178,    -1,  1171,    -1,    -1,
      -1,    -1,    -1,   109,    -1,   111,    -1,   113,    -1,    -1,
    1843,  1844,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1205,    -1,    -1,    -1,    -1,    -1,   292,    -1,
      -1,    -1,    -1,    -1,    -1,  1209,    -1,   375,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   151,  1879,   153,   154,    -1,
      -1,    -1,  1308,  1309,  1310,  1311,  1312,  1313,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   406,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1911,    -1,
      -1,  1337,  1338,    -1,    -1,    -1,    -1,  1270,  1271,   195,
      -1,    -1,    -1,    -1,   432,    -1,    -1,    -1,   436,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   455,    -1,    -1,
      -1,   459,   460,    -1,    -1,   463,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1308,  1309,  1310,  1311,  1312,  1313,
     478,   479,   480,   481,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   258,  1409,   260,   261,    -1,    -1,    -1,   497,
      -1,    -1,  1097,  1337,  1338,    -1,    -1,   505,    -1,    -1,
    1105,    -1,    -1,   437,    -1,   439,    -1,    -1,    -1,    -1,
      -1,    -1,   288,    -1,   448,   449,    -1,    -1,   294,    -1,
      -1,    -1,    -1,    -1,    -1,   533,    -1,    -1,    -1,  1134,
      -1,    -1,    -1,  1386,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   322,  1152,    -1,    -1,
      -1,    -1,   328,    -1,   330,    -1,   564,    -1,    -1,    -1,
      -1,    -1,    -1,   571,    -1,  1409,  1171,    -1,   576,    -1,
      -1,    -1,    -1,    -1,    -1,  1501,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   599,   600,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1209,    -1,    -1,    -1,    -1,    -1,
    1536,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   556,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1566,    -1,   418,    -1,    -1,    -1,    -1,    -1,  1501,    -1,
      -1,    -1,   660,    -1,  1580,    -1,    -1,  1501,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   444,    -1,
     446,   447,    -1,    -1,    -1,    -1,    -1,  1603,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1536,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1308,  1309,  1310,  1311,  1312,  1313,    -1,
      -1,    -1,    -1,    -1,    -1,   491,    -1,    -1,    -1,   727,
      -1,    -1,  1566,    -1,  1577,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1337,  1338,   742,   511,  1580,    -1,   746,    -1,
     516,    -1,   518,    -1,    -1,    -1,    -1,   755,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1603,
      -1,    -1,   538,  1689,   540,   541,    -1,    -1,    -1,   777,
      -1,    -1,    -1,    -1,    -1,  1628,  1629,    -1,   786,    -1,
      -1,    -1,   558,    -1,   792,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   570,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1409,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   591,    -1,   593,   594,    -1,
      -1,   829,    -1,    -1,    -1,    -1,    -1,    -1,   836,    -1,
      -1,    -1,    -1,    -1,   768,    -1,    -1,    -1,    -1,   615,
     616,    -1,    -1,    -1,    -1,  1689,   622,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   862,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    47,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1501,    -1,    -1,    -1,
      -1,  1827,    -1,    -1,    -1,    73,    -1,    -1,   916,    -1,
      -1,   845,   846,    -1,    -1,    -1,    -1,  1843,    -1,    -1,
      -1,    -1,   856,   857,   858,    -1,    -1,   861,    -1,    -1,
      -1,  1536,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   119,    -1,  1879,    -1,    -1,    -1,    -1,    -1,  1812,
      -1,  1566,    -1,    -1,   132,    -1,   134,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1580,    -1,   178,    -1,    -1,
      -1,    -1,    -1,  1827,    -1,    -1,    -1,    -1,    -1,   997,
      -1,    -1,    -1,  1001,    -1,    -1,   164,    -1,  1603,  1843,
    1008,    -1,    -1,    -1,    -1,    -1,   940,    -1,    -1,    -1,
    1018,    -1,    -1,   181,    -1,    -1,    -1,  1025,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1034,    -1,  1036,    -1,
      -1,    -1,    -1,    -1,    -1,  1879,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     984,   219,    -1,    -1,    -1,   223,    -1,    -1,   226,   227,
    1068,    -1,   230,    -1,  1072,   233,   234,    -1,   844,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1086,   855,
      -1,  1089,    -1,    -1,  1689,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1031,    -1,   875,
      -1,    -1,    -1,    -1,    -1,    -1,  1040,  1041,  1042,  1043,
     886,    -1,    -1,    -1,  1048,  1049,    -1,    -1,    -1,   895,
      -1,    -1,    -1,    -1,  1058,    -1,    -1,   295,    -1,    -1,
     298,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    47,    -1,
      -1,    -1,    -1,    -1,    -1,  1079,    -1,  1081,    -1,    -1,
     318,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   359,    -1,
      -1,   362,   363,    -1,    -1,   333,    -1,    -1,  1176,    -1,
      -1,   372,   373,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   387,   388,    -1,    -1,
      -1,    -1,  1200,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1134,    -1,    -1,    -1,    -1,    -1,    -1,   408,    -1,    -1,
     119,    -1,    -1,    -1,    -1,   991,    -1,    -1,    -1,    -1,
      -1,    -1,  1827,   132,    -1,   134,  1160,    -1,    -1,    -1,
      -1,    -1,  1166,    -1,  1168,  1169,    -1,   438,  1843,  1015,
      -1,    -1,    -1,  1177,    -1,  1179,    -1,  1181,    -1,  1183,
      -1,    -1,    -1,    -1,  1188,    -1,    -1,    -1,    -1,   427,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1879,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1290,    -1,    -1,    -1,  1294,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1246,    -1,   482,    -1,  1324,   226,   227,  1253,
    1254,   230,    -1,    -1,   233,   234,   494,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1277,    -1,    -1,    -1,    -1,    -1,    -1,
    1284,    -1,    -1,  1287,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1374,    -1,    -1,  1377,
      -1,    -1,    -1,    -1,    -1,  1151,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1317,  1392,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     568,    -1,    -1,    -1,    -1,    -1,  1182,    -1,    -1,   318,
      -1,    -1,  1188,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1357,   333,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1442,    -1,    -1,    -1,    -1,   607,
     608,    -1,    -1,  1451,    -1,    -1,    -1,  1455,    -1,    -1,
      -1,    -1,   620,    -1,    -1,    -1,  1390,    -1,    -1,    -1,
      -1,  1469,  1470,    -1,  1398,    -1,  1400,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   688,   689,   690,
     691,   692,   693,   694,   695,   696,   697,   698,   699,   700,
     701,   702,   703,   704,   705,   706,    -1,    -1,    -1,    -1,
      -1,    -1,  1446,  1447,    -1,    -1,    -1,    -1,   427,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1461,  1462,    -1,
    1464,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1473,
      -1,    -1,    -1,  1319,    -1,    -1,    -1,    -1,    -1,  1483,
    1484,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   731,   732,    -1,    -1,   768,    -1,   737,
      -1,    -1,    -1,  1581,  1582,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   494,    -1,    -1,    -1,    -1,
     758,  1367,    -1,   761,   762,    -1,   764,    82,   766,   767,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    98,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   805,    -1,    -1,
      -1,   809,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1589,  1590,    -1,    -1,    -1,
      -1,    -1,   147,    -1,    -1,  1599,   151,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   162,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1699,    -1,    -1,   179,    -1,    -1,    -1,   607,   608,
    1634,  1635,    -1,    -1,    -1,    -1,    -1,   192,    -1,    -1,
     195,   620,    -1,  1721,    -1,    -1,   884,    -1,    -1,    -1,
      -1,   922,    -1,    -1,    -1,    -1,   927,    -1,    -1,    -1,
      -1,  1739,    -1,    -1,    -1,    -1,    -1,   938,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1766,  1535,
     245,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   258,    -1,  1783,    -1,   978,  1786,  1713,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1731,    -1,    -1,
    1734,  1735,    -1,    -1,    -1,    -1,    -1,  1741,    -1,   294,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   302,    -1,    -1,
      -1,    -1,   731,   732,    -1,    -1,    -1,    -1,   737,    -1,
      -1,    -1,    -1,    -1,    -1,   320,    -1,   322,    -1,    -1,
      -1,    55,    56,    -1,    -1,    -1,  1014,    -1,    -1,   758,
      -1,    -1,   761,   762,    -1,   764,    -1,   766,   767,    -1,
      -1,    -1,  1870,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1661,    -1,    -1,    -1,   374,
      -1,    -1,    -1,    -1,  1062,    -1,   805,  1065,    -1,    -1,
     809,    -1,  1678,    -1,  1105,    -1,    -1,    -1,    -1,    -1,
      -1,  1845,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   408,    -1,    -1,   140,    -1,    -1,   143,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   156,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   438,    -1,    -1,    -1,  1891,    -1,   444,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   884,  1752,  1178,    -1,    -1,
      -1,  1915,    -1,    -1,    -1,    -1,    -1,    -1,  1922,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   210,    -1,    -1,    -1,
      -1,    -1,    -1,  1937,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1212,  1213,  1214,    -1,    -1,  1184,    -1,  1219,  1220,
      -1,    -1,    -1,    -1,  1192,  1193,    -1,    -1,   513,   514,
      -1,    -1,    -1,    -1,   519,    -1,    -1,    -1,    -1,    -1,
    1241,   255,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   265,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   276,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1847,    -1,    -1,    -1,    -1,    -1,  1278,  1279,  1247,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   572,  1256,   303,
      -1,  1259,    -1,  1261,  1262,   309,   310,    -1,    -1,    -1,
     314,    -1,    -1,    -1,   589,  1014,   591,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   612,    -1,    -1,
      -1,    -1,   346,  1301,    -1,    -1,    -1,   351,    -1,    -1,
     354,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   633,    -1,
    1926,    -1,    -1,  1062,    -1,    -1,  1065,    -1,    -1,    -1,
      -1,   646,    -1,   648,   649,    -1,   651,    -1,    -1,   654,
      -1,    -1,   657,   658,   659,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1368,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   711,   441,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   453,
     454,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     735,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   752,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1184,    -1,    -1,    -1,    -1,
      -1,    -1,  1450,  1192,  1193,    -1,    -1,    -1,    -1,   774,
     775,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   793,    -1,
      -1,  1479,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1505,  1247,    -1,
      -1,    -1,    -1,  1511,    -1,    -1,    -1,  1256,    -1,    -1,
    1259,    -1,  1261,  1262,    -1,   162,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   583,
    1571,    -1,    -1,    -1,   859,    -1,    -1,    -1,    -1,    -1,
     865,    -1,    -1,    -1,   191,   192,    -1,    -1,    -1,    -1,
     875,    -1,  1301,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    48,    -1,    -1,  1576,    52,
      -1,    54,    -1,    -1,    -1,    -1,   223,   631,    -1,    -1,
      -1,    -1,    -1,   230,    -1,    -1,    -1,    -1,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   926,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    98,    99,    -1,   101,  1368,
     103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,    -1,   117,   118,   119,    -1,   121,   122,
      -1,    -1,  1650,  1651,    -1,    -1,   129,    -1,    -1,    -1,
    1658,   298,    -1,    -1,  1662,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   149,    -1,    -1,   152,
     153,    -1,    -1,   320,   321,    -1,   159,   160,   161,   162,
     163,   164,   165,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     173,   174,    -1,    -1,   341,    -1,    -1,    -1,    -1,    -1,
      -1,  1450,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   769,    -1,    -1,    -1,    -1,
      -1,    -1,   776,  1764,    -1,    -1,    -1,    -1,    -1,    -1,
    1479,    -1,  1057,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   179,    -1,    -1,    -1,  1754,    -1,    -1,    -1,
      -1,    -1,    -1,  1794,    -1,   192,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   411,    -1,    -1,    -1,   205,    -1,
     207,    -1,  1097,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     427,   428,    -1,   430,   431,    -1,    -1,    -1,    -1,    -1,
      -1,   438,    -1,    -1,    -1,   442,    -1,   851,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1816,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1151,  1576,    -1,    -1,
      -1,    -1,    -1,    -1,  1875,  1876,   483,    -1,    -1,    -1,
     487,    -1,    -1,    -1,    -1,    -1,  1171,   901,    -1,    -1,
      -1,    -1,   906,  1178,    -1,    -1,   293,    -1,    -1,  1900,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   519,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1205,    -1,    -1,  1924,  1209,  1893,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1650,  1651,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1662,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   569,    -1,    -1,   572,    -1,  1968,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   588,   589,    -1,    -1,  1270,  1271,    -1,    -1,    -1,
      -1,    -1,   599,    -1,    -1,    -1,   603,    -1,    -1,    -1,
      -1,    -1,    -1,   610,    -1,   612,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1308,  1309,  1310,    -1,  1312,  1313,    -1,
      -1,    -1,    -1,    -1,  1319,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1754,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   464,    -1,    -1,
      -1,    -1,    -1,   470,    -1,    -1,    -1,    -1,   475,    -1,
      -1,    -1,  1367,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1110,  1111,  1112,    -1,
      -1,  1386,    -1,    -1,   711,    -1,    -1,  1816,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     727,   728,    -1,    -1,    -1,    -1,  1140,    -1,    -1,    -1,
     737,   738,    -1,   740,   741,    -1,    -1,    -1,    -1,    -1,
      -1,  1155,    -1,    -1,    -1,   752,    -1,    -1,   755,    -1,
     757,   758,    -1,    -1,    -1,    -1,    -1,   764,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   562,    -1,   774,   775,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1191,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   793,    -1,    -1,    -1,
     797,    -1,   589,    -1,   801,    -1,    -1,    -1,   805,   806,
      -1,    -1,   809,   810,    -1,   602,    -1,    -1,    -1,    -1,
     817,    -1,    -1,    -1,    -1,    -1,  1501,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1535,    -1,   859,   860,    -1,    -1,   653,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   671,   672,    -1,    -1,    -1,   886,
      -1,    -1,    -1,    -1,   681,    -1,   683,   684,    -1,    -1,
      -1,    -1,  1577,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1322,    -1,
      -1,  1325,    -1,    -1,   711,    -1,    -1,    -1,  1603,   926,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   724,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   735,    -1,
      -1,    -1,    -1,  1628,  1629,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   749,    -1,    -1,   752,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   779,    -1,   991,   782,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1678,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1008,  1009,    -1,    -1,    -1,    -1,    -1,  1015,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   818,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1057,    -1,    -1,    -1,    -1,  1062,  1063,    -1,  1065,  1066,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   865,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   875,   876,
      -1,    -1,    -1,    -1,    -1,    -1,   883,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1514,    -1,  1787,    -1,    -1,    -1,    -1,   904,    -1,    -1,
      -1,    -1,    -1,    -1,     5,    -1,   913,    -1,    -1,    -1,
      -1,    12,    13,    14,    15,    16,    -1,  1812,    -1,   926,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   934,    -1,    -1,
      -1,    -1,    -1,    -1,   941,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    -1,  1844,
      -1,    52,  1847,    54,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1178,    -1,    -1,    -1,    -1,    -1,  1184,  1185,    70,
      71,    -1,  1596,    -1,    -1,    -1,    -1,    -1,   985,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1205,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    98,    99,    -1,
     101,    -1,   103,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,   117,   118,   119,    -1,
     121,   122,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,
    1247,  1248,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1256,
    1257,    -1,  1259,    -1,    -1,  1052,    -1,  1054,   149,  1056,
      -1,   152,   153,  1270,  1271,    -1,    -1,    -1,   159,   160,
     161,   162,   163,   164,   165,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1710,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1719,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1122,  1123,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     5,    -1,    -1,    -1,    -1,    -1,    -1,    12,    13,
      14,    15,    16,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1386,
      -1,    -1,    -1,    -1,    -1,  1182,    -1,    -1,    -1,    -1,
      -1,  1188,    -1,    -1,    48,    -1,    -1,    -1,    52,    -1,
      54,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1205,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1222,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1235,    -1,
      -1,  1238,    -1,    -1,    98,    99,    -1,   101,    -1,   103,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,   117,   118,   119,    -1,   121,   122,    -1,
      -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1495,    -1,
      -1,    -1,  1289,    -1,    -1,   149,    -1,    -1,   152,   153,
      -1,    -1,    -1,    -1,  1511,   159,   160,   161,   162,   163,
     164,   165,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1327,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1345,    -1,
      -1,  1348,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1577,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1386,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1396,
    1397,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1628,  1629,    -1,  1421,    -1,  1423,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     3,  1643,  1644,    -1,    -1,
      -1,     9,    -1,    -1,    12,    13,    14,    15,    16,    17,
      -1,    19,  1659,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    -1,    -1,
      -1,    -1,    50,    51,    -1,    -1,    -1,    55,    56,    57,
      58,    59,    60,    61,    62,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    70,    71,  1501,    -1,    -1,    -1,    -1,  1506,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   100,    -1,    -1,    -1,   104,   105,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1752,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1760,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   129,    -1,    -1,    48,  1562,    -1,    -1,    52,    -1,
      54,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     148,    -1,    -1,    -1,   152,   153,    -1,    71,    -1,    -1,
      -1,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1601,  1812,    -1,  1604,    -1,  1816,
    1817,    -1,    -1,  1820,    98,    99,    -1,   101,    -1,   103,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,   117,   118,   119,    -1,   121,   122,  1846,
      -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   149,   150,    -1,   152,   153,
      -1,    -1,    -1,   157,    -1,   159,   160,   161,   162,   163,
     164,   165,    -1,    -1,    -1,    -1,  1893,  1894,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,  1926,
      19,    -1,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    -1,    -1,    48,
      -1,    50,    51,    52,    -1,    54,    55,    56,    57,    58,
      59,    60,    61,    62,    63,    -1,    -1,    -1,    67,    -1,
      69,    70,    71,    72,    -1,    74,    -1,    -1,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,    87,    88,
      89,    90,    91,    92,    93,    94,    -1,    96,  1795,    98,
      99,   100,   101,    -1,   103,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,    -1,   121,   122,    -1,    -1,    -1,    -1,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   148,
     149,    -1,    -1,   152,   153,    -1,    -1,    -1,   157,    -1,
     159,   160,   161,   162,   163,   164,   165,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   174,     3,     4,     5,     6,
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
      -1,    -1,   149,    -1,    -1,   152,   153,    -1,    -1,    -1,
     157,    -1,   159,   160,   161,   162,   163,   164,   165,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   174,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    -1,    19,    -1,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    -1,    -1,    48,    -1,    50,    51,    52,    -1,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    -1,
      -1,    -1,    67,    -1,    -1,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    98,    99,   100,   101,    -1,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,   117,   118,   119,    -1,   121,   122,    -1,    -1,
      -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   149,    -1,   151,   152,   153,    -1,
      -1,    -1,    -1,    -1,   159,   160,   161,   162,   163,   164,
     165,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   173,   174,
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
     163,   164,   165,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     173,   174,     3,     4,     5,     6,     7,     8,     9,    10,
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
     161,   162,   163,   164,   165,     3,     4,     5,     6,     7,
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
     160,   161,   162,   163,   164,   165,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,    19,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    -1,
      -1,    -1,    -1,    50,    51,    -1,    -1,    -1,    55,    56,
      57,    58,    59,    60,    61,    62,    63,    -1,    -1,    -1,
      67,    -1,    -1,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    96,
      -1,    -1,    -1,   100,   101,    -1,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    -1,    -1,    -1,   116,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   149,   150,    -1,   152,   153,    -1,    -1,    -1,
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
      -1,   149,   150,    -1,   152,   153,    -1,    -1,    -1,   157,
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
      -1,    -1,    -1,    -1,    -1,    -1,    69,    -1,    71,    72,
      -1,    74,    -1,    -1,    77,    78,    79,    80,    81,    82,
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
      56,    57,    58,    59,    60,    61,    62,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,    -1,    -1,    -1,
      12,    13,    14,    15,    16,    -1,    -1,    19,    -1,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,   152,    49,    50,    51,
      -1,    53,    -1,    -1,    48,    -1,    -1,    -1,    52,    -1,
      54,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   104,   105,    98,    99,    -1,   101,    -1,   103,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,   117,   118,   119,    48,   121,   122,    -1,
      52,    -1,    54,    -1,    -1,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,
     152,    -1,    -1,    -1,    -1,   149,   150,    -1,   152,   153,
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
     329,   330,   341,   344,   377,   380,   390,   395,   397,   403,
     407,   412,   413,   414,   415,   416,   417,   418,   419,   439,
     456,   457,   458,   459,     0,   176,   180,   196,   281,   283,
     292,   295,   307,   311,   316,   115,   149,    56,    59,    60,
      62,   149,   149,   401,   402,   403,   303,   304,   104,   105,
     180,   357,   378,   379,   357,   149,   390,   149,   149,   149,
     196,   402,   407,   413,   414,   415,   417,   418,   419,   104,
     318,   154,   176,   284,   292,   295,   412,   416,   455,   456,
     459,   460,   174,   177,   146,   157,   173,   217,   360,    87,
     155,   396,   357,   177,   177,   177,   174,   104,   105,   149,
     196,   289,   398,   407,   408,   409,   410,   411,   412,   416,
     420,   421,   422,   423,   424,   430,     3,    46,    47,    49,
      53,   309,     3,     4,   153,   196,   283,   296,   300,   302,
     312,   317,   392,   412,   416,   459,   281,   283,   295,   307,
     311,   316,   393,   412,   416,    63,   301,   301,   296,   302,
     301,   296,   301,   296,   152,   401,   155,   177,   149,   157,
     225,   401,   401,   176,   272,   273,   153,   292,   295,   457,
     357,   357,   390,   173,   295,   149,   196,   398,   407,   412,
     421,   153,   196,   459,   391,    63,    64,    65,    66,   153,
     171,   357,   366,   368,   372,   374,   375,   317,    55,   153,
     196,   291,   295,   299,   300,   306,   307,   313,   314,   315,
     316,   320,   327,   328,   344,   353,   355,   439,   451,   452,
     453,   454,   459,   460,   104,   105,   157,   180,   317,   430,
     403,   149,   373,   374,   149,   149,   115,   182,   183,    48,
      52,    54,    71,    98,    99,   101,   103,   113,   114,   117,
     118,   119,   121,   122,   149,   153,   159,   162,   163,   164,
     165,   178,   179,   182,   184,   187,   195,   196,   197,   198,
     201,   202,   203,   204,   205,   206,   207,   208,   209,   210,
     211,   212,   213,   219,   317,   151,   153,   195,   196,   212,
     214,   292,   317,   358,   359,   376,   455,   460,   295,   413,
     414,   415,   417,   418,   419,   151,   151,   151,   151,   151,
     151,   151,   153,   292,   439,   457,   153,   160,   196,   214,
     283,   284,   291,   293,   295,   307,   314,   316,   348,   349,
     352,   353,   354,   451,   459,   149,   412,   416,   459,   149,
     155,   101,   152,   153,   157,   179,   181,   214,   361,   362,
     363,   364,   365,    21,   361,   149,   357,   225,   149,   155,
     155,   155,   402,   407,   409,   410,   411,   420,   422,   423,
     424,   295,   408,   421,   155,    96,   400,   153,   401,   438,
     439,   401,   401,   396,   272,   149,   401,   438,   396,   401,
     401,   295,   398,   149,   149,   294,   295,   292,   295,   176,
     292,   455,   460,   319,   157,   396,   272,   357,   360,   283,
     300,   394,   412,   416,   157,   396,   272,   378,   295,   307,
     295,   295,   104,   318,   104,   105,   180,   317,   322,   378,
     176,   180,   356,   148,   176,     3,   288,   290,   295,   299,
     225,   176,   176,   400,   149,   400,   177,   214,   402,   407,
     295,   149,   176,   357,   157,   357,   157,   357,   131,   160,
     161,   371,   151,   155,   357,   375,   151,   401,   154,   176,
     293,   295,   307,   314,   316,   450,   451,   459,   460,   149,
     153,   161,   173,   196,   439,   440,   441,   442,   443,   444,
     445,   462,   196,   320,   459,   295,   314,   301,   296,   401,
     151,   293,   295,   452,   293,   439,   452,     9,   345,   357,
     342,   157,   366,   173,   366,    12,    86,   101,   104,   105,
     179,   404,   405,   406,   151,   115,   149,   195,   149,   149,
     198,   149,   195,   149,   149,   195,   195,    18,    20,    83,
     153,   162,   163,   199,   200,   214,   221,   225,   330,   358,
     459,   155,   176,   149,   184,   158,   158,   118,   120,   121,
     122,   149,   152,   153,   157,   158,   198,   198,   166,   160,
     167,   168,   162,   163,   123,   124,   125,   126,   169,   170,
     127,   128,   161,   159,   171,   129,   130,   172,   151,   155,
     152,   176,   132,   133,   134,   135,   136,   137,   138,   139,
     140,   141,   142,   173,   216,   217,   218,   149,   196,   434,
     435,   436,   437,   438,   151,   155,   151,   151,   151,   151,
     151,   151,   149,   401,   438,   439,   149,   438,   439,   176,
     292,   457,   176,   177,   177,   149,   161,   196,   407,   425,
     426,   427,   428,   429,   430,   431,   432,   433,   131,   459,
     177,   177,   357,   357,   176,   176,   176,   153,   181,   176,
     362,   156,   155,   461,   361,   152,   153,   156,   365,   150,
     214,   220,   149,   176,   176,   176,   176,   407,   409,   410,
     411,   420,   422,   423,   424,   151,   151,   151,   151,   151,
     151,   151,   408,   421,   401,   149,   360,   154,   176,   225,
     396,   176,   225,   398,   221,   359,   221,   359,   398,   388,
     225,   396,   400,   157,   396,   272,   388,   225,   396,   324,
     325,   323,   157,   131,   295,   350,   351,   354,   355,   151,
     155,    68,   274,   275,   177,   295,   288,   160,   214,   176,
     407,   349,   388,   154,   176,   149,   370,   368,   369,    76,
     305,   180,   293,   439,   452,   295,   299,   459,   176,   441,
     442,   443,   154,   176,    17,   214,   295,   440,   462,   401,
     401,   439,   293,   450,   460,   295,   180,   401,   293,   452,
     317,   155,   461,   173,   217,   346,   157,   345,   151,   359,
     151,   151,   155,   149,   174,   358,   153,   358,   358,   358,
     214,   358,   151,   358,   358,   358,   176,   151,   162,   163,
     200,    17,   297,   151,   155,   151,   160,   161,   151,   220,
     214,   157,   180,   180,   113,   153,   180,   150,   188,   189,
     190,   214,   113,   153,   180,   330,   214,   188,   180,   198,
     201,   201,   201,   202,   202,   203,   203,   204,   204,   204,
     204,   205,   205,   206,   207,   208,   209,   210,   156,   221,
     174,   182,   153,   180,   214,   157,   214,   176,   435,   436,
     437,   295,   434,   401,   401,   214,   359,   149,   401,   438,
     439,   149,   438,   439,   176,   176,   154,   154,   149,   407,
     426,   427,   428,   431,    17,   295,   425,   429,   149,   401,
     444,   462,   401,   401,   462,   149,   401,   444,   401,   401,
     177,   213,   357,   154,   155,   154,   155,   462,   462,   131,
     347,   348,   349,   347,   357,   176,   212,   213,   214,   399,
     461,   361,   363,   148,   176,   151,   155,   176,   347,   180,
     398,   180,   151,   151,   151,   151,   151,   151,   149,   401,
     438,   439,   149,   401,   438,   439,   398,   182,   439,   214,
     225,   350,   151,   151,   151,   151,   386,   387,   225,   388,
     225,   396,   387,   225,   157,   157,   157,   331,   177,   177,
     180,   276,   357,    17,    69,    71,    74,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    90,
      91,    92,    93,    94,    96,   104,   105,   116,   176,   221,
     222,   223,   224,   225,   226,   227,   229,   230,   240,   244,
     245,   246,   247,   248,   249,   254,   255,   261,   262,   263,
     277,   295,   299,   357,   397,    68,   174,   177,   177,   177,
     347,   177,   387,   281,   283,   292,   381,   382,   383,   384,
     376,   173,   367,   367,   293,   452,   153,   160,   196,   214,
     317,   214,   295,   350,   151,   151,   151,     5,   295,   401,
     440,   157,   180,   430,     9,   357,   148,   361,   345,   461,
     157,   151,   405,   188,   151,   176,   155,   151,   151,   155,
     151,   198,   151,   151,   151,   198,    17,   297,   214,   151,
     151,   150,   157,   198,   154,   177,   188,   113,   117,   119,
     181,   191,   192,   193,   151,   155,   191,   154,   155,   148,
     212,   156,   151,   191,   177,   362,   350,   151,   151,   151,
     434,   176,   176,   350,   350,   431,   151,   151,   151,   151,
     149,   407,   430,   425,   429,   176,   176,   154,   177,   462,
     176,   176,   177,   177,   177,   177,   360,   191,   131,   165,
     177,   177,   148,   361,   214,   150,   214,   347,   177,   173,
     149,   401,   438,   439,   149,   401,   438,   439,   176,   176,
     400,   151,   177,   177,   389,   387,   225,   389,   331,   331,
     331,     3,     9,    71,   148,   278,   285,   286,   292,   295,
     332,   337,   455,   151,   155,   155,   174,   149,    59,    60,
     174,   225,   277,   397,   149,    17,   223,   149,   149,   174,
     357,   174,   357,   160,   357,   157,   222,   149,   149,   149,
     225,   214,   215,   215,    13,   264,    72,   231,   174,   177,
     227,    76,   174,   357,    89,   250,   356,   295,   156,   276,
     174,   154,   154,   177,   155,   389,   398,   177,   174,   177,
     174,   177,   151,   359,   373,   373,   176,   177,   177,   177,
     214,   177,   149,   401,   444,   439,   294,     5,   160,   177,
     214,   345,   401,   401,   317,   346,   461,   148,   148,   176,
     151,   180,    76,   185,   186,   358,   198,   198,   198,   198,
     198,   157,   362,   155,   148,   194,   153,   192,   194,   194,
     154,   155,   120,   152,   190,   154,   220,   212,   174,   154,
     461,   177,   149,   401,   438,   439,   350,   350,   177,   177,
     151,   149,   401,   438,   439,   149,   401,   444,   407,   401,
     401,   350,   350,   154,   349,   352,   352,   353,   151,   155,
     155,   151,   177,   213,   213,   154,   154,   177,   177,   151,
     214,   176,   176,   350,   350,   360,   401,   155,   151,   148,
     389,   148,   148,   148,   148,   292,   330,   338,   455,   292,
     337,   149,   326,   174,   174,   149,   156,   196,   333,   334,
     340,   407,   408,   421,   155,   174,   357,   176,   357,   151,
     188,   189,   174,   225,   174,   225,   221,    78,   151,   221,
     232,   277,   279,   282,   288,   295,   299,   151,   173,   174,
     221,   241,   242,   277,   174,   174,   221,   174,   362,   174,
     221,   220,   221,   108,   109,   110,   111,   112,   256,   258,
     259,   174,    95,   174,    82,   149,   149,   177,   148,   174,
     174,   149,   223,   225,   401,   174,   151,   176,   148,   148,
     176,   155,   155,   154,   154,   154,   177,   151,   176,   214,
     214,   177,   154,   177,   461,   343,   157,   346,   148,   381,
     151,   156,   151,   155,   156,   362,   461,   220,   118,   191,
     192,   153,   192,   153,   192,   154,   148,   151,   176,   177,
     177,   151,   151,   176,   176,   177,   177,   177,   176,   176,
     154,   177,   151,   401,   350,   350,   177,   177,   221,   148,
     326,   326,   326,   149,   196,   335,   336,   438,   446,   447,
     448,   449,   174,   155,   174,   333,   174,   376,   402,   407,
     214,   295,   155,   174,   339,   340,   339,   357,   131,   354,
     355,   221,   151,   151,   149,   223,   151,   221,   295,   223,
     221,   222,   143,   144,   145,   165,   174,   243,   151,   156,
     222,   174,   461,   151,   151,   151,   225,   258,   259,   149,
     214,   149,   182,   232,   198,   251,   107,   223,   401,   382,
     176,   176,   154,   350,   177,   177,   154,   154,   148,   157,
     345,   177,   214,   186,   214,   461,   148,   154,   154,   191,
     191,   350,   151,   151,   350,   350,   151,   151,   154,   155,
     131,   349,   131,   154,   177,   177,   151,   151,   154,   447,
     448,   449,   295,   446,   155,   174,   401,   401,   174,   151,
     407,   401,   174,   223,    75,    76,   157,   235,   236,   237,
     151,   221,   223,   174,   104,   173,   221,   222,   221,   223,
     242,   174,   148,   157,   237,   223,   149,   176,   174,   182,
     151,   156,   151,   151,   155,   156,   249,   253,   357,   398,
     177,   154,   154,   345,   461,   148,   148,   154,   154,   177,
     177,   177,   176,   177,   151,   151,   151,   151,   151,   446,
     401,   334,   213,   233,   234,   399,   156,   176,   223,   235,
     174,   151,    73,   222,   221,   144,   165,   243,   174,   165,
      73,   222,   174,   176,   176,   260,   293,   295,   455,   156,
     174,   153,   182,   265,   266,   267,   223,   198,   188,    73,
     106,   250,   252,   151,   461,   148,   151,   151,   151,   352,
     149,   401,   438,   439,   336,   131,   155,   156,   270,   271,
     277,    73,   174,   223,   150,   150,   221,   222,   221,   223,
     270,   260,   177,   149,   196,   398,   446,   180,   156,   101,
     149,   151,   156,   155,    73,   151,   223,   149,   223,   223,
     148,   176,   213,   233,   236,   238,   239,   277,   223,   165,
     165,   165,   238,   177,   174,   257,   295,   265,   154,   213,
     174,   265,   267,   223,   221,   107,   107,   350,   223,   228,
     177,   236,   221,   150,   221,   221,   177,   257,   212,   151,
     156,   182,   151,   151,   156,   151,   253,    73,   248,   177,
     223,   148,   228,   148,   151,   225,   182,   268,   149,   174,
     268,   223,    73,   151,   225,   155,   156,   213,   151,   223,
     182,   180,   269,   151,   174,   151,   155,   174,   180
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
     390,   395,   395,   395,   396,   396,   397,   397,   397,   397,
     397,   397,   397,   397,   397,   397,   398,   398,   398,   399,
     400,   400,   401,   401,   402,   402,   403,   404,   404,   405,
     405,   405,   406,   406,   406,   406,   406,   406,   407,   407,
     408,   408,   408,   408,   409,   409,   409,   409,   410,   410,
     410,   410,   410,   410,   410,   411,   411,   411,   411,   412,
     412,   412,   413,   413,   413,   413,   413,   414,   414,   414,
     414,   415,   415,   415,   415,   415,   415,   416,   416,   416,
     417,   417,   417,   417,   417,   418,   418,   418,   418,   419,
     419,   419,   419,   419,   419,   420,   420,   421,   421,   421,
     421,   422,   422,   422,   422,   423,   423,   423,   423,   423,
     423,   423,   424,   424,   424,   424,   424,   425,   425,   425,
     425,   425,   426,   426,   426,   427,   427,   427,   427,   428,
     428,   428,   429,   429,   429,   429,   429,   430,   430,   431,
     431,   431,   432,   432,   433,   433,   434,   434,   434,   435,
     435,   435,   435,   435,   436,   436,   436,   436,   437,   437,
     437,   438,   438,   438,   438,   439,   439,   439,   439,   440,
     440,   440,   440,   441,   441,   441,   441,   441,   442,   442,
     442,   442,   443,   443,   443,   444,   444,   444,   445,   445,
     445,   445,   445,   445,   446,   446,   446,   447,   447,   447,
     447,   447,   448,   448,   448,   448,   449,   449,   450,   450,
     450,   451,   451,   452,   452,   452,   452,   452,   452,   453,
     453,   453,   453,   453,   453,   453,   453,   453,   453,   454,
     454,   454,   454,   455,   455,   455,   456,   456,   457,   457,
     457,   457,   457,   457,   458,   458,   458,   458,   458,   458,
     459,   459,   459,   460,   460,   461,   461,   462,   462
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
#line 529 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.enterScope(); }
#line 6891 "Parser/parser.cc"
    break;

  case 3:
#line 533 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 6897 "Parser/parser.cc"
    break;

  case 4:
#line 540 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantInteger( *(yyvsp[0].tok) ) ); }
#line 6903 "Parser/parser.cc"
    break;

  case 5:
#line 541 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 6909 "Parser/parser.cc"
    break;

  case 6:
#line 542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 6915 "Parser/parser.cc"
    break;

  case 7:
#line 543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 6921 "Parser/parser.cc"
    break;

  case 8:
#line 544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantChar( *(yyvsp[0].tok) ) ); }
#line 6927 "Parser/parser.cc"
    break;

  case 19:
#line 565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 6933 "Parser/parser.cc"
    break;

  case 20:
#line 569 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.constant) = build_constantStr( *(yyvsp[0].str) ); }
#line 6939 "Parser/parser.cc"
    break;

  case 21:
#line 573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 6945 "Parser/parser.cc"
    break;

  case 22:
#line 575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 6955 "Parser/parser.cc"
    break;

  case 23:
#line 586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 6961 "Parser/parser.cc"
    break;

  case 24:
#line 588 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 6967 "Parser/parser.cc"
    break;

  case 25:
#line 592 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_dimensionref( (yyvsp[0].tok) ) ); }
#line 6973 "Parser/parser.cc"
    break;

  case 27:
#line 595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 6979 "Parser/parser.cc"
    break;

  case 28:
#line 597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new StmtExpr( dynamic_cast<CompoundStmt *>(maybeMoveBuild<Statement>((yyvsp[-1].sn)) ) ) ); }
#line 6985 "Parser/parser.cc"
    break;

  case 29:
#line 599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.en) = nullptr; }
#line 6991 "Parser/parser.cc"
    break;

  case 30:
#line 601 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.en) = nullptr; }
#line 6997 "Parser/parser.cc"
    break;

  case 31:
#line 603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild<Expression>( (yyvsp[-3].en) );
			(yyval.en) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 7007 "Parser/parser.cc"
    break;

  case 33:
#line 617 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// steal the association node from the singleton and delete the wrapper
			(yyvsp[-2].genexpr)->associations.splice((yyvsp[-2].genexpr)->associations.end(), (yyvsp[0].genexpr)->associations);
			delete (yyvsp[0].genexpr);
			(yyval.genexpr) = (yyvsp[-2].genexpr);
		}
#line 7018 "Parser/parser.cc"
    break;

  case 34:
#line 627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuildType((yyvsp[-2].decl)), maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } );
		}
#line 7027 "Parser/parser.cc"
    break;

  case 35:
#line 632 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } ); }
#line 7033 "Parser/parser.cc"
    break;

  case 37:
#line 642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-5].en), new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) ) ) )) ) ); }
#line 7039 "Parser/parser.cc"
    break;

  case 38:
#line 648 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7045 "Parser/parser.cc"
    break;

  case 39:
#line 650 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.en) = new ExpressionNode( new ConstructorExpr( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) ) );
		}
#line 7055 "Parser/parser.cc"
    break;

  case 40:
#line 656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7061 "Parser/parser.cc"
    break;

  case 41:
#line 658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 7067 "Parser/parser.cc"
    break;

  case 42:
#line 660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 7073 "Parser/parser.cc"
    break;

  case 43:
#line 662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), new ExpressionNode( (yyvsp[-2].constant) ) ) ); }
#line 7079 "Parser/parser.cc"
    break;

  case 44:
#line 664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7085 "Parser/parser.cc"
    break;

  case 45:
#line 666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 7091 "Parser/parser.cc"
    break;

  case 46:
#line 668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-1].en), build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) ) ) ); }
#line 7097 "Parser/parser.cc"
    break;

  case 47:
#line 670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7103 "Parser/parser.cc"
    break;

  case 48:
#line 672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[0].aggKey), (yyvsp[-2].en) ) ); }
#line 7109 "Parser/parser.cc"
    break;

  case 49:
#line 674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7115 "Parser/parser.cc"
    break;

  case 50:
#line 676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 7121 "Parser/parser.cc"
    break;

  case 51:
#line 678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7127 "Parser/parser.cc"
    break;

  case 52:
#line 680 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::IncrPost, (yyvsp[-1].en) ) ); }
#line 7133 "Parser/parser.cc"
    break;

  case 53:
#line 682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::DecrPost, (yyvsp[-1].en) ) ); }
#line 7139 "Parser/parser.cc"
    break;

  case 54:
#line 684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].in), true ) ) ); }
#line 7145 "Parser/parser.cc"
    break;

  case 55:
#line 686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].in), true ))->set_maybeConstructed( false ) ) ); }
#line 7151 "Parser/parser.cc"
    break;

  case 56:
#line 688 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) );
		}
#line 7161 "Parser/parser.cc"
    break;

  case 57:
#line 697 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7167 "Parser/parser.cc"
    break;

  case 60:
#line 704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7173 "Parser/parser.cc"
    break;

  case 61:
#line 709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Default parameter for argument is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7179 "Parser/parser.cc"
    break;

  case 64:
#line 716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7185 "Parser/parser.cc"
    break;

  case 66:
#line 722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-1].tok) ) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7191 "Parser/parser.cc"
    break;

  case 67:
#line 724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-3].tok) ) ), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7197 "Parser/parser.cc"
    break;

  case 68:
#line 726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7203 "Parser/parser.cc"
    break;

  case 69:
#line 728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7209 "Parser/parser.cc"
    break;

  case 70:
#line 730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7215 "Parser/parser.cc"
    break;

  case 71:
#line 732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7221 "Parser/parser.cc"
    break;

  case 72:
#line 737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_constantInteger( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 7227 "Parser/parser.cc"
    break;

  case 73:
#line 739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_field_name_FLOATINGconstant( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 7233 "Parser/parser.cc"
    break;

  case 74:
#line 741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_varref( (yyvsp[-1].tok) ), (yyvsp[0].en) ) );
		}
#line 7241 "Parser/parser.cc"
    break;

  case 75:
#line 748 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7247 "Parser/parser.cc"
    break;

  case 76:
#line 750 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Expression * constant = build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) );
			(yyval.en) = (yyvsp[-1].en) != nullptr ? new ExpressionNode( build_fieldSel( (yyvsp[-1].en),  constant ) ) : new ExpressionNode( constant );
		}
#line 7256 "Parser/parser.cc"
    break;

  case 79:
#line 762 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 7262 "Parser/parser.cc"
    break;

  case 80:
#line 764 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en)->set_extension( true ); }
#line 7268 "Parser/parser.cc"
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
#line 7288 "Parser/parser.cc"
    break;

  case 82:
#line 785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_val( (yyvsp[-1].op), (yyvsp[0].en) ) ); }
#line 7294 "Parser/parser.cc"
    break;

  case 83:
#line 787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Incr, (yyvsp[0].en) ) ); }
#line 7300 "Parser/parser.cc"
    break;

  case 84:
#line 789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Decr, (yyvsp[0].en) ) ); }
#line 7306 "Parser/parser.cc"
    break;

  case 85:
#line 791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7312 "Parser/parser.cc"
    break;

  case 86:
#line 793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 7318 "Parser/parser.cc"
    break;

  case 87:
#line 795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7324 "Parser/parser.cc"
    break;

  case 88:
#line 797 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 7330 "Parser/parser.cc"
    break;

  case 89:
#line 799 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_offsetOf( (yyvsp[-3].decl), build_varref( (yyvsp[-1].tok) ) ) ); }
#line 7336 "Parser/parser.cc"
    break;

  case 90:
#line 801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.en) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 7345 "Parser/parser.cc"
    break;

  case 91:
#line 808 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::PointTo; }
#line 7351 "Parser/parser.cc"
    break;

  case 92:
#line 809 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AddressOf; }
#line 7357 "Parser/parser.cc"
    break;

  case 93:
#line 811 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::And; }
#line 7363 "Parser/parser.cc"
    break;

  case 94:
#line 815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::UnPlus; }
#line 7369 "Parser/parser.cc"
    break;

  case 95:
#line 816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::UnMinus; }
#line 7375 "Parser/parser.cc"
    break;

  case 96:
#line 817 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::Neg; }
#line 7381 "Parser/parser.cc"
    break;

  case 97:
#line 818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::BitNeg; }
#line 7387 "Parser/parser.cc"
    break;

  case 99:
#line 824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cast( (yyvsp[-2].decl), (yyvsp[0].en) ) ); }
#line 7393 "Parser/parser.cc"
    break;

  case 100:
#line 826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 7399 "Parser/parser.cc"
    break;

  case 101:
#line 828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 7405 "Parser/parser.cc"
    break;

  case 102:
#line 830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( nullptr ) ) ); }
#line 7411 "Parser/parser.cc"
    break;

  case 103:
#line 832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 7417 "Parser/parser.cc"
    break;

  case 104:
#line 834 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Return cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7423 "Parser/parser.cc"
    break;

  case 105:
#line 836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7429 "Parser/parser.cc"
    break;

  case 106:
#line 838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7435 "Parser/parser.cc"
    break;

  case 114:
#line 858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Exp, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7441 "Parser/parser.cc"
    break;

  case 116:
#line 864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mul, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7447 "Parser/parser.cc"
    break;

  case 117:
#line 866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Div, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7453 "Parser/parser.cc"
    break;

  case 118:
#line 868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mod, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7459 "Parser/parser.cc"
    break;

  case 120:
#line 874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Plus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7465 "Parser/parser.cc"
    break;

  case 121:
#line 876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Minus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7471 "Parser/parser.cc"
    break;

  case 123:
#line 882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7477 "Parser/parser.cc"
    break;

  case 124:
#line 884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::RShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7483 "Parser/parser.cc"
    break;

  case 126:
#line 890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7489 "Parser/parser.cc"
    break;

  case 127:
#line 892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7495 "Parser/parser.cc"
    break;

  case 128:
#line 894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7501 "Parser/parser.cc"
    break;

  case 129:
#line 896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7507 "Parser/parser.cc"
    break;

  case 131:
#line 902 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Eq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7513 "Parser/parser.cc"
    break;

  case 132:
#line 904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Neq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7519 "Parser/parser.cc"
    break;

  case 134:
#line 910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitAnd, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7525 "Parser/parser.cc"
    break;

  case 136:
#line 916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Xor, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7531 "Parser/parser.cc"
    break;

  case 138:
#line 922 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitOr, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7537 "Parser/parser.cc"
    break;

  case 140:
#line 928 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), true ) ); }
#line 7543 "Parser/parser.cc"
    break;

  case 142:
#line 934 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), false ) ); }
#line 7549 "Parser/parser.cc"
    break;

  case 144:
#line 940 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7555 "Parser/parser.cc"
    break;

  case 145:
#line 943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-3].en), (yyvsp[-3].en), (yyvsp[0].en) ) ); }
#line 7561 "Parser/parser.cc"
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
#line 7573 "Parser/parser.cc"
    break;

  case 149:
#line 962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7579 "Parser/parser.cc"
    break;

  case 150:
#line 967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7585 "Parser/parser.cc"
    break;

  case 154:
#line 977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::Assign; }
#line 7591 "Parser/parser.cc"
    break;

  case 155:
#line 978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AtAssn; }
#line 7597 "Parser/parser.cc"
    break;

  case 156:
#line 982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ExpAssn; }
#line 7603 "Parser/parser.cc"
    break;

  case 157:
#line 983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MulAssn; }
#line 7609 "Parser/parser.cc"
    break;

  case 158:
#line 984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::DivAssn; }
#line 7615 "Parser/parser.cc"
    break;

  case 159:
#line 985 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ModAssn; }
#line 7621 "Parser/parser.cc"
    break;

  case 160:
#line 986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::PlusAssn; }
#line 7627 "Parser/parser.cc"
    break;

  case 161:
#line 987 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MinusAssn; }
#line 7633 "Parser/parser.cc"
    break;

  case 162:
#line 988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::LSAssn; }
#line 7639 "Parser/parser.cc"
    break;

  case 163:
#line 989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::RSAssn; }
#line 7645 "Parser/parser.cc"
    break;

  case 164:
#line 990 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AndAssn; }
#line 7651 "Parser/parser.cc"
    break;

  case 165:
#line 991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ERAssn; }
#line 7657 "Parser/parser.cc"
    break;

  case 166:
#line 992 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::OrAssn; }
#line 7663 "Parser/parser.cc"
    break;

  case 167:
#line 1003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)(new ExpressionNode( nullptr ) )->set_last( (yyvsp[-1].en) ) ) ); }
#line 7669 "Parser/parser.cc"
    break;

  case 168:
#line 1005 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-4].en)->set_last( (yyvsp[-1].en) ) ) )); }
#line 7675 "Parser/parser.cc"
    break;

  case 170:
#line 1011 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7681 "Parser/parser.cc"
    break;

  case 171:
#line 1013 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7687 "Parser/parser.cc"
    break;

  case 172:
#line 1015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7693 "Parser/parser.cc"
    break;

  case 174:
#line 1021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new CommaExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7699 "Parser/parser.cc"
    break;

  case 175:
#line 1026 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7705 "Parser/parser.cc"
    break;

  case 187:
#line 1044 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 7711 "Parser/parser.cc"
    break;

  case 189:
#line 1047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_directive( (yyvsp[0].tok) ) ); }
#line 7717 "Parser/parser.cc"
    break;

  case 190:
#line 1053 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[0].sn)->add_label( (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 7723 "Parser/parser.cc"
    break;

  case 191:
#line 1058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (StatementNode *)0 ) ); }
#line 7729 "Parser/parser.cc"
    break;

  case 192:
#line 1063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (yyvsp[-2].sn) ) ); }
#line 7735 "Parser/parser.cc"
    break;

  case 194:
#line 1069 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 7741 "Parser/parser.cc"
    break;

  case 195:
#line 1074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7747 "Parser/parser.cc"
    break;

  case 196:
#line 1076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7753 "Parser/parser.cc"
    break;

  case 197:
#line 1078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7759 "Parser/parser.cc"
    break;

  case 198:
#line 1080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7765 "Parser/parser.cc"
    break;

  case 201:
#line 1087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 7771 "Parser/parser.cc"
    break;

  case 202:
#line 1092 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_expr( (yyvsp[-1].en) ) ); }
#line 7777 "Parser/parser.cc"
    break;

  case 203:
#line 1094 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_mutex( nullptr, new StatementNode( build_expr( (yyvsp[-1].en) ) ) ) ); }
#line 7783 "Parser/parser.cc"
    break;

  case 204:
#line 1102 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn); }
#line 7789 "Parser/parser.cc"
    break;

  case 205:
#line 1104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( true, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 7795 "Parser/parser.cc"
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
#line 7809 "Parser/parser.cc"
    break;

  case 207:
#line 1116 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( false, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 7815 "Parser/parser.cc"
    break;

  case 208:
#line 1118 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( false, (yyvsp[-7].en), (yyvsp[-2].sn) ) );
			(yyval.sn) = (yyvsp[-3].decl) ? new StatementNode( build_compound( (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 7824 "Parser/parser.cc"
    break;

  case 209:
#line 1127 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-2].ifctl), maybe_build_compound( (yyvsp[0].sn) ), nullptr ) ); }
#line 7830 "Parser/parser.cc"
    break;

  case 210:
#line 1129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-4].ifctl), maybe_build_compound( (yyvsp[-2].sn) ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 7836 "Parser/parser.cc"
    break;

  case 211:
#line 1134 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( nullptr, (yyvsp[0].en) ); }
#line 7842 "Parser/parser.cc"
    break;

  case 212:
#line 1136 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 7848 "Parser/parser.cc"
    break;

  case 213:
#line 1138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 7854 "Parser/parser.cc"
    break;

  case 214:
#line 1140 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[-1].decl), (yyvsp[0].en) ); }
#line 7860 "Parser/parser.cc"
    break;

  case 215:
#line 1147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = (yyvsp[0].en); }
#line 7866 "Parser/parser.cc"
    break;

  case 216:
#line 1149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7872 "Parser/parser.cc"
    break;

  case 218:
#line 1154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.sn) = new StatementNode( build_case( (yyvsp[0].en) ) ); }
#line 7878 "Parser/parser.cc"
    break;

  case 219:
#line 1156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)((yyvsp[-2].sn)->set_last( new StatementNode( build_case( (yyvsp[0].en) ) ) ) ); }
#line 7884 "Parser/parser.cc"
    break;

  case 220:
#line 1160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn); }
#line 7890 "Parser/parser.cc"
    break;

  case 221:
#line 1161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.sn) = new StatementNode( build_default() ); }
#line 7896 "Parser/parser.cc"
    break;

  case 223:
#line 1173 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)( (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) )); }
#line 7902 "Parser/parser.cc"
    break;

  case 224:
#line 1177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( maybe_build_compound( (yyvsp[0].sn) ) ); }
#line 7908 "Parser/parser.cc"
    break;

  case 225:
#line 1182 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = nullptr; }
#line 7914 "Parser/parser.cc"
    break;

  case 227:
#line 1188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ); }
#line 7920 "Parser/parser.cc"
    break;

  case 228:
#line 1190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)( (yyvsp[-2].sn)->set_last( (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ) ) ); }
#line 7926 "Parser/parser.cc"
    break;

  case 229:
#line 1195 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( new CondCtl( nullptr, new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 7932 "Parser/parser.cc"
    break;

  case 230:
#line 1197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( (yyvsp[-2].ifctl), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 7938 "Parser/parser.cc"
    break;

  case 231:
#line 1200 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( (yyvsp[-4].ifctl), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[0].sn) ) ); }
#line 7944 "Parser/parser.cc"
    break;

  case 232:
#line 1202 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( new ExpressionNode( build_constantInteger( *new string( "1" ) ) ), maybe_build_compound( (yyvsp[-4].sn) ) ) ); }
#line 7950 "Parser/parser.cc"
    break;

  case 233:
#line 1204 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( (yyvsp[-2].en), maybe_build_compound( (yyvsp[-5].sn) ) ) ); }
#line 7956 "Parser/parser.cc"
    break;

  case 234:
#line 1207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( (yyvsp[-3].en), maybe_build_compound( (yyvsp[-6].sn) ), (yyvsp[0].sn) ) ); }
#line 7962 "Parser/parser.cc"
    break;

  case 235:
#line 1209 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( new ForCtrl( (ExpressionNode * )nullptr, (ExpressionNode * )nullptr, (ExpressionNode * )nullptr ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 7968 "Parser/parser.cc"
    break;

  case 236:
#line 1211 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( (yyvsp[-2].fctl), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 7974 "Parser/parser.cc"
    break;

  case 237:
#line 1214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( (yyvsp[-4].fctl), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[0].sn) ) ); }
#line 7980 "Parser/parser.cc"
    break;

  case 239:
#line 1224 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 7999 "Parser/parser.cc"
    break;

  case 240:
#line 1242 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (ExpressionNode * )nullptr, (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8005 "Parser/parser.cc"
    break;

  case 241:
#line 1244 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8011 "Parser/parser.cc"
    break;

  case 242:
#line 1246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-3].decl), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8017 "Parser/parser.cc"
    break;

  case 243:
#line 1249 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8024 "Parser/parser.cc"
    break;

  case 244:
#line 1252 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LEThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8031 "Parser/parser.cc"
    break;

  case 245:
#line 1255 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].en)->clone(), (yyvsp[-1].compop), (yyvsp[0].en), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8037 "Parser/parser.cc"
    break;

  case 246:
#line 1257 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8043 "Parser/parser.cc"
    break;

  case 247:
#line 1259 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( new ExpressionNode( build_constantInteger( *new string( "0u" ) ) ), (yyvsp[-1].en), nullptr, OperKinds::LThan, nullptr, nullptr ); }
#line 8049 "Parser/parser.cc"
    break;

  case 248:
#line 1261 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-2].en), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8056 "Parser/parser.cc"
    break;

  case 249:
#line 1264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-3].en), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LEThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8063 "Parser/parser.cc"
    break;

  case 250:
#line 1267 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), (yyvsp[-1].compop), (yyvsp[0].en), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8069 "Parser/parser.cc"
    break;

  case 251:
#line 1269 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8075 "Parser/parser.cc"
    break;

  case 252:
#line 1272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Array interator is currently unimplemented." ); (yyval.fctl) = nullptr;
			(yyval.fctl) = forCtrl( new ExpressionNode( build_varref( (yyvsp[0].tok) ) ), (yyvsp[-2].en), nullptr, OperKinds::Range, nullptr, nullptr );
		}
#line 8084 "Parser/parser.cc"
    break;

  case 253:
#line 1279 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), OperKinds::LThan, nullptr, new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8090 "Parser/parser.cc"
    break;

  case 254:
#line 1281 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), OperKinds::GThan, nullptr, new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8096 "Parser/parser.cc"
    break;

  case 255:
#line 1283 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), OperKinds::LThan, nullptr, (yyvsp[0].en) ); }
#line 8102 "Parser/parser.cc"
    break;

  case 256:
#line 1285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), OperKinds::GThan, nullptr, (yyvsp[0].en) ); }
#line 8108 "Parser/parser.cc"
    break;

  case 257:
#line 1287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), OperKinds::LThan, nullptr, nullptr ); }
#line 8114 "Parser/parser.cc"
    break;

  case 258:
#line 1292 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LThan; }
#line 8120 "Parser/parser.cc"
    break;

  case 259:
#line 1294 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 8126 "Parser/parser.cc"
    break;

  case 260:
#line 1296 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GThan; }
#line 8132 "Parser/parser.cc"
    break;

  case 261:
#line 1298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GEThan; }
#line 8138 "Parser/parser.cc"
    break;

  case 262:
#line 1303 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Goto ) ); }
#line 8144 "Parser/parser.cc"
    break;

  case 263:
#line 1307 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_computedgoto( (yyvsp[-1].en) ) ); }
#line 8150 "Parser/parser.cc"
    break;

  case 264:
#line 1310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThrough ) ); }
#line 8156 "Parser/parser.cc"
    break;

  case 265:
#line 1312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::FallThrough ) ); }
#line 8162 "Parser/parser.cc"
    break;

  case 266:
#line 1314 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThroughDefault ) ); }
#line 8168 "Parser/parser.cc"
    break;

  case 267:
#line 1317 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Continue ) ); }
#line 8174 "Parser/parser.cc"
    break;

  case 268:
#line 1321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Continue ) ); }
#line 8180 "Parser/parser.cc"
    break;

  case 269:
#line 1324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Break ) ); }
#line 8186 "Parser/parser.cc"
    break;

  case 270:
#line 1328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Break ) ); }
#line 8192 "Parser/parser.cc"
    break;

  case 271:
#line 1330 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_return( (yyvsp[-1].en) ) ); }
#line 8198 "Parser/parser.cc"
    break;

  case 272:
#line 1332 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 8204 "Parser/parser.cc"
    break;

  case 273:
#line 1334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr ) ); }
#line 8210 "Parser/parser.cc"
    break;

  case 274:
#line 1336 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn) ) ); }
#line 8216 "Parser/parser.cc"
    break;

  case 275:
#line 1338 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Coroutine ) ); }
#line 8222 "Parser/parser.cc"
    break;

  case 276:
#line 1340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Coroutine ) ); }
#line 8228 "Parser/parser.cc"
    break;

  case 277:
#line 1342 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Generator ) ); }
#line 8234 "Parser/parser.cc"
    break;

  case 278:
#line 1344 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Generator ) ); }
#line 8240 "Parser/parser.cc"
    break;

  case 279:
#line 1346 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_throw( (yyvsp[-1].en) ) ); }
#line 8246 "Parser/parser.cc"
    break;

  case 280:
#line 1348 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume( (yyvsp[-1].en) ) ); }
#line 8252 "Parser/parser.cc"
    break;

  case 281:
#line 1350 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume_at( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 8258 "Parser/parser.cc"
    break;

  case 284:
#line 1360 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_with( (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8264 "Parser/parser.cc"
    break;

  case 285:
#line 1366 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_mutex( (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8270 "Parser/parser.cc"
    break;

  case 286:
#line 1370 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 8276 "Parser/parser.cc"
    break;

  case 287:
#line 1375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8282 "Parser/parser.cc"
    break;

  case 289:
#line 1381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 8288 "Parser/parser.cc"
    break;

  case 290:
#line 1385 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) )); }
#line 8294 "Parser/parser.cc"
    break;

  case 292:
#line 1392 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8300 "Parser/parser.cc"
    break;

  case 293:
#line 1396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 8306 "Parser/parser.cc"
    break;

  case 294:
#line 1401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8312 "Parser/parser.cc"
    break;

  case 295:
#line 1403 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-3].en), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[-4].en), (yyvsp[0].wfs) ); }
#line 8318 "Parser/parser.cc"
    break;

  case 296:
#line 1405 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8324 "Parser/parser.cc"
    break;

  case 297:
#line 1407 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( nullptr, maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8330 "Parser/parser.cc"
    break;

  case 298:
#line 1410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 8336 "Parser/parser.cc"
    break;

  case 299:
#line 1412 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-5].en), maybe_build_compound( (yyvsp[-4].sn) ), (yyvsp[-6].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8342 "Parser/parser.cc"
    break;

  case 300:
#line 1417 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-1].en), (yyvsp[0].sn), (yyvsp[-2].en) ) ); }
#line 8348 "Parser/parser.cc"
    break;

  case 301:
#line 1419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-3].en), (yyvsp[-2].sn), (yyvsp[-4].en), (yyvsp[0].wfs) ) ); }
#line 8354 "Parser/parser.cc"
    break;

  case 302:
#line 1424 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), (yyvsp[0].sn), 0 ) ); }
#line 8360 "Parser/parser.cc"
    break;

  case 303:
#line 1426 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), 0, (yyvsp[0].sn) ) ); }
#line 8366 "Parser/parser.cc"
    break;

  case 304:
#line 1428 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-2].sn), (yyvsp[-1].sn), (yyvsp[0].sn) ) ); }
#line 8372 "Parser/parser.cc"
    break;

  case 305:
#line 1433 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8378 "Parser/parser.cc"
    break;

  case 306:
#line 1435 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)(yyvsp[-8].sn)->set_last( new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ) ); }
#line 8384 "Parser/parser.cc"
    break;

  case 307:
#line 1440 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8390 "Parser/parser.cc"
    break;

  case 308:
#line 1441 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[0].en); }
#line 8396 "Parser/parser.cc"
    break;

  case 309:
#line 1445 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 8402 "Parser/parser.cc"
    break;

  case 310:
#line 1446 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 8408 "Parser/parser.cc"
    break;

  case 311:
#line 1447 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.catch_kind) = CatchStmt::Resume; }
#line 8414 "Parser/parser.cc"
    break;

  case 312:
#line 1448 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Resume; }
#line 8420 "Parser/parser.cc"
    break;

  case 313:
#line 1452 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = new StatementNode( build_finally( (yyvsp[0].sn) ) ); }
#line 8426 "Parser/parser.cc"
    break;

  case 315:
#line 1459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 8432 "Parser/parser.cc"
    break;

  case 316:
#line 1461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 8438 "Parser/parser.cc"
    break;

  case 317:
#line 1463 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 8444 "Parser/parser.cc"
    break;

  case 322:
#line 1478 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-4].flag), (yyvsp[-2].constant), 0 ) ); }
#line 8450 "Parser/parser.cc"
    break;

  case 323:
#line 1480 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-6].flag), (yyvsp[-4].constant), (yyvsp[-2].en) ) ); }
#line 8456 "Parser/parser.cc"
    break;

  case 324:
#line 1482 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-8].flag), (yyvsp[-6].constant), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 8462 "Parser/parser.cc"
    break;

  case 325:
#line 1484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-10].flag), (yyvsp[-8].constant), (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 8468 "Parser/parser.cc"
    break;

  case 326:
#line 1486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-12].flag), (yyvsp[-9].constant), 0, (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].label) ) ); }
#line 8474 "Parser/parser.cc"
    break;

  case 327:
#line 1491 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = false; }
#line 8480 "Parser/parser.cc"
    break;

  case 328:
#line 1493 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = true; }
#line 8486 "Parser/parser.cc"
    break;

  case 329:
#line 1498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8492 "Parser/parser.cc"
    break;

  case 332:
#line 1505 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 8498 "Parser/parser.cc"
    break;

  case 333:
#line 1510 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( nullptr, (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 8504 "Parser/parser.cc"
    break;

  case 334:
#line 1512 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( (yyvsp[-5].tok), (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 8510 "Parser/parser.cc"
    break;

  case 335:
#line 1517 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8516 "Parser/parser.cc"
    break;

  case 336:
#line 1519 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 8522 "Parser/parser.cc"
    break;

  case 337:
#line 1521 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( (yyvsp[0].constant) ) )); }
#line 8528 "Parser/parser.cc"
    break;

  case 338:
#line 1526 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = new LabelNode(); (yyval.label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 8537 "Parser/parser.cc"
    break;

  case 339:
#line 1531 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = (yyvsp[-2].label); (yyvsp[-2].label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 8546 "Parser/parser.cc"
    break;

  case 340:
#line 1541 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8552 "Parser/parser.cc"
    break;

  case 343:
#line 1548 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 8558 "Parser/parser.cc"
    break;

  case 344:
#line 1553 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8564 "Parser/parser.cc"
    break;

  case 346:
#line 1559 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 8570 "Parser/parser.cc"
    break;

  case 347:
#line 1561 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-2].decl) ); }
#line 8576 "Parser/parser.cc"
    break;

  case 357:
#line 1587 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-4].en), (yyvsp[-2].constant) ); }
#line 8582 "Parser/parser.cc"
    break;

  case 358:
#line 1589 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-2].en), build_constantStr( *new string( "\"\"" ) ) ); }
#line 8588 "Parser/parser.cc"
    break;

  case 362:
#line 1607 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 8594 "Parser/parser.cc"
    break;

  case 364:
#line 1613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].in) ); }
#line 8600 "Parser/parser.cc"
    break;

  case 365:
#line 1617 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 8606 "Parser/parser.cc"
    break;

  case 366:
#line 1619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].in) ) ); }
#line 8612 "Parser/parser.cc"
    break;

  case 367:
#line 1626 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 8618 "Parser/parser.cc"
    break;

  case 368:
#line 1628 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 8624 "Parser/parser.cc"
    break;

  case 369:
#line 1630 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 8630 "Parser/parser.cc"
    break;

  case 371:
#line 1636 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 8636 "Parser/parser.cc"
    break;

  case 372:
#line 1638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 8642 "Parser/parser.cc"
    break;

  case 373:
#line 1640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 8648 "Parser/parser.cc"
    break;

  case 374:
#line 1642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeClone( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->appendList( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 8659 "Parser/parser.cc"
    break;

  case 375:
#line 1675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), 0 )->addQualifiers( (yyvsp[0].decl) ); }
#line 8665 "Parser/parser.cc"
    break;

  case 376:
#line 1677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), 0 )->addQualifiers( (yyvsp[0].decl) ); }
#line 8671 "Parser/parser.cc"
    break;

  case 377:
#line 1682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 8677 "Parser/parser.cc"
    break;

  case 378:
#line 1685 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-6].decl)->appendList( (yyvsp[-2].decl) ) ); }
#line 8683 "Parser/parser.cc"
    break;

  case 379:
#line 1690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 8692 "Parser/parser.cc"
    break;

  case 380:
#line 1695 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 8701 "Parser/parser.cc"
    break;

  case 381:
#line 1700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "3" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 8710 "Parser/parser.cc"
    break;

  case 382:
#line 1711 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "4" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef();
		}
#line 8719 "Parser/parser.cc"
    break;

  case 383:
#line 1716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "5" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 8728 "Parser/parser.cc"
    break;

  case 384:
#line 1721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "6" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[-3].decl) )->addTypedef();
		}
#line 8737 "Parser/parser.cc"
    break;

  case 385:
#line 1726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "7" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-2].decl) )->addTypedef();
		}
#line 8746 "Parser/parser.cc"
    break;

  case 386:
#line 1731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "8" );
			(yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-3].decl) )->addTypedef()->addType( (yyvsp[-3].decl) );
		}
#line 8755 "Parser/parser.cc"
    break;

  case 387:
#line 1740 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 8763 "Parser/parser.cc"
    break;

  case 388:
#line 1744 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 8771 "Parser/parser.cc"
    break;

  case 389:
#line 1751 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 8777 "Parser/parser.cc"
    break;

  case 393:
#line 1761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 8783 "Parser/parser.cc"
    break;

  case 394:
#line 1763 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ) ); }
#line 8789 "Parser/parser.cc"
    break;

  case 407:
#line 1802 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8795 "Parser/parser.cc"
    break;

  case 410:
#line 1814 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 8801 "Parser/parser.cc"
    break;

  case 413:
#line 1824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Const ); }
#line 8807 "Parser/parser.cc"
    break;

  case 414:
#line 1826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Restrict ); }
#line 8813 "Parser/parser.cc"
    break;

  case 415:
#line 1828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Volatile ); }
#line 8819 "Parser/parser.cc"
    break;

  case 416:
#line 1830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Atomic ); }
#line 8825 "Parser/parser.cc"
    break;

  case 418:
#line 1836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newForall( (yyvsp[-1].decl) ); }
#line 8831 "Parser/parser.cc"
    break;

  case 420:
#line 1842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 8837 "Parser/parser.cc"
    break;

  case 421:
#line 1844 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 8843 "Parser/parser.cc"
    break;

  case 423:
#line 1855 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 8849 "Parser/parser.cc"
    break;

  case 424:
#line 1860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Extern ); }
#line 8855 "Parser/parser.cc"
    break;

  case 425:
#line 1862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Static ); }
#line 8861 "Parser/parser.cc"
    break;

  case 426:
#line 1864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Auto ); }
#line 8867 "Parser/parser.cc"
    break;

  case 427:
#line 1866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Register ); }
#line 8873 "Parser/parser.cc"
    break;

  case 428:
#line 1868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Threadlocal ); }
#line 8879 "Parser/parser.cc"
    break;

  case 429:
#line 1871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Inline ); }
#line 8885 "Parser/parser.cc"
    break;

  case 430:
#line 1873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Fortran ); }
#line 8891 "Parser/parser.cc"
    break;

  case 431:
#line 1875 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Noreturn ); }
#line 8897 "Parser/parser.cc"
    break;

  case 432:
#line 1880 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 8903 "Parser/parser.cc"
    break;

  case 433:
#line 1882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Bool ); }
#line 8909 "Parser/parser.cc"
    break;

  case 434:
#line 1884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Char ); }
#line 8915 "Parser/parser.cc"
    break;

  case 435:
#line 1886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int ); }
#line 8921 "Parser/parser.cc"
    break;

  case 436:
#line 1888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 ); }
#line 8927 "Parser/parser.cc"
    break;

  case 437:
#line 1890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 )->addType( DeclarationNode::newSignedNess( DeclarationNode::Unsigned ) ); }
#line 8933 "Parser/parser.cc"
    break;

  case 438:
#line 1892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Float ); }
#line 8939 "Parser/parser.cc"
    break;

  case 439:
#line 1894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Double ); }
#line 8945 "Parser/parser.cc"
    break;

  case 440:
#line 1896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat80 ); }
#line 8951 "Parser/parser.cc"
    break;

  case 441:
#line 1898 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat128 ); }
#line 8957 "Parser/parser.cc"
    break;

  case 442:
#line 1900 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat16 ); }
#line 8963 "Parser/parser.cc"
    break;

  case 443:
#line 1902 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32 ); }
#line 8969 "Parser/parser.cc"
    break;

  case 444:
#line 1904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32x ); }
#line 8975 "Parser/parser.cc"
    break;

  case 445:
#line 1906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64 ); }
#line 8981 "Parser/parser.cc"
    break;

  case 446:
#line 1908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64x ); }
#line 8987 "Parser/parser.cc"
    break;

  case 447:
#line 1910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat128 ); }
#line 8993 "Parser/parser.cc"
    break;

  case 448:
#line 1912 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 8999 "Parser/parser.cc"
    break;

  case 449:
#line 1914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9005 "Parser/parser.cc"
    break;

  case 450:
#line 1916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9011 "Parser/parser.cc"
    break;

  case 451:
#line 1918 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Complex ); }
#line 9017 "Parser/parser.cc"
    break;

  case 452:
#line 1920 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Imaginary ); }
#line 9023 "Parser/parser.cc"
    break;

  case 453:
#line 1922 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Signed ); }
#line 9029 "Parser/parser.cc"
    break;

  case 454:
#line 1924 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Unsigned ); }
#line 9035 "Parser/parser.cc"
    break;

  case 455:
#line 1926 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Short ); }
#line 9041 "Parser/parser.cc"
    break;

  case 456:
#line 1928 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Long ); }
#line 9047 "Parser/parser.cc"
    break;

  case 457:
#line 1930 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Valist ); }
#line 9053 "Parser/parser.cc"
    break;

  case 458:
#line 1932 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::AutoType ); }
#line 9059 "Parser/parser.cc"
    break;

  case 460:
#line 1938 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9065 "Parser/parser.cc"
    break;

  case 462:
#line 1944 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVtableType( (yyvsp[-2].decl) ); }
#line 9071 "Parser/parser.cc"
    break;

  case 463:
#line 1950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9077 "Parser/parser.cc"
    break;

  case 464:
#line 1952 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9083 "Parser/parser.cc"
    break;

  case 466:
#line 1959 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9089 "Parser/parser.cc"
    break;

  case 467:
#line 1961 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9095 "Parser/parser.cc"
    break;

  case 468:
#line 1963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9101 "Parser/parser.cc"
    break;

  case 469:
#line 1965 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 9107 "Parser/parser.cc"
    break;

  case 471:
#line 1972 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9113 "Parser/parser.cc"
    break;

  case 473:
#line 1978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9119 "Parser/parser.cc"
    break;

  case 474:
#line 1980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9125 "Parser/parser.cc"
    break;

  case 475:
#line 1982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 9131 "Parser/parser.cc"
    break;

  case 476:
#line 1987 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 9137 "Parser/parser.cc"
    break;

  case 477:
#line 1989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en) ); }
#line 9143 "Parser/parser.cc"
    break;

  case 478:
#line 1991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 9149 "Parser/parser.cc"
    break;

  case 479:
#line 1993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en), true ); }
#line 9155 "Parser/parser.cc"
    break;

  case 480:
#line 1995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Zero ); }
#line 9161 "Parser/parser.cc"
    break;

  case 481:
#line 1997 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::One ); }
#line 9167 "Parser/parser.cc"
    break;

  case 483:
#line 2003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9173 "Parser/parser.cc"
    break;

  case 484:
#line 2005 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9179 "Parser/parser.cc"
    break;

  case 485:
#line 2007 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9185 "Parser/parser.cc"
    break;

  case 487:
#line 2013 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 9191 "Parser/parser.cc"
    break;

  case 488:
#line 2015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 9197 "Parser/parser.cc"
    break;

  case 489:
#line 2017 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 9206 "Parser/parser.cc"
    break;

  case 491:
#line 2026 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9212 "Parser/parser.cc"
    break;

  case 492:
#line 2028 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9218 "Parser/parser.cc"
    break;

  case 493:
#line 2030 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9224 "Parser/parser.cc"
    break;

  case 495:
#line 2036 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9230 "Parser/parser.cc"
    break;

  case 496:
#line 2038 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9236 "Parser/parser.cc"
    break;

  case 498:
#line 2044 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9242 "Parser/parser.cc"
    break;

  case 499:
#line 2046 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9248 "Parser/parser.cc"
    break;

  case 500:
#line 2048 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9254 "Parser/parser.cc"
    break;

  case 502:
#line 2054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9260 "Parser/parser.cc"
    break;

  case 503:
#line 2056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9266 "Parser/parser.cc"
    break;

  case 504:
#line 2061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypedef( (yyvsp[0].tok) ); }
#line 9272 "Parser/parser.cc"
    break;

  case 505:
#line 2063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 9278 "Parser/parser.cc"
    break;

  case 506:
#line 2065 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 9284 "Parser/parser.cc"
    break;

  case 508:
#line 2068 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), (yyvsp[0].decl) ); }
#line 9290 "Parser/parser.cc"
    break;

  case 509:
#line 2070 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), (yyvsp[0].decl) ); }
#line 9296 "Parser/parser.cc"
    break;

  case 510:
#line 2075 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[0].tok), nullptr ); }
#line 9302 "Parser/parser.cc"
    break;

  case 511:
#line 2077 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-2].tok), nullptr ); }
#line 9308 "Parser/parser.cc"
    break;

  case 512:
#line 2079 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 9314 "Parser/parser.cc"
    break;

  case 517:
#line 2094 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 9320 "Parser/parser.cc"
    break;

  case 518:
#line 2096 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 9326 "Parser/parser.cc"
    break;

  case 519:
#line 2098 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 9335 "Parser/parser.cc"
    break;

  case 520:
#line 2103 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) ); }
#line 9341 "Parser/parser.cc"
    break;

  case 521:
#line 2105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 9350 "Parser/parser.cc"
    break;

  case 522:
#line 2110 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypedef( (yyvsp[-5].tok) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 9359 "Parser/parser.cc"
    break;

  case 523:
#line 2115 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 9368 "Parser/parser.cc"
    break;

  case 524:
#line 2120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeGen( (yyvsp[-5].tok), nullptr );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 9377 "Parser/parser.cc"
    break;

  case 526:
#line 2129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9383 "Parser/parser.cc"
    break;

  case 527:
#line 2131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 9389 "Parser/parser.cc"
    break;

  case 528:
#line 2136 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 9399 "Parser/parser.cc"
    break;

  case 529:
#line 2142 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 9414 "Parser/parser.cc"
    break;

  case 532:
#line 2161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Struct; }
#line 9420 "Parser/parser.cc"
    break;

  case 533:
#line 2163 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Union; }
#line 9426 "Parser/parser.cc"
    break;

  case 534:
#line 2165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Exception; }
#line 9432 "Parser/parser.cc"
    break;

  case 535:
#line 2171 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 9438 "Parser/parser.cc"
    break;

  case 536:
#line 2173 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 9444 "Parser/parser.cc"
    break;

  case 537:
#line 2175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Generator; }
#line 9450 "Parser/parser.cc"
    break;

  case 538:
#line 2177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor generator is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 9456 "Parser/parser.cc"
    break;

  case 539:
#line 2179 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Coroutine; }
#line 9462 "Parser/parser.cc"
    break;

  case 540:
#line 2181 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor coroutine is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 9468 "Parser/parser.cc"
    break;

  case 541:
#line 2183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Thread; }
#line 9474 "Parser/parser.cc"
    break;

  case 542:
#line 2185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor thread is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 9480 "Parser/parser.cc"
    break;

  case 543:
#line 2190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9486 "Parser/parser.cc"
    break;

  case 544:
#line 2192 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 9492 "Parser/parser.cc"
    break;

  case 545:
#line 2197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); }
#line 9498 "Parser/parser.cc"
    break;

  case 546:
#line 2199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 9504 "Parser/parser.cc"
    break;

  case 547:
#line 2201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-1].decl) ) {								// field declarator ?
				(yyvsp[-1].decl) = DeclarationNode::newName( nullptr );
			} // if
			(yyvsp[-1].decl)->inLine = true;
			(yyval.decl) = distAttr( (yyvsp[-2].decl), (yyvsp[-1].decl) );					// mark all fields in list
			distInl( (yyvsp[-1].decl) );
		}
#line 9517 "Parser/parser.cc"
    break;

  case 548:
#line 2210 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9523 "Parser/parser.cc"
    break;

  case 551:
#line 2214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 9529 "Parser/parser.cc"
    break;

  case 552:
#line 2216 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 9535 "Parser/parser.cc"
    break;

  case 555:
#line 2223 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9541 "Parser/parser.cc"
    break;

  case 557:
#line 2226 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 9547 "Parser/parser.cc"
    break;

  case 558:
#line 2231 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].en) ); }
#line 9553 "Parser/parser.cc"
    break;

  case 559:
#line 2234 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 9559 "Parser/parser.cc"
    break;

  case 560:
#line 2237 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 9565 "Parser/parser.cc"
    break;

  case 561:
#line 2242 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9571 "Parser/parser.cc"
    break;

  case 563:
#line 2245 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 9577 "Parser/parser.cc"
    break;

  case 565:
#line 2256 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 9583 "Parser/parser.cc"
    break;

  case 566:
#line 2258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 9589 "Parser/parser.cc"
    break;

  case 568:
#line 2265 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 9595 "Parser/parser.cc"
    break;

  case 569:
#line 2270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9601 "Parser/parser.cc"
    break;

  case 571:
#line 2276 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 9607 "Parser/parser.cc"
    break;

  case 572:
#line 2281 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-4].decl) ); }
#line 9613 "Parser/parser.cc"
    break;

  case 573:
#line 2283 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); }
#line 9619 "Parser/parser.cc"
    break;

  case 574:
#line 2285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].tok), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) ); }
#line 9625 "Parser/parser.cc"
    break;

  case 575:
#line 2288 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-4].decl)->name, (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 9631 "Parser/parser.cc"
    break;

  case 576:
#line 2290 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-6].decl)->storageClasses.val != 0 || (yyvsp[-6].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			SemanticError( yylloc, "Typed enumeration is currently unimplemented." ); (yyval.decl) = nullptr;
		}
#line 9640 "Parser/parser.cc"
    break;

  case 577:
#line 2295 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].decl)->storageClasses.val != 0 || (yyvsp[-4].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			typedefTable.makeTypedef( *(yyvsp[-1].tok) );
		}
#line 9649 "Parser/parser.cc"
    break;

  case 578:
#line 2300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typed enumeration is currently unimplemented." ); (yyval.decl) = nullptr;
		}
#line 9657 "Parser/parser.cc"
    break;

  case 579:
#line 2304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-8].decl)->storageClasses.val != 0 || (yyvsp[-8].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			typedefTable.makeTypedef( *(yyvsp[-5].decl)->name );
			SemanticError( yylloc, "Typed enumeration is currently unimplemented." ); (yyval.decl) = nullptr;
		}
#line 9667 "Parser/parser.cc"
    break;

  case 581:
#line 2314 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); (yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), 0, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 9673 "Parser/parser.cc"
    break;

  case 582:
#line 2316 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].decl)->type->symbolic.name );	(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].decl)->type->symbolic.name, 0, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 9679 "Parser/parser.cc"
    break;

  case 583:
#line 2321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumConstant( (yyvsp[-1].tok), (yyvsp[0].en) ); }
#line 9685 "Parser/parser.cc"
    break;

  case 584:
#line 2323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumConstant( new string("inline"), nullptr ); }
#line 9691 "Parser/parser.cc"
    break;

  case 585:
#line 2325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( DeclarationNode::newEnumConstant( (yyvsp[-1].tok), (yyvsp[0].en) ) ); }
#line 9697 "Parser/parser.cc"
    break;

  case 586:
#line 2327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( DeclarationNode::newEnumConstant( new string("inline"), nullptr ) ); }
#line 9703 "Parser/parser.cc"
    break;

  case 587:
#line 2332 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9709 "Parser/parser.cc"
    break;

  case 588:
#line 2336 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].in)->get_expression(); }
#line 9715 "Parser/parser.cc"
    break;

  case 589:
#line 2341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 9721 "Parser/parser.cc"
    break;

  case 590:
#line 2343 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9727 "Parser/parser.cc"
    break;

  case 593:
#line 2347 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9733 "Parser/parser.cc"
    break;

  case 594:
#line 2349 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 9739 "Parser/parser.cc"
    break;

  case 595:
#line 2351 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 9745 "Parser/parser.cc"
    break;

  case 597:
#line 2359 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9751 "Parser/parser.cc"
    break;

  case 598:
#line 2361 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9757 "Parser/parser.cc"
    break;

  case 599:
#line 2363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-8].decl)->appendList( (yyvsp[-4].decl) )->appendList( (yyvsp[0].decl) ); }
#line 9763 "Parser/parser.cc"
    break;

  case 601:
#line 2369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9769 "Parser/parser.cc"
    break;

  case 602:
#line 2374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9775 "Parser/parser.cc"
    break;

  case 603:
#line 2376 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9781 "Parser/parser.cc"
    break;

  case 605:
#line 2379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 9787 "Parser/parser.cc"
    break;

  case 608:
#line 2386 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9793 "Parser/parser.cc"
    break;

  case 609:
#line 2388 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9799 "Parser/parser.cc"
    break;

  case 611:
#line 2397 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 9805 "Parser/parser.cc"
    break;

  case 612:
#line 2400 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 9811 "Parser/parser.cc"
    break;

  case 613:
#line 2402 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 9817 "Parser/parser.cc"
    break;

  case 618:
#line 2412 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9823 "Parser/parser.cc"
    break;

  case 620:
#line 2419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 9829 "Parser/parser.cc"
    break;

  case 621:
#line 2421 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 9835 "Parser/parser.cc"
    break;

  case 622:
#line 2426 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 9841 "Parser/parser.cc"
    break;

  case 623:
#line 2428 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 9847 "Parser/parser.cc"
    break;

  case 624:
#line 2437 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 9853 "Parser/parser.cc"
    break;

  case 625:
#line 2439 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 9859 "Parser/parser.cc"
    break;

  case 631:
#line 2452 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 9865 "Parser/parser.cc"
    break;

  case 634:
#line 2462 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 9871 "Parser/parser.cc"
    break;

  case 635:
#line 2463 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (yyvsp[-1].op) == OperKinds::Assign ? (yyvsp[0].in) : (yyvsp[0].in)->set_maybeConstructed( false ); }
#line 9877 "Parser/parser.cc"
    break;

  case 636:
#line 2464 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.in) = new InitializerNode( true ); }
#line 9883 "Parser/parser.cc"
    break;

  case 637:
#line 2465 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 9889 "Parser/parser.cc"
    break;

  case 638:
#line 2469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = new InitializerNode( (yyvsp[0].en) ); }
#line 9895 "Parser/parser.cc"
    break;

  case 639:
#line 2470 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 9901 "Parser/parser.cc"
    break;

  case 640:
#line 2475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 9907 "Parser/parser.cc"
    break;

  case 642:
#line 2477 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ); }
#line 9913 "Parser/parser.cc"
    break;

  case 643:
#line 2478 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (InitializerNode *)( (yyvsp[-2].in)->set_last( (yyvsp[0].in) ) ); }
#line 9919 "Parser/parser.cc"
    break;

  case 644:
#line 2479 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.in) = (InitializerNode *)((yyvsp[-3].in)->set_last( (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ) )); }
#line 9925 "Parser/parser.cc"
    break;

  case 646:
#line 2495 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[-1].tok) ) ); }
#line 9931 "Parser/parser.cc"
    break;

  case 648:
#line 2501 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-1].en)->set_last( (yyvsp[0].en) )); }
#line 9937 "Parser/parser.cc"
    break;

  case 649:
#line 2507 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 9943 "Parser/parser.cc"
    break;

  case 650:
#line 2510 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 9949 "Parser/parser.cc"
    break;

  case 651:
#line 2512 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 9955 "Parser/parser.cc"
    break;

  case 652:
#line 2514 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-4].en) ), maybeMoveBuild<Expression>( (yyvsp[-2].en) ) ) ); }
#line 9961 "Parser/parser.cc"
    break;

  case 653:
#line 2516 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 9967 "Parser/parser.cc"
    break;

  case 655:
#line 2540 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl) ); }
#line 9973 "Parser/parser.cc"
    break;

  case 656:
#line 2545 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9979 "Parser/parser.cc"
    break;

  case 657:
#line 2547 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 9985 "Parser/parser.cc"
    break;

  case 658:
#line 2552 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "9" );
			if ( (yyvsp[-1].tclass) == TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 9996 "Parser/parser.cc"
    break;

  case 659:
#line 2559 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 10002 "Parser/parser.cc"
    break;

  case 660:
#line 2561 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "9" ); }
#line 10008 "Parser/parser.cc"
    break;

  case 661:
#line 2563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 10014 "Parser/parser.cc"
    break;

  case 662:
#line 2565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "9" );
			(yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 10023 "Parser/parser.cc"
    break;

  case 663:
#line 2571 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 10029 "Parser/parser.cc"
    break;

  case 664:
#line 2576 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 10035 "Parser/parser.cc"
    break;

  case 665:
#line 2578 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 10041 "Parser/parser.cc"
    break;

  case 666:
#line 2580 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::DStype; }
#line 10047 "Parser/parser.cc"
    break;

  case 667:
#line 2584 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 10053 "Parser/parser.cc"
    break;

  case 668:
#line 2589 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 10059 "Parser/parser.cc"
    break;

  case 669:
#line 2591 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 10065 "Parser/parser.cc"
    break;

  case 670:
#line 2593 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ftype; }
#line 10071 "Parser/parser.cc"
    break;

  case 671:
#line 2595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 10077 "Parser/parser.cc"
    break;

  case 672:
#line 2600 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10083 "Parser/parser.cc"
    break;

  case 675:
#line 2607 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 10089 "Parser/parser.cc"
    break;

  case 676:
#line 2612 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 10095 "Parser/parser.cc"
    break;

  case 677:
#line 2614 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 10101 "Parser/parser.cc"
    break;

  case 678:
#line 2621 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 10107 "Parser/parser.cc"
    break;

  case 680:
#line 2624 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 10113 "Parser/parser.cc"
    break;

  case 681:
#line 2626 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)( (yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 10119 "Parser/parser.cc"
    break;

  case 682:
#line 2631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 10125 "Parser/parser.cc"
    break;

  case 683:
#line 2633 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10131 "Parser/parser.cc"
    break;

  case 684:
#line 2635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 10137 "Parser/parser.cc"
    break;

  case 685:
#line 2640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 10143 "Parser/parser.cc"
    break;

  case 686:
#line 2642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 10149 "Parser/parser.cc"
    break;

  case 687:
#line 2647 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "10" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), 0 );
		}
#line 10158 "Parser/parser.cc"
    break;

  case 688:
#line 2652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "11" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 10167 "Parser/parser.cc"
    break;

  case 689:
#line 2660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), 0 ); }
#line 10173 "Parser/parser.cc"
    break;

  case 690:
#line 2662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-8].tok), (yyvsp[-6].decl), (yyvsp[-2].decl) ); }
#line 10179 "Parser/parser.cc"
    break;

  case 692:
#line 2668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl) ); }
#line 10185 "Parser/parser.cc"
    break;

  case 697:
#line 2680 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 10191 "Parser/parser.cc"
    break;

  case 698:
#line 2685 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10197 "Parser/parser.cc"
    break;

  case 699:
#line 2687 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 10203 "Parser/parser.cc"
    break;

  case 701:
#line 2695 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl);	}
#line 10209 "Parser/parser.cc"
    break;

  case 702:
#line 2700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10215 "Parser/parser.cc"
    break;

  case 703:
#line 2702 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->appendList( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 10221 "Parser/parser.cc"
    break;

  case 704:
#line 2707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10227 "Parser/parser.cc"
    break;

  case 706:
#line 2712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 10233 "Parser/parser.cc"
    break;

  case 707:
#line 2716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 10239 "Parser/parser.cc"
    break;

  case 708:
#line 2721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( (yyvsp[0].tok) ) ) ); }
#line 10245 "Parser/parser.cc"
    break;

  case 711:
#line 2725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 10254 "Parser/parser.cc"
    break;

  case 712:
#line 2730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( false, (yyvsp[-2].constant), 0 ) ) ); }
#line 10260 "Parser/parser.cc"
    break;

  case 713:
#line 2732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = LinkageSpec::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 10269 "Parser/parser.cc"
    break;

  case 714:
#line 2737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10279 "Parser/parser.cc"
    break;

  case 715:
#line 2743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 10288 "Parser/parser.cc"
    break;

  case 716:
#line 2748 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10298 "Parser/parser.cc"
    break;

  case 717:
#line 2754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 10307 "Parser/parser.cc"
    break;

  case 718:
#line 2759 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10317 "Parser/parser.cc"
    break;

  case 719:
#line 2765 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.val) || (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 10326 "Parser/parser.cc"
    break;

  case 720:
#line 2770 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10336 "Parser/parser.cc"
    break;

  case 722:
#line 2785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].sn) ); }
#line 10342 "Parser/parser.cc"
    break;

  case 723:
#line 2787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].sn) ); }
#line 10348 "Parser/parser.cc"
    break;

  case 724:
#line 2792 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; forall = false; }
#line 10354 "Parser/parser.cc"
    break;

  case 725:
#line 2794 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); forall = false; }
#line 10360 "Parser/parser.cc"
    break;

  case 726:
#line 2799 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10371 "Parser/parser.cc"
    break;

  case 727:
#line 2806 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 10380 "Parser/parser.cc"
    break;

  case 728:
#line 2811 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 10389 "Parser/parser.cc"
    break;

  case 729:
#line 2817 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 10395 "Parser/parser.cc"
    break;

  case 730:
#line 2820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 10401 "Parser/parser.cc"
    break;

  case 731:
#line 2823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10407 "Parser/parser.cc"
    break;

  case 732:
#line 2827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-4].decl) );
		}
#line 10416 "Parser/parser.cc"
    break;

  case 733:
#line 2833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10422 "Parser/parser.cc"
    break;

  case 734:
#line 2836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10428 "Parser/parser.cc"
    break;

  case 735:
#line 2839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 10434 "Parser/parser.cc"
    break;

  case 739:
#line 2850 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 10440 "Parser/parser.cc"
    break;

  case 740:
#line 2855 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10446 "Parser/parser.cc"
    break;

  case 741:
#line 2857 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = (yyvsp[-2].constant);
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 10456 "Parser/parser.cc"
    break;

  case 742:
#line 2866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10462 "Parser/parser.cc"
    break;

  case 745:
#line 2873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10468 "Parser/parser.cc"
    break;

  case 746:
#line 2878 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 10474 "Parser/parser.cc"
    break;

  case 748:
#line 2884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10480 "Parser/parser.cc"
    break;

  case 749:
#line 2889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10486 "Parser/parser.cc"
    break;

  case 750:
#line 2891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 10492 "Parser/parser.cc"
    break;

  case 751:
#line 2893 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 10498 "Parser/parser.cc"
    break;

  case 756:
#line 2902 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 10504 "Parser/parser.cc"
    break;

  case 757:
#line 2904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 10510 "Parser/parser.cc"
    break;

  case 758:
#line 2939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 10516 "Parser/parser.cc"
    break;

  case 759:
#line 2941 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10522 "Parser/parser.cc"
    break;

  case 760:
#line 2946 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10528 "Parser/parser.cc"
    break;

  case 762:
#line 2949 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10534 "Parser/parser.cc"
    break;

  case 763:
#line 2951 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10540 "Parser/parser.cc"
    break;

  case 764:
#line 2956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10546 "Parser/parser.cc"
    break;

  case 765:
#line 2958 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10552 "Parser/parser.cc"
    break;

  case 766:
#line 2960 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10558 "Parser/parser.cc"
    break;

  case 767:
#line 2962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10564 "Parser/parser.cc"
    break;

  case 768:
#line 2967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 10570 "Parser/parser.cc"
    break;

  case 769:
#line 2969 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10576 "Parser/parser.cc"
    break;

  case 770:
#line 2971 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10582 "Parser/parser.cc"
    break;

  case 771:
#line 2973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10588 "Parser/parser.cc"
    break;

  case 772:
#line 2975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10594 "Parser/parser.cc"
    break;

  case 773:
#line 2977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10600 "Parser/parser.cc"
    break;

  case 774:
#line 2979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10606 "Parser/parser.cc"
    break;

  case 775:
#line 2984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10612 "Parser/parser.cc"
    break;

  case 776:
#line 2986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 10618 "Parser/parser.cc"
    break;

  case 777:
#line 2988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10624 "Parser/parser.cc"
    break;

  case 778:
#line 2990 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10630 "Parser/parser.cc"
    break;

  case 779:
#line 2999 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10636 "Parser/parser.cc"
    break;

  case 781:
#line 3002 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10642 "Parser/parser.cc"
    break;

  case 782:
#line 3007 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10648 "Parser/parser.cc"
    break;

  case 783:
#line 3009 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10654 "Parser/parser.cc"
    break;

  case 784:
#line 3011 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 10660 "Parser/parser.cc"
    break;

  case 785:
#line 3013 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10666 "Parser/parser.cc"
    break;

  case 786:
#line 3015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10672 "Parser/parser.cc"
    break;

  case 787:
#line 3020 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10678 "Parser/parser.cc"
    break;

  case 788:
#line 3022 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10684 "Parser/parser.cc"
    break;

  case 789:
#line 3024 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10690 "Parser/parser.cc"
    break;

  case 790:
#line 3026 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10696 "Parser/parser.cc"
    break;

  case 791:
#line 3031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10702 "Parser/parser.cc"
    break;

  case 792:
#line 3033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10708 "Parser/parser.cc"
    break;

  case 793:
#line 3035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10714 "Parser/parser.cc"
    break;

  case 794:
#line 3037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10720 "Parser/parser.cc"
    break;

  case 795:
#line 3039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10726 "Parser/parser.cc"
    break;

  case 796:
#line 3041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10732 "Parser/parser.cc"
    break;

  case 800:
#line 3059 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 10738 "Parser/parser.cc"
    break;

  case 801:
#line 3061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10744 "Parser/parser.cc"
    break;

  case 802:
#line 3063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 10750 "Parser/parser.cc"
    break;

  case 803:
#line 3065 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10756 "Parser/parser.cc"
    break;

  case 804:
#line 3067 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10762 "Parser/parser.cc"
    break;

  case 805:
#line 3072 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10768 "Parser/parser.cc"
    break;

  case 806:
#line 3074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10774 "Parser/parser.cc"
    break;

  case 807:
#line 3076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10780 "Parser/parser.cc"
    break;

  case 808:
#line 3078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10786 "Parser/parser.cc"
    break;

  case 809:
#line 3083 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10792 "Parser/parser.cc"
    break;

  case 810:
#line 3085 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10798 "Parser/parser.cc"
    break;

  case 811:
#line 3087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10804 "Parser/parser.cc"
    break;

  case 812:
#line 3089 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10810 "Parser/parser.cc"
    break;

  case 813:
#line 3091 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10816 "Parser/parser.cc"
    break;

  case 814:
#line 3093 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10822 "Parser/parser.cc"
    break;

  case 815:
#line 3108 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "ID" );
		}
#line 10831 "Parser/parser.cc"
    break;

  case 816:
#line 3113 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10837 "Parser/parser.cc"
    break;

  case 817:
#line 3118 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10843 "Parser/parser.cc"
    break;

  case 819:
#line 3121 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10849 "Parser/parser.cc"
    break;

  case 820:
#line 3123 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10855 "Parser/parser.cc"
    break;

  case 821:
#line 3128 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10861 "Parser/parser.cc"
    break;

  case 822:
#line 3130 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10867 "Parser/parser.cc"
    break;

  case 823:
#line 3132 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10873 "Parser/parser.cc"
    break;

  case 824:
#line 3134 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10879 "Parser/parser.cc"
    break;

  case 825:
#line 3139 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 10885 "Parser/parser.cc"
    break;

  case 826:
#line 3141 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10891 "Parser/parser.cc"
    break;

  case 827:
#line 3143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10897 "Parser/parser.cc"
    break;

  case 828:
#line 3145 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10903 "Parser/parser.cc"
    break;

  case 829:
#line 3147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10909 "Parser/parser.cc"
    break;

  case 830:
#line 3149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10915 "Parser/parser.cc"
    break;

  case 831:
#line 3151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10921 "Parser/parser.cc"
    break;

  case 832:
#line 3156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10927 "Parser/parser.cc"
    break;

  case 833:
#line 3158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10933 "Parser/parser.cc"
    break;

  case 834:
#line 3160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 10939 "Parser/parser.cc"
    break;

  case 835:
#line 3162 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10945 "Parser/parser.cc"
    break;

  case 836:
#line 3164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10951 "Parser/parser.cc"
    break;

  case 837:
#line 3174 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10957 "Parser/parser.cc"
    break;

  case 838:
#line 3176 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10963 "Parser/parser.cc"
    break;

  case 840:
#line 3179 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10969 "Parser/parser.cc"
    break;

  case 841:
#line 3181 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10975 "Parser/parser.cc"
    break;

  case 842:
#line 3186 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10981 "Parser/parser.cc"
    break;

  case 843:
#line 3188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10987 "Parser/parser.cc"
    break;

  case 844:
#line 3190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10993 "Parser/parser.cc"
    break;

  case 845:
#line 3195 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 10999 "Parser/parser.cc"
    break;

  case 846:
#line 3197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11005 "Parser/parser.cc"
    break;

  case 847:
#line 3199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11011 "Parser/parser.cc"
    break;

  case 848:
#line 3201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11017 "Parser/parser.cc"
    break;

  case 849:
#line 3206 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11023 "Parser/parser.cc"
    break;

  case 850:
#line 3208 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11029 "Parser/parser.cc"
    break;

  case 851:
#line 3210 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11035 "Parser/parser.cc"
    break;

  case 852:
#line 3224 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11041 "Parser/parser.cc"
    break;

  case 853:
#line 3226 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11047 "Parser/parser.cc"
    break;

  case 855:
#line 3229 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11053 "Parser/parser.cc"
    break;

  case 856:
#line 3231 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11059 "Parser/parser.cc"
    break;

  case 857:
#line 3236 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11065 "Parser/parser.cc"
    break;

  case 858:
#line 3238 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11071 "Parser/parser.cc"
    break;

  case 859:
#line 3243 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11077 "Parser/parser.cc"
    break;

  case 860:
#line 3245 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11083 "Parser/parser.cc"
    break;

  case 861:
#line 3247 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11089 "Parser/parser.cc"
    break;

  case 862:
#line 3252 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11095 "Parser/parser.cc"
    break;

  case 863:
#line 3254 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11101 "Parser/parser.cc"
    break;

  case 864:
#line 3259 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11107 "Parser/parser.cc"
    break;

  case 865:
#line 3261 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11113 "Parser/parser.cc"
    break;

  case 867:
#line 3279 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11119 "Parser/parser.cc"
    break;

  case 868:
#line 3281 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11125 "Parser/parser.cc"
    break;

  case 869:
#line 3286 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( 0, (yyvsp[0].op) ); }
#line 11131 "Parser/parser.cc"
    break;

  case 870:
#line 3288 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 11137 "Parser/parser.cc"
    break;

  case 871:
#line 3290 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11143 "Parser/parser.cc"
    break;

  case 872:
#line 3292 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11149 "Parser/parser.cc"
    break;

  case 873:
#line 3294 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11155 "Parser/parser.cc"
    break;

  case 875:
#line 3300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11161 "Parser/parser.cc"
    break;

  case 876:
#line 3302 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11167 "Parser/parser.cc"
    break;

  case 877:
#line 3304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11173 "Parser/parser.cc"
    break;

  case 878:
#line 3309 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 11179 "Parser/parser.cc"
    break;

  case 879:
#line 3311 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11185 "Parser/parser.cc"
    break;

  case 880:
#line 3313 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11191 "Parser/parser.cc"
    break;

  case 881:
#line 3319 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false ); }
#line 11197 "Parser/parser.cc"
    break;

  case 882:
#line 3321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false )->addArray( (yyvsp[0].decl) ); }
#line 11203 "Parser/parser.cc"
    break;

  case 883:
#line 3323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-4].en), 0, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].en), 0, false ) ); }
#line 11209 "Parser/parser.cc"
    break;

  case 885:
#line 3330 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), 0, false ); }
#line 11215 "Parser/parser.cc"
    break;

  case 886:
#line 3332 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 11221 "Parser/parser.cc"
    break;

  case 887:
#line 3334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newArray( (yyvsp[-2].en), 0, false ) ); }
#line 11227 "Parser/parser.cc"
    break;

  case 888:
#line 3336 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 11233 "Parser/parser.cc"
    break;

  case 890:
#line 3371 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 11239 "Parser/parser.cc"
    break;

  case 891:
#line 3373 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11245 "Parser/parser.cc"
    break;

  case 892:
#line 3375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11251 "Parser/parser.cc"
    break;

  case 893:
#line 3380 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].op) ); }
#line 11257 "Parser/parser.cc"
    break;

  case 894:
#line 3382 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 11263 "Parser/parser.cc"
    break;

  case 895:
#line 3384 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 11269 "Parser/parser.cc"
    break;

  case 896:
#line 3386 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11275 "Parser/parser.cc"
    break;

  case 897:
#line 3388 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11281 "Parser/parser.cc"
    break;

  case 899:
#line 3394 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11287 "Parser/parser.cc"
    break;

  case 900:
#line 3396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11293 "Parser/parser.cc"
    break;

  case 901:
#line 3398 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11299 "Parser/parser.cc"
    break;

  case 902:
#line 3403 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 11305 "Parser/parser.cc"
    break;

  case 903:
#line 3405 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11311 "Parser/parser.cc"
    break;

  case 904:
#line 3407 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11317 "Parser/parser.cc"
    break;

  case 906:
#line 3414 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11323 "Parser/parser.cc"
    break;

  case 908:
#line 3425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false ); }
#line 11329 "Parser/parser.cc"
    break;

  case 909:
#line 3428 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 11335 "Parser/parser.cc"
    break;

  case 910:
#line 3430 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, (yyvsp[-2].decl), false ); }
#line 11341 "Parser/parser.cc"
    break;

  case 911:
#line 3433 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 11347 "Parser/parser.cc"
    break;

  case 912:
#line 3435 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 11353 "Parser/parser.cc"
    break;

  case 913:
#line 3437 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-4].decl), true ); }
#line 11359 "Parser/parser.cc"
    break;

  case 915:
#line 3452 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11365 "Parser/parser.cc"
    break;

  case 916:
#line 3454 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11371 "Parser/parser.cc"
    break;

  case 917:
#line 3459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( 0, (yyvsp[0].op) ); }
#line 11377 "Parser/parser.cc"
    break;

  case 918:
#line 3461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 11383 "Parser/parser.cc"
    break;

  case 919:
#line 3463 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11389 "Parser/parser.cc"
    break;

  case 920:
#line 3465 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11395 "Parser/parser.cc"
    break;

  case 921:
#line 3467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11401 "Parser/parser.cc"
    break;

  case 923:
#line 3473 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11407 "Parser/parser.cc"
    break;

  case 924:
#line 3475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11413 "Parser/parser.cc"
    break;

  case 925:
#line 3477 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11419 "Parser/parser.cc"
    break;

  case 926:
#line 3482 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11425 "Parser/parser.cc"
    break;

  case 927:
#line 3484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11431 "Parser/parser.cc"
    break;

  case 930:
#line 3494 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11437 "Parser/parser.cc"
    break;

  case 933:
#line 3505 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11443 "Parser/parser.cc"
    break;

  case 934:
#line 3507 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11449 "Parser/parser.cc"
    break;

  case 935:
#line 3509 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11455 "Parser/parser.cc"
    break;

  case 936:
#line 3511 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11461 "Parser/parser.cc"
    break;

  case 937:
#line 3513 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11467 "Parser/parser.cc"
    break;

  case 938:
#line 3515 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11473 "Parser/parser.cc"
    break;

  case 939:
#line 3522 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11479 "Parser/parser.cc"
    break;

  case 940:
#line 3524 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11485 "Parser/parser.cc"
    break;

  case 941:
#line 3526 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11491 "Parser/parser.cc"
    break;

  case 942:
#line 3528 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 11497 "Parser/parser.cc"
    break;

  case 943:
#line 3530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11503 "Parser/parser.cc"
    break;

  case 944:
#line 3533 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11509 "Parser/parser.cc"
    break;

  case 945:
#line 3535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11515 "Parser/parser.cc"
    break;

  case 946:
#line 3537 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11521 "Parser/parser.cc"
    break;

  case 947:
#line 3539 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 11527 "Parser/parser.cc"
    break;

  case 948:
#line 3541 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11533 "Parser/parser.cc"
    break;

  case 949:
#line 3546 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 11539 "Parser/parser.cc"
    break;

  case 950:
#line 3548 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 11545 "Parser/parser.cc"
    break;

  case 951:
#line 3553 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 11551 "Parser/parser.cc"
    break;

  case 952:
#line 3555 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl)->addQualifiers( (yyvsp[-4].decl) ), true ); }
#line 11557 "Parser/parser.cc"
    break;

  case 954:
#line 3582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11563 "Parser/parser.cc"
    break;

  case 958:
#line 3593 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11569 "Parser/parser.cc"
    break;

  case 959:
#line 3595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11575 "Parser/parser.cc"
    break;

  case 960:
#line 3597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11581 "Parser/parser.cc"
    break;

  case 961:
#line 3599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11587 "Parser/parser.cc"
    break;

  case 962:
#line 3601 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11593 "Parser/parser.cc"
    break;

  case 963:
#line 3603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11599 "Parser/parser.cc"
    break;

  case 964:
#line 3610 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11605 "Parser/parser.cc"
    break;

  case 965:
#line 3612 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11611 "Parser/parser.cc"
    break;

  case 966:
#line 3614 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11617 "Parser/parser.cc"
    break;

  case 967:
#line 3616 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11623 "Parser/parser.cc"
    break;

  case 968:
#line 3618 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11629 "Parser/parser.cc"
    break;

  case 969:
#line 3620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11635 "Parser/parser.cc"
    break;

  case 970:
#line 3625 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 11641 "Parser/parser.cc"
    break;

  case 971:
#line 3627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11647 "Parser/parser.cc"
    break;

  case 972:
#line 3629 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11653 "Parser/parser.cc"
    break;

  case 973:
#line 3636 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 11659 "Parser/parser.cc"
    break;

  case 974:
#line 3638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 11665 "Parser/parser.cc"
    break;

  case 977:
#line 3662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 11671 "Parser/parser.cc"
    break;

  case 978:
#line 3664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 11677 "Parser/parser.cc"
    break;


#line 11681 "Parser/parser.cc"

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
#line 3667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "make install" //
// End: //
