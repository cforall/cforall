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
#define YYLAST   18934

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  175
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  288
/* YYNRULES -- Number of rules.  */
#define YYNRULES  975
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  1987

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
    1196,  1198,  1200,  1202,  1204,  1209,  1210,  1232,  1234,  1236,
    1239,  1242,  1245,  1247,  1249,  1251,  1254,  1257,  1259,  1262,
    1269,  1271,  1273,  1275,  1277,  1282,  1284,  1286,  1288,  1293,
    1295,  1300,  1302,  1304,  1306,  1309,  1313,  1316,  1320,  1322,
    1324,  1326,  1328,  1330,  1332,  1334,  1336,  1338,  1340,  1345,
    1346,  1350,  1356,  1361,  1366,  1367,  1371,  1375,  1380,  1381,
    1387,  1391,  1393,  1395,  1397,  1400,  1402,  1407,  1409,  1414,
    1416,  1418,  1423,  1425,  1431,  1432,  1436,  1437,  1438,  1439,
    1443,  1448,  1449,  1451,  1453,  1455,  1459,  1463,  1464,  1468,
    1470,  1472,  1474,  1476,  1482,  1483,  1489,  1490,  1494,  1495,
    1500,  1502,  1508,  1509,  1511,  1516,  1521,  1532,  1533,  1537,
    1538,  1544,  1545,  1549,  1551,  1555,  1557,  1561,  1562,  1566,
    1567,  1571,  1572,  1573,  1577,  1579,  1594,  1595,  1596,  1597,
    1599,  1603,  1605,  1609,  1616,  1618,  1620,  1625,  1626,  1628,
    1630,  1632,  1664,  1667,  1672,  1674,  1680,  1685,  1690,  1701,
    1706,  1711,  1716,  1721,  1730,  1734,  1741,  1743,  1744,  1745,
    1751,  1753,  1758,  1759,  1760,  1769,  1770,  1771,  1775,  1776,
    1777,  1786,  1787,  1788,  1793,  1794,  1803,  1804,  1809,  1810,
    1814,  1816,  1818,  1820,  1822,  1826,  1831,  1832,  1834,  1844,
    1845,  1850,  1852,  1854,  1856,  1858,  1861,  1863,  1865,  1870,
    1872,  1874,  1876,  1878,  1880,  1882,  1884,  1886,  1888,  1890,
    1892,  1894,  1896,  1898,  1900,  1902,  1904,  1906,  1908,  1910,
    1912,  1914,  1916,  1918,  1920,  1922,  1924,  1929,  1930,  1934,
    1941,  1942,  1948,  1949,  1951,  1953,  1955,  1960,  1962,  1967,
    1968,  1970,  1972,  1977,  1979,  1981,  1983,  1985,  1987,  1992,
    1993,  1995,  1997,  2002,  2004,  2003,  2007,  2015,  2016,  2018,
    2020,  2025,  2026,  2028,  2033,  2034,  2036,  2038,  2043,  2044,
    2046,  2051,  2053,  2055,  2057,  2058,  2060,  2065,  2067,  2069,
    2074,  2075,  2079,  2080,  2085,  2084,  2089,  2088,  2096,  2095,
    2106,  2105,  2115,  2120,  2121,  2126,  2132,  2146,  2147,  2151,
    2153,  2155,  2161,  2163,  2165,  2167,  2169,  2171,  2173,  2175,
    2181,  2182,  2187,  2189,  2191,  2200,  2202,  2203,  2204,  2206,
    2208,  2209,  2214,  2215,  2216,  2221,  2223,  2226,  2233,  2234,
    2235,  2241,  2246,  2248,  2254,  2255,  2261,  2262,  2266,  2271,
    2274,  2273,  2277,  2280,  2286,  2285,  2294,  2300,  2304,  2306,
    2311,  2313,  2315,  2317,  2323,  2326,  2332,  2333,  2335,  2336,
    2337,  2339,  2341,  2348,  2349,  2351,  2353,  2358,  2359,  2365,
    2366,  2368,  2369,  2374,  2375,  2376,  2378,  2386,  2387,  2389,
    2392,  2394,  2398,  2399,  2400,  2402,  2404,  2409,  2411,  2416,
    2418,  2427,  2429,  2434,  2435,  2436,  2440,  2441,  2442,  2447,
    2448,  2453,  2454,  2455,  2456,  2460,  2461,  2466,  2467,  2468,
    2469,  2470,  2484,  2485,  2490,  2491,  2497,  2499,  2502,  2504,
    2506,  2529,  2530,  2536,  2537,  2543,  2542,  2552,  2551,  2555,
    2561,  2567,  2568,  2570,  2574,  2579,  2581,  2583,  2585,  2591,
    2592,  2596,  2597,  2602,  2604,  2611,  2613,  2614,  2616,  2621,
    2623,  2625,  2630,  2632,  2637,  2642,  2650,  2652,  2657,  2658,
    2663,  2664,  2668,  2669,  2670,  2675,  2677,  2683,  2685,  2690,
    2692,  2698,  2699,  2703,  2707,  2711,  2713,  2714,  2715,  2720,
    2723,  2722,  2734,  2733,  2745,  2744,  2756,  2755,  2769,  2775,
    2777,  2783,  2784,  2789,  2796,  2801,  2807,  2810,  2813,  2817,
    2823,  2826,  2829,  2834,  2835,  2836,  2840,  2846,  2847,  2857,
    2858,  2862,  2863,  2868,  2873,  2874,  2880,  2881,  2883,  2888,
    2889,  2890,  2891,  2892,  2894,  2929,  2931,  2936,  2938,  2939,
    2941,  2946,  2948,  2950,  2952,  2957,  2959,  2961,  2963,  2965,
    2967,  2969,  2974,  2976,  2978,  2980,  2989,  2991,  2992,  2997,
    2999,  3001,  3003,  3005,  3010,  3012,  3014,  3016,  3021,  3023,
    3025,  3027,  3029,  3031,  3043,  3044,  3045,  3049,  3051,  3053,
    3055,  3057,  3062,  3064,  3066,  3068,  3073,  3075,  3077,  3079,
    3081,  3083,  3098,  3103,  3108,  3110,  3111,  3113,  3118,  3120,
    3122,  3124,  3129,  3131,  3133,  3135,  3137,  3139,  3141,  3146,
    3148,  3150,  3152,  3154,  3164,  3166,  3168,  3169,  3171,  3176,
    3178,  3180,  3185,  3187,  3189,  3191,  3196,  3198,  3200,  3214,
    3216,  3218,  3219,  3221,  3226,  3228,  3233,  3235,  3237,  3242,
    3244,  3249,  3251,  3268,  3269,  3271,  3276,  3278,  3280,  3282,
    3284,  3289,  3290,  3292,  3294,  3299,  3301,  3303,  3309,  3311,
    3313,  3316,  3320,  3322,  3324,  3326,  3360,  3361,  3363,  3365,
    3370,  3372,  3374,  3376,  3378,  3383,  3384,  3386,  3388,  3393,
    3395,  3397,  3403,  3404,  3406,  3415,  3418,  3420,  3423,  3425,
    3427,  3441,  3442,  3444,  3449,  3451,  3453,  3455,  3457,  3462,
    3463,  3465,  3467,  3472,  3474,  3482,  3483,  3484,  3489,  3490,
    3495,  3497,  3499,  3501,  3503,  3505,  3512,  3514,  3516,  3518,
    3520,  3523,  3525,  3527,  3529,  3531,  3536,  3538,  3540,  3545,
    3571,  3572,  3574,  3578,  3579,  3583,  3585,  3587,  3589,  3591,
    3593,  3600,  3602,  3604,  3606,  3608,  3610,  3615,  3617,  3619,
    3626,  3628,  3646,  3648,  3653,  3654
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

#define YYPACT_NINF (-1720)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-856)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
      39, 11050,   197,   220, 15272,   117, -1720, -1720, -1720, -1720,
   -1720, -1720, -1720, -1720, -1720, -1720, -1720,    89,   730,   116,
   -1720, -1720, -1720, -1720, -1720, -1720, -1720, -1720, -1720, -1720,
   -1720, -1720, -1720, -1720, -1720, -1720, -1720, -1720, -1720, -1720,
   -1720, -1720, -1720, -1720, -1720, -1720, -1720,    94,   318, -1720,
   -1720, -1720, -1720, -1720, -1720,  4411,  4411,   288, 11050,   293,
     302, -1720, -1720,   414, -1720, -1720, -1720, -1720, -1720, -1720,
   -1720, -1720, -1720,  2493, -1720,   372,   348, -1720, -1720, -1720,
   -1720, -1720, 15122, -1720, -1720,   398,   433,   250,    48, -1720,
    4411,   433,   433,   433,   423,  3707,   603,   664,  9389, -1720,
   -1720, -1720, 14972,  1019, -1720, -1720, -1720,  1402,   642, 11579,
    1207,   926,  1402,   933,   477, -1720, -1720, -1720, -1720,   651,
   -1720, -1720, -1720, -1720,   537, -1720, -1720, -1720, -1720, -1720,
     601,   608,   651, -1720,   651,   614, -1720, -1720, -1720, 15828,
    4411, -1720, -1720,  4411, -1720, 11050,   584, 15880, -1720, -1720,
    4321, 16831, -1720,   912,   912, -1720,  2260, -1720, -1720, -1720,
   -1720,   378, 13582,  3064,   651, -1720, -1720, -1720, -1720, -1720,
   -1720,   627, -1720,   607,   669,   695, -1720,   689, 18409, 14202,
    2625,  2493,   -38,   640,   659,   723,   726,   735,   753, -1720,
   -1720, 16030, 10401,   697, -1720, 15415, -1720, -1720, -1720, -1720,
     761, -1720, -1720,   660, -1720, 17617,   901, 17761, -1720,   781,
    4411,   608,   783,   788,   796,   806, -1720, -1720, -1720,  2919,
    2799,   828,   892,   131, -1720, -1720,   651,   651,    74,    80,
     236,    74, -1720,   651,   651, -1720,  3135, -1720, -1720,   848,
     851,   912, 13160, -1720, -1720, 15122, -1720, -1720,  1402, -1720,
    1331,   477,   854,   966,    80,  4411,   250, -1720, 12440, -1720,
     912,   912,   875,   966,    80,  4411, -1720, 12332, -1720, -1720,
     912, -1720,   912, -1720,   441,  4161,  4411, -1720,  1958,   929,
   -1720, -1720, -1720, 15574,   608,   101, -1720, -1720, 16881, -1720,
     892,    40, -1720, 18409, 16831,  3813,  3135, -1720,   360, -1720,
   -1720, -1720, 15880,  4411,   936, -1720, -1720, -1720, -1720,  4411,
    3439,   326,   661, -1720,  4411,   607, -1720,   564,   651,   969,
   16082,   674, 13740, 13318,  1402,  1402, -1720,  1402,   912,  1402,
     912, -1720, -1720,   651, -1720,   939, -1720, 16232, -1720, -1720,
   -1720, 16284,   761, -1720,   944,   494,  1293,   957,   477,   963,
   -1720,  2260,   965,   607,  2260,  2155, -1720,   979,  1034, 18481,
    1016,  1020, 18409, 18553,  1033, -1720, -1720, -1720, -1720, -1720,
   -1720, -1720, 18625, 18625, 14048,  1038,  3721, -1720, -1720, -1720,
   -1720,  1050, -1720,  1064, -1720,   884, -1720, 18409, 18409, -1720,
    1036,   371,   572,   707,   432,   710,  1049,  1067,  1057,  1131,
     218, -1720,   666, -1720,  1103, -1720,   708,  3934, 14510, -1720,
   -1720,   699,  1103, -1720, -1720,   703, -1720, -1720,  2625,  1113,
    1119,  1124,  1128,  1135,  1137, -1720, -1720,   400,  1138, -1720,
     362,  1138, -1720, -1720, 15828, -1720,   717,  1147, 14664, -1720,
   -1720,  4399,  3293,  1162, 13740,  1181,   827,   904, -1720, -1720,
   -1720, -1720, -1720,  4411,  4424, -1720, -1720, -1720, -1720, -1720,
   -1720,  1164,  4009,  1038, 17617,  1188,  1190, -1720, -1720,  1173,
   17761,   655, -1720, -1720, -1720, 17833,  1198, -1720, -1720, -1720,
   -1720, -1720,  2919,   704,  1200,  1202,  1203,   818,  1205,  1210,
    1212,  2799, -1720, -1720,   651,  1219,   250,  1195, -1720, -1720,
    1217, -1720, -1720,   608,   966, -1720, -1720, -1720,   608, -1720,
   -1720,  3135, -1720, 14510, 14510, -1720,   912,  4321, 17609, 13582,
   -1720, -1720, -1720, -1720, -1720,   608,   966,    40, -1720, -1720,
    1402,  1214,   966,    80, -1720,   608,   966, -1720, 13948, -1720,
     912,   912, -1720, -1720,  1215,   501,  1225,   477,  1227, -1720,
   17039, -1720,   714, -1720,  1348, 17505, -1720,  4321,  8341, 13160,
   -1720, 15574, 18697, -1720, -1720, -1720, -1720, -1720,  3813,   896,
    3135, -1720, 13582,   892, -1720,  1271, -1720,  1279, -1720, -1720,
   -1720, -1720, -1720,  2260, -1720, -1720,  1353,  4296, 16284, 10401,
   -1720, 16434, -1720,   912,   912, -1720, -1720,   761, -1720,   746,
    1276,  1418, 18409,   732,  1217,  1263, -1720,   651,   651, -1720,
    1138, -1720, 16082, -1720, -1720, 17320,   912,   912, -1720,  4296,
     651, -1720, 16688, -1720, -1720, 16232, -1720,   378,  1283,   238,
    1282,  1293,   728, 15880,   729, -1720, -1720, -1720, -1720, -1720,
   -1720,   743, -1720,  1292,  1268, -1720, 14356, -1720, 16486, 16486,
   -1720, 14356, -1720, 18409, 14356, -1720, -1720, 15626, 16486, 16486,
     708,  1172,  1291,   498,  1362, -1720,   780,  1294,   741,  1295,
   -1720, 17833, 18409, 17905,  1286,  1958,  1958, -1720,  1805, -1720,
   -1720, 17977,  2116, 18409, 17977,  1958, -1720, -1720, 18409, 18409,
   18409, 18409, 18409, 18409, 18409, 18409, 18409, 18409, 18409, 18409,
   18409, 18409, 18409, 18409, 18409, 18409, 18409, 18049,  1273,   689,
    4082, 10401, -1720, -1720, -1720, -1720, -1720, -1720, -1720, -1720,
   -1720, -1720, -1720,  1303, 18409, -1720, -1720,   699,  1480, -1720,
   -1720,   651,   651, -1720, -1720, 14510, -1720,   402,  1138, -1720,
     434,  1138, -1720, -1720, -1720,  1217, -1720, -1720,  1217, 18769,
   -1720, -1720, 10401,  1309,  1312,  2550,  1432,  2826,   466,  1263,
   -1720,   651,   651,  1263,   489, -1720,   651,   651, 18409,  4411,
     986,   989,  1263,   -17, 13108, 13108,  4411, -1720, -1720, 18409,
    1173, -1720, 17617,  1320, -1720,  2421, -1720, -1720, -1720, -1720,
   -1720,   799, -1720, 13108,  1958,  4321,  1958,   676,  1318,  1321,
    1322,   804,  1325,  1326,  1327,   520,  1138, -1720, -1720,   565,
    1138, -1720, -1720, -1720,  4321,   689, -1720,  1138, 18769, -1720,
     608, 17039, -1720, -1720,   809,  1328,   821,  1332, -1720,  1336,
   -1720,   608, -1720, -1720,   608,   966,  1336, -1720,   608,  1330,
    1340,  1341, -1720, -1720, 17320, -1720,  1334, -1720, -1720, -1720,
    1958,  4411,  9895,  1433,  1337, 17407, -1720,  1147, -1720, 13108,
     824, -1720,  1336, -1720, 15880, 14510,  1329, -1720,  1329, -1720,
   -1720, -1720, -1720, 16232, -1720, 10563, 14818, -1720, 17039,  1349,
    1357,  1359, -1720,  8974,   651, -1720,   732, -1720, -1720, -1720,
   -1720,  1217, -1720, -1720, -1720,   912, -1720,  3634, -1720, -1720,
     477,  1998,  1364, -1720, 17761, -1720,  1293,  1283, -1720, -1720,
    1358,  1365,  2155, 17977, -1720,  1366,   348,  1363,  1369,  1371,
    1370,  1373, 18409,  1375,  1378,  1381, 10401, 18409, -1720, -1720,
    1439, -1720, -1720, -1720, 18409, -1720,  1384,  1385, 17689,  1002,
   -1720, 17977, -1720, -1720, -1720,  3468, -1720, -1720,   857, -1720,
   -1720, -1720, -1720,  3468, -1720, -1720,  1025,   516, -1720, -1720,
    1036,  1036,  1036,   371,   371,   572,   572,   707,   707,   707,
     707,   432,   432,   710,  1049,  1067,  1057,  1131, 18409,   925,
   -1720,  1386,  3468, -1720, -1720, 17617, -1720, 17039,  1387,  1388,
    1389,  1480, -1720, -1720, -1720, -1720, -1720, -1720, -1720, -1720,
    1217, -1720, -1720,  1217, 17039, 17039, -1720, -1720,  2550,   909,
    1390,  1392,  1394,  1395,  2316,  2826, -1720, -1720, -1720, -1720,
   -1720, -1720, -1720, -1720, -1720, -1720, -1720, -1720, -1720, -1720,
    1397, -1720,  1263, -1720, -1720, -1720, -1720, -1720, -1720, -1720,
   -1720,  1398,  1401, -1720,   250,  3468,  1037,    62, -1720, -1720,
    1400, -1720, 17761, -1720, 18409, -1720, 18121, 13108, -1720, -1720,
   -1720,  1380,   578,  1138, -1720,   600,  1138, -1720, -1720, -1720,
   -1720,  1217, -1720, -1720, -1720,  1217,   892,  1404,  1217, -1720,
   -1720, -1720, -1720, -1720, -1720, -1720,  1409, -1720, -1720,  1336,
   -1720,   608, -1720, -1720, -1720, -1720, -1720, 11680,  1410,  1408,
   -1720,   259, -1720,   518,   230, 10239,  1416, 12937,  1420,  1422,
    1139,  2766,  2977, 18193,  1423, -1720, -1720,  1424,  1425, -1720,
   -1720,   608, 18409, 18409,  1562,  1421,   529, -1720,  1504,  1431,
    1406, -1720, -1720, -1720,  9723, -1720, -1720, -1720, -1720, -1720,
    2104, -1720, -1720, -1720,  1489, -1720, -1720, -1720,  1958, -1720,
   -1720, 11527, 15122,  1434, -1720,  4411, -1720,  1413,  1435,  1437,
   -1720,  1041, -1720, -1720, -1720,  4321, -1720, -1720,  1414,  1428,
     897, 15880,   607,   607, -1720, -1720,  1038,  1147, 14664, -1720,
    1103, -1720, 10725, -1720,   610,  1138, -1720,   912,  9187, -1720,
   -1720,  1293,   651,   651,   378,   238, -1720, -1720,  1283,  1444,
    1446, -1720, -1720,   908,   541, 10401,  1958, -1720,   541, 15678,
     541, -1720, 18409, 18409, 18409, -1720, -1720, -1720, -1720, 18409,
   18409,  1438, 17617, -1720, -1720,  1441,   548, -1720,  2640, -1720,
   -1720,  1044, -1720,   -59, -1720, 17977,  1046, -1720, 17833, -1720,
   -1720, 18409,  1442,  1052,  1065,  1173, -1720,   617,  1138, -1720,
   -1720, 17039, 17039, -1720, -1720,  1447,   620,  1138, -1720,   634,
    3202,   651,   651, -1720, -1720, 17039, 17039, -1720,  1443, -1720,
   13582, 13582,  1454,  1451,  1452,  1457, -1720,  1456, 18409, 18409,
    1070,  1458, -1720, -1720, -1720, -1720, -1720, -1720,  1462, 18409,
   -1720, -1720, -1720,  1217, -1720, -1720, -1720,  1217, 17039, 17039,
     250,   651,  1079,  1464,  1469, -1720, -1720,  1470, 11833, 11986,
   12139, 15880, 16486, 16486,  1471, -1720,  1445,  1448,  2405, 12282,
   -1720,   265,  4411, -1720, -1720,  4411, -1720, 17977, 18409,   317,
     386, -1720, -1720, -1720, -1720, 18409,  1087,  1550,  1481,  1483,
   -1720,  1461, -1720,  1463, 18409,  1465, 17617,  1468, 18409, 17833,
   18409,   882, -1720,  1477,    38, -1720,    -4,  1482, -1720, -1720,
    1488, -1720,  1478, -1720,  1479,  1494, 12937,   508, 12598,   651,
     290, -1720, -1720, -1720,  1487, -1720,  1497, -1720,  1506, -1720,
    1500, -1720,  1501, -1720, -1720, -1720, -1720, 10887,  1507,  1508,
    1509, -1720,  1514, -1720, -1720, -1720,  1217, 18409, 18409,  1147,
    1512, -1720,  1283, -1720,  1503,   321, -1720,  1520, -1720, -1720,
   15880, -1720,  1519,  1515,   914, -1720,  1516, -1720, -1720, -1720,
   -1720, -1720, 17617,  1173, 17833, -1720,  1556,  3468, -1720,  1556,
    1556, -1720,  3468,  2899,  3367, -1720, -1720,  1084, -1720, -1720,
   -1720,  1527,  1525, -1720, -1720, -1720,  1217, -1720, -1720,  1526,
    1528,   651, -1720, -1720, -1720,  1217, -1720, -1720, -1720,  1529,
   -1720, -1720, -1720, -1720, -1720, -1720, -1720, -1720, -1720, -1720,
   -1720, -1720, -1720,  1531, -1720, -1720, -1720, -1720,  1535,  1542,
     651, -1720, 17039, 17039, -1720, -1720, -1720, -1720, 18409, -1720,
   -1720,  1546, -1720,  1471,  1471,  1471,   826,  1521,   343, -1720,
    4070,   353, 14510, -1720, -1720, -1720,  3846, 18409,  3216,   421,
   -1720, -1720,   -10,  1540,  1540,  4411, -1720, -1720, 17188, -1720,
    1545,  1551,   437, -1720, -1720, -1720, -1720,   915,  1554, 12937,
   10239, 12937, 10067, -1720, -1720,   438, -1720,  1173, -1720,   917,
     920,   923, -1720, -1720, -1720, -1720,   608,   882,  1559, -1720,
   -1720, 18409, -1720,  1560,   689, 10239, -1720, -1720, -1720, -1720,
   18409,  1598, -1720, 12937, -1720,   651, 13582, -1720, -1720, 15880,
   -1720, -1720, -1720, -1720, -1720,  1557, -1720, 17039, -1720, -1720,
    1563, -1720,  1564,  1565,  1567,  1293, -1720, -1720, -1720, -1720,
   18409, -1720, 15678, 18409,  1173,  1574,  1099, -1720,  1114, -1720,
    3468, -1720,  3468, -1720, -1720, -1720, -1720, 17039,  1578,  1580,
   -1720, -1720, 17039, 17039,  1582,  1583,  1118, 13266, 13424, -1720,
    1584, -1720, -1720, -1720, -1720,  1585,  1588,  1123, -1720, -1720,
   -1720, -1720,   826,  1611,   462, -1720, -1720, -1720, -1720,   651,
     651, -1720, -1720, -1720,   473, -1720,   928,  3846,   611, -1720,
    3216,   651, -1720, -1720, -1720, -1720, -1720, -1720, -1720, -1720,
   12937, -1720,    84, 18265, -1720,  1431,  1593, 18409,   398,  1591,
     423,  9558, 15880, -1720, 18409, 18409,   942,   105, -1720, 18409,
   -1720,  1599,   115, 12937, -1720, -1720,  1601, -1720, -1720,  1587,
     689,   550,  1597,  1606,  1126,  1670, -1720, -1720, -1720,  4411,
    4321, -1720, -1720,  1608,  1609, -1720, -1720, -1720,  1293,  1283,
    1618, -1720, -1720, -1720,  1619, -1720, -1720, -1720,  1129,  1140,
   -1720, -1720, -1720, -1720, -1720, -1720, -1720, -1720, -1720, -1720,
    1617, -1720, -1720,  1622,  1629, -1720, -1720, -1720,  1631,  1635,
    1636,  1611, -1720,   651, -1720, -1720, -1720, -1720, -1720,  1616,
    4070, -1720, 18409,  1632, -1720, -1720, 12701, -1720,  1620,   937,
   12937,  1431, 13898,  1431,  1621, -1720, -1720, -1720, -1720,  4505,
   18409, 12937, 10067,  1623,  1626, -1720, -1720, -1720, -1720, 16636,
   -1720,  1645,  1633,    22, 12937, -1720, 18409, 17977,   194, -1720,
   -1720, -1720,  1641, -1720, -1720,  1283,  1655, -1720, -1720, -1720,
   -1720,  1659,  1660,  1661, 13582,  1651, -1720, -1720,   652,  1138,
   -1720, -1720,   826, -1720,   264, -1720,  1141, -1720, -1720, 11209,
   -1720, -1720, -1720,  1639, -1720, 18409,  1667, 18409,   960,  1649,
     449, -1720, -1720, 18409, -1720, 11209, 16636, -1720,  4233, 16434,
    1958,  1669, -1720,  1725,  1678,   573,  1674, -1720,  1759, -1720,
     945, 12937,  1689, 12937, 12937, -1720,  1693, -1720, -1720, -1720,
   -1720, -1720, -1720, -1720, -1720,  1217, -1720, 18409, 18409, -1720,
    1224, 11368, -1720, -1720, -1720, -1720,  1431,  1694,  1696, 18409,
   18409, 18409, -1720, -1720,  1224, -1720,  1675,  3604,  3051, -1720,
   -1720, -1720,    22,  1701, 18409,  1682,    22,    22, 12937, -1720,
   -1720, 18409,  1741,  1750, -1720, 17039, -1720, -1720, 12701, -1720,
    1224, -1720,  1695,  1702,   507, -1720,  1431, -1720,  1675, 18409,
    1707,  3051,  1705,   689,  1715, -1720,   586, -1720, -1720,   947,
    1670,   286, -1720, -1720, 12819,  1721, 12701, 18409, 18337, 18409,
    1722,  1720, -1720,   608,   689,  1723, -1720,  1699,   689, -1720,
   -1720, 12937,  1803,  1726, -1720, -1720, 12819,  1431, -1720,  1431,
    1431, -1720,   608, -1720, -1720,  1155, 18409, -1720,   977, -1720,
   12937, -1720, -1720,   689,  1958,  1729,  1708, -1720, -1720, -1720,
     980, -1720, -1720,  1710,  1958, -1720, -1720
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,   404,     0,     2,   404,   421,   422,   423,   424,   425,
     426,   427,   428,   410,   412,   411,   413,     0,     0,     0,
     429,   431,   452,   432,   453,   435,   436,   450,   451,   430,
     448,   449,   433,   434,   437,   438,   439,   440,   441,   442,
     443,   444,   445,   446,   447,   454,   455,   739,   457,   530,
     531,   534,   536,   532,   538,     0,     0,     0,   404,     0,
       0,    16,   501,   507,     9,    10,    11,    12,    13,    14,
      15,   705,    93,     0,    19,     0,     2,    91,    92,    17,
      18,   755,   404,   706,   353,     0,   356,   631,   358,   367,
       0,   357,   387,   388,     0,     0,     0,     0,   484,   406,
     408,   414,   404,   416,   419,   469,   456,   392,   462,   467,
     393,   479,   394,   494,   498,   504,   483,   510,   522,   739,
     527,   528,   511,   577,   359,   360,     3,   707,   718,   409,
       0,     0,   739,   777,   739,     2,   794,   795,   796,   404,
       0,   953,   954,     0,     1,   404,     0,   404,   376,   377,
       0,   484,   398,   399,   400,   710,     0,   533,   535,   537,
     539,     0,   404,     0,   740,   741,   529,   458,   624,   625,
     623,   684,   679,   669,     0,     0,   708,     0,     0,   404,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   502,
     505,   404,   404,     0,   955,   484,   784,   802,   959,   952,
     950,   957,   352,     0,   155,   637,   154,     0,   361,     0,
       0,     0,     0,     0,     0,     0,   351,   854,   855,     0,
       0,   386,   737,   739,   733,   758,   739,   739,   735,     2,
     739,   734,   815,   739,   739,   812,     0,   477,   478,     0,
       0,   404,   404,   421,     2,   404,   368,   407,   417,   470,
       0,   499,     0,   721,     2,     0,   631,   369,   484,   463,
     480,   495,     0,   721,     2,     0,   420,   464,   471,   472,
     481,   486,   496,   500,     0,   514,     0,   699,     2,     2,
     719,   776,   778,   404,     0,     2,     2,   963,   484,   966,
     737,   737,     3,     0,   484,     0,     0,   379,   739,   735,
     734,     2,   404,     0,     0,   665,   667,   666,   668,     0,
       0,   661,     0,   651,     0,   660,   671,     0,   739,     2,
     404,   974,   405,   404,   416,   395,   462,   396,   487,   397,
     494,   491,   512,   739,   513,     0,   612,   404,   613,   928,
     929,   404,   614,   616,   501,   507,     0,   578,   579,     0,
     742,     0,   682,   670,     0,   746,    21,     0,    20,     0,
       0,     0,     0,     0,     0,    23,    25,     4,     8,     5,
       6,     7,     0,     0,   404,     2,     0,    94,    95,    96,
      97,    78,    24,    79,    36,    77,    98,     0,     0,   113,
     115,   119,   122,   125,   130,   133,   135,   137,   139,   141,
     143,   146,     0,    26,     0,   508,     2,    98,   404,   147,
     676,   627,   498,   629,   675,     0,   626,   630,     0,     0,
       0,     0,     0,     0,     0,   756,   782,   739,   792,   800,
     804,   810,     2,   961,   404,   964,     2,    91,   404,     3,
     611,     0,   974,     0,   405,   462,   487,   494,     3,     3,
     593,   597,   607,   613,   614,     2,   785,   803,   951,     2,
       2,    23,     0,     2,   637,    24,     0,   635,   638,   972,
       0,     0,   644,   633,   632,     0,     0,   723,     2,     2,
       2,     2,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   761,   818,   739,     0,   631,     2,   757,   765,
     881,   759,   760,     0,   721,     2,   814,   822,     0,   816,
     817,     0,   382,   404,   404,   468,   405,     0,   484,   404,
     956,   960,   958,   485,   703,     0,   721,   737,   362,   370,
     418,     0,   721,     2,   703,     0,   721,   680,   465,   466,
     482,   497,   503,   506,   501,   507,   525,   526,     0,   681,
     404,   621,     0,   191,   345,   404,     3,     0,   484,   404,
     720,   404,     0,   364,     2,   365,   700,   384,     0,     0,
       0,     2,   404,   737,   703,     0,     2,     0,   664,   663,
     662,   657,   415,     0,   655,   672,   460,     0,   404,   404,
     930,   405,   401,   402,   403,   934,   925,   926,   932,     2,
       2,    92,     0,   890,   904,   974,   886,   739,   739,   895,
     902,   619,   404,   492,   615,   405,   488,   489,   493,     0,
     739,   940,   405,   945,   937,   404,   942,     0,   972,   584,
       0,     0,     0,   404,     0,   754,   753,   749,   751,   752,
     750,     0,   744,   747,     0,    22,   404,    85,   404,   404,
      80,   404,    87,     0,   404,    83,    84,   404,   404,   404,
       2,    94,    95,     0,     0,   173,     0,     0,   528,     0,
     950,     0,     0,     0,     0,     0,     0,    46,     0,    52,
      53,    57,     0,     0,    57,     0,    81,    82,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   404,   156,   157,   158,   159,   160,   161,   162,   163,
     164,   165,   166,   154,     0,   152,   153,     2,   866,   628,
     863,   739,   739,   871,   509,   404,   783,   739,   793,   801,
     805,   811,     2,   786,   788,   790,     2,   806,   808,     0,
     962,   965,   404,     0,     0,     2,    92,   890,   739,   974,
     836,   739,   739,   974,   739,   851,   739,   739,     3,   615,
       0,     0,   974,   974,   404,   404,     0,     2,   646,     0,
     972,   643,   973,     0,   639,     0,     2,   642,   645,   170,
     169,     0,     2,   404,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   739,   770,   774,   813,   739,
     827,   832,   762,   819,     0,     0,   390,   878,     0,   724,
       0,   404,   725,   383,     0,     0,     0,     0,   381,     2,
     726,     0,   366,   703,     0,   721,     2,   727,     0,     0,
       0,     0,   540,   600,   405,     3,     3,   604,   603,   797,
       0,     0,   404,   346,     0,   484,     3,    91,     3,   404,
       0,     3,     2,   659,   404,   404,   653,   652,   653,   461,
     459,   578,   936,   404,   941,   405,   404,   927,   404,     0,
       0,     0,   905,     0,   739,   975,   891,   892,   620,   888,
     889,   903,   931,   935,   933,   490,   525,     0,   939,   944,
     581,   973,     0,   154,     0,   580,     0,   972,   685,   683,
       0,     0,   746,    57,   709,     0,     2,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   404,     0,   112,   111,
       0,   108,   107,    27,     0,    28,     0,     0,     0,     0,
       3,    57,    42,    43,    50,     0,    49,    61,     0,    58,
      59,    62,    45,     0,    44,    48,     0,     0,    41,   114,
     116,   117,   118,   120,   121,   123,   124,   128,   129,   126,
     127,   131,   132,   134,   136,   138,   140,   142,     0,     0,
     355,     0,     0,    29,     3,   637,   148,   404,     0,     0,
       0,   867,   868,   864,   865,   678,   677,     2,   787,   789,
     791,     2,   807,   809,   404,   404,   883,   882,     2,     0,
       0,     0,     0,     0,   739,   891,   839,   856,     2,   834,
     842,   617,   837,   838,   618,     2,   849,   859,   852,   853,
       0,     3,   974,   374,     2,   967,     2,   608,   609,   587,
       3,     3,     3,     3,   631,     0,   146,     0,     3,     3,
       0,   640,     0,   634,     0,   722,     0,   404,     3,   378,
     380,     0,   739,   771,   775,   739,   828,   833,     2,   763,
     766,   768,     2,   820,   823,   825,   737,     0,   879,     3,
     729,     3,   474,   473,   476,   475,     2,   704,   730,     2,
     728,     0,   704,   731,   540,   540,   540,   404,     0,     0,
     622,     0,   349,     0,     0,   404,     0,     2,     0,     0,
       0,     0,     0,   175,     0,   279,   280,     0,     0,   318,
     317,     0,   150,   150,   324,   501,   507,   189,     0,   176,
       0,   199,   177,   178,   404,   193,   179,   180,   181,   182,
       0,   183,   184,   285,     0,   185,   186,   187,     0,   188,
     195,   484,   404,     0,   197,     0,   343,     0,     0,     0,
       3,     0,   704,   692,   693,     0,     3,   688,     3,     3,
       0,   404,   669,   669,   938,   943,     2,    91,   404,     3,
     499,     3,   405,     3,   739,   898,   901,   404,     3,   887,
     893,     0,   739,   739,     0,   584,   569,   585,   972,     0,
       2,   743,   745,     0,    86,   404,     0,    90,    88,   404,
       0,   102,     0,     0,     0,   106,   110,   109,   174,     0,
       0,     0,   637,    99,   167,     0,     0,    75,     0,    75,
      75,     0,    63,    65,    40,     0,     0,    38,     0,    39,
     145,     0,     0,     0,     0,   972,     3,   739,   874,   877,
     869,   404,   404,     3,     3,     0,   739,   845,   848,   739,
       0,   739,   739,   840,   857,   404,   404,   968,     0,   610,
     404,   404,     0,     0,     0,     0,   363,     3,     0,     0,
       0,     0,   636,   641,     3,   172,   171,     3,     0,     0,
       2,   764,   767,   769,     2,   821,   824,   826,   404,   404,
     631,   739,     0,     0,     0,   704,   732,     0,   404,   404,
     404,   404,   404,   404,   523,   551,     3,     3,   552,   484,
     541,     0,     0,   779,     2,     0,   347,    57,     0,     0,
       0,   270,   271,   196,   198,     0,     0,     0,     2,     2,
     266,     0,   264,     0,     0,     0,   637,     0,     0,     0,
       0,     0,   151,     0,     0,   325,     0,     0,     3,   202,
       0,   194,     0,   261,     0,     0,     2,     0,   484,   739,
       0,   344,   885,   884,     0,     2,     0,   695,     2,   690,
       0,   691,     0,   673,   654,   658,   656,   404,     0,     0,
       0,     3,     0,     2,   894,   896,   897,     0,     0,    91,
       0,     3,   972,   574,     0,   584,   582,     0,   572,   686,
     404,   748,     0,     0,     0,    32,     0,   103,   105,   104,
     101,   100,   637,   972,     0,    56,    72,     0,    66,    73,
      74,    51,     0,     0,     0,    60,    47,     0,   144,   354,
      30,     0,     0,     2,   870,   872,   873,     3,     3,     0,
       0,   739,     2,   841,   843,   844,     2,   858,   860,     0,
     835,   850,     3,     3,   969,     3,   595,   594,   598,   971,
       2,     2,   970,     0,     3,   736,   647,   648,     0,     0,
     739,   385,   404,   404,     3,     3,   391,   738,     0,   829,
     713,     0,   715,   523,   523,   523,   558,   528,     0,   564,
     552,     0,   404,   515,   550,   546,     0,     0,     0,     0,
     553,   555,   739,   566,   566,     0,   547,   562,   404,   350,
       0,    58,     0,   274,   275,   272,   273,     0,     0,     2,
     404,     2,   404,   267,   265,     0,   259,   972,   268,     0,
       0,     0,   306,   307,   308,   309,     0,   299,     0,   300,
     276,     0,   277,     0,     0,   404,   204,   192,   263,   262,
       0,   297,   316,     2,   348,   739,   404,   711,   674,   404,
       2,     2,   946,   947,   948,     0,   899,   404,     3,     3,
       0,   907,     0,     0,     0,     0,   583,   571,     3,    89,
       0,    31,   404,     0,   972,     0,     0,    76,     0,    64,
       0,    70,     0,    68,    37,   149,   875,   404,     0,     0,
     780,   798,   404,   404,     0,     0,     0,   404,   404,   650,
       0,   371,   373,     3,     3,     0,     0,     0,   717,   519,
     521,   517,     0,   914,     0,   559,   919,   561,   911,   739,
     739,   545,   565,   549,     0,   548,     0,     0,     0,   568,
       0,   739,   542,   556,   567,   557,   563,   602,   606,   605,
       2,   203,     0,     0,   230,   211,     0,     0,   213,   358,
     212,   484,   404,   234,     0,   175,   240,     0,   235,   175,
     260,     0,     0,     2,   283,   310,     0,   301,     2,     0,
       0,     0,     0,   288,     0,   284,   190,   372,   689,     0,
       0,   949,     3,     0,     0,   906,   908,   573,     0,   972,
       2,    35,    33,    34,     0,    54,   168,    67,     0,     0,
       3,   781,   799,     3,     3,   846,   861,   375,     2,   592,
       3,   591,   649,     0,     0,   772,   830,   880,     0,     0,
       0,   915,   916,   739,   544,   912,   913,   543,   524,     0,
       0,   282,     0,     0,     2,   222,     2,   205,     0,     0,
       2,   214,   484,   241,     0,   256,   257,   258,   255,   244,
       0,     2,   404,     0,     0,     2,   207,   281,     2,   404,
     278,     0,     0,   326,     2,   286,     0,    57,     0,   298,
     694,   696,     0,   909,   910,   972,     0,   687,    55,    71,
      69,     0,     0,     0,   404,     0,   773,   831,   739,   922,
     924,   917,     0,   554,   215,   218,     0,   217,   221,   404,
     224,   223,   232,     0,     3,   175,   249,     0,   245,     0,
     242,     3,   236,   175,   269,   404,   404,     3,   311,   405,
     315,     0,   319,     0,     0,     0,   327,   328,   209,   289,
       0,     2,     0,     2,     2,   900,     0,   576,   876,   847,
     862,   596,     2,   918,   920,   921,   560,     0,     0,   220,
     225,   404,   339,   231,   229,   237,   246,   257,   255,     0,
     175,     0,   233,   239,   225,     3,   304,     0,   914,   312,
     313,   314,   326,     0,     0,     0,   326,     0,     2,   287,
     294,     0,   291,   293,   575,   404,   216,   219,     2,     3,
     226,   340,   251,   250,   247,   238,   243,     3,   304,     0,
       0,   915,     0,     0,     0,   320,     0,   329,   210,     0,
     284,     0,     3,   200,   227,     0,     2,     0,     0,     0,
       0,     0,   305,     0,   332,     0,   330,     0,   332,   290,
     292,     2,     0,     0,   201,   206,   228,   253,   254,   252,
     248,   208,     0,   302,   333,     0,     0,   321,     0,   295,
       2,   923,   303,     0,     0,     0,     0,   296,   334,   335,
       0,   331,   322,     0,     0,   323,   336
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1720,  5499,  5755, -1720,    -1,   379,  1069,  -169, -1720,  1505,
   -1720,   296, -1720,  -668,   558,   656,  -828,  -812, -1720,    85,
    5131,  1683, -1720,   245, -1720,  1229,   349,   626,   636,   322,
     622,  1194,  1196,  1201,  1193,  1218, -1720,  -142,  -152,  7985,
     777, -1720,  -396, -1720, -1720,  -647,  4128,  -943,   628, -1720,
    -126, -1720,   770,   -15, -1720, -1720, -1720,   368,    57, -1720,
   -1569, -1448,   244,    43, -1720, -1720, -1720,   158,   103, -1720,
   -1720, -1720, -1720,     3, -1627,   144, -1720, -1720,     6, -1720,
   -1720, -1720,    20,   392,   393,   107, -1720, -1720, -1720, -1720,
   -1719, -1720,    49,    -3, -1720,   109, -1720,    92, -1720, -1720,
   -1720,   792,  -804,  -861, -1277, -1720,    14,    37,   310,  7381,
    -700,  -669, -1720,  -263, -1720,    63,  -129,    24,  -270,  -231,
    3290,  6849,  -634, -1720,   118,    12,   237,  2097, -1720,  1900,
   -1720,   182,  3540, -1720, -1720, -1720,   221, -1720, -1720,   771,
     261,  4137,  2485,   -24,  1709,  -279, -1720, -1720, -1720, -1720,
   -1720,  -334,  3719,  4909, -1720,  -353,    82, -1720,   451,   202,
   -1720,   145,   647, -1720,   447,  -175, -1720, -1720, -1720,  5078,
    -586, -1088,  -685,  -405,   -35,  1185, -1720, -1191,  -147,   -14,
    1108,   814,  4434,  -125,  -448,  -244,  -184,  -437,  1183, -1720,
    1492,   401,  1098,  1391, -1720, -1720, -1720, -1720,   169,  -156,
      47,  -836, -1720,    87, -1720, -1720,   557,   399, -1720, -1720,
   -1720,  1970,  -714,  -482, -1034,    66, -1720, -1720, -1720, -1720,
   -1720,   -29,  -748,  -119, -1696,  -203,  6989,   -66,  6592, -1720,
    1063, -1720,  1950,   -20,  -197,  -177,  -166,     1,   -72,   -69,
     -67,   323,    11,    32,   141,  -144,   -86,   -89,   -88,   -79,
    -683,  -635,  -632,  -629,  -680,  -101,  -628, -1720, -1720,  -681,
    1253,  1257,  1260,  2398,  7211,  -571,  -530,  -513,  -508,  -718,
   -1720, -1557, -1588, -1583, -1559,  -578,   -60,  -300, -1720, -1720,
     -47,   137,   -76, -1720,  7885,   186,  -614,  -313
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,  1128,   213,   381,   382,    80,    81,   383,   358,   384,
    1414,  1415,   385,   948,   949,   950,  1231,  1232,  1233,  1426,
     407,   387,   388,   389,   663,   664,   390,   391,   392,   393,
     394,   395,   396,   397,   398,   399,   400,   409,  1047,   665,
    1353,   724,   207,   726,   403,   791,  1129,  1130,  1131,  1132,
    1133,  1134,  1135,  1934,  1136,  1137,  1358,  1666,  1815,  1816,
    1755,  1756,  1757,  1909,  1910,  1138,  1677,  1678,  1770,  1139,
    1140,  1141,  1142,  1143,  1144,  1366,  1694,  1854,  1789,  1145,
    1146,  1547,  1920,  1548,  1549,  1837,  1147,  1148,  1149,  1356,
    1845,  1846,  1847,  1965,  1980,  1870,  1871,   284,   285,   852,
     853,  1101,    83,    84,    85,    86,    87,  1669,   440,    90,
      91,    92,    93,    94,   221,   557,   442,   411,   443,    97,
     294,    99,   100,   101,   323,   324,   104,   105,   166,   106,
     870,   325,   152,   109,   241,   110,   153,   250,   327,   328,
     329,   154,   404,   115,   116,   331,   117,   548,   841,   839,
     840,  1503,   332,   333,   120,   121,  1097,  1320,  1509,  1510,
    1634,  1635,  1321,  1498,  1653,  1511,   122,   630,  1584,   334,
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
     610,  1637,  1638,  1639,  1640,   595,   453,   339,   340,   341,
     416,   199,   141,   142,   143,   343,   783,   611
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      79,   183,   131,    79,   184,   280,   185,   181,   357,   231,
     515,   725,   528,   103,   902,   335,   957,   353,   148,   496,
     556,   668,   484,   474,   939,    96,   402,   780,   150,  1170,
     932,   297,   887,   321,   893,   198,   401,   623,    88,  -697,
    1020,   626,   485,   613,  1738,   907,  1027,   992,  1150,  1739,
     590,   190,   836,   486,    79,    79,  1817,    79,  1307,   131,
      57,  1433,   349,   289,    95,   825,   827,   621,  1788,   879,
     103,   624,    79,  1740,  1016,   487,  1742,  1017,  1553,  1466,
    1467,    79,    96,   196,   186,   477,   880,   563,   565,    79,
    1043,   881,   862,  1434,    79,    88,   228,    79,   484,   253,
     198,    79,   338,   263,  1154,   187,   194,  1406,  1058,   419,
     248,   278,   420,   425,   421,   435,   256,   512,   485,   102,
    1010,    95,  1092,  1011,   176,  1236,  1012,  1013,  1376,   486,
     488,   489,   564,  1551,   493,   209,   495,   356,   140,    79,
     490,   140,    79,   497,    79,  1554,   131,   183,  1162,    79,
     184,   487,   185,   482,  1243,    79,   602,   103,   560,  1752,
    1753,   209,    79,   287,    57,   613,  1050,  -341,   590,    96,
    1347,   194,  1817,  1922,  1160,  1843,   102,  1926,   452,    79,
      79,   196,    88,   107,  1811,   909,   887,  1821,  -342,   564,
    1752,  1753,   422,  1278,    79,   140,   456,   144,   521,   503,
     492,    57,   508,   210,   465,  -721,   488,   489,    95,    79,
     493,   292,  1552,   423,   188,   433,   490,  1277,    79,    79,
    -698,   196,   111,   183,   525,   879,   184,  1279,   185,   568,
     186,  -721,   155,   904,   535,    79,  1315,  -341,   156,   140,
     107,  1754,   880,   162,    79,  1203,   196,   881,   667,   669,
     543,   187,   816,  1668,    79,   521,  1771,    79,  -342,   532,
     596,  1772,   112,   102,    79,   161,   517,  1851,   201,   520,
     530,  1491,  1775,  1226,    79,    79,   492,    79,  1668,   111,
     278,  1890,   140,  1365,   497,   798,   784,   996,   874,  1329,
    1330,  1020,   888,  1199,    79,    79,  1217,   196,   856,  1738,
    1852,  1333,    79,  1788,  1739,   799,    57,    96,    79,    79,
    1250,    89,   613,    79,   149,  1190,   800,  1586,   872,   112,
    1198,   504,   424,   260,   135,   899,   520,   107,  1740,   955,
    1150,  1742,  1263,   201,   248,  1264,   613,    19,   801,  1821,
     266,   764,   892,   613,   267,    79,   526,   270,   706,   272,
      79,  1089,   537,    79,   640,   898,   536,  1334,   751,  1951,
     188,   198,   585,   549,  1811,   417,   111,  1821,    89,  1041,
    1041,   798,  1287,  1010,  1118,  1305,  1011,   819,   596,  1012,
    1255,   135,   822,   146,   204,   505,  1154,   279,  1041,   497,
     707,   799,   823,   802,   803,  1867,   204,  1316,   828,   830,
     585,   559,   800,   804,  1331,   813,   112,   205,   246,   837,
     419,   903,   257,   420,  1325,   421,  1428,    79,   229,   456,
    1515,   254,  1908,   206,   801,   264,  1466,  1467,  1317,  1279,
      57,   522,   194,  1326,   170,   170,  1908,   175,   190,  1516,
      79,    79,   177,   321,  1190,  1325,  1021,  1315,  1315,  1315,
    1024,   178,    79,    79,  1041,    89,   248,   578,   750,  1037,
    1038,    79,  1936,   465,  1564,   107,  1395,   204,   135,   170,
      57,   812,    57,   274,   279,   820,   189,    63,   879,   802,
     803,    79,    62,    63,   813,   266,   579,   580,   522,   804,
      79,  1523,   456,   422,   903,   880,   419,   831,  1642,   420,
     881,   421,   191,   834,   111,   452,   598,   838,  1515,   571,
      79,   746,   338,   497,   423,   497,    79,  1643,   457,   170,
     667,   915,   170,   917,   918,   667,   919,  1645,   667,   921,
      75,   689,   923,   924,   925,   170,    57,   861,   690,   691,
     981,  1458,   347,   279,   112,   542,    63,   667,  1245,   742,
     812,   997,   596,   497,    79,   497,    79,   696,   697,    57,
    1525,   266,   267,   179,   617,   613,   272,    79,   529,    79,
     248,   456,   202,  1175,   157,   452,  1651,   158,   159,   150,
     160,   533,    79,  1001,  1407,    96,    79,   497,    -3,   170,
      57,  1437,   934,   934,   201,  1652,   613,   216,  1051,  1598,
     893,   698,   699,  1174,   934,  1402,   236,   650,  1316,  1316,
    1316,  1661,  1680,  1861,  1881,  1018,  1031,  1743,    79,   600,
    1599,  1601,  1603,   424,   598,   835,   401,   530,  1651,   274,
      79,  1441,   686,   687,   170,    57,  1744,  1046,  1025,  1317,
    1317,  1317,   600,   179,   170,  -398,  1077,  1747,    57,   927,
     179,  -855,  1041,   686,   546,   170,   764,   551,  -520,  1520,
     928,   929,   934,   850,  1239,   279,  1646,  1327,  1328,  1068,
      57,  1235,  1939,   497,    79,    79,  1060,    79,   179,   559,
      57,    79,   170,   686,    79,  -625,   543,    57,   170,   170,
      57,  1221,   276,   170,  1080,  1076,  1425,   452,  1222,   417,
     417,  1782,  1540,  1235,    57,  1088,  1783,   876,  1090,    79,
     237,   238,  1093,   239,  1072,   586,   274,   240,   497,  1269,
    1197,    57,    57,  1384,  1895,   170,  1667,  1290,  1679,  1896,
     170,   497,  1764,   170,   692,   693,  1773,  1947,   452,  1042,
    1042,   457,  1948,   107,    13,    14,    15,    16,    17,  1294,
     278,  1667,   632,   497,    79,   634,    79,   293,  1042,  1393,
     452,   452,   425,   600,   497,   279,  1443,   266,    79,  1452,
     497,  -341,  1718,   497,  1719,    79,   351,  1596,   310,   452,
     260,   465,   111,  1456,    79,  1423,   157,   600,  1583,   158,
     159,   426,   160,    79,    79,    79,  1193,   321,   894,   904,
    1276,  1862,    57,    72,   356,   497,  1091,   785,   786,  1595,
     427,   787,   582,    79,   457,   460,   583,   708,   354,   417,
     170,   709,   112,   599,  1042,   278,  1829,   600,    72,   497,
     694,   695,   170,   170,    77,   601,  1240,   700,   701,    13,
      14,    15,    16,    17,   355,   452,   455,   602,   727,    79,
      79,   465,   497,   278,   734,   425,   248,   497,   735,    77,
      78,    72,   191,   671,   103,   849,   338,   530,  1283,   850,
     876,   588,   671,  1300,   428,    72,    96,   429,  1163,   908,
     910,   599,  1875,   583,   583,   600,   430,   248,  1165,    88,
    1883,   861,    77,   601,   911,   599,    79,    57,   912,   600,
      79,   936,   937,   613,   431,    79,    77,    78,   892,  1537,
     459,   640,  1046,  1262,   764,    95,    13,    14,    15,    16,
      17,   417,   473,  1681,    13,    14,    15,    16,    17,  -399,
     475,   933,   478,   959,   146,   934,  -400,  1915,    13,    14,
      15,    16,    17,   479,    79,    13,    14,    15,    16,    17,
    1055,   480,    79,   505,  1056,    72,  1397,   497,  -402,  1497,
    1082,   481,   170,   446,   934,  1306,   871,   505,  1679,   808,
    1152,   497,  1084,   571,    57,  1632,   934,   497,  1332,   497,
    1714,    79,    57,   494,   465,  1594,    77,    78,   495,   140,
    1542,  1543,  1544,  1545,  1546,  1351,    57,   513,   896,  1709,
     514,   140,   677,    57,   678,   679,   680,    79,  1234,   904,
     170,   524,  1235,    79,    79,  1872,   353,   353,   967,   968,
     969,   970,  1042,   243,     6,     7,     8,     9,    10,    11,
      12,  1872,   534,   681,   107,  -403,   682,   683,   960,   961,
     962,   684,   685,   452,    79,   571,  1377,   425,  1383,   497,
    1322,   417,   735,   209,   942,   943,  1486,   946,  1018,  1411,
     425,   954,   600,  1235,   958,  1591,  1662,  1911,  1682,  1592,
     934,  1683,   934,   111,  1684,  1056,   321,   553,   934,  1748,
     934,  1241,   265,   735,  1416,  1765,  1766,  1767,  1823,   983,
     620,   593,   934,   574,   616,  1796,  1899,   934,  1949,  1438,
    1235,  -854,   934,  1765,  1877,  1767,   465,  1768,   593,    79,
      79,    79,   593,   112,  -570,   934,  1769,   103,   596,  1850,
     631,  1318,  1795,   588,  1468,  1878,  1474,  1475,  1976,    96,
     644,  1983,  1973,   465,  -176,  1984,   401,   401,   633,    79,
    1033,  1034,    88,  1035,  1036,   338,   103,    79,   170,   645,
      79,    79,   253,   263,    79,   170,  1224,  1056,    96,  1629,
    1630,  1631,    89,   248,    79,   648,   256,  1211,    95,   649,
     861,    88,  1215,  1059,  1164,  1061,  1308,  1309,  1310,  1237,
    1238,  1856,   653,  1223,  -112,  -112,  -112,  -112,  -112,  -112,
      79,  -147,  -147,   671,   530,  1035,  1375,    95,  1431,  1432,
    1436,  1432,   688,  1524,  1526,    79,  1440,  1432,   675,   593,
     702,   243,     6,     7,     8,     9,    10,    11,    12,  1007,
    1424,   465,   676,  1152,  1476,  1424,   703,    79,   704,  1100,
     170,   170,  1514,  1007,  1488,  1465,  1327,  1328,  1604,  1056,
      61,  1562,   140,   168,   169,    64,    65,    66,    67,    68,
      69,    70,  1152,  1716,  1056,   710,   452,   452,   321,    79,
     705,  1322,  1322,  1322,   736,  1499,  1322,  1670,  1717,  1432,
     737,   140,  1727,  1728,   466,   738,  1192,  1737,   934,   739,
     170,  1786,  1787,  1799,  1432,   170,   740,   107,   741,   140,
     446,   432,  1670,   768,  1800,  1432,  1868,  1869,  1513,  1752,
    1753,    -3,   627,  -111,  -111,  -111,  -111,  -111,  -111,   484,
    1973,  1974,  -401,  1340,  1429,  1430,   107,    79,   963,   964,
     -16,    79,   971,   972,    79,   148,   111,   338,   782,   485,
     965,   966,  1318,  1318,  1318,   150,  1496,  1500,  1654,  1654,
     486,  1385,  1386,   446,   -17,   465,   781,   792,    18,   817,
     650,   805,   861,   806,   807,   111,   809,   417,  -389,   593,
     446,   810,   487,   811,   894,   465,   112,    79,   815,   532,
     286,   833,  -518,   260,    13,    14,    15,    16,    17,   931,
     530,  -389,  -516,   593,   842,  1691,    47,    48,    49,    50,
      51,    52,    53,    54,    61,   112,   593,   168,   169,    64,
      65,    66,    67,    68,    69,    70,   243,     6,     7,     8,
       9,    10,    11,    12,  1514,    89,   851,   488,   489,  1468,
    1685,   465,   493,   686,  1163,   863,    79,   490,   865,   869,
     882,    79,    79,    79,  1165,   884,   602,   321,   901,   906,
    1647,   913,   914,   941,    89,   935,   938,   980,   140,  1014,
     798,    13,    14,    15,    16,    17,  1216,  1417,  1418,  1419,
     985,   246,   257,  1006,  1420,  1421,  1007,  1416,  1053,  1062,
     799,  1468,  1063,  1064,   254,   264,  1065,  1066,  1067,  1083,
    1513,   800,   446,  1085,  -701,  -601,   170,  1094,   492,   170,
     170,   170,    13,    14,    15,    16,    17,  1095,  1096,    79,
    1184,  1155,  1171,   801,  1658,    79,   338,    79,  1185,  1838,
    1186,  1156,  1196,   170,    79,  1200,  1201,  1204,  1206,   170,
    1207,  1781,  1208,   446,  1210,  1209,  1212,   551,   465,  1213,
     465,   778,  1214,   466,   170,  1219,  1220,  1242,  1247,  1248,
    1249,  1256,   103,  1257,   103,  1258,  1259,   140,  1282,  -589,
      57,  1267,  -588,  1289,    96,  1301,    96,  -702,   802,   803,
     613,  1323,   465,  1324,   813,  1335,  1838,   103,   804,  1338,
     170,  1339,  1348,  1349,  1350,  1355,  1357,  -624,  1365,    96,
    1359,  1791,  1730,  1163,    79,  1412,   934,  1371,  1379,  1372,
    1369,  1373,  1408,  1165,  1409,  1422,  1424,  1464,  1451,    79,
    1814,    79,  1381,   452,   452,  1469,  1470,  1471,  1472,    72,
     401,  1432,  1477,  1480,  1844,  1489,  1439,  1490,  1492,  1504,
    1502,   149,  1505,    13,    14,    15,    16,    17,  1528,   727,
     812,  1555,  1529,   497,  1531,  1533,  1557,  1534,  1565,  1536,
      77,    78,  1538,  1560,   593,  1567,    79,   616,  1672,    79,
    1672,  1550,  1558,  1559,  1568,  1570,  1571,  1468,   848,   465,
    1585,  1572,  1573,  1574,  1514,  1576,  1581,   140,  1587,   140,
    1589,  1590,  1593,  1672,  1597,  1605,  1606,  1610,   529,  1611,
     425,    57,   465,   248,    82,  1619,   256,   147,   417,  1476,
     484,   533,   140,  1621,  1628,  1641,  1507,   446,    79,    79,
    1235,   170,  1660,  1663,   170,  1695,   140,    79,  1688,  1690,
     485,  1701,   107,  1707,   107,  1906,  1814,  1705,  1706,  1889,
    1164,   486,  1715,  1844,  1708,   401,   401,  1844,  1844,  1721,
    1513,  1722,  1840,  1725,  1726,  1337,  1735,   107,  1732,  1736,
      72,    82,  1924,   487,  1760,   170,   210,  1774,  1784,    79,
    1778,   111,   401,   111,  1945,   465,   180,  1785,  1118,   465,
    1632,  1780,  1793,  1794,   497,    82,  1797,  1798,  -590,   497,
     465,    77,    78,  1806,   530,  1964,   111,  1942,   220,  1964,
    1807,   245,  1808,   465,   103,    82,  1809,  1810,  1818,  1840,
     452,   112,  1855,   112,  1822,  1825,    96,  1833,   488,   489,
    1834,  1841,   493,  1857,  1978,  1693,  1728,  1842,   490,   140,
    1858,  1859,  1860,  1873,  1975,   183,   112,  1963,   184,  -501,
     185,   568,   147,  1880,   401,  1892,  1893,  1894,    82,  1897,
     147,   103,  1898,   296,   302,   813,  1972,    79,  1901,    79,
      89,  1904,    89,    96,  1912,   320,  1913,   103,  1930,  1919,
     465,   466,   465,   465,   778,  1923,  1925,  1931,  1943,    96,
    1937,  1944,   408,   180,   180,    89,  1946,  1938,   492,  1955,
    1961,  1962,  1966,  1967,   147,   438,  1970,  1971,   245,  1164,
    1981,   674,  1982,   103,  1985,  1521,    79,    79,  1712,   196,
    1672,  1435,   930,   260,   170,    96,   973,   465,   976,   974,
    1354,   812,   220,   220,  1361,   975,    61,   465,   170,   140,
     170,    64,    65,    66,    67,    68,    69,    70,   944,   296,
      79,  1956,   456,  1692,   977,  1907,  1776,  1917,    82,   848,
    1832,  1879,  1853,   465,  1952,   465,  1950,  1672,  1941,  1686,
    1687,   245,   170,  1885,  1884,  1968,  1927,  1370,   167,   593,
     465,  1644,  1813,  1672,   107,   465,   140,  1866,   945,   523,
    1501,  1655,  1367,   788,   170,  1052,  1173,  1588,  1698,   465,
       3,   302,   140,    79,   867,  1202,   446,   302,   296,   296,
     988,   246,   257,    79,   989,   147,   848,   990,     0,  1672,
       0,     0,     0,   111,  1561,     0,     0,     0,     0,     0,
       0,   107,     0,   320,   603,   612,  1081,  1194,   140,     0,
       0,     0,     0,     0,  1230,     0,     0,   107,     0,     0,
     320,     0,  1230,   182,   320,     0,     0,     0,     0,     0,
       0,  1849,     0,   112,     0,     0,     0,     0,     0,   170,
     111,     0,     0,     0,     0,   223,     0,     0,     0,     0,
       0,  1230,     0,   107,   466,     0,   111,   408,     0,    61,
       0,     0,   170,  1183,    64,    65,    66,    67,    68,    69,
      70,     0,   529,     0,     0,     0,     0,     0,   170,     0,
     112,     0,    89,     0,     0,     0,     0,   170,     0,     0,
       0,   408,   111,     0,   728,   848,   112,     0,     0,    61,
     298,   180,   168,   169,    64,    65,    66,    67,    68,    69,
      70,     0,   848,   848,  1230,     0,     0,   147,     0,     0,
       0,   438,     0,     0,     0,   757,     0,   612,     0,    89,
       0,     0,   112,    18,     0,   170,     0,     0,     0,   170,
       0,     0,     0,     0,     0,    89,     0,     0,     0,     0,
     170,     0,     0,     0,     0,     0,     0,  1664,   446,  1673,
       0,     0,     0,   170,     0,   220,     0,   635,     0,   483,
     223,     0,  1246,     0,   220,    51,    52,    53,    54,     0,
    1362,    89,     0,     0,     0,     0,   298,     0,     0,  1253,
    1254,  1696,     0,     0,   296,   249,   408,   408,     0,     0,
     296,     0,   320,     0,     0,    61,   269,     0,   168,   169,
      64,    65,    66,    67,    68,    69,    70,    61,     0,  1891,
       0,     0,    64,    65,    66,    67,    68,    69,    70,   952,
     170,     0,   170,   170,     0,     0,     0,     0,     0,     0,
     296,   636,     0,     0,     0,   569,   298,     0,   249,     0,
       0,   296,     0,   296,     0,   320,   637,     0,     0,   638,
     639,    64,    65,    66,    67,    68,    69,    70,     0,   953,
       0,   320,   438,     0,   612,     0,     0,   170,  1363,     0,
       0,     0,   603,     0,     0,     0,   603,   170,  1751,     0,
       0,   466,   249,     0,     0,   320,     0,  1230,     0,     0,
       0,     0,     0,     0,     0,   612,     0,     0,   320,     0,
       0,  1777,     0,   170,     0,   170,   147,     0,     0,     0,
       0,     0,     0,   305,   306,   307,   308,     0,     0,   408,
     170,   147,   147,     0,   408,   170,     0,   408,     0,     0,
     147,   147,   147,     0,     0,     0,     0,     0,     0,   170,
       0,     0,     0,  1979,     0,   249,     0,     0,     0,   848,
     848,    61,     0,  1986,   168,   169,    64,    65,    66,    67,
      68,    69,    70,   848,   848,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1820,   249,    57,     0,  1824,     0,
       0,   249,   758,     0,   438,     0,     0,     0,     0,  1831,
       0,     0,     0,     0,     0,     0,   848,   848,   236,     0,
     728,   728,  1848,   309,     0,   466,     0,    61,   408,   249,
     217,   218,    64,    65,    66,    67,    68,    69,    70,     0,
       0,   310,   797,     0,     0,   438,  1447,  1448,   757,     0,
     757,   223,     0,     0,     0,     0,     0,     0,     0,     0,
    1462,  1463,     0,     0,     0,     0,     0,   320,   320,     0,
       0,   298,     0,     0,     0,  1260,    74,   298,     0,     0,
       0,     0,     0,     0,     0,     0,   320,     0,   296,  1900,
       0,  1902,  1903,  1484,  1485,     0,   114,     0,     0,   114,
       0,   466,     0,     0,     0,     0,  1230,   296,     0,     0,
       0,  1230,  1230,  1230,     0,     0,    61,   298,     0,   217,
     218,    64,    65,    66,    67,    68,    69,    70,   860,     0,
     298,     0,    61,     0,     0,     0,  1928,    64,    65,    66,
      67,    68,    69,    70,    72,   408,  1933,     0,     0,     0,
       0,   249,   320,   114,     0,     0,     0,   147,   408,     0,
     593,     0,     0,     0,  1506,    74,   320,     0,  1178,     0,
       0,  1507,  1954,    57,  1933,    77,    78,   114,     0,   603,
       0,    74,     0,     0,   777,     0,     0,     0,     0,  1969,
       0,     0,     0,   251,  1954,     0,     0,   114,     0,     0,
     848,   848,     0,     0,    61,     0,     0,     0,  1977,    64,
      65,    66,    67,    68,    69,    70,     0,   593,     0,   438,
       0,     0,     0,     0,     0,   249,     0,     0,     0,     0,
       0,     0,    72,     0,   114,     0,  1659,     0,   507,     0,
     114,     0,   114,     0,     0,   249,   251,    13,    14,    15,
      16,    17,    73,    74,     0,     0,   317,   114,   348,     0,
       0,    61,     0,    77,    78,   249,    64,    65,    66,    67,
      68,    69,    70,     0,   412,     0,     0,  1623,  1624,  1230,
       0,  1230,     0,     0,   728,     0,   114,   412,     0,    72,
     251,     0,     0,     0,     0,   848,     0,     0,   249,     0,
       0,   757,     0,     0,     0,    57,     0,     0,   757,  1008,
      74,     0,     0,   600,     0,  1009,     0,   758,     0,     0,
      77,    78,   249,     0,     0,   848,     0,     0,     0,   249,
     848,   848,     0,     0,     0,     0,    61,   114,     0,     0,
     114,    64,    65,    66,    67,    68,    69,    70,     0,     0,
     320,    61,     0,   251,     0,   298,    64,    65,    66,    67,
      68,    69,    70,  1227,    72,     0,     0,  1228,     0,  1229,
     547,     0,  1702,     0,   298,     0,     0,     0,   114,     0,
       0,     0,     0,   251,    73,    74,     0,     0,     0,   251,
     147,     0,     0,     0,     0,    77,    78,   114,   408,     0,
      74,     0,  1720,  1427,     0,     0,     0,  1723,  1724,     0,
       0,     0,     0,     0,     0,   114,     0,   251,   114,   733,
       0,    13,    14,    15,    16,    17,     0,   408,     0,     0,
       0,     0,   114,     0,     0,   744,   114,     0,   747,     0,
       0,     0,     0,     0,   245,    82,     0,     0,    13,    14,
      15,    16,    17,     0,     0,     0,     0,     0,   296,     0,
       0,     0,     0,     0,   147,     0,     0,     0,     0,   412,
       0,   438,     0,     0,     0,     0,     0,    61,     0,    57,
     168,   169,    64,    65,    66,    67,    68,    69,    70,     0,
       0,     0,     0,     0,     0,   507,     0,     0,   438,     0,
       0,     0,   147,   412,     0,     0,    57,     0,     0,     0,
      61,     0,     0,   217,   218,    64,    65,    66,    67,    68,
      69,    70,     0,     0,     0,     0,     0,     0,     0,   114,
       0,     0,     0,   412,     0,     0,     0,    61,    72,   251,
     217,   218,    64,    65,    66,    67,    68,    69,    70,     0,
    1342,   249,     0,     0,     0,     0,     0,     0,   219,    74,
       0,     0,   249,   320,   320,    72,     0,     0,  1009,    77,
      78,     0,     0,     0,  1261,   758,     0,     0,     0,     0,
       0,     0,   249,     0,     0,   755,    74,     0,     0,   600,
       0,     0,     0,     0,     0,     0,    77,   756,     0,    57,
       0,   147,   147,   147,   147,   147,   147,     0,   412,   412,
      61,  1508,   302,   251,   114,    64,    65,    66,    67,    68,
      69,    70,  1227,   848,     0,     0,  1228,     0,  1229,     0,
      61,     0,     0,   217,   218,    64,    65,    66,    67,    68,
      69,    70,     0,     0,     0,   114,     0,     0,     0,     0,
     114,     0,     0,   251,   114,     0,   114,     0,    72,    74,
       0,   245,  1600,     0,     0,     0,     0,   114,     0,     0,
       0,     0,     0,    13,    14,    15,    16,    17,   219,    74,
     438,     0,   348,   114,   412,     0,   251,     0,    61,    77,
      78,   168,   169,    64,    65,    66,    67,    68,    69,    70,
    1932,     0,     0,   147,     0,     0,     0,   114,     0,     0,
     251,     0,     0,     0,   547,     0,     0,   251,     0,     0,
     114,     0,   900,     0,     0,   298,     0,     0,   114,     0,
       0,    57,     0,     0,     0,   733,   733,     0,     0,     0,
       0,   412,     0,   114,   114,   999,   412,  1344,  1002,   412,
       0,     0,   114,   114,   114,     0,     0,    13,    14,    15,
      16,    17,    61,     0,     0,   217,   218,    64,    65,    66,
      67,    68,    69,    70,     0,    61,     0,     0,   344,   345,
      64,    65,    66,    67,    68,    69,    70,     0,     0,  1633,
      72,     0,     0,  1508,     0,   408,     0,     0,     0,  1508,
       0,  1508,     0,     0,     0,     0,   412,     0,     0,   507,
    1887,    74,     0,  1070,   497,    57,     0,  1074,     0,     0,
    1459,    77,    78,   408,     0,   408,    75,     0,     0,     0,
     412,   346,     0,     0,     0,     0,     0,     0,    13,    14,
      15,    16,    17,     0,     0,     0,    61,   412,   408,   217,
     218,    64,    65,    66,    67,    68,    69,    70,   249,   320,
       0,     0,   147,     0,     0,     0,     0,     0,     0,   114,
     114,     0,     0,     0,    72,     0,     0,     0,  1512,     0,
       0,     0,     0,     0,     0,   147,     0,     0,   114,   249,
       0,     0,     0,     0,   295,    74,    57,     0,     0,     0,
       0,    98,     0,     0,   151,    77,    78,     0,     0,     0,
     320,   320,     0,    61,     0,     0,   114,     0,    64,    65,
      66,    67,    68,    69,    70,  1633,  1633,    61,     0,     0,
     217,   218,    64,    65,    66,    67,    68,    69,    70,   251,
    1508,     0,     0,  1508,     0,     0,     0,   412,     0,     0,
     251,     0,     0,     0,   114,    72,     0,     0,    98,   114,
     412,  1260,    74,     0,   302,   147,     0,     0,   114,     0,
    1180,   412,     0,   114,     0,  1506,    74,     0,     0,     0,
       0,     0,   195,     0,     0,     0,    77,    78,     0,     0,
       0,     0,     0,   296,     0,     0,     0,     0,     0,   733,
       0,     0,   258,     0,    61,     0,     0,   217,   218,    64,
      65,    66,    67,    68,    69,    70,     0,     0,     0,     0,
       0,   412,     0,     0,     0,     0,   249,     0,     0,     0,
       0,     0,    72,     0,  1633,     0,     0,     0,     0,   288,
       0,     0,     0,  1508,     0,    98,     0,     0,     0,     0,
       0,     0,   755,    74,     0,   302,   600,     0,     0,     0,
    1512,     0,   322,    77,   756,   408,  1648,     0,  1512,     0,
    1292,     0,   147,  1296,     0,   249,   602,     0,    61,     0,
     418,     0,   114,    64,    65,    66,    67,    68,    69,    70,
    1227,   288,   444,     0,  1228,     0,  1229,   320,     0,   114,
     114,     0,     0,     0,     0,  1633,     0,     0,     0,     0,
       0,     0,   147,     0,     0,     0,     0,     0,     0,     0,
     491,     0,     0,     0,     0,     0,     0,    74,   147,   147,
    1602,  1888,   302,     0,     0,     0,   511,     0,     0,     0,
       0,   516,   518,     0,     0,   195,     0,     0,     0,     0,
      61,   108,   114,   168,   169,    64,    65,    66,    67,    68,
      69,    70,     0,     0,   147,     0,     0,   538,     0,     0,
     540,     0,   541,     0,     0,     0,     0,     0,     0,    61,
    1888,  1888,     0,   558,    64,    65,    66,    67,    68,    69,
      70,  1227,   114,     0,     0,  1228,   570,  1229,     0,     0,
     412,     0,     0,     0,     0,     0,   576,  1749,   108,     0,
    1512,     0,     0,     0,  1888,     0,     0,     0,     0,     0,
     591,     0,     0,   615,     0,     0,     0,     0,    74,   412,
       0,     0,     0,     0,     0,     0,     0,   622,     0,     0,
       0,   622,     0,     0,     0,     0,   251,   114,     0,     0,
       0,     0,   259,     0,     0,  1445,     0,     0,     0,     0,
     298,     0,     0,     0,  1454,     0,   114,     0,     0,     0,
       0,     0,     0,   412,     0,     0,     0,  1180,     0,     0,
       0,     0,     0,     0,    57,     0,     0,     0,     0,  1405,
       0,     0,     0,     0,     0,   108,     0,     0,     0,     0,
     412,     0,     0,     0,   114,     0,     0,     0,     0,     0,
    1512,     0,   326,     0,     0,    61,     0,     0,   217,   218,
      64,    65,    66,    67,    68,    69,    70,     0,     0,     0,
     118,     0,     0,   118,   288,     0,     0,     0,   591,     0,
       0,     0,   445,    72,     0,    61,   114,   114,   217,   218,
      64,    65,    66,    67,    68,    69,    70,     0,     0,     0,
     114,   114,     0,  1887,    74,   114,   114,   497,     0,     0,
       0,     0,     0,     0,    77,    78,     0,     0,   249,     0,
       0,     0,     0,     0,     0,     0,     0,   118,     0,     0,
       0,     0,     0,   114,   114,     0,     0,     0,   298,     0,
       0,  1191,     0,   114,   114,   114,   114,   114,   114,     0,
       0,   118,     0,     0,   251,     0,     0,   539,    61,   444,
       0,   217,   218,    64,    65,    66,    67,    68,    69,    70,
     364,   118,   365,   108,   366,    62,    63,    64,    65,    66,
      67,    68,    69,    70,     0,     0,    72,   569,   298,     0,
     844,     0,     0,     0,     0,   518,     0,     0,     0,   855,
       0,   558,     0,   251,     0,     0,   219,    74,   118,   249,
     592,     0,   322,   259,   118,     0,   118,    77,    78,     0,
     673,   298,   412,    75,   375,     0,     0,   592,   622,   875,
       0,   592,     0,    57,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   886,  1636,   114,     0,     0,   118,     0,
       0,     0,   591,     0,     0,     0,     0,   895,     0,     0,
     118,     0,     0,     0,    61,   622,    57,   217,   218,    64,
      65,    66,    67,    68,    69,    70,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   249,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,    61,     0,     0,
     217,   218,    64,    65,    66,    67,    68,    69,    70,     0,
       0,   118,   295,    74,   118,     0,     0,   114,   114,   118,
       0,     0,     0,    77,    78,    72,     0,     0,   592,     0,
       0,     0,     0,     0,     0,     0,     0,   412,     0,     0,
       0,     0,     0,     0,     0,  1506,    74,     0,     0,     0,
       0,   444,   118,   114,     0,     0,    77,    78,     0,     0,
       0,     0,     0,     0,     0,   412,     0,   412,   991,     0,
       0,   118,     0,     0,     0,     0,     0,     0,     0,     0,
    1636,  1636,     0,     0,     0,     0,     0,     0,     0,     0,
     412,     0,   875,     0,     0,     0,     0,  1015,     0,     0,
       0,   114,     0,     0,   114,     0,     0,     0,     0,   445,
       0,     0,   114,     0,   444,   444,   712,   713,   714,   715,
     716,   717,   718,   719,   720,   721,   722,   114,     0,     0,
     204,     0,     0,   444,     0,     0,     0,     0,     0,     0,
     326,     0,   114,   118,     0,     0,     0,   114,   114,   259,
       0,   108,   114,   114,     0,     0,     0,   723,     0,     0,
      61,   844,   445,   189,    63,    64,    65,    66,    67,    68,
      69,    70,     0,     0,     0,     0,     0,   118,   592,   445,
       0,     0,     0,     0,     0,     0,     0,     0,   113,  1636,
       0,     0,  1151,     0,     0,     0,     0,     0,     0,   444,
       0,     0,   592,   118,   151,     0,   251,   114,     0,    74,
       0,     0,   777,   622,     0,   592,  1182,     0,   844,     0,
       0,    61,     0,  1188,   217,   218,    64,    65,    66,    67,
      68,    69,    70,    61,     0,     0,   542,    63,    64,    65,
      66,    67,    68,    69,    70,   113,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,  1864,     0,     0,     0,
    1636,     0,     0,     0,     0,     0,   322,     0,     0,  1506,
      74,     0,     0,     0,     0,     0,  1507,     0,     0,     0,
      77,    78,   118,   118,     0,   982,  1636,     0,     0,   261,
       0,     0,     0,     0,     0,     0,     0,   251,     0,     0,
       0,   445,     0,     0,     0,     0,     0,   412,     0,     0,
       0,     0,    61,     0,   114,   544,   545,    64,    65,    66,
      67,    68,    69,    70,   118,     0,     0,   844,   118,     0,
     118,     0,   113,     0,     0,  1636,  1636,     0,     0,   114,
       0,     0,   445,     0,   844,   844,     0,     0,     0,   330,
       0,     0,     0,     0,   114,     0,     0,     0,     0,     0,
       0,     0,     0,    75,   326,   326,     0,     0,     0,  1636,
     114,   114,     0,     0,   251,     0,     0,     0,     0,   447,
       0,     0,     0,   326,    61,     0,     0,   217,   218,    64,
      65,    66,    67,    68,    69,    70,     0,   444,     0,     0,
       0,     0,   118,     0,     0,     0,   114,     0,     0,     0,
       0,   326,    72,     0,     0,   118,     0,   118,   118,     0,
     118,     0,     0,   118,     0,     0,   118,   118,   118,     0,
       0,     0,  1887,    74,     0,     0,   497,  1319,     0,     0,
     114,     0,   108,    77,    78,  1151,     0,    61,     0,   326,
      62,    63,    64,    65,    66,    67,    68,    69,    70,     0,
       0,     0,     0,   592,     0,     0,   259,     0,   326,     0,
     113,     0,    61,     0,  1151,   217,   218,    64,    65,    66,
      67,    68,    69,    70,     0,     0,     0,     0,     0,     0,
       0,     0,  1368,     0,     0,     0,     0,     0,    75,     0,
      72,     0,     0,     0,   118,     0,     0,   594,     0,     0,
     261,     0,     0,     0,     0,     0,   445,     0,   591,     0,
     295,    74,     0,     0,   594,     0,     0,   516,   594,     0,
       0,    77,    78,     0,     0,     0,     0,     0,     0,   171,
     174,     0,     0,     0,     0,   322,     0,     0,     0,     0,
      61,     0,   666,   168,   169,    64,    65,    66,    67,    68,
      69,    70,    61,     0,     0,   168,   169,    64,    65,    66,
      67,    68,    69,    70,   212,    61,     0,   326,   168,   169,
      64,    65,    66,    67,    68,    69,    70,     0,     0,     0,
       0,   844,   844,     0,   326,   326,     0,     0,   455,     0,
       0,     0,     0,   359,     0,   844,   844,   360,     0,   361,
     444,   444,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   118,     0,   459,   290,   594,   362,   291,     0,     0,
       0,     0,     0,   118,   118,     0,     0,     0,   844,   844,
     311,     0,     0,     0,     0,     0,     0,   326,  1319,  1319,
    1319,   151,     0,   363,   364,     0,   365,     0,   366,  1826,
      63,    64,    65,    66,    67,    68,    69,    70,   367,   368,
     356,     0,   369,   370,   371,     0,   372,   373,     0,     0,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,   824,   826,     0,   476,   108,     0,     0,     0,     0,
       0,     0,     0,     0,   374,     0,   447,    75,   375,     0,
       0,     0,     0,     0,   376,    77,    78,   377,   378,   379,
     380,     0,     0,     0,   108,     0,     0,   322,  1827,  -175,
       0,     0,     0,     0,     0,     0,     0,   330,     0,   527,
       0,     0,   259,     0,     0,     0,   261,     0,   113,   171,
     151,     0,     0,     0,     0,     0,     0,     0,     0,   447,
     171,     0,     0,     0,     0,     0,     0,     0,   592,     0,
       0,     0,     0,     0,     0,   594,   447,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   573,     0,     0,
       0,     0,     0,   575,   577,   445,     0,     0,   584,   594,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   594,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   844,   844,   666,     0,     0,     0,     0,   666,
     629,     0,   666,     0,     0,   311,     0,     0,   311,     0,
       0,   326,   326,     0,     0,     0,     0,     0,  1650,     0,
       0,   666,     0,     0,     0,   326,   326,     0,   844,     0,
     326,   326,     0,     0,     0,     0,   118,     0,     0,     0,
    1671,     0,  1671,     0,   118,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   979,     0,     0,   326,   326,
       0,     0,     0,     0,     0,  1671,     0,     0,   447,     0,
       0,     0,     0,   118,     0,     0,   322,     0,     0,   151,
       0,     0,     0,     0,     0,     0,     0,   844,     0,     0,
       0,   118,     0,     0,     0,   212,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   772,   773,   447,
     118,     0,     0,     0,     0,     0,     0,   844,     0,     0,
       0,     0,   844,   844,     0,     0,     0,   444,   444,     0,
     119,   330,   330,   119,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1741,     0,     0,     0,   445,   118,     0,
     330,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   330,     0,
       0,     0,  1762,     0,     0,     0,     0,   119,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   113,
       0,   119,     0,     0,     0,     0,   330,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     594,   119,     0,   261,     0,   330,     0,   311,     0,     0,
       0,     0,   326,   326,     0,     0,     0,   118,   118,   118,
     118,   118,   118,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   119,     0,
       0,     0,     0,     0,   119,     0,   119,     0,   326,     0,
       0,     0,  1671,   447,     0,   629,     0,     0,     0,  1839,
     108,     0,   108,     0,     0,     0,     0,     0,     0,   123,
       0,     0,   123,     0,     0,     0,     0,     0,   119,     0,
       0,     0,     0,     0,   444,   108,     0,     0,     0,     0,
     119,     0,     0,     0,     0,     0,   326,     0,     0,  1671,
       0,     0,     0,     0,     0,     0,     0,   326,     0,     0,
       0,     0,     0,     0,   330,  1671,  1839,     0,     0,   118,
       0,     0,     0,     0,     0,     0,   123,     0,     0,     0,
       0,   330,   330,     0,     0,     0,     0,   326,     0,     0,
       0,   119,   326,   326,   119,     0,     0,   326,   326,   119,
     123,  1671,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1921,     0,
     123,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   119,     0,   330,   844,     0,     0,     0,     0,
       0,     0,     0,  1032,     0,     0,     0,     0,     0,     0,
    1044,   119,   259,     0,     0,     0,     0,   123,     0,     0,
       0,   118,     0,   123,     0,   123,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   113,     0,     0,     0,     0,     0,     0,   118,
       0,   118,     0,     0,     0,     0,     0,   123,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   123,
       0,   113,     0,     0,   118,     0,     0,     0,     0,     0,
       0,     0,     0,   119,     0,  1102,     0,     0,   118,   261,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   386,
       0,   118,   108,     0,     0,   594,     0,   119,     0,   592,
     123,     0,     0,   123,     0,     0,     0,     0,   123,     0,
       0,     0,     0,     0,     0,  1195,     0,     0,     0,     0,
     629,     0,   447,   119,   326,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   108,
       0,   123,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   108,   592,     0,     0,     0,
     123,     0,     0,     0,     0,     0,     0,     0,   330,   330,
       0,   118,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   330,   330,     0,     0,     0,   330,   330,     0,
       0,   108,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   119,   119,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   330,   330,     0,     0,     0,
       0,     0,     0,     0,     0,   326,     0,     0,     0,     0,
       0,     0,   123,     0,     0,     0,  1522,     0,     0,     0,
       0,     0,     0,  1527,   119,     0,     0,     0,   119,     0,
     119,     0,  1535,     0,     0,     0,  1539,     0,  1541,     0,
       0,     0,     0,     0,     0,     0,   123,     0,     0,     0,
     647,   118,     0,   386,   652,     0,     0,     0,     0,     1,
       0,     0,   145,   655,   656,     0,     0,     0,     0,     0,
       0,     0,   123,     0,     0,     0,     0,     0,   386,   386,
       0,     0,     0,     0,   447,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   118,   386,
       0,     0,   119,     0,  1341,  1343,  1345,     0,     0,     0,
       0,     0,     0,     0,   118,   119,     0,   119,   119,     0,
     119,     0,     0,   119,     0,     0,   119,   119,   119,   386,
       0,     0,     0,     0,  1364,   192,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1102,
     118,   123,   123,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1627,     0,     0,   330,
     330,     0,     0,     0,     0,   629,     0,     0,     0,     0,
       0,     0,     0,   123,   283,     0,     0,   123,     0,   123,
       0,     0,     0,     0,   119,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   330,     0,     0,  1665,     0,
    1676,     0,     0,     0,     0,     0,     0,   113,     0,   113,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1665,     0,     0,     0,     0,     0,     0,
       0,     0,   113,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   330,     0,     0,     0,     0,     0,     0,
       0,   123,     0,     0,   330,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   123,     0,   123,   123,   283,   123,
       0,     0,   123,     0,     0,   123,   123,   123,     0,     0,
       0,     0,     0,   519,   330,     0,     0,     0,     0,   330,
     330,     0,     0,   283,   330,   330,  1517,     0,     0,  1519,
       0,   119,     0,   283,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   119,   119,     0,     0,   550,   554,     0,
       0,     0,     0,     0,   561,   562,     0,     0,     0,     0,
       0,  1759,     0,     0,     0,  1761,     0,     0,     0,     0,
     572,     0,  1763,     0,     0,     0,     0,     0,     0,   261,
       0,     0,     0,   123,     0,     0,     0,     0,   589,   386,
     386,   386,   386,   386,   386,   386,   386,   386,   386,   386,
     386,   386,   386,   386,   386,   386,   386,   386,     0,     0,
       0,   203,     0,     0,     0,     0,     0,   214,   215,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   672,     0,     0,     0,     0,     0,
       0,   277,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1828,  1830,   386,
    1676,     0,     0,     0,     0,   711,     0,     0,     0,   113,
       0,     0,     0,     0,     0,     0,   594,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     123,   749,     0,     0,     0,   752,     0,     0,     0,     0,
       0,   330,   123,   123,     0,     0,     0,     0,     0,  1656,
       0,     0,     0,     0,   774,  1876,   113,     0,   775,   776,
       0,     0,   779,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   113,   594,     0,     0,     0,   793,   794,   795,
     796,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   818,     0,     0,     0,
       0,     0,     0,     0,   821,     0,   119,  1914,   113,  1916,
       0,     0,     0,     0,   119,     0,     0,     0,     0,   629,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1929,
       0,     0,   283,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   330,   119,     0,     0,     0,   566,     0,     0,
       0,     0,     0,   386,     0,     0,     0,     0,   386,     0,
       0,   119,     0,   859,     0,  1957,  1959,  1960,     0,   386,
     550,     0,     0,     0,     0,   864,     0,     0,     0,     0,
     119,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   878,   883,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   386,
       0,     0,     0,     0,     0,     0,     0,     0,   119,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1790,     0,     0,     0,     0,     0,     0,
       0,     0,   629,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   926,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   123,     0,     0,     0,     0,
       0,     0,     0,   123,     0,     0,     0,     0,     0,     0,
       0,     0,   753,     0,   754,     0,     0,     0,     0,     0,
       0,     0,     0,   770,   771,     0,     0,     0,     0,     0,
       0,     0,   123,     0,     0,     0,     0,   119,   119,   119,
     119,   119,   119,     0,     0,     0,   987,     0,     0,     0,
     123,     0,     0,     0,     0,     0,   386,     0,     0,     0,
       0,  1004,     0,     0,     0,  1005,     0,     0,     0,   123,
       0,     0,     0,     0,   878,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1045,     0,     0,     0,
       0,     0,     0,     0,     0,  1054,     0,   123,     0,     0,
       0,  1057,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   386,
       0,   854,     0,     0,     0,     0,     0,     0,     0,   119,
       0,     0,     0,     0,     0,     0,     0,     0,     1,     0,
       0,     0,     0,     0,     0,     1,     0,     0,     0,     0,
       0,     0,     0,   386,   386,   386,     0,     0,     0,     0,
     386,   386,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     1,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   386,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   123,   123,   123,   123,
     123,   123,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   386,
     386,   119,     0,     0,     0,  1205,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   119,
       0,   119,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   119,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   119,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   123,     0,
       0,     0,     0,     0,     0,     0,  1251,     0,     0,     0,
    1252,   119,     0,     0,     0,     0,     0,   878,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1265,     0,     0,
       0,     0,     0,  1030,  1266,     0,     0,     0,     0,     0,
       0,     0,     0,  1270,     0,  1271,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1298,     0,     0,
       0,  1299,     0,     0,     0,     0,     0,     0,     0,     0,
     123,   119,     0,     0,     0,   145,     0,     0,     1,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1098,  1099,     0,     0,     0,     0,     0,     0,   123,     0,
     123,  1157,  1158,  1159,     0,     0,  1161,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   123,     0,     0,     0,     0,     0,   165,
       0,     0,     0,     0,     0,     0,     0,   123,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   165,     0,     0,     0,     0,
     123,     0,     0,     0,     0,  1387,     0,     0,     0,     0,
       0,   119,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   386,     0,     0,     0,  1225,     0,     0,     0,  1410,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   165,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   165,     0,   165,     0,   119,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1244,
       0,     0,     0,     0,   119,     0,     0,     0,     0,     0,
     123,     0,     0,     0,     0,     0,   350,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   350,     0,     0,     0,     0,     0,     0,
     119,     0,     0,     0,     0,     0,  1268,     0,     0,  1482,
       0,     0,     0,  1483,     0,  1272,  1273,  1274,  1275,     0,
       0,     0,     0,  1280,  1281,     0,     0,     0,     0,     0,
       0,   165,     0,  1288,     0,   165,     0,     0,   165,   165,
       0,     0,   165,  1518,     0,   165,   165,     0,     0,     0,
       0,     0,     0,     0,  1302,     0,  1303,  1530,  1532,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     123,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1566,     0,     0,  1569,     0,     0,
       0,     0,     0,   386,     0,     0,     0,   165,     0,  1360,
     165,     0,  1577,     0,     0,     0,     0,   123,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     165,     0,     0,   123,     0,  1374,     0,   386,     0,     0,
       0,  1378,     0,  1380,  1382,   165,     0,     0,     0,     0,
       0,     0,  1388,     0,  1389,     0,  1390,     0,  1392,     0,
       0,     0,  1607,  1400,     0,     0,     0,   247,     0,   123,
       0,  1612,     0,     0,     0,  1613,     0,     0,   268,     0,
     271,     0,   273,     0,     0,     0,     0,     0,     0,  1617,
    1618,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   386,   386,
     247,  1442,   271,   273,     0,     0,     0,     0,  1449,  1450,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   165,
       0,     0,     0,     0,     0,   386,     0,     0,     0,     0,
       0,     0,  1473,     0,     0,     0,   163,     0,     0,  1478,
       0,     0,  1479,     0,   247,     0,     0,     0,     0,     0,
     386,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1699,
    1700,     0,   214,     0,   350,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   165,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   386,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   247,   275,   271,
     273,     0,     0,  1556,     0,     0,     0,     0,     0,     0,
       0,   281,     0,   282,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   247,     0,     0,
       0,     0,     0,   247,     0,     0,  1575,     0,     0,     0,
       0,     0,     0,     0,  1580,     0,  1582,     0,     0,     0,
     350,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   247,     0,     0,     0,     0,     0,   618,     0,   273,
       0,     0,     0,     0,     0,     0,     0,  1779,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   165,
     165,     0,  1608,  1609,     0,     0,     0,     0,     0,  1569,
       0,     0,   165,     0,     0,   501,   502,  1614,  1615,   506,
    1616,     0,   509,   510,     0,     0,     0,  1804,     0,  1620,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1625,
    1626,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1819,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   247,     0,     0,
       0,     0,     0,     0,  1835,     0,     0,  1836,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   247,     0,   618,   273,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   587,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   619,   165,   165,     0,     0,     0,     0,   165,
       0,     0,     0,  1703,  1704,     0,     0,     0,     0,     0,
     247,     0,     0,  1710,     0,     0,     0,     0,     0,     0,
     165,     0,     0,   165,   165,     0,   165,     0,   165,   165,
     247,  1905,     0,     0,     0,   247,     0,   247,     0,     0,
       0,     0,     0,   337,     0,     0,     0,     0,  1733,  1734,
       0,     0,     0,     0,     0,     0,     0,   247,     0,   247,
     247,     0,     0,     0,     0,     0,     0,   165,     0,     0,
       0,   165,   434,   337,     0,     0,     0,   247,     0,     0,
       0,     0,     0,     0,     0,     0,   743,     0,     0,   247,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   500,     0,     0,     0,     0,     0,
     247,   500,   618,   273,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1792,     0,     0,
       0,     0,     0,   193,   247,   618,     0,     0,     0,     0,
       0,   247,     0,     0,     0,  1801,   165,     0,  1802,  1803,
       0,     0,     0,   814,     0,  1805,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   500,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   193,     0,
       0,   337,   604,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   193,     0,     0,     0,     0,     0,     0,
       0,     0,   625,     0,     0,     0,     0,     0,     0,     0,
     193,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   441,     0,     0,     0,     0,     0,  1874,
       0,     0,     0,     0,     0,     0,  1882,     0,     0,     0,
       0,     0,  1886,     0,     0,     0,   889,   890,     0,     0,
       0,     0,     0,     0,     0,     0,   165,     0,     0,   897,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   500,     0,     0,     0,   193,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   500,   745,
    1918,   500,   748,     0,     0,     0,     0,     0,     0,   337,
       0,     0,     0,   604,   165,     0,     0,   165,     0,     0,
       0,     0,     0,     0,  1935,     0,     0,     0,     0,     0,
       0,     0,  1940,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   193,     0,     0,     0,  1953,     0,     0,
       0,     0,     0,   247,   500,     0,     0,     0,   500,     0,
       0,   193,     0,     0,   247,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     993,   994,     0,     0,   247,     0,   998,     0,     0,     0,
     337,     0,     0,     0,     0,   247,     0,     0,     0,     0,
       0,     0,     0,     0,   247,     0,     0,  1019,     0,     0,
    1022,  1023,     0,  1026,     0,  1028,  1029,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   165,     0,     0,     0,
     500,     0,     0,   337,   165,   165,     0,     0,     0,   193,
       0,     0,     0,     0,  1069,     0,     0,     0,  1073,   873,
     337,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     604,     0,     0,     0,   604,     0,     0,     0,     0,   193,
       0,   891,     0,   337,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   165,
     247,     0,     0,     0,     0,     0,     0,     0,   165,     0,
       0,   165,     0,   165,   165,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   247,     0,     0,     0,     0,     0,
       0,     0,     0,  1189,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   165,   193,   193,     0,     0,     0,     0,
     441,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   337,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   500,   500,
       0,     0,     0,     0,     0,     0,     0,     0,   500,  1000,
       0,   500,  1003,   193,     0,     0,     0,     0,     0,     0,
       0,   165,     0,   337,     0,     0,   604,   200,   604,   604,
     441,     0,     0,     0,     0,   604,     0,     0,     0,     0,
       0,     0,     0,   255,     0,   337,   337,     0,     0,     0,
       0,     0,     0,   193,     0,     0,     0,     0,     0,     0,
     247,     0,     0,  1189,   337,     0,     0,     0,   500,     0,
       0,     0,   500,     0,   193,     0,   500,  1071,     0,     0,
     500,  1075,     0,     0,     0,     0,     0,     0,  1078,     0,
       0,   247,   200,     0,     0,     0,   303,   247,     0,     0,
       0,     0,     0,   165,     0,     0,     0,   342,     0,     0,
       0,  1291,     0,     0,  1295,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   200,     0,     0,     0,     0,     0,
     337,   500,   165,     0,     0,     0,     0,   454,     0,     0,
     458,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   441,     0,     0,     0,     0,   604,   165,     0,
       0,     0,     0,     0,   165,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   193,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     200,     0,     0,   441,     0,     0,     0,   337,     0,     0,
       0,     0,     0,   255,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   441,   441,   165,     0,     0,
       0,     0,     0,     0,   410,     0,     0,     0,   247,     0,
       0,     0,     0,  1394,   441,     0,     0,   439,     0,   458,
       0,  1403,  1404,     0,     0,     0,     0,   200,     0,     0,
     467,     0,   467,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   500,     0,     0,   597,     0,   614,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   247,     0,   604,
     604,     0,     0,     0,     0,     0,   604,     0,     0,     0,
       0,   165,   165,     0,     0,     0,  1444,     0,     0,   350,
     441,     0,     0,   165,     0,  1453,   193,     0,  1457,     0,
    1460,  1461,     0,     0,     0,     0,     0,     0,     0,   670,
       0,     0,     0,     0,     0,     0,     0,     0,   337,     0,
       0,     0,     0,   500,  1293,     0,   500,  1297,   567,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1487,     0,     0,   200,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   193,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   597,     0,     0,     0,     0,     0,   769,
       0,     0,     0,     0,     0,   165,     0,     0,     0,     0,
       0,     0,     0,     0,   242,   243,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,  1563,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,  -405,  -405,   337,
    -405,    45,    46,     0,  -405,   604,  1396,     0,   200,   200,
     165,     0,     0,     0,   454,     0,     0,     0,     0,     0,
       0,    57,     0,     0,     0,     0,   337,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   441,     0,
    1457,     0,     0,     0,     0,    62,    63,     0,     0,   467,
       0,     0,     0,     0,     0,   467,     0,   342,   500,  1446,
     790,     0,     0,     0,     0,     0,     0,   500,  1455,  1622,
     604,     0,     0,     0,   454,     0,   877,     0,     0,   165,
       0,   337,   337,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    75,     0,     0,     0,   597,     0,   247,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   200,     0,
     247,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   670,     0,   670,   670,     0,   670,     0,     0,   670,
       0,     0,   670,   670,   670,     0,     0,   858,     0,     0,
       0,     0,   193,     0,  1697,     0,     0,     0,     0,   193,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   439,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   193,   885,     0,     0,
     247,     0,     0,     0,     0,     0,   454,     0,   337,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   247,     0,     0,     0,     0,     0,     0,     0,     0,
     200,     0,     0,     0,     0,     0,     0,     0,  1745,  1746,
       0,     0,     0,     0,     0,     0,     0,   454,   920,     0,
    1750,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   441,   441,     0,     0,     0,   790,   940,     0,   454,
     454,     0,     0,     0,     0,     0,   951,     0,   956,   951,
       0,     0,     0,     0,     0,     0,     0,     0,   454,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   247,     0,
       0,     0,     0,     0,     0,     0,   984,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   500,     0,   986,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     995,     0,     0,   500,     0,     0,     0,     0,     0,     0,
       0,     0,  1812,     0,   439,     0,     0,   984,     0,     0,
       0,     0,     0,     0,   454,     0,     0,     0,     0,     0,
     200,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     769,     0,     0,     0,  1048,     0,     0,   467,   193,     0,
     247,     0,     0,     0,     0,     0,     0,   337,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1863,     0,     0,
       0,     0,     0,  1079,     0,     0,     0,     0,     0,     0,
       0,   342,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   337,   337,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   500,   500,     0,     0,     0,     0,     0,
     410,     0,     0,     0,     0,     0,     0,     0,     0,   500,
    1179,  1181,     0,     0,     0,     0,     0,     0,   439,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   193,     0,     0,     0,     0,     0,   467,
       0,     0,     0,     0,     0,     0,     0,     0,   951,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   984,     0,     0,     0,     0,     0,     0,     0,  1218,
       0,     0,     0,     0,     0,     0,   951,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   454,     0,     0,     0,     0,   193,     0,     0,
       0,     0,   500,     0,     0,     0,     0,     0,     0,     0,
     500,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     467,     0,     0,     0,     0,     0,     0,     0,     0,  1187,
       0,     0,   670,     0,     0,     0,    13,    14,    15,    16,
      17,     0,     0,     0,     0,     0,     0,     0,   441,   441,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   337,     0,     0,     0,   500,
    1865,     0,   359,   500,     0,     0,   360,     0,   361,     0,
       0,     0,     0,     0,     0,     0,   255,   467,     0,  1284,
       0,  1286,     0,     0,    57,   362,     0,     0,     0,   500,
       0,     0,     0,     0,     0,     0,   200,     0,     0,     0,
       0,     0,     0,   597,     0,     0,     0,     0,     0,     0,
       0,     0,   363,   364,     0,   365,     0,   366,    62,    63,
      64,    65,    66,    67,    68,    69,    70,   367,   368,   356,
     342,   369,   370,   371,   670,   372,   373,     0,   500,   500,
       0,     0,     0,    72,     0,     0,     0,  1352,  1352,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   374,     0,     0,    75,   375,     0,     0,
       0,     0,   500,   376,   437,    78,   377,   378,   379,   380,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   454,   454,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1391,     0,     0,
       0,     0,     0,  1401,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   441,     0,     0,     0,     0,
     439,     0,  1398,   670,   670,   670,     0,   670,   670,    13,
      14,    15,    16,    17,   458,     0,     0,   467,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     951,     0,     0,   790,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   359,     0,     0,     0,   360,
       0,   361,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   255,     0,     0,     0,    57,   362,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   342,     0,  1481,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   363,   364,     0,   365,     0,
     366,    62,    63,    64,    65,    66,    67,    68,    69,    70,
     367,   368,   356,     0,   369,   370,   371,     0,   372,   373,
       0,     0,   951,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   467,     0,     0,   790,     0,   374,     0,     0,    75,
     375,     0,     0,     0,     0,     0,   376,  1399,    78,   377,
     378,   379,   380,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   940,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1578,  1579,     0,     0,     0,   200,     0,     0,
       0,     0,   242,   243,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,     0,   467,    19,   790,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,  -405,  -405,     0,  -405,    45,
      46,     0,  -405,     0,     0,     0,     0,     0,     0,     0,
       0,   342,     0,     0,     0,     0,     0,     0,     0,    57,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   670,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   410,     0,     0,
      61,     0,  1649,    62,    63,    64,    65,    66,    67,    68,
      69,    70,   454,   454,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1689,     0,    73,    74,
       0,    75,   244,     0,     0,     0,  -712,     0,     0,    77,
      78,     0,     0,     0,     0,     0,   255,     0,     0,     0,
       0,   242,   243,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,  1711,     0,    19,  1713,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,  -405,  -405,     0,  -405,    45,    46,
       0,  -405,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    57,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   255,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    62,    63,   670,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    72,     0,   454,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      75,   244,     0,     0,     0,     0,     0,     0,    77,    78,
       0,   670,     0,     0,   458,     0,     4,   243,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
    1103,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,     0,
       0,   359,   951,    45,    46,   360,     0,   361,    47,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
      56,     0,  1104,    57,  1105,    -2,     0,  1106,     0,     0,
    1107,  1108,  1109,  1110,  1111,  1112,  1113,  1114,  1115,  1116,
    1117,  1118,  -284,  1119,  1120,  1121,  1122,  1123,     0,  1124,
       0,   363,   364,    60,   461,     0,   366,  1125,  1126,    64,
      65,    66,    67,    68,    69,    70,   367,   368,   356,  1127,
     369,   370,   371,     0,   372,   373,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    -3,   374,     0,     0,    75,   406,     0,     0,     0,
     279,     0,   376,    77,    78,   377,   378,   379,   380,     0,
       0,     0,     0,     0,     0,     0,     0,  -175,     4,   243,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,  1103,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,     0,     0,   359,     0,    45,    46,   360,     0,   361,
      47,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,    56,     0,  1104,    57,  1105,    -2,     0,  1106,
       0,     0,  1107,  1108,  1109,  1110,  1111,  1112,  1113,  1114,
    1115,  1116,  1117,  1118,  -284,  1119,  1120,  1121,  1122,  1123,
       0,  1124,     0,   363,   364,    60,   461,     0,   366,  1125,
    1126,    64,    65,    66,    67,    68,    69,    70,   367,   368,
     356,  1127,   369,   370,   371,     0,   372,   373,     0,     0,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   374,     0,     0,    75,   406,     0,
       0,     0,   279,     0,   376,    77,    78,   377,   378,   379,
     380,     0,     0,     0,     0,     0,     0,     0,     0,  -175,
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
    1674,  1675,     4,   243,     6,     7,     8,     9,    10,    11,
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
       0,    75,   406,     0,     0,     0,     0,     0,   376,    77,
      78,   377,   378,   379,   380,   243,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,     0,     0,   359,
       0,    45,    46,   360,     0,   361,   318,    48,    49,    50,
      51,    52,    53,    54,     0,     0,     0,     0,     0,     0,
       0,    57,   362,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   363,
     364,     0,   365,     0,   366,    62,    63,    64,    65,    66,
      67,    68,    69,    70,   367,   368,   356,     0,   369,   370,
     371,     0,   372,   373,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     374,     0,     0,    75,   436,     0,     0,     0,     0,     0,
     376,   437,    78,   377,   378,   379,   380,   243,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,     0,
       0,   359,     0,    45,    46,   360,     0,   361,   318,    48,
      49,    50,    51,    52,    53,    54,     0,     0,     0,     0,
       0,     0,     0,    57,   362,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   363,   364,     0,   365,     0,   366,    62,    63,    64,
      65,    66,    67,    68,    69,    70,   367,   368,   356,     0,
     369,   370,   371,     0,   372,   373,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   374,     0,     0,    75,  1176,     0,     0,     0,
       0,     0,   376,  1177,    78,   377,   378,   379,   380,   243,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,     0,     0,   359,     0,    45,    46,   360,     0,   361,
     318,    48,    49,    50,    51,    52,    53,    54,     0,     0,
       0,     0,     0,     0,     0,    57,   362,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   363,   364,     0,   365,     0,   366,    62,
      63,    64,    65,    66,    67,    68,    69,    70,   367,   368,
     356,     0,   369,   370,   371,     0,   372,   373,     0,     0,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   374,     0,     0,    75,   375,     0,
       0,     0,     0,     0,   376,    77,    78,   377,   378,   379,
     380,   243,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,     0,     0,   359,     0,    45,    46,   360,
       0,   361,   318,    48,    49,    50,    51,    52,    53,    54,
       0,     0,     0,     0,     0,     0,     0,    57,   362,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   363,   364,     0,   365,     0,
     366,    62,    63,    64,    65,    66,    67,    68,    69,    70,
     367,   368,   356,     0,   369,   370,   371,     0,   372,   373,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   374,     0,     0,    75,
     436,     0,     0,     0,     0,     0,   376,    77,    78,   377,
     378,   379,   380,     4,     5,     6,     7,     8,     9,    10,
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
       0,     0,     0,     0,  -337,  -337,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    60,
       0,     0,     0,    62,    63,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -337,     0,     0,
       0,    75,    76,     0,     0,     0,     0,     0,     0,    77,
      78,     4,   243,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,     0,     0,    45,    46,
       0,     0,     0,    47,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,    56,     0,     0,    57,     0,
       0,     0,     0,  -338,  -338,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    60,     0,
       0,     0,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -338,     0,     0,     0,
      75,    76,     0,     0,     0,     0,     0,     0,    77,    78,
     242,   243,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,     0,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,  -405,  -405,     0,  -405,    45,    46,     0,
    -405,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    13,    14,    15,    16,    17,     0,    57,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,     0,     0,     0,    61,    45,
      46,    62,    63,    64,    65,    66,    67,    68,    69,    70,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    57,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    73,    74,     0,    75,
     244,     0,     0,  1311,     0,     0,     0,    77,    78,  1312,
       0,     0,    13,    14,    15,    16,    17,    18,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,     0,     0,
      45,    46,     0,     0,     0,    47,    48,    49,    50,    51,
      52,    53,    54,     0,     0,     0,     0,     0,     0,     0,
      57,  1313,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      60,     0,     0,     0,    62,    63,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1314,     0,
       0,     0,    75,   916,     0,     0,  1311,     0,     0,     0,
      77,    78,  1312,     0,     0,    13,    14,    15,    16,    17,
      18,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,     0,
       0,     0,     0,    45,    46,     0,     0,     0,    47,    48,
      49,    50,    51,    52,    53,    54,     0,     0,     0,     0,
       0,     0,     0,    57,  1313,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    60,     0,     0,     0,    62,    63,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1493,     0,     0,     0,    75,   916,     0,     0,  1311,
       0,     0,     0,    77,    78,  1312,     0,     0,    13,    14,
      15,    16,    17,    18,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,     0,     0,    45,    46,     0,     0,
       0,    47,    48,    49,    50,    51,    52,    53,    54,     0,
       0,     0,     0,     0,     0,     0,    57,  1313,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    60,     0,     0,     0,
      62,    63,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1494,     0,     0,     0,    75,   916,
       0,     0,  1311,     0,     0,     0,    77,    78,  1312,     0,
       0,    13,    14,    15,    16,    17,    18,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,     0,     0,     0,     0,    45,
      46,     0,     0,     0,    47,    48,    49,    50,    51,    52,
      53,    54,     0,     0,     0,     0,     0,     0,     0,    57,
    1313,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    60,
       0,     0,     0,    62,    63,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   242,     0,  1495,     0,     0,
       0,    75,   916,     0,    13,    14,    15,    16,    17,    77,
      78,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,  -405,  -405,
       0,  -405,    45,    46,     0,  -405,     0,     0,     0,     0,
       0,     0,     0,     0,    13,    14,    15,    16,    17,     0,
       0,    19,    57,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,  -404,  -404,
       0,  -404,    45,    46,     0,  -404,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    57,     0,     0,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    75,   301,     0,     0,     0,     0,
       0,     0,    77,    78,   243,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,     0,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,  -405,  -405,     0,  -405,
      45,    46,     0,  -405,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      57,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    61,     0,     0,    62,    63,    64,    65,    66,    67,
      68,    69,    70,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    73,
      74,     0,    75,   244,     0,     0,     0,  -716,     0,     0,
      77,    78,   243,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,     0,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,  -405,  -405,     0,  -405,    45,    46,
       0,  -405,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    57,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    61,
       0,     0,    62,    63,    64,    65,    66,    67,    68,    69,
      70,     0,     0,     0,     0,     0,     0,     0,  1336,     0,
       0,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    73,    74,   359,
      75,   244,     0,   360,     0,   361,     0,     0,    77,    78,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1104,     0,   362,     0,     0,  1106,  1752,  1753,  1107,  1108,
    1109,  1110,  1111,  1112,  1113,  1114,  1115,  1116,  1117,  1118,
    -284,  1119,  1120,  1121,  1122,  1123,     0,  1124,     0,   363,
     364,     0,   461,     0,   366,  1125,  1126,    64,    65,    66,
      67,    68,    69,    70,   367,   368,   356,  1127,   369,   370,
     371,     0,   372,   373,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,  1336,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     374,     0,     0,    75,   375,     0,     0,     0,   279,     0,
     376,    77,    78,   377,   378,   379,   380,   359,     0,     0,
       0,   360,     0,   361,     0,  -175,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1104,     0,
     362,    -2,     0,  1106,     0,     0,  1107,  1108,  1109,  1110,
    1111,  1112,  1113,  1114,  1115,  1116,  1117,  1118,  -284,  1119,
    1120,  1121,  1122,  1123,     0,  1124,     0,   363,   364,     0,
     461,     0,   366,  1125,  1126,    64,    65,    66,    67,    68,
      69,    70,   367,   368,   356,  1127,   369,   370,   371,     0,
     372,   373,     0,     0,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,  1336,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   374,     0,
       0,    75,   375,     0,     0,     0,   279,     0,   376,    77,
      78,   377,   378,   379,   380,   359,     0,     0,     0,   360,
       0,   361,     0,  -175,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1104,     0,   362,     0,
       0,  1106,     0,     0,  1107,  1108,  1109,  1110,  1111,  1112,
    1113,  1114,  1115,  1116,  1117,  1118,  -284,  1119,  1120,  1121,
    1122,  1123,     0,  1124,     0,   363,   364,     0,   461,     0,
     366,  1125,  1126,    64,    65,    66,    67,    68,    69,    70,
     367,   368,   356,  1127,   369,   370,   371,     0,   372,   373,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   374,     0,     0,    75,
     375,     0,     0,     0,   279,     0,   376,    77,    78,   377,
     378,   379,   380,     0,     0,     0,     0,     0,     0,     0,
       0,  -175,   243,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,     0,     0,    45,    46,
       0,     0,     0,   318,    48,    49,    50,    51,    52,    53,
      54,     0,    13,    14,    15,    16,    17,    18,    57,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,     0,     0,
      45,    46,    62,    63,     0,    47,    48,    49,    50,    51,
      52,    53,    54,     0,     0,     0,     0,     0,     0,     0,
      57,     0,     0,     0,     0,     0,     0,    72,     0,  1039,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -586,
      75,   319,     0,     0,    62,    63,     0,     0,    77,    78,
     243,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    75,     0,     0,     0,    45,    46,     0,     0,
       0,   318,    48,    49,    50,    51,    52,    53,    54,     0,
      13,    14,    15,    16,    17,    18,    57,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,     0,     0,    45,    46,
      62,    63,     0,   318,    48,    49,    50,    51,    52,    53,
      54,     0,     0,     0,     0,     0,     0,     0,    57,     0,
       0,     0,     0,     0,     0,    72,     0,  1729,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    75,   319,
       0,     0,    62,    63,     0,     0,    77,    78,   243,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      75,     0,     0,     0,    45,    46,     0,     0,     0,   318,
      48,    49,    50,    51,    52,    53,    54,     0,     0,     0,
       0,     0,     0,     0,    57,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    62,    63,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    72,     0,  1731,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    75,   319,     0,     0,
       0,     0,     0,     0,    77,    78,   243,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,     0,     0,
       0,     0,    45,    46,     0,     0,     0,   318,    48,    49,
      50,    51,    52,    53,    54,     0,     0,     0,     0,     0,
       0,     0,    57,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
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
       0,     0,    75,   301,     0,     0,     0,     0,     0,     0,
      77,    78,   243,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,     0,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,  -405,  -405,     0,  -405,    45,    46,
       0,  -405,     0,     0,     0,     0,     0,     0,     0,     0,
      13,    14,    15,    16,    17,     0,     0,    19,    57,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,  -405,  -405,     0,  -405,    45,    46,
       0,  -405,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    57,     0,
       0,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      75,   244,     0,     0,     0,     0,     0,     0,    77,    78,
      13,    14,    15,    16,    17,    18,   657,    19,   658,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,   359,     0,    45,    46,
     360,     0,   361,    47,    48,    49,    50,    51,    52,    53,
      54,     0,     0,     0,     0,     0,     0,     0,    57,   362,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   659,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   363,   364,     0,   365,
       0,   366,    62,    63,    64,    65,    66,    67,    68,    69,
      70,   367,   368,   356,     0,   369,   370,   371,     0,   372,
     373,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   374,     0,     0,
      75,   660,     0,     0,     0,   279,     0,   376,    77,    78,
     661,   662,   379,   380,    13,    14,    15,    16,    17,    18,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,     0,     0,
     359,     0,    45,    46,   360,     0,   361,    47,    48,    49,
      50,    51,    52,    53,    54,     0,     0,     0,     0,     0,
       0,     0,    57,   362,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     363,   364,     0,   365,     0,   366,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   367,   368,   356,     0,   369,
     370,   371,     0,   372,   373,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   374,     0,   405,    75,   406,     0,     0,     0,     0,
       0,   376,    77,    78,   377,   378,   379,   380,    13,    14,
      15,    16,    17,    18,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,   359,     0,    45,    46,   360,     0,
     361,    47,    48,    49,    50,    51,    52,    53,    54,     0,
       0,     0,     0,     0,     0,     0,    57,   362,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   363,   364,     0,   365,     0,   366,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   367,
     368,   356,     0,   369,   370,   371,     0,   372,   373,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   374,     0,     0,    75,   660,
       0,     0,     0,   279,     0,   376,    77,    78,   377,   378,
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
       0,     0,    75,   406,     0,     0,     0,     0,     0,   376,
      77,    78,   377,   378,   379,   380,    13,    14,    15,    16,
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
       0,     0,     0,   376,    77,    78,   377,   378,   379,   380,
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
     377,   378,   379,   380,    13,    14,    15,    16,    17,    18,
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
       0,    73,    74,     0,    75,    76,     0,     0,     0,  -714,
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
      44,  -405,  -405,     0,  -405,    45,    46,     0,  -405,     0,
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
       0,     0,     0,     0,     0,    77,    78,   555,   243,     6,
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
       0,     0,     0,     0,  1413,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   922,    75,   916,
       0,     0,    62,    63,     0,     0,    77,    78,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      75,   916,     0,     0,     0,     0,     0,     0,    77,    78,
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
       0,     0,    45,    46,    62,    63,     0,   318,    48,    49,
      50,    51,    52,    53,    54,     0,     0,     0,     0,     0,
       0,     0,    57,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    75,   432,     0,     0,    62,    63,     0,     0,
      77,    78,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    75,   319,     0,     0,     0,     0,
       0,     0,    77,    78,    13,    14,    15,    16,    17,    18,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,     0,     0,
       0,     0,    45,    46,     0,     0,     0,   318,    48,    49,
      50,    51,    52,    53,    54,     0,    13,    14,    15,    16,
      17,    18,    57,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
       0,     0,     0,     0,    45,    46,    62,    63,     0,   318,
      48,    49,    50,    51,    52,    53,    54,     0,     0,     0,
       0,     0,     0,     0,    57,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    75,   286,     0,     0,    62,    63,
       0,     0,    77,    78,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    75,   432,     0,     0,
       0,     0,     0,     0,    77,    78,    13,    14,    15,    16,
      17,    18,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
       0,     0,     0,     0,    45,    46,     0,     0,     0,   318,
      48,    49,    50,    51,    52,    53,    54,     0,    13,    14,
      15,    16,    17,    18,    57,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,     0,     0,    45,    46,    62,    63,
       0,    47,    48,    49,    50,    51,    52,    53,    54,     0,
       0,     0,     0,     0,     0,     0,    57,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    75,   301,     0,     0,
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
       0,   318,    48,    49,    50,    51,    52,    53,    54,     0,
      13,    14,    15,    16,    17,    18,    57,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,     0,     0,    45,    46,
      62,    63,     0,   318,    48,    49,    50,    51,    52,    53,
      54,     0,     0,     0,     0,     0,     0,     0,    57,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    75,   916,
       0,     0,    62,    63,     0,     0,    77,    78,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      75,     0,     0,    13,    14,    15,    16,    17,    77,    78,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,  -405,  -405,     0,
    -405,    45,    46,     0,  -405,     0,     0,     0,     0,     0,
       0,     0,     0,    13,    14,    15,    16,    17,     0,     0,
      19,    57,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,  -405,  -405,     0,
    -405,    45,    46,     0,  -405,    62,    63,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    57,     0,     0,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    75,   301,    62,    63,     0,     0,     0,
       0,    77,    78,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    75,     0,     0,     0,     0,     0,     0,
       0,    77,    78,   243,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,     0,     0,     0,     0,    45,
      46,     0,     0,     0,   318,    48,    49,    50,    51,    52,
      53,    54,     0,     0,     0,     0,     0,     0,     0,    57,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    62,    63,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     843,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -599,    75,   243,     6,     7,     8,     9,    10,    11,    12,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1657,
       0,     0,     0,     0,   243,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     0,    19,
      75,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,     0,     0,
      45,    46,     0,     0,     0,   318,    48,    49,    50,    51,
      52,    53,    54,     0,     0,     0,     0,     0,     0,     0,
      57,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   243,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    62,    63,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,  -405,  -405,     0,  -405,    45,    46,     0,
    -405,     0,     0,     0,     0,     0,     0,     0,     0,     0,
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
       0,    13,    14,    15,    16,    17,     0,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,  -405,  -405,    75,  -405,    45,
      46,     0,  -405,     0,     0,   359,     0,     0,     0,   360,
       0,   361,     0,     0,     0,     0,     0,     0,     0,    57,
       0,     0,     0,     0,     0,     0,     0,     0,   362,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    62,    63,   363,   364,     0,   461,     0,
     366,    62,    63,    64,    65,    66,    67,    68,    69,    70,
     367,   368,   356,     0,   369,   370,   371,   359,   372,   373,
       0,   360,     0,   361,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     362,    75,     0,     0,     0,     0,   374,    74,     0,   462,
     463,     0,     0,     0,   464,     0,   376,    77,    78,   377,
     378,   379,   380,     0,     0,     0,     0,   363,   364,     0,
     365,     0,   366,    62,    63,    64,    65,    66,    67,    68,
      69,    70,   367,   368,   356,     0,   369,   370,   371,   359,
     372,   373,     0,   360,     0,   361,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   362,     0,     0,     0,     0,     0,   374,  1221,
       0,    75,   375,     0,     0,     0,  1222,     0,   376,    77,
      78,   377,   378,   379,   380,     0,     0,     0,     0,   363,
     364,     0,   365,     0,   366,    62,    63,    64,    65,    66,
      67,    68,    69,    70,   367,   368,   356,     0,   369,   370,
     371,   359,   372,   373,     0,   360,     0,   361,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   362,     0,     0,     0,     0,     0,
     374,     0,     0,    75,   375,     0,     0,     0,   464,     0,
     376,    77,    78,   377,   378,   379,   380,     0,     0,     0,
       0,   363,   364,     0,   365,     0,   366,    62,    63,    64,
      65,    66,    67,    68,    69,    70,   367,   368,   356,     0,
     369,   370,   371,   359,   372,   373,     0,   360,     0,   361,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   362,     0,     0,     0,
       0,     0,   374,   789,     0,    75,   375,     0,     0,     0,
       0,     0,   376,    77,    78,   377,   378,   379,   380,     0,
       0,     0,     0,   363,   364,     0,   365,     0,   366,    62,
      63,    64,    65,    66,    67,    68,    69,    70,   367,   368,
     356,     0,   369,   370,   371,   359,   372,   373,     0,   360,
       0,   361,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   362,     0,
       0,     0,     0,     0,   374,     0,     0,    75,   375,     0,
       0,     0,   279,     0,   376,    77,    78,   377,   378,   379,
     380,     0,     0,     0,     0,   363,   364,     0,   365,     0,
     366,    62,    63,    64,    65,    66,    67,    68,    69,    70,
     367,   368,   356,     0,   369,   370,   371,   359,   372,   373,
       0,   360,     0,   361,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     362,     0,     0,     0,     0,     0,   374,   947,     0,    75,
     375,     0,     0,     0,     0,     0,   376,    77,    78,   377,
     378,   379,   380,     0,     0,     0,     0,   363,   364,     0,
     365,     0,   366,    62,    63,    64,    65,    66,    67,    68,
      69,    70,   367,   368,   356,     0,   369,   370,   371,   359,
     372,   373,     0,   360,     0,   361,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   362,     0,     0,     0,     0,     0,   374,     0,
       0,    75,   375,     0,     0,   978,     0,     0,   376,    77,
      78,   377,   378,   379,   380,     0,     0,     0,     0,   363,
     364,     0,   365,     0,   366,    62,    63,    64,    65,    66,
      67,    68,    69,    70,   367,   368,   356,     0,   369,   370,
     371,   359,   372,   373,     0,   360,     0,   361,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   362,     0,     0,     0,     0,     0,
     374,  1285,     0,    75,   375,     0,     0,     0,     0,     0,
     376,    77,    78,   377,   378,   379,   380,     0,     0,     0,
       0,   363,   364,     0,   365,     0,   366,    62,    63,    64,
      65,    66,    67,    68,    69,    70,   367,   368,   356,     0,
     369,   370,   371,   359,   372,   373,     0,   360,     0,   361,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   362,     0,     0,     0,
       0,     0,   374,     0,     0,    75,   375,     0,     0,     0,
    1346,     0,   376,    77,    78,   377,   378,   379,   380,     0,
       0,     0,     0,   363,   364,     0,   365,     0,   366,    62,
      63,    64,    65,    66,    67,    68,    69,    70,   367,   368,
     356,     0,   369,   370,   371,   359,   372,   373,     0,   360,
       0,   361,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   362,     0,
       0,     0,     0,     0,   374,     0,  1758,    75,   375,     0,
       0,     0,     0,     0,   376,    77,    78,   377,   378,   379,
     380,     0,     0,     0,     0,   363,   364,     0,   365,     0,
     366,    62,    63,    64,    65,    66,    67,    68,    69,    70,
     367,   368,   356,     0,   369,   370,   371,   359,   372,   373,
       0,   360,     0,   361,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     362,     0,     0,     0,     0,     0,   374,  1958,     0,    75,
     375,     0,     0,     0,     0,     0,   376,    77,    78,   377,
     378,   379,   380,     0,     0,     0,     0,   363,   364,     0,
     365,     0,   366,    62,    63,    64,    65,    66,    67,    68,
      69,    70,   367,   368,   356,     0,   369,   370,   371,   359,
     372,   373,     0,   360,     0,   361,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   362,     0,     0,     0,     0,     0,   374,     0,
       0,    75,   375,     0,     0,     0,     0,     0,   376,    77,
      78,   377,   378,   379,   380,     0,     0,     0,     0,   363,
     364,     0,   365,     0,   366,    62,    63,    64,    65,    66,
      67,    68,    69,    70,   367,   368,   356,     0,   369,   370,
     371,   359,   372,   373,     0,   360,     0,   361,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   362,     0,     0,     0,     0,     0,
     646,     0,     0,    75,   375,     0,     0,     0,     0,     0,
     376,    77,    78,   377,   378,   379,   380,     0,     0,     0,
       0,   363,   364,     0,   365,     0,   366,    62,    63,    64,
      65,    66,    67,    68,    69,    70,   367,   368,   356,     0,
     369,   370,   371,   359,   372,   373,     0,   360,     0,   361,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   362,     0,     0,     0,
       0,     0,   651,     0,     0,    75,   375,     0,     0,     0,
       0,     0,   376,    77,    78,   377,   378,   379,   380,     0,
       0,     0,     0,   363,   364,     0,   365,     0,   366,    62,
      63,    64,    65,    66,    67,    68,    69,    70,   367,   368,
     356,     0,   369,   370,   371,   359,   372,   373,     0,   360,
       0,   361,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   362,     0,
       0,     0,     0,     0,   654,     0,     0,    75,   375,     0,
       0,     0,     0,     0,   376,    77,    78,   377,   378,   379,
     380,     0,     0,     0,     0,   363,   364,     0,   365,     0,
     366,    62,    63,    64,    65,    66,    67,    68,    69,    70,
     367,   368,   356,     0,   369,   370,   371,   359,   372,   373,
       0,   360,     0,   361,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     362,     0,     0,     0,     0,     0,   374,     0,     0,    75,
     375,     0,     0,     0,     0,     0,   376,   857,    78,   377,
     378,   379,   380,     0,     0,     0,     0,   363,   364,     0,
     365,     0,   366,    62,    63,    64,    65,    66,    67,    68,
      69,    70,   367,   368,   356,     0,   369,   370,   371,     0,
     372,   373,     0,     0,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   374,     0,
       0,    75,   375,     0,     0,     0,     0,     0,   376,   437,
      78,   377,   378,   379,   380
};

static const yytype_int16 yycheck[] =
{
       1,    73,     1,     4,    73,   131,    73,    73,   177,    95,
     241,   407,   256,     1,   628,   162,   684,   173,     4,   222,
     283,   374,   219,   207,   671,     1,   178,   464,     4,   865,
     664,   150,   603,   162,   612,    82,   178,   337,     1,     0,
     758,   341,   219,   322,  1632,   631,   764,   728,   852,  1632,
     320,    75,   534,   219,    55,    56,  1752,    58,  1092,    58,
      70,   120,   163,   139,     1,   513,   514,   337,  1695,   599,
      58,   341,    73,  1632,   757,   219,  1633,   757,    82,  1270,
    1271,    82,    58,    82,    73,   211,   599,   290,   291,    90,
     775,   599,   574,   152,    95,    58,    95,    98,   295,    98,
     147,   102,   162,   102,   852,    73,    82,  1195,   793,   181,
      98,   149,   181,   151,   181,   191,   102,   236,   295,     1,
     755,    58,   836,   755,    58,   953,   755,   755,  1162,   295,
     219,   219,   149,    95,   220,    87,    96,   115,     1,   140,
     219,     4,   143,   153,   145,   149,   145,   219,   862,   150,
     219,   295,   219,   219,   982,   156,   173,   145,   284,    75,
      76,    87,   163,   139,    70,   444,   780,    87,   438,   145,
    1113,   147,  1868,  1892,   859,   153,    58,  1896,   192,   180,
     181,   180,   145,     1,  1741,   633,   757,  1756,    87,   149,
      75,    76,   181,   131,   195,    58,   195,     0,   245,   228,
     220,    70,   231,   155,   205,   157,   295,   295,   145,   210,
     296,   145,   174,   181,    73,   191,   295,  1045,   219,   220,
       0,   220,     1,   295,   253,   755,   295,   165,   295,   295,
     219,   157,   115,   629,   263,   236,  1097,   157,   149,   102,
      58,   157,   755,   149,   245,   913,   245,   755,   374,   374,
     274,   219,   496,  1530,   255,   302,   151,   258,   157,   258,
     320,   156,     1,   145,   265,   149,   242,    73,    82,   245,
     258,  1305,   157,   941,   275,   276,   296,   278,  1555,    58,
     149,  1838,   145,    89,   153,   482,   470,   735,   588,    59,
      60,  1009,   605,   907,   295,   296,   930,   296,   561,  1887,
     106,  1105,   303,  1930,  1887,   482,    70,   283,   309,   310,
     991,     1,   591,   314,     4,   886,   482,  1405,   588,    58,
     906,   229,   181,   102,     1,   625,   302,   145,  1887,   682,
    1134,  1888,  1015,   147,   322,  1015,   615,    19,   482,  1908,
     103,   442,   612,   622,   107,   346,   254,   110,   130,   112,
     351,   833,   265,   354,   355,   625,   264,  1105,   434,    73,
     219,   408,   315,   276,  1921,   179,   145,  1936,    58,   774,
     775,   568,  1057,  1008,    88,  1089,  1008,   503,   438,  1008,
    1008,    58,   508,     4,   146,   149,  1134,   157,   793,   153,
     172,   568,   511,   482,   482,   131,   146,  1097,   517,   525,
     353,   283,   568,   482,   174,   491,   145,   157,    98,   535,
     482,   173,   102,   482,   155,   482,  1228,   418,    95,   418,
     155,    98,  1870,   173,   568,   102,  1617,  1618,  1097,   165,
      70,   245,   408,   174,    55,    56,  1884,   149,   462,   174,
     441,   442,   149,   572,  1015,   155,   759,  1308,  1309,  1310,
     763,   149,   453,   454,   859,   145,   444,   131,   434,   772,
     773,   462,  1910,   464,   174,   283,  1184,   146,   145,    90,
      70,   491,    70,   152,   157,   504,   104,   105,  1008,   568,
     568,   482,   104,   105,   570,   248,   160,   161,   302,   568,
     491,   174,   491,   482,   173,  1008,   568,   526,   155,   568,
    1008,   568,   154,   532,   283,   519,   320,   536,   155,   149,
     511,   149,   572,   153,   482,   153,   517,   174,   195,   140,
     646,   646,   143,   648,   649,   651,   651,   174,   654,   654,
     152,   160,   657,   658,   659,   156,    70,   572,   167,   168,
     709,  1259,   163,   157,   283,   104,   105,   673,   985,   149,
     570,   149,   612,   153,   555,   153,   557,   125,   126,    70,
     174,   324,   325,   149,   327,   844,   329,   568,   258,   570,
     558,   570,   174,   873,    56,   589,   155,    59,    60,   555,
      62,   258,   583,   149,  1198,   561,   587,   153,   155,   210,
      70,  1238,   155,   155,   408,   174,   875,   174,   782,  1427,
    1178,   169,   170,   873,   155,  1191,     3,   362,  1308,  1309,
    1310,   174,   174,  1804,   165,   149,   768,   155,   619,   153,
    1432,  1433,  1434,   482,   438,   533,   768,   615,   155,   152,
     631,  1245,   387,   388,   255,    70,   174,   779,   149,  1308,
    1309,  1310,   153,   149,   265,     3,   815,   174,    70,   151,
     149,   157,  1057,   408,   275,   276,   757,   278,   157,  1327,
     162,   163,   155,   155,   148,   157,  1502,   149,   150,   149,
      70,   155,   165,   153,   675,   676,   795,   678,   149,   561,
      70,   682,   303,   438,   685,   156,   710,    70,   309,   310,
      70,   150,   155,   314,   820,   814,   148,   711,   157,   513,
     514,   151,  1349,   155,    70,   831,   156,   589,   834,   710,
      46,    47,   838,    49,   149,   151,   152,    53,   153,  1032,
     904,    70,    70,  1171,   151,   346,  1530,   149,  1532,   156,
     351,   153,  1675,   354,   162,   163,  1679,   151,   752,   774,
     775,   418,   156,   561,    12,    13,    14,    15,    16,   149,
     149,  1555,   351,   153,   755,   354,   757,   173,   793,   149,
     774,   775,   151,   153,   153,   157,   149,   530,   769,   149,
     153,   157,  1600,   153,  1602,   776,   149,  1424,   171,   793,
     559,   782,   561,   149,   785,  1222,    56,   153,  1402,    59,
      60,   151,    62,   794,   795,   796,   897,   926,   612,  1195,
    1044,   149,    70,   129,   115,   153,   835,   152,   153,  1423,
     151,   156,   151,   814,   491,   155,   155,   151,   149,   633,
     441,   155,   561,   149,   859,   149,  1769,   153,   129,   153,
     123,   124,   453,   454,   160,   161,   978,   127,   128,    12,
      13,    14,    15,    16,   149,   859,   149,   173,   149,   850,
     851,   852,   153,   149,   151,   151,   844,   153,   155,   160,
     161,   129,   154,   155,   852,   151,   926,   855,  1052,   155,
     752,   154,   155,  1076,   151,   129,   852,   151,   864,   151,
     151,   149,  1825,   155,   155,   153,   151,   875,   864,   852,
    1833,   926,   160,   161,   151,   149,   897,    70,   155,   153,
     901,   160,   161,  1182,   151,   906,   160,   161,  1178,  1346,
     149,   912,  1054,  1014,  1015,   852,    12,    13,    14,    15,
      16,   735,    21,  1537,    12,    13,    14,    15,    16,     3,
     149,   151,   149,   688,   555,   155,     3,  1880,    12,    13,
      14,    15,    16,   155,   945,    12,    13,    14,    15,    16,
     151,   155,   953,   149,   155,   129,  1187,   153,   131,  1312,
     151,   155,   583,   192,   155,  1091,   587,   149,  1772,   151,
     852,   153,   151,   149,    70,   149,   155,   153,  1104,   153,
    1594,   982,    70,   155,   985,  1422,   160,   161,    96,   852,
     108,   109,   110,   111,   112,  1121,    70,   149,   619,  1585,
     149,   864,   118,    70,   120,   121,   122,  1008,   151,  1405,
     631,   157,   155,  1014,  1015,  1819,  1172,  1173,   696,   697,
     698,   699,  1057,     4,     5,     6,     7,     8,     9,    10,
      11,  1835,   157,   149,   852,   131,   152,   153,   689,   690,
     691,   157,   158,  1057,  1045,   149,  1165,   151,   151,   153,
    1097,   865,   155,    87,   675,   676,  1300,   678,   149,   151,
     151,   682,   153,   155,   685,   151,   151,  1871,   151,   155,
     155,   151,   155,   852,   151,   155,  1205,   148,   155,   151,
     155,   156,    63,   155,  1209,   143,   144,   145,   151,   710,
     151,   320,   155,   157,   323,  1709,   151,   155,   151,  1241,
     155,   157,   155,   143,   144,   145,  1107,   165,   337,  1110,
    1111,  1112,   341,   852,   157,   155,   174,  1105,  1178,  1787,
     157,  1097,  1708,   154,  1271,   165,  1278,  1279,   151,  1105,
     151,   151,   155,  1134,   174,   155,  1278,  1279,   173,  1140,
     154,   155,  1105,   154,   155,  1205,  1134,  1148,   769,   115,
    1151,  1152,  1151,  1152,  1155,   776,   154,   155,  1134,  1493,
    1494,  1495,   852,  1151,  1165,   149,  1152,   922,  1105,   149,
    1205,  1134,   927,   794,   864,   796,  1094,  1095,  1096,   154,
     155,  1795,   149,   938,    12,    13,    14,    15,    16,    17,
    1191,   154,   155,   155,  1182,   154,   155,  1134,   154,   155,
     154,   155,   166,  1329,  1330,  1206,   154,   155,   158,   438,
     161,     4,     5,     6,     7,     8,     9,    10,    11,   154,
     155,  1222,   158,  1105,   154,   155,   159,  1228,   171,   850,
     851,   852,  1318,   154,   155,  1270,   149,   150,   154,   155,
     101,  1367,  1105,   104,   105,   106,   107,   108,   109,   110,
     111,   112,  1134,   154,   155,   152,  1270,  1271,  1387,  1260,
     129,  1308,  1309,  1310,   151,  1312,  1313,  1530,   154,   155,
     151,  1134,   154,   155,   205,   151,   897,   154,   155,   151,
     901,   155,   156,   154,   155,   906,   151,  1105,   151,  1152,
     519,   153,  1555,   131,   154,   155,   155,   156,  1318,    75,
      76,   154,     9,    12,    13,    14,    15,    16,    17,  1506,
     155,   156,   131,   174,  1229,  1230,  1134,  1318,   692,   693,
     156,  1322,   700,   701,  1325,  1311,  1105,  1387,   155,  1506,
     694,   695,  1308,  1309,  1310,  1311,  1312,  1313,  1513,  1514,
    1506,  1172,  1173,   572,   156,  1346,   156,   149,    17,   154,
    1105,   151,  1387,   151,   151,  1134,   151,  1171,   151,   588,
     589,   151,  1506,   151,  1178,  1366,  1105,  1368,   149,  1368,
     153,   157,   157,  1152,    12,    13,    14,    15,    16,    17,
    1368,   174,   157,   612,   157,  1554,    55,    56,    57,    58,
      59,    60,    61,    62,   101,  1134,   625,   104,   105,   106,
     107,   108,   109,   110,   111,   112,     4,     5,     6,     7,
       8,     9,    10,    11,  1500,  1105,    68,  1506,  1506,  1566,
    1546,  1422,  1508,  1178,  1410,   154,  1427,  1506,   149,    76,
     154,  1432,  1433,  1434,  1410,    17,   173,  1566,   155,   157,
    1506,   149,   174,   157,  1134,   151,   151,   174,  1311,    17,
    1647,    12,    13,    14,    15,    16,    17,  1212,  1213,  1214,
     157,  1151,  1152,   154,  1219,  1220,   154,  1592,   148,   151,
    1647,  1618,   151,   151,  1151,  1152,   151,   151,   151,   151,
    1500,  1647,   711,   151,   148,   151,  1107,   157,  1508,  1110,
    1111,  1112,    12,    13,    14,    15,    16,   157,   157,  1500,
     151,    68,   173,  1647,  1518,  1506,  1566,  1508,   151,  1779,
     151,   174,   148,  1134,  1515,   157,   151,   151,   155,  1140,
     151,  1690,   151,   752,   151,   155,   151,  1148,  1529,   151,
    1531,   462,   151,   464,  1155,   151,   151,   151,   151,   151,
     151,   151,  1530,   151,  1532,   151,   151,  1410,   148,   151,
      70,   154,   151,   173,  1530,   151,  1532,   148,  1647,  1647,
    1839,   151,  1563,   155,  1650,   149,  1836,  1555,  1647,   149,
    1191,   149,   149,   149,   149,    13,    72,   156,    89,  1555,
     174,  1700,  1617,  1569,  1585,  1206,   155,   174,   174,   154,
     156,   154,   148,  1569,   148,   157,   155,   154,   151,  1600,
    1752,  1602,   174,  1617,  1618,   151,   155,   155,   151,   129,
    1752,   155,   154,   151,  1783,   151,   174,   148,   148,   174,
     149,  1311,   174,    12,    13,    14,    15,    16,    78,   149,
    1650,   149,   151,   153,   151,   174,   148,   174,   151,   174,
     160,   161,   174,   149,   873,   148,  1647,   876,  1530,  1650,
    1532,   174,   174,   174,   148,   155,   155,  1804,   550,  1660,
     157,   154,   154,   154,  1750,   151,   154,  1530,   148,  1532,
     151,   156,   156,  1555,   118,   148,   151,   151,  1368,   151,
     151,    70,  1683,  1671,     1,   154,  1672,     4,  1502,   154,
    1887,  1368,  1555,   151,   148,   174,   156,   926,  1699,  1700,
     155,  1322,   151,   149,  1325,   107,  1569,  1708,   149,   149,
    1887,   154,  1530,   148,  1532,  1867,  1868,   154,   154,  1838,
    1410,  1887,   148,  1892,   157,  1867,  1868,  1896,  1897,   151,
    1750,   151,  1779,   151,   151,  1107,   151,  1555,   154,   151,
     129,    58,  1894,  1887,   151,  1366,   155,   148,   151,  1750,
     149,  1530,  1894,  1532,  1923,  1756,    73,   151,    88,  1760,
     149,   174,   154,   154,   153,    82,   148,   148,   151,   153,
    1771,   160,   161,   151,  1762,  1944,  1555,  1919,    95,  1948,
     151,    98,   151,  1784,  1772,   102,   151,   151,   156,  1836,
    1804,  1530,   151,  1532,   174,   174,  1772,   174,  1887,  1887,
     174,   156,  1888,   148,  1973,  1560,   155,   174,  1887,  1672,
     151,   151,   151,   174,  1966,  1887,  1555,  1943,  1887,   152,
    1887,  1887,   139,   174,  1966,   156,   101,   149,   145,   155,
     147,  1819,    73,   150,   151,  1921,  1962,  1838,   149,  1840,
    1530,   148,  1532,  1819,   150,   162,   150,  1835,   107,   174,
    1851,   782,  1853,  1854,   785,   154,   174,   107,   151,  1835,
     165,   156,   179,   180,   181,  1555,   151,   165,  1888,   148,
     148,   151,   149,   174,   191,   192,    73,   151,   195,  1569,
     151,   376,   174,  1871,   174,  1327,  1887,  1888,  1592,  1888,
    1772,  1235,   663,  1672,  1515,  1871,   702,  1898,   705,   703,
    1123,  1921,   219,   220,  1134,   704,   101,  1908,  1529,  1772,
    1531,   106,   107,   108,   109,   110,   111,   112,   113,   236,
    1921,  1936,  1921,  1555,   706,  1868,  1682,  1884,   245,   821,
    1772,  1828,  1788,  1934,  1931,  1936,  1930,  1819,  1918,  1547,
    1547,   258,  1563,  1836,  1835,  1948,  1897,  1155,    48,  1178,
    1951,  1500,  1750,  1835,  1772,  1956,  1819,  1812,   153,   250,
    1313,  1514,  1148,   471,  1585,   782,   868,  1410,  1569,  1970,
       0,   288,  1835,  1974,   583,   912,  1205,   294,   295,   296,
     727,  1671,  1672,  1984,   727,   302,   878,   727,    -1,  1871,
      -1,    -1,    -1,  1772,  1366,    -1,    -1,    -1,    -1,    -1,
      -1,  1819,    -1,   320,   321,   322,   821,     9,  1871,    -1,
      -1,    -1,    -1,    -1,   945,    -1,    -1,  1835,    -1,    -1,
     337,    -1,   953,    73,   341,    -1,    -1,    -1,    -1,    -1,
      -1,  1786,    -1,  1772,    -1,    -1,    -1,    -1,    -1,  1660,
    1819,    -1,    -1,    -1,    -1,    95,    -1,    -1,    -1,    -1,
      -1,   982,    -1,  1871,   985,    -1,  1835,   374,    -1,   101,
      -1,    -1,  1683,   878,   106,   107,   108,   109,   110,   111,
     112,    -1,  1762,    -1,    -1,    -1,    -1,    -1,  1699,    -1,
    1819,    -1,  1772,    -1,    -1,    -1,    -1,  1708,    -1,    -1,
      -1,   408,  1871,    -1,   411,   987,  1835,    -1,    -1,   101,
     150,   418,   104,   105,   106,   107,   108,   109,   110,   111,
     112,    -1,  1004,  1005,  1045,    -1,    -1,   434,    -1,    -1,
      -1,   438,    -1,    -1,    -1,   442,    -1,   444,    -1,  1819,
      -1,    -1,  1871,    17,    -1,  1756,    -1,    -1,    -1,  1760,
      -1,    -1,    -1,    -1,    -1,  1835,    -1,    -1,    -1,    -1,
    1771,    -1,    -1,    -1,    -1,    -1,    -1,  1529,  1387,  1531,
      -1,    -1,    -1,  1784,    -1,   482,    -1,    12,    -1,   219,
     220,    -1,   987,    -1,   491,    59,    60,    61,    62,    -1,
      76,  1871,    -1,    -1,    -1,    -1,   236,    -1,    -1,  1004,
    1005,  1563,    -1,    -1,   511,    98,   513,   514,    -1,    -1,
     517,    -1,   519,    -1,    -1,   101,   109,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   101,    -1,  1840,
      -1,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
    1851,    -1,  1853,  1854,    -1,    -1,    -1,    -1,    -1,    -1,
     557,    86,    -1,    -1,    -1,   295,   296,    -1,   151,    -1,
      -1,   568,    -1,   570,    -1,   572,   101,    -1,    -1,   104,
     105,   106,   107,   108,   109,   110,   111,   112,    -1,   153,
      -1,   588,   589,    -1,   591,    -1,    -1,  1898,   174,    -1,
      -1,    -1,   599,    -1,    -1,    -1,   603,  1908,  1660,    -1,
      -1,  1222,   195,    -1,    -1,   612,    -1,  1228,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   622,    -1,    -1,   625,    -1,
      -1,  1683,    -1,  1934,    -1,  1936,   633,    -1,    -1,    -1,
      -1,    -1,    -1,    63,    64,    65,    66,    -1,    -1,   646,
    1951,   648,   649,    -1,   651,  1956,    -1,   654,    -1,    -1,
     657,   658,   659,    -1,    -1,    -1,    -1,    -1,    -1,  1970,
      -1,    -1,    -1,  1974,    -1,   258,    -1,    -1,    -1,  1251,
    1252,   101,    -1,  1984,   104,   105,   106,   107,   108,   109,
     110,   111,   112,  1265,  1266,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1756,   288,    70,    -1,  1760,    -1,
      -1,   294,   442,    -1,   711,    -1,    -1,    -1,    -1,  1771,
      -1,    -1,    -1,    -1,    -1,    -1,  1298,  1299,     3,    -1,
     727,   728,  1784,   153,    -1,  1346,    -1,   101,   735,   322,
     104,   105,   106,   107,   108,   109,   110,   111,   112,    -1,
      -1,   171,   482,    -1,    -1,   752,  1251,  1252,   755,    -1,
     757,   491,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1265,  1266,    -1,    -1,    -1,    -1,    -1,   774,   775,    -1,
      -1,   511,    -1,    -1,    -1,   149,   150,   517,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   793,    -1,   795,  1851,
      -1,  1853,  1854,  1298,  1299,    -1,     1,    -1,    -1,     4,
      -1,  1422,    -1,    -1,    -1,    -1,  1427,   814,    -1,    -1,
      -1,  1432,  1433,  1434,    -1,    -1,   101,   557,    -1,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   568,    -1,
     570,    -1,   101,    -1,    -1,    -1,  1898,   106,   107,   108,
     109,   110,   111,   112,   129,   852,  1908,    -1,    -1,    -1,
      -1,   444,   859,    58,    -1,    -1,    -1,   864,   865,    -1,
    1779,    -1,    -1,    -1,   149,   150,   873,    -1,   875,    -1,
      -1,   156,  1934,    70,  1936,   160,   161,    82,    -1,   886,
      -1,   150,    -1,    -1,   153,    -1,    -1,    -1,    -1,  1951,
      -1,    -1,    -1,    98,  1956,    -1,    -1,   102,    -1,    -1,
    1482,  1483,    -1,    -1,   101,    -1,    -1,    -1,  1970,   106,
     107,   108,   109,   110,   111,   112,    -1,  1836,    -1,   926,
      -1,    -1,    -1,    -1,    -1,   518,    -1,    -1,    -1,    -1,
      -1,    -1,   129,    -1,   139,    -1,  1518,    -1,   230,    -1,
     145,    -1,   147,    -1,    -1,   538,   151,    12,    13,    14,
      15,    16,   149,   150,    -1,    -1,   161,   162,   163,    -1,
      -1,   101,    -1,   160,   161,   558,   106,   107,   108,   109,
     110,   111,   112,    -1,   179,    -1,    -1,  1482,  1483,  1600,
      -1,  1602,    -1,    -1,   991,    -1,   191,   192,    -1,   129,
     195,    -1,    -1,    -1,    -1,  1577,    -1,    -1,   591,    -1,
      -1,  1008,    -1,    -1,    -1,    70,    -1,    -1,  1015,   149,
     150,    -1,    -1,   153,    -1,   755,    -1,   757,    -1,    -1,
     160,   161,   615,    -1,    -1,  1607,    -1,    -1,    -1,   622,
    1612,  1613,    -1,    -1,    -1,    -1,   101,   242,    -1,    -1,
     245,   106,   107,   108,   109,   110,   111,   112,    -1,    -1,
    1057,   101,    -1,   258,    -1,   795,   106,   107,   108,   109,
     110,   111,   112,   113,   129,    -1,    -1,   117,    -1,   119,
     275,    -1,  1577,    -1,   814,    -1,    -1,    -1,   283,    -1,
      -1,    -1,    -1,   288,   149,   150,    -1,    -1,    -1,   294,
    1097,    -1,    -1,    -1,    -1,   160,   161,   302,  1105,    -1,
     150,    -1,  1607,   153,    -1,    -1,    -1,  1612,  1613,    -1,
      -1,    -1,    -1,    -1,    -1,   320,    -1,   322,   323,   411,
      -1,    12,    13,    14,    15,    16,    -1,  1134,    -1,    -1,
      -1,    -1,   337,    -1,    -1,   427,   341,    -1,   430,    -1,
      -1,    -1,    -1,    -1,  1151,  1152,    -1,    -1,    12,    13,
      14,    15,    16,    -1,    -1,    -1,    -1,    -1,  1165,    -1,
      -1,    -1,    -1,    -1,  1171,    -1,    -1,    -1,    -1,   374,
      -1,  1178,    -1,    -1,    -1,    -1,    -1,   101,    -1,    70,
     104,   105,   106,   107,   108,   109,   110,   111,   112,    -1,
      -1,    -1,    -1,    -1,    -1,   487,    -1,    -1,  1205,    -1,
      -1,    -1,  1209,   408,    -1,    -1,    70,    -1,    -1,    -1,
     101,    -1,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   434,
      -1,    -1,    -1,   438,    -1,    -1,    -1,   101,   129,   444,
     104,   105,   106,   107,   108,   109,   110,   111,   112,    -1,
     174,   844,    -1,    -1,    -1,    -1,    -1,    -1,   149,   150,
      -1,    -1,   855,  1270,  1271,   129,    -1,    -1,  1008,   160,
     161,    -1,    -1,    -1,  1014,  1015,    -1,    -1,    -1,    -1,
      -1,    -1,   875,    -1,    -1,   149,   150,    -1,    -1,   153,
      -1,    -1,    -1,    -1,    -1,    -1,   160,   161,    -1,    70,
      -1,  1308,  1309,  1310,  1311,  1312,  1313,    -1,   513,   514,
     101,  1318,  1319,   518,   519,   106,   107,   108,   109,   110,
     111,   112,   113,  1905,    -1,    -1,   117,    -1,   119,    -1,
     101,    -1,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,    -1,    -1,    -1,   550,    -1,    -1,    -1,    -1,
     555,    -1,    -1,   558,   559,    -1,   561,    -1,   129,   150,
      -1,  1368,   153,    -1,    -1,    -1,    -1,   572,    -1,    -1,
      -1,    -1,    -1,    12,    13,    14,    15,    16,   149,   150,
    1387,    -1,   587,   588,   589,    -1,   591,    -1,   101,   160,
     161,   104,   105,   106,   107,   108,   109,   110,   111,   112,
    1905,    -1,    -1,  1410,    -1,    -1,    -1,   612,    -1,    -1,
     615,    -1,    -1,    -1,   619,    -1,    -1,   622,    -1,    -1,
     625,    -1,   627,    -1,    -1,  1165,    -1,    -1,   633,    -1,
      -1,    70,    -1,    -1,    -1,   727,   728,    -1,    -1,    -1,
      -1,   646,    -1,   648,   649,   737,   651,   160,   740,   654,
      -1,    -1,   657,   658,   659,    -1,    -1,    12,    13,    14,
      15,    16,   101,    -1,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,   101,    -1,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,    -1,    -1,  1496,
     129,    -1,    -1,  1500,    -1,  1502,    -1,    -1,    -1,  1506,
      -1,  1508,    -1,    -1,    -1,    -1,   711,    -1,    -1,   801,
     149,   150,    -1,   805,   153,    70,    -1,   809,    -1,    -1,
    1260,   160,   161,  1530,    -1,  1532,   152,    -1,    -1,    -1,
     735,   157,    -1,    -1,    -1,    -1,    -1,    -1,    12,    13,
      14,    15,    16,    -1,    -1,    -1,   101,   752,  1555,   104,
     105,   106,   107,   108,   109,   110,   111,   112,  1151,  1566,
      -1,    -1,  1569,    -1,    -1,    -1,    -1,    -1,    -1,   774,
     775,    -1,    -1,    -1,   129,    -1,    -1,    -1,  1318,    -1,
      -1,    -1,    -1,    -1,    -1,  1592,    -1,    -1,   793,  1182,
      -1,    -1,    -1,    -1,   149,   150,    70,    -1,    -1,    -1,
      -1,     1,    -1,    -1,     4,   160,   161,    -1,    -1,    -1,
    1617,  1618,    -1,   101,    -1,    -1,   821,    -1,   106,   107,
     108,   109,   110,   111,   112,  1632,  1633,   101,    -1,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   844,
    1647,    -1,    -1,  1650,    -1,    -1,    -1,   852,    -1,    -1,
     855,    -1,    -1,    -1,   859,   129,    -1,    -1,    58,   864,
     865,   149,   150,    -1,  1671,  1672,    -1,    -1,   873,    -1,
     875,   876,    -1,   878,    -1,   149,   150,    -1,    -1,    -1,
      -1,    -1,    82,    -1,    -1,    -1,   160,   161,    -1,    -1,
      -1,    -1,    -1,  1700,    -1,    -1,    -1,    -1,    -1,   991,
      -1,    -1,   102,    -1,   101,    -1,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    -1,    -1,    -1,    -1,
      -1,   926,    -1,    -1,    -1,    -1,  1319,    -1,    -1,    -1,
      -1,    -1,   129,    -1,  1741,    -1,    -1,    -1,    -1,   139,
      -1,    -1,    -1,  1750,    -1,   145,    -1,    -1,    -1,    -1,
      -1,    -1,   149,   150,    -1,  1762,   153,    -1,    -1,    -1,
    1500,    -1,   162,   160,   161,  1772,  1506,    -1,  1508,    -1,
    1062,    -1,  1779,  1065,    -1,  1368,   173,    -1,   101,    -1,
     180,    -1,   987,   106,   107,   108,   109,   110,   111,   112,
     113,   191,   192,    -1,   117,    -1,   119,  1804,    -1,  1004,
    1005,    -1,    -1,    -1,    -1,  1812,    -1,    -1,    -1,    -1,
      -1,    -1,  1819,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     220,    -1,    -1,    -1,    -1,    -1,    -1,   150,  1835,  1836,
     153,  1838,  1839,    -1,    -1,    -1,   236,    -1,    -1,    -1,
      -1,   241,   242,    -1,    -1,   245,    -1,    -1,    -1,    -1,
     101,     1,  1057,   104,   105,   106,   107,   108,   109,   110,
     111,   112,    -1,    -1,  1871,    -1,    -1,   267,    -1,    -1,
     270,    -1,   272,    -1,    -1,    -1,    -1,    -1,    -1,   101,
    1887,  1888,    -1,   283,   106,   107,   108,   109,   110,   111,
     112,   113,  1097,    -1,    -1,   117,   296,   119,    -1,    -1,
    1105,    -1,    -1,    -1,    -1,    -1,   157,  1647,    58,    -1,
    1650,    -1,    -1,    -1,  1921,    -1,    -1,    -1,    -1,    -1,
     320,    -1,    -1,   323,    -1,    -1,    -1,    -1,   150,  1134,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   337,    -1,    -1,
      -1,   341,    -1,    -1,    -1,    -1,  1151,  1152,    -1,    -1,
      -1,    -1,   102,    -1,    -1,  1247,    -1,    -1,    -1,    -1,
    1700,    -1,    -1,    -1,  1256,    -1,  1171,    -1,    -1,    -1,
      -1,    -1,    -1,  1178,    -1,    -1,    -1,  1182,    -1,    -1,
      -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,  1194,
      -1,    -1,    -1,    -1,    -1,   145,    -1,    -1,    -1,    -1,
    1205,    -1,    -1,    -1,  1209,    -1,    -1,    -1,    -1,    -1,
    1750,    -1,   162,    -1,    -1,   101,    -1,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,    -1,    -1,    -1,
       1,    -1,    -1,     4,   434,    -1,    -1,    -1,   438,    -1,
      -1,    -1,   192,   129,    -1,   101,  1251,  1252,   104,   105,
     106,   107,   108,   109,   110,   111,   112,    -1,    -1,    -1,
    1265,  1266,    -1,   149,   150,  1270,  1271,   153,    -1,    -1,
      -1,    -1,    -1,    -1,   160,   161,    -1,    -1,  1671,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    58,    -1,    -1,
      -1,    -1,    -1,  1298,  1299,    -1,    -1,    -1,  1838,    -1,
      -1,   157,    -1,  1308,  1309,  1310,  1311,  1312,  1313,    -1,
      -1,    82,    -1,    -1,  1319,    -1,    -1,   267,   101,   519,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      99,   102,   101,   283,   103,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,    -1,   129,  1887,  1888,    -1,
     550,    -1,    -1,    -1,    -1,   555,    -1,    -1,    -1,   559,
      -1,   561,    -1,  1368,    -1,    -1,   149,   150,   139,  1762,
     320,    -1,   572,   323,   145,    -1,   147,   160,   161,    -1,
     149,  1921,  1387,   152,   153,    -1,    -1,   337,   588,   589,
      -1,   341,    -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   603,  1496,  1410,    -1,    -1,   179,    -1,
      -1,    -1,   612,    -1,    -1,    -1,    -1,   617,    -1,    -1,
     191,    -1,    -1,    -1,   101,   625,    70,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1839,    -1,    -1,    -1,
      -1,    -1,   129,    -1,    -1,    -1,    -1,   101,    -1,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,    -1,
      -1,   242,   149,   150,   245,    -1,    -1,  1482,  1483,   250,
      -1,    -1,    -1,   160,   161,   129,    -1,    -1,   438,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1502,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   149,   150,    -1,    -1,    -1,
      -1,   711,   283,  1518,    -1,    -1,   160,   161,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1530,    -1,  1532,   728,    -1,
      -1,   302,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1632,  1633,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1555,    -1,   752,    -1,    -1,    -1,    -1,   757,    -1,    -1,
      -1,  1566,    -1,    -1,  1569,    -1,    -1,    -1,    -1,   519,
      -1,    -1,  1577,    -1,   774,   775,   132,   133,   134,   135,
     136,   137,   138,   139,   140,   141,   142,  1592,    -1,    -1,
     146,    -1,    -1,   793,    -1,    -1,    -1,    -1,    -1,    -1,
     550,    -1,  1607,   374,    -1,    -1,    -1,  1612,  1613,   559,
      -1,   561,  1617,  1618,    -1,    -1,    -1,   173,    -1,    -1,
     101,   821,   572,   104,   105,   106,   107,   108,   109,   110,
     111,   112,    -1,    -1,    -1,    -1,    -1,   408,   588,   589,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,  1741,
      -1,    -1,   852,    -1,    -1,    -1,    -1,    -1,    -1,   859,
      -1,    -1,   612,   434,   864,    -1,  1671,  1672,    -1,   150,
      -1,    -1,   153,   873,    -1,   625,   876,    -1,   878,    -1,
      -1,   101,    -1,   883,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   101,    -1,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,    58,    -1,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,  1808,    -1,    -1,    -1,
    1812,    -1,    -1,    -1,    -1,    -1,   926,    -1,    -1,   149,
     150,    -1,    -1,    -1,    -1,    -1,   156,    -1,    -1,    -1,
     160,   161,   513,   514,    -1,   153,  1838,    -1,    -1,   102,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1762,    -1,    -1,
      -1,   711,    -1,    -1,    -1,    -1,    -1,  1772,    -1,    -1,
      -1,    -1,   101,    -1,  1779,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   555,    -1,    -1,   987,   559,    -1,
     561,    -1,   145,    -1,    -1,  1887,  1888,    -1,    -1,  1804,
      -1,    -1,   752,    -1,  1004,  1005,    -1,    -1,    -1,   162,
      -1,    -1,    -1,    -1,  1819,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   152,   774,   775,    -1,    -1,    -1,  1921,
    1835,  1836,    -1,    -1,  1839,    -1,    -1,    -1,    -1,   192,
      -1,    -1,    -1,   793,   101,    -1,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    -1,  1057,    -1,    -1,
      -1,    -1,   633,    -1,    -1,    -1,  1871,    -1,    -1,    -1,
      -1,   821,   129,    -1,    -1,   646,    -1,   648,   649,    -1,
     651,    -1,    -1,   654,    -1,    -1,   657,   658,   659,    -1,
      -1,    -1,   149,   150,    -1,    -1,   153,  1097,    -1,    -1,
    1905,    -1,   852,   160,   161,  1105,    -1,   101,    -1,   859,
     104,   105,   106,   107,   108,   109,   110,   111,   112,    -1,
      -1,    -1,    -1,   873,    -1,    -1,   876,    -1,   878,    -1,
     283,    -1,   101,    -1,  1134,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1152,    -1,    -1,    -1,    -1,    -1,   152,    -1,
     129,    -1,    -1,    -1,   735,    -1,    -1,   320,    -1,    -1,
     323,    -1,    -1,    -1,    -1,    -1,   926,    -1,  1178,    -1,
     149,   150,    -1,    -1,   337,    -1,    -1,  1187,   341,    -1,
      -1,   160,   161,    -1,    -1,    -1,    -1,    -1,    -1,    55,
      56,    -1,    -1,    -1,    -1,  1205,    -1,    -1,    -1,    -1,
     101,    -1,   374,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   101,    -1,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    90,   101,    -1,   987,   104,   105,
     106,   107,   108,   109,   110,   111,   112,    -1,    -1,    -1,
      -1,  1251,  1252,    -1,  1004,  1005,    -1,    -1,   149,    -1,
      -1,    -1,    -1,    48,    -1,  1265,  1266,    52,    -1,    54,
    1270,  1271,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   852,    -1,   149,   140,   438,    71,   143,    -1,    -1,
      -1,    -1,    -1,   864,   865,    -1,    -1,    -1,  1298,  1299,
     156,    -1,    -1,    -1,    -1,    -1,    -1,  1057,  1308,  1309,
    1310,  1311,    -1,    98,    99,    -1,   101,    -1,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,   117,   118,   119,    -1,   121,   122,    -1,    -1,
      -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,   513,   514,    -1,   210,  1105,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   149,    -1,   519,   152,   153,    -1,
      -1,    -1,    -1,    -1,   159,   160,   161,   162,   163,   164,
     165,    -1,    -1,    -1,  1134,    -1,    -1,  1387,   173,   174,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   550,    -1,   255,
      -1,    -1,  1152,    -1,    -1,    -1,   559,    -1,   561,   265,
    1410,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   572,
     276,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1178,    -1,
      -1,    -1,    -1,    -1,    -1,   588,   589,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   303,    -1,    -1,
      -1,    -1,    -1,   309,   310,  1205,    -1,    -1,   314,   612,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   625,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1482,  1483,   646,    -1,    -1,    -1,    -1,   651,
     346,    -1,   654,    -1,    -1,   351,    -1,    -1,   354,    -1,
      -1,  1251,  1252,    -1,    -1,    -1,    -1,    -1,  1508,    -1,
      -1,   673,    -1,    -1,    -1,  1265,  1266,    -1,  1518,    -1,
    1270,  1271,    -1,    -1,    -1,    -1,  1097,    -1,    -1,    -1,
    1530,    -1,  1532,    -1,  1105,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   707,    -1,    -1,  1298,  1299,
      -1,    -1,    -1,    -1,    -1,  1555,    -1,    -1,   711,    -1,
      -1,    -1,    -1,  1134,    -1,    -1,  1566,    -1,    -1,  1569,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1577,    -1,    -1,
      -1,  1152,    -1,    -1,    -1,   441,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   453,   454,   752,
    1171,    -1,    -1,    -1,    -1,    -1,    -1,  1607,    -1,    -1,
      -1,    -1,  1612,  1613,    -1,    -1,    -1,  1617,  1618,    -1,
       1,   774,   775,     4,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1633,    -1,    -1,    -1,  1387,  1209,    -1,
     793,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   821,    -1,
      -1,    -1,  1672,    -1,    -1,    -1,    -1,    58,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   852,
      -1,    82,    -1,    -1,    -1,    -1,   859,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     873,   102,    -1,   876,    -1,   878,    -1,   583,    -1,    -1,
      -1,    -1,  1482,  1483,    -1,    -1,    -1,  1308,  1309,  1310,
    1311,  1312,  1313,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   139,    -1,
      -1,    -1,    -1,    -1,   145,    -1,   147,    -1,  1518,    -1,
      -1,    -1,  1772,   926,    -1,   631,    -1,    -1,    -1,  1779,
    1530,    -1,  1532,    -1,    -1,    -1,    -1,    -1,    -1,     1,
      -1,    -1,     4,    -1,    -1,    -1,    -1,    -1,   179,    -1,
      -1,    -1,    -1,    -1,  1804,  1555,    -1,    -1,    -1,    -1,
     191,    -1,    -1,    -1,    -1,    -1,  1566,    -1,    -1,  1819,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1577,    -1,    -1,
      -1,    -1,    -1,    -1,   987,  1835,  1836,    -1,    -1,  1410,
      -1,    -1,    -1,    -1,    -1,    -1,    58,    -1,    -1,    -1,
      -1,  1004,  1005,    -1,    -1,    -1,    -1,  1607,    -1,    -1,
      -1,   242,  1612,  1613,   245,    -1,    -1,  1617,  1618,   250,
      82,  1871,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1888,    -1,
     102,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   283,    -1,  1057,  1905,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   769,    -1,    -1,    -1,    -1,    -1,    -1,
     776,   302,  1672,    -1,    -1,    -1,    -1,   139,    -1,    -1,
      -1,  1502,    -1,   145,    -1,   147,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1105,    -1,    -1,    -1,    -1,    -1,    -1,  1530,
      -1,  1532,    -1,    -1,    -1,    -1,    -1,   179,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   191,
      -1,  1134,    -1,    -1,  1555,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   374,    -1,   851,    -1,    -1,  1569,  1152,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   178,
      -1,  1592,  1772,    -1,    -1,  1178,    -1,   408,    -1,  1779,
     242,    -1,    -1,   245,    -1,    -1,    -1,    -1,   250,    -1,
      -1,    -1,    -1,    -1,    -1,   901,    -1,    -1,    -1,    -1,
     906,    -1,  1205,   434,  1804,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1819,
      -1,   283,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1835,  1836,    -1,    -1,    -1,
     302,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1251,  1252,
      -1,  1672,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1265,  1266,    -1,    -1,    -1,  1270,  1271,    -1,
      -1,  1871,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   513,   514,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1298,  1299,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1905,    -1,    -1,    -1,    -1,
      -1,    -1,   374,    -1,    -1,    -1,  1328,    -1,    -1,    -1,
      -1,    -1,    -1,  1335,   555,    -1,    -1,    -1,   559,    -1,
     561,    -1,  1344,    -1,    -1,    -1,  1348,    -1,  1350,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   408,    -1,    -1,    -1,
     359,  1772,    -1,   362,   363,    -1,    -1,    -1,    -1,     0,
      -1,    -1,     3,   372,   373,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   434,    -1,    -1,    -1,    -1,    -1,   387,   388,
      -1,    -1,    -1,    -1,  1387,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1819,   408,
      -1,    -1,   633,    -1,  1110,  1111,  1112,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1835,   646,    -1,   648,   649,    -1,
     651,    -1,    -1,   654,    -1,    -1,   657,   658,   659,   438,
      -1,    -1,    -1,    -1,  1140,    76,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1155,
    1871,   513,   514,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1488,    -1,    -1,  1482,
    1483,    -1,    -1,    -1,    -1,  1191,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   555,   135,    -1,    -1,   559,    -1,   561,
      -1,    -1,    -1,    -1,   735,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1518,    -1,    -1,  1530,    -1,
    1532,    -1,    -1,    -1,    -1,    -1,    -1,  1530,    -1,  1532,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1555,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1555,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1566,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   633,    -1,    -1,  1577,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   646,    -1,   648,   649,   229,   651,
      -1,    -1,   654,    -1,    -1,   657,   658,   659,    -1,    -1,
      -1,    -1,    -1,   244,  1607,    -1,    -1,    -1,    -1,  1612,
    1613,    -1,    -1,   254,  1617,  1618,  1322,    -1,    -1,  1325,
      -1,   852,    -1,   264,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   864,   865,    -1,    -1,   278,   279,    -1,
      -1,    -1,    -1,    -1,   285,   286,    -1,    -1,    -1,    -1,
      -1,  1663,    -1,    -1,    -1,  1667,    -1,    -1,    -1,    -1,
     301,    -1,  1674,    -1,    -1,    -1,    -1,    -1,    -1,  1672,
      -1,    -1,    -1,   735,    -1,    -1,    -1,    -1,   319,   688,
     689,   690,   691,   692,   693,   694,   695,   696,   697,   698,
     699,   700,   701,   702,   703,   704,   705,   706,    -1,    -1,
      -1,    86,    -1,    -1,    -1,    -1,    -1,    92,    93,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   375,    -1,    -1,    -1,    -1,    -1,
      -1,   126,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1769,  1770,   768,
    1772,    -1,    -1,    -1,    -1,   406,    -1,    -1,    -1,  1772,
      -1,    -1,    -1,    -1,    -1,    -1,  1779,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     852,   432,    -1,    -1,    -1,   436,    -1,    -1,    -1,    -1,
      -1,  1804,   864,   865,    -1,    -1,    -1,    -1,    -1,  1515,
      -1,    -1,    -1,    -1,   455,  1827,  1819,    -1,   459,   460,
      -1,    -1,   463,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1835,  1836,    -1,    -1,    -1,   478,   479,   480,
     481,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   497,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   505,    -1,  1097,  1879,  1871,  1881,
      -1,    -1,    -1,    -1,  1105,    -1,    -1,    -1,    -1,  1585,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1901,
      -1,    -1,   533,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1905,  1134,    -1,    -1,    -1,   292,    -1,    -1,
      -1,    -1,    -1,   922,    -1,    -1,    -1,    -1,   927,    -1,
      -1,  1152,    -1,   564,    -1,  1937,  1938,  1939,    -1,   938,
     571,    -1,    -1,    -1,    -1,   576,    -1,    -1,    -1,    -1,
    1171,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   599,   600,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   978,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1209,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1699,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1708,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   660,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1097,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1105,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   437,    -1,   439,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   448,   449,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1134,    -1,    -1,    -1,    -1,  1308,  1309,  1310,
    1311,  1312,  1313,    -1,    -1,    -1,   727,    -1,    -1,    -1,
    1152,    -1,    -1,    -1,    -1,    -1,  1105,    -1,    -1,    -1,
      -1,   742,    -1,    -1,    -1,   746,    -1,    -1,    -1,  1171,
      -1,    -1,    -1,    -1,   755,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   777,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   786,    -1,  1209,    -1,    -1,
      -1,   792,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1178,
      -1,   556,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1410,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   829,    -1,
      -1,    -1,    -1,    -1,    -1,   836,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1212,  1213,  1214,    -1,    -1,    -1,    -1,
    1219,  1220,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   862,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1241,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1308,  1309,  1310,  1311,
    1312,  1313,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1278,
    1279,  1502,    -1,    -1,    -1,   916,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1530,
      -1,  1532,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1555,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1569,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1410,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   997,    -1,    -1,    -1,
    1001,  1592,    -1,    -1,    -1,    -1,    -1,  1008,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1018,    -1,    -1,
      -1,    -1,    -1,   768,  1025,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1034,    -1,  1036,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1068,    -1,    -1,
      -1,  1072,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1502,  1672,    -1,    -1,    -1,  1086,    -1,    -1,  1089,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     845,   846,    -1,    -1,    -1,    -1,    -1,    -1,  1530,    -1,
    1532,   856,   857,   858,    -1,    -1,   861,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1555,    -1,    -1,    -1,    -1,    -1,    47,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1569,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
    1592,    -1,    -1,    -1,    -1,  1176,    -1,    -1,    -1,    -1,
      -1,  1772,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1560,    -1,    -1,    -1,   940,    -1,    -1,    -1,  1200,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   119,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   132,    -1,   134,    -1,  1819,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   984,
      -1,    -1,    -1,    -1,  1835,    -1,    -1,    -1,    -1,    -1,
    1672,    -1,    -1,    -1,    -1,    -1,   164,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   181,    -1,    -1,    -1,    -1,    -1,    -1,
    1871,    -1,    -1,    -1,    -1,    -1,  1031,    -1,    -1,  1290,
      -1,    -1,    -1,  1294,    -1,  1040,  1041,  1042,  1043,    -1,
      -1,    -1,    -1,  1048,  1049,    -1,    -1,    -1,    -1,    -1,
      -1,   219,    -1,  1058,    -1,   223,    -1,    -1,   226,   227,
      -1,    -1,   230,  1324,    -1,   233,   234,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1079,    -1,  1081,  1338,  1339,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1772,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1375,    -1,    -1,  1378,    -1,    -1,
      -1,    -1,    -1,  1752,    -1,    -1,    -1,   295,    -1,  1134,
     298,    -1,  1393,    -1,    -1,    -1,    -1,  1819,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     318,    -1,    -1,  1835,    -1,  1160,    -1,  1786,    -1,    -1,
      -1,  1166,    -1,  1168,  1169,   333,    -1,    -1,    -1,    -1,
      -1,    -1,  1177,    -1,  1179,    -1,  1181,    -1,  1183,    -1,
      -1,    -1,  1443,  1188,    -1,    -1,    -1,    98,    -1,  1871,
      -1,  1452,    -1,    -1,    -1,  1456,    -1,    -1,   109,    -1,
     111,    -1,   113,    -1,    -1,    -1,    -1,    -1,    -1,  1470,
    1471,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1867,  1868,
     151,  1246,   153,   154,    -1,    -1,    -1,    -1,  1253,  1254,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   427,
      -1,    -1,    -1,    -1,    -1,  1894,    -1,    -1,    -1,    -1,
      -1,    -1,  1277,    -1,    -1,    -1,    47,    -1,    -1,  1284,
      -1,    -1,  1287,    -1,   195,    -1,    -1,    -1,    -1,    -1,
    1919,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1570,
    1571,    -1,  1317,    -1,   482,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   494,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1966,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   258,   119,   260,
     261,    -1,    -1,  1358,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   132,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   288,    -1,    -1,
      -1,    -1,    -1,   294,    -1,    -1,  1391,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1399,    -1,  1401,    -1,    -1,    -1,
     568,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   322,    -1,    -1,    -1,    -1,    -1,   328,    -1,   330,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1688,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   607,
     608,    -1,  1447,  1448,    -1,    -1,    -1,    -1,    -1,  1710,
      -1,    -1,   620,    -1,    -1,   226,   227,  1462,  1463,   230,
    1465,    -1,   233,   234,    -1,    -1,    -1,  1728,    -1,  1474,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1484,
    1485,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1754,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   418,    -1,    -1,
      -1,    -1,    -1,    -1,  1775,    -1,    -1,  1778,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   444,    -1,   446,   447,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   318,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   333,   731,   732,    -1,    -1,    -1,    -1,   737,
      -1,    -1,    -1,  1578,  1579,    -1,    -1,    -1,    -1,    -1,
     491,    -1,    -1,  1588,    -1,    -1,    -1,    -1,    -1,    -1,
     758,    -1,    -1,   761,   762,    -1,   764,    -1,   766,   767,
     511,  1862,    -1,    -1,    -1,   516,    -1,   518,    -1,    -1,
      -1,    -1,    -1,   162,    -1,    -1,    -1,    -1,  1623,  1624,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   538,    -1,   540,
     541,    -1,    -1,    -1,    -1,    -1,    -1,   805,    -1,    -1,
      -1,   809,   191,   192,    -1,    -1,    -1,   558,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   427,    -1,    -1,   570,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   223,    -1,    -1,    -1,    -1,    -1,
     591,   230,   593,   594,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1702,    -1,    -1,
      -1,    -1,    -1,    82,   615,   616,    -1,    -1,    -1,    -1,
      -1,   622,    -1,    -1,    -1,  1720,   884,    -1,  1723,  1724,
      -1,    -1,    -1,   494,    -1,  1730,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   298,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   147,    -1,
      -1,   320,   321,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   162,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   341,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     179,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   192,    -1,    -1,    -1,    -1,    -1,  1824,
      -1,    -1,    -1,    -1,    -1,    -1,  1831,    -1,    -1,    -1,
      -1,    -1,  1837,    -1,    -1,    -1,   607,   608,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1014,    -1,    -1,   620,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   411,    -1,    -1,    -1,   245,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   427,   428,
    1885,   430,   431,    -1,    -1,    -1,    -1,    -1,    -1,   438,
      -1,    -1,    -1,   442,  1062,    -1,    -1,  1065,    -1,    -1,
      -1,    -1,    -1,    -1,  1909,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1917,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   302,    -1,    -1,    -1,  1932,    -1,    -1,
      -1,    -1,    -1,   844,   483,    -1,    -1,    -1,   487,    -1,
      -1,   320,    -1,    -1,   855,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     731,   732,    -1,    -1,   875,    -1,   737,    -1,    -1,    -1,
     519,    -1,    -1,    -1,    -1,   886,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   895,    -1,    -1,   758,    -1,    -1,
     761,   762,    -1,   764,    -1,   766,   767,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1184,    -1,    -1,    -1,
     569,    -1,    -1,   572,  1192,  1193,    -1,    -1,    -1,   408,
      -1,    -1,    -1,    -1,   805,    -1,    -1,    -1,   809,   588,
     589,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     599,    -1,    -1,    -1,   603,    -1,    -1,    -1,    -1,   438,
      -1,   610,    -1,   612,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1247,
     991,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1256,    -1,
      -1,  1259,    -1,  1261,  1262,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1015,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   884,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1301,   513,   514,    -1,    -1,    -1,    -1,
     519,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   711,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   727,   728,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   737,   738,
      -1,   740,   741,   572,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1369,    -1,   752,    -1,    -1,   755,    82,   757,   758,
     589,    -1,    -1,    -1,    -1,   764,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    98,    -1,   774,   775,    -1,    -1,    -1,
      -1,    -1,    -1,   612,    -1,    -1,    -1,    -1,    -1,    -1,
    1151,    -1,    -1,  1014,   793,    -1,    -1,    -1,   797,    -1,
      -1,    -1,   801,    -1,   633,    -1,   805,   806,    -1,    -1,
     809,   810,    -1,    -1,    -1,    -1,    -1,    -1,   817,    -1,
      -1,  1182,   147,    -1,    -1,    -1,   151,  1188,    -1,    -1,
      -1,    -1,    -1,  1451,    -1,    -1,    -1,   162,    -1,    -1,
      -1,  1062,    -1,    -1,  1065,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   179,    -1,    -1,    -1,    -1,    -1,
     859,   860,  1480,    -1,    -1,    -1,    -1,   192,    -1,    -1,
     195,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   711,    -1,    -1,    -1,    -1,   886,  1506,    -1,
      -1,    -1,    -1,    -1,  1512,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   735,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     245,    -1,    -1,   752,    -1,    -1,    -1,   926,    -1,    -1,
      -1,    -1,    -1,   258,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   774,   775,  1565,    -1,    -1,
      -1,    -1,    -1,    -1,   179,    -1,    -1,    -1,  1319,    -1,
      -1,    -1,    -1,  1184,   793,    -1,    -1,   192,    -1,   294,
      -1,  1192,  1193,    -1,    -1,    -1,    -1,   302,    -1,    -1,
     205,    -1,   207,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   991,    -1,    -1,   320,    -1,   322,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1368,    -1,  1008,
    1009,    -1,    -1,    -1,    -1,    -1,  1015,    -1,    -1,    -1,
      -1,  1639,  1640,    -1,    -1,    -1,  1247,    -1,    -1,  1647,
     859,    -1,    -1,  1651,    -1,  1256,   865,    -1,  1259,    -1,
    1261,  1262,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   374,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1057,    -1,
      -1,    -1,    -1,  1062,  1063,    -1,  1065,  1066,   293,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1301,    -1,    -1,   408,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   926,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   438,    -1,    -1,    -1,    -1,    -1,   444,
      -1,    -1,    -1,    -1,    -1,  1743,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,  1369,    -1,
      19,    -1,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,  1178,
      49,    50,    51,    -1,    53,  1184,  1185,    -1,   513,   514,
    1808,    -1,    -1,    -1,   519,    -1,    -1,    -1,    -1,    -1,
      -1,    70,    -1,    -1,    -1,    -1,  1205,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1057,    -1,
    1451,    -1,    -1,    -1,    -1,   104,   105,    -1,    -1,   464,
      -1,    -1,    -1,    -1,    -1,   470,    -1,   572,  1247,  1248,
     475,    -1,    -1,    -1,    -1,    -1,    -1,  1256,  1257,  1480,
    1259,    -1,    -1,    -1,   589,    -1,   591,    -1,    -1,  1887,
      -1,  1270,  1271,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   152,    -1,    -1,    -1,   612,    -1,  1650,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   633,    -1,
    1671,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   646,    -1,   648,   649,    -1,   651,    -1,    -1,   654,
      -1,    -1,   657,   658,   659,    -1,    -1,   562,    -1,    -1,
      -1,    -1,  1171,    -1,  1565,    -1,    -1,    -1,    -1,  1178,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   589,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1205,   602,    -1,    -1,
    1741,    -1,    -1,    -1,    -1,    -1,   711,    -1,  1387,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1762,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     735,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1639,  1640,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   752,   653,    -1,
    1651,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1270,  1271,    -1,    -1,    -1,   671,   672,    -1,   774,
     775,    -1,    -1,    -1,    -1,    -1,   681,    -1,   683,   684,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   793,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1839,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   711,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1496,    -1,   724,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     735,    -1,    -1,  1512,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1743,    -1,   749,    -1,    -1,   752,    -1,    -1,
      -1,    -1,    -1,    -1,   859,    -1,    -1,    -1,    -1,    -1,
     865,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     875,    -1,    -1,    -1,   779,    -1,    -1,   782,  1387,    -1,
    1921,    -1,    -1,    -1,    -1,    -1,    -1,  1566,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1808,    -1,    -1,
      -1,    -1,    -1,   818,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   926,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1617,  1618,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1632,  1633,    -1,    -1,    -1,    -1,    -1,
     865,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1648,
     875,   876,    -1,    -1,    -1,    -1,    -1,    -1,   883,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1502,    -1,    -1,    -1,    -1,    -1,   904,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   913,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   926,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   934,
      -1,    -1,    -1,    -1,    -1,    -1,   941,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1057,    -1,    -1,    -1,    -1,  1566,    -1,    -1,
      -1,    -1,  1741,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1749,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     985,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     5,
      -1,    -1,  1097,    -1,    -1,    -1,    12,    13,    14,    15,
      16,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1617,  1618,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1804,    -1,    -1,    -1,  1808,
    1809,    -1,    48,  1812,    -1,    -1,    52,    -1,    54,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1151,  1052,    -1,  1054,
      -1,  1056,    -1,    -1,    70,    71,    -1,    -1,    -1,  1838,
      -1,    -1,    -1,    -1,    -1,    -1,  1171,    -1,    -1,    -1,
      -1,    -1,    -1,  1178,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    98,    99,    -1,   101,    -1,   103,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
    1205,   117,   118,   119,  1209,   121,   122,    -1,  1887,  1888,
      -1,    -1,    -1,   129,    -1,    -1,    -1,  1122,  1123,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   149,    -1,    -1,   152,   153,    -1,    -1,
      -1,    -1,  1921,   159,   160,   161,   162,   163,   164,   165,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1270,  1271,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1182,    -1,    -1,
      -1,    -1,    -1,  1188,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1804,    -1,    -1,    -1,    -1,
    1205,    -1,     5,  1308,  1309,  1310,    -1,  1312,  1313,    12,
      13,    14,    15,    16,  1319,    -1,    -1,  1222,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1235,    -1,    -1,  1238,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    48,    -1,    -1,    -1,    52,
      -1,    54,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1368,    -1,    -1,    -1,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1387,    -1,  1289,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    98,    99,    -1,   101,    -1,
     103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,    -1,   117,   118,   119,    -1,   121,   122,
      -1,    -1,  1327,    -1,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1346,    -1,    -1,  1349,    -1,   149,    -1,    -1,   152,
     153,    -1,    -1,    -1,    -1,    -1,   159,   160,   161,   162,
     163,   164,   165,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1387,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1397,  1398,    -1,    -1,    -1,  1502,    -1,    -1,
      -1,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    -1,  1422,    19,  1424,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    -1,    49,    50,
      51,    -1,    53,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1566,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1592,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1502,    -1,    -1,
     101,    -1,  1507,   104,   105,   106,   107,   108,   109,   110,
     111,   112,  1617,  1618,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1551,    -1,   149,   150,
      -1,   152,   153,    -1,    -1,    -1,   157,    -1,    -1,   160,
     161,    -1,    -1,    -1,    -1,    -1,  1671,    -1,    -1,    -1,
      -1,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,  1590,    -1,    19,  1593,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    -1,    49,    50,    51,
      -1,    53,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1762,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   104,   105,  1779,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,  1804,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     152,   153,    -1,    -1,    -1,    -1,    -1,    -1,   160,   161,
      -1,  1836,    -1,    -1,  1839,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,    19,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    -1,
      -1,    48,  1787,    50,    51,    52,    -1,    54,    55,    56,
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
     115,   116,   117,   118,   119,    -1,   121,   122,    -1,    -1,
      -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   149,    -1,    -1,   152,   153,    -1,
      -1,    -1,   157,    -1,   159,   160,   161,   162,   163,   164,
     165,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   174,
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
      -1,    -1,    -1,    -1,    -1,     3,    -1,   148,    -1,    -1,
      -1,   152,   153,    -1,    12,    13,    14,    15,    16,   160,
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
      -1,   101,    -1,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,
     150,    -1,   152,   153,    -1,    -1,    -1,   157,    -1,    -1,
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
     112,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    17,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,   150,    48,
     152,   153,    -1,    52,    -1,    54,    -1,    -1,   160,   161,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      69,    -1,    71,    -1,    -1,    74,    75,    76,    77,    78,
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
      71,    72,    -1,    74,    -1,    -1,    77,    78,    79,    80,
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
      -1,    12,    13,    14,    15,    16,    -1,    -1,    19,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,   152,    49,    50,
      51,    -1,    53,    -1,    -1,    48,    -1,    -1,    -1,    52,
      -1,    54,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   104,   105,    98,    99,    -1,   101,    -1,
     103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,    -1,   117,   118,   119,    48,   121,   122,
      -1,    52,    -1,    54,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      71,   152,    -1,    -1,    -1,    -1,   149,   150,    -1,   152,
     153,    -1,    -1,    -1,   157,    -1,   159,   160,   161,   162,
     163,   164,   165,    -1,    -1,    -1,    -1,    98,    99,    -1,
     101,    -1,   103,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,   117,   118,   119,    48,
     121,   122,    -1,    52,    -1,    54,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,   149,   150,
      -1,   152,   153,    -1,    -1,    -1,   157,    -1,   159,   160,
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
     332,   337,   455,   151,   155,   155,   174,   149,   150,    59,
      60,   174,   225,   277,   397,   149,    17,   223,   149,   149,
     174,   357,   174,   357,   160,   357,   157,   222,   149,   149,
     149,   225,   214,   215,   215,    13,   264,    72,   231,   174,
     177,   227,    76,   174,   357,    89,   250,   356,   295,   156,
     276,   174,   154,   154,   177,   155,   389,   398,   177,   174,
     177,   174,   177,   151,   359,   373,   373,   176,   177,   177,
     177,   214,   177,   149,   401,   444,   439,   294,     5,   160,
     177,   214,   345,   401,   401,   317,   346,   461,   148,   148,
     176,   151,   180,    76,   185,   186,   358,   198,   198,   198,
     198,   198,   157,   362,   155,   148,   194,   153,   192,   194,
     194,   154,   155,   120,   152,   190,   154,   220,   212,   174,
     154,   461,   177,   149,   401,   438,   439,   350,   350,   177,
     177,   151,   149,   401,   438,   439,   149,   401,   444,   407,
     401,   401,   350,   350,   154,   349,   352,   352,   353,   151,
     155,   155,   151,   177,   213,   213,   154,   154,   177,   177,
     151,   214,   176,   176,   350,   350,   360,   401,   155,   151,
     148,   389,   148,   148,   148,   148,   292,   330,   338,   455,
     292,   337,   149,   326,   174,   174,   149,   156,   196,   333,
     334,   340,   407,   408,   421,   155,   174,   357,   176,   357,
     188,   189,   221,   174,   225,   174,   225,   221,    78,   151,
     176,   151,   176,   174,   174,   221,   174,   362,   174,   221,
     220,   221,   108,   109,   110,   111,   112,   256,   258,   259,
     174,    95,   174,    82,   149,   149,   177,   148,   174,   174,
     149,   223,   225,   401,   174,   151,   176,   148,   148,   176,
     155,   155,   154,   154,   154,   177,   151,   176,   214,   214,
     177,   154,   177,   461,   343,   157,   346,   148,   381,   151,
     156,   151,   155,   156,   362,   461,   220,   118,   191,   192,
     153,   192,   153,   192,   154,   148,   151,   176,   177,   177,
     151,   151,   176,   176,   177,   177,   177,   176,   176,   154,
     177,   151,   401,   350,   350,   177,   177,   221,   148,   326,
     326,   326,   149,   196,   335,   336,   438,   446,   447,   448,
     449,   174,   155,   174,   333,   174,   376,   402,   407,   214,
     295,   155,   174,   339,   340,   339,   357,   131,   354,   355,
     151,   174,   151,   149,   223,   221,   232,   277,   279,   282,
     288,   295,   299,   223,   173,   174,   221,   241,   242,   277,
     174,   461,   151,   151,   151,   225,   258,   259,   149,   214,
     149,   182,   232,   198,   251,   107,   223,   401,   382,   176,
     176,   154,   350,   177,   177,   154,   154,   148,   157,   345,
     177,   214,   186,   214,   461,   148,   154,   154,   191,   191,
     350,   151,   151,   350,   350,   151,   151,   154,   155,   131,
     349,   131,   154,   177,   177,   151,   151,   154,   447,   448,
     449,   295,   446,   155,   174,   401,   401,   174,   151,   407,
     401,   223,    75,    76,   157,   235,   236,   237,   151,   221,
     151,   221,   295,   221,   222,   143,   144,   145,   165,   174,
     243,   151,   156,   222,   148,   157,   237,   223,   149,   176,
     174,   182,   151,   156,   151,   151,   155,   156,   249,   253,
     357,   398,   177,   154,   154,   345,   461,   148,   148,   154,
     154,   177,   177,   177,   176,   177,   151,   151,   151,   151,
     151,   446,   401,   334,   213,   233,   234,   399,   156,   176,
     223,   235,   174,   151,   223,   174,   104,   173,   221,   222,
     221,   223,   242,   174,   174,   176,   176,   260,   293,   295,
     455,   156,   174,   153,   182,   265,   266,   267,   223,   198,
     188,    73,   106,   250,   252,   151,   461,   148,   151,   151,
     151,   352,   149,   401,   438,   439,   336,   131,   155,   156,
     270,   271,   277,   174,   177,   222,   221,   144,   165,   243,
     174,   165,   177,   222,   270,   260,   177,   149,   196,   398,
     446,   180,   156,   101,   149,   151,   156,   155,    73,   151,
     223,   149,   223,   223,   148,   176,   213,   233,   236,   238,
     239,   277,   150,   150,   221,   222,   221,   238,   177,   174,
     257,   295,   265,   154,   213,   174,   265,   267,   223,   221,
     107,   107,   350,   223,   228,   177,   236,   165,   165,   165,
     177,   257,   212,   151,   156,   182,   151,   151,   156,   151,
     253,    73,   248,   177,   223,   148,   228,   221,   150,   221,
     221,   148,   151,   225,   182,   268,   149,   174,   268,   223,
      73,   151,   225,   155,   156,   213,   151,   223,   182,   180,
     269,   151,   174,   151,   155,   174,   180
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
     391,   390,   392,   390,   393,   390,   394,   390,   395,   395,
     395,   396,   396,   397,   397,   397,   397,   397,   397,   397,
     397,   397,   397,   398,   398,   398,   399,   400,   400,   401,
     401,   402,   402,   403,   404,   404,   405,   405,   405,   406,
     406,   406,   406,   406,   406,   407,   407,   408,   408,   408,
     408,   409,   409,   409,   409,   410,   410,   410,   410,   410,
     410,   410,   411,   411,   411,   411,   412,   412,   412,   413,
     413,   413,   413,   413,   414,   414,   414,   414,   415,   415,
     415,   415,   415,   415,   416,   416,   416,   417,   417,   417,
     417,   417,   418,   418,   418,   418,   419,   419,   419,   419,
     419,   419,   420,   420,   421,   421,   421,   421,   422,   422,
     422,   422,   423,   423,   423,   423,   423,   423,   423,   424,
     424,   424,   424,   424,   425,   425,   425,   425,   425,   426,
     426,   426,   427,   427,   427,   427,   428,   428,   428,   429,
     429,   429,   429,   429,   430,   430,   431,   431,   431,   432,
     432,   433,   433,   434,   434,   434,   435,   435,   435,   435,
     435,   436,   436,   436,   436,   437,   437,   437,   438,   438,
     438,   438,   439,   439,   439,   439,   440,   440,   440,   440,
     441,   441,   441,   441,   441,   442,   442,   442,   442,   443,
     443,   443,   444,   444,   444,   445,   445,   445,   445,   445,
     445,   446,   446,   446,   447,   447,   447,   447,   447,   448,
     448,   448,   448,   449,   449,   450,   450,   450,   451,   451,
     452,   452,   452,   452,   452,   452,   453,   453,   453,   453,
     453,   453,   453,   453,   453,   453,   454,   454,   454,   454,
     455,   455,   455,   456,   456,   457,   457,   457,   457,   457,
     457,   458,   458,   458,   458,   458,   458,   459,   459,   459,
     460,   460,   461,   461,   462,   462
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
       1,     2,     2,     4,     3,     5,    10,     5,    10,     5,
       7,     1,     1,     1,     2,     1,     3,     1,     1,     3,
       3,     2,     1,     2,     2,     0,     1,     2,     3,     7,
       4,     7,     6,     7,     4,     1,     3,     4,     5,     4,
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
       0,     8,     0,     7,     0,     7,     0,     8,     1,     2,
       3,     0,     4,     3,     4,     4,     4,     4,     5,     5,
       5,     5,     6,     1,     1,     1,     3,     0,     5,     0,
       1,     1,     2,     6,     1,     3,     0,     1,     4,     1,
       1,     1,     1,     1,     1,     1,     3,     2,     1,     2,
       2,     2,     3,     4,     5,     2,     4,     5,     4,     5,
       3,     4,     8,     9,     3,     4,     2,     1,     2,     6,
       8,     9,     3,     4,     2,     3,     4,     5,     4,     5,
       4,     5,     3,     4,     1,     1,     1,     4,     8,     9,
       3,     4,     2,     3,     3,     4,     4,     5,     4,     5,
       3,     4,     1,     3,     2,     1,     2,     2,     2,     3,
       4,     5,     2,     4,     5,     4,     5,     3,     4,     6,
       8,     9,     3,     4,     2,     4,     1,     2,     2,     2,
       3,     4,     2,     4,     4,     3,     6,     8,     3,     2,
       4,     1,     2,     2,     1,     1,     2,     3,     4,     2,
       4,     6,     8,     1,     2,     2,     1,     2,     2,     3,
       4,     1,     4,     4,     3,     5,     8,     3,     2,     3,
       7,     1,     5,     5,     6,     6,     1,     3,     2,     2,
       1,     2,     2,     3,     4,     1,     4,     4,     3,     5,
       8,     3,     1,     2,     1,     2,     6,     5,     6,     7,
       7,     1,     2,     2,     1,     2,     2,     3,     4,     1,
       4,     4,     3,     8,     3,     1,     1,     2,     1,     1,
       2,     3,     2,     3,     2,     3,     3,     2,     4,     3,
       2,     3,     2,     4,     3,     2,     6,     6,     6,     7,
       1,     2,     1,     1,     1,     2,     3,     2,     3,     2,
       3,     3,     4,     2,     3,     4,     2,     5,     6,     7,
       6,     6,     0,     1,     0,     2
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
#line 6783 "Parser/parser.cc"
    break;

  case 3:
#line 533 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 6789 "Parser/parser.cc"
    break;

  case 4:
#line 540 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantInteger( *(yyvsp[0].tok) ) ); }
#line 6795 "Parser/parser.cc"
    break;

  case 5:
#line 541 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 6801 "Parser/parser.cc"
    break;

  case 6:
#line 542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 6807 "Parser/parser.cc"
    break;

  case 7:
#line 543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 6813 "Parser/parser.cc"
    break;

  case 8:
#line 544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantChar( *(yyvsp[0].tok) ) ); }
#line 6819 "Parser/parser.cc"
    break;

  case 19:
#line 565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 6825 "Parser/parser.cc"
    break;

  case 20:
#line 569 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.constant) = build_constantStr( *(yyvsp[0].str) ); }
#line 6831 "Parser/parser.cc"
    break;

  case 21:
#line 573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 6837 "Parser/parser.cc"
    break;

  case 22:
#line 575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 6847 "Parser/parser.cc"
    break;

  case 23:
#line 586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 6853 "Parser/parser.cc"
    break;

  case 24:
#line 588 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 6859 "Parser/parser.cc"
    break;

  case 25:
#line 592 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_dimensionref( (yyvsp[0].tok) ) ); }
#line 6865 "Parser/parser.cc"
    break;

  case 27:
#line 595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 6871 "Parser/parser.cc"
    break;

  case 28:
#line 597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new StmtExpr( dynamic_cast<CompoundStmt *>(maybeMoveBuild<Statement>((yyvsp[-1].sn)) ) ) ); }
#line 6877 "Parser/parser.cc"
    break;

  case 29:
#line 599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.en) = nullptr; }
#line 6883 "Parser/parser.cc"
    break;

  case 30:
#line 601 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.en) = nullptr; }
#line 6889 "Parser/parser.cc"
    break;

  case 31:
#line 603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild<Expression>( (yyvsp[-3].en) );
			(yyval.en) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 6899 "Parser/parser.cc"
    break;

  case 33:
#line 617 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// steal the association node from the singleton and delete the wrapper
			(yyvsp[-2].genexpr)->associations.splice((yyvsp[-2].genexpr)->associations.end(), (yyvsp[0].genexpr)->associations);
			delete (yyvsp[0].genexpr);
			(yyval.genexpr) = (yyvsp[-2].genexpr);
		}
#line 6910 "Parser/parser.cc"
    break;

  case 34:
#line 627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuildType((yyvsp[-2].decl)), maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } );
		}
#line 6919 "Parser/parser.cc"
    break;

  case 35:
#line 632 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } ); }
#line 6925 "Parser/parser.cc"
    break;

  case 37:
#line 642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-5].en), new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) ) ) )) ) ); }
#line 6931 "Parser/parser.cc"
    break;

  case 38:
#line 648 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 6937 "Parser/parser.cc"
    break;

  case 39:
#line 650 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.en) = new ExpressionNode( new ConstructorExpr( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) ) );
		}
#line 6947 "Parser/parser.cc"
    break;

  case 40:
#line 656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 6953 "Parser/parser.cc"
    break;

  case 41:
#line 658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 6959 "Parser/parser.cc"
    break;

  case 42:
#line 660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 6965 "Parser/parser.cc"
    break;

  case 43:
#line 662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), new ExpressionNode( (yyvsp[-2].constant) ) ) ); }
#line 6971 "Parser/parser.cc"
    break;

  case 44:
#line 664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 6977 "Parser/parser.cc"
    break;

  case 45:
#line 666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 6983 "Parser/parser.cc"
    break;

  case 46:
#line 668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-1].en), build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) ) ) ); }
#line 6989 "Parser/parser.cc"
    break;

  case 47:
#line 670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 6995 "Parser/parser.cc"
    break;

  case 48:
#line 672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[0].aggKey), (yyvsp[-2].en) ) ); }
#line 7001 "Parser/parser.cc"
    break;

  case 49:
#line 674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7007 "Parser/parser.cc"
    break;

  case 50:
#line 676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 7013 "Parser/parser.cc"
    break;

  case 51:
#line 678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7019 "Parser/parser.cc"
    break;

  case 52:
#line 680 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::IncrPost, (yyvsp[-1].en) ) ); }
#line 7025 "Parser/parser.cc"
    break;

  case 53:
#line 682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::DecrPost, (yyvsp[-1].en) ) ); }
#line 7031 "Parser/parser.cc"
    break;

  case 54:
#line 684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].in), true ) ) ); }
#line 7037 "Parser/parser.cc"
    break;

  case 55:
#line 686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].in), true ))->set_maybeConstructed( false ) ) ); }
#line 7043 "Parser/parser.cc"
    break;

  case 56:
#line 688 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) );
		}
#line 7053 "Parser/parser.cc"
    break;

  case 57:
#line 697 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7059 "Parser/parser.cc"
    break;

  case 60:
#line 704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7065 "Parser/parser.cc"
    break;

  case 61:
#line 709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Default parameter for argument is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7071 "Parser/parser.cc"
    break;

  case 64:
#line 716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7077 "Parser/parser.cc"
    break;

  case 66:
#line 722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-1].tok) ) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7083 "Parser/parser.cc"
    break;

  case 67:
#line 724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-3].tok) ) ), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7089 "Parser/parser.cc"
    break;

  case 68:
#line 726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7095 "Parser/parser.cc"
    break;

  case 69:
#line 728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7101 "Parser/parser.cc"
    break;

  case 70:
#line 730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7107 "Parser/parser.cc"
    break;

  case 71:
#line 732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7113 "Parser/parser.cc"
    break;

  case 72:
#line 737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_constantInteger( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 7119 "Parser/parser.cc"
    break;

  case 73:
#line 739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_field_name_FLOATINGconstant( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 7125 "Parser/parser.cc"
    break;

  case 74:
#line 741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_varref( (yyvsp[-1].tok) ), (yyvsp[0].en) ) );
		}
#line 7133 "Parser/parser.cc"
    break;

  case 75:
#line 748 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7139 "Parser/parser.cc"
    break;

  case 76:
#line 750 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Expression * constant = build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) );
			(yyval.en) = (yyvsp[-1].en) != nullptr ? new ExpressionNode( build_fieldSel( (yyvsp[-1].en),  constant ) ) : new ExpressionNode( constant );
		}
#line 7148 "Parser/parser.cc"
    break;

  case 79:
#line 762 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 7154 "Parser/parser.cc"
    break;

  case 80:
#line 764 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en)->set_extension( true ); }
#line 7160 "Parser/parser.cc"
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
#line 7180 "Parser/parser.cc"
    break;

  case 82:
#line 785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_val( (yyvsp[-1].op), (yyvsp[0].en) ) ); }
#line 7186 "Parser/parser.cc"
    break;

  case 83:
#line 787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Incr, (yyvsp[0].en) ) ); }
#line 7192 "Parser/parser.cc"
    break;

  case 84:
#line 789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Decr, (yyvsp[0].en) ) ); }
#line 7198 "Parser/parser.cc"
    break;

  case 85:
#line 791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7204 "Parser/parser.cc"
    break;

  case 86:
#line 793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 7210 "Parser/parser.cc"
    break;

  case 87:
#line 795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7216 "Parser/parser.cc"
    break;

  case 88:
#line 797 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 7222 "Parser/parser.cc"
    break;

  case 89:
#line 799 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_offsetOf( (yyvsp[-3].decl), build_varref( (yyvsp[-1].tok) ) ) ); }
#line 7228 "Parser/parser.cc"
    break;

  case 90:
#line 801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.en) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 7237 "Parser/parser.cc"
    break;

  case 91:
#line 808 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::PointTo; }
#line 7243 "Parser/parser.cc"
    break;

  case 92:
#line 809 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AddressOf; }
#line 7249 "Parser/parser.cc"
    break;

  case 93:
#line 811 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::And; }
#line 7255 "Parser/parser.cc"
    break;

  case 94:
#line 815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::UnPlus; }
#line 7261 "Parser/parser.cc"
    break;

  case 95:
#line 816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::UnMinus; }
#line 7267 "Parser/parser.cc"
    break;

  case 96:
#line 817 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::Neg; }
#line 7273 "Parser/parser.cc"
    break;

  case 97:
#line 818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::BitNeg; }
#line 7279 "Parser/parser.cc"
    break;

  case 99:
#line 824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cast( (yyvsp[-2].decl), (yyvsp[0].en) ) ); }
#line 7285 "Parser/parser.cc"
    break;

  case 100:
#line 826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 7291 "Parser/parser.cc"
    break;

  case 101:
#line 828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 7297 "Parser/parser.cc"
    break;

  case 102:
#line 830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( nullptr ) ) ); }
#line 7303 "Parser/parser.cc"
    break;

  case 103:
#line 832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 7309 "Parser/parser.cc"
    break;

  case 104:
#line 834 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Return cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7315 "Parser/parser.cc"
    break;

  case 105:
#line 836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7321 "Parser/parser.cc"
    break;

  case 106:
#line 838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7327 "Parser/parser.cc"
    break;

  case 114:
#line 858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Exp, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7333 "Parser/parser.cc"
    break;

  case 116:
#line 864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mul, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7339 "Parser/parser.cc"
    break;

  case 117:
#line 866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Div, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7345 "Parser/parser.cc"
    break;

  case 118:
#line 868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mod, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7351 "Parser/parser.cc"
    break;

  case 120:
#line 874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Plus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7357 "Parser/parser.cc"
    break;

  case 121:
#line 876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Minus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7363 "Parser/parser.cc"
    break;

  case 123:
#line 882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7369 "Parser/parser.cc"
    break;

  case 124:
#line 884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::RShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7375 "Parser/parser.cc"
    break;

  case 126:
#line 890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7381 "Parser/parser.cc"
    break;

  case 127:
#line 892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7387 "Parser/parser.cc"
    break;

  case 128:
#line 894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7393 "Parser/parser.cc"
    break;

  case 129:
#line 896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7399 "Parser/parser.cc"
    break;

  case 131:
#line 902 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Eq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7405 "Parser/parser.cc"
    break;

  case 132:
#line 904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Neq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7411 "Parser/parser.cc"
    break;

  case 134:
#line 910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitAnd, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7417 "Parser/parser.cc"
    break;

  case 136:
#line 916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Xor, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7423 "Parser/parser.cc"
    break;

  case 138:
#line 922 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitOr, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7429 "Parser/parser.cc"
    break;

  case 140:
#line 928 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), true ) ); }
#line 7435 "Parser/parser.cc"
    break;

  case 142:
#line 934 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), false ) ); }
#line 7441 "Parser/parser.cc"
    break;

  case 144:
#line 940 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7447 "Parser/parser.cc"
    break;

  case 145:
#line 943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-3].en), (yyvsp[-3].en), (yyvsp[0].en) ) ); }
#line 7453 "Parser/parser.cc"
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
#line 7465 "Parser/parser.cc"
    break;

  case 149:
#line 962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7471 "Parser/parser.cc"
    break;

  case 150:
#line 967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7477 "Parser/parser.cc"
    break;

  case 154:
#line 977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::Assign; }
#line 7483 "Parser/parser.cc"
    break;

  case 155:
#line 978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AtAssn; }
#line 7489 "Parser/parser.cc"
    break;

  case 156:
#line 982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ExpAssn; }
#line 7495 "Parser/parser.cc"
    break;

  case 157:
#line 983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MulAssn; }
#line 7501 "Parser/parser.cc"
    break;

  case 158:
#line 984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::DivAssn; }
#line 7507 "Parser/parser.cc"
    break;

  case 159:
#line 985 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ModAssn; }
#line 7513 "Parser/parser.cc"
    break;

  case 160:
#line 986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::PlusAssn; }
#line 7519 "Parser/parser.cc"
    break;

  case 161:
#line 987 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MinusAssn; }
#line 7525 "Parser/parser.cc"
    break;

  case 162:
#line 988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::LSAssn; }
#line 7531 "Parser/parser.cc"
    break;

  case 163:
#line 989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::RSAssn; }
#line 7537 "Parser/parser.cc"
    break;

  case 164:
#line 990 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AndAssn; }
#line 7543 "Parser/parser.cc"
    break;

  case 165:
#line 991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ERAssn; }
#line 7549 "Parser/parser.cc"
    break;

  case 166:
#line 992 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::OrAssn; }
#line 7555 "Parser/parser.cc"
    break;

  case 167:
#line 1003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)(new ExpressionNode( nullptr ) )->set_last( (yyvsp[-1].en) ) ) ); }
#line 7561 "Parser/parser.cc"
    break;

  case 168:
#line 1005 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-4].en)->set_last( (yyvsp[-1].en) ) ) )); }
#line 7567 "Parser/parser.cc"
    break;

  case 170:
#line 1011 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7573 "Parser/parser.cc"
    break;

  case 171:
#line 1013 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7579 "Parser/parser.cc"
    break;

  case 172:
#line 1015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7585 "Parser/parser.cc"
    break;

  case 174:
#line 1021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new CommaExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7591 "Parser/parser.cc"
    break;

  case 175:
#line 1026 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7597 "Parser/parser.cc"
    break;

  case 187:
#line 1044 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 7603 "Parser/parser.cc"
    break;

  case 189:
#line 1047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_directive( (yyvsp[0].tok) ) ); }
#line 7609 "Parser/parser.cc"
    break;

  case 190:
#line 1053 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[0].sn)->add_label( (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 7615 "Parser/parser.cc"
    break;

  case 191:
#line 1058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (StatementNode *)0 ) ); }
#line 7621 "Parser/parser.cc"
    break;

  case 192:
#line 1063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (yyvsp[-2].sn) ) ); }
#line 7627 "Parser/parser.cc"
    break;

  case 194:
#line 1069 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 7633 "Parser/parser.cc"
    break;

  case 195:
#line 1074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7639 "Parser/parser.cc"
    break;

  case 196:
#line 1076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7645 "Parser/parser.cc"
    break;

  case 197:
#line 1078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7651 "Parser/parser.cc"
    break;

  case 198:
#line 1080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7657 "Parser/parser.cc"
    break;

  case 201:
#line 1087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 7663 "Parser/parser.cc"
    break;

  case 202:
#line 1092 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_expr( (yyvsp[-1].en) ) ); }
#line 7669 "Parser/parser.cc"
    break;

  case 203:
#line 1095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Mutex expression is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 7675 "Parser/parser.cc"
    break;

  case 204:
#line 1102 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn); }
#line 7681 "Parser/parser.cc"
    break;

  case 205:
#line 1104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( true, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 7687 "Parser/parser.cc"
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
#line 7701 "Parser/parser.cc"
    break;

  case 207:
#line 1116 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( false, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 7707 "Parser/parser.cc"
    break;

  case 208:
#line 1118 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( false, (yyvsp[-7].en), (yyvsp[-2].sn) ) );
			(yyval.sn) = (yyvsp[-3].decl) ? new StatementNode( build_compound( (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 7716 "Parser/parser.cc"
    break;

  case 209:
#line 1127 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-2].ifctl), maybe_build_compound( (yyvsp[0].sn) ), nullptr ) ); }
#line 7722 "Parser/parser.cc"
    break;

  case 210:
#line 1129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-4].ifctl), maybe_build_compound( (yyvsp[-2].sn) ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 7728 "Parser/parser.cc"
    break;

  case 211:
#line 1134 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new IfCtrl( nullptr, (yyvsp[0].en) ); }
#line 7734 "Parser/parser.cc"
    break;

  case 212:
#line 1136 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new IfCtrl( (yyvsp[0].decl), nullptr ); }
#line 7740 "Parser/parser.cc"
    break;

  case 213:
#line 1138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new IfCtrl( (yyvsp[0].decl), nullptr ); }
#line 7746 "Parser/parser.cc"
    break;

  case 214:
#line 1140 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new IfCtrl( (yyvsp[-1].decl), (yyvsp[0].en) ); }
#line 7752 "Parser/parser.cc"
    break;

  case 215:
#line 1147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = (yyvsp[0].en); }
#line 7758 "Parser/parser.cc"
    break;

  case 216:
#line 1149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7764 "Parser/parser.cc"
    break;

  case 218:
#line 1154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.sn) = new StatementNode( build_case( (yyvsp[0].en) ) ); }
#line 7770 "Parser/parser.cc"
    break;

  case 219:
#line 1156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)((yyvsp[-2].sn)->set_last( new StatementNode( build_case( (yyvsp[0].en) ) ) ) ); }
#line 7776 "Parser/parser.cc"
    break;

  case 220:
#line 1160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn); }
#line 7782 "Parser/parser.cc"
    break;

  case 221:
#line 1161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.sn) = new StatementNode( build_default() ); }
#line 7788 "Parser/parser.cc"
    break;

  case 223:
#line 1173 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)( (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) )); }
#line 7794 "Parser/parser.cc"
    break;

  case 224:
#line 1177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( maybe_build_compound( (yyvsp[0].sn) ) ); }
#line 7800 "Parser/parser.cc"
    break;

  case 225:
#line 1182 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = nullptr; }
#line 7806 "Parser/parser.cc"
    break;

  case 227:
#line 1188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ); }
#line 7812 "Parser/parser.cc"
    break;

  case 228:
#line 1190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)( (yyvsp[-2].sn)->set_last( (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ) ) ); }
#line 7818 "Parser/parser.cc"
    break;

  case 229:
#line 1195 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( (yyvsp[-3].ifctl), maybe_build_compound( (yyvsp[-1].sn) ) ) ); }
#line 7824 "Parser/parser.cc"
    break;

  case 230:
#line 1197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( new IfCtrl( nullptr, new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 7830 "Parser/parser.cc"
    break;

  case 231:
#line 1199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( (yyvsp[-2].en), maybe_build_compound( (yyvsp[-5].sn) ) ) ); }
#line 7836 "Parser/parser.cc"
    break;

  case 232:
#line 1201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( new ExpressionNode( build_constantInteger( *new string( "1" ) ) ), maybe_build_compound( (yyvsp[-4].sn) ) ) ); }
#line 7842 "Parser/parser.cc"
    break;

  case 233:
#line 1203 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( (yyvsp[-3].fctl), maybe_build_compound( (yyvsp[-1].sn) ) ) ); }
#line 7848 "Parser/parser.cc"
    break;

  case 234:
#line 1205 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( new ForCtrl( (ExpressionNode * )nullptr, (ExpressionNode * )nullptr, (ExpressionNode * )nullptr ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 7854 "Parser/parser.cc"
    break;

  case 236:
#line 1215 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 7873 "Parser/parser.cc"
    break;

  case 237:
#line 1233 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (ExpressionNode * )nullptr, (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 7879 "Parser/parser.cc"
    break;

  case 238:
#line 1235 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 7885 "Parser/parser.cc"
    break;

  case 239:
#line 1237 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-3].decl), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 7891 "Parser/parser.cc"
    break;

  case 240:
#line 1240 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7898 "Parser/parser.cc"
    break;

  case 241:
#line 1243 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LEThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7905 "Parser/parser.cc"
    break;

  case 242:
#line 1246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].en)->clone(), (yyvsp[-1].compop), (yyvsp[0].en), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7911 "Parser/parser.cc"
    break;

  case 243:
#line 1248 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 7917 "Parser/parser.cc"
    break;

  case 244:
#line 1250 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( new ExpressionNode( build_constantInteger( *new string( "0u" ) ) ), (yyvsp[-1].en), nullptr, OperKinds::LThan, nullptr, nullptr ); }
#line 7923 "Parser/parser.cc"
    break;

  case 245:
#line 1252 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-2].en), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7930 "Parser/parser.cc"
    break;

  case 246:
#line 1255 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-3].en), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LEThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7937 "Parser/parser.cc"
    break;

  case 247:
#line 1258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), (yyvsp[-1].compop), (yyvsp[0].en), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7943 "Parser/parser.cc"
    break;

  case 248:
#line 1260 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 7949 "Parser/parser.cc"
    break;

  case 249:
#line 1263 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Array interator is currently unimplemented." ); (yyval.fctl) = nullptr;
			(yyval.fctl) = forCtrl( new ExpressionNode( build_varref( (yyvsp[0].tok) ) ), (yyvsp[-2].en), nullptr, OperKinds::Range, nullptr, nullptr );
		}
#line 7958 "Parser/parser.cc"
    break;

  case 250:
#line 1270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), OperKinds::LThan, nullptr, new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7964 "Parser/parser.cc"
    break;

  case 251:
#line 1272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), OperKinds::GThan, nullptr, new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7970 "Parser/parser.cc"
    break;

  case 252:
#line 1274 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), OperKinds::LThan, nullptr, (yyvsp[0].en) ); }
#line 7976 "Parser/parser.cc"
    break;

  case 253:
#line 1276 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), OperKinds::GThan, nullptr, (yyvsp[0].en) ); }
#line 7982 "Parser/parser.cc"
    break;

  case 254:
#line 1278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), OperKinds::LThan, nullptr, nullptr ); }
#line 7988 "Parser/parser.cc"
    break;

  case 255:
#line 1283 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LThan; }
#line 7994 "Parser/parser.cc"
    break;

  case 256:
#line 1285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 8000 "Parser/parser.cc"
    break;

  case 257:
#line 1287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GThan; }
#line 8006 "Parser/parser.cc"
    break;

  case 258:
#line 1289 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GEThan; }
#line 8012 "Parser/parser.cc"
    break;

  case 259:
#line 1294 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Goto ) ); }
#line 8018 "Parser/parser.cc"
    break;

  case 260:
#line 1298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_computedgoto( (yyvsp[-1].en) ) ); }
#line 8024 "Parser/parser.cc"
    break;

  case 261:
#line 1301 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThrough ) ); }
#line 8030 "Parser/parser.cc"
    break;

  case 262:
#line 1303 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::FallThrough ) ); }
#line 8036 "Parser/parser.cc"
    break;

  case 263:
#line 1305 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThroughDefault ) ); }
#line 8042 "Parser/parser.cc"
    break;

  case 264:
#line 1308 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Continue ) ); }
#line 8048 "Parser/parser.cc"
    break;

  case 265:
#line 1312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Continue ) ); }
#line 8054 "Parser/parser.cc"
    break;

  case 266:
#line 1315 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Break ) ); }
#line 8060 "Parser/parser.cc"
    break;

  case 267:
#line 1319 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Break ) ); }
#line 8066 "Parser/parser.cc"
    break;

  case 268:
#line 1321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_return( (yyvsp[-1].en) ) ); }
#line 8072 "Parser/parser.cc"
    break;

  case 269:
#line 1323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 8078 "Parser/parser.cc"
    break;

  case 270:
#line 1325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr ) ); }
#line 8084 "Parser/parser.cc"
    break;

  case 271:
#line 1327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn) ) ); }
#line 8090 "Parser/parser.cc"
    break;

  case 272:
#line 1329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Coroutine ) ); }
#line 8096 "Parser/parser.cc"
    break;

  case 273:
#line 1331 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Coroutine ) ); }
#line 8102 "Parser/parser.cc"
    break;

  case 274:
#line 1333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Generator ) ); }
#line 8108 "Parser/parser.cc"
    break;

  case 275:
#line 1335 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Generator ) ); }
#line 8114 "Parser/parser.cc"
    break;

  case 276:
#line 1337 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_throw( (yyvsp[-1].en) ) ); }
#line 8120 "Parser/parser.cc"
    break;

  case 277:
#line 1339 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume( (yyvsp[-1].en) ) ); }
#line 8126 "Parser/parser.cc"
    break;

  case 278:
#line 1341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume_at( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 8132 "Parser/parser.cc"
    break;

  case 281:
#line 1351 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_with( (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8138 "Parser/parser.cc"
    break;

  case 282:
#line 1357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_mutex( (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8144 "Parser/parser.cc"
    break;

  case 283:
#line 1361 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 8150 "Parser/parser.cc"
    break;

  case 284:
#line 1366 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8156 "Parser/parser.cc"
    break;

  case 286:
#line 1372 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 8162 "Parser/parser.cc"
    break;

  case 287:
#line 1376 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) )); }
#line 8168 "Parser/parser.cc"
    break;

  case 289:
#line 1383 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8174 "Parser/parser.cc"
    break;

  case 290:
#line 1387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 8180 "Parser/parser.cc"
    break;

  case 291:
#line 1392 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8186 "Parser/parser.cc"
    break;

  case 292:
#line 1394 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-3].en), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[-4].en), (yyvsp[0].wfs) ); }
#line 8192 "Parser/parser.cc"
    break;

  case 293:
#line 1396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8198 "Parser/parser.cc"
    break;

  case 294:
#line 1398 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( nullptr, maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8204 "Parser/parser.cc"
    break;

  case 295:
#line 1401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 8210 "Parser/parser.cc"
    break;

  case 296:
#line 1403 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-5].en), maybe_build_compound( (yyvsp[-4].sn) ), (yyvsp[-6].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8216 "Parser/parser.cc"
    break;

  case 297:
#line 1408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-1].en), (yyvsp[0].sn), (yyvsp[-2].en) ) ); }
#line 8222 "Parser/parser.cc"
    break;

  case 298:
#line 1410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-3].en), (yyvsp[-2].sn), (yyvsp[-4].en), (yyvsp[0].wfs) ) ); }
#line 8228 "Parser/parser.cc"
    break;

  case 299:
#line 1415 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), (yyvsp[0].sn), 0 ) ); }
#line 8234 "Parser/parser.cc"
    break;

  case 300:
#line 1417 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), 0, (yyvsp[0].sn) ) ); }
#line 8240 "Parser/parser.cc"
    break;

  case 301:
#line 1419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-2].sn), (yyvsp[-1].sn), (yyvsp[0].sn) ) ); }
#line 8246 "Parser/parser.cc"
    break;

  case 302:
#line 1424 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8252 "Parser/parser.cc"
    break;

  case 303:
#line 1426 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)(yyvsp[-8].sn)->set_last( new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ) ); }
#line 8258 "Parser/parser.cc"
    break;

  case 304:
#line 1431 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8264 "Parser/parser.cc"
    break;

  case 305:
#line 1432 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[0].en); }
#line 8270 "Parser/parser.cc"
    break;

  case 306:
#line 1436 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 8276 "Parser/parser.cc"
    break;

  case 307:
#line 1437 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 8282 "Parser/parser.cc"
    break;

  case 308:
#line 1438 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.catch_kind) = CatchStmt::Resume; }
#line 8288 "Parser/parser.cc"
    break;

  case 309:
#line 1439 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Resume; }
#line 8294 "Parser/parser.cc"
    break;

  case 310:
#line 1443 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = new StatementNode( build_finally( (yyvsp[0].sn) ) ); }
#line 8300 "Parser/parser.cc"
    break;

  case 312:
#line 1450 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 8306 "Parser/parser.cc"
    break;

  case 313:
#line 1452 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 8312 "Parser/parser.cc"
    break;

  case 314:
#line 1454 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 8318 "Parser/parser.cc"
    break;

  case 319:
#line 1469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-4].flag), (yyvsp[-2].constant), 0 ) ); }
#line 8324 "Parser/parser.cc"
    break;

  case 320:
#line 1471 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-6].flag), (yyvsp[-4].constant), (yyvsp[-2].en) ) ); }
#line 8330 "Parser/parser.cc"
    break;

  case 321:
#line 1473 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-8].flag), (yyvsp[-6].constant), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 8336 "Parser/parser.cc"
    break;

  case 322:
#line 1475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-10].flag), (yyvsp[-8].constant), (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 8342 "Parser/parser.cc"
    break;

  case 323:
#line 1477 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-12].flag), (yyvsp[-9].constant), 0, (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].label) ) ); }
#line 8348 "Parser/parser.cc"
    break;

  case 324:
#line 1482 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = false; }
#line 8354 "Parser/parser.cc"
    break;

  case 325:
#line 1484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = true; }
#line 8360 "Parser/parser.cc"
    break;

  case 326:
#line 1489 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8366 "Parser/parser.cc"
    break;

  case 329:
#line 1496 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 8372 "Parser/parser.cc"
    break;

  case 330:
#line 1501 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( nullptr, (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 8378 "Parser/parser.cc"
    break;

  case 331:
#line 1503 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( (yyvsp[-5].tok), (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 8384 "Parser/parser.cc"
    break;

  case 332:
#line 1508 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8390 "Parser/parser.cc"
    break;

  case 333:
#line 1510 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 8396 "Parser/parser.cc"
    break;

  case 334:
#line 1512 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( (yyvsp[0].constant) ) )); }
#line 8402 "Parser/parser.cc"
    break;

  case 335:
#line 1517 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = new LabelNode(); (yyval.label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 8411 "Parser/parser.cc"
    break;

  case 336:
#line 1522 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = (yyvsp[-2].label); (yyvsp[-2].label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 8420 "Parser/parser.cc"
    break;

  case 337:
#line 1532 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8426 "Parser/parser.cc"
    break;

  case 340:
#line 1539 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 8432 "Parser/parser.cc"
    break;

  case 341:
#line 1544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8438 "Parser/parser.cc"
    break;

  case 343:
#line 1550 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 8444 "Parser/parser.cc"
    break;

  case 344:
#line 1552 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-2].decl) ); }
#line 8450 "Parser/parser.cc"
    break;

  case 354:
#line 1578 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-4].en), (yyvsp[-2].constant) ); }
#line 8456 "Parser/parser.cc"
    break;

  case 355:
#line 1580 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-2].en), build_constantStr( *new string( "\"\"" ) ) ); }
#line 8462 "Parser/parser.cc"
    break;

  case 359:
#line 1598 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 8468 "Parser/parser.cc"
    break;

  case 361:
#line 1604 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].in) ); }
#line 8474 "Parser/parser.cc"
    break;

  case 362:
#line 1608 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 8480 "Parser/parser.cc"
    break;

  case 363:
#line 1610 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].in) ) ); }
#line 8486 "Parser/parser.cc"
    break;

  case 364:
#line 1617 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 8492 "Parser/parser.cc"
    break;

  case 365:
#line 1619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 8498 "Parser/parser.cc"
    break;

  case 366:
#line 1621 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 8504 "Parser/parser.cc"
    break;

  case 368:
#line 1627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 8510 "Parser/parser.cc"
    break;

  case 369:
#line 1629 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 8516 "Parser/parser.cc"
    break;

  case 370:
#line 1631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 8522 "Parser/parser.cc"
    break;

  case 371:
#line 1633 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeClone( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->appendList( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 8533 "Parser/parser.cc"
    break;

  case 372:
#line 1666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), 0 )->addQualifiers( (yyvsp[0].decl) ); }
#line 8539 "Parser/parser.cc"
    break;

  case 373:
#line 1668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), 0 )->addQualifiers( (yyvsp[0].decl) ); }
#line 8545 "Parser/parser.cc"
    break;

  case 374:
#line 1673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 8551 "Parser/parser.cc"
    break;

  case 375:
#line 1676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-6].decl)->appendList( (yyvsp[-2].decl) ) ); }
#line 8557 "Parser/parser.cc"
    break;

  case 376:
#line 1681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 8566 "Parser/parser.cc"
    break;

  case 377:
#line 1686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 8575 "Parser/parser.cc"
    break;

  case 378:
#line 1691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "3" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 8584 "Parser/parser.cc"
    break;

  case 379:
#line 1702 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "4" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef();
		}
#line 8593 "Parser/parser.cc"
    break;

  case 380:
#line 1707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "5" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 8602 "Parser/parser.cc"
    break;

  case 381:
#line 1712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "6" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[-3].decl) )->addTypedef();
		}
#line 8611 "Parser/parser.cc"
    break;

  case 382:
#line 1717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "7" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-2].decl) )->addTypedef();
		}
#line 8620 "Parser/parser.cc"
    break;

  case 383:
#line 1722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "8" );
			(yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-3].decl) )->addTypedef()->addType( (yyvsp[-3].decl) );
		}
#line 8629 "Parser/parser.cc"
    break;

  case 384:
#line 1731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 8637 "Parser/parser.cc"
    break;

  case 385:
#line 1735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 8645 "Parser/parser.cc"
    break;

  case 386:
#line 1742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 8651 "Parser/parser.cc"
    break;

  case 390:
#line 1752 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 8657 "Parser/parser.cc"
    break;

  case 391:
#line 1754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ) ); }
#line 8663 "Parser/parser.cc"
    break;

  case 404:
#line 1793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8669 "Parser/parser.cc"
    break;

  case 407:
#line 1805 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 8675 "Parser/parser.cc"
    break;

  case 410:
#line 1815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Const ); }
#line 8681 "Parser/parser.cc"
    break;

  case 411:
#line 1817 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Restrict ); }
#line 8687 "Parser/parser.cc"
    break;

  case 412:
#line 1819 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Volatile ); }
#line 8693 "Parser/parser.cc"
    break;

  case 413:
#line 1821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Atomic ); }
#line 8699 "Parser/parser.cc"
    break;

  case 415:
#line 1827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newForall( (yyvsp[-1].decl) ); }
#line 8705 "Parser/parser.cc"
    break;

  case 417:
#line 1833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 8711 "Parser/parser.cc"
    break;

  case 418:
#line 1835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 8717 "Parser/parser.cc"
    break;

  case 420:
#line 1846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 8723 "Parser/parser.cc"
    break;

  case 421:
#line 1851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Extern ); }
#line 8729 "Parser/parser.cc"
    break;

  case 422:
#line 1853 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Static ); }
#line 8735 "Parser/parser.cc"
    break;

  case 423:
#line 1855 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Auto ); }
#line 8741 "Parser/parser.cc"
    break;

  case 424:
#line 1857 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Register ); }
#line 8747 "Parser/parser.cc"
    break;

  case 425:
#line 1859 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Threadlocal ); }
#line 8753 "Parser/parser.cc"
    break;

  case 426:
#line 1862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Inline ); }
#line 8759 "Parser/parser.cc"
    break;

  case 427:
#line 1864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Fortran ); }
#line 8765 "Parser/parser.cc"
    break;

  case 428:
#line 1866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Noreturn ); }
#line 8771 "Parser/parser.cc"
    break;

  case 429:
#line 1871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 8777 "Parser/parser.cc"
    break;

  case 430:
#line 1873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Bool ); }
#line 8783 "Parser/parser.cc"
    break;

  case 431:
#line 1875 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Char ); }
#line 8789 "Parser/parser.cc"
    break;

  case 432:
#line 1877 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int ); }
#line 8795 "Parser/parser.cc"
    break;

  case 433:
#line 1879 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 ); }
#line 8801 "Parser/parser.cc"
    break;

  case 434:
#line 1881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 )->addType( DeclarationNode::newSignedNess( DeclarationNode::Unsigned ) ); }
#line 8807 "Parser/parser.cc"
    break;

  case 435:
#line 1883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Float ); }
#line 8813 "Parser/parser.cc"
    break;

  case 436:
#line 1885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Double ); }
#line 8819 "Parser/parser.cc"
    break;

  case 437:
#line 1887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat80 ); }
#line 8825 "Parser/parser.cc"
    break;

  case 438:
#line 1889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat128 ); }
#line 8831 "Parser/parser.cc"
    break;

  case 439:
#line 1891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat16 ); }
#line 8837 "Parser/parser.cc"
    break;

  case 440:
#line 1893 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32 ); }
#line 8843 "Parser/parser.cc"
    break;

  case 441:
#line 1895 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32x ); }
#line 8849 "Parser/parser.cc"
    break;

  case 442:
#line 1897 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64 ); }
#line 8855 "Parser/parser.cc"
    break;

  case 443:
#line 1899 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64x ); }
#line 8861 "Parser/parser.cc"
    break;

  case 444:
#line 1901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat128 ); }
#line 8867 "Parser/parser.cc"
    break;

  case 445:
#line 1903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 8873 "Parser/parser.cc"
    break;

  case 446:
#line 1905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 8879 "Parser/parser.cc"
    break;

  case 447:
#line 1907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 8885 "Parser/parser.cc"
    break;

  case 448:
#line 1909 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Complex ); }
#line 8891 "Parser/parser.cc"
    break;

  case 449:
#line 1911 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Imaginary ); }
#line 8897 "Parser/parser.cc"
    break;

  case 450:
#line 1913 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Signed ); }
#line 8903 "Parser/parser.cc"
    break;

  case 451:
#line 1915 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Unsigned ); }
#line 8909 "Parser/parser.cc"
    break;

  case 452:
#line 1917 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Short ); }
#line 8915 "Parser/parser.cc"
    break;

  case 453:
#line 1919 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Long ); }
#line 8921 "Parser/parser.cc"
    break;

  case 454:
#line 1921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Valist ); }
#line 8927 "Parser/parser.cc"
    break;

  case 455:
#line 1923 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::AutoType ); }
#line 8933 "Parser/parser.cc"
    break;

  case 457:
#line 1929 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8939 "Parser/parser.cc"
    break;

  case 459:
#line 1935 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVtableType( (yyvsp[-2].decl) ); }
#line 8945 "Parser/parser.cc"
    break;

  case 460:
#line 1941 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8951 "Parser/parser.cc"
    break;

  case 461:
#line 1943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 8957 "Parser/parser.cc"
    break;

  case 463:
#line 1950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 8963 "Parser/parser.cc"
    break;

  case 464:
#line 1952 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 8969 "Parser/parser.cc"
    break;

  case 465:
#line 1954 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 8975 "Parser/parser.cc"
    break;

  case 466:
#line 1956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 8981 "Parser/parser.cc"
    break;

  case 468:
#line 1963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 8987 "Parser/parser.cc"
    break;

  case 470:
#line 1969 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 8993 "Parser/parser.cc"
    break;

  case 471:
#line 1971 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 8999 "Parser/parser.cc"
    break;

  case 472:
#line 1973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 9005 "Parser/parser.cc"
    break;

  case 473:
#line 1978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 9011 "Parser/parser.cc"
    break;

  case 474:
#line 1980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en) ); }
#line 9017 "Parser/parser.cc"
    break;

  case 475:
#line 1982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 9023 "Parser/parser.cc"
    break;

  case 476:
#line 1984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en), true ); }
#line 9029 "Parser/parser.cc"
    break;

  case 477:
#line 1986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Zero ); }
#line 9035 "Parser/parser.cc"
    break;

  case 478:
#line 1988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::One ); }
#line 9041 "Parser/parser.cc"
    break;

  case 480:
#line 1994 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9047 "Parser/parser.cc"
    break;

  case 481:
#line 1996 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9053 "Parser/parser.cc"
    break;

  case 482:
#line 1998 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9059 "Parser/parser.cc"
    break;

  case 484:
#line 2004 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 9065 "Parser/parser.cc"
    break;

  case 485:
#line 2006 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 9071 "Parser/parser.cc"
    break;

  case 486:
#line 2008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 9080 "Parser/parser.cc"
    break;

  case 488:
#line 2017 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9086 "Parser/parser.cc"
    break;

  case 489:
#line 2019 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9092 "Parser/parser.cc"
    break;

  case 490:
#line 2021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9098 "Parser/parser.cc"
    break;

  case 492:
#line 2027 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9104 "Parser/parser.cc"
    break;

  case 493:
#line 2029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9110 "Parser/parser.cc"
    break;

  case 495:
#line 2035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9116 "Parser/parser.cc"
    break;

  case 496:
#line 2037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9122 "Parser/parser.cc"
    break;

  case 497:
#line 2039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9128 "Parser/parser.cc"
    break;

  case 499:
#line 2045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9134 "Parser/parser.cc"
    break;

  case 500:
#line 2047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9140 "Parser/parser.cc"
    break;

  case 501:
#line 2052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypedef( (yyvsp[0].tok) ); }
#line 9146 "Parser/parser.cc"
    break;

  case 502:
#line 2054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 9152 "Parser/parser.cc"
    break;

  case 503:
#line 2056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 9158 "Parser/parser.cc"
    break;

  case 505:
#line 2059 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), (yyvsp[0].decl) ); }
#line 9164 "Parser/parser.cc"
    break;

  case 506:
#line 2061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), (yyvsp[0].decl) ); }
#line 9170 "Parser/parser.cc"
    break;

  case 507:
#line 2066 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[0].tok), nullptr ); }
#line 9176 "Parser/parser.cc"
    break;

  case 508:
#line 2068 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-2].tok), nullptr ); }
#line 9182 "Parser/parser.cc"
    break;

  case 509:
#line 2070 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 9188 "Parser/parser.cc"
    break;

  case 514:
#line 2085 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 9194 "Parser/parser.cc"
    break;

  case 515:
#line 2087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 9200 "Parser/parser.cc"
    break;

  case 516:
#line 2089 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 9209 "Parser/parser.cc"
    break;

  case 517:
#line 2094 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) ); }
#line 9215 "Parser/parser.cc"
    break;

  case 518:
#line 2096 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 9224 "Parser/parser.cc"
    break;

  case 519:
#line 2101 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypedef( (yyvsp[-5].tok) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 9233 "Parser/parser.cc"
    break;

  case 520:
#line 2106 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 9242 "Parser/parser.cc"
    break;

  case 521:
#line 2111 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeGen( (yyvsp[-5].tok), nullptr );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 9251 "Parser/parser.cc"
    break;

  case 523:
#line 2120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9257 "Parser/parser.cc"
    break;

  case 524:
#line 2122 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 9263 "Parser/parser.cc"
    break;

  case 525:
#line 2127 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 9273 "Parser/parser.cc"
    break;

  case 526:
#line 2133 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 9288 "Parser/parser.cc"
    break;

  case 529:
#line 2152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Struct; }
#line 9294 "Parser/parser.cc"
    break;

  case 530:
#line 2154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Union; }
#line 9300 "Parser/parser.cc"
    break;

  case 531:
#line 2156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Exception; }
#line 9306 "Parser/parser.cc"
    break;

  case 532:
#line 2162 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 9312 "Parser/parser.cc"
    break;

  case 533:
#line 2164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 9318 "Parser/parser.cc"
    break;

  case 534:
#line 2166 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Generator; }
#line 9324 "Parser/parser.cc"
    break;

  case 535:
#line 2168 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor generator is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 9330 "Parser/parser.cc"
    break;

  case 536:
#line 2170 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Coroutine; }
#line 9336 "Parser/parser.cc"
    break;

  case 537:
#line 2172 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor coroutine is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 9342 "Parser/parser.cc"
    break;

  case 538:
#line 2174 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Thread; }
#line 9348 "Parser/parser.cc"
    break;

  case 539:
#line 2176 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor thread is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 9354 "Parser/parser.cc"
    break;

  case 540:
#line 2181 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9360 "Parser/parser.cc"
    break;

  case 541:
#line 2183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 9366 "Parser/parser.cc"
    break;

  case 542:
#line 2188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); }
#line 9372 "Parser/parser.cc"
    break;

  case 543:
#line 2190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 9378 "Parser/parser.cc"
    break;

  case 544:
#line 2192 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-1].decl) ) {								// field declarator ?
				(yyvsp[-1].decl) = DeclarationNode::newName( nullptr );
			} // if
			(yyvsp[-1].decl)->inLine = true;
			(yyval.decl) = distAttr( (yyvsp[-2].decl), (yyvsp[-1].decl) );					// mark all fields in list
			distInl( (yyvsp[-1].decl) );
		}
#line 9391 "Parser/parser.cc"
    break;

  case 545:
#line 2201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9397 "Parser/parser.cc"
    break;

  case 548:
#line 2205 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 9403 "Parser/parser.cc"
    break;

  case 549:
#line 2207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 9409 "Parser/parser.cc"
    break;

  case 552:
#line 2214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9415 "Parser/parser.cc"
    break;

  case 554:
#line 2217 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 9421 "Parser/parser.cc"
    break;

  case 555:
#line 2222 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].en) ); }
#line 9427 "Parser/parser.cc"
    break;

  case 556:
#line 2225 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 9433 "Parser/parser.cc"
    break;

  case 557:
#line 2228 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 9439 "Parser/parser.cc"
    break;

  case 558:
#line 2233 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9445 "Parser/parser.cc"
    break;

  case 560:
#line 2236 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 9451 "Parser/parser.cc"
    break;

  case 562:
#line 2247 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 9457 "Parser/parser.cc"
    break;

  case 563:
#line 2249 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 9463 "Parser/parser.cc"
    break;

  case 565:
#line 2256 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 9469 "Parser/parser.cc"
    break;

  case 566:
#line 2261 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9475 "Parser/parser.cc"
    break;

  case 568:
#line 2267 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 9481 "Parser/parser.cc"
    break;

  case 569:
#line 2272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-4].decl) ); }
#line 9487 "Parser/parser.cc"
    break;

  case 570:
#line 2274 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); }
#line 9493 "Parser/parser.cc"
    break;

  case 571:
#line 2276 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].tok), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) ); }
#line 9499 "Parser/parser.cc"
    break;

  case 572:
#line 2279 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-4].decl)->name, (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 9505 "Parser/parser.cc"
    break;

  case 573:
#line 2281 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-6].decl)->storageClasses.val != 0 || (yyvsp[-6].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			SemanticError( yylloc, "Typed enumeration is currently unimplemented." ); (yyval.decl) = nullptr;
		}
#line 9514 "Parser/parser.cc"
    break;

  case 574:
#line 2286 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].decl)->storageClasses.val != 0 || (yyvsp[-4].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			typedefTable.makeTypedef( *(yyvsp[-1].tok) );
		}
#line 9523 "Parser/parser.cc"
    break;

  case 575:
#line 2291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typed enumeration is currently unimplemented." ); (yyval.decl) = nullptr;
		}
#line 9531 "Parser/parser.cc"
    break;

  case 576:
#line 2295 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-8].decl)->storageClasses.val != 0 || (yyvsp[-8].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			typedefTable.makeTypedef( *(yyvsp[-5].decl)->name );
			SemanticError( yylloc, "Typed enumeration is currently unimplemented." ); (yyval.decl) = nullptr;
		}
#line 9541 "Parser/parser.cc"
    break;

  case 578:
#line 2305 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); (yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), 0, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 9547 "Parser/parser.cc"
    break;

  case 579:
#line 2307 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].decl)->type->symbolic.name );	(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].decl)->type->symbolic.name, 0, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 9553 "Parser/parser.cc"
    break;

  case 580:
#line 2312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumConstant( (yyvsp[-1].tok), (yyvsp[0].en) ); }
#line 9559 "Parser/parser.cc"
    break;

  case 581:
#line 2314 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumConstant( new string("inline"), nullptr ); }
#line 9565 "Parser/parser.cc"
    break;

  case 582:
#line 2316 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( DeclarationNode::newEnumConstant( (yyvsp[-1].tok), (yyvsp[0].en) ) ); }
#line 9571 "Parser/parser.cc"
    break;

  case 583:
#line 2318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( DeclarationNode::newEnumConstant( new string("inline"), nullptr ) ); }
#line 9577 "Parser/parser.cc"
    break;

  case 584:
#line 2323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9583 "Parser/parser.cc"
    break;

  case 585:
#line 2327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].in)->get_expression(); }
#line 9589 "Parser/parser.cc"
    break;

  case 586:
#line 2332 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 9595 "Parser/parser.cc"
    break;

  case 587:
#line 2334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9601 "Parser/parser.cc"
    break;

  case 590:
#line 2338 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9607 "Parser/parser.cc"
    break;

  case 591:
#line 2340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 9613 "Parser/parser.cc"
    break;

  case 592:
#line 2342 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 9619 "Parser/parser.cc"
    break;

  case 594:
#line 2350 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9625 "Parser/parser.cc"
    break;

  case 595:
#line 2352 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9631 "Parser/parser.cc"
    break;

  case 596:
#line 2354 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-8].decl)->appendList( (yyvsp[-4].decl) )->appendList( (yyvsp[0].decl) ); }
#line 9637 "Parser/parser.cc"
    break;

  case 598:
#line 2360 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9643 "Parser/parser.cc"
    break;

  case 599:
#line 2365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9649 "Parser/parser.cc"
    break;

  case 600:
#line 2367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9655 "Parser/parser.cc"
    break;

  case 602:
#line 2370 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 9661 "Parser/parser.cc"
    break;

  case 605:
#line 2377 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9667 "Parser/parser.cc"
    break;

  case 606:
#line 2379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9673 "Parser/parser.cc"
    break;

  case 608:
#line 2388 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 9679 "Parser/parser.cc"
    break;

  case 609:
#line 2391 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 9685 "Parser/parser.cc"
    break;

  case 610:
#line 2393 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 9691 "Parser/parser.cc"
    break;

  case 615:
#line 2403 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9697 "Parser/parser.cc"
    break;

  case 617:
#line 2410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 9703 "Parser/parser.cc"
    break;

  case 618:
#line 2412 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 9709 "Parser/parser.cc"
    break;

  case 619:
#line 2417 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 9715 "Parser/parser.cc"
    break;

  case 620:
#line 2419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 9721 "Parser/parser.cc"
    break;

  case 621:
#line 2428 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 9727 "Parser/parser.cc"
    break;

  case 622:
#line 2430 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 9733 "Parser/parser.cc"
    break;

  case 628:
#line 2443 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 9739 "Parser/parser.cc"
    break;

  case 631:
#line 2453 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 9745 "Parser/parser.cc"
    break;

  case 632:
#line 2454 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (yyvsp[-1].op) == OperKinds::Assign ? (yyvsp[0].in) : (yyvsp[0].in)->set_maybeConstructed( false ); }
#line 9751 "Parser/parser.cc"
    break;

  case 633:
#line 2455 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.in) = new InitializerNode( true ); }
#line 9757 "Parser/parser.cc"
    break;

  case 634:
#line 2456 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 9763 "Parser/parser.cc"
    break;

  case 635:
#line 2460 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = new InitializerNode( (yyvsp[0].en) ); }
#line 9769 "Parser/parser.cc"
    break;

  case 636:
#line 2461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 9775 "Parser/parser.cc"
    break;

  case 637:
#line 2466 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 9781 "Parser/parser.cc"
    break;

  case 639:
#line 2468 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ); }
#line 9787 "Parser/parser.cc"
    break;

  case 640:
#line 2469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (InitializerNode *)( (yyvsp[-2].in)->set_last( (yyvsp[0].in) ) ); }
#line 9793 "Parser/parser.cc"
    break;

  case 641:
#line 2470 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.in) = (InitializerNode *)((yyvsp[-3].in)->set_last( (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ) )); }
#line 9799 "Parser/parser.cc"
    break;

  case 643:
#line 2486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[-1].tok) ) ); }
#line 9805 "Parser/parser.cc"
    break;

  case 645:
#line 2492 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-1].en)->set_last( (yyvsp[0].en) )); }
#line 9811 "Parser/parser.cc"
    break;

  case 646:
#line 2498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 9817 "Parser/parser.cc"
    break;

  case 647:
#line 2501 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 9823 "Parser/parser.cc"
    break;

  case 648:
#line 2503 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 9829 "Parser/parser.cc"
    break;

  case 649:
#line 2505 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-4].en) ), maybeMoveBuild<Expression>( (yyvsp[-2].en) ) ) ); }
#line 9835 "Parser/parser.cc"
    break;

  case 650:
#line 2507 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 9841 "Parser/parser.cc"
    break;

  case 652:
#line 2531 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl) ); }
#line 9847 "Parser/parser.cc"
    break;

  case 653:
#line 2536 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9853 "Parser/parser.cc"
    break;

  case 654:
#line 2538 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 9859 "Parser/parser.cc"
    break;

  case 655:
#line 2543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "9" );
			if ( (yyvsp[-1].tclass) == TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 9870 "Parser/parser.cc"
    break;

  case 656:
#line 2550 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 9876 "Parser/parser.cc"
    break;

  case 657:
#line 2552 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "9" ); }
#line 9882 "Parser/parser.cc"
    break;

  case 658:
#line 2554 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 9888 "Parser/parser.cc"
    break;

  case 659:
#line 2556 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "9" );
			(yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 9897 "Parser/parser.cc"
    break;

  case 660:
#line 2562 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 9903 "Parser/parser.cc"
    break;

  case 661:
#line 2567 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 9909 "Parser/parser.cc"
    break;

  case 662:
#line 2569 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 9915 "Parser/parser.cc"
    break;

  case 663:
#line 2571 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::DStype; }
#line 9921 "Parser/parser.cc"
    break;

  case 664:
#line 2575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 9927 "Parser/parser.cc"
    break;

  case 665:
#line 2580 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 9933 "Parser/parser.cc"
    break;

  case 666:
#line 2582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 9939 "Parser/parser.cc"
    break;

  case 667:
#line 2584 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ftype; }
#line 9945 "Parser/parser.cc"
    break;

  case 668:
#line 2586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 9951 "Parser/parser.cc"
    break;

  case 669:
#line 2591 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9957 "Parser/parser.cc"
    break;

  case 672:
#line 2598 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 9963 "Parser/parser.cc"
    break;

  case 673:
#line 2603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 9969 "Parser/parser.cc"
    break;

  case 674:
#line 2605 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 9975 "Parser/parser.cc"
    break;

  case 675:
#line 2612 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 9981 "Parser/parser.cc"
    break;

  case 677:
#line 2615 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 9987 "Parser/parser.cc"
    break;

  case 678:
#line 2617 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)( (yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 9993 "Parser/parser.cc"
    break;

  case 679:
#line 2622 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 9999 "Parser/parser.cc"
    break;

  case 680:
#line 2624 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10005 "Parser/parser.cc"
    break;

  case 681:
#line 2626 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 10011 "Parser/parser.cc"
    break;

  case 682:
#line 2631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 10017 "Parser/parser.cc"
    break;

  case 683:
#line 2633 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 10023 "Parser/parser.cc"
    break;

  case 684:
#line 2638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "10" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), 0 );
		}
#line 10032 "Parser/parser.cc"
    break;

  case 685:
#line 2643 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "11" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 10041 "Parser/parser.cc"
    break;

  case 686:
#line 2651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), 0 ); }
#line 10047 "Parser/parser.cc"
    break;

  case 687:
#line 2653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-8].tok), (yyvsp[-6].decl), (yyvsp[-2].decl) ); }
#line 10053 "Parser/parser.cc"
    break;

  case 689:
#line 2659 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl) ); }
#line 10059 "Parser/parser.cc"
    break;

  case 694:
#line 2671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 10065 "Parser/parser.cc"
    break;

  case 695:
#line 2676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10071 "Parser/parser.cc"
    break;

  case 696:
#line 2678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 10077 "Parser/parser.cc"
    break;

  case 698:
#line 2686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl);	}
#line 10083 "Parser/parser.cc"
    break;

  case 699:
#line 2691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10089 "Parser/parser.cc"
    break;

  case 700:
#line 2693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->appendList( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 10095 "Parser/parser.cc"
    break;

  case 701:
#line 2698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10101 "Parser/parser.cc"
    break;

  case 703:
#line 2703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 10107 "Parser/parser.cc"
    break;

  case 704:
#line 2707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 10113 "Parser/parser.cc"
    break;

  case 705:
#line 2712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( (yyvsp[0].tok) ) ) ); }
#line 10119 "Parser/parser.cc"
    break;

  case 708:
#line 2716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 10128 "Parser/parser.cc"
    break;

  case 709:
#line 2721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( false, (yyvsp[-2].constant), 0 ) ) ); }
#line 10134 "Parser/parser.cc"
    break;

  case 710:
#line 2723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = LinkageSpec::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 10143 "Parser/parser.cc"
    break;

  case 711:
#line 2728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10153 "Parser/parser.cc"
    break;

  case 712:
#line 2734 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 10162 "Parser/parser.cc"
    break;

  case 713:
#line 2739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10172 "Parser/parser.cc"
    break;

  case 714:
#line 2745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 10181 "Parser/parser.cc"
    break;

  case 715:
#line 2750 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10191 "Parser/parser.cc"
    break;

  case 716:
#line 2756 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.val) || (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 10200 "Parser/parser.cc"
    break;

  case 717:
#line 2761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10210 "Parser/parser.cc"
    break;

  case 719:
#line 2776 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].sn) ); }
#line 10216 "Parser/parser.cc"
    break;

  case 720:
#line 2778 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].sn) ); }
#line 10222 "Parser/parser.cc"
    break;

  case 721:
#line 2783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; forall = false; }
#line 10228 "Parser/parser.cc"
    break;

  case 722:
#line 2785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); forall = false; }
#line 10234 "Parser/parser.cc"
    break;

  case 723:
#line 2790 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10245 "Parser/parser.cc"
    break;

  case 724:
#line 2797 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 10254 "Parser/parser.cc"
    break;

  case 725:
#line 2802 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 10263 "Parser/parser.cc"
    break;

  case 726:
#line 2808 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 10269 "Parser/parser.cc"
    break;

  case 727:
#line 2811 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 10275 "Parser/parser.cc"
    break;

  case 728:
#line 2814 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10281 "Parser/parser.cc"
    break;

  case 729:
#line 2818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-4].decl) );
		}
#line 10290 "Parser/parser.cc"
    break;

  case 730:
#line 2824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10296 "Parser/parser.cc"
    break;

  case 731:
#line 2827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10302 "Parser/parser.cc"
    break;

  case 732:
#line 2830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 10308 "Parser/parser.cc"
    break;

  case 736:
#line 2841 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 10314 "Parser/parser.cc"
    break;

  case 737:
#line 2846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10320 "Parser/parser.cc"
    break;

  case 738:
#line 2848 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = (yyvsp[-2].constant);
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 10330 "Parser/parser.cc"
    break;

  case 739:
#line 2857 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10336 "Parser/parser.cc"
    break;

  case 742:
#line 2864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10342 "Parser/parser.cc"
    break;

  case 743:
#line 2869 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 10348 "Parser/parser.cc"
    break;

  case 745:
#line 2875 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10354 "Parser/parser.cc"
    break;

  case 746:
#line 2880 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10360 "Parser/parser.cc"
    break;

  case 747:
#line 2882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 10366 "Parser/parser.cc"
    break;

  case 748:
#line 2884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 10372 "Parser/parser.cc"
    break;

  case 753:
#line 2893 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 10378 "Parser/parser.cc"
    break;

  case 754:
#line 2895 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 10384 "Parser/parser.cc"
    break;

  case 755:
#line 2930 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 10390 "Parser/parser.cc"
    break;

  case 756:
#line 2932 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10396 "Parser/parser.cc"
    break;

  case 757:
#line 2937 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10402 "Parser/parser.cc"
    break;

  case 759:
#line 2940 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10408 "Parser/parser.cc"
    break;

  case 760:
#line 2942 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10414 "Parser/parser.cc"
    break;

  case 761:
#line 2947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10420 "Parser/parser.cc"
    break;

  case 762:
#line 2949 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10426 "Parser/parser.cc"
    break;

  case 763:
#line 2951 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10432 "Parser/parser.cc"
    break;

  case 764:
#line 2953 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10438 "Parser/parser.cc"
    break;

  case 765:
#line 2958 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 10444 "Parser/parser.cc"
    break;

  case 766:
#line 2960 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10450 "Parser/parser.cc"
    break;

  case 767:
#line 2962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10456 "Parser/parser.cc"
    break;

  case 768:
#line 2964 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10462 "Parser/parser.cc"
    break;

  case 769:
#line 2966 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10468 "Parser/parser.cc"
    break;

  case 770:
#line 2968 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10474 "Parser/parser.cc"
    break;

  case 771:
#line 2970 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10480 "Parser/parser.cc"
    break;

  case 772:
#line 2975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10486 "Parser/parser.cc"
    break;

  case 773:
#line 2977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 10492 "Parser/parser.cc"
    break;

  case 774:
#line 2979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10498 "Parser/parser.cc"
    break;

  case 775:
#line 2981 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10504 "Parser/parser.cc"
    break;

  case 776:
#line 2990 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10510 "Parser/parser.cc"
    break;

  case 778:
#line 2993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10516 "Parser/parser.cc"
    break;

  case 779:
#line 2998 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10522 "Parser/parser.cc"
    break;

  case 780:
#line 3000 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10528 "Parser/parser.cc"
    break;

  case 781:
#line 3002 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 10534 "Parser/parser.cc"
    break;

  case 782:
#line 3004 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10540 "Parser/parser.cc"
    break;

  case 783:
#line 3006 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10546 "Parser/parser.cc"
    break;

  case 784:
#line 3011 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10552 "Parser/parser.cc"
    break;

  case 785:
#line 3013 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10558 "Parser/parser.cc"
    break;

  case 786:
#line 3015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10564 "Parser/parser.cc"
    break;

  case 787:
#line 3017 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10570 "Parser/parser.cc"
    break;

  case 788:
#line 3022 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10576 "Parser/parser.cc"
    break;

  case 789:
#line 3024 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10582 "Parser/parser.cc"
    break;

  case 790:
#line 3026 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10588 "Parser/parser.cc"
    break;

  case 791:
#line 3028 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10594 "Parser/parser.cc"
    break;

  case 792:
#line 3030 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10600 "Parser/parser.cc"
    break;

  case 793:
#line 3032 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10606 "Parser/parser.cc"
    break;

  case 797:
#line 3050 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 10612 "Parser/parser.cc"
    break;

  case 798:
#line 3052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10618 "Parser/parser.cc"
    break;

  case 799:
#line 3054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 10624 "Parser/parser.cc"
    break;

  case 800:
#line 3056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10630 "Parser/parser.cc"
    break;

  case 801:
#line 3058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10636 "Parser/parser.cc"
    break;

  case 802:
#line 3063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10642 "Parser/parser.cc"
    break;

  case 803:
#line 3065 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10648 "Parser/parser.cc"
    break;

  case 804:
#line 3067 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10654 "Parser/parser.cc"
    break;

  case 805:
#line 3069 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10660 "Parser/parser.cc"
    break;

  case 806:
#line 3074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10666 "Parser/parser.cc"
    break;

  case 807:
#line 3076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10672 "Parser/parser.cc"
    break;

  case 808:
#line 3078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10678 "Parser/parser.cc"
    break;

  case 809:
#line 3080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10684 "Parser/parser.cc"
    break;

  case 810:
#line 3082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10690 "Parser/parser.cc"
    break;

  case 811:
#line 3084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10696 "Parser/parser.cc"
    break;

  case 812:
#line 3099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "ID" );
		}
#line 10705 "Parser/parser.cc"
    break;

  case 813:
#line 3104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10711 "Parser/parser.cc"
    break;

  case 814:
#line 3109 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10717 "Parser/parser.cc"
    break;

  case 816:
#line 3112 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10723 "Parser/parser.cc"
    break;

  case 817:
#line 3114 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10729 "Parser/parser.cc"
    break;

  case 818:
#line 3119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10735 "Parser/parser.cc"
    break;

  case 819:
#line 3121 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10741 "Parser/parser.cc"
    break;

  case 820:
#line 3123 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10747 "Parser/parser.cc"
    break;

  case 821:
#line 3125 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10753 "Parser/parser.cc"
    break;

  case 822:
#line 3130 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 10759 "Parser/parser.cc"
    break;

  case 823:
#line 3132 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10765 "Parser/parser.cc"
    break;

  case 824:
#line 3134 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10771 "Parser/parser.cc"
    break;

  case 825:
#line 3136 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10777 "Parser/parser.cc"
    break;

  case 826:
#line 3138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10783 "Parser/parser.cc"
    break;

  case 827:
#line 3140 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10789 "Parser/parser.cc"
    break;

  case 828:
#line 3142 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10795 "Parser/parser.cc"
    break;

  case 829:
#line 3147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10801 "Parser/parser.cc"
    break;

  case 830:
#line 3149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10807 "Parser/parser.cc"
    break;

  case 831:
#line 3151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 10813 "Parser/parser.cc"
    break;

  case 832:
#line 3153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10819 "Parser/parser.cc"
    break;

  case 833:
#line 3155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10825 "Parser/parser.cc"
    break;

  case 834:
#line 3165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10831 "Parser/parser.cc"
    break;

  case 835:
#line 3167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10837 "Parser/parser.cc"
    break;

  case 837:
#line 3170 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10843 "Parser/parser.cc"
    break;

  case 838:
#line 3172 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10849 "Parser/parser.cc"
    break;

  case 839:
#line 3177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10855 "Parser/parser.cc"
    break;

  case 840:
#line 3179 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10861 "Parser/parser.cc"
    break;

  case 841:
#line 3181 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10867 "Parser/parser.cc"
    break;

  case 842:
#line 3186 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 10873 "Parser/parser.cc"
    break;

  case 843:
#line 3188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10879 "Parser/parser.cc"
    break;

  case 844:
#line 3190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10885 "Parser/parser.cc"
    break;

  case 845:
#line 3192 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10891 "Parser/parser.cc"
    break;

  case 846:
#line 3197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10897 "Parser/parser.cc"
    break;

  case 847:
#line 3199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10903 "Parser/parser.cc"
    break;

  case 848:
#line 3201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10909 "Parser/parser.cc"
    break;

  case 849:
#line 3215 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10915 "Parser/parser.cc"
    break;

  case 850:
#line 3217 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10921 "Parser/parser.cc"
    break;

  case 852:
#line 3220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10927 "Parser/parser.cc"
    break;

  case 853:
#line 3222 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10933 "Parser/parser.cc"
    break;

  case 854:
#line 3227 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 10939 "Parser/parser.cc"
    break;

  case 855:
#line 3229 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 10945 "Parser/parser.cc"
    break;

  case 856:
#line 3234 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10951 "Parser/parser.cc"
    break;

  case 857:
#line 3236 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10957 "Parser/parser.cc"
    break;

  case 858:
#line 3238 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10963 "Parser/parser.cc"
    break;

  case 859:
#line 3243 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 10969 "Parser/parser.cc"
    break;

  case 860:
#line 3245 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10975 "Parser/parser.cc"
    break;

  case 861:
#line 3250 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10981 "Parser/parser.cc"
    break;

  case 862:
#line 3252 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10987 "Parser/parser.cc"
    break;

  case 864:
#line 3270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10993 "Parser/parser.cc"
    break;

  case 865:
#line 3272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10999 "Parser/parser.cc"
    break;

  case 866:
#line 3277 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( 0, (yyvsp[0].op) ); }
#line 11005 "Parser/parser.cc"
    break;

  case 867:
#line 3279 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 11011 "Parser/parser.cc"
    break;

  case 868:
#line 3281 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11017 "Parser/parser.cc"
    break;

  case 869:
#line 3283 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11023 "Parser/parser.cc"
    break;

  case 870:
#line 3285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11029 "Parser/parser.cc"
    break;

  case 872:
#line 3291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11035 "Parser/parser.cc"
    break;

  case 873:
#line 3293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11041 "Parser/parser.cc"
    break;

  case 874:
#line 3295 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11047 "Parser/parser.cc"
    break;

  case 875:
#line 3300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 11053 "Parser/parser.cc"
    break;

  case 876:
#line 3302 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11059 "Parser/parser.cc"
    break;

  case 877:
#line 3304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11065 "Parser/parser.cc"
    break;

  case 878:
#line 3310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false ); }
#line 11071 "Parser/parser.cc"
    break;

  case 879:
#line 3312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false )->addArray( (yyvsp[0].decl) ); }
#line 11077 "Parser/parser.cc"
    break;

  case 880:
#line 3314 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-4].en), 0, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].en), 0, false ) ); }
#line 11083 "Parser/parser.cc"
    break;

  case 882:
#line 3321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), 0, false ); }
#line 11089 "Parser/parser.cc"
    break;

  case 883:
#line 3323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 11095 "Parser/parser.cc"
    break;

  case 884:
#line 3325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newArray( (yyvsp[-2].en), 0, false ) ); }
#line 11101 "Parser/parser.cc"
    break;

  case 885:
#line 3327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 11107 "Parser/parser.cc"
    break;

  case 887:
#line 3362 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 11113 "Parser/parser.cc"
    break;

  case 888:
#line 3364 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11119 "Parser/parser.cc"
    break;

  case 889:
#line 3366 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11125 "Parser/parser.cc"
    break;

  case 890:
#line 3371 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].op) ); }
#line 11131 "Parser/parser.cc"
    break;

  case 891:
#line 3373 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 11137 "Parser/parser.cc"
    break;

  case 892:
#line 3375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 11143 "Parser/parser.cc"
    break;

  case 893:
#line 3377 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11149 "Parser/parser.cc"
    break;

  case 894:
#line 3379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11155 "Parser/parser.cc"
    break;

  case 896:
#line 3385 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11161 "Parser/parser.cc"
    break;

  case 897:
#line 3387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11167 "Parser/parser.cc"
    break;

  case 898:
#line 3389 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11173 "Parser/parser.cc"
    break;

  case 899:
#line 3394 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 11179 "Parser/parser.cc"
    break;

  case 900:
#line 3396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11185 "Parser/parser.cc"
    break;

  case 901:
#line 3398 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11191 "Parser/parser.cc"
    break;

  case 903:
#line 3405 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11197 "Parser/parser.cc"
    break;

  case 905:
#line 3416 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false ); }
#line 11203 "Parser/parser.cc"
    break;

  case 906:
#line 3419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 11209 "Parser/parser.cc"
    break;

  case 907:
#line 3421 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, (yyvsp[-2].decl), false ); }
#line 11215 "Parser/parser.cc"
    break;

  case 908:
#line 3424 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 11221 "Parser/parser.cc"
    break;

  case 909:
#line 3426 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 11227 "Parser/parser.cc"
    break;

  case 910:
#line 3428 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-4].decl), true ); }
#line 11233 "Parser/parser.cc"
    break;

  case 912:
#line 3443 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11239 "Parser/parser.cc"
    break;

  case 913:
#line 3445 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11245 "Parser/parser.cc"
    break;

  case 914:
#line 3450 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( 0, (yyvsp[0].op) ); }
#line 11251 "Parser/parser.cc"
    break;

  case 915:
#line 3452 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 11257 "Parser/parser.cc"
    break;

  case 916:
#line 3454 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11263 "Parser/parser.cc"
    break;

  case 917:
#line 3456 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11269 "Parser/parser.cc"
    break;

  case 918:
#line 3458 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11275 "Parser/parser.cc"
    break;

  case 920:
#line 3464 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11281 "Parser/parser.cc"
    break;

  case 921:
#line 3466 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11287 "Parser/parser.cc"
    break;

  case 922:
#line 3468 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11293 "Parser/parser.cc"
    break;

  case 923:
#line 3473 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11299 "Parser/parser.cc"
    break;

  case 924:
#line 3475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11305 "Parser/parser.cc"
    break;

  case 927:
#line 3485 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11311 "Parser/parser.cc"
    break;

  case 930:
#line 3496 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11317 "Parser/parser.cc"
    break;

  case 931:
#line 3498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11323 "Parser/parser.cc"
    break;

  case 932:
#line 3500 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11329 "Parser/parser.cc"
    break;

  case 933:
#line 3502 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11335 "Parser/parser.cc"
    break;

  case 934:
#line 3504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11341 "Parser/parser.cc"
    break;

  case 935:
#line 3506 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11347 "Parser/parser.cc"
    break;

  case 936:
#line 3513 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11353 "Parser/parser.cc"
    break;

  case 937:
#line 3515 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11359 "Parser/parser.cc"
    break;

  case 938:
#line 3517 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11365 "Parser/parser.cc"
    break;

  case 939:
#line 3519 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 11371 "Parser/parser.cc"
    break;

  case 940:
#line 3521 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11377 "Parser/parser.cc"
    break;

  case 941:
#line 3524 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11383 "Parser/parser.cc"
    break;

  case 942:
#line 3526 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11389 "Parser/parser.cc"
    break;

  case 943:
#line 3528 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11395 "Parser/parser.cc"
    break;

  case 944:
#line 3530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 11401 "Parser/parser.cc"
    break;

  case 945:
#line 3532 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11407 "Parser/parser.cc"
    break;

  case 946:
#line 3537 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 11413 "Parser/parser.cc"
    break;

  case 947:
#line 3539 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 11419 "Parser/parser.cc"
    break;

  case 948:
#line 3544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 11425 "Parser/parser.cc"
    break;

  case 949:
#line 3546 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl)->addQualifiers( (yyvsp[-4].decl) ), true ); }
#line 11431 "Parser/parser.cc"
    break;

  case 951:
#line 3573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11437 "Parser/parser.cc"
    break;

  case 955:
#line 3584 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11443 "Parser/parser.cc"
    break;

  case 956:
#line 3586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11449 "Parser/parser.cc"
    break;

  case 957:
#line 3588 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11455 "Parser/parser.cc"
    break;

  case 958:
#line 3590 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11461 "Parser/parser.cc"
    break;

  case 959:
#line 3592 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11467 "Parser/parser.cc"
    break;

  case 960:
#line 3594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11473 "Parser/parser.cc"
    break;

  case 961:
#line 3601 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11479 "Parser/parser.cc"
    break;

  case 962:
#line 3603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11485 "Parser/parser.cc"
    break;

  case 963:
#line 3605 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11491 "Parser/parser.cc"
    break;

  case 964:
#line 3607 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11497 "Parser/parser.cc"
    break;

  case 965:
#line 3609 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11503 "Parser/parser.cc"
    break;

  case 966:
#line 3611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11509 "Parser/parser.cc"
    break;

  case 967:
#line 3616 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 11515 "Parser/parser.cc"
    break;

  case 968:
#line 3618 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11521 "Parser/parser.cc"
    break;

  case 969:
#line 3620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11527 "Parser/parser.cc"
    break;

  case 970:
#line 3627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 11533 "Parser/parser.cc"
    break;

  case 971:
#line 3629 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 11539 "Parser/parser.cc"
    break;

  case 974:
#line 3653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 11545 "Parser/parser.cc"
    break;

  case 975:
#line 3655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 11551 "Parser/parser.cc"
    break;


#line 11555 "Parser/parser.cc"

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
#line 3658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "make install" //
// End: //
