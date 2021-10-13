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
#define YYLAST   18977

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  175
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  289
/* YYNRULES -- Number of rules.  */
#define YYNRULES  977
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
    1196,  1198,  1200,  1202,  1204,  1210,  1211,  1217,  1218,  1240,
    1242,  1244,  1247,  1250,  1253,  1255,  1257,  1259,  1262,  1265,
    1267,  1270,  1277,  1279,  1281,  1283,  1285,  1290,  1292,  1294,
    1296,  1301,  1303,  1308,  1310,  1312,  1314,  1317,  1321,  1324,
    1328,  1330,  1332,  1334,  1336,  1338,  1340,  1342,  1344,  1346,
    1348,  1353,  1354,  1358,  1364,  1369,  1374,  1375,  1379,  1383,
    1388,  1389,  1395,  1399,  1401,  1403,  1405,  1408,  1410,  1415,
    1417,  1422,  1424,  1426,  1431,  1433,  1439,  1440,  1444,  1445,
    1446,  1447,  1451,  1456,  1457,  1459,  1461,  1463,  1467,  1471,
    1472,  1476,  1478,  1480,  1482,  1484,  1490,  1491,  1497,  1498,
    1502,  1503,  1508,  1510,  1516,  1517,  1519,  1524,  1529,  1540,
    1541,  1545,  1546,  1552,  1553,  1557,  1559,  1563,  1565,  1569,
    1570,  1574,  1575,  1579,  1580,  1581,  1585,  1587,  1602,  1603,
    1604,  1605,  1607,  1611,  1613,  1617,  1624,  1626,  1628,  1633,
    1634,  1636,  1638,  1640,  1672,  1675,  1680,  1682,  1688,  1693,
    1698,  1709,  1714,  1719,  1724,  1729,  1738,  1742,  1749,  1751,
    1752,  1753,  1759,  1761,  1766,  1767,  1768,  1777,  1778,  1779,
    1783,  1784,  1785,  1794,  1795,  1796,  1801,  1802,  1811,  1812,
    1817,  1818,  1822,  1824,  1826,  1828,  1830,  1834,  1839,  1840,
    1842,  1852,  1853,  1858,  1860,  1862,  1864,  1866,  1869,  1871,
    1873,  1878,  1880,  1882,  1884,  1886,  1888,  1890,  1892,  1894,
    1896,  1898,  1900,  1902,  1904,  1906,  1908,  1910,  1912,  1914,
    1916,  1918,  1920,  1922,  1924,  1926,  1928,  1930,  1932,  1937,
    1938,  1942,  1949,  1950,  1956,  1957,  1959,  1961,  1963,  1968,
    1970,  1975,  1976,  1978,  1980,  1985,  1987,  1989,  1991,  1993,
    1995,  2000,  2001,  2003,  2005,  2010,  2012,  2011,  2015,  2023,
    2024,  2026,  2028,  2033,  2034,  2036,  2041,  2042,  2044,  2046,
    2051,  2052,  2054,  2059,  2061,  2063,  2065,  2066,  2068,  2073,
    2075,  2077,  2082,  2083,  2087,  2088,  2093,  2092,  2097,  2096,
    2104,  2103,  2114,  2113,  2123,  2128,  2129,  2134,  2140,  2154,
    2155,  2159,  2161,  2163,  2169,  2171,  2173,  2175,  2177,  2179,
    2181,  2183,  2189,  2190,  2195,  2197,  2199,  2208,  2210,  2211,
    2212,  2214,  2216,  2217,  2222,  2223,  2224,  2229,  2231,  2234,
    2241,  2242,  2243,  2249,  2254,  2256,  2262,  2263,  2269,  2270,
    2274,  2279,  2282,  2281,  2285,  2288,  2294,  2293,  2302,  2308,
    2312,  2314,  2319,  2321,  2323,  2325,  2331,  2334,  2340,  2341,
    2343,  2344,  2345,  2347,  2349,  2356,  2357,  2359,  2361,  2366,
    2367,  2373,  2374,  2376,  2377,  2382,  2383,  2384,  2386,  2394,
    2395,  2397,  2400,  2402,  2406,  2407,  2408,  2410,  2412,  2417,
    2419,  2424,  2426,  2435,  2437,  2442,  2443,  2444,  2448,  2449,
    2450,  2455,  2456,  2461,  2462,  2463,  2464,  2468,  2469,  2474,
    2475,  2476,  2477,  2478,  2492,  2493,  2498,  2499,  2505,  2507,
    2510,  2512,  2514,  2537,  2538,  2544,  2545,  2551,  2550,  2560,
    2559,  2563,  2569,  2575,  2576,  2578,  2582,  2587,  2589,  2591,
    2593,  2599,  2600,  2604,  2605,  2610,  2612,  2619,  2621,  2622,
    2624,  2629,  2631,  2633,  2638,  2640,  2645,  2650,  2658,  2660,
    2665,  2666,  2671,  2672,  2676,  2677,  2678,  2683,  2685,  2691,
    2693,  2698,  2700,  2706,  2707,  2711,  2715,  2719,  2721,  2722,
    2723,  2728,  2731,  2730,  2742,  2741,  2753,  2752,  2764,  2763,
    2777,  2783,  2785,  2791,  2792,  2797,  2804,  2809,  2815,  2818,
    2821,  2825,  2831,  2834,  2837,  2842,  2843,  2844,  2848,  2854,
    2855,  2865,  2866,  2870,  2871,  2876,  2881,  2882,  2888,  2889,
    2891,  2896,  2897,  2898,  2899,  2900,  2902,  2937,  2939,  2944,
    2946,  2947,  2949,  2954,  2956,  2958,  2960,  2965,  2967,  2969,
    2971,  2973,  2975,  2977,  2982,  2984,  2986,  2988,  2997,  2999,
    3000,  3005,  3007,  3009,  3011,  3013,  3018,  3020,  3022,  3024,
    3029,  3031,  3033,  3035,  3037,  3039,  3051,  3052,  3053,  3057,
    3059,  3061,  3063,  3065,  3070,  3072,  3074,  3076,  3081,  3083,
    3085,  3087,  3089,  3091,  3106,  3111,  3116,  3118,  3119,  3121,
    3126,  3128,  3130,  3132,  3137,  3139,  3141,  3143,  3145,  3147,
    3149,  3154,  3156,  3158,  3160,  3162,  3172,  3174,  3176,  3177,
    3179,  3184,  3186,  3188,  3193,  3195,  3197,  3199,  3204,  3206,
    3208,  3222,  3224,  3226,  3227,  3229,  3234,  3236,  3241,  3243,
    3245,  3250,  3252,  3257,  3259,  3276,  3277,  3279,  3284,  3286,
    3288,  3290,  3292,  3297,  3298,  3300,  3302,  3307,  3309,  3311,
    3317,  3319,  3321,  3324,  3328,  3330,  3332,  3334,  3368,  3369,
    3371,  3373,  3378,  3380,  3382,  3384,  3386,  3391,  3392,  3394,
    3396,  3401,  3403,  3405,  3411,  3412,  3414,  3423,  3426,  3428,
    3431,  3433,  3435,  3449,  3450,  3452,  3457,  3459,  3461,  3463,
    3465,  3470,  3471,  3473,  3475,  3480,  3482,  3490,  3491,  3492,
    3497,  3498,  3503,  3505,  3507,  3509,  3511,  3513,  3520,  3522,
    3524,  3526,  3528,  3531,  3533,  3535,  3537,  3539,  3544,  3546,
    3548,  3553,  3579,  3580,  3582,  3586,  3587,  3591,  3593,  3595,
    3597,  3599,  3601,  3608,  3610,  3612,  3614,  3616,  3618,  3623,
    3625,  3627,  3634,  3636,  3654,  3656,  3661,  3662
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
  "iteration_statement", "loop_default_opt", "for_control_expression_list",
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

#define YYPACT_NINF (-1711)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-858)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     170, 10993,   184,   230, 15254,   171, -1711, -1711, -1711, -1711,
   -1711, -1711, -1711, -1711, -1711, -1711, -1711,   157,   726,   299,
   -1711, -1711, -1711, -1711, -1711, -1711, -1711, -1711, -1711, -1711,
   -1711, -1711, -1711, -1711, -1711, -1711, -1711, -1711, -1711, -1711,
   -1711, -1711, -1711, -1711, -1711, -1711, -1711,    54,   465, -1711,
   -1711, -1711, -1711, -1711, -1711,  4603,  4603,   321, 10993,   344,
     416, -1711, -1711,   434, -1711, -1711, -1711, -1711, -1711, -1711,
   -1711, -1711, -1711,  2402, -1711,   742,   438, -1711, -1711, -1711,
   -1711, -1711, 15104, -1711, -1711,   415,   450,   395,   276, -1711,
    4603,   450,   450,   450,   459,  4469,   633,   672, 11152, -1711,
   -1711, -1711, 14954,   993, -1711, -1711, -1711,  2341,   644,  5023,
     985,   813,  2341,   827,   509, -1711, -1711, -1711, -1711,   615,
   -1711, -1711, -1711, -1711,   538, -1711, -1711, -1711, -1711, -1711,
     577,   572,   615, -1711,   615,   617, -1711, -1711, -1711, 15810,
    4603, -1711, -1711,  4603, -1711, 10993,   559, 15862, -1711, -1711,
    4545, 16874, -1711,   745,   745, -1711,  2172, -1711, -1711, -1711,
   -1711,    31, 13564,  3344,   615, -1711, -1711, -1711, -1711, -1711,
   -1711,   628, -1711,   612,   642,   689, -1711,   738, 18452, 14184,
    2619,  2402,   670,   705,   714,   750,   776,   792,   798, -1711,
   -1711, 16012, 10344,   728, -1711, 15397, -1711, -1711, -1711, -1711,
     744, -1711, -1711,   779, -1711,  9470,   933, 17804, -1711,   808,
    4603,   572,   815,   807,   822,   825, -1711, -1711, -1711,  3538,
    3170,   830,   891,   140, -1711, -1711,   615,   615,   -16,   106,
     374,   -16, -1711,   615,   615, -1711,  3867, -1711, -1711,   857,
     870,   745, 13142, -1711, -1711, 15104, -1711, -1711,  2341, -1711,
    1417,   509,   879,   935,   106,  4603,   395, -1711, 12558, -1711,
     745,   745,   884,   935,   106,  4603, -1711, 12450, -1711, -1711,
     745, -1711,   745, -1711,   794,  3114,  4603, -1711,  2087,   906,
   -1711, -1711, -1711, 15556,   572,   111, -1711, -1711, 16924, -1711,
     891,    77, -1711, 18452, 16874,  3765,  3867, -1711,   385, -1711,
   -1711, -1711, 15862,  4603,   943, -1711, -1711, -1711, -1711,  4603,
    3197,   306,   600, -1711,  4603,   612, -1711,   789,   615,   873,
   16064,   647, 13722, 13300,  2341,  2341, -1711,  2341,   745,  2341,
     745, -1711, -1711,   615, -1711,   919, -1711, 16214, -1711, -1711,
   -1711, 16266,   744, -1711,   950,   101,  1526,   971,   509,   989,
   -1711,  2172,   908,   612,  2172,  2100, -1711,   947,  1019, 18524,
    1024,  1037, 18452, 18596,  1042, -1711, -1711, -1711, -1711, -1711,
   -1711, -1711, 18668, 18668, 14030,   970,  4660, -1711, -1711, -1711,
   -1711,  1030, -1711,  1046, -1711,   890, -1711, 18452, 18452, -1711,
    1048,   575,   889,   938,   553,   976,  1007,  1057,  1047,  1091,
      52, -1711,   654, -1711,  1076, -1711,   958,  4418, 14492, -1711,
   -1711,   325,  1076, -1711, -1711,   720, -1711, -1711,  2619,  1083,
    1093,  1097,  1107,  1111,  1113, -1711, -1711,   387,  1084, -1711,
     736,  1084, -1711, -1711, 15810, -1711,   961,  1088, 14646, -1711,
   -1711,  4237,  4155,  1121, 13722,  1146,   701,  1169, -1711, -1711,
   -1711, -1711, -1711,  4603,  4345, -1711, -1711, -1711, -1711, -1711,
   -1711,  1118,  4167,   970,  9470,  1126,  1128, -1711, -1711,  1138,
   17804,   706, -1711, -1711, -1711, 17876,  1119, -1711, -1711, -1711,
   -1711, -1711,  3538,   587,  1155,  1162,  1174,   661,  1179,  1198,
    1204,  3170, -1711, -1711,   615,  1139,   395,  1145, -1711, -1711,
    1165, -1711, -1711,   572,   935, -1711, -1711, -1711,   572, -1711,
   -1711,  3867, -1711, 14492, 14492, -1711,   745,  4545, 17652, 13564,
   -1711, -1711, -1711, -1711, -1711,   572,   935,    77, -1711, -1711,
    2341,  1151,   935,   106, -1711,   572,   935, -1711, 13930, -1711,
     745,   745, -1711, -1711,  1190,   179,  1206,   509,  1208, -1711,
   17082, -1711,   739, -1711,  1293, 17548, -1711,  4545, 16425, 13142,
   -1711, 15556, 18740, -1711, -1711, -1711, -1711, -1711,  3765,   715,
    3867, -1711, 13564,   891, -1711,  1225, -1711,  1232, -1711, -1711,
   -1711, -1711, -1711,  2172, -1711, -1711,  1306,  3740, 16266, 10344,
   -1711, 16477, -1711,   745,   745, -1711, -1711,   744, -1711,   755,
    1229,  1367, 18452,  1921,  1165,  1212, -1711,   615,   615, -1711,
    1084, -1711, 16064, -1711, -1711, 17363,   745,   745, -1711,  3740,
     615, -1711, 16731, -1711, -1711, 16214, -1711,    31,  1233,   239,
    1230,  1526,   740, 15862,   758, -1711, -1711, -1711, -1711, -1711,
   -1711,   766, -1711,  1241,  1217, -1711, 14338, -1711, 16529, 16529,
   -1711, 14338, -1711, 18452, 14338, -1711, -1711, 15608, 16529, 16529,
     958,  1437,  1507,   535,  1604, -1711,   768,  1251,   962,  1252,
   -1711, 17876, 18452, 17948,  1235,  2087,  2087, -1711,  2592, -1711,
   -1711, 18020,  2385, 18452, 18020,  2087, -1711, -1711, 18452, 18452,
   18452, 18452, 18452, 18452, 18452, 18452, 18452, 18452, 18452, 18452,
   18452, 18452, 18452, 18452, 18452, 18452, 18452, 18092,  1231,   738,
    2300, 10344, -1711, -1711, -1711, -1711, -1711, -1711, -1711, -1711,
   -1711, -1711, -1711,  1253, 18452, -1711, -1711,   325,  2002, -1711,
   -1711,   615,   615, -1711, -1711, 14492, -1711,   425,  1084, -1711,
     775,  1084, -1711, -1711, -1711,  1165, -1711, -1711,  1165, 18812,
   -1711, -1711, 10344,  1254,  1257,  2707,  1396,  3525,   428,  1212,
   -1711,   615,   615,  1212,   431, -1711,   615,   615, 18452,  4603,
     986,   994,  1212,    89, 13090, 13090,  4603, -1711, -1711, 18452,
    1138, -1711,  9470,  1267, -1711,  1288, -1711, -1711, -1711, -1711,
   -1711,   778, -1711, 13090,  2087,  4545,  2087,   783,  1266,  1268,
    1271,   797,  1272,  1276,  1277,   455,  1084, -1711, -1711,   461,
    1084, -1711, -1711, -1711,  4545,   738, -1711,  1084, 18812, -1711,
     572, 17082, -1711, -1711,   800,  1278,   805,  1285, -1711,  1273,
   -1711,   572, -1711, -1711,   572,   935,  1273, -1711,   572,  1283,
    1287,  1289, -1711, -1711, 17363, -1711,  1294, -1711, -1711, -1711,
    2087,  4603,  9838,  1379,  1282, 17450, -1711,  1088, -1711, 13090,
     816, -1711,  1273, -1711, 15862, 14492,  1284, -1711,  1284, -1711,
   -1711, -1711, -1711, 16214, -1711, 10506, 14800, -1711, 17082,  1310,
    1311,  1312, -1711,  8878,   615, -1711,  1921, -1711, -1711, -1711,
   -1711,  1165, -1711, -1711, -1711,   745, -1711,  3359, -1711, -1711,
     509,  1991,  1316, -1711, 17804, -1711,  1526,  1233, -1711, -1711,
    1308,  1317,  2100, 18020, -1711,  1318,   438,  1315,  1330,  1332,
    1329,  1334, 18452,  1335,  1336,  1339, 10344, 18452, -1711, -1711,
    1627, -1711, -1711, -1711, 18452, -1711,  1341,  1342, 17660,  1002,
   -1711, 18020, -1711, -1711, -1711,  3490, -1711, -1711,   819, -1711,
   -1711, -1711, -1711,  3490, -1711, -1711,  1008,   407, -1711, -1711,
    1048,  1048,  1048,   575,   575,   889,   889,   938,   938,   938,
     938,   553,   553,   976,  1007,  1057,  1047,  1091, 18452,  1010,
   -1711,  1343,  3490, -1711, -1711,  9470, -1711, 17082,  1344,  1346,
    1348,  2002, -1711, -1711, -1711, -1711, -1711, -1711, -1711, -1711,
    1165, -1711, -1711,  1165, 17082, 17082, -1711, -1711,  2707,   721,
    1349,  1351,  1356,  1357,  2645,  3525, -1711, -1711, -1711, -1711,
   -1711, -1711, -1711, -1711, -1711, -1711, -1711, -1711, -1711, -1711,
    1358, -1711,  1212, -1711, -1711, -1711, -1711, -1711, -1711, -1711,
   -1711,  1359,  1360, -1711,   395,  3490,  1039,     2, -1711, -1711,
    1368, -1711, 17804, -1711, 18452, -1711, 18164, 13090, -1711, -1711,
   -1711,  1352,   462,  1084, -1711,   490,  1084, -1711, -1711, -1711,
   -1711,  1165, -1711, -1711, -1711,  1165,   891,  1364,  1165, -1711,
   -1711, -1711, -1711, -1711, -1711, -1711,  1380, -1711, -1711,  1273,
   -1711,   572, -1711, -1711, -1711, -1711, -1711, 11782,  1382,  1375,
   -1711,   363, -1711,   223,   192, 10182,  1387, 12919,  1388,  1392,
    2813,  2908,  3049, 18236,  1394, -1711, -1711,  1395,  1400, -1711,
   -1711,   572, 18452, 18452,  1518,  1389,   552, -1711,  1478,  1397,
    1377, -1711, -1711, -1711,  9666, -1711, -1711, -1711, -1711, -1711,
    2263, -1711, -1711, -1711,  1469, -1711, -1711, -1711,  2087, -1711,
   -1711, 11629, 15104,  1406, -1711,  4603, -1711,  1390,  1411,  1413,
   -1711,  1086, -1711, -1711, -1711,  4545, -1711, -1711,  1398,  1399,
     821, 15862,   612,   612, -1711, -1711,   970,  1088, 14646, -1711,
    1076, -1711, 10668, -1711,   500,  1084, -1711,   745,  9053, -1711,
   -1711,  1526,   615,   615,    31,   239, -1711, -1711,  1233,  1420,
    1421, -1711, -1711,   854,   513, 10344,  2087, -1711,   513, 15660,
     513, -1711, 18452, 18452, 18452, -1711, -1711, -1711, -1711, 18452,
   18452,  1422,  9470, -1711, -1711,  1415,   555, -1711,  3625, -1711,
   -1711,  1092, -1711,     6, -1711, 18020,  1096, -1711, 17876, -1711,
   -1711, 18452,  1407,  1101,  1112,  1138, -1711,   503,  1084, -1711,
   -1711, 17082, 17082, -1711, -1711,  1423,   547,  1084, -1711,   558,
     977,   615,   615, -1711, -1711, 17082, 17082, -1711,  1426, -1711,
   13564, 13564,  1431,  1429,  1432,  1435, -1711,  1433, 18452, 18452,
    1115,  1436, -1711, -1711, -1711, -1711, -1711, -1711,  1440, 18452,
   -1711, -1711, -1711,  1165, -1711, -1711, -1711,  1165, 17082, 17082,
     395,   615,  1125,  1442,  1446, -1711, -1711,  1448, 11935, 12088,
   12241, 15862, 16529, 16529,  1450, -1711,  1434,  1438,  2313,  8709,
   -1711,   369,  4603, -1711, -1711,  4603, -1711, 17732,   286,   402,
   -1711, -1711, -1711, -1711, 18452,  1454,  1528,  1459,  1460, -1711,
    1439, -1711,  1449, 18452,  1451,  9470,  1455, 18452, 17876, 18452,
    1114, -1711,  1473,    65, -1711,    -6,  1465, -1711, -1711,  1501,
   -1711,  1476, -1711,  1477,  1505, 12919,   667, 12716,   615,   435,
   -1711, -1711, -1711,  1504, -1711,  1508, -1711,  1509, -1711,  1503,
   -1711,  1506, -1711, -1711, -1711, -1711, 10830,  1510,  1511,  1513,
   -1711,  1512, -1711, -1711, -1711,  1165, 18452, 18452,  1088,  1514,
   -1711,  1233, -1711,  1515,   420, -1711,  1521, -1711, -1711, 15862,
   -1711,  1520,  1519,   865, -1711,  1527, -1711, -1711, -1711, -1711,
   -1711,  9470,  1138, 17876, -1711,  1558,  3490, -1711,  1558,  1558,
   -1711,  3490,  4043,  4624, -1711, -1711,  1132, -1711, -1711, -1711,
    1529,  1531, -1711, -1711, -1711,  1165, -1711, -1711,  1533,  1535,
     615, -1711, -1711, -1711,  1165, -1711, -1711, -1711,  1536, -1711,
   -1711, -1711, -1711, -1711, -1711, -1711, -1711, -1711, -1711, -1711,
   -1711, -1711,  1539, -1711, -1711, -1711, -1711,  1540,  1538,   615,
   -1711, 17082, 17082, -1711, -1711, -1711, -1711, 18452, -1711, -1711,
    1542, -1711,  1450,  1450,  1450,   777,  1522,   442, -1711,  4329,
     451, 14492, -1711, -1711, -1711,  3880, 18452,  4009,   457, -1711,
   -1711,    48,  1544,  1544,  4603, -1711, -1711, 17231, -1711, 18452,
    1548,  1560, -1711, -1711, -1711, -1711,   866,  1546, 12919, 10182,
   12919, 10010, -1711, -1711,   464, -1711,  1138, -1711,   883,   895,
     912, -1711, -1711, -1711, -1711,   572,  1114,  1563, -1711, -1711,
   18452, -1711,  1566,   738, 10182, -1711, -1711, -1711, -1711, 18452,
    1602, -1711, 12919, -1711,   615, 13564, -1711, -1711, 15862, -1711,
   -1711, -1711, -1711, -1711,  1564, -1711, 17082, -1711, -1711,  1570,
   -1711,  1571,  1569,  1572,  1526, -1711, -1711, -1711, -1711, 18452,
   -1711, 15660, 18452,  1138,  1578,  1137, -1711,  1141, -1711,  3490,
   -1711,  3490, -1711, -1711, -1711, -1711, 17082,  1577,  1580, -1711,
   -1711, 17082, 17082,  1586,  1587,  1143, 13248, 13406, -1711,  1589,
   -1711, -1711, -1711, -1711,  1590,  1594,  1148, -1711, -1711, -1711,
   -1711,   777,  2169,   467, -1711, -1711, -1711, -1711,   615,   615,
   -1711, -1711, -1711,   514, -1711,   913,  3880,   682, -1711,  4009,
     615, -1711, -1711, -1711, -1711, -1711, -1711, -1711, -1711,   521,
   12919,   346, 18308, -1711,  1397,  1595, 18452,   415,  1565,   459,
   12400, 15862, -1711, 18452, 18452,  1032,   593, -1711, 18452, -1711,
    1592,   351, 12919, -1711, -1711,  1598, -1711, -1711,  1582,   738,
     616,  1601,  1603,  1149,  1669, -1711, -1711, -1711,  4603,  4545,
   -1711, -1711,  1605,  1606, -1711, -1711, -1711,  1526,  1233,  1613,
   -1711, -1711, -1711,  1614, -1711, -1711, -1711,  1157,  1161, -1711,
   -1711, -1711, -1711, -1711, -1711, -1711, -1711, -1711, -1711,  1612,
   -1711, -1711,  1615,  1616, -1711, -1711, -1711,  1617,  1618,  1619,
    2169, -1711,   615, -1711, -1711, -1711, -1711, -1711,  1620,  4329,
   -1711, -1711, 18452,  1609, -1711, -1711,  9391, -1711,  1600,   918,
    1696,  1397, 13880,  1397,  1608, -1711, -1711, -1711, -1711,  9254,
   18452,  1696, 10010,  1610,  1623, -1711, -1711, -1711, -1711, 16679,
   -1711,  1621,  1624,   241, 12919, -1711, 18452, 18020,   439, -1711,
   -1711, -1711,  1628, -1711, -1711,  1233,  1632, -1711, -1711, -1711,
   -1711,  1635,  1637,  1640, 13564,  1638, -1711, -1711,   578,  1084,
   -1711, -1711,   777, -1711,   228, -1711,  1167, -1711, -1711, 11311,
   -1711, -1711, -1711,  1696, 12919, 12919, 18452,  1642, 18452,  1064,
    1625,   218, 12919, -1711, 18452, -1711, 11311, 16679, -1711,  4362,
   16477,  2087,  1636, -1711,  1700,  1654,   622,  1649, -1711,  1733,
   -1711,   924, 12919,  1659, 12919, 12919, -1711,  1661, -1711, -1711,
   -1711, -1711, -1711, -1711, -1711, -1711,  1165, -1711, 18452, 18452,
   -1711,  1270, 11470, -1711,  1639,  1737, -1711, -1711,  1397,  1665,
    1668, 18452, 18452, 18452, -1711, -1711,  1270, -1711,  1645,  2951,
    3648, -1711, -1711, -1711,   241,  1657, 18452,  1646,   241,   241,
   12919, -1711, -1711, 18452,  1719,  1722, -1711, 17082, -1711, -1711,
    9391, -1711,  1270, -1711, -1711, -1711, -1711,  1667,  1672,   354,
   -1711,  1397, -1711, -1711,  1645, 18452,  1682,  3648,  1685,   738,
    1697, -1711,   648, -1711, -1711,   925,  1669,   332, -1711, -1711,
   12801,  1701,  9391, 18452, 18380, 18452,  1702,  1704, -1711,   572,
     738,  1703, -1711,  1683,   738, -1711, -1711, 12919,  1783,  1707,
   -1711, -1711, 12801,  1397, -1711,  1397,  1397, -1711,   572, -1711,
   -1711,  1196, 18452, -1711,   941, -1711, 12919, -1711, -1711,   738,
    2087,  1708,  1687, -1711, -1711, -1711,   944, -1711, -1711,  1689,
    2087, -1711, -1711
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,   406,     0,     2,   406,   423,   424,   425,   426,   427,
     428,   429,   430,   412,   414,   413,   415,     0,     0,     0,
     431,   433,   454,   434,   455,   437,   438,   452,   453,   432,
     450,   451,   435,   436,   439,   440,   441,   442,   443,   444,
     445,   446,   447,   448,   449,   456,   457,   741,   459,   532,
     533,   536,   538,   534,   540,     0,     0,     0,   406,     0,
       0,    16,   503,   509,     9,    10,    11,    12,    13,    14,
      15,   707,    93,     0,    19,     0,     2,    91,    92,    17,
      18,   757,   406,   708,   355,     0,   358,   633,   360,   369,
       0,   359,   389,   390,     0,     0,     0,     0,   486,   408,
     410,   416,   406,   418,   421,   471,   458,   394,   464,   469,
     395,   481,   396,   496,   500,   506,   485,   512,   524,   741,
     529,   530,   513,   579,   361,   362,     3,   709,   720,   411,
       0,     0,   741,   779,   741,     2,   796,   797,   798,   406,
       0,   955,   956,     0,     1,   406,     0,   406,   378,   379,
       0,   486,   400,   401,   402,   712,     0,   535,   537,   539,
     541,     0,   406,     0,   742,   743,   531,   460,   626,   627,
     625,   686,   681,   671,     0,     0,   710,     0,     0,   406,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   504,
     507,   406,   406,     0,   957,   486,   786,   804,   961,   954,
     952,   959,   354,     0,   155,   639,   154,     0,   363,     0,
       0,     0,     0,     0,     0,     0,   353,   856,   857,     0,
       0,   388,   739,   741,   735,   760,   741,   741,   737,     2,
     741,   736,   817,   741,   741,   814,     0,   479,   480,     0,
       0,   406,   406,   423,     2,   406,   370,   409,   419,   472,
       0,   501,     0,   723,     2,     0,   633,   371,   486,   465,
     482,   497,     0,   723,     2,     0,   422,   466,   473,   474,
     483,   488,   498,   502,     0,   516,     0,   701,     2,     2,
     721,   778,   780,   406,     0,     2,     2,   965,   486,   968,
     739,   739,     3,     0,   486,     0,     0,   381,   741,   737,
     736,     2,   406,     0,     0,   667,   669,   668,   670,     0,
       0,   663,     0,   653,     0,   662,   673,     0,   741,     2,
     406,   976,   407,   406,   418,   397,   464,   398,   489,   399,
     496,   493,   514,   741,   515,     0,   614,   406,   615,   930,
     931,   406,   616,   618,   503,   509,     0,   580,   581,     0,
     744,     0,   684,   672,     0,   748,    21,     0,    20,     0,
       0,     0,     0,     0,     0,    23,    25,     4,     8,     5,
       6,     7,     0,     0,   406,     2,     0,    94,    95,    96,
      97,    78,    24,    79,    36,    77,    98,     0,     0,   113,
     115,   119,   122,   125,   130,   133,   135,   137,   139,   141,
     143,   146,     0,    26,     0,   510,     2,    98,   406,   147,
     678,   629,   500,   631,   677,     0,   628,   632,     0,     0,
       0,     0,     0,     0,     0,   758,   784,   741,   794,   802,
     806,   812,     2,   963,   406,   966,     2,    91,   406,     3,
     613,     0,   976,     0,   407,   464,   489,   496,     3,     3,
     595,   599,   609,   615,   616,     2,   787,   805,   953,     2,
       2,    23,     0,     2,   639,    24,     0,   637,   640,   974,
       0,     0,   646,   635,   634,     0,     0,   725,     2,     2,
       2,     2,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   763,   820,   741,     0,   633,     2,   759,   767,
     883,   761,   762,     0,   723,     2,   816,   824,     0,   818,
     819,     0,   384,   406,   406,   470,   407,     0,   486,   406,
     958,   962,   960,   487,   705,     0,   723,   739,   364,   372,
     420,     0,   723,     2,   705,     0,   723,   682,   467,   468,
     484,   499,   505,   508,   503,   509,   527,   528,     0,   683,
     406,   623,     0,   191,   347,   406,     3,     0,   486,   406,
     722,   406,     0,   366,     2,   367,   702,   386,     0,     0,
       0,     2,   406,   739,   705,     0,     2,     0,   666,   665,
     664,   659,   417,     0,   657,   674,   462,     0,   406,   406,
     932,   407,   403,   404,   405,   936,   927,   928,   934,     2,
       2,    92,     0,   892,   906,   976,   888,   741,   741,   897,
     904,   621,   406,   494,   617,   407,   490,   491,   495,     0,
     741,   942,   407,   947,   939,   406,   944,     0,   974,   586,
       0,     0,     0,   406,     0,   756,   755,   751,   753,   754,
     752,     0,   746,   749,     0,    22,   406,    85,   406,   406,
      80,   406,    87,     0,   406,    83,    84,   406,   406,   406,
       2,    94,    95,     0,     0,   173,     0,     0,   530,     0,
     952,     0,     0,     0,     0,     0,     0,    46,     0,    52,
      53,    57,     0,     0,    57,     0,    81,    82,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   406,   156,   157,   158,   159,   160,   161,   162,   163,
     164,   165,   166,   154,     0,   152,   153,     2,   868,   630,
     865,   741,   741,   873,   511,   406,   785,   741,   795,   803,
     807,   813,     2,   788,   790,   792,     2,   808,   810,     0,
     964,   967,   406,     0,     0,     2,    92,   892,   741,   976,
     838,   741,   741,   976,   741,   853,   741,   741,     3,   617,
       0,     0,   976,   976,   406,   406,     0,     2,   648,     0,
     974,   645,   975,     0,   641,     0,     2,   644,   647,   170,
     169,     0,     2,   406,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   741,   772,   776,   815,   741,
     829,   834,   764,   821,     0,     0,   392,   880,     0,   726,
       0,   406,   727,   385,     0,     0,     0,     0,   383,     2,
     728,     0,   368,   705,     0,   723,     2,   729,     0,     0,
       0,     0,   542,   602,   407,     3,     3,   606,   605,   799,
       0,     0,   406,   348,     0,   486,     3,    91,     3,   406,
       0,     3,     2,   661,   406,   406,   655,   654,   655,   463,
     461,   580,   938,   406,   943,   407,   406,   929,   406,     0,
       0,     0,   907,     0,   741,   977,   893,   894,   622,   890,
     891,   905,   933,   937,   935,   492,   527,     0,   941,   946,
     583,   975,     0,   154,     0,   582,     0,   974,   687,   685,
       0,     0,   748,    57,   711,     0,     2,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   406,     0,   112,   111,
       0,   108,   107,    27,     0,    28,     0,     0,     0,     0,
       3,    57,    42,    43,    50,     0,    49,    61,     0,    58,
      59,    62,    45,     0,    44,    48,     0,     0,    41,   114,
     116,   117,   118,   120,   121,   123,   124,   128,   129,   126,
     127,   131,   132,   134,   136,   138,   140,   142,     0,     0,
     357,     0,     0,    29,     3,   639,   148,   406,     0,     0,
       0,   869,   870,   866,   867,   680,   679,     2,   789,   791,
     793,     2,   809,   811,   406,   406,   885,   884,     2,     0,
       0,     0,     0,     0,   741,   893,   841,   858,     2,   836,
     844,   619,   839,   840,   620,     2,   851,   861,   854,   855,
       0,     3,   976,   376,     2,   969,     2,   610,   611,   589,
       3,     3,     3,     3,   633,     0,   146,     0,     3,     3,
       0,   642,     0,   636,     0,   724,     0,   406,     3,   380,
     382,     0,   741,   773,   777,   741,   830,   835,     2,   765,
     768,   770,     2,   822,   825,   827,   739,     0,   881,     3,
     731,     3,   476,   475,   478,   477,     2,   706,   732,     2,
     730,     0,   706,   733,   542,   542,   542,   406,     0,     0,
     624,     0,   351,     0,     0,   406,     0,     2,     0,     0,
       0,     0,     0,   175,     0,   281,   282,     0,     0,   320,
     319,     0,   150,   150,   326,   503,   509,   189,     0,   176,
       0,   199,   177,   178,   406,   193,   179,   180,   181,   182,
       0,   183,   184,   287,     0,   185,   186,   187,     0,   188,
     195,   486,   406,     0,   197,     0,   345,     0,     0,     0,
       3,     0,   706,   694,   695,     0,     3,   690,     3,     3,
       0,   406,   671,   671,   940,   945,     2,    91,   406,     3,
     501,     3,   407,     3,   741,   900,   903,   406,     3,   889,
     895,     0,   741,   741,     0,   586,   571,   587,   974,     0,
       2,   745,   747,     0,    86,   406,     0,    90,    88,   406,
       0,   102,     0,     0,     0,   106,   110,   109,   174,     0,
       0,     0,   639,    99,   167,     0,     0,    75,     0,    75,
      75,     0,    63,    65,    40,     0,     0,    38,     0,    39,
     145,     0,     0,     0,     0,   974,     3,   741,   876,   879,
     871,   406,   406,     3,     3,     0,   741,   847,   850,   741,
       0,   741,   741,   842,   859,   406,   406,   970,     0,   612,
     406,   406,     0,     0,     0,     0,   365,     3,     0,     0,
       0,     0,   638,   643,     3,   172,   171,     3,     0,     0,
       2,   766,   769,   771,     2,   823,   826,   828,   406,   406,
     633,   741,     0,     0,     0,   706,   734,     0,   406,   406,
     406,   406,   406,   406,   525,   553,     3,     3,   554,   486,
     543,     0,     0,   781,     2,     0,   349,    57,     0,     0,
     272,   273,   196,   198,     0,     0,     0,     2,     2,   268,
       0,   266,     0,     0,     0,   639,     0,     0,     0,     0,
       0,   151,     0,     0,   327,     0,     0,     3,   202,     0,
     194,     0,   263,     0,     0,     2,     0,   486,   741,     0,
     346,   887,   886,     0,     2,     0,   697,     2,   692,     0,
     693,     0,   675,   656,   660,   658,   406,     0,     0,     0,
       3,     0,     2,   896,   898,   899,     0,     0,    91,     0,
       3,   974,   576,     0,   586,   584,     0,   574,   688,   406,
     750,     0,     0,     0,    32,     0,   103,   105,   104,   101,
     100,   639,   974,     0,    56,    72,     0,    66,    73,    74,
      51,     0,     0,     0,    60,    47,     0,   144,   356,    30,
       0,     0,     2,   872,   874,   875,     3,     3,     0,     0,
     741,     2,   843,   845,   846,     2,   860,   862,     0,   837,
     852,     3,     3,   971,     3,   597,   596,   600,   973,     2,
       2,   972,     0,     3,   738,   649,   650,     0,     0,   741,
     387,   406,   406,     3,     3,   393,   740,     0,   831,   715,
       0,   717,   525,   525,   525,   560,   530,     0,   566,   554,
       0,   406,   517,   552,   548,     0,     0,     0,     0,   555,
     557,   741,   568,   568,     0,   549,   564,   406,   352,     0,
       0,    58,   276,   277,   274,   275,     0,     0,     2,   406,
       2,   406,   269,   267,     0,   261,   974,   270,     0,     0,
       0,   308,   309,   310,   311,     0,   301,     0,   302,   278,
       0,   279,     0,     0,   406,   204,   192,   265,   264,     0,
     299,   318,     2,   350,   741,   406,   713,   676,   406,     2,
       2,   948,   949,   950,     0,   901,   406,     3,     3,     0,
     909,     0,     0,     0,     0,   585,   573,     3,    89,     0,
      31,   406,     0,   974,     0,     0,    76,     0,    64,     0,
      70,     0,    68,    37,   149,   877,   406,     0,     0,   782,
     800,   406,   406,     0,     0,     0,   406,   406,   652,     0,
     373,   375,     3,     3,     0,     0,     0,   719,   521,   523,
     519,     0,   916,     0,   561,   921,   563,   913,   741,   741,
     547,   567,   551,     0,   550,     0,     0,     0,   570,     0,
     741,   544,   558,   569,   559,   565,   604,   608,   607,     0,
       2,     0,     0,   230,   211,     0,     0,   213,   360,   212,
     486,   406,   234,     0,   175,   242,     0,   237,   175,   262,
       0,     0,     2,   285,   312,     0,   303,     2,     0,     0,
       0,     0,   290,     0,   286,   190,   374,   691,     0,     0,
     951,     3,     0,     0,   908,   910,   575,     0,   974,     2,
      35,    33,    34,     0,    54,   168,    67,     0,     0,     3,
     783,   801,     3,     3,   848,   863,   377,     2,   594,     3,
     593,   651,     0,     0,   774,   832,   882,     0,     0,     0,
     917,   918,   741,   546,   914,   915,   545,   526,     0,     0,
     203,   284,     0,     0,     2,   222,     2,   205,     0,     0,
     235,   214,   486,   243,     0,   258,   259,   260,   257,   246,
       0,   235,   406,     0,     0,     2,   207,   283,     2,   406,
     280,     0,     0,   328,     2,   288,     0,    57,     0,   300,
     696,   698,     0,   911,   912,   974,     0,   689,    55,    71,
      69,     0,     0,     0,   406,     0,   775,   833,   741,   924,
     926,   919,     0,   556,   215,   218,     0,   217,   221,   406,
     224,   223,   232,   235,     2,     2,   175,   251,     0,   247,
       0,   244,     2,   238,   175,   271,   406,   406,     3,   313,
     407,   317,     0,   321,     0,     0,     0,   329,   330,   209,
     291,     0,     2,     0,     2,     2,   902,     0,   578,   878,
     849,   864,   598,     2,   920,   922,   923,   562,     0,     0,
     220,   225,   406,   341,     0,     0,     3,   239,   248,   259,
     257,     0,   175,     0,     3,   241,   225,     3,   306,     0,
     916,   314,   315,   316,   328,     0,     0,     0,   328,     0,
       2,   289,   296,     0,   293,   295,   577,   406,   216,   219,
       2,     3,   226,   342,   231,   236,   229,   253,   252,   249,
     240,   245,   233,     3,   306,     0,     0,   917,     0,     0,
       0,   322,     0,   331,   210,     0,   286,     0,     3,   200,
     227,     0,     2,     0,     0,     0,     0,     0,   307,     0,
     334,     0,   332,     0,   334,   292,   294,     2,     0,     0,
     201,   206,   228,   255,   256,   254,   250,   208,     0,   304,
     335,     0,     0,   323,     0,   297,     2,   925,   305,     0,
       0,     0,     0,   298,   336,   337,     0,   333,   324,     0,
       0,   325,   338
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1711,  6037,  5909, -1711,    -1,   348,  1133,   -82, -1711,  1488,
   -1711,   274, -1711,  -668,   539,   632,  -681,  -885, -1711,   124,
    2279,  1732, -1711,  1214, -1711,  1207,   481,   665,   675,    67,
     671,  1171,  1166,  1170,  1175,  1178, -1711,  -147,  -177,  7899,
     762, -1711,  -359, -1711, -1711,  -644,  2875, -1028,  1585, -1711,
      14, -1711,   752,   -55, -1711, -1711, -1711,   336,    26, -1711,
   -1710, -1461,   212,    12, -1711, -1711, -1454, -1711,   128,    72,
   -1711, -1711, -1711, -1711,   -33, -1592,   117, -1711, -1711,   -30,
   -1711, -1711, -1711,   -17,   362,   368,    79, -1711, -1711, -1711,
   -1711,  -916, -1711,    20,   -34, -1711,    81, -1711,  -186, -1711,
   -1711, -1711,   770,  -801,  -910, -1148, -1711,    17,    40,     9,
    2684,  -837,  -710, -1711,  -269, -1711,    62,  -156,    24,  -233,
    -224,  3623,  6039,  -620, -1711,   173,   222,   423,   771, -1711,
    1873, -1711,   176,  3820, -1711, -1711, -1711,   131, -1711, -1711,
    1788,   185,  4401,  2518,   -38,  1679,  -280, -1711, -1711, -1711,
   -1711, -1711,  -282,  4600,  5310, -1711,  -351,   137, -1711,   432,
     181, -1711,   126,   627, -1711,   429,  -138, -1711, -1711, -1711,
    5332,  -616, -1143,  -494,  -384,  -413,  -664, -1711, -1140,  -151,
     -69,   968,   795,  7944,  -152,  -480,  -247,   -93,  -435,  1164,
   -1711,  1483,    64,  1081,  1361, -1711, -1711, -1711, -1711,   205,
    -141,    86,  -847, -1711,   277, -1711, -1711,   549,   391, -1711,
   -1711, -1711,  1960,  -715,  -457,  -967,     7, -1711, -1711, -1711,
   -1711, -1711,    25,  -766,   -75, -1694,  -198,  6968,   -68,  6703,
   -1711,  1050, -1711,  2916,  -140,  -211,  -179,  -166,     1,   -54,
     -53,   -47,   189,   -13,    -3,    18,  -157,   -83,  -155,  -145,
    -104,  -719,  -708,  -686,  -676,  -691,  -128,  -633, -1711, -1711,
    -679,  1237,  1238,  1239,   452,  7067,  -581,  -563,  -543,  -526,
    -547, -1711, -1520, -1543, -1541, -1531,  -582,  -123,  -228, -1711,
   -1711,   -37,     3,    49, -1711,  7763,   893,  -578,  -377
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,  1128,   213,   381,   382,    80,    81,   383,   358,   384,
    1413,  1414,   385,   948,   949,   950,  1231,  1232,  1233,  1425,
     407,   387,   388,   389,   663,   664,   390,   391,   392,   393,
     394,   395,   396,   397,   398,   399,   400,   409,  1047,   665,
    1352,   724,   207,   726,   403,   791,  1129,  1130,  1131,  1132,
    1133,  1134,  1135,  1940,  1136,  1137,  1357,  1665,  1815,  1816,
    1755,  1756,  1757,  1911,  1912,  1138,  1825,  1676,  1677,  1770,
    1139,  1140,  1141,  1142,  1143,  1144,  1365,  1693,  1855,  1789,
    1145,  1146,  1546,  1926,  1547,  1548,  1838,  1147,  1148,  1149,
    1355,  1846,  1847,  1848,  1971,  1986,  1871,  1872,   284,   285,
     852,   853,  1101,    83,    84,    85,    86,    87,  1668,   440,
      90,    91,    92,    93,    94,   221,   557,   442,   411,   443,
      97,   294,    99,   100,   101,   323,   324,   104,   105,   166,
     106,   870,   325,   152,   109,   241,   110,   153,   250,   327,
     328,   329,   154,   404,   115,   116,   331,   117,   548,   841,
     839,   840,  1502,   332,   333,   120,   121,  1097,  1320,  1508,
    1509,  1633,  1634,  1321,  1497,  1652,  1510,   122,   630,  1583,
     334,   628,   905,  1040,   448,   449,   845,   846,   450,   451,
     847,   336,   552,  1153,   413,   414,   208,   468,   469,   470,
     471,   472,   312,  1172,   313,   868,   866,   581,   314,   352,
     315,   316,   415,   124,   172,   173,   125,  1166,  1167,  1168,
    1169,     2,  1086,  1087,   829,  1304,   126,   304,   252,   262,
     531,   127,   211,   128,   222,  1049,   832,   498,   164,   129,
     641,   642,   643,   130,   224,   225,   226,   227,   299,   132,
     133,   134,   197,   136,   137,   138,   230,   300,   232,   233,
     234,   759,   760,   761,   762,   763,   235,   765,   766,   767,
     729,   730,   731,   732,   499,   139,   605,   606,   607,   608,
     609,   610,  1636,  1637,  1638,  1639,   595,   453,   339,   340,
     341,   416,   199,   141,   142,   143,   343,   783,   611
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      79,   402,   131,    79,   140,   181,   321,   140,   484,   528,
      89,   335,   231,   149,   556,   907,   957,   515,  1170,   183,
     184,   148,   887,   668,   496,    96,   185,   939,   150,   780,
     893,   401,   353,   825,   827,   349,   879,   190,  1016,   338,
     485,    88,   613,   504,   932,   198,  1821,  1010,   725,   992,
     902,  1150,  1405,   486,    79,    79,   880,    79,  1817,   131,
     186,   140,   487,    95,   488,   176,  1017,    89,   526,  1011,
     187,   209,    79,   881,   489,   297,  1552,   836,   536,  1012,
     492,    79,    96,   196,   484,  1346,  1154,   590,  1737,    79,
    1738,   188,   563,   565,    79,   357,   228,    79,    88,   253,
    1739,    79,  1788,   263,   621,   140,   194,   246,   624,   623,
     198,   257,  1741,   626,   474,   490,   485,   862,    57,   256,
      95,  1092,  1013,   452,    57,  1307,  1432,   419,   420,   486,
    1465,  1466,   111,  1278,   421,    62,    63,   493,   487,    79,
     488,  -723,    79,  1553,    79,   280,   131,  1162,   140,    79,
     489,   482,   292,   909,    89,    79,   492,  1081,  1433,   861,
    1550,   512,    79,   287,   613,   183,   184,  1279,   422,    96,
    -699,   194,   185,   495,   102,  1817,   887,   107,   423,    79,
      79,   196,   706,    75,   144,    88,   112,  1315,   289,   111,
     135,   490,   879,  -343,    79,  1375,   456,   596,  -344,   424,
    1821,   497,  1050,   162,   465,   590,   186,    95,   521,    79,
      57,  1020,   880,   493,  1183,   433,   187,  1027,    79,    79,
    1811,   196,   669,   103,   707,   477,   564,   568,   888,   881,
    -700,   102,  1821,   260,   107,    79,   543,   188,   564,  1551,
     435,   183,   184,   112,    79,  1203,   196,   135,   185,   816,
     179,  1328,  1329,   503,    79,   996,   508,    79,  -857,   532,
    1316,  1585,   602,  -343,    79,   521,   517,   529,  -344,   520,
     904,   798,  1236,  1226,    79,    79,   111,    79,   525,   157,
     103,  1043,   158,   159,   229,   160,   155,   254,   535,   278,
    1198,   264,   856,   497,    79,    79,  1263,   196,   560,  1058,
    1010,  1243,    79,   799,  1332,  1190,   156,    96,    79,    79,
    1217,   613,  1250,    79,   764,   596,   800,  1832,   102,  1892,
     248,   107,  1011,  1246,  1264,   801,   520,   802,   179,  1199,
     112,   955,  1012,  1150,   135,   613,  -522,   803,  1490,  1333,
    1253,  1254,   613,  1427,  1788,    79,  1737,   835,  1738,   279,
      79,   812,   146,    79,   640,   872,   356,   798,  1739,  1868,
     874,  1042,  1042,   209,  1277,  1160,  1330,   103,  1154,  1874,
    1741,   198,  1327,   934,  1305,  1255,  1089,   784,   804,   892,
    1042,  1667,  1021,  1883,   457,   204,  1024,  1317,   667,   799,
    1041,  1041,   898,  1279,  1844,  1037,  1038,   899,  1315,  1315,
    1315,   585,   800,   170,   170,  1957,  1667,  1811,   813,  1041,
    1910,   801,   903,   802,   111,   632,   321,    79,   634,   456,
    1118,  1752,  1753,   803,   190,  1910,  1752,  1753,   419,   420,
     812,   210,   194,  -723,  1190,   421,   823,   578,   170,   585,
      79,    79,   828,   279,    57,   879,  1042,   533,   161,   338,
     452,  1942,    79,    79,    72,    57,   559,    57,   750,   107,
    1522,    79,  1020,   465,   804,   880,   579,   580,   112,   422,
     175,  1316,  1316,  1316,   727,  1041,  1465,  1466,   497,   423,
     530,    79,   881,   751,    19,    77,    78,   813,   170,   596,
      79,   170,   456,   177,   915,    57,   917,   918,    57,   919,
     424,    57,   921,  1754,   170,   923,   924,   925,  1775,   934,
      79,   347,  1852,   861,   419,   420,    79,   819,  1325,  1945,
     452,   421,   822,   505,  1514,    57,   266,   497,  1364,   820,
     267,    57,    57,   270,   571,   272,   742,  1326,   497,   830,
     497,   204,   537,  1515,   248,  1853,  1598,  1600,  1602,   837,
    1245,   831,   205,   549,    79,  1239,    79,   834,   170,   279,
      57,   838,  1235,  1287,   613,   178,   204,    79,   206,    79,
      57,   456,   274,    57,   997,  1401,  1524,  1018,   497,   150,
    1025,   600,    79,   179,   600,    96,    79,  1446,  1447,   202,
    1325,  1031,   191,   903,  1436,   613,   893,  1641,  1317,  1317,
    1317,  1461,  1462,   170,  1068,    -3,  1514,   457,   497,  1563,
    1072,  1290,  1650,   170,   497,   497,  1642,    57,    79,   934,
    1406,   401,  1742,   546,   170,  1644,   551,   981,    57,   764,
      79,  1651,  1046,   216,  1483,  1484,   236,  1394,  1679,  1294,
    1174,  1743,   452,   497,  1042,  1175,  1764,  -400,    57,  1392,
    1773,   170,  1442,   600,  1645,  1269,   497,   170,   170,  1520,
     667,   274,   170,  1221,  1862,   667,   248,  1440,   667,  1650,
    1222,   266,   543,  1041,    79,    79,   934,    79,   696,   697,
     457,    79,   507,   452,    79,    57,   927,   667,  1746,  1051,
     260,  1383,   111,   276,   170,  1750,  1451,   928,   929,   170,
     497,   179,   170,  1424,  1539,   452,   452,  1455,  -627,    79,
    1235,   600,  1457,    13,    14,    15,    16,    17,   237,   238,
    1060,   239,   698,   699,   452,   240,   278,  1863,  1666,   279,
    1678,   497,   293,  1077,   559,   689,   278,   107,   425,  1076,
     497,  1830,   690,   691,  1771,  1597,   112,   266,   267,  1772,
     617,   582,   272,  1666,    79,   583,    79,    13,    14,    15,
      16,    17,   876,   967,   968,   969,   970,  1782,    79,  1193,
     321,    57,  1783,  1897,  -343,    79,    72,   351,  1898,  1595,
     248,   465,   157,   310,    79,   158,   159,  1422,   160,   170,
     452,   354,   861,    79,    79,    79,   599,  1276,  1877,  1953,
     600,   170,   170,   338,  1954,   708,  1885,    77,   601,   709,
     505,  1197,   808,    79,   497,    57,  -401,  1622,  1623,   278,
     602,   425,   850,  1582,   279,    13,    14,    15,    16,    17,
    -402,  1240,  -404,   425,  1080,   497,   904,   530,   355,    13,
      14,    15,    16,    17,  1594,  1088,   189,    63,  1090,    79,
      79,   465,  1093,   356,  1920,   140,   426,  1464,   785,   786,
    1091,    89,   787,   733,   571,   427,   425,   140,   497,   249,
    1018,   734,   425,  1164,   600,   735,    96,   455,  1300,   744,
     269,  1163,   747,    57,    72,   746,  1262,   764,  1165,   497,
     849,   908,    88,   459,   850,   583,    79,    57,   542,    63,
      79,   428,   613,   146,   599,    79,    72,  1046,   600,   910,
    1536,   640,  1701,   583,    95,    77,    78,   911,  1717,   933,
    1718,   912,   249,   934,  1001,   876,  1631,   429,   497,  1055,
     497,   170,   278,  1056,   460,   871,   497,    77,    78,   507,
     586,   274,  1719,   430,    79,   892,   505,  1722,  1723,   431,
     497,  1082,    79,   266,   473,   934,  1084,   475,  1680,  1283,
     934,  1496,   479,  1396,   478,   571,   249,   896,  1708,   497,
    1234,  1678,  1382,   861,  1235,   201,   735,   480,  1928,   170,
     481,    79,  1932,   111,   465,   494,  1593,   495,   452,   243,
       6,     7,     8,     9,    10,    11,    12,   243,     6,     7,
       8,     9,    10,    11,    12,  1410,   513,    79,   677,  1235,
     678,   679,   680,    79,    79,  1713,  1590,  1661,  1873,   514,
    1591,   934,   209,   942,   943,  1152,   946,   588,   107,   249,
     954,   353,   353,   958,  1681,  1873,   524,   112,   934,   681,
     201,   534,   682,   683,    79,   904,  1682,   684,   685,   321,
    1056,   692,   693,  1485,   553,   596,   265,  1415,   983,   249,
    1322,   694,   695,  1683,  1747,   249,   248,   934,   735,  1823,
     620,  1913,   417,   934,   103,  1901,  1955,   530,    61,  1235,
     934,   633,   338,    64,    65,    66,    67,    68,    69,    70,
    1376,  1795,  1982,   249,  1437,  1989,  1979,   248,   644,  1990,
     574,  1473,  1474,   700,   701,  1306,   465,  -856,   140,    79,
      79,    79,   191,   671,    89,   588,   671,   170,  1331,  1851,
    1467,  1318,   936,   937,   170,   671,  1260,    74,  -572,    96,
    1796,   401,   401,   465,   645,  1350,  -391,   140,   522,    79,
    1033,  1034,  1059,    89,  1061,    88,   631,    79,  1035,  1036,
      79,    79,   253,   263,    79,   140,  1224,  1056,    96,  -391,
     246,   257,  1237,  1238,    79,   934,  1241,    95,   702,   256,
     960,   961,   962,   648,    88,  1765,  1766,  1767,  1512,   733,
     733,    13,    14,    15,    16,    17,   649,   934,   675,   999,
      79,   653,  1002,  -147,  -147,   522,    95,  1768,  1100,   170,
     170,   452,   452,  1729,   676,    79,  1769,  1765,  1879,  1767,
    1628,  1629,  1630,   598,   688,   249,   703,  1857,   704,   934,
     705,   465,  1541,  1542,  1543,  1544,  1545,    79,   710,  1880,
     321,  1308,  1309,  1310,   736,  1513,   111,   432,  -176,    57,
    1035,  1374,    -3,  1938,   737,  1192,  1430,  1431,   738,   170,
    1435,  1431,   768,   507,   170,  1439,  1431,  1070,   739,    79,
    1669,  1074,   740,   338,   741,   111,  1007,  1423,   792,  1475,
    1423,  1322,  1322,  1322,   -16,  1498,  1322,  -403,  1152,  1007,
    1487,   107,   -17,   260,   781,  1669,  1603,  1056,   815,   249,
     112,  1715,  1056,   782,   484,  1716,  1431,  1726,  1727,   817,
    -405,   201,  1736,   934,  1786,  1787,   805,  1152,   833,   249,
     107,  1799,  1431,   806,   140,  1800,  1431,    79,   286,   112,
     149,    79,  1869,  1870,    79,   807,   485,   103,   148,   249,
     809,   598,  1318,  1318,  1318,   150,  1495,  1499,   466,   486,
     254,   264,  1523,  1525,   465,  1752,  1753,  -520,   487,   810,
     488,  1979,  1980,  1428,  1429,   811,   103,   963,   964,  1512,
     489,   851,   249,  -518,   465,   842,    79,   492,   532,   965,
     966,   971,   972,   248,  1653,  1653,   529,  1384,  1385,   863,
    1561,   865,   869,   882,   884,   602,   249,   906,   901,    61,
     913,   914,   941,   249,    64,    65,    66,    67,    68,    69,
      70,   490,   935,   938,   530,   980,   417,   417,  1006,   321,
     985,  1007,   140,  1014,  1467,  1053,  1513,  1062,  1164,  1063,
     465,  -703,  1064,  1065,   493,    79,  1163,  1066,  1067,  1083,
      79,    79,    79,  1165,    18,   798,  1085,  1646,    74,  1415,
    1094,   777,   338,   733,  1095,  -603,  1096,  1155,  1657,  -112,
    -112,  -112,  -112,  -112,  -112,   170,  1156,  1171,   170,   170,
     170,  1184,  1185,  1186,  1196,  1200,  1467,   799,  1201,  1204,
    1206,  1690,    47,    48,    49,    50,    51,    52,    53,    54,
     800,  1207,   170,  1208,  1209,  1210,  1212,  1213,   170,   801,
    1214,   802,  1219,  1220,  1242,  1247,   551,  1248,    79,  1249,
    1256,   803,  1257,   170,    79,   894,    79,  1258,  1259,   812,
    -591,  -590,  1267,    79,  1292,  1301,  1282,  1296,   848,  -111,
    -111,  -111,  -111,  -111,  -111,  1289,   417,   465,  -704,   465,
    1324,  1354,   140,  1323,   140,   627,  1334,  1337,    89,   170,
      89,  1338,   804,  1347,  1348,  -626,  1839,   452,   452,  1349,
    1356,  1358,   934,    96,  1411,    96,   533,   140,  1364,  1684,
     613,   465,  1368,    89,  1370,  1371,   813,  1372,  1407,  1408,
    1423,   140,  1378,  1380,  1450,  1814,   650,  1164,    96,  1421,
    1463,  1438,  1468,    79,  1469,  1163,  1471,  1470,  1431,   530,
    1476,  1479,  1165,  1488,  1489,   778,  1491,   466,    79,  1501,
      79,   686,   687,  1327,  1839,   401,  1527,  1781,  1503,  1512,
    1528,  1530,  1504,  1532,  1554,   249,    13,    14,    15,    16,
      17,   931,   686,  1533,  1791,  1535,   249,    61,   417,  1537,
     168,   169,    64,    65,    66,    67,    68,    69,    70,    13,
      14,    15,    16,    17,  1216,    79,   249,  1549,    79,  1556,
    1557,  1558,   686,  1467,  1559,  1564,  1566,  1567,  1569,   465,
     111,  1570,   111,  1575,  1571,  1572,  1513,  1573,  1580,  1586,
     170,  1588,  1584,   170,   140,  1589,  1596,  1604,   484,   246,
     257,   465,  1605,  1592,  1609,   111,  1610,   425,   256,  1620,
    1627,  1908,  1814,  1618,  1475,  1662,  1640,    79,    79,  1444,
    1506,  1845,  1671,  1235,  1671,   107,    79,   107,  1453,  1694,
     485,  1660,  1687,   170,   112,  1689,   112,  1706,  1700,  1930,
     210,   401,   401,   486,  1704,  1705,  1714,  1671,  1720,  1707,
     107,  1721,   487,    82,   488,   452,   147,  1724,  1725,   112,
    1774,  1734,  1841,  1731,   489,  1735,  1760,  1778,    79,   401,
     492,   103,  1784,   103,  1785,   465,  1780,  1118,   417,  1793,
    1794,  1797,  1798,  -592,  1891,  1818,  1806,  1807,  1808,  1809,
    1810,   529,  1824,   497,  1822,   140,   103,  1842,  1948,  1856,
    1858,    89,  1826,   465,  1834,   490,  1859,   812,  1860,   848,
      82,  1861,  1894,  1727,  -503,  1981,    96,  1835,  1843,  1882,
    1841,  1895,   260,  1896,  1899,   180,  1900,   493,  1903,  1906,
    1915,  1929,  1845,  1914,    82,  1917,  1845,  1845,  1918,  1925,
    1931,   568,   140,   465,   465,   401,  1936,   220,    89,  1937,
     245,   465,  1943,  1949,    82,   183,   184,  1944,    79,   140,
      79,  1950,   185,    96,   813,    89,   848,  1951,  1952,  1961,
    1967,   465,  1972,   465,   465,  1968,  1976,  1973,  1977,  1987,
      96,  1988,   170,  1991,   674,  1711,  1521,  1434,  1970,   974,
     930,   147,  1970,   973,   975,   140,   170,    82,   170,   147,
     976,    89,   296,   302,   977,  1353,  1360,  1962,    79,    79,
    1691,   196,   248,  1776,   320,  1909,    96,  1984,  1923,   465,
    1833,  1881,   959,   111,  1958,  1854,  1956,  1947,  1685,   465,
     170,   408,   180,   180,  1686,   466,  1887,  1886,   778,  1933,
    1974,   167,   249,   147,   438,  1369,    79,   245,   456,   523,
    1813,  1643,   170,    13,    14,    15,    16,    17,  1867,   465,
    1500,   465,  1654,  1366,   867,  1671,  1052,  1635,   107,  1173,
     111,   220,   220,   249,   788,   848,   465,   112,  1587,  1697,
       3,   465,  1202,  1969,   988,   989,   990,   111,   296,     0,
       0,     0,   848,   848,     0,   465,     0,    82,     0,    79,
     446,     0,  1978,     0,   530,     0,     0,     0,     0,    79,
     245,    57,  1671,     0,   103,   107,     0,     0,     0,     0,
    1194,     0,     0,   111,   112,     0,     0,     0,   170,  1671,
       0,     0,   107,     0,    13,    14,    15,    16,    17,     0,
     302,   112,     0,     0,     0,     0,   302,   296,   296,     0,
     170,     0,     0,     0,   147,     0,     0,     0,     0,     0,
       0,   103,     0,     0,     0,  1671,   170,     0,   107,     0,
      72,     0,   320,   603,   612,   170,     0,   112,   103,     0,
       0,     0,     0,     0,   417,     0,     0,     0,     0,   320,
     599,   894,    57,   320,   600,     0,     0,     0,  1230,     0,
       0,    77,   601,  1635,  1635,     0,  1230,     0,     0,     0,
     249,     0,    61,     0,   103,   168,   169,    64,    65,    66,
      67,    68,    69,    70,   170,     0,   408,     0,   593,     0,
       0,   616,   635,     0,     0,  1230,     0,     0,   466,     0,
       0,     0,     0,     0,     0,   593,     0,     0,     0,   593,
       0,    72,   170,     0,     0,     0,  1211,     0,   249,     0,
     408,  1215,     0,   728,     0,     0,     0,     0,     0,     0,
     180,   727,  1223,     0,     0,   497,     0,     0,     0,     0,
       0,     0,    77,    78,     0,     0,   147,     0,     0,     0,
     438,     0,   170,   170,   757,     0,   612,     0,  1230,     0,
     170,    13,    14,    15,    16,    17,   636,     0,    61,  1893,
       0,     0,  1635,    64,    65,    66,    67,    68,    69,    70,
     170,   637,   170,   170,   638,   639,    64,    65,    66,    67,
      68,    69,    70,     0,   220,     0,     0,     0,     0,   848,
     848,     0,     0,   220,     0,     0,   593,     0,     0,     0,
       0,     0,     0,   848,   848,   305,   306,   307,   308,    57,
       0,     0,     0,   296,     0,   408,   408,     0,   170,   296,
       0,   320,     0,     0,     0,     0,     0,     0,   170,     0,
    1865,     0,     0,     0,  1635,     0,   848,   848,     0,     0,
       0,     0,     0,    61,     0,     0,   168,   169,    64,    65,
      66,    67,    68,    69,    70,     0,     0,     0,   170,   296,
     170,  1635,     0,     0,     0,     0,     0,     0,    72,     0,
     296,     0,   296,     0,   320,   170,     0,   446,     0,     0,
     170,     0,     0,     0,     0,     0,   236,     0,  1631,   650,
     320,   438,   497,   612,   170,   309,     0,     0,  1985,    77,
      78,   603,     0,     0,     0,   603,     0,     0,  1992,  1361,
       0,  1635,  1635,   310,   320,   243,     6,     7,     8,     9,
      10,    11,    12,     0,   612,   466,     0,   320,     0,     0,
     446,  1230,     0,     0,    61,   147,     0,   168,   169,    64,
      65,    66,    67,    68,    69,    70,   593,   446,   408,  1635,
     147,   147,     0,   408,     0,     0,   408,     0,     0,   147,
     147,   147,   686,     0,   417,     0,     0,     0,     0,     0,
     593,    61,    18,     0,   542,    63,    64,    65,    66,    67,
      68,    69,    70,   593,    61,     0,     0,   217,   218,    64,
      65,    66,    67,    68,    69,    70,  1416,  1417,  1418,     0,
       0,     0,     0,  1419,  1420,     0,     0,  1362,     0,     0,
       0,   249,    72,   438,    51,    52,    53,    54,     0,   848,
     848,     0,     0,   982,     0,     0,     0,   386,     0,   728,
     728,     0,  1505,    74,     0,     0,     0,   408,     0,  1506,
       0,     0,    57,    77,    78,     0,     0,     0,   466,     0,
       0,     0,     0,     0,   438,  1658,    61,   757,     0,   757,
       0,    64,    65,    66,    67,    68,    69,    70,   952,   446,
       0,     0,     0,    61,     0,     0,   320,   320,    64,    65,
      66,    67,    68,    69,    70,     0,     0,     0,     0,   114,
       0,     0,   114,     0,     0,   320,     0,   296,     0,     0,
       0,    72,     0,   249,     0,     0,     0,     0,   953,     0,
     446,     0,     0,     0,   848,     0,   296,     0,     0,     0,
       0,    73,    74,     0,   466,     0,     0,     0,     0,  1230,
       0,     0,    77,    78,  1230,  1230,  1230,     0,     0,     0,
       0,     0,     0,     0,   848,     0,   114,     0,     0,   848,
     848,     0,     0,     0,   408,     0,     0,     0,     0,     0,
       0,   320,     0,     0,     0,     0,   147,   408,     0,     0,
     114,     0,     0,     0,     0,   320,     0,  1178,     0,     0,
       0,   249,     0,     0,     0,     0,   251,     0,   603,     0,
     114,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    13,    14,    15,    16,    17,     0,     0,   647,     0,
       0,   386,   652,     0,     0,     0,     0,     0,     0,     0,
       0,   655,   656,     0,     0,     0,     0,   114,   438,     0,
       0,   593,     0,   114,   616,   114,   386,   386,     0,   251,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   317,
     114,   348,     0,     0,     0,     0,     0,   386,     0,    57,
       0,     0,  1336,    61,     0,     0,     0,   412,    64,    65,
      66,    67,    68,    69,    70,   944,     0,     0,     0,   114,
     412,     0,     0,   251,   446,    57,     0,   386,     0,     0,
      61,     0,     0,   728,     0,    64,    65,    66,    67,    68,
      69,    70,  1230,     0,  1230,     0,     0,     0,     0,     0,
     757,     0,     0,     0,     0,   945,    61,   757,    72,   217,
     218,    64,    65,    66,    67,    68,    69,    70,     0,     0,
     114,     0,     0,   114,     0,     0,   193,     0,    73,    74,
       0,     0,     0,  1692,     0,     0,   251,     0,     0,    77,
      78,     0,     0,     0,     0,     0,     0,     0,     0,   320,
       0,     0,     0,   547,  1260,    74,     0,     0,     0,     0,
       0,   114,     0,     0,     0,     0,   251,     0,    61,     0,
       0,     0,   251,    64,    65,    66,    67,    68,    69,    70,
     114,     0,     0,     0,     0,     0,     0,     0,     0,   147,
       0,   193,     0,     0,     0,     0,    72,   408,   114,     0,
     251,   114,     0,     0,     0,     0,   193,     0,     0,     0,
       0,     0,     0,     0,     0,   114,  1008,    74,     0,   114,
     600,     0,     0,   193,     0,     0,   408,    77,    78,     0,
       0,     0,     0,     0,     0,   848,   441,     0,     0,     0,
       0,     0,     0,   245,    82,     0,     0,     0,     0,     0,
       0,     0,   412,     0,     0,     0,     0,   296,     0,     0,
       0,     0,     0,   147,     0,     0,     0,     0,     0,     0,
     438,     0,     0,     0,    61,     0,     0,   168,   169,    64,
      65,    66,    67,    68,    69,    70,   412,     0,     0,   193,
       0,     0,     0,     0,     0,     0,     0,   438,     0,     0,
       0,   147,     0,     0,     0,     0,     0,     0,     0,     0,
    1560,     0,   114,     0,     0,     0,   412,     0,     0,     0,
       0,     0,   251,     0,     0,     0,   593,   386,   386,   386,
     386,   386,   386,   386,   386,   386,   386,   386,   386,   386,
     386,   386,   386,   386,   386,   386,   193,  1339,     0,   182,
       0,     0,     0,   446,     0,     0,     0,     0,     0,     0,
    1850,     0,   320,   320,   193,     0,     0,     0,     0,    61,
       0,   223,   168,   169,    64,    65,    66,    67,    68,    69,
      70,    57,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   412,   412,     0,     0,     0,   251,   114,     0,     0,
     147,   147,   147,   147,   147,   147,     0,   386,     0,     0,
    1507,   302,    61,     0,     0,   217,   218,    64,    65,    66,
      67,    68,    69,    70,     0,     0,   298,     0,   114,     0,
       0,     0,     0,   114,     0,     0,   251,   114,     0,   114,
      72,     0,  1341,     0,     0,     0,     0,     0,     0,     0,
     114,     0,   193,     0,     0,     0,     0,     0,     0,   245,
    1889,    74,     0,     0,   497,   348,   114,   412,     0,   251,
       0,    77,    78,  1663,     0,  1672,     0,     0,   438,     0,
       0,     0,   193,     0,     0,     0,     0,     0,     0,     0,
     114,     0,     0,   251,     0,   483,   223,   547,     0,     0,
     251,   147,     0,   114,     0,   900,     0,  1695,     0,     0,
      61,   114,   298,   168,   169,    64,    65,    66,    67,    68,
      69,    70,     0,     0,   412,     0,   114,   114,     0,   412,
       0,     0,   412,     0,   446,   114,   114,   114,     0,     0,
       0,     0,    13,    14,    15,    16,    17,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   193,   193,     0,
       0,   386,     0,   441,     0,     0,   386,     0,     0,  1343,
       0,   569,   298,     0,     0,    61,     0,   386,   544,   545,
      64,    65,    66,    67,    68,    69,    70,  1632,     0,   412,
       0,  1507,     0,   408,     0,     0,     0,  1507,     0,  1507,
      57,     0,     0,     0,     0,  1751,     0,     0,     0,   666,
       0,     0,     0,   412,     0,     0,   193,   386,     0,     0,
       0,   408,     0,   408,     0,     0,    75,  1777,     0,     0,
     412,    61,     0,   441,   217,   218,    64,    65,    66,    67,
      68,    69,    70,     0,     0,     0,   408,     0,     0,     0,
       0,     0,   114,   114,     0,     0,   193,   320,    61,    72,
     147,   168,   169,    64,    65,    66,    67,    68,    69,    70,
       0,   114,     0,     0,     0,     0,     0,   193,     0,   219,
      74,     0,     0,   147,     0,     0,     0,     0,     0,     0,
      77,    78,     0,     0,     0,     0,     0,     0,     0,   114,
       0,  1820,     0,     0,     0,     0,     0,     0,   320,   320,
       0,     0,     0,     0,   576,     0,     0,     0,   758,     0,
       0,     0,   251,  1632,  1632,     0,     0,     0,     0,  1849,
     412,     0,     0,   251,     0,     0,     0,   114,  1507,     0,
       0,  1507,   114,   412,   386,     0,     0,     0,   824,   826,
       0,   114,     0,  1180,   412,   441,   114,     0,   797,     0,
       0,     0,   302,   147,     0,     0,     0,   223,     0,  1875,
    1876,     0,     0,     0,     0,     0,     0,  1884,     0,   193,
       0,     0,     0,     0,     0,     0,     0,   298,     0,     0,
       0,   296,     0,   298,     0,     0,   441,  1902,     0,  1904,
    1905,     0,     0,     0,   412,    61,     0,     0,   344,   345,
      64,    65,    66,    67,    68,    69,    70,   386,   441,   441,
      61,     0,     0,   217,   218,    64,    65,    66,    67,    68,
      69,    70,  1632,   298,     0,     0,     0,   441,     0,     0,
       0,  1507,     0,     0,   860,  1934,   298,     0,     0,     0,
       0,   386,   386,   386,   302,  1939,    75,     0,   386,   386,
       0,   346,     0,     0,   408,   114,     0,     0,     0,     0,
       0,   147,     0,     0,     0,     0,  1191,     0,     0,     0,
     386,   666,   114,   114,     0,  1960,   666,  1939,     0,   666,
       0,     0,     0,     0,     0,     0,   320,    13,    14,    15,
      16,    17,  1975,   441,  1632,     0,     0,  1960,   666,   193,
       0,   147,     0,     0,     0,     0,     0,   386,   386,     0,
       0,  1983,     0,     0,     0,     0,     0,   593,   147,   147,
       0,  1890,   302,     0,     0,   114,     0,     0,     0,     0,
       0,     0,   979,     0,     0,     0,     0,     0,     0,     0,
       0,    61,     0,     0,     0,    57,    64,    65,    66,    67,
      68,    69,    70,  1227,   147,     0,     0,  1228,    57,  1229,
     193,     0,     0,     0,     0,   114,     0,     0,     0,     0,
       0,  1890,  1890,   412,    98,   593,    61,   151,     0,   217,
     218,    64,    65,    66,    67,    68,    69,    70,     0,    61,
      74,     0,   217,   218,    64,    65,    66,    67,    68,    69,
      70,     0,   412,     0,    72,     0,     0,     0,     0,  1890,
      13,    14,    15,    16,    17,     0,     0,    72,     0,   251,
     114,  1009,     0,   758,   755,    74,     0,     0,   600,     0,
       0,    98,     0,     0,     0,    77,   756,   219,    74,   114,
       0,     0,     0,     0,     0,     0,   412,     0,    77,    78,
    1180,     0,     0,     0,     0,   195,     0,     0,     0,     0,
       0,   298,  1404,     0,     0,     0,     0,     0,    57,     0,
       0,     0,     0,   412,     0,   258,    61,   114,     0,     0,
     298,    64,    65,    66,    67,    68,    69,    70,  1227,     0,
       0,   441,  1228,     0,  1229,     0,     0,     0,     0,    61,
       0,     0,   217,   218,    64,    65,    66,    67,    68,    69,
      70,     0,   288,     0,     0,     0,     0,     0,    98,   114,
     114,     0,     0,     0,     0,    74,     0,    72,  1426,     0,
       0,     0,     0,   114,   114,   322,     0,     0,   114,   114,
       0,     0,     0,     0,     0,     0,     0,  1889,    74,     0,
       0,   497,     0,   418,     0,     0,     0,     0,    77,    78,
       0,     0,     0,     0,   288,   444,   114,   114,     0,     0,
       0,   108,     0,     0,     0,     0,   114,   114,   114,   114,
     114,   114,     0,     0,     0,    57,     0,   251,   386,     0,
       0,    61,     0,   491,    62,    63,    64,    65,    66,    67,
      68,    69,    70,     0,     0,   193,     0,     0,     0,   511,
       0,     0,   193,     0,   516,   518,    61,     0,   195,   217,
     218,    64,    65,    66,    67,    68,    69,    70,   108,    13,
      14,    15,    16,    17,     0,   251,     0,     0,     0,   193,
     538,     0,    75,   540,    72,   541,     0,     0,     0,     0,
       0,     0,     0,     0,   412,     0,   558,     0,     0,     0,
       0,     0,     0,     0,   295,    74,     0,     0,     0,   570,
       0,     0,   259,     0,  1009,    77,    78,   114,     0,     0,
    1261,   758,     0,     0,     0,     0,     0,    57,     0,     0,
       0,     0,     0,   591,     0,     0,   615,     0,     0,     0,
      57,     0,     0,     0,   441,   441,     0,     0,     0,     0,
     622,     0,     0,     0,   622,   108,     0,     0,    61,     0,
       0,   217,   218,    64,    65,    66,    67,    68,    69,    70,
       0,    61,   326,     0,   217,   218,    64,    65,    66,    67,
      68,    69,    70,     0,     0,     0,    72,     0,     0,   114,
     114,     0,     0,     0,     0,     0,     0,     0,     0,    72,
       0,     0,   445,     0,     0,     0,   295,    74,     0,   412,
       0,    13,    14,    15,    16,    17,     0,    77,    78,  1505,
      74,   386,     0,     0,     0,   114,     0,     0,     0,     0,
      77,    78,     0,     0,     0,     0,     0,   412,     0,   412,
       0,     0,     0,     0,     0,     0,     0,   288,     0,     0,
       0,   591,     0,     0,     0,   386,     0,     0,     0,     0,
     193,     0,   412,     0,     0,     0,     0,     0,     0,    57,
       0,   298,     0,   114,     0,     0,   114,   539,     0,     0,
       0,     0,     0,     0,   114,     0,     0,     0,     0,     0,
       0,     0,     0,   108,     0,     0,     0,     0,     0,   114,
      61,     0,     0,   217,   218,    64,    65,    66,    67,    68,
      69,    70,     0,     0,   114,     0,     0,     0,     0,   114,
     114,     0,     0,     0,   114,   114,     0,     0,    72,     0,
     592,     0,   444,   259,    61,     0,     0,   386,   386,    64,
      65,    66,    67,    68,    69,    70,  1227,   592,  1505,    74,
    1228,   592,  1229,     0,     0,     0,     0,     0,     0,    77,
      78,     0,     0,   844,     0,   386,  1458,     0,   518,     0,
       0,     0,   855,     0,   558,   193,     0,     0,   251,   114,
       0,     0,     0,    74,     0,   322,  1599,     0,     0,     0,
       0,     0,     0,     0,   386,     0,     0,     0,     0,  1526,
       0,   622,   875,     0,     0,     0,     0,     0,  1534,     0,
       0,     0,  1538,     0,  1540,     0,   886,     0,     0,     0,
       0,     0,     0,     0,  1511,   591,     0,     0,     0,     0,
     895,     0,     0,     0,     0,     0,     0,     0,   622,   193,
       0,   386,     0,     0,     0,     0,    61,     0,   592,   217,
     218,    64,    65,    66,    67,    68,    69,    70,    61,     0,
       0,   189,    63,    64,    65,    66,    67,    68,    69,    70,
     251,     0,     0,     0,    72,     0,     0,     0,     0,     0,
     412,     0,     0,     0,     0,     0,     0,   114,     0,     0,
     441,   441,     0,     0,   755,    74,     0,     0,   600,     0,
       0,     0,     0,     0,     0,    77,   756,    74,     0,     0,
     777,     0,   114,     0,     0,     0,     0,     0,   602,     0,
       0,     0,     0,     0,   444,     0,     0,   114,    61,   445,
       0,   168,   169,    64,    65,    66,    67,    68,    69,    70,
       0,   991,     0,     0,   114,   114,     0,     0,   251,     0,
       0,     0,  1626,     0,     0,     0,     0,     0,     0,     0,
     326,     0,     0,     0,     0,   875,     0,     0,     0,   259,
    1015,   108,     0,     0,     0,     0,   455,     0,     0,     0,
     114,     0,   445,     0,  1659,     0,     0,   444,   444,     0,
       0,     0,   113,     0,  1664,     0,  1675,     0,   592,   445,
       0,     0,     0,     0,     0,  1511,   444,     0,     0,     0,
       0,  1647,     0,  1511,     0,   114,     0,     0,     0,  1664,
      61,     0,   592,   217,   218,    64,    65,    66,    67,    68,
      69,    70,     0,     0,   844,   592,    61,     0,     0,   168,
     169,    64,    65,    66,    67,    68,    69,    70,    72,   113,
       0,     0,     0,    61,     0,     0,   217,   218,    64,    65,
      66,    67,    68,    69,    70,  1151,     0,     0,  1505,    74,
       0,     0,   444,     0,     0,  1506,     0,   151,   441,    77,
      78,    72,     0,     0,   459,     0,   622,     0,     0,  1182,
       0,   844,     0,   261,     0,     0,  1188,     0,     0,     0,
       0,  1889,    74,     0,     0,   497,     0,     0,     0,     0,
       0,     0,    77,    78,     0,     0,     0,     0,     0,     0,
       0,   445,     0,     0,     0,     0,     0,  1759,     0,     0,
       0,  1761,     0,     0,     0,     0,   113,     0,  1763,   322,
     712,   713,   714,   715,   716,   717,   718,   719,   720,   721,
     722,     0,  1748,   330,   204,  1511,     0,     0,     0,     0,
      61,     0,   445,   217,   218,    64,    65,    66,    67,    68,
      69,    70,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   723,     0,   447,   326,   326,     0,     0,    72,     0,
       0,   118,     0,     0,   118,     0,     0,     0,     0,     0,
     844,     0,     0,   326,     0,   298,     0,     0,   219,    74,
       0,     0,     0,     0,     0,     0,     0,   844,   844,    77,
      78,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   326,     0,     0,  1829,  1831,    61,  1675,     0,   217,
     218,    64,    65,    66,    67,    68,    69,    70,   118,     0,
       0,     0,     0,     0,     0,  1511,     0,     0,     0,     0,
       0,     0,   108,     0,    72,     0,     0,     0,     0,   326,
     444,     0,   118,     0,   113,     0,     0,     0,     0,     0,
       0,     0,     0,   592,   295,    74,   259,     0,   326,     0,
       0,     0,   118,  1878,    61,    77,    78,   168,   169,    64,
      65,    66,    67,    68,    69,    70,     0,     0,     0,     0,
    1319,   594,     0,     0,   261,    61,     0,     0,  1151,     0,
      64,    65,    66,    67,    68,    69,    70,  1227,   594,   118,
       0,  1228,   594,  1229,     0,   118,   445,   118,     0,     0,
       0,     0,     0,     0,     0,   298,  1919,  1151,  1921,   364,
       0,   365,     0,   366,    62,    63,    64,    65,    66,    67,
      68,    69,    70,     0,    74,  1367,     0,  1601,  1935,   118,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   118,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   591,     0,     0,     0,   569,   298,   326,     0,   673,
     516,     0,    75,   375,     0,     0,     0,     0,  1963,  1965,
    1966,     0,     0,     0,   326,   326,     0,     0,   322,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   594,
       0,     0,   118,   298,     0,   118,     0,     0,     0,     0,
     118,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   844,   844,     0,   326,     0,     0,
       0,     0,     0,   118,     0,     0,     0,     0,   844,   844,
       0,     0,     0,   444,   444,     0,     0,     0,     0,     0,
       0,     0,   118,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     447,   844,   844,     0,     0,   108,     0,     0,     0,     0,
       0,  1319,  1319,  1319,   151,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   330,     0,     0,   108,     0,     0,     0,     0,     0,
     261,     0,   113,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   259,   447,   118,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   594,
     447,     0,     0,     0,     0,     0,     0,     0,   592,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   118,   322,
       0,     0,     0,   594,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   445,   594,     0,     0,     0,
       0,     0,   151,     0,   118,    13,    14,    15,    16,    17,
       0,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,     0,
       0,   326,   326,    45,    46,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   326,   326,     0,     0,     0,
     326,   326,     0,    57,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   844,   844,     0,     0,     0,     0,
       0,     0,   447,   118,   118,     0,     0,     0,   326,   326,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1649,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     844,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1670,   447,  1670,   118,     0,     0,     0,   118,
       0,   118,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   330,   330,  1670,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   322,     0,
       0,   151,     0,     0,   330,     0,     0,     0,     0,   844,
       0,     0,     0,     0,     0,     0,   445,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   330,     0,     0,     0,     0,     0,     0,   844,
       0,     0,     0,   118,   844,   844,     0,     0,     0,   444,
     444,     0,     0,     0,     0,     0,   118,     0,   118,   118,
       0,   118,     0,   113,   118,  1740,     0,   118,   118,   118,
     330,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   594,     0,     0,   261,     0,   330,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1762,     0,     0,     0,     0,     0,
       0,   326,   326,     0,     0,     0,     0,     0,     0,     0,
       0,   119,     0,     0,   119,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   447,     0,     0,
       0,     0,     0,   123,     0,   118,   123,   326,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   108,
       0,   108,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   119,     0,
       0,     0,     0,     0,   108,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   326,     0,     0,   330,     0,
     123,     0,   119,     0,     0,  1670,   326,     0,     0,     0,
       0,     0,  1840,     0,     0,   330,   330,     0,     0,     0,
       0,     0,   119,     0,   123,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   326,   444,     0,     0,
       0,   326,   326,     0,   123,     0,   326,   326,     0,     0,
       0,     0,  1670,     0,     0,     0,     0,     0,     0,   119,
       0,     0,   118,     0,     0,   119,     0,   119,   330,  1670,
    1840,     0,     0,     0,   118,   118,     0,     0,     0,     0,
       0,   123,     0,     0,     0,     0,     0,   123,     0,   123,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   119,
       0,   259,     0,     0,     0,  1670,     0,     0,     0,     0,
       0,   119,     0,     0,     0,     0,   113,     0,     0,     0,
       0,   123,     0,  1927,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   123,     0,     0,     0,     0,     0,     0,
     844,     0,     0,     0,     0,   113,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   119,   261,     0,   119,     0,     0,     0,     0,
     119,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   123,     0,     0,   123,     0,   594,
       0,     0,   123,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   108,   119,     0,     0,     0,     0,     0,   592,
       0,     0,     0,     0,     0,     0,   447,     0,     0,     0,
       0,     0,   119,     0,     0,   123,     0,     0,     0,     0,
       0,     0,     0,     0,   326,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   123,     0,     0,     0,     0,   108,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   330,   330,     0,     0,   108,   592,     0,     0,
       0,     0,     0,     0,     0,     0,   330,   330,     0,     0,
       0,   330,   330,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   119,     0,     0,     0,     0,     0,
       0,     0,   108,     0,     0,     0,     0,   118,     0,   330,
     330,     0,     0,     0,     0,   118,   123,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   119,     0,
       0,     0,     0,     0,     0,     0,     0,   326,     0,     0,
       0,     0,     0,     0,   118,     0,     0,     0,     0,     0,
     123,     0,     0,     0,   119,     0,     0,     0,     0,     0,
       0,     0,   118,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   123,     0,     0,     0,
       0,   118,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   447,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   118,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   119,   119,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   123,   123,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   119,     0,     0,     0,   119,
       0,   119,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   330,   330,     0,     0,     0,   123,     0,     0,
       0,   123,     0,   123,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   118,   118,
     118,   118,   118,   118,     0,     0,     0,     0,   330,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     113,     0,   113,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   119,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   113,   119,     0,   119,   119,
       0,   119,     0,     0,   119,   123,   330,   119,   119,   119,
       0,     0,     0,     0,     0,     0,     0,   330,   123,     0,
     123,   123,     0,   123,     0,     0,   123,     0,     0,   123,
     123,   123,     0,     0,     0,   203,     0,     0,     0,     0,
       0,   214,   215,     0,     0,     0,     0,   330,     0,   118,
       0,     0,   330,   330,     0,     0,     0,   330,   330,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   277,     0,     1,     0,     0,
     145,     0,     0,     0,     0,   119,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   123,     0,     0,
       0,     0,   261,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   118,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   192,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   118,
       0,   118,     0,     0,     0,     0,     0,   247,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   268,     0,
     271,     0,   273,     0,   118,     0,     0,     0,     0,     0,
       0,     0,   119,     0,     0,     0,     0,     0,   118,     0,
       0,     0,   283,   113,   119,   119,     0,     0,     0,     0,
     594,     0,     0,     0,   123,     0,     0,     0,     0,     0,
     247,   118,   271,   273,     0,     0,   123,   123,     0,     0,
       0,   566,     0,     0,     0,   330,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     113,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   247,     0,     0,   113,   594,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   283,     0,     0,     0,
       0,   118,     0,   113,     0,     0,     0,     0,     0,     0,
       0,   519,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   283,     0,     0,     0,     0,     0,   247,     0,   271,
     273,   283,     0,     0,     0,     0,     0,     0,   330,     0,
       0,     0,     0,     0,     0,   550,   554,     0,     0,     0,
       0,     0,   561,   562,     0,     0,     0,   247,     0,     0,
       0,     0,     0,   247,     0,     0,     0,     0,   572,     0,
       0,     0,     0,     0,     0,     0,   753,     0,   754,     0,
       0,     0,     0,     0,     0,     0,   589,   770,   771,     0,
       0,   247,     0,     0,     0,     0,     0,   618,     0,   273,
       0,     0,   118,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   119,     0,     0,
       0,     0,   672,     0,     0,   119,     0,     0,     0,   118,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   123,
       0,     0,     0,     0,     0,     0,   118,   123,     0,     0,
       0,     0,     0,   711,   119,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   247,     0,     0,
       0,     0,   119,     0,     0,   854,   123,     0,     0,   749,
       0,     0,   118,   752,     0,     0,     0,     0,     0,     0,
       0,   119,     0,   247,   123,   618,   273,     0,     0,     0,
       0,     0,   774,     0,     0,     0,   775,   776,     0,     0,
     779,     0,     0,   123,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   793,   794,   795,   796,   119,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     247,     0,     0,     0,   818,     0,     0,     0,     0,     0,
       0,   123,   821,     0,     0,     0,     0,     0,     0,     0,
     247,     0,     0,     0,     0,   247,     0,   247,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     283,     0,     0,     0,     0,     0,     0,   247,     0,   247,
     247,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   247,     0,     0,
       0,   859,     0,     0,     0,     0,     0,     0,   550,   247,
       0,     0,     0,   864,     0,     0,     0,     0,   119,   119,
     119,   119,   119,   119,     0,     0,     0,     0,     0,     0,
     247,     0,   618,   273,     0,     0,   878,   883,     0,     0,
     123,   123,   123,   123,   123,   123,     0,     0,     0,     0,
       0,     0,     0,     0,   247,   618,     0,     0,     0,     0,
       0,   247,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1030,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   926,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   119,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   123,     0,     0,     0,     0,     0,     0,     0,     0,
     165,     0,     0,     0,  1098,  1099,     0,     0,     0,     0,
       0,     0,     0,     0,   987,  1157,  1158,  1159,     0,     0,
    1161,     0,     0,     0,     0,     0,   165,     0,     0,  1004,
       0,     0,     0,  1005,     0,     0,     0,     0,     0,     0,
       0,     0,   878,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   119,     0,     0,  1045,     0,     0,     0,     0,     0,
       0,     0,   165,  1054,     0,     0,     0,     0,     0,  1057,
       0,     0,     0,   123,     0,   165,     0,   165,     0,   119,
       0,   119,     0,     0,     0,     0,     0,     0,     0,  1225,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   123,     0,   123,   119,     0,     1,   350,     0,     0,
       0,     0,     0,     1,     0,     0,     0,     0,   119,     0,
       0,     0,     0,   247,   350,     0,   123,     0,     0,     0,
       0,     0,     0,  1244,   247,     0,     0,     0,     0,     1,
     123,   119,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   247,     0,     0,     0,     0,     0,
       0,     0,   165,   123,     0,   247,   165,     0,     0,   165,
     165,     0,     0,   165,   247,     0,   165,   165,     0,     0,
    1268,     0,     0,     0,     0,     0,     0,     0,     0,  1272,
    1273,  1274,  1275,  1205,     0,     0,     0,  1280,  1281,     0,
       0,     0,     0,     0,     0,     0,     0,  1288,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   119,     0,     0,     0,     0,     0,     0,  1302,     0,
    1303,     0,     0,     0,     0,     0,     0,     0,   165,     0,
       0,   165,     0,   123,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   163,     0,     0,     0,     0,
       0,   165,     0,     0,     0,     0,     0,     0,     0,     0,
     247,     0,     0,     0,  1251,     0,   165,     0,  1252,     0,
       0,     0,     0,  1359,     0,   878,     0,     0,     0,     0,
       0,     0,     0,     0,   247,  1265,     0,     0,     0,     0,
       0,     0,  1266,     0,     0,     0,     0,     0,     0,  1373,
       0,  1270,     0,  1271,     0,  1377,     0,  1379,  1381,     0,
       0,     0,   119,     0,     0,     0,  1387,   275,  1388,     0,
    1389,     0,  1391,     0,     0,     0,     0,  1399,     0,     0,
     281,     0,   282,     0,   123,  1298,     0,     0,     0,  1299,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   145,     0,     0,     1,     0,     0,   119,
     165,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   119,     0,     0,     0,
       0,   123,     0,     0,     0,  1441,     0,     0,     0,     0,
       0,     0,  1448,  1449,     0,     0,     0,     0,   123,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   119,     0,     0,   350,  1472,     0,     0,     0,
     247,     0,     0,  1477,   501,   502,  1478,   165,   506,     0,
       0,   509,   510,     0,   123,     0,     0,     0,     0,     0,
       0,     0,     0,  1386,     0,     0,     0,     0,     0,     0,
       0,   247,     0,     0,     0,     0,   214,   247,     0,   337,
       0,     0,     0,     0,     0,     0,     0,  1409,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   434,   337,
       0,     0,     0,     0,     0,     0,  1555,     0,     0,     0,
       0,   350,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   587,     0,     0,     0,
     500,     0,     0,     0,     0,     0,     0,   500,     0,  1574,
       0,   619,     0,     0,     0,     0,     0,  1579,     0,  1581,
     165,   165,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   165,     0,     0,     0,  1481,     0,     0,
       0,  1482,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1607,  1608,     0,   247,     0,
       0,  1517,     0,     0,     0,   500,     0,     0,     0,     0,
    1613,  1614,     0,  1615,  1529,  1531,     0,     0,     0,     0,
       0,     0,  1619,     0,     0,     0,     0,   337,   604,     0,
       0,     0,  1624,  1625,     0,   743,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   247,     0,   625,     0,
       0,  1565,     0,     0,  1568,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1576,
       0,     0,     0,     0,   165,   165,     0,     0,     0,     0,
     165,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   165,   814,     0,   165,   165,     0,   165,     0,   165,
     165,     0,     0,     0,     0,     0,     0,     0,   500,  1606,
       0,     0,     0,     0,     0,     0,  1702,  1703,  1611,     0,
       0,     0,  1612,     0,   500,   745,  1709,   500,   748,     0,
       0,     0,     0,     0,     0,   337,  1616,  1617,   165,   604,
       0,     0,   165,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1732,  1733,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     500,     0,     0,     0,   500,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   889,   890,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   337,   165,   897,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1698,  1699,     0,     0,
    1792,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1801,     0,
       0,  1802,  1803,     0,     0,     0,   500,     0,  1805,   337,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   873,   337,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   604,     0,     0,     0,
     604,     0,     0,     0,     0,     0,     0,   891,     0,   337,
       0,     0,     0,     0,     0,     0,     0,     0,   247,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   993,
     994,     0,     0,     0,     0,   998,     0,     0,     0,   247,
       0,     0,     0,     0,     0,     0,     0,   165,     0,     0,
       0,     0,     0,     0,  1779,     0,  1019,     0,     0,  1022,
    1023,     0,  1026,     0,  1028,  1029,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1568,  1888,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1804,   165,     0,     0,   165,     0,
       0,     0,     0,  1069,     0,     0,     0,  1073,   337,   247,
       0,     0,     0,     0,     0,  1916,     0,     0,     0,     0,
       0,  1819,     0,  1922,   500,   500,  1924,     0,     0,     0,
       0,   247,     0,     0,   500,  1000,     0,   500,  1003,     0,
       0,     0,  1836,     0,     0,  1837,     0,     0,     0,   337,
    1941,     0,   604,     0,   604,   604,     0,     0,     0,     0,
       0,   604,  1946,     0,     0,     0,     0,     0,     0,     0,
       0,   337,   337,     0,     0,   200,     0,  1959,     0,     0,
       0,     0,  1189,     0,     0,     0,     0,     0,     0,     0,
     337,   255,     0,     0,   500,     0,     0,     0,   500,     0,
       0,     0,   500,  1071,     0,     0,   500,  1075,     0,   247,
       0,     0,     0,     0,  1078,     0,     0,   165,     0,     0,
       0,     0,     0,     0,     0,   165,   165,     0,     0,     0,
    1907,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     200,     0,     0,     0,   303,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   342,   337,   500,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   200,     0,     0,     0,     0,     0,     0,     0,
     165,     0,     0,   604,     0,   454,     0,     0,   458,   165,
       0,     0,   165,     0,   165,   165,   247,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1189,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   337,     0,     0,     0,     0,     0,   171,
     174,     0,     0,     0,   165,     0,     0,     0,   200,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   255,     0,     0,     0,     0,     0,     0,     0,     0,
    1291,     0,     0,  1295,   212,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   458,   500,     0,
       0,     0,     0,     0,     0,   200,     0,     0,     0,     0,
       0,   165,     0,     0,     0,   604,   604,     0,   410,     0,
       0,     0,   604,   597,   290,   614,     0,   291,     0,     0,
       0,   439,     0,     0,     0,     0,     0,     0,     0,     0,
     311,     0,     0,     0,   467,     0,   467,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   337,     0,     0,     0,     0,   500,
    1293,     0,   500,  1297,     0,     0,     0,   670,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1393,   165,   476,     0,     0,     0,     0,     0,
    1402,  1403,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   200,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   165,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   567,     0,     0,     0,     0,     0,     0,   527,
       0,   597,     0,     0,     0,     0,     0,   769,   165,   171,
       0,     0,     0,     0,   165,  1443,     0,     0,     0,     0,
     171,     0,     0,     0,  1452,     0,     0,  1456,     0,  1459,
    1460,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   337,     0,   573,     0,     0,
       0,   604,  1395,   575,   577,     0,     0,     0,   584,     0,
       0,     0,     0,     0,     0,     0,     0,   165,     0,  1486,
       0,     0,   337,     0,     0,     0,   200,   200,     0,     0,
       0,     0,   454,     0,     0,     0,     0,     0,     0,     0,
     629,     0,     0,     0,     0,   311,     0,     0,   311,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   500,  1445,     0,     0,     0,     0,
       0,     0,     0,   500,  1454,     0,   604,     0,     0,     0,
       0,     0,     0,     0,     0,   342,  1562,   337,   337,     0,
       0,   165,   165,     0,     0,     0,     0,     0,     0,   350,
       0,     0,   454,   165,   877,     0,     0,     0,     0,     0,
       0,     0,     0,   467,     0,     0,     0,     0,     0,   467,
       0,     0,     0,     0,   790,   597,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   212,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   200,   772,   773,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   670,
       0,   670,   670,     0,   670,     0,     0,   670,  1456,     0,
     670,   670,   670,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   165,     0,  1621,     0,     0,
       0,     0,     0,   337,     0,     0,     0,     0,     0,     0,
       0,   858,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   454,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   439,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   200,     0,
       0,   885,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   165,     0,     0,     0,   454,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   311,     0,     0,
       0,     0,  1696,     0,     0,     0,     0,   454,   454,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   920,     0,     0,     0,   454,     0,     0,     0,
       0,     0,   500,     0,     0,     0,     0,     0,     0,     0,
     790,   940,     0,     0,     0,   629,     0,     0,   500,     0,
     951,     0,   956,   951,     0,     0,     0,     0,     0,     0,
       0,     0,   165,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1744,  1745,     0,     0,
     984,     0,     0,     0,     0,     0,     0,     0,  1749,     0,
       0,     0,   454,   986,     0,     0,     0,     0,   200,     0,
       0,     0,   337,     0,   995,     0,     0,     0,   769,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   439,     0,
       0,   984,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1048,     0,
       0,   467,     0,   337,   337,     0,     0,     0,     0,   342,
       0,     0,     0,     0,     0,     0,     0,     0,   500,   500,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1812,     0,   242,  1032,   500,     0,     0,  1079,     0,     0,
    1044,    13,    14,    15,    16,    17,     0,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,  -407,  -407,     0,  -407,    45,
      46,     0,  -407,     0,   410,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1179,  1181,  1864,     0,     0,    57,
       0,     0,   439,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1102,     0,     0,     0,     0,
       0,     0,     0,   467,     0,     0,     0,   500,     0,     0,
       0,     0,   951,    62,    63,   500,     0,     0,     0,     0,
     454,     0,     0,     0,     0,   984,     0,     0,     0,     0,
       0,     0,     0,  1218,     0,     0,     0,     0,    72,     0,
     951,     0,     0,     0,     0,  1195,     0,     0,     0,     0,
     629,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     670,    75,   301,     0,     0,     0,     0,     0,     0,    77,
      78,   337,     0,     0,     0,   500,  1866,     0,     0,   500,
       0,     0,     0,  1187,   467,     0,     0,     0,     0,     0,
      13,    14,    15,    16,    17,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   500,     0,     0,     0,
       0,     0,     0,     0,   255,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   359,     0,     0,     0,
     360,     0,   361,     0,   200,     0,     0,     0,     0,     0,
       0,   597,     0,     0,     0,     0,     0,     0,    57,   362,
       0,   467,     0,  1284,     0,  1286,   500,   500,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   342,     0,
       0,     0,   670,     0,     0,     0,   363,   364,     0,   365,
       0,   366,    62,    63,    64,    65,    66,    67,    68,    69,
      70,   367,   368,   356,   500,   369,   370,   371,     0,   372,
     373,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1351,  1351,     0,     0,     0,     0,   374,     0,     0,
      75,   375,     0,   454,   454,     0,     0,   376,   437,    78,
     377,   378,   379,   380,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1340,  1342,  1344,     0,  1397,     0,
       0,     0,     0,     0,     0,    13,    14,    15,    16,    17,
       0,   670,   670,   670,     0,   670,   670,     0,     0,     0,
       0,  1390,   458,     0,  1363,     0,     0,  1400,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1102,
       0,   359,     0,     0,   439,   360,     0,   361,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   467,     0,    57,   362,     0,     0,     0,     0,     0,
     255,     0,     0,     0,   951,   629,     0,   790,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   342,
       0,   363,   364,     0,   365,     0,   366,    62,    63,    64,
      65,    66,    67,    68,    69,    70,   367,   368,   356,     0,
     369,   370,   371,     0,   372,   373,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,  1480,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   374,     0,     0,    75,   375,     0,     0,     0,
       0,     0,   376,  1398,    78,   377,   378,   379,   380,     0,
       0,     0,     0,     0,     0,     0,   951,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   467,     0,     0,   790,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   200,     0,  1516,     0,     0,  1518,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   940,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1577,  1578,     0,     0,     0,
       0,     0,   359,     0,     0,     0,   360,     0,   361,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     467,     0,   790,     0,     0,   362,     0,     0,   342,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   363,   364,   670,   365,     0,   366,  1827,    63,
      64,    65,    66,    67,    68,    69,    70,   367,   368,   356,
       0,   369,   370,   371,     0,   372,   373,     0,     0,   454,
     454,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     410,     0,     0,   374,     0,  1648,    75,   375,  1335,     0,
       0,     0,     0,   376,    77,    78,   377,   378,   379,   380,
       0,     0,     0,     0,     0,     0,     0,  1828,  -175,     0,
       0,     0,     0,   255,     0,     0,     0,     0,     0,   359,
       0,     0,     0,   360,     0,   361,     0,     0,     0,  1688,
       0,     0,     0,     0,     0,     0,     0,     0,  1655,     0,
    1104,     0,   362,     0,     0,  1106,  1752,  1753,  1107,  1108,
    1109,  1110,  1111,  1112,  1113,  1114,  1115,  1116,  1117,  1118,
    -286,  1119,  1120,  1121,  1122,  1123,     0,  1124,  1710,   363,
     364,  1712,   461,     0,   366,  1125,  1126,    64,    65,    66,
      67,    68,    69,    70,   367,   368,   356,  1127,   369,   370,
     371,     0,   372,   373,     0,     0,     0,     0,   359,     0,
      72,     0,   360,     0,   361,   255,     0,     0,   629,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     374,   362,   670,    75,   375,     0,     0,     0,   279,     0,
     376,    77,    78,   377,   378,   379,   380,     0,     0,     0,
       0,     0,     0,     0,     0,  -175,     0,   454,   363,   364,
       0,   461,     0,   366,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   367,   368,   356,     0,   369,   370,   371,
       0,   372,   373,     0,     0,     0,     0,     0,     0,    72,
     670,     0,     0,   458,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   374,
      74,     0,   462,   463,     0,     0,     0,   464,     0,   376,
      77,    78,   377,   378,   379,   380,     0,     0,     0,     0,
       0,     0,  1790,     0,     0,     0,     0,     0,     0,     0,
       0,   629,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     4,
     243,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,  1103,     0,    19,   951,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,   359,     0,    45,    46,   360,     0,
     361,    47,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,    56,     0,  1104,    57,  1105,    -2,     0,
    1106,     0,     0,  1107,  1108,  1109,  1110,  1111,  1112,  1113,
    1114,  1115,  1116,  1117,  1118,  -286,  1119,  1120,  1121,  1122,
    1123,     0,  1124,     0,   363,   364,    60,   461,     0,   366,
    1125,  1126,    64,    65,    66,    67,    68,    69,    70,   367,
     368,   356,  1127,   369,   370,   371,     0,   372,   373,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    -3,   374,     0,     0,    75,   406,
       0,     0,     0,   279,     0,   376,    77,    78,   377,   378,
     379,   380,     0,     0,     0,     0,     0,     0,     0,     0,
    -175,     4,   243,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,  1103,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,   359,     0,    45,    46,
     360,     0,   361,    47,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,    56,     0,  1104,    57,  1105,
      -2,     0,  1106,     0,     0,  1107,  1108,  1109,  1110,  1111,
    1112,  1113,  1114,  1115,  1116,  1117,  1118,  -286,  1119,  1120,
    1121,  1122,  1123,     0,  1124,     0,   363,   364,    60,   461,
       0,   366,  1125,  1126,    64,    65,    66,    67,    68,    69,
      70,   367,   368,   356,  1127,   369,   370,   371,     0,   372,
     373,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   374,     0,     0,
      75,   406,     0,     0,     0,   279,     0,   376,    77,    78,
     377,   378,   379,   380,     0,     0,     0,     0,     0,     0,
       0,     0,  -175,     4,   243,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,   359,     0,
      45,    46,   360,     0,   361,    47,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,    56,     0,     0,
      57,   362,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   363,   364,
      60,   365,     0,   366,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   367,   368,   356,     0,   369,   370,   371,
       0,   372,   373,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   374,
       0,     0,    75,   406,     0,     0,     0,     0,     0,   376,
      77,    78,   377,   378,   379,   380,     0,     0,     0,     0,
       0,     0,     0,  1673,  1674,     4,   243,     6,     7,     8,
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
      37,    38,    39,    40,    41,    42,    43,    44,  -407,  -407,
       0,  -407,    45,    46,     0,  -407,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    57,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    61,     0,     0,    62,    63,    64,    65,
      66,    67,    68,    69,    70,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    73,    74,     0,    75,   244,     0,     0,     0,  -714,
       0,     0,    77,    78,     4,   243,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,     0,     0,     0,
       0,    45,    46,     0,     0,     0,    47,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,    56,     0,
       0,    57,     0,     0,     0,     0,  -339,  -339,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    60,     0,     0,     0,    62,    63,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -339,
       0,     0,     0,    75,    76,     0,     0,     0,     0,     0,
       0,    77,    78,     4,   243,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,     0,     0,
      45,    46,     0,     0,     0,    47,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,    56,     0,     0,
      57,     0,     0,     0,     0,  -340,  -340,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      60,     0,     0,     0,    62,    63,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -340,     0,
       0,     0,    75,    76,     0,     0,     0,     0,     0,     0,
      77,    78,   242,   243,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,     0,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,  -407,  -407,     0,  -407,    45,
      46,     0,  -407,     0,     0,     0,     0,     0,     0,     0,
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
    1314,     0,     0,     0,    75,   916,     0,     0,  1311,     0,
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
       0,     0,     0,  1492,     0,     0,     0,    75,   916,     0,
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
       0,     0,     0,     0,     0,     0,  1493,     0,     0,     0,
      75,   916,     0,     0,  1311,     0,     0,     0,    77,    78,
    1312,     0,     0,    13,    14,    15,    16,    17,    18,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,     0,     0,     0,
       0,    45,    46,     0,     0,     0,    47,    48,    49,    50,
      51,    52,    53,    54,     0,     0,     0,     0,     0,     0,
       0,    57,  1313,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    60,     0,     0,     0,    62,    63,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1494,
       0,     0,     0,    75,   916,     0,     0,     0,     0,     0,
       0,    77,    78,   242,   243,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,     0,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,  -407,  -407,     0,  -407,
      45,    46,     0,  -407,     0,     0,     0,     0,     0,     0,
       0,     0,    13,    14,    15,    16,    17,     0,     0,    19,
      57,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,  -406,  -406,     0,  -406,
      45,    46,     0,  -406,    62,    63,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      57,     0,     0,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    75,   244,     0,     0,     0,     0,     0,     0,
      77,    78,   243,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,     0,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,  -407,  -407,     0,  -407,    45,    46,
       0,  -407,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    57,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    61,
       0,     0,    62,    63,    64,    65,    66,    67,    68,    69,
      70,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    73,    74,     0,
      75,   244,     0,     0,     0,  -718,     0,     0,    77,    78,
     243,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,     0,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,  -407,  -407,     0,  -407,    45,    46,     0,  -407,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    57,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    61,  1335,     0,
      62,    63,    64,    65,    66,    67,    68,    69,    70,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,   359,
       0,     0,     0,   360,     0,   361,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    73,    74,     0,    75,   244,
    1104,     0,   362,    -2,     0,  1106,    77,    78,  1107,  1108,
    1109,  1110,  1111,  1112,  1113,  1114,  1115,  1116,  1117,  1118,
    -286,  1119,  1120,  1121,  1122,  1123,     0,  1124,     0,   363,
     364,     0,   461,     0,   366,  1125,  1126,    64,    65,    66,
      67,    68,    69,    70,   367,   368,   356,  1127,   369,   370,
     371,     0,   372,   373,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,  1335,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     374,     0,     0,    75,   375,     0,     0,     0,   279,     0,
     376,    77,    78,   377,   378,   379,   380,   359,     0,     0,
       0,   360,     0,   361,     0,  -175,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1104,     0,
     362,     0,     0,  1106,     0,     0,  1107,  1108,  1109,  1110,
    1111,  1112,  1113,  1114,  1115,  1116,  1117,  1118,  -286,  1119,
    1120,  1121,  1122,  1123,     0,  1124,     0,   363,   364,     0,
     461,     0,   366,  1125,  1126,    64,    65,    66,    67,    68,
      69,    70,   367,   368,   356,  1127,   369,   370,   371,     0,
     372,   373,     0,     0,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   374,     0,
       0,    75,   375,     0,     0,     0,   279,     0,   376,    77,
      78,   377,   378,   379,   380,     0,     0,     0,     0,     0,
       0,     0,     0,  -175,   243,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,     0,     0,
      45,    46,     0,     0,     0,   318,    48,    49,    50,    51,
      52,    53,    54,     0,    13,    14,    15,    16,    17,    18,
      57,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,     0,     0,
       0,     0,    45,    46,    62,    63,     0,    47,    48,    49,
      50,    51,    52,    53,    54,     0,     0,     0,     0,     0,
       0,     0,    57,     0,     0,     0,     0,     0,     0,    72,
       0,  1039,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -588,    75,   319,     0,     0,    62,    63,     0,     0,
      77,    78,   243,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    75,     0,     0,     0,    45,    46,
       0,     0,     0,   318,    48,    49,    50,    51,    52,    53,
      54,     0,    13,    14,    15,    16,    17,    18,    57,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,     0,     0,
      45,    46,    62,    63,     0,   318,    48,    49,    50,    51,
      52,    53,    54,     0,     0,     0,     0,     0,     0,     0,
      57,     0,     0,     0,     0,     0,     0,    72,     0,  1728,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      75,   319,     0,     0,    62,    63,     0,     0,    77,    78,
     243,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    75,     0,     0,     0,    45,    46,     0,     0,
       0,   318,    48,    49,    50,    51,    52,    53,    54,     0,
       0,     0,     0,     0,     0,     0,    57,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      62,    63,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    72,     0,  1730,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    75,   319,
       0,     0,     0,     0,     0,     0,    77,    78,   243,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
       0,     0,     0,     0,    45,    46,     0,     0,     0,   318,
      48,    49,    50,    51,    52,    53,    54,     0,     0,     0,
       0,     0,     0,     0,    57,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    62,    63,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
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
       0,     0,     0,     0,    75,   301,     0,     0,     0,     0,
       0,     0,    77,    78,   243,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,     0,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,  -407,  -407,     0,  -407,
      45,    46,     0,  -407,     0,     0,     0,     0,     0,     0,
       0,     0,    13,    14,    15,    16,    17,     0,     0,    19,
      57,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,  -407,  -407,     0,  -407,
      45,    46,     0,  -407,    62,    63,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      57,     0,     0,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    75,   244,     0,     0,     0,     0,     0,     0,
      77,    78,    13,    14,    15,    16,    17,    18,   657,    19,
     658,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,   359,     0,
      45,    46,   360,     0,   361,    47,    48,    49,    50,    51,
      52,    53,    54,     0,     0,     0,     0,     0,     0,     0,
      57,   362,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   659,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   363,   364,
       0,   365,     0,   366,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   367,   368,   356,     0,   369,   370,   371,
       0,   372,   373,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   374,
       0,     0,    75,   660,     0,     0,     0,   279,     0,   376,
      77,    78,   661,   662,   379,   380,    13,    14,    15,    16,
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
       0,     0,     0,   374,     0,   405,    75,   406,     0,     0,
       0,     0,     0,   376,    77,    78,   377,   378,   379,   380,
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
      75,   660,     0,     0,     0,   279,     0,   376,    77,    78,
     377,   378,   379,   380,    13,    14,    15,    16,    17,    18,
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
       0,   374,     0,     0,    75,   406,     0,     0,     0,     0,
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
       0,     0,     0,     0,     0,   374,     0,     0,    75,   436,
       0,     0,     0,     0,     0,   376,    77,    78,   377,   378,
     379,   380,    13,    14,    15,    16,    17,    18,     0,    19,
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
       0,     0,    75,   375,     0,     0,     0,     0,     0,   376,
      77,    78,   377,   378,   379,   380,    13,    14,    15,    16,
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
       0,  -716,     0,     0,    77,    78,    13,    14,    15,    16,
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
      42,    43,    44,  -407,  -407,     0,  -407,    45,    46,     0,
    -407,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    57,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    61,     0,
       0,    62,    63,    64,    65,    66,    67,    68,    69,    70,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    73,    74,     0,    75,
     301,     0,     0,     0,     0,     0,     0,    77,    78,   555,
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
      57,     0,     0,     0,     0,     0,  1412,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   922,
      75,   916,     0,     0,    62,    63,     0,     0,    77,    78,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    75,   916,     0,     0,     0,     0,     0,     0,
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
       0,     0,     0,     0,    45,    46,    62,    63,     0,   318,
      48,    49,    50,    51,    52,    53,    54,     0,     0,     0,
       0,     0,     0,     0,    57,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    75,   432,     0,     0,    62,    63,
       0,     0,    77,    78,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    75,   319,     0,     0,
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
       0,   318,    48,    49,    50,    51,    52,    53,    54,     0,
       0,     0,     0,     0,     0,     0,    57,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    75,   286,     0,     0,
      62,    63,     0,     0,    77,    78,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    75,   432,
       0,     0,     0,     0,     0,     0,    77,    78,   242,   243,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,     0,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,  -407,  -407,     0,  -407,    45,    46,     0,  -407,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    13,
      14,    15,    16,    17,    18,    57,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,     0,     0,     0,     0,    45,    46,    62,
      63,     0,   318,    48,    49,    50,    51,    52,    53,    54,
       0,    13,    14,    15,    16,    17,    18,    57,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,     0,     0,    75,     0,    45,
      46,    62,    63,     0,    47,    48,    49,    50,    51,    52,
      53,    54,     0,     0,     0,     0,     0,     0,     0,    57,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    75,
     301,     0,     0,    62,    63,     0,     0,    77,    78,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    75,   916,     0,     0,     0,     0,     0,     0,    77,
      78,    13,    14,    15,    16,    17,    18,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,     0,     0,     0,     0,    45,
      46,     0,     0,     0,   318,    48,    49,    50,    51,    52,
      53,    54,     0,    13,    14,    15,    16,    17,    18,    57,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,     0,     0,     0,
       0,    45,    46,    62,    63,     0,   318,    48,    49,    50,
      51,    52,    53,    54,     0,     0,     0,     0,     0,     0,
       0,    57,     0,     0,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    75,   916,     0,     0,    62,    63,     0,     0,    77,
      78,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    75,     0,     0,    13,    14,    15,    16,
      17,    77,    78,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
    -407,  -407,     0,  -407,    45,    46,     0,  -407,     0,     0,
       0,     0,     0,     0,     0,     0,    13,    14,    15,    16,
      17,     0,     0,    19,    57,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
    -407,  -407,     0,  -407,    45,    46,     0,  -407,    62,    63,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    57,     0,     0,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    75,   301,    62,    63,
       0,     0,     0,     0,    77,    78,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    75,     0,     0,     0,
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
       0,     0,     0,   843,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -601,    75,   243,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,     0,     0,     0,
       0,    45,    46,     0,     0,     0,   318,    48,    49,    50,
      51,    52,    53,    54,     0,     0,     0,     0,     0,     0,
       0,    57,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    62,    63,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1656,     0,     0,     0,     0,   243,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,     0,    19,    75,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,     0,
       0,     0,     0,    45,    46,     0,     0,     0,   318,    48,
      49,    50,    51,    52,    53,    54,     0,     0,     0,     0,
       0,     0,     0,    57,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   243,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    62,    63,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,  -407,  -407,     0,  -407,
      45,    46,     0,  -407,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    75,     0,     0,     0,     0,
      57,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    62,    63,     0,     0,     0,     0,
      13,    14,    15,    16,    17,    18,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,     0,     0,    45,    46,
       0,     0,    75,    47,    48,    49,    50,    51,    52,    53,
      54,     0,     0,     0,     0,     0,     0,     0,    57,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    61,
       0,     0,    62,    63,    64,    65,    66,    67,    68,    69,
      70,     0,     0,     0,    13,    14,    15,    16,    17,     0,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,  -407,  -407,
      75,  -407,    45,    46,     0,  -407,     0,     0,   359,     0,
       0,     0,   360,     0,   361,     0,     0,     0,     0,     0,
       0,     0,    57,     0,     0,     0,     0,     0,     0,     0,
       0,   362,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    62,    63,   363,   364,
       0,   365,     0,   366,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   367,   368,   356,     0,   369,   370,   371,
     359,   372,   373,     0,   360,     0,   361,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   362,    75,     0,     0,     0,     0,   374,
    1221,     0,    75,   375,     0,     0,     0,  1222,     0,   376,
      77,    78,   377,   378,   379,   380,     0,     0,     0,     0,
     363,   364,     0,   365,     0,   366,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   367,   368,   356,     0,   369,
     370,   371,   359,   372,   373,     0,   360,     0,   361,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   362,     0,     0,     0,     0,
       0,   374,   947,  1519,    75,   375,     0,     0,     0,     0,
       0,   376,    77,    78,   377,   378,   379,   380,     0,     0,
       0,     0,   363,   364,     0,   365,     0,   366,    62,    63,
      64,    65,    66,    67,    68,    69,    70,   367,   368,   356,
       0,   369,   370,   371,   359,   372,   373,     0,   360,     0,
     361,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   362,     0,     0,
       0,     0,     0,   374,     0,     0,    75,   375,     0,     0,
       0,   464,     0,   376,    77,    78,   377,   378,   379,   380,
       0,     0,     0,     0,   363,   364,     0,   365,     0,   366,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   367,
     368,   356,     0,   369,   370,   371,   359,   372,   373,     0,
     360,     0,   361,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   362,
       0,     0,     0,     0,     0,   374,   789,     0,    75,   375,
       0,     0,     0,     0,     0,   376,    77,    78,   377,   378,
     379,   380,     0,     0,     0,     0,   363,   364,     0,   365,
       0,   366,    62,    63,    64,    65,    66,    67,    68,    69,
      70,   367,   368,   356,     0,   369,   370,   371,   359,   372,
     373,     0,   360,     0,   361,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   362,     0,     0,     0,     0,     0,   374,     0,     0,
      75,   375,     0,     0,     0,   279,     0,   376,    77,    78,
     377,   378,   379,   380,     0,     0,     0,     0,   363,   364,
       0,   365,     0,   366,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   367,   368,   356,     0,   369,   370,   371,
     359,   372,   373,     0,   360,     0,   361,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   362,     0,     0,     0,     0,     0,   374,
     947,     0,    75,   375,     0,     0,     0,     0,     0,   376,
      77,    78,   377,   378,   379,   380,     0,     0,     0,     0,
     363,   364,     0,   365,     0,   366,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   367,   368,   356,     0,   369,
     370,   371,   359,   372,   373,     0,   360,     0,   361,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   362,     0,     0,     0,     0,
       0,   374,     0,     0,    75,   375,     0,     0,   978,     0,
       0,   376,    77,    78,   377,   378,   379,   380,     0,     0,
       0,     0,   363,   364,     0,   365,     0,   366,    62,    63,
      64,    65,    66,    67,    68,    69,    70,   367,   368,   356,
       0,   369,   370,   371,   359,   372,   373,     0,   360,     0,
     361,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   362,     0,     0,
       0,     0,     0,   374,  1285,     0,    75,   375,     0,     0,
       0,     0,     0,   376,    77,    78,   377,   378,   379,   380,
       0,     0,     0,     0,   363,   364,     0,   365,     0,   366,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   367,
     368,   356,     0,   369,   370,   371,   359,   372,   373,     0,
     360,     0,   361,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   362,
       0,     0,     0,     0,     0,   374,     0,     0,    75,   375,
       0,     0,     0,  1345,     0,   376,    77,    78,   377,   378,
     379,   380,     0,     0,     0,     0,   363,   364,     0,   365,
       0,   366,    62,    63,    64,    65,    66,    67,    68,    69,
      70,   367,   368,   356,     0,   369,   370,   371,   359,   372,
     373,     0,   360,     0,   361,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   362,     0,     0,     0,     0,     0,   374,     0,  1758,
      75,   375,     0,     0,     0,     0,     0,   376,    77,    78,
     377,   378,   379,   380,     0,     0,     0,     0,   363,   364,
       0,   365,     0,   366,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   367,   368,   356,     0,   369,   370,   371,
     359,   372,   373,     0,   360,     0,   361,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   362,     0,     0,     0,     0,     0,   374,
    1964,     0,    75,   375,     0,     0,     0,     0,     0,   376,
      77,    78,   377,   378,   379,   380,     0,     0,     0,     0,
     363,   364,     0,   365,     0,   366,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   367,   368,   356,     0,   369,
     370,   371,   359,   372,   373,     0,   360,     0,   361,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   362,     0,     0,     0,     0,
       0,   374,     0,     0,    75,   375,     0,     0,     0,     0,
       0,   376,    77,    78,   377,   378,   379,   380,     0,     0,
       0,     0,   363,   364,     0,   365,     0,   366,    62,    63,
      64,    65,    66,    67,    68,    69,    70,   367,   368,   356,
       0,   369,   370,   371,   359,   372,   373,     0,   360,     0,
     361,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   362,     0,     0,
       0,     0,     0,   646,     0,     0,    75,   375,     0,     0,
       0,     0,     0,   376,    77,    78,   377,   378,   379,   380,
       0,     0,     0,     0,   363,   364,     0,   365,     0,   366,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   367,
     368,   356,     0,   369,   370,   371,   359,   372,   373,     0,
     360,     0,   361,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   362,
       0,     0,     0,     0,     0,   651,     0,     0,    75,   375,
       0,     0,     0,     0,     0,   376,    77,    78,   377,   378,
     379,   380,     0,     0,     0,     0,   363,   364,     0,   365,
       0,   366,    62,    63,    64,    65,    66,    67,    68,    69,
      70,   367,   368,   356,     0,   369,   370,   371,   359,   372,
     373,     0,   360,     0,   361,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   362,     0,     0,     0,     0,     0,   654,     0,     0,
      75,   375,     0,     0,     0,     0,     0,   376,    77,    78,
     377,   378,   379,   380,     0,     0,     0,     0,   363,   364,
       0,   365,     0,   366,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   367,   368,   356,     0,   369,   370,   371,
     359,   372,   373,     0,   360,     0,   361,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   362,     0,     0,     0,     0,     0,   374,
       0,     0,    75,   375,     0,     0,     0,     0,     0,   376,
     857,    78,   377,   378,   379,   380,     0,     0,     0,     0,
     363,   364,     0,   365,     0,   366,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   367,   368,   356,     0,   369,
     370,   371,     0,   372,   373,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   374,     0,     0,    75,   375,     0,     0,     0,     0,
       0,   376,   437,    78,   377,   378,   379,   380
};

static const yytype_int16 yycheck[] =
{
       1,   178,     1,     4,     1,    73,   162,     4,   219,   256,
       1,   162,    95,     4,   283,   631,   684,   241,   865,    73,
      73,     4,   603,   374,   222,     1,    73,   671,     4,   464,
     612,   178,   173,   513,   514,   163,   599,    75,   757,   162,
     219,     1,   322,   229,   664,    82,  1756,   755,   407,   728,
     628,   852,  1195,   219,    55,    56,   599,    58,  1752,    58,
      73,    58,   219,     1,   219,    58,   757,    58,   254,   755,
      73,    87,    73,   599,   219,   150,    82,   534,   264,   755,
     220,    82,    58,    82,   295,  1113,   852,   320,  1631,    90,
    1631,    73,   290,   291,    95,   177,    95,    98,    58,    98,
    1631,   102,  1694,   102,   337,   102,    82,    98,   341,   337,
     147,   102,  1632,   341,   207,   219,   295,   574,    70,   102,
      58,   836,   755,   192,    70,  1092,   120,   181,   181,   295,
    1270,  1271,     1,   131,   181,   104,   105,   220,   295,   140,
     295,   157,   143,   149,   145,   131,   145,   862,   145,   150,
     295,   219,   145,   633,   145,   156,   296,   821,   152,   572,
      95,   236,   163,   139,   444,   219,   219,   165,   181,   145,
       0,   147,   219,    96,     1,  1869,   757,     1,   181,   180,
     181,   180,   130,   152,     0,   145,     1,  1097,   139,    58,
       1,   295,   755,    87,   195,  1162,   195,   320,    87,   181,
    1910,   153,   780,   149,   205,   438,   219,   145,   245,   210,
      70,   758,   755,   296,   878,   191,   219,   764,   219,   220,
    1740,   220,   374,     1,   172,   211,   149,   295,   605,   755,
       0,    58,  1942,   102,    58,   236,   274,   219,   149,   174,
     191,   295,   295,    58,   245,   913,   245,    58,   295,   496,
     149,    59,    60,   228,   255,   735,   231,   258,   157,   258,
    1097,  1404,   173,   157,   265,   302,   242,   258,   157,   245,
     629,   482,   953,   941,   275,   276,   145,   278,   253,    56,
      58,   775,    59,    60,    95,    62,   115,    98,   263,   149,
     906,   102,   561,   153,   295,   296,  1015,   296,   284,   793,
    1008,   982,   303,   482,  1105,   886,   149,   283,   309,   310,
     930,   591,   991,   314,   442,   438,   482,  1771,   145,  1839,
      98,   145,  1008,   987,  1015,   482,   302,   482,   149,   907,
     145,   682,  1008,  1134,   145,   615,   157,   482,  1305,  1105,
    1004,  1005,   622,  1228,  1936,   346,  1889,   533,  1889,   157,
     351,   491,     4,   354,   355,   588,   115,   568,  1889,   131,
     588,   774,   775,    87,  1045,   859,   174,   145,  1134,  1823,
    1890,   408,   149,   155,  1089,  1008,   833,   470,   482,   612,
     793,  1529,   759,   165,   195,   146,   763,  1097,   374,   568,
     774,   775,   625,   165,   153,   772,   773,   625,  1308,  1309,
    1310,   315,   568,    55,    56,    73,  1554,  1927,   491,   793,
    1871,   568,   173,   568,   283,   351,   572,   418,   354,   418,
      88,    75,    76,   568,   462,  1886,    75,    76,   482,   482,
     570,   155,   408,   157,  1015,   482,   511,   131,    90,   353,
     441,   442,   517,   157,    70,  1008,   859,   258,   149,   572,
     519,  1912,   453,   454,   129,    70,   283,    70,   434,   283,
     174,   462,  1009,   464,   568,  1008,   160,   161,   283,   482,
     149,  1308,  1309,  1310,   149,   859,  1616,  1617,   153,   482,
     258,   482,  1008,   434,    19,   160,   161,   570,   140,   612,
     491,   143,   491,   149,   646,    70,   648,   649,    70,   651,
     482,    70,   654,   157,   156,   657,   658,   659,   157,   155,
     511,   163,    73,   926,   568,   568,   517,   503,   155,   165,
     589,   568,   508,   149,   155,    70,   103,   153,    89,   504,
     107,    70,    70,   110,   149,   112,   149,   174,   153,   525,
     153,   146,   265,   174,   322,   106,  1431,  1432,  1433,   535,
     985,   526,   157,   276,   555,   148,   557,   532,   210,   157,
      70,   536,   155,  1057,   844,   149,   146,   568,   173,   570,
      70,   570,   152,    70,   149,  1191,   174,   149,   153,   555,
     149,   153,   583,   149,   153,   561,   587,  1251,  1252,   174,
     155,   768,   154,   173,  1238,   875,  1178,   155,  1308,  1309,
    1310,  1265,  1266,   255,   149,   155,   155,   418,   153,   174,
     149,   149,   155,   265,   153,   153,   174,    70,   619,   155,
    1198,   768,   155,   275,   276,   174,   278,   709,    70,   757,
     631,   174,   779,   174,  1298,  1299,     3,  1184,   174,   149,
     873,   174,   711,   153,  1057,   873,  1674,     3,    70,   149,
    1678,   303,   149,   153,  1501,  1032,   153,   309,   310,  1327,
     646,   152,   314,   150,  1804,   651,   444,  1245,   654,   155,
     157,   248,   710,  1057,   675,   676,   155,   678,   125,   126,
     491,   682,   230,   752,   685,    70,   151,   673,   174,   782,
     559,  1171,   561,   155,   346,   174,   149,   162,   163,   351,
     153,   149,   354,   148,  1348,   774,   775,   149,   156,   710,
     155,   153,  1259,    12,    13,    14,    15,    16,    46,    47,
     795,    49,   169,   170,   793,    53,   149,   149,  1529,   157,
    1531,   153,   173,   815,   561,   160,   149,   561,   151,   814,
     153,  1769,   167,   168,   151,  1426,   561,   324,   325,   156,
     327,   151,   329,  1554,   755,   155,   757,    12,    13,    14,
      15,    16,   589,   696,   697,   698,   699,   151,   769,   897,
     926,    70,   156,   151,   157,   776,   129,   149,   156,  1423,
     558,   782,    56,   171,   785,    59,    60,  1222,    62,   441,
     859,   149,  1205,   794,   795,   796,   149,  1044,  1826,   151,
     153,   453,   454,   926,   156,   151,  1834,   160,   161,   155,
     149,   904,   151,   814,   153,    70,     3,  1481,  1482,   149,
     173,   151,   155,  1401,   157,    12,    13,    14,    15,    16,
       3,   978,   131,   151,   820,   153,  1195,   615,   149,    12,
      13,    14,    15,    16,  1422,   831,   104,   105,   834,   850,
     851,   852,   838,   115,  1882,   852,   151,  1270,   152,   153,
     835,   852,   156,   411,   149,   151,   151,   864,   153,    98,
     149,   151,   151,   864,   153,   155,   852,   149,  1076,   427,
     109,   864,   430,    70,   129,   149,  1014,  1015,   864,   153,
     151,   151,   852,   149,   155,   155,   897,    70,   104,   105,
     901,   151,  1182,   555,   149,   906,   129,  1054,   153,   151,
    1345,   912,  1576,   155,   852,   160,   161,   151,  1599,   151,
    1601,   155,   151,   155,   149,   752,   149,   151,   153,   151,
     153,   583,   149,   155,   155,   587,   153,   160,   161,   487,
     151,   152,  1606,   151,   945,  1178,   149,  1611,  1612,   151,
     153,   151,   953,   530,    21,   155,   151,   149,  1536,  1052,
     155,  1312,   155,  1187,   149,   149,   195,   619,  1584,   153,
     151,  1772,   151,  1386,   155,    82,   155,   155,  1894,   631,
     155,   982,  1898,   852,   985,   155,  1421,    96,  1057,     4,
       5,     6,     7,     8,     9,    10,    11,     4,     5,     6,
       7,     8,     9,    10,    11,   151,   149,  1008,   118,   155,
     120,   121,   122,  1014,  1015,  1593,   151,   151,  1819,   149,
     155,   155,    87,   675,   676,   852,   678,   154,   852,   258,
     682,  1172,  1173,   685,   151,  1836,   157,   852,   155,   149,
     147,   157,   152,   153,  1045,  1404,   151,   157,   158,  1205,
     155,   162,   163,  1300,   148,  1178,    63,  1209,   710,   288,
    1097,   123,   124,   151,   151,   294,   844,   155,   155,   151,
     151,  1872,   179,   155,   852,   151,   151,   855,   101,   155,
     155,   173,  1205,   106,   107,   108,   109,   110,   111,   112,
    1165,  1707,   151,   322,  1241,   151,   155,   875,   151,   155,
     157,  1278,  1279,   127,   128,  1091,  1107,   157,  1105,  1110,
    1111,  1112,   154,   155,  1105,   154,   155,   769,  1104,  1787,
    1271,  1097,   160,   161,   776,   155,   149,   150,   157,  1105,
    1708,  1278,  1279,  1134,   115,  1121,   151,  1134,   245,  1140,
     154,   155,   794,  1134,   796,  1105,   157,  1148,   154,   155,
    1151,  1152,  1151,  1152,  1155,  1152,   154,   155,  1134,   174,
    1151,  1152,   154,   155,  1165,   155,   156,  1105,   161,  1152,
     689,   690,   691,   149,  1134,   143,   144,   145,  1318,   727,
     728,    12,    13,    14,    15,    16,   149,   155,   158,   737,
    1191,   149,   740,   154,   155,   302,  1134,   165,   850,   851,
     852,  1270,  1271,  1616,   158,  1206,   174,   143,   144,   145,
    1492,  1493,  1494,   320,   166,   444,   159,  1795,   171,   155,
     129,  1222,   108,   109,   110,   111,   112,  1228,   152,   165,
    1386,  1094,  1095,  1096,   151,  1318,  1105,   153,   174,    70,
     154,   155,   154,  1907,   151,   897,   154,   155,   151,   901,
     154,   155,   131,   801,   906,   154,   155,   805,   151,  1260,
    1529,   809,   151,  1386,   151,  1134,   154,   155,   149,   154,
     155,  1308,  1309,  1310,   156,  1312,  1313,   131,  1105,   154,
     155,  1105,   156,  1152,   156,  1554,   154,   155,   149,   518,
    1105,   154,   155,   155,  1505,   154,   155,   154,   155,   154,
     131,   408,   154,   155,   155,   156,   151,  1134,   157,   538,
    1134,   154,   155,   151,  1311,   154,   155,  1318,   153,  1134,
    1311,  1322,   155,   156,  1325,   151,  1505,  1105,  1311,   558,
     151,   438,  1308,  1309,  1310,  1311,  1312,  1313,   205,  1505,
    1151,  1152,  1328,  1329,  1345,    75,    76,   157,  1505,   151,
    1505,   155,   156,  1229,  1230,   151,  1134,   692,   693,  1499,
    1505,    68,   591,   157,  1365,   157,  1367,  1507,  1367,   694,
     695,   700,   701,  1151,  1512,  1513,  1367,  1172,  1173,   154,
    1366,   149,    76,   154,    17,   173,   615,   157,   155,   101,
     149,   174,   157,   622,   106,   107,   108,   109,   110,   111,
     112,  1505,   151,   151,  1182,   174,   513,   514,   154,  1565,
     157,   154,  1409,    17,  1565,   148,  1499,   151,  1409,   151,
    1421,   148,   151,   151,  1507,  1426,  1409,   151,   151,   151,
    1431,  1432,  1433,  1409,    17,  1646,   151,  1505,   150,  1591,
     157,   153,  1565,   991,   157,   151,   157,    68,  1517,    12,
      13,    14,    15,    16,    17,  1107,   174,   173,  1110,  1111,
    1112,   151,   151,   151,   148,   157,  1617,  1646,   151,   151,
     155,  1553,    55,    56,    57,    58,    59,    60,    61,    62,
    1646,   151,  1134,   151,   155,   151,   151,   151,  1140,  1646,
     151,  1646,   151,   151,   151,   151,  1148,   151,  1499,   151,
     151,  1646,   151,  1155,  1505,   612,  1507,   151,   151,  1649,
     151,   151,   154,  1514,  1062,   151,   148,  1065,   550,    12,
      13,    14,    15,    16,    17,   173,   633,  1528,   148,  1530,
     155,    13,  1529,   151,  1531,     9,   149,   149,  1529,  1191,
    1531,   149,  1646,   149,   149,   156,  1779,  1616,  1617,   149,
      72,   174,   155,  1529,  1206,  1531,  1367,  1554,    89,  1545,
    1840,  1562,   156,  1554,   174,   154,  1649,   154,   148,   148,
     155,  1568,   174,   174,   151,  1752,   362,  1568,  1554,   157,
     154,   174,   151,  1584,   155,  1568,   151,   155,   155,  1367,
     154,   151,  1568,   151,   148,   462,   148,   464,  1599,   149,
    1601,   387,   388,   149,  1837,  1752,    78,  1689,   174,  1749,
     151,   151,   174,   174,   149,   844,    12,    13,    14,    15,
      16,    17,   408,   174,  1699,   174,   855,   101,   735,   174,
     104,   105,   106,   107,   108,   109,   110,   111,   112,    12,
      13,    14,    15,    16,    17,  1646,   875,   174,  1649,   148,
     174,   174,   438,  1804,   149,   151,   148,   148,   155,  1660,
    1529,   155,  1531,   151,   154,   154,  1749,   154,   154,   148,
    1322,   151,   157,  1325,  1671,   156,   118,   148,  1889,  1670,
    1671,  1682,   151,   156,   151,  1554,   151,   151,  1671,   151,
     148,  1868,  1869,   154,   154,   149,   174,  1698,  1699,  1247,
     156,  1783,  1529,   155,  1531,  1529,  1707,  1531,  1256,   107,
    1889,   151,   149,  1365,  1529,   149,  1531,   148,   154,  1896,
     155,  1868,  1869,  1889,   154,   154,   148,  1554,   151,   157,
    1554,   151,  1889,     1,  1889,  1804,     4,   151,   151,  1554,
     148,   151,  1779,   154,  1889,   151,   151,   149,  1749,  1896,
    1890,  1529,   151,  1531,   151,  1756,   174,    88,   865,   154,
     154,   148,   148,   151,  1839,   156,   151,   151,   151,   151,
     151,  1762,    76,   153,   174,  1772,  1554,   156,  1925,   151,
     148,  1772,   174,  1784,   174,  1889,   151,  1927,   151,   821,
      58,   151,   156,   155,   152,  1972,  1772,   174,   174,   174,
    1837,   101,  1671,   149,   155,    73,    73,  1890,   149,   148,
      73,   154,  1894,   174,    82,   150,  1898,  1899,   150,   174,
     174,  1889,  1819,  1824,  1825,  1972,   107,    95,  1819,   107,
      98,  1832,   165,   151,   102,  1889,  1889,   165,  1839,  1836,
    1841,   156,  1889,  1819,  1927,  1836,   878,  1929,   151,   148,
     148,  1852,   149,  1854,  1855,   151,    73,   174,   151,   151,
    1836,   174,  1514,   174,   376,  1591,  1327,  1235,  1950,   703,
     663,   139,  1954,   702,   704,  1872,  1528,   145,  1530,   147,
     705,  1872,   150,   151,   706,  1123,  1134,  1942,  1889,  1890,
    1554,  1890,  1670,  1681,   162,  1869,  1872,  1979,  1886,  1900,
    1772,  1829,   688,  1772,  1937,  1788,  1936,  1924,  1546,  1910,
    1562,   179,   180,   181,  1546,   782,  1837,  1836,   785,  1899,
    1954,    48,  1151,   191,   192,  1155,  1927,   195,  1927,   250,
    1749,  1499,  1584,    12,    13,    14,    15,    16,  1812,  1940,
    1313,  1942,  1513,  1148,   583,  1772,   782,  1495,  1772,   868,
    1819,   219,   220,  1182,   471,   987,  1957,  1772,  1409,  1568,
       0,  1962,   912,  1949,   727,   727,   727,  1836,   236,    -1,
      -1,    -1,  1004,  1005,    -1,  1976,    -1,   245,    -1,  1980,
     192,    -1,  1968,    -1,  1762,    -1,    -1,    -1,    -1,  1990,
     258,    70,  1819,    -1,  1772,  1819,    -1,    -1,    -1,    -1,
       9,    -1,    -1,  1872,  1819,    -1,    -1,    -1,  1660,  1836,
      -1,    -1,  1836,    -1,    12,    13,    14,    15,    16,    -1,
     288,  1836,    -1,    -1,    -1,    -1,   294,   295,   296,    -1,
    1682,    -1,    -1,    -1,   302,    -1,    -1,    -1,    -1,    -1,
      -1,  1819,    -1,    -1,    -1,  1872,  1698,    -1,  1872,    -1,
     129,    -1,   320,   321,   322,  1707,    -1,  1872,  1836,    -1,
      -1,    -1,    -1,    -1,  1171,    -1,    -1,    -1,    -1,   337,
     149,  1178,    70,   341,   153,    -1,    -1,    -1,   945,    -1,
      -1,   160,   161,  1631,  1632,    -1,   953,    -1,    -1,    -1,
    1319,    -1,   101,    -1,  1872,   104,   105,   106,   107,   108,
     109,   110,   111,   112,  1756,    -1,   374,    -1,   320,    -1,
      -1,   323,    12,    -1,    -1,   982,    -1,    -1,   985,    -1,
      -1,    -1,    -1,    -1,    -1,   337,    -1,    -1,    -1,   341,
      -1,   129,  1784,    -1,    -1,    -1,   922,    -1,  1367,    -1,
     408,   927,    -1,   411,    -1,    -1,    -1,    -1,    -1,    -1,
     418,   149,   938,    -1,    -1,   153,    -1,    -1,    -1,    -1,
      -1,    -1,   160,   161,    -1,    -1,   434,    -1,    -1,    -1,
     438,    -1,  1824,  1825,   442,    -1,   444,    -1,  1045,    -1,
    1832,    12,    13,    14,    15,    16,    86,    -1,   101,  1841,
      -1,    -1,  1740,   106,   107,   108,   109,   110,   111,   112,
    1852,   101,  1854,  1855,   104,   105,   106,   107,   108,   109,
     110,   111,   112,    -1,   482,    -1,    -1,    -1,    -1,  1251,
    1252,    -1,    -1,   491,    -1,    -1,   438,    -1,    -1,    -1,
      -1,    -1,    -1,  1265,  1266,    63,    64,    65,    66,    70,
      -1,    -1,    -1,   511,    -1,   513,   514,    -1,  1900,   517,
      -1,   519,    -1,    -1,    -1,    -1,    -1,    -1,  1910,    -1,
    1808,    -1,    -1,    -1,  1812,    -1,  1298,  1299,    -1,    -1,
      -1,    -1,    -1,   101,    -1,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,    -1,    -1,    -1,  1940,   557,
    1942,  1839,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,
     568,    -1,   570,    -1,   572,  1957,    -1,   519,    -1,    -1,
    1962,    -1,    -1,    -1,    -1,    -1,     3,    -1,   149,  1105,
     588,   589,   153,   591,  1976,   153,    -1,    -1,  1980,   160,
     161,   599,    -1,    -1,    -1,   603,    -1,    -1,  1990,    76,
      -1,  1889,  1890,   171,   612,     4,     5,     6,     7,     8,
       9,    10,    11,    -1,   622,  1222,    -1,   625,    -1,    -1,
     572,  1228,    -1,    -1,   101,   633,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   588,   589,   646,  1927,
     648,   649,    -1,   651,    -1,    -1,   654,    -1,    -1,   657,
     658,   659,  1178,    -1,  1501,    -1,    -1,    -1,    -1,    -1,
     612,   101,    17,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   625,   101,    -1,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,  1212,  1213,  1214,    -1,
      -1,    -1,    -1,  1219,  1220,    -1,    -1,   174,    -1,    -1,
      -1,  1670,   129,   711,    59,    60,    61,    62,    -1,  1481,
    1482,    -1,    -1,   153,    -1,    -1,    -1,   178,    -1,   727,
     728,    -1,   149,   150,    -1,    -1,    -1,   735,    -1,   156,
      -1,    -1,    70,   160,   161,    -1,    -1,    -1,  1345,    -1,
      -1,    -1,    -1,    -1,   752,  1517,   101,   755,    -1,   757,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   711,
      -1,    -1,    -1,   101,    -1,    -1,   774,   775,   106,   107,
     108,   109,   110,   111,   112,    -1,    -1,    -1,    -1,     1,
      -1,    -1,     4,    -1,    -1,   793,    -1,   795,    -1,    -1,
      -1,   129,    -1,  1762,    -1,    -1,    -1,    -1,   153,    -1,
     752,    -1,    -1,    -1,  1576,    -1,   814,    -1,    -1,    -1,
      -1,   149,   150,    -1,  1421,    -1,    -1,    -1,    -1,  1426,
      -1,    -1,   160,   161,  1431,  1432,  1433,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1606,    -1,    58,    -1,    -1,  1611,
    1612,    -1,    -1,    -1,   852,    -1,    -1,    -1,    -1,    -1,
      -1,   859,    -1,    -1,    -1,    -1,   864,   865,    -1,    -1,
      82,    -1,    -1,    -1,    -1,   873,    -1,   875,    -1,    -1,
      -1,  1840,    -1,    -1,    -1,    -1,    98,    -1,   886,    -1,
     102,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    12,    13,    14,    15,    16,    -1,    -1,   359,    -1,
      -1,   362,   363,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   372,   373,    -1,    -1,    -1,    -1,   139,   926,    -1,
      -1,   873,    -1,   145,   876,   147,   387,   388,    -1,   151,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   161,
     162,   163,    -1,    -1,    -1,    -1,    -1,   408,    -1,    70,
      -1,    -1,  1107,   101,    -1,    -1,    -1,   179,   106,   107,
     108,   109,   110,   111,   112,   113,    -1,    -1,    -1,   191,
     192,    -1,    -1,   195,   926,    70,    -1,   438,    -1,    -1,
     101,    -1,    -1,   991,    -1,   106,   107,   108,   109,   110,
     111,   112,  1599,    -1,  1601,    -1,    -1,    -1,    -1,    -1,
    1008,    -1,    -1,    -1,    -1,   153,   101,  1015,   129,   104,
     105,   106,   107,   108,   109,   110,   111,   112,    -1,    -1,
     242,    -1,    -1,   245,    -1,    -1,    82,    -1,   149,   150,
      -1,    -1,    -1,  1559,    -1,    -1,   258,    -1,    -1,   160,
     161,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1057,
      -1,    -1,    -1,   275,   149,   150,    -1,    -1,    -1,    -1,
      -1,   283,    -1,    -1,    -1,    -1,   288,    -1,   101,    -1,
      -1,    -1,   294,   106,   107,   108,   109,   110,   111,   112,
     302,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1097,
      -1,   147,    -1,    -1,    -1,    -1,   129,  1105,   320,    -1,
     322,   323,    -1,    -1,    -1,    -1,   162,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   337,   149,   150,    -1,   341,
     153,    -1,    -1,   179,    -1,    -1,  1134,   160,   161,    -1,
      -1,    -1,    -1,    -1,    -1,  1907,   192,    -1,    -1,    -1,
      -1,    -1,    -1,  1151,  1152,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   374,    -1,    -1,    -1,    -1,  1165,    -1,    -1,
      -1,    -1,    -1,  1171,    -1,    -1,    -1,    -1,    -1,    -1,
    1178,    -1,    -1,    -1,   101,    -1,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   408,    -1,    -1,   245,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1205,    -1,    -1,
      -1,  1209,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1365,    -1,   434,    -1,    -1,    -1,   438,    -1,    -1,    -1,
      -1,    -1,   444,    -1,    -1,    -1,  1178,   688,   689,   690,
     691,   692,   693,   694,   695,   696,   697,   698,   699,   700,
     701,   702,   703,   704,   705,   706,   302,   174,    -1,    73,
      -1,    -1,    -1,  1205,    -1,    -1,    -1,    -1,    -1,    -1,
    1786,    -1,  1270,  1271,   320,    -1,    -1,    -1,    -1,   101,
      -1,    95,   104,   105,   106,   107,   108,   109,   110,   111,
     112,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   513,   514,    -1,    -1,    -1,   518,   519,    -1,    -1,
    1308,  1309,  1310,  1311,  1312,  1313,    -1,   768,    -1,    -1,
    1318,  1319,   101,    -1,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,    -1,   150,    -1,   550,    -1,
      -1,    -1,    -1,   555,    -1,    -1,   558,   559,    -1,   561,
     129,    -1,   174,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     572,    -1,   408,    -1,    -1,    -1,    -1,    -1,    -1,  1367,
     149,   150,    -1,    -1,   153,   587,   588,   589,    -1,   591,
      -1,   160,   161,  1528,    -1,  1530,    -1,    -1,  1386,    -1,
      -1,    -1,   438,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     612,    -1,    -1,   615,    -1,   219,   220,   619,    -1,    -1,
     622,  1409,    -1,   625,    -1,   627,    -1,  1562,    -1,    -1,
     101,   633,   236,   104,   105,   106,   107,   108,   109,   110,
     111,   112,    -1,    -1,   646,    -1,   648,   649,    -1,   651,
      -1,    -1,   654,    -1,  1386,   657,   658,   659,    -1,    -1,
      -1,    -1,    12,    13,    14,    15,    16,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   513,   514,    -1,
      -1,   922,    -1,   519,    -1,    -1,   927,    -1,    -1,   160,
      -1,   295,   296,    -1,    -1,   101,    -1,   938,   104,   105,
     106,   107,   108,   109,   110,   111,   112,  1495,    -1,   711,
      -1,  1499,    -1,  1501,    -1,    -1,    -1,  1505,    -1,  1507,
      70,    -1,    -1,    -1,    -1,  1660,    -1,    -1,    -1,   374,
      -1,    -1,    -1,   735,    -1,    -1,   572,   978,    -1,    -1,
      -1,  1529,    -1,  1531,    -1,    -1,   152,  1682,    -1,    -1,
     752,   101,    -1,   589,   104,   105,   106,   107,   108,   109,
     110,   111,   112,    -1,    -1,    -1,  1554,    -1,    -1,    -1,
      -1,    -1,   774,   775,    -1,    -1,   612,  1565,   101,   129,
    1568,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      -1,   793,    -1,    -1,    -1,    -1,    -1,   633,    -1,   149,
     150,    -1,    -1,  1591,    -1,    -1,    -1,    -1,    -1,    -1,
     160,   161,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   821,
      -1,  1756,    -1,    -1,    -1,    -1,    -1,    -1,  1616,  1617,
      -1,    -1,    -1,    -1,   157,    -1,    -1,    -1,   442,    -1,
      -1,    -1,   844,  1631,  1632,    -1,    -1,    -1,    -1,  1784,
     852,    -1,    -1,   855,    -1,    -1,    -1,   859,  1646,    -1,
      -1,  1649,   864,   865,  1105,    -1,    -1,    -1,   513,   514,
      -1,   873,    -1,   875,   876,   711,   878,    -1,   482,    -1,
      -1,    -1,  1670,  1671,    -1,    -1,    -1,   491,    -1,  1824,
    1825,    -1,    -1,    -1,    -1,    -1,    -1,  1832,    -1,   735,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   511,    -1,    -1,
      -1,  1699,    -1,   517,    -1,    -1,   752,  1852,    -1,  1854,
    1855,    -1,    -1,    -1,   926,   101,    -1,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,  1178,   774,   775,
     101,    -1,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,  1740,   557,    -1,    -1,    -1,   793,    -1,    -1,
      -1,  1749,    -1,    -1,   568,  1900,   570,    -1,    -1,    -1,
      -1,  1212,  1213,  1214,  1762,  1910,   152,    -1,  1219,  1220,
      -1,   157,    -1,    -1,  1772,   987,    -1,    -1,    -1,    -1,
      -1,  1779,    -1,    -1,    -1,    -1,   157,    -1,    -1,    -1,
    1241,   646,  1004,  1005,    -1,  1940,   651,  1942,    -1,   654,
      -1,    -1,    -1,    -1,    -1,    -1,  1804,    12,    13,    14,
      15,    16,  1957,   859,  1812,    -1,    -1,  1962,   673,   865,
      -1,  1819,    -1,    -1,    -1,    -1,    -1,  1278,  1279,    -1,
      -1,  1976,    -1,    -1,    -1,    -1,    -1,  1779,  1836,  1837,
      -1,  1839,  1840,    -1,    -1,  1057,    -1,    -1,    -1,    -1,
      -1,    -1,   707,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   101,    -1,    -1,    -1,    70,   106,   107,   108,   109,
     110,   111,   112,   113,  1872,    -1,    -1,   117,    70,   119,
     926,    -1,    -1,    -1,    -1,  1097,    -1,    -1,    -1,    -1,
      -1,  1889,  1890,  1105,     1,  1837,   101,     4,    -1,   104,
     105,   106,   107,   108,   109,   110,   111,   112,    -1,   101,
     150,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,    -1,  1134,    -1,   129,    -1,    -1,    -1,    -1,  1927,
      12,    13,    14,    15,    16,    -1,    -1,   129,    -1,  1151,
    1152,   755,    -1,   757,   149,   150,    -1,    -1,   153,    -1,
      -1,    58,    -1,    -1,    -1,   160,   161,   149,   150,  1171,
      -1,    -1,    -1,    -1,    -1,    -1,  1178,    -1,   160,   161,
    1182,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,
      -1,   795,  1194,    -1,    -1,    -1,    -1,    -1,    70,    -1,
      -1,    -1,    -1,  1205,    -1,   102,   101,  1209,    -1,    -1,
     814,   106,   107,   108,   109,   110,   111,   112,   113,    -1,
      -1,  1057,   117,    -1,   119,    -1,    -1,    -1,    -1,   101,
      -1,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,    -1,   139,    -1,    -1,    -1,    -1,    -1,   145,  1251,
    1252,    -1,    -1,    -1,    -1,   150,    -1,   129,   153,    -1,
      -1,    -1,    -1,  1265,  1266,   162,    -1,    -1,  1270,  1271,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,   150,    -1,
      -1,   153,    -1,   180,    -1,    -1,    -1,    -1,   160,   161,
      -1,    -1,    -1,    -1,   191,   192,  1298,  1299,    -1,    -1,
      -1,     1,    -1,    -1,    -1,    -1,  1308,  1309,  1310,  1311,
    1312,  1313,    -1,    -1,    -1,    70,    -1,  1319,  1559,    -1,
      -1,   101,    -1,   220,   104,   105,   106,   107,   108,   109,
     110,   111,   112,    -1,    -1,  1171,    -1,    -1,    -1,   236,
      -1,    -1,  1178,    -1,   241,   242,   101,    -1,   245,   104,
     105,   106,   107,   108,   109,   110,   111,   112,    58,    12,
      13,    14,    15,    16,    -1,  1367,    -1,    -1,    -1,  1205,
     267,    -1,   152,   270,   129,   272,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1386,    -1,   283,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   149,   150,    -1,    -1,    -1,   296,
      -1,    -1,   102,    -1,  1008,   160,   161,  1409,    -1,    -1,
    1014,  1015,    -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,
      -1,    -1,    -1,   320,    -1,    -1,   323,    -1,    -1,    -1,
      70,    -1,    -1,    -1,  1270,  1271,    -1,    -1,    -1,    -1,
     337,    -1,    -1,    -1,   341,   145,    -1,    -1,   101,    -1,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      -1,   101,   162,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,    -1,    -1,    -1,   129,    -1,    -1,  1481,
    1482,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,
      -1,    -1,   192,    -1,    -1,    -1,   149,   150,    -1,  1501,
      -1,    12,    13,    14,    15,    16,    -1,   160,   161,   149,
     150,  1752,    -1,    -1,    -1,  1517,    -1,    -1,    -1,    -1,
     160,   161,    -1,    -1,    -1,    -1,    -1,  1529,    -1,  1531,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   434,    -1,    -1,
      -1,   438,    -1,    -1,    -1,  1786,    -1,    -1,    -1,    -1,
    1386,    -1,  1554,    -1,    -1,    -1,    -1,    -1,    -1,    70,
      -1,  1165,    -1,  1565,    -1,    -1,  1568,   267,    -1,    -1,
      -1,    -1,    -1,    -1,  1576,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   283,    -1,    -1,    -1,    -1,    -1,  1591,
     101,    -1,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,    -1,    -1,  1606,    -1,    -1,    -1,    -1,  1611,
    1612,    -1,    -1,    -1,  1616,  1617,    -1,    -1,   129,    -1,
     320,    -1,   519,   323,   101,    -1,    -1,  1868,  1869,   106,
     107,   108,   109,   110,   111,   112,   113,   337,   149,   150,
     117,   341,   119,    -1,    -1,    -1,    -1,    -1,    -1,   160,
     161,    -1,    -1,   550,    -1,  1896,  1260,    -1,   555,    -1,
      -1,    -1,   559,    -1,   561,  1501,    -1,    -1,  1670,  1671,
      -1,    -1,    -1,   150,    -1,   572,   153,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1925,    -1,    -1,    -1,    -1,  1334,
      -1,   588,   589,    -1,    -1,    -1,    -1,    -1,  1343,    -1,
      -1,    -1,  1347,    -1,  1349,    -1,   603,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1318,   612,    -1,    -1,    -1,    -1,
     617,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   625,  1565,
      -1,  1972,    -1,    -1,    -1,    -1,   101,    -1,   438,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   101,    -1,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
    1762,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
    1772,    -1,    -1,    -1,    -1,    -1,    -1,  1779,    -1,    -1,
    1616,  1617,    -1,    -1,   149,   150,    -1,    -1,   153,    -1,
      -1,    -1,    -1,    -1,    -1,   160,   161,   150,    -1,    -1,
     153,    -1,  1804,    -1,    -1,    -1,    -1,    -1,   173,    -1,
      -1,    -1,    -1,    -1,   711,    -1,    -1,  1819,   101,   519,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      -1,   728,    -1,    -1,  1836,  1837,    -1,    -1,  1840,    -1,
      -1,    -1,  1487,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     550,    -1,    -1,    -1,    -1,   752,    -1,    -1,    -1,   559,
     757,   561,    -1,    -1,    -1,    -1,   149,    -1,    -1,    -1,
    1872,    -1,   572,    -1,  1519,    -1,    -1,   774,   775,    -1,
      -1,    -1,     1,    -1,  1529,    -1,  1531,    -1,   588,   589,
      -1,    -1,    -1,    -1,    -1,  1499,   793,    -1,    -1,    -1,
      -1,  1505,    -1,  1507,    -1,  1907,    -1,    -1,    -1,  1554,
     101,    -1,   612,   104,   105,   106,   107,   108,   109,   110,
     111,   112,    -1,    -1,   821,   625,   101,    -1,    -1,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   129,    58,
      -1,    -1,    -1,   101,    -1,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   852,    -1,    -1,   149,   150,
      -1,    -1,   859,    -1,    -1,   156,    -1,   864,  1804,   160,
     161,   129,    -1,    -1,   149,    -1,   873,    -1,    -1,   876,
      -1,   878,    -1,   102,    -1,    -1,   883,    -1,    -1,    -1,
      -1,   149,   150,    -1,    -1,   153,    -1,    -1,    -1,    -1,
      -1,    -1,   160,   161,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   711,    -1,    -1,    -1,    -1,    -1,  1662,    -1,    -1,
      -1,  1666,    -1,    -1,    -1,    -1,   145,    -1,  1673,   926,
     132,   133,   134,   135,   136,   137,   138,   139,   140,   141,
     142,    -1,  1646,   162,   146,  1649,    -1,    -1,    -1,    -1,
     101,    -1,   752,   104,   105,   106,   107,   108,   109,   110,
     111,   112,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   173,    -1,   192,   774,   775,    -1,    -1,   129,    -1,
      -1,     1,    -1,    -1,     4,    -1,    -1,    -1,    -1,    -1,
     987,    -1,    -1,   793,    -1,  1699,    -1,    -1,   149,   150,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1004,  1005,   160,
     161,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   821,    -1,    -1,  1769,  1770,   101,  1772,    -1,   104,
     105,   106,   107,   108,   109,   110,   111,   112,    58,    -1,
      -1,    -1,    -1,    -1,    -1,  1749,    -1,    -1,    -1,    -1,
      -1,    -1,   852,    -1,   129,    -1,    -1,    -1,    -1,   859,
    1057,    -1,    82,    -1,   283,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   873,   149,   150,   876,    -1,   878,    -1,
      -1,    -1,   102,  1828,   101,   160,   161,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    -1,    -1,    -1,    -1,
    1097,   320,    -1,    -1,   323,   101,    -1,    -1,  1105,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   337,   139,
      -1,   117,   341,   119,    -1,   145,   926,   147,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1839,  1881,  1134,  1883,    99,
      -1,   101,    -1,   103,   104,   105,   106,   107,   108,   109,
     110,   111,   112,    -1,   150,  1152,    -1,   153,  1903,   179,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   191,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1178,    -1,    -1,    -1,  1889,  1890,   987,    -1,   149,
    1187,    -1,   152,   153,    -1,    -1,    -1,    -1,  1943,  1944,
    1945,    -1,    -1,    -1,  1004,  1005,    -1,    -1,  1205,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   438,
      -1,    -1,   242,  1927,    -1,   245,    -1,    -1,    -1,    -1,
     250,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1251,  1252,    -1,  1057,    -1,    -1,
      -1,    -1,    -1,   283,    -1,    -1,    -1,    -1,  1265,  1266,
      -1,    -1,    -1,  1270,  1271,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   302,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     519,  1298,  1299,    -1,    -1,  1105,    -1,    -1,    -1,    -1,
      -1,  1308,  1309,  1310,  1311,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   550,    -1,    -1,  1134,    -1,    -1,    -1,    -1,    -1,
     559,    -1,   561,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1152,   572,   374,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   588,
     589,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1178,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   408,  1386,
      -1,    -1,    -1,   612,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1205,   625,    -1,    -1,    -1,
      -1,    -1,  1409,    -1,   434,    12,    13,    14,    15,    16,
      -1,    -1,    19,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    -1,
      -1,  1251,  1252,    50,    51,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1265,  1266,    -1,    -1,    -1,
    1270,  1271,    -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1481,  1482,    -1,    -1,    -1,    -1,
      -1,    -1,   711,   513,   514,    -1,    -1,    -1,  1298,  1299,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1507,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1517,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1529,   752,  1531,   555,    -1,    -1,    -1,   559,
      -1,   561,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   774,   775,  1554,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1565,    -1,
      -1,  1568,    -1,    -1,   793,    -1,    -1,    -1,    -1,  1576,
      -1,    -1,    -1,    -1,    -1,    -1,  1386,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   821,    -1,    -1,    -1,    -1,    -1,    -1,  1606,
      -1,    -1,    -1,   633,  1611,  1612,    -1,    -1,    -1,  1616,
    1617,    -1,    -1,    -1,    -1,    -1,   646,    -1,   648,   649,
      -1,   651,    -1,   852,   654,  1632,    -1,   657,   658,   659,
     859,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   873,    -1,    -1,   876,    -1,   878,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1671,    -1,    -1,    -1,    -1,    -1,
      -1,  1481,  1482,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     1,    -1,    -1,     4,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   926,    -1,    -1,
      -1,    -1,    -1,     1,    -1,   735,     4,  1517,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1529,
      -1,  1531,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    58,    -1,
      -1,    -1,    -1,    -1,  1554,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1565,    -1,    -1,   987,    -1,
      58,    -1,    82,    -1,    -1,  1772,  1576,    -1,    -1,    -1,
      -1,    -1,  1779,    -1,    -1,  1004,  1005,    -1,    -1,    -1,
      -1,    -1,   102,    -1,    82,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1606,  1804,    -1,    -1,
      -1,  1611,  1612,    -1,   102,    -1,  1616,  1617,    -1,    -1,
      -1,    -1,  1819,    -1,    -1,    -1,    -1,    -1,    -1,   139,
      -1,    -1,   852,    -1,    -1,   145,    -1,   147,  1057,  1836,
    1837,    -1,    -1,    -1,   864,   865,    -1,    -1,    -1,    -1,
      -1,   139,    -1,    -1,    -1,    -1,    -1,   145,    -1,   147,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   179,
      -1,  1671,    -1,    -1,    -1,  1872,    -1,    -1,    -1,    -1,
      -1,   191,    -1,    -1,    -1,    -1,  1105,    -1,    -1,    -1,
      -1,   179,    -1,  1890,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   191,    -1,    -1,    -1,    -1,    -1,    -1,
    1907,    -1,    -1,    -1,    -1,  1134,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   242,  1152,    -1,   245,    -1,    -1,    -1,    -1,
     250,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   242,    -1,    -1,   245,    -1,  1178,
      -1,    -1,   250,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1772,   283,    -1,    -1,    -1,    -1,    -1,  1779,
      -1,    -1,    -1,    -1,    -1,    -1,  1205,    -1,    -1,    -1,
      -1,    -1,   302,    -1,    -1,   283,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1804,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   302,    -1,    -1,    -1,    -1,  1819,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1251,  1252,    -1,    -1,  1836,  1837,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1265,  1266,    -1,    -1,
      -1,  1270,  1271,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   374,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1872,    -1,    -1,    -1,    -1,  1097,    -1,  1298,
    1299,    -1,    -1,    -1,    -1,  1105,   374,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   408,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1907,    -1,    -1,
      -1,    -1,    -1,    -1,  1134,    -1,    -1,    -1,    -1,    -1,
     408,    -1,    -1,    -1,   434,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1152,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   434,    -1,    -1,    -1,
      -1,  1171,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1386,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1209,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   513,   514,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   513,   514,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   555,    -1,    -1,    -1,   559,
      -1,   561,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1481,  1482,    -1,    -1,    -1,   555,    -1,    -1,
      -1,   559,    -1,   561,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1308,  1309,
    1310,  1311,  1312,  1313,    -1,    -1,    -1,    -1,  1517,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1529,    -1,  1531,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   633,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1554,   646,    -1,   648,   649,
      -1,   651,    -1,    -1,   654,   633,  1565,   657,   658,   659,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1576,   646,    -1,
     648,   649,    -1,   651,    -1,    -1,   654,    -1,    -1,   657,
     658,   659,    -1,    -1,    -1,    86,    -1,    -1,    -1,    -1,
      -1,    92,    93,    -1,    -1,    -1,    -1,  1606,    -1,  1409,
      -1,    -1,  1611,  1612,    -1,    -1,    -1,  1616,  1617,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   126,    -1,     0,    -1,    -1,
       3,    -1,    -1,    -1,    -1,   735,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   735,    -1,    -1,
      -1,    -1,  1671,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1501,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    76,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1529,
      -1,  1531,    -1,    -1,    -1,    -1,    -1,    98,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,    -1,
     111,    -1,   113,    -1,  1554,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   852,    -1,    -1,    -1,    -1,    -1,  1568,    -1,
      -1,    -1,   135,  1772,   864,   865,    -1,    -1,    -1,    -1,
    1779,    -1,    -1,    -1,   852,    -1,    -1,    -1,    -1,    -1,
     151,  1591,   153,   154,    -1,    -1,   864,   865,    -1,    -1,
      -1,   292,    -1,    -1,    -1,  1804,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1819,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   195,    -1,    -1,  1836,  1837,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   229,    -1,    -1,    -1,
      -1,  1671,    -1,  1872,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   244,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   254,    -1,    -1,    -1,    -1,    -1,   258,    -1,   260,
     261,   264,    -1,    -1,    -1,    -1,    -1,    -1,  1907,    -1,
      -1,    -1,    -1,    -1,    -1,   278,   279,    -1,    -1,    -1,
      -1,    -1,   285,   286,    -1,    -1,    -1,   288,    -1,    -1,
      -1,    -1,    -1,   294,    -1,    -1,    -1,    -1,   301,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   437,    -1,   439,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   319,   448,   449,    -1,
      -1,   322,    -1,    -1,    -1,    -1,    -1,   328,    -1,   330,
      -1,    -1,  1772,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1097,    -1,    -1,
      -1,    -1,   375,    -1,    -1,  1105,    -1,    -1,    -1,  1819,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1097,
      -1,    -1,    -1,    -1,    -1,    -1,  1836,  1105,    -1,    -1,
      -1,    -1,    -1,   406,  1134,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   418,    -1,    -1,
      -1,    -1,  1152,    -1,    -1,   556,  1134,    -1,    -1,   432,
      -1,    -1,  1872,   436,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1171,    -1,   444,  1152,   446,   447,    -1,    -1,    -1,
      -1,    -1,   455,    -1,    -1,    -1,   459,   460,    -1,    -1,
     463,    -1,    -1,  1171,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   478,   479,   480,   481,  1209,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     491,    -1,    -1,    -1,   497,    -1,    -1,    -1,    -1,    -1,
      -1,  1209,   505,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     511,    -1,    -1,    -1,    -1,   516,    -1,   518,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     533,    -1,    -1,    -1,    -1,    -1,    -1,   538,    -1,   540,
     541,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   558,    -1,    -1,
      -1,   564,    -1,    -1,    -1,    -1,    -1,    -1,   571,   570,
      -1,    -1,    -1,   576,    -1,    -1,    -1,    -1,  1308,  1309,
    1310,  1311,  1312,  1313,    -1,    -1,    -1,    -1,    -1,    -1,
     591,    -1,   593,   594,    -1,    -1,   599,   600,    -1,    -1,
    1308,  1309,  1310,  1311,  1312,  1313,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   615,   616,    -1,    -1,    -1,    -1,
      -1,   622,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   768,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   660,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1409,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1409,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      47,    -1,    -1,    -1,   845,   846,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   727,   856,   857,   858,    -1,    -1,
     861,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,   742,
      -1,    -1,    -1,   746,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   755,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1501,    -1,    -1,   777,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   119,   786,    -1,    -1,    -1,    -1,    -1,   792,
      -1,    -1,    -1,  1501,    -1,   132,    -1,   134,    -1,  1529,
      -1,  1531,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   940,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1529,    -1,  1531,  1554,    -1,   829,   164,    -1,    -1,
      -1,    -1,    -1,   836,    -1,    -1,    -1,    -1,  1568,    -1,
      -1,    -1,    -1,   844,   181,    -1,  1554,    -1,    -1,    -1,
      -1,    -1,    -1,   984,   855,    -1,    -1,    -1,    -1,   862,
    1568,  1591,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   875,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   219,  1591,    -1,   886,   223,    -1,    -1,   226,
     227,    -1,    -1,   230,   895,    -1,   233,   234,    -1,    -1,
    1031,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1040,
    1041,  1042,  1043,   916,    -1,    -1,    -1,  1048,  1049,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1058,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1671,    -1,    -1,    -1,    -1,    -1,    -1,  1079,    -1,
    1081,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   295,    -1,
      -1,   298,    -1,  1671,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    47,    -1,    -1,    -1,    -1,
      -1,   318,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     991,    -1,    -1,    -1,   997,    -1,   333,    -1,  1001,    -1,
      -1,    -1,    -1,  1134,    -1,  1008,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1015,  1018,    -1,    -1,    -1,    -1,
      -1,    -1,  1025,    -1,    -1,    -1,    -1,    -1,    -1,  1160,
      -1,  1034,    -1,  1036,    -1,  1166,    -1,  1168,  1169,    -1,
      -1,    -1,  1772,    -1,    -1,    -1,  1177,   119,  1179,    -1,
    1181,    -1,  1183,    -1,    -1,    -1,    -1,  1188,    -1,    -1,
     132,    -1,   134,    -1,  1772,  1068,    -1,    -1,    -1,  1072,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1086,    -1,    -1,  1089,    -1,    -1,  1819,
     427,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1836,    -1,    -1,    -1,
      -1,  1819,    -1,    -1,    -1,  1246,    -1,    -1,    -1,    -1,
      -1,    -1,  1253,  1254,    -1,    -1,    -1,    -1,  1836,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1872,    -1,    -1,   482,  1277,    -1,    -1,    -1,
    1151,    -1,    -1,  1284,   226,   227,  1287,   494,   230,    -1,
      -1,   233,   234,    -1,  1872,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1176,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1182,    -1,    -1,    -1,    -1,  1317,  1188,    -1,   162,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1200,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   191,   192,
      -1,    -1,    -1,    -1,    -1,    -1,  1357,    -1,    -1,    -1,
      -1,   568,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   318,    -1,    -1,    -1,
     223,    -1,    -1,    -1,    -1,    -1,    -1,   230,    -1,  1390,
      -1,   333,    -1,    -1,    -1,    -1,    -1,  1398,    -1,  1400,
     607,   608,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   620,    -1,    -1,    -1,  1290,    -1,    -1,
      -1,  1294,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1446,  1447,    -1,  1319,    -1,
      -1,  1324,    -1,    -1,    -1,   298,    -1,    -1,    -1,    -1,
    1461,  1462,    -1,  1464,  1337,  1338,    -1,    -1,    -1,    -1,
      -1,    -1,  1473,    -1,    -1,    -1,    -1,   320,   321,    -1,
      -1,    -1,  1483,  1484,    -1,   427,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1367,    -1,   341,    -1,
      -1,  1374,    -1,    -1,  1377,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1392,
      -1,    -1,    -1,    -1,   731,   732,    -1,    -1,    -1,    -1,
     737,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   758,   494,    -1,   761,   762,    -1,   764,    -1,   766,
     767,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   411,  1442,
      -1,    -1,    -1,    -1,    -1,    -1,  1577,  1578,  1451,    -1,
      -1,    -1,  1455,    -1,   427,   428,  1587,   430,   431,    -1,
      -1,    -1,    -1,    -1,    -1,   438,  1469,  1470,   805,   442,
      -1,    -1,   809,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1622,  1623,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     483,    -1,    -1,    -1,   487,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   607,   608,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   519,   884,   620,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1569,  1570,    -1,    -1,
    1701,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1719,    -1,
      -1,  1722,  1723,    -1,    -1,    -1,   569,    -1,  1729,   572,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   588,   589,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   599,    -1,    -1,    -1,
     603,    -1,    -1,    -1,    -1,    -1,    -1,   610,    -1,   612,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1649,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   731,
     732,    -1,    -1,    -1,    -1,   737,    -1,    -1,    -1,  1670,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1014,    -1,    -1,
      -1,    -1,    -1,    -1,  1687,    -1,   758,    -1,    -1,   761,
     762,    -1,   764,    -1,   766,   767,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1709,  1838,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1727,  1062,    -1,    -1,  1065,    -1,
      -1,    -1,    -1,   805,    -1,    -1,    -1,   809,   711,  1740,
      -1,    -1,    -1,    -1,    -1,  1876,    -1,    -1,    -1,    -1,
      -1,  1754,    -1,  1884,   727,   728,  1887,    -1,    -1,    -1,
      -1,  1762,    -1,    -1,   737,   738,    -1,   740,   741,    -1,
      -1,    -1,  1775,    -1,    -1,  1778,    -1,    -1,    -1,   752,
    1911,    -1,   755,    -1,   757,   758,    -1,    -1,    -1,    -1,
      -1,   764,  1923,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   774,   775,    -1,    -1,    82,    -1,  1938,    -1,    -1,
      -1,    -1,   884,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     793,    98,    -1,    -1,   797,    -1,    -1,    -1,   801,    -1,
      -1,    -1,   805,   806,    -1,    -1,   809,   810,    -1,  1840,
      -1,    -1,    -1,    -1,   817,    -1,    -1,  1184,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1192,  1193,    -1,    -1,    -1,
    1863,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     147,    -1,    -1,    -1,   151,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   162,   859,   860,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   179,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1247,    -1,    -1,   886,    -1,   192,    -1,    -1,   195,  1256,
      -1,    -1,  1259,    -1,  1261,  1262,  1927,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1014,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   926,    -1,    -1,    -1,    -1,    -1,    55,
      56,    -1,    -1,    -1,  1301,    -1,    -1,    -1,   245,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   258,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1062,    -1,    -1,  1065,    90,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   294,   991,    -1,
      -1,    -1,    -1,    -1,    -1,   302,    -1,    -1,    -1,    -1,
      -1,  1368,    -1,    -1,    -1,  1008,  1009,    -1,   179,    -1,
      -1,    -1,  1015,   320,   140,   322,    -1,   143,    -1,    -1,
      -1,   192,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     156,    -1,    -1,    -1,   205,    -1,   207,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1057,    -1,    -1,    -1,    -1,  1062,
    1063,    -1,  1065,  1066,    -1,    -1,    -1,   374,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1184,  1450,   210,    -1,    -1,    -1,    -1,    -1,
    1192,  1193,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   408,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1479,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   293,    -1,    -1,    -1,    -1,    -1,    -1,   255,
      -1,   438,    -1,    -1,    -1,    -1,    -1,   444,  1505,   265,
      -1,    -1,    -1,    -1,  1511,  1247,    -1,    -1,    -1,    -1,
     276,    -1,    -1,    -1,  1256,    -1,    -1,  1259,    -1,  1261,
    1262,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1178,    -1,   303,    -1,    -1,
      -1,  1184,  1185,   309,   310,    -1,    -1,    -1,   314,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1564,    -1,  1301,
      -1,    -1,  1205,    -1,    -1,    -1,   513,   514,    -1,    -1,
      -1,    -1,   519,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     346,    -1,    -1,    -1,    -1,   351,    -1,    -1,   354,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1247,  1248,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1256,  1257,    -1,  1259,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   572,  1368,  1270,  1271,    -1,
      -1,  1638,  1639,    -1,    -1,    -1,    -1,    -1,    -1,  1646,
      -1,    -1,   589,  1650,   591,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   464,    -1,    -1,    -1,    -1,    -1,   470,
      -1,    -1,    -1,    -1,   475,   612,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   441,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   633,   453,   454,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   646,
      -1,   648,   649,    -1,   651,    -1,    -1,   654,  1450,    -1,
     657,   658,   659,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1742,    -1,  1479,    -1,    -1,
      -1,    -1,    -1,  1386,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   562,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   711,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   589,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   735,    -1,
      -1,   602,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1808,    -1,    -1,    -1,   752,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   583,    -1,    -1,
      -1,    -1,  1564,    -1,    -1,    -1,    -1,   774,   775,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   653,    -1,    -1,    -1,   793,    -1,    -1,    -1,
      -1,    -1,  1495,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     671,   672,    -1,    -1,    -1,   631,    -1,    -1,  1511,    -1,
     681,    -1,   683,   684,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1889,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1638,  1639,    -1,    -1,
     711,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1650,    -1,
      -1,    -1,   859,   724,    -1,    -1,    -1,    -1,   865,    -1,
      -1,    -1,  1565,    -1,   735,    -1,    -1,    -1,   875,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   749,    -1,
      -1,   752,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   779,    -1,
      -1,   782,    -1,  1616,  1617,    -1,    -1,    -1,    -1,   926,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1631,  1632,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1742,    -1,     3,   769,  1647,    -1,    -1,   818,    -1,    -1,
     776,    12,    13,    14,    15,    16,    -1,    -1,    19,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    -1,    49,    50,
      51,    -1,    53,    -1,   865,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   875,   876,  1808,    -1,    -1,    70,
      -1,    -1,   883,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   851,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   904,    -1,    -1,    -1,  1740,    -1,    -1,
      -1,    -1,   913,   104,   105,  1748,    -1,    -1,    -1,    -1,
    1057,    -1,    -1,    -1,    -1,   926,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   934,    -1,    -1,    -1,    -1,   129,    -1,
     941,    -1,    -1,    -1,    -1,   901,    -1,    -1,    -1,    -1,
     906,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1097,   152,   153,    -1,    -1,    -1,    -1,    -1,    -1,   160,
     161,  1804,    -1,    -1,    -1,  1808,  1809,    -1,    -1,  1812,
      -1,    -1,    -1,     5,   985,    -1,    -1,    -1,    -1,    -1,
      12,    13,    14,    15,    16,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1839,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1151,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    -1,    -1,    -1,
      52,    -1,    54,    -1,  1171,    -1,    -1,    -1,    -1,    -1,
      -1,  1178,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,
      -1,  1052,    -1,  1054,    -1,  1056,  1889,  1890,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1205,    -1,
      -1,    -1,  1209,    -1,    -1,    -1,    98,    99,    -1,   101,
      -1,   103,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,  1927,   117,   118,   119,    -1,   121,
     122,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1122,  1123,    -1,    -1,    -1,    -1,   149,    -1,    -1,
     152,   153,    -1,  1270,  1271,    -1,    -1,   159,   160,   161,
     162,   163,   164,   165,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1110,  1111,  1112,    -1,     5,    -1,
      -1,    -1,    -1,    -1,    -1,    12,    13,    14,    15,    16,
      -1,  1308,  1309,  1310,    -1,  1312,  1313,    -1,    -1,    -1,
      -1,  1182,  1319,    -1,  1140,    -1,    -1,  1188,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1155,
      -1,    48,    -1,    -1,  1205,    52,    -1,    54,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1222,    -1,    70,    71,    -1,    -1,    -1,    -1,    -1,
    1367,    -1,    -1,    -1,  1235,  1191,    -1,  1238,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1386,
      -1,    98,    99,    -1,   101,    -1,   103,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,    -1,
     117,   118,   119,    -1,   121,   122,    -1,    -1,    -1,    -1,
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,  1289,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   149,    -1,    -1,   152,   153,    -1,    -1,    -1,
      -1,    -1,   159,   160,   161,   162,   163,   164,   165,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1327,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1345,    -1,    -1,  1348,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1501,    -1,  1322,    -1,    -1,  1325,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1386,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1396,  1397,    -1,    -1,    -1,
      -1,    -1,    48,    -1,    -1,    -1,    52,    -1,    54,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1421,    -1,  1423,    -1,    -1,    71,    -1,    -1,  1565,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    98,    99,  1591,   101,    -1,   103,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
      -1,   117,   118,   119,    -1,   121,   122,    -1,    -1,  1616,
    1617,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1501,    -1,    -1,   149,    -1,  1506,   152,   153,    17,    -1,
      -1,    -1,    -1,   159,   160,   161,   162,   163,   164,   165,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   173,   174,    -1,
      -1,    -1,    -1,  1670,    -1,    -1,    -1,    -1,    -1,    48,
      -1,    -1,    -1,    52,    -1,    54,    -1,    -1,    -1,  1550,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1514,    -1,
      69,    -1,    71,    -1,    -1,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,    87,    88,
      89,    90,    91,    92,    93,    94,    -1,    96,  1589,    98,
      99,  1592,   101,    -1,   103,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,    -1,   121,   122,    -1,    -1,    -1,    -1,    48,    -1,
     129,    -1,    52,    -1,    54,  1762,    -1,    -1,  1584,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     149,    71,  1779,   152,   153,    -1,    -1,    -1,   157,    -1,
     159,   160,   161,   162,   163,   164,   165,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   174,    -1,  1804,    98,    99,
      -1,   101,    -1,   103,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,    -1,   117,   118,   119,
      -1,   121,   122,    -1,    -1,    -1,    -1,    -1,    -1,   129,
    1837,    -1,    -1,  1840,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,
     150,    -1,   152,   153,    -1,    -1,    -1,   157,    -1,   159,
     160,   161,   162,   163,   164,   165,    -1,    -1,    -1,    -1,
      -1,    -1,  1698,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1707,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    -1,    19,  1787,    21,    22,    23,
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
      -1,    -1,   152,   153,    -1,    -1,    -1,    -1,    -1,   159,
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
      -1,    -1,    -1,   152,   153,    -1,    -1,    -1,    -1,    -1,
      -1,   160,   161,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    -1,    -1,    19,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,    17,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    48,
      -1,    -1,    -1,    52,    -1,    54,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   149,   150,    -1,   152,   153,
      69,    -1,    71,    72,    -1,    74,   160,   161,    77,    78,
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
      -1,    -1,    -1,    -1,    -1,    -1,   160,   161,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    -1,    -1,    19,    -1,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    -1,    49,    50,    51,    -1,    53,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    12,
      13,    14,    15,    16,    17,    70,    19,    -1,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    -1,    -1,    -1,    -1,    50,    51,   104,
     105,    -1,    55,    56,    57,    58,    59,    60,    61,    62,
      -1,    12,    13,    14,    15,    16,    17,    70,    19,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    -1,    -1,   152,    -1,    50,
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
      -1,    -1,    -1,   152,    -1,    -1,    12,    13,    14,    15,
      16,   160,   161,    19,    -1,    21,    22,    23,    24,    25,
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
      -1,    -1,    -1,    -1,    -1,    -1,   152,   153,   104,   105,
      -1,    -1,    -1,    -1,   160,   161,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   152,    -1,    -1,    -1,
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
      -1,    -1,    -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   151,   152,     4,     5,     6,     7,     8,
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
      -1,    -1,   131,    -1,    -1,    -1,    -1,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,    19,   152,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    -1,
      -1,    -1,    -1,    50,    51,    -1,    -1,    -1,    55,    56,
      57,    58,    59,    60,    61,    62,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,   104,   105,    19,
      -1,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    -1,    49,
      50,    51,    -1,    53,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   152,    -1,    -1,    -1,    -1,
      70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   104,   105,    -1,    -1,    -1,    -1,
      12,    13,    14,    15,    16,    17,    -1,    19,    -1,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    -1,    -1,    -1,    -1,    50,    51,
      -1,    -1,   152,    55,    56,    57,    58,    59,    60,    61,
      62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,
      -1,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,    -1,    -1,    -1,    12,    13,    14,    15,    16,    -1,
      -1,    19,    -1,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
     152,    49,    50,    51,    -1,    53,    -1,    -1,    48,    -1,
      -1,    -1,    52,    -1,    54,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   104,   105,    98,    99,
      -1,   101,    -1,   103,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,    -1,   117,   118,   119,
      48,   121,   122,    -1,    52,    -1,    54,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    71,   152,    -1,    -1,    -1,    -1,   149,
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
       0,   176,   386,   387,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    19,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    50,    51,    55,    56,    57,
      58,    59,    60,    61,    62,    63,    67,    70,    71,    96,
     100,   101,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   116,   129,   149,   150,   152,   153,   160,   161,   179,
     180,   181,   196,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   291,   293,   295,   296,   297,
     298,   299,   300,   301,   302,   303,   305,   307,   308,   309,
     311,   312,   316,   317,   318,   319,   320,   322,   328,   329,
     330,   331,   342,   345,   378,   381,   391,   396,   398,   404,
     408,   413,   414,   415,   416,   417,   418,   419,   420,   440,
     457,   458,   459,   460,     0,   176,   180,   196,   282,   284,
     293,   296,   308,   312,   317,   115,   149,    56,    59,    60,
      62,   149,   149,   402,   403,   404,   304,   305,   104,   105,
     180,   358,   379,   380,   358,   149,   391,   149,   149,   149,
     196,   403,   408,   414,   415,   416,   418,   419,   420,   104,
     319,   154,   176,   285,   293,   296,   413,   417,   456,   457,
     460,   461,   174,   177,   146,   157,   173,   217,   361,    87,
     155,   397,   358,   177,   177,   177,   174,   104,   105,   149,
     196,   290,   399,   408,   409,   410,   411,   412,   413,   417,
     421,   422,   423,   424,   425,   431,     3,    46,    47,    49,
      53,   310,     3,     4,   153,   196,   284,   297,   301,   303,
     313,   318,   393,   413,   417,   460,   282,   284,   296,   308,
     312,   317,   394,   413,   417,    63,   302,   302,   297,   303,
     302,   297,   302,   297,   152,   402,   155,   177,   149,   157,
     225,   402,   402,   176,   273,   274,   153,   293,   296,   458,
     358,   358,   391,   173,   296,   149,   196,   399,   408,   413,
     422,   153,   196,   460,   392,    63,    64,    65,    66,   153,
     171,   358,   367,   369,   373,   375,   376,   318,    55,   153,
     196,   292,   296,   300,   301,   307,   308,   314,   315,   316,
     317,   321,   328,   329,   345,   354,   356,   440,   452,   453,
     454,   455,   460,   461,   104,   105,   157,   180,   318,   431,
     404,   149,   374,   375,   149,   149,   115,   182,   183,    48,
      52,    54,    71,    98,    99,   101,   103,   113,   114,   117,
     118,   119,   121,   122,   149,   153,   159,   162,   163,   164,
     165,   178,   179,   182,   184,   187,   195,   196,   197,   198,
     201,   202,   203,   204,   205,   206,   207,   208,   209,   210,
     211,   212,   213,   219,   318,   151,   153,   195,   196,   212,
     214,   293,   318,   359,   360,   377,   456,   461,   296,   414,
     415,   416,   418,   419,   420,   151,   151,   151,   151,   151,
     151,   151,   153,   293,   440,   458,   153,   160,   196,   214,
     284,   285,   292,   294,   296,   308,   315,   317,   349,   350,
     353,   354,   355,   452,   460,   149,   413,   417,   460,   149,
     155,   101,   152,   153,   157,   179,   181,   214,   362,   363,
     364,   365,   366,    21,   362,   149,   358,   225,   149,   155,
     155,   155,   403,   408,   410,   411,   412,   421,   423,   424,
     425,   296,   409,   422,   155,    96,   401,   153,   402,   439,
     440,   402,   402,   397,   273,   149,   402,   439,   397,   402,
     402,   296,   399,   149,   149,   295,   296,   293,   296,   176,
     293,   456,   461,   320,   157,   397,   273,   358,   361,   284,
     301,   395,   413,   417,   157,   397,   273,   379,   296,   308,
     296,   296,   104,   319,   104,   105,   180,   318,   323,   379,
     176,   180,   357,   148,   176,     3,   289,   291,   296,   300,
     225,   176,   176,   401,   149,   401,   177,   214,   403,   408,
     296,   149,   176,   358,   157,   358,   157,   358,   131,   160,
     161,   372,   151,   155,   358,   376,   151,   402,   154,   176,
     294,   296,   308,   315,   317,   451,   452,   460,   461,   149,
     153,   161,   173,   196,   440,   441,   442,   443,   444,   445,
     446,   463,   196,   321,   460,   296,   315,   302,   297,   402,
     151,   294,   296,   453,   294,   440,   453,     9,   346,   358,
     343,   157,   367,   173,   367,    12,    86,   101,   104,   105,
     179,   405,   406,   407,   151,   115,   149,   195,   149,   149,
     198,   149,   195,   149,   149,   195,   195,    18,    20,    83,
     153,   162,   163,   199,   200,   214,   221,   225,   331,   359,
     460,   155,   176,   149,   184,   158,   158,   118,   120,   121,
     122,   149,   152,   153,   157,   158,   198,   198,   166,   160,
     167,   168,   162,   163,   123,   124,   125,   126,   169,   170,
     127,   128,   161,   159,   171,   129,   130,   172,   151,   155,
     152,   176,   132,   133,   134,   135,   136,   137,   138,   139,
     140,   141,   142,   173,   216,   217,   218,   149,   196,   435,
     436,   437,   438,   439,   151,   155,   151,   151,   151,   151,
     151,   151,   149,   402,   439,   440,   149,   439,   440,   176,
     293,   458,   176,   177,   177,   149,   161,   196,   408,   426,
     427,   428,   429,   430,   431,   432,   433,   434,   131,   460,
     177,   177,   358,   358,   176,   176,   176,   153,   181,   176,
     363,   156,   155,   462,   362,   152,   153,   156,   366,   150,
     214,   220,   149,   176,   176,   176,   176,   408,   410,   411,
     412,   421,   423,   424,   425,   151,   151,   151,   151,   151,
     151,   151,   409,   422,   402,   149,   361,   154,   176,   225,
     397,   176,   225,   399,   221,   360,   221,   360,   399,   389,
     225,   397,   401,   157,   397,   273,   389,   225,   397,   325,
     326,   324,   157,   131,   296,   351,   352,   355,   356,   151,
     155,    68,   275,   276,   177,   296,   289,   160,   214,   176,
     408,   350,   389,   154,   176,   149,   371,   369,   370,    76,
     306,   180,   294,   440,   453,   296,   300,   460,   176,   442,
     443,   444,   154,   176,    17,   214,   296,   441,   463,   402,
     402,   440,   294,   451,   461,   296,   180,   402,   294,   453,
     318,   155,   462,   173,   217,   347,   157,   346,   151,   360,
     151,   151,   155,   149,   174,   359,   153,   359,   359,   359,
     214,   359,   151,   359,   359,   359,   176,   151,   162,   163,
     200,    17,   298,   151,   155,   151,   160,   161,   151,   220,
     214,   157,   180,   180,   113,   153,   180,   150,   188,   189,
     190,   214,   113,   153,   180,   331,   214,   188,   180,   198,
     201,   201,   201,   202,   202,   203,   203,   204,   204,   204,
     204,   205,   205,   206,   207,   208,   209,   210,   156,   221,
     174,   182,   153,   180,   214,   157,   214,   176,   436,   437,
     438,   296,   435,   402,   402,   214,   360,   149,   402,   439,
     440,   149,   439,   440,   176,   176,   154,   154,   149,   408,
     427,   428,   429,   432,    17,   296,   426,   430,   149,   402,
     445,   463,   402,   402,   463,   149,   402,   445,   402,   402,
     177,   213,   358,   154,   155,   154,   155,   463,   463,   131,
     348,   349,   350,   348,   358,   176,   212,   213,   214,   400,
     462,   362,   364,   148,   176,   151,   155,   176,   348,   180,
     399,   180,   151,   151,   151,   151,   151,   151,   149,   402,
     439,   440,   149,   402,   439,   440,   399,   182,   440,   214,
     225,   351,   151,   151,   151,   151,   387,   388,   225,   389,
     225,   397,   388,   225,   157,   157,   157,   332,   177,   177,
     180,   277,   358,    17,    69,    71,    74,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    90,
      91,    92,    93,    94,    96,   104,   105,   116,   176,   221,
     222,   223,   224,   225,   226,   227,   229,   230,   240,   245,
     246,   247,   248,   249,   250,   255,   256,   262,   263,   264,
     278,   296,   300,   358,   398,    68,   174,   177,   177,   177,
     348,   177,   388,   282,   284,   293,   382,   383,   384,   385,
     377,   173,   368,   368,   294,   453,   153,   160,   196,   214,
     318,   214,   296,   351,   151,   151,   151,     5,   296,   402,
     441,   157,   180,   431,     9,   358,   148,   362,   346,   462,
     157,   151,   406,   188,   151,   176,   155,   151,   151,   155,
     151,   198,   151,   151,   151,   198,    17,   298,   214,   151,
     151,   150,   157,   198,   154,   177,   188,   113,   117,   119,
     181,   191,   192,   193,   151,   155,   191,   154,   155,   148,
     212,   156,   151,   191,   177,   363,   351,   151,   151,   151,
     435,   176,   176,   351,   351,   432,   151,   151,   151,   151,
     149,   408,   431,   426,   430,   176,   176,   154,   177,   463,
     176,   176,   177,   177,   177,   177,   361,   191,   131,   165,
     177,   177,   148,   362,   214,   150,   214,   348,   177,   173,
     149,   402,   439,   440,   149,   402,   439,   440,   176,   176,
     401,   151,   177,   177,   390,   388,   225,   390,   332,   332,
     332,     3,     9,    71,   148,   279,   286,   287,   293,   296,
     333,   338,   456,   151,   155,   155,   174,   149,    59,    60,
     174,   225,   278,   398,   149,    17,   223,   149,   149,   174,
     358,   174,   358,   160,   358,   157,   222,   149,   149,   149,
     225,   214,   215,   215,    13,   265,    72,   231,   174,   177,
     227,    76,   174,   358,    89,   251,   357,   296,   156,   277,
     174,   154,   154,   177,   155,   390,   399,   177,   174,   177,
     174,   177,   151,   360,   374,   374,   176,   177,   177,   177,
     214,   177,   149,   402,   445,   440,   295,     5,   160,   177,
     214,   346,   402,   402,   318,   347,   462,   148,   148,   176,
     151,   180,    76,   185,   186,   359,   198,   198,   198,   198,
     198,   157,   363,   155,   148,   194,   153,   192,   194,   194,
     154,   155,   120,   152,   190,   154,   220,   212,   174,   154,
     462,   177,   149,   402,   439,   440,   351,   351,   177,   177,
     151,   149,   402,   439,   440,   149,   402,   445,   408,   402,
     402,   351,   351,   154,   350,   353,   353,   354,   151,   155,
     155,   151,   177,   213,   213,   154,   154,   177,   177,   151,
     214,   176,   176,   351,   351,   361,   402,   155,   151,   148,
     390,   148,   148,   148,   148,   293,   331,   339,   456,   293,
     338,   149,   327,   174,   174,   149,   156,   196,   334,   335,
     341,   408,   409,   422,   155,   174,   358,   176,   358,   151,
     188,   189,   174,   225,   174,   225,   221,    78,   151,   176,
     151,   176,   174,   174,   221,   174,   363,   174,   221,   220,
     221,   108,   109,   110,   111,   112,   257,   259,   260,   174,
      95,   174,    82,   149,   149,   177,   148,   174,   174,   149,
     223,   225,   402,   174,   151,   176,   148,   148,   176,   155,
     155,   154,   154,   154,   177,   151,   176,   214,   214,   177,
     154,   177,   462,   344,   157,   347,   148,   382,   151,   156,
     151,   155,   156,   363,   462,   220,   118,   191,   192,   153,
     192,   153,   192,   154,   148,   151,   176,   177,   177,   151,
     151,   176,   176,   177,   177,   177,   176,   176,   154,   177,
     151,   402,   351,   351,   177,   177,   221,   148,   327,   327,
     327,   149,   196,   336,   337,   439,   447,   448,   449,   450,
     174,   155,   174,   334,   174,   377,   403,   408,   214,   296,
     155,   174,   340,   341,   340,   358,   131,   355,   356,   221,
     151,   151,   149,   223,   221,   232,   278,   280,   283,   289,
     296,   300,   223,   173,   174,   221,   242,   243,   278,   174,
     462,   151,   151,   151,   225,   259,   260,   149,   214,   149,
     182,   232,   198,   252,   107,   223,   402,   383,   176,   176,
     154,   351,   177,   177,   154,   154,   148,   157,   346,   177,
     214,   186,   214,   462,   148,   154,   154,   191,   191,   351,
     151,   151,   351,   351,   151,   151,   154,   155,   131,   350,
     131,   154,   177,   177,   151,   151,   154,   448,   449,   450,
     296,   447,   155,   174,   402,   402,   174,   151,   408,   402,
     174,   223,    75,    76,   157,   235,   236,   237,   151,   221,
     151,   221,   296,   221,   222,   143,   144,   145,   165,   174,
     244,   151,   156,   222,   148,   157,   237,   223,   149,   176,
     174,   182,   151,   156,   151,   151,   155,   156,   250,   254,
     358,   399,   177,   154,   154,   346,   462,   148,   148,   154,
     154,   177,   177,   177,   176,   177,   151,   151,   151,   151,
     151,   447,   402,   335,   213,   233,   234,   400,   156,   176,
     223,   235,   174,   151,    76,   241,   174,   104,   173,   221,
     222,   221,   241,   243,   174,   174,   176,   176,   261,   294,
     296,   456,   156,   174,   153,   182,   266,   267,   268,   223,
     198,   188,    73,   106,   251,   253,   151,   462,   148,   151,
     151,   151,   353,   149,   402,   439,   440,   337,   131,   155,
     156,   271,   272,   278,   241,   223,   223,   222,   221,   144,
     165,   244,   174,   165,   223,   222,   271,   261,   177,   149,
     196,   399,   447,   180,   156,   101,   149,   151,   156,   155,
      73,   151,   223,   149,   223,   223,   148,   176,   213,   233,
     236,   238,   239,   278,   174,    73,   177,   150,   150,   221,
     222,   221,   177,   238,   177,   174,   258,   296,   266,   154,
     213,   174,   266,   268,   223,   221,   107,   107,   351,   223,
     228,   177,   236,   165,   165,   165,   177,   258,   212,   151,
     156,   182,   151,   151,   156,   151,   254,    73,   249,   177,
     223,   148,   228,   221,   150,   221,   221,   148,   151,   225,
     182,   269,   149,   174,   269,   223,    73,   151,   225,   155,
     156,   213,   151,   223,   182,   180,   270,   151,   174,   151,
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
     240,   240,   240,   240,   240,   241,   241,   242,   242,   243,
     243,   243,   243,   243,   243,   243,   243,   243,   243,   243,
     243,   243,   243,   243,   243,   243,   243,   244,   244,   244,
     244,   245,   245,   245,   245,   245,   245,   245,   245,   245,
     245,   245,   245,   245,   245,   245,   245,   245,   245,   245,
     245,   246,   246,   247,   248,   249,   250,   250,   251,   251,
     252,   252,   253,   254,   254,   254,   254,   254,   254,   255,
     255,   256,   256,   256,   257,   257,   258,   258,   259,   259,
     259,   259,   260,   261,   261,   261,   261,   261,   262,   263,
     263,   264,   264,   264,   264,   264,   265,   265,   266,   266,
     267,   267,   268,   268,   269,   269,   269,   270,   270,   271,
     271,   272,   272,   273,   273,   274,   274,   275,   275,   276,
     276,   277,   277,   278,   278,   278,   279,   279,   280,   280,
     280,   280,   280,   281,   281,   281,   282,   282,   282,   283,
     283,   283,   283,   283,   284,   284,   285,   285,   286,   286,
     286,   287,   287,   287,   287,   287,   288,   288,   289,   289,
     289,   289,   290,   290,   291,   291,   291,   292,   292,   292,
     293,   293,   293,   294,   294,   294,   295,   295,   296,   296,
     297,   297,   298,   298,   298,   298,   298,   299,   300,   300,
     300,   301,   301,   302,   302,   302,   302,   302,   302,   302,
     302,   303,   303,   303,   303,   303,   303,   303,   303,   303,
     303,   303,   303,   303,   303,   303,   303,   303,   303,   303,
     303,   303,   303,   303,   303,   303,   303,   303,   303,   304,
     304,   305,   306,   306,   307,   307,   307,   307,   307,   308,
     308,   309,   309,   309,   309,   310,   310,   310,   310,   310,
     310,   311,   311,   311,   311,   312,   313,   312,   312,   314,
     314,   314,   314,   315,   315,   315,   316,   316,   316,   316,
     317,   317,   317,   318,   318,   318,   318,   318,   318,   319,
     319,   319,   320,   320,   321,   321,   323,   322,   324,   322,
     325,   322,   326,   322,   322,   327,   327,   328,   328,   329,
     329,   330,   330,   330,   331,   331,   331,   331,   331,   331,
     331,   331,   332,   332,   333,   333,   333,   333,   333,   333,
     333,   333,   333,   333,   334,   334,   334,   335,   335,   335,
     336,   336,   336,   337,   338,   338,   339,   339,   340,   340,
     341,   342,   343,   342,   342,   342,   344,   342,   342,   342,
     345,   345,   346,   346,   346,   346,   347,   347,   348,   348,
     348,   348,   348,   348,   348,   349,   349,   349,   349,   350,
     350,   351,   351,   351,   351,   352,   352,   352,   352,   353,
     353,   353,   353,   353,   354,   354,   354,   354,   354,   355,
     355,   356,   356,   357,   357,   358,   358,   358,   359,   359,
     359,   360,   360,   361,   361,   361,   361,   362,   362,   363,
     363,   363,   363,   363,   364,   364,   365,   365,   366,   366,
     366,   366,   366,   367,   367,   368,   368,   370,   369,   371,
     369,   369,   369,   372,   372,   372,   372,   373,   373,   373,
     373,   374,   374,   375,   375,   376,   376,   377,   377,   377,
     377,   378,   378,   378,   379,   379,   380,   380,   381,   381,
     382,   382,   383,   383,   384,   384,   384,   385,   385,   386,
     386,   387,   387,   388,   388,   389,   390,   391,   391,   391,
     391,   391,   392,   391,   393,   391,   394,   391,   395,   391,
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
       4,     2,     6,     1,     2,     1,     2,     1,     2,     1,
       1,     2,     2,     5,     3,     5,    10,     5,    10,     5,
       7,     1,     1,     1,     2,     1,     3,     1,     1,     3,
       3,     2,     1,     2,     2,     0,     1,     2,     3,     8,
       4,     8,     6,     8,     4,     0,     3,     1,     3,     4,
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
       2,     5,     0,     8,     0,     7,     0,     7,     0,     8,
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
#line 6794 "Parser/parser.cc"
    break;

  case 3:
#line 533 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 6800 "Parser/parser.cc"
    break;

  case 4:
#line 540 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantInteger( *(yyvsp[0].tok) ) ); }
#line 6806 "Parser/parser.cc"
    break;

  case 5:
#line 541 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 6812 "Parser/parser.cc"
    break;

  case 6:
#line 542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 6818 "Parser/parser.cc"
    break;

  case 7:
#line 543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 6824 "Parser/parser.cc"
    break;

  case 8:
#line 544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantChar( *(yyvsp[0].tok) ) ); }
#line 6830 "Parser/parser.cc"
    break;

  case 19:
#line 565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 6836 "Parser/parser.cc"
    break;

  case 20:
#line 569 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.constant) = build_constantStr( *(yyvsp[0].str) ); }
#line 6842 "Parser/parser.cc"
    break;

  case 21:
#line 573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 6848 "Parser/parser.cc"
    break;

  case 22:
#line 575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 6858 "Parser/parser.cc"
    break;

  case 23:
#line 586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 6864 "Parser/parser.cc"
    break;

  case 24:
#line 588 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 6870 "Parser/parser.cc"
    break;

  case 25:
#line 592 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_dimensionref( (yyvsp[0].tok) ) ); }
#line 6876 "Parser/parser.cc"
    break;

  case 27:
#line 595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 6882 "Parser/parser.cc"
    break;

  case 28:
#line 597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new StmtExpr( dynamic_cast<CompoundStmt *>(maybeMoveBuild<Statement>((yyvsp[-1].sn)) ) ) ); }
#line 6888 "Parser/parser.cc"
    break;

  case 29:
#line 599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.en) = nullptr; }
#line 6894 "Parser/parser.cc"
    break;

  case 30:
#line 601 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.en) = nullptr; }
#line 6900 "Parser/parser.cc"
    break;

  case 31:
#line 603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild<Expression>( (yyvsp[-3].en) );
			(yyval.en) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 6910 "Parser/parser.cc"
    break;

  case 33:
#line 617 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// steal the association node from the singleton and delete the wrapper
			(yyvsp[-2].genexpr)->associations.splice((yyvsp[-2].genexpr)->associations.end(), (yyvsp[0].genexpr)->associations);
			delete (yyvsp[0].genexpr);
			(yyval.genexpr) = (yyvsp[-2].genexpr);
		}
#line 6921 "Parser/parser.cc"
    break;

  case 34:
#line 627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuildType((yyvsp[-2].decl)), maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } );
		}
#line 6930 "Parser/parser.cc"
    break;

  case 35:
#line 632 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } ); }
#line 6936 "Parser/parser.cc"
    break;

  case 37:
#line 642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-5].en), new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) ) ) )) ) ); }
#line 6942 "Parser/parser.cc"
    break;

  case 38:
#line 648 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 6948 "Parser/parser.cc"
    break;

  case 39:
#line 650 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.en) = new ExpressionNode( new ConstructorExpr( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) ) );
		}
#line 6958 "Parser/parser.cc"
    break;

  case 40:
#line 656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 6964 "Parser/parser.cc"
    break;

  case 41:
#line 658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 6970 "Parser/parser.cc"
    break;

  case 42:
#line 660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 6976 "Parser/parser.cc"
    break;

  case 43:
#line 662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), new ExpressionNode( (yyvsp[-2].constant) ) ) ); }
#line 6982 "Parser/parser.cc"
    break;

  case 44:
#line 664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 6988 "Parser/parser.cc"
    break;

  case 45:
#line 666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 6994 "Parser/parser.cc"
    break;

  case 46:
#line 668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-1].en), build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) ) ) ); }
#line 7000 "Parser/parser.cc"
    break;

  case 47:
#line 670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7006 "Parser/parser.cc"
    break;

  case 48:
#line 672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[0].aggKey), (yyvsp[-2].en) ) ); }
#line 7012 "Parser/parser.cc"
    break;

  case 49:
#line 674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7018 "Parser/parser.cc"
    break;

  case 50:
#line 676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 7024 "Parser/parser.cc"
    break;

  case 51:
#line 678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7030 "Parser/parser.cc"
    break;

  case 52:
#line 680 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::IncrPost, (yyvsp[-1].en) ) ); }
#line 7036 "Parser/parser.cc"
    break;

  case 53:
#line 682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::DecrPost, (yyvsp[-1].en) ) ); }
#line 7042 "Parser/parser.cc"
    break;

  case 54:
#line 684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].in), true ) ) ); }
#line 7048 "Parser/parser.cc"
    break;

  case 55:
#line 686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].in), true ))->set_maybeConstructed( false ) ) ); }
#line 7054 "Parser/parser.cc"
    break;

  case 56:
#line 688 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) );
		}
#line 7064 "Parser/parser.cc"
    break;

  case 57:
#line 697 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7070 "Parser/parser.cc"
    break;

  case 60:
#line 704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7076 "Parser/parser.cc"
    break;

  case 61:
#line 709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Default parameter for argument is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7082 "Parser/parser.cc"
    break;

  case 64:
#line 716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7088 "Parser/parser.cc"
    break;

  case 66:
#line 722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-1].tok) ) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7094 "Parser/parser.cc"
    break;

  case 67:
#line 724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-3].tok) ) ), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7100 "Parser/parser.cc"
    break;

  case 68:
#line 726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7106 "Parser/parser.cc"
    break;

  case 69:
#line 728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7112 "Parser/parser.cc"
    break;

  case 70:
#line 730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7118 "Parser/parser.cc"
    break;

  case 71:
#line 732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7124 "Parser/parser.cc"
    break;

  case 72:
#line 737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_constantInteger( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 7130 "Parser/parser.cc"
    break;

  case 73:
#line 739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_field_name_FLOATINGconstant( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 7136 "Parser/parser.cc"
    break;

  case 74:
#line 741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_varref( (yyvsp[-1].tok) ), (yyvsp[0].en) ) );
		}
#line 7144 "Parser/parser.cc"
    break;

  case 75:
#line 748 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7150 "Parser/parser.cc"
    break;

  case 76:
#line 750 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Expression * constant = build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) );
			(yyval.en) = (yyvsp[-1].en) != nullptr ? new ExpressionNode( build_fieldSel( (yyvsp[-1].en),  constant ) ) : new ExpressionNode( constant );
		}
#line 7159 "Parser/parser.cc"
    break;

  case 79:
#line 762 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 7165 "Parser/parser.cc"
    break;

  case 80:
#line 764 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en)->set_extension( true ); }
#line 7171 "Parser/parser.cc"
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
#line 7191 "Parser/parser.cc"
    break;

  case 82:
#line 785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_val( (yyvsp[-1].op), (yyvsp[0].en) ) ); }
#line 7197 "Parser/parser.cc"
    break;

  case 83:
#line 787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Incr, (yyvsp[0].en) ) ); }
#line 7203 "Parser/parser.cc"
    break;

  case 84:
#line 789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Decr, (yyvsp[0].en) ) ); }
#line 7209 "Parser/parser.cc"
    break;

  case 85:
#line 791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7215 "Parser/parser.cc"
    break;

  case 86:
#line 793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 7221 "Parser/parser.cc"
    break;

  case 87:
#line 795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7227 "Parser/parser.cc"
    break;

  case 88:
#line 797 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 7233 "Parser/parser.cc"
    break;

  case 89:
#line 799 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_offsetOf( (yyvsp[-3].decl), build_varref( (yyvsp[-1].tok) ) ) ); }
#line 7239 "Parser/parser.cc"
    break;

  case 90:
#line 801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.en) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 7248 "Parser/parser.cc"
    break;

  case 91:
#line 808 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::PointTo; }
#line 7254 "Parser/parser.cc"
    break;

  case 92:
#line 809 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AddressOf; }
#line 7260 "Parser/parser.cc"
    break;

  case 93:
#line 811 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::And; }
#line 7266 "Parser/parser.cc"
    break;

  case 94:
#line 815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::UnPlus; }
#line 7272 "Parser/parser.cc"
    break;

  case 95:
#line 816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::UnMinus; }
#line 7278 "Parser/parser.cc"
    break;

  case 96:
#line 817 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::Neg; }
#line 7284 "Parser/parser.cc"
    break;

  case 97:
#line 818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::BitNeg; }
#line 7290 "Parser/parser.cc"
    break;

  case 99:
#line 824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cast( (yyvsp[-2].decl), (yyvsp[0].en) ) ); }
#line 7296 "Parser/parser.cc"
    break;

  case 100:
#line 826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 7302 "Parser/parser.cc"
    break;

  case 101:
#line 828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 7308 "Parser/parser.cc"
    break;

  case 102:
#line 830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( nullptr ) ) ); }
#line 7314 "Parser/parser.cc"
    break;

  case 103:
#line 832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 7320 "Parser/parser.cc"
    break;

  case 104:
#line 834 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Return cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7326 "Parser/parser.cc"
    break;

  case 105:
#line 836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7332 "Parser/parser.cc"
    break;

  case 106:
#line 838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7338 "Parser/parser.cc"
    break;

  case 114:
#line 858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Exp, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7344 "Parser/parser.cc"
    break;

  case 116:
#line 864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mul, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7350 "Parser/parser.cc"
    break;

  case 117:
#line 866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Div, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7356 "Parser/parser.cc"
    break;

  case 118:
#line 868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mod, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7362 "Parser/parser.cc"
    break;

  case 120:
#line 874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Plus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7368 "Parser/parser.cc"
    break;

  case 121:
#line 876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Minus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7374 "Parser/parser.cc"
    break;

  case 123:
#line 882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7380 "Parser/parser.cc"
    break;

  case 124:
#line 884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::RShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7386 "Parser/parser.cc"
    break;

  case 126:
#line 890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7392 "Parser/parser.cc"
    break;

  case 127:
#line 892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7398 "Parser/parser.cc"
    break;

  case 128:
#line 894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7404 "Parser/parser.cc"
    break;

  case 129:
#line 896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7410 "Parser/parser.cc"
    break;

  case 131:
#line 902 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Eq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7416 "Parser/parser.cc"
    break;

  case 132:
#line 904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Neq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7422 "Parser/parser.cc"
    break;

  case 134:
#line 910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitAnd, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7428 "Parser/parser.cc"
    break;

  case 136:
#line 916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Xor, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7434 "Parser/parser.cc"
    break;

  case 138:
#line 922 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitOr, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7440 "Parser/parser.cc"
    break;

  case 140:
#line 928 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), true ) ); }
#line 7446 "Parser/parser.cc"
    break;

  case 142:
#line 934 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), false ) ); }
#line 7452 "Parser/parser.cc"
    break;

  case 144:
#line 940 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7458 "Parser/parser.cc"
    break;

  case 145:
#line 943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-3].en), (yyvsp[-3].en), (yyvsp[0].en) ) ); }
#line 7464 "Parser/parser.cc"
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
#line 7476 "Parser/parser.cc"
    break;

  case 149:
#line 962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7482 "Parser/parser.cc"
    break;

  case 150:
#line 967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7488 "Parser/parser.cc"
    break;

  case 154:
#line 977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::Assign; }
#line 7494 "Parser/parser.cc"
    break;

  case 155:
#line 978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AtAssn; }
#line 7500 "Parser/parser.cc"
    break;

  case 156:
#line 982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ExpAssn; }
#line 7506 "Parser/parser.cc"
    break;

  case 157:
#line 983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MulAssn; }
#line 7512 "Parser/parser.cc"
    break;

  case 158:
#line 984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::DivAssn; }
#line 7518 "Parser/parser.cc"
    break;

  case 159:
#line 985 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ModAssn; }
#line 7524 "Parser/parser.cc"
    break;

  case 160:
#line 986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::PlusAssn; }
#line 7530 "Parser/parser.cc"
    break;

  case 161:
#line 987 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MinusAssn; }
#line 7536 "Parser/parser.cc"
    break;

  case 162:
#line 988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::LSAssn; }
#line 7542 "Parser/parser.cc"
    break;

  case 163:
#line 989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::RSAssn; }
#line 7548 "Parser/parser.cc"
    break;

  case 164:
#line 990 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AndAssn; }
#line 7554 "Parser/parser.cc"
    break;

  case 165:
#line 991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ERAssn; }
#line 7560 "Parser/parser.cc"
    break;

  case 166:
#line 992 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::OrAssn; }
#line 7566 "Parser/parser.cc"
    break;

  case 167:
#line 1003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)(new ExpressionNode( nullptr ) )->set_last( (yyvsp[-1].en) ) ) ); }
#line 7572 "Parser/parser.cc"
    break;

  case 168:
#line 1005 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-4].en)->set_last( (yyvsp[-1].en) ) ) )); }
#line 7578 "Parser/parser.cc"
    break;

  case 170:
#line 1011 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7584 "Parser/parser.cc"
    break;

  case 171:
#line 1013 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7590 "Parser/parser.cc"
    break;

  case 172:
#line 1015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7596 "Parser/parser.cc"
    break;

  case 174:
#line 1021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new CommaExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7602 "Parser/parser.cc"
    break;

  case 175:
#line 1026 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7608 "Parser/parser.cc"
    break;

  case 187:
#line 1044 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 7614 "Parser/parser.cc"
    break;

  case 189:
#line 1047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_directive( (yyvsp[0].tok) ) ); }
#line 7620 "Parser/parser.cc"
    break;

  case 190:
#line 1053 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[0].sn)->add_label( (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 7626 "Parser/parser.cc"
    break;

  case 191:
#line 1058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (StatementNode *)0 ) ); }
#line 7632 "Parser/parser.cc"
    break;

  case 192:
#line 1063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (yyvsp[-2].sn) ) ); }
#line 7638 "Parser/parser.cc"
    break;

  case 194:
#line 1069 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 7644 "Parser/parser.cc"
    break;

  case 195:
#line 1074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7650 "Parser/parser.cc"
    break;

  case 196:
#line 1076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7656 "Parser/parser.cc"
    break;

  case 197:
#line 1078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7662 "Parser/parser.cc"
    break;

  case 198:
#line 1080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7668 "Parser/parser.cc"
    break;

  case 201:
#line 1087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 7674 "Parser/parser.cc"
    break;

  case 202:
#line 1092 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_expr( (yyvsp[-1].en) ) ); }
#line 7680 "Parser/parser.cc"
    break;

  case 203:
#line 1094 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_mutex( nullptr, new StatementNode( build_expr( (yyvsp[-1].en) ) ) ) ); }
#line 7686 "Parser/parser.cc"
    break;

  case 204:
#line 1102 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn); }
#line 7692 "Parser/parser.cc"
    break;

  case 205:
#line 1104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( true, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 7698 "Parser/parser.cc"
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
#line 7712 "Parser/parser.cc"
    break;

  case 207:
#line 1116 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( false, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 7718 "Parser/parser.cc"
    break;

  case 208:
#line 1118 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( false, (yyvsp[-7].en), (yyvsp[-2].sn) ) );
			(yyval.sn) = (yyvsp[-3].decl) ? new StatementNode( build_compound( (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 7727 "Parser/parser.cc"
    break;

  case 209:
#line 1127 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-2].ifctl), maybe_build_compound( (yyvsp[0].sn) ), nullptr ) ); }
#line 7733 "Parser/parser.cc"
    break;

  case 210:
#line 1129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-4].ifctl), maybe_build_compound( (yyvsp[-2].sn) ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 7739 "Parser/parser.cc"
    break;

  case 211:
#line 1134 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new IfCtrl( nullptr, (yyvsp[0].en) ); }
#line 7745 "Parser/parser.cc"
    break;

  case 212:
#line 1136 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new IfCtrl( (yyvsp[0].decl), nullptr ); }
#line 7751 "Parser/parser.cc"
    break;

  case 213:
#line 1138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new IfCtrl( (yyvsp[0].decl), nullptr ); }
#line 7757 "Parser/parser.cc"
    break;

  case 214:
#line 1140 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new IfCtrl( (yyvsp[-1].decl), (yyvsp[0].en) ); }
#line 7763 "Parser/parser.cc"
    break;

  case 215:
#line 1147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = (yyvsp[0].en); }
#line 7769 "Parser/parser.cc"
    break;

  case 216:
#line 1149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7775 "Parser/parser.cc"
    break;

  case 218:
#line 1154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.sn) = new StatementNode( build_case( (yyvsp[0].en) ) ); }
#line 7781 "Parser/parser.cc"
    break;

  case 219:
#line 1156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)((yyvsp[-2].sn)->set_last( new StatementNode( build_case( (yyvsp[0].en) ) ) ) ); }
#line 7787 "Parser/parser.cc"
    break;

  case 220:
#line 1160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn); }
#line 7793 "Parser/parser.cc"
    break;

  case 221:
#line 1161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.sn) = new StatementNode( build_default() ); }
#line 7799 "Parser/parser.cc"
    break;

  case 223:
#line 1173 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)( (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) )); }
#line 7805 "Parser/parser.cc"
    break;

  case 224:
#line 1177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( maybe_build_compound( (yyvsp[0].sn) ) ); }
#line 7811 "Parser/parser.cc"
    break;

  case 225:
#line 1182 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = nullptr; }
#line 7817 "Parser/parser.cc"
    break;

  case 227:
#line 1188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ); }
#line 7823 "Parser/parser.cc"
    break;

  case 228:
#line 1190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)( (yyvsp[-2].sn)->set_last( (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ) ) ); }
#line 7829 "Parser/parser.cc"
    break;

  case 229:
#line 1195 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( (yyvsp[-4].ifctl), maybe_build_compound( (yyvsp[-1].sn) ) ) ); }
#line 7835 "Parser/parser.cc"
    break;

  case 230:
#line 1197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( new IfCtrl( nullptr, new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 7841 "Parser/parser.cc"
    break;

  case 231:
#line 1199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( (yyvsp[-3].en), maybe_build_compound( (yyvsp[-6].sn) ) ) ); }
#line 7847 "Parser/parser.cc"
    break;

  case 232:
#line 1201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( new ExpressionNode( build_constantInteger( *new string( "1" ) ) ), maybe_build_compound( (yyvsp[-4].sn) ) ) ); }
#line 7853 "Parser/parser.cc"
    break;

  case 233:
#line 1203 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( (yyvsp[-4].fctl), maybe_build_compound( (yyvsp[-1].sn) ) ) ); }
#line 7859 "Parser/parser.cc"
    break;

  case 234:
#line 1205 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( new ForCtrl( (ExpressionNode * )nullptr, (ExpressionNode * )nullptr, (ExpressionNode * )nullptr ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 7865 "Parser/parser.cc"
    break;

  case 235:
#line 1210 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = nullptr; }
#line 7871 "Parser/parser.cc"
    break;

  case 236:
#line 1212 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Loop default block is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 7877 "Parser/parser.cc"
    break;

  case 238:
#line 1223 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 7896 "Parser/parser.cc"
    break;

  case 239:
#line 1241 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (ExpressionNode * )nullptr, (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 7902 "Parser/parser.cc"
    break;

  case 240:
#line 1243 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 7908 "Parser/parser.cc"
    break;

  case 241:
#line 1245 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-3].decl), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 7914 "Parser/parser.cc"
    break;

  case 242:
#line 1248 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7921 "Parser/parser.cc"
    break;

  case 243:
#line 1251 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LEThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7928 "Parser/parser.cc"
    break;

  case 244:
#line 1254 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].en)->clone(), (yyvsp[-1].compop), (yyvsp[0].en), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7934 "Parser/parser.cc"
    break;

  case 245:
#line 1256 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 7940 "Parser/parser.cc"
    break;

  case 246:
#line 1258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( new ExpressionNode( build_constantInteger( *new string( "0u" ) ) ), (yyvsp[-1].en), nullptr, OperKinds::LThan, nullptr, nullptr ); }
#line 7946 "Parser/parser.cc"
    break;

  case 247:
#line 1260 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-2].en), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7953 "Parser/parser.cc"
    break;

  case 248:
#line 1263 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-3].en), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LEThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7960 "Parser/parser.cc"
    break;

  case 249:
#line 1266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), (yyvsp[-1].compop), (yyvsp[0].en), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7966 "Parser/parser.cc"
    break;

  case 250:
#line 1268 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 7972 "Parser/parser.cc"
    break;

  case 251:
#line 1271 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Array interator is currently unimplemented." ); (yyval.fctl) = nullptr;
			(yyval.fctl) = forCtrl( new ExpressionNode( build_varref( (yyvsp[0].tok) ) ), (yyvsp[-2].en), nullptr, OperKinds::Range, nullptr, nullptr );
		}
#line 7981 "Parser/parser.cc"
    break;

  case 252:
#line 1278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), OperKinds::LThan, nullptr, new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7987 "Parser/parser.cc"
    break;

  case 253:
#line 1280 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), OperKinds::GThan, nullptr, new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7993 "Parser/parser.cc"
    break;

  case 254:
#line 1282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), OperKinds::LThan, nullptr, (yyvsp[0].en) ); }
#line 7999 "Parser/parser.cc"
    break;

  case 255:
#line 1284 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), OperKinds::GThan, nullptr, (yyvsp[0].en) ); }
#line 8005 "Parser/parser.cc"
    break;

  case 256:
#line 1286 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), OperKinds::LThan, nullptr, nullptr ); }
#line 8011 "Parser/parser.cc"
    break;

  case 257:
#line 1291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LThan; }
#line 8017 "Parser/parser.cc"
    break;

  case 258:
#line 1293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 8023 "Parser/parser.cc"
    break;

  case 259:
#line 1295 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GThan; }
#line 8029 "Parser/parser.cc"
    break;

  case 260:
#line 1297 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GEThan; }
#line 8035 "Parser/parser.cc"
    break;

  case 261:
#line 1302 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Goto ) ); }
#line 8041 "Parser/parser.cc"
    break;

  case 262:
#line 1306 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_computedgoto( (yyvsp[-1].en) ) ); }
#line 8047 "Parser/parser.cc"
    break;

  case 263:
#line 1309 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThrough ) ); }
#line 8053 "Parser/parser.cc"
    break;

  case 264:
#line 1311 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::FallThrough ) ); }
#line 8059 "Parser/parser.cc"
    break;

  case 265:
#line 1313 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThroughDefault ) ); }
#line 8065 "Parser/parser.cc"
    break;

  case 266:
#line 1316 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Continue ) ); }
#line 8071 "Parser/parser.cc"
    break;

  case 267:
#line 1320 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Continue ) ); }
#line 8077 "Parser/parser.cc"
    break;

  case 268:
#line 1323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Break ) ); }
#line 8083 "Parser/parser.cc"
    break;

  case 269:
#line 1327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Break ) ); }
#line 8089 "Parser/parser.cc"
    break;

  case 270:
#line 1329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_return( (yyvsp[-1].en) ) ); }
#line 8095 "Parser/parser.cc"
    break;

  case 271:
#line 1331 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 8101 "Parser/parser.cc"
    break;

  case 272:
#line 1333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr ) ); }
#line 8107 "Parser/parser.cc"
    break;

  case 273:
#line 1335 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn) ) ); }
#line 8113 "Parser/parser.cc"
    break;

  case 274:
#line 1337 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Coroutine ) ); }
#line 8119 "Parser/parser.cc"
    break;

  case 275:
#line 1339 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Coroutine ) ); }
#line 8125 "Parser/parser.cc"
    break;

  case 276:
#line 1341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Generator ) ); }
#line 8131 "Parser/parser.cc"
    break;

  case 277:
#line 1343 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Generator ) ); }
#line 8137 "Parser/parser.cc"
    break;

  case 278:
#line 1345 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_throw( (yyvsp[-1].en) ) ); }
#line 8143 "Parser/parser.cc"
    break;

  case 279:
#line 1347 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume( (yyvsp[-1].en) ) ); }
#line 8149 "Parser/parser.cc"
    break;

  case 280:
#line 1349 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume_at( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 8155 "Parser/parser.cc"
    break;

  case 283:
#line 1359 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_with( (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8161 "Parser/parser.cc"
    break;

  case 284:
#line 1365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_mutex( (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8167 "Parser/parser.cc"
    break;

  case 285:
#line 1369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 8173 "Parser/parser.cc"
    break;

  case 286:
#line 1374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8179 "Parser/parser.cc"
    break;

  case 288:
#line 1380 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 8185 "Parser/parser.cc"
    break;

  case 289:
#line 1384 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) )); }
#line 8191 "Parser/parser.cc"
    break;

  case 291:
#line 1391 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8197 "Parser/parser.cc"
    break;

  case 292:
#line 1395 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 8203 "Parser/parser.cc"
    break;

  case 293:
#line 1400 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8209 "Parser/parser.cc"
    break;

  case 294:
#line 1402 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-3].en), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[-4].en), (yyvsp[0].wfs) ); }
#line 8215 "Parser/parser.cc"
    break;

  case 295:
#line 1404 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8221 "Parser/parser.cc"
    break;

  case 296:
#line 1406 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( nullptr, maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8227 "Parser/parser.cc"
    break;

  case 297:
#line 1409 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 8233 "Parser/parser.cc"
    break;

  case 298:
#line 1411 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-5].en), maybe_build_compound( (yyvsp[-4].sn) ), (yyvsp[-6].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8239 "Parser/parser.cc"
    break;

  case 299:
#line 1416 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-1].en), (yyvsp[0].sn), (yyvsp[-2].en) ) ); }
#line 8245 "Parser/parser.cc"
    break;

  case 300:
#line 1418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-3].en), (yyvsp[-2].sn), (yyvsp[-4].en), (yyvsp[0].wfs) ) ); }
#line 8251 "Parser/parser.cc"
    break;

  case 301:
#line 1423 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), (yyvsp[0].sn), 0 ) ); }
#line 8257 "Parser/parser.cc"
    break;

  case 302:
#line 1425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), 0, (yyvsp[0].sn) ) ); }
#line 8263 "Parser/parser.cc"
    break;

  case 303:
#line 1427 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-2].sn), (yyvsp[-1].sn), (yyvsp[0].sn) ) ); }
#line 8269 "Parser/parser.cc"
    break;

  case 304:
#line 1432 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8275 "Parser/parser.cc"
    break;

  case 305:
#line 1434 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)(yyvsp[-8].sn)->set_last( new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ) ); }
#line 8281 "Parser/parser.cc"
    break;

  case 306:
#line 1439 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8287 "Parser/parser.cc"
    break;

  case 307:
#line 1440 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[0].en); }
#line 8293 "Parser/parser.cc"
    break;

  case 308:
#line 1444 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 8299 "Parser/parser.cc"
    break;

  case 309:
#line 1445 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 8305 "Parser/parser.cc"
    break;

  case 310:
#line 1446 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.catch_kind) = CatchStmt::Resume; }
#line 8311 "Parser/parser.cc"
    break;

  case 311:
#line 1447 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Resume; }
#line 8317 "Parser/parser.cc"
    break;

  case 312:
#line 1451 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = new StatementNode( build_finally( (yyvsp[0].sn) ) ); }
#line 8323 "Parser/parser.cc"
    break;

  case 314:
#line 1458 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 8329 "Parser/parser.cc"
    break;

  case 315:
#line 1460 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 8335 "Parser/parser.cc"
    break;

  case 316:
#line 1462 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 8341 "Parser/parser.cc"
    break;

  case 321:
#line 1477 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-4].flag), (yyvsp[-2].constant), 0 ) ); }
#line 8347 "Parser/parser.cc"
    break;

  case 322:
#line 1479 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-6].flag), (yyvsp[-4].constant), (yyvsp[-2].en) ) ); }
#line 8353 "Parser/parser.cc"
    break;

  case 323:
#line 1481 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-8].flag), (yyvsp[-6].constant), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 8359 "Parser/parser.cc"
    break;

  case 324:
#line 1483 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-10].flag), (yyvsp[-8].constant), (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 8365 "Parser/parser.cc"
    break;

  case 325:
#line 1485 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-12].flag), (yyvsp[-9].constant), 0, (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].label) ) ); }
#line 8371 "Parser/parser.cc"
    break;

  case 326:
#line 1490 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = false; }
#line 8377 "Parser/parser.cc"
    break;

  case 327:
#line 1492 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = true; }
#line 8383 "Parser/parser.cc"
    break;

  case 328:
#line 1497 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8389 "Parser/parser.cc"
    break;

  case 331:
#line 1504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 8395 "Parser/parser.cc"
    break;

  case 332:
#line 1509 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( nullptr, (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 8401 "Parser/parser.cc"
    break;

  case 333:
#line 1511 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( (yyvsp[-5].tok), (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 8407 "Parser/parser.cc"
    break;

  case 334:
#line 1516 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8413 "Parser/parser.cc"
    break;

  case 335:
#line 1518 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 8419 "Parser/parser.cc"
    break;

  case 336:
#line 1520 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( (yyvsp[0].constant) ) )); }
#line 8425 "Parser/parser.cc"
    break;

  case 337:
#line 1525 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = new LabelNode(); (yyval.label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 8434 "Parser/parser.cc"
    break;

  case 338:
#line 1530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = (yyvsp[-2].label); (yyvsp[-2].label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 8443 "Parser/parser.cc"
    break;

  case 339:
#line 1540 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8449 "Parser/parser.cc"
    break;

  case 342:
#line 1547 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 8455 "Parser/parser.cc"
    break;

  case 343:
#line 1552 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8461 "Parser/parser.cc"
    break;

  case 345:
#line 1558 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 8467 "Parser/parser.cc"
    break;

  case 346:
#line 1560 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-2].decl) ); }
#line 8473 "Parser/parser.cc"
    break;

  case 356:
#line 1586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-4].en), (yyvsp[-2].constant) ); }
#line 8479 "Parser/parser.cc"
    break;

  case 357:
#line 1588 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-2].en), build_constantStr( *new string( "\"\"" ) ) ); }
#line 8485 "Parser/parser.cc"
    break;

  case 361:
#line 1606 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 8491 "Parser/parser.cc"
    break;

  case 363:
#line 1612 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].in) ); }
#line 8497 "Parser/parser.cc"
    break;

  case 364:
#line 1616 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 8503 "Parser/parser.cc"
    break;

  case 365:
#line 1618 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].in) ) ); }
#line 8509 "Parser/parser.cc"
    break;

  case 366:
#line 1625 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 8515 "Parser/parser.cc"
    break;

  case 367:
#line 1627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 8521 "Parser/parser.cc"
    break;

  case 368:
#line 1629 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 8527 "Parser/parser.cc"
    break;

  case 370:
#line 1635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 8533 "Parser/parser.cc"
    break;

  case 371:
#line 1637 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 8539 "Parser/parser.cc"
    break;

  case 372:
#line 1639 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 8545 "Parser/parser.cc"
    break;

  case 373:
#line 1641 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeClone( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->appendList( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 8556 "Parser/parser.cc"
    break;

  case 374:
#line 1674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), 0 )->addQualifiers( (yyvsp[0].decl) ); }
#line 8562 "Parser/parser.cc"
    break;

  case 375:
#line 1676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), 0 )->addQualifiers( (yyvsp[0].decl) ); }
#line 8568 "Parser/parser.cc"
    break;

  case 376:
#line 1681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 8574 "Parser/parser.cc"
    break;

  case 377:
#line 1684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-6].decl)->appendList( (yyvsp[-2].decl) ) ); }
#line 8580 "Parser/parser.cc"
    break;

  case 378:
#line 1689 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 8589 "Parser/parser.cc"
    break;

  case 379:
#line 1694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 8598 "Parser/parser.cc"
    break;

  case 380:
#line 1699 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "3" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 8607 "Parser/parser.cc"
    break;

  case 381:
#line 1710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "4" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef();
		}
#line 8616 "Parser/parser.cc"
    break;

  case 382:
#line 1715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "5" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 8625 "Parser/parser.cc"
    break;

  case 383:
#line 1720 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "6" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[-3].decl) )->addTypedef();
		}
#line 8634 "Parser/parser.cc"
    break;

  case 384:
#line 1725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "7" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-2].decl) )->addTypedef();
		}
#line 8643 "Parser/parser.cc"
    break;

  case 385:
#line 1730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "8" );
			(yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-3].decl) )->addTypedef()->addType( (yyvsp[-3].decl) );
		}
#line 8652 "Parser/parser.cc"
    break;

  case 386:
#line 1739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 8660 "Parser/parser.cc"
    break;

  case 387:
#line 1743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 8668 "Parser/parser.cc"
    break;

  case 388:
#line 1750 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 8674 "Parser/parser.cc"
    break;

  case 392:
#line 1760 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 8680 "Parser/parser.cc"
    break;

  case 393:
#line 1762 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ) ); }
#line 8686 "Parser/parser.cc"
    break;

  case 406:
#line 1801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8692 "Parser/parser.cc"
    break;

  case 409:
#line 1813 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 8698 "Parser/parser.cc"
    break;

  case 412:
#line 1823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Const ); }
#line 8704 "Parser/parser.cc"
    break;

  case 413:
#line 1825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Restrict ); }
#line 8710 "Parser/parser.cc"
    break;

  case 414:
#line 1827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Volatile ); }
#line 8716 "Parser/parser.cc"
    break;

  case 415:
#line 1829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Atomic ); }
#line 8722 "Parser/parser.cc"
    break;

  case 417:
#line 1835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newForall( (yyvsp[-1].decl) ); }
#line 8728 "Parser/parser.cc"
    break;

  case 419:
#line 1841 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 8734 "Parser/parser.cc"
    break;

  case 420:
#line 1843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 8740 "Parser/parser.cc"
    break;

  case 422:
#line 1854 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 8746 "Parser/parser.cc"
    break;

  case 423:
#line 1859 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Extern ); }
#line 8752 "Parser/parser.cc"
    break;

  case 424:
#line 1861 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Static ); }
#line 8758 "Parser/parser.cc"
    break;

  case 425:
#line 1863 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Auto ); }
#line 8764 "Parser/parser.cc"
    break;

  case 426:
#line 1865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Register ); }
#line 8770 "Parser/parser.cc"
    break;

  case 427:
#line 1867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Threadlocal ); }
#line 8776 "Parser/parser.cc"
    break;

  case 428:
#line 1870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Inline ); }
#line 8782 "Parser/parser.cc"
    break;

  case 429:
#line 1872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Fortran ); }
#line 8788 "Parser/parser.cc"
    break;

  case 430:
#line 1874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Noreturn ); }
#line 8794 "Parser/parser.cc"
    break;

  case 431:
#line 1879 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 8800 "Parser/parser.cc"
    break;

  case 432:
#line 1881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Bool ); }
#line 8806 "Parser/parser.cc"
    break;

  case 433:
#line 1883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Char ); }
#line 8812 "Parser/parser.cc"
    break;

  case 434:
#line 1885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int ); }
#line 8818 "Parser/parser.cc"
    break;

  case 435:
#line 1887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 ); }
#line 8824 "Parser/parser.cc"
    break;

  case 436:
#line 1889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 )->addType( DeclarationNode::newSignedNess( DeclarationNode::Unsigned ) ); }
#line 8830 "Parser/parser.cc"
    break;

  case 437:
#line 1891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Float ); }
#line 8836 "Parser/parser.cc"
    break;

  case 438:
#line 1893 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Double ); }
#line 8842 "Parser/parser.cc"
    break;

  case 439:
#line 1895 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat80 ); }
#line 8848 "Parser/parser.cc"
    break;

  case 440:
#line 1897 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat128 ); }
#line 8854 "Parser/parser.cc"
    break;

  case 441:
#line 1899 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat16 ); }
#line 8860 "Parser/parser.cc"
    break;

  case 442:
#line 1901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32 ); }
#line 8866 "Parser/parser.cc"
    break;

  case 443:
#line 1903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32x ); }
#line 8872 "Parser/parser.cc"
    break;

  case 444:
#line 1905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64 ); }
#line 8878 "Parser/parser.cc"
    break;

  case 445:
#line 1907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64x ); }
#line 8884 "Parser/parser.cc"
    break;

  case 446:
#line 1909 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat128 ); }
#line 8890 "Parser/parser.cc"
    break;

  case 447:
#line 1911 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 8896 "Parser/parser.cc"
    break;

  case 448:
#line 1913 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 8902 "Parser/parser.cc"
    break;

  case 449:
#line 1915 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 8908 "Parser/parser.cc"
    break;

  case 450:
#line 1917 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Complex ); }
#line 8914 "Parser/parser.cc"
    break;

  case 451:
#line 1919 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Imaginary ); }
#line 8920 "Parser/parser.cc"
    break;

  case 452:
#line 1921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Signed ); }
#line 8926 "Parser/parser.cc"
    break;

  case 453:
#line 1923 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Unsigned ); }
#line 8932 "Parser/parser.cc"
    break;

  case 454:
#line 1925 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Short ); }
#line 8938 "Parser/parser.cc"
    break;

  case 455:
#line 1927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Long ); }
#line 8944 "Parser/parser.cc"
    break;

  case 456:
#line 1929 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Valist ); }
#line 8950 "Parser/parser.cc"
    break;

  case 457:
#line 1931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::AutoType ); }
#line 8956 "Parser/parser.cc"
    break;

  case 459:
#line 1937 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8962 "Parser/parser.cc"
    break;

  case 461:
#line 1943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVtableType( (yyvsp[-2].decl) ); }
#line 8968 "Parser/parser.cc"
    break;

  case 462:
#line 1949 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8974 "Parser/parser.cc"
    break;

  case 463:
#line 1951 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 8980 "Parser/parser.cc"
    break;

  case 465:
#line 1958 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 8986 "Parser/parser.cc"
    break;

  case 466:
#line 1960 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 8992 "Parser/parser.cc"
    break;

  case 467:
#line 1962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 8998 "Parser/parser.cc"
    break;

  case 468:
#line 1964 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 9004 "Parser/parser.cc"
    break;

  case 470:
#line 1971 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9010 "Parser/parser.cc"
    break;

  case 472:
#line 1977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9016 "Parser/parser.cc"
    break;

  case 473:
#line 1979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9022 "Parser/parser.cc"
    break;

  case 474:
#line 1981 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 9028 "Parser/parser.cc"
    break;

  case 475:
#line 1986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 9034 "Parser/parser.cc"
    break;

  case 476:
#line 1988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en) ); }
#line 9040 "Parser/parser.cc"
    break;

  case 477:
#line 1990 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 9046 "Parser/parser.cc"
    break;

  case 478:
#line 1992 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en), true ); }
#line 9052 "Parser/parser.cc"
    break;

  case 479:
#line 1994 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Zero ); }
#line 9058 "Parser/parser.cc"
    break;

  case 480:
#line 1996 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::One ); }
#line 9064 "Parser/parser.cc"
    break;

  case 482:
#line 2002 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9070 "Parser/parser.cc"
    break;

  case 483:
#line 2004 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9076 "Parser/parser.cc"
    break;

  case 484:
#line 2006 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9082 "Parser/parser.cc"
    break;

  case 486:
#line 2012 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 9088 "Parser/parser.cc"
    break;

  case 487:
#line 2014 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 9094 "Parser/parser.cc"
    break;

  case 488:
#line 2016 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 9103 "Parser/parser.cc"
    break;

  case 490:
#line 2025 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9109 "Parser/parser.cc"
    break;

  case 491:
#line 2027 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9115 "Parser/parser.cc"
    break;

  case 492:
#line 2029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9121 "Parser/parser.cc"
    break;

  case 494:
#line 2035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9127 "Parser/parser.cc"
    break;

  case 495:
#line 2037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9133 "Parser/parser.cc"
    break;

  case 497:
#line 2043 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9139 "Parser/parser.cc"
    break;

  case 498:
#line 2045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9145 "Parser/parser.cc"
    break;

  case 499:
#line 2047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9151 "Parser/parser.cc"
    break;

  case 501:
#line 2053 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9157 "Parser/parser.cc"
    break;

  case 502:
#line 2055 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9163 "Parser/parser.cc"
    break;

  case 503:
#line 2060 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypedef( (yyvsp[0].tok) ); }
#line 9169 "Parser/parser.cc"
    break;

  case 504:
#line 2062 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 9175 "Parser/parser.cc"
    break;

  case 505:
#line 2064 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 9181 "Parser/parser.cc"
    break;

  case 507:
#line 2067 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), (yyvsp[0].decl) ); }
#line 9187 "Parser/parser.cc"
    break;

  case 508:
#line 2069 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), (yyvsp[0].decl) ); }
#line 9193 "Parser/parser.cc"
    break;

  case 509:
#line 2074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[0].tok), nullptr ); }
#line 9199 "Parser/parser.cc"
    break;

  case 510:
#line 2076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-2].tok), nullptr ); }
#line 9205 "Parser/parser.cc"
    break;

  case 511:
#line 2078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 9211 "Parser/parser.cc"
    break;

  case 516:
#line 2093 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 9217 "Parser/parser.cc"
    break;

  case 517:
#line 2095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 9223 "Parser/parser.cc"
    break;

  case 518:
#line 2097 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 9232 "Parser/parser.cc"
    break;

  case 519:
#line 2102 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) ); }
#line 9238 "Parser/parser.cc"
    break;

  case 520:
#line 2104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 9247 "Parser/parser.cc"
    break;

  case 521:
#line 2109 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypedef( (yyvsp[-5].tok) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 9256 "Parser/parser.cc"
    break;

  case 522:
#line 2114 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 9265 "Parser/parser.cc"
    break;

  case 523:
#line 2119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeGen( (yyvsp[-5].tok), nullptr );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 9274 "Parser/parser.cc"
    break;

  case 525:
#line 2128 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9280 "Parser/parser.cc"
    break;

  case 526:
#line 2130 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 9286 "Parser/parser.cc"
    break;

  case 527:
#line 2135 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 9296 "Parser/parser.cc"
    break;

  case 528:
#line 2141 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 9311 "Parser/parser.cc"
    break;

  case 531:
#line 2160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Struct; }
#line 9317 "Parser/parser.cc"
    break;

  case 532:
#line 2162 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Union; }
#line 9323 "Parser/parser.cc"
    break;

  case 533:
#line 2164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Exception; }
#line 9329 "Parser/parser.cc"
    break;

  case 534:
#line 2170 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 9335 "Parser/parser.cc"
    break;

  case 535:
#line 2172 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 9341 "Parser/parser.cc"
    break;

  case 536:
#line 2174 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Generator; }
#line 9347 "Parser/parser.cc"
    break;

  case 537:
#line 2176 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor generator is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 9353 "Parser/parser.cc"
    break;

  case 538:
#line 2178 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Coroutine; }
#line 9359 "Parser/parser.cc"
    break;

  case 539:
#line 2180 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor coroutine is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 9365 "Parser/parser.cc"
    break;

  case 540:
#line 2182 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Thread; }
#line 9371 "Parser/parser.cc"
    break;

  case 541:
#line 2184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor thread is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 9377 "Parser/parser.cc"
    break;

  case 542:
#line 2189 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9383 "Parser/parser.cc"
    break;

  case 543:
#line 2191 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 9389 "Parser/parser.cc"
    break;

  case 544:
#line 2196 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); }
#line 9395 "Parser/parser.cc"
    break;

  case 545:
#line 2198 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 9401 "Parser/parser.cc"
    break;

  case 546:
#line 2200 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-1].decl) ) {								// field declarator ?
				(yyvsp[-1].decl) = DeclarationNode::newName( nullptr );
			} // if
			(yyvsp[-1].decl)->inLine = true;
			(yyval.decl) = distAttr( (yyvsp[-2].decl), (yyvsp[-1].decl) );					// mark all fields in list
			distInl( (yyvsp[-1].decl) );
		}
#line 9414 "Parser/parser.cc"
    break;

  case 547:
#line 2209 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9420 "Parser/parser.cc"
    break;

  case 550:
#line 2213 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 9426 "Parser/parser.cc"
    break;

  case 551:
#line 2215 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 9432 "Parser/parser.cc"
    break;

  case 554:
#line 2222 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9438 "Parser/parser.cc"
    break;

  case 556:
#line 2225 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 9444 "Parser/parser.cc"
    break;

  case 557:
#line 2230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].en) ); }
#line 9450 "Parser/parser.cc"
    break;

  case 558:
#line 2233 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 9456 "Parser/parser.cc"
    break;

  case 559:
#line 2236 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 9462 "Parser/parser.cc"
    break;

  case 560:
#line 2241 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9468 "Parser/parser.cc"
    break;

  case 562:
#line 2244 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 9474 "Parser/parser.cc"
    break;

  case 564:
#line 2255 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 9480 "Parser/parser.cc"
    break;

  case 565:
#line 2257 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 9486 "Parser/parser.cc"
    break;

  case 567:
#line 2264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 9492 "Parser/parser.cc"
    break;

  case 568:
#line 2269 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9498 "Parser/parser.cc"
    break;

  case 570:
#line 2275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 9504 "Parser/parser.cc"
    break;

  case 571:
#line 2280 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-4].decl) ); }
#line 9510 "Parser/parser.cc"
    break;

  case 572:
#line 2282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); }
#line 9516 "Parser/parser.cc"
    break;

  case 573:
#line 2284 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].tok), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) ); }
#line 9522 "Parser/parser.cc"
    break;

  case 574:
#line 2287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-4].decl)->name, (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 9528 "Parser/parser.cc"
    break;

  case 575:
#line 2289 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-6].decl)->storageClasses.val != 0 || (yyvsp[-6].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			SemanticError( yylloc, "Typed enumeration is currently unimplemented." ); (yyval.decl) = nullptr;
		}
#line 9537 "Parser/parser.cc"
    break;

  case 576:
#line 2294 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].decl)->storageClasses.val != 0 || (yyvsp[-4].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			typedefTable.makeTypedef( *(yyvsp[-1].tok) );
		}
#line 9546 "Parser/parser.cc"
    break;

  case 577:
#line 2299 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typed enumeration is currently unimplemented." ); (yyval.decl) = nullptr;
		}
#line 9554 "Parser/parser.cc"
    break;

  case 578:
#line 2303 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-8].decl)->storageClasses.val != 0 || (yyvsp[-8].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			typedefTable.makeTypedef( *(yyvsp[-5].decl)->name );
			SemanticError( yylloc, "Typed enumeration is currently unimplemented." ); (yyval.decl) = nullptr;
		}
#line 9564 "Parser/parser.cc"
    break;

  case 580:
#line 2313 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); (yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), 0, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 9570 "Parser/parser.cc"
    break;

  case 581:
#line 2315 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].decl)->type->symbolic.name );	(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].decl)->type->symbolic.name, 0, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 9576 "Parser/parser.cc"
    break;

  case 582:
#line 2320 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumConstant( (yyvsp[-1].tok), (yyvsp[0].en) ); }
#line 9582 "Parser/parser.cc"
    break;

  case 583:
#line 2322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumConstant( new string("inline"), nullptr ); }
#line 9588 "Parser/parser.cc"
    break;

  case 584:
#line 2324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( DeclarationNode::newEnumConstant( (yyvsp[-1].tok), (yyvsp[0].en) ) ); }
#line 9594 "Parser/parser.cc"
    break;

  case 585:
#line 2326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( DeclarationNode::newEnumConstant( new string("inline"), nullptr ) ); }
#line 9600 "Parser/parser.cc"
    break;

  case 586:
#line 2331 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9606 "Parser/parser.cc"
    break;

  case 587:
#line 2335 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].in)->get_expression(); }
#line 9612 "Parser/parser.cc"
    break;

  case 588:
#line 2340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 9618 "Parser/parser.cc"
    break;

  case 589:
#line 2342 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9624 "Parser/parser.cc"
    break;

  case 592:
#line 2346 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9630 "Parser/parser.cc"
    break;

  case 593:
#line 2348 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 9636 "Parser/parser.cc"
    break;

  case 594:
#line 2350 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 9642 "Parser/parser.cc"
    break;

  case 596:
#line 2358 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9648 "Parser/parser.cc"
    break;

  case 597:
#line 2360 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9654 "Parser/parser.cc"
    break;

  case 598:
#line 2362 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-8].decl)->appendList( (yyvsp[-4].decl) )->appendList( (yyvsp[0].decl) ); }
#line 9660 "Parser/parser.cc"
    break;

  case 600:
#line 2368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9666 "Parser/parser.cc"
    break;

  case 601:
#line 2373 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9672 "Parser/parser.cc"
    break;

  case 602:
#line 2375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9678 "Parser/parser.cc"
    break;

  case 604:
#line 2378 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 9684 "Parser/parser.cc"
    break;

  case 607:
#line 2385 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9690 "Parser/parser.cc"
    break;

  case 608:
#line 2387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9696 "Parser/parser.cc"
    break;

  case 610:
#line 2396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 9702 "Parser/parser.cc"
    break;

  case 611:
#line 2399 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 9708 "Parser/parser.cc"
    break;

  case 612:
#line 2401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 9714 "Parser/parser.cc"
    break;

  case 617:
#line 2411 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9720 "Parser/parser.cc"
    break;

  case 619:
#line 2418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 9726 "Parser/parser.cc"
    break;

  case 620:
#line 2420 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 9732 "Parser/parser.cc"
    break;

  case 621:
#line 2425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 9738 "Parser/parser.cc"
    break;

  case 622:
#line 2427 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 9744 "Parser/parser.cc"
    break;

  case 623:
#line 2436 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 9750 "Parser/parser.cc"
    break;

  case 624:
#line 2438 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 9756 "Parser/parser.cc"
    break;

  case 630:
#line 2451 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 9762 "Parser/parser.cc"
    break;

  case 633:
#line 2461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 9768 "Parser/parser.cc"
    break;

  case 634:
#line 2462 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (yyvsp[-1].op) == OperKinds::Assign ? (yyvsp[0].in) : (yyvsp[0].in)->set_maybeConstructed( false ); }
#line 9774 "Parser/parser.cc"
    break;

  case 635:
#line 2463 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.in) = new InitializerNode( true ); }
#line 9780 "Parser/parser.cc"
    break;

  case 636:
#line 2464 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 9786 "Parser/parser.cc"
    break;

  case 637:
#line 2468 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = new InitializerNode( (yyvsp[0].en) ); }
#line 9792 "Parser/parser.cc"
    break;

  case 638:
#line 2469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 9798 "Parser/parser.cc"
    break;

  case 639:
#line 2474 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 9804 "Parser/parser.cc"
    break;

  case 641:
#line 2476 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ); }
#line 9810 "Parser/parser.cc"
    break;

  case 642:
#line 2477 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (InitializerNode *)( (yyvsp[-2].in)->set_last( (yyvsp[0].in) ) ); }
#line 9816 "Parser/parser.cc"
    break;

  case 643:
#line 2478 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.in) = (InitializerNode *)((yyvsp[-3].in)->set_last( (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ) )); }
#line 9822 "Parser/parser.cc"
    break;

  case 645:
#line 2494 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[-1].tok) ) ); }
#line 9828 "Parser/parser.cc"
    break;

  case 647:
#line 2500 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-1].en)->set_last( (yyvsp[0].en) )); }
#line 9834 "Parser/parser.cc"
    break;

  case 648:
#line 2506 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 9840 "Parser/parser.cc"
    break;

  case 649:
#line 2509 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 9846 "Parser/parser.cc"
    break;

  case 650:
#line 2511 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 9852 "Parser/parser.cc"
    break;

  case 651:
#line 2513 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-4].en) ), maybeMoveBuild<Expression>( (yyvsp[-2].en) ) ) ); }
#line 9858 "Parser/parser.cc"
    break;

  case 652:
#line 2515 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 9864 "Parser/parser.cc"
    break;

  case 654:
#line 2539 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl) ); }
#line 9870 "Parser/parser.cc"
    break;

  case 655:
#line 2544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9876 "Parser/parser.cc"
    break;

  case 656:
#line 2546 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 9882 "Parser/parser.cc"
    break;

  case 657:
#line 2551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "9" );
			if ( (yyvsp[-1].tclass) == TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 9893 "Parser/parser.cc"
    break;

  case 658:
#line 2558 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 9899 "Parser/parser.cc"
    break;

  case 659:
#line 2560 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "9" ); }
#line 9905 "Parser/parser.cc"
    break;

  case 660:
#line 2562 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 9911 "Parser/parser.cc"
    break;

  case 661:
#line 2564 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "9" );
			(yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 9920 "Parser/parser.cc"
    break;

  case 662:
#line 2570 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 9926 "Parser/parser.cc"
    break;

  case 663:
#line 2575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 9932 "Parser/parser.cc"
    break;

  case 664:
#line 2577 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 9938 "Parser/parser.cc"
    break;

  case 665:
#line 2579 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::DStype; }
#line 9944 "Parser/parser.cc"
    break;

  case 666:
#line 2583 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 9950 "Parser/parser.cc"
    break;

  case 667:
#line 2588 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 9956 "Parser/parser.cc"
    break;

  case 668:
#line 2590 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 9962 "Parser/parser.cc"
    break;

  case 669:
#line 2592 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ftype; }
#line 9968 "Parser/parser.cc"
    break;

  case 670:
#line 2594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 9974 "Parser/parser.cc"
    break;

  case 671:
#line 2599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9980 "Parser/parser.cc"
    break;

  case 674:
#line 2606 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 9986 "Parser/parser.cc"
    break;

  case 675:
#line 2611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 9992 "Parser/parser.cc"
    break;

  case 676:
#line 2613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 9998 "Parser/parser.cc"
    break;

  case 677:
#line 2620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 10004 "Parser/parser.cc"
    break;

  case 679:
#line 2623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 10010 "Parser/parser.cc"
    break;

  case 680:
#line 2625 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)( (yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 10016 "Parser/parser.cc"
    break;

  case 681:
#line 2630 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 10022 "Parser/parser.cc"
    break;

  case 682:
#line 2632 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10028 "Parser/parser.cc"
    break;

  case 683:
#line 2634 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 10034 "Parser/parser.cc"
    break;

  case 684:
#line 2639 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 10040 "Parser/parser.cc"
    break;

  case 685:
#line 2641 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 10046 "Parser/parser.cc"
    break;

  case 686:
#line 2646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "10" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), 0 );
		}
#line 10055 "Parser/parser.cc"
    break;

  case 687:
#line 2651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "11" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 10064 "Parser/parser.cc"
    break;

  case 688:
#line 2659 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), 0 ); }
#line 10070 "Parser/parser.cc"
    break;

  case 689:
#line 2661 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-8].tok), (yyvsp[-6].decl), (yyvsp[-2].decl) ); }
#line 10076 "Parser/parser.cc"
    break;

  case 691:
#line 2667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl) ); }
#line 10082 "Parser/parser.cc"
    break;

  case 696:
#line 2679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 10088 "Parser/parser.cc"
    break;

  case 697:
#line 2684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10094 "Parser/parser.cc"
    break;

  case 698:
#line 2686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 10100 "Parser/parser.cc"
    break;

  case 700:
#line 2694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl);	}
#line 10106 "Parser/parser.cc"
    break;

  case 701:
#line 2699 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10112 "Parser/parser.cc"
    break;

  case 702:
#line 2701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->appendList( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 10118 "Parser/parser.cc"
    break;

  case 703:
#line 2706 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10124 "Parser/parser.cc"
    break;

  case 705:
#line 2711 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 10130 "Parser/parser.cc"
    break;

  case 706:
#line 2715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 10136 "Parser/parser.cc"
    break;

  case 707:
#line 2720 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( (yyvsp[0].tok) ) ) ); }
#line 10142 "Parser/parser.cc"
    break;

  case 710:
#line 2724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 10151 "Parser/parser.cc"
    break;

  case 711:
#line 2729 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( false, (yyvsp[-2].constant), 0 ) ) ); }
#line 10157 "Parser/parser.cc"
    break;

  case 712:
#line 2731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = LinkageSpec::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 10166 "Parser/parser.cc"
    break;

  case 713:
#line 2736 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10176 "Parser/parser.cc"
    break;

  case 714:
#line 2742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 10185 "Parser/parser.cc"
    break;

  case 715:
#line 2747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10195 "Parser/parser.cc"
    break;

  case 716:
#line 2753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 10204 "Parser/parser.cc"
    break;

  case 717:
#line 2758 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10214 "Parser/parser.cc"
    break;

  case 718:
#line 2764 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.val) || (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 10223 "Parser/parser.cc"
    break;

  case 719:
#line 2769 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10233 "Parser/parser.cc"
    break;

  case 721:
#line 2784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].sn) ); }
#line 10239 "Parser/parser.cc"
    break;

  case 722:
#line 2786 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].sn) ); }
#line 10245 "Parser/parser.cc"
    break;

  case 723:
#line 2791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; forall = false; }
#line 10251 "Parser/parser.cc"
    break;

  case 724:
#line 2793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); forall = false; }
#line 10257 "Parser/parser.cc"
    break;

  case 725:
#line 2798 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10268 "Parser/parser.cc"
    break;

  case 726:
#line 2805 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 10277 "Parser/parser.cc"
    break;

  case 727:
#line 2810 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 10286 "Parser/parser.cc"
    break;

  case 728:
#line 2816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 10292 "Parser/parser.cc"
    break;

  case 729:
#line 2819 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 10298 "Parser/parser.cc"
    break;

  case 730:
#line 2822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10304 "Parser/parser.cc"
    break;

  case 731:
#line 2826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-4].decl) );
		}
#line 10313 "Parser/parser.cc"
    break;

  case 732:
#line 2832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10319 "Parser/parser.cc"
    break;

  case 733:
#line 2835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10325 "Parser/parser.cc"
    break;

  case 734:
#line 2838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 10331 "Parser/parser.cc"
    break;

  case 738:
#line 2849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 10337 "Parser/parser.cc"
    break;

  case 739:
#line 2854 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10343 "Parser/parser.cc"
    break;

  case 740:
#line 2856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = (yyvsp[-2].constant);
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 10353 "Parser/parser.cc"
    break;

  case 741:
#line 2865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10359 "Parser/parser.cc"
    break;

  case 744:
#line 2872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10365 "Parser/parser.cc"
    break;

  case 745:
#line 2877 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 10371 "Parser/parser.cc"
    break;

  case 747:
#line 2883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10377 "Parser/parser.cc"
    break;

  case 748:
#line 2888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10383 "Parser/parser.cc"
    break;

  case 749:
#line 2890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 10389 "Parser/parser.cc"
    break;

  case 750:
#line 2892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 10395 "Parser/parser.cc"
    break;

  case 755:
#line 2901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 10401 "Parser/parser.cc"
    break;

  case 756:
#line 2903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 10407 "Parser/parser.cc"
    break;

  case 757:
#line 2938 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 10413 "Parser/parser.cc"
    break;

  case 758:
#line 2940 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10419 "Parser/parser.cc"
    break;

  case 759:
#line 2945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10425 "Parser/parser.cc"
    break;

  case 761:
#line 2948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10431 "Parser/parser.cc"
    break;

  case 762:
#line 2950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10437 "Parser/parser.cc"
    break;

  case 763:
#line 2955 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10443 "Parser/parser.cc"
    break;

  case 764:
#line 2957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10449 "Parser/parser.cc"
    break;

  case 765:
#line 2959 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10455 "Parser/parser.cc"
    break;

  case 766:
#line 2961 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10461 "Parser/parser.cc"
    break;

  case 767:
#line 2966 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 10467 "Parser/parser.cc"
    break;

  case 768:
#line 2968 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10473 "Parser/parser.cc"
    break;

  case 769:
#line 2970 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10479 "Parser/parser.cc"
    break;

  case 770:
#line 2972 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10485 "Parser/parser.cc"
    break;

  case 771:
#line 2974 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10491 "Parser/parser.cc"
    break;

  case 772:
#line 2976 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10497 "Parser/parser.cc"
    break;

  case 773:
#line 2978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10503 "Parser/parser.cc"
    break;

  case 774:
#line 2983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10509 "Parser/parser.cc"
    break;

  case 775:
#line 2985 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 10515 "Parser/parser.cc"
    break;

  case 776:
#line 2987 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10521 "Parser/parser.cc"
    break;

  case 777:
#line 2989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10527 "Parser/parser.cc"
    break;

  case 778:
#line 2998 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10533 "Parser/parser.cc"
    break;

  case 780:
#line 3001 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10539 "Parser/parser.cc"
    break;

  case 781:
#line 3006 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10545 "Parser/parser.cc"
    break;

  case 782:
#line 3008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10551 "Parser/parser.cc"
    break;

  case 783:
#line 3010 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 10557 "Parser/parser.cc"
    break;

  case 784:
#line 3012 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10563 "Parser/parser.cc"
    break;

  case 785:
#line 3014 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10569 "Parser/parser.cc"
    break;

  case 786:
#line 3019 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10575 "Parser/parser.cc"
    break;

  case 787:
#line 3021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10581 "Parser/parser.cc"
    break;

  case 788:
#line 3023 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10587 "Parser/parser.cc"
    break;

  case 789:
#line 3025 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10593 "Parser/parser.cc"
    break;

  case 790:
#line 3030 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10599 "Parser/parser.cc"
    break;

  case 791:
#line 3032 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10605 "Parser/parser.cc"
    break;

  case 792:
#line 3034 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10611 "Parser/parser.cc"
    break;

  case 793:
#line 3036 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10617 "Parser/parser.cc"
    break;

  case 794:
#line 3038 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10623 "Parser/parser.cc"
    break;

  case 795:
#line 3040 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10629 "Parser/parser.cc"
    break;

  case 799:
#line 3058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 10635 "Parser/parser.cc"
    break;

  case 800:
#line 3060 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10641 "Parser/parser.cc"
    break;

  case 801:
#line 3062 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 10647 "Parser/parser.cc"
    break;

  case 802:
#line 3064 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10653 "Parser/parser.cc"
    break;

  case 803:
#line 3066 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10659 "Parser/parser.cc"
    break;

  case 804:
#line 3071 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10665 "Parser/parser.cc"
    break;

  case 805:
#line 3073 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10671 "Parser/parser.cc"
    break;

  case 806:
#line 3075 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10677 "Parser/parser.cc"
    break;

  case 807:
#line 3077 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10683 "Parser/parser.cc"
    break;

  case 808:
#line 3082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10689 "Parser/parser.cc"
    break;

  case 809:
#line 3084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10695 "Parser/parser.cc"
    break;

  case 810:
#line 3086 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10701 "Parser/parser.cc"
    break;

  case 811:
#line 3088 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10707 "Parser/parser.cc"
    break;

  case 812:
#line 3090 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10713 "Parser/parser.cc"
    break;

  case 813:
#line 3092 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10719 "Parser/parser.cc"
    break;

  case 814:
#line 3107 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "ID" );
		}
#line 10728 "Parser/parser.cc"
    break;

  case 815:
#line 3112 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10734 "Parser/parser.cc"
    break;

  case 816:
#line 3117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10740 "Parser/parser.cc"
    break;

  case 818:
#line 3120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10746 "Parser/parser.cc"
    break;

  case 819:
#line 3122 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10752 "Parser/parser.cc"
    break;

  case 820:
#line 3127 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10758 "Parser/parser.cc"
    break;

  case 821:
#line 3129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10764 "Parser/parser.cc"
    break;

  case 822:
#line 3131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10770 "Parser/parser.cc"
    break;

  case 823:
#line 3133 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10776 "Parser/parser.cc"
    break;

  case 824:
#line 3138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 10782 "Parser/parser.cc"
    break;

  case 825:
#line 3140 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10788 "Parser/parser.cc"
    break;

  case 826:
#line 3142 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10794 "Parser/parser.cc"
    break;

  case 827:
#line 3144 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10800 "Parser/parser.cc"
    break;

  case 828:
#line 3146 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10806 "Parser/parser.cc"
    break;

  case 829:
#line 3148 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10812 "Parser/parser.cc"
    break;

  case 830:
#line 3150 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10818 "Parser/parser.cc"
    break;

  case 831:
#line 3155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10824 "Parser/parser.cc"
    break;

  case 832:
#line 3157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10830 "Parser/parser.cc"
    break;

  case 833:
#line 3159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 10836 "Parser/parser.cc"
    break;

  case 834:
#line 3161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10842 "Parser/parser.cc"
    break;

  case 835:
#line 3163 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10848 "Parser/parser.cc"
    break;

  case 836:
#line 3173 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10854 "Parser/parser.cc"
    break;

  case 837:
#line 3175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10860 "Parser/parser.cc"
    break;

  case 839:
#line 3178 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10866 "Parser/parser.cc"
    break;

  case 840:
#line 3180 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10872 "Parser/parser.cc"
    break;

  case 841:
#line 3185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10878 "Parser/parser.cc"
    break;

  case 842:
#line 3187 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10884 "Parser/parser.cc"
    break;

  case 843:
#line 3189 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10890 "Parser/parser.cc"
    break;

  case 844:
#line 3194 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 10896 "Parser/parser.cc"
    break;

  case 845:
#line 3196 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10902 "Parser/parser.cc"
    break;

  case 846:
#line 3198 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10908 "Parser/parser.cc"
    break;

  case 847:
#line 3200 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10914 "Parser/parser.cc"
    break;

  case 848:
#line 3205 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10920 "Parser/parser.cc"
    break;

  case 849:
#line 3207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10926 "Parser/parser.cc"
    break;

  case 850:
#line 3209 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10932 "Parser/parser.cc"
    break;

  case 851:
#line 3223 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10938 "Parser/parser.cc"
    break;

  case 852:
#line 3225 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10944 "Parser/parser.cc"
    break;

  case 854:
#line 3228 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10950 "Parser/parser.cc"
    break;

  case 855:
#line 3230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10956 "Parser/parser.cc"
    break;

  case 856:
#line 3235 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 10962 "Parser/parser.cc"
    break;

  case 857:
#line 3237 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 10968 "Parser/parser.cc"
    break;

  case 858:
#line 3242 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10974 "Parser/parser.cc"
    break;

  case 859:
#line 3244 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10980 "Parser/parser.cc"
    break;

  case 860:
#line 3246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10986 "Parser/parser.cc"
    break;

  case 861:
#line 3251 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 10992 "Parser/parser.cc"
    break;

  case 862:
#line 3253 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10998 "Parser/parser.cc"
    break;

  case 863:
#line 3258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11004 "Parser/parser.cc"
    break;

  case 864:
#line 3260 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11010 "Parser/parser.cc"
    break;

  case 866:
#line 3278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11016 "Parser/parser.cc"
    break;

  case 867:
#line 3280 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11022 "Parser/parser.cc"
    break;

  case 868:
#line 3285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( 0, (yyvsp[0].op) ); }
#line 11028 "Parser/parser.cc"
    break;

  case 869:
#line 3287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 11034 "Parser/parser.cc"
    break;

  case 870:
#line 3289 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11040 "Parser/parser.cc"
    break;

  case 871:
#line 3291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11046 "Parser/parser.cc"
    break;

  case 872:
#line 3293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11052 "Parser/parser.cc"
    break;

  case 874:
#line 3299 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11058 "Parser/parser.cc"
    break;

  case 875:
#line 3301 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11064 "Parser/parser.cc"
    break;

  case 876:
#line 3303 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11070 "Parser/parser.cc"
    break;

  case 877:
#line 3308 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 11076 "Parser/parser.cc"
    break;

  case 878:
#line 3310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11082 "Parser/parser.cc"
    break;

  case 879:
#line 3312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11088 "Parser/parser.cc"
    break;

  case 880:
#line 3318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false ); }
#line 11094 "Parser/parser.cc"
    break;

  case 881:
#line 3320 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false )->addArray( (yyvsp[0].decl) ); }
#line 11100 "Parser/parser.cc"
    break;

  case 882:
#line 3322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-4].en), 0, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].en), 0, false ) ); }
#line 11106 "Parser/parser.cc"
    break;

  case 884:
#line 3329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), 0, false ); }
#line 11112 "Parser/parser.cc"
    break;

  case 885:
#line 3331 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 11118 "Parser/parser.cc"
    break;

  case 886:
#line 3333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newArray( (yyvsp[-2].en), 0, false ) ); }
#line 11124 "Parser/parser.cc"
    break;

  case 887:
#line 3335 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 11130 "Parser/parser.cc"
    break;

  case 889:
#line 3370 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 11136 "Parser/parser.cc"
    break;

  case 890:
#line 3372 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11142 "Parser/parser.cc"
    break;

  case 891:
#line 3374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11148 "Parser/parser.cc"
    break;

  case 892:
#line 3379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].op) ); }
#line 11154 "Parser/parser.cc"
    break;

  case 893:
#line 3381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 11160 "Parser/parser.cc"
    break;

  case 894:
#line 3383 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 11166 "Parser/parser.cc"
    break;

  case 895:
#line 3385 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11172 "Parser/parser.cc"
    break;

  case 896:
#line 3387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11178 "Parser/parser.cc"
    break;

  case 898:
#line 3393 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11184 "Parser/parser.cc"
    break;

  case 899:
#line 3395 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11190 "Parser/parser.cc"
    break;

  case 900:
#line 3397 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11196 "Parser/parser.cc"
    break;

  case 901:
#line 3402 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 11202 "Parser/parser.cc"
    break;

  case 902:
#line 3404 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11208 "Parser/parser.cc"
    break;

  case 903:
#line 3406 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11214 "Parser/parser.cc"
    break;

  case 905:
#line 3413 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11220 "Parser/parser.cc"
    break;

  case 907:
#line 3424 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false ); }
#line 11226 "Parser/parser.cc"
    break;

  case 908:
#line 3427 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 11232 "Parser/parser.cc"
    break;

  case 909:
#line 3429 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, (yyvsp[-2].decl), false ); }
#line 11238 "Parser/parser.cc"
    break;

  case 910:
#line 3432 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 11244 "Parser/parser.cc"
    break;

  case 911:
#line 3434 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 11250 "Parser/parser.cc"
    break;

  case 912:
#line 3436 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-4].decl), true ); }
#line 11256 "Parser/parser.cc"
    break;

  case 914:
#line 3451 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11262 "Parser/parser.cc"
    break;

  case 915:
#line 3453 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11268 "Parser/parser.cc"
    break;

  case 916:
#line 3458 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( 0, (yyvsp[0].op) ); }
#line 11274 "Parser/parser.cc"
    break;

  case 917:
#line 3460 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 11280 "Parser/parser.cc"
    break;

  case 918:
#line 3462 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11286 "Parser/parser.cc"
    break;

  case 919:
#line 3464 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11292 "Parser/parser.cc"
    break;

  case 920:
#line 3466 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11298 "Parser/parser.cc"
    break;

  case 922:
#line 3472 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11304 "Parser/parser.cc"
    break;

  case 923:
#line 3474 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11310 "Parser/parser.cc"
    break;

  case 924:
#line 3476 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11316 "Parser/parser.cc"
    break;

  case 925:
#line 3481 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11322 "Parser/parser.cc"
    break;

  case 926:
#line 3483 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11328 "Parser/parser.cc"
    break;

  case 929:
#line 3493 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11334 "Parser/parser.cc"
    break;

  case 932:
#line 3504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11340 "Parser/parser.cc"
    break;

  case 933:
#line 3506 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11346 "Parser/parser.cc"
    break;

  case 934:
#line 3508 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11352 "Parser/parser.cc"
    break;

  case 935:
#line 3510 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11358 "Parser/parser.cc"
    break;

  case 936:
#line 3512 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11364 "Parser/parser.cc"
    break;

  case 937:
#line 3514 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11370 "Parser/parser.cc"
    break;

  case 938:
#line 3521 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11376 "Parser/parser.cc"
    break;

  case 939:
#line 3523 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11382 "Parser/parser.cc"
    break;

  case 940:
#line 3525 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11388 "Parser/parser.cc"
    break;

  case 941:
#line 3527 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 11394 "Parser/parser.cc"
    break;

  case 942:
#line 3529 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11400 "Parser/parser.cc"
    break;

  case 943:
#line 3532 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11406 "Parser/parser.cc"
    break;

  case 944:
#line 3534 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11412 "Parser/parser.cc"
    break;

  case 945:
#line 3536 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11418 "Parser/parser.cc"
    break;

  case 946:
#line 3538 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 11424 "Parser/parser.cc"
    break;

  case 947:
#line 3540 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11430 "Parser/parser.cc"
    break;

  case 948:
#line 3545 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 11436 "Parser/parser.cc"
    break;

  case 949:
#line 3547 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 11442 "Parser/parser.cc"
    break;

  case 950:
#line 3552 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 11448 "Parser/parser.cc"
    break;

  case 951:
#line 3554 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl)->addQualifiers( (yyvsp[-4].decl) ), true ); }
#line 11454 "Parser/parser.cc"
    break;

  case 953:
#line 3581 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11460 "Parser/parser.cc"
    break;

  case 957:
#line 3592 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11466 "Parser/parser.cc"
    break;

  case 958:
#line 3594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11472 "Parser/parser.cc"
    break;

  case 959:
#line 3596 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11478 "Parser/parser.cc"
    break;

  case 960:
#line 3598 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11484 "Parser/parser.cc"
    break;

  case 961:
#line 3600 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11490 "Parser/parser.cc"
    break;

  case 962:
#line 3602 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11496 "Parser/parser.cc"
    break;

  case 963:
#line 3609 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11502 "Parser/parser.cc"
    break;

  case 964:
#line 3611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11508 "Parser/parser.cc"
    break;

  case 965:
#line 3613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11514 "Parser/parser.cc"
    break;

  case 966:
#line 3615 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11520 "Parser/parser.cc"
    break;

  case 967:
#line 3617 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11526 "Parser/parser.cc"
    break;

  case 968:
#line 3619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11532 "Parser/parser.cc"
    break;

  case 969:
#line 3624 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 11538 "Parser/parser.cc"
    break;

  case 970:
#line 3626 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11544 "Parser/parser.cc"
    break;

  case 971:
#line 3628 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11550 "Parser/parser.cc"
    break;

  case 972:
#line 3635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 11556 "Parser/parser.cc"
    break;

  case 973:
#line 3637 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 11562 "Parser/parser.cc"
    break;

  case 976:
#line 3661 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 11568 "Parser/parser.cc"
    break;

  case 977:
#line 3663 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 11574 "Parser/parser.cc"
    break;


#line 11578 "Parser/parser.cc"

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
#line 3666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "make install" //
// End: //
