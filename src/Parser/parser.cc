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
#define YYLAST   19221

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

#define YYPACT_NINF (-1699)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-856)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
      90, 11267,   132,   190, 15654,   -37, -1699, -1699, -1699, -1699,
   -1699, -1699, -1699, -1699, -1699, -1699, -1699,    88,   803,   138,
   -1699, -1699, -1699, -1699, -1699, -1699, -1699, -1699, -1699, -1699,
   -1699, -1699, -1699, -1699, -1699, -1699, -1699, -1699, -1699, -1699,
   -1699, -1699, -1699, -1699, -1699, -1699, -1699,     7,   328, -1699,
   -1699, -1699, -1699, -1699, -1699,  3380,  3380,   232, 11267,   294,
     310, -1699, -1699,   330, -1699, -1699, -1699, -1699, -1699, -1699,
   -1699, -1699, -1699,  2269, -1699,   789,   119, -1699, -1699, -1699,
   -1699, -1699, 15504, -1699, -1699,   323,   360,   525,    95, -1699,
    3380,   360,   360,   360,   326,  4181,   515,   765, 11426, -1699,
   -1699, -1699, 15354,  1094, -1699, -1699, -1699,  1291,   541, 11478,
     815,   757,  1291,   855,   371, -1699, -1699, -1699, -1699,   482,
   -1699, -1699, -1699, -1699,   418, -1699, -1699, -1699, -1699, -1699,
     427,   434,   482, -1699,   482,   442, -1699, -1699, -1699, 16210,
    3380, -1699, -1699,  3380, -1699, 11267,   429, 16262, -1699, -1699,
    4247, 17274, -1699,   934,   934, -1699,  2853, -1699, -1699, -1699,
   -1699,   304, 13964,  3281,   482, -1699, -1699, -1699, -1699, -1699,
   -1699,   470, -1699,   529,   494,   504, -1699,   591, 18696, 14584,
    3855,  2269,   639,   512,   612,   626,   631,   683,   697, -1699,
   -1699, 16412, 10618,   593, -1699, 15797, -1699, -1699, -1699, -1699,
     715, -1699, -1699,   717, -1699,  8478,   862, 18048, -1699,   737,
    3380,   434,   739,   736,   744,   746, -1699, -1699, -1699,  3597,
    3313,   772,   812,   350, -1699, -1699,   482,   482,    44,   228,
     384,    44, -1699,   482,   482, -1699,  3498, -1699, -1699,   793,
     814,   934, 13542, -1699, -1699, 15504, -1699, -1699,  1291, -1699,
    1126,   371,   819,   891,   228,  3380,   525, -1699, 12822, -1699,
     934,   934,   826,   891,   228,  3380, -1699, 12571, -1699, -1699,
     934, -1699,   934, -1699,   850,  3449,  3380, -1699,  1635,   843,
   -1699, -1699, -1699, 15956,   434,   254, -1699, -1699,  9304, -1699,
     812,     4, -1699, 18696, 17274,  3839,  3498, -1699,   385, -1699,
   -1699, -1699, 16262,  3380,   849, -1699, -1699, -1699, -1699,  3380,
    2501,   253,   150, -1699,  3380,   529, -1699,   890,   482,   873,
   16464,   859, 14122, 13700,  1291,  1291, -1699,  1291,   934,  1291,
     934, -1699, -1699,   482, -1699,   858, -1699, 16614, -1699, -1699,
   -1699, 16666,   715, -1699,   879,   216,   732,   886,   371,   903,
   -1699,  2853,   914,   529,  2853,  1979, -1699,   930,   992, 18768,
     943,   946, 18696, 18840,   969, -1699, -1699, -1699, -1699, -1699,
   -1699, -1699, 18912, 18912, 14430,   965,  4311, -1699, -1699, -1699,
   -1699,   972, -1699,   983, -1699,  1117, -1699, 18696, 18696, -1699,
     957,   686,   889,  1025,   543,  1031,  1011,   987,  1007,  1051,
     -42, -1699,   534, -1699,  1049, -1699,  1040,  3841, 14892, -1699,
   -1699,   578,  1049, -1699, -1699,   606, -1699, -1699,  3855,  1041,
    1052,  1057,  1060,  1063,  1069, -1699, -1699,   419,  1071, -1699,
     -20,  1071, -1699, -1699, 16210, -1699,  1068,  1072, 15046, -1699,
   -1699,  3169,  3057,  1097, 14122,  1122,   547,   721, -1699, -1699,
   -1699, -1699, -1699,  3380,  3773, -1699, -1699, -1699, -1699, -1699,
   -1699,  1078,  3510,   965,  8478,  1099,  1104, -1699, -1699,  1108,
   18048,   540, -1699, -1699, -1699, 18120,  1116, -1699, -1699, -1699,
   -1699, -1699,  3597,   679,  1120,  1163,  1169,   783,  1172,  1180,
    1186,  3313, -1699, -1699,   482,  1132,   525,  1119, -1699, -1699,
    1136, -1699, -1699,   434,   891, -1699, -1699, -1699,   434, -1699,
   -1699,  3498, -1699, 14892, 14892, -1699,   934,  4247, 17896, 13964,
   -1699, -1699, -1699, -1699, -1699,   434,   891,     4, -1699, -1699,
    1291,  1150,   891,   228, -1699,   434,   891, -1699, 12714, -1699,
     934,   934, -1699, -1699,  1178,   390,  1183,   371,  1196, -1699,
   17326, -1699,   623, -1699,  1275, 17792, -1699,  4247, 16825, 13542,
   -1699, 15956, 18984, -1699, -1699, -1699, -1699, -1699,  3839,   824,
    3498, -1699, 13964,   812, -1699,  1203, -1699,  1218, -1699, -1699,
   -1699, -1699, -1699,  2853, -1699, -1699,  1297,  3670, 16666, 10618,
   -1699, 16877, -1699,   934,   934, -1699, -1699,   715, -1699,  1080,
    1222,  1373, 18696,   925,  1136,  1221, -1699,   482,   482, -1699,
    1071, -1699, 16464, -1699, -1699, 17607,   934,   934, -1699,  3670,
     482, -1699, 17131, -1699, -1699, 16614, -1699,   304,  1254,   367,
    1257,   732,   632, 16262,   645, -1699, -1699, -1699, -1699, -1699,
   -1699,   725, -1699,  1263,  1242, -1699, 14738, -1699, 16929, 16929,
   -1699, 14738, -1699, 18696, 14738, -1699, -1699, 16008, 16929, 16929,
    1040,  1433,  1478,   647,  1528, -1699,   773,  1268,  1070,  1271,
   -1699, 18120, 18696, 18192,  1267,  1635,  1635, -1699,  2063, -1699,
   -1699, 18264,  2283, 18696, 18264,  1635, -1699, -1699, 18696, 18696,
   18696, 18696, 18696, 18696, 18696, 18696, 18696, 18696, 18696, 18696,
   18696, 18696, 18696, 18696, 18696, 18696, 18696, 18336,  1259,   591,
    3612, 10618, -1699, -1699, -1699, -1699, -1699, -1699, -1699, -1699,
   -1699, -1699, -1699,  1285, 18696, -1699, -1699,   578,  1155, -1699,
   -1699,   482,   482, -1699, -1699, 14892, -1699,   435,  1071, -1699,
     497,  1071, -1699, -1699, -1699,  1136, -1699, -1699,  1136, 19056,
   -1699, -1699, 10618,  1300,  1307,  3137,  1446,  2514,   436,  1221,
   -1699,   482,   482,  1221,   472, -1699,   482,   482, 18696,  3380,
    1088,  1103,  1221,   -32, 13490, 13490,  3380, -1699, -1699, 18696,
    1108, -1699,  8478,  1323, -1699,  1276, -1699, -1699, -1699, -1699,
   -1699,   778, -1699, 13490,  1635,  4247,  1635,   596,  1328,  1330,
    1336,   808,  1346,  1348,  1349,   483,  1071, -1699, -1699,   485,
    1071, -1699, -1699, -1699,  4247,   591, -1699,  1071, 19056, -1699,
     434, 17326, -1699, -1699,   809,  1350,   835,  1356, -1699,  1360,
   -1699,   434, -1699, -1699,   434,   891,  1360, -1699,   434,  1353,
    1354,  1355, -1699, -1699, 17607, -1699,  1363, -1699, -1699, -1699,
    1635,  3380, 10112,  1447,  1343, 17694, -1699,  1072, -1699, 13490,
     844, -1699,  1360, -1699, 16262, 14892,  1351, -1699,  1351, -1699,
   -1699, -1699, -1699, 16614, -1699, 10780, 15200, -1699, 17326,  1370,
    1372,  1375, -1699,  9001,   482, -1699,   925, -1699, -1699, -1699,
   -1699,  1136, -1699, -1699, -1699,   934, -1699,  2925, -1699, -1699,
     371,   960,  1382, -1699, 18048, -1699,   732,  1254, -1699, -1699,
    1371,  1381,  1979, 18264, -1699,  1395,   119,  1392,  1397,  1398,
    1400,  1399, 18696,  1401,  1405,  1406, 10618, 18696, -1699, -1699,
    1589, -1699, -1699, -1699, 18696, -1699,  1407,  1409, 17904,  1113,
   -1699, 18264, -1699, -1699, -1699,  2687, -1699, -1699,   848, -1699,
   -1699, -1699, -1699,  2687, -1699, -1699,  1124,   460, -1699, -1699,
     957,   957,   957,   686,   686,   889,   889,  1025,  1025,  1025,
    1025,   543,   543,  1031,  1011,   987,  1007,  1051, 18696,  1130,
   -1699,  1411,  2687, -1699, -1699,  8478, -1699, 17326,  1412,  1416,
    1419,  1155, -1699, -1699, -1699, -1699, -1699, -1699, -1699, -1699,
    1136, -1699, -1699,  1136, 17326, 17326, -1699, -1699,  3137,   845,
    1421,  1422,  1423,  1426,  2301,  2514, -1699, -1699, -1699, -1699,
   -1699, -1699, -1699, -1699, -1699, -1699, -1699, -1699, -1699, -1699,
    1424, -1699,  1221, -1699, -1699, -1699, -1699, -1699, -1699, -1699,
   -1699,  1429,  1430, -1699,   525,  2687,  1128,   127, -1699, -1699,
    1434, -1699, 18048, -1699, 18696, -1699, 18408, 13490, -1699, -1699,
   -1699,  1414,   495,  1071, -1699,   517,  1071, -1699, -1699, -1699,
   -1699,  1136, -1699, -1699, -1699,  1136,   812,  1437,  1136, -1699,
   -1699, -1699, -1699, -1699, -1699, -1699,  1441, -1699, -1699,  1360,
   -1699,   434, -1699, -1699, -1699, -1699, -1699,  9724,  1445,  1442,
   -1699,   227, -1699,   336,   277, 10456,  1450, 13319,  1459,  1462,
    2400,  2884,  2019, 18480,  1463, -1699, -1699,  1464,  1465, -1699,
   -1699,   434, 18696, 18696,  1602,  1460,   140, -1699,  1546,  1466,
    1451, -1699, -1699, -1699,  9940, -1699, -1699, -1699, -1699, -1699,
     811, -1699, -1699, -1699,  1530, -1699, -1699, -1699,  1635, -1699,
   -1699, 11903, 15504,  1468, -1699,  3380, -1699,  1453,  1474,  1476,
   -1699,  1138, -1699, -1699, -1699,  4247, -1699, -1699,  1457,  1458,
     867, 16262,   529,   529, -1699, -1699,   965,  1072, 15046, -1699,
    1049, -1699, 10942, -1699,   537,  1071, -1699,   934,  9510, -1699,
   -1699,   732,   482,   482,   304,   367, -1699, -1699,  1254,  1485,
    1487, -1699, -1699,   874,   345, 10618,  1635, -1699,   345, 16060,
     345, -1699, 18696, 18696, 18696, -1699, -1699, -1699, -1699, 18696,
   18696,  1479,  8478, -1699, -1699,  1482,   492, -1699,  3193, -1699,
   -1699,  1151, -1699,   -14, -1699, 18264,  1156, -1699, 18120, -1699,
   -1699, 18696,  1470,  1158,  1179,  1108, -1699,   542,  1071, -1699,
   -1699, 17326, 17326, -1699, -1699,  1489,   565,  1071, -1699,   575,
    1290,   482,   482, -1699, -1699, 17326, 17326, -1699,  1492, -1699,
   13964, 13964,  1496,  1495,  1498,  1500, -1699,  1499, 18696, 18696,
    1191,  1501, -1699, -1699, -1699, -1699, -1699, -1699,  1505, 18696,
   -1699, -1699, -1699,  1136, -1699, -1699, -1699,  1136, 17326, 17326,
     525,   482,  1193,  1506,  1510, -1699, -1699,  1512, 12056, 12209,
   12362, 16262, 16929, 16929,  1513, -1699,  1491,  1493,  2102, 12664,
   -1699,   231,  3380, -1699, -1699,  3380, -1699, 17976,   124,   308,
   -1699, -1699, -1699, -1699, 18696,  1517,  1583,  1518,  1520, -1699,
    1503, -1699,  1508, 18696,  1509,  8478,  1514, 18696, 18120, 18696,
     937, -1699,  1515,    24, -1699,    -6,  1523, -1699, -1699,  1527,
   -1699,  1516, -1699,  1521,  1531, 13319,   660, 12980,   482,   303,
   -1699, -1699, -1699,  1533, -1699,  1538, -1699,  1539, -1699,  1536,
   -1699,  1544, -1699, -1699, -1699, -1699, 11104,  1540,  1547,  1549,
   -1699,  1558, -1699, -1699, -1699,  1136, 18696, 18696,  1072,  1564,
   -1699,  1254, -1699,  1555,   559, -1699,  1577, -1699, -1699, 16262,
   -1699,  1579,  1572,   883, -1699,  1575, -1699, -1699, -1699, -1699,
   -1699,  8478,  1108, 18120, -1699,  1614,  2687, -1699,  1614,  1614,
   -1699,  2687,  4081,  4326, -1699, -1699,  1197, -1699, -1699, -1699,
    1586,  1587, -1699, -1699, -1699,  1136, -1699, -1699,  1598,  1600,
     482, -1699, -1699, -1699,  1136, -1699, -1699, -1699,  1606, -1699,
   -1699, -1699, -1699, -1699, -1699, -1699, -1699, -1699, -1699, -1699,
   -1699, -1699,  1599, -1699, -1699, -1699, -1699,  1613,  1609,   482,
   -1699, 17326, 17326, -1699, -1699, -1699, -1699, 18696, -1699, -1699,
    1620, -1699,  1513,  1513,  1513,  1127,  1595,   396, -1699,  4009,
     422, 14892, -1699, -1699, -1699,  3983, 18696,  3737,   424, -1699,
   -1699,    78,  1622,  1622,  3380, -1699, -1699, 17475, -1699, 18696,
    1621,  1631, -1699, -1699, -1699, -1699,   908,  1636, 13319, 10456,
   13319, 10284, -1699, -1699,   465, -1699,  1108, -1699,   924,   933,
     942, -1699, -1699, -1699, -1699,   434,   937,  1638, -1699, -1699,
   18696, -1699,  1639,   591, 10456, -1699, -1699, -1699, -1699, 18696,
    1682, -1699, 13319, -1699,   482, 13964, -1699, -1699, 16262, -1699,
   -1699, -1699, -1699, -1699,  1641, -1699, 17326, -1699, -1699,  1642,
   -1699,  1643,  1644,  1633,   732, -1699, -1699, -1699, -1699, 18696,
   -1699, 16060, 18696,  1108,  1650,  1201, -1699,  1208, -1699,  2687,
   -1699,  2687, -1699, -1699, -1699, -1699, 17326,  1652,  1653, -1699,
   -1699, 17326, 17326,  1656,  1657,  1215, 13648, 13806, -1699,  1647,
   -1699, -1699, -1699, -1699,  1659,  1660,  1217, -1699, -1699, -1699,
   -1699,  1127,  2036,   499, -1699, -1699, -1699, -1699,   482,   482,
   -1699, -1699, -1699,   546, -1699,   974,  3983,   694, -1699,  3737,
     482, -1699, -1699, -1699, -1699, -1699, -1699, -1699, -1699,   548,
   13319,   203, 18552, -1699,  1466,  1663, 18696,   323,  1662,   326,
   12521, 16262, -1699, 18696, 18696,   827,   251, -1699, 18696, -1699,
    1670,   291, 13319, -1699, -1699,  1672, -1699, -1699,  1648,   591,
     574,  1674,  1678,  1237,  1735, -1699, -1699, -1699,  3380,  4247,
   -1699, -1699,  1676,  1680, -1699, -1699, -1699,   732,  1254,  1684,
   -1699, -1699, -1699,  1687, -1699, -1699, -1699,  1251,  1256, -1699,
   -1699, -1699, -1699, -1699, -1699, -1699, -1699, -1699, -1699,  1685,
   -1699, -1699,  1689,  1691, -1699, -1699, -1699,  1693,  1694,  1695,
    2036, -1699,   482, -1699, -1699, -1699, -1699, -1699,  1703,  4009,
   -1699, -1699, 18696,  1702, -1699, -1699, 13083, -1699,  1664,   977,
   13319,  1466, 14280,  1466,  1673, -1699, -1699, -1699, -1699,  5784,
   18696, 13319, 10284,  1686,  1690, -1699, -1699, -1699, -1699, 17079,
   -1699,  1709,  1696,    19, 13319, -1699, 18696, 18264,   468, -1699,
   -1699, -1699,  1716, -1699, -1699,  1254,  1720, -1699, -1699, -1699,
   -1699,  1722,  1726,  1728, 13964,  1725, -1699, -1699,   594,  1071,
   -1699, -1699,  1127, -1699,   141, -1699,  1272, -1699, -1699, 11585,
   -1699, -1699, -1699,  1697, -1699, 18696,  1729, 18696,  1184,  1714,
     280, -1699, -1699, 18696, -1699, 11585, 17079, -1699,  4118, 16877,
    1635,  1736, -1699,  1790,  1751,   602,  1746, -1699,  1829, -1699,
     980, 13319,  1754, 13319, 13319, -1699,  1756, -1699, -1699, -1699,
   -1699, -1699, -1699, -1699, -1699,  1136, -1699, 18696, 18696, -1699,
    1377, 11744, -1699, -1699, -1699, -1699,  1466,  1755,  1759, 18696,
   18696, 18696, -1699, -1699,  1377, -1699,  1732,  3423,  2741, -1699,
   -1699, -1699,    19,  1761, 18696,  1734,    19,    19, 13319, -1699,
   -1699, 18696,  1805,  1809, -1699, 17326, -1699, -1699, 13083, -1699,
    1377, -1699,  1752,  1760,   366, -1699,  1466, -1699,  1732, 18696,
    1768,  2741,  1765,   591,  1776, -1699,   608, -1699, -1699,   985,
    1735,   188, -1699, -1699, 13201,  1780, 13083, 18696, 18624, 18696,
    1782,  1783, -1699,   434,   591,  1787, -1699,  1763,   591, -1699,
   -1699, 13319,  1866,  1791, -1699, -1699, 13201,  1466, -1699,  1466,
    1466, -1699,   434, -1699, -1699,  1301, 18696, -1699,  1010, -1699,
   13319, -1699, -1699,   591,  1635,  1793,  1767, -1699, -1699, -1699,
    1042, -1699, -1699,  1772,  1635, -1699, -1699
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
     270,   271,   196,   198,     0,     0,     0,     2,     2,   266,
       0,   264,     0,     0,     0,   637,     0,     0,     0,     0,
       0,   151,     0,     0,   325,     0,     0,     3,   202,     0,
     194,     0,   261,     0,     0,     2,     0,   484,   739,     0,
     344,   885,   884,     0,     2,     0,   695,     2,   690,     0,
     691,     0,   673,   654,   658,   656,   404,     0,     0,     0,
       3,     0,     2,   894,   896,   897,     0,     0,    91,     0,
       3,   972,   574,     0,   584,   582,     0,   572,   686,   404,
     748,     0,     0,     0,    32,     0,   103,   105,   104,   101,
     100,   637,   972,     0,    56,    72,     0,    66,    73,    74,
      51,     0,     0,     0,    60,    47,     0,   144,   354,    30,
       0,     0,     2,   870,   872,   873,     3,     3,     0,     0,
     739,     2,   841,   843,   844,     2,   858,   860,     0,   835,
     850,     3,     3,   969,     3,   595,   594,   598,   971,     2,
       2,   970,     0,     3,   736,   647,   648,     0,     0,   739,
     385,   404,   404,     3,     3,   391,   738,     0,   829,   713,
       0,   715,   523,   523,   523,   558,   528,     0,   564,   552,
       0,   404,   515,   550,   546,     0,     0,     0,     0,   553,
     555,   739,   566,   566,     0,   547,   562,   404,   350,     0,
       0,    58,   274,   275,   272,   273,     0,     0,     2,   404,
       2,   404,   267,   265,     0,   259,   972,   268,     0,     0,
       0,   306,   307,   308,   309,     0,   299,     0,   300,   276,
       0,   277,     0,     0,   404,   204,   192,   263,   262,     0,
     297,   316,     2,   348,   739,   404,   711,   674,   404,     2,
       2,   946,   947,   948,     0,   899,   404,     3,     3,     0,
     907,     0,     0,     0,     0,   583,   571,     3,    89,     0,
      31,   404,     0,   972,     0,     0,    76,     0,    64,     0,
      70,     0,    68,    37,   149,   875,   404,     0,     0,   780,
     798,   404,   404,     0,     0,     0,   404,   404,   650,     0,
     371,   373,     3,     3,     0,     0,     0,   717,   519,   521,
     517,     0,   914,     0,   559,   919,   561,   911,   739,   739,
     545,   565,   549,     0,   548,     0,     0,     0,   568,     0,
     739,   542,   556,   567,   557,   563,   602,   606,   605,     0,
       2,     0,     0,   230,   211,     0,     0,   213,   358,   212,
     484,   404,   234,     0,   175,   240,     0,   235,   175,   260,
       0,     0,     2,   283,   310,     0,   301,     2,     0,     0,
       0,     0,   288,     0,   284,   190,   372,   689,     0,     0,
     949,     3,     0,     0,   906,   908,   573,     0,   972,     2,
      35,    33,    34,     0,    54,   168,    67,     0,     0,     3,
     781,   799,     3,     3,   846,   861,   375,     2,   592,     3,
     591,   649,     0,     0,   772,   830,   880,     0,     0,     0,
     915,   916,   739,   544,   912,   913,   543,   524,     0,     0,
     203,   282,     0,     0,     2,   222,     2,   205,     0,     0,
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
   -1699,  6091,  6309, -1699,    -1,   348,  1758,  -173, -1699,  1571,
   -1699,   357, -1699,  -671,   624,   718,  -891, -1053, -1699,   237,
    5751,  1704, -1699,  1255, -1699,  1289,   116,   777,   779,   520,
     775,  1252,  1253,  1260,  1261,  1262, -1699,  -103,  -157,  8309,
     836, -1699,  -364, -1699, -1699,  -644,  4100, -1081,   706, -1699,
      75, -1699,   831,    31, -1699, -1699, -1699,   406,    93, -1699,
   -1698, -1386,   293,    91, -1699, -1699, -1699,   200,   149, -1699,
   -1699, -1699, -1699,    47, -1612,   191, -1699, -1699,    50, -1699,
   -1699, -1699,    64,   438,   439,   151, -1699, -1699, -1699, -1699,
    -686, -1699,    89,    41, -1699,   155, -1699,   -40, -1699, -1699,
   -1699,   838,  -814,  -947, -1261, -1699,    25,    21,   110,  7759,
    -877,  -837, -1699,  -277, -1699,    29,  -122,   225,  -213,  -234,
    3592,  4430,  -603, -1699,    40,    22,   221,  1380, -1699,  1946,
   -1699,   185,  3935, -1699, -1699, -1699,    55, -1699, -1699,   753,
     204,  4397,  2490,   -23,  1753,  -280, -1699, -1699, -1699, -1699,
   -1699,  -477,  5052,  5165, -1699,  -356,  -239, -1699,   502,   247,
   -1699,   195,   696, -1699,   498,   -29, -1699, -1699, -1699,  5670,
    -616, -1151,  -667,  -453,  -508,   109, -1699, -1223,  -152,   643,
    1835,   864,  2917,  -129,  -488,  -247,  -154,  -419,  1228, -1699,
    1542,    76,  1147,  1436, -1699, -1699, -1699, -1699,   313,  -162,
    -120,  -846, -1699,   160, -1699, -1699,   607,   453, -1699, -1699,
   -1699,  2022,  -701,  -464,  -914,   118, -1699, -1699, -1699, -1699,
   -1699,   378,  -802,  -134, -1636,  -186,  6919,   -68,  6566, -1699,
    1123, -1699,  1932,  -181,  -211,  -126,  -124,     1,   -72,   -59,
     -56,   621,   -22,    -8,    -4,   -82,   -27,   -67,   -61,   -54,
    -711,  -669,  -637,  -634,  -690,  -130,  -632, -1699, -1699,  -657,
    1302,  1304,  1305,  1827,  7645,  -569,  -571,  -533,  -525,  -727,
   -1699, -1441, -1568, -1558, -1539,  -588,  -150,  -201, -1699, -1699,
     -62,   198,   -79, -1699,  8102,   347,  -593,  -556
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,  1128,   213,   381,   382,    80,    81,   383,   358,   384,
    1413,  1414,   385,   948,   949,   950,  1231,  1232,  1233,  1425,
     407,   387,   388,   389,   663,   664,   390,   391,   392,   393,
     394,   395,   396,   397,   398,   399,   400,   409,  1047,   665,
    1352,   724,   207,   726,   403,   791,  1129,  1130,  1131,  1132,
    1133,  1134,  1135,  1934,  1136,  1137,  1357,  1665,  1815,  1816,
    1755,  1756,  1757,  1909,  1910,  1138,  1676,  1677,  1770,  1139,
    1140,  1141,  1142,  1143,  1144,  1365,  1693,  1854,  1789,  1145,
    1146,  1546,  1920,  1547,  1548,  1837,  1147,  1148,  1149,  1355,
    1845,  1846,  1847,  1965,  1980,  1870,  1871,   284,   285,   852,
     853,  1101,    83,    84,    85,    86,    87,  1668,   440,    90,
      91,    92,    93,    94,   221,   557,   442,   411,   443,    97,
     294,    99,   100,   101,   323,   324,   104,   105,   166,   106,
     870,   325,   152,   109,   241,   110,   153,   250,   327,   328,
     329,   154,   404,   115,   116,   331,   117,   548,   841,   839,
     840,  1502,   332,   333,   120,   121,  1097,  1320,  1508,  1509,
    1633,  1634,  1321,  1497,  1652,  1510,   122,   630,  1583,   334,
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
     610,  1636,  1637,  1638,  1639,   595,   453,   339,   340,   341,
     416,   199,   141,   142,   143,   343,   783,   611
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      79,   183,   131,    79,   357,   181,   556,   515,   484,   528,
     335,   353,   338,   957,   184,   907,   297,   185,   668,  1170,
     198,   402,    88,   103,   893,   825,   827,   939,   879,   148,
      95,  1020,  1346,   349,   887,   902,   496,  1027,  1150,   492,
     321,   102,   613,   725,  1405,   780,  1016,  1465,  1466,   888,
    1154,   186,   190,   474,    79,    79,   111,    79,  1821,   131,
     289,   932,  1236,  1737,   861,   187,   880,  1017,   231,   188,
     836,   992,    79,  1738,   881,   401,  1552,    57,   155,    88,
     103,    79,  1788,   196,   484,   198,  1010,    95,   706,    79,
    -697,  1243,  1739,   485,    79,   486,   228,    79,   102,   253,
     495,    79,   512,   263,   563,   565,  1432,   590,  1043,   419,
     862,    89,   435,   111,   149,   492,  1817,   564,  1011,  1550,
     248,  1012,   420,  1013,   621,   421,  1058,   256,   624,   746,
     707,   209,   144,   497,   356,  1092,   623,   487,  1433,    79,
     626,   602,    79,  1553,    79,   909,   131,   183,    57,    79,
    1315,   482,   488,   564,  1277,    79,   162,   260,   489,   422,
     184,  1162,    79,   185,   613,   490,    88,   103,    89,   485,
     596,   486,  1843,   423,    95,  1427,   176,   424,  1307,    79,
      79,   196,   209,   521,   879,   102,   107,  1050,   887,   504,
    -698,  1741,  1160,   493,    79,   585,   456,   186,  1551,   140,
     111,  -721,   140,  1021,   465,   112,   280,  1024,   246,    79,
    1821,   187,   257,   487,   526,   188,  1037,  1038,    79,    79,
    1316,   196,   880,   183,   536,   590,    96,   568,   488,   150,
     881,   497,  1817,   585,   489,    79,   184,   156,  1821,   185,
     521,   490,  1203,   107,    79,   669,   196,   996,  1375,   816,
     210,   543,  -721,  1585,    79,    89,   140,    79,  1278,   532,
    1317,  1951,   112,   292,    79,   904,  1042,  1042,  1667,   493,
    1226,   798,  1867,   191,    79,    79,  1118,    79,  1752,  1753,
     530,   279,  1020,    96,   856,  1042,   477,   161,   596,   179,
    1198,  1332,  1279,  1667,    79,    79,  -625,   196,  1522,  1811,
     140,   582,    79,  1333,  1263,   583,  1279,   194,    79,    79,
     812,   613,   764,    79,  1199,  -341,   784,  1190,  1788,  1737,
    1150,  1041,  1041,   559,   266,  1264,   955,  1217,   267,  1738,
     107,   270,  1154,   272,  1250,   613,  1328,  1329,   111,  1010,
    1041,  -342,   613,   140,   248,    79,   198,    19,  1739,   112,
      79,  1042,   146,    79,   640,   751,   799,   798,   800,   560,
    1754,  1315,  1315,  1315,   287,   179,  1752,  1753,   529,  1089,
      96,  1011,   194,  -855,  1012,   872,  1255,   823,  1598,  1600,
    1602,   175,  1325,   828,   578,  -341,  1514,   874,  1305,   812,
    1287,  1490,   157,  1465,  1466,   158,   159,  1890,   160,   892,
     801,  1326,  1771,   170,   170,  1515,  1041,  1772,    62,    63,
     419,  -342,   898,   579,   580,   802,   433,    79,   861,   456,
      57,   803,   338,   420,   899,   537,   421,   632,   804,   201,
     634,  1316,  1316,  1316,   279,   934,   549,   879,   170,   190,
      79,    79,   799,   177,   800,  1881,  1190,  1741,  1775,   667,
     321,  1330,    79,    79,    57,    57,    75,  1394,  1325,   178,
     422,    79,   596,   465,   813,   279,   248,   517,   107,   266,
     520,  1317,  1317,  1317,   423,   880,  1269,  1563,   424,   179,
    1811,    79,  1524,   881,  1908,  1327,   801,   112,   170,    57,
      79,   170,   456,   835,   201,  1221,   419,   202,  1908,   278,
     216,   802,  1222,   497,   170,    57,    57,   803,    96,   420,
      79,   347,   421,   204,   804,    -3,    79,   915,   236,   917,
     918,   934,   919,   274,  1936,   921,   417,   520,   923,   924,
     925,  1939,  1457,   505,   571,  1597,   981,   497,   497,   179,
     903,  1851,    57,   813,  -398,   266,   267,  -520,   617,  1042,
     272,  1641,    57,    57,    79,    57,    79,  1364,   170,    13,
      14,    15,    16,    17,   613,    57,  1245,    79,   742,    79,
    1642,   456,   497,   276,  1852,  1401,   278,  1514,   819,  1650,
     248,  1861,    79,   822,   997,  1018,    79,    57,   497,   600,
     893,   279,   522,  1764,  1436,   613,  1644,  1773,  1651,  -341,
     830,   559,   293,   170,  1041,  1406,   503,    57,  1239,   508,
     837,  1031,    57,   170,   260,  1235,   111,    57,    79,   351,
     934,  1025,   135,   546,   170,   600,   551,   764,  1051,   876,
      79,   525,  1068,   194,  1072,    57,   497,   530,   497,  1679,
    1424,   535,  1077,   354,  1290,    57,  1001,  1235,   497,   522,
     497,   170,  1440,   355,  1742,  1645,  1520,   170,   170,   750,
    1174,  1060,   170,   426,    57,   401,  1294,   598,   696,   697,
     497,   204,  1175,  1743,    79,    79,  1046,    79,  -402,   135,
    1076,    79,   205,  1383,    79,   708,  1392,   543,  1829,   709,
     600,  1442,   785,   786,   170,   497,   787,   861,   206,   170,
     310,  1650,   170,   934,  1539,   204,   356,    72,  1717,    79,
    1718,   274,   698,   699,  1451,  1666,   229,  1678,   497,   254,
    1746,   667,  1750,   264,  1455,  1782,   667,   727,   600,   667,
    1783,   497,   903,    13,    14,    15,    16,    17,    77,    78,
    1666,   627,   455,  1862,  1875,   278,   107,   497,   667,   497,
    1197,   266,  1883,  1895,    79,   201,    79,   734,  1896,  1947,
    -399,   735,  1464,   427,  1948,   112,   135,  1193,    79,    13,
      14,    15,    16,    17,   849,    79,   338,   428,   850,  1595,
     150,   465,   429,   908,    79,   598,    96,   583,   278,   170,
     425,    57,   876,    79,    79,    79,   910,  1276,   927,  1915,
     583,   170,   170,  1422,   321,   960,   961,   962,  1582,   928,
     929,   237,   238,    79,   239,   850,   457,   279,   240,   243,
       6,     7,     8,     9,    10,    11,    12,    57,   278,  1594,
     425,   904,   497,    61,   430,   452,   168,   169,    64,    65,
      66,    67,    68,    69,    70,   425,   689,   497,   431,    79,
      79,   465,  -403,   690,   691,  1308,  1309,  1310,  -400,   157,
     417,   417,   158,   159,   459,   160,   248,    13,    14,    15,
      16,    17,   460,    88,   103,  1240,   911,   530,   861,   533,
     912,    95,   820,   473,  1262,   764,   475,  1361,   478,  1163,
    1300,   479,  1152,   189,    63,  1080,    79,   248,  1283,   480,
      79,   481,   613,   146,   831,    79,  1088,   111,   495,  1090,
     834,   640,    61,  1093,   838,   168,   169,    64,    65,    66,
      67,    68,    69,    70,   933,    57,  1536,   494,   934,  1055,
    1081,   170,   505,  1056,   808,   871,   497,    13,    14,    15,
      16,    17,   513,  1680,    79,   446,    13,    14,    15,    16,
      17,  1046,    79,  1396,   542,    63,  1496,   505,  1678,   894,
    1082,   497,    89,   514,   934,   892,  -389,   896,  1708,  1194,
    1765,  1766,  1767,   571,  1164,   425,   524,   497,   209,   170,
     417,    79,   934,   534,   465,  1362,  1084,  1183,    72,  -389,
     934,   553,  1768,   571,  1018,    57,   425,   497,   600,  1234,
    1713,  1769,  1593,  1235,    57,  1872,   574,    79,   599,   620,
     353,   353,   600,    79,    79,  1628,  1629,  1630,  1382,    77,
     601,  1872,   735,   942,   943,  1410,   946,   588,   596,  1235,
     954,  1376,   602,   958,  1590,  1322,  -854,   107,  1591,   457,
     904,   586,   274,  -570,    79,  1541,  1542,  1543,  1544,  1545,
     140,   692,   693,  1485,    72,   338,   112,  1911,   983,  1661,
     631,    61,   140,   934,   168,   169,    64,    65,    66,    67,
      68,    69,    70,   593,   599,  1681,   616,    96,   600,   934,
    1415,   644,   417,   321,  1682,    77,   601,   633,  1056,  1165,
     593,  1795,   648,  1683,   593,   649,  1246,   934,   243,     6,
       7,     8,     9,    10,    11,    12,   465,   645,  1729,    79,
      79,    79,   457,  1253,  1254,  1796,  1850,   170,   653,  1467,
     671,  1473,  1474,   688,   170,  1747,    88,   103,  1823,   735,
     675,  1899,   934,   465,    95,  1235,  1949,  1512,  1437,    79,
     934,   676,  1059,    18,  1061,  1152,   703,    79,   694,   695,
      79,    79,   253,   263,    79,    88,   103,   265,   700,   701,
     111,  1976,   452,    95,    79,  1973,  1306,    13,    14,    15,
      16,    17,   702,   248,  1152,   401,   401,   256,   704,  1331,
     705,    47,    48,    49,    50,    51,    52,    53,    54,   111,
      79,   593,   736,  1983,   191,   671,  1350,  1984,  1100,   170,
     170,   710,  1856,   737,   530,    79,  1922,   260,   738,    72,
    1926,   739,   417,  1091,   740,    89,   967,   968,   969,   970,
     741,   465,   588,   671,   432,    57,    -3,    79,   768,   599,
     936,   937,   452,   600,   -16,   677,   338,   678,   679,   680,
      77,    78,  1033,  1034,    89,  1192,  1322,  1322,  1322,   170,
    1498,  1322,  1669,  -401,   170,   -17,    72,  1035,  1036,    79,
     781,   246,   257,   782,   321,   792,   681,  1224,  1056,   682,
     683,   805,   446,   817,   684,   685,  1631,  1669,  1237,  1238,
     497,   815,  -147,  -147,    72,   934,  1241,    77,    78,   286,
     107,  1513,  1035,  1374,   484,   243,     6,     7,     8,     9,
      10,    11,    12,   140,   727,  1430,  1431,   833,   497,   112,
    1435,  1431,  1439,  1431,   806,    77,    78,    79,  1512,   107,
     807,    79,  1318,   809,    79,   446,   492,  1765,  1877,  1767,
      96,   810,   140,  1007,  1423,  -518,   148,   811,   112,   934,
    -516,   593,   446,   851,   465,  1475,  1423,  1007,  1487,  1878,
     140,  1603,  1056,   842,   452,  1715,  1056,   863,  -176,    96,
    1446,  1447,  1716,  1431,   465,   593,    79,   865,   532,  1726,
    1727,  1736,   934,   869,  1461,  1462,   882,    61,   593,   485,
    1690,   486,    64,    65,    66,    67,    68,    69,    70,   530,
     884,    61,  1786,  1787,   602,   452,    64,    65,    66,    67,
      68,    69,    70,  1523,  1525,  1799,  1431,  1483,  1484,   901,
    1800,  1431,   913,  1467,   906,   338,   914,   452,   452,   935,
     465,   149,   938,   487,   941,    79,    74,  1868,  1869,   777,
      79,    79,    79,   980,  1163,   798,   452,  1646,   488,  1260,
      74,  1561,   985,   321,   489,  -112,  -112,  -112,  -112,  -112,
    -112,   490,  1752,  1753,  1006,   170,  1973,  1974,   170,   170,
     170,  1007,  1415,  1014,   446,  1467,  1428,  1429,   812,   963,
     964,  1053,  1513,   965,   966,   971,   972,   529,   249,  1062,
     493,  1063,   170,  1653,  1653,  1384,  1385,  1064,   170,   269,
    -111,  -111,  -111,  -111,  -111,  -111,   551,  1065,    79,  1066,
    1067,  1083,   452,   170,    79,   446,    79,  1085,  -701,   140,
    1094,  1095,  1096,    79,  -601,  1155,  1781,  1156,   417,  1164,
     799,  1184,   800,  1185,  1171,   894,  1186,   465,  1200,   465,
    1196,   249,  1201,  1318,  1318,  1318,   150,  1495,  1499,   170,
      13,    14,    15,    16,    17,   931,  1204,  1206,  1207,  1208,
    1210,   103,  1212,   103,  1411,  1209,  1213,  1214,  1219,   613,
    1220,   465,  1242,  1247,   801,  1791,  1838,  1248,  1512,  1671,
    1249,  1671,  1256,  1257,  1258,   249,   103,  1259,  1267,   802,
    -589,  -588,  1282,    79,   111,   803,   111,  1289,  1301,  -702,
    1622,  1623,   804,  1163,  1671,  1814,  1323,  1324,    79,  1334,
      79,    13,    14,    15,    16,    17,  1216,   140,  1337,   111,
    1844,  1338,  1347,  1348,  1349,  1354,  -624,   650,  1356,  1364,
    1684,   934,   813,  1838,  1368,  1358,   593,  1370,  1371,   616,
    1372,  1378,  1380,  1407,  1165,  1408,  1421,  1423,   249,    89,
    1450,    89,   686,   687,  1438,    79,  1463,  1468,    79,   401,
    1469,  1471,  1467,  1470,  1431,  1476,  1479,  1488,  1489,   465,
    1491,  1527,  1501,   686,    89,  1503,  1327,  1504,   249,  1528,
     170,  1530,  1554,   170,   249,  1556,   484,  1532,  1164,   446,
    1559,   465,  1533,  1535,  1564,  1701,  1566,  1567,  1537,  1549,
    1557,  1569,   248,   686,  1571,  1558,   256,    79,    79,  1570,
     452,  1572,   249,  1573,  1889,    82,    79,   492,   147,  1575,
    1906,  1814,  1584,   170,   107,  1719,   107,  1840,  1580,  1844,
    1722,  1723,  1513,  1844,  1844,  1586,   260,   140,  1589,   140,
    1588,  1592,  1596,   112,  1604,   112,    61,  1924,  1605,   107,
     812,    64,    65,    66,    67,    68,    69,    70,    79,  1609,
    1945,  1610,   140,  1618,    96,   465,    96,   425,   112,   465,
    1620,   485,    82,   486,   401,   401,   140,  1475,  1627,  1640,
     465,  1964,   254,   264,  1840,  1964,  1235,   180,  1506,    96,
     246,   257,  1660,   465,   530,  1662,    82,  1687,  1689,  1694,
    1707,   401,  1706,  1165,   103,  1700,  1704,  1705,  1714,   220,
    1978,  1731,   245,  1720,  1721,   487,    82,  1724,  1725,  1975,
    1734,  1735,  1671,  1336,  1760,   183,  1942,   210,  1774,   568,
     488,  1778,  1780,  1118,   249,  1784,   489,   111,   184,  1785,
    1793,   185,  1797,   490,  1794,  1798,  -590,    79,  1822,    79,
    1806,   103,  1807,   147,  1808,  1809,  1810,  1825,   417,    82,
     465,   147,   465,   465,   296,   302,   497,   103,  1818,  1671,
    1833,   493,   170,   401,  1834,  1841,   320,  1855,  1857,   140,
    1842,  1873,   529,  1858,   111,  1671,   170,  1859,   170,  1860,
    1727,  -501,    89,   408,   180,   180,    79,    79,  1880,   196,
     111,  1893,  1892,   103,   813,   147,   438,   465,   249,   245,
    1894,  1897,  1898,  1901,  1904,  1912,  1919,   465,  1925,  1913,
     170,  1671,  1930,   452,   452,  1923,  1931,  1937,   249,  1943,
      79,  1944,   456,   220,   220,  1938,   111,  1946,  1955,    89,
    1961,   593,   170,   465,  1962,   465,  1966,  1967,   249,  1970,
     296,  1982,  1971,   959,  1981,    89,  1985,   674,  1711,    82,
     465,  1521,   930,  1434,   973,   465,   974,   107,   446,  1353,
    1691,  1907,   245,   466,   975,  1360,   976,  1956,   977,   465,
     140,   249,  1832,    79,  1776,  1917,   112,  1879,  1952,  1853,
    1950,    89,  1941,    79,  1685,  1686,  1927,  1885,   533,  1968,
    1884,   635,   302,  1369,   167,   249,  1813,    96,   302,   296,
     296,  1643,   249,   523,   107,   182,   147,  1866,   170,  1500,
    1052,  1654,  1366,   788,  1932,  1173,  1587,   140,  1963,   867,
     107,  1697,     3,   112,   320,   603,   612,   223,     0,   988,
     170,   989,   990,   140,     0,  1202,     0,  1972,     0,   112,
       0,   320,     0,     0,    96,   320,   170,     0,    13,    14,
      15,    16,    17,     0,     0,   170,   107,   507,     0,     0,
      96,     0,     0,     0,     0,   636,     0,     0,     0,   140,
       0,  1560,     0,     0,     0,   112,     0,     0,   408,     0,
     637,     0,   298,   638,   639,    64,    65,    66,    67,    68,
      69,    70,     0,     0,     0,     0,    96,     0,     0,     0,
       0,     0,     0,     0,   170,   236,    57,     0,   170,     0,
       0,     0,   408,     0,     0,   728,     0,     0,     0,   170,
      61,     0,   180,   168,   169,    64,    65,    66,    67,    68,
      69,    70,   170,     0,     0,     0,     0,     0,   147,   446,
       0,     0,   438,     0,     0,     0,   757,     0,   612,     0,
       0,   483,   223,     0,     0,     0,     0,     0,     0,     0,
    1657,     0,     0,     0,    61,    72,     0,     0,   298,    64,
      65,    66,    67,    68,    69,    70,   944,  1211,     0,  1343,
       0,     0,  1215,     0,     0,  1631,   220,     0,  1891,   497,
       0,     0,     0,  1223,     0,   220,    77,    78,     0,   170,
       0,   170,   170,    61,     0,     0,   217,   218,    64,    65,
      66,    67,    68,    69,    70,   296,   945,   408,   408,     0,
     778,   296,   466,   320,   249,     0,     0,   569,   298,     0,
       0,    72,     0,     0,  1663,   249,  1672,     0,   733,     0,
       0,     0,     0,     0,     0,     0,   170,     0,     0,     0,
       0,  1505,    74,     0,   744,   249,   170,   747,  1506,   452,
     452,   296,    77,    78,     0,     0,     0,     0,  1695,     0,
       0,     0,   296,     0,   296,     0,   320,     0,     0,     0,
       0,     0,   170,     0,   170,     0,     0,     0,     0,     0,
       0,     0,   320,   438,     0,   612,     0,     0,     0,   170,
      18,     0,     0,   603,   170,     0,     0,   603,     0,     0,
       0,     0,     0,     0,   507,     0,   320,     0,   170,     0,
       0,     0,  1979,     0,     0,     0,   612,     0,     0,   320,
       0,     0,  1986,     0,     0,     0,     0,   147,     0,    57,
       0,     0,    51,    52,    53,    54,     0,     0,     0,     0,
     408,     0,   147,   147,     0,   408,     0,     0,   408,     0,
     650,   147,   147,   147,     0,     0,  1751,     0,     0,     0,
      61,    57,     0,     0,   758,    64,    65,    66,    67,    68,
      69,    70,     0,     0,    61,   848,     0,     0,  1777,    64,
      65,    66,    67,    68,    69,    70,   952,     0,    72,     0,
       0,     0,    61,     0,     0,   217,   218,    64,    65,    66,
      67,    68,    69,    70,   797,   438,     0,     0,    73,    74,
       0,     0,     0,   223,     0,     0,     0,     0,     0,    77,
      78,   728,   728,   686,     0,     0,   953,     0,     0,   408,
       0,     0,     0,   298,     0,     0,     0,   452,     0,   298,
    1260,    74,     0,     0,     0,     0,   438,     0,     0,   757,
       0,   757,  1820,     0,     0,     0,  1824,  1416,  1417,  1418,
       0,     0,     0,     0,  1419,  1420,     0,  1831,   320,   320,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   298,
    1848,   114,     0,     0,   114,     0,     0,   320,     0,   296,
     860,    61,   298,     0,   168,   169,    64,    65,    66,    67,
      68,    69,    70,     0,     0,     0,     0,     0,   296,     0,
       0,     0,     0,     0,     0,     0,    13,    14,    15,    16,
      17,   249,   593,     0,     0,     0,     0,     0,     0,     0,
     466,     0,     0,   778,     0,     0,     0,     0,   114,     0,
       0,     0,     0,     0,   733,   733,   408,  1900,     0,  1902,
    1903,     0,   249,   320,   999,     0,     0,  1002,   147,   408,
       0,     0,   114,     0,  1339,     0,     0,   320,     0,  1178,
       0,     0,     0,     0,    57,     0,     0,     0,   251,   593,
     603,     0,   114,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    61,     0,  1928,   168,   169,    64,    65,    66,
      67,    68,    69,    70,  1933,    61,     0,     0,   217,   218,
      64,    65,    66,    67,    68,    69,    70,     0,   507,   114,
     438,     0,  1070,     0,     0,   114,  1074,   114,     0,     0,
    1954,   251,  1933,    72,     0,     0,     0,     0,     0,     0,
       0,   317,   114,   348,     0,     0,   848,  1969,   576,     0,
       0,     0,  1954,   755,    74,     0,     0,   600,     0,   412,
       0,     0,     0,     0,    77,   756,  1977,     0,     0,     0,
       0,   114,   412,     0,     0,   251,     0,  1009,     0,   758,
       0,     0,     0,     0,     0,   728,     0,     0,     0,   249,
       0,     0,     0,  1230,     0,     0,     0,     0,     0,     0,
       0,  1230,   757,   848,     0,     0,     0,     0,     0,   757,
       0,     0,     0,     0,     0,     0,     0,   298,     0,     0,
       0,     0,   114,     0,     0,   114,     0,     0,     0,     0,
    1230,     0,     0,   466,     0,     0,   298,   249,   251,     0,
       0,     0,     0,    13,    14,    15,    16,    17,     0,     0,
       0,   320,     0,     0,     0,   547,     0,     0,     0,     0,
       0,     0,     0,   114,     0,     0,     0,     0,   251,     0,
       0,     0,     0,     0,   251,     0,     0,     0,    61,     0,
       0,     0,   114,    64,    65,    66,    67,    68,    69,    70,
    1227,   147,     0,  1230,  1228,     0,  1229,     0,     0,   408,
     114,    57,   251,   114,  1692,     0,     0,     0,   733,     0,
       0,     0,   848,     0,     0,     0,     0,   114,     0,     0,
       0,   114,     0,     0,     0,     0,     0,    74,   408,   848,
     848,     0,    61,     0,     0,   217,   218,    64,    65,    66,
      67,    68,    69,    70,     0,   245,    82,     0,     0,     0,
       0,     0,     0,     0,   412,     0,     0,     0,     0,   296,
      72,     0,     0,     0,     0,   147,     0,     0,     0,     0,
       0,     0,   438,     0,     0,     0,     0,     0,     0,  1292,
    1887,    74,  1296,     0,   497,     0,     0,     0,   412,     0,
       0,    77,    78,     0,     0,     0,     0,     0,     0,   438,
       0,     0,     0,   147,     0,     0,   305,   306,   307,   308,
       0,     0,     0,     0,   114,     0,     0,     0,   412,     0,
       0,     0,     0,     0,   251,     0,     0,     0,     0,     0,
    1009,     0,     0,     0,     0,     0,  1261,   758,     0,     0,
       0,     0,     0,     0,    61,     0,     0,   168,   169,    64,
      65,    66,    67,    68,    69,    70,     0,     0,     0,     0,
       0,     0,   171,   174,   320,   320,     0,     0,     0,     0,
     466,     0,     0,     0,     0,    61,  1230,     0,   168,   169,
      64,    65,    66,    67,    68,    69,    70,     0,     0,     0,
       0,     0,     0,   412,   412,     0,   309,   212,   251,   114,
       0,     0,   147,   147,   147,   147,   147,   147,     0,     0,
       0,     0,  1507,   302,   310,     0,    61,     0,     0,   217,
     218,    64,    65,    66,    67,    68,    69,    70,     0,     0,
     114,  1849,     0,     0,     0,   114,     0,     0,   251,   114,
     249,   114,     0,     0,     0,     0,     0,   290,  1341,     0,
     291,     0,   114,     0,     0,     0,     0,     0,     0,     0,
       0,   245,     0,   311,  1444,     0,     0,   348,   114,   412,
       0,   251,  1191,  1453,     0,     0,   848,   848,     0,     0,
     438,     0,     0,     0,     0,     0,     0,   298,     0,     0,
     848,   848,   114,   466,     0,   251,     0,     0,     0,   547,
       0,     0,   251,   147,     0,   114,     0,   900,     0,     0,
       0,     0,     0,   114,     0,     0,     0,   476,     0,     0,
       0,     0,     0,   848,   848,     0,   412,     0,   114,   114,
       0,   412,   249,     0,   412,     0,     0,   114,   114,   114,
       0,     0,     0,     0,     0,     0,     0,     0,    61,     0,
       0,   217,   218,    64,    65,    66,    67,    68,    69,    70,
       0,     0,   527,     0,     0,     0,     0,     0,     0,   466,
       0,     0,   171,     0,  1230,     0,    72,     0,     0,  1230,
    1230,  1230,  1458,   171,     0,     0,     0,     0,     0,  1632,
       0,   412,     0,  1507,     0,   408,   755,    74,     0,  1507,
     600,  1507,     0,     0,     0,     0,     0,    77,   756,   249,
     573,     0,     0,     0,     0,   412,   575,   577,     0,     0,
     602,   584,     0,   408,     0,   408,     0,     0,    61,     0,
       0,     0,   412,    64,    65,    66,    67,    68,    69,    70,
    1511,     0,     0,     0,     0,     0,     0,     0,   408,     0,
       0,     0,     0,   629,   114,   114,    72,     0,   311,   320,
      61,   311,   147,   168,   169,    64,    65,    66,    67,    68,
      69,    70,     0,   114,     0,     0,  1008,    74,     0,     0,
     600,     0,     0,     0,    61,   147,     0,    77,    78,    64,
      65,    66,    67,    68,    69,    70,  1227,     0,     0,     0,
    1228,   114,  1229,     0,     0,     0,   848,   848,   455,     0,
     320,   320,  1635,     0,     0,    13,    14,    15,    16,    17,
       0,     0,     0,     0,   251,  1632,  1632,     0,     0,     0,
       0,     0,   412,    74,     0,   251,  1426,     0,     0,   114,
    1507,     0,  1658,  1507,   114,   412,     0,  1230,   212,  1230,
       0,     0,     0,   114,     0,  1180,   412,     0,   114,     0,
     772,   773,     0,     0,   302,   147,     0,     0,     0,     0,
       0,     0,    61,    57,     0,   344,   345,    64,    65,    66,
      67,    68,    69,    70,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   296,     0,     0,     0,     0,     0,     0,
       0,   848,     0,     0,    61,     0,   412,   217,   218,    64,
      65,    66,    67,    68,    69,    70,     0,     0,     0,     0,
       0,  1511,     0,    75,     0,     0,     0,  1647,   346,  1511,
       0,   848,    72,     0,  1632,     0,   848,   848,     0,     0,
       0,     0,     0,  1507,     0,     0,     0,     0,  1635,  1635,
       0,     0,   219,    74,     0,     0,   302,     0,     0,     0,
       0,     0,     0,    77,    78,     0,   408,   114,     0,     0,
       0,    61,     0,   147,   168,   169,    64,    65,    66,    67,
      68,    69,    70,    57,   114,   114,     0,     0,     0,     0,
     311,     0,     0,     0,     0,     0,     0,     0,   320,     0,
      13,    14,    15,    16,    17,     0,  1632,     0,     0,     0,
       0,     0,     0,   147,    61,     0,     0,   217,   218,    64,
      65,    66,    67,    68,    69,    70,     0,     0,     0,   147,
     147,     0,  1888,   302,     0,     0,     0,   114,   629,     0,
      61,     0,    72,   544,   545,    64,    65,    66,    67,    68,
      69,    70,     0,     0,     0,     0,     0,  1635,    57,     0,
       0,     0,  1887,    74,     0,   147,   497,     0,  1748,     0,
       0,  1511,     0,    77,    78,     0,     0,   114,     0,     0,
       0,  1888,  1888,    98,     0,   412,   151,     0,     0,    61,
       0,    75,   217,   218,    64,    65,    66,    67,    68,    69,
      70,    61,     0,     0,   189,    63,    64,    65,    66,    67,
      68,    69,    70,     0,   412,  1888,     0,    72,     0,     0,
       0,   298,     0,     0,     0,  1864,     0,     0,     0,  1635,
       0,   251,   114,     0,     0,     0,     0,   295,    74,     0,
      98,     0,     0,     0,     0,     0,     0,     0,    77,    78,
      74,   114,     0,   777,     0,  1635,     0,    57,   412,     0,
       0,     0,  1180,     0,   195,     0,     0,     0,     0,     0,
       0,  1511,     0,     0,  1404,     0,  1032,     0,     0,     0,
       0,     0,     0,  1044,   258,   412,     0,     0,    61,   114,
       0,   217,   218,    64,    65,    66,    67,    68,    69,    70,
       0,     0,     0,    61,  1635,  1635,   542,    63,    64,    65,
      66,    67,    68,    69,    70,     0,    72,     0,     0,     0,
       0,   288,     0,     0,     0,     0,     0,    98,     0,     0,
     848,   114,   114,     0,     0,     0,   219,    74,  1635,    13,
      14,    15,    16,    17,   322,   114,   114,    77,    78,     0,
     114,   114,     0,     0,     0,   982,     0,     0,  1102,     0,
     298,    61,   418,     0,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   288,   444,     0,     0,     0,   114,   114,
       0,     0,     0,     0,     0,     0,     0,     0,   114,   114,
     114,   114,   114,   114,     0,     0,     0,    57,     0,   251,
       0,     0,   491,     0,     0,     0,     0,     0,  1195,   569,
     298,     0,    75,   629,     0,     0,     0,     0,   511,     0,
       0,     0,     0,   516,   518,     0,     0,   195,    61,     0,
       0,   217,   218,    64,    65,    66,    67,    68,    69,    70,
       0,     0,     0,   298,     0,     0,     0,   251,     0,   538,
       0,     0,   540,     0,   541,     0,    72,    13,    14,    15,
      16,    17,     0,     0,    61,   558,   412,   168,   169,    64,
      65,    66,    67,    68,    69,    70,  1505,    74,   570,     0,
       0,     0,     0,     0,     0,     0,     0,    77,    78,   114,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    57,
       0,     0,   591,     0,     0,   615,     0,     0,     0,     0,
       0,     0,   459,     0,     0,    57,     0,     0,     0,   622,
       0,     0,     0,   622,     0,     0,   108,     0,     0,     0,
      61,     0,     0,   217,   218,    64,    65,    66,    67,    68,
      69,    70,     0,     0,     0,     0,    61,     0,     0,     0,
       0,    64,    65,    66,    67,    68,    69,    70,    72,     0,
       0,   114,   114,   712,   713,   714,   715,   716,   717,   718,
     719,   720,   721,   722,    72,     0,     0,   204,   295,    74,
       0,   412,     0,   108,     0,     0,     0,     0,     0,    77,
      78,     0,     0,     0,    73,    74,     0,   114,     0,     0,
       0,     0,     0,     0,   723,    77,    78,     0,     0,   412,
       0,   412,     0,     0,     0,     0,   288,  1340,  1342,  1344,
     591,     0,     0,     0,     0,     0,     0,   259,     0,     0,
       0,     0,     0,     0,   412,     0,     0,     0,     0,     0,
       0,     0,     0,    57,     0,   114,     0,  1363,   114,     0,
       0,     0,     0,     0,     0,     0,   114,     0,     0,     0,
       0,     0,  1102,     0,     0,     0,     0,     0,     0,     0,
     108,   114,     0,     0,    61,     0,     0,   217,   218,    64,
      65,    66,    67,    68,    69,    70,   114,   326,     0,     0,
       0,   114,   114,     0,     0,     0,   114,   114,   629,     0,
      61,   444,    72,   217,   218,    64,    65,    66,    67,    68,
      69,    70,     0,     0,     0,     0,     0,   445,     0,     0,
       0,     0,  1505,    74,     0,     0,     0,     0,    72,     0,
       0,     0,   844,    77,    78,     0,     0,   518,     0,     0,
       0,   855,     0,   558,     0,     0,     0,     0,  1505,    74,
     251,   114,     0,     0,   322,  1506,     0,     0,     0,    77,
      78,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     622,   875,    61,     0,     0,     0,     0,    64,    65,    66,
      67,    68,    69,    70,  1227,   886,     0,     0,  1228,     0,
    1229,     0,   539,     0,   591,     0,     0,     0,     0,   895,
       0,     0,     0,     0,     0,     0,     0,   622,   108,    61,
       0,     0,   217,   218,    64,    65,    66,    67,    68,    69,
      70,    74,     0,     0,  1599,     0,     0,     0,     0,  1516,
       0,     0,  1518,     0,     0,     0,     0,    72,     0,     0,
       0,     0,   251,     0,     0,   592,     0,     0,   259,     0,
       0,     0,   412,     0,     0,     0,     0,  1887,    74,   114,
       0,   497,   592,     0,     0,     0,   592,     0,    77,    78,
       0,     0,    61,     0,     0,   217,   218,    64,    65,    66,
      67,    68,    69,    70,   114,     0,     0,     0,     0,     0,
       0,     0,     0,   444,     0,     0,     0,     0,     0,   114,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     991,     0,     0,     0,     0,   114,   114,     0,     0,   251,
     219,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    77,    78,     0,   875,     0,     0,     0,    61,  1015,
       0,   217,   218,    64,    65,    66,    67,    68,    69,    70,
       0,   114,     0,     0,     0,     0,   444,   444,     0,     0,
       0,     0,     0,   592,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,   444,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   114,   295,    74,   113,     0,
       0,     0,     0,     0,     0,     0,     0,    77,    78,     0,
     364,     0,   365,   844,   366,    62,    63,    64,    65,    66,
      67,    68,    69,    70,     0,     0,     0,    61,     0,     0,
       0,  1655,    64,    65,    66,    67,    68,    69,    70,  1227,
       0,     0,     0,  1228,  1151,  1229,     0,     0,     0,     0,
       0,   444,     0,     0,   445,   113,   151,     0,     0,     0,
     673,     0,     0,    75,   375,   622,     0,     0,  1182,     0,
     844,     0,     0,     0,   666,  1188,    74,     0,     0,  1601,
       0,     0,     0,     0,     0,   326,     0,     0,     0,     0,
       0,     0,     0,     0,   259,     0,   108,     0,     0,   261,
       0,   629,     0,     0,     0,     0,     0,   445,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   322,     0,
       0,     0,     0,   592,   445,     0,     0,     0,   247,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   268,
       0,   271,   113,   273,     0,     0,     0,   592,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   330,
     592,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   844,
       0,   247,     0,   271,   273,     0,     0,     0,     0,   447,
       0,     0,     0,     0,     0,     0,   844,   844,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   824,   826,  1790,     0,     0,     0,     0,
       0,     0,     0,     0,   629,   247,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   445,     0,     0,   444,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     113,     0,     0,     0,     0,     0,     0,   445,   247,  1319,
     271,   273,     0,     0,     0,     0,     0,  1151,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   326,
     326,     0,     0,     0,     0,     0,     0,   594,   247,     0,
     261,     0,     0,     0,   247,     0,  1151,     0,   326,     0,
       0,     0,     0,     0,   594,     0,     0,     0,   594,     0,
       0,     0,     0,     0,  1367,     0,   666,     0,     0,     0,
       0,   666,   247,     0,   666,     0,   326,     0,   618,     0,
     273,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     591,     0,     0,   666,     0,     0,     0,     0,     0,   516,
       0,     0,     0,     0,     0,     0,     0,   108,     0,     0,
       0,     0,     0,     0,   326,     0,     0,   322,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   979,   592,     0,
       0,   259,     0,   326,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   594,     0,     0,     0,     0,
       0,     0,     0,   844,   844,     0,     0,     0,   247,     0,
       0,     0,     0,     0,     0,     0,     0,   844,   844,     0,
       0,   445,   444,   444,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   247,     0,   618,   273,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     844,   844,     0,     0,     0,     0,     0,     0,     0,     0,
    1319,  1319,  1319,   151,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   447,     0,     0,     0,
       0,   247,   326,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   326,
     326,   247,     0,     0,     0,     0,   247,   330,   247,     0,
       0,     0,     0,     0,     0,     0,   261,     0,   113,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   247,   447,
     247,   247,     0,     0,     0,     0,     0,     0,   322,     0,
       0,     0,     0,     0,     0,   594,   447,     0,   247,     0,
       0,     0,   326,     0,     0,     0,     0,     0,     0,     0,
     247,   151,     0,     0,     0,     0,     0,     0,     0,   594,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   247,   594,   618,   273,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     108,     0,     0,     0,     0,   247,   618,     0,     0,     0,
       0,     0,   247,   118,     0,     0,   118,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   108,
       0,     0,     0,   844,   844,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   259,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1649,
       0,     0,     0,     0,     0,     0,     0,     0,   447,   844,
     118,     0,     0,   592,     0,     0,     0,     0,     0,     0,
       0,  1670,     0,  1670,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   118,     0,     0,     0,     0,     0,
     445,     0,     0,     0,     0,     0,  1670,     0,     0,   447,
       0,     0,     0,     0,   118,     0,     0,   322,     0,     0,
     151,     0,     0,     0,     0,     0,   119,     0,   844,   119,
       0,   330,   330,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   326,   326,     0,     0,
     330,   118,     0,     0,     0,     0,     0,   118,   844,   118,
     326,   326,     0,   844,   844,   326,   326,     0,   444,   444,
       0,     0,     0,     0,     0,     0,     0,     0,   330,     0,
       0,     0,     0,   119,  1740,     0,     0,     0,     0,     0,
       0,   118,     0,   326,   326,     0,     0,     0,     0,     0,
       0,     0,     0,   118,     0,     0,     0,   119,     0,   113,
       0,     0,     0,     0,     0,     0,   330,     0,     0,     0,
       0,     0,     0,  1762,     0,     0,     0,   119,     0,     0,
     594,     0,     0,   261,   247,   330,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   247,     0,     0,     0,     0,
       0,     0,     0,     0,   118,     0,     0,   118,     0,     0,
       0,     0,   118,     0,   119,   247,     0,     0,     0,     0,
     119,     0,   119,     0,     0,     0,   247,     0,     0,     0,
       0,   445,     0,   447,     0,   247,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   118,     0,     0,     0,     0,
       0,     0,     0,     0,   119,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   118,     0,   119,     0,     0,     0,
       0,     0,     0,     0,  1670,     0,     0,     0,     0,     0,
       0,  1839,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   330,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   444,     0,     0,     0,
       0,   330,   330,     0,     0,     0,     0,   119,     0,     0,
     119,  1670,     0,     0,     0,   119,   326,   326,     0,     0,
       0,   247,     0,     0,     0,     0,   118,  1670,  1839,     0,
       0,     0,     0,     0,  1526,     0,     0,     0,     0,     0,
       0,     0,     0,  1534,     0,   247,     0,  1538,   119,  1540,
       0,     0,   326,     0,   330,     0,     0,     0,     0,     0,
     118,     0,     0,  1670,   108,     0,   108,   119,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1921,     0,     0,     0,     0,     0,   118,     0,     0,   108,
       0,     0,     0,     0,     0,     0,     0,   844,     0,     0,
     326,     0,   113,     0,     0,     0,     0,     0,     0,     0,
       0,   326,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   113,     0,     0,     0,     0,     0,     0,     0,   119,
       0,   326,     0,     0,     0,     0,   326,   326,     0,   261,
       0,   326,   326,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   118,   118,     0,     0,     0,
       0,     0,     0,   119,     0,   594,     0,     0,     0,     0,
       0,   247,     0,     0,     0,     0,     0,  1626,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   119,
       0,     0,   447,     0,     0,     0,   259,   118,     0,     0,
       0,   118,   247,   118,     0,     0,     0,     0,   247,  1659,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1664,
       0,  1675,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   330,   330,
       0,     0,     0,     0,  1664,     0,     0,     0,     0,     0,
       0,     0,   330,   330,     0,     0,     0,   330,   330,     0,
       0,   123,     0,     0,   123,     0,     0,     0,   119,   119,
       0,     0,     0,     0,     0,   118,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   330,   330,     0,   118,     0,
     118,   118,     0,   118,     0,     0,   118,   108,     0,   118,
     118,   118,     0,     0,   592,     0,     0,     0,     0,     0,
     119,     0,     0,     0,   119,     0,   119,     0,   123,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   326,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   247,
       0,     0,   123,     0,   108,     0,     0,     0,     0,     0,
       0,     0,  1759,     0,     0,     0,  1761,     0,     0,     0,
     108,   592,   123,  1763,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   447,     0,     0,     0,   118,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   247,   119,     0,
       0,     0,     0,     0,     0,     0,   108,     0,     0,   123,
       0,   119,     0,   119,   119,   123,   119,   123,     0,   119,
       0,     0,   119,   119,   119,     0,     0,     0,     0,     0,
       0,     0,   359,     0,     0,     0,   360,     0,   361,     0,
     326,     0,     0,     0,     0,     0,     0,     0,     0,   123,
       0,     0,     0,     0,     0,   362,     0,     0,     0,     0,
       0,   123,     0,     0,     0,     0,     0,     0,     0,  1828,
    1830,     0,  1675,     0,     0,     0,     0,     0,   330,   330,
       0,     0,   363,   364,     0,   365,     0,   366,  1826,    63,
      64,    65,    66,    67,    68,    69,    70,   367,   368,   356,
     119,   369,   370,   371,   118,   372,   373,     0,     0,     0,
       0,     0,   123,    72,   330,   123,   118,   118,     0,     0,
     123,     0,     0,     0,     0,     0,   113,  1876,   113,   386,
       0,     0,     0,   374,     0,     0,    75,   375,     0,     0,
       0,     0,     0,   376,    77,    78,   377,   378,   379,   380,
       0,   113,     0,   123,     0,     0,     0,  1827,  -175,     0,
       0,     0,   330,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   123,   330,     0,     0,     0,     0,     0,  1914,
       0,  1916,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1929,     0,   330,     0,     0,     0,     0,   330,   330,
       0,     0,     0,   330,   330,     0,     0,   119,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   119,
     119,     0,     0,     0,     0,     0,     0,  1957,  1959,  1960,
       0,     0,     0,     0,   123,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   261,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   123,   247,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     1,     0,     0,   145,     0,     0,     0,     0,     0,
     247,     0,     0,     0,   123,     0,     0,     0,     0,     0,
     647,     0,     0,   386,   652,     0,     0,     0,     0,     0,
       0,     0,     0,   655,   656,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   386,   386,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   118,
       0,     0,     0,     0,     0,     0,     0,   118,     0,   386,
       0,     0,     0,     0,     0,     0,     0,   192,     0,   113,
     247,     0,     0,     0,     0,     0,   594,     0,     0,     0,
       0,     0,     0,   123,   123,     0,   118,     0,     0,   386,
       0,     0,   247,     0,     0,     0,     0,     0,     0,     0,
       0,   330,     0,     0,   118,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   113,     0,     0,     0,
       0,     0,     0,   118,     0,   123,   283,     0,     0,   123,
       0,   123,   113,   594,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   118,   119,     0,     0,     0,     0,     0,   113,   247,
     119,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   119,
       0,     0,   330,   123,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   123,   119,   123,   123,
     283,   123,     0,     0,   123,     0,     0,   123,   123,   123,
       0,     0,     0,     0,     0,   519,   119,     0,     0,     0,
       0,     0,     0,     0,     0,   283,     0,     0,     0,     0,
       0,   247,     0,     0,     0,   283,     0,     0,     0,     0,
     118,   118,   118,   118,   118,   118,     0,     0,     0,   550,
     554,     0,     0,     0,   119,     0,   561,   562,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   572,     0,     0,   203,     0,     0,     0,     0,
       0,   214,   215,     0,     0,   123,     0,     0,     0,     0,
     589,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   277,     0,     0,     0,   386,
     386,   386,   386,   386,   386,   386,   386,   386,   386,   386,
     386,   386,   386,   386,   386,   386,   386,   386,     0,     0,
       0,   118,     0,     0,     0,     0,   672,     0,     0,     0,
       0,     0,     0,   119,   119,   119,   119,   119,   119,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   711,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   386,
       0,     0,   123,   749,     0,     0,     0,   752,     0,     0,
       0,     0,     0,     0,   123,   123,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   774,     0,     0,     0,
     775,   776,     0,   118,   779,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   793,
     794,   795,   796,     0,   119,     0,     0,     0,     0,     0,
       0,   118,     0,   118,     0,     0,     0,     0,   818,     0,
       0,     0,     0,     0,     0,     0,   821,     0,     0,     0,
       0,   566,     0,     0,     0,     0,   118,     0,     0,     0,
       0,     0,     0,   165,     0,     0,     0,     0,     0,     0,
     118,     0,     0,     0,   283,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   165,
       0,     0,     0,   118,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   859,     0,     0,     0,     0,
       0,     0,   550,     0,     0,     0,   119,   864,     0,     0,
       0,     0,     0,   386,     0,     0,     0,     0,   386,     0,
       0,     0,     0,     0,     0,   165,     0,     0,     0,   386,
     878,   883,     0,     0,   119,     0,   119,     0,   165,     0,
     165,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   119,
       0,     0,     0,   118,     0,     0,     0,     0,     0,   386,
     350,     0,     0,   119,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   753,   350,   754,     0,
       0,   926,     0,     0,     0,     0,   119,   770,   771,     0,
       0,     0,     0,     0,     0,     0,     0,   123,     0,     0,
       0,     0,     0,     0,     0,   123,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   165,     0,     0,     0,   165,
       0,     0,   165,   165,     0,     0,   165,     0,     0,   165,
     165,     0,     0,     0,   123,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   987,     0,
       0,     0,   123,     0,   118,     0,     0,     0,     0,     0,
       0,     0,     0,  1004,     0,     0,   119,  1005,     0,     0,
       0,   123,     0,     0,     0,     0,   878,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   386,     0,     0,     0,
       0,   165,     0,     0,   165,   854,     0,     0,  1045,     0,
       0,   118,     0,     0,     0,     0,     0,  1054,     0,   123,
       0,     0,     0,  1057,   165,     0,     0,   118,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   165,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       1,     0,     0,   118,     0,     0,     0,     1,     0,   386,
       0,     0,     0,     0,     0,     0,     0,   119,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     1,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   386,   386,   386,   163,     0,     0,     0,
     386,   386,     0,     0,     0,     0,     0,     0,   123,   123,
     123,   123,   123,   123,   119,     0,     0,     0,     0,     0,
       0,     0,   386,   165,     0,     0,     0,     0,     0,     0,
     119,     0,     0,     0,     0,     0,     0,  1205,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   386,
     386,     0,     0,     0,     0,     0,   119,     0,   275,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   350,     0,
       0,   281,     0,   282,     0,     0,     0,     0,     0,     0,
     165,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1030,     0,   123,
       0,     0,     0,     0,     0,     0,     0,     0,  1251,     0,
       0,     0,  1252,     0,     0,     0,     0,     0,     0,   878,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1265,
       0,     0,     0,     0,     0,     0,  1266,     0,     0,     0,
       0,     0,     0,     0,     0,  1270,     0,  1271,     0,     0,
       0,     0,     0,     0,   350,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   501,   502,     0,     0,   506,
       0,     0,   509,   510,  1098,  1099,     0,     0,     0,  1298,
       0,     0,     0,  1299,     0,  1157,  1158,  1159,     0,     0,
    1161,   123,     0,   165,   165,     0,     0,   145,     0,     0,
       1,     0,     0,     0,     0,     0,   165,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   123,
       0,   123,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   123,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   587,   123,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1225,
       0,     0,   619,     0,     0,     0,     0,     0,     0,     0,
       0,   123,     0,     0,     0,     0,     0,  1386,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1409,     0,  1244,     0,     0,     0,   165,   165,     0,
       0,     0,     0,   165,     0,     0,     0,     0,     0,     0,
     386,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   165,     0,     0,   165,   165,     0,
     165,     0,   165,   165,     0,     0,     0,     0,     0,     0,
    1268,   123,     0,     0,     0,     0,   743,     0,     0,  1272,
    1273,  1274,  1275,     0,     0,     0,     0,  1280,  1281,     0,
       0,     0,     0,     0,     0,     0,     0,  1288,     0,     0,
       0,   165,     0,     0,     0,   165,     0,     0,     0,     0,
       0,  1481,     0,     0,     0,  1482,     0,     0,  1302,     0,
    1303,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   814,     0,  1517,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1529,  1531,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   123,  1359,     0,     0,     0,     0,     0,     0,
     165,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1565,     0,     0,  1568,  1373,
       0,     0,     0,     0,     0,  1377,     0,  1379,  1381,     0,
       0,     0,     0,  1576,     0,     0,  1387,     0,  1388,   123,
    1389,     0,  1391,     0,     0,     0,     0,  1399,     0,     0,
       0,     0,     0,   386,     0,   123,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   889,   890,     0,     0,
       0,     0,     0,  1606,     0,     0,     0,   386,     0,   897,
       0,   123,  1611,     0,     0,     0,  1612,     0,     0,     0,
       0,     0,     0,     0,     0,  1441,     0,     0,     0,     0,
    1616,  1617,  1448,  1449,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     165,     0,     0,     0,     0,     0,  1472,     0,     0,     0,
       0,     0,     0,  1477,     0,     0,  1478,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   386,   386,
       0,     0,     0,     0,     0,     0,   214,     0,   165,     0,
       0,   165,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   386,     0,     0,     0,     0,
     993,   994,     0,     0,     0,     0,   998,     0,     0,     0,
    1698,  1699,     0,     0,     0,     0,  1555,     0,     0,     0,
     386,     0,     0,     0,     0,     0,     0,  1019,     0,     0,
    1022,  1023,     0,  1026,     0,  1028,  1029,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1574,
       0,     0,     0,     0,     0,     0,     0,  1579,     0,  1581,
       0,     0,     0,     0,     0,     0,     0,   386,     0,     0,
       0,     0,     0,     0,  1069,     0,     0,     0,  1073,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     165,     0,     0,     0,     0,  1607,  1608,     0,   165,   165,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1613,  1614,     0,  1615,     0,     0,     0,     0,  1779,     0,
       0,     0,  1619,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1624,  1625,     0,     0,     0,     0,     0,     0,
    1568,     0,     0,  1189,     0,     0,     0,   337,     0,     0,
       0,     0,     0,   165,     0,     0,     0,     0,  1804,     0,
       0,     0,   165,     0,     0,   165,     0,   165,   165,     0,
       0,     0,     0,     0,     0,     0,   434,   337,     0,     0,
       0,   193,     0,     0,     0,  1819,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1835,   165,   500,  1836,
       0,     0,     0,     0,     0,   500,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1702,  1703,     0,     0,
       0,     0,     0,     0,     0,     0,  1709,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   193,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   193,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1732,  1733,  1189,   165,     0,     0,     0,   193,     0,
       0,     0,     0,   500,     0,     0,     0,     0,     0,     0,
       0,   441,     0,  1905,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   337,   604,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1291,     0,     0,  1295,     0,   625,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   193,     0,     0,     0,     0,     0,
    1792,     0,     0,     0,     0,     0,   165,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1801,     0,
       0,  1802,  1803,     0,     0,     0,     0,     0,  1805,     0,
       0,     0,     0,     0,     0,   165,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   500,     0,     0,     0,
       0,   193,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   165,   500,   745,     0,   500,   748,   165,     0,   193,
       0,     0,     0,   337,     0,     0,     0,   604,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1393,     0,     0,     0,     0,     0,     0,
       0,  1402,  1403,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   500,     0,
     165,     0,   500,  1874,     0,     0,     0,     0,     0,     0,
    1882,     0,     0,     0,     0,     0,  1886,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   337,     0,  1443,   193,     0,     0,
       0,     0,     0,     0,     0,  1452,     0,     0,  1456,     0,
    1459,  1460,     0,     0,   200,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1918,     0,     0,   193,     0,     0,
     255,     0,     0,     0,   165,   165,     0,     0,     0,     0,
       0,     0,   350,     0,   500,     0,   165,   337,  1935,     0,
    1486,     0,     0,     0,     0,     0,  1940,     0,     0,     0,
       0,     0,     0,   873,   337,     0,     0,     0,     0,     0,
       0,  1953,     0,     0,   604,     0,     0,     0,   604,   200,
       0,     0,     0,   303,     0,   891,     0,   337,     0,     0,
       0,     0,     0,     0,   342,     0,     0,     0,     0,     0,
       0,     0,   193,   193,     0,     0,     0,     0,   441,     0,
       0,   200,     0,     0,     0,     0,     0,  1562,     0,     0,
       0,     0,     0,     0,   454,     0,     0,   458,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   165,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   193,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   200,   441,     0,
       0,     0,     0,     0,     0,     0,   337,     0,     0,     0,
     255,     0,     0,     0,     0,     0,     0,     0,     0,  1456,
       0,   193,   500,   500,   165,     0,     0,     0,     0,     0,
       0,     0,   500,  1000,     0,   500,  1003,     0,     0,     0,
       0,     0,   193,     0,     0,     0,   458,   337,  1621,     0,
     604,     0,   604,   604,   200,     0,     0,     0,     0,   604,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   337,
     337,     0,   597,     0,   614,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   337,     0,
       0,     0,   500,     0,     0,     0,   500,     0,     0,     0,
     500,  1071,     0,   165,   500,  1075,     0,     0,     0,     0,
       0,     0,  1078,     0,     0,     0,     0,     0,     0,     0,
     441,     0,     0,     0,     0,     0,   670,     0,     0,     0,
       0,     0,     0,  1696,     0,     0,     0,     0,   410,     0,
       0,     0,     0,     0,   193,     0,     0,     0,     0,     0,
       0,   439,     0,     0,   337,   500,     0,     0,     0,     0,
     200,   441,     0,     0,   467,     0,   467,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   359,     0,     0,     0,
     360,   604,   361,   441,   441,     0,     0,     0,     0,     0,
     597,     0,     0,     0,     0,     0,   769,     0,     0,   362,
       0,     0,   441,     0,     0,     0,     0,  1744,  1745,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1749,
       0,   337,     0,     0,     0,     0,   363,   364,     0,   461,
       0,   366,    62,    63,    64,    65,    66,    67,    68,    69,
      70,   367,   368,   356,     0,   369,   370,   371,     0,   372,
     373,     0,   567,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,   200,   200,     0,   441,     0,
       0,   454,     0,     0,   193,     0,     0,   374,    74,     0,
     462,   463,     0,     0,     0,   464,   500,   376,    77,    78,
     377,   378,   379,   380,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   604,   604,     0,     0,     0,     0,     0,
     604,  1812,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   342,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   193,     0,     0,     0,     0,
       0,   454,     0,   877,     0,     0,     0,     0,     0,     0,
       0,     0,   337,     0,     0,     0,     0,   500,  1293,     0,
     500,  1297,     0,     0,   597,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1863,     0,     0,
       0,     0,     0,     0,     0,   200,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   670,     0,
     670,   670,     0,   670,     0,     0,   670,     0,     0,   670,
     670,   670,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   467,     0,     0,     0,     0,     0,   467,
       0,     0,     0,     0,   790,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   454,     0,     0,   441,     0,     0,     0,
       0,     0,     0,   337,     0,     0,     0,     0,     0,   604,
    1395,     0,     0,     0,     0,     0,     0,   200,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     337,     0,     0,     0,   454,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   858,     0,     0,     0,     0,   454,   454,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   500,  1445,     0,   454,     0,     0,   439,     0,
       0,   500,  1454,     0,   604,     0,     0,     0,     0,     0,
       0,   885,     0,     0,     0,   337,   337,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     193,     0,     0,     0,     0,     0,     0,   193,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   454,   920,     0,   193,     0,     0,   200,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   769,     0,     0,
     790,   940,     0,     0,     0,     0,     0,     0,     0,     0,
     951,     0,   956,   951,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1187,     0,     0,     0,
       0,     0,     0,    13,    14,    15,    16,    17,     0,     0,
     984,     0,     0,     0,     0,     0,     0,     0,   342,   441,
     441,   337,     0,   986,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   995,     0,     0,     0,     0,   359,
       0,     0,     0,   360,     0,   361,     0,     0,   439,     0,
       0,   984,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    57,   362,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1048,     0,
       0,   467,     0,     0,     0,     0,     0,     0,     0,   363,
     364,     0,   365,     0,   366,    62,    63,    64,    65,    66,
      67,    68,    69,    70,   367,   368,   356,     0,   369,   370,
     371,     0,   372,   373,     0,     0,     0,  1079,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     500,     0,     0,     0,     0,   193,     0,     0,     0,     0,
     374,     0,     0,    75,   375,     0,   500,     0,     0,   454,
     376,   437,    78,   377,   378,   379,   380,     0,     0,     0,
       0,     0,     0,     0,   410,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1179,  1181,     0,     0,     0,     0,
       0,     0,   439,     0,     0,     0,     0,     0,     0,   670,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     337,     0,     0,   467,     0,     0,     0,     0,     0,     0,
       0,     0,   951,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   984,     0,     0,     0,     0,
       0,     0,     0,  1218,     0,     0,     0,     0,     0,     0,
     951,     0,     0,   255,     0,     0,     0,     0,     0,     0,
     193,   337,   337,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   200,     0,     0,   500,   500,     0,     0,
     597,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   500,     0,   467,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   342,     0,     0,
       0,   670,     0,     0,     0,     0,    13,    14,    15,    16,
      17,     0,     0,    19,   193,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
    -405,  -405,     0,  -405,    45,    46,     0,  -405,     0,     0,
       0,   467,     0,  1284,     0,  1286,     0,     0,     0,     0,
       0,     0,   454,   454,    57,   441,   441,     0,     0,     0,
       0,     0,     0,     0,     0,   500,     0,     0,     0,     0,
       0,     0,     0,   500,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    62,    63,
     670,   670,   670,     0,   670,   670,     0,     0,     0,     0,
       0,   458,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1351,  1351,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   337,
       0,     0,     0,   500,  1865,     0,    75,   500,     0,     0,
       0,     0,     0,     0,    77,    78,     0,     0,     0,   255,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   500,     0,     0,     0,     0,   342,     0,
       0,  1390,     0,     0,     0,     0,     0,  1400,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   439,  1397,     0,     0,     0,     0,
       0,     0,    13,    14,    15,    16,    17,     0,     0,     0,
       0,   467,   500,   500,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   951,     0,     0,   790,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   359,     0,
       0,     0,   360,   441,   361,     0,   500,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      57,   362,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1480,     0,
       0,     0,     0,   200,     0,     0,     0,     0,   363,   364,
       0,   365,     0,   366,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   367,   368,   356,     0,   369,   370,   371,
       0,   372,   373,     0,     0,     0,   951,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   467,     0,     0,   790,     0,   374,
       0,     0,    75,   375,     0,     0,     0,   342,     0,   376,
    1398,    78,   377,   378,   379,   380,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   670,     0,   940,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1577,  1578,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   454,   454,
       0,     0,     0,     0,     0,     0,     0,  1311,     0,     0,
     467,     0,   790,  1312,     0,     0,    13,    14,    15,    16,
      17,    18,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
       0,     0,   255,     0,    45,    46,     0,     0,     0,    47,
      48,    49,    50,    51,    52,    53,    54,     0,     0,     0,
       0,     0,     0,     0,    57,  1313,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     410,     0,     0,     0,     0,  1648,     0,     0,     0,     0,
       0,     0,     0,     0,    60,     0,     0,     0,    62,    63,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,  1688,
       0,     0,     0,     0,   255,     0,     0,     0,     0,     0,
       0,     0,  1314,     0,     0,     0,    75,   916,     0,     0,
       0,   670,     0,     0,    77,    78,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1710,     0,
       0,  1712,     0,     0,     0,     0,   454,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   670,     0,
       0,   458,     0,     4,   243,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,  1103,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,   359,     0,
      45,    46,   360,     0,   361,    47,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,    56,     0,  1104,
      57,  1105,    -2,     0,  1106,     0,     0,  1107,  1108,  1109,
    1110,  1111,  1112,  1113,  1114,  1115,  1116,  1117,  1118,  -284,
    1119,  1120,  1121,  1122,  1123,     0,  1124,     0,   363,   364,
      60,   461,     0,   366,  1125,  1126,    64,    65,    66,    67,
      68,    69,    70,   367,   368,   356,  1127,   369,   370,   371,
       0,   372,   373,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    -3,   374,
       0,     0,    75,   406,     0,     0,   951,   279,     0,   376,
      77,    78,   377,   378,   379,   380,     0,     0,     0,     0,
       0,     0,     0,     0,  -175,     4,   243,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,  1103,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,     0,     0,
     359,     0,    45,    46,   360,     0,   361,    47,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,    56,
       0,  1104,    57,  1105,    -2,     0,  1106,     0,     0,  1107,
    1108,  1109,  1110,  1111,  1112,  1113,  1114,  1115,  1116,  1117,
    1118,  -284,  1119,  1120,  1121,  1122,  1123,     0,  1124,     0,
     363,   364,    60,   461,     0,   366,  1125,  1126,    64,    65,
      66,    67,    68,    69,    70,   367,   368,   356,  1127,   369,
     370,   371,     0,   372,   373,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   374,     0,     0,    75,   406,     0,     0,     0,   279,
       0,   376,    77,    78,   377,   378,   379,   380,     0,     0,
       0,     0,     0,     0,     0,     0,  -175,     4,   243,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
       0,     0,   359,     0,    45,    46,   360,     0,   361,    47,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,    56,     0,     0,    57,   362,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   363,   364,    60,   365,     0,   366,    62,    63,
      64,    65,    66,    67,    68,    69,    70,   367,   368,   356,
       0,   369,   370,   371,     0,   372,   373,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   374,     0,     0,    75,   406,     0,     0,
       0,     0,     0,   376,    77,    78,   377,   378,   379,   380,
       0,     0,     0,     0,     0,     0,     0,  1673,  1674,     4,
     243,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,   359,     0,    45,    46,   360,     0,
     361,    47,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,    56,     0,     0,    57,   362,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   363,   364,    60,   365,     0,   366,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   367,
     368,   356,     0,   369,   370,   371,     0,   372,   373,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   374,     0,     0,    75,   406,
       0,     0,     0,     0,     0,   376,    77,    78,   377,   378,
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
      75,   436,     0,     0,     0,     0,     0,   376,   437,    78,
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
       0,     0,    75,  1176,     0,     0,     0,     0,     0,   376,
    1177,    78,   377,   378,   379,   380,   243,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
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
       0,   374,     0,     0,    75,   375,     0,     0,     0,     0,
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
       0,     0,     0,   376,    77,    78,   377,   378,   379,   380,
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
      76,     0,     0,     0,     0,     0,     0,    77,    78,   242,
     243,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,     0,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,  -405,  -405,     0,  -405,    45,    46,     0,  -405,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      13,    14,    15,    16,    17,     0,    57,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,     0,    61,    45,    46,
      62,    63,    64,    65,    66,    67,    68,    69,    70,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    57,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    73,    74,     0,    75,   244,
       0,     0,     0,  -712,     0,     0,    77,    78,     4,   243,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,     0,     0,     0,     0,    45,    46,     0,     0,     0,
      47,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,    56,     0,     0,    57,     0,     0,     0,     0,
    -337,  -337,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    60,     0,     0,     0,    62,
      63,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -337,     0,     0,     0,    75,    76,     0,
       0,     0,     0,     0,     0,    77,    78,     4,   243,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
       0,     0,     0,     0,    45,    46,     0,     0,     0,    47,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,    56,     0,     0,    57,     0,     0,     0,     0,  -338,
    -338,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    60,     0,     0,     0,    62,    63,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -338,     0,     0,     0,    75,    76,     0,     0,
       0,     0,     0,     0,    77,    78,   242,   243,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
       0,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,  -405,
    -405,     0,  -405,    45,    46,     0,  -405,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    57,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    61,     0,     0,    62,    63,    64,
      65,    66,    67,    68,    69,    70,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    73,    74,     0,    75,   244,     0,     0,  1311,
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
       0,     0,     0,     0,  1492,     0,     0,     0,    75,   916,
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
       0,     0,     0,     0,     0,     0,     0,  1493,     0,     0,
       0,    75,   916,     0,     0,  1311,     0,     0,     0,    77,
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
    1494,     0,     0,     0,    75,   916,     0,     0,     0,     0,
       0,     0,    77,    78,   242,   243,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,     0,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,  -405,  -405,     0,
    -405,    45,    46,     0,  -405,     0,     0,     0,     0,     0,
       0,     0,     0,    13,    14,    15,    16,    17,     0,     0,
      19,    57,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,  -404,  -404,     0,
    -404,    45,    46,     0,  -404,    62,    63,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    57,     0,     0,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   242,     0,     0,
       0,     0,     0,    75,   244,     0,    13,    14,    15,    16,
      17,    77,    78,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
    -405,  -405,     0,  -405,    45,    46,     0,  -405,     0,     0,
       0,     0,     0,     0,     0,     0,    13,    14,    15,    16,
      17,     0,     0,    19,    57,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
    -405,  -405,     0,  -405,    45,    46,     0,  -405,    62,    63,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    57,     0,     0,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    75,   301,     0,     0,
       0,     0,     0,     0,    77,    78,   243,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,     0,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,  -405,  -405,
       0,  -405,    45,    46,     0,  -405,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    57,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    61,     0,     0,    62,    63,    64,    65,
      66,    67,    68,    69,    70,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    73,    74,     0,    75,   244,     0,     0,     0,  -716,
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
    1335,     0,     0,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    73,
      74,   359,    75,   244,     0,   360,     0,   361,     0,     0,
      77,    78,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1104,     0,   362,     0,     0,  1106,  1752,  1753,
    1107,  1108,  1109,  1110,  1111,  1112,  1113,  1114,  1115,  1116,
    1117,  1118,  -284,  1119,  1120,  1121,  1122,  1123,     0,  1124,
       0,   363,   364,     0,   461,     0,   366,  1125,  1126,    64,
      65,    66,    67,    68,    69,    70,   367,   368,   356,  1127,
     369,   370,   371,     0,   372,   373,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,  1335,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   374,     0,     0,    75,   375,     0,     0,     0,
     279,     0,   376,    77,    78,   377,   378,   379,   380,   359,
       0,     0,     0,   360,     0,   361,     0,  -175,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1104,     0,   362,    -2,     0,  1106,     0,     0,  1107,  1108,
    1109,  1110,  1111,  1112,  1113,  1114,  1115,  1116,  1117,  1118,
    -284,  1119,  1120,  1121,  1122,  1123,     0,  1124,     0,   363,
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
    1111,  1112,  1113,  1114,  1115,  1116,  1117,  1118,  -284,  1119,
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
       0,  -586,    75,   319,     0,     0,    62,    63,     0,     0,
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
      39,    40,    41,    42,    43,    44,  -405,  -405,     0,  -405,
      45,    46,     0,  -405,     0,     0,     0,     0,     0,     0,
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
       0,  -714,     0,     0,    77,    78,    13,    14,    15,    16,
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
      42,    43,    44,  -405,  -405,     0,  -405,    45,    46,     0,
    -405,     0,     0,     0,     0,     0,     0,     0,     0,     0,
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
      44,  -405,  -405,     0,  -405,    45,    46,     0,  -405,     0,
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
    -405,  -405,     0,  -405,    45,    46,     0,  -405,     0,     0,
     243,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    57,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,     0,     0,    45,    46,    62,    63,
       0,   318,    48,    49,    50,    51,    52,    53,    54,     0,
       0,     0,     0,     0,     0,     0,    57,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    75,   301,     0,     0,
      62,    63,     0,     0,    77,    78,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   843,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -599,    75,   243,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,     0,     0,     0,     0,    45,    46,     0,     0,     0,
     318,    48,    49,    50,    51,    52,    53,    54,     0,     0,
       0,     0,     0,     0,     0,    57,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    62,
      63,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1656,     0,     0,     0,
       0,   243,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,     0,    19,    75,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,     0,     0,     0,     0,    45,    46,     0,
       0,     0,   318,    48,    49,    50,    51,    52,    53,    54,
       0,     0,     0,     0,     0,     0,     0,    57,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   243,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    62,    63,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
    -405,  -405,     0,  -405,    45,    46,     0,  -405,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    75,
       0,     0,     0,     0,    57,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    62,    63,
       0,     0,     0,     0,    13,    14,    15,    16,    17,    18,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,     0,     0,
       0,     0,    45,    46,     0,     0,    75,    47,    48,    49,
      50,    51,    52,    53,    54,     0,     0,     0,     0,     0,
       0,     0,    57,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    61,     0,     0,    62,    63,    64,    65,
      66,    67,    68,    69,    70,     0,     0,     0,    13,    14,
      15,    16,    17,     0,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,  -405,  -405,    75,  -405,    45,    46,     0,  -405,
       0,     0,   359,     0,     0,     0,   360,     0,   361,     0,
       0,     0,     0,     0,     0,     0,    57,     0,     0,     0,
       0,     0,     0,     0,     0,   362,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      62,    63,   363,   364,     0,   365,     0,   366,    62,    63,
      64,    65,    66,    67,    68,    69,    70,   367,   368,   356,
       0,   369,   370,   371,   359,   372,   373,     0,   360,     0,
     361,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   362,    75,     0,
       0,     0,     0,   374,  1221,     0,    75,   375,     0,     0,
       0,  1222,     0,   376,    77,    78,   377,   378,   379,   380,
       0,     0,     0,     0,   363,   364,     0,   365,     0,   366,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   367,
     368,   356,     0,   369,   370,   371,   359,   372,   373,     0,
     360,     0,   361,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   362,
       0,     0,     0,     0,     0,   374,   947,  1519,    75,   375,
       0,     0,     0,     0,     0,   376,    77,    78,   377,   378,
     379,   380,     0,     0,     0,     0,   363,   364,     0,   365,
       0,   366,    62,    63,    64,    65,    66,    67,    68,    69,
      70,   367,   368,   356,     0,   369,   370,   371,   359,   372,
     373,     0,   360,     0,   361,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   362,     0,     0,     0,     0,     0,   374,     0,     0,
      75,   375,     0,     0,     0,   464,     0,   376,    77,    78,
     377,   378,   379,   380,     0,     0,     0,     0,   363,   364,
       0,   365,     0,   366,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   367,   368,   356,     0,   369,   370,   371,
     359,   372,   373,     0,   360,     0,   361,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   362,     0,     0,     0,     0,     0,   374,
     789,     0,    75,   375,     0,     0,     0,     0,     0,   376,
      77,    78,   377,   378,   379,   380,     0,     0,     0,     0,
     363,   364,     0,   365,     0,   366,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   367,   368,   356,     0,   369,
     370,   371,   359,   372,   373,     0,   360,     0,   361,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   362,     0,     0,     0,     0,
       0,   374,     0,     0,    75,   375,     0,     0,     0,   279,
       0,   376,    77,    78,   377,   378,   379,   380,     0,     0,
       0,     0,   363,   364,     0,   365,     0,   366,    62,    63,
      64,    65,    66,    67,    68,    69,    70,   367,   368,   356,
       0,   369,   370,   371,   359,   372,   373,     0,   360,     0,
     361,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   362,     0,     0,
       0,     0,     0,   374,   947,     0,    75,   375,     0,     0,
       0,     0,     0,   376,    77,    78,   377,   378,   379,   380,
       0,     0,     0,     0,   363,   364,     0,   365,     0,   366,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   367,
     368,   356,     0,   369,   370,   371,   359,   372,   373,     0,
     360,     0,   361,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   362,
       0,     0,     0,     0,     0,   374,     0,     0,    75,   375,
       0,     0,   978,     0,     0,   376,    77,    78,   377,   378,
     379,   380,     0,     0,     0,     0,   363,   364,     0,   365,
       0,   366,    62,    63,    64,    65,    66,    67,    68,    69,
      70,   367,   368,   356,     0,   369,   370,   371,   359,   372,
     373,     0,   360,     0,   361,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   362,     0,     0,     0,     0,     0,   374,  1285,     0,
      75,   375,     0,     0,     0,     0,     0,   376,    77,    78,
     377,   378,   379,   380,     0,     0,     0,     0,   363,   364,
       0,   365,     0,   366,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   367,   368,   356,     0,   369,   370,   371,
     359,   372,   373,     0,   360,     0,   361,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   362,     0,     0,     0,     0,     0,   374,
       0,     0,    75,   375,     0,     0,     0,  1345,     0,   376,
      77,    78,   377,   378,   379,   380,     0,     0,     0,     0,
     363,   364,     0,   365,     0,   366,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   367,   368,   356,     0,   369,
     370,   371,   359,   372,   373,     0,   360,     0,   361,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   362,     0,     0,     0,     0,
       0,   374,     0,  1758,    75,   375,     0,     0,     0,     0,
       0,   376,    77,    78,   377,   378,   379,   380,     0,     0,
       0,     0,   363,   364,     0,   365,     0,   366,    62,    63,
      64,    65,    66,    67,    68,    69,    70,   367,   368,   356,
       0,   369,   370,   371,   359,   372,   373,     0,   360,     0,
     361,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   362,     0,     0,
       0,     0,     0,   374,  1958,     0,    75,   375,     0,     0,
       0,     0,     0,   376,    77,    78,   377,   378,   379,   380,
       0,     0,     0,     0,   363,   364,     0,   365,     0,   366,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   367,
     368,   356,     0,   369,   370,   371,   359,   372,   373,     0,
     360,     0,   361,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   362,
       0,     0,     0,     0,     0,   374,     0,     0,    75,   375,
       0,     0,     0,     0,     0,   376,    77,    78,   377,   378,
     379,   380,     0,     0,     0,     0,   363,   364,     0,   365,
       0,   366,    62,    63,    64,    65,    66,    67,    68,    69,
      70,   367,   368,   356,     0,   369,   370,   371,   359,   372,
     373,     0,   360,     0,   361,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   362,     0,     0,     0,     0,     0,   646,     0,     0,
      75,   375,     0,     0,     0,     0,     0,   376,    77,    78,
     377,   378,   379,   380,     0,     0,     0,     0,   363,   364,
       0,   365,     0,   366,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   367,   368,   356,     0,   369,   370,   371,
     359,   372,   373,     0,   360,     0,   361,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   362,     0,     0,     0,     0,     0,   651,
       0,     0,    75,   375,     0,     0,     0,     0,     0,   376,
      77,    78,   377,   378,   379,   380,     0,     0,     0,     0,
     363,   364,     0,   365,     0,   366,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   367,   368,   356,     0,   369,
     370,   371,   359,   372,   373,     0,   360,     0,   361,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   362,     0,     0,     0,     0,
       0,   654,     0,     0,    75,   375,     0,     0,     0,     0,
       0,   376,    77,    78,   377,   378,   379,   380,     0,     0,
       0,     0,   363,   364,     0,   365,     0,   366,    62,    63,
      64,    65,    66,    67,    68,    69,    70,   367,   368,   356,
       0,   369,   370,   371,   359,   372,   373,     0,   360,     0,
     361,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   362,     0,     0,
       0,     0,     0,   374,     0,     0,    75,   375,     0,     0,
       0,     0,     0,   376,   857,    78,   377,   378,   379,   380,
       0,     0,     0,     0,   363,   364,     0,   365,     0,   366,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   367,
     368,   356,     0,   369,   370,   371,     0,   372,   373,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   374,     0,     0,    75,   375,
       0,     0,     0,     0,     0,   376,   437,    78,   377,   378,
     379,   380
};

static const yytype_int16 yycheck[] =
{
       1,    73,     1,     4,   177,    73,   283,   241,   219,   256,
     162,   173,   162,   684,    73,   631,   150,    73,   374,   865,
      82,   178,     1,     1,   612,   513,   514,   671,   599,     4,
       1,   758,  1113,   163,   603,   628,   222,   764,   852,   220,
     162,     1,   322,   407,  1195,   464,   757,  1270,  1271,   605,
     852,    73,    75,   207,    55,    56,     1,    58,  1756,    58,
     139,   664,   953,  1631,   572,    73,   599,   757,    95,    73,
     534,   728,    73,  1631,   599,   178,    82,    70,   115,    58,
      58,    82,  1694,    82,   295,   147,   755,    58,   130,    90,
       0,   982,  1631,   219,    95,   219,    95,    98,    58,    98,
      96,   102,   236,   102,   290,   291,   120,   320,   775,   181,
     574,     1,   191,    58,     4,   296,  1752,   149,   755,    95,
      98,   755,   181,   755,   337,   181,   793,   102,   341,   149,
     172,    87,     0,   153,   115,   836,   337,   219,   152,   140,
     341,   173,   143,   149,   145,   633,   145,   219,    70,   150,
    1097,   219,   219,   149,  1045,   156,   149,   102,   219,   181,
     219,   862,   163,   219,   444,   219,   145,   145,    58,   295,
     320,   295,   153,   181,   145,  1228,    58,   181,  1092,   180,
     181,   180,    87,   245,   755,   145,     1,   780,   757,   229,
       0,  1632,   859,   220,   195,   315,   195,   219,   174,     1,
     145,   157,     4,   759,   205,     1,   131,   763,    98,   210,
    1908,   219,   102,   295,   254,   219,   772,   773,   219,   220,
    1097,   220,   755,   295,   264,   438,     1,   295,   295,     4,
     755,   153,  1868,   353,   295,   236,   295,   149,  1936,   295,
     302,   295,   913,    58,   245,   374,   245,   735,  1162,   496,
     155,   274,   157,  1404,   255,   145,    58,   258,   131,   258,
    1097,    73,    58,   145,   265,   629,   774,   775,  1529,   296,
     941,   482,   131,   154,   275,   276,    88,   278,    75,    76,
     258,   157,  1009,    58,   561,   793,   211,   149,   438,   149,
     906,  1105,   165,  1554,   295,   296,   156,   296,   174,  1740,
     102,   151,   303,  1105,  1015,   155,   165,    82,   309,   310,
     491,   591,   442,   314,   907,    87,   470,   886,  1930,  1887,
    1134,   774,   775,   283,   103,  1015,   682,   930,   107,  1887,
     145,   110,  1134,   112,   991,   615,    59,    60,   283,  1008,
     793,    87,   622,   145,   322,   346,   408,    19,  1887,   145,
     351,   859,     4,   354,   355,   434,   482,   568,   482,   284,
     157,  1308,  1309,  1310,   139,   149,    75,    76,   258,   833,
     145,  1008,   147,   157,  1008,   588,  1008,   511,  1431,  1432,
    1433,   149,   155,   517,   131,   157,   155,   588,  1089,   570,
    1057,  1305,    56,  1616,  1617,    59,    60,  1838,    62,   612,
     482,   174,   151,    55,    56,   174,   859,   156,   104,   105,
     482,   157,   625,   160,   161,   482,   191,   418,   926,   418,
      70,   482,   572,   482,   625,   265,   482,   351,   482,    82,
     354,  1308,  1309,  1310,   157,   155,   276,  1008,    90,   462,
     441,   442,   568,   149,   568,   165,  1015,  1888,   157,   374,
     572,   174,   453,   454,    70,    70,   152,  1184,   155,   149,
     482,   462,   612,   464,   491,   157,   444,   242,   283,   248,
     245,  1308,  1309,  1310,   482,  1008,  1032,   174,   482,   149,
    1921,   482,   174,  1008,  1870,   149,   568,   283,   140,    70,
     491,   143,   491,   533,   147,   150,   568,   174,  1884,   149,
     174,   568,   157,   153,   156,    70,    70,   568,   283,   568,
     511,   163,   568,   146,   568,   155,   517,   646,     3,   648,
     649,   155,   651,   152,  1910,   654,   179,   302,   657,   658,
     659,   165,  1259,   149,   149,  1426,   709,   153,   153,   149,
     173,    73,    70,   570,     3,   324,   325,   157,   327,  1057,
     329,   155,    70,    70,   555,    70,   557,    89,   210,    12,
      13,    14,    15,    16,   844,    70,   985,   568,   149,   570,
     174,   570,   153,   155,   106,  1191,   149,   155,   503,   155,
     558,  1804,   583,   508,   149,   149,   587,    70,   153,   153,
    1178,   157,   245,  1674,  1238,   875,   174,  1678,   174,   157,
     525,   561,   173,   255,  1057,  1198,   228,    70,   148,   231,
     535,   768,    70,   265,   559,   155,   561,    70,   619,   149,
     155,   149,     1,   275,   276,   153,   278,   757,   782,   589,
     631,   253,   149,   408,   149,    70,   153,   615,   153,   174,
     148,   263,   815,   149,   149,    70,   149,   155,   153,   302,
     153,   303,  1245,   149,   155,  1501,  1327,   309,   310,   434,
     873,   795,   314,   151,    70,   768,   149,   320,   125,   126,
     153,   146,   873,   174,   675,   676,   779,   678,   131,    58,
     814,   682,   157,  1171,   685,   151,   149,   710,  1769,   155,
     153,   149,   152,   153,   346,   153,   156,  1205,   173,   351,
     171,   155,   354,   155,  1348,   146,   115,   129,  1599,   710,
    1601,   152,   169,   170,   149,  1529,    95,  1531,   153,    98,
     174,   646,   174,   102,   149,   151,   651,   149,   153,   654,
     156,   153,   173,    12,    13,    14,    15,    16,   160,   161,
    1554,     9,   149,   149,  1825,   149,   561,   153,   673,   153,
     904,   530,  1833,   151,   755,   408,   757,   151,   156,   151,
       3,   155,  1270,   151,   156,   561,   145,   897,   769,    12,
      13,    14,    15,    16,   151,   776,   926,   151,   155,  1423,
     555,   782,   151,   151,   785,   438,   561,   155,   149,   441,
     151,    70,   752,   794,   795,   796,   151,  1044,   151,  1880,
     155,   453,   454,  1222,   926,   689,   690,   691,  1401,   162,
     163,    46,    47,   814,    49,   155,   195,   157,    53,     4,
       5,     6,     7,     8,     9,    10,    11,    70,   149,  1422,
     151,  1195,   153,   101,   151,   192,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   151,   160,   153,   151,   850,
     851,   852,   131,   167,   168,  1094,  1095,  1096,     3,    56,
     513,   514,    59,    60,   149,    62,   844,    12,    13,    14,
      15,    16,   155,   852,   852,   978,   151,   855,  1386,   258,
     155,   852,   504,    21,  1014,  1015,   149,    76,   149,   864,
    1076,   155,   852,   104,   105,   820,   897,   875,  1052,   155,
     901,   155,  1182,   555,   526,   906,   831,   852,    96,   834,
     532,   912,   101,   838,   536,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   151,    70,  1345,   155,   155,   151,
     821,   583,   149,   155,   151,   587,   153,    12,    13,    14,
      15,    16,   149,  1536,   945,   192,    12,    13,    14,    15,
      16,  1054,   953,  1187,   104,   105,  1312,   149,  1772,   612,
     151,   153,   852,   149,   155,  1178,   151,   619,  1584,     9,
     143,   144,   145,   149,   864,   151,   157,   153,    87,   631,
     633,   982,   155,   157,   985,   174,   151,   878,   129,   174,
     155,   148,   165,   149,   149,    70,   151,   153,   153,   151,
    1593,   174,  1421,   155,    70,  1819,   157,  1008,   149,   151,
    1172,  1173,   153,  1014,  1015,  1492,  1493,  1494,   151,   160,
     161,  1835,   155,   675,   676,   151,   678,   154,  1178,   155,
     682,  1165,   173,   685,   151,  1097,   157,   852,   155,   418,
    1404,   151,   152,   157,  1045,   108,   109,   110,   111,   112,
     852,   162,   163,  1300,   129,  1205,   852,  1871,   710,   151,
     157,   101,   864,   155,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   320,   149,   151,   323,   852,   153,   155,
    1209,   151,   735,  1205,   151,   160,   161,   173,   155,   864,
     337,  1707,   149,   151,   341,   149,   987,   155,     4,     5,
       6,     7,     8,     9,    10,    11,  1107,   115,  1616,  1110,
    1111,  1112,   491,  1004,  1005,  1708,  1787,   769,   149,  1271,
     155,  1278,  1279,   166,   776,   151,  1105,  1105,   151,   155,
     158,   151,   155,  1134,  1105,   155,   151,  1318,  1241,  1140,
     155,   158,   794,    17,   796,  1105,   159,  1148,   123,   124,
    1151,  1152,  1151,  1152,  1155,  1134,  1134,    63,   127,   128,
    1105,   151,   519,  1134,  1165,   155,  1091,    12,    13,    14,
      15,    16,   161,  1151,  1134,  1278,  1279,  1152,   171,  1104,
     129,    55,    56,    57,    58,    59,    60,    61,    62,  1134,
    1191,   438,   151,   151,   154,   155,  1121,   155,   850,   851,
     852,   152,  1795,   151,  1182,  1206,  1892,  1152,   151,   129,
    1896,   151,   865,   835,   151,  1105,   696,   697,   698,   699,
     151,  1222,   154,   155,   153,    70,   154,  1228,   131,   149,
     160,   161,   589,   153,   156,   118,  1386,   120,   121,   122,
     160,   161,   154,   155,  1134,   897,  1308,  1309,  1310,   901,
    1312,  1313,  1529,   131,   906,   156,   129,   154,   155,  1260,
     156,  1151,  1152,   155,  1386,   149,   149,   154,   155,   152,
     153,   151,   519,   154,   157,   158,   149,  1554,   154,   155,
     153,   149,   154,   155,   129,   155,   156,   160,   161,   153,
    1105,  1318,   154,   155,  1505,     4,     5,     6,     7,     8,
       9,    10,    11,  1105,   149,   154,   155,   157,   153,  1105,
     154,   155,   154,   155,   151,   160,   161,  1318,  1499,  1134,
     151,  1322,  1097,   151,  1325,   572,  1507,   143,   144,   145,
    1105,   151,  1134,   154,   155,   157,  1311,   151,  1134,   155,
     157,   588,   589,    68,  1345,   154,   155,   154,   155,   165,
    1152,   154,   155,   157,   711,   154,   155,   154,   174,  1134,
    1251,  1252,   154,   155,  1365,   612,  1367,   149,  1367,   154,
     155,   154,   155,    76,  1265,  1266,   154,   101,   625,  1505,
    1553,  1505,   106,   107,   108,   109,   110,   111,   112,  1367,
      17,   101,   155,   156,   173,   752,   106,   107,   108,   109,
     110,   111,   112,  1328,  1329,   154,   155,  1298,  1299,   155,
     154,   155,   149,  1565,   157,  1565,   174,   774,   775,   151,
    1421,  1311,   151,  1505,   157,  1426,   150,   155,   156,   153,
    1431,  1432,  1433,   174,  1409,  1646,   793,  1505,  1505,   149,
     150,  1366,   157,  1565,  1505,    12,    13,    14,    15,    16,
      17,  1505,    75,    76,   154,  1107,   155,   156,  1110,  1111,
    1112,   154,  1591,    17,   711,  1617,  1229,  1230,  1649,   692,
     693,   148,  1499,   694,   695,   700,   701,  1367,    98,   151,
    1507,   151,  1134,  1512,  1513,  1172,  1173,   151,  1140,   109,
      12,    13,    14,    15,    16,    17,  1148,   151,  1499,   151,
     151,   151,   859,  1155,  1505,   752,  1507,   151,   148,  1311,
     157,   157,   157,  1514,   151,    68,  1689,   174,  1171,  1409,
    1646,   151,  1646,   151,   173,  1178,   151,  1528,   157,  1530,
     148,   151,   151,  1308,  1309,  1310,  1311,  1312,  1313,  1191,
      12,    13,    14,    15,    16,    17,   151,   155,   151,   151,
     151,  1529,   151,  1531,  1206,   155,   151,   151,   151,  1839,
     151,  1562,   151,   151,  1646,  1699,  1779,   151,  1749,  1529,
     151,  1531,   151,   151,   151,   195,  1554,   151,   154,  1646,
     151,   151,   148,  1584,  1529,  1646,  1531,   173,   151,   148,
    1481,  1482,  1646,  1568,  1554,  1752,   151,   155,  1599,   149,
    1601,    12,    13,    14,    15,    16,    17,  1409,   149,  1554,
    1783,   149,   149,   149,   149,    13,   156,   362,    72,    89,
    1545,   155,  1649,  1836,   156,   174,   873,   174,   154,   876,
     154,   174,   174,   148,  1409,   148,   157,   155,   258,  1529,
     151,  1531,   387,   388,   174,  1646,   154,   151,  1649,  1752,
     155,   151,  1804,   155,   155,   154,   151,   151,   148,  1660,
     148,    78,   149,   408,  1554,   174,   149,   174,   288,   151,
    1322,   151,   149,  1325,   294,   148,  1887,   174,  1568,   926,
     149,  1682,   174,   174,   151,  1576,   148,   148,   174,   174,
     174,   155,  1670,   438,   154,   174,  1671,  1698,  1699,   155,
    1057,   154,   322,   154,  1838,     1,  1707,  1888,     4,   151,
    1867,  1868,   157,  1365,  1529,  1606,  1531,  1779,   154,  1892,
    1611,  1612,  1749,  1896,  1897,   148,  1671,  1529,   156,  1531,
     151,   156,   118,  1529,   148,  1531,   101,  1894,   151,  1554,
    1921,   106,   107,   108,   109,   110,   111,   112,  1749,   151,
    1923,   151,  1554,   154,  1529,  1756,  1531,   151,  1554,  1760,
     151,  1887,    58,  1887,  1867,  1868,  1568,   154,   148,   174,
    1771,  1944,  1151,  1152,  1836,  1948,   155,    73,   156,  1554,
    1670,  1671,   151,  1784,  1762,   149,    82,   149,   149,   107,
     157,  1894,   148,  1568,  1772,   154,   154,   154,   148,    95,
    1973,   154,    98,   151,   151,  1887,   102,   151,   151,  1966,
     151,   151,  1772,  1107,   151,  1887,  1919,   155,   148,  1887,
    1887,   149,   174,    88,   444,   151,  1887,  1772,  1887,   151,
     154,  1887,   148,  1887,   154,   148,   151,  1838,   174,  1840,
     151,  1819,   151,   139,   151,   151,   151,   174,  1501,   145,
    1851,   147,  1853,  1854,   150,   151,   153,  1835,   156,  1819,
     174,  1888,  1514,  1966,   174,   156,   162,   151,   148,  1671,
     174,   174,  1762,   151,  1819,  1835,  1528,   151,  1530,   151,
     155,   152,  1772,   179,   180,   181,  1887,  1888,   174,  1888,
    1835,   101,   156,  1871,  1921,   191,   192,  1898,   518,   195,
     149,   155,    73,   149,   148,   150,   174,  1908,   174,   150,
    1562,  1871,   107,  1270,  1271,   154,   107,   165,   538,   151,
    1921,   156,  1921,   219,   220,   165,  1871,   151,   148,  1819,
     148,  1178,  1584,  1934,   151,  1936,   149,   174,   558,    73,
     236,   174,   151,   688,   151,  1835,   174,   376,  1591,   245,
    1951,  1327,   663,  1235,   702,  1956,   703,  1772,  1205,  1123,
    1554,  1868,   258,   205,   704,  1134,   705,  1936,   706,  1970,
    1772,   591,  1772,  1974,  1681,  1884,  1772,  1828,  1931,  1788,
    1930,  1871,  1918,  1984,  1546,  1546,  1897,  1836,  1367,  1948,
    1835,    12,   288,  1155,    48,   615,  1749,  1772,   294,   295,
     296,  1499,   622,   250,  1819,    73,   302,  1812,  1660,  1313,
     782,  1513,  1148,   471,  1905,   868,  1409,  1819,  1943,   583,
    1835,  1568,     0,  1819,   320,   321,   322,    95,    -1,   727,
    1682,   727,   727,  1835,    -1,   912,    -1,  1962,    -1,  1835,
      -1,   337,    -1,    -1,  1819,   341,  1698,    -1,    12,    13,
      14,    15,    16,    -1,    -1,  1707,  1871,   230,    -1,    -1,
    1835,    -1,    -1,    -1,    -1,    86,    -1,    -1,    -1,  1871,
      -1,  1365,    -1,    -1,    -1,  1871,    -1,    -1,   374,    -1,
     101,    -1,   150,   104,   105,   106,   107,   108,   109,   110,
     111,   112,    -1,    -1,    -1,    -1,  1871,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1756,     3,    70,    -1,  1760,    -1,
      -1,    -1,   408,    -1,    -1,   411,    -1,    -1,    -1,  1771,
     101,    -1,   418,   104,   105,   106,   107,   108,   109,   110,
     111,   112,  1784,    -1,    -1,    -1,    -1,    -1,   434,  1386,
      -1,    -1,   438,    -1,    -1,    -1,   442,    -1,   444,    -1,
      -1,   219,   220,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1517,    -1,    -1,    -1,   101,   129,    -1,    -1,   236,   106,
     107,   108,   109,   110,   111,   112,   113,   922,    -1,   160,
      -1,    -1,   927,    -1,    -1,   149,   482,    -1,  1840,   153,
      -1,    -1,    -1,   938,    -1,   491,   160,   161,    -1,  1851,
      -1,  1853,  1854,   101,    -1,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   511,   153,   513,   514,    -1,
     462,   517,   464,   519,   844,    -1,    -1,   295,   296,    -1,
      -1,   129,    -1,    -1,  1528,   855,  1530,    -1,   411,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1898,    -1,    -1,    -1,
      -1,   149,   150,    -1,   427,   875,  1908,   430,   156,  1616,
    1617,   557,   160,   161,    -1,    -1,    -1,    -1,  1562,    -1,
      -1,    -1,   568,    -1,   570,    -1,   572,    -1,    -1,    -1,
      -1,    -1,  1934,    -1,  1936,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   588,   589,    -1,   591,    -1,    -1,    -1,  1951,
      17,    -1,    -1,   599,  1956,    -1,    -1,   603,    -1,    -1,
      -1,    -1,    -1,    -1,   487,    -1,   612,    -1,  1970,    -1,
      -1,    -1,  1974,    -1,    -1,    -1,   622,    -1,    -1,   625,
      -1,    -1,  1984,    -1,    -1,    -1,    -1,   633,    -1,    70,
      -1,    -1,    59,    60,    61,    62,    -1,    -1,    -1,    -1,
     646,    -1,   648,   649,    -1,   651,    -1,    -1,   654,    -1,
    1105,   657,   658,   659,    -1,    -1,  1660,    -1,    -1,    -1,
     101,    70,    -1,    -1,   442,   106,   107,   108,   109,   110,
     111,   112,    -1,    -1,   101,   550,    -1,    -1,  1682,   106,
     107,   108,   109,   110,   111,   112,   113,    -1,   129,    -1,
      -1,    -1,   101,    -1,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   482,   711,    -1,    -1,   149,   150,
      -1,    -1,    -1,   491,    -1,    -1,    -1,    -1,    -1,   160,
     161,   727,   728,  1178,    -1,    -1,   153,    -1,    -1,   735,
      -1,    -1,    -1,   511,    -1,    -1,    -1,  1804,    -1,   517,
     149,   150,    -1,    -1,    -1,    -1,   752,    -1,    -1,   755,
      -1,   757,  1756,    -1,    -1,    -1,  1760,  1212,  1213,  1214,
      -1,    -1,    -1,    -1,  1219,  1220,    -1,  1771,   774,   775,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   557,
    1784,     1,    -1,    -1,     4,    -1,    -1,   793,    -1,   795,
     568,   101,   570,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,    -1,    -1,    -1,    -1,    -1,   814,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    12,    13,    14,    15,
      16,  1151,  1779,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     782,    -1,    -1,   785,    -1,    -1,    -1,    -1,    58,    -1,
      -1,    -1,    -1,    -1,   727,   728,   852,  1851,    -1,  1853,
    1854,    -1,  1182,   859,   737,    -1,    -1,   740,   864,   865,
      -1,    -1,    82,    -1,   174,    -1,    -1,   873,    -1,   875,
      -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,    98,  1836,
     886,    -1,   102,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   101,    -1,  1898,   104,   105,   106,   107,   108,
     109,   110,   111,   112,  1908,   101,    -1,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,    -1,   801,   139,
     926,    -1,   805,    -1,    -1,   145,   809,   147,    -1,    -1,
    1934,   151,  1936,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   161,   162,   163,    -1,    -1,   821,  1951,   157,    -1,
      -1,    -1,  1956,   149,   150,    -1,    -1,   153,    -1,   179,
      -1,    -1,    -1,    -1,   160,   161,  1970,    -1,    -1,    -1,
      -1,   191,   192,    -1,    -1,   195,    -1,   755,    -1,   757,
      -1,    -1,    -1,    -1,    -1,   991,    -1,    -1,    -1,  1319,
      -1,    -1,    -1,   945,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   953,  1008,   878,    -1,    -1,    -1,    -1,    -1,  1015,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   795,    -1,    -1,
      -1,    -1,   242,    -1,    -1,   245,    -1,    -1,    -1,    -1,
     982,    -1,    -1,   985,    -1,    -1,   814,  1367,   258,    -1,
      -1,    -1,    -1,    12,    13,    14,    15,    16,    -1,    -1,
      -1,  1057,    -1,    -1,    -1,   275,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   283,    -1,    -1,    -1,    -1,   288,    -1,
      -1,    -1,    -1,    -1,   294,    -1,    -1,    -1,   101,    -1,
      -1,    -1,   302,   106,   107,   108,   109,   110,   111,   112,
     113,  1097,    -1,  1045,   117,    -1,   119,    -1,    -1,  1105,
     320,    70,   322,   323,  1559,    -1,    -1,    -1,   991,    -1,
      -1,    -1,   987,    -1,    -1,    -1,    -1,   337,    -1,    -1,
      -1,   341,    -1,    -1,    -1,    -1,    -1,   150,  1134,  1004,
    1005,    -1,   101,    -1,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,  1151,  1152,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   374,    -1,    -1,    -1,    -1,  1165,
     129,    -1,    -1,    -1,    -1,  1171,    -1,    -1,    -1,    -1,
      -1,    -1,  1178,    -1,    -1,    -1,    -1,    -1,    -1,  1062,
     149,   150,  1065,    -1,   153,    -1,    -1,    -1,   408,    -1,
      -1,   160,   161,    -1,    -1,    -1,    -1,    -1,    -1,  1205,
      -1,    -1,    -1,  1209,    -1,    -1,    63,    64,    65,    66,
      -1,    -1,    -1,    -1,   434,    -1,    -1,    -1,   438,    -1,
      -1,    -1,    -1,    -1,   444,    -1,    -1,    -1,    -1,    -1,
    1008,    -1,    -1,    -1,    -1,    -1,  1014,  1015,    -1,    -1,
      -1,    -1,    -1,    -1,   101,    -1,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    -1,    -1,    -1,    -1,
      -1,    -1,    55,    56,  1270,  1271,    -1,    -1,    -1,    -1,
    1222,    -1,    -1,    -1,    -1,   101,  1228,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,    -1,    -1,    -1,
      -1,    -1,    -1,   513,   514,    -1,   153,    90,   518,   519,
      -1,    -1,  1308,  1309,  1310,  1311,  1312,  1313,    -1,    -1,
      -1,    -1,  1318,  1319,   171,    -1,   101,    -1,    -1,   104,
     105,   106,   107,   108,   109,   110,   111,   112,    -1,    -1,
     550,  1786,    -1,    -1,    -1,   555,    -1,    -1,   558,   559,
    1670,   561,    -1,    -1,    -1,    -1,    -1,   140,   174,    -1,
     143,    -1,   572,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1367,    -1,   156,  1247,    -1,    -1,   587,   588,   589,
      -1,   591,   157,  1256,    -1,    -1,  1251,  1252,    -1,    -1,
    1386,    -1,    -1,    -1,    -1,    -1,    -1,  1165,    -1,    -1,
    1265,  1266,   612,  1345,    -1,   615,    -1,    -1,    -1,   619,
      -1,    -1,   622,  1409,    -1,   625,    -1,   627,    -1,    -1,
      -1,    -1,    -1,   633,    -1,    -1,    -1,   210,    -1,    -1,
      -1,    -1,    -1,  1298,  1299,    -1,   646,    -1,   648,   649,
      -1,   651,  1762,    -1,   654,    -1,    -1,   657,   658,   659,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      -1,    -1,   255,    -1,    -1,    -1,    -1,    -1,    -1,  1421,
      -1,    -1,   265,    -1,  1426,    -1,   129,    -1,    -1,  1431,
    1432,  1433,  1260,   276,    -1,    -1,    -1,    -1,    -1,  1495,
      -1,   711,    -1,  1499,    -1,  1501,   149,   150,    -1,  1505,
     153,  1507,    -1,    -1,    -1,    -1,    -1,   160,   161,  1839,
     303,    -1,    -1,    -1,    -1,   735,   309,   310,    -1,    -1,
     173,   314,    -1,  1529,    -1,  1531,    -1,    -1,   101,    -1,
      -1,    -1,   752,   106,   107,   108,   109,   110,   111,   112,
    1318,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1554,    -1,
      -1,    -1,    -1,   346,   774,   775,   129,    -1,   351,  1565,
     101,   354,  1568,   104,   105,   106,   107,   108,   109,   110,
     111,   112,    -1,   793,    -1,    -1,   149,   150,    -1,    -1,
     153,    -1,    -1,    -1,   101,  1591,    -1,   160,   161,   106,
     107,   108,   109,   110,   111,   112,   113,    -1,    -1,    -1,
     117,   821,   119,    -1,    -1,    -1,  1481,  1482,   149,    -1,
    1616,  1617,  1495,    -1,    -1,    12,    13,    14,    15,    16,
      -1,    -1,    -1,    -1,   844,  1631,  1632,    -1,    -1,    -1,
      -1,    -1,   852,   150,    -1,   855,   153,    -1,    -1,   859,
    1646,    -1,  1517,  1649,   864,   865,    -1,  1599,   441,  1601,
      -1,    -1,    -1,   873,    -1,   875,   876,    -1,   878,    -1,
     453,   454,    -1,    -1,  1670,  1671,    -1,    -1,    -1,    -1,
      -1,    -1,   101,    70,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1699,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1576,    -1,    -1,   101,    -1,   926,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    -1,    -1,    -1,    -1,
      -1,  1499,    -1,   152,    -1,    -1,    -1,  1505,   157,  1507,
      -1,  1606,   129,    -1,  1740,    -1,  1611,  1612,    -1,    -1,
      -1,    -1,    -1,  1749,    -1,    -1,    -1,    -1,  1631,  1632,
      -1,    -1,   149,   150,    -1,    -1,  1762,    -1,    -1,    -1,
      -1,    -1,    -1,   160,   161,    -1,  1772,   987,    -1,    -1,
      -1,   101,    -1,  1779,   104,   105,   106,   107,   108,   109,
     110,   111,   112,    70,  1004,  1005,    -1,    -1,    -1,    -1,
     583,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1804,    -1,
      12,    13,    14,    15,    16,    -1,  1812,    -1,    -1,    -1,
      -1,    -1,    -1,  1819,   101,    -1,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    -1,    -1,    -1,  1835,
    1836,    -1,  1838,  1839,    -1,    -1,    -1,  1057,   631,    -1,
     101,    -1,   129,   104,   105,   106,   107,   108,   109,   110,
     111,   112,    -1,    -1,    -1,    -1,    -1,  1740,    70,    -1,
      -1,    -1,   149,   150,    -1,  1871,   153,    -1,  1646,    -1,
      -1,  1649,    -1,   160,   161,    -1,    -1,  1097,    -1,    -1,
      -1,  1887,  1888,     1,    -1,  1105,     4,    -1,    -1,   101,
      -1,   152,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   101,    -1,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,    -1,  1134,  1921,    -1,   129,    -1,    -1,
      -1,  1699,    -1,    -1,    -1,  1808,    -1,    -1,    -1,  1812,
      -1,  1151,  1152,    -1,    -1,    -1,    -1,   149,   150,    -1,
      58,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   160,   161,
     150,  1171,    -1,   153,    -1,  1838,    -1,    70,  1178,    -1,
      -1,    -1,  1182,    -1,    82,    -1,    -1,    -1,    -1,    -1,
      -1,  1749,    -1,    -1,  1194,    -1,   769,    -1,    -1,    -1,
      -1,    -1,    -1,   776,   102,  1205,    -1,    -1,   101,  1209,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      -1,    -1,    -1,   101,  1887,  1888,   104,   105,   106,   107,
     108,   109,   110,   111,   112,    -1,   129,    -1,    -1,    -1,
      -1,   139,    -1,    -1,    -1,    -1,    -1,   145,    -1,    -1,
    1905,  1251,  1252,    -1,    -1,    -1,   149,   150,  1921,    12,
      13,    14,    15,    16,   162,  1265,  1266,   160,   161,    -1,
    1270,  1271,    -1,    -1,    -1,   153,    -1,    -1,   851,    -1,
    1838,   101,   180,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   191,   192,    -1,    -1,    -1,  1298,  1299,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1308,  1309,
    1310,  1311,  1312,  1313,    -1,    -1,    -1,    70,    -1,  1319,
      -1,    -1,   220,    -1,    -1,    -1,    -1,    -1,   901,  1887,
    1888,    -1,   152,   906,    -1,    -1,    -1,    -1,   236,    -1,
      -1,    -1,    -1,   241,   242,    -1,    -1,   245,   101,    -1,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      -1,    -1,    -1,  1921,    -1,    -1,    -1,  1367,    -1,   267,
      -1,    -1,   270,    -1,   272,    -1,   129,    12,    13,    14,
      15,    16,    -1,    -1,   101,   283,  1386,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   149,   150,   296,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   160,   161,  1409,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,
      -1,    -1,   320,    -1,    -1,   323,    -1,    -1,    -1,    -1,
      -1,    -1,   149,    -1,    -1,    70,    -1,    -1,    -1,   337,
      -1,    -1,    -1,   341,    -1,    -1,     1,    -1,    -1,    -1,
     101,    -1,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,    -1,    -1,    -1,    -1,   101,    -1,    -1,    -1,
      -1,   106,   107,   108,   109,   110,   111,   112,   129,    -1,
      -1,  1481,  1482,   132,   133,   134,   135,   136,   137,   138,
     139,   140,   141,   142,   129,    -1,    -1,   146,   149,   150,
      -1,  1501,    -1,    58,    -1,    -1,    -1,    -1,    -1,   160,
     161,    -1,    -1,    -1,   149,   150,    -1,  1517,    -1,    -1,
      -1,    -1,    -1,    -1,   173,   160,   161,    -1,    -1,  1529,
      -1,  1531,    -1,    -1,    -1,    -1,   434,  1110,  1111,  1112,
     438,    -1,    -1,    -1,    -1,    -1,    -1,   102,    -1,    -1,
      -1,    -1,    -1,    -1,  1554,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    70,    -1,  1565,    -1,  1140,  1568,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1576,    -1,    -1,    -1,
      -1,    -1,  1155,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     145,  1591,    -1,    -1,   101,    -1,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,  1606,   162,    -1,    -1,
      -1,  1611,  1612,    -1,    -1,    -1,  1616,  1617,  1191,    -1,
     101,   519,   129,   104,   105,   106,   107,   108,   109,   110,
     111,   112,    -1,    -1,    -1,    -1,    -1,   192,    -1,    -1,
      -1,    -1,   149,   150,    -1,    -1,    -1,    -1,   129,    -1,
      -1,    -1,   550,   160,   161,    -1,    -1,   555,    -1,    -1,
      -1,   559,    -1,   561,    -1,    -1,    -1,    -1,   149,   150,
    1670,  1671,    -1,    -1,   572,   156,    -1,    -1,    -1,   160,
     161,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     588,   589,   101,    -1,    -1,    -1,    -1,   106,   107,   108,
     109,   110,   111,   112,   113,   603,    -1,    -1,   117,    -1,
     119,    -1,   267,    -1,   612,    -1,    -1,    -1,    -1,   617,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   625,   283,   101,
      -1,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   150,    -1,    -1,   153,    -1,    -1,    -1,    -1,  1322,
      -1,    -1,  1325,    -1,    -1,    -1,    -1,   129,    -1,    -1,
      -1,    -1,  1762,    -1,    -1,   320,    -1,    -1,   323,    -1,
      -1,    -1,  1772,    -1,    -1,    -1,    -1,   149,   150,  1779,
      -1,   153,   337,    -1,    -1,    -1,   341,    -1,   160,   161,
      -1,    -1,   101,    -1,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,  1804,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   711,    -1,    -1,    -1,    -1,    -1,  1819,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     728,    -1,    -1,    -1,    -1,  1835,  1836,    -1,    -1,  1839,
     149,   150,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   160,   161,    -1,   752,    -1,    -1,    -1,   101,   757,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      -1,  1871,    -1,    -1,    -1,    -1,   774,   775,    -1,    -1,
      -1,    -1,    -1,   438,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   793,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1905,   149,   150,     1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   160,   161,    -1,
      99,    -1,   101,   821,   103,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,    -1,    -1,   101,    -1,    -1,
      -1,  1514,   106,   107,   108,   109,   110,   111,   112,   113,
      -1,    -1,    -1,   117,   852,   119,    -1,    -1,    -1,    -1,
      -1,   859,    -1,    -1,   519,    58,   864,    -1,    -1,    -1,
     149,    -1,    -1,   152,   153,   873,    -1,    -1,   876,    -1,
     878,    -1,    -1,    -1,   374,   883,   150,    -1,    -1,   153,
      -1,    -1,    -1,    -1,    -1,   550,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   559,    -1,   561,    -1,    -1,   102,
      -1,  1584,    -1,    -1,    -1,    -1,    -1,   572,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   926,    -1,
      -1,    -1,    -1,   588,   589,    -1,    -1,    -1,    98,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   109,
      -1,   111,   145,   113,    -1,    -1,    -1,   612,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   162,
     625,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   987,
      -1,   151,    -1,   153,   154,    -1,    -1,    -1,    -1,   192,
      -1,    -1,    -1,    -1,    -1,    -1,  1004,  1005,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   513,   514,  1698,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1707,   195,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   711,    -1,    -1,  1057,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     283,    -1,    -1,    -1,    -1,    -1,    -1,   752,   258,  1097,
     260,   261,    -1,    -1,    -1,    -1,    -1,  1105,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   774,
     775,    -1,    -1,    -1,    -1,    -1,    -1,   320,   288,    -1,
     323,    -1,    -1,    -1,   294,    -1,  1134,    -1,   793,    -1,
      -1,    -1,    -1,    -1,   337,    -1,    -1,    -1,   341,    -1,
      -1,    -1,    -1,    -1,  1152,    -1,   646,    -1,    -1,    -1,
      -1,   651,   322,    -1,   654,    -1,   821,    -1,   328,    -1,
     330,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1178,    -1,    -1,   673,    -1,    -1,    -1,    -1,    -1,  1187,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   852,    -1,    -1,
      -1,    -1,    -1,    -1,   859,    -1,    -1,  1205,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   707,   873,    -1,
      -1,   876,    -1,   878,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   438,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1251,  1252,    -1,    -1,    -1,   418,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1265,  1266,    -1,
      -1,   926,  1270,  1271,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   444,    -1,   446,   447,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1298,  1299,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1308,  1309,  1310,  1311,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   519,    -1,    -1,    -1,
      -1,   491,   987,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1004,
    1005,   511,    -1,    -1,    -1,    -1,   516,   550,   518,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   559,    -1,   561,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   538,   572,
     540,   541,    -1,    -1,    -1,    -1,    -1,    -1,  1386,    -1,
      -1,    -1,    -1,    -1,    -1,   588,   589,    -1,   558,    -1,
      -1,    -1,  1057,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     570,  1409,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   612,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   591,   625,   593,   594,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1105,    -1,    -1,    -1,    -1,   615,   616,    -1,    -1,    -1,
      -1,    -1,   622,     1,    -1,    -1,     4,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1134,
      -1,    -1,    -1,  1481,  1482,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1152,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1507,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   711,  1517,
      58,    -1,    -1,  1178,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1529,    -1,  1531,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,
    1205,    -1,    -1,    -1,    -1,    -1,  1554,    -1,    -1,   752,
      -1,    -1,    -1,    -1,   102,    -1,    -1,  1565,    -1,    -1,
    1568,    -1,    -1,    -1,    -1,    -1,     1,    -1,  1576,     4,
      -1,   774,   775,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1251,  1252,    -1,    -1,
     793,   139,    -1,    -1,    -1,    -1,    -1,   145,  1606,   147,
    1265,  1266,    -1,  1611,  1612,  1270,  1271,    -1,  1616,  1617,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   821,    -1,
      -1,    -1,    -1,    58,  1632,    -1,    -1,    -1,    -1,    -1,
      -1,   179,    -1,  1298,  1299,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   191,    -1,    -1,    -1,    82,    -1,   852,
      -1,    -1,    -1,    -1,    -1,    -1,   859,    -1,    -1,    -1,
      -1,    -1,    -1,  1671,    -1,    -1,    -1,   102,    -1,    -1,
     873,    -1,    -1,   876,   844,   878,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   855,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   242,    -1,    -1,   245,    -1,    -1,
      -1,    -1,   250,    -1,   139,   875,    -1,    -1,    -1,    -1,
     145,    -1,   147,    -1,    -1,    -1,   886,    -1,    -1,    -1,
      -1,  1386,    -1,   926,    -1,   895,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   283,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   179,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   302,    -1,   191,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1772,    -1,    -1,    -1,    -1,    -1,
      -1,  1779,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   987,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1804,    -1,    -1,    -1,
      -1,  1004,  1005,    -1,    -1,    -1,    -1,   242,    -1,    -1,
     245,  1819,    -1,    -1,    -1,   250,  1481,  1482,    -1,    -1,
      -1,   991,    -1,    -1,    -1,    -1,   374,  1835,  1836,    -1,
      -1,    -1,    -1,    -1,  1334,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1343,    -1,  1015,    -1,  1347,   283,  1349,
      -1,    -1,  1517,    -1,  1057,    -1,    -1,    -1,    -1,    -1,
     408,    -1,    -1,  1871,  1529,    -1,  1531,   302,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1888,    -1,    -1,    -1,    -1,    -1,   434,    -1,    -1,  1554,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1905,    -1,    -1,
    1565,    -1,  1105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1576,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   374,
      -1,  1606,    -1,    -1,    -1,    -1,  1611,  1612,    -1,  1152,
      -1,  1616,  1617,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   513,   514,    -1,    -1,    -1,
      -1,    -1,    -1,   408,    -1,  1178,    -1,    -1,    -1,    -1,
      -1,  1151,    -1,    -1,    -1,    -1,    -1,  1487,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   434,
      -1,    -1,  1205,    -1,    -1,    -1,  1671,   555,    -1,    -1,
      -1,   559,  1182,   561,    -1,    -1,    -1,    -1,  1188,  1519,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1529,
      -1,  1531,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1251,  1252,
      -1,    -1,    -1,    -1,  1554,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1265,  1266,    -1,    -1,    -1,  1270,  1271,    -1,
      -1,     1,    -1,    -1,     4,    -1,    -1,    -1,   513,   514,
      -1,    -1,    -1,    -1,    -1,   633,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1298,  1299,    -1,   646,    -1,
     648,   649,    -1,   651,    -1,    -1,   654,  1772,    -1,   657,
     658,   659,    -1,    -1,  1779,    -1,    -1,    -1,    -1,    -1,
     555,    -1,    -1,    -1,   559,    -1,   561,    -1,    58,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1804,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1319,
      -1,    -1,    82,    -1,  1819,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1662,    -1,    -1,    -1,  1666,    -1,    -1,    -1,
    1835,  1836,   102,  1673,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1386,    -1,    -1,    -1,   735,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1367,   633,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1871,    -1,    -1,   139,
      -1,   646,    -1,   648,   649,   145,   651,   147,    -1,   654,
      -1,    -1,   657,   658,   659,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    48,    -1,    -1,    -1,    52,    -1,    54,    -1,
    1905,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   179,
      -1,    -1,    -1,    -1,    -1,    71,    -1,    -1,    -1,    -1,
      -1,   191,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1769,
    1770,    -1,  1772,    -1,    -1,    -1,    -1,    -1,  1481,  1482,
      -1,    -1,    98,    99,    -1,   101,    -1,   103,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     735,   117,   118,   119,   852,   121,   122,    -1,    -1,    -1,
      -1,    -1,   242,   129,  1517,   245,   864,   865,    -1,    -1,
     250,    -1,    -1,    -1,    -1,    -1,  1529,  1827,  1531,   178,
      -1,    -1,    -1,   149,    -1,    -1,   152,   153,    -1,    -1,
      -1,    -1,    -1,   159,   160,   161,   162,   163,   164,   165,
      -1,  1554,    -1,   283,    -1,    -1,    -1,   173,   174,    -1,
      -1,    -1,  1565,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   302,  1576,    -1,    -1,    -1,    -1,    -1,  1879,
      -1,  1881,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1901,    -1,  1606,    -1,    -1,    -1,    -1,  1611,  1612,
      -1,    -1,    -1,  1616,  1617,    -1,    -1,   852,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   864,
     865,    -1,    -1,    -1,    -1,    -1,    -1,  1937,  1938,  1939,
      -1,    -1,    -1,    -1,   374,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1671,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   408,  1649,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     0,    -1,    -1,     3,    -1,    -1,    -1,    -1,    -1,
    1670,    -1,    -1,    -1,   434,    -1,    -1,    -1,    -1,    -1,
     359,    -1,    -1,   362,   363,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   372,   373,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   387,   388,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1097,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1105,    -1,   408,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    76,    -1,  1772,
    1740,    -1,    -1,    -1,    -1,    -1,  1779,    -1,    -1,    -1,
      -1,    -1,    -1,   513,   514,    -1,  1134,    -1,    -1,   438,
      -1,    -1,  1762,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1804,    -1,    -1,  1152,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1819,    -1,    -1,    -1,
      -1,    -1,    -1,  1171,    -1,   555,   135,    -1,    -1,   559,
      -1,   561,  1835,  1836,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1209,  1097,    -1,    -1,    -1,    -1,    -1,  1871,  1839,
    1105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1134,
      -1,    -1,  1905,   633,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   646,  1152,   648,   649,
     229,   651,    -1,    -1,   654,    -1,    -1,   657,   658,   659,
      -1,    -1,    -1,    -1,    -1,   244,  1171,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   254,    -1,    -1,    -1,    -1,
      -1,  1921,    -1,    -1,    -1,   264,    -1,    -1,    -1,    -1,
    1308,  1309,  1310,  1311,  1312,  1313,    -1,    -1,    -1,   278,
     279,    -1,    -1,    -1,  1209,    -1,   285,   286,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   301,    -1,    -1,    86,    -1,    -1,    -1,    -1,
      -1,    92,    93,    -1,    -1,   735,    -1,    -1,    -1,    -1,
     319,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   126,    -1,    -1,    -1,   688,
     689,   690,   691,   692,   693,   694,   695,   696,   697,   698,
     699,   700,   701,   702,   703,   704,   705,   706,    -1,    -1,
      -1,  1409,    -1,    -1,    -1,    -1,   375,    -1,    -1,    -1,
      -1,    -1,    -1,  1308,  1309,  1310,  1311,  1312,  1313,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   406,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   768,
      -1,    -1,   852,   432,    -1,    -1,    -1,   436,    -1,    -1,
      -1,    -1,    -1,    -1,   864,   865,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   455,    -1,    -1,    -1,
     459,   460,    -1,  1501,   463,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   478,
     479,   480,   481,    -1,  1409,    -1,    -1,    -1,    -1,    -1,
      -1,  1529,    -1,  1531,    -1,    -1,    -1,    -1,   497,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   505,    -1,    -1,    -1,
      -1,   292,    -1,    -1,    -1,    -1,  1554,    -1,    -1,    -1,
      -1,    -1,    -1,    47,    -1,    -1,    -1,    -1,    -1,    -1,
    1568,    -1,    -1,    -1,   533,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,  1591,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   564,    -1,    -1,    -1,    -1,
      -1,    -1,   571,    -1,    -1,    -1,  1501,   576,    -1,    -1,
      -1,    -1,    -1,   922,    -1,    -1,    -1,    -1,   927,    -1,
      -1,    -1,    -1,    -1,    -1,   119,    -1,    -1,    -1,   938,
     599,   600,    -1,    -1,  1529,    -1,  1531,    -1,   132,    -1,
     134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1554,
      -1,    -1,    -1,  1671,    -1,    -1,    -1,    -1,    -1,   978,
     164,    -1,    -1,  1568,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   437,   181,   439,    -1,
      -1,   660,    -1,    -1,    -1,    -1,  1591,   448,   449,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1097,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1105,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   219,    -1,    -1,    -1,   223,
      -1,    -1,   226,   227,    -1,    -1,   230,    -1,    -1,   233,
     234,    -1,    -1,    -1,  1134,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   727,    -1,
      -1,    -1,  1152,    -1,  1772,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   742,    -1,    -1,  1671,   746,    -1,    -1,
      -1,  1171,    -1,    -1,    -1,    -1,   755,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1105,    -1,    -1,    -1,
      -1,   295,    -1,    -1,   298,   556,    -1,    -1,   777,    -1,
      -1,  1819,    -1,    -1,    -1,    -1,    -1,   786,    -1,  1209,
      -1,    -1,    -1,   792,   318,    -1,    -1,  1835,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   333,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     829,    -1,    -1,  1871,    -1,    -1,    -1,   836,    -1,  1178,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1772,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   862,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1212,  1213,  1214,    47,    -1,    -1,    -1,
    1219,  1220,    -1,    -1,    -1,    -1,    -1,    -1,  1308,  1309,
    1310,  1311,  1312,  1313,  1819,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1241,   427,    -1,    -1,    -1,    -1,    -1,    -1,
    1835,    -1,    -1,    -1,    -1,    -1,    -1,   916,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1278,
    1279,    -1,    -1,    -1,    -1,    -1,  1871,    -1,   119,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   482,    -1,
      -1,   132,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,
     494,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   768,    -1,  1409,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   997,    -1,
      -1,    -1,  1001,    -1,    -1,    -1,    -1,    -1,    -1,  1008,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1018,
      -1,    -1,    -1,    -1,    -1,    -1,  1025,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1034,    -1,  1036,    -1,    -1,
      -1,    -1,    -1,    -1,   568,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   226,   227,    -1,    -1,   230,
      -1,    -1,   233,   234,   845,   846,    -1,    -1,    -1,  1068,
      -1,    -1,    -1,  1072,    -1,   856,   857,   858,    -1,    -1,
     861,  1501,    -1,   607,   608,    -1,    -1,  1086,    -1,    -1,
    1089,    -1,    -1,    -1,    -1,    -1,   620,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1529,
      -1,  1531,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1554,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   318,  1568,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   940,
      -1,    -1,   333,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1591,    -1,    -1,    -1,    -1,    -1,  1176,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1200,    -1,   984,    -1,    -1,    -1,   731,   732,    -1,
      -1,    -1,    -1,   737,    -1,    -1,    -1,    -1,    -1,    -1,
    1559,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   758,    -1,    -1,   761,   762,    -1,
     764,    -1,   766,   767,    -1,    -1,    -1,    -1,    -1,    -1,
    1031,  1671,    -1,    -1,    -1,    -1,   427,    -1,    -1,  1040,
    1041,  1042,  1043,    -1,    -1,    -1,    -1,  1048,  1049,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1058,    -1,    -1,
      -1,   805,    -1,    -1,    -1,   809,    -1,    -1,    -1,    -1,
      -1,  1290,    -1,    -1,    -1,  1294,    -1,    -1,  1079,    -1,
    1081,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   494,    -1,  1324,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1337,  1338,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1772,  1134,    -1,    -1,    -1,    -1,    -1,    -1,
     884,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1374,    -1,    -1,  1377,  1160,
      -1,    -1,    -1,    -1,    -1,  1166,    -1,  1168,  1169,    -1,
      -1,    -1,    -1,  1392,    -1,    -1,  1177,    -1,  1179,  1819,
    1181,    -1,  1183,    -1,    -1,    -1,    -1,  1188,    -1,    -1,
      -1,    -1,    -1,  1752,    -1,  1835,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   607,   608,    -1,    -1,
      -1,    -1,    -1,  1442,    -1,    -1,    -1,  1786,    -1,   620,
      -1,  1871,  1451,    -1,    -1,    -1,  1455,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1246,    -1,    -1,    -1,    -1,
    1469,  1470,  1253,  1254,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1014,    -1,    -1,    -1,    -1,    -1,  1277,    -1,    -1,    -1,
      -1,    -1,    -1,  1284,    -1,    -1,  1287,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1867,  1868,
      -1,    -1,    -1,    -1,    -1,    -1,  1317,    -1,  1062,    -1,
      -1,  1065,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1894,    -1,    -1,    -1,    -1,
     731,   732,    -1,    -1,    -1,    -1,   737,    -1,    -1,    -1,
    1569,  1570,    -1,    -1,    -1,    -1,  1357,    -1,    -1,    -1,
    1919,    -1,    -1,    -1,    -1,    -1,    -1,   758,    -1,    -1,
     761,   762,    -1,   764,    -1,   766,   767,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1390,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1398,    -1,  1400,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1966,    -1,    -1,
      -1,    -1,    -1,    -1,   805,    -1,    -1,    -1,   809,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1184,    -1,    -1,    -1,    -1,  1446,  1447,    -1,  1192,  1193,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1461,  1462,    -1,  1464,    -1,    -1,    -1,    -1,  1687,    -1,
      -1,    -1,  1473,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1483,  1484,    -1,    -1,    -1,    -1,    -1,    -1,
    1709,    -1,    -1,   884,    -1,    -1,    -1,   162,    -1,    -1,
      -1,    -1,    -1,  1247,    -1,    -1,    -1,    -1,  1727,    -1,
      -1,    -1,  1256,    -1,    -1,  1259,    -1,  1261,  1262,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   191,   192,    -1,    -1,
      -1,    82,    -1,    -1,    -1,  1754,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1775,  1301,   223,  1778,
      -1,    -1,    -1,    -1,    -1,   230,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1577,  1578,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1587,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   147,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   162,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1622,  1623,  1014,  1368,    -1,    -1,    -1,   179,    -1,
      -1,    -1,    -1,   298,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   192,    -1,  1862,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   320,   321,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1062,    -1,    -1,  1065,    -1,   341,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   245,    -1,    -1,    -1,    -1,    -1,
    1701,    -1,    -1,    -1,    -1,    -1,  1450,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1719,    -1,
      -1,  1722,  1723,    -1,    -1,    -1,    -1,    -1,  1729,    -1,
      -1,    -1,    -1,    -1,    -1,  1479,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   411,    -1,    -1,    -1,
      -1,   302,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1505,   427,   428,    -1,   430,   431,  1511,    -1,   320,
      -1,    -1,    -1,   438,    -1,    -1,    -1,   442,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1184,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1192,  1193,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   483,    -1,
    1564,    -1,   487,  1824,    -1,    -1,    -1,    -1,    -1,    -1,
    1831,    -1,    -1,    -1,    -1,    -1,  1837,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   519,    -1,  1247,   408,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1256,    -1,    -1,  1259,    -1,
    1261,  1262,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1885,    -1,    -1,   438,    -1,    -1,
      98,    -1,    -1,    -1,  1638,  1639,    -1,    -1,    -1,    -1,
      -1,    -1,  1646,    -1,   569,    -1,  1650,   572,  1909,    -1,
    1301,    -1,    -1,    -1,    -1,    -1,  1917,    -1,    -1,    -1,
      -1,    -1,    -1,   588,   589,    -1,    -1,    -1,    -1,    -1,
      -1,  1932,    -1,    -1,   599,    -1,    -1,    -1,   603,   147,
      -1,    -1,    -1,   151,    -1,   610,    -1,   612,    -1,    -1,
      -1,    -1,    -1,    -1,   162,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   513,   514,    -1,    -1,    -1,    -1,   519,    -1,
      -1,   179,    -1,    -1,    -1,    -1,    -1,  1368,    -1,    -1,
      -1,    -1,    -1,    -1,   192,    -1,    -1,   195,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1742,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   572,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   245,   589,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   711,    -1,    -1,    -1,
     258,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1450,
      -1,   612,   727,   728,  1808,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   737,   738,    -1,   740,   741,    -1,    -1,    -1,
      -1,    -1,   633,    -1,    -1,    -1,   294,   752,  1479,    -1,
     755,    -1,   757,   758,   302,    -1,    -1,    -1,    -1,   764,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   774,
     775,    -1,   320,    -1,   322,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   793,    -1,
      -1,    -1,   797,    -1,    -1,    -1,   801,    -1,    -1,    -1,
     805,   806,    -1,  1887,   809,   810,    -1,    -1,    -1,    -1,
      -1,    -1,   817,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     711,    -1,    -1,    -1,    -1,    -1,   374,    -1,    -1,    -1,
      -1,    -1,    -1,  1564,    -1,    -1,    -1,    -1,   179,    -1,
      -1,    -1,    -1,    -1,   735,    -1,    -1,    -1,    -1,    -1,
      -1,   192,    -1,    -1,   859,   860,    -1,    -1,    -1,    -1,
     408,   752,    -1,    -1,   205,    -1,   207,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    -1,    -1,    -1,
      52,   886,    54,   774,   775,    -1,    -1,    -1,    -1,    -1,
     438,    -1,    -1,    -1,    -1,    -1,   444,    -1,    -1,    71,
      -1,    -1,   793,    -1,    -1,    -1,    -1,  1638,  1639,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1650,
      -1,   926,    -1,    -1,    -1,    -1,    98,    99,    -1,   101,
      -1,   103,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,    -1,   117,   118,   119,    -1,   121,
     122,    -1,   293,    -1,    -1,    -1,    -1,   129,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   513,   514,    -1,   859,    -1,
      -1,   519,    -1,    -1,   865,    -1,    -1,   149,   150,    -1,
     152,   153,    -1,    -1,    -1,   157,   991,   159,   160,   161,
     162,   163,   164,   165,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1008,  1009,    -1,    -1,    -1,    -1,    -1,
    1015,  1742,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   572,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   926,    -1,    -1,    -1,    -1,
      -1,   589,    -1,   591,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1057,    -1,    -1,    -1,    -1,  1062,  1063,    -1,
    1065,  1066,    -1,    -1,   612,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1808,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   633,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   646,    -1,
     648,   649,    -1,   651,    -1,    -1,   654,    -1,    -1,   657,
     658,   659,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   464,    -1,    -1,    -1,    -1,    -1,   470,
      -1,    -1,    -1,    -1,   475,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   711,    -1,    -1,  1057,    -1,    -1,    -1,
      -1,    -1,    -1,  1178,    -1,    -1,    -1,    -1,    -1,  1184,
    1185,    -1,    -1,    -1,    -1,    -1,    -1,   735,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1205,    -1,    -1,    -1,   752,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   562,    -1,    -1,    -1,    -1,   774,   775,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1247,  1248,    -1,   793,    -1,    -1,   589,    -1,
      -1,  1256,  1257,    -1,  1259,    -1,    -1,    -1,    -1,    -1,
      -1,   602,    -1,    -1,    -1,  1270,  1271,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1171,    -1,    -1,    -1,    -1,    -1,    -1,  1178,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   859,   653,    -1,  1205,    -1,    -1,   865,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   875,    -1,    -1,
     671,   672,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     681,    -1,   683,   684,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     5,    -1,    -1,    -1,
      -1,    -1,    -1,    12,    13,    14,    15,    16,    -1,    -1,
     711,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   926,  1270,
    1271,  1386,    -1,   724,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   735,    -1,    -1,    -1,    -1,    48,
      -1,    -1,    -1,    52,    -1,    54,    -1,    -1,   749,    -1,
      -1,   752,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   779,    -1,
      -1,   782,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    98,
      99,    -1,   101,    -1,   103,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,    -1,   117,   118,
     119,    -1,   121,   122,    -1,    -1,    -1,   818,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1495,    -1,    -1,    -1,    -1,  1386,    -1,    -1,    -1,    -1,
     149,    -1,    -1,   152,   153,    -1,  1511,    -1,    -1,  1057,
     159,   160,   161,   162,   163,   164,   165,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   865,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   875,   876,    -1,    -1,    -1,    -1,
      -1,    -1,   883,    -1,    -1,    -1,    -1,    -1,    -1,  1097,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1565,    -1,    -1,   904,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   913,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   926,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   934,    -1,    -1,    -1,    -1,    -1,    -1,
     941,    -1,    -1,  1151,    -1,    -1,    -1,    -1,    -1,    -1,
    1501,  1616,  1617,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1171,    -1,    -1,  1631,  1632,    -1,    -1,
    1178,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1647,    -1,   985,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1205,    -1,    -1,
      -1,  1209,    -1,    -1,    -1,    -1,    12,    13,    14,    15,
      16,    -1,    -1,    19,  1565,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    -1,    49,    50,    51,    -1,    53,    -1,    -1,
      -1,  1052,    -1,  1054,    -1,  1056,    -1,    -1,    -1,    -1,
      -1,    -1,  1270,  1271,    70,  1616,  1617,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1740,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1748,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,   105,
    1308,  1309,  1310,    -1,  1312,  1313,    -1,    -1,    -1,    -1,
      -1,  1319,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1122,  1123,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1804,
      -1,    -1,    -1,  1808,  1809,    -1,   152,  1812,    -1,    -1,
      -1,    -1,    -1,    -1,   160,   161,    -1,    -1,    -1,  1367,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1838,    -1,    -1,    -1,    -1,  1386,    -1,
      -1,  1182,    -1,    -1,    -1,    -1,    -1,  1188,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1205,     5,    -1,    -1,    -1,    -1,
      -1,    -1,    12,    13,    14,    15,    16,    -1,    -1,    -1,
      -1,  1222,  1887,  1888,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1235,    -1,    -1,  1238,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    -1,
      -1,    -1,    52,  1804,    54,    -1,  1921,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1289,    -1,
      -1,    -1,    -1,  1501,    -1,    -1,    -1,    -1,    98,    99,
      -1,   101,    -1,   103,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,    -1,   117,   118,   119,
      -1,   121,   122,    -1,    -1,    -1,  1327,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1345,    -1,    -1,  1348,    -1,   149,
      -1,    -1,   152,   153,    -1,    -1,    -1,  1565,    -1,   159,
     160,   161,   162,   163,   164,   165,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1591,    -1,  1386,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1396,  1397,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1616,  1617,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,    -1,    -1,
    1421,    -1,  1423,     9,    -1,    -1,    12,    13,    14,    15,
      16,    17,    -1,    19,    -1,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      -1,    -1,  1670,    -1,    50,    51,    -1,    -1,    -1,    55,
      56,    57,    58,    59,    60,    61,    62,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1501,    -1,    -1,    -1,    -1,  1506,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   100,    -1,    -1,    -1,   104,   105,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,  1550,
      -1,    -1,    -1,    -1,  1762,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   148,    -1,    -1,    -1,   152,   153,    -1,    -1,
      -1,  1779,    -1,    -1,   160,   161,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1589,    -1,
      -1,  1592,    -1,    -1,    -1,    -1,  1804,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1836,    -1,
      -1,  1839,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    -1,    19,
      -1,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    -1,    -1,    48,    -1,
      50,    51,    52,    -1,    54,    55,    56,    57,    58,    59,
      60,    61,    62,    63,    -1,    -1,    -1,    67,    -1,    69,
      70,    71,    72,    -1,    74,    -1,    -1,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    92,    93,    94,    -1,    96,    -1,    98,    99,
     100,   101,    -1,   103,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
      -1,   121,   122,    -1,    -1,    -1,    -1,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   148,   149,
      -1,    -1,   152,   153,    -1,    -1,  1787,   157,    -1,   159,
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
      14,    15,    16,    -1,    -1,    19,    -1,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    -1,    49,    50,    51,    -1,    53,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      12,    13,    14,    15,    16,    -1,    70,    19,    -1,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    -1,    -1,    -1,   101,    50,    51,
     104,   105,   106,   107,   108,   109,   110,   111,   112,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,
      -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   149,   150,    -1,   152,   153,
      -1,    -1,    -1,   157,    -1,    -1,   160,   161,     3,     4,
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
      16,    17,    -1,    19,    -1,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      -1,    -1,    -1,    -1,    50,    51,    -1,    -1,    -1,    55,
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
      -1,    -1,   149,   150,    -1,   152,   153,    -1,    -1,     3,
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
      -1,    -1,    -1,    12,    13,    14,    15,    16,    -1,    -1,
      19,    70,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    -1,
      49,    50,    51,    -1,    53,   104,   105,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,    -1,    -1,
      -1,    -1,    -1,   152,   153,    -1,    12,    13,    14,    15,
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
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   131,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,   152,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    -1,    19,    -1,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    -1,    -1,    -1,    -1,    50,    51,    -1,    -1,    -1,
      55,    56,    57,    58,    59,    60,    61,    62,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,
     105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   131,    -1,    -1,    -1,
      -1,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    -1,    19,   152,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    -1,    -1,    -1,    -1,    50,    51,    -1,
      -1,    -1,    55,    56,    57,    58,    59,    60,    61,    62,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,   104,   105,    19,    -1,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    -1,    49,    50,    51,    -1,    53,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,
      -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,   105,
      -1,    -1,    -1,    -1,    12,    13,    14,    15,    16,    17,
      -1,    19,    -1,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    -1,    -1,
      -1,    -1,    50,    51,    -1,    -1,   152,    55,    56,    57,
      58,    59,    60,    61,    62,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   101,    -1,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,    -1,    -1,    -1,    12,    13,
      14,    15,    16,    -1,    -1,    19,    -1,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,   152,    49,    50,    51,    -1,    53,
      -1,    -1,    48,    -1,    -1,    -1,    52,    -1,    54,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     104,   105,    98,    99,    -1,   101,    -1,   103,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
      -1,   117,   118,   119,    48,   121,   122,    -1,    52,    -1,
      54,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,   152,    -1,
      -1,    -1,    -1,   149,   150,    -1,   152,   153,    -1,    -1,
      -1,   157,    -1,   159,   160,   161,   162,   163,   164,   165,
      -1,    -1,    -1,    -1,    98,    99,    -1,   101,    -1,   103,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,   117,   118,   119,    48,   121,   122,    -1,
      52,    -1,    54,    -1,    -1,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,
      -1,    -1,    -1,    -1,    -1,   149,   150,   151,   152,   153,
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
      -1,   149,    -1,    -1,   152,   153,    -1,    -1,    -1,   157,
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
      -1,    -1,   156,    -1,    -1,   159,   160,   161,   162,   163,
     164,   165,    -1,    -1,    -1,    -1,    98,    99,    -1,   101,
      -1,   103,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,    -1,   117,   118,   119,    48,   121,
     122,    -1,    52,    -1,    54,    -1,    -1,   129,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    71,    -1,    -1,    -1,    -1,    -1,   149,   150,    -1,
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
      -1,   149,    -1,   151,   152,   153,    -1,    -1,    -1,    -1,
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
     114,   115,    -1,   117,   118,   119,    -1,   121,   122,    -1,
      -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   149,    -1,    -1,   152,   153,
      -1,    -1,    -1,    -1,    -1,   159,   160,   161,   162,   163,
     164,   165
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
     188,   189,   174,   225,   174,   225,   221,    78,   151,   176,
     151,   176,   174,   174,   221,   174,   362,   174,   221,   220,
     221,   108,   109,   110,   111,   112,   256,   258,   259,   174,
      95,   174,    82,   149,   149,   177,   148,   174,   174,   149,
     223,   225,   401,   174,   151,   176,   148,   148,   176,   155,
     155,   154,   154,   154,   177,   151,   176,   214,   214,   177,
     154,   177,   461,   343,   157,   346,   148,   381,   151,   156,
     151,   155,   156,   362,   461,   220,   118,   191,   192,   153,
     192,   153,   192,   154,   148,   151,   176,   177,   177,   151,
     151,   176,   176,   177,   177,   177,   176,   176,   154,   177,
     151,   401,   350,   350,   177,   177,   221,   148,   326,   326,
     326,   149,   196,   335,   336,   438,   446,   447,   448,   449,
     174,   155,   174,   333,   174,   376,   402,   407,   214,   295,
     155,   174,   339,   340,   339,   357,   131,   354,   355,   221,
     151,   151,   149,   223,   221,   232,   277,   279,   282,   288,
     295,   299,   223,   173,   174,   221,   241,   242,   277,   174,
     461,   151,   151,   151,   225,   258,   259,   149,   214,   149,
     182,   232,   198,   251,   107,   223,   401,   382,   176,   176,
     154,   350,   177,   177,   154,   154,   148,   157,   345,   177,
     214,   186,   214,   461,   148,   154,   154,   191,   191,   350,
     151,   151,   350,   350,   151,   151,   154,   155,   131,   349,
     131,   154,   177,   177,   151,   151,   154,   447,   448,   449,
     295,   446,   155,   174,   401,   401,   174,   151,   407,   401,
     174,   223,    75,    76,   157,   235,   236,   237,   151,   221,
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
       1,     2,     2,     5,     3,     5,    10,     5,    10,     5,
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
#line 6841 "Parser/parser.cc"
    break;

  case 3:
#line 533 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 6847 "Parser/parser.cc"
    break;

  case 4:
#line 540 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantInteger( *(yyvsp[0].tok) ) ); }
#line 6853 "Parser/parser.cc"
    break;

  case 5:
#line 541 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 6859 "Parser/parser.cc"
    break;

  case 6:
#line 542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 6865 "Parser/parser.cc"
    break;

  case 7:
#line 543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 6871 "Parser/parser.cc"
    break;

  case 8:
#line 544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantChar( *(yyvsp[0].tok) ) ); }
#line 6877 "Parser/parser.cc"
    break;

  case 19:
#line 565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 6883 "Parser/parser.cc"
    break;

  case 20:
#line 569 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.constant) = build_constantStr( *(yyvsp[0].str) ); }
#line 6889 "Parser/parser.cc"
    break;

  case 21:
#line 573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 6895 "Parser/parser.cc"
    break;

  case 22:
#line 575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 6905 "Parser/parser.cc"
    break;

  case 23:
#line 586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 6911 "Parser/parser.cc"
    break;

  case 24:
#line 588 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 6917 "Parser/parser.cc"
    break;

  case 25:
#line 592 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_dimensionref( (yyvsp[0].tok) ) ); }
#line 6923 "Parser/parser.cc"
    break;

  case 27:
#line 595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 6929 "Parser/parser.cc"
    break;

  case 28:
#line 597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new StmtExpr( dynamic_cast<CompoundStmt *>(maybeMoveBuild<Statement>((yyvsp[-1].sn)) ) ) ); }
#line 6935 "Parser/parser.cc"
    break;

  case 29:
#line 599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.en) = nullptr; }
#line 6941 "Parser/parser.cc"
    break;

  case 30:
#line 601 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.en) = nullptr; }
#line 6947 "Parser/parser.cc"
    break;

  case 31:
#line 603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild<Expression>( (yyvsp[-3].en) );
			(yyval.en) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 6957 "Parser/parser.cc"
    break;

  case 33:
#line 617 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// steal the association node from the singleton and delete the wrapper
			(yyvsp[-2].genexpr)->associations.splice((yyvsp[-2].genexpr)->associations.end(), (yyvsp[0].genexpr)->associations);
			delete (yyvsp[0].genexpr);
			(yyval.genexpr) = (yyvsp[-2].genexpr);
		}
#line 6968 "Parser/parser.cc"
    break;

  case 34:
#line 627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuildType((yyvsp[-2].decl)), maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } );
		}
#line 6977 "Parser/parser.cc"
    break;

  case 35:
#line 632 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } ); }
#line 6983 "Parser/parser.cc"
    break;

  case 37:
#line 642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-5].en), new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) ) ) )) ) ); }
#line 6989 "Parser/parser.cc"
    break;

  case 38:
#line 648 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 6995 "Parser/parser.cc"
    break;

  case 39:
#line 650 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.en) = new ExpressionNode( new ConstructorExpr( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) ) );
		}
#line 7005 "Parser/parser.cc"
    break;

  case 40:
#line 656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7011 "Parser/parser.cc"
    break;

  case 41:
#line 658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 7017 "Parser/parser.cc"
    break;

  case 42:
#line 660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 7023 "Parser/parser.cc"
    break;

  case 43:
#line 662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), new ExpressionNode( (yyvsp[-2].constant) ) ) ); }
#line 7029 "Parser/parser.cc"
    break;

  case 44:
#line 664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7035 "Parser/parser.cc"
    break;

  case 45:
#line 666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 7041 "Parser/parser.cc"
    break;

  case 46:
#line 668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-1].en), build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) ) ) ); }
#line 7047 "Parser/parser.cc"
    break;

  case 47:
#line 670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7053 "Parser/parser.cc"
    break;

  case 48:
#line 672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[0].aggKey), (yyvsp[-2].en) ) ); }
#line 7059 "Parser/parser.cc"
    break;

  case 49:
#line 674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7065 "Parser/parser.cc"
    break;

  case 50:
#line 676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 7071 "Parser/parser.cc"
    break;

  case 51:
#line 678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7077 "Parser/parser.cc"
    break;

  case 52:
#line 680 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::IncrPost, (yyvsp[-1].en) ) ); }
#line 7083 "Parser/parser.cc"
    break;

  case 53:
#line 682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::DecrPost, (yyvsp[-1].en) ) ); }
#line 7089 "Parser/parser.cc"
    break;

  case 54:
#line 684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].in), true ) ) ); }
#line 7095 "Parser/parser.cc"
    break;

  case 55:
#line 686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].in), true ))->set_maybeConstructed( false ) ) ); }
#line 7101 "Parser/parser.cc"
    break;

  case 56:
#line 688 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) );
		}
#line 7111 "Parser/parser.cc"
    break;

  case 57:
#line 697 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7117 "Parser/parser.cc"
    break;

  case 60:
#line 704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7123 "Parser/parser.cc"
    break;

  case 61:
#line 709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Default parameter for argument is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7129 "Parser/parser.cc"
    break;

  case 64:
#line 716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7135 "Parser/parser.cc"
    break;

  case 66:
#line 722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-1].tok) ) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7141 "Parser/parser.cc"
    break;

  case 67:
#line 724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-3].tok) ) ), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7147 "Parser/parser.cc"
    break;

  case 68:
#line 726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7153 "Parser/parser.cc"
    break;

  case 69:
#line 728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7159 "Parser/parser.cc"
    break;

  case 70:
#line 730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7165 "Parser/parser.cc"
    break;

  case 71:
#line 732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7171 "Parser/parser.cc"
    break;

  case 72:
#line 737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_constantInteger( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 7177 "Parser/parser.cc"
    break;

  case 73:
#line 739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_field_name_FLOATINGconstant( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 7183 "Parser/parser.cc"
    break;

  case 74:
#line 741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_varref( (yyvsp[-1].tok) ), (yyvsp[0].en) ) );
		}
#line 7191 "Parser/parser.cc"
    break;

  case 75:
#line 748 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7197 "Parser/parser.cc"
    break;

  case 76:
#line 750 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Expression * constant = build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) );
			(yyval.en) = (yyvsp[-1].en) != nullptr ? new ExpressionNode( build_fieldSel( (yyvsp[-1].en),  constant ) ) : new ExpressionNode( constant );
		}
#line 7206 "Parser/parser.cc"
    break;

  case 79:
#line 762 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 7212 "Parser/parser.cc"
    break;

  case 80:
#line 764 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en)->set_extension( true ); }
#line 7218 "Parser/parser.cc"
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
#line 7238 "Parser/parser.cc"
    break;

  case 82:
#line 785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_val( (yyvsp[-1].op), (yyvsp[0].en) ) ); }
#line 7244 "Parser/parser.cc"
    break;

  case 83:
#line 787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Incr, (yyvsp[0].en) ) ); }
#line 7250 "Parser/parser.cc"
    break;

  case 84:
#line 789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Decr, (yyvsp[0].en) ) ); }
#line 7256 "Parser/parser.cc"
    break;

  case 85:
#line 791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7262 "Parser/parser.cc"
    break;

  case 86:
#line 793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 7268 "Parser/parser.cc"
    break;

  case 87:
#line 795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7274 "Parser/parser.cc"
    break;

  case 88:
#line 797 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 7280 "Parser/parser.cc"
    break;

  case 89:
#line 799 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_offsetOf( (yyvsp[-3].decl), build_varref( (yyvsp[-1].tok) ) ) ); }
#line 7286 "Parser/parser.cc"
    break;

  case 90:
#line 801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.en) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 7295 "Parser/parser.cc"
    break;

  case 91:
#line 808 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::PointTo; }
#line 7301 "Parser/parser.cc"
    break;

  case 92:
#line 809 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AddressOf; }
#line 7307 "Parser/parser.cc"
    break;

  case 93:
#line 811 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::And; }
#line 7313 "Parser/parser.cc"
    break;

  case 94:
#line 815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::UnPlus; }
#line 7319 "Parser/parser.cc"
    break;

  case 95:
#line 816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::UnMinus; }
#line 7325 "Parser/parser.cc"
    break;

  case 96:
#line 817 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::Neg; }
#line 7331 "Parser/parser.cc"
    break;

  case 97:
#line 818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::BitNeg; }
#line 7337 "Parser/parser.cc"
    break;

  case 99:
#line 824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cast( (yyvsp[-2].decl), (yyvsp[0].en) ) ); }
#line 7343 "Parser/parser.cc"
    break;

  case 100:
#line 826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 7349 "Parser/parser.cc"
    break;

  case 101:
#line 828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 7355 "Parser/parser.cc"
    break;

  case 102:
#line 830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( nullptr ) ) ); }
#line 7361 "Parser/parser.cc"
    break;

  case 103:
#line 832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 7367 "Parser/parser.cc"
    break;

  case 104:
#line 834 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Return cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7373 "Parser/parser.cc"
    break;

  case 105:
#line 836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7379 "Parser/parser.cc"
    break;

  case 106:
#line 838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7385 "Parser/parser.cc"
    break;

  case 114:
#line 858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Exp, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7391 "Parser/parser.cc"
    break;

  case 116:
#line 864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mul, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7397 "Parser/parser.cc"
    break;

  case 117:
#line 866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Div, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7403 "Parser/parser.cc"
    break;

  case 118:
#line 868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mod, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7409 "Parser/parser.cc"
    break;

  case 120:
#line 874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Plus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7415 "Parser/parser.cc"
    break;

  case 121:
#line 876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Minus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7421 "Parser/parser.cc"
    break;

  case 123:
#line 882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7427 "Parser/parser.cc"
    break;

  case 124:
#line 884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::RShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7433 "Parser/parser.cc"
    break;

  case 126:
#line 890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7439 "Parser/parser.cc"
    break;

  case 127:
#line 892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7445 "Parser/parser.cc"
    break;

  case 128:
#line 894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7451 "Parser/parser.cc"
    break;

  case 129:
#line 896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7457 "Parser/parser.cc"
    break;

  case 131:
#line 902 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Eq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7463 "Parser/parser.cc"
    break;

  case 132:
#line 904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Neq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7469 "Parser/parser.cc"
    break;

  case 134:
#line 910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitAnd, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7475 "Parser/parser.cc"
    break;

  case 136:
#line 916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Xor, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7481 "Parser/parser.cc"
    break;

  case 138:
#line 922 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitOr, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7487 "Parser/parser.cc"
    break;

  case 140:
#line 928 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), true ) ); }
#line 7493 "Parser/parser.cc"
    break;

  case 142:
#line 934 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), false ) ); }
#line 7499 "Parser/parser.cc"
    break;

  case 144:
#line 940 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7505 "Parser/parser.cc"
    break;

  case 145:
#line 943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-3].en), (yyvsp[-3].en), (yyvsp[0].en) ) ); }
#line 7511 "Parser/parser.cc"
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
#line 7523 "Parser/parser.cc"
    break;

  case 149:
#line 962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7529 "Parser/parser.cc"
    break;

  case 150:
#line 967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7535 "Parser/parser.cc"
    break;

  case 154:
#line 977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::Assign; }
#line 7541 "Parser/parser.cc"
    break;

  case 155:
#line 978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AtAssn; }
#line 7547 "Parser/parser.cc"
    break;

  case 156:
#line 982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ExpAssn; }
#line 7553 "Parser/parser.cc"
    break;

  case 157:
#line 983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MulAssn; }
#line 7559 "Parser/parser.cc"
    break;

  case 158:
#line 984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::DivAssn; }
#line 7565 "Parser/parser.cc"
    break;

  case 159:
#line 985 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ModAssn; }
#line 7571 "Parser/parser.cc"
    break;

  case 160:
#line 986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::PlusAssn; }
#line 7577 "Parser/parser.cc"
    break;

  case 161:
#line 987 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MinusAssn; }
#line 7583 "Parser/parser.cc"
    break;

  case 162:
#line 988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::LSAssn; }
#line 7589 "Parser/parser.cc"
    break;

  case 163:
#line 989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::RSAssn; }
#line 7595 "Parser/parser.cc"
    break;

  case 164:
#line 990 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AndAssn; }
#line 7601 "Parser/parser.cc"
    break;

  case 165:
#line 991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ERAssn; }
#line 7607 "Parser/parser.cc"
    break;

  case 166:
#line 992 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::OrAssn; }
#line 7613 "Parser/parser.cc"
    break;

  case 167:
#line 1003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)(new ExpressionNode( nullptr ) )->set_last( (yyvsp[-1].en) ) ) ); }
#line 7619 "Parser/parser.cc"
    break;

  case 168:
#line 1005 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-4].en)->set_last( (yyvsp[-1].en) ) ) )); }
#line 7625 "Parser/parser.cc"
    break;

  case 170:
#line 1011 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7631 "Parser/parser.cc"
    break;

  case 171:
#line 1013 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7637 "Parser/parser.cc"
    break;

  case 172:
#line 1015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7643 "Parser/parser.cc"
    break;

  case 174:
#line 1021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new CommaExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7649 "Parser/parser.cc"
    break;

  case 175:
#line 1026 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7655 "Parser/parser.cc"
    break;

  case 187:
#line 1044 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 7661 "Parser/parser.cc"
    break;

  case 189:
#line 1047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_directive( (yyvsp[0].tok) ) ); }
#line 7667 "Parser/parser.cc"
    break;

  case 190:
#line 1053 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[0].sn)->add_label( (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 7673 "Parser/parser.cc"
    break;

  case 191:
#line 1058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (StatementNode *)0 ) ); }
#line 7679 "Parser/parser.cc"
    break;

  case 192:
#line 1063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (yyvsp[-2].sn) ) ); }
#line 7685 "Parser/parser.cc"
    break;

  case 194:
#line 1069 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 7691 "Parser/parser.cc"
    break;

  case 195:
#line 1074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7697 "Parser/parser.cc"
    break;

  case 196:
#line 1076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7703 "Parser/parser.cc"
    break;

  case 197:
#line 1078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7709 "Parser/parser.cc"
    break;

  case 198:
#line 1080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7715 "Parser/parser.cc"
    break;

  case 201:
#line 1087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 7721 "Parser/parser.cc"
    break;

  case 202:
#line 1092 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_expr( (yyvsp[-1].en) ) ); }
#line 7727 "Parser/parser.cc"
    break;

  case 203:
#line 1094 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_mutex( nullptr, new StatementNode( build_expr( (yyvsp[-1].en) ) ) ) ); }
#line 7733 "Parser/parser.cc"
    break;

  case 204:
#line 1102 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn); }
#line 7739 "Parser/parser.cc"
    break;

  case 205:
#line 1104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( true, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 7745 "Parser/parser.cc"
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
#line 7759 "Parser/parser.cc"
    break;

  case 207:
#line 1116 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( false, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 7765 "Parser/parser.cc"
    break;

  case 208:
#line 1118 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( false, (yyvsp[-7].en), (yyvsp[-2].sn) ) );
			(yyval.sn) = (yyvsp[-3].decl) ? new StatementNode( build_compound( (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 7774 "Parser/parser.cc"
    break;

  case 209:
#line 1127 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-2].ifctl), maybe_build_compound( (yyvsp[0].sn) ), nullptr ) ); }
#line 7780 "Parser/parser.cc"
    break;

  case 210:
#line 1129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-4].ifctl), maybe_build_compound( (yyvsp[-2].sn) ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 7786 "Parser/parser.cc"
    break;

  case 211:
#line 1134 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new IfCtrl( nullptr, (yyvsp[0].en) ); }
#line 7792 "Parser/parser.cc"
    break;

  case 212:
#line 1136 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new IfCtrl( (yyvsp[0].decl), nullptr ); }
#line 7798 "Parser/parser.cc"
    break;

  case 213:
#line 1138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new IfCtrl( (yyvsp[0].decl), nullptr ); }
#line 7804 "Parser/parser.cc"
    break;

  case 214:
#line 1140 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new IfCtrl( (yyvsp[-1].decl), (yyvsp[0].en) ); }
#line 7810 "Parser/parser.cc"
    break;

  case 215:
#line 1147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = (yyvsp[0].en); }
#line 7816 "Parser/parser.cc"
    break;

  case 216:
#line 1149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7822 "Parser/parser.cc"
    break;

  case 218:
#line 1154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.sn) = new StatementNode( build_case( (yyvsp[0].en) ) ); }
#line 7828 "Parser/parser.cc"
    break;

  case 219:
#line 1156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)((yyvsp[-2].sn)->set_last( new StatementNode( build_case( (yyvsp[0].en) ) ) ) ); }
#line 7834 "Parser/parser.cc"
    break;

  case 220:
#line 1160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn); }
#line 7840 "Parser/parser.cc"
    break;

  case 221:
#line 1161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.sn) = new StatementNode( build_default() ); }
#line 7846 "Parser/parser.cc"
    break;

  case 223:
#line 1173 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)( (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) )); }
#line 7852 "Parser/parser.cc"
    break;

  case 224:
#line 1177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( maybe_build_compound( (yyvsp[0].sn) ) ); }
#line 7858 "Parser/parser.cc"
    break;

  case 225:
#line 1182 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = nullptr; }
#line 7864 "Parser/parser.cc"
    break;

  case 227:
#line 1188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ); }
#line 7870 "Parser/parser.cc"
    break;

  case 228:
#line 1190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)( (yyvsp[-2].sn)->set_last( (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ) ) ); }
#line 7876 "Parser/parser.cc"
    break;

  case 229:
#line 1195 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( (yyvsp[-3].ifctl), maybe_build_compound( (yyvsp[-1].sn) ) ) ); }
#line 7882 "Parser/parser.cc"
    break;

  case 230:
#line 1197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( new IfCtrl( nullptr, new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 7888 "Parser/parser.cc"
    break;

  case 231:
#line 1199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( (yyvsp[-2].en), maybe_build_compound( (yyvsp[-5].sn) ) ) ); }
#line 7894 "Parser/parser.cc"
    break;

  case 232:
#line 1201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( new ExpressionNode( build_constantInteger( *new string( "1" ) ) ), maybe_build_compound( (yyvsp[-4].sn) ) ) ); }
#line 7900 "Parser/parser.cc"
    break;

  case 233:
#line 1203 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( (yyvsp[-3].fctl), maybe_build_compound( (yyvsp[-1].sn) ) ) ); }
#line 7906 "Parser/parser.cc"
    break;

  case 234:
#line 1205 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( new ForCtrl( (ExpressionNode * )nullptr, (ExpressionNode * )nullptr, (ExpressionNode * )nullptr ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 7912 "Parser/parser.cc"
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
#line 7931 "Parser/parser.cc"
    break;

  case 237:
#line 1233 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (ExpressionNode * )nullptr, (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 7937 "Parser/parser.cc"
    break;

  case 238:
#line 1235 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 7943 "Parser/parser.cc"
    break;

  case 239:
#line 1237 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-3].decl), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 7949 "Parser/parser.cc"
    break;

  case 240:
#line 1240 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7956 "Parser/parser.cc"
    break;

  case 241:
#line 1243 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LEThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7963 "Parser/parser.cc"
    break;

  case 242:
#line 1246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].en)->clone(), (yyvsp[-1].compop), (yyvsp[0].en), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7969 "Parser/parser.cc"
    break;

  case 243:
#line 1248 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 7975 "Parser/parser.cc"
    break;

  case 244:
#line 1250 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( new ExpressionNode( build_constantInteger( *new string( "0u" ) ) ), (yyvsp[-1].en), nullptr, OperKinds::LThan, nullptr, nullptr ); }
#line 7981 "Parser/parser.cc"
    break;

  case 245:
#line 1252 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-2].en), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7988 "Parser/parser.cc"
    break;

  case 246:
#line 1255 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-3].en), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LEThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7995 "Parser/parser.cc"
    break;

  case 247:
#line 1258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), (yyvsp[-1].compop), (yyvsp[0].en), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8001 "Parser/parser.cc"
    break;

  case 248:
#line 1260 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8007 "Parser/parser.cc"
    break;

  case 249:
#line 1263 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Array interator is currently unimplemented." ); (yyval.fctl) = nullptr;
			(yyval.fctl) = forCtrl( new ExpressionNode( build_varref( (yyvsp[0].tok) ) ), (yyvsp[-2].en), nullptr, OperKinds::Range, nullptr, nullptr );
		}
#line 8016 "Parser/parser.cc"
    break;

  case 250:
#line 1270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), OperKinds::LThan, nullptr, new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8022 "Parser/parser.cc"
    break;

  case 251:
#line 1272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), OperKinds::GThan, nullptr, new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 8028 "Parser/parser.cc"
    break;

  case 252:
#line 1274 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), OperKinds::LThan, nullptr, (yyvsp[0].en) ); }
#line 8034 "Parser/parser.cc"
    break;

  case 253:
#line 1276 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), OperKinds::GThan, nullptr, (yyvsp[0].en) ); }
#line 8040 "Parser/parser.cc"
    break;

  case 254:
#line 1278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), OperKinds::LThan, nullptr, nullptr ); }
#line 8046 "Parser/parser.cc"
    break;

  case 255:
#line 1283 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LThan; }
#line 8052 "Parser/parser.cc"
    break;

  case 256:
#line 1285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 8058 "Parser/parser.cc"
    break;

  case 257:
#line 1287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GThan; }
#line 8064 "Parser/parser.cc"
    break;

  case 258:
#line 1289 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GEThan; }
#line 8070 "Parser/parser.cc"
    break;

  case 259:
#line 1294 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Goto ) ); }
#line 8076 "Parser/parser.cc"
    break;

  case 260:
#line 1298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_computedgoto( (yyvsp[-1].en) ) ); }
#line 8082 "Parser/parser.cc"
    break;

  case 261:
#line 1301 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThrough ) ); }
#line 8088 "Parser/parser.cc"
    break;

  case 262:
#line 1303 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::FallThrough ) ); }
#line 8094 "Parser/parser.cc"
    break;

  case 263:
#line 1305 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThroughDefault ) ); }
#line 8100 "Parser/parser.cc"
    break;

  case 264:
#line 1308 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Continue ) ); }
#line 8106 "Parser/parser.cc"
    break;

  case 265:
#line 1312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Continue ) ); }
#line 8112 "Parser/parser.cc"
    break;

  case 266:
#line 1315 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Break ) ); }
#line 8118 "Parser/parser.cc"
    break;

  case 267:
#line 1319 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Break ) ); }
#line 8124 "Parser/parser.cc"
    break;

  case 268:
#line 1321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_return( (yyvsp[-1].en) ) ); }
#line 8130 "Parser/parser.cc"
    break;

  case 269:
#line 1323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 8136 "Parser/parser.cc"
    break;

  case 270:
#line 1325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr ) ); }
#line 8142 "Parser/parser.cc"
    break;

  case 271:
#line 1327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn) ) ); }
#line 8148 "Parser/parser.cc"
    break;

  case 272:
#line 1329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Coroutine ) ); }
#line 8154 "Parser/parser.cc"
    break;

  case 273:
#line 1331 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Coroutine ) ); }
#line 8160 "Parser/parser.cc"
    break;

  case 274:
#line 1333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Generator ) ); }
#line 8166 "Parser/parser.cc"
    break;

  case 275:
#line 1335 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Generator ) ); }
#line 8172 "Parser/parser.cc"
    break;

  case 276:
#line 1337 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_throw( (yyvsp[-1].en) ) ); }
#line 8178 "Parser/parser.cc"
    break;

  case 277:
#line 1339 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume( (yyvsp[-1].en) ) ); }
#line 8184 "Parser/parser.cc"
    break;

  case 278:
#line 1341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume_at( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 8190 "Parser/parser.cc"
    break;

  case 281:
#line 1351 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_with( (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8196 "Parser/parser.cc"
    break;

  case 282:
#line 1357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_mutex( (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8202 "Parser/parser.cc"
    break;

  case 283:
#line 1361 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 8208 "Parser/parser.cc"
    break;

  case 284:
#line 1366 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8214 "Parser/parser.cc"
    break;

  case 286:
#line 1372 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 8220 "Parser/parser.cc"
    break;

  case 287:
#line 1376 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) )); }
#line 8226 "Parser/parser.cc"
    break;

  case 289:
#line 1383 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8232 "Parser/parser.cc"
    break;

  case 290:
#line 1387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 8238 "Parser/parser.cc"
    break;

  case 291:
#line 1392 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8244 "Parser/parser.cc"
    break;

  case 292:
#line 1394 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-3].en), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[-4].en), (yyvsp[0].wfs) ); }
#line 8250 "Parser/parser.cc"
    break;

  case 293:
#line 1396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8256 "Parser/parser.cc"
    break;

  case 294:
#line 1398 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( nullptr, maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8262 "Parser/parser.cc"
    break;

  case 295:
#line 1401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 8268 "Parser/parser.cc"
    break;

  case 296:
#line 1403 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-5].en), maybe_build_compound( (yyvsp[-4].sn) ), (yyvsp[-6].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8274 "Parser/parser.cc"
    break;

  case 297:
#line 1408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-1].en), (yyvsp[0].sn), (yyvsp[-2].en) ) ); }
#line 8280 "Parser/parser.cc"
    break;

  case 298:
#line 1410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-3].en), (yyvsp[-2].sn), (yyvsp[-4].en), (yyvsp[0].wfs) ) ); }
#line 8286 "Parser/parser.cc"
    break;

  case 299:
#line 1415 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), (yyvsp[0].sn), 0 ) ); }
#line 8292 "Parser/parser.cc"
    break;

  case 300:
#line 1417 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), 0, (yyvsp[0].sn) ) ); }
#line 8298 "Parser/parser.cc"
    break;

  case 301:
#line 1419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-2].sn), (yyvsp[-1].sn), (yyvsp[0].sn) ) ); }
#line 8304 "Parser/parser.cc"
    break;

  case 302:
#line 1424 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8310 "Parser/parser.cc"
    break;

  case 303:
#line 1426 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)(yyvsp[-8].sn)->set_last( new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ) ); }
#line 8316 "Parser/parser.cc"
    break;

  case 304:
#line 1431 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8322 "Parser/parser.cc"
    break;

  case 305:
#line 1432 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[0].en); }
#line 8328 "Parser/parser.cc"
    break;

  case 306:
#line 1436 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 8334 "Parser/parser.cc"
    break;

  case 307:
#line 1437 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 8340 "Parser/parser.cc"
    break;

  case 308:
#line 1438 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.catch_kind) = CatchStmt::Resume; }
#line 8346 "Parser/parser.cc"
    break;

  case 309:
#line 1439 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Resume; }
#line 8352 "Parser/parser.cc"
    break;

  case 310:
#line 1443 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = new StatementNode( build_finally( (yyvsp[0].sn) ) ); }
#line 8358 "Parser/parser.cc"
    break;

  case 312:
#line 1450 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 8364 "Parser/parser.cc"
    break;

  case 313:
#line 1452 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 8370 "Parser/parser.cc"
    break;

  case 314:
#line 1454 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 8376 "Parser/parser.cc"
    break;

  case 319:
#line 1469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-4].flag), (yyvsp[-2].constant), 0 ) ); }
#line 8382 "Parser/parser.cc"
    break;

  case 320:
#line 1471 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-6].flag), (yyvsp[-4].constant), (yyvsp[-2].en) ) ); }
#line 8388 "Parser/parser.cc"
    break;

  case 321:
#line 1473 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-8].flag), (yyvsp[-6].constant), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 8394 "Parser/parser.cc"
    break;

  case 322:
#line 1475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-10].flag), (yyvsp[-8].constant), (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 8400 "Parser/parser.cc"
    break;

  case 323:
#line 1477 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-12].flag), (yyvsp[-9].constant), 0, (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].label) ) ); }
#line 8406 "Parser/parser.cc"
    break;

  case 324:
#line 1482 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = false; }
#line 8412 "Parser/parser.cc"
    break;

  case 325:
#line 1484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = true; }
#line 8418 "Parser/parser.cc"
    break;

  case 326:
#line 1489 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8424 "Parser/parser.cc"
    break;

  case 329:
#line 1496 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 8430 "Parser/parser.cc"
    break;

  case 330:
#line 1501 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( nullptr, (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 8436 "Parser/parser.cc"
    break;

  case 331:
#line 1503 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( (yyvsp[-5].tok), (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 8442 "Parser/parser.cc"
    break;

  case 332:
#line 1508 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8448 "Parser/parser.cc"
    break;

  case 333:
#line 1510 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 8454 "Parser/parser.cc"
    break;

  case 334:
#line 1512 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( (yyvsp[0].constant) ) )); }
#line 8460 "Parser/parser.cc"
    break;

  case 335:
#line 1517 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = new LabelNode(); (yyval.label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 8469 "Parser/parser.cc"
    break;

  case 336:
#line 1522 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = (yyvsp[-2].label); (yyvsp[-2].label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 8478 "Parser/parser.cc"
    break;

  case 337:
#line 1532 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8484 "Parser/parser.cc"
    break;

  case 340:
#line 1539 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 8490 "Parser/parser.cc"
    break;

  case 341:
#line 1544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8496 "Parser/parser.cc"
    break;

  case 343:
#line 1550 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 8502 "Parser/parser.cc"
    break;

  case 344:
#line 1552 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-2].decl) ); }
#line 8508 "Parser/parser.cc"
    break;

  case 354:
#line 1578 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-4].en), (yyvsp[-2].constant) ); }
#line 8514 "Parser/parser.cc"
    break;

  case 355:
#line 1580 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-2].en), build_constantStr( *new string( "\"\"" ) ) ); }
#line 8520 "Parser/parser.cc"
    break;

  case 359:
#line 1598 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 8526 "Parser/parser.cc"
    break;

  case 361:
#line 1604 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].in) ); }
#line 8532 "Parser/parser.cc"
    break;

  case 362:
#line 1608 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 8538 "Parser/parser.cc"
    break;

  case 363:
#line 1610 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].in) ) ); }
#line 8544 "Parser/parser.cc"
    break;

  case 364:
#line 1617 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 8550 "Parser/parser.cc"
    break;

  case 365:
#line 1619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 8556 "Parser/parser.cc"
    break;

  case 366:
#line 1621 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 8562 "Parser/parser.cc"
    break;

  case 368:
#line 1627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 8568 "Parser/parser.cc"
    break;

  case 369:
#line 1629 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 8574 "Parser/parser.cc"
    break;

  case 370:
#line 1631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 8580 "Parser/parser.cc"
    break;

  case 371:
#line 1633 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeClone( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->appendList( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 8591 "Parser/parser.cc"
    break;

  case 372:
#line 1666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), 0 )->addQualifiers( (yyvsp[0].decl) ); }
#line 8597 "Parser/parser.cc"
    break;

  case 373:
#line 1668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), 0 )->addQualifiers( (yyvsp[0].decl) ); }
#line 8603 "Parser/parser.cc"
    break;

  case 374:
#line 1673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 8609 "Parser/parser.cc"
    break;

  case 375:
#line 1676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-6].decl)->appendList( (yyvsp[-2].decl) ) ); }
#line 8615 "Parser/parser.cc"
    break;

  case 376:
#line 1681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 8624 "Parser/parser.cc"
    break;

  case 377:
#line 1686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 8633 "Parser/parser.cc"
    break;

  case 378:
#line 1691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "3" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 8642 "Parser/parser.cc"
    break;

  case 379:
#line 1702 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "4" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef();
		}
#line 8651 "Parser/parser.cc"
    break;

  case 380:
#line 1707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "5" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 8660 "Parser/parser.cc"
    break;

  case 381:
#line 1712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "6" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[-3].decl) )->addTypedef();
		}
#line 8669 "Parser/parser.cc"
    break;

  case 382:
#line 1717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "7" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-2].decl) )->addTypedef();
		}
#line 8678 "Parser/parser.cc"
    break;

  case 383:
#line 1722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "8" );
			(yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-3].decl) )->addTypedef()->addType( (yyvsp[-3].decl) );
		}
#line 8687 "Parser/parser.cc"
    break;

  case 384:
#line 1731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 8695 "Parser/parser.cc"
    break;

  case 385:
#line 1735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 8703 "Parser/parser.cc"
    break;

  case 386:
#line 1742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 8709 "Parser/parser.cc"
    break;

  case 390:
#line 1752 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 8715 "Parser/parser.cc"
    break;

  case 391:
#line 1754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ) ); }
#line 8721 "Parser/parser.cc"
    break;

  case 404:
#line 1793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8727 "Parser/parser.cc"
    break;

  case 407:
#line 1805 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 8733 "Parser/parser.cc"
    break;

  case 410:
#line 1815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Const ); }
#line 8739 "Parser/parser.cc"
    break;

  case 411:
#line 1817 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Restrict ); }
#line 8745 "Parser/parser.cc"
    break;

  case 412:
#line 1819 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Volatile ); }
#line 8751 "Parser/parser.cc"
    break;

  case 413:
#line 1821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Atomic ); }
#line 8757 "Parser/parser.cc"
    break;

  case 415:
#line 1827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newForall( (yyvsp[-1].decl) ); }
#line 8763 "Parser/parser.cc"
    break;

  case 417:
#line 1833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 8769 "Parser/parser.cc"
    break;

  case 418:
#line 1835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 8775 "Parser/parser.cc"
    break;

  case 420:
#line 1846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 8781 "Parser/parser.cc"
    break;

  case 421:
#line 1851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Extern ); }
#line 8787 "Parser/parser.cc"
    break;

  case 422:
#line 1853 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Static ); }
#line 8793 "Parser/parser.cc"
    break;

  case 423:
#line 1855 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Auto ); }
#line 8799 "Parser/parser.cc"
    break;

  case 424:
#line 1857 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Register ); }
#line 8805 "Parser/parser.cc"
    break;

  case 425:
#line 1859 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Threadlocal ); }
#line 8811 "Parser/parser.cc"
    break;

  case 426:
#line 1862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Inline ); }
#line 8817 "Parser/parser.cc"
    break;

  case 427:
#line 1864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Fortran ); }
#line 8823 "Parser/parser.cc"
    break;

  case 428:
#line 1866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Noreturn ); }
#line 8829 "Parser/parser.cc"
    break;

  case 429:
#line 1871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 8835 "Parser/parser.cc"
    break;

  case 430:
#line 1873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Bool ); }
#line 8841 "Parser/parser.cc"
    break;

  case 431:
#line 1875 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Char ); }
#line 8847 "Parser/parser.cc"
    break;

  case 432:
#line 1877 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int ); }
#line 8853 "Parser/parser.cc"
    break;

  case 433:
#line 1879 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 ); }
#line 8859 "Parser/parser.cc"
    break;

  case 434:
#line 1881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 )->addType( DeclarationNode::newSignedNess( DeclarationNode::Unsigned ) ); }
#line 8865 "Parser/parser.cc"
    break;

  case 435:
#line 1883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Float ); }
#line 8871 "Parser/parser.cc"
    break;

  case 436:
#line 1885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Double ); }
#line 8877 "Parser/parser.cc"
    break;

  case 437:
#line 1887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat80 ); }
#line 8883 "Parser/parser.cc"
    break;

  case 438:
#line 1889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat128 ); }
#line 8889 "Parser/parser.cc"
    break;

  case 439:
#line 1891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat16 ); }
#line 8895 "Parser/parser.cc"
    break;

  case 440:
#line 1893 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32 ); }
#line 8901 "Parser/parser.cc"
    break;

  case 441:
#line 1895 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32x ); }
#line 8907 "Parser/parser.cc"
    break;

  case 442:
#line 1897 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64 ); }
#line 8913 "Parser/parser.cc"
    break;

  case 443:
#line 1899 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64x ); }
#line 8919 "Parser/parser.cc"
    break;

  case 444:
#line 1901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat128 ); }
#line 8925 "Parser/parser.cc"
    break;

  case 445:
#line 1903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 8931 "Parser/parser.cc"
    break;

  case 446:
#line 1905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 8937 "Parser/parser.cc"
    break;

  case 447:
#line 1907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 8943 "Parser/parser.cc"
    break;

  case 448:
#line 1909 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Complex ); }
#line 8949 "Parser/parser.cc"
    break;

  case 449:
#line 1911 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Imaginary ); }
#line 8955 "Parser/parser.cc"
    break;

  case 450:
#line 1913 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Signed ); }
#line 8961 "Parser/parser.cc"
    break;

  case 451:
#line 1915 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Unsigned ); }
#line 8967 "Parser/parser.cc"
    break;

  case 452:
#line 1917 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Short ); }
#line 8973 "Parser/parser.cc"
    break;

  case 453:
#line 1919 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Long ); }
#line 8979 "Parser/parser.cc"
    break;

  case 454:
#line 1921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Valist ); }
#line 8985 "Parser/parser.cc"
    break;

  case 455:
#line 1923 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::AutoType ); }
#line 8991 "Parser/parser.cc"
    break;

  case 457:
#line 1929 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8997 "Parser/parser.cc"
    break;

  case 459:
#line 1935 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVtableType( (yyvsp[-2].decl) ); }
#line 9003 "Parser/parser.cc"
    break;

  case 460:
#line 1941 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9009 "Parser/parser.cc"
    break;

  case 461:
#line 1943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9015 "Parser/parser.cc"
    break;

  case 463:
#line 1950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9021 "Parser/parser.cc"
    break;

  case 464:
#line 1952 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9027 "Parser/parser.cc"
    break;

  case 465:
#line 1954 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9033 "Parser/parser.cc"
    break;

  case 466:
#line 1956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 9039 "Parser/parser.cc"
    break;

  case 468:
#line 1963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9045 "Parser/parser.cc"
    break;

  case 470:
#line 1969 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9051 "Parser/parser.cc"
    break;

  case 471:
#line 1971 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9057 "Parser/parser.cc"
    break;

  case 472:
#line 1973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 9063 "Parser/parser.cc"
    break;

  case 473:
#line 1978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 9069 "Parser/parser.cc"
    break;

  case 474:
#line 1980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en) ); }
#line 9075 "Parser/parser.cc"
    break;

  case 475:
#line 1982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 9081 "Parser/parser.cc"
    break;

  case 476:
#line 1984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en), true ); }
#line 9087 "Parser/parser.cc"
    break;

  case 477:
#line 1986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Zero ); }
#line 9093 "Parser/parser.cc"
    break;

  case 478:
#line 1988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::One ); }
#line 9099 "Parser/parser.cc"
    break;

  case 480:
#line 1994 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9105 "Parser/parser.cc"
    break;

  case 481:
#line 1996 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9111 "Parser/parser.cc"
    break;

  case 482:
#line 1998 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9117 "Parser/parser.cc"
    break;

  case 484:
#line 2004 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 9123 "Parser/parser.cc"
    break;

  case 485:
#line 2006 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 9129 "Parser/parser.cc"
    break;

  case 486:
#line 2008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 9138 "Parser/parser.cc"
    break;

  case 488:
#line 2017 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9144 "Parser/parser.cc"
    break;

  case 489:
#line 2019 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9150 "Parser/parser.cc"
    break;

  case 490:
#line 2021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9156 "Parser/parser.cc"
    break;

  case 492:
#line 2027 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9162 "Parser/parser.cc"
    break;

  case 493:
#line 2029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9168 "Parser/parser.cc"
    break;

  case 495:
#line 2035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9174 "Parser/parser.cc"
    break;

  case 496:
#line 2037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9180 "Parser/parser.cc"
    break;

  case 497:
#line 2039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9186 "Parser/parser.cc"
    break;

  case 499:
#line 2045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9192 "Parser/parser.cc"
    break;

  case 500:
#line 2047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9198 "Parser/parser.cc"
    break;

  case 501:
#line 2052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypedef( (yyvsp[0].tok) ); }
#line 9204 "Parser/parser.cc"
    break;

  case 502:
#line 2054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 9210 "Parser/parser.cc"
    break;

  case 503:
#line 2056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 9216 "Parser/parser.cc"
    break;

  case 505:
#line 2059 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), (yyvsp[0].decl) ); }
#line 9222 "Parser/parser.cc"
    break;

  case 506:
#line 2061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), (yyvsp[0].decl) ); }
#line 9228 "Parser/parser.cc"
    break;

  case 507:
#line 2066 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[0].tok), nullptr ); }
#line 9234 "Parser/parser.cc"
    break;

  case 508:
#line 2068 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-2].tok), nullptr ); }
#line 9240 "Parser/parser.cc"
    break;

  case 509:
#line 2070 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 9246 "Parser/parser.cc"
    break;

  case 514:
#line 2085 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 9252 "Parser/parser.cc"
    break;

  case 515:
#line 2087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 9258 "Parser/parser.cc"
    break;

  case 516:
#line 2089 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 9267 "Parser/parser.cc"
    break;

  case 517:
#line 2094 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) ); }
#line 9273 "Parser/parser.cc"
    break;

  case 518:
#line 2096 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 9282 "Parser/parser.cc"
    break;

  case 519:
#line 2101 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypedef( (yyvsp[-5].tok) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 9291 "Parser/parser.cc"
    break;

  case 520:
#line 2106 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 9300 "Parser/parser.cc"
    break;

  case 521:
#line 2111 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeGen( (yyvsp[-5].tok), nullptr );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 9309 "Parser/parser.cc"
    break;

  case 523:
#line 2120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9315 "Parser/parser.cc"
    break;

  case 524:
#line 2122 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 9321 "Parser/parser.cc"
    break;

  case 525:
#line 2127 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 9331 "Parser/parser.cc"
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
#line 9346 "Parser/parser.cc"
    break;

  case 529:
#line 2152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Struct; }
#line 9352 "Parser/parser.cc"
    break;

  case 530:
#line 2154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Union; }
#line 9358 "Parser/parser.cc"
    break;

  case 531:
#line 2156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Exception; }
#line 9364 "Parser/parser.cc"
    break;

  case 532:
#line 2162 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 9370 "Parser/parser.cc"
    break;

  case 533:
#line 2164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 9376 "Parser/parser.cc"
    break;

  case 534:
#line 2166 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Generator; }
#line 9382 "Parser/parser.cc"
    break;

  case 535:
#line 2168 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor generator is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 9388 "Parser/parser.cc"
    break;

  case 536:
#line 2170 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Coroutine; }
#line 9394 "Parser/parser.cc"
    break;

  case 537:
#line 2172 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor coroutine is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 9400 "Parser/parser.cc"
    break;

  case 538:
#line 2174 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Thread; }
#line 9406 "Parser/parser.cc"
    break;

  case 539:
#line 2176 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor thread is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 9412 "Parser/parser.cc"
    break;

  case 540:
#line 2181 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9418 "Parser/parser.cc"
    break;

  case 541:
#line 2183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 9424 "Parser/parser.cc"
    break;

  case 542:
#line 2188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); }
#line 9430 "Parser/parser.cc"
    break;

  case 543:
#line 2190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 9436 "Parser/parser.cc"
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
#line 9449 "Parser/parser.cc"
    break;

  case 545:
#line 2201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9455 "Parser/parser.cc"
    break;

  case 548:
#line 2205 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 9461 "Parser/parser.cc"
    break;

  case 549:
#line 2207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 9467 "Parser/parser.cc"
    break;

  case 552:
#line 2214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9473 "Parser/parser.cc"
    break;

  case 554:
#line 2217 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 9479 "Parser/parser.cc"
    break;

  case 555:
#line 2222 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].en) ); }
#line 9485 "Parser/parser.cc"
    break;

  case 556:
#line 2225 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 9491 "Parser/parser.cc"
    break;

  case 557:
#line 2228 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 9497 "Parser/parser.cc"
    break;

  case 558:
#line 2233 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9503 "Parser/parser.cc"
    break;

  case 560:
#line 2236 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 9509 "Parser/parser.cc"
    break;

  case 562:
#line 2247 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 9515 "Parser/parser.cc"
    break;

  case 563:
#line 2249 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 9521 "Parser/parser.cc"
    break;

  case 565:
#line 2256 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 9527 "Parser/parser.cc"
    break;

  case 566:
#line 2261 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9533 "Parser/parser.cc"
    break;

  case 568:
#line 2267 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 9539 "Parser/parser.cc"
    break;

  case 569:
#line 2272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-4].decl) ); }
#line 9545 "Parser/parser.cc"
    break;

  case 570:
#line 2274 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); }
#line 9551 "Parser/parser.cc"
    break;

  case 571:
#line 2276 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].tok), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) ); }
#line 9557 "Parser/parser.cc"
    break;

  case 572:
#line 2279 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-4].decl)->name, (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 9563 "Parser/parser.cc"
    break;

  case 573:
#line 2281 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-6].decl)->storageClasses.val != 0 || (yyvsp[-6].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			SemanticError( yylloc, "Typed enumeration is currently unimplemented." ); (yyval.decl) = nullptr;
		}
#line 9572 "Parser/parser.cc"
    break;

  case 574:
#line 2286 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].decl)->storageClasses.val != 0 || (yyvsp[-4].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			typedefTable.makeTypedef( *(yyvsp[-1].tok) );
		}
#line 9581 "Parser/parser.cc"
    break;

  case 575:
#line 2291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typed enumeration is currently unimplemented." ); (yyval.decl) = nullptr;
		}
#line 9589 "Parser/parser.cc"
    break;

  case 576:
#line 2295 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-8].decl)->storageClasses.val != 0 || (yyvsp[-8].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			typedefTable.makeTypedef( *(yyvsp[-5].decl)->name );
			SemanticError( yylloc, "Typed enumeration is currently unimplemented." ); (yyval.decl) = nullptr;
		}
#line 9599 "Parser/parser.cc"
    break;

  case 578:
#line 2305 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); (yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), 0, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 9605 "Parser/parser.cc"
    break;

  case 579:
#line 2307 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].decl)->type->symbolic.name );	(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].decl)->type->symbolic.name, 0, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 9611 "Parser/parser.cc"
    break;

  case 580:
#line 2312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumConstant( (yyvsp[-1].tok), (yyvsp[0].en) ); }
#line 9617 "Parser/parser.cc"
    break;

  case 581:
#line 2314 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumConstant( new string("inline"), nullptr ); }
#line 9623 "Parser/parser.cc"
    break;

  case 582:
#line 2316 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( DeclarationNode::newEnumConstant( (yyvsp[-1].tok), (yyvsp[0].en) ) ); }
#line 9629 "Parser/parser.cc"
    break;

  case 583:
#line 2318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( DeclarationNode::newEnumConstant( new string("inline"), nullptr ) ); }
#line 9635 "Parser/parser.cc"
    break;

  case 584:
#line 2323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9641 "Parser/parser.cc"
    break;

  case 585:
#line 2327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].in)->get_expression(); }
#line 9647 "Parser/parser.cc"
    break;

  case 586:
#line 2332 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 9653 "Parser/parser.cc"
    break;

  case 587:
#line 2334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9659 "Parser/parser.cc"
    break;

  case 590:
#line 2338 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9665 "Parser/parser.cc"
    break;

  case 591:
#line 2340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 9671 "Parser/parser.cc"
    break;

  case 592:
#line 2342 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 9677 "Parser/parser.cc"
    break;

  case 594:
#line 2350 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9683 "Parser/parser.cc"
    break;

  case 595:
#line 2352 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9689 "Parser/parser.cc"
    break;

  case 596:
#line 2354 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-8].decl)->appendList( (yyvsp[-4].decl) )->appendList( (yyvsp[0].decl) ); }
#line 9695 "Parser/parser.cc"
    break;

  case 598:
#line 2360 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9701 "Parser/parser.cc"
    break;

  case 599:
#line 2365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9707 "Parser/parser.cc"
    break;

  case 600:
#line 2367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9713 "Parser/parser.cc"
    break;

  case 602:
#line 2370 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 9719 "Parser/parser.cc"
    break;

  case 605:
#line 2377 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9725 "Parser/parser.cc"
    break;

  case 606:
#line 2379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9731 "Parser/parser.cc"
    break;

  case 608:
#line 2388 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 9737 "Parser/parser.cc"
    break;

  case 609:
#line 2391 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 9743 "Parser/parser.cc"
    break;

  case 610:
#line 2393 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 9749 "Parser/parser.cc"
    break;

  case 615:
#line 2403 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9755 "Parser/parser.cc"
    break;

  case 617:
#line 2410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 9761 "Parser/parser.cc"
    break;

  case 618:
#line 2412 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 9767 "Parser/parser.cc"
    break;

  case 619:
#line 2417 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 9773 "Parser/parser.cc"
    break;

  case 620:
#line 2419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 9779 "Parser/parser.cc"
    break;

  case 621:
#line 2428 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 9785 "Parser/parser.cc"
    break;

  case 622:
#line 2430 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 9791 "Parser/parser.cc"
    break;

  case 628:
#line 2443 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 9797 "Parser/parser.cc"
    break;

  case 631:
#line 2453 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 9803 "Parser/parser.cc"
    break;

  case 632:
#line 2454 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (yyvsp[-1].op) == OperKinds::Assign ? (yyvsp[0].in) : (yyvsp[0].in)->set_maybeConstructed( false ); }
#line 9809 "Parser/parser.cc"
    break;

  case 633:
#line 2455 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.in) = new InitializerNode( true ); }
#line 9815 "Parser/parser.cc"
    break;

  case 634:
#line 2456 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 9821 "Parser/parser.cc"
    break;

  case 635:
#line 2460 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = new InitializerNode( (yyvsp[0].en) ); }
#line 9827 "Parser/parser.cc"
    break;

  case 636:
#line 2461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 9833 "Parser/parser.cc"
    break;

  case 637:
#line 2466 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 9839 "Parser/parser.cc"
    break;

  case 639:
#line 2468 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ); }
#line 9845 "Parser/parser.cc"
    break;

  case 640:
#line 2469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (InitializerNode *)( (yyvsp[-2].in)->set_last( (yyvsp[0].in) ) ); }
#line 9851 "Parser/parser.cc"
    break;

  case 641:
#line 2470 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.in) = (InitializerNode *)((yyvsp[-3].in)->set_last( (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ) )); }
#line 9857 "Parser/parser.cc"
    break;

  case 643:
#line 2486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[-1].tok) ) ); }
#line 9863 "Parser/parser.cc"
    break;

  case 645:
#line 2492 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-1].en)->set_last( (yyvsp[0].en) )); }
#line 9869 "Parser/parser.cc"
    break;

  case 646:
#line 2498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 9875 "Parser/parser.cc"
    break;

  case 647:
#line 2501 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 9881 "Parser/parser.cc"
    break;

  case 648:
#line 2503 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 9887 "Parser/parser.cc"
    break;

  case 649:
#line 2505 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-4].en) ), maybeMoveBuild<Expression>( (yyvsp[-2].en) ) ) ); }
#line 9893 "Parser/parser.cc"
    break;

  case 650:
#line 2507 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 9899 "Parser/parser.cc"
    break;

  case 652:
#line 2531 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl) ); }
#line 9905 "Parser/parser.cc"
    break;

  case 653:
#line 2536 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9911 "Parser/parser.cc"
    break;

  case 654:
#line 2538 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 9917 "Parser/parser.cc"
    break;

  case 655:
#line 2543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "9" );
			if ( (yyvsp[-1].tclass) == TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 9928 "Parser/parser.cc"
    break;

  case 656:
#line 2550 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 9934 "Parser/parser.cc"
    break;

  case 657:
#line 2552 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "9" ); }
#line 9940 "Parser/parser.cc"
    break;

  case 658:
#line 2554 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 9946 "Parser/parser.cc"
    break;

  case 659:
#line 2556 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "9" );
			(yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 9955 "Parser/parser.cc"
    break;

  case 660:
#line 2562 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 9961 "Parser/parser.cc"
    break;

  case 661:
#line 2567 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 9967 "Parser/parser.cc"
    break;

  case 662:
#line 2569 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 9973 "Parser/parser.cc"
    break;

  case 663:
#line 2571 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::DStype; }
#line 9979 "Parser/parser.cc"
    break;

  case 664:
#line 2575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 9985 "Parser/parser.cc"
    break;

  case 665:
#line 2580 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 9991 "Parser/parser.cc"
    break;

  case 666:
#line 2582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 9997 "Parser/parser.cc"
    break;

  case 667:
#line 2584 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ftype; }
#line 10003 "Parser/parser.cc"
    break;

  case 668:
#line 2586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 10009 "Parser/parser.cc"
    break;

  case 669:
#line 2591 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10015 "Parser/parser.cc"
    break;

  case 672:
#line 2598 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 10021 "Parser/parser.cc"
    break;

  case 673:
#line 2603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 10027 "Parser/parser.cc"
    break;

  case 674:
#line 2605 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 10033 "Parser/parser.cc"
    break;

  case 675:
#line 2612 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 10039 "Parser/parser.cc"
    break;

  case 677:
#line 2615 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 10045 "Parser/parser.cc"
    break;

  case 678:
#line 2617 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)( (yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 10051 "Parser/parser.cc"
    break;

  case 679:
#line 2622 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 10057 "Parser/parser.cc"
    break;

  case 680:
#line 2624 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10063 "Parser/parser.cc"
    break;

  case 681:
#line 2626 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 10069 "Parser/parser.cc"
    break;

  case 682:
#line 2631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 10075 "Parser/parser.cc"
    break;

  case 683:
#line 2633 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 10081 "Parser/parser.cc"
    break;

  case 684:
#line 2638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "10" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), 0 );
		}
#line 10090 "Parser/parser.cc"
    break;

  case 685:
#line 2643 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "11" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 10099 "Parser/parser.cc"
    break;

  case 686:
#line 2651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), 0 ); }
#line 10105 "Parser/parser.cc"
    break;

  case 687:
#line 2653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-8].tok), (yyvsp[-6].decl), (yyvsp[-2].decl) ); }
#line 10111 "Parser/parser.cc"
    break;

  case 689:
#line 2659 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl) ); }
#line 10117 "Parser/parser.cc"
    break;

  case 694:
#line 2671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 10123 "Parser/parser.cc"
    break;

  case 695:
#line 2676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10129 "Parser/parser.cc"
    break;

  case 696:
#line 2678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 10135 "Parser/parser.cc"
    break;

  case 698:
#line 2686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl);	}
#line 10141 "Parser/parser.cc"
    break;

  case 699:
#line 2691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10147 "Parser/parser.cc"
    break;

  case 700:
#line 2693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->appendList( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 10153 "Parser/parser.cc"
    break;

  case 701:
#line 2698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10159 "Parser/parser.cc"
    break;

  case 703:
#line 2703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 10165 "Parser/parser.cc"
    break;

  case 704:
#line 2707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 10171 "Parser/parser.cc"
    break;

  case 705:
#line 2712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( (yyvsp[0].tok) ) ) ); }
#line 10177 "Parser/parser.cc"
    break;

  case 708:
#line 2716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 10186 "Parser/parser.cc"
    break;

  case 709:
#line 2721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( false, (yyvsp[-2].constant), 0 ) ) ); }
#line 10192 "Parser/parser.cc"
    break;

  case 710:
#line 2723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = LinkageSpec::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 10201 "Parser/parser.cc"
    break;

  case 711:
#line 2728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10211 "Parser/parser.cc"
    break;

  case 712:
#line 2734 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 10220 "Parser/parser.cc"
    break;

  case 713:
#line 2739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10230 "Parser/parser.cc"
    break;

  case 714:
#line 2745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 10239 "Parser/parser.cc"
    break;

  case 715:
#line 2750 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10249 "Parser/parser.cc"
    break;

  case 716:
#line 2756 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.val) || (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 10258 "Parser/parser.cc"
    break;

  case 717:
#line 2761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10268 "Parser/parser.cc"
    break;

  case 719:
#line 2776 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].sn) ); }
#line 10274 "Parser/parser.cc"
    break;

  case 720:
#line 2778 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].sn) ); }
#line 10280 "Parser/parser.cc"
    break;

  case 721:
#line 2783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; forall = false; }
#line 10286 "Parser/parser.cc"
    break;

  case 722:
#line 2785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); forall = false; }
#line 10292 "Parser/parser.cc"
    break;

  case 723:
#line 2790 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10303 "Parser/parser.cc"
    break;

  case 724:
#line 2797 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 10312 "Parser/parser.cc"
    break;

  case 725:
#line 2802 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 10321 "Parser/parser.cc"
    break;

  case 726:
#line 2808 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 10327 "Parser/parser.cc"
    break;

  case 727:
#line 2811 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 10333 "Parser/parser.cc"
    break;

  case 728:
#line 2814 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10339 "Parser/parser.cc"
    break;

  case 729:
#line 2818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-4].decl) );
		}
#line 10348 "Parser/parser.cc"
    break;

  case 730:
#line 2824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10354 "Parser/parser.cc"
    break;

  case 731:
#line 2827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10360 "Parser/parser.cc"
    break;

  case 732:
#line 2830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 10366 "Parser/parser.cc"
    break;

  case 736:
#line 2841 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 10372 "Parser/parser.cc"
    break;

  case 737:
#line 2846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10378 "Parser/parser.cc"
    break;

  case 738:
#line 2848 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = (yyvsp[-2].constant);
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 10388 "Parser/parser.cc"
    break;

  case 739:
#line 2857 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10394 "Parser/parser.cc"
    break;

  case 742:
#line 2864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10400 "Parser/parser.cc"
    break;

  case 743:
#line 2869 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 10406 "Parser/parser.cc"
    break;

  case 745:
#line 2875 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10412 "Parser/parser.cc"
    break;

  case 746:
#line 2880 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10418 "Parser/parser.cc"
    break;

  case 747:
#line 2882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 10424 "Parser/parser.cc"
    break;

  case 748:
#line 2884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 10430 "Parser/parser.cc"
    break;

  case 753:
#line 2893 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 10436 "Parser/parser.cc"
    break;

  case 754:
#line 2895 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 10442 "Parser/parser.cc"
    break;

  case 755:
#line 2930 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 10448 "Parser/parser.cc"
    break;

  case 756:
#line 2932 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10454 "Parser/parser.cc"
    break;

  case 757:
#line 2937 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10460 "Parser/parser.cc"
    break;

  case 759:
#line 2940 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10466 "Parser/parser.cc"
    break;

  case 760:
#line 2942 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10472 "Parser/parser.cc"
    break;

  case 761:
#line 2947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10478 "Parser/parser.cc"
    break;

  case 762:
#line 2949 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10484 "Parser/parser.cc"
    break;

  case 763:
#line 2951 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10490 "Parser/parser.cc"
    break;

  case 764:
#line 2953 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10496 "Parser/parser.cc"
    break;

  case 765:
#line 2958 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 10502 "Parser/parser.cc"
    break;

  case 766:
#line 2960 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10508 "Parser/parser.cc"
    break;

  case 767:
#line 2962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10514 "Parser/parser.cc"
    break;

  case 768:
#line 2964 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10520 "Parser/parser.cc"
    break;

  case 769:
#line 2966 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10526 "Parser/parser.cc"
    break;

  case 770:
#line 2968 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10532 "Parser/parser.cc"
    break;

  case 771:
#line 2970 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10538 "Parser/parser.cc"
    break;

  case 772:
#line 2975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10544 "Parser/parser.cc"
    break;

  case 773:
#line 2977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 10550 "Parser/parser.cc"
    break;

  case 774:
#line 2979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10556 "Parser/parser.cc"
    break;

  case 775:
#line 2981 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10562 "Parser/parser.cc"
    break;

  case 776:
#line 2990 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10568 "Parser/parser.cc"
    break;

  case 778:
#line 2993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10574 "Parser/parser.cc"
    break;

  case 779:
#line 2998 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10580 "Parser/parser.cc"
    break;

  case 780:
#line 3000 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10586 "Parser/parser.cc"
    break;

  case 781:
#line 3002 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 10592 "Parser/parser.cc"
    break;

  case 782:
#line 3004 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10598 "Parser/parser.cc"
    break;

  case 783:
#line 3006 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10604 "Parser/parser.cc"
    break;

  case 784:
#line 3011 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10610 "Parser/parser.cc"
    break;

  case 785:
#line 3013 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10616 "Parser/parser.cc"
    break;

  case 786:
#line 3015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10622 "Parser/parser.cc"
    break;

  case 787:
#line 3017 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10628 "Parser/parser.cc"
    break;

  case 788:
#line 3022 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10634 "Parser/parser.cc"
    break;

  case 789:
#line 3024 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10640 "Parser/parser.cc"
    break;

  case 790:
#line 3026 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10646 "Parser/parser.cc"
    break;

  case 791:
#line 3028 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10652 "Parser/parser.cc"
    break;

  case 792:
#line 3030 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10658 "Parser/parser.cc"
    break;

  case 793:
#line 3032 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10664 "Parser/parser.cc"
    break;

  case 797:
#line 3050 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 10670 "Parser/parser.cc"
    break;

  case 798:
#line 3052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10676 "Parser/parser.cc"
    break;

  case 799:
#line 3054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 10682 "Parser/parser.cc"
    break;

  case 800:
#line 3056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10688 "Parser/parser.cc"
    break;

  case 801:
#line 3058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10694 "Parser/parser.cc"
    break;

  case 802:
#line 3063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10700 "Parser/parser.cc"
    break;

  case 803:
#line 3065 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10706 "Parser/parser.cc"
    break;

  case 804:
#line 3067 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10712 "Parser/parser.cc"
    break;

  case 805:
#line 3069 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10718 "Parser/parser.cc"
    break;

  case 806:
#line 3074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10724 "Parser/parser.cc"
    break;

  case 807:
#line 3076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10730 "Parser/parser.cc"
    break;

  case 808:
#line 3078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10736 "Parser/parser.cc"
    break;

  case 809:
#line 3080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10742 "Parser/parser.cc"
    break;

  case 810:
#line 3082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10748 "Parser/parser.cc"
    break;

  case 811:
#line 3084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10754 "Parser/parser.cc"
    break;

  case 812:
#line 3099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "ID" );
		}
#line 10763 "Parser/parser.cc"
    break;

  case 813:
#line 3104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10769 "Parser/parser.cc"
    break;

  case 814:
#line 3109 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10775 "Parser/parser.cc"
    break;

  case 816:
#line 3112 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10781 "Parser/parser.cc"
    break;

  case 817:
#line 3114 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10787 "Parser/parser.cc"
    break;

  case 818:
#line 3119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10793 "Parser/parser.cc"
    break;

  case 819:
#line 3121 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10799 "Parser/parser.cc"
    break;

  case 820:
#line 3123 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10805 "Parser/parser.cc"
    break;

  case 821:
#line 3125 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10811 "Parser/parser.cc"
    break;

  case 822:
#line 3130 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 10817 "Parser/parser.cc"
    break;

  case 823:
#line 3132 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10823 "Parser/parser.cc"
    break;

  case 824:
#line 3134 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10829 "Parser/parser.cc"
    break;

  case 825:
#line 3136 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10835 "Parser/parser.cc"
    break;

  case 826:
#line 3138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10841 "Parser/parser.cc"
    break;

  case 827:
#line 3140 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10847 "Parser/parser.cc"
    break;

  case 828:
#line 3142 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10853 "Parser/parser.cc"
    break;

  case 829:
#line 3147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10859 "Parser/parser.cc"
    break;

  case 830:
#line 3149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10865 "Parser/parser.cc"
    break;

  case 831:
#line 3151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 10871 "Parser/parser.cc"
    break;

  case 832:
#line 3153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10877 "Parser/parser.cc"
    break;

  case 833:
#line 3155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10883 "Parser/parser.cc"
    break;

  case 834:
#line 3165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10889 "Parser/parser.cc"
    break;

  case 835:
#line 3167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10895 "Parser/parser.cc"
    break;

  case 837:
#line 3170 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10901 "Parser/parser.cc"
    break;

  case 838:
#line 3172 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10907 "Parser/parser.cc"
    break;

  case 839:
#line 3177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10913 "Parser/parser.cc"
    break;

  case 840:
#line 3179 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10919 "Parser/parser.cc"
    break;

  case 841:
#line 3181 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10925 "Parser/parser.cc"
    break;

  case 842:
#line 3186 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 10931 "Parser/parser.cc"
    break;

  case 843:
#line 3188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10937 "Parser/parser.cc"
    break;

  case 844:
#line 3190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10943 "Parser/parser.cc"
    break;

  case 845:
#line 3192 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10949 "Parser/parser.cc"
    break;

  case 846:
#line 3197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10955 "Parser/parser.cc"
    break;

  case 847:
#line 3199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10961 "Parser/parser.cc"
    break;

  case 848:
#line 3201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10967 "Parser/parser.cc"
    break;

  case 849:
#line 3215 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10973 "Parser/parser.cc"
    break;

  case 850:
#line 3217 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10979 "Parser/parser.cc"
    break;

  case 852:
#line 3220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10985 "Parser/parser.cc"
    break;

  case 853:
#line 3222 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10991 "Parser/parser.cc"
    break;

  case 854:
#line 3227 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 10997 "Parser/parser.cc"
    break;

  case 855:
#line 3229 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11003 "Parser/parser.cc"
    break;

  case 856:
#line 3234 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11009 "Parser/parser.cc"
    break;

  case 857:
#line 3236 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11015 "Parser/parser.cc"
    break;

  case 858:
#line 3238 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11021 "Parser/parser.cc"
    break;

  case 859:
#line 3243 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11027 "Parser/parser.cc"
    break;

  case 860:
#line 3245 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11033 "Parser/parser.cc"
    break;

  case 861:
#line 3250 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11039 "Parser/parser.cc"
    break;

  case 862:
#line 3252 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11045 "Parser/parser.cc"
    break;

  case 864:
#line 3270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11051 "Parser/parser.cc"
    break;

  case 865:
#line 3272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11057 "Parser/parser.cc"
    break;

  case 866:
#line 3277 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( 0, (yyvsp[0].op) ); }
#line 11063 "Parser/parser.cc"
    break;

  case 867:
#line 3279 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 11069 "Parser/parser.cc"
    break;

  case 868:
#line 3281 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11075 "Parser/parser.cc"
    break;

  case 869:
#line 3283 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11081 "Parser/parser.cc"
    break;

  case 870:
#line 3285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11087 "Parser/parser.cc"
    break;

  case 872:
#line 3291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11093 "Parser/parser.cc"
    break;

  case 873:
#line 3293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11099 "Parser/parser.cc"
    break;

  case 874:
#line 3295 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11105 "Parser/parser.cc"
    break;

  case 875:
#line 3300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 11111 "Parser/parser.cc"
    break;

  case 876:
#line 3302 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11117 "Parser/parser.cc"
    break;

  case 877:
#line 3304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11123 "Parser/parser.cc"
    break;

  case 878:
#line 3310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false ); }
#line 11129 "Parser/parser.cc"
    break;

  case 879:
#line 3312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false )->addArray( (yyvsp[0].decl) ); }
#line 11135 "Parser/parser.cc"
    break;

  case 880:
#line 3314 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-4].en), 0, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].en), 0, false ) ); }
#line 11141 "Parser/parser.cc"
    break;

  case 882:
#line 3321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), 0, false ); }
#line 11147 "Parser/parser.cc"
    break;

  case 883:
#line 3323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 11153 "Parser/parser.cc"
    break;

  case 884:
#line 3325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newArray( (yyvsp[-2].en), 0, false ) ); }
#line 11159 "Parser/parser.cc"
    break;

  case 885:
#line 3327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 11165 "Parser/parser.cc"
    break;

  case 887:
#line 3362 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 11171 "Parser/parser.cc"
    break;

  case 888:
#line 3364 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11177 "Parser/parser.cc"
    break;

  case 889:
#line 3366 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11183 "Parser/parser.cc"
    break;

  case 890:
#line 3371 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].op) ); }
#line 11189 "Parser/parser.cc"
    break;

  case 891:
#line 3373 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 11195 "Parser/parser.cc"
    break;

  case 892:
#line 3375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 11201 "Parser/parser.cc"
    break;

  case 893:
#line 3377 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11207 "Parser/parser.cc"
    break;

  case 894:
#line 3379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11213 "Parser/parser.cc"
    break;

  case 896:
#line 3385 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11219 "Parser/parser.cc"
    break;

  case 897:
#line 3387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11225 "Parser/parser.cc"
    break;

  case 898:
#line 3389 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11231 "Parser/parser.cc"
    break;

  case 899:
#line 3394 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 11237 "Parser/parser.cc"
    break;

  case 900:
#line 3396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11243 "Parser/parser.cc"
    break;

  case 901:
#line 3398 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11249 "Parser/parser.cc"
    break;

  case 903:
#line 3405 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11255 "Parser/parser.cc"
    break;

  case 905:
#line 3416 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false ); }
#line 11261 "Parser/parser.cc"
    break;

  case 906:
#line 3419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 11267 "Parser/parser.cc"
    break;

  case 907:
#line 3421 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, (yyvsp[-2].decl), false ); }
#line 11273 "Parser/parser.cc"
    break;

  case 908:
#line 3424 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 11279 "Parser/parser.cc"
    break;

  case 909:
#line 3426 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 11285 "Parser/parser.cc"
    break;

  case 910:
#line 3428 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-4].decl), true ); }
#line 11291 "Parser/parser.cc"
    break;

  case 912:
#line 3443 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11297 "Parser/parser.cc"
    break;

  case 913:
#line 3445 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11303 "Parser/parser.cc"
    break;

  case 914:
#line 3450 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( 0, (yyvsp[0].op) ); }
#line 11309 "Parser/parser.cc"
    break;

  case 915:
#line 3452 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 11315 "Parser/parser.cc"
    break;

  case 916:
#line 3454 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11321 "Parser/parser.cc"
    break;

  case 917:
#line 3456 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11327 "Parser/parser.cc"
    break;

  case 918:
#line 3458 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11333 "Parser/parser.cc"
    break;

  case 920:
#line 3464 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11339 "Parser/parser.cc"
    break;

  case 921:
#line 3466 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11345 "Parser/parser.cc"
    break;

  case 922:
#line 3468 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11351 "Parser/parser.cc"
    break;

  case 923:
#line 3473 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11357 "Parser/parser.cc"
    break;

  case 924:
#line 3475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11363 "Parser/parser.cc"
    break;

  case 927:
#line 3485 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11369 "Parser/parser.cc"
    break;

  case 930:
#line 3496 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11375 "Parser/parser.cc"
    break;

  case 931:
#line 3498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11381 "Parser/parser.cc"
    break;

  case 932:
#line 3500 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11387 "Parser/parser.cc"
    break;

  case 933:
#line 3502 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11393 "Parser/parser.cc"
    break;

  case 934:
#line 3504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11399 "Parser/parser.cc"
    break;

  case 935:
#line 3506 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11405 "Parser/parser.cc"
    break;

  case 936:
#line 3513 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11411 "Parser/parser.cc"
    break;

  case 937:
#line 3515 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11417 "Parser/parser.cc"
    break;

  case 938:
#line 3517 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11423 "Parser/parser.cc"
    break;

  case 939:
#line 3519 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 11429 "Parser/parser.cc"
    break;

  case 940:
#line 3521 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11435 "Parser/parser.cc"
    break;

  case 941:
#line 3524 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11441 "Parser/parser.cc"
    break;

  case 942:
#line 3526 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11447 "Parser/parser.cc"
    break;

  case 943:
#line 3528 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11453 "Parser/parser.cc"
    break;

  case 944:
#line 3530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 11459 "Parser/parser.cc"
    break;

  case 945:
#line 3532 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11465 "Parser/parser.cc"
    break;

  case 946:
#line 3537 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 11471 "Parser/parser.cc"
    break;

  case 947:
#line 3539 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 11477 "Parser/parser.cc"
    break;

  case 948:
#line 3544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 11483 "Parser/parser.cc"
    break;

  case 949:
#line 3546 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl)->addQualifiers( (yyvsp[-4].decl) ), true ); }
#line 11489 "Parser/parser.cc"
    break;

  case 951:
#line 3573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11495 "Parser/parser.cc"
    break;

  case 955:
#line 3584 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11501 "Parser/parser.cc"
    break;

  case 956:
#line 3586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11507 "Parser/parser.cc"
    break;

  case 957:
#line 3588 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11513 "Parser/parser.cc"
    break;

  case 958:
#line 3590 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11519 "Parser/parser.cc"
    break;

  case 959:
#line 3592 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11525 "Parser/parser.cc"
    break;

  case 960:
#line 3594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11531 "Parser/parser.cc"
    break;

  case 961:
#line 3601 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11537 "Parser/parser.cc"
    break;

  case 962:
#line 3603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11543 "Parser/parser.cc"
    break;

  case 963:
#line 3605 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11549 "Parser/parser.cc"
    break;

  case 964:
#line 3607 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11555 "Parser/parser.cc"
    break;

  case 965:
#line 3609 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11561 "Parser/parser.cc"
    break;

  case 966:
#line 3611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11567 "Parser/parser.cc"
    break;

  case 967:
#line 3616 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 11573 "Parser/parser.cc"
    break;

  case 968:
#line 3618 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11579 "Parser/parser.cc"
    break;

  case 969:
#line 3620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11585 "Parser/parser.cc"
    break;

  case 970:
#line 3627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 11591 "Parser/parser.cc"
    break;

  case 971:
#line 3629 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 11597 "Parser/parser.cc"
    break;

  case 974:
#line 3653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 11603 "Parser/parser.cc"
    break;

  case 975:
#line 3655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 11609 "Parser/parser.cc"
    break;


#line 11613 "Parser/parser.cc"

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
