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
#define YYFINAL  143
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   18691

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  175
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  286
/* YYNRULES -- Number of rules.  */
#define YYNRULES  972
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  1981

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
       0,   530,   530,   534,   541,   542,   543,   544,   545,   549,
     550,   551,   552,   553,   554,   555,   559,   560,   561,   566,
     570,   571,   582,   584,   586,   590,   591,   593,   595,   597,
     599,   612,   613,   623,   628,   633,   634,   640,   646,   652,
     654,   656,   658,   660,   662,   664,   666,   668,   670,   672,
     674,   676,   678,   680,   682,   684,   694,   695,   696,   701,
     704,   708,   709,   713,   714,   716,   718,   720,   722,   724,
     729,   731,   733,   741,   742,   750,   753,   754,   756,   761,
     777,   779,   781,   783,   785,   787,   789,   791,   793,   801,
     802,   804,   808,   809,   810,   811,   815,   816,   818,   820,
     822,   824,   826,   828,   830,   837,   838,   839,   840,   844,
     845,   849,   850,   855,   856,   858,   860,   865,   866,   868,
     873,   874,   876,   881,   882,   884,   886,   888,   893,   894,
     896,   901,   902,   907,   908,   913,   914,   919,   920,   925,
     926,   931,   932,   935,   940,   945,   946,   954,   960,   961,
     965,   966,   970,   971,   975,   976,   977,   978,   979,   980,
     981,   982,   983,   984,   985,   995,   997,  1002,  1003,  1005,
    1007,  1012,  1013,  1019,  1020,  1026,  1027,  1028,  1029,  1030,
    1031,  1032,  1033,  1034,  1035,  1036,  1038,  1039,  1045,  1050,
    1052,  1060,  1061,  1066,  1068,  1070,  1072,  1074,  1078,  1079,
    1084,  1091,  1093,  1095,  1105,  1107,  1115,  1118,  1123,  1125,
    1127,  1129,  1137,  1138,  1140,  1144,  1146,  1150,  1151,  1162,
    1163,  1167,  1172,  1173,  1177,  1179,  1184,  1186,  1188,  1190,
    1192,  1194,  1199,  1200,  1222,  1224,  1226,  1229,  1232,  1235,
    1237,  1239,  1241,  1244,  1247,  1249,  1252,  1259,  1261,  1263,
    1265,  1267,  1272,  1274,  1276,  1278,  1283,  1285,  1290,  1292,
    1294,  1296,  1299,  1303,  1306,  1310,  1312,  1314,  1316,  1318,
    1320,  1322,  1324,  1326,  1328,  1330,  1335,  1336,  1340,  1348,
    1353,  1358,  1359,  1363,  1367,  1372,  1373,  1379,  1383,  1385,
    1387,  1389,  1392,  1394,  1399,  1401,  1406,  1408,  1410,  1415,
    1417,  1423,  1424,  1428,  1429,  1430,  1431,  1435,  1440,  1441,
    1443,  1445,  1447,  1451,  1455,  1456,  1460,  1462,  1464,  1466,
    1468,  1474,  1475,  1481,  1482,  1486,  1487,  1492,  1494,  1500,
    1501,  1503,  1508,  1513,  1524,  1525,  1529,  1530,  1536,  1537,
    1541,  1543,  1547,  1549,  1553,  1554,  1558,  1559,  1563,  1564,
    1565,  1569,  1571,  1586,  1587,  1588,  1589,  1591,  1595,  1597,
    1601,  1608,  1610,  1612,  1617,  1618,  1620,  1622,  1624,  1656,
    1659,  1664,  1666,  1672,  1677,  1682,  1693,  1698,  1703,  1708,
    1713,  1722,  1726,  1733,  1735,  1736,  1737,  1743,  1745,  1750,
    1751,  1752,  1761,  1762,  1763,  1767,  1768,  1769,  1778,  1779,
    1780,  1785,  1786,  1795,  1796,  1801,  1802,  1806,  1808,  1810,
    1812,  1814,  1818,  1823,  1824,  1826,  1836,  1837,  1842,  1844,
    1846,  1848,  1850,  1853,  1855,  1857,  1862,  1864,  1866,  1868,
    1870,  1872,  1874,  1876,  1878,  1880,  1882,  1884,  1886,  1888,
    1890,  1892,  1894,  1896,  1898,  1900,  1902,  1904,  1906,  1908,
    1910,  1912,  1914,  1916,  1921,  1922,  1926,  1933,  1934,  1940,
    1941,  1943,  1945,  1947,  1952,  1954,  1959,  1960,  1962,  1964,
    1969,  1971,  1973,  1975,  1977,  1979,  1984,  1985,  1987,  1989,
    1994,  1996,  1995,  1999,  2007,  2008,  2010,  2012,  2017,  2018,
    2020,  2025,  2026,  2028,  2030,  2035,  2036,  2038,  2043,  2045,
    2047,  2049,  2050,  2052,  2057,  2059,  2061,  2066,  2067,  2071,
    2072,  2077,  2076,  2081,  2080,  2088,  2087,  2098,  2097,  2107,
    2112,  2113,  2118,  2124,  2138,  2139,  2143,  2145,  2147,  2153,
    2155,  2157,  2159,  2161,  2163,  2165,  2167,  2173,  2174,  2179,
    2181,  2183,  2192,  2194,  2195,  2196,  2198,  2200,  2201,  2206,
    2207,  2208,  2213,  2215,  2218,  2225,  2226,  2227,  2233,  2238,
    2240,  2246,  2247,  2253,  2254,  2258,  2263,  2266,  2265,  2269,
    2272,  2278,  2277,  2286,  2292,  2296,  2298,  2303,  2305,  2307,
    2309,  2315,  2318,  2324,  2325,  2327,  2328,  2329,  2331,  2333,
    2340,  2341,  2343,  2345,  2350,  2351,  2357,  2358,  2360,  2361,
    2366,  2367,  2368,  2370,  2378,  2379,  2381,  2384,  2386,  2390,
    2391,  2392,  2394,  2396,  2401,  2403,  2408,  2410,  2419,  2421,
    2426,  2427,  2428,  2432,  2433,  2434,  2439,  2440,  2445,  2446,
    2447,  2448,  2452,  2453,  2458,  2459,  2460,  2461,  2462,  2476,
    2477,  2482,  2483,  2489,  2491,  2494,  2496,  2498,  2521,  2522,
    2528,  2529,  2535,  2534,  2544,  2543,  2547,  2553,  2559,  2560,
    2562,  2566,  2571,  2573,  2575,  2577,  2583,  2584,  2588,  2589,
    2594,  2596,  2603,  2605,  2606,  2608,  2613,  2615,  2617,  2622,
    2624,  2629,  2634,  2642,  2644,  2649,  2650,  2655,  2656,  2660,
    2661,  2662,  2667,  2669,  2675,  2677,  2682,  2684,  2690,  2691,
    2695,  2699,  2703,  2705,  2706,  2707,  2712,  2715,  2714,  2726,
    2725,  2737,  2736,  2748,  2747,  2761,  2767,  2769,  2775,  2776,
    2781,  2788,  2793,  2799,  2802,  2805,  2809,  2815,  2818,  2821,
    2826,  2827,  2828,  2832,  2838,  2839,  2849,  2850,  2854,  2855,
    2860,  2865,  2866,  2872,  2873,  2875,  2880,  2881,  2882,  2883,
    2884,  2886,  2921,  2923,  2928,  2930,  2931,  2933,  2938,  2940,
    2942,  2944,  2949,  2951,  2953,  2955,  2957,  2959,  2961,  2966,
    2968,  2970,  2972,  2981,  2983,  2984,  2989,  2991,  2993,  2995,
    2997,  3002,  3004,  3006,  3008,  3013,  3015,  3017,  3019,  3021,
    3023,  3035,  3036,  3037,  3041,  3043,  3045,  3047,  3049,  3054,
    3056,  3058,  3060,  3065,  3067,  3069,  3071,  3073,  3075,  3090,
    3095,  3100,  3102,  3103,  3105,  3110,  3112,  3114,  3116,  3121,
    3123,  3125,  3127,  3129,  3131,  3133,  3138,  3140,  3142,  3144,
    3146,  3156,  3158,  3160,  3161,  3163,  3168,  3170,  3172,  3177,
    3179,  3181,  3183,  3188,  3190,  3192,  3206,  3208,  3210,  3211,
    3213,  3218,  3220,  3225,  3227,  3229,  3234,  3236,  3241,  3243,
    3260,  3261,  3263,  3268,  3270,  3272,  3274,  3276,  3281,  3282,
    3284,  3286,  3291,  3293,  3295,  3301,  3303,  3305,  3308,  3312,
    3314,  3316,  3318,  3352,  3353,  3355,  3357,  3362,  3364,  3366,
    3368,  3370,  3375,  3376,  3378,  3380,  3385,  3387,  3389,  3395,
    3396,  3398,  3407,  3410,  3412,  3415,  3417,  3419,  3433,  3434,
    3436,  3441,  3443,  3445,  3447,  3449,  3454,  3455,  3457,  3459,
    3464,  3466,  3474,  3475,  3476,  3481,  3482,  3487,  3489,  3491,
    3493,  3495,  3497,  3504,  3506,  3508,  3510,  3512,  3515,  3517,
    3519,  3521,  3523,  3528,  3530,  3532,  3537,  3563,  3564,  3566,
    3570,  3571,  3575,  3577,  3579,  3581,  3583,  3585,  3592,  3594,
    3596,  3598,  3600,  3602,  3607,  3609,  3611,  3618,  3620,  3638,
    3640,  3645,  3646
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
     395,   396,   397,   398,   399,   400,   401,   402,   125,    40,
      64,    41,    46,    91,    93,    44,    58,   123,    96,    94,
      42,    38,    43,    45,    33,   126,    92,    47,    37,    60,
      62,   124,    63,    61,    59
};
# endif

#define YYPACT_NINF (-1687)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-853)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     168,  9427,   239,   254, 15170,   157, -1687, -1687, -1687, -1687,
   -1687, -1687, -1687, -1687, -1687, -1687, -1687,   156,   761,   165,
   -1687, -1687, -1687, -1687, -1687, -1687, -1687, -1687, -1687, -1687,
   -1687, -1687, -1687, -1687, -1687, -1687, -1687, -1687, -1687, -1687,
   -1687, -1687, -1687, -1687, -1687, -1687, -1687,   106,   367, -1687,
   -1687, -1687, -1687, -1687, -1687,  3098,  3098,   170,  9427,   183,
     281, -1687, -1687,   305, -1687, -1687, -1687, -1687, -1687, -1687,
   -1687, -1687, -1687,   777, -1687,   759,   364, -1687, -1687, -1687,
   -1687, 15020, -1687, -1687,   360,   461,   474,   407, -1687,  3098,
     461,   461,   461,   468,  4899,   648,   939, 10802, -1687, -1687,
   -1687, 14870,  1302, -1687, -1687, -1687,  2315,   654,  8137,  1023,
     757,  2315,   827,   520, -1687, -1687, -1687, -1687,   626, -1687,
   -1687, -1687, -1687,   561, -1687, -1687, -1687, -1687, -1687,   597,
     595,   626, -1687,   626,   625, -1687, -1687, -1687, 15726,  3098,
   -1687, -1687,  3098, -1687,  9427,   615, 15778, -1687, -1687,  4964,
   16790, -1687,   711,   711, -1687,  2271, -1687, -1687, -1687, -1687,
     303, 13480,  3415,   626, -1687, -1687, -1687, -1687, -1687, -1687,
     641, -1687,   624,   661,   679, -1687,   688, 18166, 14100,  4604,
     777,   414,   662,   687,   693,   720,   730,   740, -1687, -1687,
   15928, 10153,   744, -1687, 15313, -1687, -1687, -1687, -1687,   754,
   -1687, -1687,   750, -1687, 17374,   909, 17518, -1687,   779,  3098,
     595,   819,   832,   835,   838, -1687, -1687, -1687,  3318,  2786,
     843,   905,   201, -1687, -1687,   626,   626,    29,    57,   207,
      29, -1687,   626,   626, -1687,  3063, -1687, -1687,   857,   864,
     711,  7849, -1687, -1687, 15020, -1687, -1687,  2315, -1687,  1666,
     520,   859,   934,    57,  3098,   474, -1687, 12351, -1687,   711,
     711,   869,   934,    57,  3098, -1687, 12100, -1687, -1687,   711,
   -1687,   711, -1687,   769,  4822,  3098, -1687,  1476,   923, -1687,
   -1687, -1687, 15472,   595,   280, -1687, -1687, 16840, -1687,   905,
      16, -1687, 18166, 16790,  3684,  3063, -1687,   265, -1687, -1687,
   -1687, 15778,  3098,   903, -1687, -1687, -1687, -1687,  3098,  3187,
     369,   471, -1687,  3098,   624, -1687,   867,   626,   927, 15980,
     706, 13638, 13058,  2315,  2315, -1687,  2315,   711,  2315,   711,
   -1687, -1687,   626, -1687,   933, -1687, 16130, -1687, -1687, -1687,
   16182,   754, -1687,   943,   439,  2139,   958,   520,   967, -1687,
    2271,   930,   624,  2271,  1172, -1687,   976,  1029, 18238,  1007,
    1009, 18166, 18310,  1012, -1687, -1687, -1687, -1687, -1687, -1687,
   -1687, 18382, 18382, 13946,  1008,  3286, -1687, -1687, -1687, -1687,
    1018, -1687,  1020, -1687,   851, -1687, 18166, 18166, -1687,  1004,
     572,   516,  1042,   529,   908,  1011,  1028,  1025,  1077,    50,
   -1687,   554, -1687,  1060, -1687,   897,  4458, 14408, -1687, -1687,
     780,  1060, -1687, -1687,   608, -1687, -1687,  4604,  1079,  1083,
    1095,  1110,  1112,  1140, -1687, -1687,   300,  1091, -1687,   647,
    1091, -1687, -1687, 15726, -1687,  1014,  1139, 14562, -1687, -1687,
    5048,  4410,  1164, 13638,  1170,   528,   536, -1687, -1687, -1687,
   -1687, -1687,  3098,  5061, -1687, -1687, -1687, -1687, -1687, -1687,
    1148,  4168,  1008, 17374,  1174,  1177, -1687, -1687,  1185, 17518,
     612, -1687, -1687, -1687, 17590,  1204, -1687, -1687, -1687, -1687,
   -1687,  3318,   592,  1208,  1210,  1220,   636,  1223,  1230,  1231,
    2786, -1687, -1687,   626,  1235,   474,  1236, -1687, -1687,  1232,
   -1687, -1687,   595,   934, -1687, -1687, -1687,   595, -1687, -1687,
    3063, -1687, 14408, 14408, -1687,   711,  4964,  6310, 13480, -1687,
   -1687, -1687, -1687, -1687,   595,   934,    16, -1687, -1687,  2315,
    1234,   934,    57, -1687,   595,   934, -1687, 12243, -1687,   711,
     711, -1687, -1687,  1250,   479,  1257,   520,  1258, -1687, 16998,
   -1687,   667, -1687,  1321,  7652, -1687,  4964, 16341,  7849, -1687,
   15472, 18454, -1687, -1687, -1687, -1687, -1687,  3684,   680,  3063,
   -1687, 13480,   905, -1687,  1263, -1687,  1270, -1687, -1687, -1687,
   -1687, -1687,  2271, -1687, -1687,  1348,  4877, 16182, 10153, -1687,
   16393, -1687,   711,   711, -1687, -1687,   754, -1687,   793,  1271,
    1410, 18166,  1226,  1232,  1259, -1687,   626,   626, -1687,  1091,
   -1687, 15980, -1687, -1687, 17279,   711,   711, -1687,  4877,   626,
   -1687, 16647, -1687, -1687, 16130, -1687,   303,  1278,   129,  1277,
    2139,   765, 15778,   770, -1687, -1687, -1687, -1687, -1687, -1687,
     781, -1687,  1286,  1262, -1687, 14254, -1687, 16445, 16445, -1687,
   14254, -1687, 18166, 14254, -1687, -1687, 15524, 16445, 16445,   897,
    1389,  1463,   462,  1471, -1687,   784,  1288,  1022,  1293, -1687,
   17590, 18166, 17662,  1290,  1476,  1476, -1687,  1408, -1687, -1687,
   17734,  2105, 18166, 17734,  1476, -1687, -1687, 18166, 18166, 18166,
   18166, 18166, 18166, 18166, 18166, 18166, 18166, 18166, 18166, 18166,
   18166, 18166, 18166, 18166, 18166, 18166, 17806,  1274,   688,  4397,
   10153, -1687, -1687, -1687, -1687, -1687, -1687, -1687, -1687, -1687,
   -1687, -1687,  1292, 18166, -1687, -1687,   780,  1966, -1687, -1687,
     626,   626, -1687, -1687, 14408, -1687,   326,  1091, -1687,   795,
    1091, -1687, -1687, -1687,  1232, -1687, -1687,  1232, 18526, -1687,
   -1687, 10153,  1299,  1301,  2920,  1440,  3429,   361,  1259, -1687,
     626,   626,  1259,   386, -1687,   626,   626, 18166,  3098,  1031,
    1035,  1259,   209, 13006, 13006,  3098, -1687, -1687, 18166,  1185,
   -1687, 17374,  1310, -1687,  1287, -1687, -1687, -1687, -1687, -1687,
     804, -1687, 13006,  1476,  4964,  1476,   809,  1312,  1313,  1316,
     814,  1317,  1318,  1320,   429,  1091, -1687, -1687,   451,  1091,
   -1687, -1687, -1687,  4964,   688, -1687,  1091, 18526, -1687,   595,
   16998, -1687, -1687,   823,  1322,   825,  1330, -1687,  1324, -1687,
     595, -1687, -1687,   595,   934,  1324, -1687,   595,  1304,  1333,
    1334, -1687, -1687, 17279, -1687,  1341, -1687, -1687, -1687,  1476,
    3098,  9819,  1429,  1326, 17366, -1687,  1139, -1687, 13006,   828,
   -1687,  1324, -1687, 15778, 14408,  1325, -1687,  1325, -1687, -1687,
   -1687, -1687, 16130, -1687, 10315, 14716, -1687, 16998,  1350,  1352,
    1354, -1687,  7395,   626, -1687,  1226, -1687, -1687, -1687, -1687,
    1232, -1687, -1687, -1687,   711, -1687,  3764, -1687, -1687,   520,
    2168,  1360, -1687, 17518, -1687,  2139,  1278, -1687, -1687,  1353,
    1361,  1172, 17734, -1687,  1371,   364,  1369,  1375,  1376,  1373,
    1380, 18166,  1381,  1382,  1386, 10153, 18166, -1687, -1687,  1674,
   -1687, -1687, -1687, 18166, -1687,  1387,  1388, 17446,  1037, -1687,
   17734, -1687, -1687, -1687,  1991, -1687, -1687,   844, -1687, -1687,
   -1687,  1991, -1687, -1687,  1040,   216, -1687, -1687,  1004,  1004,
    1004,   572,   572,   516,   516,  1042,  1042,  1042,  1042,   529,
     529,   908,  1011,  1028,  1025,  1077, 18166,  1049, -1687,  1391,
    1991, -1687, -1687, 17374, -1687, 16998,  1392,  1393,  1394,  1966,
   -1687, -1687, -1687, -1687, -1687, -1687, -1687, -1687,  1232, -1687,
   -1687,  1232, 16998, 16998, -1687, -1687,  2920,   762,  1396,  1398,
    1399,  1404,  2570,  3429, -1687, -1687, -1687, -1687, -1687, -1687,
   -1687, -1687, -1687, -1687, -1687, -1687, -1687, -1687,  1402, -1687,
    1259, -1687, -1687, -1687, -1687, -1687, -1687, -1687, -1687,  1411,
    1412, -1687,   474,  1991,  1055,   120, -1687, -1687,  1417, -1687,
   17518, -1687, 18166, -1687, 17878, 13006, -1687, -1687, -1687,  1367,
     456,  1091, -1687,   466,  1091, -1687, -1687, -1687, -1687,  1232,
   -1687, -1687, -1687,  1232,   905,  1416,  1232, -1687, -1687, -1687,
   -1687, -1687, -1687, -1687,  1423, -1687, -1687,  1324, -1687,   595,
   -1687, -1687, -1687, -1687, -1687, 11432,  1424,  1425, -1687,   332,
   -1687,   341,   110,  9991,  1441, 12835,  1442,  1443,  1788,  2647,
    2773, 17950,  1446, -1687, -1687,  1449,  1450, -1687, -1687,   595,
   18166, 18166,  1587,  1448,   261, -1687,  1533,  1454,  1436, -1687,
   -1687, -1687,  9647, -1687, -1687, -1687, -1687, -1687,  2237, -1687,
   -1687, -1687,  1522, -1687, -1687, -1687,  1476, -1687, -1687, 11279,
   15020,  1456, -1687,  3098, -1687,  1439,  1461,  1464, -1687,  1061,
   -1687, -1687, -1687,  4964, -1687, -1687,  1445,  1447,   895, 15778,
     624,   624, -1687, -1687,  1008,  1139, 14562, -1687,  1060, -1687,
   10477, -1687,   484,  1091, -1687,   711,  8599, -1687, -1687,  2139,
     626,   626,   303,   129, -1687, -1687,  1278,  1469,  1474, -1687,
   -1687,   906,   362, 10153,  1476, -1687,   362, 15576,   362, -1687,
   18166, 18166, 18166, -1687, -1687, -1687, -1687, 18166, 18166,  1466,
   17374, -1687, -1687,  1472,   315, -1687,  3347, -1687, -1687,  1063,
   -1687,    81, -1687, 17734,  1066, -1687, 17590, -1687, -1687, 18166,
    1455,  1068,  1074,  1185, -1687,   491,  1091, -1687, -1687, 16998,
   16998, -1687, -1687,  1473,   507,  1091, -1687,   540,  2022,   626,
     626, -1687, -1687, 16998, 16998, -1687,  1478, -1687, 13480, 13480,
    1479,  1480,  1481,  1483, -1687,  1484, 18166, 18166,  1078,  1488,
   -1687, -1687, -1687, -1687, -1687, -1687,  1487, 18166, -1687, -1687,
   -1687,  1232, -1687, -1687, -1687,  1232, 16998, 16998,   474,   626,
    1082,  1492,  1498, -1687, -1687,  1499, 11585, 11738, 11891, 15778,
   16445, 16445,  1503, -1687,  1482,  1486,  2441, 12193, -1687,   342,
    3098, -1687, -1687,  3098, -1687, 17734,   350,   401, -1687, -1687,
   -1687, -1687, 18166,  1504,  1577,  1506,  1507, -1687,  1490, -1687,
    1494, 18166,  1497, 17374,  1508, 18166, 17590, 18166,  1090, -1687,
    1520,   163, -1687,    58,  1512, -1687, -1687,  1514, -1687,  1523,
   -1687,  1525,  1517, 12835,   493, 12632,   626,   415, -1687, -1687,
   -1687,  1521, -1687,  1529, -1687,  1530, -1687,  1518, -1687,  1541,
   -1687, -1687, -1687, -1687, 10639,  1548,  1549,  1551, -1687,  1555,
   -1687, -1687, -1687,  1232, 18166, 18166,  1139,  1553, -1687,  1278,
   -1687,  1554,   445, -1687,  1560, -1687, -1687, 15778, -1687,  1559,
    1573,   907, -1687,  1574, -1687, -1687, -1687, -1687, -1687, 17374,
    1185, 17590, -1687,  1614,  1991, -1687,  1614,  1614, -1687,  1991,
    4225,  4660, -1687, -1687,  1097, -1687, -1687, -1687,  1585,  1584,
   -1687, -1687, -1687,  1232, -1687, -1687,  1589,  1591,   626, -1687,
   -1687, -1687,  1232, -1687, -1687, -1687,  1594, -1687, -1687, -1687,
   -1687, -1687, -1687, -1687, -1687, -1687, -1687, -1687, -1687, -1687,
    1582, -1687, -1687, -1687, -1687,  1593,  1597,   626, -1687, 16998,
   16998, -1687, -1687, -1687, -1687, 18166, -1687, -1687,  1602, -1687,
    1503,  1503,  1503,   888,  1578,   527, -1687,  4738,   532, 14408,
   -1687, -1687, -1687,  3977, 18166,  4540,   537, -1687, -1687,    62,
    1595,  1595,  3098, -1687, -1687, 17147, -1687,   912, -1687, -1687,
   -1687, -1687,   922,  1605, 12835,  9991, 12835,  9180, -1687, -1687,
     548, -1687,  1185, -1687,   928,   936,   938, -1687, -1687, -1687,
   -1687,   595,  1090,  1606, -1687, -1687, 18166, -1687,  1607,   688,
    9991, -1687, -1687, -1687, -1687, 18166,  1653, -1687, 12835, -1687,
     626, 13480, -1687, -1687, 15778, -1687, -1687, -1687, -1687, -1687,
    1609, -1687, 16998, -1687, -1687,  1616, -1687,  1618,  1613,  1611,
    2139, -1687, -1687, -1687, -1687, 18166, -1687, 15576, 18166,  1185,
    1625,  1100, -1687,  1105, -1687,  1991, -1687,  1991, -1687, -1687,
   -1687, -1687, 16998,  1623,  1628, -1687, -1687, 16998, 16998,  1629,
    1630,  1116, 13164, 13322, -1687,  1632, -1687, -1687, -1687, -1687,
    1631,  1633,  1131, -1687, -1687, -1687, -1687,   888,  2044,   562,
   -1687, -1687, -1687, -1687,   626,   626, -1687, -1687, -1687,   574,
   -1687,   946,  3977,   513, -1687,  4540,   626, -1687, -1687, -1687,
   -1687, -1687, -1687, -1687, -1687, 12835,   402, 18022, -1687,  1454,
    1638, 18166,   360,  1635,   468, 12050, 15778, -1687, 18166, 18166,
     911,    15, -1687, 18166, -1687,  1643,   410, 12835, -1687, -1687,
    1634, -1687, -1687,  1619,   688,   428,  1644,  1647,  1093,  1706,
   -1687, -1687, -1687,  3098,  4964, -1687, -1687,  1645,  1646, -1687,
   -1687, -1687,  2139,  1278,  1654, -1687, -1687, -1687,  1656, -1687,
   -1687, -1687,  1135,  1143, -1687, -1687, -1687, -1687, -1687, -1687,
   -1687, -1687, -1687, -1687,  1657, -1687, -1687,  1667,  1670, -1687,
   -1687, -1687,  1675,  1676,  1677,  2044, -1687,   626, -1687, -1687,
   -1687, -1687, -1687,  1664,  4738, -1687, 18166,  1678, -1687, -1687,
   12461, -1687,  1658,   955, 12835,  1454, 13796,  1454,  1661, -1687,
   -1687, -1687, -1687,  8903, 18166, 12835,  9180,  1662,  1663, -1687,
   -1687, -1687, -1687, 16595, -1687,  1683,  1668,    21, 12835, -1687,
   18166, 17734,   320, -1687, -1687, -1687,  1689, -1687, -1687,  1278,
    1693, -1687, -1687, -1687, -1687,  1692,  1698,  1700, 13480,  1699,
   -1687, -1687,   542,  1091, -1687, -1687,   888, -1687,   126, -1687,
    1144, -1687, -1687, 10961, -1687, -1687, -1687,  1684, -1687, 18166,
    1705, 18166,   925,  1688,   105, -1687, -1687, 18166, -1687, 10961,
   16595, -1687,  4810, 16393,  1476,  1707, -1687,  1764,  1717,   551,
    1712, -1687,  1797, -1687,   966, 12835,  1720, 12835, 12835, -1687,
    1723, -1687, -1687, -1687, -1687, -1687, -1687, -1687, -1687,  1232,
   -1687, 18166, 18166, -1687,  1249, 11120, -1687, -1687, -1687, -1687,
    1454,  1725,  1728, 18166, 18166, 18166, -1687, -1687,  1249, -1687,
    1708,  2990,  3951, -1687, -1687, -1687,    21,  1719, 18166,  1710,
      21,    21, 12835, -1687, -1687, 18166,  1772,  1778, -1687, 16998,
   -1687, -1687, 12461, -1687,  1249, -1687,  1721,  1737,   208, -1687,
    1454, -1687,  1708, 18166,  1752,  3951,  1753,   688,  1759, -1687,
     586, -1687, -1687,   975,  1706,   154, -1687, -1687, 12717,  1763,
   12461, 18166, 18094, 18166,  1765,  1766, -1687,   595,   688,  1769,
   -1687,  1745,   688, -1687, -1687, 12835,  1848,  1773, -1687, -1687,
   12717,  1454, -1687,  1454,  1454, -1687,   595, -1687, -1687,  1173,
   18166, -1687,   984, -1687, 12835, -1687, -1687,   688,  1476,  1777,
    1749, -1687, -1687, -1687,   985, -1687, -1687,  1757,  1476, -1687,
   -1687
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,   401,     0,     2,   401,   418,   419,   420,   421,   422,
     423,   424,   425,   407,   409,   408,   410,     0,     0,     0,
     426,   428,   449,   429,   450,   432,   433,   447,   448,   427,
     445,   446,   430,   431,   434,   435,   436,   437,   438,   439,
     440,   441,   442,   443,   444,   451,   452,   736,   454,   527,
     528,   531,   533,   529,   535,     0,     0,     0,   401,     0,
       0,    16,   498,   504,     9,    10,    11,    12,    13,    14,
      15,   702,    91,     0,    18,     0,     2,    89,    90,    17,
     752,   401,   703,   350,     0,   353,   628,   355,   364,     0,
     354,   384,   385,     0,     0,     0,     0,   481,   403,   405,
     411,   401,   413,   416,   466,   453,   389,   459,   464,   390,
     476,   391,   491,   495,   501,   480,   507,   519,   736,   524,
     525,   508,   574,   356,   357,     3,   704,   715,   406,     0,
       0,   736,   774,   736,     2,   791,   792,   793,   401,     0,
     950,   951,     0,     1,   401,     0,   401,   373,   374,     0,
     481,   395,   396,   397,   707,     0,   530,   532,   534,   536,
       0,   401,     0,   737,   738,   526,   455,   621,   622,   620,
     681,   676,   666,     0,     0,   705,     0,     0,   401,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   499,   502,
     401,   401,     0,   952,   481,   781,   799,   956,   949,   947,
     954,   349,     0,   153,   634,   152,     0,   358,     0,     0,
       0,     0,     0,     0,     0,   348,   851,   852,     0,     0,
     383,   734,   736,   730,   755,   736,   736,   732,     2,   736,
     731,   812,   736,   736,   809,     0,   474,   475,     0,     0,
     401,   401,   418,     2,   401,   365,   404,   414,   467,     0,
     496,     0,   718,     2,     0,   628,   366,   481,   460,   477,
     492,     0,   718,     2,     0,   417,   461,   468,   469,   478,
     483,   493,   497,     0,   511,     0,   696,     2,     2,   716,
     773,   775,   401,     0,     2,     2,   960,   481,   963,   734,
     734,     3,     0,   481,     0,     0,   376,   736,   732,   731,
       2,   401,     0,     0,   662,   664,   663,   665,     0,     0,
     658,     0,   648,     0,   657,   668,     0,   736,     2,   401,
     971,   402,   401,   413,   392,   459,   393,   484,   394,   491,
     488,   509,   736,   510,     0,   609,   401,   610,   925,   926,
     401,   611,   613,   498,   504,     0,   575,   576,     0,   739,
       0,   679,   667,     0,   743,    20,     0,    19,     0,     0,
       0,     0,     0,     0,    22,    24,     4,     8,     5,     6,
       7,     0,     0,   401,     2,     0,    92,    93,    94,    95,
      76,    23,    77,    35,    75,    96,     0,     0,   111,   113,
     117,   120,   123,   128,   131,   133,   135,   137,   139,   141,
     144,     0,    25,     0,   505,     2,    96,   401,   145,   673,
     624,   495,   626,   672,     0,   623,   627,     0,     0,     0,
       0,     0,     0,     0,   753,   779,   736,   789,   797,   801,
     807,     2,   958,   401,   961,     2,    89,   401,     3,   608,
       0,   971,     0,   402,   459,   484,   491,     3,     3,   590,
     594,   604,   610,   611,     2,   782,   800,   948,     2,     2,
      22,     0,     2,   634,    23,     0,   632,   635,   969,     0,
       0,   641,   630,   629,     0,     0,   720,     2,     2,     2,
       2,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   758,   815,   736,     0,   628,     2,   754,   762,   878,
     756,   757,     0,   718,     2,   811,   819,     0,   813,   814,
       0,   379,   401,   401,   465,   402,     0,   481,   401,   953,
     957,   955,   482,   700,     0,   718,   734,   359,   367,   415,
       0,   718,     2,   700,     0,   718,   677,   462,   463,   479,
     494,   500,   503,   498,   504,   522,   523,     0,   678,   401,
     618,     0,   189,   342,   401,     3,     0,   481,   401,   717,
     401,     0,   361,     2,   362,   697,   381,     0,     0,     0,
       2,   401,   734,   700,     0,     2,     0,   661,   660,   659,
     654,   412,     0,   652,   669,   457,     0,   401,   401,   927,
     402,   398,   399,   400,   931,   922,   923,   929,     2,     2,
      90,     0,   887,   901,   971,   883,   736,   736,   892,   899,
     616,   401,   489,   612,   402,   485,   486,   490,     0,   736,
     937,   402,   942,   934,   401,   939,     0,   969,   581,     0,
       0,     0,   401,     0,   751,   750,   746,   748,   749,   747,
       0,   741,   744,     0,    21,   401,    83,   401,   401,    78,
     401,    85,     0,   401,    81,    82,   401,   401,   401,     2,
      92,    93,     0,     0,   171,     0,     0,   525,     0,   947,
       0,     0,     0,     0,     0,     0,    45,     0,    51,    52,
      56,     0,     0,    56,     0,    79,    80,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     401,   154,   155,   156,   157,   158,   159,   160,   161,   162,
     163,   164,   152,     0,   150,   151,     2,   863,   625,   860,
     736,   736,   868,   506,   401,   780,   736,   790,   798,   802,
     808,     2,   783,   785,   787,     2,   803,   805,     0,   959,
     962,   401,     0,     0,     2,    90,   887,   736,   971,   833,
     736,   736,   971,   736,   848,   736,   736,     3,   612,     0,
       0,   971,   971,   401,   401,     0,     2,   643,     0,   969,
     640,   970,     0,   636,     0,     2,   639,   642,   168,   167,
       0,     2,   401,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   736,   767,   771,   810,   736,   824,
     829,   759,   816,     0,     0,   387,   875,     0,   721,     0,
     401,   722,   380,     0,     0,     0,     0,   378,     2,   723,
       0,   363,   700,     0,   718,     2,   724,     0,     0,     0,
       0,   537,   597,   402,     3,     3,   601,   600,   794,     0,
       0,   401,   343,     0,   481,     3,    89,     3,   401,     0,
       3,     2,   656,   401,   401,   650,   649,   650,   458,   456,
     575,   933,   401,   938,   402,   401,   924,   401,     0,     0,
       0,   902,     0,   736,   972,   888,   889,   617,   885,   886,
     900,   928,   932,   930,   487,   522,     0,   936,   941,   578,
     970,     0,   152,     0,   577,     0,   969,   682,   680,     0,
       0,   743,    56,   706,     0,     2,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   401,     0,   110,   109,     0,
     106,   105,    26,     0,    27,     0,     0,     0,     0,     3,
      56,    41,    42,    49,     0,    48,    59,     0,    57,    60,
      44,     0,    43,    47,     0,     0,    40,   112,   114,   115,
     116,   118,   119,   121,   122,   126,   127,   124,   125,   129,
     130,   132,   134,   136,   138,   140,     0,     0,   352,     0,
       0,    28,     3,   634,   146,   401,     0,     0,     0,   864,
     865,   861,   862,   675,   674,     2,   784,   786,   788,     2,
     804,   806,   401,   401,   880,   879,     2,     0,     0,     0,
       0,     0,   736,   888,   836,   853,     2,   831,   839,   614,
     834,   835,   615,     2,   846,   856,   849,   850,     0,     3,
     971,   371,     2,   964,     2,   605,   606,   584,     3,     3,
       3,     3,   628,     0,   144,     0,     3,     3,     0,   637,
       0,   631,     0,   719,     0,   401,     3,   375,   377,     0,
     736,   768,   772,   736,   825,   830,     2,   760,   763,   765,
       2,   817,   820,   822,   734,     0,   876,     3,   726,     3,
     471,   470,   473,   472,     2,   701,   727,     2,   725,     0,
     701,   728,   537,   537,   537,   401,     0,     0,   619,     0,
     346,     0,     0,   401,     0,     2,     0,     0,     0,     0,
       0,   173,     0,   276,   277,     0,     0,   315,   314,     0,
     148,   148,   321,   498,   504,   187,     0,   174,     0,   197,
     175,   176,   401,   191,   177,   178,   179,   180,     0,   181,
     182,   282,     0,   183,   184,   185,     0,   186,   193,   481,
     401,     0,   195,     0,   340,     0,     0,     0,     3,     0,
     701,   689,   690,     0,     3,   685,     3,     3,     0,   401,
     666,   666,   935,   940,     2,    89,   401,     3,   496,     3,
     402,     3,   736,   895,   898,   401,     3,   884,   890,     0,
     736,   736,     0,   581,   566,   582,   969,     0,     2,   740,
     742,     0,    84,   401,     0,    88,    86,   401,     0,   100,
       0,     0,     0,   104,   108,   107,   172,     0,     0,     0,
     634,    97,   165,     0,     0,    73,     0,    73,    73,     0,
      61,    63,    39,     0,     0,    37,     0,    38,   143,     0,
       0,     0,     0,   969,     3,   736,   871,   874,   866,   401,
     401,     3,     3,     0,   736,   842,   845,   736,     0,   736,
     736,   837,   854,   401,   401,   965,     0,   607,   401,   401,
       0,     0,     0,     0,   360,     3,     0,     0,     0,     0,
     633,   638,     3,   170,   169,     3,     0,     0,     2,   761,
     764,   766,     2,   818,   821,   823,   401,   401,   628,   736,
       0,     0,     0,   701,   729,     0,   401,   401,   401,   401,
     401,   401,   520,   548,     3,     3,   549,   481,   538,     0,
       0,   776,     2,     0,   344,    56,     0,     0,   267,   268,
     194,   196,     0,     0,     0,     2,     2,   263,     0,   261,
       0,     0,     0,   634,     0,     0,     0,     0,     0,   149,
       0,     0,   322,     0,     0,     3,   200,     0,   192,     0,
     258,     0,     0,     2,     0,   481,   736,     0,   341,   882,
     881,     0,     2,     0,   692,     2,   687,     0,   688,     0,
     670,   651,   655,   653,   401,     0,     0,     0,     3,     0,
       2,   891,   893,   894,     0,     0,    89,     0,     3,   969,
     571,     0,   581,   579,     0,   569,   683,   401,   745,     0,
       0,     0,    31,     0,   101,   103,   102,    99,    98,   634,
     969,     0,    55,    70,     0,    64,    71,    72,    50,     0,
       0,     0,    58,    46,     0,   142,   351,    29,     0,     0,
       2,   867,   869,   870,     3,     3,     0,     0,   736,     2,
     838,   840,   841,     2,   855,   857,     0,   832,   847,     3,
       3,   966,     3,   592,   591,   595,   968,     2,     2,   967,
       0,     3,   733,   644,   645,     0,     0,   736,   382,   401,
     401,     3,     3,   388,   735,     0,   826,   710,     0,   712,
     520,   520,   520,   555,   525,     0,   561,   549,     0,   401,
     512,   547,   543,     0,     0,     0,     0,   550,   552,   736,
     563,   563,     0,   544,   559,   401,   347,     0,   271,   272,
     269,   270,     0,     0,     2,   401,     2,   401,   264,   262,
       0,   256,   969,   265,     0,     0,     0,   303,   304,   305,
     306,     0,   296,     0,   297,   273,     0,   274,     0,     0,
     401,   201,   190,   260,   259,     0,   294,   313,     2,   345,
     736,   401,   708,   671,   401,     2,     2,   943,   944,   945,
       0,   896,   401,     3,     3,     0,   904,     0,     0,     0,
       0,   580,   568,     3,    87,     0,    30,   401,     0,   969,
       0,     0,    74,     0,    62,     0,    68,     0,    66,    36,
     147,   872,   401,     0,     0,   777,   795,   401,   401,     0,
       0,     0,   401,   401,   647,     0,   368,   370,     3,     3,
       0,     0,     0,   714,   516,   518,   514,     0,   911,     0,
     556,   916,   558,   908,   736,   736,   542,   562,   546,     0,
     545,     0,     0,     0,   565,     0,   736,   539,   553,   564,
     554,   560,   599,   603,   602,     2,     0,     0,   227,   208,
       0,     0,   210,   355,   209,   481,   401,   231,     0,   173,
     237,     0,   232,   173,   257,     0,     0,     2,   280,   307,
       0,   298,     2,     0,     0,     0,     0,   285,     0,   281,
     188,   369,   686,     0,     0,   946,     3,     0,     0,   903,
     905,   570,     0,   969,     2,    34,    32,    33,     0,    53,
     166,    65,     0,     0,     3,   778,   796,     3,     3,   843,
     858,   372,     2,   589,     3,   588,   646,     0,     0,   769,
     827,   877,     0,     0,     0,   912,   913,   736,   541,   909,
     910,   540,   521,     0,     0,   279,     0,     0,     2,   219,
       2,   202,     0,     0,     2,   211,   481,   238,     0,   253,
     254,   255,   252,   241,     0,     2,   401,     0,     0,     2,
     204,   278,     2,   401,   275,     0,     0,   323,     2,   283,
       0,    56,     0,   295,   691,   693,     0,   906,   907,   969,
       0,   684,    54,    69,    67,     0,     0,     0,   401,     0,
     770,   828,   736,   919,   921,   914,     0,   551,   212,   215,
       0,   214,   218,   401,   221,   220,   229,     0,     3,   173,
     246,     0,   242,     0,   239,     3,   233,   173,   266,   401,
     401,     3,   308,   402,   312,     0,   316,     0,     0,     0,
     324,   325,   206,   286,     0,     2,     0,     2,     2,   897,
       0,   573,   873,   844,   859,   593,     2,   915,   917,   918,
     557,     0,     0,   217,   222,   401,   336,   228,   226,   234,
     243,   254,   252,     0,   173,     0,   230,   236,   222,     3,
     301,     0,   911,   309,   310,   311,   323,     0,     0,     0,
     323,     0,     2,   284,   291,     0,   288,   290,   572,   401,
     213,   216,     2,     3,   223,   337,   248,   247,   244,   235,
     240,     3,   301,     0,     0,   912,     0,     0,     0,   317,
       0,   326,   207,     0,   281,     0,     3,   198,   224,     0,
       2,     0,     0,     0,     0,     0,   302,     0,   329,     0,
     327,     0,   329,   287,   289,     2,     0,     0,   199,   203,
     225,   250,   251,   249,   245,   205,     0,   299,   330,     0,
       0,   318,     0,   292,     2,   920,   300,     0,     0,     0,
       0,   293,   331,   332,     0,   328,   319,     0,     0,   320,
     333
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1687,  5371,  5131, -1687,    -1,   657,  -171, -1687,  1558, -1687,
     347, -1687,  -662,   702,  -920, -1054, -1687,   109,  5524,  1849,
   -1687,  1796, -1687,  1279,    87,   660,   664,  -230,   669,  1238,
    1240,  1233,  1241,  1242, -1687,  -144,   -96,  7761,   830, -1687,
    -382, -1687, -1687,  -642,  2256, -1028,  1094, -1687,  1864, -1687,
     808,    18, -1687, -1687, -1687,   404,    90, -1687, -1686, -1318,
     282,    79, -1687, -1687, -1687,   194,   139, -1687, -1687, -1687,
   -1687,    40, -1643,   182, -1687, -1687,    42, -1687, -1687, -1687,
      56,   427,   430,   140, -1687, -1687, -1687, -1687,  -733, -1687,
      80,    32, -1687,   146, -1687,  -134, -1687, -1687, -1687,   831,
    -806,  -895, -1102, -1687,    14,    11,   260,  6865,  -872,  -864,
   -1687,  -278, -1687,    19,  -139,    39,   -16,  -215,  3430,  6401,
    -633, -1687,    47,   251,   481,  1941, -1687,  1928, -1687,    43,
     756, -1687, -1687, -1687,   189, -1687, -1687,   378,   124,  3572,
    2629,   -28,  1734,  -298, -1687, -1687, -1687, -1687, -1687,  -638,
    4170,  4228, -1687,  -354,   -49, -1687,   488,   242, -1687,   184,
     677, -1687,   480,  -138, -1687, -1687, -1687,  4960,  -616, -1114,
    -628,  -579,  -345,  -611, -1687, -1110,  -153,   -61,  1136,   846,
    3056,  -125,  -391,  -226,  -170,  -436,  1215, -1687,  1527,  -211,
    1134,  1421, -1687, -1687, -1687, -1687,   206,  -146,  -164,  -830,
   -1687,    53, -1687, -1687,   601,   440, -1687, -1687, -1687,  2009,
    -704,  -460,  -923,    31, -1687, -1687, -1687, -1687, -1687,   231,
    -744,  -148, -1637,  -162,  6551,   -58,  6338, -1687,  1101, -1687,
    2756,  -203,  -216,  -186,  -183,     5,   -66,   -62,   -56,   344,
      -7,     1,    17,  -177,   -85,  -168,  -147,  -143,  -703,  -684,
    -621,  -619,  -658,   -94,  -617, -1687, -1687,  -678,  1285,  1289,
    1295,  1419,  7044,  -544,  -556,  -533,  -514,  -701, -1687, -1546,
   -1566, -1551, -1542,  -574,    59,  -245, -1687, -1687,   -42,     9,
     -87, -1687,  7816,   313,  -575,  -566
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,  1126,   212,   380,   381,   169,   382,   357,   383,  1411,
    1412,   384,   947,   948,  1229,  1230,  1231,  1423,   406,   386,
     387,   388,   662,   663,   389,   390,   391,   392,   393,   394,
     395,   396,   397,   398,   399,   408,  1045,   664,  1350,   723,
     206,   725,   402,   790,  1127,  1128,  1129,  1130,  1131,  1132,
    1133,  1928,  1134,  1135,  1355,  1660,  1809,  1810,  1749,  1750,
    1751,  1903,  1904,  1136,  1671,  1672,  1764,  1137,  1138,  1139,
    1140,  1141,  1142,  1363,  1688,  1848,  1783,  1143,  1144,  1542,
    1914,  1543,  1544,  1831,  1145,  1146,  1147,  1353,  1839,  1840,
    1841,  1959,  1974,  1864,  1865,   283,   284,   851,   852,  1099,
      82,    83,    84,    85,    86,  1663,   439,    89,    90,    91,
      92,    93,   220,   556,   441,   410,   442,    96,   293,    98,
      99,   100,   322,   323,   103,   104,   165,   105,   869,   324,
     151,   108,   240,   109,   152,   249,   326,   327,   328,   153,
     403,   114,   115,   330,   116,   547,   840,   838,   839,  1500,
     331,   332,   119,   120,  1095,  1318,  1506,  1507,  1629,  1630,
    1319,  1495,  1648,  1508,   121,   629,  1579,   333,   627,   904,
    1038,   447,   448,   844,   845,   449,   450,   846,   335,   551,
    1151,   412,   413,   207,   467,   468,   469,   470,   471,   311,
    1170,   312,   867,   865,   580,   313,   351,   314,   315,   414,
     123,   171,   172,   124,  1164,  1165,  1166,  1167,     2,  1084,
    1085,   828,  1302,   125,   303,   251,   261,   530,   126,   210,
     127,   221,  1047,   831,   497,   163,   128,   640,   641,   642,
     129,   223,   224,   225,   226,   298,   131,   132,   133,   196,
     135,   136,   137,   229,   299,   231,   232,   233,   758,   759,
     760,   761,   762,   234,   764,   765,   766,   728,   729,   730,
     731,   498,   138,   604,   605,   606,   607,   608,   609,  1632,
    1633,  1634,  1635,   594,   452,   338,   339,   340,   415,   198,
     140,   141,   142,   342,   782,   610
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      79,   296,   483,    79,   555,   356,   130,   182,   334,   230,
     139,   183,    87,   139,   906,   180,   491,   184,   147,   667,
      94,   955,   320,   612,   724,   514,   352,   779,   938,   527,
     931,  1234,   484,   400,  1168,   485,   473,   892,   887,   197,
      95,   486,   878,   149,   106,  1148,  1782,   189,   101,   990,
     487,   288,   901,  1014,    79,    79,  1018,    79,   886,   495,
    1241,  1732,  1025,   130,  1815,   879,   185,   139,   348,    87,
    1008,   488,    79,   835,   186,   489,  1733,    94,   483,  1403,
      79,   401,  1736,  1344,   880,  1734,   195,   511,    79,   175,
     187,   622,   491,    79,   503,   625,    79,    95,  1015,   227,
      79,   106,   252,   434,   197,   101,   262,  1152,   484,  1811,
     139,   485,   494,   861,   418,   255,   208,   486,   419,   525,
     193,   824,   826,  1275,   420,   111,   487,   562,   564,   535,
     451,  1090,    57,  1009,   492,  1010,   355,  1011,    79,   631,
    1548,    79,   633,    79,  -338,   612,  1041,   488,    79,   130,
     584,   489,   182,   139,    79,    87,   183,  1160,  1463,  1464,
     481,    79,   184,    94,  1056,   563,  1765,  1305,  -694,  1326,
    1327,  1766,  1425,   421,  1837,   291,    57,   286,    79,    79,
     705,   422,   111,    95,   195,   193,  -718,   106,   584,  1805,
     110,   101,  1019,    79,  1039,  1039,  1022,   423,   878,   455,
    1313,  1430,   520,   464,  1048,  1035,  1036,  1549,    79,  1079,
     492,   185,   886,  1039,  -338,   496,  1815,    79,    79,   186,
     337,   879,   706,  1314,   195,  1811,   860,  1945,   182,   432,
    1158,  1315,   183,  1431,    79,   187,   567,  1373,   184,   143,
     880,   908,  1116,    79,  1815,   542,   903,   110,   668,   195,
    1201,  1276,   102,    79,  -695,   161,    79,  1861,  1546,   520,
     933,    88,   531,    79,   148,   797,  1181,   278,   111,   815,
    1875,    57,   154,    79,    79,   203,    79,    57,  1224,  1039,
     516,  1782,   855,   519,  1328,  1277,  1884,   811,  1581,  1196,
     259,  1277,   612,    79,    79,   798,  1215,  1330,   799,   783,
     195,    79,   902,   589,   800,   155,  1018,    79,    79,   102,
    1261,  1248,    79,   801,   160,  1732,   612,   536,    88,   174,
     620,    95,  1008,   612,   623,   106,  1148,   953,   548,   558,
    1733,  1197,   176,   110,   802,    57,  1736,  1547,   803,  1734,
     519,  1188,   873,   994,    79,   134,   750,   763,   247,    79,
     277,   797,    79,   639,   496,  1262,   504,   245,   563,  1331,
     496,   256,   822,   933,  1237,   197,   811,  -339,   827,  1805,
      57,  1233,  1087,  1933,  1244,  1594,  1596,  1598,   595,   898,
    1488,   798,   601,  1303,   799,  1009,    19,  1010,  1152,  1253,
     800,  1251,  1252,  1845,   200,   102,    57,   156,   834,   801,
     157,   158,   134,   159,    88,   812,   111,    62,    63,  1362,
     178,  1313,  1313,  1313,   570,   418,    79,  -622,   496,   419,
     802,   589,   455,  1662,   803,   420,  1846,  1285,  1040,  1040,
     177,    57,   320,   189,  1314,  1314,  1314,  -339,   228,    79,
      79,   253,  1315,  1315,  1315,   263,   193,  1040,  1662,   741,
     878,    79,    79,   496,   178,    75,    57,   451,   502,   200,
      79,   507,   464,  1422,  1267,   965,   966,   967,   968,  1188,
    1233,   110,   749,   879,   421,   995,  1039,  1746,  1747,   496,
      79,  1392,   422,   524,   812,  1746,  1747,  1323,   134,    79,
    1325,   416,   880,   534,   208,   455,   595,  1512,   423,    57,
     577,   418,  1463,  1464,  1593,   419,  1324,   278,   529,    79,
    1016,   420,  1219,  1040,   599,    79,  1513,   528,   190,  1220,
     914,    57,   916,   917,  1518,   918,    57,   451,   920,   578,
     579,   922,   923,   924,   201,  1023,    57,   979,   456,   599,
      13,    14,    15,    16,    17,   612,  1902,  1243,    13,    14,
      15,    16,    17,    79,    57,    79,  1455,   521,   278,  1748,
    1902,    57,   209,   277,  -718,   424,    79,  1769,    79,   445,
    1323,   871,   247,  1399,   455,  1520,   612,    57,  1066,  1776,
     860,    79,   496,   265,  1777,    79,  1930,   266,   178,  1559,
     269,   203,   271,   149,  1434,   891,  -852,   273,    57,    95,
    1070,   532,   892,   106,   496,  1288,    57,   558,   897,   496,
      57,  1049,    57,   926,   521,  1292,    -3,    79,   902,   496,
     203,  1404,   581,   400,   927,   928,   582,  1173,   178,    79,
     337,   204,   597,  1390,  1044,   875,  -517,   599,  1444,  1445,
    1440,  1758,   215,  1075,   496,  1767,  1058,   205,   849,   451,
     278,   235,  1459,  1460,   695,   696,  1449,  -395,    80,  -399,
     496,   145,   763,  1517,   424,  1074,   496,  -400,  1438,  1641,
     595,  1029,   273,    79,    79,  1712,    79,  1713,   691,   692,
      79,   542,  1637,    79,   111,  1481,  1482,  1512,  1855,  1453,
     451,  1856,  1646,   599,   247,   496,    57,   592,   697,   698,
     615,  1638,  1889,   933,  1535,   707,  1640,  1890,    79,   708,
    1040,  1647,   451,   451,   592,    80,   275,  1737,   592,  1661,
     200,  1673,  1674,    13,    14,    15,    16,    17,   265,  1646,
      80,   451,   688,  1195,   819,  1823,  1738,  1941,    80,   689,
     690,   277,  1942,   424,  1661,   496,   277,   259,  1741,   110,
     597,    80,   278,    79,    80,    79,   830,   107,    80,   733,
    -396,   456,   833,   734,   784,   785,   837,    79,   786,    13,
      14,    15,    16,    17,    79,   958,   959,   960,  1381,  1591,
     464,    57,  -338,    79,  1420,   504,   320,   807,   292,   496,
     350,  1869,    79,    79,    79,   309,   745,   451,   875,  1877,
     496,    80,  1191,   355,   265,   266,    80,   616,   247,   271,
     353,   903,    79,   425,   107,   592,  1274,   156,   848,   346,
     157,   158,   849,   159,  1578,   416,   416,    57,   354,   570,
    -397,   424,  1238,   496,   456,    72,    80,    80,   426,    13,
      14,    15,    16,    17,   427,  1590,  1909,    57,    79,    79,
     464,    80,  1624,  1625,  1626,   598,  1172,   258,   860,   599,
     139,   465,    87,   188,    63,   529,    77,   600,  1618,  1619,
      94,   428,   139,   541,    63,    80,    80,  1161,    61,   601,
    1281,   429,   612,    64,    65,    66,    67,    68,    69,    70,
      95,   430,    80,   454,   106,    79,   445,    57,  1150,    79,
     107,    80,  1163,   458,    79,   459,    72,  1532,  1044,    72,
     639,  1016,  1298,   424,    80,   599,   907,   325,  1260,   763,
     582,   909,    72,  1462,   893,   582,    73,    74,   474,   726,
     472,   545,   910,   496,   550,   932,   911,    77,    78,   933,
      77,    78,   598,    79,   999,   416,   599,   444,   496,   445,
      79,    80,    80,    77,    78,  1053,  1494,  1675,   277,  1054,
    1673,  1696,   496,   504,  1703,   592,   445,   496,   477,   676,
    1394,   677,   678,   679,  1080,   111,  1082,   570,   933,    79,
     933,   496,   464,  1589,   337,   236,   237,   478,   238,   592,
     479,  1714,   239,   480,   451,  1232,  1717,  1718,   493,  1233,
     680,   494,   592,   681,   682,    79,   512,  1866,   683,   684,
     265,    79,    79,   513,  1708,  1374,   523,    72,   585,   273,
     903,   208,   538,  1866,   352,   352,   533,   242,     6,     7,
       8,     9,    10,    11,    12,   699,   700,  1627,   107,   860,
     110,   496,    79,  1306,  1307,  1308,  1380,   416,    77,    78,
     734,   190,   670,  1320,  1759,  1760,  1761,  1408,  1586,  1905,
     573,  1233,  1587,  1655,   320,  1089,   933,  1233,  1759,  1871,
    1761,   552,  1483,  1656,    80,   591,  1762,   933,   258,  1676,
     933,   587,  1413,   933,   619,  1763,  1789,  1677,   445,  1678,
    1872,  1054,   591,   933,   247,  1435,   591,  1742,    80,  -174,
    -851,   734,   102,   632,   464,   529,  1817,    79,    79,    79,
     933,    88,   139,  1510,    87,  -567,  1465,  1893,   777,  1844,
     465,  1233,    94,  1162,   630,   247,  1943,   643,  1790,   445,
     933,   464,   400,   400,  1316,  1970,  1977,    79,    80,  1967,
    1978,   139,    95,    87,   644,    79,   106,    80,    79,    79,
    1150,    94,    79,  1916,   252,   262,   647,  1920,   648,   139,
     891,   652,    79,   670,   255,   693,   694,    80,   587,   670,
     687,    95,   701,    80,  -386,   106,   674,   416,   675,  1150,
    1471,  1472,   935,   936,   634,  1031,  1032,   702,    79,  1033,
    1034,  1222,  1054,   591,  1235,  1236,   703,  -386,  1537,  1538,
    1539,  1540,  1541,    79,   933,  1239,   704,   451,   451,  -145,
    -145,   145,   709,    80,  1850,  1033,  1372,  1428,  1429,   464,
    1433,  1429,  1437,  1429,    80,    79,    80,   111,  1005,  1421,
     735,  1511,  1473,  1421,   736,   595,  1005,  1485,    13,    14,
      15,    16,    17,   870,   431,   320,   737,  1664,  1780,  1781,
     592,  1599,  1054,   615,  1710,  1054,   111,    79,   635,  1711,
    1429,   738,   337,   739,  1320,  1320,  1320,  1724,  1496,  1320,
    1721,  1722,  1664,   636,   444,   895,   637,   638,    64,    65,
      66,    67,    68,    69,    70,  1731,   933,   483,  1926,  1793,
    1429,   740,   110,    -3,  1510,   767,    57,  1794,  1429,  1862,
    1863,  -398,   491,   445,   -16,   325,   242,     6,     7,     8,
       9,    10,    11,    12,   258,    79,   107,   484,   139,    79,
     485,   110,    79,   147,  1746,  1747,   486,   444,  1967,  1968,
     -17,   941,   942,   780,   945,   487,  1426,  1427,   952,   259,
     781,   956,   464,   591,   444,  1316,  1316,  1316,   149,  1493,
    1497,   961,   962,   791,   102,    72,   488,   963,   964,   804,
     489,   805,   464,    88,    79,   264,   981,   591,   969,   970,
     531,   806,  1649,  1649,   808,   598,  1382,  1383,  1685,   599,
     591,   809,   810,   102,   814,   285,    77,   600,    61,   850,
     816,   832,    88,    64,    65,    66,    67,    68,    69,    70,
     247,  -110,  -110,  -110,  -110,  -110,  -110,  -515,  1465,   245,
     256,    80,  1511,    80,  -513,   841,   139,   862,   464,   864,
     492,  1161,   320,    79,   868,   881,   797,   883,    79,    79,
      79,   529,   601,   900,   905,   912,   913,    74,   465,   934,
     776,   777,   811,   337,   937,  1642,  1163,   940,   978,   983,
    1057,    80,  1059,  1004,  1653,  1005,   798,  1012,  1051,   799,
    1465,  1092,  1413,  1060,  1061,   800,   444,  1062,  1063,  1064,
      80,  1065,  -698,  1081,   801,  -109,  -109,  -109,  -109,  -109,
    -109,  1083,   416,    13,    14,    15,    16,    17,   930,   893,
    1093,  1094,  -598,   253,   263,   802,    79,  1153,  1169,   803,
    1154,  1182,    79,  1183,    79,  1184,  1098,   444,  1194,    61,
    1198,    79,  1199,  1775,    64,    65,    66,    67,    68,    69,
      70,   943,  1202,   464,  1204,   464,  1205,  1206,  1207,   325,
     325,  1208,  1210,  1211,   139,   612,   139,  1212,  1217,  1218,
    1287,  1510,  1240,  1245,  1246,  1247,  1785,  1254,   325,  1255,
    1256,   451,   451,  1190,   592,  1257,  1265,   464,    74,   139,
     812,   944,  -586,  -585,    95,  1280,    95,  1299,   106,   148,
     106,  -699,  1666,   139,  1666,  1321,   325,    61,  1161,    79,
    1322,   445,    64,    65,    66,    67,    68,    69,    70,    95,
    1332,  1335,  1336,   106,    79,  1345,    79,  1666,  1346,  1347,
    1352,  1228,   400,  1163,  -621,  1354,  1838,   107,  1228,   933,
    1356,  1362,  1366,  1368,   325,  1369,   529,  1405,  1370,  1376,
     337,  1378,  1406,  1419,  1448,   528,    74,  1421,   591,  1436,
    1466,   258,  1461,   325,  1469,  1467,  1468,  1228,  1477,  1429,
     465,    79,  1474,  1486,    79,  1465,  1487,  1489,   506,   111,
    1808,   111,  1499,  1325,   464,  1523,  1501,  1524,  1526,  1511,
    1502,  1550,  1552,    80,  1528,   483,  1555,  1162,  1529,    80,
      80,  1531,  1560,  1565,   111,   139,   464,  1562,  1563,   491,
     255,   444,  1533,    18,  1883,   847,    13,    14,    15,    16,
      17,  1214,    79,    79,  1545,   484,  1566,  1553,   485,  1554,
    1228,    79,  1567,  1568,   486,  1569,  1571,  1576,  1582,   532,
    1584,  1580,   811,   487,   110,  1838,   110,   400,   400,  1838,
    1838,    47,    48,    49,    50,    51,    52,    53,    54,  1585,
    1588,  1834,  1592,  1600,   488,  1601,  1614,   451,   489,   110,
    1605,   325,  1606,    79,   400,   424,  1939,  1473,  1616,   464,
    1623,  1504,  1636,   464,  1657,  1682,  1684,  1832,   325,   325,
    1689,  1701,   445,  1695,   464,  1900,  1808,  1958,  1702,  1936,
    1699,  1958,  1700,  1709,  1715,   139,   102,   464,   102,  1716,
    1719,  1720,  1729,  1772,  1730,    88,  1726,    88,  1834,  1754,
     209,  1768,  1918,  1774,  1116,  1778,  1972,   492,  1779,  1787,
    1788,   102,  1791,   550,  1792,    95,    80,    80,  -587,   106,
      88,   325,   416,  1666,  1832,   182,   400,   496,  1800,   183,
      80,  1801,   139,   567,  1162,   184,  1802,  1803,  1804,   732,
     812,    79,  1816,    79,  1812,  1819,  1827,  1828,   139,  1835,
    1849,  1851,  1836,  1852,   464,   743,   464,   464,   746,  1853,
      81,  1854,    95,   146,  1722,   259,   106,  -498,  1867,   107,
    1666,  1409,  1874,  1886,  1969,  1887,  1888,  1891,    95,  1895,
    1892,  1898,   106,  1917,   139,  1906,  1666,   465,  1907,  1924,
      79,    79,  1913,  1228,  1919,  1925,  1931,   195,   107,    61,
     111,   464,   167,   168,    64,    65,    66,    67,    68,    69,
      70,   464,  1932,  1937,    95,   506,   258,    81,   106,  1938,
    1940,  1949,  1666,  1955,    79,    80,   247,  1956,  1960,  1961,
     455,  1964,   179,  1976,  1965,   245,   256,   464,  1975,   464,
      81,  1979,   591,   673,  1706,  1432,   973,   111,    74,   971,
    1358,   929,   972,   219,   464,   974,   244,   975,  1950,   464,
      81,  1351,  1901,   111,  1686,   110,   847,  1911,  1770,   444,
    1826,  1873,  1337,   464,  1847,  1946,  1944,    79,  1935,  1680,
    1879,  1921,  1681,    80,  1962,  1878,   166,    79,    13,    14,
      15,    16,    17,   522,  1367,  1639,  1807,   146,  1498,   111,
    1860,  1650,  1364,    81,   279,   146,  1050,   787,   295,   301,
     465,  1171,   110,   866,  1692,   325,   325,   529,  1583,     3,
     319,   986,  1200,   847,     0,   987,   528,   102,   110,   325,
     325,   988,    80,     0,   325,   325,    88,   407,   179,   179,
       0,     0,     0,     0,     0,     0,    57,     0,   248,   146,
     437,     0,     0,   244,     0,     0,     0,     0,     0,   268,
       0,     0,   325,   325,   110,     0,    13,    14,    15,    16,
      17,     0,     0,     0,   102,     0,     0,   219,   219,     0,
       0,     0,     0,    88,   476,     0,   465,     0,     0,     0,
     102,  1228,     0,     0,   295,     0,  1228,  1228,  1228,    88,
       0,   248,    61,    81,     0,    72,     0,    64,    65,    66,
      67,    68,    69,    70,  1225,     0,   244,     0,  1226,     0,
    1227,     0,     0,     0,    57,   726,   102,     0,     0,   496,
       0,   847,    18,    61,     0,    88,    77,    78,    64,    65,
      66,    67,    68,    69,    70,   248,   301,     0,   847,   847,
     444,    74,   301,   295,   295,   732,   732,   559,   626,     0,
     146,   592,     0,     0,    80,   997,     0,   649,  1000,     0,
      80,     0,    80,     0,    51,    52,    53,    54,   319,   602,
     611,  1258,    74,    72,     0,     0,     0,  1192,     0,     0,
       0,     0,   685,   686,     0,   319,     0,     0,     0,   319,
       0,     0,     0,  1627,     0,     0,     0,   496,   248,  1334,
       0,     0,     0,   685,    77,    78,    61,     0,   592,     0,
       0,    64,    65,    66,    67,    68,    69,    70,   950,   506,
       0,     0,   407,  1068,     0,     0,     0,  1072,   248,     0,
       0,     0,     0,   685,   248,   325,   325,   666,     0,     0,
      61,     0,     0,   167,   168,    64,    65,    66,    67,    68,
      69,    70,  1228,     0,  1228,    74,   407,     0,   951,   727,
       0,     0,   248,     0,     0,     0,   179,     0,     0,    61,
       0,   325,   167,   168,    64,    65,    66,    67,    68,    69,
      70,   107,   146,   107,     0,     0,   437,     0,     0,    74,
     756,     0,   611,     0,     0,     0,     0,     0,     0,    80,
       0,     0,    80,     0,     0,     0,   107,     0,     0,     0,
       0,     0,     0,  1359,     0,     0,     0,   325,    74,   242,
       6,     7,     8,     9,    10,    11,    12,     0,   325,     0,
     219,     0,     0,     0,   304,   305,   306,   307,    61,   219,
       0,   167,   168,    64,    65,    66,    67,    68,    69,    70,
       0,    80,     0,     0,     0,     0,     0,     0,   325,   295,
       0,   407,   407,   325,   325,   295,   818,   319,   325,   325,
       0,   821,    61,     0,     0,   167,   168,    64,    65,    66,
      67,    68,    69,    70,   248,   847,   847,    74,   829,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   836,   847,
     847,    80,     0,     0,     0,   295,     0,     0,   732,     0,
       0,  1360,     0,     0,     0,     0,   295,     0,   295,     0,
     319,    74,   258,     0,   308,     0,     0,     0,     0,     0,
       0,     0,   847,   847,     0,     0,   319,   437,     0,   611,
       0,     0,   309,     0,   235,     0,     0,   602,     0,     0,
       0,   602,     0,     0,     0,     0,     0,  1556,   248,     0,
     319,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     611,     0,     0,   319,     0,     0,     0,     0,   248,  1290,
       0,   146,  1294,   957,     0,     0,     0,     0,     0,    80,
       0,  1885,     0,     0,   407,     0,   146,   146,   248,   407,
       0,     0,   407,     0,     0,   146,   146,   146,     0,   666,
       0,     0,     0,     0,   666,     0,     0,   666,     0,     0,
       0,     0,   107,     0,     0,     0,     0,     0,     0,   591,
       0,   248,     0,     0,     0,     0,   666,     0,    80,    80,
       0,     0,    61,     0,     0,   216,   217,    64,    65,    66,
      67,    68,    69,    70,   325,   248,     0,     0,     0,   437,
       0,     0,   248,     0,     0,     0,     0,     0,     0,   107,
      72,     0,    80,     0,     0,   727,   727,     0,     0,     0,
       0,     0,     0,   407,     0,   107,   591,     0,     0,     0,
    1503,    74,     0,     0,     0,     0,     0,  1504,     0,     0,
     437,    77,    78,   756,     0,   756,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   847,   847,     0,  1658,     0,
    1667,   107,   319,   319,     0,  1973,     0,     0,     0,   665,
     113,     0,     0,   113,     0,  1980,     0,     0,     0,     0,
      57,   319,     0,   295,     0,     0,     0,     0,     0,     0,
       0,  1654,  1690,     0,     0,   325,     0,     0,     0,     0,
       0,     0,   295,     0,  1442,     0,     0,     0,     0,     0,
       0,    61,     0,  1451,   216,   217,    64,    65,    66,    67,
      68,    69,    70,  1078,     0,     0,     0,   113,     0,     0,
       0,     0,     0,     0,  1086,     0,     0,  1088,     0,     0,
     407,  1091,     0,     0,     0,     0,     0,   319,   847,     0,
     113,     0,   146,   407,     0,     0,     0,  1209,     0,  1258,
      74,   319,  1213,  1176,     0,     0,   250,     0,     0,     0,
     113,     0,     0,  1221,   602,     0,     0,     0,   847,     0,
       0,     0,     0,   847,   847,     0,     0,     0,    61,  1745,
       0,   167,   168,    64,    65,    66,    67,    68,    69,    70,
       0,     0,     0,     0,     0,     0,     0,   113,   823,   825,
       0,  1771,     0,   113,   437,   113,     0,     0,     0,   250,
       0,     0,     0,     0,   248,     0,     0,     0,     0,   316,
     113,   347,     0,     0,     0,   248,     0,    74,    13,    14,
      15,    16,    17,     0,     0,     0,     0,   411,     0,     0,
       0,     0,     0,     0,     0,   248,     0,     0,     0,   113,
     411,  1339,     0,   250,     0,     0,     0,     0,     0,   181,
       0,     0,     0,     0,     0,     0,     0,     0,   727,     0,
       0,     0,     0,     0,  1814,     0,     0,     0,  1818,     0,
     222,     0,     0,     0,     0,   756,    57,     0,     0,  1825,
       0,     0,   756,     0,     0,     0,     0,     0,     0,     0,
     113,     0,  1842,   113,    61,     0,     0,   167,   168,    64,
      65,    66,    67,    68,    69,    70,   250,    61,     0,     0,
     216,   217,    64,    65,    66,    67,    68,    69,    70,   649,
       0,   665,     0,   546,   319,   297,   665,     0,     0,   665,
       0,   113,  1631,     0,     0,    72,   250,     0,     0,     0,
       0,     0,   250,    74,     0,     0,     0,     0,   665,     0,
     113,     0,     0,  1341,     0,   218,    74,     0,     0,  1894,
       0,  1896,  1897,     0,   146,     0,    77,    78,   113,     0,
     250,   113,   407,  1304,     0,     0,     0,     0,     0,     0,
       0,     0,   977,     0,     0,   113,  1329,     0,     0,   113,
       0,     0,   685,     0,   482,   222,     0,     0,     0,     0,
       0,   407,     0,  1348,     0,     0,  1922,     0,     0,     0,
       0,   297,     0,     0,     0,     0,  1927,     0,   244,    81,
       0,     0,   411,     0,     0,     0,  1414,  1415,  1416,     0,
       0,     0,   295,  1417,  1418,     0,     0,     0,   146,     0,
       0,    61,  1948,     0,  1927,   437,    64,    65,    66,    67,
      68,    69,    70,     0,     0,   847,   411,     0,     0,  1963,
       0,     0,     0,     0,  1948,     0,  1631,  1631,     0,    72,
     568,   297,   437,     0,     0,     0,   146,     0,  1971,     0,
      57,     0,   113,     0,     0,     0,   411,     0,     0,  1006,
      74,     0,   250,   599,     0,    13,    14,    15,    16,    17,
      77,    78,     0,     0,     0,     0,     0,     0,     0,     0,
     248,    61,     0,     0,   216,   217,    64,    65,    66,    67,
      68,    69,    70,     0,     0,     0,     0,     0,     0,     0,
       0,   170,   173,     0,     0,     0,     0,   319,   319,    72,
       0,   248,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    57,     0,     0,     0,     0,     0,  1881,
      74,   411,   411,   496,     0,   211,   250,   113,     0,     0,
      77,    78,     0,     0,  1631,   146,   146,   146,   146,   146,
     146,     0,     0,     0,    61,  1505,   301,   216,   217,    64,
      65,    66,    67,    68,    69,    70,     0,     0,   113,     0,
       0,     0,     0,   113,     0,     0,   250,   113,     0,   113,
    1519,  1521,    72,     0,     0,   289,     0,   757,   290,    61,
     113,     0,   167,   168,    64,    65,    66,    67,    68,    69,
      70,   310,   294,    74,   244,   347,   113,   411,     0,   250,
       0,  1858,     0,    77,    78,  1631,     0,     0,  1557,     0,
       0,     0,     0,   437,     0,     0,     0,   796,     0,     0,
     113,     0,     0,   250,     0,     0,   222,   546,    74,     0,
     250,  1631,     0,   113,     0,   899,   146,     0,   248,     0,
       0,   113,     0,     0,     0,   475,   297,     0,     0,     0,
       0,     0,   297,     0,   411,     0,   113,   113,     0,   411,
       0,     0,   411,     0,     0,   113,   113,   113,    61,     0,
       0,   167,   168,    64,    65,    66,    67,    68,    69,    70,
    1631,  1631,     0,     0,     0,     0,   248,     0,     0,     0,
     526,     0,   297,     0,     0,     0,     0,     0,     0,     0,
     170,     0,     0,   859,     0,   297,     0,     0,     0,     0,
       0,   170,     0,     0,  1631,     0,     0,    74,     0,   411,
       0,     0,  1628,     0,   575,     0,  1505,     0,   407,     0,
       0,  1687,  1505,     0,  1505,     0,     0,     0,   572,     0,
       0,     0,     0,   411,   574,   576,     0,     0,     0,   583,
       0,     0,     0,     0,   407,     0,   407,     0,     0,     0,
     411,     0,     0,     0,     0,   363,     0,   364,    57,   365,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   407,
       0,   628,   113,   113,     0,  1679,   310,     0,     0,   310,
     319,     0,     0,   146,     0,     0,     0,     0,     0,    61,
       0,   113,   216,   217,    64,    65,    66,    67,    68,    69,
      70,    97,     0,     0,   150,   672,   146,     0,    75,   374,
       0,    13,    14,    15,    16,    17,     0,    72,    61,   113,
       0,     0,     0,    64,    65,    66,    67,    68,    69,    70,
    1225,   319,   319,     0,  1226,     0,  1227,   218,    74,     0,
       0,     0,   250,     0,     0,     0,  1628,  1628,    77,    78,
     411,     0,     0,   250,     0,     0,     0,   113,    97,     0,
       0,  1505,   113,   411,  1505,     0,   211,    74,     0,    57,
    1424,   113,     0,  1178,   411,     0,   113,     0,   771,   772,
    1007,   194,   757,     0,   301,   146,    61,     0,     0,   343,
     344,    64,    65,    66,    67,    68,    69,    70,     0,     0,
      61,   257,     0,   216,   217,    64,    65,    66,    67,    68,
      69,    70,     0,   295,     0,     0,     0,     0,     0,     0,
     297,     0,     0,     0,   411,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,    74,     0,    75,   287,   297,
       0,     0,   345,   112,    97,     0,  1843,     0,   754,    74,
       0,     0,   599,     0,  1628,     0,     0,     0,  1522,    77,
     755,   321,     0,  1505,     0,     0,     0,  1530,     0,     0,
       0,  1534,     0,  1536,     0,   301,   248,     0,     0,   417,
       0,     0,     0,     0,   113,   407,     0,     0,     0,     0,
     287,   443,   146,     0,     0,     0,     0,     0,     0,     0,
     112,   113,   113,     0,     0,     0,     0,     0,   310,     0,
       0,     0,     0,     0,     0,     0,     0,   319,     0,   490,
       0,     0,     0,     0,     0,  1628,     0,     0,     0,     0,
       0,     0,   146,     0,     0,   510,     0,     0,     0,     0,
     515,   517,     0,   260,   194,     0,     0,     0,   146,   146,
       0,  1882,   301,     0,   113,     0,   628,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   537,   248,     0,   539,
       0,   540,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   557,     0,   146,     0,   112,     0,     0,     0,
       0,     0,     0,     0,   113,   569,     0,     0,     0,     0,
    1882,  1882,   411,   329,     0,     0,     0,     0,     0,     0,
       0,  1622,     0,     0,     0,     0,     0,     0,     0,   590,
       0,     0,   614,     0,    57,     0,     0,     0,     0,     0,
       0,   411,  1007,   446,  1882,     0,   621,     0,  1259,   757,
     621,     0,     0,     0,   248,     0,     0,     0,   250,   113,
       0,  1659,     0,  1670,     0,    61,     0,     0,   216,   217,
      64,    65,    66,    67,    68,    69,    70,     0,   113,     0,
       0,  1957,     0,     0,     0,   411,  1659,     0,     0,  1178,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
    1966,  1402,     0,     0,  1030,     0,     0,     0,     0,     0,
       0,  1042,   411,   294,    74,     0,   113,     0,     0,     0,
       0,     0,     0,     0,    77,    78,     0,     0,     0,     0,
       0,     0,     0,     0,   112,     0,     0,     0,     0,     0,
       0,     0,     0,   287,     0,    61,     0,   590,   216,   217,
      64,    65,    66,    67,    68,    69,    70,     0,   113,   113,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   593,   113,   113,   260,     0,     0,   113,   113,     0,
       0,     0,     0,     0,     0,     0,  1100,     0,   593,     0,
       0,     0,   593,  1753,    74,     0,     0,  1755,     0,   297,
       0,  1189,     0,     0,  1757,   113,   113,     0,     0,     0,
       0,     0,     0,     0,     0,   113,   113,   113,   113,   113,
     113,     0,     0,     0,     0,     0,   250,     0,   443,     0,
       0,     0,     0,     0,     0,     0,  1193,     0,     0,     0,
       0,   628,     0,    13,    14,    15,    16,    17,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   843,
       0,     0,     0,     0,   517,     0,     0,     0,   854,     0,
     557,     0,     0,     0,   250,     0,     0,     0,     0,     0,
       0,   321,     0,     0,     0,     0,     0,     0,     0,   593,
       0,     0,     0,   411,  1456,     0,     0,   621,   874,  1822,
    1824,    57,  1670,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   885,     0,     0,     0,   113,     0,     0,     0,
       0,   590,     0,     0,     0,     0,   894,    57,     0,     0,
       0,     0,    61,     0,   621,   216,   217,    64,    65,    66,
      67,    68,    69,    70,     0,     0,     0,     0,     0,     0,
       0,     0,  1509,     0,     0,     0,     0,  1870,    61,     0,
      72,   216,   217,    64,    65,    66,    67,    68,    69,    70,
     446,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1881,    74,     0,     0,   496,     0,    72,     0,   113,   113,
       0,    77,    78,     0,     0,     0,     0,     0,     0,     0,
       0,   329,     0,     0,     0,     0,  1503,    74,   411,  1908,
     260,  1910,   112,     0,     0,     0,     0,    77,    78,     0,
     443,     0,     0,   446,   113,     0,     0,     0,     0,     0,
       0,  1923,     0,     0,   411,     0,   411,   989,     0,   593,
     446,     0,     0,     0,  1338,  1340,  1342,     0,     0,     0,
       0,   117,     0,     0,   117,     0,     0,     0,     0,   411,
       0,   874,     0,   593,     0,     0,  1013,  1951,  1953,  1954,
     113,     0,     0,   113,  1361,     0,   593,     0,     0,     0,
       0,   113,     0,   443,   443,     0,     0,     0,     0,  1100,
       0,     0,     0,     0,     0,     0,   113,     0,     0,     0,
       0,     0,   443,     0,     0,     0,     0,     0,   117,   118,
       0,   113,   118,     0,     0,     0,   113,   113,     0,     0,
       0,   113,   113,     0,     0,   628,     0,     0,     0,     0,
     843,   117,     0,  1509,     0,     0,     0,     0,     0,  1643,
       0,  1509,     0,     0,     0,     0,     0,     0,     0,    61,
       0,   117,   188,    63,    64,    65,    66,    67,    68,    69,
      70,  1149,   446,     0,     0,     0,   118,     0,   443,     0,
       0,     0,     0,   150,   250,   113,     0,     0,     0,     0,
       0,     0,   621,     0,     0,  1180,     0,   843,   117,   118,
       0,     0,  1186,     0,   117,     0,   117,     0,    74,     0,
       0,   776,     0,   446,     0,     0,    61,     0,     0,   118,
       0,    64,    65,    66,    67,    68,    69,    70,  1225,     0,
       0,     0,  1226,     0,  1227,   329,   329,     0,   117,     0,
       0,     0,     0,     0,     0,   321,     0,     0,     0,     0,
     117,     0,     0,     0,   329,     0,   118,     0,     0,     0,
       0,     0,   118,     0,   118,    74,  1514,     0,  1595,  1516,
       0,     0,     0,     0,     0,   250,     0,     0,     0,     0,
       0,     0,   329,     0,     0,   411,     0,     0,  1743,     0,
       0,  1509,   113,     0,     0,     0,   118,     0,     0,     0,
       0,   117,     0,     0,   117,   843,     0,     0,   118,   117,
       0,     0,     0,   112,     0,     0,     0,   113,     0,     0,
     329,     0,   843,   843,     0,     0,     0,     0,     0,     0,
       0,     0,   113,     0,   593,     0,     0,   260,     0,   329,
     297,     0,   117,     0,     0,     0,     0,     0,   113,   113,
       0,     0,   250,     0,     0,     0,     0,     0,     0,   118,
       0,   117,   118,     0,     0,     0,     0,   118,     0,     0,
       0,     0,     0,     0,     0,   443,     0,     0,     0,     0,
       0,     0,     0,     0,   113,     0,     0,   446,    61,     0,
    1509,   541,    63,    64,    65,    66,    67,    68,    69,    70,
     118,    61,     0,     0,   216,   217,    64,    65,    66,    67,
      68,    69,    70,     0,     0,  1317,     0,     0,   113,   118,
       0,     0,     0,  1149,     0,     0,     0,     0,     0,    72,
       0,     0,     0,   117,     0,     0,     0,    74,     0,     0,
     980,     0,    13,    14,    15,    16,    17,   329,     0,   754,
      74,     0,  1149,   599,     0,     0,     0,     0,  1651,     0,
      77,   755,     0,     0,   329,   329,     0,   117,     0,     0,
    1365,     0,     0,   601,     0,     0,     0,     0,   297,     0,
     711,   712,   713,   714,   715,   716,   717,   718,   719,   720,
     721,   118,     0,   117,   203,     0,   590,     0,     0,     0,
      57,     0,     0,     0,     0,   515,    13,    14,    15,    16,
      17,     0,     0,     0,     0,     0,     0,   329,     0,     0,
       0,   722,     0,   321,     0,   118,   628,   568,   297,     0,
       0,    61,     0,     0,   216,   217,    64,    65,    66,    67,
      68,    69,    70,     0,     0,     0,     0,     0,     0,     0,
       0,   118,     0,     0,     0,     0,     0,     0,     0,    72,
       0,   297,     0,     0,    57,   112,     0,     0,     0,   843,
     843,     0,   117,   117,     0,     0,     0,     0,     0,  1503,
      74,     0,     0,   843,   843,     0,     0,     0,   443,   443,
      77,    78,     0,     0,   112,    61,     0,     0,     0,     0,
      64,    65,    66,    67,    68,    69,    70,     0,     0,     0,
       0,     0,   260,     0,   117,     0,   843,   843,   117,     0,
     117,     0,     0,    72,     0,     0,  1317,  1317,  1317,   150,
     118,   118,     0,     0,     0,     0,     0,     0,   593,  1784,
       0,     0,     0,    73,    74,     0,     0,     0,   628,     0,
       0,    61,     0,     0,    77,    78,    64,    65,    66,    67,
      68,    69,    70,  1225,     0,   446,     0,  1226,     0,  1227,
       0,     0,   118,     0,     0,     0,   118,     0,   118,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   117,     0,     0,     0,     0,     0,     0,     0,
      74,     0,     0,  1597,   321,   117,     0,   117,   117,     0,
     117,   329,   329,   117,     0,     0,   117,   117,   117,     0,
       0,     0,     0,     0,     0,   329,   329,   150,     0,    61,
     329,   329,   216,   217,    64,    65,    66,    67,    68,    69,
      70,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     118,     0,     0,     0,     0,     0,     0,    72,   329,   329,
       0,     0,     0,   118,     0,   118,   118,     0,   118,     0,
       0,   118,     0,     0,   118,   118,   118,  1503,    74,     0,
       0,     0,     0,     0,  1504,     0,     0,     0,    77,    78,
       0,     0,     0,     0,   117,     0,     0,     0,     0,   843,
     843,    61,     0,     0,   216,   217,    64,    65,    66,    67,
      68,    69,    70,    61,     0,     0,   543,   544,    64,    65,
      66,    67,    68,    69,    70,  1645,     0,     0,     0,    72,
       0,     0,     0,     0,     0,   843,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1665,   446,  1665,     0,  1881,
      74,   122,   118,   496,   122,     0,     0,     0,     0,     0,
      77,    78,    74,     0,    75,     0,     0,     0,    61,     0,
    1665,    62,    63,    64,    65,    66,    67,    68,    69,    70,
       0,   321,     0,     0,   150,     0,     0,     0,     0,     0,
      61,     0,   843,   216,   217,    64,    65,    66,    67,    68,
      69,    70,     0,     0,     0,     0,     0,     0,   122,     0,
       0,   117,     0,     0,     0,     0,     0,    74,    72,    75,
       0,     0,   843,   117,   117,     0,     0,   843,   843,     0,
       0,   122,   443,   443,     0,     0,     0,     0,   218,    74,
       0,   329,   329,     0,     0,     0,     0,     0,  1735,    77,
      78,   122,     0,     0,     0,    61,     0,     0,   216,   217,
      64,    65,    66,    67,    68,    69,    70,     0,     0,   118,
       0,     0,     0,     0,     0,     0,     0,   329,     0,     0,
       0,   118,   118,    72,     0,     0,  1756,   112,   122,   112,
       0,     0,     0,     0,   122,     0,   122,     0,     0,     0,
       0,     0,     0,   294,    74,     0,     0,     0,     0,     0,
       0,     0,   112,     0,    77,    78,     0,     0,     0,     0,
       0,     0,     0,   329,     0,     0,     0,     0,   122,     0,
       0,     0,     0,     0,   329,     0,     0,     0,     0,    61,
     122,     0,   167,   168,    64,    65,    66,    67,    68,    69,
      70,     0,    61,     0,     0,   167,   168,    64,    65,    66,
      67,    68,    69,    70,   329,     0,     0,     0,     0,   329,
     329,     0,     0,     0,   329,   329,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1665,   454,    74,     0,
       0,   122,     0,  1833,   122,     0,     0,     0,     0,   122,
     458,    74,     0,     0,     0,     0,   202,     0,     0,     0,
       0,     0,   213,   214,     0,     0,     0,     0,   443,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   260,     0,
       0,     0,   122,  1665,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   276,     0,     0,  1665,
    1833,   122,     0,     0,     0,   117,     0,     0,     0,     0,
       0,     0,     0,   117,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1665,     0,     0,     0,     0,
       0,     0,   117,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1915,     0,     0,     0,     0,     0,     0,     0,
     117,     0,     0,   118,     0,     0,     0,     0,     0,   843,
       0,   118,     0,   122,     0,     0,     0,     0,   112,   117,
       0,     0,     0,     0,     0,   593,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     118,     0,     0,     0,     0,     0,     0,   122,     0,     0,
     329,     1,     0,     0,   144,     0,     0,   117,   118,     0,
       0,     0,     0,     0,     0,   112,     0,     0,     0,     0,
       0,     0,     0,   122,     0,     0,     0,   118,     0,     0,
       0,   112,   593,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   565,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   118,     0,   112,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   191,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   329,   122,   122,     0,     0,   117,   117,   117,   117,
     117,   117,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   282,     0,     0,     0,     0,
       0,     0,     0,     0,   122,     0,     0,     0,   122,     0,
     122,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   118,   118,   118,   118,   118,   118,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   752,     0,   753,
       0,     0,     0,     0,     0,     0,     0,   117,   769,   770,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   122,     0,     0,     0,     0,     0,     0,   282,
       0,     0,     0,     0,     0,   122,     0,   122,   122,     0,
     122,     0,     0,   122,   518,     0,   122,   122,   122,     0,
       0,     0,     0,     0,   282,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   282,   118,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   549,   553,
       0,     0,     0,     0,     0,   560,   561,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   117,
       0,   571,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   853,     0,     0,   588,
       0,     0,     0,     0,   122,   117,     0,   117,     0,     0,
       0,   385,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     117,     0,     0,     0,     0,     0,     0,   118,     0,     0,
       0,     0,     0,     0,   117,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   671,     0,     0,     0,     0,
       0,     0,     0,   118,     0,   118,     0,   117,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   710,     0,   118,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   118,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   748,     0,     0,     0,   751,     0,     0,     0,
       0,   122,     0,     0,     0,   118,     0,     0,     0,     0,
       0,     0,     0,   122,   122,   773,     0,     0,     0,   774,
     775,     0,     0,   778,     0,     0,   117,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   792,   793,
     794,   795,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   817,     0,     0,
       0,     0,     0,     0,     0,   820,     0,     0,     0,     0,
       0,     0,   646,     0,     0,   385,   651,     0,     0,     0,
       0,     0,     0,     0,   118,   654,   655,     0,  1028,     0,
       0,     0,     0,   282,     0,     0,     0,     0,     0,     0,
     385,   385,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   385,     0,     0,   858,     0,   117,     0,     0,     0,
       0,   549,     0,     0,     0,     0,   863,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   385,     0,     0,     0,     0,     0,     0,     0,   877,
     882,     0,     0,     0,     0,  1096,  1097,     0,     0,     0,
       0,     0,     0,   117,     0,     0,  1155,  1156,  1157,     0,
       0,  1159,     0,     0,   118,     0,     0,     0,     0,   117,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     925,     0,     0,     0,     0,   117,     0,     0,     0,     0,
       0,   118,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   122,     0,   118,     0,     0,
       0,     0,     0,   122,     0,     0,     0,     0,     0,     0,
    1223,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   122,   118,     0,     0,     0,   985,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     122,     0,  1002,  1242,     0,     0,  1003,     0,     0,     0,
       0,     0,     0,     0,     0,   877,     0,     0,     0,   122,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1043,     0,     0,
       0,     0,     0,     0,     0,     0,  1052,     0,     0,     0,
    1266,     0,  1055,     0,     0,     0,     0,   122,     0,  1270,
    1271,  1272,  1273,     0,     0,     0,     0,  1278,  1279,     0,
       0,     0,     0,     0,     0,     0,     0,  1286,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     1,
       0,     0,     0,     0,     0,     0,     1,     0,  1300,     0,
    1301,   385,   385,   385,   385,   385,   385,   385,   385,   385,
     385,   385,   385,   385,   385,   385,   385,   385,   385,   385,
       0,     0,     1,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1357,     0,     0,   122,   122,   122,   122,
     122,   122,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1203,     0,     0,  1371,
       0,   385,     0,     0,     0,  1375,     0,  1377,  1379,     0,
       0,     0,     0,     0,     0,     0,  1385,     0,  1386,     0,
    1387,     0,  1389,     0,     0,     0,     0,  1397,     0,     0,
       0,     0,    13,    14,    15,    16,    17,     0,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,  -402,  -402,     0,  -402,
      45,    46,     0,  -402,     0,     0,  1249,   122,     0,     0,
    1250,     0,     0,     0,     0,  1439,     0,   877,     0,     0,
      57,     0,  1446,  1447,     0,   164,     0,  1263,     0,     0,
       0,     0,     0,     0,  1264,     0,     0,     0,     0,     0,
       0,     0,     0,  1268,     0,  1269,  1470,     0,     0,     0,
       0,   164,     0,  1475,    62,    63,  1476,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1296,     0,     0,
       0,  1297,     0,     0,     0,   385,   213,     0,     0,     0,
     385,     0,     0,     0,     0,   144,   164,     0,     1,   122,
       0,   385,    75,     0,     0,     0,     0,     0,     0,   164,
       0,   164,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   122,  1551,   122,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   246,     0,
     385,   349,     0,     0,     0,     0,     0,     0,     0,   267,
     122,   270,     0,   272,     0,     0,     0,     0,   349,  1570,
       0,     0,     0,     0,   122,     0,     0,  1575,     0,  1577,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1384,     0,   122,     0,     0,
       0,   246,     0,   270,   272,     0,   164,     0,     0,     0,
     164,     0,     0,   164,   164,     0,     0,   164,     0,  1407,
     164,   164,     0,     0,     0,  1603,  1604,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1609,  1610,     0,  1611,     0,   246,     0,     0,   162,     0,
       0,     0,  1615,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1620,  1621,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   122,   385,     0,     0,
       0,     0,   164,     0,     0,   164,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   164,     0,     0,   246,  1479,
     270,   272,     0,  1480,     0,     0,     0,     0,     0,   274,
     164,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   280,     0,   281,     0,     0,     0,   246,     0,
       0,     0,     0,  1515,   246,     0,     0,     0,     0,     0,
     385,     0,     0,     0,  1697,  1698,  1525,  1527,     0,     0,
       0,     0,     0,     0,  1704,     0,     0,     0,     0,     0,
       0,     0,   246,     0,     0,     0,   122,     0,   617,     0,
     272,     0,     0,     0,   385,   385,   385,     0,     0,     0,
       0,   385,   385,  1561,     0,     0,  1564,     0,     0,  1727,
    1728,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1572,     0,   385,   164,     0,     0,     0,     0,     0,
       0,     0,     0,   122,     0,     0,   500,   501,     0,     0,
     505,     0,     0,   508,   509,     0,     0,     0,     0,   122,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     385,   385,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1602,     0,     0,     0,     0,     0,     0,   246,   349,
    1607,     0,     0,     0,  1608,   122,     0,  1786,     0,     0,
       0,   164,     0,     0,     0,     0,     0,     0,  1612,  1613,
       0,     0,     0,     0,   246,  1795,   617,   272,  1796,  1797,
       0,     0,     0,     0,     0,  1799,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   586,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   618,     0,     0,     0,     0,     0,     0,
       0,   246,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   349,     0,     0,     0,     0,
       0,   246,     0,     0,     0,     0,   246,     0,   246,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1693,  1694,   246,     0,
     246,   246,     0,     0,   164,   164,   192,     0,     0,  1868,
       0,     0,     0,     0,     0,     0,  1876,   164,   246,     0,
       0,     0,  1880,     0,     0,     0,     0,     0,     0,     0,
     246,     0,     0,     0,     0,     0,     0,   742,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   246,     0,   617,   272,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1912,   192,     0,     0,     0,   246,   617,     0,     0,     0,
       0,     0,   246,     0,     0,     0,   192,     0,     0,     0,
       0,     0,     0,     0,  1929,     0,     0,     0,     0,     0,
       0,     0,  1934,   192,   813,     0,     0,     0,     0,     0,
       0,     0,     0,  1773,     0,     0,   440,  1947,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   164,   164,
       0,     0,     0,     0,   164,  1564,     0,     0,     0,   385,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1798,     0,   164,     0,     0,   164,   164,
       0,   164,     0,   164,   164,     0,     0,     0,     0,   192,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1813,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1829,     0,   164,  1830,     0,     0,   164,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   888,   889,     0,
       0,     0,     0,     0,     0,     0,   192,     0,     0,     0,
     896,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   192,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   336,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   164,     0,     0,     0,     0,     0,  1899,     0,     0,
       0,     0,     0,     0,   433,   336,     0,     0,     0,     0,
       0,     0,     0,     0,   246,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   246,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   499,     0,     0,     0,
     385,     0,   192,   499,     0,   246,     0,     0,     0,     0,
       0,   991,   992,     0,     0,     0,   246,   996,     0,     0,
       0,     0,     0,     0,     0,   246,     0,     0,     0,     0,
       0,     0,   192,     0,   385,     0,     0,     0,  1017,     0,
       0,  1020,  1021,     0,  1024,     0,  1026,  1027,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   499,     0,     0,     0,     0,     0,     0,     0,     0,
     164,     0,     0,     0,     0,  1067,     0,     0,     0,  1071,
       0,     0,     0,   336,   603,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   192,   192,     0,
       0,     0,     0,   440,   624,   385,   385,     0,     0,     0,
     246,     0,     0,     0,     0,     0,     0,     0,   164,     0,
    1185,   164,     0,     0,     0,     0,     0,    13,    14,    15,
      16,    17,   385,     0,   246,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1187,     0,   192,   385,     0,     0,
       0,     0,     0,   358,     0,     0,     0,   359,     0,   360,
       0,     0,     0,   440,   499,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    57,   361,     0,     0,     0,
     499,   744,     0,   499,   747,     0,   192,     0,     0,     0,
       0,   336,     0,     0,   385,   603,     0,     0,     0,     0,
       0,     0,     0,   362,   363,     0,   364,   192,   365,    62,
      63,    64,    65,    66,    67,    68,    69,    70,   366,   367,
     355,     0,   368,   369,   370,     0,   371,   372,     0,     0,
     164,     0,     0,     0,    72,     0,   499,     0,   164,   164,
     499,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   373,     0,     0,    75,   374,     0,
     246,     0,     0,     0,   375,   436,    78,   376,   377,   378,
     379,     0,   336,  1187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   440,     0,     0,     0,     0,
       0,   246,     0,   164,     0,     0,     0,   246,     0,     0,
       0,     0,   164,     0,     0,   164,     0,   164,   164,   192,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1289,   499,     0,  1293,   336,   440,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   872,   336,     0,     0,     0,     0,   164,   440,   440,
       0,     0,   603,     0,     0,     0,   603,     0,     0,     0,
       0,     0,     0,   890,     0,   336,     0,   440,     0,     0,
       0,     0,     0,     0,    13,    14,    15,    16,    17,    18,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,     0,     0,
       0,     0,    45,    46,   164,     0,     0,    47,    48,    49,
      50,    51,    52,    53,    54,     0,     0,     0,   246,     0,
       0,     0,    57,   440,     0,     0,     0,     0,     0,   192,
       0,     0,     0,  1391,     0,     0,     0,     0,     0,     0,
       0,  1400,  1401,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    61,   336,     0,    62,    63,    64,    65,
      66,    67,    68,    69,    70,     0,   246,     0,     0,     0,
     499,   499,     0,     0,     0,     0,     0,     0,     0,     0,
     499,   998,     0,   499,  1001,     0,   164,     0,     0,     0,
     192,     0,     0,     0,     0,   336,  1441,     0,   603,     0,
     603,   603,    74,     0,    75,  1450,     0,   603,  1454,     0,
    1457,  1458,     0,     0,     0,   164,     0,   336,   336,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   336,     0,     0,     0,
     499,   164,     0,     0,   499,     0,     0,   164,   499,  1069,
    1484,     0,   499,  1073,     0,     0,     0,     0,     0,     0,
    1076,    13,    14,    15,    16,    17,    18,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,     0,     0,   199,   164,    45,
      46,     0,   336,   499,    47,    48,    49,    50,    51,    52,
      53,    54,     0,   254,     0,     0,     0,  1558,     0,    57,
     440,     0,     0,     0,     0,     0,     0,     0,     0,   603,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   409,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   438,    62,    63,     0,     0,     0,     0,     0,
       0,     0,   199,     0,     0,   466,   302,   466,     0,   336,
       0,     0,   164,   164,     0,     0,     0,   341,     0,     0,
     349,     0,     0,     0,   164,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   199,     0,     0,     0,     0,  1454,
       0,    75,     0,     0,     0,     0,     0,   453,     0,     0,
     457,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1617,     0,
       0,     0,     0,   499,   192,     0,     0,     0,     0,     0,
       0,   192,     0,     0,     0,     0,   246,     0,     0,     0,
     603,   603,     0,   566,     0,     0,     0,   603,     0,     0,
     199,     0,     0,     0,     0,     0,   246,     0,   192,     0,
       0,     0,     0,   254,     0,   164,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   336,
       0,     0,     0,     0,   499,  1291,     0,   499,  1295,   457,
       0,  1691,     0,     0,     0,     0,     0,   199,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   440,   440,   596,   246,   613,     0,     0,
     164,     0,     0,     0,     0,     0,     0,     0,     0,    13,
      14,    15,    16,    17,     0,     0,    19,   246,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,     0,     0,  1739,  1740,    45,    46,   669,
       0,     0,     0,     0,     0,     0,     0,  1744,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    57,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   164,
     336,     0,     0,   199,   466,     0,   603,  1393,     0,     0,
     466,     0,     0,     0,   246,   789,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   336,     0,   192,
       0,     0,     0,   596,     0,     0,     0,     0,     0,   768,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1806,   499,
    1443,     0,     0,     0,     0,     0,     0,     0,   499,  1452,
       0,   603,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   336,   336,     0,     0,   246,     0,     0,     0,
       0,     0,   857,     0,     0,     0,     0,     0,   199,   199,
       0,     0,     0,     0,   453,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   438,
       0,     0,     0,  1857,     0,     0,     0,     0,     0,     0,
       0,     0,   884,     0,   192,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   341,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   453,     0,   876,     0,     0,     0,
       0,     0,     0,   919,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   192,   596,   336,     0,
       0,   789,   939,     0,     0,     0,     0,     0,     0,     0,
       0,   949,     0,   954,   949,     0,     0,     0,   199,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   669,     0,   669,   669,     0,   669,     0,     0,   669,
       0,   982,   669,   669,   669,     0,     0,   440,   440,     0,
       0,     0,     0,     0,   984,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   993,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   438,
       0,     0,   982,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   453,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   499,     0,  1046,
       0,     0,   466,     0,     0,     0,     0,     0,     0,     0,
     199,     0,     0,   499,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   453,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1077,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   453,
     453,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1395,   336,     0,     0,   453,     0,
       0,    13,    14,    15,    16,    17,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   409,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1177,  1179,     0,     0,     0,
       0,     0,     0,   438,     0,     0,     0,   358,     0,     0,
       0,   359,     0,   360,     0,     0,   336,   336,     0,     0,
       0,     0,     0,   440,   466,     0,     0,     0,     0,    57,
     361,   499,   499,   949,   453,     0,     0,     0,     0,     0,
     199,     0,     0,     0,     0,     0,   982,   499,     0,     0,
     768,     0,     0,     0,  1216,     0,     0,   362,   363,     0,
     364,   949,   365,    62,    63,    64,    65,    66,    67,    68,
      69,    70,   366,   367,   355,     0,   368,   369,   370,     0,
     371,   372,     0,     0,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   341,     0,     0,   466,     0,     0,     0,   373,     0,
       0,    75,   374,     0,     0,     0,     0,     0,   375,  1396,
      78,   376,   377,   378,   379,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   499,
       0,     0,     0,     0,     0,     0,     0,   499,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   466,     0,  1282,     0,  1284,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   336,     0,     0,     0,   499,  1859,     0,     0,
     499,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   453,     0,     0,     0,     0,   499,     0,     0,     0,
       0,  1349,  1349,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   669,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   499,   499,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1388,     0,     0,     0,     0,     0,  1398,     0,     0,
       0,   358,     0,     0,     0,   359,     0,   360,     0,   499,
       0,     0,     0,     0,   438,   254,     0,     0,     0,     0,
       0,     0,     0,     0,   361,     0,     0,     0,     0,     0,
       0,   466,     0,     0,     0,   199,     0,     0,     0,     0,
       0,     0,   596,     0,   949,     0,     0,   789,     0,     0,
       0,   362,   363,     0,   364,     0,   365,  1820,    63,    64,
      65,    66,    67,    68,    69,    70,   366,   367,   355,   341,
     368,   369,   370,   669,   371,   372,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1478,     0,
       0,     0,   373,     0,     0,    75,   374,     0,     0,     0,
       0,     0,   375,    77,    78,   376,   377,   378,   379,     0,
       0,     0,     0,     0,     0,     0,  1821,  -173,     0,     0,
       0,     0,     0,     0,   453,   453,   949,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   466,     0,     0,   789,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   669,   669,   669,     0,   669,   669,     0,     0,
       0,     0,     0,   457,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   939,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1573,  1574,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     466,   254,   789,     4,   242,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     0,    19,
     341,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,   358,     0,
      45,    46,   359,     0,   360,    47,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,    56,     0,     0,
      57,   361,     0,     0,     0,     0,     0,     0,     0,     0,
     409,     0,     0,     0,     0,  1644,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   362,   363,
      60,   364,     0,   365,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   366,   367,   355,     0,   368,   369,   370,
       0,   371,   372,     0,     0,     0,     0,  1683,     0,    72,
       0,     0,     0,     0,     0,   199,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   373,
       0,     0,    75,   405,     0,     0,     0,     0,     0,   375,
      77,    78,   376,   377,   378,   379,  1705,     0,     0,  1707,
       0,     0,     0,  1668,  1669,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   341,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   669,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   453,   453,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,     0,     0,     0,     0,    45,    46,     0,
       0,   254,    47,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,    56,     0,     0,    57,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    59,     0,     0,     0,    60,    61,     0,
       0,    62,    63,    64,    65,    66,    67,    68,    69,    70,
       0,     0,   949,    71,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   254,     0,     0,     0,    73,    74,     0,    75,
      76,     0,     0,     0,     0,     0,     0,    77,    78,   669,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   453,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   669,     0,     0,   457,
       4,   242,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,  1101,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,     0,     0,   358,     0,    45,    46,   359,
       0,   360,    47,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,    56,     0,  1102,    57,  1103,    -2,
       0,  1104,     0,     0,  1105,  1106,  1107,  1108,  1109,  1110,
    1111,  1112,  1113,  1114,  1115,  1116,  -281,  1117,  1118,  1119,
    1120,  1121,     0,  1122,     0,   362,   363,    60,   460,     0,
     365,  1123,  1124,    64,    65,    66,    67,    68,    69,    70,
     366,   367,   355,  1125,   368,   369,   370,     0,   371,   372,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    -3,   373,    74,     0,    75,
     405,     0,     0,     0,   278,     0,   375,    77,    78,   376,
     377,   378,   379,     0,     0,     0,     0,     0,     0,     0,
       0,  -173,     4,   242,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,  1101,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,     0,     0,   358,     0,    45,
      46,   359,     0,   360,    47,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,    56,     0,  1102,    57,
    1103,    -2,     0,  1104,     0,     0,  1105,  1106,  1107,  1108,
    1109,  1110,  1111,  1112,  1113,  1114,  1115,  1116,  -281,  1117,
    1118,  1119,  1120,  1121,     0,  1122,     0,   362,   363,    60,
     460,     0,   365,  1123,  1124,    64,    65,    66,    67,    68,
      69,    70,   366,   367,   355,  1125,   368,   369,   370,     0,
     371,   372,     0,     0,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   373,    74,
       0,    75,   405,     0,     0,     0,   278,     0,   375,    77,
      78,   376,   377,   378,   379,     0,     0,     0,     0,     0,
       0,     0,     0,  -173,     4,   242,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,     0,     0,   358,
       0,    45,    46,   359,     0,   360,    47,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,    56,     0,
       0,    57,   361,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   362,
     363,    60,   364,     0,   365,    62,    63,    64,    65,    66,
      67,    68,    69,    70,   366,   367,   355,     0,   368,   369,
     370,     0,   371,   372,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     373,     0,     0,    75,   405,     0,     0,     0,     0,     0,
     375,    77,    78,   376,   377,   378,   379,   242,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,     0,
       0,   358,     0,    45,    46,   359,     0,   360,   317,    48,
      49,    50,    51,    52,    53,    54,     0,     0,     0,     0,
       0,     0,     0,    57,   361,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   362,   363,     0,   364,     0,   365,    62,    63,    64,
      65,    66,    67,    68,    69,    70,   366,   367,   355,     0,
     368,   369,   370,     0,   371,   372,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   373,     0,     0,    75,   435,     0,     0,     0,
       0,     0,   375,   436,    78,   376,   377,   378,   379,   242,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,     0,     0,   358,     0,    45,    46,   359,     0,   360,
     317,    48,    49,    50,    51,    52,    53,    54,     0,     0,
       0,     0,     0,     0,     0,    57,   361,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   362,   363,     0,   364,     0,   365,    62,
      63,    64,    65,    66,    67,    68,    69,    70,   366,   367,
     355,     0,   368,   369,   370,     0,   371,   372,     0,     0,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   373,     0,     0,    75,  1174,     0,
       0,     0,     0,     0,   375,  1175,    78,   376,   377,   378,
     379,   242,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,     0,     0,   358,     0,    45,    46,   359,
       0,   360,   317,    48,    49,    50,    51,    52,    53,    54,
       0,     0,     0,     0,     0,     0,     0,    57,   361,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   362,   363,     0,   364,     0,
     365,    62,    63,    64,    65,    66,    67,    68,    69,    70,
     366,   367,   355,     0,   368,   369,   370,     0,   371,   372,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   373,     0,     0,    75,
     374,     0,     0,     0,     0,     0,   375,    77,    78,   376,
     377,   378,   379,   242,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,     0,     0,   358,     0,    45,
      46,   359,     0,   360,   317,    48,    49,    50,    51,    52,
      53,    54,     0,     0,     0,     0,     0,     0,     0,    57,
     361,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   362,   363,     0,
     364,     0,   365,    62,    63,    64,    65,    66,    67,    68,
      69,    70,   366,   367,   355,     0,   368,   369,   370,     0,
     371,   372,     0,     0,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   373,     0,
       0,    75,   435,     0,     0,     0,     0,     0,   375,    77,
      78,   376,   377,   378,   379,   241,   242,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,     0,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,  -402,  -402,
       0,  -402,    45,    46,     0,  -402,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    57,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    61,     0,     0,    62,    63,    64,    65,
      66,    67,    68,    69,    70,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    73,    74,     0,    75,   243,     0,     0,     0,  -709,
       0,     0,    77,    78,     4,   242,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,     0,     0,     0,
       0,    45,    46,     0,     0,     0,    47,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,    56,     0,
       0,    57,     0,     0,     0,     0,  -334,  -334,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    60,     0,     0,     0,    62,    63,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -334,
       0,     0,     0,    75,    76,     0,     0,     0,     0,     0,
       0,    77,    78,     4,   242,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,     0,     0,
      45,    46,     0,     0,     0,    47,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,    56,     0,     0,
      57,     0,     0,     0,     0,  -335,  -335,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      60,     0,     0,     0,    62,    63,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -335,     0,
       0,     0,    75,    76,     0,     0,     0,     0,     0,     0,
      77,    78,   241,   242,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,     0,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,  -402,  -402,     0,  -402,    45,
      46,     0,  -402,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    57,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      61,     0,     0,    62,    63,    64,    65,    66,    67,    68,
      69,    70,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    73,    74,
       0,    75,   243,     0,     0,  1309,     0,     0,     0,    77,
      78,  1310,     0,     0,    13,    14,    15,    16,    17,    18,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,     0,     0,
       0,     0,    45,    46,     0,     0,     0,    47,    48,    49,
      50,    51,    52,    53,    54,     0,     0,     0,     0,     0,
       0,     0,    57,  1311,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    60,     0,     0,     0,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1312,     0,     0,     0,    75,   915,     0,     0,  1309,     0,
       0,     0,    77,    78,  1310,     0,     0,    13,    14,    15,
      16,    17,    18,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,     0,     0,     0,     0,    45,    46,     0,     0,     0,
      47,    48,    49,    50,    51,    52,    53,    54,     0,     0,
       0,     0,     0,     0,     0,    57,  1311,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    60,     0,     0,     0,    62,
      63,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1490,     0,     0,     0,    75,   915,     0,
       0,  1309,     0,     0,     0,    77,    78,  1310,     0,     0,
      13,    14,    15,    16,    17,    18,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,     0,     0,    45,    46,
       0,     0,     0,    47,    48,    49,    50,    51,    52,    53,
      54,     0,     0,     0,     0,     0,     0,     0,    57,  1311,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    60,     0,
       0,     0,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1491,     0,     0,     0,
      75,   915,     0,     0,  1309,     0,     0,     0,    77,    78,
    1310,     0,     0,    13,    14,    15,    16,    17,    18,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,     0,     0,     0,
       0,    45,    46,     0,     0,     0,    47,    48,    49,    50,
      51,    52,    53,    54,     0,     0,     0,     0,     0,     0,
       0,    57,  1311,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    60,     0,     0,     0,    62,    63,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1492,
       0,     0,     0,    75,   915,     0,     0,     0,     0,     0,
       0,    77,    78,   241,   242,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,     0,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,  -402,  -402,     0,  -402,
      45,    46,     0,  -402,     0,     0,     0,     0,     0,     0,
       0,     0,    13,    14,    15,    16,    17,     0,     0,    19,
      57,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,  -401,  -401,     0,  -401,
      45,    46,     0,  -401,    62,    63,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      57,     0,     0,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   241,     0,     0,     0,
       0,     0,    75,   243,     0,    13,    14,    15,    16,    17,
      77,    78,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,  -402,
    -402,     0,  -402,    45,    46,     0,  -402,     0,     0,     0,
       0,     0,     0,     0,     0,    13,    14,    15,    16,    17,
       0,     0,    19,    57,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,  -402,
    -402,     0,  -402,    45,    46,     0,  -402,    62,    63,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    57,     0,     0,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    75,   300,     0,     0,     0,
       0,     0,     0,    77,    78,   242,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,     0,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,  -402,  -402,     0,
    -402,    45,    46,     0,  -402,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    57,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    61,     0,     0,    62,    63,    64,    65,    66,
      67,    68,    69,    70,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1333,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      73,    74,     0,    75,   243,     0,     0,     0,  -713,   358,
       0,    77,    78,   359,     0,   360,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1102,     0,   361,     0,     0,  1104,  1746,  1747,  1105,  1106,
    1107,  1108,  1109,  1110,  1111,  1112,  1113,  1114,  1115,  1116,
    -281,  1117,  1118,  1119,  1120,  1121,     0,  1122,     0,   362,
     363,     0,   460,     0,   365,  1123,  1124,    64,    65,    66,
      67,    68,    69,    70,   366,   367,   355,  1125,   368,   369,
     370,     0,   371,   372,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     373,    74,     0,    75,   374,     0,     0,     0,   278,     0,
     375,    77,    78,   376,   377,   378,   379,     0,     0,     0,
       0,     0,     0,     0,     0,  -173,   242,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,     0,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,  -402,  -402,
       0,  -402,    45,    46,     0,  -402,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    57,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    61,  1333,     0,    62,    63,    64,    65,
      66,    67,    68,    69,    70,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,   358,     0,     0,     0,   359,
       0,   360,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    73,    74,     0,    75,   243,  1102,     0,   361,    -2,
       0,  1104,    77,    78,  1105,  1106,  1107,  1108,  1109,  1110,
    1111,  1112,  1113,  1114,  1115,  1116,  -281,  1117,  1118,  1119,
    1120,  1121,     0,  1122,     0,   362,   363,     0,   460,     0,
     365,  1123,  1124,    64,    65,    66,    67,    68,    69,    70,
     366,   367,   355,  1125,   368,   369,   370,     0,   371,   372,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,  1333,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   373,    74,     0,    75,
     374,     0,     0,     0,   278,     0,   375,    77,    78,   376,
     377,   378,   379,   358,     0,     0,     0,   359,     0,   360,
       0,  -173,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1102,     0,   361,     0,     0,  1104,
       0,     0,  1105,  1106,  1107,  1108,  1109,  1110,  1111,  1112,
    1113,  1114,  1115,  1116,  -281,  1117,  1118,  1119,  1120,  1121,
       0,  1122,     0,   362,   363,     0,   460,     0,   365,  1123,
    1124,    64,    65,    66,    67,    68,    69,    70,   366,   367,
     355,  1125,   368,   369,   370,     0,   371,   372,     0,     0,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   373,    74,     0,    75,   374,     0,
       0,     0,   278,     0,   375,    77,    78,   376,   377,   378,
     379,     0,     0,     0,     0,     0,     0,     0,     0,  -173,
     242,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,     0,     0,    45,    46,     0,     0,
       0,   317,    48,    49,    50,    51,    52,    53,    54,     0,
      13,    14,    15,    16,    17,    18,    57,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,     0,     0,    45,    46,
      62,    63,     0,   317,    48,    49,    50,    51,    52,    53,
      54,     0,     0,     0,     0,     0,     0,     0,    57,     0,
       0,     0,     0,     0,     0,    72,     0,  1037,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -583,    75,   318,
       0,     0,    62,    63,     0,     0,    77,    78,   242,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      75,     0,     0,     0,    45,    46,     0,     0,     0,   317,
      48,    49,    50,    51,    52,    53,    54,     0,     0,     0,
       0,     0,     0,     0,    57,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    62,    63,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    72,     0,  1723,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    75,   318,     0,     0,
       0,     0,     0,     0,    77,    78,   242,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,     0,     0,
       0,     0,    45,    46,     0,     0,     0,   317,    48,    49,
      50,    51,    52,    53,    54,     0,     0,     0,     0,     0,
       0,     0,    57,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    72,     0,  1725,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    75,   318,     0,     0,     0,     0,
       0,     0,    77,    78,   242,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,     0,     0,
      45,    46,     0,     0,     0,   317,    48,    49,    50,    51,
      52,    53,    54,     0,     0,     0,     0,     0,     0,     0,
      57,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    62,    63,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    75,   318,     0,     0,     0,     0,     0,     0,
      77,    78,   242,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,     0,     0,    45,    46,
       0,     0,     0,   317,    48,    49,    50,    51,    52,    53,
      54,     0,     0,     0,     0,     0,     0,     0,    57,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      75,   300,     0,     0,     0,     0,     0,     0,    77,    78,
     242,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,     0,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,  -402,  -402,     0,  -402,    45,    46,     0,  -402,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    57,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      62,    63,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    75,   243,
       0,     0,     0,     0,     0,     0,    77,    78,    13,    14,
      15,    16,    17,    18,   656,    19,   657,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,   358,     0,    45,    46,   359,     0,
     360,    47,    48,    49,    50,    51,    52,    53,    54,     0,
       0,     0,     0,     0,     0,     0,    57,   361,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   658,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   362,   363,     0,   364,     0,   365,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   366,
     367,   355,     0,   368,   369,   370,     0,   371,   372,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   373,     0,     0,    75,   659,
       0,     0,     0,   278,     0,   375,    77,    78,   660,   661,
     378,   379,    13,    14,    15,    16,    17,    18,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,   358,     0,
      45,    46,   359,     0,   360,    47,    48,    49,    50,    51,
      52,    53,    54,     0,     0,     0,     0,     0,     0,     0,
      57,   361,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   362,   363,
       0,   364,     0,   365,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   366,   367,   355,     0,   368,   369,   370,
       0,   371,   372,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   373,
       0,   404,    75,   405,     0,     0,     0,     0,     0,   375,
      77,    78,   376,   377,   378,   379,    13,    14,    15,    16,
      17,    18,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
       0,     0,   358,     0,    45,    46,   359,     0,   360,    47,
      48,    49,    50,    51,    52,    53,    54,     0,     0,     0,
       0,     0,     0,     0,    57,   361,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   362,   363,     0,   364,     0,   365,    62,    63,
      64,    65,    66,    67,    68,    69,    70,   366,   367,   355,
       0,   368,   369,   370,     0,   371,   372,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   373,     0,     0,    75,   659,     0,     0,
       0,   278,     0,   375,    77,    78,   376,   377,   378,   379,
      13,    14,    15,    16,    17,    18,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,   358,     0,    45,    46,
     359,     0,   360,    47,    48,    49,    50,    51,    52,    53,
      54,     0,     0,     0,     0,     0,     0,     0,    57,   361,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   362,   363,     0,   364,
       0,   365,    62,    63,    64,    65,    66,    67,    68,    69,
      70,   366,   367,   355,     0,   368,   369,   370,     0,   371,
     372,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   373,     0,     0,
      75,   405,     0,     0,     0,     0,     0,   375,    77,    78,
     376,   377,   378,   379,    13,    14,    15,    16,    17,    18,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,     0,     0,
     358,     0,    45,    46,   359,     0,   360,   317,    48,    49,
      50,    51,    52,    53,    54,     0,     0,     0,     0,     0,
       0,     0,    57,   361,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     362,   363,     0,   364,     0,   365,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   366,   367,   355,     0,   368,
     369,   370,     0,   371,   372,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   373,     0,     0,    75,   435,     0,     0,     0,     0,
       0,   375,    77,    78,   376,   377,   378,   379,    13,    14,
      15,    16,    17,    18,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,   358,     0,    45,    46,   359,     0,
     360,   317,    48,    49,    50,    51,    52,    53,    54,     0,
       0,     0,     0,     0,     0,     0,    57,   361,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   362,   363,     0,   364,     0,   365,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   366,
     367,   355,     0,   368,   369,   370,     0,   371,   372,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   373,     0,     0,    75,   374,
       0,     0,     0,     0,     0,   375,    77,    78,   376,   377,
     378,   379,    13,    14,    15,    16,    17,    18,     0,    19,
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
      74,     0,    75,    76,     0,     0,     0,  -711,     0,     0,
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
      74,     0,    75,    76,     0,    13,    14,    15,    16,    17,
      77,    78,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,  -402,
    -402,     0,  -402,    45,    46,     0,  -402,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    57,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    61,     0,     0,    62,    63,    64,
      65,    66,    67,    68,    69,    70,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    73,    74,     0,    75,   300,     0,     0,     0,
       0,     0,     0,    77,    78,   554,   242,     6,     7,     8,
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
       0,     0,  1410,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   921,    75,   915,     0,     0,
      62,    63,     0,     0,    77,    78,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    75,   915,
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
       0,     0,     0,     0,     0,     0,     0,     0,    75,   285,
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
      45,    46,    62,    63,     0,   317,    48,    49,    50,    51,
      52,    53,    54,     0,     0,     0,     0,     0,     0,     0,
      57,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      75,   431,     0,     0,    62,    63,     0,     0,    77,    78,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    75,   318,     0,     0,     0,     0,     0,     0,
      77,    78,    13,    14,    15,    16,    17,    18,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,     0,     0,
      45,    46,     0,     0,     0,   317,    48,    49,    50,    51,
      52,    53,    54,     0,    13,    14,    15,    16,    17,    18,
      57,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,     0,     0,
       0,     0,    45,    46,    62,    63,     0,   317,    48,    49,
      50,    51,    52,    53,    54,     0,     0,     0,     0,     0,
       0,     0,    57,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    75,   285,     0,     0,    62,    63,     0,     0,
      77,    78,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    75,   431,     0,     0,     0,     0,
       0,     0,    77,    78,   241,   242,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,     0,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,  -402,  -402,     0,
    -402,    45,    46,     0,  -402,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    13,    14,    15,    16,    17,
      18,    57,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,     0,
       0,     0,     0,    45,    46,    62,    63,     0,   317,    48,
      49,    50,    51,    52,    53,    54,     0,    13,    14,    15,
      16,    17,    18,    57,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,     0,     0,    75,     0,    45,    46,    62,    63,     0,
      47,    48,    49,    50,    51,    52,    53,    54,     0,     0,
       0,     0,     0,     0,     0,    57,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    75,   300,     0,     0,    62,
      63,     0,     0,    77,    78,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    75,   915,     0,
       0,     0,     0,     0,     0,    77,    78,    13,    14,    15,
      16,    17,    18,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,     0,     0,     0,     0,    45,    46,     0,     0,     0,
     317,    48,    49,    50,    51,    52,    53,    54,     0,    13,
      14,    15,    16,    17,    18,    57,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,     0,     0,     0,     0,    45,    46,    62,
      63,     0,   317,    48,    49,    50,    51,    52,    53,    54,
       0,     0,     0,     0,     0,     0,     0,    57,     0,     0,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    75,   915,     0,
       0,    62,    63,     0,     0,    77,    78,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    75,
       0,     0,    13,    14,    15,    16,    17,    77,    78,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,  -402,  -402,     0,  -402,
      45,    46,     0,  -402,     0,     0,     0,     0,     0,     0,
       0,     0,    13,    14,    15,    16,    17,     0,     0,    19,
      57,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,  -402,  -402,     0,  -402,
      45,    46,     0,  -402,    62,    63,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      57,     0,     0,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    75,   300,    62,    63,     0,     0,     0,     0,
      77,    78,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    75,     0,     0,     0,     0,     0,     0,     0,
      77,    78,   242,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,     0,     0,    45,    46,
       0,     0,     0,   317,    48,    49,    50,    51,    52,    53,
      54,     0,     0,     0,     0,     0,     0,     0,    57,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   842,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -596,
      75,   242,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,     0,     0,     0,     0,    45,    46,     0,
       0,     0,   317,    48,    49,    50,    51,    52,    53,    54,
       0,     0,     0,     0,     0,     0,     0,    57,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    62,    63,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1652,     0,
       0,     0,     0,   242,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,     0,    19,    75,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,     0,     0,     0,     0,    45,
      46,     0,     0,     0,   317,    48,    49,    50,    51,    52,
      53,    54,     0,     0,     0,     0,     0,     0,     0,    57,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     242,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    62,    63,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,  -402,  -402,     0,  -402,    45,    46,     0,  -402,
       0,     0,   358,     0,     0,     0,   359,     0,   360,     0,
       0,    75,     0,     0,     0,     0,    57,     0,     0,     0,
       0,     0,     0,     0,     0,   361,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      62,    63,   362,   363,     0,   460,     0,   365,    62,    63,
      64,    65,    66,    67,    68,    69,    70,   366,   367,   355,
       0,   368,   369,   370,   358,   371,   372,     0,   359,     0,
     360,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   361,    75,     0,
       0,     0,     0,   373,    74,     0,   461,   462,     0,     0,
       0,   463,     0,   375,    77,    78,   376,   377,   378,   379,
       0,     0,     0,     0,   362,   363,     0,   364,     0,   365,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   366,
     367,   355,     0,   368,   369,   370,   358,   371,   372,     0,
     359,     0,   360,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   361,
       0,     0,     0,     0,     0,   373,  1219,     0,    75,   374,
       0,     0,     0,  1220,     0,   375,    77,    78,   376,   377,
     378,   379,     0,     0,     0,     0,   362,   363,     0,   364,
       0,   365,    62,    63,    64,    65,    66,    67,    68,    69,
      70,   366,   367,   355,     0,   368,   369,   370,   358,   371,
     372,     0,   359,     0,   360,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   361,     0,     0,     0,     0,     0,   373,     0,     0,
      75,   374,     0,     0,     0,   463,     0,   375,    77,    78,
     376,   377,   378,   379,     0,     0,     0,     0,   362,   363,
       0,   364,     0,   365,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   366,   367,   355,     0,   368,   369,   370,
     358,   371,   372,     0,   359,     0,   360,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   361,     0,     0,     0,     0,     0,   373,
     788,     0,    75,   374,     0,     0,     0,     0,     0,   375,
      77,    78,   376,   377,   378,   379,     0,     0,     0,     0,
     362,   363,     0,   364,     0,   365,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   366,   367,   355,     0,   368,
     369,   370,   358,   371,   372,     0,   359,     0,   360,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   361,     0,     0,     0,     0,
       0,   373,     0,     0,    75,   374,     0,     0,     0,   278,
       0,   375,    77,    78,   376,   377,   378,   379,     0,     0,
       0,     0,   362,   363,     0,   364,     0,   365,    62,    63,
      64,    65,    66,    67,    68,    69,    70,   366,   367,   355,
       0,   368,   369,   370,   358,   371,   372,     0,   359,     0,
     360,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   361,     0,     0,
       0,     0,     0,   373,   946,     0,    75,   374,     0,     0,
       0,     0,     0,   375,    77,    78,   376,   377,   378,   379,
       0,     0,     0,     0,   362,   363,     0,   364,     0,   365,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   366,
     367,   355,     0,   368,   369,   370,   358,   371,   372,     0,
     359,     0,   360,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   361,
       0,     0,     0,     0,     0,   373,     0,     0,    75,   374,
       0,     0,   976,     0,     0,   375,    77,    78,   376,   377,
     378,   379,     0,     0,     0,     0,   362,   363,     0,   364,
       0,   365,    62,    63,    64,    65,    66,    67,    68,    69,
      70,   366,   367,   355,     0,   368,   369,   370,   358,   371,
     372,     0,   359,     0,   360,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   361,     0,     0,     0,     0,     0,   373,  1283,     0,
      75,   374,     0,     0,     0,     0,     0,   375,    77,    78,
     376,   377,   378,   379,     0,     0,     0,     0,   362,   363,
       0,   364,     0,   365,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   366,   367,   355,     0,   368,   369,   370,
     358,   371,   372,     0,   359,     0,   360,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   361,     0,     0,     0,     0,     0,   373,
       0,     0,    75,   374,     0,     0,     0,  1343,     0,   375,
      77,    78,   376,   377,   378,   379,     0,     0,     0,     0,
     362,   363,     0,   364,     0,   365,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   366,   367,   355,     0,   368,
     369,   370,   358,   371,   372,     0,   359,     0,   360,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   361,     0,     0,     0,     0,
       0,   373,     0,  1752,    75,   374,     0,     0,     0,     0,
       0,   375,    77,    78,   376,   377,   378,   379,     0,     0,
       0,     0,   362,   363,     0,   364,     0,   365,    62,    63,
      64,    65,    66,    67,    68,    69,    70,   366,   367,   355,
       0,   368,   369,   370,   358,   371,   372,     0,   359,     0,
     360,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   361,     0,     0,
       0,     0,     0,   373,  1952,     0,    75,   374,     0,     0,
       0,     0,     0,   375,    77,    78,   376,   377,   378,   379,
       0,     0,     0,     0,   362,   363,     0,   364,     0,   365,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   366,
     367,   355,     0,   368,   369,   370,   358,   371,   372,     0,
     359,     0,   360,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   361,
       0,     0,     0,     0,     0,   373,     0,     0,    75,   374,
       0,     0,     0,     0,     0,   375,    77,    78,   376,   377,
     378,   379,     0,     0,     0,     0,   362,   363,     0,   364,
       0,   365,    62,    63,    64,    65,    66,    67,    68,    69,
      70,   366,   367,   355,     0,   368,   369,   370,   358,   371,
     372,     0,   359,     0,   360,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   361,     0,     0,     0,     0,     0,   645,     0,     0,
      75,   374,     0,     0,     0,     0,     0,   375,    77,    78,
     376,   377,   378,   379,     0,     0,     0,     0,   362,   363,
       0,   364,     0,   365,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   366,   367,   355,     0,   368,   369,   370,
     358,   371,   372,     0,   359,     0,   360,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   361,     0,     0,     0,     0,     0,   650,
       0,     0,    75,   374,     0,     0,     0,     0,     0,   375,
      77,    78,   376,   377,   378,   379,     0,     0,     0,     0,
     362,   363,     0,   364,     0,   365,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   366,   367,   355,     0,   368,
     369,   370,   358,   371,   372,     0,   359,     0,   360,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   361,     0,     0,     0,     0,
       0,   653,     0,     0,    75,   374,     0,     0,     0,     0,
       0,   375,    77,    78,   376,   377,   378,   379,     0,     0,
       0,     0,   362,   363,     0,   364,     0,   365,    62,    63,
      64,    65,    66,    67,    68,    69,    70,   366,   367,   355,
       0,   368,   369,   370,   358,   371,   372,     0,   359,     0,
     360,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   361,     0,     0,
       0,     0,     0,   373,     0,     0,    75,   374,     0,     0,
       0,     0,     0,   375,   856,    78,   376,   377,   378,   379,
       0,     0,     0,     0,   362,   363,     0,   364,     0,   365,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   366,
     367,   355,     0,   368,   369,   370,     0,   371,   372,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   373,     0,     0,    75,   374,
       0,     0,     0,     0,     0,   375,   436,    78,   376,   377,
     378,   379
};

static const yytype_int16 yycheck[] =
{
       1,   149,   218,     4,   282,   176,     1,    73,   161,    94,
       1,    73,     1,     4,   630,    73,   219,    73,     4,   373,
       1,   683,   161,   321,   406,   240,   172,   463,   670,   255,
     663,   951,   218,   177,   864,   218,   206,   611,   604,    81,
       1,   218,   598,     4,     1,   851,  1689,    75,     1,   727,
     218,   138,   627,   756,    55,    56,   757,    58,   602,   221,
     980,  1627,   763,    58,  1750,   598,    73,    58,   162,    58,
     754,   218,    73,   533,    73,   218,  1627,    58,   294,  1193,
      81,   177,  1628,  1111,   598,  1627,    81,   235,    89,    58,
      73,   336,   295,    94,   228,   340,    97,    58,   756,    94,
     101,    58,    97,   190,   146,    58,   101,   851,   294,  1746,
     101,   294,    96,   573,   180,   101,    87,   294,   180,   253,
      81,   512,   513,  1043,   180,     1,   294,   289,   290,   263,
     191,   835,    70,   754,   219,   754,   115,   754,   139,   350,
      82,   142,   353,   144,    87,   443,   774,   294,   149,   144,
     314,   294,   218,   144,   155,   144,   218,   861,  1268,  1269,
     218,   162,   218,   144,   792,   149,   151,  1090,     0,    59,
      60,   156,  1226,   180,   153,   144,    70,   138,   179,   180,
     130,   180,    58,   144,   179,   146,   157,   144,   352,  1735,
       1,   144,   758,   194,   773,   774,   762,   180,   754,   194,
    1095,   120,   244,   204,   779,   771,   772,   149,   209,   820,
     295,   218,   756,   792,   157,   153,  1902,   218,   219,   218,
     161,   754,   172,  1095,   219,  1862,   571,    73,   294,   190,
     858,  1095,   294,   152,   235,   218,   294,  1160,   294,     0,
     754,   632,    88,   244,  1930,   273,   628,    58,   373,   244,
     912,   131,     1,   254,     0,   149,   257,   131,    95,   301,
     155,     1,   257,   264,     4,   481,   877,   157,   144,   495,
     165,    70,   115,   274,   275,   146,   277,    70,   940,   858,
     241,  1924,   560,   244,   174,   165,  1832,   490,  1402,   905,
     101,   165,   590,   294,   295,   481,   929,  1103,   481,   469,
     295,   302,   173,   319,   481,   149,  1007,   308,   309,    58,
    1013,   989,   313,   481,   149,  1881,   614,   264,    58,   149,
     336,   282,  1006,   621,   340,   282,  1132,   681,   275,   282,
    1881,   906,   149,   144,   481,    70,  1882,   174,   481,  1881,
     301,   885,   587,   734,   345,     1,   433,   441,    97,   350,
     149,   567,   353,   354,   153,  1013,   149,    97,   149,  1103,
     153,   101,   510,   155,   148,   407,   569,    87,   516,  1915,
      70,   155,   832,   165,   985,  1429,  1430,  1431,   319,   624,
    1303,   567,   173,  1087,   567,  1006,    19,  1006,  1132,  1006,
     567,  1002,  1003,    73,    81,   144,    70,    56,   532,   567,
      59,    60,    58,    62,   144,   490,   282,   104,   105,    89,
     149,  1306,  1307,  1308,   149,   481,   417,   156,   153,   481,
     567,   437,   417,  1525,   567,   481,   106,  1055,   773,   774,
     149,    70,   571,   461,  1306,  1307,  1308,   157,    94,   440,
     441,    97,  1306,  1307,  1308,   101,   407,   792,  1550,   149,
    1006,   452,   453,   153,   149,   152,    70,   518,   227,   146,
     461,   230,   463,   148,  1030,   695,   696,   697,   698,  1013,
     155,   282,   433,  1006,   481,   149,  1055,    75,    76,   153,
     481,  1182,   481,   252,   569,    75,    76,   155,   144,   490,
     149,   178,  1006,   262,    87,   490,   437,   155,   481,    70,
     131,   567,  1612,  1613,  1424,   567,   174,   157,   257,   510,
     149,   567,   150,   858,   153,   516,   174,   257,   154,   157,
     645,    70,   647,   648,   174,   650,    70,   588,   653,   160,
     161,   656,   657,   658,   174,   149,    70,   708,   194,   153,
      12,    13,    14,    15,    16,   843,  1864,   983,    12,    13,
      14,    15,    16,   554,    70,   556,  1257,   244,   157,   157,
    1878,    70,   155,   149,   157,   151,   567,   157,   569,   191,
     155,   587,   321,  1189,   569,   174,   874,    70,   149,   151,
     925,   582,   153,   102,   156,   586,  1904,   106,   149,   174,
     109,   146,   111,   554,  1236,   611,   157,   152,    70,   560,
     149,   257,  1176,   560,   153,   149,    70,   560,   624,   153,
      70,   781,    70,   151,   301,   149,   155,   618,   173,   153,
     146,  1196,   151,   767,   162,   163,   155,   872,   149,   630,
     571,   157,   319,   149,   778,   588,   157,   153,  1249,  1250,
     149,  1669,   174,   814,   153,  1673,   794,   173,   155,   710,
     157,     3,  1263,  1264,   125,   126,   149,     3,     1,   131,
     153,     4,   756,  1325,   151,   813,   153,   131,  1243,  1499,
     611,   767,   152,   674,   675,  1595,   677,  1597,   162,   163,
     681,   709,   155,   684,   560,  1296,  1297,   155,  1798,   149,
     751,   149,   155,   153,   443,   153,    70,   319,   169,   170,
     322,   174,   151,   155,  1346,   151,   174,   156,   709,   155,
    1055,   174,   773,   774,   336,    58,   155,   155,   340,  1525,
     407,  1527,   174,    12,    13,    14,    15,    16,   247,   155,
      73,   792,   160,   903,   503,  1763,   174,   151,    81,   167,
     168,   149,   156,   151,  1550,   153,   149,   558,   174,   560,
     437,    94,   157,   754,    97,   756,   525,     1,   101,   151,
       3,   417,   531,   155,   152,   153,   535,   768,   156,    12,
      13,    14,    15,    16,   775,   688,   689,   690,  1169,  1421,
     781,    70,   157,   784,  1220,   149,   925,   151,   173,   153,
     149,  1819,   793,   794,   795,   171,   149,   858,   751,  1827,
     153,   144,   896,   115,   323,   324,   149,   326,   557,   328,
     149,  1193,   813,   151,    58,   437,  1042,    56,   151,   162,
      59,    60,   155,    62,  1399,   512,   513,    70,   149,   149,
       3,   151,   976,   153,   490,   129,   179,   180,   151,    12,
      13,    14,    15,    16,   151,  1420,  1874,    70,   849,   850,
     851,   194,  1490,  1491,  1492,   149,   872,   101,  1203,   153,
     851,   204,   851,   104,   105,   614,   160,   161,  1479,  1480,
     851,   151,   863,   104,   105,   218,   219,   863,   101,   173,
    1050,   151,  1180,   106,   107,   108,   109,   110,   111,   112,
     851,   151,   235,   149,   851,   896,   518,    70,   851,   900,
     144,   244,   863,   149,   905,   155,   129,  1343,  1052,   129,
     911,   149,  1074,   151,   257,   153,   151,   161,  1012,  1013,
     155,   151,   129,  1268,   611,   155,   149,   150,   149,   149,
      21,   274,   151,   153,   277,   151,   155,   160,   161,   155,
     160,   161,   149,   944,   149,   632,   153,   191,   153,   571,
     951,   294,   295,   160,   161,   151,  1310,  1532,   149,   155,
    1766,  1572,   153,   149,  1580,   587,   588,   153,   149,   118,
    1185,   120,   121,   122,   151,   851,   151,   149,   155,   980,
     155,   153,   983,  1419,   925,    46,    47,   155,    49,   611,
     155,  1602,    53,   155,  1055,   151,  1607,  1608,   155,   155,
     149,    96,   624,   152,   153,  1006,   149,  1813,   157,   158,
     529,  1012,  1013,   149,  1589,  1163,   157,   129,   151,   152,
    1402,    87,   266,  1829,  1170,  1171,   157,     4,     5,     6,
       7,     8,     9,    10,    11,   127,   128,   149,   282,  1384,
     851,   153,  1043,  1092,  1093,  1094,   151,   734,   160,   161,
     155,   154,   155,  1095,   143,   144,   145,   151,   151,  1865,
     157,   155,   155,   151,  1203,   834,   155,   155,   143,   144,
     145,   148,  1298,   151,   417,   319,   165,   155,   322,   151,
     155,   154,  1207,   155,   151,   174,  1702,   151,   710,   151,
     165,   155,   336,   155,   843,  1239,   340,   151,   441,   174,
     157,   155,   851,   173,  1105,   854,   151,  1108,  1109,  1110,
     155,   851,  1103,  1316,  1103,   157,  1269,   151,   461,  1781,
     463,   155,  1103,   863,   157,   874,   151,   151,  1703,   751,
     155,  1132,  1276,  1277,  1095,   151,   151,  1138,   481,   155,
     155,  1132,  1103,  1132,   115,  1146,  1103,   490,  1149,  1150,
    1103,  1132,  1153,  1886,  1149,  1150,   149,  1890,   149,  1150,
    1176,   149,  1163,   155,  1150,   123,   124,   510,   154,   155,
     166,  1132,   161,   516,   151,  1132,   158,   864,   158,  1132,
    1276,  1277,   160,   161,    12,   154,   155,   159,  1189,   154,
     155,   154,   155,   437,   154,   155,   171,   174,   108,   109,
     110,   111,   112,  1204,   155,   156,   129,  1268,  1269,   154,
     155,   554,   152,   556,  1789,   154,   155,   154,   155,  1220,
     154,   155,   154,   155,   567,  1226,   569,  1103,   154,   155,
     151,  1316,   154,   155,   151,  1176,   154,   155,    12,    13,
      14,    15,    16,   586,   153,  1384,   151,  1525,   155,   156,
     872,   154,   155,   875,   154,   155,  1132,  1258,    86,   154,
     155,   151,  1203,   151,  1306,  1307,  1308,  1612,  1310,  1311,
     154,   155,  1550,   101,   518,   618,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   154,   155,  1503,  1899,   154,
     155,   151,  1103,   154,  1497,   131,    70,   154,   155,   155,
     156,   131,  1505,   925,   156,   549,     4,     5,     6,     7,
       8,     9,    10,    11,   558,  1316,   560,  1503,  1309,  1320,
    1503,  1132,  1323,  1309,    75,    76,  1503,   571,   155,   156,
     156,   674,   675,   156,   677,  1503,  1227,  1228,   681,  1150,
     155,   684,  1343,   587,   588,  1306,  1307,  1308,  1309,  1310,
    1311,   691,   692,   149,  1103,   129,  1503,   693,   694,   151,
    1503,   151,  1363,  1103,  1365,    63,   709,   611,   699,   700,
    1365,   151,  1510,  1511,   151,   149,  1170,  1171,  1549,   153,
     624,   151,   151,  1132,   149,   153,   160,   161,   101,    68,
     154,   157,  1132,   106,   107,   108,   109,   110,   111,   112,
    1149,    12,    13,    14,    15,    16,    17,   157,  1561,  1149,
    1150,   754,  1497,   756,   157,   157,  1407,   154,  1419,   149,
    1505,  1407,  1561,  1424,    76,   154,  1642,    17,  1429,  1430,
    1431,  1180,   173,   155,   157,   149,   174,   150,   781,   151,
     153,   784,  1645,  1384,   151,  1503,  1407,   157,   174,   157,
     793,   794,   795,   154,  1515,   154,  1642,    17,   148,  1642,
    1613,   157,  1587,   151,   151,  1642,   710,   151,   151,   151,
     813,   151,   148,   151,  1642,    12,    13,    14,    15,    16,
      17,   151,  1169,    12,    13,    14,    15,    16,    17,  1176,
     157,   157,   151,  1149,  1150,  1642,  1497,    68,   173,  1642,
     174,   151,  1503,   151,  1505,   151,   849,   751,   148,   101,
     157,  1512,   151,  1684,   106,   107,   108,   109,   110,   111,
     112,   113,   151,  1524,   155,  1526,   151,   151,   155,   773,
     774,   151,   151,   151,  1525,  1833,  1527,   151,   151,   151,
     173,  1744,   151,   151,   151,   151,  1694,   151,   792,   151,
     151,  1612,  1613,   896,  1176,   151,   154,  1558,   150,  1550,
    1645,   153,   151,   151,  1525,   148,  1527,   151,  1525,  1309,
    1527,   148,  1525,  1564,  1527,   151,   820,   101,  1564,  1580,
     155,  1203,   106,   107,   108,   109,   110,   111,   112,  1550,
     149,   149,   149,  1550,  1595,   149,  1597,  1550,   149,   149,
      13,   944,  1746,  1564,   156,    72,  1777,   851,   951,   155,
     174,    89,   156,   174,   858,   154,  1365,   148,   154,   174,
    1561,   174,   148,   157,   151,  1365,   150,   155,   872,   174,
     151,   875,   154,   877,   151,   155,   155,   980,   151,   155,
     983,  1642,   154,   151,  1645,  1798,   148,   148,   229,  1525,
    1746,  1527,   149,   149,  1655,    78,   174,   151,   151,  1744,
     174,   149,   148,  1006,   174,  1881,   149,  1407,   174,  1012,
    1013,   174,   151,   155,  1550,  1666,  1677,   148,   148,  1882,
    1666,   925,   174,    17,  1832,   549,    12,    13,    14,    15,
      16,    17,  1693,  1694,   174,  1881,   155,   174,  1881,   174,
    1043,  1702,   154,   154,  1881,   154,   151,   154,   148,  1365,
     151,   157,  1915,  1881,  1525,  1886,  1527,  1861,  1862,  1890,
    1891,    55,    56,    57,    58,    59,    60,    61,    62,   156,
     156,  1773,   118,   148,  1881,   151,   154,  1798,  1881,  1550,
     151,   985,   151,  1744,  1888,   151,  1917,   154,   151,  1750,
     148,   156,   174,  1754,   149,   149,   149,  1773,  1002,  1003,
     107,   148,  1384,   154,  1765,  1861,  1862,  1938,   157,  1913,
     154,  1942,   154,   148,   151,  1766,  1525,  1778,  1527,   151,
     151,   151,   151,   149,   151,  1525,   154,  1527,  1830,   151,
     155,   148,  1888,   174,    88,   151,  1967,  1882,   151,   154,
     154,  1550,   148,  1146,   148,  1766,  1149,  1150,   151,  1766,
    1550,  1055,  1499,  1766,  1830,  1881,  1960,   153,   151,  1881,
    1163,   151,  1813,  1881,  1564,  1881,   151,   151,   151,   410,
    1915,  1832,   174,  1834,   156,   174,   174,   174,  1829,   156,
     151,   148,   174,   151,  1845,   426,  1847,  1848,   429,   151,
       1,   151,  1813,     4,   155,  1666,  1813,   152,   174,  1103,
    1813,  1204,   174,   156,  1960,   101,   149,   155,  1829,   149,
      73,   148,  1829,   154,  1865,   150,  1829,  1220,   150,   107,
    1881,  1882,   174,  1226,   174,   107,   165,  1882,  1132,   101,
    1766,  1892,   104,   105,   106,   107,   108,   109,   110,   111,
     112,  1902,   165,   151,  1865,   486,  1150,    58,  1865,   156,
     151,   148,  1865,   148,  1915,  1258,  1665,   151,   149,   174,
    1915,    73,    73,   174,   151,  1665,  1666,  1928,   151,  1930,
      81,   174,  1176,   375,  1587,  1233,   703,  1813,   150,   701,
    1132,   662,   702,    94,  1945,   704,    97,   705,  1930,  1950,
     101,  1121,  1862,  1829,  1550,  1766,   820,  1878,  1676,  1203,
    1766,  1822,   174,  1964,  1782,  1925,  1924,  1968,  1912,  1542,
    1830,  1891,  1542,  1316,  1942,  1829,    48,  1978,    12,    13,
      14,    15,    16,   249,  1153,  1497,  1744,   138,  1311,  1865,
    1806,  1511,  1146,   144,   130,   146,   781,   470,   149,   150,
    1343,   867,  1813,   582,  1564,  1249,  1250,  1756,  1407,     0,
     161,   726,   911,   877,    -1,   726,  1756,  1766,  1829,  1263,
    1264,   726,  1365,    -1,  1268,  1269,  1766,   178,   179,   180,
      -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,    97,   190,
     191,    -1,    -1,   194,    -1,    -1,    -1,    -1,    -1,   108,
      -1,    -1,  1296,  1297,  1865,    -1,    12,    13,    14,    15,
      16,    -1,    -1,    -1,  1813,    -1,    -1,   218,   219,    -1,
      -1,    -1,    -1,  1813,   210,    -1,  1419,    -1,    -1,    -1,
    1829,  1424,    -1,    -1,   235,    -1,  1429,  1430,  1431,  1829,
      -1,   150,   101,   244,    -1,   129,    -1,   106,   107,   108,
     109,   110,   111,   112,   113,    -1,   257,    -1,   117,    -1,
     119,    -1,    -1,    -1,    70,   149,  1865,    -1,    -1,   153,
      -1,   985,    17,   101,    -1,  1865,   160,   161,   106,   107,
     108,   109,   110,   111,   112,   194,   287,    -1,  1002,  1003,
    1384,   150,   293,   294,   295,   726,   727,   283,     9,    -1,
     301,  1773,    -1,    -1,  1497,   736,    -1,   361,   739,    -1,
    1503,    -1,  1505,    -1,    59,    60,    61,    62,   319,   320,
     321,   149,   150,   129,    -1,    -1,    -1,     9,    -1,    -1,
      -1,    -1,   386,   387,    -1,   336,    -1,    -1,    -1,   340,
      -1,    -1,    -1,   149,    -1,    -1,    -1,   153,   257,  1105,
      -1,    -1,    -1,   407,   160,   161,   101,    -1,  1830,    -1,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   800,
      -1,    -1,   373,   804,    -1,    -1,    -1,   808,   287,    -1,
      -1,    -1,    -1,   437,   293,  1479,  1480,   373,    -1,    -1,
     101,    -1,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,  1595,    -1,  1597,   150,   407,    -1,   153,   410,
      -1,    -1,   321,    -1,    -1,    -1,   417,    -1,    -1,   101,
      -1,  1515,   104,   105,   106,   107,   108,   109,   110,   111,
     112,  1525,   433,  1527,    -1,    -1,   437,    -1,    -1,   150,
     441,    -1,   443,    -1,    -1,    -1,    -1,    -1,    -1,  1642,
      -1,    -1,  1645,    -1,    -1,    -1,  1550,    -1,    -1,    -1,
      -1,    -1,    -1,    76,    -1,    -1,    -1,  1561,   150,     4,
       5,     6,     7,     8,     9,    10,    11,    -1,  1572,    -1,
     481,    -1,    -1,    -1,    63,    64,    65,    66,   101,   490,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      -1,  1694,    -1,    -1,    -1,    -1,    -1,    -1,  1602,   510,
      -1,   512,   513,  1607,  1608,   516,   502,   518,  1612,  1613,
      -1,   507,   101,    -1,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   443,  1249,  1250,   150,   524,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   534,  1263,
    1264,  1744,    -1,    -1,    -1,   556,    -1,    -1,   989,    -1,
      -1,   174,    -1,    -1,    -1,    -1,   567,    -1,   569,    -1,
     571,   150,  1666,    -1,   153,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1296,  1297,    -1,    -1,   587,   588,    -1,   590,
      -1,    -1,   171,    -1,     3,    -1,    -1,   598,    -1,    -1,
      -1,   602,    -1,    -1,    -1,    -1,    -1,  1363,   517,    -1,
     611,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     621,    -1,    -1,   624,    -1,    -1,    -1,    -1,   537,  1060,
      -1,   632,  1063,   687,    -1,    -1,    -1,    -1,    -1,  1832,
      -1,  1834,    -1,    -1,   645,    -1,   647,   648,   557,   650,
      -1,    -1,   653,    -1,    -1,   656,   657,   658,    -1,   645,
      -1,    -1,    -1,    -1,   650,    -1,    -1,   653,    -1,    -1,
      -1,    -1,  1766,    -1,    -1,    -1,    -1,    -1,    -1,  1773,
      -1,   590,    -1,    -1,    -1,    -1,   672,    -1,  1881,  1882,
      -1,    -1,   101,    -1,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,  1798,   614,    -1,    -1,    -1,   710,
      -1,    -1,   621,    -1,    -1,    -1,    -1,    -1,    -1,  1813,
     129,    -1,  1915,    -1,    -1,   726,   727,    -1,    -1,    -1,
      -1,    -1,    -1,   734,    -1,  1829,  1830,    -1,    -1,    -1,
     149,   150,    -1,    -1,    -1,    -1,    -1,   156,    -1,    -1,
     751,   160,   161,   754,    -1,   756,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1479,  1480,    -1,  1524,    -1,
    1526,  1865,   773,   774,    -1,  1968,    -1,    -1,    -1,   373,
       1,    -1,    -1,     4,    -1,  1978,    -1,    -1,    -1,    -1,
      70,   792,    -1,   794,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1515,  1558,    -1,    -1,  1899,    -1,    -1,    -1,    -1,
      -1,    -1,   813,    -1,  1245,    -1,    -1,    -1,    -1,    -1,
      -1,   101,    -1,  1254,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   819,    -1,    -1,    -1,    58,    -1,    -1,
      -1,    -1,    -1,    -1,   830,    -1,    -1,   833,    -1,    -1,
     851,   837,    -1,    -1,    -1,    -1,    -1,   858,  1572,    -1,
      81,    -1,   863,   864,    -1,    -1,    -1,   921,    -1,   149,
     150,   872,   926,   874,    -1,    -1,    97,    -1,    -1,    -1,
     101,    -1,    -1,   937,   885,    -1,    -1,    -1,  1602,    -1,
      -1,    -1,    -1,  1607,  1608,    -1,    -1,    -1,   101,  1655,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   138,   512,   513,
      -1,  1677,    -1,   144,   925,   146,    -1,    -1,    -1,   150,
      -1,    -1,    -1,    -1,   843,    -1,    -1,    -1,    -1,   160,
     161,   162,    -1,    -1,    -1,   854,    -1,   150,    12,    13,
      14,    15,    16,    -1,    -1,    -1,    -1,   178,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   874,    -1,    -1,    -1,   190,
     191,   174,    -1,   194,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   989,    -1,
      -1,    -1,    -1,    -1,  1750,    -1,    -1,    -1,  1754,    -1,
      94,    -1,    -1,    -1,    -1,  1006,    70,    -1,    -1,  1765,
      -1,    -1,  1013,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     241,    -1,  1778,   244,   101,    -1,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   257,   101,    -1,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,  1103,
      -1,   645,    -1,   274,  1055,   149,   650,    -1,    -1,   653,
      -1,   282,  1493,    -1,    -1,   129,   287,    -1,    -1,    -1,
      -1,    -1,   293,   150,    -1,    -1,    -1,    -1,   672,    -1,
     301,    -1,    -1,   160,    -1,   149,   150,    -1,    -1,  1845,
      -1,  1847,  1848,    -1,  1095,    -1,   160,   161,   319,    -1,
     321,   322,  1103,  1089,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   706,    -1,    -1,   336,  1102,    -1,    -1,   340,
      -1,    -1,  1176,    -1,   218,   219,    -1,    -1,    -1,    -1,
      -1,  1132,    -1,  1119,    -1,    -1,  1892,    -1,    -1,    -1,
      -1,   235,    -1,    -1,    -1,    -1,  1902,    -1,  1149,  1150,
      -1,    -1,   373,    -1,    -1,    -1,  1210,  1211,  1212,    -1,
      -1,    -1,  1163,  1217,  1218,    -1,    -1,    -1,  1169,    -1,
      -1,   101,  1928,    -1,  1930,  1176,   106,   107,   108,   109,
     110,   111,   112,    -1,    -1,  1899,   407,    -1,    -1,  1945,
      -1,    -1,    -1,    -1,  1950,    -1,  1627,  1628,    -1,   129,
     294,   295,  1203,    -1,    -1,    -1,  1207,    -1,  1964,    -1,
      70,    -1,   433,    -1,    -1,    -1,   437,    -1,    -1,   149,
     150,    -1,   443,   153,    -1,    12,    13,    14,    15,    16,
     160,   161,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1149,   101,    -1,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    55,    56,    -1,    -1,    -1,    -1,  1268,  1269,   129,
      -1,  1180,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,   149,
     150,   512,   513,   153,    -1,    89,   517,   518,    -1,    -1,
     160,   161,    -1,    -1,  1735,  1306,  1307,  1308,  1309,  1310,
    1311,    -1,    -1,    -1,   101,  1316,  1317,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    -1,    -1,   549,    -1,
      -1,    -1,    -1,   554,    -1,    -1,   557,   558,    -1,   560,
    1326,  1327,   129,    -1,    -1,   139,    -1,   441,   142,   101,
     571,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   155,   149,   150,  1365,   586,   587,   588,    -1,   590,
      -1,  1802,    -1,   160,   161,  1806,    -1,    -1,  1364,    -1,
      -1,    -1,    -1,  1384,    -1,    -1,    -1,   481,    -1,    -1,
     611,    -1,    -1,   614,    -1,    -1,   490,   618,   150,    -1,
     621,  1832,    -1,   624,    -1,   626,  1407,    -1,  1317,    -1,
      -1,   632,    -1,    -1,    -1,   209,   510,    -1,    -1,    -1,
      -1,    -1,   516,    -1,   645,    -1,   647,   648,    -1,   650,
      -1,    -1,   653,    -1,    -1,   656,   657,   658,   101,    -1,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
    1881,  1882,    -1,    -1,    -1,    -1,  1365,    -1,    -1,    -1,
     254,    -1,   556,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     264,    -1,    -1,   567,    -1,   569,    -1,    -1,    -1,    -1,
      -1,   275,    -1,    -1,  1915,    -1,    -1,   150,    -1,   710,
      -1,    -1,  1493,    -1,   157,    -1,  1497,    -1,  1499,    -1,
      -1,  1555,  1503,    -1,  1505,    -1,    -1,    -1,   302,    -1,
      -1,    -1,    -1,   734,   308,   309,    -1,    -1,    -1,   313,
      -1,    -1,    -1,    -1,  1525,    -1,  1527,    -1,    -1,    -1,
     751,    -1,    -1,    -1,    -1,    99,    -1,   101,    70,   103,
     104,   105,   106,   107,   108,   109,   110,   111,   112,  1550,
      -1,   345,   773,   774,    -1,  1541,   350,    -1,    -1,   353,
    1561,    -1,    -1,  1564,    -1,    -1,    -1,    -1,    -1,   101,
      -1,   792,   104,   105,   106,   107,   108,   109,   110,   111,
     112,     1,    -1,    -1,     4,   149,  1587,    -1,   152,   153,
      -1,    12,    13,    14,    15,    16,    -1,   129,   101,   820,
      -1,    -1,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,  1612,  1613,    -1,   117,    -1,   119,   149,   150,    -1,
      -1,    -1,   843,    -1,    -1,    -1,  1627,  1628,   160,   161,
     851,    -1,    -1,   854,    -1,    -1,    -1,   858,    58,    -1,
      -1,  1642,   863,   864,  1645,    -1,   440,   150,    -1,    70,
     153,   872,    -1,   874,   875,    -1,   877,    -1,   452,   453,
     754,    81,   756,    -1,  1665,  1666,   101,    -1,    -1,   104,
     105,   106,   107,   108,   109,   110,   111,   112,    -1,    -1,
     101,   101,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,    -1,  1694,    -1,    -1,    -1,    -1,    -1,    -1,
     794,    -1,    -1,    -1,   925,    -1,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,   150,    -1,   152,   138,   813,
      -1,    -1,   157,     1,   144,    -1,  1780,    -1,   149,   150,
      -1,    -1,   153,    -1,  1735,    -1,    -1,    -1,  1332,   160,
     161,   161,    -1,  1744,    -1,    -1,    -1,  1341,    -1,    -1,
      -1,  1345,    -1,  1347,    -1,  1756,  1665,    -1,    -1,   179,
      -1,    -1,    -1,    -1,   985,  1766,    -1,    -1,    -1,    -1,
     190,   191,  1773,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      58,  1002,  1003,    -1,    -1,    -1,    -1,    -1,   582,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1798,    -1,   219,
      -1,    -1,    -1,    -1,    -1,  1806,    -1,    -1,    -1,    -1,
      -1,    -1,  1813,    -1,    -1,   235,    -1,    -1,    -1,    -1,
     240,   241,    -1,   101,   244,    -1,    -1,    -1,  1829,  1830,
      -1,  1832,  1833,    -1,  1055,    -1,   630,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   266,  1756,    -1,   269,
      -1,   271,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   282,    -1,  1865,    -1,   144,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1095,   295,    -1,    -1,    -1,    -1,
    1881,  1882,  1103,   161,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1485,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   319,
      -1,    -1,   322,    -1,    70,    -1,    -1,    -1,    -1,    -1,
      -1,  1132,  1006,   191,  1915,    -1,   336,    -1,  1012,  1013,
     340,    -1,    -1,    -1,  1833,    -1,    -1,    -1,  1149,  1150,
      -1,  1525,    -1,  1527,    -1,   101,    -1,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,    -1,  1169,    -1,
      -1,  1937,    -1,    -1,    -1,  1176,  1550,    -1,    -1,  1180,
      -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
    1956,  1192,    -1,    -1,   768,    -1,    -1,    -1,    -1,    -1,
      -1,   775,  1203,   149,   150,    -1,  1207,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   160,   161,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   282,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   433,    -1,   101,    -1,   437,   104,   105,
     106,   107,   108,   109,   110,   111,   112,    -1,  1249,  1250,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   319,  1263,  1264,   322,    -1,    -1,  1268,  1269,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   850,    -1,   336,    -1,
      -1,    -1,   340,  1657,   150,    -1,    -1,  1661,    -1,  1163,
      -1,   157,    -1,    -1,  1668,  1296,  1297,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1306,  1307,  1308,  1309,  1310,
    1311,    -1,    -1,    -1,    -1,    -1,  1317,    -1,   518,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   900,    -1,    -1,    -1,
      -1,   905,    -1,    12,    13,    14,    15,    16,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   549,
      -1,    -1,    -1,    -1,   554,    -1,    -1,    -1,   558,    -1,
     560,    -1,    -1,    -1,  1365,    -1,    -1,    -1,    -1,    -1,
      -1,   571,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   437,
      -1,    -1,    -1,  1384,  1258,    -1,    -1,   587,   588,  1763,
    1764,    70,  1766,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   602,    -1,    -1,    -1,  1407,    -1,    -1,    -1,
      -1,   611,    -1,    -1,    -1,    -1,   616,    70,    -1,    -1,
      -1,    -1,   101,    -1,   624,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1316,    -1,    -1,    -1,    -1,  1821,   101,    -1,
     129,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     518,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     149,   150,    -1,    -1,   153,    -1,   129,    -1,  1479,  1480,
      -1,   160,   161,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   549,    -1,    -1,    -1,    -1,   149,   150,  1499,  1873,
     558,  1875,   560,    -1,    -1,    -1,    -1,   160,   161,    -1,
     710,    -1,    -1,   571,  1515,    -1,    -1,    -1,    -1,    -1,
      -1,  1895,    -1,    -1,  1525,    -1,  1527,   727,    -1,   587,
     588,    -1,    -1,    -1,  1108,  1109,  1110,    -1,    -1,    -1,
      -1,     1,    -1,    -1,     4,    -1,    -1,    -1,    -1,  1550,
      -1,   751,    -1,   611,    -1,    -1,   756,  1931,  1932,  1933,
    1561,    -1,    -1,  1564,  1138,    -1,   624,    -1,    -1,    -1,
      -1,  1572,    -1,   773,   774,    -1,    -1,    -1,    -1,  1153,
      -1,    -1,    -1,    -1,    -1,    -1,  1587,    -1,    -1,    -1,
      -1,    -1,   792,    -1,    -1,    -1,    -1,    -1,    58,     1,
      -1,  1602,     4,    -1,    -1,    -1,  1607,  1608,    -1,    -1,
      -1,  1612,  1613,    -1,    -1,  1189,    -1,    -1,    -1,    -1,
     820,    81,    -1,  1497,    -1,    -1,    -1,    -1,    -1,  1503,
      -1,  1505,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,
      -1,   101,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   851,   710,    -1,    -1,    -1,    58,    -1,   858,    -1,
      -1,    -1,    -1,   863,  1665,  1666,    -1,    -1,    -1,    -1,
      -1,    -1,   872,    -1,    -1,   875,    -1,   877,   138,    81,
      -1,    -1,   882,    -1,   144,    -1,   146,    -1,   150,    -1,
      -1,   153,    -1,   751,    -1,    -1,   101,    -1,    -1,   101,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,    -1,
      -1,    -1,   117,    -1,   119,   773,   774,    -1,   178,    -1,
      -1,    -1,    -1,    -1,    -1,   925,    -1,    -1,    -1,    -1,
     190,    -1,    -1,    -1,   792,    -1,   138,    -1,    -1,    -1,
      -1,    -1,   144,    -1,   146,   150,  1320,    -1,   153,  1323,
      -1,    -1,    -1,    -1,    -1,  1756,    -1,    -1,    -1,    -1,
      -1,    -1,   820,    -1,    -1,  1766,    -1,    -1,  1642,    -1,
      -1,  1645,  1773,    -1,    -1,    -1,   178,    -1,    -1,    -1,
      -1,   241,    -1,    -1,   244,   985,    -1,    -1,   190,   249,
      -1,    -1,    -1,   851,    -1,    -1,    -1,  1798,    -1,    -1,
     858,    -1,  1002,  1003,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1813,    -1,   872,    -1,    -1,   875,    -1,   877,
    1694,    -1,   282,    -1,    -1,    -1,    -1,    -1,  1829,  1830,
      -1,    -1,  1833,    -1,    -1,    -1,    -1,    -1,    -1,   241,
      -1,   301,   244,    -1,    -1,    -1,    -1,   249,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1055,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1865,    -1,    -1,   925,   101,    -1,
    1744,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     282,   101,    -1,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,    -1,    -1,  1095,    -1,    -1,  1899,   301,
      -1,    -1,    -1,  1103,    -1,    -1,    -1,    -1,    -1,   129,
      -1,    -1,    -1,   373,    -1,    -1,    -1,   150,    -1,    -1,
     153,    -1,    12,    13,    14,    15,    16,   985,    -1,   149,
     150,    -1,  1132,   153,    -1,    -1,    -1,    -1,  1512,    -1,
     160,   161,    -1,    -1,  1002,  1003,    -1,   407,    -1,    -1,
    1150,    -1,    -1,   173,    -1,    -1,    -1,    -1,  1832,    -1,
     132,   133,   134,   135,   136,   137,   138,   139,   140,   141,
     142,   373,    -1,   433,   146,    -1,  1176,    -1,    -1,    -1,
      70,    -1,    -1,    -1,    -1,  1185,    12,    13,    14,    15,
      16,    -1,    -1,    -1,    -1,    -1,    -1,  1055,    -1,    -1,
      -1,   173,    -1,  1203,    -1,   407,  1580,  1881,  1882,    -1,
      -1,   101,    -1,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   433,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,
      -1,  1915,    -1,    -1,    70,  1103,    -1,    -1,    -1,  1249,
    1250,    -1,   512,   513,    -1,    -1,    -1,    -1,    -1,   149,
     150,    -1,    -1,  1263,  1264,    -1,    -1,    -1,  1268,  1269,
     160,   161,    -1,    -1,  1132,   101,    -1,    -1,    -1,    -1,
     106,   107,   108,   109,   110,   111,   112,    -1,    -1,    -1,
      -1,    -1,  1150,    -1,   554,    -1,  1296,  1297,   558,    -1,
     560,    -1,    -1,   129,    -1,    -1,  1306,  1307,  1308,  1309,
     512,   513,    -1,    -1,    -1,    -1,    -1,    -1,  1176,  1693,
      -1,    -1,    -1,   149,   150,    -1,    -1,    -1,  1702,    -1,
      -1,   101,    -1,    -1,   160,   161,   106,   107,   108,   109,
     110,   111,   112,   113,    -1,  1203,    -1,   117,    -1,   119,
      -1,    -1,   554,    -1,    -1,    -1,   558,    -1,   560,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   632,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     150,    -1,    -1,   153,  1384,   645,    -1,   647,   648,    -1,
     650,  1249,  1250,   653,    -1,    -1,   656,   657,   658,    -1,
      -1,    -1,    -1,    -1,    -1,  1263,  1264,  1407,    -1,   101,
    1268,  1269,   104,   105,   106,   107,   108,   109,   110,   111,
     112,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     632,    -1,    -1,    -1,    -1,    -1,    -1,   129,  1296,  1297,
      -1,    -1,    -1,   645,    -1,   647,   648,    -1,   650,    -1,
      -1,   653,    -1,    -1,   656,   657,   658,   149,   150,    -1,
      -1,    -1,    -1,    -1,   156,    -1,    -1,    -1,   160,   161,
      -1,    -1,    -1,    -1,   734,    -1,    -1,    -1,    -1,  1479,
    1480,   101,    -1,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   101,    -1,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,  1505,    -1,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,  1515,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1525,  1384,  1527,    -1,   149,
     150,     1,   734,   153,     4,    -1,    -1,    -1,    -1,    -1,
     160,   161,   150,    -1,   152,    -1,    -1,    -1,   101,    -1,
    1550,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      -1,  1561,    -1,    -1,  1564,    -1,    -1,    -1,    -1,    -1,
     101,    -1,  1572,   104,   105,   106,   107,   108,   109,   110,
     111,   112,    -1,    -1,    -1,    -1,    -1,    -1,    58,    -1,
      -1,   851,    -1,    -1,    -1,    -1,    -1,   150,   129,   152,
      -1,    -1,  1602,   863,   864,    -1,    -1,  1607,  1608,    -1,
      -1,    81,  1612,  1613,    -1,    -1,    -1,    -1,   149,   150,
      -1,  1479,  1480,    -1,    -1,    -1,    -1,    -1,  1628,   160,
     161,   101,    -1,    -1,    -1,   101,    -1,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,    -1,    -1,   851,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1515,    -1,    -1,
      -1,   863,   864,   129,    -1,    -1,  1666,  1525,   138,  1527,
      -1,    -1,    -1,    -1,   144,    -1,   146,    -1,    -1,    -1,
      -1,    -1,    -1,   149,   150,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1550,    -1,   160,   161,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1561,    -1,    -1,    -1,    -1,   178,    -1,
      -1,    -1,    -1,    -1,  1572,    -1,    -1,    -1,    -1,   101,
     190,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,    -1,   101,    -1,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,  1602,    -1,    -1,    -1,    -1,  1607,
    1608,    -1,    -1,    -1,  1612,  1613,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1766,   149,   150,    -1,
      -1,   241,    -1,  1773,   244,    -1,    -1,    -1,    -1,   249,
     149,   150,    -1,    -1,    -1,    -1,    85,    -1,    -1,    -1,
      -1,    -1,    91,    92,    -1,    -1,    -1,    -1,  1798,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1666,    -1,
      -1,    -1,   282,  1813,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   125,    -1,    -1,  1829,
    1830,   301,    -1,    -1,    -1,  1095,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1103,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1865,    -1,    -1,    -1,    -1,
      -1,    -1,  1132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1882,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1150,    -1,    -1,  1095,    -1,    -1,    -1,    -1,    -1,  1899,
      -1,  1103,    -1,   373,    -1,    -1,    -1,    -1,  1766,  1169,
      -1,    -1,    -1,    -1,    -1,  1773,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1132,    -1,    -1,    -1,    -1,    -1,    -1,   407,    -1,    -1,
    1798,     0,    -1,    -1,     3,    -1,    -1,  1207,  1150,    -1,
      -1,    -1,    -1,    -1,    -1,  1813,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   433,    -1,    -1,    -1,  1169,    -1,    -1,
      -1,  1829,  1830,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   291,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1207,    -1,  1865,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    76,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1899,   512,   513,    -1,    -1,  1306,  1307,  1308,  1309,
    1310,  1311,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   554,    -1,    -1,    -1,   558,    -1,
     560,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1306,  1307,  1308,  1309,  1310,  1311,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   436,    -1,   438,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1407,   447,   448,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   632,    -1,    -1,    -1,    -1,    -1,    -1,   228,
      -1,    -1,    -1,    -1,    -1,   645,    -1,   647,   648,    -1,
     650,    -1,    -1,   653,   243,    -1,   656,   657,   658,    -1,
      -1,    -1,    -1,    -1,   253,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   263,  1407,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   277,   278,
      -1,    -1,    -1,    -1,    -1,   284,   285,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1499,
      -1,   300,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   555,    -1,    -1,   318,
      -1,    -1,    -1,    -1,   734,  1525,    -1,  1527,    -1,    -1,
      -1,   177,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1550,    -1,    -1,    -1,    -1,    -1,    -1,  1499,    -1,    -1,
      -1,    -1,    -1,    -1,  1564,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   374,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1525,    -1,  1527,    -1,  1587,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   405,    -1,  1550,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1564,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   431,    -1,    -1,    -1,   435,    -1,    -1,    -1,
      -1,   851,    -1,    -1,    -1,  1587,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   863,   864,   454,    -1,    -1,    -1,   458,
     459,    -1,    -1,   462,    -1,    -1,  1666,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   477,   478,
     479,   480,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   496,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   504,    -1,    -1,    -1,    -1,
      -1,    -1,   358,    -1,    -1,   361,   362,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1666,   371,   372,    -1,   767,    -1,
      -1,    -1,    -1,   532,    -1,    -1,    -1,    -1,    -1,    -1,
     386,   387,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   407,    -1,    -1,   563,    -1,  1766,    -1,    -1,    -1,
      -1,   570,    -1,    -1,    -1,    -1,   575,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   437,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   598,
     599,    -1,    -1,    -1,    -1,   844,   845,    -1,    -1,    -1,
      -1,    -1,    -1,  1813,    -1,    -1,   855,   856,   857,    -1,
      -1,   860,    -1,    -1,  1766,    -1,    -1,    -1,    -1,  1829,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     659,    -1,    -1,    -1,    -1,  1865,    -1,    -1,    -1,    -1,
      -1,  1813,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1095,    -1,  1829,    -1,    -1,
      -1,    -1,    -1,  1103,    -1,    -1,    -1,    -1,    -1,    -1,
     939,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1132,  1865,    -1,    -1,    -1,   726,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1150,    -1,   741,   982,    -1,    -1,   745,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   754,    -1,    -1,    -1,  1169,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   776,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   785,    -1,    -1,    -1,
    1029,    -1,   791,    -1,    -1,    -1,    -1,  1207,    -1,  1038,
    1039,  1040,  1041,    -1,    -1,    -1,    -1,  1046,  1047,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1056,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   828,
      -1,    -1,    -1,    -1,    -1,    -1,   835,    -1,  1077,    -1,
    1079,   687,   688,   689,   690,   691,   692,   693,   694,   695,
     696,   697,   698,   699,   700,   701,   702,   703,   704,   705,
      -1,    -1,   861,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1132,    -1,    -1,  1306,  1307,  1308,  1309,
    1310,  1311,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   915,    -1,    -1,  1158,
      -1,   767,    -1,    -1,    -1,  1164,    -1,  1166,  1167,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1175,    -1,  1177,    -1,
    1179,    -1,  1181,    -1,    -1,    -1,    -1,  1186,    -1,    -1,
      -1,    -1,    12,    13,    14,    15,    16,    -1,    -1,    19,
      -1,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    -1,    49,
      50,    51,    -1,    53,    -1,    -1,   995,  1407,    -1,    -1,
     999,    -1,    -1,    -1,    -1,  1244,    -1,  1006,    -1,    -1,
      70,    -1,  1251,  1252,    -1,    47,    -1,  1016,    -1,    -1,
      -1,    -1,    -1,    -1,  1023,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1032,    -1,  1034,  1275,    -1,    -1,    -1,
      -1,    73,    -1,  1282,   104,   105,  1285,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1066,    -1,    -1,
      -1,  1070,    -1,    -1,    -1,   921,  1315,    -1,    -1,    -1,
     926,    -1,    -1,    -1,    -1,  1084,   118,    -1,  1087,  1499,
      -1,   937,   152,    -1,    -1,    -1,    -1,    -1,    -1,   131,
      -1,   133,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1525,  1355,  1527,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    97,    -1,
     976,   163,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   108,
    1550,   110,    -1,   112,    -1,    -1,    -1,    -1,   180,  1388,
      -1,    -1,    -1,    -1,  1564,    -1,    -1,  1396,    -1,  1398,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1174,    -1,  1587,    -1,    -1,
      -1,   150,    -1,   152,   153,    -1,   218,    -1,    -1,    -1,
     222,    -1,    -1,   225,   226,    -1,    -1,   229,    -1,  1198,
     232,   233,    -1,    -1,    -1,  1444,  1445,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1459,  1460,    -1,  1462,    -1,   194,    -1,    -1,    47,    -1,
      -1,    -1,  1471,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1481,  1482,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1666,  1103,    -1,    -1,
      -1,    -1,   294,    -1,    -1,   297,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   317,    -1,    -1,   257,  1288,
     259,   260,    -1,  1292,    -1,    -1,    -1,    -1,    -1,   118,
     332,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   131,    -1,   133,    -1,    -1,    -1,   287,    -1,
      -1,    -1,    -1,  1322,   293,    -1,    -1,    -1,    -1,    -1,
    1176,    -1,    -1,    -1,  1573,  1574,  1335,  1336,    -1,    -1,
      -1,    -1,    -1,    -1,  1583,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   321,    -1,    -1,    -1,  1766,    -1,   327,    -1,
     329,    -1,    -1,    -1,  1210,  1211,  1212,    -1,    -1,    -1,
      -1,  1217,  1218,  1372,    -1,    -1,  1375,    -1,    -1,  1618,
    1619,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1390,    -1,  1239,   426,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1813,    -1,    -1,   225,   226,    -1,    -1,
     229,    -1,    -1,   232,   233,    -1,    -1,    -1,    -1,  1829,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1276,  1277,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1440,    -1,    -1,    -1,    -1,    -1,    -1,   417,   481,
    1449,    -1,    -1,    -1,  1453,  1865,    -1,  1696,    -1,    -1,
      -1,   493,    -1,    -1,    -1,    -1,    -1,    -1,  1467,  1468,
      -1,    -1,    -1,    -1,   443,  1714,   445,   446,  1717,  1718,
      -1,    -1,    -1,    -1,    -1,  1724,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   317,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   332,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   490,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   567,    -1,    -1,    -1,    -1,
      -1,   510,    -1,    -1,    -1,    -1,   515,    -1,   517,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1565,  1566,   537,    -1,
     539,   540,    -1,    -1,   606,   607,    81,    -1,    -1,  1818,
      -1,    -1,    -1,    -1,    -1,    -1,  1825,   619,   557,    -1,
      -1,    -1,  1831,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     569,    -1,    -1,    -1,    -1,    -1,    -1,   426,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   590,    -1,   592,   593,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1879,   146,    -1,    -1,    -1,   614,   615,    -1,    -1,    -1,
      -1,    -1,   621,    -1,    -1,    -1,   161,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1903,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1911,   178,   493,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1682,    -1,    -1,   191,  1926,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   730,   731,
      -1,    -1,    -1,    -1,   736,  1704,    -1,    -1,    -1,  1555,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1722,    -1,   757,    -1,    -1,   760,   761,
      -1,   763,    -1,   765,   766,    -1,    -1,    -1,    -1,   244,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1748,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1769,    -1,   804,  1772,    -1,    -1,   808,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   606,   607,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   301,    -1,    -1,    -1,
     619,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   319,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   161,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   883,    -1,    -1,    -1,    -1,    -1,  1856,    -1,    -1,
      -1,    -1,    -1,    -1,   190,   191,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   843,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   854,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   222,    -1,    -1,    -1,
    1746,    -1,   407,   229,    -1,   874,    -1,    -1,    -1,    -1,
      -1,   730,   731,    -1,    -1,    -1,   885,   736,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   894,    -1,    -1,    -1,    -1,
      -1,    -1,   437,    -1,  1780,    -1,    -1,    -1,   757,    -1,
      -1,   760,   761,    -1,   763,    -1,   765,   766,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   297,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1012,    -1,    -1,    -1,    -1,   804,    -1,    -1,    -1,   808,
      -1,    -1,    -1,   319,   320,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   512,   513,    -1,
      -1,    -1,    -1,   518,   340,  1861,  1862,    -1,    -1,    -1,
     989,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1060,    -1,
       5,  1063,    -1,    -1,    -1,    -1,    -1,    12,    13,    14,
      15,    16,  1888,    -1,  1013,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   883,    -1,   571,  1913,    -1,    -1,
      -1,    -1,    -1,    48,    -1,    -1,    -1,    52,    -1,    54,
      -1,    -1,    -1,   588,   410,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    70,    71,    -1,    -1,    -1,
     426,   427,    -1,   429,   430,    -1,   611,    -1,    -1,    -1,
      -1,   437,    -1,    -1,  1960,   441,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    98,    99,    -1,   101,   632,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,   117,   118,   119,    -1,   121,   122,    -1,    -1,
    1182,    -1,    -1,    -1,   129,    -1,   482,    -1,  1190,  1191,
     486,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   149,    -1,    -1,   152,   153,    -1,
    1149,    -1,    -1,    -1,   159,   160,   161,   162,   163,   164,
     165,    -1,   518,  1012,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   710,    -1,    -1,    -1,    -1,
      -1,  1180,    -1,  1245,    -1,    -1,    -1,  1186,    -1,    -1,
      -1,    -1,  1254,    -1,    -1,  1257,    -1,  1259,  1260,   734,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1060,   568,    -1,  1063,   571,   751,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   587,   588,    -1,    -1,    -1,    -1,  1299,   773,   774,
      -1,    -1,   598,    -1,    -1,    -1,   602,    -1,    -1,    -1,
      -1,    -1,    -1,   609,    -1,   611,    -1,   792,    -1,    -1,
      -1,    -1,    -1,    -1,    12,    13,    14,    15,    16,    17,
      -1,    19,    -1,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    -1,    -1,
      -1,    -1,    50,    51,  1366,    -1,    -1,    55,    56,    57,
      58,    59,    60,    61,    62,    -1,    -1,    -1,  1317,    -1,
      -1,    -1,    70,   858,    -1,    -1,    -1,    -1,    -1,   864,
      -1,    -1,    -1,  1182,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1190,  1191,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   101,   710,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,    -1,  1365,    -1,    -1,    -1,
     726,   727,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     736,   737,    -1,   739,   740,    -1,  1448,    -1,    -1,    -1,
     925,    -1,    -1,    -1,    -1,   751,  1245,    -1,   754,    -1,
     756,   757,   150,    -1,   152,  1254,    -1,   763,  1257,    -1,
    1259,  1260,    -1,    -1,    -1,  1477,    -1,   773,   774,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   792,    -1,    -1,    -1,
     796,  1503,    -1,    -1,   800,    -1,    -1,  1509,   804,   805,
    1299,    -1,   808,   809,    -1,    -1,    -1,    -1,    -1,    -1,
     816,    12,    13,    14,    15,    16,    17,    -1,    19,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    -1,    -1,    81,  1560,    50,
      51,    -1,   858,   859,    55,    56,    57,    58,    59,    60,
      61,    62,    -1,    97,    -1,    -1,    -1,  1366,    -1,    70,
    1055,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   885,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   178,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   191,   104,   105,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   146,    -1,    -1,   204,   150,   206,    -1,   925,
      -1,    -1,  1634,  1635,    -1,    -1,    -1,   161,    -1,    -1,
    1642,    -1,    -1,    -1,  1646,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   178,    -1,    -1,    -1,    -1,  1448,
      -1,   152,    -1,    -1,    -1,    -1,    -1,   191,    -1,    -1,
     194,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1477,    -1,
      -1,    -1,    -1,   989,  1169,    -1,    -1,    -1,    -1,    -1,
      -1,  1176,    -1,    -1,    -1,    -1,  1645,    -1,    -1,    -1,
    1006,  1007,    -1,   292,    -1,    -1,    -1,  1013,    -1,    -1,
     244,    -1,    -1,    -1,    -1,    -1,  1665,    -1,  1203,    -1,
      -1,    -1,    -1,   257,    -1,  1737,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1055,
      -1,    -1,    -1,    -1,  1060,  1061,    -1,  1063,  1064,   293,
      -1,  1560,    -1,    -1,    -1,    -1,    -1,   301,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1268,  1269,   319,  1735,   321,    -1,    -1,
    1802,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    12,
      13,    14,    15,    16,    -1,    -1,    19,  1756,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    -1,    -1,  1634,  1635,    50,    51,   373,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1646,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1881,
    1176,    -1,    -1,   407,   463,    -1,  1182,  1183,    -1,    -1,
     469,    -1,    -1,    -1,  1833,   474,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1203,    -1,  1384,
      -1,    -1,    -1,   437,    -1,    -1,    -1,    -1,    -1,   443,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1737,  1245,
    1246,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1254,  1255,
      -1,  1257,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1268,  1269,    -1,    -1,  1915,    -1,    -1,    -1,
      -1,    -1,   561,    -1,    -1,    -1,    -1,    -1,   512,   513,
      -1,    -1,    -1,    -1,   518,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   588,
      -1,    -1,    -1,  1802,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   601,    -1,  1499,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   571,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   588,    -1,   590,    -1,    -1,    -1,
      -1,    -1,    -1,   652,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1561,   611,  1384,    -1,
      -1,   670,   671,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   680,    -1,   682,   683,    -1,    -1,    -1,   632,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   645,    -1,   647,   648,    -1,   650,    -1,    -1,   653,
      -1,   710,   656,   657,   658,    -1,    -1,  1612,  1613,    -1,
      -1,    -1,    -1,    -1,   723,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   734,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   748,
      -1,    -1,   751,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   710,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1493,    -1,   778,
      -1,    -1,   781,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     734,    -1,    -1,  1509,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   751,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   817,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   773,
     774,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     5,  1561,    -1,    -1,   792,    -1,
      -1,    12,    13,    14,    15,    16,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   864,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   874,   875,    -1,    -1,    -1,
      -1,    -1,    -1,   882,    -1,    -1,    -1,    48,    -1,    -1,
      -1,    52,    -1,    54,    -1,    -1,  1612,  1613,    -1,    -1,
      -1,    -1,    -1,  1798,   903,    -1,    -1,    -1,    -1,    70,
      71,  1627,  1628,   912,   858,    -1,    -1,    -1,    -1,    -1,
     864,    -1,    -1,    -1,    -1,    -1,   925,  1643,    -1,    -1,
     874,    -1,    -1,    -1,   933,    -1,    -1,    98,    99,    -1,
     101,   940,   103,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,   117,   118,   119,    -1,
     121,   122,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   925,    -1,    -1,   983,    -1,    -1,    -1,   149,    -1,
      -1,   152,   153,    -1,    -1,    -1,    -1,    -1,   159,   160,
     161,   162,   163,   164,   165,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1735,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1743,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1050,    -1,  1052,    -1,  1054,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1798,    -1,    -1,    -1,  1802,  1803,    -1,    -1,
    1806,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1055,    -1,    -1,    -1,    -1,  1832,    -1,    -1,    -1,
      -1,  1120,  1121,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1095,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1881,  1882,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1180,    -1,    -1,    -1,    -1,    -1,  1186,    -1,    -1,
      -1,    48,    -1,    -1,    -1,    52,    -1,    54,    -1,  1915,
      -1,    -1,    -1,    -1,  1203,  1149,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,
      -1,  1220,    -1,    -1,    -1,  1169,    -1,    -1,    -1,    -1,
      -1,    -1,  1176,    -1,  1233,    -1,    -1,  1236,    -1,    -1,
      -1,    98,    99,    -1,   101,    -1,   103,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,  1203,
     117,   118,   119,  1207,   121,   122,    -1,    -1,    -1,    -1,
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1287,    -1,
      -1,    -1,   149,    -1,    -1,   152,   153,    -1,    -1,    -1,
      -1,    -1,   159,   160,   161,   162,   163,   164,   165,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   173,   174,    -1,    -1,
      -1,    -1,    -1,    -1,  1268,  1269,  1325,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1343,    -1,    -1,  1346,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1306,  1307,  1308,    -1,  1310,  1311,    -1,    -1,
      -1,    -1,    -1,  1317,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1384,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1394,  1395,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1419,  1365,  1421,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    -1,    19,
    1384,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    -1,    -1,    48,    -1,
      50,    51,    52,    -1,    54,    55,    56,    57,    58,    59,
      60,    61,    62,    63,    -1,    -1,    -1,    67,    -1,    -1,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1499,    -1,    -1,    -1,    -1,  1504,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    98,    99,
     100,   101,    -1,   103,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,    -1,   117,   118,   119,
      -1,   121,   122,    -1,    -1,    -1,    -1,  1546,    -1,   129,
      -1,    -1,    -1,    -1,    -1,  1499,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,
      -1,    -1,   152,   153,    -1,    -1,    -1,    -1,    -1,   159,
     160,   161,   162,   163,   164,   165,  1585,    -1,    -1,  1588,
      -1,    -1,    -1,   173,   174,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1561,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1587,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1612,  1613,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    -1,    19,    -1,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    -1,    -1,    -1,    -1,    50,    51,    -1,
      -1,  1665,    55,    56,    57,    58,    59,    60,    61,    62,
      63,    -1,    -1,    -1,    67,    -1,    -1,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    96,    -1,    -1,    -1,   100,   101,    -1,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      -1,    -1,  1781,   116,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1756,    -1,    -1,    -1,   149,   150,    -1,   152,
     153,    -1,    -1,    -1,    -1,    -1,    -1,   160,   161,  1773,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1798,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1830,    -1,    -1,  1833,
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
      -1,    -1,    -1,    -1,    -1,   148,   149,   150,    -1,   152,
     153,    -1,    -1,    -1,   157,    -1,   159,   160,   161,   162,
     163,   164,   165,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   174,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    -1,    19,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    -1,    -1,    48,    -1,    50,
      51,    52,    -1,    54,    55,    56,    57,    58,    59,    60,
      61,    62,    63,    -1,    -1,    -1,    67,    -1,    69,    70,
      71,    72,    -1,    74,    -1,    -1,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    90,
      91,    92,    93,    94,    -1,    96,    -1,    98,    99,   100,
     101,    -1,   103,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,    -1,
     121,   122,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,   150,
      -1,   152,   153,    -1,    -1,    -1,   157,    -1,   159,   160,
     161,   162,   163,   164,   165,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   174,     3,     4,     5,     6,     7,     8,
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
     161,   162,   163,   164,   165,     3,     4,     5,     6,     7,
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
      -1,    -1,    -1,    -1,    -1,    -1,     3,    -1,    -1,    -1,
      -1,    -1,   152,   153,    -1,    12,    13,    14,    15,    16,
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
      -1,    -1,    -1,    -1,    -1,   152,   153,    -1,    -1,    -1,
      -1,    -1,    -1,   160,   161,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    17,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     149,   150,    -1,   152,   153,    -1,    -1,    -1,   157,    48,
      -1,   160,   161,    52,    -1,    54,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      69,    -1,    71,    -1,    -1,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,    87,    88,
      89,    90,    91,    92,    93,    94,    -1,    96,    -1,    98,
      99,    -1,   101,    -1,   103,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,    -1,   121,   122,    -1,    -1,    -1,    -1,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     149,   150,    -1,   152,   153,    -1,    -1,    -1,   157,    -1,
     159,   160,   161,   162,   163,   164,   165,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   174,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    -1,
      -1,    19,    -1,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      -1,    49,    50,    51,    -1,    53,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   101,    17,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   129,    -1,    -1,    -1,    48,    -1,    -1,    -1,    52,
      -1,    54,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   149,   150,    -1,   152,   153,    69,    -1,    71,    72,
      -1,    74,   160,   161,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,    87,    88,    89,    90,    91,    92,
      93,    94,    -1,    96,    -1,    98,    99,    -1,   101,    -1,
     103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,    -1,   121,   122,
      -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   149,   150,    -1,   152,
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
      -1,    -1,    -1,    -1,   149,   150,    -1,   152,   153,    -1,
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
     150,    -1,   152,   153,    -1,    12,    13,    14,    15,    16,
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
      -1,    -1,    48,    -1,    -1,    -1,    52,    -1,    54,    -1,
      -1,   152,    -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,   149,   150,    -1,   152,   153,
      -1,    -1,    -1,   157,    -1,   159,   160,   161,   162,   163,
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
       0,   176,   383,   384,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    19,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    50,    51,    55,    56,    57,
      58,    59,    60,    61,    62,    63,    67,    70,    71,    96,
     100,   101,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   116,   129,   149,   150,   152,   153,   160,   161,   179,
     180,   194,   275,   276,   277,   278,   279,   280,   281,   282,
     283,   284,   285,   286,   288,   290,   292,   293,   294,   295,
     296,   297,   298,   299,   300,   302,   304,   305,   306,   308,
     309,   313,   314,   315,   316,   317,   319,   325,   326,   327,
     328,   339,   342,   375,   378,   388,   393,   395,   401,   405,
     410,   411,   412,   413,   414,   415,   416,   417,   437,   454,
     455,   456,   457,     0,   176,   180,   194,   279,   281,   290,
     293,   305,   309,   314,   115,   149,    56,    59,    60,    62,
     149,   149,   399,   400,   401,   301,   302,   104,   105,   180,
     355,   376,   377,   355,   149,   388,   149,   149,   149,   194,
     400,   405,   411,   412,   413,   415,   416,   417,   104,   316,
     154,   176,   282,   290,   293,   410,   414,   453,   454,   457,
     458,   174,   177,   146,   157,   173,   215,   358,    87,   155,
     394,   355,   177,   177,   177,   174,   104,   105,   149,   194,
     287,   396,   405,   406,   407,   408,   409,   410,   414,   418,
     419,   420,   421,   422,   428,     3,    46,    47,    49,    53,
     307,     3,     4,   153,   194,   281,   294,   298,   300,   310,
     315,   390,   410,   414,   457,   279,   281,   293,   305,   309,
     314,   391,   410,   414,    63,   299,   299,   294,   300,   299,
     294,   299,   294,   152,   399,   155,   177,   149,   157,   223,
     399,   399,   176,   270,   271,   153,   290,   293,   455,   355,
     355,   388,   173,   293,   149,   194,   396,   405,   410,   419,
     153,   194,   457,   389,    63,    64,    65,    66,   153,   171,
     355,   364,   366,   370,   372,   373,   315,    55,   153,   194,
     289,   293,   297,   298,   304,   305,   311,   312,   313,   314,
     318,   325,   326,   342,   351,   353,   437,   449,   450,   451,
     452,   457,   458,   104,   105,   157,   180,   315,   428,   401,
     149,   371,   372,   149,   149,   115,   181,   182,    48,    52,
      54,    71,    98,    99,   101,   103,   113,   114,   117,   118,
     119,   121,   122,   149,   153,   159,   162,   163,   164,   165,
     178,   179,   181,   183,   186,   193,   194,   195,   196,   199,
     200,   201,   202,   203,   204,   205,   206,   207,   208,   209,
     210,   211,   217,   315,   151,   153,   193,   194,   210,   212,
     290,   315,   356,   357,   374,   453,   458,   293,   411,   412,
     413,   415,   416,   417,   151,   151,   151,   151,   151,   151,
     151,   153,   290,   437,   455,   153,   160,   194,   212,   281,
     282,   289,   291,   293,   305,   312,   314,   346,   347,   350,
     351,   352,   449,   457,   149,   410,   414,   457,   149,   155,
     101,   152,   153,   157,   179,   180,   212,   359,   360,   361,
     362,   363,    21,   359,   149,   355,   223,   149,   155,   155,
     155,   400,   405,   407,   408,   409,   418,   420,   421,   422,
     293,   406,   419,   155,    96,   398,   153,   399,   436,   437,
     399,   399,   394,   270,   149,   399,   436,   394,   399,   399,
     293,   396,   149,   149,   292,   293,   290,   293,   176,   290,
     453,   458,   317,   157,   394,   270,   355,   358,   281,   298,
     392,   410,   414,   157,   394,   270,   376,   293,   305,   293,
     293,   104,   316,   104,   105,   180,   315,   320,   376,   176,
     180,   354,   148,   176,     3,   286,   288,   293,   297,   223,
     176,   176,   398,   149,   398,   177,   212,   400,   405,   293,
     149,   176,   355,   157,   355,   157,   355,   131,   160,   161,
     369,   151,   155,   355,   373,   151,   399,   154,   176,   291,
     293,   305,   312,   314,   448,   449,   457,   458,   149,   153,
     161,   173,   194,   437,   438,   439,   440,   441,   442,   443,
     460,   194,   318,   457,   293,   312,   299,   294,   399,   151,
     291,   293,   450,   291,   437,   450,     9,   343,   355,   340,
     157,   364,   173,   364,    12,    86,   101,   104,   105,   179,
     402,   403,   404,   151,   115,   149,   193,   149,   149,   196,
     149,   193,   149,   149,   193,   193,    18,    20,    83,   153,
     162,   163,   197,   198,   212,   219,   223,   328,   356,   457,
     155,   176,   149,   183,   158,   158,   118,   120,   121,   122,
     149,   152,   153,   157,   158,   196,   196,   166,   160,   167,
     168,   162,   163,   123,   124,   125,   126,   169,   170,   127,
     128,   161,   159,   171,   129,   130,   172,   151,   155,   152,
     176,   132,   133,   134,   135,   136,   137,   138,   139,   140,
     141,   142,   173,   214,   215,   216,   149,   194,   432,   433,
     434,   435,   436,   151,   155,   151,   151,   151,   151,   151,
     151,   149,   399,   436,   437,   149,   436,   437,   176,   290,
     455,   176,   177,   177,   149,   161,   194,   405,   423,   424,
     425,   426,   427,   428,   429,   430,   431,   131,   457,   177,
     177,   355,   355,   176,   176,   176,   153,   180,   176,   360,
     156,   155,   459,   359,   152,   153,   156,   363,   150,   212,
     218,   149,   176,   176,   176,   176,   405,   407,   408,   409,
     418,   420,   421,   422,   151,   151,   151,   151,   151,   151,
     151,   406,   419,   399,   149,   358,   154,   176,   223,   394,
     176,   223,   396,   219,   357,   219,   357,   396,   386,   223,
     394,   398,   157,   394,   270,   386,   223,   394,   322,   323,
     321,   157,   131,   293,   348,   349,   352,   353,   151,   155,
      68,   272,   273,   177,   293,   286,   160,   212,   176,   405,
     347,   386,   154,   176,   149,   368,   366,   367,    76,   303,
     180,   291,   437,   450,   293,   297,   457,   176,   439,   440,
     441,   154,   176,    17,   212,   293,   438,   460,   399,   399,
     437,   291,   448,   458,   293,   180,   399,   291,   450,   315,
     155,   459,   173,   215,   344,   157,   343,   151,   357,   151,
     151,   155,   149,   174,   356,   153,   356,   356,   356,   212,
     356,   151,   356,   356,   356,   176,   151,   162,   163,   198,
      17,   295,   151,   155,   151,   160,   161,   151,   218,   212,
     157,   180,   180,   113,   153,   180,   150,   187,   188,   212,
     113,   153,   180,   328,   212,   187,   180,   196,   199,   199,
     199,   200,   200,   201,   201,   202,   202,   202,   202,   203,
     203,   204,   205,   206,   207,   208,   156,   219,   174,   181,
     153,   180,   212,   157,   212,   176,   433,   434,   435,   293,
     432,   399,   399,   212,   357,   149,   399,   436,   437,   149,
     436,   437,   176,   176,   154,   154,   149,   405,   424,   425,
     426,   429,    17,   293,   423,   427,   149,   399,   442,   460,
     399,   399,   460,   149,   399,   442,   399,   399,   177,   211,
     355,   154,   155,   154,   155,   460,   460,   131,   345,   346,
     347,   345,   355,   176,   210,   211,   212,   397,   459,   359,
     361,   148,   176,   151,   155,   176,   345,   180,   396,   180,
     151,   151,   151,   151,   151,   151,   149,   399,   436,   437,
     149,   399,   436,   437,   396,   181,   437,   212,   223,   348,
     151,   151,   151,   151,   384,   385,   223,   386,   223,   394,
     385,   223,   157,   157,   157,   329,   177,   177,   180,   274,
     355,    17,    69,    71,    74,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    90,    91,    92,
      93,    94,    96,   104,   105,   116,   176,   219,   220,   221,
     222,   223,   224,   225,   227,   228,   238,   242,   243,   244,
     245,   246,   247,   252,   253,   259,   260,   261,   275,   293,
     297,   355,   395,    68,   174,   177,   177,   177,   345,   177,
     385,   279,   281,   290,   379,   380,   381,   382,   374,   173,
     365,   365,   291,   450,   153,   160,   194,   212,   315,   212,
     293,   348,   151,   151,   151,     5,   293,   399,   438,   157,
     180,   428,     9,   355,   148,   359,   343,   459,   157,   151,
     403,   187,   151,   176,   155,   151,   151,   155,   151,   196,
     151,   151,   151,   196,    17,   295,   212,   151,   151,   150,
     157,   196,   154,   177,   187,   113,   117,   119,   180,   189,
     190,   191,   151,   155,   189,   154,   155,   148,   210,   156,
     151,   189,   177,   360,   348,   151,   151,   151,   432,   176,
     176,   348,   348,   429,   151,   151,   151,   151,   149,   405,
     428,   423,   427,   176,   176,   154,   177,   460,   176,   176,
     177,   177,   177,   177,   358,   189,   131,   165,   177,   177,
     148,   359,   212,   150,   212,   345,   177,   173,   149,   399,
     436,   437,   149,   399,   436,   437,   176,   176,   398,   151,
     177,   177,   387,   385,   223,   387,   329,   329,   329,     3,
       9,    71,   148,   276,   283,   284,   290,   293,   330,   335,
     453,   151,   155,   155,   174,   149,    59,    60,   174,   223,
     275,   395,   149,    17,   221,   149,   149,   174,   355,   174,
     355,   160,   355,   157,   220,   149,   149,   149,   223,   212,
     213,   213,    13,   262,    72,   229,   174,   177,   225,    76,
     174,   355,    89,   248,   354,   293,   156,   274,   174,   154,
     154,   177,   155,   387,   396,   177,   174,   177,   174,   177,
     151,   357,   371,   371,   176,   177,   177,   177,   212,   177,
     149,   399,   442,   437,   292,     5,   160,   177,   212,   343,
     399,   399,   315,   344,   459,   148,   148,   176,   151,   180,
      76,   184,   185,   356,   196,   196,   196,   196,   196,   157,
     360,   155,   148,   192,   153,   190,   192,   192,   154,   155,
     120,   152,   188,   154,   218,   210,   174,   154,   459,   177,
     149,   399,   436,   437,   348,   348,   177,   177,   151,   149,
     399,   436,   437,   149,   399,   442,   405,   399,   399,   348,
     348,   154,   347,   350,   350,   351,   151,   155,   155,   151,
     177,   211,   211,   154,   154,   177,   177,   151,   212,   176,
     176,   348,   348,   358,   399,   155,   151,   148,   387,   148,
     148,   148,   148,   290,   328,   336,   453,   290,   335,   149,
     324,   174,   174,   149,   156,   194,   331,   332,   338,   405,
     406,   419,   155,   174,   355,   176,   355,   187,   174,   223,
     174,   223,   219,    78,   151,   176,   151,   176,   174,   174,
     219,   174,   360,   174,   219,   218,   219,   108,   109,   110,
     111,   112,   254,   256,   257,   174,    95,   174,    82,   149,
     149,   177,   148,   174,   174,   149,   221,   223,   399,   174,
     151,   176,   148,   148,   176,   155,   155,   154,   154,   154,
     177,   151,   176,   212,   212,   177,   154,   177,   459,   341,
     157,   344,   148,   379,   151,   156,   151,   155,   156,   360,
     459,   218,   118,   189,   190,   153,   190,   153,   190,   154,
     148,   151,   176,   177,   177,   151,   151,   176,   176,   177,
     177,   177,   176,   176,   154,   177,   151,   399,   348,   348,
     177,   177,   219,   148,   324,   324,   324,   149,   194,   333,
     334,   436,   444,   445,   446,   447,   174,   155,   174,   331,
     174,   374,   400,   405,   212,   293,   155,   174,   337,   338,
     337,   355,   131,   352,   353,   151,   151,   149,   221,   219,
     230,   275,   277,   280,   286,   293,   297,   221,   173,   174,
     219,   239,   240,   275,   174,   459,   151,   151,   151,   223,
     256,   257,   149,   212,   149,   181,   230,   196,   249,   107,
     221,   399,   380,   176,   176,   154,   348,   177,   177,   154,
     154,   148,   157,   343,   177,   212,   185,   212,   459,   148,
     154,   154,   189,   189,   348,   151,   151,   348,   348,   151,
     151,   154,   155,   131,   347,   131,   154,   177,   177,   151,
     151,   154,   445,   446,   447,   293,   444,   155,   174,   399,
     399,   174,   151,   405,   399,   221,    75,    76,   157,   233,
     234,   235,   151,   219,   151,   219,   293,   219,   220,   143,
     144,   145,   165,   174,   241,   151,   156,   220,   148,   157,
     235,   221,   149,   176,   174,   181,   151,   156,   151,   151,
     155,   156,   247,   251,   355,   396,   177,   154,   154,   343,
     459,   148,   148,   154,   154,   177,   177,   177,   176,   177,
     151,   151,   151,   151,   151,   444,   399,   332,   211,   231,
     232,   397,   156,   176,   221,   233,   174,   151,   221,   174,
     104,   173,   219,   220,   219,   221,   240,   174,   174,   176,
     176,   258,   291,   293,   453,   156,   174,   153,   181,   263,
     264,   265,   221,   196,   187,    73,   106,   248,   250,   151,
     459,   148,   151,   151,   151,   350,   149,   399,   436,   437,
     334,   131,   155,   156,   268,   269,   275,   174,   177,   220,
     219,   144,   165,   241,   174,   165,   177,   220,   268,   258,
     177,   149,   194,   396,   444,   180,   156,   101,   149,   151,
     156,   155,    73,   151,   221,   149,   221,   221,   148,   176,
     211,   231,   234,   236,   237,   275,   150,   150,   219,   220,
     219,   236,   177,   174,   255,   293,   263,   154,   211,   174,
     263,   265,   221,   219,   107,   107,   348,   221,   226,   177,
     234,   165,   165,   165,   177,   255,   210,   151,   156,   181,
     151,   151,   156,   151,   251,    73,   246,   177,   221,   148,
     226,   219,   150,   219,   219,   148,   151,   223,   181,   266,
     149,   174,   266,   221,    73,   151,   223,   155,   156,   211,
     151,   221,   181,   180,   267,   151,   174,   151,   155,   174,
     180
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_int16 yyr1[] =
{
       0,   175,   176,   177,   178,   178,   178,   178,   178,   179,
     179,   179,   179,   179,   179,   179,   180,   180,   180,   181,
     182,   182,   183,   183,   183,   183,   183,   183,   183,   183,
     183,   184,   184,   185,   185,   186,   186,   186,   186,   186,
     186,   186,   186,   186,   186,   186,   186,   186,   186,   186,
     186,   186,   186,   186,   186,   186,   187,   187,   187,   188,
     188,   189,   189,   190,   190,   190,   190,   190,   190,   190,
     191,   191,   191,   192,   192,   193,   193,   193,   193,   193,
     193,   193,   193,   193,   193,   193,   193,   193,   193,   194,
     194,   194,   195,   195,   195,   195,   196,   196,   196,   196,
     196,   196,   196,   196,   196,   197,   197,   197,   197,   198,
     198,   199,   199,   200,   200,   200,   200,   201,   201,   201,
     202,   202,   202,   203,   203,   203,   203,   203,   204,   204,
     204,   205,   205,   206,   206,   207,   207,   208,   208,   209,
     209,   210,   210,   210,   211,   212,   212,   212,   213,   213,
     214,   214,   215,   215,   216,   216,   216,   216,   216,   216,
     216,   216,   216,   216,   216,   217,   217,   218,   218,   218,
     218,   219,   219,   220,   220,   221,   221,   221,   221,   221,
     221,   221,   221,   221,   221,   221,   221,   221,   222,   223,
     223,   224,   224,   225,   225,   225,   225,   225,   226,   226,
     227,   228,   228,   228,   228,   228,   229,   229,   230,   230,
     230,   230,   231,   231,   231,   232,   232,   233,   233,   234,
     234,   235,   236,   236,   237,   237,   238,   238,   238,   238,
     238,   238,   239,   239,   240,   240,   240,   240,   240,   240,
     240,   240,   240,   240,   240,   240,   240,   240,   240,   240,
     240,   240,   241,   241,   241,   241,   242,   242,   242,   242,
     242,   242,   242,   242,   242,   242,   242,   242,   242,   242,
     242,   242,   242,   242,   242,   242,   243,   243,   244,   245,
     246,   247,   247,   248,   248,   249,   249,   250,   251,   251,
     251,   251,   251,   251,   252,   252,   253,   253,   253,   254,
     254,   255,   255,   256,   256,   256,   256,   257,   258,   258,
     258,   258,   258,   259,   260,   260,   261,   261,   261,   261,
     261,   262,   262,   263,   263,   264,   264,   265,   265,   266,
     266,   266,   267,   267,   268,   268,   269,   269,   270,   270,
     271,   271,   272,   272,   273,   273,   274,   274,   275,   275,
     275,   276,   276,   277,   277,   277,   277,   277,   278,   278,
     278,   279,   279,   279,   280,   280,   280,   280,   280,   281,
     281,   282,   282,   283,   283,   283,   284,   284,   284,   284,
     284,   285,   285,   286,   286,   286,   286,   287,   287,   288,
     288,   288,   289,   289,   289,   290,   290,   290,   291,   291,
     291,   292,   292,   293,   293,   294,   294,   295,   295,   295,
     295,   295,   296,   297,   297,   297,   298,   298,   299,   299,
     299,   299,   299,   299,   299,   299,   300,   300,   300,   300,
     300,   300,   300,   300,   300,   300,   300,   300,   300,   300,
     300,   300,   300,   300,   300,   300,   300,   300,   300,   300,
     300,   300,   300,   300,   301,   301,   302,   303,   303,   304,
     304,   304,   304,   304,   305,   305,   306,   306,   306,   306,
     307,   307,   307,   307,   307,   307,   308,   308,   308,   308,
     309,   310,   309,   309,   311,   311,   311,   311,   312,   312,
     312,   313,   313,   313,   313,   314,   314,   314,   315,   315,
     315,   315,   315,   315,   316,   316,   316,   317,   317,   318,
     318,   320,   319,   321,   319,   322,   319,   323,   319,   319,
     324,   324,   325,   325,   326,   326,   327,   327,   327,   328,
     328,   328,   328,   328,   328,   328,   328,   329,   329,   330,
     330,   330,   330,   330,   330,   330,   330,   330,   330,   331,
     331,   331,   332,   332,   332,   333,   333,   333,   334,   335,
     335,   336,   336,   337,   337,   338,   339,   340,   339,   339,
     339,   341,   339,   339,   339,   342,   342,   343,   343,   343,
     343,   344,   344,   345,   345,   345,   345,   345,   345,   345,
     346,   346,   346,   346,   347,   347,   348,   348,   348,   348,
     349,   349,   349,   349,   350,   350,   350,   350,   350,   351,
     351,   351,   351,   351,   352,   352,   353,   353,   354,   354,
     355,   355,   355,   356,   356,   356,   357,   357,   358,   358,
     358,   358,   359,   359,   360,   360,   360,   360,   360,   361,
     361,   362,   362,   363,   363,   363,   363,   363,   364,   364,
     365,   365,   367,   366,   368,   366,   366,   366,   369,   369,
     369,   369,   370,   370,   370,   370,   371,   371,   372,   372,
     373,   373,   374,   374,   374,   374,   375,   375,   375,   376,
     376,   377,   377,   378,   378,   379,   379,   380,   380,   381,
     381,   381,   382,   382,   383,   383,   384,   384,   385,   385,
     386,   387,   388,   388,   388,   388,   388,   389,   388,   390,
     388,   391,   388,   392,   388,   393,   393,   393,   394,   394,
     395,   395,   395,   395,   395,   395,   395,   395,   395,   395,
     396,   396,   396,   397,   398,   398,   399,   399,   400,   400,
     401,   402,   402,   403,   403,   403,   404,   404,   404,   404,
     404,   404,   405,   405,   406,   406,   406,   406,   407,   407,
     407,   407,   408,   408,   408,   408,   408,   408,   408,   409,
     409,   409,   409,   410,   410,   410,   411,   411,   411,   411,
     411,   412,   412,   412,   412,   413,   413,   413,   413,   413,
     413,   414,   414,   414,   415,   415,   415,   415,   415,   416,
     416,   416,   416,   417,   417,   417,   417,   417,   417,   418,
     418,   419,   419,   419,   419,   420,   420,   420,   420,   421,
     421,   421,   421,   421,   421,   421,   422,   422,   422,   422,
     422,   423,   423,   423,   423,   423,   424,   424,   424,   425,
     425,   425,   425,   426,   426,   426,   427,   427,   427,   427,
     427,   428,   428,   429,   429,   429,   430,   430,   431,   431,
     432,   432,   432,   433,   433,   433,   433,   433,   434,   434,
     434,   434,   435,   435,   435,   436,   436,   436,   436,   437,
     437,   437,   437,   438,   438,   438,   438,   439,   439,   439,
     439,   439,   440,   440,   440,   440,   441,   441,   441,   442,
     442,   442,   443,   443,   443,   443,   443,   443,   444,   444,
     444,   445,   445,   445,   445,   445,   446,   446,   446,   446,
     447,   447,   448,   448,   448,   449,   449,   450,   450,   450,
     450,   450,   450,   451,   451,   451,   451,   451,   451,   451,
     451,   451,   451,   452,   452,   452,   452,   453,   453,   453,
     454,   454,   455,   455,   455,   455,   455,   455,   456,   456,
     456,   456,   456,   456,   457,   457,   457,   458,   458,   459,
     459,   460,   460
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
       0,     0,     1,     1,     1,     2,     5,     0,     8,     0,
       7,     0,     7,     0,     8,     1,     2,     3,     0,     4,
       3,     4,     4,     4,     4,     5,     5,     5,     5,     6,
       1,     1,     1,     3,     0,     5,     0,     1,     1,     2,
       6,     1,     3,     0,     1,     4,     1,     1,     1,     1,
       1,     1,     1,     3,     2,     1,     2,     2,     2,     3,
       4,     5,     2,     4,     5,     4,     5,     3,     4,     8,
       9,     3,     4,     2,     1,     2,     6,     8,     9,     3,
       4,     2,     3,     4,     5,     4,     5,     4,     5,     3,
       4,     1,     1,     1,     4,     8,     9,     3,     4,     2,
       3,     3,     4,     4,     5,     4,     5,     3,     4,     1,
       3,     2,     1,     2,     2,     2,     3,     4,     5,     2,
       4,     5,     4,     5,     3,     4,     6,     8,     9,     3,
       4,     2,     4,     1,     2,     2,     2,     3,     4,     2,
       4,     4,     3,     6,     8,     3,     2,     4,     1,     2,
       2,     1,     1,     2,     3,     4,     2,     4,     6,     8,
       1,     2,     2,     1,     2,     2,     3,     4,     1,     4,
       4,     3,     5,     8,     3,     2,     3,     7,     1,     5,
       5,     6,     6,     1,     3,     2,     2,     1,     2,     2,
       3,     4,     1,     4,     4,     3,     5,     8,     3,     1,
       2,     1,     2,     6,     5,     6,     7,     7,     1,     2,
       2,     1,     2,     2,     3,     4,     1,     4,     4,     3,
       8,     3,     1,     1,     2,     1,     1,     2,     3,     2,
       3,     2,     3,     3,     2,     4,     3,     2,     3,     2,
       4,     3,     2,     6,     6,     6,     7,     1,     2,     1,
       1,     1,     2,     3,     2,     3,     2,     3,     3,     4,
       2,     3,     4,     2,     5,     6,     7,     6,     6,     0,
       1,     0,     2
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
#line 530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.enterScope(); }
#line 6734 "Parser/parser.cc"
    break;

  case 3:
#line 534 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 6740 "Parser/parser.cc"
    break;

  case 4:
#line 541 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantInteger( *(yyvsp[0].tok) ) ); }
#line 6746 "Parser/parser.cc"
    break;

  case 5:
#line 542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 6752 "Parser/parser.cc"
    break;

  case 6:
#line 543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 6758 "Parser/parser.cc"
    break;

  case 7:
#line 544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 6764 "Parser/parser.cc"
    break;

  case 8:
#line 545 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantChar( *(yyvsp[0].tok) ) ); }
#line 6770 "Parser/parser.cc"
    break;

  case 18:
#line 562 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 6776 "Parser/parser.cc"
    break;

  case 19:
#line 566 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.constant) = build_constantStr( *(yyvsp[0].str) ); }
#line 6782 "Parser/parser.cc"
    break;

  case 20:
#line 570 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 6788 "Parser/parser.cc"
    break;

  case 21:
#line 572 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 6798 "Parser/parser.cc"
    break;

  case 22:
#line 583 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 6804 "Parser/parser.cc"
    break;

  case 23:
#line 585 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 6810 "Parser/parser.cc"
    break;

  case 24:
#line 589 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_dimensionref( (yyvsp[0].tok) ) ); }
#line 6816 "Parser/parser.cc"
    break;

  case 26:
#line 592 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 6822 "Parser/parser.cc"
    break;

  case 27:
#line 594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new StmtExpr( dynamic_cast<CompoundStmt *>(maybeMoveBuild<Statement>((yyvsp[-1].sn)) ) ) ); }
#line 6828 "Parser/parser.cc"
    break;

  case 28:
#line 596 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.en) = nullptr; }
#line 6834 "Parser/parser.cc"
    break;

  case 29:
#line 598 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.en) = nullptr; }
#line 6840 "Parser/parser.cc"
    break;

  case 30:
#line 600 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild<Expression>( (yyvsp[-3].en) );
			(yyval.en) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 6850 "Parser/parser.cc"
    break;

  case 32:
#line 614 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// steal the association node from the singleton and delete the wrapper
			(yyvsp[-2].genexpr)->associations.splice((yyvsp[-2].genexpr)->associations.end(), (yyvsp[0].genexpr)->associations);
			delete (yyvsp[0].genexpr);
			(yyval.genexpr) = (yyvsp[-2].genexpr);
		}
#line 6861 "Parser/parser.cc"
    break;

  case 33:
#line 624 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuildType((yyvsp[-2].decl)), maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } );
		}
#line 6870 "Parser/parser.cc"
    break;

  case 34:
#line 629 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } ); }
#line 6876 "Parser/parser.cc"
    break;

  case 36:
#line 639 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-5].en), new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) ) ) )) ) ); }
#line 6882 "Parser/parser.cc"
    break;

  case 37:
#line 645 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 6888 "Parser/parser.cc"
    break;

  case 38:
#line 647 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.en) = new ExpressionNode( new ConstructorExpr( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) ) );
		}
#line 6898 "Parser/parser.cc"
    break;

  case 39:
#line 653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 6904 "Parser/parser.cc"
    break;

  case 40:
#line 655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 6910 "Parser/parser.cc"
    break;

  case 41:
#line 657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 6916 "Parser/parser.cc"
    break;

  case 42:
#line 659 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), new ExpressionNode( (yyvsp[-2].constant) ) ) ); }
#line 6922 "Parser/parser.cc"
    break;

  case 43:
#line 661 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 6928 "Parser/parser.cc"
    break;

  case 44:
#line 663 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 6934 "Parser/parser.cc"
    break;

  case 45:
#line 665 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-1].en), build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) ) ) ); }
#line 6940 "Parser/parser.cc"
    break;

  case 46:
#line 667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 6946 "Parser/parser.cc"
    break;

  case 47:
#line 669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[0].aggKey), (yyvsp[-2].en) ) ); }
#line 6952 "Parser/parser.cc"
    break;

  case 48:
#line 671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 6958 "Parser/parser.cc"
    break;

  case 49:
#line 673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 6964 "Parser/parser.cc"
    break;

  case 50:
#line 675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 6970 "Parser/parser.cc"
    break;

  case 51:
#line 677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::IncrPost, (yyvsp[-1].en) ) ); }
#line 6976 "Parser/parser.cc"
    break;

  case 52:
#line 679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::DecrPost, (yyvsp[-1].en) ) ); }
#line 6982 "Parser/parser.cc"
    break;

  case 53:
#line 681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].in), true ) ) ); }
#line 6988 "Parser/parser.cc"
    break;

  case 54:
#line 683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].in), true ))->set_maybeConstructed( false ) ) ); }
#line 6994 "Parser/parser.cc"
    break;

  case 55:
#line 685 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) );
		}
#line 7004 "Parser/parser.cc"
    break;

  case 56:
#line 694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7010 "Parser/parser.cc"
    break;

  case 58:
#line 697 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7016 "Parser/parser.cc"
    break;

  case 59:
#line 702 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Default parameter for argument is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7022 "Parser/parser.cc"
    break;

  case 62:
#line 709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7028 "Parser/parser.cc"
    break;

  case 64:
#line 715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-1].tok) ) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7034 "Parser/parser.cc"
    break;

  case 65:
#line 717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-3].tok) ) ), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7040 "Parser/parser.cc"
    break;

  case 66:
#line 719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7046 "Parser/parser.cc"
    break;

  case 67:
#line 721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7052 "Parser/parser.cc"
    break;

  case 68:
#line 723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7058 "Parser/parser.cc"
    break;

  case 69:
#line 725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7064 "Parser/parser.cc"
    break;

  case 70:
#line 730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_constantInteger( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 7070 "Parser/parser.cc"
    break;

  case 71:
#line 732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_field_name_FLOATINGconstant( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 7076 "Parser/parser.cc"
    break;

  case 72:
#line 734 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_varref( (yyvsp[-1].tok) ), (yyvsp[0].en) ) );
		}
#line 7084 "Parser/parser.cc"
    break;

  case 73:
#line 741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7090 "Parser/parser.cc"
    break;

  case 74:
#line 743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Expression * constant = build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) );
			(yyval.en) = (yyvsp[-1].en) != nullptr ? new ExpressionNode( build_fieldSel( (yyvsp[-1].en),  constant ) ) : new ExpressionNode( constant );
		}
#line 7099 "Parser/parser.cc"
    break;

  case 77:
#line 755 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 7105 "Parser/parser.cc"
    break;

  case 78:
#line 757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en)->set_extension( true ); }
#line 7111 "Parser/parser.cc"
    break;

  case 79:
#line 762 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 7131 "Parser/parser.cc"
    break;

  case 80:
#line 778 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_val( (yyvsp[-1].op), (yyvsp[0].en) ) ); }
#line 7137 "Parser/parser.cc"
    break;

  case 81:
#line 780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Incr, (yyvsp[0].en) ) ); }
#line 7143 "Parser/parser.cc"
    break;

  case 82:
#line 782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Decr, (yyvsp[0].en) ) ); }
#line 7149 "Parser/parser.cc"
    break;

  case 83:
#line 784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7155 "Parser/parser.cc"
    break;

  case 84:
#line 786 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 7161 "Parser/parser.cc"
    break;

  case 85:
#line 788 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7167 "Parser/parser.cc"
    break;

  case 86:
#line 790 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 7173 "Parser/parser.cc"
    break;

  case 87:
#line 792 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_offsetOf( (yyvsp[-3].decl), build_varref( (yyvsp[-1].tok) ) ) ); }
#line 7179 "Parser/parser.cc"
    break;

  case 88:
#line 794 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.en) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 7188 "Parser/parser.cc"
    break;

  case 89:
#line 801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::PointTo; }
#line 7194 "Parser/parser.cc"
    break;

  case 90:
#line 802 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AddressOf; }
#line 7200 "Parser/parser.cc"
    break;

  case 91:
#line 804 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::And; }
#line 7206 "Parser/parser.cc"
    break;

  case 92:
#line 808 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::UnPlus; }
#line 7212 "Parser/parser.cc"
    break;

  case 93:
#line 809 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::UnMinus; }
#line 7218 "Parser/parser.cc"
    break;

  case 94:
#line 810 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::Neg; }
#line 7224 "Parser/parser.cc"
    break;

  case 95:
#line 811 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::BitNeg; }
#line 7230 "Parser/parser.cc"
    break;

  case 97:
#line 817 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cast( (yyvsp[-2].decl), (yyvsp[0].en) ) ); }
#line 7236 "Parser/parser.cc"
    break;

  case 98:
#line 819 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 7242 "Parser/parser.cc"
    break;

  case 99:
#line 821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 7248 "Parser/parser.cc"
    break;

  case 100:
#line 823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( nullptr ) ) ); }
#line 7254 "Parser/parser.cc"
    break;

  case 101:
#line 825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 7260 "Parser/parser.cc"
    break;

  case 102:
#line 827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Return cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7266 "Parser/parser.cc"
    break;

  case 103:
#line 829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7272 "Parser/parser.cc"
    break;

  case 104:
#line 831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7278 "Parser/parser.cc"
    break;

  case 112:
#line 851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Exp, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7284 "Parser/parser.cc"
    break;

  case 114:
#line 857 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mul, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7290 "Parser/parser.cc"
    break;

  case 115:
#line 859 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Div, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7296 "Parser/parser.cc"
    break;

  case 116:
#line 861 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mod, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7302 "Parser/parser.cc"
    break;

  case 118:
#line 867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Plus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7308 "Parser/parser.cc"
    break;

  case 119:
#line 869 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Minus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7314 "Parser/parser.cc"
    break;

  case 121:
#line 875 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7320 "Parser/parser.cc"
    break;

  case 122:
#line 877 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::RShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7326 "Parser/parser.cc"
    break;

  case 124:
#line 883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7332 "Parser/parser.cc"
    break;

  case 125:
#line 885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7338 "Parser/parser.cc"
    break;

  case 126:
#line 887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7344 "Parser/parser.cc"
    break;

  case 127:
#line 889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7350 "Parser/parser.cc"
    break;

  case 129:
#line 895 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Eq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7356 "Parser/parser.cc"
    break;

  case 130:
#line 897 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Neq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7362 "Parser/parser.cc"
    break;

  case 132:
#line 903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitAnd, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7368 "Parser/parser.cc"
    break;

  case 134:
#line 909 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Xor, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7374 "Parser/parser.cc"
    break;

  case 136:
#line 915 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitOr, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7380 "Parser/parser.cc"
    break;

  case 138:
#line 921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), true ) ); }
#line 7386 "Parser/parser.cc"
    break;

  case 140:
#line 927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), false ) ); }
#line 7392 "Parser/parser.cc"
    break;

  case 142:
#line 933 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7398 "Parser/parser.cc"
    break;

  case 143:
#line 936 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-3].en), (yyvsp[-3].en), (yyvsp[0].en) ) ); }
#line 7404 "Parser/parser.cc"
    break;

  case 146:
#line 947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
//			if ( $2 == OperKinds::AtAssn ) {
//				SemanticError( yylloc, "C @= assignment is currently unimplemented." ); $$ = nullptr;
//			} else {
				(yyval.en) = new ExpressionNode( build_binary_val( (yyvsp[-1].op), (yyvsp[-2].en), (yyvsp[0].en) ) );
//			} // if
		}
#line 7416 "Parser/parser.cc"
    break;

  case 147:
#line 955 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7422 "Parser/parser.cc"
    break;

  case 148:
#line 960 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7428 "Parser/parser.cc"
    break;

  case 152:
#line 970 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::Assign; }
#line 7434 "Parser/parser.cc"
    break;

  case 153:
#line 971 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AtAssn; }
#line 7440 "Parser/parser.cc"
    break;

  case 154:
#line 975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ExpAssn; }
#line 7446 "Parser/parser.cc"
    break;

  case 155:
#line 976 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MulAssn; }
#line 7452 "Parser/parser.cc"
    break;

  case 156:
#line 977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::DivAssn; }
#line 7458 "Parser/parser.cc"
    break;

  case 157:
#line 978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ModAssn; }
#line 7464 "Parser/parser.cc"
    break;

  case 158:
#line 979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::PlusAssn; }
#line 7470 "Parser/parser.cc"
    break;

  case 159:
#line 980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MinusAssn; }
#line 7476 "Parser/parser.cc"
    break;

  case 160:
#line 981 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::LSAssn; }
#line 7482 "Parser/parser.cc"
    break;

  case 161:
#line 982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::RSAssn; }
#line 7488 "Parser/parser.cc"
    break;

  case 162:
#line 983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AndAssn; }
#line 7494 "Parser/parser.cc"
    break;

  case 163:
#line 984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ERAssn; }
#line 7500 "Parser/parser.cc"
    break;

  case 164:
#line 985 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::OrAssn; }
#line 7506 "Parser/parser.cc"
    break;

  case 165:
#line 996 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)(new ExpressionNode( nullptr ) )->set_last( (yyvsp[-1].en) ) ) ); }
#line 7512 "Parser/parser.cc"
    break;

  case 166:
#line 998 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-4].en)->set_last( (yyvsp[-1].en) ) ) )); }
#line 7518 "Parser/parser.cc"
    break;

  case 168:
#line 1004 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7524 "Parser/parser.cc"
    break;

  case 169:
#line 1006 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7530 "Parser/parser.cc"
    break;

  case 170:
#line 1008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7536 "Parser/parser.cc"
    break;

  case 172:
#line 1014 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new CommaExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7542 "Parser/parser.cc"
    break;

  case 173:
#line 1019 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7548 "Parser/parser.cc"
    break;

  case 185:
#line 1037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 7554 "Parser/parser.cc"
    break;

  case 187:
#line 1040 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_directive( (yyvsp[0].tok) ) ); }
#line 7560 "Parser/parser.cc"
    break;

  case 188:
#line 1046 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[0].sn)->add_label( (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 7566 "Parser/parser.cc"
    break;

  case 189:
#line 1051 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (StatementNode *)0 ) ); }
#line 7572 "Parser/parser.cc"
    break;

  case 190:
#line 1056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (yyvsp[-2].sn) ) ); }
#line 7578 "Parser/parser.cc"
    break;

  case 192:
#line 1062 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 7584 "Parser/parser.cc"
    break;

  case 193:
#line 1067 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7590 "Parser/parser.cc"
    break;

  case 194:
#line 1069 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7596 "Parser/parser.cc"
    break;

  case 195:
#line 1071 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7602 "Parser/parser.cc"
    break;

  case 196:
#line 1073 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 7608 "Parser/parser.cc"
    break;

  case 199:
#line 1080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 7614 "Parser/parser.cc"
    break;

  case 200:
#line 1085 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_expr( (yyvsp[-1].en) ) ); }
#line 7620 "Parser/parser.cc"
    break;

  case 201:
#line 1092 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn); }
#line 7626 "Parser/parser.cc"
    break;

  case 202:
#line 1094 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( true, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 7632 "Parser/parser.cc"
    break;

  case 203:
#line 1096 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( true, (yyvsp[-7].en), (yyvsp[-2].sn) ) );
			// The semantics of the declaration list is changed to include associated initialization, which is performed
			// *before* the transfer to the appropriate case clause by hoisting the declarations into a compound
			// statement around the switch.  Statements after the initial declaration list can never be executed, and
			// therefore, are removed from the grammar even though C allows it. The change also applies to choose
			// statement.
			(yyval.sn) = (yyvsp[-3].decl) ? new StatementNode( build_compound( (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 7646 "Parser/parser.cc"
    break;

  case 204:
#line 1106 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( false, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 7652 "Parser/parser.cc"
    break;

  case 205:
#line 1108 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( false, (yyvsp[-7].en), (yyvsp[-2].sn) ) );
			(yyval.sn) = (yyvsp[-3].decl) ? new StatementNode( build_compound( (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 7661 "Parser/parser.cc"
    break;

  case 206:
#line 1117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-2].ifctl), maybe_build_compound( (yyvsp[0].sn) ), nullptr ) ); }
#line 7667 "Parser/parser.cc"
    break;

  case 207:
#line 1119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-4].ifctl), maybe_build_compound( (yyvsp[-2].sn) ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 7673 "Parser/parser.cc"
    break;

  case 208:
#line 1124 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new IfCtrl( nullptr, (yyvsp[0].en) ); }
#line 7679 "Parser/parser.cc"
    break;

  case 209:
#line 1126 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new IfCtrl( (yyvsp[0].decl), nullptr ); }
#line 7685 "Parser/parser.cc"
    break;

  case 210:
#line 1128 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new IfCtrl( (yyvsp[0].decl), nullptr ); }
#line 7691 "Parser/parser.cc"
    break;

  case 211:
#line 1130 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new IfCtrl( (yyvsp[-1].decl), (yyvsp[0].en) ); }
#line 7697 "Parser/parser.cc"
    break;

  case 212:
#line 1137 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = (yyvsp[0].en); }
#line 7703 "Parser/parser.cc"
    break;

  case 213:
#line 1139 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7709 "Parser/parser.cc"
    break;

  case 215:
#line 1144 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.sn) = new StatementNode( build_case( (yyvsp[0].en) ) ); }
#line 7715 "Parser/parser.cc"
    break;

  case 216:
#line 1146 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)((yyvsp[-2].sn)->set_last( new StatementNode( build_case( (yyvsp[0].en) ) ) ) ); }
#line 7721 "Parser/parser.cc"
    break;

  case 217:
#line 1150 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn); }
#line 7727 "Parser/parser.cc"
    break;

  case 218:
#line 1151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.sn) = new StatementNode( build_default() ); }
#line 7733 "Parser/parser.cc"
    break;

  case 220:
#line 1163 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)( (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) )); }
#line 7739 "Parser/parser.cc"
    break;

  case 221:
#line 1167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( maybe_build_compound( (yyvsp[0].sn) ) ); }
#line 7745 "Parser/parser.cc"
    break;

  case 222:
#line 1172 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = nullptr; }
#line 7751 "Parser/parser.cc"
    break;

  case 224:
#line 1178 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ); }
#line 7757 "Parser/parser.cc"
    break;

  case 225:
#line 1180 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)( (yyvsp[-2].sn)->set_last( (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ) ) ); }
#line 7763 "Parser/parser.cc"
    break;

  case 226:
#line 1185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( (yyvsp[-3].ifctl), maybe_build_compound( (yyvsp[-1].sn) ) ) ); }
#line 7769 "Parser/parser.cc"
    break;

  case 227:
#line 1187 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( new IfCtrl( nullptr, new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 7775 "Parser/parser.cc"
    break;

  case 228:
#line 1189 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( (yyvsp[-2].en), maybe_build_compound( (yyvsp[-5].sn) ) ) ); }
#line 7781 "Parser/parser.cc"
    break;

  case 229:
#line 1191 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( new ExpressionNode( build_constantInteger( *new string( "1" ) ) ), maybe_build_compound( (yyvsp[-4].sn) ) ) ); }
#line 7787 "Parser/parser.cc"
    break;

  case 230:
#line 1193 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( (yyvsp[-3].fctl), maybe_build_compound( (yyvsp[-1].sn) ) ) ); }
#line 7793 "Parser/parser.cc"
    break;

  case 231:
#line 1195 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( new ForCtrl( (ExpressionNode * )nullptr, (ExpressionNode * )nullptr, (ExpressionNode * )nullptr ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 7799 "Parser/parser.cc"
    break;

  case 233:
#line 1205 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 7818 "Parser/parser.cc"
    break;

  case 234:
#line 1223 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (ExpressionNode * )nullptr, (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 7824 "Parser/parser.cc"
    break;

  case 235:
#line 1225 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 7830 "Parser/parser.cc"
    break;

  case 236:
#line 1227 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-3].decl), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 7836 "Parser/parser.cc"
    break;

  case 237:
#line 1230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7843 "Parser/parser.cc"
    break;

  case 238:
#line 1233 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LEThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7850 "Parser/parser.cc"
    break;

  case 239:
#line 1236 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].en)->clone(), (yyvsp[-1].compop), (yyvsp[0].en), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7856 "Parser/parser.cc"
    break;

  case 240:
#line 1238 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 7862 "Parser/parser.cc"
    break;

  case 241:
#line 1240 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( new ExpressionNode( build_constantInteger( *new string( "0u" ) ) ), (yyvsp[-1].en), nullptr, OperKinds::LThan, nullptr, nullptr ); }
#line 7868 "Parser/parser.cc"
    break;

  case 242:
#line 1242 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-2].en), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7875 "Parser/parser.cc"
    break;

  case 243:
#line 1245 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-3].en), new ExpressionNode( build_constantInteger( *new string( "0" ) ) ),
						OperKinds::LEThan, (yyvsp[0].en)->clone(), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7882 "Parser/parser.cc"
    break;

  case 244:
#line 1248 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), (yyvsp[-1].compop), (yyvsp[0].en), new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7888 "Parser/parser.cc"
    break;

  case 245:
#line 1250 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 7894 "Parser/parser.cc"
    break;

  case 246:
#line 1253 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Array interator is currently unimplemented." ); (yyval.fctl) = nullptr;
			(yyval.fctl) = forCtrl( new ExpressionNode( build_varref( (yyvsp[0].tok) ) ), (yyvsp[-2].en), nullptr, OperKinds::Range, nullptr, nullptr );
		}
#line 7903 "Parser/parser.cc"
    break;

  case 247:
#line 1260 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), OperKinds::LThan, nullptr, new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7909 "Parser/parser.cc"
    break;

  case 248:
#line 1262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), OperKinds::GThan, nullptr, new ExpressionNode( build_constantInteger( *new string( "1" ) ) ) ); }
#line 7915 "Parser/parser.cc"
    break;

  case 249:
#line 1264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), OperKinds::LThan, nullptr, (yyvsp[0].en) ); }
#line 7921 "Parser/parser.cc"
    break;

  case 250:
#line 1266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), OperKinds::GThan, nullptr, (yyvsp[0].en) ); }
#line 7927 "Parser/parser.cc"
    break;

  case 251:
#line 1268 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), OperKinds::LThan, nullptr, nullptr ); }
#line 7933 "Parser/parser.cc"
    break;

  case 252:
#line 1273 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LThan; }
#line 7939 "Parser/parser.cc"
    break;

  case 253:
#line 1275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 7945 "Parser/parser.cc"
    break;

  case 254:
#line 1277 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GThan; }
#line 7951 "Parser/parser.cc"
    break;

  case 255:
#line 1279 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GEThan; }
#line 7957 "Parser/parser.cc"
    break;

  case 256:
#line 1284 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Goto ) ); }
#line 7963 "Parser/parser.cc"
    break;

  case 257:
#line 1288 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_computedgoto( (yyvsp[-1].en) ) ); }
#line 7969 "Parser/parser.cc"
    break;

  case 258:
#line 1291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThrough ) ); }
#line 7975 "Parser/parser.cc"
    break;

  case 259:
#line 1293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::FallThrough ) ); }
#line 7981 "Parser/parser.cc"
    break;

  case 260:
#line 1295 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThroughDefault ) ); }
#line 7987 "Parser/parser.cc"
    break;

  case 261:
#line 1298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Continue ) ); }
#line 7993 "Parser/parser.cc"
    break;

  case 262:
#line 1302 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Continue ) ); }
#line 7999 "Parser/parser.cc"
    break;

  case 263:
#line 1305 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Break ) ); }
#line 8005 "Parser/parser.cc"
    break;

  case 264:
#line 1309 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Break ) ); }
#line 8011 "Parser/parser.cc"
    break;

  case 265:
#line 1311 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_return( (yyvsp[-1].en) ) ); }
#line 8017 "Parser/parser.cc"
    break;

  case 266:
#line 1313 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 8023 "Parser/parser.cc"
    break;

  case 267:
#line 1315 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr ) ); }
#line 8029 "Parser/parser.cc"
    break;

  case 268:
#line 1317 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn) ) ); }
#line 8035 "Parser/parser.cc"
    break;

  case 269:
#line 1319 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Coroutine ) ); }
#line 8041 "Parser/parser.cc"
    break;

  case 270:
#line 1321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Coroutine ) ); }
#line 8047 "Parser/parser.cc"
    break;

  case 271:
#line 1323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Generator ) ); }
#line 8053 "Parser/parser.cc"
    break;

  case 272:
#line 1325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Generator ) ); }
#line 8059 "Parser/parser.cc"
    break;

  case 273:
#line 1327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_throw( (yyvsp[-1].en) ) ); }
#line 8065 "Parser/parser.cc"
    break;

  case 274:
#line 1329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume( (yyvsp[-1].en) ) ); }
#line 8071 "Parser/parser.cc"
    break;

  case 275:
#line 1331 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume_at( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 8077 "Parser/parser.cc"
    break;

  case 278:
#line 1341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_with( (yyvsp[-2].en), (yyvsp[0].sn) ) );
		}
#line 8085 "Parser/parser.cc"
    break;

  case 279:
#line 1349 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_mutex( (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8091 "Parser/parser.cc"
    break;

  case 280:
#line 1353 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 8097 "Parser/parser.cc"
    break;

  case 281:
#line 1358 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8103 "Parser/parser.cc"
    break;

  case 283:
#line 1364 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 8109 "Parser/parser.cc"
    break;

  case 284:
#line 1368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) )); }
#line 8115 "Parser/parser.cc"
    break;

  case 286:
#line 1375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8121 "Parser/parser.cc"
    break;

  case 287:
#line 1379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 8127 "Parser/parser.cc"
    break;

  case 288:
#line 1384 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8133 "Parser/parser.cc"
    break;

  case 289:
#line 1386 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-3].en), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[-4].en), (yyvsp[0].wfs) ); }
#line 8139 "Parser/parser.cc"
    break;

  case 290:
#line 1388 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8145 "Parser/parser.cc"
    break;

  case 291:
#line 1390 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( nullptr, maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8151 "Parser/parser.cc"
    break;

  case 292:
#line 1393 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 8157 "Parser/parser.cc"
    break;

  case 293:
#line 1395 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-5].en), maybe_build_compound( (yyvsp[-4].sn) ), (yyvsp[-6].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8163 "Parser/parser.cc"
    break;

  case 294:
#line 1400 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-1].en), (yyvsp[0].sn), (yyvsp[-2].en) ) ); }
#line 8169 "Parser/parser.cc"
    break;

  case 295:
#line 1402 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-3].en), (yyvsp[-2].sn), (yyvsp[-4].en), (yyvsp[0].wfs) ) ); }
#line 8175 "Parser/parser.cc"
    break;

  case 296:
#line 1407 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), (yyvsp[0].sn), 0 ) ); }
#line 8181 "Parser/parser.cc"
    break;

  case 297:
#line 1409 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), 0, (yyvsp[0].sn) ) ); }
#line 8187 "Parser/parser.cc"
    break;

  case 298:
#line 1411 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-2].sn), (yyvsp[-1].sn), (yyvsp[0].sn) ) ); }
#line 8193 "Parser/parser.cc"
    break;

  case 299:
#line 1416 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8199 "Parser/parser.cc"
    break;

  case 300:
#line 1418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)(yyvsp[-8].sn)->set_last( new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ) ); }
#line 8205 "Parser/parser.cc"
    break;

  case 301:
#line 1423 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8211 "Parser/parser.cc"
    break;

  case 302:
#line 1424 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[0].en); }
#line 8217 "Parser/parser.cc"
    break;

  case 303:
#line 1428 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 8223 "Parser/parser.cc"
    break;

  case 304:
#line 1429 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 8229 "Parser/parser.cc"
    break;

  case 305:
#line 1430 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.catch_kind) = CatchStmt::Resume; }
#line 8235 "Parser/parser.cc"
    break;

  case 306:
#line 1431 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Resume; }
#line 8241 "Parser/parser.cc"
    break;

  case 307:
#line 1435 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = new StatementNode( build_finally( (yyvsp[0].sn) ) ); }
#line 8247 "Parser/parser.cc"
    break;

  case 309:
#line 1442 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 8253 "Parser/parser.cc"
    break;

  case 310:
#line 1444 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 8259 "Parser/parser.cc"
    break;

  case 311:
#line 1446 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 8265 "Parser/parser.cc"
    break;

  case 316:
#line 1461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-4].flag), (yyvsp[-2].constant), 0 ) ); }
#line 8271 "Parser/parser.cc"
    break;

  case 317:
#line 1463 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-6].flag), (yyvsp[-4].constant), (yyvsp[-2].en) ) ); }
#line 8277 "Parser/parser.cc"
    break;

  case 318:
#line 1465 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-8].flag), (yyvsp[-6].constant), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 8283 "Parser/parser.cc"
    break;

  case 319:
#line 1467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-10].flag), (yyvsp[-8].constant), (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 8289 "Parser/parser.cc"
    break;

  case 320:
#line 1469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-12].flag), (yyvsp[-9].constant), 0, (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].label) ) ); }
#line 8295 "Parser/parser.cc"
    break;

  case 321:
#line 1474 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = false; }
#line 8301 "Parser/parser.cc"
    break;

  case 322:
#line 1476 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = true; }
#line 8307 "Parser/parser.cc"
    break;

  case 323:
#line 1481 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8313 "Parser/parser.cc"
    break;

  case 326:
#line 1488 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 8319 "Parser/parser.cc"
    break;

  case 327:
#line 1493 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( nullptr, (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 8325 "Parser/parser.cc"
    break;

  case 328:
#line 1495 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( (yyvsp[-5].tok), (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 8331 "Parser/parser.cc"
    break;

  case 329:
#line 1500 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8337 "Parser/parser.cc"
    break;

  case 330:
#line 1502 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 8343 "Parser/parser.cc"
    break;

  case 331:
#line 1504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( (yyvsp[0].constant) ) )); }
#line 8349 "Parser/parser.cc"
    break;

  case 332:
#line 1509 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = new LabelNode(); (yyval.label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 8358 "Parser/parser.cc"
    break;

  case 333:
#line 1514 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = (yyvsp[-2].label); (yyvsp[-2].label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 8367 "Parser/parser.cc"
    break;

  case 334:
#line 1524 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8373 "Parser/parser.cc"
    break;

  case 337:
#line 1531 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 8379 "Parser/parser.cc"
    break;

  case 338:
#line 1536 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8385 "Parser/parser.cc"
    break;

  case 340:
#line 1542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 8391 "Parser/parser.cc"
    break;

  case 341:
#line 1544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-2].decl) ); }
#line 8397 "Parser/parser.cc"
    break;

  case 351:
#line 1570 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-4].en), (yyvsp[-2].constant) ); }
#line 8403 "Parser/parser.cc"
    break;

  case 352:
#line 1572 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-2].en), build_constantStr( *new string( "\"\"" ) ) ); }
#line 8409 "Parser/parser.cc"
    break;

  case 356:
#line 1590 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 8415 "Parser/parser.cc"
    break;

  case 358:
#line 1596 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].in) ); }
#line 8421 "Parser/parser.cc"
    break;

  case 359:
#line 1600 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 8427 "Parser/parser.cc"
    break;

  case 360:
#line 1602 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].in) ) ); }
#line 8433 "Parser/parser.cc"
    break;

  case 361:
#line 1609 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 8439 "Parser/parser.cc"
    break;

  case 362:
#line 1611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 8445 "Parser/parser.cc"
    break;

  case 363:
#line 1613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 8451 "Parser/parser.cc"
    break;

  case 365:
#line 1619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 8457 "Parser/parser.cc"
    break;

  case 366:
#line 1621 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 8463 "Parser/parser.cc"
    break;

  case 367:
#line 1623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 8469 "Parser/parser.cc"
    break;

  case 368:
#line 1625 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeClone( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->appendList( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 8480 "Parser/parser.cc"
    break;

  case 369:
#line 1658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), 0 )->addQualifiers( (yyvsp[0].decl) ); }
#line 8486 "Parser/parser.cc"
    break;

  case 370:
#line 1660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), 0 )->addQualifiers( (yyvsp[0].decl) ); }
#line 8492 "Parser/parser.cc"
    break;

  case 371:
#line 1665 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 8498 "Parser/parser.cc"
    break;

  case 372:
#line 1668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-6].decl)->appendList( (yyvsp[-2].decl) ) ); }
#line 8504 "Parser/parser.cc"
    break;

  case 373:
#line 1673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 8513 "Parser/parser.cc"
    break;

  case 374:
#line 1678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 8522 "Parser/parser.cc"
    break;

  case 375:
#line 1683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "3" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 8531 "Parser/parser.cc"
    break;

  case 376:
#line 1694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "4" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef();
		}
#line 8540 "Parser/parser.cc"
    break;

  case 377:
#line 1699 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "5" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 8549 "Parser/parser.cc"
    break;

  case 378:
#line 1704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "6" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[-3].decl) )->addTypedef();
		}
#line 8558 "Parser/parser.cc"
    break;

  case 379:
#line 1709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "7" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-2].decl) )->addTypedef();
		}
#line 8567 "Parser/parser.cc"
    break;

  case 380:
#line 1714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "8" );
			(yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-3].decl) )->addTypedef()->addType( (yyvsp[-3].decl) );
		}
#line 8576 "Parser/parser.cc"
    break;

  case 381:
#line 1723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 8584 "Parser/parser.cc"
    break;

  case 382:
#line 1727 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 8592 "Parser/parser.cc"
    break;

  case 383:
#line 1734 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 8598 "Parser/parser.cc"
    break;

  case 387:
#line 1744 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 8604 "Parser/parser.cc"
    break;

  case 388:
#line 1746 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ) ); }
#line 8610 "Parser/parser.cc"
    break;

  case 401:
#line 1785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8616 "Parser/parser.cc"
    break;

  case 404:
#line 1797 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 8622 "Parser/parser.cc"
    break;

  case 407:
#line 1807 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Const ); }
#line 8628 "Parser/parser.cc"
    break;

  case 408:
#line 1809 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Restrict ); }
#line 8634 "Parser/parser.cc"
    break;

  case 409:
#line 1811 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Volatile ); }
#line 8640 "Parser/parser.cc"
    break;

  case 410:
#line 1813 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Atomic ); }
#line 8646 "Parser/parser.cc"
    break;

  case 412:
#line 1819 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newForall( (yyvsp[-1].decl) ); }
#line 8652 "Parser/parser.cc"
    break;

  case 414:
#line 1825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 8658 "Parser/parser.cc"
    break;

  case 415:
#line 1827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 8664 "Parser/parser.cc"
    break;

  case 417:
#line 1838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 8670 "Parser/parser.cc"
    break;

  case 418:
#line 1843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Extern ); }
#line 8676 "Parser/parser.cc"
    break;

  case 419:
#line 1845 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Static ); }
#line 8682 "Parser/parser.cc"
    break;

  case 420:
#line 1847 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Auto ); }
#line 8688 "Parser/parser.cc"
    break;

  case 421:
#line 1849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Register ); }
#line 8694 "Parser/parser.cc"
    break;

  case 422:
#line 1851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Threadlocal ); }
#line 8700 "Parser/parser.cc"
    break;

  case 423:
#line 1854 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Inline ); }
#line 8706 "Parser/parser.cc"
    break;

  case 424:
#line 1856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Fortran ); }
#line 8712 "Parser/parser.cc"
    break;

  case 425:
#line 1858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Noreturn ); }
#line 8718 "Parser/parser.cc"
    break;

  case 426:
#line 1863 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 8724 "Parser/parser.cc"
    break;

  case 427:
#line 1865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Bool ); }
#line 8730 "Parser/parser.cc"
    break;

  case 428:
#line 1867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Char ); }
#line 8736 "Parser/parser.cc"
    break;

  case 429:
#line 1869 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int ); }
#line 8742 "Parser/parser.cc"
    break;

  case 430:
#line 1871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 ); }
#line 8748 "Parser/parser.cc"
    break;

  case 431:
#line 1873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 )->addType( DeclarationNode::newSignedNess( DeclarationNode::Unsigned ) ); }
#line 8754 "Parser/parser.cc"
    break;

  case 432:
#line 1875 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Float ); }
#line 8760 "Parser/parser.cc"
    break;

  case 433:
#line 1877 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Double ); }
#line 8766 "Parser/parser.cc"
    break;

  case 434:
#line 1879 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat80 ); }
#line 8772 "Parser/parser.cc"
    break;

  case 435:
#line 1881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat128 ); }
#line 8778 "Parser/parser.cc"
    break;

  case 436:
#line 1883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat16 ); }
#line 8784 "Parser/parser.cc"
    break;

  case 437:
#line 1885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32 ); }
#line 8790 "Parser/parser.cc"
    break;

  case 438:
#line 1887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32x ); }
#line 8796 "Parser/parser.cc"
    break;

  case 439:
#line 1889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64 ); }
#line 8802 "Parser/parser.cc"
    break;

  case 440:
#line 1891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64x ); }
#line 8808 "Parser/parser.cc"
    break;

  case 441:
#line 1893 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat128 ); }
#line 8814 "Parser/parser.cc"
    break;

  case 442:
#line 1895 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 8820 "Parser/parser.cc"
    break;

  case 443:
#line 1897 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 8826 "Parser/parser.cc"
    break;

  case 444:
#line 1899 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 8832 "Parser/parser.cc"
    break;

  case 445:
#line 1901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Complex ); }
#line 8838 "Parser/parser.cc"
    break;

  case 446:
#line 1903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Imaginary ); }
#line 8844 "Parser/parser.cc"
    break;

  case 447:
#line 1905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Signed ); }
#line 8850 "Parser/parser.cc"
    break;

  case 448:
#line 1907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Unsigned ); }
#line 8856 "Parser/parser.cc"
    break;

  case 449:
#line 1909 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Short ); }
#line 8862 "Parser/parser.cc"
    break;

  case 450:
#line 1911 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Long ); }
#line 8868 "Parser/parser.cc"
    break;

  case 451:
#line 1913 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Valist ); }
#line 8874 "Parser/parser.cc"
    break;

  case 452:
#line 1915 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::AutoType ); }
#line 8880 "Parser/parser.cc"
    break;

  case 454:
#line 1921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8886 "Parser/parser.cc"
    break;

  case 456:
#line 1927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVtableType( (yyvsp[-2].decl) ); }
#line 8892 "Parser/parser.cc"
    break;

  case 457:
#line 1933 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 8898 "Parser/parser.cc"
    break;

  case 458:
#line 1935 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 8904 "Parser/parser.cc"
    break;

  case 460:
#line 1942 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 8910 "Parser/parser.cc"
    break;

  case 461:
#line 1944 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 8916 "Parser/parser.cc"
    break;

  case 462:
#line 1946 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 8922 "Parser/parser.cc"
    break;

  case 463:
#line 1948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 8928 "Parser/parser.cc"
    break;

  case 465:
#line 1955 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 8934 "Parser/parser.cc"
    break;

  case 467:
#line 1961 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 8940 "Parser/parser.cc"
    break;

  case 468:
#line 1963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 8946 "Parser/parser.cc"
    break;

  case 469:
#line 1965 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 8952 "Parser/parser.cc"
    break;

  case 470:
#line 1970 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 8958 "Parser/parser.cc"
    break;

  case 471:
#line 1972 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en) ); }
#line 8964 "Parser/parser.cc"
    break;

  case 472:
#line 1974 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 8970 "Parser/parser.cc"
    break;

  case 473:
#line 1976 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en), true ); }
#line 8976 "Parser/parser.cc"
    break;

  case 474:
#line 1978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Zero ); }
#line 8982 "Parser/parser.cc"
    break;

  case 475:
#line 1980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::One ); }
#line 8988 "Parser/parser.cc"
    break;

  case 477:
#line 1986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 8994 "Parser/parser.cc"
    break;

  case 478:
#line 1988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9000 "Parser/parser.cc"
    break;

  case 479:
#line 1990 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9006 "Parser/parser.cc"
    break;

  case 481:
#line 1996 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 9012 "Parser/parser.cc"
    break;

  case 482:
#line 1998 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 9018 "Parser/parser.cc"
    break;

  case 483:
#line 2000 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 9027 "Parser/parser.cc"
    break;

  case 485:
#line 2009 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9033 "Parser/parser.cc"
    break;

  case 486:
#line 2011 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9039 "Parser/parser.cc"
    break;

  case 487:
#line 2013 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9045 "Parser/parser.cc"
    break;

  case 489:
#line 2019 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9051 "Parser/parser.cc"
    break;

  case 490:
#line 2021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9057 "Parser/parser.cc"
    break;

  case 492:
#line 2027 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9063 "Parser/parser.cc"
    break;

  case 493:
#line 2029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9069 "Parser/parser.cc"
    break;

  case 494:
#line 2031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9075 "Parser/parser.cc"
    break;

  case 496:
#line 2037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9081 "Parser/parser.cc"
    break;

  case 497:
#line 2039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9087 "Parser/parser.cc"
    break;

  case 498:
#line 2044 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypedef( (yyvsp[0].tok) ); }
#line 9093 "Parser/parser.cc"
    break;

  case 499:
#line 2046 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 9099 "Parser/parser.cc"
    break;

  case 500:
#line 2048 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 9105 "Parser/parser.cc"
    break;

  case 502:
#line 2051 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), (yyvsp[0].decl) ); }
#line 9111 "Parser/parser.cc"
    break;

  case 503:
#line 2053 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), (yyvsp[0].decl) ); }
#line 9117 "Parser/parser.cc"
    break;

  case 504:
#line 2058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[0].tok), nullptr ); }
#line 9123 "Parser/parser.cc"
    break;

  case 505:
#line 2060 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-2].tok), nullptr ); }
#line 9129 "Parser/parser.cc"
    break;

  case 506:
#line 2062 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 9135 "Parser/parser.cc"
    break;

  case 511:
#line 2077 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 9141 "Parser/parser.cc"
    break;

  case 512:
#line 2079 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 9147 "Parser/parser.cc"
    break;

  case 513:
#line 2081 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 9156 "Parser/parser.cc"
    break;

  case 514:
#line 2086 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) ); }
#line 9162 "Parser/parser.cc"
    break;

  case 515:
#line 2088 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 9171 "Parser/parser.cc"
    break;

  case 516:
#line 2093 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypedef( (yyvsp[-5].tok) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 9180 "Parser/parser.cc"
    break;

  case 517:
#line 2098 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 9189 "Parser/parser.cc"
    break;

  case 518:
#line 2103 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeGen( (yyvsp[-5].tok), nullptr );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 9198 "Parser/parser.cc"
    break;

  case 520:
#line 2112 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9204 "Parser/parser.cc"
    break;

  case 521:
#line 2114 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 9210 "Parser/parser.cc"
    break;

  case 522:
#line 2119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 9220 "Parser/parser.cc"
    break;

  case 523:
#line 2125 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 9235 "Parser/parser.cc"
    break;

  case 526:
#line 2144 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Struct; }
#line 9241 "Parser/parser.cc"
    break;

  case 527:
#line 2146 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Union; }
#line 9247 "Parser/parser.cc"
    break;

  case 528:
#line 2148 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Exception; }
#line 9253 "Parser/parser.cc"
    break;

  case 529:
#line 2154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 9259 "Parser/parser.cc"
    break;

  case 530:
#line 2156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 9265 "Parser/parser.cc"
    break;

  case 531:
#line 2158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Generator; }
#line 9271 "Parser/parser.cc"
    break;

  case 532:
#line 2160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor generator is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 9277 "Parser/parser.cc"
    break;

  case 533:
#line 2162 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Coroutine; }
#line 9283 "Parser/parser.cc"
    break;

  case 534:
#line 2164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor coroutine is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 9289 "Parser/parser.cc"
    break;

  case 535:
#line 2166 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Thread; }
#line 9295 "Parser/parser.cc"
    break;

  case 536:
#line 2168 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor thread is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 9301 "Parser/parser.cc"
    break;

  case 537:
#line 2173 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9307 "Parser/parser.cc"
    break;

  case 538:
#line 2175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 9313 "Parser/parser.cc"
    break;

  case 539:
#line 2180 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); }
#line 9319 "Parser/parser.cc"
    break;

  case 540:
#line 2182 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 9325 "Parser/parser.cc"
    break;

  case 541:
#line 2184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-1].decl) ) {								// field declarator ?
				(yyvsp[-1].decl) = DeclarationNode::newName( nullptr );
			} // if
			(yyvsp[-1].decl)->inLine = true;
			(yyval.decl) = distAttr( (yyvsp[-2].decl), (yyvsp[-1].decl) );					// mark all fields in list
			distInl( (yyvsp[-1].decl) );
		}
#line 9338 "Parser/parser.cc"
    break;

  case 542:
#line 2193 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9344 "Parser/parser.cc"
    break;

  case 545:
#line 2197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 9350 "Parser/parser.cc"
    break;

  case 546:
#line 2199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 9356 "Parser/parser.cc"
    break;

  case 549:
#line 2206 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9362 "Parser/parser.cc"
    break;

  case 551:
#line 2209 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 9368 "Parser/parser.cc"
    break;

  case 552:
#line 2214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].en) ); }
#line 9374 "Parser/parser.cc"
    break;

  case 553:
#line 2217 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 9380 "Parser/parser.cc"
    break;

  case 554:
#line 2220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 9386 "Parser/parser.cc"
    break;

  case 555:
#line 2225 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9392 "Parser/parser.cc"
    break;

  case 557:
#line 2228 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 9398 "Parser/parser.cc"
    break;

  case 559:
#line 2239 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 9404 "Parser/parser.cc"
    break;

  case 560:
#line 2241 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 9410 "Parser/parser.cc"
    break;

  case 562:
#line 2248 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 9416 "Parser/parser.cc"
    break;

  case 563:
#line 2253 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9422 "Parser/parser.cc"
    break;

  case 565:
#line 2259 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 9428 "Parser/parser.cc"
    break;

  case 566:
#line 2264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-4].decl) ); }
#line 9434 "Parser/parser.cc"
    break;

  case 567:
#line 2266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); }
#line 9440 "Parser/parser.cc"
    break;

  case 568:
#line 2268 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].tok), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) ); }
#line 9446 "Parser/parser.cc"
    break;

  case 569:
#line 2271 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-4].decl)->name, (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 9452 "Parser/parser.cc"
    break;

  case 570:
#line 2273 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-6].decl)->storageClasses.val != 0 || (yyvsp[-6].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			SemanticError( yylloc, "Typed enumeration is currently unimplemented." ); (yyval.decl) = nullptr;
		}
#line 9461 "Parser/parser.cc"
    break;

  case 571:
#line 2278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].decl)->storageClasses.val != 0 || (yyvsp[-4].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			typedefTable.makeTypedef( *(yyvsp[-1].tok) );
		}
#line 9470 "Parser/parser.cc"
    break;

  case 572:
#line 2283 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typed enumeration is currently unimplemented." ); (yyval.decl) = nullptr;
		}
#line 9478 "Parser/parser.cc"
    break;

  case 573:
#line 2287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-8].decl)->storageClasses.val != 0 || (yyvsp[-8].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			typedefTable.makeTypedef( *(yyvsp[-5].decl)->name );
			SemanticError( yylloc, "Typed enumeration is currently unimplemented." ); (yyval.decl) = nullptr;
		}
#line 9488 "Parser/parser.cc"
    break;

  case 575:
#line 2297 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); (yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), 0, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 9494 "Parser/parser.cc"
    break;

  case 576:
#line 2299 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].decl)->type->symbolic.name );	(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].decl)->type->symbolic.name, 0, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 9500 "Parser/parser.cc"
    break;

  case 577:
#line 2304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumConstant( (yyvsp[-1].tok), (yyvsp[0].en) ); }
#line 9506 "Parser/parser.cc"
    break;

  case 578:
#line 2306 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumConstant( new string("inline"), nullptr ); }
#line 9512 "Parser/parser.cc"
    break;

  case 579:
#line 2308 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( DeclarationNode::newEnumConstant( (yyvsp[-1].tok), (yyvsp[0].en) ) ); }
#line 9518 "Parser/parser.cc"
    break;

  case 580:
#line 2310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( DeclarationNode::newEnumConstant( new string("inline"), nullptr ) ); }
#line 9524 "Parser/parser.cc"
    break;

  case 581:
#line 2315 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9530 "Parser/parser.cc"
    break;

  case 582:
#line 2319 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].in)->get_expression(); }
#line 9536 "Parser/parser.cc"
    break;

  case 583:
#line 2324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 9542 "Parser/parser.cc"
    break;

  case 584:
#line 2326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9548 "Parser/parser.cc"
    break;

  case 587:
#line 2330 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9554 "Parser/parser.cc"
    break;

  case 588:
#line 2332 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 9560 "Parser/parser.cc"
    break;

  case 589:
#line 2334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 9566 "Parser/parser.cc"
    break;

  case 591:
#line 2342 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9572 "Parser/parser.cc"
    break;

  case 592:
#line 2344 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9578 "Parser/parser.cc"
    break;

  case 593:
#line 2346 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-8].decl)->appendList( (yyvsp[-4].decl) )->appendList( (yyvsp[0].decl) ); }
#line 9584 "Parser/parser.cc"
    break;

  case 595:
#line 2352 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9590 "Parser/parser.cc"
    break;

  case 596:
#line 2357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9596 "Parser/parser.cc"
    break;

  case 597:
#line 2359 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9602 "Parser/parser.cc"
    break;

  case 599:
#line 2362 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 9608 "Parser/parser.cc"
    break;

  case 602:
#line 2369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9614 "Parser/parser.cc"
    break;

  case 603:
#line 2371 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 9620 "Parser/parser.cc"
    break;

  case 605:
#line 2380 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 9626 "Parser/parser.cc"
    break;

  case 606:
#line 2383 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 9632 "Parser/parser.cc"
    break;

  case 607:
#line 2385 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 9638 "Parser/parser.cc"
    break;

  case 612:
#line 2395 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9644 "Parser/parser.cc"
    break;

  case 614:
#line 2402 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 9650 "Parser/parser.cc"
    break;

  case 615:
#line 2404 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 9656 "Parser/parser.cc"
    break;

  case 616:
#line 2409 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 9662 "Parser/parser.cc"
    break;

  case 617:
#line 2411 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 9668 "Parser/parser.cc"
    break;

  case 618:
#line 2420 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 9674 "Parser/parser.cc"
    break;

  case 619:
#line 2422 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 9680 "Parser/parser.cc"
    break;

  case 625:
#line 2435 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 9686 "Parser/parser.cc"
    break;

  case 628:
#line 2445 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 9692 "Parser/parser.cc"
    break;

  case 629:
#line 2446 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (yyvsp[-1].op) == OperKinds::Assign ? (yyvsp[0].in) : (yyvsp[0].in)->set_maybeConstructed( false ); }
#line 9698 "Parser/parser.cc"
    break;

  case 630:
#line 2447 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.in) = new InitializerNode( true ); }
#line 9704 "Parser/parser.cc"
    break;

  case 631:
#line 2448 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 9710 "Parser/parser.cc"
    break;

  case 632:
#line 2452 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = new InitializerNode( (yyvsp[0].en) ); }
#line 9716 "Parser/parser.cc"
    break;

  case 633:
#line 2453 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 9722 "Parser/parser.cc"
    break;

  case 634:
#line 2458 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 9728 "Parser/parser.cc"
    break;

  case 636:
#line 2460 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ); }
#line 9734 "Parser/parser.cc"
    break;

  case 637:
#line 2461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (InitializerNode *)( (yyvsp[-2].in)->set_last( (yyvsp[0].in) ) ); }
#line 9740 "Parser/parser.cc"
    break;

  case 638:
#line 2462 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.in) = (InitializerNode *)((yyvsp[-3].in)->set_last( (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ) )); }
#line 9746 "Parser/parser.cc"
    break;

  case 640:
#line 2478 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[-1].tok) ) ); }
#line 9752 "Parser/parser.cc"
    break;

  case 642:
#line 2484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-1].en)->set_last( (yyvsp[0].en) )); }
#line 9758 "Parser/parser.cc"
    break;

  case 643:
#line 2490 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 9764 "Parser/parser.cc"
    break;

  case 644:
#line 2493 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 9770 "Parser/parser.cc"
    break;

  case 645:
#line 2495 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 9776 "Parser/parser.cc"
    break;

  case 646:
#line 2497 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-4].en) ), maybeMoveBuild<Expression>( (yyvsp[-2].en) ) ) ); }
#line 9782 "Parser/parser.cc"
    break;

  case 647:
#line 2499 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 9788 "Parser/parser.cc"
    break;

  case 649:
#line 2523 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl) ); }
#line 9794 "Parser/parser.cc"
    break;

  case 650:
#line 2528 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9800 "Parser/parser.cc"
    break;

  case 651:
#line 2530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 9806 "Parser/parser.cc"
    break;

  case 652:
#line 2535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "9" );
			if ( (yyvsp[-1].tclass) == TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 9817 "Parser/parser.cc"
    break;

  case 653:
#line 2542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 9823 "Parser/parser.cc"
    break;

  case 654:
#line 2544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "9" ); }
#line 9829 "Parser/parser.cc"
    break;

  case 655:
#line 2546 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 9835 "Parser/parser.cc"
    break;

  case 656:
#line 2548 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "9" );
			(yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 9844 "Parser/parser.cc"
    break;

  case 657:
#line 2554 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 9850 "Parser/parser.cc"
    break;

  case 658:
#line 2559 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 9856 "Parser/parser.cc"
    break;

  case 659:
#line 2561 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 9862 "Parser/parser.cc"
    break;

  case 660:
#line 2563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::DStype; }
#line 9868 "Parser/parser.cc"
    break;

  case 661:
#line 2567 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 9874 "Parser/parser.cc"
    break;

  case 662:
#line 2572 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 9880 "Parser/parser.cc"
    break;

  case 663:
#line 2574 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 9886 "Parser/parser.cc"
    break;

  case 664:
#line 2576 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ftype; }
#line 9892 "Parser/parser.cc"
    break;

  case 665:
#line 2578 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 9898 "Parser/parser.cc"
    break;

  case 666:
#line 2583 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9904 "Parser/parser.cc"
    break;

  case 669:
#line 2590 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 9910 "Parser/parser.cc"
    break;

  case 670:
#line 2595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 9916 "Parser/parser.cc"
    break;

  case 671:
#line 2597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 9922 "Parser/parser.cc"
    break;

  case 672:
#line 2604 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 9928 "Parser/parser.cc"
    break;

  case 674:
#line 2607 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 9934 "Parser/parser.cc"
    break;

  case 675:
#line 2609 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)( (yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 9940 "Parser/parser.cc"
    break;

  case 676:
#line 2614 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 9946 "Parser/parser.cc"
    break;

  case 677:
#line 2616 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 9952 "Parser/parser.cc"
    break;

  case 678:
#line 2618 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 9958 "Parser/parser.cc"
    break;

  case 679:
#line 2623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 9964 "Parser/parser.cc"
    break;

  case 680:
#line 2625 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 9970 "Parser/parser.cc"
    break;

  case 681:
#line 2630 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "10" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), 0 );
		}
#line 9979 "Parser/parser.cc"
    break;

  case 682:
#line 2635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "11" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 9988 "Parser/parser.cc"
    break;

  case 683:
#line 2643 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), 0 ); }
#line 9994 "Parser/parser.cc"
    break;

  case 684:
#line 2645 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-8].tok), (yyvsp[-6].decl), (yyvsp[-2].decl) ); }
#line 10000 "Parser/parser.cc"
    break;

  case 686:
#line 2651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl) ); }
#line 10006 "Parser/parser.cc"
    break;

  case 691:
#line 2663 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 10012 "Parser/parser.cc"
    break;

  case 692:
#line 2668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10018 "Parser/parser.cc"
    break;

  case 693:
#line 2670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 10024 "Parser/parser.cc"
    break;

  case 695:
#line 2678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl);	}
#line 10030 "Parser/parser.cc"
    break;

  case 696:
#line 2683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10036 "Parser/parser.cc"
    break;

  case 697:
#line 2685 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->appendList( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 10042 "Parser/parser.cc"
    break;

  case 698:
#line 2690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10048 "Parser/parser.cc"
    break;

  case 700:
#line 2695 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 10054 "Parser/parser.cc"
    break;

  case 701:
#line 2699 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 10060 "Parser/parser.cc"
    break;

  case 702:
#line 2704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( (yyvsp[0].tok) ) ) ); }
#line 10066 "Parser/parser.cc"
    break;

  case 705:
#line 2708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 10075 "Parser/parser.cc"
    break;

  case 706:
#line 2713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( false, (yyvsp[-2].constant), 0 ) ) ); }
#line 10081 "Parser/parser.cc"
    break;

  case 707:
#line 2715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = LinkageSpec::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 10090 "Parser/parser.cc"
    break;

  case 708:
#line 2720 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10100 "Parser/parser.cc"
    break;

  case 709:
#line 2726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 10109 "Parser/parser.cc"
    break;

  case 710:
#line 2731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10119 "Parser/parser.cc"
    break;

  case 711:
#line 2737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 10128 "Parser/parser.cc"
    break;

  case 712:
#line 2742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10138 "Parser/parser.cc"
    break;

  case 713:
#line 2748 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.val) || (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 10147 "Parser/parser.cc"
    break;

  case 714:
#line 2753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10157 "Parser/parser.cc"
    break;

  case 716:
#line 2768 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].sn) ); }
#line 10163 "Parser/parser.cc"
    break;

  case 717:
#line 2770 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].sn) ); }
#line 10169 "Parser/parser.cc"
    break;

  case 718:
#line 2775 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; forall = false; }
#line 10175 "Parser/parser.cc"
    break;

  case 719:
#line 2777 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); forall = false; }
#line 10181 "Parser/parser.cc"
    break;

  case 720:
#line 2782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10192 "Parser/parser.cc"
    break;

  case 721:
#line 2789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 10201 "Parser/parser.cc"
    break;

  case 722:
#line 2794 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 10210 "Parser/parser.cc"
    break;

  case 723:
#line 2800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 10216 "Parser/parser.cc"
    break;

  case 724:
#line 2803 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 10222 "Parser/parser.cc"
    break;

  case 725:
#line 2806 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10228 "Parser/parser.cc"
    break;

  case 726:
#line 2810 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-4].decl) );
		}
#line 10237 "Parser/parser.cc"
    break;

  case 727:
#line 2816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10243 "Parser/parser.cc"
    break;

  case 728:
#line 2819 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10249 "Parser/parser.cc"
    break;

  case 729:
#line 2822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 10255 "Parser/parser.cc"
    break;

  case 733:
#line 2833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 10261 "Parser/parser.cc"
    break;

  case 734:
#line 2838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10267 "Parser/parser.cc"
    break;

  case 735:
#line 2840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = (yyvsp[-2].constant);
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 10277 "Parser/parser.cc"
    break;

  case 736:
#line 2849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10283 "Parser/parser.cc"
    break;

  case 739:
#line 2856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10289 "Parser/parser.cc"
    break;

  case 740:
#line 2861 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 10295 "Parser/parser.cc"
    break;

  case 742:
#line 2867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10301 "Parser/parser.cc"
    break;

  case 743:
#line 2872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10307 "Parser/parser.cc"
    break;

  case 744:
#line 2874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 10313 "Parser/parser.cc"
    break;

  case 745:
#line 2876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 10319 "Parser/parser.cc"
    break;

  case 750:
#line 2885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 10325 "Parser/parser.cc"
    break;

  case 751:
#line 2887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 10331 "Parser/parser.cc"
    break;

  case 752:
#line 2922 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 10337 "Parser/parser.cc"
    break;

  case 753:
#line 2924 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10343 "Parser/parser.cc"
    break;

  case 754:
#line 2929 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10349 "Parser/parser.cc"
    break;

  case 756:
#line 2932 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10355 "Parser/parser.cc"
    break;

  case 757:
#line 2934 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10361 "Parser/parser.cc"
    break;

  case 758:
#line 2939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10367 "Parser/parser.cc"
    break;

  case 759:
#line 2941 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10373 "Parser/parser.cc"
    break;

  case 760:
#line 2943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10379 "Parser/parser.cc"
    break;

  case 761:
#line 2945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10385 "Parser/parser.cc"
    break;

  case 762:
#line 2950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 10391 "Parser/parser.cc"
    break;

  case 763:
#line 2952 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10397 "Parser/parser.cc"
    break;

  case 764:
#line 2954 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10403 "Parser/parser.cc"
    break;

  case 765:
#line 2956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10409 "Parser/parser.cc"
    break;

  case 766:
#line 2958 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10415 "Parser/parser.cc"
    break;

  case 767:
#line 2960 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10421 "Parser/parser.cc"
    break;

  case 768:
#line 2962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10427 "Parser/parser.cc"
    break;

  case 769:
#line 2967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10433 "Parser/parser.cc"
    break;

  case 770:
#line 2969 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 10439 "Parser/parser.cc"
    break;

  case 771:
#line 2971 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10445 "Parser/parser.cc"
    break;

  case 772:
#line 2973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10451 "Parser/parser.cc"
    break;

  case 773:
#line 2982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10457 "Parser/parser.cc"
    break;

  case 775:
#line 2985 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10463 "Parser/parser.cc"
    break;

  case 776:
#line 2990 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10469 "Parser/parser.cc"
    break;

  case 777:
#line 2992 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10475 "Parser/parser.cc"
    break;

  case 778:
#line 2994 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 10481 "Parser/parser.cc"
    break;

  case 779:
#line 2996 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10487 "Parser/parser.cc"
    break;

  case 780:
#line 2998 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10493 "Parser/parser.cc"
    break;

  case 781:
#line 3003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10499 "Parser/parser.cc"
    break;

  case 782:
#line 3005 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10505 "Parser/parser.cc"
    break;

  case 783:
#line 3007 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10511 "Parser/parser.cc"
    break;

  case 784:
#line 3009 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10517 "Parser/parser.cc"
    break;

  case 785:
#line 3014 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10523 "Parser/parser.cc"
    break;

  case 786:
#line 3016 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10529 "Parser/parser.cc"
    break;

  case 787:
#line 3018 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10535 "Parser/parser.cc"
    break;

  case 788:
#line 3020 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10541 "Parser/parser.cc"
    break;

  case 789:
#line 3022 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10547 "Parser/parser.cc"
    break;

  case 790:
#line 3024 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10553 "Parser/parser.cc"
    break;

  case 794:
#line 3042 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 10559 "Parser/parser.cc"
    break;

  case 795:
#line 3044 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10565 "Parser/parser.cc"
    break;

  case 796:
#line 3046 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 10571 "Parser/parser.cc"
    break;

  case 797:
#line 3048 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10577 "Parser/parser.cc"
    break;

  case 798:
#line 3050 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10583 "Parser/parser.cc"
    break;

  case 799:
#line 3055 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10589 "Parser/parser.cc"
    break;

  case 800:
#line 3057 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10595 "Parser/parser.cc"
    break;

  case 801:
#line 3059 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10601 "Parser/parser.cc"
    break;

  case 802:
#line 3061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10607 "Parser/parser.cc"
    break;

  case 803:
#line 3066 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10613 "Parser/parser.cc"
    break;

  case 804:
#line 3068 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10619 "Parser/parser.cc"
    break;

  case 805:
#line 3070 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10625 "Parser/parser.cc"
    break;

  case 806:
#line 3072 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10631 "Parser/parser.cc"
    break;

  case 807:
#line 3074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10637 "Parser/parser.cc"
    break;

  case 808:
#line 3076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10643 "Parser/parser.cc"
    break;

  case 809:
#line 3091 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "ID" );
		}
#line 10652 "Parser/parser.cc"
    break;

  case 810:
#line 3096 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10658 "Parser/parser.cc"
    break;

  case 811:
#line 3101 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10664 "Parser/parser.cc"
    break;

  case 813:
#line 3104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10670 "Parser/parser.cc"
    break;

  case 814:
#line 3106 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10676 "Parser/parser.cc"
    break;

  case 815:
#line 3111 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10682 "Parser/parser.cc"
    break;

  case 816:
#line 3113 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10688 "Parser/parser.cc"
    break;

  case 817:
#line 3115 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10694 "Parser/parser.cc"
    break;

  case 818:
#line 3117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10700 "Parser/parser.cc"
    break;

  case 819:
#line 3122 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 10706 "Parser/parser.cc"
    break;

  case 820:
#line 3124 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10712 "Parser/parser.cc"
    break;

  case 821:
#line 3126 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10718 "Parser/parser.cc"
    break;

  case 822:
#line 3128 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10724 "Parser/parser.cc"
    break;

  case 823:
#line 3130 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 10730 "Parser/parser.cc"
    break;

  case 824:
#line 3132 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10736 "Parser/parser.cc"
    break;

  case 825:
#line 3134 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10742 "Parser/parser.cc"
    break;

  case 826:
#line 3139 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10748 "Parser/parser.cc"
    break;

  case 827:
#line 3141 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10754 "Parser/parser.cc"
    break;

  case 828:
#line 3143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 10760 "Parser/parser.cc"
    break;

  case 829:
#line 3145 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10766 "Parser/parser.cc"
    break;

  case 830:
#line 3147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10772 "Parser/parser.cc"
    break;

  case 831:
#line 3157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10778 "Parser/parser.cc"
    break;

  case 832:
#line 3159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10784 "Parser/parser.cc"
    break;

  case 834:
#line 3162 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10790 "Parser/parser.cc"
    break;

  case 835:
#line 3164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10796 "Parser/parser.cc"
    break;

  case 836:
#line 3169 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10802 "Parser/parser.cc"
    break;

  case 837:
#line 3171 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10808 "Parser/parser.cc"
    break;

  case 838:
#line 3173 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10814 "Parser/parser.cc"
    break;

  case 839:
#line 3178 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 10820 "Parser/parser.cc"
    break;

  case 840:
#line 3180 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10826 "Parser/parser.cc"
    break;

  case 841:
#line 3182 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10832 "Parser/parser.cc"
    break;

  case 842:
#line 3184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10838 "Parser/parser.cc"
    break;

  case 843:
#line 3189 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10844 "Parser/parser.cc"
    break;

  case 844:
#line 3191 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10850 "Parser/parser.cc"
    break;

  case 845:
#line 3193 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10856 "Parser/parser.cc"
    break;

  case 846:
#line 3207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10862 "Parser/parser.cc"
    break;

  case 847:
#line 3209 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10868 "Parser/parser.cc"
    break;

  case 849:
#line 3212 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10874 "Parser/parser.cc"
    break;

  case 850:
#line 3214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10880 "Parser/parser.cc"
    break;

  case 851:
#line 3219 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 10886 "Parser/parser.cc"
    break;

  case 852:
#line 3221 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 10892 "Parser/parser.cc"
    break;

  case 853:
#line 3226 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10898 "Parser/parser.cc"
    break;

  case 854:
#line 3228 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10904 "Parser/parser.cc"
    break;

  case 855:
#line 3230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10910 "Parser/parser.cc"
    break;

  case 856:
#line 3235 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 10916 "Parser/parser.cc"
    break;

  case 857:
#line 3237 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10922 "Parser/parser.cc"
    break;

  case 858:
#line 3242 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10928 "Parser/parser.cc"
    break;

  case 859:
#line 3244 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 10934 "Parser/parser.cc"
    break;

  case 861:
#line 3262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10940 "Parser/parser.cc"
    break;

  case 862:
#line 3264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10946 "Parser/parser.cc"
    break;

  case 863:
#line 3269 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( 0, (yyvsp[0].op) ); }
#line 10952 "Parser/parser.cc"
    break;

  case 864:
#line 3271 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 10958 "Parser/parser.cc"
    break;

  case 865:
#line 3273 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 10964 "Parser/parser.cc"
    break;

  case 866:
#line 3275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 10970 "Parser/parser.cc"
    break;

  case 867:
#line 3277 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10976 "Parser/parser.cc"
    break;

  case 869:
#line 3283 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10982 "Parser/parser.cc"
    break;

  case 870:
#line 3285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 10988 "Parser/parser.cc"
    break;

  case 871:
#line 3287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10994 "Parser/parser.cc"
    break;

  case 872:
#line 3292 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 11000 "Parser/parser.cc"
    break;

  case 873:
#line 3294 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11006 "Parser/parser.cc"
    break;

  case 874:
#line 3296 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11012 "Parser/parser.cc"
    break;

  case 875:
#line 3302 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false ); }
#line 11018 "Parser/parser.cc"
    break;

  case 876:
#line 3304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false )->addArray( (yyvsp[0].decl) ); }
#line 11024 "Parser/parser.cc"
    break;

  case 877:
#line 3306 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-4].en), 0, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].en), 0, false ) ); }
#line 11030 "Parser/parser.cc"
    break;

  case 879:
#line 3313 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), 0, false ); }
#line 11036 "Parser/parser.cc"
    break;

  case 880:
#line 3315 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 11042 "Parser/parser.cc"
    break;

  case 881:
#line 3317 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newArray( (yyvsp[-2].en), 0, false ) ); }
#line 11048 "Parser/parser.cc"
    break;

  case 882:
#line 3319 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 11054 "Parser/parser.cc"
    break;

  case 884:
#line 3354 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 11060 "Parser/parser.cc"
    break;

  case 885:
#line 3356 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11066 "Parser/parser.cc"
    break;

  case 886:
#line 3358 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11072 "Parser/parser.cc"
    break;

  case 887:
#line 3363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].op) ); }
#line 11078 "Parser/parser.cc"
    break;

  case 888:
#line 3365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 11084 "Parser/parser.cc"
    break;

  case 889:
#line 3367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 11090 "Parser/parser.cc"
    break;

  case 890:
#line 3369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11096 "Parser/parser.cc"
    break;

  case 891:
#line 3371 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11102 "Parser/parser.cc"
    break;

  case 893:
#line 3377 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11108 "Parser/parser.cc"
    break;

  case 894:
#line 3379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11114 "Parser/parser.cc"
    break;

  case 895:
#line 3381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11120 "Parser/parser.cc"
    break;

  case 896:
#line 3386 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 11126 "Parser/parser.cc"
    break;

  case 897:
#line 3388 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11132 "Parser/parser.cc"
    break;

  case 898:
#line 3390 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11138 "Parser/parser.cc"
    break;

  case 900:
#line 3397 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11144 "Parser/parser.cc"
    break;

  case 902:
#line 3408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false ); }
#line 11150 "Parser/parser.cc"
    break;

  case 903:
#line 3411 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 11156 "Parser/parser.cc"
    break;

  case 904:
#line 3413 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, (yyvsp[-2].decl), false ); }
#line 11162 "Parser/parser.cc"
    break;

  case 905:
#line 3416 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 11168 "Parser/parser.cc"
    break;

  case 906:
#line 3418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 11174 "Parser/parser.cc"
    break;

  case 907:
#line 3420 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-4].decl), true ); }
#line 11180 "Parser/parser.cc"
    break;

  case 909:
#line 3435 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11186 "Parser/parser.cc"
    break;

  case 910:
#line 3437 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11192 "Parser/parser.cc"
    break;

  case 911:
#line 3442 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( 0, (yyvsp[0].op) ); }
#line 11198 "Parser/parser.cc"
    break;

  case 912:
#line 3444 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 11204 "Parser/parser.cc"
    break;

  case 913:
#line 3446 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11210 "Parser/parser.cc"
    break;

  case 914:
#line 3448 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11216 "Parser/parser.cc"
    break;

  case 915:
#line 3450 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11222 "Parser/parser.cc"
    break;

  case 917:
#line 3456 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11228 "Parser/parser.cc"
    break;

  case 918:
#line 3458 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11234 "Parser/parser.cc"
    break;

  case 919:
#line 3460 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11240 "Parser/parser.cc"
    break;

  case 920:
#line 3465 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11246 "Parser/parser.cc"
    break;

  case 921:
#line 3467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11252 "Parser/parser.cc"
    break;

  case 924:
#line 3477 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11258 "Parser/parser.cc"
    break;

  case 927:
#line 3488 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11264 "Parser/parser.cc"
    break;

  case 928:
#line 3490 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11270 "Parser/parser.cc"
    break;

  case 929:
#line 3492 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11276 "Parser/parser.cc"
    break;

  case 930:
#line 3494 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11282 "Parser/parser.cc"
    break;

  case 931:
#line 3496 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11288 "Parser/parser.cc"
    break;

  case 932:
#line 3498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11294 "Parser/parser.cc"
    break;

  case 933:
#line 3505 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11300 "Parser/parser.cc"
    break;

  case 934:
#line 3507 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11306 "Parser/parser.cc"
    break;

  case 935:
#line 3509 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11312 "Parser/parser.cc"
    break;

  case 936:
#line 3511 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 11318 "Parser/parser.cc"
    break;

  case 937:
#line 3513 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11324 "Parser/parser.cc"
    break;

  case 938:
#line 3516 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11330 "Parser/parser.cc"
    break;

  case 939:
#line 3518 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11336 "Parser/parser.cc"
    break;

  case 940:
#line 3520 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 11342 "Parser/parser.cc"
    break;

  case 941:
#line 3522 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 11348 "Parser/parser.cc"
    break;

  case 942:
#line 3524 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11354 "Parser/parser.cc"
    break;

  case 943:
#line 3529 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 11360 "Parser/parser.cc"
    break;

  case 944:
#line 3531 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 11366 "Parser/parser.cc"
    break;

  case 945:
#line 3536 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 11372 "Parser/parser.cc"
    break;

  case 946:
#line 3538 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl)->addQualifiers( (yyvsp[-4].decl) ), true ); }
#line 11378 "Parser/parser.cc"
    break;

  case 948:
#line 3565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11384 "Parser/parser.cc"
    break;

  case 952:
#line 3576 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11390 "Parser/parser.cc"
    break;

  case 953:
#line 3578 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11396 "Parser/parser.cc"
    break;

  case 954:
#line 3580 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11402 "Parser/parser.cc"
    break;

  case 955:
#line 3582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11408 "Parser/parser.cc"
    break;

  case 956:
#line 3584 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11414 "Parser/parser.cc"
    break;

  case 957:
#line 3586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 11420 "Parser/parser.cc"
    break;

  case 958:
#line 3593 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11426 "Parser/parser.cc"
    break;

  case 959:
#line 3595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11432 "Parser/parser.cc"
    break;

  case 960:
#line 3597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11438 "Parser/parser.cc"
    break;

  case 961:
#line 3599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11444 "Parser/parser.cc"
    break;

  case 962:
#line 3601 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 11450 "Parser/parser.cc"
    break;

  case 963:
#line 3603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 11456 "Parser/parser.cc"
    break;

  case 964:
#line 3608 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 11462 "Parser/parser.cc"
    break;

  case 965:
#line 3610 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11468 "Parser/parser.cc"
    break;

  case 966:
#line 3612 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11474 "Parser/parser.cc"
    break;

  case 967:
#line 3619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 11480 "Parser/parser.cc"
    break;

  case 968:
#line 3621 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 11486 "Parser/parser.cc"
    break;

  case 971:
#line 3645 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 11492 "Parser/parser.cc"
    break;

  case 972:
#line 3647 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 11498 "Parser/parser.cc"
    break;


#line 11502 "Parser/parser.cc"

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
#line 3650 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "make install" //
// End: //
