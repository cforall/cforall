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

#include "SynTree/Attribute.h"     // for Attribute

// lex uses __null in a boolean context, it's fine.
//#pragma GCC diagnostic ignored "-Wparentheses-equality"

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

DeclarationNode * distAttr( DeclarationNode * typeSpec, DeclarationNode * declList ) {
	// distribute declaration_specifier across all declared variables, e.g., static, const, but not __attribute__.
	assert( declList );
//	printf( "distAttr1 typeSpec %p\n", typeSpec ); typeSpec->print( std::cout );
	DeclarationNode * cur = declList, * cl = (new DeclarationNode)->addType( typeSpec );
//	printf( "distAttr2 cl %p\n", cl ); cl->type->print( std::cout );
//	cl->type->aggregate.name = cl->type->aggInst.aggregate->aggregate.name;

	for ( cur = dynamic_cast<DeclarationNode *>( cur->get_next() ); cur != nullptr; cur = dynamic_cast<DeclarationNode *>( cur->get_next() ) ) {
		cl->cloneBaseType( cur );
	} // for
	declList->addType( cl );
//	printf( "distAttr3 declList %p\n", declList ); declList->print( std::cout, 0 );
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
			// printf( "fieldDecl1 typeSpec %p\n", typeSpec ); typeSpec->type->print( std::cout );
			SemanticWarning( yylloc, Warning::SuperfluousDecl, ss.str().c_str() );
			return nullptr;
		} // if
		// printf( "fieldDecl2 typeSpec %p\n", typeSpec ); typeSpec->type->print( std::cout );
		fieldList = DeclarationNode::newName( nullptr );
	} // if
//	return distAttr( typeSpec, fieldList );				// mark all fields in list

	// printf( "fieldDecl3 typeSpec %p\n", typeSpec ); typeSpec->print( std::cout, 0 );
	DeclarationNode * temp = distAttr( typeSpec, fieldList );				// mark all fields in list
	// printf( "fieldDecl4 temp %p\n", temp ); temp->print( std::cout, 0 );
	return temp;
} // fieldDecl

#define NEW_ZERO new ExpressionNode( build_constantInteger( *new string( "0" ) ) )
#define NEW_ONE  new ExpressionNode( build_constantInteger( *new string( "1" ) ) )

ForCtrl * forCtrl( DeclarationNode * index, ExpressionNode * start, enum OperKinds compop, ExpressionNode * comp, ExpressionNode * inc ) {
	if ( index->initializer ) {
		SemanticError( yylloc, "Direct initialization disallowed. Use instead: type var; initialization ~ comparison ~ increment." );
	} // if
	if ( index->next ) {
		SemanticError( yylloc, "Multiple loop indexes disallowed in for-loop declaration." );
	} // if
	return new ForCtrl( index->addInitializer( new InitializerNode( start ) ),
		// NULL comp/inc => leave blank
		comp ? new ExpressionNode( build_binary_val( compop, new ExpressionNode( build_varref( new string( *index->name ) ) ), comp ) ) : nullptr,
		inc ? new ExpressionNode( build_binary_val( compop == OperKinds::LThan || compop == OperKinds::LEThan ? // choose += or -= for upto/downto
							OperKinds::PlusAssn : OperKinds::MinusAssn, new ExpressionNode( build_varref( new string( *index->name ) ) ), inc ) ) : nullptr );
} // forCtrl

ForCtrl * forCtrl( ExpressionNode * type, string * index, ExpressionNode * start, enum OperKinds compop, ExpressionNode * comp, ExpressionNode * inc ) {
	ConstantExpr * constant = dynamic_cast<ConstantExpr *>(type->expr.get());
	if ( constant && (constant->get_constant()->get_value() == "0" || constant->get_constant()->get_value() == "1") ) {
		type = new ExpressionNode( new CastExpr( maybeMoveBuild<Expression>(type), new BasicType( Type::Qualifiers(), BasicType::SignedInt ) ) );
	} // if
//	type = new ExpressionNode( build_func( new ExpressionNode( build_varref( new string( "__for_control_index_constraints__" ) ) ), type ) );
	return new ForCtrl(
		distAttr( DeclarationNode::newTypeof( type, true ), DeclarationNode::newName( index )->addInitializer( new InitializerNode( start ) ) ),
		// NULL comp/inc => leave blank
		comp ? new ExpressionNode( build_binary_val( compop, new ExpressionNode( build_varref( new string( *index ) ) ), comp ) ) : nullptr,
		inc ? new ExpressionNode( build_binary_val( compop == OperKinds::LThan || compop == OperKinds::LEThan ? // choose += or -= for upto/downto
							OperKinds::PlusAssn : OperKinds::MinusAssn, new ExpressionNode( build_varref( new string( *index ) ) ), inc ) ) : nullptr );
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

#line 293 "Parser/parser.cc"

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
#line 264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"

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

#line 661 "Parser/parser.cc"

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
#define YYLAST   20542

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  175
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  291
/* YYNRULES -- Number of rules.  */
#define YYNRULES  1015
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  2060

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
       0,   563,   563,   567,   574,   575,   576,   577,   578,   582,
     583,   584,   585,   586,   587,   588,   592,   593,   597,   598,
     603,   607,   608,   619,   621,   623,   627,   628,   630,   632,
     634,   636,   646,   654,   663,   664,   674,   679,   684,   685,
     690,   696,   698,   700,   706,   708,   710,   712,   714,   716,
     718,   720,   722,   724,   726,   728,   730,   732,   734,   736,
     738,   748,   749,   753,   754,   759,   762,   766,   767,   771,
     772,   774,   776,   778,   780,   782,   787,   789,   791,   799,
     800,   808,   811,   812,   814,   819,   835,   837,   839,   841,
     843,   845,   847,   849,   851,   859,   860,   862,   866,   867,
     868,   869,   873,   874,   876,   878,   880,   882,   884,   886,
     888,   895,   896,   897,   898,   902,   903,   907,   908,   913,
     914,   916,   918,   923,   924,   926,   931,   932,   934,   939,
     940,   942,   944,   946,   951,   952,   954,   959,   960,   965,
     966,   971,   972,   977,   978,   983,   984,   989,   990,   993,
     998,  1003,  1004,  1012,  1018,  1019,  1023,  1024,  1028,  1029,
    1033,  1034,  1035,  1036,  1037,  1038,  1039,  1040,  1041,  1042,
    1043,  1053,  1055,  1060,  1061,  1063,  1065,  1070,  1071,  1077,
    1078,  1084,  1085,  1086,  1087,  1088,  1089,  1090,  1091,  1092,
    1093,  1094,  1096,  1097,  1103,  1105,  1115,  1117,  1125,  1126,
    1131,  1133,  1135,  1137,  1139,  1143,  1144,  1146,  1151,  1153,
    1160,  1162,  1164,  1174,  1176,  1178,  1183,  1188,  1191,  1196,
    1198,  1200,  1202,  1210,  1211,  1213,  1217,  1219,  1223,  1225,
    1226,  1228,  1230,  1235,  1236,  1240,  1245,  1246,  1250,  1252,
    1257,  1259,  1264,  1266,  1268,  1270,  1275,  1277,  1279,  1281,
    1286,  1288,  1293,  1294,  1316,  1318,  1320,  1323,  1325,  1327,
    1329,  1332,  1335,  1337,  1340,  1342,  1347,  1349,  1351,  1353,
    1355,  1357,  1360,  1362,  1364,  1366,  1372,  1374,  1376,  1378,
    1380,  1383,  1385,  1387,  1389,  1395,  1400,  1407,  1409,  1411,
    1416,  1418,  1423,  1424,  1426,  1431,  1433,  1438,  1440,  1442,
    1444,  1447,  1451,  1454,  1458,  1460,  1462,  1464,  1466,  1468,
    1470,  1472,  1474,  1476,  1478,  1483,  1484,  1488,  1494,  1499,
    1504,  1505,  1509,  1513,  1518,  1519,  1525,  1529,  1531,  1533,
    1535,  1538,  1540,  1545,  1547,  1552,  1554,  1556,  1561,  1563,
    1569,  1570,  1574,  1575,  1576,  1577,  1581,  1586,  1587,  1589,
    1591,  1593,  1597,  1601,  1602,  1606,  1608,  1610,  1612,  1614,
    1620,  1621,  1627,  1628,  1632,  1633,  1638,  1640,  1646,  1647,
    1649,  1654,  1659,  1670,  1671,  1675,  1676,  1682,  1683,  1687,
    1689,  1693,  1695,  1699,  1700,  1704,  1705,  1709,  1716,  1717,
    1721,  1723,  1738,  1739,  1740,  1741,  1743,  1747,  1749,  1753,
    1760,  1762,  1764,  1769,  1770,  1772,  1774,  1776,  1808,  1811,
    1816,  1818,  1824,  1829,  1834,  1845,  1850,  1855,  1860,  1865,
    1874,  1878,  1885,  1887,  1888,  1889,  1895,  1897,  1902,  1903,
    1904,  1913,  1914,  1915,  1919,  1920,  1927,  1936,  1937,  1938,
    1943,  1944,  1953,  1954,  1959,  1960,  1964,  1966,  1968,  1970,
    1972,  1976,  1981,  1982,  1984,  1994,  1995,  2000,  2002,  2004,
    2006,  2008,  2011,  2013,  2015,  2020,  2022,  2024,  2026,  2028,
    2030,  2032,  2034,  2036,  2038,  2040,  2042,  2044,  2046,  2048,
    2050,  2052,  2054,  2056,  2058,  2060,  2062,  2064,  2066,  2068,
    2070,  2072,  2074,  2079,  2080,  2084,  2091,  2092,  2098,  2099,
    2101,  2103,  2105,  2110,  2112,  2117,  2118,  2120,  2122,  2127,
    2129,  2131,  2133,  2135,  2137,  2142,  2149,  2151,  2153,  2158,
    2166,  2165,  2169,  2177,  2178,  2180,  2182,  2187,  2188,  2190,
    2195,  2196,  2198,  2200,  2205,  2206,  2208,  2213,  2215,  2217,
    2219,  2220,  2222,  2227,  2229,  2231,  2236,  2243,  2247,  2248,
    2253,  2252,  2257,  2256,  2275,  2274,  2286,  2285,  2296,  2301,
    2302,  2307,  2313,  2327,  2328,  2332,  2334,  2336,  2342,  2344,
    2346,  2348,  2350,  2352,  2354,  2356,  2362,  2363,  2368,  2377,
    2379,  2388,  2390,  2391,  2392,  2394,  2396,  2397,  2402,  2403,
    2404,  2409,  2411,  2414,  2421,  2422,  2423,  2429,  2434,  2436,
    2442,  2443,  2449,  2450,  2454,  2459,  2462,  2461,  2465,  2468,
    2470,  2478,  2477,  2486,  2492,  2496,  2498,  2503,  2505,  2507,
    2509,  2515,  2516,  2517,  2524,  2525,  2527,  2528,  2529,  2531,
    2533,  2540,  2541,  2543,  2545,  2550,  2551,  2557,  2558,  2560,
    2561,  2566,  2567,  2568,  2570,  2578,  2579,  2581,  2584,  2586,
    2590,  2591,  2592,  2594,  2596,  2601,  2603,  2608,  2610,  2619,
    2621,  2626,  2627,  2628,  2632,  2633,  2634,  2639,  2640,  2645,
    2646,  2647,  2648,  2652,  2653,  2658,  2659,  2660,  2661,  2662,
    2676,  2677,  2682,  2683,  2689,  2691,  2694,  2696,  2698,  2721,
    2722,  2728,  2729,  2735,  2734,  2744,  2743,  2747,  2753,  2759,
    2760,  2762,  2766,  2771,  2773,  2775,  2777,  2783,  2784,  2788,
    2789,  2794,  2796,  2803,  2805,  2806,  2808,  2813,  2815,  2817,
    2822,  2824,  2829,  2834,  2842,  2844,  2849,  2850,  2855,  2856,
    2860,  2861,  2862,  2867,  2869,  2875,  2877,  2882,  2884,  2890,
    2891,  2895,  2899,  2903,  2905,  2906,  2907,  2912,  2915,  2914,
    2926,  2925,  2937,  2936,  2948,  2947,  2959,  2958,  2972,  2978,
    2980,  2986,  2987,  2998,  3005,  3010,  3016,  3019,  3022,  3026,
    3032,  3035,  3038,  3043,  3044,  3045,  3049,  3055,  3056,  3066,
    3067,  3071,  3072,  3077,  3082,  3083,  3089,  3090,  3092,  3097,
    3098,  3099,  3100,  3101,  3103,  3138,  3140,  3145,  3147,  3148,
    3150,  3155,  3157,  3159,  3161,  3166,  3168,  3170,  3172,  3174,
    3176,  3178,  3183,  3185,  3187,  3189,  3198,  3200,  3201,  3206,
    3208,  3210,  3212,  3214,  3219,  3221,  3223,  3225,  3230,  3232,
    3234,  3236,  3238,  3240,  3252,  3253,  3254,  3258,  3260,  3262,
    3264,  3266,  3271,  3273,  3275,  3277,  3282,  3284,  3286,  3288,
    3290,  3292,  3307,  3312,  3317,  3319,  3320,  3322,  3327,  3329,
    3331,  3333,  3338,  3340,  3342,  3344,  3346,  3348,  3350,  3355,
    3357,  3359,  3361,  3363,  3373,  3375,  3377,  3378,  3380,  3385,
    3387,  3389,  3394,  3396,  3398,  3400,  3405,  3407,  3409,  3423,
    3425,  3427,  3428,  3430,  3435,  3437,  3442,  3444,  3446,  3451,
    3453,  3458,  3460,  3477,  3478,  3480,  3485,  3487,  3489,  3491,
    3493,  3498,  3499,  3501,  3503,  3508,  3510,  3512,  3518,  3520,
    3522,  3525,  3529,  3531,  3533,  3535,  3569,  3570,  3572,  3574,
    3579,  3581,  3583,  3585,  3587,  3592,  3593,  3595,  3597,  3602,
    3604,  3606,  3612,  3613,  3615,  3624,  3627,  3629,  3632,  3634,
    3636,  3650,  3651,  3653,  3658,  3660,  3662,  3664,  3666,  3671,
    3672,  3674,  3676,  3681,  3683,  3691,  3692,  3693,  3698,  3699,
    3704,  3706,  3708,  3710,  3712,  3714,  3721,  3723,  3725,  3727,
    3729,  3732,  3734,  3736,  3738,  3740,  3745,  3747,  3749,  3754,
    3780,  3781,  3783,  3787,  3788,  3792,  3794,  3796,  3798,  3800,
    3802,  3809,  3811,  3813,  3815,  3817,  3819,  3824,  3826,  3828,
    3835,  3837,  3855,  3857,  3862,  3863
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
  "for_control_expression", "downupdowneq", "updown", "updowneq",
  "jump_statement", "fall_through_name", "with_statement",
  "mutex_statement", "when_clause", "when_clause_opt", "waitfor",
  "cast_expression_list", "timeout", "waitfor_clause", "waitfor_statement",
  "exception_statement", "handler_clause", "handler_predicate_opt",
  "handler_key", "finally_clause", "exception_declaration",
  "enable_disable_statement", "enable_disable_key", "asm_statement",
  "asm_volatile_opt", "asm_operands_opt", "asm_operands_list",
  "asm_operand", "asm_clobbers_list_opt", "label_list",
  "declaration_list_opt", "declaration_list", "KR_parameter_list_opt",
  "KR_parameter_list", "local_label_declaration_opt",
  "local_label_declaration_list", "local_label_list", "declaration",
  "static_assert", "cfa_declaration", "cfa_variable_declaration",
  "cfa_variable_specifier", "cfa_function_declaration",
  "cfa_function_specifier", "cfa_function_return",
  "cfa_typedef_declaration", "typedef_declaration", "typedef_expression",
  "c_declaration", "declaring_list", "declaration_specifier",
  "declaration_specifier_nobody", "type_specifier",
  "type_specifier_nobody", "type_qualifier_list_opt",
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

#define YYPACT_NINF (-1720)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-896)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
      73, 11764,    99,   159, 16344,    75, -1720, -1720, -1720, -1720,
   -1720, -1720, -1720, -1720, -1720, -1720, -1720,    47,   898,    67,
   -1720, -1720, -1720, -1720, -1720, -1720, -1720, -1720, -1720, -1720,
   -1720, -1720, -1720, -1720, -1720, -1720, -1720, -1720, -1720, -1720,
   -1720, -1720, -1720, -1720, -1720, -1720, -1720,    -2,   211, -1720,
   -1720, -1720, -1720, -1720, -1720,  4684,  4684,   152, 11764,   166,
     208, -1720, -1720,   235, -1720, -1720, -1720, -1720, -1720, -1720,
   -1720, -1720, -1720,  2536, -1720,   629,   250, -1720, -1720, -1720,
   -1720, -1720, 16194, -1720, -1720,   311,   269,   338,    42, -1720,
    4684,   269,   269,   269,   325,  4417,   517,   916, 11923, -1720,
   -1720, -1720, 16044,  1091, -1720, -1720, -1720,  2502,   544,  5699,
     573,   784,  2502,   875,   415, -1720, -1720, -1720, -1720,   503,
   -1720, -1720, -1720, -1720,   462, -1720, -1720, -1720, -1720, -1720,
     493,   501,   503, -1720,   503,   507, -1720, -1720, -1720, 16900,
    4684, -1720, -1720,  4684, -1720, 11764,   504, 16952, -1720, -1720,
    4478, 17964, -1720,   828,   828,   537,  2246, -1720, -1720, -1720,
   -1720,   520, 14180,  2994,   503, -1720, -1720, -1720, -1720, -1720,
   -1720,   601, -1720,   586,   627,   632, -1720,   679, 19836, 15274,
    3421,  2536,   301,   687,   711,   716,   725,   731,   753, -1720,
   -1720, 17102, 10723,   698, -1720, 16487, -1720, -1720, -1720, -1720,
     757, -1720, -1720,   762, -1720, 18828,   907,  8808, -1720,   789,
    4684,   501,   799,   795,   800,   820, -1720, -1720, -1720,  3551,
    2859,   826,   887,    68, -1720, -1720,   503,   503,    70,    74,
     119,    70, -1720,   503,   503, -1720,  4113, -1720, -1720,   848,
     865,   828, 14074, -1720, -1720, 16194, -1720, -1720,  2502, -1720,
    1514,   415,   832,   929,    74,  4684,   338, -1720, 13472, -1720,
     828,   828,   864,   929,    74,  4684, -1720, 20422, -1720, -1720,
     828, -1720,   828, -1720,  1146,  4207,  4684, -1720,   893,   900,
   -1720, -1720, -1720, 16646,   501,    76, -1720, -1720, 18014, -1720,
     887,    49, -1720, 19836, 17964,  3618,  4113, -1720,   348, -1720,
   -1720, -1720, 16952,  4684, -1720,   871, -1720, -1720, -1720, -1720,
    4684,  2622,   355,   478, -1720,  4684,   586, -1720,   857,   503,
     503,   896, 17154,   696, 14654, 14232,  2502,  2502, -1720,  2502,
     828,  2502,   828, -1720, -1720,   503, -1720,   908, -1720, 17304,
   -1720, -1720, -1720, 17356,   757, -1720,   915,   442,   722,   920,
     415,   923, -1720,  2246,   888,   586,  2246,  1213, -1720,   943,
     989, 19908,   962,   964, 19836, 19980,  1000, 15020, -1720, -1720,
   -1720, -1720, -1720, -1720, 20052, 20052, 15120,   973,  4100, -1720,
   -1720, -1720, -1720,   556, -1720,   584, -1720,  1441, -1720, 19836,
   19836, -1720,  1001,   706,   984,  1158,   600,  1168,  1014,  1022,
    1017,  1062,     4, -1720,   666, -1720,  1083, -1720,  1131,  3195,
   15582, -1720, -1720,   659,  1083, -1720, -1720,   724, -1720, -1720,
    3421,  1063,  1089,  1096,  1110,  1118,  1120, -1720, -1720,   388,
    1092, -1720,   743,  1092, -1720, -1720, 16900, -1720,  1143,  1088,
   15736, -1720, -1720,  4562,  4344,  1152, 14654,  1162,   598,   636,
   -1720, -1720, -1720, -1720, -1720,  4684,  4638, -1720, -1720, -1720,
   -1720, -1720, -1720, 13221,  3073,   973, 18828,  1121,  1123, -1720,
   -1720,  1134,  8808,   699, -1720, -1720, -1720, 19044,  1156, -1720,
   -1720, -1720, -1720, -1720,  3551,   767,  1165,  1176,  1183,   833,
    1193,  1199,  1203,  2859, -1720, -1720,   503,  1216,   338,  1229,
   -1720, -1720,  1214, -1720, -1720,   501,   929, -1720, -1720, -1720,
     501, -1720, -1720,  4113, -1720, 15582, 15582, -1720,   828,  4478,
   18742, 14812, -1720, -1720, -1720, -1720, -1720,   501,   929,    49,
   -1720, -1720,  2502,  1236,   929,    74, -1720,   501,   929, -1720,
   20472, -1720,   828,   828, -1720, -1720,  1257,   481,  1260,   415,
    1265, -1720, 18172, -1720,   786, -1720,  1332, 18638, -1720,  4478,
   17515, 14074, -1720, 16646, 20124, -1720, -1720, -1720, -1720, -1720,
    3618,   904,  4113, -1720, 14812,   887, 11764, -1720,  1270, -1720,
    1283, -1720, -1720, -1720, -1720, -1720,  2246, -1720, -1720,  1366,
    4287,  1289, 17356, 10723, -1720, 17567, -1720,   828,   828, -1720,
   -1720,   757, -1720,   748,  1295,  1456, 19836,  1452,  1214,  1285,
   -1720,   503,   503, -1720,  1092, -1720, 17154, -1720, -1720, 18453,
     828,   828, -1720,  4287,   503, -1720, 17821, -1720, -1720, 17304,
   -1720,   520,  1314,  1305,  1336,   722,   816, 16952,   823, -1720,
   -1720, -1720, -1720, -1720, -1720,   837, -1720,  1339,  1334, -1720,
   15428, -1720, 17619, 17619, -1720, 15428, -1720, 19836, -1720,  5699,
    5699, 15428, -1720, -1720, 16698, 17619, 17619,  1131,  1127,  1363,
     508,  1375, -1720,   840,  1359,  1148,  1368, -1720, 19044, 19836,
   19116,  1360, 19836,   893, 19836,   893, -1720,  2062, -1720, -1720,
   19188,  2137, 19836, 19188,   893, -1720, -1720, 19836, 19836, 19836,
   19836, 19836, 19836, 19836, 19836, 19836, 19836, 19836, 19836, 19836,
   19836, 19836, 19836, 19836, 19836, 19836, 19260,  1347,   679,  3788,
   10723, -1720, -1720, -1720, -1720, -1720, -1720, -1720, -1720, -1720,
   -1720, -1720,  1380, 19836, -1720, -1720,   659,  1824, -1720, -1720,
     503,   503, -1720, -1720, 15582, -1720,   401,  1092, -1720,   862,
    1092, -1720, -1720, -1720,  1214, -1720, -1720,  1214, 20196, -1720,
   -1720, 10723,  1378,  1384,  2702,  1522,  3012,   516,  1285, -1720,
     503,   503,  1285,   548, -1720,   503,   503, 19836,  4684,  1175,
    1177,  1285,   353, 14022, 14022,  4684, -1720, -1720, 19836,  1134,
   -1720, 18828,  1393, -1720,  1775, -1720, -1720, -1720, -1720, -1720,
     880, -1720, 14022,   893,  4478,   893,   954,  1391,  1392,  1394,
     963,  1395,  1397,  1399,   550,  1092, -1720, -1720,   558,  1092,
   -1720, -1720, -1720,  4478,   679, -1720,  1092, 20196, -1720,   501,
   18172, -1720, -1720,   974,  1400,   979,  1401, -1720,  1396, -1720,
     501, -1720, -1720,   501,   929,  1396, -1720,   501,  1398,  1407,
    1408, -1720, -1720, 18453, -1720,  1403, -1720, -1720, -1720,   893,
    4684,  9882,  1488,  1383, 18540, -1720,  1088, -1720, 14022,   983,
   -1720, -1720,  1396, -1720, 16952, 15582,  1409, -1720,  1409, -1720,
   -1720, -1720,   722, -1720, 17304, -1720, 10885, 15890, -1720, 18172,
    1429,  1433,  1434, -1720,  8295,   503, -1720,  1452, -1720, -1720,
   -1720, -1720,  1214, -1720, -1720, -1720,   828, -1720,  3281, -1720,
   -1720,   415,   935,  1418, 19332, -1720,   722,  1314, -1720, -1720,
    1431,  1435,  1213, 19188, -1720,  1440,   250,  1445,  1455,  1457,
    1448,  1458, 19836,  1459,  1460,  1466, 10723, 19836, -1720, -1720,
    1414, -1720, -1720, -1720, 19836, -1720,  1467,  1468, 18900,  1182,
   -1720, 19188,  1442, -1720,  1470, -1720, -1720,  1980, -1720, -1720,
    1005, -1720, -1720, -1720, -1720,  1980, -1720, -1720,  1192,    88,
   -1720, -1720,  1001,  1001,  1001,   706,   706,   984,   984,  1158,
    1158,  1158,  1158,   600,   600,  1168,  1014,  1022,  1017,  1062,
   19836,  1157, -1720,  1474,  1980, -1720, -1720, 18828, -1720, 18172,
    1475,  1476,  1479,  1824, -1720, -1720, -1720, -1720, -1720, -1720,
   -1720, -1720,  1214, -1720, -1720,  1214, 18172, 18172, -1720, -1720,
    2702,   911,  1480,  1482,  1483,  1484,  2326,  3012, -1720, -1720,
   -1720, -1720, -1720, -1720, -1720, -1720, -1720, -1720, -1720, -1720,
   -1720, -1720,  1485, -1720,  1285, -1720, -1720, -1720, -1720, -1720,
   -1720, -1720, -1720,  1489,  1490, -1720,   338,  1980,  1194,    10,
   -1720, -1720,  1496, -1720,  8808, -1720, 19836,   503, 19404, 14022,
   -1720, -1720, -1720,  1472,   566,  1092, -1720,   567,  1092, -1720,
   -1720, -1720, -1720,  1214, -1720, -1720, -1720,  1214,   887,  1495,
    1214, -1720, -1720, -1720, -1720, -1720, -1720, -1720,  1499, -1720,
   -1720,  1396, -1720,   501, -1720, -1720, -1720, -1720, -1720, 12553,
    1497,  1501, -1720,    84, -1720,   474,   419, 10561,  1502, 13851,
    1503,  1505,  2345,  2423,  2685, 19476,  1508, -1720, -1720,  1509,
    1510, -1720, -1720,   501, 19836, 19836,  1637,  1504,   273, -1720,
    1589,  1511,  1491, -1720, -1720, -1720,  9692, -1720, -1720, -1720,
   -1720, -1720,  2314, -1720, -1720, -1720,  1574, -1720, -1720, -1720,
     893, -1720, -1720, 12400, 16194,  1512, -1720,  4684, -1720,  1493,
    1517,  1518, -1720,  1198, -1720, -1720, -1720, -1720,  4478, -1720,
   -1720,  1507,  1513,  1013, 16952,   586,   586,  1314, -1720, -1720,
     973,  1088, 15736, -1720,  1083, -1720, 11047, -1720,   590,  1092,
   -1720,   828, 11601, -1720, -1720,   722,   503,   503,   520,  1305,
   -1720, 18828, -1720,  1314,  1516,  1525, -1720, -1720,  1018,   270,
   10723,   893, -1720,   270, 16750,   270, -1720, 19836, 19836, 19836,
   -1720, -1720, -1720, -1720, 19836, 19836,  1520, 18828, -1720, -1720,
    1523,   533, -1720, -1720, -1720,  3349, -1720, -1720,  1202, -1720,
     234, -1720, 19188,  1206, -1720, 19044, -1720, -1720, 19836,  1526,
    1215,  1243,  1134, -1720,   591,  1092, -1720, -1720, 18172, 18172,
   -1720, -1720,  1524,   596,  1092, -1720,   613,  1597,   503,   503,
   -1720, -1720, 18172, 18172, -1720,  1530, -1720, 14812, 14812,  1534,
    1535,  1537,  1540, -1720,  1539, 19836, 19836,  1248,  1542, -1720,
   -1720, -1720, -1720, -1720, -1720, -1720,  1548, 19836, -1720, -1720,
   -1720,  1214, -1720, -1720, -1720,  1214, 18172, 18172,   338,   503,
    1253,  1559,  1564, -1720, -1720,  1565, 12706, 12859, 13012, 16952,
   17619, 17619,  1568, -1720,  1544,  1545,   977, 13314, -1720,   239,
    4684, -1720, -1720,  4684, -1720, 18972,   -74,    11, -1720, -1720,
   -1720, -1720, 19836,  1573,  1646, 10398, 10054, -1720,  1552, -1720,
    1553, 19836,  1555, 18828,  1556, 19836, 19044, 19836,   958, -1720,
    1557,    31, -1720,   214,  1583, -1720, -1720,  1585, -1720,  1560,
   -1720,  1562,  1588, 13851,   650, 13630,   503,   317, -1720, -1720,
   -1720,  1587, -1720,  1591, -1720,  1592, -1720,  1594, -1720,  1599,
   -1720, -1720, -1720, -1720,  1596, 11209,  1598,  1602,  1604, -1720,
    1608, -1720, -1720, -1720,  1214, 19836, 19836,  1088,  1607, -1720,
    1314, -1720,  1612,   222, -1720,  1134,  1616, -1720, -1720, 16952,
   -1720,  1619,  1617,  1019, -1720,  1620, -1720, -1720, -1720, -1720,
   -1720, 18828,  1134, 19044, -1720,  1657,  1980, -1720,  1657,  1657,
   -1720,  1980,  3917,  4617, -1720, -1720,  1256, -1720, -1720, -1720,
    1633,  1634, -1720, -1720, -1720,  1214, -1720, -1720,  1635,  1636,
     503, -1720, -1720, -1720,  1214, -1720, -1720, -1720,  1638, -1720,
   -1720, -1720, -1720, -1720, -1720, -1720, -1720, -1720, -1720, -1720,
   -1720, -1720,  1639, -1720, -1720, -1720, -1720,  1641,  1640,   503,
   -1720, 18172, 18172, -1720, -1720, -1720, -1720, 19836, -1720, -1720,
    1644, -1720,  1568,  1568,  1568,   770,  1614,   374, -1720,  3479,
     387, 15582, -1720, -1720, -1720,  3903, 19836,  4897,   394, -1720,
   -1720,    54,  1642,  1642,  4684, -1720, -1720, 18321, -1720, 19836,
    1647,  1645, -1720, -1720, -1720, -1720,  1039,  1650, 13851,  1511,
    1652, 19836,   311,  1649,   325, 13171, 16952, -1720, -1720, -1720,
     671, 13851, 19836,  1042,   354, -1720, 19836, 18752, -1720, -1720,
     433, -1720,  1134, -1720,  1045,  1048,  1050, -1720, -1720, -1720,
   -1720,   501,   958,  1658, -1720, -1720, 19836, -1720,  1660,   679,
   10561, -1720, -1720, -1720, -1720, 19836,  1699, -1720,  9439, -1720,
     503, 14812, -1720, -1720, 16952, -1720, -1720, -1720, -1720, -1720,
   -1720,  1662, -1720, 18172, -1720, -1720,  1663, -1720,  1665,  1664,
    1654,   722, -1720,  1666, -1720, -1720, -1720, 19836, -1720, 16750,
   19836,  1134,  1672,  1258, -1720,  1280, -1720,  1980, -1720,  1980,
   -1720, -1720, -1720, -1720, 18172,  1674,  1675, -1720, -1720, 18172,
   18172,  1678,  1679,  1282, 14338, 14496, -1720,  1677, -1720, -1720,
   -1720, -1720,  1681,  1684,  1284, -1720, -1720, -1720, -1720,   770,
    2165,   445, -1720, -1720, -1720, -1720,   503,   503, -1720, -1720,
   -1720,   453, -1720,  1051,  3903,   742, -1720,  4897,   503, -1720,
   -1720, -1720, -1720, -1720, -1720, -1720, -1720,   488, 13851,   274,
   19548,  1769, 13851,  1511, 14970, -1720, -1720, -1720, -1720, -1720,
   19836,  1771,  1671,  9511,  1698, 19836, 13851, 10226,  1511,   671,
    1094,  1676, 19836, -1720,  1701,   405, 13851, -1720, -1720,  1702,
   -1720, -1720,  1680,   679,   605,  1704,  1707,  1301,  1765, -1720,
   -1720, -1720, -1720,  4684,  4478, -1720, -1720,  1705,  1706, -1720,
   -1720, -1720,   722,  1314, -1720,  1713, -1720, -1720, -1720,  1716,
   -1720, -1720, -1720,  1308,  1329, -1720, -1720, -1720, -1720, -1720,
   -1720, -1720, -1720, -1720, -1720,  1714, -1720, -1720,  1715,  1717,
   -1720, -1720, -1720,  1718,  1720,  1721,  2165, -1720,   503, -1720,
   -1720, -1720, -1720, -1720,  1727,  3479, -1720, -1720,  7133,    94,
   11374, -1720, 13733, -1720,    37,  1057, 13851,  1794,   352, 13851,
   19836,  1738,   671,  1094,  1722, 20268,  1708,   439,  1822, -1720,
   19836,  1749, 19836, 19836,  1511,  1728, 11535, -1720, -1720, -1720,
   17769, -1720,  1748,  1732,   223, 13851, -1720, 19836, 19188,   662,
   -1720, -1720, -1720,  1756, -1720, -1720,  1314,  1760, -1720, -1720,
   -1720, -1720,  1758,  1761,  1762, 14812,  1763, -1720, -1720,   625,
    1092, -1720, -1720,   770, -1720, -1720,   162, -1720,   321, -1720,
   -1720, -1720,  1767, 12082, -1720, -1720, 13851, -1720,    39, -1720,
   13851, 19836, -1720, -1720, 19836,  1770, 19836, 19836,  1738,  1511,
   19620, 19836, 13851,   581,  1754,   593, -1720, -1720,  1773, 12082,
   17769, -1720,  4257, 17567,   893,  1766, -1720,  1825,  1774,   608,
    1772, -1720,  1857, -1720,  1066, 13851,  1782, 13851, 13851, -1720,
    1785, -1720, -1720, -1720, -1720, -1720, -1720, -1720, -1720,  1214,
   -1720, 19836, -1720, 19836, -1720, -1720,  1415, 12241, -1720, -1720,
   13851, -1720, -1720,  1511,   617,  1776,   618, -1720, -1720,  1511,
    1511, -1720, 19836, 19692, 19836, -1720,  1415, -1720,  1768,  3214,
    3970, -1720, -1720, -1720,   223,  1781, 19836,  1778,   223,   223,
   13851, -1720, -1720, 19836,  1830,  1833, -1720, 18172, -1720, -1720,
   13733, -1720,  1415, -1720, -1720, 19836, 19764, 19836,  1511, -1720,
    1511,  1511, -1720,  1768, 19836,  1792,  3970,  1793,   679,  1797,
   -1720,   697, -1720, -1720,  1071,  1765,   271, -1720, -1720,  9064,
    1802, 13733,  1511, -1720,  1511,  1511,  1806,  1804, -1720,   501,
     679,  1807, -1720,  1783,   679, -1720, -1720, 13851,  1887,  1815,
   -1720, -1720, -1720,  9251, -1720,   501, -1720, -1720,  1340, 19836,
   -1720,  1072, -1720, 13851, -1720, -1720,   679,   893,  1816,  1798,
   -1720, -1720, -1720,  1077, -1720, -1720,  1808,   893, -1720, -1720
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,   440,     0,     2,   440,   457,   458,   459,   460,   461,
     462,   463,   464,   446,   448,   447,   449,     0,     0,     0,
     465,   467,   488,   468,   489,   471,   472,   486,   487,   466,
     484,   485,   469,   470,   473,   474,   475,   476,   477,   478,
     479,   480,   481,   482,   483,   490,   491,   779,   493,   566,
     567,   570,   572,   568,   574,     0,     0,     0,   440,     0,
       0,    16,   537,   543,     9,    10,    11,    12,    13,    14,
      15,   743,    97,     0,    19,     0,     2,    95,    96,    17,
      18,   795,   440,   744,   389,     0,   392,   669,   394,   403,
       0,   393,   423,   424,     0,     0,     0,     0,   520,   442,
     444,   450,   440,   452,   455,   505,   492,   428,   498,   503,
     429,   515,   430,   530,   534,   540,   519,   546,   558,   779,
     563,   564,   547,   614,   395,   396,     3,   745,   758,   445,
       0,     0,   779,   817,   779,     2,   834,   835,   836,   440,
       0,   993,   994,     0,     1,   440,     0,   440,   412,   413,
       0,   520,   434,   435,   436,   748,     0,   569,   571,   573,
     575,     0,   440,     0,   780,   781,   565,   494,   662,   663,
     661,   722,   717,   707,     0,     0,   746,     0,     0,   440,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   538,
     541,   440,   440,     0,   995,   520,   824,   842,   999,   992,
     990,   997,   388,     0,   159,   675,   158,     0,   397,     0,
       0,     0,     0,     0,     0,     0,   387,   894,   895,     0,
       0,   422,   777,   779,   773,   798,   779,   779,   775,     2,
     779,   774,   855,   779,   779,   852,     0,   513,   514,     0,
       0,   440,   440,   457,     2,   440,   404,   443,   453,   506,
       0,   535,     0,   761,     2,     0,   669,   405,   520,   499,
     516,   531,     0,   761,     2,     0,   456,   500,   507,   508,
     517,   522,   532,   536,     0,   550,     0,   737,     2,     2,
     759,   816,   818,   440,     0,     2,     2,  1003,   520,  1006,
     777,   777,     3,     0,   520,     0,     0,   415,   779,   775,
     774,     2,   440,     0,   741,     0,   703,   705,   704,   706,
       0,     0,   699,     0,   689,     0,   698,   709,     0,   779,
     779,     2,   440,  1014,   441,   440,   452,   431,   498,   432,
     523,   433,   530,   527,   548,   779,   549,     0,   650,   440,
     651,   968,   969,   440,   652,   654,   537,   543,     0,   615,
     616,     0,   782,     0,   720,   708,     0,   786,    21,     0,
      20,     0,     0,     0,     0,     0,     0,    23,    25,     4,
       8,     5,     6,     7,     0,     0,   440,     2,     0,    98,
      99,   100,   101,    82,    24,    83,    38,    81,   102,     0,
       0,   117,   119,   123,   126,   129,   134,   137,   139,   141,
     143,   145,   147,   150,     0,    26,     0,   544,     2,   102,
     440,   151,   714,   665,   534,   667,   713,     0,   664,   668,
       0,     0,     0,     0,     0,     0,     0,   796,   822,   779,
     832,   840,   844,   850,     2,  1001,   440,  1004,     2,    95,
     440,     3,   649,     0,  1014,     0,   441,   498,   523,   530,
       3,     3,   631,   635,   645,   651,   652,     2,   825,   843,
     991,     2,     2,    23,     0,     2,   675,    24,     0,   673,
     676,  1012,     0,     0,   682,   671,   670,     0,     0,   763,
       2,     2,     2,     2,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   801,   858,   779,     0,   669,     2,
     797,   805,   921,   799,   800,     0,   761,     2,   854,   862,
       0,   856,   857,     0,   418,   440,   440,   504,   441,     0,
     520,   440,   996,  1000,   998,   521,   741,     0,   761,   777,
     398,   406,   454,     0,   761,     2,   741,     0,   761,   718,
     501,   502,   518,   533,   539,   542,   537,   543,   561,   562,
       0,   719,   440,   659,     0,   196,   381,   440,     3,     0,
     520,   440,   760,   440,     0,   400,     2,   401,   738,   420,
       0,     0,     0,     2,   440,   777,   440,   741,     0,     2,
       0,   702,   701,   700,   695,   451,     0,   693,   710,   496,
       0,     0,   440,   440,   970,   441,   437,   438,   439,   974,
     965,   966,   972,     2,     2,    96,     0,   930,   944,  1014,
     926,   779,   779,   935,   942,   657,   440,   528,   653,   441,
     524,   525,   529,     0,   779,   980,   441,   985,   977,   440,
     982,     0,  1012,   621,     0,     0,     0,   440,     0,   794,
     793,   789,   791,   792,   790,     0,   784,   787,     0,    22,
     440,    89,   440,   440,    84,   440,    91,     0,    32,     0,
      33,   440,    87,    88,   440,   440,   440,     2,    98,    99,
       0,     0,   177,     0,     0,   564,     0,   990,     0,     0,
       0,     0,     0,     0,     0,     0,    50,     0,    56,    57,
      61,     0,     0,    61,     0,    85,    86,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     440,   160,   161,   162,   163,   164,   165,   166,   167,   168,
     169,   170,   158,     0,   156,   157,     2,   906,   666,   903,
     779,   779,   911,   545,   440,   823,   779,   833,   841,   845,
     851,     2,   826,   828,   830,     2,   846,   848,     0,  1002,
    1005,   440,     0,     0,     2,    96,   930,   779,  1014,   876,
     779,   779,  1014,   779,   891,   779,   779,     3,   653,     0,
       0,  1014,  1014,   440,   440,     0,     2,   684,     0,  1012,
     681,  1013,     0,   677,     0,     2,   680,   683,   174,   173,
       0,     2,   440,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   779,   810,   814,   853,   779,   867,
     872,   802,   859,     0,     0,   426,   918,     0,   764,     0,
     440,   765,   419,     0,     0,     0,     0,   417,     2,   766,
       0,   402,   741,     0,   761,     2,   767,     0,     0,     0,
       0,   576,   638,   441,     3,     3,   642,   641,   837,     0,
       0,   440,   382,     0,   520,     3,    95,     3,   440,     0,
       3,   742,     2,   697,   440,   440,   691,   690,   691,   497,
     495,   615,     0,   976,   440,   981,   441,   440,   967,   440,
       0,     0,     0,   945,     0,   779,  1015,   931,   932,   658,
     928,   929,   943,   971,   975,   973,   526,   561,     0,   979,
     984,   618,  1013,     0,     0,   617,     0,  1012,   723,   721,
       0,     0,   786,    61,   747,     0,     2,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   440,     0,   116,   115,
       0,   112,   111,    27,     0,    28,     0,     0,     0,     0,
       3,    61,     0,    46,     0,    47,    54,     0,    53,    65,
       0,    62,    63,    66,    49,     0,    48,    52,     0,     0,
      45,   118,   120,   121,   122,   124,   125,   127,   128,   132,
     133,   130,   131,   135,   136,   138,   140,   142,   144,   146,
       0,     0,   391,     0,     0,    29,     3,   675,   152,   440,
       0,     0,     0,   907,   908,   904,   905,   716,   715,     2,
     827,   829,   831,     2,   847,   849,   440,   440,   923,   922,
       2,     0,     0,     0,     0,     0,   779,   931,   879,   896,
       2,   874,   882,   655,   877,   878,   656,     2,   889,   899,
     892,   893,     0,     3,  1014,   410,     2,  1007,     2,   646,
     647,   625,     3,     3,     3,     3,   669,     0,   150,     0,
       3,     3,     0,   678,     0,   672,     0,   779,     0,   440,
       3,   414,   416,     0,   779,   811,   815,   779,   868,   873,
       2,   803,   806,   808,     2,   860,   863,   865,   777,     0,
     919,     3,   769,     3,   510,   509,   512,   511,     2,   742,
     770,     2,   768,     0,   742,   771,   576,   576,   576,   440,
       0,     0,   660,     0,   385,     0,     0,   440,     0,     2,
       0,     0,     0,     0,     0,   179,     0,   315,   316,     0,
       0,   354,   353,     0,   154,   154,   360,   537,   543,   193,
       0,   180,     0,   204,   181,   182,   440,   198,   183,   184,
     185,   186,     0,   187,   188,   321,     0,   189,   190,   191,
       0,   192,   200,   520,   440,     0,   202,     0,   379,     0,
       0,     0,     3,     0,   749,   742,   730,   731,     0,     3,
     726,     3,     3,     0,   440,   707,   707,  1012,   978,   983,
       2,    95,   440,     3,   535,     3,   441,     3,   779,   938,
     941,   440,     3,   927,   933,     0,   779,   779,     0,   621,
     605,   675,   622,  1012,     0,     2,   783,   785,     0,    90,
     440,     0,    94,    92,   440,     0,   106,     0,     0,     0,
     110,   114,   113,   178,     0,     0,     0,   675,   103,   171,
       0,     0,    41,    42,    79,     0,    79,    79,     0,    67,
      69,    44,     0,     0,    40,     0,    43,   149,     0,     0,
       0,     0,  1012,     3,   779,   914,   917,   909,   440,   440,
       3,     3,     0,   779,   885,   888,   779,     0,   779,   779,
     880,   897,   440,   440,  1008,     0,   648,   440,   440,     0,
       0,     0,     0,   399,     3,     0,     0,     0,     0,   674,
     679,     3,   762,   176,   175,     3,     0,     0,     2,   804,
     807,   809,     2,   861,   864,   866,   440,   440,   669,   779,
       0,     0,     0,   742,   772,     0,   440,   440,   440,   440,
     440,   440,   559,   587,     3,     3,   588,   520,   577,     0,
       0,   819,     2,     0,   383,    61,     0,     0,   306,   307,
     201,   203,     0,     0,     0,   440,   440,   302,     0,   300,
       0,     0,     0,   675,     0,     0,     0,     0,     0,   155,
       0,     0,   361,     0,     0,     3,   208,     0,   199,     0,
     297,     0,     0,     2,     0,   520,   779,     0,   380,   925,
     924,     0,     2,     0,   733,     2,   728,     0,   729,     0,
     711,   692,   696,   694,     0,   440,     0,     0,     0,     3,
       0,     2,   934,   936,   937,     0,     0,    95,     0,     3,
    1012,   611,     0,   621,   619,  1012,     0,   608,   724,   440,
     788,     0,     0,     0,    34,     0,   107,   109,   108,   105,
     104,   675,  1012,     0,    60,    76,     0,    70,    77,    78,
      55,     0,     0,     0,    64,    51,     0,   148,   390,    30,
       0,     0,     2,   910,   912,   913,     3,     3,     0,     0,
     779,     2,   881,   883,   884,     2,   898,   900,     0,   875,
     890,     3,     3,  1009,     3,   633,   632,   636,  1011,     2,
       2,  1010,     0,     3,   776,   685,   686,     0,     0,   779,
     421,   440,   440,     3,     3,   427,   778,     0,   869,   753,
       0,   755,   559,   559,   559,   594,   564,     0,   600,   588,
       0,   440,   551,   586,   582,     0,     0,     0,     0,   589,
     591,   779,   602,   602,     0,   583,   598,   440,   386,     0,
       0,    62,   310,   311,   308,   309,     0,     0,     2,   219,
       0,     0,   221,   394,   220,   520,   440,   288,   287,   289,
       0,     2,   179,   257,     0,   252,     0,   179,   303,   301,
       0,   295,  1012,   304,     0,     0,     0,   342,   343,   344,
     345,     0,   335,     0,   336,   312,     0,   313,     0,     0,
     440,   210,   197,   299,   298,     0,   333,   352,     0,   384,
     779,   440,   751,   712,   440,     2,     2,   609,   986,   987,
     988,     0,   939,   440,     3,     3,     0,   947,     0,     0,
       0,     0,   620,     0,   607,     3,    93,     0,    31,   440,
       0,  1012,     0,     0,    80,     0,    68,     0,    74,     0,
      72,    39,   153,   915,   440,     0,     0,   820,   838,   440,
     440,     0,     0,     0,   440,   440,   688,     0,   407,   409,
       3,     3,     0,     0,     0,   757,   555,   557,   553,     0,
     954,     0,   595,   959,   597,   951,   779,   779,   581,   601,
     585,     0,   584,     0,     0,     0,   604,     0,   779,   578,
     592,   603,   593,   599,   640,   644,   643,     0,     2,     0,
       0,   240,     2,   222,   520,   293,   291,   294,   290,   292,
       0,   248,     0,   266,   292,     0,     2,   440,   258,     0,
     276,     0,     0,   296,     0,     0,     2,   319,   346,     0,
     337,     2,     0,     0,     0,     0,   324,     0,   320,   195,
     194,   408,   727,     0,     0,   989,     3,     0,     0,   946,
     948,   610,     0,  1012,   623,     2,    37,    35,    36,     0,
      58,   172,    71,     0,     0,     3,   821,   839,     3,     3,
     886,   901,   411,     2,   630,     3,   629,   687,     0,     0,
     812,   870,   920,     0,     0,     0,   955,   956,   779,   580,
     952,   953,   579,   560,     0,     0,   209,   318,     0,     0,
       0,   233,     2,   211,     0,     0,     2,   242,   260,     2,
     179,   285,     0,   267,     0,     0,   261,   259,   250,   253,
       0,   292,     0,   179,   277,     0,     0,   214,   317,     2,
     440,   314,     0,     0,   362,     2,   322,     0,    61,     0,
     334,   732,   734,     0,   949,   950,  1012,     0,   725,    59,
      75,    73,     0,     0,     0,   440,     0,   813,   871,   779,
     962,   964,   957,     0,   590,   228,   223,   226,     0,   225,
     232,   231,     0,   440,   235,   234,     2,   244,     0,   241,
       2,     0,   249,   254,     0,   292,     0,   179,   286,   268,
       0,     0,     2,   279,   280,   278,   256,   305,     0,   440,
     440,     3,   347,   441,   351,     0,   355,     0,     0,     0,
     363,   364,   217,   325,     0,     2,     0,     2,     2,   940,
       0,   613,   916,   887,   902,   634,     2,   958,   960,   961,
     596,     0,   230,     0,   229,   213,   236,   440,   375,   245,
       2,   246,   243,   263,   270,   271,   269,   255,   265,   264,
     262,   251,     0,     0,     0,   216,   236,     3,   340,     0,
     954,   348,   349,   350,   362,     0,     0,     0,   362,     0,
       2,   323,   330,     0,   327,   329,   612,   440,   224,   227,
       2,     3,   237,   376,   247,     0,     0,     0,   282,   284,
     283,   281,     3,   340,     0,     0,   955,     0,     0,     0,
     356,     0,   365,   218,     0,   320,     0,     3,   205,     0,
       0,     2,   273,   275,   274,   272,     0,     0,   341,     0,
     368,     0,   366,     0,   368,   326,   328,     2,     0,     0,
     207,   206,   212,     0,   215,     0,   338,   369,     0,     0,
     357,     0,   331,     2,   963,   339,     0,     0,     0,     0,
     332,   370,   371,     0,   367,   358,     0,     0,   359,   372
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1720,  5340,  5219, -1720,    -1,   249,  1138,  -146, -1720,  1590,
   -1720,   342, -1720,  -660,   631,   726,  -927, -1054, -1720,   252,
    6153,  1888, -1720,  1331, -1720,  1311,   555,   802,   824,   595,
     796,  1278,  1279,  1281,  1291,  1288, -1720,   165,  -176,  8064,
     858, -1720,  1600, -1720, -1720,  -668,  5995, -1069,  1536, -1720,
     130, -1720,   850,   -13, -1720, -1720, -1720,   416,    78, -1720,
   -1646, -1575,   282,    56, -1720, -1720, -1720,   296, -1493, -1478,
   -1281, -1720, -1720, -1720, -1720,     8, -1719,   178, -1720, -1720,
      14, -1720, -1720, -1720,    27,   440,   447,   121, -1720, -1720,
   -1720, -1720,  -734, -1720,    55,     7, -1720,   131, -1720,  -147,
   -1720, -1720, -1720,   867,  -680,  -926, -1314, -1720,    12, -1182,
     124,  5689,  -853,  -788, -1720,  -279, -1720,    33,  -157,   788,
    -263,  -235,  3540,  6449,  -628, -1720,    63,   960,   620,  2041,
   -1720,  1988, -1720,   145,  3548,  -277, -1720, -1720,   126, -1720,
   -1720,  2425,   287,  4298,  2637,   -23,  1787,  -296, -1720, -1720,
   -1720, -1720, -1720,  -247,  1113,  4055, -1720,  -358,   167, -1720,
     521,   246, -1720,   180,   714, -1720,   514,    -3, -1720, -1720,
   -1720,  4698,  -599, -1105,  -561,  -415,   -31,  -637, -1720, -1239,
    -153,   290,  1304,   889,  4623,  -204,  -471,  -243,  -193,  -451,
    1261, -1720,  1575,   569,  1179,  1473, -1720, -1720, -1720, -1720,
     349,  -162,   -27,  -858, -1720,   169, -1720, -1720,   624,   456,
   -1720, -1720, -1720,  2064,  -708,  -417,  -867,   -32, -1720, -1720,
   -1720, -1720, -1720, -1720,    17,  -750,  -149, -1667,  -185,  7605,
     -66,  5999, -1720,  1144, -1720,  2911,  -212,  -199,  -197,  -192,
      20,   -61,   -49,   -48,   308,     6,    79,    81,  -187,   -72,
    -159,  -144,  -126,  -724,  -694,  -678,  -648,  -703,  -113,  -629,
   -1720, -1720,  -697,  1335,  1337,  1338,  2089,  6986,  -560,  -552,
    -550,  -526,  -702, -1720, -1527, -1639, -1623, -1608,  -577,  -127,
    -281, -1720, -1720,   185,    65,   -51, -1720,  7380,   870,  -540,
    -580
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,  1140,   213,   383,   384,    80,    81,   385,   360,   386,
    1433,  1434,   387,   960,   961,   962,  1248,  1249,  1250,  1445,
     409,   389,   390,   391,   670,   671,   392,   393,   394,   395,
     396,   397,   398,   399,   400,   401,   402,   411,  1059,   672,
    1370,   733,   207,   735,   405,   800,  1141,  1142,  1143,  1144,
    1145,  1146,  1147,  2009,  1148,  1149,  1375,  1550,  1867,  1868,
    1801,  1802,  1803,  1981,  1982,  1150,  1564,  1565,  1566,  1709,
    1710,  1151,  1152,  1153,  1154,  1155,  1156,  1383,  1737,  1918,
    1840,  1157,  1158,  1582,  1995,  1583,  1584,  1901,  1159,  1160,
    1161,  1373,  1909,  1910,  1911,  2038,  2053,  1936,  1937,   284,
     285,   861,   862,  1113,    83,    84,    85,    86,    87,    88,
     442,    90,    91,    92,    93,    94,   221,   559,   444,   413,
     445,    97,   294,    99,   100,   101,   325,   326,   104,   105,
     166,   106,   880,   327,   152,   109,   241,   110,   153,   250,
     329,   330,   331,   154,   406,   115,   116,   333,   117,   550,
     850,   848,   849,  1522,   334,   335,   120,   121,  1109,  1338,
    1528,  1529,  1671,  1672,  1339,  1517,  1690,  1530,   122,   634,
    1620,   336,   632,   915,  1052,   450,   451,   854,   855,   452,
     453,   856,   338,   554,  1165,   415,   416,   208,   470,   471,
     472,   473,   474,   313,  1185,   314,   878,   876,   584,   315,
     354,   316,   317,   417,   124,   172,   173,   125,  1179,  1180,
    1181,  1182,     2,  1098,  1099,   576,  1174,   126,   304,   305,
     252,   262,   533,   127,   211,   128,   222,  1061,   841,   500,
     164,   129,   645,   646,   647,   130,   224,   225,   226,   227,
     299,   132,   133,   134,   135,   136,   137,   138,   230,   300,
     232,   233,   234,   768,   769,   770,   771,   772,   235,   774,
     775,   776,   738,   739,   740,   741,   501,   139,   609,   610,
     611,   612,   613,   614,  1674,  1675,  1676,  1677,   599,   455,
     341,   342,   343,   418,   199,   141,   142,   143,   345,   792,
     615
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      79,   297,   404,    79,   558,   323,   517,   181,   494,   337,
     949,   355,   183,   530,   476,   789,   148,  1183,   675,  1839,
     486,   131,   487,   231,   184,   185,   176,   488,   617,   899,
    1783,   359,   489,   969,    95,   340,   917,   498,  1253,   904,
    1004,  1552,  1028,   942,   834,   836,  1784,   898,  1485,  1486,
     351,   890,   190,   891,    79,    79,  1364,    79,   627,   594,
     490,  1785,   630,  1029,   102,  1032,   140,  1260,    57,   140,
    1022,  1039,    79,  -735,  1722,   491,   625,   892,   131,   186,
     628,    79,   506,   279,   494,  1714,  1023,   514,   289,    79,
     660,    95,   913,   492,    79,  1870,   486,    79,   487,   144,
    1542,    79,   196,   488,  1424,   565,   567,   528,   489,   838,
    1876,  1166,  1940,   292,   256,   228,  1024,   538,   253,   845,
     421,   102,   263,   140,    57,    89,  1586,   111,   149,   209,
    1294,  1869,   422,   423,   715,  1025,   490,  1104,    57,    79,
     437,  1295,    79,  1787,    79,   497,   107,   162,   495,    79,
     617,   491,   187,   484,   188,    79,  1875,   209,   183,  -736,
     872,  -377,    79,  -378,  1175,   131,   919,   140,   279,   492,
     184,   185,   676,  1553,  1553,  1296,   716,   594,    95,    79,
      79,  1162,    89,  1333,   111,  1544,   660,   424,  1033,    57,
     155,  1447,  1036,  1093,    79,   600,   156,   210,   566,  -761,
     196,  1049,  1050,   107,   467,  1587,   898,   499,   102,    79,
     140,  1877,   890,  1941,   891,   458,   161,   278,    79,    79,
    1815,   499,   246,  1055,   495,   186,   257,  -761,   260,   570,
      19,  -377,  1322,  -378,   183,    79,  1256,  1325,   892,  1343,
     196,  1070,  1821,  1252,    79,   505,   184,   185,   510,  1062,
    1871,   545,  1197,   146,    79,   825,  1334,    79,  1344,  1862,
     425,   280,   426,  1218,    79,   196,  1869,   198,   507,    89,
     527,   111,   499,  1008,    79,    79,  1552,    79,   534,   793,
     537,   821,  1715,  1187,   865,   807,  1839,   808,   112,   588,
     107,  1241,   809,  1931,    79,    79,  1588,   810,   187,   617,
     188,   175,    79,  1280,   170,   170,  1267,  1172,  1393,    79,
      79,   885,  1232,   600,    79,   177,   196,  1213,  1622,  1032,
    1783,  1335,  1932,   617,  1281,   811,  1022,  1296,   588,   883,
     617,   773,   198,   967,  1875,  1885,  1784,  1204,   358,   170,
     812,   479,  1023,   403,  2027,   112,   561,    79,   910,  1798,
    1799,  1785,    79,   903,  1452,    79,   644,   178,   813,  1130,
     821,  1980,  1263,  1589,   832,  1875,   909,  1351,  1053,  1053,
     837,   807,  1024,   808,   274,  1962,  1907,  1214,   809,  1270,
    1271,  1980,   531,   810,   179,   760,  1453,  1053,   844,   170,
     197,  1272,   170,  1323,  1534,   914,  1166,  1636,  1638,  1640,
    1333,  1333,  1333,   229,   191,   170,   254,  2011,  1553,   111,
     264,   811,   349,  1535,   562,  1485,  1486,   323,    57,    79,
    1236,   822,   179,   421,    -3,  1101,   812,  1237,   107,  -663,
     523,  1800,   112,  1787,   539,   422,   423,  1350,  1820,  1822,
     458,   190,    79,    79,   813,   551,   925,   340,   927,   928,
     278,   929,   427,  1053,    79,    79,  1510,   931,    57,   170,
     933,   934,   935,    79,  1286,   467,  1162,  1204,   890,  1862,
     891,    57,  1343,  1334,  1334,  1334,  1933,  1934,  1346,  1347,
    1798,  1799,   454,    79,   204,   202,   581,   523,   197,   600,
     424,  1599,    79,  1712,   892,   205,  1413,   573,  1721,   216,
     822,   499,   566,   459,   170,  1716,   674,   944,  1305,   421,
    1717,   206,    79,   458,   170,   582,   583,  1881,    79,  1635,
     236,   422,   423,   829,   548,   170,   606,   553,   197,  1679,
     157,  1884,  1886,   158,   159,  1553,   160,   751,  1335,  1335,
    1335,   499,  1534,   870,   871,   840,  1262,  -434,  1680,  1688,
    1009,   843,   170,   197,   499,   847,    79,   617,    79,   170,
     170,  1682,  1826,   425,   170,   426,   535,   274,  1689,    79,
     112,    79,   993,    57,  1477,    79,   279,   243,     6,     7,
       8,     9,    10,    11,    12,    79,    57,  1456,   944,    79,
     617,   179,   458,  1348,   944,   198,   131,   170,  1063,  -895,
    1788,  1043,   170,  1189,  1891,   170,  1420,  1723,  1688,    95,
      13,    14,    15,    16,    17,   904,  1925,   276,    57,  1789,
      57,  1188,    79,  1345,    62,    63,   561,  1792,    57,   585,
     179,  1466,  1467,   586,    79,   828,    57,    57,  -556,   102,
     831,   140,   278,   944,  1814,  1481,  1482,  1404,    13,    14,
      15,    16,    17,   773,  1053,  1072,   887,   839,   279,   937,
      57,    57,  1796,  1683,  -377,  1030,    57,   846,    57,   604,
     938,   939,    75,  1426,  1088,  1551,  1567,   293,  1089,  1503,
    1504,  1444,    79,    57,    79,  1540,    79,   260,  1252,   111,
      79,  1553,   170,    79,  -750,    57,   545,  1037,  1575,  1080,
      89,   604,   111,   499,   170,   170,    57,  1084,   107,   682,
    1763,   499,  1764,  1401,   683,  1308,  1312,  1553,    79,   499,
     499,   107,  1460,   266,  -425,   705,   706,   267,   459,  -438,
     270,   631,   272,   189,    63,  1915,   944,   684,  1212,  1411,
    1462,  1883,   685,   604,   499,  1471,  1952,  -425,   944,   499,
     353,  1382,  1054,  1054,  1896,  1553,  1833,   311,  1954,  1967,
    1425,  1834,  1475,    79,  1968,    79,   604,  -439,  1916,   707,
     708,  1054,   944,   944,  1926,  1633,   356,    79,   499,   323,
     674,   357,  1985,  1987,    79,   674,  1442,  -435,    72,    96,
     467,   674,   150,    79,   358,  1207,    13,    14,    15,    16,
      17,   459,    79,    79,    79,   859,   146,   279,   736,   340,
     674,   454,   499,  1293,  1705,  1706,  1707,   717,  1947,    77,
      78,   718,    79,    61,   887,    72,   168,   169,    64,    65,
      66,    67,    68,    69,    70,   170,  1708,  1054,   428,   881,
      13,    14,    15,    16,    17,   603,    96,   457,  2023,   604,
     112,   794,   795,  2024,    57,   796,    77,   605,    79,    79,
     467,  1103,   429,   112,  1660,  1661,   698,   430,   266,   606,
     194,  1300,   907,   699,   700,   743,   431,    72,  -436,   744,
    1619,    79,   432,   454,   170,  1623,  1176,    13,    14,    15,
      16,    17,   755,   427,    95,   499,   499,   603,    57,    72,
     617,   604,  1632,  1318,   433,   870,   461,    79,    77,    78,
    1551,    79,  1572,  1279,   773,    79,   278,   462,   427,  1669,
     499,   644,   636,   499,  1164,   638,   140,   287,   475,   903,
      77,    78,   953,    96,   955,   194,   958,   858,   477,   140,
     966,   859,   403,   970,  1208,    57,   266,   267,   480,   621,
     481,   272,   201,  1058,   157,   482,    79,   158,   159,  1092,
     160,   103,   237,   238,    79,   239,  1415,   918,   995,   240,
    1100,   586,  1516,  1102,   920,   483,  1746,  1105,   586,   435,
     236,   496,   507,   497,   817,    89,   499,   111,   921,   526,
    1631,   943,   922,    79,    61,   944,   467,   515,  1177,    64,
      65,    66,    67,    68,    69,    70,   107,  1765,   589,   274,
     454,  1013,  1768,  1769,   516,   499,   209,   201,   103,    79,
    1435,   536,  1753,   355,   355,    79,    79,   170,   577,  1394,
     519,  1067,  1724,   522,   170,  1068,    61,  1567,  1054,   168,
     169,    64,    65,    66,    67,    68,    69,    70,   555,   419,
     592,   454,  1071,   573,  1073,   427,    79,   499,   248,   624,
    1030,   637,   427,   323,   604,   600,  1577,  1578,  1579,  1580,
    1581,    96,  -894,   454,   454,  1505,  1554,  -606,    61,   403,
     635,   217,   218,    64,    65,    66,    67,    68,    69,    70,
     522,  1759,   454,   340,   648,   243,     6,     7,     8,     9,
      10,    11,    12,   278,   649,   103,    72,   499,  1112,   170,
     170,   652,   507,   653,   118,   524,   499,   118,   467,  1493,
    1494,    79,    79,    79,  1532,  1094,  1525,    74,   678,   944,
    1096,   170,   573,  1526,   944,  1487,   499,    77,    78,  -116,
    -116,  -116,  -116,  -116,  -116,   467,   701,   702,   112,   657,
      95,    79,   266,  1846,   265,  1257,  1251,  1206,   454,    79,
    1252,   170,    79,    79,  1400,   170,    79,   697,   744,  1430,
    1628,   118,   524,  1252,  1629,   711,   256,    79,  1914,    95,
    1164,   712,   140,   253,   263,  1705,  1706,  1707,   713,   870,
    1699,   714,   602,  1938,   944,   118,  1725,   944,   194,  1726,
     944,  1727,  1793,  1068,    79,   944,   744,  1708,  1878,  1164,
     467,   140,   944,  1847,   745,   118,  1713,  1971,   532,  1938,
      79,  1252,  2025,  2049,   759,   639,   944,  2046,  2056,   140,
    1997,  1058,  2057,  1324,  2001,   719,   467,  1705,  1706,  1707,
     746,    89,    -3,   111,    79,   434,  1349,   747,   323,   944,
     544,    63,   118,   972,   973,   974,  1484,  1983,   118,  1708,
     118,   748,   107,  1368,  1533,  1666,  1667,  1668,  -180,   749,
      89,   750,   111,  1326,  1327,  1328,    79,   -17,   340,   790,
     201,   703,   704,   777,   248,   191,   678,   246,   257,   791,
     260,   107,   118,  -437,  1340,   709,   710,   592,   678,   640,
     979,   980,   981,   982,   118,   801,  1920,  1532,   946,   947,
     602,  1554,   944,  1258,   641,   494,   814,   642,   643,    64,
      65,    66,    67,    68,    69,    70,   486,   815,   487,  1045,
    1046,  1047,  1048,   488,   816,    79,  1239,  1068,   489,    79,
    2007,   148,    79,   468,   818,   150,  1254,  1255,  -151,  -151,
     819,    96,  1047,  1392,   820,   118,  1450,  1451,   118,   454,
    1455,  1451,   467,   118,    96,   824,   490,   286,   170,  1459,
    1451,   170,   170,   170,   870,  -115,  -115,  -115,  -115,  -115,
    -115,   491,   467,   826,    79,   419,   419,    13,    14,    15,
      16,    17,   941,   842,   140,   170,   118,  1019,  1443,   492,
     860,   170,  1495,  1443,   112,   534,   248,  1019,  1507,   553,
    1641,  1068,  1761,  1068,  -554,   118,   170,  -552,  1556,  1556,
     140,   140,   851,  1457,   873,  1435,    13,    14,    15,    16,
      17,  1231,   875,   112,  1762,  1451,  1772,  1773,  1782,   944,
     467,  1176,   879,  1734,   323,    79,   882,  1533,  1487,   893,
      79,    79,    79,   149,   170,   495,  1837,  1838,   606,  1684,
     403,   403,  1850,  1451,    13,    14,    15,    16,    17,   912,
    1431,   254,   264,   895,   340,   821,  1543,  1545,   914,    89,
      89,   111,   111,  1851,  1451,   807,   905,   808,   923,   118,
    1798,  1799,   809,   916,   140,  2046,  2047,   810,  1448,  1449,
     107,   107,  1487,   975,   976,   983,   984,   419,   924,   531,
     945,  1340,  1340,  1340,  1597,  1518,  1340,   951,    79,   948,
     248,   992,    57,   118,    79,   811,    79,   977,   978,  1691,
    1691,    18,  1018,    79,  1402,  1403,   103,   997,  1019,  1026,
     812,  1065,  1074,  1075,  -739,  1076,  1077,   467,  1078,   118,
    1079,  1095,  1097,  1177,  -639,  1106,  1167,  1168,   813,   686,
     467,   687,   688,   689,  1107,  1108,  1210,  1902,   256,    47,
      48,    49,    50,    51,    52,    53,    54,   454,   454,   532,
    1198,    72,  1184,  1532,  1199,  1200,  1216,  1832,  1215,   170,
     690,  1219,   170,   691,   692,  1842,  1242,   467,   693,   694,
    1221,   603,   787,  1224,   468,   604,  1222,   617,  1223,  1225,
    1227,  1228,    77,   605,   419,   822,  1176,  1229,  1234,  1235,
      79,   140,  1866,  1775,  1243,  1259,  1264,  1265,   118,   118,
    1266,  1273,   170,  1274,  1275,  1276,    79,  1902,    79,  1284,
    -627,  -626,   112,   112,  1299,  1307,  1319,  -740,  1341,    96,
    1372,  1352,  1355,  1556,  1356,   140,  1342,  1365,  1366,  1367,
    -662,  1374,  1178,  1382,  1427,  1376,   944,  1388,  1386,   140,
     118,  1389,  1390,  1428,   118,  1470,   118,  1441,  1443,   246,
     257,  1396,   260,    79,  1483,  1488,    79,  1398,  1908,   118,
    1489,  1491,  1490,   535,  1451,   654,  1496,   467,    61,  1499,
    1458,   467,  1487,    64,    65,    66,    67,    68,    69,    70,
    1508,  1728,  1509,  1511,    89,   467,   111,  1521,  1523,  1524,
     695,   696,  1345,  1533,  1547,   467,  1568,  1569,  1177,  1571,
    1573,  1585,  1590,  1592,  1593,   107,  1594,  1595,  1600,  1602,
    1603,   695,    79,    79,  1607,   419,  1277,    74,   494,  1605,
     118,    79,  1608,  1961,  1606,  1978,  1609,  1866,  1610,  1612,
     486,  1617,   487,   118,  1624,   118,   118,   488,   118,  1621,
    1626,   695,   489,  1627,   118,  1634,  1630,   118,   118,   118,
    1556,  1642,   140,   170,   821,  1643,  1647,  1648,  1678,   427,
    1999,  1658,  1665,  1656,    79,  1495,  1698,   170,  1526,  1700,
     490,   467,  1252,  1702,   210,   467,  1738,  1731,   467,  1733,
     170,  1752,  1751,   248,  1754,   491,  1745,  1749,  1908,  1750,
    1760,   103,  1908,  1908,   532,  1766,  1767,  1695,   531,  1770,
    1771,  1777,  1780,   492,   467,  1781,    13,    14,    15,    16,
      17,    89,  1806,   111,  1809,  1810,   248,   170,  1816,  1825,
    1823,  1829,  2021,  1130,  1831,  1835,   857,   118,  1836,  1844,
    1845,  1848,   107,  2048,  1849,  -628,  1857,  1880,  1858,  1859,
     170,  1860,  1861,  1890,  2037,   467,    61,   112,  2037,   467,
     499,    64,    65,    66,    67,    68,    69,    70,   495,    82,
    -537,   467,   147,   570,    57,  1892,  1887,  1336,   183,  1894,
    2051,    79,  1897,    79,  1905,    96,  1906,  1919,  1921,  1922,
     184,   185,  1923,  1924,   467,  1935,   467,   467,  1773,  1953,
    1945,  1955,  1964,  1966,   822,    74,  1965,  1969,   786,   468,
    1970,  1973,   787,  1976,    96,  1998,  1556,  2005,   140,   467,
    2006,  1986,  1994,  2019,   454,   454,    82,   170,  2022,  2020,
    2032,   170,  2000,    72,  2034,  2035,  2039,  2040,    79,    79,
    2043,   180,  1556,   403,   140,   170,  2044,  2054,   681,   467,
      82,  1757,  2055,   736,   118,   170,  1541,   499,  1454,   467,
     196,   940,  2058,   220,    77,    78,   245,   118,   118,   985,
      82,   986,   170,  1371,   987,    79,  1378,    89,  2033,   111,
    1556,   170,   140,   989,   112,   988,  1735,  1827,   467,   734,
     467,  1979,  1992,  1819,  2028,  1904,   458,  1917,   107,  2026,
    2017,  1957,  1729,    89,  2002,   111,   467,   147,   971,  1730,
    1956,  2041,   467,    82,  1387,   147,   167,   525,   296,   302,
    1681,  1864,   467,  1930,   107,  1520,    79,  1692,   797,  1384,
     322,   170,  1064,  1625,   419,   170,    79,  1186,   170,   877,
    1742,    89,   905,   111,     3,     0,  1217,   410,   180,   180,
       0,  1000,     0,  1001,  1002,     0,     0,   103,     0,   147,
     440,    61,   107,   245,   170,  1904,    64,    65,    66,    67,
      68,    69,    70,  1244,     0,  1247,   403,  1245,   403,  1246,
       0,     0,     0,  1247,     0,     0,   103,   220,   220,     0,
       0,     0,     0,     0,  1336,  1336,  1336,   150,  1515,  1519,
       0,     0,     0,   248,   296,   170,     0,     0,     0,   170,
      74,   403,  1247,    82,   857,   468,     0,     0,     0,   249,
       0,   170,     0,    96,    96,   454,   245,     0,     0,  2036,
     269,     0,     0,  1963,    18,     0,   532,     0,     0,  2018,
     112,     0,     0,    61,   170,  2045,   170,   170,    64,    65,
      66,    67,    68,    69,    70,   956,   302,    13,    14,    15,
      16,    17,   302,   296,   296,     0,   112,     0,     0,   170,
     147,     0,   249,   857,     0,  1247,    51,    52,    53,    54,
       0,     0,     0,     0,   403,     0,     0,     0,     0,     0,
     322,   607,   616,     0,     0,   957,     0,  1178,     0,   170,
       0,     0,   118,     0,   112,     0,     0,   322,     0,   170,
     118,   322,     0,     0,     0,    57,   249,     0,    61,     0,
       0,     0,     0,    64,    65,    66,    67,    68,    69,    70,
     964,     0,     0,     0,     0,     0,     0,     0,   170,   118,
     170,     0,     0,  1226,   410,     0,     0,     0,  1230,     0,
       0,     0,     0,     0,     0,     0,   170,   118,     0,  1238,
       0,     0,   170,     0,     0,     0,     0,     0,     0,     0,
     965,     0,   170,     0,    72,     0,  2052,   118,   410,   249,
       0,   737,     0,   857,     0,     0,  2059,     0,   180,   306,
     307,   308,   309,     0,  1669,   103,   103,     0,   499,   509,
     857,   857,     0,     0,   147,    77,    78,     0,   440,   249,
       0,     0,   766,     0,   616,   249,     0,   118,     0,     0,
       0,     0,     0,     0,     0,   532,     0,    61,     0,   468,
     168,   169,    64,    65,    66,    67,    68,    69,    70,     0,
       0,     0,     0,     0,     0,   249,     0,     0,     0,     0,
       0,     0,   220,     0,     0,   468,     0,     0,    96,     0,
       0,   220,     0,  1247,     0,     0,     0,     0,     0,     0,
    1379,   419,  1178,     0,     0,     0,    57,     0,     0,   310,
       0,   296,     0,   410,   410,     0,     0,   296,     0,   322,
       0,     0,     0,     0,     0,    61,     0,   311,   168,   169,
      64,    65,    66,    67,    68,    69,    70,    61,     0,     0,
     217,   218,    64,    65,    66,    67,    68,    69,    70,   118,
     118,   118,   118,   118,   118,     0,    61,   296,   654,   168,
     169,    64,    65,    66,    67,    68,    69,    70,   296,     0,
     296,     0,   322,     0,    82,     0,     0,     0,   118,   118,
       0,     0,     0,     0,     0,  1277,    74,     0,     0,     0,
     322,   440,     0,   616,     0,     0,     0,   249,  1380,     0,
       0,   607,     0,     0,     0,   607,     0,     0,     0,     0,
       0,   468,   742,     0,   322,    96,   243,     6,     7,     8,
       9,    10,    11,    12,   616,   248,     0,   322,   753,  1357,
       0,   756,     0,   695,    61,   147,     0,   168,   169,    64,
      65,    66,    67,    68,    69,    70,     0,     0,   410,     0,
     147,   147,   118,   410,     0,     0,     0,     0,     0,   410,
     103,     0,   147,   147,   147,     0,     0,     0,  1436,  1437,
    1438,   249,     0,     0,     0,  1439,  1440,     0,     0,     0,
       0,     0,   857,   857,     0,     0,     0,     0,   509,   468,
       0,   249,     0,     0,  1247,     0,   857,   857,     0,  1247,
    1247,  1247,     0,     0,     0,     0,     0,  1359,     0,     0,
       0,   249,     0,     0,     0,     0,    57,     0,   440,     0,
       0,     0,     0,     0,     0,     0,     0,   448,     0,     0,
     857,   857,     0,     0,   737,   737,     0,     0,     0,     0,
       0,     0,   410,     0,   118,     0,   249,    61,   114,     0,
       0,   114,    64,    65,    66,    67,    68,    69,    70,   440,
       0,     0,   766,     0,   766,  1354,     0,     0,     0,     0,
     249,    96,     0,     0,   532,    72,     0,   249,     0,   118,
       0,   322,   322,     0,     0,     0,     0,   103,     0,     0,
       0,     0,     0,     0,     0,    73,    74,    96,     0,     0,
     322,     0,   296,     0,     0,   114,    77,    78,     0,     0,
     249,   269,     0,   118,     0,     0,     0,     0,     0,     0,
       0,   296,     0,     0,     0,     0,     0,   118,     0,   114,
       0,     0,     0,    61,     0,    96,   168,   169,    64,    65,
      66,    67,    68,    69,    70,   251,     0,     0,     0,   114,
       0,     0,   118,     0,     0,     0,     0,   597,     0,   410,
     620,     0,     0,     0,     0,     0,   322,     0,     0,     0,
       0,     0,   147,   410,   597,     0,     0,     0,   597,     0,
       0,     0,   322,     0,  1192,  1247,   114,  1247,     0,   579,
       0,     0,   114,     0,   114,   607,    61,     0,   251,   168,
     169,    64,    65,    66,    67,    68,    69,    70,   318,   114,
     350,     0,     0,    61,     0,   857,   857,     0,    64,    65,
      66,    67,    68,    69,    70,     0,   414,     0,     0,     0,
       0,     0,     0,     0,   440,   742,   742,     0,   114,   414,
     118,    72,   251,   103,     0,  1011,     0,     0,  1014,     0,
       0,  1696,     0,     0,     0,  1361,     0,     0,     0,     0,
       0,  1020,    74,     0,     0,   604,     0,     0,     0,   103,
       0,     0,    77,    78,     0,   597,     0,     0,     0,     0,
       0,    13,    14,    15,    16,    17,     0,     0,     0,   114,
       0,     0,   114,     0,     0,     0,     0,     0,     0,     0,
       0,   737,     0,     0,   249,   251,     0,   103,     0,   509,
       0,     0,     0,  1082,     0,   249,     0,  1086,   766,     0,
       0,     0,   549,     0,     0,   766,     0,   857,     0,  1596,
     114,     0,     0,     0,     0,   251,  1736,   249,     0,    57,
       0,   251,     0,     0,     0,     0,     0,     0,     0,   114,
       0,     0,     0,     0,     0,     0,   448,     0,   857,     0,
       0,     0,     0,   857,   857,     0,     0,   322,     0,   114,
      61,   251,   114,   217,   218,    64,    65,    66,    67,    68,
      69,    70,     0,     0,     0,     0,   114,     0,     0,     0,
     114,     0,     0,     0,   182,     0,   118,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,   147,     0,   448,
       0,     0,     0,     0,     0,   410,   223,     0,   219,    74,
       0,     0,   118,   414,     0,     0,     0,   597,   448,    77,
      78,     0,     0,     0,    13,    14,    15,    16,    17,     0,
       0,     0,     0,     0,   410,     0,     0,     0,     0,     0,
       0,   597,     0,     0,     0,     0,     0,   414,     0,     0,
     118,   245,    82,     0,   597,     0,     0,     0,     0,     0,
       0,   298,     0,     0,     0,     0,   296,     0,     0,     0,
       0,     0,   147,   114,     0,     0,     0,   414,     0,     0,
     440,     0,    57,   251,  1701,     0,     0,     0,     0,     0,
       0,     0,   742,     0,     0,    61,     0,  1711,   346,   347,
      64,    65,    66,    67,    68,    69,    70,     0,   440,     0,
       0,     0,   147,    61,     0,     0,   217,   218,    64,    65,
      66,    67,    68,    69,    70,     0,     0,     0,     0,     0,
     485,   223,     0,     0,  1740,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,   448,    75,   298,     0,     0,
       0,   348,   414,   414,     0,     0,     0,   251,   114,     0,
       0,   764,    74,  1310,     0,   604,  1314,     0,  1913,     0,
       0,     0,    77,   765,    61,   322,   322,   189,    63,    64,
      65,    66,    67,    68,    69,    70,   448,     0,     0,   114,
       0,     0,     0,     0,   114,     0,     0,   251,   114,     0,
     114,     0,     0,     0,   249,     0,   571,   298,     0,     0,
       0,   114,     0,   114,   147,   147,   147,   147,   147,   147,
       0,     0,     0,    74,  1527,   302,   786,   350,     0,   114,
     414,     0,   251,     0,  1797,     0,     0,   249,  1807,     0,
       0,     0,     0,   410,   410,     0,     0,     0,     0,     0,
       0,     0,  1818,   114,     0,     0,   251,     0,     0,     0,
     549,     0,  1828,   251,     0,     0,   114,     0,   911,     0,
       0,     0,     0,   245,   114,     0,     0,     0,     0,     0,
       0,   857,     0,     0,    57,     0,     0,   414,     0,   114,
     114,     0,   414,   440,     0,     0,     0,     0,   414,     0,
       0,   114,   114,   114,     0,     0,     0,     0,     0,   597,
       0,     0,   620,     0,     0,    61,     0,   147,   217,   218,
      64,    65,    66,    67,    68,    69,    70,   721,   722,   723,
     724,   725,   726,   727,   728,   729,   730,   731,  1874,     0,
       0,   204,  1879,    72,     0,  1882,     0,     0,     0,     0,
       0,     0,     0,  1464,     0,   767,     0,   414,     0,     0,
       0,   448,  1473,  1959,    74,     0,     0,   499,   732,     0,
       0,  1912,     0,     0,    77,    78,     0,     0,   249,     0,
       0,   414,    61,     0,     0,   217,   218,    64,    65,    66,
      67,    68,    69,    70,     0,   806,     0,     0,   414,     0,
       0,     0,     0,  1670,   223,     0,     0,  1527,     0,   410,
       0,     0,  1939,  1527,     0,  1527,  1942,     0,     0,     0,
     114,   114,     0,     0,   298,     0,   249,     0,  1951,     0,
     298,     0,     0,    13,    14,    15,    16,    17,  1205,   114,
       0,     0,     0,   302,   147,     0,     0,     0,     0,     0,
      61,  1972,     0,  1974,  1975,    64,    65,    66,    67,    68,
      69,    70,  1244,     0,     0,     0,  1245,   114,  1246,     0,
     298,     0,     0,     0,     0,     0,  1984,     0,   410,     0,
       0,   869,     0,   298,     0,     0,     0,     0,     0,   322,
     251,    57,   147,     0,     0,     0,     0,     0,   414,    74,
       0,   251,  1446,     0,     0,   114,  2003,     0,     0,     0,
       0,   114,   414,     0,     0,     0,  2008,   147,     0,     0,
       0,   114,    61,  1194,   414,     0,   114,    64,    65,    66,
      67,    68,    69,    70,     0,     0,     0,     0,     0,     0,
       0,    98,   322,   322,   151,  2031,     0,  2008,     0,   108,
      72,     0,     0,     0,     0,     0,     0,  1670,  1670,     0,
       0,     0,     0,  2042,     0,     0,     0,     0,     0,  2031,
      73,    74,  1527,   414,     0,  1527,     0,     0,     0,  2050,
      61,    77,    78,   217,   218,    64,    65,    66,    67,    68,
      69,    70,   302,     0,     0,     0,   249,     0,    98,     0,
       0,     0,     0,     0,  1673,   410,   108,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,   597,     0,     0,
       0,    57,   195,     0,     0,     0,     0,     0,  1525,    74,
       0,     0,   296,     0,     0,  1526,   114,     0,     0,    77,
      78,     0,   258,     0,     0,   448,     0,     0,     0,     0,
     259,     0,    61,   114,   114,   217,   218,    64,    65,    66,
      67,    68,    69,    70,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1670,  1021,     0,   767,     0,   288,
      72,     0,     0,  1527,     0,    98,     0,     0,    57,     0,
       0,     0,     0,   108,     0,     0,     0,     0,     0,     0,
     219,    74,   324,     0,     0,     0,   114,     0,     0,     0,
     328,    77,    78,     0,     0,   298,     0,     0,   147,    61,
     420,     0,   217,   218,    64,    65,    66,    67,    68,    69,
      70,   288,   446,     0,   298,     0,     0,     0,     0,     0,
     447,     0,     0,   322,     0,   249,   114,    72,     0,     0,
       0,  1670,     0,     0,   414,     0,     0,     0,  1673,  1673,
     493,   147,     0,     0,     0,     0,     0,   295,    74,     0,
       0,     0,     0,     0,     0,     0,   513,     0,    77,    78,
       0,   518,   520,   414,     0,   195,     0,   147,   147,     0,
    1960,   302,     0,     0,     0,     0,     0,     0,     0,     0,
     251,   114,     0,     0,     0,     0,     0,   540,     0,     0,
     542,     0,   543,     0,     0,   541,     0,     0,     0,     0,
       0,   114,     0,   560,     0,   147,     0,     0,     0,   414,
     448,   108,     0,  1194,     0,     0,   572,     0,     0,     0,
       0,     0,     0,     0,     0,  1423,     0,  1960,  1960,     0,
       0,     0,     0,     0,     0,     0,     0,   414,     0,     0,
       0,   114,   595,     0,     0,   619,     0,     0,     0,     0,
     596,     0,     0,   259,     0,  1673,     0,     0,     0,   626,
       0,     0,     0,   626,  1960,     0,     0,   596,     0,    61,
       0,   596,   544,    63,    64,    65,    66,    67,    68,    69,
      70,     0,     0,     0,     0,   114,   114,   659,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   114,
     114,     0,     0,     0,   114,   114,     0,     0,     0,     0,
       0,  1021,     0,     0,     0,     0,     0,  1278,   767,     0,
       0,   994,     0,     0,   249,     0,     0,     0,  1928,     0,
       0,     0,  1673,   114,   114,     0,     0,     0,     0,     0,
       0,     0,     0,   114,   114,   114,   114,   114,   114,     0,
       0,     0,     0,    57,   251,     0,   288,     0,     0,     0,
     595,     0,    13,    14,    15,    16,    17,     0,   596,     0,
       0,  1673,   414,   414,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   659,    61,     0,     0,   217,   218,    64,
      65,    66,    67,    68,    69,    70,     0,     0,    61,     0,
       0,     0,   251,    64,    65,    66,    67,    68,    69,    70,
    1244,     0,    72,     0,  1245,     0,  1246,     0,     0,     0,
      57,     0,   414,     0,     0,     0,     0,     0,  1673,  1673,
       0,     0,  1525,    74,     0,     0,   119,     0,     0,   119,
       0,   446,     0,    77,    78,     0,   114,    74,     0,   447,
    1637,    61,     0,     0,   217,   218,    64,    65,    66,    67,
      68,    69,    70,     0,     0,  1673,     0,     0,     0,   298,
       0,     0,   853,     0,     0,     0,     0,   520,     0,    72,
     328,   864,     0,   560,     0,     0,     0,     0,     0,   259,
       0,   108,     0,   119,   324,     0,    98,     0,     0,  1959,
      74,     0,   447,   499,   108,    13,    14,    15,    16,    17,
      77,    78,   626,   886,     0,     0,     0,   119,   114,   114,
     596,   447,     0,     0,     0,     0,     0,   897,     0,     0,
       0,     0,     0,     0,     0,     0,   595,   119,   414,     0,
       0,   906,     0,     0,   596,     0,     0,     0,     0,   626,
       0,     0,     0,     0,   114,     0,     0,   596,     0,     0,
       0,     0,     0,    57,     0,     0,     0,     0,  1478,     0,
       0,     0,   251,   114,   119,     0,     0,     0,     0,   366,
     119,   367,   119,   368,    62,    63,    64,    65,    66,    67,
      68,    69,    70,     0,    61,     0,     0,   217,   218,    64,
      65,    66,    67,    68,    69,    70,     0,   414,     0,     0,
       0,     0,     0,     0,   119,     0,     0,     0,   114,     0,
       0,   114,    72,     0,     0,     0,   119,  1531,     0,   680,
     114,     0,    75,   377,     0,   597,     0,     0,     0,     0,
     446,     0,   295,    74,     0,     0,   114,     0,   447,     0,
       0,     0,     0,    77,    78,     0,     0,  1003,     0,     0,
       0,   114,     0,     0,     0,     0,   114,   114,     0,     0,
       0,   114,   114,     0,     0,     0,     0,   119,     0,   113,
     119,   886,     0,     0,     0,   119,  1027,     0,    61,   447,
       0,   546,   547,    64,    65,    66,    67,    68,    69,    70,
       0,     0,     0,   446,   446,   597,     0,     0,     0,     0,
       0,   328,   328,     0,     0,     0,     0,     0,   119,     0,
       0,   251,   446,     0,     0,     0,     0,     0,     0,     0,
     328,     0,     0,     0,   414,     0,   113,   119,    61,    75,
       0,   217,   218,    64,    65,    66,    67,    68,    69,    70,
     853,     0,     0,     0,     0,     0,     0,     0,   328,     0,
       0,     0,     0,     0,     0,     0,    72,     0,    61,     0,
       0,    62,    63,    64,    65,    66,    67,    68,    69,    70,
     261,  1163,     0,     0,     0,     0,  1959,    74,   446,   108,
     499,     0,     0,     0,   151,     0,   328,    77,    78,     0,
       0,     0,     0,     0,   626,     0,     0,  1196,     0,   853,
    1531,   119,   596,     0,  1202,   259,  1685,   328,  1531,    75,
       0,     0,     0,   113,     0,    61,     0,     0,   217,   218,
      64,    65,    66,    67,    68,    69,    70,     0,     0,     0,
     332,     0,     0,     0,     0,   119,     0,   114,     0,     0,
       0,     0,     0,    72,     0,     0,   324,     0,     0,     0,
       0,     0,     0,     0,   447,     0,     0,     0,     0,     0,
     449,   119,   114,   764,    74,     0,     0,   604,     0,     0,
       0,     0,     0,     0,    77,   765,     0,     0,     0,     0,
     114,     0,     0,     0,     0,     0,     0,   606,    61,     0,
       0,   217,   218,    64,    65,    66,    67,    68,    69,    70,
       0,     0,     0,     0,     0,     0,   114,   114,     0,   853,
     251,     0,     0,     0,     0,     0,    72,   328,     0,     0,
       0,     0,     0,     0,     0,     0,   853,   853,     0,     0,
       0,     0,     0,     0,   328,   328,   219,    74,     0,     0,
     119,   119,     0,     0,   114,     0,     0,    77,    78,    61,
       0,   113,   217,   218,    64,    65,    66,    67,    68,    69,
      70,     0,     0,     0,     0,  1794,     0,     0,  1531,     0,
       0,     0,     0,     0,     0,     0,     0,    72,     0,   446,
       0,     0,   119,     0,   114,     0,   119,   328,   119,     0,
     598,     0,     0,   261,     0,     0,     0,   295,    74,     0,
       0,   119,     0,     0,     0,     0,     0,   598,    77,    78,
       0,   598,     0,     0,     0,     0,     0,     0,     0,  1337,
       0,     0,     0,     0,     0,   298,     0,  1163,     0,     0,
       0,     0,     0,    61,     0,   108,   168,   169,    64,    65,
      66,    67,    68,    69,    70,     0,     0,     0,   171,   174,
       0,     0,     0,     0,     0,     0,  1163,     0,     0,     0,
       0,     0,   119,     0,   108,     0,     0,     0,     0,   123,
       0,     0,   123,     0,  1385,   119,  1531,   119,   119,     0,
     119,   457,   259,   212,     0,     0,   119,     0,    61,   119,
     119,   119,     0,    64,    65,    66,    67,    68,    69,    70,
    1244,     0,   595,     0,  1245,     0,  1246,     0,   598,    61,
     596,   518,   168,   169,    64,    65,    66,    67,    68,    69,
      70,     0,     0,     0,     0,     0,   123,     0,     0,     0,
     324,     0,     0,   290,     0,     0,   291,    74,   447,     0,
    1639,     0,     0,     0,     0,     0,     0,     0,     0,   312,
     123,     0,     0,     0,     0,    61,     0,   461,   168,   169,
      64,    65,    66,    67,    68,    69,    70,     0,     0,   119,
     123,     0,     0,     0,     0,     0,     0,     0,   853,   853,
       0,     0,     0,   298,     0,     0,   328,   328,     0,   449,
       0,     0,   853,   853,     0,     0,     0,   446,   446,     0,
     328,   328,     0,   478,     0,   328,   328,   123,     0,     0,
       0,     0,     0,   123,     0,   123,     0,     0,     0,     0,
     332,     0,     0,     0,     0,     0,   853,   853,     0,   261,
       0,   113,     0,     0,   328,   328,  1337,  1337,  1337,   151,
     571,   298,   449,     0,   113,     0,     0,   123,   529,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   171,   123,
     598,   449,     0,     0,     0,  1555,  1555,     0,     0,   171,
       0,     0,     0,   108,   108,     0,     0,   298,     0,    13,
      14,    15,    16,    17,   598,     0,   119,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   575,   598,     0,   119,
     119,     0,     0,   578,   580,     0,     0,     0,   587,     0,
     123,     0,     0,   123,     0,   324,     0,     0,   123,     0,
       0,     0,     0,   447,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    57,     0,   151,
       0,   633,     0,     0,     0,     0,   312,     0,     0,   312,
       0,   123,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    61,     0,
     123,   217,   218,    64,    65,    66,    67,    68,    69,    70,
       0,     0,     0,     0,     0,     0,     0,     0,   449,     0,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   853,   853,     0,     0,     0,  1525,    74,     0,   328,
     328,     0,     0,     0,     0,     0,     0,    77,    78,   449,
       0,     0,     0,     0,     0,     0,   212,  1687,     0,     0,
       0,     0,     0,     0,   123,     0,     0,   853,   781,   782,
       0,   332,   332,     0,     0,   328,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1704,     0,     0,     0,
     332,     0,     0,     0,   259,     0,     0,     0,   123,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   332,     0,
    1555,     0,     0,     0,   123,     0,     0,     0,   108,     0,
       0,   324,     0,     0,   151,     0,     0,     0,     0,   328,
       0,     0,     0,   853,     0,     0,     0,     0,     0,   113,
       0,   328,     0,     0,   119,     0,   332,     0,     0,     0,
       0,     0,   119,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   598,     0,   853,   261,     0,   332,     0,   853,
     853,     0,   328,     0,   446,   446,     0,   328,   328,     0,
       0,   119,   328,   328,     0,     0,     0,     0,     0,   312,
    1786,     0,     0,   123,   123,     0,     0,     0,     0,   119,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   449,     0,     0,     0,     0,   119,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   123,     0,  1555,   633,   123,
       0,   123,     0,     0,     0,   108,     0,     0,     0,     0,
       0,     0,     0,     0,   123,     0,     0,     0,     0,   119,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   332,     0,     0,
       0,     0,     0,     0,     0,   203,     0,     0,     0,     0,
       0,   214,   215,     0,   332,   332,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   123,     0,     0,     0,     0,
       1,     0,     0,   145,     0,   277,     0,     0,   123,     0,
     123,   123,     0,   123,     0,     0,     0,     0,     0,   123,
       0,     0,   123,   123,   123,     0,     0,   332,     0,     0,
    1903,     0,     0,     0,     0,     0,     0,     0,   596,     0,
       0,   119,   119,   119,   119,   119,   119,     0,     0,     0,
       0,     0,     0,     0,     0,   446,     0,     0,     0,     0,
       0,  1044,     0,   328,     0,     0,     0,     0,  1056,     0,
     119,   119,     0,  1555,     0,   113,   192,     0,     0,     0,
       0,   108,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1555,
    1903,     0,   123,     0,   113,     0,     0,   108,   596,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   261,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   283,     0,  1555,     0,     0,
       0,     0,     0,  1114,   119,   108,     0,     0,     0,     0,
     598,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1996,     0,     0,     0,     0,   633,     0,     0,     0,     0,
       0,   568,     0,     0,     0,     0,     0,   853,   449,     0,
       0,     0,     0,     0,     0,   328,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1209,     0,     0,     0,   633,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   123,
       0,     0,     0,     0,     0,     0,   332,   332,     0,   283,
       0,     0,   123,   123,     0,     0,   119,     0,     0,     0,
     332,   332,     0,     0,   521,   332,   332,     0,     0,     0,
       0,     0,     0,     0,   283,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   283,     0,     0,     0,     0,     0,
       0,   119,     0,     0,   332,   332,     0,     0,   552,   556,
       0,     0,     0,     0,     0,   563,   564,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   574,     0,     0,     0,   119,     0,     0,     0,     0,
       0,     0,     0,   113,   113,     0,     0,     0,   762,   119,
     763,   593,     0,     0,     0,     0,     0,     0,     0,   779,
     780,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   119,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   449,     0,     0,     0,     0,     0,     0,
       0,    13,    14,    15,    16,    17,     0,   679,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,  1358,  1360,  1362,   720,    45,
      46,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    57,
       0,   193,   119,     0,   758,  1381,     0,   863,   761,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1114,     0,     0,     0,     0,     0,     0,   783,     0,   332,
     332,   784,   785,     0,     0,   788,     0,   123,     0,     0,
       0,     0,     0,     0,     0,   123,     0,     0,     0,     0,
     802,   803,   804,   805,     0,     0,     0,     0,   633,     0,
       0,     0,     0,     0,     0,   332,   193,     0,     0,   827,
       0,     0,     0,     0,   123,     0,     0,   830,     0,     0,
       0,   193,     0,     0,   261,     0,     0,     0,     0,     0,
       0,     0,   123,     0,     0,     0,     0,     0,   193,     0,
       0,     0,     0,     0,     0,   283,     0,     0,     0,     0,
       0,   443,   123,     0,     0,     0,     0,     0,   113,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   332,
       0,     0,     0,     0,     0,     0,   868,     0,     0,     0,
       0,   332,     0,   552,     0,     0,     0,     0,     0,   874,
       0,     0,   123,     0,     0,     0,     0,     0,   119,     0,
       0,     0,     0,     0,   193,     0,     0,     0,     0,     0,
       0,     0,   332,   889,   894,     0,     0,   332,   332,     0,
       0,     0,   332,   332,   119,     0,     0,     0,     0,     0,
       0,     0,     0,  1536,     0,     0,  1538,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   193,   119,     0,     0,     0,  1042,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   936,     0,     0,
       0,   193,     0,     0,     0,   113,     0,     0,     0,     0,
       0,     0,     0,     0,   123,   123,   123,   123,   123,   123,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   165,     0,     0,     0,
       0,     0,     0,   123,   123,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   165,  1110,  1111,     0,   999,     0,     0,     0,
       0,     0,     0,     0,  1169,  1170,  1171,     0,     0,  1173,
       0,  1016,     0,     0,     0,  1017,     0,     0,     0,   193,
       0,     0,     0,     0,   889,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   165,     0,
       0,     0,     0,     0,     0,     0,  1057,   123,   598,   193,
       0,   165,     0,   165,     0,  1066,     0,     0,     0,     0,
       0,  1069,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   332,     0,     0,     0,  1693,     0,     0,
       0,     0,     0,   352,     0,     0,     0,     0,     0,  1240,
       0,   113,     0,     0,     0,     0,     0,     0,     1,     0,
     352,     0,     0,     0,     0,     1,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   113,   598,     0,
       0,     0,     0,     0,   193,   193,     0,     0,     0,     0,
     443,     0,     1,     0,     0,  1261,     0,     0,   165,   123,
       0,     0,   165,     0,     0,   165,   165,     0,     0,   165,
       0,     0,   165,   165,     0,   113,     0,     0,     0,     0,
       0,     0,     0,     0,   633,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   123,     0,     0,     0,     0,     0,
       0,     0,  1285,   193,     0,     0,  1220,     0,     0,     0,
       0,  1289,  1290,  1291,  1292,   332,     0,     0,     0,  1297,
    1298,     0,   443,     0,     0,     0,     0,     0,   123,  1306,
       0,     0,     0,     0,   165,     0,     0,   165,     0,     0,
       0,     0,   123,     0,     0,   193,     0,     0,     0,     0,
    1320,     0,  1321,     0,     0,     0,     0,     0,   165,   165,
       0,     0,     0,     0,     0,     0,   193,   123,     0,     0,
       0,   388,     0,     0,   165,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1268,
       0,     0,     0,  1269,     0,     0,     0,     0,     0,     0,
     889,     0,     0,     0,     0,  1377,  1841,     0,     0,     0,
    1282,   673,     0,     0,     0,   633,     0,  1283,     0,     0,
       0,     0,     0,     0,     0,     0,  1287,     0,  1288,     0,
       0,  1391,     0,     0,     0,     0,     0,     0,  1395,     0,
    1397,  1399,     0,     0,     0,     0,     0,     0,     0,   443,
    1406,     0,  1407,     0,  1408,   123,  1410,     0,     0,     0,
    1316,  1418,     0,     0,  1317,     0,     0,     0,   165,     0,
       0,     0,     0,   193,     0,     0,     0,     0,   145,     0,
       0,     1,     0,     0,     0,     0,     0,     0,     0,     0,
     443,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   443,   443,     0,     0,     0,     0,     0,     0,
       0,     0,  1461,   352,     0,     0,     0,     0,     0,  1468,
    1469,   443,     0,     0,     0,   165,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     833,   835,     0,  1492,   651,     0,     0,   388,   656,     0,
    1497,     0,     0,     0,  1498,     0,     0,   662,   663,     0,
    1405,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   388,   388,     0,     0,     0,   247,     0,     0,
       0,     0,     0,     0,   214,  1429,     0,   443,   268,     0,
     271,     0,   273,   388,   193,     0,     0,     0,     0,   352,
       0,   123,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   388,  1591,     0,     0,   123,     0,     0,
     247,     0,   271,   273,     0,     0,     0,     0,     0,     0,
     165,   165,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   165,     0,   193,     0,     0,  1611,     0,
       0,     0,     0,     0,     0,   123,  1616,     0,  1618,     0,
       0,     0,     0,     0,   247,   673,     0,     0,  1501,     0,
     673,     0,  1502,     0,     0,     0,   673,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   673,     0,     0,     0,     0,
       0,     0,  1537,     0,     0,  1645,  1646,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1651,  1652,     0,  1653,     0,     0,     0,   247,     0,   271,
     273,   991,  1657,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1662,  1663,     0,     0,     0,     0,     0,     0,
       0,     0,  1601,     0,     0,  1604,     0,   247,     0,   165,
     165,     0,     0,   247,     0,   165,     0,     0,     0,     0,
       0,  1613,     0,     0,     0,     0,     0,     0,   443,     0,
       0,     0,     0,     0,     0,     0,   165,     0,     0,   165,
     165,     0,   165,   247,   165,   165,     0,     0,     0,   622,
       0,   273,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1644,     0,     0,     0,     0,     0,     0,     0,
       0,  1649,     0,   165,     0,  1650,     0,   165,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1654,
    1655,     0,     0,  1747,  1748,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1755,     0,     0,     0,     0,     0,
     388,   388,   388,   388,   388,   388,   388,   388,   388,   388,
     388,   388,   388,   388,   388,   388,   388,   388,   388,   247,
       0,     0,     0,   193,     0,     0,     0,     0,     0,  1778,
    1779,   193,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   165,   247,     0,   622,   273,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   193,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     388,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   247,     0,     0,  1743,  1744,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   247,     0,     0,  1843,     0,   247,     0,   247,
       0,     0,     0,     0,     0,     0,   443,   443,     0,     0,
       0,     0,     0,     0,  1852,     0,     0,  1853,  1854,   247,
       0,   247,   247,     0,  1856,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   247,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   247,     0,     0,     0,   165,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   247,     0,   622,   273,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   165,   388,   247,   622,
       0,  1830,     0,   165,     0,   247,   165,     0,     0,     0,
       0,     0,     0,     0,     0,   388,     0,     0,     0,     0,
     388,     0,     0,     0,   193,  1604,     0,     0,     0,     0,
       0,   388,     0,     0,     0,     0,     0,     0,   247,   268,
       0,     0,     0,  1855,     0,     0,     0,     0,     0,     0,
    1958,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1865,     0,     0,     0,     0,     0,
    1873,     0,     0,   388,     0,     0,     0,     0,   339,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1899,     0,     0,  1900,
       0,     0,     0,     0,     0,     0,  1993,   436,   339,     0,
       0,   361,     0,     0,     0,   362,     0,   363,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   165,     0,     0,
    2010,     0,     0,     0,   364,   165,   165,     0,     0,   502,
     193,  2016,     0,     0,     0,     0,   502,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  2029,     0,     0,     0,
       0,   365,   366,     0,   367,     0,   368,    62,    63,    64,
      65,    66,    67,    68,    69,    70,   369,   370,   358,     0,
     371,   372,   373,     0,   374,   375,     0,     0,     0,     0,
       0,     0,    72,   165,     0,     0,  1977,     0,     0,     0,
     388,     0,   165,     0,     0,   165,     0,   165,   165,     0,
       0,     0,   376,     0,   502,    75,   377,     0,     0,     0,
     193,     0,   378,    77,    78,   379,   380,   381,   382,     0,
       0,     0,   247,     0,     0,     0,     0,     0,   339,   608,
       0,     0,     0,   247,     0,     0,     0,     0,   165,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   629,
       0,     0,     0,     0,     0,   247,     0,     0,     0,     0,
       0,     0,     0,   443,   443,   388,   247,  1546,     0,     0,
    1549,  1563,     0,     0,     0,   247,  1570,     0,     0,     0,
    1574,     0,  1576,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     388,   388,   388,     0,     0,   165,     0,   388,   388,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   502,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   388,     0,     0,     0,   502,   754,     0,   502,   757,
       0,     0,     0,     0,     0,     0,   339,     0,     0,     0,
     608,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   388,   388,
       0,     0,   247,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   200,     0,     0,     0,     0,     0,     0,   165,
       0,   502,     0,     0,     0,   502,   247,     0,   255,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   165,     0,
       0,     0,  1664,     0,     0,     0,     0,   339,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   165,     0,     0,   200,     0,     0,
     165,   303,     0,     0,  1697,     0,     0,     0,     0,     0,
       0,     0,   344,     0,   443,     0,  1703,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   502,     0,   200,
     339,  1718,  1720,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   456,     0,     0,   460,     0,     0,   884,   339,
       0,     0,     0,     0,     0,  1549,     0,     0,     0,   608,
       0,     0,     0,   608,     0,     0,     0,     0,     0,   165,
     902,     0,   339,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   247,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   200,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   255,     0,
       0,     0,     0,     0,     0,   247,     0,     0,     0,     0,
       0,   247,   163,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   460,   165,   165,     0,     0,     0,
       0,     0,   200,   352,     0,     0,     0,   165,     0,     0,
       0,     0,     0,     0,     0,  1805,     0,     0,     0,     0,
       0,     0,   601,     0,   618,  1808,   339,     0,  1813,     0,
    1817,     0,  1563,     0,     0,     0,     0,  1824,     0,     0,
       0,     0,   502,   502,   275,     0,     0,     0,     0,     0,
       0,     0,   502,  1012,     0,   502,  1015,   281,     0,   282,
       0,     0,     0,     0,     0,     0,     0,   339,   388,     0,
     608,     0,   608,   608,     0,     0,   677,     0,     0,   608,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   339,
     339,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   247,   165,   339,     0,
     200,     0,   502,     0,     0,     0,   502,     0,     0,     0,
     502,  1083,     0,     0,   502,  1087,     0,     0,     0,     0,
    1889,     0,  1090,     0,     0,  1893,     0,  1895,     0,     0,
     601,     0,     0,     0,     0,     0,   778,     0,     0,     0,
       0,   503,   504,     0,   247,   508,     0,     0,   511,   512,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   339,   502,     0,     0,   165,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1943,     0,     0,  1944,
       0,  1946,     0,   608,     0,  1949,  1950,     0,     0,     0,
       0,     0,     0,     0,     0,   200,   200,     0,     0,     0,
       0,   456,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   339,     0,   590,   591,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     623,     0,     0,     0,     0,     0,     0,  1988,  1990,  1991,
       0,   388,     0,     0,   344,     0,     0,     0,   165,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  2004,     0,
       0,     0,     0,   456,     0,   888,     0,     0,     0,     0,
    2012,  2014,  2015,     0,     0,     0,     0,     0,     0,   502,
     388,     0,     0,     0,     0,     0,   601,     0,     0,     0,
       0,     0,     0,     0,   247,     0,   608,   608,     0,     0,
       0,     0,     0,   608,     0,     0,     0,   200,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     677,     0,   677,   677,   752,   677,     0,     0,     0,     0,
       0,   677,     0,     0,   677,   677,   677,     0,     0,     0,
       0,     0,     0,     0,     0,   339,     0,     0,     0,     0,
     502,  1311,     0,   502,  1315,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   388,     0,   388,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     456,   823,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   388,
       0,     0,     0,     0,   200,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   247,     0,     0,     0,
       0,   456,     0,     0,     0,     0,     0,   388,     0,     0,
       0,     0,     0,   247,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   456,   456,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   339,     0,
       0,     0,   456,     0,   608,  1414,     0,     0,     0,     0,
       0,     0,   388,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   339,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   900,   901,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   908,
       0,     0,     0,     0,     0,   247,     0,     0,     0,     0,
       0,     0,     0,   412,     0,     0,     0,     0,   456,     0,
     502,  1465,     0,     0,     0,   200,   441,     0,     0,   502,
    1474,     0,   608,     0,     0,     0,   778,     0,     0,   469,
       0,   469,     0,   339,   339,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1201,     0,     0,     0,     0,     0,     0,    13,    14,    15,
      16,    17,     0,     0,     0,     0,   344,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   361,     0,  1005,  1006,   362,     0,   363,
       0,  1010,   247,     0,     0,     0,     0,   569,     0,     0,
       0,     0,     0,     0,     0,    57,   364,     0,     0,     0,
       0,     0,  1031,     0,     0,  1034,  1035,     0,  1038,     0,
    1040,  1041,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   339,     0,   365,   366,     0,   367,     0,   368,    62,
      63,    64,    65,    66,    67,    68,    69,    70,   369,   370,
     358,     0,   371,   372,   373,     0,   374,   375,     0,  1081,
       0,     0,     0,  1085,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   376,   247,     0,    75,   377,   456,
       0,     0,     0,     0,   378,   439,    78,   379,   380,   381,
     382,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   677,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1203,   502,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   502,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     469,     0,     0,     0,     0,     0,   469,     0,     0,     0,
       0,   799,     0,   255,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   200,     0,     0,     0,     0,     0,
       0,     0,   601,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   339,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     344,     0,     0,     0,   677,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   867,     0,
       0,  1203,     0,     0,     0,     0,     0,     0,     0,     0,
     339,   339,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   502,   502,   441,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   456,   456,     0,
     896,   502,  1302,     0,     0,     0,     0,     0,     0,  1309,
       0,     0,  1313,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   677,   677,   677,     0,
     677,   677,     0,     0,     0,     0,     0,   460,     0,     0,
       0,   930,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   799,   950,     0,     0,   952,     0,   954,     0,
       0,     0,     0,     0,   963,     0,   968,   963,     0,     0,
       0,     0,     0,     0,     0,   255,     0,     0,     0,     0,
       0,     0,   502,     0,     0,     0,     0,     0,     0,     0,
     502,     0,     0,     0,   996,   344,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   998,     0,     0,
       0,     0,     0,  1412,     0,     0,     0,     0,  1007,     0,
       0,  1421,  1422,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   441,     0,     0,   996,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   339,     0,     0,     0,   502,  1929,     0,     0,   502,
       0,     0,  1060,     0,     0,   469,   361,     0,     0,     0,
     362,     0,   363,     0,     0,     0,     0,     0,     0,  1463,
       0,     0,     0,     0,     0,     0,     0,     0,  1472,   364,
       0,  1476,     0,  1479,  1480,     0,     0,     0,   502,     0,
       0,  1091,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   200,     0,     0,     0,     0,   365,   366,     0,   367,
       0,   368,    62,    63,    64,    65,    66,    67,    68,    69,
      70,   369,   370,   358,  1506,   371,   372,   373,     0,   374,
     375,     0,     0,     0,     0,   255,     0,    72,     0,   412,
       0,     0,     0,     0,     0,   502,   502,     0,     0,     0,
    1193,  1195,     0,     0,     0,     0,     0,   376,   441,     0,
      75,   377,     0,     0,     0,   466,     0,   378,    77,    78,
     379,   380,   381,   382,     0,     0,     0,     0,     0,     0,
       0,   344,   502,     0,     0,     0,     0,   963,     0,     0,
       0,  1598,     0,     0,     0,     0,     0,     0,     0,     0,
     996,     0,     0,     0,     0,     0,     0,     0,  1233,   677,
       0,     0,     0,     0,     0,   963,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   456,   456,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   469,     0,     0,     0,  2030,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1476,     0,     0,     0,     0,
       0,  1353,     0,     0,   255,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1659,     0,     0,     0,     0,     0,
       0,     0,   361,     0,     0,     0,   362,     0,   363,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   469,     0,
    1301,     0,  1304,  1116,     0,   364,    -2,     0,  1118,  -238,
    -238,  1119,  1120,  1121,  1122,  1123,  1124,  1125,  1126,  1127,
    1128,  1129,  1130,  -320,  1131,  1132,  1133,  1134,  1135,     0,
    1136,     0,   365,   366,     0,   463,     0,   368,  1137,  1138,
      64,    65,    66,    67,    68,    69,    70,   369,   370,   358,
    1139,   371,   372,   373,     0,   374,   375,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,  1369,  1369,
       0,     0,     0,     0,     0,  1741,     0,     0,     0,     0,
     677,     0,  -238,   376,     0,     0,    75,   377,     0,     0,
       0,   279,     0,   378,    77,    78,   379,   380,   381,   382,
       0,     0,     0,     0,     0,   456,     0,     0,  -179,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  2030,     0,     0,     0,     0,     0,     0,     0,
    1409,     0,     0,     0,     0,     0,  1419,     0,  1353,     0,
       0,     0,     0,     0,     0,   469,     0,     0,     0,     0,
     677,  1790,  1791,   460,   441,     0,     0,     0,     0,     0,
       0,     0,     0,  1795,     0,     0,     0,     0,     0,   361,
       0,   469,     0,   362,     0,   363,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   963,     0,     0,   799,
    1116,     0,   364,    -2,     0,  1118,  -239,  -239,  1119,  1120,
    1121,  1122,  1123,  1124,  1125,  1126,  1127,  1128,  1129,  1130,
    -320,  1131,  1132,  1133,  1134,  1135,     0,  1136,     0,   365,
     366,     0,   463,     0,   368,  1137,  1138,    64,    65,    66,
      67,    68,    69,    70,   369,   370,   358,  1139,   371,   372,
     373,  1500,   374,   375,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1863,     0,     0,     0,     0,     0,  -239,
     376,     0,     0,    75,   377,     0,     0,     0,   279,   963,
     378,    77,    78,   379,   380,   381,   382,     0,     0,     0,
       0,     0,     0,     0,     0,  -179,     0,   469,     0,     0,
     799,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1739,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1353,     0,     0,     0,
       0,     0,     0,     0,  1927,     0,     0,     0,     0,   950,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1614,
    1615,     0,     0,     0,     0,     0,     0,   361,     0,     0,
       0,   362,     0,   363,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   469,     0,   799,  1116,     0,
     364,    -2,     0,  1118,     0,     0,  1119,  1120,  1121,  1122,
    1123,  1124,  1125,  1126,  1127,  1128,  1129,  1130,  -320,  1131,
    1132,  1133,  1134,  1135,     0,  1136,     0,   365,   366,     0,
     463,     0,   368,  1137,  1138,    64,    65,    66,    67,    68,
      69,    70,   369,   370,   358,  1139,   371,   372,   373,   361,
     374,   375,     0,   362,     0,   363,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   364,     0,     0,   412,     0,     0,   376,     0,
    1686,    75,   377,     0,     0,     0,   279,     0,   378,    77,
      78,   379,   380,   381,   382,     0,     0,     0,     0,   365,
     366,     0,   367,  -179,   368,  1811,    63,    64,    65,    66,
      67,    68,    69,    70,   369,   370,   358,     0,   371,   372,
     373,     0,   374,   375,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1732,     0,     0,     0,  1557,  1558,  1559,     0,     0,     0,
     376,  1812,     0,    75,   377,     0,     0,     0,     0,     0,
     378,    77,    78,   379,   380,   381,   382,     0,     0,     0,
       0,     0,     0,     0,     0,  -179,     0,     0,     0,     0,
       0,  1756,     0,     0,  1758,     4,   243,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,  1115,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,     0,     0,
     361,     0,    45,    46,   362,     0,   363,    47,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,    56,
       0,  1116,    57,  1117,    -2,     0,  1118,     0,     0,  1119,
    1120,  1121,  1122,  1123,  1124,  1125,  1126,  1127,  1128,  1129,
    1130,  -320,  1131,  1132,  1133,  1134,  1135,     0,  1136,     0,
     365,   366,    60,   463,     0,   368,  1137,  1138,    64,    65,
      66,    67,    68,    69,    70,   369,   370,   358,  1139,   371,
     372,   373,     0,   374,   375,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      -3,   376,     0,     0,    75,   408,     0,     0,     0,   279,
       0,   378,    77,    78,   379,   380,   381,   382,     0,     0,
       0,     0,     0,     0,     0,     0,  -179,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     4,   243,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,  1115,
       0,    19,   963,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,     0,     0,
     361,     0,    45,    46,   362,     0,   363,    47,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,    56,
       0,  1116,    57,  1117,    -2,     0,  1118,     0,     0,  1119,
    1120,  1121,  1122,  1123,  1124,  1125,  1126,  1127,  1128,  1129,
    1130,  -320,  1131,  1132,  1133,  1134,  1135,     0,  1136,     0,
     365,   366,    60,   463,     0,   368,  1137,  1138,    64,    65,
      66,    67,    68,    69,    70,   369,   370,   358,  1139,   371,
     372,   373,     0,   374,   375,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   376,     0,     0,    75,   408,     0,     0,     0,   279,
       0,   378,    77,    78,   379,   380,   381,   382,     0,     0,
       0,     0,     0,     0,     0,     0,  -179,     4,   243,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
       0,     0,   361,     0,    45,    46,   362,     0,   363,    47,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,    56,     0,     0,    57,   364,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   365,   366,    60,   367,     0,   368,    62,    63,
      64,    65,    66,    67,    68,    69,    70,   369,   370,   358,
       0,   371,   372,   373,     0,   374,   375,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1557,  1558,  1559,
       0,     0,     0,   376,  1560,  1561,    75,   408,     0,     0,
       0,     0,     0,   378,    77,    78,   379,   380,   381,   382,
       0,     0,     0,     0,     0,     0,     0,     0,  1562,     4,
     243,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,   361,     0,    45,    46,   362,     0,
     363,    47,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,    56,     0,     0,    57,   364,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   365,   366,    60,   367,     0,   368,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   369,
     370,   358,     0,   371,   372,   373,     0,   374,   375,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1557,
    1558,  1559,     0,     0,     0,   376,  1560,     0,    75,   408,
       0,     0,     0,     0,     0,   378,    77,    78,   379,   380,
     381,   382,     0,     0,     0,     0,     0,     0,     0,     0,
    1562,     4,   243,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,   361,     0,    45,    46,
     362,     0,   363,    47,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,    56,     0,     0,    57,   364,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   365,   366,    60,   367,
       0,   368,    62,    63,    64,    65,    66,    67,    68,    69,
      70,   369,   370,   358,     0,   371,   372,   373,     0,   374,
     375,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   376,     0,  1548,
      75,   408,     0,     0,     0,     0,     0,   378,    77,    78,
     379,   380,   381,   382,     4,   243,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,     0,     0,   361,
       0,    45,    46,   362,     0,   363,    47,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,    56,     0,
       0,    57,   364,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   365,
     366,    60,   367,     0,   368,    62,    63,    64,    65,    66,
      67,    68,    69,    70,   369,   370,   358,     0,   371,   372,
     373,     0,   374,   375,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     376,     0,     0,    75,   408,     0,     0,     0,     0,     0,
     378,    77,    78,   379,   380,   381,   382,   243,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,     0,
       0,   361,     0,    45,    46,   362,     0,   363,   319,    48,
      49,    50,    51,    52,    53,    54,     0,     0,     0,     0,
       0,     0,     0,    57,   364,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   365,   366,     0,   367,     0,   368,    62,    63,    64,
      65,    66,    67,    68,    69,    70,   369,   370,   358,     0,
     371,   372,   373,     0,   374,   375,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   376,     0,     0,    75,   438,     0,     0,     0,
       0,     0,   378,   439,    78,   379,   380,   381,   382,   243,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,     0,     0,   361,     0,    45,    46,   362,     0,   363,
     319,    48,    49,    50,    51,    52,    53,    54,     0,     0,
       0,     0,     0,     0,     0,    57,   364,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   365,   366,     0,   367,     0,   368,    62,
      63,    64,    65,    66,    67,    68,    69,    70,   369,   370,
     358,     0,   371,   372,   373,     0,   374,   375,     0,     0,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   376,     0,     0,    75,  1190,     0,
       0,     0,     0,     0,   378,  1191,    78,   379,   380,   381,
     382,   243,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,     0,     0,   361,     0,    45,    46,   362,
       0,   363,   319,    48,    49,    50,    51,    52,    53,    54,
       0,     0,     0,     0,     0,     0,     0,    57,   364,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   365,   366,     0,   367,     0,
     368,    62,    63,    64,    65,    66,    67,    68,    69,    70,
     369,   370,   358,     0,   371,   372,   373,     0,   374,   375,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   376,     0,     0,    75,
     377,     0,     0,     0,     0,     0,   378,    77,    78,   379,
     380,   381,   382,   243,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,     0,     0,   361,     0,    45,
      46,   362,     0,   363,   319,    48,    49,    50,    51,    52,
      53,    54,     0,     0,     0,     0,     0,     0,     0,    57,
     364,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   365,   366,     0,
     367,     0,   368,    62,    63,    64,    65,    66,    67,    68,
      69,    70,   369,   370,   358,     0,   371,   372,   373,     0,
     374,   375,     0,     0,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   376,     0,
       0,    75,   438,     0,     0,     0,     0,     0,   378,    77,
      78,   379,   380,   381,   382,  1872,     0,    -2,    -2,    -2,
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
       0,     0,     0,     0,    -2,    -2,  1898,     0,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,     0,    -2,     0,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,     0,    -2,    -2,    -2,     0,    -2,     0,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,
       0,     0,    -2,     0,     0,    -2,  1416,     0,     0,     0,
      -2,    -2,     0,    13,    14,    15,    16,    17,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    -2,     0,     0,     0,    -2,
      -2,     0,     0,     0,     0,     0,     0,     0,     0,   361,
       0,     0,     0,   362,     0,   363,     0,     0,     0,     0,
       0,     0,     0,     0,    -2,     0,     0,     0,     0,     0,
       0,    57,   364,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    -2,     0,     0,     0,    -2,    -2,     0,
       0,     0,     0,     0,     0,    -2,    -2,     0,     0,   365,
     366,     0,   367,     0,   368,    62,    63,    64,    65,    66,
      67,    68,    69,    70,   369,   370,   358,     0,   371,   372,
     373,     0,   374,   375,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     376,     0,     0,    75,   377,     0,     0,     0,     0,     0,
     378,  1417,    78,   379,   380,   381,   382,     4,     5,     6,
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
      36,    37,    38,    39,    40,    41,    42,    43,    44,  -441,
    -441,     0,  -441,    45,    46,     0,  -441,     0,     0,     0,
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
    -752,     0,     0,    77,    78,     4,   243,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,     0,     0,
       0,     0,    45,    46,     0,     0,     0,    47,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,    56,
       0,     0,    57,     0,     0,     0,     0,  -373,  -373,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    60,     0,     0,     0,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -373,     0,     0,     0,    75,    76,     0,     0,     0,     0,
       0,     0,    77,    78,     4,   243,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,     0,     0,     0,
       0,    45,    46,     0,     0,     0,    47,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,    56,     0,
       0,    57,     0,     0,     0,     0,  -374,  -374,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    60,     0,     0,     0,    62,    63,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -374,
       0,     0,     0,    75,    76,     0,     0,     0,     0,     0,
       0,    77,    78,   242,   243,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,     0,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,  -441,  -441,     0,  -441,
      45,    46,     0,  -441,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      57,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    61,     0,     0,    62,    63,    64,    65,    66,    67,
      68,    69,    70,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    73,
      74,     0,    75,   244,     0,     0,  1329,     0,     0,     0,
      77,    78,  1330,     0,     0,    13,    14,    15,    16,    17,
      18,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,     0,
       0,     0,     0,    45,    46,     0,     0,     0,    47,    48,
      49,    50,    51,    52,    53,    54,     0,     0,     0,     0,
       0,     0,     0,    57,  1331,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    60,     0,     0,     0,    62,    63,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1332,     0,     0,     0,    75,   926,     0,     0,  1329,
       0,     0,     0,    77,    78,  1330,     0,     0,    13,    14,
      15,    16,    17,    18,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,     0,     0,    45,    46,     0,     0,
       0,    47,    48,    49,    50,    51,    52,    53,    54,     0,
       0,     0,     0,     0,     0,     0,    57,  1331,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    60,     0,     0,     0,
      62,    63,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1512,     0,     0,     0,    75,   926,
       0,     0,  1329,     0,     0,     0,    77,    78,  1330,     0,
       0,    13,    14,    15,    16,    17,    18,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,     0,     0,     0,     0,    45,
      46,     0,     0,     0,    47,    48,    49,    50,    51,    52,
      53,    54,     0,     0,     0,     0,     0,     0,     0,    57,
    1331,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    60,
       0,     0,     0,    62,    63,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1513,     0,     0,
       0,    75,   926,     0,     0,  1329,     0,     0,     0,    77,
      78,  1330,     0,     0,    13,    14,    15,    16,    17,    18,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,     0,     0,
       0,     0,    45,    46,     0,     0,     0,    47,    48,    49,
      50,    51,    52,    53,    54,     0,     0,     0,     0,     0,
       0,     0,    57,  1331,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    60,     0,     0,     0,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1514,     0,     0,     0,    75,   926,     0,     0,     0,     0,
       0,     0,    77,    78,   242,   243,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,     0,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,  -441,  -441,     0,
    -441,    45,    46,     0,  -441,     0,     0,     0,     0,     0,
       0,     0,     0,    13,    14,    15,    16,    17,     0,     0,
      19,    57,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,     0,     0,     0,
       0,    45,    46,     0,     0,    62,    63,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    57,     0,     0,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   242,     0,     0,
       0,     0,   658,    75,   244,     0,    13,    14,    15,    16,
      17,    77,    78,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
    -441,  -441,     0,  -441,    45,    46,     0,  -441,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   -16,     0,     0,
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
      37,    38,    39,    40,    41,    42,    43,    44,  -441,  -441,
       0,  -441,    45,    46,     0,  -441,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    57,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    61,     0,     0,    62,    63,    64,    65,
      66,    67,    68,    69,    70,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    73,    74,     0,    75,   244,     0,     0,     0,  -756,
       0,     0,    77,    78,   243,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,     0,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,  -441,  -441,     0,  -441,
      45,    46,     0,  -441,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      57,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    61,     0,     0,    62,    63,    64,    65,    66,    67,
      68,    69,    70,     0,     0,     0,     0,     0,     0,     0,
    1353,     0,     0,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    73,
      74,   361,    75,   244,     0,   362,     0,   363,     0,     0,
      77,    78,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1116,     0,   364,     0,     0,  1118,  1798,  1799,
    1119,  1120,  1121,  1122,  1123,  1124,  1125,  1126,  1127,  1128,
    1129,  1130,  -320,  1131,  1132,  1133,  1134,  1135,     0,  1136,
       0,   365,   366,     0,   463,     0,   368,  1137,  1138,    64,
      65,    66,    67,    68,    69,    70,   369,   370,   358,  1139,
     371,   372,   373,     0,   374,   375,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,  1353,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   376,     0,     0,    75,   377,     0,     0,     0,
     279,     0,   378,    77,    78,   379,   380,   381,   382,   361,
       0,     0,     0,   362,     0,   363,     0,  -179,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1116,     0,   364,     0,     0,  1118,     0,     0,  1119,  1120,
    1121,  1122,  1123,  1124,  1125,  1126,  1127,  1128,  1129,  1130,
    -320,  1131,  1132,  1133,  1134,  1135,     0,  1136,     0,   365,
     366,     0,   463,     0,   368,  1137,  1138,    64,    65,    66,
      67,    68,    69,    70,   369,   370,   358,  1139,   371,   372,
     373,     0,   374,   375,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     376,     0,     0,    75,   377,     0,     0,     0,   279,     0,
     378,    77,    78,   379,   380,   381,   382,     0,     0,     0,
       0,     0,     0,     0,     0,  -179,   243,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,     0,     0,
       0,     0,    45,    46,     0,     0,     0,   319,    48,    49,
      50,    51,    52,    53,    54,     0,    13,    14,    15,    16,
      17,    18,    57,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
       0,     0,     0,     0,    45,    46,    62,    63,     0,    47,
      48,    49,    50,    51,    52,    53,    54,     0,     0,     0,
       0,     0,     0,     0,    57,     0,     0,     0,     0,     0,
       0,    72,     0,  1051,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -624,    75,   321,     0,     0,    62,    63,
       0,     0,    77,    78,   243,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    75,     0,     0,     0,
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
       0,   320,    75,   321,     0,     0,    62,    63,     0,     0,
      77,    78,   243,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    75,     0,     0,     0,    45,    46,
       0,     0,     0,   319,    48,    49,    50,    51,    52,    53,
      54,     0,     0,     0,     0,     0,     0,     0,    57,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    72,     0,  1774,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      75,   321,     0,     0,     0,     0,     0,     0,    77,    78,
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
       0,     0,     0,     0,     0,    72,     0,  1776,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    75,   321,
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
       0,     0,     0,     0,     0,     0,    75,   301,     0,     0,
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
       0,     0,     0,     0,    75,   321,     0,     0,     0,     0,
       0,     0,    77,    78,   243,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,     0,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,  -441,  -441,     0,  -441,
      45,    46,     0,  -441,     0,     0,     0,     0,     0,     0,
       0,     0,    13,    14,    15,    16,    17,     0,     0,    19,
      57,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,     0,     0,
      45,    46,     0,     0,    62,    63,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      57,     0,     0,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   658,    75,   244,     0,     0,     0,     0,     0,     0,
      77,    78,    13,    14,    15,    16,    17,    18,   664,    19,
     665,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,   361,     0,
      45,    46,   362,     0,   363,    47,    48,    49,    50,    51,
      52,    53,    54,     0,     0,     0,     0,     0,     0,     0,
      57,   364,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   666,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   365,   366,
       0,   367,     0,   368,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   369,   370,   358,     0,   371,   372,   373,
       0,   374,   375,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   376,
       0,     0,    75,   667,     0,     0,     0,   279,     0,   378,
      77,    78,   668,   669,   381,   382,    13,    14,    15,    16,
      17,    18,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
       0,     0,   361,     0,    45,    46,   362,     0,   363,    47,
      48,    49,    50,    51,    52,    53,    54,     0,     0,     0,
       0,     0,     0,     0,    57,   364,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   365,   366,     0,   367,     0,   368,    62,    63,
      64,    65,    66,    67,    68,    69,    70,   369,   370,   358,
       0,   371,   372,   373,     0,   374,   375,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   376,     0,   407,    75,   408,     0,     0,
       0,     0,     0,   378,    77,    78,   379,   380,   381,   382,
      13,    14,    15,    16,    17,    18,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,   361,     0,    45,    46,
     362,     0,   363,    47,    48,    49,    50,    51,    52,    53,
      54,     0,     0,     0,     0,     0,     0,     0,    57,   364,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   365,   366,     0,   367,
       0,   368,    62,    63,    64,    65,    66,    67,    68,    69,
      70,   369,   370,   358,     0,   371,   372,   373,     0,   374,
     375,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   376,     0,     0,
      75,   667,     0,     0,     0,   279,     0,   378,    77,    78,
     379,   380,   381,   382,    13,    14,    15,    16,    17,    18,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,     0,     0,
     361,     0,    45,    46,   362,     0,   363,    47,    48,    49,
      50,    51,    52,    53,    54,     0,     0,     0,     0,     0,
       0,     0,    57,   364,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     365,   366,     0,   367,     0,   368,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   369,   370,   358,     0,   371,
     372,   373,     0,   374,   375,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   376,     0,     0,    75,   408,     0,     0,     0,     0,
       0,   378,    77,    78,   379,   380,   381,   382,    13,    14,
      15,    16,    17,    18,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,   361,     0,    45,    46,   362,     0,
     363,   319,    48,    49,    50,    51,    52,    53,    54,     0,
       0,     0,     0,     0,     0,     0,    57,   364,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   365,   366,     0,   367,     0,   368,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   369,
     370,   358,     0,   371,   372,   373,     0,   374,   375,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   376,     0,     0,    75,   438,
       0,     0,     0,     0,     0,   378,    77,    78,   379,   380,
     381,   382,    13,    14,    15,    16,    17,    18,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,   361,     0,
      45,    46,   362,     0,   363,   319,    48,    49,    50,    51,
      52,    53,    54,     0,     0,     0,     0,     0,     0,     0,
      57,   364,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   365,   366,
       0,   367,     0,   368,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   369,   370,   358,     0,   371,   372,   373,
       0,   374,   375,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   376,
       0,     0,    75,   377,     0,     0,     0,     0,     0,   378,
      77,    78,   379,   380,   381,   382,    13,    14,    15,    16,
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
       0,  -754,     0,     0,    77,    78,    13,    14,    15,    16,
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
      42,    43,    44,  -441,  -441,     0,  -441,    45,    46,     0,
    -441,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    57,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    61,     0,
       0,    62,    63,    64,    65,    66,    67,    68,    69,    70,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    73,    74,     0,    75,
     301,     0,     0,     0,     0,     0,     0,    77,    78,   557,
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
      57,     0,     0,     0,     0,     0,  1432,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   932,
      75,   926,     0,     0,    62,    63,     0,     0,    77,    78,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    75,   926,     0,     0,     0,     0,     0,     0,
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
       0,     0,     0,     0,    75,   434,     0,     0,    62,    63,
       0,     0,    77,    78,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    75,   321,     0,     0,
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
       0,     0,     0,     0,     0,     0,     0,     0,    75,   434,
       0,     0,     0,     0,     0,     0,    77,    78,   242,   243,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,     0,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,  -441,  -441,     0,  -441,    45,    46,     0,  -441,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    13,
      14,    15,    16,    17,    18,    57,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,     0,     0,     0,     0,    45,    46,    62,
      63,     0,   319,    48,    49,    50,    51,    52,    53,    54,
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
       0,    75,   926,     0,     0,     0,     0,     0,     0,    77,
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
       0,    75,   926,     0,     0,    62,    63,     0,     0,    77,
      78,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    75,     0,     0,    13,    14,    15,    16,
      17,    77,    78,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
    -441,  -441,     0,  -441,    45,    46,     0,  -441,     0,     0,
       0,     0,     0,     0,     0,     0,    13,    14,    15,    16,
      17,     0,     0,    19,    57,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
    -441,  -441,     0,  -441,    45,    46,     0,  -441,    62,    63,
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
       0,     0,    45,    46,     0,     0,     0,   319,    48,    49,
      50,    51,    52,    53,    54,     0,     0,     0,     0,     0,
       0,     0,    57,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   852,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -637,    75,   243,     6,     7,     8,     9,
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
       0,     0,  1694,     0,     0,     0,     0,   243,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,     0,    19,    75,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,     0,
       0,     0,     0,    45,    46,     0,     0,     0,   319,    48,
      49,    50,    51,    52,    53,    54,     0,     0,     0,     0,
       0,     0,     0,    57,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   243,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    62,    63,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,  -441,  -441,     0,  -441,
      45,    46,     0,  -441,     0,     0,     0,     0,     0,     0,
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
      37,    38,    39,    40,    41,    42,    43,    44,  -441,  -441,
      75,  -441,    45,    46,     0,  -441,     0,     0,     0,     0,
     361,     0,     0,     0,   362,     0,   363,     0,     0,     0,
       0,     0,    57,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   364,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    62,    63,     0,     0,
     365,   366,     0,   367,     0,   368,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   369,   370,   358,     0,   371,
     372,   373,     0,   374,   375,     0,   361,     0,     0,     0,
     362,    72,   363,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    75,  1557,  1558,  1559,     0,   364,
       0,   376,  1719,     0,    75,   377,     0,     0,     0,     0,
       0,   378,    77,    78,   379,   380,   381,   382,     0,     0,
       0,     0,     0,     0,     0,     0,   365,   366,     0,   463,
       0,   368,    62,    63,    64,    65,    66,    67,    68,    69,
      70,   369,   370,   358,     0,   371,   372,   373,   361,   374,
     375,     0,   362,     0,   363,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   364,     0,     0,     0,     0,     0,   376,    74,     0,
     464,   465,     0,     0,     0,   466,     0,   378,    77,    78,
     379,   380,   381,   382,     0,     0,     0,     0,   365,   366,
       0,   367,     0,   368,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   369,   370,   358,     0,   371,   372,   373,
     361,   374,   375,     0,   362,     0,   363,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   364,     0,     0,     0,     0,     0,   376,
    1236,     0,    75,   377,     0,     0,     0,  1237,     0,   378,
      77,    78,   379,   380,   381,   382,     0,     0,     0,     0,
     365,   366,     0,   367,     0,   368,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   369,   370,   358,     0,   371,
     372,   373,   361,   374,   375,     0,   362,     0,   363,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   364,     0,     0,     0,     0,
       0,   376,   959,  1539,    75,   377,     0,     0,     0,     0,
       0,   378,    77,    78,   379,   380,   381,   382,     0,     0,
       0,     0,   365,   366,     0,   367,     0,   368,    62,    63,
      64,    65,    66,    67,    68,    69,    70,   369,   370,   358,
       0,   371,   372,   373,   361,   374,   375,     0,   362,     0,
     363,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   364,     0,     0,
       0,     0,     0,   376,   798,     0,    75,   377,     0,     0,
       0,     0,     0,   378,    77,    78,   379,   380,   381,   382,
       0,     0,     0,     0,   365,   366,     0,   367,     0,   368,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   369,
     370,   358,     0,   371,   372,   373,   361,   374,   375,     0,
     362,     0,   363,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   364,
       0,     0,     0,     0,     0,   376,     0,     0,    75,   377,
       0,     0,     0,   279,     0,   378,    77,    78,   379,   380,
     381,   382,     0,     0,     0,     0,   365,   366,     0,   367,
       0,   368,    62,    63,    64,    65,    66,    67,    68,    69,
      70,   369,   370,   358,     0,   371,   372,   373,   361,   374,
     375,     0,   362,     0,   363,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   364,     0,     0,     0,     0,     0,   376,   959,     0,
      75,   377,     0,     0,     0,     0,     0,   378,    77,    78,
     379,   380,   381,   382,     0,     0,     0,     0,   365,   366,
       0,   367,     0,   368,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   369,   370,   358,     0,   371,   372,   373,
     361,   374,   375,     0,   362,     0,   363,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   364,     0,     0,     0,     0,     0,   376,
       0,     0,    75,   377,     0,     0,   990,     0,     0,   378,
      77,    78,   379,   380,   381,   382,     0,     0,     0,     0,
     365,   366,     0,   367,     0,   368,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   369,   370,   358,     0,   371,
     372,   373,   361,   374,   375,     0,   362,     0,   363,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   364,     0,     0,     0,     0,
       0,   376,     0,     0,    75,   377,     0,     0,     0,  1211,
       0,   378,    77,    78,   379,   380,   381,   382,     0,     0,
       0,     0,   365,   366,     0,   367,     0,   368,    62,    63,
      64,    65,    66,    67,    68,    69,    70,   369,   370,   358,
       0,   371,   372,   373,   361,   374,   375,     0,   362,     0,
     363,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   364,     0,     0,
       0,     0,     0,   376,  1303,     0,    75,   377,     0,     0,
       0,     0,     0,   378,    77,    78,   379,   380,   381,   382,
       0,     0,     0,     0,   365,   366,     0,   367,     0,   368,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   369,
     370,   358,     0,   371,   372,   373,   361,   374,   375,     0,
     362,     0,   363,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   364,
       0,     0,     0,     0,     0,   376,     0,     0,    75,   377,
       0,     0,     0,  1363,     0,   378,    77,    78,   379,   380,
     381,   382,     0,     0,     0,     0,   365,   366,     0,   367,
       0,   368,    62,    63,    64,    65,    66,    67,    68,    69,
      70,   369,   370,   358,     0,   371,   372,   373,   361,   374,
     375,     0,   362,     0,   363,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   364,     0,     0,     0,     0,     0,   376,     0,  1804,
      75,   377,     0,     0,     0,     0,     0,   378,    77,    78,
     379,   380,   381,   382,     0,     0,     0,     0,   365,   366,
       0,   367,     0,   368,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   369,   370,   358,     0,   371,   372,   373,
     361,   374,   375,     0,   362,     0,   363,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   364,     0,     0,     0,     0,     0,   376,
    1948,     0,    75,   377,     0,     0,     0,     0,     0,   378,
      77,    78,   379,   380,   381,   382,     0,     0,     0,     0,
     365,   366,     0,   367,     0,   368,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   369,   370,   358,     0,   371,
     372,   373,   361,   374,   375,     0,   362,     0,   363,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   364,     0,     0,     0,     0,
       0,   376,  1989,     0,    75,   377,     0,     0,     0,     0,
       0,   378,    77,    78,   379,   380,   381,   382,     0,     0,
       0,     0,   365,   366,     0,   367,     0,   368,    62,    63,
      64,    65,    66,    67,    68,    69,    70,   369,   370,   358,
       0,   371,   372,   373,   361,   374,   375,     0,   362,     0,
     363,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   364,     0,     0,
       0,     0,     0,   376,  2013,     0,    75,   377,     0,     0,
       0,     0,     0,   378,    77,    78,   379,   380,   381,   382,
       0,     0,     0,     0,   365,   366,     0,   367,     0,   368,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   369,
     370,   358,     0,   371,   372,   373,   361,   374,   375,     0,
     362,     0,   363,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   364,
       0,     0,     0,     0,     0,   376,     0,     0,    75,   377,
       0,     0,     0,     0,     0,   378,    77,    78,   379,   380,
     381,   382,     0,     0,     0,     0,   365,   366,     0,   367,
       0,   368,    62,    63,    64,    65,    66,    67,    68,    69,
      70,   369,   370,   358,     0,   371,   372,   373,   361,   374,
     375,     0,   362,     0,   363,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   364,     0,     0,     0,     0,     0,   650,     0,     0,
      75,   377,     0,     0,     0,     0,     0,   378,    77,    78,
     379,   380,   381,   382,     0,     0,     0,     0,   365,   366,
       0,   367,     0,   368,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   369,   370,   358,     0,   371,   372,   373,
     361,   374,   375,     0,   362,     0,   363,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   364,     0,     0,     0,     0,     0,   655,
       0,     0,    75,   377,     0,     0,     0,     0,     0,   378,
      77,    78,   379,   380,   381,   382,     0,     0,     0,     0,
     365,   366,     0,   367,     0,   368,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   369,   370,   358,     0,   371,
     372,   373,   361,   374,   375,     0,   362,     0,   363,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   364,     0,     0,     0,     0,
       0,   661,     0,     0,    75,   377,     0,     0,     0,     0,
       0,   378,    77,    78,   379,   380,   381,   382,     0,     0,
       0,     0,   365,   366,     0,   367,     0,   368,    62,    63,
      64,    65,    66,    67,    68,    69,    70,   369,   370,   358,
       0,   371,   372,   373,   361,   374,   375,     0,   362,     0,
     363,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   364,     0,     0,
       0,     0,     0,   376,     0,     0,    75,   377,     0,     0,
       0,     0,     0,   378,   866,    78,   379,   380,   381,   382,
       0,     0,     0,     0,   365,   366,     0,   367,     0,   368,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   369,
     370,   358,     0,   371,   372,   373,   361,   374,   375,     0,
     362,     0,   363,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   364,
       0,     0,     0,     0,     0,   376,     0,     0,    75,   377,
       0,     0,     0,     0,     0,   378,   439,    78,   379,   380,
     381,   382,     0,     0,     0,     0,   365,   366,     0,   367,
       0,   368,  1888,    63,    64,    65,    66,    67,    68,    69,
      70,   369,   370,   358,     0,   371,   372,   373,     0,   374,
     375,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   376,     0,     0,
      75,   377,     0,     0,     0,     0,     0,   378,    77,    78,
     379,   380,   381,   382,    13,    14,    15,    16,    17,     0,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,  -440,  -440,
       0,  -440,    45,    46,     0,  -440,     0,     0,     0,     0,
       0,     0,     0,     0,    13,    14,    15,    16,    17,     0,
       0,    19,    57,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,  -441,  -441,
       0,  -441,    45,    46,     0,  -441,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    57
};

static const yytype_int16 yycheck[] =
{
       1,   150,   178,     4,   283,   162,   241,    73,   220,   162,
     678,   173,    73,   256,   207,   466,     4,   875,   376,  1738,
     219,     1,   219,    95,    73,    73,    58,   219,   324,   609,
    1669,   177,   219,   693,     1,   162,   635,   222,   965,   616,
     737,  1355,   766,   671,   515,   516,  1669,   607,  1287,  1288,
     163,   603,    75,   603,    55,    56,  1125,    58,   339,   322,
     219,  1669,   343,   766,     1,   767,     1,   994,    70,     4,
     764,   773,    73,     0,  1567,   219,   339,   603,    58,    73,
     343,    82,   229,   157,   296,  1563,   764,   236,   139,    90,
     367,    58,   632,   219,    95,     1,   295,    98,   295,     0,
     174,   102,    82,   295,  1209,   290,   291,   254,   295,   526,
      73,   861,    73,   145,   102,    95,   764,   264,    98,   536,
     181,    58,   102,    58,    70,     1,    95,     1,     4,    87,
    1057,  1798,   181,   181,   130,   764,   295,   845,    70,   140,
     191,   131,   143,  1670,   145,    96,     1,   149,   220,   150,
     446,   295,    73,   219,    73,   156,  1802,    87,   219,     0,
     577,    87,   163,    87,   872,   145,   637,   102,   157,   295,
     219,   219,   376,  1355,  1356,   165,   172,   440,   145,   180,
     181,   861,    58,  1109,    58,   174,   463,   181,   768,    70,
     115,  1245,   772,   830,   195,   322,   149,   155,   149,   157,
     180,   781,   782,    58,   205,   174,   766,   153,   145,   210,
     145,   174,   764,   174,   764,   195,   149,   149,   219,   220,
    1713,   153,    98,   784,   296,   219,   102,   157,   102,   295,
      19,   157,  1099,   157,   295,   236,   148,  1104,   764,   155,
     220,   802,  1720,   155,   245,   228,   295,   295,   231,   789,
     156,   274,   889,     4,   255,   498,  1109,   258,   174,  1786,
     181,   131,   181,   923,   265,   245,  1933,    82,   149,   145,
     253,   145,   153,   744,   275,   276,  1590,   278,   258,   472,
     263,   493,  1563,   882,   563,   484,  2005,   484,     1,   316,
     145,   951,   484,   131,   295,   296,    82,   484,   219,   595,
     219,   149,   303,  1027,    55,    56,  1003,   868,  1175,   310,
     311,   592,   940,   440,   315,   149,   296,   916,  1423,  1021,
    1959,  1109,     1,   619,  1027,   484,  1020,   165,   355,   592,
     626,   444,   147,   691,  1980,  1813,  1959,   897,   115,    90,
     484,   211,  1020,   178,    73,    58,   283,   348,   629,    75,
      76,  1959,   353,   616,   120,   356,   357,   149,   484,    88,
     572,  1936,   999,   149,   513,  2011,   629,  1117,   783,   784,
     519,   570,  1020,   570,   152,  1902,   153,   917,   570,  1016,
    1017,  1956,   258,   570,   149,   436,   152,   802,   535,   140,
      82,  1020,   143,  1101,   155,   173,  1146,  1451,  1452,  1453,
    1326,  1327,  1328,    95,   154,   156,    98,  1982,  1590,   283,
     102,   570,   163,   174,   284,  1654,  1655,   574,    70,   420,
     150,   493,   149,   484,   155,   842,   570,   157,   283,   156,
     245,   157,   145,  1960,   265,   484,   484,  1117,  1719,  1720,
     420,   464,   443,   444,   570,   276,   650,   574,   652,   653,
     149,   655,   151,   868,   455,   456,  1323,   661,    70,   210,
     664,   665,   666,   464,  1044,   466,  1146,  1027,  1020,  1996,
    1020,    70,   155,  1326,  1327,  1328,   155,   156,    59,    60,
      75,    76,   192,   484,   146,   174,   131,   302,   180,   616,
     484,   174,   493,  1562,  1020,   157,  1198,   149,  1567,   174,
     572,   153,   149,   195,   255,   151,   376,   155,  1069,   570,
     156,   173,   513,   493,   265,   160,   161,   165,   519,  1446,
       3,   570,   570,   506,   275,   276,   173,   278,   220,   155,
      56,  1812,  1813,    59,    60,  1717,    62,   149,  1326,  1327,
    1328,   153,   155,   574,   576,   528,   997,     3,   174,   155,
     149,   534,   303,   245,   153,   538,   557,   853,   559,   310,
     311,   174,   157,   484,   315,   484,   258,   152,   174,   570,
     283,   572,   718,    70,  1276,   576,   157,     4,     5,     6,
       7,     8,     9,    10,    11,   586,    70,  1255,   155,   590,
     886,   149,   572,   174,   155,   410,   576,   348,   791,   157,
     155,   777,   353,   884,   165,   356,  1205,   174,   155,   576,
      12,    13,    14,    15,    16,  1192,  1855,   155,    70,   174,
      70,   884,   623,   149,   104,   105,   563,   174,    70,   151,
     149,  1268,  1269,   155,   635,   505,    70,    70,   157,   576,
     510,   576,   149,   155,  1713,  1282,  1283,  1187,    12,    13,
      14,    15,    16,   766,  1069,   804,   593,   527,   157,   151,
      70,    70,   174,  1521,   157,   149,    70,   537,    70,   153,
     162,   163,   152,  1213,   823,  1355,  1356,   173,   824,  1316,
    1317,   148,   683,    70,   685,  1345,   687,   561,   155,   563,
     691,  1873,   443,   694,   157,    70,   719,   149,  1366,   149,
     576,   153,   576,   153,   455,   456,    70,   149,   563,   153,
    1637,   153,  1639,  1184,   158,   149,   149,  1899,   719,   153,
     153,   576,  1262,   103,   151,   125,   126,   107,   420,   131,
     110,     9,   112,   104,   105,    73,   155,   153,   914,   149,
     149,  1810,   158,   153,   153,   149,   165,   174,   155,   153,
     149,    89,   783,   784,  1823,  1937,   151,   171,   165,   151,
    1211,   156,   149,   764,   156,   766,   153,   131,   106,   169,
     170,   802,   155,   155,   149,  1443,   149,   778,   153,   936,
     650,   149,   165,   165,   785,   655,  1237,     3,   129,     1,
     791,   661,     4,   794,   115,   908,    12,    13,    14,    15,
      16,   493,   803,   804,   805,   155,   557,   157,   149,   936,
     680,   521,   153,  1056,   143,   144,   145,   151,  1887,   160,
     161,   155,   823,   101,   761,   129,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   586,   165,   868,   151,   590,
      12,    13,    14,    15,    16,   149,    58,   149,   151,   153,
     563,   152,   153,   156,    70,   156,   160,   161,   859,   860,
     861,   844,   151,   576,  1501,  1502,   160,   151,   248,   173,
      82,  1064,   623,   167,   168,   151,   151,   129,     3,   155,
    1420,   882,   151,   593,   635,  1425,   874,    12,    13,    14,
      15,    16,   149,   151,   861,   153,   153,   149,    70,   129,
    1196,   153,  1442,  1088,   151,   936,   149,   908,   160,   161,
    1590,   912,  1363,  1026,  1027,   916,   149,   155,   151,   149,
     153,   922,   353,   153,   861,   356,   861,   139,    21,  1192,
     160,   161,   683,   145,   685,   147,   687,   151,   149,   874,
     691,   155,   777,   694,     9,    70,   326,   327,   149,   329,
     155,   331,    82,   788,    56,   155,   957,    59,    60,   829,
      62,     1,    46,    47,   965,    49,  1201,   151,   719,    53,
     840,   155,  1330,   843,   151,   155,  1613,   847,   155,   191,
       3,   155,   149,    96,   151,   861,   153,   861,   151,   157,
    1441,   151,   155,   994,   101,   155,   997,   149,   874,   106,
     107,   108,   109,   110,   111,   112,   861,  1644,   151,   152,
     720,   149,  1649,  1650,   149,   153,    87,   147,    58,  1020,
    1224,   157,  1621,  1185,  1186,  1026,  1027,   778,   157,  1178,
     242,   151,  1572,   245,   785,   155,   101,  1717,  1069,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   148,   179,
     154,   761,   803,   149,   805,   151,  1057,   153,    98,   151,
     149,   173,   151,  1220,   153,  1192,   108,   109,   110,   111,
     112,   283,   157,   783,   784,  1318,  1355,   157,   101,   914,
     157,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     302,  1631,   802,  1220,   151,     4,     5,     6,     7,     8,
       9,    10,    11,   149,   115,   145,   129,   153,   859,   860,
     861,   149,   149,   149,     1,   245,   153,     4,  1119,  1295,
    1296,  1122,  1123,  1124,  1336,   151,   149,   150,   155,   155,
     151,   882,   149,   156,   155,  1288,   153,   160,   161,    12,
      13,    14,    15,    16,    17,  1146,   162,   163,   861,   149,
    1117,  1152,   532,  1752,    63,   990,   151,   908,   868,  1160,
     155,   912,  1163,  1164,   151,   916,  1167,   166,   155,   151,
     151,    58,   302,   155,   155,   161,  1164,  1178,  1838,  1146,
    1117,   159,  1117,  1163,  1164,   143,   144,   145,   171,  1220,
     151,   129,   322,  1873,   155,    82,   151,   155,   410,   151,
     155,   151,   151,   155,  1205,   155,   155,   165,   151,  1146,
    1211,  1146,   155,  1753,   151,   102,   174,   151,   258,  1899,
    1221,   155,   151,   151,   436,    12,   155,   155,   151,  1164,
    1964,  1066,   155,  1103,  1968,   152,  1237,   143,   144,   145,
     151,  1117,   154,  1117,  1245,   153,  1116,   151,  1405,   155,
     104,   105,   139,   698,   699,   700,  1287,  1937,   145,   165,
     147,   151,  1117,  1133,  1336,  1512,  1513,  1514,   174,   151,
    1146,   151,  1146,  1106,  1107,  1108,  1277,   156,  1405,   156,
     410,   123,   124,   131,   324,   154,   155,  1163,  1164,   155,
    1164,  1146,   179,   131,  1109,   127,   128,   154,   155,    86,
     705,   706,   707,   708,   191,   149,  1846,  1519,   160,   161,
     440,  1590,   155,   156,   101,  1527,   151,   104,   105,   106,
     107,   108,   109,   110,   111,   112,  1525,   151,  1525,   154,
     155,   154,   155,  1525,   151,  1336,   154,   155,  1525,  1340,
    1977,  1329,  1343,   205,   151,   557,   154,   155,   154,   155,
     151,   563,   154,   155,   151,   242,   154,   155,   245,  1069,
     154,   155,  1363,   250,   576,   149,  1525,   153,  1119,   154,
     155,  1122,  1123,  1124,  1405,    12,    13,    14,    15,    16,
      17,  1525,  1383,   154,  1385,   515,   516,    12,    13,    14,
      15,    16,    17,   157,  1329,  1146,   283,   154,   155,  1525,
      68,  1152,   154,   155,  1117,  1385,   446,   154,   155,  1160,
     154,   155,   154,   155,   157,   302,  1167,   157,  1355,  1356,
    1355,  1356,   157,  1258,   154,  1629,    12,    13,    14,    15,
      16,    17,   149,  1146,   154,   155,   154,   155,   154,   155,
    1441,  1429,    76,  1589,  1601,  1446,   157,  1519,  1601,   154,
    1451,  1452,  1453,  1329,  1205,  1527,   155,   156,   173,  1525,
    1295,  1296,   154,   155,    12,    13,    14,    15,    16,   155,
    1221,  1163,  1164,    17,  1601,  1687,  1346,  1347,   173,  1355,
    1356,  1355,  1356,   154,   155,  1684,   616,  1684,   149,   376,
      75,    76,  1684,   157,  1429,   155,   156,  1684,  1246,  1247,
    1355,  1356,  1655,   701,   702,   709,   710,   637,   174,  1385,
     151,  1326,  1327,  1328,  1384,  1330,  1331,   157,  1519,   151,
     560,   174,    70,   410,  1525,  1684,  1527,   703,   704,  1532,
    1533,    17,   154,  1534,  1185,  1186,   576,   157,   154,    17,
    1684,   148,   151,   151,   148,   151,   151,  1548,   151,   436,
     151,   151,   151,  1429,   151,   157,    68,   174,  1684,   118,
    1561,   120,   121,   122,   157,   157,   148,  1830,  1556,    55,
      56,    57,    58,    59,    60,    61,    62,  1287,  1288,   619,
     151,   129,   173,  1795,   151,   151,   151,  1733,   157,  1340,
     149,   151,  1343,   152,   153,  1744,   154,  1598,   157,   158,
     155,   149,   464,   155,   466,   153,   151,  1903,   151,   151,
     151,   151,   160,   161,   744,  1687,  1604,   151,   151,   151,
    1621,  1556,  1798,  1654,   154,   151,   151,   151,   515,   516,
     151,   151,  1383,   151,   151,   151,  1637,  1900,  1639,   154,
     151,   151,  1355,  1356,   148,   173,   151,   148,   151,   861,
      13,   149,   149,  1590,   149,  1590,   155,   149,   149,   149,
     156,    72,   874,    89,   148,   174,   155,   174,   156,  1604,
     557,   154,   154,   148,   561,   151,   563,   157,   155,  1555,
    1556,   174,  1556,  1684,   154,   151,  1687,   174,  1834,   576,
     155,   151,   155,  1385,   155,   364,   154,  1698,   101,   151,
     174,  1702,  1855,   106,   107,   108,   109,   110,   111,   112,
     151,  1581,   148,   148,  1590,  1716,  1590,   149,   174,   174,
     389,   390,   149,  1795,    78,  1726,   174,   174,  1604,   174,
     174,   174,   149,   148,   174,  1590,   174,   149,   151,   148,
     148,   410,  1743,  1744,   148,   875,   149,   150,  1960,   155,
     637,  1752,   154,  1902,   155,  1931,   154,  1933,   154,   151,
    1959,   154,  1959,   650,   148,   652,   653,  1959,   655,   157,
     151,   440,  1959,   156,   661,   118,   156,   664,   665,   666,
    1717,   148,  1717,  1534,  1996,   151,   151,   151,   174,   151,
    1966,   151,   148,   154,  1795,   154,   151,  1548,   156,   149,
    1959,  1802,   155,   151,   155,  1806,   107,   149,  1809,   149,
    1561,   157,   148,   853,   148,  1959,   154,   154,  1964,   154,
     148,   861,  1968,  1969,   864,   151,   151,  1537,  1704,   151,
     151,   154,   151,  1959,  1835,   151,    12,    13,    14,    15,
      16,  1717,    73,  1717,    73,   174,   886,  1598,   150,   148,
     174,   149,  1998,    88,   174,   151,   552,   744,   151,   154,
     154,   148,  1717,  2039,   148,   151,   151,    73,   151,   151,
    1621,   151,   151,   165,  2020,  1876,   101,  1590,  2024,  1880,
     153,   106,   107,   108,   109,   110,   111,   112,  1960,     1,
     152,  1892,     4,  1959,    70,    73,   174,  1109,  1959,   150,
    2046,  1902,   174,  1904,   156,  1117,   174,   151,   148,   151,
    1959,  1959,   151,   151,  1915,   148,  1917,  1918,   155,   165,
     150,   148,   156,   149,  1996,   150,   101,   155,   153,   791,
      73,   149,   794,   148,  1146,   154,  1873,   107,  1873,  1940,
     107,   165,   174,   151,  1654,  1655,    58,  1698,   151,   156,
     148,  1702,   174,   129,   148,   151,   149,   174,  1959,  1960,
      73,    73,  1899,  1798,  1899,  1716,   151,   151,   378,  1970,
      82,  1629,   174,   149,   861,  1726,  1345,   153,  1252,  1980,
    1960,   670,   174,    95,   160,   161,    98,   874,   875,   711,
     102,   712,  1743,  1135,   713,  1996,  1146,  1873,  2011,  1873,
    1937,  1752,  1937,   715,  1717,   714,  1590,  1725,  2009,   409,
    2011,  1933,  1956,  1717,  2006,  1830,  1996,  1839,  1873,  2005,
    1993,  1900,  1582,  1899,  1969,  1899,  2027,   139,   697,  1582,
    1899,  2024,  2033,   145,  1167,   147,    48,   250,   150,   151,
    1519,  1795,  2043,  1863,  1899,  1331,  2047,  1533,   473,  1160,
     162,  1802,   791,  1429,  1184,  1806,  2057,   878,  1809,   586,
    1604,  1937,  1192,  1937,     0,    -1,   922,   179,   180,   181,
      -1,   736,    -1,   736,   736,    -1,    -1,  1117,    -1,   191,
     192,   101,  1937,   195,  1835,  1900,   106,   107,   108,   109,
     110,   111,   112,   113,    -1,   957,  1931,   117,  1933,   119,
      -1,    -1,    -1,   965,    -1,    -1,  1146,   219,   220,    -1,
      -1,    -1,    -1,    -1,  1326,  1327,  1328,  1329,  1330,  1331,
      -1,    -1,    -1,  1163,   236,  1876,    -1,    -1,    -1,  1880,
     150,  1966,   994,   245,   830,   997,    -1,    -1,    -1,    98,
      -1,  1892,    -1,  1355,  1356,  1855,   258,    -1,    -1,  2019,
     109,    -1,    -1,  1904,    17,    -1,  1196,    -1,    -1,  1994,
    1873,    -1,    -1,   101,  1915,  2035,  1917,  1918,   106,   107,
     108,   109,   110,   111,   112,   113,   288,    12,    13,    14,
      15,    16,   294,   295,   296,    -1,  1899,    -1,    -1,  1940,
     302,    -1,   151,   889,    -1,  1057,    59,    60,    61,    62,
      -1,    -1,    -1,    -1,  2039,    -1,    -1,    -1,    -1,    -1,
     322,   323,   324,    -1,    -1,   153,    -1,  1429,    -1,  1970,
      -1,    -1,  1109,    -1,  1937,    -1,    -1,   339,    -1,  1980,
    1117,   343,    -1,    -1,    -1,    70,   195,    -1,   101,    -1,
      -1,    -1,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2009,  1146,
    2011,    -1,    -1,   932,   376,    -1,    -1,    -1,   937,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2027,  1164,    -1,   948,
      -1,    -1,  2033,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     153,    -1,  2043,    -1,   129,    -1,  2047,  1184,   410,   258,
      -1,   413,    -1,   999,    -1,    -1,  2057,    -1,   420,    63,
      64,    65,    66,    -1,   149,  1355,  1356,    -1,   153,   230,
    1016,  1017,    -1,    -1,   436,   160,   161,    -1,   440,   288,
      -1,    -1,   444,    -1,   446,   294,    -1,  1224,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1385,    -1,   101,    -1,  1211,
     104,   105,   106,   107,   108,   109,   110,   111,   112,    -1,
      -1,    -1,    -1,    -1,    -1,   324,    -1,    -1,    -1,    -1,
      -1,    -1,   484,    -1,    -1,  1237,    -1,    -1,  1590,    -1,
      -1,   493,    -1,  1245,    -1,    -1,    -1,    -1,    -1,    -1,
      76,  1521,  1604,    -1,    -1,    -1,    70,    -1,    -1,   153,
      -1,   513,    -1,   515,   516,    -1,    -1,   519,    -1,   521,
      -1,    -1,    -1,    -1,    -1,   101,    -1,   171,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   101,    -1,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,  1326,
    1327,  1328,  1329,  1330,  1331,    -1,   101,   559,  1117,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   570,    -1,
     572,    -1,   574,    -1,   576,    -1,    -1,    -1,  1355,  1356,
      -1,    -1,    -1,    -1,    -1,   149,   150,    -1,    -1,    -1,
     592,   593,    -1,   595,    -1,    -1,    -1,   446,   174,    -1,
      -1,   603,    -1,    -1,    -1,   607,    -1,    -1,    -1,    -1,
      -1,  1363,   413,    -1,   616,  1717,     4,     5,     6,     7,
       8,     9,    10,    11,   626,  1555,    -1,   629,   429,   174,
      -1,   432,    -1,  1192,   101,   637,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    -1,    -1,   650,    -1,
     652,   653,  1429,   655,    -1,    -1,    -1,    -1,    -1,   661,
    1590,    -1,   664,   665,   666,    -1,    -1,    -1,  1227,  1228,
    1229,   520,    -1,    -1,    -1,  1234,  1235,    -1,    -1,    -1,
      -1,    -1,  1268,  1269,    -1,    -1,    -1,    -1,   489,  1441,
      -1,   540,    -1,    -1,  1446,    -1,  1282,  1283,    -1,  1451,
    1452,  1453,    -1,    -1,    -1,    -1,    -1,   174,    -1,    -1,
      -1,   560,    -1,    -1,    -1,    -1,    70,    -1,   720,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   192,    -1,    -1,
    1316,  1317,    -1,    -1,   736,   737,    -1,    -1,    -1,    -1,
      -1,    -1,   744,    -1,  1521,    -1,   595,   101,     1,    -1,
      -1,     4,   106,   107,   108,   109,   110,   111,   112,   761,
      -1,    -1,   764,    -1,   766,  1119,    -1,    -1,    -1,    -1,
     619,  1873,    -1,    -1,  1704,   129,    -1,   626,    -1,  1556,
      -1,   783,   784,    -1,    -1,    -1,    -1,  1717,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   149,   150,  1899,    -1,    -1,
     802,    -1,   804,    -1,    -1,    58,   160,   161,    -1,    -1,
     659,   660,    -1,  1590,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   823,    -1,    -1,    -1,    -1,    -1,  1604,    -1,    82,
      -1,    -1,    -1,   101,    -1,  1937,   104,   105,   106,   107,
     108,   109,   110,   111,   112,    98,    -1,    -1,    -1,   102,
      -1,    -1,  1629,    -1,    -1,    -1,    -1,   322,    -1,   861,
     325,    -1,    -1,    -1,    -1,    -1,   868,    -1,    -1,    -1,
      -1,    -1,   874,   875,   339,    -1,    -1,    -1,   343,    -1,
      -1,    -1,   884,    -1,   886,  1637,   139,  1639,    -1,   157,
      -1,    -1,   145,    -1,   147,   897,   101,    -1,   151,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   161,   162,
     163,    -1,    -1,   101,    -1,  1501,  1502,    -1,   106,   107,
     108,   109,   110,   111,   112,    -1,   179,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   936,   736,   737,    -1,   191,   192,
    1717,   129,   195,  1873,    -1,   746,    -1,    -1,   749,    -1,
      -1,  1537,    -1,    -1,    -1,   160,    -1,    -1,    -1,    -1,
      -1,   149,   150,    -1,    -1,   153,    -1,    -1,    -1,  1899,
      -1,    -1,   160,   161,    -1,   440,    -1,    -1,    -1,    -1,
      -1,    12,    13,    14,    15,    16,    -1,    -1,    -1,   242,
      -1,    -1,   245,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1003,    -1,    -1,   853,   258,    -1,  1937,    -1,   810,
      -1,    -1,    -1,   814,    -1,   864,    -1,   818,  1020,    -1,
      -1,    -1,   275,    -1,    -1,  1027,    -1,  1613,    -1,  1383,
     283,    -1,    -1,    -1,    -1,   288,  1595,   886,    -1,    70,
      -1,   294,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   302,
      -1,    -1,    -1,    -1,    -1,    -1,   521,    -1,  1644,    -1,
      -1,    -1,    -1,  1649,  1650,    -1,    -1,  1069,    -1,   322,
     101,   324,   325,   104,   105,   106,   107,   108,   109,   110,
     111,   112,    -1,    -1,    -1,    -1,   339,    -1,    -1,    -1,
     343,    -1,    -1,    -1,    73,    -1,  1873,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1109,    -1,   574,
      -1,    -1,    -1,    -1,    -1,  1117,    95,    -1,   149,   150,
      -1,    -1,  1899,   376,    -1,    -1,    -1,   592,   593,   160,
     161,    -1,    -1,    -1,    12,    13,    14,    15,    16,    -1,
      -1,    -1,    -1,    -1,  1146,    -1,    -1,    -1,    -1,    -1,
      -1,   616,    -1,    -1,    -1,    -1,    -1,   410,    -1,    -1,
    1937,  1163,  1164,    -1,   629,    -1,    -1,    -1,    -1,    -1,
      -1,   150,    -1,    -1,    -1,    -1,  1178,    -1,    -1,    -1,
      -1,    -1,  1184,   436,    -1,    -1,    -1,   440,    -1,    -1,
    1192,    -1,    70,   446,  1548,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1003,    -1,    -1,   101,    -1,  1561,   104,   105,
     106,   107,   108,   109,   110,   111,   112,    -1,  1220,    -1,
      -1,    -1,  1224,   101,    -1,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,    -1,    -1,    -1,    -1,    -1,
     219,   220,    -1,    -1,  1598,    -1,    -1,    -1,    -1,    -1,
      -1,   129,    -1,    -1,    -1,   720,   152,   236,    -1,    -1,
      -1,   157,   515,   516,    -1,    -1,    -1,   520,   521,    -1,
      -1,   149,   150,  1074,    -1,   153,  1077,    -1,  1837,    -1,
      -1,    -1,   160,   161,   101,  1287,  1288,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   761,    -1,    -1,   552,
      -1,    -1,    -1,    -1,   557,    -1,    -1,   560,   561,    -1,
     563,    -1,    -1,    -1,  1163,    -1,   295,   296,    -1,    -1,
      -1,   574,    -1,   576,  1326,  1327,  1328,  1329,  1330,  1331,
      -1,    -1,    -1,   150,  1336,  1337,   153,   590,    -1,   592,
     593,    -1,   595,    -1,  1698,    -1,    -1,  1196,  1702,    -1,
      -1,    -1,    -1,  1355,  1356,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1716,   616,    -1,    -1,   619,    -1,    -1,    -1,
     623,    -1,  1726,   626,    -1,    -1,   629,    -1,   631,    -1,
      -1,    -1,    -1,  1385,   637,    -1,    -1,    -1,    -1,    -1,
      -1,  1977,    -1,    -1,    70,    -1,    -1,   650,    -1,   652,
     653,    -1,   655,  1405,    -1,    -1,    -1,    -1,   661,    -1,
      -1,   664,   665,   666,    -1,    -1,    -1,    -1,    -1,   884,
      -1,    -1,   887,    -1,    -1,   101,    -1,  1429,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   132,   133,   134,
     135,   136,   137,   138,   139,   140,   141,   142,  1802,    -1,
      -1,   146,  1806,   129,    -1,  1809,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1264,    -1,   444,    -1,   720,    -1,    -1,
      -1,   936,  1273,   149,   150,    -1,    -1,   153,   173,    -1,
      -1,  1835,    -1,    -1,   160,   161,    -1,    -1,  1337,    -1,
      -1,   744,   101,    -1,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,   484,    -1,    -1,   761,    -1,
      -1,    -1,    -1,  1515,   493,    -1,    -1,  1519,    -1,  1521,
      -1,    -1,  1876,  1525,    -1,  1527,  1880,    -1,    -1,    -1,
     783,   784,    -1,    -1,   513,    -1,  1385,    -1,  1892,    -1,
     519,    -1,    -1,    12,    13,    14,    15,    16,   157,   802,
      -1,    -1,    -1,  1555,  1556,    -1,    -1,    -1,    -1,    -1,
     101,  1915,    -1,  1917,  1918,   106,   107,   108,   109,   110,
     111,   112,   113,    -1,    -1,    -1,   117,   830,   119,    -1,
     559,    -1,    -1,    -1,    -1,    -1,  1940,    -1,  1590,    -1,
      -1,   570,    -1,   572,    -1,    -1,    -1,    -1,    -1,  1601,
     853,    70,  1604,    -1,    -1,    -1,    -1,    -1,   861,   150,
      -1,   864,   153,    -1,    -1,   868,  1970,    -1,    -1,    -1,
      -1,   874,   875,    -1,    -1,    -1,  1980,  1629,    -1,    -1,
      -1,   884,   101,   886,   887,    -1,   889,   106,   107,   108,
     109,   110,   111,   112,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     1,  1654,  1655,     4,  2009,    -1,  2011,    -1,     1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,  1669,  1670,    -1,
      -1,    -1,    -1,  2027,    -1,    -1,    -1,    -1,    -1,  2033,
     149,   150,  1684,   936,    -1,  1687,    -1,    -1,    -1,  2043,
     101,   160,   161,   104,   105,   106,   107,   108,   109,   110,
     111,   112,  1704,    -1,    -1,    -1,  1555,    -1,    58,    -1,
      -1,    -1,    -1,    -1,  1515,  1717,    58,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1192,    -1,    -1,
      -1,    70,    82,    -1,    -1,    -1,    -1,    -1,   149,   150,
      -1,    -1,  1744,    -1,    -1,   156,   999,    -1,    -1,   160,
     161,    -1,   102,    -1,    -1,  1220,    -1,    -1,    -1,    -1,
     102,    -1,   101,  1016,  1017,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1786,   764,    -1,   766,    -1,   139,
     129,    -1,    -1,  1795,    -1,   145,    -1,    -1,    70,    -1,
      -1,    -1,    -1,   145,    -1,    -1,    -1,    -1,    -1,    -1,
     149,   150,   162,    -1,    -1,    -1,  1069,    -1,    -1,    -1,
     162,   160,   161,    -1,    -1,   804,    -1,    -1,  1830,   101,
     180,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   191,   192,    -1,   823,    -1,    -1,    -1,    -1,    -1,
     192,    -1,    -1,  1855,    -1,  1704,  1109,   129,    -1,    -1,
      -1,  1863,    -1,    -1,  1117,    -1,    -1,    -1,  1669,  1670,
     220,  1873,    -1,    -1,    -1,    -1,    -1,   149,   150,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   236,    -1,   160,   161,
      -1,   241,   242,  1146,    -1,   245,    -1,  1899,  1900,    -1,
    1902,  1903,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1163,  1164,    -1,    -1,    -1,    -1,    -1,   267,    -1,    -1,
     270,    -1,   272,    -1,    -1,   267,    -1,    -1,    -1,    -1,
      -1,  1184,    -1,   283,    -1,  1937,    -1,    -1,    -1,  1192,
    1405,   283,    -1,  1196,    -1,    -1,   296,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1208,    -1,  1959,  1960,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1220,    -1,    -1,
      -1,  1224,   322,    -1,    -1,   325,    -1,    -1,    -1,    -1,
     322,    -1,    -1,   325,    -1,  1786,    -1,    -1,    -1,   339,
      -1,    -1,    -1,   343,  1996,    -1,    -1,   339,    -1,   101,
      -1,   343,   104,   105,   106,   107,   108,   109,   110,   111,
     112,    -1,    -1,    -1,    -1,  1268,  1269,   367,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1282,
    1283,    -1,    -1,    -1,  1287,  1288,    -1,    -1,    -1,    -1,
      -1,  1020,    -1,    -1,    -1,    -1,    -1,  1026,  1027,    -1,
      -1,   153,    -1,    -1,  1903,    -1,    -1,    -1,  1859,    -1,
      -1,    -1,  1863,  1316,  1317,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1326,  1327,  1328,  1329,  1330,  1331,    -1,
      -1,    -1,    -1,    70,  1337,    -1,   436,    -1,    -1,    -1,
     440,    -1,    12,    13,    14,    15,    16,    -1,   440,    -1,
      -1,  1902,  1355,  1356,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   463,   101,    -1,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    -1,    -1,   101,    -1,
      -1,    -1,  1385,   106,   107,   108,   109,   110,   111,   112,
     113,    -1,   129,    -1,   117,    -1,   119,    -1,    -1,    -1,
      70,    -1,  1405,    -1,    -1,    -1,    -1,    -1,  1959,  1960,
      -1,    -1,   149,   150,    -1,    -1,     1,    -1,    -1,     4,
      -1,   521,    -1,   160,   161,    -1,  1429,   150,    -1,   521,
     153,   101,    -1,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,    -1,    -1,  1996,    -1,    -1,    -1,  1178,
      -1,    -1,   552,    -1,    -1,    -1,    -1,   557,    -1,   129,
     552,   561,    -1,   563,    -1,    -1,    -1,    -1,    -1,   561,
      -1,   563,    -1,    58,   574,    -1,   576,    -1,    -1,   149,
     150,    -1,   574,   153,   576,    12,    13,    14,    15,    16,
     160,   161,   592,   593,    -1,    -1,    -1,    82,  1501,  1502,
     592,   593,    -1,    -1,    -1,    -1,    -1,   607,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   616,   102,  1521,    -1,
      -1,   621,    -1,    -1,   616,    -1,    -1,    -1,    -1,   629,
      -1,    -1,    -1,    -1,  1537,    -1,    -1,   629,    -1,    -1,
      -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,  1277,    -1,
      -1,    -1,  1555,  1556,   139,    -1,    -1,    -1,    -1,    99,
     145,   101,   147,   103,   104,   105,   106,   107,   108,   109,
     110,   111,   112,    -1,   101,    -1,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    -1,  1590,    -1,    -1,
      -1,    -1,    -1,    -1,   179,    -1,    -1,    -1,  1601,    -1,
      -1,  1604,   129,    -1,    -1,    -1,   191,  1336,    -1,   149,
    1613,    -1,   152,   153,    -1,  1830,    -1,    -1,    -1,    -1,
     720,    -1,   149,   150,    -1,    -1,  1629,    -1,   720,    -1,
      -1,    -1,    -1,   160,   161,    -1,    -1,   737,    -1,    -1,
      -1,  1644,    -1,    -1,    -1,    -1,  1649,  1650,    -1,    -1,
      -1,  1654,  1655,    -1,    -1,    -1,    -1,   242,    -1,     1,
     245,   761,    -1,    -1,    -1,   250,   766,    -1,   101,   761,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      -1,    -1,    -1,   783,   784,  1900,    -1,    -1,    -1,    -1,
      -1,   783,   784,    -1,    -1,    -1,    -1,    -1,   283,    -1,
      -1,  1704,   802,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     802,    -1,    -1,    -1,  1717,    -1,    58,   302,   101,   152,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     830,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   830,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,   101,    -1,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     102,   861,    -1,    -1,    -1,    -1,   149,   150,   868,   861,
     153,    -1,    -1,    -1,   874,    -1,   868,   160,   161,    -1,
      -1,    -1,    -1,    -1,   884,    -1,    -1,   887,    -1,   889,
    1519,   376,   884,    -1,   894,   887,  1525,   889,  1527,   152,
      -1,    -1,    -1,   145,    -1,   101,    -1,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,    -1,    -1,    -1,
     162,    -1,    -1,    -1,    -1,   410,    -1,  1830,    -1,    -1,
      -1,    -1,    -1,   129,    -1,    -1,   936,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   936,    -1,    -1,    -1,    -1,    -1,
     192,   436,  1855,   149,   150,    -1,    -1,   153,    -1,    -1,
      -1,    -1,    -1,    -1,   160,   161,    -1,    -1,    -1,    -1,
    1873,    -1,    -1,    -1,    -1,    -1,    -1,   173,   101,    -1,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      -1,    -1,    -1,    -1,    -1,    -1,  1899,  1900,    -1,   999,
    1903,    -1,    -1,    -1,    -1,    -1,   129,   999,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1016,  1017,    -1,    -1,
      -1,    -1,    -1,    -1,  1016,  1017,   149,   150,    -1,    -1,
     515,   516,    -1,    -1,  1937,    -1,    -1,   160,   161,   101,
      -1,   283,   104,   105,   106,   107,   108,   109,   110,   111,
     112,    -1,    -1,    -1,    -1,  1684,    -1,    -1,  1687,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,  1069,
      -1,    -1,   557,    -1,  1977,    -1,   561,  1069,   563,    -1,
     322,    -1,    -1,   325,    -1,    -1,    -1,   149,   150,    -1,
      -1,   576,    -1,    -1,    -1,    -1,    -1,   339,   160,   161,
      -1,   343,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1109,
      -1,    -1,    -1,    -1,    -1,  1744,    -1,  1117,    -1,    -1,
      -1,    -1,    -1,   101,    -1,  1117,   104,   105,   106,   107,
     108,   109,   110,   111,   112,    -1,    -1,    -1,    55,    56,
      -1,    -1,    -1,    -1,    -1,    -1,  1146,    -1,    -1,    -1,
      -1,    -1,   637,    -1,  1146,    -1,    -1,    -1,    -1,     1,
      -1,    -1,     4,    -1,  1164,   650,  1795,   652,   653,    -1,
     655,   149,  1164,    90,    -1,    -1,   661,    -1,   101,   664,
     665,   666,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,    -1,  1192,    -1,   117,    -1,   119,    -1,   440,   101,
    1192,  1201,   104,   105,   106,   107,   108,   109,   110,   111,
     112,    -1,    -1,    -1,    -1,    -1,    58,    -1,    -1,    -1,
    1220,    -1,    -1,   140,    -1,    -1,   143,   150,  1220,    -1,
     153,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   156,
      82,    -1,    -1,    -1,    -1,   101,    -1,   149,   104,   105,
     106,   107,   108,   109,   110,   111,   112,    -1,    -1,   744,
     102,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1268,  1269,
      -1,    -1,    -1,  1902,    -1,    -1,  1268,  1269,    -1,   521,
      -1,    -1,  1282,  1283,    -1,    -1,    -1,  1287,  1288,    -1,
    1282,  1283,    -1,   210,    -1,  1287,  1288,   139,    -1,    -1,
      -1,    -1,    -1,   145,    -1,   147,    -1,    -1,    -1,    -1,
     552,    -1,    -1,    -1,    -1,    -1,  1316,  1317,    -1,   561,
      -1,   563,    -1,    -1,  1316,  1317,  1326,  1327,  1328,  1329,
    1959,  1960,   574,    -1,   576,    -1,    -1,   179,   255,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   265,   191,
     592,   593,    -1,    -1,    -1,  1355,  1356,    -1,    -1,   276,
      -1,    -1,    -1,  1355,  1356,    -1,    -1,  1996,    -1,    12,
      13,    14,    15,    16,   616,    -1,   861,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   303,   629,    -1,   874,
     875,    -1,    -1,   310,   311,    -1,    -1,    -1,   315,    -1,
     242,    -1,    -1,   245,    -1,  1405,    -1,    -1,   250,    -1,
      -1,    -1,    -1,  1405,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,  1429,
      -1,   348,    -1,    -1,    -1,    -1,   353,    -1,    -1,   356,
      -1,   283,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,
     302,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   720,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1501,  1502,    -1,    -1,    -1,   149,   150,    -1,  1501,
    1502,    -1,    -1,    -1,    -1,    -1,    -1,   160,   161,   761,
      -1,    -1,    -1,    -1,    -1,    -1,   443,  1527,    -1,    -1,
      -1,    -1,    -1,    -1,   376,    -1,    -1,  1537,   455,   456,
      -1,   783,   784,    -1,    -1,  1537,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1556,    -1,    -1,    -1,
     802,    -1,    -1,    -1,  1556,    -1,    -1,    -1,   410,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   830,    -1,
    1590,    -1,    -1,    -1,   436,    -1,    -1,    -1,  1590,    -1,
      -1,  1601,    -1,    -1,  1604,    -1,    -1,    -1,    -1,  1601,
      -1,    -1,    -1,  1613,    -1,    -1,    -1,    -1,    -1,   861,
      -1,  1613,    -1,    -1,  1109,    -1,   868,    -1,    -1,    -1,
      -1,    -1,  1117,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   884,    -1,  1644,   887,    -1,   889,    -1,  1649,
    1650,    -1,  1644,    -1,  1654,  1655,    -1,  1649,  1650,    -1,
      -1,  1146,  1654,  1655,    -1,    -1,    -1,    -1,    -1,   586,
    1670,    -1,    -1,   515,   516,    -1,    -1,    -1,    -1,  1164,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   936,    -1,    -1,    -1,    -1,  1184,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   557,    -1,  1717,   635,   561,
      -1,   563,    -1,    -1,    -1,  1717,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   576,    -1,    -1,    -1,    -1,  1224,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   999,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    86,    -1,    -1,    -1,    -1,
      -1,    92,    93,    -1,  1016,  1017,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   637,    -1,    -1,    -1,    -1,
       0,    -1,    -1,     3,    -1,   126,    -1,    -1,   650,    -1,
     652,   653,    -1,   655,    -1,    -1,    -1,    -1,    -1,   661,
      -1,    -1,   664,   665,   666,    -1,    -1,  1069,    -1,    -1,
    1830,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1830,    -1,
      -1,  1326,  1327,  1328,  1329,  1330,  1331,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1855,    -1,    -1,    -1,    -1,
      -1,   778,    -1,  1855,    -1,    -1,    -1,    -1,   785,    -1,
    1355,  1356,    -1,  1873,    -1,  1117,    76,    -1,    -1,    -1,
      -1,  1873,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1899,
    1900,    -1,   744,    -1,  1146,    -1,    -1,  1899,  1900,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1164,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   135,    -1,  1937,    -1,    -1,
      -1,    -1,    -1,   860,  1429,  1937,    -1,    -1,    -1,    -1,
    1192,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1960,    -1,    -1,    -1,    -1,   882,    -1,    -1,    -1,    -1,
      -1,   292,    -1,    -1,    -1,    -1,    -1,  1977,  1220,    -1,
      -1,    -1,    -1,    -1,    -1,  1977,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   912,    -1,    -1,    -1,   916,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   861,
      -1,    -1,    -1,    -1,    -1,    -1,  1268,  1269,    -1,   229,
      -1,    -1,   874,   875,    -1,    -1,  1521,    -1,    -1,    -1,
    1282,  1283,    -1,    -1,   244,  1287,  1288,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   254,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   264,    -1,    -1,    -1,    -1,    -1,
      -1,  1556,    -1,    -1,  1316,  1317,    -1,    -1,   278,   279,
      -1,    -1,    -1,    -1,    -1,   285,   286,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   301,    -1,    -1,    -1,  1590,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1355,  1356,    -1,    -1,    -1,   439,  1604,
     441,   321,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   450,
     451,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1629,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1405,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    12,    13,    14,    15,    16,    -1,   377,    19,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,  1122,  1123,  1124,   408,    50,
      51,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,
      -1,    82,  1717,    -1,   434,  1152,    -1,   558,   438,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1167,    -1,    -1,    -1,    -1,    -1,    -1,   457,    -1,  1501,
    1502,   461,   462,    -1,    -1,   465,    -1,  1109,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1117,    -1,    -1,    -1,    -1,
     480,   481,   482,   483,    -1,    -1,    -1,    -1,  1205,    -1,
      -1,    -1,    -1,    -1,    -1,  1537,   147,    -1,    -1,   499,
      -1,    -1,    -1,    -1,  1146,    -1,    -1,   507,    -1,    -1,
      -1,   162,    -1,    -1,  1556,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1164,    -1,    -1,    -1,    -1,    -1,   179,    -1,
      -1,    -1,    -1,    -1,    -1,   535,    -1,    -1,    -1,    -1,
      -1,   192,  1184,    -1,    -1,    -1,    -1,    -1,  1590,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1601,
      -1,    -1,    -1,    -1,    -1,    -1,   566,    -1,    -1,    -1,
      -1,  1613,    -1,   573,    -1,    -1,    -1,    -1,    -1,   579,
      -1,    -1,  1224,    -1,    -1,    -1,    -1,    -1,  1873,    -1,
      -1,    -1,    -1,    -1,   245,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1644,   603,   604,    -1,    -1,  1649,  1650,    -1,
      -1,    -1,  1654,  1655,  1899,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1340,    -1,    -1,  1343,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   302,  1937,    -1,    -1,    -1,   777,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   667,    -1,    -1,
      -1,   322,    -1,    -1,    -1,  1717,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1326,  1327,  1328,  1329,  1330,  1331,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    47,    -1,    -1,    -1,
      -1,    -1,    -1,  1355,  1356,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,   854,   855,    -1,   736,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   865,   866,   867,    -1,    -1,   870,
      -1,   751,    -1,    -1,    -1,   755,    -1,    -1,    -1,   410,
      -1,    -1,    -1,    -1,   764,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   119,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   786,  1429,  1830,   440,
      -1,   132,    -1,   134,    -1,   795,    -1,    -1,    -1,    -1,
      -1,   801,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1855,    -1,    -1,    -1,  1534,    -1,    -1,
      -1,    -1,    -1,   164,    -1,    -1,    -1,    -1,    -1,   950,
      -1,  1873,    -1,    -1,    -1,    -1,    -1,    -1,   838,    -1,
     181,    -1,    -1,    -1,    -1,   845,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1899,  1900,    -1,
      -1,    -1,    -1,    -1,   515,   516,    -1,    -1,    -1,    -1,
     521,    -1,   872,    -1,    -1,   996,    -1,    -1,   219,  1521,
      -1,    -1,   223,    -1,    -1,   226,   227,    -1,    -1,   230,
      -1,    -1,   233,   234,    -1,  1937,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1621,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1556,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1043,   574,    -1,    -1,   926,    -1,    -1,    -1,
      -1,  1052,  1053,  1054,  1055,  1977,    -1,    -1,    -1,  1060,
    1061,    -1,   593,    -1,    -1,    -1,    -1,    -1,  1590,  1070,
      -1,    -1,    -1,    -1,   295,    -1,    -1,   298,    -1,    -1,
      -1,    -1,  1604,    -1,    -1,   616,    -1,    -1,    -1,    -1,
    1091,    -1,  1093,    -1,    -1,    -1,    -1,    -1,   319,   320,
      -1,    -1,    -1,    -1,    -1,    -1,   637,  1629,    -1,    -1,
      -1,   178,    -1,    -1,   335,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1009,
      -1,    -1,    -1,  1013,    -1,    -1,    -1,    -1,    -1,    -1,
    1020,    -1,    -1,    -1,    -1,  1146,  1743,    -1,    -1,    -1,
    1030,   376,    -1,    -1,    -1,  1752,    -1,  1037,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1046,    -1,  1048,    -1,
      -1,  1172,    -1,    -1,    -1,    -1,    -1,    -1,  1179,    -1,
    1181,  1182,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   720,
    1191,    -1,  1193,    -1,  1195,  1717,  1197,    -1,    -1,    -1,
    1080,  1202,    -1,    -1,  1084,    -1,    -1,    -1,   429,    -1,
      -1,    -1,    -1,   744,    -1,    -1,    -1,    -1,  1098,    -1,
      -1,  1101,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     761,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   783,   784,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1263,   484,    -1,    -1,    -1,    -1,    -1,  1270,
    1271,   802,    -1,    -1,    -1,   496,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     515,   516,    -1,  1294,   361,    -1,    -1,   364,   365,    -1,
    1301,    -1,    -1,    -1,  1305,    -1,    -1,   374,   375,    -1,
    1190,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   389,   390,    -1,    -1,    -1,    98,    -1,    -1,
      -1,    -1,    -1,    -1,  1335,  1215,    -1,   868,   109,    -1,
     111,    -1,   113,   410,   875,    -1,    -1,    -1,    -1,   570,
      -1,  1873,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   440,  1375,    -1,    -1,  1899,    -1,    -1,
     151,    -1,   153,   154,    -1,    -1,    -1,    -1,    -1,    -1,
     611,   612,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   624,    -1,   936,    -1,    -1,  1409,    -1,
      -1,    -1,    -1,    -1,    -1,  1937,  1417,    -1,  1419,    -1,
      -1,    -1,    -1,    -1,   195,   650,    -1,    -1,  1308,    -1,
     655,    -1,  1312,    -1,    -1,    -1,   661,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   680,    -1,    -1,    -1,    -1,
      -1,    -1,  1342,    -1,    -1,  1466,  1467,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1481,  1482,    -1,  1484,    -1,    -1,    -1,   258,    -1,   260,
     261,   716,  1493,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1503,  1504,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1392,    -1,    -1,  1395,    -1,   288,    -1,   740,
     741,    -1,    -1,   294,    -1,   746,    -1,    -1,    -1,    -1,
      -1,  1411,    -1,    -1,    -1,    -1,    -1,    -1,  1069,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   767,    -1,    -1,   770,
     771,    -1,   773,   324,   775,   776,    -1,    -1,    -1,   330,
      -1,   332,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1462,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1471,    -1,   814,    -1,  1475,    -1,   818,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1489,
    1490,    -1,    -1,  1614,  1615,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1625,    -1,    -1,    -1,    -1,    -1,
     697,   698,   699,   700,   701,   702,   703,   704,   705,   706,
     707,   708,   709,   710,   711,   712,   713,   714,   715,   420,
      -1,    -1,    -1,  1184,    -1,    -1,    -1,    -1,    -1,  1660,
    1661,  1192,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   895,   446,    -1,   448,   449,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1220,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     777,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   493,    -1,    -1,  1605,  1606,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   513,    -1,    -1,  1746,    -1,   518,    -1,   520,
      -1,    -1,    -1,    -1,    -1,    -1,  1287,  1288,    -1,    -1,
      -1,    -1,    -1,    -1,  1765,    -1,    -1,  1768,  1769,   540,
      -1,   542,   543,    -1,  1775,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   560,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   572,    -1,    -1,    -1,  1026,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   595,    -1,   597,   598,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1067,   914,   619,   620,
      -1,  1731,    -1,  1074,    -1,   626,  1077,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   932,    -1,    -1,    -1,    -1,
     937,    -1,    -1,    -1,  1405,  1755,    -1,    -1,    -1,    -1,
      -1,   948,    -1,    -1,    -1,    -1,    -1,    -1,   659,   660,
      -1,    -1,    -1,  1773,    -1,    -1,    -1,    -1,    -1,    -1,
    1901,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     1,    -1,    -1,    -1,    -1,    -1,
    1800,    -1,    -1,   990,    -1,    -1,    -1,    -1,   162,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1826,    -1,    -1,  1829,
      -1,    -1,    -1,    -1,    -1,    -1,  1957,   191,   192,    -1,
      -1,    48,    -1,    -1,    -1,    52,    -1,    54,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1198,    -1,    -1,
    1981,    -1,    -1,    -1,    71,  1206,  1207,    -1,    -1,   223,
    1521,  1992,    -1,    -1,    -1,    -1,   230,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2007,    -1,    -1,    -1,
      -1,    98,    99,    -1,   101,    -1,   103,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,    -1,
     117,   118,   119,    -1,   121,   122,    -1,    -1,    -1,    -1,
      -1,    -1,   129,  1264,    -1,    -1,  1926,    -1,    -1,    -1,
    1117,    -1,  1273,    -1,    -1,  1276,    -1,  1278,  1279,    -1,
      -1,    -1,   149,    -1,   298,   152,   153,    -1,    -1,    -1,
    1601,    -1,   159,   160,   161,   162,   163,   164,   165,    -1,
      -1,    -1,   853,    -1,    -1,    -1,    -1,    -1,   322,   323,
      -1,    -1,    -1,   864,    -1,    -1,    -1,    -1,  1319,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   343,
      -1,    -1,    -1,    -1,    -1,   886,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1654,  1655,  1192,   897,  1352,    -1,    -1,
    1355,  1356,    -1,    -1,    -1,   906,  1361,    -1,    -1,    -1,
    1365,    -1,  1367,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1227,  1228,  1229,    -1,    -1,  1386,    -1,  1234,  1235,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   413,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1258,    -1,    -1,    -1,   429,   430,    -1,   432,   433,
      -1,    -1,    -1,    -1,    -1,    -1,   440,    -1,    -1,    -1,
     444,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1295,  1296,
      -1,    -1,  1003,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,    -1,  1470,
      -1,   485,    -1,    -1,    -1,   489,  1027,    -1,    98,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1499,    -1,
      -1,    -1,  1507,    -1,    -1,    -1,    -1,   521,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1525,    -1,    -1,   147,    -1,    -1,
    1531,   151,    -1,    -1,  1539,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   162,    -1,  1855,    -1,  1551,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   571,    -1,   179,
     574,  1566,  1567,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   192,    -1,    -1,   195,    -1,    -1,   592,   593,
      -1,    -1,    -1,    -1,    -1,  1590,    -1,    -1,    -1,   603,
      -1,    -1,    -1,   607,    -1,    -1,    -1,    -1,    -1,  1600,
     614,    -1,   616,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1163,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   245,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   258,    -1,
      -1,    -1,    -1,    -1,    -1,  1196,    -1,    -1,    -1,    -1,
      -1,  1202,    47,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   294,  1676,  1677,    -1,    -1,    -1,
      -1,    -1,   302,  1684,    -1,    -1,    -1,  1688,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1700,    -1,    -1,    -1,    -1,
      -1,    -1,   322,    -1,   324,  1710,   720,    -1,  1713,    -1,
    1715,    -1,  1717,    -1,    -1,    -1,    -1,  1722,    -1,    -1,
      -1,    -1,   736,   737,   119,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   746,   747,    -1,   749,   750,   132,    -1,   134,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   761,  1595,    -1,
     764,    -1,   766,   767,    -1,    -1,   376,    -1,    -1,   773,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   783,
     784,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1337,  1788,   802,    -1,
     410,    -1,   806,    -1,    -1,    -1,   810,    -1,    -1,    -1,
     814,   815,    -1,    -1,   818,   819,    -1,    -1,    -1,    -1,
    1815,    -1,   826,    -1,    -1,  1820,    -1,  1822,    -1,    -1,
     440,    -1,    -1,    -1,    -1,    -1,   446,    -1,    -1,    -1,
      -1,   226,   227,    -1,  1385,   230,    -1,    -1,   233,   234,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   868,   869,    -1,    -1,  1859,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1881,    -1,    -1,  1884,
      -1,  1886,    -1,   897,    -1,  1890,  1891,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   515,   516,    -1,    -1,    -1,
      -1,   521,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   936,    -1,   319,   320,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     335,    -1,    -1,    -1,    -1,    -1,    -1,  1952,  1953,  1954,
      -1,  1798,    -1,    -1,   574,    -1,    -1,    -1,  1959,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1973,    -1,
      -1,    -1,    -1,   593,    -1,   595,    -1,    -1,    -1,    -1,
    1985,  1986,  1987,    -1,    -1,    -1,    -1,    -1,    -1,  1003,
    1837,    -1,    -1,    -1,    -1,    -1,   616,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1555,    -1,  1020,  1021,    -1,    -1,
      -1,    -1,    -1,  1027,    -1,    -1,    -1,   637,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     650,    -1,   652,   653,   429,   655,    -1,    -1,    -1,    -1,
      -1,   661,    -1,    -1,   664,   665,   666,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1069,    -1,    -1,    -1,    -1,
    1074,  1075,    -1,  1077,  1078,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1931,    -1,  1933,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     720,   496,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1966,
      -1,    -1,    -1,    -1,   744,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1687,    -1,    -1,    -1,
      -1,   761,    -1,    -1,    -1,    -1,    -1,  1994,    -1,    -1,
      -1,    -1,    -1,  1704,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   783,   784,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1192,    -1,
      -1,    -1,   802,    -1,  1198,  1199,    -1,    -1,    -1,    -1,
      -1,    -1,  2039,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1220,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   611,   612,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   624,
      -1,    -1,    -1,    -1,    -1,  1786,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   179,    -1,    -1,    -1,    -1,   868,    -1,
    1264,  1265,    -1,    -1,    -1,   875,   192,    -1,    -1,  1273,
    1274,    -1,  1276,    -1,    -1,    -1,   886,    -1,    -1,   205,
      -1,   207,    -1,  1287,  1288,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       5,    -1,    -1,    -1,    -1,    -1,    -1,    12,    13,    14,
      15,    16,    -1,    -1,    -1,    -1,   936,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    -1,   740,   741,    52,    -1,    54,
      -1,   746,  1903,    -1,    -1,    -1,    -1,   293,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    70,    71,    -1,    -1,    -1,
      -1,    -1,   767,    -1,    -1,   770,   771,    -1,   773,    -1,
     775,   776,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1405,    -1,    98,    99,    -1,   101,    -1,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,   117,   118,   119,    -1,   121,   122,    -1,   814,
      -1,    -1,    -1,   818,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   149,  1996,    -1,   152,   153,  1069,
      -1,    -1,    -1,    -1,   159,   160,   161,   162,   163,   164,
     165,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1109,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     895,  1515,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1531,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     466,    -1,    -1,    -1,    -1,    -1,   472,    -1,    -1,    -1,
      -1,   477,    -1,  1163,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1184,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1192,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1601,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1220,    -1,    -1,    -1,  1224,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   564,    -1,
      -1,  1026,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1654,  1655,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1669,  1670,   593,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1287,  1288,    -1,
     606,  1685,  1067,    -1,    -1,    -1,    -1,    -1,    -1,  1074,
      -1,    -1,  1077,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1326,  1327,  1328,    -1,
    1330,  1331,    -1,    -1,    -1,    -1,    -1,  1337,    -1,    -1,
      -1,   657,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   678,   679,    -1,    -1,   682,    -1,   684,    -1,
      -1,    -1,    -1,    -1,   690,    -1,   692,   693,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1385,    -1,    -1,    -1,    -1,
      -1,    -1,  1786,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1794,    -1,    -1,    -1,   720,  1405,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   733,    -1,    -1,
      -1,    -1,    -1,  1198,    -1,    -1,    -1,    -1,   744,    -1,
      -1,  1206,  1207,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   758,    -1,    -1,   761,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1855,    -1,    -1,    -1,  1859,  1860,    -1,    -1,  1863,
      -1,    -1,   788,    -1,    -1,   791,    48,    -1,    -1,    -1,
      52,    -1,    54,    -1,    -1,    -1,    -1,    -1,    -1,  1264,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1273,    71,
      -1,  1276,    -1,  1278,  1279,    -1,    -1,    -1,  1902,    -1,
      -1,   827,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1521,    -1,    -1,    -1,    -1,    98,    99,    -1,   101,
      -1,   103,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,  1319,   117,   118,   119,    -1,   121,
     122,    -1,    -1,    -1,    -1,  1555,    -1,   129,    -1,   875,
      -1,    -1,    -1,    -1,    -1,  1959,  1960,    -1,    -1,    -1,
     886,   887,    -1,    -1,    -1,    -1,    -1,   149,   894,    -1,
     152,   153,    -1,    -1,    -1,   157,    -1,   159,   160,   161,
     162,   163,   164,   165,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1601,  1996,    -1,    -1,    -1,    -1,   923,    -1,    -1,
      -1,  1386,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     936,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   944,  1629,
      -1,    -1,    -1,    -1,    -1,   951,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1654,  1655,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   997,    -1,    -1,    -1,     1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1470,    -1,    -1,    -1,    -1,
      -1,    17,    -1,    -1,  1704,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1499,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    48,    -1,    -1,    -1,    52,    -1,    54,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1064,    -1,
    1066,    -1,  1068,    69,    -1,    71,    72,    -1,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    91,    92,    93,    94,    -1,
      96,    -1,    98,    99,    -1,   101,    -1,   103,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,    -1,   121,   122,    -1,    -1,    -1,
      -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,  1134,  1135,
      -1,    -1,    -1,    -1,    -1,  1600,    -1,    -1,    -1,    -1,
    1830,    -1,   148,   149,    -1,    -1,   152,   153,    -1,    -1,
      -1,   157,    -1,   159,   160,   161,   162,   163,   164,   165,
      -1,    -1,    -1,    -1,    -1,  1855,    -1,    -1,   174,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1196,    -1,    -1,    -1,    -1,    -1,  1202,    -1,    17,    -1,
      -1,    -1,    -1,    -1,    -1,  1211,    -1,    -1,    -1,    -1,
    1900,  1676,  1677,  1903,  1220,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1688,    -1,    -1,    -1,    -1,    -1,    48,
      -1,  1237,    -1,    52,    -1,    54,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1252,    -1,    -1,  1255,
      69,    -1,    71,    72,    -1,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,    87,    88,
      89,    90,    91,    92,    93,    94,    -1,    96,    -1,    98,
      99,    -1,   101,    -1,   103,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,  1307,   121,   122,    -1,    -1,    -1,    -1,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1788,    -1,    -1,    -1,    -1,    -1,   148,
     149,    -1,    -1,   152,   153,    -1,    -1,    -1,   157,  1345,
     159,   160,   161,   162,   163,   164,   165,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   174,    -1,  1363,    -1,    -1,
    1366,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    17,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1859,    -1,    -1,    -1,    -1,  1405,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1415,
    1416,    -1,    -1,    -1,    -1,    -1,    -1,    48,    -1,    -1,
      -1,    52,    -1,    54,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1441,    -1,  1443,    69,    -1,
      71,    72,    -1,    74,    -1,    -1,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    90,
      91,    92,    93,    94,    -1,    96,    -1,    98,    99,    -1,
     101,    -1,   103,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,    48,
     121,   122,    -1,    52,    -1,    54,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    71,    -1,    -1,  1521,    -1,    -1,   149,    -1,
    1526,   152,   153,    -1,    -1,    -1,   157,    -1,   159,   160,
     161,   162,   163,   164,   165,    -1,    -1,    -1,    -1,    98,
      99,    -1,   101,   174,   103,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,    -1,   117,   118,
     119,    -1,   121,   122,    -1,    -1,    -1,    -1,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1586,    -1,    -1,    -1,   143,   144,   145,    -1,    -1,    -1,
     149,   150,    -1,   152,   153,    -1,    -1,    -1,    -1,    -1,
     159,   160,   161,   162,   163,   164,   165,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   174,    -1,    -1,    -1,    -1,
      -1,  1627,    -1,    -1,  1630,     3,     4,     5,     6,     7,
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
     148,   149,    -1,    -1,   152,   153,    -1,    -1,    -1,   157,
      -1,   159,   160,   161,   162,   163,   164,   165,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   174,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      -1,    19,  1838,    21,    22,    23,    24,    25,    26,    27,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   143,   144,   145,
      -1,    -1,    -1,   149,   150,   151,   152,   153,    -1,    -1,
      -1,    -1,    -1,   159,   160,   161,   162,   163,   164,   165,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   174,     3,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   143,
     144,   145,    -1,    -1,    -1,   149,   150,    -1,   152,   153,
      -1,    -1,    -1,    -1,    -1,   159,   160,   161,   162,   163,
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
      -1,    -1,    -1,   148,    -1,    -1,    -1,   152,   153,    -1,
      -1,    -1,    -1,    -1,    -1,   160,   161,    -1,    -1,    98,
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
      -1,    -1,    -1,    12,    13,    14,    15,    16,    -1,    -1,
      19,    70,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    -1,    -1,    -1,
      -1,    50,    51,    -1,    -1,   104,   105,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,    -1,    -1,
      -1,    -1,   101,   152,   153,    -1,    12,    13,    14,    15,
      16,   160,   161,    19,    -1,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    -1,    49,    50,    51,    -1,    53,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   156,    -1,    -1,
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
      69,    -1,    71,    -1,    -1,    74,    -1,    -1,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,    87,    88,
      89,    90,    91,    92,    93,    94,    -1,    96,    -1,    98,
      99,    -1,   101,    -1,   103,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,    -1,   121,   122,    -1,    -1,    -1,    -1,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     149,    -1,    -1,   152,   153,    -1,    -1,    -1,   157,    -1,
     159,   160,   161,   162,   163,   164,   165,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   174,     4,     5,     6,     7,
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
      56,    57,    58,    59,    60,    61,    62,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,
      -1,   129,    -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   151,   152,   153,    -1,    -1,   104,   105,
      -1,    -1,   160,   161,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    -1,    19,
      -1,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,   152,    -1,    -1,    -1,
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
      -1,   151,   152,   153,    -1,    -1,   104,   105,    -1,    -1,
     160,   161,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    -1,    19,    -1,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,   152,    -1,    -1,    -1,    50,    51,
      -1,    -1,    -1,    55,    56,    57,    58,    59,    60,    61,
      62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   104,   105,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,   131,
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
      40,    41,    42,    43,    44,    45,    -1,    -1,    -1,    -1,
      50,    51,    -1,    -1,   104,   105,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   101,   152,   153,    -1,    -1,    -1,    -1,    -1,    -1,
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
     152,    49,    50,    51,    -1,    53,    -1,    -1,    -1,    -1,
      48,    -1,    -1,    -1,    52,    -1,    54,    -1,    -1,    -1,
      -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   104,   105,    -1,    -1,
      98,    99,    -1,   101,    -1,   103,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,    -1,   117,
     118,   119,    -1,   121,   122,    -1,    48,    -1,    -1,    -1,
      52,   129,    54,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   152,   143,   144,   145,    -1,    71,
      -1,   149,   150,    -1,   152,   153,    -1,    -1,    -1,    -1,
      -1,   159,   160,   161,   162,   163,   164,   165,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    98,    99,    -1,   101,
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
      -1,    71,    -1,    -1,    -1,    -1,    -1,   149,   150,    -1,
     152,   153,    -1,    -1,    -1,    -1,    -1,   159,   160,   161,
     162,   163,   164,   165,    -1,    -1,    -1,    -1,    98,    99,
      -1,   101,    -1,   103,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,    -1,   117,   118,   119,
      48,   121,   122,    -1,    52,    -1,    54,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,   149,
      -1,    -1,   152,   153,    -1,    -1,   156,    -1,    -1,   159,
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
      -1,   149,   150,    -1,   152,   153,    -1,    -1,    -1,    -1,
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
     162,   163,   164,   165,    12,    13,    14,    15,    16,    -1,
      -1,    19,    -1,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      -1,    49,    50,    51,    -1,    53,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    12,    13,    14,    15,    16,    -1,
      -1,    19,    70,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      -1,    49,    50,    51,    -1,    53,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    70
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_int16 yystos[] =
{
       0,   176,   387,   388,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    19,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    50,    51,    55,    56,    57,
      58,    59,    60,    61,    62,    63,    67,    70,    71,    96,
     100,   101,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   116,   129,   149,   150,   152,   153,   160,   161,   179,
     180,   181,   196,   279,   280,   281,   282,   283,   284,   285,
     286,   287,   288,   289,   290,   292,   294,   296,   297,   298,
     299,   300,   301,   302,   303,   304,   306,   308,   309,   310,
     312,   313,   317,   318,   319,   320,   321,   323,   329,   330,
     331,   332,   343,   346,   379,   382,   392,   398,   400,   406,
     410,   415,   416,   417,   418,   419,   420,   421,   422,   442,
     459,   460,   461,   462,     0,   176,   180,   196,   283,   285,
     294,   297,   309,   313,   318,   115,   149,    56,    59,    60,
      62,   149,   149,   404,   405,   406,   305,   306,   104,   105,
     180,   359,   380,   381,   359,   149,   392,   149,   149,   149,
     196,   405,   410,   416,   417,   418,   420,   421,   422,   104,
     320,   154,   176,   286,   294,   297,   415,   419,   458,   459,
     462,   463,   174,   177,   146,   157,   173,   217,   362,    87,
     155,   399,   359,   177,   177,   177,   174,   104,   105,   149,
     196,   291,   401,   410,   411,   412,   413,   414,   415,   419,
     423,   424,   425,   426,   427,   433,     3,    46,    47,    49,
      53,   311,     3,     4,   153,   196,   285,   298,   302,   304,
     314,   319,   395,   415,   419,   462,   283,   285,   297,   309,
     313,   318,   396,   415,   419,    63,   303,   303,   298,   304,
     303,   298,   303,   298,   152,   404,   155,   177,   149,   157,
     225,   404,   404,   176,   274,   275,   153,   294,   297,   460,
     359,   359,   392,   173,   297,   149,   196,   401,   410,   415,
     424,   153,   196,   462,   393,   394,    63,    64,    65,    66,
     153,   171,   359,   368,   370,   374,   376,   377,   319,    55,
     151,   153,   196,   293,   297,   301,   302,   308,   309,   315,
     316,   317,   318,   322,   329,   330,   346,   355,   357,   442,
     454,   455,   456,   457,   462,   463,   104,   105,   157,   180,
     319,   433,   406,   149,   375,   376,   149,   149,   115,   182,
     183,    48,    52,    54,    71,    98,    99,   101,   103,   113,
     114,   117,   118,   119,   121,   122,   149,   153,   159,   162,
     163,   164,   165,   178,   179,   182,   184,   187,   195,   196,
     197,   198,   201,   202,   203,   204,   205,   206,   207,   208,
     209,   210,   211,   212,   213,   219,   319,   151,   153,   195,
     196,   212,   214,   294,   319,   360,   361,   378,   458,   463,
     297,   416,   417,   418,   420,   421,   422,   151,   151,   151,
     151,   151,   151,   151,   153,   294,   442,   460,   153,   160,
     196,   214,   285,   286,   293,   295,   297,   309,   316,   318,
     350,   351,   354,   355,   356,   454,   462,   149,   415,   419,
     462,   149,   155,   101,   152,   153,   157,   179,   181,   214,
     363,   364,   365,   366,   367,    21,   363,   149,   359,   225,
     149,   155,   155,   155,   405,   410,   412,   413,   414,   423,
     425,   426,   427,   297,   411,   424,   155,    96,   403,   153,
     404,   441,   442,   404,   404,   399,   274,   149,   404,   441,
     399,   404,   404,   297,   401,   149,   149,   296,   297,   294,
     297,   176,   294,   458,   463,   321,   157,   399,   274,   359,
     362,   285,   302,   397,   415,   419,   157,   399,   274,   380,
     297,   309,   297,   297,   104,   320,   104,   105,   180,   319,
     324,   380,   176,   180,   358,   148,   176,     3,   290,   292,
     297,   301,   225,   176,   176,   403,   149,   403,   177,   214,
     405,   410,   297,   149,   176,   359,   390,   157,   359,   157,
     359,   131,   160,   161,   373,   151,   155,   359,   377,   151,
     404,   404,   154,   176,   295,   297,   309,   316,   318,   453,
     454,   462,   463,   149,   153,   161,   173,   196,   442,   443,
     444,   445,   446,   447,   448,   465,   196,   322,   462,   297,
     316,   303,   298,   404,   151,   295,   297,   455,   295,   442,
     455,     9,   347,   359,   344,   157,   368,   173,   368,    12,
      86,   101,   104,   105,   179,   407,   408,   409,   151,   115,
     149,   195,   149,   149,   198,   149,   195,   149,   101,   297,
     310,   149,   195,   195,    18,    20,    83,   153,   162,   163,
     199,   200,   214,   221,   225,   332,   360,   462,   155,   176,
     149,   184,   153,   158,   153,   158,   118,   120,   121,   122,
     149,   152,   153,   157,   158,   198,   198,   166,   160,   167,
     168,   162,   163,   123,   124,   125,   126,   169,   170,   127,
     128,   161,   159,   171,   129,   130,   172,   151,   155,   152,
     176,   132,   133,   134,   135,   136,   137,   138,   139,   140,
     141,   142,   173,   216,   217,   218,   149,   196,   437,   438,
     439,   440,   441,   151,   155,   151,   151,   151,   151,   151,
     151,   149,   404,   441,   442,   149,   441,   442,   176,   294,
     460,   176,   177,   177,   149,   161,   196,   410,   428,   429,
     430,   431,   432,   433,   434,   435,   436,   131,   462,   177,
     177,   359,   359,   176,   176,   176,   153,   181,   176,   364,
     156,   155,   464,   363,   152,   153,   156,   367,   150,   214,
     220,   149,   176,   176,   176,   176,   410,   412,   413,   414,
     423,   425,   426,   427,   151,   151,   151,   151,   151,   151,
     151,   411,   424,   404,   149,   362,   154,   176,   225,   399,
     176,   225,   401,   221,   361,   221,   361,   401,   390,   225,
     399,   403,   157,   399,   274,   390,   225,   399,   326,   327,
     325,   157,   131,   297,   352,   353,   356,   357,   151,   155,
      68,   276,   277,   177,   297,   290,   160,   214,   176,   410,
     351,   392,   390,   154,   176,   149,   372,   370,   371,    76,
     307,   180,   157,   295,   442,   455,   297,   301,   462,   176,
     444,   445,   446,   154,   176,    17,   214,   297,   443,   465,
     404,   404,   442,   295,   453,   463,   297,   180,   404,   295,
     455,   319,   155,   464,   173,   348,   157,   347,   151,   361,
     151,   151,   155,   149,   174,   360,   153,   360,   360,   360,
     214,   360,   151,   360,   360,   360,   176,   151,   162,   163,
     200,    17,   299,   151,   155,   151,   160,   161,   151,   220,
     214,   157,   214,   180,   214,   180,   113,   153,   180,   150,
     188,   189,   190,   214,   113,   153,   180,   332,   214,   188,
     180,   198,   201,   201,   201,   202,   202,   203,   203,   204,
     204,   204,   204,   205,   205,   206,   207,   208,   209,   210,
     156,   221,   174,   182,   153,   180,   214,   157,   214,   176,
     438,   439,   440,   297,   437,   404,   404,   214,   361,   149,
     404,   441,   442,   149,   441,   442,   176,   176,   154,   154,
     149,   410,   429,   430,   431,   434,    17,   297,   428,   432,
     149,   404,   447,   465,   404,   404,   465,   149,   404,   447,
     404,   404,   177,   213,   359,   154,   155,   154,   155,   465,
     465,   131,   349,   350,   351,   349,   359,   176,   212,   213,
     214,   402,   464,   363,   365,   148,   176,   151,   155,   176,
     349,   180,   401,   180,   151,   151,   151,   151,   151,   151,
     149,   404,   441,   442,   149,   404,   441,   442,   401,   182,
     442,   214,   225,   352,   151,   151,   151,   151,   388,   389,
     225,   390,   225,   399,   389,   225,   157,   157,   157,   333,
     177,   177,   180,   278,   359,    17,    69,    71,    74,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    90,    91,    92,    93,    94,    96,   104,   105,   116,
     176,   221,   222,   223,   224,   225,   226,   227,   229,   230,
     240,   246,   247,   248,   249,   250,   251,   256,   257,   263,
     264,   265,   279,   297,   301,   359,   400,    68,   174,   177,
     177,   177,   349,   177,   391,   389,   283,   285,   294,   383,
     384,   385,   386,   378,   173,   369,   369,   347,   295,   455,
     153,   160,   196,   214,   319,   214,   297,   352,   151,   151,
     151,     5,   297,   404,   443,   157,   180,   433,     9,   359,
     148,   157,   213,   347,   464,   157,   151,   408,   188,   151,
     176,   155,   151,   151,   155,   151,   198,   151,   151,   151,
     198,    17,   299,   214,   151,   151,   150,   157,   198,   154,
     177,   188,   154,   154,   113,   117,   119,   181,   191,   192,
     193,   151,   155,   191,   154,   155,   148,   212,   156,   151,
     191,   177,   364,   352,   151,   151,   151,   437,   176,   176,
     352,   352,   434,   151,   151,   151,   151,   149,   410,   433,
     428,   432,   176,   176,   154,   177,   465,   176,   176,   177,
     177,   177,   177,   362,   191,   131,   165,   177,   177,   148,
     363,   214,   404,   150,   214,   349,   177,   173,   149,   404,
     441,   442,   149,   404,   441,   442,   176,   176,   403,   151,
     177,   177,   391,   389,   225,   391,   333,   333,   333,     3,
       9,    71,   148,   280,   287,   288,   294,   297,   334,   339,
     458,   151,   155,   155,   174,   149,    59,    60,   174,   225,
     279,   400,   149,    17,   223,   149,   149,   174,   359,   174,
     359,   160,   359,   157,   222,   149,   149,   149,   225,   214,
     215,   215,    13,   266,    72,   231,   174,   177,   227,    76,
     174,   359,    89,   252,   358,   297,   156,   278,   174,   154,
     154,   177,   155,   391,   401,   177,   174,   177,   174,   177,
     151,   361,   375,   375,   464,   176,   177,   177,   177,   214,
     177,   149,   404,   447,   442,   296,     5,   160,   177,   214,
     347,   404,   404,   319,   348,   364,   464,   148,   148,   176,
     151,   180,    76,   185,   186,   360,   198,   198,   198,   198,
     198,   157,   364,   155,   148,   194,   153,   192,   194,   194,
     154,   155,   120,   152,   190,   154,   220,   212,   174,   154,
     464,   177,   149,   404,   441,   442,   352,   352,   177,   177,
     151,   149,   404,   441,   442,   149,   404,   447,   410,   404,
     404,   352,   352,   154,   351,   354,   354,   355,   151,   155,
     155,   151,   177,   213,   213,   154,   154,   177,   177,   151,
     214,   176,   176,   352,   352,   362,   404,   155,   151,   148,
     391,   148,   148,   148,   148,   294,   332,   340,   458,   294,
     339,   149,   328,   174,   174,   149,   156,   196,   335,   336,
     342,   410,   411,   424,   155,   174,   359,   176,   359,   151,
     188,   189,   174,   225,   174,   225,   221,    78,   151,   221,
     232,   279,   281,   284,   290,   297,   301,   143,   144,   145,
     150,   151,   174,   221,   241,   242,   243,   279,   174,   174,
     221,   174,   364,   174,   221,   220,   221,   108,   109,   110,
     111,   112,   258,   260,   261,   174,    95,   174,    82,   149,
     149,   177,   148,   174,   174,   149,   223,   225,   404,   174,
     151,   176,   148,   148,   176,   155,   155,   148,   154,   154,
     154,   177,   151,   176,   214,   214,   177,   154,   177,   464,
     345,   157,   348,   464,   148,   383,   151,   156,   151,   155,
     156,   364,   464,   220,   118,   191,   192,   153,   192,   153,
     192,   154,   148,   151,   176,   177,   177,   151,   151,   176,
     176,   177,   177,   177,   176,   176,   154,   177,   151,   404,
     352,   352,   177,   177,   221,   148,   328,   328,   328,   149,
     196,   337,   338,   441,   449,   450,   451,   452,   174,   155,
     174,   335,   174,   378,   405,   410,   214,   297,   155,   174,
     341,   342,   341,   359,   131,   356,   357,   221,   151,   151,
     149,   223,   151,   221,   297,   143,   144,   145,   165,   244,
     245,   223,   222,   174,   244,   245,   151,   156,   221,   150,
     221,   222,   243,   174,   464,   151,   151,   151,   225,   260,
     261,   149,   214,   149,   182,   232,   198,   253,   107,     1,
     223,   404,   384,   176,   176,   154,   352,   177,   177,   154,
     154,   148,   157,   347,   148,   177,   214,   186,   214,   464,
     148,   154,   154,   191,   191,   352,   151,   151,   352,   352,
     151,   151,   154,   155,   131,   351,   131,   154,   177,   177,
     151,   151,   154,   450,   451,   452,   297,   449,   155,   174,
     404,   404,   174,   151,   410,   404,   174,   223,    75,    76,
     157,   235,   236,   237,   151,   221,    73,   223,   221,    73,
     174,   104,   150,   221,   222,   243,   150,   221,   223,   242,
     245,   244,   245,   174,   221,   148,   157,   237,   223,   149,
     176,   174,   182,   151,   156,   151,   151,   155,   156,   251,
     255,   359,   401,   177,   154,   154,   347,   464,   148,   148,
     154,   154,   177,   177,   177,   176,   177,   151,   151,   151,
     151,   151,   449,   404,   336,     1,   213,   233,   234,   402,
       1,   156,     1,   176,   223,   235,    73,   174,   151,   223,
      73,   165,   223,   222,   245,   244,   245,   174,   104,   221,
     165,   165,    73,   221,   150,   221,   222,   174,     1,   176,
     176,   262,   295,   297,   458,   156,   174,   153,   182,   267,
     268,   269,   223,   198,   188,    73,   106,   252,   254,   151,
     464,   148,   151,   151,   151,   354,   149,   404,   441,   442,
     338,   131,     1,   155,   156,   148,   272,   273,   279,   223,
      73,   174,   223,   221,   221,   150,   221,   222,   150,   221,
     221,   223,   165,   165,   165,   148,   272,   262,   177,   149,
     196,   401,   449,   180,   156,   101,   149,   151,   156,   155,
      73,   151,   223,   149,   223,   223,   148,   176,   213,   233,
     236,   238,   239,   279,   223,   165,   165,   165,   221,   150,
     221,   221,   238,   177,   174,   259,   297,   267,   154,   213,
     174,   267,   269,   223,   221,   107,   107,   352,   223,   228,
     177,   236,   221,   150,   221,   221,   177,   259,   212,   151,
     156,   182,   151,   151,   156,   151,   255,    73,   250,   177,
       1,   223,   148,   228,   148,   151,   225,   182,   270,   149,
     174,   270,   223,    73,   151,   225,   155,   156,   213,   151,
     223,   182,   180,   271,   151,   174,   151,   155,   174,   180
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
     242,   242,   242,   242,   242,   242,   242,   242,   242,   242,
     242,   242,   242,   242,   242,   242,   242,   243,   243,   243,
     244,   244,   245,   245,   245,   246,   246,   246,   246,   246,
     246,   246,   246,   246,   246,   246,   246,   246,   246,   246,
     246,   246,   246,   246,   246,   247,   247,   248,   249,   250,
     251,   251,   252,   252,   253,   253,   254,   255,   255,   255,
     255,   255,   255,   256,   256,   257,   257,   257,   258,   258,
     259,   259,   260,   260,   260,   260,   261,   262,   262,   262,
     262,   262,   263,   264,   264,   265,   265,   265,   265,   265,
     266,   266,   267,   267,   268,   268,   269,   269,   270,   270,
     270,   271,   271,   272,   272,   273,   273,   274,   274,   275,
     275,   276,   276,   277,   277,   278,   278,   279,   279,   279,
     280,   280,   281,   281,   281,   281,   281,   282,   282,   282,
     283,   283,   283,   284,   284,   284,   284,   284,   285,   285,
     286,   286,   287,   287,   287,   288,   288,   288,   288,   288,
     289,   289,   290,   290,   290,   290,   291,   291,   292,   292,
     292,   293,   293,   293,   294,   294,   294,   295,   295,   295,
     296,   296,   297,   297,   298,   298,   299,   299,   299,   299,
     299,   300,   301,   301,   301,   302,   302,   303,   303,   303,
     303,   303,   303,   303,   303,   304,   304,   304,   304,   304,
     304,   304,   304,   304,   304,   304,   304,   304,   304,   304,
     304,   304,   304,   304,   304,   304,   304,   304,   304,   304,
     304,   304,   304,   305,   305,   306,   307,   307,   308,   308,
     308,   308,   308,   309,   309,   310,   310,   310,   310,   311,
     311,   311,   311,   311,   311,   312,   312,   312,   312,   313,
     314,   313,   313,   315,   315,   315,   315,   316,   316,   316,
     317,   317,   317,   317,   318,   318,   318,   319,   319,   319,
     319,   319,   319,   320,   320,   320,   321,   321,   322,   322,
     324,   323,   325,   323,   326,   323,   327,   323,   323,   328,
     328,   329,   329,   330,   330,   331,   331,   331,   332,   332,
     332,   332,   332,   332,   332,   332,   333,   333,   334,   334,
     334,   334,   334,   334,   334,   334,   334,   334,   335,   335,
     335,   336,   336,   336,   337,   337,   337,   338,   339,   339,
     340,   340,   341,   341,   342,   343,   344,   343,   343,   343,
     343,   345,   343,   343,   343,   346,   346,   347,   347,   347,
     347,   348,   348,   348,   349,   349,   349,   349,   349,   349,
     349,   350,   350,   350,   350,   351,   351,   352,   352,   352,
     352,   353,   353,   353,   353,   354,   354,   354,   354,   354,
     355,   355,   355,   355,   355,   356,   356,   357,   357,   358,
     358,   359,   359,   359,   360,   360,   360,   361,   361,   362,
     362,   362,   362,   363,   363,   364,   364,   364,   364,   364,
     365,   365,   366,   366,   367,   367,   367,   367,   367,   368,
     368,   369,   369,   371,   370,   372,   370,   370,   370,   373,
     373,   373,   373,   374,   374,   374,   374,   375,   375,   376,
     376,   377,   377,   378,   378,   378,   378,   379,   379,   379,
     380,   380,   381,   381,   382,   382,   383,   383,   384,   384,
     385,   385,   385,   386,   386,   387,   387,   388,   388,   389,
     389,   390,   391,   392,   392,   392,   392,   392,   393,   392,
     394,   392,   395,   392,   396,   392,   397,   392,   398,   398,
     398,   399,   399,   400,   400,   400,   400,   400,   400,   400,
     400,   400,   400,   401,   401,   401,   402,   403,   403,   404,
     404,   405,   405,   406,   407,   407,   408,   408,   408,   409,
     409,   409,   409,   409,   409,   410,   410,   411,   411,   411,
     411,   412,   412,   412,   412,   413,   413,   413,   413,   413,
     413,   413,   414,   414,   414,   414,   415,   415,   415,   416,
     416,   416,   416,   416,   417,   417,   417,   417,   418,   418,
     418,   418,   418,   418,   419,   419,   419,   420,   420,   420,
     420,   420,   421,   421,   421,   421,   422,   422,   422,   422,
     422,   422,   423,   423,   424,   424,   424,   424,   425,   425,
     425,   425,   426,   426,   426,   426,   426,   426,   426,   427,
     427,   427,   427,   427,   428,   428,   428,   428,   428,   429,
     429,   429,   430,   430,   430,   430,   431,   431,   431,   432,
     432,   432,   432,   432,   433,   433,   434,   434,   434,   435,
     435,   436,   436,   437,   437,   437,   438,   438,   438,   438,
     438,   439,   439,   439,   439,   440,   440,   440,   441,   441,
     441,   441,   442,   442,   442,   442,   443,   443,   443,   443,
     444,   444,   444,   444,   444,   445,   445,   445,   445,   446,
     446,   446,   447,   447,   447,   448,   448,   448,   448,   448,
     448,   449,   449,   449,   450,   450,   450,   450,   450,   451,
     451,   451,   451,   452,   452,   453,   453,   453,   454,   454,
     455,   455,   455,   455,   455,   455,   456,   456,   456,   456,
     456,   456,   456,   456,   456,   456,   457,   457,   457,   457,
     458,   458,   458,   459,   459,   460,   460,   460,   460,   460,
     460,   461,   461,   461,   461,   461,   461,   462,   462,   462,
     463,   463,   464,   464,   465,   465
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
       3,     3,     5,     5,     5,     5,     2,     3,     4,     5,
       5,     5,     7,     7,     7,     7,     2,     3,     4,     4,
       4,     6,     6,     6,     6,     3,     4,     1,     1,     1,
       1,     1,     1,     1,     1,     3,     4,     2,     3,     3,
       2,     3,     2,     3,     3,     6,     2,     2,     3,     3,
       3,     3,     3,     3,     5,     1,     1,     5,     5,     4,
       0,     1,     4,     6,     1,     3,     4,     3,     5,     3,
       3,     6,     7,     3,     5,     3,     3,     4,     8,     9,
       0,     2,     1,     1,     1,     1,     2,     1,     2,     2,
       2,     1,     3,     1,     1,     6,     8,    10,    12,    14,
       0,     1,     0,     1,     1,     3,     4,     7,     0,     1,
       3,     1,     3,     0,     1,     1,     2,     0,     1,     4,
       5,     0,     1,     3,     4,     1,     3,     2,     2,     1,
       7,     5,     1,     1,     1,     1,     1,     2,     3,     6,
       3,     3,     4,     1,     2,     2,     3,     8,     8,     8,
       5,     9,     2,     2,     5,     3,     5,     4,     3,     4,
       4,     7,     2,     1,     1,     1,     3,     6,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       0,     1,     1,     2,     1,     1,     1,     1,     1,     1,
       1,     4,     1,     2,     3,     1,     2,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     0,     1,     5,     0,     1,     1,     2,
       2,     3,     3,     1,     3,     1,     2,     2,     2,     4,
       4,     4,     4,     1,     1,     1,     2,     2,     3,     1,
       0,     3,     2,     1,     2,     2,     3,     1,     2,     2,
       1,     2,     2,     3,     1,     2,     2,     1,     2,     3,
       1,     2,     3,     1,     3,     4,     1,     1,     1,     1,
       0,     7,     0,     8,     0,     8,     0,     8,     1,     0,
       3,     3,     3,     1,     1,     2,     1,     1,     1,     2,
       1,     2,     1,     2,     1,     2,     0,     2,     3,     4,
       4,     3,     2,     2,     3,     3,     2,     1,     0,     1,
       4,     1,     2,     2,     0,     1,     4,     1,     2,     3,
       1,     2,     0,     1,     2,     6,     0,     8,     7,     8,
       9,     0,    12,    11,     1,     3,     3,     2,     2,     4,
       5,     0,     2,     5,     0,     1,     1,     1,     5,     5,
       5,     1,     5,     5,     9,     1,     5,     0,     1,     1,
       5,     1,     1,     5,     5,     1,     3,     3,     4,     1,
       1,     1,     1,     2,     1,     3,     3,     2,     3,     1,
       3,     1,     1,     1,     1,     1,     2,     1,     1,     0,
       2,     2,     4,     1,     4,     0,     1,     2,     3,     4,
       2,     2,     1,     2,     2,     5,     5,     7,     6,     1,
       3,     0,     2,     0,     5,     0,     5,     3,     1,     0,
       1,     1,     1,     1,     1,     1,     1,     0,     1,     1,
       2,     5,     6,     1,     1,     3,     3,     2,     3,     3,
       2,     4,     1,     4,     7,    10,     1,     4,     2,     2,
       1,     1,     5,     2,     5,     0,     1,     3,     4,     0,
       1,     0,     0,     1,     1,     1,     2,     5,     0,     6,
       0,     8,     0,     7,     0,     7,     0,     8,     1,     2,
       3,     0,     5,     3,     4,     4,     4,     4,     5,     5,
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
#line 563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.enterScope(); }
#line 7175 "Parser/parser.cc"
    break;

  case 3:
#line 567 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 7181 "Parser/parser.cc"
    break;

  case 4:
#line 574 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantInteger( *(yyvsp[0].tok) ) ); }
#line 7187 "Parser/parser.cc"
    break;

  case 5:
#line 575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7193 "Parser/parser.cc"
    break;

  case 6:
#line 576 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7199 "Parser/parser.cc"
    break;

  case 7:
#line 577 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7205 "Parser/parser.cc"
    break;

  case 8:
#line 578 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantChar( *(yyvsp[0].tok) ) ); }
#line 7211 "Parser/parser.cc"
    break;

  case 19:
#line 599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 7217 "Parser/parser.cc"
    break;

  case 20:
#line 603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.constant) = build_constantStr( *(yyvsp[0].str) ); }
#line 7223 "Parser/parser.cc"
    break;

  case 21:
#line 607 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 7229 "Parser/parser.cc"
    break;

  case 22:
#line 609 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 7239 "Parser/parser.cc"
    break;

  case 23:
#line 620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 7245 "Parser/parser.cc"
    break;

  case 24:
#line 622 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 7251 "Parser/parser.cc"
    break;

  case 25:
#line 626 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_dimensionref( (yyvsp[0].tok) ) ); }
#line 7257 "Parser/parser.cc"
    break;

  case 27:
#line 629 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 7263 "Parser/parser.cc"
    break;

  case 28:
#line 631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new StmtExpr( dynamic_cast<CompoundStmt *>(maybeMoveBuild<Statement>((yyvsp[-1].sn)) ) ) ); }
#line 7269 "Parser/parser.cc"
    break;

  case 29:
#line 633 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7275 "Parser/parser.cc"
    break;

  case 30:
#line 635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7281 "Parser/parser.cc"
    break;

  case 31:
#line 637 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild<Expression>( (yyvsp[-3].en) );
			(yyval.en) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 7291 "Parser/parser.cc"
    break;

  case 32:
#line 647 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "Adjacent identifiers are not meaningful in an expression. "
											   "Possible problem is identifier \"", *(yyvsp[-1].tok).str,
											   "\" is a misspelled typename or an incorrectly specified type name, "
											   "e.g., missing generic parameter or missing struct/union/enum before typename." ) );
			(yyval.en) = nullptr;
 		}
#line 7303 "Parser/parser.cc"
    break;

  case 33:
#line 655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "Identifier \"", *(yyvsp[-1].tok).str, "\" cannot appear before a type. "
											   "Possible problem is misspelled storage or CV qualifier." ) );
			(yyval.en) = nullptr;
		}
#line 7313 "Parser/parser.cc"
    break;

  case 35:
#line 665 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// steal the association node from the singleton and delete the wrapper
			(yyvsp[-2].genexpr)->associations.splice((yyvsp[-2].genexpr)->associations.end(), (yyvsp[0].genexpr)->associations);
			delete (yyvsp[0].genexpr);
			(yyval.genexpr) = (yyvsp[-2].genexpr);
		}
#line 7324 "Parser/parser.cc"
    break;

  case 36:
#line 675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuildType((yyvsp[-2].decl)), maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } );
		}
#line 7333 "Parser/parser.cc"
    break;

  case 37:
#line 680 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } ); }
#line 7339 "Parser/parser.cc"
    break;

  case 39:
#line 689 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-5].en), new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) ) ) )) ) ); }
#line 7345 "Parser/parser.cc"
    break;

  case 40:
#line 695 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7351 "Parser/parser.cc"
    break;

  case 41:
#line 697 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7357 "Parser/parser.cc"
    break;

  case 42:
#line 699 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, new ExpressionNode( (yyvsp[-3].constant) ), (yyvsp[-1].en) ) ); }
#line 7363 "Parser/parser.cc"
    break;

  case 43:
#line 701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.en) = new ExpressionNode( new ConstructorExpr( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) ) );
		}
#line 7373 "Parser/parser.cc"
    break;

  case 44:
#line 707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7379 "Parser/parser.cc"
    break;

  case 45:
#line 709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 7385 "Parser/parser.cc"
    break;

  case 46:
#line 711 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 7391 "Parser/parser.cc"
    break;

  case 47:
#line 713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), new ExpressionNode( (yyvsp[-2].constant) ) ) ); }
#line 7397 "Parser/parser.cc"
    break;

  case 48:
#line 715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7403 "Parser/parser.cc"
    break;

  case 49:
#line 717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 7409 "Parser/parser.cc"
    break;

  case 50:
#line 719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-1].en), build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) ) ) ); }
#line 7415 "Parser/parser.cc"
    break;

  case 51:
#line 721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7421 "Parser/parser.cc"
    break;

  case 52:
#line 723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[0].aggKey), (yyvsp[-2].en) ) ); }
#line 7427 "Parser/parser.cc"
    break;

  case 53:
#line 725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7433 "Parser/parser.cc"
    break;

  case 54:
#line 727 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 7439 "Parser/parser.cc"
    break;

  case 55:
#line 729 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7445 "Parser/parser.cc"
    break;

  case 56:
#line 731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::IncrPost, (yyvsp[-1].en) ) ); }
#line 7451 "Parser/parser.cc"
    break;

  case 57:
#line 733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::DecrPost, (yyvsp[-1].en) ) ); }
#line 7457 "Parser/parser.cc"
    break;

  case 58:
#line 735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].in), true ) ) ); }
#line 7463 "Parser/parser.cc"
    break;

  case 59:
#line 737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].in), true ))->set_maybeConstructed( false ) ) ); }
#line 7469 "Parser/parser.cc"
    break;

  case 60:
#line 739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) );
		}
#line 7479 "Parser/parser.cc"
    break;

  case 61:
#line 748 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7485 "Parser/parser.cc"
    break;

  case 64:
#line 755 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7491 "Parser/parser.cc"
    break;

  case 65:
#line 760 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Default parameter for argument is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7497 "Parser/parser.cc"
    break;

  case 68:
#line 767 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7503 "Parser/parser.cc"
    break;

  case 70:
#line 773 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-1].tok) ) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7509 "Parser/parser.cc"
    break;

  case 71:
#line 775 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-3].tok) ) ), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7515 "Parser/parser.cc"
    break;

  case 72:
#line 777 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7521 "Parser/parser.cc"
    break;

  case 73:
#line 779 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7527 "Parser/parser.cc"
    break;

  case 74:
#line 781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7533 "Parser/parser.cc"
    break;

  case 75:
#line 783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7539 "Parser/parser.cc"
    break;

  case 76:
#line 788 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_constantInteger( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 7545 "Parser/parser.cc"
    break;

  case 77:
#line 790 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_field_name_FLOATINGconstant( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 7551 "Parser/parser.cc"
    break;

  case 78:
#line 792 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_varref( (yyvsp[-1].tok) ), (yyvsp[0].en) ) );
		}
#line 7559 "Parser/parser.cc"
    break;

  case 79:
#line 799 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7565 "Parser/parser.cc"
    break;

  case 80:
#line 801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Expression * constant = build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) );
			(yyval.en) = (yyvsp[-1].en) != nullptr ? new ExpressionNode( build_fieldSel( (yyvsp[-1].en),  constant ) ) : new ExpressionNode( constant );
		}
#line 7574 "Parser/parser.cc"
    break;

  case 83:
#line 813 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 7580 "Parser/parser.cc"
    break;

  case 84:
#line 815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en)->set_extension( true ); }
#line 7586 "Parser/parser.cc"
    break;

  case 85:
#line 820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 7606 "Parser/parser.cc"
    break;

  case 86:
#line 836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_val( (yyvsp[-1].op), (yyvsp[0].en) ) ); }
#line 7612 "Parser/parser.cc"
    break;

  case 87:
#line 838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Incr, (yyvsp[0].en) ) ); }
#line 7618 "Parser/parser.cc"
    break;

  case 88:
#line 840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Decr, (yyvsp[0].en) ) ); }
#line 7624 "Parser/parser.cc"
    break;

  case 89:
#line 842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7630 "Parser/parser.cc"
    break;

  case 90:
#line 844 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 7636 "Parser/parser.cc"
    break;

  case 91:
#line 846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7642 "Parser/parser.cc"
    break;

  case 92:
#line 848 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 7648 "Parser/parser.cc"
    break;

  case 93:
#line 850 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_offsetOf( (yyvsp[-3].decl), build_varref( (yyvsp[-1].tok) ) ) ); }
#line 7654 "Parser/parser.cc"
    break;

  case 94:
#line 852 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.en) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 7663 "Parser/parser.cc"
    break;

  case 95:
#line 859 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::PointTo; }
#line 7669 "Parser/parser.cc"
    break;

  case 96:
#line 860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AddressOf; }
#line 7675 "Parser/parser.cc"
    break;

  case 97:
#line 862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::And; }
#line 7681 "Parser/parser.cc"
    break;

  case 98:
#line 866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::UnPlus; }
#line 7687 "Parser/parser.cc"
    break;

  case 99:
#line 867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::UnMinus; }
#line 7693 "Parser/parser.cc"
    break;

  case 100:
#line 868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::Neg; }
#line 7699 "Parser/parser.cc"
    break;

  case 101:
#line 869 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::BitNeg; }
#line 7705 "Parser/parser.cc"
    break;

  case 103:
#line 875 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cast( (yyvsp[-2].decl), (yyvsp[0].en) ) ); }
#line 7711 "Parser/parser.cc"
    break;

  case 104:
#line 877 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 7717 "Parser/parser.cc"
    break;

  case 105:
#line 879 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 7723 "Parser/parser.cc"
    break;

  case 106:
#line 881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( nullptr ) ) ); }
#line 7729 "Parser/parser.cc"
    break;

  case 107:
#line 883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 7735 "Parser/parser.cc"
    break;

  case 108:
#line 885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Return cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7741 "Parser/parser.cc"
    break;

  case 109:
#line 887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7747 "Parser/parser.cc"
    break;

  case 110:
#line 889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7753 "Parser/parser.cc"
    break;

  case 118:
#line 909 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Exp, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7759 "Parser/parser.cc"
    break;

  case 120:
#line 915 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mul, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7765 "Parser/parser.cc"
    break;

  case 121:
#line 917 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Div, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7771 "Parser/parser.cc"
    break;

  case 122:
#line 919 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mod, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7777 "Parser/parser.cc"
    break;

  case 124:
#line 925 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Plus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7783 "Parser/parser.cc"
    break;

  case 125:
#line 927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Minus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7789 "Parser/parser.cc"
    break;

  case 127:
#line 933 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7795 "Parser/parser.cc"
    break;

  case 128:
#line 935 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::RShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7801 "Parser/parser.cc"
    break;

  case 130:
#line 941 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7807 "Parser/parser.cc"
    break;

  case 131:
#line 943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7813 "Parser/parser.cc"
    break;

  case 132:
#line 945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7819 "Parser/parser.cc"
    break;

  case 133:
#line 947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7825 "Parser/parser.cc"
    break;

  case 135:
#line 953 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Eq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7831 "Parser/parser.cc"
    break;

  case 136:
#line 955 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Neq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7837 "Parser/parser.cc"
    break;

  case 138:
#line 961 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitAnd, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7843 "Parser/parser.cc"
    break;

  case 140:
#line 967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Xor, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7849 "Parser/parser.cc"
    break;

  case 142:
#line 973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitOr, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7855 "Parser/parser.cc"
    break;

  case 144:
#line 979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), true ) ); }
#line 7861 "Parser/parser.cc"
    break;

  case 146:
#line 985 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), false ) ); }
#line 7867 "Parser/parser.cc"
    break;

  case 148:
#line 991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7873 "Parser/parser.cc"
    break;

  case 149:
#line 994 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-3].en), (yyvsp[-3].en), (yyvsp[0].en) ) ); }
#line 7879 "Parser/parser.cc"
    break;

  case 152:
#line 1005 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
//			if ( $2 == OperKinds::AtAssn ) {
//				SemanticError( yylloc, "C @= assignment is currently unimplemented." ); $$ = nullptr;
//			} else {
				(yyval.en) = new ExpressionNode( build_binary_val( (yyvsp[-1].op), (yyvsp[-2].en), (yyvsp[0].en) ) );
//			} // if
		}
#line 7891 "Parser/parser.cc"
    break;

  case 153:
#line 1013 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7897 "Parser/parser.cc"
    break;

  case 154:
#line 1018 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7903 "Parser/parser.cc"
    break;

  case 158:
#line 1028 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::Assign; }
#line 7909 "Parser/parser.cc"
    break;

  case 159:
#line 1029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AtAssn; }
#line 7915 "Parser/parser.cc"
    break;

  case 160:
#line 1033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ExpAssn; }
#line 7921 "Parser/parser.cc"
    break;

  case 161:
#line 1034 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MulAssn; }
#line 7927 "Parser/parser.cc"
    break;

  case 162:
#line 1035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::DivAssn; }
#line 7933 "Parser/parser.cc"
    break;

  case 163:
#line 1036 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ModAssn; }
#line 7939 "Parser/parser.cc"
    break;

  case 164:
#line 1037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::PlusAssn; }
#line 7945 "Parser/parser.cc"
    break;

  case 165:
#line 1038 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MinusAssn; }
#line 7951 "Parser/parser.cc"
    break;

  case 166:
#line 1039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::LSAssn; }
#line 7957 "Parser/parser.cc"
    break;

  case 167:
#line 1040 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::RSAssn; }
#line 7963 "Parser/parser.cc"
    break;

  case 168:
#line 1041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AndAssn; }
#line 7969 "Parser/parser.cc"
    break;

  case 169:
#line 1042 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ERAssn; }
#line 7975 "Parser/parser.cc"
    break;

  case 170:
#line 1043 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::OrAssn; }
#line 7981 "Parser/parser.cc"
    break;

  case 171:
#line 1054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)(new ExpressionNode( nullptr ) )->set_last( (yyvsp[-1].en) ) ) ); }
#line 7987 "Parser/parser.cc"
    break;

  case 172:
#line 1056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-4].en)->set_last( (yyvsp[-1].en) ) ) )); }
#line 7993 "Parser/parser.cc"
    break;

  case 174:
#line 1062 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7999 "Parser/parser.cc"
    break;

  case 175:
#line 1064 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 8005 "Parser/parser.cc"
    break;

  case 176:
#line 1066 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8011 "Parser/parser.cc"
    break;

  case 178:
#line 1072 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new CommaExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 8017 "Parser/parser.cc"
    break;

  case 179:
#line 1077 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8023 "Parser/parser.cc"
    break;

  case 191:
#line 1095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 8029 "Parser/parser.cc"
    break;

  case 193:
#line 1098 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_directive( (yyvsp[0].tok) ) ); }
#line 8035 "Parser/parser.cc"
    break;

  case 194:
#line 1104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[0].sn)->add_label( (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 8041 "Parser/parser.cc"
    break;

  case 195:
#line 1106 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "Label \"", *(yyvsp[-3].tok).str, "\" must be associated with a statement, "
											   "where a declaration, case, or default is not a statement. "
											   "Move the label or terminate with a semi-colon." ) );
			(yyval.sn) = nullptr;
		}
#line 8052 "Parser/parser.cc"
    break;

  case 196:
#line 1116 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (StatementNode *)0 ) ); }
#line 8058 "Parser/parser.cc"
    break;

  case 197:
#line 1121 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (yyvsp[-2].sn) ) ); }
#line 8064 "Parser/parser.cc"
    break;

  case 199:
#line 1127 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 8070 "Parser/parser.cc"
    break;

  case 200:
#line 1132 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8076 "Parser/parser.cc"
    break;

  case 201:
#line 1134 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8082 "Parser/parser.cc"
    break;

  case 202:
#line 1136 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8088 "Parser/parser.cc"
    break;

  case 203:
#line 1138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8094 "Parser/parser.cc"
    break;

  case 206:
#line 1145 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 8100 "Parser/parser.cc"
    break;

  case 207:
#line 1147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Declarations only allowed at the start of the switch body, i.e., after the '{'." ); (yyval.sn) = nullptr; }
#line 8106 "Parser/parser.cc"
    break;

  case 208:
#line 1152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_expr( (yyvsp[-1].en) ) ); }
#line 8112 "Parser/parser.cc"
    break;

  case 209:
#line 1154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_mutex( nullptr, new StatementNode( build_expr( (yyvsp[-1].en) ) ) ) ); }
#line 8118 "Parser/parser.cc"
    break;

  case 210:
#line 1161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn); }
#line 8124 "Parser/parser.cc"
    break;

  case 211:
#line 1163 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( true, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8130 "Parser/parser.cc"
    break;

  case 212:
#line 1165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( true, (yyvsp[-7].en), (yyvsp[-2].sn) ) );
			// The semantics of the declaration list is changed to include associated initialization, which is performed
			// *before* the transfer to the appropriate case clause by hoisting the declarations into a compound
			// statement around the switch.  Statements after the initial declaration list can never be executed, and
			// therefore, are removed from the grammar even though C allows it. The change also applies to choose
			// statement.
			(yyval.sn) = (yyvsp[-3].decl) ? new StatementNode( build_compound( (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 8144 "Parser/parser.cc"
    break;

  case 213:
#line 1175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Only declarations can appear before the list of case clauses." ); (yyval.sn) = nullptr; }
#line 8150 "Parser/parser.cc"
    break;

  case 214:
#line 1177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( false, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8156 "Parser/parser.cc"
    break;

  case 215:
#line 1179 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( false, (yyvsp[-7].en), (yyvsp[-2].sn) ) );
			(yyval.sn) = (yyvsp[-3].decl) ? new StatementNode( build_compound( (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 8165 "Parser/parser.cc"
    break;

  case 216:
#line 1184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Only declarations can appear before the list of case clauses." ); (yyval.sn) = nullptr; }
#line 8171 "Parser/parser.cc"
    break;

  case 217:
#line 1190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-2].ifctl), maybe_build_compound( (yyvsp[0].sn) ), nullptr ) ); }
#line 8177 "Parser/parser.cc"
    break;

  case 218:
#line 1192 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-4].ifctl), maybe_build_compound( (yyvsp[-2].sn) ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8183 "Parser/parser.cc"
    break;

  case 219:
#line 1197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( nullptr, (yyvsp[0].en) ); }
#line 8189 "Parser/parser.cc"
    break;

  case 220:
#line 1199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 8195 "Parser/parser.cc"
    break;

  case 221:
#line 1201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 8201 "Parser/parser.cc"
    break;

  case 222:
#line 1203 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[-1].decl), (yyvsp[0].en) ); }
#line 8207 "Parser/parser.cc"
    break;

  case 223:
#line 1210 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = (yyvsp[0].en); }
#line 8213 "Parser/parser.cc"
    break;

  case 224:
#line 1212 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 8219 "Parser/parser.cc"
    break;

  case 226:
#line 1217 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.sn) = new StatementNode( build_case( (yyvsp[0].en) ) ); }
#line 8225 "Parser/parser.cc"
    break;

  case 227:
#line 1219 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)((yyvsp[-2].sn)->set_last( new StatementNode( build_case( (yyvsp[0].en) ) ) ) ); }
#line 8231 "Parser/parser.cc"
    break;

  case 228:
#line 1224 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing case list after case." ); (yyval.sn) = nullptr; }
#line 8237 "Parser/parser.cc"
    break;

  case 229:
#line 1225 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn); }
#line 8243 "Parser/parser.cc"
    break;

  case 230:
#line 1227 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing colon after case list." ); (yyval.sn) = nullptr; }
#line 8249 "Parser/parser.cc"
    break;

  case 231:
#line 1228 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.sn) = new StatementNode( build_default() ); }
#line 8255 "Parser/parser.cc"
    break;

  case 232:
#line 1231 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing colon after default." ); (yyval.sn) = nullptr; }
#line 8261 "Parser/parser.cc"
    break;

  case 234:
#line 1236 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)( (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) )); }
#line 8267 "Parser/parser.cc"
    break;

  case 235:
#line 1240 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( maybe_build_compound( (yyvsp[0].sn) ) ); }
#line 8273 "Parser/parser.cc"
    break;

  case 236:
#line 1245 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = nullptr; }
#line 8279 "Parser/parser.cc"
    break;

  case 238:
#line 1251 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ); }
#line 8285 "Parser/parser.cc"
    break;

  case 239:
#line 1253 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)( (yyvsp[-2].sn)->set_last( (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ) ) ); }
#line 8291 "Parser/parser.cc"
    break;

  case 240:
#line 1258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8297 "Parser/parser.cc"
    break;

  case 241:
#line 1260 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_while( new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( (yyvsp[-2].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse, "" );
		}
#line 8306 "Parser/parser.cc"
    break;

  case 242:
#line 1265 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( (yyvsp[-2].ifctl), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8312 "Parser/parser.cc"
    break;

  case 243:
#line 1267 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( (yyvsp[-4].ifctl), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[0].sn) ) ); }
#line 8318 "Parser/parser.cc"
    break;

  case 244:
#line 1269 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( NEW_ONE, maybe_build_compound( (yyvsp[-4].sn) ) ) ); }
#line 8324 "Parser/parser.cc"
    break;

  case 245:
#line 1271 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_do_while( NEW_ONE, maybe_build_compound( (yyvsp[-5].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse, "" );
		}
#line 8333 "Parser/parser.cc"
    break;

  case 246:
#line 1276 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( (yyvsp[-2].en), maybe_build_compound( (yyvsp[-5].sn) ) ) ); }
#line 8339 "Parser/parser.cc"
    break;

  case 247:
#line 1278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( (yyvsp[-3].en), maybe_build_compound( (yyvsp[-6].sn) ), (yyvsp[0].sn) ) ); }
#line 8345 "Parser/parser.cc"
    break;

  case 248:
#line 1280 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( new ForCtrl( (ExpressionNode * )nullptr, (ExpressionNode * )nullptr, (ExpressionNode * )nullptr ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8351 "Parser/parser.cc"
    break;

  case 249:
#line 1282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_for( new ForCtrl( (ExpressionNode * )nullptr, (ExpressionNode * )nullptr, (ExpressionNode * )nullptr ), maybe_build_compound( (yyvsp[-2].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse, "" );
		}
#line 8360 "Parser/parser.cc"
    break;

  case 250:
#line 1287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( (yyvsp[-2].fctl), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8366 "Parser/parser.cc"
    break;

  case 251:
#line 1289 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( (yyvsp[-4].fctl), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[0].sn) ) ); }
#line 8372 "Parser/parser.cc"
    break;

  case 253:
#line 1299 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 8391 "Parser/parser.cc"
    break;

  case 254:
#line 1317 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (ExpressionNode * )nullptr, (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8397 "Parser/parser.cc"
    break;

  case 255:
#line 1319 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8403 "Parser/parser.cc"
    break;

  case 256:
#line 1321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-3].decl), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8409 "Parser/parser.cc"
    break;

  case 257:
#line 1324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), NEW_ZERO, OperKinds::LThan, (yyvsp[0].en)->clone(), NEW_ONE ); }
#line 8415 "Parser/parser.cc"
    break;

  case 258:
#line 1326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-1].compop) == OperKinds::GThan || (yyvsp[-1].compop) == OperKinds::GEThan ? (yyvsp[0].en)->clone() : NEW_ZERO, (yyvsp[-1].compop), (yyvsp[0].en)->clone(), NEW_ONE ); }
#line 8421 "Parser/parser.cc"
    break;

  case 259:
#line 1328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].en)->clone(), (yyvsp[-1].compop), (yyvsp[0].en), NEW_ONE ); }
#line 8427 "Parser/parser.cc"
    break;

  case 260:
#line 1330 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ),
						(yyvsp[-1].compop) == OperKinds::GThan || (yyvsp[-1].compop) == OperKinds::GEThan ? (yyvsp[0].en)->clone() : NEW_ZERO, (yyvsp[-1].compop), (yyvsp[0].en)->clone(), NEW_ONE ); }
#line 8434 "Parser/parser.cc"
    break;

  case 261:
#line 1333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].en)->clone(), (yyvsp[-1].compop), nullptr, NEW_ONE ); }
#line 8440 "Parser/parser.cc"
    break;

  case 262:
#line 1336 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8446 "Parser/parser.cc"
    break;

  case 263:
#line 1338 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), new string( DeclarationNode::anonymous.newName() ),
						(yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ? (yyvsp[-2].en)->clone() : NEW_ZERO,	(yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[0].en) ); }
#line 8453 "Parser/parser.cc"
    break;

  case 264:
#line 1341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) ); }
#line 8459 "Parser/parser.cc"
    break;

  case 265:
#line 1343 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan ) { SemanticError( yylloc, "Negative range \"-~\" is meaningless when comparison and iterator are empty. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-4].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 8468 "Parser/parser.cc"
    break;

  case 266:
#line 1348 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( new ExpressionNode( build_constantInteger( *new string( "0u" ) ) ), (yyvsp[-1].en), nullptr, OperKinds::LThan, nullptr, nullptr ); }
#line 8474 "Parser/parser.cc"
    break;

  case 267:
#line 1350 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-2].en), NEW_ZERO, OperKinds::LThan, (yyvsp[0].en)->clone(), NEW_ONE ); }
#line 8480 "Parser/parser.cc"
    break;

  case 268:
#line 1352 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-3].en), (yyvsp[-1].compop) == OperKinds::GThan || (yyvsp[-1].compop) == OperKinds::GEThan ? (yyvsp[0].en)->clone() : NEW_ZERO, (yyvsp[-1].compop), (yyvsp[0].en)->clone(), NEW_ONE ); }
#line 8486 "Parser/parser.cc"
    break;

  case 269:
#line 1354 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), (yyvsp[-1].compop), (yyvsp[0].en), NEW_ONE ); }
#line 8492 "Parser/parser.cc"
    break;

  case 270:
#line 1356 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-4].en),	(yyvsp[-1].compop) == OperKinds::GThan || (yyvsp[-1].compop) == OperKinds::GEThan ? (yyvsp[0].en)->clone() : NEW_ZERO, (yyvsp[-1].compop), (yyvsp[0].en)->clone(), NEW_ONE ); }
#line 8498 "Parser/parser.cc"
    break;

  case 271:
#line 1358 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), (yyvsp[-1].compop), nullptr, NEW_ONE ); }
#line 8504 "Parser/parser.cc"
    break;

  case 272:
#line 1361 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8510 "Parser/parser.cc"
    break;

  case 273:
#line 1363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-6].en),	(yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ? (yyvsp[-2].en)->clone() : NEW_ZERO, (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[0].en) ); }
#line 8516 "Parser/parser.cc"
    break;

  case 274:
#line 1365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) ); }
#line 8522 "Parser/parser.cc"
    break;

  case 275:
#line 1367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan ) { SemanticError( yylloc, "Negative range \"-~\" is meaningless when comparison and iterator are empty. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 8531 "Parser/parser.cc"
    break;

  case 276:
#line 1373 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-1].decl), NEW_ZERO, OperKinds::LThan, (yyvsp[0].en), NEW_ONE ); }
#line 8537 "Parser/parser.cc"
    break;

  case 277:
#line 1375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].decl), (yyvsp[-1].compop) == OperKinds::GThan || (yyvsp[-1].compop) == OperKinds::GEThan ? (yyvsp[0].en)->clone() : NEW_ZERO, (yyvsp[-1].compop), (yyvsp[0].en), NEW_ONE ); }
#line 8543 "Parser/parser.cc"
    break;

  case 278:
#line 1377 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-3].decl), (yyvsp[-2].en), (yyvsp[-1].compop), (yyvsp[0].en), NEW_ONE ); }
#line 8549 "Parser/parser.cc"
    break;

  case 279:
#line 1379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-3].decl), (yyvsp[-1].compop) == OperKinds::GThan || (yyvsp[-1].compop) == OperKinds::GEThan ? (yyvsp[0].en)->clone() : NEW_ZERO, (yyvsp[-1].compop), (yyvsp[0].en), NEW_ONE ); }
#line 8555 "Parser/parser.cc"
    break;

  case 280:
#line 1381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-3].decl), (yyvsp[-2].en), (yyvsp[-1].compop), nullptr, NEW_ONE ); }
#line 8561 "Parser/parser.cc"
    break;

  case 281:
#line 1384 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-4].en), (yyvsp[-3].compop), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8567 "Parser/parser.cc"
    break;

  case 282:
#line 1386 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ? (yyvsp[-2].en)->clone() : NEW_ZERO, (yyvsp[-3].compop), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8573 "Parser/parser.cc"
    break;

  case 283:
#line 1388 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-4].en), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) ); }
#line 8579 "Parser/parser.cc"
    break;

  case 284:
#line 1390 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan ) { SemanticError( yylloc, "Negative range \"-~\" is meaningless when comparison and iterator are empty. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-4].en), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 8588 "Parser/parser.cc"
    break;

  case 285:
#line 1396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.fctl) = nullptr;
			//$$ = forCtrl( new ExpressionNode( build_varref( $3 ) ), $1, nullptr, OperKinds::Range, nullptr, nullptr );
		}
#line 8597 "Parser/parser.cc"
    break;

  case 286:
#line 1401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.fctl) = nullptr;
		}
#line 8605 "Parser/parser.cc"
    break;

  case 287:
#line 1408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GThan; }
#line 8611 "Parser/parser.cc"
    break;

  case 288:
#line 1410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 8617 "Parser/parser.cc"
    break;

  case 289:
#line 1412 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GEThan; }
#line 8623 "Parser/parser.cc"
    break;

  case 290:
#line 1417 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LThan; }
#line 8629 "Parser/parser.cc"
    break;

  case 291:
#line 1419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GThan; }
#line 8635 "Parser/parser.cc"
    break;

  case 293:
#line 1425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 8641 "Parser/parser.cc"
    break;

  case 294:
#line 1427 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GEThan; }
#line 8647 "Parser/parser.cc"
    break;

  case 295:
#line 1432 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Goto ) ); }
#line 8653 "Parser/parser.cc"
    break;

  case 296:
#line 1436 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_computedgoto( (yyvsp[-1].en) ) ); }
#line 8659 "Parser/parser.cc"
    break;

  case 297:
#line 1439 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThrough ) ); }
#line 8665 "Parser/parser.cc"
    break;

  case 298:
#line 1441 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::FallThrough ) ); }
#line 8671 "Parser/parser.cc"
    break;

  case 299:
#line 1443 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThroughDefault ) ); }
#line 8677 "Parser/parser.cc"
    break;

  case 300:
#line 1446 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Continue ) ); }
#line 8683 "Parser/parser.cc"
    break;

  case 301:
#line 1450 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Continue ) ); }
#line 8689 "Parser/parser.cc"
    break;

  case 302:
#line 1453 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Break ) ); }
#line 8695 "Parser/parser.cc"
    break;

  case 303:
#line 1457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Break ) ); }
#line 8701 "Parser/parser.cc"
    break;

  case 304:
#line 1459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_return( (yyvsp[-1].en) ) ); }
#line 8707 "Parser/parser.cc"
    break;

  case 305:
#line 1461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 8713 "Parser/parser.cc"
    break;

  case 306:
#line 1463 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr ) ); }
#line 8719 "Parser/parser.cc"
    break;

  case 307:
#line 1465 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn) ) ); }
#line 8725 "Parser/parser.cc"
    break;

  case 308:
#line 1467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Coroutine ) ); }
#line 8731 "Parser/parser.cc"
    break;

  case 309:
#line 1469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Coroutine ) ); }
#line 8737 "Parser/parser.cc"
    break;

  case 310:
#line 1471 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Generator ) ); }
#line 8743 "Parser/parser.cc"
    break;

  case 311:
#line 1473 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Generator ) ); }
#line 8749 "Parser/parser.cc"
    break;

  case 312:
#line 1475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_throw( (yyvsp[-1].en) ) ); }
#line 8755 "Parser/parser.cc"
    break;

  case 313:
#line 1477 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume( (yyvsp[-1].en) ) ); }
#line 8761 "Parser/parser.cc"
    break;

  case 314:
#line 1479 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume_at( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 8767 "Parser/parser.cc"
    break;

  case 317:
#line 1489 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_with( (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8773 "Parser/parser.cc"
    break;

  case 318:
#line 1495 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_mutex( (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8779 "Parser/parser.cc"
    break;

  case 319:
#line 1499 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 8785 "Parser/parser.cc"
    break;

  case 320:
#line 1504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8791 "Parser/parser.cc"
    break;

  case 322:
#line 1510 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 8797 "Parser/parser.cc"
    break;

  case 323:
#line 1514 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) )); }
#line 8803 "Parser/parser.cc"
    break;

  case 325:
#line 1521 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8809 "Parser/parser.cc"
    break;

  case 326:
#line 1525 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 8815 "Parser/parser.cc"
    break;

  case 327:
#line 1530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8821 "Parser/parser.cc"
    break;

  case 328:
#line 1532 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-3].en), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[-4].en), (yyvsp[0].wfs) ); }
#line 8827 "Parser/parser.cc"
    break;

  case 329:
#line 1534 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8833 "Parser/parser.cc"
    break;

  case 330:
#line 1536 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( nullptr, maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8839 "Parser/parser.cc"
    break;

  case 331:
#line 1539 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 8845 "Parser/parser.cc"
    break;

  case 332:
#line 1541 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-5].en), maybe_build_compound( (yyvsp[-4].sn) ), (yyvsp[-6].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8851 "Parser/parser.cc"
    break;

  case 333:
#line 1546 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-1].en), (yyvsp[0].sn), (yyvsp[-2].en) ) ); }
#line 8857 "Parser/parser.cc"
    break;

  case 334:
#line 1548 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-3].en), (yyvsp[-2].sn), (yyvsp[-4].en), (yyvsp[0].wfs) ) ); }
#line 8863 "Parser/parser.cc"
    break;

  case 335:
#line 1553 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), (yyvsp[0].sn), 0 ) ); }
#line 8869 "Parser/parser.cc"
    break;

  case 336:
#line 1555 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), 0, (yyvsp[0].sn) ) ); }
#line 8875 "Parser/parser.cc"
    break;

  case 337:
#line 1557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-2].sn), (yyvsp[-1].sn), (yyvsp[0].sn) ) ); }
#line 8881 "Parser/parser.cc"
    break;

  case 338:
#line 1562 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8887 "Parser/parser.cc"
    break;

  case 339:
#line 1564 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)(yyvsp[-8].sn)->set_last( new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ) ); }
#line 8893 "Parser/parser.cc"
    break;

  case 340:
#line 1569 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8899 "Parser/parser.cc"
    break;

  case 341:
#line 1570 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[0].en); }
#line 8905 "Parser/parser.cc"
    break;

  case 342:
#line 1574 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 8911 "Parser/parser.cc"
    break;

  case 343:
#line 1575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 8917 "Parser/parser.cc"
    break;

  case 344:
#line 1576 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.catch_kind) = CatchStmt::Resume; }
#line 8923 "Parser/parser.cc"
    break;

  case 345:
#line 1577 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Resume; }
#line 8929 "Parser/parser.cc"
    break;

  case 346:
#line 1581 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = new StatementNode( build_finally( (yyvsp[0].sn) ) ); }
#line 8935 "Parser/parser.cc"
    break;

  case 348:
#line 1588 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 8941 "Parser/parser.cc"
    break;

  case 349:
#line 1590 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 8947 "Parser/parser.cc"
    break;

  case 350:
#line 1592 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 8953 "Parser/parser.cc"
    break;

  case 355:
#line 1607 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-4].flag), (yyvsp[-2].constant), 0 ) ); }
#line 8959 "Parser/parser.cc"
    break;

  case 356:
#line 1609 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-6].flag), (yyvsp[-4].constant), (yyvsp[-2].en) ) ); }
#line 8965 "Parser/parser.cc"
    break;

  case 357:
#line 1611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-8].flag), (yyvsp[-6].constant), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 8971 "Parser/parser.cc"
    break;

  case 358:
#line 1613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-10].flag), (yyvsp[-8].constant), (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 8977 "Parser/parser.cc"
    break;

  case 359:
#line 1615 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-12].flag), (yyvsp[-9].constant), 0, (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].label) ) ); }
#line 8983 "Parser/parser.cc"
    break;

  case 360:
#line 1620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = false; }
#line 8989 "Parser/parser.cc"
    break;

  case 361:
#line 1622 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = true; }
#line 8995 "Parser/parser.cc"
    break;

  case 362:
#line 1627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9001 "Parser/parser.cc"
    break;

  case 365:
#line 1634 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 9007 "Parser/parser.cc"
    break;

  case 366:
#line 1639 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( nullptr, (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 9013 "Parser/parser.cc"
    break;

  case 367:
#line 1641 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( (yyvsp[-5].tok), (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 9019 "Parser/parser.cc"
    break;

  case 368:
#line 1646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9025 "Parser/parser.cc"
    break;

  case 369:
#line 1648 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 9031 "Parser/parser.cc"
    break;

  case 370:
#line 1650 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( (yyvsp[0].constant) ) )); }
#line 9037 "Parser/parser.cc"
    break;

  case 371:
#line 1655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = new LabelNode(); (yyval.label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 9046 "Parser/parser.cc"
    break;

  case 372:
#line 1660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = (yyvsp[-2].label); (yyvsp[-2].label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 9055 "Parser/parser.cc"
    break;

  case 373:
#line 1670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9061 "Parser/parser.cc"
    break;

  case 376:
#line 1677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 9067 "Parser/parser.cc"
    break;

  case 377:
#line 1682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9073 "Parser/parser.cc"
    break;

  case 379:
#line 1688 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 9079 "Parser/parser.cc"
    break;

  case 380:
#line 1690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-2].decl) ); }
#line 9085 "Parser/parser.cc"
    break;

  case 387:
#line 1710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "C_DECLARATION1 %p %s\n", $$, $$->name ? $$->name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 9096 "Parser/parser.cc"
    break;

  case 390:
#line 1722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-4].en), (yyvsp[-2].constant) ); }
#line 9102 "Parser/parser.cc"
    break;

  case 391:
#line 1724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-2].en), build_constantStr( *new string( "\"\"" ) ) ); }
#line 9108 "Parser/parser.cc"
    break;

  case 395:
#line 1742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9114 "Parser/parser.cc"
    break;

  case 397:
#line 1748 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].in) ); }
#line 9120 "Parser/parser.cc"
    break;

  case 398:
#line 1752 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 9126 "Parser/parser.cc"
    break;

  case 399:
#line 1754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].in) ) ); }
#line 9132 "Parser/parser.cc"
    break;

  case 400:
#line 1761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 9138 "Parser/parser.cc"
    break;

  case 401:
#line 1763 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 9144 "Parser/parser.cc"
    break;

  case 402:
#line 1765 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 9150 "Parser/parser.cc"
    break;

  case 404:
#line 1771 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9156 "Parser/parser.cc"
    break;

  case 405:
#line 1773 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9162 "Parser/parser.cc"
    break;

  case 406:
#line 1775 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 9168 "Parser/parser.cc"
    break;

  case 407:
#line 1777 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeClone( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->appendList( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 9179 "Parser/parser.cc"
    break;

  case 408:
#line 1810 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), 0 )->addQualifiers( (yyvsp[0].decl) ); }
#line 9185 "Parser/parser.cc"
    break;

  case 409:
#line 1812 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), 0 )->addQualifiers( (yyvsp[0].decl) ); }
#line 9191 "Parser/parser.cc"
    break;

  case 410:
#line 1817 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 9197 "Parser/parser.cc"
    break;

  case 411:
#line 1820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-6].decl)->appendList( (yyvsp[-2].decl) ) ); }
#line 9203 "Parser/parser.cc"
    break;

  case 412:
#line 1825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 9212 "Parser/parser.cc"
    break;

  case 413:
#line 1830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 9221 "Parser/parser.cc"
    break;

  case 414:
#line 1835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "3" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 9230 "Parser/parser.cc"
    break;

  case 415:
#line 1846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "4" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef();
		}
#line 9239 "Parser/parser.cc"
    break;

  case 416:
#line 1851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "5" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 9248 "Parser/parser.cc"
    break;

  case 417:
#line 1856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "6" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[-3].decl) )->addTypedef();
		}
#line 9257 "Parser/parser.cc"
    break;

  case 418:
#line 1861 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "7" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-2].decl) )->addTypedef();
		}
#line 9266 "Parser/parser.cc"
    break;

  case 419:
#line 1866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "8" );
			(yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-3].decl) )->addTypedef()->addType( (yyvsp[-3].decl) );
		}
#line 9275 "Parser/parser.cc"
    break;

  case 420:
#line 1875 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 9283 "Parser/parser.cc"
    break;

  case 421:
#line 1879 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 9291 "Parser/parser.cc"
    break;

  case 422:
#line 1886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 9297 "Parser/parser.cc"
    break;

  case 426:
#line 1896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 9303 "Parser/parser.cc"
    break;

  case 427:
#line 1898 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ) ); }
#line 9309 "Parser/parser.cc"
    break;

  case 435:
#line 1921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_type_specifier2 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 9320 "Parser/parser.cc"
    break;

  case 440:
#line 1943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9326 "Parser/parser.cc"
    break;

  case 443:
#line 1955 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9332 "Parser/parser.cc"
    break;

  case 446:
#line 1965 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Const ); }
#line 9338 "Parser/parser.cc"
    break;

  case 447:
#line 1967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Restrict ); }
#line 9344 "Parser/parser.cc"
    break;

  case 448:
#line 1969 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Volatile ); }
#line 9350 "Parser/parser.cc"
    break;

  case 449:
#line 1971 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Atomic ); }
#line 9356 "Parser/parser.cc"
    break;

  case 451:
#line 1977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newForall( (yyvsp[-1].decl) ); }
#line 9362 "Parser/parser.cc"
    break;

  case 453:
#line 1983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9368 "Parser/parser.cc"
    break;

  case 454:
#line 1985 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9374 "Parser/parser.cc"
    break;

  case 456:
#line 1996 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9380 "Parser/parser.cc"
    break;

  case 457:
#line 2001 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Extern ); }
#line 9386 "Parser/parser.cc"
    break;

  case 458:
#line 2003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Static ); }
#line 9392 "Parser/parser.cc"
    break;

  case 459:
#line 2005 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Auto ); }
#line 9398 "Parser/parser.cc"
    break;

  case 460:
#line 2007 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Register ); }
#line 9404 "Parser/parser.cc"
    break;

  case 461:
#line 2009 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Threadlocal ); }
#line 9410 "Parser/parser.cc"
    break;

  case 462:
#line 2012 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Inline ); }
#line 9416 "Parser/parser.cc"
    break;

  case 463:
#line 2014 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Fortran ); }
#line 9422 "Parser/parser.cc"
    break;

  case 464:
#line 2016 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Noreturn ); }
#line 9428 "Parser/parser.cc"
    break;

  case 465:
#line 2021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 9434 "Parser/parser.cc"
    break;

  case 466:
#line 2023 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Bool ); }
#line 9440 "Parser/parser.cc"
    break;

  case 467:
#line 2025 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Char ); }
#line 9446 "Parser/parser.cc"
    break;

  case 468:
#line 2027 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int ); }
#line 9452 "Parser/parser.cc"
    break;

  case 469:
#line 2029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 ); }
#line 9458 "Parser/parser.cc"
    break;

  case 470:
#line 2031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 )->addType( DeclarationNode::newSignedNess( DeclarationNode::Unsigned ) ); }
#line 9464 "Parser/parser.cc"
    break;

  case 471:
#line 2033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Float ); }
#line 9470 "Parser/parser.cc"
    break;

  case 472:
#line 2035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Double ); }
#line 9476 "Parser/parser.cc"
    break;

  case 473:
#line 2037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat80 ); }
#line 9482 "Parser/parser.cc"
    break;

  case 474:
#line 2039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat128 ); }
#line 9488 "Parser/parser.cc"
    break;

  case 475:
#line 2041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat16 ); }
#line 9494 "Parser/parser.cc"
    break;

  case 476:
#line 2043 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32 ); }
#line 9500 "Parser/parser.cc"
    break;

  case 477:
#line 2045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32x ); }
#line 9506 "Parser/parser.cc"
    break;

  case 478:
#line 2047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64 ); }
#line 9512 "Parser/parser.cc"
    break;

  case 479:
#line 2049 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64x ); }
#line 9518 "Parser/parser.cc"
    break;

  case 480:
#line 2051 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat128 ); }
#line 9524 "Parser/parser.cc"
    break;

  case 481:
#line 2053 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9530 "Parser/parser.cc"
    break;

  case 482:
#line 2055 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9536 "Parser/parser.cc"
    break;

  case 483:
#line 2057 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9542 "Parser/parser.cc"
    break;

  case 484:
#line 2059 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Complex ); }
#line 9548 "Parser/parser.cc"
    break;

  case 485:
#line 2061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Imaginary ); }
#line 9554 "Parser/parser.cc"
    break;

  case 486:
#line 2063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Signed ); }
#line 9560 "Parser/parser.cc"
    break;

  case 487:
#line 2065 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Unsigned ); }
#line 9566 "Parser/parser.cc"
    break;

  case 488:
#line 2067 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Short ); }
#line 9572 "Parser/parser.cc"
    break;

  case 489:
#line 2069 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Long ); }
#line 9578 "Parser/parser.cc"
    break;

  case 490:
#line 2071 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Valist ); }
#line 9584 "Parser/parser.cc"
    break;

  case 491:
#line 2073 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::AutoType ); }
#line 9590 "Parser/parser.cc"
    break;

  case 493:
#line 2079 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9596 "Parser/parser.cc"
    break;

  case 495:
#line 2085 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVtableType( (yyvsp[-2].decl) ); }
#line 9602 "Parser/parser.cc"
    break;

  case 496:
#line 2091 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9608 "Parser/parser.cc"
    break;

  case 497:
#line 2093 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9614 "Parser/parser.cc"
    break;

  case 499:
#line 2100 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9620 "Parser/parser.cc"
    break;

  case 500:
#line 2102 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9626 "Parser/parser.cc"
    break;

  case 501:
#line 2104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9632 "Parser/parser.cc"
    break;

  case 502:
#line 2106 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 9638 "Parser/parser.cc"
    break;

  case 504:
#line 2113 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9644 "Parser/parser.cc"
    break;

  case 506:
#line 2119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9650 "Parser/parser.cc"
    break;

  case 507:
#line 2121 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9656 "Parser/parser.cc"
    break;

  case 508:
#line 2123 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 9662 "Parser/parser.cc"
    break;

  case 509:
#line 2128 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 9668 "Parser/parser.cc"
    break;

  case 510:
#line 2130 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en) ); }
#line 9674 "Parser/parser.cc"
    break;

  case 511:
#line 2132 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 9680 "Parser/parser.cc"
    break;

  case 512:
#line 2134 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en), true ); }
#line 9686 "Parser/parser.cc"
    break;

  case 513:
#line 2136 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Zero ); }
#line 9692 "Parser/parser.cc"
    break;

  case 514:
#line 2138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::One ); }
#line 9698 "Parser/parser.cc"
    break;

  case 515:
#line 2143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_declaration_specifier %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 9709 "Parser/parser.cc"
    break;

  case 516:
#line 2150 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9715 "Parser/parser.cc"
    break;

  case 517:
#line 2152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9721 "Parser/parser.cc"
    break;

  case 518:
#line 2154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9727 "Parser/parser.cc"
    break;

  case 519:
#line 2159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_type_specifier %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 9738 "Parser/parser.cc"
    break;

  case 520:
#line 2166 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 9744 "Parser/parser.cc"
    break;

  case 521:
#line 2168 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 9750 "Parser/parser.cc"
    break;

  case 522:
#line 2170 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 9759 "Parser/parser.cc"
    break;

  case 524:
#line 2179 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9765 "Parser/parser.cc"
    break;

  case 525:
#line 2181 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9771 "Parser/parser.cc"
    break;

  case 526:
#line 2183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9777 "Parser/parser.cc"
    break;

  case 528:
#line 2189 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9783 "Parser/parser.cc"
    break;

  case 529:
#line 2191 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9789 "Parser/parser.cc"
    break;

  case 531:
#line 2197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9795 "Parser/parser.cc"
    break;

  case 532:
#line 2199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9801 "Parser/parser.cc"
    break;

  case 533:
#line 2201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9807 "Parser/parser.cc"
    break;

  case 535:
#line 2207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9813 "Parser/parser.cc"
    break;

  case 536:
#line 2209 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9819 "Parser/parser.cc"
    break;

  case 537:
#line 2214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypedef( (yyvsp[0].tok) ); }
#line 9825 "Parser/parser.cc"
    break;

  case 538:
#line 2216 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 9831 "Parser/parser.cc"
    break;

  case 539:
#line 2218 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 9837 "Parser/parser.cc"
    break;

  case 541:
#line 2221 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), (yyvsp[0].decl) ); }
#line 9843 "Parser/parser.cc"
    break;

  case 542:
#line 2223 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), (yyvsp[0].decl) ); }
#line 9849 "Parser/parser.cc"
    break;

  case 543:
#line 2228 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[0].tok), nullptr ); }
#line 9855 "Parser/parser.cc"
    break;

  case 544:
#line 2230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-2].tok), nullptr ); }
#line 9861 "Parser/parser.cc"
    break;

  case 545:
#line 2232 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 9867 "Parser/parser.cc"
    break;

  case 546:
#line 2237 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "elaborated_type %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 9878 "Parser/parser.cc"
    break;

  case 550:
#line 2253 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 9884 "Parser/parser.cc"
    break;

  case 551:
#line 2255 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 9890 "Parser/parser.cc"
    break;

  case 552:
#line 2257 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 9899 "Parser/parser.cc"
    break;

  case 553:
#line 2262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "aggregate_type1 %s\n", $3.str->c_str() );
			// if ( $2 )
			// 	for ( Attribute * attr: reverseIterate( $2->attributes ) ) {
			// 		printf( "copySpecifiers12 %s\n", attr->name.c_str() );
			// 	} // for
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
			// printf( "aggregate_type2 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			// 	printf( "aggregate_type3 %s\n", attr->name.c_str() );
			// } // for
		}
#line 9916 "Parser/parser.cc"
    break;

  case 554:
#line 2275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 9925 "Parser/parser.cc"
    break;

  case 555:
#line 2280 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "AGG3\n" );
			DeclarationNode::newFromTypedef( (yyvsp[-5].tok) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 9935 "Parser/parser.cc"
    break;

  case 556:
#line 2286 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 9944 "Parser/parser.cc"
    break;

  case 557:
#line 2291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "AGG4\n" );
			DeclarationNode::newFromTypeGen( (yyvsp[-5].tok), nullptr );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 9954 "Parser/parser.cc"
    break;

  case 559:
#line 2301 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9960 "Parser/parser.cc"
    break;

  case 560:
#line 2303 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 9966 "Parser/parser.cc"
    break;

  case 561:
#line 2308 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 9976 "Parser/parser.cc"
    break;

  case 562:
#line 2314 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 9991 "Parser/parser.cc"
    break;

  case 565:
#line 2333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Struct; }
#line 9997 "Parser/parser.cc"
    break;

  case 566:
#line 2335 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Union; }
#line 10003 "Parser/parser.cc"
    break;

  case 567:
#line 2337 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Exception; }
#line 10009 "Parser/parser.cc"
    break;

  case 568:
#line 2343 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 10015 "Parser/parser.cc"
    break;

  case 569:
#line 2345 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 10021 "Parser/parser.cc"
    break;

  case 570:
#line 2347 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Generator; }
#line 10027 "Parser/parser.cc"
    break;

  case 571:
#line 2349 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor generator is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 10033 "Parser/parser.cc"
    break;

  case 572:
#line 2351 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Coroutine; }
#line 10039 "Parser/parser.cc"
    break;

  case 573:
#line 2353 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor coroutine is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 10045 "Parser/parser.cc"
    break;

  case 574:
#line 2355 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Thread; }
#line 10051 "Parser/parser.cc"
    break;

  case 575:
#line 2357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor thread is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 10057 "Parser/parser.cc"
    break;

  case 576:
#line 2362 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10063 "Parser/parser.cc"
    break;

  case 577:
#line 2364 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 10069 "Parser/parser.cc"
    break;

  case 578:
#line 2369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "type_specifier1 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			(yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) );
			// printf( "type_specifier2 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10082 "Parser/parser.cc"
    break;

  case 579:
#line 2378 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 10088 "Parser/parser.cc"
    break;

  case 580:
#line 2380 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-1].decl) ) {								// field declarator ?
				(yyvsp[-1].decl) = DeclarationNode::newName( nullptr );
			} // if
			(yyvsp[-1].decl)->inLine = true;
			(yyval.decl) = distAttr( (yyvsp[-2].decl), (yyvsp[-1].decl) );					// mark all fields in list
			distInl( (yyvsp[-1].decl) );
		}
#line 10101 "Parser/parser.cc"
    break;

  case 581:
#line 2389 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10107 "Parser/parser.cc"
    break;

  case 584:
#line 2393 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 10113 "Parser/parser.cc"
    break;

  case 585:
#line 2395 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10119 "Parser/parser.cc"
    break;

  case 588:
#line 2402 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10125 "Parser/parser.cc"
    break;

  case 590:
#line 2405 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 10131 "Parser/parser.cc"
    break;

  case 591:
#line 2410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].en) ); }
#line 10137 "Parser/parser.cc"
    break;

  case 592:
#line 2413 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 10143 "Parser/parser.cc"
    break;

  case 593:
#line 2416 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 10149 "Parser/parser.cc"
    break;

  case 594:
#line 2421 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10155 "Parser/parser.cc"
    break;

  case 596:
#line 2424 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 10161 "Parser/parser.cc"
    break;

  case 598:
#line 2435 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 10167 "Parser/parser.cc"
    break;

  case 599:
#line 2437 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 10173 "Parser/parser.cc"
    break;

  case 601:
#line 2444 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 10179 "Parser/parser.cc"
    break;

  case 602:
#line 2449 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 10185 "Parser/parser.cc"
    break;

  case 604:
#line 2455 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 10191 "Parser/parser.cc"
    break;

  case 605:
#line 2460 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10197 "Parser/parser.cc"
    break;

  case 606:
#line 2462 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); }
#line 10203 "Parser/parser.cc"
    break;

  case 607:
#line 2464 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].tok), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) ); }
#line 10209 "Parser/parser.cc"
    break;

  case 608:
#line 2467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-4].decl)->name, (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 10215 "Parser/parser.cc"
    break;

  case 609:
#line 2469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Unvalued enumerated type is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10221 "Parser/parser.cc"
    break;

  case 610:
#line 2471 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-6].decl)->storageClasses.val != 0 || (yyvsp[-6].decl)->type->qualifiers.val != 0 )
			{ SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }

			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 10232 "Parser/parser.cc"
    break;

  case 611:
#line 2478 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].decl)->storageClasses.val != 0 || (yyvsp[-4].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			typedefTable.makeTypedef( *(yyvsp[-1].tok) );
		}
#line 10241 "Parser/parser.cc"
    break;

  case 612:
#line 2483 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, (yyvsp[-9].decl) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 10249 "Parser/parser.cc"
    break;

  case 613:
#line 2487 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-8].decl)->storageClasses.val != 0 || (yyvsp[-8].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			typedefTable.makeTypedef( *(yyvsp[-5].decl)->name );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].decl)->name, (yyvsp[-2].decl), true, (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 10259 "Parser/parser.cc"
    break;

  case 615:
#line 2497 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); (yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), 0, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 10265 "Parser/parser.cc"
    break;

  case 616:
#line 2499 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].decl)->type->symbolic.name );	(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].decl)->type->symbolic.name, 0, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 10271 "Parser/parser.cc"
    break;

  case 617:
#line 2504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].in) ); }
#line 10277 "Parser/parser.cc"
    break;

  case 618:
#line 2506 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( new string("inline"), nullptr ); }
#line 10283 "Parser/parser.cc"
    break;

  case 619:
#line 2508 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].in) ) ); }
#line 10289 "Parser/parser.cc"
    break;

  case 620:
#line 2510 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( DeclarationNode::newEnumValueGeneric( new string("inline"), nullptr ) ); }
#line 10295 "Parser/parser.cc"
    break;

  case 621:
#line 2515 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 10301 "Parser/parser.cc"
    break;

  case 622:
#line 2516 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = new InitializerNode( (yyvsp[0].en) ); }
#line 10307 "Parser/parser.cc"
    break;

  case 623:
#line 2517 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                     { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 10313 "Parser/parser.cc"
    break;

  case 624:
#line 2524 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 10319 "Parser/parser.cc"
    break;

  case 625:
#line 2526 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10325 "Parser/parser.cc"
    break;

  case 628:
#line 2530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10331 "Parser/parser.cc"
    break;

  case 629:
#line 2532 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 10337 "Parser/parser.cc"
    break;

  case 630:
#line 2534 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 10343 "Parser/parser.cc"
    break;

  case 632:
#line 2542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10349 "Parser/parser.cc"
    break;

  case 633:
#line 2544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10355 "Parser/parser.cc"
    break;

  case 634:
#line 2546 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-8].decl)->appendList( (yyvsp[-4].decl) )->appendList( (yyvsp[0].decl) ); }
#line 10361 "Parser/parser.cc"
    break;

  case 636:
#line 2552 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10367 "Parser/parser.cc"
    break;

  case 637:
#line 2557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10373 "Parser/parser.cc"
    break;

  case 638:
#line 2559 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10379 "Parser/parser.cc"
    break;

  case 640:
#line 2562 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 10385 "Parser/parser.cc"
    break;

  case 643:
#line 2569 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10391 "Parser/parser.cc"
    break;

  case 644:
#line 2571 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10397 "Parser/parser.cc"
    break;

  case 646:
#line 2580 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 10403 "Parser/parser.cc"
    break;

  case 647:
#line 2583 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 10409 "Parser/parser.cc"
    break;

  case 648:
#line 2585 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 10415 "Parser/parser.cc"
    break;

  case 653:
#line 2595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10421 "Parser/parser.cc"
    break;

  case 655:
#line 2602 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 10427 "Parser/parser.cc"
    break;

  case 656:
#line 2604 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 10433 "Parser/parser.cc"
    break;

  case 657:
#line 2609 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 10439 "Parser/parser.cc"
    break;

  case 658:
#line 2611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 10445 "Parser/parser.cc"
    break;

  case 659:
#line 2620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 10451 "Parser/parser.cc"
    break;

  case 660:
#line 2622 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 10457 "Parser/parser.cc"
    break;

  case 666:
#line 2635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10463 "Parser/parser.cc"
    break;

  case 669:
#line 2645 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 10469 "Parser/parser.cc"
    break;

  case 670:
#line 2646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (yyvsp[-1].op) == OperKinds::Assign ? (yyvsp[0].in) : (yyvsp[0].in)->set_maybeConstructed( false ); }
#line 10475 "Parser/parser.cc"
    break;

  case 671:
#line 2647 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.in) = new InitializerNode( true ); }
#line 10481 "Parser/parser.cc"
    break;

  case 672:
#line 2648 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 10487 "Parser/parser.cc"
    break;

  case 673:
#line 2652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = new InitializerNode( (yyvsp[0].en) ); }
#line 10493 "Parser/parser.cc"
    break;

  case 674:
#line 2653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 10499 "Parser/parser.cc"
    break;

  case 675:
#line 2658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 10505 "Parser/parser.cc"
    break;

  case 677:
#line 2660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ); }
#line 10511 "Parser/parser.cc"
    break;

  case 678:
#line 2661 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (InitializerNode *)( (yyvsp[-2].in)->set_last( (yyvsp[0].in) ) ); }
#line 10517 "Parser/parser.cc"
    break;

  case 679:
#line 2662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.in) = (InitializerNode *)((yyvsp[-3].in)->set_last( (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ) )); }
#line 10523 "Parser/parser.cc"
    break;

  case 681:
#line 2678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[-1].tok) ) ); }
#line 10529 "Parser/parser.cc"
    break;

  case 683:
#line 2684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-1].en)->set_last( (yyvsp[0].en) )); }
#line 10535 "Parser/parser.cc"
    break;

  case 684:
#line 2690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 10541 "Parser/parser.cc"
    break;

  case 685:
#line 2693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 10547 "Parser/parser.cc"
    break;

  case 686:
#line 2695 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 10553 "Parser/parser.cc"
    break;

  case 687:
#line 2697 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-4].en) ), maybeMoveBuild<Expression>( (yyvsp[-2].en) ) ) ); }
#line 10559 "Parser/parser.cc"
    break;

  case 688:
#line 2699 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 10565 "Parser/parser.cc"
    break;

  case 690:
#line 2723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl) ); }
#line 10571 "Parser/parser.cc"
    break;

  case 691:
#line 2728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10577 "Parser/parser.cc"
    break;

  case 692:
#line 2730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 10583 "Parser/parser.cc"
    break;

  case 693:
#line 2735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "9" );
			if ( (yyvsp[-1].tclass) == TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 10594 "Parser/parser.cc"
    break;

  case 694:
#line 2742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 10600 "Parser/parser.cc"
    break;

  case 695:
#line 2744 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "9" ); }
#line 10606 "Parser/parser.cc"
    break;

  case 696:
#line 2746 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 10612 "Parser/parser.cc"
    break;

  case 697:
#line 2748 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "9" );
			(yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 10621 "Parser/parser.cc"
    break;

  case 698:
#line 2754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 10627 "Parser/parser.cc"
    break;

  case 699:
#line 2759 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 10633 "Parser/parser.cc"
    break;

  case 700:
#line 2761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 10639 "Parser/parser.cc"
    break;

  case 701:
#line 2763 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::DStype; }
#line 10645 "Parser/parser.cc"
    break;

  case 702:
#line 2767 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 10651 "Parser/parser.cc"
    break;

  case 703:
#line 2772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 10657 "Parser/parser.cc"
    break;

  case 704:
#line 2774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 10663 "Parser/parser.cc"
    break;

  case 705:
#line 2776 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ftype; }
#line 10669 "Parser/parser.cc"
    break;

  case 706:
#line 2778 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 10675 "Parser/parser.cc"
    break;

  case 707:
#line 2783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10681 "Parser/parser.cc"
    break;

  case 710:
#line 2790 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 10687 "Parser/parser.cc"
    break;

  case 711:
#line 2795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 10693 "Parser/parser.cc"
    break;

  case 712:
#line 2797 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 10699 "Parser/parser.cc"
    break;

  case 713:
#line 2804 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 10705 "Parser/parser.cc"
    break;

  case 715:
#line 2807 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 10711 "Parser/parser.cc"
    break;

  case 716:
#line 2809 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)( (yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 10717 "Parser/parser.cc"
    break;

  case 717:
#line 2814 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 10723 "Parser/parser.cc"
    break;

  case 718:
#line 2816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10729 "Parser/parser.cc"
    break;

  case 719:
#line 2818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 10735 "Parser/parser.cc"
    break;

  case 720:
#line 2823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 10741 "Parser/parser.cc"
    break;

  case 721:
#line 2825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 10747 "Parser/parser.cc"
    break;

  case 722:
#line 2830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "10" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), 0 );
		}
#line 10756 "Parser/parser.cc"
    break;

  case 723:
#line 2835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "11" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 10765 "Parser/parser.cc"
    break;

  case 724:
#line 2843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), 0 ); }
#line 10771 "Parser/parser.cc"
    break;

  case 725:
#line 2845 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-8].tok), (yyvsp[-6].decl), (yyvsp[-2].decl) ); }
#line 10777 "Parser/parser.cc"
    break;

  case 727:
#line 2851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl) ); }
#line 10783 "Parser/parser.cc"
    break;

  case 732:
#line 2863 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 10789 "Parser/parser.cc"
    break;

  case 733:
#line 2868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10795 "Parser/parser.cc"
    break;

  case 734:
#line 2870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 10801 "Parser/parser.cc"
    break;

  case 736:
#line 2878 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl);	}
#line 10807 "Parser/parser.cc"
    break;

  case 737:
#line 2883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10813 "Parser/parser.cc"
    break;

  case 738:
#line 2885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->appendList( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 10819 "Parser/parser.cc"
    break;

  case 739:
#line 2890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10825 "Parser/parser.cc"
    break;

  case 741:
#line 2895 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 10831 "Parser/parser.cc"
    break;

  case 742:
#line 2899 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 10837 "Parser/parser.cc"
    break;

  case 743:
#line 2904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( (yyvsp[0].tok) ) ) ); }
#line 10843 "Parser/parser.cc"
    break;

  case 746:
#line 2908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 10852 "Parser/parser.cc"
    break;

  case 747:
#line 2913 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( false, (yyvsp[-2].constant), 0 ) ) ); }
#line 10858 "Parser/parser.cc"
    break;

  case 748:
#line 2915 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = LinkageSpec::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 10867 "Parser/parser.cc"
    break;

  case 749:
#line 2920 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-1].decl);
		}
#line 10877 "Parser/parser.cc"
    break;

  case 750:
#line 2926 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = LinkageSpec::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 10886 "Parser/parser.cc"
    break;

  case 751:
#line 2931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10896 "Parser/parser.cc"
    break;

  case 752:
#line 2937 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 10905 "Parser/parser.cc"
    break;

  case 753:
#line 2942 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10915 "Parser/parser.cc"
    break;

  case 754:
#line 2948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 10924 "Parser/parser.cc"
    break;

  case 755:
#line 2953 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10934 "Parser/parser.cc"
    break;

  case 756:
#line 2959 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.val) || (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 10943 "Parser/parser.cc"
    break;

  case 757:
#line 2964 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10953 "Parser/parser.cc"
    break;

  case 759:
#line 2979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].sn) ); }
#line 10959 "Parser/parser.cc"
    break;

  case 760:
#line 2981 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].sn) ); }
#line 10965 "Parser/parser.cc"
    break;

  case 761:
#line 2986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; forall = false; }
#line 10971 "Parser/parser.cc"
    break;

  case 762:
#line 2988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.en) = (yyvsp[-2].en); forall = false;
			if ( (yyvsp[0].decl) ) {
				SemanticError( yylloc, "Attributes cannot be associated with function body. Move attribute(s) before \"with\" clause." );
				(yyval.en) = nullptr;
			} // if
		}
#line 10983 "Parser/parser.cc"
    break;

  case 763:
#line 2999 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 10994 "Parser/parser.cc"
    break;

  case 764:
#line 3006 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 11003 "Parser/parser.cc"
    break;

  case 765:
#line 3011 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 11012 "Parser/parser.cc"
    break;

  case 766:
#line 3017 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 11018 "Parser/parser.cc"
    break;

  case 767:
#line 3020 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 11024 "Parser/parser.cc"
    break;

  case 768:
#line 3023 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 11030 "Parser/parser.cc"
    break;

  case 769:
#line 3027 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-4].decl) );
		}
#line 11039 "Parser/parser.cc"
    break;

  case 770:
#line 3033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 11045 "Parser/parser.cc"
    break;

  case 771:
#line 3036 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 11051 "Parser/parser.cc"
    break;

  case 772:
#line 3039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 11057 "Parser/parser.cc"
    break;

  case 776:
#line 3050 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 11063 "Parser/parser.cc"
    break;

  case 777:
#line 3055 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11069 "Parser/parser.cc"
    break;

  case 778:
#line 3057 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = (yyvsp[-2].constant);
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 11079 "Parser/parser.cc"
    break;

  case 779:
#line 3066 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11085 "Parser/parser.cc"
    break;

  case 782:
#line 3073 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11091 "Parser/parser.cc"
    break;

  case 783:
#line 3078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 11097 "Parser/parser.cc"
    break;

  case 785:
#line 3084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11103 "Parser/parser.cc"
    break;

  case 786:
#line 3089 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11109 "Parser/parser.cc"
    break;

  case 787:
#line 3091 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 11115 "Parser/parser.cc"
    break;

  case 788:
#line 3093 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 11121 "Parser/parser.cc"
    break;

  case 793:
#line 3102 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 11127 "Parser/parser.cc"
    break;

  case 794:
#line 3104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 11133 "Parser/parser.cc"
    break;

  case 795:
#line 3139 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11139 "Parser/parser.cc"
    break;

  case 796:
#line 3141 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11145 "Parser/parser.cc"
    break;

  case 797:
#line 3146 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11151 "Parser/parser.cc"
    break;

  case 799:
#line 3149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11157 "Parser/parser.cc"
    break;

  case 800:
#line 3151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11163 "Parser/parser.cc"
    break;

  case 801:
#line 3156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11169 "Parser/parser.cc"
    break;

  case 802:
#line 3158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11175 "Parser/parser.cc"
    break;

  case 803:
#line 3160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11181 "Parser/parser.cc"
    break;

  case 804:
#line 3162 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11187 "Parser/parser.cc"
    break;

  case 805:
#line 3167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11193 "Parser/parser.cc"
    break;

  case 806:
#line 3169 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11199 "Parser/parser.cc"
    break;

  case 807:
#line 3171 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11205 "Parser/parser.cc"
    break;

  case 808:
#line 3173 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11211 "Parser/parser.cc"
    break;

  case 809:
#line 3175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11217 "Parser/parser.cc"
    break;

  case 810:
#line 3177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11223 "Parser/parser.cc"
    break;

  case 811:
#line 3179 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11229 "Parser/parser.cc"
    break;

  case 812:
#line 3184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11235 "Parser/parser.cc"
    break;

  case 813:
#line 3186 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 11241 "Parser/parser.cc"
    break;

  case 814:
#line 3188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11247 "Parser/parser.cc"
    break;

  case 815:
#line 3190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11253 "Parser/parser.cc"
    break;

  case 816:
#line 3199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11259 "Parser/parser.cc"
    break;

  case 818:
#line 3202 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11265 "Parser/parser.cc"
    break;

  case 819:
#line 3207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11271 "Parser/parser.cc"
    break;

  case 820:
#line 3209 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11277 "Parser/parser.cc"
    break;

  case 821:
#line 3211 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 11283 "Parser/parser.cc"
    break;

  case 822:
#line 3213 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11289 "Parser/parser.cc"
    break;

  case 823:
#line 3215 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11295 "Parser/parser.cc"
    break;

  case 824:
#line 3220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11301 "Parser/parser.cc"
    break;

  case 825:
#line 3222 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11307 "Parser/parser.cc"
    break;

  case 826:
#line 3224 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11313 "Parser/parser.cc"
    break;

  case 827:
#line 3226 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11319 "Parser/parser.cc"
    break;

  case 828:
#line 3231 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11325 "Parser/parser.cc"
    break;

  case 829:
#line 3233 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11331 "Parser/parser.cc"
    break;

  case 830:
#line 3235 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11337 "Parser/parser.cc"
    break;

  case 831:
#line 3237 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11343 "Parser/parser.cc"
    break;

  case 832:
#line 3239 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11349 "Parser/parser.cc"
    break;

  case 833:
#line 3241 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11355 "Parser/parser.cc"
    break;

  case 837:
#line 3259 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 11361 "Parser/parser.cc"
    break;

  case 838:
#line 3261 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11367 "Parser/parser.cc"
    break;

  case 839:
#line 3263 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 11373 "Parser/parser.cc"
    break;

  case 840:
#line 3265 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11379 "Parser/parser.cc"
    break;

  case 841:
#line 3267 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11385 "Parser/parser.cc"
    break;

  case 842:
#line 3272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11391 "Parser/parser.cc"
    break;

  case 843:
#line 3274 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11397 "Parser/parser.cc"
    break;

  case 844:
#line 3276 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11403 "Parser/parser.cc"
    break;

  case 845:
#line 3278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11409 "Parser/parser.cc"
    break;

  case 846:
#line 3283 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11415 "Parser/parser.cc"
    break;

  case 847:
#line 3285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11421 "Parser/parser.cc"
    break;

  case 848:
#line 3287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11427 "Parser/parser.cc"
    break;

  case 849:
#line 3289 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11433 "Parser/parser.cc"
    break;

  case 850:
#line 3291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11439 "Parser/parser.cc"
    break;

  case 851:
#line 3293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11445 "Parser/parser.cc"
    break;

  case 852:
#line 3308 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "ID" );
		}
#line 11454 "Parser/parser.cc"
    break;

  case 853:
#line 3313 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11460 "Parser/parser.cc"
    break;

  case 854:
#line 3318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11466 "Parser/parser.cc"
    break;

  case 856:
#line 3321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11472 "Parser/parser.cc"
    break;

  case 857:
#line 3323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11478 "Parser/parser.cc"
    break;

  case 858:
#line 3328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11484 "Parser/parser.cc"
    break;

  case 859:
#line 3330 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11490 "Parser/parser.cc"
    break;

  case 860:
#line 3332 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11496 "Parser/parser.cc"
    break;

  case 861:
#line 3334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11502 "Parser/parser.cc"
    break;

  case 862:
#line 3339 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11508 "Parser/parser.cc"
    break;

  case 863:
#line 3341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11514 "Parser/parser.cc"
    break;

  case 864:
#line 3343 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11520 "Parser/parser.cc"
    break;

  case 865:
#line 3345 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11526 "Parser/parser.cc"
    break;

  case 866:
#line 3347 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11532 "Parser/parser.cc"
    break;

  case 867:
#line 3349 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11538 "Parser/parser.cc"
    break;

  case 868:
#line 3351 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11544 "Parser/parser.cc"
    break;

  case 869:
#line 3356 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11550 "Parser/parser.cc"
    break;

  case 870:
#line 3358 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11556 "Parser/parser.cc"
    break;

  case 871:
#line 3360 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 11562 "Parser/parser.cc"
    break;

  case 872:
#line 3362 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11568 "Parser/parser.cc"
    break;

  case 873:
#line 3364 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11574 "Parser/parser.cc"
    break;

  case 874:
#line 3374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11580 "Parser/parser.cc"
    break;

  case 875:
#line 3376 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11586 "Parser/parser.cc"
    break;

  case 877:
#line 3379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11592 "Parser/parser.cc"
    break;

  case 878:
#line 3381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11598 "Parser/parser.cc"
    break;

  case 879:
#line 3386 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11604 "Parser/parser.cc"
    break;

  case 880:
#line 3388 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11610 "Parser/parser.cc"
    break;

  case 881:
#line 3390 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11616 "Parser/parser.cc"
    break;

  case 882:
#line 3395 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11622 "Parser/parser.cc"
    break;

  case 883:
#line 3397 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11628 "Parser/parser.cc"
    break;

  case 884:
#line 3399 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11634 "Parser/parser.cc"
    break;

  case 885:
#line 3401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11640 "Parser/parser.cc"
    break;

  case 886:
#line 3406 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11646 "Parser/parser.cc"
    break;

  case 887:
#line 3408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11652 "Parser/parser.cc"
    break;

  case 888:
#line 3410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11658 "Parser/parser.cc"
    break;

  case 889:
#line 3424 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11664 "Parser/parser.cc"
    break;

  case 890:
#line 3426 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11670 "Parser/parser.cc"
    break;

  case 892:
#line 3429 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11676 "Parser/parser.cc"
    break;

  case 893:
#line 3431 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11682 "Parser/parser.cc"
    break;

  case 894:
#line 3436 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11688 "Parser/parser.cc"
    break;

  case 895:
#line 3438 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11694 "Parser/parser.cc"
    break;

  case 896:
#line 3443 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11700 "Parser/parser.cc"
    break;

  case 897:
#line 3445 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11706 "Parser/parser.cc"
    break;

  case 898:
#line 3447 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11712 "Parser/parser.cc"
    break;

  case 899:
#line 3452 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11718 "Parser/parser.cc"
    break;

  case 900:
#line 3454 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11724 "Parser/parser.cc"
    break;

  case 901:
#line 3459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11730 "Parser/parser.cc"
    break;

  case 902:
#line 3461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11736 "Parser/parser.cc"
    break;

  case 904:
#line 3479 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11742 "Parser/parser.cc"
    break;

  case 905:
#line 3481 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11748 "Parser/parser.cc"
    break;

  case 906:
#line 3486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( 0, (yyvsp[0].op) ); }
#line 11754 "Parser/parser.cc"
    break;

  case 907:
#line 3488 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 11760 "Parser/parser.cc"
    break;

  case 908:
#line 3490 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11766 "Parser/parser.cc"
    break;

  case 909:
#line 3492 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11772 "Parser/parser.cc"
    break;

  case 910:
#line 3494 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11778 "Parser/parser.cc"
    break;

  case 912:
#line 3500 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11784 "Parser/parser.cc"
    break;

  case 913:
#line 3502 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11790 "Parser/parser.cc"
    break;

  case 914:
#line 3504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11796 "Parser/parser.cc"
    break;

  case 915:
#line 3509 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 11802 "Parser/parser.cc"
    break;

  case 916:
#line 3511 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11808 "Parser/parser.cc"
    break;

  case 917:
#line 3513 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11814 "Parser/parser.cc"
    break;

  case 918:
#line 3519 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false ); }
#line 11820 "Parser/parser.cc"
    break;

  case 919:
#line 3521 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false )->addArray( (yyvsp[0].decl) ); }
#line 11826 "Parser/parser.cc"
    break;

  case 920:
#line 3523 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-4].en), 0, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].en), 0, false ) ); }
#line 11832 "Parser/parser.cc"
    break;

  case 922:
#line 3530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), 0, false ); }
#line 11838 "Parser/parser.cc"
    break;

  case 923:
#line 3532 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 11844 "Parser/parser.cc"
    break;

  case 924:
#line 3534 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newArray( (yyvsp[-2].en), 0, false ) ); }
#line 11850 "Parser/parser.cc"
    break;

  case 925:
#line 3536 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 11856 "Parser/parser.cc"
    break;

  case 927:
#line 3571 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 11862 "Parser/parser.cc"
    break;

  case 928:
#line 3573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11868 "Parser/parser.cc"
    break;

  case 929:
#line 3575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11874 "Parser/parser.cc"
    break;

  case 930:
#line 3580 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].op) ); }
#line 11880 "Parser/parser.cc"
    break;

  case 931:
#line 3582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 11886 "Parser/parser.cc"
    break;

  case 932:
#line 3584 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 11892 "Parser/parser.cc"
    break;

  case 933:
#line 3586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11898 "Parser/parser.cc"
    break;

  case 934:
#line 3588 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11904 "Parser/parser.cc"
    break;

  case 936:
#line 3594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11910 "Parser/parser.cc"
    break;

  case 937:
#line 3596 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11916 "Parser/parser.cc"
    break;

  case 938:
#line 3598 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11922 "Parser/parser.cc"
    break;

  case 939:
#line 3603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 11928 "Parser/parser.cc"
    break;

  case 940:
#line 3605 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11934 "Parser/parser.cc"
    break;

  case 941:
#line 3607 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11940 "Parser/parser.cc"
    break;

  case 943:
#line 3614 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11946 "Parser/parser.cc"
    break;

  case 945:
#line 3625 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false ); }
#line 11952 "Parser/parser.cc"
    break;

  case 946:
#line 3628 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 11958 "Parser/parser.cc"
    break;

  case 947:
#line 3630 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, (yyvsp[-2].decl), false ); }
#line 11964 "Parser/parser.cc"
    break;

  case 948:
#line 3633 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 11970 "Parser/parser.cc"
    break;

  case 949:
#line 3635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 11976 "Parser/parser.cc"
    break;

  case 950:
#line 3637 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-4].decl), true ); }
#line 11982 "Parser/parser.cc"
    break;

  case 952:
#line 3652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11988 "Parser/parser.cc"
    break;

  case 953:
#line 3654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11994 "Parser/parser.cc"
    break;

  case 954:
#line 3659 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( 0, (yyvsp[0].op) ); }
#line 12000 "Parser/parser.cc"
    break;

  case 955:
#line 3661 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 12006 "Parser/parser.cc"
    break;

  case 956:
#line 3663 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12012 "Parser/parser.cc"
    break;

  case 957:
#line 3665 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12018 "Parser/parser.cc"
    break;

  case 958:
#line 3667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12024 "Parser/parser.cc"
    break;

  case 960:
#line 3673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12030 "Parser/parser.cc"
    break;

  case 961:
#line 3675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12036 "Parser/parser.cc"
    break;

  case 962:
#line 3677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12042 "Parser/parser.cc"
    break;

  case 963:
#line 3682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12048 "Parser/parser.cc"
    break;

  case 964:
#line 3684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12054 "Parser/parser.cc"
    break;

  case 967:
#line 3694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12060 "Parser/parser.cc"
    break;

  case 970:
#line 3705 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12066 "Parser/parser.cc"
    break;

  case 971:
#line 3707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12072 "Parser/parser.cc"
    break;

  case 972:
#line 3709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12078 "Parser/parser.cc"
    break;

  case 973:
#line 3711 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12084 "Parser/parser.cc"
    break;

  case 974:
#line 3713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12090 "Parser/parser.cc"
    break;

  case 975:
#line 3715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12096 "Parser/parser.cc"
    break;

  case 976:
#line 3722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 12102 "Parser/parser.cc"
    break;

  case 977:
#line 3724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12108 "Parser/parser.cc"
    break;

  case 978:
#line 3726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 12114 "Parser/parser.cc"
    break;

  case 979:
#line 3728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 12120 "Parser/parser.cc"
    break;

  case 980:
#line 3730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12126 "Parser/parser.cc"
    break;

  case 981:
#line 3733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 12132 "Parser/parser.cc"
    break;

  case 982:
#line 3735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12138 "Parser/parser.cc"
    break;

  case 983:
#line 3737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 12144 "Parser/parser.cc"
    break;

  case 984:
#line 3739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 12150 "Parser/parser.cc"
    break;

  case 985:
#line 3741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12156 "Parser/parser.cc"
    break;

  case 986:
#line 3746 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 12162 "Parser/parser.cc"
    break;

  case 987:
#line 3748 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 12168 "Parser/parser.cc"
    break;

  case 988:
#line 3753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 12174 "Parser/parser.cc"
    break;

  case 989:
#line 3755 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl)->addQualifiers( (yyvsp[-4].decl) ), true ); }
#line 12180 "Parser/parser.cc"
    break;

  case 991:
#line 3782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12186 "Parser/parser.cc"
    break;

  case 995:
#line 3793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12192 "Parser/parser.cc"
    break;

  case 996:
#line 3795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12198 "Parser/parser.cc"
    break;

  case 997:
#line 3797 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12204 "Parser/parser.cc"
    break;

  case 998:
#line 3799 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12210 "Parser/parser.cc"
    break;

  case 999:
#line 3801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12216 "Parser/parser.cc"
    break;

  case 1000:
#line 3803 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12222 "Parser/parser.cc"
    break;

  case 1001:
#line 3810 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12228 "Parser/parser.cc"
    break;

  case 1002:
#line 3812 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12234 "Parser/parser.cc"
    break;

  case 1003:
#line 3814 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12240 "Parser/parser.cc"
    break;

  case 1004:
#line 3816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12246 "Parser/parser.cc"
    break;

  case 1005:
#line 3818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12252 "Parser/parser.cc"
    break;

  case 1006:
#line 3820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12258 "Parser/parser.cc"
    break;

  case 1007:
#line 3825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 12264 "Parser/parser.cc"
    break;

  case 1008:
#line 3827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 12270 "Parser/parser.cc"
    break;

  case 1009:
#line 3829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 12276 "Parser/parser.cc"
    break;

  case 1010:
#line 3836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 12282 "Parser/parser.cc"
    break;

  case 1011:
#line 3838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 12288 "Parser/parser.cc"
    break;

  case 1014:
#line 3862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 12294 "Parser/parser.cc"
    break;

  case 1015:
#line 3864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 12300 "Parser/parser.cc"
    break;


#line 12304 "Parser/parser.cc"

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
#line 3867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "make install" //
// End: //
