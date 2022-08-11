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
#define UPDOWN( compop, left, right ) (compop == OperKinds::LThan || compop == OperKinds::LEThan ? left : right)

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

#line 294 "Parser/parser.cc"

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
#line 265 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"

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

#line 662 "Parser/parser.cc"

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
#define YYLAST   21110

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  175
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  291
/* YYNRULES -- Number of rules.  */
#define YYNRULES  1020
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  2063

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
       0,   564,   564,   568,   575,   576,   577,   578,   579,   583,
     584,   585,   586,   587,   588,   589,   593,   594,   598,   599,
     604,   608,   609,   620,   622,   624,   628,   629,   631,   633,
     635,   637,   647,   655,   664,   665,   675,   680,   685,   686,
     691,   697,   699,   701,   707,   709,   711,   713,   715,   717,
     719,   721,   723,   725,   727,   729,   731,   733,   735,   737,
     739,   749,   750,   754,   755,   760,   763,   767,   768,   772,
     773,   775,   777,   779,   781,   783,   788,   790,   792,   800,
     801,   809,   812,   813,   815,   820,   836,   838,   840,   842,
     844,   846,   848,   850,   852,   860,   861,   863,   867,   868,
     869,   870,   874,   875,   877,   879,   881,   883,   885,   887,
     889,   896,   897,   898,   899,   903,   904,   908,   909,   914,
     915,   917,   919,   924,   925,   927,   932,   933,   935,   940,
     941,   943,   945,   947,   952,   953,   955,   960,   961,   966,
     967,   972,   973,   978,   979,   984,   985,   990,   991,   994,
     999,  1004,  1005,  1013,  1019,  1020,  1024,  1025,  1029,  1030,
    1034,  1035,  1036,  1037,  1038,  1039,  1040,  1041,  1042,  1043,
    1044,  1054,  1056,  1061,  1062,  1064,  1066,  1071,  1072,  1078,
    1079,  1085,  1086,  1087,  1088,  1089,  1090,  1091,  1092,  1093,
    1094,  1095,  1097,  1098,  1104,  1106,  1116,  1118,  1126,  1127,
    1132,  1134,  1136,  1138,  1140,  1144,  1145,  1147,  1152,  1154,
    1161,  1163,  1165,  1175,  1177,  1179,  1184,  1189,  1192,  1197,
    1199,  1201,  1203,  1211,  1212,  1214,  1218,  1220,  1224,  1226,
    1227,  1229,  1231,  1236,  1237,  1241,  1246,  1247,  1251,  1253,
    1258,  1260,  1265,  1267,  1269,  1271,  1276,  1278,  1280,  1282,
    1287,  1289,  1294,  1295,  1317,  1319,  1321,  1324,  1326,  1329,
    1331,  1333,  1336,  1338,  1340,  1342,  1344,  1346,  1349,  1351,
    1354,  1356,  1358,  1364,  1366,  1368,  1373,  1375,  1377,  1384,
    1386,  1389,  1391,  1393,  1399,  1401,  1403,  1408,  1410,  1412,
    1419,  1424,  1432,  1434,  1436,  1441,  1443,  1448,  1449,  1451,
    1456,  1458,  1463,  1465,  1467,  1469,  1472,  1476,  1479,  1483,
    1485,  1487,  1489,  1491,  1493,  1495,  1497,  1499,  1501,  1503,
    1508,  1509,  1513,  1519,  1524,  1529,  1530,  1534,  1538,  1543,
    1544,  1550,  1554,  1556,  1558,  1560,  1563,  1565,  1570,  1572,
    1577,  1579,  1581,  1586,  1588,  1594,  1595,  1599,  1600,  1601,
    1602,  1606,  1611,  1612,  1614,  1616,  1618,  1622,  1626,  1627,
    1631,  1633,  1635,  1637,  1639,  1645,  1646,  1652,  1653,  1657,
    1658,  1663,  1665,  1671,  1672,  1674,  1679,  1684,  1695,  1696,
    1700,  1701,  1707,  1708,  1712,  1714,  1718,  1720,  1724,  1725,
    1729,  1730,  1734,  1741,  1742,  1746,  1748,  1763,  1764,  1765,
    1766,  1768,  1772,  1774,  1778,  1785,  1787,  1789,  1794,  1795,
    1797,  1799,  1801,  1833,  1836,  1841,  1843,  1849,  1854,  1859,
    1870,  1875,  1880,  1885,  1890,  1899,  1903,  1910,  1912,  1913,
    1914,  1920,  1922,  1927,  1928,  1929,  1938,  1939,  1940,  1944,
    1945,  1952,  1961,  1962,  1963,  1968,  1969,  1978,  1979,  1984,
    1985,  1989,  1991,  1993,  1995,  1997,  2001,  2006,  2007,  2009,
    2019,  2020,  2025,  2027,  2029,  2031,  2033,  2036,  2038,  2040,
    2045,  2047,  2049,  2051,  2053,  2055,  2057,  2059,  2061,  2063,
    2065,  2067,  2069,  2071,  2073,  2075,  2077,  2079,  2081,  2083,
    2085,  2087,  2089,  2091,  2093,  2095,  2097,  2099,  2104,  2105,
    2109,  2116,  2117,  2123,  2124,  2126,  2128,  2130,  2135,  2137,
    2142,  2143,  2145,  2147,  2152,  2154,  2156,  2158,  2160,  2162,
    2167,  2174,  2176,  2178,  2183,  2191,  2190,  2194,  2202,  2203,
    2205,  2207,  2212,  2213,  2215,  2220,  2221,  2223,  2225,  2230,
    2231,  2233,  2238,  2240,  2242,  2244,  2245,  2247,  2252,  2254,
    2256,  2261,  2268,  2272,  2273,  2278,  2277,  2282,  2281,  2300,
    2299,  2311,  2310,  2321,  2326,  2327,  2332,  2338,  2352,  2353,
    2357,  2359,  2361,  2367,  2369,  2371,  2373,  2375,  2377,  2379,
    2381,  2387,  2388,  2393,  2402,  2404,  2413,  2415,  2416,  2417,
    2419,  2421,  2422,  2427,  2428,  2429,  2434,  2436,  2439,  2446,
    2447,  2448,  2454,  2459,  2461,  2467,  2468,  2474,  2475,  2479,
    2484,  2487,  2486,  2490,  2493,  2495,  2503,  2502,  2511,  2517,
    2521,  2523,  2528,  2530,  2532,  2534,  2540,  2541,  2542,  2549,
    2550,  2552,  2553,  2554,  2556,  2558,  2565,  2566,  2568,  2570,
    2575,  2576,  2582,  2583,  2585,  2586,  2591,  2592,  2593,  2595,
    2603,  2604,  2606,  2609,  2611,  2615,  2616,  2617,  2619,  2621,
    2626,  2628,  2633,  2635,  2644,  2646,  2651,  2652,  2653,  2657,
    2658,  2659,  2664,  2665,  2670,  2671,  2672,  2673,  2677,  2678,
    2683,  2684,  2685,  2686,  2687,  2701,  2702,  2707,  2708,  2714,
    2716,  2719,  2721,  2723,  2746,  2747,  2753,  2754,  2760,  2759,
    2769,  2768,  2772,  2778,  2784,  2785,  2787,  2791,  2796,  2798,
    2800,  2802,  2808,  2809,  2813,  2814,  2819,  2821,  2828,  2830,
    2831,  2833,  2838,  2840,  2842,  2847,  2849,  2854,  2859,  2867,
    2869,  2874,  2875,  2880,  2881,  2885,  2886,  2887,  2892,  2894,
    2900,  2902,  2907,  2909,  2915,  2916,  2920,  2924,  2928,  2930,
    2931,  2932,  2937,  2940,  2939,  2951,  2950,  2962,  2961,  2973,
    2972,  2984,  2983,  2997,  3003,  3005,  3011,  3012,  3023,  3030,
    3035,  3041,  3044,  3047,  3051,  3057,  3060,  3063,  3068,  3069,
    3070,  3074,  3080,  3081,  3091,  3092,  3096,  3097,  3102,  3107,
    3108,  3114,  3115,  3117,  3122,  3123,  3124,  3125,  3126,  3128,
    3163,  3165,  3170,  3172,  3173,  3175,  3180,  3182,  3184,  3186,
    3191,  3193,  3195,  3197,  3199,  3201,  3203,  3208,  3210,  3212,
    3214,  3223,  3225,  3226,  3231,  3233,  3235,  3237,  3239,  3244,
    3246,  3248,  3250,  3255,  3257,  3259,  3261,  3263,  3265,  3277,
    3278,  3279,  3283,  3285,  3287,  3289,  3291,  3296,  3298,  3300,
    3302,  3307,  3309,  3311,  3313,  3315,  3317,  3332,  3337,  3342,
    3344,  3345,  3347,  3352,  3354,  3356,  3358,  3363,  3365,  3367,
    3369,  3371,  3373,  3375,  3380,  3382,  3384,  3386,  3388,  3398,
    3400,  3402,  3403,  3405,  3410,  3412,  3414,  3419,  3421,  3423,
    3425,  3430,  3432,  3434,  3448,  3450,  3452,  3453,  3455,  3460,
    3462,  3467,  3469,  3471,  3476,  3478,  3483,  3485,  3502,  3503,
    3505,  3510,  3512,  3514,  3516,  3518,  3523,  3524,  3526,  3528,
    3533,  3535,  3537,  3543,  3545,  3547,  3550,  3554,  3556,  3558,
    3560,  3594,  3595,  3597,  3599,  3604,  3606,  3608,  3610,  3612,
    3617,  3618,  3620,  3622,  3627,  3629,  3631,  3637,  3638,  3640,
    3649,  3652,  3654,  3657,  3659,  3661,  3675,  3676,  3678,  3683,
    3685,  3687,  3689,  3691,  3696,  3697,  3699,  3701,  3706,  3708,
    3716,  3717,  3718,  3723,  3724,  3729,  3731,  3733,  3735,  3737,
    3739,  3746,  3748,  3750,  3752,  3754,  3757,  3759,  3761,  3763,
    3765,  3770,  3772,  3774,  3779,  3805,  3806,  3808,  3812,  3813,
    3817,  3819,  3821,  3823,  3825,  3827,  3834,  3836,  3838,  3840,
    3842,  3844,  3849,  3851,  3853,  3860,  3862,  3880,  3882,  3887,
    3888
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

#define YYPACT_NINF (-1784)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-901)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
      91, 11977,   127,   351, 16264,    38, -1784, -1784, -1784, -1784,
   -1784, -1784, -1784, -1784, -1784, -1784, -1784,   232,   843,   248,
   -1784, -1784, -1784, -1784, -1784, -1784, -1784, -1784, -1784, -1784,
   -1784, -1784, -1784, -1784, -1784, -1784, -1784, -1784, -1784, -1784,
   -1784, -1784, -1784, -1784, -1784, -1784, -1784,   151,   425, -1784,
   -1784, -1784, -1784, -1784, -1784,  3530,  3530,   318, 11977,   321,
     330, -1784, -1784,   348, -1784, -1784, -1784, -1784, -1784, -1784,
   -1784, -1784, -1784,  3656, -1784,   759,   304, -1784, -1784, -1784,
   -1784, -1784,  6278, -1784, -1784,   349,   371,   537,    81, -1784,
    3530,   371,   371,   371,   370,  4411,   554,   870, 12136, -1784,
   -1784, -1784, 16114,   846, -1784, -1784, -1784,  2060,   581, 12188,
     836,   970,  2060,  1155,   435, -1784, -1784, -1784, -1784,   520,
   -1784, -1784, -1784, -1784,   441, -1784, -1784, -1784, -1784, -1784,
     458,   478,   520, -1784,   520,   496, -1784, -1784, -1784, 16820,
    3530, -1784, -1784,  3530, -1784, 11977,   456, 16872, -1784, -1784,
    4851, 17884, -1784,  1049,  1049,   531,  1583, -1784, -1784, -1784,
   -1784,   429, 14250,  2302,   520, -1784, -1784, -1784, -1784, -1784,
   -1784,   525, -1784,   540,   542,   565, -1784,   641, 20404, 15344,
    2850,  3656,   848,   590,   620,   672,   682,   697,   738, -1784,
   -1784, 17022, 10860,   634, -1784, 16407, -1784, -1784, -1784, -1784,
     663, -1784, -1784,   765, -1784,  5833,   815, 18892, -1784,   724,
    3530,   478,   777,   778,   797,   805, -1784, -1784, -1784,  3274,
    3261,   811,   818,    10, -1784, -1784,   520,   520,    36,   267,
     107,    36, -1784,   520,   520, -1784,  4679, -1784, -1784,   795,
     823,  1049, 14144, -1784, -1784,  6278, -1784, -1784,  2060, -1784,
    2285,   435,   832,   919,   267,  3530,   537, -1784, 13542, -1784,
    1049,  1049,   852,   919,   267,  3530, -1784, 20990, -1784, -1784,
    1049, -1784,  1049, -1784,   931,  2912,  3530, -1784,  1732,   881,
   -1784, -1784, -1784, 16566,   478,   276, -1784, -1784, 17934, -1784,
     818,    96, -1784, 20404, 17884,  3499,  4679, -1784,   325, -1784,
   -1784, -1784, 16872,  3530, -1784,   855, -1784, -1784, -1784, -1784,
    3530,  2338,   390,   258, -1784,  3530,   540, -1784,   915,   520,
     520,   896, 17074,   942, 14724, 14302,  2060,  2060, -1784,  2060,
    1049,  2060,  1049, -1784, -1784,   520, -1784,   906, -1784, 17224,
   -1784, -1784, -1784, 17276,   663, -1784,   903,   519,  1711,   918,
     435,   923, -1784,  1583,   899,   540,  1583,  2084, -1784,   932,
     984, 20476,   978,   981, 20404, 20548,  1015, 15090, -1784, -1784,
   -1784, -1784, -1784, -1784, 20620, 20620, 15190,  1018,  3003, -1784,
   -1784, -1784, -1784,   153, -1784,   656, -1784,  1166, -1784, 20404,
   20404, -1784,   973,   661,   944,   990,   626,  1120,  1025,  1039,
    1034,  1083,    94, -1784,   574, -1784,  1077, -1784,  1101,  3025,
   15652, -1784, -1784,   609,  1077, -1784, -1784,   599, -1784, -1784,
    2850,  1064,  1081,  1087,  1089,  1111,  1134, -1784, -1784,   396,
    1100, -1784,   739,  1100, -1784, -1784, 16820, -1784,  1105,  1135,
   15806, -1784, -1784,  3123,  4304,  1143, 14724,  1162,   636,   685,
   -1784, -1784, -1784, -1784, -1784,  3530,  3610, -1784, -1784, -1784,
   -1784, -1784, -1784, 13434,  3421,  1018,  5833,  1158,  1160, -1784,
   -1784,  1152, 18892,   727, -1784, -1784, -1784, 18964,  1177, -1784,
   -1784, -1784, -1784, -1784,  3274,   757,  1185,  1189,  1192,   864,
    1194,  1214,  1218,  3261, -1784, -1784,   520,  1211,   537,  1193,
   -1784, -1784,  1230, -1784, -1784,   478,   919, -1784, -1784, -1784,
     478, -1784, -1784,  4679, -1784, 15652, 15652, -1784,  1049,  4851,
   18662, 14882, -1784, -1784, -1784, -1784, -1784,   478,   919,    96,
   -1784, -1784,  2060,  1228,   919,   267, -1784,   478,   919, -1784,
   21040, -1784,  1049,  1049, -1784, -1784,  1253,   669,  1267,   435,
    1274, -1784, 18092, -1784,   790, -1784,  1328, 18558, -1784,  4851,
   17435, 14144, -1784, 16566, 20692, -1784, -1784, -1784, -1784, -1784,
    3499,   871,  4679, -1784, 14882,   818, 11977, -1784,  1248, -1784,
    1259, -1784, -1784, -1784, -1784, -1784,  1583, -1784, -1784,  1341,
    3511,  1278, 17276, 10860, -1784, 17487, -1784,  1049,  1049, -1784,
   -1784,   663, -1784,   789,  1287,  1427, 20404,   988,  1230,  1276,
   -1784,   520,   520, -1784,  1100, -1784, 17074, -1784, -1784, 18373,
    1049,  1049, -1784,  3511,   520, -1784, 17741, -1784, -1784, 17224,
   -1784,   429,  1305,  1288,  1309,  1711,   802, 16872,   837, -1784,
   -1784, -1784, -1784, -1784, -1784,   840, -1784,  1313,  1293, -1784,
   15498, -1784, 17539, 17539, -1784, 15498, -1784, 20404, -1784, 12188,
   12188, 15498, -1784, -1784, 16618, 17539, 17539,  1101,  1642,  1665,
     479,  1690, -1784,   890,  1318,  1104,  1323, -1784, 18964, 20404,
   19036,  1319, 20404,  1732, 20404,  1732, -1784,  2133, -1784, -1784,
   19108,   868, 20404, 19108,  1732, -1784, -1784, 20404, 20404, 20404,
   20404, 20404, 20404, 20404, 20404, 20404, 20404, 20404, 20404, 20404,
   20404, 20404, 20404, 20404, 20404, 20404, 19180,  1301,   641,  3448,
   10860, -1784, -1784, -1784, -1784, -1784, -1784, -1784, -1784, -1784,
   -1784, -1784,  1324, 20404, -1784, -1784,   609,  1482, -1784, -1784,
     520,   520, -1784, -1784, 15652, -1784,   419,  1100, -1784,   845,
    1100, -1784, -1784, -1784,  1230, -1784, -1784,  1230, 20764, -1784,
   -1784, 10860,  1326,  1329,  3917,  1465,  3433,   421,  1276, -1784,
     520,   520,  1276,   459, -1784,   520,   520, 20404,  3530,  1123,
    1126,  1276,   255, 14092, 14092,  3530, -1784, -1784, 20404,  1152,
   -1784,  5833,  1337, -1784,  2166, -1784, -1784, -1784, -1784, -1784,
     892, -1784, 14092,  1732,  4851,  1732,   902,  1335,  1336,  1338,
     928,  1339,  1340,  1352,   483,  1100, -1784, -1784,   516,  1100,
   -1784, -1784, -1784,  4851,   641, -1784,  1100, 20764, -1784,   478,
   18092, -1784, -1784,   935,  1353,   937,  1354, -1784,  1358, -1784,
     478, -1784, -1784,   478,   919,  1358, -1784,   478,  1360,  1362,
    1363, -1784, -1784, 18373, -1784,  1370, -1784, -1784, -1784,  1732,
    3530, 10019,  1441,  1348, 18460, -1784,  1135, -1784, 14092,   967,
   -1784, -1784,  1358, -1784, 16872, 15652,  1350, -1784,  1350, -1784,
   -1784, -1784,  1711, -1784, 17224, -1784, 11022, 15960, -1784, 18092,
    1381,  1383,  1387, -1784, 11738,   520, -1784,   988, -1784, -1784,
   -1784, -1784,  1230, -1784, -1784, -1784,  1049, -1784,  2487, -1784,
   -1784,   435,  1747,  1395, 19252, -1784,  1711,  1305, -1784, -1784,
    1388,  1393,  2084, 19108, -1784,  1397,   304,  1394,  1399,  1400,
    1398,  1403, 20404,  1404,  1405,  1406, 10860, 20404, -1784, -1784,
    2185, -1784, -1784, -1784, 20404, -1784,  1408,  1410, 18748,  1137,
   -1784, 19108,  1409, -1784,  1411, -1784, -1784,  3575, -1784, -1784,
     987, -1784, -1784, -1784, -1784,  3575, -1784, -1784,  1142,   115,
   -1784, -1784,   973,   973,   973,   661,   661,   944,   944,   990,
     990,   990,   990,   626,   626,  1120,  1025,  1039,  1034,  1083,
   20404,  1148, -1784,  1415,  3575, -1784, -1784,  5833, -1784, 18092,
    1416,  1421,  1423,  1482, -1784, -1784, -1784, -1784, -1784, -1784,
   -1784, -1784,  1230, -1784, -1784,  1230, 18092, 18092, -1784, -1784,
    3917,   879,  1424,  1425,  1426,  1430,  2473,  3433, -1784, -1784,
   -1784, -1784, -1784, -1784, -1784, -1784, -1784, -1784, -1784, -1784,
   -1784, -1784,  1428, -1784,  1276, -1784, -1784, -1784, -1784, -1784,
   -1784, -1784, -1784,  1432,  1436, -1784,   537,  3575,  1151,   270,
   -1784, -1784,  1414, -1784, 18892, -1784, 20404,   520, 19324, 14092,
   -1784, -1784, -1784,  1417,   518,  1100, -1784,   528,  1100, -1784,
   -1784, -1784, -1784,  1230, -1784, -1784, -1784,  1230,   818,  1437,
    1230, -1784, -1784, -1784, -1784, -1784, -1784, -1784,  1443, -1784,
   -1784,  1358, -1784,   478, -1784, -1784, -1784, -1784, -1784, 12766,
    1442,  1439, -1784,   229, -1784,   517,   192, 10698,  1429, 13921,
    1447,  1449,  2613,  2723,  2105, 19396,  1450, -1784, -1784,  1452,
    1453, -1784, -1784,   478, 20404, 20404,  1591,  1451,   379, -1784,
    1533,  1455,  1448, -1784, -1784, -1784,  9847, -1784, -1784, -1784,
   -1784, -1784,  2416, -1784, -1784, -1784,  1517, -1784, -1784, -1784,
    1732, -1784, -1784, 12613,  6278,  1457, -1784,  3530, -1784,  1454,
    1463,  1470, -1784,  1179, -1784, -1784, -1784, -1784,  4851, -1784,
   -1784,  1459,  1460,   989, 16872,   540,   540,  1305, -1784, -1784,
    1018,  1135, 15806, -1784,  1077, -1784, 11184, -1784,   577,  1100,
   -1784,  1049, 11814, -1784, -1784,  1711,   520,   520,   429,  1288,
   -1784,  5833, -1784,  1305,  1477,  1481, -1784, -1784,   995,   463,
   10860,  1732, -1784,   463, 16670,   463, -1784, 20404, 20404, 20404,
   -1784, -1784, -1784, -1784, 20404, 20404,  1480,  5833, -1784, -1784,
    1484,   713, -1784, -1784, -1784,  3024, -1784, -1784,  1196, -1784,
      85, -1784, 19108,  1198, -1784, 18964, -1784, -1784, 20404,  1466,
    1201,  1203,  1152, -1784,   584,  1100, -1784, -1784, 18092, 18092,
   -1784, -1784,  1490,   586,  1100, -1784,   587,  1996,   520,   520,
   -1784, -1784, 18092, 18092, -1784,  1491, -1784, 14882, 14882,  1499,
    1496,  1497,  1511, -1784,  1508, 20404, 20404,  1209,  1512, -1784,
   -1784, -1784, -1784, -1784, -1784, -1784,  1514, 20404, -1784, -1784,
   -1784,  1230, -1784, -1784, -1784,  1230, 18092, 18092,   537,   520,
    1220,  1518,  1523, -1784, -1784,  1525, 12919, 13072, 13225, 16872,
   17539, 17539,  1547, -1784,  1524,  1535,  2227,  6092, -1784,   316,
    3530, -1784, -1784,  3530, -1784, 18820,   -37,   358, -1784, -1784,
   -1784, -1784, 20404,  1561,  1607, 10535, 10191, -1784,  1537, -1784,
    1538, 20404,  1539,  5833,  1541, 20404, 18964, 20404,  1024, -1784,
    1542,   146, -1784,    22,  1568, -1784, -1784,  1570, -1784,  1545,
   -1784,  1548,  1572, 13921,   939, 13700,   520,   345, -1784, -1784,
   -1784,  1574, -1784,  1575, -1784,  1578, -1784,  1573, -1784,  1576,
   -1784, -1784, -1784, -1784,  1579, 11346,  1581,  1585,  1586, -1784,
    1582, -1784, -1784, -1784,  1230, 20404, 20404,  1135,  1589, -1784,
    1305, -1784,  1580,   139, -1784,  1152,  1584, -1784, -1784, 16872,
   -1784,  1593,  1595,  1005, -1784,  1596, -1784, -1784, -1784, -1784,
   -1784,  5833,  1152, 18964, -1784,  1627,  3575, -1784,  1627,  1627,
   -1784,  3575,  3196,  3356, -1784, -1784,  1234, -1784, -1784, -1784,
    1612,  1604, -1784, -1784, -1784,  1230, -1784, -1784,  1613,  1615,
     520, -1784, -1784, -1784,  1230, -1784, -1784, -1784,  1621, -1784,
   -1784, -1784, -1784, -1784, -1784, -1784, -1784, -1784, -1784, -1784,
   -1784, -1784,  1619, -1784, -1784, -1784, -1784,  1620,  1625,   520,
   -1784, 18092, 18092, -1784, -1784, -1784, -1784, 20404, -1784, -1784,
    1629, -1784,  1547,  1547,  1547,   810,  1606,   470, -1784,  4385,
     488, 15652, -1784, -1784, -1784,  3759, 20404,  4838,   557, -1784,
   -1784,    30,  1622,  1622,  3530, -1784, -1784, 18241, -1784, 20404,
    1630,  1633, -1784, -1784, -1784, -1784,  1028,  1639, 13921,  1455,
    1640, 20404,   349,  1635,   370, 13384, 16872, -1784, -1784, -1784,
     515, 13921, 20404,   722,   221, -1784, 20404,  8402, -1784, -1784,
     598, -1784,  1152, -1784,  1030,  1037,  1038, -1784, -1784, -1784,
   -1784,   478,  1024,  1643, -1784, -1784, 20404, -1784,  1646,   641,
   10698, -1784, -1784, -1784, -1784, 20404,  1689, -1784,  9675, -1784,
     520, 14882, -1784, -1784, 16872, -1784, -1784, -1784, -1784, -1784,
   -1784,  1647, -1784, 18092, -1784, -1784,  1648, -1784,  1652,  1660,
    1654,  1711, -1784,  1677, -1784, -1784, -1784, 20404, -1784, 16670,
   20404,  1152,  1678,  1236, -1784,  1239, -1784,  3575, -1784,  3575,
   -1784, -1784, -1784, -1784, 18092,  1662,  1676, -1784, -1784, 18092,
   18092,  1680,  1683,  1245, 14408, 14566, -1784,  1674, -1784, -1784,
   -1784, -1784,  1684,  1685,  1250, -1784, -1784, -1784, -1784,   810,
    1515,   604, -1784, -1784, -1784, -1784,   520,   520, -1784, -1784,
   -1784,   625, -1784,  1044,  3759,  1023, -1784,  4838,   520, -1784,
   -1784, -1784, -1784, -1784, -1784, -1784, -1784,   633, 13921,   406,
   19468,  1756, 13921,  1455, 15040, -1784, -1784, -1784, -1784, -1784,
   20404,  1772,  1675, 18672, 19540, 13921, 10363,  1455,   515,  1063,
    1687, 20404, -1784,  1702,   423, 13921, -1784, -1784,  1713, -1784,
   -1784,  1691,   641,   681,  1712,  1716,  1251,  1776, -1784, -1784,
   -1784, -1784,  3530,  4851, -1784, -1784,  1715,  1718, -1784, -1784,
   -1784,  1711,  1305, -1784,  1726, -1784, -1784, -1784,  1728, -1784,
   -1784, -1784,  1260,  1266, -1784, -1784, -1784, -1784, -1784, -1784,
   -1784, -1784, -1784, -1784,  1727, -1784, -1784,  1729,  1730, -1784,
   -1784, -1784,  1733,  1736,  1741,  1515, -1784,   520, -1784, -1784,
   -1784, -1784, -1784,  1740,  4385, -1784, -1784,  5757,   219, 11511,
   -1784, 13803, -1784,     1,  1045, 13921,  1806,   472, 13921, 20404,
    1731,   515,  1063,  1720, 20836,  1734,   562,  1823, -1784, 20404,
   19612, 20404,  1455,  1735, 11672, -1784, -1784, -1784, 17689, -1784,
    1746,  1738,    95, 13921, -1784, 20404, 19108,   449, -1784, -1784,
   -1784,  1752, -1784, -1784,  1305,  1757, -1784, -1784, -1784, -1784,
    1753,  1759,  1764, 14882,  1761, -1784, -1784,   632,  1100, -1784,
   -1784,   810, -1784, -1784,   375, -1784,    78, -1784, -1784, -1784,
    1760, 12295, -1784, -1784, 13921, -1784,     4, -1784, 13921, 19684,
   -1784, -1784, 20404, 19756, 20404,  1731,  1455, 19828, 19900, 13921,
     646,  1742,   650, -1784, -1784,  1769, 12295, 17689, -1784,  4775,
   17487,  1732,  1762, -1784,  1819,  1773,   718,  1766, -1784,  1850,
   -1784,  1062, 13921,  1775, 13921, 13921, -1784,  1778, -1784, -1784,
   -1784, -1784, -1784, -1784, -1784, -1784,  1230, -1784, 20404, -1784,
   20404, -1784, -1784,  1347, 12454, -1784, -1784, 13921, -1784, -1784,
   -1784,  1455,   655,  1763,   662, -1784, -1784,  1455, -1784,  1455,
   -1784, 19972, 20044, 20116, -1784,  1347, -1784,  1758,  2550,  4291,
   -1784, -1784, -1784,    95,  1781, 20404,  1768,    95,    95, 13921,
   -1784, -1784, 20404,  1820,  1830, -1784, 18092, -1784, -1784, 13803,
   -1784,  1347, -1784, -1784, 20188, 20260, 20332, -1784,  1455, -1784,
    1455, -1784,  1455, -1784,  1758, 20404,  1779,  4291,  1782,   641,
    1792, -1784,   719, -1784, -1784,  1088,  1776,   125, -1784, -1784,
    9423,  1796, 13803, -1784,  1455, -1784,  1455, -1784,  1455,  1797,
    1795, -1784,   478,   641,  1799, -1784,  1786,   641, -1784, -1784,
   13921,  1876,  1801, -1784, -1784, -1784,  9542, -1784,   478, -1784,
   -1784,  1272, 20404, -1784,  1090, -1784, 13921, -1784, -1784,   641,
    1732,  1802,  1787, -1784, -1784, -1784,  1095, -1784, -1784,  1788,
    1732, -1784, -1784
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,   445,     0,     2,   445,   462,   463,   464,   465,   466,
     467,   468,   469,   451,   453,   452,   454,     0,     0,     0,
     470,   472,   493,   473,   494,   476,   477,   491,   492,   471,
     489,   490,   474,   475,   478,   479,   480,   481,   482,   483,
     484,   485,   486,   487,   488,   495,   496,   784,   498,   571,
     572,   575,   577,   573,   579,     0,     0,     0,   445,     0,
       0,    16,   542,   548,     9,    10,    11,    12,    13,    14,
      15,   748,    97,     0,    19,     0,     2,    95,    96,    17,
      18,   800,   445,   749,   394,     0,   397,   674,   399,   408,
       0,   398,   428,   429,     0,     0,     0,     0,   525,   447,
     449,   455,   445,   457,   460,   510,   497,   433,   503,   508,
     434,   520,   435,   535,   539,   545,   524,   551,   563,   784,
     568,   569,   552,   619,   400,   401,     3,   750,   763,   450,
       0,     0,   784,   822,   784,     2,   839,   840,   841,   445,
       0,   998,   999,     0,     1,   445,     0,   445,   417,   418,
       0,   525,   439,   440,   441,   753,     0,   574,   576,   578,
     580,     0,   445,     0,   785,   786,   570,   499,   667,   668,
     666,   727,   722,   712,     0,     0,   751,     0,     0,   445,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   543,
     546,   445,   445,     0,  1000,   525,   829,   847,  1004,   997,
     995,  1002,   393,     0,   159,   680,   158,     0,   402,     0,
       0,     0,     0,     0,     0,     0,   392,   899,   900,     0,
       0,   427,   782,   784,   778,   803,   784,   784,   780,     2,
     784,   779,   860,   784,   784,   857,     0,   518,   519,     0,
       0,   445,   445,   462,     2,   445,   409,   448,   458,   511,
       0,   540,     0,   766,     2,     0,   674,   410,   525,   504,
     521,   536,     0,   766,     2,     0,   461,   505,   512,   513,
     522,   527,   537,   541,     0,   555,     0,   742,     2,     2,
     764,   821,   823,   445,     0,     2,     2,  1008,   525,  1011,
     782,   782,     3,     0,   525,     0,     0,   420,   784,   780,
     779,     2,   445,     0,   746,     0,   708,   710,   709,   711,
       0,     0,   704,     0,   694,     0,   703,   714,     0,   784,
     784,     2,   445,  1019,   446,   445,   457,   436,   503,   437,
     528,   438,   535,   532,   553,   784,   554,     0,   655,   445,
     656,   973,   974,   445,   657,   659,   542,   548,     0,   620,
     621,     0,   787,     0,   725,   713,     0,   791,    21,     0,
      20,     0,     0,     0,     0,     0,     0,    23,    25,     4,
       8,     5,     6,     7,     0,     0,   445,     2,     0,    98,
      99,   100,   101,    82,    24,    83,    38,    81,   102,     0,
       0,   117,   119,   123,   126,   129,   134,   137,   139,   141,
     143,   145,   147,   150,     0,    26,     0,   549,     2,   102,
     445,   151,   719,   670,   539,   672,   718,     0,   669,   673,
       0,     0,     0,     0,     0,     0,     0,   801,   827,   784,
     837,   845,   849,   855,     2,  1006,   445,  1009,     2,    95,
     445,     3,   654,     0,  1019,     0,   446,   503,   528,   535,
       3,     3,   636,   640,   650,   656,   657,     2,   830,   848,
     996,     2,     2,    23,     0,     2,   680,    24,     0,   678,
     681,  1017,     0,     0,   687,   676,   675,     0,     0,   768,
       2,     2,     2,     2,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   806,   863,   784,     0,   674,     2,
     802,   810,   926,   804,   805,     0,   766,     2,   859,   867,
       0,   861,   862,     0,   423,   445,   445,   509,   446,     0,
     525,   445,  1001,  1005,  1003,   526,   746,     0,   766,   782,
     403,   411,   459,     0,   766,     2,   746,     0,   766,   723,
     506,   507,   523,   538,   544,   547,   542,   548,   566,   567,
       0,   724,   445,   664,     0,   196,   386,   445,     3,     0,
     525,   445,   765,   445,     0,   405,     2,   406,   743,   425,
       0,     0,     0,     2,   445,   782,   445,   746,     0,     2,
       0,   707,   706,   705,   700,   456,     0,   698,   715,   501,
       0,     0,   445,   445,   975,   446,   442,   443,   444,   979,
     970,   971,   977,     2,     2,    96,     0,   935,   949,  1019,
     931,   784,   784,   940,   947,   662,   445,   533,   658,   446,
     529,   530,   534,     0,   784,   985,   446,   990,   982,   445,
     987,     0,  1017,   626,     0,     0,     0,   445,     0,   799,
     798,   794,   796,   797,   795,     0,   789,   792,     0,    22,
     445,    89,   445,   445,    84,   445,    91,     0,    32,     0,
      33,   445,    87,    88,   445,   445,   445,     2,    98,    99,
       0,     0,   177,     0,     0,   569,     0,   995,     0,     0,
       0,     0,     0,     0,     0,     0,    50,     0,    56,    57,
      61,     0,     0,    61,     0,    85,    86,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     445,   160,   161,   162,   163,   164,   165,   166,   167,   168,
     169,   170,   158,     0,   156,   157,     2,   911,   671,   908,
     784,   784,   916,   550,   445,   828,   784,   838,   846,   850,
     856,     2,   831,   833,   835,     2,   851,   853,     0,  1007,
    1010,   445,     0,     0,     2,    96,   935,   784,  1019,   881,
     784,   784,  1019,   784,   896,   784,   784,     3,   658,     0,
       0,  1019,  1019,   445,   445,     0,     2,   689,     0,  1017,
     686,  1018,     0,   682,     0,     2,   685,   688,   174,   173,
       0,     2,   445,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   784,   815,   819,   858,   784,   872,
     877,   807,   864,     0,     0,   431,   923,     0,   769,     0,
     445,   770,   424,     0,     0,     0,     0,   422,     2,   771,
       0,   407,   746,     0,   766,     2,   772,     0,     0,     0,
       0,   581,   643,   446,     3,     3,   647,   646,   842,     0,
       0,   445,   387,     0,   525,     3,    95,     3,   445,     0,
       3,   747,     2,   702,   445,   445,   696,   695,   696,   502,
     500,   620,     0,   981,   445,   986,   446,   445,   972,   445,
       0,     0,     0,   950,     0,   784,  1020,   936,   937,   663,
     933,   934,   948,   976,   980,   978,   531,   566,     0,   984,
     989,   623,  1018,     0,     0,   622,     0,  1017,   728,   726,
       0,     0,   791,    61,   752,     0,     2,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   445,     0,   116,   115,
       0,   112,   111,    27,     0,    28,     0,     0,     0,     0,
       3,    61,     0,    46,     0,    47,    54,     0,    53,    65,
       0,    62,    63,    66,    49,     0,    48,    52,     0,     0,
      45,   118,   120,   121,   122,   124,   125,   127,   128,   132,
     133,   130,   131,   135,   136,   138,   140,   142,   144,   146,
       0,     0,   396,     0,     0,    29,     3,   680,   152,   445,
       0,     0,     0,   912,   913,   909,   910,   721,   720,     2,
     832,   834,   836,     2,   852,   854,   445,   445,   928,   927,
       2,     0,     0,     0,     0,     0,   784,   936,   884,   901,
       2,   879,   887,   660,   882,   883,   661,     2,   894,   904,
     897,   898,     0,     3,  1019,   415,     2,  1012,     2,   651,
     652,   630,     3,     3,     3,     3,   674,     0,   150,     0,
       3,     3,     0,   683,     0,   677,     0,   784,     0,   445,
       3,   419,   421,     0,   784,   816,   820,   784,   873,   878,
       2,   808,   811,   813,     2,   865,   868,   870,   782,     0,
     924,     3,   774,     3,   515,   514,   517,   516,     2,   747,
     775,     2,   773,     0,   747,   776,   581,   581,   581,   445,
       0,     0,   665,     0,   390,     0,     0,   445,     0,     2,
       0,     0,     0,     0,     0,   179,     0,   320,   321,     0,
       0,   359,   358,     0,   154,   154,   365,   542,   548,   193,
       0,   180,     0,   204,   181,   182,   445,   198,   183,   184,
     185,   186,     0,   187,   188,   326,     0,   189,   190,   191,
       0,   192,   200,   525,   445,     0,   202,     0,   384,     0,
       0,     0,     3,     0,   754,   747,   735,   736,     0,     3,
     731,     3,     3,     0,   445,   712,   712,  1017,   983,   988,
       2,    95,   445,     3,   540,     3,   446,     3,   784,   943,
     946,   445,     3,   932,   938,     0,   784,   784,     0,   626,
     610,   680,   627,  1017,     0,     2,   788,   790,     0,    90,
     445,     0,    94,    92,   445,     0,   106,     0,     0,     0,
     110,   114,   113,   178,     0,     0,     0,   680,   103,   171,
       0,     0,    41,    42,    79,     0,    79,    79,     0,    67,
      69,    44,     0,     0,    40,     0,    43,   149,     0,     0,
       0,     0,  1017,     3,   784,   919,   922,   914,   445,   445,
       3,     3,     0,   784,   890,   893,   784,     0,   784,   784,
     885,   902,   445,   445,  1013,     0,   653,   445,   445,     0,
       0,     0,     0,   404,     3,     0,     0,     0,     0,   679,
     684,     3,   767,   176,   175,     3,     0,     0,     2,   809,
     812,   814,     2,   866,   869,   871,   445,   445,   674,   784,
       0,     0,     0,   747,   777,     0,   445,   445,   445,   445,
     445,   445,   564,   592,     3,     3,   593,   525,   582,     0,
       0,   824,     2,     0,   388,    61,     0,     0,   311,   312,
     201,   203,     0,     0,     0,   445,   445,   307,     0,   305,
       0,     0,     0,   680,     0,     0,     0,     0,     0,   155,
       0,     0,   366,     0,     0,     3,   208,     0,   199,     0,
     302,     0,     0,     2,     0,   525,   784,     0,   385,   930,
     929,     0,     2,     0,   738,     2,   733,     0,   734,     0,
     716,   697,   701,   699,     0,   445,     0,     0,     0,     3,
       0,     2,   939,   941,   942,     0,     0,    95,     0,     3,
    1017,   616,     0,   626,   624,  1017,     0,   613,   729,   445,
     793,     0,     0,     0,    34,     0,   107,   109,   108,   105,
     104,   680,  1017,     0,    60,    76,     0,    70,    77,    78,
      55,     0,     0,     0,    64,    51,     0,   148,   395,    30,
       0,     0,     2,   915,   917,   918,     3,     3,     0,     0,
     784,     2,   886,   888,   889,     2,   903,   905,     0,   880,
     895,     3,     3,  1014,     3,   638,   637,   641,  1016,     2,
       2,  1015,     0,     3,   781,   690,   691,     0,     0,   784,
     426,   445,   445,     3,     3,   432,   783,     0,   874,   758,
       0,   760,   564,   564,   564,   599,   569,     0,   605,   593,
       0,   445,   556,   591,   587,     0,     0,     0,     0,   594,
     596,   784,   607,   607,     0,   588,   603,   445,   391,     0,
       0,    62,   315,   316,   313,   314,     0,     0,     2,   219,
       0,     0,   221,   399,   220,   525,   445,   293,   292,   294,
       0,     2,   179,   257,     0,   252,     0,   179,   308,   306,
       0,   300,  1017,   309,     0,     0,     0,   347,   348,   349,
     350,     0,   340,     0,   341,   317,     0,   318,     0,     0,
     445,   210,   197,   304,   303,     0,   338,   357,     0,   389,
     784,   445,   756,   717,   445,     2,     2,   614,   991,   992,
     993,     0,   944,   445,     3,     3,     0,   952,     0,     0,
       0,     0,   625,     0,   612,     3,    93,     0,    31,   445,
       0,  1017,     0,     0,    80,     0,    68,     0,    74,     0,
      72,    39,   153,   920,   445,     0,     0,   825,   843,   445,
     445,     0,     0,     0,   445,   445,   693,     0,   412,   414,
       3,     3,     0,     0,     0,   762,   560,   562,   558,     0,
     959,     0,   600,   964,   602,   956,   784,   784,   586,   606,
     590,     0,   589,     0,     0,     0,   609,     0,   784,   583,
     597,   608,   598,   604,   645,   649,   648,     0,     2,     0,
       0,   240,     2,   222,   525,   298,   296,   299,   295,   297,
       0,   248,     0,   179,     0,     2,   445,   258,     0,   279,
       0,     0,   301,     0,     0,     2,   324,   351,     0,   342,
       2,     0,     0,     0,     0,   329,     0,   325,   195,   194,
     413,   732,     0,     0,   994,     3,     0,     0,   951,   953,
     615,     0,  1017,   628,     2,    37,    35,    36,     0,    58,
     172,    71,     0,     0,     3,   826,   844,     3,     3,   891,
     906,   416,     2,   635,     3,   634,   692,     0,     0,   817,
     875,   925,     0,     0,     0,   960,   961,   784,   585,   957,
     958,   584,   565,     0,     0,   209,   323,     0,     0,     0,
     233,     2,   211,     0,     0,     2,   242,   260,     2,   179,
     290,     0,   268,     0,     0,   261,   259,   250,   253,     0,
       0,   179,   280,     0,     0,   214,   322,     2,   445,   319,
       0,     0,   367,     2,   327,     0,    61,     0,   339,   737,
     739,     0,   954,   955,  1017,     0,   730,    59,    75,    73,
       0,     0,     0,   445,     0,   818,   876,   784,   967,   969,
     962,     0,   595,   228,   223,   226,     0,   225,   232,   231,
       0,   445,   235,   234,     2,   244,     0,   241,     2,     0,
     249,   254,     0,     0,   179,   291,   269,     0,     0,     2,
     282,   283,   281,   256,   310,     0,   445,   445,     3,   352,
     446,   356,     0,   360,     0,     0,     0,   368,   369,   217,
     330,     0,     2,     0,     2,     2,   945,     0,   618,   921,
     892,   907,   639,     2,   963,   965,   966,   601,     0,   230,
       0,   229,   213,   236,   445,   380,   245,     2,   246,   243,
     266,   263,   271,   272,   270,   255,   267,   264,   265,   262,
     251,     0,     0,     0,   216,   236,     3,   345,     0,   959,
     353,   354,   355,   367,     0,     0,     0,   367,     0,     2,
     328,   335,     0,   332,   334,   617,   445,   224,   227,     2,
       3,   237,   381,   247,     0,     0,     0,   288,   285,   289,
     286,   287,   284,     3,   345,     0,     0,   960,     0,     0,
       0,   361,     0,   370,   218,     0,   325,     0,     3,   205,
       0,     0,     2,   277,   274,   278,   275,   276,   273,     0,
       0,   346,     0,   373,     0,   371,     0,   373,   331,   333,
       2,     0,     0,   207,   206,   212,     0,   215,     0,   343,
     374,     0,     0,   362,     0,   336,     2,   968,   344,     0,
       0,     0,     0,   337,   375,   376,     0,   372,   363,     0,
       0,   364,   377
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1784,  5964,  5720, -1784,    -1,   249,  1884,    14, -1784,  1577,
   -1784,   327, -1784,  -658,   622,   698,  -916,  -967, -1784,   183,
    1858,  1881, -1784,   152, -1784,  1295,   455,   732,   735,   605,
     733,  1258,  1261,  1257,  1270,  1256, -1784,  -171,  -167,  7934,
     850, -1784,  1563, -1784, -1784,  -651,  7500, -1087,  2286, -1784,
     118, -1784,   829,   -24, -1784, -1784, -1784,   399,    60, -1784,
   -1783, -1438,   268,    39, -1784, -1784, -1784,   277, -1483, -1784,
   -1103, -1784, -1784, -1784, -1784,    -9, -1687,   158, -1784, -1784,
      -5, -1784, -1784, -1784,     5,   420,   424,   106, -1784, -1784,
   -1784, -1784,  -709, -1784,    37,   -20, -1784,   112, -1784,   -97,
   -1784, -1784, -1784,   847,  -677,  -928, -1294, -1784,     8, -1168,
      63,  7563,  -817,  -761, -1784,  -262, -1784,   286,  -142,   185,
    -270,  -239,  3488,  6772,  -603, -1784,    70,   215,   904,  2317,
   -1784,  1962, -1784,   181,  3749,  -232, -1784, -1784,   213, -1784,
   -1784,  2079,   224,  4107,  2630,   -12,  1771,  -292, -1784, -1784,
   -1784, -1784, -1784,  -290,  4928,  5038, -1784,  -363,   193, -1784,
     493,   225, -1784,   157,   691, -1784,   490,   -86, -1784, -1784,
   -1784,  5412,  -509, -1133,  -396,  -486,   -38,  1536, -1784, -1229,
    -145,  -183,  1714,   865,  6345,  -216,  -468,  -248,  -173,  -451,
    1242, -1784,  1554,   -95,  1156,  1444, -1784, -1784, -1784, -1784,
     272,  -148,  -152,  -849, -1784,   317, -1784, -1784,   610,   434,
   -1784, -1784, -1784,  2042,  -655,  -419,  -818,   -29, -1784, -1784,
   -1784, -1784, -1784, -1784,  -120,  -756,  -134, -1735,  -198,  7634,
     -72,  3849, -1784,  1122, -1784,  1102,  -210,  -205,  -200,  -196,
      27,   -69,   -68,   -67,   853,   -43,    -8,    -7,  -177,   -64,
    -176,  -159,  -149,  -663,  -649,  -640,  -633,  -660,  -126,  -624,
   -1784, -1784,  -662,  1310,  1311,  1312,   465,  7090,  -566,  -567,
    -563,  -558,  -685, -1784, -1619, -1625, -1616, -1613,  -583,  -123,
    -209, -1784, -1784,   -60,   144,   -93, -1784,  7409,   309,  -549,
    -513
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,  1140,   213,   383,   384,    80,    81,   385,   360,   386,
    1433,  1434,   387,   960,   961,   962,  1248,  1249,  1250,  1445,
     409,   389,   390,   391,   670,   671,   392,   393,   394,   395,
     396,   397,   398,   399,   400,   401,   402,   411,  1059,   672,
    1370,   733,   207,   735,   405,   800,  1141,  1142,  1143,  1144,
    1145,  1146,  1147,  2010,  1148,  1149,  1375,  1550,  1865,  1866,
    1800,  1801,  1802,  1980,  1981,  1150,  1564,  1565,  1566,  1709,
    1710,  1151,  1152,  1153,  1154,  1155,  1156,  1383,  1736,  1915,
    1838,  1157,  1158,  1582,  1996,  1583,  1584,  1898,  1159,  1160,
    1161,  1373,  1906,  1907,  1908,  2041,  2056,  1933,  1934,   284,
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
      79,   181,   517,    79,   183,   184,   185,   403,   530,   454,
     494,   404,   148,   675,   486,   789,   297,   337,  1873,   487,
     323,   558,   198,   488,   498,   355,  1183,   949,   131,   176,
     186,   231,   617,   904,   476,   969,   890,   351,  1364,   340,
     891,   898,   489,   490,  1782,   892,   289,   834,   836,  1253,
    1837,  1786,   594,  1783,    79,    79,  1784,    79,  1485,  1486,
     491,  1552,  1867,   190,    89,   187,   188,   149,   942,   625,
     492,   102,    79,   628,  1874,  1004,  1424,  1937,  1260,  1929,
      57,    79,  1032,   913,  1721,   131,   494,   198,  1039,    79,
     486,  -740,   565,   567,    79,   487,   899,    79,   437,   488,
      57,    79,   514,  1028,  1588,  1166,  1029,   838,   505,   196,
     256,   510,   421,   422,   423,  1022,   292,   845,   489,   490,
     279,    89,   228,   209,  1023,   253,   917,   144,   102,   263,
     627,  1024,   506,   527,   630,   660,   491,  1542,   424,    79,
    1025,  1294,    79,   537,    79,   140,   492,   484,   140,    79,
     183,   184,   185,   155,   617,    79,   495,   528,   872,   278,
     676,   246,    79,   499,   588,   257,  1860,   538,   209,   919,
     594,  1589,   131,   425,   426,  1875,   186,    57,  1938,    79,
      79,  1333,   107,   499,  1162,   523,    96,  1553,  1553,   150,
    1104,   359,   497,  -766,    79,  1867,  1873,   890,  2030,   600,
     898,   891,   140,   588,   467,  1452,   892,   196,    89,    79,
     358,   187,   188,  1130,   111,   102,   103,  1175,    79,    79,
    1868,    57,   458,   570,   715,   112,   183,   184,   185,  1873,
    1814,   660,   495,  1930,  1931,    79,   210,  1453,  -766,   107,
    1062,  1586,   523,    96,    79,   566,   140,   196,  1904,   280,
     825,  1346,  1347,   146,    79,  1033,   507,    79,   636,  1036,
     499,   638,   545,  1256,    79,  1218,   716,   194,  1049,  1050,
    1252,   111,   196,   103,    79,    79,  1008,    79,  1447,   807,
    1961,  1322,   112,   821,   808,   534,  1325,    95,   809,   140,
    1622,   274,  1334,  1241,    79,    79,  1552,  1053,  1053,   793,
     162,   865,    79,   617,   170,   170,   682,   810,   811,    79,
      79,   683,   914,   248,    79,   260,  1053,   600,   773,  1837,
    1587,   531,   883,   196,   287,   812,   107,   617,   967,   479,
      96,  1204,   194,  1782,   617,   813,  1032,  1232,   454,   170,
    1786,  1267,  1783,   760,    95,  1784,   903,    79,  1335,   279,
     198,  -741,    79,   561,  -382,    79,   644,  1393,   111,   909,
     103,  1351,   821,  -383,  1280,   807,  1348,  1281,  1214,   112,
     808,  1022,  1715,  1187,   809,  1869,   435,  1716,  1860,   832,
    1023,   156,  1053,   885,  1343,   837,   829,  1024,  1055,   170,
    1166,   201,   170,   810,   811,    57,  1272,   161,  1333,  1333,
    1333,  1295,   562,  1344,   566,   170,  1070,  1213,   840,   585,
     454,   812,   349,   586,   843,   421,   422,   423,   847,    79,
     910,   813,  1553,  1101,  -382,  1485,  1486,   519,   606,   822,
     522,    95,   323,  -383,   925,  1296,   927,   928,   844,   929,
    1350,   424,    79,    79,    19,   931,  1323,   458,   933,   934,
     935,   340,   190,   890,    79,    79,   201,   891,   191,   170,
    1714,  1204,   892,    79,   107,   467,    57,   175,    96,  1162,
     177,  1534,  1172,   532,   573,  1712,   425,   426,   499,   178,
    1720,  1797,  1798,    79,  1636,  1638,  1640,   522,   419,    57,
    1535,    57,    79,   600,   674,  1979,   111,   179,  1797,  1798,
    1343,   421,   422,   423,   170,  1510,  1928,   112,   822,  1334,
    1334,  1334,    79,  1413,   170,   279,   654,  1979,    79,  1599,
     458,   581,  1912,   202,   548,   170,    -3,   553,   179,    57,
    1635,  1286,  1544,    62,    63,  -668,   870,   454,  1382,   248,
    1296,   695,   696,  2012,   216,   751,  1262,   871,  1553,   499,
     582,   583,   170,    57,   524,  1913,    79,   236,    79,   170,
     170,   617,   695,  1799,   170,  1335,  1335,  1335,  1009,    79,
    1030,    79,   499,   157,   604,    79,   158,   159,   454,   160,
    1824,    75,   539,  1053,  -439,    79,    57,   274,    57,    79,
      57,  1477,   695,   551,   617,   194,   276,   170,    57,   458,
     454,   454,   170,   131,  1456,   170,   403,   278,  1037,   904,
    1043,   524,   604,  1236,  1188,  1819,  1820,  1058,  1063,   454,
    1237,   759,    79,   828,  1922,  1679,  1813,   944,   831,   293,
     937,   602,  1080,   561,    79,   279,   499,  1879,  1404,    89,
     773,   938,   939,  1534,  1680,   839,   102,    57,    13,    14,
      15,    16,    17,  -382,    57,   846,    57,    57,  1705,  1706,
    1707,   248,  1682,   887,  1426,  1084,  1345,  1308,   179,   499,
    1072,   499,  1683,  1305,   353,  1189,  -900,  1312,  1551,  1567,
    1708,   499,    79,   204,    79,   454,    79,  1540,  -755,  1088,
      79,   356,   170,    79,   205,   509,  1420,    13,    14,    15,
      16,    17,    57,  1553,   170,   170,    57,   545,  1882,  1883,
     206,   311,  1688,  1460,   357,  1575,  1401,   944,    79,   201,
     140,  1762,  1881,  1763,  1103,   717,  1411,  1888,  1553,   718,
     604,  1689,   993,  1462,  1893,  1471,  1475,   499,    72,   499,
     604,   428,   150,   403,   107,  1054,  1054,  1212,    96,   602,
     743,   705,   706,   944,   744,    57,   358,   107,   736,  1787,
    1425,    96,   499,    79,  1054,    79,  1553,  -443,   674,    77,
      78,   429,  1722,   674,   260,   248,   111,    79,  1788,   674,
    1688,  1923,  1207,   457,    79,   499,  1442,   112,   944,   111,
     467,   103,  1633,    79,   323,   707,   708,  1945,   674,  1791,
     112,   944,    79,    79,    79,   944,   146,  1795,  1293,   684,
     944,  1951,   461,   340,   685,  1953,  -444,   944,   179,  1257,
    1984,   698,    79,   430,   419,   419,  -561,  1986,   699,   700,
    1054,   887,  1831,   431,   532,   170,   475,  1832,  1089,   881,
     243,     6,     7,     8,     9,    10,    11,    12,   432,   971,
     243,     6,     7,     8,     9,    10,    11,    12,    79,    79,
     467,  1444,    95,   189,    63,  1705,  1706,  1707,  1252,  1966,
    2026,  1619,   907,   477,  1967,  2027,  1623,   944,   742,   794,
     795,    79,  1176,   796,   170,    18,   454,  1708,   755,   433,
    1318,  1300,   499,  1632,   753,  1058,  1713,   756,   870,   157,
    1279,   773,   158,   159,   617,   160,   278,    79,   427,   265,
     499,    79,  1572,  1551,   497,    79,   237,   238,    72,   239,
     462,   644,   903,   240,    89,   905,   480,    51,    52,    53,
      54,  1164,   953,   481,   955,   197,   958,  1177,   603,    72,
     966,   858,   604,   970,   515,   859,   419,  1092,   229,    77,
      78,   254,   482,   918,   509,   264,    79,   586,  1100,  1669,
     483,  1102,  1415,   499,    79,  1105,   496,  1516,   995,    61,
      77,    78,   516,  -440,    64,    65,    66,    67,    68,    69,
      70,   964,    13,    14,    15,    16,    17,  -430,   920,   526,
    1631,   921,   586,    79,  1013,   922,   467,   278,   499,   427,
      13,    14,    15,    16,    17,   140,   209,   266,  1435,   536,
    -430,   267,   577,   507,   270,   817,   272,   499,   140,    79,
     573,   965,   427,  1723,   499,    79,    79,   170,  1030,   555,
     427,  1054,   604,   197,   170,   544,    63,   355,   355,  1567,
      57,   943,   107,  1067,  1394,   944,    96,  1068,   459,  1340,
     592,   278,  1071,   419,  1073,   499,    79,   624,    57,  1178,
    -899,    13,    14,    15,    16,    17,   589,   274,   248,   600,
    1505,    72,   637,   197,   111,  -611,   103,   507,   323,   532,
     635,   499,  1758,   648,  1226,   112,  1094,  1457,  1096,  1230,
     944,   603,   944,  1554,   859,   604,   279,   340,   197,   649,
    1238,   248,    77,   605,   454,   454,   701,   702,  1112,   170,
     170,   535,  1752,   703,   704,   606,   573,    72,   467,    57,
     499,    79,    79,    79,   403,   403,  1532,   652,  1493,  1494,
     653,   170,  1577,  1578,  1579,  1580,  1581,   603,  1251,   697,
    1400,   604,  1252,  1487,   744,   467,  1430,    95,    77,   605,
    1252,    79,   266,   972,   973,   974,  1628,  1206,  -441,    79,
    1629,   170,    79,    79,   657,   170,    79,    13,    14,    15,
      16,    17,   256,   678,   427,   182,   499,    79,  1911,  1699,
      89,  1724,   870,   944,   419,   944,   711,  1164,  1725,  1726,
     253,   263,  1068,   944,  1935,  1792,  1876,   223,   712,   744,
     944,   742,   742,  1845,    79,   713,  1705,  1706,  1707,    89,
     467,  1011,   714,  1970,  1014,   745,  1164,  1252,   944,  1935,
      79,  1324,  1666,  1667,  1668,    57,   246,   257,  1708,   719,
     266,   267,   746,   621,  1349,   272,   467,  -180,   747,  2028,
     748,  2052,  1844,   944,    79,  2049,  2059,   709,   710,  1484,
    2060,  1368,   298,   434,  1998,   191,   678,  1982,  2002,   592,
     678,   140,   749,   323,   946,   947,  1340,  1340,  1340,   654,
    1518,  1340,  1533,   459,   777,   509,    79,  1045,  1046,  1082,
    1047,  1048,   340,  1086,   686,   750,   687,   688,   689,    -3,
     140,  1239,  1068,  -442,  1336,  1917,  1254,  1255,   107,  1326,
    1327,  1328,    96,   944,  1258,  -151,  -151,   791,   140,  1532,
     979,   980,   981,   982,   -17,   690,   790,   494,   691,   692,
     486,   485,   223,   693,   694,   487,   801,   107,  1554,   488,
     111,    96,   103,  1047,  1392,    79,   814,   148,   298,    79,
     815,   112,    79,   816,   695,   818,   459,   826,   489,   490,
    1450,  1451,  1455,  1451,  1695,  1459,  1451,  1019,  1443,   111,
     824,   103,   467,  1495,  1443,   819,   491,   870,   170,   820,
     112,   170,   170,   170,  1019,  1507,   492,   260,   248,  1436,
    1437,  1438,   467,   286,    79,   842,  1439,  1440,  1641,  1068,
    1760,  1068,   149,  1761,  1451,   170,   860,   571,   298,  1771,
    1772,   170,   873,    95,  1781,   944,  1835,  1836,   875,   553,
    -559,   532,   534,  1435,  1848,  1451,   170,   879,    89,    89,
    1849,  1451,  1797,  1798,  -557,  1556,  1556,  2049,  2050,  1448,
    1449,   851,    95,   975,   976,   882,   266,  1176,   977,   978,
     467,   893,   983,   984,   895,    79,  1691,  1691,   531,   606,
      79,    79,    79,  1684,   170,  1533,  1487,  1402,  1403,   323,
     912,   914,   923,   495,  1543,  1545,   916,   924,   742,   945,
    1431,   454,   454,   140,   948,   992,   951,   821,   340,   807,
    1018,   997,  1026,  1019,   808,  1065,  1074,  1075,   809,  1076,
    1077,  1078,  1177,   419,    13,    14,    15,    16,    17,   140,
     140,   905,  1597,  1079,  1095,  1097,  -744,   810,   811,  1167,
    1487,  1336,  1336,  1336,   150,  1515,  1519,  1106,    79,  1107,
    1108,  -644,  1168,  1184,    79,   812,    79,    13,    14,    15,
      16,    17,  1198,    79,  1199,   813,   107,   107,  1200,  1310,
      96,    96,  1314,  1210,  1216,  1215,   767,   467,  1219,  1221,
    1222,  1223,    57,  1224,  1225,  1227,  1228,  1229,  1899,  1234,
     467,  1235,  1299,  1242,   256,  1243,  1259,  1264,   111,   111,
     103,   103,  1265,   140,  1266,  1273,  1274,  1275,  1352,   112,
     112,  1276,  1284,  -632,  1532,    57,   806,  -631,  1319,   170,
    1307,  -745,   170,  1341,  1342,   223,  1355,   467,  1356,  1365,
     532,  1366,  1367,  1733,  1372,  1374,  1382,  -667,   617,  1840,
     944,    72,  1176,  1386,  1178,   298,  1774,  1389,   246,   257,
      79,   298,  1376,   822,  1390,  1427,   403,  1899,  1388,  1428,
    1864,   736,   170,  1396,  1398,   499,    79,  1441,    79,  1443,
    1458,  1470,    77,    78,    72,  1483,   306,   307,   308,   309,
    1488,  1489,  1490,    89,  -116,  -116,  -116,  -116,  -116,  -116,
    1556,   298,  1491,  1451,  1669,  1499,  1496,  1177,   499,  1508,
     454,  1509,   869,  1511,   298,    77,    78,  -115,  -115,  -115,
    -115,  -115,  -115,    79,    61,  1547,    79,   168,   169,    64,
      65,    66,    67,    68,    69,    70,  1521,   467,  1523,  1727,
     140,   467,    13,    14,    15,    16,    17,   941,  1487,  1524,
    1345,  1568,  1569,  1571,   467,  1573,  1585,  1590,  1592,  1593,
     631,  1595,  1594,  1602,   467,  1600,  1603,  1607,  1605,  1464,
    1533,  1606,  1624,  1612,   140,  1608,   310,  1621,  1473,  1609,
    1610,    79,    79,  1617,  1626,  1634,  1830,  1735,   140,   494,
      79,  1627,  1630,   486,   311,  1643,  1208,   403,   487,   403,
    1642,  1977,   488,  1864,  1647,  1960,  1648,   531,  1901,   260,
     248,   107,   427,  1656,  1495,    96,  1658,  1665,  1526,    89,
    1678,   489,   490,   170,  1698,  1252,  1556,   821,  1700,  1178,
     210,  1702,  1730,    79,   403,  1732,  1737,   170,  2000,   491,
     467,  1744,  1748,   111,   467,   103,  1749,   467,  1750,   492,
     170,  1751,    61,  1765,   112,   168,   169,    64,    65,    66,
      67,    68,    69,    70,  2021,  1753,  1759,  1766,  1776,  1805,
     419,  1769,   467,    61,  1770,  1779,  1780,  1901,    64,    65,
      66,    67,    68,    69,    70,  1808,  1905,   170,    61,  1809,
    1823,   168,   169,    64,    65,    66,    67,    68,    69,    70,
     140,  1821,  1827,  1833,  1130,  1829,  1021,  1834,   767,  1842,
     170,   403,  1843,   467,  1846,  2051,  1847,   467,  -633,  1878,
    1855,  1856,    82,  -542,  1857,   147,   570,  1858,   467,   183,
     184,   185,  1859,   499,  1884,   495,  1889,   107,    79,  1887,
      79,    96,  1902,  1916,  1919,  1918,   298,  1952,  1932,  1894,
    1920,   467,  1903,   467,   467,  1921,  1772,  1954,  1963,   532,
    1964,  1968,  1965,  1969,  1972,   298,  1975,  2006,  1985,   111,
    2022,   103,  1995,   822,    89,  1999,   467,  2007,  2023,    82,
     112,  1556,  2001,  2025,  2035,  2037,  2038,   170,  2042,  2046,
    1454,   170,  2047,  2057,   180,   681,  1756,    79,    79,    89,
    2043,  2058,  2061,    82,   170,   940,  1556,  1541,   467,   985,
     987,   989,   734,   986,   170,  1378,   220,  1905,   467,   245,
    1673,  1905,  1905,    82,   988,  1371,   196,  1910,  2036,  1734,
    1978,   170,  1825,  1818,  1993,  1914,    79,    89,  2031,  2020,
     170,  2029,  1728,  1956,  1556,  2003,  1729,  2044,  1955,   467,
     167,   467,  1681,  2024,  1387,   140,   254,   264,  1927,  1862,
     147,   525,  1520,  1692,   458,  1384,    82,   797,   147,   467,
     877,   296,   302,  1064,  1186,   467,   388,  2040,  1741,  1625,
     140,  2040,     3,   322,  1217,   467,  1000,  1001,  1002,    79,
     170,     0,   107,     0,   170,     0,    96,   170,     0,    79,
     410,   180,   180,  2054,   243,     6,     7,     8,     9,    10,
      11,    12,   147,   440,     0,     0,   245,   107,   140,     0,
       0,    96,   170,     0,   111,     0,   103,     0,     0,   468,
       0,     0,     0,     0,     0,   112,   639,    61,     0,     0,
     220,   220,    64,    65,    66,    67,    68,    69,    70,   111,
       0,   103,     0,     0,     0,   107,     0,   296,     0,    96,
     112,     0,  1021,   170,     0,     0,    82,   170,  1278,   767,
       0,     0,     0,     0,  1673,  1673,     0,     0,   170,   245,
    2039,     0,     0,     0,     0,  1277,    74,   111,     0,   103,
    1962,     0,     0,     0,     0,     0,  2048,     0,   112,     0,
       0,   170,     0,   170,   170,     0,     0,     0,     0,   302,
     640,     0,     0,     0,     0,   302,   296,   296,     0,     0,
       0,     0,     0,   147,     0,   641,   170,     0,   642,   643,
      64,    65,    66,    67,    68,    69,    70,    13,    14,    15,
      16,    17,  1231,   322,   607,   616,    61,     0,     0,   168,
     169,    64,    65,    66,    67,    68,    69,    70,   170,   651,
     322,     0,   388,   656,   322,     0,     0,     0,   170,     0,
     236,     0,   662,   663,    61,     0,     0,     0,   535,    64,
      65,    66,    67,    68,    69,    70,   956,   388,   388,     0,
    1673,     0,     0,     0,     0,     0,     0,   410,     0,   170,
       0,   170,     0,     0,     0,  1361,   857,    61,   388,     0,
       0,   448,    64,    65,    66,    67,    68,    69,    70,   170,
     298,     0,     0,     0,     0,   170,   957,     0,     0,     0,
       0,   410,     0,     0,   737,   170,     0,     0,   388,  2055,
       0,   180,    18,     0,     0,     0,     0,     0,     0,  2062,
       0,     0,     0,     0,     0,     0,    74,   147,     0,   786,
       0,   440,  1925,     0,     0,   766,  1673,   616,    61,     0,
       0,   217,   218,    64,    65,    66,    67,    68,    69,    70,
      47,    48,    49,    50,    51,    52,    53,    54,   787,     0,
     468,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,  1673,   220,  1093,     0,     0,     0,
       0,     0,     0,     0,   220,     0,  1525,    74,     0,  1478,
       0,     0,     0,  1526,     0,     0,     0,    77,    78,     0,
       0,     0,     0,     0,   296,     0,   410,   410,     0,     0,
     296,   597,   322,    61,   620,     0,   346,   347,    64,    65,
      66,    67,    68,    69,    70,   249,     0,     0,   597,     0,
       0,     0,   597,  1673,  1673,  1197,   269,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1531,    61,
     296,     0,   168,   169,    64,    65,    66,    67,    68,    69,
      70,   296,     0,   296,    75,   322,     0,    82,     0,   348,
       0,     0,  1673,     0,     0,     0,     0,     0,   249,     0,
       0,     0,     0,   322,   440,     0,   616,     0,     0,     0,
       0,     0,     0,     0,   607,     0,     0,     0,   607,     0,
       0,     0,  1379,     0,     0,   579,     0,   322,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   616,     0,     0,
     322,     0,   249,     0,     0,     0,     0,    61,   147,   597,
     168,   169,    64,    65,    66,    67,    68,    69,    70,     0,
       0,   410,     0,   147,   147,  1263,   410,     0,     0,     0,
       0,     0,   410,    57,   857,   147,   147,   147,     0,     0,
       0,     0,  1270,  1271,     0,   388,   388,   388,   388,   388,
     388,   388,   388,   388,   388,   388,   388,   388,   388,   388,
     388,   388,   388,   388,    61,   249,     0,   217,   218,    64,
      65,    66,    67,    68,    69,    70,     0,     0,    61,     0,
    1380,   217,   218,    64,    65,    66,    67,    68,    69,    70,
     448,   440,     0,   857,     0,   249,     0,     0,     0,     0,
       0,   249,     0,     0,     0,     0,     0,   737,   737,     0,
      57,  1531,  1277,    74,     0,   410,     0,  1685,     0,  1531,
       0,   114,     0,     0,   114,   388,     0,     0,     0,     0,
       0,   249,   440,     0,  1205,   766,     0,   766,     0,     0,
       0,    61,     0,   448,   217,   218,    64,    65,    66,    67,
      68,    69,    70,     0,   322,   322,     0,     0,     0,     0,
       0,   597,   448,     0,     0,   468,     0,     0,   787,    72,
       0,     0,     0,   322,     0,   296,     0,     0,   114,     0,
       0,     0,     0,     0,     0,   597,     0,     0,     0,  1958,
      74,     0,     0,   499,   296,     0,     0,     0,   597,     0,
      77,    78,   114,   857,    61,     0,     0,   168,   169,    64,
      65,    66,    67,    68,    69,    70,     0,     0,   251,     0,
     857,   857,   114,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   410,     0,     0,     0,     0,     0,     0,   322,
       0,     0,     0,     0,     0,   147,   410,     0,     0,     0,
       0,     0,     0,   249,     0,   322,     0,  1192,     0,   114,
       0,     0,   388,     0,     0,   114,     0,   114,   607,     0,
       0,   251,     0,     0,     0,     0,  1793,  1357,     0,  1531,
     388,   318,   114,   350,     0,   388,     0,     0,     0,   448,
       0,     0,     0,     0,  1466,  1467,   388,     0,     0,   414,
       0,     0,     0,     0,     0,     0,     0,   440,  1481,  1482,
       0,   114,   414,     0,    61,   251,     0,   168,   169,    64,
      65,    66,    67,    68,    69,    70,     0,   249,     0,     0,
     448,  1247,     0,     0,     0,   298,     0,     0,   388,  1247,
       0,     0,  1503,  1504,     0,     0,     0,   249,     0,     0,
       0,     0,    13,    14,    15,    16,    17,     0,     0,     0,
       0,     0,   114,     0,     0,   114,     0,   249,  1247,     0,
       0,   468,     0,     0,   737,     0,     0,     0,   251,     0,
       0,     0,     0,     0,     0,     0,  1531,  1359,     0,     0,
       0,   766,     0,     0,     0,   549,     0,     0,   766,     0,
       0,     0,   249,   114,     0,     0,     0,     0,   251,     0,
      57,     0,     0,     0,   251,     0,     0,     0,     0,     0,
       0,     0,   114,     0,     0,     0,   249,     0,     0,     0,
       0,  1247,     0,   249,     0,     0,     0,     0,     0,     0,
     322,    61,   114,     0,   251,   114,    64,    65,    66,    67,
      68,    69,    70,   597,     0,     0,   620,     0,     0,   114,
       0,     0,     0,   114,     0,   388,   249,   269,     0,    72,
       0,     0,   857,   857,     0,     0,     0,     0,     0,     0,
     147,     0,     0,     0,     0,     0,   857,   857,   410,    73,
      74,   298,     0,     0,     0,     0,   414,     0,     0,     0,
      77,    78,     0,    61,     0,   448,   546,   547,    64,    65,
      66,    67,    68,    69,    70,     0,     0,   410,     0,     0,
     857,   857,     0,     0,     0,     0,     0,  1660,  1661,     0,
     414,     0,     0,     0,   245,    82,     0,     0,     0,     0,
     388,     0,     0,     0,     0,     0,     0,     0,     0,   296,
     571,   298,     0,     0,    75,   147,   114,     0,     0,     0,
     414,     0,     0,   440,     0,     0,   251,     0,     0,     0,
       0,     0,     0,     0,     0,   388,   388,   388,     0,     0,
       0,     0,   388,   388,     0,   468,     0,     0,     0,   298,
       0,   440,   366,     0,   367,   147,   368,    62,    63,    64,
      65,    66,    67,    68,    69,    70,   388,     0,     0,     0,
       0,   468,     0,     0,     0,    61,     0,     0,     0,  1247,
      64,    65,    66,    67,    68,    69,    70,  1244,     0,     0,
       0,  1245,     0,  1246,     0,   414,   414,     0,     0,  1745,
     251,   114,   680,   388,   388,    75,   377,   721,   722,   723,
     724,   725,   726,   727,   728,   729,   730,   731,   322,   322,
     249,   204,     0,     0,    74,     0,     0,  1446,     0,     0,
    1764,   249,   114,     0,     0,  1767,  1768,   114,     0,     0,
     251,   114,     0,   114,     0,     0,     0,     0,   732,     0,
       0,     0,     0,   249,   114,     0,   114,   147,   147,   147,
     147,   147,   147,     0,     0,   857,   857,  1527,   302,     0,
     350,     0,   114,   414,    61,   251,     0,   168,   169,    64,
      65,    66,    67,    68,    69,    70,   410,   410,     0,     0,
       0,     0,     0,     0,     0,     0,   114,   468,     0,   251,
       0,  1696,     0,   549,     0,     0,   251,     0,     0,   114,
       0,   911,     0,     0,     0,     0,   245,   114,     0,     0,
       0,   597,   457,    13,    14,    15,    16,    17,     0,     0,
     414,     0,   114,   114,     0,   414,   440,     0,     0,     0,
       0,   414,     0,     0,   114,   114,   114,    61,     0,   448,
       0,     0,    64,    65,    66,    67,    68,    69,    70,  1244,
     147,     0,     0,  1245,     0,  1246,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   468,     0,   857,     0,     0,
    1247,    57,     0,     0,     0,  1247,  1247,  1247,     0,     0,
       0,     0,     0,     0,    57,     0,    74,     0,     0,  1637,
     414,     0,     0,     0,     0,     0,     0,     0,   857,     0,
       0,     0,    61,   857,   857,   217,   218,    64,    65,    66,
      67,    68,    69,    70,   414,    61,     0,     0,   217,   218,
      64,    65,    66,    67,    68,    69,    70,     0,     0,     0,
      72,   414,     0,     0,     0,     0,  1670,     0,     0,     0,
    1527,     0,   410,    72,     0,  1354,  1527,     0,  1527,     0,
     219,    74,     0,   114,   114,     0,     0,     0,     0,     0,
       0,    77,    78,   219,    74,     0,     0,     0,     0,     0,
       0,     0,   114,     0,    77,    78,   302,   147,     0,     0,
       0,     0,     0,     0,     0,    13,    14,    15,    16,    17,
       0,     0,     0,   388,     0,     0,     0,    61,     0,     0,
     114,     0,    64,    65,    66,    67,    68,    69,    70,  1244,
       0,   410,     0,  1245,     0,  1246,     0,     0,     0,     0,
     249,     0,   322,   251,   448,   147,     0,     0,     0,    98,
       0,   414,   151,     0,   251,     0,     0,     0,   114,     0,
       0,     0,     0,    57,   114,   414,    74,     0,     0,  1639,
     147,     0,  2008,   249,   114,     0,  1194,   414,     0,   114,
       0,  1247,    61,  1247,     0,   189,    63,    64,    65,    66,
      67,    68,    69,    70,    61,   322,   322,   217,   218,    64,
      65,    66,    67,    68,    69,    70,    98,     0,     0,    61,
    1670,  1670,   544,    63,    64,    65,    66,    67,    68,    69,
      70,     0,    72,     0,     0,  1527,   414,     0,  1527,    57,
     195,    74,     0,     0,   786,     0,     0,     0,     0,     0,
       0,     0,   764,    74,     0,   302,   604,     0,     0,     0,
     258,     0,     0,    77,   765,     0,     0,   410,     0,     0,
      61,   994,     0,   217,   218,    64,    65,    66,    67,    68,
      69,    70,    61,     0,     0,    62,    63,    64,    65,    66,
      67,    68,    69,    70,   296,     0,     0,   288,    72,   114,
       0,    61,     0,    98,   168,   169,    64,    65,    66,    67,
      68,    69,    70,     0,     0,     0,   114,   114,   295,    74,
     324,     0,     0,     0,   249,   388,     0,     0,     0,    77,
      78,     0,     0,    75,     0,     0,  1670,     0,   420,  1596,
       0,     0,     0,     0,     0,  1527,    61,     0,     0,   288,
     446,    64,    65,    66,    67,    68,    69,    70,  1244,     0,
     857,     0,  1245,   388,  1246,     0,     0,     0,     0,   114,
       0,     0,   249,     0,     0,     0,     0,     0,   493,   147,
       0,    61,     0,     0,   168,   169,    64,    65,    66,    67,
      68,    69,    70,     0,   513,    74,    57,     0,     0,   518,
     520,     0,     0,   195,   322,     0,     0,     0,     0,   114,
       0,     0,  1670,     0,     0,     0,     0,   414,     0,     0,
     108,     0,   147,     0,     0,   540,     0,    61,   542,   461,
     543,     0,    64,    65,    66,    67,    68,    69,    70,     0,
       0,   560,     0,     0,     0,     0,   414,   147,   147,     0,
    1959,   302,     0,     0,   572,    72,   388,     0,   388,     0,
       0,     0,     0,   251,   114,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    73,    74,   108,     0,     0,
     595,     0,     0,   619,   114,   147,    77,    78,     0,     0,
       0,     0,   414,   388,     0,     0,  1194,   626,     0,    57,
       0,   626,     0,     0,  1701,     0,     0,     0,  1423,  1959,
    1959,     0,     0,     0,     0,     0,     0,  1711,     0,     0,
     414,   259,     0,   388,   114,   659,     0,     0,     0,     0,
      61,     0,     0,   217,   218,    64,    65,    66,    67,    68,
      69,    70,   249,     0,     0,     0,     0,     0,  1959,     0,
       0,     0,     0,     0,  1739,     0,     0,     0,    72,     0,
       0,     0,     0,     0,   108,     0,   165,     0,   114,   114,
     388,     0,     0,     0,     0,     0,     0,   597,  1525,    74,
       0,   328,   114,   114,     0,     0,     0,   114,   114,    77,
      78,     0,   165,     0,   288,     0,     0,     0,   595,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   447,     0,     0,     0,     0,   114,   114,     0,     0,
       0,   659,     0,     0,     0,     0,   114,   114,   114,   114,
     114,   114,     0,     0,     0,     0,     0,   251,   165,     0,
       0,     0,     0,     0,     0,     0,   597,     0,     0,     0,
       0,   165,     0,   165,  1796,   414,   414,     0,  1806,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1817,     0,     0,     0,     0,     0,     0,     0,   446,
       0,  1826,     0,   352,     0,   251,   541,     0,    61,     0,
       0,   249,     0,    64,    65,    66,    67,    68,    69,    70,
     352,     0,   108,     0,     0,   414,     0,     0,     0,     0,
     853,     0,     0,     0,     0,   520,    72,     0,     0,   864,
       0,   560,     0,     0,     0,     0,     0,     0,     0,   114,
       0,     0,   324,     0,    98,     0,  1020,    74,   165,     0,
     604,   596,   165,     0,   259,   165,   165,    77,    78,   165,
     626,   886,   165,   165,     0,     0,     0,  1872,   596,     0,
       0,  1877,   596,     0,  1880,   897,     0,     0,     0,     0,
       0,     0,     0,     0,   595,     0,     0,     0,   113,   906,
       0,     0,     0,     0,     0,     0,     0,   626,     0,  1909,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   114,   114,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   165,     0,     0,   165,     0,     0,
       0,   414,     0,     0,     0,     0,     0,     0,     0,     0,
    1936,     0,     0,     0,  1939,   113,     0,   114,   165,   165,
       0,     0,     0,     0,     0,  1950,     0,     0,     0,     0,
       0,     0,     0,     0,   165,   251,   114,     0,     0,   596,
       0,     0,     0,     0,     0,     0,     0,     0,  1971,     0,
    1973,  1974,     0,     0,     0,     0,     0,     0,   446,   261,
       0,     0,     0,     0,     0,     0,     0,   249,     0,     0,
     414,     0,     0,  1983,     0,  1003,     0,     0,     0,     0,
       0,   114,     0,     0,   114,     0,     0,     0,     0,     0,
       0,     0,     0,   114,     0,     0,     0,     0,     0,   886,
       0,     0,   113,     0,  1027,  2004,     0,     0,     0,   114,
       0,     0,     0,     0,     0,  2009,     0,     0,     0,   332,
     447,   446,   446,     0,   114,     0,     0,     0,   165,   114,
     114,     0,     0,     0,   114,   114,     0,     0,     0,     0,
     446,     0,     0,     0,     0,     0,  2034,     0,  2009,   449,
       0,   328,     0,    13,    14,    15,    16,    17,     0,     0,
     259,     0,   108,     0,     0,     0,  2045,     0,   853,     0,
       0,     0,  2034,   447,     0,   108,     0,     0,     0,     0,
       0,     0,  2053,   352,   251,     0,     0,     0,     0,     0,
       0,   596,   447,     0,     0,   165,   414,     0,     0,  1163,
       0,     0,     0,     0,     0,     0,   446,     0,     0,     0,
       0,    57,   151,     0,     0,   596,     0,     0,     0,     0,
       0,     0,   626,     0,     0,  1196,     0,   853,   596,     0,
       0,     0,  1202,     0,     0,     0,     0,     0,     0,     0,
     113,     0,    61,     0,     0,   217,   218,    64,    65,    66,
      67,    68,    69,    70,     0,    61,     0,     0,   217,   218,
      64,    65,    66,    67,    68,    69,    70,     0,     0,   352,
      72,     0,     0,     0,   324,     0,     0,     0,     0,   598,
       0,     0,   261,    72,     0,     0,     0,     0,     0,     0,
    1958,    74,     0,     0,   499,     0,   598,     0,     0,     0,
     598,    77,    78,   764,    74,     0,     0,   604,   114,     0,
     165,   165,     0,     0,    77,   765,     0,     0,     0,   447,
       0,     0,     0,   165,     0,     0,     0,   606,     0,     0,
       0,     0,     0,   114,     0,     0,    61,   853,     0,   217,
     218,    64,    65,    66,    67,    68,    69,    70,     0,     0,
       0,   114,     0,     0,   853,   853,     0,     0,     0,     0,
     447,     0,    61,     0,    72,   217,   218,    64,    65,    66,
      67,    68,    69,    70,     0,     0,   114,   114,     0,     0,
     251,     0,   328,   328,  1525,    74,     0,     0,     0,     0,
      72,  1526,     0,     0,     0,    77,    78,   598,     0,     0,
       0,   328,     0,     0,     0,     0,     0,   446,     0,     0,
     219,    74,     0,     0,   114,     0,     0,     0,     0,     0,
       0,    77,    78,     0,     0,     0,     0,     0,     0,   328,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   165,
     165,     0,     0,     0,     0,   165,     0,  1337,     0,     0,
       0,     0,     0,     0,     0,  1163,   114,     0,     0,     0,
     108,     0,     0,     0,     0,     0,   165,   328,     0,   165,
     165,     0,   165,     0,   165,   165,     0,     0,   449,     0,
       0,     0,     0,   596,  1163,     0,   259,     0,   328,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1385,     0,     0,     0,     0,     0,     0,   332,
       0,     0,     0,   165,     0,     0,     0,   165,   261,     0,
     113,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     595,   449,     0,   113,     0,   447,     0,     0,     0,   518,
       0,    13,    14,    15,    16,    17,     0,     0,     0,   598,
     449,     0,     0,     0,     0,     0,     0,     0,   324,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   598,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   598,     0,     0,     0,
       0,     0,     0,     0,   165,     0,     0,     0,   328,    57,
       0,     0,     0,     0,     0,     0,   853,   853,     0,     0,
       0,     0,     0,     0,     0,   328,   328,     0,     0,     0,
     853,   853,     0,     0,     0,   446,   446,     0,     0,     0,
      61,     0,     0,   217,   218,    64,    65,    66,    67,    68,
      69,    70,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   853,   853,     0,     0,    72,     0,
       0,     0,     0,     0,  1337,  1337,  1337,   151,   328,     0,
       0,     0,     0,     0,     0,     0,     0,   449,   295,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    77,
      78,     0,     0,  1555,  1555,     0,     0,     0,     0,     0,
      13,    14,    15,    16,    17,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   108,     0,   449,     0,
       0,     0,     0,     0,     0,   165,    61,     0,     0,   217,
     218,    64,    65,    66,    67,    68,    69,    70,     0,     0,
     332,   332,     0,   324,     0,   108,     0,     0,     0,     0,
       0,     0,     0,     0,    72,     0,     0,     0,    57,   332,
       0,     0,     0,   259,     0,     0,   165,   151,     0,     0,
       0,     0,     0,   165,  1958,    74,   165,     0,   499,   118,
       0,     0,   118,     0,     0,    77,    78,   332,     0,    61,
       0,   596,   217,   218,    64,    65,    66,    67,    68,    69,
      70,     0,    61,     0,     0,   217,   218,    64,    65,    66,
      67,    68,    69,    70,     0,     0,     0,    72,   113,   447,
       0,     0,     0,     0,     0,   332,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,   118,  1525,    74,   853,
     853,   598,     0,     0,   261,     0,   332,     0,    77,    78,
     295,    74,     0,     0,     0,     0,     0,     0,     0,     0,
     118,    77,    78,     0,     0,  1687,     0,   328,   328,     0,
       0,     0,     0,     0,     0,   853,     0,     0,     0,     0,
     118,   328,   328,     0,     0,     0,   328,   328,     0,   119,
       0,     0,   119,   449,  1704,     0,     0,   165,     0,     0,
       0,     0,     0,     0,     0,   165,   165,     0,     0,     0,
       0,     0,     0,     0,     0,   328,   328,   118,     0,     0,
       0,     0,     0,   118,     0,   118,     0,     0,  1555,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   324,
       0,     0,   151,     0,     0,     0,   119,     0,     0,     0,
       0,   853,     0,     0,   108,   108,   332,   118,     0,     0,
       0,     0,     0,   165,     0,     0,     0,     0,     0,   118,
     119,     0,   165,   332,   332,   165,     0,   165,   165,     0,
       0,     0,   853,     0,     0,     0,     0,   853,   853,     0,
     119,     0,   446,   446,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   447,     0,     0,     0,  1785,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   165,     0,
     118,     0,     0,   118,     0,     0,   332,   119,   118,     0,
       0,     0,     0,   119,     0,   119,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1555,     0,     0,     0,     0,     0,
       0,   118,     0,     0,     0,     0,     0,   119,     0,     0,
       0,     0,     0,     0,   113,     0,     0,     0,     0,   119,
     118,     0,     0,     0,     0,   165,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     328,   328,     0,   113,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   261,     0,     0,     0,     0,     0,     0,     0,     0,
     119,     0,     0,   119,     0,     0,   328,     0,   119,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   598,
       0,     0,     0,     0,   118,   259,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1900,     0,     0,   165,
       0,   119,     0,     0,     0,     0,     0,   449,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   118,   108,
     119,   446,     0,     0,     0,     0,     0,     0,   165,     0,
     328,     0,     0,     0,     0,     0,     0,     0,     0,  1555,
       0,     0,   328,     0,   118,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   165,   332,   332,     0,     0,     0,
     165,     0,     0,     0,  1555,  1900,     0,     0,     0,   332,
     332,     0,     0,   328,   332,   332,     0,     0,   328,   328,
       0,     0,     0,   328,   328,     0,     0,     0,     0,     0,
       0,     0,     0,   123,   119,     0,   123,     0,     0,     0,
       0,     0,  1555,   332,   332,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   118,   118,     0,     0,  1997,   119,   165,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   113,   113,   853,   108,     0,     0,     0,     0,
     123,     0,     0,     0,   119,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   118,     0,     0,     0,   118,
       0,   118,     0,     0,   123,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   118,     0,     0,     0,     0,     0,
       0,     0,   449,     0,   123,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   165,   165,     0,     0,     0,
       0,     0,     0,   352,     0,     0,     0,   165,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   123,     0,   119,   119,     0,     0,   123,     0,   123,
       0,     0,     0,     0,     0,   118,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   596,   118,     0,
     118,   118,     0,   118,     0,     0,     0,     0,     0,   118,
       0,   123,   118,   118,   118,   119,     0,     0,     0,   119,
       0,   119,   328,   123,     0,     0,     0,     0,   332,   332,
       0,     0,     0,     0,   119,     0,     0,     0,     0,     0,
     108,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   165,     0,     0,     0,
       0,     0,     0,     0,   332,   108,   596,     0,     0,     0,
       0,     0,     0,     0,   123,     0,     0,   123,     0,     0,
       0,     0,   123,   261,     0,     0,     0,     0,     0,     0,
       0,     0,   118,     0,     0,   119,     0,     0,     0,     0,
       0,     0,     0,   108,     0,     0,     0,     0,   119,     0,
     119,   119,     0,   119,     0,   123,     0,   113,     0,   119,
       0,     0,   119,   119,   119,     0,   165,     0,   332,     0,
       0,     0,     0,     0,   123,     0,     0,     0,     0,     0,
     332,     0,     0,     0,     0,   328,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   332,     0,     0,     0,     0,   332,   332,  1863,     0,
       0,   332,   332,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   119,     0,     0,     0,     0,     0,   123,   118,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   118,   118,     0,   361,   203,   165,     0,   362,
       0,   363,   214,   215,     0,     0,     0,     0,     0,     0,
       0,     0,   123,   113,     0,     0,     0,     0,   364,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   277,     0,   123,     0,
       0,     0,     0,     0,     0,   365,   366,     0,   367,     0,
     368,    62,    63,    64,    65,    66,    67,    68,    69,    70,
     369,   370,   358,     0,   371,   372,   373,     0,   374,   375,
       0,   361,     0,     0,     0,   362,    72,   363,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   119,
       0,     0,     0,     0,   364,     0,   376,     0,     0,    75,
     377,     0,   119,   119,     0,     0,   378,    77,    78,   379,
     380,   381,   382,     0,     0,     0,     0,   123,   123,     0,
       0,   365,   366,     0,   463,   598,   368,    62,    63,    64,
      65,    66,    67,    68,    69,    70,   369,   370,   358,     0,
     371,   372,   373,     0,   374,   375,     0,     0,     0,     0,
     332,     0,    72,     0,     1,     0,     0,   145,     0,   123,
       0,     0,     0,   123,     0,   123,     0,     0,   113,     0,
       0,     0,   376,    74,     0,   464,   465,     0,   123,     0,
     466,     0,   378,    77,    78,   379,   380,   381,   382,     0,
       0,     0,     0,   113,   598,     0,     0,     0,     0,     0,
       0,     0,   568,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   118,     0,     0,
     192,   113,     0,     0,     0,   118,     0,     0,     0,   123,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   123,     0,   123,   123,     0,   123,     0,     0,
       0,     0,     0,   123,   118,     0,   123,   123,   123,     0,
       0,     0,     0,   332,     0,     0,     0,     0,     0,     0,
       0,     0,   118,     0,     0,   242,     0,     0,     0,   283,
       0,     0,     0,     0,    13,    14,    15,    16,    17,     0,
       0,    19,   118,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,  -446,  -446,
       0,  -446,    45,    46,     0,  -446,     0,   119,     0,     0,
       0,     0,   118,     0,     0,   119,   123,     0,     0,   762,
       0,   763,    57,     0,     0,     0,     0,     0,     0,     0,
     779,   780,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   119,     0,     0,     0,     0,     0,
       0,     0,     0,   283,     0,     0,    62,    63,     0,     0,
       0,     0,   119,     0,     0,     0,     0,     0,   521,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   283,     0,
       0,    72,   119,     0,     0,     0,     0,     0,   283,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   552,   556,    75,   301,     0,     0,     0,   563,
     564,     0,    77,    78,   118,   118,   118,   118,   118,   118,
       0,     0,   119,     0,     0,   574,     0,     0,     0,     0,
       0,     0,     0,   123,     0,     0,     0,     0,   863,     0,
       0,     0,     0,   118,   118,   593,   123,   123,     0,     0,
      13,    14,    15,    16,    17,    18,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,     0,     0,    45,    46,
       0,     0,     0,    47,    48,    49,    50,    51,    52,    53,
      54,   679,     0,     0,     0,     0,     0,     0,    57,     0,
       0,     0,     0,     0,     0,     0,     0,   118,     0,     0,
       0,     0,     0,     0,   119,   119,   119,   119,   119,   119,
       0,     0,   720,     0,     0,     0,     0,     0,     0,    61,
       0,     0,    62,    63,    64,    65,    66,    67,    68,    69,
      70,     0,     0,   119,   119,     0,     0,     0,   758,     0,
     171,   174,   761,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   783,     0,     0,     0,   784,   785,    73,    74,   788,
      75,    76,     0,     0,     0,   212,     0,     0,    77,    78,
       0,     0,     0,     0,   802,   803,   804,   805,     0,   118,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   827,     0,     0,     0,   119,     0,     0,
       0,   830,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   118,   290,     0,     0,   291,     0,
       0,     0,     0,     0,     0,     0,     0,  1042,     0,   283,
       0,   312,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   118,     0,
       0,   123,     0,     0,     0,     0,     0,     0,     0,   123,
     868,     0,   118,     0,     0,     0,     0,   552,     0,     0,
       0,     0,     0,   874,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   478,     0,   118,   123,   119,
       0,     0,     0,     0,     0,     0,     0,   889,   894,     0,
       0,     0,     0,     0,  1110,  1111,   123,     0,     0,     0,
       0,     0,     0,     0,     0,  1169,  1170,  1171,     0,     0,
    1173,     0,     0,     0,   119,     0,   123,     0,     0,     0,
     529,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     171,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   171,     0,     0,     0,     0,     0,     0,   119,     0,
       0,   936,     0,     0,     0,     0,   123,     0,     0,     0,
       0,     0,   119,     0,   118,     0,     0,     0,   575,     0,
       0,     0,     0,     0,     0,   578,   580,     0,     0,     0,
     587,     0,     0,     0,     0,     0,     0,   119,     0,     0,
    1240,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   633,     0,     0,     0,     0,   312,     0,
     999,   312,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1016,  1261,     0,     0,  1017,
       0,     0,     0,     0,     0,     0,     0,     0,   889,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   123,   123,
     123,   123,   123,   123,     0,     0,     0,     0,     0,     0,
    1057,     0,     0,     0,   119,     0,     0,     0,     0,  1066,
       0,     0,     0,  1285,     0,  1069,     0,   123,   123,     0,
       0,     0,  1289,  1290,  1291,  1292,     0,     0,     0,     0,
    1297,  1298,     0,     0,     0,     0,     0,     0,   212,     0,
    1306,     0,     0,     0,     0,     0,     0,     0,     0,   118,
     781,   782,     1,     0,     0,     0,     0,     0,     0,     1,
       0,  1320,     0,  1321,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   118,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     1,     0,     0,     0,
       0,   123,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   118,     0,     0,     0,  1377,     0,     0,     0,
     247,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   268,     0,   271,     0,   273,     0,     0,     0,     0,
    1220,     0,  1391,     0,     0,     0,     0,     0,     0,  1395,
       0,  1397,  1399,     0,     0,     0,     0,     0,     0,   119,
       0,  1406,     0,  1407,     0,  1408,     0,  1410,     0,     0,
       0,     0,  1418,   247,     0,   271,   273,     0,     0,     0,
       0,   312,     0,   123,   119,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   247,   123,     0,
       0,     0,   119,  1268,     0,     0,     0,  1269,     0,     0,
     633,     0,     0,  1461,   889,     0,     0,     0,     0,     0,
    1468,  1469,     0,     0,  1282,     0,     0,     0,     0,     0,
       0,  1283,   123,     0,     0,     0,     0,     0,     0,     0,
    1287,     0,  1288,     0,  1492,     0,   123,     0,     0,     0,
       0,  1497,     0,     0,     0,  1498,     0,     0,     0,     0,
     247,     0,   271,   273,     0,     0,     0,     0,     0,     0,
       0,   123,     0,     0,  1316,     0,     0,     0,  1317,     0,
       0,     0,     0,     0,     0,   214,     0,     0,     0,     0,
     247,     0,   145,     0,     0,     1,   247,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1591,   247,     0,     0,     0,
       0,     0,   622,     0,   273,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1044,     0,     0,     0,     0,   123,  1611,
    1056,     0,     0,     0,     0,     0,     0,  1616,     0,  1618,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1405,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1429,
       0,     0,     0,     0,     0,     0,  1645,  1646,     0,     0,
       0,     0,   247,     0,     0,     0,     0,     0,     0,     0,
       0,  1651,  1652,     0,  1653,  1114,     0,     0,     0,     0,
       0,     0,     0,  1657,     0,     0,     0,     0,   247,     0,
     622,   273,     0,  1662,  1663,     0,     0,   633,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   339,     0,     0,     0,     0,  1209,     0,     0,
       0,   633,     0,     0,     0,   247,     0,     0,     0,     0,
       0,     0,  1501,     0,     0,     0,  1502,     0,     0,     0,
       0,   436,   339,   123,     0,   247,     0,     0,     0,     0,
     247,     0,   247,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1537,     0,   123,     0,
       0,     0,   247,   502,   247,   247,     0,     0,     0,     0,
     502,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   247,     0,  1746,  1747,     0,     0,     0,     0,
       0,     0,     0,     0,   247,  1754,   123,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1601,     0,     0,  1604,
       0,     0,     0,     0,     0,     0,     0,   247,     0,   622,
     273,     0,     0,     0,     0,  1613,     0,     0,     0,     0,
    1777,  1778,     0,     0,     0,     0,     0,     0,   502,     0,
       0,   247,   622,     0,     0,     0,     0,     0,   247,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   339,   608,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1644,     0,     0,     0,
       0,   247,   268,   629,     0,  1649,     0,     0,     0,  1650,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1654,  1655,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1841,     0,  1358,  1360,  1362,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1850,     0,     0,  1851,  1852,     0,
       0,   200,     0,     0,  1854,     0,     0,  1381,     0,     0,
       0,     0,     0,   502,     0,     0,     0,   255,     0,     0,
       0,     0,  1114,     0,     0,     0,     0,     0,     0,   502,
     754,     0,   502,   757,     0,     0,     0,     0,     0,     0,
     339,     0,     0,     0,   608,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     633,     0,     0,     0,     0,     0,   200,     0,     0,     0,
     303,     0,     0,     0,     0,     0,     0,     0,     0,  1742,
    1743,   344,     0,     0,     0,   502,     0,     0,     0,   502,
       0,     0,     0,     0,     0,     0,     0,     0,   200,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   456,     0,     0,   460,     0,     0,     0,     0,     0,
       0,   339,     0,     0,     0,     0,     0,     0,  1957,     0,
       0,     0,     0,     0,     0,   247,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   247,     0,     0,     0,
       0,     0,     0,     0,     0,   193,     0,     0,     0,     0,
       0,     0,     0,     0,   200,     0,     0,     0,   247,     0,
       0,   502,     0,     0,   339,     0,     0,   255,     0,   247,
       0,     0,     0,     0,     0,     0,  1994,     0,   247,     0,
       0,   163,   884,   339,     0,  1536,     0,     0,  1538,     0,
       0,     0,     0,   608,  1828,     0,     0,   608,     0,     0,
    2011,     0,     0,   460,   902,     0,   339,     0,     0,     0,
     193,   200,     0,  2019,     0,     0,     0,     0,  1604,     0,
       0,     0,     0,     0,     0,   193,     0,     0,  2032,     0,
       0,   601,     0,   618,     0,     0,  1853,     0,     0,     0,
       0,     0,   193,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   275,     0,   443,     0,     0,     0,     0,
       0,     0,     0,  1871,     0,     0,   281,     0,   282,     0,
       0,     0,     0,     0,     0,   247,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   677,     0,     0,  1896,     0,
       0,  1897,     0,     0,     0,     0,     0,     0,     0,   247,
       0,     0,     0,     0,     0,     0,     0,     0,   193,     0,
     339,     0,     0,     0,     0,     0,     0,     0,     0,   200,
       0,     0,     0,     0,     0,     0,   502,   502,     0,     0,
       0,     0,     0,     0,     0,     0,   502,  1012,     0,   502,
    1015,     0,     0,     0,     0,     0,     0,     0,     0,   601,
       0,   339,     0,     0,   608,   778,   608,   608,     0,     0,
     503,   504,     0,   608,   508,   193,     0,   511,   512,     0,
       0,     0,     0,   339,   339,     0,   673,     0,     0,  1693,
       0,     0,     0,     0,     0,   193,     0,  1976,     0,     0,
       0,     0,   339,     0,     0,     0,   502,     0,     0,     0,
     502,     0,     0,     0,   502,  1083,     0,     0,   502,  1087,
       0,     0,     0,     0,     0,     0,  1090,     0,     0,     0,
       0,     0,     0,     0,   200,   200,     0,     0,     0,     0,
     456,     0,     0,     0,     0,   247,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   590,   591,     0,     0,     0,   339,   502,
       0,     0,     0,     0,     0,     0,   633,     0,   247,   623,
       0,     0,     0,   193,   247,     0,     0,     0,     0,     0,
       0,     0,     0,   344,     0,     0,     0,   608,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   456,   193,   888,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   833,   835,     0,     0,     0,
       0,     0,     0,     0,     0,   601,   339,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   200,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   677,
       0,   677,   677,   752,   677,     0,     0,     0,     0,     0,
     677,     0,     0,   677,   677,   677,     0,     0,   193,   193,
       0,     0,     0,     0,   443,     0,     0,  1839,     0,     0,
       0,     0,     0,   502,     0,     0,   633,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   247,
     608,   608,     0,   412,     0,     0,     0,   608,     0,     0,
       0,     0,     0,     0,     0,     0,   441,     0,     0,   456,
     823,     0,     0,     0,     0,     0,     0,   193,     0,   469,
       0,   469,     0,     0,     0,     0,     0,     0,     0,     0,
     673,     0,     0,   200,     0,   673,   443,   247,     0,   339,
       0,   673,     0,     0,   502,  1311,     0,   502,  1315,     0,
     456,     0,     0,     0,     0,     0,     0,     0,     0,   193,
     673,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   456,   456,     0,     0,     0,     0,     0,     0,
     193,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   456,     0,     0,     0,     0,   991,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   569,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   900,   901,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   908,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   456,     0,     0,
       0,     0,   339,   443,   200,     0,     0,     0,   608,  1414,
       0,     0,     0,     0,     0,   778,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   193,     0,     0,
     339,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   443,     0,     0,   247,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   344,   443,   443,     0,     0,
       0,     0,     0,     0,   502,  1465,     0,     0,     0,     0,
       0,     0,     0,   502,  1474,   443,   608,     0,     0,     0,
       0,     0,     0,     0,  1005,  1006,     0,   339,   339,     0,
    1010,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     469,  1031,     0,     0,  1034,  1035,   469,  1038,     0,  1040,
    1041,   799,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   443,     0,     0,     0,     0,     0,     0,   193,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1081,     0,
     361,     0,  1085,     0,   362,     0,   363,     0,     0,   247,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   364,     0,     0,   247,     0,   456,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   339,     0,     0,   867,   193,
     365,   366,     0,   367,     0,   368,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   369,   370,   358,   677,   371,
     372,   373,     0,   374,   375,     0,     0,   441,     0,  1203,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
     896,     0,     0,     0,     0,  1557,  1558,  1559,     0,     0,
       0,   376,  1718,     0,    75,   377,     0,   247,     0,     0,
       0,   378,    77,    78,   379,   380,   381,   382,     0,     0,
       0,     0,   255,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   930,     0,   200,     0,     0,     0,     0,     0,     0,
       0,   601,     0,     0,     0,   502,     0,     0,     0,     0,
       0,     0,   799,   950,     0,     0,   952,     0,   954,     0,
       0,   502,     0,     0,   963,     0,   968,   963,     0,   344,
       0,     0,   443,   677,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   996,     0,     0,     0,     0,     0,
    1203,     0,     0,     0,     0,     0,     0,   998,     0,     0,
       0,     0,   247,     0,     0,     0,     0,     0,  1007,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   339,   441,     0,     0,   996,   456,   456,     0,     0,
       0,  1302,     0,     0,     0,     0,     0,     0,  1309,     0,
       0,  1313,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1060,     0,     0,   469,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   677,   677,   677,     0,   677,
     677,     0,     0,     0,   339,   339,   460,   193,     0,     0,
       0,     0,     0,     0,     0,   193,     0,     0,     0,   502,
     502,  1091,     0,     0,     0,     0,     0,     0,     0,   247,
       0,     0,     0,     0,     0,   502,     0,     0,     0,     0,
       0,     0,     0,   193,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   255,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   412,
       0,     0,     0,     0,   344,     0,     0,     0,     0,     0,
    1193,  1195,     0,     0,     0,     0,     0,     0,   441,     0,
       0,     0,  1412,     0,     0,     0,     0,     0,     0,     0,
    1421,  1422,     0,     0,     0,     0,     0,     0,     0,     0,
     443,   443,  1546,     0,     0,  1549,  1563,   963,     0,     0,
       0,  1570,     0,     0,     0,  1574,     0,  1576,     0,     0,
     996,     0,     0,     0,     0,   502,     0,     0,  1233,     0,
       0,     0,     0,   502,     0,   963,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1463,     0,
       0,     0,     0,     0,     0,     0,     0,  1472,     0,     0,
    1476,     0,  1479,  1480,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     200,   469,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   339,     0,     0,     0,   502,  1926,     0,
       0,   502,     0,  1506,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   255,     0,     0,     0,   193,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   502,
       0,     0,     0,     0,     0,     0,     0,     0,   469,     0,
    1301,     0,  1304,     0,     0,     0,     0,  1664,     0,     0,
     344,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1598,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   677,  1697,
       0,     0,     0,     0,     0,     0,     0,     0,   502,   502,
       0,  1703,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   456,   456,     0,  1717,  1719,  1369,  1369,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   193,     0,     0,   502,     0,     0,
    1549,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1476,     0,     0,     0,     0,     0,
       0,     0,     0,   255,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1409,     0,     0,  1659,     0,     0,  1419,     0,     0,     0,
       0,     0,     0,     0,     0,   469,     0,     0,     0,     0,
       0,     0,     0,     0,   441,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   193,     0,     0,     0,     0,     0,
       0,   469,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   963,     0,     0,   799,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1804,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1807,     0,     0,  1812,  1816,     0,  1563,   443,   443,     0,
       0,  1822,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1740,     0,     0,   677,     0,     0,
       0,  1500,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   456,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   963,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   469,     0,     0,
     799,     0,     0,     0,     0,     0,   677,     0,     0,   460,
    1789,  1790,     0,     0,  1886,     0,     0,     0,     0,  1890,
    1892,     0,  1794,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   950,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1614,
    1615,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   469,     0,   799,     0,  1941,
       0,     0,  1942,  1944,     0,     0,     0,  1947,  1949,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   443,     0,     0,     0,
       0,  1861,     0,     0,  2033,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1353,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1988,  1990,  1992,     0,   412,     0,     0,     0,     0,
    1686,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   361,  2005,     0,     0,   362,     0,   363,     0,     0,
       0,     0,     0,     0,  2014,  2016,  2018,     0,     0,     0,
       0,  1924,  1116,     0,   364,    -2,     0,  1118,  -238,  -238,
    1119,  1120,  1121,  1122,  1123,  1124,  1125,  1126,  1127,  1128,
    1129,  1130,  -325,  1131,  1132,  1133,  1134,  1135,     0,  1136,
    1731,   365,   366,     0,   463,     0,   368,  1137,  1138,    64,
      65,    66,    67,    68,    69,    70,   369,   370,   358,  1139,
     371,   372,   373,  2033,   374,   375,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,  1353,
       0,  1755,     0,     0,  1757,     0,     0,     0,     0,     0,
       0,  -238,   376,     0,     0,    75,   377,     0,     0,     0,
     279,     0,   378,    77,    78,   379,   380,   381,   382,     0,
     361,     0,     0,     0,   362,     0,   363,  -179,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1116,     0,   364,    -2,     0,  1118,  -239,  -239,  1119,
    1120,  1121,  1122,  1123,  1124,  1125,  1126,  1127,  1128,  1129,
    1130,  -325,  1131,  1132,  1133,  1134,  1135,     0,  1136,     0,
     365,   366,     0,   463,     0,   368,  1137,  1138,    64,    65,
      66,    67,    68,    69,    70,   369,   370,   358,  1139,   371,
     372,   373,     0,   374,   375,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,  1738,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -239,   376,  1353,     0,    75,   377,     0,     0,     0,   279,
       0,   378,    77,    78,   379,   380,   381,   382,     0,     0,
       0,     0,     0,     0,     0,     0,  -179,     0,     0,     0,
       0,     0,     0,   361,     0,     0,     0,   362,     0,   363,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1116,     0,   364,    -2,     0,  1118,
       0,     0,  1119,  1120,  1121,  1122,  1123,  1124,  1125,  1126,
    1127,  1128,  1129,  1130,  -325,  1131,  1132,  1133,  1134,  1135,
     963,  1136,     0,   365,   366,     0,   463,     0,   368,  1137,
    1138,    64,    65,    66,    67,    68,    69,    70,   369,   370,
     358,  1139,   371,   372,   373,     0,   374,   375,     0,     0,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   376,     0,     0,    75,   377,     0,
       0,     0,   279,     0,   378,    77,    78,   379,   380,   381,
     382,     0,     0,     0,     0,     0,     0,     0,     0,  -179,
       4,   243,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,  1115,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,     0,     0,   361,     0,    45,    46,   362,
       0,   363,    47,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,    56,     0,  1116,    57,  1117,    -2,
       0,  1118,     0,     0,  1119,  1120,  1121,  1122,  1123,  1124,
    1125,  1126,  1127,  1128,  1129,  1130,  -325,  1131,  1132,  1133,
    1134,  1135,     0,  1136,     0,   365,   366,    60,   463,     0,
     368,  1137,  1138,    64,    65,    66,    67,    68,    69,    70,
     369,   370,   358,  1139,   371,   372,   373,     0,   374,   375,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    -3,   376,     0,     0,    75,
     408,     0,     0,     0,   279,     0,   378,    77,    78,   379,
     380,   381,   382,     0,     0,     0,     0,     0,     0,     0,
       0,  -179,     4,   243,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,  1115,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,     0,     0,   361,     0,    45,
      46,   362,     0,   363,    47,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,    56,     0,  1116,    57,
    1117,    -2,     0,  1118,     0,     0,  1119,  1120,  1121,  1122,
    1123,  1124,  1125,  1126,  1127,  1128,  1129,  1130,  -325,  1131,
    1132,  1133,  1134,  1135,     0,  1136,     0,   365,   366,    60,
     463,     0,   368,  1137,  1138,    64,    65,    66,    67,    68,
      69,    70,   369,   370,   358,  1139,   371,   372,   373,     0,
     374,   375,     0,     0,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   376,     0,
       0,    75,   408,     0,     0,     0,   279,     0,   378,    77,
      78,   379,   380,   381,   382,     0,     0,     0,     0,     0,
       0,     0,     0,  -179,     4,   243,     6,     7,     8,     9,
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
       0,     0,     0,     0,  1557,  1558,  1559,     0,     0,     0,
     376,  1560,  1561,    75,   408,     0,     0,     0,     0,     0,
     378,    77,    78,   379,   380,   381,   382,     0,     0,     0,
       0,     0,     0,     0,     0,  1562,     4,   243,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,     0,
       0,   361,     0,    45,    46,   362,     0,   363,    47,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
      56,     0,     0,    57,   364,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   365,   366,    60,   367,     0,   368,    62,    63,    64,
      65,    66,    67,    68,    69,    70,   369,   370,   358,     0,
     371,   372,   373,     0,   374,   375,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1557,  1558,  1559,     0,
       0,     0,   376,  1560,     0,    75,   408,     0,     0,     0,
       0,     0,   378,    77,    78,   379,   380,   381,   382,     0,
       0,     0,     0,     0,     0,     0,     0,  1562,     4,   243,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,     0,     0,   361,     0,    45,    46,   362,     0,   363,
      47,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,    56,     0,     0,    57,   364,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   365,   366,    60,   367,     0,   368,    62,
      63,    64,    65,    66,    67,    68,    69,    70,   369,   370,
     358,     0,   371,   372,   373,     0,   374,   375,     0,     0,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   376,     0,  1548,    75,   408,     0,
       0,     0,     0,     0,   378,    77,    78,   379,   380,   381,
     382,     4,   243,     6,     7,     8,     9,    10,    11,    12,
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
       0,     0,     0,     0,     0,     0,     0,   376,     0,     0,
      75,   408,     0,     0,     0,     0,     0,   378,    77,    78,
     379,   380,   381,   382,   243,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     0,    19,
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
       0,     0,    75,   438,     0,     0,     0,     0,     0,   378,
     439,    78,   379,   380,   381,   382,   243,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,     0,     0,
     361,     0,    45,    46,   362,     0,   363,   319,    48,    49,
      50,    51,    52,    53,    54,     0,     0,     0,     0,     0,
       0,     0,    57,   364,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     365,   366,     0,   367,     0,   368,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   369,   370,   358,     0,   371,
     372,   373,     0,   374,   375,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   376,     0,     0,    75,  1190,     0,     0,     0,     0,
       0,   378,  1191,    78,   379,   380,   381,   382,   243,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
       0,     0,   361,     0,    45,    46,   362,     0,   363,   319,
      48,    49,    50,    51,    52,    53,    54,     0,     0,     0,
       0,     0,     0,     0,    57,   364,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   365,   366,     0,   367,     0,   368,    62,    63,
      64,    65,    66,    67,    68,    69,    70,   369,   370,   358,
       0,   371,   372,   373,     0,   374,   375,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   376,     0,     0,    75,   377,     0,     0,
       0,     0,     0,   378,    77,    78,   379,   380,   381,   382,
     243,     6,     7,     8,     9,    10,    11,    12,    13,    14,
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
     381,   382,  1870,     0,    -2,    -2,    -2,    -2,    -2,    -2,
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
       0,    -2,    -2,  1895,     0,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
       0,    -2,     0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
       0,    -2,    -2,    -2,     0,    -2,     0,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,     0,     0,     0,    -2,
       0,     0,    -2,  1201,     0,     0,     0,    -2,    -2,     0,
      13,    14,    15,    16,    17,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    -2,     0,     0,     0,    -2,    -2,     0,     0,
       0,     0,     0,     0,     0,     0,   361,     0,     0,     0,
     362,     0,   363,     0,     0,     0,     0,     0,     0,     0,
       0,    -2,     0,     0,     0,     0,     0,     0,    57,   364,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1416,
      -2,     0,     0,     0,    -2,    -2,    13,    14,    15,    16,
      17,     0,    -2,    -2,     0,     0,   365,   366,     0,   367,
       0,   368,    62,    63,    64,    65,    66,    67,    68,    69,
      70,   369,   370,   358,     0,   371,   372,   373,     0,   374,
     375,     0,   361,     0,     0,     0,   362,    72,   363,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    57,   364,     0,   376,     0,     0,
      75,   377,     0,     0,     0,     0,     0,   378,   439,    78,
     379,   380,   381,   382,     0,     0,     0,     0,     0,     0,
       0,     0,   365,   366,     0,   367,     0,   368,    62,    63,
      64,    65,    66,    67,    68,    69,    70,   369,   370,   358,
       0,   371,   372,   373,     0,   374,   375,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   376,     0,     0,    75,   377,     0,     0,
       0,     0,     0,   378,  1417,    78,   379,   380,   381,   382,
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
      43,    44,  -446,  -446,     0,  -446,    45,    46,     0,  -446,
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
       0,     0,     0,  -757,     0,     0,    77,    78,     4,   243,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,     0,     0,     0,     0,    45,    46,     0,     0,     0,
      47,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,    56,     0,     0,    57,     0,     0,     0,     0,
    -378,  -378,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    60,     0,     0,     0,    62,
      63,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -378,     0,     0,     0,    75,    76,     0,
       0,     0,     0,     0,     0,    77,    78,     4,   243,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
       0,     0,     0,     0,    45,    46,     0,     0,     0,    47,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,    56,     0,     0,    57,     0,     0,     0,     0,  -379,
    -379,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    60,     0,     0,     0,    62,    63,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -379,     0,     0,     0,    75,    76,     0,     0,
       0,     0,     0,     0,    77,    78,   242,   243,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
       0,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,  -446,
    -446,     0,  -446,    45,    46,     0,  -446,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    57,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    61,     0,     0,    62,    63,    64,
      65,    66,    67,    68,    69,    70,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    73,    74,     0,    75,   244,     0,     0,  1329,
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
       0,     0,     0,     0,  1332,     0,     0,     0,    75,   926,
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
       0,     0,     0,     0,     0,     0,     0,  1512,     0,     0,
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
    1513,     0,     0,     0,    75,   926,     0,     0,  1329,     0,
       0,     0,    77,    78,  1330,     0,     0,    13,    14,    15,
      16,    17,    18,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,     0,     0,     0,     0,    45,    46,     0,     0,     0,
      47,    48,    49,    50,    51,    52,    53,    54,     0,     0,
       0,     0,     0,     0,     0,    57,  1331,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    60,     0,     0,     0,    62,
      63,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1514,     0,     0,     0,    75,   926,     0,
       0,     0,     0,     0,     0,    77,    78,   242,   243,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,     0,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
    -446,  -446,     0,  -446,    45,    46,     0,  -446,     0,     0,
       0,     0,     0,     0,     0,     0,    13,    14,    15,    16,
      17,     0,     0,    19,    57,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
       0,     0,     0,     0,    45,    46,     0,     0,    62,    63,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    57,     0,     0,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   658,    75,   244,     0,     0,
       0,     0,     0,     0,    77,    78,   243,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,     0,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,  -446,  -446,
     -16,  -446,    45,    46,     0,  -446,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    57,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    61,     0,     0,    62,    63,    64,    65,
      66,    67,    68,    69,    70,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    73,    74,     0,    75,   244,     0,     0,     0,  -761,
       0,     0,    77,    78,   243,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,     0,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,  -446,  -446,     0,  -446,
      45,    46,     0,  -446,     0,     0,     0,     0,     0,     0,
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
       0,     0,  1116,     0,   364,     0,     0,  1118,  1797,  1798,
    1119,  1120,  1121,  1122,  1123,  1124,  1125,  1126,  1127,  1128,
    1129,  1130,  -325,  1131,  1132,  1133,  1134,  1135,     0,  1136,
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
    -325,  1131,  1132,  1133,  1134,  1135,     0,  1136,     0,   365,
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
       0,     0,     0,  -629,    75,   321,     0,     0,    62,    63,
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
       0,     0,     0,     0,     0,     0,     0,    72,     0,  1773,
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
       0,     0,     0,     0,     0,    72,     0,  1775,     0,     0,
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
      39,    40,    41,    42,    43,    44,  -446,  -446,     0,  -446,
      45,    46,     0,  -446,     0,     0,     0,     0,     0,     0,
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
       0,  -759,     0,     0,    77,    78,    13,    14,    15,    16,
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
      42,    43,    44,  -446,  -446,     0,  -446,    45,    46,     0,
    -446,     0,     0,     0,     0,     0,     0,     0,     0,     0,
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
      44,  -446,  -446,     0,  -446,    45,    46,     0,  -446,     0,
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
    -446,  -446,     0,  -446,    45,    46,     0,  -446,     0,     0,
       0,     0,     0,     0,     0,     0,    13,    14,    15,    16,
      17,     0,     0,    19,    57,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
    -446,  -446,     0,  -446,    45,    46,     0,  -446,    62,    63,
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
       0,     0,     0,  -642,    75,   243,     6,     7,     8,     9,
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
      39,    40,    41,    42,    43,    44,  -446,  -446,     0,  -446,
      45,    46,     0,  -446,     0,     0,     0,     0,     0,     0,
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
      37,    38,    39,    40,    41,    42,    43,    44,  -446,  -446,
      75,  -446,    45,    46,     0,  -446,     0,     0,     0,     0,
     361,     0,     0,     0,   362,     0,   363,     0,     0,     0,
       0,     0,    57,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   364,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    62,    63,     0,     0,
     365,   366,     0,   367,     0,   368,  1810,    63,    64,    65,
      66,    67,    68,    69,    70,   369,   370,   358,     0,   371,
     372,   373,     0,   374,   375,     0,   361,     0,     0,     0,
     362,    72,   363,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    75,  1557,  1558,  1559,     0,   364,
       0,   376,  1811,     0,    75,   377,     0,     0,     0,     0,
       0,   378,    77,    78,   379,   380,   381,   382,     0,     0,
       0,     0,     0,     0,     0,     0,   365,   366,     0,   367,
       0,   368,    62,    63,    64,    65,    66,    67,    68,    69,
      70,   369,   370,   358,     0,   371,   372,   373,   361,   374,
     375,     0,   362,     0,   363,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   364,     0,     0,     0,     0,     0,   376,  1236,     0,
      75,   377,     0,     0,     0,  1237,     0,   378,    77,    78,
     379,   380,   381,   382,     0,     0,     0,     0,   365,   366,
       0,   367,     0,   368,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   369,   370,   358,     0,   371,   372,   373,
     361,   374,   375,     0,   362,     0,   363,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   364,     0,     0,     0,     0,     0,   376,
     959,  1539,    75,   377,     0,     0,     0,     0,     0,   378,
      77,    78,   379,   380,   381,   382,     0,     0,     0,     0,
     365,   366,     0,   367,     0,   368,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   369,   370,   358,     0,   371,
     372,   373,   361,   374,   375,     0,   362,     0,   363,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   364,     0,     0,     0,     0,
       0,   376,     0,     0,    75,   377,     0,     0,     0,   466,
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
       0,   364,     0,     0,     0,     0,     0,   376,     0,  1803,
      75,   377,     0,     0,     0,     0,     0,   378,    77,    78,
     379,   380,   381,   382,     0,     0,     0,     0,   365,   366,
       0,   367,     0,   368,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   369,   370,   358,     0,   371,   372,   373,
     361,   374,   375,     0,   362,     0,   363,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   364,     0,     0,     0,     0,     0,   376,
    1815,     0,    75,   377,     0,     0,     0,     0,     0,   378,
      77,    78,   379,   380,   381,   382,     0,     0,     0,     0,
     365,   366,     0,   367,     0,   368,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   369,   370,   358,     0,   371,
     372,   373,   361,   374,   375,     0,   362,     0,   363,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   364,     0,     0,     0,     0,
       0,   376,  1891,     0,    75,   377,     0,     0,     0,     0,
       0,   378,    77,    78,   379,   380,   381,   382,     0,     0,
       0,     0,   365,   366,     0,   367,     0,   368,    62,    63,
      64,    65,    66,    67,    68,    69,    70,   369,   370,   358,
       0,   371,   372,   373,   361,   374,   375,     0,   362,     0,
     363,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   364,     0,     0,
       0,     0,     0,   376,  1940,     0,    75,   377,     0,     0,
       0,     0,     0,   378,    77,    78,   379,   380,   381,   382,
       0,     0,     0,     0,   365,   366,     0,   367,     0,   368,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   369,
     370,   358,     0,   371,   372,   373,   361,   374,   375,     0,
     362,     0,   363,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   364,
       0,     0,     0,     0,     0,   376,  1943,     0,    75,   377,
       0,     0,     0,     0,     0,   378,    77,    78,   379,   380,
     381,   382,     0,     0,     0,     0,   365,   366,     0,   367,
       0,   368,    62,    63,    64,    65,    66,    67,    68,    69,
      70,   369,   370,   358,     0,   371,   372,   373,   361,   374,
     375,     0,   362,     0,   363,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   364,     0,     0,     0,     0,     0,   376,  1946,     0,
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
       0,   376,  1987,     0,    75,   377,     0,     0,     0,     0,
       0,   378,    77,    78,   379,   380,   381,   382,     0,     0,
       0,     0,   365,   366,     0,   367,     0,   368,    62,    63,
      64,    65,    66,    67,    68,    69,    70,   369,   370,   358,
       0,   371,   372,   373,   361,   374,   375,     0,   362,     0,
     363,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   364,     0,     0,
       0,     0,     0,   376,  1989,     0,    75,   377,     0,     0,
       0,     0,     0,   378,    77,    78,   379,   380,   381,   382,
       0,     0,     0,     0,   365,   366,     0,   367,     0,   368,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   369,
     370,   358,     0,   371,   372,   373,   361,   374,   375,     0,
     362,     0,   363,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   364,
       0,     0,     0,     0,     0,   376,  1991,     0,    75,   377,
       0,     0,     0,     0,     0,   378,    77,    78,   379,   380,
     381,   382,     0,     0,     0,     0,   365,   366,     0,   367,
       0,   368,    62,    63,    64,    65,    66,    67,    68,    69,
      70,   369,   370,   358,     0,   371,   372,   373,   361,   374,
     375,     0,   362,     0,   363,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   364,     0,     0,     0,     0,     0,   376,  2013,     0,
      75,   377,     0,     0,     0,     0,     0,   378,    77,    78,
     379,   380,   381,   382,     0,     0,     0,     0,   365,   366,
       0,   367,     0,   368,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   369,   370,   358,     0,   371,   372,   373,
     361,   374,   375,     0,   362,     0,   363,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   364,     0,     0,     0,     0,     0,   376,
    2015,     0,    75,   377,     0,     0,     0,     0,     0,   378,
      77,    78,   379,   380,   381,   382,     0,     0,     0,     0,
     365,   366,     0,   367,     0,   368,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   369,   370,   358,     0,   371,
     372,   373,   361,   374,   375,     0,   362,     0,   363,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   364,     0,     0,     0,     0,
       0,   376,  2017,     0,    75,   377,     0,     0,     0,     0,
       0,   378,    77,    78,   379,   380,   381,   382,     0,     0,
       0,     0,   365,   366,     0,   367,     0,   368,    62,    63,
      64,    65,    66,    67,    68,    69,    70,   369,   370,   358,
       0,   371,   372,   373,   361,   374,   375,     0,   362,     0,
     363,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   364,     0,     0,
       0,     0,     0,   376,     0,     0,    75,   377,     0,     0,
       0,     0,     0,   378,    77,    78,   379,   380,   381,   382,
       0,     0,     0,     0,   365,   366,     0,   367,     0,   368,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   369,
     370,   358,     0,   371,   372,   373,   361,   374,   375,     0,
     362,     0,   363,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   364,
       0,     0,     0,     0,     0,   650,     0,     0,    75,   377,
       0,     0,     0,     0,     0,   378,    77,    78,   379,   380,
     381,   382,     0,     0,     0,     0,   365,   366,     0,   367,
       0,   368,    62,    63,    64,    65,    66,    67,    68,    69,
      70,   369,   370,   358,     0,   371,   372,   373,   361,   374,
     375,     0,   362,     0,   363,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   364,     0,     0,     0,     0,     0,   655,     0,     0,
      75,   377,     0,     0,     0,     0,     0,   378,    77,    78,
     379,   380,   381,   382,     0,     0,     0,     0,   365,   366,
       0,   367,     0,   368,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   369,   370,   358,     0,   371,   372,   373,
     361,   374,   375,     0,   362,     0,   363,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   364,     0,     0,     0,     0,     0,   661,
       0,     0,    75,   377,     0,     0,     0,     0,     0,   378,
      77,    78,   379,   380,   381,   382,     0,     0,     0,     0,
     365,   366,     0,   367,     0,   368,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   369,   370,   358,     0,   371,
     372,   373,   361,   374,   375,     0,   362,     0,   363,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   364,     0,     0,     0,     0,
       0,   376,     0,     0,    75,   377,     0,     0,     0,     0,
       0,   378,   866,    78,   379,   380,   381,   382,     0,     0,
       0,     0,   365,   366,     0,   367,     0,   368,    62,    63,
      64,    65,    66,    67,    68,    69,    70,   369,   370,   358,
       0,   371,   372,   373,   361,   374,   375,     0,   362,     0,
     363,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   364,     0,     0,
       0,     0,     0,   376,     0,     0,    75,   377,     0,     0,
       0,     0,     0,   378,   439,    78,   379,   380,   381,   382,
       0,     0,     0,     0,   365,   366,     0,   367,     0,   368,
    1885,    63,    64,    65,    66,    67,    68,    69,    70,   369,
     370,   358,     0,   371,   372,   373,     0,   374,   375,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   376,     0,     0,    75,   377,
       0,     0,     0,     0,     0,   378,    77,    78,   379,   380,
     381,   382,    13,    14,    15,    16,    17,     0,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,  -445,  -445,     0,  -445,
      45,    46,     0,  -445,     0,     0,     0,     0,     0,     0,
       0,     0,    13,    14,    15,    16,    17,     0,     0,    19,
      57,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,  -446,  -446,     0,  -446,
      45,    46,     0,  -446,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      57
};

static const yytype_int16 yycheck[] =
{
       1,    73,   241,     4,    73,    73,    73,   178,   256,   192,
     220,   178,     4,   376,   219,   466,   150,   162,  1801,   219,
     162,   283,    82,   219,   222,   173,   875,   678,     1,    58,
      73,    95,   324,   616,   207,   693,   603,   163,  1125,   162,
     603,   607,   219,   219,  1669,   603,   139,   515,   516,   965,
    1737,  1670,   322,  1669,    55,    56,  1669,    58,  1287,  1288,
     219,  1355,  1797,    75,     1,    73,    73,     4,   671,   339,
     219,     1,    73,   343,    73,   737,  1209,    73,   994,     1,
      70,    82,   767,   632,  1567,    58,   296,   147,   773,    90,
     295,     0,   290,   291,    95,   295,   609,    98,   191,   295,
      70,   102,   236,   766,    82,   861,   766,   526,   228,    82,
     102,   231,   181,   181,   181,   764,   145,   536,   295,   295,
     157,    58,    95,    87,   764,    98,   635,     0,    58,   102,
     339,   764,   229,   253,   343,   367,   295,   174,   181,   140,
     764,  1057,   143,   263,   145,     1,   295,   219,     4,   150,
     219,   219,   219,   115,   446,   156,   220,   254,   577,   149,
     376,    98,   163,   153,   316,   102,  1785,   264,    87,   637,
     440,   149,   145,   181,   181,   174,   219,    70,   174,   180,
     181,  1109,     1,   153,   861,   245,     1,  1355,  1356,     4,
     845,   177,    96,   157,   195,  1930,  1979,   764,    73,   322,
     766,   764,    58,   355,   205,   120,   764,   180,   145,   210,
     115,   219,   219,    88,     1,   145,     1,   872,   219,   220,
       1,    70,   195,   295,   130,     1,   295,   295,   295,  2012,
    1713,   463,   296,   155,   156,   236,   155,   152,   157,    58,
     789,    95,   302,    58,   245,   149,   102,   220,   153,   131,
     498,    59,    60,     4,   255,   768,   149,   258,   353,   772,
     153,   356,   274,   148,   265,   923,   172,    82,   781,   782,
     155,    58,   245,    58,   275,   276,   744,   278,  1245,   484,
    1899,  1099,    58,   493,   484,   258,  1104,     1,   484,   145,
    1423,   152,  1109,   951,   295,   296,  1590,   783,   784,   472,
     149,   563,   303,   595,    55,    56,   153,   484,   484,   310,
     311,   158,   173,    98,   315,   102,   802,   440,   444,  2006,
     174,   258,   592,   296,   139,   484,   145,   619,   691,   211,
     145,   897,   147,  1958,   626,   484,  1021,   940,   521,    90,
    1959,  1003,  1958,   436,    58,  1958,   616,   348,  1109,   157,
     410,     0,   353,   283,    87,   356,   357,  1175,   145,   629,
     145,  1117,   572,    87,  1027,   570,   174,  1027,   917,   145,
     570,  1020,   151,   882,   570,   156,   191,   156,  1997,   513,
    1020,   149,   868,   592,   155,   519,   506,  1020,   784,   140,
    1146,    82,   143,   570,   570,    70,  1020,   149,  1326,  1327,
    1328,   131,   284,   174,   149,   156,   802,   916,   528,   151,
     593,   570,   163,   155,   534,   484,   484,   484,   538,   420,
     629,   570,  1590,   842,   157,  1654,  1655,   242,   173,   493,
     245,   145,   574,   157,   650,   165,   652,   653,   535,   655,
    1117,   484,   443,   444,    19,   661,  1101,   420,   664,   665,
     666,   574,   464,  1020,   455,   456,   147,  1020,   154,   210,
    1563,  1027,  1020,   464,   283,   466,    70,   149,   283,  1146,
     149,   155,   868,   258,   149,  1562,   484,   484,   153,   149,
    1567,    75,    76,   484,  1451,  1452,  1453,   302,   179,    70,
     174,    70,   493,   616,   376,  1933,   283,   149,    75,    76,
     155,   570,   570,   570,   255,  1323,   131,   283,   572,  1326,
    1327,  1328,   513,  1198,   265,   157,   364,  1955,   519,   174,
     493,   131,    73,   174,   275,   276,   155,   278,   149,    70,
    1446,  1044,   174,   104,   105,   156,   574,   720,    89,   324,
     165,   389,   390,  1981,   174,   149,   997,   576,  1716,   153,
     160,   161,   303,    70,   245,   106,   557,     3,   559,   310,
     311,   853,   410,   157,   315,  1326,  1327,  1328,   149,   570,
     149,   572,   153,    56,   153,   576,    59,    60,   761,    62,
     157,   152,   265,  1069,     3,   586,    70,   152,    70,   590,
      70,  1276,   440,   276,   886,   410,   155,   348,    70,   572,
     783,   784,   353,   576,  1255,   356,   777,   149,   149,  1192,
     777,   302,   153,   150,   884,  1718,  1719,   788,   791,   802,
     157,   436,   623,   505,  1853,   155,  1713,   155,   510,   173,
     151,   322,   149,   563,   635,   157,   153,   165,  1187,   576,
     766,   162,   163,   155,   174,   527,   576,    70,    12,    13,
      14,    15,    16,   157,    70,   537,    70,    70,   143,   144,
     145,   446,   174,   593,  1213,   149,   149,   149,   149,   153,
     804,   153,  1521,  1069,   149,   884,   157,   149,  1355,  1356,
     165,   153,   683,   146,   685,   868,   687,  1345,   157,   823,
     691,   149,   443,   694,   157,   230,  1205,    12,    13,    14,
      15,    16,    70,  1871,   455,   456,    70,   719,  1811,  1812,
     173,   171,   155,  1262,   149,  1366,  1184,   155,   719,   410,
     576,  1637,  1809,  1639,   844,   151,   149,   165,  1896,   155,
     153,   174,   718,   149,  1821,   149,   149,   153,   129,   153,
     153,   151,   557,   914,   563,   783,   784,   914,   563,   440,
     151,   125,   126,   155,   155,    70,   115,   576,   149,   155,
    1211,   576,   153,   764,   802,   766,  1934,   131,   650,   160,
     161,   151,   174,   655,   561,   560,   563,   778,   174,   661,
     155,   149,   908,   149,   785,   153,  1237,   563,   155,   576,
     791,   576,  1443,   794,   936,   169,   170,  1884,   680,   174,
     576,   155,   803,   804,   805,   155,   557,   174,  1056,   153,
     155,   165,   149,   936,   158,   165,   131,   155,   149,   990,
     165,   160,   823,   151,   515,   516,   157,   165,   167,   168,
     868,   761,   151,   151,   619,   586,    21,   156,   824,   590,
       4,     5,     6,     7,     8,     9,    10,    11,   151,   697,
       4,     5,     6,     7,     8,     9,    10,    11,   859,   860,
     861,   148,   576,   104,   105,   143,   144,   145,   155,   151,
     151,  1420,   623,   149,   156,   156,  1425,   155,   413,   152,
     153,   882,   874,   156,   635,    17,  1069,   165,   149,   151,
    1088,  1064,   153,  1442,   429,  1066,   174,   432,   936,    56,
    1026,  1027,    59,    60,  1196,    62,   149,   908,   151,    63,
     153,   912,  1363,  1590,    96,   916,    46,    47,   129,    49,
     155,   922,  1192,    53,   861,   616,   149,    59,    60,    61,
      62,   861,   683,   155,   685,    82,   687,   874,   149,   129,
     691,   151,   153,   694,   149,   155,   637,   829,    95,   160,
     161,    98,   155,   151,   489,   102,   957,   155,   840,   149,
     155,   843,  1201,   153,   965,   847,   155,  1330,   719,   101,
     160,   161,   149,     3,   106,   107,   108,   109,   110,   111,
     112,   113,    12,    13,    14,    15,    16,   151,   151,   157,
    1441,   151,   155,   994,   149,   155,   997,   149,   153,   151,
      12,    13,    14,    15,    16,   861,    87,   103,  1224,   157,
     174,   107,   157,   149,   110,   151,   112,   153,   874,  1020,
     149,   153,   151,  1572,   153,  1026,  1027,   778,   149,   148,
     151,  1069,   153,   180,   785,   104,   105,  1185,  1186,  1716,
      70,   151,   861,   151,  1178,   155,   861,   155,   195,  1109,
     154,   149,   803,   744,   805,   153,  1057,   151,    70,   874,
     157,    12,    13,    14,    15,    16,   151,   152,   853,  1192,
    1318,   129,   173,   220,   861,   157,   861,   149,  1220,   864,
     157,   153,  1631,   151,   932,   861,   151,  1258,   151,   937,
     155,   149,   155,  1355,   155,   153,   157,  1220,   245,   115,
     948,   886,   160,   161,  1287,  1288,   162,   163,   859,   860,
     861,   258,  1621,   123,   124,   173,   149,   129,  1119,    70,
     153,  1122,  1123,  1124,  1295,  1296,  1336,   149,  1295,  1296,
     149,   882,   108,   109,   110,   111,   112,   149,   151,   166,
     151,   153,   155,  1288,   155,  1146,   151,   861,   160,   161,
     155,  1152,   248,   698,   699,   700,   151,   908,     3,  1160,
     155,   912,  1163,  1164,   149,   916,  1167,    12,    13,    14,
      15,    16,  1164,   155,   151,    73,   153,  1178,  1836,   151,
    1117,   151,  1220,   155,   875,   155,   161,  1117,   151,   151,
    1163,  1164,   155,   155,  1871,   151,   151,    95,   159,   155,
     155,   736,   737,  1752,  1205,   171,   143,   144,   145,  1146,
    1211,   746,   129,   151,   749,   151,  1146,   155,   155,  1896,
    1221,  1103,  1512,  1513,  1514,    70,  1163,  1164,   165,   152,
     326,   327,   151,   329,  1116,   331,  1237,   174,   151,   151,
     151,   151,  1751,   155,  1245,   155,   151,   127,   128,  1287,
     155,  1133,   150,   153,  1963,   154,   155,  1934,  1967,   154,
     155,  1117,   151,  1405,   160,   161,  1326,  1327,  1328,  1117,
    1330,  1331,  1336,   420,   131,   810,  1277,   154,   155,   814,
     154,   155,  1405,   818,   118,   151,   120,   121,   122,   154,
    1146,   154,   155,   131,  1109,  1844,   154,   155,  1117,  1106,
    1107,  1108,  1117,   155,   156,   154,   155,   155,  1164,  1519,
     705,   706,   707,   708,   156,   149,   156,  1527,   152,   153,
    1525,   219,   220,   157,   158,  1525,   149,  1146,  1590,  1525,
    1117,  1146,  1117,   154,   155,  1336,   151,  1329,   236,  1340,
     151,  1117,  1343,   151,  1192,   151,   493,   154,  1525,  1525,
     154,   155,   154,   155,  1537,   154,   155,   154,   155,  1146,
     149,  1146,  1363,   154,   155,   151,  1525,  1405,  1119,   151,
    1146,  1122,  1123,  1124,   154,   155,  1525,  1164,  1163,  1227,
    1228,  1229,  1383,   153,  1385,   157,  1234,  1235,   154,   155,
     154,   155,  1329,   154,   155,  1146,    68,   295,   296,   154,
     155,  1152,   154,  1117,   154,   155,   155,   156,   149,  1160,
     157,  1196,  1385,  1629,   154,   155,  1167,    76,  1355,  1356,
     154,   155,    75,    76,   157,  1355,  1356,   155,   156,  1246,
    1247,   157,  1146,   701,   702,   157,   532,  1429,   703,   704,
    1441,   154,   709,   710,    17,  1446,  1532,  1533,  1385,   173,
    1451,  1452,  1453,  1525,  1205,  1519,  1601,  1185,  1186,  1601,
     155,   173,   149,  1527,  1346,  1347,   157,   174,  1003,   151,
    1221,  1654,  1655,  1329,   151,   174,   157,  1687,  1601,  1684,
     154,   157,    17,   154,  1684,   148,   151,   151,  1684,   151,
     151,   151,  1429,  1184,    12,    13,    14,    15,    16,  1355,
    1356,  1192,  1384,   151,   151,   151,   148,  1684,  1684,    68,
    1655,  1326,  1327,  1328,  1329,  1330,  1331,   157,  1519,   157,
     157,   151,   174,   173,  1525,  1684,  1527,    12,    13,    14,
      15,    16,   151,  1534,   151,  1684,  1355,  1356,   151,  1074,
    1355,  1356,  1077,   148,   151,   157,   444,  1548,   151,   155,
     151,   151,    70,   155,   151,   151,   151,   151,  1828,   151,
    1561,   151,   148,   154,  1556,   154,   151,   151,  1355,  1356,
    1355,  1356,   151,  1429,   151,   151,   151,   151,   149,  1355,
    1356,   151,   154,   151,  1794,    70,   484,   151,   151,  1340,
     173,   148,  1343,   151,   155,   493,   149,  1598,   149,   149,
    1385,   149,   149,  1589,    13,    72,    89,   156,  1900,  1743,
     155,   129,  1604,   156,  1429,   513,  1654,   154,  1555,  1556,
    1621,   519,   174,  1687,   154,   148,  1797,  1897,   174,   148,
    1797,   149,  1383,   174,   174,   153,  1637,   157,  1639,   155,
     174,   151,   160,   161,   129,   154,    63,    64,    65,    66,
     151,   155,   155,  1590,    12,    13,    14,    15,    16,    17,
    1590,   559,   151,   155,   149,   151,   154,  1604,   153,   151,
    1853,   148,   570,   148,   572,   160,   161,    12,    13,    14,
      15,    16,    17,  1684,   101,    78,  1687,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   149,  1698,   174,  1581,
    1556,  1702,    12,    13,    14,    15,    16,    17,  1853,   174,
     149,   174,   174,   174,  1715,   174,   174,   149,   148,   174,
       9,   149,   174,   148,  1725,   151,   148,   148,   155,  1264,
    1794,   155,   148,   151,  1590,   154,   153,   157,  1273,   154,
     154,  1742,  1743,   154,   151,   118,  1732,  1595,  1604,  1959,
    1751,   156,   156,  1958,   171,   151,     9,  1928,  1958,  1930,
     148,  1928,  1958,  1930,   151,  1899,   151,  1704,  1828,  1556,
    1555,  1590,   151,   154,   154,  1590,   151,   148,   156,  1716,
     174,  1958,  1958,  1534,   151,   155,  1716,  1997,   149,  1604,
     155,   151,   149,  1794,  1965,   149,   107,  1548,  1965,  1958,
    1801,   154,   154,  1590,  1805,  1590,   154,  1808,   148,  1958,
    1561,   157,   101,   151,  1590,   104,   105,   106,   107,   108,
     109,   110,   111,   112,  1995,   148,   148,   151,   154,    73,
    1521,   151,  1833,   101,   151,   151,   151,  1897,   106,   107,
     108,   109,   110,   111,   112,    73,  1832,  1598,   101,   174,
     148,   104,   105,   106,   107,   108,   109,   110,   111,   112,
    1716,   174,   149,   151,    88,   174,   764,   151,   766,   154,
    1621,  2042,   154,  1874,   148,  2042,   148,  1878,   151,    73,
     151,   151,     1,   152,   151,     4,  1958,   151,  1889,  1958,
    1958,  1958,   151,   153,   174,  1959,    73,  1716,  1899,   165,
    1901,  1716,   156,   151,   151,   148,   804,   165,   148,   174,
     151,  1912,   174,  1914,  1915,   151,   155,   148,   156,  1704,
     101,   155,   149,    73,   149,   823,   148,   107,   165,  1716,
     151,  1716,   174,  1997,  1871,   154,  1937,   107,   156,    58,
    1716,  1871,   174,   151,   148,   148,   151,  1698,   149,    73,
    1252,  1702,   151,   151,    73,   378,  1629,  1958,  1959,  1896,
     174,   174,   174,    82,  1715,   670,  1896,  1345,  1969,   711,
     713,   715,   409,   712,  1725,  1146,    95,  1963,  1979,    98,
    1515,  1967,  1968,   102,   714,  1135,  1959,  1835,  2012,  1590,
    1930,  1742,  1724,  1716,  1955,  1837,  1997,  1934,  2007,  1994,
    1751,  2006,  1582,  1897,  1934,  1968,  1582,  2027,  1896,  2010,
      48,  2012,  1519,  1999,  1167,  1871,  1163,  1164,  1861,  1794,
     139,   250,  1331,  1533,  1997,  1160,   145,   473,   147,  2030,
     586,   150,   151,   791,   878,  2036,   178,  2023,  1604,  1429,
    1896,  2027,     0,   162,   922,  2046,   736,   736,   736,  2050,
    1801,    -1,  1871,    -1,  1805,    -1,  1871,  1808,    -1,  2060,
     179,   180,   181,  2049,     4,     5,     6,     7,     8,     9,
      10,    11,   191,   192,    -1,    -1,   195,  1896,  1934,    -1,
      -1,  1896,  1833,    -1,  1871,    -1,  1871,    -1,    -1,   205,
      -1,    -1,    -1,    -1,    -1,  1871,    12,   101,    -1,    -1,
     219,   220,   106,   107,   108,   109,   110,   111,   112,  1896,
      -1,  1896,    -1,    -1,    -1,  1934,    -1,   236,    -1,  1934,
    1896,    -1,  1020,  1874,    -1,    -1,   245,  1878,  1026,  1027,
      -1,    -1,    -1,    -1,  1669,  1670,    -1,    -1,  1889,   258,
    2022,    -1,    -1,    -1,    -1,   149,   150,  1934,    -1,  1934,
    1901,    -1,    -1,    -1,    -1,    -1,  2038,    -1,  1934,    -1,
      -1,  1912,    -1,  1914,  1915,    -1,    -1,    -1,    -1,   288,
      86,    -1,    -1,    -1,    -1,   294,   295,   296,    -1,    -1,
      -1,    -1,    -1,   302,    -1,   101,  1937,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,    12,    13,    14,
      15,    16,    17,   322,   323,   324,   101,    -1,    -1,   104,
     105,   106,   107,   108,   109,   110,   111,   112,  1969,   361,
     339,    -1,   364,   365,   343,    -1,    -1,    -1,  1979,    -1,
       3,    -1,   374,   375,   101,    -1,    -1,    -1,  1385,   106,
     107,   108,   109,   110,   111,   112,   113,   389,   390,    -1,
    1785,    -1,    -1,    -1,    -1,    -1,    -1,   376,    -1,  2010,
      -1,  2012,    -1,    -1,    -1,   160,   552,   101,   410,    -1,
      -1,   192,   106,   107,   108,   109,   110,   111,   112,  2030,
    1178,    -1,    -1,    -1,    -1,  2036,   153,    -1,    -1,    -1,
      -1,   410,    -1,    -1,   413,  2046,    -1,    -1,   440,  2050,
      -1,   420,    17,    -1,    -1,    -1,    -1,    -1,    -1,  2060,
      -1,    -1,    -1,    -1,    -1,    -1,   150,   436,    -1,   153,
      -1,   440,  1857,    -1,    -1,   444,  1861,   446,   101,    -1,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      55,    56,    57,    58,    59,    60,    61,    62,   464,    -1,
     466,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1899,   484,   830,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   493,    -1,   149,   150,    -1,  1277,
      -1,    -1,    -1,   156,    -1,    -1,    -1,   160,   161,    -1,
      -1,    -1,    -1,    -1,   513,    -1,   515,   516,    -1,    -1,
     519,   322,   521,   101,   325,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,    98,    -1,    -1,   339,    -1,
      -1,    -1,   343,  1958,  1959,   889,   109,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1336,   101,
     559,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   570,    -1,   572,   152,   574,    -1,   576,    -1,   157,
      -1,    -1,  1997,    -1,    -1,    -1,    -1,    -1,   151,    -1,
      -1,    -1,    -1,   592,   593,    -1,   595,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   603,    -1,    -1,    -1,   607,    -1,
      -1,    -1,    76,    -1,    -1,   157,    -1,   616,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   626,    -1,    -1,
     629,    -1,   195,    -1,    -1,    -1,    -1,   101,   637,   440,
     104,   105,   106,   107,   108,   109,   110,   111,   112,    -1,
      -1,   650,    -1,   652,   653,   999,   655,    -1,    -1,    -1,
      -1,    -1,   661,    70,   830,   664,   665,   666,    -1,    -1,
      -1,    -1,  1016,  1017,    -1,   697,   698,   699,   700,   701,
     702,   703,   704,   705,   706,   707,   708,   709,   710,   711,
     712,   713,   714,   715,   101,   258,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    -1,    -1,   101,    -1,
     174,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     521,   720,    -1,   889,    -1,   288,    -1,    -1,    -1,    -1,
      -1,   294,    -1,    -1,    -1,    -1,    -1,   736,   737,    -1,
      70,  1519,   149,   150,    -1,   744,    -1,  1525,    -1,  1527,
      -1,     1,    -1,    -1,     4,   777,    -1,    -1,    -1,    -1,
      -1,   324,   761,    -1,   157,   764,    -1,   766,    -1,    -1,
      -1,   101,    -1,   574,   104,   105,   106,   107,   108,   109,
     110,   111,   112,    -1,   783,   784,    -1,    -1,    -1,    -1,
      -1,   592,   593,    -1,    -1,   791,    -1,    -1,   794,   129,
      -1,    -1,    -1,   802,    -1,   804,    -1,    -1,    58,    -1,
      -1,    -1,    -1,    -1,    -1,   616,    -1,    -1,    -1,   149,
     150,    -1,    -1,   153,   823,    -1,    -1,    -1,   629,    -1,
     160,   161,    82,   999,   101,    -1,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    -1,    -1,    98,    -1,
    1016,  1017,   102,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   861,    -1,    -1,    -1,    -1,    -1,    -1,   868,
      -1,    -1,    -1,    -1,    -1,   874,   875,    -1,    -1,    -1,
      -1,    -1,    -1,   446,    -1,   884,    -1,   886,    -1,   139,
      -1,    -1,   914,    -1,    -1,   145,    -1,   147,   897,    -1,
      -1,   151,    -1,    -1,    -1,    -1,  1684,   174,    -1,  1687,
     932,   161,   162,   163,    -1,   937,    -1,    -1,    -1,   720,
      -1,    -1,    -1,    -1,  1268,  1269,   948,    -1,    -1,   179,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   936,  1282,  1283,
      -1,   191,   192,    -1,   101,   195,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    -1,   520,    -1,    -1,
     761,   957,    -1,    -1,    -1,  1743,    -1,    -1,   990,   965,
      -1,    -1,  1316,  1317,    -1,    -1,    -1,   540,    -1,    -1,
      -1,    -1,    12,    13,    14,    15,    16,    -1,    -1,    -1,
      -1,    -1,   242,    -1,    -1,   245,    -1,   560,   994,    -1,
      -1,   997,    -1,    -1,  1003,    -1,    -1,    -1,   258,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1794,   174,    -1,    -1,
      -1,  1020,    -1,    -1,    -1,   275,    -1,    -1,  1027,    -1,
      -1,    -1,   595,   283,    -1,    -1,    -1,    -1,   288,    -1,
      70,    -1,    -1,    -1,   294,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   302,    -1,    -1,    -1,   619,    -1,    -1,    -1,
      -1,  1057,    -1,   626,    -1,    -1,    -1,    -1,    -1,    -1,
    1069,   101,   322,    -1,   324,   325,   106,   107,   108,   109,
     110,   111,   112,   884,    -1,    -1,   887,    -1,    -1,   339,
      -1,    -1,    -1,   343,    -1,  1117,   659,   660,    -1,   129,
      -1,    -1,  1268,  1269,    -1,    -1,    -1,    -1,    -1,    -1,
    1109,    -1,    -1,    -1,    -1,    -1,  1282,  1283,  1117,   149,
     150,  1899,    -1,    -1,    -1,    -1,   376,    -1,    -1,    -1,
     160,   161,    -1,   101,    -1,   936,   104,   105,   106,   107,
     108,   109,   110,   111,   112,    -1,    -1,  1146,    -1,    -1,
    1316,  1317,    -1,    -1,    -1,    -1,    -1,  1501,  1502,    -1,
     410,    -1,    -1,    -1,  1163,  1164,    -1,    -1,    -1,    -1,
    1192,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1178,
    1958,  1959,    -1,    -1,   152,  1184,   436,    -1,    -1,    -1,
     440,    -1,    -1,  1192,    -1,    -1,   446,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1227,  1228,  1229,    -1,    -1,
      -1,    -1,  1234,  1235,    -1,  1211,    -1,    -1,    -1,  1997,
      -1,  1220,    99,    -1,   101,  1224,   103,   104,   105,   106,
     107,   108,   109,   110,   111,   112,  1258,    -1,    -1,    -1,
      -1,  1237,    -1,    -1,    -1,   101,    -1,    -1,    -1,  1245,
     106,   107,   108,   109,   110,   111,   112,   113,    -1,    -1,
      -1,   117,    -1,   119,    -1,   515,   516,    -1,    -1,  1613,
     520,   521,   149,  1295,  1296,   152,   153,   132,   133,   134,
     135,   136,   137,   138,   139,   140,   141,   142,  1287,  1288,
     853,   146,    -1,    -1,   150,    -1,    -1,   153,    -1,    -1,
    1644,   864,   552,    -1,    -1,  1649,  1650,   557,    -1,    -1,
     560,   561,    -1,   563,    -1,    -1,    -1,    -1,   173,    -1,
      -1,    -1,    -1,   886,   574,    -1,   576,  1326,  1327,  1328,
    1329,  1330,  1331,    -1,    -1,  1501,  1502,  1336,  1337,    -1,
     590,    -1,   592,   593,   101,   595,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,  1355,  1356,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   616,  1363,    -1,   619,
      -1,  1537,    -1,   623,    -1,    -1,   626,    -1,    -1,   629,
      -1,   631,    -1,    -1,    -1,    -1,  1385,   637,    -1,    -1,
      -1,  1192,   149,    12,    13,    14,    15,    16,    -1,    -1,
     650,    -1,   652,   653,    -1,   655,  1405,    -1,    -1,    -1,
      -1,   661,    -1,    -1,   664,   665,   666,   101,    -1,  1220,
      -1,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
    1429,    -1,    -1,   117,    -1,   119,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1441,    -1,  1613,    -1,    -1,
    1446,    70,    -1,    -1,    -1,  1451,  1452,  1453,    -1,    -1,
      -1,    -1,    -1,    -1,    70,    -1,   150,    -1,    -1,   153,
     720,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1644,    -1,
      -1,    -1,   101,  1649,  1650,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   744,   101,    -1,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,    -1,    -1,    -1,
     129,   761,    -1,    -1,    -1,    -1,  1515,    -1,    -1,    -1,
    1519,    -1,  1521,   129,    -1,  1119,  1525,    -1,  1527,    -1,
     149,   150,    -1,   783,   784,    -1,    -1,    -1,    -1,    -1,
      -1,   160,   161,   149,   150,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   802,    -1,   160,   161,  1555,  1556,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    12,    13,    14,    15,    16,
      -1,    -1,    -1,  1595,    -1,    -1,    -1,   101,    -1,    -1,
     830,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
      -1,  1590,    -1,   117,    -1,   119,    -1,    -1,    -1,    -1,
    1163,    -1,  1601,   853,  1405,  1604,    -1,    -1,    -1,     1,
      -1,   861,     4,    -1,   864,    -1,    -1,    -1,   868,    -1,
      -1,    -1,    -1,    70,   874,   875,   150,    -1,    -1,   153,
    1629,    -1,  1976,  1196,   884,    -1,   886,   887,    -1,   889,
      -1,  1637,   101,  1639,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   101,  1654,  1655,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    58,    -1,    -1,   101,
    1669,  1670,   104,   105,   106,   107,   108,   109,   110,   111,
     112,    -1,   129,    -1,    -1,  1684,   936,    -1,  1687,    70,
      82,   150,    -1,    -1,   153,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   149,   150,    -1,  1704,   153,    -1,    -1,    -1,
     102,    -1,    -1,   160,   161,    -1,    -1,  1716,    -1,    -1,
     101,   153,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   101,    -1,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,  1743,    -1,    -1,   139,   129,   999,
      -1,   101,    -1,   145,   104,   105,   106,   107,   108,   109,
     110,   111,   112,    -1,    -1,    -1,  1016,  1017,   149,   150,
     162,    -1,    -1,    -1,  1337,  1797,    -1,    -1,    -1,   160,
     161,    -1,    -1,   152,    -1,    -1,  1785,    -1,   180,  1383,
      -1,    -1,    -1,    -1,    -1,  1794,   101,    -1,    -1,   191,
     192,   106,   107,   108,   109,   110,   111,   112,   113,    -1,
    1976,    -1,   117,  1835,   119,    -1,    -1,    -1,    -1,  1069,
      -1,    -1,  1385,    -1,    -1,    -1,    -1,    -1,   220,  1828,
      -1,   101,    -1,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,    -1,   236,   150,    70,    -1,    -1,   241,
     242,    -1,    -1,   245,  1853,    -1,    -1,    -1,    -1,  1109,
      -1,    -1,  1861,    -1,    -1,    -1,    -1,  1117,    -1,    -1,
       1,    -1,  1871,    -1,    -1,   267,    -1,   101,   270,   149,
     272,    -1,   106,   107,   108,   109,   110,   111,   112,    -1,
      -1,   283,    -1,    -1,    -1,    -1,  1146,  1896,  1897,    -1,
    1899,  1900,    -1,    -1,   296,   129,  1928,    -1,  1930,    -1,
      -1,    -1,    -1,  1163,  1164,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   149,   150,    58,    -1,    -1,
     322,    -1,    -1,   325,  1184,  1934,   160,   161,    -1,    -1,
      -1,    -1,  1192,  1965,    -1,    -1,  1196,   339,    -1,    70,
      -1,   343,    -1,    -1,  1548,    -1,    -1,    -1,  1208,  1958,
    1959,    -1,    -1,    -1,    -1,    -1,    -1,  1561,    -1,    -1,
    1220,   102,    -1,  1995,  1224,   367,    -1,    -1,    -1,    -1,
     101,    -1,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,  1555,    -1,    -1,    -1,    -1,    -1,  1997,    -1,
      -1,    -1,    -1,    -1,  1598,    -1,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,   145,    -1,    47,    -1,  1268,  1269,
    2042,    -1,    -1,    -1,    -1,    -1,    -1,  1828,   149,   150,
      -1,   162,  1282,  1283,    -1,    -1,    -1,  1287,  1288,   160,
     161,    -1,    73,    -1,   436,    -1,    -1,    -1,   440,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   192,    -1,    -1,    -1,    -1,  1316,  1317,    -1,    -1,
      -1,   463,    -1,    -1,    -1,    -1,  1326,  1327,  1328,  1329,
    1330,  1331,    -1,    -1,    -1,    -1,    -1,  1337,   119,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1897,    -1,    -1,    -1,
      -1,   132,    -1,   134,  1698,  1355,  1356,    -1,  1702,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1715,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   521,
      -1,  1725,    -1,   164,    -1,  1385,   267,    -1,   101,    -1,
      -1,  1704,    -1,   106,   107,   108,   109,   110,   111,   112,
     181,    -1,   283,    -1,    -1,  1405,    -1,    -1,    -1,    -1,
     552,    -1,    -1,    -1,    -1,   557,   129,    -1,    -1,   561,
      -1,   563,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1429,
      -1,    -1,   574,    -1,   576,    -1,   149,   150,   219,    -1,
     153,   322,   223,    -1,   325,   226,   227,   160,   161,   230,
     592,   593,   233,   234,    -1,    -1,    -1,  1801,   339,    -1,
      -1,  1805,   343,    -1,  1808,   607,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   616,    -1,    -1,    -1,     1,   621,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   629,    -1,  1833,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1501,  1502,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   295,    -1,    -1,   298,    -1,    -1,
      -1,  1521,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1874,    -1,    -1,    -1,  1878,    58,    -1,  1537,   319,   320,
      -1,    -1,    -1,    -1,    -1,  1889,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   335,  1555,  1556,    -1,    -1,   440,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1912,    -1,
    1914,  1915,    -1,    -1,    -1,    -1,    -1,    -1,   720,   102,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1900,    -1,    -1,
    1590,    -1,    -1,  1937,    -1,   737,    -1,    -1,    -1,    -1,
      -1,  1601,    -1,    -1,  1604,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1613,    -1,    -1,    -1,    -1,    -1,   761,
      -1,    -1,   145,    -1,   766,  1969,    -1,    -1,    -1,  1629,
      -1,    -1,    -1,    -1,    -1,  1979,    -1,    -1,    -1,   162,
     521,   783,   784,    -1,  1644,    -1,    -1,    -1,   429,  1649,
    1650,    -1,    -1,    -1,  1654,  1655,    -1,    -1,    -1,    -1,
     802,    -1,    -1,    -1,    -1,    -1,  2010,    -1,  2012,   192,
      -1,   552,    -1,    12,    13,    14,    15,    16,    -1,    -1,
     561,    -1,   563,    -1,    -1,    -1,  2030,    -1,   830,    -1,
      -1,    -1,  2036,   574,    -1,   576,    -1,    -1,    -1,    -1,
      -1,    -1,  2046,   484,  1704,    -1,    -1,    -1,    -1,    -1,
      -1,   592,   593,    -1,    -1,   496,  1716,    -1,    -1,   861,
      -1,    -1,    -1,    -1,    -1,    -1,   868,    -1,    -1,    -1,
      -1,    70,   874,    -1,    -1,   616,    -1,    -1,    -1,    -1,
      -1,    -1,   884,    -1,    -1,   887,    -1,   889,   629,    -1,
      -1,    -1,   894,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     283,    -1,   101,    -1,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,   101,    -1,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,    -1,    -1,   570,
     129,    -1,    -1,    -1,   936,    -1,    -1,    -1,    -1,   322,
      -1,    -1,   325,   129,    -1,    -1,    -1,    -1,    -1,    -1,
     149,   150,    -1,    -1,   153,    -1,   339,    -1,    -1,    -1,
     343,   160,   161,   149,   150,    -1,    -1,   153,  1828,    -1,
     611,   612,    -1,    -1,   160,   161,    -1,    -1,    -1,   720,
      -1,    -1,    -1,   624,    -1,    -1,    -1,   173,    -1,    -1,
      -1,    -1,    -1,  1853,    -1,    -1,   101,   999,    -1,   104,
     105,   106,   107,   108,   109,   110,   111,   112,    -1,    -1,
      -1,  1871,    -1,    -1,  1016,  1017,    -1,    -1,    -1,    -1,
     761,    -1,   101,    -1,   129,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,    -1,  1896,  1897,    -1,    -1,
    1900,    -1,   783,   784,   149,   150,    -1,    -1,    -1,    -1,
     129,   156,    -1,    -1,    -1,   160,   161,   440,    -1,    -1,
      -1,   802,    -1,    -1,    -1,    -1,    -1,  1069,    -1,    -1,
     149,   150,    -1,    -1,  1934,    -1,    -1,    -1,    -1,    -1,
      -1,   160,   161,    -1,    -1,    -1,    -1,    -1,    -1,   830,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   740,
     741,    -1,    -1,    -1,    -1,   746,    -1,  1109,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1117,  1976,    -1,    -1,    -1,
     861,    -1,    -1,    -1,    -1,    -1,   767,   868,    -1,   770,
     771,    -1,   773,    -1,   775,   776,    -1,    -1,   521,    -1,
      -1,    -1,    -1,   884,  1146,    -1,   887,    -1,   889,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1164,    -1,    -1,    -1,    -1,    -1,    -1,   552,
      -1,    -1,    -1,   814,    -1,    -1,    -1,   818,   561,    -1,
     563,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1192,   574,    -1,   576,    -1,   936,    -1,    -1,    -1,  1201,
      -1,    12,    13,    14,    15,    16,    -1,    -1,    -1,   592,
     593,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1220,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   616,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   629,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   895,    -1,    -1,    -1,   999,    70,
      -1,    -1,    -1,    -1,    -1,    -1,  1268,  1269,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1016,  1017,    -1,    -1,    -1,
    1282,  1283,    -1,    -1,    -1,  1287,  1288,    -1,    -1,    -1,
     101,    -1,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1316,  1317,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,  1326,  1327,  1328,  1329,  1069,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   720,   149,   150,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   160,
     161,    -1,    -1,  1355,  1356,    -1,    -1,    -1,    -1,    -1,
      12,    13,    14,    15,    16,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1117,    -1,   761,    -1,
      -1,    -1,    -1,    -1,    -1,  1026,   101,    -1,    -1,   104,
     105,   106,   107,   108,   109,   110,   111,   112,    -1,    -1,
     783,   784,    -1,  1405,    -1,  1146,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    70,   802,
      -1,    -1,    -1,  1164,    -1,    -1,  1067,  1429,    -1,    -1,
      -1,    -1,    -1,  1074,   149,   150,  1077,    -1,   153,     1,
      -1,    -1,     4,    -1,    -1,   160,   161,   830,    -1,   101,
      -1,  1192,   104,   105,   106,   107,   108,   109,   110,   111,
     112,    -1,   101,    -1,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,    -1,    -1,   129,   861,  1220,
      -1,    -1,    -1,    -1,    -1,   868,    -1,    -1,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    58,   149,   150,  1501,
    1502,   884,    -1,    -1,   887,    -1,   889,    -1,   160,   161,
     149,   150,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      82,   160,   161,    -1,    -1,  1527,    -1,  1268,  1269,    -1,
      -1,    -1,    -1,    -1,    -1,  1537,    -1,    -1,    -1,    -1,
     102,  1282,  1283,    -1,    -1,    -1,  1287,  1288,    -1,     1,
      -1,    -1,     4,   936,  1556,    -1,    -1,  1198,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1206,  1207,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1316,  1317,   139,    -1,    -1,
      -1,    -1,    -1,   145,    -1,   147,    -1,    -1,  1590,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1601,
      -1,    -1,  1604,    -1,    -1,    -1,    58,    -1,    -1,    -1,
      -1,  1613,    -1,    -1,  1355,  1356,   999,   179,    -1,    -1,
      -1,    -1,    -1,  1264,    -1,    -1,    -1,    -1,    -1,   191,
      82,    -1,  1273,  1016,  1017,  1276,    -1,  1278,  1279,    -1,
      -1,    -1,  1644,    -1,    -1,    -1,    -1,  1649,  1650,    -1,
     102,    -1,  1654,  1655,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1405,    -1,    -1,    -1,  1670,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1319,    -1,
     242,    -1,    -1,   245,    -1,    -1,  1069,   139,   250,    -1,
      -1,    -1,    -1,   145,    -1,   147,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1716,    -1,    -1,    -1,    -1,    -1,
      -1,   283,    -1,    -1,    -1,    -1,    -1,   179,    -1,    -1,
      -1,    -1,    -1,    -1,  1117,    -1,    -1,    -1,    -1,   191,
     302,    -1,    -1,    -1,    -1,  1386,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1501,  1502,    -1,  1146,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1164,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     242,    -1,    -1,   245,    -1,    -1,  1537,    -1,   250,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1192,
      -1,    -1,    -1,    -1,   376,  1556,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1828,    -1,    -1,  1470,
      -1,   283,    -1,    -1,    -1,    -1,    -1,  1220,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   410,  1590,
     302,  1853,    -1,    -1,    -1,    -1,    -1,    -1,  1499,    -1,
    1601,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1871,
      -1,    -1,  1613,    -1,   436,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1525,  1268,  1269,    -1,    -1,    -1,
    1531,    -1,    -1,    -1,  1896,  1897,    -1,    -1,    -1,  1282,
    1283,    -1,    -1,  1644,  1287,  1288,    -1,    -1,  1649,  1650,
      -1,    -1,    -1,  1654,  1655,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     1,   376,    -1,     4,    -1,    -1,    -1,
      -1,    -1,  1934,  1316,  1317,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   515,   516,    -1,    -1,  1959,   410,  1600,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1355,  1356,  1976,  1716,    -1,    -1,    -1,    -1,
      58,    -1,    -1,    -1,   436,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   557,    -1,    -1,    -1,   561,
      -1,   563,    -1,    -1,    82,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   576,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1405,    -1,   102,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1676,  1677,    -1,    -1,    -1,
      -1,    -1,    -1,  1684,    -1,    -1,    -1,  1688,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   139,    -1,   515,   516,    -1,    -1,   145,    -1,   147,
      -1,    -1,    -1,    -1,    -1,   637,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1828,   650,    -1,
     652,   653,    -1,   655,    -1,    -1,    -1,    -1,    -1,   661,
      -1,   179,   664,   665,   666,   557,    -1,    -1,    -1,   561,
      -1,   563,  1853,   191,    -1,    -1,    -1,    -1,  1501,  1502,
      -1,    -1,    -1,    -1,   576,    -1,    -1,    -1,    -1,    -1,
    1871,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1787,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1537,  1896,  1897,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   242,    -1,    -1,   245,    -1,    -1,
      -1,    -1,   250,  1556,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   744,    -1,    -1,   637,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1934,    -1,    -1,    -1,    -1,   650,    -1,
     652,   653,    -1,   655,    -1,   283,    -1,  1590,    -1,   661,
      -1,    -1,   664,   665,   666,    -1,  1857,    -1,  1601,    -1,
      -1,    -1,    -1,    -1,   302,    -1,    -1,    -1,    -1,    -1,
    1613,    -1,    -1,    -1,    -1,  1976,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1644,    -1,    -1,    -1,    -1,  1649,  1650,     1,    -1,
      -1,  1654,  1655,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   744,    -1,    -1,    -1,    -1,    -1,   376,   861,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   874,   875,    -1,    48,    86,  1958,    -1,    52,
      -1,    54,    92,    93,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   410,  1716,    -1,    -1,    -1,    -1,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   126,    -1,   436,    -1,
      -1,    -1,    -1,    -1,    -1,    98,    99,    -1,   101,    -1,
     103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,    -1,   117,   118,   119,    -1,   121,   122,
      -1,    48,    -1,    -1,    -1,    52,   129,    54,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   861,
      -1,    -1,    -1,    -1,    71,    -1,   149,    -1,    -1,   152,
     153,    -1,   874,   875,    -1,    -1,   159,   160,   161,   162,
     163,   164,   165,    -1,    -1,    -1,    -1,   515,   516,    -1,
      -1,    98,    99,    -1,   101,  1828,   103,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,    -1,
     117,   118,   119,    -1,   121,   122,    -1,    -1,    -1,    -1,
    1853,    -1,   129,    -1,     0,    -1,    -1,     3,    -1,   557,
      -1,    -1,    -1,   561,    -1,   563,    -1,    -1,  1871,    -1,
      -1,    -1,   149,   150,    -1,   152,   153,    -1,   576,    -1,
     157,    -1,   159,   160,   161,   162,   163,   164,   165,    -1,
      -1,    -1,    -1,  1896,  1897,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   292,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1109,    -1,    -1,
      76,  1934,    -1,    -1,    -1,  1117,    -1,    -1,    -1,   637,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   650,    -1,   652,   653,    -1,   655,    -1,    -1,
      -1,    -1,    -1,   661,  1146,    -1,   664,   665,   666,    -1,
      -1,    -1,    -1,  1976,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1164,    -1,    -1,     3,    -1,    -1,    -1,   135,
      -1,    -1,    -1,    -1,    12,    13,    14,    15,    16,    -1,
      -1,    19,  1184,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      -1,    49,    50,    51,    -1,    53,    -1,  1109,    -1,    -1,
      -1,    -1,  1224,    -1,    -1,  1117,   744,    -1,    -1,   439,
      -1,   441,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     450,   451,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1146,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   229,    -1,    -1,   104,   105,    -1,    -1,
      -1,    -1,  1164,    -1,    -1,    -1,    -1,    -1,   244,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   254,    -1,
      -1,   129,  1184,    -1,    -1,    -1,    -1,    -1,   264,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   278,   279,   152,   153,    -1,    -1,    -1,   285,
     286,    -1,   160,   161,  1326,  1327,  1328,  1329,  1330,  1331,
      -1,    -1,  1224,    -1,    -1,   301,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   861,    -1,    -1,    -1,    -1,   558,    -1,
      -1,    -1,    -1,  1355,  1356,   321,   874,   875,    -1,    -1,
      12,    13,    14,    15,    16,    17,    -1,    19,    -1,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    -1,    -1,    -1,    -1,    50,    51,
      -1,    -1,    -1,    55,    56,    57,    58,    59,    60,    61,
      62,   377,    -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1429,    -1,    -1,
      -1,    -1,    -1,    -1,  1326,  1327,  1328,  1329,  1330,  1331,
      -1,    -1,   408,    -1,    -1,    -1,    -1,    -1,    -1,   101,
      -1,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,    -1,    -1,  1355,  1356,    -1,    -1,    -1,   434,    -1,
      55,    56,   438,    -1,    -1,    -1,    -1,   129,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   457,    -1,    -1,    -1,   461,   462,   149,   150,   465,
     152,   153,    -1,    -1,    -1,    90,    -1,    -1,   160,   161,
      -1,    -1,    -1,    -1,   480,   481,   482,   483,    -1,  1521,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   499,    -1,    -1,    -1,  1429,    -1,    -1,
      -1,   507,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1556,   140,    -1,    -1,   143,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   777,    -1,   535,
      -1,   156,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1590,    -1,
      -1,  1109,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1117,
     566,    -1,  1604,    -1,    -1,    -1,    -1,   573,    -1,    -1,
      -1,    -1,    -1,   579,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   210,    -1,  1629,  1146,  1521,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   603,   604,    -1,
      -1,    -1,    -1,    -1,   854,   855,  1164,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   865,   866,   867,    -1,    -1,
     870,    -1,    -1,    -1,  1556,    -1,  1184,    -1,    -1,    -1,
     255,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     265,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   276,    -1,    -1,    -1,    -1,    -1,    -1,  1590,    -1,
      -1,   667,    -1,    -1,    -1,    -1,  1224,    -1,    -1,    -1,
      -1,    -1,  1604,    -1,  1716,    -1,    -1,    -1,   303,    -1,
      -1,    -1,    -1,    -1,    -1,   310,   311,    -1,    -1,    -1,
     315,    -1,    -1,    -1,    -1,    -1,    -1,  1629,    -1,    -1,
     950,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   348,    -1,    -1,    -1,    -1,   353,    -1,
     736,   356,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   751,   996,    -1,    -1,   755,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   764,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1326,  1327,
    1328,  1329,  1330,  1331,    -1,    -1,    -1,    -1,    -1,    -1,
     786,    -1,    -1,    -1,  1716,    -1,    -1,    -1,    -1,   795,
      -1,    -1,    -1,  1043,    -1,   801,    -1,  1355,  1356,    -1,
      -1,    -1,  1052,  1053,  1054,  1055,    -1,    -1,    -1,    -1,
    1060,  1061,    -1,    -1,    -1,    -1,    -1,    -1,   443,    -1,
    1070,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1871,
     455,   456,   838,    -1,    -1,    -1,    -1,    -1,    -1,   845,
      -1,  1091,    -1,  1093,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1896,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   872,    -1,    -1,    -1,
      -1,  1429,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1934,    -1,    -1,    -1,  1146,    -1,    -1,    -1,
      98,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   109,    -1,   111,    -1,   113,    -1,    -1,    -1,    -1,
     926,    -1,  1172,    -1,    -1,    -1,    -1,    -1,    -1,  1179,
      -1,  1181,  1182,    -1,    -1,    -1,    -1,    -1,    -1,  1871,
      -1,  1191,    -1,  1193,    -1,  1195,    -1,  1197,    -1,    -1,
      -1,    -1,  1202,   151,    -1,   153,   154,    -1,    -1,    -1,
      -1,   586,    -1,  1521,  1896,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   195,  1556,    -1,
      -1,    -1,  1934,  1009,    -1,    -1,    -1,  1013,    -1,    -1,
     635,    -1,    -1,  1263,  1020,    -1,    -1,    -1,    -1,    -1,
    1270,  1271,    -1,    -1,  1030,    -1,    -1,    -1,    -1,    -1,
      -1,  1037,  1590,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1046,    -1,  1048,    -1,  1294,    -1,  1604,    -1,    -1,    -1,
      -1,  1301,    -1,    -1,    -1,  1305,    -1,    -1,    -1,    -1,
     258,    -1,   260,   261,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1629,    -1,    -1,  1080,    -1,    -1,    -1,  1084,    -1,
      -1,    -1,    -1,    -1,    -1,  1335,    -1,    -1,    -1,    -1,
     288,    -1,  1098,    -1,    -1,  1101,   294,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1375,   324,    -1,    -1,    -1,
      -1,    -1,   330,    -1,   332,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   778,    -1,    -1,    -1,    -1,  1716,  1409,
     785,    -1,    -1,    -1,    -1,    -1,    -1,  1417,    -1,  1419,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1190,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1215,
      -1,    -1,    -1,    -1,    -1,    -1,  1466,  1467,    -1,    -1,
      -1,    -1,   420,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1481,  1482,    -1,  1484,   860,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1493,    -1,    -1,    -1,    -1,   446,    -1,
     448,   449,    -1,  1503,  1504,    -1,    -1,   882,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   162,    -1,    -1,    -1,    -1,   912,    -1,    -1,
      -1,   916,    -1,    -1,    -1,   493,    -1,    -1,    -1,    -1,
      -1,    -1,  1308,    -1,    -1,    -1,  1312,    -1,    -1,    -1,
      -1,   191,   192,  1871,    -1,   513,    -1,    -1,    -1,    -1,
     518,    -1,   520,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1342,    -1,  1896,    -1,
      -1,    -1,   540,   223,   542,   543,    -1,    -1,    -1,    -1,
     230,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   560,    -1,  1614,  1615,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   572,  1625,  1934,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1392,    -1,    -1,  1395,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   595,    -1,   597,
     598,    -1,    -1,    -1,    -1,  1411,    -1,    -1,    -1,    -1,
    1660,  1661,    -1,    -1,    -1,    -1,    -1,    -1,   298,    -1,
      -1,   619,   620,    -1,    -1,    -1,    -1,    -1,   626,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   322,   323,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1462,    -1,    -1,    -1,
      -1,   659,   660,   343,    -1,  1471,    -1,    -1,    -1,  1475,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1489,  1490,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1745,    -1,  1122,  1123,  1124,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1764,    -1,    -1,  1767,  1768,    -1,
      -1,    82,    -1,    -1,  1774,    -1,    -1,  1152,    -1,    -1,
      -1,    -1,    -1,   413,    -1,    -1,    -1,    98,    -1,    -1,
      -1,    -1,  1167,    -1,    -1,    -1,    -1,    -1,    -1,   429,
     430,    -1,   432,   433,    -1,    -1,    -1,    -1,    -1,    -1,
     440,    -1,    -1,    -1,   444,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1205,    -1,    -1,    -1,    -1,    -1,   147,    -1,    -1,    -1,
     151,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1605,
    1606,   162,    -1,    -1,    -1,   485,    -1,    -1,    -1,   489,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   179,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   192,    -1,    -1,   195,    -1,    -1,    -1,    -1,    -1,
      -1,   521,    -1,    -1,    -1,    -1,    -1,    -1,  1898,    -1,
      -1,    -1,    -1,    -1,    -1,   853,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   864,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   245,    -1,    -1,    -1,   886,    -1,
      -1,   571,    -1,    -1,   574,    -1,    -1,   258,    -1,   897,
      -1,    -1,    -1,    -1,    -1,    -1,  1956,    -1,   906,    -1,
      -1,    47,   592,   593,    -1,  1340,    -1,    -1,  1343,    -1,
      -1,    -1,    -1,   603,  1730,    -1,    -1,   607,    -1,    -1,
    1980,    -1,    -1,   294,   614,    -1,   616,    -1,    -1,    -1,
     147,   302,    -1,  1993,    -1,    -1,    -1,    -1,  1754,    -1,
      -1,    -1,    -1,    -1,    -1,   162,    -1,    -1,  2008,    -1,
      -1,   322,    -1,   324,    -1,    -1,  1772,    -1,    -1,    -1,
      -1,    -1,   179,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   119,    -1,   192,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1799,    -1,    -1,   132,    -1,   134,    -1,
      -1,    -1,    -1,    -1,    -1,  1003,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   376,    -1,    -1,  1824,    -1,
      -1,  1827,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1027,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   245,    -1,
     720,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   410,
      -1,    -1,    -1,    -1,    -1,    -1,   736,   737,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   746,   747,    -1,   749,
     750,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   440,
      -1,   761,    -1,    -1,   764,   446,   766,   767,    -1,    -1,
     226,   227,    -1,   773,   230,   302,    -1,   233,   234,    -1,
      -1,    -1,    -1,   783,   784,    -1,   376,    -1,    -1,  1534,
      -1,    -1,    -1,    -1,    -1,   322,    -1,  1923,    -1,    -1,
      -1,    -1,   802,    -1,    -1,    -1,   806,    -1,    -1,    -1,
     810,    -1,    -1,    -1,   814,   815,    -1,    -1,   818,   819,
      -1,    -1,    -1,    -1,    -1,    -1,   826,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   515,   516,    -1,    -1,    -1,    -1,
     521,    -1,    -1,    -1,    -1,  1163,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   319,   320,    -1,    -1,    -1,   868,   869,
      -1,    -1,    -1,    -1,    -1,    -1,  1621,    -1,  1196,   335,
      -1,    -1,    -1,   410,  1202,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   574,    -1,    -1,    -1,   897,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   593,   440,   595,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   515,   516,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   616,   936,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   637,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   650,
      -1,   652,   653,   429,   655,    -1,    -1,    -1,    -1,    -1,
     661,    -1,    -1,   664,   665,   666,    -1,    -1,   515,   516,
      -1,    -1,    -1,    -1,   521,    -1,    -1,  1742,    -1,    -1,
      -1,    -1,    -1,  1003,    -1,    -1,  1751,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1337,
    1020,  1021,    -1,   179,    -1,    -1,    -1,  1027,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   192,    -1,    -1,   720,
     496,    -1,    -1,    -1,    -1,    -1,    -1,   574,    -1,   205,
      -1,   207,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     650,    -1,    -1,   744,    -1,   655,   593,  1385,    -1,  1069,
      -1,   661,    -1,    -1,  1074,  1075,    -1,  1077,  1078,    -1,
     761,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   616,
     680,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   783,   784,    -1,    -1,    -1,    -1,    -1,    -1,
     637,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   802,    -1,    -1,    -1,    -1,   716,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   293,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   611,   612,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   624,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   868,    -1,    -1,
      -1,    -1,  1192,   720,   875,    -1,    -1,    -1,  1198,  1199,
      -1,    -1,    -1,    -1,    -1,   886,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   744,    -1,    -1,
    1220,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   761,    -1,    -1,  1555,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   936,   783,   784,    -1,    -1,
      -1,    -1,    -1,    -1,  1264,  1265,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1273,  1274,   802,  1276,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   740,   741,    -1,  1287,  1288,    -1,
     746,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     466,   767,    -1,    -1,   770,   771,   472,   773,    -1,   775,
     776,   477,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   868,    -1,    -1,    -1,    -1,    -1,    -1,   875,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   814,    -1,
      48,    -1,   818,    -1,    52,    -1,    54,    -1,    -1,  1687,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    71,    -1,    -1,  1704,    -1,  1069,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1405,    -1,    -1,   564,   936,
      98,    99,    -1,   101,    -1,   103,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,  1109,   117,
     118,   119,    -1,   121,   122,    -1,    -1,   593,    -1,   895,
      -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     606,    -1,    -1,    -1,    -1,   143,   144,   145,    -1,    -1,
      -1,   149,   150,    -1,   152,   153,    -1,  1785,    -1,    -1,
      -1,   159,   160,   161,   162,   163,   164,   165,    -1,    -1,
      -1,    -1,  1163,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   657,    -1,  1184,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1192,    -1,    -1,    -1,  1515,    -1,    -1,    -1,    -1,
      -1,    -1,   678,   679,    -1,    -1,   682,    -1,   684,    -1,
      -1,  1531,    -1,    -1,   690,    -1,   692,   693,    -1,  1220,
      -1,    -1,  1069,  1224,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   720,    -1,    -1,    -1,    -1,    -1,
    1026,    -1,    -1,    -1,    -1,    -1,    -1,   733,    -1,    -1,
      -1,    -1,  1900,    -1,    -1,    -1,    -1,    -1,   744,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1601,   758,    -1,    -1,   761,  1287,  1288,    -1,    -1,
      -1,  1067,    -1,    -1,    -1,    -1,    -1,    -1,  1074,    -1,
      -1,  1077,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   788,    -1,    -1,   791,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1326,  1327,  1328,    -1,  1330,
    1331,    -1,    -1,    -1,  1654,  1655,  1337,  1184,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1192,    -1,    -1,    -1,  1669,
    1670,   827,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1997,
      -1,    -1,    -1,    -1,    -1,  1685,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1220,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1385,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   875,
      -1,    -1,    -1,    -1,  1405,    -1,    -1,    -1,    -1,    -1,
     886,   887,    -1,    -1,    -1,    -1,    -1,    -1,   894,    -1,
      -1,    -1,  1198,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1206,  1207,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1287,  1288,  1352,    -1,    -1,  1355,  1356,   923,    -1,    -1,
      -1,  1361,    -1,    -1,    -1,  1365,    -1,  1367,    -1,    -1,
     936,    -1,    -1,    -1,    -1,  1785,    -1,    -1,   944,    -1,
      -1,    -1,    -1,  1793,    -1,   951,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1264,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1273,    -1,    -1,
    1276,    -1,  1278,  1279,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1521,   997,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1853,    -1,    -1,    -1,  1857,  1858,    -1,
      -1,  1861,    -1,  1319,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1555,    -1,    -1,    -1,  1405,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1899,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1064,    -1,
    1066,    -1,  1068,    -1,    -1,    -1,    -1,  1507,    -1,    -1,
    1601,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1386,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1629,  1539,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1958,  1959,
      -1,  1551,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1654,  1655,    -1,  1566,  1567,  1134,  1135,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1521,    -1,    -1,  1997,    -1,    -1,
    1590,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1470,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1704,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1196,    -1,    -1,  1499,    -1,    -1,  1202,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1211,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1220,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1601,    -1,    -1,    -1,    -1,    -1,
      -1,  1237,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1252,    -1,    -1,  1255,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1700,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1710,    -1,    -1,  1713,  1714,    -1,  1716,  1654,  1655,    -1,
      -1,  1721,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1600,    -1,    -1,  1828,    -1,    -1,
      -1,  1307,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1853,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1345,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1363,    -1,    -1,
    1366,    -1,    -1,    -1,    -1,    -1,  1897,    -1,    -1,  1900,
    1676,  1677,    -1,    -1,  1814,    -1,    -1,    -1,    -1,  1819,
    1820,    -1,  1688,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1405,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1415,
    1416,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1441,    -1,  1443,    -1,  1879,
      -1,    -1,  1882,  1883,    -1,    -1,    -1,  1887,  1888,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1853,    -1,    -1,    -1,
      -1,  1787,    -1,    -1,     1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1951,  1952,  1953,    -1,  1521,    -1,    -1,    -1,    -1,
    1526,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    48,  1972,    -1,    -1,    52,    -1,    54,    -1,    -1,
      -1,    -1,    -1,    -1,  1984,  1985,  1986,    -1,    -1,    -1,
      -1,  1857,    69,    -1,    71,    72,    -1,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    91,    92,    93,    94,    -1,    96,
    1586,    98,    99,    -1,   101,    -1,   103,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,     1,   121,   122,    -1,    -1,    -1,    -1,
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    17,
      -1,  1627,    -1,    -1,  1630,    -1,    -1,    -1,    -1,    -1,
      -1,   148,   149,    -1,    -1,   152,   153,    -1,    -1,    -1,
     157,    -1,   159,   160,   161,   162,   163,   164,   165,    -1,
      48,    -1,    -1,    -1,    52,    -1,    54,   174,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    69,    -1,    71,    72,    -1,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    90,    91,    92,    93,    94,    -1,    96,    -1,
      98,    99,    -1,   101,    -1,   103,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,    -1,   121,   122,    -1,    -1,    -1,    -1,    -1,
      -1,   129,    -1,    -1,    -1,    -1,     1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     148,   149,    17,    -1,   152,   153,    -1,    -1,    -1,   157,
      -1,   159,   160,   161,   162,   163,   164,   165,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   174,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    -1,    -1,    -1,    52,    -1,    54,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    69,    -1,    71,    72,    -1,    74,
      -1,    -1,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
    1836,    96,    -1,    98,    99,    -1,   101,    -1,   103,   104,
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
      63,    -1,    -1,    -1,    67,    -1,    69,    70,    71,    72,
      -1,    74,    -1,    -1,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,    87,    88,    89,    90,    91,    92,
      93,    94,    -1,    96,    -1,    98,    99,   100,   101,    -1,
     103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,    -1,   121,   122,
      -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   148,   149,    -1,    -1,   152,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,    -1,
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
      -1,    -1,    -1,    -1,   143,   144,   145,    -1,    -1,    -1,
     149,   150,   151,   152,   153,    -1,    -1,    -1,    -1,    -1,
     159,   160,   161,   162,   163,   164,   165,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   174,     3,     4,     5,     6,
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
      -1,    -1,    -1,    -1,    -1,    -1,   143,   144,   145,    -1,
      -1,    -1,   149,   150,    -1,   152,   153,    -1,    -1,    -1,
      -1,    -1,   159,   160,   161,   162,   163,   164,   165,    -1,
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
     165,     3,     4,     5,     6,     7,     8,     9,    10,    11,
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
     164,   165,     1,    -1,     3,     4,     5,     6,     7,     8,
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
      -1,   160,   161,     1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      -1,    19,    -1,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      -1,    49,    50,    51,    -1,    53,    -1,    55,    56,    57,
      58,    59,    60,    61,    62,    63,    -1,    -1,    -1,    67,
      -1,    -1,    70,     5,    -1,    -1,    -1,    75,    76,    -1,
      12,    13,    14,    15,    16,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   100,    -1,    -1,    -1,   104,   105,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    -1,    -1,    -1,
      52,    -1,    54,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     5,
     148,    -1,    -1,    -1,   152,   153,    12,    13,    14,    15,
      16,    -1,   160,   161,    -1,    -1,    98,    99,    -1,   101,
      -1,   103,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,    -1,   117,   118,   119,    -1,   121,
     122,    -1,    48,    -1,    -1,    -1,    52,   129,    54,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    70,    71,    -1,   149,    -1,    -1,
     152,   153,    -1,    -1,    -1,    -1,    -1,   159,   160,   161,
     162,   163,   164,   165,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,   160,   161,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    -1,    -1,    19,    -1,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    -1,    49,    50,    51,    -1,    53,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    12,    13,    14,    15,
      16,    -1,    -1,    19,    70,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      -1,    -1,    -1,    -1,    50,    51,    -1,    -1,   104,   105,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   101,   152,   153,    -1,    -1,
      -1,    -1,    -1,    -1,   160,   161,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    -1,
      -1,    19,    -1,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
     156,    49,    50,    51,    -1,    53,    -1,    -1,    -1,    -1,
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
     150,   151,   152,   153,    -1,    -1,    -1,    -1,    -1,   159,
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
      -1,    -1,    -1,    -1,    -1,   149,   150,    -1,   152,   153,
      -1,    -1,    -1,    -1,    -1,   159,   160,   161,   162,   163,
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
      -1,    -1,    -1,    -1,    -1,   149,   150,    -1,   152,   153,
      -1,    -1,    -1,    -1,    -1,   159,   160,   161,   162,   163,
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
     164,   165,    12,    13,    14,    15,    16,    -1,    -1,    19,
      -1,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    -1,    49,
      50,    51,    -1,    53,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    12,    13,    14,    15,    16,    -1,    -1,    19,
      70,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    -1,    49,
      50,    51,    -1,    53,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      70
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
     245,   223,   222,   174,   245,   151,   156,   221,   150,   221,
     222,   243,   174,   464,   151,   151,   151,   225,   260,   261,
     149,   214,   149,   182,   232,   198,   253,   107,     1,   223,
     404,   384,   176,   176,   154,   352,   177,   177,   154,   154,
     148,   157,   347,   148,   177,   214,   186,   214,   464,   148,
     154,   154,   191,   191,   352,   151,   151,   352,   352,   151,
     151,   154,   155,   131,   351,   131,   154,   177,   177,   151,
     151,   154,   450,   451,   452,   297,   449,   155,   174,   404,
     404,   174,   151,   410,   404,   174,   223,    75,    76,   157,
     235,   236,   237,   151,   221,    73,   223,   221,    73,   174,
     104,   150,   221,   222,   243,   150,   221,   223,   242,   245,
     245,   174,   221,   148,   157,   237,   223,   149,   176,   174,
     182,   151,   156,   151,   151,   155,   156,   251,   255,   359,
     401,   177,   154,   154,   347,   464,   148,   148,   154,   154,
     177,   177,   177,   176,   177,   151,   151,   151,   151,   151,
     449,   404,   336,     1,   213,   233,   234,   402,     1,   156,
       1,   176,   223,   235,    73,   174,   151,   223,    73,   165,
     223,   222,   245,   245,   174,   104,   221,   165,   165,    73,
     221,   150,   221,   222,   174,     1,   176,   176,   262,   295,
     297,   458,   156,   174,   153,   182,   267,   268,   269,   223,
     198,   188,    73,   106,   252,   254,   151,   464,   148,   151,
     151,   151,   354,   149,   404,   441,   442,   338,   131,     1,
     155,   156,   148,   272,   273,   279,   223,    73,   174,   223,
     150,   221,   221,   150,   221,   222,   150,   221,   150,   221,
     223,   165,   165,   165,   148,   272,   262,   177,   149,   196,
     401,   449,   180,   156,   101,   149,   151,   156,   155,    73,
     151,   223,   149,   223,   223,   148,   176,   213,   233,   236,
     238,   239,   279,   223,   165,   165,   165,   150,   221,   150,
     221,   150,   221,   238,   177,   174,   259,   297,   267,   154,
     213,   174,   267,   269,   223,   221,   107,   107,   352,   223,
     228,   177,   236,   150,   221,   150,   221,   150,   221,   177,
     259,   212,   151,   156,   182,   151,   151,   156,   151,   255,
      73,   250,   177,     1,   223,   148,   228,   148,   151,   225,
     182,   270,   149,   174,   270,   223,    73,   151,   225,   155,
     156,   213,   151,   223,   182,   180,   271,   151,   174,   151,
     155,   174,   180
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
     242,   242,   242,   242,   242,   242,   242,   242,   242,   242,
     242,   242,   243,   243,   243,   244,   244,   245,   245,   245,
     246,   246,   246,   246,   246,   246,   246,   246,   246,   246,
     246,   246,   246,   246,   246,   246,   246,   246,   246,   246,
     247,   247,   248,   249,   250,   251,   251,   252,   252,   253,
     253,   254,   255,   255,   255,   255,   255,   255,   256,   256,
     257,   257,   257,   258,   258,   259,   259,   260,   260,   260,
     260,   261,   262,   262,   262,   262,   262,   263,   264,   264,
     265,   265,   265,   265,   265,   266,   266,   267,   267,   268,
     268,   269,   269,   270,   270,   270,   271,   271,   272,   272,
     273,   273,   274,   274,   275,   275,   276,   276,   277,   277,
     278,   278,   279,   279,   279,   280,   280,   281,   281,   281,
     281,   281,   282,   282,   282,   283,   283,   283,   284,   284,
     284,   284,   284,   285,   285,   286,   286,   287,   287,   287,
     288,   288,   288,   288,   288,   289,   289,   290,   290,   290,
     290,   291,   291,   292,   292,   292,   293,   293,   293,   294,
     294,   294,   295,   295,   295,   296,   296,   297,   297,   298,
     298,   299,   299,   299,   299,   299,   300,   301,   301,   301,
     302,   302,   303,   303,   303,   303,   303,   303,   303,   303,
     304,   304,   304,   304,   304,   304,   304,   304,   304,   304,
     304,   304,   304,   304,   304,   304,   304,   304,   304,   304,
     304,   304,   304,   304,   304,   304,   304,   304,   305,   305,
     306,   307,   307,   308,   308,   308,   308,   308,   309,   309,
     310,   310,   310,   310,   311,   311,   311,   311,   311,   311,
     312,   312,   312,   312,   313,   314,   313,   313,   315,   315,
     315,   315,   316,   316,   316,   317,   317,   317,   317,   318,
     318,   318,   319,   319,   319,   319,   319,   319,   320,   320,
     320,   321,   321,   322,   322,   324,   323,   325,   323,   326,
     323,   327,   323,   323,   328,   328,   329,   329,   330,   330,
     331,   331,   331,   332,   332,   332,   332,   332,   332,   332,
     332,   333,   333,   334,   334,   334,   334,   334,   334,   334,
     334,   334,   334,   335,   335,   335,   336,   336,   336,   337,
     337,   337,   338,   339,   339,   340,   340,   341,   341,   342,
     343,   344,   343,   343,   343,   343,   345,   343,   343,   343,
     346,   346,   347,   347,   347,   347,   348,   348,   348,   349,
     349,   349,   349,   349,   349,   349,   350,   350,   350,   350,
     351,   351,   352,   352,   352,   352,   353,   353,   353,   353,
     354,   354,   354,   354,   354,   355,   355,   355,   355,   355,
     356,   356,   357,   357,   358,   358,   359,   359,   359,   360,
     360,   360,   361,   361,   362,   362,   362,   362,   363,   363,
     364,   364,   364,   364,   364,   365,   365,   366,   366,   367,
     367,   367,   367,   367,   368,   368,   369,   369,   371,   370,
     372,   370,   370,   370,   373,   373,   373,   373,   374,   374,
     374,   374,   375,   375,   376,   376,   377,   377,   378,   378,
     378,   378,   379,   379,   379,   380,   380,   381,   381,   382,
     382,   383,   383,   384,   384,   385,   385,   385,   386,   386,
     387,   387,   388,   388,   389,   389,   390,   391,   392,   392,
     392,   392,   392,   393,   392,   394,   392,   395,   392,   396,
     392,   397,   392,   398,   398,   398,   399,   399,   400,   400,
     400,   400,   400,   400,   400,   400,   400,   400,   401,   401,
     401,   402,   403,   403,   404,   404,   405,   405,   406,   407,
     407,   408,   408,   408,   409,   409,   409,   409,   409,   409,
     410,   410,   411,   411,   411,   411,   412,   412,   412,   412,
     413,   413,   413,   413,   413,   413,   413,   414,   414,   414,
     414,   415,   415,   415,   416,   416,   416,   416,   416,   417,
     417,   417,   417,   418,   418,   418,   418,   418,   418,   419,
     419,   419,   420,   420,   420,   420,   420,   421,   421,   421,
     421,   422,   422,   422,   422,   422,   422,   423,   423,   424,
     424,   424,   424,   425,   425,   425,   425,   426,   426,   426,
     426,   426,   426,   426,   427,   427,   427,   427,   427,   428,
     428,   428,   428,   428,   429,   429,   429,   430,   430,   430,
     430,   431,   431,   431,   432,   432,   432,   432,   432,   433,
     433,   434,   434,   434,   435,   435,   436,   436,   437,   437,
     437,   438,   438,   438,   438,   438,   439,   439,   439,   439,
     440,   440,   440,   441,   441,   441,   441,   442,   442,   442,
     442,   443,   443,   443,   443,   444,   444,   444,   444,   444,
     445,   445,   445,   445,   446,   446,   446,   447,   447,   447,
     448,   448,   448,   448,   448,   448,   449,   449,   449,   450,
     450,   450,   450,   450,   451,   451,   451,   451,   452,   452,
     453,   453,   453,   454,   454,   455,   455,   455,   455,   455,
     455,   456,   456,   456,   456,   456,   456,   456,   456,   456,
     456,   457,   457,   457,   457,   458,   458,   458,   459,   459,
     460,   460,   460,   460,   460,   460,   461,   461,   461,   461,
     461,   461,   462,   462,   462,   463,   463,   464,   464,   465,
     465
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
       3,     3,     5,     5,     5,     5,     5,     5,     3,     4,
       5,     5,     5,     7,     7,     7,     7,     7,     7,     2,
       3,     4,     4,     4,     6,     6,     6,     6,     6,     6,
       3,     4,     1,     1,     1,     1,     1,     1,     1,     1,
       3,     4,     2,     3,     3,     2,     3,     2,     3,     3,
       6,     2,     2,     3,     3,     3,     3,     3,     3,     5,
       1,     1,     5,     5,     4,     0,     1,     4,     6,     1,
       3,     4,     3,     5,     3,     3,     6,     7,     3,     5,
       3,     3,     4,     8,     9,     0,     2,     1,     1,     1,
       1,     2,     1,     2,     2,     2,     1,     3,     1,     1,
       6,     8,    10,    12,    14,     0,     1,     0,     1,     1,
       3,     4,     7,     0,     1,     3,     1,     3,     0,     1,
       1,     2,     0,     1,     4,     5,     0,     1,     3,     4,
       1,     3,     2,     2,     1,     7,     5,     1,     1,     1,
       1,     1,     2,     3,     6,     3,     3,     4,     1,     2,
       2,     3,     8,     8,     8,     5,     9,     2,     2,     5,
       3,     5,     4,     3,     4,     4,     7,     2,     1,     1,
       1,     3,     6,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     0,     1,     1,     2,     1,
       1,     1,     1,     1,     1,     1,     4,     1,     2,     3,
       1,     2,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     0,     1,
       5,     0,     1,     1,     2,     2,     3,     3,     1,     3,
       1,     2,     2,     2,     4,     4,     4,     4,     1,     1,
       1,     2,     2,     3,     1,     0,     3,     2,     1,     2,
       2,     3,     1,     2,     2,     1,     2,     2,     3,     1,
       2,     2,     1,     2,     3,     1,     2,     3,     1,     3,
       4,     1,     1,     1,     1,     0,     7,     0,     8,     0,
       8,     0,     8,     1,     0,     3,     3,     3,     1,     1,
       2,     1,     1,     1,     2,     1,     2,     1,     2,     1,
       2,     0,     2,     3,     4,     4,     3,     2,     2,     3,
       3,     2,     1,     0,     1,     4,     1,     2,     2,     0,
       1,     4,     1,     2,     3,     1,     2,     0,     1,     2,
       6,     0,     8,     7,     8,     9,     0,    12,    11,     1,
       3,     3,     2,     2,     4,     5,     0,     2,     5,     0,
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
       7,     0,     8,     1,     2,     3,     0,     5,     3,     4,
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
#line 564 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.enterScope(); }
#line 7296 "Parser/parser.cc"
    break;

  case 3:
#line 568 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 7302 "Parser/parser.cc"
    break;

  case 4:
#line 575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantInteger( *(yyvsp[0].tok) ) ); }
#line 7308 "Parser/parser.cc"
    break;

  case 5:
#line 576 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7314 "Parser/parser.cc"
    break;

  case 6:
#line 577 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7320 "Parser/parser.cc"
    break;

  case 7:
#line 578 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7326 "Parser/parser.cc"
    break;

  case 8:
#line 579 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantChar( *(yyvsp[0].tok) ) ); }
#line 7332 "Parser/parser.cc"
    break;

  case 19:
#line 600 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 7338 "Parser/parser.cc"
    break;

  case 20:
#line 604 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.constant) = build_constantStr( *(yyvsp[0].str) ); }
#line 7344 "Parser/parser.cc"
    break;

  case 21:
#line 608 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 7350 "Parser/parser.cc"
    break;

  case 22:
#line 610 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 7360 "Parser/parser.cc"
    break;

  case 23:
#line 621 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 7366 "Parser/parser.cc"
    break;

  case 24:
#line 623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 7372 "Parser/parser.cc"
    break;

  case 25:
#line 627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_dimensionref( (yyvsp[0].tok) ) ); }
#line 7378 "Parser/parser.cc"
    break;

  case 27:
#line 630 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 7384 "Parser/parser.cc"
    break;

  case 28:
#line 632 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new StmtExpr( dynamic_cast<CompoundStmt *>(maybeMoveBuild<Statement>((yyvsp[-1].sn)) ) ) ); }
#line 7390 "Parser/parser.cc"
    break;

  case 29:
#line 634 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7396 "Parser/parser.cc"
    break;

  case 30:
#line 636 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7402 "Parser/parser.cc"
    break;

  case 31:
#line 638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild<Expression>( (yyvsp[-3].en) );
			(yyval.en) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 7412 "Parser/parser.cc"
    break;

  case 32:
#line 648 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "Adjacent identifiers are not meaningful in an expression. "
											   "Possible problem is identifier \"", *(yyvsp[-1].tok).str,
											   "\" is a misspelled typename or an incorrectly specified type name, "
											   "e.g., missing generic parameter or missing struct/union/enum before typename." ) );
			(yyval.en) = nullptr;
 		}
#line 7424 "Parser/parser.cc"
    break;

  case 33:
#line 656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "Identifier \"", *(yyvsp[-1].tok).str, "\" cannot appear before a type. "
											   "Possible problem is misspelled storage or CV qualifier." ) );
			(yyval.en) = nullptr;
		}
#line 7434 "Parser/parser.cc"
    break;

  case 35:
#line 666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// steal the association node from the singleton and delete the wrapper
			(yyvsp[-2].genexpr)->associations.splice((yyvsp[-2].genexpr)->associations.end(), (yyvsp[0].genexpr)->associations);
			delete (yyvsp[0].genexpr);
			(yyval.genexpr) = (yyvsp[-2].genexpr);
		}
#line 7445 "Parser/parser.cc"
    break;

  case 36:
#line 676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuildType((yyvsp[-2].decl)), maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } );
		}
#line 7454 "Parser/parser.cc"
    break;

  case 37:
#line 681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } ); }
#line 7460 "Parser/parser.cc"
    break;

  case 39:
#line 690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-5].en), new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) ) ) )) ) ); }
#line 7466 "Parser/parser.cc"
    break;

  case 40:
#line 696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7472 "Parser/parser.cc"
    break;

  case 41:
#line 698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7478 "Parser/parser.cc"
    break;

  case 42:
#line 700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, new ExpressionNode( (yyvsp[-3].constant) ), (yyvsp[-1].en) ) ); }
#line 7484 "Parser/parser.cc"
    break;

  case 43:
#line 702 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.en) = new ExpressionNode( new ConstructorExpr( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) ) );
		}
#line 7494 "Parser/parser.cc"
    break;

  case 44:
#line 708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7500 "Parser/parser.cc"
    break;

  case 45:
#line 710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 7506 "Parser/parser.cc"
    break;

  case 46:
#line 712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 7512 "Parser/parser.cc"
    break;

  case 47:
#line 714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), new ExpressionNode( (yyvsp[-2].constant) ) ) ); }
#line 7518 "Parser/parser.cc"
    break;

  case 48:
#line 716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7524 "Parser/parser.cc"
    break;

  case 49:
#line 718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 7530 "Parser/parser.cc"
    break;

  case 50:
#line 720 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-1].en), build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) ) ) ); }
#line 7536 "Parser/parser.cc"
    break;

  case 51:
#line 722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7542 "Parser/parser.cc"
    break;

  case 52:
#line 724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[0].aggKey), (yyvsp[-2].en) ) ); }
#line 7548 "Parser/parser.cc"
    break;

  case 53:
#line 726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7554 "Parser/parser.cc"
    break;

  case 54:
#line 728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 7560 "Parser/parser.cc"
    break;

  case 55:
#line 730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7566 "Parser/parser.cc"
    break;

  case 56:
#line 732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::IncrPost, (yyvsp[-1].en) ) ); }
#line 7572 "Parser/parser.cc"
    break;

  case 57:
#line 734 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::DecrPost, (yyvsp[-1].en) ) ); }
#line 7578 "Parser/parser.cc"
    break;

  case 58:
#line 736 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].in), true ) ) ); }
#line 7584 "Parser/parser.cc"
    break;

  case 59:
#line 738 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].in), true ))->set_maybeConstructed( false ) ) ); }
#line 7590 "Parser/parser.cc"
    break;

  case 60:
#line 740 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) );
		}
#line 7600 "Parser/parser.cc"
    break;

  case 61:
#line 749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7606 "Parser/parser.cc"
    break;

  case 64:
#line 756 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7612 "Parser/parser.cc"
    break;

  case 65:
#line 761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Default parameter for argument is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7618 "Parser/parser.cc"
    break;

  case 68:
#line 768 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7624 "Parser/parser.cc"
    break;

  case 70:
#line 774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-1].tok) ) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7630 "Parser/parser.cc"
    break;

  case 71:
#line 776 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-3].tok) ) ), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7636 "Parser/parser.cc"
    break;

  case 72:
#line 778 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7642 "Parser/parser.cc"
    break;

  case 73:
#line 780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7648 "Parser/parser.cc"
    break;

  case 74:
#line 782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7654 "Parser/parser.cc"
    break;

  case 75:
#line 784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7660 "Parser/parser.cc"
    break;

  case 76:
#line 789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_constantInteger( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 7666 "Parser/parser.cc"
    break;

  case 77:
#line 791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_field_name_FLOATINGconstant( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 7672 "Parser/parser.cc"
    break;

  case 78:
#line 793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_varref( (yyvsp[-1].tok) ), (yyvsp[0].en) ) );
		}
#line 7680 "Parser/parser.cc"
    break;

  case 79:
#line 800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7686 "Parser/parser.cc"
    break;

  case 80:
#line 802 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Expression * constant = build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) );
			(yyval.en) = (yyvsp[-1].en) != nullptr ? new ExpressionNode( build_fieldSel( (yyvsp[-1].en),  constant ) ) : new ExpressionNode( constant );
		}
#line 7695 "Parser/parser.cc"
    break;

  case 83:
#line 814 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 7701 "Parser/parser.cc"
    break;

  case 84:
#line 816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en)->set_extension( true ); }
#line 7707 "Parser/parser.cc"
    break;

  case 85:
#line 821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 7727 "Parser/parser.cc"
    break;

  case 86:
#line 837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_val( (yyvsp[-1].op), (yyvsp[0].en) ) ); }
#line 7733 "Parser/parser.cc"
    break;

  case 87:
#line 839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Incr, (yyvsp[0].en) ) ); }
#line 7739 "Parser/parser.cc"
    break;

  case 88:
#line 841 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Decr, (yyvsp[0].en) ) ); }
#line 7745 "Parser/parser.cc"
    break;

  case 89:
#line 843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7751 "Parser/parser.cc"
    break;

  case 90:
#line 845 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 7757 "Parser/parser.cc"
    break;

  case 91:
#line 847 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7763 "Parser/parser.cc"
    break;

  case 92:
#line 849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 7769 "Parser/parser.cc"
    break;

  case 93:
#line 851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_offsetOf( (yyvsp[-3].decl), build_varref( (yyvsp[-1].tok) ) ) ); }
#line 7775 "Parser/parser.cc"
    break;

  case 94:
#line 853 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.en) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 7784 "Parser/parser.cc"
    break;

  case 95:
#line 860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::PointTo; }
#line 7790 "Parser/parser.cc"
    break;

  case 96:
#line 861 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AddressOf; }
#line 7796 "Parser/parser.cc"
    break;

  case 97:
#line 863 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::And; }
#line 7802 "Parser/parser.cc"
    break;

  case 98:
#line 867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::UnPlus; }
#line 7808 "Parser/parser.cc"
    break;

  case 99:
#line 868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::UnMinus; }
#line 7814 "Parser/parser.cc"
    break;

  case 100:
#line 869 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::Neg; }
#line 7820 "Parser/parser.cc"
    break;

  case 101:
#line 870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::BitNeg; }
#line 7826 "Parser/parser.cc"
    break;

  case 103:
#line 876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cast( (yyvsp[-2].decl), (yyvsp[0].en) ) ); }
#line 7832 "Parser/parser.cc"
    break;

  case 104:
#line 878 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 7838 "Parser/parser.cc"
    break;

  case 105:
#line 880 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 7844 "Parser/parser.cc"
    break;

  case 106:
#line 882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( nullptr ) ) ); }
#line 7850 "Parser/parser.cc"
    break;

  case 107:
#line 884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 7856 "Parser/parser.cc"
    break;

  case 108:
#line 886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Return cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7862 "Parser/parser.cc"
    break;

  case 109:
#line 888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7868 "Parser/parser.cc"
    break;

  case 110:
#line 890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7874 "Parser/parser.cc"
    break;

  case 118:
#line 910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Exp, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7880 "Parser/parser.cc"
    break;

  case 120:
#line 916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mul, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7886 "Parser/parser.cc"
    break;

  case 121:
#line 918 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Div, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7892 "Parser/parser.cc"
    break;

  case 122:
#line 920 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mod, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7898 "Parser/parser.cc"
    break;

  case 124:
#line 926 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Plus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7904 "Parser/parser.cc"
    break;

  case 125:
#line 928 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Minus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7910 "Parser/parser.cc"
    break;

  case 127:
#line 934 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7916 "Parser/parser.cc"
    break;

  case 128:
#line 936 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::RShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7922 "Parser/parser.cc"
    break;

  case 130:
#line 942 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7928 "Parser/parser.cc"
    break;

  case 131:
#line 944 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7934 "Parser/parser.cc"
    break;

  case 132:
#line 946 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7940 "Parser/parser.cc"
    break;

  case 133:
#line 948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7946 "Parser/parser.cc"
    break;

  case 135:
#line 954 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Eq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7952 "Parser/parser.cc"
    break;

  case 136:
#line 956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Neq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7958 "Parser/parser.cc"
    break;

  case 138:
#line 962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitAnd, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7964 "Parser/parser.cc"
    break;

  case 140:
#line 968 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Xor, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7970 "Parser/parser.cc"
    break;

  case 142:
#line 974 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitOr, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7976 "Parser/parser.cc"
    break;

  case 144:
#line 980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), true ) ); }
#line 7982 "Parser/parser.cc"
    break;

  case 146:
#line 986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), false ) ); }
#line 7988 "Parser/parser.cc"
    break;

  case 148:
#line 992 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7994 "Parser/parser.cc"
    break;

  case 149:
#line 995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-3].en), (yyvsp[-3].en), (yyvsp[0].en) ) ); }
#line 8000 "Parser/parser.cc"
    break;

  case 152:
#line 1006 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
//			if ( $2 == OperKinds::AtAssn ) {
//				SemanticError( yylloc, "C @= assignment is currently unimplemented." ); $$ = nullptr;
//			} else {
				(yyval.en) = new ExpressionNode( build_binary_val( (yyvsp[-1].op), (yyvsp[-2].en), (yyvsp[0].en) ) );
//			} // if
		}
#line 8012 "Parser/parser.cc"
    break;

  case 153:
#line 1014 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8018 "Parser/parser.cc"
    break;

  case 154:
#line 1019 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8024 "Parser/parser.cc"
    break;

  case 158:
#line 1029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::Assign; }
#line 8030 "Parser/parser.cc"
    break;

  case 159:
#line 1030 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AtAssn; }
#line 8036 "Parser/parser.cc"
    break;

  case 160:
#line 1034 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ExpAssn; }
#line 8042 "Parser/parser.cc"
    break;

  case 161:
#line 1035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MulAssn; }
#line 8048 "Parser/parser.cc"
    break;

  case 162:
#line 1036 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::DivAssn; }
#line 8054 "Parser/parser.cc"
    break;

  case 163:
#line 1037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ModAssn; }
#line 8060 "Parser/parser.cc"
    break;

  case 164:
#line 1038 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::PlusAssn; }
#line 8066 "Parser/parser.cc"
    break;

  case 165:
#line 1039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MinusAssn; }
#line 8072 "Parser/parser.cc"
    break;

  case 166:
#line 1040 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::LSAssn; }
#line 8078 "Parser/parser.cc"
    break;

  case 167:
#line 1041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::RSAssn; }
#line 8084 "Parser/parser.cc"
    break;

  case 168:
#line 1042 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AndAssn; }
#line 8090 "Parser/parser.cc"
    break;

  case 169:
#line 1043 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ERAssn; }
#line 8096 "Parser/parser.cc"
    break;

  case 170:
#line 1044 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::OrAssn; }
#line 8102 "Parser/parser.cc"
    break;

  case 171:
#line 1055 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)(new ExpressionNode( nullptr ) )->set_last( (yyvsp[-1].en) ) ) ); }
#line 8108 "Parser/parser.cc"
    break;

  case 172:
#line 1057 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-4].en)->set_last( (yyvsp[-1].en) ) ) )); }
#line 8114 "Parser/parser.cc"
    break;

  case 174:
#line 1063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8120 "Parser/parser.cc"
    break;

  case 175:
#line 1065 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 8126 "Parser/parser.cc"
    break;

  case 176:
#line 1067 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8132 "Parser/parser.cc"
    break;

  case 178:
#line 1073 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new CommaExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 8138 "Parser/parser.cc"
    break;

  case 179:
#line 1078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8144 "Parser/parser.cc"
    break;

  case 191:
#line 1096 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 8150 "Parser/parser.cc"
    break;

  case 193:
#line 1099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_directive( (yyvsp[0].tok) ) ); }
#line 8156 "Parser/parser.cc"
    break;

  case 194:
#line 1105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[0].sn)->add_label( (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 8162 "Parser/parser.cc"
    break;

  case 195:
#line 1107 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "Label \"", *(yyvsp[-3].tok).str, "\" must be associated with a statement, "
											   "where a declaration, case, or default is not a statement. "
											   "Move the label or terminate with a semi-colon." ) );
			(yyval.sn) = nullptr;
		}
#line 8173 "Parser/parser.cc"
    break;

  case 196:
#line 1117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (StatementNode *)0 ) ); }
#line 8179 "Parser/parser.cc"
    break;

  case 197:
#line 1122 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (yyvsp[-2].sn) ) ); }
#line 8185 "Parser/parser.cc"
    break;

  case 199:
#line 1128 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 8191 "Parser/parser.cc"
    break;

  case 200:
#line 1133 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8197 "Parser/parser.cc"
    break;

  case 201:
#line 1135 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8203 "Parser/parser.cc"
    break;

  case 202:
#line 1137 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8209 "Parser/parser.cc"
    break;

  case 203:
#line 1139 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8215 "Parser/parser.cc"
    break;

  case 206:
#line 1146 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 8221 "Parser/parser.cc"
    break;

  case 207:
#line 1148 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Declarations only allowed at the start of the switch body, i.e., after the '{'." ); (yyval.sn) = nullptr; }
#line 8227 "Parser/parser.cc"
    break;

  case 208:
#line 1153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_expr( (yyvsp[-1].en) ) ); }
#line 8233 "Parser/parser.cc"
    break;

  case 209:
#line 1155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_mutex( nullptr, new StatementNode( build_expr( (yyvsp[-1].en) ) ) ) ); }
#line 8239 "Parser/parser.cc"
    break;

  case 210:
#line 1162 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn); }
#line 8245 "Parser/parser.cc"
    break;

  case 211:
#line 1164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( true, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8251 "Parser/parser.cc"
    break;

  case 212:
#line 1166 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( true, (yyvsp[-7].en), (yyvsp[-2].sn) ) );
			// The semantics of the declaration list is changed to include associated initialization, which is performed
			// *before* the transfer to the appropriate case clause by hoisting the declarations into a compound
			// statement around the switch.  Statements after the initial declaration list can never be executed, and
			// therefore, are removed from the grammar even though C allows it. The change also applies to choose
			// statement.
			(yyval.sn) = (yyvsp[-3].decl) ? new StatementNode( build_compound( (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 8265 "Parser/parser.cc"
    break;

  case 213:
#line 1176 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Only declarations can appear before the list of case clauses." ); (yyval.sn) = nullptr; }
#line 8271 "Parser/parser.cc"
    break;

  case 214:
#line 1178 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( false, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8277 "Parser/parser.cc"
    break;

  case 215:
#line 1180 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( false, (yyvsp[-7].en), (yyvsp[-2].sn) ) );
			(yyval.sn) = (yyvsp[-3].decl) ? new StatementNode( build_compound( (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 8286 "Parser/parser.cc"
    break;

  case 216:
#line 1185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Only declarations can appear before the list of case clauses." ); (yyval.sn) = nullptr; }
#line 8292 "Parser/parser.cc"
    break;

  case 217:
#line 1191 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-2].ifctl), maybe_build_compound( (yyvsp[0].sn) ), nullptr ) ); }
#line 8298 "Parser/parser.cc"
    break;

  case 218:
#line 1193 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-4].ifctl), maybe_build_compound( (yyvsp[-2].sn) ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8304 "Parser/parser.cc"
    break;

  case 219:
#line 1198 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( nullptr, (yyvsp[0].en) ); }
#line 8310 "Parser/parser.cc"
    break;

  case 220:
#line 1200 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 8316 "Parser/parser.cc"
    break;

  case 221:
#line 1202 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 8322 "Parser/parser.cc"
    break;

  case 222:
#line 1204 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[-1].decl), (yyvsp[0].en) ); }
#line 8328 "Parser/parser.cc"
    break;

  case 223:
#line 1211 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = (yyvsp[0].en); }
#line 8334 "Parser/parser.cc"
    break;

  case 224:
#line 1213 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 8340 "Parser/parser.cc"
    break;

  case 226:
#line 1218 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.sn) = new StatementNode( build_case( (yyvsp[0].en) ) ); }
#line 8346 "Parser/parser.cc"
    break;

  case 227:
#line 1220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)((yyvsp[-2].sn)->set_last( new StatementNode( build_case( (yyvsp[0].en) ) ) ) ); }
#line 8352 "Parser/parser.cc"
    break;

  case 228:
#line 1225 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing case list after case." ); (yyval.sn) = nullptr; }
#line 8358 "Parser/parser.cc"
    break;

  case 229:
#line 1226 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn); }
#line 8364 "Parser/parser.cc"
    break;

  case 230:
#line 1228 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing colon after case list." ); (yyval.sn) = nullptr; }
#line 8370 "Parser/parser.cc"
    break;

  case 231:
#line 1229 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.sn) = new StatementNode( build_default() ); }
#line 8376 "Parser/parser.cc"
    break;

  case 232:
#line 1232 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing colon after default." ); (yyval.sn) = nullptr; }
#line 8382 "Parser/parser.cc"
    break;

  case 234:
#line 1237 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)( (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) )); }
#line 8388 "Parser/parser.cc"
    break;

  case 235:
#line 1241 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( maybe_build_compound( (yyvsp[0].sn) ) ); }
#line 8394 "Parser/parser.cc"
    break;

  case 236:
#line 1246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = nullptr; }
#line 8400 "Parser/parser.cc"
    break;

  case 238:
#line 1252 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ); }
#line 8406 "Parser/parser.cc"
    break;

  case 239:
#line 1254 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)( (yyvsp[-2].sn)->set_last( (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ) ) ); }
#line 8412 "Parser/parser.cc"
    break;

  case 240:
#line 1259 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8418 "Parser/parser.cc"
    break;

  case 241:
#line 1261 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_while( new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( (yyvsp[-2].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse, "" );
		}
#line 8427 "Parser/parser.cc"
    break;

  case 242:
#line 1266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( (yyvsp[-2].ifctl), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8433 "Parser/parser.cc"
    break;

  case 243:
#line 1268 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( (yyvsp[-4].ifctl), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[0].sn) ) ); }
#line 8439 "Parser/parser.cc"
    break;

  case 244:
#line 1270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( NEW_ONE, maybe_build_compound( (yyvsp[-4].sn) ) ) ); }
#line 8445 "Parser/parser.cc"
    break;

  case 245:
#line 1272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_do_while( NEW_ONE, maybe_build_compound( (yyvsp[-5].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse, "" );
		}
#line 8454 "Parser/parser.cc"
    break;

  case 246:
#line 1277 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( (yyvsp[-2].en), maybe_build_compound( (yyvsp[-5].sn) ) ) ); }
#line 8460 "Parser/parser.cc"
    break;

  case 247:
#line 1279 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( (yyvsp[-3].en), maybe_build_compound( (yyvsp[-6].sn) ), (yyvsp[0].sn) ) ); }
#line 8466 "Parser/parser.cc"
    break;

  case 248:
#line 1281 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( new ForCtrl( (ExpressionNode * )nullptr, (ExpressionNode * )nullptr, (ExpressionNode * )nullptr ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8472 "Parser/parser.cc"
    break;

  case 249:
#line 1283 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_for( new ForCtrl( (ExpressionNode * )nullptr, (ExpressionNode * )nullptr, (ExpressionNode * )nullptr ), maybe_build_compound( (yyvsp[-2].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse, "" );
		}
#line 8481 "Parser/parser.cc"
    break;

  case 250:
#line 1288 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( (yyvsp[-2].fctl), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8487 "Parser/parser.cc"
    break;

  case 251:
#line 1290 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( (yyvsp[-4].fctl), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[0].sn) ) ); }
#line 8493 "Parser/parser.cc"
    break;

  case 253:
#line 1300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 8512 "Parser/parser.cc"
    break;

  case 254:
#line 1318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (ExpressionNode * )nullptr, (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8518 "Parser/parser.cc"
    break;

  case 255:
#line 1320 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8524 "Parser/parser.cc"
    break;

  case 256:
#line 1322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-3].decl), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8530 "Parser/parser.cc"
    break;

  case 257:
#line 1325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), NEW_ZERO, OperKinds::LThan, (yyvsp[0].en)->clone(), NEW_ONE ); }
#line 8536 "Parser/parser.cc"
    break;

  case 258:
#line 1327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].compop), NEW_ZERO, (yyvsp[0].en)->clone() ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 8542 "Parser/parser.cc"
    break;

  case 259:
#line 1330 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].compop), (yyvsp[-2].en)->clone(), (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), (yyvsp[-2].en)->clone() ), NEW_ONE ); }
#line 8548 "Parser/parser.cc"
    break;

  case 260:
#line 1332 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing loop fields ('@') with an anonymous loop index is not useful." ); (yyval.fctl) = nullptr; }
#line 8554 "Parser/parser.cc"
    break;

  case 261:
#line 1334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing loop fields ('@') with an anonymous loop index is not useful." ); (yyval.fctl) = nullptr; }
#line 8560 "Parser/parser.cc"
    break;

  case 262:
#line 1337 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en)->clone(), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), (yyvsp[0].en) ); }
#line 8566 "Parser/parser.cc"
    break;

  case 263:
#line 1339 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing loop fields ('@') with an anonymous loop index is not useful." ); (yyval.fctl) = nullptr; }
#line 8572 "Parser/parser.cc"
    break;

  case 264:
#line 1341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing loop fields ('@') with an anonymous loop index is not useful." ); (yyval.fctl) = nullptr; }
#line 8578 "Parser/parser.cc"
    break;

  case 265:
#line 1343 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing loop fields ('@') with an anonymous loop index is not useful." ); (yyval.fctl) = nullptr; }
#line 8584 "Parser/parser.cc"
    break;

  case 266:
#line 1345 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing loop fields ('@') with an anonymous loop index is not useful." ); (yyval.fctl) = nullptr; }
#line 8590 "Parser/parser.cc"
    break;

  case 267:
#line 1347 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing loop fields ('@') with an anonymous loop index is not useful." ); (yyval.fctl) = nullptr; }
#line 8596 "Parser/parser.cc"
    break;

  case 268:
#line 1350 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-2].en), NEW_ZERO, OperKinds::LThan, (yyvsp[0].en)->clone(), NEW_ONE ); }
#line 8602 "Parser/parser.cc"
    break;

  case 269:
#line 1352 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-3].en), UPDOWN( (yyvsp[-1].compop), NEW_ZERO, (yyvsp[0].en)->clone() ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 8608 "Parser/parser.cc"
    break;

  case 270:
#line 1355 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), UPDOWN( (yyvsp[-1].compop), (yyvsp[-2].en)->clone(), (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), (yyvsp[-2].en)->clone() ), NEW_ONE ); }
#line 8614 "Parser/parser.cc"
    break;

  case 271:
#line 1357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing start value so cannot compare." ); (yyval.fctl) = nullptr; }
#line 8620 "Parser/parser.cc"
    break;

  case 272:
#line 1359 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LEThan || (yyvsp[-1].compop) == OperKinds::GEThan ) { SemanticError( yylloc, "Decrement with missing comparison is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 8629 "Parser/parser.cc"
    break;

  case 273:
#line 1365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en)->clone(), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), (yyvsp[0].en) ); }
#line 8635 "Parser/parser.cc"
    break;

  case 274:
#line 1367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing start declaration so cannot compare." ); (yyval.fctl) = nullptr; }
#line 8641 "Parser/parser.cc"
    break;

  case 275:
#line 1369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LEThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, "Equal for comparison is meaningless with missing comparison. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 8650 "Parser/parser.cc"
    break;

  case 276:
#line 1374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en)->clone(), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), nullptr ); }
#line 8656 "Parser/parser.cc"
    break;

  case 277:
#line 1376 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing start declaration so cannot compare." ); (yyval.fctl) = nullptr; }
#line 8662 "Parser/parser.cc"
    break;

  case 278:
#line 1378 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan ) { SemanticError( yylloc, "Negative range \"-~\" is meaningless when comparison and iterator are empty. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, "Equal for comparison is meaningless with missing comparison. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 8672 "Parser/parser.cc"
    break;

  case 279:
#line 1385 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-1].decl), NEW_ZERO, OperKinds::LThan, (yyvsp[0].en), NEW_ONE ); }
#line 8678 "Parser/parser.cc"
    break;

  case 280:
#line 1387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].decl), UPDOWN( (yyvsp[-1].compop), NEW_ZERO, (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 8684 "Parser/parser.cc"
    break;

  case 281:
#line 1390 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-3].decl), UPDOWN( (yyvsp[-1].compop), (yyvsp[-2].en)->clone(), (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), (yyvsp[-2].en)->clone() ), NEW_ONE ); }
#line 8690 "Parser/parser.cc"
    break;

  case 282:
#line 1392 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing start value so cannot compare." ); (yyval.fctl) = nullptr; }
#line 8696 "Parser/parser.cc"
    break;

  case 283:
#line 1394 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LEThan || (yyvsp[-1].compop) == OperKinds::GEThan ) { SemanticError( yylloc, "Decrement with missing comparison is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-3].decl), (yyvsp[-2].en), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 8705 "Parser/parser.cc"
    break;

  case 284:
#line 1400 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), (yyvsp[0].en) ); }
#line 8711 "Parser/parser.cc"
    break;

  case 285:
#line 1402 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing start declaration so cannot compare." ); (yyval.fctl) = nullptr; }
#line 8717 "Parser/parser.cc"
    break;

  case 286:
#line 1404 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LEThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, "Equal for comparison is meaningless with missing comparison. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-4].en), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 8726 "Parser/parser.cc"
    break;

  case 287:
#line 1409 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), nullptr ); }
#line 8732 "Parser/parser.cc"
    break;

  case 288:
#line 1411 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing start declaration so cannot compare." ); (yyval.fctl) = nullptr; }
#line 8738 "Parser/parser.cc"
    break;

  case 289:
#line 1413 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan ) { SemanticError( yylloc, "Negative range \"-~\" is meaningless when comparison and iterator are empty. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, "Equal for comparison is meaningless with missing comparison. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-4].en), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 8748 "Parser/parser.cc"
    break;

  case 290:
#line 1420 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.fctl) = nullptr;
			//$$ = forCtrl( new ExpressionNode( build_varref( $3 ) ), $1, nullptr, OperKinds::Range, nullptr, nullptr );
		}
#line 8757 "Parser/parser.cc"
    break;

  case 291:
#line 1425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LEThan || (yyvsp[-1].compop) == OperKinds::GEThan ) { SemanticError( yylloc, "All enumation ranges are equal (all values). Remove \"=~\"." ); (yyval.fctl) = nullptr; }
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.fctl) = nullptr;
		}
#line 8766 "Parser/parser.cc"
    break;

  case 292:
#line 1433 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GThan; }
#line 8772 "Parser/parser.cc"
    break;

  case 293:
#line 1435 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 8778 "Parser/parser.cc"
    break;

  case 294:
#line 1437 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GEThan; }
#line 8784 "Parser/parser.cc"
    break;

  case 295:
#line 1442 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LThan; }
#line 8790 "Parser/parser.cc"
    break;

  case 296:
#line 1444 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GThan; }
#line 8796 "Parser/parser.cc"
    break;

  case 298:
#line 1450 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 8802 "Parser/parser.cc"
    break;

  case 299:
#line 1452 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GEThan; }
#line 8808 "Parser/parser.cc"
    break;

  case 300:
#line 1457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Goto ) ); }
#line 8814 "Parser/parser.cc"
    break;

  case 301:
#line 1461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_computedgoto( (yyvsp[-1].en) ) ); }
#line 8820 "Parser/parser.cc"
    break;

  case 302:
#line 1464 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThrough ) ); }
#line 8826 "Parser/parser.cc"
    break;

  case 303:
#line 1466 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::FallThrough ) ); }
#line 8832 "Parser/parser.cc"
    break;

  case 304:
#line 1468 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThroughDefault ) ); }
#line 8838 "Parser/parser.cc"
    break;

  case 305:
#line 1471 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Continue ) ); }
#line 8844 "Parser/parser.cc"
    break;

  case 306:
#line 1475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Continue ) ); }
#line 8850 "Parser/parser.cc"
    break;

  case 307:
#line 1478 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Break ) ); }
#line 8856 "Parser/parser.cc"
    break;

  case 308:
#line 1482 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Break ) ); }
#line 8862 "Parser/parser.cc"
    break;

  case 309:
#line 1484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_return( (yyvsp[-1].en) ) ); }
#line 8868 "Parser/parser.cc"
    break;

  case 310:
#line 1486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 8874 "Parser/parser.cc"
    break;

  case 311:
#line 1488 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr ) ); }
#line 8880 "Parser/parser.cc"
    break;

  case 312:
#line 1490 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn) ) ); }
#line 8886 "Parser/parser.cc"
    break;

  case 313:
#line 1492 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Coroutine ) ); }
#line 8892 "Parser/parser.cc"
    break;

  case 314:
#line 1494 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Coroutine ) ); }
#line 8898 "Parser/parser.cc"
    break;

  case 315:
#line 1496 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Generator ) ); }
#line 8904 "Parser/parser.cc"
    break;

  case 316:
#line 1498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Generator ) ); }
#line 8910 "Parser/parser.cc"
    break;

  case 317:
#line 1500 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_throw( (yyvsp[-1].en) ) ); }
#line 8916 "Parser/parser.cc"
    break;

  case 318:
#line 1502 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume( (yyvsp[-1].en) ) ); }
#line 8922 "Parser/parser.cc"
    break;

  case 319:
#line 1504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume_at( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 8928 "Parser/parser.cc"
    break;

  case 322:
#line 1514 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_with( (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8934 "Parser/parser.cc"
    break;

  case 323:
#line 1520 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_mutex( (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8940 "Parser/parser.cc"
    break;

  case 324:
#line 1524 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 8946 "Parser/parser.cc"
    break;

  case 325:
#line 1529 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8952 "Parser/parser.cc"
    break;

  case 327:
#line 1535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 8958 "Parser/parser.cc"
    break;

  case 328:
#line 1539 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) )); }
#line 8964 "Parser/parser.cc"
    break;

  case 330:
#line 1546 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8970 "Parser/parser.cc"
    break;

  case 331:
#line 1550 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 8976 "Parser/parser.cc"
    break;

  case 332:
#line 1555 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8982 "Parser/parser.cc"
    break;

  case 333:
#line 1557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-3].en), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[-4].en), (yyvsp[0].wfs) ); }
#line 8988 "Parser/parser.cc"
    break;

  case 334:
#line 1559 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 8994 "Parser/parser.cc"
    break;

  case 335:
#line 1561 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( nullptr, maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 9000 "Parser/parser.cc"
    break;

  case 336:
#line 1564 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 9006 "Parser/parser.cc"
    break;

  case 337:
#line 1566 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-5].en), maybe_build_compound( (yyvsp[-4].sn) ), (yyvsp[-6].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 9012 "Parser/parser.cc"
    break;

  case 338:
#line 1571 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-1].en), (yyvsp[0].sn), (yyvsp[-2].en) ) ); }
#line 9018 "Parser/parser.cc"
    break;

  case 339:
#line 1573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-3].en), (yyvsp[-2].sn), (yyvsp[-4].en), (yyvsp[0].wfs) ) ); }
#line 9024 "Parser/parser.cc"
    break;

  case 340:
#line 1578 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), (yyvsp[0].sn), 0 ) ); }
#line 9030 "Parser/parser.cc"
    break;

  case 341:
#line 1580 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), 0, (yyvsp[0].sn) ) ); }
#line 9036 "Parser/parser.cc"
    break;

  case 342:
#line 1582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-2].sn), (yyvsp[-1].sn), (yyvsp[0].sn) ) ); }
#line 9042 "Parser/parser.cc"
    break;

  case 343:
#line 1587 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 9048 "Parser/parser.cc"
    break;

  case 344:
#line 1589 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)(yyvsp[-8].sn)->set_last( new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ) ); }
#line 9054 "Parser/parser.cc"
    break;

  case 345:
#line 1594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9060 "Parser/parser.cc"
    break;

  case 346:
#line 1595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[0].en); }
#line 9066 "Parser/parser.cc"
    break;

  case 347:
#line 1599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 9072 "Parser/parser.cc"
    break;

  case 348:
#line 1600 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 9078 "Parser/parser.cc"
    break;

  case 349:
#line 1601 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.catch_kind) = CatchStmt::Resume; }
#line 9084 "Parser/parser.cc"
    break;

  case 350:
#line 1602 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Resume; }
#line 9090 "Parser/parser.cc"
    break;

  case 351:
#line 1606 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = new StatementNode( build_finally( (yyvsp[0].sn) ) ); }
#line 9096 "Parser/parser.cc"
    break;

  case 353:
#line 1613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 9102 "Parser/parser.cc"
    break;

  case 354:
#line 1615 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 9108 "Parser/parser.cc"
    break;

  case 355:
#line 1617 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 9114 "Parser/parser.cc"
    break;

  case 360:
#line 1632 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-4].flag), (yyvsp[-2].constant), 0 ) ); }
#line 9120 "Parser/parser.cc"
    break;

  case 361:
#line 1634 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-6].flag), (yyvsp[-4].constant), (yyvsp[-2].en) ) ); }
#line 9126 "Parser/parser.cc"
    break;

  case 362:
#line 1636 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-8].flag), (yyvsp[-6].constant), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 9132 "Parser/parser.cc"
    break;

  case 363:
#line 1638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-10].flag), (yyvsp[-8].constant), (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 9138 "Parser/parser.cc"
    break;

  case 364:
#line 1640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-12].flag), (yyvsp[-9].constant), 0, (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].label) ) ); }
#line 9144 "Parser/parser.cc"
    break;

  case 365:
#line 1645 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = false; }
#line 9150 "Parser/parser.cc"
    break;

  case 366:
#line 1647 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = true; }
#line 9156 "Parser/parser.cc"
    break;

  case 367:
#line 1652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9162 "Parser/parser.cc"
    break;

  case 370:
#line 1659 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 9168 "Parser/parser.cc"
    break;

  case 371:
#line 1664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( nullptr, (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 9174 "Parser/parser.cc"
    break;

  case 372:
#line 1666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( (yyvsp[-5].tok), (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 9180 "Parser/parser.cc"
    break;

  case 373:
#line 1671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9186 "Parser/parser.cc"
    break;

  case 374:
#line 1673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 9192 "Parser/parser.cc"
    break;

  case 375:
#line 1675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( (yyvsp[0].constant) ) )); }
#line 9198 "Parser/parser.cc"
    break;

  case 376:
#line 1680 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = new LabelNode(); (yyval.label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 9207 "Parser/parser.cc"
    break;

  case 377:
#line 1685 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = (yyvsp[-2].label); (yyvsp[-2].label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 9216 "Parser/parser.cc"
    break;

  case 378:
#line 1695 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9222 "Parser/parser.cc"
    break;

  case 381:
#line 1702 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 9228 "Parser/parser.cc"
    break;

  case 382:
#line 1707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9234 "Parser/parser.cc"
    break;

  case 384:
#line 1713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 9240 "Parser/parser.cc"
    break;

  case 385:
#line 1715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-2].decl) ); }
#line 9246 "Parser/parser.cc"
    break;

  case 392:
#line 1735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "C_DECLARATION1 %p %s\n", $$, $$->name ? $$->name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 9257 "Parser/parser.cc"
    break;

  case 395:
#line 1747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-4].en), (yyvsp[-2].constant) ); }
#line 9263 "Parser/parser.cc"
    break;

  case 396:
#line 1749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-2].en), build_constantStr( *new string( "\"\"" ) ) ); }
#line 9269 "Parser/parser.cc"
    break;

  case 400:
#line 1767 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9275 "Parser/parser.cc"
    break;

  case 402:
#line 1773 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].in) ); }
#line 9281 "Parser/parser.cc"
    break;

  case 403:
#line 1777 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 9287 "Parser/parser.cc"
    break;

  case 404:
#line 1779 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].in) ) ); }
#line 9293 "Parser/parser.cc"
    break;

  case 405:
#line 1786 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 9299 "Parser/parser.cc"
    break;

  case 406:
#line 1788 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 9305 "Parser/parser.cc"
    break;

  case 407:
#line 1790 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 9311 "Parser/parser.cc"
    break;

  case 409:
#line 1796 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9317 "Parser/parser.cc"
    break;

  case 410:
#line 1798 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9323 "Parser/parser.cc"
    break;

  case 411:
#line 1800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 9329 "Parser/parser.cc"
    break;

  case 412:
#line 1802 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeClone( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->appendList( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 9340 "Parser/parser.cc"
    break;

  case 413:
#line 1835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), 0 )->addQualifiers( (yyvsp[0].decl) ); }
#line 9346 "Parser/parser.cc"
    break;

  case 414:
#line 1837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), 0 )->addQualifiers( (yyvsp[0].decl) ); }
#line 9352 "Parser/parser.cc"
    break;

  case 415:
#line 1842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 9358 "Parser/parser.cc"
    break;

  case 416:
#line 1845 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-6].decl)->appendList( (yyvsp[-2].decl) ) ); }
#line 9364 "Parser/parser.cc"
    break;

  case 417:
#line 1850 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 9373 "Parser/parser.cc"
    break;

  case 418:
#line 1855 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 9382 "Parser/parser.cc"
    break;

  case 419:
#line 1860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "3" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 9391 "Parser/parser.cc"
    break;

  case 420:
#line 1871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "4" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef();
		}
#line 9400 "Parser/parser.cc"
    break;

  case 421:
#line 1876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "5" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 9409 "Parser/parser.cc"
    break;

  case 422:
#line 1881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "6" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[-3].decl) )->addTypedef();
		}
#line 9418 "Parser/parser.cc"
    break;

  case 423:
#line 1886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "7" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-2].decl) )->addTypedef();
		}
#line 9427 "Parser/parser.cc"
    break;

  case 424:
#line 1891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "8" );
			(yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-3].decl) )->addTypedef()->addType( (yyvsp[-3].decl) );
		}
#line 9436 "Parser/parser.cc"
    break;

  case 425:
#line 1900 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 9444 "Parser/parser.cc"
    break;

  case 426:
#line 1904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 9452 "Parser/parser.cc"
    break;

  case 427:
#line 1911 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 9458 "Parser/parser.cc"
    break;

  case 431:
#line 1921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 9464 "Parser/parser.cc"
    break;

  case 432:
#line 1923 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ) ); }
#line 9470 "Parser/parser.cc"
    break;

  case 440:
#line 1946 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_type_specifier2 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 9481 "Parser/parser.cc"
    break;

  case 445:
#line 1968 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9487 "Parser/parser.cc"
    break;

  case 448:
#line 1980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9493 "Parser/parser.cc"
    break;

  case 451:
#line 1990 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Const ); }
#line 9499 "Parser/parser.cc"
    break;

  case 452:
#line 1992 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Restrict ); }
#line 9505 "Parser/parser.cc"
    break;

  case 453:
#line 1994 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Volatile ); }
#line 9511 "Parser/parser.cc"
    break;

  case 454:
#line 1996 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Atomic ); }
#line 9517 "Parser/parser.cc"
    break;

  case 456:
#line 2002 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newForall( (yyvsp[-1].decl) ); }
#line 9523 "Parser/parser.cc"
    break;

  case 458:
#line 2008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9529 "Parser/parser.cc"
    break;

  case 459:
#line 2010 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9535 "Parser/parser.cc"
    break;

  case 461:
#line 2021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9541 "Parser/parser.cc"
    break;

  case 462:
#line 2026 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Extern ); }
#line 9547 "Parser/parser.cc"
    break;

  case 463:
#line 2028 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Static ); }
#line 9553 "Parser/parser.cc"
    break;

  case 464:
#line 2030 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Auto ); }
#line 9559 "Parser/parser.cc"
    break;

  case 465:
#line 2032 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Register ); }
#line 9565 "Parser/parser.cc"
    break;

  case 466:
#line 2034 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Threadlocal ); }
#line 9571 "Parser/parser.cc"
    break;

  case 467:
#line 2037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Inline ); }
#line 9577 "Parser/parser.cc"
    break;

  case 468:
#line 2039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Fortran ); }
#line 9583 "Parser/parser.cc"
    break;

  case 469:
#line 2041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Noreturn ); }
#line 9589 "Parser/parser.cc"
    break;

  case 470:
#line 2046 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 9595 "Parser/parser.cc"
    break;

  case 471:
#line 2048 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Bool ); }
#line 9601 "Parser/parser.cc"
    break;

  case 472:
#line 2050 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Char ); }
#line 9607 "Parser/parser.cc"
    break;

  case 473:
#line 2052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int ); }
#line 9613 "Parser/parser.cc"
    break;

  case 474:
#line 2054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 ); }
#line 9619 "Parser/parser.cc"
    break;

  case 475:
#line 2056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 )->addType( DeclarationNode::newSignedNess( DeclarationNode::Unsigned ) ); }
#line 9625 "Parser/parser.cc"
    break;

  case 476:
#line 2058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Float ); }
#line 9631 "Parser/parser.cc"
    break;

  case 477:
#line 2060 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Double ); }
#line 9637 "Parser/parser.cc"
    break;

  case 478:
#line 2062 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat80 ); }
#line 9643 "Parser/parser.cc"
    break;

  case 479:
#line 2064 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat128 ); }
#line 9649 "Parser/parser.cc"
    break;

  case 480:
#line 2066 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat16 ); }
#line 9655 "Parser/parser.cc"
    break;

  case 481:
#line 2068 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32 ); }
#line 9661 "Parser/parser.cc"
    break;

  case 482:
#line 2070 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32x ); }
#line 9667 "Parser/parser.cc"
    break;

  case 483:
#line 2072 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64 ); }
#line 9673 "Parser/parser.cc"
    break;

  case 484:
#line 2074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64x ); }
#line 9679 "Parser/parser.cc"
    break;

  case 485:
#line 2076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat128 ); }
#line 9685 "Parser/parser.cc"
    break;

  case 486:
#line 2078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9691 "Parser/parser.cc"
    break;

  case 487:
#line 2080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9697 "Parser/parser.cc"
    break;

  case 488:
#line 2082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9703 "Parser/parser.cc"
    break;

  case 489:
#line 2084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Complex ); }
#line 9709 "Parser/parser.cc"
    break;

  case 490:
#line 2086 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Imaginary ); }
#line 9715 "Parser/parser.cc"
    break;

  case 491:
#line 2088 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Signed ); }
#line 9721 "Parser/parser.cc"
    break;

  case 492:
#line 2090 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Unsigned ); }
#line 9727 "Parser/parser.cc"
    break;

  case 493:
#line 2092 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Short ); }
#line 9733 "Parser/parser.cc"
    break;

  case 494:
#line 2094 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Long ); }
#line 9739 "Parser/parser.cc"
    break;

  case 495:
#line 2096 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Valist ); }
#line 9745 "Parser/parser.cc"
    break;

  case 496:
#line 2098 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::AutoType ); }
#line 9751 "Parser/parser.cc"
    break;

  case 498:
#line 2104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9757 "Parser/parser.cc"
    break;

  case 500:
#line 2110 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVtableType( (yyvsp[-2].decl) ); }
#line 9763 "Parser/parser.cc"
    break;

  case 501:
#line 2116 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9769 "Parser/parser.cc"
    break;

  case 502:
#line 2118 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9775 "Parser/parser.cc"
    break;

  case 504:
#line 2125 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9781 "Parser/parser.cc"
    break;

  case 505:
#line 2127 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9787 "Parser/parser.cc"
    break;

  case 506:
#line 2129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9793 "Parser/parser.cc"
    break;

  case 507:
#line 2131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 9799 "Parser/parser.cc"
    break;

  case 509:
#line 2138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9805 "Parser/parser.cc"
    break;

  case 511:
#line 2144 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9811 "Parser/parser.cc"
    break;

  case 512:
#line 2146 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9817 "Parser/parser.cc"
    break;

  case 513:
#line 2148 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 9823 "Parser/parser.cc"
    break;

  case 514:
#line 2153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 9829 "Parser/parser.cc"
    break;

  case 515:
#line 2155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en) ); }
#line 9835 "Parser/parser.cc"
    break;

  case 516:
#line 2157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 9841 "Parser/parser.cc"
    break;

  case 517:
#line 2159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en), true ); }
#line 9847 "Parser/parser.cc"
    break;

  case 518:
#line 2161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Zero ); }
#line 9853 "Parser/parser.cc"
    break;

  case 519:
#line 2163 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::One ); }
#line 9859 "Parser/parser.cc"
    break;

  case 520:
#line 2168 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_declaration_specifier %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 9870 "Parser/parser.cc"
    break;

  case 521:
#line 2175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9876 "Parser/parser.cc"
    break;

  case 522:
#line 2177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9882 "Parser/parser.cc"
    break;

  case 523:
#line 2179 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9888 "Parser/parser.cc"
    break;

  case 524:
#line 2184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_type_specifier %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 9899 "Parser/parser.cc"
    break;

  case 525:
#line 2191 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 9905 "Parser/parser.cc"
    break;

  case 526:
#line 2193 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 9911 "Parser/parser.cc"
    break;

  case 527:
#line 2195 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 9920 "Parser/parser.cc"
    break;

  case 529:
#line 2204 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9926 "Parser/parser.cc"
    break;

  case 530:
#line 2206 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9932 "Parser/parser.cc"
    break;

  case 531:
#line 2208 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9938 "Parser/parser.cc"
    break;

  case 533:
#line 2214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9944 "Parser/parser.cc"
    break;

  case 534:
#line 2216 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9950 "Parser/parser.cc"
    break;

  case 536:
#line 2222 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9956 "Parser/parser.cc"
    break;

  case 537:
#line 2224 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9962 "Parser/parser.cc"
    break;

  case 538:
#line 2226 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9968 "Parser/parser.cc"
    break;

  case 540:
#line 2232 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9974 "Parser/parser.cc"
    break;

  case 541:
#line 2234 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9980 "Parser/parser.cc"
    break;

  case 542:
#line 2239 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypedef( (yyvsp[0].tok) ); }
#line 9986 "Parser/parser.cc"
    break;

  case 543:
#line 2241 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 9992 "Parser/parser.cc"
    break;

  case 544:
#line 2243 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 9998 "Parser/parser.cc"
    break;

  case 546:
#line 2246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), (yyvsp[0].decl) ); }
#line 10004 "Parser/parser.cc"
    break;

  case 547:
#line 2248 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), (yyvsp[0].decl) ); }
#line 10010 "Parser/parser.cc"
    break;

  case 548:
#line 2253 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[0].tok), nullptr ); }
#line 10016 "Parser/parser.cc"
    break;

  case 549:
#line 2255 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-2].tok), nullptr ); }
#line 10022 "Parser/parser.cc"
    break;

  case 550:
#line 2257 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 10028 "Parser/parser.cc"
    break;

  case 551:
#line 2262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "elaborated_type %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10039 "Parser/parser.cc"
    break;

  case 555:
#line 2278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 10045 "Parser/parser.cc"
    break;

  case 556:
#line 2280 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 10051 "Parser/parser.cc"
    break;

  case 557:
#line 2282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 10060 "Parser/parser.cc"
    break;

  case 558:
#line 2287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 10077 "Parser/parser.cc"
    break;

  case 559:
#line 2300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 10086 "Parser/parser.cc"
    break;

  case 560:
#line 2305 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "AGG3\n" );
			DeclarationNode::newFromTypedef( (yyvsp[-5].tok) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 10096 "Parser/parser.cc"
    break;

  case 561:
#line 2311 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 10105 "Parser/parser.cc"
    break;

  case 562:
#line 2316 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "AGG4\n" );
			DeclarationNode::newFromTypeGen( (yyvsp[-5].tok), nullptr );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 10115 "Parser/parser.cc"
    break;

  case 564:
#line 2326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 10121 "Parser/parser.cc"
    break;

  case 565:
#line 2328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 10127 "Parser/parser.cc"
    break;

  case 566:
#line 2333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 10137 "Parser/parser.cc"
    break;

  case 567:
#line 2339 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 10152 "Parser/parser.cc"
    break;

  case 570:
#line 2358 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Struct; }
#line 10158 "Parser/parser.cc"
    break;

  case 571:
#line 2360 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Union; }
#line 10164 "Parser/parser.cc"
    break;

  case 572:
#line 2362 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Exception; }
#line 10170 "Parser/parser.cc"
    break;

  case 573:
#line 2368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 10176 "Parser/parser.cc"
    break;

  case 574:
#line 2370 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 10182 "Parser/parser.cc"
    break;

  case 575:
#line 2372 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Generator; }
#line 10188 "Parser/parser.cc"
    break;

  case 576:
#line 2374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor generator is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 10194 "Parser/parser.cc"
    break;

  case 577:
#line 2376 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Coroutine; }
#line 10200 "Parser/parser.cc"
    break;

  case 578:
#line 2378 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor coroutine is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 10206 "Parser/parser.cc"
    break;

  case 579:
#line 2380 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Thread; }
#line 10212 "Parser/parser.cc"
    break;

  case 580:
#line 2382 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor thread is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 10218 "Parser/parser.cc"
    break;

  case 581:
#line 2387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10224 "Parser/parser.cc"
    break;

  case 582:
#line 2389 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 10230 "Parser/parser.cc"
    break;

  case 583:
#line 2394 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "type_specifier1 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			(yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) );
			// printf( "type_specifier2 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10243 "Parser/parser.cc"
    break;

  case 584:
#line 2403 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 10249 "Parser/parser.cc"
    break;

  case 585:
#line 2405 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-1].decl) ) {								// field declarator ?
				(yyvsp[-1].decl) = DeclarationNode::newName( nullptr );
			} // if
			(yyvsp[-1].decl)->inLine = true;
			(yyval.decl) = distAttr( (yyvsp[-2].decl), (yyvsp[-1].decl) );					// mark all fields in list
			distInl( (yyvsp[-1].decl) );
		}
#line 10262 "Parser/parser.cc"
    break;

  case 586:
#line 2414 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10268 "Parser/parser.cc"
    break;

  case 589:
#line 2418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 10274 "Parser/parser.cc"
    break;

  case 590:
#line 2420 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10280 "Parser/parser.cc"
    break;

  case 593:
#line 2427 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10286 "Parser/parser.cc"
    break;

  case 595:
#line 2430 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 10292 "Parser/parser.cc"
    break;

  case 596:
#line 2435 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].en) ); }
#line 10298 "Parser/parser.cc"
    break;

  case 597:
#line 2438 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 10304 "Parser/parser.cc"
    break;

  case 598:
#line 2441 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 10310 "Parser/parser.cc"
    break;

  case 599:
#line 2446 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10316 "Parser/parser.cc"
    break;

  case 601:
#line 2449 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 10322 "Parser/parser.cc"
    break;

  case 603:
#line 2460 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 10328 "Parser/parser.cc"
    break;

  case 604:
#line 2462 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 10334 "Parser/parser.cc"
    break;

  case 606:
#line 2469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 10340 "Parser/parser.cc"
    break;

  case 607:
#line 2474 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 10346 "Parser/parser.cc"
    break;

  case 609:
#line 2480 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 10352 "Parser/parser.cc"
    break;

  case 610:
#line 2485 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10358 "Parser/parser.cc"
    break;

  case 611:
#line 2487 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); }
#line 10364 "Parser/parser.cc"
    break;

  case 612:
#line 2489 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].tok), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) ); }
#line 10370 "Parser/parser.cc"
    break;

  case 613:
#line 2492 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-4].decl)->name, (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 10376 "Parser/parser.cc"
    break;

  case 614:
#line 2494 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Unvalued enumerated type is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10382 "Parser/parser.cc"
    break;

  case 615:
#line 2496 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-6].decl)->storageClasses.val != 0 || (yyvsp[-6].decl)->type->qualifiers.val != 0 )
			{ SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }

			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 10393 "Parser/parser.cc"
    break;

  case 616:
#line 2503 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].decl)->storageClasses.val != 0 || (yyvsp[-4].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			typedefTable.makeTypedef( *(yyvsp[-1].tok) );
		}
#line 10402 "Parser/parser.cc"
    break;

  case 617:
#line 2508 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, (yyvsp[-9].decl) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 10410 "Parser/parser.cc"
    break;

  case 618:
#line 2512 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-8].decl)->storageClasses.val != 0 || (yyvsp[-8].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			typedefTable.makeTypedef( *(yyvsp[-5].decl)->name );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].decl)->name, (yyvsp[-2].decl), true, (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 10420 "Parser/parser.cc"
    break;

  case 620:
#line 2522 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); (yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), 0, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 10426 "Parser/parser.cc"
    break;

  case 621:
#line 2524 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].decl)->type->symbolic.name );	(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].decl)->type->symbolic.name, 0, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 10432 "Parser/parser.cc"
    break;

  case 622:
#line 2529 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].in) ); }
#line 10438 "Parser/parser.cc"
    break;

  case 623:
#line 2531 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( new string("inline"), nullptr ); }
#line 10444 "Parser/parser.cc"
    break;

  case 624:
#line 2533 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].in) ) ); }
#line 10450 "Parser/parser.cc"
    break;

  case 625:
#line 2535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( DeclarationNode::newEnumValueGeneric( new string("inline"), nullptr ) ); }
#line 10456 "Parser/parser.cc"
    break;

  case 626:
#line 2540 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 10462 "Parser/parser.cc"
    break;

  case 627:
#line 2541 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = new InitializerNode( (yyvsp[0].en) ); }
#line 10468 "Parser/parser.cc"
    break;

  case 628:
#line 2542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                     { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 10474 "Parser/parser.cc"
    break;

  case 629:
#line 2549 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 10480 "Parser/parser.cc"
    break;

  case 630:
#line 2551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10486 "Parser/parser.cc"
    break;

  case 633:
#line 2555 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10492 "Parser/parser.cc"
    break;

  case 634:
#line 2557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 10498 "Parser/parser.cc"
    break;

  case 635:
#line 2559 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 10504 "Parser/parser.cc"
    break;

  case 637:
#line 2567 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10510 "Parser/parser.cc"
    break;

  case 638:
#line 2569 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10516 "Parser/parser.cc"
    break;

  case 639:
#line 2571 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-8].decl)->appendList( (yyvsp[-4].decl) )->appendList( (yyvsp[0].decl) ); }
#line 10522 "Parser/parser.cc"
    break;

  case 641:
#line 2577 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10528 "Parser/parser.cc"
    break;

  case 642:
#line 2582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10534 "Parser/parser.cc"
    break;

  case 643:
#line 2584 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10540 "Parser/parser.cc"
    break;

  case 645:
#line 2587 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 10546 "Parser/parser.cc"
    break;

  case 648:
#line 2594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10552 "Parser/parser.cc"
    break;

  case 649:
#line 2596 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10558 "Parser/parser.cc"
    break;

  case 651:
#line 2605 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 10564 "Parser/parser.cc"
    break;

  case 652:
#line 2608 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 10570 "Parser/parser.cc"
    break;

  case 653:
#line 2610 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 10576 "Parser/parser.cc"
    break;

  case 658:
#line 2620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10582 "Parser/parser.cc"
    break;

  case 660:
#line 2627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 10588 "Parser/parser.cc"
    break;

  case 661:
#line 2629 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 10594 "Parser/parser.cc"
    break;

  case 662:
#line 2634 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 10600 "Parser/parser.cc"
    break;

  case 663:
#line 2636 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 10606 "Parser/parser.cc"
    break;

  case 664:
#line 2645 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 10612 "Parser/parser.cc"
    break;

  case 665:
#line 2647 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 10618 "Parser/parser.cc"
    break;

  case 671:
#line 2660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10624 "Parser/parser.cc"
    break;

  case 674:
#line 2670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 10630 "Parser/parser.cc"
    break;

  case 675:
#line 2671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (yyvsp[-1].op) == OperKinds::Assign ? (yyvsp[0].in) : (yyvsp[0].in)->set_maybeConstructed( false ); }
#line 10636 "Parser/parser.cc"
    break;

  case 676:
#line 2672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.in) = new InitializerNode( true ); }
#line 10642 "Parser/parser.cc"
    break;

  case 677:
#line 2673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 10648 "Parser/parser.cc"
    break;

  case 678:
#line 2677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = new InitializerNode( (yyvsp[0].en) ); }
#line 10654 "Parser/parser.cc"
    break;

  case 679:
#line 2678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 10660 "Parser/parser.cc"
    break;

  case 680:
#line 2683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 10666 "Parser/parser.cc"
    break;

  case 682:
#line 2685 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ); }
#line 10672 "Parser/parser.cc"
    break;

  case 683:
#line 2686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (InitializerNode *)( (yyvsp[-2].in)->set_last( (yyvsp[0].in) ) ); }
#line 10678 "Parser/parser.cc"
    break;

  case 684:
#line 2687 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.in) = (InitializerNode *)((yyvsp[-3].in)->set_last( (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ) )); }
#line 10684 "Parser/parser.cc"
    break;

  case 686:
#line 2703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[-1].tok) ) ); }
#line 10690 "Parser/parser.cc"
    break;

  case 688:
#line 2709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-1].en)->set_last( (yyvsp[0].en) )); }
#line 10696 "Parser/parser.cc"
    break;

  case 689:
#line 2715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 10702 "Parser/parser.cc"
    break;

  case 690:
#line 2718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 10708 "Parser/parser.cc"
    break;

  case 691:
#line 2720 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 10714 "Parser/parser.cc"
    break;

  case 692:
#line 2722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-4].en) ), maybeMoveBuild<Expression>( (yyvsp[-2].en) ) ) ); }
#line 10720 "Parser/parser.cc"
    break;

  case 693:
#line 2724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 10726 "Parser/parser.cc"
    break;

  case 695:
#line 2748 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl) ); }
#line 10732 "Parser/parser.cc"
    break;

  case 696:
#line 2753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10738 "Parser/parser.cc"
    break;

  case 697:
#line 2755 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 10744 "Parser/parser.cc"
    break;

  case 698:
#line 2760 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "9" );
			if ( (yyvsp[-1].tclass) == TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 10755 "Parser/parser.cc"
    break;

  case 699:
#line 2767 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 10761 "Parser/parser.cc"
    break;

  case 700:
#line 2769 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "9" ); }
#line 10767 "Parser/parser.cc"
    break;

  case 701:
#line 2771 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 10773 "Parser/parser.cc"
    break;

  case 702:
#line 2773 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "9" );
			(yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 10782 "Parser/parser.cc"
    break;

  case 703:
#line 2779 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 10788 "Parser/parser.cc"
    break;

  case 704:
#line 2784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 10794 "Parser/parser.cc"
    break;

  case 705:
#line 2786 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 10800 "Parser/parser.cc"
    break;

  case 706:
#line 2788 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::DStype; }
#line 10806 "Parser/parser.cc"
    break;

  case 707:
#line 2792 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 10812 "Parser/parser.cc"
    break;

  case 708:
#line 2797 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 10818 "Parser/parser.cc"
    break;

  case 709:
#line 2799 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 10824 "Parser/parser.cc"
    break;

  case 710:
#line 2801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ftype; }
#line 10830 "Parser/parser.cc"
    break;

  case 711:
#line 2803 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 10836 "Parser/parser.cc"
    break;

  case 712:
#line 2808 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10842 "Parser/parser.cc"
    break;

  case 715:
#line 2815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 10848 "Parser/parser.cc"
    break;

  case 716:
#line 2820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 10854 "Parser/parser.cc"
    break;

  case 717:
#line 2822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 10860 "Parser/parser.cc"
    break;

  case 718:
#line 2829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 10866 "Parser/parser.cc"
    break;

  case 720:
#line 2832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 10872 "Parser/parser.cc"
    break;

  case 721:
#line 2834 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)( (yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 10878 "Parser/parser.cc"
    break;

  case 722:
#line 2839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 10884 "Parser/parser.cc"
    break;

  case 723:
#line 2841 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10890 "Parser/parser.cc"
    break;

  case 724:
#line 2843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 10896 "Parser/parser.cc"
    break;

  case 725:
#line 2848 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 10902 "Parser/parser.cc"
    break;

  case 726:
#line 2850 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 10908 "Parser/parser.cc"
    break;

  case 727:
#line 2855 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "10" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), 0 );
		}
#line 10917 "Parser/parser.cc"
    break;

  case 728:
#line 2860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "11" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 10926 "Parser/parser.cc"
    break;

  case 729:
#line 2868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), 0 ); }
#line 10932 "Parser/parser.cc"
    break;

  case 730:
#line 2870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-8].tok), (yyvsp[-6].decl), (yyvsp[-2].decl) ); }
#line 10938 "Parser/parser.cc"
    break;

  case 732:
#line 2876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl) ); }
#line 10944 "Parser/parser.cc"
    break;

  case 737:
#line 2888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 10950 "Parser/parser.cc"
    break;

  case 738:
#line 2893 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10956 "Parser/parser.cc"
    break;

  case 739:
#line 2895 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 10962 "Parser/parser.cc"
    break;

  case 741:
#line 2903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl);	}
#line 10968 "Parser/parser.cc"
    break;

  case 742:
#line 2908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10974 "Parser/parser.cc"
    break;

  case 743:
#line 2910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->appendList( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 10980 "Parser/parser.cc"
    break;

  case 744:
#line 2915 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10986 "Parser/parser.cc"
    break;

  case 746:
#line 2920 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 10992 "Parser/parser.cc"
    break;

  case 747:
#line 2924 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 10998 "Parser/parser.cc"
    break;

  case 748:
#line 2929 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( (yyvsp[0].tok) ) ) ); }
#line 11004 "Parser/parser.cc"
    break;

  case 751:
#line 2933 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 11013 "Parser/parser.cc"
    break;

  case 752:
#line 2938 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( false, (yyvsp[-2].constant), 0 ) ) ); }
#line 11019 "Parser/parser.cc"
    break;

  case 753:
#line 2940 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = LinkageSpec::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 11028 "Parser/parser.cc"
    break;

  case 754:
#line 2945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-1].decl);
		}
#line 11038 "Parser/parser.cc"
    break;

  case 755:
#line 2951 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = LinkageSpec::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 11047 "Parser/parser.cc"
    break;

  case 756:
#line 2956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11057 "Parser/parser.cc"
    break;

  case 757:
#line 2962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 11066 "Parser/parser.cc"
    break;

  case 758:
#line 2967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11076 "Parser/parser.cc"
    break;

  case 759:
#line 2973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 11085 "Parser/parser.cc"
    break;

  case 760:
#line 2978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11095 "Parser/parser.cc"
    break;

  case 761:
#line 2984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.val) || (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 11104 "Parser/parser.cc"
    break;

  case 762:
#line 2989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11114 "Parser/parser.cc"
    break;

  case 764:
#line 3004 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].sn) ); }
#line 11120 "Parser/parser.cc"
    break;

  case 765:
#line 3006 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].sn) ); }
#line 11126 "Parser/parser.cc"
    break;

  case 766:
#line 3011 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; forall = false; }
#line 11132 "Parser/parser.cc"
    break;

  case 767:
#line 3013 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.en) = (yyvsp[-2].en); forall = false;
			if ( (yyvsp[0].decl) ) {
				SemanticError( yylloc, "Attributes cannot be associated with function body. Move attribute(s) before \"with\" clause." );
				(yyval.en) = nullptr;
			} // if
		}
#line 11144 "Parser/parser.cc"
    break;

  case 768:
#line 3024 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11155 "Parser/parser.cc"
    break;

  case 769:
#line 3031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 11164 "Parser/parser.cc"
    break;

  case 770:
#line 3036 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 11173 "Parser/parser.cc"
    break;

  case 771:
#line 3042 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 11179 "Parser/parser.cc"
    break;

  case 772:
#line 3045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 11185 "Parser/parser.cc"
    break;

  case 773:
#line 3048 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 11191 "Parser/parser.cc"
    break;

  case 774:
#line 3052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-4].decl) );
		}
#line 11200 "Parser/parser.cc"
    break;

  case 775:
#line 3058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 11206 "Parser/parser.cc"
    break;

  case 776:
#line 3061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 11212 "Parser/parser.cc"
    break;

  case 777:
#line 3064 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 11218 "Parser/parser.cc"
    break;

  case 781:
#line 3075 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 11224 "Parser/parser.cc"
    break;

  case 782:
#line 3080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11230 "Parser/parser.cc"
    break;

  case 783:
#line 3082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = (yyvsp[-2].constant);
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 11240 "Parser/parser.cc"
    break;

  case 784:
#line 3091 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11246 "Parser/parser.cc"
    break;

  case 787:
#line 3098 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11252 "Parser/parser.cc"
    break;

  case 788:
#line 3103 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 11258 "Parser/parser.cc"
    break;

  case 790:
#line 3109 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11264 "Parser/parser.cc"
    break;

  case 791:
#line 3114 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11270 "Parser/parser.cc"
    break;

  case 792:
#line 3116 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 11276 "Parser/parser.cc"
    break;

  case 793:
#line 3118 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 11282 "Parser/parser.cc"
    break;

  case 798:
#line 3127 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 11288 "Parser/parser.cc"
    break;

  case 799:
#line 3129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 11294 "Parser/parser.cc"
    break;

  case 800:
#line 3164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11300 "Parser/parser.cc"
    break;

  case 801:
#line 3166 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11306 "Parser/parser.cc"
    break;

  case 802:
#line 3171 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11312 "Parser/parser.cc"
    break;

  case 804:
#line 3174 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11318 "Parser/parser.cc"
    break;

  case 805:
#line 3176 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11324 "Parser/parser.cc"
    break;

  case 806:
#line 3181 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11330 "Parser/parser.cc"
    break;

  case 807:
#line 3183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11336 "Parser/parser.cc"
    break;

  case 808:
#line 3185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11342 "Parser/parser.cc"
    break;

  case 809:
#line 3187 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11348 "Parser/parser.cc"
    break;

  case 810:
#line 3192 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11354 "Parser/parser.cc"
    break;

  case 811:
#line 3194 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11360 "Parser/parser.cc"
    break;

  case 812:
#line 3196 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11366 "Parser/parser.cc"
    break;

  case 813:
#line 3198 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11372 "Parser/parser.cc"
    break;

  case 814:
#line 3200 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11378 "Parser/parser.cc"
    break;

  case 815:
#line 3202 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11384 "Parser/parser.cc"
    break;

  case 816:
#line 3204 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11390 "Parser/parser.cc"
    break;

  case 817:
#line 3209 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11396 "Parser/parser.cc"
    break;

  case 818:
#line 3211 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 11402 "Parser/parser.cc"
    break;

  case 819:
#line 3213 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11408 "Parser/parser.cc"
    break;

  case 820:
#line 3215 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11414 "Parser/parser.cc"
    break;

  case 821:
#line 3224 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11420 "Parser/parser.cc"
    break;

  case 823:
#line 3227 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11426 "Parser/parser.cc"
    break;

  case 824:
#line 3232 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11432 "Parser/parser.cc"
    break;

  case 825:
#line 3234 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11438 "Parser/parser.cc"
    break;

  case 826:
#line 3236 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 11444 "Parser/parser.cc"
    break;

  case 827:
#line 3238 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11450 "Parser/parser.cc"
    break;

  case 828:
#line 3240 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11456 "Parser/parser.cc"
    break;

  case 829:
#line 3245 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11462 "Parser/parser.cc"
    break;

  case 830:
#line 3247 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11468 "Parser/parser.cc"
    break;

  case 831:
#line 3249 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11474 "Parser/parser.cc"
    break;

  case 832:
#line 3251 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11480 "Parser/parser.cc"
    break;

  case 833:
#line 3256 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11486 "Parser/parser.cc"
    break;

  case 834:
#line 3258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11492 "Parser/parser.cc"
    break;

  case 835:
#line 3260 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11498 "Parser/parser.cc"
    break;

  case 836:
#line 3262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11504 "Parser/parser.cc"
    break;

  case 837:
#line 3264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11510 "Parser/parser.cc"
    break;

  case 838:
#line 3266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11516 "Parser/parser.cc"
    break;

  case 842:
#line 3284 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 11522 "Parser/parser.cc"
    break;

  case 843:
#line 3286 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11528 "Parser/parser.cc"
    break;

  case 844:
#line 3288 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 11534 "Parser/parser.cc"
    break;

  case 845:
#line 3290 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11540 "Parser/parser.cc"
    break;

  case 846:
#line 3292 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11546 "Parser/parser.cc"
    break;

  case 847:
#line 3297 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11552 "Parser/parser.cc"
    break;

  case 848:
#line 3299 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11558 "Parser/parser.cc"
    break;

  case 849:
#line 3301 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11564 "Parser/parser.cc"
    break;

  case 850:
#line 3303 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11570 "Parser/parser.cc"
    break;

  case 851:
#line 3308 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11576 "Parser/parser.cc"
    break;

  case 852:
#line 3310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11582 "Parser/parser.cc"
    break;

  case 853:
#line 3312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11588 "Parser/parser.cc"
    break;

  case 854:
#line 3314 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11594 "Parser/parser.cc"
    break;

  case 855:
#line 3316 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11600 "Parser/parser.cc"
    break;

  case 856:
#line 3318 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11606 "Parser/parser.cc"
    break;

  case 857:
#line 3333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "ID" );
		}
#line 11615 "Parser/parser.cc"
    break;

  case 858:
#line 3338 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11621 "Parser/parser.cc"
    break;

  case 859:
#line 3343 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11627 "Parser/parser.cc"
    break;

  case 861:
#line 3346 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11633 "Parser/parser.cc"
    break;

  case 862:
#line 3348 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11639 "Parser/parser.cc"
    break;

  case 863:
#line 3353 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11645 "Parser/parser.cc"
    break;

  case 864:
#line 3355 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11651 "Parser/parser.cc"
    break;

  case 865:
#line 3357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11657 "Parser/parser.cc"
    break;

  case 866:
#line 3359 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11663 "Parser/parser.cc"
    break;

  case 867:
#line 3364 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11669 "Parser/parser.cc"
    break;

  case 868:
#line 3366 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11675 "Parser/parser.cc"
    break;

  case 869:
#line 3368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11681 "Parser/parser.cc"
    break;

  case 870:
#line 3370 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11687 "Parser/parser.cc"
    break;

  case 871:
#line 3372 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11693 "Parser/parser.cc"
    break;

  case 872:
#line 3374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11699 "Parser/parser.cc"
    break;

  case 873:
#line 3376 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11705 "Parser/parser.cc"
    break;

  case 874:
#line 3381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11711 "Parser/parser.cc"
    break;

  case 875:
#line 3383 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11717 "Parser/parser.cc"
    break;

  case 876:
#line 3385 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 11723 "Parser/parser.cc"
    break;

  case 877:
#line 3387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11729 "Parser/parser.cc"
    break;

  case 878:
#line 3389 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11735 "Parser/parser.cc"
    break;

  case 879:
#line 3399 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11741 "Parser/parser.cc"
    break;

  case 880:
#line 3401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11747 "Parser/parser.cc"
    break;

  case 882:
#line 3404 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11753 "Parser/parser.cc"
    break;

  case 883:
#line 3406 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11759 "Parser/parser.cc"
    break;

  case 884:
#line 3411 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11765 "Parser/parser.cc"
    break;

  case 885:
#line 3413 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11771 "Parser/parser.cc"
    break;

  case 886:
#line 3415 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11777 "Parser/parser.cc"
    break;

  case 887:
#line 3420 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11783 "Parser/parser.cc"
    break;

  case 888:
#line 3422 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11789 "Parser/parser.cc"
    break;

  case 889:
#line 3424 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11795 "Parser/parser.cc"
    break;

  case 890:
#line 3426 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11801 "Parser/parser.cc"
    break;

  case 891:
#line 3431 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11807 "Parser/parser.cc"
    break;

  case 892:
#line 3433 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11813 "Parser/parser.cc"
    break;

  case 893:
#line 3435 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11819 "Parser/parser.cc"
    break;

  case 894:
#line 3449 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11825 "Parser/parser.cc"
    break;

  case 895:
#line 3451 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11831 "Parser/parser.cc"
    break;

  case 897:
#line 3454 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11837 "Parser/parser.cc"
    break;

  case 898:
#line 3456 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11843 "Parser/parser.cc"
    break;

  case 899:
#line 3461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11849 "Parser/parser.cc"
    break;

  case 900:
#line 3463 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11855 "Parser/parser.cc"
    break;

  case 901:
#line 3468 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11861 "Parser/parser.cc"
    break;

  case 902:
#line 3470 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11867 "Parser/parser.cc"
    break;

  case 903:
#line 3472 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11873 "Parser/parser.cc"
    break;

  case 904:
#line 3477 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11879 "Parser/parser.cc"
    break;

  case 905:
#line 3479 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11885 "Parser/parser.cc"
    break;

  case 906:
#line 3484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11891 "Parser/parser.cc"
    break;

  case 907:
#line 3486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11897 "Parser/parser.cc"
    break;

  case 909:
#line 3504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11903 "Parser/parser.cc"
    break;

  case 910:
#line 3506 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11909 "Parser/parser.cc"
    break;

  case 911:
#line 3511 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( 0, (yyvsp[0].op) ); }
#line 11915 "Parser/parser.cc"
    break;

  case 912:
#line 3513 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 11921 "Parser/parser.cc"
    break;

  case 913:
#line 3515 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11927 "Parser/parser.cc"
    break;

  case 914:
#line 3517 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11933 "Parser/parser.cc"
    break;

  case 915:
#line 3519 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11939 "Parser/parser.cc"
    break;

  case 917:
#line 3525 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11945 "Parser/parser.cc"
    break;

  case 918:
#line 3527 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11951 "Parser/parser.cc"
    break;

  case 919:
#line 3529 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11957 "Parser/parser.cc"
    break;

  case 920:
#line 3534 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 11963 "Parser/parser.cc"
    break;

  case 921:
#line 3536 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11969 "Parser/parser.cc"
    break;

  case 922:
#line 3538 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11975 "Parser/parser.cc"
    break;

  case 923:
#line 3544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false ); }
#line 11981 "Parser/parser.cc"
    break;

  case 924:
#line 3546 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false )->addArray( (yyvsp[0].decl) ); }
#line 11987 "Parser/parser.cc"
    break;

  case 925:
#line 3548 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-4].en), 0, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].en), 0, false ) ); }
#line 11993 "Parser/parser.cc"
    break;

  case 927:
#line 3555 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), 0, false ); }
#line 11999 "Parser/parser.cc"
    break;

  case 928:
#line 3557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 12005 "Parser/parser.cc"
    break;

  case 929:
#line 3559 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newArray( (yyvsp[-2].en), 0, false ) ); }
#line 12011 "Parser/parser.cc"
    break;

  case 930:
#line 3561 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 12017 "Parser/parser.cc"
    break;

  case 932:
#line 3596 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 12023 "Parser/parser.cc"
    break;

  case 933:
#line 3598 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12029 "Parser/parser.cc"
    break;

  case 934:
#line 3600 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12035 "Parser/parser.cc"
    break;

  case 935:
#line 3605 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].op) ); }
#line 12041 "Parser/parser.cc"
    break;

  case 936:
#line 3607 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 12047 "Parser/parser.cc"
    break;

  case 937:
#line 3609 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12053 "Parser/parser.cc"
    break;

  case 938:
#line 3611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12059 "Parser/parser.cc"
    break;

  case 939:
#line 3613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12065 "Parser/parser.cc"
    break;

  case 941:
#line 3619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12071 "Parser/parser.cc"
    break;

  case 942:
#line 3621 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12077 "Parser/parser.cc"
    break;

  case 943:
#line 3623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12083 "Parser/parser.cc"
    break;

  case 944:
#line 3628 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 12089 "Parser/parser.cc"
    break;

  case 945:
#line 3630 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12095 "Parser/parser.cc"
    break;

  case 946:
#line 3632 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12101 "Parser/parser.cc"
    break;

  case 948:
#line 3639 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12107 "Parser/parser.cc"
    break;

  case 950:
#line 3650 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false ); }
#line 12113 "Parser/parser.cc"
    break;

  case 951:
#line 3653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 12119 "Parser/parser.cc"
    break;

  case 952:
#line 3655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, (yyvsp[-2].decl), false ); }
#line 12125 "Parser/parser.cc"
    break;

  case 953:
#line 3658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 12131 "Parser/parser.cc"
    break;

  case 954:
#line 3660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 12137 "Parser/parser.cc"
    break;

  case 955:
#line 3662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-4].decl), true ); }
#line 12143 "Parser/parser.cc"
    break;

  case 957:
#line 3677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12149 "Parser/parser.cc"
    break;

  case 958:
#line 3679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12155 "Parser/parser.cc"
    break;

  case 959:
#line 3684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( 0, (yyvsp[0].op) ); }
#line 12161 "Parser/parser.cc"
    break;

  case 960:
#line 3686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 12167 "Parser/parser.cc"
    break;

  case 961:
#line 3688 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12173 "Parser/parser.cc"
    break;

  case 962:
#line 3690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12179 "Parser/parser.cc"
    break;

  case 963:
#line 3692 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12185 "Parser/parser.cc"
    break;

  case 965:
#line 3698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12191 "Parser/parser.cc"
    break;

  case 966:
#line 3700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12197 "Parser/parser.cc"
    break;

  case 967:
#line 3702 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12203 "Parser/parser.cc"
    break;

  case 968:
#line 3707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12209 "Parser/parser.cc"
    break;

  case 969:
#line 3709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12215 "Parser/parser.cc"
    break;

  case 972:
#line 3719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12221 "Parser/parser.cc"
    break;

  case 975:
#line 3730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12227 "Parser/parser.cc"
    break;

  case 976:
#line 3732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12233 "Parser/parser.cc"
    break;

  case 977:
#line 3734 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12239 "Parser/parser.cc"
    break;

  case 978:
#line 3736 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12245 "Parser/parser.cc"
    break;

  case 979:
#line 3738 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12251 "Parser/parser.cc"
    break;

  case 980:
#line 3740 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12257 "Parser/parser.cc"
    break;

  case 981:
#line 3747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 12263 "Parser/parser.cc"
    break;

  case 982:
#line 3749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12269 "Parser/parser.cc"
    break;

  case 983:
#line 3751 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 12275 "Parser/parser.cc"
    break;

  case 984:
#line 3753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 12281 "Parser/parser.cc"
    break;

  case 985:
#line 3755 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12287 "Parser/parser.cc"
    break;

  case 986:
#line 3758 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 12293 "Parser/parser.cc"
    break;

  case 987:
#line 3760 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12299 "Parser/parser.cc"
    break;

  case 988:
#line 3762 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 12305 "Parser/parser.cc"
    break;

  case 989:
#line 3764 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 12311 "Parser/parser.cc"
    break;

  case 990:
#line 3766 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12317 "Parser/parser.cc"
    break;

  case 991:
#line 3771 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 12323 "Parser/parser.cc"
    break;

  case 992:
#line 3773 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 12329 "Parser/parser.cc"
    break;

  case 993:
#line 3778 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 12335 "Parser/parser.cc"
    break;

  case 994:
#line 3780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl)->addQualifiers( (yyvsp[-4].decl) ), true ); }
#line 12341 "Parser/parser.cc"
    break;

  case 996:
#line 3807 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12347 "Parser/parser.cc"
    break;

  case 1000:
#line 3818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12353 "Parser/parser.cc"
    break;

  case 1001:
#line 3820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12359 "Parser/parser.cc"
    break;

  case 1002:
#line 3822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12365 "Parser/parser.cc"
    break;

  case 1003:
#line 3824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12371 "Parser/parser.cc"
    break;

  case 1004:
#line 3826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12377 "Parser/parser.cc"
    break;

  case 1005:
#line 3828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12383 "Parser/parser.cc"
    break;

  case 1006:
#line 3835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12389 "Parser/parser.cc"
    break;

  case 1007:
#line 3837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12395 "Parser/parser.cc"
    break;

  case 1008:
#line 3839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12401 "Parser/parser.cc"
    break;

  case 1009:
#line 3841 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12407 "Parser/parser.cc"
    break;

  case 1010:
#line 3843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12413 "Parser/parser.cc"
    break;

  case 1011:
#line 3845 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12419 "Parser/parser.cc"
    break;

  case 1012:
#line 3850 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 12425 "Parser/parser.cc"
    break;

  case 1013:
#line 3852 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 12431 "Parser/parser.cc"
    break;

  case 1014:
#line 3854 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 12437 "Parser/parser.cc"
    break;

  case 1015:
#line 3861 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 12443 "Parser/parser.cc"
    break;

  case 1016:
#line 3863 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 12449 "Parser/parser.cc"
    break;

  case 1019:
#line 3887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 12455 "Parser/parser.cc"
    break;

  case 1020:
#line 3889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 12461 "Parser/parser.cc"
    break;


#line 12465 "Parser/parser.cc"

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
#line 3892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "make install" //
// End: //
