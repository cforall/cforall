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
#ifdef __clang__
#pragma GCC diagnostic ignored "-Wparentheses-equality"
#endif

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
#define MISSING_ANON_FIELD "Missing loop fields with an anonymous loop index is meaningless as loop index is unavailable in loop body."
#define MISSING_LOW "Missing low value for up-to range so index is uninitialized."
#define MISSING_HIGH "Missing high value for down-to range so index is uninitialized."

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

#line 299 "Parser/parser.cc"

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
    THREADLOCALGCC = 263,
    THREADLOCALC11 = 264,
    INLINE = 265,
    FORTRAN = 266,
    NORETURN = 267,
    CONST = 268,
    VOLATILE = 269,
    RESTRICT = 270,
    ATOMIC = 271,
    FORALL = 272,
    MUTEX = 273,
    VIRTUAL = 274,
    VTABLE = 275,
    COERCE = 276,
    VOID = 277,
    CHAR = 278,
    SHORT = 279,
    INT = 280,
    LONG = 281,
    FLOAT = 282,
    DOUBLE = 283,
    SIGNED = 284,
    UNSIGNED = 285,
    BOOL = 286,
    COMPLEX = 287,
    IMAGINARY = 288,
    INT128 = 289,
    UINT128 = 290,
    uuFLOAT80 = 291,
    uuFLOAT128 = 292,
    uFLOAT16 = 293,
    uFLOAT32 = 294,
    uFLOAT32X = 295,
    uFLOAT64 = 296,
    uFLOAT64X = 297,
    uFLOAT128 = 298,
    DECIMAL32 = 299,
    DECIMAL64 = 300,
    DECIMAL128 = 301,
    ZERO_T = 302,
    ONE_T = 303,
    SIZEOF = 304,
    TYPEOF = 305,
    VALIST = 306,
    AUTO_TYPE = 307,
    OFFSETOF = 308,
    BASETYPEOF = 309,
    TYPEID = 310,
    ENUM = 311,
    STRUCT = 312,
    UNION = 313,
    EXCEPTION = 314,
    GENERATOR = 315,
    COROUTINE = 316,
    MONITOR = 317,
    THREAD = 318,
    OTYPE = 319,
    FTYPE = 320,
    DTYPE = 321,
    TTYPE = 322,
    TRAIT = 323,
    LABEL = 324,
    SUSPEND = 325,
    ATTRIBUTE = 326,
    EXTENSION = 327,
    IF = 328,
    ELSE = 329,
    SWITCH = 330,
    CASE = 331,
    DEFAULT = 332,
    DO = 333,
    WHILE = 334,
    FOR = 335,
    BREAK = 336,
    CONTINUE = 337,
    GOTO = 338,
    RETURN = 339,
    CHOOSE = 340,
    FALLTHRU = 341,
    FALLTHROUGH = 342,
    WITH = 343,
    WHEN = 344,
    WAITFOR = 345,
    DISABLE = 346,
    ENABLE = 347,
    TRY = 348,
    THROW = 349,
    THROWRESUME = 350,
    AT = 351,
    ASM = 352,
    ALIGNAS = 353,
    ALIGNOF = 354,
    GENERIC = 355,
    STATICASSERT = 356,
    IDENTIFIER = 357,
    QUOTED_IDENTIFIER = 358,
    TYPEDIMname = 359,
    TYPEDEFname = 360,
    TYPEGENname = 361,
    TIMEOUT = 362,
    WOR = 363,
    CATCH = 364,
    RECOVER = 365,
    CATCHRESUME = 366,
    FIXUP = 367,
    FINALLY = 368,
    INTEGERconstant = 369,
    CHARACTERconstant = 370,
    STRINGliteral = 371,
    DIRECTIVE = 372,
    FLOATING_DECIMALconstant = 373,
    FLOATING_FRACTIONconstant = 374,
    FLOATINGconstant = 375,
    ARROW = 376,
    ICR = 377,
    DECR = 378,
    LS = 379,
    RS = 380,
    LE = 381,
    GE = 382,
    EQ = 383,
    NE = 384,
    ANDAND = 385,
    OROR = 386,
    ELLIPSIS = 387,
    EXPassign = 388,
    MULTassign = 389,
    DIVassign = 390,
    MODassign = 391,
    PLUSassign = 392,
    MINUSassign = 393,
    LSassign = 394,
    RSassign = 395,
    ANDassign = 396,
    ERassign = 397,
    ORassign = 398,
    ErangeUpEq = 399,
    ErangeDown = 400,
    ErangeDownEq = 401,
    ATassign = 402,
    THEN = 403
  };
#endif
/* Tokens.  */
#define TYPEDEF 258
#define EXTERN 259
#define STATIC 260
#define AUTO 261
#define REGISTER 262
#define THREADLOCALGCC 263
#define THREADLOCALC11 264
#define INLINE 265
#define FORTRAN 266
#define NORETURN 267
#define CONST 268
#define VOLATILE 269
#define RESTRICT 270
#define ATOMIC 271
#define FORALL 272
#define MUTEX 273
#define VIRTUAL 274
#define VTABLE 275
#define COERCE 276
#define VOID 277
#define CHAR 278
#define SHORT 279
#define INT 280
#define LONG 281
#define FLOAT 282
#define DOUBLE 283
#define SIGNED 284
#define UNSIGNED 285
#define BOOL 286
#define COMPLEX 287
#define IMAGINARY 288
#define INT128 289
#define UINT128 290
#define uuFLOAT80 291
#define uuFLOAT128 292
#define uFLOAT16 293
#define uFLOAT32 294
#define uFLOAT32X 295
#define uFLOAT64 296
#define uFLOAT64X 297
#define uFLOAT128 298
#define DECIMAL32 299
#define DECIMAL64 300
#define DECIMAL128 301
#define ZERO_T 302
#define ONE_T 303
#define SIZEOF 304
#define TYPEOF 305
#define VALIST 306
#define AUTO_TYPE 307
#define OFFSETOF 308
#define BASETYPEOF 309
#define TYPEID 310
#define ENUM 311
#define STRUCT 312
#define UNION 313
#define EXCEPTION 314
#define GENERATOR 315
#define COROUTINE 316
#define MONITOR 317
#define THREAD 318
#define OTYPE 319
#define FTYPE 320
#define DTYPE 321
#define TTYPE 322
#define TRAIT 323
#define LABEL 324
#define SUSPEND 325
#define ATTRIBUTE 326
#define EXTENSION 327
#define IF 328
#define ELSE 329
#define SWITCH 330
#define CASE 331
#define DEFAULT 332
#define DO 333
#define WHILE 334
#define FOR 335
#define BREAK 336
#define CONTINUE 337
#define GOTO 338
#define RETURN 339
#define CHOOSE 340
#define FALLTHRU 341
#define FALLTHROUGH 342
#define WITH 343
#define WHEN 344
#define WAITFOR 345
#define DISABLE 346
#define ENABLE 347
#define TRY 348
#define THROW 349
#define THROWRESUME 350
#define AT 351
#define ASM 352
#define ALIGNAS 353
#define ALIGNOF 354
#define GENERIC 355
#define STATICASSERT 356
#define IDENTIFIER 357
#define QUOTED_IDENTIFIER 358
#define TYPEDIMname 359
#define TYPEDEFname 360
#define TYPEGENname 361
#define TIMEOUT 362
#define WOR 363
#define CATCH 364
#define RECOVER 365
#define CATCHRESUME 366
#define FIXUP 367
#define FINALLY 368
#define INTEGERconstant 369
#define CHARACTERconstant 370
#define STRINGliteral 371
#define DIRECTIVE 372
#define FLOATING_DECIMALconstant 373
#define FLOATING_FRACTIONconstant 374
#define FLOATINGconstant 375
#define ARROW 376
#define ICR 377
#define DECR 378
#define LS 379
#define RS 380
#define LE 381
#define GE 382
#define EQ 383
#define NE 384
#define ANDAND 385
#define OROR 386
#define ELLIPSIS 387
#define EXPassign 388
#define MULTassign 389
#define DIVassign 390
#define MODassign 391
#define PLUSassign 392
#define MINUSassign 393
#define LSassign 394
#define RSassign 395
#define ANDassign 396
#define ERassign 397
#define ORassign 398
#define ErangeUpEq 399
#define ErangeDown 400
#define ErangeDownEq 401
#define ATassign 402
#define THEN 403

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"

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

#line 669 "Parser/parser.cc"

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
#define YYFINAL  145
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   21268

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  176
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  291
/* YYNRULES -- Number of rules.  */
#define YYNRULES  1030
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  2089

#define YYUNDEFTOK  2
#define YYMAXUTOK   403


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
       2,     2,     2,   165,     2,     2,     2,   169,   162,     2,
     150,   152,   161,   163,   156,   164,   153,   168,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,   157,   175,
     170,   174,   171,   173,   151,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,   154,   167,   155,   160,     2,   159,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,   158,   172,   149,   166,     2,     2,     2,
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
     145,   146,   147,   148
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   569,   569,   573,   580,   581,   582,   583,   584,   588,
     589,   590,   591,   592,   593,   594,   598,   599,   603,   604,
     609,   613,   614,   625,   627,   629,   633,   634,   636,   638,
     640,   642,   652,   660,   669,   670,   680,   685,   690,   691,
     696,   702,   704,   706,   712,   714,   716,   718,   720,   722,
     724,   726,   728,   730,   732,   734,   736,   738,   740,   742,
     744,   754,   755,   759,   760,   765,   768,   772,   773,   777,
     778,   780,   782,   784,   786,   788,   793,   795,   797,   805,
     806,   814,   817,   818,   820,   825,   841,   843,   845,   847,
     849,   851,   853,   855,   857,   865,   866,   868,   872,   873,
     874,   875,   879,   880,   882,   884,   886,   888,   890,   892,
     894,   901,   902,   903,   904,   908,   909,   913,   914,   919,
     920,   922,   924,   929,   930,   932,   937,   938,   940,   945,
     946,   948,   950,   952,   957,   958,   960,   965,   966,   971,
     972,   977,   978,   983,   984,   989,   990,   995,   996,   999,
    1004,  1009,  1010,  1018,  1024,  1025,  1029,  1030,  1034,  1035,
    1039,  1040,  1041,  1042,  1043,  1044,  1045,  1046,  1047,  1048,
    1049,  1059,  1061,  1066,  1067,  1069,  1071,  1076,  1077,  1083,
    1084,  1090,  1091,  1092,  1093,  1094,  1095,  1096,  1097,  1098,
    1099,  1100,  1102,  1103,  1109,  1111,  1121,  1123,  1131,  1132,
    1137,  1139,  1141,  1143,  1145,  1149,  1150,  1152,  1157,  1159,
    1166,  1168,  1170,  1180,  1182,  1184,  1189,  1194,  1197,  1202,
    1204,  1206,  1208,  1216,  1217,  1219,  1223,  1225,  1229,  1231,
    1232,  1234,  1236,  1241,  1242,  1246,  1251,  1252,  1256,  1258,
    1263,  1265,  1270,  1272,  1274,  1276,  1281,  1283,  1285,  1287,
    1292,  1294,  1299,  1300,  1322,  1324,  1326,  1329,  1331,  1334,
    1336,  1339,  1341,  1346,  1351,  1353,  1358,  1363,  1365,  1367,
    1369,  1371,  1374,  1376,  1379,  1381,  1386,  1392,  1395,  1397,
    1402,  1408,  1410,  1415,  1421,  1424,  1426,  1429,  1431,  1436,
    1443,  1445,  1450,  1456,  1458,  1463,  1469,  1472,  1477,  1485,
    1487,  1489,  1494,  1496,  1501,  1502,  1504,  1509,  1511,  1516,
    1518,  1520,  1522,  1525,  1529,  1532,  1536,  1538,  1540,  1542,
    1544,  1546,  1548,  1550,  1552,  1554,  1556,  1561,  1562,  1566,
    1572,  1577,  1582,  1583,  1587,  1591,  1596,  1597,  1603,  1607,
    1609,  1611,  1613,  1616,  1618,  1623,  1625,  1630,  1632,  1634,
    1639,  1641,  1647,  1648,  1652,  1653,  1654,  1655,  1659,  1664,
    1665,  1667,  1669,  1671,  1675,  1679,  1680,  1684,  1686,  1688,
    1690,  1692,  1698,  1699,  1705,  1706,  1710,  1711,  1716,  1718,
    1724,  1725,  1727,  1732,  1737,  1748,  1749,  1753,  1754,  1760,
    1761,  1765,  1767,  1771,  1773,  1777,  1778,  1782,  1783,  1787,
    1794,  1795,  1799,  1801,  1816,  1817,  1818,  1819,  1821,  1825,
    1827,  1831,  1838,  1840,  1842,  1847,  1848,  1850,  1852,  1854,
    1886,  1889,  1894,  1896,  1902,  1907,  1912,  1923,  1928,  1933,
    1938,  1943,  1952,  1956,  1963,  1965,  1966,  1967,  1973,  1975,
    1980,  1981,  1982,  1991,  1992,  1993,  1997,  1998,  2005,  2014,
    2015,  2016,  2021,  2022,  2031,  2032,  2037,  2038,  2042,  2044,
    2046,  2048,  2050,  2054,  2059,  2060,  2062,  2072,  2073,  2078,
    2080,  2082,  2084,  2086,  2088,  2091,  2093,  2095,  2100,  2102,
    2104,  2106,  2108,  2110,  2112,  2114,  2116,  2118,  2120,  2122,
    2124,  2126,  2128,  2130,  2132,  2134,  2136,  2138,  2140,  2142,
    2144,  2146,  2148,  2150,  2152,  2154,  2159,  2160,  2164,  2171,
    2172,  2178,  2179,  2181,  2183,  2185,  2190,  2192,  2197,  2198,
    2200,  2202,  2207,  2209,  2211,  2213,  2215,  2217,  2222,  2229,
    2231,  2233,  2238,  2246,  2245,  2249,  2257,  2258,  2260,  2262,
    2267,  2268,  2270,  2275,  2276,  2278,  2280,  2285,  2286,  2288,
    2293,  2295,  2297,  2299,  2300,  2302,  2307,  2309,  2311,  2316,
    2323,  2327,  2328,  2333,  2332,  2337,  2336,  2355,  2354,  2366,
    2365,  2376,  2381,  2382,  2387,  2393,  2407,  2408,  2412,  2414,
    2416,  2422,  2424,  2426,  2428,  2430,  2432,  2434,  2436,  2442,
    2443,  2448,  2457,  2459,  2468,  2470,  2471,  2472,  2474,  2476,
    2477,  2482,  2483,  2484,  2489,  2491,  2494,  2501,  2502,  2503,
    2509,  2514,  2516,  2522,  2523,  2529,  2530,  2534,  2539,  2542,
    2541,  2545,  2548,  2555,  2560,  2559,  2568,  2573,  2577,  2581,
    2585,  2587,  2592,  2594,  2596,  2598,  2604,  2605,  2606,  2613,
    2614,  2616,  2617,  2618,  2620,  2622,  2629,  2630,  2632,  2634,
    2639,  2640,  2646,  2647,  2649,  2650,  2655,  2656,  2657,  2659,
    2667,  2668,  2670,  2673,  2675,  2679,  2680,  2681,  2683,  2685,
    2690,  2692,  2697,  2699,  2708,  2710,  2715,  2716,  2717,  2721,
    2722,  2723,  2728,  2729,  2734,  2735,  2736,  2737,  2741,  2742,
    2747,  2748,  2749,  2750,  2751,  2765,  2766,  2771,  2772,  2778,
    2780,  2783,  2785,  2787,  2810,  2811,  2817,  2818,  2824,  2823,
    2833,  2832,  2836,  2842,  2848,  2849,  2851,  2855,  2860,  2862,
    2864,  2866,  2872,  2873,  2877,  2878,  2883,  2885,  2892,  2894,
    2895,  2897,  2902,  2904,  2906,  2911,  2913,  2918,  2923,  2931,
    2933,  2938,  2939,  2944,  2945,  2949,  2950,  2951,  2956,  2958,
    2964,  2966,  2971,  2973,  2979,  2980,  2984,  2988,  2992,  2994,
    2995,  2996,  3001,  3004,  3003,  3015,  3014,  3026,  3025,  3037,
    3036,  3048,  3047,  3061,  3067,  3069,  3075,  3076,  3087,  3094,
    3099,  3105,  3108,  3111,  3115,  3121,  3124,  3127,  3132,  3133,
    3134,  3138,  3144,  3145,  3155,  3156,  3160,  3161,  3166,  3171,
    3172,  3178,  3179,  3181,  3186,  3187,  3188,  3189,  3190,  3192,
    3227,  3229,  3234,  3236,  3237,  3239,  3244,  3246,  3248,  3250,
    3255,  3257,  3259,  3261,  3263,  3265,  3267,  3272,  3274,  3276,
    3278,  3287,  3289,  3290,  3295,  3297,  3299,  3301,  3303,  3308,
    3310,  3312,  3314,  3319,  3321,  3323,  3325,  3327,  3329,  3341,
    3342,  3343,  3347,  3349,  3351,  3353,  3355,  3360,  3362,  3364,
    3366,  3371,  3373,  3375,  3377,  3379,  3381,  3396,  3401,  3406,
    3408,  3409,  3411,  3416,  3418,  3420,  3422,  3427,  3429,  3431,
    3433,  3435,  3437,  3439,  3444,  3446,  3448,  3450,  3452,  3462,
    3464,  3466,  3467,  3469,  3474,  3476,  3478,  3483,  3485,  3487,
    3489,  3494,  3496,  3498,  3512,  3514,  3516,  3517,  3519,  3524,
    3526,  3531,  3533,  3535,  3540,  3542,  3547,  3549,  3566,  3567,
    3569,  3574,  3576,  3578,  3580,  3582,  3587,  3588,  3590,  3592,
    3597,  3599,  3601,  3607,  3609,  3611,  3614,  3618,  3620,  3622,
    3624,  3658,  3659,  3661,  3663,  3668,  3670,  3672,  3674,  3676,
    3681,  3682,  3684,  3686,  3691,  3693,  3695,  3701,  3702,  3704,
    3713,  3716,  3718,  3721,  3723,  3725,  3739,  3740,  3742,  3747,
    3749,  3751,  3753,  3755,  3760,  3761,  3763,  3765,  3770,  3772,
    3780,  3781,  3782,  3787,  3788,  3793,  3795,  3797,  3799,  3801,
    3803,  3810,  3812,  3814,  3816,  3818,  3821,  3823,  3825,  3827,
    3829,  3834,  3836,  3838,  3843,  3869,  3870,  3872,  3876,  3877,
    3881,  3883,  3885,  3887,  3889,  3891,  3898,  3900,  3902,  3904,
    3906,  3908,  3913,  3915,  3917,  3924,  3926,  3944,  3946,  3951,
    3952
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 1
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "TYPEDEF", "EXTERN", "STATIC", "AUTO",
  "REGISTER", "THREADLOCALGCC", "THREADLOCALC11", "INLINE", "FORTRAN",
  "NORETURN", "CONST", "VOLATILE", "RESTRICT", "ATOMIC", "FORALL", "MUTEX",
  "VIRTUAL", "VTABLE", "COERCE", "VOID", "CHAR", "SHORT", "INT", "LONG",
  "FLOAT", "DOUBLE", "SIGNED", "UNSIGNED", "BOOL", "COMPLEX", "IMAGINARY",
  "INT128", "UINT128", "uuFLOAT80", "uuFLOAT128", "uFLOAT16", "uFLOAT32",
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
     395,   396,   397,   398,   399,   400,   401,   402,   403,   125,
      40,    64,    41,    46,    91,    93,    44,    58,   123,    96,
      94,    42,    38,    43,    45,    33,   126,    92,    47,    37,
      60,    62,   124,    63,    61,    59
};
# endif

#define YYPACT_NINF (-1736)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-911)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     266, 12012,   289,   307, 16362,    99, -1736, -1736, -1736, -1736,
   -1736, -1736, -1736, -1736, -1736, -1736, -1736, -1736,   219,   986,
     228, -1736, -1736, -1736, -1736, -1736, -1736, -1736, -1736, -1736,
   -1736, -1736, -1736, -1736, -1736, -1736, -1736, -1736, -1736, -1736,
   -1736, -1736, -1736, -1736, -1736, -1736, -1736, -1736,    98,   385,
   -1736, -1736, -1736, -1736, -1736, -1736,  5108,  5108,   328, 12012,
     341,   354, -1736, -1736,   416, -1736, -1736, -1736, -1736, -1736,
   -1736, -1736, -1736, -1736,  2248, -1736,   632,   286, -1736, -1736,
   -1736, -1736, -1736, 16212, -1736, -1736,   362,   432,   482,   442,
   -1736,  5108,   432,   432,   432,   418,  4850,   611,   976, 12172,
   -1736, -1736, -1736, 16062,  2216, -1736, -1736, -1736,  3876,   631,
   12224,   764,   971,  3876,  1142,   483, -1736, -1736, -1736, -1736,
     587, -1736, -1736, -1736, -1736,   516, -1736, -1736, -1736, -1736,
   -1736,   601,   604,   587, -1736,   587,   630, -1736, -1736, -1736,
   17216,  5108, -1736, -1736,  5108, -1736, 12012,   585, 17268, -1736,
   -1736,  5035, 18280, -1736,  1002,  1002,   684,  2548, -1736, -1736,
   -1736, -1736,   282, 13907,  3009,   587, -1736, -1736, -1736, -1736,
   -1736, -1736,   680, -1736,   610,   715,   717, -1736,   729, 20670,
   15230,  4240,  2248,   470,   737,   802,   811,   816,   819,   821,
   -1736, -1736, 17418, 10966,   826, -1736, 16803, -1736, -1736, -1736,
   -1736,   831, -1736, -1736,   804, -1736, 18726,   829, 18942, -1736,
     841,  5108,   604,   853,   858,   871,   876, -1736, -1736, -1736,
    2982,  3862,   889,   965,    81, -1736, -1736,   587,   587,    30,
     105,   329,    30, -1736,   587,   587, -1736,  4309, -1736, -1736,
     918,   927,  1002, 18583, -1736, -1736, 16212, -1736, -1736,  3876,
   -1736,  2131,   483,   943,  1019,   105,  5108,   482, -1736, 13430,
   -1736,  1002,  1002,   973,  1019,   105,  5108, -1736,  9410, -1736,
   -1736,  1002, -1736,  1002, -1736,   712,  4655,  5108, -1736,  1571,
     987, -1736, -1736, -1736, 16010,   604,   162, -1736, -1736, 18330,
   -1736,   965,    41, -1736, 20670, 18280,  3322,  4309, -1736,   394,
   -1736, -1736, -1736, 17268,  5108, -1736,   980, -1736, -1736, -1736,
   -1736,  5108,  3614,   290,   -68, -1736,  5108,   610, -1736,   754,
     587,   587,  1005, 17470,   769, 14384, 18635,  3876,  3876, -1736,
    3876,  1002,  3876,  1002, -1736, -1736,   587, -1736,   989, -1736,
   17620, -1736, -1736, -1736, 17672,   831, -1736,  1012,   337,  1474,
    1018,   483,  1020, -1736,  2548,  1007,   610,  2548,  1244, -1736,
    1049,  1087, 20742,  1067,  1070, 20670, 20814,  1080,  8779, -1736,
   -1736, -1736, -1736, -1736, -1736, 20886, 20886, 15076,  1056,  4495,
   -1736, -1736, -1736, -1736,   206, -1736,   248, -1736,   969, -1736,
   20670, 20670, -1736,  1065,   789,   757,   808,   701,   696,  1074,
    1088,  1078,  1122,    78, -1736,   734, -1736,  1102, -1736,   790,
    2831, 15538, -1736, -1736,   787,  1102, -1736, -1736,   744, -1736,
   -1736,  4240,  1117,  1121,  1164,  1178,  1181,  1184, -1736, -1736,
     423,  1109, -1736,   755,  1109, -1736, -1736, 17216, -1736,   823,
    1150, 15692, -1736, -1736,  5191,  4077,  1213, 14384,  1216,   833,
     912, -1736, -1736, -1736, -1736, -1736,  5108,  5248, -1736, -1736,
   -1736, -1736, -1736, -1736,  8357,  3699,  1056, 18726,  1182,  1202,
   -1736, -1736,  1208, 18942,   700, -1736, -1736, -1736, 19014,  1218,
   -1736, -1736, -1736, -1736, -1736,  2982,   648,  1236,  1240,  1245,
     716,  1248,  1256,  1272,  3862, -1736, -1736,   587,  1231,   482,
    1271, -1736, -1736,  1274, -1736, -1736,   604,  1019, -1736, -1736,
   -1736,   604, -1736, -1736,  4309, -1736, 15538, 15538, -1736,  1002,
    5035,  8888, 14543, -1736, -1736, -1736, -1736, -1736,   604,  1019,
      41, -1736, -1736,  3876,  1273,  1019,   105, -1736,   604,  1019,
   -1736, 13321, -1736,  1002,  1002, -1736, -1736,  1275,   400,  1276,
     483,  1277, -1736, 16521, -1736,   800, -1736,  1360, 18480, -1736,
    5035,  6770, 18583, -1736, 16010, 20958, -1736, -1736, -1736, -1736,
   -1736,  3322,   725,  4309, -1736, 14543,   965, 12012, -1736,  1283,
   -1736,  1280, -1736, -1736, -1736, -1736, -1736,  2548, -1736, -1736,
    1362,  5179,  3787, 17672, 10966, -1736, 17822, -1736,  1002,  1002,
   -1736, -1736,   831, -1736,   847,  1287,  1425, 20670,  1081,  1274,
    1270, -1736,   587,   587, -1736,  1109, -1736, 17470, -1736, -1736,
   16962,  1002,  1002, -1736,  5179,   587, -1736, 18137, -1736, -1736,
   17620, -1736,   282,  1289,  1279,  1288,  1474,   810, 17268,   837,
   -1736, -1736, -1736, -1736, -1736, -1736,   838, -1736,  1298,  1285,
   -1736, 15384, -1736, 17874, 17874, -1736, 15384, -1736, 20670, -1736,
   12224, 12224, 15384, -1736, -1736, 17014, 17874, 17874,   790,  1403,
    1497,   656,  1618, -1736,   869,  1297,   878,  1302, -1736, 19014,
   20670, 19086,  1308, 20670,  1571, 20670,  1571, -1736,  1393, -1736,
   -1736, 19158,  2311, 20670, 19158,  1571, -1736, -1736, 20670, 20670,
   20670, 20670, 20670, 20670, 20670, 20670, 20670, 20670, 20670, 20670,
   20670, 20670, 20670, 20670, 20670, 20670, 20670, 19230,  1290,   729,
    4356, 10966, -1736, -1736, -1736, -1736, -1736, -1736, -1736, -1736,
   -1736, -1736, -1736,  1312, 20670, -1736, -1736,   787,  1173, -1736,
   -1736,   587,   587, -1736, -1736, 15538, -1736,   465,  1109, -1736,
     901,  1109, -1736, -1736, -1736,  1274, -1736, -1736,  1274, 21030,
   -1736, -1736, 10966,  1300,  1321,  2663,  1459,  3228,   525,  1270,
   -1736,   587,   587,  1270,   590, -1736,   587,   587, 20670,  5108,
     898,   954,  1270,   -19, 13748, 13748,  5108, -1736, -1736, 20670,
    1208, -1736, 18726,  1330, -1736,  1542, -1736, -1736, -1736, -1736,
   -1736,   905, -1736, 13748,  1571,  5035,  1571,   910,  1328,  1329,
    1333,   915,  1334,  1335,  1336,   599,  1109, -1736, -1736,   633,
    1109, -1736, -1736, -1736,  5035,   729, -1736,  1109, 21030, -1736,
     604, 16521, -1736, -1736,   919,  1345,   922,  1364, -1736,  1371,
   -1736,   604, -1736, -1736,   604,  1019,  1371, -1736,   604,  1365,
    1366,  1368, -1736, -1736, 16962, -1736,  1370, -1736, -1736, -1736,
    1571,  5108, 10120,  1463,  1353, 18033, -1736,  1150, -1736, 13748,
     932, -1736, -1736,  1371, -1736, 17268, 15538,  1363, -1736,  1363,
   -1736, -1736, -1736,  1474,   587,   587, -1736, 17620, -1736, 11129,
   15846, -1736, 16521,  1382,  1386,  1387, -1736,  8449,   587, -1736,
    1081, -1736, -1736, -1736, -1736,  1274, -1736, -1736, -1736,  1002,
   -1736,  3802, -1736, -1736,   483,  2058,  1397, 19302, -1736,  1474,
    1289, -1736, -1736,  1390,  1398,  1244, 19158, -1736,  1399,   286,
    1401,  1406,  1407,  1404,  1409, 20670,  1411,  1412,  1413, 10966,
   20670, -1736, -1736,  1673, -1736, -1736, -1736, 20670, -1736,  1414,
    1416, 18798,   994, -1736, 19158,  1415, -1736,  1420, -1736, -1736,
    5317, -1736, -1736,   937, -1736, -1736, -1736, -1736,  5317, -1736,
   -1736,  1083,   269, -1736, -1736,  1065,  1065,  1065,   789,   789,
     757,   757,   808,   808,   808,   808,   701,   701,   696,  1074,
    1088,  1078,  1122, 20670,  1089, -1736,  1417,  5317, -1736, -1736,
   18726, -1736, 16521,  1419,  1437,  1439,  1173, -1736, -1736, -1736,
   -1736, -1736, -1736, -1736, -1736,  1274, -1736, -1736,  1274, 16521,
   16521, -1736, -1736,  2663,   731,  1440,  1447,  1449,  1450,  4047,
    3228, -1736, -1736, -1736, -1736, -1736, -1736, -1736, -1736, -1736,
   -1736, -1736, -1736, -1736, -1736,  1422, -1736,  1270, -1736, -1736,
   -1736, -1736, -1736, -1736, -1736, -1736,  1453,  1455, -1736,   482,
    5317,  1103,   215, -1736, -1736,  1461, -1736, 18942, -1736, 20670,
     587, 19374, 13748, -1736, -1736, -1736,  1441,   647,  1109, -1736,
     659,  1109, -1736, -1736, -1736, -1736,  1274, -1736, -1736, -1736,
    1274,   965,  1462,  1274, -1736, -1736, -1736, -1736, -1736, -1736,
   -1736,  1464, -1736, -1736,  1371, -1736,   604, -1736, -1736, -1736,
   -1736, -1736,  5826,  1465,  1467, -1736,   113, -1736,   570,   466,
   10803,  1468, 14913,  1471,  1475,  2579,  3114,  3085, 19446,  1477,
   -1736, -1736,  1487,  1489, -1736, -1736,   604, 20670, 20670,  1610,
    1484,    28, -1736,  1569,  1490,  1481, -1736, -1736, -1736,  9928,
   -1736, -1736, -1736, -1736, -1736,  2911, -1736, -1736, -1736,  1568,
   -1736, -1736, -1736,  1571, -1736, -1736, 12652, 16212,  1502, -1736,
    5108, -1736,  1485,  1509,  1510, -1736,  1111, -1736, -1736, -1736,
   -1736,  5035, -1736, -1736,  1491,  1492,   947, 17268,   610,   610,
    1289,  1512,  1513, -1736, -1736,  1056,  1150, 15692, -1736,  1102,
   -1736, 11292, -1736,   661,  1109, -1736,  1002, 11848, -1736, -1736,
    1474,   587,   587,   282,  1279, -1736, 18726, -1736,  1289,  1519,
    1523, -1736, -1736,   952,   467, 10966,  1571, -1736,   467, 17066,
     467, -1736, 20670, 20670, 20670, -1736, -1736, -1736, -1736, 20670,
   20670,  1516, 18726, -1736, -1736,  1538,   541, -1736, -1736, -1736,
    4876, -1736, -1736,  1124, -1736,   159, -1736, 19158,  1127, -1736,
   19014, -1736, -1736, 20670,  1500,  1143,  1146,  1208, -1736,   672,
    1109, -1736, -1736, 16521, 16521, -1736, -1736,  1545,   686,  1109,
   -1736,   687,  2733,   587,   587, -1736, -1736, 16521, 16521, -1736,
    1544, -1736, 14543, 14543,  1548,  1546,  1547,  1555, -1736,  1552,
   20670, 20670,  1153,  1556, -1736, -1736, -1736, -1736, -1736, -1736,
   -1736,  1562, 20670, -1736, -1736, -1736,  1274, -1736, -1736, -1736,
    1274, 16521, 16521,   482,   587,  1156,  1563,  1567, -1736, -1736,
    1570, 12805, 12958, 13111, 17268, 17874, 17874,  1573, -1736,  1543,
    1550,  2401,  9101, -1736,   387,  5108, -1736, -1736,  5108, -1736,
   18870,    19,   240, -1736, -1736, -1736, -1736, 20670,  1576,  1638,
   10639, 10293, -1736,  1553, -1736,  1554, 20670,  1558, 18726,  1560,
   20670, 19014, 20670,  1209, -1736,  1565,    86, -1736,   109,  1577,
   -1736, -1736,  1572, -1736,  1566, -1736,  1574,  1580, 14913,   193,
   13589,   587,   428, -1736, -1736, -1736,  1579, -1736,  1594, -1736,
    1595, -1736,  1564, -1736,  1591, -1736, -1736, -1736, -1736,  1607,
    1474,  1474, 11455,  1603,  1605,  1609, -1736,  1614, -1736, -1736,
   -1736,  1274, 20670, 20670,  1150,  1612, -1736,  1289, -1736,  1599,
     199, -1736,  1208,  1621, -1736, -1736, 17268, -1736,  1622,  1616,
     978, -1736,  1623, -1736, -1736, -1736, -1736, -1736, 18726,  1208,
   19014, -1736,  1656,  5317, -1736,  1656,  1656, -1736,  5317,  5091,
    5156, -1736, -1736,  1158, -1736, -1736, -1736,  1629,  1627, -1736,
   -1736, -1736,  1274, -1736, -1736,  1639,  1640,   587, -1736, -1736,
   -1736,  1274, -1736, -1736, -1736,  1644, -1736, -1736, -1736, -1736,
   -1736, -1736, -1736, -1736, -1736, -1736, -1736, -1736, -1736,  1626,
   -1736, -1736, -1736, -1736,  1642,  1647,   587, -1736, 16521, 16521,
   -1736, -1736, -1736, -1736, 20670, -1736, -1736,  1651, -1736,  1573,
    1573,  1573,   850,  1628,   436, -1736,  4694,   487, 15538, -1736,
   -1736, -1736,  3428, 20670,  4626,   532, -1736, -1736,    89,  1645,
    1645,  5108, -1736, -1736, 16671, -1736, 20670,  1648,  1649, -1736,
   -1736, -1736, -1736,   981,  1658, 14913,  1490,  1659, 20670,   362,
    1657,   418, 13271, 17268, -1736, -1736, -1736,   408, 14913, 20670,
    1062,   445, -1736, 20670,  9744, -1736, -1736,   554, -1736,  1208,
   -1736,   995,  1016,  1021, -1736, -1736, -1736, -1736,   604,  1209,
    1665, -1736, -1736, 20670, -1736,  1667,   729, 10803, -1736, -1736,
   -1736, -1736, 20670,  1711, -1736,  9672, -1736,   587, 14543, -1736,
   -1736, 17268, -1736, -1736, -1736,  1289,  1289, -1736, -1736, -1736,
    1669, -1736, 16521, -1736, -1736,  1671, -1736,  1675,  1679,  1674,
    1474, -1736,  1685, -1736, -1736, -1736, 20670, -1736, 17066, 20670,
    1208,  1686,  1170, -1736,  1187, -1736,  5317, -1736,  5317, -1736,
   -1736, -1736, -1736, 16521,  1684,  1688, -1736, -1736, 16521, 16521,
    1689,  1690,  1207, 14066, 14225, -1736,  1682, -1736, -1736, -1736,
   -1736,  1691,  1692,  1210, -1736, -1736, -1736, -1736,   850,  1458,
     567, -1736, -1736, -1736, -1736,   587,   587, -1736, -1736, -1736,
     609, -1736,  1027,  3428,   679, -1736,  4626,   587, -1736, -1736,
   -1736, -1736, -1736, -1736, -1736, -1736,   621, 14913,    58, 19518,
    1764, 14913,  1490, 14702, -1736, -1736, -1736, -1736, 20670, -1736,
   19590,  1773,  1677, 18650, 19662, 14913, 10466,  1490,   523,  1140,
    1680, 20670, -1736,  1700,    88, 14913, -1736, -1736,  1704, -1736,
   -1736,  1681,   729,   529,  1698,  1706,  1214,  1771, -1736, -1736,
   -1736, -1736,  5108,  5035,  1713,  1714, -1736, -1736,  1709,  1712,
   -1736, -1736, -1736,  1474,  1289, -1736,  1717, -1736, -1736, -1736,
    1719, -1736, -1736, -1736,  1217,  1220, -1736, -1736, -1736, -1736,
   -1736, -1736, -1736, -1736, -1736, -1736,  1718, -1736, -1736,  1721,
    1723, -1736, -1736, -1736,  1726,  1727,  1728,  1458, -1736,   587,
   -1736, -1736, -1736, -1736, -1736,  1715,  4694, -1736, -1736,  4428,
     115, 11621, -1736, 14795, -1736,    -2,  1039, 14913,  1797,   624,
    1722,   291, 14913, 20670,  1730,   523,  1140,  1710, 21102,  1724,
     355,  1813, -1736, 19734, 19806, 20670,  1490,  1720, 11783, -1736,
   -1736, -1736, 18085, -1736,  1737,  1725,    52, 14913, -1736, 20670,
   19158,   426, -1736, -1736, -1736, -1736, -1736,  1744, -1736, -1736,
    1289,  1753, -1736, -1736, -1736, -1736,  1752,  1761,  1762, 14543,
    1755, -1736, -1736,   708,  1109, -1736, -1736,   850, -1736, -1736,
     229, -1736,   340, -1736, -1736, -1736,  1766, 12332, -1736, -1736,
   14913, -1736,    47, -1736, 14913, 20670,  1765, 19878, -1736, -1736,
   19950, 20022, 20670,  1730,  1490, 20094, 20166, 14913,  1757,   359,
    1763,   405, -1736, -1736,  1772, 12332, 18085, -1736,  4730, 17822,
    1571,  1774, -1736,  1826,  1786,   579,  1781, -1736,  1864, -1736,
    1040, 14913,  1793, 14913, 14913, -1736,  1795, -1736, -1736, -1736,
   -1736, -1736, -1736, -1736, -1736,  1274, -1736, 20670, -1736, 20670,
   -1736, -1736,  1307, 12492, -1736, -1736, 14913, -1736, -1736,  1490,
   -1736, -1736,  1490,  1783,   536,  1784,   568, -1736, -1736,  1490,
   -1736,  1490, -1736,  1796, 20238, 20310, 20382, -1736,  1307, -1736,
    1776,  3199,  4207, -1736, -1736, -1736,    52,  1798, 20670,  1779,
      52,    52, 14913, -1736, -1736, 20670,  1848,  1849, -1736, 16521,
   -1736, -1736, 14795, -1736,  1307, -1736, -1736,  1807, 20454, 20526,
   20598, -1736, -1736,  1490, -1736,  1490, -1736,  1490, -1736,  1776,
   20670,  1808,  4207,  1809,   729,  1812, -1736,   638, -1736, -1736,
    1041,  1771,   591, -1736, -1736,  9222,  1816, 14795, -1736, -1736,
    1490, -1736,  1490, -1736,  1490,  1819,  1817, -1736,   604,   729,
    1820, -1736,  1799,   729, -1736, -1736, 14913,  1897,  1821, -1736,
   -1736, -1736,  9482, -1736,   604, -1736, -1736,  1229, 20670, -1736,
    1046, -1736, 14913, -1736, -1736,   729,  1571,  1824,  1803, -1736,
   -1736, -1736,  1058, -1736, -1736,  1810,  1571, -1736, -1736
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,   452,     0,     2,   452,   469,   470,   471,   472,   473,
     474,   475,   476,   477,   458,   460,   459,   461,     0,     0,
       0,   478,   480,   501,   481,   502,   484,   485,   499,   500,
     479,   497,   498,   482,   483,   486,   487,   488,   489,   490,
     491,   492,   493,   494,   495,   496,   503,   504,   794,   506,
     579,   580,   583,   585,   581,   587,     0,     0,     0,   452,
       0,     0,    16,   550,   556,     9,    10,    11,    12,    13,
      14,    15,   758,    97,     0,    19,     0,     2,    95,    96,
      17,    18,   810,   452,   759,   401,     0,   404,   684,   406,
     415,     0,   405,   435,   436,     0,     0,     0,     0,   533,
     454,   456,   462,   452,   464,   467,   518,   505,   440,   511,
     516,   441,   528,   442,   543,   547,   553,   532,   559,   571,
     794,   576,   577,   560,   629,   407,   408,     3,   760,   773,
     457,     0,     0,   794,   832,   794,     2,   849,   850,   851,
     452,     0,  1008,  1009,     0,     1,   452,     0,   452,   424,
     425,     0,   533,   446,   447,   448,   763,     0,   582,   584,
     586,   588,     0,   452,     0,   795,   796,   578,   507,   677,
     678,   676,   737,   732,   722,     0,     0,   761,     0,     0,
     452,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     551,   554,   452,   452,     0,  1010,   533,   839,   857,  1014,
    1007,  1005,  1012,   400,     0,   159,   690,   158,     0,   409,
       0,     0,     0,     0,     0,     0,     0,   399,   909,   910,
       0,     0,   434,   792,   794,   788,   813,   794,   794,   790,
       2,   794,   789,   870,   794,   794,   867,     0,   526,   527,
       0,     0,   452,   452,   469,     2,   452,   416,   455,   465,
     519,     0,   548,     0,   776,     2,     0,   684,   417,   533,
     512,   529,   544,     0,   776,     2,     0,   468,   513,   520,
     521,   530,   535,   545,   549,     0,   563,     0,   752,     2,
       2,   774,   831,   833,   452,     0,     2,     2,  1018,   533,
    1021,   792,   792,     3,     0,   533,     0,     0,   427,   794,
     790,   789,     2,   452,     0,   756,     0,   718,   720,   719,
     721,     0,     0,   714,     0,   704,     0,   713,   724,     0,
     794,   794,     2,   452,  1029,   453,   452,   464,   443,   511,
     444,   536,   445,   543,   540,   561,   794,   562,     0,   665,
     452,   666,   983,   984,   452,   667,   669,   550,   556,     0,
     630,   631,     0,   797,     0,   735,   723,     0,   801,    21,
       0,    20,     0,     0,     0,     0,     0,     0,    23,    25,
       4,     8,     5,     6,     7,     0,     0,   452,     2,     0,
      98,    99,   100,   101,    82,    24,    83,    38,    81,   102,
       0,     0,   117,   119,   123,   126,   129,   134,   137,   139,
     141,   143,   145,   147,   150,     0,    26,     0,   557,     2,
     102,   452,   151,   729,   680,   547,   682,   728,     0,   679,
     683,     0,     0,     0,     0,     0,     0,     0,   811,   837,
     794,   847,   855,   859,   865,     2,  1016,   452,  1019,     2,
      95,   452,     3,   664,     0,  1029,     0,   453,   511,   536,
     543,     3,     3,   646,   650,   660,   666,   667,     2,   840,
     858,  1006,     2,     2,    23,     0,     2,   690,    24,     0,
     688,   691,  1027,     0,     0,   697,   686,   685,     0,     0,
     778,     2,     2,     2,     2,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   816,   873,   794,     0,   684,
       2,   812,   820,   936,   814,   815,     0,   776,     2,   869,
     877,     0,   871,   872,     0,   430,   452,   452,   517,   453,
       0,   533,   452,  1011,  1015,  1013,   534,   756,     0,   776,
     792,   410,   418,   466,     0,   776,     2,   756,     0,   776,
     733,   514,   515,   531,   546,   552,   555,   550,   556,   574,
     575,     0,   734,   452,   674,     0,   196,   393,   452,     3,
       0,   533,   452,   775,   452,     0,   412,     2,   413,   753,
     432,     0,     0,     0,     2,   452,   792,   452,   756,     0,
       2,     0,   717,   716,   715,   710,   463,     0,   708,   725,
     509,     0,     0,   452,   452,   985,   453,   449,   450,   451,
     989,   980,   981,   987,     2,     2,    96,     0,   945,   959,
    1029,   941,   794,   794,   950,   957,   672,   452,   541,   668,
     453,   537,   538,   542,     0,   794,   995,   453,  1000,   992,
     452,   997,     0,  1027,   636,     0,     0,     0,   452,     0,
     809,   808,   804,   806,   807,   805,     0,   799,   802,     0,
      22,   452,    89,   452,   452,    84,   452,    91,     0,    32,
       0,    33,   452,    87,    88,   452,   452,   452,     2,    98,
      99,     0,     0,   177,     0,     0,   577,     0,  1005,     0,
       0,     0,     0,     0,     0,     0,     0,    50,     0,    56,
      57,    61,     0,     0,    61,     0,    85,    86,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   452,   160,   161,   162,   163,   164,   165,   166,   167,
     168,   169,   170,   158,     0,   156,   157,     2,   921,   681,
     918,   794,   794,   926,   558,   452,   838,   794,   848,   856,
     860,   866,     2,   841,   843,   845,     2,   861,   863,     0,
    1017,  1020,   452,     0,     0,     2,    96,   945,   794,  1029,
     891,   794,   794,  1029,   794,   906,   794,   794,     3,   668,
       0,     0,  1029,  1029,   452,   452,     0,     2,   699,     0,
    1027,   696,  1028,     0,   692,     0,     2,   695,   698,   174,
     173,     0,     2,   452,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   794,   825,   829,   868,   794,
     882,   887,   817,   874,     0,     0,   438,   933,     0,   779,
       0,   452,   780,   431,     0,     0,     0,     0,   429,     2,
     781,     0,   414,   756,     0,   776,     2,   782,     0,     0,
       0,     0,   589,   653,   453,     3,     3,   657,   656,   852,
       0,     0,   452,   394,     0,   533,     3,    95,     3,   452,
       0,     3,   757,     2,   712,   452,   452,   706,   705,   706,
     510,   508,   630,     0,   794,   794,   991,   452,   996,   453,
     452,   982,   452,     0,     0,     0,   960,     0,   794,  1030,
     946,   947,   673,   943,   944,   958,   986,   990,   988,   539,
     574,     0,   994,   999,   633,  1028,     0,     0,   632,     0,
    1027,   738,   736,     0,     0,   801,    61,   762,     0,     2,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   452,
       0,   116,   115,     0,   112,   111,    27,     0,    28,     0,
       0,     0,     0,     3,    61,     0,    46,     0,    47,    54,
       0,    53,    65,     0,    62,    63,    66,    49,     0,    48,
      52,     0,     0,    45,   118,   120,   121,   122,   124,   125,
     127,   128,   132,   133,   130,   131,   135,   136,   138,   140,
     142,   144,   146,     0,     0,   403,     0,     0,    29,     3,
     690,   152,   452,     0,     0,     0,   922,   923,   919,   920,
     731,   730,     2,   842,   844,   846,     2,   862,   864,   452,
     452,   938,   937,     2,     0,     0,     0,     0,     0,   794,
     946,   894,   911,     2,   889,   897,   670,   892,   893,   671,
       2,   904,   914,   907,   908,     0,     3,  1029,   422,     2,
    1022,     2,   661,   662,   640,     3,     3,     3,     3,   684,
       0,   150,     0,     3,     3,     0,   693,     0,   687,     0,
     794,     0,   452,     3,   426,   428,     0,   794,   826,   830,
     794,   883,   888,     2,   818,   821,   823,     2,   875,   878,
     880,   792,     0,   934,     3,   784,     3,   523,   522,   525,
     524,     2,   757,   785,     2,   783,     0,   757,   786,   589,
     589,   589,   452,     0,     0,   675,     0,   397,     0,     0,
     452,     0,     2,     0,     0,     0,     0,     0,   179,     0,
     327,   328,     0,     0,   366,   365,     0,   154,   154,   372,
     550,   556,   193,     0,   180,     0,   204,   181,   182,   452,
     198,   183,   184,   185,   186,     0,   187,   188,   333,     0,
     189,   190,   191,     0,   192,   200,   533,   452,     0,   202,
       0,   391,     0,     0,     0,     3,     0,   764,   757,   745,
     746,     0,     3,   741,     3,     3,     0,   452,   722,   722,
    1027,     0,     0,   993,   998,     2,    95,   452,     3,   548,
       3,   453,     3,   794,   953,   956,   452,     3,   942,   948,
       0,   794,   794,     0,   636,   618,   690,   637,  1027,     0,
       2,   798,   800,     0,    90,   452,     0,    94,    92,   452,
       0,   106,     0,     0,     0,   110,   114,   113,   178,     0,
       0,     0,   690,   103,   171,     0,     0,    41,    42,    79,
       0,    79,    79,     0,    67,    69,    44,     0,     0,    40,
       0,    43,   149,     0,     0,     0,     0,  1027,     3,   794,
     929,   932,   924,   452,   452,     3,     3,     0,   794,   900,
     903,   794,     0,   794,   794,   895,   912,   452,   452,  1023,
       0,   663,   452,   452,     0,     0,     0,     0,   411,     3,
       0,     0,     0,     0,   689,   694,     3,   777,   176,   175,
       3,     0,     0,     2,   819,   822,   824,     2,   876,   879,
     881,   452,   452,   684,   794,     0,     0,     0,   757,   787,
       0,   452,   452,   452,   452,   452,   452,   572,   600,     3,
       3,   601,   533,   590,     0,     0,   834,     2,     0,   395,
      61,     0,     0,   318,   319,   201,   203,     0,     0,     0,
     452,   452,   314,     0,   312,     0,     0,     0,   690,     0,
       0,     0,     0,     0,   155,     0,     0,   373,     0,     0,
       3,   208,     0,   199,     0,   309,     0,     0,     2,     0,
     533,   794,     0,   392,   940,   939,     0,     2,     0,   748,
       2,   743,     0,   744,     0,   726,   707,   711,   709,     0,
       0,     0,   452,     0,     0,     0,     3,     0,     2,   949,
     951,   952,     0,     0,    95,     0,     3,  1027,   624,     0,
     636,   634,  1027,     0,   621,   739,   452,   803,     0,     0,
       0,    34,     0,   107,   109,   108,   105,   104,   690,  1027,
       0,    60,    76,     0,    70,    77,    78,    55,     0,     0,
       0,    64,    51,     0,   148,   402,    30,     0,     0,     2,
     925,   927,   928,     3,     3,     0,     0,   794,     2,   896,
     898,   899,     2,   913,   915,     0,   890,   905,     3,     3,
    1024,     3,   648,   647,   651,  1026,     2,     2,  1025,     0,
       3,   791,   700,   701,     0,     0,   794,   433,   452,   452,
       3,     3,   439,   793,     0,   884,   768,     0,   770,   572,
     572,   572,   607,   577,     0,   613,   601,     0,   452,   564,
     599,   595,     0,     0,     0,     0,   602,   604,   794,   615,
     615,     0,   596,   611,   452,   398,     0,     0,    62,   322,
     323,   320,   321,     0,     0,     2,   219,     0,     0,   221,
     406,   220,   533,   452,   300,   299,   301,     0,     2,   179,
     259,     0,   252,     0,   179,   315,   313,     0,   307,  1027,
     316,     0,     0,     0,   354,   355,   356,   357,     0,   347,
       0,   348,   324,     0,   325,     0,     0,   452,   210,   197,
     311,   310,     0,   345,   364,     0,   396,   794,   452,   766,
     727,   452,     2,     2,   623,  1027,  1027,  1001,  1002,  1003,
       0,   954,   452,     3,     3,     0,   962,     0,     0,     0,
       0,   635,     0,   620,     3,    93,     0,    31,   452,     0,
    1027,     0,     0,    80,     0,    68,     0,    74,     0,    72,
      39,   153,   930,   452,     0,     0,   835,   853,   452,   452,
       0,     0,     0,   452,   452,   703,     0,   419,   421,     3,
       3,     0,     0,     0,   772,   568,   570,   566,     0,   969,
       0,   608,   974,   610,   966,   794,   794,   594,   614,   598,
       0,   597,     0,     0,     0,   617,     0,   794,   591,   605,
     616,   606,   612,   655,   659,   658,     0,     2,     0,     0,
     240,     2,   222,   533,   305,   303,   306,   302,     0,   304,
       0,   248,     0,   179,     0,     2,   452,   260,     0,   285,
       0,     0,   308,     0,     0,     2,   331,   358,     0,   349,
       2,     0,     0,     0,     0,   336,     0,   332,   195,   194,
     420,   742,     0,     0,     0,     0,  1004,     3,     0,     0,
     961,   963,   622,     0,  1027,   638,     2,    37,    35,    36,
       0,    58,   172,    71,     0,     0,     3,   836,   854,     3,
       3,   901,   916,   423,     2,   645,     3,   644,   702,     0,
       0,   827,   885,   935,     0,     0,     0,   970,   971,   794,
     593,   967,   968,   592,   573,     0,     0,   209,   330,     0,
       0,     0,   233,     2,   211,     0,     0,     2,   242,   257,
     268,   262,     2,   179,   297,     0,   272,     0,     0,   263,
     261,   250,   253,     0,     0,   179,   286,     0,     0,   214,
     329,     2,   452,   326,     0,     0,   374,     2,   334,     0,
      61,     0,   346,   747,   749,   626,   628,     0,   964,   965,
    1027,     0,   740,    59,    75,    73,     0,     0,     0,   452,
       0,   828,   886,   794,   977,   979,   972,     0,   603,   228,
     223,   226,     0,   225,   232,   231,     0,   452,   235,   234,
       2,   244,     0,   241,     2,     0,     0,     0,   249,   254,
       0,     0,   179,   298,   273,     0,     0,     2,     0,   288,
     289,   287,   256,   317,     0,   452,   452,     3,   359,   453,
     363,     0,   367,     0,     0,     0,   375,   376,   217,   337,
       0,     2,     0,     2,     2,   955,     0,   627,   931,   902,
     917,   649,     2,   973,   975,   976,   609,     0,   230,     0,
     229,   213,   236,   452,   387,   245,     2,   246,   243,   258,
     271,   269,   265,   277,   275,   276,   274,   255,   270,   266,
     267,   264,   251,     0,     0,     0,     0,   216,   236,     3,
     352,     0,   969,   360,   361,   362,   374,     0,     0,     0,
     374,     0,     2,   335,   342,     0,   339,   341,   625,   452,
     224,   227,     2,     3,   237,   388,   247,     0,     0,     0,
       0,   296,   294,   291,   295,   292,   293,   290,     3,   352,
       0,     0,   970,     0,     0,     0,   368,     0,   377,   218,
       0,   332,     0,     3,   205,     0,     0,     2,   284,   282,
     279,   283,   280,   281,   278,     0,     0,   353,     0,   380,
       0,   378,     0,   380,   338,   340,     2,     0,     0,   207,
     206,   212,     0,   215,     0,   350,   381,     0,     0,   369,
       0,   343,     2,   978,   351,     0,     0,     0,     0,   344,
     382,   383,     0,   379,   370,     0,     0,   371,   384
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1736,  5834,  5225, -1736,    -1,   252,  2101,  -114, -1736,  1604,
   -1736,   344, -1736,  -686,   634,   736,  -931, -1020, -1736,   139,
    6687,  1602, -1736,  1743, -1736,  1317,   576,   693,   699,   409,
     695,  1284,  1286,  1291,  1282,  1292, -1736,  -161,   -96,  8291,
     860, -1736,  1590, -1736, -1736,  -644,  3354, -1068,  2108, -1736,
     222, -1736,   854,   -35, -1736, -1736, -1736,   410,    57, -1736,
   -1627, -1581,   275,    32, -1736, -1736, -1736,   285, -1505, -1736,
   -1458, -1736, -1736, -1736, -1736,   -20, -1717,   163, -1736, -1736,
     -14, -1736, -1736, -1736,     0,   431,   435,   102, -1736, -1736,
   -1736, -1736,  -767, -1736,    34,   -25, -1736,   114, -1736,   -27,
   -1736, -1736, -1736,   861,  -615,  -753, -1315, -1736,    27, -1161,
     378,  1879,  -685,  -681, -1736,  -280, -1736,    12,  -158,   158,
    -300,  -236,  3497,  6748,  -653, -1736,   107,    33,   894,  1984,
   -1736,  1983, -1736,   148,  3505,  -290, -1736, -1736,    80, -1736,
   -1736,   512,   175,  4112,  2294,   -54,  1787,  -250, -1736, -1736,
   -1736, -1736, -1736,  -231,  4372,  5032, -1736,  -360,   183, -1736,
     511,   235, -1736,   167,   710, -1736,   508,  -128, -1736, -1736,
   -1736,  5387,  -547, -1149,  -698,  -243,  -331,  -574, -1736, -1196,
    -147,  -108,  1759,   887,  7627,  -328,  -466,  -247,  -154,  -453,
    1259, -1736,  1578,   349,  1174,  1469, -1736, -1736, -1736, -1736,
     225,  -153,  -237,  -869, -1736,    25, -1736, -1736,   618,   446,
   -1736, -1736, -1736,  2066,  -720,  -451,  -965,   -31, -1736, -1736,
   -1736, -1736, -1736, -1736,    10,  -820,  -131, -1735,  -198,  6974,
     -63,  6220, -1736,  1133, -1736,  2230,  -197,  -172,  -163,  -152,
       8,   -73,   -72,   -59,   386,   -12,     5,    29,  -150,   -70,
    -100,   -95,   -36,  -731,  -722,  -643,  -637,  -706,  -118,  -629,
   -1736, -1736,  -675,  1339,  1340,  1341,  2468,  7083,  -555,  -575,
    -563,  -503,  -741, -1736, -1512, -1639, -1626, -1619,  -585,  -125,
    -227, -1736, -1736,   -71,   429,   -93, -1736,  7551,   375,  1144,
    -496
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,  1143,   214,   384,   385,    81,    82,   386,   361,   387,
    1440,  1441,   388,   963,   964,   965,  1253,  1254,  1255,  1452,
     410,   390,   391,   392,   671,   672,   393,   394,   395,   396,
     397,   398,   399,   400,   401,   402,   403,   412,  1062,   673,
    1375,   734,   208,   736,   406,   801,  1144,  1145,  1146,  1147,
    1148,  1149,  1150,  2035,  1151,  1152,  1380,  1557,  1881,  1882,
    1812,  1813,  1814,  2003,  2004,  1153,  1571,  1572,  1573,  1719,
    1720,  1154,  1155,  1156,  1157,  1158,  1159,  1388,  1746,  1934,
    1852,  1160,  1161,  1589,  2021,  1590,  1591,  1917,  1162,  1163,
    1164,  1378,  1925,  1926,  1927,  2067,  2082,  1952,  1953,   285,
     286,   862,   863,  1116,    84,    85,    86,    87,    88,    89,
     443,    91,    92,    93,    94,    95,   222,   560,   445,   414,
     446,    98,   295,   100,   101,   102,   326,   327,   105,   106,
     167,   107,   881,   328,   153,   110,   242,   111,   154,   251,
     330,   331,   332,   155,   407,   116,   117,   334,   118,   551,
     851,   849,   850,  1529,   335,   336,   121,   122,  1112,  1343,
    1535,  1536,  1680,  1681,  1344,  1524,  1699,  1537,   123,   635,
    1629,   337,   633,   918,  1055,   451,   452,   855,   856,   453,
     454,   857,   339,   555,  1168,   416,   417,   209,   471,   472,
     473,   474,   475,   314,  1188,   315,   879,   877,   585,   316,
     355,   317,   318,   418,   125,   173,   174,   126,  1182,  1183,
    1184,  1185,     2,  1101,  1102,   577,  1177,   127,   305,   306,
     253,   263,   534,   128,   212,   129,   223,  1064,   842,   501,
     165,   130,   646,   647,   648,   131,   225,   226,   227,   228,
     300,   133,   134,   135,   136,   137,   138,   139,   231,   301,
     233,   234,   235,   769,   770,   771,   772,   773,   236,   775,
     776,   777,   739,   740,   741,   742,   502,   140,   610,   611,
     612,   613,   614,   615,  1683,  1684,  1685,  1686,   600,   456,
     342,   343,   344,   419,   200,   142,   143,   144,   346,   793,
     616
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      80,   184,   185,    80,   559,   324,   518,  1186,   972,   132,
     531,   182,   199,    96,   790,   186,   338,   676,   404,   945,
     298,   356,   191,   595,   495,   499,   232,  1035,   177,   893,
    1851,   149,   907,  1042,   104,   952,  1031,  1258,   341,  1794,
     626,   894,  1169,  1025,   629,  1559,   352,   290,   487,   677,
     835,   837,  1795,   901,   477,    80,    80,   488,    80,  1796,
    1369,  1032,   187,  1007,   360,  1431,  1265,   132,   489,  1731,
     490,    96,  1890,    80,  1883,   618,   839,   199,   661,   188,
     589,   112,    80,   405,   586,   455,   846,  1058,   587,   920,
      80,   197,   104,   566,   568,    80,  1492,  1493,    80,   438,
     495,   895,    80,   189,   229,  1073,   515,   254,   103,   422,
     423,   264,  1724,   628,   902,   293,  1884,   631,   210,   589,
     491,  1956,  1026,   424,   487,   492,  1107,   873,  1027,  1299,
     257,   567,   249,   488,  1809,  1810,  1028,  1327,   498,   112,
      80,   595,  1330,    80,   489,    80,   490,   184,   185,   108,
      80,   496,    58,  1178,   132,   607,    80,   485,    96,    97,
      58,   186,   151,    80,  1809,  1810,   103,  1798,   359,    58,
     425,  1175,   922,  1891,   661,   524,   113,   280,   180,   104,
      80,    80,  1593,   261,   493,  -678,  1889,   426,  -776,   197,
     893,   567,  1595,  -389,  1549,    80,   491,   618,   601,  1560,
    1560,   492,   894,   507,   459,   468,  1923,   108,   187,   716,
      80,   427,   901,  1398,  1883,   156,  1811,    97,  1828,    80,
      80,   546,  1957,   184,   185,   188,   112,   496,   529,   197,
    1454,   279,   524,   571,   113,   500,    80,   186,   539,   506,
    1223,   195,   511,   500,   871,    80,  1838,  1165,   163,   189,
    -390,   717,   826,   103,   197,    80,   147,  1096,    80,  1596,
     493,  1594,   895,  -389,   528,    80,  -750,   535,  1246,  1348,
    1833,  1834,  1885,  1036,   538,    80,    80,  1039,    80,  1011,
    1459,  1631,  1559,  1035,   866,  1876,  1052,  1053,  1349,   145,
    1237,   540,   533,   886,   108,    80,    80,   822,   288,  1285,
    1356,  1025,   552,    80,    97,   197,   195,  -751,   171,   171,
      80,    80,  1460,   808,  1851,    80,   601,   906,  1202,   794,
    -390,   113,   809,   928,  1286,   930,   931,   774,   932,  1169,
     912,  1272,   970,   810,   934,   811,  1190,   936,   937,   938,
     199,  1948,  1794,   171,   761,  1209,   618,  1300,    80,   860,
     436,   280,   275,    80,   281,  1795,    80,   645,   249,  1338,
     683,  1947,  1796,  1517,   112,   684,   888,  1900,  1901,   157,
     618,  2002,  1218,   917,  1310,  1889,   822,   618,   162,    90,
    1026,  1301,   150,   833,  1328,   812,  1027,    63,    64,   838,
     813,   562,  1104,   171,  1277,  1301,   171,  2002,   280,   808,
      58,   520,   685,   913,   523,    20,  1984,   686,   809,   171,
    1889,   191,   422,   423,   455,  1551,   350,   324,  1261,   810,
      80,   811,   582,  2037,   823,  1257,   424,  1339,  1268,   459,
     141,  1340,   108,   141,   480,    76,  1560,    90,  1645,  1647,
    1649,   192,    97,    80,    80,  1275,  1276,   947,   893,   814,
     341,   583,   584,  1057,  1057,    80,    80,  1897,   202,   113,
     894,   523,  1420,   171,    80,    58,   468,  1492,  1493,   198,
    1798,   812,  1057,   425,   885,  1209,   813,   247,   176,   508,
     249,   258,   230,   500,    80,   255,   455,   180,   141,   265,
     426,   178,   601,    80,    58,  -910,  1949,  1950,   422,   423,
    1931,  1722,   459,   823,   179,  1355,  1730,   563,   171,   845,
    1876,   947,   424,    80,   427,   947,  1387,   830,   171,    80,
     895,  1906,  1644,   202,    90,  1974,  1351,  1352,   549,   171,
     210,   554,   141,  1932,  1165,   814,    58,   203,  1057,   841,
    1484,  1056,  1056,  1541,   574,   844,   872,  1267,   500,   848,
     180,  1291,  1714,  1715,  1716,   420,   171,    80,  -569,    80,
    1056,   947,  1542,   171,   171,  1560,   180,   198,   171,   195,
      80,  1976,    80,   752,  1717,   141,    80,   500,  1338,  1338,
    1338,   459,   460,  1718,  1348,   132,    80,  1193,    -3,    96,
      80,    80,  1688,   217,   249,   760,    58,  1725,   211,   675,
    -776,   171,  1726,  1606,   618,   996,   171,   198,   871,   171,
     104,  1689,   907,   455,   237,  1012,  1463,   404,  1241,   500,
     279,   525,   428,    80,   280,  1242,  1056,   158,  1061,   205,
     159,   160,   198,   161,  -446,    80,   275,   532,  1066,   618,
     206,  1353,   261,  1541,   112,   536,  1339,  1339,  1339,   774,
    1340,  1340,  1340,   533,   455,  1827,   207,   112,    58,  1692,
    1194,    58,  1691,  1427,  1547,  2056,   546,  1714,  1715,  1716,
      58,   562,   277,  1941,  1075,  1033,   455,   455,   525,   605,
    1133,  1845,  1046,    80,   103,    80,  1846,    80,  1697,  1717,
    1451,    80,   947,  1091,    80,   455,   171,  1257,   603,  1473,
    1474,   890,  2008,   637,    58,   449,   639,  1698,   171,   171,
     947,  1092,   108,  1488,  1489,  1774,   151,  1775,    58,    80,
    1350,  1406,    97,  1799,   947,   108,  1560,  1582,   829,  1732,
      58,  1989,    58,   832,  2010,    97,  1990,   190,    64,   113,
    1040,  1057,  1800,    58,   605,  1558,  1574,  1510,  1511,  1083,
     840,   279,   113,   500,  1560,  1899,   404,    58,    58,   294,
     847,   455,   280,  1432,    80,  1697,    80,  1912,   244,     6,
       7,     8,     9,    10,    11,    12,    13,   947,    80,    58,
     947,   324,   312,  1087,  1803,    80,   202,   500,  -389,  1449,
    2052,   468,  1560,  1212,    80,  2053,  1807,  1313,   279,  1895,
     428,   500,   500,    80,    80,    80,  1642,   460,   940,  1317,
     147,  1418,  1298,   500,   341,   605,   603,   545,    64,   941,
     942,  1217,  1469,    80,   710,   711,   500,   706,   707,  1056,
     354,   428,  1262,   500,  1967,   598,  1478,  1482,   621,   171,
     500,   605,  -765,   882,   884,   359,    14,    15,    16,    17,
      18,   476,   598,   795,   796,  1106,   598,   797,  1942,    80,
      80,   468,   500,  1615,  1616,   357,   508,   358,   818,   890,
     500,   708,   709,   675,    96,   574,   910,   428,   675,   500,
     460,  1033,    80,   428,   675,   605,   718,   249,   171,   429,
     719,   420,   420,  1323,   871,   104,   744,   906,   533,    73,
     745,  1442,  1179,   675,    58,   756,   590,   275,  1061,   500,
      80,  1284,   774,  1305,    80,  1579,  -437,    73,    80,   604,
     702,   703,   249,   605,   645,    14,    15,    16,    17,    18,
      78,   606,   704,   705,  1669,  1670,   956,   737,   958,  -437,
     961,   500,   112,   607,   969,   192,   679,   973,    78,    79,
     699,   618,   859,   598,   430,    90,   860,   700,   701,    80,
     463,  1491,   921,   431,   455,  -450,   587,    80,   432,  1167,
    1422,   433,   998,   434,  -447,  1523,   458,    73,   593,   679,
      73,   462,  1558,    58,    14,    15,    16,    17,    18,   923,
     924,   478,   908,   587,   925,  1640,    80,   604,   267,   468,
    1678,   605,   268,   481,   500,   271,   141,   273,    78,    79,
     108,    78,    79,   420,   482,    14,    15,    16,    17,    18,
      97,   946,    80,   238,   239,   947,   240,   483,    80,    80,
     241,   171,   484,  1181,   449,   356,   356,   113,   171,   949,
     950,  1345,    58,   158,  -451,   497,   159,   160,  1757,   161,
    1399,  1016,  1095,  1048,  1049,   500,  1074,  1070,  1076,    80,
     279,  1071,   498,  1103,   500,   508,  1105,   324,   516,   500,
    1108,  1097,   601,    58,  1099,   947,  1512,   517,   947,  1776,
    1561,   871,   574,  1764,  1779,  1780,   500,   449,   687,  1256,
     688,   689,   690,  1257,    14,    15,    16,    17,    18,  1405,
     341,   527,  1464,   745,  1437,   598,   449,   210,  1257,  1050,
    1051,  1574,  1115,   171,   171,   982,   983,   984,   985,   691,
     420,   468,   692,   693,    80,    80,    80,   694,   695,   598,
    1637,   537,    96,  1708,  1638,   171,   556,   947,   578,   404,
     404,   625,   598,   267,  1539,  -448,  1494,  1734,   468,  1244,
    1071,   947,    58,   104,    80,    14,    15,    16,    17,    18,
     593,    96,    80,  1211,  1930,    80,    80,   171,  1735,    80,
    -909,   171,  1071,  1736,   254,   264,  -619,   947,   636,  1804,
      80,   638,   104,   745,   455,   455,    14,    15,    16,    17,
      18,  1892,  1993,  2054,   257,   947,  1257,   947,  2078,   249,
     112,   649,  2075,   650,  1500,  1501,  1714,  1715,  1716,    80,
    2085,    73,   679,    58,  2086,   468,  1860,   653,   947,  2023,
     654,   267,   268,  2027,   622,    80,   273,  1167,  1717,   112,
     658,   604,   698,   449,   533,   605,   712,  1723,  1259,  1260,
      90,   468,    78,   606,    58,   947,  1263,   261,   713,    80,
     714,   420,   715,  1180,   324,   720,  1167,   640,  -151,  -151,
    1345,  1345,  1345,   435,  1525,  1345,  1050,  1397,   108,   746,
    1341,  1540,  1954,   747,   449,   975,   976,   977,    97,  1457,
    1458,    80,  1462,  1458,  1714,  1715,  1716,   341,  1675,  1676,
    1677,   141,  1331,  1332,  1333,   113,   947,   108,  1466,  1458,
    1954,  1022,  1450,    73,   141,    -3,  1717,    97,  1502,  1450,
    1442,  1022,  1514,  1650,  1071,  -180,   748,  1561,  1584,  1585,
    1586,  1587,  1588,   737,   113,  1772,  1071,   500,  1329,  1539,
     749,   641,  1786,   750,    78,    79,   751,   495,  2005,   -17,
      80,  1354,  1773,  1458,    80,   778,   642,    80,  -449,   643,
     644,    65,    66,    67,    68,    69,    70,    71,  1373,   791,
     487,   149,  1783,  1784,   792,  1793,   947,   468,   802,   488,
    1849,  1850,  1864,  1458,   171,  1865,  1458,   171,   171,   171,
     489,   825,   490,  1809,  1810,  2075,  2076,   468,   815,    80,
    1455,  1456,   816,   104,   104,   978,   979,   817,   535,   598,
     819,   171,   621,   980,   981,   986,   987,   171,   820,    80,
      80,  1700,  1700,  1407,  1408,   554,  -116,  -116,  -116,  -116,
    -116,  -116,   171,   533,   821,  2033,   827,   267,   287,   861,
     876,   843,   491,  -567,  -565,   852,  1704,   492,   874,   880,
     112,   112,   896,   898,   607,   915,   919,   468,   926,   948,
     324,   449,    80,   917,   951,  1021,  1540,    80,    80,    80,
     927,  1494,   171,  1179,   496,   995,   954,  1563,  1563,  1693,
    1000,    14,    15,    16,    17,    18,  1022,  1029,  1438,  1068,
    1077,  1078,  1743,   341,   632,  1079,  1080,  1081,  1082,  1341,
    1341,  1341,   151,  1522,  1526,    62,   493,  1098,    90,   822,
      65,    66,    67,    68,    69,    70,    71,   959,   108,   108,
    -115,  -115,  -115,  -115,  -115,  -115,  1100,  1494,    97,    97,
    -754,   808,  -654,  1109,  1110,    80,  1111,    90,  1171,    58,
     809,    80,  1170,    80,  1203,   113,   113,  1187,  1204,  1205,
      80,   810,  1918,   811,   247,   258,  1215,   960,  1220,   141,
    1221,  1224,   255,   265,   468,   455,   455,  1226,  1227,  1228,
    1229,  1230,   420,  1232,  1233,  1234,  1239,   468,  1240,  1264,
    1247,  1269,   908,  1550,  1552,  1248,    62,  1289,   141,   169,
     170,    65,    66,    67,    68,    69,    70,    71,    73,  1270,
     257,  1271,  1278,   812,  1181,   249,   141,   171,   813,  1279,
     171,  1280,  1281,    83,   468,  -642,   148,  -641,  1678,  1539,
    1304,  1604,   500,  -755,  1324,  1312,  1918,  1346,  1357,    78,
      79,  1360,  1854,  1347,  1377,  1361,   823,  1370,  1844,    80,
     104,    14,    15,    16,    17,    18,   944,  1371,  1179,  1372,
     171,  -677,  1379,   261,    62,    80,   947,    80,   404,    65,
      66,    67,    68,    69,    70,    71,  1381,   814,  1387,  1391,
    1393,    83,   171,   171,  1394,  1395,  1401,  1403,  1434,   618,
    1410,  1411,  1435,    62,  1448,  1465,   181,   112,    65,    66,
      67,    68,    69,    70,    71,    83,    14,    15,    16,    17,
      18,  1236,    80,    75,  1450,    80,   787,  1477,   221,  1490,
    1495,   246,  1496,  1497,  1563,    83,   468,  1498,  1458,   598,
     468,  1503,   150,  1880,  1506,  1515,  1516,  1554,  1530,  1518,
    1612,  1599,  1494,  1528,   468,  1531,  1350,  1597,  1575,  1576,
    1602,  1607,  1924,  1578,   468,  1580,  1540,   449,    90,    90,
    1592,  1600,   148,  1609,  1610,   108,   533,  1613,    83,  1601,
     148,    80,    80,   297,   303,    97,  1614,  1630,  1617,   104,
    1618,   455,    80,   141,  1619,   323,  1621,  1626,   532,  1181,
    1633,  1920,   113,  1636,  1635,  1643,   536,   916,  1651,  1652,
    1639,  1665,   411,   181,   181,   495,   404,  1983,   404,   141,
     141,  1656,  1657,   171,   148,   441,   428,  1502,   246,  1667,
    1674,  1707,  1533,  1687,  1257,    80,   112,   171,  1709,   487,
    1737,  1711,   468,   211,  1180,  1740,   468,  1742,   488,  1747,
     171,   468,   221,   221,  1756,   822,  1760,   404,  1762,   489,
    1761,   490,  1763,  1563,  1765,  1771,  1777,  1788,  1817,   297,
    1778,  1781,  1782,  1791,  1792,  1920,   468,  1822,    83,  1837,
    1847,  2000,  1823,  1880,  1841,  1835,  1843,   171,  1848,  2047,
    1133,   246,  1855,  1856,  1858,   141,  1862,  1859,  1863,   500,
    -643,  1894,  1924,  1871,   108,  1872,  1924,  1924,  1873,  1874,
    1875,   491,   171,  -550,    97,  1902,   492,  1907,  1896,   468,
    1905,   303,  2025,   468,  1921,  1913,  1935,   303,   297,   297,
    1922,   113,  1937,   420,  1938,   148,   468,   404,   184,   185,
    2050,  1784,   496,  1939,  1940,  1951,  1960,    80,   571,    80,
     104,  1977,   186,  1973,   449,   323,   608,   617,  1987,  1975,
     468,  1986,   468,   468,  1065,  2066,  1988,  1991,  1992,  2066,
     247,   258,   323,  1995,  1998,   493,   323,  2011,   104,  2007,
    2009,  2020,   823,  2024,  2026,   468,  2031,  2032,  2038,   171,
    2048,  2080,   194,   171,  2051,  2061,  2049,   112,  2063,  2064,
    2068,  2072,  2077,  2073,  2069,    90,  2083,   171,  2084,   411,
      80,    80,  1768,   682,  1548,  2087,   104,   171,   943,  1180,
     197,   468,   141,  1461,  1563,   112,   988,   991,  1376,   989,
     735,   468,  2062,  1383,   171,   990,  2001,  1744,   992,  1839,
    2018,  1832,  2057,   411,  1933,   171,   738,  2055,  1979,  2046,
    1738,    80,  1563,   181,  1739,  2028,   141,   194,  2070,  1978,
     459,  1392,   168,   112,   468,   108,   468,  1690,   526,   148,
     141,  1878,   194,   441,  1946,    97,  1527,   767,  1701,   617,
    1389,  1067,   798,  1189,  1634,   468,   878,  1751,  1222,   194,
    1563,   468,   113,   108,  1219,   171,     3,     0,  1213,   171,
       0,   468,   444,    97,   171,    80,  1003,  1004,  1005,     0,
       0,     0,     0,   250,     0,    80,     0,   221,     0,     0,
     113,   532,     0,     0,   270,     0,   221,     0,     0,   171,
       0,   108,     0,     0,    90,     0,     0,     0,   655,     0,
       0,    97,     0,     0,     0,     0,   297,     0,   411,   411,
       0,     0,   297,     0,   323,   194,     0,     0,   113,     0,
       0,     0,     0,   696,   697,     0,   250,     0,     0,     0,
       0,     0,   171,     0,     0,     0,   171,     0,     0,    19,
       0,     0,     0,     0,   696,   141,     0,     0,     0,   171,
      62,     0,   297,   169,   170,    65,    66,    67,    68,    69,
      70,    71,  1985,   297,     0,   297,     0,   323,     0,    83,
     250,     0,   194,   171,   696,   171,   171,    48,    49,    50,
      51,    52,    53,    54,    55,   323,   441,     0,   617,     0,
       0,     0,   194,     0,     0,     0,   608,     0,   171,     0,
     608,     0,     0,     0,     0,     0,     0,     0,     0,   323,
     244,     6,     7,     8,     9,    10,    11,    12,    13,   617,
       0,     0,   323,     0,     0,     0,     0,     0,     0,     0,
     148,     0,     0,   250,   171,     0,     0,     0,     0,     0,
       0,     0,     0,   411,   171,   148,   148,     0,   411,     0,
       0,     0,     0,     0,   411,    90,     0,   148,   148,   148,
    2065,     0,     0,   250,     0,     0,     0,     0,     0,   250,
     266,     0,     0,     0,     0,     0,  2074,   171,     0,   171,
     194,     0,     0,    90,     0,   115,     0,     0,   115,     0,
       0,     0,     0,     0,   183,     0,     0,   469,   171,   250,
       0,     0,   858,     0,   171,     0,   141,     0,     0,    58,
     194,     0,     0,   441,   171,     0,   224,     0,  2081,    19,
       0,    90,     0,     0,  1409,     0,     0,     0,  2088,   738,
     738,     0,     0,     0,   141,     0,     0,   411,     0,     0,
      62,     0,     0,   115,   598,    65,    66,    67,    68,    69,
      70,    71,  1433,     0,   441,     0,     0,   767,     0,   767,
       0,    52,    53,    54,    55,     0,     0,   115,    73,     0,
       0,   299,   141,     0,     0,     0,   323,   323,     0,     0,
       0,     0,     0,   252,     0,   194,   194,   115,    74,    75,
       0,   444,     0,     0,   237,   323,     0,   297,     0,    78,
      79,  1467,     0,    62,     0,     0,     0,     0,    65,    66,
      67,    68,    69,    70,    71,   967,   297,     0,   598,     0,
       0,   250,     0,     0,   115,     0,     0,     0,     0,     0,
     115,   974,   115,     0,     0,     0,   252,     0,     0,     0,
     486,   224,     0,     0,   194,     0,   319,   115,   351,     0,
       0,     0,     0,     0,   411,   968,     0,   299,     0,     0,
       0,   323,     0,   444,   415,     0,     0,   148,   411,     0,
       0,     0,     0,     0,     0,     0,   115,   415,     0,   323,
     252,  1197,     0,     0,     0,     0,   194,     0,     0,     0,
       0,     0,   608,    62,     0,   250,   218,   219,    65,    66,
      67,    68,    69,    70,    71,     0,     0,   194,     0,     0,
       0,     0,     0,     0,     0,   250,   572,   299,     0,     0,
       0,    73,     0,     0,     0,     0,     0,   115,     0,     0,
     115,   441,     0,     0,     0,   250,     0,     0,     0,     0,
       0,  1532,    75,   252,     0,     0,     0,     0,  1533,     0,
       0,     0,    78,    79,     0,     0,   788,     0,   469,     0,
     550,  1628,     0,     0,     0,     0,  1632,     0,   115,     0,
     250,     0,     0,   252,     0,     0,     0,     0,     0,   252,
     858,     0,     0,  1641,     0,     0,     0,   115,     0,     0,
     444,     0,     0,     0,   250,     0,     0,     0,   738,     0,
       0,   250,   307,   308,   309,   310,     0,   115,     0,   252,
     115,     0,     0,     0,   194,   767,     0,     0,     0,     0,
       0,     0,   767,     0,   115,     0,     0,     0,   115,     0,
       0,   444,     0,     0,   250,   270,     0,     0,     0,     0,
      62,   858,     0,   169,   170,    65,    66,    67,    68,    69,
      70,    71,     0,   444,   444,     0,     0,     0,     0,     0,
       0,   415,     0,     0,   323,   768,     0,     0,  1231,     0,
       0,    62,   444,  1235,   169,   170,    65,    66,    67,    68,
      69,    70,    71,     0,  1243,     0,     0,     0,     0,   510,
       0,     0,   311,     0,     0,   415,     0,     0,     0,     0,
       0,     0,     0,     0,   148,   807,     0,     0,     0,     0,
     312,     0,   411,  1733,   224,     0,     0,     0,     0,     0,
       0,   115,     0,     0,     0,   415,     0,     0,     0,     0,
       0,   252,     0,     0,   299,     0,     0,     0,   444,     0,
     299,   411,     0,     0,  1362,   194,     0,     0,     0,  1754,
    1755,   858,     0,     0,     0,    62,     0,     0,   246,    83,
      65,    66,    67,    68,    69,    70,    71,     0,   858,   858,
       0,     0,     0,   297,  1770,     0,     0,     0,     0,   148,
     299,     0,     0,    73,     0,     0,     0,     0,     0,   441,
       0,   870,     0,   299,     0,     0,     0,     0,     0,     0,
     415,   415,     0,  1023,    75,   252,   115,   605,   194,     0,
       0,     0,     0,     0,    78,    79,     0,   441,     0,     0,
       0,   148,     0,     0,     0,    62,     0,     0,   250,     0,
      65,    66,    67,    68,    69,    70,    71,   115,     0,   250,
       0,     0,   115,     0,     0,   252,   115,     0,   115,     0,
       0,     0,     0,   655,     0,     0,     0,     0,     0,   115,
       0,   115,     0,   250,     0,     0,     0,     0,     0,     0,
       0,     0,   743,  1282,    75,   351,     0,   115,   415,     0,
     252,     0,     0,   469,   323,   323,   788,     0,   754,     0,
       0,   757,     0,     0,     0,     0,     0,     0,  1861,     0,
       0,   115,     0,     0,   252,     0,     0,     0,   550,     0,
       0,   252,     0,     0,   115,     0,   914,     0,     0,     0,
       0,     0,   115,   148,   148,   148,   148,   148,   148,     0,
     696,     0,     0,  1534,   303,   415,     0,   115,   115,     0,
     415,   444,     0,     0,     0,     0,   415,     0,   510,   115,
     115,   115,   411,   411,   722,   723,   724,   725,   726,   727,
     728,   729,   730,   731,   732,  1443,  1444,  1445,   205,     0,
       0,     0,  1446,  1447,     0,     0,     0,     0,  1384,     0,
       0,     0,   246,     0,     0,  1024,     0,   768,     0,     0,
       0,     0,     0,     0,  1936,   733,     0,     0,     0,     0,
       0,     0,     0,    62,   441,   415,   169,   170,    65,    66,
      67,    68,    69,    70,    71,     0,     0,     0,     0,     0,
       0,     0,   858,   858,     0,   299,     0,     0,   148,   415,
       0,     0,     0,     0,     0,     0,   858,   858,     0,     0,
       0,     0,     0,    58,   299,     0,   415,     0,     0,     0,
       0,  1252,     0,     0,     0,     0,   194,     0,     0,  1252,
       0,     0,     0,     0,     0,     0,   194,     0,   115,   115,
     858,   858,     0,     0,    62,     0,  1385,   218,   219,    65,
      66,    67,    68,    69,    70,    71,     0,   115,  1252,     0,
       0,   469,     0,     0,   194,     0,     0,     0,     0,     0,
       0,    62,    73,     0,   347,   348,    65,    66,    67,    68,
      69,    70,    71,     0,  1679,   115,     0,     0,  1534,     0,
     411,     0,   220,    75,  1534,     0,  1534,     0,     0,     0,
       0,     0,     0,    78,    79,     0,     0,     0,   252,     0,
     250,     0,     0,     0,     0,     0,   415,     0,     0,   252,
       0,  1252,    76,   115,   303,   148,     0,   349,     0,   115,
     415,   444,   444,     0,     0,     0,     0,     0,     0,     0,
       0,   115,     0,  1199,   415,   250,   115,    62,     0,     0,
     169,   170,    65,    66,    67,    68,    69,    70,    71,   411,
       0,     0,     0,     0,     0,   743,   743,     0,     0,     0,
     323,     0,     0,   148,     0,  1014,    62,     0,  1017,   169,
     170,    65,    66,    67,    68,    69,    70,    71,     0,     0,
    1359,     0,     0,   415,     0,     0,     0,     0,     0,     0,
     148,    14,    15,    16,    17,    18,  1366,     0,     0,     0,
       0,     0,     0,  1024,     0,     0,     0,     0,     0,  1283,
     768,     0,     0,     0,     0,   323,   323,   858,   858,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,   510,
    1679,  1679,     0,  1085,     0,     0,     0,  1089,     0,  1364,
       0,   194,     0,     0,     0,  1534,   115,     0,  1534,    58,
       0,    62,     0,  1705,   218,   219,    65,    66,    67,    68,
      69,    70,    71,   115,   115,   303,     0,   469,     0,     0,
       0,     0,     0,     0,     0,     0,   250,     0,   411,    73,
      62,     0,     0,   218,   219,    65,    66,    67,    68,    69,
      70,    71,     0,   469,     0,  1745,     0,     0,     0,  1981,
      75,  1252,     0,   500,     0,   297,     0,     0,    73,     0,
      78,    79,     0,     0,     0,     0,   115,     0,     0,     0,
       0,     0,     0,     0,   250,     0,     0,     0,   765,    75,
       0,   858,   605,     0,     0,     0,     0,     0,     0,    78,
     766,     0,     0,    58,     0,     0,     0,     0,     0,  1679,
       0,     0,     0,     0,     0,     0,   115,   194,  1534,     0,
       0,   299,   858,     0,   415,     0,     0,   858,   858,     0,
       0,     0,     0,     0,    62,     0,     0,   218,   219,    65,
      66,    67,    68,    69,    70,    71,     0,     0,     0,     0,
       0,     0,     0,   415,   148,     0,     0,     0,     0,     0,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
     252,   115,     0,     0,     0,     0,     0,     0,     0,   469,
       0,   323,   296,    75,   743,     0,     0,     0,     0,  1679,
       0,   115,     0,    78,    79,     0,     0,   194,     0,   148,
       0,   415,     0,     0,     0,  1199,  1603,     0,    99,    58,
       0,   152,     0,     0,     0,     0,   109,  1430,     0,     0,
       0,     0,  1485,     0,     0,     0,     0,   148,   148,   415,
    1982,   303,     0,   115,     0,     0,     0,     0,     0,     0,
      62,     0,     0,   218,   219,    65,    66,    67,    68,    69,
      70,    71,   444,   444,     0,  1315,   250,     0,  1319,   469,
       0,     0,     0,     0,  1252,   148,    99,     0,    73,  1252,
    1252,  1252,     0,     0,   109,     0,     0,   115,   115,     0,
       0,  1538,     0,     0,     0,     0,     0,     0,  1532,    75,
     196,   115,   115,  1982,  1982,     0,   115,   115,     0,    78,
      79,     0,  1929,     0,     0,     0,     0,     0,     0,     0,
     259,     0,     0,     0,     0,     0,     0,     0,   260,     0,
       0,     0,     0,     0,     0,   115,   115,     0,     0,     0,
       0,     0,     0,     0,  1982,   115,   115,   115,   115,   115,
     115,     0,     0,     0,     0,     0,   252,   289,     0,     0,
       0,     0,     0,    99,     0,     0,     0,     0,     0,     0,
       0,   109,     0,     0,   415,   415,     0,     0,     0,     0,
     325,     0,     0,  1710,     0,     0,     0,     0,   329,     0,
       0,     0,     0,     0,     0,     0,  1721,     0,   421,     0,
       0,     0,     0,     0,   252,     0,     0,     0,     0,   289,
     447,     0,     0,     0,     0,     0,     0,   250,   448,     0,
       0,     0,     0,     0,     0,     0,   415,     0,     0,     0,
       0,     0,     0,  1749,     0,     0,    62,     0,   494,   169,
     170,    65,    66,    67,    68,    69,    70,    71,     0,     0,
     115,   674,     0,     0,   514,     0,     0,  1471,     0,   519,
     521,     0,     0,   196,     0,     0,  1480,  1252,   444,  1252,
       0,     0,     0,     0,     0,     0,  1538,     0,   858,     0,
       0,     0,  1694,     0,  1538,   541,     0,     0,   543,     0,
     544,     0,   580,   542,     0,     0,     0,     0,     0,     0,
       0,   561,     0,     0,     0,     0,     0,     0,     0,   109,
       0,     0,     0,     0,   573,     0,     0,     0,     0,     0,
       0,    62,   115,   115,   190,    64,    65,    66,    67,    68,
      69,    70,    71,     0,     0,  1808,     0,     0,     0,  1818,
     596,     0,   415,   620,     0,     0,     0,     0,   597,     0,
       0,   260,     0,  1831,     0,     0,     0,   627,   115,     0,
       0,   627,     0,  1840,     0,   597,     0,     0,     0,   597,
      75,     0,     0,   787,     0,     0,   252,   115,     0,     0,
       0,     0,     0,     0,     0,   660,     0,     0,     0,     0,
     834,   836,     0,     0,     0,    14,    15,    16,    17,    18,
     244,     6,     7,     8,     9,    10,    11,    12,    13,    62,
       0,   415,   218,   219,    65,    66,    67,    68,    69,    70,
      71,     0,   115,   250,    62,   115,     0,   218,   219,    65,
      66,    67,    68,    69,    70,    71,   115,     0,     0,     0,
       0,  1888,     0,  1805,     0,  1893,  1538,     0,     0,     0,
    1898,     0,   115,    58,   289,     0,     0,     0,   596,     0,
       0,     0,     0,     0,     0,   883,   597,   115,     0,     0,
       0,     0,   115,   115,     0,  1928,     0,   115,   115,     0,
    1210,   660,     0,     0,    62,     0,     0,   218,   219,    65,
      66,    67,    68,    69,    70,    71,     0,     0,     0,     0,
       0,     0,     0,   299,     0,     0,     0,     0,     0,     0,
    1682,     0,    73,     0,     0,     0,     0,     0,  1955,     0,
       0,     0,  1958,     0,     0,   674,     0,   252,     0,     0,
     674,     0,   220,    75,     0,  1972,   674,     0,     0,   447,
     415,     0,     0,    78,    79,     0,     0,   448,     0,     0,
       0,     0,     0,     0,     0,   674,  1538,     0,     0,  1994,
       0,  1996,  1997,     0,     0,     0,     0,     0,     0,     0,
     854,     0,     0,     0,     0,   521,     0,     0,   329,   865,
       0,   561,     0,     0,  2006,     0,     0,   260,     0,   109,
       0,   994,   325,     0,    99,     0,     0,     0,     0,     0,
     448,     0,   109,     0,     0,     0,     0,     0,     0,     0,
     627,   889,     0,     0,     0,     0,     0,     0,   597,   448,
    2029,     0,     0,     0,     0,   900,     0,     0,     0,     0,
    2034,     0,     0,   114,   596,     0,     0,     0,    58,   909,
       0,     0,   597,     0,     0,     0,     0,   627,     0,     0,
       0,     0,     0,     0,     0,   597,   115,     0,     0,     0,
       0,     0,     0,  2060,     0,  2034,  1682,  1682,   299,    62,
       0,     0,   218,   219,    65,    66,    67,    68,    69,    70,
      71,     0,     0,   115,  2071,     0,     0,     0,     0,     0,
    2060,   114,     0,     0,     0,     0,     0,     0,     0,    62,
    2079,   115,   218,   219,    65,    66,    67,    68,    69,    70,
      71,     0,     0,     0,     0,     0,     0,  1282,    75,     0,
       0,     0,     0,     0,     0,     0,     0,    73,     0,   115,
     115,   572,   299,   252,     0,   262,     0,     0,   447,     0,
      14,    15,    16,    17,    18,     0,   448,   765,    75,     0,
       0,   605,     0,     0,     0,  1006,     0,     0,    78,   766,
       0,     0,     0,     0,     0,     0,     0,   115,     0,     0,
       0,   607,   299,    14,    15,    16,    17,    18,   114,   889,
       0,     0,     0,     0,  1030,  1682,     0,   448,     0,     0,
       0,     0,     0,     0,     0,   333,     0,     0,    58,     0,
       0,   447,   447,     0,     0,     0,     0,     0,     0,   329,
     329,     0,     0,   115,     0,     0,     0,     0,     0,     0,
     447,     0,     0,     0,     0,   450,     0,     0,   329,    62,
       0,    58,   218,   219,    65,    66,    67,    68,    69,    70,
      71,     0,    14,    15,    16,    17,    18,     0,   854,     0,
       0,     0,     0,     0,     0,     0,   329,    73,     0,     0,
       0,  1944,    62,     0,     0,  1682,     0,    65,    66,    67,
      68,    69,    70,    71,     0,     0,     0,  1981,    75,  1166,
       0,   500,     0,     0,     0,     0,   447,   109,    78,    79,
      73,     0,   152,   119,   329,     0,   119,     0,     0,     0,
      58,     0,     0,     0,   627,     0,  1682,  1201,     0,   854,
      74,    75,   597,     0,  1207,   260,   114,   329,     0,     0,
       0,    78,    79,     0,     0,     0,     0,     0,     0,     0,
       0,    62,     0,     0,   218,   219,    65,    66,    67,    68,
      69,    70,    71,     0,     0,     0,     0,     0,     0,  1879,
       0,   119,     0,     0,     0,   599,   325,     0,   262,    73,
       0,     0,     0,     0,   448,     0,     0,     0,     0,  1682,
    1682,     0,   599,     0,     0,   119,   599,     0,    62,   296,
      75,   545,    64,    65,    66,    67,    68,    69,    70,    71,
      78,    79,     0,     0,     0,   119,     0,   362,     0,     0,
       0,   363,     0,   364,     0,     0,     0,     0,     0,     0,
    1682,     0,     0,     0,     0,     0,     0,     0,     0,   854,
     365,     0,     0,     0,     0,     0,     0,   329,     0,     0,
     997,     0,   119,     0,     0,     0,   854,   854,   119,     0,
     119,     0,     0,     0,   329,   329,     0,   366,   367,     0,
     368,     0,   369,    63,    64,    65,    66,    67,    68,    69,
      70,    71,   370,   371,   359,     0,   372,   373,   374,     0,
     375,   376,   119,   599,     0,     0,     0,     0,    73,     0,
       0,     0,     0,     0,   119,     0,     0,     0,     0,   447,
       0,     0,     0,     0,     0,     0,     0,   329,   377,     0,
       0,    76,   378,     0,     0,     0,     0,     0,   379,    78,
      79,   380,   381,   382,   383,   367,     0,   368,     0,   369,
      63,    64,    65,    66,    67,    68,    69,    70,    71,  1342,
       0,     0,     0,     0,     0,   119,     0,  1166,   119,     0,
       0,     0,     0,   119,     0,   109,     0,     0,     0,     0,
       0,     0,     0,     0,   450,     0,     0,     0,     0,    14,
      15,    16,    17,    18,     0,   681,  1166,     0,    76,   378,
       0,     0,     0,     0,   109,     0,   119,     0,     0,     0,
       0,     0,     0,     0,  1390,   333,     0,     0,     0,     0,
       0,     0,   260,     0,   262,   119,   114,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   450,     0,   114,
       0,     0,     0,     0,   596,     0,     0,    58,     0,     0,
       0,     0,   597,   519,     0,   599,   450,     0,     0,     0,
       0,  1553,     0,     0,  1556,  1570,     0,     0,     0,     0,
    1577,     0,   325,     0,  1581,     0,  1583,     0,    62,   599,
     448,   218,   219,    65,    66,    67,    68,    69,    70,    71,
       0,     0,   599,     0,     0,     0,     0,     0,     0,   119,
       0,     0,     0,     0,     0,     0,    73,    62,     0,     0,
     547,   548,    65,    66,    67,    68,    69,    70,    71,     0,
     854,   854,     0,     0,     0,     0,  1532,    75,   329,   329,
       0,     0,     0,   119,   854,   854,     0,    78,    79,   447,
     447,     0,   329,   329,     0,     0,    62,   329,   329,   218,
     219,    65,    66,    67,    68,    69,    70,    71,    76,   119,
       0,     0,     0,     0,     0,     0,     0,     0,   854,   854,
       0,     0,     0,     0,    73,     0,   329,   329,  1342,  1342,
    1342,   152,    62,   450,     0,   218,   219,    65,    66,    67,
      68,    69,    70,    71,  1532,    75,     0,     0,     0,     0,
       0,  1533,     0,     0,     0,    78,    79,  1562,  1562,     0,
      73,     0,     0,     0,     0,   109,   109,     0,  1673,     0,
       0,     0,     0,     0,   450,     0,     0,     0,     0,     0,
    1981,    75,     0,     0,   500,     0,     0,     0,   119,   119,
       0,    78,    79,     0,     0,     0,   333,   333,     0,     0,
    1706,     0,     0,     0,     0,     0,     0,     0,     0,   325,
       0,     0,  1712,     0,     0,   333,     0,   448,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1727,  1729,     0,
     119,     0,     0,   152,   119,     0,   119,     0,     0,     0,
       0,     0,     0,   333,     0,     0,     0,     0,     0,   119,
       0,  1556,    62,     0,     0,   218,   219,    65,    66,    67,
      68,    69,    70,    71,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   114,     0,     0,     0,    62,     0,
      73,   333,     0,    65,    66,    67,    68,    69,    70,    71,
    1249,     0,     0,     0,  1250,     0,  1251,     0,     0,   599,
     220,    75,   262,     0,   333,   854,   854,     0,     0,     0,
     119,    78,    79,   329,   329,     0,     0,     0,     0,     0,
       0,     0,     0,   119,     0,   119,   119,    75,   119,     0,
    1453,  1696,     0,   120,   119,     0,   120,   119,   119,   119,
       0,   854,     0,     0,     0,     0,     0,     0,     0,   329,
       0,   450,     0,     0,     0,     0,     0,     0,     0,     0,
    1713,     0,     0,  1816,     0,     0,     0,     0,   260,     0,
       0,     0,  1819,     0,  1821,     0,     0,  1826,  1830,     0,
    1570,     0,     0,     0,     0,  1836,     0,     0,     0,     0,
       0,   120,     0,     0,  1562,     0,     0,     0,     0,     0,
       0,     0,   109,     0,     0,   325,     0,     0,   152,     0,
       0,     0,     0,   329,   333,   120,     0,   119,     0,   854,
       0,     0,     0,     0,     0,     0,     0,   329,     0,     0,
       0,   333,   333,     0,     0,   120,     0,    62,     0,     0,
     218,   219,    65,    66,    67,    68,    69,    70,    71,     0,
     854,     0,     0,     0,     0,   854,   854,     0,   329,     0,
     447,   447,     0,   329,   329,    73,     0,     0,   329,   329,
       0,     0,   120,     0,     0,     0,  1797,     0,   120,     0,
     120,     0,  1904,     0,   333,   296,    75,  1909,  1911,     0,
       0,     0,     0,    62,     0,     0,    78,    79,    65,    66,
      67,    68,    69,    70,    71,  1249,     0,     0,     0,  1250,
      62,  1251,   120,   169,   170,    65,    66,    67,    68,    69,
      70,    71,     0,  1562,   120,     0,     0,     0,     0,     0,
       0,   109,   114,     0,   119,     0,     0,     0,     0,     0,
       0,     0,    75,     0,     0,  1646,     0,   119,   119,  1959,
       0,  1962,     0,     0,  1964,  1966,     0,     0,    62,  1969,
    1971,   114,     0,    65,    66,    67,    68,    69,    70,    71,
    1249,     0,     0,     0,  1250,   120,  1251,     0,   120,   262,
       0,    62,     0,   120,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    62,     0,     0,   169,   170,    65,    66,
      67,    68,    69,    70,    71,     0,     0,    75,     0,   599,
    1648,     0,   204,     0,     0,     0,   120,     0,   215,   216,
       0,     0,     0,     0,     0,     0,     0,     0,  2013,  2015,
    2017,     0,    76,     0,     0,   120,     0,   450,     0,  1919,
       0,   458,     0,     0,     0,     0,     0,   597,     0,  2030,
      62,     0,   278,   169,   170,    65,    66,    67,    68,    69,
      70,    71,  2040,  2042,  2044,     0,   447,     0,     0,     0,
       0,     0,     0,     0,   329,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1562,   333,   333,     0,   124,     0,
       0,   124,   109,     0,     0,     0,     0,     0,   462,   333,
     333,     0,     0,     0,   333,   333,     0,     0,     0,   120,
       0,     0,  1562,  1919,     0,     0,     0,     0,     0,    62,
     109,   597,     0,     0,    65,    66,    67,    68,    69,    70,
      71,  1249,     0,   333,   333,  1250,     0,  1251,     0,     0,
       0,     0,     0,   120,     0,     0,   124,     0,     0,     0,
    1562,     0,     0,     0,     0,     0,     0,     0,   109,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    75,   120,
     124,     0,   114,   114,     0,     0,     0,     0,     0,  2022,
       0,     0,     0,     0,   119,     0,     0,     0,     0,     0,
     124,     0,   119,     0,     0,     0,   854,     0,     0,     0,
       0,     0,     0,     0,   329,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   569,     0,
       0,   119,     0,     0,   450,     0,     0,   124,     0,     0,
       0,     0,     0,   124,     0,   124,     0,     0,     0,   119,
       0,     0,     0,     0,     0,     0,     0,     0,   120,   120,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   119,
       0,     0,     0,     0,     0,     0,     0,   124,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   124,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     120,     0,     0,     0,   120,     0,   120,     0,     0,     0,
       0,   119,     0,     0,     0,     0,     0,     0,     0,   120,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     333,   333,     0,     0,     0,     0,     0,     0,     0,     0,
     124,     0,     0,   124,     0,     0,     0,     0,   124,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   333,     0,     0,     0,
       0,     0,     0,     0,     0,   763,     0,   764,     0,     0,
     120,   124,     0,     0,     0,   262,   780,   781,     0,     0,
       0,     0,     0,   120,     0,   120,   120,     0,   120,     0,
     124,     0,     0,     0,   120,     0,     0,   120,   120,   120,
       0,     0,     0,   119,   119,   119,   119,   119,   119,   114,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     333,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   119,   119,   333,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   124,   333,     0,     0,     0,     0,
     333,   333,     0,     0,     0,   333,   333,   120,     0,     0,
       0,     0,     0,     0,   864,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   124,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   119,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   124,     0,     0,     0,     0,  1334,
       0,     0,     0,     0,     1,     0,  1335,   146,   114,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,    47,     0,
       0,     0,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,   120,     0,     0,    58,  1336,     0,
     119,     0,     0,   124,   124,     0,     0,   120,   120,     0,
       0,   193,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    61,     0,     0,
       0,    63,    64,     0,     0,   119,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   124,     0,     0,     0,   124,
       0,   124,     0,     0,   599,     0,    73,     0,     0,     0,
       0,     0,     0,     0,   124,     0,     0,     0,     0,   119,
     284,     0,     0,     0,     0,  1337,     0,     0,     0,    76,
     929,   333,     0,   119,     0,     0,     0,    78,    79,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   114,
       0,     0,     0,  1045,     0,     0,     0,     0,     0,     0,
     119,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   124,     0,   114,   599,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   124,     0,
     124,   124,     0,   124,     0,     0,     0,     0,     0,   124,
       0,     0,   124,   124,   124,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   284,   114,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   522,
    1113,  1114,     0,     0,     0,     0,     0,     0,     0,   284,
       0,  1172,  1173,  1174,     0,     0,  1176,     0,   119,   284,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   333,     0,   553,   557,     0,     0,     0,     0,     0,
     564,   565,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   124,     0,     0,     0,   575,     0,     0,     0,
       0,     0,     0,     0,   120,     0,     0,     0,     0,     0,
       0,     0,   120,     0,     0,     0,   594,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1245,     0,
       0,   120,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   120,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   680,     0,     0,     0,     0,     0,     0,   120,
       0,     0,     0,     0,  1266,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   721,     0,     0,     0,     0,     0,   124,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   119,
       0,   120,   124,   124,     0,     0,     0,     0,   166,   759,
       0,  1290,     0,   762,     0,     0,     0,     0,     0,     0,
    1294,  1295,  1296,  1297,     0,     0,     0,   119,  1302,  1303,
       0,     0,   784,     0,   166,     0,   785,   786,  1311,     0,
     789,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   803,   804,   805,   806,  1325,
       0,  1326,     0,     0,     0,   119,     0,     0,     0,     0,
       0,     0,     0,     0,   828,     0,     0,     0,     0,     0,
     166,     0,   831,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   166,     0,   166,     0,     0,     0,     0,
       0,     0,     0,   120,   120,   120,   120,   120,   120,     0,
     284,     0,     0,     0,  1382,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   353,     0,     0,     0,     0,
       0,     0,   120,   120,     0,     0,     0,     0,     0,     0,
    1396,   869,   353,     0,     0,     0,     0,  1400,   553,  1402,
    1404,     0,     0,     0,   875,     0,     0,     0,     0,     0,
       0,  1413,     0,  1414,     0,  1415,     0,  1417,     0,     0,
       0,     0,  1425,     0,     0,     0,     0,     0,   892,   897,
     166,     0,     0,     0,   166,     0,     0,   166,   166,     0,
       0,   166,     0,     0,   166,   166,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   120,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1468,     0,     0,     0,     0,     0,   124,
    1475,  1476,   939,     0,     0,     0,     0,   124,     0,     0,
       0,     0,     0,     0,     0,     0,   166,     0,     0,   166,
       0,     0,     0,     0,  1499,     0,     0,     0,     0,     0,
       0,  1504,     0,     0,     0,  1505,   124,     0,     0,     0,
     166,   166,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   124,     0,   166,     0,     0,     0,
     120,     0,     0,     0,     0,   215,     0,     0,     0,     0,
       0,  1002,     0,     0,   124,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1019,     0,     0,     0,
    1020,     0,     0,     0,     0,   120,     0,     0,     0,   892,
       0,     0,     0,     0,     0,  1598,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   124,     0,     0,     0,
       0,  1060,     0,     0,     0,     0,     0,     0,     0,   120,
    1069,     0,     0,     0,     0,     0,  1072,     0,     0,     0,
       0,  1620,     0,   120,     0,     0,     0,     0,     0,  1625,
     166,  1627,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     120,     0,     0,     1,     0,     0,     0,     0,     0,     0,
       1,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1654,  1655,
       0,     0,     0,     0,     0,   353,     0,     1,     0,     0,
       0,     0,     0,  1660,  1661,     0,  1662,   166,   124,   124,
     124,   124,   124,   124,     0,  1666,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1671,  1672,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   124,   124,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   120,     0,
       0,     0,     0,  1225,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   243,   244,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     0,     0,
      20,   353,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,  -453,  -453,     0,
    -453,    46,    47,   124,  -453,     0,     0,     0,     0,     0,
       0,     0,   166,   166,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,   166,  1273,   248,  1758,  1759,
    1274,     0,     0,     0,     0,     0,     0,   892,   269,  1766,
     272,     0,   274,     0,     0,     0,   389,  1287,     0,     0,
       0,     0,     0,     0,  1288,    63,    64,     0,     0,     0,
       0,     0,     0,  1292,     0,  1293,     0,     0,     0,     0,
       0,     0,     0,     0,  1789,  1790,     0,     0,     0,     0,
     248,     0,   272,   274,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   124,     0,  1321,     0,   120,
       0,  1322,     0,    76,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   146,     0,     0,     1,     0,
       0,     0,     0,     0,   248,     0,     0,   120,     0,     0,
     124,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   166,   166,     0,     0,     0,     0,   166,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1857,     0,   124,   120,     0,     0,   166,     0,
       0,   166,   166,     0,   166,     0,   166,   166,   124,     0,
       0,  1866,     0,     0,  1867,  1868,     0,   248,     0,   272,
     274,  1870,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   164,     0,     0,   124,     0,     0,     0,  1412,
       0,     0,     0,     0,     0,   166,     0,   248,     0,   166,
       0,     0,     0,   248,     0,     0,     0,     0,     0,   652,
       0,     0,   389,   657,  1436,     0,     0,     0,     0,     0,
       0,     0,   663,   664,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   248,     0,     0,     0,   389,   389,   623,
       0,   274,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   276,     0,     0,     0,   389,     0,
       0,     0,     0,     0,   166,   166,     0,   282,     0,   283,
       0,     0,     0,   124,     0,     0,     0,     0,   166,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   389,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1980,     0,     0,     0,     0,  1508,     0,     0,
       0,  1509,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   248,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1544,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   248,     0,   623,   274,     0,
       0,   504,   505,     0,  2019,   509,     0,     0,   512,   513,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  2036,     0,
       0,  1608,     0,     0,  1611,     0,     0,     0,     0,     0,
       0,     0,   248,  2045,     0,     0,   340,     0,     0,   166,
       0,     0,  1622,     0,     0,     0,     0,     0,  2058,     0,
       0,     0,   248,     0,     0,     0,     0,   248,     0,   248,
       0,     0,     0,     0,   124,   437,   340,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   248,
     166,   248,   248,     0,   591,   592,     0,   166,     0,     0,
     166,     0,   124,  1653,     0,     0,     0,   503,     0,   248,
     624,     0,  1658,     0,   503,     0,  1659,     0,     0,     0,
       0,   248,     0,     0,     0,     0,     0,     0,     0,     0,
    1663,  1664,     0,     0,     0,     0,     0,     0,     0,     0,
     124,     0,     0,     0,   248,     0,   623,   274,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   248,   623,
       0,     0,     0,     0,     0,   248,     0,     0,     0,     0,
       0,     0,   503,     0,     0,   389,   389,   389,   389,   389,
     389,   389,   389,   389,   389,   389,   389,   389,   389,   389,
     389,   389,   389,   389,   753,     0,   340,   609,   248,   269,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   166,     0,     0,     0,   630,     0,     0,
       0,   166,   166,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1752,  1753,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   389,     0,     0,     0,     0,
       0,   824,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   166,
       0,     0,     0,     0,     0,     0,     0,   503,   166,     0,
       0,   166,     0,   166,   166,     0,     0,     0,     0,     0,
       0,     0,     0,   503,   755,     0,   503,   758,     0,     0,
       0,     0,     0,     0,   340,     0,     0,     0,   609,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   166,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   503,
       0,     0,     0,   503,  1842,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   903,   904,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   911,
    1611,     0,   248,     0,   389,   340,     0,     0,     0,     0,
       0,   166,     0,   248,     0,     0,     0,     0,  1869,     0,
       0,     0,   389,     0,     0,     0,     0,   389,     0,     0,
       0,     0,     0,     0,   201,     0,     0,   248,   389,     0,
       0,     0,     0,     0,     0,  1887,     0,     0,   248,     0,
     256,     0,     0,     0,     0,   503,     0,   248,   340,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1915,     0,     0,  1916,   887,   340,     0,     0,
     389,     0,     0,   172,   175,     0,     0,   609,     0,     0,
       0,   609,     0,     0,     0,     0,     0,   166,   905,   201,
     340,     0,     0,   304,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   345,  1008,  1009,     0,   213,     0,
       0,  1013,     0,     0,     0,     0,   166,     0,     0,     0,
       0,   201,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1034,     0,   457,  1037,  1038,   461,  1041,     0,
    1043,  1044,   166,     0,   248,     0,     0,     0,   166,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   291,     0,
       0,   292,     0,     0,     0,     0,  1999,     0,   248,     0,
       0,     0,     0,     0,   313,     0,     0,     0,     0,  1084,
       0,     0,     0,  1088,     0,     0,     0,   201,     0,     0,
       0,     0,     0,     0,   340,     0,     0,   389,     0,     0,
     256,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     503,   503,     0,     0,     0,     0,     0,   166,     0,     0,
     503,  1015,     0,   503,  1018,     0,     0,     0,   479,     0,
       0,     0,     0,     0,     0,   340,   461,     0,   609,     0,
     609,   609,     0,     0,   201,     0,     0,   609,  1191,  1192,
       0,     0,     0,     0,     0,     0,     0,   340,   340,     0,
       0,     0,  1208,     0,   602,     0,   619,     0,     0,     0,
       0,     0,     0,   530,   389,     0,   340,     0,     0,     0,
     503,     0,     0,   172,   503,     0,     0,     0,   503,  1086,
       0,     0,   503,  1090,   172,   166,   166,     0,     0,     0,
    1093,     0,     0,   353,   248,     0,     0,   166,     0,   389,
     389,   389,     0,     0,     0,     0,   389,   389,   678,     0,
       0,   576,     0,     0,     0,     0,     0,     0,   579,   581,
       0,     0,     0,   588,     0,     0,     0,     0,     0,   248,
     389,     0,   340,   503,     0,   248,     0,     0,     0,     0,
       0,     0,   201,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   634,     0,     0,     0,
       0,   313,     0,   609,   313,     0,     0,   389,   389,     0,
       0,     0,   602,     0,     0,     0,     0,     0,   779,     0,
       0,     0,     0,  1208,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   166,
       0,     0,   340,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1307,     0,     0,     0,     0,     0,
       0,  1314,     0,     0,  1318,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   201,   201,     0,
       0,   213,     0,   457,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   782,   783,     0,     0,     0,     0,   503,
     248,     0,     0,   166,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   609,   609,     0,     0,
       0,     0,     0,   609,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   345,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   248,     0,
       0,     0,     0,     0,     0,   457,     0,   891,     0,     0,
       0,     0,     0,     0,     0,   340,     0,     0,     0,     0,
     503,  1316,     0,   503,  1320,     0,     0,     0,   602,     0,
       0,     0,     0,     0,     0,     0,     0,  1419,     0,     0,
       0,     0,     0,     0,     0,  1428,  1429,     0,     0,   201,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   166,   678,     0,   678,   678,     0,   678,     0,     0,
       0,     0,     0,   678,   313,     0,   678,   678,   678,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1470,     0,     0,     0,     0,     0,     0,
       0,     0,  1479,     0,     0,  1483,     0,  1486,  1487,     0,
       0,     0,     0,   634,     0,     0,     0,     0,     0,     0,
       0,     0,   457,     0,     0,     0,     0,     0,     0,     0,
     340,     0,     0,     0,     0,     0,   609,  1421,     0,   389,
       0,     0,     0,     0,     0,     0,   201,     0,  1513,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   340,     0,
     248,     0,     0,   457,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   457,   457,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   503,  1472,   457,     0,     0,     0,     0,     0,
       0,   503,  1481,     0,   609,  1605,     0,     0,     0,     0,
      14,    15,    16,    17,    18,   340,   340,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,  1047,     0,    46,    47,
       0,     0,     0,  1059,     0,     0,     0,     0,     0,     0,
     457,     0,     0,     0,     0,     0,     0,   201,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     779,     0,     0,     0,   248,     0,     0,     0,     0,     0,
       0,  1483,     0,     0,  1206,     0,     0,     0,     0,   659,
       0,   248,    14,    15,    16,    17,    18,     0,     0,     0,
       0,   413,     0,     0,     0,     0,     0,     0,     0,     0,
    1668,     0,     0,     0,   442,     0,     0,     0,  1117,     0,
     345,     0,     0,     0,     0,   340,   389,   470,   362,   470,
       0,     0,   363,     0,   364,     0,     0,     0,     0,     0,
     634,     0,     0,     0,   -16,     0,     0,     0,     0,     0,
      58,   365,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   389,     0,     0,     0,
       0,     0,  1214,     0,     0,   248,   634,     0,   366,   367,
       0,   368,     0,   369,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   370,   371,   359,     0,   372,   373,   374,
       0,   375,   376,     0,     0,     0,     0,     0,     0,    73,
       0,  1750,     0,     0,     0,   570,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   377,
       0,     0,    76,   378,     0,   503,     0,     0,     0,   379,
     440,    79,   380,   381,   382,   383,     0,     0,     0,     0,
       0,   503,     0,   457,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   389,     0,   389,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1801,
    1802,     0,     0,   678,     0,     0,     0,   248,     0,     0,
       0,  1806,     0,     0,     0,   389,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   340,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   389,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   256,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   201,     0,
       0,     0,     0,     0,     0,     0,   340,   340,   602,     0,
       0,     0,  1363,  1365,  1367,   389,     0,     0,   470,     0,
       0,   503,   503,     0,   470,     0,     0,     0,     0,   800,
     248,     0,     0,  1877,     0,     0,   345,   503,     0,     0,
     678,     0,  1386,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    14,    15,    16,    17,    18,  1117,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,    47,     0,     0,     0,     0,     0,   634,     0,     0,
       0,     0,     0,   457,   457,     0,     0,  1943,     0,     0,
      58,     0,     0,     0,     0,     0,   868,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     503,   659,   678,   678,   678,   442,   678,   678,   503,     0,
       0,     0,     0,   461,     0,     0,     0,     0,   899,     0,
       0,    14,    15,    16,    17,    18,     0,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,  -453,  -453,     0,  -453,    46,
      47,   256,  -453,     0,     0,     0,     0,     0,     0,   933,
       0,     0,   340,     0,     0,     0,   503,  1945,     0,    58,
     503,     0,     0,   345,     0,     0,     0,     0,     0,     0,
     800,   953,  1543,     0,   955,  1545,   957,     0,     0,     0,
       0,     0,   966,     0,   971,   966,     0,     0,     0,     0,
       0,     0,     0,    63,    64,     0,     0,     0,     0,     0,
       0,   503,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   999,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1001,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1010,   634,   634,     0,
       0,    76,     0,     0,     0,     0,     0,     0,     0,     0,
     442,     0,     0,   999,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   503,   503,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   201,
    1063,     0,     0,   470,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   243,   503,     0,     0,     0,     0,
       0,     0,     0,   256,    14,    15,    16,    17,    18,  1094,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,  -453,  -453,
       0,  -453,    46,    47,     0,  -453,     0,     0,     0,   345,
       0,     0,     0,     0,     0,     0,     0,   413,  1702,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
    1198,  1200,     0,     0,     0,     0,     0,     0,   442,   678,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    63,    64,     0,     0,
       0,     0,     0,     0,   457,   457,     0,   966,     0,     0,
       0,     0,     0,  2059,     0,     0,     0,     0,     0,     0,
     999,    73,     0,     0,     0,     0,     0,     0,  1238,     0,
    1358,     0,     0,     0,     0,   966,     0,     0,     0,     0,
       0,     0,     0,     0,    76,   302,     0,   634,     0,     0,
       0,     0,    78,    79,   256,     0,     0,     0,     0,     0,
       0,   362,     0,     0,     0,   363,     0,   364,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   470,  1119,     0,   365,    -2,     0,  1121,  -238,  -238,
    1122,  1123,  1124,  1125,  1126,  1127,  1128,  1129,  1130,  1131,
    1132,  1133,  -332,  1134,  1135,  1136,  1137,  1138,     0,  1139,
       0,   366,   367,     0,   464,     0,   369,  1140,  1141,    65,
      66,    67,    68,    69,    70,    71,   370,   371,   359,  1142,
     372,   373,   374,     0,   375,   376,     0,     0,     0,     0,
       0,     0,    73,     0,     0,     0,     0,     0,   470,     0,
    1306,     0,  1309,     0,     0,     0,     0,     0,     0,     0,
       0,  -238,   377,     0,     0,    76,   378,     0,     0,  1853,
     280,     0,   379,    78,    79,   380,   381,   382,   383,     0,
     634,     0,     0,   678,     0,     0,     0,  -179,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     457,     0,     0,    14,    15,    16,    17,    18,  1374,  1374,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,  -452,  -452,     0,
    -452,    46,    47,     0,  -452,     0,     0,   678,     0,     0,
     461,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    58,     0,  2059,     0,     0,     0,     0,     0,     0,
       0,     0,  1416,     0,     0,     0,     0,     0,  1426,     0,
    1358,     0,     0,     0,     0,     0,     0,   470,     0,     0,
       0,     0,     0,     0,     0,     0,   442,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   362,     0,   470,     0,   363,     0,   364,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   966,     0,
       0,   800,  1119,     0,   365,    -2,     0,  1121,  -239,  -239,
    1122,  1123,  1124,  1125,  1126,  1127,  1128,  1129,  1130,  1131,
    1132,  1133,  -332,  1134,  1135,  1136,  1137,  1138,     0,  1139,
       0,   366,   367,     0,   464,     0,   369,  1140,  1141,    65,
      66,    67,    68,    69,    70,    71,   370,   371,   359,  1142,
     372,   373,   374,  1507,   375,   376,     0,     0,     0,     0,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -239,   377,     0,     0,    76,   378,     0,     0,     0,
     280,   966,   379,    78,    79,   380,   381,   382,   383,     0,
       0,     0,     0,     0,     0,     0,     0,  -179,     0,   470,
       0,     0,   800,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1748,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1358,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   953,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1623,  1624,     0,     0,     0,     0,     0,
       0,   362,     0,     0,     0,   363,     0,   364,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   470,
       0,   800,  1119,     0,   365,    -2,     0,  1121,     0,     0,
    1122,  1123,  1124,  1125,  1126,  1127,  1128,  1129,  1130,  1131,
    1132,  1133,  -332,  1134,  1135,  1136,  1137,  1138,     0,  1139,
       0,   366,   367,     0,   464,     0,   369,  1140,  1141,    65,
      66,    67,    68,    69,    70,    71,   370,   371,   359,  1142,
     372,   373,   374,   362,   375,   376,     0,   363,     0,   364,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   365,     0,     0,   413,
       0,     0,   377,     0,  1695,    76,   378,     0,     0,     0,
     280,     0,   379,    78,    79,   380,   381,   382,   383,     0,
       0,     0,     0,   366,   367,     0,   368,  -179,   369,    63,
      64,    65,    66,    67,    68,    69,    70,    71,   370,   371,
     359,     0,   372,   373,   374,     0,   375,   376,     0,     0,
       0,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1741,     0,     0,     0,  1564,  1565,
    1566,     0,     0,     0,   377,  1728,     0,    76,   378,     0,
       0,     0,     0,     0,   379,    78,    79,   380,   381,   382,
     383,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1767,     0,     0,
    1769,     4,   244,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,  1118,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,   362,     0,    46,
      47,   363,     0,   364,    48,    49,    50,    51,    52,    53,
      54,    55,    56,     0,     0,     0,    57,     0,  1119,    58,
    1120,    -2,     0,  1121,     0,     0,  1122,  1123,  1124,  1125,
    1126,  1127,  1128,  1129,  1130,  1131,  1132,  1133,  -332,  1134,
    1135,  1136,  1137,  1138,     0,  1139,     0,   366,   367,    61,
     464,     0,   369,  1140,  1141,    65,    66,    67,    68,    69,
      70,    71,   370,   371,   359,  1142,   372,   373,   374,     0,
     375,   376,     0,     0,     0,     0,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    -3,   377,     0,
       0,    76,   409,     0,     0,     0,   280,     0,   379,    78,
      79,   380,   381,   382,   383,     0,     0,     0,     0,     0,
       0,     0,     0,  -179,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     4,   244,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,  1118,     0,
      20,   966,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,   362,
       0,    46,    47,   363,     0,   364,    48,    49,    50,    51,
      52,    53,    54,    55,    56,     0,     0,     0,    57,     0,
    1119,    58,  1120,    -2,     0,  1121,     0,     0,  1122,  1123,
    1124,  1125,  1126,  1127,  1128,  1129,  1130,  1131,  1132,  1133,
    -332,  1134,  1135,  1136,  1137,  1138,     0,  1139,     0,   366,
     367,    61,   464,     0,   369,  1140,  1141,    65,    66,    67,
      68,    69,    70,    71,   370,   371,   359,  1142,   372,   373,
     374,     0,   375,   376,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     377,     0,     0,    76,   409,     0,     0,     0,   280,     0,
     379,    78,    79,   380,   381,   382,   383,     0,     0,     0,
       0,     0,     0,     0,     0,  -179,     4,   244,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   362,     0,    46,    47,   363,     0,   364,    48,
      49,    50,    51,    52,    53,    54,    55,    56,     0,     0,
       0,    57,     0,     0,    58,   365,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   366,   367,    61,   368,     0,   369,    63,    64,
      65,    66,    67,    68,    69,    70,    71,   370,   371,   359,
       0,   372,   373,   374,     0,   375,   376,     0,     0,     0,
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1564,  1565,  1566,
       0,     0,     0,   377,  1567,  1568,    76,   409,     0,     0,
       0,     0,     0,   379,    78,    79,   380,   381,   382,   383,
       0,     0,     0,     0,     0,     0,     0,     0,  1569,     4,
     244,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,   362,     0,    46,    47,   363,
       0,   364,    48,    49,    50,    51,    52,    53,    54,    55,
      56,     0,     0,     0,    57,     0,     0,    58,   365,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   366,   367,    61,   368,     0,
     369,    63,    64,    65,    66,    67,    68,    69,    70,    71,
     370,   371,   359,     0,   372,   373,   374,     0,   375,   376,
       0,     0,     0,     0,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1564,  1565,  1566,     0,     0,     0,   377,  1567,     0,    76,
     409,     0,     0,     0,     0,     0,   379,    78,    79,   380,
     381,   382,   383,     0,     0,     0,     0,     0,     0,     0,
       0,  1569,     4,   244,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,   362,     0,
      46,    47,   363,     0,   364,    48,    49,    50,    51,    52,
      53,    54,    55,    56,     0,     0,     0,    57,     0,     0,
      58,   365,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   366,   367,
      61,   368,     0,   369,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   370,   371,   359,     0,   372,   373,   374,
       0,   375,   376,     0,     0,     0,     0,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   377,
       0,  1555,    76,   409,     0,     0,     0,     0,     0,   379,
      78,    79,   380,   381,   382,   383,     4,   244,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   362,     0,    46,    47,   363,     0,   364,    48,
      49,    50,    51,    52,    53,    54,    55,    56,     0,     0,
       0,    57,     0,     0,    58,   365,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   366,   367,    61,   368,     0,   369,    63,    64,
      65,    66,    67,    68,    69,    70,    71,   370,   371,   359,
       0,   372,   373,   374,     0,   375,   376,     0,     0,     0,
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   377,     0,     0,    76,   409,     0,     0,
       0,     0,     0,   379,    78,    79,   380,   381,   382,   383,
     244,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,   362,     0,    46,    47,   363,
       0,   364,   320,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,   365,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   366,   367,     0,   368,     0,
     369,    63,    64,    65,    66,    67,    68,    69,    70,    71,
     370,   371,   359,     0,   372,   373,   374,     0,   375,   376,
       0,     0,     0,     0,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   377,     0,     0,    76,
     439,     0,     0,     0,     0,     0,   379,   440,    79,   380,
     381,   382,   383,   244,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,   362,     0,
      46,    47,   363,     0,   364,   320,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,   365,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   366,   367,
       0,   368,     0,   369,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   370,   371,   359,     0,   372,   373,   374,
       0,   375,   376,     0,     0,     0,     0,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   377,
       0,     0,    76,  1195,     0,     0,     0,     0,     0,   379,
    1196,    79,   380,   381,   382,   383,   244,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   362,     0,    46,    47,   363,     0,   364,   320,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,   365,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   366,   367,     0,   368,     0,   369,    63,    64,    65,
      66,    67,    68,    69,    70,    71,   370,   371,   359,     0,
     372,   373,   374,     0,   375,   376,     0,     0,     0,     0,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   377,     0,     0,    76,   378,     0,     0,     0,
       0,     0,   379,    78,    79,   380,   381,   382,   383,   244,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,   362,     0,    46,    47,   363,     0,
     364,   320,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,   365,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   366,   367,     0,   368,     0,   369,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   370,
     371,   359,     0,   372,   373,   374,     0,   375,   376,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   377,     0,     0,    76,   439,
       0,     0,     0,     0,     0,   379,    78,    79,   380,   381,
     382,   383,  1886,     0,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
       0,    -2,     0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
       0,    -2,    -2,    -2,     0,    -2,     0,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,     0,     0,     0,    -2,
       0,     0,    -2,     0,     0,     0,     0,    -2,    -2,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    -2,     0,     0,     0,    -2,    -2,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    -2,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      -2,     0,     0,     0,    -2,    -2,     0,     0,     0,     0,
       0,     0,    -2,    -2,  1914,     0,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,     0,    -2,     0,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,     0,    -2,    -2,    -2,     0,    -2,     0,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,     0,
       0,    -2,     0,  1423,    -2,     0,     0,     0,     0,    -2,
      -2,    14,    15,    16,    17,    18,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    -2,     0,     0,     0,    -2,    -2,
       0,     0,     0,     0,     0,     0,     0,   362,     0,     0,
       0,   363,     0,   364,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    -2,     0,     0,     0,     0,     0,    58,
     365,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    -2,     0,     0,     0,    -2,    -2,     0,     0,
       0,     0,     0,     0,    -2,    -2,     0,   366,   367,     0,
     368,     0,   369,    63,    64,    65,    66,    67,    68,    69,
      70,    71,   370,   371,   359,     0,   372,   373,   374,     0,
     375,   376,     0,     0,     0,     0,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   377,     0,
       0,    76,   378,     0,     0,     0,     0,     0,   379,  1424,
      79,   380,   381,   382,   383,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,    47,     0,     0,     0,    48,    49,
      50,    51,    52,    53,    54,    55,    56,     0,     0,     0,
      57,     0,     0,    58,    59,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    60,
       0,     0,     0,    61,    62,     0,     0,    63,    64,    65,
      66,    67,    68,    69,    70,    71,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,    75,     0,    76,    77,     0,     0,     0,
       0,     0,     0,    78,    79,   243,   244,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,  -453,
    -453,     0,  -453,    46,    47,     0,  -453,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    14,    15,    16,
      17,    18,     0,    58,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,    62,    46,    47,    63,    64,    65,
      66,    67,    68,    69,    70,    71,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,    75,     0,    76,   245,     0,     0,     0,
    -767,     0,     0,    78,    79,     4,   244,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,    47,     0,     0,     0,    48,    49,
      50,    51,    52,    53,    54,    55,    56,     0,     0,     0,
      57,     0,     0,    58,     0,     0,     0,     0,  -385,  -385,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    61,     0,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -385,     0,     0,     0,    76,    77,     0,     0,     0,
       0,     0,     0,    78,    79,     4,   244,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,    47,     0,     0,     0,    48,    49,
      50,    51,    52,    53,    54,    55,    56,     0,     0,     0,
      57,     0,     0,    58,     0,     0,     0,     0,  -386,  -386,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    61,     0,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -386,     0,     0,     0,    76,    77,     0,     0,     0,
       0,     0,     0,    78,    79,   243,   244,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,  -453,
    -453,     0,  -453,    46,    47,     0,  -453,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    62,     0,     0,    63,    64,    65,
      66,    67,    68,    69,    70,    71,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,    75,     0,    76,   245,     0,  1334,     0,
       0,     0,     0,    78,    79,  1335,     0,     0,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,    47,     0,     0,
       0,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,  1336,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    61,     0,     0,     0,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1519,     0,     0,     0,    76,   929,
       0,  1334,     0,     0,     0,     0,    78,    79,  1335,     0,
       0,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
      47,     0,     0,     0,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
    1336,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    61,
       0,     0,     0,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1520,     0,     0,
       0,    76,   929,     0,  1334,     0,     0,     0,     0,    78,
      79,  1335,     0,     0,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,    47,     0,     0,     0,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,  1336,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    61,     0,     0,     0,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1521,     0,     0,     0,    76,   929,     0,     0,     0,     0,
       0,     0,    78,    79,   243,   244,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,  -453,  -453,
       0,  -453,    46,    47,     0,  -453,     0,     0,     0,     0,
       0,     0,     0,     0,    14,    15,    16,    17,    18,     0,
       0,    20,    58,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,  -453,  -453,
       0,  -453,    46,    47,     0,  -453,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    76,   245,     0,     0,     0,     0,
       0,     0,    78,    79,   244,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     0,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,  -453,  -453,     0,
    -453,    46,    47,     0,  -453,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    62,     0,     0,    63,    64,    65,    66,    67,
      68,    69,    70,    71,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,    75,     0,    76,   245,     0,     0,     0,  -771,     0,
       0,    78,    79,   244,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,     0,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,  -453,  -453,     0,  -453,
      46,    47,     0,  -453,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    62,     0,     0,    63,    64,    65,    66,    67,    68,
      69,    70,    71,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
      75,     0,    76,   245,     0,     0,     0,     0,     0,     0,
      78,    79,   244,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
      47,     0,     0,     0,   320,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    73,     0,
    1054,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -639,    76,   322,     0,     0,     0,     0,     0,     0,    78,
      79,   244,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,    47,
       0,     0,     0,   320,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   321,
      76,   322,     0,     0,     0,     0,     0,     0,    78,    79,
     244,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,    47,     0,
       0,     0,   320,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    73,     0,  1785,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    76,
     322,     0,     0,     0,     0,     0,     0,    78,    79,   244,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,    47,     0,     0,
       0,   320,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    73,     0,  1787,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    76,   322,
       0,     0,     0,     0,     0,     0,    78,    79,   244,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,    47,     0,     0,     0,
     320,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    76,   302,     0,
       0,     0,     0,     0,     0,    78,    79,   244,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,    47,     0,     0,     0,   320,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    76,   322,     0,     0,
       0,     0,     0,     0,    78,    79,   244,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,  -453,
    -453,     0,  -453,    46,    47,     0,  -453,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    63,    64,     0,
       0,     0,     0,  1358,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   362,     0,     0,     0,   363,     0,
     364,     0,     0,     0,     0,    76,   245,     0,     0,     0,
       0,     0,     0,    78,    79,  1119,     0,   365,     0,     0,
    1121,  1809,  1810,  1122,  1123,  1124,  1125,  1126,  1127,  1128,
    1129,  1130,  1131,  1132,  1133,  -332,  1134,  1135,  1136,  1137,
    1138,     0,  1139,     0,   366,   367,     0,   464,     0,   369,
    1140,  1141,    65,    66,    67,    68,    69,    70,    71,   370,
     371,   359,  1142,   372,   373,   374,     0,   375,   376,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,  1358,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   377,     0,     0,    76,   378,
       0,     0,     0,   280,     0,   379,    78,    79,   380,   381,
     382,   383,   362,     0,     0,     0,   363,     0,   364,     0,
    -179,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1119,     0,   365,     0,     0,  1121,     0,
       0,  1122,  1123,  1124,  1125,  1126,  1127,  1128,  1129,  1130,
    1131,  1132,  1133,  -332,  1134,  1135,  1136,  1137,  1138,     0,
    1139,     0,   366,   367,     0,   464,     0,   369,  1140,  1141,
      65,    66,    67,    68,    69,    70,    71,   370,   371,   359,
    1142,   372,   373,   374,     0,   375,   376,     0,     0,     0,
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   377,     0,     0,    76,   378,     0,     0,
       0,   280,     0,   379,    78,    79,   380,   381,   382,   383,
       0,     0,     0,     0,     0,     0,     0,     0,  -179,    14,
      15,    16,    17,    18,    19,   665,    20,   666,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,   362,     0,    46,    47,   363,
       0,   364,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,   365,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     667,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   366,   367,     0,   368,     0,
     369,    63,    64,    65,    66,    67,    68,    69,    70,    71,
     370,   371,   359,     0,   372,   373,   374,     0,   375,   376,
       0,     0,     0,     0,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   377,     0,     0,    76,
     668,     0,     0,     0,   280,     0,   379,    78,    79,   669,
     670,   382,   383,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,   362,
       0,    46,    47,   363,     0,   364,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,   365,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   366,
     367,     0,   368,     0,   369,    63,    64,    65,    66,    67,
      68,    69,    70,    71,   370,   371,   359,     0,   372,   373,
     374,     0,   375,   376,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     377,     0,   408,    76,   409,     0,     0,     0,     0,     0,
     379,    78,    79,   380,   381,   382,   383,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   362,     0,    46,    47,   363,     0,   364,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,   365,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   366,   367,     0,   368,     0,   369,    63,
      64,    65,    66,    67,    68,    69,    70,    71,   370,   371,
     359,     0,   372,   373,   374,     0,   375,   376,     0,     0,
       0,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   377,     0,     0,    76,   668,     0,
       0,     0,   280,     0,   379,    78,    79,   380,   381,   382,
     383,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,   362,     0,    46,
      47,   363,     0,   364,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
     365,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   366,   367,     0,
     368,     0,   369,    63,    64,    65,    66,    67,    68,    69,
      70,    71,   370,   371,   359,     0,   372,   373,   374,     0,
     375,   376,     0,     0,     0,     0,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   377,     0,
       0,    76,   409,     0,     0,     0,     0,     0,   379,    78,
      79,   380,   381,   382,   383,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   362,     0,    46,    47,   363,     0,   364,   320,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,   365,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   366,   367,     0,   368,     0,   369,    63,    64,    65,
      66,    67,    68,    69,    70,    71,   370,   371,   359,     0,
     372,   373,   374,     0,   375,   376,     0,     0,     0,     0,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   377,     0,     0,    76,   439,     0,     0,     0,
       0,     0,   379,    78,    79,   380,   381,   382,   383,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,   362,     0,    46,    47,   363,
       0,   364,   320,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,   365,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   366,   367,     0,   368,     0,
     369,    63,    64,    65,    66,    67,    68,    69,    70,    71,
     370,   371,   359,     0,   372,   373,   374,     0,   375,   376,
       0,     0,     0,     0,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   377,     0,     0,    76,
     378,     0,     0,     0,     0,     0,   379,    78,    79,   380,
     381,   382,   383,   558,   244,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,    47,     0,     0,     0,    48,    49,    50,    51,
      52,    53,    54,    55,     0,    14,    15,    16,    17,    18,
      19,    58,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,    47,    63,    64,     0,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    76,    62,     0,     0,    63,    64,    65,
      66,    67,    68,    69,    70,    71,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,    75,     0,    76,    77,     0,     0,     0,
    -769,     0,     0,    78,    79,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,    47,     0,     0,     0,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    62,     0,     0,    63,    64,    65,
      66,    67,    68,    69,    70,    71,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,    75,     0,    76,    77,     0,     0,     0,
       0,     0,     0,    78,    79,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,    47,     0,     0,     0,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    62,     0,     0,    63,    64,    65,
      66,    67,    68,    69,    70,    71,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    76,    77,     0,     0,     0,
       0,     0,     0,    78,    79,   244,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,    47,     0,     0,     0,   320,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   853,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -652,    76,   244,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,    47,     0,     0,     0,   320,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1703,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    14,    15,    16,    17,
      18,     0,     0,    20,    76,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
    -453,  -453,     0,  -453,    46,    47,     0,  -453,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    62,     0,     0,    63,    64,
      65,    66,    67,    68,    69,    70,    71,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,    75,     0,    76,   302,     0,     0,
       0,     0,     0,     0,    78,    79,   244,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,    47,     0,     0,     0,   320,    49,
      50,    51,    52,    53,    54,    55,     0,    14,    15,    16,
      17,    18,    19,    58,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,    47,    63,    64,     0,
      48,    49,    50,    51,    52,    53,    54,    55,     0,    14,
      15,    16,    17,    18,    19,    58,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,    76,     0,    46,    47,    63,
      64,     0,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,  1439,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   935,    76,   929,     0,
       0,    63,    64,     0,     0,    78,    79,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    76,
     929,     0,     0,     0,     0,     0,     0,    78,    79,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,    47,     0,
       0,     0,    48,    49,    50,    51,    52,    53,    54,    55,
       0,    14,    15,    16,    17,    18,    19,    58,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
      47,    63,    64,     0,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    76,
     287,     0,     0,    63,    64,     0,     0,    78,    79,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    76,    77,     0,     0,     0,     0,     0,     0,    78,
      79,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
      47,     0,     0,     0,    48,    49,    50,    51,    52,    53,
      54,    55,     0,    14,    15,    16,    17,    18,    19,    58,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,    47,    63,    64,     0,   320,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    76,   435,     0,     0,    63,    64,     0,     0,    78,
      79,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    76,   322,     0,     0,     0,     0,     0,
       0,    78,    79,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,    47,     0,     0,     0,   320,    49,    50,    51,
      52,    53,    54,    55,     0,    14,    15,    16,    17,    18,
      19,    58,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,    47,    63,    64,     0,   320,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    76,   287,     0,     0,    63,    64,     0,
       0,    78,    79,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    76,   435,     0,     0,     0,
       0,     0,     0,    78,    79,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,    47,     0,     0,     0,   320,    49,
      50,    51,    52,    53,    54,    55,     0,    14,    15,    16,
      17,    18,    19,    58,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,    47,    63,    64,     0,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    76,   302,     0,     0,    63,
      64,     0,     0,    78,    79,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    76,   929,     0,
       0,     0,     0,     0,     0,    78,    79,   244,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,     0,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
    -453,  -453,     0,  -453,    46,    47,     0,  -453,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    14,    15,
      16,    17,    18,    19,    58,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,    47,    63,    64,
       0,   320,    49,    50,    51,    52,    53,    54,    55,     0,
      14,    15,    16,    17,    18,    19,    58,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,    76,     0,    46,    47,
      63,    64,     0,   320,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    76,   929,
       0,     0,    63,    64,     0,     0,    78,    79,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      76,     0,     0,    14,    15,    16,    17,    18,    78,    79,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,  -453,  -453,     0,
    -453,    46,    47,     0,  -453,     0,     0,     0,     0,     0,
       0,     0,     0,    14,    15,    16,    17,    18,     0,     0,
      20,    58,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,  -453,  -453,     0,
    -453,    46,    47,     0,  -453,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    76,   302,    63,    64,     0,     0,     0,
       0,    78,    79,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    76,     0,     0,     0,     0,     0,     0,
       0,    78,    79,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,    47,     0,     0,     0,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    62,     0,     0,    63,    64,    65,    66,    67,
      68,    69,    70,    71,     0,     0,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,    76,    46,    47,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,     0,    14,    15,
      16,    17,    18,    19,    58,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,    47,    63,    64,
       0,   320,    49,    50,    51,    52,    53,    54,    55,   362,
       0,     0,     0,   363,     0,   364,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   365,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    76,     0,     0,     0,
      63,    64,     0,     0,     0,     0,     0,     0,     0,   366,
     367,     0,   368,     0,   369,  1824,    64,    65,    66,    67,
      68,    69,    70,    71,   370,   371,   359,     0,   372,   373,
     374,     0,   375,   376,     0,   362,     0,     0,     0,   363,
      73,   364,     0,     0,     0,     0,     0,     0,    76,     0,
       0,     0,     0,     0,  1564,  1565,  1566,     0,   365,     0,
     377,  1825,     0,    76,   378,     0,     0,     0,     0,     0,
     379,    78,    79,   380,   381,   382,   383,     0,     0,     0,
       0,     0,     0,     0,     0,   366,   367,     0,   464,     0,
     369,    63,    64,    65,    66,    67,    68,    69,    70,    71,
     370,   371,   359,     0,   372,   373,   374,   362,   375,   376,
       0,   363,     0,   364,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     365,     0,     0,     0,     0,     0,   377,    75,     0,   465,
     466,     0,     0,     0,   467,     0,   379,    78,    79,   380,
     381,   382,   383,     0,     0,     0,     0,   366,   367,     0,
     368,     0,   369,    63,    64,    65,    66,    67,    68,    69,
      70,    71,   370,   371,   359,     0,   372,   373,   374,   362,
     375,   376,     0,   363,     0,   364,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   365,     0,     0,     0,     0,     0,   377,  1241,
       0,    76,   378,     0,     0,     0,  1242,     0,   379,    78,
      79,   380,   381,   382,   383,     0,     0,     0,     0,   366,
     367,     0,   368,     0,   369,    63,    64,    65,    66,    67,
      68,    69,    70,    71,   370,   371,   359,     0,   372,   373,
     374,   362,   375,   376,     0,   363,     0,   364,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   365,     0,     0,     0,     0,     0,
     377,   962,  1546,    76,   378,     0,     0,     0,     0,     0,
     379,    78,    79,   380,   381,   382,   383,     0,     0,     0,
       0,   366,   367,     0,   368,     0,   369,    63,    64,    65,
      66,    67,    68,    69,    70,    71,   370,   371,   359,     0,
     372,   373,   374,   362,   375,   376,     0,   363,     0,   364,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   365,     0,     0,     0,
       0,     0,   377,     0,     0,    76,   378,     0,     0,     0,
     467,     0,   379,    78,    79,   380,   381,   382,   383,     0,
       0,     0,     0,   366,   367,     0,   368,     0,   369,    63,
      64,    65,    66,    67,    68,    69,    70,    71,   370,   371,
     359,     0,   372,   373,   374,   362,   375,   376,     0,   363,
       0,   364,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   365,     0,
       0,     0,     0,     0,   377,   799,     0,    76,   378,     0,
       0,     0,     0,     0,   379,    78,    79,   380,   381,   382,
     383,     0,     0,     0,     0,   366,   367,     0,   368,     0,
     369,    63,    64,    65,    66,    67,    68,    69,    70,    71,
     370,   371,   359,     0,   372,   373,   374,   362,   375,   376,
       0,   363,     0,   364,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     365,     0,     0,     0,     0,     0,   377,     0,     0,    76,
     378,     0,     0,     0,   280,     0,   379,    78,    79,   380,
     381,   382,   383,     0,     0,     0,     0,   366,   367,     0,
     368,     0,   369,    63,    64,    65,    66,    67,    68,    69,
      70,    71,   370,   371,   359,     0,   372,   373,   374,   362,
     375,   376,     0,   363,     0,   364,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   365,     0,     0,     0,     0,     0,   377,   962,
       0,    76,   378,     0,     0,     0,     0,     0,   379,    78,
      79,   380,   381,   382,   383,     0,     0,     0,     0,   366,
     367,     0,   368,     0,   369,    63,    64,    65,    66,    67,
      68,    69,    70,    71,   370,   371,   359,     0,   372,   373,
     374,   362,   375,   376,     0,   363,     0,   364,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   365,     0,     0,     0,     0,     0,
     377,     0,     0,    76,   378,     0,     0,   993,     0,     0,
     379,    78,    79,   380,   381,   382,   383,     0,     0,     0,
       0,   366,   367,     0,   368,     0,   369,    63,    64,    65,
      66,    67,    68,    69,    70,    71,   370,   371,   359,     0,
     372,   373,   374,   362,   375,   376,     0,   363,     0,   364,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   365,     0,     0,     0,
       0,     0,   377,     0,     0,    76,   378,     0,     0,     0,
    1216,     0,   379,    78,    79,   380,   381,   382,   383,     0,
       0,     0,     0,   366,   367,     0,   368,     0,   369,    63,
      64,    65,    66,    67,    68,    69,    70,    71,   370,   371,
     359,     0,   372,   373,   374,   362,   375,   376,     0,   363,
       0,   364,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   365,     0,
       0,     0,     0,     0,   377,  1308,     0,    76,   378,     0,
       0,     0,     0,     0,   379,    78,    79,   380,   381,   382,
     383,     0,     0,     0,     0,   366,   367,     0,   368,     0,
     369,    63,    64,    65,    66,    67,    68,    69,    70,    71,
     370,   371,   359,     0,   372,   373,   374,   362,   375,   376,
       0,   363,     0,   364,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     365,     0,     0,     0,     0,     0,   377,     0,     0,    76,
     378,     0,     0,     0,  1368,     0,   379,    78,    79,   380,
     381,   382,   383,     0,     0,     0,     0,   366,   367,     0,
     368,     0,   369,    63,    64,    65,    66,    67,    68,    69,
      70,    71,   370,   371,   359,     0,   372,   373,   374,   362,
     375,   376,     0,   363,     0,   364,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   365,     0,     0,     0,     0,     0,   377,     0,
    1815,    76,   378,     0,     0,     0,     0,     0,   379,    78,
      79,   380,   381,   382,   383,     0,     0,     0,     0,   366,
     367,     0,   368,     0,   369,    63,    64,    65,    66,    67,
      68,    69,    70,    71,   370,   371,   359,     0,   372,   373,
     374,   362,   375,   376,     0,   363,     0,   364,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   365,     0,     0,     0,     0,     0,
     377,  1820,     0,    76,   378,     0,     0,     0,     0,     0,
     379,    78,    79,   380,   381,   382,   383,     0,     0,     0,
       0,   366,   367,     0,   368,     0,   369,    63,    64,    65,
      66,    67,    68,    69,    70,    71,   370,   371,   359,     0,
     372,   373,   374,   362,   375,   376,     0,   363,     0,   364,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   365,     0,     0,     0,
       0,     0,   377,  1829,     0,    76,   378,     0,     0,     0,
       0,     0,   379,    78,    79,   380,   381,   382,   383,     0,
       0,     0,     0,   366,   367,     0,   368,     0,   369,    63,
      64,    65,    66,    67,    68,    69,    70,    71,   370,   371,
     359,     0,   372,   373,   374,   362,   375,   376,     0,   363,
       0,   364,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   365,     0,
       0,     0,     0,     0,   377,  1908,     0,    76,   378,     0,
       0,     0,     0,     0,   379,    78,    79,   380,   381,   382,
     383,     0,     0,     0,     0,   366,   367,     0,   368,     0,
     369,    63,    64,    65,    66,    67,    68,    69,    70,    71,
     370,   371,   359,     0,   372,   373,   374,   362,   375,   376,
       0,   363,     0,   364,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     365,     0,     0,     0,     0,     0,   377,  1910,     0,    76,
     378,     0,     0,     0,     0,     0,   379,    78,    79,   380,
     381,   382,   383,     0,     0,     0,     0,   366,   367,     0,
     368,     0,   369,    63,    64,    65,    66,    67,    68,    69,
      70,    71,   370,   371,   359,     0,   372,   373,   374,   362,
     375,   376,     0,   363,     0,   364,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   365,     0,     0,     0,     0,     0,   377,  1961,
       0,    76,   378,     0,     0,     0,     0,     0,   379,    78,
      79,   380,   381,   382,   383,     0,     0,     0,     0,   366,
     367,     0,   368,     0,   369,    63,    64,    65,    66,    67,
      68,    69,    70,    71,   370,   371,   359,     0,   372,   373,
     374,   362,   375,   376,     0,   363,     0,   364,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   365,     0,     0,     0,     0,     0,
     377,  1963,     0,    76,   378,     0,     0,     0,     0,     0,
     379,    78,    79,   380,   381,   382,   383,     0,     0,     0,
       0,   366,   367,     0,   368,     0,   369,    63,    64,    65,
      66,    67,    68,    69,    70,    71,   370,   371,   359,     0,
     372,   373,   374,   362,   375,   376,     0,   363,     0,   364,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   365,     0,     0,     0,
       0,     0,   377,  1965,     0,    76,   378,     0,     0,     0,
       0,     0,   379,    78,    79,   380,   381,   382,   383,     0,
       0,     0,     0,   366,   367,     0,   368,     0,   369,    63,
      64,    65,    66,    67,    68,    69,    70,    71,   370,   371,
     359,     0,   372,   373,   374,   362,   375,   376,     0,   363,
       0,   364,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   365,     0,
       0,     0,     0,     0,   377,  1968,     0,    76,   378,     0,
       0,     0,     0,     0,   379,    78,    79,   380,   381,   382,
     383,     0,     0,     0,     0,   366,   367,     0,   368,     0,
     369,    63,    64,    65,    66,    67,    68,    69,    70,    71,
     370,   371,   359,     0,   372,   373,   374,   362,   375,   376,
       0,   363,     0,   364,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     365,     0,     0,     0,     0,     0,   377,  1970,     0,    76,
     378,     0,     0,     0,     0,     0,   379,    78,    79,   380,
     381,   382,   383,     0,     0,     0,     0,   366,   367,     0,
     368,     0,   369,    63,    64,    65,    66,    67,    68,    69,
      70,    71,   370,   371,   359,     0,   372,   373,   374,   362,
     375,   376,     0,   363,     0,   364,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   365,     0,     0,     0,     0,     0,   377,  2012,
       0,    76,   378,     0,     0,     0,     0,     0,   379,    78,
      79,   380,   381,   382,   383,     0,     0,     0,     0,   366,
     367,     0,   368,     0,   369,    63,    64,    65,    66,    67,
      68,    69,    70,    71,   370,   371,   359,     0,   372,   373,
     374,   362,   375,   376,     0,   363,     0,   364,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   365,     0,     0,     0,     0,     0,
     377,  2014,     0,    76,   378,     0,     0,     0,     0,     0,
     379,    78,    79,   380,   381,   382,   383,     0,     0,     0,
       0,   366,   367,     0,   368,     0,   369,    63,    64,    65,
      66,    67,    68,    69,    70,    71,   370,   371,   359,     0,
     372,   373,   374,   362,   375,   376,     0,   363,     0,   364,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   365,     0,     0,     0,
       0,     0,   377,  2016,     0,    76,   378,     0,     0,     0,
       0,     0,   379,    78,    79,   380,   381,   382,   383,     0,
       0,     0,     0,   366,   367,     0,   368,     0,   369,    63,
      64,    65,    66,    67,    68,    69,    70,    71,   370,   371,
     359,     0,   372,   373,   374,   362,   375,   376,     0,   363,
       0,   364,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   365,     0,
       0,     0,     0,     0,   377,  2039,     0,    76,   378,     0,
       0,     0,     0,     0,   379,    78,    79,   380,   381,   382,
     383,     0,     0,     0,     0,   366,   367,     0,   368,     0,
     369,    63,    64,    65,    66,    67,    68,    69,    70,    71,
     370,   371,   359,     0,   372,   373,   374,   362,   375,   376,
       0,   363,     0,   364,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     365,     0,     0,     0,     0,     0,   377,  2041,     0,    76,
     378,     0,     0,     0,     0,     0,   379,    78,    79,   380,
     381,   382,   383,     0,     0,     0,     0,   366,   367,     0,
     368,     0,   369,    63,    64,    65,    66,    67,    68,    69,
      70,    71,   370,   371,   359,     0,   372,   373,   374,   362,
     375,   376,     0,   363,     0,   364,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   365,     0,     0,     0,     0,     0,   377,  2043,
       0,    76,   378,     0,     0,     0,     0,     0,   379,    78,
      79,   380,   381,   382,   383,     0,     0,     0,     0,   366,
     367,     0,   368,     0,   369,    63,    64,    65,    66,    67,
      68,    69,    70,    71,   370,   371,   359,     0,   372,   373,
     374,   362,   375,   376,     0,   363,     0,   364,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   365,     0,     0,     0,     0,     0,
     377,     0,     0,    76,   378,     0,     0,     0,     0,     0,
     379,    78,    79,   380,   381,   382,   383,     0,     0,     0,
       0,   366,   367,     0,   368,     0,   369,    63,    64,    65,
      66,    67,    68,    69,    70,    71,   370,   371,   359,     0,
     372,   373,   374,   362,   375,   376,     0,   363,     0,   364,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   365,     0,     0,     0,
       0,     0,   651,     0,     0,    76,   378,     0,     0,     0,
       0,     0,   379,    78,    79,   380,   381,   382,   383,     0,
       0,     0,     0,   366,   367,     0,   368,     0,   369,    63,
      64,    65,    66,    67,    68,    69,    70,    71,   370,   371,
     359,     0,   372,   373,   374,   362,   375,   376,     0,   363,
       0,   364,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   365,     0,
       0,     0,     0,     0,   656,     0,     0,    76,   378,     0,
       0,     0,     0,     0,   379,    78,    79,   380,   381,   382,
     383,     0,     0,     0,     0,   366,   367,     0,   368,     0,
     369,    63,    64,    65,    66,    67,    68,    69,    70,    71,
     370,   371,   359,     0,   372,   373,   374,   362,   375,   376,
       0,   363,     0,   364,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     365,     0,     0,     0,     0,     0,   662,     0,     0,    76,
     378,     0,     0,     0,     0,     0,   379,    78,    79,   380,
     381,   382,   383,     0,     0,     0,     0,   366,   367,     0,
     368,     0,   369,    63,    64,    65,    66,    67,    68,    69,
      70,    71,   370,   371,   359,     0,   372,   373,   374,   362,
     375,   376,     0,   363,     0,   364,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   365,     0,     0,     0,     0,     0,   377,     0,
       0,    76,   378,     0,     0,     0,     0,     0,   379,   867,
      79,   380,   381,   382,   383,     0,     0,     0,     0,   366,
     367,     0,   368,     0,   369,    63,    64,    65,    66,    67,
      68,    69,    70,    71,   370,   371,   359,     0,   372,   373,
     374,   362,   375,   376,     0,   363,     0,   364,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   365,     0,     0,     0,     0,     0,
     377,     0,     0,    76,   378,     0,     0,     0,     0,     0,
     379,   440,    79,   380,   381,   382,   383,     0,     0,     0,
       0,   366,   367,     0,   368,     0,   369,  1903,    64,    65,
      66,    67,    68,    69,    70,    71,   370,   371,   359,     0,
     372,   373,   374,     0,   375,   376,     0,     0,     0,     0,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   377,     0,     0,    76,   378,     0,     0,     0,
       0,     0,   379,    78,    79,   380,   381,   382,   383
};

static const yytype_int16 yycheck[] =
{
       1,    74,    74,     4,   284,   163,   242,   876,   694,     1,
     257,    74,    83,     1,   467,    74,   163,   377,   179,   672,
     151,   174,    76,   323,   221,   223,    96,   768,    59,   604,
    1747,     4,   617,   774,     1,   679,   767,   968,   163,  1678,
     340,   604,   862,   765,   344,  1360,   164,   140,   220,   377,
     516,   517,  1678,   608,   208,    56,    57,   220,    59,  1678,
    1128,   767,    74,   738,   178,  1214,   997,    59,   220,  1574,
     220,    59,    74,    74,  1809,   325,   527,   148,   368,    74,
     317,     1,    83,   179,   152,   193,   537,   785,   156,   636,
      91,    83,    59,   291,   292,    96,  1292,  1293,    99,   192,
     297,   604,   103,    74,    96,   803,   237,    99,     1,   182,
     182,   103,  1570,   340,   610,   146,     1,   344,    88,   356,
     220,    74,   765,   182,   296,   220,   846,   578,   765,  1060,
     103,   150,    99,   296,    76,    77,   765,  1102,    97,    59,
     141,   441,  1107,   144,   296,   146,   296,   220,   220,     1,
     151,   221,    71,   873,   146,   174,   157,   220,   146,     1,
      71,   220,     4,   164,    76,    77,    59,  1679,   116,    71,
     182,   869,   638,   175,   464,   246,     1,   158,   150,   146,
     181,   182,    96,   103,   220,   157,  1813,   182,   158,   181,
     765,   150,    83,    88,   175,   196,   296,   447,   323,  1360,
    1361,   296,   765,   230,   196,   206,   154,    59,   220,   131,
     211,   182,   767,  1178,  1949,   116,   158,    59,  1723,   220,
     221,   275,   175,   296,   296,   220,   146,   297,   255,   221,
    1250,   150,   303,   296,    59,   154,   237,   296,   265,   229,
     926,    83,   232,   154,   575,   246,   158,   862,   150,   220,
      88,   173,   499,   146,   246,   256,     4,   831,   259,   150,
     296,   175,   765,   158,   254,   266,     0,   259,   954,   156,
    1728,  1729,   157,   769,   264,   276,   277,   773,   279,   745,
     121,  1430,  1597,  1024,   564,  1797,   782,   783,   175,     0,
     943,   266,   259,   593,   146,   296,   297,   494,   140,  1030,
    1120,  1023,   277,   304,   146,   297,   148,     0,    56,    57,
     311,   312,   153,   485,  2031,   316,   441,   617,   892,   473,
     158,   146,   485,   651,  1030,   653,   654,   445,   656,  1149,
     630,  1006,   692,   485,   662,   485,   883,   665,   666,   667,
     411,     1,  1981,    91,   437,   900,   596,   132,   349,   156,
     192,   158,   153,   354,   132,  1981,   357,   358,   325,  1112,
     154,   132,  1981,  1328,   284,   159,   593,  1825,  1826,   150,
     620,  1952,   919,   174,  1072,  2002,   573,   627,   150,     1,
    1023,   166,     4,   514,  1104,   485,  1023,   105,   106,   520,
     485,   284,   843,   141,  1023,   166,   144,  1978,   158,   571,
      71,   243,   154,   630,   246,    20,  1918,   159,   571,   157,
    2037,   465,   485,   485,   522,   175,   164,   575,   149,   571,
     421,   571,   132,  2004,   494,   156,   485,  1112,  1002,   421,
       1,  1112,   284,     4,   212,   153,  1597,    59,  1458,  1459,
    1460,   155,   284,   444,   445,  1019,  1020,   156,  1023,   485,
     575,   161,   162,   784,   785,   456,   457,   166,    83,   284,
    1023,   303,  1203,   211,   465,    71,   467,  1663,  1664,    83,
    1982,   571,   803,   485,   592,  1030,   571,    99,   150,   150,
     447,   103,    96,   154,   485,    99,   594,   150,    59,   103,
     485,   150,   617,   494,    71,   158,   156,   157,   571,   571,
      74,  1569,   494,   573,   150,  1120,  1574,   285,   256,   536,
    2022,   156,   571,   514,   485,   156,    90,   507,   266,   520,
    1023,   166,  1453,   148,   146,   166,    60,    61,   276,   277,
      88,   279,   103,   107,  1149,   571,    71,   175,   869,   529,
    1281,   784,   785,   156,   150,   535,   577,  1000,   154,   539,
     150,  1047,   144,   145,   146,   180,   304,   558,   158,   560,
     803,   156,   175,   311,   312,  1726,   150,   181,   316,   411,
     571,   166,   573,   150,   166,   146,   577,   154,  1331,  1332,
    1333,   573,   196,   175,   156,   577,   587,   887,   156,   577,
     591,   592,   156,   175,   561,   437,    71,   152,   156,   377,
     158,   349,   157,   175,   854,   719,   354,   221,   939,   357,
     577,   175,  1197,   721,     3,   150,  1260,   778,   151,   154,
     150,   246,   152,   624,   158,   158,   869,    57,   789,   147,
      60,    61,   246,    63,     3,   636,   153,   259,   792,   889,
     158,   175,   562,   156,   564,   259,  1331,  1332,  1333,   767,
    1331,  1332,  1333,   620,   762,  1723,   174,   577,    71,  1528,
     887,    71,   175,  1210,  1350,    74,   720,   144,   145,   146,
      71,   564,   156,  1869,   805,   150,   784,   785,   303,   154,
      89,   152,   778,   684,   577,   686,   157,   688,   156,   166,
     149,   692,   156,   824,   695,   803,   444,   156,   323,  1273,
    1274,   594,   166,   354,    71,   193,   357,   175,   456,   457,
     156,   825,   564,  1287,  1288,  1646,   558,  1648,    71,   720,
     150,  1187,   564,   156,   156,   577,  1887,  1371,   506,   175,
      71,   152,    71,   511,   166,   577,   157,   105,   106,   564,
     150,  1072,   175,    71,   154,  1360,  1361,  1321,  1322,   150,
     528,   150,   577,   154,  1915,  1823,   917,    71,    71,   174,
     538,   869,   158,  1216,   765,   156,   767,  1835,     4,     5,
       6,     7,     8,     9,    10,    11,    12,   156,   779,    71,
     156,   939,   172,   150,   175,   786,   411,   154,   158,  1242,
     152,   792,  1953,   911,   795,   157,   175,   150,   150,   175,
     152,   154,   154,   804,   805,   806,  1450,   421,   152,   150,
     558,   150,  1059,   154,   939,   154,   441,   105,   106,   163,
     164,   917,   150,   824,   128,   129,   154,   126,   127,  1072,
     150,   152,   993,   154,  1902,   323,   150,   150,   326,   587,
     154,   154,   158,   591,   592,   116,    13,    14,    15,    16,
      17,    22,   340,   153,   154,   845,   344,   157,   150,   860,
     861,   862,   154,  1410,  1411,   150,   150,   150,   152,   762,
     154,   170,   171,   651,   862,   150,   624,   152,   656,   154,
     494,   150,   883,   152,   662,   154,   152,   854,   636,   152,
     156,   516,   517,  1091,  1225,   862,   152,  1197,   865,   130,
     156,  1229,   875,   681,    71,   150,   152,   153,  1069,   154,
     911,  1029,  1030,  1067,   915,  1368,   152,   130,   919,   150,
     163,   164,   889,   154,   925,    13,    14,    15,    16,    17,
     161,   162,   124,   125,  1508,  1509,   684,   150,   686,   175,
     688,   154,   862,   174,   692,   155,   156,   695,   161,   162,
     161,  1201,   152,   441,   152,   577,   156,   168,   169,   960,
     156,  1292,   152,   152,  1072,   132,   156,   968,   152,   862,
    1206,   152,   720,   152,     3,  1335,   150,   130,   155,   156,
     130,   150,  1597,    71,    13,    14,    15,    16,    17,   152,
     152,   150,   617,   156,   156,  1448,   997,   150,   104,  1000,
     150,   154,   108,   150,   154,   111,   577,   113,   161,   162,
     862,   161,   162,   638,   156,    13,    14,    15,    16,    17,
     862,   152,  1023,    47,    48,   156,    50,   156,  1029,  1030,
      54,   779,   156,   875,   522,  1188,  1189,   862,   786,   161,
     162,  1112,    71,    57,   132,   156,    60,    61,  1622,    63,
    1181,   150,   830,   155,   156,   154,   804,   152,   806,  1060,
     150,   156,    97,   841,   154,   150,   844,  1225,   150,   154,
     848,   152,  1197,    71,   152,   156,  1323,   150,   156,  1653,
    1360,  1412,   150,  1630,  1658,  1659,   154,   575,   119,   152,
     121,   122,   123,   156,    13,    14,    15,    16,    17,   152,
    1225,   158,  1263,   156,   152,   593,   594,    88,   156,   155,
     156,  1726,   860,   861,   862,   706,   707,   708,   709,   150,
     745,  1122,   153,   154,  1125,  1126,  1127,   158,   159,   617,
     152,   158,  1120,   152,   156,   883,   149,   156,   158,  1300,
    1301,   152,   630,   249,  1341,     3,  1293,   152,  1149,   155,
     156,   156,    71,  1120,  1155,    13,    14,    15,    16,    17,
     155,  1149,  1163,   911,  1850,  1166,  1167,   915,   152,  1170,
     158,   919,   156,   152,  1166,  1167,   158,   156,   158,   152,
    1181,   174,  1149,   156,  1292,  1293,    13,    14,    15,    16,
      17,   152,   152,   152,  1167,   156,   156,   156,   152,  1166,
    1120,   152,   156,   116,  1300,  1301,   144,   145,   146,  1210,
     152,   130,   156,    71,   156,  1216,  1763,   150,   156,  1986,
     150,   327,   328,  1990,   330,  1226,   332,  1120,   166,  1149,
     150,   150,   167,   721,  1201,   154,   162,   175,   155,   156,
     862,  1242,   161,   162,    71,   156,   157,  1167,   160,  1250,
     172,   876,   130,   875,  1412,   153,  1149,    13,   155,   156,
    1331,  1332,  1333,   154,  1335,  1336,   155,   156,  1120,   152,
    1112,  1341,  1887,   152,   762,   699,   700,   701,  1120,   155,
     156,  1282,   155,   156,   144,   145,   146,  1412,  1519,  1520,
    1521,   862,  1109,  1110,  1111,  1120,   156,  1149,   155,   156,
    1915,   155,   156,   130,   875,   155,   166,  1149,   155,   156,
    1638,   155,   156,   155,   156,   175,   152,  1597,   109,   110,
     111,   112,   113,   150,  1149,   155,   156,   154,  1106,  1526,
     152,    87,  1663,   152,   161,   162,   152,  1534,  1953,   157,
    1341,  1119,   155,   156,  1345,   132,   102,  1348,   132,   105,
     106,   107,   108,   109,   110,   111,   112,   113,  1136,   157,
    1532,  1334,   155,   156,   156,   155,   156,  1368,   150,  1532,
     156,   157,   155,   156,  1122,   155,   156,  1125,  1126,  1127,
    1532,   150,  1532,    76,    77,   156,   157,  1388,   152,  1390,
    1251,  1252,   152,  1360,  1361,   702,   703,   152,  1390,   887,
     152,  1149,   890,   704,   705,   710,   711,  1155,   152,  1410,
    1411,  1539,  1540,  1188,  1189,  1163,    13,    14,    15,    16,
      17,    18,  1170,  1390,   152,  1999,   155,   533,   154,    69,
     150,   158,  1532,   158,   158,   158,  1544,  1532,   155,    77,
    1360,  1361,   155,    18,   174,   156,   158,  1448,   150,   152,
    1608,   939,  1453,   174,   152,   155,  1526,  1458,  1459,  1460,
     175,  1608,  1210,  1436,  1534,   175,   158,  1360,  1361,  1532,
     158,    13,    14,    15,    16,    17,   155,    18,  1226,   149,
     152,   152,  1596,  1608,    10,   152,   152,   152,   152,  1331,
    1332,  1333,  1334,  1335,  1336,   102,  1532,   152,  1120,  1696,
     107,   108,   109,   110,   111,   112,   113,   114,  1360,  1361,
      13,    14,    15,    16,    17,    18,   152,  1664,  1360,  1361,
     149,  1693,   152,   158,   158,  1526,   158,  1149,   175,    71,
    1693,  1532,    69,  1534,   152,  1360,  1361,   174,   152,   152,
    1541,  1693,  1842,  1693,  1166,  1167,   149,   154,   158,  1120,
     152,   152,  1166,  1167,  1555,  1663,  1664,   156,   152,   152,
     156,   152,  1187,   152,   152,   152,   152,  1568,   152,   152,
     155,   152,  1197,  1351,  1352,   155,   102,   155,  1149,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   130,   152,
    1563,   152,   152,  1693,  1436,  1562,  1167,  1345,  1693,   152,
    1348,   152,   152,     1,  1605,   152,     4,   152,   150,  1806,
     149,  1389,   154,   149,   152,   174,  1916,   152,   150,   161,
     162,   150,  1753,   156,    14,   150,  1696,   150,  1742,  1630,
    1597,    13,    14,    15,    16,    17,    18,   150,  1611,   150,
    1388,   157,    73,  1563,   102,  1646,   156,  1648,  1809,   107,
     108,   109,   110,   111,   112,   113,   175,  1693,    90,   157,
     175,    59,  1410,  1411,   155,   155,   175,   175,   149,  1919,
     158,   158,   149,   102,   158,   175,    74,  1597,   107,   108,
     109,   110,   111,   112,   113,    83,    13,    14,    15,    16,
      17,    18,  1693,   151,   156,  1696,   154,   152,    96,   155,
     152,    99,   156,   156,  1597,   103,  1707,   152,   156,  1197,
    1711,   155,  1334,  1809,   152,   152,   149,    79,   175,   149,
     156,   149,  1869,   150,  1725,   175,   150,   150,   175,   175,
     150,   152,  1846,   175,  1735,   175,  1806,  1225,  1360,  1361,
     175,   175,   140,   149,   149,  1597,  1713,   156,   146,   175,
     148,  1752,  1753,   151,   152,  1597,   149,   158,   155,  1726,
     155,  1869,  1763,  1334,   155,   163,   152,   155,  1390,  1611,
     149,  1842,  1597,   157,   152,   119,  1390,   633,   149,   152,
     157,   155,   180,   181,   182,  1982,  1947,  1918,  1949,  1360,
    1361,   152,   152,  1541,   192,   193,   152,   155,   196,   152,
     149,   152,   157,   175,   156,  1806,  1726,  1555,   150,  1981,
    1588,   152,  1813,   156,  1436,   150,  1817,   150,  1981,   108,
    1568,  1822,   220,   221,   155,  2022,   155,  1988,   149,  1981,
     155,  1981,   158,  1726,   149,   149,   152,   155,    74,   237,
     152,   152,   152,   152,   152,  1916,  1847,    74,   246,   149,
     152,  1947,   175,  1949,   150,   175,   175,  1605,   152,  2020,
      89,   259,   149,   149,   155,  1436,   149,   155,   149,   154,
     152,    74,  1986,   152,  1726,   152,  1990,  1991,   152,   152,
     152,  1981,  1630,   153,  1726,   175,  1981,    74,   166,  1890,
     166,   289,  1988,  1894,   157,   175,   152,   295,   296,   297,
     175,  1726,   149,  1528,   152,   303,  1907,  2068,  1981,  1981,
    2024,   156,  1982,   152,   152,   149,   151,  1918,  1981,  1920,
    1887,   149,  1981,   166,  1412,   323,   324,   325,   102,   166,
    1931,   157,  1933,  1934,   790,  2049,   150,   156,    74,  2053,
    1562,  1563,   340,   150,   149,  1981,   344,   151,  1915,   166,
     166,   175,  2022,   155,   175,  1956,   108,   108,   151,  1707,
     152,  2075,    83,  1711,   152,   149,   157,  1887,   149,   152,
     150,    74,  2068,   152,   175,  1597,   152,  1725,   175,   377,
    1981,  1982,  1638,   379,  1350,   175,  1953,  1735,   671,  1611,
    1982,  1992,  1563,  1257,  1887,  1915,   712,   715,  1138,   713,
     410,  2002,  2037,  1149,  1752,   714,  1949,  1597,   716,  1734,
    1978,  1726,  2032,   411,  1851,  1763,   414,  2031,  1916,  2019,
    1589,  2022,  1915,   421,  1589,  1991,  1597,   148,  2053,  1915,
    2022,  1170,    49,  1953,  2035,  1887,  2037,  1526,   251,   437,
    1611,  1806,   163,   441,  1877,  1887,  1336,   445,  1540,   447,
    1163,   792,   474,   879,  1436,  2056,   587,  1611,   925,   180,
    1953,  2062,  1887,  1915,   920,  1813,     0,    -1,    10,  1817,
      -1,  2072,   193,  1915,  1822,  2076,   737,   737,   737,    -1,
      -1,    -1,    -1,    99,    -1,  2086,    -1,   485,    -1,    -1,
    1915,  1713,    -1,    -1,   110,    -1,   494,    -1,    -1,  1847,
      -1,  1953,    -1,    -1,  1726,    -1,    -1,    -1,   365,    -1,
      -1,  1953,    -1,    -1,    -1,    -1,   514,    -1,   516,   517,
      -1,    -1,   520,    -1,   522,   246,    -1,    -1,  1953,    -1,
      -1,    -1,    -1,   390,   391,    -1,   152,    -1,    -1,    -1,
      -1,    -1,  1890,    -1,    -1,    -1,  1894,    -1,    -1,    18,
      -1,    -1,    -1,    -1,   411,  1726,    -1,    -1,    -1,  1907,
     102,    -1,   560,   105,   106,   107,   108,   109,   110,   111,
     112,   113,  1920,   571,    -1,   573,    -1,   575,    -1,   577,
     196,    -1,   303,  1931,   441,  1933,  1934,    56,    57,    58,
      59,    60,    61,    62,    63,   593,   594,    -1,   596,    -1,
      -1,    -1,   323,    -1,    -1,    -1,   604,    -1,  1956,    -1,
     608,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   617,
       4,     5,     6,     7,     8,     9,    10,    11,    12,   627,
      -1,    -1,   630,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     638,    -1,    -1,   259,  1992,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   651,  2002,   653,   654,    -1,   656,    -1,
      -1,    -1,    -1,    -1,   662,  1887,    -1,   665,   666,   667,
    2048,    -1,    -1,   289,    -1,    -1,    -1,    -1,    -1,   295,
      64,    -1,    -1,    -1,    -1,    -1,  2064,  2035,    -1,  2037,
     411,    -1,    -1,  1915,    -1,     1,    -1,    -1,     4,    -1,
      -1,    -1,    -1,    -1,    74,    -1,    -1,   206,  2056,   325,
      -1,    -1,   553,    -1,  2062,    -1,  1887,    -1,    -1,    71,
     441,    -1,    -1,   721,  2072,    -1,    96,    -1,  2076,    18,
      -1,  1953,    -1,    -1,  1190,    -1,    -1,    -1,  2086,   737,
     738,    -1,    -1,    -1,  1915,    -1,    -1,   745,    -1,    -1,
     102,    -1,    -1,    59,  1842,   107,   108,   109,   110,   111,
     112,   113,  1218,    -1,   762,    -1,    -1,   765,    -1,   767,
      -1,    60,    61,    62,    63,    -1,    -1,    83,   130,    -1,
      -1,   151,  1953,    -1,    -1,    -1,   784,   785,    -1,    -1,
      -1,    -1,    -1,    99,    -1,   516,   517,   103,   150,   151,
      -1,   522,    -1,    -1,     3,   803,    -1,   805,    -1,   161,
     162,  1267,    -1,   102,    -1,    -1,    -1,    -1,   107,   108,
     109,   110,   111,   112,   113,   114,   824,    -1,  1916,    -1,
      -1,   447,    -1,    -1,   140,    -1,    -1,    -1,    -1,    -1,
     146,   698,   148,    -1,    -1,    -1,   152,    -1,    -1,    -1,
     220,   221,    -1,    -1,   575,    -1,   162,   163,   164,    -1,
      -1,    -1,    -1,    -1,   862,   154,    -1,   237,    -1,    -1,
      -1,   869,    -1,   594,   180,    -1,    -1,   875,   876,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   192,   193,    -1,   887,
     196,   889,    -1,    -1,    -1,    -1,   617,    -1,    -1,    -1,
      -1,    -1,   900,   102,    -1,   521,   105,   106,   107,   108,
     109,   110,   111,   112,   113,    -1,    -1,   638,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   541,   296,   297,    -1,    -1,
      -1,   130,    -1,    -1,    -1,    -1,    -1,   243,    -1,    -1,
     246,   939,    -1,    -1,    -1,   561,    -1,    -1,    -1,    -1,
      -1,   150,   151,   259,    -1,    -1,    -1,    -1,   157,    -1,
      -1,    -1,   161,   162,    -1,    -1,   465,    -1,   467,    -1,
     276,  1427,    -1,    -1,    -1,    -1,  1432,    -1,   284,    -1,
     596,    -1,    -1,   289,    -1,    -1,    -1,    -1,    -1,   295,
     831,    -1,    -1,  1449,    -1,    -1,    -1,   303,    -1,    -1,
     721,    -1,    -1,    -1,   620,    -1,    -1,    -1,  1006,    -1,
      -1,   627,    64,    65,    66,    67,    -1,   323,    -1,   325,
     326,    -1,    -1,    -1,   745,  1023,    -1,    -1,    -1,    -1,
      -1,    -1,  1030,    -1,   340,    -1,    -1,    -1,   344,    -1,
      -1,   762,    -1,    -1,   660,   661,    -1,    -1,    -1,    -1,
     102,   892,    -1,   105,   106,   107,   108,   109,   110,   111,
     112,   113,    -1,   784,   785,    -1,    -1,    -1,    -1,    -1,
      -1,   377,    -1,    -1,  1072,   445,    -1,    -1,   935,    -1,
      -1,   102,   803,   940,   105,   106,   107,   108,   109,   110,
     111,   112,   113,    -1,   951,    -1,    -1,    -1,    -1,   231,
      -1,    -1,   154,    -1,    -1,   411,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1112,   485,    -1,    -1,    -1,    -1,
     172,    -1,  1120,  1579,   494,    -1,    -1,    -1,    -1,    -1,
      -1,   437,    -1,    -1,    -1,   441,    -1,    -1,    -1,    -1,
      -1,   447,    -1,    -1,   514,    -1,    -1,    -1,   869,    -1,
     520,  1149,    -1,    -1,   175,   876,    -1,    -1,    -1,  1615,
    1616,  1002,    -1,    -1,    -1,   102,    -1,    -1,  1166,  1167,
     107,   108,   109,   110,   111,   112,   113,    -1,  1019,  1020,
      -1,    -1,    -1,  1181,  1640,    -1,    -1,    -1,    -1,  1187,
     560,    -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,  1197,
      -1,   571,    -1,   573,    -1,    -1,    -1,    -1,    -1,    -1,
     516,   517,    -1,   150,   151,   521,   522,   154,   939,    -1,
      -1,    -1,    -1,    -1,   161,   162,    -1,  1225,    -1,    -1,
      -1,  1229,    -1,    -1,    -1,   102,    -1,    -1,   854,    -1,
     107,   108,   109,   110,   111,   112,   113,   553,    -1,   865,
      -1,    -1,   558,    -1,    -1,   561,   562,    -1,   564,    -1,
      -1,    -1,    -1,  1120,    -1,    -1,    -1,    -1,    -1,   575,
      -1,   577,    -1,   889,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   414,   150,   151,   591,    -1,   593,   594,    -1,
     596,    -1,    -1,   792,  1292,  1293,   795,    -1,   430,    -1,
      -1,   433,    -1,    -1,    -1,    -1,    -1,    -1,  1764,    -1,
      -1,   617,    -1,    -1,   620,    -1,    -1,    -1,   624,    -1,
      -1,   627,    -1,    -1,   630,    -1,   632,    -1,    -1,    -1,
      -1,    -1,   638,  1331,  1332,  1333,  1334,  1335,  1336,    -1,
    1197,    -1,    -1,  1341,  1342,   651,    -1,   653,   654,    -1,
     656,  1072,    -1,    -1,    -1,    -1,   662,    -1,   490,   665,
     666,   667,  1360,  1361,   133,   134,   135,   136,   137,   138,
     139,   140,   141,   142,   143,  1232,  1233,  1234,   147,    -1,
      -1,    -1,  1239,  1240,    -1,    -1,    -1,    -1,    77,    -1,
      -1,    -1,  1390,    -1,    -1,   765,    -1,   767,    -1,    -1,
      -1,    -1,    -1,    -1,  1860,   174,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   102,  1412,   721,   105,   106,   107,   108,
     109,   110,   111,   112,   113,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1273,  1274,    -1,   805,    -1,    -1,  1436,   745,
      -1,    -1,    -1,    -1,    -1,    -1,  1287,  1288,    -1,    -1,
      -1,    -1,    -1,    71,   824,    -1,   762,    -1,    -1,    -1,
      -1,   960,    -1,    -1,    -1,    -1,  1187,    -1,    -1,   968,
      -1,    -1,    -1,    -1,    -1,    -1,  1197,    -1,   784,   785,
    1321,  1322,    -1,    -1,   102,    -1,   175,   105,   106,   107,
     108,   109,   110,   111,   112,   113,    -1,   803,   997,    -1,
      -1,  1000,    -1,    -1,  1225,    -1,    -1,    -1,    -1,    -1,
      -1,   102,   130,    -1,   105,   106,   107,   108,   109,   110,
     111,   112,   113,    -1,  1522,   831,    -1,    -1,  1526,    -1,
    1528,    -1,   150,   151,  1532,    -1,  1534,    -1,    -1,    -1,
      -1,    -1,    -1,   161,   162,    -1,    -1,    -1,   854,    -1,
    1166,    -1,    -1,    -1,    -1,    -1,   862,    -1,    -1,   865,
      -1,  1060,   153,   869,  1562,  1563,    -1,   158,    -1,   875,
     876,  1292,  1293,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   887,    -1,   889,   890,  1201,   892,   102,    -1,    -1,
     105,   106,   107,   108,   109,   110,   111,   112,   113,  1597,
      -1,    -1,    -1,    -1,    -1,   737,   738,    -1,    -1,    -1,
    1608,    -1,    -1,  1611,    -1,   747,   102,    -1,   750,   105,
     106,   107,   108,   109,   110,   111,   112,   113,    -1,    -1,
    1122,    -1,    -1,   939,    -1,    -1,    -1,    -1,    -1,    -1,
    1638,    13,    14,    15,    16,    17,   161,    -1,    -1,    -1,
      -1,    -1,    -1,  1023,    -1,    -1,    -1,    -1,    -1,  1029,
    1030,    -1,    -1,    -1,    -1,  1663,  1664,  1508,  1509,    -1,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   811,
    1678,  1679,    -1,   815,    -1,    -1,    -1,   819,    -1,   175,
      -1,  1412,    -1,    -1,    -1,  1693,  1002,    -1,  1696,    71,
      -1,   102,    -1,  1544,   105,   106,   107,   108,   109,   110,
     111,   112,   113,  1019,  1020,  1713,    -1,  1216,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1342,    -1,  1726,   130,
     102,    -1,    -1,   105,   106,   107,   108,   109,   110,   111,
     112,   113,    -1,  1242,    -1,  1602,    -1,    -1,    -1,   150,
     151,  1250,    -1,   154,    -1,  1753,    -1,    -1,   130,    -1,
     161,   162,    -1,    -1,    -1,    -1,  1072,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1390,    -1,    -1,    -1,   150,   151,
      -1,  1622,   154,    -1,    -1,    -1,    -1,    -1,    -1,   161,
     162,    -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,  1797,
      -1,    -1,    -1,    -1,    -1,    -1,  1112,  1528,  1806,    -1,
      -1,  1181,  1653,    -1,  1120,    -1,    -1,  1658,  1659,    -1,
      -1,    -1,    -1,    -1,   102,    -1,    -1,   105,   106,   107,
     108,   109,   110,   111,   112,   113,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1149,  1842,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1166,  1167,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1368,
      -1,  1869,   150,   151,  1006,    -1,    -1,    -1,    -1,  1877,
      -1,  1187,    -1,   161,   162,    -1,    -1,  1608,    -1,  1887,
      -1,  1197,    -1,    -1,    -1,  1201,  1388,    -1,     1,    71,
      -1,     4,    -1,    -1,    -1,    -1,     1,  1213,    -1,    -1,
      -1,    -1,  1282,    -1,    -1,    -1,    -1,  1915,  1916,  1225,
    1918,  1919,    -1,  1229,    -1,    -1,    -1,    -1,    -1,    -1,
     102,    -1,    -1,   105,   106,   107,   108,   109,   110,   111,
     112,   113,  1663,  1664,    -1,  1077,  1562,    -1,  1080,  1448,
      -1,    -1,    -1,    -1,  1453,  1953,    59,    -1,   130,  1458,
    1459,  1460,    -1,    -1,    59,    -1,    -1,  1273,  1274,    -1,
      -1,  1341,    -1,    -1,    -1,    -1,    -1,    -1,   150,   151,
      83,  1287,  1288,  1981,  1982,    -1,  1292,  1293,    -1,   161,
     162,    -1,  1849,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     103,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,
      -1,    -1,    -1,    -1,    -1,  1321,  1322,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  2022,  1331,  1332,  1333,  1334,  1335,
    1336,    -1,    -1,    -1,    -1,    -1,  1342,   140,    -1,    -1,
      -1,    -1,    -1,   146,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   146,    -1,    -1,  1360,  1361,    -1,    -1,    -1,    -1,
     163,    -1,    -1,  1555,    -1,    -1,    -1,    -1,   163,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1568,    -1,   181,    -1,
      -1,    -1,    -1,    -1,  1390,    -1,    -1,    -1,    -1,   192,
     193,    -1,    -1,    -1,    -1,    -1,    -1,  1713,   193,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1412,    -1,    -1,    -1,
      -1,    -1,    -1,  1605,    -1,    -1,   102,    -1,   221,   105,
     106,   107,   108,   109,   110,   111,   112,   113,    -1,    -1,
    1436,   377,    -1,    -1,   237,    -1,    -1,  1269,    -1,   242,
     243,    -1,    -1,   246,    -1,    -1,  1278,  1646,  1869,  1648,
      -1,    -1,    -1,    -1,    -1,    -1,  1526,    -1,  1999,    -1,
      -1,    -1,  1532,    -1,  1534,   268,    -1,    -1,   271,    -1,
     273,    -1,   158,   268,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   284,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   284,
      -1,    -1,    -1,    -1,   297,    -1,    -1,    -1,    -1,    -1,
      -1,   102,  1508,  1509,   105,   106,   107,   108,   109,   110,
     111,   112,   113,    -1,    -1,  1707,    -1,    -1,    -1,  1711,
     323,    -1,  1528,   326,    -1,    -1,    -1,    -1,   323,    -1,
      -1,   326,    -1,  1725,    -1,    -1,    -1,   340,  1544,    -1,
      -1,   344,    -1,  1735,    -1,   340,    -1,    -1,    -1,   344,
     151,    -1,    -1,   154,    -1,    -1,  1562,  1563,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   368,    -1,    -1,    -1,    -1,
     516,   517,    -1,    -1,    -1,    13,    14,    15,    16,    17,
       4,     5,     6,     7,     8,     9,    10,    11,    12,   102,
      -1,  1597,   105,   106,   107,   108,   109,   110,   111,   112,
     113,    -1,  1608,  1919,   102,  1611,    -1,   105,   106,   107,
     108,   109,   110,   111,   112,   113,  1622,    -1,    -1,    -1,
      -1,  1813,    -1,  1693,    -1,  1817,  1696,    -1,    -1,    -1,
    1822,    -1,  1638,    71,   437,    -1,    -1,    -1,   441,    -1,
      -1,    -1,    -1,    -1,    -1,   158,   441,  1653,    -1,    -1,
      -1,    -1,  1658,  1659,    -1,  1847,    -1,  1663,  1664,    -1,
     158,   464,    -1,    -1,   102,    -1,    -1,   105,   106,   107,
     108,   109,   110,   111,   112,   113,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1753,    -1,    -1,    -1,    -1,    -1,    -1,
    1522,    -1,   130,    -1,    -1,    -1,    -1,    -1,  1890,    -1,
      -1,    -1,  1894,    -1,    -1,   651,    -1,  1713,    -1,    -1,
     656,    -1,   150,   151,    -1,  1907,   662,    -1,    -1,   522,
    1726,    -1,    -1,   161,   162,    -1,    -1,   522,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   681,  1806,    -1,    -1,  1931,
      -1,  1933,  1934,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     553,    -1,    -1,    -1,    -1,   558,    -1,    -1,   553,   562,
      -1,   564,    -1,    -1,  1956,    -1,    -1,   562,    -1,   564,
      -1,   717,   575,    -1,   577,    -1,    -1,    -1,    -1,    -1,
     575,    -1,   577,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     593,   594,    -1,    -1,    -1,    -1,    -1,    -1,   593,   594,
    1992,    -1,    -1,    -1,    -1,   608,    -1,    -1,    -1,    -1,
    2002,    -1,    -1,     1,   617,    -1,    -1,    -1,    71,   622,
      -1,    -1,   617,    -1,    -1,    -1,    -1,   630,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   630,  1842,    -1,    -1,    -1,
      -1,    -1,    -1,  2035,    -1,  2037,  1678,  1679,  1918,   102,
      -1,    -1,   105,   106,   107,   108,   109,   110,   111,   112,
     113,    -1,    -1,  1869,  2056,    -1,    -1,    -1,    -1,    -1,
    2062,    59,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   102,
    2072,  1887,   105,   106,   107,   108,   109,   110,   111,   112,
     113,    -1,    -1,    -1,    -1,    -1,    -1,   150,   151,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   130,    -1,  1915,
    1916,  1981,  1982,  1919,    -1,   103,    -1,    -1,   721,    -1,
      13,    14,    15,    16,    17,    -1,   721,   150,   151,    -1,
      -1,   154,    -1,    -1,    -1,   738,    -1,    -1,   161,   162,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1953,    -1,    -1,
      -1,   174,  2022,    13,    14,    15,    16,    17,   146,   762,
      -1,    -1,    -1,    -1,   767,  1797,    -1,   762,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   163,    -1,    -1,    71,    -1,
      -1,   784,   785,    -1,    -1,    -1,    -1,    -1,    -1,   784,
     785,    -1,    -1,  1999,    -1,    -1,    -1,    -1,    -1,    -1,
     803,    -1,    -1,    -1,    -1,   193,    -1,    -1,   803,   102,
      -1,    71,   105,   106,   107,   108,   109,   110,   111,   112,
     113,    -1,    13,    14,    15,    16,    17,    -1,   831,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   831,   130,    -1,    -1,
      -1,  1873,   102,    -1,    -1,  1877,    -1,   107,   108,   109,
     110,   111,   112,   113,    -1,    -1,    -1,   150,   151,   862,
      -1,   154,    -1,    -1,    -1,    -1,   869,   862,   161,   162,
     130,    -1,   875,     1,   869,    -1,     4,    -1,    -1,    -1,
      71,    -1,    -1,    -1,   887,    -1,  1918,   890,    -1,   892,
     150,   151,   887,    -1,   897,   890,   284,   892,    -1,    -1,
      -1,   161,   162,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   102,    -1,    -1,   105,   106,   107,   108,   109,   110,
     111,   112,   113,    -1,    -1,    -1,    -1,    -1,    -1,     1,
      -1,    59,    -1,    -1,    -1,   323,   939,    -1,   326,   130,
      -1,    -1,    -1,    -1,   939,    -1,    -1,    -1,    -1,  1981,
    1982,    -1,   340,    -1,    -1,    83,   344,    -1,   102,   150,
     151,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     161,   162,    -1,    -1,    -1,   103,    -1,    49,    -1,    -1,
      -1,    53,    -1,    55,    -1,    -1,    -1,    -1,    -1,    -1,
    2022,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1002,
      72,    -1,    -1,    -1,    -1,    -1,    -1,  1002,    -1,    -1,
     154,    -1,   140,    -1,    -1,    -1,  1019,  1020,   146,    -1,
     148,    -1,    -1,    -1,  1019,  1020,    -1,    99,   100,    -1,
     102,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,    -1,   118,   119,   120,    -1,
     122,   123,   180,   441,    -1,    -1,    -1,    -1,   130,    -1,
      -1,    -1,    -1,    -1,   192,    -1,    -1,    -1,    -1,  1072,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1072,   150,    -1,
      -1,   153,   154,    -1,    -1,    -1,    -1,    -1,   160,   161,
     162,   163,   164,   165,   166,   100,    -1,   102,    -1,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,  1112,
      -1,    -1,    -1,    -1,    -1,   243,    -1,  1120,   246,    -1,
      -1,    -1,    -1,   251,    -1,  1120,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   522,    -1,    -1,    -1,    -1,    13,
      14,    15,    16,    17,    -1,   150,  1149,    -1,   153,   154,
      -1,    -1,    -1,    -1,  1149,    -1,   284,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1167,   553,    -1,    -1,    -1,    -1,
      -1,    -1,  1167,    -1,   562,   303,   564,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   575,    -1,   577,
      -1,    -1,    -1,    -1,  1197,    -1,    -1,    71,    -1,    -1,
      -1,    -1,  1197,  1206,    -1,   593,   594,    -1,    -1,    -1,
      -1,  1357,    -1,    -1,  1360,  1361,    -1,    -1,    -1,    -1,
    1366,    -1,  1225,    -1,  1370,    -1,  1372,    -1,   102,   617,
    1225,   105,   106,   107,   108,   109,   110,   111,   112,   113,
      -1,    -1,   630,    -1,    -1,    -1,    -1,    -1,    -1,   377,
      -1,    -1,    -1,    -1,    -1,    -1,   130,   102,    -1,    -1,
     105,   106,   107,   108,   109,   110,   111,   112,   113,    -1,
    1273,  1274,    -1,    -1,    -1,    -1,   150,   151,  1273,  1274,
      -1,    -1,    -1,   411,  1287,  1288,    -1,   161,   162,  1292,
    1293,    -1,  1287,  1288,    -1,    -1,   102,  1292,  1293,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   153,   437,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1321,  1322,
      -1,    -1,    -1,    -1,   130,    -1,  1321,  1322,  1331,  1332,
    1333,  1334,   102,   721,    -1,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   150,   151,    -1,    -1,    -1,    -1,
      -1,   157,    -1,    -1,    -1,   161,   162,  1360,  1361,    -1,
     130,    -1,    -1,    -1,    -1,  1360,  1361,    -1,  1514,    -1,
      -1,    -1,    -1,    -1,   762,    -1,    -1,    -1,    -1,    -1,
     150,   151,    -1,    -1,   154,    -1,    -1,    -1,   516,   517,
      -1,   161,   162,    -1,    -1,    -1,   784,   785,    -1,    -1,
    1546,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1412,
      -1,    -1,  1558,    -1,    -1,   803,    -1,  1412,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1573,  1574,    -1,
     558,    -1,    -1,  1436,   562,    -1,   564,    -1,    -1,    -1,
      -1,    -1,    -1,   831,    -1,    -1,    -1,    -1,    -1,   577,
      -1,  1597,   102,    -1,    -1,   105,   106,   107,   108,   109,
     110,   111,   112,   113,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   862,    -1,    -1,    -1,   102,    -1,
     130,   869,    -1,   107,   108,   109,   110,   111,   112,   113,
     114,    -1,    -1,    -1,   118,    -1,   120,    -1,    -1,   887,
     150,   151,   890,    -1,   892,  1508,  1509,    -1,    -1,    -1,
     638,   161,   162,  1508,  1509,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   651,    -1,   653,   654,   151,   656,    -1,
     154,  1534,    -1,     1,   662,    -1,     4,   665,   666,   667,
      -1,  1544,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1544,
      -1,   939,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1563,    -1,    -1,  1709,    -1,    -1,    -1,    -1,  1563,    -1,
      -1,    -1,  1718,    -1,  1720,    -1,    -1,  1723,  1724,    -1,
    1726,    -1,    -1,    -1,    -1,  1731,    -1,    -1,    -1,    -1,
      -1,    59,    -1,    -1,  1597,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1597,    -1,    -1,  1608,    -1,    -1,  1611,    -1,
      -1,    -1,    -1,  1608,  1002,    83,    -1,   745,    -1,  1622,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1622,    -1,    -1,
      -1,  1019,  1020,    -1,    -1,   103,    -1,   102,    -1,    -1,
     105,   106,   107,   108,   109,   110,   111,   112,   113,    -1,
    1653,    -1,    -1,    -1,    -1,  1658,  1659,    -1,  1653,    -1,
    1663,  1664,    -1,  1658,  1659,   130,    -1,    -1,  1663,  1664,
      -1,    -1,   140,    -1,    -1,    -1,  1679,    -1,   146,    -1,
     148,    -1,  1828,    -1,  1072,   150,   151,  1833,  1834,    -1,
      -1,    -1,    -1,   102,    -1,    -1,   161,   162,   107,   108,
     109,   110,   111,   112,   113,   114,    -1,    -1,    -1,   118,
     102,   120,   180,   105,   106,   107,   108,   109,   110,   111,
     112,   113,    -1,  1726,   192,    -1,    -1,    -1,    -1,    -1,
      -1,  1726,  1120,    -1,   862,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   151,    -1,    -1,   154,    -1,   875,   876,  1895,
      -1,  1897,    -1,    -1,  1900,  1901,    -1,    -1,   102,  1905,
    1906,  1149,    -1,   107,   108,   109,   110,   111,   112,   113,
     114,    -1,    -1,    -1,   118,   243,   120,    -1,   246,  1167,
      -1,   102,    -1,   251,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   102,    -1,    -1,   105,   106,   107,   108,
     109,   110,   111,   112,   113,    -1,    -1,   151,    -1,  1197,
     154,    -1,    87,    -1,    -1,    -1,   284,    -1,    93,    94,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1974,  1975,
    1976,    -1,   153,    -1,    -1,   303,    -1,  1225,    -1,  1842,
      -1,   150,    -1,    -1,    -1,    -1,    -1,  1842,    -1,  1995,
     102,    -1,   127,   105,   106,   107,   108,   109,   110,   111,
     112,   113,  2008,  2009,  2010,    -1,  1869,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1869,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1887,  1273,  1274,    -1,     1,    -1,
      -1,     4,  1887,    -1,    -1,    -1,    -1,    -1,   150,  1287,
    1288,    -1,    -1,    -1,  1292,  1293,    -1,    -1,    -1,   377,
      -1,    -1,  1915,  1916,    -1,    -1,    -1,    -1,    -1,   102,
    1915,  1916,    -1,    -1,   107,   108,   109,   110,   111,   112,
     113,   114,    -1,  1321,  1322,   118,    -1,   120,    -1,    -1,
      -1,    -1,    -1,   411,    -1,    -1,    59,    -1,    -1,    -1,
    1953,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1953,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,   437,
      83,    -1,  1360,  1361,    -1,    -1,    -1,    -1,    -1,  1982,
      -1,    -1,    -1,    -1,  1112,    -1,    -1,    -1,    -1,    -1,
     103,    -1,  1120,    -1,    -1,    -1,  1999,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1999,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   293,    -1,
      -1,  1149,    -1,    -1,  1412,    -1,    -1,   140,    -1,    -1,
      -1,    -1,    -1,   146,    -1,   148,    -1,    -1,    -1,  1167,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   516,   517,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1187,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   180,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   192,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     558,    -1,    -1,    -1,   562,    -1,   564,    -1,    -1,    -1,
      -1,  1229,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   577,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1508,  1509,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     243,    -1,    -1,   246,    -1,    -1,    -1,    -1,   251,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1544,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   440,    -1,   442,    -1,    -1,
     638,   284,    -1,    -1,    -1,  1563,   451,   452,    -1,    -1,
      -1,    -1,    -1,   651,    -1,   653,   654,    -1,   656,    -1,
     303,    -1,    -1,    -1,   662,    -1,    -1,   665,   666,   667,
      -1,    -1,    -1,  1331,  1332,  1333,  1334,  1335,  1336,  1597,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1608,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1360,  1361,  1622,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   377,  1653,    -1,    -1,    -1,    -1,
    1658,  1659,    -1,    -1,    -1,  1663,  1664,   745,    -1,    -1,
      -1,    -1,    -1,    -1,   559,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   411,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1436,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   437,    -1,    -1,    -1,    -1,     3,
      -1,    -1,    -1,    -1,     0,    -1,    10,     3,  1726,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    52,    -1,
      -1,    -1,    56,    57,    58,    59,    60,    61,    62,    63,
      -1,    -1,    -1,    -1,   862,    -1,    -1,    71,    72,    -1,
    1528,    -1,    -1,   516,   517,    -1,    -1,   875,   876,    -1,
      -1,    77,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,
      -1,   105,   106,    -1,    -1,  1563,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   558,    -1,    -1,    -1,   562,
      -1,   564,    -1,    -1,  1842,    -1,   130,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   577,    -1,    -1,    -1,    -1,  1597,
     136,    -1,    -1,    -1,    -1,   149,    -1,    -1,    -1,   153,
     154,  1869,    -1,  1611,    -1,    -1,    -1,   161,   162,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1887,
      -1,    -1,    -1,   778,    -1,    -1,    -1,    -1,    -1,    -1,
    1638,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   638,    -1,  1915,  1916,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   651,    -1,
     653,   654,    -1,   656,    -1,    -1,    -1,    -1,    -1,   662,
      -1,    -1,   665,   666,   667,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   230,  1953,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   245,
     855,   856,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   255,
      -1,   866,   867,   868,    -1,    -1,   871,    -1,  1726,   265,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1999,    -1,   279,   280,    -1,    -1,    -1,    -1,    -1,
     286,   287,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   745,    -1,    -1,    -1,   302,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1112,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1120,    -1,    -1,    -1,   322,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   953,    -1,
      -1,  1149,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1167,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   378,    -1,    -1,    -1,    -1,    -1,    -1,  1187,
      -1,    -1,    -1,    -1,   999,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   409,    -1,    -1,    -1,    -1,    -1,   862,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1887,
      -1,  1229,   875,   876,    -1,    -1,    -1,    -1,    48,   435,
      -1,  1046,    -1,   439,    -1,    -1,    -1,    -1,    -1,    -1,
    1055,  1056,  1057,  1058,    -1,    -1,    -1,  1915,  1063,  1064,
      -1,    -1,   458,    -1,    74,    -1,   462,   463,  1073,    -1,
     466,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   481,   482,   483,   484,  1094,
      -1,  1096,    -1,    -1,    -1,  1953,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   500,    -1,    -1,    -1,    -1,    -1,
     120,    -1,   508,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   133,    -1,   135,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1331,  1332,  1333,  1334,  1335,  1336,    -1,
     536,    -1,    -1,    -1,  1149,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   165,    -1,    -1,    -1,    -1,
      -1,    -1,  1360,  1361,    -1,    -1,    -1,    -1,    -1,    -1,
    1175,   567,   182,    -1,    -1,    -1,    -1,  1182,   574,  1184,
    1185,    -1,    -1,    -1,   580,    -1,    -1,    -1,    -1,    -1,
      -1,  1196,    -1,  1198,    -1,  1200,    -1,  1202,    -1,    -1,
      -1,    -1,  1207,    -1,    -1,    -1,    -1,    -1,   604,   605,
     220,    -1,    -1,    -1,   224,    -1,    -1,   227,   228,    -1,
      -1,   231,    -1,    -1,   234,   235,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1436,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1268,    -1,    -1,    -1,    -1,    -1,  1112,
    1275,  1276,   668,    -1,    -1,    -1,    -1,  1120,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   296,    -1,    -1,   299,
      -1,    -1,    -1,    -1,  1299,    -1,    -1,    -1,    -1,    -1,
      -1,  1306,    -1,    -1,    -1,  1310,  1149,    -1,    -1,    -1,
     320,   321,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1167,    -1,   336,    -1,    -1,    -1,
    1528,    -1,    -1,    -1,    -1,  1340,    -1,    -1,    -1,    -1,
      -1,   737,    -1,    -1,  1187,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   752,    -1,    -1,    -1,
     756,    -1,    -1,    -1,    -1,  1563,    -1,    -1,    -1,   765,
      -1,    -1,    -1,    -1,    -1,  1380,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1229,    -1,    -1,    -1,
      -1,   787,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1597,
     796,    -1,    -1,    -1,    -1,    -1,   802,    -1,    -1,    -1,
      -1,  1416,    -1,  1611,    -1,    -1,    -1,    -1,    -1,  1424,
     430,  1426,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1638,    -1,    -1,   839,    -1,    -1,    -1,    -1,    -1,    -1,
     846,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1473,  1474,
      -1,    -1,    -1,    -1,    -1,   485,    -1,   873,    -1,    -1,
      -1,    -1,    -1,  1488,  1489,    -1,  1491,   497,  1331,  1332,
    1333,  1334,  1335,  1336,    -1,  1500,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1510,  1511,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1360,  1361,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1726,    -1,
      -1,    -1,    -1,   929,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    -1,    -1,
      20,   571,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    -1,
      50,    51,    52,  1436,    54,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   612,   613,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    71,    -1,    -1,    -1,   625,  1012,    99,  1623,  1624,
    1016,    -1,    -1,    -1,    -1,    -1,    -1,  1023,   110,  1634,
     112,    -1,   114,    -1,    -1,    -1,   179,  1033,    -1,    -1,
      -1,    -1,    -1,    -1,  1040,   105,   106,    -1,    -1,    -1,
      -1,    -1,    -1,  1049,    -1,  1051,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1669,  1670,    -1,    -1,    -1,    -1,
     152,    -1,   154,   155,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1528,    -1,  1083,    -1,  1887,
      -1,  1087,    -1,   153,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1101,    -1,    -1,  1104,    -1,
      -1,    -1,    -1,    -1,   196,    -1,    -1,  1915,    -1,    -1,
    1563,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   741,   742,    -1,    -1,    -1,    -1,   747,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1757,    -1,  1597,  1953,    -1,    -1,   768,    -1,
      -1,   771,   772,    -1,   774,    -1,   776,   777,  1611,    -1,
      -1,  1776,    -1,    -1,  1779,  1780,    -1,   259,    -1,   261,
     262,  1786,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    48,    -1,    -1,  1638,    -1,    -1,    -1,  1195,
      -1,    -1,    -1,    -1,    -1,   815,    -1,   289,    -1,   819,
      -1,    -1,    -1,   295,    -1,    -1,    -1,    -1,    -1,   362,
      -1,    -1,   365,   366,  1220,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   375,   376,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   325,    -1,    -1,    -1,   390,   391,   331,
      -1,   333,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   120,    -1,    -1,    -1,   411,    -1,
      -1,    -1,    -1,    -1,   884,   885,    -1,   133,    -1,   135,
      -1,    -1,    -1,  1726,    -1,    -1,    -1,    -1,   898,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   441,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1917,    -1,    -1,    -1,    -1,  1313,    -1,    -1,
      -1,  1317,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   421,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1347,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   447,    -1,   449,   450,    -1,
      -1,   227,   228,    -1,  1979,   231,    -1,    -1,   234,   235,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2003,    -1,
      -1,  1397,    -1,    -1,  1400,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   494,  2018,    -1,    -1,   163,    -1,    -1,  1029,
      -1,    -1,  1418,    -1,    -1,    -1,    -1,    -1,  2033,    -1,
      -1,    -1,   514,    -1,    -1,    -1,    -1,   519,    -1,   521,
      -1,    -1,    -1,    -1,  1887,   192,   193,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   541,
    1070,   543,   544,    -1,   320,   321,    -1,  1077,    -1,    -1,
    1080,    -1,  1915,  1469,    -1,    -1,    -1,   224,    -1,   561,
     336,    -1,  1478,    -1,   231,    -1,  1482,    -1,    -1,    -1,
      -1,   573,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1496,  1497,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1953,    -1,    -1,    -1,   596,    -1,   598,   599,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   620,   621,
      -1,    -1,    -1,    -1,    -1,   627,    -1,    -1,    -1,    -1,
      -1,    -1,   299,    -1,    -1,   698,   699,   700,   701,   702,
     703,   704,   705,   706,   707,   708,   709,   710,   711,   712,
     713,   714,   715,   716,   430,    -1,   323,   324,   660,   661,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1203,    -1,    -1,    -1,   344,    -1,    -1,
      -1,  1211,  1212,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1612,  1613,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   778,    -1,    -1,    -1,    -1,
      -1,   497,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1269,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   414,  1278,    -1,
      -1,  1281,    -1,  1283,  1284,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   430,   431,    -1,   433,   434,    -1,    -1,
      -1,    -1,    -1,    -1,   441,    -1,    -1,    -1,   445,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1324,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   486,
      -1,    -1,    -1,   490,  1740,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   612,   613,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   625,
    1766,    -1,   854,    -1,   917,   522,    -1,    -1,    -1,    -1,
      -1,  1391,    -1,   865,    -1,    -1,    -1,    -1,  1784,    -1,
      -1,    -1,   935,    -1,    -1,    -1,    -1,   940,    -1,    -1,
      -1,    -1,    -1,    -1,    83,    -1,    -1,   889,   951,    -1,
      -1,    -1,    -1,    -1,    -1,  1811,    -1,    -1,   900,    -1,
      99,    -1,    -1,    -1,    -1,   572,    -1,   909,   575,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1838,    -1,    -1,  1841,   593,   594,    -1,    -1,
     993,    -1,    -1,    56,    57,    -1,    -1,   604,    -1,    -1,
      -1,   608,    -1,    -1,    -1,    -1,    -1,  1477,   615,   148,
     617,    -1,    -1,   152,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   163,   741,   742,    -1,    91,    -1,
      -1,   747,    -1,    -1,    -1,    -1,  1506,    -1,    -1,    -1,
      -1,   180,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   768,    -1,   193,   771,   772,   196,   774,    -1,
     776,   777,  1532,    -1,  1006,    -1,    -1,    -1,  1538,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,
      -1,   144,    -1,    -1,    -1,    -1,  1942,    -1,  1030,    -1,
      -1,    -1,    -1,    -1,   157,    -1,    -1,    -1,    -1,   815,
      -1,    -1,    -1,   819,    -1,    -1,    -1,   246,    -1,    -1,
      -1,    -1,    -1,    -1,   721,    -1,    -1,  1120,    -1,    -1,
     259,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     737,   738,    -1,    -1,    -1,    -1,    -1,  1607,    -1,    -1,
     747,   748,    -1,   750,   751,    -1,    -1,    -1,   211,    -1,
      -1,    -1,    -1,    -1,    -1,   762,   295,    -1,   765,    -1,
     767,   768,    -1,    -1,   303,    -1,    -1,   774,   884,   885,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   784,   785,    -1,
      -1,    -1,   898,    -1,   323,    -1,   325,    -1,    -1,    -1,
      -1,    -1,    -1,   256,  1197,    -1,   803,    -1,    -1,    -1,
     807,    -1,    -1,   266,   811,    -1,    -1,    -1,   815,   816,
      -1,    -1,   819,   820,   277,  1685,  1686,    -1,    -1,    -1,
     827,    -1,    -1,  1693,  1166,    -1,    -1,  1697,    -1,  1232,
    1233,  1234,    -1,    -1,    -1,    -1,  1239,  1240,   377,    -1,
      -1,   304,    -1,    -1,    -1,    -1,    -1,    -1,   311,   312,
      -1,    -1,    -1,   316,    -1,    -1,    -1,    -1,    -1,  1201,
    1263,    -1,   869,   870,    -1,  1207,    -1,    -1,    -1,    -1,
      -1,    -1,   411,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   349,    -1,    -1,    -1,
      -1,   354,    -1,   900,   357,    -1,    -1,  1300,  1301,    -1,
      -1,    -1,   441,    -1,    -1,    -1,    -1,    -1,   447,    -1,
      -1,    -1,    -1,  1029,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1799,
      -1,    -1,   939,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1070,    -1,    -1,    -1,    -1,    -1,
      -1,  1077,    -1,    -1,  1080,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   516,   517,    -1,
      -1,   444,    -1,   522,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   456,   457,    -1,    -1,    -1,    -1,  1006,
    1342,    -1,    -1,  1873,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1023,  1024,    -1,    -1,
      -1,    -1,    -1,  1030,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   575,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1390,    -1,
      -1,    -1,    -1,    -1,    -1,   594,    -1,   596,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1072,    -1,    -1,    -1,    -1,
    1077,  1078,    -1,  1080,  1081,    -1,    -1,    -1,   617,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1203,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1211,  1212,    -1,    -1,   638,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1981,   651,    -1,   653,   654,    -1,   656,    -1,    -1,
      -1,    -1,    -1,   662,   587,    -1,   665,   666,   667,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1269,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1278,    -1,    -1,  1281,    -1,  1283,  1284,    -1,
      -1,    -1,    -1,   636,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   721,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1197,    -1,    -1,    -1,    -1,    -1,  1203,  1204,    -1,  1602,
      -1,    -1,    -1,    -1,    -1,    -1,   745,    -1,  1324,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1225,    -1,
    1562,    -1,    -1,   762,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   784,   785,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1269,  1270,   803,    -1,    -1,    -1,    -1,    -1,
      -1,  1278,  1279,    -1,  1281,  1391,    -1,    -1,    -1,    -1,
      13,    14,    15,    16,    17,  1292,  1293,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,   779,    -1,    51,    52,
      -1,    -1,    -1,   786,    -1,    -1,    -1,    -1,    -1,    -1,
     869,    -1,    -1,    -1,    -1,    -1,    -1,   876,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     889,    -1,    -1,    -1,  1696,    -1,    -1,    -1,    -1,    -1,
      -1,  1477,    -1,    -1,     5,    -1,    -1,    -1,    -1,   102,
      -1,  1713,    13,    14,    15,    16,    17,    -1,    -1,    -1,
      -1,   180,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1506,    -1,    -1,    -1,   193,    -1,    -1,    -1,   861,    -1,
     939,    -1,    -1,    -1,    -1,  1412,  1809,   206,    49,   208,
      -1,    -1,    53,    -1,    55,    -1,    -1,    -1,    -1,    -1,
     883,    -1,    -1,    -1,   157,    -1,    -1,    -1,    -1,    -1,
      71,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1849,    -1,    -1,    -1,
      -1,    -1,   915,    -1,    -1,  1797,   919,    -1,    99,   100,
      -1,   102,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,    -1,   118,   119,   120,
      -1,   122,   123,    -1,    -1,    -1,    -1,    -1,    -1,   130,
      -1,  1607,    -1,    -1,    -1,   294,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   150,
      -1,    -1,   153,   154,    -1,  1522,    -1,    -1,    -1,   160,
     161,   162,   163,   164,   165,   166,    -1,    -1,    -1,    -1,
      -1,  1538,    -1,  1072,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1947,    -1,  1949,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1685,
    1686,    -1,    -1,  1112,    -1,    -1,    -1,  1919,    -1,    -1,
      -1,  1697,    -1,    -1,    -1,  1988,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1608,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2020,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1166,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1187,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1663,  1664,  1197,    -1,
      -1,    -1,  1125,  1126,  1127,  2068,    -1,    -1,   467,    -1,
      -1,  1678,  1679,    -1,   473,    -1,    -1,    -1,    -1,   478,
    2022,    -1,    -1,  1799,    -1,    -1,  1225,  1694,    -1,    -1,
    1229,    -1,  1155,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    13,    14,    15,    16,    17,  1170,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    52,    -1,    -1,    -1,    -1,    -1,  1210,    -1,    -1,
      -1,    -1,    -1,  1292,  1293,    -1,    -1,  1873,    -1,    -1,
      71,    -1,    -1,    -1,    -1,    -1,   565,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1797,   102,  1331,  1332,  1333,   594,  1335,  1336,  1805,    -1,
      -1,    -1,    -1,  1342,    -1,    -1,    -1,    -1,   607,    -1,
      -1,    13,    14,    15,    16,    17,    -1,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    -1,    50,    51,
      52,  1390,    54,    -1,    -1,    -1,    -1,    -1,    -1,   658,
      -1,    -1,  1869,    -1,    -1,    -1,  1873,  1874,    -1,    71,
    1877,    -1,    -1,  1412,    -1,    -1,    -1,    -1,    -1,    -1,
     679,   680,  1345,    -1,   683,  1348,   685,    -1,    -1,    -1,
      -1,    -1,   691,    -1,   693,   694,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   105,   106,    -1,    -1,    -1,    -1,    -1,
      -1,  1918,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   721,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   734,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   745,  1410,  1411,    -1,
      -1,   153,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     759,    -1,    -1,   762,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1981,  1982,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1528,
     789,    -1,    -1,   792,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     3,  2022,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1562,    13,    14,    15,    16,    17,   828,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      -1,    50,    51,    52,    -1,    54,    -1,    -1,    -1,  1608,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   876,  1541,    -1,
      -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     889,   890,    -1,    -1,    -1,    -1,    -1,    -1,   897,  1638,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   105,   106,    -1,    -1,
      -1,    -1,    -1,    -1,  1663,  1664,    -1,   926,    -1,    -1,
      -1,    -1,    -1,     1,    -1,    -1,    -1,    -1,    -1,    -1,
     939,   130,    -1,    -1,    -1,    -1,    -1,    -1,   947,    -1,
      18,    -1,    -1,    -1,    -1,   954,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   153,   154,    -1,  1630,    -1,    -1,
      -1,    -1,   161,   162,  1713,    -1,    -1,    -1,    -1,    -1,
      -1,    49,    -1,    -1,    -1,    53,    -1,    55,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1000,    70,    -1,    72,    73,    -1,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    90,    91,    92,    93,    94,    95,    -1,    97,
      -1,    99,   100,    -1,   102,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,    -1,   122,   123,    -1,    -1,    -1,    -1,
      -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,  1067,    -1,
    1069,    -1,  1071,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   149,   150,    -1,    -1,   153,   154,    -1,    -1,  1752,
     158,    -1,   160,   161,   162,   163,   164,   165,   166,    -1,
    1763,    -1,    -1,  1842,    -1,    -1,    -1,   175,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1869,    -1,    -1,    13,    14,    15,    16,    17,  1137,  1138,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    -1,
      50,    51,    52,    -1,    54,    -1,    -1,  1916,    -1,    -1,
    1919,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    71,    -1,     1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1201,    -1,    -1,    -1,    -1,    -1,  1207,    -1,
      18,    -1,    -1,    -1,    -1,    -1,    -1,  1216,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1225,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    49,    -1,  1242,    -1,    53,    -1,    55,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1257,    -1,
      -1,  1260,    70,    -1,    72,    73,    -1,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    90,    91,    92,    93,    94,    95,    -1,    97,
      -1,    99,   100,    -1,   102,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,  1312,   122,   123,    -1,    -1,    -1,    -1,
      -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   149,   150,    -1,    -1,   153,   154,    -1,    -1,    -1,
     158,  1350,   160,   161,   162,   163,   164,   165,   166,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   175,    -1,  1368,
      -1,    -1,  1371,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      18,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1412,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1422,  1423,    -1,    -1,    -1,    -1,    -1,
      -1,    49,    -1,    -1,    -1,    53,    -1,    55,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1448,
      -1,  1450,    70,    -1,    72,    73,    -1,    75,    -1,    -1,
      78,    79,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    90,    91,    92,    93,    94,    95,    -1,    97,
      -1,    99,   100,    -1,   102,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,    49,   122,   123,    -1,    53,    -1,    55,
      -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,  1528,
      -1,    -1,   150,    -1,  1533,   153,   154,    -1,    -1,    -1,
     158,    -1,   160,   161,   162,   163,   164,   165,   166,    -1,
      -1,    -1,    -1,    99,   100,    -1,   102,   175,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,    -1,   118,   119,   120,    -1,   122,   123,    -1,    -1,
      -1,    -1,    -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1593,    -1,    -1,    -1,   144,   145,
     146,    -1,    -1,    -1,   150,   151,    -1,   153,   154,    -1,
      -1,    -1,    -1,    -1,   160,   161,   162,   163,   164,   165,
     166,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1636,    -1,    -1,
    1639,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    49,    -1,    51,
      52,    53,    -1,    55,    56,    57,    58,    59,    60,    61,
      62,    63,    64,    -1,    -1,    -1,    68,    -1,    70,    71,
      72,    73,    -1,    75,    -1,    -1,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    91,
      92,    93,    94,    95,    -1,    97,    -1,    99,   100,   101,
     102,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,    -1,
     122,   123,    -1,    -1,    -1,    -1,    -1,    -1,   130,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,   150,    -1,
      -1,   153,   154,    -1,    -1,    -1,   158,    -1,   160,   161,
     162,   163,   164,   165,   166,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   175,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,  1850,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    49,
      -1,    51,    52,    53,    -1,    55,    56,    57,    58,    59,
      60,    61,    62,    63,    64,    -1,    -1,    -1,    68,    -1,
      70,    71,    72,    73,    -1,    75,    -1,    -1,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    92,    93,    94,    95,    -1,    97,    -1,    99,
     100,   101,   102,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,    -1,   122,   123,    -1,    -1,    -1,    -1,    -1,    -1,
     130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     150,    -1,    -1,   153,   154,    -1,    -1,    -1,   158,    -1,
     160,   161,   162,   163,   164,   165,   166,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   175,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    49,    -1,    51,    52,    53,    -1,    55,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    -1,    -1,
      -1,    68,    -1,    -1,    71,    72,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    99,   100,   101,   102,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
      -1,   118,   119,   120,    -1,   122,   123,    -1,    -1,    -1,
      -1,    -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   144,   145,   146,
      -1,    -1,    -1,   150,   151,   152,   153,   154,    -1,    -1,
      -1,    -1,    -1,   160,   161,   162,   163,   164,   165,   166,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   175,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    49,    -1,    51,    52,    53,
      -1,    55,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    68,    -1,    -1,    71,    72,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    99,   100,   101,   102,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,    -1,   118,   119,   120,    -1,   122,   123,
      -1,    -1,    -1,    -1,    -1,    -1,   130,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     144,   145,   146,    -1,    -1,    -1,   150,   151,    -1,   153,
     154,    -1,    -1,    -1,    -1,    -1,   160,   161,   162,   163,
     164,   165,   166,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   175,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    49,    -1,
      51,    52,    53,    -1,    55,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    -1,    -1,    -1,    68,    -1,    -1,
      71,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    99,   100,
     101,   102,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,    -1,   118,   119,   120,
      -1,   122,   123,    -1,    -1,    -1,    -1,    -1,    -1,   130,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   150,
      -1,   152,   153,   154,    -1,    -1,    -1,    -1,    -1,   160,
     161,   162,   163,   164,   165,   166,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    49,    -1,    51,    52,    53,    -1,    55,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    -1,    -1,
      -1,    68,    -1,    -1,    71,    72,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    99,   100,   101,   102,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
      -1,   118,   119,   120,    -1,   122,   123,    -1,    -1,    -1,
      -1,    -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   150,    -1,    -1,   153,   154,    -1,    -1,
      -1,    -1,    -1,   160,   161,   162,   163,   164,   165,   166,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    49,    -1,    51,    52,    53,
      -1,    55,    56,    57,    58,    59,    60,    61,    62,    63,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    72,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    99,   100,    -1,   102,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,    -1,   118,   119,   120,    -1,   122,   123,
      -1,    -1,    -1,    -1,    -1,    -1,   130,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   150,    -1,    -1,   153,
     154,    -1,    -1,    -1,    -1,    -1,   160,   161,   162,   163,
     164,   165,   166,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    49,    -1,
      51,    52,    53,    -1,    55,    56,    57,    58,    59,    60,
      61,    62,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      71,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    99,   100,
      -1,   102,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,    -1,   118,   119,   120,
      -1,   122,   123,    -1,    -1,    -1,    -1,    -1,    -1,   130,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   150,
      -1,    -1,   153,   154,    -1,    -1,    -1,    -1,    -1,   160,
     161,   162,   163,   164,   165,   166,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    49,    -1,    51,    52,    53,    -1,    55,    56,    57,
      58,    59,    60,    61,    62,    63,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    71,    72,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    99,   100,    -1,   102,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,    -1,
     118,   119,   120,    -1,   122,   123,    -1,    -1,    -1,    -1,
      -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   150,    -1,    -1,   153,   154,    -1,    -1,    -1,
      -1,    -1,   160,   161,   162,   163,   164,   165,   166,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    49,    -1,    51,    52,    53,    -1,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    71,    72,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    99,   100,    -1,   102,    -1,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,    -1,   118,   119,   120,    -1,   122,   123,    -1,
      -1,    -1,    -1,    -1,    -1,   130,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   150,    -1,    -1,   153,   154,
      -1,    -1,    -1,    -1,    -1,   160,   161,   162,   163,   164,
     165,   166,     1,    -1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      -1,    50,    51,    52,    -1,    54,    -1,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    -1,    -1,    -1,    68,
      -1,    -1,    71,    -1,    -1,    -1,    -1,    76,    77,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   101,    -1,    -1,    -1,   105,   106,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     149,    -1,    -1,    -1,   153,   154,    -1,    -1,    -1,    -1,
      -1,    -1,   161,   162,     1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    -1,    50,    51,    52,    -1,    54,    -1,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    -1,    -1,
      -1,    68,    -1,     5,    71,    -1,    -1,    -1,    -1,    76,
      77,    13,    14,    15,    16,    17,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   101,    -1,    -1,    -1,   105,   106,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    49,    -1,    -1,
      -1,    53,    -1,    55,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    71,
      72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   149,    -1,    -1,    -1,   153,   154,    -1,    -1,
      -1,    -1,    -1,    -1,   161,   162,    -1,    99,   100,    -1,
     102,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,    -1,   118,   119,   120,    -1,
     122,   123,    -1,    -1,    -1,    -1,    -1,    -1,   130,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   150,    -1,
      -1,   153,   154,    -1,    -1,    -1,    -1,    -1,   160,   161,
     162,   163,   164,   165,   166,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    52,    -1,    -1,    -1,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    -1,    -1,    -1,
      68,    -1,    -1,    71,    72,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    97,
      -1,    -1,    -1,   101,   102,    -1,    -1,   105,   106,   107,
     108,   109,   110,   111,   112,   113,    -1,    -1,    -1,   117,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   150,   151,    -1,   153,   154,    -1,    -1,    -1,
      -1,    -1,    -1,   161,   162,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      -1,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    -1,    50,    51,    52,    -1,    54,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,    14,    15,
      16,    17,    -1,    71,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,   102,    51,    52,   105,   106,   107,
     108,   109,   110,   111,   112,   113,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    71,    -1,    -1,    -1,    -1,
      -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   150,   151,    -1,   153,   154,    -1,    -1,    -1,
     158,    -1,    -1,   161,   162,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    52,    -1,    -1,    -1,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    -1,    -1,    -1,
      68,    -1,    -1,    71,    -1,    -1,    -1,    -1,    76,    77,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   101,    -1,    -1,    -1,   105,   106,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   149,    -1,    -1,    -1,   153,   154,    -1,    -1,    -1,
      -1,    -1,    -1,   161,   162,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    52,    -1,    -1,    -1,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    -1,    -1,    -1,
      68,    -1,    -1,    71,    -1,    -1,    -1,    -1,    76,    77,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   101,    -1,    -1,    -1,   105,   106,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   149,    -1,    -1,    -1,   153,   154,    -1,    -1,    -1,
      -1,    -1,    -1,   161,   162,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      -1,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    -1,    50,    51,    52,    -1,    54,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   102,    -1,    -1,   105,   106,   107,
     108,   109,   110,   111,   112,   113,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   150,   151,    -1,   153,   154,    -1,     3,    -1,
      -1,    -1,    -1,   161,   162,    10,    -1,    -1,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,    52,    -1,    -1,
      -1,    56,    57,    58,    59,    60,    61,    62,    63,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    71,    72,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,    -1,
     105,   106,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   130,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   149,    -1,    -1,    -1,   153,   154,
      -1,     3,    -1,    -1,    -1,    -1,   161,   162,    10,    -1,
      -1,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,
      52,    -1,    -1,    -1,    56,    57,    58,    59,    60,    61,
      62,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,
      72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,
      -1,    -1,    -1,   105,   106,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   130,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,    -1,    -1,
      -1,   153,   154,    -1,     3,    -1,    -1,    -1,    -1,   161,
     162,    10,    -1,    -1,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    52,    -1,    -1,    -1,    56,    57,    58,
      59,    60,    61,    62,    63,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    71,    72,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   101,    -1,    -1,    -1,   105,   106,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     149,    -1,    -1,    -1,   153,   154,    -1,    -1,    -1,    -1,
      -1,    -1,   161,   162,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    -1,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      -1,    50,    51,    52,    -1,    54,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,    -1,
      -1,    20,    71,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      -1,    50,    51,    52,    -1,    54,   105,   106,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   153,   154,    -1,    -1,    -1,    -1,
      -1,    -1,   161,   162,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    -1,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    -1,
      50,    51,    52,    -1,    54,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   102,    -1,    -1,   105,   106,   107,   108,   109,
     110,   111,   112,   113,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     150,   151,    -1,   153,   154,    -1,    -1,    -1,   158,    -1,
      -1,   161,   162,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    -1,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    -1,    50,
      51,    52,    -1,    54,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   102,    -1,    -1,   105,   106,   107,   108,   109,   110,
     111,   112,   113,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   130,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   150,
     151,    -1,   153,   154,    -1,    -1,    -1,    -1,    -1,    -1,
     161,   162,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,
      52,    -1,    -1,    -1,    56,    57,    58,    59,    60,    61,
      62,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   105,   106,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   130,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     152,   153,   154,    -1,    -1,    -1,    -1,    -1,    -1,   161,
     162,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    52,
      -1,    -1,    -1,    56,    57,    58,    59,    60,    61,    62,
      63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   105,   106,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   130,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,
     153,   154,    -1,    -1,    -1,    -1,    -1,    -1,   161,   162,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    52,    -1,
      -1,    -1,    56,    57,    58,    59,    60,    61,    62,    63,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   105,   106,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   130,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   153,
     154,    -1,    -1,    -1,    -1,    -1,    -1,   161,   162,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,    52,    -1,    -1,
      -1,    56,    57,    58,    59,    60,    61,    62,    63,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     105,   106,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   130,    -1,   132,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   153,   154,
      -1,    -1,    -1,    -1,    -1,    -1,   161,   162,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    52,    -1,    -1,    -1,
      56,    57,    58,    59,    60,    61,    62,    63,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   105,
     106,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   153,   154,    -1,
      -1,    -1,    -1,    -1,    -1,   161,   162,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    52,    -1,    -1,    -1,    56,
      57,    58,    59,    60,    61,    62,    63,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   105,   106,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   153,   154,    -1,    -1,
      -1,    -1,    -1,    -1,   161,   162,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      -1,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    -1,    50,    51,    52,    -1,    54,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   105,   106,    -1,
      -1,    -1,    -1,    18,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    49,    -1,    -1,    -1,    53,    -1,
      55,    -1,    -1,    -1,    -1,   153,   154,    -1,    -1,    -1,
      -1,    -1,    -1,   161,   162,    70,    -1,    72,    -1,    -1,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    -1,    97,    -1,    99,   100,    -1,   102,    -1,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,    -1,   122,   123,    -1,
      -1,    -1,    -1,    -1,    -1,   130,    -1,    -1,    -1,    -1,
      -1,    18,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   150,    -1,    -1,   153,   154,
      -1,    -1,    -1,   158,    -1,   160,   161,   162,   163,   164,
     165,   166,    49,    -1,    -1,    -1,    53,    -1,    55,    -1,
     175,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    70,    -1,    72,    -1,    -1,    75,    -1,
      -1,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    91,    92,    93,    94,    95,    -1,
      97,    -1,    99,   100,    -1,   102,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,    -1,   122,   123,    -1,    -1,    -1,
      -1,    -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   150,    -1,    -1,   153,   154,    -1,    -1,
      -1,   158,    -1,   160,   161,   162,   163,   164,   165,   166,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   175,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    49,    -1,    51,    52,    53,
      -1,    55,    56,    57,    58,    59,    60,    61,    62,    63,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    72,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      84,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    99,   100,    -1,   102,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,    -1,   118,   119,   120,    -1,   122,   123,
      -1,    -1,    -1,    -1,    -1,    -1,   130,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   150,    -1,    -1,   153,
     154,    -1,    -1,    -1,   158,    -1,   160,   161,   162,   163,
     164,   165,   166,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    49,
      -1,    51,    52,    53,    -1,    55,    56,    57,    58,    59,
      60,    61,    62,    63,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    71,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    99,
     100,    -1,   102,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,    -1,   118,   119,
     120,    -1,   122,   123,    -1,    -1,    -1,    -1,    -1,    -1,
     130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     150,    -1,   152,   153,   154,    -1,    -1,    -1,    -1,    -1,
     160,   161,   162,   163,   164,   165,   166,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    49,    -1,    51,    52,    53,    -1,    55,
      56,    57,    58,    59,    60,    61,    62,    63,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    71,    72,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    99,   100,    -1,   102,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,    -1,   118,   119,   120,    -1,   122,   123,    -1,    -1,
      -1,    -1,    -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   150,    -1,    -1,   153,   154,    -1,
      -1,    -1,   158,    -1,   160,   161,   162,   163,   164,   165,
     166,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    49,    -1,    51,
      52,    53,    -1,    55,    56,    57,    58,    59,    60,    61,
      62,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,
      72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    99,   100,    -1,
     102,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,    -1,   118,   119,   120,    -1,
     122,   123,    -1,    -1,    -1,    -1,    -1,    -1,   130,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   150,    -1,
      -1,   153,   154,    -1,    -1,    -1,    -1,    -1,   160,   161,
     162,   163,   164,   165,   166,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    49,    -1,    51,    52,    53,    -1,    55,    56,    57,
      58,    59,    60,    61,    62,    63,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    71,    72,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    99,   100,    -1,   102,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,    -1,
     118,   119,   120,    -1,   122,   123,    -1,    -1,    -1,    -1,
      -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   150,    -1,    -1,   153,   154,    -1,    -1,    -1,
      -1,    -1,   160,   161,   162,   163,   164,   165,   166,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    49,    -1,    51,    52,    53,
      -1,    55,    56,    57,    58,    59,    60,    61,    62,    63,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    72,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    99,   100,    -1,   102,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,    -1,   118,   119,   120,    -1,   122,   123,
      -1,    -1,    -1,    -1,    -1,    -1,   130,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   150,    -1,    -1,   153,
     154,    -1,    -1,    -1,    -1,    -1,   160,   161,   162,   163,
     164,   165,   166,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    52,    -1,    -1,    -1,    56,    57,    58,    59,
      60,    61,    62,    63,    -1,    13,    14,    15,    16,    17,
      18,    71,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    52,   105,   106,    -1,    56,    57,
      58,    59,    60,    61,    62,    63,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   153,   102,    -1,    -1,   105,   106,   107,
     108,   109,   110,   111,   112,   113,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   150,   151,    -1,   153,   154,    -1,    -1,    -1,
     158,    -1,    -1,   161,   162,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    52,    -1,    -1,    -1,    56,    57,
      58,    59,    60,    61,    62,    63,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   102,    -1,    -1,   105,   106,   107,
     108,   109,   110,   111,   112,   113,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   150,   151,    -1,   153,   154,    -1,    -1,    -1,
      -1,    -1,    -1,   161,   162,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    52,    -1,    -1,    -1,    56,    57,
      58,    59,    60,    61,    62,    63,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   102,    -1,    -1,   105,   106,   107,
     108,   109,   110,   111,   112,   113,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   153,   154,    -1,    -1,    -1,
      -1,    -1,    -1,   161,   162,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    52,    -1,    -1,    -1,    56,    57,    58,
      59,    60,    61,    62,    63,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   105,   106,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   152,   153,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    52,    -1,    -1,    -1,    56,    57,    58,
      59,    60,    61,    62,    63,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   105,   106,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    13,    14,    15,    16,
      17,    -1,    -1,    20,   153,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    -1,    50,    51,    52,    -1,    54,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   102,    -1,    -1,   105,   106,
     107,   108,   109,   110,   111,   112,   113,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   150,   151,    -1,   153,   154,    -1,    -1,
      -1,    -1,    -1,    -1,   161,   162,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    52,    -1,    -1,    -1,    56,    57,
      58,    59,    60,    61,    62,    63,    -1,    13,    14,    15,
      16,    17,    18,    71,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    52,   105,   106,    -1,
      56,    57,    58,    59,    60,    61,    62,    63,    -1,    13,
      14,    15,    16,    17,    18,    71,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,   153,    -1,    51,    52,   105,
     106,    -1,    56,    57,    58,    59,    60,    61,    62,    63,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    -1,    -1,
      -1,    -1,    -1,    77,   130,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   152,   153,   154,    -1,
      -1,   105,   106,    -1,    -1,   161,   162,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   130,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   153,
     154,    -1,    -1,    -1,    -1,    -1,    -1,   161,   162,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    52,    -1,
      -1,    -1,    56,    57,    58,    59,    60,    61,    62,    63,
      -1,    13,    14,    15,    16,    17,    18,    71,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,
      52,   105,   106,    -1,    56,    57,    58,    59,    60,    61,
      62,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,
      -1,    -1,    -1,    -1,    -1,    -1,   130,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   153,
     154,    -1,    -1,   105,   106,    -1,    -1,   161,   162,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   130,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   153,   154,    -1,    -1,    -1,    -1,    -1,    -1,   161,
     162,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,
      52,    -1,    -1,    -1,    56,    57,    58,    59,    60,    61,
      62,    63,    -1,    13,    14,    15,    16,    17,    18,    71,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    52,   105,   106,    -1,    56,    57,    58,    59,
      60,    61,    62,    63,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    71,    -1,    -1,    -1,    -1,    -1,    -1,   130,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   153,   154,    -1,    -1,   105,   106,    -1,    -1,   161,
     162,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   153,   154,    -1,    -1,    -1,    -1,    -1,
      -1,   161,   162,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    52,    -1,    -1,    -1,    56,    57,    58,    59,
      60,    61,    62,    63,    -1,    13,    14,    15,    16,    17,
      18,    71,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    52,   105,   106,    -1,    56,    57,
      58,    59,    60,    61,    62,    63,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,    -1,
     130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   153,   154,    -1,    -1,   105,   106,    -1,
      -1,   161,   162,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   153,   154,    -1,    -1,    -1,
      -1,    -1,    -1,   161,   162,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    52,    -1,    -1,    -1,    56,    57,
      58,    59,    60,    61,    62,    63,    -1,    13,    14,    15,
      16,    17,    18,    71,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    52,   105,   106,    -1,
      56,    57,    58,    59,    60,    61,    62,    63,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    71,    -1,    -1,    -1,    -1,
      -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   153,   154,    -1,    -1,   105,
     106,    -1,    -1,   161,   162,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   153,   154,    -1,
      -1,    -1,    -1,    -1,    -1,   161,   162,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    -1,    50,    51,    52,    -1,    54,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,    14,
      15,    16,    17,    18,    71,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,    52,   105,   106,
      -1,    56,    57,    58,    59,    60,    61,    62,    63,    -1,
      13,    14,    15,    16,    17,    18,    71,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,   153,    -1,    51,    52,
     105,   106,    -1,    56,    57,    58,    59,    60,    61,    62,
      63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    -1,
      -1,    -1,    -1,    -1,    -1,   130,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   153,   154,
      -1,    -1,   105,   106,    -1,    -1,   161,   162,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   130,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     153,    -1,    -1,    13,    14,    15,    16,    17,   161,   162,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    -1,
      50,    51,    52,    -1,    54,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    13,    14,    15,    16,    17,    -1,    -1,
      20,    71,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    -1,
      50,    51,    52,    -1,    54,   105,   106,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   153,   154,   105,   106,    -1,    -1,    -1,
      -1,   161,   162,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   153,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   161,   162,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    52,    -1,    -1,    -1,    56,    57,    58,    59,
      60,    61,    62,    63,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   102,    -1,    -1,   105,   106,   107,   108,   109,
     110,   111,   112,   113,    -1,    -1,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,   153,    51,    52,    -1,    -1,    -1,    56,
      57,    58,    59,    60,    61,    62,    63,    -1,    13,    14,
      15,    16,    17,    18,    71,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,    52,   105,   106,
      -1,    56,    57,    58,    59,    60,    61,    62,    63,    49,
      -1,    -1,    -1,    53,    -1,    55,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   153,    -1,    -1,    -1,
     105,   106,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    99,
     100,    -1,   102,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,    -1,   118,   119,
     120,    -1,   122,   123,    -1,    49,    -1,    -1,    -1,    53,
     130,    55,    -1,    -1,    -1,    -1,    -1,    -1,   153,    -1,
      -1,    -1,    -1,    -1,   144,   145,   146,    -1,    72,    -1,
     150,   151,    -1,   153,   154,    -1,    -1,    -1,    -1,    -1,
     160,   161,   162,   163,   164,   165,   166,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    99,   100,    -1,   102,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,    -1,   118,   119,   120,    49,   122,   123,
      -1,    53,    -1,    55,    -1,    -1,   130,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    -1,    -1,    -1,    -1,    -1,   150,   151,    -1,   153,
     154,    -1,    -1,    -1,   158,    -1,   160,   161,   162,   163,
     164,   165,   166,    -1,    -1,    -1,    -1,    99,   100,    -1,
     102,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,    -1,   118,   119,   120,    49,
     122,   123,    -1,    53,    -1,    55,    -1,    -1,   130,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,   150,   151,
      -1,   153,   154,    -1,    -1,    -1,   158,    -1,   160,   161,
     162,   163,   164,   165,   166,    -1,    -1,    -1,    -1,    99,
     100,    -1,   102,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,    -1,   118,   119,
     120,    49,   122,   123,    -1,    53,    -1,    55,    -1,    -1,
     130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,
     150,   151,   152,   153,   154,    -1,    -1,    -1,    -1,    -1,
     160,   161,   162,   163,   164,   165,   166,    -1,    -1,    -1,
      -1,    99,   100,    -1,   102,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,    -1,
     118,   119,   120,    49,   122,   123,    -1,    53,    -1,    55,
      -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    -1,   150,    -1,    -1,   153,   154,    -1,    -1,    -1,
     158,    -1,   160,   161,   162,   163,   164,   165,   166,    -1,
      -1,    -1,    -1,    99,   100,    -1,   102,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,    -1,   118,   119,   120,    49,   122,   123,    -1,    53,
      -1,    55,    -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,   150,   151,    -1,   153,   154,    -1,
      -1,    -1,    -1,    -1,   160,   161,   162,   163,   164,   165,
     166,    -1,    -1,    -1,    -1,    99,   100,    -1,   102,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,    -1,   118,   119,   120,    49,   122,   123,
      -1,    53,    -1,    55,    -1,    -1,   130,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    -1,    -1,    -1,    -1,    -1,   150,    -1,    -1,   153,
     154,    -1,    -1,    -1,   158,    -1,   160,   161,   162,   163,
     164,   165,   166,    -1,    -1,    -1,    -1,    99,   100,    -1,
     102,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,    -1,   118,   119,   120,    49,
     122,   123,    -1,    53,    -1,    55,    -1,    -1,   130,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,   150,   151,
      -1,   153,   154,    -1,    -1,    -1,    -1,    -1,   160,   161,
     162,   163,   164,   165,   166,    -1,    -1,    -1,    -1,    99,
     100,    -1,   102,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,    -1,   118,   119,
     120,    49,   122,   123,    -1,    53,    -1,    55,    -1,    -1,
     130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,
     150,    -1,    -1,   153,   154,    -1,    -1,   157,    -1,    -1,
     160,   161,   162,   163,   164,   165,   166,    -1,    -1,    -1,
      -1,    99,   100,    -1,   102,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,    -1,
     118,   119,   120,    49,   122,   123,    -1,    53,    -1,    55,
      -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    -1,   150,    -1,    -1,   153,   154,    -1,    -1,    -1,
     158,    -1,   160,   161,   162,   163,   164,   165,   166,    -1,
      -1,    -1,    -1,    99,   100,    -1,   102,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,    -1,   118,   119,   120,    49,   122,   123,    -1,    53,
      -1,    55,    -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,   150,   151,    -1,   153,   154,    -1,
      -1,    -1,    -1,    -1,   160,   161,   162,   163,   164,   165,
     166,    -1,    -1,    -1,    -1,    99,   100,    -1,   102,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,    -1,   118,   119,   120,    49,   122,   123,
      -1,    53,    -1,    55,    -1,    -1,   130,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    -1,    -1,    -1,    -1,    -1,   150,    -1,    -1,   153,
     154,    -1,    -1,    -1,   158,    -1,   160,   161,   162,   163,
     164,   165,   166,    -1,    -1,    -1,    -1,    99,   100,    -1,
     102,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,    -1,   118,   119,   120,    49,
     122,   123,    -1,    53,    -1,    55,    -1,    -1,   130,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,   150,    -1,
     152,   153,   154,    -1,    -1,    -1,    -1,    -1,   160,   161,
     162,   163,   164,   165,   166,    -1,    -1,    -1,    -1,    99,
     100,    -1,   102,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,    -1,   118,   119,
     120,    49,   122,   123,    -1,    53,    -1,    55,    -1,    -1,
     130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,
     150,   151,    -1,   153,   154,    -1,    -1,    -1,    -1,    -1,
     160,   161,   162,   163,   164,   165,   166,    -1,    -1,    -1,
      -1,    99,   100,    -1,   102,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,    -1,
     118,   119,   120,    49,   122,   123,    -1,    53,    -1,    55,
      -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    -1,   150,   151,    -1,   153,   154,    -1,    -1,    -1,
      -1,    -1,   160,   161,   162,   163,   164,   165,   166,    -1,
      -1,    -1,    -1,    99,   100,    -1,   102,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,    -1,   118,   119,   120,    49,   122,   123,    -1,    53,
      -1,    55,    -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,   150,   151,    -1,   153,   154,    -1,
      -1,    -1,    -1,    -1,   160,   161,   162,   163,   164,   165,
     166,    -1,    -1,    -1,    -1,    99,   100,    -1,   102,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,    -1,   118,   119,   120,    49,   122,   123,
      -1,    53,    -1,    55,    -1,    -1,   130,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    -1,    -1,    -1,    -1,    -1,   150,   151,    -1,   153,
     154,    -1,    -1,    -1,    -1,    -1,   160,   161,   162,   163,
     164,   165,   166,    -1,    -1,    -1,    -1,    99,   100,    -1,
     102,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,    -1,   118,   119,   120,    49,
     122,   123,    -1,    53,    -1,    55,    -1,    -1,   130,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,   150,   151,
      -1,   153,   154,    -1,    -1,    -1,    -1,    -1,   160,   161,
     162,   163,   164,   165,   166,    -1,    -1,    -1,    -1,    99,
     100,    -1,   102,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,    -1,   118,   119,
     120,    49,   122,   123,    -1,    53,    -1,    55,    -1,    -1,
     130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,
     150,   151,    -1,   153,   154,    -1,    -1,    -1,    -1,    -1,
     160,   161,   162,   163,   164,   165,   166,    -1,    -1,    -1,
      -1,    99,   100,    -1,   102,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,    -1,
     118,   119,   120,    49,   122,   123,    -1,    53,    -1,    55,
      -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    -1,   150,   151,    -1,   153,   154,    -1,    -1,    -1,
      -1,    -1,   160,   161,   162,   163,   164,   165,   166,    -1,
      -1,    -1,    -1,    99,   100,    -1,   102,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,    -1,   118,   119,   120,    49,   122,   123,    -1,    53,
      -1,    55,    -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,   150,   151,    -1,   153,   154,    -1,
      -1,    -1,    -1,    -1,   160,   161,   162,   163,   164,   165,
     166,    -1,    -1,    -1,    -1,    99,   100,    -1,   102,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,    -1,   118,   119,   120,    49,   122,   123,
      -1,    53,    -1,    55,    -1,    -1,   130,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    -1,    -1,    -1,    -1,    -1,   150,   151,    -1,   153,
     154,    -1,    -1,    -1,    -1,    -1,   160,   161,   162,   163,
     164,   165,   166,    -1,    -1,    -1,    -1,    99,   100,    -1,
     102,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,    -1,   118,   119,   120,    49,
     122,   123,    -1,    53,    -1,    55,    -1,    -1,   130,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,   150,   151,
      -1,   153,   154,    -1,    -1,    -1,    -1,    -1,   160,   161,
     162,   163,   164,   165,   166,    -1,    -1,    -1,    -1,    99,
     100,    -1,   102,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,    -1,   118,   119,
     120,    49,   122,   123,    -1,    53,    -1,    55,    -1,    -1,
     130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,
     150,   151,    -1,   153,   154,    -1,    -1,    -1,    -1,    -1,
     160,   161,   162,   163,   164,   165,   166,    -1,    -1,    -1,
      -1,    99,   100,    -1,   102,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,    -1,
     118,   119,   120,    49,   122,   123,    -1,    53,    -1,    55,
      -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    -1,   150,   151,    -1,   153,   154,    -1,    -1,    -1,
      -1,    -1,   160,   161,   162,   163,   164,   165,   166,    -1,
      -1,    -1,    -1,    99,   100,    -1,   102,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,    -1,   118,   119,   120,    49,   122,   123,    -1,    53,
      -1,    55,    -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,   150,   151,    -1,   153,   154,    -1,
      -1,    -1,    -1,    -1,   160,   161,   162,   163,   164,   165,
     166,    -1,    -1,    -1,    -1,    99,   100,    -1,   102,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,    -1,   118,   119,   120,    49,   122,   123,
      -1,    53,    -1,    55,    -1,    -1,   130,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    -1,    -1,    -1,    -1,    -1,   150,   151,    -1,   153,
     154,    -1,    -1,    -1,    -1,    -1,   160,   161,   162,   163,
     164,   165,   166,    -1,    -1,    -1,    -1,    99,   100,    -1,
     102,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,    -1,   118,   119,   120,    49,
     122,   123,    -1,    53,    -1,    55,    -1,    -1,   130,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,   150,   151,
      -1,   153,   154,    -1,    -1,    -1,    -1,    -1,   160,   161,
     162,   163,   164,   165,   166,    -1,    -1,    -1,    -1,    99,
     100,    -1,   102,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,    -1,   118,   119,
     120,    49,   122,   123,    -1,    53,    -1,    55,    -1,    -1,
     130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,
     150,    -1,    -1,   153,   154,    -1,    -1,    -1,    -1,    -1,
     160,   161,   162,   163,   164,   165,   166,    -1,    -1,    -1,
      -1,    99,   100,    -1,   102,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,    -1,
     118,   119,   120,    49,   122,   123,    -1,    53,    -1,    55,
      -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    -1,   150,    -1,    -1,   153,   154,    -1,    -1,    -1,
      -1,    -1,   160,   161,   162,   163,   164,   165,   166,    -1,
      -1,    -1,    -1,    99,   100,    -1,   102,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,    -1,   118,   119,   120,    49,   122,   123,    -1,    53,
      -1,    55,    -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,   150,    -1,    -1,   153,   154,    -1,
      -1,    -1,    -1,    -1,   160,   161,   162,   163,   164,   165,
     166,    -1,    -1,    -1,    -1,    99,   100,    -1,   102,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,    -1,   118,   119,   120,    49,   122,   123,
      -1,    53,    -1,    55,    -1,    -1,   130,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    -1,    -1,    -1,    -1,    -1,   150,    -1,    -1,   153,
     154,    -1,    -1,    -1,    -1,    -1,   160,   161,   162,   163,
     164,   165,   166,    -1,    -1,    -1,    -1,    99,   100,    -1,
     102,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,    -1,   118,   119,   120,    49,
     122,   123,    -1,    53,    -1,    55,    -1,    -1,   130,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,   150,    -1,
      -1,   153,   154,    -1,    -1,    -1,    -1,    -1,   160,   161,
     162,   163,   164,   165,   166,    -1,    -1,    -1,    -1,    99,
     100,    -1,   102,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,    -1,   118,   119,
     120,    49,   122,   123,    -1,    53,    -1,    55,    -1,    -1,
     130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,
     150,    -1,    -1,   153,   154,    -1,    -1,    -1,    -1,    -1,
     160,   161,   162,   163,   164,   165,   166,    -1,    -1,    -1,
      -1,    99,   100,    -1,   102,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,    -1,
     118,   119,   120,    -1,   122,   123,    -1,    -1,    -1,    -1,
      -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   150,    -1,    -1,   153,   154,    -1,    -1,    -1,
      -1,    -1,   160,   161,   162,   163,   164,   165,   166
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_int16 yystos[] =
{
       0,   177,   388,   389,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      20,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    51,    52,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    68,    71,    72,
      97,   101,   102,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   117,   130,   150,   151,   153,   154,   161,   162,
     180,   181,   182,   197,   280,   281,   282,   283,   284,   285,
     286,   287,   288,   289,   290,   291,   293,   295,   297,   298,
     299,   300,   301,   302,   303,   304,   305,   307,   309,   310,
     311,   313,   314,   318,   319,   320,   321,   322,   324,   330,
     331,   332,   333,   344,   347,   380,   383,   393,   399,   401,
     407,   411,   416,   417,   418,   419,   420,   421,   422,   423,
     443,   460,   461,   462,   463,     0,   177,   181,   197,   284,
     286,   295,   298,   310,   314,   319,   116,   150,    57,    60,
      61,    63,   150,   150,   405,   406,   407,   306,   307,   105,
     106,   181,   360,   381,   382,   360,   150,   393,   150,   150,
     150,   197,   406,   411,   417,   418,   419,   421,   422,   423,
     105,   321,   155,   177,   287,   295,   298,   416,   420,   459,
     460,   463,   464,   175,   178,   147,   158,   174,   218,   363,
      88,   156,   400,   360,   178,   178,   178,   175,   105,   106,
     150,   197,   292,   402,   411,   412,   413,   414,   415,   416,
     420,   424,   425,   426,   427,   428,   434,     3,    47,    48,
      50,    54,   312,     3,     4,   154,   197,   286,   299,   303,
     305,   315,   320,   396,   416,   420,   463,   284,   286,   298,
     310,   314,   319,   397,   416,   420,    64,   304,   304,   299,
     305,   304,   299,   304,   299,   153,   405,   156,   178,   150,
     158,   226,   405,   405,   177,   275,   276,   154,   295,   298,
     461,   360,   360,   393,   174,   298,   150,   197,   402,   411,
     416,   425,   154,   197,   463,   394,   395,    64,    65,    66,
      67,   154,   172,   360,   369,   371,   375,   377,   378,   320,
      56,   152,   154,   197,   294,   298,   302,   303,   309,   310,
     316,   317,   318,   319,   323,   330,   331,   347,   356,   358,
     443,   455,   456,   457,   458,   463,   464,   105,   106,   158,
     181,   320,   434,   407,   150,   376,   377,   150,   150,   116,
     183,   184,    49,    53,    55,    72,    99,   100,   102,   104,
     114,   115,   118,   119,   120,   122,   123,   150,   154,   160,
     163,   164,   165,   166,   179,   180,   183,   185,   188,   196,
     197,   198,   199,   202,   203,   204,   205,   206,   207,   208,
     209,   210,   211,   212,   213,   214,   220,   320,   152,   154,
     196,   197,   213,   215,   295,   320,   361,   362,   379,   459,
     464,   298,   417,   418,   419,   421,   422,   423,   152,   152,
     152,   152,   152,   152,   152,   154,   295,   443,   461,   154,
     161,   197,   215,   286,   287,   294,   296,   298,   310,   317,
     319,   351,   352,   355,   356,   357,   455,   463,   150,   416,
     420,   463,   150,   156,   102,   153,   154,   158,   180,   182,
     215,   364,   365,   366,   367,   368,    22,   364,   150,   360,
     226,   150,   156,   156,   156,   406,   411,   413,   414,   415,
     424,   426,   427,   428,   298,   412,   425,   156,    97,   404,
     154,   405,   442,   443,   405,   405,   400,   275,   150,   405,
     442,   400,   405,   405,   298,   402,   150,   150,   297,   298,
     295,   298,   177,   295,   459,   464,   322,   158,   400,   275,
     360,   363,   286,   303,   398,   416,   420,   158,   400,   275,
     381,   298,   310,   298,   298,   105,   321,   105,   106,   181,
     320,   325,   381,   177,   181,   359,   149,   177,     3,   291,
     293,   298,   302,   226,   177,   177,   404,   150,   404,   178,
     215,   406,   411,   298,   150,   177,   360,   391,   158,   360,
     158,   360,   132,   161,   162,   374,   152,   156,   360,   378,
     152,   405,   405,   155,   177,   296,   298,   310,   317,   319,
     454,   455,   463,   464,   150,   154,   162,   174,   197,   443,
     444,   445,   446,   447,   448,   449,   466,   197,   323,   463,
     298,   317,   304,   299,   405,   152,   296,   298,   456,   296,
     443,   456,    10,   348,   360,   345,   158,   369,   174,   369,
      13,    87,   102,   105,   106,   180,   408,   409,   410,   152,
     116,   150,   196,   150,   150,   199,   150,   196,   150,   102,
     298,   311,   150,   196,   196,    19,    21,    84,   154,   163,
     164,   200,   201,   215,   222,   226,   333,   361,   463,   156,
     177,   150,   185,   154,   159,   154,   159,   119,   121,   122,
     123,   150,   153,   154,   158,   159,   199,   199,   167,   161,
     168,   169,   163,   164,   124,   125,   126,   127,   170,   171,
     128,   129,   162,   160,   172,   130,   131,   173,   152,   156,
     153,   177,   133,   134,   135,   136,   137,   138,   139,   140,
     141,   142,   143,   174,   217,   218,   219,   150,   197,   438,
     439,   440,   441,   442,   152,   156,   152,   152,   152,   152,
     152,   152,   150,   405,   442,   443,   150,   442,   443,   177,
     295,   461,   177,   178,   178,   150,   162,   197,   411,   429,
     430,   431,   432,   433,   434,   435,   436,   437,   132,   463,
     178,   178,   360,   360,   177,   177,   177,   154,   182,   177,
     365,   157,   156,   465,   364,   153,   154,   157,   368,   151,
     215,   221,   150,   177,   177,   177,   177,   411,   413,   414,
     415,   424,   426,   427,   428,   152,   152,   152,   152,   152,
     152,   152,   412,   425,   405,   150,   363,   155,   177,   226,
     400,   177,   226,   402,   222,   362,   222,   362,   402,   391,
     226,   400,   404,   158,   400,   275,   391,   226,   400,   327,
     328,   326,   158,   132,   298,   353,   354,   357,   358,   152,
     156,    69,   277,   278,   178,   298,   291,   161,   215,   177,
     411,   352,   393,   391,   155,   177,   150,   373,   371,   372,
      77,   308,   181,   158,   181,   434,   296,   443,   456,   298,
     302,   463,   177,   445,   446,   447,   155,   177,    18,   215,
     298,   444,   466,   405,   405,   443,   296,   454,   464,   298,
     181,   405,   296,   456,   320,   156,   465,   174,   349,   158,
     348,   152,   362,   152,   152,   156,   150,   175,   361,   154,
     361,   361,   361,   215,   361,   152,   361,   361,   361,   177,
     152,   163,   164,   201,    18,   300,   152,   156,   152,   161,
     162,   152,   221,   215,   158,   215,   181,   215,   181,   114,
     154,   181,   151,   189,   190,   191,   215,   114,   154,   181,
     333,   215,   189,   181,   199,   202,   202,   202,   203,   203,
     204,   204,   205,   205,   205,   205,   206,   206,   207,   208,
     209,   210,   211,   157,   222,   175,   183,   154,   181,   215,
     158,   215,   177,   439,   440,   441,   298,   438,   405,   405,
     215,   362,   150,   405,   442,   443,   150,   442,   443,   177,
     177,   155,   155,   150,   411,   430,   431,   432,   435,    18,
     298,   429,   433,   150,   405,   448,   466,   405,   405,   466,
     150,   405,   448,   405,   405,   178,   214,   360,   155,   156,
     155,   156,   466,   466,   132,   350,   351,   352,   350,   360,
     177,   213,   214,   215,   403,   465,   364,   366,   149,   177,
     152,   156,   177,   350,   181,   402,   181,   152,   152,   152,
     152,   152,   152,   150,   405,   442,   443,   150,   405,   442,
     443,   402,   183,   443,   215,   226,   353,   152,   152,   152,
     152,   389,   390,   226,   391,   226,   400,   390,   226,   158,
     158,   158,   334,   178,   178,   181,   279,   360,    18,    70,
      72,    75,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    91,    92,    93,    94,    95,    97,
     105,   106,   117,   177,   222,   223,   224,   225,   226,   227,
     228,   230,   231,   241,   247,   248,   249,   250,   251,   252,
     257,   258,   264,   265,   266,   280,   298,   302,   360,   401,
      69,   175,   178,   178,   178,   350,   178,   392,   390,   284,
     286,   295,   384,   385,   386,   387,   379,   174,   370,   370,
     348,   405,   405,   296,   456,   154,   161,   197,   215,   320,
     215,   298,   353,   152,   152,   152,     5,   298,   405,   444,
     158,   181,   434,    10,   360,   149,   158,   214,   348,   465,
     158,   152,   409,   189,   152,   177,   156,   152,   152,   156,
     152,   199,   152,   152,   152,   199,    18,   300,   215,   152,
     152,   151,   158,   199,   155,   178,   189,   155,   155,   114,
     118,   120,   182,   192,   193,   194,   152,   156,   192,   155,
     156,   149,   213,   157,   152,   192,   178,   365,   353,   152,
     152,   152,   438,   177,   177,   353,   353,   435,   152,   152,
     152,   152,   150,   411,   434,   429,   433,   177,   177,   155,
     178,   466,   177,   177,   178,   178,   178,   178,   363,   192,
     132,   166,   178,   178,   149,   364,   215,   405,   151,   215,
     350,   178,   174,   150,   405,   442,   443,   150,   405,   442,
     443,   177,   177,   404,   152,   178,   178,   392,   390,   226,
     392,   334,   334,   334,     3,    10,    72,   149,   281,   288,
     289,   295,   298,   335,   340,   459,   152,   156,   156,   175,
     150,    60,    61,   175,   226,   280,   401,   150,    18,   224,
     150,   150,   175,   360,   175,   360,   161,   360,   158,   223,
     150,   150,   150,   226,   215,   216,   216,    14,   267,    73,
     232,   175,   178,   228,    77,   175,   360,    90,   253,   359,
     298,   157,   279,   175,   155,   155,   178,   156,   392,   402,
     178,   175,   178,   175,   178,   152,   362,   376,   376,   465,
     158,   158,   177,   178,   178,   178,   215,   178,   150,   405,
     448,   443,   297,     5,   161,   178,   215,   348,   405,   405,
     320,   349,   365,   465,   149,   149,   177,   152,   181,    77,
     186,   187,   361,   199,   199,   199,   199,   199,   158,   365,
     156,   149,   195,   154,   193,   195,   195,   155,   156,   121,
     153,   191,   155,   221,   213,   175,   155,   465,   178,   150,
     405,   442,   443,   353,   353,   178,   178,   152,   150,   405,
     442,   443,   150,   405,   448,   411,   405,   405,   353,   353,
     155,   352,   355,   355,   356,   152,   156,   156,   152,   178,
     214,   214,   155,   155,   178,   178,   152,   215,   177,   177,
     353,   353,   363,   405,   156,   152,   149,   392,   149,   149,
     149,   149,   295,   333,   341,   459,   295,   340,   150,   329,
     175,   175,   150,   157,   197,   336,   337,   343,   411,   412,
     425,   156,   175,   360,   177,   360,   152,   189,   190,   175,
     226,   175,   226,   222,    79,   152,   222,   233,   280,   282,
     285,   291,   298,   302,   144,   145,   146,   151,   152,   175,
     222,   242,   243,   244,   280,   175,   175,   222,   175,   365,
     175,   222,   221,   222,   109,   110,   111,   112,   113,   259,
     261,   262,   175,    96,   175,    83,   150,   150,   178,   149,
     175,   175,   150,   224,   226,   405,   175,   152,   177,   149,
     149,   177,   156,   156,   149,   348,   348,   155,   155,   155,
     178,   152,   177,   215,   215,   178,   155,   178,   465,   346,
     158,   349,   465,   149,   384,   152,   157,   152,   156,   157,
     365,   465,   221,   119,   192,   193,   154,   193,   154,   193,
     155,   149,   152,   177,   178,   178,   152,   152,   177,   177,
     178,   178,   178,   177,   177,   155,   178,   152,   405,   353,
     353,   178,   178,   222,   149,   329,   329,   329,   150,   197,
     338,   339,   442,   450,   451,   452,   453,   175,   156,   175,
     336,   175,   379,   406,   411,   215,   298,   156,   175,   342,
     343,   342,   360,   132,   357,   358,   222,   152,   152,   150,
     224,   152,   222,   298,   144,   145,   146,   166,   175,   245,
     246,   224,   223,   175,   246,   152,   157,   222,   151,   222,
     223,   244,   175,   465,   152,   152,   152,   226,   261,   262,
     150,   215,   150,   183,   233,   199,   254,   108,     1,   224,
     405,   385,   177,   177,   465,   465,   155,   353,   178,   178,
     155,   155,   149,   158,   348,   149,   178,   215,   187,   215,
     465,   149,   155,   155,   192,   192,   353,   152,   152,   353,
     353,   152,   152,   155,   156,   132,   352,   132,   155,   178,
     178,   152,   152,   155,   451,   452,   453,   298,   450,   156,
     175,   405,   405,   175,   152,   411,   405,   175,   224,    76,
      77,   158,   236,   237,   238,   152,   222,    74,   224,   222,
     151,   222,    74,   175,   105,   151,   222,   223,   244,   151,
     222,   224,   243,   246,   246,   175,   222,   149,   158,   238,
     224,   150,   177,   175,   183,   152,   157,   152,   152,   156,
     157,   252,   256,   360,   402,   149,   149,   178,   155,   155,
     348,   465,   149,   149,   155,   155,   178,   178,   178,   177,
     178,   152,   152,   152,   152,   152,   450,   405,   337,     1,
     214,   234,   235,   403,     1,   157,     1,   177,   224,   236,
      74,   175,   152,   224,    74,   175,   166,   166,   224,   223,
     246,   246,   175,   105,   222,   166,   166,    74,   151,   222,
     151,   222,   223,   175,     1,   177,   177,   263,   296,   298,
     459,   157,   175,   154,   183,   268,   269,   270,   224,   199,
     189,    74,   107,   253,   255,   152,   465,   149,   152,   152,
     152,   355,   150,   405,   442,   443,   339,   132,     1,   156,
     157,   149,   273,   274,   280,   224,    74,   175,   224,   222,
     151,   151,   222,   151,   222,   151,   222,   223,   151,   222,
     151,   222,   224,   166,   166,   166,   166,   149,   273,   263,
     178,   150,   197,   402,   450,   181,   157,   102,   150,   152,
     157,   156,    74,   152,   224,   150,   224,   224,   149,   177,
     214,   234,   237,   239,   240,   280,   224,   166,   166,   166,
     166,   151,   151,   222,   151,   222,   151,   222,   239,   178,
     175,   260,   298,   268,   155,   214,   175,   268,   270,   224,
     222,   108,   108,   353,   224,   229,   178,   237,   151,   151,
     222,   151,   222,   151,   222,   178,   260,   213,   152,   157,
     183,   152,   152,   157,   152,   256,    74,   251,   178,     1,
     224,   149,   229,   149,   152,   226,   183,   271,   150,   175,
     271,   224,    74,   152,   226,   156,   157,   214,   152,   224,
     183,   181,   272,   152,   175,   152,   156,   175,   181
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_int16 yyr1[] =
{
       0,   176,   177,   178,   179,   179,   179,   179,   179,   180,
     180,   180,   180,   180,   180,   180,   181,   181,   182,   182,
     183,   184,   184,   185,   185,   185,   185,   185,   185,   185,
     185,   185,   185,   185,   186,   186,   187,   187,   188,   188,
     188,   188,   188,   188,   188,   188,   188,   188,   188,   188,
     188,   188,   188,   188,   188,   188,   188,   188,   188,   188,
     188,   189,   189,   190,   190,   191,   191,   192,   192,   193,
     193,   193,   193,   193,   193,   193,   194,   194,   194,   195,
     195,   196,   196,   196,   196,   196,   196,   196,   196,   196,
     196,   196,   196,   196,   196,   197,   197,   197,   198,   198,
     198,   198,   199,   199,   199,   199,   199,   199,   199,   199,
     199,   200,   200,   200,   200,   201,   201,   202,   202,   203,
     203,   203,   203,   204,   204,   204,   205,   205,   205,   206,
     206,   206,   206,   206,   207,   207,   207,   208,   208,   209,
     209,   210,   210,   211,   211,   212,   212,   213,   213,   213,
     214,   215,   215,   215,   216,   216,   217,   217,   218,   218,
     219,   219,   219,   219,   219,   219,   219,   219,   219,   219,
     219,   220,   220,   221,   221,   221,   221,   222,   222,   223,
     223,   224,   224,   224,   224,   224,   224,   224,   224,   224,
     224,   224,   224,   224,   225,   225,   226,   226,   227,   227,
     228,   228,   228,   228,   228,   229,   229,   229,   230,   230,
     231,   231,   231,   231,   231,   231,   231,   232,   232,   233,
     233,   233,   233,   234,   234,   234,   235,   235,   236,   236,
     236,   236,   236,   237,   237,   238,   239,   239,   240,   240,
     241,   241,   241,   241,   241,   241,   241,   241,   241,   241,
     241,   241,   242,   242,   243,   243,   243,   243,   243,   243,
     243,   243,   243,   243,   243,   243,   243,   243,   243,   243,
     243,   243,   243,   243,   243,   243,   243,   243,   243,   243,
     243,   243,   243,   243,   243,   243,   243,   243,   243,   243,
     243,   243,   243,   243,   243,   243,   243,   243,   243,   244,
     244,   244,   245,   245,   246,   246,   246,   247,   247,   247,
     247,   247,   247,   247,   247,   247,   247,   247,   247,   247,
     247,   247,   247,   247,   247,   247,   247,   248,   248,   249,
     250,   251,   252,   252,   253,   253,   254,   254,   255,   256,
     256,   256,   256,   256,   256,   257,   257,   258,   258,   258,
     259,   259,   260,   260,   261,   261,   261,   261,   262,   263,
     263,   263,   263,   263,   264,   265,   265,   266,   266,   266,
     266,   266,   267,   267,   268,   268,   269,   269,   270,   270,
     271,   271,   271,   272,   272,   273,   273,   274,   274,   275,
     275,   276,   276,   277,   277,   278,   278,   279,   279,   280,
     280,   280,   281,   281,   282,   282,   282,   282,   282,   283,
     283,   283,   284,   284,   284,   285,   285,   285,   285,   285,
     286,   286,   287,   287,   288,   288,   288,   289,   289,   289,
     289,   289,   290,   290,   291,   291,   291,   291,   292,   292,
     293,   293,   293,   294,   294,   294,   295,   295,   295,   296,
     296,   296,   297,   297,   298,   298,   299,   299,   300,   300,
     300,   300,   300,   301,   302,   302,   302,   303,   303,   304,
     304,   304,   304,   304,   304,   304,   304,   304,   305,   305,
     305,   305,   305,   305,   305,   305,   305,   305,   305,   305,
     305,   305,   305,   305,   305,   305,   305,   305,   305,   305,
     305,   305,   305,   305,   305,   305,   306,   306,   307,   308,
     308,   309,   309,   309,   309,   309,   310,   310,   311,   311,
     311,   311,   312,   312,   312,   312,   312,   312,   313,   313,
     313,   313,   314,   315,   314,   314,   316,   316,   316,   316,
     317,   317,   317,   318,   318,   318,   318,   319,   319,   319,
     320,   320,   320,   320,   320,   320,   321,   321,   321,   322,
     322,   323,   323,   325,   324,   326,   324,   327,   324,   328,
     324,   324,   329,   329,   330,   330,   331,   331,   332,   332,
     332,   333,   333,   333,   333,   333,   333,   333,   333,   334,
     334,   335,   335,   335,   335,   335,   335,   335,   335,   335,
     335,   336,   336,   336,   337,   337,   337,   338,   338,   338,
     339,   340,   340,   341,   341,   342,   342,   343,   344,   345,
     344,   344,   344,   344,   346,   344,   344,   344,   344,   344,
     347,   347,   348,   348,   348,   348,   349,   349,   349,   350,
     350,   350,   350,   350,   350,   350,   351,   351,   351,   351,
     352,   352,   353,   353,   353,   353,   354,   354,   354,   354,
     355,   355,   355,   355,   355,   356,   356,   356,   356,   356,
     357,   357,   358,   358,   359,   359,   360,   360,   360,   361,
     361,   361,   362,   362,   363,   363,   363,   363,   364,   364,
     365,   365,   365,   365,   365,   366,   366,   367,   367,   368,
     368,   368,   368,   368,   369,   369,   370,   370,   372,   371,
     373,   371,   371,   371,   374,   374,   374,   374,   375,   375,
     375,   375,   376,   376,   377,   377,   378,   378,   379,   379,
     379,   379,   380,   380,   380,   381,   381,   382,   382,   383,
     383,   384,   384,   385,   385,   386,   386,   386,   387,   387,
     388,   388,   389,   389,   390,   390,   391,   392,   393,   393,
     393,   393,   393,   394,   393,   395,   393,   396,   393,   397,
     393,   398,   393,   399,   399,   399,   400,   400,   401,   401,
     401,   401,   401,   401,   401,   401,   401,   401,   402,   402,
     402,   403,   404,   404,   405,   405,   406,   406,   407,   408,
     408,   409,   409,   409,   410,   410,   410,   410,   410,   410,
     411,   411,   412,   412,   412,   412,   413,   413,   413,   413,
     414,   414,   414,   414,   414,   414,   414,   415,   415,   415,
     415,   416,   416,   416,   417,   417,   417,   417,   417,   418,
     418,   418,   418,   419,   419,   419,   419,   419,   419,   420,
     420,   420,   421,   421,   421,   421,   421,   422,   422,   422,
     422,   423,   423,   423,   423,   423,   423,   424,   424,   425,
     425,   425,   425,   426,   426,   426,   426,   427,   427,   427,
     427,   427,   427,   427,   428,   428,   428,   428,   428,   429,
     429,   429,   429,   429,   430,   430,   430,   431,   431,   431,
     431,   432,   432,   432,   433,   433,   433,   433,   433,   434,
     434,   435,   435,   435,   436,   436,   437,   437,   438,   438,
     438,   439,   439,   439,   439,   439,   440,   440,   440,   440,
     441,   441,   441,   442,   442,   442,   442,   443,   443,   443,
     443,   444,   444,   444,   444,   445,   445,   445,   445,   445,
     446,   446,   446,   446,   447,   447,   447,   448,   448,   448,
     449,   449,   449,   449,   449,   449,   450,   450,   450,   451,
     451,   451,   451,   451,   452,   452,   452,   452,   453,   453,
     454,   454,   454,   455,   455,   456,   456,   456,   456,   456,
     456,   457,   457,   457,   457,   457,   457,   457,   457,   457,
     457,   458,   458,   458,   458,   459,   459,   459,   460,   460,
     461,   461,   461,   461,   461,   461,   462,   462,   462,   462,
     462,   462,   463,   463,   463,   464,   464,   465,   465,   466,
     466
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
       5,     7,     1,     3,     4,     5,     4,     3,     5,     1,
       2,     3,     3,     3,     5,     5,     5,     5,     3,     5,
       5,     5,     3,     4,     5,     5,     5,     5,     7,     7,
       7,     7,     7,     7,     7,     2,     3,     4,     4,     4,
       6,     6,     6,     6,     6,     6,     6,     3,     4,     1,
       1,     1,     1,     1,     1,     1,     1,     3,     4,     2,
       3,     3,     2,     3,     2,     3,     3,     6,     2,     2,
       3,     3,     3,     3,     3,     3,     5,     1,     1,     5,
       5,     4,     0,     1,     4,     6,     1,     3,     4,     3,
       5,     3,     3,     6,     7,     3,     5,     3,     3,     4,
       8,     9,     0,     2,     1,     1,     1,     1,     2,     1,
       2,     2,     2,     1,     3,     1,     1,     6,     8,    10,
      12,    14,     0,     1,     0,     1,     1,     3,     4,     7,
       0,     1,     3,     1,     3,     0,     1,     1,     2,     0,
       1,     4,     5,     0,     1,     3,     4,     1,     3,     2,
       2,     1,     7,     5,     1,     1,     1,     1,     1,     2,
       3,     6,     3,     3,     4,     1,     2,     2,     3,     8,
       8,     8,     5,     9,     2,     2,     5,     3,     5,     4,
       3,     4,     4,     7,     2,     1,     1,     1,     3,     6,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     0,     1,     1,     2,     1,     1,     1,     1,
       1,     1,     1,     4,     1,     2,     3,     1,     2,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     0,     1,     5,     0,
       1,     1,     2,     2,     3,     3,     1,     3,     1,     2,
       2,     2,     4,     4,     4,     4,     1,     1,     1,     2,
       2,     3,     1,     0,     3,     2,     1,     2,     2,     3,
       1,     2,     2,     1,     2,     2,     3,     1,     2,     2,
       1,     2,     3,     1,     2,     3,     1,     3,     4,     1,
       1,     1,     1,     0,     7,     0,     8,     0,     8,     0,
       8,     1,     0,     3,     3,     3,     1,     1,     2,     1,
       1,     1,     2,     1,     2,     1,     2,     1,     2,     0,
       2,     3,     4,     4,     3,     2,     2,     3,     3,     2,
       1,     0,     1,     4,     1,     2,     2,     0,     1,     4,
       1,     2,     3,     1,     2,     0,     1,     2,     6,     0,
       8,     7,     9,     8,     0,    12,    10,    11,    10,     1,
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
#line 569 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.enterScope(); }
#line 7342 "Parser/parser.cc"
    break;

  case 3:
#line 573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 7348 "Parser/parser.cc"
    break;

  case 4:
#line 580 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantInteger( *(yyvsp[0].tok) ) ); }
#line 7354 "Parser/parser.cc"
    break;

  case 5:
#line 581 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7360 "Parser/parser.cc"
    break;

  case 6:
#line 582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7366 "Parser/parser.cc"
    break;

  case 7:
#line 583 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7372 "Parser/parser.cc"
    break;

  case 8:
#line 584 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantChar( *(yyvsp[0].tok) ) ); }
#line 7378 "Parser/parser.cc"
    break;

  case 19:
#line 605 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 7384 "Parser/parser.cc"
    break;

  case 20:
#line 609 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.constant) = build_constantStr( *(yyvsp[0].str) ); }
#line 7390 "Parser/parser.cc"
    break;

  case 21:
#line 613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 7396 "Parser/parser.cc"
    break;

  case 22:
#line 615 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 7406 "Parser/parser.cc"
    break;

  case 23:
#line 626 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 7412 "Parser/parser.cc"
    break;

  case 24:
#line 628 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 7418 "Parser/parser.cc"
    break;

  case 25:
#line 632 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_dimensionref( (yyvsp[0].tok) ) ); }
#line 7424 "Parser/parser.cc"
    break;

  case 27:
#line 635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 7430 "Parser/parser.cc"
    break;

  case 28:
#line 637 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new StmtExpr( dynamic_cast<CompoundStmt *>(maybeMoveBuild<Statement>((yyvsp[-1].sn)) ) ) ); }
#line 7436 "Parser/parser.cc"
    break;

  case 29:
#line 639 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_qualified_expr( (yyvsp[-2].decl), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7442 "Parser/parser.cc"
    break;

  case 30:
#line 641 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7448 "Parser/parser.cc"
    break;

  case 31:
#line 643 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild<Expression>( (yyvsp[-3].en) );
			(yyval.en) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 7458 "Parser/parser.cc"
    break;

  case 32:
#line 653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "Adjacent identifiers are not meaningful in an expression. "
											   "Possible problem is identifier \"", *(yyvsp[-1].tok).str,
											   "\" is a misspelled typename or an incorrectly specified type name, "
											   "e.g., missing generic parameter or missing struct/union/enum before typename." ) );
			(yyval.en) = nullptr;
 		}
#line 7470 "Parser/parser.cc"
    break;

  case 33:
#line 661 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "Identifier \"", *(yyvsp[-1].tok).str, "\" cannot appear before a type. "
											   "Possible problem is misspelled storage or CV qualifier." ) );
			(yyval.en) = nullptr;
		}
#line 7480 "Parser/parser.cc"
    break;

  case 35:
#line 671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// steal the association node from the singleton and delete the wrapper
			(yyvsp[-2].genexpr)->associations.splice((yyvsp[-2].genexpr)->associations.end(), (yyvsp[0].genexpr)->associations);
			delete (yyvsp[0].genexpr);
			(yyval.genexpr) = (yyvsp[-2].genexpr);
		}
#line 7491 "Parser/parser.cc"
    break;

  case 36:
#line 681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuildType((yyvsp[-2].decl)), maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } );
		}
#line 7500 "Parser/parser.cc"
    break;

  case 37:
#line 686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } ); }
#line 7506 "Parser/parser.cc"
    break;

  case 39:
#line 695 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-5].en), new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) ) ) )) ) ); }
#line 7512 "Parser/parser.cc"
    break;

  case 40:
#line 701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7518 "Parser/parser.cc"
    break;

  case 41:
#line 703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7524 "Parser/parser.cc"
    break;

  case 42:
#line 705 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, new ExpressionNode( (yyvsp[-3].constant) ), (yyvsp[-1].en) ) ); }
#line 7530 "Parser/parser.cc"
    break;

  case 43:
#line 707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.en) = new ExpressionNode( new ConstructorExpr( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) ) );
		}
#line 7540 "Parser/parser.cc"
    break;

  case 44:
#line 713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7546 "Parser/parser.cc"
    break;

  case 45:
#line 715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 7552 "Parser/parser.cc"
    break;

  case 46:
#line 717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 7558 "Parser/parser.cc"
    break;

  case 47:
#line 719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), new ExpressionNode( (yyvsp[-2].constant) ) ) ); }
#line 7564 "Parser/parser.cc"
    break;

  case 48:
#line 721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7570 "Parser/parser.cc"
    break;

  case 49:
#line 723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 7576 "Parser/parser.cc"
    break;

  case 50:
#line 725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-1].en), build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) ) ) ); }
#line 7582 "Parser/parser.cc"
    break;

  case 51:
#line 727 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7588 "Parser/parser.cc"
    break;

  case 52:
#line 729 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[0].aggKey), (yyvsp[-2].en) ) ); }
#line 7594 "Parser/parser.cc"
    break;

  case 53:
#line 731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7600 "Parser/parser.cc"
    break;

  case 54:
#line 733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 7606 "Parser/parser.cc"
    break;

  case 55:
#line 735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7612 "Parser/parser.cc"
    break;

  case 56:
#line 737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::IncrPost, (yyvsp[-1].en) ) ); }
#line 7618 "Parser/parser.cc"
    break;

  case 57:
#line 739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::DecrPost, (yyvsp[-1].en) ) ); }
#line 7624 "Parser/parser.cc"
    break;

  case 58:
#line 741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].in), true ) ) ); }
#line 7630 "Parser/parser.cc"
    break;

  case 59:
#line 743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].in), true ))->set_maybeConstructed( false ) ) ); }
#line 7636 "Parser/parser.cc"
    break;

  case 60:
#line 745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) );
		}
#line 7646 "Parser/parser.cc"
    break;

  case 61:
#line 754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7652 "Parser/parser.cc"
    break;

  case 64:
#line 761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7658 "Parser/parser.cc"
    break;

  case 65:
#line 766 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Default parameter for argument is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7664 "Parser/parser.cc"
    break;

  case 68:
#line 773 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7670 "Parser/parser.cc"
    break;

  case 70:
#line 779 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-1].tok) ) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7676 "Parser/parser.cc"
    break;

  case 71:
#line 781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-3].tok) ) ), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7682 "Parser/parser.cc"
    break;

  case 72:
#line 783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7688 "Parser/parser.cc"
    break;

  case 73:
#line 785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7694 "Parser/parser.cc"
    break;

  case 74:
#line 787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7700 "Parser/parser.cc"
    break;

  case 75:
#line 789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7706 "Parser/parser.cc"
    break;

  case 76:
#line 794 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_constantInteger( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 7712 "Parser/parser.cc"
    break;

  case 77:
#line 796 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_field_name_FLOATINGconstant( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 7718 "Parser/parser.cc"
    break;

  case 78:
#line 798 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_varref( (yyvsp[-1].tok) ), (yyvsp[0].en) ) );
		}
#line 7726 "Parser/parser.cc"
    break;

  case 79:
#line 805 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7732 "Parser/parser.cc"
    break;

  case 80:
#line 807 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Expression * constant = build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) );
			(yyval.en) = (yyvsp[-1].en) != nullptr ? new ExpressionNode( build_fieldSel( (yyvsp[-1].en),  constant ) ) : new ExpressionNode( constant );
		}
#line 7741 "Parser/parser.cc"
    break;

  case 83:
#line 819 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 7747 "Parser/parser.cc"
    break;

  case 84:
#line 821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en)->set_extension( true ); }
#line 7753 "Parser/parser.cc"
    break;

  case 85:
#line 826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 7773 "Parser/parser.cc"
    break;

  case 86:
#line 842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_val( (yyvsp[-1].op), (yyvsp[0].en) ) ); }
#line 7779 "Parser/parser.cc"
    break;

  case 87:
#line 844 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Incr, (yyvsp[0].en) ) ); }
#line 7785 "Parser/parser.cc"
    break;

  case 88:
#line 846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Decr, (yyvsp[0].en) ) ); }
#line 7791 "Parser/parser.cc"
    break;

  case 89:
#line 848 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7797 "Parser/parser.cc"
    break;

  case 90:
#line 850 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 7803 "Parser/parser.cc"
    break;

  case 91:
#line 852 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7809 "Parser/parser.cc"
    break;

  case 92:
#line 854 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 7815 "Parser/parser.cc"
    break;

  case 93:
#line 856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_offsetOf( (yyvsp[-3].decl), build_varref( (yyvsp[-1].tok) ) ) ); }
#line 7821 "Parser/parser.cc"
    break;

  case 94:
#line 858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.en) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 7830 "Parser/parser.cc"
    break;

  case 95:
#line 865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::PointTo; }
#line 7836 "Parser/parser.cc"
    break;

  case 96:
#line 866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AddressOf; }
#line 7842 "Parser/parser.cc"
    break;

  case 97:
#line 868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::And; }
#line 7848 "Parser/parser.cc"
    break;

  case 98:
#line 872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::UnPlus; }
#line 7854 "Parser/parser.cc"
    break;

  case 99:
#line 873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::UnMinus; }
#line 7860 "Parser/parser.cc"
    break;

  case 100:
#line 874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::Neg; }
#line 7866 "Parser/parser.cc"
    break;

  case 101:
#line 875 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::BitNeg; }
#line 7872 "Parser/parser.cc"
    break;

  case 103:
#line 881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cast( (yyvsp[-2].decl), (yyvsp[0].en) ) ); }
#line 7878 "Parser/parser.cc"
    break;

  case 104:
#line 883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 7884 "Parser/parser.cc"
    break;

  case 105:
#line 885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 7890 "Parser/parser.cc"
    break;

  case 106:
#line 887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( nullptr ) ) ); }
#line 7896 "Parser/parser.cc"
    break;

  case 107:
#line 889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 7902 "Parser/parser.cc"
    break;

  case 108:
#line 891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Return cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7908 "Parser/parser.cc"
    break;

  case 109:
#line 893 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7914 "Parser/parser.cc"
    break;

  case 110:
#line 895 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7920 "Parser/parser.cc"
    break;

  case 118:
#line 915 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Exp, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7926 "Parser/parser.cc"
    break;

  case 120:
#line 921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mul, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7932 "Parser/parser.cc"
    break;

  case 121:
#line 923 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Div, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7938 "Parser/parser.cc"
    break;

  case 122:
#line 925 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mod, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7944 "Parser/parser.cc"
    break;

  case 124:
#line 931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Plus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7950 "Parser/parser.cc"
    break;

  case 125:
#line 933 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Minus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7956 "Parser/parser.cc"
    break;

  case 127:
#line 939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7962 "Parser/parser.cc"
    break;

  case 128:
#line 941 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::RShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7968 "Parser/parser.cc"
    break;

  case 130:
#line 947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7974 "Parser/parser.cc"
    break;

  case 131:
#line 949 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7980 "Parser/parser.cc"
    break;

  case 132:
#line 951 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7986 "Parser/parser.cc"
    break;

  case 133:
#line 953 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7992 "Parser/parser.cc"
    break;

  case 135:
#line 959 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Eq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7998 "Parser/parser.cc"
    break;

  case 136:
#line 961 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Neq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8004 "Parser/parser.cc"
    break;

  case 138:
#line 967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitAnd, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8010 "Parser/parser.cc"
    break;

  case 140:
#line 973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Xor, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8016 "Parser/parser.cc"
    break;

  case 142:
#line 979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitOr, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8022 "Parser/parser.cc"
    break;

  case 144:
#line 985 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), true ) ); }
#line 8028 "Parser/parser.cc"
    break;

  case 146:
#line 991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), false ) ); }
#line 8034 "Parser/parser.cc"
    break;

  case 148:
#line 997 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8040 "Parser/parser.cc"
    break;

  case 149:
#line 1000 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-3].en), (yyvsp[-3].en), (yyvsp[0].en) ) ); }
#line 8046 "Parser/parser.cc"
    break;

  case 152:
#line 1011 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
//			if ( $2 == OperKinds::AtAssn ) {
//				SemanticError( yylloc, "C @= assignment is currently unimplemented." ); $$ = nullptr;
//			} else {
				(yyval.en) = new ExpressionNode( build_binary_val( (yyvsp[-1].op), (yyvsp[-2].en), (yyvsp[0].en) ) );
//			} // if
		}
#line 8058 "Parser/parser.cc"
    break;

  case 153:
#line 1019 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8064 "Parser/parser.cc"
    break;

  case 154:
#line 1024 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8070 "Parser/parser.cc"
    break;

  case 158:
#line 1034 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::Assign; }
#line 8076 "Parser/parser.cc"
    break;

  case 159:
#line 1035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AtAssn; }
#line 8082 "Parser/parser.cc"
    break;

  case 160:
#line 1039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ExpAssn; }
#line 8088 "Parser/parser.cc"
    break;

  case 161:
#line 1040 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MulAssn; }
#line 8094 "Parser/parser.cc"
    break;

  case 162:
#line 1041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::DivAssn; }
#line 8100 "Parser/parser.cc"
    break;

  case 163:
#line 1042 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ModAssn; }
#line 8106 "Parser/parser.cc"
    break;

  case 164:
#line 1043 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::PlusAssn; }
#line 8112 "Parser/parser.cc"
    break;

  case 165:
#line 1044 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MinusAssn; }
#line 8118 "Parser/parser.cc"
    break;

  case 166:
#line 1045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::LSAssn; }
#line 8124 "Parser/parser.cc"
    break;

  case 167:
#line 1046 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::RSAssn; }
#line 8130 "Parser/parser.cc"
    break;

  case 168:
#line 1047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AndAssn; }
#line 8136 "Parser/parser.cc"
    break;

  case 169:
#line 1048 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ERAssn; }
#line 8142 "Parser/parser.cc"
    break;

  case 170:
#line 1049 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::OrAssn; }
#line 8148 "Parser/parser.cc"
    break;

  case 171:
#line 1060 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)(new ExpressionNode( nullptr ) )->set_last( (yyvsp[-1].en) ) ) ); }
#line 8154 "Parser/parser.cc"
    break;

  case 172:
#line 1062 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-4].en)->set_last( (yyvsp[-1].en) ) ) )); }
#line 8160 "Parser/parser.cc"
    break;

  case 174:
#line 1068 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8166 "Parser/parser.cc"
    break;

  case 175:
#line 1070 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 8172 "Parser/parser.cc"
    break;

  case 176:
#line 1072 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8178 "Parser/parser.cc"
    break;

  case 178:
#line 1078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new CommaExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 8184 "Parser/parser.cc"
    break;

  case 179:
#line 1083 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8190 "Parser/parser.cc"
    break;

  case 191:
#line 1101 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 8196 "Parser/parser.cc"
    break;

  case 193:
#line 1104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_directive( (yyvsp[0].tok) ) ); }
#line 8202 "Parser/parser.cc"
    break;

  case 194:
#line 1110 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[0].sn)->add_label( (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 8208 "Parser/parser.cc"
    break;

  case 195:
#line 1112 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "Label \"", *(yyvsp[-3].tok).str, "\" must be associated with a statement, "
											   "where a declaration, case, or default is not a statement. "
											   "Move the label or terminate with a semi-colon." ) );
			(yyval.sn) = nullptr;
		}
#line 8219 "Parser/parser.cc"
    break;

  case 196:
#line 1122 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (StatementNode *)0 ) ); }
#line 8225 "Parser/parser.cc"
    break;

  case 197:
#line 1127 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (yyvsp[-2].sn) ) ); }
#line 8231 "Parser/parser.cc"
    break;

  case 199:
#line 1133 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 8237 "Parser/parser.cc"
    break;

  case 200:
#line 1138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8243 "Parser/parser.cc"
    break;

  case 201:
#line 1140 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8249 "Parser/parser.cc"
    break;

  case 202:
#line 1142 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8255 "Parser/parser.cc"
    break;

  case 203:
#line 1144 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8261 "Parser/parser.cc"
    break;

  case 206:
#line 1151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 8267 "Parser/parser.cc"
    break;

  case 207:
#line 1153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Declarations only allowed at the start of the switch body, i.e., after the '{'." ); (yyval.sn) = nullptr; }
#line 8273 "Parser/parser.cc"
    break;

  case 208:
#line 1158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_expr( (yyvsp[-1].en) ) ); }
#line 8279 "Parser/parser.cc"
    break;

  case 209:
#line 1160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_mutex( nullptr, new StatementNode( build_expr( (yyvsp[-1].en) ) ) ) ); }
#line 8285 "Parser/parser.cc"
    break;

  case 210:
#line 1167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn); }
#line 8291 "Parser/parser.cc"
    break;

  case 211:
#line 1169 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( true, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8297 "Parser/parser.cc"
    break;

  case 212:
#line 1171 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( true, (yyvsp[-7].en), (yyvsp[-2].sn) ) );
			// The semantics of the declaration list is changed to include associated initialization, which is performed
			// *before* the transfer to the appropriate case clause by hoisting the declarations into a compound
			// statement around the switch.  Statements after the initial declaration list can never be executed, and
			// therefore, are removed from the grammar even though C allows it. The change also applies to choose
			// statement.
			(yyval.sn) = (yyvsp[-3].decl) ? new StatementNode( build_compound( (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 8311 "Parser/parser.cc"
    break;

  case 213:
#line 1181 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Only declarations can appear before the list of case clauses." ); (yyval.sn) = nullptr; }
#line 8317 "Parser/parser.cc"
    break;

  case 214:
#line 1183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( false, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8323 "Parser/parser.cc"
    break;

  case 215:
#line 1185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( false, (yyvsp[-7].en), (yyvsp[-2].sn) ) );
			(yyval.sn) = (yyvsp[-3].decl) ? new StatementNode( build_compound( (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 8332 "Parser/parser.cc"
    break;

  case 216:
#line 1190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Only declarations can appear before the list of case clauses." ); (yyval.sn) = nullptr; }
#line 8338 "Parser/parser.cc"
    break;

  case 217:
#line 1196 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-2].ifctl), maybe_build_compound( (yyvsp[0].sn) ), nullptr ) ); }
#line 8344 "Parser/parser.cc"
    break;

  case 218:
#line 1198 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-4].ifctl), maybe_build_compound( (yyvsp[-2].sn) ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8350 "Parser/parser.cc"
    break;

  case 219:
#line 1203 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( nullptr, (yyvsp[0].en) ); }
#line 8356 "Parser/parser.cc"
    break;

  case 220:
#line 1205 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 8362 "Parser/parser.cc"
    break;

  case 221:
#line 1207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 8368 "Parser/parser.cc"
    break;

  case 222:
#line 1209 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[-1].decl), (yyvsp[0].en) ); }
#line 8374 "Parser/parser.cc"
    break;

  case 223:
#line 1216 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = (yyvsp[0].en); }
#line 8380 "Parser/parser.cc"
    break;

  case 224:
#line 1218 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 8386 "Parser/parser.cc"
    break;

  case 226:
#line 1223 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.sn) = new StatementNode( build_case( (yyvsp[0].en) ) ); }
#line 8392 "Parser/parser.cc"
    break;

  case 227:
#line 1225 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)((yyvsp[-2].sn)->set_last( new StatementNode( build_case( (yyvsp[0].en) ) ) ) ); }
#line 8398 "Parser/parser.cc"
    break;

  case 228:
#line 1230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing case list after case." ); (yyval.sn) = nullptr; }
#line 8404 "Parser/parser.cc"
    break;

  case 229:
#line 1231 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn); }
#line 8410 "Parser/parser.cc"
    break;

  case 230:
#line 1233 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing colon after case list." ); (yyval.sn) = nullptr; }
#line 8416 "Parser/parser.cc"
    break;

  case 231:
#line 1234 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.sn) = new StatementNode( build_default() ); }
#line 8422 "Parser/parser.cc"
    break;

  case 232:
#line 1237 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing colon after default." ); (yyval.sn) = nullptr; }
#line 8428 "Parser/parser.cc"
    break;

  case 234:
#line 1242 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)( (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) )); }
#line 8434 "Parser/parser.cc"
    break;

  case 235:
#line 1246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( maybe_build_compound( (yyvsp[0].sn) ) ); }
#line 8440 "Parser/parser.cc"
    break;

  case 236:
#line 1251 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = nullptr; }
#line 8446 "Parser/parser.cc"
    break;

  case 238:
#line 1257 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ); }
#line 8452 "Parser/parser.cc"
    break;

  case 239:
#line 1259 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)( (yyvsp[-2].sn)->set_last( (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ) ) ); }
#line 8458 "Parser/parser.cc"
    break;

  case 240:
#line 1264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8464 "Parser/parser.cc"
    break;

  case 241:
#line 1266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_while( new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( (yyvsp[-2].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse, "" );
		}
#line 8473 "Parser/parser.cc"
    break;

  case 242:
#line 1271 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( (yyvsp[-2].ifctl), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8479 "Parser/parser.cc"
    break;

  case 243:
#line 1273 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( (yyvsp[-4].ifctl), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[0].sn) ) ); }
#line 8485 "Parser/parser.cc"
    break;

  case 244:
#line 1275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( NEW_ONE, maybe_build_compound( (yyvsp[-4].sn) ) ) ); }
#line 8491 "Parser/parser.cc"
    break;

  case 245:
#line 1277 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_do_while( NEW_ONE, maybe_build_compound( (yyvsp[-5].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse, "" );
		}
#line 8500 "Parser/parser.cc"
    break;

  case 246:
#line 1282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( (yyvsp[-2].en), maybe_build_compound( (yyvsp[-5].sn) ) ) ); }
#line 8506 "Parser/parser.cc"
    break;

  case 247:
#line 1284 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( (yyvsp[-3].en), maybe_build_compound( (yyvsp[-6].sn) ), (yyvsp[0].sn) ) ); }
#line 8512 "Parser/parser.cc"
    break;

  case 248:
#line 1286 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( new ForCtrl( (ExpressionNode * )nullptr, (ExpressionNode * )nullptr, (ExpressionNode * )nullptr ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8518 "Parser/parser.cc"
    break;

  case 249:
#line 1288 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_for( new ForCtrl( (ExpressionNode * )nullptr, (ExpressionNode * )nullptr, (ExpressionNode * )nullptr ), maybe_build_compound( (yyvsp[-2].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse, "" );
		}
#line 8527 "Parser/parser.cc"
    break;

  case 250:
#line 1293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( (yyvsp[-2].fctl), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8533 "Parser/parser.cc"
    break;

  case 251:
#line 1295 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( (yyvsp[-4].fctl), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[0].sn) ) ); }
#line 8539 "Parser/parser.cc"
    break;

  case 253:
#line 1305 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 8558 "Parser/parser.cc"
    break;

  case 254:
#line 1323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (ExpressionNode * )nullptr, (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8564 "Parser/parser.cc"
    break;

  case 255:
#line 1325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8570 "Parser/parser.cc"
    break;

  case 256:
#line 1327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-3].decl), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8576 "Parser/parser.cc"
    break;

  case 257:
#line 1330 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (ExpressionNode *)nullptr, (yyvsp[0].en), nullptr ); }
#line 8582 "Parser/parser.cc"
    break;

  case 258:
#line 1332 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (ExpressionNode *)nullptr, (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8588 "Parser/parser.cc"
    break;

  case 259:
#line 1335 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), NEW_ZERO, OperKinds::LThan, (yyvsp[0].en)->clone(), NEW_ONE ); }
#line 8594 "Parser/parser.cc"
    break;

  case 260:
#line 1337 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].compop), NEW_ZERO, (yyvsp[0].en)->clone() ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 8600 "Parser/parser.cc"
    break;

  case 261:
#line 1340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].compop), (yyvsp[-2].en)->clone(), (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), (yyvsp[-2].en)->clone() ), NEW_ONE ); }
#line 8606 "Parser/parser.cc"
    break;

  case 262:
#line 1342 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[0].en)->clone(), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 8615 "Parser/parser.cc"
    break;

  case 263:
#line 1347 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
		}
#line 8624 "Parser/parser.cc"
    break;

  case 264:
#line 1352 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en)->clone(), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), (yyvsp[0].en) ); }
#line 8630 "Parser/parser.cc"
    break;

  case 265:
#line 1354 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-2].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].en)->clone(), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 8639 "Parser/parser.cc"
    break;

  case 266:
#line 1359 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
		}
#line 8648 "Parser/parser.cc"
    break;

  case 267:
#line 1364 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 8654 "Parser/parser.cc"
    break;

  case 268:
#line 1366 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 8660 "Parser/parser.cc"
    break;

  case 269:
#line 1368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 8666 "Parser/parser.cc"
    break;

  case 270:
#line 1370 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 8672 "Parser/parser.cc"
    break;

  case 271:
#line 1372 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 8678 "Parser/parser.cc"
    break;

  case 272:
#line 1375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-2].en), NEW_ZERO, OperKinds::LThan, (yyvsp[0].en)->clone(), NEW_ONE ); }
#line 8684 "Parser/parser.cc"
    break;

  case 273:
#line 1377 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-3].en), UPDOWN( (yyvsp[-1].compop), NEW_ZERO, (yyvsp[0].en)->clone() ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 8690 "Parser/parser.cc"
    break;

  case 274:
#line 1380 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), UPDOWN( (yyvsp[-1].compop), (yyvsp[-2].en)->clone(), (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), (yyvsp[-2].en)->clone() ), NEW_ONE ); }
#line 8696 "Parser/parser.cc"
    break;

  case 275:
#line 1382 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-4].en), (yyvsp[0].en)->clone(), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 8705 "Parser/parser.cc"
    break;

  case 276:
#line 1387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::GThan || (yyvsp[-1].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 8715 "Parser/parser.cc"
    break;

  case 277:
#line 1393 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing low/high value for up/down-to range so index is uninitialized." ); (yyval.fctl) = nullptr; }
#line 8721 "Parser/parser.cc"
    break;

  case 278:
#line 1396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en)->clone(), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), (yyvsp[0].en) ); }
#line 8727 "Parser/parser.cc"
    break;

  case 279:
#line 1398 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-6].en), (yyvsp[-2].en)->clone(), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 8736 "Parser/parser.cc"
    break;

  case 280:
#line 1403 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 8746 "Parser/parser.cc"
    break;

  case 281:
#line 1409 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en)->clone(), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), nullptr ); }
#line 8752 "Parser/parser.cc"
    break;

  case 282:
#line 1411 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-6].en), (yyvsp[-2].en)->clone(), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 8761 "Parser/parser.cc"
    break;

  case 283:
#line 1416 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 8771 "Parser/parser.cc"
    break;

  case 284:
#line 1422 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing low/high value for up/down-to range so index is uninitialized." ); (yyval.fctl) = nullptr; }
#line 8777 "Parser/parser.cc"
    break;

  case 285:
#line 1425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-1].decl), NEW_ZERO, OperKinds::LThan, (yyvsp[0].en), NEW_ONE ); }
#line 8783 "Parser/parser.cc"
    break;

  case 286:
#line 1427 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].decl), UPDOWN( (yyvsp[-1].compop), NEW_ZERO, (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 8789 "Parser/parser.cc"
    break;

  case 287:
#line 1430 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-3].decl), UPDOWN( (yyvsp[-1].compop), (yyvsp[-2].en)->clone(), (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), (yyvsp[-2].en)->clone() ), NEW_ONE ); }
#line 8795 "Parser/parser.cc"
    break;

  case 288:
#line 1432 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-3].decl), (yyvsp[0].en), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 8804 "Parser/parser.cc"
    break;

  case 289:
#line 1437 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::GThan || (yyvsp[-1].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-3].decl), (yyvsp[-2].en), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 8814 "Parser/parser.cc"
    break;

  case 290:
#line 1444 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), (yyvsp[0].en) ); }
#line 8820 "Parser/parser.cc"
    break;

  case 291:
#line 1446 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-2].en), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 8829 "Parser/parser.cc"
    break;

  case 292:
#line 1451 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-4].en), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 8839 "Parser/parser.cc"
    break;

  case 293:
#line 1457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), nullptr ); }
#line 8845 "Parser/parser.cc"
    break;

  case 294:
#line 1459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-2].en), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 8854 "Parser/parser.cc"
    break;

  case 295:
#line 1464 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-4].en), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 8864 "Parser/parser.cc"
    break;

  case 296:
#line 1470 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing low/high value for up/down-to range so index is uninitialized." ); (yyval.fctl) = nullptr; }
#line 8870 "Parser/parser.cc"
    break;

  case 297:
#line 1473 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.fctl) = nullptr;
			//$$ = forCtrl( new ExpressionNode( build_varref( $3 ) ), $1, nullptr, OperKinds::Range, nullptr, nullptr );
		}
#line 8879 "Parser/parser.cc"
    break;

  case 298:
#line 1478 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LEThan || (yyvsp[-1].compop) == OperKinds::GEThan ) { SemanticError( yylloc, "All enumation ranges are equal (all values). Remove \"=~\"." ); (yyval.fctl) = nullptr; }
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.fctl) = nullptr;
		}
#line 8888 "Parser/parser.cc"
    break;

  case 299:
#line 1486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GThan; }
#line 8894 "Parser/parser.cc"
    break;

  case 300:
#line 1488 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 8900 "Parser/parser.cc"
    break;

  case 301:
#line 1490 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GEThan; }
#line 8906 "Parser/parser.cc"
    break;

  case 302:
#line 1495 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LThan; }
#line 8912 "Parser/parser.cc"
    break;

  case 303:
#line 1497 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GThan; }
#line 8918 "Parser/parser.cc"
    break;

  case 305:
#line 1503 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 8924 "Parser/parser.cc"
    break;

  case 306:
#line 1505 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GEThan; }
#line 8930 "Parser/parser.cc"
    break;

  case 307:
#line 1510 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Goto ) ); }
#line 8936 "Parser/parser.cc"
    break;

  case 308:
#line 1514 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_computedgoto( (yyvsp[-1].en) ) ); }
#line 8942 "Parser/parser.cc"
    break;

  case 309:
#line 1517 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThrough ) ); }
#line 8948 "Parser/parser.cc"
    break;

  case 310:
#line 1519 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::FallThrough ) ); }
#line 8954 "Parser/parser.cc"
    break;

  case 311:
#line 1521 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThroughDefault ) ); }
#line 8960 "Parser/parser.cc"
    break;

  case 312:
#line 1524 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Continue ) ); }
#line 8966 "Parser/parser.cc"
    break;

  case 313:
#line 1528 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Continue ) ); }
#line 8972 "Parser/parser.cc"
    break;

  case 314:
#line 1531 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Break ) ); }
#line 8978 "Parser/parser.cc"
    break;

  case 315:
#line 1535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Break ) ); }
#line 8984 "Parser/parser.cc"
    break;

  case 316:
#line 1537 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_return( (yyvsp[-1].en) ) ); }
#line 8990 "Parser/parser.cc"
    break;

  case 317:
#line 1539 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 8996 "Parser/parser.cc"
    break;

  case 318:
#line 1541 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr ) ); }
#line 9002 "Parser/parser.cc"
    break;

  case 319:
#line 1543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn) ) ); }
#line 9008 "Parser/parser.cc"
    break;

  case 320:
#line 1545 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Coroutine ) ); }
#line 9014 "Parser/parser.cc"
    break;

  case 321:
#line 1547 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Coroutine ) ); }
#line 9020 "Parser/parser.cc"
    break;

  case 322:
#line 1549 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Generator ) ); }
#line 9026 "Parser/parser.cc"
    break;

  case 323:
#line 1551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Generator ) ); }
#line 9032 "Parser/parser.cc"
    break;

  case 324:
#line 1553 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_throw( (yyvsp[-1].en) ) ); }
#line 9038 "Parser/parser.cc"
    break;

  case 325:
#line 1555 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume( (yyvsp[-1].en) ) ); }
#line 9044 "Parser/parser.cc"
    break;

  case 326:
#line 1557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume_at( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 9050 "Parser/parser.cc"
    break;

  case 329:
#line 1567 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_with( (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 9056 "Parser/parser.cc"
    break;

  case 330:
#line 1573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_mutex( (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 9062 "Parser/parser.cc"
    break;

  case 331:
#line 1577 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 9068 "Parser/parser.cc"
    break;

  case 332:
#line 1582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9074 "Parser/parser.cc"
    break;

  case 334:
#line 1588 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 9080 "Parser/parser.cc"
    break;

  case 335:
#line 1592 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) )); }
#line 9086 "Parser/parser.cc"
    break;

  case 337:
#line 1599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.en) = nullptr; }
#line 9092 "Parser/parser.cc"
    break;

  case 338:
#line 1603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 9098 "Parser/parser.cc"
    break;

  case 339:
#line 1608 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 9104 "Parser/parser.cc"
    break;

  case 340:
#line 1610 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-3].en), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[-4].en), (yyvsp[0].wfs) ); }
#line 9110 "Parser/parser.cc"
    break;

  case 341:
#line 1612 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 9116 "Parser/parser.cc"
    break;

  case 342:
#line 1614 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( nullptr, maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 9122 "Parser/parser.cc"
    break;

  case 343:
#line 1617 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 9128 "Parser/parser.cc"
    break;

  case 344:
#line 1619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-5].en), maybe_build_compound( (yyvsp[-4].sn) ), (yyvsp[-6].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 9134 "Parser/parser.cc"
    break;

  case 345:
#line 1624 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-1].en), (yyvsp[0].sn), (yyvsp[-2].en) ) ); }
#line 9140 "Parser/parser.cc"
    break;

  case 346:
#line 1626 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-3].en), (yyvsp[-2].sn), (yyvsp[-4].en), (yyvsp[0].wfs) ) ); }
#line 9146 "Parser/parser.cc"
    break;

  case 347:
#line 1631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), (yyvsp[0].sn), 0 ) ); }
#line 9152 "Parser/parser.cc"
    break;

  case 348:
#line 1633 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), 0, (yyvsp[0].sn) ) ); }
#line 9158 "Parser/parser.cc"
    break;

  case 349:
#line 1635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-2].sn), (yyvsp[-1].sn), (yyvsp[0].sn) ) ); }
#line 9164 "Parser/parser.cc"
    break;

  case 350:
#line 1640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 9170 "Parser/parser.cc"
    break;

  case 351:
#line 1642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)(yyvsp[-8].sn)->set_last( new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ) ); }
#line 9176 "Parser/parser.cc"
    break;

  case 352:
#line 1647 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9182 "Parser/parser.cc"
    break;

  case 353:
#line 1648 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[0].en); }
#line 9188 "Parser/parser.cc"
    break;

  case 354:
#line 1652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 9194 "Parser/parser.cc"
    break;

  case 355:
#line 1653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 9200 "Parser/parser.cc"
    break;

  case 356:
#line 1654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.catch_kind) = CatchStmt::Resume; }
#line 9206 "Parser/parser.cc"
    break;

  case 357:
#line 1655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Resume; }
#line 9212 "Parser/parser.cc"
    break;

  case 358:
#line 1659 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = new StatementNode( build_finally( (yyvsp[0].sn) ) ); }
#line 9218 "Parser/parser.cc"
    break;

  case 360:
#line 1666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 9224 "Parser/parser.cc"
    break;

  case 361:
#line 1668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 9230 "Parser/parser.cc"
    break;

  case 362:
#line 1670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 9236 "Parser/parser.cc"
    break;

  case 367:
#line 1685 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-4].flag), (yyvsp[-2].constant), 0 ) ); }
#line 9242 "Parser/parser.cc"
    break;

  case 368:
#line 1687 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-6].flag), (yyvsp[-4].constant), (yyvsp[-2].en) ) ); }
#line 9248 "Parser/parser.cc"
    break;

  case 369:
#line 1689 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-8].flag), (yyvsp[-6].constant), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 9254 "Parser/parser.cc"
    break;

  case 370:
#line 1691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-10].flag), (yyvsp[-8].constant), (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 9260 "Parser/parser.cc"
    break;

  case 371:
#line 1693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-12].flag), (yyvsp[-9].constant), 0, (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].label) ) ); }
#line 9266 "Parser/parser.cc"
    break;

  case 372:
#line 1698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = false; }
#line 9272 "Parser/parser.cc"
    break;

  case 373:
#line 1700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = true; }
#line 9278 "Parser/parser.cc"
    break;

  case 374:
#line 1705 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9284 "Parser/parser.cc"
    break;

  case 377:
#line 1712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 9290 "Parser/parser.cc"
    break;

  case 378:
#line 1717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( nullptr, (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 9296 "Parser/parser.cc"
    break;

  case 379:
#line 1719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( (yyvsp[-5].tok), (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 9302 "Parser/parser.cc"
    break;

  case 380:
#line 1724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9308 "Parser/parser.cc"
    break;

  case 381:
#line 1726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 9314 "Parser/parser.cc"
    break;

  case 382:
#line 1728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( (yyvsp[0].constant) ) )); }
#line 9320 "Parser/parser.cc"
    break;

  case 383:
#line 1733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = new LabelNode(); (yyval.label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 9329 "Parser/parser.cc"
    break;

  case 384:
#line 1738 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = (yyvsp[-2].label); (yyvsp[-2].label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 9338 "Parser/parser.cc"
    break;

  case 385:
#line 1748 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9344 "Parser/parser.cc"
    break;

  case 388:
#line 1755 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 9350 "Parser/parser.cc"
    break;

  case 389:
#line 1760 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9356 "Parser/parser.cc"
    break;

  case 391:
#line 1766 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 9362 "Parser/parser.cc"
    break;

  case 392:
#line 1768 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-2].decl) ); }
#line 9368 "Parser/parser.cc"
    break;

  case 399:
#line 1788 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "C_DECLARATION1 %p %s\n", $$, $$->name ? $$->name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 9379 "Parser/parser.cc"
    break;

  case 402:
#line 1800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-4].en), (yyvsp[-2].constant) ); }
#line 9385 "Parser/parser.cc"
    break;

  case 403:
#line 1802 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-2].en), build_constantStr( *new string( "\"\"" ) ) ); }
#line 9391 "Parser/parser.cc"
    break;

  case 407:
#line 1820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9397 "Parser/parser.cc"
    break;

  case 409:
#line 1826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].in) ); }
#line 9403 "Parser/parser.cc"
    break;

  case 410:
#line 1830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 9409 "Parser/parser.cc"
    break;

  case 411:
#line 1832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].in) ) ); }
#line 9415 "Parser/parser.cc"
    break;

  case 412:
#line 1839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 9421 "Parser/parser.cc"
    break;

  case 413:
#line 1841 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 9427 "Parser/parser.cc"
    break;

  case 414:
#line 1843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 9433 "Parser/parser.cc"
    break;

  case 416:
#line 1849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9439 "Parser/parser.cc"
    break;

  case 417:
#line 1851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9445 "Parser/parser.cc"
    break;

  case 418:
#line 1853 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 9451 "Parser/parser.cc"
    break;

  case 419:
#line 1855 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeClone( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->appendList( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 9462 "Parser/parser.cc"
    break;

  case 420:
#line 1888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), 0 )->addQualifiers( (yyvsp[0].decl) ); }
#line 9468 "Parser/parser.cc"
    break;

  case 421:
#line 1890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), 0 )->addQualifiers( (yyvsp[0].decl) ); }
#line 9474 "Parser/parser.cc"
    break;

  case 422:
#line 1895 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 9480 "Parser/parser.cc"
    break;

  case 423:
#line 1898 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-6].decl)->appendList( (yyvsp[-2].decl) ) ); }
#line 9486 "Parser/parser.cc"
    break;

  case 424:
#line 1903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 9495 "Parser/parser.cc"
    break;

  case 425:
#line 1908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 9504 "Parser/parser.cc"
    break;

  case 426:
#line 1913 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "3" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 9513 "Parser/parser.cc"
    break;

  case 427:
#line 1924 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "4" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef();
		}
#line 9522 "Parser/parser.cc"
    break;

  case 428:
#line 1929 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "5" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 9531 "Parser/parser.cc"
    break;

  case 429:
#line 1934 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "6" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[-3].decl) )->addTypedef();
		}
#line 9540 "Parser/parser.cc"
    break;

  case 430:
#line 1939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "7" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-2].decl) )->addTypedef();
		}
#line 9549 "Parser/parser.cc"
    break;

  case 431:
#line 1944 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "8" );
			(yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-3].decl) )->addTypedef()->addType( (yyvsp[-3].decl) );
		}
#line 9558 "Parser/parser.cc"
    break;

  case 432:
#line 1953 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 9566 "Parser/parser.cc"
    break;

  case 433:
#line 1957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 9574 "Parser/parser.cc"
    break;

  case 434:
#line 1964 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 9580 "Parser/parser.cc"
    break;

  case 438:
#line 1974 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 9586 "Parser/parser.cc"
    break;

  case 439:
#line 1976 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ) ); }
#line 9592 "Parser/parser.cc"
    break;

  case 447:
#line 1999 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_type_specifier2 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 9603 "Parser/parser.cc"
    break;

  case 452:
#line 2021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9609 "Parser/parser.cc"
    break;

  case 455:
#line 2033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9615 "Parser/parser.cc"
    break;

  case 458:
#line 2043 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Const ); }
#line 9621 "Parser/parser.cc"
    break;

  case 459:
#line 2045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Restrict ); }
#line 9627 "Parser/parser.cc"
    break;

  case 460:
#line 2047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Volatile ); }
#line 9633 "Parser/parser.cc"
    break;

  case 461:
#line 2049 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Atomic ); }
#line 9639 "Parser/parser.cc"
    break;

  case 463:
#line 2055 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newForall( (yyvsp[-1].decl) ); }
#line 9645 "Parser/parser.cc"
    break;

  case 465:
#line 2061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9651 "Parser/parser.cc"
    break;

  case 466:
#line 2063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9657 "Parser/parser.cc"
    break;

  case 468:
#line 2074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9663 "Parser/parser.cc"
    break;

  case 469:
#line 2079 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Extern ); }
#line 9669 "Parser/parser.cc"
    break;

  case 470:
#line 2081 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Static ); }
#line 9675 "Parser/parser.cc"
    break;

  case 471:
#line 2083 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Auto ); }
#line 9681 "Parser/parser.cc"
    break;

  case 472:
#line 2085 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Register ); }
#line 9687 "Parser/parser.cc"
    break;

  case 473:
#line 2087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::ThreadlocalGcc ); }
#line 9693 "Parser/parser.cc"
    break;

  case 474:
#line 2089 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::ThreadlocalC11 ); }
#line 9699 "Parser/parser.cc"
    break;

  case 475:
#line 2092 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Inline ); }
#line 9705 "Parser/parser.cc"
    break;

  case 476:
#line 2094 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Fortran ); }
#line 9711 "Parser/parser.cc"
    break;

  case 477:
#line 2096 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Noreturn ); }
#line 9717 "Parser/parser.cc"
    break;

  case 478:
#line 2101 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 9723 "Parser/parser.cc"
    break;

  case 479:
#line 2103 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Bool ); }
#line 9729 "Parser/parser.cc"
    break;

  case 480:
#line 2105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Char ); }
#line 9735 "Parser/parser.cc"
    break;

  case 481:
#line 2107 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int ); }
#line 9741 "Parser/parser.cc"
    break;

  case 482:
#line 2109 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 ); }
#line 9747 "Parser/parser.cc"
    break;

  case 483:
#line 2111 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 )->addType( DeclarationNode::newSignedNess( DeclarationNode::Unsigned ) ); }
#line 9753 "Parser/parser.cc"
    break;

  case 484:
#line 2113 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Float ); }
#line 9759 "Parser/parser.cc"
    break;

  case 485:
#line 2115 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Double ); }
#line 9765 "Parser/parser.cc"
    break;

  case 486:
#line 2117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat80 ); }
#line 9771 "Parser/parser.cc"
    break;

  case 487:
#line 2119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat128 ); }
#line 9777 "Parser/parser.cc"
    break;

  case 488:
#line 2121 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat16 ); }
#line 9783 "Parser/parser.cc"
    break;

  case 489:
#line 2123 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32 ); }
#line 9789 "Parser/parser.cc"
    break;

  case 490:
#line 2125 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32x ); }
#line 9795 "Parser/parser.cc"
    break;

  case 491:
#line 2127 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64 ); }
#line 9801 "Parser/parser.cc"
    break;

  case 492:
#line 2129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64x ); }
#line 9807 "Parser/parser.cc"
    break;

  case 493:
#line 2131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat128 ); }
#line 9813 "Parser/parser.cc"
    break;

  case 494:
#line 2133 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9819 "Parser/parser.cc"
    break;

  case 495:
#line 2135 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9825 "Parser/parser.cc"
    break;

  case 496:
#line 2137 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9831 "Parser/parser.cc"
    break;

  case 497:
#line 2139 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Complex ); }
#line 9837 "Parser/parser.cc"
    break;

  case 498:
#line 2141 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Imaginary ); }
#line 9843 "Parser/parser.cc"
    break;

  case 499:
#line 2143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Signed ); }
#line 9849 "Parser/parser.cc"
    break;

  case 500:
#line 2145 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Unsigned ); }
#line 9855 "Parser/parser.cc"
    break;

  case 501:
#line 2147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Short ); }
#line 9861 "Parser/parser.cc"
    break;

  case 502:
#line 2149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Long ); }
#line 9867 "Parser/parser.cc"
    break;

  case 503:
#line 2151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Valist ); }
#line 9873 "Parser/parser.cc"
    break;

  case 504:
#line 2153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::AutoType ); }
#line 9879 "Parser/parser.cc"
    break;

  case 506:
#line 2159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9885 "Parser/parser.cc"
    break;

  case 508:
#line 2165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVtableType( (yyvsp[-2].decl) ); }
#line 9891 "Parser/parser.cc"
    break;

  case 509:
#line 2171 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9897 "Parser/parser.cc"
    break;

  case 510:
#line 2173 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9903 "Parser/parser.cc"
    break;

  case 512:
#line 2180 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9909 "Parser/parser.cc"
    break;

  case 513:
#line 2182 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9915 "Parser/parser.cc"
    break;

  case 514:
#line 2184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9921 "Parser/parser.cc"
    break;

  case 515:
#line 2186 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 9927 "Parser/parser.cc"
    break;

  case 517:
#line 2193 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9933 "Parser/parser.cc"
    break;

  case 519:
#line 2199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9939 "Parser/parser.cc"
    break;

  case 520:
#line 2201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9945 "Parser/parser.cc"
    break;

  case 521:
#line 2203 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 9951 "Parser/parser.cc"
    break;

  case 522:
#line 2208 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 9957 "Parser/parser.cc"
    break;

  case 523:
#line 2210 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en) ); }
#line 9963 "Parser/parser.cc"
    break;

  case 524:
#line 2212 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 9969 "Parser/parser.cc"
    break;

  case 525:
#line 2214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en), true ); }
#line 9975 "Parser/parser.cc"
    break;

  case 526:
#line 2216 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Zero ); }
#line 9981 "Parser/parser.cc"
    break;

  case 527:
#line 2218 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::One ); }
#line 9987 "Parser/parser.cc"
    break;

  case 528:
#line 2223 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_declaration_specifier %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 9998 "Parser/parser.cc"
    break;

  case 529:
#line 2230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10004 "Parser/parser.cc"
    break;

  case 530:
#line 2232 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10010 "Parser/parser.cc"
    break;

  case 531:
#line 2234 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10016 "Parser/parser.cc"
    break;

  case 532:
#line 2239 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_type_specifier %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10027 "Parser/parser.cc"
    break;

  case 533:
#line 2246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 10033 "Parser/parser.cc"
    break;

  case 534:
#line 2248 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10039 "Parser/parser.cc"
    break;

  case 535:
#line 2250 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 10048 "Parser/parser.cc"
    break;

  case 537:
#line 2259 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10054 "Parser/parser.cc"
    break;

  case 538:
#line 2261 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10060 "Parser/parser.cc"
    break;

  case 539:
#line 2263 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10066 "Parser/parser.cc"
    break;

  case 541:
#line 2269 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10072 "Parser/parser.cc"
    break;

  case 542:
#line 2271 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10078 "Parser/parser.cc"
    break;

  case 544:
#line 2277 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10084 "Parser/parser.cc"
    break;

  case 545:
#line 2279 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10090 "Parser/parser.cc"
    break;

  case 546:
#line 2281 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10096 "Parser/parser.cc"
    break;

  case 548:
#line 2287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10102 "Parser/parser.cc"
    break;

  case 549:
#line 2289 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10108 "Parser/parser.cc"
    break;

  case 550:
#line 2294 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypedef( (yyvsp[0].tok) ); }
#line 10114 "Parser/parser.cc"
    break;

  case 551:
#line 2296 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 10120 "Parser/parser.cc"
    break;

  case 552:
#line 2298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 10126 "Parser/parser.cc"
    break;

  case 554:
#line 2301 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), (yyvsp[0].decl) ); }
#line 10132 "Parser/parser.cc"
    break;

  case 555:
#line 2303 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), (yyvsp[0].decl) ); }
#line 10138 "Parser/parser.cc"
    break;

  case 556:
#line 2308 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[0].tok), nullptr ); }
#line 10144 "Parser/parser.cc"
    break;

  case 557:
#line 2310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-2].tok), nullptr ); }
#line 10150 "Parser/parser.cc"
    break;

  case 558:
#line 2312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 10156 "Parser/parser.cc"
    break;

  case 559:
#line 2317 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "elaborated_type %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10167 "Parser/parser.cc"
    break;

  case 563:
#line 2333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 10173 "Parser/parser.cc"
    break;

  case 564:
#line 2335 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 10179 "Parser/parser.cc"
    break;

  case 565:
#line 2337 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 10188 "Parser/parser.cc"
    break;

  case 566:
#line 2342 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 10205 "Parser/parser.cc"
    break;

  case 567:
#line 2355 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 10214 "Parser/parser.cc"
    break;

  case 568:
#line 2360 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "AGG3\n" );
			DeclarationNode::newFromTypedef( (yyvsp[-5].tok) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 10224 "Parser/parser.cc"
    break;

  case 569:
#line 2366 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 10233 "Parser/parser.cc"
    break;

  case 570:
#line 2371 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "AGG4\n" );
			DeclarationNode::newFromTypeGen( (yyvsp[-5].tok), nullptr );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 10243 "Parser/parser.cc"
    break;

  case 572:
#line 2381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 10249 "Parser/parser.cc"
    break;

  case 573:
#line 2383 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 10255 "Parser/parser.cc"
    break;

  case 574:
#line 2388 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 10265 "Parser/parser.cc"
    break;

  case 575:
#line 2394 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 10280 "Parser/parser.cc"
    break;

  case 578:
#line 2413 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Struct; }
#line 10286 "Parser/parser.cc"
    break;

  case 579:
#line 2415 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Union; }
#line 10292 "Parser/parser.cc"
    break;

  case 580:
#line 2417 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Exception; }
#line 10298 "Parser/parser.cc"
    break;

  case 581:
#line 2423 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 10304 "Parser/parser.cc"
    break;

  case 582:
#line 2425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 10310 "Parser/parser.cc"
    break;

  case 583:
#line 2427 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Generator; }
#line 10316 "Parser/parser.cc"
    break;

  case 584:
#line 2429 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor generator is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 10322 "Parser/parser.cc"
    break;

  case 585:
#line 2431 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Coroutine; }
#line 10328 "Parser/parser.cc"
    break;

  case 586:
#line 2433 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor coroutine is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 10334 "Parser/parser.cc"
    break;

  case 587:
#line 2435 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Thread; }
#line 10340 "Parser/parser.cc"
    break;

  case 588:
#line 2437 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor thread is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 10346 "Parser/parser.cc"
    break;

  case 589:
#line 2442 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10352 "Parser/parser.cc"
    break;

  case 590:
#line 2444 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 10358 "Parser/parser.cc"
    break;

  case 591:
#line 2449 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "type_specifier1 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			(yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) );
			// printf( "type_specifier2 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10371 "Parser/parser.cc"
    break;

  case 592:
#line 2458 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 10377 "Parser/parser.cc"
    break;

  case 593:
#line 2460 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-1].decl) ) {								// field declarator ?
				(yyvsp[-1].decl) = DeclarationNode::newName( nullptr );
			} // if
			(yyvsp[-1].decl)->inLine = true;
			(yyval.decl) = distAttr( (yyvsp[-2].decl), (yyvsp[-1].decl) );					// mark all fields in list
			distInl( (yyvsp[-1].decl) );
		}
#line 10390 "Parser/parser.cc"
    break;

  case 594:
#line 2469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10396 "Parser/parser.cc"
    break;

  case 597:
#line 2473 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 10402 "Parser/parser.cc"
    break;

  case 598:
#line 2475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10408 "Parser/parser.cc"
    break;

  case 601:
#line 2482 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10414 "Parser/parser.cc"
    break;

  case 603:
#line 2485 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 10420 "Parser/parser.cc"
    break;

  case 604:
#line 2490 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].en) ); }
#line 10426 "Parser/parser.cc"
    break;

  case 605:
#line 2493 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 10432 "Parser/parser.cc"
    break;

  case 606:
#line 2496 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 10438 "Parser/parser.cc"
    break;

  case 607:
#line 2501 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10444 "Parser/parser.cc"
    break;

  case 609:
#line 2504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 10450 "Parser/parser.cc"
    break;

  case 611:
#line 2515 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 10456 "Parser/parser.cc"
    break;

  case 612:
#line 2517 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 10462 "Parser/parser.cc"
    break;

  case 614:
#line 2524 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 10468 "Parser/parser.cc"
    break;

  case 615:
#line 2529 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 10474 "Parser/parser.cc"
    break;

  case 617:
#line 2535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 10480 "Parser/parser.cc"
    break;

  case 618:
#line 2540 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, false )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10486 "Parser/parser.cc"
    break;

  case 619:
#line 2542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); }
#line 10492 "Parser/parser.cc"
    break;

  case 620:
#line 2544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].tok), (yyvsp[-2].decl), true, false )->addQualifiers( (yyvsp[-6].decl) ); }
#line 10498 "Parser/parser.cc"
    break;

  case 621:
#line 2547 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-4].decl)->name, (yyvsp[-2].decl), true, false )->addQualifiers( (yyvsp[-5].decl) ); }
#line 10504 "Parser/parser.cc"
    break;

  case 622:
#line 2549 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-6].decl)->storageClasses.val != 0 || (yyvsp[-6].decl)->type->qualifiers.val != 0 )
			{ SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }

			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true, (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 10515 "Parser/parser.cc"
    break;

  case 623:
#line 2556 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 10523 "Parser/parser.cc"
    break;

  case 624:
#line 2560 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].decl)->storageClasses.val != 0 || (yyvsp[-4].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			typedefTable.makeTypedef( *(yyvsp[-1].tok) );
		}
#line 10532 "Parser/parser.cc"
    break;

  case 625:
#line 2565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, true, (yyvsp[-9].decl) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 10540 "Parser/parser.cc"
    break;

  case 626:
#line 2570 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].tok), (yyvsp[-2].decl), true, true, nullptr )->addQualifiers( (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 10548 "Parser/parser.cc"
    break;

  case 627:
#line 2574 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].decl)->name, (yyvsp[-2].decl), true, true, (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 10556 "Parser/parser.cc"
    break;

  case 628:
#line 2578 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].decl)->name, (yyvsp[-2].decl), true, true, nullptr )->addQualifiers( (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 10564 "Parser/parser.cc"
    break;

  case 630:
#line 2586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); (yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), 0, false, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 10570 "Parser/parser.cc"
    break;

  case 631:
#line 2588 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].decl)->type->symbolic.name );	(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].decl)->type->symbolic.name, 0, false, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 10576 "Parser/parser.cc"
    break;

  case 632:
#line 2593 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].in) ); }
#line 10582 "Parser/parser.cc"
    break;

  case 633:
#line 2595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( new string("inline"), nullptr ); }
#line 10588 "Parser/parser.cc"
    break;

  case 634:
#line 2597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].in) ) ); }
#line 10594 "Parser/parser.cc"
    break;

  case 635:
#line 2599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( DeclarationNode::newEnumValueGeneric( new string("inline"), nullptr ) ); }
#line 10600 "Parser/parser.cc"
    break;

  case 636:
#line 2604 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 10606 "Parser/parser.cc"
    break;

  case 637:
#line 2605 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = new InitializerNode( (yyvsp[0].en) ); }
#line 10612 "Parser/parser.cc"
    break;

  case 638:
#line 2606 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                     { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 10618 "Parser/parser.cc"
    break;

  case 639:
#line 2613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 10624 "Parser/parser.cc"
    break;

  case 640:
#line 2615 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10630 "Parser/parser.cc"
    break;

  case 643:
#line 2619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10636 "Parser/parser.cc"
    break;

  case 644:
#line 2621 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 10642 "Parser/parser.cc"
    break;

  case 645:
#line 2623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 10648 "Parser/parser.cc"
    break;

  case 647:
#line 2631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10654 "Parser/parser.cc"
    break;

  case 648:
#line 2633 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10660 "Parser/parser.cc"
    break;

  case 649:
#line 2635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-8].decl)->appendList( (yyvsp[-4].decl) )->appendList( (yyvsp[0].decl) ); }
#line 10666 "Parser/parser.cc"
    break;

  case 651:
#line 2641 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10672 "Parser/parser.cc"
    break;

  case 652:
#line 2646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10678 "Parser/parser.cc"
    break;

  case 653:
#line 2648 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10684 "Parser/parser.cc"
    break;

  case 655:
#line 2651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 10690 "Parser/parser.cc"
    break;

  case 658:
#line 2658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10696 "Parser/parser.cc"
    break;

  case 659:
#line 2660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10702 "Parser/parser.cc"
    break;

  case 661:
#line 2669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 10708 "Parser/parser.cc"
    break;

  case 662:
#line 2672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 10714 "Parser/parser.cc"
    break;

  case 663:
#line 2674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 10720 "Parser/parser.cc"
    break;

  case 668:
#line 2684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10726 "Parser/parser.cc"
    break;

  case 670:
#line 2691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 10732 "Parser/parser.cc"
    break;

  case 671:
#line 2693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 10738 "Parser/parser.cc"
    break;

  case 672:
#line 2698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 10744 "Parser/parser.cc"
    break;

  case 673:
#line 2700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 10750 "Parser/parser.cc"
    break;

  case 674:
#line 2709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 10756 "Parser/parser.cc"
    break;

  case 675:
#line 2711 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 10762 "Parser/parser.cc"
    break;

  case 681:
#line 2724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10768 "Parser/parser.cc"
    break;

  case 684:
#line 2734 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 10774 "Parser/parser.cc"
    break;

  case 685:
#line 2735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (yyvsp[-1].op) == OperKinds::Assign ? (yyvsp[0].in) : (yyvsp[0].in)->set_maybeConstructed( false ); }
#line 10780 "Parser/parser.cc"
    break;

  case 686:
#line 2736 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.in) = new InitializerNode( true ); }
#line 10786 "Parser/parser.cc"
    break;

  case 687:
#line 2737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 10792 "Parser/parser.cc"
    break;

  case 688:
#line 2741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = new InitializerNode( (yyvsp[0].en) ); }
#line 10798 "Parser/parser.cc"
    break;

  case 689:
#line 2742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 10804 "Parser/parser.cc"
    break;

  case 690:
#line 2747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 10810 "Parser/parser.cc"
    break;

  case 692:
#line 2749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ); }
#line 10816 "Parser/parser.cc"
    break;

  case 693:
#line 2750 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (InitializerNode *)( (yyvsp[-2].in)->set_last( (yyvsp[0].in) ) ); }
#line 10822 "Parser/parser.cc"
    break;

  case 694:
#line 2751 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.in) = (InitializerNode *)((yyvsp[-3].in)->set_last( (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ) )); }
#line 10828 "Parser/parser.cc"
    break;

  case 696:
#line 2767 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[-1].tok) ) ); }
#line 10834 "Parser/parser.cc"
    break;

  case 698:
#line 2773 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-1].en)->set_last( (yyvsp[0].en) )); }
#line 10840 "Parser/parser.cc"
    break;

  case 699:
#line 2779 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 10846 "Parser/parser.cc"
    break;

  case 700:
#line 2782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 10852 "Parser/parser.cc"
    break;

  case 701:
#line 2784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 10858 "Parser/parser.cc"
    break;

  case 702:
#line 2786 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-4].en) ), maybeMoveBuild<Expression>( (yyvsp[-2].en) ) ) ); }
#line 10864 "Parser/parser.cc"
    break;

  case 703:
#line 2788 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 10870 "Parser/parser.cc"
    break;

  case 705:
#line 2812 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl) ); }
#line 10876 "Parser/parser.cc"
    break;

  case 706:
#line 2817 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10882 "Parser/parser.cc"
    break;

  case 707:
#line 2819 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 10888 "Parser/parser.cc"
    break;

  case 708:
#line 2824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "9" );
			if ( (yyvsp[-1].tclass) == TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 10899 "Parser/parser.cc"
    break;

  case 709:
#line 2831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 10905 "Parser/parser.cc"
    break;

  case 710:
#line 2833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "9" ); }
#line 10911 "Parser/parser.cc"
    break;

  case 711:
#line 2835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 10917 "Parser/parser.cc"
    break;

  case 712:
#line 2837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "9" );
			(yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 10926 "Parser/parser.cc"
    break;

  case 713:
#line 2843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 10932 "Parser/parser.cc"
    break;

  case 714:
#line 2848 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 10938 "Parser/parser.cc"
    break;

  case 715:
#line 2850 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 10944 "Parser/parser.cc"
    break;

  case 716:
#line 2852 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::DStype; }
#line 10950 "Parser/parser.cc"
    break;

  case 717:
#line 2856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 10956 "Parser/parser.cc"
    break;

  case 718:
#line 2861 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 10962 "Parser/parser.cc"
    break;

  case 719:
#line 2863 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 10968 "Parser/parser.cc"
    break;

  case 720:
#line 2865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ftype; }
#line 10974 "Parser/parser.cc"
    break;

  case 721:
#line 2867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 10980 "Parser/parser.cc"
    break;

  case 722:
#line 2872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10986 "Parser/parser.cc"
    break;

  case 725:
#line 2879 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 10992 "Parser/parser.cc"
    break;

  case 726:
#line 2884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 10998 "Parser/parser.cc"
    break;

  case 727:
#line 2886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 11004 "Parser/parser.cc"
    break;

  case 728:
#line 2893 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 11010 "Parser/parser.cc"
    break;

  case 730:
#line 2896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 11016 "Parser/parser.cc"
    break;

  case 731:
#line 2898 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)( (yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 11022 "Parser/parser.cc"
    break;

  case 732:
#line 2903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 11028 "Parser/parser.cc"
    break;

  case 733:
#line 2905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11034 "Parser/parser.cc"
    break;

  case 734:
#line 2907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 11040 "Parser/parser.cc"
    break;

  case 735:
#line 2912 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 11046 "Parser/parser.cc"
    break;

  case 736:
#line 2914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 11052 "Parser/parser.cc"
    break;

  case 737:
#line 2919 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "10" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), 0 );
		}
#line 11061 "Parser/parser.cc"
    break;

  case 738:
#line 2924 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "11" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 11070 "Parser/parser.cc"
    break;

  case 739:
#line 2932 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), 0 ); }
#line 11076 "Parser/parser.cc"
    break;

  case 740:
#line 2934 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-8].tok), (yyvsp[-6].decl), (yyvsp[-2].decl) ); }
#line 11082 "Parser/parser.cc"
    break;

  case 742:
#line 2940 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl) ); }
#line 11088 "Parser/parser.cc"
    break;

  case 747:
#line 2952 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 11094 "Parser/parser.cc"
    break;

  case 748:
#line 2957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 11100 "Parser/parser.cc"
    break;

  case 749:
#line 2959 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 11106 "Parser/parser.cc"
    break;

  case 751:
#line 2967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl);	}
#line 11112 "Parser/parser.cc"
    break;

  case 752:
#line 2972 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11118 "Parser/parser.cc"
    break;

  case 753:
#line 2974 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->appendList( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 11124 "Parser/parser.cc"
    break;

  case 754:
#line 2979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11130 "Parser/parser.cc"
    break;

  case 756:
#line 2984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 11136 "Parser/parser.cc"
    break;

  case 757:
#line 2988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 11142 "Parser/parser.cc"
    break;

  case 758:
#line 2993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( (yyvsp[0].tok) ) ) ); }
#line 11148 "Parser/parser.cc"
    break;

  case 761:
#line 2997 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 11157 "Parser/parser.cc"
    break;

  case 762:
#line 3002 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( false, (yyvsp[-2].constant), 0 ) ) ); }
#line 11163 "Parser/parser.cc"
    break;

  case 763:
#line 3004 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = LinkageSpec::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 11172 "Parser/parser.cc"
    break;

  case 764:
#line 3009 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-1].decl);
		}
#line 11182 "Parser/parser.cc"
    break;

  case 765:
#line 3015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = LinkageSpec::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 11191 "Parser/parser.cc"
    break;

  case 766:
#line 3020 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11201 "Parser/parser.cc"
    break;

  case 767:
#line 3026 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 11210 "Parser/parser.cc"
    break;

  case 768:
#line 3031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11220 "Parser/parser.cc"
    break;

  case 769:
#line 3037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 11229 "Parser/parser.cc"
    break;

  case 770:
#line 3042 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11239 "Parser/parser.cc"
    break;

  case 771:
#line 3048 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.val) || (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 11248 "Parser/parser.cc"
    break;

  case 772:
#line 3053 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11258 "Parser/parser.cc"
    break;

  case 774:
#line 3068 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].sn) ); }
#line 11264 "Parser/parser.cc"
    break;

  case 775:
#line 3070 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].sn) ); }
#line 11270 "Parser/parser.cc"
    break;

  case 776:
#line 3075 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; forall = false; }
#line 11276 "Parser/parser.cc"
    break;

  case 777:
#line 3077 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.en) = (yyvsp[-2].en); forall = false;
			if ( (yyvsp[0].decl) ) {
				SemanticError( yylloc, "Attributes cannot be associated with function body. Move attribute(s) before \"with\" clause." );
				(yyval.en) = nullptr;
			} // if
		}
#line 11288 "Parser/parser.cc"
    break;

  case 778:
#line 3088 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11299 "Parser/parser.cc"
    break;

  case 779:
#line 3095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 11308 "Parser/parser.cc"
    break;

  case 780:
#line 3100 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 11317 "Parser/parser.cc"
    break;

  case 781:
#line 3106 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 11323 "Parser/parser.cc"
    break;

  case 782:
#line 3109 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 11329 "Parser/parser.cc"
    break;

  case 783:
#line 3112 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 11335 "Parser/parser.cc"
    break;

  case 784:
#line 3116 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-4].decl) );
		}
#line 11344 "Parser/parser.cc"
    break;

  case 785:
#line 3122 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 11350 "Parser/parser.cc"
    break;

  case 786:
#line 3125 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 11356 "Parser/parser.cc"
    break;

  case 787:
#line 3128 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 11362 "Parser/parser.cc"
    break;

  case 791:
#line 3139 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 11368 "Parser/parser.cc"
    break;

  case 792:
#line 3144 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11374 "Parser/parser.cc"
    break;

  case 793:
#line 3146 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = (yyvsp[-2].constant);
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 11384 "Parser/parser.cc"
    break;

  case 794:
#line 3155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11390 "Parser/parser.cc"
    break;

  case 797:
#line 3162 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11396 "Parser/parser.cc"
    break;

  case 798:
#line 3167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 11402 "Parser/parser.cc"
    break;

  case 800:
#line 3173 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11408 "Parser/parser.cc"
    break;

  case 801:
#line 3178 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11414 "Parser/parser.cc"
    break;

  case 802:
#line 3180 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 11420 "Parser/parser.cc"
    break;

  case 803:
#line 3182 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 11426 "Parser/parser.cc"
    break;

  case 808:
#line 3191 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 11432 "Parser/parser.cc"
    break;

  case 809:
#line 3193 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 11438 "Parser/parser.cc"
    break;

  case 810:
#line 3228 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11444 "Parser/parser.cc"
    break;

  case 811:
#line 3230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11450 "Parser/parser.cc"
    break;

  case 812:
#line 3235 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11456 "Parser/parser.cc"
    break;

  case 814:
#line 3238 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11462 "Parser/parser.cc"
    break;

  case 815:
#line 3240 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11468 "Parser/parser.cc"
    break;

  case 816:
#line 3245 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11474 "Parser/parser.cc"
    break;

  case 817:
#line 3247 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11480 "Parser/parser.cc"
    break;

  case 818:
#line 3249 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11486 "Parser/parser.cc"
    break;

  case 819:
#line 3251 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11492 "Parser/parser.cc"
    break;

  case 820:
#line 3256 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11498 "Parser/parser.cc"
    break;

  case 821:
#line 3258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11504 "Parser/parser.cc"
    break;

  case 822:
#line 3260 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11510 "Parser/parser.cc"
    break;

  case 823:
#line 3262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11516 "Parser/parser.cc"
    break;

  case 824:
#line 3264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11522 "Parser/parser.cc"
    break;

  case 825:
#line 3266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11528 "Parser/parser.cc"
    break;

  case 826:
#line 3268 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11534 "Parser/parser.cc"
    break;

  case 827:
#line 3273 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11540 "Parser/parser.cc"
    break;

  case 828:
#line 3275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 11546 "Parser/parser.cc"
    break;

  case 829:
#line 3277 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11552 "Parser/parser.cc"
    break;

  case 830:
#line 3279 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11558 "Parser/parser.cc"
    break;

  case 831:
#line 3288 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11564 "Parser/parser.cc"
    break;

  case 833:
#line 3291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11570 "Parser/parser.cc"
    break;

  case 834:
#line 3296 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11576 "Parser/parser.cc"
    break;

  case 835:
#line 3298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11582 "Parser/parser.cc"
    break;

  case 836:
#line 3300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 11588 "Parser/parser.cc"
    break;

  case 837:
#line 3302 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11594 "Parser/parser.cc"
    break;

  case 838:
#line 3304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11600 "Parser/parser.cc"
    break;

  case 839:
#line 3309 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11606 "Parser/parser.cc"
    break;

  case 840:
#line 3311 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11612 "Parser/parser.cc"
    break;

  case 841:
#line 3313 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11618 "Parser/parser.cc"
    break;

  case 842:
#line 3315 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11624 "Parser/parser.cc"
    break;

  case 843:
#line 3320 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11630 "Parser/parser.cc"
    break;

  case 844:
#line 3322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11636 "Parser/parser.cc"
    break;

  case 845:
#line 3324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11642 "Parser/parser.cc"
    break;

  case 846:
#line 3326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11648 "Parser/parser.cc"
    break;

  case 847:
#line 3328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11654 "Parser/parser.cc"
    break;

  case 848:
#line 3330 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11660 "Parser/parser.cc"
    break;

  case 852:
#line 3348 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 11666 "Parser/parser.cc"
    break;

  case 853:
#line 3350 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11672 "Parser/parser.cc"
    break;

  case 854:
#line 3352 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 11678 "Parser/parser.cc"
    break;

  case 855:
#line 3354 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11684 "Parser/parser.cc"
    break;

  case 856:
#line 3356 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11690 "Parser/parser.cc"
    break;

  case 857:
#line 3361 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11696 "Parser/parser.cc"
    break;

  case 858:
#line 3363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11702 "Parser/parser.cc"
    break;

  case 859:
#line 3365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11708 "Parser/parser.cc"
    break;

  case 860:
#line 3367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11714 "Parser/parser.cc"
    break;

  case 861:
#line 3372 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11720 "Parser/parser.cc"
    break;

  case 862:
#line 3374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11726 "Parser/parser.cc"
    break;

  case 863:
#line 3376 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11732 "Parser/parser.cc"
    break;

  case 864:
#line 3378 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11738 "Parser/parser.cc"
    break;

  case 865:
#line 3380 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11744 "Parser/parser.cc"
    break;

  case 866:
#line 3382 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11750 "Parser/parser.cc"
    break;

  case 867:
#line 3397 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "ID" );
		}
#line 11759 "Parser/parser.cc"
    break;

  case 868:
#line 3402 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11765 "Parser/parser.cc"
    break;

  case 869:
#line 3407 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11771 "Parser/parser.cc"
    break;

  case 871:
#line 3410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11777 "Parser/parser.cc"
    break;

  case 872:
#line 3412 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11783 "Parser/parser.cc"
    break;

  case 873:
#line 3417 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11789 "Parser/parser.cc"
    break;

  case 874:
#line 3419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11795 "Parser/parser.cc"
    break;

  case 875:
#line 3421 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11801 "Parser/parser.cc"
    break;

  case 876:
#line 3423 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11807 "Parser/parser.cc"
    break;

  case 877:
#line 3428 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11813 "Parser/parser.cc"
    break;

  case 878:
#line 3430 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11819 "Parser/parser.cc"
    break;

  case 879:
#line 3432 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11825 "Parser/parser.cc"
    break;

  case 880:
#line 3434 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11831 "Parser/parser.cc"
    break;

  case 881:
#line 3436 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11837 "Parser/parser.cc"
    break;

  case 882:
#line 3438 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11843 "Parser/parser.cc"
    break;

  case 883:
#line 3440 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11849 "Parser/parser.cc"
    break;

  case 884:
#line 3445 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11855 "Parser/parser.cc"
    break;

  case 885:
#line 3447 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11861 "Parser/parser.cc"
    break;

  case 886:
#line 3449 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 11867 "Parser/parser.cc"
    break;

  case 887:
#line 3451 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11873 "Parser/parser.cc"
    break;

  case 888:
#line 3453 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11879 "Parser/parser.cc"
    break;

  case 889:
#line 3463 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11885 "Parser/parser.cc"
    break;

  case 890:
#line 3465 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11891 "Parser/parser.cc"
    break;

  case 892:
#line 3468 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11897 "Parser/parser.cc"
    break;

  case 893:
#line 3470 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11903 "Parser/parser.cc"
    break;

  case 894:
#line 3475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11909 "Parser/parser.cc"
    break;

  case 895:
#line 3477 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11915 "Parser/parser.cc"
    break;

  case 896:
#line 3479 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11921 "Parser/parser.cc"
    break;

  case 897:
#line 3484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11927 "Parser/parser.cc"
    break;

  case 898:
#line 3486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11933 "Parser/parser.cc"
    break;

  case 899:
#line 3488 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11939 "Parser/parser.cc"
    break;

  case 900:
#line 3490 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11945 "Parser/parser.cc"
    break;

  case 901:
#line 3495 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11951 "Parser/parser.cc"
    break;

  case 902:
#line 3497 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11957 "Parser/parser.cc"
    break;

  case 903:
#line 3499 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11963 "Parser/parser.cc"
    break;

  case 904:
#line 3513 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11969 "Parser/parser.cc"
    break;

  case 905:
#line 3515 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11975 "Parser/parser.cc"
    break;

  case 907:
#line 3518 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11981 "Parser/parser.cc"
    break;

  case 908:
#line 3520 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11987 "Parser/parser.cc"
    break;

  case 909:
#line 3525 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11993 "Parser/parser.cc"
    break;

  case 910:
#line 3527 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11999 "Parser/parser.cc"
    break;

  case 911:
#line 3532 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12005 "Parser/parser.cc"
    break;

  case 912:
#line 3534 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12011 "Parser/parser.cc"
    break;

  case 913:
#line 3536 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12017 "Parser/parser.cc"
    break;

  case 914:
#line 3541 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12023 "Parser/parser.cc"
    break;

  case 915:
#line 3543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12029 "Parser/parser.cc"
    break;

  case 916:
#line 3548 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12035 "Parser/parser.cc"
    break;

  case 917:
#line 3550 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12041 "Parser/parser.cc"
    break;

  case 919:
#line 3568 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12047 "Parser/parser.cc"
    break;

  case 920:
#line 3570 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12053 "Parser/parser.cc"
    break;

  case 921:
#line 3575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( 0, (yyvsp[0].op) ); }
#line 12059 "Parser/parser.cc"
    break;

  case 922:
#line 3577 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 12065 "Parser/parser.cc"
    break;

  case 923:
#line 3579 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12071 "Parser/parser.cc"
    break;

  case 924:
#line 3581 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12077 "Parser/parser.cc"
    break;

  case 925:
#line 3583 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12083 "Parser/parser.cc"
    break;

  case 927:
#line 3589 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12089 "Parser/parser.cc"
    break;

  case 928:
#line 3591 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12095 "Parser/parser.cc"
    break;

  case 929:
#line 3593 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12101 "Parser/parser.cc"
    break;

  case 930:
#line 3598 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 12107 "Parser/parser.cc"
    break;

  case 931:
#line 3600 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12113 "Parser/parser.cc"
    break;

  case 932:
#line 3602 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12119 "Parser/parser.cc"
    break;

  case 933:
#line 3608 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false ); }
#line 12125 "Parser/parser.cc"
    break;

  case 934:
#line 3610 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false )->addArray( (yyvsp[0].decl) ); }
#line 12131 "Parser/parser.cc"
    break;

  case 935:
#line 3612 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-4].en), 0, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].en), 0, false ) ); }
#line 12137 "Parser/parser.cc"
    break;

  case 937:
#line 3619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), 0, false ); }
#line 12143 "Parser/parser.cc"
    break;

  case 938:
#line 3621 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 12149 "Parser/parser.cc"
    break;

  case 939:
#line 3623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newArray( (yyvsp[-2].en), 0, false ) ); }
#line 12155 "Parser/parser.cc"
    break;

  case 940:
#line 3625 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 12161 "Parser/parser.cc"
    break;

  case 942:
#line 3660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 12167 "Parser/parser.cc"
    break;

  case 943:
#line 3662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12173 "Parser/parser.cc"
    break;

  case 944:
#line 3664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12179 "Parser/parser.cc"
    break;

  case 945:
#line 3669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].op) ); }
#line 12185 "Parser/parser.cc"
    break;

  case 946:
#line 3671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 12191 "Parser/parser.cc"
    break;

  case 947:
#line 3673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12197 "Parser/parser.cc"
    break;

  case 948:
#line 3675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12203 "Parser/parser.cc"
    break;

  case 949:
#line 3677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12209 "Parser/parser.cc"
    break;

  case 951:
#line 3683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12215 "Parser/parser.cc"
    break;

  case 952:
#line 3685 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12221 "Parser/parser.cc"
    break;

  case 953:
#line 3687 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12227 "Parser/parser.cc"
    break;

  case 954:
#line 3692 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 12233 "Parser/parser.cc"
    break;

  case 955:
#line 3694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12239 "Parser/parser.cc"
    break;

  case 956:
#line 3696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12245 "Parser/parser.cc"
    break;

  case 958:
#line 3703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12251 "Parser/parser.cc"
    break;

  case 960:
#line 3714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false ); }
#line 12257 "Parser/parser.cc"
    break;

  case 961:
#line 3717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 12263 "Parser/parser.cc"
    break;

  case 962:
#line 3719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, (yyvsp[-2].decl), false ); }
#line 12269 "Parser/parser.cc"
    break;

  case 963:
#line 3722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 12275 "Parser/parser.cc"
    break;

  case 964:
#line 3724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 12281 "Parser/parser.cc"
    break;

  case 965:
#line 3726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-4].decl), true ); }
#line 12287 "Parser/parser.cc"
    break;

  case 967:
#line 3741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12293 "Parser/parser.cc"
    break;

  case 968:
#line 3743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12299 "Parser/parser.cc"
    break;

  case 969:
#line 3748 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( 0, (yyvsp[0].op) ); }
#line 12305 "Parser/parser.cc"
    break;

  case 970:
#line 3750 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 12311 "Parser/parser.cc"
    break;

  case 971:
#line 3752 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12317 "Parser/parser.cc"
    break;

  case 972:
#line 3754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12323 "Parser/parser.cc"
    break;

  case 973:
#line 3756 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12329 "Parser/parser.cc"
    break;

  case 975:
#line 3762 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12335 "Parser/parser.cc"
    break;

  case 976:
#line 3764 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12341 "Parser/parser.cc"
    break;

  case 977:
#line 3766 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12347 "Parser/parser.cc"
    break;

  case 978:
#line 3771 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12353 "Parser/parser.cc"
    break;

  case 979:
#line 3773 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12359 "Parser/parser.cc"
    break;

  case 982:
#line 3783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12365 "Parser/parser.cc"
    break;

  case 985:
#line 3794 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12371 "Parser/parser.cc"
    break;

  case 986:
#line 3796 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12377 "Parser/parser.cc"
    break;

  case 987:
#line 3798 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12383 "Parser/parser.cc"
    break;

  case 988:
#line 3800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12389 "Parser/parser.cc"
    break;

  case 989:
#line 3802 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12395 "Parser/parser.cc"
    break;

  case 990:
#line 3804 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12401 "Parser/parser.cc"
    break;

  case 991:
#line 3811 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 12407 "Parser/parser.cc"
    break;

  case 992:
#line 3813 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12413 "Parser/parser.cc"
    break;

  case 993:
#line 3815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 12419 "Parser/parser.cc"
    break;

  case 994:
#line 3817 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 12425 "Parser/parser.cc"
    break;

  case 995:
#line 3819 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12431 "Parser/parser.cc"
    break;

  case 996:
#line 3822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 12437 "Parser/parser.cc"
    break;

  case 997:
#line 3824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12443 "Parser/parser.cc"
    break;

  case 998:
#line 3826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 12449 "Parser/parser.cc"
    break;

  case 999:
#line 3828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 12455 "Parser/parser.cc"
    break;

  case 1000:
#line 3830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12461 "Parser/parser.cc"
    break;

  case 1001:
#line 3835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 12467 "Parser/parser.cc"
    break;

  case 1002:
#line 3837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 12473 "Parser/parser.cc"
    break;

  case 1003:
#line 3842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 12479 "Parser/parser.cc"
    break;

  case 1004:
#line 3844 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl)->addQualifiers( (yyvsp[-4].decl) ), true ); }
#line 12485 "Parser/parser.cc"
    break;

  case 1006:
#line 3871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12491 "Parser/parser.cc"
    break;

  case 1010:
#line 3882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12497 "Parser/parser.cc"
    break;

  case 1011:
#line 3884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12503 "Parser/parser.cc"
    break;

  case 1012:
#line 3886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12509 "Parser/parser.cc"
    break;

  case 1013:
#line 3888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12515 "Parser/parser.cc"
    break;

  case 1014:
#line 3890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12521 "Parser/parser.cc"
    break;

  case 1015:
#line 3892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12527 "Parser/parser.cc"
    break;

  case 1016:
#line 3899 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12533 "Parser/parser.cc"
    break;

  case 1017:
#line 3901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12539 "Parser/parser.cc"
    break;

  case 1018:
#line 3903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12545 "Parser/parser.cc"
    break;

  case 1019:
#line 3905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12551 "Parser/parser.cc"
    break;

  case 1020:
#line 3907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12557 "Parser/parser.cc"
    break;

  case 1021:
#line 3909 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12563 "Parser/parser.cc"
    break;

  case 1022:
#line 3914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 12569 "Parser/parser.cc"
    break;

  case 1023:
#line 3916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 12575 "Parser/parser.cc"
    break;

  case 1024:
#line 3918 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 12581 "Parser/parser.cc"
    break;

  case 1025:
#line 3925 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 12587 "Parser/parser.cc"
    break;

  case 1026:
#line 3927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 12593 "Parser/parser.cc"
    break;

  case 1029:
#line 3951 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 12599 "Parser/parser.cc"
    break;

  case 1030:
#line 3953 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 12605 "Parser/parser.cc"
    break;


#line 12609 "Parser/parser.cc"

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
#line 3956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "make install" //
// End: //
