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

#line 297 "Parser/parser.cc"

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
#line 268 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"

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

#line 665 "Parser/parser.cc"

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
#define YYLAST   21407

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  175
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  291
/* YYNRULES -- Number of rules.  */
#define YYNRULES  1027
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  2076

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
       0,   567,   567,   571,   578,   579,   580,   581,   582,   586,
     587,   588,   589,   590,   591,   592,   596,   597,   601,   602,
     607,   611,   612,   623,   625,   627,   631,   632,   634,   636,
     638,   640,   650,   658,   667,   668,   678,   683,   688,   689,
     694,   700,   702,   704,   710,   712,   714,   716,   718,   720,
     722,   724,   726,   728,   730,   732,   734,   736,   738,   740,
     742,   752,   753,   757,   758,   763,   766,   770,   771,   775,
     776,   778,   780,   782,   784,   786,   791,   793,   795,   803,
     804,   812,   815,   816,   818,   823,   839,   841,   843,   845,
     847,   849,   851,   853,   855,   863,   864,   866,   870,   871,
     872,   873,   877,   878,   880,   882,   884,   886,   888,   890,
     892,   899,   900,   901,   902,   906,   907,   911,   912,   917,
     918,   920,   922,   927,   928,   930,   935,   936,   938,   943,
     944,   946,   948,   950,   955,   956,   958,   963,   964,   969,
     970,   975,   976,   981,   982,   987,   988,   993,   994,   997,
    1002,  1007,  1008,  1016,  1022,  1023,  1027,  1028,  1032,  1033,
    1037,  1038,  1039,  1040,  1041,  1042,  1043,  1044,  1045,  1046,
    1047,  1057,  1059,  1064,  1065,  1067,  1069,  1074,  1075,  1081,
    1082,  1088,  1089,  1090,  1091,  1092,  1093,  1094,  1095,  1096,
    1097,  1098,  1100,  1101,  1107,  1109,  1119,  1121,  1129,  1130,
    1135,  1137,  1139,  1141,  1143,  1147,  1148,  1150,  1155,  1157,
    1164,  1166,  1168,  1178,  1180,  1182,  1187,  1192,  1195,  1200,
    1202,  1204,  1206,  1214,  1215,  1217,  1221,  1223,  1227,  1229,
    1230,  1232,  1234,  1239,  1240,  1244,  1249,  1250,  1254,  1256,
    1261,  1263,  1268,  1270,  1272,  1274,  1279,  1281,  1283,  1285,
    1290,  1292,  1297,  1298,  1320,  1322,  1324,  1327,  1329,  1332,
    1334,  1337,  1339,  1344,  1349,  1351,  1356,  1361,  1363,  1365,
    1367,  1369,  1372,  1374,  1377,  1379,  1384,  1390,  1393,  1395,
    1400,  1406,  1408,  1413,  1419,  1422,  1424,  1427,  1429,  1434,
    1441,  1443,  1448,  1454,  1456,  1461,  1467,  1470,  1475,  1483,
    1485,  1487,  1492,  1494,  1499,  1500,  1502,  1507,  1509,  1514,
    1516,  1518,  1520,  1523,  1527,  1530,  1534,  1536,  1538,  1540,
    1542,  1544,  1546,  1548,  1550,  1552,  1554,  1559,  1560,  1564,
    1570,  1575,  1580,  1581,  1585,  1589,  1594,  1595,  1601,  1605,
    1607,  1609,  1611,  1614,  1616,  1621,  1623,  1628,  1630,  1632,
    1637,  1639,  1645,  1646,  1650,  1651,  1652,  1653,  1657,  1662,
    1663,  1665,  1667,  1669,  1673,  1677,  1678,  1682,  1684,  1686,
    1688,  1690,  1696,  1697,  1703,  1704,  1708,  1709,  1714,  1716,
    1722,  1723,  1725,  1730,  1735,  1746,  1747,  1751,  1752,  1758,
    1759,  1763,  1765,  1769,  1771,  1775,  1776,  1780,  1781,  1785,
    1792,  1793,  1797,  1799,  1814,  1815,  1816,  1817,  1819,  1823,
    1825,  1829,  1836,  1838,  1840,  1845,  1846,  1848,  1850,  1852,
    1884,  1887,  1892,  1894,  1900,  1905,  1910,  1921,  1926,  1931,
    1936,  1941,  1950,  1954,  1961,  1963,  1964,  1965,  1971,  1973,
    1978,  1979,  1980,  1989,  1990,  1991,  1995,  1996,  2003,  2012,
    2013,  2014,  2019,  2020,  2029,  2030,  2035,  2036,  2040,  2042,
    2044,  2046,  2048,  2052,  2057,  2058,  2060,  2070,  2071,  2076,
    2078,  2080,  2082,  2084,  2087,  2089,  2091,  2096,  2098,  2100,
    2102,  2104,  2106,  2108,  2110,  2112,  2114,  2116,  2118,  2120,
    2122,  2124,  2126,  2128,  2130,  2132,  2134,  2136,  2138,  2140,
    2142,  2144,  2146,  2148,  2150,  2155,  2156,  2160,  2167,  2168,
    2174,  2175,  2177,  2179,  2181,  2186,  2188,  2193,  2194,  2196,
    2198,  2203,  2205,  2207,  2209,  2211,  2213,  2218,  2225,  2227,
    2229,  2234,  2242,  2241,  2245,  2253,  2254,  2256,  2258,  2263,
    2264,  2266,  2271,  2272,  2274,  2276,  2281,  2282,  2284,  2289,
    2291,  2293,  2295,  2296,  2298,  2303,  2305,  2307,  2312,  2319,
    2323,  2324,  2329,  2328,  2333,  2332,  2351,  2350,  2362,  2361,
    2372,  2377,  2378,  2383,  2389,  2403,  2404,  2408,  2410,  2412,
    2418,  2420,  2422,  2424,  2426,  2428,  2430,  2432,  2438,  2439,
    2444,  2453,  2455,  2464,  2466,  2467,  2468,  2470,  2472,  2473,
    2478,  2479,  2480,  2485,  2487,  2490,  2497,  2498,  2499,  2505,
    2510,  2512,  2518,  2519,  2525,  2526,  2530,  2535,  2538,  2537,
    2541,  2544,  2546,  2554,  2553,  2562,  2568,  2572,  2574,  2579,
    2581,  2583,  2585,  2591,  2592,  2593,  2600,  2601,  2603,  2604,
    2605,  2607,  2609,  2616,  2617,  2619,  2621,  2626,  2627,  2633,
    2634,  2636,  2637,  2642,  2643,  2644,  2646,  2654,  2655,  2657,
    2660,  2662,  2666,  2667,  2668,  2670,  2672,  2677,  2679,  2684,
    2686,  2695,  2697,  2702,  2703,  2704,  2708,  2709,  2710,  2715,
    2716,  2721,  2722,  2723,  2724,  2728,  2729,  2734,  2735,  2736,
    2737,  2738,  2752,  2753,  2758,  2759,  2765,  2767,  2770,  2772,
    2774,  2797,  2798,  2804,  2805,  2811,  2810,  2820,  2819,  2823,
    2829,  2835,  2836,  2838,  2842,  2847,  2849,  2851,  2853,  2859,
    2860,  2864,  2865,  2870,  2872,  2879,  2881,  2882,  2884,  2889,
    2891,  2893,  2898,  2900,  2905,  2910,  2918,  2920,  2925,  2926,
    2931,  2932,  2936,  2937,  2938,  2943,  2945,  2951,  2953,  2958,
    2960,  2966,  2967,  2971,  2975,  2979,  2981,  2982,  2983,  2988,
    2991,  2990,  3002,  3001,  3013,  3012,  3024,  3023,  3035,  3034,
    3048,  3054,  3056,  3062,  3063,  3074,  3081,  3086,  3092,  3095,
    3098,  3102,  3108,  3111,  3114,  3119,  3120,  3121,  3125,  3131,
    3132,  3142,  3143,  3147,  3148,  3153,  3158,  3159,  3165,  3166,
    3168,  3173,  3174,  3175,  3176,  3177,  3179,  3214,  3216,  3221,
    3223,  3224,  3226,  3231,  3233,  3235,  3237,  3242,  3244,  3246,
    3248,  3250,  3252,  3254,  3259,  3261,  3263,  3265,  3274,  3276,
    3277,  3282,  3284,  3286,  3288,  3290,  3295,  3297,  3299,  3301,
    3306,  3308,  3310,  3312,  3314,  3316,  3328,  3329,  3330,  3334,
    3336,  3338,  3340,  3342,  3347,  3349,  3351,  3353,  3358,  3360,
    3362,  3364,  3366,  3368,  3383,  3388,  3393,  3395,  3396,  3398,
    3403,  3405,  3407,  3409,  3414,  3416,  3418,  3420,  3422,  3424,
    3426,  3431,  3433,  3435,  3437,  3439,  3449,  3451,  3453,  3454,
    3456,  3461,  3463,  3465,  3470,  3472,  3474,  3476,  3481,  3483,
    3485,  3499,  3501,  3503,  3504,  3506,  3511,  3513,  3518,  3520,
    3522,  3527,  3529,  3534,  3536,  3553,  3554,  3556,  3561,  3563,
    3565,  3567,  3569,  3574,  3575,  3577,  3579,  3584,  3586,  3588,
    3594,  3596,  3598,  3601,  3605,  3607,  3609,  3611,  3645,  3646,
    3648,  3650,  3655,  3657,  3659,  3661,  3663,  3668,  3669,  3671,
    3673,  3678,  3680,  3682,  3688,  3689,  3691,  3700,  3703,  3705,
    3708,  3710,  3712,  3726,  3727,  3729,  3734,  3736,  3738,  3740,
    3742,  3747,  3748,  3750,  3752,  3757,  3759,  3767,  3768,  3769,
    3774,  3775,  3780,  3782,  3784,  3786,  3788,  3790,  3797,  3799,
    3801,  3803,  3805,  3808,  3810,  3812,  3814,  3816,  3821,  3823,
    3825,  3830,  3856,  3857,  3859,  3863,  3864,  3868,  3870,  3872,
    3874,  3876,  3878,  3885,  3887,  3889,  3891,  3893,  3895,  3900,
    3902,  3904,  3911,  3913,  3931,  3933,  3938,  3939
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

#define YYPACT_NINF (-1725)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-908)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     133, 11921,   145,   165, 16358,   128, -1725, -1725, -1725, -1725,
   -1725, -1725, -1725, -1725, -1725, -1725, -1725,   210,   873,   227,
   -1725, -1725, -1725, -1725, -1725, -1725, -1725, -1725, -1725, -1725,
   -1725, -1725, -1725, -1725, -1725, -1725, -1725, -1725, -1725, -1725,
   -1725, -1725, -1725, -1725, -1725, -1725, -1725,     9,    83, -1725,
   -1725, -1725, -1725, -1725, -1725,  3817,  3817,   254, 11921,   264,
     273, -1725, -1725,   317, -1725, -1725, -1725, -1725, -1725, -1725,
   -1725, -1725, -1725,  2563, -1725,   667,   116, -1725, -1725, -1725,
   -1725, -1725, 16208, -1725, -1725,   179,   358,   328,    81, -1725,
    3817,   358,   358,   358,   270,  4268,   523,   827, 12080, -1725,
   -1725, -1725, 16058,  2632, -1725, -1725, -1725,  2647,   536, 12132,
     670,   840,  2647,   890,   438, -1725, -1725, -1725, -1725,   492,
   -1725, -1725, -1725, -1725,   461, -1725, -1725, -1725, -1725, -1725,
     542,   500,   492, -1725,   492,   528, -1725, -1725, -1725, 16764,
    3817, -1725, -1725,  3817, -1725, 11921,   553, 16914, -1725, -1725,
    4380, 17926, -1725,  1090,  1090,   552,  3237, -1725, -1725, -1725,
   -1725,   262, 14194,  3305,   492, -1725, -1725, -1725, -1725, -1725,
   -1725,   603, -1725,   583,   661,   689, -1725,   704, 20810, 15288,
    2634,  2563,   476,   699,   720,   750,   769,   776,   779, -1725,
   -1725, 16966, 10950,   800, -1725, 16501, -1725, -1725, -1725, -1725,
     803, -1725, -1725,   685, -1725, 18866,   947, 19082, -1725,   808,
    3817,   500,   821,   822,   865,   910, -1725, -1725, -1725,  3593,
    3389,   933,   932,   329, -1725, -1725,   492,   492,    80,   105,
     361,    80, -1725,   492,   492, -1725,  3736, -1725, -1725,   896,
     941,  1090, 14088, -1725, -1725, 16208, -1725, -1725,  2647, -1725,
    2792,   438,   996,  1044,   105,  3817,   328, -1725, 13486, -1725,
    1090,  1090,  1004,  1044,   105,  3817, -1725, 13378, -1725, -1725,
    1090, -1725,  1090, -1725,   804,  4532,  3817, -1725,  1552,  1032,
   -1725, -1725, -1725, 16660,   500,   121, -1725, -1725, 17976, -1725,
     932,    82, -1725, 20810, 17926,  4056,  3736, -1725,   410, -1725,
   -1725, -1725, 16914,  3817, -1725,  1028, -1725, -1725, -1725, -1725,
    3817,  3425,   441,   584, -1725,  3817,   583, -1725,   843,   492,
     492,  1046, 17116,   826, 14668, 14246,  2647,  2647, -1725,  2647,
    1090,  2647,  1090, -1725, -1725,   492, -1725,  1058, -1725, 17168,
   -1725, -1725, -1725, 17318,   803, -1725,  1067,   545,   833,  1070,
     438,  1081, -1725,  3237,  1077,   583,  3237,  2165, -1725,  1089,
    1143, 20882,  1111,  1116, 20810, 20954,  1121,  8185, -1725, -1725,
   -1725, -1725, -1725, -1725, 21026, 21026, 15134,  1120,  3622, -1725,
   -1725, -1725, -1725,   148, -1725,   204, -1725,   989, -1725, 20810,
   20810, -1725,  1114,   601,   900,   949,   541,  1040,  1137,  1118,
    1135,  1175,   248, -1725,   734, -1725,  1158, -1725,  1015,  3370,
   15596, -1725, -1725,   708,  1158, -1725, -1725,   795, -1725, -1725,
    2634,  1180,  1190,  1197,  1203,  1206,  1210, -1725, -1725,   430,
    1170, -1725,   809,  1170, -1725, -1725, 16764, -1725,  1052,  1211,
   15750, -1725, -1725,  4687,  3864,  1198, 14668,  1236,   636,   766,
   -1725, -1725, -1725, -1725, -1725,  3817,  4708, -1725, -1725, -1725,
   -1725, -1725, -1725,  6201,  3655,  1120, 18866,  1221,  1227, -1725,
   -1725,  1216, 19082,   710, -1725, -1725, -1725, 19154,  1185, -1725,
   -1725, -1725, -1725, -1725,  3593,   733,  1230,  1234,  1249,   812,
    1263,  1278,  1282,  3389, -1725, -1725,   492,  1241,   328,  1280,
   -1725, -1725,  1286, -1725, -1725,   500,  1044, -1725, -1725, -1725,
     500, -1725, -1725,  3736, -1725, 15596, 15596, -1725,  1090,  4380,
   18704, 14826, -1725, -1725, -1725, -1725, -1725,   500,  1044,    82,
   -1725, -1725,  2647,  1285,  1044,   105, -1725,   500,  1044, -1725,
   15034, -1725,  1090,  1090, -1725, -1725,  1289,   637,  1296,   438,
    1297, -1725, 18134, -1725,   816, -1725,  1375, 18600, -1725,  4380,
   17477, 14088, -1725, 16660, 21098, -1725, -1725, -1725, -1725, -1725,
    4056,   864,  3736, -1725, 14826,   932, 11921, -1725,  1304, -1725,
    1312, -1725, -1725, -1725, -1725, -1725,  3237, -1725, -1725,  1386,
    4628,  1306, 17318, 10950, -1725, 17529, -1725,  1090,  1090, -1725,
   -1725,   803, -1725,   738,  1311,  1449, 20810,  1113,  1286,  1295,
   -1725,   492,   492, -1725,  1170, -1725, 17116, -1725, -1725, 18415,
    1090,  1090, -1725,  4628,   492, -1725, 17783, -1725, -1725, 17168,
   -1725,   262,  1314,  1298,  1313,   833,   834, 16914,   846, -1725,
   -1725, -1725, -1725, -1725, -1725,   863, -1725,  1333,  1309, -1725,
   15442, -1725, 17581, 17581, -1725, 15442, -1725, 20810, -1725, 12132,
   12132, 15442, -1725, -1725, 16712, 17581, 17581,  1015,  1179,  1217,
     597,  1240, -1725,   886,  1336,  1088,  1337, -1725, 19154, 20810,
   19226,  1335, 20810,  1552, 20810,  1552, -1725,  3186, -1725, -1725,
   19298,  1651, 20810, 19298,  1552, -1725, -1725, 20810, 20810, 20810,
   20810, 20810, 20810, 20810, 20810, 20810, 20810, 20810, 20810, 20810,
   20810, 20810, 20810, 20810, 20810, 20810, 19370,  1316,   704,  4520,
   10950, -1725, -1725, -1725, -1725, -1725, -1725, -1725, -1725, -1725,
   -1725, -1725,  1338, 20810, -1725, -1725,   708,  2175, -1725, -1725,
     492,   492, -1725, -1725, 15596, -1725,   457,  1170, -1725,   835,
    1170, -1725, -1725, -1725,  1286, -1725, -1725,  1286, 21170, -1725,
   -1725, 10950,  1339,  1343,  3490,  1481,  2664,   460,  1295, -1725,
     492,   492,  1295,   470, -1725,   492,   492, 20810,  3817,  1128,
    1131,  1295,    33, 14036, 14036,  3817, -1725, -1725, 20810,  1216,
   -1725, 18866,  1352, -1725,  2851, -1725, -1725, -1725, -1725, -1725,
     889, -1725, 14036,  1552,  4380,  1552,   901,  1350,  1353,  1354,
     911,  1357,  1358,  1360,   512,  1170, -1725, -1725,   571,  1170,
   -1725, -1725, -1725,  4380,   704, -1725,  1170, 21170, -1725,   500,
   18134, -1725, -1725,   902,  1362,   920,  1363, -1725,  1355, -1725,
     500, -1725, -1725,   500,  1044,  1355, -1725,   500,  1359,  1364,
    1365, -1725, -1725, 18415, -1725,  1366, -1725, -1725, -1725,  1552,
    3817, 10109,  1447,  1345, 18502, -1725,  1211, -1725, 14036,   930,
   -1725, -1725,  1355, -1725, 16914, 15596,  1347, -1725,  1347, -1725,
   -1725, -1725,   833, -1725, 17168, -1725, 11112, 15904, -1725, 18134,
    1372,  1376,  1377, -1725,  5969,   492, -1725,  1113, -1725, -1725,
   -1725, -1725,  1286, -1725, -1725, -1725,  1090, -1725,  3465, -1725,
   -1725,   438,  1570,  1381, 19442, -1725,   833,  1314, -1725, -1725,
    1373,  1383,  2165, 19298, -1725,  1384,   116,  1385,  1391,  1392,
    1389,  1400, 20810,  1401,  1403,  1404, 10950, 20810, -1725, -1725,
    1461, -1725, -1725, -1725, 20810, -1725,  1405,  1406, 18938,  1133,
   -1725, 19298,  1387, -1725,  1407, -1725, -1725,  4005, -1725, -1725,
     936, -1725, -1725, -1725, -1725,  4005, -1725, -1725,  1140,   491,
   -1725, -1725,  1114,  1114,  1114,   601,   601,   900,   900,   949,
     949,   949,   949,   541,   541,  1040,  1137,  1118,  1135,  1175,
   20810,  1141, -1725,  1408,  4005, -1725, -1725, 18866, -1725, 18134,
    1411,  1413,  1414,  2175, -1725, -1725, -1725, -1725, -1725, -1725,
   -1725, -1725,  1286, -1725, -1725,  1286, 18134, 18134, -1725, -1725,
    3490,   885,  1415,  1418,  1419,  1420,  3086,  2664, -1725, -1725,
   -1725, -1725, -1725, -1725, -1725, -1725, -1725, -1725, -1725, -1725,
   -1725, -1725,  1422, -1725,  1295, -1725, -1725, -1725, -1725, -1725,
   -1725, -1725, -1725,  1421,  1423, -1725,   328,  4005,  1154,   114,
   -1725, -1725,  1410, -1725, 19082, -1725, 20810,   492, 19514, 14036,
   -1725, -1725, -1725,  1412,   574,  1170, -1725,   588,  1170, -1725,
   -1725, -1725, -1725,  1286, -1725, -1725, -1725,  1286,   932,  1426,
    1286, -1725, -1725, -1725, -1725, -1725, -1725, -1725,  1425, -1725,
   -1725,  1355, -1725,   500, -1725, -1725, -1725, -1725, -1725, 12710,
    1427,  1428, -1725,   342, -1725,   472,   290, 10788,  1431, 13865,
    1432,  1435,  3063,  3377,  3166, 19586,  1437, -1725, -1725,  1441,
    1442, -1725, -1725,   500, 20810, 20810,  1579,  1443,   506, -1725,
    1521,  1446,  1430, -1725, -1725, -1725,  9937, -1725, -1725, -1725,
   -1725, -1725,  2352, -1725, -1725, -1725,  1513, -1725, -1725, -1725,
    1552, -1725, -1725, 12557, 16208,  1450, -1725,  3817, -1725,  1433,
    1451,  1455, -1725,  1165, -1725, -1725, -1725, -1725,  4380, -1725,
   -1725,  1438,  1445,   979, 16914,   583,   583,  1314, -1725, -1725,
    1120,  1211, 15750, -1725,  1158, -1725, 11274, -1725,   658,  1170,
   -1725,  1090,  9257, -1725, -1725,   833,   492,   492,   262,  1298,
   -1725, 18866, -1725,  1314,  1468,  1473, -1725, -1725,   981,   691,
   10950,  1552, -1725,   691,  7668,   691, -1725, 20810, 20810, 20810,
   -1725, -1725, -1725, -1725, 20810, 20810,  1466, 18866, -1725, -1725,
    1469,   672, -1725, -1725, -1725,  3957, -1725, -1725,  1172, -1725,
     359, -1725, 19298,  1178, -1725, 19154, -1725, -1725, 20810,  1452,
    1183,  1189,  1216, -1725,   660,  1170, -1725, -1725, 18134, 18134,
   -1725, -1725,  1474,   663,  1170, -1725,   676,  2271,   492,   492,
   -1725, -1725, 18134, 18134, -1725,  1477, -1725, 14826, 14826,  1482,
    1479,  1480,  1486, -1725,  1484, 20810, 20810,  1192,  1487, -1725,
   -1725, -1725, -1725, -1725, -1725, -1725,  1491, 20810, -1725, -1725,
   -1725,  1286, -1725, -1725, -1725,  1286, 18134, 18134,   328,   492,
    1196,  1492,  1497, -1725, -1725,  1498, 12863, 13016, 13169, 16914,
   17581, 17581,  1499, -1725,  1454,  1475,  2364,  7189, -1725,   362,
    3817, -1725, -1725,  3817, -1725, 19010,    19,   134, -1725, -1725,
   -1725, -1725, 20810,  1501,  1569, 10625, 10281, -1725,  1493, -1725,
    1495, 20810,  1510, 18866,  1511, 20810, 19154, 20810,  1064, -1725,
    1515,    24, -1725,    -8,  1502, -1725, -1725,  1506, -1725,  1517,
   -1725,  1520,  1507, 13865,   632, 13644,   492,   386, -1725, -1725,
   -1725,  1504, -1725,  1525, -1725,  1539, -1725,  1540, -1725,  1541,
   -1725, -1725, -1725, -1725,  1550, 11436,  1545,  1548,  1553, -1725,
    1514, -1725, -1725, -1725,  1286, 20810, 20810,  1211,  1554, -1725,
    1314, -1725,  1557,    95, -1725,  1216,  1561, -1725, -1725, 16914,
   -1725,  1571,  1567,   988, -1725,  1568, -1725, -1725, -1725, -1725,
   -1725, 18866,  1216, 19154, -1725,  1608,  4005, -1725,  1608,  1608,
   -1725,  4005,  4090,  4503, -1725, -1725,  1205, -1725, -1725, -1725,
    1581,  1576, -1725, -1725, -1725,  1286, -1725, -1725,  1580,  1582,
     492, -1725, -1725, -1725,  1286, -1725, -1725, -1725,  1585, -1725,
   -1725, -1725, -1725, -1725, -1725, -1725, -1725, -1725, -1725, -1725,
   -1725, -1725,  1578, -1725, -1725, -1725, -1725,  1587,  1588,   492,
   -1725, 18134, 18134, -1725, -1725, -1725, -1725, 20810, -1725, -1725,
    1596, -1725,  1499,  1499,  1499,   849,  1556,   392, -1725,  4207,
     394, 15596, -1725, -1725, -1725,  4149, 20810,  3992,   396, -1725,
   -1725,    46,  1591,  1591,  3817, -1725, -1725, 18283, -1725, 20810,
    1594,  1603, -1725, -1725, -1725, -1725,  1001,  1606, 13865,  1446,
    1605, 20810,   179,  1612,   270, 13328, 16914, -1725, -1725, -1725,
     887, 13865, 20810,   904,   440, -1725, 20810, 18714, -1725, -1725,
     402, -1725,  1216, -1725,  1031,  1047,  1048, -1725, -1725, -1725,
   -1725,   500,  1064,  1616, -1725, -1725, 20810, -1725,  1621,   704,
   10788, -1725, -1725, -1725, -1725, 20810,  1664, -1725,  9765, -1725,
     492, 14826, -1725, -1725, 16914, -1725, -1725, -1725, -1725, -1725,
   -1725,  1619, -1725, 18134, -1725, -1725,  1622, -1725,  1623,  1627,
    1624,   833, -1725,  1630, -1725, -1725, -1725, 20810, -1725,  7668,
   20810,  1216,  1637,  1209, -1725,  1215, -1725,  4005, -1725,  4005,
   -1725, -1725, -1725, -1725, 18134,  1635,  1636, -1725, -1725, 18134,
   18134,  1638,  1640,  1220, 14352, 14510, -1725,  1634, -1725, -1725,
   -1725, -1725,  1641,  1646,  1225, -1725, -1725, -1725, -1725,   849,
    2242,   498, -1725, -1725, -1725, -1725,   492,   492, -1725, -1725,
   -1725,   509, -1725,  1061,  4149,   644, -1725,  3992,   492, -1725,
   -1725, -1725, -1725, -1725, -1725, -1725, -1725,   548, 13865,    56,
   19658,  1725, 13865,  1446, 14984, -1725, -1725, -1725, -1725, 20810,
   -1725, 19730,  1726,  1626, 18790, 19802, 13865, 10453,  1446,   631,
    1147,  1628, 20810, -1725,  1655,   363, 13865, -1725, -1725,  1657,
   -1725, -1725,  1633,   704,   456,  1660,  1666,  1232,  1732, -1725,
   -1725, -1725, -1725,  3817,  4380, -1725, -1725,  1667,  1668, -1725,
   -1725, -1725,   833,  1314, -1725,  1676, -1725, -1725, -1725,  1677,
   -1725, -1725, -1725,  1239,  1242, -1725, -1725, -1725, -1725, -1725,
   -1725, -1725, -1725, -1725, -1725,  1680, -1725, -1725,  1685,  1688,
   -1725, -1725, -1725,  1689,  1693,  1697,  2242, -1725,   492, -1725,
   -1725, -1725, -1725, -1725,  1698,  4207, -1725, -1725,  5781,   125,
   11601, -1725, 13747, -1725,    13,  1063, 13865,  1782,   557,  1692,
     271, 13865, 20810,  1706,   631,  1147,  1686, 21242,  1696,   295,
    1789, -1725, 19874, 19946, 20810,  1446,  1690, 11762, -1725, -1725,
   -1725, 17731, -1725,  1709,  1694,    35, 13865, -1725, 20810, 19298,
     378, -1725, -1725, -1725,  1718, -1725, -1725,  1314,  1722, -1725,
   -1725, -1725, -1725,  1721,  1723,  1728, 14826,  1727, -1725, -1725,
     679,  1170, -1725, -1725,   849, -1725, -1725,   276, -1725,   229,
   -1725, -1725, -1725,  1729, 12239, -1725, -1725, 13865, -1725,    48,
   -1725, 13865, 20810,  1731, 20018, -1725, -1725, 20090, 20162, 20810,
    1706,  1446, 20234, 20306, 13865,  1719,   387,  1720,   459, -1725,
   -1725,  1735, 12239, 17731, -1725,  4241, 17529,  1552,  1733, -1725,
    1787,  1742,   600,  1737, -1725,  1821, -1725,  1066, 13865,  1746,
   13865, 13865, -1725,  1750, -1725, -1725, -1725, -1725, -1725, -1725,
   -1725, -1725,  1286, -1725, 20810, -1725, 20810, -1725, -1725,  1323,
   12398, -1725, -1725, 13865, -1725, -1725,  1446, -1725, -1725,  1446,
    1738,   534,  1740,   550, -1725, -1725,  1446, -1725,  1446, -1725,
    1758, 20378, 20450, 20522, -1725,  1323, -1725,  1741,  2827,  2905,
   -1725, -1725, -1725,    35,  1755, 20810,  1749,    35,    35, 13865,
   -1725, -1725, 20810,  1805,  1809, -1725, 18134, -1725, -1725, 13747,
   -1725,  1323, -1725, -1725,  1774, 20594, 20666, 20738, -1725, -1725,
    1446, -1725,  1446, -1725,  1446, -1725,  1741, 20810,  1775,  2905,
    1771,   704,  1778, -1725,   714, -1725, -1725,  1068,  1732,   398,
   -1725, -1725,  9459,  1783, 13747, -1725, -1725,  1446, -1725,  1446,
   -1725,  1446,  1784,  1779, -1725,   500,   704,  1785, -1725,  1759,
     704, -1725, -1725, 13865,  1865,  1788, -1725, -1725, -1725,  9641,
   -1725,   500, -1725, -1725,  1247, 20810, -1725,  1086, -1725, 13865,
   -1725, -1725,   704,  1552,  1790,  1769, -1725, -1725, -1725,  1117,
   -1725, -1725,  1772,  1552, -1725, -1725
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,   452,     0,     2,   452,   469,   470,   471,   472,   473,
     474,   475,   476,   458,   460,   459,   461,     0,     0,     0,
     477,   479,   500,   480,   501,   483,   484,   498,   499,   478,
     496,   497,   481,   482,   485,   486,   487,   488,   489,   490,
     491,   492,   493,   494,   495,   502,   503,   791,   505,   578,
     579,   582,   584,   580,   586,     0,     0,     0,   452,     0,
       0,    16,   549,   555,     9,    10,    11,    12,    13,    14,
      15,   755,    97,     0,    19,     0,     2,    95,    96,    17,
      18,   807,   452,   756,   401,     0,   404,   681,   406,   415,
       0,   405,   435,   436,     0,     0,     0,     0,   532,   454,
     456,   462,   452,   464,   467,   517,   504,   440,   510,   515,
     441,   527,   442,   542,   546,   552,   531,   558,   570,   791,
     575,   576,   559,   626,   407,   408,     3,   757,   770,   457,
       0,     0,   791,   829,   791,     2,   846,   847,   848,   452,
       0,  1005,  1006,     0,     1,   452,     0,   452,   424,   425,
       0,   532,   446,   447,   448,   760,     0,   581,   583,   585,
     587,     0,   452,     0,   792,   793,   577,   506,   674,   675,
     673,   734,   729,   719,     0,     0,   758,     0,     0,   452,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   550,
     553,   452,   452,     0,  1007,   532,   836,   854,  1011,  1004,
    1002,  1009,   400,     0,   159,   687,   158,     0,   409,     0,
       0,     0,     0,     0,     0,     0,   399,   906,   907,     0,
       0,   434,   789,   791,   785,   810,   791,   791,   787,     2,
     791,   786,   867,   791,   791,   864,     0,   525,   526,     0,
       0,   452,   452,   469,     2,   452,   416,   455,   465,   518,
       0,   547,     0,   773,     2,     0,   681,   417,   532,   511,
     528,   543,     0,   773,     2,     0,   468,   512,   519,   520,
     529,   534,   544,   548,     0,   562,     0,   749,     2,     2,
     771,   828,   830,   452,     0,     2,     2,  1015,   532,  1018,
     789,   789,     3,     0,   532,     0,     0,   427,   791,   787,
     786,     2,   452,     0,   753,     0,   715,   717,   716,   718,
       0,     0,   711,     0,   701,     0,   710,   721,     0,   791,
     791,     2,   452,  1026,   453,   452,   464,   443,   510,   444,
     535,   445,   542,   539,   560,   791,   561,     0,   662,   452,
     663,   980,   981,   452,   664,   666,   549,   555,     0,   627,
     628,     0,   794,     0,   732,   720,     0,   798,    21,     0,
      20,     0,     0,     0,     0,     0,     0,    23,    25,     4,
       8,     5,     6,     7,     0,     0,   452,     2,     0,    98,
      99,   100,   101,    82,    24,    83,    38,    81,   102,     0,
       0,   117,   119,   123,   126,   129,   134,   137,   139,   141,
     143,   145,   147,   150,     0,    26,     0,   556,     2,   102,
     452,   151,   726,   677,   546,   679,   725,     0,   676,   680,
       0,     0,     0,     0,     0,     0,     0,   808,   834,   791,
     844,   852,   856,   862,     2,  1013,   452,  1016,     2,    95,
     452,     3,   661,     0,  1026,     0,   453,   510,   535,   542,
       3,     3,   643,   647,   657,   663,   664,     2,   837,   855,
    1003,     2,     2,    23,     0,     2,   687,    24,     0,   685,
     688,  1024,     0,     0,   694,   683,   682,     0,     0,   775,
       2,     2,     2,     2,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   813,   870,   791,     0,   681,     2,
     809,   817,   933,   811,   812,     0,   773,     2,   866,   874,
       0,   868,   869,     0,   430,   452,   452,   516,   453,     0,
     532,   452,  1008,  1012,  1010,   533,   753,     0,   773,   789,
     410,   418,   466,     0,   773,     2,   753,     0,   773,   730,
     513,   514,   530,   545,   551,   554,   549,   555,   573,   574,
       0,   731,   452,   671,     0,   196,   393,   452,     3,     0,
     532,   452,   772,   452,     0,   412,     2,   413,   750,   432,
       0,     0,     0,     2,   452,   789,   452,   753,     0,     2,
       0,   714,   713,   712,   707,   463,     0,   705,   722,   508,
       0,     0,   452,   452,   982,   453,   449,   450,   451,   986,
     977,   978,   984,     2,     2,    96,     0,   942,   956,  1026,
     938,   791,   791,   947,   954,   669,   452,   540,   665,   453,
     536,   537,   541,     0,   791,   992,   453,   997,   989,   452,
     994,     0,  1024,   633,     0,     0,     0,   452,     0,   806,
     805,   801,   803,   804,   802,     0,   796,   799,     0,    22,
     452,    89,   452,   452,    84,   452,    91,     0,    32,     0,
      33,   452,    87,    88,   452,   452,   452,     2,    98,    99,
       0,     0,   177,     0,     0,   576,     0,  1002,     0,     0,
       0,     0,     0,     0,     0,     0,    50,     0,    56,    57,
      61,     0,     0,    61,     0,    85,    86,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     452,   160,   161,   162,   163,   164,   165,   166,   167,   168,
     169,   170,   158,     0,   156,   157,     2,   918,   678,   915,
     791,   791,   923,   557,   452,   835,   791,   845,   853,   857,
     863,     2,   838,   840,   842,     2,   858,   860,     0,  1014,
    1017,   452,     0,     0,     2,    96,   942,   791,  1026,   888,
     791,   791,  1026,   791,   903,   791,   791,     3,   665,     0,
       0,  1026,  1026,   452,   452,     0,     2,   696,     0,  1024,
     693,  1025,     0,   689,     0,     2,   692,   695,   174,   173,
       0,     2,   452,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   791,   822,   826,   865,   791,   879,
     884,   814,   871,     0,     0,   438,   930,     0,   776,     0,
     452,   777,   431,     0,     0,     0,     0,   429,     2,   778,
       0,   414,   753,     0,   773,     2,   779,     0,     0,     0,
       0,   588,   650,   453,     3,     3,   654,   653,   849,     0,
       0,   452,   394,     0,   532,     3,    95,     3,   452,     0,
       3,   754,     2,   709,   452,   452,   703,   702,   703,   509,
     507,   627,     0,   988,   452,   993,   453,   452,   979,   452,
       0,     0,     0,   957,     0,   791,  1027,   943,   944,   670,
     940,   941,   955,   983,   987,   985,   538,   573,     0,   991,
     996,   630,  1025,     0,     0,   629,     0,  1024,   735,   733,
       0,     0,   798,    61,   759,     0,     2,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   452,     0,   116,   115,
       0,   112,   111,    27,     0,    28,     0,     0,     0,     0,
       3,    61,     0,    46,     0,    47,    54,     0,    53,    65,
       0,    62,    63,    66,    49,     0,    48,    52,     0,     0,
      45,   118,   120,   121,   122,   124,   125,   127,   128,   132,
     133,   130,   131,   135,   136,   138,   140,   142,   144,   146,
       0,     0,   403,     0,     0,    29,     3,   687,   152,   452,
       0,     0,     0,   919,   920,   916,   917,   728,   727,     2,
     839,   841,   843,     2,   859,   861,   452,   452,   935,   934,
       2,     0,     0,     0,     0,     0,   791,   943,   891,   908,
       2,   886,   894,   667,   889,   890,   668,     2,   901,   911,
     904,   905,     0,     3,  1026,   422,     2,  1019,     2,   658,
     659,   637,     3,     3,     3,     3,   681,     0,   150,     0,
       3,     3,     0,   690,     0,   684,     0,   791,     0,   452,
       3,   426,   428,     0,   791,   823,   827,   791,   880,   885,
       2,   815,   818,   820,     2,   872,   875,   877,   789,     0,
     931,     3,   781,     3,   522,   521,   524,   523,     2,   754,
     782,     2,   780,     0,   754,   783,   588,   588,   588,   452,
       0,     0,   672,     0,   397,     0,     0,   452,     0,     2,
       0,     0,     0,     0,     0,   179,     0,   327,   328,     0,
       0,   366,   365,     0,   154,   154,   372,   549,   555,   193,
       0,   180,     0,   204,   181,   182,   452,   198,   183,   184,
     185,   186,     0,   187,   188,   333,     0,   189,   190,   191,
       0,   192,   200,   532,   452,     0,   202,     0,   391,     0,
       0,     0,     3,     0,   761,   754,   742,   743,     0,     3,
     738,     3,     3,     0,   452,   719,   719,  1024,   990,   995,
       2,    95,   452,     3,   547,     3,   453,     3,   791,   950,
     953,   452,     3,   939,   945,     0,   791,   791,     0,   633,
     617,   687,   634,  1024,     0,     2,   795,   797,     0,    90,
     452,     0,    94,    92,   452,     0,   106,     0,     0,     0,
     110,   114,   113,   178,     0,     0,     0,   687,   103,   171,
       0,     0,    41,    42,    79,     0,    79,    79,     0,    67,
      69,    44,     0,     0,    40,     0,    43,   149,     0,     0,
       0,     0,  1024,     3,   791,   926,   929,   921,   452,   452,
       3,     3,     0,   791,   897,   900,   791,     0,   791,   791,
     892,   909,   452,   452,  1020,     0,   660,   452,   452,     0,
       0,     0,     0,   411,     3,     0,     0,     0,     0,   686,
     691,     3,   774,   176,   175,     3,     0,     0,     2,   816,
     819,   821,     2,   873,   876,   878,   452,   452,   681,   791,
       0,     0,     0,   754,   784,     0,   452,   452,   452,   452,
     452,   452,   571,   599,     3,     3,   600,   532,   589,     0,
       0,   831,     2,     0,   395,    61,     0,     0,   318,   319,
     201,   203,     0,     0,     0,   452,   452,   314,     0,   312,
       0,     0,     0,   687,     0,     0,     0,     0,     0,   155,
       0,     0,   373,     0,     0,     3,   208,     0,   199,     0,
     309,     0,     0,     2,     0,   532,   791,     0,   392,   937,
     936,     0,     2,     0,   745,     2,   740,     0,   741,     0,
     723,   704,   708,   706,     0,   452,     0,     0,     0,     3,
       0,     2,   946,   948,   949,     0,     0,    95,     0,     3,
    1024,   623,     0,   633,   631,  1024,     0,   620,   736,   452,
     800,     0,     0,     0,    34,     0,   107,   109,   108,   105,
     104,   687,  1024,     0,    60,    76,     0,    70,    77,    78,
      55,     0,     0,     0,    64,    51,     0,   148,   402,    30,
       0,     0,     2,   922,   924,   925,     3,     3,     0,     0,
     791,     2,   893,   895,   896,     2,   910,   912,     0,   887,
     902,     3,     3,  1021,     3,   645,   644,   648,  1023,     2,
       2,  1022,     0,     3,   788,   697,   698,     0,     0,   791,
     433,   452,   452,     3,     3,   439,   790,     0,   881,   765,
       0,   767,   571,   571,   571,   606,   576,     0,   612,   600,
       0,   452,   563,   598,   594,     0,     0,     0,     0,   601,
     603,   791,   614,   614,     0,   595,   610,   452,   398,     0,
       0,    62,   322,   323,   320,   321,     0,     0,     2,   219,
       0,     0,   221,   406,   220,   532,   452,   300,   299,   301,
       0,     2,   179,   259,     0,   252,     0,   179,   315,   313,
       0,   307,  1024,   316,     0,     0,     0,   354,   355,   356,
     357,     0,   347,     0,   348,   324,     0,   325,     0,     0,
     452,   210,   197,   311,   310,     0,   345,   364,     0,   396,
     791,   452,   763,   724,   452,     2,     2,   621,   998,   999,
    1000,     0,   951,   452,     3,     3,     0,   959,     0,     0,
       0,     0,   632,     0,   619,     3,    93,     0,    31,   452,
       0,  1024,     0,     0,    80,     0,    68,     0,    74,     0,
      72,    39,   153,   927,   452,     0,     0,   832,   850,   452,
     452,     0,     0,     0,   452,   452,   700,     0,   419,   421,
       3,     3,     0,     0,     0,   769,   567,   569,   565,     0,
     966,     0,   607,   971,   609,   963,   791,   791,   593,   613,
     597,     0,   596,     0,     0,     0,   616,     0,   791,   590,
     604,   615,   605,   611,   652,   656,   655,     0,     2,     0,
       0,   240,     2,   222,   532,   305,   303,   306,   302,     0,
     304,     0,   248,     0,   179,     0,     2,   452,   260,     0,
     285,     0,     0,   308,     0,     0,     2,   331,   358,     0,
     349,     2,     0,     0,     0,     0,   336,     0,   332,   195,
     194,   420,   739,     0,     0,  1001,     3,     0,     0,   958,
     960,   622,     0,  1024,   635,     2,    37,    35,    36,     0,
      58,   172,    71,     0,     0,     3,   833,   851,     3,     3,
     898,   913,   423,     2,   642,     3,   641,   699,     0,     0,
     824,   882,   932,     0,     0,     0,   967,   968,   791,   592,
     964,   965,   591,   572,     0,     0,   209,   330,     0,     0,
       0,   233,     2,   211,     0,     0,     2,   242,   257,   268,
     262,     2,   179,   297,     0,   272,     0,     0,   263,   261,
     250,   253,     0,     0,   179,   286,     0,     0,   214,   329,
       2,   452,   326,     0,     0,   374,     2,   334,     0,    61,
       0,   346,   744,   746,     0,   961,   962,  1024,     0,   737,
      59,    75,    73,     0,     0,     0,   452,     0,   825,   883,
     791,   974,   976,   969,     0,   602,   228,   223,   226,     0,
     225,   232,   231,     0,   452,   235,   234,     2,   244,     0,
     241,     2,     0,     0,     0,   249,   254,     0,     0,   179,
     298,   273,     0,     0,     2,     0,   288,   289,   287,   256,
     317,     0,   452,   452,     3,   359,   453,   363,     0,   367,
       0,     0,     0,   375,   376,   217,   337,     0,     2,     0,
       2,     2,   952,     0,   625,   928,   899,   914,   646,     2,
     970,   972,   973,   608,     0,   230,     0,   229,   213,   236,
     452,   387,   245,     2,   246,   243,   258,   271,   269,   265,
     277,   275,   276,   274,   255,   270,   266,   267,   264,   251,
       0,     0,     0,     0,   216,   236,     3,   352,     0,   966,
     360,   361,   362,   374,     0,     0,     0,   374,     0,     2,
     335,   342,     0,   339,   341,   624,   452,   224,   227,     2,
       3,   237,   388,   247,     0,     0,     0,     0,   296,   294,
     291,   295,   292,   293,   290,     3,   352,     0,     0,   967,
       0,     0,     0,   368,     0,   377,   218,     0,   332,     0,
       3,   205,     0,     0,     2,   284,   282,   279,   283,   280,
     281,   278,     0,     0,   353,     0,   380,     0,   378,     0,
     380,   338,   340,     2,     0,     0,   207,   206,   212,     0,
     215,     0,   350,   381,     0,     0,   369,     0,   343,     2,
     975,   351,     0,     0,     0,     0,   344,   382,   383,     0,
     379,   370,     0,     0,   371,   384
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1725,  6062,  5393, -1725,    -1,   289,  1390,  -165, -1725,  1572,
   -1725,   319, -1725,  -682,   604,   700,  -903, -1028, -1725,   160,
    6259,  1924, -1725,  1610, -1725,  1283,   196,   719,   706,   299,
     716,  1243,  1244,  1245,  1246,  1248, -1725,  -141,  -108,  8084,
     824, -1725,  1560, -1725, -1725,  -658,  7219, -1074,  1348, -1725,
    -100, -1725,   811,   -63, -1725, -1725, -1725,   375,    30, -1725,
   -1724, -1585,   246,     8, -1725, -1725, -1725,   258, -1503, -1725,
   -1324, -1725, -1725, -1725, -1725,   -42, -1703,   139, -1725, -1725,
     -37, -1725, -1725, -1725,   -23,   403,   404,    86, -1725, -1725,
   -1725, -1725,  -833, -1725,     6,   -50, -1725,    90, -1725,  -177,
   -1725, -1725, -1725,   828,  -659,  -893, -1319, -1725,    26, -1057,
      62,  1994,  -858,  -819, -1725,  -260, -1725,    67,  -144,   390,
    -300,  -228,  3714,  6987,  -600, -1725,   190,   182,   974,  2003,
   -1725,  1945, -1725,   255,  3943,  -294, -1725, -1725,   194, -1725,
   -1725,   508,   350,  4504,  2803,   -49,  1748,  -308, -1725, -1725,
   -1725, -1725, -1725,  -399,  1177,  4845, -1725,  -361,    -9, -1725,
     482,   208, -1725,   143,   678, -1725,   478,  -101, -1725, -1725,
   -1725,  5175,  -594, -1151,  -696,  -613,  -415,  1107, -1725, -1131,
    -153,   198,  1597,   852,  4413,   -35,  -471,  -248,  -186,  -449,
    1222, -1725,  1543,   566,  1136,  1439, -1725, -1725, -1725, -1725,
     252,  -163,  -102,  -850, -1725,   322, -1725, -1725,   589,   413,
   -1725, -1725, -1725,  2024,  -737,  -414,  -914,   -30, -1725, -1725,
   -1725, -1725, -1725, -1725,  -146,  -781,  -136, -1694,  -198,  7226,
     -66,  6563, -1725,  1105, -1725,  3418,   -47,  -200,  -172,  -159,
       1,   -72,   -68,   -67,   619,   102,   104,   124,  -152,   -91,
    -121,  -109,   -95,  -720,  -716,  -646,  -634,  -701,  -110,  -627,
   -1725, -1725,  -661,  1292,  1293,  1294,   275,  7334,  -551,  -563,
    -561,  -498,  -683, -1725, -1638, -1635, -1631, -1620,  -589,  -112,
    -310, -1725, -1725,   -13,   259,   -64, -1725,  7728,    79,   202,
    -548
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,  1140,   213,   383,   384,    80,    81,   385,   360,   386,
    1433,  1434,   387,   960,   961,   962,  1248,  1249,  1250,  1445,
     409,   389,   390,   391,   670,   671,   392,   393,   394,   395,
     396,   397,   398,   399,   400,   401,   402,   411,  1059,   672,
    1370,   733,   207,   735,   405,   800,  1141,  1142,  1143,  1144,
    1145,  1146,  1147,  2022,  1148,  1149,  1375,  1550,  1868,  1869,
    1801,  1802,  1803,  1990,  1991,  1150,  1564,  1565,  1566,  1710,
    1711,  1151,  1152,  1153,  1154,  1155,  1156,  1383,  1737,  1921,
    1841,  1157,  1158,  1582,  2008,  1583,  1584,  1904,  1159,  1160,
    1161,  1373,  1912,  1913,  1914,  2054,  2069,  1939,  1940,   284,
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
      79,   183,   131,    79,   231,   184,   185,   181,   530,   337,
     355,   969,   359,   517,   297,   675,   617,   789,   323,   486,
     949,   476,   594,   558,   498,  1183,   190,   904,   176,   627,
     148,   280,  1787,   630,  1783,  1840,  1552,   403,  1784,   625,
     890,   917,   891,   628,   834,   836,  1028,   487,  1022,  1785,
     340,  1364,   506,   351,    79,    79,   898,    79,  1424,   131,
     488,   899,  1253,    89,  1722,  1029,   149,   489,    95,   198,
     404,   942,    79,   660,  1588,   289,  1004,   528,  1876,    57,
    1166,    79,   505,   196,  1032,   510,  1877,   538,  1055,    79,
    1039,  1260,   565,   567,    79,   486,   228,    79,   490,   253,
     514,    79,    19,   263,  1870,   892,  1070,   527,  1104,   421,
     491,   479,   838,   422,   423,   292,    57,   537,  1023,  1586,
      89,  1943,   845,   487,   492,    95,  1871,   437,   256,   495,
    1024,  1798,  1799,  -747,   198,  1175,   488,  1025,   617,    79,
     594,  1589,    79,   489,    79,   144,   131,   183,  1863,    79,
     358,   184,   185,   484,  1294,    79,  1485,  1486,   162,   870,
     246,   201,    79,   872,   257,  -748,   919,   209,   209,   660,
    1053,  1053,  1172,   494,   490,   186,   279,   187,   497,    79,
      79,   196,   566,   103,   562,  1322,   491,  1878,  1910,  1053,
    1325,   102,  -389,  1542,    79,   111,   458,   188,  1587,   499,
     492,   890,  1162,   891,   467,   495,   606,    89,  -390,    79,
     600,  1817,    95,  1800,   588,   898,  1333,  1447,    79,    79,
    1033,   196,  1944,   183,  1036,   545,   201,   184,   185,   570,
    1935,   566,   523,  1049,  1050,    79,   210,  -773,  -773,  1715,
     103,  1218,  1870,   155,    79,  1295,   196,   274,   102,   494,
     825,  1334,   111,   588,    79,  1053,   107,    79,   419,   534,
     140,  1393,  -389,   140,    79,  1876,   892,  1971,   914,  1241,
     191,  1552,  1622,  1008,    79,    79,   674,    79,  -390,  1296,
     248,  1872,   885,   424,   807,   425,   793,   617,  1187,   523,
    1335,   279,   883,   146,    79,    79,   260,   196,  1553,  1553,
    1876,   682,    79,   865,  1022,   426,   683,  1280,  1544,    79,
      79,   617,   808,   107,    79,  1840,   903,   140,   617,   910,
     531,   186,  1213,   187,   524,   809,  1281,   103,   600,   909,
     967,  1787,   810,  1783,   773,   102,  1351,  1784,  1032,   111,
    1232,   676,  1267,   188,   170,   170,  1204,    79,  1785,  1346,
    1347,   112,    79,   202,  1989,    79,   644,   684,   844,   156,
     829,   140,   685,   811,  1323,  1166,    62,    63,  1054,  1054,
     807,  1863,   760,  1305,  1023,   812,   161,   832,   715,   170,
    1989,   524,   840,   837,  1936,  1937,  1024,  1054,   843,   813,
     454,    96,   847,  1272,   150,  1822,  1823,   198,   808,    57,
     107,   602,   822,   175,   140,   828,  2024,  1934,   112,  1510,
     831,   809,   421,   177,    75,   190,   422,   423,   810,    79,
     716,   458,   178,  1636,  1638,  1640,   944,   839,  1101,   170,
     323,    57,   170,  1333,  1333,  1333,  1884,   846,  1798,  1799,
     532,  1296,    79,    79,   216,   170,   821,   279,    96,   811,
     944,  1918,   349,  1054,    79,    79,  1053,   890,  1350,   891,
    1893,   812,   340,    79,  1348,   467,   179,  1382,  1334,  1334,
    1334,  2043,   194,   561,   204,   813,  1204,   111,   278,  1452,
      57,   822,   499,    79,  1919,   205,  1130,  1162,  1713,   201,
    1887,  1888,    79,  1721,   458,   112,  1286,  1343,   421,   170,
      57,   206,   422,   423,   600,   509,   248,  1335,  1335,  1335,
     507,  1453,    79,    -3,   499,  1413,  1344,  1534,    79,   602,
    1827,   870,   892,  1485,  1486,   821,   236,    57,   157,   287,
      57,   158,   159,  1553,   160,    96,  1535,   194,   107,  -446,
      57,  1343,   944,  1635,   170,   617,   871,  1679,  1262,  1534,
     674,  1688,  1961,   993,   170,   674,    79,   944,    79,   573,
    1599,   674,    57,   499,   548,   170,  1680,   553,  1682,    79,
    1689,    79,   581,   458,  1189,    79,  1723,   131,   617,   751,
     674,   435,    57,   499,  1188,    79,   424,   539,   425,    79,
     274,  1716,   170,  1477,   419,   419,  1717,  1456,   551,   170,
     170,   582,   583,   904,   170,  1063,  1009,  1834,   426,  1030,
     499,  1420,  1835,   604,   944,   925,   276,   927,   928,  1037,
     929,  1345,    79,   604,  1963,   278,   931,   427,   248,   933,
     934,   935,   519,   112,    79,   522,   403,   170,    89,  1256,
    1816,    57,   170,    95,    57,   170,  1252,  1058,    13,    14,
      15,    16,    17,  1788,  1054,   179,   773,   279,    57,  1089,
    1553,  1080,  -675,  1540,  1688,   499,   705,   706,  1072,  1043,
     545,  1683,  1789,    96,   243,     6,     7,     8,     9,    10,
      11,    12,    79,  1792,    79,  -389,    79,  1088,   742,   944,
      79,   278,   522,    79,   179,   905,  1551,  1567,  1103,  1995,
     448,   197,  -907,   944,   753,   944,    57,   756,  1575,  -762,
     707,   708,   944,  1401,   229,  1997,   419,   254,    79,   454,
    1084,   264,  1796,  1308,   499,  1928,   293,   499,    57,  1092,
      57,  1882,   170,    57,  1763,   585,  1764,  1312,  1886,   586,
    1100,   499,   248,  1102,   170,   170,    57,  1105,   937,    57,
    1899,  1976,   353,   561,   311,   260,  1977,   111,   103,   938,
     939,   698,  1425,    79,   509,    79,   102,  -450,   699,   700,
     111,   189,    63,   403,  1705,  1706,  1707,    79,    13,    14,
      15,    16,    17,   887,    79,  1633,   179,   859,  1442,   279,
     467,   454,   323,    79,  -568,   427,  1708,   499,  1207,   197,
     194,   532,    79,    79,    79,   870,  1212,  1411,  1293,  1462,
     356,   604,  1471,   499,   459,  1954,   499,  1553,   107,   358,
    1444,  -437,    79,   419,   340,  1475,   759,  1252,  1929,   604,
     597,   107,   499,   620,   913,   140,    57,    72,   357,   197,
     462,  1236,   631,  -447,  -437,  1553,   146,   597,  1237,  1257,
     428,   597,    13,    14,    15,    16,    17,   736,    79,    79,
     467,   499,   794,   795,   197,  2039,   796,    72,    77,    78,
    2040,   429,  1484,   237,   238,   170,   239,   535,  1300,   881,
     240,    79,   278,  1553,   427,   717,   499,   603,   617,   718,
    1318,   604,   903,  -448,   972,   973,   974,  -451,    77,    78,
    1176,   430,    13,    14,    15,    16,    17,    79,   544,    63,
      57,    79,   907,   112,  1572,    79,  1279,   773,   454,   636,
     431,   644,   638,    89,   170,  1058,   112,   432,    95,   157,
     433,  1551,   158,   159,    61,   160,  1177,   168,   169,    64,
      65,    66,    67,    68,    69,    70,   743,   150,   597,   457,
     744,   887,   461,    96,   419,    72,    79,   477,   755,   454,
      57,   507,   499,   817,    79,   499,    96,   858,   475,  1516,
     480,   859,   953,  1415,   955,   603,   958,   481,    72,   604,
     966,   454,   454,   970,  1013,   918,    77,   605,   499,   586,
     870,  1062,  1631,    79,   589,   274,   467,   920,  1669,   606,
     454,   586,   499,  1324,   979,   980,   981,   982,   995,    77,
      78,   742,   742,   573,   921,   427,  1349,   499,   922,    79,
     482,  1011,   355,   355,  1014,    79,    79,  1753,   497,   448,
    1705,  1706,  1707,  1368,  1030,   248,   427,   943,   604,   459,
    1067,   944,  1394,   103,  1068,   515,   532,  1705,  1706,  1707,
     278,  1164,  1708,  1094,   499,   111,    79,   944,  1567,   944,
     507,  1709,   701,   702,   499,   483,   454,   170,   248,  1708,
    1505,  1096,   703,   704,   170,   944,   323,   266,  1714,   573,
     600,   267,   448,   499,   270,   509,   272,  1251,   496,  1082,
     516,  1252,  1071,  1086,  1073,  1554,  1340,  1326,  1327,  1328,
     597,   448,    13,    14,    15,    16,    17,   686,   340,   687,
     688,   689,   459,  1666,  1667,  1668,   107,  1457,   467,  1214,
     140,    79,    79,    79,   597,    13,    14,    15,    16,    17,
    1400,   209,  1430,   140,   744,  1487,  1252,   597,   690,  1628,
    2010,   691,   692,  1629,  2014,   467,   693,   694,  1112,   170,
     170,    79,  1699,   526,   403,   403,   944,  1917,  1847,    79,
      57,   536,    79,    79,   253,   263,    79,   709,   710,   191,
     678,   170,  1577,  1578,  1579,  1580,  1581,    79,   118,    89,
     555,   118,  1725,    57,    95,   577,   944,  1493,  1494,  1435,
     256,  -116,  -116,  -116,  -116,  -116,  -116,  1206,  1726,  1727,
     592,   170,  1068,   944,    79,   170,   592,   678,    89,   624,
     467,   112,  1793,    95,  1879,  1941,   744,  1980,   944,  2041,
      79,  1252,   266,   944,  -906,   246,   257,  -618,   448,  -115,
    -115,  -115,  -115,  -115,  -115,   118,   467,  2065,   635,  1775,
     648,  2062,    72,  1941,    79,  1533,  1543,  1545,   946,   947,
     637,    96,    13,    14,    15,    16,    17,   941,   649,   118,
     652,   323,   603,   419,  1178,   653,   604,   454,  2072,   448,
     657,   905,  2073,    77,   605,   678,    79,   712,   742,   118,
     697,  1992,  1045,  1046,  1597,  1047,  1048,  1239,  1068,  1532,
    1705,  1706,  1707,   340,  1254,  1255,   944,  1258,   711,   103,
     266,   267,   944,   621,   714,   272,   713,  1164,  -151,  -151,
     719,   111,  1708,  1340,  1340,  1340,   118,  1518,  1340,  1047,
    1392,  -180,   118,   434,   118,   486,  1450,  1451,   103,   777,
    1554,   745,  1455,  1451,   801,    79,  1164,  1459,  1451,    79,
     111,   746,    79,  1019,  1443,   248,  1495,  1443,   747,  1310,
    1019,  1507,  1314,   487,   748,   148,   118,   749,   260,  1641,
    1068,   750,   467,  1761,  1068,    -3,   488,  -449,   118,  1762,
    1451,   791,   107,   489,  1772,  1773,   140,   -17,   532,  1782,
     944,   814,   467,   790,    79,   815,   534,  1838,  1839,  1404,
     824,   149,   597,  1851,  1451,   620,  1852,  1451,  1798,  1799,
     816,   107,  2062,  2063,   490,   140,  1448,  1449,   170,   977,
     978,   170,   170,   170,   818,  1426,   491,    89,    89,   118,
     975,   976,   118,   140,  1734,   983,   984,   118,  1533,   819,
     492,  1691,  1691,   820,   826,   170,   495,  1402,  1403,   286,
     467,   170,   842,   860,   448,    79,  -566,   531,  1487,   553,
      79,    79,    79,  -564,   851,  1176,   170,   323,   873,  1684,
     118,   875,   879,   882,  1460,   893,   895,   112,   606,   912,
     916,   914,  1532,    13,    14,    15,    16,    17,  1231,   118,
     494,  1728,   923,   924,   807,   454,   454,   945,   948,   340,
     992,  1177,   951,  1018,   170,   997,   112,  1019,  1026,  1336,
    1065,  1074,  1487,  -751,  1075,  1076,   266,    96,  1077,  1078,
    1431,  1079,   808,  1095,  1097,  1167,  1106,  -651,    79,  1168,
    1184,  1107,  1108,  1198,    79,   809,    79,  1199,  1200,  1210,
    1215,  1905,   810,    79,  1216,  1219,    96,   103,   103,  1464,
    1221,  1242,  1222,  1223,  1224,  1556,  1556,   467,  1473,   111,
     111,  1225,  1227,   118,  1228,  1229,  1234,  1235,  1299,  1259,
     467,  1243,  1264,   811,  1265,  1266,  1273,   532,  1833,  1274,
    1275,  1276,  -639,  -752,  -638,   812,  1284,  1319,  1341,  1208,
    1352,  1355,   256,  1342,  1356,  1307,  1365,   118,   140,   813,
    1366,  1367,  1372,  1374,  1435,   468,   822,   467,   617,  -674,
     419,   944,  1382,  1905,  1376,  1389,  1386,  1388,  1843,  1390,
     107,   107,  1396,   118,   140,   140,  1427,   246,   257,  1398,
      79,  1428,  1619,  1441,  1443,  1470,  1458,  1623,  1523,   170,
    1176,  1483,   170,  1488,  1489,  1490,    79,  1491,    79,  1451,
     821,  1496,  1499,  1508,  1632,  1509,  1511,  1547,  1521,  1524,
    1345,  1590,    89,    61,  1592,  1600,  1595,   403,    64,    65,
      66,    67,    68,    69,    70,  1612,  1177,  1568,    18,  1569,
    1911,    61,   170,  1602,   168,   169,    64,    65,    66,    67,
      68,    69,    70,    79,  1571,  1573,    79,  1603,   140,  1585,
    1867,  1593,   118,   118,  1594,  1605,  1606,   467,  1607,  1608,
     597,   467,  1609,  1487,  1533,   112,   112,  1610,  1617,  1624,
      51,    52,    53,    54,  1621,   467,  1336,  1336,  1336,   150,
    1515,  1519,  1626,  1627,  1630,   467,  1634,  1643,   448,  1642,
    1678,  1647,  1656,  1648,   118,  1695,   427,   248,   118,  1658,
     118,  1495,    79,    79,  1665,    96,    96,  1526,  1532,  1252,
     260,    79,    61,   118,  1698,  1700,  1702,    64,    65,    66,
      67,    68,    69,    70,   964,  1731,   531,   210,   486,  1970,
    1733,  1738,   103,  1745,  1724,  1751,  1749,  1750,  1754,    89,
    1556,  1752,   254,   264,   111,  1760,  1766,  1767,  1777,  1770,
    1673,  1771,  1780,   403,    79,   403,   487,  1781,  1806,  1811,
    1812,   467,  1824,  1826,   965,   467,  1830,  1832,  1911,   488,
     467,  1836,  1911,  1911,   118,   140,   489,  1837,  1907,  1178,
    1130,  1845,  1846,   170,  1849,  1850,  1987,   118,  1867,   118,
     118,  -640,   118,  1759,   403,   467,  1858,   170,   118,  1859,
    1860,   118,   118,   118,  1861,   107,  2037,   490,  1862,   140,
     170,   499,   454,   454,   787,  1881,   468,  1883,  -549,   491,
    1889,  1892,  1894,   140,  1900,  1908,  2034,  2012,  1909,  1922,
    1924,  2053,  1925,   492,  1926,  2053,   467,  1938,   495,  1927,
     467,  1947,  1773,  1964,  1960,  1962,   532,   170,  1974,  1973,
    1907,  1975,  1978,   467,  1979,  1982,   183,  2067,  1985,   103,
     184,   185,   570,  1994,    79,  1996,    79,  1556,  1998,  2011,
     170,   111,  2018,   448,   403,  2007,  2019,   467,   822,   467,
     467,   118,   494,  2013,  2025,    82,  2035,  2036,   147,  2038,
    2051,  2048,  2050,  2056,  2055,  2052,    89,  1093,  2059,  2060,
     112,  2070,   467,  2071,  1673,  1673,  2074,  2064,  1757,  1541,
     681,  2061,  1454,   940,   985,  1848,   986,  1378,   987,  1371,
     988,  2049,   821,   989,    89,  1735,  1988,    79,    79,   734,
     196,  1828,   107,  2005,   654,  1821,   140,  2044,   467,  1920,
      96,  2042,    82,  2033,  2015,  1729,  1730,   170,   467,  1966,
    2057,   170,  1965,   167,  1178,  1387,  1197,   180,   525,   695,
     696,  1681,    89,  1865,   535,   170,    82,  1933,    79,  1520,
     458,  1692,  1384,  1064,  1186,   170,   797,  1742,  1625,   220,
     695,   467,   245,   467,     3,   877,    82,  1217,  1000,  1001,
    1002,     0,   170,     0,     0,     0,     0,     0,   118,     0,
       0,   170,   467,     0,     0,     0,     0,     0,   467,  1923,
     695,   118,   118,     0,   454,     0,   103,     0,   467,     0,
       0,  1673,    79,   147,  1556,     0,     0,   112,   111,    82,
       0,   147,    79,     0,   296,   302,   193,     0,     0,     0,
       0,     0,     0,     0,   103,     0,   322,     0,     0,     0,
       0,   170,  1556,     0,     0,   170,   111,     0,     0,     0,
     170,   249,     0,   410,   180,   180,  1263,    96,     0,     0,
       0,     0,   269,     0,     0,   147,   440,     0,     0,   245,
       0,     0,   103,  1270,  1271,   170,     0,     0,     0,   107,
    1556,     0,     0,   140,   111,  1931,     0,     0,     0,  1673,
       0,   193,     0,   220,   220,     0,     0,     0,     0,   857,
       0,     0,     0,     0,   249,     0,   193,   107,     0,     0,
     296,   140,     0,     0,     0,     0,   170,     0,     0,    82,
     170,     0,     0,   193,     0,     0,     0,   639,     0,     0,
    1673,   468,   245,   170,   787,     0,   443,    13,    14,    15,
      16,    17,     0,     0,     0,   107,  1972,     0,   249,   140,
       0,     0,     0,     0,     0,     0,     0,   170,     0,   170,
     170,     0,   302,     0,     0,     0,     0,     0,   302,   296,
     296,     0,     0,     0,   112,     0,   147,     0,     0,     0,
       0,     0,   170,     0,     0,     0,     0,     0,     0,   193,
       0,     0,     0,  1673,  1673,    57,   322,   607,   616,     0,
       0,   640,   112,     0,    13,    14,    15,    16,    17,     0,
       0,   249,     0,   322,    96,     0,   641,   322,   170,   642,
     643,    64,    65,    66,    67,    68,    69,    70,   170,     0,
       0,     0,     0,     0,  1673,     0,   118,     0,     0,     0,
     112,   249,    96,     0,   118,     0,   193,   249,     0,     0,
     410,     0,     0,     0,    72,     0,     0,   971,     0,     0,
       0,   170,    57,   170,     0,     0,   193,     0,     0,     0,
       0,     0,     0,   118,   736,     0,     0,   249,   499,     0,
      96,     0,   170,     0,   410,    77,    78,   737,   170,   597,
       0,   118,     0,     0,   180,     0,     0,  1247,   170,     0,
       0,     0,  2068,     0,     0,  1247,     0,     0,     0,     0,
     147,   118,  2075,     0,   440,     0,     0,   236,   766,     0,
     616,    72,    61,     0,     0,  1466,  1467,    64,    65,    66,
      67,    68,    69,    70,  1247,     0,     0,   468,     0,  1481,
    1482,  1669,     0,     0,     0,   499,     0,     0,     0,     0,
       0,   118,    77,    78,   193,     0,     0,     0,   220,     0,
       0,   597,     0,     0,     0,     0,     0,   220,     0,     0,
    1277,    74,     0,  1503,  1504,     0,     0,   857,  1379,     0,
       0,     0,     0,     0,   193,     0,     0,   296,     0,   410,
     410,     0,     0,   296,     0,   322,     0,  1247,     0,   249,
       0,     0,     0,    61,     0,     0,   168,   169,    64,    65,
      66,    67,    68,    69,    70,    61,     0,  1354,   217,   218,
      64,    65,    66,    67,    68,    69,    70,     0,     0,     0,
       0,     0,     0,   296,     0,     0,   857,     0,     0,     0,
       0,     0,     0,    72,   296,     0,   296,     0,   322,     0,
      82,     0,     0,   118,   118,   118,   118,   118,   118,   193,
     193,     0,     0,  1525,    74,   443,   322,   440,     0,   616,
    1526,     0,     0,   249,    77,    78,  1380,   607,     0,     0,
       0,   607,   118,   118,     0,     0,     0,     0,     0,     0,
     322,     0,  1226,   249,     0,     0,     0,  1230,     0,     0,
     616,     0,     0,   322,     0,     0,     0,     0,  1238,     0,
       0,   147,     0,   249,     0,     0,     0,     0,   193,     0,
       0,     0,     0,     0,   410,     0,   147,   147,     0,   410,
       0,     0,     0,     0,     0,   410,     0,   443,   147,   147,
     147,     0,     0,     0,     0,     0,   857,     0,   249,     0,
       0,   468,     0,     0,     0,     0,   118,     0,  1660,  1661,
     193,     0,     0,   857,   857,     0,     0,     0,     0,     0,
       0,     0,   249,     0,     0,     0,     0,   468,     0,   249,
       0,   193,     0,    57,     0,  1247,   243,     6,     7,     8,
       9,    10,    11,    12,   440,     0,    13,    14,    15,    16,
      17,   243,     6,     7,     8,     9,    10,    11,    12,     0,
     737,   737,   249,   269,    61,     0,     0,     0,   410,    64,
      65,    66,    67,    68,    69,    70,    13,    14,    15,    16,
      17,     0,     0,     0,     0,   440,     0,     0,   766,     0,
     766,     0,    72,     0,     0,   265,     0,     0,   118,     0,
       0,     0,     0,     0,    57,     0,     0,   322,   322,     0,
       0,     0,    73,    74,   443,     0,     0,     0,     0,     0,
    1746,     0,     0,    77,    78,     0,   322,   654,   296,     0,
       0,  1596,     0,   118,    57,    61,     0,     0,   193,     0,
      64,    65,    66,    67,    68,    69,    70,   296,     0,     0,
       0,  1765,     0,   468,     0,   443,  1768,  1769,     0,     0,
       0,     0,     0,    72,     0,    61,     0,   118,   217,   218,
      64,    65,    66,    67,    68,    69,    70,   443,   443,     0,
       0,   118,     0,    73,    74,   410,     0,     0,     0,     0,
       0,     0,   322,    72,    77,    78,   443,     0,   147,   410,
       0,     0,   695,     0,   114,     0,   118,   114,   322,    18,
    1192,     0,     0,   764,    74,     0,     0,   604,     0,     0,
       0,   607,     0,     0,    77,   765,     0,     0,     0,     0,
       0,   468,     0,     0,     0,     0,  1247,  1436,  1437,  1438,
       0,  1247,  1247,  1247,  1439,  1440,     0,    47,    48,    49,
      50,    51,    52,    53,    54,     0,   249,     0,     0,     0,
     440,   114,   443,     0,     0,   857,   857,   249,     0,   193,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   857,
     857,     0,     0,     0,     0,   114,     0,     0,     0,   249,
       0,     0,     0,     0,   118,     0,  1701,    57,     0,     0,
       0,   251,     0,     0,     0,   114,     0,     0,     0,  1712,
       0,     0,     0,   857,   857,     0,     0,    13,    14,    15,
      16,    17,     0,     0,     0,     0,     0,   737,    61,     0,
     193,   217,   218,    64,    65,    66,    67,    68,    69,    70,
       0,     0,   114,     0,   766,     0,  1740,     0,   114,     0,
     114,   766,    61,     0,   251,     0,    72,    64,    65,    66,
      67,    68,    69,    70,   318,   114,   350,     0,     0,     0,
       0,     0,     0,     0,     0,    57,  1968,    74,     0,     0,
     499,     0,   414,     0,     0,     0,     0,    77,    78,     0,
       0,     0,     0,   322,   114,   414,     0,     0,   251,     0,
       0,    74,     0,     0,   786,     0,    61,     0,     0,   217,
     218,    64,    65,    66,    67,    68,    69,    70,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1247,     0,  1247,
       0,     0,     0,   147,    72,     0,     0,     0,     0,     0,
       0,   410,     0,     0,     0,   114,  1797,     0,   114,     0,
    1807,   118,     0,     0,  1968,    74,     0,     0,   499,     0,
       0,   251,     0,   443,  1820,    77,    78,     0,     0,     0,
     410,     0,     0,     0,  1829,     0,     0,     0,   549,   118,
       0,     0,     0,     0,     0,     0,   114,   245,    82,     0,
       0,   251,     0,  2020,     0,     0,     0,   251,   857,   857,
       0,     0,   296,     0,     0,   114,     0,     0,   147,     0,
       0,     0,     0,     0,     0,     0,   440,   118,     0,     0,
       0,     0,     0,     0,     0,   114,     0,   251,   114,     0,
       0,     0,     0,     0,  1696,     0,     0,     0,     0,     0,
       0,     0,   114,     0,   440,     0,   114,     0,   147,     0,
    1875,     0,     0,     0,  1880,     0,    57,     0,     0,  1885,
       0,     0,     0,     0,    61,     0,   249,   168,   169,    64,
      65,    66,    67,    68,    69,    70,     0,     0,   193,   414,
       0,     0,     0,     0,  1915,     0,   193,    61,     0,     0,
     217,   218,    64,    65,    66,    67,    68,    69,    70,   249,
       0,     0,     0,     0,     0,  1736,     0,     0,     0,     0,
     857,   322,   322,   414,   193,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1942,     0,     0,     0,  1945,
       0,     0,     0,     0,     0,  1277,    74,  1357,     0,   114,
       0,   857,  1959,   414,     0,     0,   857,   857,     0,   251,
     147,   147,   147,   147,   147,   147,     0,     0,     0,     0,
    1527,   302,     0,     0,     0,     0,  1981,    61,  1983,  1984,
     168,   169,    64,    65,    66,    67,    68,    69,    70,   410,
     410,   443,   443,     0,     0,     0,     0,    61,     0,     0,
       0,  1993,    64,    65,    66,    67,    68,    69,    70,   956,
     306,   307,   308,   309,     0,     0,     0,     0,     0,   245,
       0,     0,     0,     0,     0,     0,     0,     0,   414,   414,
       0,     0,     0,   251,   114,     0,  1361,  2016,     0,   440,
       0,     0,     0,     0,     0,     0,     0,  2021,    61,   957,
     249,   168,   169,    64,    65,    66,    67,    68,    69,    70,
       0,     0,     0,   147,     0,   114,     0,     0,     0,     0,
     114,     0,     0,   251,   114,     0,   114,     0,     0,     0,
    2047,     0,  2021,     0,     0,     0,     0,   114,     0,   114,
       0,     0,     0,     0,     0,     0,     0,     0,   249,     0,
     310,  2058,     0,   350,     0,   114,   414,  2047,   251,   193,
       0,    13,    14,    15,    16,    17,    61,  2066,   311,   346,
     347,    64,    65,    66,    67,    68,    69,    70,     0,   114,
       0,     0,   251,     0,     0,     0,   549,     0,     0,   251,
       0,     0,   114,     0,   911,     0,     0,     0,     0,  1670,
     114,     0,     0,  1527,     0,   410,     0,     0,  1916,  1527,
       0,  1527,     0,   414,     0,   114,   114,    75,   414,    57,
       0,     0,   348,     0,   414,     0,     0,   114,   114,   114,
       0,     0,     0,     0,     0,     0,     0,     0,    61,   302,
     147,   168,   169,    64,    65,    66,    67,    68,    69,    70,
      61,   182,     0,   217,   218,    64,    65,    66,    67,    68,
      69,    70,   721,   722,   723,   724,   725,   726,   727,   728,
     729,   730,   731,   223,   410,   193,   204,     0,    72,     0,
       0,     0,     0,   414,     0,   322,    61,     0,   147,   168,
     169,    64,    65,    66,    67,    68,    69,    70,   219,    74,
       0,     0,     0,   732,     0,     0,     0,   414,     0,    77,
      78,  1359,     0,   147,     0,     0,     0,     0,   249,     0,
       0,     0,     0,     0,   414,     0,    61,     0,   298,   217,
     218,    64,    65,    66,    67,    68,    69,    70,   322,   322,
       0,     0,   579,   857,     0,     0,   114,   114,     0,     0,
       0,    61,     0,  1670,  1670,   193,    64,    65,    66,    67,
      68,    69,    70,     0,     0,   114,     0,     0,  1527,     0,
       0,  1527,     0,     0,     0,     0,     0,     0,     0,    72,
       0,     0,  1205,     0,     0,     0,     0,     0,   302,     0,
       0,     0,     0,   114,     0,     0,     0,   485,   223,  1020,
      74,   410,     0,   604,     0,     0,     0,     0,   443,   443,
      77,    78,     0,     0,   298,     0,   251,     0,     0,     0,
       0,     0,     0,    57,   414,     0,     0,   251,   296,     0,
       0,   114,     0,     0,     0,     0,     0,   114,   414,     0,
       0,     0,     0,     0,     0,     0,     0,   114,     0,  1194,
     414,     0,   114,     0,    61,     0,     0,   217,   218,    64,
      65,    66,    67,    68,    69,    70,     0,   249,     0,     0,
    1670,     0,     0,   571,   298,    98,     0,     0,   151,  1527,
       0,   366,    72,   367,     0,   368,    62,    63,    64,    65,
      66,    67,    68,    69,    70,     0,     0,     0,     0,   414,
       0,     0,   219,    74,     0,     0,     0,     0,    13,    14,
      15,    16,    17,    77,    78,   147,    61,     0,     0,   189,
      63,    64,    65,    66,    67,    68,    69,    70,     0,     0,
       0,   680,    98,     0,    75,   377,     0,     0,     0,     0,
     322,     0,     0,     0,     0,     0,     0,     0,  1670,     0,
       0,     0,     0,     0,     0,     0,   195,     0,   147,     0,
       0,     0,   114,     0,     0,    74,    57,     0,   786,     0,
       0,     0,     0,     0,     0,     0,   258,     0,     0,   114,
     114,     0,     0,     0,     0,     0,   147,   147,     0,  1969,
     302,     0,     0,     0,     0,     0,     0,    61,     0,     0,
     217,   218,    64,    65,    66,    67,    68,    69,    70,     0,
     443,     0,     0,   288,     0,     0,     0,     0,     0,    98,
       0,     0,   767,     0,   147,    72,     0,     0,     0,     0,
       0,     0,   114,     0,     0,     0,   324,     0,     0,     0,
       0,     0,     0,     0,     0,   295,    74,     0,     0,     0,
       0,     0,  1969,  1969,   420,     0,    77,    78,     0,     0,
       0,     0,   806,     0,     0,   288,   446,     0,     0,   249,
       0,   223,   114,     0,     0,     0,     0,     0,    61,     0,
     414,   168,   169,    64,    65,    66,    67,    68,    69,    70,
       0,   298,     0,  1969,   493,     0,     0,   298,     0,     0,
       0,     0,     0,     0,   108,     0,     0,     0,     0,   414,
     513,     0,     0,     0,     0,   518,   520,     0,     0,   195,
       0,     0,     0,     0,     0,    61,   251,   114,   217,   218,
      64,    65,    66,    67,    68,    69,    70,   298,     0,     0,
       0,   540,     0,     0,   542,     0,   543,   114,   869,     0,
     298,     0,     0,    72,     0,   414,     0,   560,     0,  1194,
       0,   108,     0,     0,    13,    14,    15,    16,    17,     0,
     572,  1423,     0,   764,    74,     0,     0,   604,     0,     0,
       0,     0,     0,   414,    77,   765,     0,   114,     0,     0,
       0,     0,     0,     0,     0,     0,   595,   606,     0,   619,
       0,     0,     0,     0,     0,   259,     0,     0,     0,     0,
       0,     0,     0,   626,     0,     0,     0,   626,    61,     0,
       0,     0,    57,    64,    65,    66,    67,    68,    69,    70,
    1244,   114,   114,     0,  1245,     0,  1246,     0,     0,     0,
       0,   659,     0,     0,     0,   114,   114,     0,   108,     0,
     114,   114,     0,    61,     0,     0,   217,   218,    64,    65,
      66,    67,    68,    69,    70,   328,    61,    74,     0,     0,
    1446,    64,    65,    66,    67,    68,    69,    70,  1244,   114,
     114,    72,  1245,     0,  1246,     0,    57,     0,     0,   114,
     114,   114,   114,   114,   114,   447,     0,     0,     0,     0,
     251,  1525,    74,     0,     0,     0,     0,     0,     0,     0,
     288,     0,    77,    78,   595,    74,     0,    61,   414,   414,
     217,   218,    64,    65,    66,    67,    68,    69,    70,     0,
       0,     0,     0,     0,     0,     0,     0,   659,     0,     0,
       0,     0,  1021,     0,   767,    72,     0,     0,   251,     0,
       0,    61,     0,     0,     0,     0,    64,    65,    66,    67,
      68,    69,    70,  1244,     0,   295,    74,  1245,   414,  1246,
     541,     0,     0,     0,     0,     0,    77,    78,     0,    57,
       0,     0,   298,     0,     0,     0,   108,     0,     0,     0,
       0,     0,   114,     0,     0,   446,     0,     0,     0,     0,
      74,   298,     0,  1637,     0,     0,     0,     0,     0,     0,
      61,     0,     0,   217,   218,    64,    65,    66,    67,    68,
      69,    70,     0,     0,     0,   596,   853,     0,   259,     0,
       0,   520,     0,     0,     0,   864,     0,   560,    72,     0,
       0,     0,   596,     0,     0,     0,   596,     0,   324,     0,
      98,     0,     0,     0,     0,     0,     0,     0,  1525,    74,
       0,     0,     0,     0,   114,   114,   626,   886,    61,    77,
      78,   217,   218,    64,    65,    66,    67,    68,    69,    70,
       0,   897,     0,     0,   414,     0,     0,     0,     0,     0,
     595,     0,     0,     0,     0,   906,    72,     0,     0,     0,
     114,     0,    61,   626,     0,   217,   218,    64,    65,    66,
      67,    68,    69,    70,     0,     0,  1525,    74,   251,   114,
       0,     0,     0,  1526,     0,     0,     0,    77,    78,    61,
      72,     0,   217,   218,    64,    65,    66,    67,    68,    69,
      70,     0,     0,   596,     0,     0,     0,     0,     0,     0,
    1968,    74,     0,   414,   499,     0,     0,    72,     0,     0,
       0,    77,    78,     0,   114,     0,     0,   114,     0,     0,
       0,     0,     0,     0,     0,     0,   114,   219,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    77,    78,
       0,     0,   114,     0,   446,     0,     0,     0,  1021,     0,
       0,     0,     0,     0,  1278,   767,     0,   114,     0,     0,
       0,  1003,   114,   114,     0,     0,     0,   114,   114,     0,
       0,     0,     0,     0,   447,     0,     0,     0,   171,   174,
       0,     0,     0,     0,     0,   886,     0,     0,     0,     0,
    1027,    61,     0,     0,   217,   218,    64,    65,    66,    67,
      68,    69,    70,     0,     0,   328,     0,   446,   446,     0,
       0,     0,     0,   212,   259,   113,   108,   251,     0,    72,
       0,     0,     0,     0,     0,     0,   446,   447,     0,   108,
     414,     0,     0,     0,     0,     0,     0,     0,     0,   295,
      74,     0,     0,     0,     0,   596,   447,     0,     0,     0,
      77,    78,     0,     0,   853,     0,     0,     0,     0,     0,
       0,     0,     0,   290,     0,     0,   291,     0,     0,   596,
       0,     0,   113,     0,     0,     0,     0,     0,     0,   312,
       0,     0,   596,     0,     0,  1163,     0,     0,     0,     0,
       0,     0,   446,     0,     0,     0,     0,     0,   151,     0,
       0,     0,     0,     0,     0,     0,   298,     0,   626,     0,
       0,  1196,     0,   853,    61,     0,   261,     0,  1202,    64,
      65,    66,    67,    68,    69,    70,  1244,     0,     0,     0,
    1245,    61,  1246,   478,   544,    63,    64,    65,    66,    67,
      68,    69,    70,    61,   114,     0,   546,   547,    64,    65,
      66,    67,    68,    69,    70,     0,     0,     0,     0,   113,
     324,     0,     0,    74,     0,     0,  1639,     0,     0,   114,
       0,     0,     0,   447,     0,     0,   332,     0,   529,     0,
       0,     0,     0,   994,     0,     0,     0,   114,   171,     0,
       0,     0,     0,     0,    75,     0,     0,     0,     0,   171,
       0,     0,     0,     0,     0,  1478,   449,     0,     0,     0,
       0,     0,     0,     0,   447,   114,   114,     0,     0,   251,
       0,     0,     0,   853,     0,     0,   575,     0,     0,     0,
       0,     0,     0,   578,   580,     0,   328,   328,   587,    61,
     853,   853,    62,    63,    64,    65,    66,    67,    68,    69,
      70,     0,     0,   114,     0,   328,     0,     0,     0,     0,
       0,     0,     0,     0,  1531,     0,     0,     0,     0,     0,
       0,   633,     0,     0,     0,     0,   312,     0,     0,   312,
       0,     0,     0,   328,     0,     0,     0,     0,     0,     0,
      75,     0,     0,   446,     0,     0,     0,   113,    61,   114,
       0,   168,   169,    64,    65,    66,    67,    68,    69,    70,
       0,     0,     0,     0,   108,     0,     0,     0,     0,    61,
       0,   328,   168,   169,    64,    65,    66,    67,    68,    69,
      70,     0,     0,  1337,     0,     0,   598,   596,     0,   261,
     259,  1163,   328,     0,     0,     0,   457,     0,     0,     0,
       0,     0,     0,   598,     0,     0,   119,   598,     0,   119,
       0,     0,     0,     0,     0,     0,   212,   461,     0,     0,
    1163,     0,     0,     0,     0,     0,     0,     0,   781,   782,
       0,     0,     0,     0,     0,     0,     0,     0,  1385,   447,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   119,     0,     0,   595,     0,     0,     0,
       0,     0,     0,     0,     0,   518,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   119,     0,     0,
       0,     0,     0,     0,   324,     0,     0,  1531,     0,     0,
       0,     0,   328,  1685,   598,  1531,     0,   119,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   328,
     328,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   853,   853,   119,     0,     0,     0,     0,     0,
     119,     0,   119,     0,     0,     0,   853,   853,     0,   312,
       0,   446,   446,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   328,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   119,   449,     0,     0,     0,     0,
     853,   853,     0,     0,     0,     0,   119,     0,     0,     0,
    1337,  1337,  1337,   151,     0,     0,     0,     0,   633,     0,
       0,     0,     0,     0,     0,     0,   332,     0,     0,     0,
     108,     0,     0,     0,     0,   261,     0,   113,     0,  1555,
    1555,     0,     0,     0,     0,     0,     0,     0,   449,     0,
     113,     0,     0,     0,     0,     0,     0,   119,     0,   108,
     119,     0,     0,     0,     0,   119,   598,   449,     0,     0,
       0,     0,  1794,     0,     0,  1531,     0,   259,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   324,
     598,     0,     0,     0,     0,     0,     0,     0,   119,     0,
       0,     0,     0,   598,     0,   596,     0,     0,     0,     0,
       0,     0,     0,   151,     0,     0,     0,   119,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   298,   447,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   123,     0,     0,   123,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1044,     0,     0,     0,     0,     0,     0,  1056,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   328,   328,  1531,     0,   853,   853,     0,     0,     0,
       0,   119,     0,     0,   449,   328,   328,     0,     0,     0,
     328,   328,     0,   123,     0,     0,     0,     0,     0,     0,
       0,  1687,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   853,     0,     0,     0,   119,     0,   123,     0,   328,
     328,     0,     0,     0,     0,   449,     0,     0,     0,     0,
    1704,     0,     0,  1114,     0,     0,     0,   123,     0,     0,
       0,   119,     0,     0,     0,     0,     0,   332,   332,     0,
       0,     0,     0,     0,     0,   633,     0,     0,   108,   108,
       0,     0,     0,     0,  1555,     0,   332,     0,     0,     0,
       0,     0,     0,     0,   123,   324,     0,     0,   151,     0,
     123,     0,   123,   298,     0,  1209,     0,   853,     0,   633,
       0,     0,     0,     0,   332,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   447,     0,
       0,     0,     0,     0,   123,     0,     0,     0,   853,     0,
     119,   119,     0,   853,   853,   113,   123,     0,   446,   446,
       0,     0,   332,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1786,     0,   571,   298,   598,     0,
       0,   261,     0,   332,     0,     0,     0,     0,     0,     0,
       0,     0,   119,     0,     0,     0,   119,     0,   119,     0,
       0,     0,     0,     0,     0,     0,     0,   123,     0,     0,
     123,   119,     0,     0,     0,   123,     0,   298,     0,     0,
       0,  1555,     0,     0,     0,     0,     0,     0,     0,     0,
     449,     0,     0,     0,   328,   328,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   123,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   123,     0,   203,
     328,     0,   119,     0,     0,   214,   215,     0,     0,     0,
       0,     0,     0,     0,     0,   119,     0,   119,   119,   259,
     119,     0,     0,   332,     0,     0,   119,     0,     0,   119,
     119,   119,     0,     0,     0,     0,     0,     0,     0,   277,
     332,   332,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   108,     0,  1358,  1360,  1362,     0,     0,
       0,     0,     0,     0,   328,  1906,     0,     0,     0,     0,
       0,   123,     0,     0,     0,     0,   328,     0,     0,     0,
       0,     0,     0,     0,     0,  1381,     0,     0,     0,     0,
     446,     0,     0,   332,     0,     0,     0,     0,     0,     0,
    1114,     0,     0,     0,     0,   123,     0,   328,  1555,   119,
       0,     0,   328,   328,     0,     0,     0,   328,   328,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   123,     0,     0,     0,     0,  1555,  1906,   633,     0,
       0,   113,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     113,     0,     0,     0,  1555,     0,     0,     0,     0,     0,
     108,     0,     0,     0,     0,     0,     0,     0,   261,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  2009,     0,   568,     0,     0,     0,     0,
     123,   123,     0,     0,     0,     0,   598,     0,     0,     0,
     853,     0,     0,     0,     0,     0,   119,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   119,
     119,     0,     0,     0,   449,     0,     0,     0,     0,     0,
       0,     0,   123,     0,     0,     0,   123,     0,   123,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   123,     0,  1536,     0,     0,  1538,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   332,   332,   596,     0,     0,     0,     0,     0,
       0,     0,  1866,     0,     0,     0,   332,   332,     0,     0,
       0,   332,   332,     0,     0,     0,     0,     0,     0,   328,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   123,     0,     0,     0,     0,   108,     0,     0,
     332,   332,     0,     0,     0,   123,     0,   123,   123,   361,
     123,     0,   762,   362,   763,   363,   123,     0,     0,   123,
     123,   123,     0,   779,   780,   108,   596,     0,     0,     0,
       0,     0,   364,     0,     0,     0,     0,     0,     0,   113,
     113,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   365,
     366,     0,   367,   108,   368,    62,    63,    64,    65,    66,
      67,    68,    69,    70,   369,   370,   358,     0,   371,   372,
     373,     0,   374,   375,     0,     0,     0,     0,     0,   449,
      72,     0,     0,     0,     0,     0,     0,     0,     0,   123,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   328,
     376,     0,     0,    75,   377,     0,     0,     0,     0,     0,
     378,    77,    78,   379,   380,   381,   382,  1693,     0,     0,
       0,   863,     0,     0,   119,     0,     0,     0,     0,     0,
       0,     0,   119,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1201,     0,     0,     0,     0,     0,
       0,    13,    14,    15,    16,    17,     0,     0,     0,     0,
       0,   119,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   332,   332,     0,     0,   119,
       0,     0,     0,     0,     0,     0,     0,   361,     0,     0,
       0,   362,     0,   363,     0,     0,     0,     0,     0,   119,
       0,     0,     0,     0,   633,     0,   123,     0,     0,    57,
     364,   332,     0,     0,     0,     0,     0,     0,     0,   123,
     123,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     261,     0,     1,     0,     0,   145,     0,   365,   366,   119,
     367,     0,   368,    62,    63,    64,    65,    66,    67,    68,
      69,    70,   369,   370,   358,     0,   371,   372,   373,     0,
     374,   375,     0,     0,   113,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,   332,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   332,   376,     0,
       0,    75,   377,     0,     0,     0,     0,     0,   378,   439,
      78,   379,   380,   381,   382,     0,     0,     0,   192,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   332,     0,
       0,     0,     0,   332,   332,     0,  1842,     0,   332,   332,
       0,     0,     0,     0,     0,   633,     0,     0,     0,     0,
    1042,   119,   119,   119,   119,   119,   119,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   283,     0,     0,
     119,   119,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    13,    14,    15,    16,    17,     0,     0,
      19,   113,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,  1110,  1111,     0,
       0,    45,    46,     0,     0,     0,     0,     0,  1169,  1170,
    1171,     0,     0,  1173,     0,     0,     0,     0,     0,     0,
       0,    57,     0,     0,   119,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   123,     0,     0,     0,     0,     0,
       0,   283,   123,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   658,     0,     0,     0,   521,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   283,     0,     0,     0,
       0,   123,     0,     0,     0,     0,   283,     0,     0,     0,
       0,     0,     0,     0,     0,   598,     0,     0,     0,   123,
     552,   556,     0,  1240,     0,     0,     0,   563,   564,     0,
       0,     0,     0,     0,     0,     0,     0,   -16,     0,   123,
     332,     0,     0,   574,     0,     0,   119,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   113,     0,
       0,     0,     0,   593,     0,     0,     0,     0,     0,  1261,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   123,
       0,   119,     0,     0,     0,     0,   113,   598,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   119,  1285,   388,     0,   679,
       0,     0,     0,     0,   113,  1289,  1290,  1291,  1292,   119,
       0,     0,     0,  1297,  1298,     0,     0,     0,     0,     0,
       0,     0,     0,  1306,     0,     0,     0,     0,     0,     0,
     720,     0,     0,     0,   119,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1320,     0,  1321,     0,     0,     0,
     332,     0,     0,     0,     0,     0,   758,     0,     0,     0,
     761,   123,   123,   123,   123,   123,   123,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   783,
       0,     0,     0,   784,   785,     0,     0,   788,     0,     0,
     123,   123,     0,     0,     0,     0,     0,     0,     0,  1377,
       0,     0,   802,   803,   804,   805,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   827,   119,     0,     0,  1391,     0,     0,     0,   830,
       0,     0,  1395,     0,  1397,  1399,     0,     0,     0,     0,
       0,     0,     0,     0,  1406,     0,  1407,     0,  1408,     0,
    1410,     0,     0,     0,     0,  1418,     0,   283,     0,     0,
       0,     0,     0,     0,   123,     0,     0,     0,     0,     0,
     165,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     651,     0,     0,   388,   656,     0,     0,     0,   868,     0,
       0,     0,     0,   662,   663,   552,   165,     0,     0,     0,
       0,   874,     0,     0,     0,     0,     0,     0,   388,   388,
       0,     0,     0,     0,     0,     0,  1461,     0,     0,     0,
       0,     0,     0,  1468,  1469,   889,   894,     0,     0,   388,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   165,     0,     0,     0,     0,  1492,     0,     0,
       0,     0,     0,     0,  1497,   165,   123,   165,  1498,   388,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   119,
       0,     0,     0,     0,     0,     0,     0,   352,   214,   936,
       0,   123,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   352,     0,     0,   119,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   123,     0,     0,  1591,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   123,
       0,     0,   165,     0,     0,   119,   165,     0,     0,   165,
     165,     0,     0,   165,     0,     0,   165,   165,   999,     0,
       0,     0,  1611,     0,   123,     0,     0,     0,     0,     0,
    1616,     0,  1618,  1016,     0,     0,     0,  1017,     0,     0,
       0,     0,     0,     0,     0,     0,   889,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1057,     0,
       0,     0,     0,     0,     0,     0,     0,  1066,   165,  1645,
    1646,   165,     0,  1069,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1651,  1652,     0,  1653,     0,     0,
       0,     0,   165,   165,     0,     0,  1657,     0,     0,     0,
       0,     0,   123,     0,     0,     0,  1662,  1663,   165,     0,
       1,     0,     0,     0,     0,     0,     0,     1,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     1,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   388,   388,   388,   388,
     388,   388,   388,   388,   388,   388,   388,   388,   388,   388,
     388,   388,   388,   388,   388,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1220,     0,
       0,     0,   165,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1747,  1748,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1755,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   388,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   352,     0,   123,
       0,     0,     0,  1778,  1779,     0,     0,     0,     0,   165,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1268,     0,     0,     0,  1269,     0,   123,     0,     0,
       0,     0,   889,     0,     0,   247,     0,     0,     0,     0,
       0,     0,  1282,     0,     0,     0,   268,     0,   271,  1283,
     273,     0,     0,     0,     0,     0,     0,     0,  1287,     0,
    1288,     0,     0,     0,     0,   123,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   352,     0,     0,     0,     0,   247,  1844,
     271,   273,  1316,     0,     0,     0,  1317,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1853,     0,
     145,  1854,  1855,     1,     0,     0,     0,     0,  1857,     0,
       0,     0,     0,   388,   165,   165,     0,     0,     0,     0,
       0,     0,   247,     0,     0,     0,     0,   165,     0,     0,
       0,   388,   242,     0,     0,     0,   388,     0,     0,     0,
       0,    13,    14,    15,    16,    17,     0,   388,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,  -453,  -453,     0,  -453,    45,
      46,     0,  -453,     0,     0,   247,     0,   271,   273,   388,
       0,     0,  1405,     0,     0,     0,     0,     0,     0,    57,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   163,     0,   247,     0,  1429,     0,     0,
       0,   247,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    62,    63,     0,     0,  1967,     0,     0,
       0,     0,     0,   165,   165,     0,     0,     0,     0,   165,
       0,   247,     0,     0,     0,     0,     0,   622,    72,   273,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     165,     0,     0,   165,   165,     0,   165,     0,   165,   165,
       0,    75,   301,     0,     0,   275,     0,     0,     0,    77,
      78,     0,     0,     0,     0,     0,     0,     0,   281,  2006,
     282,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1501,     0,     0,     0,  1502,     0,   388,   165,     0,     0,
       0,   165,     0,  2023,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  2032,     0,
       0,     0,     0,     0,  1537,     0,     0,   247,     0,     0,
       0,     0,     0,  2045,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   247,     0,   622,   273,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   388,   503,   504,  1601,     0,   508,  1604,   165,   511,
     512,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1613,     0,     0,     0,     0,     0,     0,
     247,     0,     0,     0,     0,     0,   388,   388,   388,     0,
       0,     0,     0,   388,   388,     0,   339,     0,     0,     0,
     247,     0,     0,     0,     0,   247,     0,   247,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   388,     0,     0,
       0,     0,     0,     0,  1644,   436,   339,   247,     0,   247,
     247,     0,     0,  1649,     0,     0,     0,  1650,     0,     0,
       0,     0,     0,     0,     0,   590,   591,   247,     0,     0,
       0,  1654,  1655,     0,   388,   388,     0,   502,     0,   247,
       0,   623,     0,     0,   502,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   247,     0,   622,   273,     0,     0,     0,   165,
       0,     0,     0,     0,     0,   673,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   247,   622,     0,     0,
       0,     0,     0,   247,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     165,     0,   502,     0,     0,     0,     0,   165,     0,     0,
     165,     0,     0,     0,     0,     0,   247,   268,     0,     0,
       0,     0,     0,     0,     0,   752,   339,   608,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1743,  1744,     0,
       0,     0,     0,     0,     0,     0,     0,   629,     0,     0,
      13,    14,    15,    16,    17,    18,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,     0,     0,    45,    46,
       0,     0,   823,    47,    48,    49,    50,    51,    52,    53,
      54,     0,     0,     0,   833,   835,     0,     0,    57,     0,
       0,     0,     0,     0,  1432,     0,     0,   502,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   165,     0,   502,   754,     0,   502,   757,     0,   165,
     165,     0,    62,    63,   339,     0,     0,     0,   608,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1831,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     200,     0,     0,     0,     0,     0,     0,  1604,     0,   502,
      75,   926,     0,   502,     0,     0,   255,   165,    77,    78,
       0,     0,     0,     0,     0,  1856,   165,   900,   901,   165,
     247,   165,   165,     0,     0,     0,     0,     0,     0,     0,
     908,   247,     0,     0,   388,   339,     0,     0,     0,     0,
       0,     0,  1874,     0,     0,     0,     0,     0,     0,   673,
       0,     0,     0,   247,   673,   200,     0,     0,     0,   303,
     673,     0,   165,     0,   247,     0,     0,     0,     0,  1902,
     344,     0,  1903,   247,     0,     0,     0,     0,     0,   673,
       0,     0,     0,     0,     0,   502,     0,   200,   339,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     456,     0,     0,   460,     0,     0,   884,   339,     0,     0,
       0,     0,     0,     0,     0,   991,     0,   608,     0,     0,
       0,   608,     0,     0,     0,     0,     0,     0,   902,   165,
     339,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1005,  1006,     0,     0,
       0,     0,  1010,   200,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   255,     0,     0,     0,
     247,  1986,     0,  1031,     0,     0,  1034,  1035,     0,  1038,
       0,  1040,  1041,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   247,     0,     0,     0,     0,     0,
       0,     0,   460,     0,     0,     0,     0,     0,     0,     0,
     200,     0,     0,   165,     0,     0,     0,     0,     0,     0,
    1081,     0,     0,     0,  1085,     0,     0,     0,     0,     0,
     601,     0,   618,     0,   339,     0,     0,   388,     0,     0,
       0,     0,   165,     0,     0,     0,     0,     0,     0,     0,
     502,   502,     0,     0,     0,     0,     0,     0,     0,     0,
     502,  1012,     0,   502,  1015,     0,     0,     0,   165,     0,
       0,     0,     0,     0,   165,   339,     0,   388,   608,     0,
     608,   608,     0,     0,   677,     0,     0,   608,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   339,   339,     0,
       0,  1203,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   339,     0,   200,     0,
     502,     0,     0,     0,   502,     0,     0,     0,   502,  1083,
     247,     0,   502,  1087,     0,     0,     0,     0,     0,     0,
    1090,     0,     0,   165,     0,     0,     0,     0,   601,     0,
       0,     0,     0,     0,   778,     0,     0,     0,     0,     0,
       0,     0,     0,   247,     0,     0,     0,     0,     0,   247,
       0,     0,     0,   388,     0,   388,     0,    13,    14,    15,
      16,    17,   339,   502,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,   608,     0,     0,   388,    45,    46,     0,     0,   165,
     165,     0,     0,   200,   200,     0,     0,   352,     0,   456,
       0,   165,  1203,     0,     0,    57,     0,     0,     0,     0,
       0,     0,     0,   412,     0,     0,   388,     0,     0,     0,
     339,     0,     0,     0,     0,     0,   441,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   658,     0,     0,   469,
       0,   469,     0,  1302,     0,     0,     0,     0,     0,     0,
    1309,     0,   344,  1313,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   388,     0,     0,     0,     0,     0,
       0,   456,     0,   888,   247,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   502,     0,     0,
       0,     0,     0,     0,   601,     0,     0,     0,     0,     0,
       0,   165,     0,     0,   608,   608,     0,     0,     0,     0,
       0,   608,     0,     0,     0,   200,     0,     0,     0,     0,
       0,     0,   247,     0,     0,     0,     0,   569,   677,     0,
     677,   677,     0,   677,     0,     0,     0,     0,     0,   677,
       0,     0,   677,   677,   677,     0,     0,     0,     0,     0,
       0,     0,     0,   339,     0,     0,     0,     0,   502,  1311,
       0,   502,  1315,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   165,  1412,     0,     0,     0,     0,     0,
       0,     0,  1421,  1422,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   456,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   200,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   456,
    1463,     0,     0,     0,     0,     0,     0,     0,     0,  1472,
       0,     0,  1476,     0,  1479,  1480,     0,     0,     0,     0,
       0,   456,   456,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   339,     0,     0,     0,
     456,   165,   608,  1414,     0,     0,     0,     0,     0,     0,
       0,     0,   247,     0,     0,  1506,     0,     0,     0,     0,
     469,     0,     0,     0,   339,     0,   469,     0,     0,     0,
       0,   799,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1546,     0,     0,  1549,  1563,     0,     0,     0,     0,
    1570,     0,     0,     0,  1574,     0,  1576,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   456,     0,   502,  1465,
       0,     0,     0,   200,     0,     0,     0,   502,  1474,     0,
     608,     0,  1598,     0,   778,     0,     0,     0,     0,     0,
       0,   339,   339,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   867,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   344,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   247,     0,     0,   441,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     896,   247,     0,     0,     0,     0,  1476,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1659,  1664,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   339,
       0,   930,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1697,     0,
       0,     0,   799,   950,     0,     0,   952,     0,   954,     0,
    1703,     0,     0,   247,   963,     0,   968,   963,     0,     0,
       0,     0,     0,     0,     0,  1718,  1720,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   456,     0,     0,
       0,     0,     0,     0,   996,     0,     0,     0,     0,  1549,
       0,     0,     0,     0,     0,     0,     0,   998,     0,     0,
       0,     0,     0,     0,     0,     0,  1741,     0,  1007,     0,
       0,     0,     0,     0,     0,     0,     0,   677,     0,     0,
       0,     0,   441,     0,     0,   996,     0,     0,     0,   502,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   502,     0,     0,     0,     0,
       0,     0,  1060,     0,     0,   469,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   255,     0,   247,     0,     0,     0,     0,     0,     0,
       0,     0,  1790,  1791,     0,     0,     0,     0,     0,     0,
       0,  1091,   200,     0,  1795,     0,     0,     0,     0,  1805,
     601,     0,     0,     0,     0,     0,     0,     0,  1808,     0,
    1810,     0,     0,  1815,  1819,   339,  1563,     0,     0,     0,
       0,  1825,     0,     0,     0,     0,     0,     0,   344,     0,
       0,     0,   677,     0,     0,     0,     0,     0,     0,   412,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1193,  1195,     0,     0,     0,     0,     0,     0,   441,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   339,   339,
       0,     0,     0,     0,     0,     0,   247,     0,     0,     0,
       0,     0,     0,   502,   502,     0,     0,   963,     0,     0,
       0,     0,     0,     0,  1864,   456,   456,     0,     0,   502,
     996,     0,     0,     0,     0,     0,     0,     0,  1233,     0,
       0,     0,     0,     0,     0,   963,  1891,     0,     0,     0,
       0,  1896,  1898,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   677,   677,   677,     0,   677,   677,
       0,     0,     0,     0,     0,   460,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   469,     0,     0,     0,     0,  1930,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1946,     0,  1949,     0,     0,  1951,  1953,     0,     0,
       0,  1956,  1958,   255,     0,     0,     0,     0,     0,     0,
     502,     0,     0,     0,     0,     0,     0,     0,   502,     0,
       0,     0,     0,   344,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   469,     0,
    1301,     0,  1304,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    2000,  2002,  2004,     0,     0,     0,     0,     0,     0,     0,
     339,     0,     0,     0,   502,  1932,     0,     0,   502,     0,
       0,  2017,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  2027,  2029,  2031,     0,  1369,  1369,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   502,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   200,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1416,     0,     0,     0,     0,     0,     0,    13,
      14,    15,    16,    17,     0,     0,     0,     0,     0,     0,
    1409,     0,     0,   255,     0,     0,  1419,     0,     0,     0,
       0,     0,     0,     0,     0,   469,     0,     0,     0,     0,
       0,     0,   502,   502,   441,   361,     0,     0,     0,   362,
       0,   363,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   469,     0,     0,     0,     0,     0,    57,   364,   344,
       0,     0,     0,     0,     0,     0,   963,     0,     0,   799,
       0,     0,     0,   502,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   365,   366,   677,   367,     0,
     368,    62,    63,    64,    65,    66,    67,    68,    69,    70,
     369,   370,   358,     0,   371,   372,   373,     0,   374,   375,
       0,     0,   456,   456,     0,     0,    72,     0,     0,     0,
       0,  1500,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   376,     0,     0,    75,
     377,     0,     0,     0,     0,     0,   378,  1417,    78,   379,
     380,   381,   382,     0,     0,     0,     0,     0,     0,   963,
       0,     0,   255,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   469,     0,     0,
     799,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    2046,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1353,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   950,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1614,
    1615,     0,     0,     0,     0,     0,     0,   361,     0,     0,
       0,   362,     0,   363,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   469,     0,   799,  1116,     0,
     364,    -2,     0,  1118,  -238,  -238,  1119,  1120,  1121,  1122,
    1123,  1124,  1125,  1126,  1127,  1128,  1129,  1130,  -332,  1131,
    1132,  1133,  1134,  1135,     0,  1136,     0,   365,   366,   677,
     463,     0,   368,  1137,  1138,    64,    65,    66,    67,    68,
      69,    70,   369,   370,   358,  1139,   371,   372,   373,     0,
     374,   375,     0,     0,   456,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   412,     0,  -238,   376,     0,
    1686,    75,   377,     0,     0,     0,   279,     0,   378,    77,
      78,   379,   380,   381,   382,     0,     0,     0,     0,     0,
       0,   677,     0,  -179,   460,     0,     0,     0,     0,     0,
       0,     0,  2046,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1353,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1732,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   361,
       0,     0,     0,   362,     0,   363,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1116,  1756,   364,    -2,  1758,  1118,  -239,  -239,  1119,  1120,
    1121,  1122,  1123,  1124,  1125,  1126,  1127,  1128,  1129,  1130,
    -332,  1131,  1132,  1133,  1134,  1135,     0,  1136,     0,   365,
     366,     0,   463,     0,   368,  1137,  1138,    64,    65,    66,
      67,    68,    69,    70,   369,   370,   358,  1139,   371,   372,
     373,     0,   374,   375,     0,     0,  1739,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1353,     0,     0,     0,     0,     0,     0,  -239,
     376,     0,     0,    75,   377,     0,     0,     0,   279,     0,
     378,    77,    78,   379,   380,   381,   382,     0,     0,     0,
       0,     0,     0,   361,     0,  -179,     0,   362,     0,   363,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1116,     0,   364,    -2,     0,  1118,
       0,     0,  1119,  1120,  1121,  1122,  1123,  1124,  1125,  1126,
    1127,  1128,  1129,  1130,  -332,  1131,  1132,  1133,  1134,  1135,
       0,  1136,     0,   365,   366,     0,   463,     0,   368,  1137,
    1138,    64,    65,    66,    67,    68,    69,    70,   369,   370,
     358,  1139,   371,   372,   373,     0,   374,   375,     0,     0,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   376,     0,     0,    75,   377,     0,
       0,     0,   279,   963,   378,    77,    78,   379,   380,   381,
     382,     0,     0,     0,     0,     0,     0,     0,     0,  -179,
       4,   243,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,  1115,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,     0,     0,   361,     0,    45,    46,   362,
       0,   363,    47,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,    56,     0,  1116,    57,  1117,    -2,
       0,  1118,     0,     0,  1119,  1120,  1121,  1122,  1123,  1124,
    1125,  1126,  1127,  1128,  1129,  1130,  -332,  1131,  1132,  1133,
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
    1123,  1124,  1125,  1126,  1127,  1128,  1129,  1130,  -332,  1131,
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
     381,   382,  1873,     0,    -2,    -2,    -2,    -2,    -2,    -2,
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
       0,    -2,    -2,  1901,     0,    -2,    -2,    -2,    -2,    -2,
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
       0,     0,    -2,    -2,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,     0,     0,     0,
       0,    45,    46,     0,     0,     0,    47,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,    56,     0,
       0,    57,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    59,     0,     0,
       0,    60,    61,     0,     0,    62,    63,    64,    65,    66,
      67,    68,    69,    70,     0,     0,     0,    71,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      73,    74,     0,    75,    76,     0,     0,     0,     0,     0,
       0,    77,    78,   242,   243,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,     0,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,  -453,  -453,     0,  -453,
      45,    46,     0,  -453,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    13,    14,    15,    16,    17,     0,
      57,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,     0,     0,
       0,    61,    45,    46,    62,    63,    64,    65,    66,    67,
      68,    69,    70,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    57,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    73,
      74,     0,    75,   244,     0,     0,     0,  -764,     0,     0,
      77,    78,     4,   243,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,     0,     0,     0,     0,    45,
      46,     0,     0,     0,    47,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,    56,     0,     0,    57,
       0,     0,     0,     0,  -385,  -385,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    60,
       0,     0,     0,    62,    63,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -385,     0,     0,
       0,    75,    76,     0,     0,     0,     0,     0,     0,    77,
      78,     4,   243,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,     0,     0,    45,    46,
       0,     0,     0,    47,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,    56,     0,     0,    57,     0,
       0,     0,     0,  -386,  -386,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    60,     0,
       0,     0,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -386,     0,     0,     0,
      75,    76,     0,     0,     0,     0,     0,     0,    77,    78,
     242,   243,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,     0,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,  -453,  -453,     0,  -453,    45,    46,     0,
    -453,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    57,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    61,     0,
       0,    62,    63,    64,    65,    66,    67,    68,    69,    70,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    73,    74,     0,    75,
     244,     0,     0,  1329,     0,     0,     0,    77,    78,  1330,
       0,     0,    13,    14,    15,    16,    17,    18,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,     0,     0,
      45,    46,     0,     0,     0,    47,    48,    49,    50,    51,
      52,    53,    54,     0,     0,     0,     0,     0,     0,     0,
      57,  1331,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      60,     0,     0,     0,    62,    63,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1332,     0,
       0,     0,    75,   926,     0,     0,  1329,     0,     0,     0,
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
       0,  1512,     0,     0,     0,    75,   926,     0,     0,  1329,
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
       0,     0,     0,     0,  1513,     0,     0,     0,    75,   926,
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
       0,     0,     0,     0,     0,     0,     0,  1514,     0,     0,
       0,    75,   926,     0,     0,     0,     0,     0,     0,    77,
      78,   242,   243,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,     0,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,  -453,  -453,     0,  -453,    45,    46,
       0,  -453,     0,     0,     0,     0,     0,     0,     0,     0,
      13,    14,    15,    16,    17,     0,     0,    19,    57,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,  -452,  -452,     0,  -452,    45,    46,
       0,  -452,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    57,     0,
       0,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      75,   244,     0,     0,     0,     0,     0,     0,    77,    78,
     243,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,     0,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,  -453,  -453,     0,  -453,    45,    46,     0,  -453,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    57,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    61,     0,     0,
      62,    63,    64,    65,    66,    67,    68,    69,    70,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    73,    74,     0,    75,   244,
       0,     0,     0,  -768,     0,     0,    77,    78,   243,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,     0,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
    -453,  -453,     0,  -453,    45,    46,     0,  -453,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    57,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    61,     0,     0,    62,    63,
      64,    65,    66,    67,    68,    69,    70,     0,     0,     0,
       0,     0,     0,     0,  1353,     0,     0,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    73,    74,   361,    75,   244,     0,   362,
       0,   363,     0,     0,    77,    78,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1116,     0,   364,     0,
       0,  1118,  1798,  1799,  1119,  1120,  1121,  1122,  1123,  1124,
    1125,  1126,  1127,  1128,  1129,  1130,  -332,  1131,  1132,  1133,
    1134,  1135,     0,  1136,     0,   365,   366,     0,   463,     0,
     368,  1137,  1138,    64,    65,    66,    67,    68,    69,    70,
     369,   370,   358,  1139,   371,   372,   373,     0,   374,   375,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,  1353,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   376,     0,     0,    75,
     377,     0,     0,     0,   279,     0,   378,    77,    78,   379,
     380,   381,   382,   361,     0,     0,     0,   362,     0,   363,
       0,  -179,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1116,     0,   364,     0,     0,  1118,
       0,     0,  1119,  1120,  1121,  1122,  1123,  1124,  1125,  1126,
    1127,  1128,  1129,  1130,  -332,  1131,  1132,  1133,  1134,  1135,
       0,  1136,     0,   365,   366,     0,   463,     0,   368,  1137,
    1138,    64,    65,    66,    67,    68,    69,    70,   369,   370,
     358,  1139,   371,   372,   373,     0,   374,   375,     0,     0,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   376,     0,     0,    75,   377,     0,
       0,     0,   279,     0,   378,    77,    78,   379,   380,   381,
     382,     0,     0,     0,     0,     0,     0,     0,     0,  -179,
     243,     6,     7,     8,     9,    10,    11,    12,    13,    14,
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
       0,     0,     0,     0,     0,    72,     0,  1051,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -636,    75,   321,
       0,     0,    62,    63,     0,     0,    77,    78,   243,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      75,     0,     0,     0,    45,    46,     0,     0,     0,   319,
      48,    49,    50,    51,    52,    53,    54,     0,    13,    14,
      15,    16,    17,    18,    57,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,     0,     0,    45,    46,    62,    63,
       0,   319,    48,    49,    50,    51,    52,    53,    54,     0,
       0,     0,     0,     0,     0,     0,    57,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   320,    75,   321,     0,     0,
      62,    63,     0,     0,    77,    78,   243,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    75,     0,
       0,     0,    45,    46,     0,     0,     0,   319,    48,    49,
      50,    51,    52,    53,    54,     0,     0,     0,     0,     0,
       0,     0,    57,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    62,    63,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    72,     0,  1774,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    75,   321,     0,     0,     0,     0,
       0,     0,    77,    78,   243,     6,     7,     8,     9,    10,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,    72,
       0,  1776,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    75,   321,     0,     0,     0,     0,     0,     0,
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
      75,   301,     0,     0,     0,     0,     0,     0,    77,    78,
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
       0,     0,     0,     0,     0,     0,     0,     0,    75,   321,
       0,     0,     0,     0,     0,     0,    77,    78,   243,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,     0,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
    -453,  -453,     0,  -453,    45,    46,     0,  -453,     0,     0,
       0,     0,     0,     0,     0,     0,    13,    14,    15,    16,
      17,     0,     0,    19,    57,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
    -453,  -453,     0,  -453,    45,    46,     0,  -453,    62,    63,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    57,     0,     0,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    75,   244,     0,     0,
       0,     0,     0,     0,    77,    78,    13,    14,    15,    16,
      17,    18,   664,    19,   665,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
       0,     0,   361,     0,    45,    46,   362,     0,   363,    47,
      48,    49,    50,    51,    52,    53,    54,     0,     0,     0,
       0,     0,     0,     0,    57,   364,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   666,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   365,   366,     0,   367,     0,   368,    62,    63,
      64,    65,    66,    67,    68,    69,    70,   369,   370,   358,
       0,   371,   372,   373,     0,   374,   375,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   376,     0,     0,    75,   667,     0,     0,
       0,   279,     0,   378,    77,    78,   668,   669,   381,   382,
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
       0,     0,     0,     0,     0,     0,     0,   376,     0,   407,
      75,   408,     0,     0,     0,     0,     0,   378,    77,    78,
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
       0,   376,     0,     0,    75,   667,     0,     0,     0,   279,
       0,   378,    77,    78,   379,   380,   381,   382,    13,    14,
      15,    16,    17,    18,     0,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,   361,     0,    45,    46,   362,     0,
     363,    47,    48,    49,    50,    51,    52,    53,    54,     0,
       0,     0,     0,     0,     0,     0,    57,   364,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   365,   366,     0,   367,     0,   368,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   369,
     370,   358,     0,   371,   372,   373,     0,   374,   375,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   376,     0,     0,    75,   408,
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
       0,     0,    75,   438,     0,     0,     0,     0,     0,   378,
      77,    78,   379,   380,   381,   382,    13,    14,    15,    16,
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
      75,    76,     0,     0,     0,  -766,     0,     0,    77,    78,
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
      38,    39,    40,    41,    42,    43,    44,  -453,  -453,     0,
    -453,    45,    46,     0,  -453,     0,     0,     0,     0,     0,
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
       0,    77,    78,   557,   243,     6,     7,     8,     9,    10,
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
      50,    51,    52,    53,    54,     0,    13,    14,    15,    16,
      17,    18,    57,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
       0,     0,    75,     0,    45,    46,    62,    63,     0,    47,
      48,    49,    50,    51,    52,    53,    54,     0,     0,     0,
       0,     0,     0,     0,    57,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   932,    75,   926,     0,     0,    62,    63,
       0,     0,    77,    78,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    75,   286,     0,     0,
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
       0,    47,    48,    49,    50,    51,    52,    53,    54,     0,
       0,     0,     0,     0,     0,     0,    57,     0,     0,     0,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    75,    76,     0,     0,
      62,    63,     0,     0,    77,    78,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    75,   434,
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
       0,     0,     0,     0,     0,     0,     0,     0,    75,   321,
       0,     0,    62,    63,     0,     0,    77,    78,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      75,   286,     0,     0,     0,     0,     0,     0,    77,    78,
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
      75,   434,     0,     0,     0,     0,     0,     0,    77,    78,
     242,   243,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,     0,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,  -453,  -453,     0,  -453,    45,    46,     0,
    -453,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    13,    14,    15,    16,    17,    18,    57,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,     0,     0,     0,     0,    45,
      46,    62,    63,     0,   319,    48,    49,    50,    51,    52,
      53,    54,     0,    13,    14,    15,    16,    17,    18,    57,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,     0,     0,    75,
       0,    45,    46,    62,    63,     0,    47,    48,    49,    50,
      51,    52,    53,    54,     0,     0,     0,     0,     0,     0,
       0,    57,     0,     0,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    75,   301,     0,     0,    62,    63,     0,     0,    77,
      78,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    75,   926,     0,     0,     0,     0,     0,
       0,    77,    78,    13,    14,    15,    16,    17,    18,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,     0,     0,     0,
       0,    45,    46,     0,     0,     0,   319,    48,    49,    50,
      51,    52,    53,    54,     0,    13,    14,    15,    16,    17,
      18,    57,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,     0,
       0,     0,     0,    45,    46,    62,    63,     0,   319,    48,
      49,    50,    51,    52,    53,    54,     0,     0,     0,     0,
       0,     0,     0,    57,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    75,   926,     0,     0,    62,    63,     0,
       0,    77,    78,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    75,     0,     0,    13,    14,
      15,    16,    17,    77,    78,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,  -453,  -453,     0,  -453,    45,    46,     0,  -453,
       0,     0,     0,     0,     0,     0,     0,     0,    13,    14,
      15,    16,    17,     0,     0,    19,    57,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,  -453,  -453,     0,  -453,    45,    46,     0,  -453,
      62,    63,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    57,     0,     0,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    75,   301,
      62,    63,     0,     0,     0,     0,    77,    78,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    75,     0,
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
       0,     0,     0,     0,     0,   852,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -649,    75,   243,     6,     7,
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
       0,     0,     0,     0,  1694,     0,     0,     0,     0,   243,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,     0,    19,    75,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,     0,     0,     0,     0,    45,    46,     0,     0,     0,
     319,    48,    49,    50,    51,    52,    53,    54,     0,     0,
       0,     0,     0,     0,     0,    57,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   243,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    62,
      63,    19,     0,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,  -453,  -453,
       0,  -453,    45,    46,     0,  -453,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    75,     0,     0,
       0,     0,    57,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    62,    63,     0,     0,
       0,     0,    13,    14,    15,    16,    17,    18,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,     0,     0,
      45,    46,     0,     0,    75,    47,    48,    49,    50,    51,
      52,    53,    54,     0,     0,     0,     0,     0,     0,     0,
      57,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    61,     0,     0,    62,    63,    64,    65,    66,    67,
      68,    69,    70,     0,     0,     0,    13,    14,    15,    16,
      17,     0,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
    -453,  -453,    75,  -453,    45,    46,     0,  -453,     0,     0,
       0,     0,   361,     0,     0,     0,   362,     0,   363,     0,
       0,     0,     0,     0,    57,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   364,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    62,    63,
       0,     0,   365,   366,     0,   367,     0,   368,    62,    63,
      64,    65,    66,    67,    68,    69,    70,   369,   370,   358,
       0,   371,   372,   373,     0,   374,   375,     0,   361,     0,
       0,     0,   362,    72,   363,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    75,  1557,  1558,  1559,
       0,   364,     0,   376,  1719,     0,    75,   377,     0,     0,
       0,     0,     0,   378,    77,    78,   379,   380,   381,   382,
       0,     0,     0,     0,     0,     0,     0,     0,   365,   366,
       0,   367,     0,   368,  1813,    63,    64,    65,    66,    67,
      68,    69,    70,   369,   370,   358,     0,   371,   372,   373,
       0,   374,   375,     0,   361,     0,     0,     0,   362,    72,
     363,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1557,  1558,  1559,     0,   364,     0,   376,
    1814,     0,    75,   377,     0,     0,     0,     0,     0,   378,
      77,    78,   379,   380,   381,   382,     0,     0,     0,     0,
       0,     0,     0,     0,   365,   366,     0,   463,     0,   368,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   369,
     370,   358,     0,   371,   372,   373,   361,   374,   375,     0,
     362,     0,   363,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   364,
       0,     0,     0,     0,     0,   376,    74,     0,   464,   465,
       0,     0,     0,   466,     0,   378,    77,    78,   379,   380,
     381,   382,     0,     0,     0,     0,   365,   366,     0,   367,
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
       0,   364,     0,     0,     0,     0,     0,   376,     0,  1804,
      75,   377,     0,     0,     0,     0,     0,   378,    77,    78,
     379,   380,   381,   382,     0,     0,     0,     0,   365,   366,
       0,   367,     0,   368,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   369,   370,   358,     0,   371,   372,   373,
     361,   374,   375,     0,   362,     0,   363,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   364,     0,     0,     0,     0,     0,   376,
    1809,     0,    75,   377,     0,     0,     0,     0,     0,   378,
      77,    78,   379,   380,   381,   382,     0,     0,     0,     0,
     365,   366,     0,   367,     0,   368,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   369,   370,   358,     0,   371,
     372,   373,   361,   374,   375,     0,   362,     0,   363,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   364,     0,     0,     0,     0,
       0,   376,  1818,     0,    75,   377,     0,     0,     0,     0,
       0,   378,    77,    78,   379,   380,   381,   382,     0,     0,
       0,     0,   365,   366,     0,   367,     0,   368,    62,    63,
      64,    65,    66,    67,    68,    69,    70,   369,   370,   358,
       0,   371,   372,   373,   361,   374,   375,     0,   362,     0,
     363,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   364,     0,     0,
       0,     0,     0,   376,  1895,     0,    75,   377,     0,     0,
       0,     0,     0,   378,    77,    78,   379,   380,   381,   382,
       0,     0,     0,     0,   365,   366,     0,   367,     0,   368,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   369,
     370,   358,     0,   371,   372,   373,   361,   374,   375,     0,
     362,     0,   363,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   364,
       0,     0,     0,     0,     0,   376,  1897,     0,    75,   377,
       0,     0,     0,     0,     0,   378,    77,    78,   379,   380,
     381,   382,     0,     0,     0,     0,   365,   366,     0,   367,
       0,   368,    62,    63,    64,    65,    66,    67,    68,    69,
      70,   369,   370,   358,     0,   371,   372,   373,   361,   374,
     375,     0,   362,     0,   363,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   364,     0,     0,     0,     0,     0,   376,  1948,     0,
      75,   377,     0,     0,     0,     0,     0,   378,    77,    78,
     379,   380,   381,   382,     0,     0,     0,     0,   365,   366,
       0,   367,     0,   368,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   369,   370,   358,     0,   371,   372,   373,
     361,   374,   375,     0,   362,     0,   363,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   364,     0,     0,     0,     0,     0,   376,
    1950,     0,    75,   377,     0,     0,     0,     0,     0,   378,
      77,    78,   379,   380,   381,   382,     0,     0,     0,     0,
     365,   366,     0,   367,     0,   368,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   369,   370,   358,     0,   371,
     372,   373,   361,   374,   375,     0,   362,     0,   363,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   364,     0,     0,     0,     0,
       0,   376,  1952,     0,    75,   377,     0,     0,     0,     0,
       0,   378,    77,    78,   379,   380,   381,   382,     0,     0,
       0,     0,   365,   366,     0,   367,     0,   368,    62,    63,
      64,    65,    66,    67,    68,    69,    70,   369,   370,   358,
       0,   371,   372,   373,   361,   374,   375,     0,   362,     0,
     363,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   364,     0,     0,
       0,     0,     0,   376,  1955,     0,    75,   377,     0,     0,
       0,     0,     0,   378,    77,    78,   379,   380,   381,   382,
       0,     0,     0,     0,   365,   366,     0,   367,     0,   368,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   369,
     370,   358,     0,   371,   372,   373,   361,   374,   375,     0,
     362,     0,   363,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   364,
       0,     0,     0,     0,     0,   376,  1957,     0,    75,   377,
       0,     0,     0,     0,     0,   378,    77,    78,   379,   380,
     381,   382,     0,     0,     0,     0,   365,   366,     0,   367,
       0,   368,    62,    63,    64,    65,    66,    67,    68,    69,
      70,   369,   370,   358,     0,   371,   372,   373,   361,   374,
     375,     0,   362,     0,   363,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   364,     0,     0,     0,     0,     0,   376,  1999,     0,
      75,   377,     0,     0,     0,     0,     0,   378,    77,    78,
     379,   380,   381,   382,     0,     0,     0,     0,   365,   366,
       0,   367,     0,   368,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   369,   370,   358,     0,   371,   372,   373,
     361,   374,   375,     0,   362,     0,   363,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   364,     0,     0,     0,     0,     0,   376,
    2001,     0,    75,   377,     0,     0,     0,     0,     0,   378,
      77,    78,   379,   380,   381,   382,     0,     0,     0,     0,
     365,   366,     0,   367,     0,   368,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   369,   370,   358,     0,   371,
     372,   373,   361,   374,   375,     0,   362,     0,   363,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   364,     0,     0,     0,     0,
       0,   376,  2003,     0,    75,   377,     0,     0,     0,     0,
       0,   378,    77,    78,   379,   380,   381,   382,     0,     0,
       0,     0,   365,   366,     0,   367,     0,   368,    62,    63,
      64,    65,    66,    67,    68,    69,    70,   369,   370,   358,
       0,   371,   372,   373,   361,   374,   375,     0,   362,     0,
     363,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   364,     0,     0,
       0,     0,     0,   376,  2026,     0,    75,   377,     0,     0,
       0,     0,     0,   378,    77,    78,   379,   380,   381,   382,
       0,     0,     0,     0,   365,   366,     0,   367,     0,   368,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   369,
     370,   358,     0,   371,   372,   373,   361,   374,   375,     0,
     362,     0,   363,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   364,
       0,     0,     0,     0,     0,   376,  2028,     0,    75,   377,
       0,     0,     0,     0,     0,   378,    77,    78,   379,   380,
     381,   382,     0,     0,     0,     0,   365,   366,     0,   367,
       0,   368,    62,    63,    64,    65,    66,    67,    68,    69,
      70,   369,   370,   358,     0,   371,   372,   373,   361,   374,
     375,     0,   362,     0,   363,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   364,     0,     0,     0,     0,     0,   376,  2030,     0,
      75,   377,     0,     0,     0,     0,     0,   378,    77,    78,
     379,   380,   381,   382,     0,     0,     0,     0,   365,   366,
       0,   367,     0,   368,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   369,   370,   358,     0,   371,   372,   373,
     361,   374,   375,     0,   362,     0,   363,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   364,     0,     0,     0,     0,     0,   376,
       0,     0,    75,   377,     0,     0,     0,     0,     0,   378,
      77,    78,   379,   380,   381,   382,     0,     0,     0,     0,
     365,   366,     0,   367,     0,   368,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   369,   370,   358,     0,   371,
     372,   373,   361,   374,   375,     0,   362,     0,   363,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   364,     0,     0,     0,     0,
       0,   650,     0,     0,    75,   377,     0,     0,     0,     0,
       0,   378,    77,    78,   379,   380,   381,   382,     0,     0,
       0,     0,   365,   366,     0,   367,     0,   368,    62,    63,
      64,    65,    66,    67,    68,    69,    70,   369,   370,   358,
       0,   371,   372,   373,   361,   374,   375,     0,   362,     0,
     363,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   364,     0,     0,
       0,     0,     0,   655,     0,     0,    75,   377,     0,     0,
       0,     0,     0,   378,    77,    78,   379,   380,   381,   382,
       0,     0,     0,     0,   365,   366,     0,   367,     0,   368,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   369,
     370,   358,     0,   371,   372,   373,   361,   374,   375,     0,
     362,     0,   363,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   364,
       0,     0,     0,     0,     0,   661,     0,     0,    75,   377,
       0,     0,     0,     0,     0,   378,    77,    78,   379,   380,
     381,   382,     0,     0,     0,     0,   365,   366,     0,   367,
       0,   368,    62,    63,    64,    65,    66,    67,    68,    69,
      70,   369,   370,   358,     0,   371,   372,   373,   361,   374,
     375,     0,   362,     0,   363,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   364,     0,     0,     0,     0,     0,   376,     0,     0,
      75,   377,     0,     0,     0,     0,     0,   378,   866,    78,
     379,   380,   381,   382,     0,     0,     0,     0,   365,   366,
       0,   367,     0,   368,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   369,   370,   358,     0,   371,   372,   373,
     361,   374,   375,     0,   362,     0,   363,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   364,     0,     0,     0,     0,     0,   376,
       0,     0,    75,   377,     0,     0,     0,     0,     0,   378,
     439,    78,   379,   380,   381,   382,     0,     0,     0,     0,
     365,   366,     0,   367,     0,   368,  1890,    63,    64,    65,
      66,    67,    68,    69,    70,   369,   370,   358,     0,   371,
     372,   373,     0,   374,   375,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   376,     0,     0,    75,   377,     0,     0,     0,     0,
       0,   378,    77,    78,   379,   380,   381,   382
};

static const yytype_int16 yycheck[] =
{
       1,    73,     1,     4,    95,    73,    73,    73,   256,   162,
     173,   693,   177,   241,   150,   376,   324,   466,   162,   219,
     678,   207,   322,   283,   222,   875,    75,   616,    58,   339,
       4,   131,  1670,   343,  1669,  1738,  1355,   178,  1669,   339,
     603,   635,   603,   343,   515,   516,   766,   219,   764,  1669,
     162,  1125,   229,   163,    55,    56,   607,    58,  1209,    58,
     219,   609,   965,     1,  1567,   766,     4,   219,     1,    82,
     178,   671,    73,   367,    82,   139,   737,   254,  1802,    70,
     861,    82,   228,    82,   767,   231,    73,   264,   784,    90,
     773,   994,   290,   291,    95,   295,    95,    98,   219,    98,
     236,   102,    19,   102,  1798,   603,   802,   253,   845,   181,
     219,   211,   526,   181,   181,   145,    70,   263,   764,    95,
      58,    73,   536,   295,   219,    58,     1,   191,   102,   220,
     764,    75,    76,     0,   147,   872,   295,   764,   446,   140,
     440,   149,   143,   295,   145,     0,   145,   219,  1786,   150,
     115,   219,   219,   219,  1057,   156,  1287,  1288,   149,   574,
      98,    82,   163,   577,   102,     0,   637,    87,    87,   463,
     783,   784,   868,   220,   295,    73,   157,    73,    96,   180,
     181,   180,   149,     1,   284,  1099,   295,   174,   153,   802,
    1104,     1,    87,   174,   195,     1,   195,    73,   174,   153,
     295,   764,   861,   764,   205,   296,   173,   145,    87,   210,
     322,  1714,   145,   157,   316,   766,  1109,  1245,   219,   220,
     768,   220,   174,   295,   772,   274,   147,   295,   295,   295,
       1,   149,   245,   781,   782,   236,   155,   157,   157,  1563,
      58,   923,  1936,   115,   245,   131,   245,   152,    58,   296,
     498,  1109,    58,   355,   255,   868,     1,   258,   179,   258,
       1,  1175,   157,     4,   265,  1989,   764,  1905,   173,   951,
     154,  1590,  1423,   744,   275,   276,   376,   278,   157,   165,
      98,   156,   592,   181,   484,   181,   472,   595,   882,   302,
    1109,   157,   592,     4,   295,   296,   102,   296,  1355,  1356,
    2024,   153,   303,   563,  1020,   181,   158,  1027,   174,   310,
     311,   619,   484,    58,   315,  2018,   616,    58,   626,   629,
     258,   219,   916,   219,   245,   484,  1027,   145,   440,   629,
     691,  1969,   484,  1968,   444,   145,  1117,  1968,  1021,   145,
     940,   376,  1003,   219,    55,    56,   897,   348,  1968,    59,
      60,     1,   353,   174,  1939,   356,   357,   153,   535,   149,
     506,   102,   158,   484,  1101,  1146,   104,   105,   783,   784,
     570,  2009,   436,  1069,  1020,   484,   149,   513,   130,    90,
    1965,   302,   528,   519,   155,   156,  1020,   802,   534,   484,
     192,     1,   538,  1020,     4,  1719,  1720,   410,   570,    70,
     145,   322,   493,   149,   145,   505,  1991,   131,    58,  1323,
     510,   570,   484,   149,   152,   464,   484,   484,   570,   420,
     172,   420,   149,  1451,  1452,  1453,   155,   527,   842,   140,
     574,    70,   143,  1326,  1327,  1328,   165,   537,    75,    76,
     258,   165,   443,   444,   174,   156,   493,   157,    58,   570,
     155,    73,   163,   868,   455,   456,  1069,  1020,  1117,  1020,
     165,   570,   574,   464,   174,   466,   149,    89,  1326,  1327,
    1328,    73,    82,   283,   146,   570,  1027,   283,   149,   120,
      70,   572,   153,   484,   106,   157,    88,  1146,  1562,   410,
    1814,  1815,   493,  1567,   493,   145,  1044,   155,   570,   210,
      70,   173,   570,   570,   616,   230,   324,  1326,  1327,  1328,
     149,   152,   513,   155,   153,  1198,   174,   155,   519,   440,
     157,   936,  1020,  1654,  1655,   572,     3,    70,    56,   139,
      70,    59,    60,  1590,    62,   145,   174,   147,   283,     3,
      70,   155,   155,  1446,   255,   853,   576,   155,   997,   155,
     650,   155,   165,   718,   265,   655,   557,   155,   559,   149,
     174,   661,    70,   153,   275,   276,   174,   278,   174,   570,
     174,   572,   131,   572,   884,   576,   174,   576,   886,   149,
     680,   191,    70,   153,   884,   586,   484,   265,   484,   590,
     152,   151,   303,  1276,   515,   516,   156,  1255,   276,   310,
     311,   160,   161,  1192,   315,   791,   149,   151,   484,   149,
     153,  1205,   156,   153,   155,   650,   155,   652,   653,   149,
     655,   149,   623,   153,   165,   149,   661,   151,   446,   664,
     665,   666,   242,   283,   635,   245,   777,   348,   576,   148,
    1714,    70,   353,   576,    70,   356,   155,   788,    12,    13,
      14,    15,    16,   155,  1069,   149,   766,   157,    70,   824,
    1717,   149,   156,  1345,   155,   153,   125,   126,   804,   777,
     719,  1521,   174,   283,     4,     5,     6,     7,     8,     9,
      10,    11,   683,   174,   685,   157,   687,   823,   413,   155,
     691,   149,   302,   694,   149,   616,  1355,  1356,   844,   165,
     192,    82,   157,   155,   429,   155,    70,   432,  1366,   157,
     169,   170,   155,  1184,    95,   165,   637,    98,   719,   521,
     149,   102,   174,   149,   153,  1856,   173,   153,    70,   829,
      70,   174,   443,    70,  1637,   151,  1639,   149,  1812,   155,
     840,   153,   560,   843,   455,   456,    70,   847,   151,    70,
    1824,   151,   149,   563,   171,   561,   156,   563,   576,   162,
     163,   160,  1211,   764,   489,   766,   576,   131,   167,   168,
     576,   104,   105,   914,   143,   144,   145,   778,    12,    13,
      14,    15,    16,   593,   785,  1443,   149,   155,  1237,   157,
     791,   593,   936,   794,   157,   151,   165,   153,   908,   180,
     410,   619,   803,   804,   805,  1220,   914,   149,  1056,   149,
     149,   153,   149,   153,   195,  1889,   153,  1874,   563,   115,
     148,   151,   823,   744,   936,   149,   436,   155,   149,   153,
     322,   576,   153,   325,   632,   576,    70,   129,   149,   220,
     155,   150,     9,     3,   174,  1902,   557,   339,   157,   990,
     151,   343,    12,    13,    14,    15,    16,   149,   859,   860,
     861,   153,   152,   153,   245,   151,   156,   129,   160,   161,
     156,   151,  1287,    46,    47,   586,    49,   258,  1064,   590,
      53,   882,   149,  1940,   151,   151,   153,   149,  1196,   155,
    1088,   153,  1192,     3,   698,   699,   700,   131,   160,   161,
     874,   151,    12,    13,    14,    15,    16,   908,   104,   105,
      70,   912,   623,   563,  1363,   916,  1026,  1027,   720,   353,
     151,   922,   356,   861,   635,  1066,   576,   151,   861,    56,
     151,  1590,    59,    60,   101,    62,   874,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   151,   557,   440,   149,
     155,   761,   149,   563,   875,   129,   957,   149,   149,   761,
      70,   149,   153,   151,   965,   153,   576,   151,    21,  1330,
     149,   155,   683,  1201,   685,   149,   687,   155,   129,   153,
     691,   783,   784,   694,   149,   151,   160,   161,   153,   155,
    1405,   789,  1441,   994,   151,   152,   997,   151,   149,   173,
     802,   155,   153,  1103,   705,   706,   707,   708,   719,   160,
     161,   736,   737,   149,   151,   151,  1116,   153,   155,  1020,
     155,   746,  1185,  1186,   749,  1026,  1027,  1621,    96,   521,
     143,   144,   145,  1133,   149,   853,   151,   151,   153,   420,
     151,   155,  1178,   861,   155,   149,   864,   143,   144,   145,
     149,   861,   165,   151,   153,   861,  1057,   155,  1717,   155,
     149,   174,   162,   163,   153,   155,   868,   778,   886,   165,
    1318,   151,   123,   124,   785,   155,  1220,   103,   174,   149,
    1192,   107,   574,   153,   110,   810,   112,   151,   155,   814,
     149,   155,   803,   818,   805,  1355,  1109,  1106,  1107,  1108,
     592,   593,    12,    13,    14,    15,    16,   118,  1220,   120,
     121,   122,   493,  1512,  1513,  1514,   861,  1258,  1119,   917,
     861,  1122,  1123,  1124,   616,    12,    13,    14,    15,    16,
     151,    87,   151,   874,   155,  1288,   155,   629,   149,   151,
    1973,   152,   153,   155,  1977,  1146,   157,   158,   859,   860,
     861,  1152,   151,   157,  1295,  1296,   155,  1839,  1752,  1160,
      70,   157,  1163,  1164,  1163,  1164,  1167,   127,   128,   154,
     155,   882,   108,   109,   110,   111,   112,  1178,     1,  1117,
     148,     4,   151,    70,  1117,   157,   155,  1295,  1296,  1224,
    1164,    12,    13,    14,    15,    16,    17,   908,   151,   151,
     154,   912,   155,   155,  1205,   916,   154,   155,  1146,   151,
    1211,   861,   151,  1146,   151,  1874,   155,   151,   155,   151,
    1221,   155,   248,   155,   157,  1163,  1164,   157,   720,    12,
      13,    14,    15,    16,    17,    58,  1237,   151,   157,  1654,
     151,   155,   129,  1902,  1245,  1336,  1346,  1347,   160,   161,
     173,   861,    12,    13,    14,    15,    16,    17,   115,    82,
     149,  1405,   149,  1184,   874,   149,   153,  1069,   151,   761,
     149,  1192,   155,   160,   161,   155,  1277,   159,  1003,   102,
     166,  1940,   154,   155,  1384,   154,   155,   154,   155,  1336,
     143,   144,   145,  1405,   154,   155,   155,   156,   161,  1117,
     326,   327,   155,   329,   129,   331,   171,  1117,   154,   155,
     152,  1117,   165,  1326,  1327,  1328,   139,  1330,  1331,   154,
     155,   174,   145,   153,   147,  1525,   154,   155,  1146,   131,
    1590,   151,   154,   155,   149,  1336,  1146,   154,   155,  1340,
    1146,   151,  1343,   154,   155,  1163,   154,   155,   151,  1074,
     154,   155,  1077,  1525,   151,  1329,   179,   151,  1164,   154,
     155,   151,  1363,   154,   155,   154,  1525,   131,   191,   154,
     155,   155,  1117,  1525,   154,   155,  1117,   156,  1196,   154,
     155,   151,  1383,   156,  1385,   151,  1385,   155,   156,  1187,
     149,  1329,   884,   154,   155,   887,   154,   155,    75,    76,
     151,  1146,   155,   156,  1525,  1146,  1246,  1247,  1119,   703,
     704,  1122,  1123,  1124,   151,  1213,  1525,  1355,  1356,   242,
     701,   702,   245,  1164,  1589,   709,   710,   250,  1519,   151,
    1525,  1532,  1533,   151,   154,  1146,  1527,  1185,  1186,   153,
    1441,  1152,   157,    68,   936,  1446,   157,  1385,  1601,  1160,
    1451,  1452,  1453,   157,   157,  1429,  1167,  1601,   154,  1525,
     283,   149,    76,   157,  1262,   154,    17,  1117,   173,   155,
     157,   173,  1519,    12,    13,    14,    15,    16,    17,   302,
    1527,  1581,   149,   174,  1684,  1287,  1288,   151,   151,  1601,
     174,  1429,   157,   154,  1205,   157,  1146,   154,    17,  1109,
     148,   151,  1655,   148,   151,   151,   532,  1117,   151,   151,
    1221,   151,  1684,   151,   151,    68,   157,   151,  1519,   174,
     173,   157,   157,   151,  1525,  1684,  1527,   151,   151,   148,
     157,  1831,  1684,  1534,   151,   151,  1146,  1355,  1356,  1264,
     155,   154,   151,   151,   155,  1355,  1356,  1548,  1273,  1355,
    1356,   151,   151,   376,   151,   151,   151,   151,   148,   151,
    1561,   154,   151,  1684,   151,   151,   151,  1385,  1733,   151,
     151,   151,   151,   148,   151,  1684,   154,   151,   151,     9,
     149,   149,  1556,   155,   149,   173,   149,   410,  1329,  1684,
     149,   149,    13,    72,  1629,   205,  1687,  1598,  1906,   156,
    1521,   155,    89,  1903,   174,   154,   156,   174,  1744,   154,
    1355,  1356,   174,   436,  1355,  1356,   148,  1555,  1556,   174,
    1621,   148,  1420,   157,   155,   151,   174,  1425,   174,  1340,
    1604,   154,  1343,   151,   155,   155,  1637,   151,  1639,   155,
    1687,   154,   151,   151,  1442,   148,   148,    78,   149,   174,
     149,   149,  1590,   101,   148,   151,   149,  1798,   106,   107,
     108,   109,   110,   111,   112,   151,  1604,   174,    17,   174,
    1835,   101,  1383,   148,   104,   105,   106,   107,   108,   109,
     110,   111,   112,  1684,   174,   174,  1687,   148,  1429,   174,
    1798,   174,   515,   516,   174,   155,   155,  1698,   148,   154,
    1192,  1702,   154,  1856,  1795,  1355,  1356,   154,   154,   148,
      59,    60,    61,    62,   157,  1716,  1326,  1327,  1328,  1329,
    1330,  1331,   151,   156,   156,  1726,   118,   151,  1220,   148,
     174,   151,   154,   151,   557,  1537,   151,  1555,   561,   151,
     563,   154,  1743,  1744,   148,  1355,  1356,   156,  1795,   155,
    1556,  1752,   101,   576,   151,   149,   151,   106,   107,   108,
     109,   110,   111,   112,   113,   149,  1704,   155,  1968,  1905,
     149,   107,  1590,   154,  1572,   148,   154,   154,   148,  1717,
    1590,   157,  1163,  1164,  1590,   148,   151,   151,   154,   151,
    1515,   151,   151,  1934,  1795,  1936,  1968,   151,    73,    73,
     174,  1802,   174,   148,   153,  1806,   149,   174,  1973,  1968,
    1811,   151,  1977,  1978,   637,  1556,  1968,   151,  1831,  1429,
      88,   154,   154,  1534,   148,   148,  1934,   650,  1936,   652,
     653,   151,   655,  1631,  1975,  1836,   151,  1548,   661,   151,
     151,   664,   665,   666,   151,  1590,  2011,  1968,   151,  1590,
    1561,   153,  1654,  1655,   464,    73,   466,   165,   152,  1968,
     174,   165,    73,  1604,   174,   156,  2007,  1975,   174,   151,
     148,  2036,   151,  1968,   151,  2040,  1877,   148,  1969,   151,
    1881,   150,   155,   148,   165,   165,  1704,  1598,   101,   156,
    1903,   149,   155,  1894,    73,   149,  1968,  2062,   148,  1717,
    1968,  1968,  1968,   165,  1905,   165,  1907,  1717,   150,   154,
    1621,  1717,   107,  1405,  2055,   174,   107,  1918,  2009,  1920,
    1921,   744,  1969,   174,   150,     1,   151,   156,     4,   151,
     151,   148,   148,   174,   149,  2035,  1874,   830,    73,   151,
    1590,   151,  1943,   174,  1669,  1670,   174,  2055,  1629,  1345,
     378,  2051,  1252,   670,   711,  1753,   712,  1146,   713,  1135,
     714,  2024,  2009,   715,  1902,  1590,  1936,  1968,  1969,   409,
    1969,  1725,  1717,  1965,   364,  1717,  1717,  2019,  1979,  1840,
    1590,  2018,    58,  2006,  1978,  1582,  1582,  1698,  1989,  1903,
    2040,  1702,  1902,    48,  1604,  1167,   889,    73,   250,   389,
     390,  1519,  1940,  1795,  1385,  1716,    82,  1864,  2009,  1331,
    2009,  1533,  1160,   791,   878,  1726,   473,  1604,  1429,    95,
     410,  2022,    98,  2024,     0,   586,   102,   922,   736,   736,
     736,    -1,  1743,    -1,    -1,    -1,    -1,    -1,   861,    -1,
      -1,  1752,  2043,    -1,    -1,    -1,    -1,    -1,  2049,  1847,
     440,   874,   875,    -1,  1856,    -1,  1874,    -1,  2059,    -1,
      -1,  1786,  2063,   139,  1874,    -1,    -1,  1717,  1874,   145,
      -1,   147,  2073,    -1,   150,   151,    82,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1902,    -1,   162,    -1,    -1,    -1,
      -1,  1802,  1902,    -1,    -1,  1806,  1902,    -1,    -1,    -1,
    1811,    98,    -1,   179,   180,   181,   999,  1717,    -1,    -1,
      -1,    -1,   109,    -1,    -1,   191,   192,    -1,    -1,   195,
      -1,    -1,  1940,  1016,  1017,  1836,    -1,    -1,    -1,  1874,
    1940,    -1,    -1,  1874,  1940,  1860,    -1,    -1,    -1,  1864,
      -1,   147,    -1,   219,   220,    -1,    -1,    -1,    -1,   552,
      -1,    -1,    -1,    -1,   151,    -1,   162,  1902,    -1,    -1,
     236,  1902,    -1,    -1,    -1,    -1,  1877,    -1,    -1,   245,
    1881,    -1,    -1,   179,    -1,    -1,    -1,    12,    -1,    -1,
    1905,   791,   258,  1894,   794,    -1,   192,    12,    13,    14,
      15,    16,    -1,    -1,    -1,  1940,  1907,    -1,   195,  1940,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1918,    -1,  1920,
    1921,    -1,   288,    -1,    -1,    -1,    -1,    -1,   294,   295,
     296,    -1,    -1,    -1,  1874,    -1,   302,    -1,    -1,    -1,
      -1,    -1,  1943,    -1,    -1,    -1,    -1,    -1,    -1,   245,
      -1,    -1,    -1,  1968,  1969,    70,   322,   323,   324,    -1,
      -1,    86,  1902,    -1,    12,    13,    14,    15,    16,    -1,
      -1,   258,    -1,   339,  1874,    -1,   101,   343,  1979,   104,
     105,   106,   107,   108,   109,   110,   111,   112,  1989,    -1,
      -1,    -1,    -1,    -1,  2009,    -1,  1109,    -1,    -1,    -1,
    1940,   288,  1902,    -1,  1117,    -1,   302,   294,    -1,    -1,
     376,    -1,    -1,    -1,   129,    -1,    -1,   697,    -1,    -1,
      -1,  2022,    70,  2024,    -1,    -1,   322,    -1,    -1,    -1,
      -1,    -1,    -1,  1146,   149,    -1,    -1,   324,   153,    -1,
    1940,    -1,  2043,    -1,   410,   160,   161,   413,  2049,  1831,
      -1,  1164,    -1,    -1,   420,    -1,    -1,   957,  2059,    -1,
      -1,    -1,  2063,    -1,    -1,   965,    -1,    -1,    -1,    -1,
     436,  1184,  2073,    -1,   440,    -1,    -1,     3,   444,    -1,
     446,   129,   101,    -1,    -1,  1268,  1269,   106,   107,   108,
     109,   110,   111,   112,   994,    -1,    -1,   997,    -1,  1282,
    1283,   149,    -1,    -1,    -1,   153,    -1,    -1,    -1,    -1,
      -1,  1224,   160,   161,   410,    -1,    -1,    -1,   484,    -1,
      -1,  1903,    -1,    -1,    -1,    -1,    -1,   493,    -1,    -1,
     149,   150,    -1,  1316,  1317,    -1,    -1,   830,    76,    -1,
      -1,    -1,    -1,    -1,   440,    -1,    -1,   513,    -1,   515,
     516,    -1,    -1,   519,    -1,   521,    -1,  1057,    -1,   446,
      -1,    -1,    -1,   101,    -1,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   101,    -1,  1119,   104,   105,
     106,   107,   108,   109,   110,   111,   112,    -1,    -1,    -1,
      -1,    -1,    -1,   559,    -1,    -1,   889,    -1,    -1,    -1,
      -1,    -1,    -1,   129,   570,    -1,   572,    -1,   574,    -1,
     576,    -1,    -1,  1326,  1327,  1328,  1329,  1330,  1331,   515,
     516,    -1,    -1,   149,   150,   521,   592,   593,    -1,   595,
     156,    -1,    -1,   520,   160,   161,   174,   603,    -1,    -1,
      -1,   607,  1355,  1356,    -1,    -1,    -1,    -1,    -1,    -1,
     616,    -1,   932,   540,    -1,    -1,    -1,   937,    -1,    -1,
     626,    -1,    -1,   629,    -1,    -1,    -1,    -1,   948,    -1,
      -1,   637,    -1,   560,    -1,    -1,    -1,    -1,   574,    -1,
      -1,    -1,    -1,    -1,   650,    -1,   652,   653,    -1,   655,
      -1,    -1,    -1,    -1,    -1,   661,    -1,   593,   664,   665,
     666,    -1,    -1,    -1,    -1,    -1,   999,    -1,   595,    -1,
      -1,  1211,    -1,    -1,    -1,    -1,  1429,    -1,  1501,  1502,
     616,    -1,    -1,  1016,  1017,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   619,    -1,    -1,    -1,    -1,  1237,    -1,   626,
      -1,   637,    -1,    70,    -1,  1245,     4,     5,     6,     7,
       8,     9,    10,    11,   720,    -1,    12,    13,    14,    15,
      16,     4,     5,     6,     7,     8,     9,    10,    11,    -1,
     736,   737,   659,   660,   101,    -1,    -1,    -1,   744,   106,
     107,   108,   109,   110,   111,   112,    12,    13,    14,    15,
      16,    -1,    -1,    -1,    -1,   761,    -1,    -1,   764,    -1,
     766,    -1,   129,    -1,    -1,    63,    -1,    -1,  1521,    -1,
      -1,    -1,    -1,    -1,    70,    -1,    -1,   783,   784,    -1,
      -1,    -1,   149,   150,   720,    -1,    -1,    -1,    -1,    -1,
    1613,    -1,    -1,   160,   161,    -1,   802,  1117,   804,    -1,
      -1,  1383,    -1,  1556,    70,   101,    -1,    -1,   744,    -1,
     106,   107,   108,   109,   110,   111,   112,   823,    -1,    -1,
      -1,  1644,    -1,  1363,    -1,   761,  1649,  1650,    -1,    -1,
      -1,    -1,    -1,   129,    -1,   101,    -1,  1590,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   783,   784,    -1,
      -1,  1604,    -1,   149,   150,   861,    -1,    -1,    -1,    -1,
      -1,    -1,   868,   129,   160,   161,   802,    -1,   874,   875,
      -1,    -1,  1192,    -1,     1,    -1,  1629,     4,   884,    17,
     886,    -1,    -1,   149,   150,    -1,    -1,   153,    -1,    -1,
      -1,   897,    -1,    -1,   160,   161,    -1,    -1,    -1,    -1,
      -1,  1441,    -1,    -1,    -1,    -1,  1446,  1227,  1228,  1229,
      -1,  1451,  1452,  1453,  1234,  1235,    -1,    55,    56,    57,
      58,    59,    60,    61,    62,    -1,   853,    -1,    -1,    -1,
     936,    58,   868,    -1,    -1,  1268,  1269,   864,    -1,   875,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1282,
    1283,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,   886,
      -1,    -1,    -1,    -1,  1717,    -1,  1548,    70,    -1,    -1,
      -1,    98,    -1,    -1,    -1,   102,    -1,    -1,    -1,  1561,
      -1,    -1,    -1,  1316,  1317,    -1,    -1,    12,    13,    14,
      15,    16,    -1,    -1,    -1,    -1,    -1,  1003,   101,    -1,
     936,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      -1,    -1,   139,    -1,  1020,    -1,  1598,    -1,   145,    -1,
     147,  1027,   101,    -1,   151,    -1,   129,   106,   107,   108,
     109,   110,   111,   112,   161,   162,   163,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    70,   149,   150,    -1,    -1,
     153,    -1,   179,    -1,    -1,    -1,    -1,   160,   161,    -1,
      -1,    -1,    -1,  1069,   191,   192,    -1,    -1,   195,    -1,
      -1,   150,    -1,    -1,   153,    -1,   101,    -1,    -1,   104,
     105,   106,   107,   108,   109,   110,   111,   112,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1637,    -1,  1639,
      -1,    -1,    -1,  1109,   129,    -1,    -1,    -1,    -1,    -1,
      -1,  1117,    -1,    -1,    -1,   242,  1698,    -1,   245,    -1,
    1702,  1874,    -1,    -1,   149,   150,    -1,    -1,   153,    -1,
      -1,   258,    -1,  1069,  1716,   160,   161,    -1,    -1,    -1,
    1146,    -1,    -1,    -1,  1726,    -1,    -1,    -1,   275,  1902,
      -1,    -1,    -1,    -1,    -1,    -1,   283,  1163,  1164,    -1,
      -1,   288,    -1,  1986,    -1,    -1,    -1,   294,  1501,  1502,
      -1,    -1,  1178,    -1,    -1,   302,    -1,    -1,  1184,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1192,  1940,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   322,    -1,   324,   325,    -1,
      -1,    -1,    -1,    -1,  1537,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   339,    -1,  1220,    -1,   343,    -1,  1224,    -1,
    1802,    -1,    -1,    -1,  1806,    -1,    70,    -1,    -1,  1811,
      -1,    -1,    -1,    -1,   101,    -1,  1163,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    -1,    -1,  1184,   376,
      -1,    -1,    -1,    -1,  1836,    -1,  1192,   101,    -1,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,  1196,
      -1,    -1,    -1,    -1,    -1,  1595,    -1,    -1,    -1,    -1,
    1613,  1287,  1288,   410,  1220,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1877,    -1,    -1,    -1,  1881,
      -1,    -1,    -1,    -1,    -1,   149,   150,   174,    -1,   436,
      -1,  1644,  1894,   440,    -1,    -1,  1649,  1650,    -1,   446,
    1326,  1327,  1328,  1329,  1330,  1331,    -1,    -1,    -1,    -1,
    1336,  1337,    -1,    -1,    -1,    -1,  1918,   101,  1920,  1921,
     104,   105,   106,   107,   108,   109,   110,   111,   112,  1355,
    1356,  1287,  1288,    -1,    -1,    -1,    -1,   101,    -1,    -1,
      -1,  1943,   106,   107,   108,   109,   110,   111,   112,   113,
      63,    64,    65,    66,    -1,    -1,    -1,    -1,    -1,  1385,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   515,   516,
      -1,    -1,    -1,   520,   521,    -1,   160,  1979,    -1,  1405,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1989,   101,   153,
    1337,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      -1,    -1,    -1,  1429,    -1,   552,    -1,    -1,    -1,    -1,
     557,    -1,    -1,   560,   561,    -1,   563,    -1,    -1,    -1,
    2022,    -1,  2024,    -1,    -1,    -1,    -1,   574,    -1,   576,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1385,    -1,
     153,  2043,    -1,   590,    -1,   592,   593,  2049,   595,  1405,
      -1,    12,    13,    14,    15,    16,   101,  2059,   171,   104,
     105,   106,   107,   108,   109,   110,   111,   112,    -1,   616,
      -1,    -1,   619,    -1,    -1,    -1,   623,    -1,    -1,   626,
      -1,    -1,   629,    -1,   631,    -1,    -1,    -1,    -1,  1515,
     637,    -1,    -1,  1519,    -1,  1521,    -1,    -1,  1838,  1525,
      -1,  1527,    -1,   650,    -1,   652,   653,   152,   655,    70,
      -1,    -1,   157,    -1,   661,    -1,    -1,   664,   665,   666,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,  1555,
    1556,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     101,    73,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   132,   133,   134,   135,   136,   137,   138,   139,
     140,   141,   142,    95,  1590,  1521,   146,    -1,   129,    -1,
      -1,    -1,    -1,   720,    -1,  1601,   101,    -1,  1604,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   149,   150,
      -1,    -1,    -1,   173,    -1,    -1,    -1,   744,    -1,   160,
     161,   174,    -1,  1629,    -1,    -1,    -1,    -1,  1555,    -1,
      -1,    -1,    -1,    -1,   761,    -1,   101,    -1,   150,   104,
     105,   106,   107,   108,   109,   110,   111,   112,  1654,  1655,
      -1,    -1,   157,  1986,    -1,    -1,   783,   784,    -1,    -1,
      -1,   101,    -1,  1669,  1670,  1601,   106,   107,   108,   109,
     110,   111,   112,    -1,    -1,   802,    -1,    -1,  1684,    -1,
      -1,  1687,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,
      -1,    -1,   157,    -1,    -1,    -1,    -1,    -1,  1704,    -1,
      -1,    -1,    -1,   830,    -1,    -1,    -1,   219,   220,   149,
     150,  1717,    -1,   153,    -1,    -1,    -1,    -1,  1654,  1655,
     160,   161,    -1,    -1,   236,    -1,   853,    -1,    -1,    -1,
      -1,    -1,    -1,    70,   861,    -1,    -1,   864,  1744,    -1,
      -1,   868,    -1,    -1,    -1,    -1,    -1,   874,   875,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   884,    -1,   886,
     887,    -1,   889,    -1,   101,    -1,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    -1,  1704,    -1,    -1,
    1786,    -1,    -1,   295,   296,     1,    -1,    -1,     4,  1795,
      -1,    99,   129,   101,    -1,   103,   104,   105,   106,   107,
     108,   109,   110,   111,   112,    -1,    -1,    -1,    -1,   936,
      -1,    -1,   149,   150,    -1,    -1,    -1,    -1,    12,    13,
      14,    15,    16,   160,   161,  1831,   101,    -1,    -1,   104,
     105,   106,   107,   108,   109,   110,   111,   112,    -1,    -1,
      -1,   149,    58,    -1,   152,   153,    -1,    -1,    -1,    -1,
    1856,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1864,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,  1874,    -1,
      -1,    -1,   999,    -1,    -1,   150,    70,    -1,   153,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   102,    -1,    -1,  1016,
    1017,    -1,    -1,    -1,    -1,    -1,  1902,  1903,    -1,  1905,
    1906,    -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,    -1,
    1856,    -1,    -1,   139,    -1,    -1,    -1,    -1,    -1,   145,
      -1,    -1,   444,    -1,  1940,   129,    -1,    -1,    -1,    -1,
      -1,    -1,  1069,    -1,    -1,    -1,   162,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   149,   150,    -1,    -1,    -1,
      -1,    -1,  1968,  1969,   180,    -1,   160,   161,    -1,    -1,
      -1,    -1,   484,    -1,    -1,   191,   192,    -1,    -1,  1906,
      -1,   493,  1109,    -1,    -1,    -1,    -1,    -1,   101,    -1,
    1117,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      -1,   513,    -1,  2009,   220,    -1,    -1,   519,    -1,    -1,
      -1,    -1,    -1,    -1,     1,    -1,    -1,    -1,    -1,  1146,
     236,    -1,    -1,    -1,    -1,   241,   242,    -1,    -1,   245,
      -1,    -1,    -1,    -1,    -1,   101,  1163,  1164,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   559,    -1,    -1,
      -1,   267,    -1,    -1,   270,    -1,   272,  1184,   570,    -1,
     572,    -1,    -1,   129,    -1,  1192,    -1,   283,    -1,  1196,
      -1,    58,    -1,    -1,    12,    13,    14,    15,    16,    -1,
     296,  1208,    -1,   149,   150,    -1,    -1,   153,    -1,    -1,
      -1,    -1,    -1,  1220,   160,   161,    -1,  1224,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   322,   173,    -1,   325,
      -1,    -1,    -1,    -1,    -1,   102,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   339,    -1,    -1,    -1,   343,   101,    -1,
      -1,    -1,    70,   106,   107,   108,   109,   110,   111,   112,
     113,  1268,  1269,    -1,   117,    -1,   119,    -1,    -1,    -1,
      -1,   367,    -1,    -1,    -1,  1282,  1283,    -1,   145,    -1,
    1287,  1288,    -1,   101,    -1,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   162,   101,   150,    -1,    -1,
     153,   106,   107,   108,   109,   110,   111,   112,   113,  1316,
    1317,   129,   117,    -1,   119,    -1,    70,    -1,    -1,  1326,
    1327,  1328,  1329,  1330,  1331,   192,    -1,    -1,    -1,    -1,
    1337,   149,   150,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     436,    -1,   160,   161,   440,   150,    -1,   101,  1355,  1356,
     104,   105,   106,   107,   108,   109,   110,   111,   112,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   463,    -1,    -1,
      -1,    -1,   764,    -1,   766,   129,    -1,    -1,  1385,    -1,
      -1,   101,    -1,    -1,    -1,    -1,   106,   107,   108,   109,
     110,   111,   112,   113,    -1,   149,   150,   117,  1405,   119,
     267,    -1,    -1,    -1,    -1,    -1,   160,   161,    -1,    70,
      -1,    -1,   804,    -1,    -1,    -1,   283,    -1,    -1,    -1,
      -1,    -1,  1429,    -1,    -1,   521,    -1,    -1,    -1,    -1,
     150,   823,    -1,   153,    -1,    -1,    -1,    -1,    -1,    -1,
     101,    -1,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,    -1,    -1,    -1,   322,   552,    -1,   325,    -1,
      -1,   557,    -1,    -1,    -1,   561,    -1,   563,   129,    -1,
      -1,    -1,   339,    -1,    -1,    -1,   343,    -1,   574,    -1,
     576,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,   150,
      -1,    -1,    -1,    -1,  1501,  1502,   592,   593,   101,   160,
     161,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      -1,   607,    -1,    -1,  1521,    -1,    -1,    -1,    -1,    -1,
     616,    -1,    -1,    -1,    -1,   621,   129,    -1,    -1,    -1,
    1537,    -1,   101,   629,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,    -1,   149,   150,  1555,  1556,
      -1,    -1,    -1,   156,    -1,    -1,    -1,   160,   161,   101,
     129,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,    -1,    -1,   440,    -1,    -1,    -1,    -1,    -1,    -1,
     149,   150,    -1,  1590,   153,    -1,    -1,   129,    -1,    -1,
      -1,   160,   161,    -1,  1601,    -1,    -1,  1604,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1613,   149,   150,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   160,   161,
      -1,    -1,  1629,    -1,   720,    -1,    -1,    -1,  1020,    -1,
      -1,    -1,    -1,    -1,  1026,  1027,    -1,  1644,    -1,    -1,
      -1,   737,  1649,  1650,    -1,    -1,    -1,  1654,  1655,    -1,
      -1,    -1,    -1,    -1,   521,    -1,    -1,    -1,    55,    56,
      -1,    -1,    -1,    -1,    -1,   761,    -1,    -1,    -1,    -1,
     766,   101,    -1,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,    -1,    -1,   552,    -1,   783,   784,    -1,
      -1,    -1,    -1,    90,   561,     1,   563,  1704,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,   802,   574,    -1,   576,
    1717,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,
     150,    -1,    -1,    -1,    -1,   592,   593,    -1,    -1,    -1,
     160,   161,    -1,    -1,   830,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   140,    -1,    -1,   143,    -1,    -1,   616,
      -1,    -1,    58,    -1,    -1,    -1,    -1,    -1,    -1,   156,
      -1,    -1,   629,    -1,    -1,   861,    -1,    -1,    -1,    -1,
      -1,    -1,   868,    -1,    -1,    -1,    -1,    -1,   874,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1178,    -1,   884,    -1,
      -1,   887,    -1,   889,   101,    -1,   102,    -1,   894,   106,
     107,   108,   109,   110,   111,   112,   113,    -1,    -1,    -1,
     117,   101,   119,   210,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   101,  1831,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,    -1,    -1,    -1,    -1,   145,
     936,    -1,    -1,   150,    -1,    -1,   153,    -1,    -1,  1856,
      -1,    -1,    -1,   720,    -1,    -1,   162,    -1,   255,    -1,
      -1,    -1,    -1,   153,    -1,    -1,    -1,  1874,   265,    -1,
      -1,    -1,    -1,    -1,   152,    -1,    -1,    -1,    -1,   276,
      -1,    -1,    -1,    -1,    -1,  1277,   192,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   761,  1902,  1903,    -1,    -1,  1906,
      -1,    -1,    -1,   999,    -1,    -1,   303,    -1,    -1,    -1,
      -1,    -1,    -1,   310,   311,    -1,   783,   784,   315,   101,
    1016,  1017,   104,   105,   106,   107,   108,   109,   110,   111,
     112,    -1,    -1,  1940,    -1,   802,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1336,    -1,    -1,    -1,    -1,    -1,
      -1,   348,    -1,    -1,    -1,    -1,   353,    -1,    -1,   356,
      -1,    -1,    -1,   830,    -1,    -1,    -1,    -1,    -1,    -1,
     152,    -1,    -1,  1069,    -1,    -1,    -1,   283,   101,  1986,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      -1,    -1,    -1,    -1,   861,    -1,    -1,    -1,    -1,   101,
      -1,   868,   104,   105,   106,   107,   108,   109,   110,   111,
     112,    -1,    -1,  1109,    -1,    -1,   322,   884,    -1,   325,
     887,  1117,   889,    -1,    -1,    -1,   149,    -1,    -1,    -1,
      -1,    -1,    -1,   339,    -1,    -1,     1,   343,    -1,     4,
      -1,    -1,    -1,    -1,    -1,    -1,   443,   149,    -1,    -1,
    1146,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   455,   456,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1164,   936,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    58,    -1,    -1,  1192,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1201,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,
      -1,    -1,    -1,    -1,  1220,    -1,    -1,  1519,    -1,    -1,
      -1,    -1,   999,  1525,   440,  1527,    -1,   102,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1016,
    1017,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1268,  1269,   139,    -1,    -1,    -1,    -1,    -1,
     145,    -1,   147,    -1,    -1,    -1,  1282,  1283,    -1,   586,
      -1,  1287,  1288,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1069,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   179,   521,    -1,    -1,    -1,    -1,
    1316,  1317,    -1,    -1,    -1,    -1,   191,    -1,    -1,    -1,
    1326,  1327,  1328,  1329,    -1,    -1,    -1,    -1,   635,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   552,    -1,    -1,    -1,
    1117,    -1,    -1,    -1,    -1,   561,    -1,   563,    -1,  1355,
    1356,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   574,    -1,
     576,    -1,    -1,    -1,    -1,    -1,    -1,   242,    -1,  1146,
     245,    -1,    -1,    -1,    -1,   250,   592,   593,    -1,    -1,
      -1,    -1,  1684,    -1,    -1,  1687,    -1,  1164,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1405,
     616,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   283,    -1,
      -1,    -1,    -1,   629,    -1,  1192,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1429,    -1,    -1,    -1,   302,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1744,  1220,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     1,    -1,    -1,     4,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   778,    -1,    -1,    -1,    -1,    -1,    -1,   785,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1268,  1269,  1795,    -1,  1501,  1502,    -1,    -1,    -1,
      -1,   376,    -1,    -1,   720,  1282,  1283,    -1,    -1,    -1,
    1287,  1288,    -1,    58,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1527,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1537,    -1,    -1,    -1,   410,    -1,    82,    -1,  1316,
    1317,    -1,    -1,    -1,    -1,   761,    -1,    -1,    -1,    -1,
    1556,    -1,    -1,   860,    -1,    -1,    -1,   102,    -1,    -1,
      -1,   436,    -1,    -1,    -1,    -1,    -1,   783,   784,    -1,
      -1,    -1,    -1,    -1,    -1,   882,    -1,    -1,  1355,  1356,
      -1,    -1,    -1,    -1,  1590,    -1,   802,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   139,  1601,    -1,    -1,  1604,    -1,
     145,    -1,   147,  1905,    -1,   912,    -1,  1613,    -1,   916,
      -1,    -1,    -1,    -1,   830,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1405,    -1,
      -1,    -1,    -1,    -1,   179,    -1,    -1,    -1,  1644,    -1,
     515,   516,    -1,  1649,  1650,   861,   191,    -1,  1654,  1655,
      -1,    -1,   868,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1670,    -1,  1968,  1969,   884,    -1,
      -1,   887,    -1,   889,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   557,    -1,    -1,    -1,   561,    -1,   563,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   242,    -1,    -1,
     245,   576,    -1,    -1,    -1,   250,    -1,  2009,    -1,    -1,
      -1,  1717,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     936,    -1,    -1,    -1,  1501,  1502,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   283,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   302,    -1,    86,
    1537,    -1,   637,    -1,    -1,    92,    93,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   650,    -1,   652,   653,  1556,
     655,    -1,    -1,   999,    -1,    -1,   661,    -1,    -1,   664,
     665,   666,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   126,
    1016,  1017,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1590,    -1,  1122,  1123,  1124,    -1,    -1,
      -1,    -1,    -1,    -1,  1601,  1831,    -1,    -1,    -1,    -1,
      -1,   376,    -1,    -1,    -1,    -1,  1613,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1152,    -1,    -1,    -1,    -1,
    1856,    -1,    -1,  1069,    -1,    -1,    -1,    -1,    -1,    -1,
    1167,    -1,    -1,    -1,    -1,   410,    -1,  1644,  1874,   744,
      -1,    -1,  1649,  1650,    -1,    -1,    -1,  1654,  1655,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   436,    -1,    -1,    -1,    -1,  1902,  1903,  1205,    -1,
      -1,  1117,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1146,    -1,    -1,    -1,  1940,    -1,    -1,    -1,    -1,    -1,
    1717,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1164,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1969,    -1,   292,    -1,    -1,    -1,    -1,
     515,   516,    -1,    -1,    -1,    -1,  1192,    -1,    -1,    -1,
    1986,    -1,    -1,    -1,    -1,    -1,   861,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   874,
     875,    -1,    -1,    -1,  1220,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   557,    -1,    -1,    -1,   561,    -1,   563,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   576,    -1,  1340,    -1,    -1,  1343,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1268,  1269,  1831,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     1,    -1,    -1,    -1,  1282,  1283,    -1,    -1,
      -1,  1287,  1288,    -1,    -1,    -1,    -1,    -1,    -1,  1856,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   637,    -1,    -1,    -1,    -1,  1874,    -1,    -1,
    1316,  1317,    -1,    -1,    -1,   650,    -1,   652,   653,    48,
     655,    -1,   439,    52,   441,    54,   661,    -1,    -1,   664,
     665,   666,    -1,   450,   451,  1902,  1903,    -1,    -1,    -1,
      -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,    -1,  1355,
    1356,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    98,
      99,    -1,   101,  1940,   103,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,    -1,   117,   118,
     119,    -1,   121,   122,    -1,    -1,    -1,    -1,    -1,  1405,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   744,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1986,
     149,    -1,    -1,   152,   153,    -1,    -1,    -1,    -1,    -1,
     159,   160,   161,   162,   163,   164,   165,  1534,    -1,    -1,
      -1,   558,    -1,    -1,  1109,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1117,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     5,    -1,    -1,    -1,    -1,    -1,
      -1,    12,    13,    14,    15,    16,    -1,    -1,    -1,    -1,
      -1,  1146,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1501,  1502,    -1,    -1,  1164,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    -1,    -1,
      -1,    52,    -1,    54,    -1,    -1,    -1,    -1,    -1,  1184,
      -1,    -1,    -1,    -1,  1621,    -1,   861,    -1,    -1,    70,
      71,  1537,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   874,
     875,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1556,    -1,     0,    -1,    -1,     3,    -1,    98,    99,  1224,
     101,    -1,   103,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,   117,   118,   119,    -1,
     121,   122,    -1,    -1,  1590,    -1,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,  1601,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1613,   149,    -1,
      -1,   152,   153,    -1,    -1,    -1,    -1,    -1,   159,   160,
     161,   162,   163,   164,   165,    -1,    -1,    -1,    76,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1644,    -1,
      -1,    -1,    -1,  1649,  1650,    -1,  1743,    -1,  1654,  1655,
      -1,    -1,    -1,    -1,    -1,  1752,    -1,    -1,    -1,    -1,
     777,  1326,  1327,  1328,  1329,  1330,  1331,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   135,    -1,    -1,
    1355,  1356,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    12,    13,    14,    15,    16,    -1,    -1,
      19,  1717,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,   854,   855,    -1,
      -1,    50,    51,    -1,    -1,    -1,    -1,    -1,   865,   866,
     867,    -1,    -1,   870,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    70,    -1,    -1,  1429,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1109,    -1,    -1,    -1,    -1,    -1,
      -1,   229,  1117,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   101,    -1,    -1,    -1,   244,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   254,    -1,    -1,    -1,
      -1,  1146,    -1,    -1,    -1,    -1,   264,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1831,    -1,    -1,    -1,  1164,
     278,   279,    -1,   950,    -1,    -1,    -1,   285,   286,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   156,    -1,  1184,
    1856,    -1,    -1,   301,    -1,    -1,  1521,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1874,    -1,
      -1,    -1,    -1,   321,    -1,    -1,    -1,    -1,    -1,   996,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1224,
      -1,  1556,    -1,    -1,    -1,    -1,  1902,  1903,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1590,  1043,   178,    -1,   377,
      -1,    -1,    -1,    -1,  1940,  1052,  1053,  1054,  1055,  1604,
      -1,    -1,    -1,  1060,  1061,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1070,    -1,    -1,    -1,    -1,    -1,    -1,
     408,    -1,    -1,    -1,  1629,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1091,    -1,  1093,    -1,    -1,    -1,
    1986,    -1,    -1,    -1,    -1,    -1,   434,    -1,    -1,    -1,
     438,  1326,  1327,  1328,  1329,  1330,  1331,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   457,
      -1,    -1,    -1,   461,   462,    -1,    -1,   465,    -1,    -1,
    1355,  1356,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1146,
      -1,    -1,   480,   481,   482,   483,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   499,  1717,    -1,    -1,  1172,    -1,    -1,    -1,   507,
      -1,    -1,  1179,    -1,  1181,  1182,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1191,    -1,  1193,    -1,  1195,    -1,
    1197,    -1,    -1,    -1,    -1,  1202,    -1,   535,    -1,    -1,
      -1,    -1,    -1,    -1,  1429,    -1,    -1,    -1,    -1,    -1,
      47,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     361,    -1,    -1,   364,   365,    -1,    -1,    -1,   566,    -1,
      -1,    -1,    -1,   374,   375,   573,    73,    -1,    -1,    -1,
      -1,   579,    -1,    -1,    -1,    -1,    -1,    -1,   389,   390,
      -1,    -1,    -1,    -1,    -1,    -1,  1263,    -1,    -1,    -1,
      -1,    -1,    -1,  1270,  1271,   603,   604,    -1,    -1,   410,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   119,    -1,    -1,    -1,    -1,  1294,    -1,    -1,
      -1,    -1,    -1,    -1,  1301,   132,  1521,   134,  1305,   440,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1874,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   164,  1335,   667,
      -1,  1556,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   181,    -1,    -1,  1902,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1590,    -1,    -1,  1375,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1604,
      -1,    -1,   219,    -1,    -1,  1940,   223,    -1,    -1,   226,
     227,    -1,    -1,   230,    -1,    -1,   233,   234,   736,    -1,
      -1,    -1,  1409,    -1,  1629,    -1,    -1,    -1,    -1,    -1,
    1417,    -1,  1419,   751,    -1,    -1,    -1,   755,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   764,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   786,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   795,   295,  1466,
    1467,   298,    -1,   801,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1481,  1482,    -1,  1484,    -1,    -1,
      -1,    -1,   319,   320,    -1,    -1,  1493,    -1,    -1,    -1,
      -1,    -1,  1717,    -1,    -1,    -1,  1503,  1504,   335,    -1,
     838,    -1,    -1,    -1,    -1,    -1,    -1,   845,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   872,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   697,   698,   699,   700,
     701,   702,   703,   704,   705,   706,   707,   708,   709,   710,
     711,   712,   713,   714,   715,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   926,    -1,
      -1,    -1,   429,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1614,  1615,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1625,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   777,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   484,    -1,  1874,
      -1,    -1,    -1,  1660,  1661,    -1,    -1,    -1,    -1,   496,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1009,    -1,    -1,    -1,  1013,    -1,  1902,    -1,    -1,
      -1,    -1,  1020,    -1,    -1,    98,    -1,    -1,    -1,    -1,
      -1,    -1,  1030,    -1,    -1,    -1,   109,    -1,   111,  1037,
     113,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1046,    -1,
    1048,    -1,    -1,    -1,    -1,  1940,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   570,    -1,    -1,    -1,    -1,   151,  1746,
     153,   154,  1080,    -1,    -1,    -1,  1084,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1765,    -1,
    1098,  1768,  1769,  1101,    -1,    -1,    -1,    -1,  1775,    -1,
      -1,    -1,    -1,   914,   611,   612,    -1,    -1,    -1,    -1,
      -1,    -1,   195,    -1,    -1,    -1,    -1,   624,    -1,    -1,
      -1,   932,     3,    -1,    -1,    -1,   937,    -1,    -1,    -1,
      -1,    12,    13,    14,    15,    16,    -1,   948,    19,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    -1,    49,    50,
      51,    -1,    53,    -1,    -1,   258,    -1,   260,   261,   990,
      -1,    -1,  1190,    -1,    -1,    -1,    -1,    -1,    -1,    70,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    47,    -1,   288,    -1,  1215,    -1,    -1,
      -1,   294,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   104,   105,    -1,    -1,  1904,    -1,    -1,
      -1,    -1,    -1,   740,   741,    -1,    -1,    -1,    -1,   746,
      -1,   324,    -1,    -1,    -1,    -1,    -1,   330,   129,   332,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     767,    -1,    -1,   770,   771,    -1,   773,    -1,   775,   776,
      -1,   152,   153,    -1,    -1,   119,    -1,    -1,    -1,   160,
     161,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,  1966,
     134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1308,    -1,    -1,    -1,  1312,    -1,  1117,   814,    -1,    -1,
      -1,   818,    -1,  1990,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2005,    -1,
      -1,    -1,    -1,    -1,  1342,    -1,    -1,   420,    -1,    -1,
      -1,    -1,    -1,  2020,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   446,    -1,   448,   449,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1192,   226,   227,  1392,    -1,   230,  1395,   895,   233,
     234,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1411,    -1,    -1,    -1,    -1,    -1,    -1,
     493,    -1,    -1,    -1,    -1,    -1,  1227,  1228,  1229,    -1,
      -1,    -1,    -1,  1234,  1235,    -1,   162,    -1,    -1,    -1,
     513,    -1,    -1,    -1,    -1,   518,    -1,   520,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1258,    -1,    -1,
      -1,    -1,    -1,    -1,  1462,   191,   192,   540,    -1,   542,
     543,    -1,    -1,  1471,    -1,    -1,    -1,  1475,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   319,   320,   560,    -1,    -1,
      -1,  1489,  1490,    -1,  1295,  1296,    -1,   223,    -1,   572,
      -1,   335,    -1,    -1,   230,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   595,    -1,   597,   598,    -1,    -1,    -1,  1026,
      -1,    -1,    -1,    -1,    -1,   376,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   619,   620,    -1,    -1,
      -1,    -1,    -1,   626,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1067,    -1,   298,    -1,    -1,    -1,    -1,  1074,    -1,    -1,
    1077,    -1,    -1,    -1,    -1,    -1,   659,   660,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   429,   322,   323,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1605,  1606,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   343,    -1,    -1,
      12,    13,    14,    15,    16,    17,    -1,    19,    -1,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    -1,    -1,    -1,    -1,    50,    51,
      -1,    -1,   496,    55,    56,    57,    58,    59,    60,    61,
      62,    -1,    -1,    -1,   515,   516,    -1,    -1,    70,    -1,
      -1,    -1,    -1,    -1,    76,    -1,    -1,   413,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1198,    -1,   429,   430,    -1,   432,   433,    -1,  1206,
    1207,    -1,   104,   105,   440,    -1,    -1,    -1,   444,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1731,    -1,    -1,    -1,   129,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      82,    -1,    -1,    -1,    -1,    -1,    -1,  1755,    -1,   485,
     152,   153,    -1,   489,    -1,    -1,    98,  1264,   160,   161,
      -1,    -1,    -1,    -1,    -1,  1773,  1273,   611,   612,  1276,
     853,  1278,  1279,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     624,   864,    -1,    -1,  1595,   521,    -1,    -1,    -1,    -1,
      -1,    -1,  1800,    -1,    -1,    -1,    -1,    -1,    -1,   650,
      -1,    -1,    -1,   886,   655,   147,    -1,    -1,    -1,   151,
     661,    -1,  1319,    -1,   897,    -1,    -1,    -1,    -1,  1827,
     162,    -1,  1830,   906,    -1,    -1,    -1,    -1,    -1,   680,
      -1,    -1,    -1,    -1,    -1,   571,    -1,   179,   574,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     192,    -1,    -1,   195,    -1,    -1,   592,   593,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   716,    -1,   603,    -1,    -1,
      -1,   607,    -1,    -1,    -1,    -1,    -1,    -1,   614,  1386,
     616,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   740,   741,    -1,    -1,
      -1,    -1,   746,   245,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   258,    -1,    -1,    -1,
    1003,  1929,    -1,   767,    -1,    -1,   770,   771,    -1,   773,
      -1,   775,   776,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1027,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   294,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     302,    -1,    -1,  1470,    -1,    -1,    -1,    -1,    -1,    -1,
     814,    -1,    -1,    -1,   818,    -1,    -1,    -1,    -1,    -1,
     322,    -1,   324,    -1,   720,    -1,    -1,  1798,    -1,    -1,
      -1,    -1,  1499,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     736,   737,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     746,   747,    -1,   749,   750,    -1,    -1,    -1,  1525,    -1,
      -1,    -1,    -1,    -1,  1531,   761,    -1,  1838,   764,    -1,
     766,   767,    -1,    -1,   376,    -1,    -1,   773,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   783,   784,    -1,
      -1,   895,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   802,    -1,   410,    -1,
     806,    -1,    -1,    -1,   810,    -1,    -1,    -1,   814,   815,
    1163,    -1,   818,   819,    -1,    -1,    -1,    -1,    -1,    -1,
     826,    -1,    -1,  1600,    -1,    -1,    -1,    -1,   440,    -1,
      -1,    -1,    -1,    -1,   446,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1196,    -1,    -1,    -1,    -1,    -1,  1202,
      -1,    -1,    -1,  1934,    -1,  1936,    -1,    12,    13,    14,
      15,    16,   868,   869,    19,    -1,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,   897,    -1,    -1,  1975,    50,    51,    -1,    -1,  1676,
    1677,    -1,    -1,   515,   516,    -1,    -1,  1684,    -1,   521,
      -1,  1688,  1026,    -1,    -1,    70,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   179,    -1,    -1,  2007,    -1,    -1,    -1,
     936,    -1,    -1,    -1,    -1,    -1,   192,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,   205,
      -1,   207,    -1,  1067,    -1,    -1,    -1,    -1,    -1,    -1,
    1074,    -1,   574,  1077,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  2055,    -1,    -1,    -1,    -1,    -1,
      -1,   593,    -1,   595,  1337,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1003,    -1,    -1,
      -1,    -1,    -1,    -1,   616,    -1,    -1,    -1,    -1,    -1,
      -1,  1788,    -1,    -1,  1020,  1021,    -1,    -1,    -1,    -1,
      -1,  1027,    -1,    -1,    -1,   637,    -1,    -1,    -1,    -1,
      -1,    -1,  1385,    -1,    -1,    -1,    -1,   293,   650,    -1,
     652,   653,    -1,   655,    -1,    -1,    -1,    -1,    -1,   661,
      -1,    -1,   664,   665,   666,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1069,    -1,    -1,    -1,    -1,  1074,  1075,
      -1,  1077,  1078,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1860,  1198,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1206,  1207,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   720,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   744,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   761,
    1264,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1273,
      -1,    -1,  1276,    -1,  1278,  1279,    -1,    -1,    -1,    -1,
      -1,   783,   784,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1192,    -1,    -1,    -1,
     802,  1968,  1198,  1199,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1555,    -1,    -1,  1319,    -1,    -1,    -1,    -1,
     466,    -1,    -1,    -1,  1220,    -1,   472,    -1,    -1,    -1,
      -1,   477,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1352,    -1,    -1,  1355,  1356,    -1,    -1,    -1,    -1,
    1361,    -1,    -1,    -1,  1365,    -1,  1367,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   868,    -1,  1264,  1265,
      -1,    -1,    -1,   875,    -1,    -1,    -1,  1273,  1274,    -1,
    1276,    -1,  1386,    -1,   886,    -1,    -1,    -1,    -1,    -1,
      -1,  1287,  1288,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   564,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   936,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1687,    -1,    -1,   593,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     606,  1704,    -1,    -1,    -1,    -1,  1470,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1499,  1507,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1405,
      -1,   657,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1539,    -1,
      -1,    -1,   678,   679,    -1,    -1,   682,    -1,   684,    -1,
    1551,    -1,    -1,  1786,   690,    -1,   692,   693,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1566,  1567,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1069,    -1,    -1,
      -1,    -1,    -1,    -1,   720,    -1,    -1,    -1,    -1,  1590,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   733,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1600,    -1,   744,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1109,    -1,    -1,
      -1,    -1,   758,    -1,    -1,   761,    -1,    -1,    -1,  1515,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1531,    -1,    -1,    -1,    -1,
      -1,    -1,   788,    -1,    -1,   791,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1163,    -1,  1906,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1676,  1677,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   827,  1184,    -1,  1688,    -1,    -1,    -1,    -1,  1700,
    1192,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1709,    -1,
    1711,    -1,    -1,  1714,  1715,  1601,  1717,    -1,    -1,    -1,
      -1,  1722,    -1,    -1,    -1,    -1,    -1,    -1,  1220,    -1,
      -1,    -1,  1224,    -1,    -1,    -1,    -1,    -1,    -1,   875,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     886,   887,    -1,    -1,    -1,    -1,    -1,    -1,   894,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1654,  1655,
      -1,    -1,    -1,    -1,    -1,    -1,  2009,    -1,    -1,    -1,
      -1,    -1,    -1,  1669,  1670,    -1,    -1,   923,    -1,    -1,
      -1,    -1,    -1,    -1,  1788,  1287,  1288,    -1,    -1,  1685,
     936,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   944,    -1,
      -1,    -1,    -1,    -1,    -1,   951,  1817,    -1,    -1,    -1,
      -1,  1822,  1823,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1326,  1327,  1328,    -1,  1330,  1331,
      -1,    -1,    -1,    -1,    -1,  1337,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   997,    -1,    -1,    -1,    -1,  1860,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1882,    -1,  1884,    -1,    -1,  1887,  1888,    -1,    -1,
      -1,  1892,  1893,  1385,    -1,    -1,    -1,    -1,    -1,    -1,
    1786,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1794,    -1,
      -1,    -1,    -1,  1405,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1064,    -1,
    1066,    -1,  1068,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1961,  1962,  1963,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1856,    -1,    -1,    -1,  1860,  1861,    -1,    -1,  1864,    -1,
      -1,  1982,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1995,  1996,  1997,    -1,  1134,  1135,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1905,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1521,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     5,    -1,    -1,    -1,    -1,    -1,    -1,    12,
      13,    14,    15,    16,    -1,    -1,    -1,    -1,    -1,    -1,
    1196,    -1,    -1,  1555,    -1,    -1,  1202,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1211,    -1,    -1,    -1,    -1,
      -1,    -1,  1968,  1969,  1220,    48,    -1,    -1,    -1,    52,
      -1,    54,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1237,    -1,    -1,    -1,    -1,    -1,    70,    71,  1601,
      -1,    -1,    -1,    -1,    -1,    -1,  1252,    -1,    -1,  1255,
      -1,    -1,    -1,  2009,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    98,    99,  1629,   101,    -1,
     103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,    -1,   117,   118,   119,    -1,   121,   122,
      -1,    -1,  1654,  1655,    -1,    -1,   129,    -1,    -1,    -1,
      -1,  1307,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   149,    -1,    -1,   152,
     153,    -1,    -1,    -1,    -1,    -1,   159,   160,   161,   162,
     163,   164,   165,    -1,    -1,    -1,    -1,    -1,    -1,  1345,
      -1,    -1,  1704,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1363,    -1,    -1,
    1366,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    17,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1405,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1415,
    1416,    -1,    -1,    -1,    -1,    -1,    -1,    48,    -1,    -1,
      -1,    52,    -1,    54,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1441,    -1,  1443,    69,    -1,
      71,    72,    -1,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    90,
      91,    92,    93,    94,    -1,    96,    -1,    98,    99,  1831,
     101,    -1,   103,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,    -1,
     121,   122,    -1,    -1,  1856,    -1,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1521,    -1,   148,   149,    -1,
    1526,   152,   153,    -1,    -1,    -1,   157,    -1,   159,   160,
     161,   162,   163,   164,   165,    -1,    -1,    -1,    -1,    -1,
      -1,  1903,    -1,   174,  1906,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    17,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1586,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,
      -1,    -1,    -1,    52,    -1,    54,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      69,  1627,    71,    72,  1630,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,    87,    88,
      89,    90,    91,    92,    93,    94,    -1,    96,    -1,    98,
      99,    -1,   101,    -1,   103,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,    -1,   121,   122,    -1,    -1,     1,    -1,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    17,    -1,    -1,    -1,    -1,    -1,    -1,   148,
     149,    -1,    -1,   152,   153,    -1,    -1,    -1,   157,    -1,
     159,   160,   161,   162,   163,   164,   165,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    -1,   174,    -1,    52,    -1,    54,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    69,    -1,    71,    72,    -1,    74,
      -1,    -1,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      -1,    96,    -1,    98,    99,    -1,   101,    -1,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,    -1,   121,   122,    -1,    -1,
      -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   149,    -1,    -1,   152,   153,    -1,
      -1,    -1,   157,  1839,   159,   160,   161,   162,   163,   164,
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
      -1,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    96,    -1,    -1,
      -1,   100,   101,    -1,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,    -1,    -1,   116,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     149,   150,    -1,   152,   153,    -1,    -1,    -1,    -1,    -1,
      -1,   160,   161,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    -1,    -1,    19,
      -1,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    -1,    49,
      50,    51,    -1,    53,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    12,    13,    14,    15,    16,    -1,
      70,    19,    -1,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    -1,    -1,
      -1,   101,    50,    51,   104,   105,   106,   107,   108,   109,
     110,   111,   112,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,
     150,    -1,   152,   153,    -1,    -1,    -1,   157,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   148,    -1,    -1,
      -1,   152,   153,    -1,    -1,    -1,    -1,    -1,    -1,   160,
     161,     3,     4,     5,     6,     7,     8,     9,    10,    11,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   149,   150,    -1,   152,   153,
      -1,    -1,    -1,   157,    -1,    -1,   160,   161,     4,     5,
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
      -1,    -1,    -1,    -1,    17,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   149,   150,    48,   152,   153,    -1,    52,
      -1,    54,    -1,    -1,   160,   161,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    69,    -1,    71,    -1,
      -1,    74,    75,    76,    77,    78,    79,    80,    81,    82,
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
      -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   151,   152,   153,    -1,    -1,
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
      58,    59,    60,    61,    62,    -1,    12,    13,    14,    15,
      16,    17,    70,    19,    -1,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      -1,    -1,   152,    -1,    50,    51,   104,   105,    -1,    55,
      56,    57,    58,    59,    60,    61,    62,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,
      -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   151,   152,   153,    -1,    -1,   104,   105,
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
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    -1,    -1,    19,    -1,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    -1,    49,    50,    51,    -1,
      53,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    12,    13,    14,    15,    16,    17,    70,    19,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    -1,    -1,    -1,    -1,    50,
      51,   104,   105,    -1,    55,    56,    57,    58,    59,    60,
      61,    62,    -1,    12,    13,    14,    15,    16,    17,    70,
      19,    -1,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    -1,    -1,   152,
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
      -1,    -1,    -1,   152,   153,    -1,    -1,    -1,    -1,    -1,
      -1,   160,   161,    12,    13,    14,    15,    16,    17,    -1,
      19,    -1,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    -1,    -1,    -1,
      -1,    50,    51,    -1,    -1,    -1,    55,    56,    57,    58,
      59,    60,    61,    62,    -1,    12,    13,    14,    15,    16,
      17,    70,    19,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    -1,
      -1,    -1,    -1,    50,    51,   104,   105,    -1,    55,    56,
      57,    58,    59,    60,    61,    62,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   152,   153,    -1,    -1,   104,   105,    -1,
      -1,   160,   161,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   152,    -1,    -1,    12,    13,
      14,    15,    16,   160,   161,    19,    -1,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    -1,    49,    50,    51,    -1,    53,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    12,    13,
      14,    15,    16,    -1,    -1,    19,    70,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    -1,    49,    50,    51,    -1,    53,
     104,   105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,   153,
     104,   105,    -1,    -1,    -1,    -1,   160,   161,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,    -1,
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
      -1,    -1,    -1,    -1,    -1,   131,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   151,   152,     4,     5,     6,
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
      -1,    -1,    -1,    -1,   131,    -1,    -1,    -1,    -1,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    -1,    19,   152,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    -1,    -1,    -1,    -1,    50,    51,    -1,    -1,    -1,
      55,    56,    57,    58,    59,    60,    61,    62,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,   104,
     105,    19,    -1,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      -1,    49,    50,    51,    -1,    53,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,    -1,    -1,
      -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   104,   105,    -1,    -1,
      -1,    -1,    12,    13,    14,    15,    16,    17,    -1,    19,
      -1,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    -1,    -1,    -1,    -1,
      50,    51,    -1,    -1,   152,    55,    56,    57,    58,    59,
      60,    61,    62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   101,    -1,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,    -1,    -1,    -1,    12,    13,    14,    15,
      16,    -1,    -1,    19,    -1,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,   152,    49,    50,    51,    -1,    53,    -1,    -1,
      -1,    -1,    48,    -1,    -1,    -1,    52,    -1,    54,    -1,
      -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,   105,
      -1,    -1,    98,    99,    -1,   101,    -1,   103,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
      -1,   117,   118,   119,    -1,   121,   122,    -1,    48,    -1,
      -1,    -1,    52,   129,    54,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   152,   143,   144,   145,
      -1,    71,    -1,   149,   150,    -1,   152,   153,    -1,    -1,
      -1,    -1,    -1,   159,   160,   161,   162,   163,   164,   165,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    98,    99,
      -1,   101,    -1,   103,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,    -1,   117,   118,   119,
      -1,   121,   122,    -1,    48,    -1,    -1,    -1,    52,   129,
      54,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   143,   144,   145,    -1,    71,    -1,   149,
     150,    -1,   152,   153,    -1,    -1,    -1,    -1,    -1,   159,
     160,   161,   162,   163,   164,   165,    -1,    -1,    -1,    -1,
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
     149,   223,   151,   221,   297,   143,   144,   145,   165,   174,
     244,   245,   223,   222,   174,   245,   151,   156,   221,   150,
     221,   222,   243,   174,   464,   151,   151,   151,   225,   260,
     261,   149,   214,   149,   182,   232,   198,   253,   107,     1,
     223,   404,   384,   176,   176,   154,   352,   177,   177,   154,
     154,   148,   157,   347,   148,   177,   214,   186,   214,   464,
     148,   154,   154,   191,   191,   352,   151,   151,   352,   352,
     151,   151,   154,   155,   131,   351,   131,   154,   177,   177,
     151,   151,   154,   450,   451,   452,   297,   449,   155,   174,
     404,   404,   174,   151,   410,   404,   174,   223,    75,    76,
     157,   235,   236,   237,   151,   221,    73,   223,   221,   150,
     221,    73,   174,   104,   150,   221,   222,   243,   150,   221,
     223,   242,   245,   245,   174,   221,   148,   157,   237,   223,
     149,   176,   174,   182,   151,   156,   151,   151,   155,   156,
     251,   255,   359,   401,   177,   154,   154,   347,   464,   148,
     148,   154,   154,   177,   177,   177,   176,   177,   151,   151,
     151,   151,   151,   449,   404,   336,     1,   213,   233,   234,
     402,     1,   156,     1,   176,   223,   235,    73,   174,   151,
     223,    73,   174,   165,   165,   223,   222,   245,   245,   174,
     104,   221,   165,   165,    73,   150,   221,   150,   221,   222,
     174,     1,   176,   176,   262,   295,   297,   458,   156,   174,
     153,   182,   267,   268,   269,   223,   198,   188,    73,   106,
     252,   254,   151,   464,   148,   151,   151,   151,   354,   149,
     404,   441,   442,   338,   131,     1,   155,   156,   148,   272,
     273,   279,   223,    73,   174,   223,   221,   150,   150,   221,
     150,   221,   150,   221,   222,   150,   221,   150,   221,   223,
     165,   165,   165,   165,   148,   272,   262,   177,   149,   196,
     401,   449,   180,   156,   101,   149,   151,   156,   155,    73,
     151,   223,   149,   223,   223,   148,   176,   213,   233,   236,
     238,   239,   279,   223,   165,   165,   165,   165,   150,   150,
     221,   150,   221,   150,   221,   238,   177,   174,   259,   297,
     267,   154,   213,   174,   267,   269,   223,   221,   107,   107,
     352,   223,   228,   177,   236,   150,   150,   221,   150,   221,
     150,   221,   177,   259,   212,   151,   156,   182,   151,   151,
     156,   151,   255,    73,   250,   177,     1,   223,   148,   228,
     148,   151,   225,   182,   270,   149,   174,   270,   223,    73,
     151,   225,   155,   156,   213,   151,   223,   182,   180,   271,
     151,   174,   151,   155,   174,   180
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
     242,   242,   242,   242,   242,   242,   242,   242,   242,   243,
     243,   243,   244,   244,   245,   245,   245,   246,   246,   246,
     246,   246,   246,   246,   246,   246,   246,   246,   246,   246,
     246,   246,   246,   246,   246,   246,   246,   247,   247,   248,
     249,   250,   251,   251,   252,   252,   253,   253,   254,   255,
     255,   255,   255,   255,   255,   256,   256,   257,   257,   257,
     258,   258,   259,   259,   260,   260,   260,   260,   261,   262,
     262,   262,   262,   262,   263,   264,   264,   265,   265,   265,
     265,   265,   266,   266,   267,   267,   268,   268,   269,   269,
     270,   270,   270,   271,   271,   272,   272,   273,   273,   274,
     274,   275,   275,   276,   276,   277,   277,   278,   278,   279,
     279,   279,   280,   280,   281,   281,   281,   281,   281,   282,
     282,   282,   283,   283,   283,   284,   284,   284,   284,   284,
     285,   285,   286,   286,   287,   287,   287,   288,   288,   288,
     288,   288,   289,   289,   290,   290,   290,   290,   291,   291,
     292,   292,   292,   293,   293,   293,   294,   294,   294,   295,
     295,   295,   296,   296,   297,   297,   298,   298,   299,   299,
     299,   299,   299,   300,   301,   301,   301,   302,   302,   303,
     303,   303,   303,   303,   303,   303,   303,   304,   304,   304,
     304,   304,   304,   304,   304,   304,   304,   304,   304,   304,
     304,   304,   304,   304,   304,   304,   304,   304,   304,   304,
     304,   304,   304,   304,   304,   305,   305,   306,   307,   307,
     308,   308,   308,   308,   308,   309,   309,   310,   310,   310,
     310,   311,   311,   311,   311,   311,   311,   312,   312,   312,
     312,   313,   314,   313,   313,   315,   315,   315,   315,   316,
     316,   316,   317,   317,   317,   317,   318,   318,   318,   319,
     319,   319,   319,   319,   319,   320,   320,   320,   321,   321,
     322,   322,   324,   323,   325,   323,   326,   323,   327,   323,
     323,   328,   328,   329,   329,   330,   330,   331,   331,   331,
     332,   332,   332,   332,   332,   332,   332,   332,   333,   333,
     334,   334,   334,   334,   334,   334,   334,   334,   334,   334,
     335,   335,   335,   336,   336,   336,   337,   337,   337,   338,
     339,   339,   340,   340,   341,   341,   342,   343,   344,   343,
     343,   343,   343,   345,   343,   343,   343,   346,   346,   347,
     347,   347,   347,   348,   348,   348,   349,   349,   349,   349,
     349,   349,   349,   350,   350,   350,   350,   351,   351,   352,
     352,   352,   352,   353,   353,   353,   353,   354,   354,   354,
     354,   354,   355,   355,   355,   355,   355,   356,   356,   357,
     357,   358,   358,   359,   359,   359,   360,   360,   360,   361,
     361,   362,   362,   362,   362,   363,   363,   364,   364,   364,
     364,   364,   365,   365,   366,   366,   367,   367,   367,   367,
     367,   368,   368,   369,   369,   371,   370,   372,   370,   370,
     370,   373,   373,   373,   373,   374,   374,   374,   374,   375,
     375,   376,   376,   377,   377,   378,   378,   378,   378,   379,
     379,   379,   380,   380,   381,   381,   382,   382,   383,   383,
     384,   384,   385,   385,   385,   386,   386,   387,   387,   388,
     388,   389,   389,   390,   391,   392,   392,   392,   392,   392,
     393,   392,   394,   392,   395,   392,   396,   392,   397,   392,
     398,   398,   398,   399,   399,   400,   400,   400,   400,   400,
     400,   400,   400,   400,   400,   401,   401,   401,   402,   403,
     403,   404,   404,   405,   405,   406,   407,   407,   408,   408,
     408,   409,   409,   409,   409,   409,   409,   410,   410,   411,
     411,   411,   411,   412,   412,   412,   412,   413,   413,   413,
     413,   413,   413,   413,   414,   414,   414,   414,   415,   415,
     415,   416,   416,   416,   416,   416,   417,   417,   417,   417,
     418,   418,   418,   418,   418,   418,   419,   419,   419,   420,
     420,   420,   420,   420,   421,   421,   421,   421,   422,   422,
     422,   422,   422,   422,   423,   423,   424,   424,   424,   424,
     425,   425,   425,   425,   426,   426,   426,   426,   426,   426,
     426,   427,   427,   427,   427,   427,   428,   428,   428,   428,
     428,   429,   429,   429,   430,   430,   430,   430,   431,   431,
     431,   432,   432,   432,   432,   432,   433,   433,   434,   434,
     434,   435,   435,   436,   436,   437,   437,   437,   438,   438,
     438,   438,   438,   439,   439,   439,   439,   440,   440,   440,
     441,   441,   441,   441,   442,   442,   442,   442,   443,   443,
     443,   443,   444,   444,   444,   444,   444,   445,   445,   445,
     445,   446,   446,   446,   447,   447,   447,   448,   448,   448,
     448,   448,   448,   449,   449,   449,   450,   450,   450,   450,
     450,   451,   451,   451,   451,   452,   452,   453,   453,   453,
     454,   454,   455,   455,   455,   455,   455,   455,   456,   456,
     456,   456,   456,   456,   456,   456,   456,   456,   457,   457,
     457,   457,   458,   458,   458,   459,   459,   460,   460,   460,
     460,   460,   460,   461,   461,   461,   461,   461,   461,   462,
     462,   462,   463,   463,   464,   464,   465,   465
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
       1,     1,     1,     1,     1,     0,     1,     5,     0,     1,
       1,     2,     2,     3,     3,     1,     3,     1,     2,     2,
       2,     4,     4,     4,     4,     1,     1,     1,     2,     2,
       3,     1,     0,     3,     2,     1,     2,     2,     3,     1,
       2,     2,     1,     2,     2,     3,     1,     2,     2,     1,
       2,     3,     1,     2,     3,     1,     3,     4,     1,     1,
       1,     1,     0,     7,     0,     8,     0,     8,     0,     8,
       1,     0,     3,     3,     3,     1,     1,     2,     1,     1,
       1,     2,     1,     2,     1,     2,     1,     2,     0,     2,
       3,     4,     4,     3,     2,     2,     3,     3,     2,     1,
       0,     1,     4,     1,     2,     2,     0,     1,     4,     1,
       2,     3,     1,     2,     0,     1,     2,     6,     0,     8,
       7,     8,     9,     0,    12,    11,     1,     3,     3,     2,
       2,     4,     5,     0,     2,     5,     0,     1,     1,     1,
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
       0,     6,     0,     8,     0,     7,     0,     7,     0,     8,
       1,     2,     3,     0,     5,     3,     4,     4,     4,     4,
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
#line 567 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.enterScope(); }
#line 7360 "Parser/parser.cc"
    break;

  case 3:
#line 571 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 7366 "Parser/parser.cc"
    break;

  case 4:
#line 578 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantInteger( *(yyvsp[0].tok) ) ); }
#line 7372 "Parser/parser.cc"
    break;

  case 5:
#line 579 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7378 "Parser/parser.cc"
    break;

  case 6:
#line 580 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7384 "Parser/parser.cc"
    break;

  case 7:
#line 581 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7390 "Parser/parser.cc"
    break;

  case 8:
#line 582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantChar( *(yyvsp[0].tok) ) ); }
#line 7396 "Parser/parser.cc"
    break;

  case 19:
#line 603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 7402 "Parser/parser.cc"
    break;

  case 20:
#line 607 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.constant) = build_constantStr( *(yyvsp[0].str) ); }
#line 7408 "Parser/parser.cc"
    break;

  case 21:
#line 611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 7414 "Parser/parser.cc"
    break;

  case 22:
#line 613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 7424 "Parser/parser.cc"
    break;

  case 23:
#line 624 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 7430 "Parser/parser.cc"
    break;

  case 24:
#line 626 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 7436 "Parser/parser.cc"
    break;

  case 25:
#line 630 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_dimensionref( (yyvsp[0].tok) ) ); }
#line 7442 "Parser/parser.cc"
    break;

  case 27:
#line 633 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 7448 "Parser/parser.cc"
    break;

  case 28:
#line 635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new StmtExpr( dynamic_cast<CompoundStmt *>(maybeMoveBuild<Statement>((yyvsp[-1].sn)) ) ) ); }
#line 7454 "Parser/parser.cc"
    break;

  case 29:
#line 637 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7460 "Parser/parser.cc"
    break;

  case 30:
#line 639 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7466 "Parser/parser.cc"
    break;

  case 31:
#line 641 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild<Expression>( (yyvsp[-3].en) );
			(yyval.en) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 7476 "Parser/parser.cc"
    break;

  case 32:
#line 651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "Adjacent identifiers are not meaningful in an expression. "
											   "Possible problem is identifier \"", *(yyvsp[-1].tok).str,
											   "\" is a misspelled typename or an incorrectly specified type name, "
											   "e.g., missing generic parameter or missing struct/union/enum before typename." ) );
			(yyval.en) = nullptr;
 		}
#line 7488 "Parser/parser.cc"
    break;

  case 33:
#line 659 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "Identifier \"", *(yyvsp[-1].tok).str, "\" cannot appear before a type. "
											   "Possible problem is misspelled storage or CV qualifier." ) );
			(yyval.en) = nullptr;
		}
#line 7498 "Parser/parser.cc"
    break;

  case 35:
#line 669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// steal the association node from the singleton and delete the wrapper
			(yyvsp[-2].genexpr)->associations.splice((yyvsp[-2].genexpr)->associations.end(), (yyvsp[0].genexpr)->associations);
			delete (yyvsp[0].genexpr);
			(yyval.genexpr) = (yyvsp[-2].genexpr);
		}
#line 7509 "Parser/parser.cc"
    break;

  case 36:
#line 679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuildType((yyvsp[-2].decl)), maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } );
		}
#line 7518 "Parser/parser.cc"
    break;

  case 37:
#line 684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } ); }
#line 7524 "Parser/parser.cc"
    break;

  case 39:
#line 693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-5].en), new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) ) ) )) ) ); }
#line 7530 "Parser/parser.cc"
    break;

  case 40:
#line 699 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7536 "Parser/parser.cc"
    break;

  case 41:
#line 701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7542 "Parser/parser.cc"
    break;

  case 42:
#line 703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, new ExpressionNode( (yyvsp[-3].constant) ), (yyvsp[-1].en) ) ); }
#line 7548 "Parser/parser.cc"
    break;

  case 43:
#line 705 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.en) = new ExpressionNode( new ConstructorExpr( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) ) );
		}
#line 7558 "Parser/parser.cc"
    break;

  case 44:
#line 711 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7564 "Parser/parser.cc"
    break;

  case 45:
#line 713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 7570 "Parser/parser.cc"
    break;

  case 46:
#line 715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 7576 "Parser/parser.cc"
    break;

  case 47:
#line 717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), new ExpressionNode( (yyvsp[-2].constant) ) ) ); }
#line 7582 "Parser/parser.cc"
    break;

  case 48:
#line 719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7588 "Parser/parser.cc"
    break;

  case 49:
#line 721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 7594 "Parser/parser.cc"
    break;

  case 50:
#line 723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-1].en), build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) ) ) ); }
#line 7600 "Parser/parser.cc"
    break;

  case 51:
#line 725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7606 "Parser/parser.cc"
    break;

  case 52:
#line 727 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[0].aggKey), (yyvsp[-2].en) ) ); }
#line 7612 "Parser/parser.cc"
    break;

  case 53:
#line 729 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7618 "Parser/parser.cc"
    break;

  case 54:
#line 731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 7624 "Parser/parser.cc"
    break;

  case 55:
#line 733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7630 "Parser/parser.cc"
    break;

  case 56:
#line 735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::IncrPost, (yyvsp[-1].en) ) ); }
#line 7636 "Parser/parser.cc"
    break;

  case 57:
#line 737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::DecrPost, (yyvsp[-1].en) ) ); }
#line 7642 "Parser/parser.cc"
    break;

  case 58:
#line 739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].in), true ) ) ); }
#line 7648 "Parser/parser.cc"
    break;

  case 59:
#line 741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].in), true ))->set_maybeConstructed( false ) ) ); }
#line 7654 "Parser/parser.cc"
    break;

  case 60:
#line 743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) );
		}
#line 7664 "Parser/parser.cc"
    break;

  case 61:
#line 752 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7670 "Parser/parser.cc"
    break;

  case 64:
#line 759 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7676 "Parser/parser.cc"
    break;

  case 65:
#line 764 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Default parameter for argument is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7682 "Parser/parser.cc"
    break;

  case 68:
#line 771 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7688 "Parser/parser.cc"
    break;

  case 70:
#line 777 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-1].tok) ) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7694 "Parser/parser.cc"
    break;

  case 71:
#line 779 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-3].tok) ) ), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7700 "Parser/parser.cc"
    break;

  case 72:
#line 781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7706 "Parser/parser.cc"
    break;

  case 73:
#line 783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7712 "Parser/parser.cc"
    break;

  case 74:
#line 785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7718 "Parser/parser.cc"
    break;

  case 75:
#line 787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7724 "Parser/parser.cc"
    break;

  case 76:
#line 792 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_constantInteger( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 7730 "Parser/parser.cc"
    break;

  case 77:
#line 794 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_field_name_FLOATINGconstant( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 7736 "Parser/parser.cc"
    break;

  case 78:
#line 796 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_varref( (yyvsp[-1].tok) ), (yyvsp[0].en) ) );
		}
#line 7744 "Parser/parser.cc"
    break;

  case 79:
#line 803 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7750 "Parser/parser.cc"
    break;

  case 80:
#line 805 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Expression * constant = build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) );
			(yyval.en) = (yyvsp[-1].en) != nullptr ? new ExpressionNode( build_fieldSel( (yyvsp[-1].en),  constant ) ) : new ExpressionNode( constant );
		}
#line 7759 "Parser/parser.cc"
    break;

  case 83:
#line 817 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 7765 "Parser/parser.cc"
    break;

  case 84:
#line 819 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en)->set_extension( true ); }
#line 7771 "Parser/parser.cc"
    break;

  case 85:
#line 824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 7791 "Parser/parser.cc"
    break;

  case 86:
#line 840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_val( (yyvsp[-1].op), (yyvsp[0].en) ) ); }
#line 7797 "Parser/parser.cc"
    break;

  case 87:
#line 842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Incr, (yyvsp[0].en) ) ); }
#line 7803 "Parser/parser.cc"
    break;

  case 88:
#line 844 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Decr, (yyvsp[0].en) ) ); }
#line 7809 "Parser/parser.cc"
    break;

  case 89:
#line 846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7815 "Parser/parser.cc"
    break;

  case 90:
#line 848 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 7821 "Parser/parser.cc"
    break;

  case 91:
#line 850 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7827 "Parser/parser.cc"
    break;

  case 92:
#line 852 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 7833 "Parser/parser.cc"
    break;

  case 93:
#line 854 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_offsetOf( (yyvsp[-3].decl), build_varref( (yyvsp[-1].tok) ) ) ); }
#line 7839 "Parser/parser.cc"
    break;

  case 94:
#line 856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.en) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 7848 "Parser/parser.cc"
    break;

  case 95:
#line 863 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::PointTo; }
#line 7854 "Parser/parser.cc"
    break;

  case 96:
#line 864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AddressOf; }
#line 7860 "Parser/parser.cc"
    break;

  case 97:
#line 866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::And; }
#line 7866 "Parser/parser.cc"
    break;

  case 98:
#line 870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::UnPlus; }
#line 7872 "Parser/parser.cc"
    break;

  case 99:
#line 871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::UnMinus; }
#line 7878 "Parser/parser.cc"
    break;

  case 100:
#line 872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::Neg; }
#line 7884 "Parser/parser.cc"
    break;

  case 101:
#line 873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::BitNeg; }
#line 7890 "Parser/parser.cc"
    break;

  case 103:
#line 879 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cast( (yyvsp[-2].decl), (yyvsp[0].en) ) ); }
#line 7896 "Parser/parser.cc"
    break;

  case 104:
#line 881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 7902 "Parser/parser.cc"
    break;

  case 105:
#line 883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 7908 "Parser/parser.cc"
    break;

  case 106:
#line 885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( nullptr ) ) ); }
#line 7914 "Parser/parser.cc"
    break;

  case 107:
#line 887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 7920 "Parser/parser.cc"
    break;

  case 108:
#line 889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Return cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7926 "Parser/parser.cc"
    break;

  case 109:
#line 891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7932 "Parser/parser.cc"
    break;

  case 110:
#line 893 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7938 "Parser/parser.cc"
    break;

  case 118:
#line 913 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Exp, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7944 "Parser/parser.cc"
    break;

  case 120:
#line 919 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mul, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7950 "Parser/parser.cc"
    break;

  case 121:
#line 921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Div, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7956 "Parser/parser.cc"
    break;

  case 122:
#line 923 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mod, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7962 "Parser/parser.cc"
    break;

  case 124:
#line 929 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Plus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7968 "Parser/parser.cc"
    break;

  case 125:
#line 931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Minus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7974 "Parser/parser.cc"
    break;

  case 127:
#line 937 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7980 "Parser/parser.cc"
    break;

  case 128:
#line 939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::RShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7986 "Parser/parser.cc"
    break;

  case 130:
#line 945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7992 "Parser/parser.cc"
    break;

  case 131:
#line 947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7998 "Parser/parser.cc"
    break;

  case 132:
#line 949 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8004 "Parser/parser.cc"
    break;

  case 133:
#line 951 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8010 "Parser/parser.cc"
    break;

  case 135:
#line 957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Eq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8016 "Parser/parser.cc"
    break;

  case 136:
#line 959 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Neq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8022 "Parser/parser.cc"
    break;

  case 138:
#line 965 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitAnd, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8028 "Parser/parser.cc"
    break;

  case 140:
#line 971 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Xor, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8034 "Parser/parser.cc"
    break;

  case 142:
#line 977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitOr, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8040 "Parser/parser.cc"
    break;

  case 144:
#line 983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), true ) ); }
#line 8046 "Parser/parser.cc"
    break;

  case 146:
#line 989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), false ) ); }
#line 8052 "Parser/parser.cc"
    break;

  case 148:
#line 995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8058 "Parser/parser.cc"
    break;

  case 149:
#line 998 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-3].en), (yyvsp[-3].en), (yyvsp[0].en) ) ); }
#line 8064 "Parser/parser.cc"
    break;

  case 152:
#line 1009 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
//			if ( $2 == OperKinds::AtAssn ) {
//				SemanticError( yylloc, "C @= assignment is currently unimplemented." ); $$ = nullptr;
//			} else {
				(yyval.en) = new ExpressionNode( build_binary_val( (yyvsp[-1].op), (yyvsp[-2].en), (yyvsp[0].en) ) );
//			} // if
		}
#line 8076 "Parser/parser.cc"
    break;

  case 153:
#line 1017 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8082 "Parser/parser.cc"
    break;

  case 154:
#line 1022 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8088 "Parser/parser.cc"
    break;

  case 158:
#line 1032 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::Assign; }
#line 8094 "Parser/parser.cc"
    break;

  case 159:
#line 1033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AtAssn; }
#line 8100 "Parser/parser.cc"
    break;

  case 160:
#line 1037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ExpAssn; }
#line 8106 "Parser/parser.cc"
    break;

  case 161:
#line 1038 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MulAssn; }
#line 8112 "Parser/parser.cc"
    break;

  case 162:
#line 1039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::DivAssn; }
#line 8118 "Parser/parser.cc"
    break;

  case 163:
#line 1040 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ModAssn; }
#line 8124 "Parser/parser.cc"
    break;

  case 164:
#line 1041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::PlusAssn; }
#line 8130 "Parser/parser.cc"
    break;

  case 165:
#line 1042 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MinusAssn; }
#line 8136 "Parser/parser.cc"
    break;

  case 166:
#line 1043 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::LSAssn; }
#line 8142 "Parser/parser.cc"
    break;

  case 167:
#line 1044 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::RSAssn; }
#line 8148 "Parser/parser.cc"
    break;

  case 168:
#line 1045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AndAssn; }
#line 8154 "Parser/parser.cc"
    break;

  case 169:
#line 1046 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ERAssn; }
#line 8160 "Parser/parser.cc"
    break;

  case 170:
#line 1047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::OrAssn; }
#line 8166 "Parser/parser.cc"
    break;

  case 171:
#line 1058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)(new ExpressionNode( nullptr ) )->set_last( (yyvsp[-1].en) ) ) ); }
#line 8172 "Parser/parser.cc"
    break;

  case 172:
#line 1060 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-4].en)->set_last( (yyvsp[-1].en) ) ) )); }
#line 8178 "Parser/parser.cc"
    break;

  case 174:
#line 1066 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8184 "Parser/parser.cc"
    break;

  case 175:
#line 1068 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 8190 "Parser/parser.cc"
    break;

  case 176:
#line 1070 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8196 "Parser/parser.cc"
    break;

  case 178:
#line 1076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new CommaExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 8202 "Parser/parser.cc"
    break;

  case 179:
#line 1081 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8208 "Parser/parser.cc"
    break;

  case 191:
#line 1099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 8214 "Parser/parser.cc"
    break;

  case 193:
#line 1102 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_directive( (yyvsp[0].tok) ) ); }
#line 8220 "Parser/parser.cc"
    break;

  case 194:
#line 1108 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[0].sn)->add_label( (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 8226 "Parser/parser.cc"
    break;

  case 195:
#line 1110 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "Label \"", *(yyvsp[-3].tok).str, "\" must be associated with a statement, "
											   "where a declaration, case, or default is not a statement. "
											   "Move the label or terminate with a semi-colon." ) );
			(yyval.sn) = nullptr;
		}
#line 8237 "Parser/parser.cc"
    break;

  case 196:
#line 1120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (StatementNode *)0 ) ); }
#line 8243 "Parser/parser.cc"
    break;

  case 197:
#line 1125 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (yyvsp[-2].sn) ) ); }
#line 8249 "Parser/parser.cc"
    break;

  case 199:
#line 1131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 8255 "Parser/parser.cc"
    break;

  case 200:
#line 1136 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8261 "Parser/parser.cc"
    break;

  case 201:
#line 1138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8267 "Parser/parser.cc"
    break;

  case 202:
#line 1140 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8273 "Parser/parser.cc"
    break;

  case 203:
#line 1142 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8279 "Parser/parser.cc"
    break;

  case 206:
#line 1149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 8285 "Parser/parser.cc"
    break;

  case 207:
#line 1151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Declarations only allowed at the start of the switch body, i.e., after the '{'." ); (yyval.sn) = nullptr; }
#line 8291 "Parser/parser.cc"
    break;

  case 208:
#line 1156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_expr( (yyvsp[-1].en) ) ); }
#line 8297 "Parser/parser.cc"
    break;

  case 209:
#line 1158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_mutex( nullptr, new StatementNode( build_expr( (yyvsp[-1].en) ) ) ) ); }
#line 8303 "Parser/parser.cc"
    break;

  case 210:
#line 1165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn); }
#line 8309 "Parser/parser.cc"
    break;

  case 211:
#line 1167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( true, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8315 "Parser/parser.cc"
    break;

  case 212:
#line 1169 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( true, (yyvsp[-7].en), (yyvsp[-2].sn) ) );
			// The semantics of the declaration list is changed to include associated initialization, which is performed
			// *before* the transfer to the appropriate case clause by hoisting the declarations into a compound
			// statement around the switch.  Statements after the initial declaration list can never be executed, and
			// therefore, are removed from the grammar even though C allows it. The change also applies to choose
			// statement.
			(yyval.sn) = (yyvsp[-3].decl) ? new StatementNode( build_compound( (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 8329 "Parser/parser.cc"
    break;

  case 213:
#line 1179 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Only declarations can appear before the list of case clauses." ); (yyval.sn) = nullptr; }
#line 8335 "Parser/parser.cc"
    break;

  case 214:
#line 1181 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( false, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8341 "Parser/parser.cc"
    break;

  case 215:
#line 1183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( false, (yyvsp[-7].en), (yyvsp[-2].sn) ) );
			(yyval.sn) = (yyvsp[-3].decl) ? new StatementNode( build_compound( (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 8350 "Parser/parser.cc"
    break;

  case 216:
#line 1188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Only declarations can appear before the list of case clauses." ); (yyval.sn) = nullptr; }
#line 8356 "Parser/parser.cc"
    break;

  case 217:
#line 1194 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-2].ifctl), maybe_build_compound( (yyvsp[0].sn) ), nullptr ) ); }
#line 8362 "Parser/parser.cc"
    break;

  case 218:
#line 1196 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-4].ifctl), maybe_build_compound( (yyvsp[-2].sn) ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8368 "Parser/parser.cc"
    break;

  case 219:
#line 1201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( nullptr, (yyvsp[0].en) ); }
#line 8374 "Parser/parser.cc"
    break;

  case 220:
#line 1203 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 8380 "Parser/parser.cc"
    break;

  case 221:
#line 1205 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 8386 "Parser/parser.cc"
    break;

  case 222:
#line 1207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[-1].decl), (yyvsp[0].en) ); }
#line 8392 "Parser/parser.cc"
    break;

  case 223:
#line 1214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = (yyvsp[0].en); }
#line 8398 "Parser/parser.cc"
    break;

  case 224:
#line 1216 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 8404 "Parser/parser.cc"
    break;

  case 226:
#line 1221 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.sn) = new StatementNode( build_case( (yyvsp[0].en) ) ); }
#line 8410 "Parser/parser.cc"
    break;

  case 227:
#line 1223 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)((yyvsp[-2].sn)->set_last( new StatementNode( build_case( (yyvsp[0].en) ) ) ) ); }
#line 8416 "Parser/parser.cc"
    break;

  case 228:
#line 1228 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing case list after case." ); (yyval.sn) = nullptr; }
#line 8422 "Parser/parser.cc"
    break;

  case 229:
#line 1229 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn); }
#line 8428 "Parser/parser.cc"
    break;

  case 230:
#line 1231 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing colon after case list." ); (yyval.sn) = nullptr; }
#line 8434 "Parser/parser.cc"
    break;

  case 231:
#line 1232 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.sn) = new StatementNode( build_default() ); }
#line 8440 "Parser/parser.cc"
    break;

  case 232:
#line 1235 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing colon after default." ); (yyval.sn) = nullptr; }
#line 8446 "Parser/parser.cc"
    break;

  case 234:
#line 1240 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)( (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) )); }
#line 8452 "Parser/parser.cc"
    break;

  case 235:
#line 1244 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( maybe_build_compound( (yyvsp[0].sn) ) ); }
#line 8458 "Parser/parser.cc"
    break;

  case 236:
#line 1249 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = nullptr; }
#line 8464 "Parser/parser.cc"
    break;

  case 238:
#line 1255 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ); }
#line 8470 "Parser/parser.cc"
    break;

  case 239:
#line 1257 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)( (yyvsp[-2].sn)->set_last( (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ) ) ); }
#line 8476 "Parser/parser.cc"
    break;

  case 240:
#line 1262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8482 "Parser/parser.cc"
    break;

  case 241:
#line 1264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_while( new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( (yyvsp[-2].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse, "" );
		}
#line 8491 "Parser/parser.cc"
    break;

  case 242:
#line 1269 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( (yyvsp[-2].ifctl), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8497 "Parser/parser.cc"
    break;

  case 243:
#line 1271 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( (yyvsp[-4].ifctl), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[0].sn) ) ); }
#line 8503 "Parser/parser.cc"
    break;

  case 244:
#line 1273 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( NEW_ONE, maybe_build_compound( (yyvsp[-4].sn) ) ) ); }
#line 8509 "Parser/parser.cc"
    break;

  case 245:
#line 1275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_do_while( NEW_ONE, maybe_build_compound( (yyvsp[-5].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse, "" );
		}
#line 8518 "Parser/parser.cc"
    break;

  case 246:
#line 1280 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( (yyvsp[-2].en), maybe_build_compound( (yyvsp[-5].sn) ) ) ); }
#line 8524 "Parser/parser.cc"
    break;

  case 247:
#line 1282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( (yyvsp[-3].en), maybe_build_compound( (yyvsp[-6].sn) ), (yyvsp[0].sn) ) ); }
#line 8530 "Parser/parser.cc"
    break;

  case 248:
#line 1284 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( new ForCtrl( (ExpressionNode * )nullptr, (ExpressionNode * )nullptr, (ExpressionNode * )nullptr ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8536 "Parser/parser.cc"
    break;

  case 249:
#line 1286 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_for( new ForCtrl( (ExpressionNode * )nullptr, (ExpressionNode * )nullptr, (ExpressionNode * )nullptr ), maybe_build_compound( (yyvsp[-2].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse, "" );
		}
#line 8545 "Parser/parser.cc"
    break;

  case 250:
#line 1291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( (yyvsp[-2].fctl), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8551 "Parser/parser.cc"
    break;

  case 251:
#line 1293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( (yyvsp[-4].fctl), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[0].sn) ) ); }
#line 8557 "Parser/parser.cc"
    break;

  case 253:
#line 1303 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 8576 "Parser/parser.cc"
    break;

  case 254:
#line 1321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (ExpressionNode * )nullptr, (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8582 "Parser/parser.cc"
    break;

  case 255:
#line 1323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8588 "Parser/parser.cc"
    break;

  case 256:
#line 1325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-3].decl), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8594 "Parser/parser.cc"
    break;

  case 257:
#line 1328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (ExpressionNode *)nullptr, (yyvsp[0].en), nullptr ); }
#line 8600 "Parser/parser.cc"
    break;

  case 258:
#line 1330 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (ExpressionNode *)nullptr, (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8606 "Parser/parser.cc"
    break;

  case 259:
#line 1333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), NEW_ZERO, OperKinds::LThan, (yyvsp[0].en)->clone(), NEW_ONE ); }
#line 8612 "Parser/parser.cc"
    break;

  case 260:
#line 1335 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].compop), NEW_ZERO, (yyvsp[0].en)->clone() ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 8618 "Parser/parser.cc"
    break;

  case 261:
#line 1338 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].compop), (yyvsp[-2].en)->clone(), (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), (yyvsp[-2].en)->clone() ), NEW_ONE ); }
#line 8624 "Parser/parser.cc"
    break;

  case 262:
#line 1340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[0].en)->clone(), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 8633 "Parser/parser.cc"
    break;

  case 263:
#line 1345 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; } 
		}
#line 8642 "Parser/parser.cc"
    break;

  case 264:
#line 1350 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en)->clone(), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), (yyvsp[0].en) ); }
#line 8648 "Parser/parser.cc"
    break;

  case 265:
#line 1352 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-2].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].en)->clone(), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 8657 "Parser/parser.cc"
    break;

  case 266:
#line 1357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; } 
		}
#line 8666 "Parser/parser.cc"
    break;

  case 267:
#line 1362 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 8672 "Parser/parser.cc"
    break;

  case 268:
#line 1364 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 8678 "Parser/parser.cc"
    break;

  case 269:
#line 1366 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 8684 "Parser/parser.cc"
    break;

  case 270:
#line 1368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 8690 "Parser/parser.cc"
    break;

  case 271:
#line 1370 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 8696 "Parser/parser.cc"
    break;

  case 272:
#line 1373 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-2].en), NEW_ZERO, OperKinds::LThan, (yyvsp[0].en)->clone(), NEW_ONE ); }
#line 8702 "Parser/parser.cc"
    break;

  case 273:
#line 1375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-3].en), UPDOWN( (yyvsp[-1].compop), NEW_ZERO, (yyvsp[0].en)->clone() ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 8708 "Parser/parser.cc"
    break;

  case 274:
#line 1378 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), UPDOWN( (yyvsp[-1].compop), (yyvsp[-2].en)->clone(), (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), (yyvsp[-2].en)->clone() ), NEW_ONE ); }
#line 8714 "Parser/parser.cc"
    break;

  case 275:
#line 1380 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-4].en), (yyvsp[0].en)->clone(), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 8723 "Parser/parser.cc"
    break;

  case 276:
#line 1385 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::GThan || (yyvsp[-1].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 8733 "Parser/parser.cc"
    break;

  case 277:
#line 1391 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing low/high value for up/down-to range so index is uninitialized." ); (yyval.fctl) = nullptr; }
#line 8739 "Parser/parser.cc"
    break;

  case 278:
#line 1394 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en)->clone(), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), (yyvsp[0].en) ); }
#line 8745 "Parser/parser.cc"
    break;

  case 279:
#line 1396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-6].en), (yyvsp[-2].en)->clone(), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 8754 "Parser/parser.cc"
    break;

  case 280:
#line 1401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 8764 "Parser/parser.cc"
    break;

  case 281:
#line 1407 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en)->clone(), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), nullptr ); }
#line 8770 "Parser/parser.cc"
    break;

  case 282:
#line 1409 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-6].en), (yyvsp[-2].en)->clone(), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 8779 "Parser/parser.cc"
    break;

  case 283:
#line 1414 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 8789 "Parser/parser.cc"
    break;

  case 284:
#line 1420 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing low/high value for up/down-to range so index is uninitialized." ); (yyval.fctl) = nullptr; }
#line 8795 "Parser/parser.cc"
    break;

  case 285:
#line 1423 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-1].decl), NEW_ZERO, OperKinds::LThan, (yyvsp[0].en), NEW_ONE ); }
#line 8801 "Parser/parser.cc"
    break;

  case 286:
#line 1425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].decl), UPDOWN( (yyvsp[-1].compop), NEW_ZERO, (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 8807 "Parser/parser.cc"
    break;

  case 287:
#line 1428 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-3].decl), UPDOWN( (yyvsp[-1].compop), (yyvsp[-2].en)->clone(), (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), (yyvsp[-2].en)->clone() ), NEW_ONE ); }
#line 8813 "Parser/parser.cc"
    break;

  case 288:
#line 1430 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-3].decl), (yyvsp[0].en), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 8822 "Parser/parser.cc"
    break;

  case 289:
#line 1435 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::GThan || (yyvsp[-1].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-3].decl), (yyvsp[-2].en), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 8832 "Parser/parser.cc"
    break;

  case 290:
#line 1442 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), (yyvsp[0].en) ); }
#line 8838 "Parser/parser.cc"
    break;

  case 291:
#line 1444 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-2].en), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 8847 "Parser/parser.cc"
    break;

  case 292:
#line 1449 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-4].en), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 8857 "Parser/parser.cc"
    break;

  case 293:
#line 1455 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), nullptr ); }
#line 8863 "Parser/parser.cc"
    break;

  case 294:
#line 1457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-2].en), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 8872 "Parser/parser.cc"
    break;

  case 295:
#line 1462 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-4].en), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 8882 "Parser/parser.cc"
    break;

  case 296:
#line 1468 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing low/high value for up/down-to range so index is uninitialized." ); (yyval.fctl) = nullptr; }
#line 8888 "Parser/parser.cc"
    break;

  case 297:
#line 1471 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.fctl) = nullptr;
			//$$ = forCtrl( new ExpressionNode( build_varref( $3 ) ), $1, nullptr, OperKinds::Range, nullptr, nullptr );
		}
#line 8897 "Parser/parser.cc"
    break;

  case 298:
#line 1476 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LEThan || (yyvsp[-1].compop) == OperKinds::GEThan ) { SemanticError( yylloc, "All enumation ranges are equal (all values). Remove \"=~\"." ); (yyval.fctl) = nullptr; }
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.fctl) = nullptr;
		}
#line 8906 "Parser/parser.cc"
    break;

  case 299:
#line 1484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GThan; }
#line 8912 "Parser/parser.cc"
    break;

  case 300:
#line 1486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 8918 "Parser/parser.cc"
    break;

  case 301:
#line 1488 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GEThan; }
#line 8924 "Parser/parser.cc"
    break;

  case 302:
#line 1493 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LThan; }
#line 8930 "Parser/parser.cc"
    break;

  case 303:
#line 1495 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GThan; }
#line 8936 "Parser/parser.cc"
    break;

  case 305:
#line 1501 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 8942 "Parser/parser.cc"
    break;

  case 306:
#line 1503 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GEThan; }
#line 8948 "Parser/parser.cc"
    break;

  case 307:
#line 1508 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Goto ) ); }
#line 8954 "Parser/parser.cc"
    break;

  case 308:
#line 1512 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_computedgoto( (yyvsp[-1].en) ) ); }
#line 8960 "Parser/parser.cc"
    break;

  case 309:
#line 1515 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThrough ) ); }
#line 8966 "Parser/parser.cc"
    break;

  case 310:
#line 1517 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::FallThrough ) ); }
#line 8972 "Parser/parser.cc"
    break;

  case 311:
#line 1519 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThroughDefault ) ); }
#line 8978 "Parser/parser.cc"
    break;

  case 312:
#line 1522 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Continue ) ); }
#line 8984 "Parser/parser.cc"
    break;

  case 313:
#line 1526 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Continue ) ); }
#line 8990 "Parser/parser.cc"
    break;

  case 314:
#line 1529 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Break ) ); }
#line 8996 "Parser/parser.cc"
    break;

  case 315:
#line 1533 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Break ) ); }
#line 9002 "Parser/parser.cc"
    break;

  case 316:
#line 1535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_return( (yyvsp[-1].en) ) ); }
#line 9008 "Parser/parser.cc"
    break;

  case 317:
#line 1537 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 9014 "Parser/parser.cc"
    break;

  case 318:
#line 1539 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr ) ); }
#line 9020 "Parser/parser.cc"
    break;

  case 319:
#line 1541 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn) ) ); }
#line 9026 "Parser/parser.cc"
    break;

  case 320:
#line 1543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Coroutine ) ); }
#line 9032 "Parser/parser.cc"
    break;

  case 321:
#line 1545 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Coroutine ) ); }
#line 9038 "Parser/parser.cc"
    break;

  case 322:
#line 1547 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Generator ) ); }
#line 9044 "Parser/parser.cc"
    break;

  case 323:
#line 1549 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Generator ) ); }
#line 9050 "Parser/parser.cc"
    break;

  case 324:
#line 1551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_throw( (yyvsp[-1].en) ) ); }
#line 9056 "Parser/parser.cc"
    break;

  case 325:
#line 1553 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume( (yyvsp[-1].en) ) ); }
#line 9062 "Parser/parser.cc"
    break;

  case 326:
#line 1555 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume_at( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 9068 "Parser/parser.cc"
    break;

  case 329:
#line 1565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_with( (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 9074 "Parser/parser.cc"
    break;

  case 330:
#line 1571 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_mutex( (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 9080 "Parser/parser.cc"
    break;

  case 331:
#line 1575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 9086 "Parser/parser.cc"
    break;

  case 332:
#line 1580 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9092 "Parser/parser.cc"
    break;

  case 334:
#line 1586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 9098 "Parser/parser.cc"
    break;

  case 335:
#line 1590 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) )); }
#line 9104 "Parser/parser.cc"
    break;

  case 337:
#line 1597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.en) = nullptr; }
#line 9110 "Parser/parser.cc"
    break;

  case 338:
#line 1601 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 9116 "Parser/parser.cc"
    break;

  case 339:
#line 1606 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 9122 "Parser/parser.cc"
    break;

  case 340:
#line 1608 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-3].en), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[-4].en), (yyvsp[0].wfs) ); }
#line 9128 "Parser/parser.cc"
    break;

  case 341:
#line 1610 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 9134 "Parser/parser.cc"
    break;

  case 342:
#line 1612 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( nullptr, maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 9140 "Parser/parser.cc"
    break;

  case 343:
#line 1615 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 9146 "Parser/parser.cc"
    break;

  case 344:
#line 1617 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-5].en), maybe_build_compound( (yyvsp[-4].sn) ), (yyvsp[-6].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 9152 "Parser/parser.cc"
    break;

  case 345:
#line 1622 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-1].en), (yyvsp[0].sn), (yyvsp[-2].en) ) ); }
#line 9158 "Parser/parser.cc"
    break;

  case 346:
#line 1624 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-3].en), (yyvsp[-2].sn), (yyvsp[-4].en), (yyvsp[0].wfs) ) ); }
#line 9164 "Parser/parser.cc"
    break;

  case 347:
#line 1629 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), (yyvsp[0].sn), 0 ) ); }
#line 9170 "Parser/parser.cc"
    break;

  case 348:
#line 1631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), 0, (yyvsp[0].sn) ) ); }
#line 9176 "Parser/parser.cc"
    break;

  case 349:
#line 1633 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-2].sn), (yyvsp[-1].sn), (yyvsp[0].sn) ) ); }
#line 9182 "Parser/parser.cc"
    break;

  case 350:
#line 1638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 9188 "Parser/parser.cc"
    break;

  case 351:
#line 1640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)(yyvsp[-8].sn)->set_last( new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ) ); }
#line 9194 "Parser/parser.cc"
    break;

  case 352:
#line 1645 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9200 "Parser/parser.cc"
    break;

  case 353:
#line 1646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[0].en); }
#line 9206 "Parser/parser.cc"
    break;

  case 354:
#line 1650 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 9212 "Parser/parser.cc"
    break;

  case 355:
#line 1651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 9218 "Parser/parser.cc"
    break;

  case 356:
#line 1652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.catch_kind) = CatchStmt::Resume; }
#line 9224 "Parser/parser.cc"
    break;

  case 357:
#line 1653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Resume; }
#line 9230 "Parser/parser.cc"
    break;

  case 358:
#line 1657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = new StatementNode( build_finally( (yyvsp[0].sn) ) ); }
#line 9236 "Parser/parser.cc"
    break;

  case 360:
#line 1664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 9242 "Parser/parser.cc"
    break;

  case 361:
#line 1666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 9248 "Parser/parser.cc"
    break;

  case 362:
#line 1668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 9254 "Parser/parser.cc"
    break;

  case 367:
#line 1683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-4].flag), (yyvsp[-2].constant), 0 ) ); }
#line 9260 "Parser/parser.cc"
    break;

  case 368:
#line 1685 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-6].flag), (yyvsp[-4].constant), (yyvsp[-2].en) ) ); }
#line 9266 "Parser/parser.cc"
    break;

  case 369:
#line 1687 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-8].flag), (yyvsp[-6].constant), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 9272 "Parser/parser.cc"
    break;

  case 370:
#line 1689 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-10].flag), (yyvsp[-8].constant), (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 9278 "Parser/parser.cc"
    break;

  case 371:
#line 1691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-12].flag), (yyvsp[-9].constant), 0, (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].label) ) ); }
#line 9284 "Parser/parser.cc"
    break;

  case 372:
#line 1696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = false; }
#line 9290 "Parser/parser.cc"
    break;

  case 373:
#line 1698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = true; }
#line 9296 "Parser/parser.cc"
    break;

  case 374:
#line 1703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9302 "Parser/parser.cc"
    break;

  case 377:
#line 1710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 9308 "Parser/parser.cc"
    break;

  case 378:
#line 1715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( nullptr, (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 9314 "Parser/parser.cc"
    break;

  case 379:
#line 1717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( (yyvsp[-5].tok), (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 9320 "Parser/parser.cc"
    break;

  case 380:
#line 1722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9326 "Parser/parser.cc"
    break;

  case 381:
#line 1724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 9332 "Parser/parser.cc"
    break;

  case 382:
#line 1726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( (yyvsp[0].constant) ) )); }
#line 9338 "Parser/parser.cc"
    break;

  case 383:
#line 1731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = new LabelNode(); (yyval.label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 9347 "Parser/parser.cc"
    break;

  case 384:
#line 1736 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = (yyvsp[-2].label); (yyvsp[-2].label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 9356 "Parser/parser.cc"
    break;

  case 385:
#line 1746 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9362 "Parser/parser.cc"
    break;

  case 388:
#line 1753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 9368 "Parser/parser.cc"
    break;

  case 389:
#line 1758 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9374 "Parser/parser.cc"
    break;

  case 391:
#line 1764 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 9380 "Parser/parser.cc"
    break;

  case 392:
#line 1766 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-2].decl) ); }
#line 9386 "Parser/parser.cc"
    break;

  case 399:
#line 1786 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "C_DECLARATION1 %p %s\n", $$, $$->name ? $$->name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 9397 "Parser/parser.cc"
    break;

  case 402:
#line 1798 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-4].en), (yyvsp[-2].constant) ); }
#line 9403 "Parser/parser.cc"
    break;

  case 403:
#line 1800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-2].en), build_constantStr( *new string( "\"\"" ) ) ); }
#line 9409 "Parser/parser.cc"
    break;

  case 407:
#line 1818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9415 "Parser/parser.cc"
    break;

  case 409:
#line 1824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].in) ); }
#line 9421 "Parser/parser.cc"
    break;

  case 410:
#line 1828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 9427 "Parser/parser.cc"
    break;

  case 411:
#line 1830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].in) ) ); }
#line 9433 "Parser/parser.cc"
    break;

  case 412:
#line 1837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 9439 "Parser/parser.cc"
    break;

  case 413:
#line 1839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 9445 "Parser/parser.cc"
    break;

  case 414:
#line 1841 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 9451 "Parser/parser.cc"
    break;

  case 416:
#line 1847 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9457 "Parser/parser.cc"
    break;

  case 417:
#line 1849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9463 "Parser/parser.cc"
    break;

  case 418:
#line 1851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 9469 "Parser/parser.cc"
    break;

  case 419:
#line 1853 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeClone( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->appendList( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 9480 "Parser/parser.cc"
    break;

  case 420:
#line 1886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), 0 )->addQualifiers( (yyvsp[0].decl) ); }
#line 9486 "Parser/parser.cc"
    break;

  case 421:
#line 1888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), 0 )->addQualifiers( (yyvsp[0].decl) ); }
#line 9492 "Parser/parser.cc"
    break;

  case 422:
#line 1893 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 9498 "Parser/parser.cc"
    break;

  case 423:
#line 1896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-6].decl)->appendList( (yyvsp[-2].decl) ) ); }
#line 9504 "Parser/parser.cc"
    break;

  case 424:
#line 1901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 9513 "Parser/parser.cc"
    break;

  case 425:
#line 1906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 9522 "Parser/parser.cc"
    break;

  case 426:
#line 1911 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "3" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 9531 "Parser/parser.cc"
    break;

  case 427:
#line 1922 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "4" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef();
		}
#line 9540 "Parser/parser.cc"
    break;

  case 428:
#line 1927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "5" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 9549 "Parser/parser.cc"
    break;

  case 429:
#line 1932 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "6" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[-3].decl) )->addTypedef();
		}
#line 9558 "Parser/parser.cc"
    break;

  case 430:
#line 1937 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "7" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-2].decl) )->addTypedef();
		}
#line 9567 "Parser/parser.cc"
    break;

  case 431:
#line 1942 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "8" );
			(yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-3].decl) )->addTypedef()->addType( (yyvsp[-3].decl) );
		}
#line 9576 "Parser/parser.cc"
    break;

  case 432:
#line 1951 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 9584 "Parser/parser.cc"
    break;

  case 433:
#line 1955 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 9592 "Parser/parser.cc"
    break;

  case 434:
#line 1962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 9598 "Parser/parser.cc"
    break;

  case 438:
#line 1972 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 9604 "Parser/parser.cc"
    break;

  case 439:
#line 1974 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ) ); }
#line 9610 "Parser/parser.cc"
    break;

  case 447:
#line 1997 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_type_specifier2 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 9621 "Parser/parser.cc"
    break;

  case 452:
#line 2019 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9627 "Parser/parser.cc"
    break;

  case 455:
#line 2031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9633 "Parser/parser.cc"
    break;

  case 458:
#line 2041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Const ); }
#line 9639 "Parser/parser.cc"
    break;

  case 459:
#line 2043 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Restrict ); }
#line 9645 "Parser/parser.cc"
    break;

  case 460:
#line 2045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Volatile ); }
#line 9651 "Parser/parser.cc"
    break;

  case 461:
#line 2047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Atomic ); }
#line 9657 "Parser/parser.cc"
    break;

  case 463:
#line 2053 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newForall( (yyvsp[-1].decl) ); }
#line 9663 "Parser/parser.cc"
    break;

  case 465:
#line 2059 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9669 "Parser/parser.cc"
    break;

  case 466:
#line 2061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9675 "Parser/parser.cc"
    break;

  case 468:
#line 2072 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9681 "Parser/parser.cc"
    break;

  case 469:
#line 2077 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Extern ); }
#line 9687 "Parser/parser.cc"
    break;

  case 470:
#line 2079 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Static ); }
#line 9693 "Parser/parser.cc"
    break;

  case 471:
#line 2081 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Auto ); }
#line 9699 "Parser/parser.cc"
    break;

  case 472:
#line 2083 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Register ); }
#line 9705 "Parser/parser.cc"
    break;

  case 473:
#line 2085 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Threadlocal ); }
#line 9711 "Parser/parser.cc"
    break;

  case 474:
#line 2088 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Inline ); }
#line 9717 "Parser/parser.cc"
    break;

  case 475:
#line 2090 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Fortran ); }
#line 9723 "Parser/parser.cc"
    break;

  case 476:
#line 2092 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Noreturn ); }
#line 9729 "Parser/parser.cc"
    break;

  case 477:
#line 2097 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 9735 "Parser/parser.cc"
    break;

  case 478:
#line 2099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Bool ); }
#line 9741 "Parser/parser.cc"
    break;

  case 479:
#line 2101 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Char ); }
#line 9747 "Parser/parser.cc"
    break;

  case 480:
#line 2103 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int ); }
#line 9753 "Parser/parser.cc"
    break;

  case 481:
#line 2105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 ); }
#line 9759 "Parser/parser.cc"
    break;

  case 482:
#line 2107 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 )->addType( DeclarationNode::newSignedNess( DeclarationNode::Unsigned ) ); }
#line 9765 "Parser/parser.cc"
    break;

  case 483:
#line 2109 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Float ); }
#line 9771 "Parser/parser.cc"
    break;

  case 484:
#line 2111 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Double ); }
#line 9777 "Parser/parser.cc"
    break;

  case 485:
#line 2113 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat80 ); }
#line 9783 "Parser/parser.cc"
    break;

  case 486:
#line 2115 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat128 ); }
#line 9789 "Parser/parser.cc"
    break;

  case 487:
#line 2117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat16 ); }
#line 9795 "Parser/parser.cc"
    break;

  case 488:
#line 2119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32 ); }
#line 9801 "Parser/parser.cc"
    break;

  case 489:
#line 2121 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32x ); }
#line 9807 "Parser/parser.cc"
    break;

  case 490:
#line 2123 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64 ); }
#line 9813 "Parser/parser.cc"
    break;

  case 491:
#line 2125 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64x ); }
#line 9819 "Parser/parser.cc"
    break;

  case 492:
#line 2127 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat128 ); }
#line 9825 "Parser/parser.cc"
    break;

  case 493:
#line 2129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9831 "Parser/parser.cc"
    break;

  case 494:
#line 2131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9837 "Parser/parser.cc"
    break;

  case 495:
#line 2133 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9843 "Parser/parser.cc"
    break;

  case 496:
#line 2135 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Complex ); }
#line 9849 "Parser/parser.cc"
    break;

  case 497:
#line 2137 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Imaginary ); }
#line 9855 "Parser/parser.cc"
    break;

  case 498:
#line 2139 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Signed ); }
#line 9861 "Parser/parser.cc"
    break;

  case 499:
#line 2141 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Unsigned ); }
#line 9867 "Parser/parser.cc"
    break;

  case 500:
#line 2143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Short ); }
#line 9873 "Parser/parser.cc"
    break;

  case 501:
#line 2145 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Long ); }
#line 9879 "Parser/parser.cc"
    break;

  case 502:
#line 2147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Valist ); }
#line 9885 "Parser/parser.cc"
    break;

  case 503:
#line 2149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::AutoType ); }
#line 9891 "Parser/parser.cc"
    break;

  case 505:
#line 2155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9897 "Parser/parser.cc"
    break;

  case 507:
#line 2161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVtableType( (yyvsp[-2].decl) ); }
#line 9903 "Parser/parser.cc"
    break;

  case 508:
#line 2167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9909 "Parser/parser.cc"
    break;

  case 509:
#line 2169 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9915 "Parser/parser.cc"
    break;

  case 511:
#line 2176 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9921 "Parser/parser.cc"
    break;

  case 512:
#line 2178 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9927 "Parser/parser.cc"
    break;

  case 513:
#line 2180 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9933 "Parser/parser.cc"
    break;

  case 514:
#line 2182 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 9939 "Parser/parser.cc"
    break;

  case 516:
#line 2189 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9945 "Parser/parser.cc"
    break;

  case 518:
#line 2195 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9951 "Parser/parser.cc"
    break;

  case 519:
#line 2197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9957 "Parser/parser.cc"
    break;

  case 520:
#line 2199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 9963 "Parser/parser.cc"
    break;

  case 521:
#line 2204 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 9969 "Parser/parser.cc"
    break;

  case 522:
#line 2206 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en) ); }
#line 9975 "Parser/parser.cc"
    break;

  case 523:
#line 2208 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 9981 "Parser/parser.cc"
    break;

  case 524:
#line 2210 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en), true ); }
#line 9987 "Parser/parser.cc"
    break;

  case 525:
#line 2212 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Zero ); }
#line 9993 "Parser/parser.cc"
    break;

  case 526:
#line 2214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::One ); }
#line 9999 "Parser/parser.cc"
    break;

  case 527:
#line 2219 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_declaration_specifier %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10010 "Parser/parser.cc"
    break;

  case 528:
#line 2226 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10016 "Parser/parser.cc"
    break;

  case 529:
#line 2228 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10022 "Parser/parser.cc"
    break;

  case 530:
#line 2230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10028 "Parser/parser.cc"
    break;

  case 531:
#line 2235 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_type_specifier %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10039 "Parser/parser.cc"
    break;

  case 532:
#line 2242 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 10045 "Parser/parser.cc"
    break;

  case 533:
#line 2244 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10051 "Parser/parser.cc"
    break;

  case 534:
#line 2246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 10060 "Parser/parser.cc"
    break;

  case 536:
#line 2255 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10066 "Parser/parser.cc"
    break;

  case 537:
#line 2257 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10072 "Parser/parser.cc"
    break;

  case 538:
#line 2259 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10078 "Parser/parser.cc"
    break;

  case 540:
#line 2265 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10084 "Parser/parser.cc"
    break;

  case 541:
#line 2267 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10090 "Parser/parser.cc"
    break;

  case 543:
#line 2273 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10096 "Parser/parser.cc"
    break;

  case 544:
#line 2275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10102 "Parser/parser.cc"
    break;

  case 545:
#line 2277 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10108 "Parser/parser.cc"
    break;

  case 547:
#line 2283 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10114 "Parser/parser.cc"
    break;

  case 548:
#line 2285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10120 "Parser/parser.cc"
    break;

  case 549:
#line 2290 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypedef( (yyvsp[0].tok) ); }
#line 10126 "Parser/parser.cc"
    break;

  case 550:
#line 2292 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 10132 "Parser/parser.cc"
    break;

  case 551:
#line 2294 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 10138 "Parser/parser.cc"
    break;

  case 553:
#line 2297 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), (yyvsp[0].decl) ); }
#line 10144 "Parser/parser.cc"
    break;

  case 554:
#line 2299 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), (yyvsp[0].decl) ); }
#line 10150 "Parser/parser.cc"
    break;

  case 555:
#line 2304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[0].tok), nullptr ); }
#line 10156 "Parser/parser.cc"
    break;

  case 556:
#line 2306 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-2].tok), nullptr ); }
#line 10162 "Parser/parser.cc"
    break;

  case 557:
#line 2308 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 10168 "Parser/parser.cc"
    break;

  case 558:
#line 2313 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "elaborated_type %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10179 "Parser/parser.cc"
    break;

  case 562:
#line 2329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 10185 "Parser/parser.cc"
    break;

  case 563:
#line 2331 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 10191 "Parser/parser.cc"
    break;

  case 564:
#line 2333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 10200 "Parser/parser.cc"
    break;

  case 565:
#line 2338 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 10217 "Parser/parser.cc"
    break;

  case 566:
#line 2351 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 10226 "Parser/parser.cc"
    break;

  case 567:
#line 2356 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "AGG3\n" );
			DeclarationNode::newFromTypedef( (yyvsp[-5].tok) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 10236 "Parser/parser.cc"
    break;

  case 568:
#line 2362 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 10245 "Parser/parser.cc"
    break;

  case 569:
#line 2367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "AGG4\n" );
			DeclarationNode::newFromTypeGen( (yyvsp[-5].tok), nullptr );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 10255 "Parser/parser.cc"
    break;

  case 571:
#line 2377 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 10261 "Parser/parser.cc"
    break;

  case 572:
#line 2379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 10267 "Parser/parser.cc"
    break;

  case 573:
#line 2384 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 10277 "Parser/parser.cc"
    break;

  case 574:
#line 2390 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 10292 "Parser/parser.cc"
    break;

  case 577:
#line 2409 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Struct; }
#line 10298 "Parser/parser.cc"
    break;

  case 578:
#line 2411 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Union; }
#line 10304 "Parser/parser.cc"
    break;

  case 579:
#line 2413 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Exception; }
#line 10310 "Parser/parser.cc"
    break;

  case 580:
#line 2419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 10316 "Parser/parser.cc"
    break;

  case 581:
#line 2421 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 10322 "Parser/parser.cc"
    break;

  case 582:
#line 2423 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Generator; }
#line 10328 "Parser/parser.cc"
    break;

  case 583:
#line 2425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor generator is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 10334 "Parser/parser.cc"
    break;

  case 584:
#line 2427 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Coroutine; }
#line 10340 "Parser/parser.cc"
    break;

  case 585:
#line 2429 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor coroutine is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 10346 "Parser/parser.cc"
    break;

  case 586:
#line 2431 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Thread; }
#line 10352 "Parser/parser.cc"
    break;

  case 587:
#line 2433 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor thread is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 10358 "Parser/parser.cc"
    break;

  case 588:
#line 2438 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10364 "Parser/parser.cc"
    break;

  case 589:
#line 2440 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 10370 "Parser/parser.cc"
    break;

  case 590:
#line 2445 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "type_specifier1 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			(yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) );
			// printf( "type_specifier2 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10383 "Parser/parser.cc"
    break;

  case 591:
#line 2454 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 10389 "Parser/parser.cc"
    break;

  case 592:
#line 2456 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-1].decl) ) {								// field declarator ?
				(yyvsp[-1].decl) = DeclarationNode::newName( nullptr );
			} // if
			(yyvsp[-1].decl)->inLine = true;
			(yyval.decl) = distAttr( (yyvsp[-2].decl), (yyvsp[-1].decl) );					// mark all fields in list
			distInl( (yyvsp[-1].decl) );
		}
#line 10402 "Parser/parser.cc"
    break;

  case 593:
#line 2465 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10408 "Parser/parser.cc"
    break;

  case 596:
#line 2469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 10414 "Parser/parser.cc"
    break;

  case 597:
#line 2471 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10420 "Parser/parser.cc"
    break;

  case 600:
#line 2478 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10426 "Parser/parser.cc"
    break;

  case 602:
#line 2481 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 10432 "Parser/parser.cc"
    break;

  case 603:
#line 2486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].en) ); }
#line 10438 "Parser/parser.cc"
    break;

  case 604:
#line 2489 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 10444 "Parser/parser.cc"
    break;

  case 605:
#line 2492 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 10450 "Parser/parser.cc"
    break;

  case 606:
#line 2497 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10456 "Parser/parser.cc"
    break;

  case 608:
#line 2500 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 10462 "Parser/parser.cc"
    break;

  case 610:
#line 2511 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 10468 "Parser/parser.cc"
    break;

  case 611:
#line 2513 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 10474 "Parser/parser.cc"
    break;

  case 613:
#line 2520 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 10480 "Parser/parser.cc"
    break;

  case 614:
#line 2525 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 10486 "Parser/parser.cc"
    break;

  case 616:
#line 2531 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 10492 "Parser/parser.cc"
    break;

  case 617:
#line 2536 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10498 "Parser/parser.cc"
    break;

  case 618:
#line 2538 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); }
#line 10504 "Parser/parser.cc"
    break;

  case 619:
#line 2540 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].tok), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) ); }
#line 10510 "Parser/parser.cc"
    break;

  case 620:
#line 2543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-4].decl)->name, (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 10516 "Parser/parser.cc"
    break;

  case 621:
#line 2545 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Unvalued enumerated type is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10522 "Parser/parser.cc"
    break;

  case 622:
#line 2547 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-6].decl)->storageClasses.val != 0 || (yyvsp[-6].decl)->type->qualifiers.val != 0 )
			{ SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }

			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 10533 "Parser/parser.cc"
    break;

  case 623:
#line 2554 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].decl)->storageClasses.val != 0 || (yyvsp[-4].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			typedefTable.makeTypedef( *(yyvsp[-1].tok) );
		}
#line 10542 "Parser/parser.cc"
    break;

  case 624:
#line 2559 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, (yyvsp[-9].decl) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 10550 "Parser/parser.cc"
    break;

  case 625:
#line 2563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-8].decl)->storageClasses.val != 0 || (yyvsp[-8].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			typedefTable.makeTypedef( *(yyvsp[-5].decl)->name );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].decl)->name, (yyvsp[-2].decl), true, (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 10560 "Parser/parser.cc"
    break;

  case 627:
#line 2573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); (yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), 0, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 10566 "Parser/parser.cc"
    break;

  case 628:
#line 2575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].decl)->type->symbolic.name );	(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].decl)->type->symbolic.name, 0, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 10572 "Parser/parser.cc"
    break;

  case 629:
#line 2580 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].in) ); }
#line 10578 "Parser/parser.cc"
    break;

  case 630:
#line 2582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( new string("inline"), nullptr ); }
#line 10584 "Parser/parser.cc"
    break;

  case 631:
#line 2584 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].in) ) ); }
#line 10590 "Parser/parser.cc"
    break;

  case 632:
#line 2586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( DeclarationNode::newEnumValueGeneric( new string("inline"), nullptr ) ); }
#line 10596 "Parser/parser.cc"
    break;

  case 633:
#line 2591 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 10602 "Parser/parser.cc"
    break;

  case 634:
#line 2592 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = new InitializerNode( (yyvsp[0].en) ); }
#line 10608 "Parser/parser.cc"
    break;

  case 635:
#line 2593 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                     { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 10614 "Parser/parser.cc"
    break;

  case 636:
#line 2600 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 10620 "Parser/parser.cc"
    break;

  case 637:
#line 2602 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10626 "Parser/parser.cc"
    break;

  case 640:
#line 2606 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10632 "Parser/parser.cc"
    break;

  case 641:
#line 2608 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 10638 "Parser/parser.cc"
    break;

  case 642:
#line 2610 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 10644 "Parser/parser.cc"
    break;

  case 644:
#line 2618 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10650 "Parser/parser.cc"
    break;

  case 645:
#line 2620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10656 "Parser/parser.cc"
    break;

  case 646:
#line 2622 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-8].decl)->appendList( (yyvsp[-4].decl) )->appendList( (yyvsp[0].decl) ); }
#line 10662 "Parser/parser.cc"
    break;

  case 648:
#line 2628 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10668 "Parser/parser.cc"
    break;

  case 649:
#line 2633 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10674 "Parser/parser.cc"
    break;

  case 650:
#line 2635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10680 "Parser/parser.cc"
    break;

  case 652:
#line 2638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 10686 "Parser/parser.cc"
    break;

  case 655:
#line 2645 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10692 "Parser/parser.cc"
    break;

  case 656:
#line 2647 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10698 "Parser/parser.cc"
    break;

  case 658:
#line 2656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 10704 "Parser/parser.cc"
    break;

  case 659:
#line 2659 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 10710 "Parser/parser.cc"
    break;

  case 660:
#line 2661 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 10716 "Parser/parser.cc"
    break;

  case 665:
#line 2671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10722 "Parser/parser.cc"
    break;

  case 667:
#line 2678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 10728 "Parser/parser.cc"
    break;

  case 668:
#line 2680 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 10734 "Parser/parser.cc"
    break;

  case 669:
#line 2685 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 10740 "Parser/parser.cc"
    break;

  case 670:
#line 2687 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 10746 "Parser/parser.cc"
    break;

  case 671:
#line 2696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 10752 "Parser/parser.cc"
    break;

  case 672:
#line 2698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 10758 "Parser/parser.cc"
    break;

  case 678:
#line 2711 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10764 "Parser/parser.cc"
    break;

  case 681:
#line 2721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 10770 "Parser/parser.cc"
    break;

  case 682:
#line 2722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (yyvsp[-1].op) == OperKinds::Assign ? (yyvsp[0].in) : (yyvsp[0].in)->set_maybeConstructed( false ); }
#line 10776 "Parser/parser.cc"
    break;

  case 683:
#line 2723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.in) = new InitializerNode( true ); }
#line 10782 "Parser/parser.cc"
    break;

  case 684:
#line 2724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 10788 "Parser/parser.cc"
    break;

  case 685:
#line 2728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = new InitializerNode( (yyvsp[0].en) ); }
#line 10794 "Parser/parser.cc"
    break;

  case 686:
#line 2729 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 10800 "Parser/parser.cc"
    break;

  case 687:
#line 2734 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 10806 "Parser/parser.cc"
    break;

  case 689:
#line 2736 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ); }
#line 10812 "Parser/parser.cc"
    break;

  case 690:
#line 2737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (InitializerNode *)( (yyvsp[-2].in)->set_last( (yyvsp[0].in) ) ); }
#line 10818 "Parser/parser.cc"
    break;

  case 691:
#line 2738 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.in) = (InitializerNode *)((yyvsp[-3].in)->set_last( (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ) )); }
#line 10824 "Parser/parser.cc"
    break;

  case 693:
#line 2754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[-1].tok) ) ); }
#line 10830 "Parser/parser.cc"
    break;

  case 695:
#line 2760 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-1].en)->set_last( (yyvsp[0].en) )); }
#line 10836 "Parser/parser.cc"
    break;

  case 696:
#line 2766 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 10842 "Parser/parser.cc"
    break;

  case 697:
#line 2769 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 10848 "Parser/parser.cc"
    break;

  case 698:
#line 2771 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 10854 "Parser/parser.cc"
    break;

  case 699:
#line 2773 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-4].en) ), maybeMoveBuild<Expression>( (yyvsp[-2].en) ) ) ); }
#line 10860 "Parser/parser.cc"
    break;

  case 700:
#line 2775 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 10866 "Parser/parser.cc"
    break;

  case 702:
#line 2799 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl) ); }
#line 10872 "Parser/parser.cc"
    break;

  case 703:
#line 2804 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10878 "Parser/parser.cc"
    break;

  case 704:
#line 2806 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 10884 "Parser/parser.cc"
    break;

  case 705:
#line 2811 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "9" );
			if ( (yyvsp[-1].tclass) == TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 10895 "Parser/parser.cc"
    break;

  case 706:
#line 2818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 10901 "Parser/parser.cc"
    break;

  case 707:
#line 2820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "9" ); }
#line 10907 "Parser/parser.cc"
    break;

  case 708:
#line 2822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 10913 "Parser/parser.cc"
    break;

  case 709:
#line 2824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "9" );
			(yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 10922 "Parser/parser.cc"
    break;

  case 710:
#line 2830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 10928 "Parser/parser.cc"
    break;

  case 711:
#line 2835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 10934 "Parser/parser.cc"
    break;

  case 712:
#line 2837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 10940 "Parser/parser.cc"
    break;

  case 713:
#line 2839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::DStype; }
#line 10946 "Parser/parser.cc"
    break;

  case 714:
#line 2843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 10952 "Parser/parser.cc"
    break;

  case 715:
#line 2848 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 10958 "Parser/parser.cc"
    break;

  case 716:
#line 2850 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 10964 "Parser/parser.cc"
    break;

  case 717:
#line 2852 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ftype; }
#line 10970 "Parser/parser.cc"
    break;

  case 718:
#line 2854 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 10976 "Parser/parser.cc"
    break;

  case 719:
#line 2859 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10982 "Parser/parser.cc"
    break;

  case 722:
#line 2866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 10988 "Parser/parser.cc"
    break;

  case 723:
#line 2871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 10994 "Parser/parser.cc"
    break;

  case 724:
#line 2873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 11000 "Parser/parser.cc"
    break;

  case 725:
#line 2880 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 11006 "Parser/parser.cc"
    break;

  case 727:
#line 2883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 11012 "Parser/parser.cc"
    break;

  case 728:
#line 2885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)( (yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 11018 "Parser/parser.cc"
    break;

  case 729:
#line 2890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 11024 "Parser/parser.cc"
    break;

  case 730:
#line 2892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11030 "Parser/parser.cc"
    break;

  case 731:
#line 2894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 11036 "Parser/parser.cc"
    break;

  case 732:
#line 2899 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 11042 "Parser/parser.cc"
    break;

  case 733:
#line 2901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 11048 "Parser/parser.cc"
    break;

  case 734:
#line 2906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "10" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), 0 );
		}
#line 11057 "Parser/parser.cc"
    break;

  case 735:
#line 2911 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "11" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 11066 "Parser/parser.cc"
    break;

  case 736:
#line 2919 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), 0 ); }
#line 11072 "Parser/parser.cc"
    break;

  case 737:
#line 2921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-8].tok), (yyvsp[-6].decl), (yyvsp[-2].decl) ); }
#line 11078 "Parser/parser.cc"
    break;

  case 739:
#line 2927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl) ); }
#line 11084 "Parser/parser.cc"
    break;

  case 744:
#line 2939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 11090 "Parser/parser.cc"
    break;

  case 745:
#line 2944 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 11096 "Parser/parser.cc"
    break;

  case 746:
#line 2946 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 11102 "Parser/parser.cc"
    break;

  case 748:
#line 2954 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl);	}
#line 11108 "Parser/parser.cc"
    break;

  case 749:
#line 2959 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11114 "Parser/parser.cc"
    break;

  case 750:
#line 2961 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->appendList( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 11120 "Parser/parser.cc"
    break;

  case 751:
#line 2966 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11126 "Parser/parser.cc"
    break;

  case 753:
#line 2971 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 11132 "Parser/parser.cc"
    break;

  case 754:
#line 2975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 11138 "Parser/parser.cc"
    break;

  case 755:
#line 2980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( (yyvsp[0].tok) ) ) ); }
#line 11144 "Parser/parser.cc"
    break;

  case 758:
#line 2984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 11153 "Parser/parser.cc"
    break;

  case 759:
#line 2989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( false, (yyvsp[-2].constant), 0 ) ) ); }
#line 11159 "Parser/parser.cc"
    break;

  case 760:
#line 2991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = LinkageSpec::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 11168 "Parser/parser.cc"
    break;

  case 761:
#line 2996 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-1].decl);
		}
#line 11178 "Parser/parser.cc"
    break;

  case 762:
#line 3002 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = LinkageSpec::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 11187 "Parser/parser.cc"
    break;

  case 763:
#line 3007 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11197 "Parser/parser.cc"
    break;

  case 764:
#line 3013 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 11206 "Parser/parser.cc"
    break;

  case 765:
#line 3018 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11216 "Parser/parser.cc"
    break;

  case 766:
#line 3024 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 11225 "Parser/parser.cc"
    break;

  case 767:
#line 3029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11235 "Parser/parser.cc"
    break;

  case 768:
#line 3035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.val) || (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 11244 "Parser/parser.cc"
    break;

  case 769:
#line 3040 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11254 "Parser/parser.cc"
    break;

  case 771:
#line 3055 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].sn) ); }
#line 11260 "Parser/parser.cc"
    break;

  case 772:
#line 3057 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].sn) ); }
#line 11266 "Parser/parser.cc"
    break;

  case 773:
#line 3062 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; forall = false; }
#line 11272 "Parser/parser.cc"
    break;

  case 774:
#line 3064 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.en) = (yyvsp[-2].en); forall = false;
			if ( (yyvsp[0].decl) ) {
				SemanticError( yylloc, "Attributes cannot be associated with function body. Move attribute(s) before \"with\" clause." );
				(yyval.en) = nullptr;
			} // if
		}
#line 11284 "Parser/parser.cc"
    break;

  case 775:
#line 3075 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11295 "Parser/parser.cc"
    break;

  case 776:
#line 3082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 11304 "Parser/parser.cc"
    break;

  case 777:
#line 3087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 11313 "Parser/parser.cc"
    break;

  case 778:
#line 3093 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 11319 "Parser/parser.cc"
    break;

  case 779:
#line 3096 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 11325 "Parser/parser.cc"
    break;

  case 780:
#line 3099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 11331 "Parser/parser.cc"
    break;

  case 781:
#line 3103 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-4].decl) );
		}
#line 11340 "Parser/parser.cc"
    break;

  case 782:
#line 3109 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 11346 "Parser/parser.cc"
    break;

  case 783:
#line 3112 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 11352 "Parser/parser.cc"
    break;

  case 784:
#line 3115 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 11358 "Parser/parser.cc"
    break;

  case 788:
#line 3126 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 11364 "Parser/parser.cc"
    break;

  case 789:
#line 3131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11370 "Parser/parser.cc"
    break;

  case 790:
#line 3133 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = (yyvsp[-2].constant);
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 11380 "Parser/parser.cc"
    break;

  case 791:
#line 3142 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11386 "Parser/parser.cc"
    break;

  case 794:
#line 3149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11392 "Parser/parser.cc"
    break;

  case 795:
#line 3154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 11398 "Parser/parser.cc"
    break;

  case 797:
#line 3160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11404 "Parser/parser.cc"
    break;

  case 798:
#line 3165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11410 "Parser/parser.cc"
    break;

  case 799:
#line 3167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 11416 "Parser/parser.cc"
    break;

  case 800:
#line 3169 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 11422 "Parser/parser.cc"
    break;

  case 805:
#line 3178 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 11428 "Parser/parser.cc"
    break;

  case 806:
#line 3180 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 11434 "Parser/parser.cc"
    break;

  case 807:
#line 3215 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11440 "Parser/parser.cc"
    break;

  case 808:
#line 3217 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11446 "Parser/parser.cc"
    break;

  case 809:
#line 3222 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11452 "Parser/parser.cc"
    break;

  case 811:
#line 3225 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11458 "Parser/parser.cc"
    break;

  case 812:
#line 3227 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11464 "Parser/parser.cc"
    break;

  case 813:
#line 3232 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11470 "Parser/parser.cc"
    break;

  case 814:
#line 3234 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11476 "Parser/parser.cc"
    break;

  case 815:
#line 3236 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11482 "Parser/parser.cc"
    break;

  case 816:
#line 3238 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11488 "Parser/parser.cc"
    break;

  case 817:
#line 3243 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11494 "Parser/parser.cc"
    break;

  case 818:
#line 3245 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11500 "Parser/parser.cc"
    break;

  case 819:
#line 3247 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11506 "Parser/parser.cc"
    break;

  case 820:
#line 3249 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11512 "Parser/parser.cc"
    break;

  case 821:
#line 3251 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11518 "Parser/parser.cc"
    break;

  case 822:
#line 3253 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11524 "Parser/parser.cc"
    break;

  case 823:
#line 3255 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11530 "Parser/parser.cc"
    break;

  case 824:
#line 3260 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11536 "Parser/parser.cc"
    break;

  case 825:
#line 3262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 11542 "Parser/parser.cc"
    break;

  case 826:
#line 3264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11548 "Parser/parser.cc"
    break;

  case 827:
#line 3266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11554 "Parser/parser.cc"
    break;

  case 828:
#line 3275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11560 "Parser/parser.cc"
    break;

  case 830:
#line 3278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11566 "Parser/parser.cc"
    break;

  case 831:
#line 3283 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11572 "Parser/parser.cc"
    break;

  case 832:
#line 3285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11578 "Parser/parser.cc"
    break;

  case 833:
#line 3287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 11584 "Parser/parser.cc"
    break;

  case 834:
#line 3289 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11590 "Parser/parser.cc"
    break;

  case 835:
#line 3291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11596 "Parser/parser.cc"
    break;

  case 836:
#line 3296 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11602 "Parser/parser.cc"
    break;

  case 837:
#line 3298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11608 "Parser/parser.cc"
    break;

  case 838:
#line 3300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11614 "Parser/parser.cc"
    break;

  case 839:
#line 3302 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11620 "Parser/parser.cc"
    break;

  case 840:
#line 3307 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11626 "Parser/parser.cc"
    break;

  case 841:
#line 3309 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11632 "Parser/parser.cc"
    break;

  case 842:
#line 3311 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11638 "Parser/parser.cc"
    break;

  case 843:
#line 3313 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11644 "Parser/parser.cc"
    break;

  case 844:
#line 3315 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11650 "Parser/parser.cc"
    break;

  case 845:
#line 3317 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11656 "Parser/parser.cc"
    break;

  case 849:
#line 3335 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 11662 "Parser/parser.cc"
    break;

  case 850:
#line 3337 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11668 "Parser/parser.cc"
    break;

  case 851:
#line 3339 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 11674 "Parser/parser.cc"
    break;

  case 852:
#line 3341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11680 "Parser/parser.cc"
    break;

  case 853:
#line 3343 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11686 "Parser/parser.cc"
    break;

  case 854:
#line 3348 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11692 "Parser/parser.cc"
    break;

  case 855:
#line 3350 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11698 "Parser/parser.cc"
    break;

  case 856:
#line 3352 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11704 "Parser/parser.cc"
    break;

  case 857:
#line 3354 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11710 "Parser/parser.cc"
    break;

  case 858:
#line 3359 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11716 "Parser/parser.cc"
    break;

  case 859:
#line 3361 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11722 "Parser/parser.cc"
    break;

  case 860:
#line 3363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11728 "Parser/parser.cc"
    break;

  case 861:
#line 3365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11734 "Parser/parser.cc"
    break;

  case 862:
#line 3367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11740 "Parser/parser.cc"
    break;

  case 863:
#line 3369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11746 "Parser/parser.cc"
    break;

  case 864:
#line 3384 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "ID" );
		}
#line 11755 "Parser/parser.cc"
    break;

  case 865:
#line 3389 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11761 "Parser/parser.cc"
    break;

  case 866:
#line 3394 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11767 "Parser/parser.cc"
    break;

  case 868:
#line 3397 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11773 "Parser/parser.cc"
    break;

  case 869:
#line 3399 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11779 "Parser/parser.cc"
    break;

  case 870:
#line 3404 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11785 "Parser/parser.cc"
    break;

  case 871:
#line 3406 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11791 "Parser/parser.cc"
    break;

  case 872:
#line 3408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11797 "Parser/parser.cc"
    break;

  case 873:
#line 3410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11803 "Parser/parser.cc"
    break;

  case 874:
#line 3415 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11809 "Parser/parser.cc"
    break;

  case 875:
#line 3417 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11815 "Parser/parser.cc"
    break;

  case 876:
#line 3419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11821 "Parser/parser.cc"
    break;

  case 877:
#line 3421 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11827 "Parser/parser.cc"
    break;

  case 878:
#line 3423 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11833 "Parser/parser.cc"
    break;

  case 879:
#line 3425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11839 "Parser/parser.cc"
    break;

  case 880:
#line 3427 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11845 "Parser/parser.cc"
    break;

  case 881:
#line 3432 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11851 "Parser/parser.cc"
    break;

  case 882:
#line 3434 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11857 "Parser/parser.cc"
    break;

  case 883:
#line 3436 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 11863 "Parser/parser.cc"
    break;

  case 884:
#line 3438 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11869 "Parser/parser.cc"
    break;

  case 885:
#line 3440 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11875 "Parser/parser.cc"
    break;

  case 886:
#line 3450 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11881 "Parser/parser.cc"
    break;

  case 887:
#line 3452 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11887 "Parser/parser.cc"
    break;

  case 889:
#line 3455 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11893 "Parser/parser.cc"
    break;

  case 890:
#line 3457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11899 "Parser/parser.cc"
    break;

  case 891:
#line 3462 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11905 "Parser/parser.cc"
    break;

  case 892:
#line 3464 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11911 "Parser/parser.cc"
    break;

  case 893:
#line 3466 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11917 "Parser/parser.cc"
    break;

  case 894:
#line 3471 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11923 "Parser/parser.cc"
    break;

  case 895:
#line 3473 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11929 "Parser/parser.cc"
    break;

  case 896:
#line 3475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11935 "Parser/parser.cc"
    break;

  case 897:
#line 3477 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11941 "Parser/parser.cc"
    break;

  case 898:
#line 3482 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11947 "Parser/parser.cc"
    break;

  case 899:
#line 3484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11953 "Parser/parser.cc"
    break;

  case 900:
#line 3486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11959 "Parser/parser.cc"
    break;

  case 901:
#line 3500 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11965 "Parser/parser.cc"
    break;

  case 902:
#line 3502 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11971 "Parser/parser.cc"
    break;

  case 904:
#line 3505 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11977 "Parser/parser.cc"
    break;

  case 905:
#line 3507 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11983 "Parser/parser.cc"
    break;

  case 906:
#line 3512 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11989 "Parser/parser.cc"
    break;

  case 907:
#line 3514 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11995 "Parser/parser.cc"
    break;

  case 908:
#line 3519 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12001 "Parser/parser.cc"
    break;

  case 909:
#line 3521 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12007 "Parser/parser.cc"
    break;

  case 910:
#line 3523 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12013 "Parser/parser.cc"
    break;

  case 911:
#line 3528 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12019 "Parser/parser.cc"
    break;

  case 912:
#line 3530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12025 "Parser/parser.cc"
    break;

  case 913:
#line 3535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12031 "Parser/parser.cc"
    break;

  case 914:
#line 3537 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12037 "Parser/parser.cc"
    break;

  case 916:
#line 3555 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12043 "Parser/parser.cc"
    break;

  case 917:
#line 3557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12049 "Parser/parser.cc"
    break;

  case 918:
#line 3562 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( 0, (yyvsp[0].op) ); }
#line 12055 "Parser/parser.cc"
    break;

  case 919:
#line 3564 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 12061 "Parser/parser.cc"
    break;

  case 920:
#line 3566 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12067 "Parser/parser.cc"
    break;

  case 921:
#line 3568 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12073 "Parser/parser.cc"
    break;

  case 922:
#line 3570 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12079 "Parser/parser.cc"
    break;

  case 924:
#line 3576 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12085 "Parser/parser.cc"
    break;

  case 925:
#line 3578 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12091 "Parser/parser.cc"
    break;

  case 926:
#line 3580 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12097 "Parser/parser.cc"
    break;

  case 927:
#line 3585 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 12103 "Parser/parser.cc"
    break;

  case 928:
#line 3587 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12109 "Parser/parser.cc"
    break;

  case 929:
#line 3589 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12115 "Parser/parser.cc"
    break;

  case 930:
#line 3595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false ); }
#line 12121 "Parser/parser.cc"
    break;

  case 931:
#line 3597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false )->addArray( (yyvsp[0].decl) ); }
#line 12127 "Parser/parser.cc"
    break;

  case 932:
#line 3599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-4].en), 0, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].en), 0, false ) ); }
#line 12133 "Parser/parser.cc"
    break;

  case 934:
#line 3606 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), 0, false ); }
#line 12139 "Parser/parser.cc"
    break;

  case 935:
#line 3608 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 12145 "Parser/parser.cc"
    break;

  case 936:
#line 3610 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newArray( (yyvsp[-2].en), 0, false ) ); }
#line 12151 "Parser/parser.cc"
    break;

  case 937:
#line 3612 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 12157 "Parser/parser.cc"
    break;

  case 939:
#line 3647 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 12163 "Parser/parser.cc"
    break;

  case 940:
#line 3649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12169 "Parser/parser.cc"
    break;

  case 941:
#line 3651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12175 "Parser/parser.cc"
    break;

  case 942:
#line 3656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].op) ); }
#line 12181 "Parser/parser.cc"
    break;

  case 943:
#line 3658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 12187 "Parser/parser.cc"
    break;

  case 944:
#line 3660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12193 "Parser/parser.cc"
    break;

  case 945:
#line 3662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12199 "Parser/parser.cc"
    break;

  case 946:
#line 3664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12205 "Parser/parser.cc"
    break;

  case 948:
#line 3670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12211 "Parser/parser.cc"
    break;

  case 949:
#line 3672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12217 "Parser/parser.cc"
    break;

  case 950:
#line 3674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12223 "Parser/parser.cc"
    break;

  case 951:
#line 3679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 12229 "Parser/parser.cc"
    break;

  case 952:
#line 3681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12235 "Parser/parser.cc"
    break;

  case 953:
#line 3683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12241 "Parser/parser.cc"
    break;

  case 955:
#line 3690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12247 "Parser/parser.cc"
    break;

  case 957:
#line 3701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false ); }
#line 12253 "Parser/parser.cc"
    break;

  case 958:
#line 3704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 12259 "Parser/parser.cc"
    break;

  case 959:
#line 3706 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, (yyvsp[-2].decl), false ); }
#line 12265 "Parser/parser.cc"
    break;

  case 960:
#line 3709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 12271 "Parser/parser.cc"
    break;

  case 961:
#line 3711 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 12277 "Parser/parser.cc"
    break;

  case 962:
#line 3713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-4].decl), true ); }
#line 12283 "Parser/parser.cc"
    break;

  case 964:
#line 3728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12289 "Parser/parser.cc"
    break;

  case 965:
#line 3730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12295 "Parser/parser.cc"
    break;

  case 966:
#line 3735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( 0, (yyvsp[0].op) ); }
#line 12301 "Parser/parser.cc"
    break;

  case 967:
#line 3737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 12307 "Parser/parser.cc"
    break;

  case 968:
#line 3739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12313 "Parser/parser.cc"
    break;

  case 969:
#line 3741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12319 "Parser/parser.cc"
    break;

  case 970:
#line 3743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12325 "Parser/parser.cc"
    break;

  case 972:
#line 3749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12331 "Parser/parser.cc"
    break;

  case 973:
#line 3751 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12337 "Parser/parser.cc"
    break;

  case 974:
#line 3753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12343 "Parser/parser.cc"
    break;

  case 975:
#line 3758 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12349 "Parser/parser.cc"
    break;

  case 976:
#line 3760 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12355 "Parser/parser.cc"
    break;

  case 979:
#line 3770 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12361 "Parser/parser.cc"
    break;

  case 982:
#line 3781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12367 "Parser/parser.cc"
    break;

  case 983:
#line 3783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12373 "Parser/parser.cc"
    break;

  case 984:
#line 3785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12379 "Parser/parser.cc"
    break;

  case 985:
#line 3787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12385 "Parser/parser.cc"
    break;

  case 986:
#line 3789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12391 "Parser/parser.cc"
    break;

  case 987:
#line 3791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12397 "Parser/parser.cc"
    break;

  case 988:
#line 3798 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 12403 "Parser/parser.cc"
    break;

  case 989:
#line 3800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12409 "Parser/parser.cc"
    break;

  case 990:
#line 3802 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 12415 "Parser/parser.cc"
    break;

  case 991:
#line 3804 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 12421 "Parser/parser.cc"
    break;

  case 992:
#line 3806 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12427 "Parser/parser.cc"
    break;

  case 993:
#line 3809 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 12433 "Parser/parser.cc"
    break;

  case 994:
#line 3811 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12439 "Parser/parser.cc"
    break;

  case 995:
#line 3813 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 12445 "Parser/parser.cc"
    break;

  case 996:
#line 3815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 12451 "Parser/parser.cc"
    break;

  case 997:
#line 3817 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12457 "Parser/parser.cc"
    break;

  case 998:
#line 3822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 12463 "Parser/parser.cc"
    break;

  case 999:
#line 3824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 12469 "Parser/parser.cc"
    break;

  case 1000:
#line 3829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 12475 "Parser/parser.cc"
    break;

  case 1001:
#line 3831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl)->addQualifiers( (yyvsp[-4].decl) ), true ); }
#line 12481 "Parser/parser.cc"
    break;

  case 1003:
#line 3858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12487 "Parser/parser.cc"
    break;

  case 1007:
#line 3869 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12493 "Parser/parser.cc"
    break;

  case 1008:
#line 3871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12499 "Parser/parser.cc"
    break;

  case 1009:
#line 3873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12505 "Parser/parser.cc"
    break;

  case 1010:
#line 3875 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12511 "Parser/parser.cc"
    break;

  case 1011:
#line 3877 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12517 "Parser/parser.cc"
    break;

  case 1012:
#line 3879 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12523 "Parser/parser.cc"
    break;

  case 1013:
#line 3886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12529 "Parser/parser.cc"
    break;

  case 1014:
#line 3888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12535 "Parser/parser.cc"
    break;

  case 1015:
#line 3890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12541 "Parser/parser.cc"
    break;

  case 1016:
#line 3892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12547 "Parser/parser.cc"
    break;

  case 1017:
#line 3894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12553 "Parser/parser.cc"
    break;

  case 1018:
#line 3896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12559 "Parser/parser.cc"
    break;

  case 1019:
#line 3901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 12565 "Parser/parser.cc"
    break;

  case 1020:
#line 3903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 12571 "Parser/parser.cc"
    break;

  case 1021:
#line 3905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 12577 "Parser/parser.cc"
    break;

  case 1022:
#line 3912 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 12583 "Parser/parser.cc"
    break;

  case 1023:
#line 3914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 12589 "Parser/parser.cc"
    break;

  case 1026:
#line 3938 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 12595 "Parser/parser.cc"
    break;

  case 1027:
#line 3940 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 12601 "Parser/parser.cc"
    break;


#line 12605 "Parser/parser.cc"

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
#line 3943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "make install" //
// End: //
