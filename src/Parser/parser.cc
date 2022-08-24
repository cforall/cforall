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
#define YYLAST   20968

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  175
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  291
/* YYNRULES -- Number of rules.  */
#define YYNRULES  1025
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  2072

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
    1334,  1339,  1344,  1346,  1351,  1356,  1358,  1360,  1362,  1364,
    1367,  1369,  1372,  1374,  1379,  1385,  1388,  1390,  1395,  1401,
    1403,  1408,  1414,  1417,  1419,  1422,  1424,  1429,  1436,  1438,
    1443,  1449,  1451,  1456,  1462,  1465,  1470,  1478,  1480,  1482,
    1487,  1489,  1494,  1495,  1497,  1502,  1504,  1509,  1511,  1513,
    1515,  1518,  1522,  1525,  1529,  1531,  1533,  1535,  1537,  1539,
    1541,  1543,  1545,  1547,  1549,  1554,  1555,  1559,  1565,  1570,
    1575,  1576,  1580,  1584,  1589,  1590,  1596,  1600,  1602,  1604,
    1606,  1609,  1611,  1616,  1618,  1623,  1625,  1627,  1632,  1634,
    1640,  1641,  1645,  1646,  1647,  1648,  1652,  1657,  1658,  1660,
    1662,  1664,  1668,  1672,  1673,  1677,  1679,  1681,  1683,  1685,
    1691,  1692,  1698,  1699,  1703,  1704,  1709,  1711,  1717,  1718,
    1720,  1725,  1730,  1741,  1742,  1746,  1747,  1753,  1754,  1758,
    1760,  1764,  1766,  1770,  1771,  1775,  1776,  1780,  1787,  1788,
    1792,  1794,  1809,  1810,  1811,  1812,  1814,  1818,  1820,  1824,
    1831,  1833,  1835,  1840,  1841,  1843,  1845,  1847,  1879,  1882,
    1887,  1889,  1895,  1900,  1905,  1916,  1921,  1926,  1931,  1936,
    1945,  1949,  1956,  1958,  1959,  1960,  1966,  1968,  1973,  1974,
    1975,  1984,  1985,  1986,  1990,  1991,  1998,  2007,  2008,  2009,
    2014,  2015,  2024,  2025,  2030,  2031,  2035,  2037,  2039,  2041,
    2043,  2047,  2052,  2053,  2055,  2065,  2066,  2071,  2073,  2075,
    2077,  2079,  2082,  2084,  2086,  2091,  2093,  2095,  2097,  2099,
    2101,  2103,  2105,  2107,  2109,  2111,  2113,  2115,  2117,  2119,
    2121,  2123,  2125,  2127,  2129,  2131,  2133,  2135,  2137,  2139,
    2141,  2143,  2145,  2150,  2151,  2155,  2162,  2163,  2169,  2170,
    2172,  2174,  2176,  2181,  2183,  2188,  2189,  2191,  2193,  2198,
    2200,  2202,  2204,  2206,  2208,  2213,  2220,  2222,  2224,  2229,
    2237,  2236,  2240,  2248,  2249,  2251,  2253,  2258,  2259,  2261,
    2266,  2267,  2269,  2271,  2276,  2277,  2279,  2284,  2286,  2288,
    2290,  2291,  2293,  2298,  2300,  2302,  2307,  2314,  2318,  2319,
    2324,  2323,  2328,  2327,  2346,  2345,  2357,  2356,  2367,  2372,
    2373,  2378,  2384,  2398,  2399,  2403,  2405,  2407,  2413,  2415,
    2417,  2419,  2421,  2423,  2425,  2427,  2433,  2434,  2439,  2448,
    2450,  2459,  2461,  2462,  2463,  2465,  2467,  2468,  2473,  2474,
    2475,  2480,  2482,  2485,  2492,  2493,  2494,  2500,  2505,  2507,
    2513,  2514,  2520,  2521,  2525,  2530,  2533,  2532,  2536,  2539,
    2541,  2549,  2548,  2557,  2563,  2567,  2569,  2574,  2576,  2578,
    2580,  2586,  2587,  2588,  2595,  2596,  2598,  2599,  2600,  2602,
    2604,  2611,  2612,  2614,  2616,  2621,  2622,  2628,  2629,  2631,
    2632,  2637,  2638,  2639,  2641,  2649,  2650,  2652,  2655,  2657,
    2661,  2662,  2663,  2665,  2667,  2672,  2674,  2679,  2681,  2690,
    2692,  2697,  2698,  2699,  2703,  2704,  2705,  2710,  2711,  2716,
    2717,  2718,  2719,  2723,  2724,  2729,  2730,  2731,  2732,  2733,
    2747,  2748,  2753,  2754,  2760,  2762,  2765,  2767,  2769,  2792,
    2793,  2799,  2800,  2806,  2805,  2815,  2814,  2818,  2824,  2830,
    2831,  2833,  2837,  2842,  2844,  2846,  2848,  2854,  2855,  2859,
    2860,  2865,  2867,  2874,  2876,  2877,  2879,  2884,  2886,  2888,
    2893,  2895,  2900,  2905,  2913,  2915,  2920,  2921,  2926,  2927,
    2931,  2932,  2933,  2938,  2940,  2946,  2948,  2953,  2955,  2961,
    2962,  2966,  2970,  2974,  2976,  2977,  2978,  2983,  2986,  2985,
    2997,  2996,  3008,  3007,  3019,  3018,  3030,  3029,  3043,  3049,
    3051,  3057,  3058,  3069,  3076,  3081,  3087,  3090,  3093,  3097,
    3103,  3106,  3109,  3114,  3115,  3116,  3120,  3126,  3127,  3137,
    3138,  3142,  3143,  3148,  3153,  3154,  3160,  3161,  3163,  3168,
    3169,  3170,  3171,  3172,  3174,  3209,  3211,  3216,  3218,  3219,
    3221,  3226,  3228,  3230,  3232,  3237,  3239,  3241,  3243,  3245,
    3247,  3249,  3254,  3256,  3258,  3260,  3269,  3271,  3272,  3277,
    3279,  3281,  3283,  3285,  3290,  3292,  3294,  3296,  3301,  3303,
    3305,  3307,  3309,  3311,  3323,  3324,  3325,  3329,  3331,  3333,
    3335,  3337,  3342,  3344,  3346,  3348,  3353,  3355,  3357,  3359,
    3361,  3363,  3378,  3383,  3388,  3390,  3391,  3393,  3398,  3400,
    3402,  3404,  3409,  3411,  3413,  3415,  3417,  3419,  3421,  3426,
    3428,  3430,  3432,  3434,  3444,  3446,  3448,  3449,  3451,  3456,
    3458,  3460,  3465,  3467,  3469,  3471,  3476,  3478,  3480,  3494,
    3496,  3498,  3499,  3501,  3506,  3508,  3513,  3515,  3517,  3522,
    3524,  3529,  3531,  3548,  3549,  3551,  3556,  3558,  3560,  3562,
    3564,  3569,  3570,  3572,  3574,  3579,  3581,  3583,  3589,  3591,
    3593,  3596,  3600,  3602,  3604,  3606,  3640,  3641,  3643,  3645,
    3650,  3652,  3654,  3656,  3658,  3663,  3664,  3666,  3668,  3673,
    3675,  3677,  3683,  3684,  3686,  3695,  3698,  3700,  3703,  3705,
    3707,  3721,  3722,  3724,  3729,  3731,  3733,  3735,  3737,  3742,
    3743,  3745,  3747,  3752,  3754,  3762,  3763,  3764,  3769,  3770,
    3775,  3777,  3779,  3781,  3783,  3785,  3792,  3794,  3796,  3798,
    3800,  3803,  3805,  3807,  3809,  3811,  3816,  3818,  3820,  3825,
    3851,  3852,  3854,  3858,  3859,  3863,  3865,  3867,  3869,  3871,
    3873,  3880,  3882,  3884,  3886,  3888,  3890,  3895,  3897,  3899,
    3906,  3908,  3926,  3928,  3933,  3934
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

#define YYPACT_NINF (-1729)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-906)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
      74, 11513,   108,   189, 16093,   102, -1729, -1729, -1729, -1729,
   -1729, -1729, -1729, -1729, -1729, -1729, -1729,    77,   793,   194,
   -1729, -1729, -1729, -1729, -1729, -1729, -1729, -1729, -1729, -1729,
   -1729, -1729, -1729, -1729, -1729, -1729, -1729, -1729, -1729, -1729,
   -1729, -1729, -1729, -1729, -1729, -1729, -1729,    41,   453, -1729,
   -1729, -1729, -1729, -1729, -1729,  4620,  4620,   291, 11513,   339,
     352, -1729, -1729,   384, -1729, -1729, -1729, -1729, -1729, -1729,
   -1729, -1729, -1729,  3404, -1729,   460,   478, -1729, -1729, -1729,
   -1729, -1729, 15943, -1729, -1729,   500,   495,   289,   413, -1729,
    4620,   495,   495,   495,   509,  4200,   692,   787, 11672, -1729,
   -1729, -1729, 15793,  1667, -1729, -1729, -1729,  1831,   707, 11724,
     997,   757,  1831,  1028,   597, -1729, -1729, -1729, -1729,   685,
   -1729, -1729, -1729, -1729,   612, -1729, -1729, -1729, -1729, -1729,
     627,   634,   685, -1729,   685,   659, -1729, -1729, -1729, 16649,
    4620, -1729, -1729,  4620, -1729, 11513,   610, 16701, -1729, -1729,
    4300, 17713, -1729,  1098,  1098,   674,  2919, -1729, -1729, -1729,
   -1729,   420, 13929,  3174,   685, -1729, -1729, -1729, -1729, -1729,
   -1729,   698, -1729,   704,   708,   753, -1729,   795, 20371, 15023,
    2449,  3404,   648,   781,   794,   801,   812,   814,   835, -1729,
   -1729, 16851, 10633,   789, -1729, 16236, -1729, -1729, -1729, -1729,
     798, -1729, -1729,   834, -1729,  8836,   970, 18643, -1729,   848,
    4620,   634,   872,   883,   900,   916, -1729, -1729, -1729,  3238,
    3069,   931,   993,   331, -1729, -1729,   685,   685,    25,    99,
     340,    25, -1729,   685,   685, -1729,  3509, -1729, -1729,   945,
     947,  1098, 13823, -1729, -1729, 15943, -1729, -1729,  1831, -1729,
    1972,   597,   942,  1015,    99,  4620,   289, -1729, 13221, -1729,
    1098,  1098,   967,  1015,    99,  4620, -1729, 13113, -1729, -1729,
    1098, -1729,  1098, -1729,   503,  4380,  4620, -1729,  1581,   959,
   -1729, -1729, -1729, 16395,   634,   149, -1729, -1729, 17763, -1729,
     993,    96, -1729, 20371, 17713,  3687,  3509, -1729,   452, -1729,
   -1729, -1729, 16701,  4620, -1729,   996, -1729, -1729, -1729, -1729,
    4620,  3451,   388,   439, -1729,  4620,   704, -1729,   589,   685,
     685,   965, 16903,   657, 14403, 13981,  1831,  1831, -1729,  1831,
    1098,  1831,  1098, -1729, -1729,   685, -1729,  1019, -1729, 17053,
   -1729, -1729, -1729, 17105,   798, -1729,  1023,   303,   958,  1043,
     597,  1049, -1729,  2919,  1039,   704,  2919,  2484, -1729,  1058,
    1099, 20443,  1068,  1075, 20371, 20515,  1105, 12970, -1729, -1729,
   -1729, -1729, -1729, -1729, 20587, 20587, 14869,  1110,  3916, -1729,
   -1729, -1729, -1729,   -80, -1729,   629, -1729,  2126, -1729, 20371,
   20371, -1729,  1101,   628,   646,   796,   488,   830,  1112,  1116,
    1109,  1158,   113, -1729,   688, -1729,  1142, -1729,   845,  4608,
   15331, -1729, -1729,   769,  1142, -1729, -1729,   732, -1729, -1729,
    2449,  1162,  1164,  1172,  1177,  1180,  1183, -1729, -1729,   468,
    1185, -1729,   755,  1185, -1729, -1729, 16649, -1729,   906,  1186,
   15485, -1729, -1729,  4503,  4016,  1212, 14403,  1222,   854,   864,
   -1729, -1729, -1729, -1729, -1729,  4620,  4528, -1729, -1729, -1729,
   -1729, -1729, -1729,  8315,  4358,  1110,  8836,  1199,  1201, -1729,
   -1729,  1224, 18643,   744, -1729, -1729, -1729, 18715,  1241, -1729,
   -1729, -1729, -1729, -1729,  3238,   575,  1242,  1245,  1261,   712,
    1265,  1272,  1282,  3069, -1729, -1729,   685,  1295,   289,  1292,
   -1729, -1729,  1294, -1729, -1729,   634,  1015, -1729, -1729, -1729,
     634, -1729, -1729,  3509, -1729, 15331, 15331, -1729,  1098,  4300,
   18491, 14561, -1729, -1729, -1729, -1729, -1729,   634,  1015,    96,
   -1729, -1729,  1831,  1296,  1015,    99, -1729,   634,  1015, -1729,
   14769, -1729,  1098,  1098, -1729, -1729,  1297,   410,  1298,   597,
    1299, -1729, 17921, -1729,   758, -1729,  1381, 18387, -1729,  4300,
   17264, 13823, -1729, 16395, 20659, -1729, -1729, -1729, -1729, -1729,
    3687,   741,  3509, -1729, 14561,   993, 11513, -1729,  1304, -1729,
    1310, -1729, -1729, -1729, -1729, -1729,  2919, -1729, -1729,  1385,
    4432,  1306, 17105, 10633, -1729, 17316, -1729,  1098,  1098, -1729,
   -1729,   798, -1729,   788,  1313,  1447, 20371,  1216,  1294,  1301,
   -1729,   685,   685, -1729,  1185, -1729, 16903, -1729, -1729, 18202,
    1098,  1098, -1729,  4432,   685, -1729, 17570, -1729, -1729, 17053,
   -1729,   420,  1314,  1302,  1311,   958,   776, 16701,   784, -1729,
   -1729, -1729, -1729, -1729, -1729,   843, -1729,  1327,  1303, -1729,
   15177, -1729, 17368, 17368, -1729, 15177, -1729, 20371, -1729, 11724,
   11724, 15177, -1729, -1729, 16447, 17368, 17368,   845,  1225,  1305,
     550,  1386, -1729,   863,  1328,   918,  1329, -1729, 18715, 20371,
   18787,  1324, 20371,  1581, 20371,  1581, -1729,  2035, -1729, -1729,
   18859,  2073, 20371, 18859,  1581, -1729, -1729, 20371, 20371, 20371,
   20371, 20371, 20371, 20371, 20371, 20371, 20371, 20371, 20371, 20371,
   20371, 20371, 20371, 20371, 20371, 20371, 18931,  1308,   795,  3340,
   10633, -1729, -1729, -1729, -1729, -1729, -1729, -1729, -1729, -1729,
   -1729, -1729,  1326, 20371, -1729, -1729,   769,  1906, -1729, -1729,
     685,   685, -1729, -1729, 15331, -1729,   474,  1185, -1729,   879,
    1185, -1729, -1729, -1729,  1294, -1729, -1729,  1294, 20731, -1729,
   -1729, 10633,  1332,  1333,  2967,  1471,  2607,   476,  1301, -1729,
     685,   685,  1301,   487, -1729,   685,   685, 20371,  4620,   950,
     962,  1301,   256, 13771, 13771,  4620, -1729, -1729, 20371,  1224,
   -1729,  8836,  1342, -1729,  2110, -1729, -1729, -1729, -1729, -1729,
     882, -1729, 13771,  1581,  4300,  1581,   896,  1341,  1343,  1344,
     899,  1346,  1348,  1356,   492,  1185, -1729, -1729,   517,  1185,
   -1729, -1729, -1729,  4300,   795, -1729,  1185, 20731, -1729,   634,
   17921, -1729, -1729,   926,  1358,   936,  1359, -1729,  1345, -1729,
     634, -1729, -1729,   634,  1015,  1345, -1729,   634,  1360,  1362,
    1363, -1729, -1729, 18202, -1729,  1364, -1729, -1729, -1729,  1581,
    4620,  9792,  1455,  1353, 18289, -1729,  1186, -1729, 13771,   901,
   -1729, -1729,  1345, -1729, 16701, 15331,  1355, -1729,  1355, -1729,
   -1729, -1729,   958, -1729, 17053, -1729, 10795, 15639, -1729, 17921,
    1379,  1380,  1383, -1729,  8412,   685, -1729,  1216, -1729, -1729,
   -1729, -1729,  1294, -1729, -1729, -1729,  1098, -1729,  3581, -1729,
   -1729,   597,  2204,  1384, 19003, -1729,   958,  1314, -1729, -1729,
    1382,  1389,  2484, 18859, -1729,  1391,   478,  1388,  1393,  1399,
    1396,  1401, 20371,  1402,  1403,  1404, 10633, 20371, -1729, -1729,
    1489, -1729, -1729, -1729, 20371, -1729,  1407,  1408, 18499,   977,
   -1729, 18859,  1410, -1729,  1411, -1729, -1729,  3264, -1729, -1729,
     946, -1729, -1729, -1729, -1729,  3264, -1729, -1729,   989,   568,
   -1729, -1729,  1101,  1101,  1101,   628,   628,   646,   646,   796,
     796,   796,   796,   488,   488,   830,  1112,  1116,  1109,  1158,
   20371,  1005, -1729,  1415,  3264, -1729, -1729,  8836, -1729, 17921,
    1419,  1420,  1421,  1906, -1729, -1729, -1729, -1729, -1729, -1729,
   -1729, -1729,  1294, -1729, -1729,  1294, 17921, 17921, -1729, -1729,
    2967,   860,  1422,  1423,  1425,  1426,  2668,  2607, -1729, -1729,
   -1729, -1729, -1729, -1729, -1729, -1729, -1729, -1729, -1729, -1729,
   -1729, -1729,  1427, -1729,  1301, -1729, -1729, -1729, -1729, -1729,
   -1729, -1729, -1729,  1428,  1429, -1729,   289,  3264,  1080,   230,
   -1729, -1729,  1414, -1729, 18643, -1729, 20371,   685, 19075, 13771,
   -1729, -1729, -1729,  1413,   532,  1185, -1729,   545,  1185, -1729,
   -1729, -1729, -1729,  1294, -1729, -1729, -1729,  1294,   993,  1432,
    1294, -1729, -1729, -1729, -1729, -1729, -1729, -1729,  1439, -1729,
   -1729,  1345, -1729,   634, -1729, -1729, -1729, -1729, -1729, 12302,
    1437,  1434, -1729,   125, -1729,   449,   325, 10471,  1387, 13600,
    1441,  1443,  2738,  2757,  3095, 19147,  1446, -1729, -1729,  1449,
    1452, -1729, -1729,   634, 20371, 20371,  1524,  1440,   531, -1729,
    1531,  1457,  1445, -1729, -1729, -1729,  9603, -1729, -1729, -1729,
   -1729, -1729,  2001, -1729, -1729, -1729,  1526, -1729, -1729, -1729,
    1581, -1729, -1729, 12149, 15943,  1461, -1729,  4620, -1729,  1448,
    1459,  1467, -1729,  1091, -1729, -1729, -1729, -1729,  4300, -1729,
   -1729,  1451,  1453,   975, 16701,   704,   704,  1314, -1729, -1729,
    1110,  1186, 15485, -1729,  1142, -1729, 10957, -1729,   554,  1185,
   -1729,  1098, 11350, -1729, -1729,   958,   685,   685,   420,  1302,
   -1729,  8836, -1729,  1314,  1478,  1480, -1729, -1729,   991,   602,
   10633,  1581, -1729,   602, 16499,   602, -1729, 20371, 20371, 20371,
   -1729, -1729, -1729, -1729, 20371, 20371,  1474,  8836, -1729, -1729,
    1477,   637, -1729, -1729, -1729,  3942, -1729, -1729,  1123, -1729,
      56, -1729, 18859,  1148, -1729, 18715, -1729, -1729, 20371,  1465,
    1154,  1156,  1224, -1729,   556,  1185, -1729, -1729, 17921, 17921,
   -1729, -1729,  1484,   578,  1185, -1729,   590,  2952,   685,   685,
   -1729, -1729, 17921, 17921, -1729,  1487, -1729, 14561, 14561,  1491,
    1488,  1492,  1493, -1729,  1494, 20371, 20371,  1171,  1497, -1729,
   -1729, -1729, -1729, -1729, -1729, -1729,  1495, 20371, -1729, -1729,
   -1729,  1294, -1729, -1729, -1729,  1294, 17921, 17921,   289,   685,
    1195,  1502,  1500, -1729, -1729,  1506, 12455, 12608, 12761, 16701,
   17368, 17368,  1508, -1729,  1481,  1485,  2232, 13063, -1729,   198,
    4620, -1729, -1729,  4620, -1729, 18571,    -3,    42, -1729, -1729,
   -1729, -1729, 20371,  1509,  1582, 10308,  9964, -1729,  1507, -1729,
    1510, 20371,  1511,  8836,  1521, 20371, 18715, 20371,  1026, -1729,
    1522,   206, -1729,    36,  1512, -1729, -1729,  1514, -1729,  1525,
   -1729,  1528,  1516, 13600,   717, 13379,   685,   228, -1729, -1729,
   -1729,  1517, -1729,  1515, -1729,  1519, -1729,  1543, -1729,  1545,
   -1729, -1729, -1729, -1729,  1532, 11119,  1549,  1550,  1551, -1729,
    1555, -1729, -1729, -1729,  1294, 20371, 20371,  1186,  1554, -1729,
    1314, -1729,  1553,   280, -1729,  1224,  1563, -1729, -1729, 16701,
   -1729,  1561,  1559,  1030, -1729,  1564, -1729, -1729, -1729, -1729,
   -1729,  8836,  1224, 18715, -1729,  1599,  3264, -1729,  1599,  1599,
   -1729,  3264,  4326,  4450, -1729, -1729,  1197, -1729, -1729, -1729,
    1573,  1571, -1729, -1729, -1729,  1294, -1729, -1729,  1572,  1574,
     685, -1729, -1729, -1729,  1294, -1729, -1729, -1729,  1575, -1729,
   -1729, -1729, -1729, -1729, -1729, -1729, -1729, -1729, -1729, -1729,
   -1729, -1729,  1579, -1729, -1729, -1729, -1729,  1585,  1576,   685,
   -1729, 17921, 17921, -1729, -1729, -1729, -1729, 20371, -1729, -1729,
    1587, -1729,  1508,  1508,  1508,   813,  1562,   461, -1729,  4032,
     465, 15331, -1729, -1729, -1729,  3784, 20371,  3769,   494, -1729,
   -1729,   114,  1584,  1584,  4620, -1729, -1729, 18070, -1729, 20371,
    1588,  1593, -1729, -1729, -1729, -1729,  1035,  1596, 13600,  1457,
    1597, 20371,   500,  1592,   509, 12920, 16701, -1729, -1729, -1729,
     613, 13600, 20371,   816,   668, -1729, 20371,  8636, -1729, -1729,
     497, -1729,  1224, -1729,  1036,  1042,  1044, -1729, -1729, -1729,
   -1729,   634,  1026,  1600, -1729, -1729, 20371, -1729,  1603,   795,
   10471, -1729, -1729, -1729, -1729, 20371,  1648, -1729,  6454, -1729,
     685, 14561, -1729, -1729, 16701, -1729, -1729, -1729, -1729, -1729,
   -1729,  1602, -1729, 17921, -1729, -1729,  1604, -1729,  1610,  1609,
    1611,   958, -1729,  1618, -1729, -1729, -1729, 20371, -1729, 16499,
   20371,  1224,  1619,  1206, -1729,  1209, -1729,  3264, -1729,  3264,
   -1729, -1729, -1729, -1729, 17921,  1622,  1623, -1729, -1729, 17921,
   17921,  1626,  1628,  1217, 14087, 14245, -1729,  1631, -1729, -1729,
   -1729, -1729,  1637,  1638,  1226, -1729, -1729, -1729, -1729,   813,
    2009,   523, -1729, -1729, -1729, -1729,   685,   685, -1729, -1729,
   -1729,   560, -1729,  1047,  3784,   748, -1729,  3769,   685, -1729,
   -1729, -1729, -1729, -1729, -1729, -1729, -1729,   564, 13600,    92,
   19219,  1709, 13600,  1457, 14719, -1729, -1729, -1729, -1729, -1729,
   19291,  1717,  1620,  9426, 19363, 13600, 10136,  1457,   613,  1078,
    1625, 20371, -1729,  1653,   421, 13600, -1729, -1729,  1658, -1729,
   -1729,  1639,   795,   672,  1661,  1663,  1233,  1730, -1729, -1729,
   -1729, -1729,  4620,  4300, -1729, -1729,  1666,  1668, -1729, -1729,
   -1729,   958,  1314, -1729,  1673, -1729, -1729, -1729,  1675, -1729,
   -1729, -1729,  1251,  1253, -1729, -1729, -1729, -1729, -1729, -1729,
   -1729, -1729, -1729, -1729,  1674, -1729, -1729,  1681,  1694, -1729,
   -1729, -1729,  1695,  1696,  1697,  2009, -1729,   685, -1729, -1729,
   -1729, -1729, -1729,  1671,  4032, -1729, -1729,  7556,    66,  9041,
   -1729, 13482, -1729,    87,  1052, 13600,  1753,  1684,   235, 13600,
   20371,  1698,   613,  1078,  1679, 20803,  1692,   313,  1785, -1729,
   19435, 19507, 20371,  1457,  1685, 11284, -1729, -1729, -1729, 17518,
   -1729,  1704,  1687,   255, 13600, -1729, 20371, 18859,   441, -1729,
   -1729, -1729,  1711, -1729, -1729,  1314,  1715, -1729, -1729, -1729,
   -1729,  1713,  1716,  1719, 14561,  1721, -1729, -1729,   626,  1185,
   -1729, -1729,   813, -1729, -1729,   285, -1729,   315, -1729, -1729,
   -1729,  1723, 11831, -1729, -1729, 13600, -1729,    88, -1729, 13600,
    1727, 19579, -1729, -1729, 19651, 19723, 20371,  1698,  1457, 19795,
   19867, 13600,  1714,   371,  1720,   426, -1729, -1729,  1732, 11831,
   17518, -1729,  4226, 17316,  1581,  1731, -1729,  1787,  1737,   686,
    1734, -1729,  1818, -1729,  1064, 13600,  1744, 13600, 13600, -1729,
    1749, -1729, -1729, -1729, -1729, -1729, -1729, -1729, -1729,  1294,
   -1729, 20371, -1729, 20371, -1729, -1729,  1339, 11990, -1729, -1729,
   13600, -1729, -1729, -1729, -1729,  1457,  1739,   482,  1740,   524,
   -1729, -1729,  1457, -1729,  1457, -1729,  1757, 19939, 20011, 20083,
   -1729,  1339, -1729,  1735,  2345,  2680, -1729, -1729, -1729,   255,
    1759, 20371,  1741,   255,   255, 13600, -1729, -1729, 20371,  1819,
    1821, -1729, 17921, -1729, -1729, 13482, -1729,  1339, -1729, -1729,
    1775, 20155, 20227, 20299, -1729, -1729,  1457, -1729,  1457, -1729,
    1457, -1729,  1735, 20371,  1778,  2680,  1779,   795,  1790, -1729,
     700, -1729, -1729,  1100,  1730,   433, -1729, -1729,  9187,  1788,
   13482, -1729, -1729,  1457, -1729,  1457, -1729,  1457,  1794,  1792,
   -1729,   634,   795,  1795, -1729,  1772,   795, -1729, -1729, 13600,
    1874,  1801, -1729, -1729, -1729,  9354, -1729,   634, -1729, -1729,
    1264, 20371, -1729,  1108, -1729, 13600, -1729, -1729,   795,  1581,
    1802,  1781, -1729, -1729, -1729,  1111, -1729, -1729,  1782,  1581,
   -1729, -1729
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,   450,     0,     2,   450,   467,   468,   469,   470,   471,
     472,   473,   474,   456,   458,   457,   459,     0,     0,     0,
     475,   477,   498,   478,   499,   481,   482,   496,   497,   476,
     494,   495,   479,   480,   483,   484,   485,   486,   487,   488,
     489,   490,   491,   492,   493,   500,   501,   789,   503,   576,
     577,   580,   582,   578,   584,     0,     0,     0,   450,     0,
       0,    16,   547,   553,     9,    10,    11,    12,    13,    14,
      15,   753,    97,     0,    19,     0,     2,    95,    96,    17,
      18,   805,   450,   754,   399,     0,   402,   679,   404,   413,
       0,   403,   433,   434,     0,     0,     0,     0,   530,   452,
     454,   460,   450,   462,   465,   515,   502,   438,   508,   513,
     439,   525,   440,   540,   544,   550,   529,   556,   568,   789,
     573,   574,   557,   624,   405,   406,     3,   755,   768,   455,
       0,     0,   789,   827,   789,     2,   844,   845,   846,   450,
       0,  1003,  1004,     0,     1,   450,     0,   450,   422,   423,
       0,   530,   444,   445,   446,   758,     0,   579,   581,   583,
     585,     0,   450,     0,   790,   791,   575,   504,   672,   673,
     671,   732,   727,   717,     0,     0,   756,     0,     0,   450,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   548,
     551,   450,   450,     0,  1005,   530,   834,   852,  1009,  1002,
    1000,  1007,   398,     0,   159,   685,   158,     0,   407,     0,
       0,     0,     0,     0,     0,     0,   397,   904,   905,     0,
       0,   432,   787,   789,   783,   808,   789,   789,   785,     2,
     789,   784,   865,   789,   789,   862,     0,   523,   524,     0,
       0,   450,   450,   467,     2,   450,   414,   453,   463,   516,
       0,   545,     0,   771,     2,     0,   679,   415,   530,   509,
     526,   541,     0,   771,     2,     0,   466,   510,   517,   518,
     527,   532,   542,   546,     0,   560,     0,   747,     2,     2,
     769,   826,   828,   450,     0,     2,     2,  1013,   530,  1016,
     787,   787,     3,     0,   530,     0,     0,   425,   789,   785,
     784,     2,   450,     0,   751,     0,   713,   715,   714,   716,
       0,     0,   709,     0,   699,     0,   708,   719,     0,   789,
     789,     2,   450,  1024,   451,   450,   462,   441,   508,   442,
     533,   443,   540,   537,   558,   789,   559,     0,   660,   450,
     661,   978,   979,   450,   662,   664,   547,   553,     0,   625,
     626,     0,   792,     0,   730,   718,     0,   796,    21,     0,
      20,     0,     0,     0,     0,     0,     0,    23,    25,     4,
       8,     5,     6,     7,     0,     0,   450,     2,     0,    98,
      99,   100,   101,    82,    24,    83,    38,    81,   102,     0,
       0,   117,   119,   123,   126,   129,   134,   137,   139,   141,
     143,   145,   147,   150,     0,    26,     0,   554,     2,   102,
     450,   151,   724,   675,   544,   677,   723,     0,   674,   678,
       0,     0,     0,     0,     0,     0,     0,   806,   832,   789,
     842,   850,   854,   860,     2,  1011,   450,  1014,     2,    95,
     450,     3,   659,     0,  1024,     0,   451,   508,   533,   540,
       3,     3,   641,   645,   655,   661,   662,     2,   835,   853,
    1001,     2,     2,    23,     0,     2,   685,    24,     0,   683,
     686,  1022,     0,     0,   692,   681,   680,     0,     0,   773,
       2,     2,     2,     2,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   811,   868,   789,     0,   679,     2,
     807,   815,   931,   809,   810,     0,   771,     2,   864,   872,
       0,   866,   867,     0,   428,   450,   450,   514,   451,     0,
     530,   450,  1006,  1010,  1008,   531,   751,     0,   771,   787,
     408,   416,   464,     0,   771,     2,   751,     0,   771,   728,
     511,   512,   528,   543,   549,   552,   547,   553,   571,   572,
       0,   729,   450,   669,     0,   196,   391,   450,     3,     0,
     530,   450,   770,   450,     0,   410,     2,   411,   748,   430,
       0,     0,     0,     2,   450,   787,   450,   751,     0,     2,
       0,   712,   711,   710,   705,   461,     0,   703,   720,   506,
       0,     0,   450,   450,   980,   451,   447,   448,   449,   984,
     975,   976,   982,     2,     2,    96,     0,   940,   954,  1024,
     936,   789,   789,   945,   952,   667,   450,   538,   663,   451,
     534,   535,   539,     0,   789,   990,   451,   995,   987,   450,
     992,     0,  1022,   631,     0,     0,     0,   450,     0,   804,
     803,   799,   801,   802,   800,     0,   794,   797,     0,    22,
     450,    89,   450,   450,    84,   450,    91,     0,    32,     0,
      33,   450,    87,    88,   450,   450,   450,     2,    98,    99,
       0,     0,   177,     0,     0,   574,     0,  1000,     0,     0,
       0,     0,     0,     0,     0,     0,    50,     0,    56,    57,
      61,     0,     0,    61,     0,    85,    86,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     450,   160,   161,   162,   163,   164,   165,   166,   167,   168,
     169,   170,   158,     0,   156,   157,     2,   916,   676,   913,
     789,   789,   921,   555,   450,   833,   789,   843,   851,   855,
     861,     2,   836,   838,   840,     2,   856,   858,     0,  1012,
    1015,   450,     0,     0,     2,    96,   940,   789,  1024,   886,
     789,   789,  1024,   789,   901,   789,   789,     3,   663,     0,
       0,  1024,  1024,   450,   450,     0,     2,   694,     0,  1022,
     691,  1023,     0,   687,     0,     2,   690,   693,   174,   173,
       0,     2,   450,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   789,   820,   824,   863,   789,   877,
     882,   812,   869,     0,     0,   436,   928,     0,   774,     0,
     450,   775,   429,     0,     0,     0,     0,   427,     2,   776,
       0,   412,   751,     0,   771,     2,   777,     0,     0,     0,
       0,   586,   648,   451,     3,     3,   652,   651,   847,     0,
       0,   450,   392,     0,   530,     3,    95,     3,   450,     0,
       3,   752,     2,   707,   450,   450,   701,   700,   701,   507,
     505,   625,     0,   986,   450,   991,   451,   450,   977,   450,
       0,     0,     0,   955,     0,   789,  1025,   941,   942,   668,
     938,   939,   953,   981,   985,   983,   536,   571,     0,   989,
     994,   628,  1023,     0,     0,   627,     0,  1022,   733,   731,
       0,     0,   796,    61,   757,     0,     2,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   450,     0,   116,   115,
       0,   112,   111,    27,     0,    28,     0,     0,     0,     0,
       3,    61,     0,    46,     0,    47,    54,     0,    53,    65,
       0,    62,    63,    66,    49,     0,    48,    52,     0,     0,
      45,   118,   120,   121,   122,   124,   125,   127,   128,   132,
     133,   130,   131,   135,   136,   138,   140,   142,   144,   146,
       0,     0,   401,     0,     0,    29,     3,   685,   152,   450,
       0,     0,     0,   917,   918,   914,   915,   726,   725,     2,
     837,   839,   841,     2,   857,   859,   450,   450,   933,   932,
       2,     0,     0,     0,     0,     0,   789,   941,   889,   906,
       2,   884,   892,   665,   887,   888,   666,     2,   899,   909,
     902,   903,     0,     3,  1024,   420,     2,  1017,     2,   656,
     657,   635,     3,     3,     3,     3,   679,     0,   150,     0,
       3,     3,     0,   688,     0,   682,     0,   789,     0,   450,
       3,   424,   426,     0,   789,   821,   825,   789,   878,   883,
       2,   813,   816,   818,     2,   870,   873,   875,   787,     0,
     929,     3,   779,     3,   520,   519,   522,   521,     2,   752,
     780,     2,   778,     0,   752,   781,   586,   586,   586,   450,
       0,     0,   670,     0,   395,     0,     0,   450,     0,     2,
       0,     0,     0,     0,     0,   179,     0,   325,   326,     0,
       0,   364,   363,     0,   154,   154,   370,   547,   553,   193,
       0,   180,     0,   204,   181,   182,   450,   198,   183,   184,
     185,   186,     0,   187,   188,   331,     0,   189,   190,   191,
       0,   192,   200,   530,   450,     0,   202,     0,   389,     0,
       0,     0,     3,     0,   759,   752,   740,   741,     0,     3,
     736,     3,     3,     0,   450,   717,   717,  1022,   988,   993,
       2,    95,   450,     3,   545,     3,   451,     3,   789,   948,
     951,   450,     3,   937,   943,     0,   789,   789,     0,   631,
     615,   685,   632,  1022,     0,     2,   793,   795,     0,    90,
     450,     0,    94,    92,   450,     0,   106,     0,     0,     0,
     110,   114,   113,   178,     0,     0,     0,   685,   103,   171,
       0,     0,    41,    42,    79,     0,    79,    79,     0,    67,
      69,    44,     0,     0,    40,     0,    43,   149,     0,     0,
       0,     0,  1022,     3,   789,   924,   927,   919,   450,   450,
       3,     3,     0,   789,   895,   898,   789,     0,   789,   789,
     890,   907,   450,   450,  1018,     0,   658,   450,   450,     0,
       0,     0,     0,   409,     3,     0,     0,     0,     0,   684,
     689,     3,   772,   176,   175,     3,     0,     0,     2,   814,
     817,   819,     2,   871,   874,   876,   450,   450,   679,   789,
       0,     0,     0,   752,   782,     0,   450,   450,   450,   450,
     450,   450,   569,   597,     3,     3,   598,   530,   587,     0,
       0,   829,     2,     0,   393,    61,     0,     0,   316,   317,
     201,   203,     0,     0,     0,   450,   450,   312,     0,   310,
       0,     0,     0,   685,     0,     0,     0,     0,     0,   155,
       0,     0,   371,     0,     0,     3,   208,     0,   199,     0,
     307,     0,     0,     2,     0,   530,   789,     0,   390,   935,
     934,     0,     2,     0,   743,     2,   738,     0,   739,     0,
     721,   702,   706,   704,     0,   450,     0,     0,     0,     3,
       0,     2,   944,   946,   947,     0,     0,    95,     0,     3,
    1022,   621,     0,   631,   629,  1022,     0,   618,   734,   450,
     798,     0,     0,     0,    34,     0,   107,   109,   108,   105,
     104,   685,  1022,     0,    60,    76,     0,    70,    77,    78,
      55,     0,     0,     0,    64,    51,     0,   148,   400,    30,
       0,     0,     2,   920,   922,   923,     3,     3,     0,     0,
     789,     2,   891,   893,   894,     2,   908,   910,     0,   885,
     900,     3,     3,  1019,     3,   643,   642,   646,  1021,     2,
       2,  1020,     0,     3,   786,   695,   696,     0,     0,   789,
     431,   450,   450,     3,     3,   437,   788,     0,   879,   763,
       0,   765,   569,   569,   569,   604,   574,     0,   610,   598,
       0,   450,   561,   596,   592,     0,     0,     0,     0,   599,
     601,   789,   612,   612,     0,   593,   608,   450,   396,     0,
       0,    62,   320,   321,   318,   319,     0,     0,     2,   219,
       0,     0,   221,   404,   220,   530,   450,   298,   297,   299,
       0,     2,   179,   257,     0,   252,     0,   179,   313,   311,
       0,   305,  1022,   314,     0,     0,     0,   352,   353,   354,
     355,     0,   345,     0,   346,   322,     0,   323,     0,     0,
     450,   210,   197,   309,   308,     0,   343,   362,     0,   394,
     789,   450,   761,   722,   450,     2,     2,   619,   996,   997,
     998,     0,   949,   450,     3,     3,     0,   957,     0,     0,
       0,     0,   630,     0,   617,     3,    93,     0,    31,   450,
       0,  1022,     0,     0,    80,     0,    68,     0,    74,     0,
      72,    39,   153,   925,   450,     0,     0,   830,   848,   450,
     450,     0,     0,     0,   450,   450,   698,     0,   417,   419,
       3,     3,     0,     0,     0,   767,   565,   567,   563,     0,
     964,     0,   605,   969,   607,   961,   789,   789,   591,   611,
     595,     0,   594,     0,     0,     0,   614,     0,   789,   588,
     602,   613,   603,   609,   650,   654,   653,     0,     2,     0,
       0,   240,     2,   222,   530,   303,   301,   304,   300,   302,
       0,   248,     0,   179,     0,     2,   450,   258,     0,   283,
       0,     0,   306,     0,     0,     2,   329,   356,     0,   347,
       2,     0,     0,     0,     0,   334,     0,   330,   195,   194,
     418,   737,     0,     0,   999,     3,     0,     0,   956,   958,
     620,     0,  1022,   633,     2,    37,    35,    36,     0,    58,
     172,    71,     0,     0,     3,   831,   849,     3,     3,   896,
     911,   421,     2,   640,     3,   639,   697,     0,     0,   822,
     880,   930,     0,     0,     0,   965,   966,   789,   590,   962,
     963,   589,   570,     0,     0,   209,   328,     0,     0,     0,
     233,     2,   211,     0,     0,     2,   242,   266,   260,     2,
     179,   295,     0,   270,     0,     0,   261,   259,   250,   253,
       0,     0,   179,   284,     0,     0,   214,   327,     2,   450,
     324,     0,     0,   372,     2,   332,     0,    61,     0,   344,
     742,   744,     0,   959,   960,  1022,     0,   735,    59,    75,
      73,     0,     0,     0,   450,     0,   823,   881,   789,   972,
     974,   967,     0,   600,   228,   223,   226,     0,   225,   232,
     231,     0,   450,   235,   234,     2,   244,     0,   241,     2,
       0,     0,   249,   254,     0,     0,   179,   296,   271,     0,
       0,     2,     0,   286,   287,   285,   256,   315,     0,   450,
     450,     3,   357,   451,   361,     0,   365,     0,     0,     0,
     373,   374,   217,   335,     0,     2,     0,     2,     2,   950,
       0,   623,   926,   897,   912,   644,     2,   968,   970,   971,
     606,     0,   230,     0,   229,   213,   236,   450,   385,   245,
       2,   246,   243,   269,   267,   263,   275,   273,   274,   272,
     255,   268,   264,   265,   262,   251,     0,     0,     0,     0,
     216,   236,     3,   350,     0,   964,   358,   359,   360,   372,
       0,     0,     0,   372,     0,     2,   333,   340,     0,   337,
     339,   622,   450,   224,   227,     2,     3,   237,   386,   247,
       0,     0,     0,     0,   294,   292,   289,   293,   290,   291,
     288,     3,   350,     0,     0,   965,     0,     0,     0,   366,
       0,   375,   218,     0,   330,     0,     3,   205,     0,     0,
       2,   282,   280,   277,   281,   278,   279,   276,     0,     0,
     351,     0,   378,     0,   376,     0,   378,   336,   338,     2,
       0,     0,   207,   206,   212,     0,   215,     0,   348,   379,
       0,     0,   367,     0,   341,     2,   973,   349,     0,     0,
       0,     0,   342,   380,   381,     0,   377,   368,     0,     0,
     369,   382
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1729,  5926,  4965, -1729,    -1,   389,  1858,  -163, -1729,  1589,
   -1729,   328, -1729,  -668,   617,   713,  -907, -1025, -1729,   175,
    2177,  1636, -1729,  -212, -1729,  1307,   591,   726,   728,   576,
     727,  1258,  1259,  1260,  1266,  1257, -1729,    37,  -168,  7975,
     847, -1729,  1577, -1729, -1729,  -645,  4388, -1060,  1552, -1729,
     445, -1729,   837,   -35, -1729, -1729, -1729,   398,    57, -1729,
   -1722, -1401,   267,    34, -1729, -1729, -1729,   281, -1445, -1729,
   -1285, -1729, -1729, -1729, -1729,   -17, -1701,   162, -1729, -1729,
     -12, -1729, -1729, -1729,     5,   419,   423,   103, -1729, -1729,
   -1729, -1729,  -701, -1729,    35,   -23, -1729,   115, -1729,    -4,
   -1729, -1729, -1729,   851,  -543,  -851, -1302, -1729,    19, -1226,
      12,  3516,  -785,  -744, -1729,  -263, -1729,    67,  -136,   278,
     152,  -209,  3484,  6602,  -619, -1729,     3,   229,   130,  2798,
   -1729,  1967, -1729,    55,  3642,  -291, -1729, -1729,   190, -1729,
   -1729,  2032,    79,  3954,  2385,   -60,  1770,  -253, -1729, -1729,
   -1729, -1729, -1729,  -536,  4544,  5148, -1729,  -347,   192, -1729,
     518,   242, -1729,   177,   710, -1729,   510,   -94, -1729, -1729,
   -1729,  5475,  -601, -1119,  -518,  -476,  -472,   583, -1729, -1248,
    -141,   142,  -128,   885,  7239,  -333,  -346,  -245,  -179,  -457,
    1256, -1729,  1569,   157,  1170,  1464, -1729, -1729, -1729, -1729,
     257,  -151,    33,  -845, -1729,   457, -1729, -1729,   622,   456,
   -1729, -1729, -1729,  2053,  -753,  -325,  -728,   -13, -1729, -1729,
   -1729, -1729, -1729, -1729,   -97,  -726,  -132, -1728,  -174,  7637,
     -68,  6539, -1729,  1135, -1729,   734,  -203,  -213,  -211,  -195,
       1,   -72,   -66,   -54,    -7,   -36,    50,    53,  -188,   -57,
    -159,  -155,  -121,  -724,  -631,  -626,  -623,  -704,   -58,  -621,
   -1729, -1729,  -652,  1325,  1335,  1337,  2997,  7027,  -560,  -562,
    -554,  -517,  -570, -1729, -1626, -1623, -1619, -1618,  -581,  -135,
    -215, -1729, -1729,   -70,    62,   -33, -1729,  7266,   472,  -367,
    -414
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,  1140,   213,   383,   384,    80,    81,   385,   360,   386,
    1433,  1434,   387,   960,   961,   962,  1248,  1249,  1250,  1445,
     409,   389,   390,   391,   670,   671,   392,   393,   394,   395,
     396,   397,   398,   399,   400,   401,   402,   411,  1059,   672,
    1370,   733,   207,   735,   405,   800,  1141,  1142,  1143,  1144,
    1145,  1146,  1147,  2018,  1148,  1149,  1375,  1550,  1866,  1867,
    1800,  1801,  1802,  1986,  1987,  1150,  1564,  1565,  1566,  1709,
    1710,  1151,  1152,  1153,  1154,  1155,  1156,  1383,  1736,  1918,
    1839,  1157,  1158,  1582,  2004,  1583,  1584,  1901,  1159,  1160,
    1161,  1373,  1909,  1910,  1911,  2050,  2065,  1936,  1937,   284,
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
      79,   183,   131,    79,   102,   181,   486,   184,   487,   789,
     404,   530,   198,    89,   359,   190,   149,   494,   297,   185,
     558,   337,   355,   148,   488,   969,   323,   340,   476,   675,
    1183,   489,   517,   949,   917,   904,  1838,   186,   231,  1485,
    1486,   890,  1028,   676,  1786,   176,  1782,   898,   498,   891,
    1783,  1784,   942,  1552,    79,    79,   107,    79,  1253,   131,
     490,   102,  1029,   140,   491,  1364,   140,  1869,    95,  1868,
      89,   617,    79,   682,  -745,   197,   660,   198,   683,  1874,
     112,    79,   486,   196,   487,  1004,   892,  1260,   229,    79,
    1424,   254,  1104,   494,    79,   264,   228,    79,   492,   253,
     488,    79,   870,   263,   514,   351,   289,   489,   144,   421,
     246,    57,   209,   107,   257,   422,   565,   567,  1588,  1175,
     140,   256,  1721,   187,   627,    95,   188,   423,   630,  1553,
    1553,   505,   292,  1022,   510,  1166,   490,   112,  1023,    79,
     491,  1024,    79,  1025,    79,   424,   131,   183,   102,    79,
    1294,   484,   654,   184,   279,    79,   527,    89,   437,  1861,
    1875,  1940,    79,   495,   140,   185,   537,  1797,  1798,   834,
     836,  1542,   660,   197,   492,   523,  1452,   695,   696,    79,
      79,   196,  -771,   186,    57,  1589,  -387,   600,   459,  -746,
     162,   111,   497,   617,    79,   899,   458,  1032,   695,   279,
     107,   838,   890,  1039,   467,  1868,   898,   140,  1453,    79,
     891,   845,    95,   197,   545,   403,  1544,   155,    79,    79,
    1447,   196,  1870,   183,   112,   506,   156,   570,   695,   184,
     103,   425,   523,   266,   426,    79,  -388,   267,   197,   495,
     270,   185,   272,   715,    79,   566,   196,   892,   111,  1799,
     528,   535,   872,   825,    79,  1218,  -387,    79,  1333,   534,
     538,  1876,  1941,  1874,    79,   913,  1055,   499,  1815,   187,
     531,   807,   188,   808,    79,    79,  1967,    79,  1714,    96,
    1343,  1187,   150,  1241,  1070,   716,   561,   103,  1552,   809,
     821,   919,   260,   793,    79,    79,   810,   196,  1874,  1344,
     865,  1586,    79,  1280,  1622,   600,  -388,  1053,  1053,    79,
      79,  1054,  1054,  1838,    79,  1213,  1932,   925,  1162,   927,
     928,  1232,   929,  1281,  1334,   811,  1053,   248,   931,   812,
    1054,   933,   934,   935,   454,   111,    96,  1204,   107,  1786,
     198,  1782,   617,   161,   967,  1783,  1784,    79,  1323,   588,
    1172,  1267,    79,  1534,  1033,    79,   644,   807,  1036,   808,
     194,  1295,   112,   813,  1553,  1335,   617,  1049,  1050,   821,
     358,  1322,  1535,   617,   103,   809,  1325,   885,   266,  1861,
    1587,   832,   810,  1343,  1346,  1347,   773,   837,   588,  1022,
     944,  1351,  1053,   146,  1023,  1296,  1054,  1024,  1008,  1272,
    1881,    57,  1599,   760,   190,   566,  1485,  1486,  1907,   829,
      57,   811,   421,   459,   910,   812,  1931,   287,   422,    79,
    1166,   458,  1062,    96,   857,   194,  1636,  1638,  1640,   606,
     423,   840,   274,  1820,  1821,   204,   822,   843,   323,   340,
     175,   847,    79,    79,   170,   170,   205,  1393,   424,   813,
    1296,  1032,   179,   914,    79,    79,   266,   267,   890,   621,
    -905,   272,   206,    79,   870,   467,   891,  1204,   944,   435,
    1933,  1934,    19,   111,   594,  1333,  1333,  1333,  1890,   170,
     278,   600,   279,    79,   499,   971,   459,   532,   177,   507,
    1553,   625,    79,   499,   458,   628,  1797,  1798,   421,  1348,
     209,   178,  1712,   892,   422,   157,  2039,  1720,   158,   159,
     636,   160,    79,   638,  1915,   822,   423,  1101,    79,   581,
     519,  1130,    57,   522,    62,    63,   944,  1884,  1885,   170,
    1382,   844,   170,   179,   425,  1985,  1957,   426,    57,  1635,
    1262,  1334,  1334,  1334,    57,   170,    57,  1916,   582,   583,
    1214,  1305,   349,   248,   201,   993,    79,    57,    79,   179,
    1985,    96,    57,   871,   189,    63,   561,  -566,   210,    79,
    -771,    79,    75,   458,  1350,    79,   280,   131,  1825,   102,
     522,   944,  1335,  1335,  1335,    79,  2020,    57,    89,    79,
     585,  1959,   594,  1053,   586,  1510,   887,  1054,  1345,   170,
     617,   573,    57,  1162,  1420,   499,  1925,   544,    63,  1043,
    1456,   904,  1063,   705,   706,    57,  1679,   751,   107,   201,
    1534,   499,    79,  1009,    57,  1030,    57,   499,  1413,   604,
    1286,   107,   191,   617,    79,  1680,  1037,   944,   140,  1682,
     604,  1080,   112,    95,   170,   499,  1553,  1991,    57,  1688,
      -3,   419,   944,  1814,   170,   112,   479,   707,   708,   545,
      57,  1089,   266,   454,   548,   170,  1084,   553,  1689,  1189,
     499,  1722,  1072,  1553,   202,   248,  1683,  1540,  1787,   944,
     179,  1308,    79,   216,    79,   499,    79,  -673,   194,  1993,
      79,  1088,   170,    79,  1312,   236,    57,  1788,   499,   170,
     170,   937,   857,  1411,   170,  1462,  1477,   604,   773,   499,
    -444,  1553,   938,   939,   759,  1688,  1256,   524,    79,   944,
    1226,  1575,   539,  1252,   278,  1230,   427,  1471,   499,   562,
    1762,   499,  1763,   551,  1791,   454,  1238,   170,  1795,  1475,
     589,   274,   170,   604,   883,   170,  1212,  1103,   870,   274,
    1883,   260,  1236,   111,  1425,    57,  1705,  1706,  1707,  1237,
    -445,   857,  1896,    79,   887,    79,   111,   276,   903,    13,
      14,    15,    16,    17,   524,  1926,   278,    79,  1708,   499,
    1442,   909,   684,   293,    79,  1444,    72,   685,   698,   248,
     467,   279,  1252,    79,   602,   699,   700,   278,  1633,   427,
     323,   340,    79,    79,    79,   103,   603,   182,   701,   702,
     604,  1293,  1551,  1567,   403,  1484,  -387,    77,   605,  1715,
    1404,   674,    79,  1832,  1716,  1058,  1950,    57,  1833,   223,
     606,  -760,   170,   237,   238,   150,   239,  1972,  1401,   717,
     240,    96,  1973,   718,   170,   170,  1426,   353,   532,   157,
    1207,  2035,   158,   159,    96,   160,  2036,   356,    79,    79,
     467,   507,   454,   817,  1164,   499,    13,    14,    15,    16,
      17,   857,   859,    89,   279,   311,    13,    14,    15,    16,
      17,    79,   201,   743,   298,  1300,  1177,   744,   857,   857,
     573,  1435,   427,  1176,   499,  1460,   794,   795,    72,   427,
     796,   499,   357,   454,   755,   654,  1572,    79,   499,   858,
     358,    79,   602,   859,  1318,    79,   107,    72,   736,   703,
     704,   644,   499,   140,    57,   454,   454,   918,    95,    77,
      78,   586,   428,   870,    57,   920,   140,   603,   457,   586,
     112,   604,    72,   617,   454,   429,   146,   461,    77,    78,
     828,   403,   430,   485,   223,   831,    79,   709,   710,  1705,
    1706,  1707,  1669,   431,    79,   432,   499,   631,  1279,   773,
     298,   944,   839,    77,    78,   170,  1666,  1667,  1668,   881,
     695,  1708,   846,  1516,  1631,  -448,   433,   419,   419,   462,
    1713,   475,  1415,    79,   921,  -449,   467,   477,   922,   191,
     678,   243,     6,     7,     8,     9,    10,    11,    12,  1030,
     454,   427,   907,   604,   943,  1436,  1437,  1438,   944,    79,
    1752,   480,  1439,  1440,   170,    79,    79,  1257,  1013,   571,
     298,  -446,   499,  1067,   355,   355,  1188,  1068,   481,  1340,
      13,    14,    15,    16,    17,   278,  1394,  1551,   507,   499,
     573,   111,   499,  1619,   499,   482,    79,   600,  1623,    61,
     592,   678,   168,   169,    64,    65,    66,    67,    68,    69,
      70,   483,   953,  1505,   955,  1632,   958,  1094,   946,   947,
     966,   944,   248,   970,   323,   340,   496,  1096,   905,   497,
     103,   944,  1554,   532,   515,   674,   516,  1251,    57,   526,
     674,  1252,   209,  1058,  1045,  1046,   674,   555,   995,   419,
      13,    14,    15,    16,    17,   248,  1047,  1048,   467,   592,
    1164,    79,    79,    79,   536,   674,  1400,  1493,  1494,    89,
     744,  1239,  1068,  1532,  1577,  1578,  1579,  1580,  1581,    96,
     857,   857,  1430,  1254,  1255,   467,  1252,  1487,  -435,  1164,
    1845,    79,  1178,   577,   857,   857,   254,   264,    89,    79,
     944,  1258,    79,    79,   253,   263,    79,   170,    57,  1914,
     624,  -435,   107,  1567,   170,   246,   257,    79,   767,   140,
    -904,  1628,  1774,   256,    95,  1629,  1699,  1724,   857,   857,
     944,   944,  1071,  1725,  1073,  1726,   112,  1068,  1792,   944,
    -616,   107,   744,  1877,    79,  1723,   635,   944,   140,   648,
     467,   454,   637,    95,   649,  1976,   419,   652,   806,  1252,
      79,  1705,  1706,  1707,   653,   112,   140,   223,    13,    14,
      15,    16,    17,   944,  -151,  -151,   467,  -116,  -116,  -116,
    -116,  -116,  -116,  1708,    79,  1047,  1392,   298,  1112,   170,
     170,  2037,  -180,   298,   657,   944,  1340,  1340,  1340,  2061,
    1518,  1340,  2068,  2058,  1758,   678,  2069,   697,  2006,   323,
     340,   170,  2010,   711,  1092,   712,    79,  1450,  1451,  1533,
     713,   979,   980,   981,   982,  1100,    57,   714,  1102,   972,
     973,   974,  1105,   298,   719,  1457,  1435,  1206,  1326,  1327,
    1328,   170,  1455,  1451,   869,   170,   298,   111,  1459,  1451,
    1019,  1443,   486,   745,   487,   746,  1532,  -115,  -115,  -115,
    -115,  -115,  -115,   747,   494,  1495,  1443,  1554,   748,  1938,
     488,   749,   403,   403,   750,    79,   111,   489,   434,    79,
      -3,   149,    79,   777,   903,    72,   103,   419,   148,  1019,
    1507,  1641,  1068,  -447,   260,   -17,  1938,   790,  1556,  1556,
    1760,  1068,   467,  1761,  1451,   603,   490,    89,    89,   604,
     491,  1771,  1772,   857,   857,   103,    77,   605,   535,   791,
    1781,   944,   467,  1735,    79,  1846,   534,  1336,  1836,  1837,
     801,   140,   248,   814,  1988,    96,   815,   531,    13,    14,
      15,    16,    17,   941,   492,  1849,  1451,  1850,  1451,  1696,
     107,   107,   816,  1093,  1797,  1798,   818,   140,   140,  2058,
    2059,  1448,  1449,   819,    96,   532,  1733,   975,   976,   454,
     454,   977,   978,   820,   112,   112,   983,   984,  1691,  1691,
     467,  1177,  1402,  1403,   824,    79,   826,   286,  1176,   860,
      79,    79,    79,   842,  -564,  -562,   851,  1684,   873,   875,
    1487,   879,  1533,   882,   895,   323,   340,   893,   916,   912,
     495,   807,  1197,   808,   606,   914,   923,   924,  1920,   945,
     948,   951,   992,   997,   821,   857,  1018,  1019,  1026,   809,
    1065,   140,  1074,  -749,  1075,  1076,   810,  1077,  1021,  1078,
     767,    13,    14,    15,    16,    17,  1231,  1079,   170,  1095,
    1097,   170,   170,   170,  1487,  -649,   857,  1106,    79,  1107,
    1108,   857,   857,  1167,    79,   811,    79,  1168,  1184,   812,
    1198,  1199,  1210,    79,  1200,   170,  1352,  1372,   298,  1215,
    1216,   170,  1219,  1221,  1222,   111,   111,   467,  1324,   553,
    1223,  1224,  1225,  1227,  1228,  1229,   170,   298,  1234,  1235,
     467,  1349,  1299,   813,  1242,  1243,  1259,   246,   257,  1831,
    1264,  1265,  1266,  1273,  1274,   256,  1275,  1276,  1368,  -637,
    -636,  1284,  1263,  1319,   103,   103,  1307,  -750,  1341,  1342,
    1355,  1532,  1356,  1556,   170,  1365,  -672,   467,  1366,  1270,
    1271,  1367,    89,  1374,  1336,  1336,  1336,   150,  1515,  1519,
    1431,  1841,   944,  1389,   532,  1382,  1177,  1386,   140,  1376,
      79,  1390,  1388,  1176,  1913,  1396,  1427,  1398,  1428,  1865,
     822,  1441,  1443,    96,    96,  1470,    79,    82,    79,  1458,
     147,  1483,  1488,  1489,  1491,   107,  1499,  1490,  1509,  1451,
     617,  1496,   140,  1508,  1511,  1523,   419,  1521,  1345,  1524,
    1547,  1590,  1592,  1602,   905,  1595,   140,  1603,  1600,   112,
    1908,   243,     6,     7,     8,     9,    10,    11,    12,  1695,
    1607,  1568,    61,    79,  1569,  1571,    79,    64,    65,    66,
      67,    68,    69,    70,    82,  1573,  1585,   467,  1605,  1593,
    1606,   467,  1594,  1608,  1609,  1610,  1612,  1178,  1617,   180,
    1621,  1624,  1626,  1487,   467,  1627,   531,  1634,    82,  1556,
    1630,  1642,  1643,  1647,   467,  1648,   427,  1658,    89,   170,
     265,   220,   170,  1656,   245,  1665,  1678,  1533,    82,  1495,
    1526,    79,    79,  1252,  1698,  1700,   260,   210,  1702,  1730,
      79,   486,  1732,   487,  1021,  1737,  1744,  1750,  1748,  1904,
    1278,   767,   494,  1983,  1749,  1865,  1753,  1759,  1751,   488,
    1966,   107,   170,  1765,  1766,   147,   489,  1769,   140,  1770,
     111,    82,  1805,   147,   248,  1776,   296,   302,  1779,  1780,
    1809,  1543,  1545,    79,  1810,   112,   454,   454,   322,  1822,
     467,  1824,   821,  2008,   467,   490,  1908,  1828,   467,   491,
    1908,  1908,  1834,  1830,  1835,   410,   180,   180,  1130,   103,
    1843,  1847,  1844,  1848,   499,  -638,  1879,   147,   440,  1597,
    1904,   245,  1856,   467,   403,   243,     6,     7,     8,     9,
      10,    11,    12,   492,  2033,  1857,  1858,  1859,  1860,  1880,
    -547,  1466,  1467,  1886,   857,   220,   220,  1889,  1891,  1897,
    1905,  1906,  1919,  1921,  1922,  1481,  1482,  1923,    96,  2049,
    1924,  1935,   296,  2049,   467,  1556,  1772,  1943,   467,  1956,
    1960,    82,  1178,  2060,    89,  1958,  1971,  1969,  1970,  1974,
     467,  1975,   183,  1978,   245,  2063,   570,  1981,   184,  1503,
    1504,    79,  1556,    79,  1990,  1992,   111,  1994,   495,  2003,
     185,    89,   298,  2007,   467,  2009,   467,   467,    13,    14,
      15,    16,    17,   170,   302,  2021,  2014,   107,  2015,  2031,
     302,   296,   296,   532,   140,  2032,  2044,   170,   147,   467,
    1556,  2034,  2046,  2047,  2051,   103,  2052,  2055,   822,    89,
     170,   112,  2056,  2066,   107,  2067,  2070,  1756,   322,   607,
     616,   140,  1541,    79,    79,  1454,   196,   681,   403,   985,
     403,   986,   989,   987,   467,   322,    57,   940,   112,   322,
     988,  1902,  1371,  1378,   467,  2045,   734,   170,  1734,    18,
    1984,  1826,   107,   419,    96,  2001,   454,  1819,  2040,   140,
    1917,  1728,  2038,  1962,    79,  1729,   458,  2029,   403,  2011,
     170,  1478,   410,  2053,  1961,   167,   112,   467,  1387,   467,
     525,    13,    14,    15,    16,    17,  1727,    47,    48,    49,
      50,    51,    52,    53,    54,    72,  1863,  1681,   467,  1930,
    2030,  1520,   797,  1692,   467,  1384,   410,  1064,  1186,   737,
     877,  1625,  1902,     3,   467,   736,   180,  1217,    79,   499,
    1741,  1000,   111,   468,     0,     0,    77,    78,    79,     0,
    1531,  1001,   147,  1002,     0,     0,   440,  1379,     0,    57,
     766,     0,   616,     0,  1660,  1661,     0,   170,   403,   111,
      18,   170,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   103,    61,     0,   170,   168,   169,    64,    65,    66,
      67,    68,    69,    70,   170,     0,     0,     0,     0,     0,
     220,     0,     0,     0,     0,     0,     0,   111,   103,   220,
       0,   170,    51,    52,    53,    54,    61,     0,    72,     0,
     170,    64,    65,    66,    67,    68,    69,    70,   956,   296,
      96,   410,   410,     0,     0,   296,     0,   322,  1669,     0,
       0,     0,   499,     0,     0,     0,   103,     0,     0,    77,
      78,     0,     0,     0,    61,  1380,     0,    96,     0,    64,
      65,    66,    67,    68,    69,    70,   964,     0,   957,     0,
     170,     0,     0,     0,   170,   296,  1745,     0,   170,     0,
       0,     0,     0,     0,     0,     0,   296,     0,   296,     0,
     322,    61,    82,  1208,     0,    96,    64,    65,    66,    67,
      68,    69,    70,   170,   448,     0,   965,  1764,   322,   440,
       0,   616,  1767,  1768,     0,   236,     0,     0,     0,   607,
       0,     0,     0,   607,   686,     0,   687,   688,   689,     0,
       0,     0,   322,  1531,     0,     0,     0,     0,     0,  1685,
      74,  1531,   616,   786,   170,   322,     0,     0,   170,     0,
       0,     0,     0,   147,     0,   690,     0,     0,   691,   692,
     170,     0,     0,   693,   694,     0,   410,     0,   147,   147,
       0,   410,     0,  1968,     0,     0,     0,   410,     0,     0,
     147,   147,   147,     0,   170,    61,   170,   170,   168,   169,
      64,    65,    66,    67,    68,    69,    70,     0,     0,     0,
       0,     0,   787,     0,   468,     0,     0,     0,     0,   170,
       0,     0,     0,    61,     0,     0,   217,   218,    64,    65,
      66,    67,    68,    69,    70,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   597,   388,   440,   620,     0,     0,
       0,    72,     0,     0,   170,     0,     0,     0,     0,     0,
       0,   597,   737,   737,   170,   597,     0,     0,     0,     0,
     410,  1525,    74,     0,     0,     0,   114,     0,  1526,   114,
       0,     0,    77,    78,     0,     0,     0,   440,     0,     0,
     766,     0,   766,     0,     0,     0,     0,   170,     0,   170,
       0,     0,     0,     0,     0,    57,     0,     0,  1793,   322,
     322,  1531,     0,     0,     0,     0,     0,     0,   170,     0,
       0,     0,     0,     0,   170,     0,     0,     0,   322,     0,
     296,     0,     0,   114,   170,     0,    61,     0,  2064,   217,
     218,    64,    65,    66,    67,    68,    69,    70,  2071,   296,
       0,    13,    14,    15,    16,    17,     0,   114,     0,     0,
       0,     0,   597,     0,    72,     0,  2048,   298,     0,     0,
       0,     0,     0,   251,     0,     0,     0,   114,     0,     0,
       0,     0,  2057,     0,  1964,    74,   639,   410,   499,     0,
       0,     0,     0,     0,   322,    77,    78,     0,     0,     0,
     147,   410,     0,     0,     0,     0,     0,     0,     0,    57,
     322,     0,  1192,     0,   114,     0,     0,     0,  1531,     0,
     114,     0,   114,   607,     0,     0,   251,     0,   651,     0,
       0,   388,   656,     0,     0,     0,   318,   114,   350,     0,
      61,   662,   663,   448,     0,    64,    65,    66,    67,    68,
      69,    70,     0,     0,   414,  2016,   388,   388,     0,     0,
     640,     0,   440,     0,     0,     0,   114,   414,    72,     0,
     251,     0,     0,     0,     0,   641,     0,   388,   642,   643,
      64,    65,    66,    67,    68,    69,    70,     0,    73,    74,
       0,     0,     0,     0,     0,     0,   448,     0,     0,    77,
      78,     0,     0,     0,     0,     0,     0,   388,     0,    13,
      14,    15,    16,    17,   597,   448,     0,   114,     0,     0,
     114,     0,     0,     0,     0,     0,   298,     0,     0,   737,
       0,     0,     0,   251,     0,     0,     0,     0,   597,   468,
       0,     0,   787,     0,     0,     0,   766,     0,     0,     0,
     549,   597,     0,   766,     0,     0,     0,     0,   114,     0,
       0,  1354,     0,   251,     0,     0,     0,    57,     0,   251,
       0,     0,     0,     0,     0,     0,     0,   114,     0,     0,
       0,     0,    13,    14,    15,    16,    17,     0,   571,   298,
       0,     0,     0,     0,     0,   322,     0,   114,    61,   251,
     114,   217,   218,    64,    65,    66,    67,    68,    69,    70,
       0,     0,     0,     0,   114,     0,     0,     0,   114,     0,
       0,     0,     0,     0,     0,     0,    72,     0,    57,   298,
       0,     0,     0,     0,     0,   147,     0,     0,     0,     0,
      57,     0,   448,   410,     0,     0,   764,    74,     0,     0,
     604,   414,     0,     0,     0,     0,     0,    77,   765,    61,
       0,     0,   217,   218,    64,    65,    66,    67,    68,    69,
      70,    61,   410,     0,   217,   218,    64,    65,    66,    67,
      68,    69,    70,   448,     0,   414,     0,     0,     0,   245,
      82,     0,     0,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,   296,  1247,     0,  1277,    74,     0,
     147,   114,     0,  1247,     0,   414,     0,     0,   440,  1964,
      74,   251,     0,   499,     0,     0,     0,     0,     0,    61,
      77,    78,   168,   169,    64,    65,    66,    67,    68,    69,
      70,     0,  1247,     0,     0,   468,   440,     0,    61,     0,
     147,   168,   169,    64,    65,    66,    67,    68,    69,    70,
       0,     0,     0,     0,   388,   388,   388,   388,   388,   388,
     388,   388,   388,   388,   388,   388,   388,   388,   388,   388,
     388,   388,   388,     0,     0,     0,   249,     0,     0,     0,
     414,   414,     0,     0,     0,   251,   114,   269,     0,     0,
       0,     0,  1357,     0,     0,  1247,   597,     0,     0,   620,
       0,     0,     0,   322,   322,     0,     0,     0,     0,     0,
       0,  1359,     0,     0,     0,  1596,     0,   114,     0,     0,
       0,     0,   114,     0,     0,   251,   114,     0,   114,   249,
       0,     0,     0,     0,   388,     0,     0,     0,     0,   114,
       0,   114,   147,   147,   147,   147,   147,   147,   448,     0,
       0,     0,  1527,   302,     0,   350,     0,   114,   414,     0,
     251,     0,   306,   307,   308,   309,     0,     0,     0,     0,
       0,   410,   410,   249,     0,     0,     0,     0,     0,     0,
       0,   114,     0,     0,   251,     0,     0,     0,   549,     0,
       0,   251,     0,     0,   114,     0,   911,     0,     0,     0,
      61,   245,   114,   168,   169,    64,    65,    66,    67,    68,
      69,    70,     0,     0,     0,   414,     0,   114,   114,     0,
     414,   440,     0,     0,     0,     0,   414,     0,     0,   114,
     114,   114,     0,    61,     0,     0,   249,     0,    64,    65,
      66,    67,    68,    69,    70,   147,     0,     0,    61,   468,
       0,     0,   310,    64,    65,    66,    67,    68,    69,    70,
       0,    13,    14,    15,    16,    17,   249,     0,     0,     0,
     311,   388,   249,     0,     0,   468,    72,     0,     0,     0,
    1701,  1277,    74,  1247,     0,   414,     0,     0,     0,   388,
       0,     0,     0,  1711,   388,     0,  1020,    74,     0,     0,
     604,     0,   249,     0,     0,   388,     0,    77,    78,   414,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    57,
       0,     0,     0,     0,     0,     0,   414,     0,     0,     0,
    1739,  1670,     0,     0,     0,  1527,     0,   410,     0,     0,
       0,  1527,     0,  1527,     0,     0,     0,   388,   114,   114,
      61,     0,     0,   217,   218,    64,    65,    66,    67,    68,
      69,    70,     0,     0,     0,     0,     0,   114,     0,     0,
       0,   302,   147,     0,     0,     0,    61,     0,    72,   168,
     169,    64,    65,    66,    67,    68,    69,    70,     0,     0,
       0,     0,     0,     0,     0,   114,     0,     0,   219,    74,
       0,   468,     0,     0,   597,     0,   410,   509,     0,    77,
      78,     0,     0,     0,     0,     0,     0,   322,   251,     0,
     147,     0,     0,     0,   249,     0,   414,     0,     0,   251,
    1796,     0,   448,   114,  1806,  1361,     0,     0,     0,   114,
     414,     0,     0,     0,     0,   147,     0,  1818,     0,   114,
       0,  1194,   414,     0,   114,    61,     0,  1827,   346,   347,
      64,    65,    66,    67,    68,    69,    70,     0,     0,     0,
     322,   322,     0,     0,   388,     0,     0,     0,     0,   468,
       0,     0,     0,     0,  1247,  1670,  1670,     0,    57,  1247,
    1247,  1247,     0,     0,     0,     0,     0,     0,   249,     0,
    1527,   414,     0,  1527,     0,     0,    75,     0,     0,     0,
       0,   348,     0,     0,     0,     0,     0,     0,   249,    61,
     302,     0,   217,   218,    64,    65,    66,    67,    68,    69,
      70,     0,   410,  1873,     0,     0,     0,  1878,   249,     0,
       0,  1882,     0,     0,     0,    61,     0,    72,     0,   388,
      64,    65,    66,    67,    68,    69,    70,  1244,     0,   296,
       0,  1245,     0,  1246,   114,     0,  1912,   219,    74,     0,
       0,     0,     0,   249,     0,     0,     0,     0,    77,    78,
       0,   114,   114,     0,   388,   388,   388,     0,     0,     0,
     742,   388,   388,     0,    74,     0,     0,   249,     0,     0,
       0,  1670,     0,     0,   249,     0,   753,  1939,     0,   756,
    1527,  1942,     0,     0,     0,   388,     0,   448,     0,     0,
       0,    61,     0,  1955,   544,    63,    64,    65,    66,    67,
      68,    69,    70,     0,   114,     0,     0,   249,   269,     0,
       0,     0,     0,     0,     0,   147,     0,  1977,     0,  1979,
    1980,     0,   388,   388,    57,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    98,   509,     0,   151,     0,
     322,     0,  1989,   994,   114,  1247,     0,  1247,  1670,     0,
       0,     0,   414,     0,     0,    61,     0,     0,   147,     0,
      64,    65,    66,    67,    68,    69,    70,     0,     0,     0,
       0,    13,    14,    15,    16,    17,     0,  2012,     0,     0,
       0,   414,     0,    72,     0,   147,   147,  2017,  1965,   302,
       0,     0,    98,     0,     0,     0,     0,     0,   251,   114,
       0,     0,    61,    73,    74,   168,   169,    64,    65,    66,
      67,    68,    69,    70,    77,    78,   195,     0,     0,   114,
    2043,     0,  2017,   147,     0,     0,     0,   414,     0,    57,
       0,  1194,     0,     0,     0,     0,   258,     0,     0,     0,
       0,  2054,     0,  1423,     0,     0,     0,  2043,   193,     0,
    1965,  1965,     0,     0,     0,   414,     0,  2062,   579,   114,
      61,     0,     0,   217,   218,    64,    65,    66,    67,    68,
      69,    70,     0,   288,     0,     0,     0,     0,     0,    98,
       0,     0,     0,     0,     0,     0,     0,     0,    72,     0,
       0,  1965,     0,   108,     0,     0,   324,     0,     0,     0,
       0,   249,     0,   114,   114,     0,     0,     0,   295,    74,
       0,     0,   249,   193,   420,     0,     0,   114,   114,    77,
      78,     0,   114,   114,     0,   288,   446,     0,   193,     0,
       0,     0,    61,     0,   249,   217,   218,    64,    65,    66,
      67,    68,    69,    70,     0,   193,     0,     0,     0,     0,
     108,   114,   114,     0,   493,     0,     0,     0,   443,     0,
       0,   114,   114,   114,   114,   114,   114,     0,     0,     0,
     513,     0,   251,     0,     0,   518,   520,     0,     0,   195,
       0,     0,     0,   742,   742,     0,     0,     0,  1205,     0,
     414,   414,     0,  1011,   259,     0,  1014,     0,     0,     0,
       0,   540,     0,     0,   542,     0,   543,    57,     0,     0,
       0,   193,     0,     0,     0,     0,     0,   560,     0,     0,
     251,     0,   388,     0,     0,     0,     0,     0,     0,     0,
     572,    13,    14,    15,    16,    17,     0,   108,    61,     0,
     414,   217,   218,    64,    65,    66,    67,    68,    69,    70,
       0,     0,     0,     0,   328,     0,   595,   509,     0,   619,
       0,  1082,     0,     0,   114,  1086,    72,     0,   193,     0,
       0,     0,     0,   626,     0,     0,     0,   626,     0,     0,
       0,     0,     0,     0,   447,     0,   295,    74,   193,    57,
       0,     0,     0,     0,     0,     0,     0,    77,    78,     0,
       0,   659,     0,     0,    57,     0,     0,     0,     0,     0,
       0,   597,     0,     0,     0,     0,     0,     0,     0,     0,
      61,     0,     0,   217,   218,    64,    65,    66,    67,    68,
      69,    70,     0,     0,     0,    61,   114,   114,   217,   218,
      64,    65,    66,    67,    68,    69,    70,     0,    72,     0,
       0,     0,     0,     0,     0,     0,   414,     0,     0,   541,
       0,     0,     0,    72,     0,     0,     0,     0,  1525,    74,
     288,     0,   114,     0,   595,   108,   193,     0,     0,    77,
      78,     0,   597,  1525,    74,     0,     0,     0,     0,     0,
     251,   114,     0,     0,    77,    78,     0,   659,     0,     0,
       0,     0,     0,     0,     0,   113,   193,     0,     0,     0,
       0,   249,     0,     0,   596,     0,     0,   259,     0,     0,
       0,     0,     0,     0,   388,   414,     0,     0,     0,     0,
       0,   596,     0,     0,     0,   596,   114,     0,     0,   114,
       0,     0,     0,     0,   249,     0,     0,     0,   114,     0,
     742,     0,     0,     0,     0,   446,     0,     0,     0,     0,
       0,     0,   113,   388,   114,   366,     0,   367,     0,   368,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   114,
       0,   193,   193,     0,   114,   114,   853,   443,     0,   114,
     114,   520,     0,    61,     0,   864,     0,   560,    64,    65,
      66,    67,    68,    69,    70,  1244,   261,     0,   324,  1245,
      98,  1246,     0,     0,     0,   680,     0,     0,    75,   377,
       0,  1310,     0,     0,  1314,     0,   626,   886,     0,     0,
       0,     0,   596,     0,     0,     0,     0,     0,     0,   251,
     193,   897,    74,     0,     0,  1446,     0,     0,     0,   113,
     595,   414,     0,     0,     0,   906,     0,     0,   388,   443,
     388,     0,     0,   626,     0,     0,   332,    61,     0,     0,
     217,   218,    64,    65,    66,    67,    68,    69,    70,     0,
       0,     0,   193,    61,     0,   249,   217,   218,    64,    65,
      66,    67,    68,    69,    70,    72,   449,     0,   388,     0,
       0,     0,     0,   193,     0,     0,     0,     0,     0,     0,
       0,    72,     0,   447,     0,   764,    74,     0,     0,   604,
       0,     0,     0,     0,     0,     0,    77,   765,     0,     0,
     388,  1525,    74,   249,     0,     0,     0,     0,  1526,   606,
       0,     0,    77,    78,   328,     0,     0,     0,     0,     0,
       0,     0,     0,   259,   446,   108,     0,     0,     0,     0,
       0,     0,     0,     0,   114,     0,   447,     0,   108,     0,
       0,  1003,     0,     0,     0,     0,     0,     0,   388,     0,
       0,     0,     0,     0,   596,   447,   443,   113,     0,   114,
       0,     0,     0,     0,     0,   886,     0,     0,     0,     0,
    1027,     0,     0,     0,     0,     0,     0,   114,   596,     0,
     193,  1464,     0,     0,     0,     0,     0,   446,   446,     0,
    1473,   596,     0,     0,     0,     0,   598,   443,     0,   261,
       0,     0,     0,     0,   114,   114,   446,     0,   251,     0,
       0,     0,     0,   598,     0,     0,     0,   598,     0,   443,
     443,    61,     0,     0,   217,   218,    64,    65,    66,    67,
      68,    69,    70,     0,   853,     0,     0,     0,   443,     0,
       0,     0,   114,     0,     0,     0,     0,    61,     0,    72,
     217,   218,    64,    65,    66,    67,    68,    69,    70,     0,
       0,     0,     0,     0,     0,  1163,     0,     0,     0,   219,
      74,     0,   446,   249,     0,    72,     0,     0,   151,     0,
      77,    78,   447,     0,     0,     0,     0,   114,   626,     0,
       0,  1196,     0,   853,     0,  1964,    74,     0,  1202,   499,
       0,     0,     0,     0,   443,     0,    77,    78,     0,     0,
       0,   193,     0,     0,   598,     0,     0,     0,     0,     0,
       0,    61,     0,   447,   217,   218,    64,    65,    66,    67,
      68,    69,    70,     0,     0,     0,     0,     0,     0,     0,
     324,     0,     0,     0,     0,   328,   328,    61,     0,    72,
       0,     0,    64,    65,    66,    67,    68,    69,    70,  1244,
       0,     0,     0,  1245,   328,  1246,     0,     0,     0,   295,
      74,     0,   193,     0,     0,     0,     0,     0,     0,    61,
      77,    78,   189,    63,    64,    65,    66,    67,    68,    69,
      70,     0,   328,     0,     0,   449,    74,     0,     0,  1637,
       0,    61,     0,   853,   546,   547,    64,    65,    66,    67,
      68,    69,    70,     0,     0,     0,     0,     0,     0,     0,
     853,   853,   249,   108,     0,     0,   332,     0,    74,     0,
     328,   786,  1673,     0,     0,   261,     0,   113,     0,     0,
       0,     0,     0,     0,     0,     0,   596,     0,   449,   259,
     113,   328,    75,    61,     0,     0,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   118,   598,   449,   118,     0,
       0,    61,     0,   446,     0,     0,    64,    65,    66,    67,
      68,    69,    70,  1244,     0,     0,     0,  1245,     0,  1246,
     598,     0,     0,     0,     0,     0,     0,     0,   447,     0,
       0,     0,     0,   598,    75,   443,     0,     0,     0,     0,
       0,     0,     0,  1337,     0,     0,     0,     0,     0,     0,
      74,  1163,   118,  1639,    61,     0,     0,   168,   169,    64,
      65,    66,    67,    68,    69,    70,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   118,     0,     0,    61,
    1163,     0,   168,   169,    64,    65,    66,    67,    68,    69,
      70,   328,     0,     0,     0,     0,   118,     0,  1385,     0,
       0,     0,   457,     0,     0,     0,     0,     0,   328,   328,
       0,     0,     0,     0,     0,     0,  1673,  1673,     0,     0,
       0,     0,     0,     0,   449,     0,   595,   461,     0,     0,
       0,     0,     0,   118,     0,   518,     0,     0,     0,   118,
       0,   118,     0,     0,     0,     0,     0,     0,     0,     0,
     193,   249,     0,     0,   324,     0,     0,     0,   193,     0,
       0,   328,     0,     0,     0,   449,     0,     0,     0,     0,
       0,    61,     0,   118,   168,   169,    64,    65,    66,    67,
      68,    69,    70,     0,     0,   118,   193,   332,   332,     0,
     721,   722,   723,   724,   725,   726,   727,   728,   729,   730,
     731,     0,   853,   853,   204,     0,   332,     0,     0,   108,
       0,     0,     0,     0,   673,     0,   853,   853,     0,     0,
       0,   446,   446,     0,     0,     0,     0,     0,     0,     0,
       0,   732,  1673,     0,   332,     0,   118,     0,   108,   118,
       0,     0,     0,     0,   118,     0,     0,     0,     0,     0,
     853,   853,     0,   443,   443,     0,   259,     0,     0,     0,
    1337,  1337,  1337,   151,     0,   113,     0,     0,     0,     0,
       0,     0,   332,     0,     0,     0,     0,   118,     0,     0,
       0,     0,     0,     0,   596,     0,     0,     0,   598,  1555,
    1555,   261,     0,   332,     0,     0,   118,     0,     0,     0,
       0,     0,     0,     0,     0,  1928,     0,     0,     0,  1673,
       0,     0,   447,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   324,
     449,     0,     0,     0,     0,     0,     0,     0,     0,  1673,
       0,     0,     0,   833,   835,     0,     0,     0,     0,     0,
     328,   328,     0,   151,     0,     0,     0,     0,     0,     0,
     118,   193,     0,     0,   328,   328,     0,     0,     0,   328,
     328,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   332,   118,     0,     0,     0,   328,   328,
       0,  1673,  1673,     0,     0,     0,     0,     0,     0,     0,
     332,   332,     0,     0,     0,     0,     0,     0,     0,     0,
     118,     0,     0,     0,     0,   853,   853,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   108,   108,     0,
       0,     0,  1673,     0,     0,     0,     0,     0,     0,     0,
       0,  1687,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   853,     0,   332,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   193,   673,     0,
    1704,     0,     0,   673,     0,     0,     0,   447,     0,   673,
       0,   203,     0,     0,     0,     0,     0,   214,   215,   118,
     118,     0,     0,     0,     0,     0,     0,     0,   673,     0,
       0,   113,     0,     0,  1555,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   324,     0,     0,   151,     0,
       0,   277,     0,     0,     0,     0,     0,   853,     0,     0,
     113,   118,     0,     0,   991,   118,     0,   118,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   193,   261,     0,
     118,     0,     0,     0,     0,     0,     0,     0,   853,     0,
       0,     0,     0,   853,   853,     0,     0,     0,   446,   446,
       0,     0,     0,   328,   328,     0,   598,     0,     0,   119,
       0,     0,   119,     0,  1785,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     443,   443,     0,     0,   449,     0,     0,     0,     0,   328,
       0,   118,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   118,     0,   118,   118,   259,   118,
    1555,     0,     0,     0,     0,   118,   119,     0,   118,   118,
     118,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   332,   332,     0,     0,     0,     0,     0,     0,
     119,     0,   108,     0,     0,     0,   332,   332,     0,     0,
       0,   332,   332,   328,     0,     0,     0,     0,     0,     0,
     119,     0,     0,     0,     0,   328,     0,   568,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     332,   332,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   328,   119,   118,     0,
       0,   328,   328,   119,     0,   119,   328,   328,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   113,
     113,     0,     0,  1903,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   119,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   446,   119,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1555,     0,   108,   449,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     443,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1555,  1903,     0,     0,     0,     0,     0,
     119,     0,     0,   119,     0,     0,     0,     0,   119,     0,
       0,     0,     0,     0,   762,   118,   763,     0,     0,     0,
       0,     0,     0,     0,     0,   779,   780,     0,   118,   118,
       0,  1555,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   119,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  2005,
     119,     0,     0,     0,     0,   332,   332,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   853,     0,     0,     0,
       0,   596,     0,     0,     0,     0,   123,     0,     0,   123,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   332,     0,     0,     0,     0,   328,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     261,     0,     0,     0,   108,     0,     0,     0,     0,     0,
       0,     0,     0,   863,   119,     0,     0,     0,     0,     0,
       0,     0,     0,   123,     0,     0,     0,     0,     0,     0,
       0,   108,   596,     0,   113,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   332,     0,   123,   119,     0,
       0,     0,     0,     0,     0,     0,     0,   332,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   123,     0,   108,
       0,     0,     0,     0,   119,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   332,     0,
       0,     0,     0,   332,   332,     0,     0,     0,   332,   332,
       0,     0,     0,     0,   123,     0,     0,     0,     0,     0,
     123,     0,   123,     0,   328,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   118,   123,     0,     0,     0,     0,     0,
       0,   118,     0,   119,   119,     0,   123,     0,     0,     0,
     113,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     118,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   119,     0,     0,   118,   119,
       0,   119,     0,     0,     0,     0,     0,   123,     0,     0,
     123,     0,     0,     0,   119,   123,     0,     0,   118,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1546,     0,  1042,  1549,  1563,     0,     0,     0,     0,  1570,
       0,     0,     0,  1574,     0,  1576,     0,     0,   123,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   118,     0,
       0,     0,     0,     0,     0,     0,     0,   123,     0,     0,
       0,     0,     0,   598,     0,   119,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   119,     0,
     119,   119,     0,   119,     0,     0,     0,     0,   332,   119,
       0,     0,   119,   119,   119,     0,     0,     0,     0,  1110,
    1111,     0,     0,     0,     0,     0,   113,     0,     0,     0,
    1169,  1170,  1171,     0,     0,  1173,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   123,     0,   113,   598,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     118,   118,   118,   118,   118,   118,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   123,     0,     0,     0,     0,
       0,   113,   119,     0,     0,  1664,     0,     0,     0,   118,
     118,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   123,     0,     0,     0,  1240,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     1,  1697,     0,   145,
       0,     0,     0,     0,     0,     0,   332,     0,     0,  1703,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1717,  1719,     0,     0,     0,     0,
       0,  1261,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   118,     0,     0,     0,     0,  1549,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     123,   123,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   192,     0,     0,     0,     0,     0,  1285,   119,
       0,     0,     0,     0,     0,     0,     0,  1289,  1290,  1291,
    1292,     0,   119,   119,     0,  1297,  1298,     0,     0,     0,
       0,     0,   123,     0,     0,  1306,   123,     0,   123,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   123,     0,     0,     0,     0,  1320,     0,  1321,     0,
       0,   283,     0,     0,     0,   118,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1804,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1808,     0,
     118,  1813,  1817,     0,  1563,     0,     0,     0,     0,  1823,
       0,  1377,   123,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   123,     0,   123,   123,     0,
     123,     0,     0,     0,   118,     0,   123,  1391,     0,   123,
     123,   123,     0,     0,  1395,     0,  1397,  1399,   118,     0,
       0,     0,     0,     0,     0,   283,  1406,     0,  1407,     0,
    1408,     0,  1410,     0,     0,     0,     0,  1418,     0,     0,
     521,     0,     0,   118,     0,     0,     0,     0,     0,     0,
     283,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     283,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1888,   552,   556,     0,     0,  1893,  1895,
       0,   563,   564,     0,     0,     0,     0,     0,     0,   123,
       0,     0,     0,     0,     0,     0,     0,   574,  1461,     0,
       0,     0,     0,     0,     0,  1468,  1469,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   593,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   119,     0,  1492,
     118,     0,     0,     0,     0,   119,  1497,     0,     0,  1945,
    1498,     0,  1947,  1949,     0,     0,     0,  1952,  1954,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   119,     0,     0,     0,     0,     0,
     214,     0,     0,   679,     0,     0,     0,     0,     0,     0,
       0,     0,   119,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   119,     0,   720,     0,   123,     0,     0,     0,
    1591,     0,     0,     0,     0,  1996,  1998,  2000,     0,   123,
     123,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     758,     0,     0,     0,   761,     0,  2013,     0,     0,     0,
       0,     0,   119,     0,  1611,     0,     0,     0,     0,  2023,
    2025,  2027,  1616,   783,  1618,     0,     0,   784,   785,     0,
       0,   788,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   802,   803,   804,   805,
       0,     0,     0,     0,     0,     0,   118,     0,     0,     0,
       0,     0,     0,     0,     0,   827,     0,     0,     0,     0,
       0,  1645,  1646,   830,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   118,     0,     0,  1651,  1652,     0,  1653,
       0,     0,     0,     0,     0,  1738,     0,     0,  1657,     0,
       0,   283,     0,     0,     0,     0,     0,     0,  1662,  1663,
       0,  1353,     0,     0,   119,   119,   119,   119,   119,   119,
       0,   118,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   868,     0,     0,     0,     0,     0,     0,   552,
       0,     0,   361,   119,   119,   874,   362,     0,   363,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1116,     0,   364,    -2,     0,  1118,   889,
     894,  1119,  1120,  1121,  1122,  1123,  1124,  1125,  1126,  1127,
    1128,  1129,  1130,  -330,  1131,  1132,  1133,  1134,  1135,     0,
    1136,     0,   365,   366,     0,   463,     0,   368,  1137,  1138,
      64,    65,    66,    67,    68,    69,    70,   369,   370,   358,
    1139,   371,   372,   373,     0,   374,   375,   119,     0,  1746,
    1747,     0,     0,    72,   123,     0,   165,     0,     0,     0,
    1754,     0,   123,   936,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   376,     0,     0,    75,   377,     0,     0,
       0,   279,   165,   378,    77,    78,   379,   380,   381,   382,
       0,   123,     0,     0,     0,  1777,  1778,     0,  -179,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   123,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   165,   123,
       0,     0,   999,     0,     0,     0,     0,     0,     0,   119,
       0,   165,     0,   165,     0,     0,     0,  1016,     0,     0,
       0,  1017,     0,     0,     0,     0,     0,     0,     0,     0,
     889,     0,     0,     0,     0,     0,     0,     0,     0,   123,
     247,     0,     0,   352,   119,     0,     0,     0,     0,     0,
    1842,   268,  1057,   271,     0,   273,     0,     0,     0,     0,
     352,  1066,     0,     0,     0,     0,     0,  1069,     0,  1851,
       0,     0,  1852,  1853,     0,     0,     0,     0,   119,  1855,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   119,   247,     0,   271,   273,     0,   165,     0,
       0,     0,   165,     0,     1,   165,   165,     0,     0,   165,
       0,     1,   165,   165,     0,     0,     0,   119,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   247,     1,     0,
       0,   123,   123,   123,   123,   123,   123,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     123,   123,     0,     0,   165,     0,     0,   165,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1220,     0,     0,     0,     0,     0,   165,   165,
     247,     0,   271,   273,   119,     0,  1963,     0,     0,     0,
       0,     0,     0,     0,   165,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     247,     0,     0,     0,     0,     0,   247,     0,     0,     0,
       0,     0,     0,     0,   123,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   247,  2002,     0,     0,
       0,     0,   622,     0,   273,  1268,     0,     0,     0,  1269,
       0,     0,     0,     0,     0,     0,   889,     0,     0,     0,
       0,  2019,     0,     0,     0,     0,  1282,     0,     0,     0,
       0,     0,     0,  1283,     0,     0,  2028,     0,   165,     0,
       0,     0,  1287,     0,  1288,     0,     0,     0,     0,     0,
       0,  2041,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   123,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1316,     0,     0,     0,
    1317,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     119,     0,   247,   352,   145,     0,     0,     1,     0,     0,
       0,   123,     0,     0,     0,   165,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   119,   247,     0,
     622,   273,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   123,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   123,
       0,     0,     0,     0,     0,   119,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   247,     0,     0,     0,     0,
       0,     0,     0,     0,   123,     0,     0,     0,     0,   352,
       0,     0,     0,     0,     0,   247,  1405,     0,     0,     0,
     247,     0,   247,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1429,   247,     0,   247,   247,     0,     0,     0,     0,
     165,   165,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   247,   165,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   247,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   339,
       0,   123,     0,     0,     0,     0,     0,   247,     0,   622,
     273,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   436,   339,
       0,   247,   622,     0,     0,     0,     0,     0,   247,     0,
       0,     0,     0,     0,  1501,     0,     0,     0,  1502,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     502,     0,     0,     0,     0,     0,     0,   502,     0,     0,
       0,   247,   268,     0,     0,     0,     0,     0,  1537,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   165,
     165,     0,     0,     0,     0,   165,     0,     0,     0,     0,
       0,     0,     0,     0,   171,   174,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   165,     0,     0,   165,
     165,     0,   165,     0,   165,   165,     0,     0,  1601,     0,
       0,  1604,     0,     0,     0,   502,     0,     0,     0,   212,
       0,     0,     0,     0,     0,     0,     0,  1613,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   123,   200,   339,
     608,     0,     0,   165,     0,     0,     0,   165,     0,     0,
       0,     0,     0,     0,   255,     0,     0,     0,     0,     0,
     629,     0,     0,     0,   123,     0,     0,     0,     0,   290,
       0,     0,   291,     0,     0,     0,     0,     0,  1644,     0,
       0,     0,     0,     0,     0,   312,     0,  1649,     0,     0,
       0,  1650,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   123,   200,     0,  1654,  1655,   303,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   344,     0,
       0,     0,     0,     0,   165,     0,     0,     0,     0,     0,
     502,     0,     0,     0,     0,   200,     0,     0,     0,   478,
       0,     0,     0,     0,     0,   247,   502,   754,   456,   502,
     757,   460,     0,     0,     0,     0,   247,   339,     0,     0,
       0,   608,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   247,     0,
       0,     0,     0,     0,   529,     0,     0,     0,     0,   247,
       0,     0,     0,     0,   171,     0,     0,     0,   247,     0,
       0,   200,   502,     0,     0,   171,   502,     0,     0,     0,
       0,     0,     0,     0,   255,     0,     0,     0,     0,     0,
       0,  1742,  1743,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   575,     0,     0,     0,     0,     0,   339,   578,
     580,     0,     0,     0,   587,     0,     0,  1864,     0,     0,
     460,     0,     0,     0,     0,   165,     0,     0,   200,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   633,   601,     0,
     618,     0,   312,     0,     0,   312,     0,     0,   502,     0,
       0,   339,     0,     0,   361,   247,   165,     0,   362,     0,
     363,     0,     0,   165,     0,     0,   165,     0,     0,   884,
     339,     0,     0,     0,     0,     0,     0,   364,     0,   247,
     608,     0,     0,     0,   608,     0,     0,     0,     0,     0,
       0,   902,   677,   339,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   365,   366,  1829,   367,     0,   368,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   369,
     370,   358,     0,   371,   372,   373,   200,   374,   375,     0,
    1604,     0,   212,     0,   163,    72,     0,     0,     0,     0,
       0,     0,     0,     0,   781,   782,     0,     0,  1854,     0,
       0,     0,     0,     0,     0,   376,   601,     0,    75,   377,
       0,     0,   778,     0,     0,   378,    77,    78,   379,   380,
     381,   382,     0,     0,     0,  1872,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   165,     0,     0,
       0,     0,     0,     0,     0,   165,   165,   339,     0,     0,
       0,  1899,     0,     0,  1900,     0,   275,     0,     0,     0,
       0,     0,     0,   502,   502,   247,     0,     0,     0,   281,
       0,   282,     0,   502,  1012,     0,   502,  1015,     0,     0,
       0,   200,   200,     0,     0,     0,     0,   456,   339,     0,
       0,   608,     0,   608,   608,     0,     0,     0,   247,     0,
     608,     0,     0,   165,   247,     0,     0,     0,     0,     0,
     339,   339,   165,     0,     0,   165,     0,   165,   165,     0,
       0,     0,     0,     0,     0,   312,     0,     0,     0,   339,
       0,     0,     0,   502,     0,     0,     0,   502,     0,     0,
     344,   502,  1083,     0,     0,   502,  1087,     0,     0,     0,
       0,     0,  1982,  1090,     0,     0,     0,     0,   165,   456,
       0,   888,     0,   503,   504,     0,     0,   508,     0,     0,
     511,   512,     0,     0,   633,     0,     0,     0,     0,     0,
       0,     0,   601,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   339,   502,     0,     0,     0,
       0,     0,     0,   200,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   677,     0,   677,   677,
       0,   677,     0,     0,   608,   165,     0,   677,     0,     0,
     677,   677,   677,     0,     0,     0,     0,     0,     0,   247,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   590,   591,     0,     0,
       0,     0,     0,   339,     0,     0,     0,     0,     0,     0,
       0,     0,   623,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   456,   247,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   165,
     200,     0,     0,     0,     0,     0,     0,  1044,     0,     0,
       0,     0,     0,     0,  1056,     0,     0,   456,     0,     0,
     502,     0,     0,     0,     0,     0,     0,     0,   165,     0,
       0,     0,     0,     0,     0,     0,     0,   608,   608,   456,
     456,     0,     0,     0,   608,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   165,     0,   752,     0,   456,     0,
     165,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   339,     0,     0,  1114,
       0,   502,  1311,     0,   502,  1315,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   633,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   823,   456,     0,     0,     0,     0,   165,
       0,   200,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1209,   778,     0,   412,   633,     0,   247,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   441,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     469,     0,   469,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   344,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   165,   165,     0,     0,   339,
       0,     0,     0,   352,     0,   608,  1414,   165,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   339,   900,   901,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   908,     0,     0,     0,     0,     0,     0,   569,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   247,
       0,   502,  1465,     0,     0,     0,     0,     0,     0,     0,
     502,  1474,     0,   608,     0,     0,   247,     0,     0,     0,
       0,     0,     0,     0,   339,   339,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   165,    13,    14,    15,
      16,    17,     0,     0,    19,   456,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,  1358,  1360,  1362,     0,    45,    46,     0,     0,     0,
       0,     0,     0,     0,     0,   677,     0,  1005,  1006,     0,
       0,     0,     0,  1010,     0,    57,     0,   247,     0,     0,
       0,  1381,     0,     0,     0,     0,     0,   165,     0,     0,
       0,     0,     0,     0,  1031,     0,  1114,  1034,  1035,     0,
    1038,     0,  1040,  1041,     0,     0,   658,  1201,     0,     0,
       0,     0,     0,     0,    13,    14,    15,    16,    17,   255,
       0,     0,   339,     0,     0,     0,     0,     0,     0,     0,
       0,   469,     0,     0,   633,     0,     0,   469,     0,     0,
     200,  1081,   799,     0,     0,  1085,     0,     0,   601,     0,
     361,     0,     0,     0,   362,     0,   363,     0,     0,     0,
       0,   -16,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    57,   364,     0,     0,   344,     0,     0,     0,
     677,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   165,     0,   247,     0,     0,     0,     0,
     365,   366,     0,   367,     0,   368,    62,    63,    64,    65,
      66,    67,    68,    69,    70,   369,   370,   358,     0,   371,
     372,   373,  1203,   374,   375,     0,     0,     0,     0,   867,
       0,    72,   502,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   456,   456,     0,     0,     0,   502,     0,
       0,   376,     0,     0,    75,   377,     0,     0,   441,     0,
       0,   378,   439,    78,   379,   380,   381,   382,     0,  1536,
       0,   896,  1538,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   677,   677,   677,     0,   677,   677,     0,     0,
       0,     0,     0,   460,     0,     0,     0,   247,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   339,     0,
       0,     0,   930,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   255,     0,   799,   950,     0,     0,   952,     0,   954,
       0,     0,     0,  1203,     0,   963,     0,   968,   963,     0,
       0,   344,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   339,   339,     0,   361,     0,     0,     0,   362,     0,
     363,     0,     0,     0,     0,   996,   502,   502,     0,     0,
       0,     0,     0,     0,  1302,     0,     0,   364,   998,     0,
       0,  1309,   502,     0,  1313,     0,     0,     0,     0,  1007,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   441,   365,   366,   996,   367,     0,   368,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   369,
     370,   358,     0,   371,   372,   373,     0,   374,   375,     0,
       0,     0,     0,  1060,     0,    72,   469,     0,     0,     0,
       0,     0,     0,  1693,     0,     0,     0,     0,     0,  1557,
    1558,  1559,     0,     0,     0,   376,  1718,   200,    75,   377,
       0,     0,     0,     0,     0,   378,    77,    78,   379,   380,
     381,   382,  1091,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   502,     0,     0,     0,     0,     0,     0,     0,
     502,   255,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1412,     0,     0,     0,     0,
       0,     0,     0,  1421,  1422,     0,     0,     0,     0,     0,
     412,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     633,  1193,  1195,     0,     0,     0,     0,   344,     0,   441,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   339,     0,     0,   361,   502,  1929,     0,   362,   502,
     363,     0,     0,     0,     0,   677,     0,     0,   963,     0,
       0,  1463,     0,     0,     0,     0,     0,   364,     0,     0,
    1472,   996,     0,  1476,     0,  1479,  1480,     0,     0,  1233,
     456,   456,     0,     0,     0,     0,   963,     0,     0,   502,
       0,     0,     0,     0,   365,   366,     0,   463,     0,   368,
      62,    63,    64,    65,    66,    67,    68,    69,    70,   369,
     370,   358,     0,   371,   372,   373,  1506,   374,   375,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
     255,     0,   469,     0,     0,     0,     0,     0,     0,     0,
       0,  1840,     0,     0,     0,   376,    74,     0,   464,   465,
     633,   502,   502,   466,     0,   378,    77,    78,   379,   380,
     381,   382,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1598,     0,     0,     0,     0,     0,     0,
       0,     0,   502,     0,     0,     0,     0,     0,     0,   469,
       0,  1301,  1871,  1304,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,
      -2,     0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,
      -2,    -2,    -2,     0,    -2,   677,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,     0,     0,  1476,    -2,  1369,
    1369,    -2,     0,     0,     0,     0,    -2,    -2,     0,     0,
     456,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1659,     0,     0,     0,
       0,    -2,     0,     0,     0,    -2,    -2,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   677,     0,     0,   460,
      -2,  1409,     0,     0,     0,     0,     0,  1419,     0,     0,
       0,     0,     0,     0,     0,     0,   469,     0,  2042,    -2,
       0,     0,     0,    -2,    -2,   441,     0,     0,     0,     0,
       0,    -2,    -2,     0,  1353,     0,     0,     0,     0,     0,
       0,     0,   469,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   963,     0,     0,
     799,     0,     0,     0,     0,   361,     0,  1740,     0,   362,
       0,   363,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1116,     0,   364,    -2,
       0,  1118,  -238,  -238,  1119,  1120,  1121,  1122,  1123,  1124,
    1125,  1126,  1127,  1128,  1129,  1130,  -330,  1131,  1132,  1133,
    1134,  1135,  1500,  1136,     0,   365,   366,     0,   463,     0,
     368,  1137,  1138,    64,    65,    66,    67,    68,    69,    70,
     369,   370,   358,  1139,   371,   372,   373,     0,   374,   375,
       0,     0,     0,  1789,  1790,     0,    72,     0,     0,     0,
     963,     0,     0,     0,     0,  1794,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -238,   376,     0,   469,    75,
     377,   799,     0,     0,   279,     0,   378,    77,    78,   379,
     380,   381,   382,     0,     0,  2042,     0,     0,     0,     0,
       0,  -179,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1353,     0,     0,     0,     0,     0,     0,     0,     0,
     950,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1614,  1615,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   361,     0,     0,     0,   362,     0,   363,     0,
       0,     0,     0,     0,     0,     0,   469,     0,   799,     0,
       0,     0,     0,  1116,  1862,   364,    -2,     0,  1118,  -239,
    -239,  1119,  1120,  1121,  1122,  1123,  1124,  1125,  1126,  1127,
    1128,  1129,  1130,  -330,  1131,  1132,  1133,  1134,  1135,     0,
    1136,     0,   365,   366,     0,   463,     0,   368,  1137,  1138,
      64,    65,    66,    67,    68,    69,    70,   369,   370,   358,
    1139,   371,   372,   373,   361,   374,   375,     0,   362,     0,
     363,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1927,   412,   364,     0,     0,
       0,  1686,  -239,   376,     0,     0,    75,   377,     0,     0,
       0,   279,     0,   378,    77,    78,   379,   380,   381,   382,
       0,     0,     0,     0,   365,   366,     0,   367,  -179,   368,
    1811,    63,    64,    65,    66,    67,    68,    69,    70,   369,
     370,   358,     0,   371,   372,   373,     0,   374,   375,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,  1731,     0,     0,     0,     0,     0,     0,     0,  1557,
    1558,  1559,     0,     0,     0,   376,  1812,     0,    75,   377,
       0,     0,     0,     0,     0,   378,    77,    78,   379,   380,
     381,   382,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1755,     0,     0,  1757,     4,   243,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
    1115,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,     0,
       0,   361,     0,    45,    46,   362,     0,   363,    47,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
      56,     0,  1116,    57,  1117,    -2,     0,  1118,     0,     0,
    1119,  1120,  1121,  1122,  1123,  1124,  1125,  1126,  1127,  1128,
    1129,  1130,  -330,  1131,  1132,  1133,  1134,  1135,     0,  1136,
       0,   365,   366,    60,   463,     0,   368,  1137,  1138,    64,
      65,    66,    67,    68,    69,    70,   369,   370,   358,  1139,
     371,   372,   373,     0,   374,   375,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    -3,   376,     0,     0,    75,   408,     0,     0,     0,
     279,     0,   378,    77,    78,   379,   380,   381,   382,     0,
       0,     0,     0,     0,     0,     0,     0,  -179,     0,     0,
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
    1130,  -330,  1131,  1132,  1133,  1134,  1135,     0,  1136,     0,
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
      78,   379,   380,   381,   382,  1898,     0,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,     0,    -2,     0,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,     0,    -2,    -2,    -2,     0,    -2,     0,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,     0,
       0,    -2,     0,     0,    -2,  1416,     0,     0,     0,    -2,
      -2,     0,    13,    14,    15,    16,    17,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    -2,     0,     0,     0,    -2,    -2,
       0,     0,     0,     0,     0,     0,     0,     0,   361,     0,
       0,     0,   362,     0,   363,     0,     0,     0,     0,     0,
       0,     0,     0,    -2,     0,     0,     0,     0,     0,     0,
      57,   364,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    -2,     0,     0,     0,    -2,    -2,     0,     0,
       0,     0,     0,     0,    -2,    -2,     0,     0,   365,   366,
       0,   367,     0,   368,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   369,   370,   358,     0,   371,   372,   373,
       0,   374,   375,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   376,
       0,     0,    75,   377,     0,     0,     0,     0,     0,   378,
    1417,    78,   379,   380,   381,   382,     4,     5,     6,     7,
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
      37,    38,    39,    40,    41,    42,    43,    44,  -451,  -451,
       0,  -451,    45,    46,     0,  -451,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    13,    14,    15,    16,
      17,     0,    57,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
       0,     0,     0,    61,    45,    46,    62,    63,    64,    65,
      66,    67,    68,    69,    70,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    57,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    73,    74,     0,    75,   244,     0,     0,     0,  -762,
       0,     0,    77,    78,     4,   243,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,     0,     0,     0,
       0,    45,    46,     0,     0,     0,    47,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,    56,     0,
       0,    57,     0,     0,     0,     0,  -383,  -383,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    60,     0,     0,     0,    62,    63,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -383,
       0,     0,     0,    75,    76,     0,     0,     0,     0,     0,
       0,    77,    78,     4,   243,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,     0,     0,
      45,    46,     0,     0,     0,    47,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,    56,     0,     0,
      57,     0,     0,     0,     0,  -384,  -384,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      60,     0,     0,     0,    62,    63,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -384,     0,
       0,     0,    75,    76,     0,     0,     0,     0,     0,     0,
      77,    78,   242,   243,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,     0,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,  -451,  -451,     0,  -451,    45,
      46,     0,  -451,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    57,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      61,     0,     0,    62,    63,    64,    65,    66,    67,    68,
      69,    70,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    73,    74,
       0,    75,   244,     0,     0,  1329,     0,     0,     0,    77,
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
    1332,     0,     0,     0,    75,   926,     0,     0,  1329,     0,
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
       0,     0,     0,  1512,     0,     0,     0,    75,   926,     0,
       0,  1329,     0,     0,     0,    77,    78,  1330,     0,     0,
      13,    14,    15,    16,    17,    18,     0,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,     0,     0,    45,    46,
       0,     0,     0,    47,    48,    49,    50,    51,    52,    53,
      54,     0,     0,     0,     0,     0,     0,     0,    57,  1331,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    60,     0,
       0,     0,    62,    63,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1513,     0,     0,     0,
      75,   926,     0,     0,  1329,     0,     0,     0,    77,    78,
    1330,     0,     0,    13,    14,    15,    16,    17,    18,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,     0,     0,     0,
       0,    45,    46,     0,     0,     0,    47,    48,    49,    50,
      51,    52,    53,    54,     0,     0,     0,     0,     0,     0,
       0,    57,  1331,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    60,     0,     0,     0,    62,    63,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1514,
       0,     0,     0,    75,   926,     0,     0,     0,     0,     0,
       0,    77,    78,   242,   243,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,     0,     0,    19,
       0,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,  -451,  -451,     0,  -451,
      45,    46,     0,  -451,     0,     0,     0,     0,     0,     0,
       0,     0,    13,    14,    15,    16,    17,     0,     0,    19,
      57,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,     0,     0,     0,     0,
      45,    46,     0,     0,    62,    63,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      57,     0,     0,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   242,     0,     0,     0,
       0,   658,    75,   244,     0,    13,    14,    15,    16,    17,
      77,    78,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,  -451,
    -451,     0,  -451,    45,    46,     0,  -451,     0,     0,     0,
       0,     0,     0,     0,     0,    13,    14,    15,    16,    17,
       0,     0,    19,    57,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,  -450,
    -450,     0,  -450,    45,    46,     0,  -450,    62,    63,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    57,     0,     0,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    75,   301,     0,     0,     0,
       0,     0,     0,    77,    78,   243,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,     0,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,  -451,  -451,     0,
    -451,    45,    46,     0,  -451,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    57,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    61,     0,     0,    62,    63,    64,    65,    66,
      67,    68,    69,    70,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      73,    74,     0,    75,   244,     0,     0,     0,  -766,     0,
       0,    77,    78,   243,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,     0,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,  -451,  -451,     0,  -451,    45,
      46,     0,  -451,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    57,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      61,     0,     0,    62,    63,    64,    65,    66,    67,    68,
      69,    70,     0,     0,     0,     0,     0,     0,     0,  1353,
       0,     0,     0,     0,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    73,    74,
     361,    75,   244,     0,   362,     0,   363,     0,     0,    77,
      78,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1116,     0,   364,     0,     0,  1118,  1797,  1798,  1119,
    1120,  1121,  1122,  1123,  1124,  1125,  1126,  1127,  1128,  1129,
    1130,  -330,  1131,  1132,  1133,  1134,  1135,     0,  1136,     0,
     365,   366,     0,   463,     0,   368,  1137,  1138,    64,    65,
      66,    67,    68,    69,    70,   369,   370,   358,  1139,   371,
     372,   373,     0,   374,   375,     0,     0,     0,     0,     0,
       0,    72,     0,     0,     0,     0,     0,  1353,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   376,     0,     0,    75,   377,     0,     0,     0,   279,
       0,   378,    77,    78,   379,   380,   381,   382,   361,     0,
       0,     0,   362,     0,   363,     0,  -179,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1116,
       0,   364,     0,     0,  1118,     0,     0,  1119,  1120,  1121,
    1122,  1123,  1124,  1125,  1126,  1127,  1128,  1129,  1130,  -330,
    1131,  1132,  1133,  1134,  1135,     0,  1136,     0,   365,   366,
       0,   463,     0,   368,  1137,  1138,    64,    65,    66,    67,
      68,    69,    70,   369,   370,   358,  1139,   371,   372,   373,
       0,   374,   375,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   376,
       0,     0,    75,   377,     0,     0,     0,   279,     0,   378,
      77,    78,   379,   380,   381,   382,     0,     0,     0,     0,
       0,     0,     0,     0,  -179,   243,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,     0,     0,     0,
       0,    45,    46,     0,     0,     0,   319,    48,    49,    50,
      51,    52,    53,    54,     0,    13,    14,    15,    16,    17,
      18,    57,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,     0,
       0,     0,     0,    45,    46,    62,    63,     0,    47,    48,
      49,    50,    51,    52,    53,    54,     0,     0,     0,     0,
       0,     0,     0,    57,     0,     0,     0,     0,     0,     0,
      72,     0,  1051,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -634,    75,   321,     0,     0,    62,    63,     0,
       0,    77,    78,   243,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    75,     0,     0,     0,    45,
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
     320,    75,   321,     0,     0,    62,    63,     0,     0,    77,
      78,   243,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    75,     0,     0,     0,    45,    46,     0,
       0,     0,   319,    48,    49,    50,    51,    52,    53,    54,
       0,     0,     0,     0,     0,     0,     0,    57,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    62,    63,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    72,     0,  1773,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    75,
     321,     0,     0,     0,     0,     0,     0,    77,    78,   243,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,     0,     0,     0,     0,    45,    46,     0,     0,     0,
     319,    48,    49,    50,    51,    52,    53,    54,     0,     0,
       0,     0,     0,     0,     0,    57,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    62,
      63,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    72,     0,  1775,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    75,   321,     0,
       0,     0,     0,     0,     0,    77,    78,   243,     6,     7,
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
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    75,   301,     0,     0,     0,
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
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    75,   321,     0,     0,     0,     0,     0,
       0,    77,    78,   243,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,     0,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,  -451,  -451,     0,  -451,    45,
      46,     0,  -451,     0,     0,     0,     0,     0,     0,     0,
       0,    13,    14,    15,    16,    17,     0,     0,    19,    57,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,  -451,  -451,     0,  -451,    45,
      46,     0,  -451,    62,    63,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    57,
       0,     0,     0,     0,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    75,   244,     0,     0,     0,     0,     0,     0,    77,
      78,    13,    14,    15,    16,    17,    18,   664,    19,   665,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,     0,     0,   361,     0,    45,
      46,   362,     0,   363,    47,    48,    49,    50,    51,    52,
      53,    54,     0,     0,     0,     0,     0,     0,     0,    57,
     364,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   666,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   365,   366,     0,
     367,     0,   368,    62,    63,    64,    65,    66,    67,    68,
      69,    70,   369,   370,   358,     0,   371,   372,   373,     0,
     374,   375,     0,     0,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   376,     0,
       0,    75,   667,     0,     0,     0,   279,     0,   378,    77,
      78,   668,   669,   381,   382,    13,    14,    15,    16,    17,
      18,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,     0,
       0,   361,     0,    45,    46,   362,     0,   363,    47,    48,
      49,    50,    51,    52,    53,    54,     0,     0,     0,     0,
       0,     0,     0,    57,   364,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   365,   366,     0,   367,     0,   368,    62,    63,    64,
      65,    66,    67,    68,    69,    70,   369,   370,   358,     0,
     371,   372,   373,     0,   374,   375,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   376,     0,   407,    75,   408,     0,     0,     0,
       0,     0,   378,    77,    78,   379,   380,   381,   382,    13,
      14,    15,    16,    17,    18,     0,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,     0,     0,   361,     0,    45,    46,   362,
       0,   363,    47,    48,    49,    50,    51,    52,    53,    54,
       0,     0,     0,     0,     0,     0,     0,    57,   364,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   365,   366,     0,   367,     0,
     368,    62,    63,    64,    65,    66,    67,    68,    69,    70,
     369,   370,   358,     0,   371,   372,   373,     0,   374,   375,
       0,     0,     0,     0,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   376,     0,     0,    75,
     667,     0,     0,     0,   279,     0,   378,    77,    78,   379,
     380,   381,   382,    13,    14,    15,    16,    17,    18,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,     0,     0,   361,
       0,    45,    46,   362,     0,   363,    47,    48,    49,    50,
      51,    52,    53,    54,     0,     0,     0,     0,     0,     0,
       0,    57,   364,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   365,
     366,     0,   367,     0,   368,    62,    63,    64,    65,    66,
      67,    68,    69,    70,   369,   370,   358,     0,   371,   372,
     373,     0,   374,   375,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     376,     0,     0,    75,   408,     0,     0,     0,     0,     0,
     378,    77,    78,   379,   380,   381,   382,    13,    14,    15,
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
       0,     0,     0,     0,   376,     0,     0,    75,   438,     0,
       0,     0,     0,     0,   378,    77,    78,   379,   380,   381,
     382,    13,    14,    15,    16,    17,    18,     0,    19,     0,
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
       0,    75,   377,     0,     0,     0,     0,     0,   378,    77,
      78,   379,   380,   381,   382,    13,    14,    15,    16,    17,
      18,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,     0,
       0,     0,     0,    45,    46,     0,     0,     0,    47,    48,
      49,    50,    51,    52,    53,    54,     0,     0,     0,     0,
       0,     0,     0,    57,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    61,     0,     0,    62,    63,    64,
      65,    66,    67,    68,    69,    70,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    73,    74,     0,    75,    76,     0,     0,     0,
    -764,     0,     0,    77,    78,    13,    14,    15,    16,    17,
      18,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,     0,
       0,     0,     0,    45,    46,     0,     0,     0,    47,    48,
      49,    50,    51,    52,    53,    54,     0,     0,     0,     0,
       0,     0,     0,    57,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    61,     0,     0,    62,    63,    64,
      65,    66,    67,    68,    69,    70,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    73,    74,     0,    75,    76,     0,     0,     0,
       0,     0,     0,    77,    78,    13,    14,    15,    16,    17,
      18,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,     0,
       0,     0,     0,    45,    46,     0,     0,     0,    47,    48,
      49,    50,    51,    52,    53,    54,     0,     0,     0,     0,
       0,     0,     0,    57,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    61,     0,     0,    62,    63,    64,
      65,    66,    67,    68,    69,    70,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    75,    76,     0,    13,    14,
      15,    16,    17,    77,    78,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,  -451,  -451,     0,  -451,    45,    46,     0,  -451,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    57,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    61,     0,     0,
      62,    63,    64,    65,    66,    67,    68,    69,    70,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    73,    74,     0,    75,   301,
       0,     0,     0,     0,     0,     0,    77,    78,   557,   243,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,     0,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,     0,     0,     0,     0,    45,    46,     0,     0,     0,
      47,    48,    49,    50,    51,    52,    53,    54,     0,    13,
      14,    15,    16,    17,    18,    57,    19,     0,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,     0,     0,     0,     0,    45,    46,    62,
      63,     0,    47,    48,    49,    50,    51,    52,    53,    54,
       0,    13,    14,    15,    16,    17,    18,    57,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,     0,     0,    75,     0,    45,
      46,    62,    63,     0,    47,    48,    49,    50,    51,    52,
      53,    54,     0,     0,     0,     0,     0,     0,     0,    57,
       0,     0,     0,     0,     0,  1432,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   932,    75,
     926,     0,     0,    62,    63,     0,     0,    77,    78,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    75,   926,     0,     0,     0,     0,     0,     0,    77,
      78,    13,    14,    15,    16,    17,    18,     0,    19,     0,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,     0,     0,     0,     0,    45,
      46,     0,     0,     0,    47,    48,    49,    50,    51,    52,
      53,    54,     0,    13,    14,    15,    16,    17,    18,    57,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,     0,     0,     0,
       0,    45,    46,    62,    63,     0,    47,    48,    49,    50,
      51,    52,    53,    54,     0,     0,     0,     0,     0,     0,
       0,    57,     0,     0,     0,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    75,   286,     0,     0,    62,    63,     0,     0,    77,
      78,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    75,    76,     0,     0,     0,     0,     0,
       0,    77,    78,    13,    14,    15,    16,    17,    18,     0,
      19,     0,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,     0,     0,     0,
       0,    45,    46,     0,     0,     0,    47,    48,    49,    50,
      51,    52,    53,    54,     0,    13,    14,    15,    16,    17,
      18,    57,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,     0,
       0,     0,     0,    45,    46,    62,    63,     0,   319,    48,
      49,    50,    51,    52,    53,    54,     0,     0,     0,     0,
       0,     0,     0,    57,     0,     0,     0,     0,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    75,   434,     0,     0,    62,    63,     0,
       0,    77,    78,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    75,   321,     0,     0,     0,
       0,     0,     0,    77,    78,    13,    14,    15,    16,    17,
      18,     0,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,     0,
       0,     0,     0,    45,    46,     0,     0,     0,   319,    48,
      49,    50,    51,    52,    53,    54,     0,    13,    14,    15,
      16,    17,    18,    57,    19,     0,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,     0,     0,     0,     0,    45,    46,    62,    63,     0,
     319,    48,    49,    50,    51,    52,    53,    54,     0,     0,
       0,     0,     0,     0,     0,    57,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    75,   286,     0,     0,    62,
      63,     0,     0,    77,    78,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    75,   434,     0,
       0,     0,     0,     0,     0,    77,    78,   242,   243,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,     0,     0,    19,     0,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
    -451,  -451,     0,  -451,    45,    46,     0,  -451,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    13,    14,
      15,    16,    17,    18,    57,    19,     0,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,     0,     0,     0,     0,    45,    46,    62,    63,
       0,   319,    48,    49,    50,    51,    52,    53,    54,     0,
      13,    14,    15,    16,    17,    18,    57,    19,     0,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,     0,     0,    75,     0,    45,    46,
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
      75,   926,     0,     0,     0,     0,     0,     0,    77,    78,
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
      75,   926,     0,     0,    62,    63,     0,     0,    77,    78,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    75,     0,     0,    13,    14,    15,    16,    17,
      77,    78,    19,     0,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,  -451,
    -451,     0,  -451,    45,    46,     0,  -451,     0,     0,     0,
       0,     0,     0,     0,     0,    13,    14,    15,    16,    17,
       0,     0,    19,    57,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,  -451,
    -451,     0,  -451,    45,    46,     0,  -451,    62,    63,     0,
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
       0,     0,   852,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -647,    75,   243,     6,     7,     8,     9,    10,
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
       0,  1694,     0,     0,     0,     0,   243,     6,     7,     8,
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
      40,    41,    42,    43,    44,  -451,  -451,     0,  -451,    45,
      46,     0,  -451,     0,     0,     0,     0,     0,     0,     0,
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
      38,    39,    40,    41,    42,    43,    44,  -451,  -451,    75,
    -451,    45,    46,     0,  -451,     0,     0,   361,     0,     0,
       0,   362,     0,   363,     0,     0,     0,     0,     0,     0,
       0,    57,     0,     0,     0,     0,     0,     0,     0,     0,
     364,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    62,    63,   365,   366,     0,
     367,     0,   368,    62,    63,    64,    65,    66,    67,    68,
      69,    70,   369,   370,   358,     0,   371,   372,   373,   361,
     374,   375,     0,   362,     0,   363,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   364,    75,     0,     0,     0,     0,   376,  1236,
       0,    75,   377,     0,     0,     0,  1237,     0,   378,    77,
      78,   379,   380,   381,   382,     0,     0,     0,     0,   365,
     366,     0,   367,     0,   368,    62,    63,    64,    65,    66,
      67,    68,    69,    70,   369,   370,   358,     0,   371,   372,
     373,   361,   374,   375,     0,   362,     0,   363,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   364,     0,     0,     0,     0,     0,
     376,   959,  1539,    75,   377,     0,     0,     0,     0,     0,
     378,    77,    78,   379,   380,   381,   382,     0,     0,     0,
       0,   365,   366,     0,   367,     0,   368,    62,    63,    64,
      65,    66,    67,    68,    69,    70,   369,   370,   358,     0,
     371,   372,   373,   361,   374,   375,     0,   362,     0,   363,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   364,     0,     0,     0,
       0,     0,   376,     0,     0,    75,   377,     0,     0,     0,
     466,     0,   378,    77,    78,   379,   380,   381,   382,     0,
       0,     0,     0,   365,   366,     0,   367,     0,   368,    62,
      63,    64,    65,    66,    67,    68,    69,    70,   369,   370,
     358,     0,   371,   372,   373,   361,   374,   375,     0,   362,
       0,   363,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   364,     0,
       0,     0,     0,     0,   376,   798,     0,    75,   377,     0,
       0,     0,     0,     0,   378,    77,    78,   379,   380,   381,
     382,     0,     0,     0,     0,   365,   366,     0,   367,     0,
     368,    62,    63,    64,    65,    66,    67,    68,    69,    70,
     369,   370,   358,     0,   371,   372,   373,   361,   374,   375,
       0,   362,     0,   363,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     364,     0,     0,     0,     0,     0,   376,     0,     0,    75,
     377,     0,     0,     0,   279,     0,   378,    77,    78,   379,
     380,   381,   382,     0,     0,     0,     0,   365,   366,     0,
     367,     0,   368,    62,    63,    64,    65,    66,    67,    68,
      69,    70,   369,   370,   358,     0,   371,   372,   373,   361,
     374,   375,     0,   362,     0,   363,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   364,     0,     0,     0,     0,     0,   376,   959,
       0,    75,   377,     0,     0,     0,     0,     0,   378,    77,
      78,   379,   380,   381,   382,     0,     0,     0,     0,   365,
     366,     0,   367,     0,   368,    62,    63,    64,    65,    66,
      67,    68,    69,    70,   369,   370,   358,     0,   371,   372,
     373,   361,   374,   375,     0,   362,     0,   363,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   364,     0,     0,     0,     0,     0,
     376,     0,     0,    75,   377,     0,     0,   990,     0,     0,
     378,    77,    78,   379,   380,   381,   382,     0,     0,     0,
       0,   365,   366,     0,   367,     0,   368,    62,    63,    64,
      65,    66,    67,    68,    69,    70,   369,   370,   358,     0,
     371,   372,   373,   361,   374,   375,     0,   362,     0,   363,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   364,     0,     0,     0,
       0,     0,   376,     0,     0,    75,   377,     0,     0,     0,
    1211,     0,   378,    77,    78,   379,   380,   381,   382,     0,
       0,     0,     0,   365,   366,     0,   367,     0,   368,    62,
      63,    64,    65,    66,    67,    68,    69,    70,   369,   370,
     358,     0,   371,   372,   373,   361,   374,   375,     0,   362,
       0,   363,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   364,     0,
       0,     0,     0,     0,   376,  1303,     0,    75,   377,     0,
       0,     0,     0,     0,   378,    77,    78,   379,   380,   381,
     382,     0,     0,     0,     0,   365,   366,     0,   367,     0,
     368,    62,    63,    64,    65,    66,    67,    68,    69,    70,
     369,   370,   358,     0,   371,   372,   373,   361,   374,   375,
       0,   362,     0,   363,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     364,     0,     0,     0,     0,     0,   376,     0,     0,    75,
     377,     0,     0,     0,  1363,     0,   378,    77,    78,   379,
     380,   381,   382,     0,     0,     0,     0,   365,   366,     0,
     367,     0,   368,    62,    63,    64,    65,    66,    67,    68,
      69,    70,   369,   370,   358,     0,   371,   372,   373,   361,
     374,   375,     0,   362,     0,   363,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   364,     0,     0,     0,     0,     0,   376,     0,
    1803,    75,   377,     0,     0,     0,     0,     0,   378,    77,
      78,   379,   380,   381,   382,     0,     0,     0,     0,   365,
     366,     0,   367,     0,   368,    62,    63,    64,    65,    66,
      67,    68,    69,    70,   369,   370,   358,     0,   371,   372,
     373,   361,   374,   375,     0,   362,     0,   363,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   364,     0,     0,     0,     0,     0,
     376,  1807,     0,    75,   377,     0,     0,     0,     0,     0,
     378,    77,    78,   379,   380,   381,   382,     0,     0,     0,
       0,   365,   366,     0,   367,     0,   368,    62,    63,    64,
      65,    66,    67,    68,    69,    70,   369,   370,   358,     0,
     371,   372,   373,   361,   374,   375,     0,   362,     0,   363,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   364,     0,     0,     0,
       0,     0,   376,  1816,     0,    75,   377,     0,     0,     0,
       0,     0,   378,    77,    78,   379,   380,   381,   382,     0,
       0,     0,     0,   365,   366,     0,   367,     0,   368,    62,
      63,    64,    65,    66,    67,    68,    69,    70,   369,   370,
     358,     0,   371,   372,   373,   361,   374,   375,     0,   362,
       0,   363,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   364,     0,
       0,     0,     0,     0,   376,  1892,     0,    75,   377,     0,
       0,     0,     0,     0,   378,    77,    78,   379,   380,   381,
     382,     0,     0,     0,     0,   365,   366,     0,   367,     0,
     368,    62,    63,    64,    65,    66,    67,    68,    69,    70,
     369,   370,   358,     0,   371,   372,   373,   361,   374,   375,
       0,   362,     0,   363,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     364,     0,     0,     0,     0,     0,   376,  1894,     0,    75,
     377,     0,     0,     0,     0,     0,   378,    77,    78,   379,
     380,   381,   382,     0,     0,     0,     0,   365,   366,     0,
     367,     0,   368,    62,    63,    64,    65,    66,    67,    68,
      69,    70,   369,   370,   358,     0,   371,   372,   373,   361,
     374,   375,     0,   362,     0,   363,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   364,     0,     0,     0,     0,     0,   376,  1944,
       0,    75,   377,     0,     0,     0,     0,     0,   378,    77,
      78,   379,   380,   381,   382,     0,     0,     0,     0,   365,
     366,     0,   367,     0,   368,    62,    63,    64,    65,    66,
      67,    68,    69,    70,   369,   370,   358,     0,   371,   372,
     373,   361,   374,   375,     0,   362,     0,   363,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   364,     0,     0,     0,     0,     0,
     376,  1946,     0,    75,   377,     0,     0,     0,     0,     0,
     378,    77,    78,   379,   380,   381,   382,     0,     0,     0,
       0,   365,   366,     0,   367,     0,   368,    62,    63,    64,
      65,    66,    67,    68,    69,    70,   369,   370,   358,     0,
     371,   372,   373,   361,   374,   375,     0,   362,     0,   363,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   364,     0,     0,     0,
       0,     0,   376,  1948,     0,    75,   377,     0,     0,     0,
       0,     0,   378,    77,    78,   379,   380,   381,   382,     0,
       0,     0,     0,   365,   366,     0,   367,     0,   368,    62,
      63,    64,    65,    66,    67,    68,    69,    70,   369,   370,
     358,     0,   371,   372,   373,   361,   374,   375,     0,   362,
       0,   363,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   364,     0,
       0,     0,     0,     0,   376,  1951,     0,    75,   377,     0,
       0,     0,     0,     0,   378,    77,    78,   379,   380,   381,
     382,     0,     0,     0,     0,   365,   366,     0,   367,     0,
     368,    62,    63,    64,    65,    66,    67,    68,    69,    70,
     369,   370,   358,     0,   371,   372,   373,   361,   374,   375,
       0,   362,     0,   363,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     364,     0,     0,     0,     0,     0,   376,  1953,     0,    75,
     377,     0,     0,     0,     0,     0,   378,    77,    78,   379,
     380,   381,   382,     0,     0,     0,     0,   365,   366,     0,
     367,     0,   368,    62,    63,    64,    65,    66,    67,    68,
      69,    70,   369,   370,   358,     0,   371,   372,   373,   361,
     374,   375,     0,   362,     0,   363,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   364,     0,     0,     0,     0,     0,   376,  1995,
       0,    75,   377,     0,     0,     0,     0,     0,   378,    77,
      78,   379,   380,   381,   382,     0,     0,     0,     0,   365,
     366,     0,   367,     0,   368,    62,    63,    64,    65,    66,
      67,    68,    69,    70,   369,   370,   358,     0,   371,   372,
     373,   361,   374,   375,     0,   362,     0,   363,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   364,     0,     0,     0,     0,     0,
     376,  1997,     0,    75,   377,     0,     0,     0,     0,     0,
     378,    77,    78,   379,   380,   381,   382,     0,     0,     0,
       0,   365,   366,     0,   367,     0,   368,    62,    63,    64,
      65,    66,    67,    68,    69,    70,   369,   370,   358,     0,
     371,   372,   373,   361,   374,   375,     0,   362,     0,   363,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   364,     0,     0,     0,
       0,     0,   376,  1999,     0,    75,   377,     0,     0,     0,
       0,     0,   378,    77,    78,   379,   380,   381,   382,     0,
       0,     0,     0,   365,   366,     0,   367,     0,   368,    62,
      63,    64,    65,    66,    67,    68,    69,    70,   369,   370,
     358,     0,   371,   372,   373,   361,   374,   375,     0,   362,
       0,   363,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   364,     0,
       0,     0,     0,     0,   376,  2022,     0,    75,   377,     0,
       0,     0,     0,     0,   378,    77,    78,   379,   380,   381,
     382,     0,     0,     0,     0,   365,   366,     0,   367,     0,
     368,    62,    63,    64,    65,    66,    67,    68,    69,    70,
     369,   370,   358,     0,   371,   372,   373,   361,   374,   375,
       0,   362,     0,   363,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     364,     0,     0,     0,     0,     0,   376,  2024,     0,    75,
     377,     0,     0,     0,     0,     0,   378,    77,    78,   379,
     380,   381,   382,     0,     0,     0,     0,   365,   366,     0,
     367,     0,   368,    62,    63,    64,    65,    66,    67,    68,
      69,    70,   369,   370,   358,     0,   371,   372,   373,   361,
     374,   375,     0,   362,     0,   363,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   364,     0,     0,     0,     0,     0,   376,  2026,
       0,    75,   377,     0,     0,     0,     0,     0,   378,    77,
      78,   379,   380,   381,   382,     0,     0,     0,     0,   365,
     366,     0,   367,     0,   368,    62,    63,    64,    65,    66,
      67,    68,    69,    70,   369,   370,   358,     0,   371,   372,
     373,   361,   374,   375,     0,   362,     0,   363,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   364,     0,     0,     0,     0,     0,
     376,     0,     0,    75,   377,     0,     0,     0,     0,     0,
     378,    77,    78,   379,   380,   381,   382,     0,     0,     0,
       0,   365,   366,     0,   367,     0,   368,    62,    63,    64,
      65,    66,    67,    68,    69,    70,   369,   370,   358,     0,
     371,   372,   373,   361,   374,   375,     0,   362,     0,   363,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   364,     0,     0,     0,
       0,     0,   650,     0,     0,    75,   377,     0,     0,     0,
       0,     0,   378,    77,    78,   379,   380,   381,   382,     0,
       0,     0,     0,   365,   366,     0,   367,     0,   368,    62,
      63,    64,    65,    66,    67,    68,    69,    70,   369,   370,
     358,     0,   371,   372,   373,   361,   374,   375,     0,   362,
       0,   363,     0,     0,    72,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   364,     0,
       0,     0,     0,     0,   655,     0,     0,    75,   377,     0,
       0,     0,     0,     0,   378,    77,    78,   379,   380,   381,
     382,     0,     0,     0,     0,   365,   366,     0,   367,     0,
     368,    62,    63,    64,    65,    66,    67,    68,    69,    70,
     369,   370,   358,     0,   371,   372,   373,   361,   374,   375,
       0,   362,     0,   363,     0,     0,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     364,     0,     0,     0,     0,     0,   661,     0,     0,    75,
     377,     0,     0,     0,     0,     0,   378,    77,    78,   379,
     380,   381,   382,     0,     0,     0,     0,   365,   366,     0,
     367,     0,   368,    62,    63,    64,    65,    66,    67,    68,
      69,    70,   369,   370,   358,     0,   371,   372,   373,   361,
     374,   375,     0,   362,     0,   363,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   364,     0,     0,     0,     0,     0,   376,     0,
       0,    75,   377,     0,     0,     0,     0,     0,   378,   866,
      78,   379,   380,   381,   382,     0,     0,     0,     0,   365,
     366,     0,   367,     0,   368,    62,    63,    64,    65,    66,
      67,    68,    69,    70,   369,   370,   358,     0,   371,   372,
     373,   361,   374,   375,     0,   362,     0,   363,     0,     0,
      72,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   364,     0,     0,     0,     0,     0,
     376,     0,     0,    75,   377,     0,     0,     0,     0,     0,
     378,   439,    78,   379,   380,   381,   382,     0,     0,     0,
       0,   365,   366,     0,   367,     0,   368,  1887,    63,    64,
      65,    66,    67,    68,    69,    70,   369,   370,   358,     0,
     371,   372,   373,     0,   374,   375,     0,     0,     0,     0,
       0,     0,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   376,     0,     0,    75,   377,     0,     0,     0,
       0,     0,   378,    77,    78,   379,   380,   381,   382
};

static const yytype_int16 yycheck[] =
{
       1,    73,     1,     4,     1,    73,   219,    73,   219,   466,
     178,   256,    82,     1,   177,    75,     4,   220,   150,    73,
     283,   162,   173,     4,   219,   693,   162,   162,   207,   376,
     875,   219,   241,   678,   635,   616,  1737,    73,    95,  1287,
    1288,   603,   766,   376,  1670,    58,  1669,   607,   222,   603,
    1669,  1669,   671,  1355,    55,    56,     1,    58,   965,    58,
     219,    58,   766,     1,   219,  1125,     4,     1,     1,  1797,
      58,   324,    73,   153,     0,    82,   367,   147,   158,  1801,
       1,    82,   295,    82,   295,   737,   603,   994,    95,    90,
    1209,    98,   845,   296,    95,   102,    95,    98,   219,    98,
     295,   102,   574,   102,   236,   163,   139,   295,     0,   181,
      98,    70,    87,    58,   102,   181,   290,   291,    82,   872,
      58,   102,  1567,    73,   339,    58,    73,   181,   343,  1355,
    1356,   228,   145,   764,   231,   861,   295,    58,   764,   140,
     295,   764,   143,   764,   145,   181,   145,   219,   145,   150,
    1057,   219,   364,   219,   157,   156,   253,   145,   191,  1785,
      73,    73,   163,   220,   102,   219,   263,    75,    76,   515,
     516,   174,   463,   180,   295,   245,   120,   389,   390,   180,
     181,   180,   157,   219,    70,   149,    87,   322,   195,     0,
     149,     1,    96,   446,   195,   609,   195,   767,   410,   157,
     145,   526,   764,   773,   205,  1933,   766,   145,   152,   210,
     764,   536,   145,   220,   274,   178,   174,   115,   219,   220,
    1245,   220,   156,   295,   145,   229,   149,   295,   440,   295,
       1,   181,   302,   103,   181,   236,    87,   107,   245,   296,
     110,   295,   112,   130,   245,   149,   245,   764,    58,   157,
     254,   258,   577,   498,   255,   923,   157,   258,  1109,   258,
     264,   174,   174,  1985,   265,   632,   784,   153,  1713,   219,
     258,   484,   219,   484,   275,   276,  1902,   278,  1563,     1,
     155,   882,     4,   951,   802,   172,   283,    58,  1590,   484,
     493,   637,   102,   472,   295,   296,   484,   296,  2020,   174,
     563,    95,   303,  1027,  1423,   440,   157,   783,   784,   310,
     311,   783,   784,  2014,   315,   916,     1,   650,   861,   652,
     653,   940,   655,  1027,  1109,   484,   802,    98,   661,   484,
     802,   664,   665,   666,   192,   145,    58,   897,   283,  1965,
     410,  1964,   595,   149,   691,  1964,  1964,   348,  1101,   316,
     868,  1003,   353,   155,   768,   356,   357,   570,   772,   570,
      82,   131,   283,   484,  1590,  1109,   619,   781,   782,   572,
     115,  1099,   174,   626,   145,   570,  1104,   592,   248,  2005,
     174,   513,   570,   155,    59,    60,   444,   519,   355,  1020,
     155,  1117,   868,     4,  1020,   165,   868,  1020,   744,  1020,
     165,    70,   174,   436,   464,   149,  1654,  1655,   153,   506,
      70,   570,   484,   420,   629,   570,   131,   139,   484,   420,
    1146,   420,   789,   145,   552,   147,  1451,  1452,  1453,   173,
     484,   528,   152,  1718,  1719,   146,   493,   534,   574,   574,
     149,   538,   443,   444,    55,    56,   157,  1175,   484,   570,
     165,  1021,   149,   173,   455,   456,   326,   327,  1020,   329,
     157,   331,   173,   464,   936,   466,  1020,  1027,   155,   191,
     155,   156,    19,   283,   322,  1326,  1327,  1328,   165,    90,
     149,   616,   157,   484,   153,   697,   493,   258,   149,   149,
    1716,   339,   493,   153,   493,   343,    75,    76,   570,   174,
      87,   149,  1562,  1020,   570,    56,    73,  1567,    59,    60,
     353,    62,   513,   356,    73,   572,   570,   842,   519,   131,
     242,    88,    70,   245,   104,   105,   155,  1812,  1813,   140,
      89,   535,   143,   149,   484,  1936,   165,   484,    70,  1446,
     997,  1326,  1327,  1328,    70,   156,    70,   106,   160,   161,
     917,  1069,   163,   324,    82,   718,   557,    70,   559,   149,
    1961,   283,    70,   576,   104,   105,   563,   157,   155,   570,
     157,   572,   152,   572,  1117,   576,   131,   576,   157,   576,
     302,   155,  1326,  1327,  1328,   586,  1987,    70,   576,   590,
     151,   165,   440,  1069,   155,  1323,   593,  1069,   149,   210,
     853,   149,    70,  1146,  1205,   153,  1854,   104,   105,   777,
    1255,  1192,   791,   125,   126,    70,   155,   149,   563,   147,
     155,   153,   623,   149,    70,   149,    70,   153,  1198,   153,
    1044,   576,   154,   886,   635,   174,   149,   155,   576,   174,
     153,   149,   563,   576,   255,   153,  1872,   165,    70,   155,
     155,   179,   155,  1713,   265,   576,   211,   169,   170,   719,
      70,   824,   532,   521,   275,   276,   149,   278,   174,   884,
     153,   174,   804,  1899,   174,   446,  1521,  1345,   155,   155,
     149,   149,   683,   174,   685,   153,   687,   156,   410,   165,
     691,   823,   303,   694,   149,     3,    70,   174,   153,   310,
     311,   151,   830,   149,   315,   149,  1276,   153,   766,   153,
       3,  1937,   162,   163,   436,   155,   148,   245,   719,   155,
     932,  1366,   265,   155,   149,   937,   151,   149,   153,   284,
    1637,   153,  1639,   276,   174,   593,   948,   348,   174,   149,
     151,   152,   353,   153,   592,   356,   914,   844,  1220,   152,
    1810,   561,   150,   563,  1211,    70,   143,   144,   145,   157,
       3,   889,  1822,   764,   761,   766,   576,   155,   616,    12,
      13,    14,    15,    16,   302,   149,   149,   778,   165,   153,
    1237,   629,   153,   173,   785,   148,   129,   158,   160,   560,
     791,   157,   155,   794,   322,   167,   168,   149,  1443,   151,
     936,   936,   803,   804,   805,   576,   149,    73,   162,   163,
     153,  1056,  1355,  1356,   777,  1287,   157,   160,   161,   151,
    1187,   376,   823,   151,   156,   788,  1886,    70,   156,    95,
     173,   157,   443,    46,    47,   557,    49,   151,  1184,   151,
      53,   563,   156,   155,   455,   456,  1213,   149,   619,    56,
     908,   151,    59,    60,   576,    62,   156,   149,   859,   860,
     861,   149,   720,   151,   861,   153,    12,    13,    14,    15,
      16,   999,   155,   861,   157,   171,    12,    13,    14,    15,
      16,   882,   410,   151,   150,  1064,   874,   155,  1016,  1017,
     149,  1224,   151,   874,   153,  1262,   152,   153,   129,   151,
     156,   153,   149,   761,   149,  1117,  1363,   908,   153,   151,
     115,   912,   440,   155,  1088,   916,   861,   129,   149,   123,
     124,   922,   153,   861,    70,   783,   784,   151,   861,   160,
     161,   155,   151,  1405,    70,   151,   874,   149,   149,   155,
     861,   153,   129,  1196,   802,   151,   557,   149,   160,   161,
     505,   914,   151,   219,   220,   510,   957,   127,   128,   143,
     144,   145,   149,   151,   965,   151,   153,     9,  1026,  1027,
     236,   155,   527,   160,   161,   586,  1512,  1513,  1514,   590,
    1192,   165,   537,  1330,  1441,   131,   151,   515,   516,   155,
     174,    21,  1201,   994,   151,   131,   997,   149,   155,   154,
     155,     4,     5,     6,     7,     8,     9,    10,    11,   149,
     868,   151,   623,   153,   151,  1227,  1228,  1229,   155,  1020,
    1621,   149,  1234,  1235,   635,  1026,  1027,   990,   149,   295,
     296,     3,   153,   151,  1185,  1186,   884,   155,   155,  1109,
      12,    13,    14,    15,    16,   149,  1178,  1590,   149,   153,
     149,   861,   153,  1420,   153,   155,  1057,  1192,  1425,   101,
     154,   155,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   155,   683,  1318,   685,  1442,   687,   151,   160,   161,
     691,   155,   853,   694,  1220,  1220,   155,   151,   616,    96,
     861,   155,  1355,   864,   149,   650,   149,   151,    70,   157,
     655,   155,    87,  1066,   154,   155,   661,   148,   719,   637,
      12,    13,    14,    15,    16,   886,   154,   155,  1119,   154,
    1117,  1122,  1123,  1124,   157,   680,   151,  1295,  1296,  1117,
     155,   154,   155,  1336,   108,   109,   110,   111,   112,   861,
    1268,  1269,   151,   154,   155,  1146,   155,  1288,   151,  1146,
    1751,  1152,   874,   157,  1282,  1283,  1163,  1164,  1146,  1160,
     155,   156,  1163,  1164,  1163,  1164,  1167,   778,    70,  1837,
     151,   174,  1117,  1716,   785,  1163,  1164,  1178,   444,  1117,
     157,   151,  1654,  1164,  1117,   155,   151,   151,  1316,  1317,
     155,   155,   803,   151,   805,   151,  1117,   155,   151,   155,
     157,  1146,   155,   151,  1205,  1572,   157,   155,  1146,   151,
    1211,  1069,   173,  1146,   115,   151,   744,   149,   484,   155,
    1221,   143,   144,   145,   149,  1146,  1164,   493,    12,    13,
      14,    15,    16,   155,   154,   155,  1237,    12,    13,    14,
      15,    16,    17,   165,  1245,   154,   155,   513,   859,   860,
     861,   151,   174,   519,   149,   155,  1326,  1327,  1328,   151,
    1330,  1331,   151,   155,  1631,   155,   155,   166,  1969,  1405,
    1405,   882,  1973,   161,   829,   159,  1277,   154,   155,  1336,
     171,   705,   706,   707,   708,   840,    70,   129,   843,   698,
     699,   700,   847,   559,   152,  1258,  1629,   908,  1106,  1107,
    1108,   912,   154,   155,   570,   916,   572,  1117,   154,   155,
     154,   155,  1525,   151,  1525,   151,  1519,    12,    13,    14,
      15,    16,    17,   151,  1527,   154,   155,  1590,   151,  1872,
    1525,   151,  1295,  1296,   151,  1336,  1146,  1525,   153,  1340,
     154,  1329,  1343,   131,  1192,   129,  1117,   875,  1329,   154,
     155,   154,   155,   131,  1164,   156,  1899,   156,  1355,  1356,
     154,   155,  1363,   154,   155,   149,  1525,  1355,  1356,   153,
    1525,   154,   155,  1501,  1502,  1146,   160,   161,  1385,   155,
     154,   155,  1383,  1595,  1385,  1752,  1385,  1109,   155,   156,
     149,  1329,  1163,   151,  1937,  1117,   151,  1385,    12,    13,
      14,    15,    16,    17,  1525,   154,   155,   154,   155,  1537,
    1355,  1356,   151,   830,    75,    76,   151,  1355,  1356,   155,
     156,  1246,  1247,   151,  1146,  1196,  1589,   701,   702,  1287,
    1288,   703,   704,   151,  1355,  1356,   709,   710,  1532,  1533,
    1441,  1429,  1185,  1186,   149,  1446,   154,   153,  1429,    68,
    1451,  1452,  1453,   157,   157,   157,   157,  1525,   154,   149,
    1601,    76,  1519,   157,    17,  1601,  1601,   154,   157,   155,
    1527,  1684,   889,  1684,   173,   173,   149,   174,  1845,   151,
     151,   157,   174,   157,  1687,  1613,   154,   154,    17,  1684,
     148,  1429,   151,   148,   151,   151,  1684,   151,   764,   151,
     766,    12,    13,    14,    15,    16,    17,   151,  1119,   151,
     151,  1122,  1123,  1124,  1655,   151,  1644,   157,  1519,   157,
     157,  1649,  1650,    68,  1525,  1684,  1527,   174,   173,  1684,
     151,   151,   148,  1534,   151,  1146,   149,    13,   804,   157,
     151,  1152,   151,   155,   151,  1355,  1356,  1548,  1103,  1160,
     151,   155,   151,   151,   151,   151,  1167,   823,   151,   151,
    1561,  1116,   148,  1684,   154,   154,   151,  1555,  1556,  1732,
     151,   151,   151,   151,   151,  1556,   151,   151,  1133,   151,
     151,   154,   999,   151,  1355,  1356,   173,   148,   151,   155,
     149,  1794,   149,  1590,  1205,   149,   156,  1598,   149,  1016,
    1017,   149,  1590,    72,  1326,  1327,  1328,  1329,  1330,  1331,
    1221,  1743,   155,   154,  1385,    89,  1604,   156,  1556,   174,
    1621,   154,   174,  1604,  1836,   174,   148,   174,   148,  1797,
    1687,   157,   155,  1355,  1356,   151,  1637,     1,  1639,   174,
       4,   154,   151,   155,   151,  1590,   151,   155,   148,   155,
    1903,   154,  1590,   151,   148,   174,  1184,   149,   149,   174,
      78,   149,   148,   148,  1192,   149,  1604,   148,   151,  1590,
    1833,     4,     5,     6,     7,     8,     9,    10,    11,  1537,
     148,   174,   101,  1684,   174,   174,  1687,   106,   107,   108,
     109,   110,   111,   112,    58,   174,   174,  1698,   155,   174,
     155,  1702,   174,   154,   154,   154,   151,  1429,   154,    73,
     157,   148,   151,  1854,  1715,   156,  1704,   118,    82,  1716,
     156,   148,   151,   151,  1725,   151,   151,   151,  1716,  1340,
      63,    95,  1343,   154,    98,   148,   174,  1794,   102,   154,
     156,  1742,  1743,   155,   151,   149,  1556,   155,   151,   149,
    1751,  1964,   149,  1964,  1020,   107,   154,   148,   154,  1829,
    1026,  1027,  1965,  1931,   154,  1933,   148,   148,   157,  1964,
    1902,  1716,  1383,   151,   151,   139,  1964,   151,  1716,   151,
    1590,   145,    73,   147,  1555,   154,   150,   151,   151,   151,
      73,  1346,  1347,  1794,   174,  1716,  1654,  1655,   162,   174,
    1801,   148,  2005,  1971,  1805,  1964,  1969,   149,  1809,  1964,
    1973,  1974,   151,   174,   151,   179,   180,   181,    88,  1590,
     154,   148,   154,   148,   153,   151,    73,   191,   192,  1384,
    1900,   195,   151,  1834,  1797,     4,     5,     6,     7,     8,
       9,    10,    11,  1964,  2007,   151,   151,   151,   151,   165,
     152,  1268,  1269,   174,  1982,   219,   220,   165,    73,   174,
     156,   174,   151,   148,   151,  1282,  1283,   151,  1590,  2032,
     151,   148,   236,  2036,  1875,  1872,   155,   150,  1879,   165,
     148,   245,  1604,  2051,  1872,   165,   149,   156,   101,   155,
    1891,    73,  1964,   149,   258,  2058,  1964,   148,  1964,  1316,
    1317,  1902,  1899,  1904,   165,   165,  1716,   150,  1965,   174,
    1964,  1899,  1178,   154,  1915,   174,  1917,  1918,    12,    13,
      14,    15,    16,  1534,   288,   150,   107,  1872,   107,   151,
     294,   295,   296,  1704,  1872,   156,   148,  1548,   302,  1940,
    1937,   151,   148,   151,   149,  1716,   174,    73,  2005,  1937,
    1561,  1872,   151,   151,  1899,   174,   174,  1629,   322,   323,
     324,  1899,  1345,  1964,  1965,  1252,  1965,   378,  1931,   711,
    1933,   712,   715,   713,  1975,   339,    70,   670,  1899,   343,
     714,  1829,  1135,  1146,  1985,  2020,   409,  1598,  1590,    17,
    1933,  1724,  1937,  1521,  1716,  1961,  1854,  1716,  2015,  1937,
    1838,  1582,  2014,  1900,  2005,  1582,  2005,  2002,  1971,  1974,
    1621,  1277,   376,  2036,  1899,    48,  1937,  2018,  1167,  2020,
     250,    12,    13,    14,    15,    16,  1581,    55,    56,    57,
      58,    59,    60,    61,    62,   129,  1794,  1519,  2039,  1862,
    2003,  1331,   473,  1533,  2045,  1160,   410,   791,   878,   413,
     586,  1429,  1900,     0,  2055,   149,   420,   922,  2059,   153,
    1604,   736,  1872,   205,    -1,    -1,   160,   161,  2069,    -1,
    1336,   736,   436,   736,    -1,    -1,   440,    76,    -1,    70,
     444,    -1,   446,    -1,  1501,  1502,    -1,  1698,  2051,  1899,
      17,  1702,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1872,   101,    -1,  1715,   104,   105,   106,   107,   108,
     109,   110,   111,   112,  1725,    -1,    -1,    -1,    -1,    -1,
     484,    -1,    -1,    -1,    -1,    -1,    -1,  1937,  1899,   493,
      -1,  1742,    59,    60,    61,    62,   101,    -1,   129,    -1,
    1751,   106,   107,   108,   109,   110,   111,   112,   113,   513,
    1872,   515,   516,    -1,    -1,   519,    -1,   521,   149,    -1,
      -1,    -1,   153,    -1,    -1,    -1,  1937,    -1,    -1,   160,
     161,    -1,    -1,    -1,   101,   174,    -1,  1899,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,    -1,   153,    -1,
    1801,    -1,    -1,    -1,  1805,   559,  1613,    -1,  1809,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   570,    -1,   572,    -1,
     574,   101,   576,     9,    -1,  1937,   106,   107,   108,   109,
     110,   111,   112,  1834,   192,    -1,   153,  1644,   592,   593,
      -1,   595,  1649,  1650,    -1,     3,    -1,    -1,    -1,   603,
      -1,    -1,    -1,   607,   118,    -1,   120,   121,   122,    -1,
      -1,    -1,   616,  1519,    -1,    -1,    -1,    -1,    -1,  1525,
     150,  1527,   626,   153,  1875,   629,    -1,    -1,  1879,    -1,
      -1,    -1,    -1,   637,    -1,   149,    -1,    -1,   152,   153,
    1891,    -1,    -1,   157,   158,    -1,   650,    -1,   652,   653,
      -1,   655,    -1,  1904,    -1,    -1,    -1,   661,    -1,    -1,
     664,   665,   666,    -1,  1915,   101,  1917,  1918,   104,   105,
     106,   107,   108,   109,   110,   111,   112,    -1,    -1,    -1,
      -1,    -1,   464,    -1,   466,    -1,    -1,    -1,    -1,  1940,
      -1,    -1,    -1,   101,    -1,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   322,   178,   720,   325,    -1,    -1,
      -1,   129,    -1,    -1,  1975,    -1,    -1,    -1,    -1,    -1,
      -1,   339,   736,   737,  1985,   343,    -1,    -1,    -1,    -1,
     744,   149,   150,    -1,    -1,    -1,     1,    -1,   156,     4,
      -1,    -1,   160,   161,    -1,    -1,    -1,   761,    -1,    -1,
     764,    -1,   766,    -1,    -1,    -1,    -1,  2018,    -1,  2020,
      -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,  1684,   783,
     784,  1687,    -1,    -1,    -1,    -1,    -1,    -1,  2039,    -1,
      -1,    -1,    -1,    -1,  2045,    -1,    -1,    -1,   802,    -1,
     804,    -1,    -1,    58,  2055,    -1,   101,    -1,  2059,   104,
     105,   106,   107,   108,   109,   110,   111,   112,  2069,   823,
      -1,    12,    13,    14,    15,    16,    -1,    82,    -1,    -1,
      -1,    -1,   440,    -1,   129,    -1,  2031,  1743,    -1,    -1,
      -1,    -1,    -1,    98,    -1,    -1,    -1,   102,    -1,    -1,
      -1,    -1,  2047,    -1,   149,   150,    12,   861,   153,    -1,
      -1,    -1,    -1,    -1,   868,   160,   161,    -1,    -1,    -1,
     874,   875,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,
     884,    -1,   886,    -1,   139,    -1,    -1,    -1,  1794,    -1,
     145,    -1,   147,   897,    -1,    -1,   151,    -1,   361,    -1,
      -1,   364,   365,    -1,    -1,    -1,   161,   162,   163,    -1,
     101,   374,   375,   521,    -1,   106,   107,   108,   109,   110,
     111,   112,    -1,    -1,   179,  1982,   389,   390,    -1,    -1,
      86,    -1,   936,    -1,    -1,    -1,   191,   192,   129,    -1,
     195,    -1,    -1,    -1,    -1,   101,    -1,   410,   104,   105,
     106,   107,   108,   109,   110,   111,   112,    -1,   149,   150,
      -1,    -1,    -1,    -1,    -1,    -1,   574,    -1,    -1,   160,
     161,    -1,    -1,    -1,    -1,    -1,    -1,   440,    -1,    12,
      13,    14,    15,    16,   592,   593,    -1,   242,    -1,    -1,
     245,    -1,    -1,    -1,    -1,    -1,  1902,    -1,    -1,  1003,
      -1,    -1,    -1,   258,    -1,    -1,    -1,    -1,   616,   791,
      -1,    -1,   794,    -1,    -1,    -1,  1020,    -1,    -1,    -1,
     275,   629,    -1,  1027,    -1,    -1,    -1,    -1,   283,    -1,
      -1,  1119,    -1,   288,    -1,    -1,    -1,    70,    -1,   294,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   302,    -1,    -1,
      -1,    -1,    12,    13,    14,    15,    16,    -1,  1964,  1965,
      -1,    -1,    -1,    -1,    -1,  1069,    -1,   322,   101,   324,
     325,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      -1,    -1,    -1,    -1,   339,    -1,    -1,    -1,   343,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,    70,  2005,
      -1,    -1,    -1,    -1,    -1,  1109,    -1,    -1,    -1,    -1,
      70,    -1,   720,  1117,    -1,    -1,   149,   150,    -1,    -1,
     153,   376,    -1,    -1,    -1,    -1,    -1,   160,   161,   101,
      -1,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   101,  1146,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   761,    -1,   410,    -1,    -1,    -1,  1163,
    1164,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,
      -1,    -1,    -1,    -1,  1178,   957,    -1,   149,   150,    -1,
    1184,   436,    -1,   965,    -1,   440,    -1,    -1,  1192,   149,
     150,   446,    -1,   153,    -1,    -1,    -1,    -1,    -1,   101,
     160,   161,   104,   105,   106,   107,   108,   109,   110,   111,
     112,    -1,   994,    -1,    -1,   997,  1220,    -1,   101,    -1,
    1224,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      -1,    -1,    -1,    -1,   697,   698,   699,   700,   701,   702,
     703,   704,   705,   706,   707,   708,   709,   710,   711,   712,
     713,   714,   715,    -1,    -1,    -1,    98,    -1,    -1,    -1,
     515,   516,    -1,    -1,    -1,   520,   521,   109,    -1,    -1,
      -1,    -1,   174,    -1,    -1,  1057,   884,    -1,    -1,   887,
      -1,    -1,    -1,  1287,  1288,    -1,    -1,    -1,    -1,    -1,
      -1,   174,    -1,    -1,    -1,  1383,    -1,   552,    -1,    -1,
      -1,    -1,   557,    -1,    -1,   560,   561,    -1,   563,   151,
      -1,    -1,    -1,    -1,   777,    -1,    -1,    -1,    -1,   574,
      -1,   576,  1326,  1327,  1328,  1329,  1330,  1331,   936,    -1,
      -1,    -1,  1336,  1337,    -1,   590,    -1,   592,   593,    -1,
     595,    -1,    63,    64,    65,    66,    -1,    -1,    -1,    -1,
      -1,  1355,  1356,   195,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   616,    -1,    -1,   619,    -1,    -1,    -1,   623,    -1,
      -1,   626,    -1,    -1,   629,    -1,   631,    -1,    -1,    -1,
     101,  1385,   637,   104,   105,   106,   107,   108,   109,   110,
     111,   112,    -1,    -1,    -1,   650,    -1,   652,   653,    -1,
     655,  1405,    -1,    -1,    -1,    -1,   661,    -1,    -1,   664,
     665,   666,    -1,   101,    -1,    -1,   258,    -1,   106,   107,
     108,   109,   110,   111,   112,  1429,    -1,    -1,   101,  1211,
      -1,    -1,   153,   106,   107,   108,   109,   110,   111,   112,
      -1,    12,    13,    14,    15,    16,   288,    -1,    -1,    -1,
     171,   914,   294,    -1,    -1,  1237,   129,    -1,    -1,    -1,
    1548,   149,   150,  1245,    -1,   720,    -1,    -1,    -1,   932,
      -1,    -1,    -1,  1561,   937,    -1,   149,   150,    -1,    -1,
     153,    -1,   324,    -1,    -1,   948,    -1,   160,   161,   744,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,
      -1,    -1,    -1,    -1,    -1,    -1,   761,    -1,    -1,    -1,
    1598,  1515,    -1,    -1,    -1,  1519,    -1,  1521,    -1,    -1,
      -1,  1525,    -1,  1527,    -1,    -1,    -1,   990,   783,   784,
     101,    -1,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,    -1,    -1,    -1,    -1,    -1,   802,    -1,    -1,
      -1,  1555,  1556,    -1,    -1,    -1,   101,    -1,   129,   104,
     105,   106,   107,   108,   109,   110,   111,   112,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   830,    -1,    -1,   149,   150,
      -1,  1363,    -1,    -1,  1192,    -1,  1590,   230,    -1,   160,
     161,    -1,    -1,    -1,    -1,    -1,    -1,  1601,   853,    -1,
    1604,    -1,    -1,    -1,   446,    -1,   861,    -1,    -1,   864,
    1698,    -1,  1220,   868,  1702,   160,    -1,    -1,    -1,   874,
     875,    -1,    -1,    -1,    -1,  1629,    -1,  1715,    -1,   884,
      -1,   886,   887,    -1,   889,   101,    -1,  1725,   104,   105,
     106,   107,   108,   109,   110,   111,   112,    -1,    -1,    -1,
    1654,  1655,    -1,    -1,  1117,    -1,    -1,    -1,    -1,  1441,
      -1,    -1,    -1,    -1,  1446,  1669,  1670,    -1,    70,  1451,
    1452,  1453,    -1,    -1,    -1,    -1,    -1,    -1,   520,    -1,
    1684,   936,    -1,  1687,    -1,    -1,   152,    -1,    -1,    -1,
      -1,   157,    -1,    -1,    -1,    -1,    -1,    -1,   540,   101,
    1704,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,    -1,  1716,  1801,    -1,    -1,    -1,  1805,   560,    -1,
      -1,  1809,    -1,    -1,    -1,   101,    -1,   129,    -1,  1192,
     106,   107,   108,   109,   110,   111,   112,   113,    -1,  1743,
      -1,   117,    -1,   119,   999,    -1,  1834,   149,   150,    -1,
      -1,    -1,    -1,   595,    -1,    -1,    -1,    -1,   160,   161,
      -1,  1016,  1017,    -1,  1227,  1228,  1229,    -1,    -1,    -1,
     413,  1234,  1235,    -1,   150,    -1,    -1,   619,    -1,    -1,
      -1,  1785,    -1,    -1,   626,    -1,   429,  1875,    -1,   432,
    1794,  1879,    -1,    -1,    -1,  1258,    -1,  1405,    -1,    -1,
      -1,   101,    -1,  1891,   104,   105,   106,   107,   108,   109,
     110,   111,   112,    -1,  1069,    -1,    -1,   659,   660,    -1,
      -1,    -1,    -1,    -1,    -1,  1829,    -1,  1915,    -1,  1917,
    1918,    -1,  1295,  1296,    70,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     1,   489,    -1,     4,    -1,
    1854,    -1,  1940,   153,  1109,  1637,    -1,  1639,  1862,    -1,
      -1,    -1,  1117,    -1,    -1,   101,    -1,    -1,  1872,    -1,
     106,   107,   108,   109,   110,   111,   112,    -1,    -1,    -1,
      -1,    12,    13,    14,    15,    16,    -1,  1975,    -1,    -1,
      -1,  1146,    -1,   129,    -1,  1899,  1900,  1985,  1902,  1903,
      -1,    -1,    58,    -1,    -1,    -1,    -1,    -1,  1163,  1164,
      -1,    -1,   101,   149,   150,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   160,   161,    82,    -1,    -1,  1184,
    2018,    -1,  2020,  1937,    -1,    -1,    -1,  1192,    -1,    70,
      -1,  1196,    -1,    -1,    -1,    -1,   102,    -1,    -1,    -1,
      -1,  2039,    -1,  1208,    -1,    -1,    -1,  2045,    82,    -1,
    1964,  1965,    -1,    -1,    -1,  1220,    -1,  2055,   157,  1224,
     101,    -1,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,    -1,   139,    -1,    -1,    -1,    -1,    -1,   145,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,
      -1,  2005,    -1,     1,    -1,    -1,   162,    -1,    -1,    -1,
      -1,   853,    -1,  1268,  1269,    -1,    -1,    -1,   149,   150,
      -1,    -1,   864,   147,   180,    -1,    -1,  1282,  1283,   160,
     161,    -1,  1287,  1288,    -1,   191,   192,    -1,   162,    -1,
      -1,    -1,   101,    -1,   886,   104,   105,   106,   107,   108,
     109,   110,   111,   112,    -1,   179,    -1,    -1,    -1,    -1,
      58,  1316,  1317,    -1,   220,    -1,    -1,    -1,   192,    -1,
      -1,  1326,  1327,  1328,  1329,  1330,  1331,    -1,    -1,    -1,
     236,    -1,  1337,    -1,    -1,   241,   242,    -1,    -1,   245,
      -1,    -1,    -1,   736,   737,    -1,    -1,    -1,   157,    -1,
    1355,  1356,    -1,   746,   102,    -1,   749,    -1,    -1,    -1,
      -1,   267,    -1,    -1,   270,    -1,   272,    70,    -1,    -1,
      -1,   245,    -1,    -1,    -1,    -1,    -1,   283,    -1,    -1,
    1385,    -1,  1595,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     296,    12,    13,    14,    15,    16,    -1,   145,   101,    -1,
    1405,   104,   105,   106,   107,   108,   109,   110,   111,   112,
      -1,    -1,    -1,    -1,   162,    -1,   322,   810,    -1,   325,
      -1,   814,    -1,    -1,  1429,   818,   129,    -1,   302,    -1,
      -1,    -1,    -1,   339,    -1,    -1,    -1,   343,    -1,    -1,
      -1,    -1,    -1,    -1,   192,    -1,   149,   150,   322,    70,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   160,   161,    -1,
      -1,   367,    -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,
      -1,  1829,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     101,    -1,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,    -1,    -1,    -1,   101,  1501,  1502,   104,   105,
     106,   107,   108,   109,   110,   111,   112,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1521,    -1,    -1,   267,
      -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,   149,   150,
     436,    -1,  1537,    -1,   440,   283,   410,    -1,    -1,   160,
     161,    -1,  1900,   149,   150,    -1,    -1,    -1,    -1,    -1,
    1555,  1556,    -1,    -1,   160,   161,    -1,   463,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     1,   440,    -1,    -1,    -1,
      -1,  1163,    -1,    -1,   322,    -1,    -1,   325,    -1,    -1,
      -1,    -1,    -1,    -1,  1797,  1590,    -1,    -1,    -1,    -1,
      -1,   339,    -1,    -1,    -1,   343,  1601,    -1,    -1,  1604,
      -1,    -1,    -1,    -1,  1196,    -1,    -1,    -1,  1613,    -1,
    1003,    -1,    -1,    -1,    -1,   521,    -1,    -1,    -1,    -1,
      -1,    -1,    58,  1836,  1629,    99,    -1,   101,    -1,   103,
     104,   105,   106,   107,   108,   109,   110,   111,   112,  1644,
      -1,   515,   516,    -1,  1649,  1650,   552,   521,    -1,  1654,
    1655,   557,    -1,   101,    -1,   561,    -1,   563,   106,   107,
     108,   109,   110,   111,   112,   113,   102,    -1,   574,   117,
     576,   119,    -1,    -1,    -1,   149,    -1,    -1,   152,   153,
      -1,  1074,    -1,    -1,  1077,    -1,   592,   593,    -1,    -1,
      -1,    -1,   440,    -1,    -1,    -1,    -1,    -1,    -1,  1704,
     574,   607,   150,    -1,    -1,   153,    -1,    -1,    -1,   145,
     616,  1716,    -1,    -1,    -1,   621,    -1,    -1,  1931,   593,
    1933,    -1,    -1,   629,    -1,    -1,   162,   101,    -1,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,    -1,
      -1,    -1,   616,   101,    -1,  1337,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   129,   192,    -1,  1971,    -1,
      -1,    -1,    -1,   637,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   129,    -1,   521,    -1,   149,   150,    -1,    -1,   153,
      -1,    -1,    -1,    -1,    -1,    -1,   160,   161,    -1,    -1,
    2003,   149,   150,  1385,    -1,    -1,    -1,    -1,   156,   173,
      -1,    -1,   160,   161,   552,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   561,   720,   563,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1829,    -1,   574,    -1,   576,    -1,
      -1,   737,    -1,    -1,    -1,    -1,    -1,    -1,  2051,    -1,
      -1,    -1,    -1,    -1,   592,   593,   720,   283,    -1,  1854,
      -1,    -1,    -1,    -1,    -1,   761,    -1,    -1,    -1,    -1,
     766,    -1,    -1,    -1,    -1,    -1,    -1,  1872,   616,    -1,
     744,  1264,    -1,    -1,    -1,    -1,    -1,   783,   784,    -1,
    1273,   629,    -1,    -1,    -1,    -1,   322,   761,    -1,   325,
      -1,    -1,    -1,    -1,  1899,  1900,   802,    -1,  1903,    -1,
      -1,    -1,    -1,   339,    -1,    -1,    -1,   343,    -1,   783,
     784,   101,    -1,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,    -1,   830,    -1,    -1,    -1,   802,    -1,
      -1,    -1,  1937,    -1,    -1,    -1,    -1,   101,    -1,   129,
     104,   105,   106,   107,   108,   109,   110,   111,   112,    -1,
      -1,    -1,    -1,    -1,    -1,   861,    -1,    -1,    -1,   149,
     150,    -1,   868,  1555,    -1,   129,    -1,    -1,   874,    -1,
     160,   161,   720,    -1,    -1,    -1,    -1,  1982,   884,    -1,
      -1,   887,    -1,   889,    -1,   149,   150,    -1,   894,   153,
      -1,    -1,    -1,    -1,   868,    -1,   160,   161,    -1,    -1,
      -1,   875,    -1,    -1,   440,    -1,    -1,    -1,    -1,    -1,
      -1,   101,    -1,   761,   104,   105,   106,   107,   108,   109,
     110,   111,   112,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     936,    -1,    -1,    -1,    -1,   783,   784,   101,    -1,   129,
      -1,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
      -1,    -1,    -1,   117,   802,   119,    -1,    -1,    -1,   149,
     150,    -1,   936,    -1,    -1,    -1,    -1,    -1,    -1,   101,
     160,   161,   104,   105,   106,   107,   108,   109,   110,   111,
     112,    -1,   830,    -1,    -1,   521,   150,    -1,    -1,   153,
      -1,   101,    -1,   999,   104,   105,   106,   107,   108,   109,
     110,   111,   112,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1016,  1017,  1704,   861,    -1,    -1,   552,    -1,   150,    -1,
     868,   153,  1515,    -1,    -1,   561,    -1,   563,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   884,    -1,   574,   887,
     576,   889,   152,   101,    -1,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,     1,   592,   593,     4,    -1,
      -1,   101,    -1,  1069,    -1,    -1,   106,   107,   108,   109,
     110,   111,   112,   113,    -1,    -1,    -1,   117,    -1,   119,
     616,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   936,    -1,
      -1,    -1,    -1,   629,   152,  1069,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1109,    -1,    -1,    -1,    -1,    -1,    -1,
     150,  1117,    58,   153,   101,    -1,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    82,    -1,    -1,   101,
    1146,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   999,    -1,    -1,    -1,    -1,   102,    -1,  1164,    -1,
      -1,    -1,   149,    -1,    -1,    -1,    -1,    -1,  1016,  1017,
      -1,    -1,    -1,    -1,    -1,    -1,  1669,  1670,    -1,    -1,
      -1,    -1,    -1,    -1,   720,    -1,  1192,   149,    -1,    -1,
      -1,    -1,    -1,   139,    -1,  1201,    -1,    -1,    -1,   145,
      -1,   147,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1184,  1903,    -1,    -1,  1220,    -1,    -1,    -1,  1192,    -1,
      -1,  1069,    -1,    -1,    -1,   761,    -1,    -1,    -1,    -1,
      -1,   101,    -1,   179,   104,   105,   106,   107,   108,   109,
     110,   111,   112,    -1,    -1,   191,  1220,   783,   784,    -1,
     132,   133,   134,   135,   136,   137,   138,   139,   140,   141,
     142,    -1,  1268,  1269,   146,    -1,   802,    -1,    -1,  1117,
      -1,    -1,    -1,    -1,   376,    -1,  1282,  1283,    -1,    -1,
      -1,  1287,  1288,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   173,  1785,    -1,   830,    -1,   242,    -1,  1146,   245,
      -1,    -1,    -1,    -1,   250,    -1,    -1,    -1,    -1,    -1,
    1316,  1317,    -1,  1287,  1288,    -1,  1164,    -1,    -1,    -1,
    1326,  1327,  1328,  1329,    -1,   861,    -1,    -1,    -1,    -1,
      -1,    -1,   868,    -1,    -1,    -1,    -1,   283,    -1,    -1,
      -1,    -1,    -1,    -1,  1192,    -1,    -1,    -1,   884,  1355,
    1356,   887,    -1,   889,    -1,    -1,   302,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1858,    -1,    -1,    -1,  1862,
      -1,    -1,  1220,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1405,
     936,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1902,
      -1,    -1,    -1,   515,   516,    -1,    -1,    -1,    -1,    -1,
    1268,  1269,    -1,  1429,    -1,    -1,    -1,    -1,    -1,    -1,
     376,  1405,    -1,    -1,  1282,  1283,    -1,    -1,    -1,  1287,
    1288,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   999,   410,    -1,    -1,    -1,  1316,  1317,
      -1,  1964,  1965,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1016,  1017,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     436,    -1,    -1,    -1,    -1,  1501,  1502,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1355,  1356,    -1,
      -1,    -1,  2005,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1527,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1537,    -1,  1069,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1521,   650,    -1,
    1556,    -1,    -1,   655,    -1,    -1,    -1,  1405,    -1,   661,
      -1,    86,    -1,    -1,    -1,    -1,    -1,    92,    93,   515,
     516,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   680,    -1,
      -1,  1117,    -1,    -1,  1590,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1601,    -1,    -1,  1604,    -1,
      -1,   126,    -1,    -1,    -1,    -1,    -1,  1613,    -1,    -1,
    1146,   557,    -1,    -1,   716,   561,    -1,   563,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1601,  1164,    -1,
     576,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1644,    -1,
      -1,    -1,    -1,  1649,  1650,    -1,    -1,    -1,  1654,  1655,
      -1,    -1,    -1,  1501,  1502,    -1,  1192,    -1,    -1,     1,
      -1,    -1,     4,    -1,  1670,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1654,  1655,    -1,    -1,  1220,    -1,    -1,    -1,    -1,  1537,
      -1,   637,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   650,    -1,   652,   653,  1556,   655,
    1716,    -1,    -1,    -1,    -1,   661,    58,    -1,   664,   665,
     666,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1268,  1269,    -1,    -1,    -1,    -1,    -1,    -1,
      82,    -1,  1590,    -1,    -1,    -1,  1282,  1283,    -1,    -1,
      -1,  1287,  1288,  1601,    -1,    -1,    -1,    -1,    -1,    -1,
     102,    -1,    -1,    -1,    -1,  1613,    -1,   292,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1316,  1317,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1644,   139,   744,    -1,
      -1,  1649,  1650,   145,    -1,   147,  1654,  1655,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1355,
    1356,    -1,    -1,  1829,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   179,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1854,   191,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1872,    -1,  1716,  1405,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1854,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1899,  1900,    -1,    -1,    -1,    -1,    -1,
     242,    -1,    -1,   245,    -1,    -1,    -1,    -1,   250,    -1,
      -1,    -1,    -1,    -1,   439,   861,   441,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   450,   451,    -1,   874,   875,
      -1,  1937,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   283,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1965,
     302,    -1,    -1,    -1,    -1,  1501,  1502,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1982,    -1,    -1,    -1,
      -1,  1829,    -1,    -1,    -1,    -1,     1,    -1,    -1,     4,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1537,    -1,    -1,    -1,    -1,  1854,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1556,    -1,    -1,    -1,  1872,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   558,   376,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    58,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1899,  1900,    -1,  1590,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1601,    -1,    82,   410,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1613,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   102,    -1,  1937,
      -1,    -1,    -1,    -1,   436,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1644,    -1,
      -1,    -1,    -1,  1649,  1650,    -1,    -1,    -1,  1654,  1655,
      -1,    -1,    -1,    -1,   139,    -1,    -1,    -1,    -1,    -1,
     145,    -1,   147,    -1,  1982,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1109,   179,    -1,    -1,    -1,    -1,    -1,
      -1,  1117,    -1,   515,   516,    -1,   191,    -1,    -1,    -1,
    1716,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1146,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   557,    -1,    -1,  1164,   561,
      -1,   563,    -1,    -1,    -1,    -1,    -1,   242,    -1,    -1,
     245,    -1,    -1,    -1,   576,   250,    -1,    -1,  1184,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1352,    -1,   777,  1355,  1356,    -1,    -1,    -1,    -1,  1361,
      -1,    -1,    -1,  1365,    -1,  1367,    -1,    -1,   283,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1224,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   302,    -1,    -1,
      -1,    -1,    -1,  1829,    -1,   637,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   650,    -1,
     652,   653,    -1,   655,    -1,    -1,    -1,    -1,  1854,   661,
      -1,    -1,   664,   665,   666,    -1,    -1,    -1,    -1,   854,
     855,    -1,    -1,    -1,    -1,    -1,  1872,    -1,    -1,    -1,
     865,   866,   867,    -1,    -1,   870,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   376,    -1,  1899,  1900,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1326,  1327,  1328,  1329,  1330,  1331,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   410,    -1,    -1,    -1,    -1,
      -1,  1937,   744,    -1,    -1,  1507,    -1,    -1,    -1,  1355,
    1356,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   436,    -1,    -1,    -1,   950,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     0,  1539,    -1,     3,
      -1,    -1,    -1,    -1,    -1,    -1,  1982,    -1,    -1,  1551,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1566,  1567,    -1,    -1,    -1,    -1,
      -1,   996,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1429,    -1,    -1,    -1,    -1,  1590,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     515,   516,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    76,    -1,    -1,    -1,    -1,    -1,  1043,   861,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1052,  1053,  1054,
    1055,    -1,   874,   875,    -1,  1060,  1061,    -1,    -1,    -1,
      -1,    -1,   557,    -1,    -1,  1070,   561,    -1,   563,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   576,    -1,    -1,    -1,    -1,  1091,    -1,  1093,    -1,
      -1,   135,    -1,    -1,    -1,  1521,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1700,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1710,    -1,
    1556,  1713,  1714,    -1,  1716,    -1,    -1,    -1,    -1,  1721,
      -1,  1146,   637,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   650,    -1,   652,   653,    -1,
     655,    -1,    -1,    -1,  1590,    -1,   661,  1172,    -1,   664,
     665,   666,    -1,    -1,  1179,    -1,  1181,  1182,  1604,    -1,
      -1,    -1,    -1,    -1,    -1,   229,  1191,    -1,  1193,    -1,
    1195,    -1,  1197,    -1,    -1,    -1,    -1,  1202,    -1,    -1,
     244,    -1,    -1,  1629,    -1,    -1,    -1,    -1,    -1,    -1,
     254,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     264,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1815,   278,   279,    -1,    -1,  1820,  1821,
      -1,   285,   286,    -1,    -1,    -1,    -1,    -1,    -1,   744,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   301,  1263,    -1,
      -1,    -1,    -1,    -1,    -1,  1270,  1271,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   321,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1109,    -1,  1294,
    1716,    -1,    -1,    -1,    -1,  1117,  1301,    -1,    -1,  1881,
    1305,    -1,  1884,  1885,    -1,    -1,    -1,  1889,  1890,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1146,    -1,    -1,    -1,    -1,    -1,
    1335,    -1,    -1,   377,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1164,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1184,    -1,   408,    -1,   861,    -1,    -1,    -1,
    1375,    -1,    -1,    -1,    -1,  1957,  1958,  1959,    -1,   874,
     875,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     434,    -1,    -1,    -1,   438,    -1,  1978,    -1,    -1,    -1,
      -1,    -1,  1224,    -1,  1409,    -1,    -1,    -1,    -1,  1991,
    1992,  1993,  1417,   457,  1419,    -1,    -1,   461,   462,    -1,
      -1,   465,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   480,   481,   482,   483,
      -1,    -1,    -1,    -1,    -1,    -1,  1872,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   499,    -1,    -1,    -1,    -1,
      -1,  1466,  1467,   507,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1899,    -1,    -1,  1481,  1482,    -1,  1484,
      -1,    -1,    -1,    -1,    -1,     1,    -1,    -1,  1493,    -1,
      -1,   535,    -1,    -1,    -1,    -1,    -1,    -1,  1503,  1504,
      -1,    17,    -1,    -1,  1326,  1327,  1328,  1329,  1330,  1331,
      -1,  1937,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   566,    -1,    -1,    -1,    -1,    -1,    -1,   573,
      -1,    -1,    48,  1355,  1356,   579,    52,    -1,    54,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    69,    -1,    71,    72,    -1,    74,   603,
     604,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    91,    92,    93,    94,    -1,
      96,    -1,    98,    99,    -1,   101,    -1,   103,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,    -1,   121,   122,  1429,    -1,  1614,
    1615,    -1,    -1,   129,  1109,    -1,    47,    -1,    -1,    -1,
    1625,    -1,  1117,   667,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   149,    -1,    -1,   152,   153,    -1,    -1,
      -1,   157,    73,   159,   160,   161,   162,   163,   164,   165,
      -1,  1146,    -1,    -1,    -1,  1660,  1661,    -1,   174,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1164,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   119,  1184,
      -1,    -1,   736,    -1,    -1,    -1,    -1,    -1,    -1,  1521,
      -1,   132,    -1,   134,    -1,    -1,    -1,   751,    -1,    -1,
      -1,   755,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     764,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1224,
      98,    -1,    -1,   164,  1556,    -1,    -1,    -1,    -1,    -1,
    1745,   109,   786,   111,    -1,   113,    -1,    -1,    -1,    -1,
     181,   795,    -1,    -1,    -1,    -1,    -1,   801,    -1,  1764,
      -1,    -1,  1767,  1768,    -1,    -1,    -1,    -1,  1590,  1774,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1604,   151,    -1,   153,   154,    -1,   219,    -1,
      -1,    -1,   223,    -1,   838,   226,   227,    -1,    -1,   230,
      -1,   845,   233,   234,    -1,    -1,    -1,  1629,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   195,   872,    -1,
      -1,  1326,  1327,  1328,  1329,  1330,  1331,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1355,  1356,    -1,    -1,   295,    -1,    -1,   298,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   926,    -1,    -1,    -1,    -1,    -1,   319,   320,
     258,    -1,   260,   261,  1716,    -1,  1901,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   335,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     288,    -1,    -1,    -1,    -1,    -1,   294,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1429,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   324,  1962,    -1,    -1,
      -1,    -1,   330,    -1,   332,  1009,    -1,    -1,    -1,  1013,
      -1,    -1,    -1,    -1,    -1,    -1,  1020,    -1,    -1,    -1,
      -1,  1986,    -1,    -1,    -1,    -1,  1030,    -1,    -1,    -1,
      -1,    -1,    -1,  1037,    -1,    -1,  2001,    -1,   429,    -1,
      -1,    -1,  1046,    -1,  1048,    -1,    -1,    -1,    -1,    -1,
      -1,  2016,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1521,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1080,    -1,    -1,    -1,
    1084,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1872,    -1,   420,   484,  1098,    -1,    -1,  1101,    -1,    -1,
      -1,  1556,    -1,    -1,    -1,   496,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1899,   446,    -1,
     448,   449,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1590,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1604,
      -1,    -1,    -1,    -1,    -1,  1937,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   493,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1629,    -1,    -1,    -1,    -1,   570,
      -1,    -1,    -1,    -1,    -1,   513,  1190,    -1,    -1,    -1,
     518,    -1,   520,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1215,   540,    -1,   542,   543,    -1,    -1,    -1,    -1,
     611,   612,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   560,   624,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   572,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   162,
      -1,  1716,    -1,    -1,    -1,    -1,    -1,   595,    -1,   597,
     598,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   191,   192,
      -1,   619,   620,    -1,    -1,    -1,    -1,    -1,   626,    -1,
      -1,    -1,    -1,    -1,  1308,    -1,    -1,    -1,  1312,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     223,    -1,    -1,    -1,    -1,    -1,    -1,   230,    -1,    -1,
      -1,   659,   660,    -1,    -1,    -1,    -1,    -1,  1342,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   740,
     741,    -1,    -1,    -1,    -1,   746,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    55,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   767,    -1,    -1,   770,
     771,    -1,   773,    -1,   775,   776,    -1,    -1,  1392,    -1,
      -1,  1395,    -1,    -1,    -1,   298,    -1,    -1,    -1,    90,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1411,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1872,    82,   322,
     323,    -1,    -1,   814,    -1,    -1,    -1,   818,    -1,    -1,
      -1,    -1,    -1,    -1,    98,    -1,    -1,    -1,    -1,    -1,
     343,    -1,    -1,    -1,  1899,    -1,    -1,    -1,    -1,   140,
      -1,    -1,   143,    -1,    -1,    -1,    -1,    -1,  1462,    -1,
      -1,    -1,    -1,    -1,    -1,   156,    -1,  1471,    -1,    -1,
      -1,  1475,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1937,   147,    -1,  1489,  1490,   151,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   162,    -1,
      -1,    -1,    -1,    -1,   895,    -1,    -1,    -1,    -1,    -1,
     413,    -1,    -1,    -1,    -1,   179,    -1,    -1,    -1,   210,
      -1,    -1,    -1,    -1,    -1,   853,   429,   430,   192,   432,
     433,   195,    -1,    -1,    -1,    -1,   864,   440,    -1,    -1,
      -1,   444,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   886,    -1,
      -1,    -1,    -1,    -1,   255,    -1,    -1,    -1,    -1,   897,
      -1,    -1,    -1,    -1,   265,    -1,    -1,    -1,   906,    -1,
      -1,   245,   485,    -1,    -1,   276,   489,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   258,    -1,    -1,    -1,    -1,    -1,
      -1,  1605,  1606,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   303,    -1,    -1,    -1,    -1,    -1,   521,   310,
     311,    -1,    -1,    -1,   315,    -1,    -1,     1,    -1,    -1,
     294,    -1,    -1,    -1,    -1,  1026,    -1,    -1,   302,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   348,   322,    -1,
     324,    -1,   353,    -1,    -1,   356,    -1,    -1,   571,    -1,
      -1,   574,    -1,    -1,    48,  1003,  1067,    -1,    52,    -1,
      54,    -1,    -1,  1074,    -1,    -1,  1077,    -1,    -1,   592,
     593,    -1,    -1,    -1,    -1,    -1,    -1,    71,    -1,  1027,
     603,    -1,    -1,    -1,   607,    -1,    -1,    -1,    -1,    -1,
      -1,   614,   376,   616,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    98,    99,  1730,   101,    -1,   103,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,   117,   118,   119,   410,   121,   122,    -1,
    1754,    -1,   443,    -1,    47,   129,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   455,   456,    -1,    -1,  1772,    -1,
      -1,    -1,    -1,    -1,    -1,   149,   440,    -1,   152,   153,
      -1,    -1,   446,    -1,    -1,   159,   160,   161,   162,   163,
     164,   165,    -1,    -1,    -1,  1799,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1198,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1206,  1207,   720,    -1,    -1,
      -1,  1825,    -1,    -1,  1828,    -1,   119,    -1,    -1,    -1,
      -1,    -1,    -1,   736,   737,  1163,    -1,    -1,    -1,   132,
      -1,   134,    -1,   746,   747,    -1,   749,   750,    -1,    -1,
      -1,   515,   516,    -1,    -1,    -1,    -1,   521,   761,    -1,
      -1,   764,    -1,   766,   767,    -1,    -1,    -1,  1196,    -1,
     773,    -1,    -1,  1264,  1202,    -1,    -1,    -1,    -1,    -1,
     783,   784,  1273,    -1,    -1,  1276,    -1,  1278,  1279,    -1,
      -1,    -1,    -1,    -1,    -1,   586,    -1,    -1,    -1,   802,
      -1,    -1,    -1,   806,    -1,    -1,    -1,   810,    -1,    -1,
     574,   814,   815,    -1,    -1,   818,   819,    -1,    -1,    -1,
      -1,    -1,  1926,   826,    -1,    -1,    -1,    -1,  1319,   593,
      -1,   595,    -1,   226,   227,    -1,    -1,   230,    -1,    -1,
     233,   234,    -1,    -1,   635,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   616,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   868,   869,    -1,    -1,    -1,
      -1,    -1,    -1,   637,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   650,    -1,   652,   653,
      -1,   655,    -1,    -1,   897,  1386,    -1,   661,    -1,    -1,
     664,   665,   666,    -1,    -1,    -1,    -1,    -1,    -1,  1337,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   319,   320,    -1,    -1,
      -1,    -1,    -1,   936,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   335,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   720,  1385,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1470,
     744,    -1,    -1,    -1,    -1,    -1,    -1,   778,    -1,    -1,
      -1,    -1,    -1,    -1,   785,    -1,    -1,   761,    -1,    -1,
    1003,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1499,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1020,  1021,   783,
     784,    -1,    -1,    -1,  1027,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1525,    -1,   429,    -1,   802,    -1,
    1531,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1069,    -1,    -1,   860,
      -1,  1074,  1075,    -1,  1077,  1078,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   882,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   496,   868,    -1,    -1,    -1,    -1,  1600,
      -1,   875,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   912,   886,    -1,   179,   916,    -1,  1555,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   192,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     205,    -1,   207,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   936,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1676,  1677,    -1,    -1,  1192,
      -1,    -1,    -1,  1684,    -1,  1198,  1199,  1688,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1220,   611,   612,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   624,    -1,    -1,    -1,    -1,    -1,    -1,   293,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1687,
      -1,  1264,  1265,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1273,  1274,    -1,  1276,    -1,    -1,  1704,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1287,  1288,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1787,    12,    13,    14,
      15,    16,    -1,    -1,    19,  1069,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  1122,  1123,  1124,    -1,    50,    51,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1109,    -1,   740,   741,    -1,
      -1,    -1,    -1,   746,    -1,    70,    -1,  1785,    -1,    -1,
      -1,  1152,    -1,    -1,    -1,    -1,    -1,  1858,    -1,    -1,
      -1,    -1,    -1,    -1,   767,    -1,  1167,   770,   771,    -1,
     773,    -1,   775,   776,    -1,    -1,   101,     5,    -1,    -1,
      -1,    -1,    -1,    -1,    12,    13,    14,    15,    16,  1163,
      -1,    -1,  1405,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   466,    -1,    -1,  1205,    -1,    -1,   472,    -1,    -1,
    1184,   814,   477,    -1,    -1,   818,    -1,    -1,  1192,    -1,
      48,    -1,    -1,    -1,    52,    -1,    54,    -1,    -1,    -1,
      -1,   156,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    70,    71,    -1,    -1,  1220,    -1,    -1,    -1,
    1224,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1964,    -1,  1903,    -1,    -1,    -1,    -1,
      98,    99,    -1,   101,    -1,   103,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,    -1,   117,
     118,   119,   895,   121,   122,    -1,    -1,    -1,    -1,   564,
      -1,   129,  1515,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1287,  1288,    -1,    -1,    -1,  1531,    -1,
      -1,   149,    -1,    -1,   152,   153,    -1,    -1,   593,    -1,
      -1,   159,   160,   161,   162,   163,   164,   165,    -1,  1340,
      -1,   606,  1343,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1326,  1327,  1328,    -1,  1330,  1331,    -1,    -1,
      -1,    -1,    -1,  1337,    -1,    -1,    -1,  2005,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1601,    -1,
      -1,    -1,   657,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1385,    -1,   678,   679,    -1,    -1,   682,    -1,   684,
      -1,    -1,    -1,  1026,    -1,   690,    -1,   692,   693,    -1,
      -1,  1405,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1654,  1655,    -1,    48,    -1,    -1,    -1,    52,    -1,
      54,    -1,    -1,    -1,    -1,   720,  1669,  1670,    -1,    -1,
      -1,    -1,    -1,    -1,  1067,    -1,    -1,    71,   733,    -1,
      -1,  1074,  1685,    -1,  1077,    -1,    -1,    -1,    -1,   744,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   758,    98,    99,   761,   101,    -1,   103,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,   117,   118,   119,    -1,   121,   122,    -1,
      -1,    -1,    -1,   788,    -1,   129,   791,    -1,    -1,    -1,
      -1,    -1,    -1,  1534,    -1,    -1,    -1,    -1,    -1,   143,
     144,   145,    -1,    -1,    -1,   149,   150,  1521,   152,   153,
      -1,    -1,    -1,    -1,    -1,   159,   160,   161,   162,   163,
     164,   165,   827,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1785,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1793,  1555,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1198,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1206,  1207,    -1,    -1,    -1,    -1,    -1,
     875,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1621,   886,   887,    -1,    -1,    -1,    -1,  1601,    -1,   894,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1854,    -1,    -1,    48,  1858,  1859,    -1,    52,  1862,
      54,    -1,    -1,    -1,    -1,  1629,    -1,    -1,   923,    -1,
      -1,  1264,    -1,    -1,    -1,    -1,    -1,    71,    -1,    -1,
    1273,   936,    -1,  1276,    -1,  1278,  1279,    -1,    -1,   944,
    1654,  1655,    -1,    -1,    -1,    -1,   951,    -1,    -1,  1902,
      -1,    -1,    -1,    -1,    98,    99,    -1,   101,    -1,   103,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,   117,   118,   119,  1319,   121,   122,    -1,
      -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,
    1704,    -1,   997,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1742,    -1,    -1,    -1,   149,   150,    -1,   152,   153,
    1751,  1964,  1965,   157,    -1,   159,   160,   161,   162,   163,
     164,   165,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1386,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2005,    -1,    -1,    -1,    -1,    -1,    -1,  1064,
      -1,  1066,     1,  1068,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    -1,
      19,    -1,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    -1,
      49,    50,    51,    -1,    53,  1829,    55,    56,    57,    58,
      59,    60,    61,    62,    63,    -1,    -1,  1470,    67,  1134,
    1135,    70,    -1,    -1,    -1,    -1,    75,    76,    -1,    -1,
    1854,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1499,    -1,    -1,    -1,
      -1,   100,    -1,    -1,    -1,   104,   105,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1900,    -1,    -1,  1903,
     129,  1196,    -1,    -1,    -1,    -1,    -1,  1202,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1211,    -1,     1,   148,
      -1,    -1,    -1,   152,   153,  1220,    -1,    -1,    -1,    -1,
      -1,   160,   161,    -1,    17,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1237,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1252,    -1,    -1,
    1255,    -1,    -1,    -1,    -1,    48,    -1,  1600,    -1,    52,
      -1,    54,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    69,    -1,    71,    72,
      -1,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,    87,    88,    89,    90,    91,    92,
      93,    94,  1307,    96,    -1,    98,    99,    -1,   101,    -1,
     103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,    -1,   121,   122,
      -1,    -1,    -1,  1676,  1677,    -1,   129,    -1,    -1,    -1,
    1345,    -1,    -1,    -1,    -1,  1688,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   148,   149,    -1,  1363,   152,
     153,  1366,    -1,    -1,   157,    -1,   159,   160,   161,   162,
     163,   164,   165,    -1,    -1,     1,    -1,    -1,    -1,    -1,
      -1,   174,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1405,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1415,  1416,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    48,    -1,    -1,    -1,    52,    -1,    54,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1441,    -1,  1443,    -1,
      -1,    -1,    -1,    69,  1787,    71,    72,    -1,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    91,    92,    93,    94,    -1,
      96,    -1,    98,    99,    -1,   101,    -1,   103,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,    48,   121,   122,    -1,    52,    -1,
      54,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1858,  1521,    71,    -1,    -1,
      -1,  1526,   148,   149,    -1,    -1,   152,   153,    -1,    -1,
      -1,   157,    -1,   159,   160,   161,   162,   163,   164,   165,
      -1,    -1,    -1,    -1,    98,    99,    -1,   101,   174,   103,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,   117,   118,   119,    -1,   121,   122,    -1,
      -1,    -1,    -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,
      -1,  1586,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   143,
     144,   145,    -1,    -1,    -1,   149,   150,    -1,   152,   153,
      -1,    -1,    -1,    -1,    -1,   159,   160,   161,   162,   163,
     164,   165,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1627,    -1,    -1,  1630,     3,     4,     5,     6,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   174,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      -1,    19,  1837,    21,    22,    23,    24,    25,    26,    27,
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
      -1,    67,    -1,    -1,    70,     5,    -1,    -1,    -1,    75,
      76,    -1,    12,    13,    14,    15,    16,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   100,    -1,    -1,    -1,   104,   105,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    -1,
      -1,    -1,    52,    -1,    54,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   148,    -1,    -1,    -1,   152,   153,    -1,    -1,
      -1,    -1,    -1,    -1,   160,   161,    -1,    -1,    98,    99,
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
      -1,    -1,    -1,    -1,    -1,    -1,    12,    13,    14,    15,
      16,    -1,    70,    19,    -1,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      -1,    -1,    -1,   101,    50,    51,   104,   105,   106,   107,
     108,   109,   110,   111,   112,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,
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
      40,    41,    42,    43,    44,    45,    -1,    -1,    -1,    -1,
      50,    51,    -1,    -1,   104,   105,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      70,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     3,    -1,    -1,    -1,
      -1,   101,   152,   153,    -1,    12,    13,    14,    15,    16,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     149,   150,    -1,   152,   153,    -1,    -1,    -1,   157,    -1,
      -1,   160,   161,     4,     5,     6,     7,     8,     9,    10,
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
     111,   112,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    17,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,   150,
      48,   152,   153,    -1,    52,    -1,    54,    -1,    -1,   160,
     161,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    69,    -1,    71,    -1,    -1,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    90,    91,    92,    93,    94,    -1,    96,    -1,
      98,    99,    -1,   101,    -1,   103,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,    -1,   121,   122,    -1,    -1,    -1,    -1,    -1,
      -1,   129,    -1,    -1,    -1,    -1,    -1,    17,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   149,    -1,    -1,   152,   153,    -1,    -1,    -1,   157,
      -1,   159,   160,   161,   162,   163,   164,   165,    48,    -1,
      -1,    -1,    52,    -1,    54,    -1,   174,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    69,
      -1,    71,    -1,    -1,    74,    -1,    -1,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    92,    93,    94,    -1,    96,    -1,    98,    99,
      -1,   101,    -1,   103,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
      -1,   121,   122,    -1,    -1,    -1,    -1,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,
      -1,    -1,   152,   153,    -1,    -1,    -1,   157,    -1,   159,
     160,   161,   162,   163,   164,   165,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   174,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    -1,
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
     129,    -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   151,   152,   153,    -1,    -1,   104,   105,    -1,
      -1,   160,   161,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    -1,    19,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,   152,    -1,    -1,    -1,    50,
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
     151,   152,   153,    -1,    -1,   104,   105,    -1,    -1,   160,
     161,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    -1,    19,    -1,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,   152,    -1,    -1,    -1,    50,    51,    -1,
      -1,    -1,    55,    56,    57,    58,    59,    60,    61,    62,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   104,   105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,   131,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,
     153,    -1,    -1,    -1,    -1,    -1,    -1,   160,   161,     4,
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
      -1,    -1,    -1,    -1,   129,    -1,   131,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,   153,    -1,
      -1,    -1,    -1,    -1,    -1,   160,   161,     4,     5,     6,
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
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   152,   153,    -1,    -1,    -1,
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
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   152,   153,    -1,    -1,    -1,    -1,    -1,
      -1,   160,   161,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    -1,    -1,    19,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    -1,    49,    50,
      51,    -1,    53,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    12,    13,    14,    15,    16,    -1,    -1,    19,    70,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    -1,    49,    50,
      51,    -1,    53,   104,   105,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   152,   153,    -1,    -1,    -1,    -1,    -1,    -1,   160,
     161,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    -1,    -1,    48,    -1,    50,
      51,    52,    -1,    54,    55,    56,    57,    58,    59,    60,
      61,    62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    83,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    98,    99,    -1,
     101,    -1,   103,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,   117,   118,   119,    -1,
     121,   122,    -1,    -1,    -1,    -1,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,    -1,
      -1,   152,   153,    -1,    -1,    -1,   157,    -1,   159,   160,
     161,   162,   163,   164,   165,    12,    13,    14,    15,    16,
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
      -1,    -1,   149,    -1,   151,   152,   153,    -1,    -1,    -1,
      -1,    -1,   159,   160,   161,   162,   163,   164,   165,    12,
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
     153,    -1,    -1,    -1,   157,    -1,   159,   160,   161,   162,
     163,   164,   165,    12,    13,    14,    15,    16,    17,    -1,
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
     159,   160,   161,   162,   163,   164,   165,    12,    13,    14,
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
     165,    12,    13,    14,    15,    16,    17,    -1,    19,    -1,
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
     161,   162,   163,   164,   165,    12,    13,    14,    15,    16,
      17,    -1,    19,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    -1,
      -1,    -1,    -1,    50,    51,    -1,    -1,    -1,    55,    56,
      57,    58,    59,    60,    61,    62,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   101,    -1,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   149,   150,    -1,   152,   153,    -1,    -1,    -1,
     157,    -1,    -1,   160,   161,    12,    13,    14,    15,    16,
      17,    -1,    19,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    -1,
      -1,    -1,    -1,    50,    51,    -1,    -1,    -1,    55,    56,
      57,    58,    59,    60,    61,    62,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   101,    -1,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   149,   150,    -1,   152,   153,    -1,    -1,    -1,
      -1,    -1,    -1,   160,   161,    12,    13,    14,    15,    16,
      17,    -1,    19,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    -1,
      -1,    -1,    -1,    50,    51,    -1,    -1,    -1,    55,    56,
      57,    58,    59,    60,    61,    62,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   101,    -1,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   152,   153,    -1,    12,    13,
      14,    15,    16,   160,   161,    19,    -1,    21,    22,    23,
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
      -1,    -1,    -1,    -1,    -1,    -1,   160,   161,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
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
      -1,    12,    13,    14,    15,    16,    17,    70,    19,    -1,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    -1,    -1,   152,    -1,    50,
      51,   104,   105,    -1,    55,    56,    57,    58,    59,    60,
      61,    62,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,
      -1,    -1,    -1,    -1,    -1,    76,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,   152,
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
      -1,    -1,    -1,    -1,    -1,   152,   153,    -1,    -1,    -1,
      -1,    -1,    -1,   160,   161,    12,    13,    14,    15,    16,
      17,    -1,    19,    -1,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    -1,
      -1,    -1,    -1,    50,    51,    -1,    -1,    -1,    55,    56,
      57,    58,    59,    60,    61,    62,    -1,    12,    13,    14,
      15,    16,    17,    70,    19,    -1,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    -1,    -1,    -1,    -1,    50,    51,   104,   105,    -1,
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
      -1,    -1,    -1,    -1,    -1,   160,   161,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    -1,    -1,    19,    -1,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    -1,    49,    50,    51,    -1,    53,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    12,    13,
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
     111,   112,   113,   114,   115,    -1,   117,   118,   119,    48,
     121,   122,    -1,    52,    -1,    54,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    71,   152,    -1,    -1,    -1,    -1,   149,   150,
      -1,   152,   153,    -1,    -1,    -1,   157,    -1,   159,   160,
     161,   162,   163,   164,   165,    -1,    -1,    -1,    -1,    98,
      99,    -1,   101,    -1,   103,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,    -1,   117,   118,
     119,    48,   121,   122,    -1,    52,    -1,    54,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,
     149,   150,   151,   152,   153,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,   149,   150,    -1,   152,   153,    -1,
      -1,    -1,    -1,    -1,   159,   160,   161,   162,   163,   164,
     165,    -1,    -1,    -1,    -1,    98,    99,    -1,   101,    -1,
     103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,    -1,   117,   118,   119,    48,   121,   122,
      -1,    52,    -1,    54,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      71,    -1,    -1,    -1,    -1,    -1,   149,    -1,    -1,   152,
     153,    -1,    -1,    -1,   157,    -1,   159,   160,   161,   162,
     163,   164,   165,    -1,    -1,    -1,    -1,    98,    99,    -1,
     101,    -1,   103,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,   117,   118,   119,    48,
     121,   122,    -1,    52,    -1,    54,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,   149,   150,
      -1,   152,   153,    -1,    -1,    -1,    -1,    -1,   159,   160,
     161,   162,   163,   164,   165,    -1,    -1,    -1,    -1,    98,
      99,    -1,   101,    -1,   103,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,    -1,   117,   118,
     119,    48,   121,   122,    -1,    52,    -1,    54,    -1,    -1,
     129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,
     149,    -1,    -1,   152,   153,    -1,    -1,   156,    -1,    -1,
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
      -1,    -1,    -1,    -1,   149,   150,    -1,   152,   153,    -1,
      -1,    -1,    -1,    -1,   159,   160,   161,   162,   163,   164,
     165,    -1,    -1,    -1,    -1,    98,    99,    -1,   101,    -1,
     103,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,    -1,   117,   118,   119,    48,   121,   122,
      -1,    52,    -1,    54,    -1,    -1,   129,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      71,    -1,    -1,    -1,    -1,    -1,   149,    -1,    -1,   152,
     153,    -1,    -1,    -1,   157,    -1,   159,   160,   161,   162,
     163,   164,   165,    -1,    -1,    -1,    -1,    98,    99,    -1,
     101,    -1,   103,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,   117,   118,   119,    48,
     121,   122,    -1,    52,    -1,    54,    -1,    -1,   129,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,   149,    -1,
     151,   152,   153,    -1,    -1,    -1,    -1,    -1,   159,   160,
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
      -1,    -1,   149,   150,    -1,   152,   153,    -1,    -1,    -1,
      -1,    -1,   159,   160,   161,   162,   163,   164,   165,    -1,
      -1,    -1,    -1,    98,    99,    -1,   101,    -1,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,   117,   118,   119,    48,   121,   122,    -1,    52,
      -1,    54,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    -1,
      -1,    -1,    -1,    -1,   149,   150,    -1,   152,   153,    -1,
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
      -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,   149,   150,
      -1,   152,   153,    -1,    -1,    -1,    -1,    -1,   159,   160,
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
      -1,    -1,   149,   150,    -1,   152,   153,    -1,    -1,    -1,
      -1,    -1,   159,   160,   161,   162,   163,   164,   165,    -1,
      -1,    -1,    -1,    98,    99,    -1,   101,    -1,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,   117,   118,   119,    48,   121,   122,    -1,    52,
      -1,    54,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    -1,
      -1,    -1,    -1,    -1,   149,   150,    -1,   152,   153,    -1,
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
      -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,   149,   150,
      -1,   152,   153,    -1,    -1,    -1,    -1,    -1,   159,   160,
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
      -1,    -1,   149,   150,    -1,   152,   153,    -1,    -1,    -1,
      -1,    -1,   159,   160,   161,   162,   163,   164,   165,    -1,
      -1,    -1,    -1,    98,    99,    -1,   101,    -1,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,   117,   118,   119,    48,   121,   122,    -1,    52,
      -1,    54,    -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    -1,
      -1,    -1,    -1,    -1,   149,   150,    -1,   152,   153,    -1,
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
      -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,   149,   150,
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
     117,   118,   119,    -1,   121,   122,    -1,    -1,    -1,    -1,
      -1,    -1,   129,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   149,    -1,    -1,   152,   153,    -1,    -1,    -1,
      -1,    -1,   159,   160,   161,   162,   163,   164,   165
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
     235,   236,   237,   151,   221,    73,   223,   150,   221,    73,
     174,   104,   150,   221,   222,   243,   150,   221,   223,   242,
     245,   245,   174,   221,   148,   157,   237,   223,   149,   176,
     174,   182,   151,   156,   151,   151,   155,   156,   251,   255,
     359,   401,   177,   154,   154,   347,   464,   148,   148,   154,
     154,   177,   177,   177,   176,   177,   151,   151,   151,   151,
     151,   449,   404,   336,     1,   213,   233,   234,   402,     1,
     156,     1,   176,   223,   235,    73,   174,   151,   223,    73,
     165,   165,   223,   222,   245,   245,   174,   104,   221,   165,
     165,    73,   150,   221,   150,   221,   222,   174,     1,   176,
     176,   262,   295,   297,   458,   156,   174,   153,   182,   267,
     268,   269,   223,   198,   188,    73,   106,   252,   254,   151,
     464,   148,   151,   151,   151,   354,   149,   404,   441,   442,
     338,   131,     1,   155,   156,   148,   272,   273,   279,   223,
      73,   174,   223,   150,   150,   221,   150,   221,   150,   221,
     222,   150,   221,   150,   221,   223,   165,   165,   165,   165,
     148,   272,   262,   177,   149,   196,   401,   449,   180,   156,
     101,   149,   151,   156,   155,    73,   151,   223,   149,   223,
     223,   148,   176,   213,   233,   236,   238,   239,   279,   223,
     165,   165,   165,   165,   150,   150,   221,   150,   221,   150,
     221,   238,   177,   174,   259,   297,   267,   154,   213,   174,
     267,   269,   223,   221,   107,   107,   352,   223,   228,   177,
     236,   150,   150,   221,   150,   221,   150,   221,   177,   259,
     212,   151,   156,   182,   151,   151,   156,   151,   255,    73,
     250,   177,     1,   223,   148,   228,   148,   151,   225,   182,
     270,   149,   174,   270,   223,    73,   151,   225,   155,   156,
     213,   151,   223,   182,   180,   271,   151,   174,   151,   155,
     174,   180
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
       3,     3,     5,     5,     5,     5,     3,     5,     5,     5,
       3,     4,     5,     5,     5,     5,     7,     7,     7,     7,
       7,     7,     7,     2,     3,     4,     4,     4,     6,     6,
       6,     6,     6,     6,     6,     3,     4,     1,     1,     1,
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
#line 567 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.enterScope(); }
#line 7272 "Parser/parser.cc"
    break;

  case 3:
#line 571 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 7278 "Parser/parser.cc"
    break;

  case 4:
#line 578 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantInteger( *(yyvsp[0].tok) ) ); }
#line 7284 "Parser/parser.cc"
    break;

  case 5:
#line 579 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7290 "Parser/parser.cc"
    break;

  case 6:
#line 580 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7296 "Parser/parser.cc"
    break;

  case 7:
#line 581 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7302 "Parser/parser.cc"
    break;

  case 8:
#line 582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantChar( *(yyvsp[0].tok) ) ); }
#line 7308 "Parser/parser.cc"
    break;

  case 19:
#line 603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 7314 "Parser/parser.cc"
    break;

  case 20:
#line 607 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.constant) = build_constantStr( *(yyvsp[0].str) ); }
#line 7320 "Parser/parser.cc"
    break;

  case 21:
#line 611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 7326 "Parser/parser.cc"
    break;

  case 22:
#line 613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 7336 "Parser/parser.cc"
    break;

  case 23:
#line 624 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 7342 "Parser/parser.cc"
    break;

  case 24:
#line 626 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 7348 "Parser/parser.cc"
    break;

  case 25:
#line 630 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_dimensionref( (yyvsp[0].tok) ) ); }
#line 7354 "Parser/parser.cc"
    break;

  case 27:
#line 633 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 7360 "Parser/parser.cc"
    break;

  case 28:
#line 635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new StmtExpr( dynamic_cast<CompoundStmt *>(maybeMoveBuild<Statement>((yyvsp[-1].sn)) ) ) ); }
#line 7366 "Parser/parser.cc"
    break;

  case 29:
#line 637 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7372 "Parser/parser.cc"
    break;

  case 30:
#line 639 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7378 "Parser/parser.cc"
    break;

  case 31:
#line 641 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild<Expression>( (yyvsp[-3].en) );
			(yyval.en) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 7388 "Parser/parser.cc"
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
#line 7400 "Parser/parser.cc"
    break;

  case 33:
#line 659 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "Identifier \"", *(yyvsp[-1].tok).str, "\" cannot appear before a type. "
											   "Possible problem is misspelled storage or CV qualifier." ) );
			(yyval.en) = nullptr;
		}
#line 7410 "Parser/parser.cc"
    break;

  case 35:
#line 669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// steal the association node from the singleton and delete the wrapper
			(yyvsp[-2].genexpr)->associations.splice((yyvsp[-2].genexpr)->associations.end(), (yyvsp[0].genexpr)->associations);
			delete (yyvsp[0].genexpr);
			(yyval.genexpr) = (yyvsp[-2].genexpr);
		}
#line 7421 "Parser/parser.cc"
    break;

  case 36:
#line 679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuildType((yyvsp[-2].decl)), maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } );
		}
#line 7430 "Parser/parser.cc"
    break;

  case 37:
#line 684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } ); }
#line 7436 "Parser/parser.cc"
    break;

  case 39:
#line 693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-5].en), new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) ) ) )) ) ); }
#line 7442 "Parser/parser.cc"
    break;

  case 40:
#line 699 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7448 "Parser/parser.cc"
    break;

  case 41:
#line 701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7454 "Parser/parser.cc"
    break;

  case 42:
#line 703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, new ExpressionNode( (yyvsp[-3].constant) ), (yyvsp[-1].en) ) ); }
#line 7460 "Parser/parser.cc"
    break;

  case 43:
#line 705 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.en) = new ExpressionNode( new ConstructorExpr( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) ) );
		}
#line 7470 "Parser/parser.cc"
    break;

  case 44:
#line 711 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7476 "Parser/parser.cc"
    break;

  case 45:
#line 713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 7482 "Parser/parser.cc"
    break;

  case 46:
#line 715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 7488 "Parser/parser.cc"
    break;

  case 47:
#line 717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), new ExpressionNode( (yyvsp[-2].constant) ) ) ); }
#line 7494 "Parser/parser.cc"
    break;

  case 48:
#line 719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7500 "Parser/parser.cc"
    break;

  case 49:
#line 721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 7506 "Parser/parser.cc"
    break;

  case 50:
#line 723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-1].en), build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) ) ) ); }
#line 7512 "Parser/parser.cc"
    break;

  case 51:
#line 725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7518 "Parser/parser.cc"
    break;

  case 52:
#line 727 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[0].aggKey), (yyvsp[-2].en) ) ); }
#line 7524 "Parser/parser.cc"
    break;

  case 53:
#line 729 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7530 "Parser/parser.cc"
    break;

  case 54:
#line 731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 7536 "Parser/parser.cc"
    break;

  case 55:
#line 733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7542 "Parser/parser.cc"
    break;

  case 56:
#line 735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::IncrPost, (yyvsp[-1].en) ) ); }
#line 7548 "Parser/parser.cc"
    break;

  case 57:
#line 737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::DecrPost, (yyvsp[-1].en) ) ); }
#line 7554 "Parser/parser.cc"
    break;

  case 58:
#line 739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].in), true ) ) ); }
#line 7560 "Parser/parser.cc"
    break;

  case 59:
#line 741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].in), true ))->set_maybeConstructed( false ) ) ); }
#line 7566 "Parser/parser.cc"
    break;

  case 60:
#line 743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) );
		}
#line 7576 "Parser/parser.cc"
    break;

  case 61:
#line 752 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7582 "Parser/parser.cc"
    break;

  case 64:
#line 759 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7588 "Parser/parser.cc"
    break;

  case 65:
#line 764 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Default parameter for argument is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7594 "Parser/parser.cc"
    break;

  case 68:
#line 771 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7600 "Parser/parser.cc"
    break;

  case 70:
#line 777 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-1].tok) ) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7606 "Parser/parser.cc"
    break;

  case 71:
#line 779 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-3].tok) ) ), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7612 "Parser/parser.cc"
    break;

  case 72:
#line 781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7618 "Parser/parser.cc"
    break;

  case 73:
#line 783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7624 "Parser/parser.cc"
    break;

  case 74:
#line 785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7630 "Parser/parser.cc"
    break;

  case 75:
#line 787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7636 "Parser/parser.cc"
    break;

  case 76:
#line 792 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_constantInteger( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 7642 "Parser/parser.cc"
    break;

  case 77:
#line 794 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_field_name_FLOATINGconstant( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 7648 "Parser/parser.cc"
    break;

  case 78:
#line 796 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_varref( (yyvsp[-1].tok) ), (yyvsp[0].en) ) );
		}
#line 7656 "Parser/parser.cc"
    break;

  case 79:
#line 803 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7662 "Parser/parser.cc"
    break;

  case 80:
#line 805 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Expression * constant = build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) );
			(yyval.en) = (yyvsp[-1].en) != nullptr ? new ExpressionNode( build_fieldSel( (yyvsp[-1].en),  constant ) ) : new ExpressionNode( constant );
		}
#line 7671 "Parser/parser.cc"
    break;

  case 83:
#line 817 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 7677 "Parser/parser.cc"
    break;

  case 84:
#line 819 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en)->set_extension( true ); }
#line 7683 "Parser/parser.cc"
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
#line 7703 "Parser/parser.cc"
    break;

  case 86:
#line 840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_val( (yyvsp[-1].op), (yyvsp[0].en) ) ); }
#line 7709 "Parser/parser.cc"
    break;

  case 87:
#line 842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Incr, (yyvsp[0].en) ) ); }
#line 7715 "Parser/parser.cc"
    break;

  case 88:
#line 844 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Decr, (yyvsp[0].en) ) ); }
#line 7721 "Parser/parser.cc"
    break;

  case 89:
#line 846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7727 "Parser/parser.cc"
    break;

  case 90:
#line 848 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 7733 "Parser/parser.cc"
    break;

  case 91:
#line 850 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7739 "Parser/parser.cc"
    break;

  case 92:
#line 852 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 7745 "Parser/parser.cc"
    break;

  case 93:
#line 854 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_offsetOf( (yyvsp[-3].decl), build_varref( (yyvsp[-1].tok) ) ) ); }
#line 7751 "Parser/parser.cc"
    break;

  case 94:
#line 856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.en) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 7760 "Parser/parser.cc"
    break;

  case 95:
#line 863 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::PointTo; }
#line 7766 "Parser/parser.cc"
    break;

  case 96:
#line 864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AddressOf; }
#line 7772 "Parser/parser.cc"
    break;

  case 97:
#line 866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::And; }
#line 7778 "Parser/parser.cc"
    break;

  case 98:
#line 870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::UnPlus; }
#line 7784 "Parser/parser.cc"
    break;

  case 99:
#line 871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::UnMinus; }
#line 7790 "Parser/parser.cc"
    break;

  case 100:
#line 872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::Neg; }
#line 7796 "Parser/parser.cc"
    break;

  case 101:
#line 873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::BitNeg; }
#line 7802 "Parser/parser.cc"
    break;

  case 103:
#line 879 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cast( (yyvsp[-2].decl), (yyvsp[0].en) ) ); }
#line 7808 "Parser/parser.cc"
    break;

  case 104:
#line 881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 7814 "Parser/parser.cc"
    break;

  case 105:
#line 883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 7820 "Parser/parser.cc"
    break;

  case 106:
#line 885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( nullptr ) ) ); }
#line 7826 "Parser/parser.cc"
    break;

  case 107:
#line 887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 7832 "Parser/parser.cc"
    break;

  case 108:
#line 889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Return cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7838 "Parser/parser.cc"
    break;

  case 109:
#line 891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7844 "Parser/parser.cc"
    break;

  case 110:
#line 893 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7850 "Parser/parser.cc"
    break;

  case 118:
#line 913 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Exp, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7856 "Parser/parser.cc"
    break;

  case 120:
#line 919 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mul, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7862 "Parser/parser.cc"
    break;

  case 121:
#line 921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Div, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7868 "Parser/parser.cc"
    break;

  case 122:
#line 923 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mod, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7874 "Parser/parser.cc"
    break;

  case 124:
#line 929 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Plus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7880 "Parser/parser.cc"
    break;

  case 125:
#line 931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Minus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7886 "Parser/parser.cc"
    break;

  case 127:
#line 937 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7892 "Parser/parser.cc"
    break;

  case 128:
#line 939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::RShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7898 "Parser/parser.cc"
    break;

  case 130:
#line 945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7904 "Parser/parser.cc"
    break;

  case 131:
#line 947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7910 "Parser/parser.cc"
    break;

  case 132:
#line 949 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7916 "Parser/parser.cc"
    break;

  case 133:
#line 951 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7922 "Parser/parser.cc"
    break;

  case 135:
#line 957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Eq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7928 "Parser/parser.cc"
    break;

  case 136:
#line 959 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Neq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7934 "Parser/parser.cc"
    break;

  case 138:
#line 965 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitAnd, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7940 "Parser/parser.cc"
    break;

  case 140:
#line 971 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Xor, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7946 "Parser/parser.cc"
    break;

  case 142:
#line 977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitOr, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7952 "Parser/parser.cc"
    break;

  case 144:
#line 983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), true ) ); }
#line 7958 "Parser/parser.cc"
    break;

  case 146:
#line 989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), false ) ); }
#line 7964 "Parser/parser.cc"
    break;

  case 148:
#line 995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7970 "Parser/parser.cc"
    break;

  case 149:
#line 998 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-3].en), (yyvsp[-3].en), (yyvsp[0].en) ) ); }
#line 7976 "Parser/parser.cc"
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
#line 7988 "Parser/parser.cc"
    break;

  case 153:
#line 1017 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7994 "Parser/parser.cc"
    break;

  case 154:
#line 1022 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8000 "Parser/parser.cc"
    break;

  case 158:
#line 1032 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::Assign; }
#line 8006 "Parser/parser.cc"
    break;

  case 159:
#line 1033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AtAssn; }
#line 8012 "Parser/parser.cc"
    break;

  case 160:
#line 1037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ExpAssn; }
#line 8018 "Parser/parser.cc"
    break;

  case 161:
#line 1038 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MulAssn; }
#line 8024 "Parser/parser.cc"
    break;

  case 162:
#line 1039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::DivAssn; }
#line 8030 "Parser/parser.cc"
    break;

  case 163:
#line 1040 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ModAssn; }
#line 8036 "Parser/parser.cc"
    break;

  case 164:
#line 1041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::PlusAssn; }
#line 8042 "Parser/parser.cc"
    break;

  case 165:
#line 1042 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MinusAssn; }
#line 8048 "Parser/parser.cc"
    break;

  case 166:
#line 1043 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::LSAssn; }
#line 8054 "Parser/parser.cc"
    break;

  case 167:
#line 1044 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::RSAssn; }
#line 8060 "Parser/parser.cc"
    break;

  case 168:
#line 1045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AndAssn; }
#line 8066 "Parser/parser.cc"
    break;

  case 169:
#line 1046 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ERAssn; }
#line 8072 "Parser/parser.cc"
    break;

  case 170:
#line 1047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::OrAssn; }
#line 8078 "Parser/parser.cc"
    break;

  case 171:
#line 1058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)(new ExpressionNode( nullptr ) )->set_last( (yyvsp[-1].en) ) ) ); }
#line 8084 "Parser/parser.cc"
    break;

  case 172:
#line 1060 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-4].en)->set_last( (yyvsp[-1].en) ) ) )); }
#line 8090 "Parser/parser.cc"
    break;

  case 174:
#line 1066 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8096 "Parser/parser.cc"
    break;

  case 175:
#line 1068 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 8102 "Parser/parser.cc"
    break;

  case 176:
#line 1070 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8108 "Parser/parser.cc"
    break;

  case 178:
#line 1076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new CommaExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 8114 "Parser/parser.cc"
    break;

  case 179:
#line 1081 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8120 "Parser/parser.cc"
    break;

  case 191:
#line 1099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 8126 "Parser/parser.cc"
    break;

  case 193:
#line 1102 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_directive( (yyvsp[0].tok) ) ); }
#line 8132 "Parser/parser.cc"
    break;

  case 194:
#line 1108 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[0].sn)->add_label( (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 8138 "Parser/parser.cc"
    break;

  case 195:
#line 1110 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "Label \"", *(yyvsp[-3].tok).str, "\" must be associated with a statement, "
											   "where a declaration, case, or default is not a statement. "
											   "Move the label or terminate with a semi-colon." ) );
			(yyval.sn) = nullptr;
		}
#line 8149 "Parser/parser.cc"
    break;

  case 196:
#line 1120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (StatementNode *)0 ) ); }
#line 8155 "Parser/parser.cc"
    break;

  case 197:
#line 1125 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (yyvsp[-2].sn) ) ); }
#line 8161 "Parser/parser.cc"
    break;

  case 199:
#line 1131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 8167 "Parser/parser.cc"
    break;

  case 200:
#line 1136 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8173 "Parser/parser.cc"
    break;

  case 201:
#line 1138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8179 "Parser/parser.cc"
    break;

  case 202:
#line 1140 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8185 "Parser/parser.cc"
    break;

  case 203:
#line 1142 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8191 "Parser/parser.cc"
    break;

  case 206:
#line 1149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 8197 "Parser/parser.cc"
    break;

  case 207:
#line 1151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Declarations only allowed at the start of the switch body, i.e., after the '{'." ); (yyval.sn) = nullptr; }
#line 8203 "Parser/parser.cc"
    break;

  case 208:
#line 1156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_expr( (yyvsp[-1].en) ) ); }
#line 8209 "Parser/parser.cc"
    break;

  case 209:
#line 1158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_mutex( nullptr, new StatementNode( build_expr( (yyvsp[-1].en) ) ) ) ); }
#line 8215 "Parser/parser.cc"
    break;

  case 210:
#line 1165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn); }
#line 8221 "Parser/parser.cc"
    break;

  case 211:
#line 1167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( true, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8227 "Parser/parser.cc"
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
#line 8241 "Parser/parser.cc"
    break;

  case 213:
#line 1179 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Only declarations can appear before the list of case clauses." ); (yyval.sn) = nullptr; }
#line 8247 "Parser/parser.cc"
    break;

  case 214:
#line 1181 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( false, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8253 "Parser/parser.cc"
    break;

  case 215:
#line 1183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( false, (yyvsp[-7].en), (yyvsp[-2].sn) ) );
			(yyval.sn) = (yyvsp[-3].decl) ? new StatementNode( build_compound( (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 8262 "Parser/parser.cc"
    break;

  case 216:
#line 1188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Only declarations can appear before the list of case clauses." ); (yyval.sn) = nullptr; }
#line 8268 "Parser/parser.cc"
    break;

  case 217:
#line 1194 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-2].ifctl), maybe_build_compound( (yyvsp[0].sn) ), nullptr ) ); }
#line 8274 "Parser/parser.cc"
    break;

  case 218:
#line 1196 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-4].ifctl), maybe_build_compound( (yyvsp[-2].sn) ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8280 "Parser/parser.cc"
    break;

  case 219:
#line 1201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( nullptr, (yyvsp[0].en) ); }
#line 8286 "Parser/parser.cc"
    break;

  case 220:
#line 1203 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 8292 "Parser/parser.cc"
    break;

  case 221:
#line 1205 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 8298 "Parser/parser.cc"
    break;

  case 222:
#line 1207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[-1].decl), (yyvsp[0].en) ); }
#line 8304 "Parser/parser.cc"
    break;

  case 223:
#line 1214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = (yyvsp[0].en); }
#line 8310 "Parser/parser.cc"
    break;

  case 224:
#line 1216 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 8316 "Parser/parser.cc"
    break;

  case 226:
#line 1221 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.sn) = new StatementNode( build_case( (yyvsp[0].en) ) ); }
#line 8322 "Parser/parser.cc"
    break;

  case 227:
#line 1223 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)((yyvsp[-2].sn)->set_last( new StatementNode( build_case( (yyvsp[0].en) ) ) ) ); }
#line 8328 "Parser/parser.cc"
    break;

  case 228:
#line 1228 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing case list after case." ); (yyval.sn) = nullptr; }
#line 8334 "Parser/parser.cc"
    break;

  case 229:
#line 1229 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn); }
#line 8340 "Parser/parser.cc"
    break;

  case 230:
#line 1231 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing colon after case list." ); (yyval.sn) = nullptr; }
#line 8346 "Parser/parser.cc"
    break;

  case 231:
#line 1232 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.sn) = new StatementNode( build_default() ); }
#line 8352 "Parser/parser.cc"
    break;

  case 232:
#line 1235 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing colon after default." ); (yyval.sn) = nullptr; }
#line 8358 "Parser/parser.cc"
    break;

  case 234:
#line 1240 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)( (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) )); }
#line 8364 "Parser/parser.cc"
    break;

  case 235:
#line 1244 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( maybe_build_compound( (yyvsp[0].sn) ) ); }
#line 8370 "Parser/parser.cc"
    break;

  case 236:
#line 1249 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = nullptr; }
#line 8376 "Parser/parser.cc"
    break;

  case 238:
#line 1255 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ); }
#line 8382 "Parser/parser.cc"
    break;

  case 239:
#line 1257 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)( (yyvsp[-2].sn)->set_last( (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ) ) ); }
#line 8388 "Parser/parser.cc"
    break;

  case 240:
#line 1262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8394 "Parser/parser.cc"
    break;

  case 241:
#line 1264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_while( new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( (yyvsp[-2].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse, "" );
		}
#line 8403 "Parser/parser.cc"
    break;

  case 242:
#line 1269 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( (yyvsp[-2].ifctl), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8409 "Parser/parser.cc"
    break;

  case 243:
#line 1271 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( (yyvsp[-4].ifctl), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[0].sn) ) ); }
#line 8415 "Parser/parser.cc"
    break;

  case 244:
#line 1273 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( NEW_ONE, maybe_build_compound( (yyvsp[-4].sn) ) ) ); }
#line 8421 "Parser/parser.cc"
    break;

  case 245:
#line 1275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_do_while( NEW_ONE, maybe_build_compound( (yyvsp[-5].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse, "" );
		}
#line 8430 "Parser/parser.cc"
    break;

  case 246:
#line 1280 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( (yyvsp[-2].en), maybe_build_compound( (yyvsp[-5].sn) ) ) ); }
#line 8436 "Parser/parser.cc"
    break;

  case 247:
#line 1282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( (yyvsp[-3].en), maybe_build_compound( (yyvsp[-6].sn) ), (yyvsp[0].sn) ) ); }
#line 8442 "Parser/parser.cc"
    break;

  case 248:
#line 1284 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( new ForCtrl( (ExpressionNode * )nullptr, (ExpressionNode * )nullptr, (ExpressionNode * )nullptr ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8448 "Parser/parser.cc"
    break;

  case 249:
#line 1286 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_for( new ForCtrl( (ExpressionNode * )nullptr, (ExpressionNode * )nullptr, (ExpressionNode * )nullptr ), maybe_build_compound( (yyvsp[-2].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse, "" );
		}
#line 8457 "Parser/parser.cc"
    break;

  case 250:
#line 1291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( (yyvsp[-2].fctl), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8463 "Parser/parser.cc"
    break;

  case 251:
#line 1293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( (yyvsp[-4].fctl), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[0].sn) ) ); }
#line 8469 "Parser/parser.cc"
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
#line 8488 "Parser/parser.cc"
    break;

  case 254:
#line 1321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (ExpressionNode * )nullptr, (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8494 "Parser/parser.cc"
    break;

  case 255:
#line 1323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8500 "Parser/parser.cc"
    break;

  case 256:
#line 1325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-3].decl), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8506 "Parser/parser.cc"
    break;

  case 257:
#line 1328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), NEW_ZERO, OperKinds::LThan, (yyvsp[0].en)->clone(), NEW_ONE ); }
#line 8512 "Parser/parser.cc"
    break;

  case 258:
#line 1330 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].compop), NEW_ZERO, (yyvsp[0].en)->clone() ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 8518 "Parser/parser.cc"
    break;

  case 259:
#line 1333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].compop), (yyvsp[-2].en)->clone(), (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), (yyvsp[-2].en)->clone() ), NEW_ONE ); }
#line 8524 "Parser/parser.cc"
    break;

  case 260:
#line 1335 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[0].en)->clone(), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 8533 "Parser/parser.cc"
    break;

  case 261:
#line 1340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; } 
		}
#line 8542 "Parser/parser.cc"
    break;

  case 262:
#line 1345 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en)->clone(), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), (yyvsp[0].en) ); }
#line 8548 "Parser/parser.cc"
    break;

  case 263:
#line 1347 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-2].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].en)->clone(), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 8557 "Parser/parser.cc"
    break;

  case 264:
#line 1352 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; } 
		}
#line 8566 "Parser/parser.cc"
    break;

  case 265:
#line 1357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 8572 "Parser/parser.cc"
    break;

  case 266:
#line 1359 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 8578 "Parser/parser.cc"
    break;

  case 267:
#line 1361 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 8584 "Parser/parser.cc"
    break;

  case 268:
#line 1363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 8590 "Parser/parser.cc"
    break;

  case 269:
#line 1365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 8596 "Parser/parser.cc"
    break;

  case 270:
#line 1368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-2].en), NEW_ZERO, OperKinds::LThan, (yyvsp[0].en)->clone(), NEW_ONE ); }
#line 8602 "Parser/parser.cc"
    break;

  case 271:
#line 1370 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-3].en), UPDOWN( (yyvsp[-1].compop), NEW_ZERO, (yyvsp[0].en)->clone() ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 8608 "Parser/parser.cc"
    break;

  case 272:
#line 1373 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), UPDOWN( (yyvsp[-1].compop), (yyvsp[-2].en)->clone(), (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), (yyvsp[-2].en)->clone() ), NEW_ONE ); }
#line 8614 "Parser/parser.cc"
    break;

  case 273:
#line 1375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-4].en), (yyvsp[0].en)->clone(), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 8623 "Parser/parser.cc"
    break;

  case 274:
#line 1380 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::GThan || (yyvsp[-1].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 8633 "Parser/parser.cc"
    break;

  case 275:
#line 1386 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing low/high value for up/down-to range so index is uninitialized." ); (yyval.fctl) = nullptr; }
#line 8639 "Parser/parser.cc"
    break;

  case 276:
#line 1389 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en)->clone(), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), (yyvsp[0].en) ); }
#line 8645 "Parser/parser.cc"
    break;

  case 277:
#line 1391 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-6].en), (yyvsp[-2].en)->clone(), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 8654 "Parser/parser.cc"
    break;

  case 278:
#line 1396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 8664 "Parser/parser.cc"
    break;

  case 279:
#line 1402 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en)->clone(), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), nullptr ); }
#line 8670 "Parser/parser.cc"
    break;

  case 280:
#line 1404 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-6].en), (yyvsp[-2].en)->clone(), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 8679 "Parser/parser.cc"
    break;

  case 281:
#line 1409 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 8689 "Parser/parser.cc"
    break;

  case 282:
#line 1415 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing low/high value for up/down-to range so index is uninitialized." ); (yyval.fctl) = nullptr; }
#line 8695 "Parser/parser.cc"
    break;

  case 283:
#line 1418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-1].decl), NEW_ZERO, OperKinds::LThan, (yyvsp[0].en), NEW_ONE ); }
#line 8701 "Parser/parser.cc"
    break;

  case 284:
#line 1420 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].decl), UPDOWN( (yyvsp[-1].compop), NEW_ZERO, (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 8707 "Parser/parser.cc"
    break;

  case 285:
#line 1423 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-3].decl), UPDOWN( (yyvsp[-1].compop), (yyvsp[-2].en)->clone(), (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), (yyvsp[-2].en)->clone() ), NEW_ONE ); }
#line 8713 "Parser/parser.cc"
    break;

  case 286:
#line 1425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-3].decl), (yyvsp[0].en), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 8722 "Parser/parser.cc"
    break;

  case 287:
#line 1430 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::GThan || (yyvsp[-1].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-3].decl), (yyvsp[-2].en), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 8732 "Parser/parser.cc"
    break;

  case 288:
#line 1437 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), (yyvsp[0].en) ); }
#line 8738 "Parser/parser.cc"
    break;

  case 289:
#line 1439 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-2].en), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 8747 "Parser/parser.cc"
    break;

  case 290:
#line 1444 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-4].en), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 8757 "Parser/parser.cc"
    break;

  case 291:
#line 1450 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), nullptr ); }
#line 8763 "Parser/parser.cc"
    break;

  case 292:
#line 1452 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-2].en), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 8772 "Parser/parser.cc"
    break;

  case 293:
#line 1457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-4].en), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 8782 "Parser/parser.cc"
    break;

  case 294:
#line 1463 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing low/high value for up/down-to range so index is uninitialized." ); (yyval.fctl) = nullptr; }
#line 8788 "Parser/parser.cc"
    break;

  case 295:
#line 1466 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.fctl) = nullptr;
			//$$ = forCtrl( new ExpressionNode( build_varref( $3 ) ), $1, nullptr, OperKinds::Range, nullptr, nullptr );
		}
#line 8797 "Parser/parser.cc"
    break;

  case 296:
#line 1471 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LEThan || (yyvsp[-1].compop) == OperKinds::GEThan ) { SemanticError( yylloc, "All enumation ranges are equal (all values). Remove \"=~\"." ); (yyval.fctl) = nullptr; }
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.fctl) = nullptr;
		}
#line 8806 "Parser/parser.cc"
    break;

  case 297:
#line 1479 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GThan; }
#line 8812 "Parser/parser.cc"
    break;

  case 298:
#line 1481 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 8818 "Parser/parser.cc"
    break;

  case 299:
#line 1483 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GEThan; }
#line 8824 "Parser/parser.cc"
    break;

  case 300:
#line 1488 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LThan; }
#line 8830 "Parser/parser.cc"
    break;

  case 301:
#line 1490 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GThan; }
#line 8836 "Parser/parser.cc"
    break;

  case 303:
#line 1496 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 8842 "Parser/parser.cc"
    break;

  case 304:
#line 1498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GEThan; }
#line 8848 "Parser/parser.cc"
    break;

  case 305:
#line 1503 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Goto ) ); }
#line 8854 "Parser/parser.cc"
    break;

  case 306:
#line 1507 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_computedgoto( (yyvsp[-1].en) ) ); }
#line 8860 "Parser/parser.cc"
    break;

  case 307:
#line 1510 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThrough ) ); }
#line 8866 "Parser/parser.cc"
    break;

  case 308:
#line 1512 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::FallThrough ) ); }
#line 8872 "Parser/parser.cc"
    break;

  case 309:
#line 1514 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThroughDefault ) ); }
#line 8878 "Parser/parser.cc"
    break;

  case 310:
#line 1517 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Continue ) ); }
#line 8884 "Parser/parser.cc"
    break;

  case 311:
#line 1521 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Continue ) ); }
#line 8890 "Parser/parser.cc"
    break;

  case 312:
#line 1524 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Break ) ); }
#line 8896 "Parser/parser.cc"
    break;

  case 313:
#line 1528 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Break ) ); }
#line 8902 "Parser/parser.cc"
    break;

  case 314:
#line 1530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_return( (yyvsp[-1].en) ) ); }
#line 8908 "Parser/parser.cc"
    break;

  case 315:
#line 1532 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 8914 "Parser/parser.cc"
    break;

  case 316:
#line 1534 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr ) ); }
#line 8920 "Parser/parser.cc"
    break;

  case 317:
#line 1536 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn) ) ); }
#line 8926 "Parser/parser.cc"
    break;

  case 318:
#line 1538 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Coroutine ) ); }
#line 8932 "Parser/parser.cc"
    break;

  case 319:
#line 1540 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Coroutine ) ); }
#line 8938 "Parser/parser.cc"
    break;

  case 320:
#line 1542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Generator ) ); }
#line 8944 "Parser/parser.cc"
    break;

  case 321:
#line 1544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Generator ) ); }
#line 8950 "Parser/parser.cc"
    break;

  case 322:
#line 1546 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_throw( (yyvsp[-1].en) ) ); }
#line 8956 "Parser/parser.cc"
    break;

  case 323:
#line 1548 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume( (yyvsp[-1].en) ) ); }
#line 8962 "Parser/parser.cc"
    break;

  case 324:
#line 1550 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume_at( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 8968 "Parser/parser.cc"
    break;

  case 327:
#line 1560 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_with( (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8974 "Parser/parser.cc"
    break;

  case 328:
#line 1566 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_mutex( (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8980 "Parser/parser.cc"
    break;

  case 329:
#line 1570 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 8986 "Parser/parser.cc"
    break;

  case 330:
#line 1575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8992 "Parser/parser.cc"
    break;

  case 332:
#line 1581 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 8998 "Parser/parser.cc"
    break;

  case 333:
#line 1585 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) )); }
#line 9004 "Parser/parser.cc"
    break;

  case 335:
#line 1592 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.en) = nullptr; }
#line 9010 "Parser/parser.cc"
    break;

  case 336:
#line 1596 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 9016 "Parser/parser.cc"
    break;

  case 337:
#line 1601 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 9022 "Parser/parser.cc"
    break;

  case 338:
#line 1603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-3].en), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[-4].en), (yyvsp[0].wfs) ); }
#line 9028 "Parser/parser.cc"
    break;

  case 339:
#line 1605 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 9034 "Parser/parser.cc"
    break;

  case 340:
#line 1607 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( nullptr, maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 9040 "Parser/parser.cc"
    break;

  case 341:
#line 1610 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 9046 "Parser/parser.cc"
    break;

  case 342:
#line 1612 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-5].en), maybe_build_compound( (yyvsp[-4].sn) ), (yyvsp[-6].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 9052 "Parser/parser.cc"
    break;

  case 343:
#line 1617 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-1].en), (yyvsp[0].sn), (yyvsp[-2].en) ) ); }
#line 9058 "Parser/parser.cc"
    break;

  case 344:
#line 1619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-3].en), (yyvsp[-2].sn), (yyvsp[-4].en), (yyvsp[0].wfs) ) ); }
#line 9064 "Parser/parser.cc"
    break;

  case 345:
#line 1624 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), (yyvsp[0].sn), 0 ) ); }
#line 9070 "Parser/parser.cc"
    break;

  case 346:
#line 1626 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), 0, (yyvsp[0].sn) ) ); }
#line 9076 "Parser/parser.cc"
    break;

  case 347:
#line 1628 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-2].sn), (yyvsp[-1].sn), (yyvsp[0].sn) ) ); }
#line 9082 "Parser/parser.cc"
    break;

  case 348:
#line 1633 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 9088 "Parser/parser.cc"
    break;

  case 349:
#line 1635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)(yyvsp[-8].sn)->set_last( new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ) ); }
#line 9094 "Parser/parser.cc"
    break;

  case 350:
#line 1640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9100 "Parser/parser.cc"
    break;

  case 351:
#line 1641 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[0].en); }
#line 9106 "Parser/parser.cc"
    break;

  case 352:
#line 1645 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 9112 "Parser/parser.cc"
    break;

  case 353:
#line 1646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 9118 "Parser/parser.cc"
    break;

  case 354:
#line 1647 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.catch_kind) = CatchStmt::Resume; }
#line 9124 "Parser/parser.cc"
    break;

  case 355:
#line 1648 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Resume; }
#line 9130 "Parser/parser.cc"
    break;

  case 356:
#line 1652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = new StatementNode( build_finally( (yyvsp[0].sn) ) ); }
#line 9136 "Parser/parser.cc"
    break;

  case 358:
#line 1659 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 9142 "Parser/parser.cc"
    break;

  case 359:
#line 1661 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 9148 "Parser/parser.cc"
    break;

  case 360:
#line 1663 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 9154 "Parser/parser.cc"
    break;

  case 365:
#line 1678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-4].flag), (yyvsp[-2].constant), 0 ) ); }
#line 9160 "Parser/parser.cc"
    break;

  case 366:
#line 1680 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-6].flag), (yyvsp[-4].constant), (yyvsp[-2].en) ) ); }
#line 9166 "Parser/parser.cc"
    break;

  case 367:
#line 1682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-8].flag), (yyvsp[-6].constant), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 9172 "Parser/parser.cc"
    break;

  case 368:
#line 1684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-10].flag), (yyvsp[-8].constant), (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 9178 "Parser/parser.cc"
    break;

  case 369:
#line 1686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-12].flag), (yyvsp[-9].constant), 0, (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].label) ) ); }
#line 9184 "Parser/parser.cc"
    break;

  case 370:
#line 1691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = false; }
#line 9190 "Parser/parser.cc"
    break;

  case 371:
#line 1693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = true; }
#line 9196 "Parser/parser.cc"
    break;

  case 372:
#line 1698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9202 "Parser/parser.cc"
    break;

  case 375:
#line 1705 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 9208 "Parser/parser.cc"
    break;

  case 376:
#line 1710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( nullptr, (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 9214 "Parser/parser.cc"
    break;

  case 377:
#line 1712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( (yyvsp[-5].tok), (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 9220 "Parser/parser.cc"
    break;

  case 378:
#line 1717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9226 "Parser/parser.cc"
    break;

  case 379:
#line 1719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 9232 "Parser/parser.cc"
    break;

  case 380:
#line 1721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( (yyvsp[0].constant) ) )); }
#line 9238 "Parser/parser.cc"
    break;

  case 381:
#line 1726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = new LabelNode(); (yyval.label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 9247 "Parser/parser.cc"
    break;

  case 382:
#line 1731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = (yyvsp[-2].label); (yyvsp[-2].label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 9256 "Parser/parser.cc"
    break;

  case 383:
#line 1741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9262 "Parser/parser.cc"
    break;

  case 386:
#line 1748 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 9268 "Parser/parser.cc"
    break;

  case 387:
#line 1753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9274 "Parser/parser.cc"
    break;

  case 389:
#line 1759 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 9280 "Parser/parser.cc"
    break;

  case 390:
#line 1761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-2].decl) ); }
#line 9286 "Parser/parser.cc"
    break;

  case 397:
#line 1781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "C_DECLARATION1 %p %s\n", $$, $$->name ? $$->name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 9297 "Parser/parser.cc"
    break;

  case 400:
#line 1793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-4].en), (yyvsp[-2].constant) ); }
#line 9303 "Parser/parser.cc"
    break;

  case 401:
#line 1795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-2].en), build_constantStr( *new string( "\"\"" ) ) ); }
#line 9309 "Parser/parser.cc"
    break;

  case 405:
#line 1813 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9315 "Parser/parser.cc"
    break;

  case 407:
#line 1819 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].in) ); }
#line 9321 "Parser/parser.cc"
    break;

  case 408:
#line 1823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 9327 "Parser/parser.cc"
    break;

  case 409:
#line 1825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].in) ) ); }
#line 9333 "Parser/parser.cc"
    break;

  case 410:
#line 1832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 9339 "Parser/parser.cc"
    break;

  case 411:
#line 1834 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 9345 "Parser/parser.cc"
    break;

  case 412:
#line 1836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 9351 "Parser/parser.cc"
    break;

  case 414:
#line 1842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9357 "Parser/parser.cc"
    break;

  case 415:
#line 1844 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9363 "Parser/parser.cc"
    break;

  case 416:
#line 1846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 9369 "Parser/parser.cc"
    break;

  case 417:
#line 1848 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeClone( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->appendList( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 9380 "Parser/parser.cc"
    break;

  case 418:
#line 1881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), 0 )->addQualifiers( (yyvsp[0].decl) ); }
#line 9386 "Parser/parser.cc"
    break;

  case 419:
#line 1883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), 0 )->addQualifiers( (yyvsp[0].decl) ); }
#line 9392 "Parser/parser.cc"
    break;

  case 420:
#line 1888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 9398 "Parser/parser.cc"
    break;

  case 421:
#line 1891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-6].decl)->appendList( (yyvsp[-2].decl) ) ); }
#line 9404 "Parser/parser.cc"
    break;

  case 422:
#line 1896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 9413 "Parser/parser.cc"
    break;

  case 423:
#line 1901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 9422 "Parser/parser.cc"
    break;

  case 424:
#line 1906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "3" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 9431 "Parser/parser.cc"
    break;

  case 425:
#line 1917 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "4" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef();
		}
#line 9440 "Parser/parser.cc"
    break;

  case 426:
#line 1922 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "5" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 9449 "Parser/parser.cc"
    break;

  case 427:
#line 1927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "6" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[-3].decl) )->addTypedef();
		}
#line 9458 "Parser/parser.cc"
    break;

  case 428:
#line 1932 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "7" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-2].decl) )->addTypedef();
		}
#line 9467 "Parser/parser.cc"
    break;

  case 429:
#line 1937 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "8" );
			(yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-3].decl) )->addTypedef()->addType( (yyvsp[-3].decl) );
		}
#line 9476 "Parser/parser.cc"
    break;

  case 430:
#line 1946 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 9484 "Parser/parser.cc"
    break;

  case 431:
#line 1950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 9492 "Parser/parser.cc"
    break;

  case 432:
#line 1957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 9498 "Parser/parser.cc"
    break;

  case 436:
#line 1967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 9504 "Parser/parser.cc"
    break;

  case 437:
#line 1969 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ) ); }
#line 9510 "Parser/parser.cc"
    break;

  case 445:
#line 1992 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_type_specifier2 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 9521 "Parser/parser.cc"
    break;

  case 450:
#line 2014 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9527 "Parser/parser.cc"
    break;

  case 453:
#line 2026 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9533 "Parser/parser.cc"
    break;

  case 456:
#line 2036 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Const ); }
#line 9539 "Parser/parser.cc"
    break;

  case 457:
#line 2038 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Restrict ); }
#line 9545 "Parser/parser.cc"
    break;

  case 458:
#line 2040 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Volatile ); }
#line 9551 "Parser/parser.cc"
    break;

  case 459:
#line 2042 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Atomic ); }
#line 9557 "Parser/parser.cc"
    break;

  case 461:
#line 2048 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newForall( (yyvsp[-1].decl) ); }
#line 9563 "Parser/parser.cc"
    break;

  case 463:
#line 2054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9569 "Parser/parser.cc"
    break;

  case 464:
#line 2056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9575 "Parser/parser.cc"
    break;

  case 466:
#line 2067 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9581 "Parser/parser.cc"
    break;

  case 467:
#line 2072 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Extern ); }
#line 9587 "Parser/parser.cc"
    break;

  case 468:
#line 2074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Static ); }
#line 9593 "Parser/parser.cc"
    break;

  case 469:
#line 2076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Auto ); }
#line 9599 "Parser/parser.cc"
    break;

  case 470:
#line 2078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Register ); }
#line 9605 "Parser/parser.cc"
    break;

  case 471:
#line 2080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Threadlocal ); }
#line 9611 "Parser/parser.cc"
    break;

  case 472:
#line 2083 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Inline ); }
#line 9617 "Parser/parser.cc"
    break;

  case 473:
#line 2085 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Fortran ); }
#line 9623 "Parser/parser.cc"
    break;

  case 474:
#line 2087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Noreturn ); }
#line 9629 "Parser/parser.cc"
    break;

  case 475:
#line 2092 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 9635 "Parser/parser.cc"
    break;

  case 476:
#line 2094 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Bool ); }
#line 9641 "Parser/parser.cc"
    break;

  case 477:
#line 2096 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Char ); }
#line 9647 "Parser/parser.cc"
    break;

  case 478:
#line 2098 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int ); }
#line 9653 "Parser/parser.cc"
    break;

  case 479:
#line 2100 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 ); }
#line 9659 "Parser/parser.cc"
    break;

  case 480:
#line 2102 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 )->addType( DeclarationNode::newSignedNess( DeclarationNode::Unsigned ) ); }
#line 9665 "Parser/parser.cc"
    break;

  case 481:
#line 2104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Float ); }
#line 9671 "Parser/parser.cc"
    break;

  case 482:
#line 2106 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Double ); }
#line 9677 "Parser/parser.cc"
    break;

  case 483:
#line 2108 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat80 ); }
#line 9683 "Parser/parser.cc"
    break;

  case 484:
#line 2110 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat128 ); }
#line 9689 "Parser/parser.cc"
    break;

  case 485:
#line 2112 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat16 ); }
#line 9695 "Parser/parser.cc"
    break;

  case 486:
#line 2114 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32 ); }
#line 9701 "Parser/parser.cc"
    break;

  case 487:
#line 2116 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32x ); }
#line 9707 "Parser/parser.cc"
    break;

  case 488:
#line 2118 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64 ); }
#line 9713 "Parser/parser.cc"
    break;

  case 489:
#line 2120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64x ); }
#line 9719 "Parser/parser.cc"
    break;

  case 490:
#line 2122 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat128 ); }
#line 9725 "Parser/parser.cc"
    break;

  case 491:
#line 2124 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9731 "Parser/parser.cc"
    break;

  case 492:
#line 2126 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9737 "Parser/parser.cc"
    break;

  case 493:
#line 2128 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9743 "Parser/parser.cc"
    break;

  case 494:
#line 2130 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Complex ); }
#line 9749 "Parser/parser.cc"
    break;

  case 495:
#line 2132 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Imaginary ); }
#line 9755 "Parser/parser.cc"
    break;

  case 496:
#line 2134 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Signed ); }
#line 9761 "Parser/parser.cc"
    break;

  case 497:
#line 2136 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Unsigned ); }
#line 9767 "Parser/parser.cc"
    break;

  case 498:
#line 2138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Short ); }
#line 9773 "Parser/parser.cc"
    break;

  case 499:
#line 2140 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Long ); }
#line 9779 "Parser/parser.cc"
    break;

  case 500:
#line 2142 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Valist ); }
#line 9785 "Parser/parser.cc"
    break;

  case 501:
#line 2144 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::AutoType ); }
#line 9791 "Parser/parser.cc"
    break;

  case 503:
#line 2150 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9797 "Parser/parser.cc"
    break;

  case 505:
#line 2156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVtableType( (yyvsp[-2].decl) ); }
#line 9803 "Parser/parser.cc"
    break;

  case 506:
#line 2162 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9809 "Parser/parser.cc"
    break;

  case 507:
#line 2164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9815 "Parser/parser.cc"
    break;

  case 509:
#line 2171 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9821 "Parser/parser.cc"
    break;

  case 510:
#line 2173 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9827 "Parser/parser.cc"
    break;

  case 511:
#line 2175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9833 "Parser/parser.cc"
    break;

  case 512:
#line 2177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 9839 "Parser/parser.cc"
    break;

  case 514:
#line 2184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9845 "Parser/parser.cc"
    break;

  case 516:
#line 2190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9851 "Parser/parser.cc"
    break;

  case 517:
#line 2192 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9857 "Parser/parser.cc"
    break;

  case 518:
#line 2194 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 9863 "Parser/parser.cc"
    break;

  case 519:
#line 2199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 9869 "Parser/parser.cc"
    break;

  case 520:
#line 2201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en) ); }
#line 9875 "Parser/parser.cc"
    break;

  case 521:
#line 2203 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 9881 "Parser/parser.cc"
    break;

  case 522:
#line 2205 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en), true ); }
#line 9887 "Parser/parser.cc"
    break;

  case 523:
#line 2207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Zero ); }
#line 9893 "Parser/parser.cc"
    break;

  case 524:
#line 2209 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::One ); }
#line 9899 "Parser/parser.cc"
    break;

  case 525:
#line 2214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_declaration_specifier %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 9910 "Parser/parser.cc"
    break;

  case 526:
#line 2221 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9916 "Parser/parser.cc"
    break;

  case 527:
#line 2223 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9922 "Parser/parser.cc"
    break;

  case 528:
#line 2225 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9928 "Parser/parser.cc"
    break;

  case 529:
#line 2230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_type_specifier %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 9939 "Parser/parser.cc"
    break;

  case 530:
#line 2237 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 9945 "Parser/parser.cc"
    break;

  case 531:
#line 2239 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 9951 "Parser/parser.cc"
    break;

  case 532:
#line 2241 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 9960 "Parser/parser.cc"
    break;

  case 534:
#line 2250 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9966 "Parser/parser.cc"
    break;

  case 535:
#line 2252 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9972 "Parser/parser.cc"
    break;

  case 536:
#line 2254 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9978 "Parser/parser.cc"
    break;

  case 538:
#line 2260 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9984 "Parser/parser.cc"
    break;

  case 539:
#line 2262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9990 "Parser/parser.cc"
    break;

  case 541:
#line 2268 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9996 "Parser/parser.cc"
    break;

  case 542:
#line 2270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10002 "Parser/parser.cc"
    break;

  case 543:
#line 2272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10008 "Parser/parser.cc"
    break;

  case 545:
#line 2278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10014 "Parser/parser.cc"
    break;

  case 546:
#line 2280 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10020 "Parser/parser.cc"
    break;

  case 547:
#line 2285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypedef( (yyvsp[0].tok) ); }
#line 10026 "Parser/parser.cc"
    break;

  case 548:
#line 2287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 10032 "Parser/parser.cc"
    break;

  case 549:
#line 2289 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 10038 "Parser/parser.cc"
    break;

  case 551:
#line 2292 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), (yyvsp[0].decl) ); }
#line 10044 "Parser/parser.cc"
    break;

  case 552:
#line 2294 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), (yyvsp[0].decl) ); }
#line 10050 "Parser/parser.cc"
    break;

  case 553:
#line 2299 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[0].tok), nullptr ); }
#line 10056 "Parser/parser.cc"
    break;

  case 554:
#line 2301 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-2].tok), nullptr ); }
#line 10062 "Parser/parser.cc"
    break;

  case 555:
#line 2303 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 10068 "Parser/parser.cc"
    break;

  case 556:
#line 2308 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "elaborated_type %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10079 "Parser/parser.cc"
    break;

  case 560:
#line 2324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 10085 "Parser/parser.cc"
    break;

  case 561:
#line 2326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 10091 "Parser/parser.cc"
    break;

  case 562:
#line 2328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 10100 "Parser/parser.cc"
    break;

  case 563:
#line 2333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 10117 "Parser/parser.cc"
    break;

  case 564:
#line 2346 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 10126 "Parser/parser.cc"
    break;

  case 565:
#line 2351 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "AGG3\n" );
			DeclarationNode::newFromTypedef( (yyvsp[-5].tok) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 10136 "Parser/parser.cc"
    break;

  case 566:
#line 2357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 10145 "Parser/parser.cc"
    break;

  case 567:
#line 2362 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "AGG4\n" );
			DeclarationNode::newFromTypeGen( (yyvsp[-5].tok), nullptr );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 10155 "Parser/parser.cc"
    break;

  case 569:
#line 2372 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 10161 "Parser/parser.cc"
    break;

  case 570:
#line 2374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 10167 "Parser/parser.cc"
    break;

  case 571:
#line 2379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 10177 "Parser/parser.cc"
    break;

  case 572:
#line 2385 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 10192 "Parser/parser.cc"
    break;

  case 575:
#line 2404 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Struct; }
#line 10198 "Parser/parser.cc"
    break;

  case 576:
#line 2406 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Union; }
#line 10204 "Parser/parser.cc"
    break;

  case 577:
#line 2408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Exception; }
#line 10210 "Parser/parser.cc"
    break;

  case 578:
#line 2414 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 10216 "Parser/parser.cc"
    break;

  case 579:
#line 2416 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 10222 "Parser/parser.cc"
    break;

  case 580:
#line 2418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Generator; }
#line 10228 "Parser/parser.cc"
    break;

  case 581:
#line 2420 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor generator is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 10234 "Parser/parser.cc"
    break;

  case 582:
#line 2422 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Coroutine; }
#line 10240 "Parser/parser.cc"
    break;

  case 583:
#line 2424 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor coroutine is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 10246 "Parser/parser.cc"
    break;

  case 584:
#line 2426 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Thread; }
#line 10252 "Parser/parser.cc"
    break;

  case 585:
#line 2428 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor thread is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 10258 "Parser/parser.cc"
    break;

  case 586:
#line 2433 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10264 "Parser/parser.cc"
    break;

  case 587:
#line 2435 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 10270 "Parser/parser.cc"
    break;

  case 588:
#line 2440 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "type_specifier1 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			(yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) );
			// printf( "type_specifier2 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10283 "Parser/parser.cc"
    break;

  case 589:
#line 2449 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 10289 "Parser/parser.cc"
    break;

  case 590:
#line 2451 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-1].decl) ) {								// field declarator ?
				(yyvsp[-1].decl) = DeclarationNode::newName( nullptr );
			} // if
			(yyvsp[-1].decl)->inLine = true;
			(yyval.decl) = distAttr( (yyvsp[-2].decl), (yyvsp[-1].decl) );					// mark all fields in list
			distInl( (yyvsp[-1].decl) );
		}
#line 10302 "Parser/parser.cc"
    break;

  case 591:
#line 2460 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10308 "Parser/parser.cc"
    break;

  case 594:
#line 2464 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 10314 "Parser/parser.cc"
    break;

  case 595:
#line 2466 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10320 "Parser/parser.cc"
    break;

  case 598:
#line 2473 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10326 "Parser/parser.cc"
    break;

  case 600:
#line 2476 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 10332 "Parser/parser.cc"
    break;

  case 601:
#line 2481 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].en) ); }
#line 10338 "Parser/parser.cc"
    break;

  case 602:
#line 2484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 10344 "Parser/parser.cc"
    break;

  case 603:
#line 2487 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 10350 "Parser/parser.cc"
    break;

  case 604:
#line 2492 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10356 "Parser/parser.cc"
    break;

  case 606:
#line 2495 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 10362 "Parser/parser.cc"
    break;

  case 608:
#line 2506 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 10368 "Parser/parser.cc"
    break;

  case 609:
#line 2508 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 10374 "Parser/parser.cc"
    break;

  case 611:
#line 2515 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 10380 "Parser/parser.cc"
    break;

  case 612:
#line 2520 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 10386 "Parser/parser.cc"
    break;

  case 614:
#line 2526 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 10392 "Parser/parser.cc"
    break;

  case 615:
#line 2531 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10398 "Parser/parser.cc"
    break;

  case 616:
#line 2533 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); }
#line 10404 "Parser/parser.cc"
    break;

  case 617:
#line 2535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].tok), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) ); }
#line 10410 "Parser/parser.cc"
    break;

  case 618:
#line 2538 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-4].decl)->name, (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 10416 "Parser/parser.cc"
    break;

  case 619:
#line 2540 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Unvalued enumerated type is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10422 "Parser/parser.cc"
    break;

  case 620:
#line 2542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-6].decl)->storageClasses.val != 0 || (yyvsp[-6].decl)->type->qualifiers.val != 0 )
			{ SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }

			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 10433 "Parser/parser.cc"
    break;

  case 621:
#line 2549 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].decl)->storageClasses.val != 0 || (yyvsp[-4].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			typedefTable.makeTypedef( *(yyvsp[-1].tok) );
		}
#line 10442 "Parser/parser.cc"
    break;

  case 622:
#line 2554 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, (yyvsp[-9].decl) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 10450 "Parser/parser.cc"
    break;

  case 623:
#line 2558 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-8].decl)->storageClasses.val != 0 || (yyvsp[-8].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			typedefTable.makeTypedef( *(yyvsp[-5].decl)->name );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].decl)->name, (yyvsp[-2].decl), true, (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 10460 "Parser/parser.cc"
    break;

  case 625:
#line 2568 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); (yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), 0, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 10466 "Parser/parser.cc"
    break;

  case 626:
#line 2570 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].decl)->type->symbolic.name );	(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].decl)->type->symbolic.name, 0, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 10472 "Parser/parser.cc"
    break;

  case 627:
#line 2575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].in) ); }
#line 10478 "Parser/parser.cc"
    break;

  case 628:
#line 2577 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( new string("inline"), nullptr ); }
#line 10484 "Parser/parser.cc"
    break;

  case 629:
#line 2579 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].in) ) ); }
#line 10490 "Parser/parser.cc"
    break;

  case 630:
#line 2581 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( DeclarationNode::newEnumValueGeneric( new string("inline"), nullptr ) ); }
#line 10496 "Parser/parser.cc"
    break;

  case 631:
#line 2586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 10502 "Parser/parser.cc"
    break;

  case 632:
#line 2587 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = new InitializerNode( (yyvsp[0].en) ); }
#line 10508 "Parser/parser.cc"
    break;

  case 633:
#line 2588 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                     { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 10514 "Parser/parser.cc"
    break;

  case 634:
#line 2595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 10520 "Parser/parser.cc"
    break;

  case 635:
#line 2597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10526 "Parser/parser.cc"
    break;

  case 638:
#line 2601 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10532 "Parser/parser.cc"
    break;

  case 639:
#line 2603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 10538 "Parser/parser.cc"
    break;

  case 640:
#line 2605 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 10544 "Parser/parser.cc"
    break;

  case 642:
#line 2613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10550 "Parser/parser.cc"
    break;

  case 643:
#line 2615 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10556 "Parser/parser.cc"
    break;

  case 644:
#line 2617 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-8].decl)->appendList( (yyvsp[-4].decl) )->appendList( (yyvsp[0].decl) ); }
#line 10562 "Parser/parser.cc"
    break;

  case 646:
#line 2623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10568 "Parser/parser.cc"
    break;

  case 647:
#line 2628 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10574 "Parser/parser.cc"
    break;

  case 648:
#line 2630 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10580 "Parser/parser.cc"
    break;

  case 650:
#line 2633 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 10586 "Parser/parser.cc"
    break;

  case 653:
#line 2640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10592 "Parser/parser.cc"
    break;

  case 654:
#line 2642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10598 "Parser/parser.cc"
    break;

  case 656:
#line 2651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 10604 "Parser/parser.cc"
    break;

  case 657:
#line 2654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 10610 "Parser/parser.cc"
    break;

  case 658:
#line 2656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 10616 "Parser/parser.cc"
    break;

  case 663:
#line 2666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10622 "Parser/parser.cc"
    break;

  case 665:
#line 2673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 10628 "Parser/parser.cc"
    break;

  case 666:
#line 2675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 10634 "Parser/parser.cc"
    break;

  case 667:
#line 2680 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 10640 "Parser/parser.cc"
    break;

  case 668:
#line 2682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 10646 "Parser/parser.cc"
    break;

  case 669:
#line 2691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 10652 "Parser/parser.cc"
    break;

  case 670:
#line 2693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 10658 "Parser/parser.cc"
    break;

  case 676:
#line 2706 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10664 "Parser/parser.cc"
    break;

  case 679:
#line 2716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 10670 "Parser/parser.cc"
    break;

  case 680:
#line 2717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (yyvsp[-1].op) == OperKinds::Assign ? (yyvsp[0].in) : (yyvsp[0].in)->set_maybeConstructed( false ); }
#line 10676 "Parser/parser.cc"
    break;

  case 681:
#line 2718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.in) = new InitializerNode( true ); }
#line 10682 "Parser/parser.cc"
    break;

  case 682:
#line 2719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 10688 "Parser/parser.cc"
    break;

  case 683:
#line 2723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = new InitializerNode( (yyvsp[0].en) ); }
#line 10694 "Parser/parser.cc"
    break;

  case 684:
#line 2724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 10700 "Parser/parser.cc"
    break;

  case 685:
#line 2729 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 10706 "Parser/parser.cc"
    break;

  case 687:
#line 2731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ); }
#line 10712 "Parser/parser.cc"
    break;

  case 688:
#line 2732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (InitializerNode *)( (yyvsp[-2].in)->set_last( (yyvsp[0].in) ) ); }
#line 10718 "Parser/parser.cc"
    break;

  case 689:
#line 2733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.in) = (InitializerNode *)((yyvsp[-3].in)->set_last( (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ) )); }
#line 10724 "Parser/parser.cc"
    break;

  case 691:
#line 2749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[-1].tok) ) ); }
#line 10730 "Parser/parser.cc"
    break;

  case 693:
#line 2755 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-1].en)->set_last( (yyvsp[0].en) )); }
#line 10736 "Parser/parser.cc"
    break;

  case 694:
#line 2761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 10742 "Parser/parser.cc"
    break;

  case 695:
#line 2764 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 10748 "Parser/parser.cc"
    break;

  case 696:
#line 2766 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 10754 "Parser/parser.cc"
    break;

  case 697:
#line 2768 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-4].en) ), maybeMoveBuild<Expression>( (yyvsp[-2].en) ) ) ); }
#line 10760 "Parser/parser.cc"
    break;

  case 698:
#line 2770 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 10766 "Parser/parser.cc"
    break;

  case 700:
#line 2794 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl) ); }
#line 10772 "Parser/parser.cc"
    break;

  case 701:
#line 2799 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10778 "Parser/parser.cc"
    break;

  case 702:
#line 2801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 10784 "Parser/parser.cc"
    break;

  case 703:
#line 2806 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "9" );
			if ( (yyvsp[-1].tclass) == TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 10795 "Parser/parser.cc"
    break;

  case 704:
#line 2813 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 10801 "Parser/parser.cc"
    break;

  case 705:
#line 2815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "9" ); }
#line 10807 "Parser/parser.cc"
    break;

  case 706:
#line 2817 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 10813 "Parser/parser.cc"
    break;

  case 707:
#line 2819 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "9" );
			(yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 10822 "Parser/parser.cc"
    break;

  case 708:
#line 2825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 10828 "Parser/parser.cc"
    break;

  case 709:
#line 2830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 10834 "Parser/parser.cc"
    break;

  case 710:
#line 2832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 10840 "Parser/parser.cc"
    break;

  case 711:
#line 2834 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::DStype; }
#line 10846 "Parser/parser.cc"
    break;

  case 712:
#line 2838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 10852 "Parser/parser.cc"
    break;

  case 713:
#line 2843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 10858 "Parser/parser.cc"
    break;

  case 714:
#line 2845 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 10864 "Parser/parser.cc"
    break;

  case 715:
#line 2847 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ftype; }
#line 10870 "Parser/parser.cc"
    break;

  case 716:
#line 2849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 10876 "Parser/parser.cc"
    break;

  case 717:
#line 2854 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10882 "Parser/parser.cc"
    break;

  case 720:
#line 2861 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 10888 "Parser/parser.cc"
    break;

  case 721:
#line 2866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 10894 "Parser/parser.cc"
    break;

  case 722:
#line 2868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 10900 "Parser/parser.cc"
    break;

  case 723:
#line 2875 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 10906 "Parser/parser.cc"
    break;

  case 725:
#line 2878 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 10912 "Parser/parser.cc"
    break;

  case 726:
#line 2880 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)( (yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 10918 "Parser/parser.cc"
    break;

  case 727:
#line 2885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 10924 "Parser/parser.cc"
    break;

  case 728:
#line 2887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10930 "Parser/parser.cc"
    break;

  case 729:
#line 2889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 10936 "Parser/parser.cc"
    break;

  case 730:
#line 2894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 10942 "Parser/parser.cc"
    break;

  case 731:
#line 2896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 10948 "Parser/parser.cc"
    break;

  case 732:
#line 2901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "10" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), 0 );
		}
#line 10957 "Parser/parser.cc"
    break;

  case 733:
#line 2906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "11" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 10966 "Parser/parser.cc"
    break;

  case 734:
#line 2914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), 0 ); }
#line 10972 "Parser/parser.cc"
    break;

  case 735:
#line 2916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-8].tok), (yyvsp[-6].decl), (yyvsp[-2].decl) ); }
#line 10978 "Parser/parser.cc"
    break;

  case 737:
#line 2922 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl) ); }
#line 10984 "Parser/parser.cc"
    break;

  case 742:
#line 2934 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 10990 "Parser/parser.cc"
    break;

  case 743:
#line 2939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10996 "Parser/parser.cc"
    break;

  case 744:
#line 2941 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 11002 "Parser/parser.cc"
    break;

  case 746:
#line 2949 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl);	}
#line 11008 "Parser/parser.cc"
    break;

  case 747:
#line 2954 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11014 "Parser/parser.cc"
    break;

  case 748:
#line 2956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->appendList( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 11020 "Parser/parser.cc"
    break;

  case 749:
#line 2961 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11026 "Parser/parser.cc"
    break;

  case 751:
#line 2966 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 11032 "Parser/parser.cc"
    break;

  case 752:
#line 2970 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 11038 "Parser/parser.cc"
    break;

  case 753:
#line 2975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( (yyvsp[0].tok) ) ) ); }
#line 11044 "Parser/parser.cc"
    break;

  case 756:
#line 2979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 11053 "Parser/parser.cc"
    break;

  case 757:
#line 2984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( false, (yyvsp[-2].constant), 0 ) ) ); }
#line 11059 "Parser/parser.cc"
    break;

  case 758:
#line 2986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = LinkageSpec::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 11068 "Parser/parser.cc"
    break;

  case 759:
#line 2991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-1].decl);
		}
#line 11078 "Parser/parser.cc"
    break;

  case 760:
#line 2997 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = LinkageSpec::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 11087 "Parser/parser.cc"
    break;

  case 761:
#line 3002 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11097 "Parser/parser.cc"
    break;

  case 762:
#line 3008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 11106 "Parser/parser.cc"
    break;

  case 763:
#line 3013 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11116 "Parser/parser.cc"
    break;

  case 764:
#line 3019 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 11125 "Parser/parser.cc"
    break;

  case 765:
#line 3024 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11135 "Parser/parser.cc"
    break;

  case 766:
#line 3030 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.val) || (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 11144 "Parser/parser.cc"
    break;

  case 767:
#line 3035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11154 "Parser/parser.cc"
    break;

  case 769:
#line 3050 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].sn) ); }
#line 11160 "Parser/parser.cc"
    break;

  case 770:
#line 3052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].sn) ); }
#line 11166 "Parser/parser.cc"
    break;

  case 771:
#line 3057 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; forall = false; }
#line 11172 "Parser/parser.cc"
    break;

  case 772:
#line 3059 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.en) = (yyvsp[-2].en); forall = false;
			if ( (yyvsp[0].decl) ) {
				SemanticError( yylloc, "Attributes cannot be associated with function body. Move attribute(s) before \"with\" clause." );
				(yyval.en) = nullptr;
			} // if
		}
#line 11184 "Parser/parser.cc"
    break;

  case 773:
#line 3070 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11195 "Parser/parser.cc"
    break;

  case 774:
#line 3077 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 11204 "Parser/parser.cc"
    break;

  case 775:
#line 3082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 11213 "Parser/parser.cc"
    break;

  case 776:
#line 3088 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 11219 "Parser/parser.cc"
    break;

  case 777:
#line 3091 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 11225 "Parser/parser.cc"
    break;

  case 778:
#line 3094 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 11231 "Parser/parser.cc"
    break;

  case 779:
#line 3098 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-4].decl) );
		}
#line 11240 "Parser/parser.cc"
    break;

  case 780:
#line 3104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 11246 "Parser/parser.cc"
    break;

  case 781:
#line 3107 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 11252 "Parser/parser.cc"
    break;

  case 782:
#line 3110 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 11258 "Parser/parser.cc"
    break;

  case 786:
#line 3121 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 11264 "Parser/parser.cc"
    break;

  case 787:
#line 3126 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11270 "Parser/parser.cc"
    break;

  case 788:
#line 3128 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = (yyvsp[-2].constant);
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 11280 "Parser/parser.cc"
    break;

  case 789:
#line 3137 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11286 "Parser/parser.cc"
    break;

  case 792:
#line 3144 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11292 "Parser/parser.cc"
    break;

  case 793:
#line 3149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 11298 "Parser/parser.cc"
    break;

  case 795:
#line 3155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11304 "Parser/parser.cc"
    break;

  case 796:
#line 3160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11310 "Parser/parser.cc"
    break;

  case 797:
#line 3162 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 11316 "Parser/parser.cc"
    break;

  case 798:
#line 3164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 11322 "Parser/parser.cc"
    break;

  case 803:
#line 3173 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 11328 "Parser/parser.cc"
    break;

  case 804:
#line 3175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 11334 "Parser/parser.cc"
    break;

  case 805:
#line 3210 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11340 "Parser/parser.cc"
    break;

  case 806:
#line 3212 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11346 "Parser/parser.cc"
    break;

  case 807:
#line 3217 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11352 "Parser/parser.cc"
    break;

  case 809:
#line 3220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11358 "Parser/parser.cc"
    break;

  case 810:
#line 3222 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11364 "Parser/parser.cc"
    break;

  case 811:
#line 3227 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11370 "Parser/parser.cc"
    break;

  case 812:
#line 3229 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11376 "Parser/parser.cc"
    break;

  case 813:
#line 3231 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11382 "Parser/parser.cc"
    break;

  case 814:
#line 3233 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11388 "Parser/parser.cc"
    break;

  case 815:
#line 3238 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11394 "Parser/parser.cc"
    break;

  case 816:
#line 3240 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11400 "Parser/parser.cc"
    break;

  case 817:
#line 3242 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11406 "Parser/parser.cc"
    break;

  case 818:
#line 3244 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11412 "Parser/parser.cc"
    break;

  case 819:
#line 3246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11418 "Parser/parser.cc"
    break;

  case 820:
#line 3248 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11424 "Parser/parser.cc"
    break;

  case 821:
#line 3250 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11430 "Parser/parser.cc"
    break;

  case 822:
#line 3255 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11436 "Parser/parser.cc"
    break;

  case 823:
#line 3257 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 11442 "Parser/parser.cc"
    break;

  case 824:
#line 3259 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11448 "Parser/parser.cc"
    break;

  case 825:
#line 3261 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11454 "Parser/parser.cc"
    break;

  case 826:
#line 3270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11460 "Parser/parser.cc"
    break;

  case 828:
#line 3273 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11466 "Parser/parser.cc"
    break;

  case 829:
#line 3278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11472 "Parser/parser.cc"
    break;

  case 830:
#line 3280 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11478 "Parser/parser.cc"
    break;

  case 831:
#line 3282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 11484 "Parser/parser.cc"
    break;

  case 832:
#line 3284 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11490 "Parser/parser.cc"
    break;

  case 833:
#line 3286 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11496 "Parser/parser.cc"
    break;

  case 834:
#line 3291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11502 "Parser/parser.cc"
    break;

  case 835:
#line 3293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11508 "Parser/parser.cc"
    break;

  case 836:
#line 3295 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11514 "Parser/parser.cc"
    break;

  case 837:
#line 3297 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11520 "Parser/parser.cc"
    break;

  case 838:
#line 3302 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11526 "Parser/parser.cc"
    break;

  case 839:
#line 3304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11532 "Parser/parser.cc"
    break;

  case 840:
#line 3306 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11538 "Parser/parser.cc"
    break;

  case 841:
#line 3308 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11544 "Parser/parser.cc"
    break;

  case 842:
#line 3310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11550 "Parser/parser.cc"
    break;

  case 843:
#line 3312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11556 "Parser/parser.cc"
    break;

  case 847:
#line 3330 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 11562 "Parser/parser.cc"
    break;

  case 848:
#line 3332 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11568 "Parser/parser.cc"
    break;

  case 849:
#line 3334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 11574 "Parser/parser.cc"
    break;

  case 850:
#line 3336 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11580 "Parser/parser.cc"
    break;

  case 851:
#line 3338 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11586 "Parser/parser.cc"
    break;

  case 852:
#line 3343 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11592 "Parser/parser.cc"
    break;

  case 853:
#line 3345 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11598 "Parser/parser.cc"
    break;

  case 854:
#line 3347 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11604 "Parser/parser.cc"
    break;

  case 855:
#line 3349 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11610 "Parser/parser.cc"
    break;

  case 856:
#line 3354 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11616 "Parser/parser.cc"
    break;

  case 857:
#line 3356 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11622 "Parser/parser.cc"
    break;

  case 858:
#line 3358 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11628 "Parser/parser.cc"
    break;

  case 859:
#line 3360 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11634 "Parser/parser.cc"
    break;

  case 860:
#line 3362 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11640 "Parser/parser.cc"
    break;

  case 861:
#line 3364 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11646 "Parser/parser.cc"
    break;

  case 862:
#line 3379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "ID" );
		}
#line 11655 "Parser/parser.cc"
    break;

  case 863:
#line 3384 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11661 "Parser/parser.cc"
    break;

  case 864:
#line 3389 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11667 "Parser/parser.cc"
    break;

  case 866:
#line 3392 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11673 "Parser/parser.cc"
    break;

  case 867:
#line 3394 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11679 "Parser/parser.cc"
    break;

  case 868:
#line 3399 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11685 "Parser/parser.cc"
    break;

  case 869:
#line 3401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11691 "Parser/parser.cc"
    break;

  case 870:
#line 3403 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11697 "Parser/parser.cc"
    break;

  case 871:
#line 3405 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11703 "Parser/parser.cc"
    break;

  case 872:
#line 3410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11709 "Parser/parser.cc"
    break;

  case 873:
#line 3412 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11715 "Parser/parser.cc"
    break;

  case 874:
#line 3414 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11721 "Parser/parser.cc"
    break;

  case 875:
#line 3416 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11727 "Parser/parser.cc"
    break;

  case 876:
#line 3418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11733 "Parser/parser.cc"
    break;

  case 877:
#line 3420 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11739 "Parser/parser.cc"
    break;

  case 878:
#line 3422 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11745 "Parser/parser.cc"
    break;

  case 879:
#line 3427 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11751 "Parser/parser.cc"
    break;

  case 880:
#line 3429 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11757 "Parser/parser.cc"
    break;

  case 881:
#line 3431 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 11763 "Parser/parser.cc"
    break;

  case 882:
#line 3433 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11769 "Parser/parser.cc"
    break;

  case 883:
#line 3435 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11775 "Parser/parser.cc"
    break;

  case 884:
#line 3445 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11781 "Parser/parser.cc"
    break;

  case 885:
#line 3447 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11787 "Parser/parser.cc"
    break;

  case 887:
#line 3450 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11793 "Parser/parser.cc"
    break;

  case 888:
#line 3452 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11799 "Parser/parser.cc"
    break;

  case 889:
#line 3457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11805 "Parser/parser.cc"
    break;

  case 890:
#line 3459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11811 "Parser/parser.cc"
    break;

  case 891:
#line 3461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11817 "Parser/parser.cc"
    break;

  case 892:
#line 3466 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11823 "Parser/parser.cc"
    break;

  case 893:
#line 3468 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11829 "Parser/parser.cc"
    break;

  case 894:
#line 3470 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11835 "Parser/parser.cc"
    break;

  case 895:
#line 3472 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11841 "Parser/parser.cc"
    break;

  case 896:
#line 3477 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11847 "Parser/parser.cc"
    break;

  case 897:
#line 3479 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11853 "Parser/parser.cc"
    break;

  case 898:
#line 3481 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11859 "Parser/parser.cc"
    break;

  case 899:
#line 3495 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11865 "Parser/parser.cc"
    break;

  case 900:
#line 3497 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11871 "Parser/parser.cc"
    break;

  case 902:
#line 3500 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11877 "Parser/parser.cc"
    break;

  case 903:
#line 3502 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11883 "Parser/parser.cc"
    break;

  case 904:
#line 3507 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11889 "Parser/parser.cc"
    break;

  case 905:
#line 3509 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11895 "Parser/parser.cc"
    break;

  case 906:
#line 3514 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11901 "Parser/parser.cc"
    break;

  case 907:
#line 3516 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11907 "Parser/parser.cc"
    break;

  case 908:
#line 3518 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11913 "Parser/parser.cc"
    break;

  case 909:
#line 3523 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11919 "Parser/parser.cc"
    break;

  case 910:
#line 3525 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11925 "Parser/parser.cc"
    break;

  case 911:
#line 3530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11931 "Parser/parser.cc"
    break;

  case 912:
#line 3532 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11937 "Parser/parser.cc"
    break;

  case 914:
#line 3550 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11943 "Parser/parser.cc"
    break;

  case 915:
#line 3552 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11949 "Parser/parser.cc"
    break;

  case 916:
#line 3557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( 0, (yyvsp[0].op) ); }
#line 11955 "Parser/parser.cc"
    break;

  case 917:
#line 3559 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 11961 "Parser/parser.cc"
    break;

  case 918:
#line 3561 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11967 "Parser/parser.cc"
    break;

  case 919:
#line 3563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11973 "Parser/parser.cc"
    break;

  case 920:
#line 3565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11979 "Parser/parser.cc"
    break;

  case 922:
#line 3571 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11985 "Parser/parser.cc"
    break;

  case 923:
#line 3573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11991 "Parser/parser.cc"
    break;

  case 924:
#line 3575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11997 "Parser/parser.cc"
    break;

  case 925:
#line 3580 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 12003 "Parser/parser.cc"
    break;

  case 926:
#line 3582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12009 "Parser/parser.cc"
    break;

  case 927:
#line 3584 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12015 "Parser/parser.cc"
    break;

  case 928:
#line 3590 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false ); }
#line 12021 "Parser/parser.cc"
    break;

  case 929:
#line 3592 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false )->addArray( (yyvsp[0].decl) ); }
#line 12027 "Parser/parser.cc"
    break;

  case 930:
#line 3594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-4].en), 0, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].en), 0, false ) ); }
#line 12033 "Parser/parser.cc"
    break;

  case 932:
#line 3601 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), 0, false ); }
#line 12039 "Parser/parser.cc"
    break;

  case 933:
#line 3603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 12045 "Parser/parser.cc"
    break;

  case 934:
#line 3605 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newArray( (yyvsp[-2].en), 0, false ) ); }
#line 12051 "Parser/parser.cc"
    break;

  case 935:
#line 3607 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 12057 "Parser/parser.cc"
    break;

  case 937:
#line 3642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 12063 "Parser/parser.cc"
    break;

  case 938:
#line 3644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12069 "Parser/parser.cc"
    break;

  case 939:
#line 3646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12075 "Parser/parser.cc"
    break;

  case 940:
#line 3651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].op) ); }
#line 12081 "Parser/parser.cc"
    break;

  case 941:
#line 3653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 12087 "Parser/parser.cc"
    break;

  case 942:
#line 3655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12093 "Parser/parser.cc"
    break;

  case 943:
#line 3657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12099 "Parser/parser.cc"
    break;

  case 944:
#line 3659 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12105 "Parser/parser.cc"
    break;

  case 946:
#line 3665 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12111 "Parser/parser.cc"
    break;

  case 947:
#line 3667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12117 "Parser/parser.cc"
    break;

  case 948:
#line 3669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12123 "Parser/parser.cc"
    break;

  case 949:
#line 3674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 12129 "Parser/parser.cc"
    break;

  case 950:
#line 3676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12135 "Parser/parser.cc"
    break;

  case 951:
#line 3678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12141 "Parser/parser.cc"
    break;

  case 953:
#line 3685 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12147 "Parser/parser.cc"
    break;

  case 955:
#line 3696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false ); }
#line 12153 "Parser/parser.cc"
    break;

  case 956:
#line 3699 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 12159 "Parser/parser.cc"
    break;

  case 957:
#line 3701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, (yyvsp[-2].decl), false ); }
#line 12165 "Parser/parser.cc"
    break;

  case 958:
#line 3704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 12171 "Parser/parser.cc"
    break;

  case 959:
#line 3706 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 12177 "Parser/parser.cc"
    break;

  case 960:
#line 3708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-4].decl), true ); }
#line 12183 "Parser/parser.cc"
    break;

  case 962:
#line 3723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12189 "Parser/parser.cc"
    break;

  case 963:
#line 3725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12195 "Parser/parser.cc"
    break;

  case 964:
#line 3730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( 0, (yyvsp[0].op) ); }
#line 12201 "Parser/parser.cc"
    break;

  case 965:
#line 3732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 12207 "Parser/parser.cc"
    break;

  case 966:
#line 3734 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12213 "Parser/parser.cc"
    break;

  case 967:
#line 3736 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12219 "Parser/parser.cc"
    break;

  case 968:
#line 3738 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12225 "Parser/parser.cc"
    break;

  case 970:
#line 3744 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12231 "Parser/parser.cc"
    break;

  case 971:
#line 3746 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12237 "Parser/parser.cc"
    break;

  case 972:
#line 3748 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12243 "Parser/parser.cc"
    break;

  case 973:
#line 3753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12249 "Parser/parser.cc"
    break;

  case 974:
#line 3755 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12255 "Parser/parser.cc"
    break;

  case 977:
#line 3765 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12261 "Parser/parser.cc"
    break;

  case 980:
#line 3776 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12267 "Parser/parser.cc"
    break;

  case 981:
#line 3778 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12273 "Parser/parser.cc"
    break;

  case 982:
#line 3780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12279 "Parser/parser.cc"
    break;

  case 983:
#line 3782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12285 "Parser/parser.cc"
    break;

  case 984:
#line 3784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12291 "Parser/parser.cc"
    break;

  case 985:
#line 3786 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12297 "Parser/parser.cc"
    break;

  case 986:
#line 3793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 12303 "Parser/parser.cc"
    break;

  case 987:
#line 3795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12309 "Parser/parser.cc"
    break;

  case 988:
#line 3797 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 12315 "Parser/parser.cc"
    break;

  case 989:
#line 3799 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 12321 "Parser/parser.cc"
    break;

  case 990:
#line 3801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12327 "Parser/parser.cc"
    break;

  case 991:
#line 3804 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 12333 "Parser/parser.cc"
    break;

  case 992:
#line 3806 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12339 "Parser/parser.cc"
    break;

  case 993:
#line 3808 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 12345 "Parser/parser.cc"
    break;

  case 994:
#line 3810 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 12351 "Parser/parser.cc"
    break;

  case 995:
#line 3812 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12357 "Parser/parser.cc"
    break;

  case 996:
#line 3817 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 12363 "Parser/parser.cc"
    break;

  case 997:
#line 3819 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 12369 "Parser/parser.cc"
    break;

  case 998:
#line 3824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 12375 "Parser/parser.cc"
    break;

  case 999:
#line 3826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl)->addQualifiers( (yyvsp[-4].decl) ), true ); }
#line 12381 "Parser/parser.cc"
    break;

  case 1001:
#line 3853 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12387 "Parser/parser.cc"
    break;

  case 1005:
#line 3864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12393 "Parser/parser.cc"
    break;

  case 1006:
#line 3866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12399 "Parser/parser.cc"
    break;

  case 1007:
#line 3868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12405 "Parser/parser.cc"
    break;

  case 1008:
#line 3870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12411 "Parser/parser.cc"
    break;

  case 1009:
#line 3872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12417 "Parser/parser.cc"
    break;

  case 1010:
#line 3874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12423 "Parser/parser.cc"
    break;

  case 1011:
#line 3881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12429 "Parser/parser.cc"
    break;

  case 1012:
#line 3883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12435 "Parser/parser.cc"
    break;

  case 1013:
#line 3885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12441 "Parser/parser.cc"
    break;

  case 1014:
#line 3887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12447 "Parser/parser.cc"
    break;

  case 1015:
#line 3889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12453 "Parser/parser.cc"
    break;

  case 1016:
#line 3891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12459 "Parser/parser.cc"
    break;

  case 1017:
#line 3896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 12465 "Parser/parser.cc"
    break;

  case 1018:
#line 3898 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 12471 "Parser/parser.cc"
    break;

  case 1019:
#line 3900 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 12477 "Parser/parser.cc"
    break;

  case 1020:
#line 3907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 12483 "Parser/parser.cc"
    break;

  case 1021:
#line 3909 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 12489 "Parser/parser.cc"
    break;

  case 1024:
#line 3933 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 12495 "Parser/parser.cc"
    break;

  case 1025:
#line 3935 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 12501 "Parser/parser.cc"
    break;


#line 12505 "Parser/parser.cc"

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
#line 3938 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "make install" //
// End: //
