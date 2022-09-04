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

#line 667 "Parser/parser.cc"

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
#define YYNRULES  1028
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  2077

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
    2078,  2080,  2082,  2084,  2086,  2089,  2091,  2093,  2098,  2100,
    2102,  2104,  2106,  2108,  2110,  2112,  2114,  2116,  2118,  2120,
    2122,  2124,  2126,  2128,  2130,  2132,  2134,  2136,  2138,  2140,
    2142,  2144,  2146,  2148,  2150,  2152,  2157,  2158,  2162,  2169,
    2170,  2176,  2177,  2179,  2181,  2183,  2188,  2190,  2195,  2196,
    2198,  2200,  2205,  2207,  2209,  2211,  2213,  2215,  2220,  2227,
    2229,  2231,  2236,  2244,  2243,  2247,  2255,  2256,  2258,  2260,
    2265,  2266,  2268,  2273,  2274,  2276,  2278,  2283,  2284,  2286,
    2291,  2293,  2295,  2297,  2298,  2300,  2305,  2307,  2309,  2314,
    2321,  2325,  2326,  2331,  2330,  2335,  2334,  2353,  2352,  2364,
    2363,  2374,  2379,  2380,  2385,  2391,  2405,  2406,  2410,  2412,
    2414,  2420,  2422,  2424,  2426,  2428,  2430,  2432,  2434,  2440,
    2441,  2446,  2455,  2457,  2466,  2468,  2469,  2470,  2472,  2474,
    2475,  2480,  2481,  2482,  2487,  2489,  2492,  2499,  2500,  2501,
    2507,  2512,  2514,  2520,  2521,  2527,  2528,  2532,  2537,  2540,
    2539,  2543,  2546,  2548,  2556,  2555,  2564,  2570,  2574,  2576,
    2581,  2583,  2585,  2587,  2593,  2594,  2595,  2602,  2603,  2605,
    2606,  2607,  2609,  2611,  2618,  2619,  2621,  2623,  2628,  2629,
    2635,  2636,  2638,  2639,  2644,  2645,  2646,  2648,  2656,  2657,
    2659,  2662,  2664,  2668,  2669,  2670,  2672,  2674,  2679,  2681,
    2686,  2688,  2697,  2699,  2704,  2705,  2706,  2710,  2711,  2712,
    2717,  2718,  2723,  2724,  2725,  2726,  2730,  2731,  2736,  2737,
    2738,  2739,  2740,  2754,  2755,  2760,  2761,  2767,  2769,  2772,
    2774,  2776,  2799,  2800,  2806,  2807,  2813,  2812,  2822,  2821,
    2825,  2831,  2837,  2838,  2840,  2844,  2849,  2851,  2853,  2855,
    2861,  2862,  2866,  2867,  2872,  2874,  2881,  2883,  2884,  2886,
    2891,  2893,  2895,  2900,  2902,  2907,  2912,  2920,  2922,  2927,
    2928,  2933,  2934,  2938,  2939,  2940,  2945,  2947,  2953,  2955,
    2960,  2962,  2968,  2969,  2973,  2977,  2981,  2983,  2984,  2985,
    2990,  2993,  2992,  3004,  3003,  3015,  3014,  3026,  3025,  3037,
    3036,  3050,  3056,  3058,  3064,  3065,  3076,  3083,  3088,  3094,
    3097,  3100,  3104,  3110,  3113,  3116,  3121,  3122,  3123,  3127,
    3133,  3134,  3144,  3145,  3149,  3150,  3155,  3160,  3161,  3167,
    3168,  3170,  3175,  3176,  3177,  3178,  3179,  3181,  3216,  3218,
    3223,  3225,  3226,  3228,  3233,  3235,  3237,  3239,  3244,  3246,
    3248,  3250,  3252,  3254,  3256,  3261,  3263,  3265,  3267,  3276,
    3278,  3279,  3284,  3286,  3288,  3290,  3292,  3297,  3299,  3301,
    3303,  3308,  3310,  3312,  3314,  3316,  3318,  3330,  3331,  3332,
    3336,  3338,  3340,  3342,  3344,  3349,  3351,  3353,  3355,  3360,
    3362,  3364,  3366,  3368,  3370,  3385,  3390,  3395,  3397,  3398,
    3400,  3405,  3407,  3409,  3411,  3416,  3418,  3420,  3422,  3424,
    3426,  3428,  3433,  3435,  3437,  3439,  3441,  3451,  3453,  3455,
    3456,  3458,  3463,  3465,  3467,  3472,  3474,  3476,  3478,  3483,
    3485,  3487,  3501,  3503,  3505,  3506,  3508,  3513,  3515,  3520,
    3522,  3524,  3529,  3531,  3536,  3538,  3555,  3556,  3558,  3563,
    3565,  3567,  3569,  3571,  3576,  3577,  3579,  3581,  3586,  3588,
    3590,  3596,  3598,  3600,  3603,  3607,  3609,  3611,  3613,  3647,
    3648,  3650,  3652,  3657,  3659,  3661,  3663,  3665,  3670,  3671,
    3673,  3675,  3680,  3682,  3684,  3690,  3691,  3693,  3702,  3705,
    3707,  3710,  3712,  3714,  3728,  3729,  3731,  3736,  3738,  3740,
    3742,  3744,  3749,  3750,  3752,  3754,  3759,  3761,  3769,  3770,
    3771,  3776,  3777,  3782,  3784,  3786,  3788,  3790,  3792,  3799,
    3801,  3803,  3805,  3807,  3810,  3812,  3814,  3816,  3818,  3823,
    3825,  3827,  3832,  3858,  3859,  3861,  3865,  3866,  3870,  3872,
    3874,  3876,  3878,  3880,  3887,  3889,  3891,  3893,  3895,  3897,
    3902,  3904,  3906,  3913,  3915,  3933,  3935,  3940,  3941
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

#define YYPACT_NINF (-1704)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-909)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     141, 12176,   264,   290, 16366,   196, -1704, -1704, -1704, -1704,
   -1704, -1704, -1704, -1704, -1704, -1704, -1704, -1704,   202,   770,
     246, -1704, -1704, -1704, -1704, -1704, -1704, -1704, -1704, -1704,
   -1704, -1704, -1704, -1704, -1704, -1704, -1704, -1704, -1704, -1704,
   -1704, -1704, -1704, -1704, -1704, -1704, -1704, -1704,    96,   355,
   -1704, -1704, -1704, -1704, -1704, -1704,  3590,  3590,   252, 12176,
     284,   302, -1704, -1704,   311, -1704, -1704, -1704, -1704, -1704,
   -1704, -1704, -1704, -1704,  2877, -1704,   681,   377, -1704, -1704,
   -1704, -1704, -1704, 16216, -1704, -1704,   441,   561,   515,   229,
   -1704,  3590,   561,   561,   561,   551,  3798,   734,   762, 12336,
   -1704, -1704, -1704, 16066,  1297, -1704, -1704, -1704,  3130,   754,
   12388,  2128,  1098,  3130,  1126,   624, -1704, -1704, -1704, -1704,
     730, -1704, -1704, -1704, -1704,   651, -1704, -1704, -1704, -1704,
   -1704,   664,   663,   730, -1704,   730,   677, -1704, -1704, -1704,
   17061,  3590, -1704, -1704,  3590, -1704, 12176,   735, 17113, -1704,
   -1704,  4531, 18125, -1704,   826,   826,   679,  2502, -1704, -1704,
   -1704, -1704,   370, 13911,  2315,   730, -1704, -1704, -1704, -1704,
   -1704, -1704,   701, -1704,   702,   761,   777, -1704,   833, 20611,
   15234,  2861,  2877,    48,   778,   789,   802,   812,   840,   843,
   -1704, -1704, 17263, 11199,   786, -1704,  9146, -1704, -1704, -1704,
   -1704,   848, -1704, -1704,   847, -1704,  8967,   984,  9237, -1704,
     858,  3590,   663,   871,   867,   873,   875, -1704, -1704, -1704,
    3165,  4086,   878,   961,    52, -1704, -1704,   730,   730,    90,
     128,    80,    90, -1704,   730,   730, -1704,  4148, -1704, -1704,
     890,   893,   826, 18502, -1704, -1704, 16216, -1704, -1704,  3130,
   -1704,  1004,   624,   915,   991,   128,  3590,   515, -1704, 13434,
   -1704,   826,   826,   928,   991,   128,  3590, -1704, 14756, -1704,
   -1704,   826, -1704,   826, -1704,   815,  2828,  3590, -1704,   903,
     950, -1704, -1704, -1704,  4911,   663,   231, -1704, -1704, 18175,
   -1704,   961,    71, -1704, 20611, 18125,  3377,  4148, -1704,    89,
   -1704, -1704, -1704, 17113,  3590, -1704,   933, -1704, -1704, -1704,
   -1704,  3590,  2380,   536,   480, -1704,  3590,   702, -1704,   814,
     730,   730,   955, 17315,   784, 14388, 18554,  3130,  3130, -1704,
    3130,   826,  3130,   826, -1704, -1704,   730, -1704,   945, -1704,
   17465, -1704, -1704, -1704, 17517,   848, -1704,   963,   411,  1368,
     979,   624,   987, -1704,  2502,  1010,   702,  2502,   943, -1704,
    1009,  1070, 20683,  1049,  1051, 20611, 20755,  1054, 13325, -1704,
   -1704, -1704, -1704, -1704, -1704, 20827, 20827, 15080,  1072,  3192,
   -1704, -1704, -1704, -1704,   604, -1704,   711, -1704,  2894, -1704,
   20611, 20611, -1704,  1040,   611,   837,   980,   583,   942,  1069,
    1081,  1071,  1144,    41, -1704,   643, -1704,  1137, -1704,   972,
    3485, 15542, -1704, -1704,  1058,  1137, -1704, -1704,   692, -1704,
   -1704,  2861,  1140,  1162,  1165,  1168,  1175,  1180, -1704, -1704,
     333,  1181, -1704,   704,  1181, -1704, -1704, 17061, -1704,  1019,
    1182, 15696, -1704, -1704,  3652,  3908,  1223, 14388,  1232,   637,
     725, -1704, -1704, -1704, -1704, -1704,  3590,  4133, -1704, -1704,
   -1704, -1704, -1704, -1704, 18318,  3304,  1072,  8967,  1220,  1225,
   -1704, -1704,  1224,  9237,   825, -1704, -1704, -1704, 18955,  1236,
   -1704, -1704, -1704, -1704, -1704,  3165,   930,  1251,  1268,  1293,
     935,  1295,  1302,  1313,  4086, -1704, -1704,   730,  1267,   515,
    1285, -1704, -1704,  1317, -1704, -1704,   663,   991, -1704, -1704,
   -1704,   663, -1704, -1704,  4148, -1704, 15542, 15542, -1704,   826,
    4531, 18648, 14547, -1704, -1704, -1704, -1704, -1704,   663,   991,
      71, -1704, -1704,  3130,  1298,   991,   128, -1704,   663,   991,
   -1704, 21197, -1704,   826,   826, -1704, -1704,  1333,   487,  1338,
     624,  1340, -1704, 16525, -1704,   715, -1704,  1443, 18399, -1704,
    4531, 16014, 18502, -1704,  4911, 20899, -1704, -1704, -1704, -1704,
   -1704,  3377,   940,  4148, -1704, 14547,   961, 12176, -1704,  1371,
   -1704,  1387, -1704, -1704, -1704, -1704, -1704,  2502, -1704, -1704,
    1463,  3491,  1389, 17517, 11199, -1704, 17667, -1704,   826,   826,
   -1704, -1704,   848, -1704,  1085,  1401,  1541, 20611,   946,  1317,
    1392, -1704,   730,   730, -1704,  1181, -1704, 17315, -1704, -1704,
   16807,   826,   826, -1704,  3491,   730, -1704, 17982, -1704, -1704,
   17465, -1704,   370,  1414,  1399,  1420,  1368,   747, 17113,   820,
   -1704, -1704, -1704, -1704, -1704, -1704,   831, -1704,  1430,  1406,
   -1704, 15388, -1704, 17719, 17719, -1704, 15388, -1704, 20611, -1704,
   12388, 12388, 15388, -1704, -1704, 16859, 17719, 17719,   972,  1265,
    1353,   700,  1472, -1704,   834,  1436,  1091,  1437, -1704, 18955,
   20611, 19027,  1432, 20611,   903, 20611,   903, -1704,  1822, -1704,
   -1704, 19099,  2130, 20611, 19099,   903, -1704, -1704, 20611, 20611,
   20611, 20611, 20611, 20611, 20611, 20611, 20611, 20611, 20611, 20611,
   20611, 20611, 20611, 20611, 20611, 20611, 20611, 19171,  1418,   833,
    2592, 11199, -1704, -1704, -1704, -1704, -1704, -1704, -1704, -1704,
   -1704, -1704, -1704,  1434, 20611, -1704, -1704,  1058,  1254, -1704,
   -1704,   730,   730, -1704, -1704, 15542, -1704,   340,  1181, -1704,
     839,  1181, -1704, -1704, -1704,  1317, -1704, -1704,  1317, 20971,
   -1704, -1704, 11199,  1439,  1441,  2197,  1579,  2737,   358,  1392,
   -1704,   730,   730,  1392,   379, -1704,   730,   730, 20611,  3590,
    1067,  1233,  1392,    59, 13752, 13752,  3590, -1704, -1704, 20611,
    1224, -1704,  8967,  1460, -1704,  1493, -1704, -1704, -1704, -1704,
   -1704,   876, -1704, 13752,   903,  4531,   903,   887,  1458,  1461,
    1462,   948,  1464,  1465,  1470,   402,  1181, -1704, -1704,   425,
    1181, -1704, -1704, -1704,  4531,   833, -1704,  1181, 20971, -1704,
     663, 16525, -1704, -1704,   964,  1473,   982,  1475, -1704,  1466,
   -1704,   663, -1704, -1704,   663,   991,  1466, -1704,   663,  1471,
    1474,  1477, -1704, -1704, 16807, -1704,  1479, -1704, -1704, -1704,
     903,  3590, 10353,  1551,  1467, 17878, -1704,  1182, -1704, 13752,
    1003, -1704, -1704,  1466, -1704, 17113, 15542,  1469, -1704,  1469,
   -1704, -1704, -1704,  1368, -1704, 17465, -1704, 11362, 15850, -1704,
   16525,  1484,  1486,  1488, -1704,  6212,   730, -1704,   946, -1704,
   -1704, -1704, -1704,  1317, -1704, -1704, -1704,   826, -1704,  2403,
   -1704, -1704,   624,  1849,  1492, 19243, -1704,  1368,  1414, -1704,
   -1704,  1487,  1496,   943, 19099, -1704,  1497,   377,  1494,  1499,
    1500,  1501,  1503, 20611,  1504,  1507,  1508, 11199, 20611, -1704,
   -1704,  1515, -1704, -1704, -1704, 20611, -1704,  1509,  1510, 18811,
    1235, -1704, 19099,  1511, -1704,  1513, -1704, -1704,  3275, -1704,
   -1704,  1002, -1704, -1704, -1704, -1704,  3275, -1704, -1704,  1238,
     136, -1704, -1704,  1040,  1040,  1040,   611,   611,   837,   837,
     980,   980,   980,   980,   583,   583,   942,  1069,  1081,  1071,
    1144, 20611,  1239, -1704,  1518,  3275, -1704, -1704,  8967, -1704,
   16525,  1522,  1523,  1524,  1254, -1704, -1704, -1704, -1704, -1704,
   -1704, -1704, -1704,  1317, -1704, -1704,  1317, 16525, 16525, -1704,
   -1704,  2197,   981,  1525,  1526,  1528,  1529,   782,  2737, -1704,
   -1704, -1704, -1704, -1704, -1704, -1704, -1704, -1704, -1704, -1704,
   -1704, -1704, -1704,  1527, -1704,  1392, -1704, -1704, -1704, -1704,
   -1704, -1704, -1704, -1704,  1534,  1537, -1704,   515,  3275,  1243,
     325, -1704, -1704,  1516, -1704,  9237, -1704, 20611,   730, 19315,
   13752, -1704, -1704, -1704,  1517,   434,  1181, -1704,   463,  1181,
   -1704, -1704, -1704, -1704,  1317, -1704, -1704, -1704,  1317,   961,
    1538,  1317, -1704, -1704, -1704, -1704, -1704, -1704, -1704,  1544,
   -1704, -1704,  1466, -1704,   663, -1704, -1704, -1704, -1704, -1704,
    9464,  1543,  1540, -1704,   249, -1704,   356,   207, 11036,  1547,
   14917,  1549,  1550,  2280,  2292,  1601, 19387,  1555, -1704, -1704,
    1565,  1569, -1704, -1704,   663, 20611, 20611,  1706,  1564,   331,
   -1704,  1649,  1567,  1552, -1704, -1704, -1704, 10180, -1704, -1704,
   -1704, -1704, -1704,  2149, -1704, -1704, -1704,  1634, -1704, -1704,
   -1704,   903, -1704, -1704,  9626, 16216,  1568, -1704,  3590, -1704,
    1554,  1575,  1576, -1704,  1245, -1704, -1704, -1704, -1704,  4531,
   -1704, -1704,  1557,  1558,  1025, 17113,   702,   702,  1414, -1704,
   -1704,  1072,  1182, 15696, -1704,  1137, -1704, 11525, -1704,   511,
    1181, -1704,   826,  7893, -1704, -1704,  1368,   730,   730,   370,
    1399, -1704,  8967, -1704,  1414,  1586,  1587, -1704, -1704,  1031,
     369, 11199,   903, -1704,   369, 16911,   369, -1704, 20611, 20611,
   20611, -1704, -1704, -1704, -1704, 20611, 20611,  1581,  8967, -1704,
   -1704,  1584,   579, -1704, -1704, -1704,  2528, -1704, -1704,  1250,
   -1704,   238, -1704, 19099,  1255, -1704, 18955, -1704, -1704, 20611,
    1562,  1266,  1282,  1224, -1704,   520,  1181, -1704, -1704, 16525,
   16525, -1704, -1704,  1589,   525,  1181, -1704,   527,  2973,   730,
     730, -1704, -1704, 16525, 16525, -1704,  1590, -1704, 14547, 14547,
    1594,  1591,  1592,  1597, -1704,  1595, 20611, 20611,  1288,  1599,
   -1704, -1704, -1704, -1704, -1704, -1704, -1704,  1598, 20611, -1704,
   -1704, -1704,  1317, -1704, -1704, -1704,  1317, 16525, 16525,   515,
     730,  1304,  1603,  1593, -1704, -1704,  1608, 12809, 12962, 13115,
   17113, 17719, 17719,  1609, -1704,  1583,  1588,  2044,  8560, -1704,
     303,  3590, -1704, -1704,  3590, -1704, 18883,   368,   466, -1704,
   -1704, -1704, -1704, 20611,  1615,  1689, 10872, 10526, -1704,  1596,
   -1704,  1600, 20611,  1602,  8967,  1604, 20611, 18955, 20611,  1411,
   -1704,  1607,    98, -1704,    75,  1620, -1704, -1704,  1623, -1704,
    1611, -1704,  1612,  1624, 14917,   431, 13593,   730,   347, -1704,
   -1704, -1704,  1626, -1704,  1640, -1704,  1644, -1704,  1627, -1704,
    1638, -1704, -1704, -1704, -1704,  1650, 11688,  1643,  1645,  1646,
   -1704,  1651, -1704, -1704, -1704,  1317, 20611, 20611,  1182,  1652,
   -1704,  1414, -1704,  1654,   100, -1704,  1224,  1655, -1704, -1704,
   17113, -1704,  1656,  1653,  1061, -1704,  1658, -1704, -1704, -1704,
   -1704, -1704,  8967,  1224, 18955, -1704,  1697,  3275, -1704,  1697,
    1697, -1704,  3275,  2629,  2805, -1704, -1704,  1308, -1704, -1704,
   -1704,  1668,  1666, -1704, -1704, -1704,  1317, -1704, -1704,  1667,
    1670,   730, -1704, -1704, -1704,  1317, -1704, -1704, -1704,  1671,
   -1704, -1704, -1704, -1704, -1704, -1704, -1704, -1704, -1704, -1704,
   -1704, -1704, -1704,  1665, -1704, -1704, -1704, -1704,  1672,  1679,
     730, -1704, 16525, 16525, -1704, -1704, -1704, -1704, 20611, -1704,
   -1704,  1683, -1704,  1609,  1609,  1609,  1088,  1659,   417, -1704,
    4445,   429, 15542, -1704, -1704, -1704,  3734, 20611,  5977,   456,
   -1704, -1704,   117,  1678,  1678,  3590, -1704, -1704, 16675, -1704,
   20611,  1684,  1687, -1704, -1704, -1704, -1704,  1077,  1691, 14917,
    1567,  1693, 20611,   441,  1692,   551, 13275, 17113, -1704, -1704,
   -1704,   680, 14917, 20611,  1178,   716, -1704, 20611, 18659, -1704,
   -1704,   543, -1704,  1224, -1704,  1078,  1084,  1120, -1704, -1704,
   -1704, -1704,   663,  1411,  1702, -1704, -1704, 20611, -1704,  1703,
     833, 11036, -1704, -1704, -1704, -1704, 20611,  1735, -1704, 10007,
   -1704,   730, 14547, -1704, -1704, 17113, -1704, -1704, -1704, -1704,
   -1704, -1704,  1699, -1704, 16525, -1704, -1704,  1701, -1704,  1707,
    1708,  1700,  1368, -1704,  1712, -1704, -1704, -1704, 20611, -1704,
   16911, 20611,  1224,  1714,  1312, -1704,  1346, -1704,  3275, -1704,
    3275, -1704, -1704, -1704, -1704, 16525,  1715,  1716, -1704, -1704,
   16525, 16525,  1718,  1719,  1354, 14070, 14229, -1704,  1711, -1704,
   -1704, -1704, -1704,  1721,  1722,  1360, -1704, -1704, -1704, -1704,
    1088,  1413,   546, -1704, -1704, -1704, -1704,   730,   730, -1704,
   -1704, -1704,   548, -1704,  1133,  3734,   532, -1704,  5977,   730,
   -1704, -1704, -1704, -1704, -1704, -1704, -1704, -1704,   592, 14917,
     225, 19459,  1804, 14917,  1567, 14706, -1704, -1704, -1704, -1704,
   20611, -1704, 19531,  1805,  1705, 18735, 19603, 14917, 10699,  1567,
     773,  1206,  1717, 20611, -1704,  1734,   360, 14917, -1704, -1704,
    1736, -1704, -1704,  1720,   833,   753,  1737,  1738,  1275,  1802,
   -1704, -1704, -1704, -1704,  3590,  4531, -1704, -1704,  1741,  1744,
   -1704, -1704, -1704,  1368,  1414, -1704,  1757, -1704, -1704, -1704,
    1760, -1704, -1704, -1704,  1362,  1380, -1704, -1704, -1704, -1704,
   -1704, -1704, -1704, -1704, -1704, -1704,  1758, -1704, -1704,  1761,
    1762, -1704, -1704, -1704,  1764,  1765,  1767,  1413, -1704,   730,
   -1704, -1704, -1704, -1704, -1704,  1739,  4445, -1704, -1704,  5792,
     163, 11854, -1704, 14799, -1704,    51,  1143, 14917,  1838,   596,
    1772,   501, 14917, 20611,  1773,   773,  1206,  1752, 21043,  1774,
     559,  1865, -1704, 19675, 19747, 20611,  1567,  1769, 12016, -1704,
   -1704, -1704, 17930, -1704,  1788,  1771,    37, 14917, -1704, 20611,
   19099,   606, -1704, -1704, -1704,  1795, -1704, -1704,  1414,  1801,
   -1704, -1704, -1704, -1704,  1800,  1811,  1812, 14547,  1810, -1704,
   -1704,   557,  1181, -1704, -1704,  1088, -1704, -1704,   378, -1704,
     132, -1704, -1704, -1704,  1818, 12496, -1704, -1704, 14917, -1704,
      65, -1704, 14917, 20611,  1821, 19819, -1704, -1704, 19891, 19963,
   20611,  1773,  1567, 20035, 20107, 14917,  1808,   564,  1809,   594,
   -1704, -1704,  1828, 12496, 17930, -1704,  3934, 17667,   903,  1823,
   -1704,  1876,  1832,   791,  1829, -1704,  1910, -1704,  1160, 14917,
    1836, 14917, 14917, -1704,  1841, -1704, -1704, -1704, -1704, -1704,
   -1704, -1704, -1704,  1317, -1704, 20611, -1704, 20611, -1704, -1704,
    1468, 12656, -1704, -1704, 14917, -1704, -1704,  1567, -1704, -1704,
    1567,  1825,   595,  1827,   609, -1704, -1704,  1567, -1704,  1567,
   -1704,  1843, 20179, 20251, 20323, -1704,  1468, -1704,  1820,  3077,
    3636, -1704, -1704, -1704,    37,  1842, 20611,  1824,    37,    37,
   14917, -1704, -1704, 20611,  1879,  1888, -1704, 16525, -1704, -1704,
   14799, -1704,  1468, -1704, -1704,  1851, 20395, 20467, 20539, -1704,
   -1704,  1567, -1704,  1567, -1704,  1567, -1704,  1820, 20611,  1848,
    3636,  1847,   833,  1853, -1704,   795, -1704, -1704,  1163,  1802,
     447, -1704, -1704,  9729,  1861, 14799, -1704, -1704,  1567, -1704,
    1567, -1704,  1567,  1866,  1862, -1704,   663,   833,  1867, -1704,
    1846,   833, -1704, -1704, 14917,  1944,  1873, -1704, -1704, -1704,
    9883, -1704,   663, -1704, -1704,  1393, 20611, -1704,  1174, -1704,
   14917, -1704, -1704,   833,   903,  1874,  1855, -1704, -1704, -1704,
    1186, -1704, -1704,  1856,   903, -1704, -1704
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
     491,   492,   493,   494,   495,   496,   503,   504,   792,   506,
     579,   580,   583,   585,   581,   587,     0,     0,     0,   452,
       0,     0,    16,   550,   556,     9,    10,    11,    12,    13,
      14,    15,   756,    97,     0,    19,     0,     2,    95,    96,
      17,    18,   808,   452,   757,   401,     0,   404,   682,   406,
     415,     0,   405,   435,   436,     0,     0,     0,     0,   533,
     454,   456,   462,   452,   464,   467,   518,   505,   440,   511,
     516,   441,   528,   442,   543,   547,   553,   532,   559,   571,
     792,   576,   577,   560,   627,   407,   408,     3,   758,   771,
     457,     0,     0,   792,   830,   792,     2,   847,   848,   849,
     452,     0,  1006,  1007,     0,     1,   452,     0,   452,   424,
     425,     0,   533,   446,   447,   448,   761,     0,   582,   584,
     586,   588,     0,   452,     0,   793,   794,   578,   507,   675,
     676,   674,   735,   730,   720,     0,     0,   759,     0,     0,
     452,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     551,   554,   452,   452,     0,  1008,   533,   837,   855,  1012,
    1005,  1003,  1010,   400,     0,   159,   688,   158,     0,   409,
       0,     0,     0,     0,     0,     0,     0,   399,   907,   908,
       0,     0,   434,   790,   792,   786,   811,   792,   792,   788,
       2,   792,   787,   868,   792,   792,   865,     0,   526,   527,
       0,     0,   452,   452,   469,     2,   452,   416,   455,   465,
     519,     0,   548,     0,   774,     2,     0,   682,   417,   533,
     512,   529,   544,     0,   774,     2,     0,   468,   513,   520,
     521,   530,   535,   545,   549,     0,   563,     0,   750,     2,
       2,   772,   829,   831,   452,     0,     2,     2,  1016,   533,
    1019,   790,   790,     3,     0,   533,     0,     0,   427,   792,
     788,   787,     2,   452,     0,   754,     0,   716,   718,   717,
     719,     0,     0,   712,     0,   702,     0,   711,   722,     0,
     792,   792,     2,   452,  1027,   453,   452,   464,   443,   511,
     444,   536,   445,   543,   540,   561,   792,   562,     0,   663,
     452,   664,   981,   982,   452,   665,   667,   550,   556,     0,
     628,   629,     0,   795,     0,   733,   721,     0,   799,    21,
       0,    20,     0,     0,     0,     0,     0,     0,    23,    25,
       4,     8,     5,     6,     7,     0,     0,   452,     2,     0,
      98,    99,   100,   101,    82,    24,    83,    38,    81,   102,
       0,     0,   117,   119,   123,   126,   129,   134,   137,   139,
     141,   143,   145,   147,   150,     0,    26,     0,   557,     2,
     102,   452,   151,   727,   678,   547,   680,   726,     0,   677,
     681,     0,     0,     0,     0,     0,     0,     0,   809,   835,
     792,   845,   853,   857,   863,     2,  1014,   452,  1017,     2,
      95,   452,     3,   662,     0,  1027,     0,   453,   511,   536,
     543,     3,     3,   644,   648,   658,   664,   665,     2,   838,
     856,  1004,     2,     2,    23,     0,     2,   688,    24,     0,
     686,   689,  1025,     0,     0,   695,   684,   683,     0,     0,
     776,     2,     2,     2,     2,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   814,   871,   792,     0,   682,
       2,   810,   818,   934,   812,   813,     0,   774,     2,   867,
     875,     0,   869,   870,     0,   430,   452,   452,   517,   453,
       0,   533,   452,  1009,  1013,  1011,   534,   754,     0,   774,
     790,   410,   418,   466,     0,   774,     2,   754,     0,   774,
     731,   514,   515,   531,   546,   552,   555,   550,   556,   574,
     575,     0,   732,   452,   672,     0,   196,   393,   452,     3,
       0,   533,   452,   773,   452,     0,   412,     2,   413,   751,
     432,     0,     0,     0,     2,   452,   790,   452,   754,     0,
       2,     0,   715,   714,   713,   708,   463,     0,   706,   723,
     509,     0,     0,   452,   452,   983,   453,   449,   450,   451,
     987,   978,   979,   985,     2,     2,    96,     0,   943,   957,
    1027,   939,   792,   792,   948,   955,   670,   452,   541,   666,
     453,   537,   538,   542,     0,   792,   993,   453,   998,   990,
     452,   995,     0,  1025,   634,     0,     0,     0,   452,     0,
     807,   806,   802,   804,   805,   803,     0,   797,   800,     0,
      22,   452,    89,   452,   452,    84,   452,    91,     0,    32,
       0,    33,   452,    87,    88,   452,   452,   452,     2,    98,
      99,     0,     0,   177,     0,     0,   577,     0,  1003,     0,
       0,     0,     0,     0,     0,     0,     0,    50,     0,    56,
      57,    61,     0,     0,    61,     0,    85,    86,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   452,   160,   161,   162,   163,   164,   165,   166,   167,
     168,   169,   170,   158,     0,   156,   157,     2,   919,   679,
     916,   792,   792,   924,   558,   452,   836,   792,   846,   854,
     858,   864,     2,   839,   841,   843,     2,   859,   861,     0,
    1015,  1018,   452,     0,     0,     2,    96,   943,   792,  1027,
     889,   792,   792,  1027,   792,   904,   792,   792,     3,   666,
       0,     0,  1027,  1027,   452,   452,     0,     2,   697,     0,
    1025,   694,  1026,     0,   690,     0,     2,   693,   696,   174,
     173,     0,     2,   452,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   792,   823,   827,   866,   792,
     880,   885,   815,   872,     0,     0,   438,   931,     0,   777,
       0,   452,   778,   431,     0,     0,     0,     0,   429,     2,
     779,     0,   414,   754,     0,   774,     2,   780,     0,     0,
       0,     0,   589,   651,   453,     3,     3,   655,   654,   850,
       0,     0,   452,   394,     0,   533,     3,    95,     3,   452,
       0,     3,   755,     2,   710,   452,   452,   704,   703,   704,
     510,   508,   628,     0,   989,   452,   994,   453,   452,   980,
     452,     0,     0,     0,   958,     0,   792,  1028,   944,   945,
     671,   941,   942,   956,   984,   988,   986,   539,   574,     0,
     992,   997,   631,  1026,     0,     0,   630,     0,  1025,   736,
     734,     0,     0,   799,    61,   760,     0,     2,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   452,     0,   116,
     115,     0,   112,   111,    27,     0,    28,     0,     0,     0,
       0,     3,    61,     0,    46,     0,    47,    54,     0,    53,
      65,     0,    62,    63,    66,    49,     0,    48,    52,     0,
       0,    45,   118,   120,   121,   122,   124,   125,   127,   128,
     132,   133,   130,   131,   135,   136,   138,   140,   142,   144,
     146,     0,     0,   403,     0,     0,    29,     3,   688,   152,
     452,     0,     0,     0,   920,   921,   917,   918,   729,   728,
       2,   840,   842,   844,     2,   860,   862,   452,   452,   936,
     935,     2,     0,     0,     0,     0,     0,   792,   944,   892,
     909,     2,   887,   895,   668,   890,   891,   669,     2,   902,
     912,   905,   906,     0,     3,  1027,   422,     2,  1020,     2,
     659,   660,   638,     3,     3,     3,     3,   682,     0,   150,
       0,     3,     3,     0,   691,     0,   685,     0,   792,     0,
     452,     3,   426,   428,     0,   792,   824,   828,   792,   881,
     886,     2,   816,   819,   821,     2,   873,   876,   878,   790,
       0,   932,     3,   782,     3,   523,   522,   525,   524,     2,
     755,   783,     2,   781,     0,   755,   784,   589,   589,   589,
     452,     0,     0,   673,     0,   397,     0,     0,   452,     0,
       2,     0,     0,     0,     0,     0,   179,     0,   327,   328,
       0,     0,   366,   365,     0,   154,   154,   372,   550,   556,
     193,     0,   180,     0,   204,   181,   182,   452,   198,   183,
     184,   185,   186,     0,   187,   188,   333,     0,   189,   190,
     191,     0,   192,   200,   533,   452,     0,   202,     0,   391,
       0,     0,     0,     3,     0,   762,   755,   743,   744,     0,
       3,   739,     3,     3,     0,   452,   720,   720,  1025,   991,
     996,     2,    95,   452,     3,   548,     3,   453,     3,   792,
     951,   954,   452,     3,   940,   946,     0,   792,   792,     0,
     634,   618,   688,   635,  1025,     0,     2,   796,   798,     0,
      90,   452,     0,    94,    92,   452,     0,   106,     0,     0,
       0,   110,   114,   113,   178,     0,     0,     0,   688,   103,
     171,     0,     0,    41,    42,    79,     0,    79,    79,     0,
      67,    69,    44,     0,     0,    40,     0,    43,   149,     0,
       0,     0,     0,  1025,     3,   792,   927,   930,   922,   452,
     452,     3,     3,     0,   792,   898,   901,   792,     0,   792,
     792,   893,   910,   452,   452,  1021,     0,   661,   452,   452,
       0,     0,     0,     0,   411,     3,     0,     0,     0,     0,
     687,   692,     3,   775,   176,   175,     3,     0,     0,     2,
     817,   820,   822,     2,   874,   877,   879,   452,   452,   682,
     792,     0,     0,     0,   755,   785,     0,   452,   452,   452,
     452,   452,   452,   572,   600,     3,     3,   601,   533,   590,
       0,     0,   832,     2,     0,   395,    61,     0,     0,   318,
     319,   201,   203,     0,     0,     0,   452,   452,   314,     0,
     312,     0,     0,     0,   688,     0,     0,     0,     0,     0,
     155,     0,     0,   373,     0,     0,     3,   208,     0,   199,
       0,   309,     0,     0,     2,     0,   533,   792,     0,   392,
     938,   937,     0,     2,     0,   746,     2,   741,     0,   742,
       0,   724,   705,   709,   707,     0,   452,     0,     0,     0,
       3,     0,     2,   947,   949,   950,     0,     0,    95,     0,
       3,  1025,   624,     0,   634,   632,  1025,     0,   621,   737,
     452,   801,     0,     0,     0,    34,     0,   107,   109,   108,
     105,   104,   688,  1025,     0,    60,    76,     0,    70,    77,
      78,    55,     0,     0,     0,    64,    51,     0,   148,   402,
      30,     0,     0,     2,   923,   925,   926,     3,     3,     0,
       0,   792,     2,   894,   896,   897,     2,   911,   913,     0,
     888,   903,     3,     3,  1022,     3,   646,   645,   649,  1024,
       2,     2,  1023,     0,     3,   789,   698,   699,     0,     0,
     792,   433,   452,   452,     3,     3,   439,   791,     0,   882,
     766,     0,   768,   572,   572,   572,   607,   577,     0,   613,
     601,     0,   452,   564,   599,   595,     0,     0,     0,     0,
     602,   604,   792,   615,   615,     0,   596,   611,   452,   398,
       0,     0,    62,   322,   323,   320,   321,     0,     0,     2,
     219,     0,     0,   221,   406,   220,   533,   452,   300,   299,
     301,     0,     2,   179,   259,     0,   252,     0,   179,   315,
     313,     0,   307,  1025,   316,     0,     0,     0,   354,   355,
     356,   357,     0,   347,     0,   348,   324,     0,   325,     0,
       0,   452,   210,   197,   311,   310,     0,   345,   364,     0,
     396,   792,   452,   764,   725,   452,     2,     2,   622,   999,
    1000,  1001,     0,   952,   452,     3,     3,     0,   960,     0,
       0,     0,     0,   633,     0,   620,     3,    93,     0,    31,
     452,     0,  1025,     0,     0,    80,     0,    68,     0,    74,
       0,    72,    39,   153,   928,   452,     0,     0,   833,   851,
     452,   452,     0,     0,     0,   452,   452,   701,     0,   419,
     421,     3,     3,     0,     0,     0,   770,   568,   570,   566,
       0,   967,     0,   608,   972,   610,   964,   792,   792,   594,
     614,   598,     0,   597,     0,     0,     0,   617,     0,   792,
     591,   605,   616,   606,   612,   653,   657,   656,     0,     2,
       0,     0,   240,     2,   222,   533,   305,   303,   306,   302,
       0,   304,     0,   248,     0,   179,     0,     2,   452,   260,
       0,   285,     0,     0,   308,     0,     0,     2,   331,   358,
       0,   349,     2,     0,     0,     0,     0,   336,     0,   332,
     195,   194,   420,   740,     0,     0,  1002,     3,     0,     0,
     959,   961,   623,     0,  1025,   636,     2,    37,    35,    36,
       0,    58,   172,    71,     0,     0,     3,   834,   852,     3,
       3,   899,   914,   423,     2,   643,     3,   642,   700,     0,
       0,   825,   883,   933,     0,     0,     0,   968,   969,   792,
     593,   965,   966,   592,   573,     0,     0,   209,   330,     0,
       0,     0,   233,     2,   211,     0,     0,     2,   242,   257,
     268,   262,     2,   179,   297,     0,   272,     0,     0,   263,
     261,   250,   253,     0,     0,   179,   286,     0,     0,   214,
     329,     2,   452,   326,     0,     0,   374,     2,   334,     0,
      61,     0,   346,   745,   747,     0,   962,   963,  1025,     0,
     738,    59,    75,    73,     0,     0,     0,   452,     0,   826,
     884,   792,   975,   977,   970,     0,   603,   228,   223,   226,
       0,   225,   232,   231,     0,   452,   235,   234,     2,   244,
       0,   241,     2,     0,     0,     0,   249,   254,     0,     0,
     179,   298,   273,     0,     0,     2,     0,   288,   289,   287,
     256,   317,     0,   452,   452,     3,   359,   453,   363,     0,
     367,     0,     0,     0,   375,   376,   217,   337,     0,     2,
       0,     2,     2,   953,     0,   626,   929,   900,   915,   647,
       2,   971,   973,   974,   609,     0,   230,     0,   229,   213,
     236,   452,   387,   245,     2,   246,   243,   258,   271,   269,
     265,   277,   275,   276,   274,   255,   270,   266,   267,   264,
     251,     0,     0,     0,     0,   216,   236,     3,   352,     0,
     967,   360,   361,   362,   374,     0,     0,     0,   374,     0,
       2,   335,   342,     0,   339,   341,   625,   452,   224,   227,
       2,     3,   237,   388,   247,     0,     0,     0,     0,   296,
     294,   291,   295,   292,   293,   290,     3,   352,     0,     0,
     968,     0,     0,     0,   368,     0,   377,   218,     0,   332,
       0,     3,   205,     0,     0,     2,   284,   282,   279,   283,
     280,   281,   278,     0,     0,   353,     0,   380,     0,   378,
       0,   380,   338,   340,     2,     0,     0,   207,   206,   212,
       0,   215,     0,   350,   381,     0,     0,   369,     0,   343,
       2,   976,   351,     0,     0,     0,     0,   344,   382,   383,
       0,   379,   370,     0,     0,   371,   384
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1704,  6640,  5565, -1704,    -1,   289,  1417,  -165, -1704,  1657,
   -1704,   398, -1704,  -670,   686,   781,  -945,  -832, -1704,   304,
    6455,  1924, -1704,    58, -1704,  1364,   493,   852,   853,   556,
     854,  1325,  1326,  1324,  1329,  1330, -1704,  -171,  -107,  8167,
     905, -1704,  1635, -1704, -1704,  -661,  3706, -1101,  2816, -1704,
     194, -1704,   901,    26, -1704, -1704, -1704,   461,   116, -1704,
   -1613, -1495,   328,    92, -1704, -1704, -1704,   337, -1463, -1704,
   -1235, -1704, -1704, -1704, -1704,    36, -1703,   219, -1704, -1704,
      42, -1704, -1704, -1704,    55,   482,   483,   164, -1704, -1704,
   -1704, -1704,  -819, -1704,    95,    28, -1704,   174, -1704,  -121,
   -1704, -1704, -1704,   911,  -693,  -956, -1264, -1704,     3, -1292,
      62,  3199,  -813,  -702, -1704,  -273, -1704,    53,  -140,   255,
    -270,  -228,  3709,  2171,  -611, -1704,     0,   182,   623,   308,
   -1704,  2031, -1704,    78,  3888,  -226, -1704, -1704,   126, -1704,
   -1704,  2279,   137,  4568,  2803,   -38,  1830,  -138, -1704, -1704,
   -1704, -1704, -1704,  -155,  5085,  5372, -1704,  -360,   267, -1704,
     562,   287, -1704,   221,   752, -1704,   554,    38, -1704, -1704,
   -1704,  5625,  -550, -1161,  -707,  -460,    33,  1506, -1704, -1258,
    -153,     8,  1276,   932,  4233,  -116,  -387,  -242,  -186,  -438,
    1299, -1704,  1621,   529,  1210,  1521, -1704, -1704, -1704, -1704,
     390,  -168,   -94,  -850, -1704,   381, -1704, -1704,   667,   489,
   -1704, -1704, -1704,  2098,  -724,  -334,  -948,   -15, -1704, -1704,
   -1704, -1704, -1704, -1704,   299,  -777,  -146, -1615,  -204,  7144,
     -65,  7008, -1704,  1177, -1704,  1605,  -189,  -216,  -185,  -181,
       1,   -54,   -47,   -46,   685,   -34,    -5,     7,  -179,   -62,
    -178,  -177,  -172,  -691,  -690,  -682,  -671,  -647,  -127,  -633,
   -1704, -1704,  -635,  1365,  1366,  1372,  1946,  7358,  -563,  -558,
    -553,  -547,  -658, -1704, -1570, -1618, -1608, -1603,  -584,  -147,
    -251, -1704, -1704,   -71,    67,   -93, -1704,  8045,    31,  -441,
    -503
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,  1141,   214,   384,   385,    81,    82,   386,   361,   387,
    1434,  1435,   388,   961,   962,   963,  1249,  1250,  1251,  1446,
     410,   390,   391,   392,   671,   672,   393,   394,   395,   396,
     397,   398,   399,   400,   401,   402,   403,   412,  1060,   673,
    1371,   734,   208,   736,   406,   801,  1142,  1143,  1144,  1145,
    1146,  1147,  1148,  2023,  1149,  1150,  1376,  1551,  1869,  1870,
    1802,  1803,  1804,  1991,  1992,  1151,  1565,  1566,  1567,  1711,
    1712,  1152,  1153,  1154,  1155,  1156,  1157,  1384,  1738,  1922,
    1842,  1158,  1159,  1583,  2009,  1584,  1585,  1905,  1160,  1161,
    1162,  1374,  1913,  1914,  1915,  2055,  2070,  1940,  1941,   285,
     286,   862,   863,  1114,    84,    85,    86,    87,    88,    89,
     443,    91,    92,    93,    94,    95,   222,   560,   445,   414,
     446,    98,   295,   100,   101,   102,   326,   327,   105,   106,
     167,   107,   881,   328,   153,   110,   242,   111,   154,   251,
     330,   331,   332,   155,   407,   116,   117,   334,   118,   551,
     851,   849,   850,  1523,   335,   336,   121,   122,  1110,  1339,
    1529,  1530,  1672,  1673,  1340,  1518,  1691,  1531,   123,   635,
    1621,   337,   633,   916,  1053,   451,   452,   855,   856,   453,
     454,   857,   339,   555,  1166,   416,   417,   209,   471,   472,
     473,   474,   475,   314,  1186,   315,   879,   877,   585,   316,
     355,   317,   318,   418,   125,   173,   174,   126,  1180,  1181,
    1182,  1183,     2,  1099,  1100,   577,  1175,   127,   305,   306,
     253,   263,   534,   128,   212,   129,   223,  1062,   842,   501,
     165,   130,   646,   647,   648,   131,   225,   226,   227,   228,
     300,   133,   134,   135,   136,   137,   138,   139,   231,   301,
     233,   234,   235,   769,   770,   771,   772,   773,   236,   775,
     776,   777,   739,   740,   741,   742,   502,   140,   610,   611,
     612,   613,   614,   615,  1675,  1676,  1677,  1678,   600,   456,
     342,   343,   344,   419,   200,   142,   143,   144,   346,   793,
     616
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      80,   103,   132,    80,   487,   298,   356,   149,   404,   182,
     338,   559,   199,   360,   518,   531,   341,   676,   950,   499,
     184,  1254,   477,   324,   970,  1365,  1184,   185,   186,   790,
    1486,  1487,   495,   905,   232,   488,  1841,   352,   191,   489,
     187,   490,   491,   492,   177,   899,   891,   290,   493,  1425,
    1261,   892,  1784,   595,    96,    80,    80,   893,    80,   103,
     132,   943,  1785,    90,  1554,  1554,   150,  1786,   141,   188,
     626,   141,   405,    80,   629,  1023,  1029,   199,  1056,   108,
     487,   189,    80,  1024,   197,  1167,   918,   566,   568,   628,
      80,   515,  1553,   631,  1025,    80,  1071,   229,    80,   438,
     254,  1788,    80,  1005,   264,  1723,   257,   900,   495,   507,
    1033,   488,    96,  1295,   202,   489,  1040,   490,   491,   492,
    1030,    90,  1105,    58,   493,  1878,   141,   112,   422,   835,
     837,   293,  1026,  1936,   529,   423,   424,   108,   113,  1944,
      80,  -748,   661,    80,   539,    80,   103,   132,   425,  1176,
      80,    58,  1323,   359,  1334,   485,    80,  1326,  1589,   496,
      58,   247,  1173,    80,  1872,   258,   184,    58,   498,  1163,
     141,   595,   716,   185,   186,   524,   601,   426,   210,   202,
      80,    80,   197,   104,  1871,   112,   187,   618,    58,   427,
    1877,  1911,   914,   839,  1587,    80,   113,   459,   279,    96,
     428,   455,   279,   846,   899,   468,   500,   891,    90,   567,
      80,   420,   892,   141,   717,   188,  -389,  1864,   893,    80,
      80,   567,   197,   589,   108,  1590,  1879,   189,  1394,   261,
     508,   571,   524,   607,   500,   496,    80,   546,   661,   574,
    1945,   104,   184,   500,   873,    80,   163,   197,  -774,   185,
     186,   920,  1818,   275,  1219,    80,    97,   826,    80,   151,
     535,   677,   589,  1623,   145,    80,  1034,  1347,  1348,   808,
    1037,   500,   112,  1588,   915,    80,    80,   525,    80,  1050,
    1051,   249,  1242,   113,   562,  1257,  -389,   794,  1937,  1938,
    -749,   866,  1253,   147,   601,    80,    80,  1335,   197,  1554,
     809,  1799,  1800,    80,   810,   822,   811,   812,   813,   618,
      80,    80,   156,   814,    97,    80,  1841,   210,   774,  -390,
    1873,   532,  1871,   884,  1054,  1054,   281,  1553,   104,  1716,
    1233,  1023,   968,  1188,   525,  1205,  1972,  1281,   195,  1024,
     199,  1352,   886,  1054,   761,   171,   171,   904,    80,  1063,
    1025,  1784,   157,    80,   603,   808,    80,   645,  1009,  1453,
     910,  1785,   108,  1306,  1033,   280,  1786,  1214,   833,  1268,
    1167,  1334,  1334,  1334,   838,    20,  1511,  1877,  1324,   911,
     171,  1282,  1349,  1801,   822,   211,   809,  -774,  1273,  -390,
     810,  1454,   811,   812,   813,   288,   162,  1486,  1487,   814,
    1788,    97,   176,   195,    58,  1344,   480,   250,  1336,  1054,
     112,    58,  1877,   158,  1448,   845,   159,   160,   270,   161,
      80,   113,   459,   655,  1345,  1351,  1554,   191,   341,    58,
     171,   422,   823,   171,   178,   324,  1799,  1800,   423,   424,
    1864,   533,   202,    80,    80,  1990,   171,   436,   696,   697,
      58,   425,   179,   350,  1163,    80,    80,  1296,   618,  1535,
     250,   180,  1714,   891,    80,  1205,   468,  1722,   892,   696,
     601,  1990,   603,    58,   893,    63,    64,  1215,  1536,   563,
     426,   180,   618,   752,    80,  1823,  1824,   500,  -676,   618,
    1010,  1297,   427,    80,   500,   459,    58,  2025,   520,   696,
     171,   523,  1636,  1344,   250,    58,  1346,   249,  1031,  1102,
    1935,   823,   605,    80,  1335,  1335,  1335,   422,  1828,    80,
    1237,  2044,  1600,    76,   423,   424,   280,  1238,   506,  1038,
     455,   511,   192,   605,    58,   926,  1131,   928,   929,    97,
     930,  1414,  1287,  1543,  1297,   171,   932,   420,   420,   934,
     935,   936,  1081,   528,   994,   171,   500,    80,   523,    80,
    1263,   180,   872,   538,   562,   549,   171,   250,   554,  -908,
      80,   675,    80,  1680,   459,  1085,    80,   103,   132,   500,
    1888,  1889,    58,  1554,  1309,  1535,    80,   860,   500,   280,
      80,    58,  1681,   171,   888,  1457,    58,   250,    58,  1929,
     171,   171,   455,   250,  1683,   171,  1064,   404,   871,   905,
    1054,  1554,  1689,  1313,  1817,  1189,   203,   500,  1059,  1478,
    1637,  1639,  1641,    80,   280,  1336,  1336,  1336,    58,   249,
      96,  1690,   586,   250,  1190,    80,   587,   180,   171,    90,
     774,  1545,   108,   171,   141,  -569,   171,   540,   906,  1554,
      14,    15,    16,    17,    18,   108,  1421,   945,   552,  1073,
    1090,  1412,   205,  1552,  1568,   605,   195,  1885,   582,   420,
    1463,  1044,  1684,   206,   500,  1472,  1541,  1476,  1089,   500,
    1919,   605,   546,    80,   428,    80,   500,    80,   261,   207,
     112,    80,   760,  1764,    80,  1765,  1383,   583,   584,   945,
     829,   113,  1789,   112,  1689,   832,  1576,  1930,    58,   706,
     707,   500,  1887,  1920,   113,   945,   618,    -3,  1724,    80,
     945,  1790,   840,  1793,  1900,  1894,   217,   267,  1445,   455,
    1962,   268,   847,   171,   271,  1253,   273,   237,    14,    15,
      16,    17,    18,   249,   404,   171,   171,  1405,   945,   618,
     945,   945,   945,   708,   709,   250,   972,  -446,   683,   104,
    1964,  1996,   888,   684,    80,   945,    80,  1797,   198,  -450,
     455,  1883,   699,  1427,  1426,  1998,   420,   275,    80,   700,
     701,   230,  1208,  1634,   255,    80,   190,    64,   265,  1955,
     341,   468,   455,   455,    80,   718,    58,   324,  1402,   719,
    1443,    58,   533,    80,    80,    80,   830,   277,  1213,   238,
     239,   455,   240,   151,   279,  1294,   241,  1055,  1055,    97,
    1258,   280,  1461,    80,  1706,  1707,  1708,   158,   841,   250,
     159,   160,    97,   161,   844,  -389,  1055,  -763,   848,    14,
      15,    16,    17,    18,   744,   675,  1709,   147,   745,   250,
     675,   354,   938,    58,   756,  1710,   675,  -451,   500,    80,
      80,   468,  1165,   939,   940,   685,   198,   859,  1717,   250,
     686,   860,   267,  1718,   312,   675,   171,   455,  1177,  1301,
     882,   460,    80,   637,    62,  1319,   639,   218,   219,    65,
      66,    67,    68,    69,    70,    71,  1059,    58,  1552,   919,
    1280,   774,  1055,   587,   250,  1835,   198,   420,    80,   294,
    1836,   357,    80,   908,    73,    96,    80,  1706,  1707,  1708,
     545,    64,   645,   904,    90,   171,  1573,   358,   250,   141,
     429,   198,  1278,    75,   604,   250,   458,  1178,   605,  1709,
     108,   430,   141,  1977,   536,    78,   606,  2040,  1978,   359,
     267,   268,  2041,   622,   431,   273,   640,    80,   607,    14,
      15,    16,    17,    18,   432,    80,   590,   275,   250,   270,
     871,  1517,   921,   954,  1416,   956,   587,   959,   795,   796,
    1620,   967,   797,   922,   971,  1624,   944,   923,   112,  1014,
     945,  1227,   433,   500,    80,   434,  1231,   468,   462,   113,
     702,   703,  1633,   463,  1632,    62,   476,  1239,   478,   996,
      65,    66,    67,    68,    69,    70,    71,    58,   356,   356,
      80,   481,    19,   482,  1093,  1568,    80,    80,  1068,   483,
     641,   484,  1069,  1395,   497,  1101,   249,   279,  1103,  1341,
     516,   500,  1106,   517,   104,   642,   601,   533,   643,   644,
      65,    66,    67,    68,    69,    70,    71,    80,   498,   618,
      48,    49,    50,    51,    52,    53,    54,    55,   171,   249,
     710,   711,  1754,   527,   341,   171,    73,  1506,   455,   210,
     279,   324,   428,  1555,   500,   508,   537,   818,  1458,   500,
     574,   578,   428,  1072,   500,  1074,   604,   625,   508,   556,
     605,  -447,   500,  1055,   704,   705,   460,    78,   606,  1436,
     593,    14,    15,    16,    17,    18,  1095,    97,  1165,   468,
     945,  -907,    80,    80,    80,   404,   404,   192,   679,  -448,
    1179,  1031,  1725,   428,  1097,   605,  1488,  -619,   945,    14,
      15,    16,    17,    18,  1104,   636,   468,  1165,  1533,  1113,
     171,   171,    80,   574,  1252,  2011,   267,   500,  1253,  2015,
      80,   649,   250,    80,    80,   254,   264,    80,   257,    58,
    1918,    96,   171,   250,   593,   679,   655,  1401,    80,   460,
      90,   745,  1942,  1431,   638,   141,   650,  1253,    73,  1494,
    1495,  1760,   973,   974,   975,   250,   108,    58,  1207,   653,
      96,   654,   171,  1848,   658,    80,   171,   698,   737,    90,
    1942,   468,   500,  1629,   141,    73,   420,  1630,    73,    78,
      79,    80,  1046,  1047,   906,   108,   247,   258,   679,  1700,
    1726,   712,   141,   945,   945,   604,  1727,   468,  1670,   605,
    1069,   713,   500,   714,   112,    80,    78,    79,  1993,    78,
      79,   696,   947,   948,   871,   113,  1341,  1341,  1341,   341,
    1519,  1341,   980,   981,   982,   983,   324,    14,    15,    16,
      17,    18,  1728,   112,   715,  1534,   945,    80,  -116,  -116,
    -116,  -116,  -116,  -116,   113,  1794,  1437,  1438,  1439,   745,
     720,   261,   746,  1440,  1441,  1880,   455,   455,  1325,   945,
     104,   244,     6,     7,     8,     9,    10,    11,    12,    13,
     487,  1350,  1981,  1849,   747,  2042,  1253,   748,  1555,   945,
     749,  1485,  1706,  1707,  1708,    58,  2066,   750,  1369,   104,
    2063,  1533,   751,   149,   945,   435,    80,    -3,  2073,   495,
      80,   488,  2074,    80,  1709,   489,   249,   490,   491,   492,
    1706,  1707,  1708,  1715,   493,   778,  1557,  1557,  1667,  1668,
    1669,   266,   945,   468,  -449,  1337,  -115,  -115,  -115,  -115,
    -115,  -115,  1709,    97,  1327,  1328,  1329,   -17,   632,   533,
     792,  -180,   791,   468,    73,    80,   802,   535,  1048,  1049,
    1240,  1069,   150,  1255,  1256,   945,  1259,   141,  -151,  -151,
    1048,  1393,    97,   815,   737,  1451,  1452,  1924,   500,   171,
    1456,  1452,   171,   171,   171,    78,    79,   825,    90,    90,
     816,  1460,  1452,   141,   141,  1735,    14,    15,    16,    17,
      18,  1839,  1840,  1177,   108,   108,   171,  1020,  1444,   871,
     827,   468,   171,  1496,  1444,   817,    80,   819,   532,  1488,
     554,    80,    80,    80,   820,   341,   843,   171,  1534,  1020,
    1508,  1685,   324,  1642,  1069,   821,   496,  1762,  1069,   808,
      62,   287,   250,   169,   170,    65,    66,    67,    68,    69,
      70,    71,   112,   112,    58,    14,    15,    16,    17,    18,
     942,  -567,  1178,   113,   113,   171,  -565,   141,   852,   822,
     809,  1763,  1452,  1488,   810,   250,   811,   812,   813,  1773,
    1774,  1432,   861,   814,  1436,  1783,   945,  1852,  1452,    80,
    1578,  1579,  1580,  1581,  1582,    80,   874,    80,    14,    15,
      16,    17,    18,  1232,    80,  1853,  1452,   876,   104,   104,
     880,  1544,  1546,    73,  1799,  1800,  1696,   883,   468,  2063,
    2064,  1449,  1450,   420,   976,   977,   894,   978,   979,   896,
     257,   468,  1906,  1670,   984,   985,   607,   500,   533,  1834,
     913,  1692,  1692,   915,    78,    79,  1403,  1404,   917,  1598,
     924,   925,  1337,  1337,  1337,   151,  1516,  1520,   946,   949,
     952,  1557,   998,   993,  1019,    62,  1020,  1027,   468,  1844,
      65,    66,    67,    68,    69,    70,    71,  1533,  1177,  1066,
    1075,    97,    97,  1076,  1077,  -752,  1078,  1079,   247,   258,
    1168,    80,  1080,   469,   141,  1096,   823,  1098,   404,  1107,
     171,  -652,  1108,   171,  1906,  1109,  1199,    80,  1200,    80,
    1201,  1211,  1169,  1185,    75,  1216,   250,   787,  1217,  1220,
    1222,  1223,  1224,    90,  1737,  1226,  1228,  1225,   141,  1229,
    1230,  1235,  1236,   455,   455,  1300,  1243,  1178,  1244,   108,
    1260,  1912,   141,   171,  1265,  1266,  1267,  1274,  1275,   183,
    1276,  1277,  1285,   261,    80,  1179,  -640,    80,  1776,  -639,
    1320,  1308,  1868,  -753,   250,  1342,  1343,  1353,   468,  1356,
    1357,   224,   468,    62,  1488,  1366,   169,   170,    65,    66,
      67,    68,    69,    70,    71,  1367,   468,   112,  1557,  1368,
    1373,  -675,  1375,   945,  1383,  1387,   468,  1377,   113,  1389,
    1390,  1391,  1397,  1399,  1534,  1428,  1429,  1459,   249,  1442,
    1444,  1471,  1510,    80,    80,  1484,  1489,  1490,  1491,  1492,
    1500,  1452,    80,   487,  1497,  1509,   299,  1512,  1524,  1522,
    1971,  1908,  1362,  1525,   404,  1346,   404,   532,  1548,   618,
    1591,  1569,  1593,   104,  1596,  1570,  1729,  1572,  1601,  1574,
      90,   495,  1586,  1606,   488,   141,  1594,  1595,   489,  1603,
     490,   491,   492,  1604,  1607,    80,   108,   493,  1609,  1608,
    1610,  1611,   468,  1613,  1625,   404,   468,  1618,  1627,  1912,
    1628,   468,  1622,  1912,  1912,  1631,  1635,  1643,  1644,  1648,
    1657,   822,  1649,   428,   171,   486,   224,  1496,  1988,   858,
    1868,  1659,  1666,  1908,  1679,  1527,   468,  2035,   171,  1699,
    1253,  1701,   299,  1739,   112,  1703,    97,  2038,   211,   255,
     265,   171,  1732,  1734,  1746,   113,  1750,  1752,  1753,  1209,
    1179,  1755,  1751,  1761,   250,   455,  1778,  1767,  1768,  2013,
    1771,  1772,  2054,  1781,  1782,  1557,  2054,   468,  1807,  1812,
    1813,   468,   788,  1827,   469,   404,  1831,   533,   171,  1837,
    1838,  1131,  1825,   500,   468,  1833,  1846,  1917,  2068,  1847,
     104,   572,   299,  1557,   571,    80,  1850,    80,   496,  1851,
    -641,   171,  1882,  1859,  1860,   184,  1861,  1862,   468,  1863,
     468,   468,   185,   186,    62,    83,  -550,  1890,   148,    65,
      66,    67,    68,    69,    70,    71,   957,    90,  1884,  1895,
    1893,  1557,   141,   468,  1901,  1909,  1910,  1923,   823,  2065,
    1925,    62,  1926,   108,   169,   170,    65,    66,    67,    68,
      69,    70,    71,  1927,  1928,    90,  1774,  1939,    80,    80,
     141,   197,  1948,    97,  1961,  1963,   958,  1965,  1975,   468,
    1974,   108,  1976,    83,  1980,  1979,  1983,  2019,   171,   468,
    1986,  1995,   171,  1997,  1999,  2008,  2020,  2012,   181,  2014,
    2036,   112,  2026,    90,  2037,  2039,   171,    83,   141,    80,
    2049,   459,   113,   250,  2052,  2051,   171,  2056,  2060,   108,
     221,  2057,   468,   246,   468,  2061,  2071,    83,  1758,   112,
    2072,  2075,  1542,   171,  1455,   941,   682,   986,   988,   987,
     113,  1372,   171,   468,   989,   735,   990,   237,  1379,   468,
     768,  2050,  1736,  1989,  1829,  1822,  2045,   104,  2006,   468,
    1921,  2043,  2034,    80,   148,  1730,  1731,   112,  1967,  2058,
      83,   536,   148,    80,  2016,   297,   303,  1966,   113,  1388,
     168,   526,  1682,  1866,  1521,   104,  1934,   323,  1693,  1187,
     807,  1065,   171,  1385,  1743,   798,   171,  1626,     3,   224,
    1218,   171,  1001,  1002,   411,   181,   181,   858,   878,  1003,
       0,     0,     0,     0,     0,     0,   148,   441,     0,   299,
     246,     0,     0,   104,     0,   299,   171,     0,     0,     0,
      97,     0,   244,     6,     7,     8,     9,    10,    11,    12,
      13,     0,     0,     0,   221,   221,    62,     0,    19,   218,
     219,    65,    66,    67,    68,    69,    70,    71,    97,     0,
       0,   297,     0,     0,     0,   299,   858,   171,     0,     0,
      83,   171,     0,     0,    73,     0,   870,   510,   299,     0,
       0,     0,     0,   246,   171,     0,     0,     0,     0,     0,
      52,    53,    54,    55,  1526,    75,    97,  1973,     0,     0,
       0,  1527,     0,     0,     0,    78,    79,     0,   171,   469,
     171,   171,   788,   303,     0,   250,     0,     0,     0,   303,
     297,   297,     0,     0,     0,     0,  1380,   148,     0,     0,
    2053,     0,    62,   171,     0,     0,     0,    65,    66,    67,
      68,    69,    70,    71,   965,     0,  2062,   323,   608,   617,
       0,    62,     0,     0,   169,   170,    65,    66,    67,    68,
      69,    70,    71,     0,   323,     0,     0,     0,   323,   171,
     248,     0,     0,     0,     0,     0,   858,     0,     0,   171,
    -437,   269,     0,   272,   966,   274,     0,     0,     0,     0,
       0,     0,     0,   858,   858,     0,     0,     0,     0,    62,
       0,   411,     0,  -437,    65,    66,    67,    68,    69,    70,
      71,     0,   171,     0,   171,     0,     0,     0,     0,     0,
       0,     0,     0,   248,  1381,   272,   274,    73,     0,     0,
       0,     0,     0,   171,     0,   411,     0,  1094,   738,   171,
       0,     0,     0,     0,     0,   181,     0,  1021,    75,   171,
       0,   605,     0,  2069,     0,     0,     0,     0,    78,    79,
     743,   148,     0,  2076,     0,   441,     0,   248,     0,   767,
    1022,   617,   768,     0,     0,  1248,   754,     0,     0,   757,
       0,     0,    62,  1248,     0,   169,   170,    65,    66,    67,
      68,    69,    70,    71,    62,     0,  1198,   169,   170,    65,
      66,    67,    68,    69,    70,    71,     0,     0,     0,   221,
     299,     0,  1248,     0,     0,   469,     0,    62,   221,     0,
     347,   348,    65,    66,    67,    68,    69,    70,    71,   299,
     248,     0,   272,   274,     0,     0,   510,     0,   297,     0,
     411,   411,     0,     0,   297,     0,   323,     0,     0,     0,
       0,     0,     0,     0,     0,  1358,     0,     0,     0,     0,
     248,     0,     0,     0,     0,     0,   248,  1360,    76,     0,
       0,     0,   449,   349,     0,  1248,     0,     0,     0,     0,
       0,     0,    62,     0,   297,   169,   170,    65,    66,    67,
      68,    69,    70,    71,     0,   297,   248,   297,     0,   323,
       0,    83,   623,     0,   274,    62,  1264,     0,   218,   219,
      65,    66,    67,    68,    69,    70,    71,   323,   441,     0,
     617,     0,     0,  1271,  1272,     0,     0,     0,   608,     0,
       0,     0,   608,     0,     0,     0,     0,     0,   580,     0,
       0,   323,     0,     0,     0,   858,   858,     0,     0,     0,
       0,   617,     0,     0,   323,     0,     0,     0,     0,   858,
     858,  1206,   148,     0,     0,     0,   307,   308,   309,   310,
       0,     0,     0,     0,     0,   411,     0,   148,   148,     0,
     411,     0,     0,     0,     0,     0,   411,     0,     0,   148,
     148,   148,   248,   858,   858,     0,     0,     0,     0,     0,
       0,     0,   598,     0,    62,   621,     0,   169,   170,    65,
      66,    67,    68,    69,    70,    71,     0,     0,   248,   598,
     623,   274,     0,   598,     0,     0,  1022,     0,     0,   469,
      62,     0,  1279,   768,     0,    65,    66,    67,    68,    69,
      70,    71,  1245,     0,     0,   441,  1246,     0,  1247,     0,
       0,     0,     0,     0,     0,   469,   311,     0,     0,     0,
       0,   738,   738,  1248,     0,   248,     0,     0,     0,   411,
       0,     0,     0,     0,   312,     0,     0,     0,     0,    75,
       0,     0,  1447,   743,   743,   248,   441,     0,     0,   767,
     248,   767,   248,  1012,    62,     0,  1015,   545,    64,    65,
      66,    67,    68,    69,    70,    71,     0,     0,   323,   323,
       0,     0,   248,     0,   248,   248,     0,     0,     0,     0,
     598,     0,     0,     0,     0,     0,     0,   323,     0,   297,
       0,    62,   248,     0,     0,     0,    65,    66,    67,    68,
      69,    70,    71,  1245,   248,     0,   995,  1246,   297,  1247,
      14,    15,    16,    17,    18,     0,     0,   510,     0,     0,
       0,  1083,     0,     0,     0,  1087,     0,   248,     0,   623,
     274,     0,     0,     0,     0,  1467,  1468,     0,   858,   858,
      75,   469,     0,  1638,   299,     0,   411,     0,     0,  1482,
    1483,   248,   623,   323,     0,     0,     0,     0,   248,   148,
     411,   449,     0,     0,   115,     0,     0,   115,    58,   323,
       0,  1193,     0,     0,  1697,     0,     0,     0,     0,     0,
       0,     0,   608,  1504,  1505,     0,     0,     0,     0,     0,
       0,   248,   269,     0,     0,     0,     0,     0,     0,    62,
       0,     0,   218,   219,    65,    66,    67,    68,    69,    70,
      71,     0,     0,     0,   449,     0,     0,     0,     0,   469,
       0,   441,   115,     0,  1248,     0,     0,    73,     0,  1248,
    1248,  1248,   598,   449,    14,    15,    16,    17,    18,     0,
       0,     0,     0,  1479,     0,     0,   115,   765,    75,     0,
     858,   605,     0,     0,     0,     0,   598,     0,    78,   766,
       0,     0,   252,     0,     0,     0,   115,    62,     0,   598,
       0,     0,    65,    66,    67,    68,    69,    70,    71,  1245,
       0,   858,     0,  1246,     0,  1247,   858,   858,   738,     0,
      62,     0,    58,   547,   548,    65,    66,    67,    68,    69,
      70,    71,  1532,   115,     0,   767,     0,     0,    58,   115,
     743,   115,   767,     0,     0,   252,    75,     0,     0,  1640,
       0,     0,     0,    62,     0,   319,   115,   351,    65,    66,
      67,    68,    69,    70,    71,     0,     0,     0,     0,    62,
       0,    76,     0,   415,    65,    66,    67,    68,    69,    70,
      71,    73,     0,     0,   323,   115,   415,     0,     0,   252,
     449,     0,     0,     0,     0,     0,     0,    73,  1661,  1662,
       0,    74,    75,   687,     0,   688,   689,   690,     0,     0,
       0,  1311,    78,    79,  1315,   248,     0,    74,    75,     0,
       0,     0,     0,     0,   148,     0,   248,     0,    78,    79,
       0,   449,   411,     0,   691,     0,   115,   692,   693,   115,
       0,     0,   694,   695,     0,  1248,     0,  1248,   248,     0,
       0,     0,   252,     0,     0,     0,     0,     0,     0,   248,
       0,   411,     0,     0,     0,    62,     0,     0,   248,   550,
      65,    66,    67,    68,    69,    70,    71,   115,   246,    83,
       0,     0,   252,     0,     0,     0,     0,     0,   252,     0,
       0,     0,     0,   297,     0,     0,   115,     0,     0,   148,
       0,     0,     0,     0,     0,     0,     0,   441,     0,     0,
    1747,     0,     0,  1278,    75,  1532,   115,     0,   252,   115,
       0,  1686,     0,  1532,   244,     6,     7,     8,     9,    10,
      11,    12,    13,   115,     0,   441,     0,   115,    58,   148,
       0,  1766,     0,     0,     0,     0,  1769,  1770,     0,     0,
       0,     0,     0,     0,   598,     0,     0,   621,     0,     0,
       0,     0,     0,     0,     0,   248,     0,     0,     0,    62,
     415,     0,   218,   219,    65,    66,    67,    68,    69,    70,
      71,     0,     0,     0,     0,     0,     0,     0,     0,   248,
       0,     0,     0,     0,     0,     0,     0,    73,     0,     0,
       0,  1465,   323,   323,   415,     0,   449,     0,     0,     0,
    1474,     0,     0,     0,     0,     0,     0,  1969,    75,     0,
       0,   500,     0,     0,     0,     0,    58,     0,    78,    79,
     115,     0,     0,     0,   415,     0,     0,     0,     0,     0,
     252,   148,   148,   148,   148,   148,   148,     0,     0,     0,
       0,  1528,   303,   858,     0,     0,     0,    62,     0,     0,
     218,   219,    65,    66,    67,    68,    69,    70,    71,     0,
     411,   411,   194,     0,     0,     0,     0,     0,     0,     0,
    1795,     0,   367,  1532,   368,    73,   369,    63,    64,    65,
      66,    67,    68,    69,    70,    71,     0,     0,     0,     0,
     246,     0,     0,     0,     0,   220,    75,     0,     0,   415,
     415,     0,     0,     0,   252,   115,    78,    79,     0,     0,
     441,     0,     0,     0,     0,   248,     0,     0,     0,     0,
       0,     0,   681,     0,     0,    76,   378,   194,     0,     0,
     299,     0,     0,     0,   148,     0,   115,     0,     0,     0,
       0,   115,   194,     0,   252,   115,     0,   115,   248,     0,
       0,     0,     0,     0,   248,     0,     0,    62,   115,   194,
     115,     0,    65,    66,    67,    68,    69,    70,    71,  1245,
       0,     0,   444,  1246,   351,  1247,   115,   415,     0,   252,
       0,  1532,     0,     0,     0,     0,    62,     0,     0,   190,
      64,    65,    66,    67,    68,    69,    70,    71,     0,     0,
     115,     0,     0,   252,     0,     0,    75,   550,     0,     0,
     252,     0,     0,   115,     0,   912,     0,     0,     0,     0,
    1671,   115,     0,     0,  1528,   194,   411,     0,    58,     0,
    1528,     0,  1528,     0,   415,    75,   115,   115,   787,   415,
       0,     0,  1674,     0,     0,   415,     0,     0,   115,   115,
     115,     0,   598,     0,     0,     0,     0,     0,     0,    62,
     303,   148,   218,   219,    65,    66,    67,    68,    69,    70,
      71,     0,     0,  2021,     0,     0,     0,     0,     0,     0,
     449,     0,   194,     0,     0,     0,     0,    73,     0,   248,
       0,   299,     0,     0,     0,   411,     0,     0,     0,     0,
       0,     0,   194,     0,   415,     0,   323,   296,    75,   148,
       0,     0,     0,     0,     0,     0,     0,     0,    78,    79,
       0,     0,     0,     0,     0,     0,     0,     0,   415,     0,
       0,     0,     0,     0,   148,     0,     0,   248,     0,     0,
       0,     0,     0,     0,     0,   415,     0,     0,     0,     0,
       0,     0,     0,     0,   572,   299,     0,     0,     0,   323,
     323,     0,     0,     0,     0,     0,     0,   115,   115,     0,
       0,     0,     0,    62,  1671,  1671,    63,    64,    65,    66,
      67,    68,    69,    70,    71,     0,   115,     0,     0,  1528,
     194,     0,  1528,     0,     0,   299,  1674,  1674,   722,   723,
     724,   725,   726,   727,   728,   729,   730,   731,   732,   303,
       0,     0,   205,     0,   115,     0,     0,     0,     0,     0,
     194,     0,   411,     0,    76,     0,     0,     0,     0,    14,
      15,    16,    17,    18,     0,     0,     0,   252,     0,   733,
       0,     0,     0,     0,     0,   415,     0,     0,   252,   297,
       0,     0,   115,     0,     0,     0,     0,     0,   115,   415,
       0,     0,     0,     0,     0,   449,     0,     0,   115,     0,
    1195,   415,    62,   115,     0,   169,   170,    65,    66,    67,
      68,    69,    70,    71,     0,     0,     0,    58,     0,     0,
      99,  1671,     0,   152,     0,   194,   194,     0,     0,     0,
    1528,   444,     0,     0,     0,     0,     0,   248,     0,     0,
       0,     0,     0,  1674,     0,     0,     0,     0,    62,     0,
     415,   218,   219,    65,    66,    67,    68,    69,    70,    71,
       0,     0,     0,     0,    62,     0,   148,   169,   170,    65,
      66,    67,    68,    69,    70,    71,    73,     0,    99,     0,
       0,     0,     0,     0,   194,     0,     0,     0,     0,     0,
       0,   323,     0,     0,     0,     0,  1969,    75,     0,  1671,
     500,     0,   196,   444,     0,     0,     0,    78,    79,   148,
       0,     0,   458,   115,     0,    58,     0,  1932,     0,     0,
       0,  1674,   259,     0,     0,     0,   194,     0,     0,     0,
     115,   115,     0,     0,     0,     0,     0,   148,   148,     0,
    1970,   303,     0,     0,     0,     0,    62,   194,     0,   218,
     219,    65,    66,    67,    68,    69,    70,    71,     0,   289,
       0,     0,  1674,     0,     0,    99,     0,     0,     0,   248,
       0,     0,     0,     0,    73,   148,     0,     0,     0,     0,
       0,     0,   325,   115,     0,     0,   248,     0,     0,     0,
       0,     0,     0,     0,  1526,    75,     0,     0,     0,   109,
     421,     0,     0,  1970,  1970,    78,    79,     0,     0,     0,
      62,   289,   447,   218,   219,    65,    66,    67,    68,    69,
      70,    71,     0,   115,     0,  1674,  1674,     0,     0,     0,
     444,   415,     0,     0,     0,     0,     0,     0,    73,     0,
     494,     0,     0,     0,  1970,     0,  1355,     0,     0,     0,
       0,     0,     0,     0,   194,     0,   514,   109,   220,    75,
     415,   519,   521,     0,     0,   196,  1674,     0,   248,    78,
      79,   444,     0,     0,     0,     0,     0,   252,   115,     0,
       0,     0,     0,     0,     0,     0,     0,   541,     0,     0,
     543,     0,   544,   444,   444,     0,     0,     0,   115,     0,
       0,   260,     0,   561,     0,     0,   415,     0,     0,     0,
    1195,     0,   444,     0,     0,     0,   573,     0,     0,     0,
      62,     0,  1424,   218,   219,    65,    66,    67,    68,    69,
      70,    71,     0,     0,   415,     0,     0,     0,   115,     0,
       0,     0,   596,     0,   109,   620,    62,     0,    73,   218,
     219,    65,    66,    67,    68,    69,    70,    71,     0,   627,
       0,   329,     0,   627,     0,     0,     0,     0,   765,    75,
       0,     0,   605,     0,    73,     0,     0,     0,   444,    78,
     766,     0,   115,   115,     0,   194,     0,   660,   248,     0,
       0,   448,   607,   674,  1969,    75,   115,   115,   500,     0,
       0,   115,   115,     0,     0,    78,    79,     0,     0,    14,
      15,    16,    17,    18,     0,     0,     0,     0,     0,     0,
       0,   598,     0,     0,     0,     0,     0,     0,     0,     0,
     115,   115,     0,     0,     0,     0,     0,     0,     0,     0,
     115,   115,   115,   115,   115,   115,   194,     0,     0,     0,
       0,   252,     0,     0,     0,     0,   289,     0,     0,     0,
     596,     0,     0,     0,     0,     0,   542,    58,     0,   415,
     415,    14,    15,    16,    17,    18,     0,     0,     0,     0,
       0,     0,   109,   660,     0,     0,     0,     0,     0,     0,
       0,   248,     0,   598,     0,     0,     0,     0,    62,   252,
       0,   218,   219,    65,    66,    67,    68,    69,    70,    71,
    1597,     0,     0,     0,     0,     0,     0,     0,     0,   415,
       0,   597,     0,     0,   260,     0,    73,     0,     0,    58,
       0,     0,   834,   836,     0,     0,     0,     0,   597,     0,
       0,   447,   597,   115,     0,    62,   220,    75,   169,   170,
      65,    66,    67,    68,    69,    70,    71,    78,    79,     0,
      62,     0,     0,   218,   219,    65,    66,    67,    68,    69,
      70,    71,   854,     0,     0,     0,     0,   521,     0,   444,
       0,   865,     0,   561,     0,     0,     0,     0,    73,     0,
       0,     0,     0,   462,   325,     0,    99,     0,     0,   172,
     175,     0,     0,     0,     0,     0,     0,     0,   296,    75,
       0,     0,   627,   887,     0,   115,   115,     0,     0,    78,
      79,     0,     0,     0,     0,     0,     0,   898,     0,     0,
       0,     0,     0,     0,   213,   415,   596,     0,     0,   597,
       0,   907,     0,     0,     0,     0,     0,     0,     0,   627,
       0,   115,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   674,     0,   252,
     115,     0,   674,     0,     0,  1702,     0,     0,   674,     0,
       0,     0,     0,     0,   291,     0,     0,   292,  1713,     0,
       0,     0,     0,     0,   194,     0,     0,   674,     0,     0,
     313,     0,   194,     0,   415,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   115,     0,     0,   115,     0,
     448,     0,     0,     0,     0,  1741,     0,   115,     0,     0,
     194,     0,     0,   992,     0,     0,     0,     0,     0,     0,
     447,     0,     0,   115,     0,     0,     0,     0,     0,     0,
       0,   329,     0,     0,   479,     0,     0,  1004,   115,     0,
     260,     0,   109,   115,   115,     0,     0,     0,   115,   115,
       0,     0,     0,   448,     0,   109,     0,     0,     0,     0,
       0,   887,     0,     0,     0,     0,  1028,     0,     0,     0,
       0,   597,   448,     0,     0,     0,     0,   444,   444,   530,
       0,     0,     0,   447,   447,     0,     0,     0,     0,   172,
       0,     0,     0,     0,     0,   597,     0,     0,   252,     0,
     172,     0,   447,     0,     0,  1798,     0,     0,   597,  1808,
       0,   415,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1821,     0,     0,     0,   576,     0,     0,
     854,     0,     0,  1830,   579,   581,     0,    62,     0,   588,
     218,   219,    65,    66,    67,    68,    69,    70,    71,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   114,
       0,  1164,     0,     0,     0,    73,     0,     0,   447,     0,
       0,     0,   634,     0,   152,     0,     0,   313,     0,     0,
     313,     0,     0,     0,   627,  1526,    75,  1197,     0,   854,
       0,     0,  1527,     0,  1203,   194,    78,    79,     0,   448,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1876,
       0,     0,     0,  1881,     0,     0,     0,   114,  1886,     0,
       0,     0,     0,    62,     0,   115,   218,   219,    65,    66,
      67,    68,    69,    70,    71,     0,   325,     0,     0,     0,
     448,     0,     0,  1916,     0,     0,     0,     0,     0,     0,
     115,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   262,   329,   329,     0,     0,     0,   213,   115,     0,
       0,   296,    75,     0,     0,     0,     0,     0,     0,   782,
     783,   329,    78,    79,  1943,     0,     0,     0,  1946,     0,
       0,     0,     0,     0,     0,     0,   115,   115,     0,   854,
     252,  1960,     0,     0,   114,     0,     0,     0,     0,   329,
       0,   194,     0,     0,     0,     0,   854,   854,     0,     0,
       0,   333,     0,     0,     0,  1982,     0,  1984,  1985,     0,
       0,     0,     0,     0,   115,     0,     0,     0,     0,     0,
     109,     0,     0,     0,     0,     0,     0,   329,     0,     0,
    1994,   450,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   597,     0,     0,   260,     0,   329,   447,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     115,     0,     0,     0,     0,     0,  2017,     0,     0,     0,
       0,   194,     0,     0,     0,     0,  2022,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1338,
     313,     0,     0,     0,     0,   448,     0,  1164,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  2048,
       0,  2022,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   114,     0,   444,   444,  1164,     0,     0,     0,
    2059,     0,     0,     0,     0,     0,  2048,     0,     0,   634,
       0,     0,     0,     0,  1386,     0,  2067,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   329,     0,
       0,   599,     0,     0,   262,     0,     0,     0,     0,     0,
       0,     0,   596,     0,     0,   329,   329,     0,   599,     0,
       0,   519,   599,     0,   558,   244,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
     325,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,   329,     0,
       0,     0,    46,    47,     0,     0,     0,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,   854,   854,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   854,   854,     0,     0,     0,   447,   447,     0,
       0,     0,     0,     0,     0,     0,   109,     0,     0,   599,
       0,     0,  1045,     0,     0,     0,    63,    64,     0,  1057,
       0,     0,     0,     0,     0,     0,   854,   854,     0,     0,
       0,     0,     0,     0,     0,   109,  1338,  1338,  1338,   152,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   260,     0,     0,   444,     0,     0,  1547,
       0,     0,  1550,  1564,    76,  1556,  1556,     0,  1571,     0,
       0,     0,  1575,     0,  1577,     0,     0,     0,     0,     0,
       0,   597,     0,     0,     0,     0,   119,     0,     0,   119,
     450,     0,     0,     0,  1115,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   448,
       0,     0,     0,     0,     0,   325,   634,     0,     0,     0,
       0,   333,     0,     0,     0,     0,     0,     0,     0,     0,
     262,     0,   114,     0,     0,     0,     0,     0,     0,   152,
       0,     0,     0,   450,   119,   114,  1210,     0,     0,     0,
     634,     0,     0,     0,     0,     0,     0,   329,   329,     0,
       0,   599,   450,     0,     0,     0,     0,     0,   119,     0,
       0,   329,   329,     0,     0,     0,   329,   329,     0,     0,
       0,     0,     0,     0,     0,   599,     0,     0,   119,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   599,     0,
       0,     0,     0,     0,     0,   329,   329,     0,     0,     0,
       0,   854,   854,     0,  1665,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   119,     0,     0,     0,     0,
       0,   119,     0,   119,     0,     0,     0,  1688,     0,     0,
       0,     0,     0,     0,   109,   109,  1698,   854,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1704,     0,
       0,     0,     0,     0,     0,   119,  1705,     0,     0,     0,
       0,     0,     0,  1719,  1721,     0,     0,   119,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   450,
       0,     0,     0,     0,   448,     0,     0,  1550,     0,     0,
    1556,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   325,     0,     0,   152,     0,     0,     0,     0,     0,
       0,     0,     0,   854,     0,     0,     0,     0,   119,     0,
     450,   119,     0,     0,     0,     0,   119,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   333,   333,   854,     0,  1359,  1361,  1363,   854,
     854,     0,     0,     0,   447,   447,     0,     0,     0,   119,
       0,   333,     0,   120,     0,     0,   120,     0,     0,     0,
    1787,     0,     0,     0,     0,     0,  1382,     0,   119,     0,
     329,   329,     0,     0,     0,     0,     0,     0,     0,   333,
       0,  1115,     0,     0,     0,     0,     0,  1806,     0,     0,
       0,     0,     0,     0,     0,     0,  1809,     0,  1811,     0,
       0,  1816,  1820,     0,  1564,     0,   329,  1556,     0,  1826,
     114,   120,     0,     0,     0,     0,     0,   333,     0,   634,
       0,     0,     0,     0,     0,   260,     0,     0,     0,     0,
       0,     0,     0,   599,     0,   120,   262,     0,   333,     0,
       0,     0,   119,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   120,     0,     0,     0,   109,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     329,     0,     0,     0,     0,     0,   119,     0,     0,     0,
       0,     0,   329,     0,     0,   450,     0,     0,     0,     0,
       0,     0,   120,     0,     0,     0,     0,     0,   120,     0,
     120,     0,   119,     0,  1892,     0,     0,     0,     0,  1897,
    1899,     0,     0,   329,     0,     0,     0,     0,   329,   329,
       0,  1907,     0,   329,   329,     0,     0,     0,     0,     0,
       0,     0,   120,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   120,     0,   447,     0,   333,     0,
       0,     0,     0,     0,  1537,     0,     0,  1539,     0,     0,
       0,     0,     0,     0,  1556,   333,   333,     0,     0,  1947,
       0,  1950,     0,     0,  1952,  1954,     0,     0,     0,  1957,
    1959,   119,   119,     0,     0,     0,   109,     0,     0,     0,
       0,     0,  1556,  1907,     0,   120,     0,     0,   120,     0,
       0,     0,     0,   120,     0,     0,   124,     0,     0,   124,
       0,     0,     0,     0,     0,     0,     0,     0,   333,     0,
       0,     0,     0,   119,     0,     0,     0,   119,     0,   119,
    1556,     0,   204,     0,     0,     0,   120,     0,   215,   216,
       0,     0,   119,     0,     0,     0,     0,     0,  2001,  2003,
    2005,     0,     0,     0,     0,   120,     0,     0,     0,  2010,
       0,     0,     0,     0,   124,     0,   114,     0,     0,  2018,
       0,     0,   278,     0,     0,     0,   854,     0,     0,     0,
       0,     0,  2028,  2030,  2032,     0,     0,     0,   124,     0,
       0,     0,     0,     0,     0,   114,     0,     0,     0,     0,
     597,     0,     0,   119,     0,     0,     0,     0,   124,     0,
       0,     0,     0,   262,     0,     0,   119,     0,   119,   119,
       0,   119,     0,     0,     0,   329,     0,   119,     0,   120,
     119,   119,   119,     0,     0,     0,     0,     0,     0,     0,
       0,   599,     0,   109,     0,   124,     0,     0,  1694,     0,
       0,   124,     0,   124,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   120,     0,     0,     0,     0,     0,   450,
       0,   109,   597,  1867,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   124,     0,     0,     0,   120,
       0,     0,     0,     0,     0,     0,     0,   124,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   109,
     119,     0,     0,     0,     0,     0,     0,   333,   333,     0,
       0,   362,     0,     0,     0,   363,     0,   364,     0,     0,
       0,   333,   333,     0,     0,   634,   333,   333,   569,     0,
       0,     0,     0,     0,   365,     0,     0,     0,   124,     0,
       0,   124,     0,     0,     0,   329,   124,     0,     0,     0,
       0,     0,     0,     0,     0,   333,   333,     0,   120,   120,
       0,   366,   367,     0,   368,     0,   369,    63,    64,    65,
      66,    67,    68,    69,    70,    71,   370,   371,   359,   124,
     372,   373,   374,     0,   375,   376,     0,     0,     0,     0,
       0,     0,    73,     0,   114,   114,     0,     0,   124,     0,
     120,     0,     0,     0,   120,     0,   120,     0,     0,     0,
       0,     0,   377,     0,     0,    76,   378,   119,     0,   120,
       0,     0,   379,    78,    79,   380,   381,   382,   383,     0,
     119,   119,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   450,     0,     0,  1843,     0,     0,
       0,     0,     0,     0,     0,     0,   634,     0,     0,     0,
      14,    15,    16,    17,    18,     0,     0,     0,     0,     0,
       0,     0,   124,     0,     0,   763,     0,   764,     0,     0,
     120,     0,     0,     0,     0,     0,   780,   781,     0,     0,
       0,     0,     0,   120,     0,   120,   120,     0,   120,     0,
       0,     0,     0,     0,   120,     0,   124,   120,   120,   120,
       0,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   124,     0,     0,     0,     0,     0,     0,     0,
     333,   333,     0,     0,     0,     0,     0,     0,     0,    62,
       0,     0,   218,   219,    65,    66,    67,    68,    69,    70,
      71,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   333,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   120,     0,     0,
       0,     0,     0,     0,   864,   262,     0,  1526,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    78,    79,
       0,   124,   124,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   114,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     333,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   333,   124,     0,     0,     0,   124,     0,   124,
       0,     0,     0,     0,     0,   119,     0,     0,     0,     0,
       0,     0,   124,   119,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   333,     0,     0,     0,  1202,   333,   333,
       0,     0,     0,   333,   333,    14,    15,    16,    17,    18,
       0,     0,   119,     0,   120,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   120,   120,     0,
     119,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   362,     0,   124,     0,   363,     0,   364,     0,     0,
     119,     0,     0,     0,     0,     0,   124,     0,   124,   124,
       0,   124,     0,    58,   365,     0,   114,   124,     0,     0,
     124,   124,   124,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     119,   366,   367,     0,   368,     0,   369,    63,    64,    65,
      66,    67,    68,    69,    70,    71,   370,   371,   359,     0,
     372,   373,   374,     0,   375,   376,     0,     0,     0,     0,
       0,     0,    73,  1043,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   377,     0,     0,    76,   378,     0,     0,     0,
     124,     0,   379,   440,    79,   380,   381,   382,   383,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     599,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   119,   119,   119,   119,   119,   119,     0,     0,
    1111,  1112,     0,     0,     0,   333,     0,     0,     0,     0,
       0,  1170,  1171,  1172,     0,     0,  1174,     0,     0,     0,
       0,   119,   119,   114,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   114,   599,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   120,     0,     0,     0,     0,   124,     0,     0,
     120,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     124,   124,     0,     0,     0,     0,     0,     0,     0,   114,
       0,     0,     0,     0,     0,   119,  1241,     0,     0,   120,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   120,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   333,     0,   120,     0,     0,
       0,     0,  1262,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   120,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   119,     0,  1286,
       0,     0,     0,     0,     0,     0,     0,     0,  1290,  1291,
    1292,  1293,     0,     0,     0,     0,  1298,  1299,     0,     0,
       0,     0,     0,     0,   389,     0,  1307,     0,     0,     0,
       1,     0,   119,   146,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1321,     0,  1322,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   119,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     119,     0,     0,     0,     0,     0,     0,     0,     0,   120,
     120,   120,   120,   120,   120,     0,     0,     0,     0,     0,
       0,     0,  1378,     0,     0,   119,     0,   193,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   120,   120,
       0,     0,     0,     0,     0,   124,     0,     0,  1392,     0,
       0,     0,     0,   124,     0,  1396,     0,  1398,  1400,     0,
       0,     0,     0,     0,     0,     0,     0,  1407,     0,  1408,
       0,  1409,     0,  1411,     0,     0,     0,     0,  1419,     0,
       0,     0,   124,     0,     0,     0,   284,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     124,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   120,   119,     0,     0,     0,     0,     0,     0,
     124,     0,     0,     0,     0,     0,     0,   652,     0,     0,
     389,   657,     0,     0,     0,     0,     0,     0,     0,  1462,
     663,   664,     0,     0,     0,     0,  1469,  1470,     0,     0,
       0,     0,     0,     0,     0,   389,   389,     0,     0,     0,
     124,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1493,     0,     0,     0,     0,     0,   389,  1498,     0,     0,
     284,  1499,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   522,     0,     0,     0,     0,
       0,     0,     0,     0,   120,   284,   389,     0,     0,     0,
       0,   215,     0,     0,     0,   284,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   553,
     557,     0,     0,     0,     0,     0,   564,   565,     0,   120,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1592,   575,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   124,   124,   124,   124,   124,   124,     0,     0,
     119,     0,   594,   120,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1612,     0,   120,     0,     0,
       0,   124,   124,  1617,     0,  1619,     0,     0,   119,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   120,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   680,     0,
       0,     0,     0,     0,     0,     0,   119,     0,     0,     0,
       0,     0,  1646,  1647,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1652,  1653,   721,
    1654,     0,     0,     0,     0,   124,   166,     0,     0,  1658,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1663,
    1664,     0,     0,     0,     0,   759,     0,     0,     0,   762,
       0,     0,   166,     0,     0,     0,     0,     0,     0,     0,
     120,     0,     0,     0,     0,     0,     0,     0,   784,     0,
       0,     0,   785,   786,     0,     0,   789,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   803,   804,   805,   806,     0,     0,     0,   166,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     828,   166,     0,   166,     0,     0,     0,   124,   831,     0,
       0,     0,     0,   389,   389,   389,   389,   389,   389,   389,
     389,   389,   389,   389,   389,   389,   389,   389,   389,   389,
     389,   389,     0,   353,     0,     0,   284,     0,     0,     0,
    1748,  1749,   124,     0,     0,     0,     0,     0,     0,     0,
     353,  1756,   164,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   869,     0,     0,
       0,     0,     0,     0,   553,     0,   124,     0,     0,     0,
     875,     0,     0,     0,     0,     0,  1779,  1780,   166,     0,
     124,     0,   166,   389,     0,   166,   166,     0,     0,   166,
       0,     0,   166,   166,   890,   895,     0,   120,     0,     0,
       0,     0,     0,     0,     0,   124,     0,     0,     0,     0,
       0,     0,     0,     0,   276,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   120,     0,   282,     0,   283,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   166,     0,     0,   166,   937,     0,
       0,     0,  1845,   120,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   166,   166,
       0,  1854,     0,     0,  1855,  1856,     0,     0,     0,     0,
       0,  1858,     0,   124,   166,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     389,   504,   505,     0,     0,   509,     0,  1000,   512,   513,
       0,     0,     0,     0,     0,     0,     0,     0,   389,     0,
       0,     0,  1017,   389,     0,     0,  1018,     0,     0,     0,
       0,     0,     0,     0,   389,   890,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1058,     0,     0,
       0,     0,     0,     0,     0,     0,  1067,     0,   166,     0,
       0,     0,  1070,     0,     0,     0,   389,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   591,   592,     0,     0,     0,     0,
    1968,     0,     0,     0,     0,     0,     0,     0,     0,     1,
     624,     0,     0,     0,     0,     0,     1,     0,     0,     0,
       0,     0,     0,   353,     0,     0,     0,     0,     0,     0,
     124,     0,     0,     0,     0,   166,     0,     0,     0,     0,
       0,     0,     0,     1,     0,     0,     0,     0,     0,     0,
       0,   340,     0,     0,     0,     0,     0,     0,   124,     0,
       0,     0,  2007,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     437,   340,     0,     0,     0,     0,  2024,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   124,  1221,     0,     0,
       0,  2033,     0,   389,   753,     0,     0,     0,     0,   353,
       0,     0,   503,     0,     0,     0,  2046,     0,     0,   503,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     166,   166,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   166,     0,     0,     0,     0,     0,     0,
       0,   824,     0,     0,     0,     0,     0,     0,   389,     0,
    1269,     0,     0,     0,  1270,     0,     0,   503,     0,     0,
       0,   890,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1283,     0,     0,     0,     0,     0,     0,  1284,     0,
       0,   340,   609,   389,   389,   389,     0,  1288,     0,  1289,
     389,   389,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   630,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   389,     0,     0,     0,     0,     0,
       0,  1317,     0,     0,     0,  1318,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   146,
       0,     0,     1,     0,     0,     0,     0,     0,     0,   166,
     166,   389,   389,     0,     0,   166,   901,   902,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   909,
       0,     0,   503,     0,     0,     0,   166,     0,     0,   166,
     166,     0,   166,     0,   166,   166,     0,     0,   503,   755,
       0,   503,   758,     0,     0,     0,     0,     0,     0,   340,
       0,     0,     0,   609,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   166,     0,     0,     0,   166,     0,     0,
       0,  1406,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   503,     0,     0,     0,   503,     0,
       0,     0,     0,     0,     0,     0,  1430,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     340,     0,     0,     0,     0,  1006,  1007,     0,     0,     0,
       0,  1011,     0,     0,     0,     0,     0,     0,  1417,     0,
       0,     0,     0,     0,   166,     0,    14,    15,    16,    17,
      18,     0,  1032,     0,     0,  1035,  1036,     0,  1039,     0,
    1041,  1042,     0,     0,     0,     0,     0,     0,     0,     0,
     503,     0,     0,   340,     0,     0,     0,     0,     0,     0,
       0,     0,   362,     0,     0,     0,   363,     0,   364,  1502,
       0,   885,   340,  1503,     0,     0,     0,     0,     0,  1082,
       0,     0,   609,  1086,    58,   365,   609,     0,     0,     0,
       0,     0,     0,   903,     0,   340,     0,     0,     0,     0,
       0,     0,     0,  1538,     0,     0,     0,     0,     0,     0,
       0,     0,   366,   367,     0,   368,     0,   369,    63,    64,
      65,    66,    67,    68,    69,    70,    71,   370,   371,   359,
       0,   372,   373,   374,     0,   375,   376,     0,     0,     0,
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1602,     0,   166,  1605,     0,     0,     0,
    1204,     0,     0,   377,     0,     0,    76,   378,     0,     0,
       0,   389,  1614,   379,  1418,    79,   380,   381,   382,   383,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   166,     0,     0,   340,
       0,     0,     0,   166,     0,     0,   166,     0,     0,     0,
       0,     0,     0,     0,     0,   503,   503,     0,     0,     0,
       0,     0,     0,  1645,     0,   503,  1013,     0,   503,  1016,
       0,     0,  1650,     0,     0,     0,  1651,     0,     0,     0,
     340,     0,     0,   609,     0,   609,   609,     0,   201,     0,
    1655,  1656,   609,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   340,   340,   256,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   340,     0,     0,     0,   503,     0,     0,     0,   503,
       0,  1204,     0,   503,  1084,     0,     0,   503,  1088,     0,
       0,     0,     0,     0,     0,  1091,     0,     0,     0,     0,
       0,     0,     0,   201,     0,     0,     0,   304,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   166,   345,     0,
       0,     0,  1303,     0,     0,   166,   166,     0,     0,  1310,
       0,     0,  1314,     0,     0,   201,     0,   340,   503,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   457,     0,
       0,   461,     0,     0,     0,     0,  1744,  1745,     0,     0,
       0,     0,     0,     0,   389,     0,   609,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   166,     0,     0,     0,     0,     0,     0,
       0,     0,   166,     0,     0,   166,     0,   166,   166,     0,
       0,   201,     0,     0,   389,   340,     0,     0,     0,     0,
       0,     0,     0,     0,   256,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   166,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     461,     0,     0,  1413,     0,     0,     0,   413,   201,     0,
       0,  1422,  1423,     0,     0,     0,     0,     0,     0,     0,
     442,     0,   503,     0,     0,     0,     0,     0,   602,     0,
     619,     0,  1832,   470,     0,   470,     0,     0,     0,   609,
     609,     0,     0,     0,     0,     0,   609,     0,     0,     0,
     389,     0,   389,     0,     0,   166,  1605,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1464,
       0,     0,     0,     0,  1857,     0,     0,     0,  1473,     0,
       0,  1477,   678,  1480,  1481,     0,     0,     0,   340,     0,
       0,   389,     0,   503,  1312,     0,   503,  1316,     0,     0,
       0,  1875,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   201,     0,     0,     0,
       0,   570,     0,   389,  1507,     0,     0,     0,  1903,     0,
       0,  1904,     0,     0,     0,     0,     0,     0,     0,   166,
       0,     0,     0,     0,     0,     0,   602,     0,     0,     0,
       0,     0,   779,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   166,     0,
       0,   389,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1599,     0,     0,   166,     0,     0,     0,     0,     0,
     166,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   340,     0,     0,     0,     0,     0,   609,  1415,     0,
       0,   201,   201,   243,     0,     0,     0,   457,     0,     0,
    1987,     0,     0,    14,    15,    16,    17,    18,     0,   340,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,  -453,  -453,   166,
    -453,    46,    47,     0,  -453,  1477,     0,     0,     0,     0,
     345,     0,     0,   503,  1466,     0,     0,     0,     0,     0,
       0,    58,   503,  1475,   470,   609,     0,     0,     0,   457,
     470,   889,     0,     0,  1660,   800,   340,   340,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   602,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   201,     0,   166,   166,     0,     0,     0,
      73,     0,     0,   353,     0,     0,   678,   166,   678,   678,
       0,   678,     0,     0,     0,     0,     0,   678,     0,     0,
     678,   678,   678,    76,   302,     0,     0,     0,     0,     0,
       0,    78,    79,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   868,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1742,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   442,     0,     0,   340,     0,   457,     0,     0,     0,
       0,     0,     0,     0,   897,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     201,     0,     0,     0,     0,     0,     0,   166,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   457,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1791,  1792,     0,     0,   931,     0,     0,     0,   457,
     457,     0,     0,  1796,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   800,   951,   457,     0,
     953,     0,   955,     0,     0,     0,     0,     0,   964,     0,
     969,   964,     0,     0,     0,     0,     0,     0,     0,   166,
       0,     0,     0,     0,   503,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   997,     0,
     503,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   999,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1008,     0,   457,     0,     0,     0,     0,     0,
       0,   201,     0,     0,     0,     0,   442,     0,     0,   997,
       0,     0,   779,  1865,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1061,     0,     0,   470,
     340,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   166,     0,     0,
       0,     0,   345,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1092,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1931,     0,     0,     0,     0,
       0,     0,     0,   340,   340,     0,   362,     0,     0,     0,
     363,     0,   364,     0,     0,     0,     0,     0,   503,   503,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   365,
       0,     0,     0,   413,   503,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1194,  1196,     0,     0,     0,     0,
       0,     0,   442,     0,     0,     0,   366,   367,     0,   464,
       0,   369,    63,    64,    65,    66,    67,    68,    69,    70,
      71,   370,   371,   359,     0,   372,   373,   374,     0,   375,
     376,   964,     0,     0,     0,     0,     0,    73,     0,     0,
       0,     0,     0,     0,   997,     0,     0,     0,     0,     0,
       0,     0,  1234,     0,     0,   457,     0,   377,    75,   964,
     465,   466,     0,     0,     0,   467,     0,   379,    78,    79,
     380,   381,   382,   383,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   503,     0,     0,     0,     0,
       0,     0,     0,   503,     0,   678,     0,     0,     0,    14,
      15,    16,    17,    18,     0,   470,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,  -453,  -453,     0,  -453,    46,    47,     0,
    -453,     0,     0,     0,     0,     0,     0,     0,     0,   256,
       0,     0,     0,     0,     0,   340,     0,    58,     0,   503,
    1933,     0,     0,   503,     0,     0,     0,     0,     0,     0,
     201,     0,   470,     0,  1302,     0,  1305,     0,   602,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    62,     0,
       0,    63,    64,    65,    66,    67,    68,    69,    70,    71,
       0,     0,     0,     0,   503,     0,   345,     0,     0,     0,
     678,     0,     0,     0,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   362,     0,     0,     0,
     363,     0,   364,     0,     0,     0,    74,    75,     0,    76,
     302,     0,  1370,  1370,     0,     0,     0,    78,    79,   365,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   503,   503,     0,
       0,     0,     0,   457,   457,     0,   366,   367,     0,   368,
       0,   369,    63,    64,    65,    66,    67,    68,    69,    70,
      71,   370,   371,   359,     0,   372,   373,   374,     0,   375,
     376,     0,     0,     0,  1410,     0,     0,    73,   503,     0,
    1420,     0,   678,   678,   678,     0,   678,   678,     0,   470,
       0,     0,     0,   461,     0,     0,     0,   377,   442,     0,
      76,   378,     0,     0,     0,   467,     0,   379,    78,    79,
     380,   381,   382,   383,     0,   470,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     964,     0,     0,   800,     0,     0,     0,     0,     0,     0,
       0,   256,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   345,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1330,     0,     0,
       0,     0,     0,     0,  1331,  1501,     0,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   964,     0,    46,    47,     0,     0,     0,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,   470,     0,     0,   800,    58,  1332,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    61,     0,   201,     0,    63,
      64,     0,     0,   951,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1615,  1616,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,   256,     0,     0,     0,     0,     0,     0,     0,   470,
       0,   800,     0,  1333,     0,     0,     0,    76,   927,     0,
       0,     0,     0,     0,     0,    78,    79,     0,     0,   243,
     244,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,     0,    20,   345,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,  -453,  -453,   678,  -453,    46,    47,     0,
    -453,     0,     0,     0,     0,     0,     0,     0,     0,   413,
       0,     0,     0,     0,  1687,     0,     0,    58,     0,     0,
     457,   457,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    62,     0,
    2047,    63,    64,    65,    66,    67,    68,    69,    70,    71,
       0,     0,     0,     0,     0,     0,     0,  1354,     0,     0,
     256,     0,     0,     0,  1733,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,    75,   362,    76,
     245,     0,   363,     0,   364,     0,     0,    78,    79,     0,
       0,     0,     0,     0,     0,  1757,     0,     0,  1759,  1117,
       0,   365,    -2,     0,  1119,  -238,  -238,  1120,  1121,  1122,
    1123,  1124,  1125,  1126,  1127,  1128,  1129,  1130,  1131,  -332,
    1132,  1133,  1134,  1135,  1136,     0,  1137,     0,   366,   367,
       0,   464,     0,   369,  1138,  1139,    65,    66,    67,    68,
      69,    70,    71,   370,   371,   359,  1140,   372,   373,   374,
       0,   375,   376,     0,     0,     0,     0,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   678,  -238,   377,
       0,     0,    76,   378,  2047,     0,     0,   280,     0,   379,
      78,    79,   380,   381,   382,   383,     0,     0,     0,     0,
       0,  1354,   457,     0,  -179,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   362,     0,     0,     0,   363,     0,   364,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   678,
       0,     0,   461,  1117,     0,   365,    -2,     0,  1119,  -239,
    -239,  1120,  1121,  1122,  1123,  1124,  1125,  1126,  1127,  1128,
    1129,  1130,  1131,  -332,  1132,  1133,  1134,  1135,  1136,     0,
    1137,     0,   366,   367,     0,   464,     0,   369,  1138,  1139,
      65,    66,    67,    68,    69,    70,    71,   370,   371,   359,
    1140,   372,   373,   374,     0,   375,   376,   964,  1740,     0,
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1354,     0,     0,     0,     0,
       0,     0,  -239,   377,     0,     0,    76,   378,     0,     0,
       0,   280,     0,   379,    78,    79,   380,   381,   382,   383,
       0,     0,     0,     0,     0,     0,   362,     0,  -179,     0,
     363,     0,   364,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1117,     0,   365,
      -2,     0,  1119,     0,     0,  1120,  1121,  1122,  1123,  1124,
    1125,  1126,  1127,  1128,  1129,  1130,  1131,  -332,  1132,  1133,
    1134,  1135,  1136,     0,  1137,     0,   366,   367,     0,   464,
       0,   369,  1138,  1139,    65,    66,    67,    68,    69,    70,
      71,   370,   371,   359,  1140,   372,   373,   374,     0,   375,
     376,     0,     0,     0,     0,     0,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   377,     0,     0,
      76,   378,     0,     0,     0,   280,     0,   379,    78,    79,
     380,   381,   382,   383,     0,     0,     0,     0,     0,     0,
       0,     0,  -179,     4,   244,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,  1116,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,   362,
       0,    46,    47,   363,     0,   364,    48,    49,    50,    51,
      52,    53,    54,    55,    56,     0,     0,     0,    57,     0,
    1117,    58,  1118,    -2,     0,  1119,     0,     0,  1120,  1121,
    1122,  1123,  1124,  1125,  1126,  1127,  1128,  1129,  1130,  1131,
    -332,  1132,  1133,  1134,  1135,  1136,     0,  1137,     0,   366,
     367,    61,   464,     0,   369,  1138,  1139,    65,    66,    67,
      68,    69,    70,    71,   370,   371,   359,  1140,   372,   373,
     374,     0,   375,   376,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    -3,
     377,     0,     0,    76,   409,     0,     0,     0,   280,     0,
     379,    78,    79,   380,   381,   382,   383,     0,     0,     0,
       0,     0,     0,     0,     0,  -179,     4,   244,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,  1116,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   362,     0,    46,    47,   363,     0,   364,    48,
      49,    50,    51,    52,    53,    54,    55,    56,     0,     0,
       0,    57,     0,  1117,    58,  1118,    -2,     0,  1119,     0,
       0,  1120,  1121,  1122,  1123,  1124,  1125,  1126,  1127,  1128,
    1129,  1130,  1131,  -332,  1132,  1133,  1134,  1135,  1136,     0,
    1137,     0,   366,   367,    61,   464,     0,   369,  1138,  1139,
      65,    66,    67,    68,    69,    70,    71,   370,   371,   359,
    1140,   372,   373,   374,     0,   375,   376,     0,     0,     0,
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   377,     0,     0,    76,   409,     0,     0,
       0,   280,     0,   379,    78,    79,   380,   381,   382,   383,
       0,     0,     0,     0,     0,     0,     0,     0,  -179,     4,
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
    1558,  1559,  1560,     0,     0,     0,   377,  1561,  1562,    76,
     409,     0,     0,     0,     0,     0,   379,    78,    79,   380,
     381,   382,   383,     0,     0,     0,     0,     0,     0,     0,
       0,  1563,     4,   244,     6,     7,     8,     9,    10,    11,
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
       0,     0,     0,  1558,  1559,  1560,     0,     0,     0,   377,
    1561,     0,    76,   409,     0,     0,     0,     0,     0,   379,
      78,    79,   380,   381,   382,   383,     0,     0,     0,     0,
       0,     0,     0,     0,  1563,     4,   244,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   362,     0,    46,    47,   363,     0,   364,    48,    49,
      50,    51,    52,    53,    54,    55,    56,     0,     0,     0,
      57,     0,     0,    58,   365,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   366,   367,    61,   368,     0,   369,    63,    64,    65,
      66,    67,    68,    69,    70,    71,   370,   371,   359,     0,
     372,   373,   374,     0,   375,   376,     0,     0,     0,     0,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   377,     0,  1549,    76,   409,     0,     0,     0,
       0,     0,   379,    78,    79,   380,   381,   382,   383,     4,
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
       0,     0,     0,     0,     0,     0,   377,     0,     0,    76,
     409,     0,     0,     0,     0,     0,   379,    78,    79,   380,
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
       0,     0,    76,   439,     0,     0,     0,     0,     0,   379,
     440,    79,   380,   381,   382,   383,   244,     6,     7,     8,
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
       0,     0,   377,     0,     0,    76,  1191,     0,     0,     0,
       0,     0,   379,  1192,    79,   380,   381,   382,   383,   244,
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
       0,     0,     0,     0,     0,   377,     0,     0,    76,   378,
       0,     0,     0,     0,     0,   379,    78,    79,   380,   381,
     382,   383,   244,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,   362,     0,    46,
      47,   363,     0,   364,   320,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
     365,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   366,   367,     0,
     368,     0,   369,    63,    64,    65,    66,    67,    68,    69,
      70,    71,   370,   371,   359,     0,   372,   373,   374,     0,
     375,   376,     0,     0,     0,     0,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   377,     0,
       0,    76,   439,     0,     0,     0,     0,     0,   379,    78,
      79,   380,   381,   382,   383,  1874,     0,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,     0,    -2,     0,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,     0,    -2,    -2,    -2,     0,    -2,     0,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,
       0,     0,    -2,     0,     0,    -2,     0,     0,     0,     0,
      -2,    -2,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    -2,     0,     0,     0,    -2,
      -2,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    -2,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    -2,     0,     0,     0,    -2,    -2,     0,
       0,     0,     0,     0,     0,    -2,    -2,  1902,     0,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,     0,    -2,     0,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,     0,    -2,    -2,    -2,     0,
      -2,     0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,     0,     0,     0,    -2,     0,     0,    -2,     0,     0,
       0,     0,    -2,    -2,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    -2,     0,     0,
       0,    -2,    -2,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    -2,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    -2,     0,     0,     0,    -2,
      -2,     0,     0,     0,     0,     0,     0,    -2,    -2,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,    47,     0,
       0,     0,    48,    49,    50,    51,    52,    53,    54,    55,
      56,     0,     0,     0,    57,     0,     0,    58,    59,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    60,     0,     0,     0,    61,    62,     0,
       0,    63,    64,    65,    66,    67,    68,    69,    70,    71,
       0,     0,     0,    72,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,    75,     0,    76,
      77,     0,     0,     0,     0,     0,     0,    78,    79,   243,
     244,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,  -453,  -453,     0,  -453,    46,    47,     0,
    -453,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    14,    15,    16,    17,    18,     0,    58,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,    62,    46,
      47,    63,    64,    65,    66,    67,    68,    69,    70,    71,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,    75,     0,    76,
     245,     0,     0,     0,  -765,     0,     0,    78,    79,     4,
     244,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,    47,     0,
       0,     0,    48,    49,    50,    51,    52,    53,    54,    55,
      56,     0,     0,     0,    57,     0,     0,    58,     0,     0,
       0,     0,  -385,  -385,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    61,     0,     0,
       0,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -385,     0,     0,     0,    76,
      77,     0,     0,     0,     0,     0,     0,    78,    79,     4,
     244,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,    47,     0,
       0,     0,    48,    49,    50,    51,    52,    53,    54,    55,
      56,     0,     0,     0,    57,     0,     0,    58,     0,     0,
       0,     0,  -386,  -386,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    61,     0,     0,
       0,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -386,     0,     0,     0,    76,
      77,     0,  1330,     0,     0,     0,     0,    78,    79,  1331,
       0,     0,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,    47,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,  1332,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      61,     0,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1513,     0,
       0,     0,    76,   927,     0,  1330,     0,     0,     0,     0,
      78,    79,  1331,     0,     0,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,    47,     0,     0,     0,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,  1332,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    61,     0,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1514,     0,     0,     0,    76,   927,     0,  1330,     0,
       0,     0,     0,    78,    79,  1331,     0,     0,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,    47,     0,     0,
       0,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,  1332,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    61,     0,     0,     0,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1515,     0,     0,     0,    76,   927,
       0,     0,     0,     0,     0,     0,    78,    79,   243,   244,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,     0,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,  -453,  -453,     0,  -453,    46,    47,     0,  -453,
       0,     0,     0,     0,     0,     0,     0,     0,    14,    15,
      16,    17,    18,     0,     0,    20,    58,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,    47,     0,     0,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   659,    76,   245,
       0,     0,     0,     0,     0,     0,    78,    79,   244,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -453,  -453,     0,  -453,    46,    47,     0,  -453,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    62,     0,     0,    63,
      64,    65,    66,    67,    68,    69,    70,    71,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,    75,     0,    76,   245,     0,
       0,     0,  -769,     0,     0,    78,    79,   244,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,     0,     0,    20,     0,    21,    22,    23,    24,    25,
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
       0,     0,     0,    74,    75,     0,    76,   245,     0,     0,
       0,     0,     0,     0,    78,    79,   244,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,    47,     0,     0,     0,   320,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    73,     0,  1052,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -637,    76,   322,     0,     0,     0,
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
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   321,    76,   322,     0,     0,     0,     0,
       0,     0,    78,    79,   244,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,    47,     0,     0,     0,   320,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      73,     0,  1775,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    76,   322,     0,     0,     0,     0,     0,
       0,    78,    79,   244,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,    47,     0,     0,     0,   320,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    73,
       0,  1777,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    76,   322,     0,     0,     0,     0,     0,     0,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    76,   302,     0,     0,     0,     0,     0,     0,    78,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      76,   322,     0,     0,     0,     0,     0,     0,    78,    79,
     244,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,  -453,  -453,     0,  -453,    46,    47,     0,
    -453,     0,     0,     0,     0,     0,     0,     0,     0,    14,
      15,    16,    17,    18,     0,     0,    20,    58,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,  -452,  -452,     0,  -452,    46,    47,     0,
    -452,    63,    64,     0,     0,     0,     0,  1354,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   362,     0,
       0,     0,   363,     0,   364,     0,     0,     0,     0,    76,
     245,     0,     0,     0,     0,     0,     0,    78,    79,  1117,
       0,   365,     0,     0,  1119,  1799,  1800,  1120,  1121,  1122,
    1123,  1124,  1125,  1126,  1127,  1128,  1129,  1130,  1131,  -332,
    1132,  1133,  1134,  1135,  1136,     0,  1137,     0,   366,   367,
       0,   464,     0,   369,  1138,  1139,    65,    66,    67,    68,
      69,    70,    71,   370,   371,   359,  1140,   372,   373,   374,
       0,   375,   376,     0,     0,     0,     0,     0,     0,    73,
       0,     0,     0,     0,     0,  1354,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   377,
       0,     0,    76,   378,     0,     0,     0,   280,     0,   379,
      78,    79,   380,   381,   382,   383,   362,     0,     0,     0,
     363,     0,   364,     0,  -179,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1117,     0,   365,
       0,     0,  1119,     0,     0,  1120,  1121,  1122,  1123,  1124,
    1125,  1126,  1127,  1128,  1129,  1130,  1131,  -332,  1132,  1133,
    1134,  1135,  1136,     0,  1137,     0,   366,   367,     0,   464,
       0,   369,  1138,  1139,    65,    66,    67,    68,    69,    70,
      71,   370,   371,   359,  1140,   372,   373,   374,     0,   375,
     376,     0,     0,     0,     0,     0,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   377,     0,     0,
      76,   378,     0,     0,     0,   280,     0,   379,    78,    79,
     380,   381,   382,   383,     0,     0,     0,     0,     0,     0,
       0,     0,  -179,    14,    15,    16,    17,    18,    19,   665,
      20,   666,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,   362,
       0,    46,    47,   363,     0,   364,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,   365,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   667,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   366,
     367,     0,   368,     0,   369,    63,    64,    65,    66,    67,
      68,    69,    70,    71,   370,   371,   359,     0,   372,   373,
     374,     0,   375,   376,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     377,     0,     0,    76,   668,     0,     0,     0,   280,     0,
     379,    78,    79,   669,   670,   382,   383,    14,    15,    16,
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
       0,     0,     0,     0,   377,     0,   408,    76,   409,     0,
       0,     0,     0,     0,   379,    78,    79,   380,   381,   382,
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
       0,    76,   668,     0,     0,     0,   280,     0,   379,    78,
      79,   380,   381,   382,   383,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   362,     0,    46,    47,   363,     0,   364,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,   365,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   366,   367,     0,   368,     0,   369,    63,    64,    65,
      66,    67,    68,    69,    70,    71,   370,   371,   359,     0,
     372,   373,   374,     0,   375,   376,     0,     0,     0,     0,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   377,     0,     0,    76,   409,     0,     0,     0,
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
     439,     0,     0,     0,     0,     0,   379,    78,    79,   380,
     381,   382,   383,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,   362,
       0,    46,    47,   363,     0,   364,   320,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,   365,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   366,
     367,     0,   368,     0,   369,    63,    64,    65,    66,    67,
      68,    69,    70,    71,   370,   371,   359,     0,   372,   373,
     374,     0,   375,   376,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     377,     0,     0,    76,   378,     0,     0,     0,     0,     0,
     379,    78,    79,   380,   381,   382,   383,   243,   244,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -453,  -453,     0,  -453,    46,    47,     0,  -453,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    14,
      15,    16,    17,    18,    19,    58,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,    47,    63,
      64,     0,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    76,    62,     0,
       0,    63,    64,    65,    66,    67,    68,    69,    70,    71,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,    75,     0,    76,
      77,     0,     0,     0,  -767,     0,     0,    78,    79,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,    47,     0,
       0,     0,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    62,     0,
       0,    63,    64,    65,    66,    67,    68,    69,    70,    71,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,    75,     0,    76,
      77,     0,     0,     0,     0,     0,     0,    78,    79,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,    47,     0,
       0,     0,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    62,     0,
       0,    63,    64,    65,    66,    67,    68,    69,    70,    71,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    76,
      77,     0,     0,     0,     0,     0,     0,    78,    79,   244,
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
       0,     0,     0,     0,     0,     0,     0,   853,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -650,    76,   244,
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
       0,     0,     0,     0,     0,     0,     0,  1695,     0,     0,
       0,   244,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,    76,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,    47,
       0,     0,     0,   320,    49,    50,    51,    52,    53,    54,
      55,     0,    14,    15,    16,    17,    18,    19,    58,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,    47,    63,    64,     0,    48,    49,    50,    51,    52,
      53,    54,    55,     0,    14,    15,    16,    17,    18,    19,
      58,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
      76,     0,    46,    47,    63,    64,     0,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,  1433,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   933,    76,   927,     0,     0,    63,    64,     0,     0,
      78,    79,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    76,   927,     0,     0,     0,     0,
       0,     0,    78,    79,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,    47,     0,     0,     0,    48,    49,    50,
      51,    52,    53,    54,    55,     0,    14,    15,    16,    17,
      18,    19,    58,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,    47,    63,    64,     0,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    76,   287,     0,     0,    63,    64,
       0,     0,    78,    79,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    76,    77,     0,     0,
       0,     0,     0,     0,    78,    79,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,    47,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,     0,    14,    15,
      16,    17,    18,    19,    58,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,    47,    63,    64,
       0,   320,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    76,   435,     0,     0,
      63,    64,     0,     0,    78,    79,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    76,   322,
       0,     0,     0,     0,     0,     0,    78,    79,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,    47,     0,     0,
       0,   320,    49,    50,    51,    52,    53,    54,    55,     0,
      14,    15,    16,    17,    18,    19,    58,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,    47,
      63,    64,     0,   320,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    76,   287,
       0,     0,    63,    64,     0,     0,    78,    79,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      76,   435,     0,     0,     0,     0,     0,     0,    78,    79,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,    47,
       0,     0,     0,   320,    49,    50,    51,    52,    53,    54,
      55,     0,    14,    15,    16,    17,    18,    19,    58,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,    47,    63,    64,     0,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      76,   302,     0,     0,    63,    64,     0,     0,    78,    79,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    76,   927,     0,     0,     0,     0,     0,     0,
      78,    79,   244,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,     0,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,  -453,  -453,     0,  -453,    46,
      47,     0,  -453,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    14,    15,    16,    17,    18,    19,    58,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,    47,    63,    64,     0,   320,    49,    50,    51,
      52,    53,    54,    55,     0,    14,    15,    16,    17,    18,
      19,    58,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,    76,     0,    46,    47,    63,    64,     0,   320,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    76,   927,     0,     0,    63,    64,     0,
       0,    78,    79,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    76,     0,     0,    14,    15,
      16,    17,    18,    78,    79,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,  -453,  -453,     0,  -453,    46,    47,     0,  -453,
       0,     0,     0,     0,     0,     0,     0,     0,    14,    15,
      16,    17,    18,     0,     0,    20,    58,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,  -453,  -453,     0,  -453,    46,    47,     0,  -453,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    76,   302,
      63,    64,     0,     0,     0,     0,    78,    79,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    76,     0,
       0,    14,    15,    16,    17,    18,    78,    79,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
      47,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    14,    15,    16,    17,    18,    19,     0,    20,
     659,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,    47,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,   -16,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    62,     0,     0,    63,    64,    65,    66,    67,    68,
      69,    70,    71,     0,     0,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,    76,    46,    47,     0,     0,     0,    48,    49,
      50,    51,    52,    53,    54,    55,     0,    14,    15,    16,
      17,    18,    19,    58,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,    47,    63,    64,     0,
     320,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    76,     0,     0,     0,    63,
      64,    14,    15,    16,    17,    18,     0,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,  -453,  -453,     0,  -453,    46,
      47,     0,  -453,     0,     0,     0,     0,    76,   362,     0,
       0,     0,   363,     0,   364,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   365,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    63,    64,     0,     0,     0,   366,   367,
       0,   368,     0,   369,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   370,   371,   359,     0,   372,   373,   374,
       0,   375,   376,     0,   362,     0,     0,     0,   363,    73,
     364,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    76,     0,  1558,  1559,  1560,     0,   365,     0,   377,
    1720,     0,    76,   378,     0,     0,     0,     0,     0,   379,
      78,    79,   380,   381,   382,   383,     0,     0,     0,     0,
       0,     0,     0,     0,   366,   367,     0,   368,     0,   369,
    1814,    64,    65,    66,    67,    68,    69,    70,    71,   370,
     371,   359,     0,   372,   373,   374,     0,   375,   376,     0,
     362,     0,     0,     0,   363,    73,   364,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1558,
    1559,  1560,     0,   365,     0,   377,  1815,     0,    76,   378,
       0,     0,     0,     0,     0,   379,    78,    79,   380,   381,
     382,   383,     0,     0,     0,     0,     0,     0,     0,     0,
     366,   367,     0,   368,     0,   369,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   370,   371,   359,     0,   372,
     373,   374,   362,   375,   376,     0,   363,     0,   364,     0,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   365,     0,     0,     0,     0,
       0,   377,  1237,     0,    76,   378,     0,     0,     0,  1238,
       0,   379,    78,    79,   380,   381,   382,   383,     0,     0,
       0,     0,   366,   367,     0,   368,     0,   369,    63,    64,
      65,    66,    67,    68,    69,    70,    71,   370,   371,   359,
       0,   372,   373,   374,   362,   375,   376,     0,   363,     0,
     364,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   365,     0,     0,
       0,     0,     0,   377,   960,  1540,    76,   378,     0,     0,
       0,     0,     0,   379,    78,    79,   380,   381,   382,   383,
       0,     0,     0,     0,   366,   367,     0,   368,     0,   369,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   370,
     371,   359,     0,   372,   373,   374,   362,   375,   376,     0,
     363,     0,   364,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   365,
       0,     0,     0,     0,     0,   377,   799,     0,    76,   378,
       0,     0,     0,     0,     0,   379,    78,    79,   380,   381,
     382,   383,     0,     0,     0,     0,   366,   367,     0,   368,
       0,   369,    63,    64,    65,    66,    67,    68,    69,    70,
      71,   370,   371,   359,     0,   372,   373,   374,   362,   375,
     376,     0,   363,     0,   364,     0,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   365,     0,     0,     0,     0,     0,   377,     0,     0,
      76,   378,     0,     0,     0,   280,     0,   379,    78,    79,
     380,   381,   382,   383,     0,     0,     0,     0,   366,   367,
       0,   368,     0,   369,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   370,   371,   359,     0,   372,   373,   374,
     362,   375,   376,     0,   363,     0,   364,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   365,     0,     0,     0,     0,     0,   377,
     960,     0,    76,   378,     0,     0,     0,     0,     0,   379,
      78,    79,   380,   381,   382,   383,     0,     0,     0,     0,
     366,   367,     0,   368,     0,   369,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   370,   371,   359,     0,   372,
     373,   374,   362,   375,   376,     0,   363,     0,   364,     0,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   365,     0,     0,     0,     0,
       0,   377,     0,     0,    76,   378,     0,     0,   991,     0,
       0,   379,    78,    79,   380,   381,   382,   383,     0,     0,
       0,     0,   366,   367,     0,   368,     0,   369,    63,    64,
      65,    66,    67,    68,    69,    70,    71,   370,   371,   359,
       0,   372,   373,   374,   362,   375,   376,     0,   363,     0,
     364,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   365,     0,     0,
       0,     0,     0,   377,     0,     0,    76,   378,     0,     0,
       0,  1212,     0,   379,    78,    79,   380,   381,   382,   383,
       0,     0,     0,     0,   366,   367,     0,   368,     0,   369,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   370,
     371,   359,     0,   372,   373,   374,   362,   375,   376,     0,
     363,     0,   364,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   365,
       0,     0,     0,     0,     0,   377,  1304,     0,    76,   378,
       0,     0,     0,     0,     0,   379,    78,    79,   380,   381,
     382,   383,     0,     0,     0,     0,   366,   367,     0,   368,
       0,   369,    63,    64,    65,    66,    67,    68,    69,    70,
      71,   370,   371,   359,     0,   372,   373,   374,   362,   375,
     376,     0,   363,     0,   364,     0,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   365,     0,     0,     0,     0,     0,   377,     0,     0,
      76,   378,     0,     0,     0,  1364,     0,   379,    78,    79,
     380,   381,   382,   383,     0,     0,     0,     0,   366,   367,
       0,   368,     0,   369,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   370,   371,   359,     0,   372,   373,   374,
     362,   375,   376,     0,   363,     0,   364,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   365,     0,     0,     0,     0,     0,   377,
       0,  1805,    76,   378,     0,     0,     0,     0,     0,   379,
      78,    79,   380,   381,   382,   383,     0,     0,     0,     0,
     366,   367,     0,   368,     0,   369,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   370,   371,   359,     0,   372,
     373,   374,   362,   375,   376,     0,   363,     0,   364,     0,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   365,     0,     0,     0,     0,
       0,   377,  1810,     0,    76,   378,     0,     0,     0,     0,
       0,   379,    78,    79,   380,   381,   382,   383,     0,     0,
       0,     0,   366,   367,     0,   368,     0,   369,    63,    64,
      65,    66,    67,    68,    69,    70,    71,   370,   371,   359,
       0,   372,   373,   374,   362,   375,   376,     0,   363,     0,
     364,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   365,     0,     0,
       0,     0,     0,   377,  1819,     0,    76,   378,     0,     0,
       0,     0,     0,   379,    78,    79,   380,   381,   382,   383,
       0,     0,     0,     0,   366,   367,     0,   368,     0,   369,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   370,
     371,   359,     0,   372,   373,   374,   362,   375,   376,     0,
     363,     0,   364,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   365,
       0,     0,     0,     0,     0,   377,  1896,     0,    76,   378,
       0,     0,     0,     0,     0,   379,    78,    79,   380,   381,
     382,   383,     0,     0,     0,     0,   366,   367,     0,   368,
       0,   369,    63,    64,    65,    66,    67,    68,    69,    70,
      71,   370,   371,   359,     0,   372,   373,   374,   362,   375,
     376,     0,   363,     0,   364,     0,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   365,     0,     0,     0,     0,     0,   377,  1898,     0,
      76,   378,     0,     0,     0,     0,     0,   379,    78,    79,
     380,   381,   382,   383,     0,     0,     0,     0,   366,   367,
       0,   368,     0,   369,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   370,   371,   359,     0,   372,   373,   374,
     362,   375,   376,     0,   363,     0,   364,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   365,     0,     0,     0,     0,     0,   377,
    1949,     0,    76,   378,     0,     0,     0,     0,     0,   379,
      78,    79,   380,   381,   382,   383,     0,     0,     0,     0,
     366,   367,     0,   368,     0,   369,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   370,   371,   359,     0,   372,
     373,   374,   362,   375,   376,     0,   363,     0,   364,     0,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   365,     0,     0,     0,     0,
       0,   377,  1951,     0,    76,   378,     0,     0,     0,     0,
       0,   379,    78,    79,   380,   381,   382,   383,     0,     0,
       0,     0,   366,   367,     0,   368,     0,   369,    63,    64,
      65,    66,    67,    68,    69,    70,    71,   370,   371,   359,
       0,   372,   373,   374,   362,   375,   376,     0,   363,     0,
     364,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   365,     0,     0,
       0,     0,     0,   377,  1953,     0,    76,   378,     0,     0,
       0,     0,     0,   379,    78,    79,   380,   381,   382,   383,
       0,     0,     0,     0,   366,   367,     0,   368,     0,   369,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   370,
     371,   359,     0,   372,   373,   374,   362,   375,   376,     0,
     363,     0,   364,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   365,
       0,     0,     0,     0,     0,   377,  1956,     0,    76,   378,
       0,     0,     0,     0,     0,   379,    78,    79,   380,   381,
     382,   383,     0,     0,     0,     0,   366,   367,     0,   368,
       0,   369,    63,    64,    65,    66,    67,    68,    69,    70,
      71,   370,   371,   359,     0,   372,   373,   374,   362,   375,
     376,     0,   363,     0,   364,     0,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   365,     0,     0,     0,     0,     0,   377,  1958,     0,
      76,   378,     0,     0,     0,     0,     0,   379,    78,    79,
     380,   381,   382,   383,     0,     0,     0,     0,   366,   367,
       0,   368,     0,   369,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   370,   371,   359,     0,   372,   373,   374,
     362,   375,   376,     0,   363,     0,   364,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   365,     0,     0,     0,     0,     0,   377,
    2000,     0,    76,   378,     0,     0,     0,     0,     0,   379,
      78,    79,   380,   381,   382,   383,     0,     0,     0,     0,
     366,   367,     0,   368,     0,   369,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   370,   371,   359,     0,   372,
     373,   374,   362,   375,   376,     0,   363,     0,   364,     0,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   365,     0,     0,     0,     0,
       0,   377,  2002,     0,    76,   378,     0,     0,     0,     0,
       0,   379,    78,    79,   380,   381,   382,   383,     0,     0,
       0,     0,   366,   367,     0,   368,     0,   369,    63,    64,
      65,    66,    67,    68,    69,    70,    71,   370,   371,   359,
       0,   372,   373,   374,   362,   375,   376,     0,   363,     0,
     364,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   365,     0,     0,
       0,     0,     0,   377,  2004,     0,    76,   378,     0,     0,
       0,     0,     0,   379,    78,    79,   380,   381,   382,   383,
       0,     0,     0,     0,   366,   367,     0,   368,     0,   369,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   370,
     371,   359,     0,   372,   373,   374,   362,   375,   376,     0,
     363,     0,   364,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   365,
       0,     0,     0,     0,     0,   377,  2027,     0,    76,   378,
       0,     0,     0,     0,     0,   379,    78,    79,   380,   381,
     382,   383,     0,     0,     0,     0,   366,   367,     0,   368,
       0,   369,    63,    64,    65,    66,    67,    68,    69,    70,
      71,   370,   371,   359,     0,   372,   373,   374,   362,   375,
     376,     0,   363,     0,   364,     0,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   365,     0,     0,     0,     0,     0,   377,  2029,     0,
      76,   378,     0,     0,     0,     0,     0,   379,    78,    79,
     380,   381,   382,   383,     0,     0,     0,     0,   366,   367,
       0,   368,     0,   369,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   370,   371,   359,     0,   372,   373,   374,
     362,   375,   376,     0,   363,     0,   364,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   365,     0,     0,     0,     0,     0,   377,
    2031,     0,    76,   378,     0,     0,     0,     0,     0,   379,
      78,    79,   380,   381,   382,   383,     0,     0,     0,     0,
     366,   367,     0,   368,     0,   369,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   370,   371,   359,     0,   372,
     373,   374,   362,   375,   376,     0,   363,     0,   364,     0,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   365,     0,     0,     0,     0,
       0,   377,     0,     0,    76,   378,     0,     0,     0,     0,
       0,   379,    78,    79,   380,   381,   382,   383,     0,     0,
       0,     0,   366,   367,     0,   368,     0,   369,    63,    64,
      65,    66,    67,    68,    69,    70,    71,   370,   371,   359,
       0,   372,   373,   374,   362,   375,   376,     0,   363,     0,
     364,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   365,     0,     0,
       0,     0,     0,   651,     0,     0,    76,   378,     0,     0,
       0,     0,     0,   379,    78,    79,   380,   381,   382,   383,
       0,     0,     0,     0,   366,   367,     0,   368,     0,   369,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   370,
     371,   359,     0,   372,   373,   374,   362,   375,   376,     0,
     363,     0,   364,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   365,
       0,     0,     0,     0,     0,   656,     0,     0,    76,   378,
       0,     0,     0,     0,     0,   379,    78,    79,   380,   381,
     382,   383,     0,     0,     0,     0,   366,   367,     0,   368,
       0,   369,    63,    64,    65,    66,    67,    68,    69,    70,
      71,   370,   371,   359,     0,   372,   373,   374,   362,   375,
     376,     0,   363,     0,   364,     0,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   365,     0,     0,     0,     0,     0,   662,     0,     0,
      76,   378,     0,     0,     0,     0,     0,   379,    78,    79,
     380,   381,   382,   383,     0,     0,     0,     0,   366,   367,
       0,   368,     0,   369,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   370,   371,   359,     0,   372,   373,   374,
     362,   375,   376,     0,   363,     0,   364,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   365,     0,     0,     0,     0,     0,   377,
       0,     0,    76,   378,     0,     0,     0,     0,     0,   379,
     867,    79,   380,   381,   382,   383,     0,     0,     0,     0,
     366,   367,     0,   368,     0,   369,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   370,   371,   359,     0,   372,
     373,   374,   362,   375,   376,     0,   363,     0,   364,     0,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   365,     0,     0,     0,     0,
       0,   377,     0,     0,    76,   378,     0,     0,     0,     0,
       0,   379,   440,    79,   380,   381,   382,   383,     0,     0,
       0,     0,   366,   367,     0,   368,     0,   369,  1891,    64,
      65,    66,    67,    68,    69,    70,    71,   370,   371,   359,
       0,   372,   373,   374,     0,   375,   376,     0,     0,     0,
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   377,     0,     0,    76,   378,     0,     0,
       0,     0,     0,   379,    78,    79,   380,   381,   382,   383,
      14,    15,    16,    17,    18,     0,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,  -453,  -453,     0,  -453,    46,    47,
       0,  -453,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    58
};

static const yytype_int16 yycheck[] =
{
       1,     1,     1,     4,   220,   151,   174,     4,   179,    74,
     163,   284,    83,   178,   242,   257,   163,   377,   679,   223,
      74,   966,   208,   163,   694,  1126,   876,    74,    74,   467,
    1288,  1289,   221,   617,    96,   220,  1739,   164,    76,   220,
      74,   220,   220,   220,    59,   608,   604,   140,   220,  1210,
     995,   604,  1670,   323,     1,    56,    57,   604,    59,    59,
      59,   672,  1670,     1,  1356,  1357,     4,  1670,     1,    74,
     340,     4,   179,    74,   344,   765,   767,   148,   785,     1,
     296,    74,    83,   765,    83,   862,   636,   291,   292,   340,
      91,   237,  1356,   344,   765,    96,   803,    96,    99,   192,
      99,  1671,   103,   738,   103,  1568,   103,   610,   297,   230,
     768,   296,    59,  1058,    83,   296,   774,   296,   296,   296,
     767,    59,   846,    71,   296,    74,    59,     1,   182,   516,
     517,   146,   765,     1,   255,   182,   182,    59,     1,    74,
     141,     0,   368,   144,   265,   146,   146,   146,   182,   873,
     151,    71,  1100,   116,  1110,   220,   157,  1105,    83,   221,
      71,    99,   869,   164,     1,   103,   220,    71,    97,   862,
     103,   441,   131,   220,   220,   246,   323,   182,    88,   148,
     181,   182,   181,     1,  1799,    59,   220,   325,    71,   182,
    1803,   154,   633,   527,    96,   196,    59,   196,   150,   146,
     152,   193,   150,   537,   767,   206,   154,   765,   146,   150,
     211,   180,   765,   146,   173,   220,    88,  1787,   765,   220,
     221,   150,   221,   317,   146,   150,   175,   220,  1176,   103,
     150,   296,   303,   174,   154,   297,   237,   275,   464,   150,
     175,    59,   296,   154,   578,   246,   150,   246,   158,   296,
     296,   638,  1715,   153,   924,   256,     1,   499,   259,     4,
     259,   377,   356,  1424,     0,   266,   769,    60,    61,   485,
     773,   154,   146,   175,   174,   276,   277,   246,   279,   782,
     783,    99,   952,   146,   284,   149,   158,   473,   156,   157,
       0,   564,   156,     4,   441,   296,   297,  1110,   297,  1591,
     485,    76,    77,   304,   485,   494,   485,   485,   485,   447,
     311,   312,   116,   485,    59,   316,  2019,    88,   445,    88,
     157,   259,  1937,   593,   784,   785,   132,  1591,   146,  1564,
     941,  1021,   692,   883,   303,   898,  1906,  1028,    83,  1021,
     411,  1118,   593,   803,   437,    56,    57,   617,   349,   790,
    1021,  1969,   150,   354,   323,   571,   357,   358,   745,   121,
     630,  1969,   284,  1070,  1022,   158,  1969,   917,   514,  1004,
    1147,  1327,  1328,  1329,   520,    20,  1324,  1990,  1102,   630,
      91,  1028,   175,   158,   573,   156,   571,   158,  1021,   158,
     571,   153,   571,   571,   571,   140,   150,  1655,  1656,   571,
    1970,   146,   150,   148,    71,   156,   212,    99,  1110,   869,
     284,    71,  2025,    57,  1246,   536,    60,    61,   110,    63,
     421,   284,   421,   365,   175,  1118,  1718,   465,   575,    71,
     141,   485,   494,   144,   150,   575,    76,    77,   485,   485,
    2010,   259,   411,   444,   445,  1940,   157,   192,   390,   391,
      71,   485,   150,   164,  1147,   456,   457,   132,   596,   156,
     152,   150,  1563,  1021,   465,  1028,   467,  1568,  1021,   411,
     617,  1966,   441,    71,  1021,   105,   106,   918,   175,   285,
     485,   150,   620,   150,   485,  1720,  1721,   154,   157,   627,
     150,   166,   485,   494,   154,   494,    71,  1992,   243,   441,
     211,   246,  1447,   156,   196,    71,   150,   325,   150,   843,
     132,   573,   154,   514,  1327,  1328,  1329,   571,   158,   520,
     151,    74,   175,   153,   571,   571,   158,   158,   229,   150,
     522,   232,   155,   154,    71,   651,    89,   653,   654,   284,
     656,  1199,  1045,   175,   166,   256,   662,   516,   517,   665,
     666,   667,   150,   254,   719,   266,   154,   558,   303,   560,
     998,   150,   577,   264,   564,   276,   277,   259,   279,   158,
     571,   377,   573,   156,   573,   150,   577,   577,   577,   154,
    1815,  1816,    71,  1875,   150,   156,   587,   156,   154,   158,
     591,    71,   175,   304,   594,  1256,    71,   289,    71,  1857,
     311,   312,   594,   295,   175,   316,   792,   778,   575,  1193,
    1070,  1903,   156,   150,  1715,   885,   175,   154,   789,  1277,
    1452,  1453,  1454,   624,   158,  1327,  1328,  1329,    71,   447,
     577,   175,   152,   325,   885,   636,   156,   150,   349,   577,
     767,   175,   564,   354,   577,   158,   357,   266,   617,  1941,
      13,    14,    15,    16,    17,   577,  1206,   156,   277,   805,
     825,   150,   147,  1356,  1357,   154,   411,   166,   132,   638,
     150,   778,  1522,   158,   154,   150,  1346,   150,   824,   154,
      74,   154,   720,   684,   152,   686,   154,   688,   562,   174,
     564,   692,   437,  1638,   695,  1640,    90,   161,   162,   156,
     506,   564,   156,   577,   156,   511,  1367,   150,    71,   126,
     127,   154,  1813,   107,   577,   156,   854,   156,   175,   720,
     156,   175,   528,   175,  1825,   166,   175,   104,   149,   721,
     166,   108,   538,   444,   111,   156,   113,     3,    13,    14,
      15,    16,    17,   561,   915,   456,   457,  1188,   156,   887,
     156,   156,   156,   170,   171,   447,   698,     3,   154,   577,
     166,   166,   762,   159,   765,   156,   767,   175,    83,   132,
     762,   175,   161,  1214,  1212,   166,   745,   153,   779,   168,
     169,    96,   909,  1444,    99,   786,   105,   106,   103,  1890,
     937,   792,   784,   785,   795,   152,    71,   937,  1185,   156,
    1238,    71,   620,   804,   805,   806,   507,   156,   915,    47,
      48,   803,    50,   558,   150,  1057,    54,   784,   785,   564,
     991,   158,  1263,   824,   144,   145,   146,    57,   529,   521,
      60,    61,   577,    63,   535,   158,   803,   158,   539,    13,
      14,    15,    16,    17,   152,   651,   166,   558,   156,   541,
     656,   150,   152,    71,   150,   175,   662,   132,   154,   860,
     861,   862,   862,   163,   164,   154,   181,   152,   152,   561,
     159,   156,   249,   157,   172,   681,   587,   869,   875,  1065,
     591,   196,   883,   354,   102,  1089,   357,   105,   106,   107,
     108,   109,   110,   111,   112,   113,  1067,    71,  1591,   152,
    1027,  1028,   869,   156,   596,   152,   221,   876,   909,   174,
     157,   150,   913,   624,   130,   862,   917,   144,   145,   146,
     105,   106,   923,  1193,   862,   636,  1364,   150,   620,   862,
     152,   246,   150,   151,   150,   627,   150,   875,   154,   166,
     862,   152,   875,   152,   259,   161,   162,   152,   157,   116,
     327,   328,   157,   330,   152,   332,    13,   958,   174,    13,
      14,    15,    16,    17,   152,   966,   152,   153,   660,   661,
     937,  1331,   152,   684,  1202,   686,   156,   688,   153,   154,
    1421,   692,   157,   152,   695,  1426,   152,   156,   862,   150,
     156,   933,   152,   154,   995,   152,   938,   998,   150,   862,
     163,   164,  1443,   156,  1442,   102,    22,   949,   150,   720,
     107,   108,   109,   110,   111,   112,   113,    71,  1186,  1187,
    1021,   150,    18,   156,   830,  1718,  1027,  1028,   152,   156,
      87,   156,   156,  1179,   156,   841,   854,   150,   844,  1110,
     150,   154,   848,   150,   862,   102,  1193,   865,   105,   106,
     107,   108,   109,   110,   111,   112,   113,  1058,    97,  1197,
      56,    57,    58,    59,    60,    61,    62,    63,   779,   887,
     128,   129,  1622,   158,  1221,   786,   130,  1319,  1070,    88,
     150,  1221,   152,  1356,   154,   150,   158,   152,  1259,   154,
     150,   158,   152,   804,   154,   806,   150,   152,   150,   149,
     154,     3,   154,  1070,   124,   125,   421,   161,   162,  1225,
     155,    13,    14,    15,    16,    17,   152,   862,  1118,  1120,
     156,   158,  1123,  1124,  1125,  1296,  1297,   155,   156,     3,
     875,   150,  1573,   152,   152,   154,  1289,   158,   156,    13,
      14,    15,    16,    17,   845,   158,  1147,  1147,  1337,   860,
     861,   862,  1153,   150,   152,  1974,   533,   154,   156,  1978,
    1161,   152,   854,  1164,  1165,  1164,  1165,  1168,  1165,    71,
    1840,  1118,   883,   865,   155,   156,  1118,   152,  1179,   494,
    1118,   156,  1875,   152,   174,  1118,   116,   156,   130,  1296,
    1297,  1632,   699,   700,   701,   887,  1118,    71,   909,   150,
    1147,   150,   913,  1753,   150,  1206,   917,   167,   150,  1147,
    1903,  1212,   154,   152,  1147,   130,  1185,   156,   130,   161,
     162,  1222,   155,   156,  1193,  1147,  1164,  1165,   156,   152,
     152,   162,  1165,   156,   156,   150,   152,  1238,   150,   154,
     156,   160,   154,   172,  1118,  1246,   161,   162,  1941,   161,
     162,  1193,   161,   162,  1221,  1118,  1327,  1328,  1329,  1406,
    1331,  1332,   706,   707,   708,   709,  1406,    13,    14,    15,
      16,    17,   152,  1147,   130,  1337,   156,  1278,    13,    14,
      15,    16,    17,    18,  1147,   152,  1228,  1229,  1230,   156,
     153,  1165,   152,  1235,  1236,   152,  1288,  1289,  1104,   156,
    1118,     4,     5,     6,     7,     8,     9,    10,    11,    12,
    1526,  1117,   152,  1754,   152,   152,   156,   152,  1591,   156,
     152,  1288,   144,   145,   146,    71,   152,   152,  1134,  1147,
     156,  1520,   152,  1330,   156,   154,  1337,   155,   152,  1528,
    1341,  1526,   156,  1344,   166,  1526,  1164,  1526,  1526,  1526,
     144,   145,   146,   175,  1526,   132,  1356,  1357,  1513,  1514,
    1515,    64,   156,  1364,   132,  1110,    13,    14,    15,    16,
      17,    18,   166,  1118,  1107,  1108,  1109,   157,    10,  1197,
     156,   175,   157,  1384,   130,  1386,   150,  1386,   155,   156,
     155,   156,  1330,   155,   156,   156,   157,  1330,   155,   156,
     155,   156,  1147,   152,   150,   155,   156,  1848,   154,  1120,
     155,   156,  1123,  1124,  1125,   161,   162,   150,  1356,  1357,
     152,   155,   156,  1356,  1357,  1590,    13,    14,    15,    16,
      17,   156,   157,  1430,  1356,  1357,  1147,   155,   156,  1406,
     155,  1442,  1153,   155,   156,   152,  1447,   152,  1386,  1602,
    1161,  1452,  1453,  1454,   152,  1602,   158,  1168,  1520,   155,
     156,  1526,  1602,   155,   156,   152,  1528,   155,   156,  1685,
     102,   154,  1164,   105,   106,   107,   108,   109,   110,   111,
     112,   113,  1356,  1357,    71,    13,    14,    15,    16,    17,
      18,   158,  1430,  1356,  1357,  1206,   158,  1430,   158,  1688,
    1685,   155,   156,  1656,  1685,  1197,  1685,  1685,  1685,   155,
     156,  1222,    69,  1685,  1630,   155,   156,   155,   156,  1520,
     109,   110,   111,   112,   113,  1526,   155,  1528,    13,    14,
      15,    16,    17,    18,  1535,   155,   156,   150,  1356,  1357,
      77,  1347,  1348,   130,    76,    77,  1538,   158,  1549,   156,
     157,  1247,  1248,  1522,   702,   703,   155,   704,   705,    18,
    1557,  1562,  1832,   150,   710,   711,   174,   154,  1386,  1734,
     156,  1533,  1534,   174,   161,   162,  1186,  1187,   158,  1385,
     150,   175,  1327,  1328,  1329,  1330,  1331,  1332,   152,   152,
     158,  1591,   158,   175,   155,   102,   155,    18,  1599,  1745,
     107,   108,   109,   110,   111,   112,   113,  1796,  1605,   149,
     152,  1356,  1357,   152,   152,   149,   152,   152,  1556,  1557,
      69,  1622,   152,   206,  1557,   152,  1688,   152,  1799,   158,
    1341,   152,   158,  1344,  1904,   158,   152,  1638,   152,  1640,
     152,   149,   175,   174,   151,   158,  1338,   154,   152,   152,
     156,   152,   152,  1591,  1596,   152,   152,   156,  1591,   152,
     152,   152,   152,  1655,  1656,   149,   155,  1605,   155,  1591,
     152,  1836,  1605,  1384,   152,   152,   152,   152,   152,    74,
     152,   152,   155,  1557,  1685,  1430,   152,  1688,  1655,   152,
     152,   174,  1799,   149,  1386,   152,   156,   150,  1699,   150,
     150,    96,  1703,   102,  1857,   150,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   150,  1717,  1591,  1718,   150,
      14,   157,    73,   156,    90,   157,  1727,   175,  1591,   175,
     155,   155,   175,   175,  1796,   149,   149,   175,  1556,   158,
     156,   152,   149,  1744,  1745,   155,   152,   156,   156,   152,
     152,   156,  1753,  1969,   155,   152,   151,   149,   175,   150,
    1906,  1832,   161,   175,  1935,   150,  1937,  1705,    79,  1907,
     150,   175,   149,  1591,   150,   175,  1582,   175,   152,   175,
    1718,  1970,   175,   156,  1969,  1718,   175,   175,  1969,   149,
    1969,  1969,  1969,   149,   156,  1796,  1718,  1969,   155,   149,
     155,   155,  1803,   152,   149,  1976,  1807,   155,   152,  1974,
     157,  1812,   158,  1978,  1979,   157,   119,   149,   152,   152,
     155,  2010,   152,   152,  1535,   220,   221,   155,  1935,   553,
    1937,   152,   149,  1904,   175,   157,  1837,  2008,  1549,   152,
     156,   150,   237,   108,  1718,   152,  1591,  2012,   156,  1164,
    1165,  1562,   150,   150,   155,  1718,   155,   149,   158,    10,
    1605,   149,   155,   149,  1556,  1857,   155,   152,   152,  1976,
     152,   152,  2037,   152,   152,  1875,  2041,  1878,    74,    74,
     175,  1882,   465,   149,   467,  2056,   150,  1705,  1599,   152,
     152,    89,   175,   154,  1895,   175,   155,  1839,  2063,   155,
    1718,   296,   297,  1903,  1969,  1906,   149,  1908,  1970,   149,
     152,  1622,    74,   152,   152,  1969,   152,   152,  1919,   152,
    1921,  1922,  1969,  1969,   102,     1,   153,   175,     4,   107,
     108,   109,   110,   111,   112,   113,   114,  1875,   166,    74,
     166,  1941,  1875,  1944,   175,   157,   175,   152,  2010,  2056,
     149,   102,   152,  1875,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   152,   152,  1903,   156,   149,  1969,  1970,
    1903,  1970,   151,  1718,   166,   166,   154,   149,   102,  1980,
     157,  1903,   150,    59,    74,   156,   150,   108,  1699,  1990,
     149,   166,  1703,   166,   151,   175,   108,   155,    74,   175,
     152,  1875,   151,  1941,   157,   152,  1717,    83,  1941,  2010,
     149,  2010,  1875,  1705,   152,   149,  1727,   150,    74,  1941,
      96,   175,  2023,    99,  2025,   152,   152,   103,  1630,  1903,
     175,   175,  1346,  1744,  1253,   671,   379,   712,   714,   713,
    1903,  1136,  1753,  2044,   715,   410,   716,     3,  1147,  2050,
     445,  2025,  1591,  1937,  1726,  1718,  2020,  1875,  1966,  2060,
    1841,  2019,  2007,  2064,   140,  1583,  1583,  1941,  1904,  2041,
     146,  1386,   148,  2074,  1979,   151,   152,  1903,  1941,  1168,
      49,   251,  1520,  1796,  1332,  1903,  1865,   163,  1534,   879,
     485,   792,  1803,  1161,  1605,   474,  1807,  1430,     0,   494,
     923,  1812,   737,   737,   180,   181,   182,   831,   587,   737,
      -1,    -1,    -1,    -1,    -1,    -1,   192,   193,    -1,   514,
     196,    -1,    -1,  1941,    -1,   520,  1837,    -1,    -1,    -1,
    1875,    -1,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    -1,    -1,    -1,   220,   221,   102,    -1,    18,   105,
     106,   107,   108,   109,   110,   111,   112,   113,  1903,    -1,
      -1,   237,    -1,    -1,    -1,   560,   890,  1878,    -1,    -1,
     246,  1882,    -1,    -1,   130,    -1,   571,   231,   573,    -1,
      -1,    -1,    -1,   259,  1895,    -1,    -1,    -1,    -1,    -1,
      60,    61,    62,    63,   150,   151,  1941,  1908,    -1,    -1,
      -1,   157,    -1,    -1,    -1,   161,   162,    -1,  1919,   792,
    1921,  1922,   795,   289,    -1,  1907,    -1,    -1,    -1,   295,
     296,   297,    -1,    -1,    -1,    -1,    77,   303,    -1,    -1,
    2036,    -1,   102,  1944,    -1,    -1,    -1,   107,   108,   109,
     110,   111,   112,   113,   114,    -1,  2052,   323,   324,   325,
      -1,   102,    -1,    -1,   105,   106,   107,   108,   109,   110,
     111,   112,   113,    -1,   340,    -1,    -1,    -1,   344,  1980,
      99,    -1,    -1,    -1,    -1,    -1,  1000,    -1,    -1,  1990,
     152,   110,    -1,   112,   154,   114,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1017,  1018,    -1,    -1,    -1,    -1,   102,
      -1,   377,    -1,   175,   107,   108,   109,   110,   111,   112,
     113,    -1,  2023,    -1,  2025,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   152,   175,   154,   155,   130,    -1,    -1,
      -1,    -1,    -1,  2044,    -1,   411,    -1,   831,   414,  2050,
      -1,    -1,    -1,    -1,    -1,   421,    -1,   150,   151,  2060,
      -1,   154,    -1,  2064,    -1,    -1,    -1,    -1,   161,   162,
     414,   437,    -1,  2074,    -1,   441,    -1,   196,    -1,   445,
     765,   447,   767,    -1,    -1,   958,   430,    -1,    -1,   433,
      -1,    -1,   102,   966,    -1,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   102,    -1,   890,   105,   106,   107,
     108,   109,   110,   111,   112,   113,    -1,    -1,    -1,   485,
     805,    -1,   995,    -1,    -1,   998,    -1,   102,   494,    -1,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   824,
     259,    -1,   261,   262,    -1,    -1,   490,    -1,   514,    -1,
     516,   517,    -1,    -1,   520,    -1,   522,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   175,    -1,    -1,    -1,    -1,
     289,    -1,    -1,    -1,    -1,    -1,   295,   175,   153,    -1,
      -1,    -1,   193,   158,    -1,  1058,    -1,    -1,    -1,    -1,
      -1,    -1,   102,    -1,   560,   105,   106,   107,   108,   109,
     110,   111,   112,   113,    -1,   571,   325,   573,    -1,   575,
      -1,   577,   331,    -1,   333,   102,  1000,    -1,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   593,   594,    -1,
     596,    -1,    -1,  1017,  1018,    -1,    -1,    -1,   604,    -1,
      -1,    -1,   608,    -1,    -1,    -1,    -1,    -1,   158,    -1,
      -1,   617,    -1,    -1,    -1,  1269,  1270,    -1,    -1,    -1,
      -1,   627,    -1,    -1,   630,    -1,    -1,    -1,    -1,  1283,
    1284,   158,   638,    -1,    -1,    -1,    64,    65,    66,    67,
      -1,    -1,    -1,    -1,    -1,   651,    -1,   653,   654,    -1,
     656,    -1,    -1,    -1,    -1,    -1,   662,    -1,    -1,   665,
     666,   667,   421,  1317,  1318,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   323,    -1,   102,   326,    -1,   105,   106,   107,
     108,   109,   110,   111,   112,   113,    -1,    -1,   447,   340,
     449,   450,    -1,   344,    -1,    -1,  1021,    -1,    -1,  1212,
     102,    -1,  1027,  1028,    -1,   107,   108,   109,   110,   111,
     112,   113,   114,    -1,    -1,   721,   118,    -1,   120,    -1,
      -1,    -1,    -1,    -1,    -1,  1238,   154,    -1,    -1,    -1,
      -1,   737,   738,  1246,    -1,   494,    -1,    -1,    -1,   745,
      -1,    -1,    -1,    -1,   172,    -1,    -1,    -1,    -1,   151,
      -1,    -1,   154,   737,   738,   514,   762,    -1,    -1,   765,
     519,   767,   521,   747,   102,    -1,   750,   105,   106,   107,
     108,   109,   110,   111,   112,   113,    -1,    -1,   784,   785,
      -1,    -1,   541,    -1,   543,   544,    -1,    -1,    -1,    -1,
     441,    -1,    -1,    -1,    -1,    -1,    -1,   803,    -1,   805,
      -1,   102,   561,    -1,    -1,    -1,   107,   108,   109,   110,
     111,   112,   113,   114,   573,    -1,   154,   118,   824,   120,
      13,    14,    15,    16,    17,    -1,    -1,   811,    -1,    -1,
      -1,   815,    -1,    -1,    -1,   819,    -1,   596,    -1,   598,
     599,    -1,    -1,    -1,    -1,  1269,  1270,    -1,  1502,  1503,
     151,  1364,    -1,   154,  1179,    -1,   862,    -1,    -1,  1283,
    1284,   620,   621,   869,    -1,    -1,    -1,    -1,   627,   875,
     876,   522,    -1,    -1,     1,    -1,    -1,     4,    71,   885,
      -1,   887,    -1,    -1,  1538,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   898,  1317,  1318,    -1,    -1,    -1,    -1,    -1,
      -1,   660,   661,    -1,    -1,    -1,    -1,    -1,    -1,   102,
      -1,    -1,   105,   106,   107,   108,   109,   110,   111,   112,
     113,    -1,    -1,    -1,   575,    -1,    -1,    -1,    -1,  1442,
      -1,   937,    59,    -1,  1447,    -1,    -1,   130,    -1,  1452,
    1453,  1454,   593,   594,    13,    14,    15,    16,    17,    -1,
      -1,    -1,    -1,  1278,    -1,    -1,    83,   150,   151,    -1,
    1614,   154,    -1,    -1,    -1,    -1,   617,    -1,   161,   162,
      -1,    -1,    99,    -1,    -1,    -1,   103,   102,    -1,   630,
      -1,    -1,   107,   108,   109,   110,   111,   112,   113,   114,
      -1,  1645,    -1,   118,    -1,   120,  1650,  1651,  1004,    -1,
     102,    -1,    71,   105,   106,   107,   108,   109,   110,   111,
     112,   113,  1337,   140,    -1,  1021,    -1,    -1,    71,   146,
    1004,   148,  1028,    -1,    -1,   152,   151,    -1,    -1,   154,
      -1,    -1,    -1,   102,    -1,   162,   163,   164,   107,   108,
     109,   110,   111,   112,   113,    -1,    -1,    -1,    -1,   102,
      -1,   153,    -1,   180,   107,   108,   109,   110,   111,   112,
     113,   130,    -1,    -1,  1070,   192,   193,    -1,    -1,   196,
     721,    -1,    -1,    -1,    -1,    -1,    -1,   130,  1502,  1503,
      -1,   150,   151,   119,    -1,   121,   122,   123,    -1,    -1,
      -1,  1075,   161,   162,  1078,   854,    -1,   150,   151,    -1,
      -1,    -1,    -1,    -1,  1110,    -1,   865,    -1,   161,   162,
      -1,   762,  1118,    -1,   150,    -1,   243,   153,   154,   246,
      -1,    -1,   158,   159,    -1,  1638,    -1,  1640,   887,    -1,
      -1,    -1,   259,    -1,    -1,    -1,    -1,    -1,    -1,   898,
      -1,  1147,    -1,    -1,    -1,   102,    -1,    -1,   907,   276,
     107,   108,   109,   110,   111,   112,   113,   284,  1164,  1165,
      -1,    -1,   289,    -1,    -1,    -1,    -1,    -1,   295,    -1,
      -1,    -1,    -1,  1179,    -1,    -1,   303,    -1,    -1,  1185,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1193,    -1,    -1,
    1614,    -1,    -1,   150,   151,  1520,   323,    -1,   325,   326,
      -1,  1526,    -1,  1528,     4,     5,     6,     7,     8,     9,
      10,    11,    12,   340,    -1,  1221,    -1,   344,    71,  1225,
      -1,  1645,    -1,    -1,    -1,    -1,  1650,  1651,    -1,    -1,
      -1,    -1,    -1,    -1,   885,    -1,    -1,   888,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1004,    -1,    -1,    -1,   102,
     377,    -1,   105,   106,   107,   108,   109,   110,   111,   112,
     113,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1028,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   130,    -1,    -1,
      -1,  1265,  1288,  1289,   411,    -1,   937,    -1,    -1,    -1,
    1274,    -1,    -1,    -1,    -1,    -1,    -1,   150,   151,    -1,
      -1,   154,    -1,    -1,    -1,    -1,    71,    -1,   161,   162,
     437,    -1,    -1,    -1,   441,    -1,    -1,    -1,    -1,    -1,
     447,  1327,  1328,  1329,  1330,  1331,  1332,    -1,    -1,    -1,
      -1,  1337,  1338,  1987,    -1,    -1,    -1,   102,    -1,    -1,
     105,   106,   107,   108,   109,   110,   111,   112,   113,    -1,
    1356,  1357,    83,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1685,    -1,   100,  1688,   102,   130,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,    -1,    -1,    -1,    -1,
    1386,    -1,    -1,    -1,    -1,   150,   151,    -1,    -1,   516,
     517,    -1,    -1,    -1,   521,   522,   161,   162,    -1,    -1,
    1406,    -1,    -1,    -1,    -1,  1164,    -1,    -1,    -1,    -1,
      -1,    -1,   150,    -1,    -1,   153,   154,   148,    -1,    -1,
    1745,    -1,    -1,    -1,  1430,    -1,   553,    -1,    -1,    -1,
      -1,   558,   163,    -1,   561,   562,    -1,   564,  1197,    -1,
      -1,    -1,    -1,    -1,  1203,    -1,    -1,   102,   575,   180,
     577,    -1,   107,   108,   109,   110,   111,   112,   113,   114,
      -1,    -1,   193,   118,   591,   120,   593,   594,    -1,   596,
      -1,  1796,    -1,    -1,    -1,    -1,   102,    -1,    -1,   105,
     106,   107,   108,   109,   110,   111,   112,   113,    -1,    -1,
     617,    -1,    -1,   620,    -1,    -1,   151,   624,    -1,    -1,
     627,    -1,    -1,   630,    -1,   632,    -1,    -1,    -1,    -1,
    1516,   638,    -1,    -1,  1520,   246,  1522,    -1,    71,    -1,
    1526,    -1,  1528,    -1,   651,   151,   653,   654,   154,   656,
      -1,    -1,  1516,    -1,    -1,   662,    -1,    -1,   665,   666,
     667,    -1,  1193,    -1,    -1,    -1,    -1,    -1,    -1,   102,
    1556,  1557,   105,   106,   107,   108,   109,   110,   111,   112,
     113,    -1,    -1,  1987,    -1,    -1,    -1,    -1,    -1,    -1,
    1221,    -1,   303,    -1,    -1,    -1,    -1,   130,    -1,  1338,
      -1,  1906,    -1,    -1,    -1,  1591,    -1,    -1,    -1,    -1,
      -1,    -1,   323,    -1,   721,    -1,  1602,   150,   151,  1605,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   161,   162,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   745,    -1,
      -1,    -1,    -1,    -1,  1630,    -1,    -1,  1386,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   762,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1969,  1970,    -1,    -1,    -1,  1655,
    1656,    -1,    -1,    -1,    -1,    -1,    -1,   784,   785,    -1,
      -1,    -1,    -1,   102,  1670,  1671,   105,   106,   107,   108,
     109,   110,   111,   112,   113,    -1,   803,    -1,    -1,  1685,
     411,    -1,  1688,    -1,    -1,  2010,  1670,  1671,   133,   134,
     135,   136,   137,   138,   139,   140,   141,   142,   143,  1705,
      -1,    -1,   147,    -1,   831,    -1,    -1,    -1,    -1,    -1,
     441,    -1,  1718,    -1,   153,    -1,    -1,    -1,    -1,    13,
      14,    15,    16,    17,    -1,    -1,    -1,   854,    -1,   174,
      -1,    -1,    -1,    -1,    -1,   862,    -1,    -1,   865,  1745,
      -1,    -1,   869,    -1,    -1,    -1,    -1,    -1,   875,   876,
      -1,    -1,    -1,    -1,    -1,  1406,    -1,    -1,   885,    -1,
     887,   888,   102,   890,    -1,   105,   106,   107,   108,   109,
     110,   111,   112,   113,    -1,    -1,    -1,    71,    -1,    -1,
       1,  1787,    -1,     4,    -1,   516,   517,    -1,    -1,    -1,
    1796,   522,    -1,    -1,    -1,    -1,    -1,  1556,    -1,    -1,
      -1,    -1,    -1,  1787,    -1,    -1,    -1,    -1,   102,    -1,
     937,   105,   106,   107,   108,   109,   110,   111,   112,   113,
      -1,    -1,    -1,    -1,   102,    -1,  1832,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   130,    -1,    59,    -1,
      -1,    -1,    -1,    -1,   575,    -1,    -1,    -1,    -1,    -1,
      -1,  1857,    -1,    -1,    -1,    -1,   150,   151,    -1,  1865,
     154,    -1,    83,   594,    -1,    -1,    -1,   161,   162,  1875,
      -1,    -1,   150,  1000,    -1,    71,    -1,  1861,    -1,    -1,
      -1,  1865,   103,    -1,    -1,    -1,   617,    -1,    -1,    -1,
    1017,  1018,    -1,    -1,    -1,    -1,    -1,  1903,  1904,    -1,
    1906,  1907,    -1,    -1,    -1,    -1,   102,   638,    -1,   105,
     106,   107,   108,   109,   110,   111,   112,   113,    -1,   140,
      -1,    -1,  1906,    -1,    -1,   146,    -1,    -1,    -1,  1688,
      -1,    -1,    -1,    -1,   130,  1941,    -1,    -1,    -1,    -1,
      -1,    -1,   163,  1070,    -1,    -1,  1705,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   150,   151,    -1,    -1,    -1,     1,
     181,    -1,    -1,  1969,  1970,   161,   162,    -1,    -1,    -1,
     102,   192,   193,   105,   106,   107,   108,   109,   110,   111,
     112,   113,    -1,  1110,    -1,  1969,  1970,    -1,    -1,    -1,
     721,  1118,    -1,    -1,    -1,    -1,    -1,    -1,   130,    -1,
     221,    -1,    -1,    -1,  2010,    -1,  1120,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   745,    -1,   237,    59,   150,   151,
    1147,   242,   243,    -1,    -1,   246,  2010,    -1,  1787,   161,
     162,   762,    -1,    -1,    -1,    -1,    -1,  1164,  1165,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   268,    -1,    -1,
     271,    -1,   273,   784,   785,    -1,    -1,    -1,  1185,    -1,
      -1,   103,    -1,   284,    -1,    -1,  1193,    -1,    -1,    -1,
    1197,    -1,   803,    -1,    -1,    -1,   297,    -1,    -1,    -1,
     102,    -1,  1209,   105,   106,   107,   108,   109,   110,   111,
     112,   113,    -1,    -1,  1221,    -1,    -1,    -1,  1225,    -1,
      -1,    -1,   323,    -1,   146,   326,   102,    -1,   130,   105,
     106,   107,   108,   109,   110,   111,   112,   113,    -1,   340,
      -1,   163,    -1,   344,    -1,    -1,    -1,    -1,   150,   151,
      -1,    -1,   154,    -1,   130,    -1,    -1,    -1,   869,   161,
     162,    -1,  1269,  1270,    -1,   876,    -1,   368,  1907,    -1,
      -1,   193,   174,   377,   150,   151,  1283,  1284,   154,    -1,
      -1,  1288,  1289,    -1,    -1,   161,   162,    -1,    -1,    13,
      14,    15,    16,    17,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1832,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1317,  1318,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1327,  1328,  1329,  1330,  1331,  1332,   937,    -1,    -1,    -1,
      -1,  1338,    -1,    -1,    -1,    -1,   437,    -1,    -1,    -1,
     441,    -1,    -1,    -1,    -1,    -1,   268,    71,    -1,  1356,
    1357,    13,    14,    15,    16,    17,    -1,    -1,    -1,    -1,
      -1,    -1,   284,   464,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2010,    -1,  1904,    -1,    -1,    -1,    -1,   102,  1386,
      -1,   105,   106,   107,   108,   109,   110,   111,   112,   113,
    1384,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1406,
      -1,   323,    -1,    -1,   326,    -1,   130,    -1,    -1,    71,
      -1,    -1,   516,   517,    -1,    -1,    -1,    -1,   340,    -1,
      -1,   522,   344,  1430,    -1,   102,   150,   151,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   161,   162,    -1,
     102,    -1,    -1,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   553,    -1,    -1,    -1,    -1,   558,    -1,  1070,
      -1,   562,    -1,   564,    -1,    -1,    -1,    -1,   130,    -1,
      -1,    -1,    -1,   150,   575,    -1,   577,    -1,    -1,    56,
      57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   150,   151,
      -1,    -1,   593,   594,    -1,  1502,  1503,    -1,    -1,   161,
     162,    -1,    -1,    -1,    -1,    -1,    -1,   608,    -1,    -1,
      -1,    -1,    -1,    -1,    91,  1522,   617,    -1,    -1,   441,
      -1,   622,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   630,
      -1,  1538,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   651,    -1,  1556,
    1557,    -1,   656,    -1,    -1,  1549,    -1,    -1,   662,    -1,
      -1,    -1,    -1,    -1,   141,    -1,    -1,   144,  1562,    -1,
      -1,    -1,    -1,    -1,  1185,    -1,    -1,   681,    -1,    -1,
     157,    -1,  1193,    -1,  1591,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1602,    -1,    -1,  1605,    -1,
     522,    -1,    -1,    -1,    -1,  1599,    -1,  1614,    -1,    -1,
    1221,    -1,    -1,   717,    -1,    -1,    -1,    -1,    -1,    -1,
     721,    -1,    -1,  1630,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   553,    -1,    -1,   211,    -1,    -1,   738,  1645,    -1,
     562,    -1,   564,  1650,  1651,    -1,    -1,    -1,  1655,  1656,
      -1,    -1,    -1,   575,    -1,   577,    -1,    -1,    -1,    -1,
      -1,   762,    -1,    -1,    -1,    -1,   767,    -1,    -1,    -1,
      -1,   593,   594,    -1,    -1,    -1,    -1,  1288,  1289,   256,
      -1,    -1,    -1,   784,   785,    -1,    -1,    -1,    -1,   266,
      -1,    -1,    -1,    -1,    -1,   617,    -1,    -1,  1705,    -1,
     277,    -1,   803,    -1,    -1,  1699,    -1,    -1,   630,  1703,
      -1,  1718,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1717,    -1,    -1,    -1,   304,    -1,    -1,
     831,    -1,    -1,  1727,   311,   312,    -1,   102,    -1,   316,
     105,   106,   107,   108,   109,   110,   111,   112,   113,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,
      -1,   862,    -1,    -1,    -1,   130,    -1,    -1,   869,    -1,
      -1,    -1,   349,    -1,   875,    -1,    -1,   354,    -1,    -1,
     357,    -1,    -1,    -1,   885,   150,   151,   888,    -1,   890,
      -1,    -1,   157,    -1,   895,  1406,   161,   162,    -1,   721,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1803,
      -1,    -1,    -1,  1807,    -1,    -1,    -1,    59,  1812,    -1,
      -1,    -1,    -1,   102,    -1,  1832,   105,   106,   107,   108,
     109,   110,   111,   112,   113,    -1,   937,    -1,    -1,    -1,
     762,    -1,    -1,  1837,    -1,    -1,    -1,    -1,    -1,    -1,
    1857,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   103,   784,   785,    -1,    -1,    -1,   444,  1875,    -1,
      -1,   150,   151,    -1,    -1,    -1,    -1,    -1,    -1,   456,
     457,   803,   161,   162,  1878,    -1,    -1,    -1,  1882,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1903,  1904,    -1,  1000,
    1907,  1895,    -1,    -1,   146,    -1,    -1,    -1,    -1,   831,
      -1,  1522,    -1,    -1,    -1,    -1,  1017,  1018,    -1,    -1,
      -1,   163,    -1,    -1,    -1,  1919,    -1,  1921,  1922,    -1,
      -1,    -1,    -1,    -1,  1941,    -1,    -1,    -1,    -1,    -1,
     862,    -1,    -1,    -1,    -1,    -1,    -1,   869,    -1,    -1,
    1944,   193,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   885,    -1,    -1,   888,    -1,   890,  1070,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1987,    -1,    -1,    -1,    -1,    -1,  1980,    -1,    -1,    -1,
      -1,  1602,    -1,    -1,    -1,    -1,  1990,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1110,
     587,    -1,    -1,    -1,    -1,   937,    -1,  1118,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2023,
      -1,  2025,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   284,    -1,  1655,  1656,  1147,    -1,    -1,    -1,
    2044,    -1,    -1,    -1,    -1,    -1,  2050,    -1,    -1,   636,
      -1,    -1,    -1,    -1,  1165,    -1,  2060,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1000,    -1,
      -1,   323,    -1,    -1,   326,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1193,    -1,    -1,  1017,  1018,    -1,   340,    -1,
      -1,  1202,   344,    -1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
    1221,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,  1070,    -1,
      -1,    -1,    51,    52,    -1,    -1,    -1,    56,    57,    58,
      59,    60,    61,    62,    63,    -1,    -1,    -1,  1269,  1270,
      -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1283,  1284,    -1,    -1,    -1,  1288,  1289,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1118,    -1,    -1,   441,
      -1,    -1,   779,    -1,    -1,    -1,   105,   106,    -1,   786,
      -1,    -1,    -1,    -1,    -1,    -1,  1317,  1318,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1147,  1327,  1328,  1329,  1330,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1165,    -1,    -1,  1857,    -1,    -1,  1353,
      -1,    -1,  1356,  1357,   153,  1356,  1357,    -1,  1362,    -1,
      -1,    -1,  1366,    -1,  1368,    -1,    -1,    -1,    -1,    -1,
      -1,  1193,    -1,    -1,    -1,    -1,     1,    -1,    -1,     4,
     522,    -1,    -1,    -1,   861,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1221,
      -1,    -1,    -1,    -1,    -1,  1406,   883,    -1,    -1,    -1,
      -1,   553,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     562,    -1,   564,    -1,    -1,    -1,    -1,    -1,    -1,  1430,
      -1,    -1,    -1,   575,    59,   577,   913,    -1,    -1,    -1,
     917,    -1,    -1,    -1,    -1,    -1,    -1,  1269,  1270,    -1,
      -1,   593,   594,    -1,    -1,    -1,    -1,    -1,    83,    -1,
      -1,  1283,  1284,    -1,    -1,    -1,  1288,  1289,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   617,    -1,    -1,   103,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   630,    -1,
      -1,    -1,    -1,    -1,    -1,  1317,  1318,    -1,    -1,    -1,
      -1,  1502,  1503,    -1,  1508,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   140,    -1,    -1,    -1,    -1,
      -1,   146,    -1,   148,    -1,    -1,    -1,  1528,    -1,    -1,
      -1,    -1,    -1,    -1,  1356,  1357,  1540,  1538,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1552,    -1,
      -1,    -1,    -1,    -1,    -1,   180,  1557,    -1,    -1,    -1,
      -1,    -1,    -1,  1567,  1568,    -1,    -1,   192,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   721,
      -1,    -1,    -1,    -1,  1406,    -1,    -1,  1591,    -1,    -1,
    1591,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1602,    -1,    -1,  1605,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1614,    -1,    -1,    -1,    -1,   243,    -1,
     762,   246,    -1,    -1,    -1,    -1,   251,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   784,   785,  1645,    -1,  1123,  1124,  1125,  1650,
    1651,    -1,    -1,    -1,  1655,  1656,    -1,    -1,    -1,   284,
      -1,   803,    -1,     1,    -1,    -1,     4,    -1,    -1,    -1,
    1671,    -1,    -1,    -1,    -1,    -1,  1153,    -1,   303,    -1,
    1502,  1503,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   831,
      -1,  1168,    -1,    -1,    -1,    -1,    -1,  1701,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1710,    -1,  1712,    -1,
      -1,  1715,  1716,    -1,  1718,    -1,  1538,  1718,    -1,  1723,
     862,    59,    -1,    -1,    -1,    -1,    -1,   869,    -1,  1206,
      -1,    -1,    -1,    -1,    -1,  1557,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   885,    -1,    83,   888,    -1,   890,    -1,
      -1,    -1,   377,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,  1591,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1602,    -1,    -1,    -1,    -1,    -1,   411,    -1,    -1,    -1,
      -1,    -1,  1614,    -1,    -1,   937,    -1,    -1,    -1,    -1,
      -1,    -1,   140,    -1,    -1,    -1,    -1,    -1,   146,    -1,
     148,    -1,   437,    -1,  1818,    -1,    -1,    -1,    -1,  1823,
    1824,    -1,    -1,  1645,    -1,    -1,    -1,    -1,  1650,  1651,
      -1,  1832,    -1,  1655,  1656,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   180,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   192,    -1,  1857,    -1,  1000,    -1,
      -1,    -1,    -1,    -1,  1341,    -1,    -1,  1344,    -1,    -1,
      -1,    -1,    -1,    -1,  1875,  1017,  1018,    -1,    -1,  1883,
      -1,  1885,    -1,    -1,  1888,  1889,    -1,    -1,    -1,  1893,
    1894,   516,   517,    -1,    -1,    -1,  1718,    -1,    -1,    -1,
      -1,    -1,  1903,  1904,    -1,   243,    -1,    -1,   246,    -1,
      -1,    -1,    -1,   251,    -1,    -1,     1,    -1,    -1,     4,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1070,    -1,
      -1,    -1,    -1,   558,    -1,    -1,    -1,   562,    -1,   564,
    1941,    -1,    87,    -1,    -1,    -1,   284,    -1,    93,    94,
      -1,    -1,   577,    -1,    -1,    -1,    -1,    -1,  1962,  1963,
    1964,    -1,    -1,    -1,    -1,   303,    -1,    -1,    -1,  1970,
      -1,    -1,    -1,    -1,    59,    -1,  1118,    -1,    -1,  1983,
      -1,    -1,   127,    -1,    -1,    -1,  1987,    -1,    -1,    -1,
      -1,    -1,  1996,  1997,  1998,    -1,    -1,    -1,    83,    -1,
      -1,    -1,    -1,    -1,    -1,  1147,    -1,    -1,    -1,    -1,
    1832,    -1,    -1,   638,    -1,    -1,    -1,    -1,   103,    -1,
      -1,    -1,    -1,  1165,    -1,    -1,   651,    -1,   653,   654,
      -1,   656,    -1,    -1,    -1,  1857,    -1,   662,    -1,   377,
     665,   666,   667,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1193,    -1,  1875,    -1,   140,    -1,    -1,  1535,    -1,
      -1,   146,    -1,   148,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   411,    -1,    -1,    -1,    -1,    -1,  1221,
      -1,  1903,  1904,     1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   180,    -1,    -1,    -1,   437,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   192,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1941,
     745,    -1,    -1,    -1,    -1,    -1,    -1,  1269,  1270,    -1,
      -1,    49,    -1,    -1,    -1,    53,    -1,    55,    -1,    -1,
      -1,  1283,  1284,    -1,    -1,  1622,  1288,  1289,   293,    -1,
      -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,   243,    -1,
      -1,   246,    -1,    -1,    -1,  1987,   251,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1317,  1318,    -1,   516,   517,
      -1,    99,   100,    -1,   102,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   284,
     118,   119,   120,    -1,   122,   123,    -1,    -1,    -1,    -1,
      -1,    -1,   130,    -1,  1356,  1357,    -1,    -1,   303,    -1,
     558,    -1,    -1,    -1,   562,    -1,   564,    -1,    -1,    -1,
      -1,    -1,   150,    -1,    -1,   153,   154,   862,    -1,   577,
      -1,    -1,   160,   161,   162,   163,   164,   165,   166,    -1,
     875,   876,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1406,    -1,    -1,  1744,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1753,    -1,    -1,    -1,
      13,    14,    15,    16,    17,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   377,    -1,    -1,   440,    -1,   442,    -1,    -1,
     638,    -1,    -1,    -1,    -1,    -1,   451,   452,    -1,    -1,
      -1,    -1,    -1,   651,    -1,   653,   654,    -1,   656,    -1,
      -1,    -1,    -1,    -1,   662,    -1,   411,   665,   666,   667,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   437,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1502,  1503,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   102,
      -1,    -1,   105,   106,   107,   108,   109,   110,   111,   112,
     113,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1538,   130,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   745,    -1,    -1,
      -1,    -1,    -1,    -1,   559,  1557,    -1,   150,   151,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   161,   162,
      -1,   516,   517,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1591,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1602,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1614,   558,    -1,    -1,    -1,   562,    -1,   564,
      -1,    -1,    -1,    -1,    -1,  1110,    -1,    -1,    -1,    -1,
      -1,    -1,   577,  1118,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1645,    -1,    -1,    -1,     5,  1650,  1651,
      -1,    -1,    -1,  1655,  1656,    13,    14,    15,    16,    17,
      -1,    -1,  1147,    -1,   862,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   875,   876,    -1,
    1165,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    49,    -1,   638,    -1,    53,    -1,    55,    -1,    -1,
    1185,    -1,    -1,    -1,    -1,    -1,   651,    -1,   653,   654,
      -1,   656,    -1,    71,    72,    -1,  1718,   662,    -1,    -1,
     665,   666,   667,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1225,    99,   100,    -1,   102,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,    -1,
     118,   119,   120,    -1,   122,   123,    -1,    -1,    -1,    -1,
      -1,    -1,   130,   778,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   150,    -1,    -1,   153,   154,    -1,    -1,    -1,
     745,    -1,   160,   161,   162,   163,   164,   165,   166,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1832,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1327,  1328,  1329,  1330,  1331,  1332,    -1,    -1,
     855,   856,    -1,    -1,    -1,  1857,    -1,    -1,    -1,    -1,
      -1,   866,   867,   868,    -1,    -1,   871,    -1,    -1,    -1,
      -1,  1356,  1357,  1875,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1903,  1904,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1110,    -1,    -1,    -1,    -1,   862,    -1,    -1,
    1118,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     875,   876,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1941,
      -1,    -1,    -1,    -1,    -1,  1430,   951,    -1,    -1,  1147,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1165,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1987,    -1,  1185,    -1,    -1,
      -1,    -1,   997,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1225,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1522,    -1,  1044,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1053,  1054,
    1055,  1056,    -1,    -1,    -1,    -1,  1061,  1062,    -1,    -1,
      -1,    -1,    -1,    -1,   179,    -1,  1071,    -1,    -1,    -1,
       0,    -1,  1557,     3,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1092,    -1,  1094,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1591,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1605,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1327,
    1328,  1329,  1330,  1331,  1332,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1147,    -1,    -1,  1630,    -1,    77,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1356,  1357,
      -1,    -1,    -1,    -1,    -1,  1110,    -1,    -1,  1173,    -1,
      -1,    -1,    -1,  1118,    -1,  1180,    -1,  1182,  1183,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1192,    -1,  1194,
      -1,  1196,    -1,  1198,    -1,    -1,    -1,    -1,  1203,    -1,
      -1,    -1,  1147,    -1,    -1,    -1,   136,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1165,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1430,  1718,    -1,    -1,    -1,    -1,    -1,    -1,
    1185,    -1,    -1,    -1,    -1,    -1,    -1,   362,    -1,    -1,
     365,   366,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1264,
     375,   376,    -1,    -1,    -1,    -1,  1271,  1272,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   390,   391,    -1,    -1,    -1,
    1225,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1295,    -1,    -1,    -1,    -1,    -1,   411,  1302,    -1,    -1,
     230,  1306,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   245,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1522,   255,   441,    -1,    -1,    -1,
      -1,  1336,    -1,    -1,    -1,   265,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   279,
     280,    -1,    -1,    -1,    -1,    -1,   286,   287,    -1,  1557,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1376,   302,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1327,  1328,  1329,  1330,  1331,  1332,    -1,    -1,
    1875,    -1,   322,  1591,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1410,    -1,  1605,    -1,    -1,
      -1,  1356,  1357,  1418,    -1,  1420,    -1,    -1,  1903,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1630,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   378,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1941,    -1,    -1,    -1,
      -1,    -1,  1467,  1468,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1482,  1483,   409,
    1485,    -1,    -1,    -1,    -1,  1430,    48,    -1,    -1,  1494,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1504,
    1505,    -1,    -1,    -1,    -1,   435,    -1,    -1,    -1,   439,
      -1,    -1,    74,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1718,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   458,    -1,
      -1,    -1,   462,   463,    -1,    -1,   466,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   481,   482,   483,   484,    -1,    -1,    -1,   120,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     500,   133,    -1,   135,    -1,    -1,    -1,  1522,   508,    -1,
      -1,    -1,    -1,   698,   699,   700,   701,   702,   703,   704,
     705,   706,   707,   708,   709,   710,   711,   712,   713,   714,
     715,   716,    -1,   165,    -1,    -1,   536,    -1,    -1,    -1,
    1615,  1616,  1557,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     182,  1626,    48,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   567,    -1,    -1,
      -1,    -1,    -1,    -1,   574,    -1,  1591,    -1,    -1,    -1,
     580,    -1,    -1,    -1,    -1,    -1,  1661,  1662,   220,    -1,
    1605,    -1,   224,   778,    -1,   227,   228,    -1,    -1,   231,
      -1,    -1,   234,   235,   604,   605,    -1,  1875,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1630,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   120,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1903,    -1,   133,    -1,   135,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   296,    -1,    -1,   299,   668,    -1,
      -1,    -1,  1747,  1941,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   320,   321,
      -1,  1766,    -1,    -1,  1769,  1770,    -1,    -1,    -1,    -1,
      -1,  1776,    -1,  1718,   336,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     915,   227,   228,    -1,    -1,   231,    -1,   737,   234,   235,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   933,    -1,
      -1,    -1,   752,   938,    -1,    -1,   756,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   949,   765,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   787,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   796,    -1,   430,    -1,
      -1,    -1,   802,    -1,    -1,    -1,   991,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   320,   321,    -1,    -1,    -1,    -1,
    1905,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   839,
     336,    -1,    -1,    -1,    -1,    -1,   846,    -1,    -1,    -1,
      -1,    -1,    -1,   485,    -1,    -1,    -1,    -1,    -1,    -1,
    1875,    -1,    -1,    -1,    -1,   497,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   873,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   163,    -1,    -1,    -1,    -1,    -1,    -1,  1903,    -1,
      -1,    -1,  1967,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     192,   193,    -1,    -1,    -1,    -1,  1991,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1941,   927,    -1,    -1,
      -1,  2006,    -1,  1118,   430,    -1,    -1,    -1,    -1,   571,
      -1,    -1,   224,    -1,    -1,    -1,  2021,    -1,    -1,   231,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     612,   613,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   625,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   497,    -1,    -1,    -1,    -1,    -1,    -1,  1193,    -1,
    1010,    -1,    -1,    -1,  1014,    -1,    -1,   299,    -1,    -1,
      -1,  1021,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1031,    -1,    -1,    -1,    -1,    -1,    -1,  1038,    -1,
      -1,   323,   324,  1228,  1229,  1230,    -1,  1047,    -1,  1049,
    1235,  1236,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   344,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1259,    -1,    -1,    -1,    -1,    -1,
      -1,  1081,    -1,    -1,    -1,  1085,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1099,
      -1,    -1,  1102,    -1,    -1,    -1,    -1,    -1,    -1,   741,
     742,  1296,  1297,    -1,    -1,   747,   612,   613,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   625,
      -1,    -1,   414,    -1,    -1,    -1,   768,    -1,    -1,   771,
     772,    -1,   774,    -1,   776,   777,    -1,    -1,   430,   431,
      -1,   433,   434,    -1,    -1,    -1,    -1,    -1,    -1,   441,
      -1,    -1,    -1,   445,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   815,    -1,    -1,    -1,   819,    -1,    -1,
      -1,  1191,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   486,    -1,    -1,    -1,   490,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1216,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     522,    -1,    -1,    -1,    -1,   741,   742,    -1,    -1,    -1,
      -1,   747,    -1,    -1,    -1,    -1,    -1,    -1,     5,    -1,
      -1,    -1,    -1,    -1,   896,    -1,    13,    14,    15,    16,
      17,    -1,   768,    -1,    -1,   771,   772,    -1,   774,    -1,
     776,   777,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     572,    -1,    -1,   575,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    49,    -1,    -1,    -1,    53,    -1,    55,  1309,
      -1,   593,   594,  1313,    -1,    -1,    -1,    -1,    -1,   815,
      -1,    -1,   604,   819,    71,    72,   608,    -1,    -1,    -1,
      -1,    -1,    -1,   615,    -1,   617,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1343,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    99,   100,    -1,   102,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
      -1,   118,   119,   120,    -1,   122,   123,    -1,    -1,    -1,
      -1,    -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1393,    -1,  1027,  1396,    -1,    -1,    -1,
     896,    -1,    -1,   150,    -1,    -1,   153,   154,    -1,    -1,
      -1,  1596,  1412,   160,   161,   162,   163,   164,   165,   166,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1068,    -1,    -1,   721,
      -1,    -1,    -1,  1075,    -1,    -1,  1078,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   737,   738,    -1,    -1,    -1,
      -1,    -1,    -1,  1463,    -1,   747,   748,    -1,   750,   751,
      -1,    -1,  1472,    -1,    -1,    -1,  1476,    -1,    -1,    -1,
     762,    -1,    -1,   765,    -1,   767,   768,    -1,    83,    -1,
    1490,  1491,   774,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   784,   785,    99,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   803,    -1,    -1,    -1,   807,    -1,    -1,    -1,   811,
      -1,  1027,    -1,   815,   816,    -1,    -1,   819,   820,    -1,
      -1,    -1,    -1,    -1,    -1,   827,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   148,    -1,    -1,    -1,   152,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1199,   163,    -1,
      -1,    -1,  1068,    -1,    -1,  1207,  1208,    -1,    -1,  1075,
      -1,    -1,  1078,    -1,    -1,   180,    -1,   869,   870,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   193,    -1,
      -1,   196,    -1,    -1,    -1,    -1,  1606,  1607,    -1,    -1,
      -1,    -1,    -1,    -1,  1799,    -1,   898,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1265,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1274,    -1,    -1,  1277,    -1,  1279,  1280,    -1,
      -1,   246,    -1,    -1,  1839,   937,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   259,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1320,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     295,    -1,    -1,  1199,    -1,    -1,    -1,   180,   303,    -1,
      -1,  1207,  1208,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     193,    -1,  1004,    -1,    -1,    -1,    -1,    -1,   323,    -1,
     325,    -1,  1732,   206,    -1,   208,    -1,    -1,    -1,  1021,
    1022,    -1,    -1,    -1,    -1,    -1,  1028,    -1,    -1,    -1,
    1935,    -1,  1937,    -1,    -1,  1387,  1756,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1265,
      -1,    -1,    -1,    -1,  1774,    -1,    -1,    -1,  1274,    -1,
      -1,  1277,   377,  1279,  1280,    -1,    -1,    -1,  1070,    -1,
      -1,  1976,    -1,  1075,  1076,    -1,  1078,  1079,    -1,    -1,
      -1,  1801,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   411,    -1,    -1,    -1,
      -1,   294,    -1,  2008,  1320,    -1,    -1,    -1,  1828,    -1,
      -1,  1831,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1471,
      -1,    -1,    -1,    -1,    -1,    -1,   441,    -1,    -1,    -1,
      -1,    -1,   447,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1500,    -1,
      -1,  2056,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1387,    -1,    -1,  1526,    -1,    -1,    -1,    -1,    -1,
    1532,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1193,    -1,    -1,    -1,    -1,    -1,  1199,  1200,    -1,
      -1,   516,   517,     3,    -1,    -1,    -1,   522,    -1,    -1,
    1930,    -1,    -1,    13,    14,    15,    16,    17,    -1,  1221,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,  1601,
      50,    51,    52,    -1,    54,  1471,    -1,    -1,    -1,    -1,
     575,    -1,    -1,  1265,  1266,    -1,    -1,    -1,    -1,    -1,
      -1,    71,  1274,  1275,   467,  1277,    -1,    -1,    -1,   594,
     473,   596,    -1,    -1,  1500,   478,  1288,  1289,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   617,    -1,    -1,   105,   106,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   638,    -1,  1677,  1678,    -1,    -1,    -1,
     130,    -1,    -1,  1685,    -1,    -1,   651,  1689,   653,   654,
      -1,   656,    -1,    -1,    -1,    -1,    -1,   662,    -1,    -1,
     665,   666,   667,   153,   154,    -1,    -1,    -1,    -1,    -1,
      -1,   161,   162,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   565,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1601,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   594,    -1,    -1,  1406,    -1,   721,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   607,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     745,    -1,    -1,    -1,    -1,    -1,    -1,  1789,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   762,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1677,  1678,    -1,    -1,   658,    -1,    -1,    -1,   784,
     785,    -1,    -1,  1689,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   679,   680,   803,    -1,
     683,    -1,   685,    -1,    -1,    -1,    -1,    -1,   691,    -1,
     693,   694,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1861,
      -1,    -1,    -1,    -1,  1516,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   721,    -1,
    1532,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   734,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   745,    -1,   869,    -1,    -1,    -1,    -1,    -1,
      -1,   876,    -1,    -1,    -1,    -1,   759,    -1,    -1,   762,
      -1,    -1,   887,  1789,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   789,    -1,    -1,   792,
    1602,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1969,    -1,    -1,
      -1,    -1,   937,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   828,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1861,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1655,  1656,    -1,    49,    -1,    -1,    -1,
      53,    -1,    55,    -1,    -1,    -1,    -1,    -1,  1670,  1671,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,   876,  1686,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   887,   888,    -1,    -1,    -1,    -1,
      -1,    -1,   895,    -1,    -1,    -1,    99,   100,    -1,   102,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,    -1,   118,   119,   120,    -1,   122,
     123,   924,    -1,    -1,    -1,    -1,    -1,   130,    -1,    -1,
      -1,    -1,    -1,    -1,   937,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   945,    -1,    -1,  1070,    -1,   150,   151,   952,
     153,   154,    -1,    -1,    -1,   158,    -1,   160,   161,   162,
     163,   164,   165,   166,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1787,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1795,    -1,  1110,    -1,    -1,    -1,    13,
      14,    15,    16,    17,    -1,   998,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    -1,    50,    51,    52,    -1,
      54,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1164,
      -1,    -1,    -1,    -1,    -1,  1857,    -1,    71,    -1,  1861,
    1862,    -1,    -1,  1865,    -1,    -1,    -1,    -1,    -1,    -1,
    1185,    -1,  1065,    -1,  1067,    -1,  1069,    -1,  1193,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   102,    -1,
      -1,   105,   106,   107,   108,   109,   110,   111,   112,   113,
      -1,    -1,    -1,    -1,  1906,    -1,  1221,    -1,    -1,    -1,
    1225,    -1,    -1,    -1,    -1,    -1,   130,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    49,    -1,    -1,    -1,
      53,    -1,    55,    -1,    -1,    -1,   150,   151,    -1,   153,
     154,    -1,  1135,  1136,    -1,    -1,    -1,   161,   162,    72,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1969,  1970,    -1,
      -1,    -1,    -1,  1288,  1289,    -1,    99,   100,    -1,   102,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,    -1,   118,   119,   120,    -1,   122,
     123,    -1,    -1,    -1,  1197,    -1,    -1,   130,  2010,    -1,
    1203,    -1,  1327,  1328,  1329,    -1,  1331,  1332,    -1,  1212,
      -1,    -1,    -1,  1338,    -1,    -1,    -1,   150,  1221,    -1,
     153,   154,    -1,    -1,    -1,   158,    -1,   160,   161,   162,
     163,   164,   165,   166,    -1,  1238,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1253,    -1,    -1,  1256,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1386,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1406,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,    -1,    -1,
      -1,    -1,    -1,    -1,    10,  1308,    -1,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,  1346,    -1,    51,    52,    -1,    -1,    -1,
      56,    57,    58,    59,    60,    61,    62,    63,    -1,    -1,
      -1,  1364,    -1,    -1,  1367,    71,    72,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   101,    -1,  1522,    -1,   105,
     106,    -1,    -1,  1406,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1416,  1417,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,
      -1,  1556,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1442,
      -1,  1444,    -1,   149,    -1,    -1,    -1,   153,   154,    -1,
      -1,    -1,    -1,    -1,    -1,   161,   162,    -1,    -1,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    -1,    -1,    20,  1602,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,  1630,    50,    51,    52,    -1,
      54,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1522,
      -1,    -1,    -1,    -1,  1527,    -1,    -1,    71,    -1,    -1,
    1655,  1656,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   102,    -1,
       1,   105,   106,   107,   108,   109,   110,   111,   112,   113,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    18,    -1,    -1,
    1705,    -1,    -1,    -1,  1587,    -1,   130,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   150,   151,    49,   153,
     154,    -1,    53,    -1,    55,    -1,    -1,   161,   162,    -1,
      -1,    -1,    -1,    -1,    -1,  1628,    -1,    -1,  1631,    70,
      -1,    72,    73,    -1,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    90,
      91,    92,    93,    94,    95,    -1,    97,    -1,    99,   100,
      -1,   102,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
      -1,   122,   123,    -1,    -1,    -1,    -1,    -1,    -1,   130,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1832,   149,   150,
      -1,    -1,   153,   154,     1,    -1,    -1,   158,    -1,   160,
     161,   162,   163,   164,   165,   166,    -1,    -1,    -1,    -1,
      -1,    18,  1857,    -1,   175,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    49,    -1,    -1,    -1,    53,    -1,    55,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1904,
      -1,    -1,  1907,    70,    -1,    72,    73,    -1,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    91,    92,    93,    94,    95,    -1,
      97,    -1,    99,   100,    -1,   102,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,    -1,   122,   123,  1840,     1,    -1,
      -1,    -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    18,    -1,    -1,    -1,    -1,
      -1,    -1,   149,   150,    -1,    -1,   153,   154,    -1,    -1,
      -1,   158,    -1,   160,   161,   162,   163,   164,   165,   166,
      -1,    -1,    -1,    -1,    -1,    -1,    49,    -1,   175,    -1,
      53,    -1,    55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,    72,
      73,    -1,    75,    -1,    -1,    78,    79,    80,    81,    82,
      83,    84,    85,    86,    87,    88,    89,    90,    91,    92,
      93,    94,    95,    -1,    97,    -1,    99,   100,    -1,   102,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,    -1,   122,
     123,    -1,    -1,    -1,    -1,    -1,    -1,   130,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   150,    -1,    -1,
     153,   154,    -1,    -1,    -1,   158,    -1,   160,   161,   162,
     163,   164,   165,   166,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   175,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,
     150,    -1,    -1,   153,   154,    -1,    -1,    -1,   158,    -1,
     160,   161,   162,   163,   164,   165,   166,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   175,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    49,    -1,    51,    52,    53,    -1,    55,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    -1,    -1,
      -1,    68,    -1,    70,    71,    72,    73,    -1,    75,    -1,
      -1,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    91,    92,    93,    94,    95,    -1,
      97,    -1,    99,   100,   101,   102,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,    -1,   122,   123,    -1,    -1,    -1,
      -1,    -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   150,    -1,    -1,   153,   154,    -1,    -1,
      -1,   158,    -1,   160,   161,   162,   163,   164,   165,   166,
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
     144,   145,   146,    -1,    -1,    -1,   150,   151,   152,   153,
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
      -1,    -1,    -1,   144,   145,   146,    -1,    -1,    -1,   150,
     151,    -1,   153,   154,    -1,    -1,    -1,    -1,    -1,   160,
     161,   162,   163,   164,   165,   166,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   175,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    49,    -1,    51,    52,    53,    -1,    55,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    -1,    -1,    -1,
      68,    -1,    -1,    71,    72,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    99,   100,   101,   102,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,    -1,
     118,   119,   120,    -1,   122,   123,    -1,    -1,    -1,    -1,
      -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   150,    -1,   152,   153,   154,    -1,    -1,    -1,
      -1,    -1,   160,   161,   162,   163,   164,   165,   166,     3,
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
     165,   166,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
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
     162,   163,   164,   165,   166,     1,    -1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    -1,    50,    51,    52,    -1,    54,    -1,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    -1,
      -1,    -1,    68,    -1,    -1,    71,    -1,    -1,    -1,    -1,
      76,    77,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,    -1,   105,
     106,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   149,    -1,    -1,    -1,   153,   154,    -1,
      -1,    -1,    -1,    -1,    -1,   161,   162,     1,    -1,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    -1,    50,    51,    52,    -1,
      54,    -1,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    68,    -1,    -1,    71,    -1,    -1,
      -1,    -1,    76,    77,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,
      -1,   105,   106,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   130,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   149,    -1,    -1,    -1,   153,
     154,    -1,    -1,    -1,    -1,    -1,    -1,   161,   162,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    52,    -1,
      -1,    -1,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    68,    -1,    -1,    71,    72,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    97,    -1,    -1,    -1,   101,   102,    -1,
      -1,   105,   106,   107,   108,   109,   110,   111,   112,   113,
      -1,    -1,    -1,   117,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   130,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   150,   151,    -1,   153,
     154,    -1,    -1,    -1,    -1,    -1,    -1,   161,   162,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    -1,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    -1,    50,    51,    52,    -1,
      54,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    13,    14,    15,    16,    17,    -1,    71,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    -1,   102,    51,
      52,   105,   106,   107,   108,   109,   110,   111,   112,   113,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,
      -1,    -1,    -1,    -1,    -1,    -1,   130,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   150,   151,    -1,   153,
     154,    -1,    -1,    -1,   158,    -1,    -1,   161,   162,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    52,    -1,
      -1,    -1,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    68,    -1,    -1,    71,    -1,    -1,
      -1,    -1,    76,    77,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,
      -1,   105,   106,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   130,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   149,    -1,    -1,    -1,   153,
     154,    -1,    -1,    -1,    -1,    -1,    -1,   161,   162,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    52,    -1,
      -1,    -1,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    68,    -1,    -1,    71,    -1,    -1,
      -1,    -1,    76,    77,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,
      -1,   105,   106,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   130,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   149,    -1,    -1,    -1,   153,
     154,    -1,     3,    -1,    -1,    -1,    -1,   161,   162,    10,
      -1,    -1,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    52,    -1,    -1,    -1,    56,    57,    58,    59,    60,
      61,    62,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      71,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     101,    -1,    -1,    -1,   105,   106,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   130,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,    -1,
      -1,    -1,   153,   154,    -1,     3,    -1,    -1,    -1,    -1,
     161,   162,    10,    -1,    -1,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    52,    -1,    -1,    -1,    56,    57,
      58,    59,    60,    61,    62,    63,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    71,    72,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   101,    -1,    -1,    -1,   105,   106,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   149,    -1,    -1,    -1,   153,   154,    -1,     3,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,   161,   162,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    -1,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    -1,    50,    51,    52,    -1,    54,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,    14,
      15,    16,    17,    -1,    -1,    20,    71,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,    52,    -1,    -1,
     105,   106,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   130,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   102,   153,   154,
      -1,    -1,    -1,    -1,    -1,    -1,   161,   162,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    -1,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    -1,    50,    51,    52,    -1,    54,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   102,    -1,    -1,   105,
     106,   107,   108,   109,   110,   111,   112,   113,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   150,   151,    -1,   153,   154,    -1,
      -1,    -1,   158,    -1,    -1,   161,   162,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,    -1,    20,    -1,    22,    23,    24,    25,    26,
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
      58,    59,    60,    61,    62,    63,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   105,   106,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   130,    -1,   132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   152,   153,   154,    -1,    -1,    -1,
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
      -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   152,   153,   154,    -1,    -1,    -1,    -1,
      -1,    -1,   161,   162,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    52,    -1,    -1,    -1,    56,    57,    58,    59,
      60,    61,    62,    63,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   105,   106,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     130,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   153,   154,    -1,    -1,    -1,    -1,    -1,
      -1,   161,   162,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    52,    -1,    -1,    -1,    56,    57,    58,    59,    60,
      61,    62,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   105,   106,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   130,
      -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   153,   154,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   153,   154,    -1,    -1,    -1,    -1,    -1,    -1,   161,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     153,   154,    -1,    -1,    -1,    -1,    -1,    -1,   161,   162,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    -1,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    -1,    50,    51,    52,    -1,
      54,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,
      14,    15,    16,    17,    -1,    -1,    20,    71,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    -1,    50,    51,    52,    -1,
      54,   105,   106,    -1,    -1,    -1,    -1,    18,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   130,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    49,    -1,
      -1,    -1,    53,    -1,    55,    -1,    -1,    -1,    -1,   153,
     154,    -1,    -1,    -1,    -1,    -1,    -1,   161,   162,    70,
      -1,    72,    -1,    -1,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    90,
      91,    92,    93,    94,    95,    -1,    97,    -1,    99,   100,
      -1,   102,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
      -1,   122,   123,    -1,    -1,    -1,    -1,    -1,    -1,   130,
      -1,    -1,    -1,    -1,    -1,    18,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   150,
      -1,    -1,   153,   154,    -1,    -1,    -1,   158,    -1,   160,
     161,   162,   163,   164,   165,   166,    49,    -1,    -1,    -1,
      53,    -1,    55,    -1,   175,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    -1,    72,
      -1,    -1,    75,    -1,    -1,    78,    79,    80,    81,    82,
      83,    84,    85,    86,    87,    88,    89,    90,    91,    92,
      93,    94,    95,    -1,    97,    -1,    99,   100,    -1,   102,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,    -1,   122,
     123,    -1,    -1,    -1,    -1,    -1,    -1,   130,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   150,    -1,    -1,
     153,   154,    -1,    -1,    -1,   158,    -1,   160,   161,   162,
     163,   164,   165,   166,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   175,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    49,
      -1,    51,    52,    53,    -1,    55,    56,    57,    58,    59,
      60,    61,    62,    63,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    71,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    84,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    99,
     100,    -1,   102,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,    -1,   118,   119,
     120,    -1,   122,   123,    -1,    -1,    -1,    -1,    -1,    -1,
     130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     150,    -1,    -1,   153,   154,    -1,    -1,    -1,   158,    -1,
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
      -1,    -1,    -1,    -1,   150,    -1,   152,   153,   154,    -1,
      -1,    -1,    -1,    -1,   160,   161,   162,   163,   164,   165,
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
      -1,   153,   154,    -1,    -1,    -1,   158,    -1,   160,   161,
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
     150,    -1,    -1,   153,   154,    -1,    -1,    -1,    -1,    -1,
     160,   161,   162,   163,   164,   165,   166,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    -1,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    -1,    50,    51,    52,    -1,    54,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,
      14,    15,    16,    17,    18,    71,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    52,   105,
     106,    -1,    56,    57,    58,    59,    60,    61,    62,    63,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   153,   102,    -1,
      -1,   105,   106,   107,   108,   109,   110,   111,   112,   113,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   130,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   150,   151,    -1,   153,
     154,    -1,    -1,    -1,   158,    -1,    -1,   161,   162,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    52,    -1,
      -1,    -1,    56,    57,    58,    59,    60,    61,    62,    63,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   102,    -1,
      -1,   105,   106,   107,   108,   109,   110,   111,   112,   113,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   130,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   150,   151,    -1,   153,
     154,    -1,    -1,    -1,    -1,    -1,    -1,   161,   162,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    52,    -1,
      -1,    -1,    56,    57,    58,    59,    60,    61,    62,    63,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   102,    -1,
      -1,   105,   106,   107,   108,   109,   110,   111,   112,   113,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   130,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,   153,     4,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,
      -1,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,   153,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    52,
      -1,    -1,    -1,    56,    57,    58,    59,    60,    61,    62,
      63,    -1,    13,    14,    15,    16,    17,    18,    71,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    52,   105,   106,    -1,    56,    57,    58,    59,    60,
      61,    62,    63,    -1,    13,    14,    15,    16,    17,    18,
      71,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
     153,    -1,    51,    52,   105,   106,    -1,    56,    57,    58,
      59,    60,    61,    62,    63,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,    77,   130,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   152,   153,   154,    -1,    -1,   105,   106,    -1,    -1,
     161,   162,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   153,   154,    -1,    -1,    -1,    -1,
      -1,    -1,   161,   162,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    52,    -1,    -1,    -1,    56,    57,    58,
      59,    60,    61,    62,    63,    -1,    13,    14,    15,    16,
      17,    18,    71,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    52,   105,   106,    -1,    56,
      57,    58,    59,    60,    61,    62,    63,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,
      -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   153,   154,    -1,    -1,   105,   106,
      -1,    -1,   161,   162,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   153,   154,    -1,    -1,
      -1,    -1,    -1,    -1,   161,   162,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    52,    -1,    -1,    -1,    56,
      57,    58,    59,    60,    61,    62,    63,    -1,    13,    14,
      15,    16,    17,    18,    71,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,    52,   105,   106,
      -1,    56,    57,    58,    59,    60,    61,    62,    63,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    71,    -1,    -1,    -1,
      -1,    -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   153,   154,    -1,    -1,
     105,   106,    -1,    -1,   161,   162,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   130,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   153,   154,
      -1,    -1,    -1,    -1,    -1,    -1,   161,   162,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,    52,    -1,    -1,
      -1,    56,    57,    58,    59,    60,    61,    62,    63,    -1,
      13,    14,    15,    16,    17,    18,    71,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    52,
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
     153,   154,    -1,    -1,    -1,    -1,    -1,    -1,   161,   162,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    52,
      -1,    -1,    -1,    56,    57,    58,    59,    60,    61,    62,
      63,    -1,    13,    14,    15,    16,    17,    18,    71,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    52,   105,   106,    -1,    56,    57,    58,    59,    60,
      61,    62,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      71,    -1,    -1,    -1,    -1,    -1,    -1,   130,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     153,   154,    -1,    -1,   105,   106,    -1,    -1,   161,   162,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   130,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   153,   154,    -1,    -1,    -1,    -1,    -1,    -1,
     161,   162,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    -1,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    -1,    50,    51,
      52,    -1,    54,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    13,    14,    15,    16,    17,    18,    71,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    52,   105,   106,    -1,    56,    57,    58,    59,
      60,    61,    62,    63,    -1,    13,    14,    15,    16,    17,
      18,    71,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,   153,    -1,    51,    52,   105,   106,    -1,    56,    57,
      58,    59,    60,    61,    62,    63,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    71,    -1,    -1,    -1,    -1,    -1,    -1,
     130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   153,   154,    -1,    -1,   105,   106,    -1,
      -1,   161,   162,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   153,    -1,    -1,    13,    14,
      15,    16,    17,   161,   162,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    -1,    50,    51,    52,    -1,    54,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,    14,
      15,    16,    17,    -1,    -1,    20,    71,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    -1,    50,    51,    52,    -1,    54,
     105,   106,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   130,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   153,   154,
     105,   106,    -1,    -1,    -1,    -1,   161,   162,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   130,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   153,    -1,
      -1,    13,    14,    15,    16,    17,   161,   162,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,
      52,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    13,    14,    15,    16,    17,    18,    -1,    20,
     102,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    52,    -1,    -1,    -1,    56,    57,    58,    59,    60,
      61,    62,    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      71,    -1,    -1,    -1,    -1,   157,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   102,    -1,    -1,   105,   106,   107,   108,   109,   110,
     111,   112,   113,    -1,    -1,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,   153,    51,    52,    -1,    -1,    -1,    56,    57,
      58,    59,    60,    61,    62,    63,    -1,    13,    14,    15,
      16,    17,    18,    71,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    52,   105,   106,    -1,
      56,    57,    58,    59,    60,    61,    62,    63,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   153,    -1,    -1,    -1,   105,
     106,    13,    14,    15,    16,    17,    -1,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    -1,    50,    51,
      52,    -1,    54,    -1,    -1,    -1,    -1,   153,    49,    -1,
      -1,    -1,    53,    -1,    55,    -1,    -1,    -1,    -1,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   105,   106,    -1,    -1,    -1,    99,   100,
      -1,   102,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,    -1,   118,   119,   120,
      -1,   122,   123,    -1,    49,    -1,    -1,    -1,    53,   130,
      55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   153,    -1,   144,   145,   146,    -1,    72,    -1,   150,
     151,    -1,   153,   154,    -1,    -1,    -1,    -1,    -1,   160,
     161,   162,   163,   164,   165,   166,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    99,   100,    -1,   102,    -1,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,    -1,   118,   119,   120,    -1,   122,   123,    -1,
      49,    -1,    -1,    -1,    53,   130,    55,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   144,
     145,   146,    -1,    72,    -1,   150,   151,    -1,   153,   154,
      -1,    -1,    -1,    -1,    -1,   160,   161,   162,   163,   164,
     165,   166,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      99,   100,    -1,   102,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,    -1,   118,
     119,   120,    49,   122,   123,    -1,    53,    -1,    55,    -1,
      -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      -1,   150,   151,    -1,   153,   154,    -1,    -1,    -1,   158,
      -1,   160,   161,   162,   163,   164,   165,   166,    -1,    -1,
      -1,    -1,    99,   100,    -1,   102,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
      -1,   118,   119,   120,    49,   122,   123,    -1,    53,    -1,
      55,    -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,
      -1,    -1,    -1,   150,   151,   152,   153,   154,    -1,    -1,
      -1,    -1,    -1,   160,   161,   162,   163,   164,   165,   166,
      -1,    -1,    -1,    -1,    99,   100,    -1,   102,    -1,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,    -1,   118,   119,   120,    49,   122,   123,    -1,
      53,    -1,    55,    -1,    -1,   130,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,   150,   151,    -1,   153,   154,
      -1,    -1,    -1,    -1,    -1,   160,   161,   162,   163,   164,
     165,   166,    -1,    -1,    -1,    -1,    99,   100,    -1,   102,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,    -1,   118,   119,   120,    49,   122,
     123,    -1,    53,    -1,    55,    -1,    -1,   130,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    -1,   150,    -1,    -1,
     153,   154,    -1,    -1,    -1,   158,    -1,   160,   161,   162,
     163,   164,   165,   166,    -1,    -1,    -1,    -1,    99,   100,
      -1,   102,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,    -1,   118,   119,   120,
      49,   122,   123,    -1,    53,    -1,    55,    -1,    -1,   130,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,   150,
     151,    -1,   153,   154,    -1,    -1,    -1,    -1,    -1,   160,
     161,   162,   163,   164,   165,   166,    -1,    -1,    -1,    -1,
      99,   100,    -1,   102,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,    -1,   118,
     119,   120,    49,   122,   123,    -1,    53,    -1,    55,    -1,
      -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      -1,   150,    -1,    -1,   153,   154,    -1,    -1,   157,    -1,
      -1,   160,   161,   162,   163,   164,   165,   166,    -1,    -1,
      -1,    -1,    99,   100,    -1,   102,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
      -1,   118,   119,   120,    49,   122,   123,    -1,    53,    -1,
      55,    -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,
      -1,    -1,    -1,   150,    -1,    -1,   153,   154,    -1,    -1,
      -1,   158,    -1,   160,   161,   162,   163,   164,   165,   166,
      -1,    -1,    -1,    -1,    99,   100,    -1,   102,    -1,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,    -1,   118,   119,   120,    49,   122,   123,    -1,
      53,    -1,    55,    -1,    -1,   130,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,   150,   151,    -1,   153,   154,
      -1,    -1,    -1,    -1,    -1,   160,   161,   162,   163,   164,
     165,   166,    -1,    -1,    -1,    -1,    99,   100,    -1,   102,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,    -1,   118,   119,   120,    49,   122,
     123,    -1,    53,    -1,    55,    -1,    -1,   130,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    -1,   150,    -1,    -1,
     153,   154,    -1,    -1,    -1,   158,    -1,   160,   161,   162,
     163,   164,   165,   166,    -1,    -1,    -1,    -1,    99,   100,
      -1,   102,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,    -1,   118,   119,   120,
      49,   122,   123,    -1,    53,    -1,    55,    -1,    -1,   130,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,   150,
      -1,   152,   153,   154,    -1,    -1,    -1,    -1,    -1,   160,
     161,   162,   163,   164,   165,   166,    -1,    -1,    -1,    -1,
      99,   100,    -1,   102,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,    -1,   118,
     119,   120,    49,   122,   123,    -1,    53,    -1,    55,    -1,
      -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      -1,   150,   151,    -1,   153,   154,    -1,    -1,    -1,    -1,
      -1,   160,   161,   162,   163,   164,   165,   166,    -1,    -1,
      -1,    -1,    99,   100,    -1,   102,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
      -1,   118,   119,   120,    49,   122,   123,    -1,    53,    -1,
      55,    -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,
      -1,    -1,    -1,   150,   151,    -1,   153,   154,    -1,    -1,
      -1,    -1,    -1,   160,   161,   162,   163,   164,   165,   166,
      -1,    -1,    -1,    -1,    99,   100,    -1,   102,    -1,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,    -1,   118,   119,   120,    49,   122,   123,    -1,
      53,    -1,    55,    -1,    -1,   130,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,   150,   151,    -1,   153,   154,
      -1,    -1,    -1,    -1,    -1,   160,   161,   162,   163,   164,
     165,   166,    -1,    -1,    -1,    -1,    99,   100,    -1,   102,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,    -1,   118,   119,   120,    49,   122,
     123,    -1,    53,    -1,    55,    -1,    -1,   130,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    -1,   150,   151,    -1,
     153,   154,    -1,    -1,    -1,    -1,    -1,   160,   161,   162,
     163,   164,   165,   166,    -1,    -1,    -1,    -1,    99,   100,
      -1,   102,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,    -1,   118,   119,   120,
      49,   122,   123,    -1,    53,    -1,    55,    -1,    -1,   130,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,   150,
     151,    -1,   153,   154,    -1,    -1,    -1,    -1,    -1,   160,
     161,   162,   163,   164,   165,   166,    -1,    -1,    -1,    -1,
      99,   100,    -1,   102,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,    -1,   118,
     119,   120,    49,   122,   123,    -1,    53,    -1,    55,    -1,
      -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      -1,   150,   151,    -1,   153,   154,    -1,    -1,    -1,    -1,
      -1,   160,   161,   162,   163,   164,   165,   166,    -1,    -1,
      -1,    -1,    99,   100,    -1,   102,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
      -1,   118,   119,   120,    49,   122,   123,    -1,    53,    -1,
      55,    -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,
      -1,    -1,    -1,   150,   151,    -1,   153,   154,    -1,    -1,
      -1,    -1,    -1,   160,   161,   162,   163,   164,   165,   166,
      -1,    -1,    -1,    -1,    99,   100,    -1,   102,    -1,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,    -1,   118,   119,   120,    49,   122,   123,    -1,
      53,    -1,    55,    -1,    -1,   130,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,   150,   151,    -1,   153,   154,
      -1,    -1,    -1,    -1,    -1,   160,   161,   162,   163,   164,
     165,   166,    -1,    -1,    -1,    -1,    99,   100,    -1,   102,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,    -1,   118,   119,   120,    49,   122,
     123,    -1,    53,    -1,    55,    -1,    -1,   130,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    -1,   150,   151,    -1,
     153,   154,    -1,    -1,    -1,    -1,    -1,   160,   161,   162,
     163,   164,   165,   166,    -1,    -1,    -1,    -1,    99,   100,
      -1,   102,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,    -1,   118,   119,   120,
      49,   122,   123,    -1,    53,    -1,    55,    -1,    -1,   130,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,   150,
     151,    -1,   153,   154,    -1,    -1,    -1,    -1,    -1,   160,
     161,   162,   163,   164,   165,   166,    -1,    -1,    -1,    -1,
      99,   100,    -1,   102,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,    -1,   118,
     119,   120,    49,   122,   123,    -1,    53,    -1,    55,    -1,
      -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      -1,   150,   151,    -1,   153,   154,    -1,    -1,    -1,    -1,
      -1,   160,   161,   162,   163,   164,   165,   166,    -1,    -1,
      -1,    -1,    99,   100,    -1,   102,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
      -1,   118,   119,   120,    49,   122,   123,    -1,    53,    -1,
      55,    -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,
      -1,    -1,    -1,   150,   151,    -1,   153,   154,    -1,    -1,
      -1,    -1,    -1,   160,   161,   162,   163,   164,   165,   166,
      -1,    -1,    -1,    -1,    99,   100,    -1,   102,    -1,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,    -1,   118,   119,   120,    49,   122,   123,    -1,
      53,    -1,    55,    -1,    -1,   130,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,   150,   151,    -1,   153,   154,
      -1,    -1,    -1,    -1,    -1,   160,   161,   162,   163,   164,
     165,   166,    -1,    -1,    -1,    -1,    99,   100,    -1,   102,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,    -1,   118,   119,   120,    49,   122,
     123,    -1,    53,    -1,    55,    -1,    -1,   130,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    -1,   150,   151,    -1,
     153,   154,    -1,    -1,    -1,    -1,    -1,   160,   161,   162,
     163,   164,   165,   166,    -1,    -1,    -1,    -1,    99,   100,
      -1,   102,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,    -1,   118,   119,   120,
      49,   122,   123,    -1,    53,    -1,    55,    -1,    -1,   130,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,   150,
     151,    -1,   153,   154,    -1,    -1,    -1,    -1,    -1,   160,
     161,   162,   163,   164,   165,   166,    -1,    -1,    -1,    -1,
      99,   100,    -1,   102,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,    -1,   118,
     119,   120,    49,   122,   123,    -1,    53,    -1,    55,    -1,
      -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      -1,   150,    -1,    -1,   153,   154,    -1,    -1,    -1,    -1,
      -1,   160,   161,   162,   163,   164,   165,   166,    -1,    -1,
      -1,    -1,    99,   100,    -1,   102,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
      -1,   118,   119,   120,    49,   122,   123,    -1,    53,    -1,
      55,    -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,
      -1,    -1,    -1,   150,    -1,    -1,   153,   154,    -1,    -1,
      -1,    -1,    -1,   160,   161,   162,   163,   164,   165,   166,
      -1,    -1,    -1,    -1,    99,   100,    -1,   102,    -1,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,    -1,   118,   119,   120,    49,   122,   123,    -1,
      53,    -1,    55,    -1,    -1,   130,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,   150,    -1,    -1,   153,   154,
      -1,    -1,    -1,    -1,    -1,   160,   161,   162,   163,   164,
     165,   166,    -1,    -1,    -1,    -1,    99,   100,    -1,   102,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,    -1,   118,   119,   120,    49,   122,
     123,    -1,    53,    -1,    55,    -1,    -1,   130,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    -1,   150,    -1,    -1,
     153,   154,    -1,    -1,    -1,    -1,    -1,   160,   161,   162,
     163,   164,   165,   166,    -1,    -1,    -1,    -1,    99,   100,
      -1,   102,    -1,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,    -1,   118,   119,   120,
      49,   122,   123,    -1,    53,    -1,    55,    -1,    -1,   130,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,   150,
      -1,    -1,   153,   154,    -1,    -1,    -1,    -1,    -1,   160,
     161,   162,   163,   164,   165,   166,    -1,    -1,    -1,    -1,
      99,   100,    -1,   102,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,    -1,   118,
     119,   120,    49,   122,   123,    -1,    53,    -1,    55,    -1,
      -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      -1,   150,    -1,    -1,   153,   154,    -1,    -1,    -1,    -1,
      -1,   160,   161,   162,   163,   164,   165,   166,    -1,    -1,
      -1,    -1,    99,   100,    -1,   102,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
      -1,   118,   119,   120,    -1,   122,   123,    -1,    -1,    -1,
      -1,    -1,    -1,   130,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   150,    -1,    -1,   153,   154,    -1,    -1,
      -1,    -1,    -1,   160,   161,   162,   163,   164,   165,   166,
      13,    14,    15,    16,    17,    -1,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    -1,    50,    51,    52,
      -1,    54,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71
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
      77,   308,   181,   158,   296,   443,   456,   298,   302,   463,
     177,   445,   446,   447,   155,   177,    18,   215,   298,   444,
     466,   405,   405,   443,   296,   454,   464,   298,   181,   405,
     296,   456,   320,   156,   465,   174,   349,   158,   348,   152,
     362,   152,   152,   156,   150,   175,   361,   154,   361,   361,
     361,   215,   361,   152,   361,   361,   361,   177,   152,   163,
     164,   201,    18,   300,   152,   156,   152,   161,   162,   152,
     221,   215,   158,   215,   181,   215,   181,   114,   154,   181,
     151,   189,   190,   191,   215,   114,   154,   181,   333,   215,
     189,   181,   199,   202,   202,   202,   203,   203,   204,   204,
     205,   205,   205,   205,   206,   206,   207,   208,   209,   210,
     211,   157,   222,   175,   183,   154,   181,   215,   158,   215,
     177,   439,   440,   441,   298,   438,   405,   405,   215,   362,
     150,   405,   442,   443,   150,   442,   443,   177,   177,   155,
     155,   150,   411,   430,   431,   432,   435,    18,   298,   429,
     433,   150,   405,   448,   466,   405,   405,   466,   150,   405,
     448,   405,   405,   178,   214,   360,   155,   156,   155,   156,
     466,   466,   132,   350,   351,   352,   350,   360,   177,   213,
     214,   215,   403,   465,   364,   366,   149,   177,   152,   156,
     177,   350,   181,   402,   181,   152,   152,   152,   152,   152,
     152,   150,   405,   442,   443,   150,   405,   442,   443,   402,
     183,   443,   215,   226,   353,   152,   152,   152,   152,   389,
     390,   226,   391,   226,   400,   390,   226,   158,   158,   158,
     334,   178,   178,   181,   279,   360,    18,    70,    72,    75,
      78,    79,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    91,    92,    93,    94,    95,    97,   105,   106,
     117,   177,   222,   223,   224,   225,   226,   227,   228,   230,
     231,   241,   247,   248,   249,   250,   251,   252,   257,   258,
     264,   265,   266,   280,   298,   302,   360,   401,    69,   175,
     178,   178,   178,   350,   178,   392,   390,   284,   286,   295,
     384,   385,   386,   387,   379,   174,   370,   370,   348,   296,
     456,   154,   161,   197,   215,   320,   215,   298,   353,   152,
     152,   152,     5,   298,   405,   444,   158,   181,   434,    10,
     360,   149,   158,   214,   348,   465,   158,   152,   409,   189,
     152,   177,   156,   152,   152,   156,   152,   199,   152,   152,
     152,   199,    18,   300,   215,   152,   152,   151,   158,   199,
     155,   178,   189,   155,   155,   114,   118,   120,   182,   192,
     193,   194,   152,   156,   192,   155,   156,   149,   213,   157,
     152,   192,   178,   365,   353,   152,   152,   152,   438,   177,
     177,   353,   353,   435,   152,   152,   152,   152,   150,   411,
     434,   429,   433,   177,   177,   155,   178,   466,   177,   177,
     178,   178,   178,   178,   363,   192,   132,   166,   178,   178,
     149,   364,   215,   405,   151,   215,   350,   178,   174,   150,
     405,   442,   443,   150,   405,   442,   443,   177,   177,   404,
     152,   178,   178,   392,   390,   226,   392,   334,   334,   334,
       3,    10,    72,   149,   281,   288,   289,   295,   298,   335,
     340,   459,   152,   156,   156,   175,   150,    60,    61,   175,
     226,   280,   401,   150,    18,   224,   150,   150,   175,   360,
     175,   360,   161,   360,   158,   223,   150,   150,   150,   226,
     215,   216,   216,    14,   267,    73,   232,   175,   178,   228,
      77,   175,   360,    90,   253,   359,   298,   157,   279,   175,
     155,   155,   178,   156,   392,   402,   178,   175,   178,   175,
     178,   152,   362,   376,   376,   465,   177,   178,   178,   178,
     215,   178,   150,   405,   448,   443,   297,     5,   161,   178,
     215,   348,   405,   405,   320,   349,   365,   465,   149,   149,
     177,   152,   181,    77,   186,   187,   361,   199,   199,   199,
     199,   199,   158,   365,   156,   149,   195,   154,   193,   195,
     195,   155,   156,   121,   153,   191,   155,   221,   213,   175,
     155,   465,   178,   150,   405,   442,   443,   353,   353,   178,
     178,   152,   150,   405,   442,   443,   150,   405,   448,   411,
     405,   405,   353,   353,   155,   352,   355,   355,   356,   152,
     156,   156,   152,   178,   214,   214,   155,   155,   178,   178,
     152,   215,   177,   177,   353,   353,   363,   405,   156,   152,
     149,   392,   149,   149,   149,   149,   295,   333,   341,   459,
     295,   340,   150,   329,   175,   175,   150,   157,   197,   336,
     337,   343,   411,   412,   425,   156,   175,   360,   177,   360,
     152,   189,   190,   175,   226,   175,   226,   222,    79,   152,
     222,   233,   280,   282,   285,   291,   298,   302,   144,   145,
     146,   151,   152,   175,   222,   242,   243,   244,   280,   175,
     175,   222,   175,   365,   175,   222,   221,   222,   109,   110,
     111,   112,   113,   259,   261,   262,   175,    96,   175,    83,
     150,   150,   178,   149,   175,   175,   150,   224,   226,   405,
     175,   152,   177,   149,   149,   177,   156,   156,   149,   155,
     155,   155,   178,   152,   177,   215,   215,   178,   155,   178,
     465,   346,   158,   349,   465,   149,   384,   152,   157,   152,
     156,   157,   365,   465,   221,   119,   192,   193,   154,   193,
     154,   193,   155,   149,   152,   177,   178,   178,   152,   152,
     177,   177,   178,   178,   178,   177,   177,   155,   178,   152,
     405,   353,   353,   178,   178,   222,   149,   329,   329,   329,
     150,   197,   338,   339,   442,   450,   451,   452,   453,   175,
     156,   175,   336,   175,   379,   406,   411,   215,   298,   156,
     175,   342,   343,   342,   360,   132,   357,   358,   222,   152,
     152,   150,   224,   152,   222,   298,   144,   145,   146,   166,
     175,   245,   246,   224,   223,   175,   246,   152,   157,   222,
     151,   222,   223,   244,   175,   465,   152,   152,   152,   226,
     261,   262,   150,   215,   150,   183,   233,   199,   254,   108,
       1,   224,   405,   385,   177,   177,   155,   353,   178,   178,
     155,   155,   149,   158,   348,   149,   178,   215,   187,   215,
     465,   149,   155,   155,   192,   192,   353,   152,   152,   353,
     353,   152,   152,   155,   156,   132,   352,   132,   155,   178,
     178,   152,   152,   155,   451,   452,   453,   298,   450,   156,
     175,   405,   405,   175,   152,   411,   405,   175,   224,    76,
      77,   158,   236,   237,   238,   152,   222,    74,   224,   222,
     151,   222,    74,   175,   105,   151,   222,   223,   244,   151,
     222,   224,   243,   246,   246,   175,   222,   149,   158,   238,
     224,   150,   177,   175,   183,   152,   157,   152,   152,   156,
     157,   252,   256,   360,   402,   178,   155,   155,   348,   465,
     149,   149,   155,   155,   178,   178,   178,   177,   178,   152,
     152,   152,   152,   152,   450,   405,   337,     1,   214,   234,
     235,   403,     1,   157,     1,   177,   224,   236,    74,   175,
     152,   224,    74,   175,   166,   166,   224,   223,   246,   246,
     175,   105,   222,   166,   166,    74,   151,   222,   151,   222,
     223,   175,     1,   177,   177,   263,   296,   298,   459,   157,
     175,   154,   183,   268,   269,   270,   224,   199,   189,    74,
     107,   253,   255,   152,   465,   149,   152,   152,   152,   355,
     150,   405,   442,   443,   339,   132,     1,   156,   157,   149,
     273,   274,   280,   224,    74,   175,   224,   222,   151,   151,
     222,   151,   222,   151,   222,   223,   151,   222,   151,   222,
     224,   166,   166,   166,   166,   149,   273,   263,   178,   150,
     197,   402,   450,   181,   157,   102,   150,   152,   157,   156,
      74,   152,   224,   150,   224,   224,   149,   177,   214,   234,
     237,   239,   240,   280,   224,   166,   166,   166,   166,   151,
     151,   222,   151,   222,   151,   222,   239,   178,   175,   260,
     298,   268,   155,   214,   175,   268,   270,   224,   222,   108,
     108,   353,   224,   229,   178,   237,   151,   151,   222,   151,
     222,   151,   222,   178,   260,   213,   152,   157,   183,   152,
     152,   157,   152,   256,    74,   251,   178,     1,   224,   149,
     229,   149,   152,   226,   183,   271,   150,   175,   271,   224,
      74,   152,   226,   156,   157,   214,   152,   224,   183,   181,
     272,   152,   175,   152,   156,   175,   181
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
     344,   344,   344,   344,   346,   344,   344,   344,   347,   347,
     348,   348,   348,   348,   349,   349,   349,   350,   350,   350,
     350,   350,   350,   350,   351,   351,   351,   351,   352,   352,
     353,   353,   353,   353,   354,   354,   354,   354,   355,   355,
     355,   355,   355,   356,   356,   356,   356,   356,   357,   357,
     358,   358,   359,   359,   360,   360,   360,   361,   361,   361,
     362,   362,   363,   363,   363,   363,   364,   364,   365,   365,
     365,   365,   365,   366,   366,   367,   367,   368,   368,   368,
     368,   368,   369,   369,   370,   370,   372,   371,   373,   371,
     371,   371,   374,   374,   374,   374,   375,   375,   375,   375,
     376,   376,   377,   377,   378,   378,   379,   379,   379,   379,
     380,   380,   380,   381,   381,   382,   382,   383,   383,   384,
     384,   385,   385,   386,   386,   386,   387,   387,   388,   388,
     389,   389,   390,   390,   391,   392,   393,   393,   393,   393,
     393,   394,   393,   395,   393,   396,   393,   397,   393,   398,
     393,   399,   399,   399,   400,   400,   401,   401,   401,   401,
     401,   401,   401,   401,   401,   401,   402,   402,   402,   403,
     404,   404,   405,   405,   406,   406,   407,   408,   408,   409,
     409,   409,   410,   410,   410,   410,   410,   410,   411,   411,
     412,   412,   412,   412,   413,   413,   413,   413,   414,   414,
     414,   414,   414,   414,   414,   415,   415,   415,   415,   416,
     416,   416,   417,   417,   417,   417,   417,   418,   418,   418,
     418,   419,   419,   419,   419,   419,   419,   420,   420,   420,
     421,   421,   421,   421,   421,   422,   422,   422,   422,   423,
     423,   423,   423,   423,   423,   424,   424,   425,   425,   425,
     425,   426,   426,   426,   426,   427,   427,   427,   427,   427,
     427,   427,   428,   428,   428,   428,   428,   429,   429,   429,
     429,   429,   430,   430,   430,   431,   431,   431,   431,   432,
     432,   432,   433,   433,   433,   433,   433,   434,   434,   435,
     435,   435,   436,   436,   437,   437,   438,   438,   438,   439,
     439,   439,   439,   439,   440,   440,   440,   440,   441,   441,
     441,   442,   442,   442,   442,   443,   443,   443,   443,   444,
     444,   444,   444,   445,   445,   445,   445,   445,   446,   446,
     446,   446,   447,   447,   447,   448,   448,   448,   449,   449,
     449,   449,   449,   449,   450,   450,   450,   451,   451,   451,
     451,   451,   452,   452,   452,   452,   453,   453,   454,   454,
     454,   455,   455,   456,   456,   456,   456,   456,   456,   457,
     457,   457,   457,   457,   457,   457,   457,   457,   457,   458,
     458,   458,   458,   459,   459,   459,   460,   460,   461,   461,
     461,   461,   461,   461,   462,   462,   462,   462,   462,   462,
     463,   463,   463,   464,   464,   465,   465,   466,   466
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
       8,     7,     8,     9,     0,    12,    11,     1,     3,     3,
       2,     2,     4,     5,     0,     2,     5,     0,     1,     1,
       1,     5,     5,     5,     1,     5,     5,     9,     1,     5,
       0,     1,     1,     5,     1,     1,     5,     5,     1,     3,
       3,     4,     1,     1,     1,     1,     2,     1,     3,     3,
       2,     3,     1,     3,     1,     1,     1,     1,     1,     2,
       1,     1,     0,     2,     2,     4,     1,     4,     0,     1,
       2,     3,     4,     2,     2,     1,     2,     2,     5,     5,
       7,     6,     1,     3,     0,     2,     0,     5,     0,     5,
       3,     1,     0,     1,     1,     1,     1,     1,     1,     1,
       0,     1,     1,     2,     5,     6,     1,     1,     3,     3,
       2,     3,     3,     2,     4,     1,     4,     7,    10,     1,
       4,     2,     2,     1,     1,     5,     2,     5,     0,     1,
       3,     4,     0,     1,     0,     0,     1,     1,     1,     2,
       5,     0,     6,     0,     8,     0,     7,     0,     7,     0,
       8,     1,     2,     3,     0,     5,     3,     4,     4,     4,
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
#line 567 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.enterScope(); }
#line 7334 "Parser/parser.cc"
    break;

  case 3:
#line 571 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 7340 "Parser/parser.cc"
    break;

  case 4:
#line 578 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantInteger( *(yyvsp[0].tok) ) ); }
#line 7346 "Parser/parser.cc"
    break;

  case 5:
#line 579 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7352 "Parser/parser.cc"
    break;

  case 6:
#line 580 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7358 "Parser/parser.cc"
    break;

  case 7:
#line 581 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7364 "Parser/parser.cc"
    break;

  case 8:
#line 582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantChar( *(yyvsp[0].tok) ) ); }
#line 7370 "Parser/parser.cc"
    break;

  case 19:
#line 603 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 7376 "Parser/parser.cc"
    break;

  case 20:
#line 607 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.constant) = build_constantStr( *(yyvsp[0].str) ); }
#line 7382 "Parser/parser.cc"
    break;

  case 21:
#line 611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 7388 "Parser/parser.cc"
    break;

  case 22:
#line 613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 7398 "Parser/parser.cc"
    break;

  case 23:
#line 624 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 7404 "Parser/parser.cc"
    break;

  case 24:
#line 626 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 7410 "Parser/parser.cc"
    break;

  case 25:
#line 630 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_dimensionref( (yyvsp[0].tok) ) ); }
#line 7416 "Parser/parser.cc"
    break;

  case 27:
#line 633 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 7422 "Parser/parser.cc"
    break;

  case 28:
#line 635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new StmtExpr( dynamic_cast<CompoundStmt *>(maybeMoveBuild<Statement>((yyvsp[-1].sn)) ) ) ); }
#line 7428 "Parser/parser.cc"
    break;

  case 29:
#line 637 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7434 "Parser/parser.cc"
    break;

  case 30:
#line 639 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7440 "Parser/parser.cc"
    break;

  case 31:
#line 641 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild<Expression>( (yyvsp[-3].en) );
			(yyval.en) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 7450 "Parser/parser.cc"
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
#line 7462 "Parser/parser.cc"
    break;

  case 33:
#line 659 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "Identifier \"", *(yyvsp[-1].tok).str, "\" cannot appear before a type. "
											   "Possible problem is misspelled storage or CV qualifier." ) );
			(yyval.en) = nullptr;
		}
#line 7472 "Parser/parser.cc"
    break;

  case 35:
#line 669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// steal the association node from the singleton and delete the wrapper
			(yyvsp[-2].genexpr)->associations.splice((yyvsp[-2].genexpr)->associations.end(), (yyvsp[0].genexpr)->associations);
			delete (yyvsp[0].genexpr);
			(yyval.genexpr) = (yyvsp[-2].genexpr);
		}
#line 7483 "Parser/parser.cc"
    break;

  case 36:
#line 679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuildType((yyvsp[-2].decl)), maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } );
		}
#line 7492 "Parser/parser.cc"
    break;

  case 37:
#line 684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } ); }
#line 7498 "Parser/parser.cc"
    break;

  case 39:
#line 693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-5].en), new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) ) ) )) ) ); }
#line 7504 "Parser/parser.cc"
    break;

  case 40:
#line 699 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7510 "Parser/parser.cc"
    break;

  case 41:
#line 701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7516 "Parser/parser.cc"
    break;

  case 42:
#line 703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, new ExpressionNode( (yyvsp[-3].constant) ), (yyvsp[-1].en) ) ); }
#line 7522 "Parser/parser.cc"
    break;

  case 43:
#line 705 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.en) = new ExpressionNode( new ConstructorExpr( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) ) );
		}
#line 7532 "Parser/parser.cc"
    break;

  case 44:
#line 711 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7538 "Parser/parser.cc"
    break;

  case 45:
#line 713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 7544 "Parser/parser.cc"
    break;

  case 46:
#line 715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 7550 "Parser/parser.cc"
    break;

  case 47:
#line 717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), new ExpressionNode( (yyvsp[-2].constant) ) ) ); }
#line 7556 "Parser/parser.cc"
    break;

  case 48:
#line 719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7562 "Parser/parser.cc"
    break;

  case 49:
#line 721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 7568 "Parser/parser.cc"
    break;

  case 50:
#line 723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-1].en), build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) ) ) ); }
#line 7574 "Parser/parser.cc"
    break;

  case 51:
#line 725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7580 "Parser/parser.cc"
    break;

  case 52:
#line 727 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[0].aggKey), (yyvsp[-2].en) ) ); }
#line 7586 "Parser/parser.cc"
    break;

  case 53:
#line 729 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7592 "Parser/parser.cc"
    break;

  case 54:
#line 731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 7598 "Parser/parser.cc"
    break;

  case 55:
#line 733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7604 "Parser/parser.cc"
    break;

  case 56:
#line 735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::IncrPost, (yyvsp[-1].en) ) ); }
#line 7610 "Parser/parser.cc"
    break;

  case 57:
#line 737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::DecrPost, (yyvsp[-1].en) ) ); }
#line 7616 "Parser/parser.cc"
    break;

  case 58:
#line 739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].in), true ) ) ); }
#line 7622 "Parser/parser.cc"
    break;

  case 59:
#line 741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].in), true ))->set_maybeConstructed( false ) ) ); }
#line 7628 "Parser/parser.cc"
    break;

  case 60:
#line 743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) );
		}
#line 7638 "Parser/parser.cc"
    break;

  case 61:
#line 752 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7644 "Parser/parser.cc"
    break;

  case 64:
#line 759 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7650 "Parser/parser.cc"
    break;

  case 65:
#line 764 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Default parameter for argument is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7656 "Parser/parser.cc"
    break;

  case 68:
#line 771 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7662 "Parser/parser.cc"
    break;

  case 70:
#line 777 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-1].tok) ) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7668 "Parser/parser.cc"
    break;

  case 71:
#line 779 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-3].tok) ) ), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7674 "Parser/parser.cc"
    break;

  case 72:
#line 781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7680 "Parser/parser.cc"
    break;

  case 73:
#line 783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7686 "Parser/parser.cc"
    break;

  case 74:
#line 785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7692 "Parser/parser.cc"
    break;

  case 75:
#line 787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7698 "Parser/parser.cc"
    break;

  case 76:
#line 792 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_constantInteger( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 7704 "Parser/parser.cc"
    break;

  case 77:
#line 794 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_field_name_FLOATINGconstant( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 7710 "Parser/parser.cc"
    break;

  case 78:
#line 796 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_varref( (yyvsp[-1].tok) ), (yyvsp[0].en) ) );
		}
#line 7718 "Parser/parser.cc"
    break;

  case 79:
#line 803 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7724 "Parser/parser.cc"
    break;

  case 80:
#line 805 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Expression * constant = build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) );
			(yyval.en) = (yyvsp[-1].en) != nullptr ? new ExpressionNode( build_fieldSel( (yyvsp[-1].en),  constant ) ) : new ExpressionNode( constant );
		}
#line 7733 "Parser/parser.cc"
    break;

  case 83:
#line 817 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 7739 "Parser/parser.cc"
    break;

  case 84:
#line 819 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en)->set_extension( true ); }
#line 7745 "Parser/parser.cc"
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
#line 7765 "Parser/parser.cc"
    break;

  case 86:
#line 840 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_val( (yyvsp[-1].op), (yyvsp[0].en) ) ); }
#line 7771 "Parser/parser.cc"
    break;

  case 87:
#line 842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Incr, (yyvsp[0].en) ) ); }
#line 7777 "Parser/parser.cc"
    break;

  case 88:
#line 844 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Decr, (yyvsp[0].en) ) ); }
#line 7783 "Parser/parser.cc"
    break;

  case 89:
#line 846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7789 "Parser/parser.cc"
    break;

  case 90:
#line 848 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 7795 "Parser/parser.cc"
    break;

  case 91:
#line 850 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7801 "Parser/parser.cc"
    break;

  case 92:
#line 852 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 7807 "Parser/parser.cc"
    break;

  case 93:
#line 854 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_offsetOf( (yyvsp[-3].decl), build_varref( (yyvsp[-1].tok) ) ) ); }
#line 7813 "Parser/parser.cc"
    break;

  case 94:
#line 856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.en) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 7822 "Parser/parser.cc"
    break;

  case 95:
#line 863 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::PointTo; }
#line 7828 "Parser/parser.cc"
    break;

  case 96:
#line 864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AddressOf; }
#line 7834 "Parser/parser.cc"
    break;

  case 97:
#line 866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::And; }
#line 7840 "Parser/parser.cc"
    break;

  case 98:
#line 870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::UnPlus; }
#line 7846 "Parser/parser.cc"
    break;

  case 99:
#line 871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::UnMinus; }
#line 7852 "Parser/parser.cc"
    break;

  case 100:
#line 872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::Neg; }
#line 7858 "Parser/parser.cc"
    break;

  case 101:
#line 873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::BitNeg; }
#line 7864 "Parser/parser.cc"
    break;

  case 103:
#line 879 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cast( (yyvsp[-2].decl), (yyvsp[0].en) ) ); }
#line 7870 "Parser/parser.cc"
    break;

  case 104:
#line 881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 7876 "Parser/parser.cc"
    break;

  case 105:
#line 883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 7882 "Parser/parser.cc"
    break;

  case 106:
#line 885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( nullptr ) ) ); }
#line 7888 "Parser/parser.cc"
    break;

  case 107:
#line 887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 7894 "Parser/parser.cc"
    break;

  case 108:
#line 889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Return cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7900 "Parser/parser.cc"
    break;

  case 109:
#line 891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7906 "Parser/parser.cc"
    break;

  case 110:
#line 893 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7912 "Parser/parser.cc"
    break;

  case 118:
#line 913 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Exp, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7918 "Parser/parser.cc"
    break;

  case 120:
#line 919 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mul, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7924 "Parser/parser.cc"
    break;

  case 121:
#line 921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Div, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7930 "Parser/parser.cc"
    break;

  case 122:
#line 923 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mod, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7936 "Parser/parser.cc"
    break;

  case 124:
#line 929 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Plus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7942 "Parser/parser.cc"
    break;

  case 125:
#line 931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Minus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7948 "Parser/parser.cc"
    break;

  case 127:
#line 937 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7954 "Parser/parser.cc"
    break;

  case 128:
#line 939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::RShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7960 "Parser/parser.cc"
    break;

  case 130:
#line 945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7966 "Parser/parser.cc"
    break;

  case 131:
#line 947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7972 "Parser/parser.cc"
    break;

  case 132:
#line 949 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7978 "Parser/parser.cc"
    break;

  case 133:
#line 951 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7984 "Parser/parser.cc"
    break;

  case 135:
#line 957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Eq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7990 "Parser/parser.cc"
    break;

  case 136:
#line 959 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Neq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 7996 "Parser/parser.cc"
    break;

  case 138:
#line 965 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitAnd, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8002 "Parser/parser.cc"
    break;

  case 140:
#line 971 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Xor, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8008 "Parser/parser.cc"
    break;

  case 142:
#line 977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitOr, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8014 "Parser/parser.cc"
    break;

  case 144:
#line 983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), true ) ); }
#line 8020 "Parser/parser.cc"
    break;

  case 146:
#line 989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), false ) ); }
#line 8026 "Parser/parser.cc"
    break;

  case 148:
#line 995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8032 "Parser/parser.cc"
    break;

  case 149:
#line 998 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-3].en), (yyvsp[-3].en), (yyvsp[0].en) ) ); }
#line 8038 "Parser/parser.cc"
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
#line 8050 "Parser/parser.cc"
    break;

  case 153:
#line 1017 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8056 "Parser/parser.cc"
    break;

  case 154:
#line 1022 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8062 "Parser/parser.cc"
    break;

  case 158:
#line 1032 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::Assign; }
#line 8068 "Parser/parser.cc"
    break;

  case 159:
#line 1033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AtAssn; }
#line 8074 "Parser/parser.cc"
    break;

  case 160:
#line 1037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ExpAssn; }
#line 8080 "Parser/parser.cc"
    break;

  case 161:
#line 1038 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MulAssn; }
#line 8086 "Parser/parser.cc"
    break;

  case 162:
#line 1039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::DivAssn; }
#line 8092 "Parser/parser.cc"
    break;

  case 163:
#line 1040 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ModAssn; }
#line 8098 "Parser/parser.cc"
    break;

  case 164:
#line 1041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::PlusAssn; }
#line 8104 "Parser/parser.cc"
    break;

  case 165:
#line 1042 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MinusAssn; }
#line 8110 "Parser/parser.cc"
    break;

  case 166:
#line 1043 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::LSAssn; }
#line 8116 "Parser/parser.cc"
    break;

  case 167:
#line 1044 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::RSAssn; }
#line 8122 "Parser/parser.cc"
    break;

  case 168:
#line 1045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AndAssn; }
#line 8128 "Parser/parser.cc"
    break;

  case 169:
#line 1046 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ERAssn; }
#line 8134 "Parser/parser.cc"
    break;

  case 170:
#line 1047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::OrAssn; }
#line 8140 "Parser/parser.cc"
    break;

  case 171:
#line 1058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)(new ExpressionNode( nullptr ) )->set_last( (yyvsp[-1].en) ) ) ); }
#line 8146 "Parser/parser.cc"
    break;

  case 172:
#line 1060 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-4].en)->set_last( (yyvsp[-1].en) ) ) )); }
#line 8152 "Parser/parser.cc"
    break;

  case 174:
#line 1066 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8158 "Parser/parser.cc"
    break;

  case 175:
#line 1068 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 8164 "Parser/parser.cc"
    break;

  case 176:
#line 1070 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8170 "Parser/parser.cc"
    break;

  case 178:
#line 1076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new CommaExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 8176 "Parser/parser.cc"
    break;

  case 179:
#line 1081 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8182 "Parser/parser.cc"
    break;

  case 191:
#line 1099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 8188 "Parser/parser.cc"
    break;

  case 193:
#line 1102 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_directive( (yyvsp[0].tok) ) ); }
#line 8194 "Parser/parser.cc"
    break;

  case 194:
#line 1108 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[0].sn)->add_label( (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 8200 "Parser/parser.cc"
    break;

  case 195:
#line 1110 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "Label \"", *(yyvsp[-3].tok).str, "\" must be associated with a statement, "
											   "where a declaration, case, or default is not a statement. "
											   "Move the label or terminate with a semi-colon." ) );
			(yyval.sn) = nullptr;
		}
#line 8211 "Parser/parser.cc"
    break;

  case 196:
#line 1120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (StatementNode *)0 ) ); }
#line 8217 "Parser/parser.cc"
    break;

  case 197:
#line 1125 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (yyvsp[-2].sn) ) ); }
#line 8223 "Parser/parser.cc"
    break;

  case 199:
#line 1131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 8229 "Parser/parser.cc"
    break;

  case 200:
#line 1136 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8235 "Parser/parser.cc"
    break;

  case 201:
#line 1138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8241 "Parser/parser.cc"
    break;

  case 202:
#line 1140 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8247 "Parser/parser.cc"
    break;

  case 203:
#line 1142 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8253 "Parser/parser.cc"
    break;

  case 206:
#line 1149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 8259 "Parser/parser.cc"
    break;

  case 207:
#line 1151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Declarations only allowed at the start of the switch body, i.e., after the '{'." ); (yyval.sn) = nullptr; }
#line 8265 "Parser/parser.cc"
    break;

  case 208:
#line 1156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_expr( (yyvsp[-1].en) ) ); }
#line 8271 "Parser/parser.cc"
    break;

  case 209:
#line 1158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_mutex( nullptr, new StatementNode( build_expr( (yyvsp[-1].en) ) ) ) ); }
#line 8277 "Parser/parser.cc"
    break;

  case 210:
#line 1165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn); }
#line 8283 "Parser/parser.cc"
    break;

  case 211:
#line 1167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( true, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8289 "Parser/parser.cc"
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
#line 8303 "Parser/parser.cc"
    break;

  case 213:
#line 1179 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Only declarations can appear before the list of case clauses." ); (yyval.sn) = nullptr; }
#line 8309 "Parser/parser.cc"
    break;

  case 214:
#line 1181 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( false, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8315 "Parser/parser.cc"
    break;

  case 215:
#line 1183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( false, (yyvsp[-7].en), (yyvsp[-2].sn) ) );
			(yyval.sn) = (yyvsp[-3].decl) ? new StatementNode( build_compound( (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 8324 "Parser/parser.cc"
    break;

  case 216:
#line 1188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Only declarations can appear before the list of case clauses." ); (yyval.sn) = nullptr; }
#line 8330 "Parser/parser.cc"
    break;

  case 217:
#line 1194 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-2].ifctl), maybe_build_compound( (yyvsp[0].sn) ), nullptr ) ); }
#line 8336 "Parser/parser.cc"
    break;

  case 218:
#line 1196 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-4].ifctl), maybe_build_compound( (yyvsp[-2].sn) ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8342 "Parser/parser.cc"
    break;

  case 219:
#line 1201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( nullptr, (yyvsp[0].en) ); }
#line 8348 "Parser/parser.cc"
    break;

  case 220:
#line 1203 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 8354 "Parser/parser.cc"
    break;

  case 221:
#line 1205 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 8360 "Parser/parser.cc"
    break;

  case 222:
#line 1207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[-1].decl), (yyvsp[0].en) ); }
#line 8366 "Parser/parser.cc"
    break;

  case 223:
#line 1214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = (yyvsp[0].en); }
#line 8372 "Parser/parser.cc"
    break;

  case 224:
#line 1216 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 8378 "Parser/parser.cc"
    break;

  case 226:
#line 1221 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.sn) = new StatementNode( build_case( (yyvsp[0].en) ) ); }
#line 8384 "Parser/parser.cc"
    break;

  case 227:
#line 1223 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)((yyvsp[-2].sn)->set_last( new StatementNode( build_case( (yyvsp[0].en) ) ) ) ); }
#line 8390 "Parser/parser.cc"
    break;

  case 228:
#line 1228 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing case list after case." ); (yyval.sn) = nullptr; }
#line 8396 "Parser/parser.cc"
    break;

  case 229:
#line 1229 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn); }
#line 8402 "Parser/parser.cc"
    break;

  case 230:
#line 1231 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing colon after case list." ); (yyval.sn) = nullptr; }
#line 8408 "Parser/parser.cc"
    break;

  case 231:
#line 1232 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.sn) = new StatementNode( build_default() ); }
#line 8414 "Parser/parser.cc"
    break;

  case 232:
#line 1235 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing colon after default." ); (yyval.sn) = nullptr; }
#line 8420 "Parser/parser.cc"
    break;

  case 234:
#line 1240 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)( (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) )); }
#line 8426 "Parser/parser.cc"
    break;

  case 235:
#line 1244 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( maybe_build_compound( (yyvsp[0].sn) ) ); }
#line 8432 "Parser/parser.cc"
    break;

  case 236:
#line 1249 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = nullptr; }
#line 8438 "Parser/parser.cc"
    break;

  case 238:
#line 1255 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ); }
#line 8444 "Parser/parser.cc"
    break;

  case 239:
#line 1257 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)( (yyvsp[-2].sn)->set_last( (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ) ) ); }
#line 8450 "Parser/parser.cc"
    break;

  case 240:
#line 1262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8456 "Parser/parser.cc"
    break;

  case 241:
#line 1264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_while( new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( (yyvsp[-2].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse, "" );
		}
#line 8465 "Parser/parser.cc"
    break;

  case 242:
#line 1269 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( (yyvsp[-2].ifctl), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8471 "Parser/parser.cc"
    break;

  case 243:
#line 1271 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( (yyvsp[-4].ifctl), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[0].sn) ) ); }
#line 8477 "Parser/parser.cc"
    break;

  case 244:
#line 1273 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( NEW_ONE, maybe_build_compound( (yyvsp[-4].sn) ) ) ); }
#line 8483 "Parser/parser.cc"
    break;

  case 245:
#line 1275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_do_while( NEW_ONE, maybe_build_compound( (yyvsp[-5].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse, "" );
		}
#line 8492 "Parser/parser.cc"
    break;

  case 246:
#line 1280 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( (yyvsp[-2].en), maybe_build_compound( (yyvsp[-5].sn) ) ) ); }
#line 8498 "Parser/parser.cc"
    break;

  case 247:
#line 1282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( (yyvsp[-3].en), maybe_build_compound( (yyvsp[-6].sn) ), (yyvsp[0].sn) ) ); }
#line 8504 "Parser/parser.cc"
    break;

  case 248:
#line 1284 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( new ForCtrl( (ExpressionNode * )nullptr, (ExpressionNode * )nullptr, (ExpressionNode * )nullptr ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8510 "Parser/parser.cc"
    break;

  case 249:
#line 1286 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_for( new ForCtrl( (ExpressionNode * )nullptr, (ExpressionNode * )nullptr, (ExpressionNode * )nullptr ), maybe_build_compound( (yyvsp[-2].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse, "" );
		}
#line 8519 "Parser/parser.cc"
    break;

  case 250:
#line 1291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( (yyvsp[-2].fctl), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8525 "Parser/parser.cc"
    break;

  case 251:
#line 1293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( (yyvsp[-4].fctl), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[0].sn) ) ); }
#line 8531 "Parser/parser.cc"
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
#line 8550 "Parser/parser.cc"
    break;

  case 254:
#line 1321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (ExpressionNode * )nullptr, (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8556 "Parser/parser.cc"
    break;

  case 255:
#line 1323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8562 "Parser/parser.cc"
    break;

  case 256:
#line 1325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-3].decl), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8568 "Parser/parser.cc"
    break;

  case 257:
#line 1328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (ExpressionNode *)nullptr, (yyvsp[0].en), nullptr ); }
#line 8574 "Parser/parser.cc"
    break;

  case 258:
#line 1330 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (ExpressionNode *)nullptr, (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8580 "Parser/parser.cc"
    break;

  case 259:
#line 1333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), NEW_ZERO, OperKinds::LThan, (yyvsp[0].en)->clone(), NEW_ONE ); }
#line 8586 "Parser/parser.cc"
    break;

  case 260:
#line 1335 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].compop), NEW_ZERO, (yyvsp[0].en)->clone() ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 8592 "Parser/parser.cc"
    break;

  case 261:
#line 1338 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].compop), (yyvsp[-2].en)->clone(), (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), (yyvsp[-2].en)->clone() ), NEW_ONE ); }
#line 8598 "Parser/parser.cc"
    break;

  case 262:
#line 1340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[0].en)->clone(), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 8607 "Parser/parser.cc"
    break;

  case 263:
#line 1345 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
		}
#line 8616 "Parser/parser.cc"
    break;

  case 264:
#line 1350 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en)->clone(), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), (yyvsp[0].en) ); }
#line 8622 "Parser/parser.cc"
    break;

  case 265:
#line 1352 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-2].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].en)->clone(), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 8631 "Parser/parser.cc"
    break;

  case 266:
#line 1357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
		}
#line 8640 "Parser/parser.cc"
    break;

  case 267:
#line 1362 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 8646 "Parser/parser.cc"
    break;

  case 268:
#line 1364 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 8652 "Parser/parser.cc"
    break;

  case 269:
#line 1366 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 8658 "Parser/parser.cc"
    break;

  case 270:
#line 1368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 8664 "Parser/parser.cc"
    break;

  case 271:
#line 1370 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 8670 "Parser/parser.cc"
    break;

  case 272:
#line 1373 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-2].en), NEW_ZERO, OperKinds::LThan, (yyvsp[0].en)->clone(), NEW_ONE ); }
#line 8676 "Parser/parser.cc"
    break;

  case 273:
#line 1375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-3].en), UPDOWN( (yyvsp[-1].compop), NEW_ZERO, (yyvsp[0].en)->clone() ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 8682 "Parser/parser.cc"
    break;

  case 274:
#line 1378 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), UPDOWN( (yyvsp[-1].compop), (yyvsp[-2].en)->clone(), (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), (yyvsp[-2].en)->clone() ), NEW_ONE ); }
#line 8688 "Parser/parser.cc"
    break;

  case 275:
#line 1380 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-4].en), (yyvsp[0].en)->clone(), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 8697 "Parser/parser.cc"
    break;

  case 276:
#line 1385 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::GThan || (yyvsp[-1].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 8707 "Parser/parser.cc"
    break;

  case 277:
#line 1391 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing low/high value for up/down-to range so index is uninitialized." ); (yyval.fctl) = nullptr; }
#line 8713 "Parser/parser.cc"
    break;

  case 278:
#line 1394 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en)->clone(), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), (yyvsp[0].en) ); }
#line 8719 "Parser/parser.cc"
    break;

  case 279:
#line 1396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-6].en), (yyvsp[-2].en)->clone(), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 8728 "Parser/parser.cc"
    break;

  case 280:
#line 1401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 8738 "Parser/parser.cc"
    break;

  case 281:
#line 1407 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en)->clone(), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), nullptr ); }
#line 8744 "Parser/parser.cc"
    break;

  case 282:
#line 1409 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-6].en), (yyvsp[-2].en)->clone(), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 8753 "Parser/parser.cc"
    break;

  case 283:
#line 1414 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 8763 "Parser/parser.cc"
    break;

  case 284:
#line 1420 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing low/high value for up/down-to range so index is uninitialized." ); (yyval.fctl) = nullptr; }
#line 8769 "Parser/parser.cc"
    break;

  case 285:
#line 1423 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-1].decl), NEW_ZERO, OperKinds::LThan, (yyvsp[0].en), NEW_ONE ); }
#line 8775 "Parser/parser.cc"
    break;

  case 286:
#line 1425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].decl), UPDOWN( (yyvsp[-1].compop), NEW_ZERO, (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 8781 "Parser/parser.cc"
    break;

  case 287:
#line 1428 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-3].decl), UPDOWN( (yyvsp[-1].compop), (yyvsp[-2].en)->clone(), (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), (yyvsp[-2].en)->clone() ), NEW_ONE ); }
#line 8787 "Parser/parser.cc"
    break;

  case 288:
#line 1430 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-3].decl), (yyvsp[0].en), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 8796 "Parser/parser.cc"
    break;

  case 289:
#line 1435 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::GThan || (yyvsp[-1].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-3].decl), (yyvsp[-2].en), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 8806 "Parser/parser.cc"
    break;

  case 290:
#line 1442 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), (yyvsp[0].en) ); }
#line 8812 "Parser/parser.cc"
    break;

  case 291:
#line 1444 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-2].en), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 8821 "Parser/parser.cc"
    break;

  case 292:
#line 1449 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-4].en), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 8831 "Parser/parser.cc"
    break;

  case 293:
#line 1455 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), nullptr ); }
#line 8837 "Parser/parser.cc"
    break;

  case 294:
#line 1457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-2].en), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 8846 "Parser/parser.cc"
    break;

  case 295:
#line 1462 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-4].en), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 8856 "Parser/parser.cc"
    break;

  case 296:
#line 1468 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing low/high value for up/down-to range so index is uninitialized." ); (yyval.fctl) = nullptr; }
#line 8862 "Parser/parser.cc"
    break;

  case 297:
#line 1471 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.fctl) = nullptr;
			//$$ = forCtrl( new ExpressionNode( build_varref( $3 ) ), $1, nullptr, OperKinds::Range, nullptr, nullptr );
		}
#line 8871 "Parser/parser.cc"
    break;

  case 298:
#line 1476 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LEThan || (yyvsp[-1].compop) == OperKinds::GEThan ) { SemanticError( yylloc, "All enumation ranges are equal (all values). Remove \"=~\"." ); (yyval.fctl) = nullptr; }
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.fctl) = nullptr;
		}
#line 8880 "Parser/parser.cc"
    break;

  case 299:
#line 1484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GThan; }
#line 8886 "Parser/parser.cc"
    break;

  case 300:
#line 1486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 8892 "Parser/parser.cc"
    break;

  case 301:
#line 1488 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GEThan; }
#line 8898 "Parser/parser.cc"
    break;

  case 302:
#line 1493 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LThan; }
#line 8904 "Parser/parser.cc"
    break;

  case 303:
#line 1495 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GThan; }
#line 8910 "Parser/parser.cc"
    break;

  case 305:
#line 1501 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 8916 "Parser/parser.cc"
    break;

  case 306:
#line 1503 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GEThan; }
#line 8922 "Parser/parser.cc"
    break;

  case 307:
#line 1508 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Goto ) ); }
#line 8928 "Parser/parser.cc"
    break;

  case 308:
#line 1512 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_computedgoto( (yyvsp[-1].en) ) ); }
#line 8934 "Parser/parser.cc"
    break;

  case 309:
#line 1515 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThrough ) ); }
#line 8940 "Parser/parser.cc"
    break;

  case 310:
#line 1517 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::FallThrough ) ); }
#line 8946 "Parser/parser.cc"
    break;

  case 311:
#line 1519 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThroughDefault ) ); }
#line 8952 "Parser/parser.cc"
    break;

  case 312:
#line 1522 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Continue ) ); }
#line 8958 "Parser/parser.cc"
    break;

  case 313:
#line 1526 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Continue ) ); }
#line 8964 "Parser/parser.cc"
    break;

  case 314:
#line 1529 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Break ) ); }
#line 8970 "Parser/parser.cc"
    break;

  case 315:
#line 1533 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Break ) ); }
#line 8976 "Parser/parser.cc"
    break;

  case 316:
#line 1535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_return( (yyvsp[-1].en) ) ); }
#line 8982 "Parser/parser.cc"
    break;

  case 317:
#line 1537 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 8988 "Parser/parser.cc"
    break;

  case 318:
#line 1539 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr ) ); }
#line 8994 "Parser/parser.cc"
    break;

  case 319:
#line 1541 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn) ) ); }
#line 9000 "Parser/parser.cc"
    break;

  case 320:
#line 1543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Coroutine ) ); }
#line 9006 "Parser/parser.cc"
    break;

  case 321:
#line 1545 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Coroutine ) ); }
#line 9012 "Parser/parser.cc"
    break;

  case 322:
#line 1547 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Generator ) ); }
#line 9018 "Parser/parser.cc"
    break;

  case 323:
#line 1549 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Generator ) ); }
#line 9024 "Parser/parser.cc"
    break;

  case 324:
#line 1551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_throw( (yyvsp[-1].en) ) ); }
#line 9030 "Parser/parser.cc"
    break;

  case 325:
#line 1553 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume( (yyvsp[-1].en) ) ); }
#line 9036 "Parser/parser.cc"
    break;

  case 326:
#line 1555 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume_at( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 9042 "Parser/parser.cc"
    break;

  case 329:
#line 1565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_with( (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 9048 "Parser/parser.cc"
    break;

  case 330:
#line 1571 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_mutex( (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 9054 "Parser/parser.cc"
    break;

  case 331:
#line 1575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 9060 "Parser/parser.cc"
    break;

  case 332:
#line 1580 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9066 "Parser/parser.cc"
    break;

  case 334:
#line 1586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 9072 "Parser/parser.cc"
    break;

  case 335:
#line 1590 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) )); }
#line 9078 "Parser/parser.cc"
    break;

  case 337:
#line 1597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.en) = nullptr; }
#line 9084 "Parser/parser.cc"
    break;

  case 338:
#line 1601 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 9090 "Parser/parser.cc"
    break;

  case 339:
#line 1606 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 9096 "Parser/parser.cc"
    break;

  case 340:
#line 1608 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-3].en), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[-4].en), (yyvsp[0].wfs) ); }
#line 9102 "Parser/parser.cc"
    break;

  case 341:
#line 1610 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 9108 "Parser/parser.cc"
    break;

  case 342:
#line 1612 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( nullptr, maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 9114 "Parser/parser.cc"
    break;

  case 343:
#line 1615 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 9120 "Parser/parser.cc"
    break;

  case 344:
#line 1617 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-5].en), maybe_build_compound( (yyvsp[-4].sn) ), (yyvsp[-6].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 9126 "Parser/parser.cc"
    break;

  case 345:
#line 1622 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-1].en), (yyvsp[0].sn), (yyvsp[-2].en) ) ); }
#line 9132 "Parser/parser.cc"
    break;

  case 346:
#line 1624 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-3].en), (yyvsp[-2].sn), (yyvsp[-4].en), (yyvsp[0].wfs) ) ); }
#line 9138 "Parser/parser.cc"
    break;

  case 347:
#line 1629 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), (yyvsp[0].sn), 0 ) ); }
#line 9144 "Parser/parser.cc"
    break;

  case 348:
#line 1631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), 0, (yyvsp[0].sn) ) ); }
#line 9150 "Parser/parser.cc"
    break;

  case 349:
#line 1633 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-2].sn), (yyvsp[-1].sn), (yyvsp[0].sn) ) ); }
#line 9156 "Parser/parser.cc"
    break;

  case 350:
#line 1638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 9162 "Parser/parser.cc"
    break;

  case 351:
#line 1640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)(yyvsp[-8].sn)->set_last( new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ) ); }
#line 9168 "Parser/parser.cc"
    break;

  case 352:
#line 1645 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9174 "Parser/parser.cc"
    break;

  case 353:
#line 1646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[0].en); }
#line 9180 "Parser/parser.cc"
    break;

  case 354:
#line 1650 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 9186 "Parser/parser.cc"
    break;

  case 355:
#line 1651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 9192 "Parser/parser.cc"
    break;

  case 356:
#line 1652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.catch_kind) = CatchStmt::Resume; }
#line 9198 "Parser/parser.cc"
    break;

  case 357:
#line 1653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Resume; }
#line 9204 "Parser/parser.cc"
    break;

  case 358:
#line 1657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = new StatementNode( build_finally( (yyvsp[0].sn) ) ); }
#line 9210 "Parser/parser.cc"
    break;

  case 360:
#line 1664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 9216 "Parser/parser.cc"
    break;

  case 361:
#line 1666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 9222 "Parser/parser.cc"
    break;

  case 362:
#line 1668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 9228 "Parser/parser.cc"
    break;

  case 367:
#line 1683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-4].flag), (yyvsp[-2].constant), 0 ) ); }
#line 9234 "Parser/parser.cc"
    break;

  case 368:
#line 1685 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-6].flag), (yyvsp[-4].constant), (yyvsp[-2].en) ) ); }
#line 9240 "Parser/parser.cc"
    break;

  case 369:
#line 1687 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-8].flag), (yyvsp[-6].constant), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 9246 "Parser/parser.cc"
    break;

  case 370:
#line 1689 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-10].flag), (yyvsp[-8].constant), (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 9252 "Parser/parser.cc"
    break;

  case 371:
#line 1691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-12].flag), (yyvsp[-9].constant), 0, (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].label) ) ); }
#line 9258 "Parser/parser.cc"
    break;

  case 372:
#line 1696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = false; }
#line 9264 "Parser/parser.cc"
    break;

  case 373:
#line 1698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = true; }
#line 9270 "Parser/parser.cc"
    break;

  case 374:
#line 1703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9276 "Parser/parser.cc"
    break;

  case 377:
#line 1710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 9282 "Parser/parser.cc"
    break;

  case 378:
#line 1715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( nullptr, (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 9288 "Parser/parser.cc"
    break;

  case 379:
#line 1717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( (yyvsp[-5].tok), (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 9294 "Parser/parser.cc"
    break;

  case 380:
#line 1722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9300 "Parser/parser.cc"
    break;

  case 381:
#line 1724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 9306 "Parser/parser.cc"
    break;

  case 382:
#line 1726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( (yyvsp[0].constant) ) )); }
#line 9312 "Parser/parser.cc"
    break;

  case 383:
#line 1731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = new LabelNode(); (yyval.label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 9321 "Parser/parser.cc"
    break;

  case 384:
#line 1736 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = (yyvsp[-2].label); (yyvsp[-2].label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 9330 "Parser/parser.cc"
    break;

  case 385:
#line 1746 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9336 "Parser/parser.cc"
    break;

  case 388:
#line 1753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 9342 "Parser/parser.cc"
    break;

  case 389:
#line 1758 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9348 "Parser/parser.cc"
    break;

  case 391:
#line 1764 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 9354 "Parser/parser.cc"
    break;

  case 392:
#line 1766 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-2].decl) ); }
#line 9360 "Parser/parser.cc"
    break;

  case 399:
#line 1786 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "C_DECLARATION1 %p %s\n", $$, $$->name ? $$->name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 9371 "Parser/parser.cc"
    break;

  case 402:
#line 1798 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-4].en), (yyvsp[-2].constant) ); }
#line 9377 "Parser/parser.cc"
    break;

  case 403:
#line 1800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-2].en), build_constantStr( *new string( "\"\"" ) ) ); }
#line 9383 "Parser/parser.cc"
    break;

  case 407:
#line 1818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9389 "Parser/parser.cc"
    break;

  case 409:
#line 1824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].in) ); }
#line 9395 "Parser/parser.cc"
    break;

  case 410:
#line 1828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 9401 "Parser/parser.cc"
    break;

  case 411:
#line 1830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].in) ) ); }
#line 9407 "Parser/parser.cc"
    break;

  case 412:
#line 1837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 9413 "Parser/parser.cc"
    break;

  case 413:
#line 1839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 9419 "Parser/parser.cc"
    break;

  case 414:
#line 1841 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 9425 "Parser/parser.cc"
    break;

  case 416:
#line 1847 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9431 "Parser/parser.cc"
    break;

  case 417:
#line 1849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9437 "Parser/parser.cc"
    break;

  case 418:
#line 1851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 9443 "Parser/parser.cc"
    break;

  case 419:
#line 1853 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeClone( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->appendList( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 9454 "Parser/parser.cc"
    break;

  case 420:
#line 1886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), 0 )->addQualifiers( (yyvsp[0].decl) ); }
#line 9460 "Parser/parser.cc"
    break;

  case 421:
#line 1888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), 0 )->addQualifiers( (yyvsp[0].decl) ); }
#line 9466 "Parser/parser.cc"
    break;

  case 422:
#line 1893 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 9472 "Parser/parser.cc"
    break;

  case 423:
#line 1896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-6].decl)->appendList( (yyvsp[-2].decl) ) ); }
#line 9478 "Parser/parser.cc"
    break;

  case 424:
#line 1901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 9487 "Parser/parser.cc"
    break;

  case 425:
#line 1906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 9496 "Parser/parser.cc"
    break;

  case 426:
#line 1911 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "3" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 9505 "Parser/parser.cc"
    break;

  case 427:
#line 1922 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "4" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef();
		}
#line 9514 "Parser/parser.cc"
    break;

  case 428:
#line 1927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "5" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 9523 "Parser/parser.cc"
    break;

  case 429:
#line 1932 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "6" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[-3].decl) )->addTypedef();
		}
#line 9532 "Parser/parser.cc"
    break;

  case 430:
#line 1937 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "7" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-2].decl) )->addTypedef();
		}
#line 9541 "Parser/parser.cc"
    break;

  case 431:
#line 1942 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "8" );
			(yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-3].decl) )->addTypedef()->addType( (yyvsp[-3].decl) );
		}
#line 9550 "Parser/parser.cc"
    break;

  case 432:
#line 1951 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 9558 "Parser/parser.cc"
    break;

  case 433:
#line 1955 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 9566 "Parser/parser.cc"
    break;

  case 434:
#line 1962 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 9572 "Parser/parser.cc"
    break;

  case 438:
#line 1972 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 9578 "Parser/parser.cc"
    break;

  case 439:
#line 1974 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ) ); }
#line 9584 "Parser/parser.cc"
    break;

  case 447:
#line 1997 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_type_specifier2 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 9595 "Parser/parser.cc"
    break;

  case 452:
#line 2019 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9601 "Parser/parser.cc"
    break;

  case 455:
#line 2031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9607 "Parser/parser.cc"
    break;

  case 458:
#line 2041 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Const ); }
#line 9613 "Parser/parser.cc"
    break;

  case 459:
#line 2043 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Restrict ); }
#line 9619 "Parser/parser.cc"
    break;

  case 460:
#line 2045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Volatile ); }
#line 9625 "Parser/parser.cc"
    break;

  case 461:
#line 2047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Atomic ); }
#line 9631 "Parser/parser.cc"
    break;

  case 463:
#line 2053 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newForall( (yyvsp[-1].decl) ); }
#line 9637 "Parser/parser.cc"
    break;

  case 465:
#line 2059 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9643 "Parser/parser.cc"
    break;

  case 466:
#line 2061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9649 "Parser/parser.cc"
    break;

  case 468:
#line 2072 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9655 "Parser/parser.cc"
    break;

  case 469:
#line 2077 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Extern ); }
#line 9661 "Parser/parser.cc"
    break;

  case 470:
#line 2079 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Static ); }
#line 9667 "Parser/parser.cc"
    break;

  case 471:
#line 2081 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Auto ); }
#line 9673 "Parser/parser.cc"
    break;

  case 472:
#line 2083 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Register ); }
#line 9679 "Parser/parser.cc"
    break;

  case 473:
#line 2085 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::ThreadlocalGcc ); }
#line 9685 "Parser/parser.cc"
    break;

  case 474:
#line 2087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::ThreadlocalC11 ); }
#line 9691 "Parser/parser.cc"
    break;

  case 475:
#line 2090 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Inline ); }
#line 9697 "Parser/parser.cc"
    break;

  case 476:
#line 2092 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Fortran ); }
#line 9703 "Parser/parser.cc"
    break;

  case 477:
#line 2094 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Noreturn ); }
#line 9709 "Parser/parser.cc"
    break;

  case 478:
#line 2099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 9715 "Parser/parser.cc"
    break;

  case 479:
#line 2101 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Bool ); }
#line 9721 "Parser/parser.cc"
    break;

  case 480:
#line 2103 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Char ); }
#line 9727 "Parser/parser.cc"
    break;

  case 481:
#line 2105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int ); }
#line 9733 "Parser/parser.cc"
    break;

  case 482:
#line 2107 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 ); }
#line 9739 "Parser/parser.cc"
    break;

  case 483:
#line 2109 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 )->addType( DeclarationNode::newSignedNess( DeclarationNode::Unsigned ) ); }
#line 9745 "Parser/parser.cc"
    break;

  case 484:
#line 2111 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Float ); }
#line 9751 "Parser/parser.cc"
    break;

  case 485:
#line 2113 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Double ); }
#line 9757 "Parser/parser.cc"
    break;

  case 486:
#line 2115 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat80 ); }
#line 9763 "Parser/parser.cc"
    break;

  case 487:
#line 2117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat128 ); }
#line 9769 "Parser/parser.cc"
    break;

  case 488:
#line 2119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat16 ); }
#line 9775 "Parser/parser.cc"
    break;

  case 489:
#line 2121 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32 ); }
#line 9781 "Parser/parser.cc"
    break;

  case 490:
#line 2123 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32x ); }
#line 9787 "Parser/parser.cc"
    break;

  case 491:
#line 2125 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64 ); }
#line 9793 "Parser/parser.cc"
    break;

  case 492:
#line 2127 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64x ); }
#line 9799 "Parser/parser.cc"
    break;

  case 493:
#line 2129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat128 ); }
#line 9805 "Parser/parser.cc"
    break;

  case 494:
#line 2131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9811 "Parser/parser.cc"
    break;

  case 495:
#line 2133 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9817 "Parser/parser.cc"
    break;

  case 496:
#line 2135 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9823 "Parser/parser.cc"
    break;

  case 497:
#line 2137 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Complex ); }
#line 9829 "Parser/parser.cc"
    break;

  case 498:
#line 2139 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Imaginary ); }
#line 9835 "Parser/parser.cc"
    break;

  case 499:
#line 2141 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Signed ); }
#line 9841 "Parser/parser.cc"
    break;

  case 500:
#line 2143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Unsigned ); }
#line 9847 "Parser/parser.cc"
    break;

  case 501:
#line 2145 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Short ); }
#line 9853 "Parser/parser.cc"
    break;

  case 502:
#line 2147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Long ); }
#line 9859 "Parser/parser.cc"
    break;

  case 503:
#line 2149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Valist ); }
#line 9865 "Parser/parser.cc"
    break;

  case 504:
#line 2151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::AutoType ); }
#line 9871 "Parser/parser.cc"
    break;

  case 506:
#line 2157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9877 "Parser/parser.cc"
    break;

  case 508:
#line 2163 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVtableType( (yyvsp[-2].decl) ); }
#line 9883 "Parser/parser.cc"
    break;

  case 509:
#line 2169 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9889 "Parser/parser.cc"
    break;

  case 510:
#line 2171 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9895 "Parser/parser.cc"
    break;

  case 512:
#line 2178 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9901 "Parser/parser.cc"
    break;

  case 513:
#line 2180 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9907 "Parser/parser.cc"
    break;

  case 514:
#line 2182 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9913 "Parser/parser.cc"
    break;

  case 515:
#line 2184 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 9919 "Parser/parser.cc"
    break;

  case 517:
#line 2191 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9925 "Parser/parser.cc"
    break;

  case 519:
#line 2197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9931 "Parser/parser.cc"
    break;

  case 520:
#line 2199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9937 "Parser/parser.cc"
    break;

  case 521:
#line 2201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 9943 "Parser/parser.cc"
    break;

  case 522:
#line 2206 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 9949 "Parser/parser.cc"
    break;

  case 523:
#line 2208 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en) ); }
#line 9955 "Parser/parser.cc"
    break;

  case 524:
#line 2210 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 9961 "Parser/parser.cc"
    break;

  case 525:
#line 2212 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en), true ); }
#line 9967 "Parser/parser.cc"
    break;

  case 526:
#line 2214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Zero ); }
#line 9973 "Parser/parser.cc"
    break;

  case 527:
#line 2216 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::One ); }
#line 9979 "Parser/parser.cc"
    break;

  case 528:
#line 2221 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_declaration_specifier %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 9990 "Parser/parser.cc"
    break;

  case 529:
#line 2228 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9996 "Parser/parser.cc"
    break;

  case 530:
#line 2230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10002 "Parser/parser.cc"
    break;

  case 531:
#line 2232 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10008 "Parser/parser.cc"
    break;

  case 532:
#line 2237 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_type_specifier %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10019 "Parser/parser.cc"
    break;

  case 533:
#line 2244 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 10025 "Parser/parser.cc"
    break;

  case 534:
#line 2246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10031 "Parser/parser.cc"
    break;

  case 535:
#line 2248 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 10040 "Parser/parser.cc"
    break;

  case 537:
#line 2257 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10046 "Parser/parser.cc"
    break;

  case 538:
#line 2259 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10052 "Parser/parser.cc"
    break;

  case 539:
#line 2261 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10058 "Parser/parser.cc"
    break;

  case 541:
#line 2267 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10064 "Parser/parser.cc"
    break;

  case 542:
#line 2269 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10070 "Parser/parser.cc"
    break;

  case 544:
#line 2275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10076 "Parser/parser.cc"
    break;

  case 545:
#line 2277 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10082 "Parser/parser.cc"
    break;

  case 546:
#line 2279 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10088 "Parser/parser.cc"
    break;

  case 548:
#line 2285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10094 "Parser/parser.cc"
    break;

  case 549:
#line 2287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10100 "Parser/parser.cc"
    break;

  case 550:
#line 2292 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypedef( (yyvsp[0].tok) ); }
#line 10106 "Parser/parser.cc"
    break;

  case 551:
#line 2294 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 10112 "Parser/parser.cc"
    break;

  case 552:
#line 2296 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 10118 "Parser/parser.cc"
    break;

  case 554:
#line 2299 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), (yyvsp[0].decl) ); }
#line 10124 "Parser/parser.cc"
    break;

  case 555:
#line 2301 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), (yyvsp[0].decl) ); }
#line 10130 "Parser/parser.cc"
    break;

  case 556:
#line 2306 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[0].tok), nullptr ); }
#line 10136 "Parser/parser.cc"
    break;

  case 557:
#line 2308 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-2].tok), nullptr ); }
#line 10142 "Parser/parser.cc"
    break;

  case 558:
#line 2310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 10148 "Parser/parser.cc"
    break;

  case 559:
#line 2315 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "elaborated_type %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10159 "Parser/parser.cc"
    break;

  case 563:
#line 2331 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 10165 "Parser/parser.cc"
    break;

  case 564:
#line 2333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 10171 "Parser/parser.cc"
    break;

  case 565:
#line 2335 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 10180 "Parser/parser.cc"
    break;

  case 566:
#line 2340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 10197 "Parser/parser.cc"
    break;

  case 567:
#line 2353 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 10206 "Parser/parser.cc"
    break;

  case 568:
#line 2358 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "AGG3\n" );
			DeclarationNode::newFromTypedef( (yyvsp[-5].tok) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 10216 "Parser/parser.cc"
    break;

  case 569:
#line 2364 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 10225 "Parser/parser.cc"
    break;

  case 570:
#line 2369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "AGG4\n" );
			DeclarationNode::newFromTypeGen( (yyvsp[-5].tok), nullptr );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 10235 "Parser/parser.cc"
    break;

  case 572:
#line 2379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 10241 "Parser/parser.cc"
    break;

  case 573:
#line 2381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 10247 "Parser/parser.cc"
    break;

  case 574:
#line 2386 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 10257 "Parser/parser.cc"
    break;

  case 575:
#line 2392 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 10272 "Parser/parser.cc"
    break;

  case 578:
#line 2411 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Struct; }
#line 10278 "Parser/parser.cc"
    break;

  case 579:
#line 2413 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Union; }
#line 10284 "Parser/parser.cc"
    break;

  case 580:
#line 2415 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Exception; }
#line 10290 "Parser/parser.cc"
    break;

  case 581:
#line 2421 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 10296 "Parser/parser.cc"
    break;

  case 582:
#line 2423 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 10302 "Parser/parser.cc"
    break;

  case 583:
#line 2425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Generator; }
#line 10308 "Parser/parser.cc"
    break;

  case 584:
#line 2427 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor generator is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 10314 "Parser/parser.cc"
    break;

  case 585:
#line 2429 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Coroutine; }
#line 10320 "Parser/parser.cc"
    break;

  case 586:
#line 2431 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor coroutine is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 10326 "Parser/parser.cc"
    break;

  case 587:
#line 2433 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Thread; }
#line 10332 "Parser/parser.cc"
    break;

  case 588:
#line 2435 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor thread is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 10338 "Parser/parser.cc"
    break;

  case 589:
#line 2440 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10344 "Parser/parser.cc"
    break;

  case 590:
#line 2442 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 10350 "Parser/parser.cc"
    break;

  case 591:
#line 2447 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "type_specifier1 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			(yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) );
			// printf( "type_specifier2 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10363 "Parser/parser.cc"
    break;

  case 592:
#line 2456 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 10369 "Parser/parser.cc"
    break;

  case 593:
#line 2458 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-1].decl) ) {								// field declarator ?
				(yyvsp[-1].decl) = DeclarationNode::newName( nullptr );
			} // if
			(yyvsp[-1].decl)->inLine = true;
			(yyval.decl) = distAttr( (yyvsp[-2].decl), (yyvsp[-1].decl) );					// mark all fields in list
			distInl( (yyvsp[-1].decl) );
		}
#line 10382 "Parser/parser.cc"
    break;

  case 594:
#line 2467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10388 "Parser/parser.cc"
    break;

  case 597:
#line 2471 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 10394 "Parser/parser.cc"
    break;

  case 598:
#line 2473 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10400 "Parser/parser.cc"
    break;

  case 601:
#line 2480 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10406 "Parser/parser.cc"
    break;

  case 603:
#line 2483 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 10412 "Parser/parser.cc"
    break;

  case 604:
#line 2488 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].en) ); }
#line 10418 "Parser/parser.cc"
    break;

  case 605:
#line 2491 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 10424 "Parser/parser.cc"
    break;

  case 606:
#line 2494 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 10430 "Parser/parser.cc"
    break;

  case 607:
#line 2499 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10436 "Parser/parser.cc"
    break;

  case 609:
#line 2502 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 10442 "Parser/parser.cc"
    break;

  case 611:
#line 2513 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 10448 "Parser/parser.cc"
    break;

  case 612:
#line 2515 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 10454 "Parser/parser.cc"
    break;

  case 614:
#line 2522 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 10460 "Parser/parser.cc"
    break;

  case 615:
#line 2527 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 10466 "Parser/parser.cc"
    break;

  case 617:
#line 2533 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 10472 "Parser/parser.cc"
    break;

  case 618:
#line 2538 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10478 "Parser/parser.cc"
    break;

  case 619:
#line 2540 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); }
#line 10484 "Parser/parser.cc"
    break;

  case 620:
#line 2542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].tok), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) ); }
#line 10490 "Parser/parser.cc"
    break;

  case 621:
#line 2545 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-4].decl)->name, (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 10496 "Parser/parser.cc"
    break;

  case 622:
#line 2547 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Unvalued enumerated type is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10502 "Parser/parser.cc"
    break;

  case 623:
#line 2549 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-6].decl)->storageClasses.val != 0 || (yyvsp[-6].decl)->type->qualifiers.val != 0 )
			{ SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }

			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 10513 "Parser/parser.cc"
    break;

  case 624:
#line 2556 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].decl)->storageClasses.val != 0 || (yyvsp[-4].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			typedefTable.makeTypedef( *(yyvsp[-1].tok) );
		}
#line 10522 "Parser/parser.cc"
    break;

  case 625:
#line 2561 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, (yyvsp[-9].decl) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 10530 "Parser/parser.cc"
    break;

  case 626:
#line 2565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-8].decl)->storageClasses.val != 0 || (yyvsp[-8].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			typedefTable.makeTypedef( *(yyvsp[-5].decl)->name );
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].decl)->name, (yyvsp[-2].decl), true, (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 10540 "Parser/parser.cc"
    break;

  case 628:
#line 2575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); (yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), 0, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 10546 "Parser/parser.cc"
    break;

  case 629:
#line 2577 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].decl)->type->symbolic.name );	(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].decl)->type->symbolic.name, 0, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 10552 "Parser/parser.cc"
    break;

  case 630:
#line 2582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].in) ); }
#line 10558 "Parser/parser.cc"
    break;

  case 631:
#line 2584 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( new string("inline"), nullptr ); }
#line 10564 "Parser/parser.cc"
    break;

  case 632:
#line 2586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].in) ) ); }
#line 10570 "Parser/parser.cc"
    break;

  case 633:
#line 2588 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( DeclarationNode::newEnumValueGeneric( new string("inline"), nullptr ) ); }
#line 10576 "Parser/parser.cc"
    break;

  case 634:
#line 2593 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 10582 "Parser/parser.cc"
    break;

  case 635:
#line 2594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = new InitializerNode( (yyvsp[0].en) ); }
#line 10588 "Parser/parser.cc"
    break;

  case 636:
#line 2595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                     { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 10594 "Parser/parser.cc"
    break;

  case 637:
#line 2602 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 10600 "Parser/parser.cc"
    break;

  case 638:
#line 2604 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10606 "Parser/parser.cc"
    break;

  case 641:
#line 2608 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10612 "Parser/parser.cc"
    break;

  case 642:
#line 2610 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 10618 "Parser/parser.cc"
    break;

  case 643:
#line 2612 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 10624 "Parser/parser.cc"
    break;

  case 645:
#line 2620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10630 "Parser/parser.cc"
    break;

  case 646:
#line 2622 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10636 "Parser/parser.cc"
    break;

  case 647:
#line 2624 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-8].decl)->appendList( (yyvsp[-4].decl) )->appendList( (yyvsp[0].decl) ); }
#line 10642 "Parser/parser.cc"
    break;

  case 649:
#line 2630 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10648 "Parser/parser.cc"
    break;

  case 650:
#line 2635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10654 "Parser/parser.cc"
    break;

  case 651:
#line 2637 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10660 "Parser/parser.cc"
    break;

  case 653:
#line 2640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 10666 "Parser/parser.cc"
    break;

  case 656:
#line 2647 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10672 "Parser/parser.cc"
    break;

  case 657:
#line 2649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10678 "Parser/parser.cc"
    break;

  case 659:
#line 2658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 10684 "Parser/parser.cc"
    break;

  case 660:
#line 2661 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 10690 "Parser/parser.cc"
    break;

  case 661:
#line 2663 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 10696 "Parser/parser.cc"
    break;

  case 666:
#line 2673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10702 "Parser/parser.cc"
    break;

  case 668:
#line 2680 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 10708 "Parser/parser.cc"
    break;

  case 669:
#line 2682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 10714 "Parser/parser.cc"
    break;

  case 670:
#line 2687 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 10720 "Parser/parser.cc"
    break;

  case 671:
#line 2689 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 10726 "Parser/parser.cc"
    break;

  case 672:
#line 2698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 10732 "Parser/parser.cc"
    break;

  case 673:
#line 2700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 10738 "Parser/parser.cc"
    break;

  case 679:
#line 2713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 10744 "Parser/parser.cc"
    break;

  case 682:
#line 2723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 10750 "Parser/parser.cc"
    break;

  case 683:
#line 2724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (yyvsp[-1].op) == OperKinds::Assign ? (yyvsp[0].in) : (yyvsp[0].in)->set_maybeConstructed( false ); }
#line 10756 "Parser/parser.cc"
    break;

  case 684:
#line 2725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.in) = new InitializerNode( true ); }
#line 10762 "Parser/parser.cc"
    break;

  case 685:
#line 2726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 10768 "Parser/parser.cc"
    break;

  case 686:
#line 2730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = new InitializerNode( (yyvsp[0].en) ); }
#line 10774 "Parser/parser.cc"
    break;

  case 687:
#line 2731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 10780 "Parser/parser.cc"
    break;

  case 688:
#line 2736 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 10786 "Parser/parser.cc"
    break;

  case 690:
#line 2738 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ); }
#line 10792 "Parser/parser.cc"
    break;

  case 691:
#line 2739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (InitializerNode *)( (yyvsp[-2].in)->set_last( (yyvsp[0].in) ) ); }
#line 10798 "Parser/parser.cc"
    break;

  case 692:
#line 2740 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.in) = (InitializerNode *)((yyvsp[-3].in)->set_last( (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ) )); }
#line 10804 "Parser/parser.cc"
    break;

  case 694:
#line 2756 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[-1].tok) ) ); }
#line 10810 "Parser/parser.cc"
    break;

  case 696:
#line 2762 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-1].en)->set_last( (yyvsp[0].en) )); }
#line 10816 "Parser/parser.cc"
    break;

  case 697:
#line 2768 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 10822 "Parser/parser.cc"
    break;

  case 698:
#line 2771 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 10828 "Parser/parser.cc"
    break;

  case 699:
#line 2773 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 10834 "Parser/parser.cc"
    break;

  case 700:
#line 2775 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-4].en) ), maybeMoveBuild<Expression>( (yyvsp[-2].en) ) ) ); }
#line 10840 "Parser/parser.cc"
    break;

  case 701:
#line 2777 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 10846 "Parser/parser.cc"
    break;

  case 703:
#line 2801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl) ); }
#line 10852 "Parser/parser.cc"
    break;

  case 704:
#line 2806 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10858 "Parser/parser.cc"
    break;

  case 705:
#line 2808 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 10864 "Parser/parser.cc"
    break;

  case 706:
#line 2813 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "9" );
			if ( (yyvsp[-1].tclass) == TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 10875 "Parser/parser.cc"
    break;

  case 707:
#line 2820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 10881 "Parser/parser.cc"
    break;

  case 708:
#line 2822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "9" ); }
#line 10887 "Parser/parser.cc"
    break;

  case 709:
#line 2824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 10893 "Parser/parser.cc"
    break;

  case 710:
#line 2826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "9" );
			(yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 10902 "Parser/parser.cc"
    break;

  case 711:
#line 2832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 10908 "Parser/parser.cc"
    break;

  case 712:
#line 2837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 10914 "Parser/parser.cc"
    break;

  case 713:
#line 2839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 10920 "Parser/parser.cc"
    break;

  case 714:
#line 2841 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::DStype; }
#line 10926 "Parser/parser.cc"
    break;

  case 715:
#line 2845 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 10932 "Parser/parser.cc"
    break;

  case 716:
#line 2850 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 10938 "Parser/parser.cc"
    break;

  case 717:
#line 2852 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 10944 "Parser/parser.cc"
    break;

  case 718:
#line 2854 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ftype; }
#line 10950 "Parser/parser.cc"
    break;

  case 719:
#line 2856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 10956 "Parser/parser.cc"
    break;

  case 720:
#line 2861 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10962 "Parser/parser.cc"
    break;

  case 723:
#line 2868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 10968 "Parser/parser.cc"
    break;

  case 724:
#line 2873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 10974 "Parser/parser.cc"
    break;

  case 725:
#line 2875 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 10980 "Parser/parser.cc"
    break;

  case 726:
#line 2882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 10986 "Parser/parser.cc"
    break;

  case 728:
#line 2885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 10992 "Parser/parser.cc"
    break;

  case 729:
#line 2887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)( (yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 10998 "Parser/parser.cc"
    break;

  case 730:
#line 2892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 11004 "Parser/parser.cc"
    break;

  case 731:
#line 2894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11010 "Parser/parser.cc"
    break;

  case 732:
#line 2896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 11016 "Parser/parser.cc"
    break;

  case 733:
#line 2901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 11022 "Parser/parser.cc"
    break;

  case 734:
#line 2903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 11028 "Parser/parser.cc"
    break;

  case 735:
#line 2908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "10" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), 0 );
		}
#line 11037 "Parser/parser.cc"
    break;

  case 736:
#line 2913 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "11" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 11046 "Parser/parser.cc"
    break;

  case 737:
#line 2921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), 0 ); }
#line 11052 "Parser/parser.cc"
    break;

  case 738:
#line 2923 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-8].tok), (yyvsp[-6].decl), (yyvsp[-2].decl) ); }
#line 11058 "Parser/parser.cc"
    break;

  case 740:
#line 2929 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl) ); }
#line 11064 "Parser/parser.cc"
    break;

  case 745:
#line 2941 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 11070 "Parser/parser.cc"
    break;

  case 746:
#line 2946 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 11076 "Parser/parser.cc"
    break;

  case 747:
#line 2948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 11082 "Parser/parser.cc"
    break;

  case 749:
#line 2956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl);	}
#line 11088 "Parser/parser.cc"
    break;

  case 750:
#line 2961 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11094 "Parser/parser.cc"
    break;

  case 751:
#line 2963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->appendList( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 11100 "Parser/parser.cc"
    break;

  case 752:
#line 2968 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11106 "Parser/parser.cc"
    break;

  case 754:
#line 2973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 11112 "Parser/parser.cc"
    break;

  case 755:
#line 2977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 11118 "Parser/parser.cc"
    break;

  case 756:
#line 2982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( (yyvsp[0].tok) ) ) ); }
#line 11124 "Parser/parser.cc"
    break;

  case 759:
#line 2986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 11133 "Parser/parser.cc"
    break;

  case 760:
#line 2991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( false, (yyvsp[-2].constant), 0 ) ) ); }
#line 11139 "Parser/parser.cc"
    break;

  case 761:
#line 2993 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = LinkageSpec::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 11148 "Parser/parser.cc"
    break;

  case 762:
#line 2998 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-1].decl);
		}
#line 11158 "Parser/parser.cc"
    break;

  case 763:
#line 3004 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = LinkageSpec::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 11167 "Parser/parser.cc"
    break;

  case 764:
#line 3009 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11177 "Parser/parser.cc"
    break;

  case 765:
#line 3015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 11186 "Parser/parser.cc"
    break;

  case 766:
#line 3020 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11196 "Parser/parser.cc"
    break;

  case 767:
#line 3026 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 11205 "Parser/parser.cc"
    break;

  case 768:
#line 3031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11215 "Parser/parser.cc"
    break;

  case 769:
#line 3037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.val) || (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 11224 "Parser/parser.cc"
    break;

  case 770:
#line 3042 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11234 "Parser/parser.cc"
    break;

  case 772:
#line 3057 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].sn) ); }
#line 11240 "Parser/parser.cc"
    break;

  case 773:
#line 3059 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].sn) ); }
#line 11246 "Parser/parser.cc"
    break;

  case 774:
#line 3064 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; forall = false; }
#line 11252 "Parser/parser.cc"
    break;

  case 775:
#line 3066 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.en) = (yyvsp[-2].en); forall = false;
			if ( (yyvsp[0].decl) ) {
				SemanticError( yylloc, "Attributes cannot be associated with function body. Move attribute(s) before \"with\" clause." );
				(yyval.en) = nullptr;
			} // if
		}
#line 11264 "Parser/parser.cc"
    break;

  case 776:
#line 3077 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11275 "Parser/parser.cc"
    break;

  case 777:
#line 3084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 11284 "Parser/parser.cc"
    break;

  case 778:
#line 3089 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 11293 "Parser/parser.cc"
    break;

  case 779:
#line 3095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 11299 "Parser/parser.cc"
    break;

  case 780:
#line 3098 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 11305 "Parser/parser.cc"
    break;

  case 781:
#line 3101 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 11311 "Parser/parser.cc"
    break;

  case 782:
#line 3105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-4].decl) );
		}
#line 11320 "Parser/parser.cc"
    break;

  case 783:
#line 3111 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 11326 "Parser/parser.cc"
    break;

  case 784:
#line 3114 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 11332 "Parser/parser.cc"
    break;

  case 785:
#line 3117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 11338 "Parser/parser.cc"
    break;

  case 789:
#line 3128 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 11344 "Parser/parser.cc"
    break;

  case 790:
#line 3133 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11350 "Parser/parser.cc"
    break;

  case 791:
#line 3135 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = (yyvsp[-2].constant);
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 11360 "Parser/parser.cc"
    break;

  case 792:
#line 3144 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11366 "Parser/parser.cc"
    break;

  case 795:
#line 3151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11372 "Parser/parser.cc"
    break;

  case 796:
#line 3156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 11378 "Parser/parser.cc"
    break;

  case 798:
#line 3162 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11384 "Parser/parser.cc"
    break;

  case 799:
#line 3167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11390 "Parser/parser.cc"
    break;

  case 800:
#line 3169 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 11396 "Parser/parser.cc"
    break;

  case 801:
#line 3171 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 11402 "Parser/parser.cc"
    break;

  case 806:
#line 3180 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 11408 "Parser/parser.cc"
    break;

  case 807:
#line 3182 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 11414 "Parser/parser.cc"
    break;

  case 808:
#line 3217 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11420 "Parser/parser.cc"
    break;

  case 809:
#line 3219 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11426 "Parser/parser.cc"
    break;

  case 810:
#line 3224 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11432 "Parser/parser.cc"
    break;

  case 812:
#line 3227 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11438 "Parser/parser.cc"
    break;

  case 813:
#line 3229 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11444 "Parser/parser.cc"
    break;

  case 814:
#line 3234 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11450 "Parser/parser.cc"
    break;

  case 815:
#line 3236 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11456 "Parser/parser.cc"
    break;

  case 816:
#line 3238 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11462 "Parser/parser.cc"
    break;

  case 817:
#line 3240 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11468 "Parser/parser.cc"
    break;

  case 818:
#line 3245 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11474 "Parser/parser.cc"
    break;

  case 819:
#line 3247 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11480 "Parser/parser.cc"
    break;

  case 820:
#line 3249 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11486 "Parser/parser.cc"
    break;

  case 821:
#line 3251 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11492 "Parser/parser.cc"
    break;

  case 822:
#line 3253 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11498 "Parser/parser.cc"
    break;

  case 823:
#line 3255 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11504 "Parser/parser.cc"
    break;

  case 824:
#line 3257 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11510 "Parser/parser.cc"
    break;

  case 825:
#line 3262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11516 "Parser/parser.cc"
    break;

  case 826:
#line 3264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 11522 "Parser/parser.cc"
    break;

  case 827:
#line 3266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11528 "Parser/parser.cc"
    break;

  case 828:
#line 3268 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11534 "Parser/parser.cc"
    break;

  case 829:
#line 3277 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11540 "Parser/parser.cc"
    break;

  case 831:
#line 3280 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11546 "Parser/parser.cc"
    break;

  case 832:
#line 3285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11552 "Parser/parser.cc"
    break;

  case 833:
#line 3287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11558 "Parser/parser.cc"
    break;

  case 834:
#line 3289 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 11564 "Parser/parser.cc"
    break;

  case 835:
#line 3291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11570 "Parser/parser.cc"
    break;

  case 836:
#line 3293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11576 "Parser/parser.cc"
    break;

  case 837:
#line 3298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11582 "Parser/parser.cc"
    break;

  case 838:
#line 3300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11588 "Parser/parser.cc"
    break;

  case 839:
#line 3302 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11594 "Parser/parser.cc"
    break;

  case 840:
#line 3304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11600 "Parser/parser.cc"
    break;

  case 841:
#line 3309 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11606 "Parser/parser.cc"
    break;

  case 842:
#line 3311 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11612 "Parser/parser.cc"
    break;

  case 843:
#line 3313 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11618 "Parser/parser.cc"
    break;

  case 844:
#line 3315 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11624 "Parser/parser.cc"
    break;

  case 845:
#line 3317 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11630 "Parser/parser.cc"
    break;

  case 846:
#line 3319 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11636 "Parser/parser.cc"
    break;

  case 850:
#line 3337 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 11642 "Parser/parser.cc"
    break;

  case 851:
#line 3339 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11648 "Parser/parser.cc"
    break;

  case 852:
#line 3341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 11654 "Parser/parser.cc"
    break;

  case 853:
#line 3343 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11660 "Parser/parser.cc"
    break;

  case 854:
#line 3345 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11666 "Parser/parser.cc"
    break;

  case 855:
#line 3350 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11672 "Parser/parser.cc"
    break;

  case 856:
#line 3352 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11678 "Parser/parser.cc"
    break;

  case 857:
#line 3354 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11684 "Parser/parser.cc"
    break;

  case 858:
#line 3356 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11690 "Parser/parser.cc"
    break;

  case 859:
#line 3361 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11696 "Parser/parser.cc"
    break;

  case 860:
#line 3363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11702 "Parser/parser.cc"
    break;

  case 861:
#line 3365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11708 "Parser/parser.cc"
    break;

  case 862:
#line 3367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11714 "Parser/parser.cc"
    break;

  case 863:
#line 3369 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11720 "Parser/parser.cc"
    break;

  case 864:
#line 3371 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11726 "Parser/parser.cc"
    break;

  case 865:
#line 3386 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "ID" );
		}
#line 11735 "Parser/parser.cc"
    break;

  case 866:
#line 3391 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11741 "Parser/parser.cc"
    break;

  case 867:
#line 3396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11747 "Parser/parser.cc"
    break;

  case 869:
#line 3399 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11753 "Parser/parser.cc"
    break;

  case 870:
#line 3401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11759 "Parser/parser.cc"
    break;

  case 871:
#line 3406 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11765 "Parser/parser.cc"
    break;

  case 872:
#line 3408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11771 "Parser/parser.cc"
    break;

  case 873:
#line 3410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11777 "Parser/parser.cc"
    break;

  case 874:
#line 3412 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11783 "Parser/parser.cc"
    break;

  case 875:
#line 3417 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11789 "Parser/parser.cc"
    break;

  case 876:
#line 3419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11795 "Parser/parser.cc"
    break;

  case 877:
#line 3421 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11801 "Parser/parser.cc"
    break;

  case 878:
#line 3423 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11807 "Parser/parser.cc"
    break;

  case 879:
#line 3425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11813 "Parser/parser.cc"
    break;

  case 880:
#line 3427 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11819 "Parser/parser.cc"
    break;

  case 881:
#line 3429 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11825 "Parser/parser.cc"
    break;

  case 882:
#line 3434 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11831 "Parser/parser.cc"
    break;

  case 883:
#line 3436 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11837 "Parser/parser.cc"
    break;

  case 884:
#line 3438 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 11843 "Parser/parser.cc"
    break;

  case 885:
#line 3440 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11849 "Parser/parser.cc"
    break;

  case 886:
#line 3442 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11855 "Parser/parser.cc"
    break;

  case 887:
#line 3452 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11861 "Parser/parser.cc"
    break;

  case 888:
#line 3454 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11867 "Parser/parser.cc"
    break;

  case 890:
#line 3457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11873 "Parser/parser.cc"
    break;

  case 891:
#line 3459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11879 "Parser/parser.cc"
    break;

  case 892:
#line 3464 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11885 "Parser/parser.cc"
    break;

  case 893:
#line 3466 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11891 "Parser/parser.cc"
    break;

  case 894:
#line 3468 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11897 "Parser/parser.cc"
    break;

  case 895:
#line 3473 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11903 "Parser/parser.cc"
    break;

  case 896:
#line 3475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11909 "Parser/parser.cc"
    break;

  case 897:
#line 3477 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11915 "Parser/parser.cc"
    break;

  case 898:
#line 3479 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11921 "Parser/parser.cc"
    break;

  case 899:
#line 3484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11927 "Parser/parser.cc"
    break;

  case 900:
#line 3486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11933 "Parser/parser.cc"
    break;

  case 901:
#line 3488 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11939 "Parser/parser.cc"
    break;

  case 902:
#line 3502 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11945 "Parser/parser.cc"
    break;

  case 903:
#line 3504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11951 "Parser/parser.cc"
    break;

  case 905:
#line 3507 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11957 "Parser/parser.cc"
    break;

  case 906:
#line 3509 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11963 "Parser/parser.cc"
    break;

  case 907:
#line 3514 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11969 "Parser/parser.cc"
    break;

  case 908:
#line 3516 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11975 "Parser/parser.cc"
    break;

  case 909:
#line 3521 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11981 "Parser/parser.cc"
    break;

  case 910:
#line 3523 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11987 "Parser/parser.cc"
    break;

  case 911:
#line 3525 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11993 "Parser/parser.cc"
    break;

  case 912:
#line 3530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11999 "Parser/parser.cc"
    break;

  case 913:
#line 3532 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12005 "Parser/parser.cc"
    break;

  case 914:
#line 3537 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12011 "Parser/parser.cc"
    break;

  case 915:
#line 3539 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12017 "Parser/parser.cc"
    break;

  case 917:
#line 3557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12023 "Parser/parser.cc"
    break;

  case 918:
#line 3559 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12029 "Parser/parser.cc"
    break;

  case 919:
#line 3564 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( 0, (yyvsp[0].op) ); }
#line 12035 "Parser/parser.cc"
    break;

  case 920:
#line 3566 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 12041 "Parser/parser.cc"
    break;

  case 921:
#line 3568 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12047 "Parser/parser.cc"
    break;

  case 922:
#line 3570 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12053 "Parser/parser.cc"
    break;

  case 923:
#line 3572 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12059 "Parser/parser.cc"
    break;

  case 925:
#line 3578 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12065 "Parser/parser.cc"
    break;

  case 926:
#line 3580 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12071 "Parser/parser.cc"
    break;

  case 927:
#line 3582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12077 "Parser/parser.cc"
    break;

  case 928:
#line 3587 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 12083 "Parser/parser.cc"
    break;

  case 929:
#line 3589 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12089 "Parser/parser.cc"
    break;

  case 930:
#line 3591 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12095 "Parser/parser.cc"
    break;

  case 931:
#line 3597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false ); }
#line 12101 "Parser/parser.cc"
    break;

  case 932:
#line 3599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false )->addArray( (yyvsp[0].decl) ); }
#line 12107 "Parser/parser.cc"
    break;

  case 933:
#line 3601 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-4].en), 0, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].en), 0, false ) ); }
#line 12113 "Parser/parser.cc"
    break;

  case 935:
#line 3608 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), 0, false ); }
#line 12119 "Parser/parser.cc"
    break;

  case 936:
#line 3610 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 12125 "Parser/parser.cc"
    break;

  case 937:
#line 3612 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newArray( (yyvsp[-2].en), 0, false ) ); }
#line 12131 "Parser/parser.cc"
    break;

  case 938:
#line 3614 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 12137 "Parser/parser.cc"
    break;

  case 940:
#line 3649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 12143 "Parser/parser.cc"
    break;

  case 941:
#line 3651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12149 "Parser/parser.cc"
    break;

  case 942:
#line 3653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12155 "Parser/parser.cc"
    break;

  case 943:
#line 3658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].op) ); }
#line 12161 "Parser/parser.cc"
    break;

  case 944:
#line 3660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 12167 "Parser/parser.cc"
    break;

  case 945:
#line 3662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12173 "Parser/parser.cc"
    break;

  case 946:
#line 3664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12179 "Parser/parser.cc"
    break;

  case 947:
#line 3666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12185 "Parser/parser.cc"
    break;

  case 949:
#line 3672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12191 "Parser/parser.cc"
    break;

  case 950:
#line 3674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12197 "Parser/parser.cc"
    break;

  case 951:
#line 3676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12203 "Parser/parser.cc"
    break;

  case 952:
#line 3681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 12209 "Parser/parser.cc"
    break;

  case 953:
#line 3683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12215 "Parser/parser.cc"
    break;

  case 954:
#line 3685 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12221 "Parser/parser.cc"
    break;

  case 956:
#line 3692 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12227 "Parser/parser.cc"
    break;

  case 958:
#line 3703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false ); }
#line 12233 "Parser/parser.cc"
    break;

  case 959:
#line 3706 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 12239 "Parser/parser.cc"
    break;

  case 960:
#line 3708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, (yyvsp[-2].decl), false ); }
#line 12245 "Parser/parser.cc"
    break;

  case 961:
#line 3711 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 12251 "Parser/parser.cc"
    break;

  case 962:
#line 3713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 12257 "Parser/parser.cc"
    break;

  case 963:
#line 3715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-4].decl), true ); }
#line 12263 "Parser/parser.cc"
    break;

  case 965:
#line 3730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12269 "Parser/parser.cc"
    break;

  case 966:
#line 3732 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12275 "Parser/parser.cc"
    break;

  case 967:
#line 3737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( 0, (yyvsp[0].op) ); }
#line 12281 "Parser/parser.cc"
    break;

  case 968:
#line 3739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 12287 "Parser/parser.cc"
    break;

  case 969:
#line 3741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12293 "Parser/parser.cc"
    break;

  case 970:
#line 3743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12299 "Parser/parser.cc"
    break;

  case 971:
#line 3745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12305 "Parser/parser.cc"
    break;

  case 973:
#line 3751 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12311 "Parser/parser.cc"
    break;

  case 974:
#line 3753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12317 "Parser/parser.cc"
    break;

  case 975:
#line 3755 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12323 "Parser/parser.cc"
    break;

  case 976:
#line 3760 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12329 "Parser/parser.cc"
    break;

  case 977:
#line 3762 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12335 "Parser/parser.cc"
    break;

  case 980:
#line 3772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12341 "Parser/parser.cc"
    break;

  case 983:
#line 3783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12347 "Parser/parser.cc"
    break;

  case 984:
#line 3785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12353 "Parser/parser.cc"
    break;

  case 985:
#line 3787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12359 "Parser/parser.cc"
    break;

  case 986:
#line 3789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12365 "Parser/parser.cc"
    break;

  case 987:
#line 3791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12371 "Parser/parser.cc"
    break;

  case 988:
#line 3793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12377 "Parser/parser.cc"
    break;

  case 989:
#line 3800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 12383 "Parser/parser.cc"
    break;

  case 990:
#line 3802 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12389 "Parser/parser.cc"
    break;

  case 991:
#line 3804 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 12395 "Parser/parser.cc"
    break;

  case 992:
#line 3806 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 12401 "Parser/parser.cc"
    break;

  case 993:
#line 3808 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12407 "Parser/parser.cc"
    break;

  case 994:
#line 3811 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 12413 "Parser/parser.cc"
    break;

  case 995:
#line 3813 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12419 "Parser/parser.cc"
    break;

  case 996:
#line 3815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 12425 "Parser/parser.cc"
    break;

  case 997:
#line 3817 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 12431 "Parser/parser.cc"
    break;

  case 998:
#line 3819 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12437 "Parser/parser.cc"
    break;

  case 999:
#line 3824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 12443 "Parser/parser.cc"
    break;

  case 1000:
#line 3826 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 12449 "Parser/parser.cc"
    break;

  case 1001:
#line 3831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 12455 "Parser/parser.cc"
    break;

  case 1002:
#line 3833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl)->addQualifiers( (yyvsp[-4].decl) ), true ); }
#line 12461 "Parser/parser.cc"
    break;

  case 1004:
#line 3860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12467 "Parser/parser.cc"
    break;

  case 1008:
#line 3871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12473 "Parser/parser.cc"
    break;

  case 1009:
#line 3873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12479 "Parser/parser.cc"
    break;

  case 1010:
#line 3875 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12485 "Parser/parser.cc"
    break;

  case 1011:
#line 3877 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12491 "Parser/parser.cc"
    break;

  case 1012:
#line 3879 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12497 "Parser/parser.cc"
    break;

  case 1013:
#line 3881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12503 "Parser/parser.cc"
    break;

  case 1014:
#line 3888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12509 "Parser/parser.cc"
    break;

  case 1015:
#line 3890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12515 "Parser/parser.cc"
    break;

  case 1016:
#line 3892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12521 "Parser/parser.cc"
    break;

  case 1017:
#line 3894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12527 "Parser/parser.cc"
    break;

  case 1018:
#line 3896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12533 "Parser/parser.cc"
    break;

  case 1019:
#line 3898 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12539 "Parser/parser.cc"
    break;

  case 1020:
#line 3903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 12545 "Parser/parser.cc"
    break;

  case 1021:
#line 3905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 12551 "Parser/parser.cc"
    break;

  case 1022:
#line 3907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 12557 "Parser/parser.cc"
    break;

  case 1023:
#line 3914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 12563 "Parser/parser.cc"
    break;

  case 1024:
#line 3916 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 12569 "Parser/parser.cc"
    break;

  case 1027:
#line 3940 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 12575 "Parser/parser.cc"
    break;

  case 1028:
#line 3942 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 12581 "Parser/parser.cc"
    break;


#line 12585 "Parser/parser.cc"

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
#line 3945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "make install" //
// End: //
