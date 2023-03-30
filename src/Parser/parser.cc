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
#include <sstream>
#include <stack>
using namespace std;

#include "SynTree/Declaration.h"
#include "ParseNode.h"
#include "TypedefTable.h"
#include "TypeData.h"
#include "SynTree/LinkageSpec.h"
#include "Common/SemanticError.h"						// error_str
#include "Common/utility.h"								// for maybeMoveBuild, maybeBuild, CodeLo...

#include "SynTree/Attribute.h"							// for Attribute

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
	// printf( "distAttr1 typeSpec %p\n", typeSpec ); typeSpec->print( std::cout );
	DeclarationNode * cur = declList, * cl = (new DeclarationNode)->addType( typeSpec );
	// printf( "distAttr2 cl %p\n", cl ); cl->type->print( std::cout );
	// cl->type->aggregate.name = cl->type->aggInst.aggregate->aggregate.name;

	for ( cur = dynamic_cast<DeclarationNode *>( cur->get_next() ); cur != nullptr; cur = dynamic_cast<DeclarationNode *>( cur->get_next() ) ) {
		cl->cloneBaseType( cur );
	} // for
	declList->addType( cl );
	// printf( "distAttr3 declList %p\n", declList ); declList->print( std::cout, 0 );
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

static ForCtrl * makeForCtrl(
		DeclarationNode * init,
		enum OperKinds compop,
		ExpressionNode * comp,
		ExpressionNode * inc ) {
	// Wrap both comp/inc if they are non-null.
	if ( comp ) comp = new ExpressionNode( build_binary_val(
		compop,
		new ExpressionNode( build_varref( new string( *init->name ) ) ),
		comp ) );
	if ( inc ) inc = new ExpressionNode( build_binary_val(
		// choose += or -= for upto/downto
		compop == OperKinds::LThan || compop == OperKinds::LEThan ? OperKinds::PlusAssn : OperKinds::MinusAssn,
		new ExpressionNode( build_varref( new string( *init->name ) ) ),
		inc ) );
	// The StatementNode call frees init->name, it must happen later.
	return new ForCtrl( new StatementNode( init ), comp, inc );
}

ForCtrl * forCtrl( DeclarationNode * index, ExpressionNode * start, enum OperKinds compop, ExpressionNode * comp, ExpressionNode * inc ) {
	if ( index->initializer ) {
		SemanticError( yylloc, "Direct initialization disallowed. Use instead: type var; initialization ~ comparison ~ increment." );
	} // if
	if ( index->next ) {
		SemanticError( yylloc, "Multiple loop indexes disallowed in for-loop declaration." );
	} // if
	DeclarationNode * initDecl = index->addInitializer( new InitializerNode( start ) );
	return makeForCtrl( initDecl, compop, comp, inc );
} // forCtrl

ForCtrl * forCtrl( ExpressionNode * type, string * index, ExpressionNode * start, enum OperKinds compop, ExpressionNode * comp, ExpressionNode * inc ) {
	ConstantExpr * constant = dynamic_cast<ConstantExpr *>(type->expr.get());
	if ( constant && (constant->get_constant()->get_value() == "0" || constant->get_constant()->get_value() == "1") ) {
		type = new ExpressionNode( new CastExpr( maybeMoveBuild( type ), new BasicType( Type::Qualifiers(), BasicType::SignedInt ) ) );
	} // if
	DeclarationNode * initDecl = distAttr(
		DeclarationNode::newTypeof( type, true ),
		DeclarationNode::newName( index )->addInitializer( new InitializerNode( start ) )
	);
	return makeForCtrl( initDecl, compop, comp, inc );
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

static void IdentifierBeforeIdentifier( string & identifier1, string & identifier2, const char * kind ) {
	SemanticError( yylloc, ::toString( "Adjacent identifiers \"", identifier1, "\" and \"", identifier2, "\" are not meaningful in a", kind, ".\n"
				   "Possible cause is misspelled type name or missing generic parameter." ) );
} // IdentifierBeforeIdentifier

static void IdentifierBeforeType( string & identifier, const char * kind ) {
	SemanticError( yylloc, ::toString( "Identifier \"", identifier, "\" cannot appear before a ", kind, ".\n"
				   "Possible cause is misspelled storage/CV qualifier, misspelled typename, or missing generic parameter." ) );
} // IdentifierBeforeType

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

#line 324 "Parser/parser.cc"

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
    VA_LIST = 306,
    VA_ARG = 307,
    AUTO_TYPE = 308,
    OFFSETOF = 309,
    BASETYPEOF = 310,
    TYPEID = 311,
    ENUM = 312,
    STRUCT = 313,
    UNION = 314,
    EXCEPTION = 315,
    GENERATOR = 316,
    COROUTINE = 317,
    MONITOR = 318,
    THREAD = 319,
    OTYPE = 320,
    FTYPE = 321,
    DTYPE = 322,
    TTYPE = 323,
    TRAIT = 324,
    LABEL = 325,
    SUSPEND = 326,
    ATTRIBUTE = 327,
    EXTENSION = 328,
    IF = 329,
    ELSE = 330,
    SWITCH = 331,
    CASE = 332,
    DEFAULT = 333,
    DO = 334,
    WHILE = 335,
    FOR = 336,
    BREAK = 337,
    CONTINUE = 338,
    GOTO = 339,
    RETURN = 340,
    CHOOSE = 341,
    FALLTHRU = 342,
    FALLTHROUGH = 343,
    WITH = 344,
    WHEN = 345,
    WAITFOR = 346,
    WAITUNTIL = 347,
    DISABLE = 348,
    ENABLE = 349,
    TRY = 350,
    THROW = 351,
    THROWRESUME = 352,
    AT = 353,
    ASM = 354,
    ALIGNAS = 355,
    ALIGNOF = 356,
    GENERIC = 357,
    STATICASSERT = 358,
    IDENTIFIER = 359,
    TYPEDIMname = 360,
    TYPEDEFname = 361,
    TYPEGENname = 362,
    TIMEOUT = 363,
    WAND = 364,
    WOR = 365,
    CATCH = 366,
    RECOVER = 367,
    CATCHRESUME = 368,
    FIXUP = 369,
    FINALLY = 370,
    INTEGERconstant = 371,
    CHARACTERconstant = 372,
    STRINGliteral = 373,
    DIRECTIVE = 374,
    FLOATING_DECIMALconstant = 375,
    FLOATING_FRACTIONconstant = 376,
    FLOATINGconstant = 377,
    ARROW = 378,
    ICR = 379,
    DECR = 380,
    LS = 381,
    RS = 382,
    LE = 383,
    GE = 384,
    EQ = 385,
    NE = 386,
    ANDAND = 387,
    OROR = 388,
    ELLIPSIS = 389,
    EXPassign = 390,
    MULTassign = 391,
    DIVassign = 392,
    MODassign = 393,
    PLUSassign = 394,
    MINUSassign = 395,
    LSassign = 396,
    RSassign = 397,
    ANDassign = 398,
    ERassign = 399,
    ORassign = 400,
    ErangeUpEq = 401,
    ErangeDown = 402,
    ErangeDownEq = 403,
    ATassign = 404,
    THEN = 405
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
#define VA_LIST 306
#define VA_ARG 307
#define AUTO_TYPE 308
#define OFFSETOF 309
#define BASETYPEOF 310
#define TYPEID 311
#define ENUM 312
#define STRUCT 313
#define UNION 314
#define EXCEPTION 315
#define GENERATOR 316
#define COROUTINE 317
#define MONITOR 318
#define THREAD 319
#define OTYPE 320
#define FTYPE 321
#define DTYPE 322
#define TTYPE 323
#define TRAIT 324
#define LABEL 325
#define SUSPEND 326
#define ATTRIBUTE 327
#define EXTENSION 328
#define IF 329
#define ELSE 330
#define SWITCH 331
#define CASE 332
#define DEFAULT 333
#define DO 334
#define WHILE 335
#define FOR 336
#define BREAK 337
#define CONTINUE 338
#define GOTO 339
#define RETURN 340
#define CHOOSE 341
#define FALLTHRU 342
#define FALLTHROUGH 343
#define WITH 344
#define WHEN 345
#define WAITFOR 346
#define WAITUNTIL 347
#define DISABLE 348
#define ENABLE 349
#define TRY 350
#define THROW 351
#define THROWRESUME 352
#define AT 353
#define ASM 354
#define ALIGNAS 355
#define ALIGNOF 356
#define GENERIC 357
#define STATICASSERT 358
#define IDENTIFIER 359
#define TYPEDIMname 360
#define TYPEDEFname 361
#define TYPEGENname 362
#define TIMEOUT 363
#define WAND 364
#define WOR 365
#define CATCH 366
#define RECOVER 367
#define CATCHRESUME 368
#define FIXUP 369
#define FINALLY 370
#define INTEGERconstant 371
#define CHARACTERconstant 372
#define STRINGliteral 373
#define DIRECTIVE 374
#define FLOATING_DECIMALconstant 375
#define FLOATING_FRACTIONconstant 376
#define FLOATINGconstant 377
#define ARROW 378
#define ICR 379
#define DECR 380
#define LS 381
#define RS 382
#define LE 383
#define GE 384
#define EQ 385
#define NE 386
#define ANDAND 387
#define OROR 388
#define ELLIPSIS 389
#define EXPassign 390
#define MULTassign 391
#define DIVassign 392
#define MODassign 393
#define PLUSassign 394
#define MINUSassign 395
#define LSassign 396
#define RSassign 397
#define ANDassign 398
#define ERassign 399
#define ORassign 400
#define ErangeUpEq 401
#define ErangeDown 402
#define ErangeDownEq 403
#define ATassign 404
#define THEN 405

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 296 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"

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
	OperKinds compop;
	LabelNode * label;
	InitializerNode * in;
	OperKinds op;
	std::string * str;
	bool flag;
	EnumHiding hide;
	CatchStmt::Kind catch_kind;
	GenericExpr * genexpr;

#line 699 "Parser/parser.cc"

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
#define YYFINAL  146
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   23905

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  178
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  309
/* YYNRULES -- Number of rules.  */
#define YYNRULES  1103
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  2226

#define YYUNDEFTOK  2
#define YYMAXUTOK   405


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
       2,     2,     2,   167,     2,     2,     2,   171,   164,     2,
     152,   154,   163,   165,   158,   166,   155,   170,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,   159,   177,
     172,   176,   173,   175,   153,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,   156,   169,   157,   162,     2,   161,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,   160,   174,   151,   168,     2,     2,     2,
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
     145,   146,   147,   148,   149,   150
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   602,   602,   606,   613,   614,   615,   616,   617,   621,
     622,   623,   624,   625,   626,   627,   628,   632,   633,   637,
     638,   643,   647,   648,   659,   661,   663,   667,   668,   670,
     672,   674,   676,   686,   688,   690,   692,   694,   696,   701,
     702,   712,   717,   722,   723,   728,   734,   736,   738,   744,
     746,   750,   752,   754,   756,   758,   760,   762,   764,   766,
     768,   770,   772,   774,   776,   778,   780,   790,   791,   795,
     796,   801,   804,   808,   809,   813,   814,   816,   818,   820,
     822,   824,   829,   831,   833,   841,   842,   850,   853,   854,
     856,   861,   877,   879,   881,   883,   885,   887,   889,   891,
     893,   901,   902,   904,   908,   909,   910,   911,   915,   916,
     918,   920,   922,   924,   926,   928,   930,   937,   938,   939,
     940,   944,   945,   949,   950,   955,   956,   958,   960,   965,
     966,   968,   973,   974,   976,   981,   982,   984,   986,   988,
     993,   994,   996,  1001,  1002,  1007,  1008,  1013,  1014,  1019,
    1020,  1025,  1026,  1031,  1032,  1035,  1040,  1045,  1046,  1054,
    1060,  1061,  1065,  1066,  1070,  1071,  1075,  1076,  1077,  1078,
    1079,  1080,  1081,  1082,  1083,  1084,  1085,  1095,  1097,  1102,
    1103,  1105,  1107,  1112,  1113,  1119,  1120,  1126,  1127,  1128,
    1129,  1130,  1131,  1132,  1133,  1134,  1135,  1136,  1137,  1139,
    1140,  1146,  1148,  1158,  1160,  1168,  1169,  1174,  1176,  1178,
    1180,  1182,  1186,  1187,  1189,  1194,  1201,  1203,  1205,  1215,
    1217,  1219,  1224,  1229,  1232,  1237,  1239,  1241,  1243,  1251,
    1252,  1254,  1258,  1260,  1264,  1266,  1267,  1269,  1271,  1276,
    1277,  1281,  1286,  1287,  1291,  1293,  1298,  1300,  1305,  1307,
    1309,  1311,  1316,  1318,  1320,  1322,  1327,  1329,  1334,  1335,
    1357,  1359,  1364,  1367,  1369,  1372,  1374,  1377,  1379,  1384,
    1389,  1391,  1396,  1401,  1403,  1405,  1407,  1409,  1412,  1414,
    1417,  1419,  1424,  1430,  1433,  1435,  1440,  1446,  1448,  1453,
    1459,  1462,  1464,  1467,  1469,  1474,  1481,  1483,  1488,  1494,
    1496,  1501,  1507,  1510,  1515,  1523,  1525,  1527,  1532,  1534,
    1539,  1540,  1542,  1547,  1549,  1554,  1556,  1558,  1560,  1563,
    1567,  1570,  1574,  1576,  1578,  1580,  1582,  1584,  1586,  1588,
    1590,  1592,  1594,  1599,  1600,  1604,  1610,  1618,  1623,  1624,
    1628,  1629,  1635,  1639,  1640,  1643,  1645,  1650,  1653,  1655,
    1657,  1660,  1662,  1666,  1671,  1672,  1676,  1681,  1683,  1688,
    1690,  1695,  1697,  1699,  1701,  1704,  1706,  1711,  1717,  1719,
    1721,  1726,  1728,  1734,  1735,  1739,  1740,  1741,  1742,  1746,
    1751,  1752,  1754,  1756,  1758,  1762,  1766,  1767,  1771,  1773,
    1775,  1777,  1779,  1785,  1786,  1792,  1793,  1797,  1798,  1803,
    1805,  1811,  1812,  1814,  1819,  1824,  1835,  1836,  1840,  1841,
    1847,  1848,  1852,  1854,  1858,  1860,  1864,  1865,  1869,  1870,
    1874,  1881,  1882,  1886,  1888,  1903,  1904,  1905,  1906,  1908,
    1912,  1914,  1918,  1925,  1927,  1929,  1934,  1935,  1937,  1939,
    1941,  1973,  1976,  1981,  1983,  1989,  1994,  1999,  2010,  2017,
    2022,  2024,  2026,  2032,  2036,  2043,  2045,  2046,  2047,  2063,
    2065,  2068,  2070,  2073,  2078,  2079,  2083,  2084,  2085,  2086,
    2096,  2097,  2098,  2107,  2108,  2109,  2113,  2114,  2115,  2124,
    2125,  2126,  2131,  2132,  2141,  2142,  2147,  2148,  2152,  2154,
    2156,  2158,  2160,  2165,  2170,  2171,  2173,  2183,  2184,  2189,
    2191,  2193,  2195,  2197,  2199,  2202,  2204,  2206,  2211,  2213,
    2215,  2217,  2219,  2221,  2223,  2225,  2227,  2229,  2231,  2233,
    2235,  2237,  2239,  2241,  2243,  2245,  2247,  2249,  2251,  2253,
    2255,  2257,  2259,  2261,  2263,  2265,  2270,  2271,  2275,  2282,
    2283,  2289,  2290,  2292,  2294,  2296,  2301,  2303,  2308,  2309,
    2311,  2313,  2318,  2320,  2322,  2324,  2326,  2328,  2333,  2340,
    2342,  2344,  2349,  2357,  2356,  2360,  2368,  2369,  2371,  2373,
    2378,  2379,  2381,  2386,  2387,  2389,  2391,  2396,  2397,  2399,
    2404,  2406,  2408,  2410,  2411,  2413,  2418,  2420,  2422,  2427,
    2434,  2438,  2439,  2444,  2443,  2448,  2447,  2457,  2456,  2467,
    2466,  2476,  2481,  2482,  2487,  2493,  2511,  2512,  2516,  2518,
    2520,  2526,  2528,  2530,  2532,  2534,  2536,  2538,  2540,  2546,
    2547,  2552,  2561,  2563,  2565,  2574,  2576,  2577,  2578,  2580,
    2582,  2583,  2588,  2589,  2590,  2595,  2597,  2600,  2603,  2610,
    2611,  2612,  2618,  2623,  2625,  2631,  2632,  2638,  2639,  2643,
    2648,  2651,  2650,  2654,  2657,  2664,  2669,  2668,  2677,  2682,
    2687,  2692,  2697,  2698,  2703,  2705,  2710,  2712,  2714,  2716,
    2721,  2722,  2728,  2729,  2730,  2737,  2738,  2740,  2741,  2742,
    2744,  2746,  2753,  2754,  2756,  2758,  2763,  2764,  2770,  2771,
    2773,  2774,  2779,  2780,  2781,  2783,  2791,  2792,  2794,  2797,
    2799,  2803,  2804,  2805,  2807,  2809,  2814,  2816,  2821,  2823,
    2832,  2834,  2839,  2840,  2841,  2845,  2846,  2847,  2852,  2853,
    2858,  2859,  2860,  2861,  2865,  2866,  2871,  2872,  2873,  2874,
    2875,  2889,  2890,  2895,  2896,  2902,  2904,  2907,  2909,  2911,
    2934,  2935,  2941,  2942,  2948,  2947,  2957,  2956,  2960,  2966,
    2972,  2973,  2975,  2979,  2984,  2986,  2988,  2990,  2996,  2997,
    3001,  3002,  3007,  3009,  3016,  3018,  3019,  3021,  3026,  3028,
    3030,  3035,  3037,  3042,  3047,  3055,  3060,  3062,  3067,  3072,
    3073,  3078,  3079,  3083,  3084,  3085,  3090,  3092,  3098,  3100,
    3105,  3107,  3113,  3114,  3118,  3122,  3126,  3128,  3141,  3143,
    3145,  3147,  3149,  3151,  3153,  3154,  3159,  3162,  3161,  3173,
    3172,  3185,  3184,  3196,  3195,  3207,  3206,  3220,  3226,  3228,
    3234,  3235,  3246,  3253,  3258,  3264,  3267,  3270,  3274,  3280,
    3283,  3286,  3291,  3292,  3293,  3294,  3298,  3304,  3305,  3315,
    3316,  3320,  3321,  3326,  3331,  3332,  3338,  3339,  3341,  3346,
    3347,  3348,  3349,  3350,  3352,  3387,  3389,  3394,  3396,  3397,
    3399,  3404,  3406,  3408,  3410,  3415,  3417,  3419,  3421,  3423,
    3425,  3427,  3432,  3434,  3436,  3438,  3447,  3449,  3450,  3455,
    3457,  3459,  3461,  3463,  3468,  3470,  3472,  3474,  3479,  3481,
    3483,  3485,  3487,  3489,  3501,  3502,  3503,  3507,  3509,  3511,
    3513,  3515,  3520,  3522,  3524,  3526,  3531,  3533,  3535,  3537,
    3539,  3541,  3553,  3558,  3563,  3565,  3566,  3568,  3573,  3575,
    3577,  3579,  3584,  3586,  3588,  3590,  3592,  3594,  3596,  3601,
    3603,  3605,  3607,  3616,  3618,  3619,  3624,  3626,  3628,  3630,
    3632,  3637,  3639,  3641,  3643,  3648,  3650,  3652,  3654,  3656,
    3658,  3668,  3670,  3672,  3673,  3675,  3680,  3682,  3684,  3689,
    3691,  3693,  3695,  3700,  3702,  3704,  3718,  3720,  3722,  3723,
    3725,  3730,  3732,  3737,  3739,  3741,  3746,  3748,  3753,  3755,
    3772,  3773,  3775,  3780,  3782,  3784,  3786,  3788,  3793,  3794,
    3796,  3798,  3803,  3805,  3807,  3813,  3815,  3818,  3821,  3823,
    3827,  3829,  3831,  3832,  3834,  3836,  3840,  3842,  3847,  3849,
    3851,  3853,  3888,  3889,  3893,  3894,  3896,  3898,  3903,  3905,
    3907,  3909,  3911,  3916,  3917,  3919,  3921,  3926,  3928,  3930,
    3936,  3937,  3939,  3948,  3951,  3953,  3956,  3958,  3960,  3974,
    3975,  3977,  3982,  3984,  3986,  3988,  3990,  3995,  3996,  3998,
    4000,  4005,  4007,  4015,  4016,  4017,  4022,  4023,  4028,  4030,
    4032,  4034,  4036,  4038,  4045,  4047,  4049,  4051,  4053,  4056,
    4058,  4060,  4062,  4064,  4069,  4071,  4073,  4078,  4104,  4105,
    4107,  4111,  4112,  4116,  4118,  4120,  4122,  4124,  4126,  4133,
    4135,  4137,  4139,  4141,  4143,  4148,  4150,  4152,  4159,  4161,
    4179,  4181,  4186,  4187
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
  "VA_LIST", "VA_ARG", "AUTO_TYPE", "OFFSETOF", "BASETYPEOF", "TYPEID",
  "ENUM", "STRUCT", "UNION", "EXCEPTION", "GENERATOR", "COROUTINE",
  "MONITOR", "THREAD", "OTYPE", "FTYPE", "DTYPE", "TTYPE", "TRAIT",
  "LABEL", "SUSPEND", "ATTRIBUTE", "EXTENSION", "IF", "ELSE", "SWITCH",
  "CASE", "DEFAULT", "DO", "WHILE", "FOR", "BREAK", "CONTINUE", "GOTO",
  "RETURN", "CHOOSE", "FALLTHRU", "FALLTHROUGH", "WITH", "WHEN", "WAITFOR",
  "WAITUNTIL", "DISABLE", "ENABLE", "TRY", "THROW", "THROWRESUME", "AT",
  "ASM", "ALIGNAS", "ALIGNOF", "GENERIC", "STATICASSERT", "IDENTIFIER",
  "TYPEDIMname", "TYPEDEFname", "TYPEGENname", "TIMEOUT", "WAND", "WOR",
  "CATCH", "RECOVER", "CATCHRESUME", "FIXUP", "FINALLY", "INTEGERconstant",
  "CHARACTERconstant", "STRINGliteral", "DIRECTIVE",
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
  "mutex_statement", "when_clause", "when_clause_opt",
  "cast_expression_list", "timeout", "wor", "waitfor",
  "wor_waitfor_clause", "waitfor_statement", "wand", "waituntil",
  "waituntil_clause", "wand_waituntil_clause", "wor_waituntil_clause",
  "waituntil_statement", "exception_statement", "handler_clause",
  "handler_predicate_opt", "handler_key", "finally_clause",
  "exception_declaration", "enable_disable_statement",
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
  "general_function_declarator", "declaration_specifier", "invalid_types",
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
  "enum_type", "$@6", "$@7", "hide_opt", "enum_type_nobody",
  "enumerator_list", "visible_hide_opt", "enumerator_value_opt",
  "cfa_parameter_ellipsis_list_opt", "cfa_parameter_list",
  "cfa_abstract_parameter_list", "parameter_type_list_opt",
  "parameter_list", "cfa_parameter_declaration",
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
  "variable_type_ptr", "variable_type_array", "variable_type_function",
  "function_type_redeclarator", "function_type_no_ptr",
  "function_type_ptr", "function_type_array",
  "identifier_parameter_declarator", "identifier_parameter_ptr",
  "identifier_parameter_array", "identifier_parameter_function",
  "type_parameter_redeclarator", "typedef_name", "type_parameter_ptr",
  "type_parameter_array", "type_parameter_function", "abstract_declarator",
  "abstract_ptr", "abstract_array", "abstract_function", "array_dimension",
  "array_type_list", "upupeq", "multi_array_dimension",
  "abstract_parameter_declarator_opt", "abstract_parameter_declarator",
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
     395,   396,   397,   398,   399,   400,   401,   402,   403,   404,
     405,   125,    40,    64,    41,    46,    91,    93,    44,    58,
     123,    96,    94,    42,    38,    43,    45,    33,   126,    92,
      47,    37,    60,    62,   124,    63,    61,    59
};
# endif

#define YYPACT_NINF (-1836)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1102)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
      67, 12750,    76,   175,  9580,   130, -1836, -1836, -1836, -1836,
   -1836, -1836, -1836, -1836, -1836, -1836, -1836, -1836,   145,   945,
     165, -1836, -1836, -1836, -1836, -1836, -1836, -1836, -1836, -1836,
   -1836, -1836, -1836, -1836, -1836, -1836, -1836, -1836, -1836, -1836,
   -1836, -1836, -1836, -1836, -1836, -1836, -1836, -1836,   105,   332,
   -1836, -1836, -1836, -1836, -1836, -1836,  4753,  4753,   314, 12750,
     429,   496, 23583, -1836,   512, -1836, -1836, -1836, -1836, -1836,
   -1836, -1836, -1836, -1836, -1836,  2215, -1836,   598,   269, -1836,
   -1836, -1836, -1836, -1836, 17683, -1836, -1836,   519,   617,   404,
     385, -1836,  4753,   617,   617,   617,   552,  4484,   664,  1012,
   12912, -1836, -1836,   737, 17531,  1896, -1836, -1836, -1836,  2732,
     824, 10405, 11283,  1070,  2732,  1112,   694, -1836, -1836, -1836,
   -1836,   711, -1836, -1836, -1836, -1836,   728, -1836, -1836, -1836,
   -1836, -1836,   700,   675,   711, -1836,   711,   756, -1836, -1836,
   -1836, 18678,  4753, -1836, -1836,  4753, -1836, 12750, -1836,   725,
   18830, -1836, -1836,  4549, 20251, -1836, -1836,  1081,  1081,   774,
    2425, -1836, -1836, -1836, -1836,   462, 14969,  2696,   711, -1836,
   -1836, -1836, -1836, -1836, -1836,   815, -1836,   807,   818,   835,
   -1836,   897, 22974, -1836, -1836, -1836, -1836, -1836, -1836, -1836,
   16450,  3446,  2215,   474,   844,   882,   892,   902,   921,   928,
   -1836, -1836, 18982, 11691,   939, -1836, 17980, -1836, -1836, -1836,
   -1836,   947, -1836, -1836,   953, -1836, 21124,  1087, 21272, -1836,
     952,  4753,   675,   965,  1006,  1024,  1031, -1836, -1836, -1836,
    3632,  3774,  1033,  1106,    60,  1106, -1836,   711,   711,   -32,
      40,   107,  1106, -1836,   711,   711,   -32,   711, -1836,   711,
   -1836,  4173, -1836, -1836,   971,  1045,  1081, 20447, -1836, 17683,
   -1836, -1836,  2732, -1836,  1831,   694,  1047,  1124,    40,  4753,
    4753,   404, -1836, 14486, -1836,  1081,  1081,  1066,  1124,    40,
    4753, -1836, 23782, -1836, -1836, -1836,  1081, -1836, -1836, -1836,
   -1836,  1081, -1836,   906,  4274,  4753, -1836,  2085,  1090, -1836,
   -1836, -1836, 17344,   675,   129, -1836, -1836, 20396, -1836,  1106,
     294, -1836, 22974, 20251,  3869,  4173, -1836,   182, -1836, -1836,
   -1836, -1836, -1836, 18830,  4753, -1836,  1078, -1836, -1836, -1836,
   -1836,  4753,  2567,   683,   597, -1836,  4753,   807, -1836,   910,
     711,   711,  1107, 19134,   383, 15452, 20949,  2732,  2732, -1836,
    2732,  1081,  2732,  1081, -1836, -1836,   711, -1836,  1123, -1836,
   19286, -1836, -1836, -1836, 19438,   947, -1836,   -95,   788,   305,
     452,   694,  1077, -1836,  2425,  1114,   807,  2425,  1564, -1836,
    1143,  1196, 23048,  1174,  1206,  1209, 22974, 23122,  1212, 23687,
   -1836, -1836, -1836, -1836, -1836, -1836, 23196, 23196, 16294,  1159,
    4700, -1836, -1836, -1836, -1836,   254, -1836,   717, -1836,  1188,
   -1836, 22974, 22974, -1836,  1176,   868,  1014,  1021,   741,  1127,
    1202,  1211,  1213,  1249,   263, -1836,   744, -1836,  1234, -1836,
    1166,  3065, 16762, -1836, -1836,   828,  1234, -1836, -1836,   825,
   -1836, -1836,  3446,  1238,  1240,  1243,  1247,  1260,  1262, -1836,
   -1836,   283,  1251, -1836,   838,  1251, -1836, -1836, 18678, -1836,
    1220,  1265, 16918, -1836, -1836,  4849,  4188,  1285, 15452,  1295,
     757,   880, -1836, -1836, -1836, -1836, -1836,  4753,  4866, -1836,
   -1836, -1836, -1836, -1836, -1836, 17239,  3808,  1159, 21124,  1272,
    1277, -1836, -1836,  1280, 21272,   866, -1836, -1836, -1836,  9164,
    1292, -1836, -1836, -1836, -1836, -1836,  3632,   918,  1293,  1309,
    1319,   951,  1355,  1357,  1368,  1370,  1378,  1388,  3774, -1836,
   -1836, -1836,   711,  1384,  1371,  1412, -1836, -1836,  1418,   404,
   -1836, -1836,   675,  1124, -1836, -1836, -1836,   404, -1836, -1836,
     675, -1836, -1836,  4173, -1836, 16762, 16762, -1836,  1081,  4549,
   21037, 15613, -1836, -1836, -1836, -1836, -1836,   675,  1124,   294,
    1422, -1836, -1836,  2732,  1435,  1124,    40, -1836,   675,  1124,
   -1836, 23833, -1836,  1081,  1081, -1836, -1836,  1438,   472,  1453,
     694,  1458, -1836, 17844, -1836,   895, -1836,  1540, 20846, -1836,
    4549, 17432, 20447, -1836, 17344, 23270, -1836, -1836, -1836, -1836,
   -1836,  3869,  1003,  4173, -1836, 15613,  1106, 12750, -1836,  1465,
   -1836,  1481, -1836, -1836, -1836, -1836, -1836,  2425, -1836, -1836,
    1557,  4429,  2832, 19438, 11691, -1836, 19590, -1836,  1081,  1081,
   -1836, -1836,   947, -1836,   888,  1480,  1633, 22974,   905,  1418,
    1491, -1836,   711,   711, -1836,  1251, -1836, 19134, -1836, -1836,
   18275,  1081,  1081, -1836,  4429,   711, -1836, 20106, -1836, -1836,
   19286, -1836,   462, -1836, -1836, -1836,  1506,  4753,  1077,  1527,
     922, 18830,   964, -1836, -1836, -1836, -1836, -1836, -1836,  1002,
   -1836,  1539,  1516, -1836, 16606, -1836,  4700, 19742, 19742, -1836,
   16606, -1836, 22974, -1836, -1836, -1836, -1836, -1836, -1836, 16606,
   -1836, -1836, 18374, 19742, 19742,  1166,  1442,  1462,   745,  1550,
   -1836,  1009,  1559,  1221,  1561, -1836,  9164, 22974, 21346,  1544,
   22974,  2085, 22974,  2085, -1836,  2602, -1836, -1836, 21420,   749,
   22974, 21420,  2085, -1836, -1836, 22974, 22974, 22974, 22974, 22974,
   22974, 22974, 22974, 22974, 22974, 22974, 22974, 22974, 22974, 22974,
   22974, 22974, 22974, 22974, 21494,  1542,   897,  3659, 11691, -1836,
   -1836, -1836, -1836, -1836, -1836, -1836, -1836, -1836, -1836, -1836,
    1565, 22974, -1836, -1836,   828,  1207, -1836, -1836,   711,   711,
   -1836, -1836, 16762, -1836,   439,  1251, -1836,   984,  1251, -1836,
   -1836, -1836,  1418, -1836, -1836,  1418, 23344, -1836, -1836, 11691,
    1570,  1576,  2904,  1711,  3139,   479,  1491, -1836,   711,   711,
    1491,   490, -1836,   711,   711, 22974,  4753,  1253,  1269,  1491,
      43, 14808, 14808,  4753, -1836, -1836, 22974,  1280, -1836, 21124,
    1600, -1836,  1476, -1836, -1836, -1836, -1836, -1836,  1027, -1836,
   14808,  2085,  4549,  2085,  1052,  1599,  1602,  1604,  1058,  1606,
    1608,  1609,  1610,  1613,  1615,   520,  1251, -1836, -1836,   533,
    1251, -1836, -1836,   571,  1251, -1836, -1836, -1836,  4549,   897,
    1748,  1251, 20541, -1836, -1836,   675, 17844, -1836, -1836, -1836,
    1073,  1617,  1082,  1618, -1836,  1603, -1836,   675, -1836,  1623,
   -1836,   675,  1124,  1603, -1836,   675,  1619,  1622,  1625, -1836,
   -1836, 18275, -1836,  1624, -1836, -1836, -1836,  2085,  4753, 10759,
    1707,  1612, -1836, -1836, 19903, -1836,  1265, -1836, 14808,  1060,
   -1836, -1836,  1603, -1836, 18830, 16762,  1607, -1836,  1607, -1836,
   -1836, -1836,   305,   711,   711, -1836, 19286, -1836, 11856, 17074,
   -1836, 17844,  1636,  1637,  1639, -1836,  9991,   711, -1836,   905,
   -1836, -1836, -1836, -1836,  1418, -1836, -1836, -1836,  1081, -1836,
    3036, -1836, -1836,   694,   398,  1643,  1611,  1640,   305, -1836,
   -1836,  1641,  1648,  1564, 21420, -1836,  1649,  1646,   269,  1647,
    1652,  1654,  1651,  1659, 22974,  1661,  1662,  1664, 11691, 22974,
   -1836, -1836,  1732, -1836, -1836, -1836, 22974, -1836,  1666,  1668,
   21198,  1276, -1836, 21420,  1667, -1836,  1669, -1836, -1836,  2364,
   -1836, -1836,  1089, -1836, -1836, -1836, -1836,  2364, -1836, -1836,
    1283,   795, -1836, -1836,  1176,  1176,  1176,   868,   868,  1014,
    1014,  1021,  1021,  1021,  1021,   741,   741,  1127,  1202,  1211,
    1213,  1249, 22974,  1307, -1836,  1673,  2364, -1836, -1836, 21124,
   -1836, 17844,  1674,  1676,  1677,  1207, -1836, -1836, -1836, -1836,
   -1836, -1836, -1836, -1836,  1418, -1836, -1836,  1418, 17844, 17844,
   -1836, -1836,  2904,  1038,  1678,  1679,  1681,  1683,  1628,  3139,
   -1836, -1836, -1836, -1836, -1836, -1836, -1836, -1836, -1836, -1836,
   -1836, -1836, -1836, -1836,  1682, -1836,  1491, -1836, -1836, -1836,
   -1836, -1836, -1836, -1836, -1836,  1686,  1688, -1836,   404,  2364,
    1312,    15, -1836, -1836,  1672, -1836, 21272, -1836, 22974,   711,
   21568, 14808, -1836, -1836, -1836,  1621,   583,  1251, -1836,   625,
    1251, -1836, -1836,   628,  1251, -1836, -1836, -1836,  1418, -1836,
   -1836, -1836,  1418, -1836, -1836, -1836,  1418,  1106,  1690, -1836,
    1418,   249, -1836,  1234,  1687, -1836, -1836, -1836, -1836, -1836,
   -1836,  1696, -1836, -1836, -1836, 18830,  1603, -1836,   675, -1836,
   -1836, -1836, -1836, -1836, 13553,  1694,  1692, -1836,     5, -1836,
     702,   142, 11526,  1699, 15973,  1700,  1704,  1888,  2284,  2449,
   21642,  1705, -1836, -1836,  1706,  1708, -1836, -1836,   675, 22974,
   22974,  1847,  1714,   806, -1836, 16138,  1789,  1709,  1697, -1836,
   -1836, -1836, 10584, -1836, -1836, -1836, -1836, -1836,  1406, -1836,
   -1836, -1836,  1395,   544, -1836,   589, -1836,   544, -1836, -1836,
   -1836,  2085, -1836, -1836, 13074, 17683,  1716, -1836,  4753, -1836,
    1701,  1722,  1723, -1836,  1334, -1836, -1836, -1836, -1836,  4549,
   -1836, -1836,  1733,  1745,  1096, 18830,   807,   807,  1506,  1077,
    1077, -1836, -1836,  1159,  1265, 16918, -1836,  1234, -1836, 12021,
   -1836,   638,  1251, -1836,  1081, 12584, -1836, -1836,   305,   711,
     711,   462,  4753, -1836, 21716, -1836,   305,  1506,  1713, -1836,
   -1836,  1098,   816, 18275, 11691,  2085, -1836,   816, 18526,   816,
   -1836, 22974, 22974, 22974, -1836, -1836, -1836, -1836, 22974, 22974,
    1721, 21124, -1836, -1836,  1724,   827, -1836, -1836, -1836,  2462,
   -1836, -1836,  1338, -1836,   290, -1836, 21420,  1382, -1836,  9164,
   -1836, -1836, 22974,  1746,  1387,  1392,  1280, -1836,   640,  1251,
   -1836, -1836, 17844, 17844, -1836, -1836,  1755,   653,  1251, -1836,
     678,  3111,   711,   711, -1836, -1836, 17844, 17844, -1836,  1729,
   -1836, 15613, 15613,  1770,  1767,  1768,  1773, -1836,  1772, 22974,
   22974,  1397,  1771, -1836, -1836, -1836, -1836, -1836, -1836, -1836,
    1778, 22974, -1836, -1836, -1836,  1418, -1836, -1836, -1836,  1418,
   -1836, -1836, -1836,  1418, 17844, 17844, 17844,   404,   711, -1836,
   -1836,  1400, 22974, 20690,  1777,  1781,  1788, -1836, -1836, -1836,
    1792, 13708, 13863, 14018, 18830, 20447, 19742, 19742,  1793, -1836,
    1763,  1776,  2669, 14325, -1836,   115,  4753, -1836, -1836,  4753,
   -1836, 21420,   348,   434, -1836, -1836, -1836, -1836, 22974,  1796,
    1864, 11360, 10934, -1836,  1779, -1836,  1780, 22974,  1783, 21124,
    1785, 22974,  9164, 22974,  1061, -1836,  1786,   108, -1836,   378,
    1857,   540,  1798, -1836, -1836,  1803, -1836,  1794, -1836,  1795,
    1806,  1813, 15973, 15973, -1836, -1836,  1880, -1836, -1836,   282,
     282,    34, 14647,   711,   246, -1836, -1836, -1836,  1820, -1836,
    1824, -1836,  1826, -1836,  1822, -1836,  1823, -1836, -1836, -1836,
   -1836,  1827,  1825,  1828, 12186,  1830,  1832,  1833, -1836,  1829,
   -1836, -1836, -1836,  1418, 22974, 22974,  1265,  1836, -1836,  1506,
   -1836,  1077,   357,  1611, 21124, -1836,  1506,  1855, -1836, 18830,
   -1836,   982,  1854,  1851,  1111, -1836,  1859, -1836, -1836, -1836,
   -1836, -1836, 21124,  1280,  9164, -1836,  1861,  2364, -1836,  1861,
    1861, -1836,  2364,  3412,  3946, -1836, -1836,  1445, -1836, -1836,
   -1836,  1862,  1860, -1836, -1836, -1836,  1418, -1836, -1836,  1863,
    1867,   711, -1836, -1836, -1836,  1418, -1836, -1836, -1836,  1868,
   -1836, -1836, -1836, -1836, -1836, -1836, -1836, -1836, -1836, -1836,
   -1836, -1836, -1836,  1870, -1836, -1836, -1836, -1836,  1871,  1875,
     711, -1836, 17844, 17844, 17844, -1836, -1836, -1836, -1836, -1836,
   22974, -1836,   249, -1836,  1234, -1836, -1836, -1836,  1865,  1873,
   -1836,  1793,  1793,  1793,  3712,   987,  1807,   482, -1836,  3712,
     556, 16762, -1836, -1836, -1836,  4094, 22974,  4259,   572, -1836,
   -1836,   193,  1866,  1866,  1866,  4753, -1836, -1836, 18141, -1836,
    1117, -1836, -1836, -1836, -1836,  1118,  1879, 15973,  1709,  1878,
   22974,   519,  1877,   552, 14180, 18830, -1836, -1836, -1836,   779,
   15973, 22974,  1115,   396, -1836, 22974,  8773, -1836, -1836,   584,
   -1836,  1280, -1836,  1126,  1128,  1135, -1836, -1836, -1836, -1836,
     675,  1061,  1881, -1836, -1836, 22974, -1836,  1884,   897, -1836,
   11526, -1836, -1836, -1836, -1836, 22974, 22974, -1836, -1836,   780,
     282, -1836,   687,   544, -1836,  9730, -1836,   711, 15613, -1836,
   -1836, 18830, -1836, -1836, -1836,   305,   305, -1836, -1836, -1836,
    1882, -1836, 17844, -1836, -1836,  1891, -1836,  1895,  1887,  1077,
    1893, -1836, -1836,  1280,  1890, -1836, -1836,  1901, -1836, -1836,
   22974, -1836, 18526, 22974,  1280,  1907,  1448, -1836,  1450, -1836,
    2364, -1836,  2364, -1836, -1836, -1836, -1836, 17844,  1905,  1906,
   -1836, -1836, 17844, 17844,  1908,  1910,  1463, 15130, 15291, -1836,
    1904, -1836, -1836, -1836, -1836, -1836,  1913,  1914,  1916,  1468,
   22974, -1836, -1836, -1836, -1836, -1836,   610,   987,  2139,   644,
   -1836, -1836, -1836, -1836,   711,   711, -1836, -1836, -1836,   708,
   -1836,  1137,  4094,   777, -1836,  4259,   711, -1836, -1836, -1836,
   -1836, -1836, -1836, -1836, -1836, -1836, 15973,   411, 21790,  1996,
   15973,  1709, 15774, -1836, -1836, -1836, -1836, 22974, -1836, 21864,
    1998,  1898, 21047, 21938, 15973, 11109,  1709,   677,  1228,  1899,
   22974, -1836,  1926,   425, 15973, -1836, -1836,  1927, -1836, -1836,
    1911,   897,   814,  1929,  1930,  1483,  1931, 15973,  1935, 15973,
   15973, 15973, 15973, -1836, -1836, -1836, -1836,  4753,  4549,  1506,
    1506, -1836, -1836,  1924,  1932, -1836, -1836, -1836,  1934,   305,
    1940, -1836,  1944, -1836, -1836, -1836, -1836,  1946, -1836, -1836,
   -1836,  1488,  1490, -1836, -1836, -1836, -1836, -1836, -1836, -1836,
   -1836, -1836, -1836,  1945, -1836, -1836,  1947,  1948,  1949, -1836,
   -1836, -1836, -1836, -1836, -1836,  1950,  1951,  1952,  2139, -1836,
     711, -1836, -1836, -1836, -1836, -1836,  1942,  3712, -1836,  8027,
     123, 12354, -1836, 15868, -1836,    97,  1171, 15973,  2032,   723,
    1941,   680, 15973, 22974,  1958,   677,  1228,  1937, 23418,  1954,
     714,  2040, -1836, 22012, 22086, 22974,  1709,  1953, 12518, -1836,
   -1836, -1836, 19954, -1836,  1964,  1956,   340, 15973, -1836, 22974,
   21420, -1836, -1836, 22974,   544, -1836, -1836,   544, -1836, -1836,
    1966,  1974,  1981, -1836, -1836,   305,  1506, -1836, -1836, -1836,
   -1836, -1836,  1985,  1991,  1993, 15613,  1970, -1836, -1836, -1836,
     681,  1251, -1836, -1836,   987, -1836, -1836,    16, -1836,   363,
   -1836, -1836, -1836,  1999, 13236, -1836, -1836, 15973, -1836,    98,
   -1836, 15973, 22974,  2004, 22160, -1836, -1836, 22234, 22308, 22974,
    1958,  1709, 22382, 22456, 15973,  1992,   716,  1995,   781, -1836,
   -1836,  2013, 13236, 19954, -1836,  4417, 19590,  2085,  1990, -1836,
    2061,  2015,   865,  2010, -1836,  2094, -1836,  1178,  1180,   509,
     724, -1836, -1836, -1836,  1506,  2023, -1836, -1836, -1836, -1836,
   -1836, -1836, -1836,  1418, -1836, 22974, -1836, 22974, -1836, -1836,
    1577, 13398, -1836, -1836, 15973, -1836, -1836,  1709, -1836, -1836,
    1709,  2012,   793,  2014,   796, -1836, -1836,  1709, -1836,  1709,
   -1836,  2028, 22530, 22604, 22678, -1836,  1577, -1836,  2007,  3338,
    3645, -1836, -1836, -1836,   340,  2033, 22974,  2008,   340,   340,
   15973, -1836, -1836, 15973,  2116, 15973,  2127,  2053, -1836, 17844,
   -1836, -1836, 15868, -1836,  1577, -1836, -1836,  2052, 22752, 22826,
   22900, -1836, -1836,  1709, -1836,  1709, -1836,  1709, -1836,  2007,
   22974,  2054,  3645,  2047,   897,  2060, -1836,   896, -1836, -1836,
   -1836, 15973, -1836, 15973, -1836, -1836, -1836, 10240,  2056, 15868,
   -1836, -1836,  1709, -1836,  1709, -1836,  1709,  2066,  2064, -1836,
     675,   897,  2067, -1836,  2043,   897, -1836, -1836,  2069, -1836,
   -1836, -1836, 10408, -1836,   675, -1836, -1836,  1522, 22974, -1836,
    1183, -1836, -1836,   897,  2085,  2070,  2049, -1836, -1836,  1193,
   -1836, -1836,  2050,  2085, -1836, -1836
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,   482,     0,     2,   482,   499,   500,   501,   502,   503,
     504,   505,   506,   507,   488,   490,   489,   491,     0,     0,
       0,   508,   510,   531,   511,   532,   514,   515,   529,   530,
     509,   527,   528,   512,   513,   516,   517,   518,   519,   520,
     521,   522,   523,   524,   525,   526,   533,   534,   839,   536,
     609,   610,   613,   615,   611,   617,     0,     0,     0,   482,
       0,     0,    17,   580,   586,     9,    10,    11,    12,    13,
      14,    15,    16,   796,   103,     0,    20,     0,     2,   101,
     102,    18,    19,   855,   482,   797,   422,     0,   425,   720,
     427,   436,     0,   426,   456,   457,     0,     0,     0,     0,
     563,   484,   486,   492,   482,   494,   497,   548,   535,   466,
     541,   546,   468,   558,   467,   573,   577,   583,   562,   589,
     601,   839,   606,   607,   590,   661,   428,   429,     3,   804,
     817,   487,     0,     0,   839,   877,   839,     2,   894,   895,
     896,   482,     0,  1081,  1082,     0,     1,   482,    17,     0,
     482,   445,   446,     0,   563,   492,   476,   477,   478,   807,
       0,   612,   614,   616,   618,     0,   482,     0,   840,   841,
     608,   537,   713,   714,   712,   773,   768,   758,     0,     0,
     805,     0,     0,   499,   798,   802,   803,   799,   800,   801,
     482,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     581,   584,   482,   482,     0,  1083,   563,   884,   902,  1087,
    1080,  1078,  1085,   421,     0,   165,   726,   164,     0,   430,
       0,     0,     0,     0,     0,     0,     0,   420,   971,   972,
       0,     0,   455,   837,   839,   837,   858,   839,   839,   465,
       2,   839,   837,   915,   839,   839,   464,   839,   934,   839,
     912,     0,   556,   557,     0,     0,   482,   482,     2,   482,
     437,   485,   495,   549,     0,   578,     0,   820,     2,     0,
       0,   720,   438,   563,   542,   559,   574,     0,   820,     2,
       0,   498,   543,   550,   551,   469,   560,   471,   472,   470,
     565,   575,   579,     0,   593,     0,   790,     2,     2,   818,
     876,   878,   482,     0,     2,     2,  1091,   563,  1094,   837,
     837,     3,     0,   563,     0,     0,   448,   839,   832,   834,
     833,   835,     2,   482,     0,   794,     0,   754,   756,   755,
     757,     0,     0,   750,     0,   740,     0,   749,   760,     0,
     839,   839,     2,   482,  1102,   483,   482,   494,   473,   541,
     474,   566,   475,   573,   570,   591,   839,   592,     0,   701,
     482,   702,  1056,  1057,   482,   703,   705,   580,   586,   662,
     664,   665,   662,   842,     0,   771,   759,     0,   846,    22,
       0,    21,     0,     0,     0,     0,     0,     0,     0,    24,
      26,     4,     8,     5,     6,     7,     0,     0,   482,     2,
       0,   104,   105,   106,   107,    88,    25,    89,    43,    87,
     108,     0,     0,   123,   125,   129,   132,   135,   140,   143,
     145,   147,   149,   151,   153,   156,     0,    27,     0,   587,
       2,   108,   482,   157,   765,   716,   577,   718,   764,     0,
     715,   719,     0,     0,     0,     0,     0,     0,     0,   856,
     882,   839,   892,   900,   904,   910,     2,  1089,   482,  1092,
       2,   101,   482,     3,   700,     0,  1102,     0,   483,   541,
     566,   573,     3,     3,   682,   686,   696,   702,   703,     2,
     885,   903,  1079,     2,     2,    24,     0,     2,   726,    25,
       0,   724,   727,  1100,     0,     0,   733,   722,   721,     0,
       0,   822,     2,     2,     2,     2,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   861,
     918,   941,   839,     0,   461,     2,   857,   865,   999,   720,
     859,   860,     0,   820,     2,   914,   922,   720,   916,   917,
       0,   933,   935,     0,   451,   482,   482,   547,   483,     0,
     563,   482,  1084,  1088,  1086,   564,   794,     0,   820,   837,
       0,   431,   439,   496,     0,   820,     2,   794,     0,   820,
     769,   544,   545,   561,   576,   582,   585,   580,   586,   604,
     605,     0,   770,   482,   710,     0,   203,   414,   482,     3,
       0,   563,   482,   819,   482,     0,   433,     2,   434,   791,
     453,     0,     0,     0,     2,   482,   837,   482,   794,     0,
       2,     0,   753,   752,   751,   746,   493,     0,   744,   761,
     539,     0,     0,   482,   482,  1058,   483,   479,   480,   481,
    1062,  1053,  1054,  1060,     2,     2,   102,     0,  1018,  1032,
    1102,  1014,   839,   839,  1023,  1030,   708,   482,   571,   704,
     483,   567,   568,   572,     0,   839,  1068,   483,  1073,  1065,
     482,  1070,     0,   671,   663,   670,  1100,     0,   662,     0,
       0,   482,     0,   854,   853,   849,   851,   852,   850,     0,
     844,   847,     0,    23,   482,    95,     0,   482,   482,    90,
     482,    97,     0,    33,    37,    38,    34,    35,    36,   482,
      93,    94,   482,   482,   482,     2,   104,   105,     0,     0,
     183,     0,     0,   607,     0,  1078,     0,     0,     0,     0,
       0,     0,     0,     0,    56,     0,    62,    63,    67,     0,
       0,    67,     0,    91,    92,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   482,   166,
     167,   168,   169,   170,   171,   172,   173,   174,   175,   176,
     164,     0,   162,   163,     2,   983,   717,   980,   839,   839,
     988,   588,   482,   883,   839,   893,   901,   905,   911,     2,
     886,   888,   890,     2,   906,   908,     0,  1090,  1093,   482,
       0,     0,     2,   102,  1018,   839,  1102,   953,   839,   839,
    1102,   839,   968,   839,   839,     3,   704,     0,     0,  1102,
    1102,   482,   482,     0,     2,   735,     0,  1100,   732,  1101,
       0,   728,     0,     2,   731,   734,   180,   179,     0,     2,
     482,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   839,   870,   874,   913,   839,
     927,   931,   939,   839,   949,   862,   919,   942,     0,     0,
       0,   995,     0,   459,   823,     0,   482,   460,   824,   452,
       0,     0,     0,     0,   450,     2,   825,     0,   435,     2,
     794,     0,   820,     2,   826,     0,     0,     0,     0,   619,
     689,   483,     3,     3,   693,   692,   897,     0,     0,   482,
     415,     0,   465,   464,   563,     3,   101,     3,   482,     0,
       3,   795,     2,   748,   482,   482,   742,   741,   742,   540,
     538,   664,   662,   839,   839,  1064,   482,  1069,   483,   482,
    1055,   482,     0,     0,     0,  1033,     0,   839,  1103,  1019,
    1020,   709,  1016,  1017,  1031,  1059,  1063,  1061,   569,   604,
       0,  1067,  1072,   667,   662,     0,   672,     0,   662,   774,
     772,     0,     0,   846,    67,   806,     0,     0,     2,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   482,     0,
     122,   121,     0,   118,   117,    28,     0,    29,     0,     0,
       0,     0,     3,    67,     0,    52,     0,    53,    60,     0,
      59,    71,     0,    68,    69,    72,    55,     0,    54,    58,
       0,     0,    51,   124,   126,   127,   128,   130,   131,   133,
     134,   138,   139,   136,   137,   141,   142,   144,   146,   148,
     150,   152,     0,     0,   424,     0,     0,    30,     3,   726,
     158,   482,     0,     0,     0,   984,   985,   981,   982,   767,
     766,     2,   887,   889,   891,     2,   907,   909,   482,   482,
    1009,  1008,     2,     0,     0,     0,     0,     0,   839,  1019,
     956,   973,     2,   951,   959,   706,   954,   955,   707,     2,
     966,   976,   969,   970,     0,     3,  1102,   443,     2,  1095,
       2,   697,   698,   676,     3,     3,     3,     3,   720,     0,
     156,     0,     3,     3,     0,   729,     0,   723,     0,   839,
       0,   482,     3,   447,   449,     0,   839,   871,   875,   839,
     928,   932,   940,   839,   950,     2,   863,   866,   868,     2,
     920,   923,   925,     2,   943,   945,   947,   837,     0,   462,
     996,     3,  1000,  1001,     3,   828,     3,   553,   552,   555,
     554,     2,   795,   829,   776,   482,     2,   827,     0,   795,
     830,   619,   619,   619,   482,     0,     0,   711,     0,   418,
       0,     0,   482,     0,   338,     0,     0,     0,     0,     0,
     185,     0,   333,   334,     0,     0,   387,   386,     0,   160,
     160,   393,   580,   586,   200,   482,     0,   186,     0,   211,
     187,   188,   482,   205,   189,   190,   191,   192,     0,   193,
     194,   339,     0,   353,   195,   359,   361,   367,   196,   197,
     198,     0,   199,   207,   563,   482,     0,   209,     0,   412,
       0,     0,     0,     3,     0,   808,   795,   783,   784,     0,
       3,   779,     3,     3,     0,   482,   758,   758,  1100,   662,
     662,  1066,  1071,     2,   101,   482,     3,   578,     3,   483,
       3,   839,  1026,  1029,   482,     3,  1015,  1021,   662,   839,
     839,     0,     0,   650,     0,   666,   662,  1100,     2,   843,
     845,     0,    96,   482,   482,     0,   100,    98,   482,     0,
     112,     0,     0,     0,   116,   120,   119,   184,     0,     0,
       0,   726,   109,   177,     0,     0,    46,    47,    85,     0,
      85,    85,     0,    73,    75,    49,     0,     0,    45,     0,
      48,   155,     0,     0,     0,     0,  1100,     3,   839,   991,
     994,   986,   482,   482,     3,     3,     0,   839,   962,   965,
     839,     0,   839,   839,   957,   974,   482,   482,  1096,     0,
     699,   482,   482,     0,     0,     0,     0,   432,     3,     0,
       0,     0,     0,   725,   730,     3,   821,   182,   181,     3,
       0,     0,     2,   864,   867,   869,     2,   921,   924,   926,
       2,   944,   946,   948,   482,   482,   482,   720,   839,  1007,
    1006,     0,     0,     0,     0,     0,     0,     3,   795,   831,
       0,   482,   482,   482,   482,   482,   482,   482,   602,   631,
       3,     3,   632,   563,   620,     0,     0,   879,     2,     0,
     416,    67,     0,     0,   324,   325,   208,   210,     0,     0,
       0,   482,   482,   320,     0,   318,     0,     0,     0,   726,
       0,     0,     0,     0,     0,   161,     0,     0,   394,     0,
       0,     0,     0,     3,   215,     0,   206,     0,   315,     0,
       0,     0,   338,   338,   344,   343,   338,   355,   354,   338,
     338,     0,   563,   839,     0,   413,  1011,  1010,     0,     2,
       0,   786,     2,   781,     0,   782,     0,   762,   743,   747,
     745,     0,     0,     0,   482,     0,     0,     0,     3,     0,
       2,  1022,  1024,  1025,     0,     0,   101,     0,     3,  1100,
     656,   662,   672,   672,   726,   673,  1100,     0,   775,   482,
     848,  1012,     0,     0,     0,    39,     0,   113,   115,   114,
     111,   110,   726,  1100,     0,    66,    82,     0,    76,    83,
      84,    61,     0,     0,     0,    70,    57,     0,   154,   423,
      31,     0,     0,     2,   987,   989,   990,     3,     3,     0,
       0,   839,     2,   958,   960,   961,     2,   975,   977,     0,
     952,   967,     3,     3,  1097,     3,   684,   683,   687,  1099,
       2,     2,  1098,     0,     3,   836,   736,   737,     0,     0,
     839,   454,   482,   482,   482,     3,     3,     3,   463,   838,
       0,  1002,     0,  1003,  1004,   998,   936,   812,     2,     0,
     814,   602,   602,   602,   632,   639,   607,     0,   645,   632,
       0,   482,   594,   630,   626,     0,     0,     0,     0,   633,
     635,   839,   647,   647,   647,     0,   627,   643,   482,   419,
       0,   328,   329,   326,   327,     0,     0,   338,   225,     0,
       0,   227,   427,   226,   563,   482,   306,   305,   307,     0,
     338,   185,   265,     0,   258,     0,   185,   321,   319,     0,
     313,  1100,   322,     0,     0,     0,   375,   376,   377,   378,
       0,   368,     0,   369,   330,     0,   331,     0,     0,   358,
     482,   216,   204,   317,   316,     0,     0,   347,   357,     0,
     338,   360,     0,   362,   385,     0,   417,   839,   482,   810,
     763,   482,     2,     2,   655,   662,   662,  1074,  1075,  1076,
       0,  1027,   482,     3,     3,     0,  1035,     0,     0,   662,
       0,   669,   668,  1100,     0,   653,     3,     0,  1013,    99,
       0,    32,   482,     0,  1100,     0,     0,    86,     0,    74,
       0,    80,     0,    78,    44,   159,   992,   482,     0,     0,
     880,   898,   482,   482,     0,     0,     0,   482,   482,   739,
       0,   440,   442,     3,     3,     3,     0,     0,     0,     0,
       0,   778,   816,   598,   600,   596,     0,     0,  1042,     0,
     640,  1047,   642,  1039,   839,   839,   625,   646,   629,     0,
     628,     0,     0,     0,   649,     0,   839,   621,   636,   648,
     637,   638,   644,   691,   695,   694,   338,     0,     0,   246,
     338,   228,   563,   311,   309,   312,   308,     0,   310,     0,
     254,     0,   185,     0,   338,   482,   266,     0,   291,     0,
       0,   314,     0,     0,   338,   337,   379,     0,   370,     2,
       0,     0,     0,     0,   340,     0,     0,   338,     0,   338,
     338,   338,   338,   202,   201,   441,   780,     0,     0,  1100,
    1100,  1077,     3,     0,     0,  1034,  1036,   654,     0,   662,
       0,   652,     2,    50,    42,    40,    41,     0,    64,   178,
      77,     0,     0,     3,   881,   899,     3,     3,   963,   978,
     444,     2,   681,     3,   680,   738,     0,     0,     0,   872,
     929,   937,   997,  1005,   623,     0,     0,     0,  1043,  1044,
     839,   624,  1040,  1041,   622,   603,     0,     0,   336,     0,
       0,     0,   239,   338,   217,     0,     0,   338,   248,   263,
     274,   268,   338,   185,   303,     0,   278,     0,     0,   269,
     267,   256,   259,     0,     0,   185,   292,     0,     0,   220,
     335,     2,   482,   332,     0,     0,   395,   338,   345,     0,
      67,   356,   349,     0,   350,   348,   363,   364,   785,   787,
       0,     0,     0,  1037,  1038,   662,  1100,   674,   777,    65,
      81,    79,     0,     0,     0,   482,     0,   873,   930,   938,
     839,  1050,  1052,  1045,     0,   634,   234,   229,   232,     0,
     231,   238,   237,     0,   482,   241,   240,   338,   250,     0,
     247,   338,     0,     0,     0,   255,   260,     0,     0,   185,
     304,   279,     0,     0,   338,     0,   294,   295,   293,   262,
     323,     0,   482,   482,     3,   380,   483,   384,     0,   388,
       0,     0,     0,   396,   397,   223,   341,     0,     0,     0,
       0,   658,   660,  1028,  1100,     0,   993,   964,   979,   685,
       2,  1046,  1048,  1049,   641,     0,   236,     0,   235,   219,
     242,   482,   408,   251,   338,   252,   249,   264,   277,   275,
     271,   283,   281,   282,   280,   261,   276,   272,   273,   270,
     257,     0,     0,     0,     0,   222,   242,     3,   373,     0,
    1042,   381,   382,   383,   395,     0,     0,     0,   395,     0,
     338,   346,   342,   338,     0,   338,     0,     0,   659,   482,
     230,   233,   338,     3,   243,   409,   253,     0,     0,     0,
       0,   302,   300,   297,   301,   298,   299,   296,     3,   373,
       0,     0,  1043,     0,     0,     0,   389,     0,   398,   224,
     351,   338,   365,   338,   657,     3,   212,     0,     0,   338,
     290,   288,   285,   289,   286,   287,   284,     0,     0,   374,
       0,   401,     0,   399,     0,   401,   352,   366,     0,   214,
     213,   218,     0,   221,     0,   371,   402,     0,     0,   390,
       0,  1051,   372,     0,     0,     0,     0,   403,   404,     0,
     400,   391,     0,     0,   392,   405
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1836,  6668,  5800, -1836,    -1,   412,  1638,  -117, -1836,  -366,
   -1836,   477, -1836,  -713, -1836,   907,  -984,  -837, -1836,   362,
    5551,  2062, -1836,  1858, -1836,  1523,   569,   946,   948,   706,
     955,  1486,  1482,  1485,  1487,  1484, -1836,  -174,  -171,  8758,
    1041, -1836,  1812, -1836, -1836,  -693,  7002, -1073,  1235, -1836,
     225, -1836,  1034,    66, -1836, -1836, -1836,   547,   161, -1836,
   -1679, -1433,   397,   135, -1836, -1836, -1836,   410, -1566, -1836,
   -1419, -1836, -1836, -1836, -1836,  -371, -1117, -1836,   545, -1199,
     549, -1836, -1836, -1836, -1836, -1836,   782, -1176, -1836, -1836,
   -1836,   100,   576,   581,   209, -1836, -1836, -1836, -1836,  -909,
   -1836,   144,    79, -1836,   211, -1836,    -8, -1836, -1836, -1836,
    1037,  -696,  -835, -1343, -1836,   247, -1216,   134,  5848,  -804,
    -743, -1836,  -287, -1836, -1836,    27, -1836,  -162,    86,  -323,
    -242,  3850,   817,  -650,    20,    84,    88,   313,  2198, -1836,
    2227, -1836,    96,  3646, -1836,  2165, -1836,    92, -1836, -1836,
     394,   119,  4475,  2731,   -38,  2016,  -328, -1836, -1836, -1836,
   -1836, -1836,  -269,  5163,  4741, -1836,  -389,   333, -1836,  -582,
     341, -1836,   271,   869, -1836,    73,   -71, -1836, -1836, -1836,
    -356,  5429,  -907,  1330,   199,  -669,  -641,  -474,  1572, -1836,
   -1318,  -159,  -184,  1568,  1065,  4574,  -209,  -464,  -196,  -178,
    -461,  1471, -1836,  1802,   423,  1373,  1689, -1836, -1836, -1836,
   -1836,   467,  -155,    74,  -899, -1836,   229, -1836, -1836, -1130,
     586, -1836, -1836, -1836,  2304,  -774,  -380,  -841,   -11, -1836,
   -1836, -1836, -1836, -1836, -1836,   274,  -782,  -238, -1835,  -197,
    8377,   -73,  7183, -1836,  1332, -1836,  5577,   -20,  -220,  -200,
    -198,    11,   -74,   -70,   -69,   183,    -7,    17,    32,  -181,
      68,  -152,  -150,  -146,    72,  -128,  -104,  -100,  -699,  -736,
    -681,  -660,  -695,  -116,  -648, -1836, -1836,  -723,  1534,  1537,
    1538,  1569, -1836,   703,  7428, -1836,  -596,  -581,  -580,  -565,
    -705, -1836, -1601, -1750, -1737, -1734,  -616,   -65,  -314, -1836,
   -1836,   -13,   197,   -68, -1836,  8169,  1165,  -301,  -349
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,  1206,   224,   405,   406,    82,    83,   407,   381,   408,
    1534,  1535,   409,  1012,  1013,  1014,  1322,  1323,  1324,  1546,
     431,   411,   412,   413,   708,   709,   414,   415,   416,   417,
     418,   419,   420,   421,   422,   423,   424,   433,  1111,   710,
    1456,   771,   218,   773,   427,   838,  1207,  1208,  1209,  1210,
    1211,  1212,  1213,  2177,  1214,  1215,  1463,  1659,  2018,  2019,
    1942,  1943,  1944,  2143,  2144,  1216,  1673,  1674,  1675,  1838,
    1839,  1217,  1218,  1219,  1220,  1221,  1222,  1865,  1869,  1480,
    1472,  1223,  1224,  1479,  1473,  1225,  1226,  1227,  1228,  1229,
    1691,  2161,  1692,  1693,  2054,  1230,  1231,  1232,  1459,  2062,
    2063,  2064,  2207,  2219,  2090,  2091,   303,   304,   909,   910,
    1178,    85,    86,    87,    88,    89,    90,   464,    92,    93,
      94,    95,    96,   232,   233,   590,   285,   466,   435,   467,
      99,   313,   101,   102,   155,   346,   347,   106,   107,   170,
     108,   930,   348,   156,   111,   256,   112,   157,   264,   350,
     351,   352,   158,   428,   117,   118,   354,   119,   581,   898,
     896,   897,  1632,   355,   356,   122,   123,  1174,  1424,  1638,
    1639,  1799,  1800,  1425,  1627,  1818,  1640,   124,   668,  1739,
     665,   357,   666,   667,  1285,  1104,   472,   473,   902,   903,
     474,   475,   904,   359,   585,  1236,   437,   438,   219,   492,
     493,   494,   495,   496,   334,  1256,   335,   928,   926,   615,
     336,   375,   337,   338,   439,   126,   176,   177,   127,  1250,
    1251,  1252,  1253,     2,  1161,  1162,   607,  1245,   128,   325,
     326,   266,   277,   564,   129,   222,   130,   316,  1113,   888,
     526,   168,   131,   679,   680,   681,   132,   318,   236,   237,
     238,   319,   134,   135,   136,   137,   138,   139,   140,   241,
     320,   243,   244,   245,   321,   247,   248,   249,   806,   807,
     808,   809,   810,   250,   812,   813,   814,   776,   777,   778,
     779,   527,  1154,  1402,   141,  1747,   640,   641,   642,   643,
     644,   645,  1802,  1803,  1804,  1805,   630,   477,   362,   363,
     364,   440,   210,   143,   144,   145,   366,   830,   646
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      81,   194,   192,    81,   344,   195,   196,   358,   425,   713,
     508,   426,   133,   544,   547,   589,   669,   648,  1021,   476,
     625,   103,   376,  1001,  1476,  1258,  1254,   827,    97,  1461,
     509,   956,   510,  1327,   719,  1407,   524,   656,   529,   201,
     498,   659,   950,  1586,  1587,   537,   658,  1925,   180,   511,
     661,   372,  1056,   942,   943,    81,    81,   220,    81,   994,
    1926,  1287,  1334,  1927,   380,  -971,  1074,  -788,   197,   944,
     133,   209,  -971,   308,    81,   561,   146,   235,   512,   103,
     513,   881,   883,    81,   514,   104,    97,    98,  1460,   105,
     153,    81,   198,   113,   508,   207,    81,   109,  1661,    81,
    1084,   361,   515,    81,  2020,  1080,  1091,   199,   239,  1081,
    1850,   267,   596,   598,   509,   278,   510,  1450,   443,  1169,
     114,  1075,   444,   445,  2021,  1368,   516,  1237,  -820,  -410,
     517,   920,    58,   511,   459,    91,   311,   209,   152,   625,
     648,    81,  1076,   104,    81,    98,    81,   105,  1246,  1369,
    2085,   113,    81,  1107,  1077,   109,   194,   506,   133,    81,
     195,   196,   512,  1429,   513,   242,    81,   103,   514,   246,
     205,  1122,  2027,  2094,    97,  -789,   885,    58,   114,    58,
    1105,  1105,  1430,  1370,  1370,   446,   515,   893,   262,   714,
      81,    81,   907,    91,   298,   597,   275,  1929,   142,  1105,
    -410,   142,   207,  1432,  1433,    81,  1695,   970,   950,   447,
     516,   519,   297,  1233,   517,   489,   525,   480,  -411,   637,
      81,   942,   943,   197,   448,  1662,  1662,   306,   922,    81,
      81,   104,   533,    98,   260,   105,   205,   944,   272,   113,
     194,   601,   207,   109,   195,   196,   553,   198,   159,  1243,
      81,   151,  2020,  1843,    58,   576,   142,   166,    81,   534,
     558,  1291,   199,   525,  2026,    58,   114,   208,    81,    81,
     207,   569,    81,  1645,  2028,  2095,  1958,  1105,   631,    81,
     240,    91,  2022,   268,   565,  1696,   845,   279,   457,  -411,
    1315,   951,  1646,    81,    81,   519,    81,   160,   648,   520,
     935,   142,   298,   521,  1713,   879,   846,   915,   847,   937,
     553,   884,   967,    81,    81,   662,   831,   165,  1060,  1434,
     977,  1406,   648,    81,   955,   848,   207,  2013,  1410,   648,
      81,    81,  1341,   873,   604,    81,  1074,   961,   525,  1419,
    1019,   877,  1306,   549,   142,   552,   962,  1106,  1106,   525,
     811,   271,    20,  1277,   849,    58,   850,  1661,   299,  1709,
     851,   563,  1460,  1712,  2086,   965,  1106,   476,  1084,  1925,
    1420,  1519,  1195,    81,   208,   188,    81,   678,   852,  1526,
    1354,   845,  1926,   520,  1355,  1927,   592,   521,    98,   481,
     798,  1075,  1408,   523,   113,  1399,   753,   631,   109,  1746,
    1437,   846,   853,   847,  1429,  1490,   854,   562,  1281,   552,
     720,   619,  1076,  1553,   208,   721,   149,  1400,   281,   209,
     848,   114,   282,  1716,  1346,   286,   202,   291,  1963,  1964,
    1237,  1421,   443,   262,  1710,   789,   444,   445,   754,   525,
     476,    81,   208,   344,  1106,  1554,   597,   501,   201,   849,
     619,   850,  1379,   480,  2122,   851,   566,  1085,   379,  1586,
    1587,  1088,  1697,  2026,    81,    81,   179,   663,   174,   174,
    1101,  1102,   664,   852,   220,   976,    81,    81,   979,   980,
    1105,   981,  1548,  1277,  1662,    81,  1436,   489,  1939,  1940,
     983,   942,   943,   985,   986,   987,  2060,   853,   865,   446,
    2026,   854,  1939,  1940,   174,    81,   934,   944,   298,   570,
    1166,    58,   293,   532,   920,    74,  1233,    81,   205,  1929,
     540,  2087,  2088,   447,   582,  1651,  1114,   443,   593,   480,
    1698,   444,   445,  1284,  1461,   634,  2037,  2038,   448,   635,
     361,   557,    81,   221,   797,  -820,    79,   636,    81, -1101,
    1844,    58,   568,   215,   174,  1845,   262,   174,   892,   637,
     663,  2013,    58,  1758,   216,   664,  1512,  1619,    63,    64,
     235,  1941,   174,   648,   476,   281,  1419,  1419,  1419,   370,
     217,   181,   631,   865,  2133,  1968,   866,    81,  1336,    81,
     867,  1061,    58,  1460,   298,   525,   921,   470,  1841,  1195,
      81,   912,    81,  1849,  1124,    58,    81,  1420,  1420,  1420,
     648,  1653,  -651,  1261,   480,   476,    81,    77,   133,  -651,
      81,    81,  1262,   712,   190,   481,   297,   103,   449,  1662,
    1147,  1082,  -599,   174,    97,   635,  1557,   476,   476,  1045,
    1807,   425,  1089,    58,  1095,  1578,   635,  1106,   182,   956,
    1474,  1115,  1110,    81,  1474,    58,   476,  2142,   242,  1808,
     281,   282,   913,   652,   190,   291,    81,   251,  1421,  1421,
    1421,   866,  1135,  1475,   153,   867,   525,  1475,   592,   262,
      98,   174,   174,  2142,   275,  1139,   113,  2079,   811,   525,
     109,   104,   174,    98,  1699,   105,   213,    58,  1477,   113,
      58,   481,   697,   109,   200,    64,   579,   174,   939,   584,
      58,  2179,    58,   114,  1645,  1759,  1761,  1763,  1650,   576,
      81,  1478,    81,  1143,    81,    58,   114,   525,    81,   227,
    1816,    81,  1811,  1810,   476,  1382,   174,   628,   563,   525,
     651,    91,   996,   174,   174,  1660,  1676,  1360,   174,  1817,
      58,   616,  1148,    58,   628,   617,    81,   874,   628,  1684,
     161,  1851,  1871,   162,   163,   878,   164,    19,  1816,  1957,
      14,    15,    16,    17,    18,    -3,  1901,  1386,  1902,  1471,
    1390,   525,   886,    58,   525,  2069,   174,  1924,  2070,   174,
    1510,  1498,  1563,   894,   635,  1868,   525,   670,   697,  2135,
     672,    81,  1930,    81,   142,  1572,   270,   875,  1662,   525,
      52,    53,    54,    55,  1195,    81,   713,   612,  1879,  1880,
     920,  1931,    81,  1833,  1834,  1835,   344,  -476,   489,    58,
    1576,    81,   887,  2080,   635,   298,  1662,   525,   996,   891,
      81,    81,    81,   895,  1280,  1836,   613,   614,  2034,   293,
    1543,  1756,   297,   148,  1431,  1867,   628,    65,    66,    67,
      68,    69,    70,    71,    72,  1016,  1816,    81,  1331,   743,
     744,  1470,   996,   722,   996,  1662,   281,   174,   723,   187,
    2036,   996,  2043,   939,  2112,  1934,   295,  1585,  1868,   174,
     174,  -480,  2049,    14,    15,    16,    17,    18,   755,   989,
    2032,   312,   756,  1502,  1503,  1017,    81,    81,   489,   712,
     990,   991,  1367,   745,   746,   712,  -410,   261,    14,    15,
      16,    17,    18,   361,   712,  1833,  1834,  1835,   283,   103,
     290,   449,   292,   525,  -809,  1748,    97,   476,  1374,   996,
     190,   648,   955,   712,  1110,   470,  1330,  1836,  -972,  2114,
    1397,   996,    58,  1326,   996,  -972,  1837,  1501,   190,    81,
      74,  2148,  1353,   811,  2150,  -714,  2105,   374,  1975,  1310,
     377,   261,   678,  1976,   290,   292,  1311,    58,  1545,   781,
     774,   332,  1996,   782,   525,  1326,  1527,   378,  1681,   262,
     793,    79,    80,  1235,   525,    98,   714,   105,   450,   470,
     149,   113,   563,   161,  1660,   109,   162,   163,    81,   164,
    1249,  1491,   575,    64,  -481,   379,    81,   628,   470,  2127,
      74,   832,   833,   261,  2128,   834,   262,  1626,   114,   174,
     920,   736,  1514,   931,   933,  1561,   451,    74,   737,   738,
     634,   628,  1796,    91,   635,    81,   452,  1809,   489,   906,
    2194,    79,    80,   907,   628,  2195,   453,   634,  1248,   252,
     253,   635,   254,  1743,   620,   293,   959,   255,    79,   636,
     297,    81,   449,  -477,   525,   454,   969,    81,    81,   174,
     617,  1754,   455,    14,    15,    16,    17,    18,  2074,  1536,
     261,   479,   290,   292,    14,    15,    16,    17,    18,   483,
    1155,   376,   376,   534,   499,   858,   142,   525,    81,   497,
     425,   484,  1163,  1525,    74,  -478,  1167,   502,   971,    74,
    1170,   142,   617,   545,   261,    14,    15,    16,    17,    18,
     261,  1531,   344,  1005,   634,  1007,  1065,  1010,   635,  1797,
     525,  1018,    58,   525,  1022,    79,   636,   741,   742,  1676,
      79,    80,   470,    58,  1663,   604,   972,   449,  1558,   525,
     973,  1426,   261,   995,   503,  1740,  1168,   996,   653,  1047,
     292,  1247,  1686,  1687,  1688,  1689,  1690,   476,   476,   739,
     740,  1119,   504,   489,    58,  1120,    81,    81,    81,   505,
    1082,   522,   449,   470,   635,   425,   425,   546,  1594,  1595,
     631,  1608,   103,  1588,   297,   523,   696,   556,   525,    97,
     534,   489,   604,   220,   525,  2163,   525,    81,  1738,  2167,
      14,    15,    16,    17,    18,  1744,   567,  1157,   174,   361,
      81,   996,   103,    81,    81,   174,  1159,    81,   608,    97,
     996,   586,  1755,  1325,   664,   267,   278,  1326,    81,   212,
    1497,  1249,  1530,  1123,   782,  1125,  1326,   747,   748,   261,
    1422,  1833,  1834,  1835,   623,  1751,  1235,  2067,    98,  1752,
     105,  1826,  1827,   996,   113,  1326,   996,   655,   109,    58,
    1853,    81,  1854,  1836,   996,   261,  1120,   653,   292,  1855,
     671,  1935,  1842,   996,    81,   782,  1235,   682,    98,  1248,
     105,   114,   696,  1913,   113,  1024,  1025,  1026,   109,   724,
     489,   725,   726,   727,   683,   212,    91,   716,    81,  1177,
     174,   174,   262,   202,   716,  2029,   686,   275,  2092,   996,
     628,   114,  2131,   651,  2132,   261,  1326,  2216,   996,    74,
     728,  2213,   344,   729,   730,   735,    91,  2222,   731,   732,
      81,  2223,  1793,  1794,  1795,   441,  2092,   563,   687,   774,
     261,   688,   142,   525,   692,   261,   749,   261,   260,   272,
      79,    80,  1279,   750,  1833,  1834,  1835,   623,   716,   142,
    1852,   752,   470,  1888,   998,   999,   996,   751,   261,   757,
     261,   261,   783,  1409,   784,  2145,  1836,   785,  1426,  1426,
    1426,   786,  1642,  1628,  1426,  -186,  1435,   456,   261,   142,
    1097,  1098,  1247,  1663,   787,   508,   788,   268,   279,   815,
     261,    81,    -3,  1454,   554,    81,  1099,  1100,    81,  -479,
     712,   -18,   142,  1313,  1120,   509,   828,   510,   829,   361,
    1328,  1329,  1890,   261,   839,   653,   292,   855,   489,  1031,
    1032,  1033,  1034,  1897,   511,  -122,  -122,  -122,  -122,  -122,
    -122,   103,   103,   856,  1824,   996,  1332,   261,   653,  -157,
    -157,   489,   489,   857,   261,  -121,  -121,  -121,  -121,  -121,
    -121,    81,   271,   512,  1467,   513,  1470,  1471,   554,   514,
    1643,  1099,  1489,   565,  1644,  1551,  1552,  1422,  1422,  1422,
     153,  1624,  1625,  1629,  1411,  1412,  1413,   515,   633,   859,
     148,   860,   172,   173,    65,    66,    67,    68,    69,    70,
      71,    72,   861,   489,   862,  1665,  1665,    98,    98,   105,
     105,   516,   863,   113,   113,   517,   869,   109,   109,  1556,
    1552,   489,   864,  1536,  1560,  1552,    81,   870,   152,  1071,
    1544,    81,    81,    81,  1596,  1544,   344,  1071,  1610,  1588,
     114,   114,  1812,    14,    15,    16,    17,    18,   993,   871,
     563,  1819,  1819,  1819,   305,    91,    91,   673,  1990,  1991,
     148,  1862,   889,  1468,    65,    66,    67,    68,    69,    70,
      71,    72,   845,   476,   476,   890,   174,   212,  -597,   174,
     174,   174,  1764,  1120,  1642,  1899,  1120,  1900,  1552,  1642,
     908,   142,   846,  -595,   847,  1249,   562,   519,   899,  1588,
    1910,  1911,   923,    81,   174,  1922,   996,   633,    81,    76,
     174,   848,   824,   925,    81,   929,    81,   945,   142,   142,
    1989,  1979,  1980,   584,    81,  2000,  1552,  2001,  1552,  2055,
     174,   947,   674,   361,  1939,  1940,   489,  1652,  1654,   628,
     849,   151,   850,  1248,   964,   566,   851,   637,   675,   489,
     676,   677,    65,    66,    67,    68,    69,    70,    71,    72,
    2213,  2214,  1549,  1550,   852,  1027,  1028,   968,   470,  1029,
    1030,   974,  1643,   975,   174,  2075,  1644,  1643,  2134,  2136,
      58,  1644,  1035,  1036,  1003,   520,  1714,  1532,   853,   521,
     441,   441,   854,   997,   489,  1000,  1820,  1821,   261,  1044,
     103,  1741,  1742,  1499,  1500,  1049,   142,  1070,   648,  1078,
    2055,   261,   148,  1071,   228,   229,    65,    66,    67,    68,
      69,    70,    71,    72,  1974,    14,    15,    16,    17,    18,
    1305,  1117,   262,  1126,  -792,   261,  1127,   275,  1128,    81,
    1129,    81,  1130,  1131,  1132,   425,   261,  1133,  2017,  1134,
    1149,  1158,  1160,  2137,  1164,   261,  1247,  1238,  -690,  1171,
    1351,    76,  1172,  1255,  1665,  1173,    98,  1284,   105,  1239,
    1271,  1272,   113,  1273,  1283,   865,   109,  1381,   260,   272,
    1286,  1288,  1289,  1292,  1293,  1295,  1296,  1249,  1297,  1298,
     536,    81,   957,  1299,    81,  1301,  1302,  2121,  1303,   114,
    1308,   476,  1309,  1373,  1316,   489,  1317,  1333,  1338,   489,
    1339,  1340,  1347,  1348,    91,  1349,   441,  1350,   174,  1358,
    -678,   174,  -677,   489,  1398,  1403,  1588,  -793,  1427,    19,
    1428,  1438,  1441,   489,   490,  1248,  1442,  1451,  1452,  2061,
    1453,  1458,   142,  1462,  1528,   103,   489,   996,   489,   489,
     489,   489,   261,  -713,  1464,  1483,    81,    81,  1485,  1486,
    1487,  1542,  1544,   866,   174,   174,  1584,   867,    48,    49,
      50,    51,    52,    53,    54,    55,   261,   142,   470,   508,
     183,     6,     7,     8,     9,    10,    11,    12,    13,  1571,
    1493,   425,   271,   425,  2140,  1856,  2017,  1642,   142,   509,
     563,   510,  1495,  1559,  1589,  1590,  1591,  1592,  1597,  1665,
    1552,    98,  1600,   105,  1615,  1616,    81,   113,   511,  1617,
    1633,   109,   489,  1620,  1656,  1631,   489,   441,  1431,  1471,
    1700,   489,   425,  1634,  1702,  2165,  1677,  1678,  1705,  2057,
    1680,   280,  1682,  1694,   114,  1706,   562,   512,  1247,   513,
    1195,  1703,  1704,   514,  1717,  1719,   489,  1720,  1724,    91,
    1722,  1723,  1757,  1731,  1806,  1725,  2189,  1727,  1726,  1728,
    1729,   515,   148,  1736,   172,   173,    65,    66,    67,    68,
      69,    70,    71,    72,   780,  1643,  1745,  2061,  1749,  1644,
    1750,  2061,  2061,  1765,  1766,   516,  1791,  1770,  1753,   517,
     791,  1771,   449,   794,  1792,  1636,   489,  1779,  1596,  1781,
     489,  1828,  1830,  1859,   425,   221,  1861,  2215,  1887,  1881,
    2057,  1891,   142,   489,   103,   194,   601,  2192,  1885,   195,
     196,   261,  1886,  1889,    81,  1893,    81,   174,  1898,  1904,
    1905,  1915,  1908,    84,  1909,  1443,   150,  1919,  1920,   174,
    1921,  1947,   103,  1952,  2206,  1953,  1965,  1967,  2206,  1971,
     536,  1993,   174,  1977,  1978,  1981,   261,  1983,  1973,  1994,
     441,  1997,   261,   489,  1995,  1998,  2217,  1999,   525,  -679,
     519,  2007,  2008,  2009,  2010,  2011,  2012,  2031,  1665,  2033,
      98,   103,   105,  -580,  2039,  2044,   113,  2071,    81,    81,
     109,    84,  2042,  2058,   825,  2072,   490,   174,  1911,   489,
    2050,   207,   489,  2059,   489,  2073,  1665,   191,    98,  2076,
     105,   489,   865,   114,   113,  2077,    84,  2078,   109,  2124,
    2089,   905,    14,    15,    16,    17,    18,  2098,    91,   231,
    2111,    81,   259,  2113,  2115,  2125,    84,  2126,  2129,  2130,
     489,   114,   489,   480,  2138,  1665,   489,    98,   489,   105,
    2147,  2151,  2149,   113,  2160,  2166,    91,   109,   520,   148,
    2164,  2171,   521,    65,    66,    67,    68,    69,    70,    71,
      72,   489,  2173,   150,  2174,  2180,  2191,  2201,  2190,    84,
     114,    58,   150,    81,  2193,   315,   323,  2203,  2204,  2208,
    2209,   142,    81,  2211,  2220,    91,  2221,  2224,   343,  1895,
     866,   992,  1038,  1555,   867,  1037,  1039,  1041,   174,  1040,
     261,  1457,   174,   772,   689,  2202,  1466,  1863,  2141,   142,
    1969,  2158,   432,   191,   191,  1962,   174,  1872,  1870,  2188,
     189,  1711,  2117,  2116,   150,   462,   174,  1857,   259,   733,
     734,    74,  1858,  2168,  2210,  1484,   171,   288,  2015,   174,
     555,   174,   174,   174,   174,  2084,  1630,    58,   142,   174,
     733,  1797,   231,   231,  1282,   525,  1481,   835,   263,   261,
    1116,  1257,    79,    80,     3,  1290,   927,  1876,  1052,   284,
     287,  1053,  1054,   315,     0,  1790,     0,     0,     0,   148,
     733,    84,     0,    65,    66,    67,    68,    69,    70,    71,
      72,     0,     0,     0,     0,   259,     0,     0,     0,     0,
       0,     0,     0,   780,   780,     0,     0,    74,     0,     0,
       0,     0,   263,  1063,     0,   174,  1066,     0,     0,   174,
       0,     0,     0,     0,   174,     0,   628,    75,    76,   323,
       0,     0,     0,     0,     0,   323,   315,   315,    79,    80,
       0,     0,     0,     0,     0,   150,     0,     0,   148,   174,
     172,   173,    65,    66,    67,    68,    69,    70,    71,    72,
       0,     0,     0,     0,   263,   343,   638,   647,     0,     0,
       0,     0,     0,     0,     0,  2205,     0,   536,     0,  1440,
     441,     0,   343,     0,  1137,     0,   343,     0,  1141,  2212,
     957,     0,  1145,     0,     0,     0,     0,     0,     0,   174,
       0,     0,     0,   174,   905,     0,     0,   628,  1156,     0,
       0,     0,     0,     0,     0,     0,   174,     0,     0,     0,
     432,  1445,     0,     0,     0,     0,     0,   490,   148,  2123,
     825,   263,    65,    66,    67,    68,    69,    70,    71,    72,
    1318,   261,     0,     0,  1319,     0,  1320,     0,     0,     0,
     327,   328,   329,   330,   432,     0,     0,   775,     0,     0,
       0,     0,     0,     0,   191,   263,   174,     0,     0,   905,
       0,   263,     0,  1270,     0,     0,     0,    76,     0,     0,
     150,     0,     0,     0,   462,     0,     0,     0,   804,   148,
     647,   172,   173,    65,    66,    67,    68,    69,    70,    71,
      72,     0,   174,   263,     0,   174,     0,   174,     0,     0,
       0,     0,     0,   148,   174,   172,   173,    65,    66,    67,
      68,    69,    70,    71,    72,     0,   148,     0,   231,     0,
      65,    66,    67,    68,    69,    70,    71,    72,  1318,     0,
     231,   331,  1319,   174,  1320,   174,     0,   698,     0,   174,
       0,   174,     0,  1023,     0,     0,     0,     0,     0,   332,
       0,     0,     0,     0,     0,   315,     0,   432,   432,     0,
       0,   315,  1447,   343,   174,    76,     0,     0,  1547,   905,
       0,     0,     0,  1337,   780,     0,  2218,     0,     0,     0,
       0,     0,   261,     0,     0,  2225,   905,   905,     0,     0,
    1344,  1345,     0,     0,     0,     0,     0,  1321,     0,   261,
       0,     0,   315,     0,     0,  1321,     0,     0,     0,     0,
       0,     0,     0,   315,     0,   315,   263,   343,     0,    84,
       0,   148,   251,   172,   173,    65,    66,    67,    68,    69,
      70,    71,    72,   698,  1321,   343,   462,   490,   647,     0,
       0,     0,     0,     0,     0,  1384,   638,     0,  1388,     0,
     638,     0,  1392,     0,     0,     0,   148,  1707,  1708,   343,
      65,    66,    67,    68,    69,    70,    71,    72,  1008,   647,
       0,     0,   343,     0,     0,     0,     0,   610,     0,     0,
       0,     0,   116,   150,     0,   116,   183,     6,     7,     8,
       9,    10,    11,    12,    13,   261,   432,  1321,   263,   150,
     150,     0,   432,     0,     0,     0,     0,     0,  1009,     0,
       0,   432,     0,     0,   150,   150,   150,     0,     0,   263,
       0,     0,     0,   148,     0,   228,   229,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,     0,   263,
     116,     0,     0,     0,     0,     0,   441,     0,     0,     0,
     148,    74,   367,   368,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,     0,   116,     0,     0,     0,     0,
     462,  1635,    76,     0,   263,     0,     0,     0,  1636,     0,
       0,   265,    79,    80,     0,   116,   775,   775,     0,     0,
       0,     0,  1300,     0,   432,     0,     0,  1304,   263,     0,
       0,    77,     0,     0,     0,   263,   369,     0,  1312,     0,
       0,   462,     0,     0,   804,     0,   804,     0,     0,     0,
       0,     0,   116,   261,     0,     0,     0,     0,   116,     0,
       0,   116,     0,   343,   343,   265,     0,     0,     0,     0,
       0,     0,  1829,     0,     0,     0,   339,   116,   371,     0,
       0,     0,   343,     0,   315,  1840,     0,  1565,     0,     0,
     905,   905,     0,     0,  1567,  1568,  1574,     0,     0,     0,
       0,   436,     0,     0,   905,   905,     0,     0,  1582,  1583,
     315,     0,     0,   116,   436,     0,   148,   265,   228,   229,
      65,    66,    67,    68,    69,    70,    71,    72,     0,   490,
    1874,     0,     0,     0,     0,     0,     0,  1321,     0,     0,
       0,     0,   905,   905,   905,     0,  1605,  1606,  1607,     0,
       0,   432,     0,     0,     0,     0,     0,     0,     0,   261,
     343,     0,     0,     0,     0,     0,   150,   432,   116,     0,
     116,     0,   932,     0,     0,     0,     0,     0,   343,     0,
    1265,     0,     0,     0,   265,     0,     0,     0,   148,     0,
       0,   638,    65,    66,    67,    68,    69,    70,    71,    72,
       0,     0,     0,     0,     0,   580,     0,     0,     0,     0,
       0,     0,     0,   116,     0,     0,    74,     0,   265,     0,
     689,     0,     0,     0,   265,     0,     0,     0,     0,     0,
     462,     0,     0,     0,   116,     0,  1072,    76,     0,     0,
     635,  1938,     0,     0,     0,  1948,     0,    79,    80,     0,
    1152,     0,     0,     0,   116,     0,   265,   116,     0,  1961,
       0,     0,     0,     0,     0,     0,     0,   490,     0,  1970,
       0,   116,     0,     0,     0,   116,     0,     0,     0,   263,
       0,     0,  1982,     0,  1984,  1985,  1986,  1987,     0,     0,
       0,     0,   263,     0,     0,     0,     0,   775,     0,     0,
       0,     0,     0,   733,     0,     0,     0,     0,     0,   436,
       0,     0,     0,     0,   804,     0,   263,     0,     0,     0,
     148,   804,   228,   229,    65,    66,    67,    68,    69,    70,
      71,    72,    14,    15,    16,    17,    18,     0,     0,  1537,
    1538,  1539,   490,   436,     0,     0,  1540,  1541,     0,     0,
     905,   905,   905,     0,  1783,  1784,  1785,     0,  2025,     0,
     490,     0,  2030,   343,     0,  1321,     0,  2035,     0,   116,
    1321,  1321,  1321,   436,  1801,     0,  1278,     0,     0,   265,
     759,   760,   761,   762,   763,   764,   765,   766,   767,   768,
     769,    58,  2065,     0,   215,   148,  1825,     0,     0,    65,
      66,    67,    68,    69,    70,    71,    72,   150,     0,     0,
       0,     0,     0,     0,     0,     0,   150,     0,     0,     0,
       0,   770,     0,   148,   432,   228,   229,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,     0,     0,
       0,     0,  2093,  1351,    76,     0,  2096,   432,     0,     0,
       0,    74,     0,     0,   432,     0,   436,   436,     0,  2110,
       0,   265,   116,     0,     0,     0,     0,     0,     0,     0,
       0,   802,    76,     0,     0,   635,   259,    84,     0,     0,
     905,     0,    79,   803,  1882,     0,     0,     0,     0,     0,
       0,   315,     0,     0,   116,     0,     0,   150,     0,   116,
       0,     0,   265,   116,     0,   116,     0,   462,     0,  2146,
       0,     0,     0,     0,     0,   905,   116,     0,   116,  1903,
     905,   905,     0,     0,  1906,  1907,     0,     0,     0,     0,
       0,     0,   371,     0,   116,   436,   462,   265,     0,     0,
     150,     0,     0,     0,     0,  2169,  1801,  1801,  2170,     0,
    2172,     0,     0,     0,     0,     0,     0,  2176,   116,     0,
       0,   265,     0,     0,     0,   580,     0,     0,   265,     0,
       0,   116,     0,   963,     0,     0,     0,     0,  1321,     0,
    1321,     0,   116,     0,     0,     0,  2196,     0,  2197,     0,
      58,     0,  2200,     0,  2176,   436,     0,     0,   116,   116,
       0,   436,     0,   343,   343,     0,     0,     0,     0,     0,
     436,     0,   263,   116,   116,   116,     0,  2200,     0,     0,
       0,     0,   148,     0,   228,   229,    65,    66,    67,    68,
      69,    70,    71,    72,     0,     0,     0,     0,     0,    14,
      15,    16,    17,    18,     0,     0,     0,   263,     0,     0,
      74,     0,     0,   150,   150,   150,   150,     0,   150,   150,
       0,     0,     0,     0,  1637,   323,     0,     0,     0,   436,
    2119,    76,     0,     0,   525,     0,     0,  1801,     0,     0,
       0,    79,    80,   432,   432,     0,     0,     0,     0,     0,
       0,     0,     0,   436,     0,     0,   148,     0,    58,     0,
      65,    66,    67,    68,    69,    70,    71,    72,  1318,     0,
     436,     0,  1319,     0,  1320,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   259,     0,     0,     0,     0,     0,
     148,     0,   116,   116,    65,    66,    67,    68,    69,    70,
      71,    72,     0,  1864,  1866,    76,   462,     0,  1760,     0,
       0,   116,     0,     0,     0,     0,     0,     0,    74,  2082,
       0,     0,     0,  1801,     0,     0,     0,     0,     0,     0,
       0,   150,     0,   638,     0,     0,     0,     0,    75,    76,
       0,  1613,     0,  1153,     0,     0,     0,   116,     0,    79,
      80,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   263,     0,     0,  1801,     0,     0,     0,     0,     0,
       0,     0,   265,     0,     0,     0,     0,     0,     0,     0,
     436,     0,     0,     0,     0,   265,     0,   110,     0,   116,
       0,     0,     0,     0,     0,   116,   436,     0,    14,    15,
      16,    17,    18,     0,     0,     0,     0,   116,     0,  1267,
     436,     0,   116,     0,     0,     0,     0,     0,     0,     0,
     263,     0,     0,     0,     0,     0,  1637,  1798,  1801,  1801,
       0,  1637,     0,   432,     0,     0,     0,  1637,     0,  1637,
       0,     0,     0,     0,    58,   110,     0,   905,     0,     0,
       0,  2175,     0,     0,     0,     0,     0,    58,     0,   436,
       0,     0,     0,     0,     0,     0,   323,   150,     0,     0,
       0,  1801,     0,     0,     0,     0,   148,     0,   228,   229,
      65,    66,    67,    68,    69,    70,    71,    72,     0,   148,
     274,   228,   229,    65,    66,    67,    68,    69,    70,    71,
      72,     0,   432,   148,    74,   575,    64,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,    74,     0,     0,
     343,     0,   116,   150,   230,    76,     0,    14,    15,    16,
      17,    18,     0,   110,     0,    79,    80,  2119,    76,   116,
     116,   525,     0,     0,     0,     0,     0,     0,    79,    80,
       0,     0,   349,     0,   150,  1046,   148,     0,   228,   229,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  2066,     0,   343,
     343,     0,     0,     0,    74,     0,    58,     0,     0,   469,
       0,   100,   116,     0,   154,     0,     0,     0,     0,  1798,
    1798,     0,   263,     0,  1635,    76,     0,     0,     0,     0,
       0,  1636,     0,     0,  1637,    79,    80,  1637,   148,     0,
     228,   229,    65,    66,    67,    68,    69,    70,    71,    72,
       0,     0,     0,     0,   323,     0,   116,     0,     0,     0,
       0,     0,     0,     0,     0,   116,    74,   432,     0,   100,
       0,     0,   148,   436,   200,    64,    65,    66,    67,    68,
      69,    70,    71,    72,     0,     0,   230,    76,   572,     0,
       0,     0,     0,     0,   206,     0,   436,    79,    80,     0,
     315,    58,     0,   436,     0,     0,     0,     0,   110,     0,
       0,     0,     0,     0,   273,     0,     0,     0,     0,     0,
       0,    76,     0,     0,   824,   265,   116,     0,     0,     0,
       0,     0,     0,   148,     0,   228,   229,    65,    66,    67,
      68,    69,    70,    71,    72,     0,   116,     0,     0,   627,
    1798,   307,   274,     0,     0,     0,   436,   100,     0,  1637,
    1267,    74,     0,     0,     0,     0,   627,     0,     0,     0,
     627,     0,  1522,     0,     0,     0,   345,     0,     0,     0,
       0,   314,    76,     0,   116,   436,     0,     0,     0,   116,
     263,     0,    79,    80,   150,     0,     0,     0,     0,     0,
       0,   442,     0,     0,     0,     0,     0,     0,     0,     0,
     148,     0,   307,   468,    65,    66,    67,    68,    69,    70,
      71,    72,  1318,     0,     0,     0,  1319,   343,  1320,     0,
       0,     0,     0,   116,   116,     0,  1798,     0,     0,     0,
       0,   518,     0,     0,     0,     0,   150,   116,   116,     0,
       0,     0,   116,   116,     0,     0,     0,     0,     0,    76,
       0,   543,  1762,     0,     0,     0,   548,   550,   627,   206,
       0,     0,     0,     0,   150,   150,     0,  2120,   323,     0,
       0,     0,     0,     0,     0,   116,   116,   116,     0,     0,
       0,     0,   571,     0,  1614,     0,   573,     0,     0,     0,
       0,   574,   116,   116,   116,   116,   116,   116,   116,     0,
       0,     0,   591,   150,   265,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   603,    58,     0,     0,     0,
       0,     0,   436,   436,     0,     0,     0,     0,     0,     0,
       0,  2120,  2120,     0,     0,     0,    14,    15,    16,    17,
      18,     0,     0,   626,     0,     0,   650,   469,   148,     0,
     228,   229,    65,    66,    67,    68,    69,    70,    71,    72,
     657,     0,     0,   265,   657,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  2120,     0,    74,     0,     0,   349,
       0,     0,     0,     0,     0,   436,     0,     0,   274,     0,
     110,     0,     0,     0,     0,    58,  1635,    76,     0,     0,
       0,   469,     0,   110,   263,     0,     0,    79,    80,     0,
     116,     0,     0,     0,     0,     0,     0,     0,     0,   627,
     469,     0,    14,    15,    16,    17,    18,   148,     0,   228,
     229,    65,    66,    67,    68,    69,    70,    71,    72,     0,
       0,     0,   148,   627,   228,   229,    65,    66,    67,    68,
      69,    70,    71,    72,     0,    74,   627,     0,   307,     0,
       0,     0,   626,     0,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,   314,    76,     0,     0,     0,
       0,    58,     0,   116,   116,   116,    79,    80,     0,     0,
     802,    76,     0,     0,   635,     0,     0,     0,     0,     0,
       0,    79,   803,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   436,   148,   637,   228,   229,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,   148,   116,
     577,   578,    65,    66,    67,    68,    69,    70,    71,    72,
       0,    74,     0,     0,     0,   265,   116,     0,     0,     0,
       0,   468,     0,     0,   469,     0,     0,     0,     0,     0,
       0,  1635,    76,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    79,    80,     0,     0,     0,     0,     0,    77,
       0,   436,     0,   901,     0,     0,     0,     0,   550,     0,
       0,     0,   914,     0,   591,   469,     0,     0,     0,   116,
       0,     0,   116,     0,     0,   345,     0,   100,     0,     0,
       0,     0,     0,   116,     0,     0,     0,   349,   349,     0,
       0,     0,     0,   657,   938,     0,   115,     0,     0,     0,
       0,     0,     0,   116,     0,     0,   349,     0,   949,     0,
       0,     0,     0,     0,     0,     0,     0,   626,   116,     0,
       0,     0,   958,   116,   116,     0,     0,     0,   116,   116,
     657,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   148,   349,   228,   229,    65,    66,    67,    68,    69,
      70,    71,    72,   148,   115,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,     0,    74,
       0,     0,     0,     0,     0,   110,     0,     0,     0,     0,
       0,     0,     0,   265,   349,     0,     0,     0,     0,  2119,
      76,     0,     0,   525,     0,     0,   436,     0,     0,   276,
      79,    80,   627,     0,    77,   274,     0,   349,   148,     0,
     228,   229,    65,    66,    67,    68,    69,    70,    71,    72,
       0,     0,     0,     0,     0,     0,     0,     0,   468,     0,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,   115,     0,     0,  1055,     0,     0,     0,     0,
     175,   178,     0,     0,   469,     0,   230,    76,     0,     0,
       0,   353,     0,     0,     0,     0,     0,    79,    80,   938,
       0,     0,     0,   148,  1079,   228,   229,    65,    66,    67,
      68,    69,    70,    71,    72,     0,   223,     0,     0,     0,
       0,   468,   468,     0,     0,     0,     0,     0,   471,     0,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
     468,     0,     0,     0,     0,     0,     0,   349,     0,     0,
       0,   314,    76,   116,     0,     0,     0,     0,     0,     0,
       0,     0,    79,    80,   349,   349,   309,     0,     0,   310,
       0,     0,     0,     0,     0,     0,   901,     0,     0,     0,
       0,     0,     0,     0,   333,     0,   116,     0,     0,     0,
       0,     0,   121,     0,     0,   121,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   116,     0,     0,     0,  1234,
       0,     0,     0,     0,     0,     0,     0,   349,   468,     0,
       0,     0,     0,     0,   154,     0,     0,   115,     0,     0,
       0,     0,     0,   116,   116,     0,   657,   265,     0,  1269,
       0,   901,     0,     0,     0,   500,  1275,     0,     0,     0,
     121,     0,   388,     0,   389,   390,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,     0,     0,   629,     0,
       0,   276,   116,     0,     0,   121,     0,     0,   110,     0,
       0,     0,     0,     0,     0,   629,     0,     0,   345,   629,
       0,     0,     0,   559,   560,   121,     0,     0,     0,     0,
       0,     0,   718,   289,   175,    77,   399,   148,   110,   172,
     173,    65,    66,    67,    68,    69,    70,    71,    72,   175,
     116,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   274,   121,     0,     0,     0,     0,     0,   121,     0,
       0,   121,     0,     0,     0,     0,     0,     0,   606,     0,
       0,   901,     0,     0,     0,   609,   611,     0,     0,     0,
     618,   627,     0,     0,     0,     0,     0,     0,   901,   901,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   121,     0,     0,     0,     0,     0,   629,     0,   349,
     469,     0,     0,   121,     0,     0,     0,     0,   333,     0,
       0,   333,     0,   148,     0,   172,   173,    65,    66,    67,
      68,    69,    70,    71,    72,     0,     0,     0,     0,     0,
     148,   468,   172,   173,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,     0,     0,     0,     0,   349,   349,
       0,     0,     0,     0,     0,     0,     0,     0,   121,     0,
     121,   479,   349,   349,     0,   121,     0,   349,   349,     0,
       0,     0,     0,     0,     0,   154,     0,     0,   483,     0,
       0,     0,     0,     0,  1423,     0,   471,     0,     0,     0,
       0,     0,  1234,     0,     0,     0,     0,     0,     0,   223,
     349,   349,   349,   121,     0,     0,     0,     0,     0,     0,
       0,   819,   820,     0,     0,     0,     0,     0,   353,     0,
       0,     0,  1234,     0,   121,     0,     0,   276,     0,   115,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     471,     0,   115,     0,     0,  1482,     0,   110,   110,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   629,   471,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   626,     0,     0,     0,     0,
       0,     0,   629,     0,   548,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   629,     0,     0,     0,   121,
       0,     0,     0,   901,   345,     0,     0,     0,     0,     0,
     469,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   120,     0,     0,   120,     0,     0,
       0,     0,     0,   121,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   333,   901,   901,     0,     0,     0,     0,     0,   121,
       0,     0,     0,     0,     0,     0,   901,   901,     0,     0,
       0,   468,   468,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   120,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   471,     0,     0,     0,     0,     0,     0,
       0,   966,     0,     0,   901,   901,   901,   120,   349,   349,
     349,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1423,  1423,  1423,   154,   550,     0,   120,     0,     0,
       0,     0,     0,     0,   471,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   121,   121,     0,     0,
       0,  1664,  1664,     0,   349,     0,   353,   353,     0,     0,
       0,     0,     0,     0,   120,     0,     0,     0,     0,     0,
     120,   274,     0,   120,     0,   353,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   121,
       0,     0,     0,   121,     0,   121,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   110,     0,   121,     0,
       0,   353,     0,   120,   345,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   349,   120,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   349,   154,
       0,     0,     0,     0,   115,     0,     0,     0,     0,     0,
    1096,     0,     0,   353,     0,     0,     0,  1108,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   629,   121,   349,   276,     0,   353,     0,   349,   349,
     120,     0,   120,   349,   349,   121,     0,   120,   121,   121,
     125,   121,     0,   125,     0,     0,     0,     0,     0,     0,
     121,     0,     0,   121,   121,   121,     0,     0,     0,     0,
       0,     0,   901,   901,   901,     0,     0,     0,     0,     0,
       0,     0,     0,   471,     0,   120,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1179,     0,     0,     0,   120,  1815,   125,     0,
       0,   110,     0,     0,     0,     0,     0,     0,   901,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   125,     0,  1832,     0,     0,     0,     0,
       0,     0,     0,   121,     0,     0,   353,     0,     0,     0,
       0,     0,     0,   125,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   353,   353,     0,     0,     0,     0,     0,
    1664,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   120,     0,     0,     0,     0,     0,     0,   345,     0,
     125,   154,     0,     0,     0,     0,   125,     0,     0,   125,
       0,     0,   901,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   120,   353,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   901,   627,   125,
       0,   120,   901,   901,     0,     0,     0,   468,   468,     0,
       0,   125,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1928,     0,
     121,   349,   193,     0,     0,     0,     0,   115,     0,     0,
       0,     0,     0,     0,     0,   121,   121,     0,     0,     0,
     110,     0,     0,     0,   234,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   125,   115,   125,     0,
       0,     0,     0,   125,     0,  1664,     0,     0,   110,   627,
       0,     0,     0,     0,     0,     0,     0,     0,   120,   120,
     276,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     317,   125,     0,   410,     0,     0,     0,   110,     0,     0,
     629,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   120,   125,     0,     0,   120,     0,   120,     0,     0,
       0,  1444,  1446,  1448,     0,     0,     0,     0,   353,   471,
     120,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   349,     0,     0,     0,     0,
       0,     0,  1469,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   507,   234,     0,
       0,     0,  1179,     0,     0,     0,     0,   353,   353,     0,
       0,     0,  2056,     0,     0,     0,     0,   125,   317,     0,
       0,   353,   353,     0,   120,     0,   353,   353,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   120,     0,     0,
     120,   120,     0,   120,     0,   468,  1523,     0,     0,     0,
       0,   125,   120,     0,     0,   120,   120,   120,     0,   353,
     353,   353,     0,     0,  1664,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   125,   214,     0,
       0,   602,   317,     0,   225,   226,     0,     0,     0,     0,
       0,     0,  1664,  2056,     0,     0,   121,     0,     0,     0,
       0,     0,     0,     0,     0,   121,   115,   115,     0,     0,
       0,     0,     0,   121,     0,     0,     0,     0,   296,     0,
       0,     0,   204,   685,     0,     0,     0,   410,   691,     0,
       0,  1664,     0,     0,     0,   120,   121,   700,   701,     0,
       0,     0,     0,   121,     0,     0,     0,     0,     0,     0,
       0,     0,   410,   410,     0,     0,     0,     0,     0,     0,
    2162,     0,     0,     0,   125,   125,   121,     0,     0,   471,
       0,     0,     0,   410,     0,     0,     0,     0,     0,   901,
       0,     0,     0,     0,     0,     0,   121,     0,   204,     0,
    1647,     0,     0,  1649,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   410,   204,     0,     0,   125,     0,     0,
       0,   125,     0,   125,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   125,     0,   204,   121,
       0,     0,     0,   805,     0,     0,     0,     0,     0,     0,
       0,   465,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   120,     0,     0,     0,     0,   353,   353,   353,
       0,     0,     0,   844,     0,     0,     0,   120,   120,     0,
       0,     0,     0,     0,     0,   234,     0,     0,     0,     0,
     125,     0,     0,     0,     0,     0,     0,   204,     0,     0,
       0,   599,     0,   125,     0,     0,   125,   125,     0,   125,
     317,     0,     0,   353,     0,     0,   317,     0,   125,     0,
       0,   125,   125,   125,     0,     0,     0,     0,     0,     0,
     276,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   121,   121,   121,   121,   121,   121,   121,     0,
       0,     0,     0,     0,     0,     0,     0,   317,     0,     0,
       0,   204,     0,     0,     0,   115,     0,     0,   919,     0,
     317,     0,   121,   121,     0,     0,     0,     0,     0,     0,
       0,   204,     0,   353,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   353,     0,     0,
       0,   125,     0,     0,     0,     0,     0,     0,     0,  1822,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   353,     0,     0,     0,     0,   353,   353,     0,
       0,     0,   353,   353,     0,     0,     0,     0,     0,     0,
       0,   800,     0,   801,     0,     0,     0,     0,     0,     0,
     121,     0,   817,   818,     0,     0,     0,     0,     0,     0,
     204,     0,     0,     0,     0,     0,   410,   410,   410,   410,
     410,   410,   410,   410,   410,   410,   410,   410,   410,   410,
     410,   410,   410,   410,   410,     0,     0,     0,     0,     0,
     204,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     115,     0,     0,     0,     0,     0,     0,     0,   120,     0,
       0,     0,     0,     0,     0,     0,     0,   120,   125,     0,
       0,     0,     0,     0,     0,   120,     0,     0,     0,     0,
       0,     0,     0,   125,   125,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   410,     0,   120,     0,
       0,     0,   121,     0,     0,   120,     0,     0,     0,  1073,
       0,   805,     0,     0,     0,     0,     0,     0,     0,   911,
       0,     0,     0,   204,   204,     0,     0,     0,   120,   465,
       0,     0,     0,     0,     0,     0,   121,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   120,   317,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   121,     0,     0,     0,   317,     0,   629,     0,     0,
       0,  1988,     0,   204,     0,     0,     0,     0,     0,     0,
       0,   120,   121,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   465,     0,     0,     0,     0,     0,     0,     0,
     353,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   121,     0,   204,     0,     0,     0,   115,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   204,
       0,     0,     0,     0,     0,     0,     0,   115,   629,     0,
       0,     0,     0,     0,     0,   410,     0,     0,     0,     0,
     410,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   410,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   115,     0,     0,     0,
       0,     0,     0,     0,   120,   120,   120,   120,   120,   120,
     120,     0,     0,     0,     0,     0,   121,     0,     0,     0,
       0,     0,     0,   410,   125,     0,     0,     0,     0,     0,
       0,     0,     0,   125,   120,   120,   465,     0,     0,     0,
       0,   125,     0,     0,   353,  1094,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     204,     0,     0,     0,   125,     0,     0,     0,     0,     0,
       0,   125,     0,     0,     0,     0,     0,   465,     0,  1073,
       0,     0,     0,     0,     0,  1352,   805,     0,     0,     0,
       0,     0,     0,     0,   125,     0,     0,     0,     1,   465,
     465,   147,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   125,     0,     0,     0,   465,     0,
       0,     0,   120,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1175,  1176,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1240,  1241,  1242,     0,     0,
    1244,     0,     0,     0,     0,     0,     0,   125,     0,     0,
       0,     0,     0,   410,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   203,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   121,   465,     0,     0,     0,
       0,     0,     0,   204,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   121,   120,     0,     0,     0,     0,     0,
       0,     0,  1314,     0,     0,   302,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   410,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   317,     0,   120,     0,
       0,     0,   121,     0,     0,   410,   204,     0,     0,     0,
     125,   125,   125,   125,   125,   125,   125,     0,  1335,     0,
       0,     0,   410,   410,   410,     0,     0,     0,     0,   410,
     410,     0,     0,   120,     0,     0,     0,     0,     0,     0,
     125,   125,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   410,   120,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1359,     0,     0,     0,     0,
       0,     0,     0,     0,  1363,  1364,  1365,  1366,   302,     0,
       0,     0,  1371,  1372,     0,   120,     0,     0,     0,     0,
     410,   410,  1380,     0,     0,     0,   551,     0,  1579,     0,
       0,     0,     0,     0,     0,     0,   302,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   302,     0,     0,
       0,  1401,     0,     0,  1404,     0,  1405,     0,   125,     0,
       0,     0,     0,     0,     0,   583,   587,     0,     0,   465,
       0,     0,   594,   595,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     605,     0,     0,     0,     0,     0,     0,     0,     0,  1641,
       0,     0,     0,     0,     0,     0,     0,     0,   120,     0,
     624,     0,  1465,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1488,     0,     0,     0,     0,     0,     0,
    1492,     0,  1494,  1496,     0,     0,     0,     0,     0,     0,
     125,     0,     0,     0,  1505,     0,  1506,   717,  1507,     0,
    1509,     0,     0,     0,     0,  1517,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   125,     0,     0,     0,   758,     0,
       0,     0,     0,   204,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   204,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   796,     0,     0,     0,   799,   125,
       0,     0,     0,     0,     0,     0,     0,  1562,     0,     0,
       0,     0,   204,     0,  1569,  1570,     0,   821,     0,     0,
     125,   822,   823,     0,     0,   826,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1593,     0,
     840,   841,   842,   843,     0,  1598,     0,     0,     0,  1599,
       0,   125,     0,     0,     0,     0,     0,   120,     0,     0,
       0,     0,     0,   872,     0,     0,     0,     0,     0,     0,
       0,  1641,   876,     0,     0,     0,  1641,  1618,     0,   465,
     465,     0,  1813,     0,  1641,   120,     0,     0,     0,     0,
       0,   225,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   169,     0,     0,   302,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   120,     0,   410,   410,   169,     0,
       0,     0,     0,  1701,     0,   918,     0,     0,     0,     0,
       0,     0,   583,     0,   125,     0,     0,     0,   924,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   941,   946,   169,     0,     0,     0,  1730,     0,
       0,     0,     0,     0,     0,     0,  1735,   169,  1737,   169,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   373,   204,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1768,  1769,     0,
       0,     0,     0,   988,     0,   373,     0,     0,     0,     0,
       0,     0,  1774,  1775,     0,  1776,     0,     0,     0,  1936,
       0,     0,  1641,     0,  1780,     0,     0,     0,     0,     0,
     711,     0,     0,     0,     0,  1786,  1787,  1788,     0,     0,
       0,     0,     0,   169,     0,     0,     0,   169,     0,     0,
     169,   169,     0,     0,   169,     0,     0,   169,   169,     0,
     169,     0,   169,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1051,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   125,     0,   317,     0,  1068,     0,     0,
       0,  1069,     0,     0,     0,     0,     0,     0,     0,     0,
     941,     0,     0,     0,     0,     0,     0,     0,     0,   204,
       0,   125,     0,     0,     0,     0,     0,     0,     0,     0,
     410,     0,  1109,     0,     0,     0,     0,   169,     0,     0,
     169,  1118,     0,     0,     0,     0,     0,  1121,     0,     0,
       0,     0,     0,     0,  1641,     0,     0,     0,     0,     0,
     125,     0,     0,   169,   169,     0,     0,     0,     0,     0,
     410,     0,     0,  1883,  1884,     0,     0,     0,     0,   169,
       0,     0,     0,     0,     0,     0,  1892,   880,   882,     0,
       0,     0,     0,     1,     0,     0,     0,  1165,     0,     0,
       0,     1,     0,     0,     0,     0,   204,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1916,  1917,  1918,     0,     0,     0,     0,
       1,     0,     0,     0,   360,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   465,   465,     0,     0,     0,
     458,   360,   317,     0,   169,     0,   410,     0,   410,     0,
       0,     0,     0,     0,     0,     0,  1294,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   528,     0,     0,     0,     0,     0,     0,   528,
       0,     0,     0,     0,     0,     0,     0,   410,     0,     0,
       0,     0,  1992,     0,     0,     0,   711,     0,     0,   373,
       0,     0,   711,     0,     0,     0,   602,   317,     0,     0,
       0,   711,     0,  2002,     0,   169,  2003,  2004,     0,     0,
       0,   410,     0,  2006,     0,     0,     0,     0,     0,     0,
     711,     0,     0,     0,     0,     0,     0,     0,     0,  1342,
       0,     0,     0,  1343,     0,     0,     0,     0,     0,   317,
     941,     0,     0,     0,     0,   528,     0,     0,     0,     0,
    1356,     0,     0,     0,     0,     0,  1043,  1357,     0,   410,
       0,     0,     0,     0,     0,     0,  1361,     0,  1362,     0,
       0,   360,   639,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   373,     0,     0,     0,     0,     0,
       0,     0,   660,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1394,     0,     0,     0,  1395,     0,     0,
       0,  1396,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   169,   169,     0,     0,   147,
       0,     0,     0,     0,     1,     0,     0,     0,   169,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   465,  2118,     0,     0,     0,     0,     0,
       0,     0,     0,   528,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   528,
     792,     0,   528,   795,     0,     0,     0,     0,     0,     0,
     360,     0,     0,     0,   639,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  2159,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1504,     0,     0,     0,   528,     0,     0,     0,   528,
       0,     0,     0,  2178,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1529,     0,  2187,     0,
       0,   169,   169,     0,     0,     0,     0,   169,     0,     0,
       0,     0,     0,     0,     0,  2198,     0,     0,     0,   360,
       0,     0,     0,     0,     0,     0,     0,     0,   169,     0,
       0,   169,   169,     0,   169,     0,   169,   169,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  2016,     0,
     528,     0,     0,   360,     0,     0,     0,     0,   169,     0,
       0,     0,   169,     0,     0,     0,   169,     0,     0,     0,
    1602,   936,   360,     0,  1603,     0,     0,     0,  1604,     0,
       0,     0,   639,     0,     0,     0,   639,     0,     0,     0,
       0,     0,     0,   954,     0,   360,   382,     0,     0,   383,
       0,   384,     0,   385,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1648,     0,     0,     0,
     386,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   169,   169,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   387,   388,
     169,   389,   390,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   391,   392,   379,     0,   393,   394,   395,
       0,   396,   397,     0,     0,     0,     0,  1718,     0,    74,
    1721,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1732,   398,
       0,     0,    77,   399,     0,     0,   360,     0,     0,   400,
      79,    80,   401,   402,   403,   404,     0,     0,     0,     0,
       0,     0,   528,   528,     0,     0,     0,   711,     0,     0,
       0,     0,   528,  1064,     0,   528,  1067,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   360,     0,     0,
     639,  1767,   639,   639,     0,     0,     0,     0,     0,   639,
    1772,     0,     0,     0,  1773,     0,     0,     0,     0,   360,
     360,     0,     0,   211,     0,     0,     0,     0,  1777,  1778,
       0,   169,     0,     0,     0,     0,     0,     0,   360,   269,
       0,     0,   528,     0,     0,     0,   528,     0,     0,     0,
       0,     0,     0,   528,  1138,     0,  1721,   528,  1142,     0,
       0,   528,  1146,     0,     0,     0,     0,     0,     0,  1150,
       0,     0,   169,     0,     0,     0,     0,     0,     0,   169,
       0,     0,   169,     0,     0,     0,   169,     0,     0,   211,
       0,     0,     0,   324,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   365,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   360,   528,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   211,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   478,     0,     0,   482,     0,   639,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1877,  1878,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   360,     0,     0,     0,
       0,     0,     0,     0,     0,   167,     0,     0,   211,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1655,     0,   269,  1658,  1672,     0,     0,     0,     0,  1679,
       0,     0,     0,  1683,   169,  1685,     0,     0,     0,     0,
       0,     0,   169,   169,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   482,   528,     0,     0,     0,     0,     0,     0,
       0,     0,   211,     0,     0,     0,     0,     0,   294,     0,
     639,   639,     0,     0,     0,     0,     0,   639,     0,     0,
       0,   300,   632,   301,   649,     0,     0,     0,     0,     0,
       0,   169,     0,     0,     0,     0,     0,  1972,     0,     0,
     169,     0,     0,   169,     0,   169,   169,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   360,
       0,     0,     0,     0,   528,  1385,     0,   528,  1389,     0,
    1721,   528,  1393,     0,     0,     0,     0,   715,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  2005,
       0,   169,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   211,     0,     0,     0,     0,     0,     0,     0,  2024,
       0,     0,  1789,     0,   530,   531,     0,     0,   535,     0,
       0,   538,   539,     0,   541,     0,   542,     0,     0,     0,
       0,   632,     0,     0,     0,     0,  2052,   816,     0,  2053,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1831,     0,     0,     0,   169,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1846,  1848,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   360,     0,     0,     0,     0,     0,   639,
    1513,     0,  1658,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   211,   211,     0,   621,   622,     0,
     478,     0,   360,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   654,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  2139,     0,
       0,     0,     0,     0,   169,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   528,  1566,     0,     0,
       0,     0,     0,     0,   365,   528,  1575,     0,   639,     0,
       0,     0,     0,   169,     0,     0,     0,     0,     0,   360,
     360,     0,     0,   478,     0,   940,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   632,     0,   169,     0,
       0,     0,   382,     0,   169,   383,     0,   384,   790,   385,
    1946,     0,     0,     0,     0,     0,     0,     0,     0,  1949,
     211,  1951,     0,     0,  1956,  1960,   386,  1672,     0,     0,
       0,     0,  1966,   715,     0,     0,   715,   715,     0,   715,
       0,     0,     0,     0,     0,     0,     0,     0,   715,     0,
       0,   715,   715,   715,   387,   388,     0,   389,   390,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,   391,
     392,   379,     0,   393,   394,   395,     0,   396,   397,   868,
     169,     0,     0,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1666,
    1667,  1668,     0,     0,     0,   398,  1847,   478,    77,   399,
       0,     0,   360,     0,     0,   400,    79,    80,   401,   402,
     403,   404,     0,     0,     0,     0,     0,     0,   434,     0,
       0,   211,     0,     0,     0,     0,     0,     0,     0,   639,
    2041,   463,     0,     0,     0,  2046,  2048,     0,   478,     0,
       0,     0,     0,     0,   491,     0,   491,     0,     0,     0,
       0,     0,     0,     0,     0,  2068,     0,   169,   169,     0,
     478,   478,     0,     0,     0,   373,     0,     0,     0,   169,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   478,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   952,
     953,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   960,     0,  2097,     0,  2100,     0,     0,  2102,
    2104,     0,     0,     0,  2107,  2109,     0,     0,     0,     0,
       0,     0,     0,   528,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   528,
     600,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   478,     0,     0,
       0,     0,     0,     0,   211,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   816,     0,     0,
       0,     0,     0,   169,  2153,  2155,  2157,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   360,     0,     0,     0,
    2182,  2184,  2186,     0,     0,  1057,  1058,   365,     0,     0,
       0,  1062,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1083,     0,     0,  1086,  1087,     0,  1090,     0,
    1092,  1093,     0,   169,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   360,   360,     0,     0,     0,
       0,     0,     0,   382,     0,     0,   383,     0,   384,     0,
     385,     0,     0,     0,     0,   528,   528,     0,     0,     0,
       0,     0,  1136,     0,     0,     0,  1140,   386,     0,     0,
    1144,   528,     0,     0,     0,     0,   491,     0,     0,     0,
       0,     0,   491,     0,     0,     0,     0,   837,     0,     0,
       0,     0,     0,     0,     0,   387,   388,     0,   389,   390,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     391,   392,   379,     0,   393,   394,   395,     0,   396,   397,
     478,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,   169,     0,     0,     0,     0,     0,     0,     0,
    1259,  1260,     0,     0,     0,     0,   398,   836,     0,    77,
     399,     0,     0,     0,  1276,     0,   400,    79,    80,   401,
     402,   403,   404,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   715,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   917,     0,     0,   528,     0,     0,     0,
       0,     0,     0,     0,   528,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   715,     0,     0,     0,     0,     0,
       0,     0,   463,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   948,     0,     0,     0,     0,
       0,     0,     0,   269,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   211,     0,     0,     0,     0,     0,
       0,     0,     0,   360,   632,     0,     0,     0,   528,  2083,
       0,     0,   528,     0,     0,     0,     0,     0,     0,     0,
     982,     0,     0,     0,     0,  1276,     0,     0,     0,     0,
       0,     0,     0,   365,     0,     0,     0,   715,     0,     0,
       0,     0,     0,     0,   837,  1002,     0,     0,  1004,     0,
    1006,     0,     0,   528,     0,     0,  1015,     0,  1020,  1015,
       0,     0,     0,     0,     0,     0,  1376,     0,     0,     0,
       0,     0,     0,  1383,     0,     0,  1387,     0,     0,     0,
    1391,     0,     0,     0,     0,     0,  1048,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1050,
     478,   478,     0,     0,     0,     0,     0,     0,     0,     0,
    1059,     0,     0,     0,     0,     0,     0,   528,   528,     0,
       0,     0,     0,     0,   463,     0,     0,  1048,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     715,   715,   715,     0,  1112,   715,   715,   491,     0,     0,
     528,     0,   482,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
    1151,    46,     0,    47,     0,     0,     0,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,  1511,     0,
       0,   269,    58,     0,     0,     0,  1520,  1521,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   365,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   434,   148,     0,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,  1266,  1268,     0,     0,
       0,     0,     0,     0,   463,     0,     0,     0,     0,     0,
       0,     0,    74,     0,     0,  1564,     0,     0,     0,     0,
       0,     0,     0,     0,  1573,     0,     0,  1577,     0,  1580,
    1581,  1873,  1015,     0,     0,    77,    78,     0,     0,     0,
       0,     0,     0,    79,    80,     0,  1048,     0,  1439,     0,
       0,     0,     0,     0,  1307,     0,     0,     0,     0,     0,
       0,  1015,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1609,     0,     0,     0,   382,
       0,     0,   383,     0,   384,     0,   385,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     211,  1181,     0,   386,    -2,     0,  1183,   491,     0,  1184,
    1185,  1186,  1187,  1188,  1189,  1190,  1191,  1192,  1193,  1194,
    1195,  -338,  -338,  1196,  1197,  1198,  1199,  1200,     0,  1201,
       0,   387,   388,   269,   485,   390,  1202,  1203,    65,    66,
      67,    68,    69,    70,    71,    72,   391,   392,   379,  1204,
     393,   394,   395,     0,   396,   397,     0,     0,     0,     0,
    1715,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   491,     0,  1375,     0,  1378,     0,
       0,     0,  1205,     0,     0,    77,   399,   365,     0,     0,
     298,     0,   400,    79,    80,   401,   402,   403,   404,     0,
       0,     0,     0,     0,     0,     0,     0,  -185,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   715,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   478,   478,  1577,     0,
       0,     0,     0,     0,     0,     0,     0,  1455,  1455,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1782,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1274,     0,     0,     0,
       0,   269,     0,     0,    14,    15,    16,    17,    18,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1508,     0,     0,
       0,     0,     0,  1518,     0,     0,     0,     0,     0,     0,
     382,     0,     0,   383,     0,   384,     0,   385,     0,     0,
       0,     0,   463,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    58,   386,     0,     0,     0,     0,   491,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1015,     0,     0,   837,     0,     0,
       0,     0,   387,   388,  1875,   389,   390,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   391,   392,   379,
       0,   393,   394,   395,     0,   396,   397,     0,     0,     0,
       0,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1601,
       0,   715,     0,   398,     0,     0,    77,   399,     0,     0,
       0,     0,     0,   400,   461,    80,   401,   402,   403,   404,
    1611,  1612,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   478,     0,     0,     0,     0,     0,
       0,  1932,  1933,     0,     0,     0,     0,     0,     0,  1015,
       0,     0,     0,  1937,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   491,     0,     0,
     837,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   715,     0,     0,   482,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  2199,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1439,     0,
       0,     0,  1002,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1733,  1734,     0,     0,     0,     0,     0,     0,
       0,     0,   491,     0,     0,     0,     0,     0,     0,   382,
       0,     0,   383,     0,   384,     0,   385,     0,     0,     0,
     491,     0,   837,     0,     0,     0,     0,  2014,     0,     0,
       0,  1181,     0,   386,    -2,     0,  1183,  -244,  -244,  1184,
    1185,  1186,  1187,  1188,  1189,  1190,  1191,  1192,  1193,  1194,
    1195,  -338,  -338,  1196,  1197,  1198,  1199,  1200,     0,  1201,
       0,   387,   388,     0,   485,   390,  1202,  1203,    65,    66,
      67,    68,    69,    70,    71,    72,   391,   392,   379,  1204,
     393,   394,   395,     0,   396,   397,     0,     0,     0,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  2081,     0,   434,
       0,  -244,  1205,     0,  1814,    77,   399,     0,     0,     0,
     298,     0,   400,    79,    80,   401,   402,   403,   404,  2199,
       0,     0,     0,     0,     0,     0,     0,  -185,    14,    15,
      16,    17,    18,     0,     0,    20,  1439,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,  1860,     0,     0,    46,   382,    47,     0,
     383,     0,   384,     0,   385,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    58,     0,  1181,
       0,   386,    -2,     0,  1183,  -245,  -245,  1184,  1185,  1186,
    1187,  1188,  1189,  1190,  1191,  1192,  1193,  1194,  1195,  -338,
    -338,  1196,  1197,  1198,  1199,  1200,     0,  1201,  1894,   387,
     388,  1896,   485,   390,  1202,  1203,    65,    66,    67,    68,
      69,    70,    71,    72,   391,   392,   379,  1204,   393,   394,
     395,     0,   396,   397,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,  1923,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -245,
    1205,     0,     0,    77,   399,     0,     0,     0,   298,     0,
     400,    79,    80,   401,   402,   403,   404,     0,     0,     0,
       0,     0,     0,     0,     0,  -185,     0,     4,   183,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,  1180,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   382,     0,    46,   383,    47,   384,     0,
     385,    48,    49,    50,    51,    52,    53,    54,    55,    56,
       0,     0,     0,    57,     0,  1181,    58,  1182,    -2,     0,
    1183,     0,     0,  1184,  1185,  1186,  1187,  1188,  1189,  1190,
    1191,  1192,  1193,  1194,  1195,  -338,  -338,  1196,  1197,  1198,
    1199,  1200,     0,  1201,     0,   387,   388,    61,   485,   390,
    1202,  1203,    65,    66,    67,    68,    69,    70,    71,    72,
     391,   392,   379,  1204,   393,   394,   395,     0,   396,   397,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    -3,  1205,     0,  1015,    77,
     430,     0,     0,     0,   298,     0,   400,    79,    80,   401,
     402,   403,   404,     0,     0,     0,     0,     0,     0,     0,
       0,  -185,     4,   183,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,  1180,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,   382,     0,
      46,   383,    47,   384,     0,   385,    48,    49,    50,    51,
      52,    53,    54,    55,    56,     0,     0,     0,    57,     0,
    1181,    58,  1182,    -2,     0,  1183,     0,     0,  1184,  1185,
    1186,  1187,  1188,  1189,  1190,  1191,  1192,  1193,  1194,  1195,
    -338,  -338,  1196,  1197,  1198,  1199,  1200,     0,  1201,     0,
     387,   388,    61,   485,   390,  1202,  1203,    65,    66,    67,
      68,    69,    70,    71,    72,   391,   392,   379,  1204,   393,
     394,   395,     0,   396,   397,     0,     0,     0,     0,     0,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1205,     0,     0,    77,   430,     0,     0,     0,   298,
       0,   400,    79,    80,   401,   402,   403,   404,     0,     0,
       0,     0,     0,     0,     0,     0,  -185,     4,   183,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   382,     0,    46,   383,    47,   384,     0,
     385,    48,    49,    50,    51,    52,    53,    54,    55,    56,
       0,     0,     0,    57,     0,     0,    58,   386,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   387,   388,    61,   389,   390,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     391,   392,   379,     0,   393,   394,   395,     0,   396,   397,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1666,  1667,  1668,     0,     0,     0,   398,  1669,  1670,    77,
     430,     0,     0,     0,     0,     0,   400,    79,    80,   401,
     402,   403,   404,     0,     0,     0,     0,     0,     0,     0,
       0,  1671,     4,   183,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,   382,     0,
      46,   383,    47,   384,     0,   385,    48,    49,    50,    51,
      52,    53,    54,    55,    56,     0,     0,     0,    57,     0,
       0,    58,   386,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     387,   388,    61,   389,   390,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,   391,   392,   379,     0,   393,
     394,   395,     0,   396,   397,     0,     0,     0,     0,     0,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1666,  1667,  1668,     0,     0,
       0,   398,  1669,     0,    77,   430,     0,     0,     0,     0,
       0,   400,    79,    80,   401,   402,   403,   404,     0,     0,
       0,     0,     0,     0,     0,     0,  1671,   183,     6,     7,
       8,     9,    10,    11,    12,    13,     0,     0,     0,     0,
       0,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
     252,   253,     0,   254,    46,     0,    47,     0,   255,     0,
       0,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     4,   183,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,   382,
       0,    46,   383,    47,   384,     0,   385,    48,    49,    50,
      51,    52,    53,    54,    55,    56,     0,     0,     0,    57,
       0,     0,    58,   386,     0,     0,     0,  -458,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -458,   387,   388,    61,   389,   390,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   391,   392,   379,     0,
     393,   394,   395,     0,   396,   397,     0,     0,     0,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   398,     0,  1657,    77,   430,     0,     0,     0,
       0,     0,   400,    79,    80,   401,   402,   403,   404,     4,
     183,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,   382,     0,    46,   383,    47,
     384,     0,   385,    48,    49,    50,    51,    52,    53,    54,
      55,    56,     0,     0,     0,    57,     0,     0,    58,   386,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   387,   388,    61,
     389,   390,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   391,   392,   379,     0,   393,   394,   395,     0,
     396,   397,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   398,     0,
       0,    77,   430,     0,     0,     0,     0,     0,   400,    79,
      80,   401,   402,   403,   404,   183,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
     382,     0,    46,   383,    47,   384,     0,   385,   340,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,   386,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   387,   388,     0,   389,   390,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   391,   392,   379,
       0,   393,   394,   395,     0,   396,   397,     0,     0,     0,
       0,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   398,     0,     0,    77,   460,     0,     0,
       0,     0,     0,   400,   461,    80,   401,   402,   403,   404,
     183,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,   382,     0,    46,   383,    47,
     384,     0,   385,   340,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,   386,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   387,   388,     0,
     389,   390,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   391,   392,   379,     0,   393,   394,   395,     0,
     396,   397,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   398,     0,
       0,    77,  1263,     0,     0,     0,     0,     0,   400,  1264,
      80,   401,   402,   403,   404,   183,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
     382,     0,    46,   383,    47,   384,     0,   385,   340,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,   386,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   387,   388,     0,   389,   390,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   391,   392,   379,
       0,   393,   394,   395,     0,   396,   397,     0,     0,     0,
       0,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   398,     0,     0,    77,   399,     0,     0,
       0,     0,     0,   400,    79,    80,   401,   402,   403,   404,
     183,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,   382,     0,    46,   383,    47,
     384,     0,   385,   340,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,   386,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   387,   388,     0,
     389,   390,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   391,   392,   379,     0,   393,   394,   395,     0,
     396,   397,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   398,     0,
       0,    77,   460,     0,     0,     0,     0,     0,   400,    79,
      80,   401,   402,   403,   404,  2023,     0,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,     0,    -2,     0,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,     0,    -2,    -2,     0,    -2,     0,    -2,
       0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
       0,     0,     0,    -2,     0,     0,    -2,     0,     0,     0,
       0,    -2,    -2,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    -2,     0,     0,
      -2,    -2,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    -2,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    -2,     0,     0,     0,    -2,
      -2,     0,     0,     0,     0,     0,     0,    -2,    -2,  2051,
       0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,    -2,     0,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,    -2,    -2,
       0,    -2,     0,    -2,     0,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,     0,     0,     0,    -2,     0,  1515,
      -2,     0,     0,     0,     0,    -2,    -2,    14,    15,    16,
      17,    18,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    -2,     0,     0,    -2,    -2,     0,     0,     0,     0,
       0,     0,     0,   382,     0,     0,   383,     0,   384,     0,
     385,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      -2,     0,     0,     0,     0,     0,    58,   386,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    -2,
       0,     0,     0,    -2,    -2,     0,     0,     0,     0,     0,
       0,    -2,    -2,     0,     0,   387,   388,     0,   389,   390,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     391,   392,   379,     0,   393,   394,   395,     0,   396,   397,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   398,     0,     0,    77,
     399,     0,     0,     0,     0,     0,   400,  1516,    80,   401,
     402,   403,   404,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,    48,    49,    50,
      51,    52,    53,    54,    55,    56,     0,     0,     0,    57,
       0,     0,    58,    59,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    60,
       0,     0,     0,    61,    62,     0,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,     0,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    75,    76,     0,    77,    78,     0,     0,     0,
       0,     0,     0,    79,    80,   257,   183,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,  -483,
    -483,     0,  -483,    46,     0,    47,     0,  -483,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   148,     0,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    75,    76,     0,    77,   258,     0,
       0,     0,  -811,     0,     0,    79,    80,   257,   183,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -483,  -483,     0,  -483,    46,     0,    47,     0,  -483,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   148,     0,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    75,    76,     0,    77,
     258,     0,     0,     0,     0,     0,     0,    79,    80,     4,
     183,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,    48,    49,    50,    51,    52,    53,    54,
      55,    56,     0,     0,     0,    57,     0,     0,    58,     0,
       0,     0,     0,  -406,  -406,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    61,
       0,     0,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -406,     0,     0,
       0,    77,    78,     0,     0,     0,     0,     0,     0,    79,
      80,     4,   183,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,    56,     0,     0,     0,    57,     0,     0,
      58,     0,     0,     0,     0,  -407,  -407,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    61,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -407,
       0,     0,     0,    77,    78,     0,  1414,     0,  1415,     0,
       0,    79,    80,  1416,     0,     0,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,  1417,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    61,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1418,     0,     0,     0,    77,   978,
       0,  1414,     0,  1415,     0,     0,    79,    80,  1416,     0,
       0,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,  1417,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    61,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1621,
       0,     0,     0,    77,   978,     0,  1414,     0,  1415,     0,
       0,    79,    80,  1416,     0,     0,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,  1417,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    61,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1622,     0,     0,     0,    77,   978,
       0,  1414,     0,  1415,     0,     0,    79,    80,  1416,     0,
       0,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,  1417,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    61,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1623,
       0,     0,     0,    77,   978,     0,     0,     0,     0,     0,
       0,    79,    80,   257,   183,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     0,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,  -483,  -483,     0,
    -483,    46,     0,    47,     0,  -483,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   257,     0,
       0,     0,     0,     0,     0,    77,   258,     0,    14,    15,
      16,    17,    18,    79,    80,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,  -483,  -483,     0,  -483,    46,     0,    47,     0,
    -483,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      77,   322,     0,     0,     0,     0,     0,     0,    79,    80,
     183,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,  -483,  -483,     0,  -483,    46,     0,    47,
       0,  -483,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     148,     0,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    75,    76,
       0,    77,   258,     0,     0,     0,  -815,     0,     0,    79,
      80,   183,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,     0,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,  -483,  -483,     0,  -483,    46,     0,
      47,     0,  -483,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   148,     0,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    75,
      76,     0,    77,   258,     0,     0,     0,     0,     0,     0,
      79,    80,   183,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,   340,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,  1103,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -675,    77,   342,     0,     0,     0,     0,     0,
       0,    79,    80,   183,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,   340,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   341,    77,   342,     0,     0,     0,     0,
       0,     0,    79,    80,   183,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,   340,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,  1912,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    77,   342,     0,     0,     0,
       0,     0,     0,    79,    80,   183,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,   340,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,  1914,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    77,   342,     0,     0,
       0,     0,     0,     0,    79,    80,   183,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,   340,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    77,   322,     0,
       0,     0,     0,     0,     0,    79,    80,   183,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
     340,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    77,   342,
       0,     0,     0,     0,     0,     0,    79,    80,   183,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -483,  -483,     0,  -483,    46,     0,    47,     0,  -483,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      63,    64,     0,     0,     0,     0,  1439,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   382,     0,     0,
     383,     0,   384,     0,   385,     0,     0,     0,     0,    77,
     258,     0,     0,     0,     0,     0,     0,    79,    80,  1181,
       0,   386,    -2,     0,  1183,  1939,  1940,  1184,  1185,  1186,
    1187,  1188,  1189,  1190,  1191,  1192,  1193,  1194,  1195,     0,
       0,  1196,  1197,  1198,  1199,  1200,     0,  1201,     0,   387,
     388,     0,   485,   390,  1202,  1203,    65,    66,    67,    68,
      69,    70,    71,    72,   391,   392,   379,  1204,   393,   394,
     395,  1439,   396,   397,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1205,     0,   382,    77,   399,   383,     0,   384,   298,   385,
     400,    79,    80,   401,   402,   403,   404,     0,     0,     0,
       0,     0,     0,     0,  1181,  -185,   386,    -2,     0,  1183,
       0,     0,  1184,  1185,  1186,  1187,  1188,  1189,  1190,  1191,
    1192,  1193,  1194,  1195,     0,     0,  1196,  1197,  1198,  1199,
    1200,     0,  1201,     0,   387,   388,     0,   485,   390,  1202,
    1203,    65,    66,    67,    68,    69,    70,    71,    72,   391,
     392,   379,  1204,   393,   394,   395,     0,   396,   397,     0,
       0,     0,     0,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1205,     0,     0,    77,   399,
       0,     0,     0,   298,     0,   400,    79,    80,   401,   402,
     403,   404,     0,     0,     0,     0,     0,     0,     0,     0,
    -185,    14,    15,    16,    17,    18,    19,   702,    20,   703,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,   382,     0,    46,
     383,    47,   384,     0,   385,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,   386,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   704,     0,     0,     0,     0,  1195,     0,
    -338,     0,     0,     0,     0,     0,     0,     0,     0,   387,
     388,     0,   389,   390,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   391,   392,   379,     0,   393,   394,
     395,     0,   396,   397,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1205,     0,     0,    77,   705,     0,     0,     0,   298,     0,
     400,    79,    80,   706,   707,   403,   404,    14,    15,    16,
      17,    18,    19,   702,    20,   703,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   382,     0,    46,   383,    47,   384,     0,
     385,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,   386,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   704,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   387,   388,     0,   389,   390,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     391,   392,   379,     0,   393,   394,   395,     0,   396,   397,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   398,     0,     0,    77,
     705,     0,     0,     0,   298,     0,   400,    79,    80,   706,
     707,   403,   404,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,   382,
       0,    46,   383,    47,   384,     0,   385,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,   386,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   387,   388,     0,   389,   390,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   391,   392,   379,     0,
     393,   394,   395,     0,   396,   397,     0,     0,     0,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   398,     0,   429,    77,   430,     0,     0,     0,
       0,     0,   400,    79,    80,   401,   402,   403,   404,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,   382,     0,    46,   383,    47,
     384,     0,   385,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,   386,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   387,   388,     0,
     389,   390,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   391,   392,   379,     0,   393,   394,   395,     0,
     396,   397,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   398,     0,
       0,    77,   705,     0,     0,     0,   298,     0,   400,    79,
      80,   401,   402,   403,   404,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   382,     0,    46,   383,    47,   384,     0,   385,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,   386,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   387,   388,     0,   389,   390,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   391,   392,
     379,     0,   393,   394,   395,     0,   396,   397,     0,     0,
       0,     0,     0,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   398,     0,     0,    77,   430,     0,
       0,     0,     0,     0,   400,    79,    80,   401,   402,   403,
     404,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,   382,     0,    46,
     383,    47,   384,     0,   385,   340,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,   386,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   387,
     388,     0,   389,   390,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   391,   392,   379,     0,   393,   394,
     395,     0,   396,   397,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     398,     0,     0,    77,   460,     0,     0,     0,     0,     0,
     400,    79,    80,   401,   402,   403,   404,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   382,     0,    46,   383,    47,   384,     0,
     385,   340,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,   386,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   387,   388,     0,   389,   390,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     391,   392,   379,     0,   393,   394,   395,     0,   396,   397,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   398,     0,     0,    77,
     399,     0,     0,     0,     0,     0,   400,    79,    80,   401,
     402,   403,   404,   183,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,     0,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   693,     0,   694,   695,   588,   183,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,   -17,     0,
       0,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   257,   183,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      63,    64,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,  -483,
    -483,     0,  -483,    46,     0,    47,     0,  -483,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    77,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    63,    64,
       0,     0,     0,     0,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,    77,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   148,     0,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    75,    76,     0,    77,    78,     0,     0,
       0,  -813,     0,     0,    79,    80,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   148,     0,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    75,    76,     0,    77,    78,
       0,     0,     0,     0,     0,     0,    79,    80,   183,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,   340,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   900,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    14,    15,    16,    17,    18,  -688,    77,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,  -483,  -483,     0,
    -483,    46,     0,    47,     0,  -483,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   148,     0,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    75,    76,     0,    77,   322,     0,     0,     0,
       0,     0,     0,    79,    80,   183,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,   340,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1823,     0,     0,     0,   183,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,    77,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
       0,     0,   340,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    63,    64,     0,     0,     0,     0,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
      77,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   984,    77,
     978,     0,     0,     0,     0,     0,     0,    79,    80,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,  1533,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    77,   978,     0,     0,     0,     0,     0,     0,    79,
      80,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    77,   305,     0,     0,     0,     0,     0,
       0,    79,    80,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    77,    78,     0,     0,     0,
       0,     0,     0,    79,    80,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    77,   456,     0,
       0,     0,     0,     0,     0,    79,    80,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,   340,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    77,
     342,     0,     0,     0,     0,     0,     0,    79,    80,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,   340,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    77,   305,     0,     0,     0,     0,     0,     0,    79,
      80,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,   340,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    77,   456,     0,     0,     0,     0,     0,
       0,    79,    80,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,   340,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    77,   322,     0,     0,     0,
       0,     0,     0,    79,    80,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    77,   978,     0,
       0,     0,     0,     0,     0,    79,    80,   183,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,     0,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
    -483,  -483,     0,  -483,    46,     0,    47,     0,  -483,     0,
       0,     0,     0,     0,     0,     0,     0,    14,    15,    16,
      17,    18,    19,     0,    20,    58,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,    63,
      64,   340,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    77,     0,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    77,
     978,     0,     0,     0,     0,     0,     0,    79,    80,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,   340,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    77,     0,     0,    14,    15,    16,    17,    18,    79,
      80,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,  -483,  -483,
       0,  -483,    46,     0,    47,     0,  -483,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    77,   322,     0,    14,
      15,    16,    17,    18,    79,    80,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,  -483,  -483,     0,  -483,    46,     0,    47,
       0,  -483,     0,     0,     0,     0,     0,     0,     0,     0,
      14,    15,    16,    17,    18,    19,     0,    20,    58,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,    63,    64,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    77,     0,    63,    64,     0,     0,     0,     0,    79,
      80,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
     382,     0,    46,   383,    47,   384,     0,   385,     0,     0,
       0,     0,    77,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   386,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   387,   388,     0,   389,   390,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,   391,   392,   379,
       0,   393,   394,   395,     0,   396,   397,     0,     0,     0,
       0,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   398,     0,     0,    77,   399,     0,     0,
       0,     0,     0,   400,   461,    80,   401,   402,   403,   404,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,   382,
       0,    46,   383,    47,   384,     0,   385,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   386,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   387,   388,     0,   389,   390,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   391,   392,   379,     0,
     393,   394,   395,     0,   396,   397,     0,     0,     0,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   398,     0,     0,    77,   399,     0,     0,     0,
       0,     0,   400,    79,    80,   401,   402,   403,   404,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     148,     0,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,    77,    47,     0,     0,     0,   340,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      14,    15,    16,    17,    18,    63,    64,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,  -483,  -483,     0,  -483,    46,     0,
      47,     0,  -483,     0,     0,     0,   382,     0,     0,   383,
       0,   384,     0,   385,    77,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     386,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    63,    64,     0,     0,     0,   387,   388,
       0,   389,   390,  1954,    64,    65,    66,    67,    68,    69,
      70,    71,    72,   391,   392,   379,     0,   393,   394,   395,
       0,   396,   397,   382,     0,     0,   383,     0,   384,    74,
     385,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    77,  1666,  1667,  1668,     0,   386,     0,   398,
    1955,     0,    77,   399,     0,     0,     0,     0,     0,   400,
      79,    80,   401,   402,   403,   404,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   387,   388,     0,   485,   390,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     391,   392,   379,     0,   393,   394,   395,   382,   396,   397,
     383,     0,   384,     0,   385,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   386,     0,     0,     0,     0,   398,    76,     0,   486,
     487,     0,     0,     0,   488,     0,   400,    79,    80,   401,
     402,   403,   404,     0,     0,     0,     0,     0,     0,   387,
     388,     0,   389,   390,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   391,   392,   379,     0,   393,   394,
     395,   382,   396,   397,   383,     0,   384,     0,   385,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   386,     0,     0,     0,     0,
     398,  1310,     0,    77,   399,     0,     0,     0,  1311,     0,
     400,    79,    80,   401,   402,   403,   404,     0,     0,     0,
       0,     0,     0,   387,   388,     0,   389,   390,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   391,   392,
     379,     0,   393,   394,   395,   382,   396,   397,   383,     0,
     384,     0,   385,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   386,
       0,     0,     0,     0,   398,     0,     0,    77,   399,     0,
       0,     0,   488,     0,   400,    79,    80,   401,   402,   403,
     404,     0,     0,     0,     0,     0,     0,   387,   388,     0,
     389,   390,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   391,   392,   379,     0,   393,   394,   395,   382,
     396,   397,   383,     0,   384,     0,   385,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   386,     0,     0,     0,     0,   398,     0,
       0,    77,   399,     0,     0,     0,   298,     0,   400,    79,
      80,   401,   402,   403,   404,     0,     0,     0,     0,     0,
       0,   387,   388,     0,   389,   390,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   391,   392,   379,     0,
     393,   394,   395,   382,   396,   397,   383,     0,   384,     0,
     385,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   386,     0,     0,
       0,     0,   398,  1011,     0,    77,   399,     0,     0,     0,
       0,     0,   400,    79,    80,   401,   402,   403,   404,     0,
       0,     0,     0,     0,     0,   387,   388,     0,   389,   390,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     391,   392,   379,     0,   393,   394,   395,   382,   396,   397,
     383,     0,   384,     0,   385,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   386,     0,     0,     0,     0,   398,     0,     0,    77,
     399,     0,     0,  1042,     0,     0,   400,    79,    80,   401,
     402,   403,   404,     0,     0,     0,     0,     0,     0,   387,
     388,     0,   389,   390,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   391,   392,   379,     0,   393,   394,
     395,   382,   396,   397,   383,     0,   384,     0,   385,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   386,     0,     0,     0,     0,
     398,  1377,     0,    77,   399,     0,     0,     0,     0,     0,
     400,    79,    80,   401,   402,   403,   404,     0,     0,     0,
       0,     0,     0,   387,   388,     0,   389,   390,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   391,   392,
     379,     0,   393,   394,   395,   382,   396,   397,   383,     0,
     384,     0,   385,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   386,
       0,     0,     0,     0,   398,     0,     0,    77,   399,     0,
       0,     0,  1449,     0,   400,    79,    80,   401,   402,   403,
     404,     0,     0,     0,     0,     0,     0,   387,   388,     0,
     389,   390,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   391,   392,   379,     0,   393,   394,   395,   382,
     396,   397,   383,     0,   384,     0,   385,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   386,     0,     0,     0,     0,   398,     0,
       0,    77,   399,     0,     0,     0,  1524,     0,   400,    79,
      80,   401,   402,   403,   404,     0,     0,     0,     0,     0,
       0,   387,   388,     0,   389,   390,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   391,   392,   379,     0,
     393,   394,   395,   382,   396,   397,   383,     0,   384,     0,
     385,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   386,     0,     0,
       0,     0,   398,     0,  1945,    77,   399,     0,     0,     0,
       0,     0,   400,    79,    80,   401,   402,   403,   404,     0,
       0,     0,     0,     0,     0,   387,   388,     0,   389,   390,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     391,   392,   379,     0,   393,   394,   395,   382,   396,   397,
     383,     0,   384,     0,   385,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   386,     0,     0,     0,     0,   398,  1950,     0,    77,
     399,     0,     0,     0,     0,     0,   400,    79,    80,   401,
     402,   403,   404,     0,     0,     0,     0,     0,     0,   387,
     388,     0,   389,   390,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   391,   392,   379,     0,   393,   394,
     395,   382,   396,   397,   383,     0,   384,     0,   385,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   386,     0,     0,     0,     0,
     398,  1959,     0,    77,   399,     0,     0,     0,     0,     0,
     400,    79,    80,   401,   402,   403,   404,     0,     0,     0,
       0,     0,     0,   387,   388,     0,   389,   390,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   391,   392,
     379,     0,   393,   394,   395,   382,   396,   397,   383,     0,
     384,     0,   385,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   386,
       0,     0,     0,     0,   398,  2045,     0,    77,   399,     0,
       0,     0,     0,     0,   400,    79,    80,   401,   402,   403,
     404,     0,     0,     0,     0,     0,     0,   387,   388,     0,
     389,   390,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   391,   392,   379,     0,   393,   394,   395,   382,
     396,   397,   383,     0,   384,     0,   385,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   386,     0,     0,     0,     0,   398,  2047,
       0,    77,   399,     0,     0,     0,     0,     0,   400,    79,
      80,   401,   402,   403,   404,     0,     0,     0,     0,     0,
       0,   387,   388,     0,   389,   390,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   391,   392,   379,     0,
     393,   394,   395,   382,   396,   397,   383,     0,   384,     0,
     385,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   386,     0,     0,
       0,     0,   398,  2099,     0,    77,   399,     0,     0,     0,
       0,     0,   400,    79,    80,   401,   402,   403,   404,     0,
       0,     0,     0,     0,     0,   387,   388,     0,   389,   390,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     391,   392,   379,     0,   393,   394,   395,   382,   396,   397,
     383,     0,   384,     0,   385,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   386,     0,     0,     0,     0,   398,  2101,     0,    77,
     399,     0,     0,     0,     0,     0,   400,    79,    80,   401,
     402,   403,   404,     0,     0,     0,     0,     0,     0,   387,
     388,     0,   389,   390,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   391,   392,   379,     0,   393,   394,
     395,   382,   396,   397,   383,     0,   384,     0,   385,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   386,     0,     0,     0,     0,
     398,  2103,     0,    77,   399,     0,     0,     0,     0,     0,
     400,    79,    80,   401,   402,   403,   404,     0,     0,     0,
       0,     0,     0,   387,   388,     0,   389,   390,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   391,   392,
     379,     0,   393,   394,   395,   382,   396,   397,   383,     0,
     384,     0,   385,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   386,
       0,     0,     0,     0,   398,  2106,     0,    77,   399,     0,
       0,     0,     0,     0,   400,    79,    80,   401,   402,   403,
     404,     0,     0,     0,     0,     0,     0,   387,   388,     0,
     389,   390,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   391,   392,   379,     0,   393,   394,   395,   382,
     396,   397,   383,     0,   384,     0,   385,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   386,     0,     0,     0,     0,   398,  2108,
       0,    77,   399,     0,     0,     0,     0,     0,   400,    79,
      80,   401,   402,   403,   404,     0,     0,     0,     0,     0,
       0,   387,   388,     0,   389,   390,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   391,   392,   379,     0,
     393,   394,   395,   382,   396,   397,   383,     0,   384,     0,
     385,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   386,     0,     0,
       0,     0,   398,  2152,     0,    77,   399,     0,     0,     0,
       0,     0,   400,    79,    80,   401,   402,   403,   404,     0,
       0,     0,     0,     0,     0,   387,   388,     0,   389,   390,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     391,   392,   379,     0,   393,   394,   395,   382,   396,   397,
     383,     0,   384,     0,   385,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   386,     0,     0,     0,     0,   398,  2154,     0,    77,
     399,     0,     0,     0,     0,     0,   400,    79,    80,   401,
     402,   403,   404,     0,     0,     0,     0,     0,     0,   387,
     388,     0,   389,   390,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   391,   392,   379,     0,   393,   394,
     395,   382,   396,   397,   383,     0,   384,     0,   385,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   386,     0,     0,     0,     0,
     398,  2156,     0,    77,   399,     0,     0,     0,     0,     0,
     400,    79,    80,   401,   402,   403,   404,     0,     0,     0,
       0,     0,     0,   387,   388,     0,   389,   390,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   391,   392,
     379,     0,   393,   394,   395,   382,   396,   397,   383,     0,
     384,     0,   385,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   386,
       0,     0,     0,     0,   398,  2181,     0,    77,   399,     0,
       0,     0,     0,     0,   400,    79,    80,   401,   402,   403,
     404,     0,     0,     0,     0,     0,     0,   387,   388,     0,
     389,   390,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   391,   392,   379,     0,   393,   394,   395,   382,
     396,   397,   383,     0,   384,     0,   385,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   386,     0,     0,     0,     0,   398,  2183,
       0,    77,   399,     0,     0,     0,     0,     0,   400,    79,
      80,   401,   402,   403,   404,     0,     0,     0,     0,     0,
       0,   387,   388,     0,   389,   390,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   391,   392,   379,     0,
     393,   394,   395,   382,   396,   397,   383,     0,   384,     0,
     385,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   386,     0,     0,
       0,     0,   398,  2185,     0,    77,   399,     0,     0,     0,
       0,     0,   400,    79,    80,   401,   402,   403,   404,     0,
       0,     0,     0,     0,     0,   387,   388,     0,   389,   390,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     391,   392,   379,     0,   393,   394,   395,   382,   396,   397,
     383,     0,   384,     0,   385,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   386,     0,     0,     0,     0,   398,     0,     0,    77,
     399,     0,     0,     0,     0,     0,   400,    79,    80,   401,
     402,   403,   404,     0,     0,     0,     0,     0,     0,   387,
     388,     0,   389,   390,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   391,   392,   379,     0,   393,   394,
     395,   382,   396,   397,   383,     0,   384,     0,   385,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   386,     0,     0,     0,     0,
     684,     0,     0,    77,   399,     0,     0,     0,     0,     0,
     400,    79,    80,   401,   402,   403,   404,     0,     0,     0,
       0,     0,     0,   387,   388,     0,   389,   390,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   391,   392,
     379,     0,   393,   394,   395,   382,   396,   397,   383,     0,
     384,     0,   385,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   386,
       0,     0,     0,     0,   690,     0,     0,    77,   399,     0,
       0,     0,     0,     0,   400,    79,    80,   401,   402,   403,
     404,     0,     0,     0,     0,     0,     0,   387,   388,     0,
     389,   390,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,   391,   392,   379,     0,   393,   394,   395,   382,
     396,   397,   383,     0,   384,     0,   385,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   386,     0,     0,     0,     0,   699,     0,
       0,    77,   399,     0,     0,     0,     0,     0,   400,    79,
      80,   401,   402,   403,   404,     0,     0,     0,     0,     0,
       0,   387,   388,     0,   389,   390,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,   391,   392,   379,     0,
     393,   394,   395,   382,   396,   397,   383,     0,   384,     0,
     385,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   386,     0,     0,
       0,     0,   398,     0,     0,    77,   399,     0,     0,     0,
       0,     0,   400,   916,    80,   401,   402,   403,   404,     0,
       0,     0,     0,     0,     0,   387,   388,     0,   389,   390,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
     391,   392,   379,     0,   393,   394,   395,   382,   396,   397,
     383,     0,   384,     0,   385,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   386,     0,     0,     0,     0,   398,     0,     0,    77,
     399,     0,     0,     0,     0,     0,   400,   461,    80,   401,
     402,   403,   404,     0,     0,     0,     0,     0,     0,   387,
     388,     0,   389,   390,  2040,    64,    65,    66,    67,    68,
      69,    70,    71,    72,   391,   392,   379,     0,   393,   394,
     395,     0,   396,   397,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     398,     0,     0,    77,   399,     0,     0,     0,     0,     0,
     400,    79,    80,   401,   402,   403,   404,   183,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,     0,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   184,     0,   185,
     186,   183,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,     0,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   693,     0,   694,   695,    14,    15,    16,    17,    18,
       0,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,  -482,
    -482,     0,  -482,    46,     0,    47,     0,  -482,     0,     0,
       0,     0,     0,     0,     0,     0,    14,    15,    16,    17,
      18,     0,     0,    20,    58,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
    -483,  -483,     0,  -483,    46,     0,    47,     0,  -483,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    58
};

static const yytype_int16 yycheck[] =
{
       1,    75,    75,     4,   166,    75,    75,   166,   182,   398,
     230,   182,     1,   251,   256,   302,   372,   345,   731,   203,
     343,     1,   177,   716,  1223,   932,   925,   488,     1,  1205,
     230,   647,   230,  1017,   400,  1165,   233,   360,   235,    77,
     218,   364,   638,  1361,  1362,   242,   360,  1797,    59,   230,
     364,   167,   775,   634,   634,    56,    57,    89,    59,   709,
    1797,   968,  1046,  1797,   181,   160,   802,     0,    75,   634,
      59,    84,   167,   141,    75,   271,     0,    97,   230,    59,
     230,   545,   546,    84,   230,     1,    59,     1,  1205,     1,
       4,    92,    75,     1,   314,    84,    97,     1,  1441,   100,
     805,   166,   230,   104,  1939,   804,   811,    75,    97,   804,
    1676,   100,   309,   310,   314,   104,   314,  1190,   192,   893,
       1,   802,   192,   192,     1,  1109,   230,   909,   160,    89,
     230,   605,    72,   314,   202,     1,   147,   150,     4,   462,
     468,   142,   802,    59,   145,    59,   147,    59,   922,   134,
     134,    59,   153,   822,   802,    59,   230,   230,   147,   160,
     230,   230,   314,   158,   314,    97,   167,   147,   314,    97,
      84,   840,    75,    75,   147,     0,   556,    72,    59,    72,
     821,   822,   177,   168,   168,   192,   314,   567,   100,   398,
     191,   192,   158,    59,   160,   152,   104,  1798,     1,   840,
     160,     4,   191,    61,    62,   206,    98,   671,   804,   192,
     314,   231,   152,   909,   314,   216,   156,   206,    89,   176,
     221,   802,   802,   230,   192,  1441,  1442,   141,   608,   230,
     231,   147,   240,   147,   100,   147,   150,   802,   104,   147,
     314,   314,   231,   147,   314,   314,   259,   230,   118,   918,
     251,     4,  2087,  1672,    72,   293,    59,   152,   259,   152,
     268,   974,   230,   156,  1943,    72,   147,    84,   269,   270,
     259,   279,   273,   158,   177,   177,  1842,   918,   343,   280,
      97,   147,   159,   100,   273,   177,   506,   104,   202,   160,
    1003,   640,   177,   294,   295,   315,   297,   152,   626,   231,
     623,   104,   160,   231,  1480,   543,   506,   594,   506,   623,
     323,   549,   668,   314,   315,    10,   494,   152,   782,   177,
     686,  1162,   650,   324,   647,   506,   315,  1928,  1169,   657,
     331,   332,  1055,   529,   152,   336,  1072,   660,   156,  1174,
     729,   537,   992,   257,   147,   259,   660,   821,   822,   156,
     466,   104,    20,   949,   506,    72,   506,  1700,   133,  1476,
     506,   273,  1479,  1480,     1,   666,   840,   551,  1073,  2119,
    1174,  1278,    90,   374,   191,    62,   377,   378,   506,  1286,
    1079,   601,  2119,   315,  1079,  2119,   302,   315,   302,   206,
     458,  1072,  1166,    99,   302,   146,   133,   462,   302,  1529,
    1182,   601,   506,   601,   158,  1246,   506,   273,    10,   323,
     156,   337,  1072,   123,   231,   161,     4,   168,   105,   432,
     601,   302,   109,   177,  1072,   112,   157,   114,  1847,  1848,
    1212,  1174,   506,   345,   152,   152,   506,   506,   175,   156,
     624,   442,   259,   605,   918,   155,   152,   222,   486,   601,
     376,   601,  1121,   442,  2055,   601,   273,   806,   118,  1777,
    1778,   810,    84,  2142,   465,   466,   152,   162,    56,    57,
     819,   820,   167,   601,    89,   684,   477,   478,   687,   688,
    1121,   690,  1319,  1079,  1700,   486,  1182,   488,    77,    78,
     699,  1072,  1072,   702,   703,   704,   156,   601,   518,   506,
    2179,   601,    77,    78,    92,   506,   622,  1072,   160,   280,
     890,    72,   155,   239,   988,   132,  1212,   518,   432,  2120,
     246,   158,   159,   506,   295,   177,   827,   601,   303,   518,
     152,   601,   601,   176,  1710,   152,  1955,  1956,   506,   156,
     605,   267,   543,   158,   458,   160,   163,   164,   549,   151,
     154,    72,   278,   149,   142,   159,   468,   145,   566,   176,
     162,  2162,    72,  1547,   160,   167,  1271,  1408,   106,   107,
     590,   160,   160,   901,   758,   262,  1411,  1412,  1413,   167,
     176,   152,   647,   603,    75,   160,   518,   588,  1049,   590,
     518,   152,    72,  1710,   160,   156,   607,   203,  1671,    90,
     601,   590,   603,  1676,   842,    72,   607,  1411,  1412,  1413,
     938,   177,   160,   936,   603,   799,   617,   155,   607,   167,
     621,   622,   936,   398,   152,   442,   152,   607,   154,  1845,
     868,   152,   160,   221,   607,   156,  1329,   821,   822,   756,
     158,   815,   152,    72,   815,  1350,   156,  1121,   152,  1265,
     110,   829,   826,   654,   110,    72,   840,  2090,   590,   177,
     347,   348,   590,   350,   152,   352,   667,     3,  1411,  1412,
    1413,   603,   152,   133,   588,   603,   156,   133,   594,   591,
     594,   269,   270,  2116,   592,   152,   594,  2005,   804,   156,
     594,   607,   280,   607,   154,   607,   177,    72,   109,   607,
      72,   518,   389,   607,   106,   107,   294,   295,   624,   297,
      72,  2144,    72,   594,   158,  1552,  1553,  1554,  1431,   757,
     721,   132,   723,   152,   725,    72,   607,   156,   729,   177,
     158,   732,  1631,   177,   918,   152,   324,   343,   650,   156,
     346,   607,   158,   331,   332,  1441,  1442,  1096,   336,   177,
      72,   154,   869,    72,   360,   158,   757,   532,   364,  1452,
      58,   177,    75,    61,    62,   540,    64,    18,   158,  1842,
      13,    14,    15,    16,    17,   158,  1760,   152,  1762,    92,
     152,   156,   557,    72,   156,  1984,   374,   177,  1987,   377,
     152,  1255,   152,   568,   156,   108,   156,   374,   485,    75,
     377,   802,   158,   804,   607,   152,    69,   533,  2024,   156,
      61,    62,    63,    64,    90,   816,  1205,   134,  1725,  1726,
    1294,   177,   823,   146,   147,   148,   988,     3,   829,    72,
     152,   832,   558,   152,   156,   160,  2052,   156,   158,   565,
     841,   842,   843,   569,   960,   168,   163,   164,   168,   155,
    1311,  1544,   152,   104,   152,    75,   462,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   158,   868,  1042,   128,
     129,    91,   158,   156,   158,  2091,   563,   465,   161,    62,
    1953,   158,   168,   799,   168,   177,   158,  1361,   108,   477,
     478,   134,  1965,    13,    14,    15,    16,    17,   154,   154,
     177,   176,   158,  1259,  1260,   156,   907,   908,   909,   684,
     165,   166,  1108,   172,   173,   690,   160,   100,    13,    14,
      15,    16,    17,   988,   699,   146,   147,   148,   111,   909,
     113,   154,   115,   156,   160,  1531,   909,  1121,  1116,   158,
     152,  1269,  1265,   718,  1118,   551,   151,   168,   160,   168,
    1147,   158,    72,   158,   158,   167,   177,  1258,   152,   960,
     132,   168,  1078,  1079,   168,   159,  2039,   152,   154,   153,
     152,   154,   973,   159,   157,   158,   160,    72,   151,   154,
     152,   174,  1889,   158,   156,   158,  1287,   152,  1449,   901,
     152,   163,   164,   909,   156,   909,  1205,   909,   154,   605,
     588,   909,   914,    58,  1700,   909,    61,    62,  1009,    64,
     924,  1249,   106,   107,   134,   118,  1017,   623,   624,   154,
     132,   155,   156,   206,   159,   159,   938,  1416,   909,   617,
    1504,   163,  1274,   621,   622,  1336,   154,   132,   170,   171,
     152,   647,  1624,   909,   156,  1046,   154,  1629,  1049,   154,
     154,   163,   164,   158,   660,   159,   154,   152,   924,    47,
      48,   156,    50,  1524,   154,   155,   654,    55,   163,   164,
     152,  1072,   154,     3,   156,   154,   154,  1078,  1079,   667,
     158,  1542,   154,    13,    14,    15,    16,    17,  1995,  1298,
     273,   152,   275,   276,    13,    14,    15,    16,    17,   152,
     875,  1256,  1257,   152,   152,   154,   909,   156,  1109,    22,
    1284,   158,   887,  1284,   132,     3,   891,   152,   154,   132,
     895,   924,   158,   152,   307,    13,    14,    15,    16,    17,
     313,  1293,  1294,   721,   152,   723,   152,   725,   156,   152,
     156,   729,    72,   156,   732,   163,   164,   126,   127,  1845,
     163,   164,   758,    72,  1441,   152,   154,   154,  1332,   156,
     158,  1174,   345,   154,   158,  1521,   892,   158,   351,   757,
     353,   924,   111,   112,   113,   114,   115,  1361,  1362,   165,
     166,   154,   158,  1184,    72,   158,  1187,  1188,  1189,   158,
     152,   158,   154,   799,   156,  1369,  1370,   152,  1369,  1370,
    1265,  1397,  1182,  1362,   152,    99,   389,   160,   156,  1182,
     152,  1212,   152,    89,   156,  2124,   156,  1218,  1519,  2128,
      13,    14,    15,    16,    17,  1526,   160,   154,   816,  1294,
    1231,   158,  1212,  1234,  1235,   823,   154,  1238,   160,  1212,
     158,   151,  1543,   154,   167,  1234,  1235,   158,  1249,    84,
     154,  1165,   154,   841,   158,   843,   158,   130,   131,   442,
    1174,   146,   147,   148,   157,   154,  1182,  1980,  1182,   158,
    1182,   154,   154,   158,  1182,   158,   158,   154,  1182,    72,
     154,  1282,   154,   168,   158,   468,   158,   470,   471,   154,
     176,   154,   177,   158,  1295,   158,  1212,   154,  1212,  1165,
    1212,  1182,   485,  1777,  1212,   736,   737,   738,  1212,   121,
    1311,   123,   124,   125,   118,   150,  1182,   158,  1319,   907,
     908,   909,  1234,   157,   158,   154,   152,  1235,  2024,   158,
     936,  1212,   154,   939,   154,   518,   158,   154,   158,   132,
     152,   158,  1504,   155,   156,   169,  1212,   154,   160,   161,
    1351,   158,  1621,  1622,  1623,   190,  2052,  1269,   152,   152,
     543,   152,  1165,   156,   152,   548,   164,   550,  1234,  1235,
     163,   164,   960,   162,   146,   147,   148,   157,   158,  1182,
    1681,   132,   988,  1739,   163,   164,   158,   174,   571,   155,
     573,   574,   154,  1168,   154,  2091,   168,   154,  1411,  1412,
    1413,   154,  1422,  1416,  1417,   177,  1181,   156,   591,  1212,
     157,   158,  1165,  1700,   154,  1635,   154,  1234,  1235,   134,
     603,  1422,   157,  1198,   259,  1426,   157,   158,  1429,   134,
    1205,   159,  1235,   157,   158,  1635,   159,  1635,   158,  1504,
     157,   158,  1743,   626,   152,   628,   629,   154,  1449,   743,
     744,   745,   746,  1754,  1635,    13,    14,    15,    16,    17,
      18,  1441,  1442,   154,  1648,   158,   159,   650,   651,   157,
     158,  1472,  1473,   154,   657,    13,    14,    15,    16,    17,
      18,  1482,  1235,  1635,    78,  1635,    91,    92,   323,  1635,
    1422,   157,   158,  1482,  1422,   157,   158,  1411,  1412,  1413,
    1414,  1415,  1416,  1417,  1171,  1172,  1173,  1635,   343,   154,
     104,   154,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   154,  1524,   154,  1441,  1442,  1441,  1442,  1441,
    1442,  1635,   154,  1441,  1442,  1635,   152,  1441,  1442,   157,
     158,  1542,   154,  1752,   157,   158,  1547,   176,  1414,   157,
     158,  1552,  1553,  1554,   157,   158,  1718,   157,   158,  1718,
    1441,  1442,  1635,    13,    14,    15,    16,    17,    18,   157,
    1482,  1642,  1643,  1644,   156,  1441,  1442,    13,  1879,  1880,
     104,  1698,   160,   177,   108,   109,   110,   111,   112,   113,
     114,   115,  1812,  1777,  1778,   160,  1184,   432,   160,  1187,
    1188,  1189,   157,   158,  1624,   157,   158,   157,   158,  1629,
      70,  1414,  1812,   160,  1812,  1529,  1482,  1637,   160,  1778,
     157,   158,   157,  1624,  1212,   157,   158,   462,  1629,   153,
    1218,  1812,   156,   152,  1635,    78,  1637,   157,  1441,  1442,
    1878,   158,   159,  1231,  1645,   157,   158,   157,   158,  1972,
    1238,    18,    88,  1718,    77,    78,  1657,  1432,  1433,  1265,
    1812,  1414,  1812,  1529,   158,  1482,  1812,   176,   104,  1670,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     158,   159,  1320,  1321,  1812,   739,   740,   160,  1294,   741,
     742,   152,  1624,   177,  1282,  1996,  1624,  1629,  2069,  2070,
      72,  1629,   747,   748,   160,  1637,  1481,  1295,  1812,  1637,
     545,   546,  1812,   154,  1715,   154,  1643,  1644,   901,   177,
    1700,  1522,  1523,  1256,  1257,   160,  1529,   157,  2056,    18,
    2053,   914,   104,   157,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,  1861,    13,    14,    15,    16,    17,
      18,   151,  1664,   154,   151,   938,   154,  1665,   154,  1760,
     154,  1762,   154,   154,   154,  1939,   949,   154,  1939,   154,
      22,   154,   154,  2074,   151,   958,  1529,    70,   154,   160,
     152,   153,   160,   176,  1700,   160,  1700,   176,  1700,   177,
     154,   154,  1700,   154,   151,  1815,  1700,   176,  1664,  1665,
     160,   160,   154,   154,   158,   158,   154,  1721,   154,   158,
     241,  1812,   647,   154,  1815,   154,   154,  2055,   154,  1700,
     154,  2005,   154,   151,   157,  1826,   157,   154,   154,  1830,
     154,   154,   154,   154,  1700,   154,   671,   154,  1426,   157,
     154,  1429,   154,  1844,   154,   158,  2005,   151,   154,    18,
     158,   152,   152,  1854,   216,  1721,   152,   152,   152,  1976,
     152,    14,  1665,    74,   151,  1845,  1867,   158,  1869,  1870,
    1871,  1872,  1055,   159,   177,   159,  1877,  1878,   177,   157,
     157,   160,   158,  1815,  1472,  1473,   157,  1815,    57,    58,
      59,    60,    61,    62,    63,    64,  1079,  1700,  1504,  2119,
       4,     5,     6,     7,     8,     9,    10,    11,    12,   154,
     177,  2085,  1665,  2087,  2085,  1690,  2087,  1937,  1721,  2119,
    1832,  2119,   177,   177,   154,   158,   158,   154,   157,  1845,
     158,  1845,   154,  1845,   157,   154,  1937,  1845,  2119,   151,
     177,  1845,  1943,   151,    80,   152,  1947,   782,   152,    92,
     152,  1952,  2126,   177,   151,  2126,   177,   177,   152,  1972,
     177,    65,   177,   177,  1845,   152,  1832,  2119,  1721,  2119,
      90,   177,   177,  2119,   154,   151,  1977,   151,   151,  1845,
     158,   158,   121,   154,   177,   160,  2160,   157,   160,   157,
     157,  2119,   104,   157,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   435,  1937,   151,  2124,   154,  1937,
     159,  2128,  2129,   151,   154,  2119,   151,   154,   159,  2119,
     451,   154,   154,   454,   151,   159,  2027,   157,   157,   154,
    2031,   152,   154,   152,  2208,   158,   152,  2208,   151,   157,
    2053,   151,  1845,  2044,  2024,  2119,  2119,  2164,   157,  2119,
    2119,  1234,   157,   160,  2055,   154,  2057,  1645,   151,   154,
     154,   157,   154,     1,   154,   177,     4,   154,   154,  1657,
     154,    75,  2052,    75,  2191,   177,   177,   151,  2195,   152,
     511,   157,  1670,   154,   154,   154,  1269,   152,   177,   157,
     925,   151,  1275,  2094,   160,   151,  2213,   151,   156,   154,
    2120,   154,   154,   154,   154,   154,   154,    75,  2024,   168,
    2024,  2091,  2024,   155,   177,    75,  2024,   151,  2119,  2120,
    2024,    59,   168,   159,   486,   151,   488,  1715,   158,  2130,
     177,  2120,  2133,   177,  2135,   154,  2052,    75,  2052,   154,
    2052,  2142,  2162,  2024,  2052,   154,    84,   154,  2052,   159,
     151,   583,    13,    14,    15,    16,    17,   153,  2024,    97,
     168,  2162,   100,   168,   151,   104,   104,   152,   158,    75,
    2171,  2052,  2173,  2162,   151,  2091,  2177,  2091,  2179,  2091,
     168,   153,   168,  2091,   177,   177,  2052,  2091,  2120,   104,
     157,    75,  2120,   108,   109,   110,   111,   112,   113,   114,
     115,  2202,    75,   141,   151,   153,   159,   151,   154,   147,
    2091,    72,   150,  2214,   154,   153,   154,   151,   154,   152,
     177,  2024,  2223,   154,   154,  2091,   177,   177,   166,  1752,
    2162,   708,   750,  1326,  2162,   749,   751,   753,  1826,   752,
    1423,  1200,  1830,   431,   386,  2179,  1212,  1700,  2087,  2052,
    1853,  2116,   190,   191,   192,  1845,  1844,  1712,  1709,  2159,
      62,  1479,  2053,  2052,   202,   203,  1854,  1691,   206,   411,
     412,   132,  1691,  2129,  2195,  1238,    49,   112,  1937,  1867,
     264,  1869,  1870,  1871,  1872,  2014,  1417,    72,  2091,  1877,
     432,   152,   230,   231,   964,   156,  1231,   495,   100,  1482,
     829,   928,   163,   164,     0,   973,   617,  1721,   774,   111,
     112,   774,   774,   251,    -1,  1612,    -1,    -1,    -1,   104,
     462,   259,    -1,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,    -1,    -1,    -1,   273,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   774,   775,    -1,    -1,   132,    -1,    -1,
      -1,    -1,   154,   784,    -1,  1943,   787,    -1,    -1,  1947,
      -1,    -1,    -1,    -1,  1952,    -1,  1972,   152,   153,   307,
      -1,    -1,    -1,    -1,    -1,   313,   314,   315,   163,   164,
      -1,    -1,    -1,    -1,    -1,   323,    -1,    -1,   104,  1977,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
      -1,    -1,    -1,    -1,   206,   343,   344,   345,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2190,    -1,   848,    -1,  1184,
    1255,    -1,   360,    -1,   855,    -1,   364,    -1,   859,  2204,
    1265,    -1,   863,    -1,    -1,    -1,    -1,    -1,    -1,  2027,
      -1,    -1,    -1,  2031,   876,    -1,    -1,  2053,   876,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2044,    -1,    -1,    -1,
     398,   177,    -1,    -1,    -1,    -1,    -1,   829,   104,  2057,
     832,   273,   108,   109,   110,   111,   112,   113,   114,   115,
     116,  1664,    -1,    -1,   120,    -1,   122,    -1,    -1,    -1,
      65,    66,    67,    68,   432,    -1,    -1,   435,    -1,    -1,
      -1,    -1,    -1,    -1,   442,   307,  2094,    -1,    -1,   941,
      -1,   313,    -1,   941,    -1,    -1,    -1,   153,    -1,    -1,
     458,    -1,    -1,    -1,   462,    -1,    -1,    -1,   466,   104,
     468,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,  2130,   345,    -1,  2133,    -1,  2135,    -1,    -1,
      -1,    -1,    -1,   104,  2142,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,   104,    -1,   506,    -1,
     108,   109,   110,   111,   112,   113,   114,   115,   116,    -1,
     518,   156,   120,  2171,   122,  2173,    -1,   389,    -1,  2177,
      -1,  2179,    -1,   735,    -1,    -1,    -1,    -1,    -1,   174,
      -1,    -1,    -1,    -1,    -1,   543,    -1,   545,   546,    -1,
      -1,   549,   163,   551,  2202,   153,    -1,    -1,   156,  1051,
      -1,    -1,    -1,  1051,  1055,    -1,  2214,    -1,    -1,    -1,
      -1,    -1,  1815,    -1,    -1,  2223,  1068,  1069,    -1,    -1,
    1068,  1069,    -1,    -1,    -1,    -1,    -1,  1009,    -1,  1832,
      -1,    -1,   590,    -1,    -1,  1017,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   601,    -1,   603,   468,   605,    -1,   607,
      -1,   104,     3,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   485,  1046,   623,   624,  1049,   626,    -1,
      -1,    -1,    -1,    -1,    -1,  1126,   634,    -1,  1129,    -1,
     638,    -1,  1133,    -1,    -1,    -1,   104,  1472,  1473,   647,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   657,
      -1,    -1,   660,    -1,    -1,    -1,    -1,   160,    -1,    -1,
      -1,    -1,     1,   671,    -1,     4,     4,     5,     6,     7,
       8,     9,    10,    11,    12,  1928,   684,  1109,   550,   687,
     688,    -1,   690,    -1,    -1,    -1,    -1,    -1,   156,    -1,
      -1,   699,    -1,    -1,   702,   703,   704,    -1,    -1,   571,
      -1,    -1,    -1,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,    -1,    -1,    -1,   591,
      59,    -1,    -1,    -1,    -1,    -1,  1631,    -1,    -1,    -1,
     104,   132,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,    -1,    -1,    84,    -1,    -1,    -1,    -1,
     758,   152,   153,    -1,   626,    -1,    -1,    -1,   159,    -1,
      -1,   100,   163,   164,    -1,   104,   774,   775,    -1,    -1,
      -1,    -1,   984,    -1,   782,    -1,    -1,   989,   650,    -1,
      -1,   155,    -1,    -1,    -1,   657,   160,    -1,  1000,    -1,
      -1,   799,    -1,    -1,   802,    -1,   804,    -1,    -1,    -1,
      -1,    -1,   141,  2056,    -1,    -1,    -1,    -1,   147,    -1,
      -1,   150,    -1,   821,   822,   154,    -1,    -1,    -1,    -1,
      -1,    -1,  1657,    -1,    -1,    -1,   165,   166,   167,    -1,
      -1,    -1,   840,    -1,   842,  1670,    -1,  1338,    -1,    -1,
    1342,  1343,    -1,    -1,  1342,  1343,  1347,    -1,    -1,    -1,
      -1,   190,    -1,    -1,  1356,  1357,    -1,    -1,  1356,  1357,
     868,    -1,    -1,   202,   203,    -1,   104,   206,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,    -1,  1311,
    1715,    -1,    -1,    -1,    -1,    -1,    -1,  1319,    -1,    -1,
      -1,    -1,  1394,  1395,  1396,    -1,  1394,  1395,  1396,    -1,
      -1,   909,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2162,
     918,    -1,    -1,    -1,    -1,    -1,   924,   925,   257,    -1,
     259,    -1,   160,    -1,    -1,    -1,    -1,    -1,   936,    -1,
     938,    -1,    -1,    -1,   273,    -1,    -1,    -1,   104,    -1,
      -1,   949,   108,   109,   110,   111,   112,   113,   114,   115,
      -1,    -1,    -1,    -1,    -1,   294,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   302,    -1,    -1,   132,    -1,   307,    -1,
    1182,    -1,    -1,    -1,   313,    -1,    -1,    -1,    -1,    -1,
     988,    -1,    -1,    -1,   323,    -1,   152,   153,    -1,    -1,
     156,  1826,    -1,    -1,    -1,  1830,    -1,   163,   164,    -1,
     872,    -1,    -1,    -1,   343,    -1,   345,   346,    -1,  1844,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1449,    -1,  1854,
      -1,   360,    -1,    -1,    -1,   364,    -1,    -1,    -1,   901,
      -1,    -1,  1867,    -1,  1869,  1870,  1871,  1872,    -1,    -1,
      -1,    -1,   914,    -1,    -1,    -1,    -1,  1055,    -1,    -1,
      -1,    -1,    -1,  1265,    -1,    -1,    -1,    -1,    -1,   398,
      -1,    -1,    -1,    -1,  1072,    -1,   938,    -1,    -1,    -1,
     104,  1079,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    13,    14,    15,    16,    17,    -1,    -1,  1301,
    1302,  1303,  1524,   432,    -1,    -1,  1308,  1309,    -1,    -1,
    1602,  1603,  1604,    -1,  1602,  1603,  1604,    -1,  1943,    -1,
    1542,    -1,  1947,  1121,    -1,  1547,    -1,  1952,    -1,   458,
    1552,  1553,  1554,   462,  1625,    -1,   160,    -1,    -1,   468,
     135,   136,   137,   138,   139,   140,   141,   142,   143,   144,
     145,    72,  1977,    -1,   149,   104,  1648,    -1,    -1,   108,
     109,   110,   111,   112,   113,   114,   115,  1165,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1174,    -1,    -1,    -1,
      -1,   176,    -1,   104,  1182,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2027,   152,   153,    -1,  2031,  1205,    -1,    -1,
      -1,   132,    -1,    -1,  1212,    -1,   545,   546,    -1,  2044,
      -1,   550,   551,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   152,   153,    -1,    -1,   156,  1234,  1235,    -1,    -1,
    1732,    -1,   163,   164,  1732,    -1,    -1,    -1,    -1,    -1,
      -1,  1249,    -1,    -1,   583,    -1,    -1,  1255,    -1,   588,
      -1,    -1,   591,   592,    -1,   594,    -1,  1265,    -1,  2094,
      -1,    -1,    -1,    -1,    -1,  1767,   605,    -1,   607,  1767,
    1772,  1773,    -1,    -1,  1772,  1773,    -1,    -1,    -1,    -1,
      -1,    -1,   621,    -1,   623,   624,  1294,   626,    -1,    -1,
    1298,    -1,    -1,    -1,    -1,  2130,  1797,  1798,  2133,    -1,
    2135,    -1,    -1,    -1,    -1,    -1,    -1,  2142,   647,    -1,
      -1,   650,    -1,    -1,    -1,   654,    -1,    -1,   657,    -1,
      -1,   660,    -1,   662,    -1,    -1,    -1,    -1,  1760,    -1,
    1762,    -1,   671,    -1,    -1,    -1,  2171,    -1,  2173,    -1,
      72,    -1,  2177,    -1,  2179,   684,    -1,    -1,   687,   688,
      -1,   690,    -1,  1361,  1362,    -1,    -1,    -1,    -1,    -1,
     699,    -1,  1234,   702,   703,   704,    -1,  2202,    -1,    -1,
      -1,    -1,   104,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,    -1,    -1,    -1,    -1,    -1,    13,
      14,    15,    16,    17,    -1,    -1,    -1,  1269,    -1,    -1,
     132,    -1,    -1,  1411,  1412,  1413,  1414,    -1,  1416,  1417,
      -1,    -1,    -1,    -1,  1422,  1423,    -1,    -1,    -1,   758,
     152,   153,    -1,    -1,   156,    -1,    -1,  1928,    -1,    -1,
      -1,   163,   164,  1441,  1442,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   782,    -1,    -1,   104,    -1,    72,    -1,
     108,   109,   110,   111,   112,   113,   114,   115,   116,    -1,
     799,    -1,   120,    -1,   122,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1482,    -1,    -1,    -1,    -1,    -1,
     104,    -1,   821,   822,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,  1705,  1706,   153,  1504,    -1,   156,    -1,
      -1,   840,    -1,    -1,    -1,    -1,    -1,    -1,   132,  2010,
      -1,    -1,    -1,  2014,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1529,    -1,  1531,    -1,    -1,    -1,    -1,   152,   153,
      -1,  1403,    -1,   872,    -1,    -1,    -1,   876,    -1,   163,
     164,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1423,    -1,    -1,  2055,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   901,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     909,    -1,    -1,    -1,    -1,   914,    -1,     1,    -1,   918,
      -1,    -1,    -1,    -1,    -1,   924,   925,    -1,    13,    14,
      15,    16,    17,    -1,    -1,    -1,    -1,   936,    -1,   938,
     939,    -1,   941,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1482,    -1,    -1,    -1,    -1,    -1,  1624,  1625,  2119,  2120,
      -1,  1629,    -1,  1631,    -1,    -1,    -1,  1635,    -1,  1637,
      -1,    -1,    -1,    -1,    72,    59,    -1,  2139,    -1,    -1,
      -1,  2139,    -1,    -1,    -1,    -1,    -1,    72,    -1,   988,
      -1,    -1,    -1,    -1,    -1,    -1,  1664,  1665,    -1,    -1,
      -1,  2162,    -1,    -1,    -1,    -1,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,    -1,   104,
     104,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,  1700,   104,   132,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,    -1,   132,    -1,    -1,
    1718,    -1,  1051,  1721,   152,   153,    -1,    13,    14,    15,
      16,    17,    -1,   147,    -1,   163,   164,   152,   153,  1068,
    1069,   156,    -1,    -1,    -1,    -1,    -1,    -1,   163,   164,
      -1,    -1,   166,    -1,  1752,   156,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1979,    -1,  1777,
    1778,    -1,    -1,    -1,   132,    -1,    72,    -1,    -1,   203,
      -1,     1,  1121,    -1,     4,    -1,    -1,    -1,    -1,  1797,
    1798,    -1,  1664,    -1,   152,   153,    -1,    -1,    -1,    -1,
      -1,   159,    -1,    -1,  1812,   163,   164,  1815,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
      -1,    -1,    -1,    -1,  1832,    -1,  1165,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1174,   132,  1845,    -1,    59,
      -1,    -1,   104,  1182,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,    -1,    -1,   152,   153,   282,    -1,
      -1,    -1,    -1,    -1,    84,    -1,  1205,   163,   164,    -1,
    1878,    72,    -1,  1212,    -1,    -1,    -1,    -1,   302,    -1,
      -1,    -1,    -1,    -1,   104,    -1,    -1,    -1,    -1,    -1,
      -1,   153,    -1,    -1,   156,  1234,  1235,    -1,    -1,    -1,
      -1,    -1,    -1,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,  1255,    -1,    -1,   343,
    1928,   141,   346,    -1,    -1,    -1,  1265,   147,    -1,  1937,
    1269,   132,    -1,    -1,    -1,    -1,   360,    -1,    -1,    -1,
     364,    -1,  1281,    -1,    -1,    -1,   166,    -1,    -1,    -1,
      -1,   152,   153,    -1,  1293,  1294,    -1,    -1,    -1,  1298,
    1832,    -1,   163,   164,  1972,    -1,    -1,    -1,    -1,    -1,
      -1,   191,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     104,    -1,   202,   203,   108,   109,   110,   111,   112,   113,
     114,   115,   116,    -1,    -1,    -1,   120,  2005,   122,    -1,
      -1,    -1,    -1,  1342,  1343,    -1,  2014,    -1,    -1,    -1,
      -1,   231,    -1,    -1,    -1,    -1,  2024,  1356,  1357,    -1,
      -1,    -1,  1361,  1362,    -1,    -1,    -1,    -1,    -1,   153,
      -1,   251,   156,    -1,    -1,    -1,   256,   257,   462,   259,
      -1,    -1,    -1,    -1,  2052,  2053,    -1,  2055,  2056,    -1,
      -1,    -1,    -1,    -1,    -1,  1394,  1395,  1396,    -1,    -1,
      -1,    -1,   282,    -1,  1403,    -1,   286,    -1,    -1,    -1,
      -1,   291,  1411,  1412,  1413,  1414,  1415,  1416,  1417,    -1,
      -1,    -1,   302,  2091,  1423,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   315,    72,    -1,    -1,    -1,
      -1,    -1,  1441,  1442,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2119,  2120,    -1,    -1,    -1,    13,    14,    15,    16,
      17,    -1,    -1,   343,    -1,    -1,   346,   551,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     360,    -1,    -1,  1482,   364,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  2162,    -1,   132,    -1,    -1,   583,
      -1,    -1,    -1,    -1,    -1,  1504,    -1,    -1,   592,    -1,
     594,    -1,    -1,    -1,    -1,    72,   152,   153,    -1,    -1,
      -1,   605,    -1,   607,  2056,    -1,    -1,   163,   164,    -1,
    1529,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   623,
     624,    -1,    13,    14,    15,    16,    17,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,    -1,
      -1,    -1,   104,   647,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,    -1,   132,   660,    -1,   458,    -1,
      -1,    -1,   462,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,   152,   153,    -1,    -1,    -1,
      -1,    72,    -1,  1602,  1603,  1604,   163,   164,    -1,    -1,
     152,   153,    -1,    -1,   156,    -1,    -1,    -1,    -1,    -1,
      -1,   163,   164,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1631,   104,   176,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,    -1,    -1,   104,  1648,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
      -1,   132,    -1,    -1,    -1,  1664,  1665,    -1,    -1,    -1,
      -1,   551,    -1,    -1,   758,    -1,    -1,    -1,    -1,    -1,
      -1,   152,   153,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   163,   164,    -1,    -1,    -1,    -1,    -1,   155,
      -1,  1700,    -1,   583,    -1,    -1,    -1,    -1,   588,    -1,
      -1,    -1,   592,    -1,   594,   799,    -1,    -1,    -1,  1718,
      -1,    -1,  1721,    -1,    -1,   605,    -1,   607,    -1,    -1,
      -1,    -1,    -1,  1732,    -1,    -1,    -1,   821,   822,    -1,
      -1,    -1,    -1,   623,   624,    -1,     1,    -1,    -1,    -1,
      -1,    -1,    -1,  1752,    -1,    -1,   840,    -1,   638,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   647,  1767,    -1,
      -1,    -1,   652,  1772,  1773,    -1,    -1,    -1,  1777,  1778,
     660,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   104,   876,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   104,    59,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,    -1,    -1,    -1,   132,
      -1,    -1,    -1,    -1,    -1,   909,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1832,   918,    -1,    -1,    -1,    -1,   152,
     153,    -1,    -1,   156,    -1,    -1,  1845,    -1,    -1,   104,
     163,   164,   936,    -1,   155,   939,    -1,   941,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   758,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,
      -1,    -1,   147,    -1,    -1,   775,    -1,    -1,    -1,    -1,
      56,    57,    -1,    -1,   988,    -1,   152,   153,    -1,    -1,
      -1,   166,    -1,    -1,    -1,    -1,    -1,   163,   164,   799,
      -1,    -1,    -1,   104,   804,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,    92,    -1,    -1,    -1,
      -1,   821,   822,    -1,    -1,    -1,    -1,    -1,   203,    -1,
      -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     840,    -1,    -1,    -1,    -1,    -1,    -1,  1051,    -1,    -1,
      -1,   152,   153,  1972,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   163,   164,  1068,  1069,   142,    -1,    -1,   145,
      -1,    -1,    -1,    -1,    -1,    -1,   876,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   160,    -1,  2005,    -1,    -1,    -1,
      -1,    -1,     1,    -1,    -1,     4,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2024,    -1,    -1,    -1,   909,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1121,   918,    -1,
      -1,    -1,    -1,    -1,   924,    -1,    -1,   302,    -1,    -1,
      -1,    -1,    -1,  2052,  2053,    -1,   936,  2056,    -1,   939,
      -1,   941,    -1,    -1,    -1,   221,   946,    -1,    -1,    -1,
      59,    -1,   102,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,    -1,    -1,   343,    -1,
      -1,   346,  2091,    -1,    -1,    84,    -1,    -1,  1182,    -1,
      -1,    -1,    -1,    -1,    -1,   360,    -1,    -1,   988,   364,
      -1,    -1,    -1,   269,   270,   104,    -1,    -1,    -1,    -1,
      -1,    -1,   152,   112,   280,   155,   156,   104,  1212,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   295,
    2139,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1235,   141,    -1,    -1,    -1,    -1,    -1,   147,    -1,
      -1,   150,    -1,    -1,    -1,    -1,    -1,    -1,   324,    -1,
      -1,  1051,    -1,    -1,    -1,   331,   332,    -1,    -1,    -1,
     336,  1265,    -1,    -1,    -1,    -1,    -1,    -1,  1068,  1069,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   190,    -1,    -1,    -1,    -1,    -1,   462,    -1,  1293,
    1294,    -1,    -1,   202,    -1,    -1,    -1,    -1,   374,    -1,
      -1,   377,    -1,   104,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,    -1,    -1,    -1,    -1,
     104,  1121,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,    -1,    -1,    -1,    -1,    -1,  1342,  1343,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   257,    -1,
     259,   152,  1356,  1357,    -1,   264,    -1,  1361,  1362,    -1,
      -1,    -1,    -1,    -1,    -1,  1165,    -1,    -1,   152,    -1,
      -1,    -1,    -1,    -1,  1174,    -1,   551,    -1,    -1,    -1,
      -1,    -1,  1182,    -1,    -1,    -1,    -1,    -1,    -1,   465,
    1394,  1395,  1396,   302,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   477,   478,    -1,    -1,    -1,    -1,    -1,   583,    -1,
      -1,    -1,  1212,    -1,   323,    -1,    -1,   592,    -1,   594,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     605,    -1,   607,    -1,    -1,  1235,    -1,  1441,  1442,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   623,   624,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1265,    -1,    -1,    -1,    -1,
      -1,    -1,   647,    -1,  1274,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   660,    -1,    -1,    -1,   398,
      -1,    -1,    -1,  1293,  1294,    -1,    -1,    -1,    -1,    -1,
    1504,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     1,    -1,    -1,     4,    -1,    -1,
      -1,    -1,    -1,   432,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   617,  1342,  1343,    -1,    -1,    -1,    -1,    -1,   458,
      -1,    -1,    -1,    -1,    -1,    -1,  1356,  1357,    -1,    -1,
      -1,  1361,  1362,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    59,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   758,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   667,    -1,    -1,  1394,  1395,  1396,    84,  1602,  1603,
    1604,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1411,  1412,  1413,  1414,  1415,    -1,   104,    -1,    -1,
      -1,    -1,    -1,    -1,   799,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   545,   546,    -1,    -1,
      -1,  1441,  1442,    -1,  1648,    -1,   821,   822,    -1,    -1,
      -1,    -1,    -1,    -1,   141,    -1,    -1,    -1,    -1,    -1,
     147,  1665,    -1,   150,    -1,   840,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   588,
      -1,    -1,    -1,   592,    -1,   594,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1700,    -1,   607,    -1,
      -1,   876,    -1,   190,  1504,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1718,   202,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1732,  1529,
      -1,    -1,    -1,    -1,   909,    -1,    -1,    -1,    -1,    -1,
     816,    -1,    -1,   918,    -1,    -1,    -1,   823,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   936,   671,  1767,   939,    -1,   941,    -1,  1772,  1773,
     257,    -1,   259,  1777,  1778,   684,    -1,   264,   687,   688,
       1,   690,    -1,     4,    -1,    -1,    -1,    -1,    -1,    -1,
     699,    -1,    -1,   702,   703,   704,    -1,    -1,    -1,    -1,
      -1,    -1,  1602,  1603,  1604,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   988,    -1,   302,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   908,    -1,    -1,    -1,   323,  1637,    59,    -1,
      -1,  1845,    -1,    -1,    -1,    -1,    -1,    -1,  1648,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    84,    -1,  1665,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   782,    -1,    -1,  1051,    -1,    -1,    -1,
      -1,    -1,    -1,   104,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1068,  1069,    -1,    -1,    -1,    -1,    -1,
    1700,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   398,    -1,    -1,    -1,    -1,    -1,    -1,  1718,    -1,
     141,  1721,    -1,    -1,    -1,    -1,   147,    -1,    -1,   150,
      -1,    -1,  1732,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   432,  1121,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1767,  1972,   190,
      -1,   458,  1772,  1773,    -1,    -1,    -1,  1777,  1778,    -1,
      -1,   202,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1798,    -1,
     909,  2005,    75,    -1,    -1,    -1,    -1,  1182,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   924,   925,    -1,    -1,    -1,
    2024,    -1,    -1,    -1,    97,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   257,  1212,   259,    -1,
      -1,    -1,    -1,   264,    -1,  1845,    -1,    -1,  2052,  2053,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   545,   546,
    1235,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     153,   302,    -1,   182,    -1,    -1,    -1,  2091,    -1,    -1,
    1265,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   588,   323,    -1,    -1,   592,    -1,   594,    -1,    -1,
      -1,  1187,  1188,  1189,    -1,    -1,    -1,    -1,  1293,  1294,
     607,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2139,    -1,    -1,    -1,    -1,
      -1,    -1,  1218,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   230,   231,    -1,
      -1,    -1,  1238,    -1,    -1,    -1,    -1,  1342,  1343,    -1,
      -1,    -1,  1972,    -1,    -1,    -1,    -1,   398,   251,    -1,
      -1,  1356,  1357,    -1,   671,    -1,  1361,  1362,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   684,    -1,    -1,
     687,   688,    -1,   690,    -1,  2005,  1282,    -1,    -1,    -1,
      -1,   432,   699,    -1,    -1,   702,   703,   704,    -1,  1394,
    1395,  1396,    -1,    -1,  2024,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   458,    88,    -1,
      -1,   314,   315,    -1,    94,    95,    -1,    -1,    -1,    -1,
      -1,    -1,  2052,  2053,    -1,    -1,  1165,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1174,  1441,  1442,    -1,    -1,
      -1,    -1,    -1,  1182,    -1,    -1,    -1,    -1,   128,    -1,
      -1,    -1,    84,   382,    -1,    -1,    -1,   386,   387,    -1,
      -1,  2091,    -1,    -1,    -1,   782,  1205,   396,   397,    -1,
      -1,    -1,    -1,  1212,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   411,   412,    -1,    -1,    -1,    -1,    -1,    -1,
    2120,    -1,    -1,    -1,   545,   546,  1235,    -1,    -1,  1504,
      -1,    -1,    -1,   432,    -1,    -1,    -1,    -1,    -1,  2139,
      -1,    -1,    -1,    -1,    -1,    -1,  1255,    -1,   150,    -1,
    1426,    -1,    -1,  1429,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   462,   166,    -1,    -1,   588,    -1,    -1,
      -1,   592,    -1,   594,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   607,    -1,   190,  1298,
      -1,    -1,    -1,   466,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   203,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   909,    -1,    -1,    -1,    -1,  1602,  1603,  1604,
      -1,    -1,    -1,   506,    -1,    -1,    -1,   924,   925,    -1,
      -1,    -1,    -1,    -1,    -1,   518,    -1,    -1,    -1,    -1,
     671,    -1,    -1,    -1,    -1,    -1,    -1,   259,    -1,    -1,
      -1,   311,    -1,   684,    -1,    -1,   687,   688,    -1,   690,
     543,    -1,    -1,  1648,    -1,    -1,   549,    -1,   699,    -1,
      -1,   702,   703,   704,    -1,    -1,    -1,    -1,    -1,    -1,
    1665,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1411,  1412,  1413,  1414,  1415,  1416,  1417,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   590,    -1,    -1,
      -1,   323,    -1,    -1,    -1,  1700,    -1,    -1,   601,    -1,
     603,    -1,  1441,  1442,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   343,    -1,  1718,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1732,    -1,    -1,
      -1,   782,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1645,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1767,    -1,    -1,    -1,    -1,  1772,  1773,    -1,
      -1,    -1,  1777,  1778,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   461,    -1,   463,    -1,    -1,    -1,    -1,    -1,    -1,
    1529,    -1,   472,   473,    -1,    -1,    -1,    -1,    -1,    -1,
     432,    -1,    -1,    -1,    -1,    -1,   735,   736,   737,   738,
     739,   740,   741,   742,   743,   744,   745,   746,   747,   748,
     749,   750,   751,   752,   753,    -1,    -1,    -1,    -1,    -1,
     462,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1845,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1165,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1174,   909,    -1,
      -1,    -1,    -1,    -1,    -1,  1182,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   924,   925,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   815,    -1,  1205,    -1,
      -1,    -1,  1631,    -1,    -1,  1212,    -1,    -1,    -1,   802,
      -1,   804,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   589,
      -1,    -1,    -1,   545,   546,    -1,    -1,    -1,  1235,   551,
      -1,    -1,    -1,    -1,    -1,    -1,  1665,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1255,   842,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1700,    -1,    -1,    -1,   868,    -1,  1972,    -1,    -1,
      -1,  1877,    -1,   605,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1298,  1721,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   624,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    2005,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1752,    -1,   647,    -1,    -1,    -1,  2024,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   671,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2052,  2053,    -1,
      -1,    -1,    -1,    -1,    -1,   984,    -1,    -1,    -1,    -1,
     989,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1000,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  2091,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1411,  1412,  1413,  1414,  1415,  1416,
    1417,    -1,    -1,    -1,    -1,    -1,  1845,    -1,    -1,    -1,
      -1,    -1,    -1,  1042,  1165,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1174,  1441,  1442,   758,    -1,    -1,    -1,
      -1,  1182,    -1,    -1,  2139,   815,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     782,    -1,    -1,    -1,  1205,    -1,    -1,    -1,    -1,    -1,
      -1,  1212,    -1,    -1,    -1,    -1,    -1,   799,    -1,  1072,
      -1,    -1,    -1,    -1,    -1,  1078,  1079,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1235,    -1,    -1,    -1,     0,   821,
     822,     3,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1255,    -1,    -1,    -1,   840,    -1,
      -1,    -1,  1529,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   902,   903,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   915,   916,   917,    -1,    -1,
     920,    -1,    -1,    -1,    -1,    -1,    -1,  1298,    -1,    -1,
      -1,    -1,    -1,  1182,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    78,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2024,   918,    -1,    -1,    -1,
      -1,    -1,    -1,   925,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2052,  1631,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1002,    -1,    -1,   137,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1265,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1249,    -1,  1665,    -1,
      -1,    -1,  2091,    -1,    -1,  1284,   988,    -1,    -1,    -1,
    1411,  1412,  1413,  1414,  1415,  1416,  1417,    -1,  1048,    -1,
      -1,    -1,  1301,  1302,  1303,    -1,    -1,    -1,    -1,  1308,
    1309,    -1,    -1,  1700,    -1,    -1,    -1,    -1,    -1,    -1,
    1441,  1442,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1332,  1721,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1095,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1104,  1105,  1106,  1107,   240,    -1,
      -1,    -1,  1112,  1113,    -1,  1752,    -1,    -1,    -1,    -1,
    1369,  1370,  1122,    -1,    -1,    -1,   258,    -1,  1351,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   268,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   279,    -1,    -1,
      -1,  1151,    -1,    -1,  1154,    -1,  1156,    -1,  1529,    -1,
      -1,    -1,    -1,    -1,    -1,   297,   298,    -1,    -1,  1121,
      -1,    -1,   304,   305,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     322,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1422,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1845,    -1,
     342,    -1,  1212,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1243,    -1,    -1,    -1,    -1,    -1,    -1,
    1250,    -1,  1252,  1253,    -1,    -1,    -1,    -1,    -1,    -1,
    1631,    -1,    -1,    -1,  1264,    -1,  1266,   399,  1268,    -1,
    1270,    -1,    -1,    -1,    -1,  1275,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1665,    -1,    -1,    -1,   430,    -1,
      -1,    -1,    -1,  1255,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1265,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   456,    -1,    -1,    -1,   460,  1700,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1337,    -1,    -1,
      -1,    -1,  1294,    -1,  1344,  1345,    -1,   479,    -1,    -1,
    1721,   483,   484,    -1,    -1,   487,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1368,    -1,
     502,   503,   504,   505,    -1,  1375,    -1,    -1,    -1,  1379,
      -1,  1752,    -1,    -1,    -1,    -1,    -1,  2024,    -1,    -1,
      -1,    -1,    -1,   525,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1624,   534,    -1,    -1,    -1,  1629,  1407,    -1,  1361,
    1362,    -1,  1635,    -1,  1637,  2052,    -1,    -1,    -1,    -1,
      -1,  1421,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    48,    -1,    -1,   566,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  2091,    -1,  1705,  1706,    75,    -1,
      -1,    -1,    -1,  1463,    -1,   597,    -1,    -1,    -1,    -1,
      -1,    -1,   604,    -1,  1845,    -1,    -1,    -1,   610,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   634,   635,   121,    -1,    -1,    -1,  1508,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1516,   134,  1518,   136,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   168,  1504,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1567,  1568,    -1,
      -1,    -1,    -1,   705,    -1,   192,    -1,    -1,    -1,    -1,
      -1,    -1,  1582,  1583,    -1,  1585,    -1,    -1,    -1,  1812,
      -1,    -1,  1815,    -1,  1594,    -1,    -1,    -1,    -1,    -1,
     398,    -1,    -1,    -1,    -1,  1605,  1606,  1607,    -1,    -1,
      -1,    -1,    -1,   230,    -1,    -1,    -1,   234,    -1,    -1,
     237,   238,    -1,    -1,   241,    -1,    -1,   244,   245,    -1,
     247,    -1,   249,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   774,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2024,    -1,  1878,    -1,   789,    -1,    -1,
      -1,   793,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     802,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1631,
      -1,  2052,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1939,    -1,   824,    -1,    -1,    -1,    -1,   314,    -1,    -1,
     317,   833,    -1,    -1,    -1,    -1,    -1,   839,    -1,    -1,
      -1,    -1,    -1,    -1,  1937,    -1,    -1,    -1,    -1,    -1,
    2091,    -1,    -1,   340,   341,    -1,    -1,    -1,    -1,    -1,
    1979,    -1,    -1,  1733,  1734,    -1,    -1,    -1,    -1,   356,
      -1,    -1,    -1,    -1,    -1,    -1,  1746,   545,   546,    -1,
      -1,    -1,    -1,   885,    -1,    -1,    -1,   889,    -1,    -1,
      -1,   893,    -1,    -1,    -1,    -1,  1718,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1783,  1784,  1785,    -1,    -1,    -1,    -1,
     922,    -1,    -1,    -1,   166,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1777,  1778,    -1,    -1,    -1,
     202,   203,  2055,    -1,   451,    -1,  2085,    -1,  2087,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   978,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   234,    -1,    -1,    -1,    -1,    -1,    -1,   241,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2126,    -1,    -1,
      -1,    -1,  1882,    -1,    -1,    -1,   684,    -1,    -1,   506,
      -1,    -1,   690,    -1,    -1,    -1,  2119,  2120,    -1,    -1,
      -1,   699,    -1,  1903,    -1,   522,  1906,  1907,    -1,    -1,
      -1,  2160,    -1,  1913,    -1,    -1,    -1,    -1,    -1,    -1,
     718,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1061,
      -1,    -1,    -1,  1065,    -1,    -1,    -1,    -1,    -1,  2162,
    1072,    -1,    -1,    -1,    -1,   317,    -1,    -1,    -1,    -1,
    1082,    -1,    -1,    -1,    -1,    -1,   754,  1089,    -1,  2208,
      -1,    -1,    -1,    -1,    -1,    -1,  1098,    -1,  1100,    -1,
      -1,   343,   344,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   601,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   364,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1135,    -1,    -1,    -1,  1139,    -1,    -1,
      -1,  1143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   642,   643,    -1,    -1,  1161,
      -1,    -1,    -1,    -1,  1166,    -1,    -1,    -1,   655,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2005,  2054,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   435,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   451,
     452,    -1,   454,   455,    -1,    -1,    -1,    -1,    -1,    -1,
     462,    -1,    -1,    -1,   466,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2117,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1263,    -1,    -1,    -1,   507,    -1,    -1,    -1,   511,
      -1,    -1,    -1,  2143,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1288,    -1,  2158,    -1,
      -1,   778,   779,    -1,    -1,    -1,    -1,   784,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2175,    -1,    -1,    -1,   551,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   805,    -1,
      -1,   808,   809,    -1,   811,    -1,   813,   814,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,    -1,
     602,    -1,    -1,   605,    -1,    -1,    -1,    -1,   855,    -1,
      -1,    -1,   859,    -1,    -1,    -1,   863,    -1,    -1,    -1,
    1382,   623,   624,    -1,  1386,    -1,    -1,    -1,  1390,    -1,
      -1,    -1,   634,    -1,    -1,    -1,   638,    -1,    -1,    -1,
      -1,    -1,    -1,   645,    -1,   647,    49,    -1,    -1,    52,
      -1,    54,    -1,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1428,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   933,   934,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,
     947,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,   120,   121,   122,
      -1,   124,   125,    -1,    -1,    -1,    -1,  1489,    -1,   132,
    1492,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1510,   152,
      -1,    -1,   155,   156,    -1,    -1,   758,    -1,    -1,   162,
     163,   164,   165,   166,   167,   168,    -1,    -1,    -1,    -1,
      -1,    -1,   774,   775,    -1,    -1,    -1,  1205,    -1,    -1,
      -1,    -1,   784,   785,    -1,   787,   788,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   799,    -1,    -1,
     802,  1563,   804,   805,    -1,    -1,    -1,    -1,    -1,   811,
    1572,    -1,    -1,    -1,  1576,    -1,    -1,    -1,    -1,   821,
     822,    -1,    -1,    84,    -1,    -1,    -1,    -1,  1590,  1591,
      -1,  1078,    -1,    -1,    -1,    -1,    -1,    -1,   840,   100,
      -1,    -1,   844,    -1,    -1,    -1,   848,    -1,    -1,    -1,
      -1,    -1,    -1,   855,   856,    -1,  1618,   859,   860,    -1,
      -1,   863,   864,    -1,    -1,    -1,    -1,    -1,    -1,   871,
      -1,    -1,  1119,    -1,    -1,    -1,    -1,    -1,    -1,  1126,
      -1,    -1,  1129,    -1,    -1,    -1,  1133,    -1,    -1,   150,
      -1,    -1,    -1,   154,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   166,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   918,   919,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   190,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   203,    -1,    -1,   206,    -1,   949,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1722,  1723,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   988,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    48,    -1,    -1,   259,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1438,    -1,   273,  1441,  1442,    -1,    -1,    -1,    -1,  1447,
      -1,    -1,    -1,  1451,  1271,  1453,    -1,    -1,    -1,    -1,
      -1,    -1,  1279,  1280,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   313,  1055,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   323,    -1,    -1,    -1,    -1,    -1,   121,    -1,
    1072,  1073,    -1,    -1,    -1,    -1,    -1,  1079,    -1,    -1,
      -1,   134,   343,   136,   345,    -1,    -1,    -1,    -1,    -1,
      -1,  1338,    -1,    -1,    -1,    -1,    -1,  1859,    -1,    -1,
    1347,    -1,    -1,  1350,    -1,  1352,  1353,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1121,
      -1,    -1,    -1,    -1,  1126,  1127,    -1,  1129,  1130,    -1,
    1892,  1133,  1134,    -1,    -1,    -1,    -1,   398,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1911,
      -1,  1398,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   432,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1941,
      -1,    -1,  1610,    -1,   237,   238,    -1,    -1,   241,    -1,
      -1,   244,   245,    -1,   247,    -1,   249,    -1,    -1,    -1,
      -1,   462,    -1,    -1,    -1,    -1,  1968,   468,    -1,  1971,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1660,    -1,    -1,    -1,  1483,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1675,  1676,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1265,    -1,    -1,    -1,    -1,    -1,  1271,
    1272,    -1,  1700,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   545,   546,    -1,   340,   341,    -1,
     551,    -1,  1294,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   356,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2080,    -1,
      -1,    -1,    -1,    -1,  1571,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1338,  1339,    -1,    -1,
      -1,    -1,    -1,    -1,   605,  1347,  1348,    -1,  1350,    -1,
      -1,    -1,    -1,  1600,    -1,    -1,    -1,    -1,    -1,  1361,
    1362,    -1,    -1,   624,    -1,   626,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   647,    -1,  1635,    -1,
      -1,    -1,    49,    -1,  1641,    52,    -1,    54,   451,    56,
    1828,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1837,
     671,  1839,    -1,    -1,  1842,  1843,    73,  1845,    -1,    -1,
      -1,    -1,  1850,   684,    -1,    -1,   687,   688,    -1,   690,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   699,    -1,
      -1,   702,   703,   704,   101,   102,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,    -1,   120,   121,   122,    -1,   124,   125,   522,
    1717,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   146,
     147,   148,    -1,    -1,    -1,   152,   153,   758,   155,   156,
      -1,    -1,  1504,    -1,    -1,   162,   163,   164,   165,   166,
     167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   190,    -1,
      -1,   782,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1531,
    1958,   203,    -1,    -1,    -1,  1963,  1964,    -1,   799,    -1,
      -1,    -1,    -1,    -1,   216,    -1,   218,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1983,    -1,  1804,  1805,    -1,
     821,   822,    -1,    -1,    -1,  1812,    -1,    -1,    -1,  1816,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   840,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   642,
     643,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   655,    -1,  2032,    -1,  2034,    -1,    -1,  2037,
    2038,    -1,    -1,    -1,  2042,  2043,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1625,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1641,
     312,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   918,    -1,    -1,
      -1,    -1,    -1,    -1,   925,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   938,    -1,    -1,
      -1,    -1,    -1,  1930,  2112,  2113,  2114,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1718,    -1,    -1,    -1,
    2148,  2149,  2150,    -1,    -1,   778,   779,   988,    -1,    -1,
      -1,   784,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   805,    -1,    -1,   808,   809,    -1,   811,    -1,
     813,   814,    -1,  2010,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1777,  1778,    -1,    -1,    -1,
      -1,    -1,    -1,    49,    -1,    -1,    52,    -1,    54,    -1,
      56,    -1,    -1,    -1,    -1,  1797,  1798,    -1,    -1,    -1,
      -1,    -1,   855,    -1,    -1,    -1,   859,    73,    -1,    -1,
     863,  1813,    -1,    -1,    -1,    -1,   488,    -1,    -1,    -1,
      -1,    -1,   494,    -1,    -1,    -1,    -1,   499,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   101,   102,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,   120,   121,   122,    -1,   124,   125,
    1121,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,
      -1,    -1,  2119,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     933,   934,    -1,    -1,    -1,    -1,   152,   153,    -1,   155,
     156,    -1,    -1,    -1,   947,    -1,   162,   163,   164,   165,
     166,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1174,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   595,    -1,    -1,  1928,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1936,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1205,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   624,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   637,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1234,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1255,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2005,  1265,    -1,    -1,    -1,  2010,  2011,
      -1,    -1,  2014,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     692,    -1,    -1,    -1,    -1,  1078,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1294,    -1,    -1,    -1,  1298,    -1,    -1,
      -1,    -1,    -1,    -1,   716,   717,    -1,    -1,   720,    -1,
     722,    -1,    -1,  2055,    -1,    -1,   728,    -1,   730,   731,
      -1,    -1,    -1,    -1,    -1,    -1,  1119,    -1,    -1,    -1,
      -1,    -1,    -1,  1126,    -1,    -1,  1129,    -1,    -1,    -1,
    1133,    -1,    -1,    -1,    -1,    -1,   758,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   771,
    1361,  1362,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     782,    -1,    -1,    -1,    -1,    -1,    -1,  2119,  2120,    -1,
      -1,    -1,    -1,    -1,   796,    -1,    -1,   799,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1411,  1412,  1413,    -1,   826,  1416,  1417,   829,    -1,    -1,
    2162,    -1,  1423,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
     872,    51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,
      60,    61,    62,    63,    64,    -1,    -1,    -1,  1271,    -1,
      -1,  1482,    72,    -1,    -1,    -1,  1279,  1280,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1504,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   925,   104,    -1,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   938,   939,    -1,    -1,
      -1,    -1,    -1,    -1,   946,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   132,    -1,    -1,  1338,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1347,    -1,    -1,  1350,    -1,  1352,
    1353,     1,   974,    -1,    -1,   155,   156,    -1,    -1,    -1,
      -1,    -1,    -1,   163,   164,    -1,   988,    -1,    18,    -1,
      -1,    -1,    -1,    -1,   996,    -1,    -1,    -1,    -1,    -1,
      -1,  1003,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1398,    -1,    -1,    -1,    49,
      -1,    -1,    52,    -1,    54,    -1,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1631,    71,    -1,    73,    74,    -1,    76,  1049,    -1,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    92,    93,    94,    95,    96,    97,    -1,    99,
      -1,   101,   102,  1664,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,    -1,   124,   125,    -1,    -1,    -1,    -1,
    1483,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1116,    -1,  1118,    -1,  1120,    -1,
      -1,    -1,   152,    -1,    -1,   155,   156,  1718,    -1,    -1,
     160,    -1,   162,   163,   164,   165,   166,   167,   168,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   177,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1752,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1777,  1778,  1571,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1199,  1200,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1600,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     5,    -1,    -1,    -1,
      -1,  1832,    -1,    -1,    13,    14,    15,    16,    17,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1269,    -1,    -1,
      -1,    -1,    -1,  1275,    -1,    -1,    -1,    -1,    -1,    -1,
      49,    -1,    -1,    52,    -1,    54,    -1,    56,    -1,    -1,
      -1,    -1,  1294,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    73,    -1,    -1,    -1,    -1,  1311,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1326,    -1,    -1,  1329,    -1,    -1,
      -1,    -1,   101,   102,  1717,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
      -1,   120,   121,   122,    -1,   124,   125,    -1,    -1,    -1,
      -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1381,
      -1,  1972,    -1,   152,    -1,    -1,   155,   156,    -1,    -1,
      -1,    -1,    -1,   162,   163,   164,   165,   166,   167,   168,
    1402,  1403,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  2005,    -1,    -1,    -1,    -1,    -1,
      -1,  1804,  1805,    -1,    -1,    -1,    -1,    -1,    -1,  1431,
      -1,    -1,    -1,  1816,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1449,    -1,    -1,
    1452,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2053,    -1,    -1,  2056,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    18,    -1,
      -1,    -1,  1504,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1514,  1515,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1524,    -1,    -1,    -1,    -1,    -1,    -1,    49,
      -1,    -1,    52,    -1,    54,    -1,    56,    -1,    -1,    -1,
    1542,    -1,  1544,    -1,    -1,    -1,    -1,  1930,    -1,    -1,
      -1,    71,    -1,    73,    74,    -1,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    92,    93,    94,    95,    96,    97,    -1,    99,
      -1,   101,   102,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,   122,    -1,   124,   125,    -1,    -1,    -1,    -1,
      -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2010,    -1,  1631,
      -1,   151,   152,    -1,  1636,   155,   156,    -1,    -1,    -1,
     160,    -1,   162,   163,   164,   165,   166,   167,   168,     1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   177,    13,    14,
      15,    16,    17,    -1,    -1,    20,    18,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,  1695,    -1,    -1,    51,    49,    53,    -1,
      52,    -1,    54,    -1,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    71,
      -1,    73,    74,    -1,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    91,
      92,    93,    94,    95,    96,    97,    -1,    99,  1750,   101,
     102,  1753,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,    -1,   124,   125,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1790,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,
     152,    -1,    -1,   155,   156,    -1,    -1,    -1,   160,    -1,
     162,   163,   164,   165,   166,   167,   168,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   177,    -1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    49,    -1,    51,    52,    53,    54,    -1,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    65,
      -1,    -1,    -1,    69,    -1,    71,    72,    73,    74,    -1,
      76,    -1,    -1,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    91,    92,    93,    94,    95,
      96,    97,    -1,    99,    -1,   101,   102,   103,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,   122,    -1,   124,   125,
      -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   151,   152,    -1,  1980,   155,
     156,    -1,    -1,    -1,   160,    -1,   162,   163,   164,   165,
     166,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   177,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    49,    -1,
      51,    52,    53,    54,    -1,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    -1,    -1,    -1,    69,    -1,
      71,    72,    73,    74,    -1,    76,    -1,    -1,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    90,
      91,    92,    93,    94,    95,    96,    97,    -1,    99,    -1,
     101,   102,   103,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,    -1,   124,   125,    -1,    -1,    -1,    -1,    -1,
      -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   152,    -1,    -1,   155,   156,    -1,    -1,    -1,   160,
      -1,   162,   163,   164,   165,   166,   167,   168,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   177,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    49,    -1,    51,    52,    53,    54,    -1,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    65,
      -1,    -1,    -1,    69,    -1,    -1,    72,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   101,   102,   103,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,   120,   121,   122,    -1,   124,   125,
      -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     146,   147,   148,    -1,    -1,    -1,   152,   153,   154,   155,
     156,    -1,    -1,    -1,    -1,    -1,   162,   163,   164,   165,
     166,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   177,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    49,    -1,
      51,    52,    53,    54,    -1,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    -1,    -1,    -1,    69,    -1,
      -1,    72,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     101,   102,   103,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,    -1,   120,
     121,   122,    -1,   124,   125,    -1,    -1,    -1,    -1,    -1,
      -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   146,   147,   148,    -1,    -1,
      -1,   152,   153,    -1,   155,   156,    -1,    -1,    -1,    -1,
      -1,   162,   163,   164,   165,   166,   167,   168,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   177,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    -1,    -1,    -1,    -1,
      -1,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    -1,    50,    51,    -1,    53,    -1,    55,    -1,
      -1,    58,    59,    60,    61,    62,    63,    64,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    49,
      -1,    51,    52,    53,    54,    -1,    56,    57,    58,    59,
      60,    61,    62,    63,    64,    65,    -1,    -1,    -1,    69,
      -1,    -1,    72,    73,    -1,    -1,    -1,   154,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     177,   101,   102,   103,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
     120,   121,   122,    -1,   124,   125,    -1,    -1,    -1,    -1,
      -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   152,    -1,   154,   155,   156,    -1,    -1,    -1,
      -1,    -1,   162,   163,   164,   165,   166,   167,   168,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    49,    -1,    51,    52,    53,
      54,    -1,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    -1,    -1,    -1,    69,    -1,    -1,    72,    73,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,   103,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,    -1,   120,   121,   122,    -1,
     124,   125,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,    -1,
      -1,   155,   156,    -1,    -1,    -1,    -1,    -1,   162,   163,
     164,   165,   166,   167,   168,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      49,    -1,    51,    52,    53,    54,    -1,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    73,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   101,   102,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
      -1,   120,   121,   122,    -1,   124,   125,    -1,    -1,    -1,
      -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   152,    -1,    -1,   155,   156,    -1,    -1,
      -1,    -1,    -1,   162,   163,   164,   165,   166,   167,   168,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    49,    -1,    51,    52,    53,
      54,    -1,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    73,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,    -1,   120,   121,   122,    -1,
     124,   125,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,    -1,
      -1,   155,   156,    -1,    -1,    -1,    -1,    -1,   162,   163,
     164,   165,   166,   167,   168,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      49,    -1,    51,    52,    53,    54,    -1,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    73,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   101,   102,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
      -1,   120,   121,   122,    -1,   124,   125,    -1,    -1,    -1,
      -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   152,    -1,    -1,   155,   156,    -1,    -1,
      -1,    -1,    -1,   162,   163,   164,   165,   166,   167,   168,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    49,    -1,    51,    52,    53,
      54,    -1,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    73,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,    -1,   120,   121,   122,    -1,
     124,   125,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,    -1,
      -1,   155,   156,    -1,    -1,    -1,    -1,    -1,   162,   163,
     164,   165,   166,   167,   168,     1,    -1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    -1,    50,    51,    -1,    53,    -1,    55,
      -1,    57,    58,    59,    60,    61,    62,    63,    64,    65,
      -1,    -1,    -1,    69,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    77,    78,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,
     106,   107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   151,    -1,    -1,    -1,   155,
     156,    -1,    -1,    -1,    -1,    -1,    -1,   163,   164,     1,
      -1,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    -1,    50,    51,
      -1,    53,    -1,    55,    -1,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    -1,    -1,    -1,    69,    -1,     5,
      72,    -1,    -1,    -1,    -1,    77,    78,    13,    14,    15,
      16,    17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   103,    -1,    -1,   106,   107,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    49,    -1,    -1,    52,    -1,    54,    -1,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    72,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,
      -1,    -1,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,
      -1,   163,   164,    -1,    -1,   101,   102,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,   120,   121,   122,    -1,   124,   125,
      -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   152,    -1,    -1,   155,
     156,    -1,    -1,    -1,    -1,    -1,   162,   163,   164,   165,
     166,   167,   168,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,
      60,    61,    62,    63,    64,    65,    -1,    -1,    -1,    69,
      -1,    -1,    72,    73,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    99,
      -1,    -1,    -1,   103,   104,    -1,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,    -1,    -1,    -1,   119,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   152,   153,    -1,   155,   156,    -1,    -1,    -1,
      -1,    -1,    -1,   163,   164,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      -1,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    -1,    50,    51,    -1,    53,    -1,    55,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   104,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   152,   153,    -1,   155,   156,    -1,
      -1,    -1,   160,    -1,    -1,   163,   164,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    -1,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    -1,    50,    51,    -1,    53,    -1,    55,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   152,   153,    -1,   155,
     156,    -1,    -1,    -1,    -1,    -1,    -1,   163,   164,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,    -1,    -1,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    -1,    -1,    -1,    69,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    77,    78,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,
      -1,    -1,   106,   107,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,    -1,    -1,
      -1,   155,   156,    -1,    -1,    -1,    -1,    -1,    -1,   163,
     164,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,
      -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    -1,    -1,    -1,    69,    -1,    -1,
      72,    -1,    -1,    -1,    -1,    77,    78,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   103,    -1,    -1,   106,   107,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,
      -1,    -1,    -1,   155,   156,    -1,     3,    -1,     5,    -1,
      -1,   163,   164,    10,    -1,    -1,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,
      57,    58,    59,    60,    61,    62,    63,    64,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    73,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,   106,
     107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   151,    -1,    -1,    -1,   155,   156,
      -1,     3,    -1,     5,    -1,    -1,   163,   164,    10,    -1,
      -1,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,
      -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,    61,
      62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   103,    -1,    -1,   106,   107,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,
      -1,    -1,    -1,   155,   156,    -1,     3,    -1,     5,    -1,
      -1,   163,   164,    10,    -1,    -1,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,
      57,    58,    59,    60,    61,    62,    63,    64,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    73,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,   106,
     107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   151,    -1,    -1,    -1,   155,   156,
      -1,     3,    -1,     5,    -1,    -1,   163,   164,    10,    -1,
      -1,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,
      -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,    61,
      62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   103,    -1,    -1,   106,   107,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,
      -1,    -1,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,
      -1,   163,   164,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    -1,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    -1,
      50,    51,    -1,    53,    -1,    55,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   106,   107,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,    -1,
      -1,    -1,    -1,    -1,    -1,   155,   156,    -1,    13,    14,
      15,    16,    17,   163,   164,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    -1,    50,    51,    -1,    53,    -1,
      55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   106,   107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     155,   156,    -1,    -1,    -1,    -1,    -1,    -1,   163,   164,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    -1,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    -1,    50,    51,    -1,    53,
      -1,    55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     104,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,   153,
      -1,   155,   156,    -1,    -1,    -1,   160,    -1,    -1,   163,
     164,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    -1,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    -1,    50,    51,    -1,
      53,    -1,    55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   104,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,
     153,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,    -1,
     163,   164,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,
      -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,    61,
      62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   106,   107,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   154,   155,   156,    -1,    -1,    -1,    -1,    -1,
      -1,   163,   164,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,
      61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   106,   107,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   154,   155,   156,    -1,    -1,    -1,    -1,
      -1,    -1,   163,   164,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,
      60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   106,   107,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   132,    -1,   134,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   155,   156,    -1,    -1,    -1,
      -1,    -1,    -1,   163,   164,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    57,    58,
      59,    60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,   107,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   132,    -1,   134,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   155,   156,    -1,    -1,
      -1,    -1,    -1,    -1,   163,   164,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    57,
      58,    59,    60,    61,    62,    63,    64,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,   107,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,   156,    -1,
      -1,    -1,    -1,    -1,    -1,   163,   164,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,
      57,    58,    59,    60,    61,    62,    63,    64,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,
     107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,   156,
      -1,    -1,    -1,    -1,    -1,    -1,   163,   164,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    -1,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    -1,    50,    51,    -1,    53,    -1,    55,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     106,   107,    -1,    -1,    -1,    -1,    18,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    49,    -1,    -1,
      52,    -1,    54,    -1,    56,    -1,    -1,    -1,    -1,   155,
     156,    -1,    -1,    -1,    -1,    -1,    -1,   163,   164,    71,
      -1,    73,    74,    -1,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    -1,
      -1,    93,    94,    95,    96,    97,    -1,    99,    -1,   101,
     102,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
     122,    18,   124,   125,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     152,    -1,    49,   155,   156,    52,    -1,    54,   160,    56,
     162,   163,   164,   165,   166,   167,   168,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    71,   177,    73,    74,    -1,    76,
      -1,    -1,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    -1,    -1,    93,    94,    95,    96,
      97,    -1,    99,    -1,   101,   102,    -1,   104,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,    -1,   124,   125,    -1,
      -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   152,    -1,    -1,   155,   156,
      -1,    -1,    -1,   160,    -1,   162,   163,   164,   165,   166,
     167,   168,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     177,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    49,    -1,    51,
      52,    53,    54,    -1,    56,    57,    58,    59,    60,    61,
      62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    85,    -1,    -1,    -1,    -1,    90,    -1,
      92,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,
     102,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,   120,   121,
     122,    -1,   124,   125,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     152,    -1,    -1,   155,   156,    -1,    -1,    -1,   160,    -1,
     162,   163,   164,   165,   166,   167,   168,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    49,    -1,    51,    52,    53,    54,    -1,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    85,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   101,   102,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,   120,   121,   122,    -1,   124,   125,
      -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   152,    -1,    -1,   155,
     156,    -1,    -1,    -1,   160,    -1,   162,   163,   164,   165,
     166,   167,   168,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    49,
      -1,    51,    52,    53,    54,    -1,    56,    57,    58,    59,
      60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    73,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   101,   102,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
     120,   121,   122,    -1,   124,   125,    -1,    -1,    -1,    -1,
      -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   152,    -1,   154,   155,   156,    -1,    -1,    -1,
      -1,    -1,   162,   163,   164,   165,   166,   167,   168,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    49,    -1,    51,    52,    53,
      54,    -1,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    73,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,    -1,   120,   121,   122,    -1,
     124,   125,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,    -1,
      -1,   155,   156,    -1,    -1,    -1,   160,    -1,   162,   163,
     164,   165,   166,   167,   168,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    49,    -1,    51,    52,    53,    54,    -1,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    73,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   101,   102,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,   120,   121,   122,    -1,   124,   125,    -1,    -1,
      -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   152,    -1,    -1,   155,   156,    -1,
      -1,    -1,    -1,    -1,   162,   163,   164,   165,   166,   167,
     168,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    49,    -1,    51,
      52,    53,    54,    -1,    56,    57,    58,    59,    60,    61,
      62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,
     102,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,   120,   121,
     122,    -1,   124,   125,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     152,    -1,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,
     162,   163,   164,   165,   166,   167,   168,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    49,    -1,    51,    52,    53,    54,    -1,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   101,   102,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,   120,   121,   122,    -1,   124,   125,
      -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   152,    -1,    -1,   155,
     156,    -1,    -1,    -1,    -1,    -1,   162,   163,   164,   165,
     166,   167,   168,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    -1,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   104,    -1,   106,   107,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,   159,    -1,
      -1,    57,    58,    59,    60,    61,    62,    63,    64,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
     106,   107,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    -1,    50,    51,    -1,    53,    -1,    55,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,
      -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,   107,
      -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,    -1,   155,    57,    58,
      59,    60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   104,    -1,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   152,   153,    -1,   155,   156,    -1,    -1,
      -1,   160,    -1,    -1,   163,   164,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,
      57,    58,    59,    60,    61,    62,    63,    64,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   152,   153,    -1,   155,   156,
      -1,    -1,    -1,    -1,    -1,    -1,   163,   164,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,
      -1,    57,    58,    59,    60,    61,    62,    63,    64,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     106,   107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    13,    14,    15,    16,    17,   154,   155,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    -1,
      50,    51,    -1,    53,    -1,    55,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   104,    -1,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   152,   153,    -1,   155,   156,    -1,    -1,    -1,
      -1,    -1,    -1,   163,   164,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    57,    58,
      59,    60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,   107,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    -1,    20,   155,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,
      -1,    -1,    57,    58,    59,    60,    61,    62,    63,    64,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   106,   107,    -1,    -1,    -1,    -1,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,
     155,    57,    58,    59,    60,    61,    62,    63,    64,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     106,   107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   154,   155,
     156,    -1,    -1,    -1,    -1,    -1,    -1,   163,   164,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,    -1,    -1,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,    78,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   106,   107,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   155,   156,    -1,    -1,    -1,    -1,    -1,    -1,   163,
     164,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,
      -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,    61,
      62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   106,   107,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,
      -1,   163,   164,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,
      60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   106,   107,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   155,   156,    -1,    -1,    -1,
      -1,    -1,    -1,   163,   164,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    57,
      58,    59,    60,    61,    62,    63,    64,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,   107,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,   156,    -1,
      -1,    -1,    -1,    -1,    -1,   163,   164,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,
      -1,    57,    58,    59,    60,    61,    62,    63,    64,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     106,   107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,
     156,    -1,    -1,    -1,    -1,    -1,    -1,   163,   164,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,    -1,    -1,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   106,   107,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   155,   156,    -1,    -1,    -1,    -1,    -1,    -1,   163,
     164,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,
      -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,    61,
      62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   106,   107,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,
      -1,   163,   164,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,
      60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   106,   107,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   155,   156,    -1,    -1,    -1,
      -1,    -1,    -1,   163,   164,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    57,
      58,    59,    60,    61,    62,    63,    64,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,   107,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,   156,    -1,
      -1,    -1,    -1,    -1,    -1,   163,   164,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    -1,    50,    51,    -1,    53,    -1,    55,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,    14,    15,
      16,    17,    18,    -1,    20,    72,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,   106,
     107,    57,    58,    59,    60,    61,    62,    63,    64,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,    -1,
     106,   107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   155,
     156,    -1,    -1,    -1,    -1,    -1,    -1,   163,   164,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,    -1,    -1,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   106,   107,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   155,    -1,    -1,    13,    14,    15,    16,    17,   163,
     164,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      -1,    50,    51,    -1,    53,    -1,    55,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,   107,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   155,   156,    -1,    13,
      14,    15,    16,    17,   163,   164,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    -1,    50,    51,    -1,    53,
      -1,    55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      13,    14,    15,    16,    17,    18,    -1,    20,    72,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,    -1,   106,   107,    57,    58,    59,    60,    61,    62,
      63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   155,    -1,   106,   107,    -1,    -1,    -1,    -1,   163,
     164,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      49,    -1,    51,    52,    53,    54,    -1,    56,    -1,    -1,
      -1,    -1,   155,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   101,   102,    -1,   104,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
      -1,   120,   121,   122,    -1,   124,   125,    -1,    -1,    -1,
      -1,    -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   152,    -1,    -1,   155,   156,    -1,    -1,
      -1,    -1,    -1,   162,   163,   164,   165,   166,   167,   168,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    49,
      -1,    51,    52,    53,    54,    -1,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   101,   102,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
     120,   121,   122,    -1,   124,   125,    -1,    -1,    -1,    -1,
      -1,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   152,    -1,    -1,   155,   156,    -1,    -1,    -1,
      -1,    -1,   162,   163,   164,   165,   166,   167,   168,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,    -1,    -1,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     104,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,   155,    53,    -1,    -1,    -1,    57,    58,    59,    60,
      61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      13,    14,    15,    16,    17,   106,   107,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    -1,    50,    51,    -1,
      53,    -1,    55,    -1,    -1,    -1,    49,    -1,    -1,    52,
      -1,    54,    -1,    56,   155,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   106,   107,    -1,    -1,    -1,   101,   102,
      -1,   104,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,    -1,   120,   121,   122,
      -1,   124,   125,    49,    -1,    -1,    52,    -1,    54,   132,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   155,   146,   147,   148,    -1,    73,    -1,   152,
     153,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,   162,
     163,   164,   165,   166,   167,   168,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   101,   102,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,   120,   121,   122,    49,   124,   125,
      52,    -1,    54,    -1,    56,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,   152,   153,    -1,   155,
     156,    -1,    -1,    -1,   160,    -1,   162,   163,   164,   165,
     166,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   101,
     102,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,   120,   121,
     122,    49,   124,   125,    52,    -1,    54,    -1,    56,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
     152,   153,    -1,   155,   156,    -1,    -1,    -1,   160,    -1,
     162,   163,   164,   165,   166,   167,   168,    -1,    -1,    -1,
      -1,    -1,    -1,   101,   102,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,   120,   121,   122,    49,   124,   125,    52,    -1,
      54,    -1,    56,    -1,   132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,   152,    -1,    -1,   155,   156,    -1,
      -1,    -1,   160,    -1,   162,   163,   164,   165,   166,   167,
     168,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,    -1,   120,   121,   122,    49,
     124,   125,    52,    -1,    54,    -1,    56,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,   152,    -1,
      -1,   155,   156,    -1,    -1,    -1,   160,    -1,   162,   163,
     164,   165,   166,   167,   168,    -1,    -1,    -1,    -1,    -1,
      -1,   101,   102,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
     120,   121,   122,    49,   124,   125,    52,    -1,    54,    -1,
      56,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,   152,   153,    -1,   155,   156,    -1,    -1,    -1,
      -1,    -1,   162,   163,   164,   165,   166,   167,   168,    -1,
      -1,    -1,    -1,    -1,    -1,   101,   102,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,   120,   121,   122,    49,   124,   125,
      52,    -1,    54,    -1,    56,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,   152,    -1,    -1,   155,
     156,    -1,    -1,   159,    -1,    -1,   162,   163,   164,   165,
     166,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   101,
     102,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,   120,   121,
     122,    49,   124,   125,    52,    -1,    54,    -1,    56,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
     152,   153,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,
     162,   163,   164,   165,   166,   167,   168,    -1,    -1,    -1,
      -1,    -1,    -1,   101,   102,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,   120,   121,   122,    49,   124,   125,    52,    -1,
      54,    -1,    56,    -1,   132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,   152,    -1,    -1,   155,   156,    -1,
      -1,    -1,   160,    -1,   162,   163,   164,   165,   166,   167,
     168,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,    -1,   120,   121,   122,    49,
     124,   125,    52,    -1,    54,    -1,    56,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,   152,    -1,
      -1,   155,   156,    -1,    -1,    -1,   160,    -1,   162,   163,
     164,   165,   166,   167,   168,    -1,    -1,    -1,    -1,    -1,
      -1,   101,   102,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
     120,   121,   122,    49,   124,   125,    52,    -1,    54,    -1,
      56,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,   152,    -1,   154,   155,   156,    -1,    -1,    -1,
      -1,    -1,   162,   163,   164,   165,   166,   167,   168,    -1,
      -1,    -1,    -1,    -1,    -1,   101,   102,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,   120,   121,   122,    49,   124,   125,
      52,    -1,    54,    -1,    56,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,   152,   153,    -1,   155,
     156,    -1,    -1,    -1,    -1,    -1,   162,   163,   164,   165,
     166,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   101,
     102,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,   120,   121,
     122,    49,   124,   125,    52,    -1,    54,    -1,    56,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
     152,   153,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,
     162,   163,   164,   165,   166,   167,   168,    -1,    -1,    -1,
      -1,    -1,    -1,   101,   102,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,   120,   121,   122,    49,   124,   125,    52,    -1,
      54,    -1,    56,    -1,   132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,   152,   153,    -1,   155,   156,    -1,
      -1,    -1,    -1,    -1,   162,   163,   164,   165,   166,   167,
     168,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,    -1,   120,   121,   122,    49,
     124,   125,    52,    -1,    54,    -1,    56,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,   152,   153,
      -1,   155,   156,    -1,    -1,    -1,    -1,    -1,   162,   163,
     164,   165,   166,   167,   168,    -1,    -1,    -1,    -1,    -1,
      -1,   101,   102,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
     120,   121,   122,    49,   124,   125,    52,    -1,    54,    -1,
      56,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,   152,   153,    -1,   155,   156,    -1,    -1,    -1,
      -1,    -1,   162,   163,   164,   165,   166,   167,   168,    -1,
      -1,    -1,    -1,    -1,    -1,   101,   102,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,   120,   121,   122,    49,   124,   125,
      52,    -1,    54,    -1,    56,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,   152,   153,    -1,   155,
     156,    -1,    -1,    -1,    -1,    -1,   162,   163,   164,   165,
     166,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   101,
     102,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,   120,   121,
     122,    49,   124,   125,    52,    -1,    54,    -1,    56,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
     152,   153,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,
     162,   163,   164,   165,   166,   167,   168,    -1,    -1,    -1,
      -1,    -1,    -1,   101,   102,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,   120,   121,   122,    49,   124,   125,    52,    -1,
      54,    -1,    56,    -1,   132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,   152,   153,    -1,   155,   156,    -1,
      -1,    -1,    -1,    -1,   162,   163,   164,   165,   166,   167,
     168,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,    -1,   120,   121,   122,    49,
     124,   125,    52,    -1,    54,    -1,    56,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,   152,   153,
      -1,   155,   156,    -1,    -1,    -1,    -1,    -1,   162,   163,
     164,   165,   166,   167,   168,    -1,    -1,    -1,    -1,    -1,
      -1,   101,   102,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
     120,   121,   122,    49,   124,   125,    52,    -1,    54,    -1,
      56,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,   152,   153,    -1,   155,   156,    -1,    -1,    -1,
      -1,    -1,   162,   163,   164,   165,   166,   167,   168,    -1,
      -1,    -1,    -1,    -1,    -1,   101,   102,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,   120,   121,   122,    49,   124,   125,
      52,    -1,    54,    -1,    56,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,   152,   153,    -1,   155,
     156,    -1,    -1,    -1,    -1,    -1,   162,   163,   164,   165,
     166,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   101,
     102,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,   120,   121,
     122,    49,   124,   125,    52,    -1,    54,    -1,    56,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
     152,   153,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,
     162,   163,   164,   165,   166,   167,   168,    -1,    -1,    -1,
      -1,    -1,    -1,   101,   102,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,   120,   121,   122,    49,   124,   125,    52,    -1,
      54,    -1,    56,    -1,   132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,   152,   153,    -1,   155,   156,    -1,
      -1,    -1,    -1,    -1,   162,   163,   164,   165,   166,   167,
     168,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,    -1,   120,   121,   122,    49,
     124,   125,    52,    -1,    54,    -1,    56,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,   152,   153,
      -1,   155,   156,    -1,    -1,    -1,    -1,    -1,   162,   163,
     164,   165,   166,   167,   168,    -1,    -1,    -1,    -1,    -1,
      -1,   101,   102,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
     120,   121,   122,    49,   124,   125,    52,    -1,    54,    -1,
      56,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,   152,   153,    -1,   155,   156,    -1,    -1,    -1,
      -1,    -1,   162,   163,   164,   165,   166,   167,   168,    -1,
      -1,    -1,    -1,    -1,    -1,   101,   102,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,   120,   121,   122,    49,   124,   125,
      52,    -1,    54,    -1,    56,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,   152,    -1,    -1,   155,
     156,    -1,    -1,    -1,    -1,    -1,   162,   163,   164,   165,
     166,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   101,
     102,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,   120,   121,
     122,    49,   124,   125,    52,    -1,    54,    -1,    56,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
     152,    -1,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,
     162,   163,   164,   165,   166,   167,   168,    -1,    -1,    -1,
      -1,    -1,    -1,   101,   102,    -1,   104,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,    -1,   120,   121,   122,    49,   124,   125,    52,    -1,
      54,    -1,    56,    -1,   132,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,   152,    -1,    -1,   155,   156,    -1,
      -1,    -1,    -1,    -1,   162,   163,   164,   165,   166,   167,
     168,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,    -1,
     104,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,    -1,   120,   121,   122,    49,
     124,   125,    52,    -1,    54,    -1,    56,    -1,   132,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,   152,    -1,
      -1,   155,   156,    -1,    -1,    -1,    -1,    -1,   162,   163,
     164,   165,   166,   167,   168,    -1,    -1,    -1,    -1,    -1,
      -1,   101,   102,    -1,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,    -1,
     120,   121,   122,    49,   124,   125,    52,    -1,    54,    -1,
      56,    -1,   132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,   152,    -1,    -1,   155,   156,    -1,    -1,    -1,
      -1,    -1,   162,   163,   164,   165,   166,   167,   168,    -1,
      -1,    -1,    -1,    -1,    -1,   101,   102,    -1,   104,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,    -1,   120,   121,   122,    49,   124,   125,
      52,    -1,    54,    -1,    56,    -1,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,   152,    -1,    -1,   155,
     156,    -1,    -1,    -1,    -1,    -1,   162,   163,   164,   165,
     166,   167,   168,    -1,    -1,    -1,    -1,    -1,    -1,   101,
     102,    -1,   104,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,    -1,   120,   121,
     122,    -1,   124,   125,    -1,    -1,    -1,    -1,    -1,    -1,
     132,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     152,    -1,    -1,   155,   156,    -1,    -1,    -1,    -1,    -1,
     162,   163,   164,   165,   166,   167,   168,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   104,    -1,   106,
     107,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    -1,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   104,    -1,   106,   107,    13,    14,    15,    16,    17,
      -1,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    -1,    50,    51,    -1,    53,    -1,    55,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    13,    14,    15,    16,
      17,    -1,    -1,    20,    72,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    -1,    50,    51,    -1,    53,    -1,    55,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_int16 yystos[] =
{
       0,   179,   401,   402,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      20,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    51,    53,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    69,    72,    73,
      99,   103,   104,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   119,   132,   152,   153,   155,   156,   163,
     164,   182,   183,   184,   199,   289,   290,   291,   292,   293,
     294,   295,   296,   297,   298,   299,   300,   303,   306,   308,
     309,   310,   311,   312,   313,   314,   315,   316,   318,   320,
     321,   322,   324,   325,   329,   330,   331,   332,   333,   335,
     341,   342,   343,   344,   355,   359,   393,   396,   406,   412,
     414,   420,   424,   429,   430,   431,   432,   433,   434,   435,
     436,   462,   480,   481,   482,   483,     0,   179,   104,   183,
     199,   293,   295,   306,   309,   312,   321,   325,   330,   118,
     152,    58,    61,    62,    64,   152,   152,   418,   419,   420,
     317,   318,   106,   107,   183,   373,   394,   395,   373,   152,
     406,   152,   152,     4,   104,   106,   107,   310,   315,   316,
     152,   199,   419,   424,   430,   431,   432,   434,   435,   436,
     106,   332,   157,   179,   296,   306,   309,   429,   433,   479,
     480,   483,   484,   177,   180,   149,   160,   176,   220,   376,
      89,   158,   413,   373,   180,   180,   180,   177,   106,   107,
     152,   199,   301,   302,   424,   425,   426,   427,   428,   429,
     433,   437,   438,   439,   440,   441,   442,   443,   444,   445,
     451,     3,    47,    48,    50,    55,   323,     3,   156,   199,
     295,   310,   314,   316,   326,   331,   409,   429,   433,   483,
      69,   293,   295,   309,   321,   325,   330,   410,   429,   433,
      65,   315,   315,   310,   316,   304,   315,   316,   323,   342,
     310,   315,   310,   155,   418,   158,   180,   152,   160,   228,
     418,   418,   179,   284,   285,   156,   306,   309,   481,   373,
     373,   406,   176,   309,   152,   199,   415,   424,   425,   429,
     438,   442,   156,   199,   483,   407,   408,    65,    66,    67,
      68,   156,   174,   373,   382,   384,   388,   390,   391,   331,
      57,   154,   156,   199,   305,   309,   313,   314,   320,   321,
     327,   328,   329,   330,   334,   341,   342,   359,   369,   371,
     462,   475,   476,   477,   478,   483,   484,   106,   107,   160,
     183,   331,   451,   420,   152,   389,   390,   152,   152,   118,
     185,   186,    49,    52,    54,    56,    73,   101,   102,   104,
     105,   116,   117,   120,   121,   122,   124,   125,   152,   156,
     162,   165,   166,   167,   168,   181,   182,   185,   187,   190,
     198,   199,   200,   201,   204,   205,   206,   207,   208,   209,
     210,   211,   212,   213,   214,   215,   216,   222,   331,   154,
     156,   198,   199,   215,   217,   306,   331,   374,   375,   392,
     479,   484,   309,   430,   431,   432,   434,   435,   436,   154,
     154,   154,   154,   154,   154,   154,   156,   306,   462,   481,
     156,   163,   199,   217,   295,   296,   305,   307,   309,   321,
     328,   330,   364,   365,   368,   369,   370,   475,   483,   152,
     429,   433,   483,   152,   158,   104,   155,   156,   160,   182,
     184,   217,   377,   378,   379,   380,   381,    22,   377,   152,
     373,   228,   152,   158,   158,   158,   419,   424,   426,   427,
     428,   437,   439,   440,   441,   443,   444,   445,   309,   425,
     438,   442,   158,    99,   417,   156,   418,   459,   462,   417,
     418,   418,   413,   284,   152,   418,   459,   417,   418,   418,
     413,   418,   418,   309,   415,   152,   152,   308,   309,   306,
     309,   179,   306,   479,   484,   333,   160,   413,   284,   373,
     373,   376,   295,   314,   411,   429,   433,   160,   413,   284,
     394,   309,   321,   309,   309,   106,   332,   106,   107,   183,
     331,   336,   394,   179,   183,   372,   151,   179,     3,   300,
     303,   309,   313,   228,   179,   179,   417,   152,   417,   180,
     217,   419,   424,   309,   152,   179,   373,   404,   160,   373,
     160,   373,   134,   163,   164,   387,   154,   158,   373,   391,
     154,   418,   418,   157,   179,   307,   309,   321,   328,   330,
     474,   475,   483,   484,   152,   156,   164,   176,   199,   462,
     464,   465,   466,   467,   468,   469,   486,   199,   334,   483,
     309,   328,   315,   310,   418,   154,   307,   309,   476,   307,
     462,   476,    10,   162,   167,   358,   360,   361,   356,   358,
     382,   176,   382,    13,    88,   104,   106,   107,   182,   421,
     422,   423,   154,   118,   152,   198,   152,   152,   152,   201,
     152,   198,   152,   104,   106,   107,   310,   315,   316,   152,
     198,   198,    19,    21,    85,   156,   165,   166,   202,   203,
     217,   224,   228,   344,   374,   483,   158,   179,   152,   187,
     156,   161,   156,   161,   121,   123,   124,   125,   152,   155,
     156,   160,   161,   201,   201,   169,   163,   170,   171,   165,
     166,   126,   127,   128,   129,   172,   173,   130,   131,   164,
     162,   174,   132,   133,   175,   154,   158,   155,   179,   135,
     136,   137,   138,   139,   140,   141,   142,   143,   144,   145,
     176,   219,   220,   221,   152,   199,   455,   456,   457,   458,
     459,   154,   158,   154,   154,   154,   154,   154,   154,   152,
     418,   459,   462,   152,   459,   462,   179,   306,   481,   179,
     180,   180,   152,   164,   199,   424,   446,   447,   448,   449,
     450,   451,   452,   453,   454,   134,   483,   180,   180,   373,
     373,   179,   179,   179,   156,   184,   179,   378,   159,   158,
     485,   377,   155,   156,   159,   381,   153,   217,   223,   152,
     179,   179,   179,   179,   424,   426,   427,   428,   437,   439,
     440,   441,   443,   444,   445,   154,   154,   154,   154,   154,
     154,   154,   154,   154,   154,   425,   438,   442,   418,   152,
     176,   157,   179,   376,   228,   413,   179,   376,   228,   415,
     224,   375,   224,   375,   415,   404,   228,   413,   417,   160,
     160,   413,   284,   404,   228,   413,   338,   339,   337,   160,
     134,   309,   366,   367,   370,   371,   154,   158,    70,   286,
     287,   180,   429,   442,   309,   300,   163,   217,   179,   424,
     365,   406,   404,   157,   179,   152,   386,   384,   385,    78,
     319,   183,   160,   183,   451,   307,   462,   476,   309,   313,
     483,   179,   465,   466,   467,   157,   179,    18,   217,   309,
     464,   486,   418,   418,   462,   307,   474,   484,   309,   183,
     418,   307,   476,   331,   158,   485,   373,   358,   160,   154,
     375,   154,   154,   158,   152,   177,   374,   187,   156,   374,
     374,   374,   217,   374,   154,   374,   374,   374,   179,   154,
     165,   166,   203,    18,   311,   154,   158,   154,   163,   164,
     154,   223,   217,   160,   217,   183,   217,   183,   116,   156,
     183,   153,   191,   192,   193,   217,   116,   156,   183,   344,
     217,   191,   183,   201,   204,   204,   204,   205,   205,   206,
     206,   207,   207,   207,   207,   208,   208,   209,   210,   211,
     212,   213,   159,   224,   177,   185,   156,   183,   217,   160,
     217,   179,   456,   457,   458,   309,   455,   418,   418,   217,
     375,   152,   418,   459,   462,   152,   459,   462,   179,   179,
     157,   157,   152,   424,   447,   448,   449,   452,    18,   309,
     446,   450,   152,   418,   468,   486,   418,   418,   486,   152,
     418,   468,   418,   418,   180,   216,   373,   157,   158,   157,
     158,   486,   486,   134,   363,   364,   365,   363,   373,   179,
     215,   216,   217,   416,   485,   377,   379,   151,   179,   154,
     158,   179,   363,   183,   415,   183,   154,   154,   154,   154,
     154,   154,   154,   154,   154,   152,   418,   459,   462,   152,
     418,   459,   462,   152,   418,   459,   462,   415,   185,    22,
     462,   217,   316,   331,   460,   228,   366,   154,   154,   154,
     154,   402,   403,   228,   151,   179,   404,   228,   413,   403,
     228,   160,   160,   160,   345,   180,   180,   183,   288,   373,
      18,    71,    73,    76,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    93,    94,    95,    96,
      97,    99,   106,   107,   119,   152,   179,   224,   225,   226,
     227,   228,   229,   230,   232,   233,   243,   249,   250,   251,
     252,   253,   254,   259,   260,   263,   264,   265,   266,   267,
     273,   274,   275,   289,   309,   313,   373,   414,    70,   177,
     180,   180,   180,   363,   180,   405,   403,   293,   295,   306,
     397,   398,   399,   400,   392,   176,   383,   383,   360,   418,
     418,   307,   476,   156,   163,   199,   217,   331,   217,   309,
     366,   154,   154,   154,     5,   309,   418,   464,   160,   183,
     451,    10,   361,   151,   176,   362,   160,   360,   160,   154,
     422,   191,   154,   158,   179,   158,   154,   154,   158,   154,
     201,   154,   154,   154,   201,    18,   311,   217,   154,   154,
     153,   160,   201,   157,   180,   191,   157,   157,   116,   120,
     122,   184,   194,   195,   196,   154,   158,   194,   157,   158,
     151,   215,   159,   154,   194,   180,   378,   366,   154,   154,
     154,   455,   179,   179,   366,   366,   452,   154,   154,   154,
     154,   152,   424,   451,   446,   450,   179,   179,   157,   180,
     486,   179,   179,   180,   180,   180,   180,   376,   194,   134,
     168,   180,   180,   151,   377,   217,   418,   153,   217,   363,
     180,   176,   152,   418,   459,   462,   152,   418,   459,   462,
     152,   418,   459,   462,   179,   179,   179,   417,   154,   146,
     168,   180,   461,   158,   180,   180,   405,   397,   403,   228,
     405,   345,   345,   345,     3,     5,    10,    73,   151,   290,
     297,   298,   306,   309,   346,   351,   479,   154,   158,   158,
     177,   152,    61,    62,   177,   228,   289,   414,   152,    18,
     226,   152,   152,   177,   373,   177,   373,   163,   373,   160,
     225,   152,   152,   152,   228,   217,   218,   218,    14,   276,
     254,   265,    74,   234,   177,   180,   230,    78,   177,   373,
      91,    92,   258,   262,   110,   133,   257,   109,   132,   261,
     257,   372,   309,   159,   288,   177,   157,   157,   180,   158,
     405,   415,   180,   177,   180,   177,   180,   154,   375,   389,
     389,   485,   358,   358,   179,   180,   180,   180,   217,   180,
     152,   418,   468,   462,   308,     5,   163,   180,   217,   360,
     418,   418,   331,   373,   160,   216,   360,   485,   151,   179,
     154,   305,   183,    78,   188,   189,   374,   201,   201,   201,
     201,   201,   160,   378,   158,   151,   197,   156,   195,   197,
     197,   157,   158,   123,   155,   193,   157,   223,   215,   177,
     157,   485,   180,   152,   418,   459,   462,   366,   366,   180,
     180,   154,   152,   418,   459,   462,   152,   418,   468,   424,
     418,   418,   366,   366,   157,   365,   368,   368,   369,   154,
     158,   158,   154,   180,   216,   216,   157,   157,   180,   180,
     154,   217,   179,   179,   179,   366,   366,   366,   376,   418,
     158,   217,   217,   316,   331,   157,   154,   151,   180,   405,
     151,   151,   151,   151,   306,   306,   344,   352,   479,   306,
     351,   152,   340,   177,   177,   152,   159,   199,   347,   348,
     354,   424,   425,   438,   442,   158,   177,   373,   179,   373,
     191,   177,   228,   177,   228,   224,    80,   154,   224,   235,
     289,   291,   294,   300,   309,   313,   146,   147,   148,   153,
     154,   177,   224,   244,   245,   246,   289,   177,   177,   224,
     177,   378,   177,   224,   223,   224,   111,   112,   113,   114,
     115,   268,   270,   271,   177,    98,   177,    84,   152,   154,
     152,   180,   151,   177,   177,   152,   152,   226,   226,   254,
     152,   264,   254,   265,   228,   418,   177,   154,   179,   151,
     151,   179,   158,   158,   151,   160,   160,   157,   157,   157,
     180,   154,   179,   217,   217,   180,   157,   180,   485,   357,
     358,   362,   362,   378,   485,   151,   397,   463,   464,   154,
     159,   154,   158,   159,   378,   485,   223,   121,   194,   195,
     156,   195,   156,   195,   157,   151,   154,   179,   180,   180,
     154,   154,   179,   179,   180,   180,   180,   179,   179,   157,
     180,   154,   418,   366,   366,   366,   180,   180,   180,   224,
     461,   151,   151,   340,   340,   340,   347,   152,   199,   349,
     350,   459,   470,   471,   472,   473,   177,   158,   177,   347,
     177,   392,   419,   424,   217,   309,   158,   177,   353,   354,
     353,   353,   373,   134,   370,   371,   154,   154,   152,   226,
     154,   224,   309,   146,   147,   148,   168,   177,   247,   248,
     226,   225,   177,   248,   154,   159,   224,   153,   224,   225,
     246,   177,   485,   154,   154,   154,   228,   270,   271,   152,
     217,   152,   185,   235,   201,   255,   201,    75,   108,   256,
     258,    75,   256,     1,   226,   418,   398,   179,   179,   360,
     360,   157,   366,   180,   180,   157,   157,   151,   358,   160,
     485,   151,   180,   154,   217,   189,   217,   485,   151,   157,
     157,   194,   194,   366,   154,   154,   366,   366,   154,   154,
     157,   158,   134,   365,   134,   157,   180,   180,   180,   154,
     154,   154,   157,   217,   177,   471,   472,   473,   309,   470,
     158,   177,   418,   418,   177,   154,   424,   418,   226,    77,
      78,   160,   238,   239,   240,   154,   224,    75,   226,   224,
     153,   224,    75,   177,   106,   153,   224,   225,   246,   153,
     224,   226,   245,   248,   248,   177,   224,   151,   160,   240,
     226,   152,   179,   177,   185,   154,   159,   154,   154,   158,
     159,   154,   226,   152,   226,   226,   226,   226,   373,   415,
     485,   485,   180,   157,   157,   160,   360,   151,   151,   151,
     157,   157,   180,   180,   180,   179,   180,   154,   154,   154,
     154,   154,   154,   470,   418,   348,     1,   216,   236,   237,
     416,     1,   159,     1,   179,   226,   238,    75,   177,   154,
     226,    75,   177,   168,   168,   226,   225,   248,   248,   177,
     106,   224,   168,   168,    75,   153,   224,   153,   224,   225,
     177,     1,   179,   179,   272,   307,   309,   479,   159,   177,
     156,   185,   277,   278,   279,   226,   201,   191,   224,   257,
     257,   151,   151,   154,   360,   485,   154,   154,   154,   368,
     152,   418,   459,   462,   350,   134,     1,   158,   159,   151,
     282,   283,   289,   226,    75,   177,   226,   224,   153,   153,
     224,   153,   224,   153,   224,   225,   153,   224,   153,   224,
     226,   168,   168,   168,   168,   151,   282,   272,   180,   152,
     199,   415,   470,   183,   159,   104,   152,   154,   159,   158,
      75,   154,   154,    75,   253,    75,   253,   485,   151,   179,
     216,   236,   239,   241,   242,   289,   226,   168,   168,   168,
     168,   153,   153,   224,   153,   224,   153,   224,   241,   180,
     177,   269,   309,   277,   157,   216,   177,   277,   279,   226,
     226,    75,   226,    75,   151,   366,   226,   231,   180,   239,
     153,   153,   224,   153,   224,   153,   224,   180,   269,   215,
     154,   159,   185,   154,   154,   159,   226,   226,   180,     1,
     226,   151,   231,   151,   154,   228,   185,   280,   152,   177,
     280,   154,   228,   158,   159,   216,   154,   185,   183,   281,
     154,   177,   154,   158,   177,   183
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_int16 yyr1[] =
{
       0,   178,   179,   180,   181,   181,   181,   181,   181,   182,
     182,   182,   182,   182,   182,   182,   182,   183,   183,   184,
     184,   185,   186,   186,   187,   187,   187,   187,   187,   187,
     187,   187,   187,   187,   187,   187,   187,   187,   187,   188,
     188,   189,   189,   190,   190,   190,   190,   190,   190,   190,
     190,   190,   190,   190,   190,   190,   190,   190,   190,   190,
     190,   190,   190,   190,   190,   190,   190,   191,   191,   192,
     192,   193,   193,   194,   194,   195,   195,   195,   195,   195,
     195,   195,   196,   196,   196,   197,   197,   198,   198,   198,
     198,   198,   198,   198,   198,   198,   198,   198,   198,   198,
     198,   199,   199,   199,   200,   200,   200,   200,   201,   201,
     201,   201,   201,   201,   201,   201,   201,   202,   202,   202,
     202,   203,   203,   204,   204,   205,   205,   205,   205,   206,
     206,   206,   207,   207,   207,   208,   208,   208,   208,   208,
     209,   209,   209,   210,   210,   211,   211,   212,   212,   213,
     213,   214,   214,   215,   215,   215,   216,   217,   217,   217,
     218,   218,   219,   219,   220,   220,   221,   221,   221,   221,
     221,   221,   221,   221,   221,   221,   221,   222,   222,   223,
     223,   223,   223,   224,   224,   225,   225,   226,   226,   226,
     226,   226,   226,   226,   226,   226,   226,   226,   226,   226,
     226,   227,   227,   228,   228,   229,   229,   230,   230,   230,
     230,   230,   231,   231,   231,   232,   233,   233,   233,   233,
     233,   233,   233,   234,   234,   235,   235,   235,   235,   236,
     236,   236,   237,   237,   238,   238,   238,   238,   238,   239,
     239,   240,   241,   241,   242,   242,   243,   243,   243,   243,
     243,   243,   243,   243,   243,   243,   243,   243,   244,   244,
     245,   245,   245,   245,   245,   245,   245,   245,   245,   245,
     245,   245,   245,   245,   245,   245,   245,   245,   245,   245,
     245,   245,   245,   245,   245,   245,   245,   245,   245,   245,
     245,   245,   245,   245,   245,   245,   245,   245,   245,   245,
     245,   245,   245,   245,   245,   246,   246,   246,   247,   247,
     248,   248,   248,   249,   249,   249,   249,   249,   249,   249,
     249,   249,   249,   249,   249,   249,   249,   249,   249,   249,
     249,   249,   249,   250,   250,   251,   252,   253,   254,   254,
     255,   255,   256,   257,   257,   258,   258,   259,   259,   259,
     259,   259,   259,   260,   261,   261,   262,   263,   263,   264,
     264,   265,   265,   265,   265,   265,   265,   266,   267,   267,
     267,   268,   268,   269,   269,   270,   270,   270,   270,   271,
     272,   272,   272,   272,   272,   273,   274,   274,   275,   275,
     275,   275,   275,   276,   276,   277,   277,   278,   278,   279,
     279,   280,   280,   280,   281,   281,   282,   282,   283,   283,
     284,   284,   285,   285,   286,   286,   287,   287,   288,   288,
     289,   289,   289,   290,   290,   291,   291,   291,   291,   291,
     292,   292,   292,   293,   293,   293,   294,   294,   294,   294,
     294,   295,   295,   296,   296,   297,   297,   297,   298,   298,
     298,   298,   298,   299,   299,   300,   300,   300,   300,   301,
     301,   301,   301,   301,   302,   302,   303,   303,   303,   303,
     304,   304,   304,   305,   305,   305,   306,   306,   306,   307,
     307,   307,   308,   308,   309,   309,   310,   310,   311,   311,
     311,   311,   311,   312,   313,   313,   313,   314,   314,   315,
     315,   315,   315,   315,   315,   315,   315,   315,   316,   316,
     316,   316,   316,   316,   316,   316,   316,   316,   316,   316,
     316,   316,   316,   316,   316,   316,   316,   316,   316,   316,
     316,   316,   316,   316,   316,   316,   317,   317,   318,   319,
     319,   320,   320,   320,   320,   320,   321,   321,   322,   322,
     322,   322,   323,   323,   323,   323,   323,   323,   324,   324,
     324,   324,   325,   326,   325,   325,   327,   327,   327,   327,
     328,   328,   328,   329,   329,   329,   329,   330,   330,   330,
     331,   331,   331,   331,   331,   331,   332,   332,   332,   333,
     333,   334,   334,   336,   335,   337,   335,   338,   335,   339,
     335,   335,   340,   340,   341,   341,   342,   342,   343,   343,
     343,   344,   344,   344,   344,   344,   344,   344,   344,   345,
     345,   346,   346,   346,   346,   346,   346,   346,   346,   346,
     346,   346,   347,   347,   347,   348,   348,   348,   348,   349,
     349,   349,   350,   351,   351,   352,   352,   353,   353,   354,
     355,   356,   355,   355,   355,   355,   357,   355,   355,   355,
     355,   355,   358,   358,   359,   359,   360,   360,   360,   360,
     361,   361,   362,   362,   362,   363,   363,   363,   363,   363,
     363,   363,   364,   364,   364,   364,   365,   365,   366,   366,
     366,   366,   367,   367,   367,   367,   368,   368,   368,   368,
     368,   369,   369,   369,   369,   369,   370,   370,   371,   371,
     372,   372,   373,   373,   373,   374,   374,   374,   375,   375,
     376,   376,   376,   376,   377,   377,   378,   378,   378,   378,
     378,   379,   379,   380,   380,   381,   381,   381,   381,   381,
     382,   382,   383,   383,   385,   384,   386,   384,   384,   384,
     387,   387,   387,   387,   388,   388,   388,   388,   389,   389,
     390,   390,   391,   391,   392,   392,   392,   392,   393,   393,
     393,   394,   394,   395,   395,   396,   396,   396,   396,   397,
     397,   398,   398,   399,   399,   399,   400,   400,   401,   401,
     402,   402,   403,   403,   404,   405,   406,   406,   406,   406,
     406,   406,   406,   406,   406,   406,   406,   407,   406,   408,
     406,   409,   406,   410,   406,   411,   406,   412,   412,   412,
     413,   413,   414,   414,   414,   414,   414,   414,   414,   414,
     414,   414,   415,   415,   415,   415,   416,   417,   417,   418,
     418,   419,   419,   420,   421,   421,   422,   422,   422,   423,
     423,   423,   423,   423,   423,   424,   424,   425,   425,   425,
     425,   426,   426,   426,   426,   427,   427,   427,   427,   427,
     427,   427,   428,   428,   428,   428,   429,   429,   429,   430,
     430,   430,   430,   430,   431,   431,   431,   431,   432,   432,
     432,   432,   432,   432,   433,   433,   433,   434,   434,   434,
     434,   434,   435,   435,   435,   435,   436,   436,   436,   436,
     436,   436,   437,   437,   438,   438,   438,   438,   439,   439,
     439,   439,   440,   440,   440,   440,   440,   440,   440,   441,
     441,   441,   441,   442,   442,   442,   443,   443,   443,   443,
     443,   444,   444,   444,   444,   445,   445,   445,   445,   445,
     445,   446,   446,   446,   446,   446,   447,   447,   447,   448,
     448,   448,   448,   449,   449,   449,   450,   450,   450,   450,
     450,   451,   451,   452,   452,   452,   453,   453,   454,   454,
     455,   455,   455,   456,   456,   456,   456,   456,   457,   457,
     457,   457,   458,   458,   458,   459,   459,   459,   459,   459,
     460,   460,   460,   460,   460,   460,   461,   461,   462,   462,
     462,   462,   463,   463,   464,   464,   464,   464,   465,   465,
     465,   465,   465,   466,   466,   466,   466,   467,   467,   467,
     468,   468,   468,   469,   469,   469,   469,   469,   469,   470,
     470,   470,   471,   471,   471,   471,   471,   472,   472,   472,
     472,   473,   473,   474,   474,   474,   475,   475,   476,   476,
     476,   476,   476,   476,   477,   477,   477,   477,   477,   477,
     477,   477,   477,   477,   478,   478,   478,   478,   479,   479,
     479,   480,   480,   481,   481,   481,   481,   481,   481,   482,
     482,   482,   482,   482,   482,   483,   483,   483,   484,   484,
     485,   485,   486,   486
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     0,     0,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     2,     1,     1,     1,     1,     3,     3,
       3,     5,     6,     2,     2,     2,     2,     2,     2,     1,
       3,     3,     3,     1,     6,     4,     4,     4,     4,     4,
       7,     3,     3,     3,     3,     3,     2,     5,     3,     3,
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
       1,     4,     4,     2,     6,     1,     2,     1,     2,     1,
       2,     1,     1,     2,     2,     2,     3,     5,    10,     7,
       5,    10,     7,     5,     7,     1,     1,     1,     2,     1,
       3,     1,     1,     3,     2,     3,     3,     2,     2,     1,
       2,     2,     0,     1,     2,     3,     4,     6,     5,     7,
       6,     7,     7,     8,     4,     6,     5,     7,     1,     3,
       4,     5,     4,     3,     5,     1,     2,     3,     3,     3,
       5,     5,     5,     5,     3,     5,     5,     5,     3,     4,
       5,     5,     5,     5,     7,     7,     7,     7,     7,     7,
       7,     2,     3,     4,     4,     4,     6,     6,     6,     6,
       6,     6,     6,     3,     4,     1,     1,     1,     1,     1,
       1,     1,     1,     3,     4,     2,     3,     3,     2,     3,
       2,     3,     3,     6,     2,     2,     3,     3,     3,     3,
       3,     3,     5,     1,     1,     5,     5,     4,     0,     1,
       1,     3,     4,     1,     1,     4,     6,     3,     5,     5,
       5,     8,     9,     1,     1,     1,     4,     3,     3,     1,
       3,     1,     3,     5,     5,     8,     9,     1,     3,     3,
       4,     8,     9,     0,     2,     1,     1,     1,     1,     2,
       1,     2,     2,     2,     1,     3,     1,     1,     6,     8,
      10,    12,    14,     0,     1,     0,     1,     1,     3,     4,
       7,     0,     1,     3,     1,     3,     0,     1,     1,     2,
       0,     1,     4,     5,     0,     1,     3,     4,     1,     3,
       2,     2,     1,     7,     5,     1,     1,     1,     1,     1,
       2,     3,     6,     3,     3,     4,     1,     2,     2,     3,
       8,     8,     8,     5,     9,     2,     2,     5,     3,     5,
       4,     3,     4,     4,     7,     2,     1,     1,     1,     3,
       3,     2,     4,     6,     1,     1,     1,     1,     1,     2,
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
       2,     3,     4,     4,     4,     3,     2,     2,     3,     3,
       2,     1,     0,     1,     4,     1,     2,     2,     2,     0,
       1,     4,     1,     2,     3,     1,     2,     0,     1,     2,
       6,     0,     9,     8,     9,     8,     0,    13,    11,    12,
      11,     1,     0,     1,     3,     3,     3,     2,     5,     5,
       1,     1,     0,     2,     5,     0,     1,     1,     1,     5,
       5,     5,     1,     5,     5,     9,     1,     5,     0,     1,
       1,     5,     1,     1,     5,     5,     1,     3,     3,     4,
       1,     1,     1,     1,     2,     1,     3,     3,     2,     3,
       1,     3,     1,     1,     1,     1,     1,     2,     1,     1,
       0,     2,     2,     4,     1,     4,     0,     1,     2,     3,
       4,     2,     2,     1,     2,     2,     5,     5,     7,     6,
       1,     3,     0,     2,     0,     5,     0,     5,     3,     1,
       0,     1,     1,     1,     1,     1,     1,     1,     0,     1,
       1,     2,     5,     6,     1,     1,     3,     3,     2,     3,
       3,     2,     4,     1,     4,     7,     5,    10,     8,     1,
       4,     2,     2,     1,     1,     5,     2,     5,     0,     1,
       3,     4,     0,     1,     0,     0,     1,     1,     2,     2,
       2,     2,     2,     2,     1,     2,     5,     0,     6,     0,
       8,     0,     7,     0,     7,     0,     8,     1,     2,     3,
       0,     5,     3,     4,     4,     4,     4,     5,     5,     5,
       5,     6,     1,     1,     1,     1,     3,     0,     5,     0,
       1,     1,     2,     6,     1,     3,     0,     1,     4,     1,
       1,     1,     1,     1,     1,     1,     3,     2,     1,     2,
       2,     2,     3,     4,     5,     2,     4,     5,     4,     5,
       3,     4,     8,     9,     3,     4,     2,     1,     2,     6,
       8,     9,     3,     4,     2,     3,     4,     5,     4,     5,
       4,     5,     3,     4,     1,     1,     1,     4,     8,     9,
       3,     4,     2,     3,     3,     4,     4,     5,     4,     5,
       3,     4,     1,     3,     2,     1,     2,     2,     2,     3,
       4,     5,     2,     4,     5,     4,     5,     3,     4,     8,
       9,     3,     4,     2,     1,     2,     6,     8,     9,     3,
       4,     2,     3,     4,     5,     4,     5,     4,     5,     3,
       4,     2,     4,     1,     2,     2,     2,     3,     4,     2,
       4,     4,     3,     6,     8,     3,     2,     4,     1,     2,
       2,     1,     1,     2,     3,     4,     2,     4,     6,     8,
       1,     2,     2,     1,     2,     2,     3,     4,     1,     4,
       4,     3,     5,     8,     3,     2,     3,     7,     5,     1,
       1,     1,     3,     3,     3,     5,     1,     1,     5,     5,
       6,     6,     0,     1,     1,     3,     2,     2,     1,     2,
       2,     3,     4,     1,     4,     4,     3,     5,     8,     3,
       1,     2,     1,     2,     6,     5,     6,     7,     7,     1,
       2,     2,     1,     2,     2,     3,     4,     1,     4,     4,
       3,     8,     3,     1,     1,     2,     1,     1,     2,     3,
       2,     3,     2,     3,     3,     2,     4,     3,     2,     3,
       2,     4,     3,     2,     6,     6,     6,     7,     1,     2,
       1,     1,     1,     2,     3,     2,     3,     2,     3,     3,
       4,     2,     3,     4,     2,     5,     6,     7,     6,     6,
       0,     1,     0,     2
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
#line 602 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.enterScope(); }
#line 7972 "Parser/parser.cc"
    break;

  case 3:
#line 606 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 7978 "Parser/parser.cc"
    break;

  case 4:
#line 613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantInteger( *(yyvsp[0].tok) ) ); }
#line 7984 "Parser/parser.cc"
    break;

  case 5:
#line 614 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7990 "Parser/parser.cc"
    break;

  case 6:
#line 615 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7996 "Parser/parser.cc"
    break;

  case 7:
#line 616 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 8002 "Parser/parser.cc"
    break;

  case 8:
#line 617 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantChar( *(yyvsp[0].tok) ) ); }
#line 8008 "Parser/parser.cc"
    break;

  case 20:
#line 639 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 8014 "Parser/parser.cc"
    break;

  case 21:
#line 643 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.constant) = build_constantStr( *(yyvsp[0].str) ); }
#line 8020 "Parser/parser.cc"
    break;

  case 22:
#line 647 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 8026 "Parser/parser.cc"
    break;

  case 23:
#line 649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 8036 "Parser/parser.cc"
    break;

  case 24:
#line 660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 8042 "Parser/parser.cc"
    break;

  case 25:
#line 662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 8048 "Parser/parser.cc"
    break;

  case 26:
#line 666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_dimensionref( (yyvsp[0].tok) ) ); }
#line 8054 "Parser/parser.cc"
    break;

  case 28:
#line 669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 8060 "Parser/parser.cc"
    break;

  case 29:
#line 671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new StmtExpr( dynamic_cast<CompoundStmt *>(maybeMoveBuild( (yyvsp[-1].sn) ) ) ) ); }
#line 8066 "Parser/parser.cc"
    break;

  case 30:
#line 673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_qualified_expr( (yyvsp[-2].decl), build_varref( (yyvsp[0].tok) ) ) ); }
#line 8072 "Parser/parser.cc"
    break;

  case 31:
#line 675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8078 "Parser/parser.cc"
    break;

  case 32:
#line 677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild( (yyvsp[-3].en) );
			(yyval.en) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 8088 "Parser/parser.cc"
    break;

  case 33:
#line 687 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, "n expression" ); (yyval.en) = nullptr; }
#line 8094 "Parser/parser.cc"
    break;

  case 34:
#line 689 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.en) = nullptr; }
#line 8100 "Parser/parser.cc"
    break;

  case 35:
#line 691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.en) = nullptr; }
#line 8106 "Parser/parser.cc"
    break;

  case 36:
#line 693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.en) = nullptr; }
#line 8112 "Parser/parser.cc"
    break;

  case 37:
#line 695 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.en) = nullptr; }
#line 8118 "Parser/parser.cc"
    break;

  case 38:
#line 697 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.en) = nullptr; }
#line 8124 "Parser/parser.cc"
    break;

  case 40:
#line 703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// steal the association node from the singleton and delete the wrapper
			(yyvsp[-2].genexpr)->associations.splice((yyvsp[-2].genexpr)->associations.end(), (yyvsp[0].genexpr)->associations);
			delete (yyvsp[0].genexpr);
			(yyval.genexpr) = (yyvsp[-2].genexpr);
		}
#line 8135 "Parser/parser.cc"
    break;

  case 41:
#line 713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuildType((yyvsp[-2].decl)), maybeMoveBuild( (yyvsp[0].en) ) } } );
		}
#line 8144 "Parser/parser.cc"
    break;

  case 42:
#line 718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuild( (yyvsp[0].en) ) } } ); }
#line 8150 "Parser/parser.cc"
    break;

  case 44:
#line 727 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-5].en), new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) ) ) )) ) ); }
#line 8156 "Parser/parser.cc"
    break;

  case 45:
#line 733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 8162 "Parser/parser.cc"
    break;

  case 46:
#line 735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 8168 "Parser/parser.cc"
    break;

  case 47:
#line 737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, new ExpressionNode( (yyvsp[-3].constant) ), (yyvsp[-1].en) ) ); }
#line 8174 "Parser/parser.cc"
    break;

  case 48:
#line 739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.en) = new ExpressionNode( new ConstructorExpr( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) ) );
		}
#line 8184 "Parser/parser.cc"
    break;

  case 49:
#line 745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 8190 "Parser/parser.cc"
    break;

  case 50:
#line 748 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( new string( "__builtin_va_arg") ) ),
											   (ExpressionNode *)((yyvsp[-4].en)->set_last( (ExpressionNode *)((yyvsp[-1].decl) ? (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) ) : (yyvsp[-2].decl)) )) ) ); }
#line 8197 "Parser/parser.cc"
    break;

  case 51:
#line 751 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 8203 "Parser/parser.cc"
    break;

  case 52:
#line 753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 8209 "Parser/parser.cc"
    break;

  case 53:
#line 755 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), new ExpressionNode( (yyvsp[-2].constant) ) ) ); }
#line 8215 "Parser/parser.cc"
    break;

  case 54:
#line 757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 8221 "Parser/parser.cc"
    break;

  case 55:
#line 759 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 8227 "Parser/parser.cc"
    break;

  case 56:
#line 761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-1].en), build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) ) ) ); }
#line 8233 "Parser/parser.cc"
    break;

  case 57:
#line 763 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 8239 "Parser/parser.cc"
    break;

  case 58:
#line 765 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[0].aggKey), (yyvsp[-2].en) ) ); }
#line 8245 "Parser/parser.cc"
    break;

  case 59:
#line 767 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 8251 "Parser/parser.cc"
    break;

  case 60:
#line 769 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 8257 "Parser/parser.cc"
    break;

  case 61:
#line 771 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 8263 "Parser/parser.cc"
    break;

  case 62:
#line 773 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_val( OperKinds::IncrPost, (yyvsp[-1].en) ) ); }
#line 8269 "Parser/parser.cc"
    break;

  case 63:
#line 775 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_val( OperKinds::DecrPost, (yyvsp[-1].en) ) ); }
#line 8275 "Parser/parser.cc"
    break;

  case 64:
#line 777 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].in), true ) ) ); }
#line 8281 "Parser/parser.cc"
    break;

  case 65:
#line 779 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].in), true ))->set_maybeConstructed( false ) ) ); }
#line 8287 "Parser/parser.cc"
    break;

  case 66:
#line 781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) );
		}
#line 8297 "Parser/parser.cc"
    break;

  case 67:
#line 790 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8303 "Parser/parser.cc"
    break;

  case 70:
#line 797 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 8309 "Parser/parser.cc"
    break;

  case 71:
#line 802 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Default parameter for argument is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8315 "Parser/parser.cc"
    break;

  case 74:
#line 809 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 8321 "Parser/parser.cc"
    break;

  case 76:
#line 815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-1].tok) ) ), maybeMoveBuild( (yyvsp[0].en) ) ) ); }
#line 8327 "Parser/parser.cc"
    break;

  case 77:
#line 817 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-3].tok) ) ), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 8333 "Parser/parser.cc"
    break;

  case 78:
#line 819 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), maybeMoveBuild( (yyvsp[0].en) ) ) ); }
#line 8339 "Parser/parser.cc"
    break;

  case 79:
#line 821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 8345 "Parser/parser.cc"
    break;

  case 80:
#line 823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), maybeMoveBuild( (yyvsp[0].en) ) ) ); }
#line 8351 "Parser/parser.cc"
    break;

  case 81:
#line 825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 8357 "Parser/parser.cc"
    break;

  case 82:
#line 830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_constantInteger( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 8363 "Parser/parser.cc"
    break;

  case 83:
#line 832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_field_name_FLOATINGconstant( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 8369 "Parser/parser.cc"
    break;

  case 84:
#line 834 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_varref( (yyvsp[-1].tok) ), (yyvsp[0].en) ) );
		}
#line 8377 "Parser/parser.cc"
    break;

  case 85:
#line 841 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8383 "Parser/parser.cc"
    break;

  case 86:
#line 843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Expression * constant = build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) );
			(yyval.en) = (yyvsp[-1].en) != nullptr ? new ExpressionNode( build_fieldSel( (yyvsp[-1].en),  constant ) ) : new ExpressionNode( constant );
		}
#line 8392 "Parser/parser.cc"
    break;

  case 89:
#line 855 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 8398 "Parser/parser.cc"
    break;

  case 90:
#line 857 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en)->set_extension( true ); }
#line 8404 "Parser/parser.cc"
    break;

  case 91:
#line 862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			switch ( (yyvsp[-1].op) ) {
			  case OperKinds::AddressOf:
				(yyval.en) = new ExpressionNode( new AddressExpr( maybeMoveBuild( (yyvsp[0].en) ) ) );
				break;
			  case OperKinds::PointTo:
				(yyval.en) = new ExpressionNode( build_unary_val( (yyvsp[-1].op), (yyvsp[0].en) ) );
				break;
			  case OperKinds::And:
				(yyval.en) = new ExpressionNode( new AddressExpr( new AddressExpr( maybeMoveBuild( (yyvsp[0].en) ) ) ) );
				break;
			  default:
				assert( false );
			}
		}
#line 8424 "Parser/parser.cc"
    break;

  case 92:
#line 878 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_val( (yyvsp[-1].op), (yyvsp[0].en) ) ); }
#line 8430 "Parser/parser.cc"
    break;

  case 93:
#line 880 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_val( OperKinds::Incr, (yyvsp[0].en) ) ); }
#line 8436 "Parser/parser.cc"
    break;

  case 94:
#line 882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_val( OperKinds::Decr, (yyvsp[0].en) ) ); }
#line 8442 "Parser/parser.cc"
    break;

  case 95:
#line 884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuild( (yyvsp[0].en) ) ) ); }
#line 8448 "Parser/parser.cc"
    break;

  case 96:
#line 886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8454 "Parser/parser.cc"
    break;

  case 97:
#line 888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuild( (yyvsp[0].en) ) ) ); }
#line 8460 "Parser/parser.cc"
    break;

  case 98:
#line 890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8466 "Parser/parser.cc"
    break;

  case 99:
#line 892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_offsetOf( (yyvsp[-3].decl), build_varref( (yyvsp[-1].tok) ) ) ); }
#line 8472 "Parser/parser.cc"
    break;

  case 100:
#line 894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.en) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 8481 "Parser/parser.cc"
    break;

  case 101:
#line 901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::PointTo; }
#line 8487 "Parser/parser.cc"
    break;

  case 102:
#line 902 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AddressOf; }
#line 8493 "Parser/parser.cc"
    break;

  case 103:
#line 904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::And; }
#line 8499 "Parser/parser.cc"
    break;

  case 104:
#line 908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::UnPlus; }
#line 8505 "Parser/parser.cc"
    break;

  case 105:
#line 909 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::UnMinus; }
#line 8511 "Parser/parser.cc"
    break;

  case 106:
#line 910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::Neg; }
#line 8517 "Parser/parser.cc"
    break;

  case 107:
#line 911 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::BitNeg; }
#line 8523 "Parser/parser.cc"
    break;

  case 109:
#line 917 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cast( (yyvsp[-2].decl), (yyvsp[0].en) ) ); }
#line 8529 "Parser/parser.cc"
    break;

  case 110:
#line 919 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 8535 "Parser/parser.cc"
    break;

  case 111:
#line 921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 8541 "Parser/parser.cc"
    break;

  case 112:
#line 923 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild( (yyvsp[0].en) ), maybeMoveBuildType( nullptr ) ) ); }
#line 8547 "Parser/parser.cc"
    break;

  case 113:
#line 925 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild( (yyvsp[0].en) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 8553 "Parser/parser.cc"
    break;

  case 114:
#line 927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Return cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8559 "Parser/parser.cc"
    break;

  case 115:
#line 929 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8565 "Parser/parser.cc"
    break;

  case 116:
#line 931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8571 "Parser/parser.cc"
    break;

  case 124:
#line 951 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Exp, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8577 "Parser/parser.cc"
    break;

  case 126:
#line 957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mul, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8583 "Parser/parser.cc"
    break;

  case 127:
#line 959 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Div, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8589 "Parser/parser.cc"
    break;

  case 128:
#line 961 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mod, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8595 "Parser/parser.cc"
    break;

  case 130:
#line 967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Plus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8601 "Parser/parser.cc"
    break;

  case 131:
#line 969 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Minus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8607 "Parser/parser.cc"
    break;

  case 133:
#line 975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8613 "Parser/parser.cc"
    break;

  case 134:
#line 977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::RShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8619 "Parser/parser.cc"
    break;

  case 136:
#line 983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8625 "Parser/parser.cc"
    break;

  case 137:
#line 985 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8631 "Parser/parser.cc"
    break;

  case 138:
#line 987 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8637 "Parser/parser.cc"
    break;

  case 139:
#line 989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8643 "Parser/parser.cc"
    break;

  case 141:
#line 995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Eq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8649 "Parser/parser.cc"
    break;

  case 142:
#line 997 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Neq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8655 "Parser/parser.cc"
    break;

  case 144:
#line 1003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitAnd, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8661 "Parser/parser.cc"
    break;

  case 146:
#line 1009 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Xor, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8667 "Parser/parser.cc"
    break;

  case 148:
#line 1015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitOr, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8673 "Parser/parser.cc"
    break;

  case 150:
#line 1021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), true ) ); }
#line 8679 "Parser/parser.cc"
    break;

  case 152:
#line 1027 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), false ) ); }
#line 8685 "Parser/parser.cc"
    break;

  case 154:
#line 1033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8691 "Parser/parser.cc"
    break;

  case 155:
#line 1036 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-3].en), (yyvsp[-3].en), (yyvsp[0].en) ) ); }
#line 8697 "Parser/parser.cc"
    break;

  case 158:
#line 1047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
//			if ( $2 == OperKinds::AtAssn ) {
//				SemanticError( yylloc, "C @= assignment is currently unimplemented." ); $$ = nullptr;
//			} else {
				(yyval.en) = new ExpressionNode( build_binary_val( (yyvsp[-1].op), (yyvsp[-2].en), (yyvsp[0].en) ) );
//			} // if
		}
#line 8709 "Parser/parser.cc"
    break;

  case 159:
#line 1055 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8715 "Parser/parser.cc"
    break;

  case 160:
#line 1060 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8721 "Parser/parser.cc"
    break;

  case 164:
#line 1070 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::Assign; }
#line 8727 "Parser/parser.cc"
    break;

  case 165:
#line 1071 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AtAssn; }
#line 8733 "Parser/parser.cc"
    break;

  case 166:
#line 1075 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ExpAssn; }
#line 8739 "Parser/parser.cc"
    break;

  case 167:
#line 1076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MulAssn; }
#line 8745 "Parser/parser.cc"
    break;

  case 168:
#line 1077 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::DivAssn; }
#line 8751 "Parser/parser.cc"
    break;

  case 169:
#line 1078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ModAssn; }
#line 8757 "Parser/parser.cc"
    break;

  case 170:
#line 1079 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::PlusAssn; }
#line 8763 "Parser/parser.cc"
    break;

  case 171:
#line 1080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MinusAssn; }
#line 8769 "Parser/parser.cc"
    break;

  case 172:
#line 1081 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::LSAssn; }
#line 8775 "Parser/parser.cc"
    break;

  case 173:
#line 1082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::RSAssn; }
#line 8781 "Parser/parser.cc"
    break;

  case 174:
#line 1083 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AndAssn; }
#line 8787 "Parser/parser.cc"
    break;

  case 175:
#line 1084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ERAssn; }
#line 8793 "Parser/parser.cc"
    break;

  case 176:
#line 1085 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::OrAssn; }
#line 8799 "Parser/parser.cc"
    break;

  case 177:
#line 1096 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)(new ExpressionNode( nullptr ) )->set_last( (yyvsp[-1].en) ) ) ); }
#line 8805 "Parser/parser.cc"
    break;

  case 178:
#line 1098 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-4].en)->set_last( (yyvsp[-1].en) ) ) )); }
#line 8811 "Parser/parser.cc"
    break;

  case 180:
#line 1104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8817 "Parser/parser.cc"
    break;

  case 181:
#line 1106 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 8823 "Parser/parser.cc"
    break;

  case 182:
#line 1108 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8829 "Parser/parser.cc"
    break;

  case 184:
#line 1114 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new CommaExpr( maybeMoveBuild( (yyvsp[-2].en) ), maybeMoveBuild( (yyvsp[0].en) ) ) ); }
#line 8835 "Parser/parser.cc"
    break;

  case 185:
#line 1119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8841 "Parser/parser.cc"
    break;

  case 198:
#line 1138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 8847 "Parser/parser.cc"
    break;

  case 200:
#line 1141 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_directive( (yyvsp[0].tok) ) ); }
#line 8853 "Parser/parser.cc"
    break;

  case 201:
#line 1147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[0].sn)->add_label( (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 8859 "Parser/parser.cc"
    break;

  case 202:
#line 1149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "Label \"", *(yyvsp[-3].tok).str, "\" must be associated with a statement, "
											   "where a declaration, case, or default is not a statement. "
											   "Move the label or terminate with a semi-colon." ) );
			(yyval.sn) = nullptr;
		}
#line 8870 "Parser/parser.cc"
    break;

  case 203:
#line 1159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (StatementNode *)0 ) ); }
#line 8876 "Parser/parser.cc"
    break;

  case 204:
#line 1164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (yyvsp[-2].sn) ) ); }
#line 8882 "Parser/parser.cc"
    break;

  case 206:
#line 1170 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 8888 "Parser/parser.cc"
    break;

  case 207:
#line 1175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8894 "Parser/parser.cc"
    break;

  case 208:
#line 1177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8900 "Parser/parser.cc"
    break;

  case 209:
#line 1179 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8906 "Parser/parser.cc"
    break;

  case 210:
#line 1181 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8912 "Parser/parser.cc"
    break;

  case 213:
#line 1188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 8918 "Parser/parser.cc"
    break;

  case 214:
#line 1190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Declarations only allowed at the start of the switch body, i.e., after the '{'." ); (yyval.sn) = nullptr; }
#line 8924 "Parser/parser.cc"
    break;

  case 215:
#line 1195 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_expr( (yyvsp[-1].en) ) ); }
#line 8930 "Parser/parser.cc"
    break;

  case 216:
#line 1202 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn); }
#line 8936 "Parser/parser.cc"
    break;

  case 217:
#line 1204 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( true, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8942 "Parser/parser.cc"
    break;

  case 218:
#line 1206 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( true, (yyvsp[-7].en), (yyvsp[-2].sn) ) );
			// The semantics of the declaration list is changed to include associated initialization, which is performed
			// *before* the transfer to the appropriate case clause by hoisting the declarations into a compound
			// statement around the switch.  Statements after the initial declaration list can never be executed, and
			// therefore, are removed from the grammar even though C allows it. The change also applies to choose
			// statement.
			(yyval.sn) = (yyvsp[-3].decl) ? new StatementNode( build_compound( (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 8956 "Parser/parser.cc"
    break;

  case 219:
#line 1216 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Only declarations can appear before the list of case clauses." ); (yyval.sn) = nullptr; }
#line 8962 "Parser/parser.cc"
    break;

  case 220:
#line 1218 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( false, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8968 "Parser/parser.cc"
    break;

  case 221:
#line 1220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( false, (yyvsp[-7].en), (yyvsp[-2].sn) ) );
			(yyval.sn) = (yyvsp[-3].decl) ? new StatementNode( build_compound( (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 8977 "Parser/parser.cc"
    break;

  case 222:
#line 1225 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Only declarations can appear before the list of case clauses." ); (yyval.sn) = nullptr; }
#line 8983 "Parser/parser.cc"
    break;

  case 223:
#line 1231 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-2].ifctl), maybe_build_compound( (yyvsp[0].sn) ), nullptr ) ); }
#line 8989 "Parser/parser.cc"
    break;

  case 224:
#line 1233 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-4].ifctl), maybe_build_compound( (yyvsp[-2].sn) ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8995 "Parser/parser.cc"
    break;

  case 225:
#line 1238 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( nullptr, (yyvsp[0].en) ); }
#line 9001 "Parser/parser.cc"
    break;

  case 226:
#line 1240 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 9007 "Parser/parser.cc"
    break;

  case 227:
#line 1242 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 9013 "Parser/parser.cc"
    break;

  case 228:
#line 1244 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[-1].decl), (yyvsp[0].en) ); }
#line 9019 "Parser/parser.cc"
    break;

  case 229:
#line 1251 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = (yyvsp[0].en); }
#line 9025 "Parser/parser.cc"
    break;

  case 230:
#line 1253 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild( (yyvsp[-2].en) ), maybeMoveBuild( (yyvsp[0].en) ) ) ); }
#line 9031 "Parser/parser.cc"
    break;

  case 232:
#line 1258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.sn) = new StatementNode( build_case( (yyvsp[0].en) ) ); }
#line 9037 "Parser/parser.cc"
    break;

  case 233:
#line 1260 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)((yyvsp[-2].sn)->set_last( new StatementNode( build_case( (yyvsp[0].en) ) ) ) ); }
#line 9043 "Parser/parser.cc"
    break;

  case 234:
#line 1265 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing case list after case." ); (yyval.sn) = nullptr; }
#line 9049 "Parser/parser.cc"
    break;

  case 235:
#line 1266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn); }
#line 9055 "Parser/parser.cc"
    break;

  case 236:
#line 1268 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing colon after case list." ); (yyval.sn) = nullptr; }
#line 9061 "Parser/parser.cc"
    break;

  case 237:
#line 1269 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.sn) = new StatementNode( build_default() ); }
#line 9067 "Parser/parser.cc"
    break;

  case 238:
#line 1272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing colon after default." ); (yyval.sn) = nullptr; }
#line 9073 "Parser/parser.cc"
    break;

  case 240:
#line 1277 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)( (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) )); }
#line 9079 "Parser/parser.cc"
    break;

  case 241:
#line 1281 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( maybe_build_compound( (yyvsp[0].sn) ) ); }
#line 9085 "Parser/parser.cc"
    break;

  case 242:
#line 1286 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = nullptr; }
#line 9091 "Parser/parser.cc"
    break;

  case 244:
#line 1292 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ); }
#line 9097 "Parser/parser.cc"
    break;

  case 245:
#line 1294 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)( (yyvsp[-2].sn)->set_last( (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ) ) ); }
#line 9103 "Parser/parser.cc"
    break;

  case 246:
#line 1299 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 9109 "Parser/parser.cc"
    break;

  case 247:
#line 1301 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_while( new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( (yyvsp[-2].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9118 "Parser/parser.cc"
    break;

  case 248:
#line 1306 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( (yyvsp[-2].ifctl), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 9124 "Parser/parser.cc"
    break;

  case 249:
#line 1308 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( (yyvsp[-4].ifctl), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[0].sn) ) ); }
#line 9130 "Parser/parser.cc"
    break;

  case 250:
#line 1310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( NEW_ONE, maybe_build_compound( (yyvsp[-4].sn) ) ) ); }
#line 9136 "Parser/parser.cc"
    break;

  case 251:
#line 1312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_do_while( NEW_ONE, maybe_build_compound( (yyvsp[-5].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9145 "Parser/parser.cc"
    break;

  case 252:
#line 1317 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( (yyvsp[-2].en), maybe_build_compound( (yyvsp[-5].sn) ) ) ); }
#line 9151 "Parser/parser.cc"
    break;

  case 253:
#line 1319 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( (yyvsp[-3].en), maybe_build_compound( (yyvsp[-6].sn) ), (yyvsp[0].sn) ) ); }
#line 9157 "Parser/parser.cc"
    break;

  case 254:
#line 1321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 9163 "Parser/parser.cc"
    break;

  case 255:
#line 1323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_for( new ForCtrl( nullptr, nullptr, nullptr ), maybe_build_compound( (yyvsp[-2].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse );
		}
#line 9172 "Parser/parser.cc"
    break;

  case 256:
#line 1328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( (yyvsp[-2].fctl), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 9178 "Parser/parser.cc"
    break;

  case 257:
#line 1330 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( (yyvsp[-4].fctl), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[0].sn) ) ); }
#line 9184 "Parser/parser.cc"
    break;

  case 259:
#line 1340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 9203 "Parser/parser.cc"
    break;

  case 260:
#line 1358 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( nullptr, (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 9209 "Parser/parser.cc"
    break;

  case 261:
#line 1360 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode * init = (yyvsp[-4].en) ? new StatementNode( new ExprStmt( maybeMoveBuild( (yyvsp[-4].en) ) ) ) : nullptr;
			(yyval.fctl) = new ForCtrl( init, (yyvsp[-2].en), (yyvsp[0].en) );
		}
#line 9218 "Parser/parser.cc"
    break;

  case 262:
#line 1365 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( new StatementNode( (yyvsp[-3].decl) ), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 9224 "Parser/parser.cc"
    break;

  case 263:
#line 1368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( nullptr, (yyvsp[0].en), nullptr ); }
#line 9230 "Parser/parser.cc"
    break;

  case 264:
#line 1370 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( nullptr, (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 9236 "Parser/parser.cc"
    break;

  case 265:
#line 1373 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), NEW_ZERO, OperKinds::LThan, (yyvsp[0].en)->clone(), NEW_ONE ); }
#line 9242 "Parser/parser.cc"
    break;

  case 266:
#line 1375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].compop), NEW_ZERO, (yyvsp[0].en)->clone() ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9248 "Parser/parser.cc"
    break;

  case 267:
#line 1378 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].compop), (yyvsp[-2].en)->clone(), (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), (yyvsp[-2].en)->clone() ), NEW_ONE ); }
#line 9254 "Parser/parser.cc"
    break;

  case 268:
#line 1380 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[0].en)->clone(), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 9263 "Parser/parser.cc"
    break;

  case 269:
#line 1385 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
		}
#line 9272 "Parser/parser.cc"
    break;

  case 270:
#line 1390 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en)->clone(), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), (yyvsp[0].en) ); }
#line 9278 "Parser/parser.cc"
    break;

  case 271:
#line 1392 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-2].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].en)->clone(), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 9287 "Parser/parser.cc"
    break;

  case 272:
#line 1397 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
		}
#line 9296 "Parser/parser.cc"
    break;

  case 273:
#line 1402 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 9302 "Parser/parser.cc"
    break;

  case 274:
#line 1404 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 9308 "Parser/parser.cc"
    break;

  case 275:
#line 1406 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 9314 "Parser/parser.cc"
    break;

  case 276:
#line 1408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 9320 "Parser/parser.cc"
    break;

  case 277:
#line 1410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 9326 "Parser/parser.cc"
    break;

  case 278:
#line 1413 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-2].en), NEW_ZERO, OperKinds::LThan, (yyvsp[0].en)->clone(), NEW_ONE ); }
#line 9332 "Parser/parser.cc"
    break;

  case 279:
#line 1415 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-3].en), UPDOWN( (yyvsp[-1].compop), NEW_ZERO, (yyvsp[0].en)->clone() ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9338 "Parser/parser.cc"
    break;

  case 280:
#line 1418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), UPDOWN( (yyvsp[-1].compop), (yyvsp[-2].en)->clone(), (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), (yyvsp[-2].en)->clone() ), NEW_ONE ); }
#line 9344 "Parser/parser.cc"
    break;

  case 281:
#line 1420 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-4].en), (yyvsp[0].en)->clone(), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 9353 "Parser/parser.cc"
    break;

  case 282:
#line 1425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::GThan || (yyvsp[-1].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 9363 "Parser/parser.cc"
    break;

  case 283:
#line 1431 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing low/high value for up/down-to range so index is uninitialized." ); (yyval.fctl) = nullptr; }
#line 9369 "Parser/parser.cc"
    break;

  case 284:
#line 1434 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en)->clone(), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), (yyvsp[0].en) ); }
#line 9375 "Parser/parser.cc"
    break;

  case 285:
#line 1436 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-6].en), (yyvsp[-2].en)->clone(), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 9384 "Parser/parser.cc"
    break;

  case 286:
#line 1441 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 9394 "Parser/parser.cc"
    break;

  case 287:
#line 1447 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en)->clone(), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), nullptr ); }
#line 9400 "Parser/parser.cc"
    break;

  case 288:
#line 1449 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-6].en), (yyvsp[-2].en)->clone(), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 9409 "Parser/parser.cc"
    break;

  case 289:
#line 1454 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 9419 "Parser/parser.cc"
    break;

  case 290:
#line 1460 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing low/high value for up/down-to range so index is uninitialized." ); (yyval.fctl) = nullptr; }
#line 9425 "Parser/parser.cc"
    break;

  case 291:
#line 1463 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-1].decl), NEW_ZERO, OperKinds::LThan, (yyvsp[0].en), NEW_ONE ); }
#line 9431 "Parser/parser.cc"
    break;

  case 292:
#line 1465 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].decl), UPDOWN( (yyvsp[-1].compop), NEW_ZERO, (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9437 "Parser/parser.cc"
    break;

  case 293:
#line 1468 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-3].decl), UPDOWN( (yyvsp[-1].compop), (yyvsp[-2].en)->clone(), (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), (yyvsp[-2].en)->clone() ), NEW_ONE ); }
#line 9443 "Parser/parser.cc"
    break;

  case 294:
#line 1470 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-3].decl), (yyvsp[0].en), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 9452 "Parser/parser.cc"
    break;

  case 295:
#line 1475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::GThan || (yyvsp[-1].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-3].decl), (yyvsp[-2].en), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 9462 "Parser/parser.cc"
    break;

  case 296:
#line 1482 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), (yyvsp[0].en) ); }
#line 9468 "Parser/parser.cc"
    break;

  case 297:
#line 1484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-2].en), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 9477 "Parser/parser.cc"
    break;

  case 298:
#line 1489 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-4].en), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 9487 "Parser/parser.cc"
    break;

  case 299:
#line 1495 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), nullptr ); }
#line 9493 "Parser/parser.cc"
    break;

  case 300:
#line 1497 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-2].en), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 9502 "Parser/parser.cc"
    break;

  case 301:
#line 1502 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-4].en), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 9512 "Parser/parser.cc"
    break;

  case 302:
#line 1508 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing low/high value for up/down-to range so index is uninitialized." ); (yyval.fctl) = nullptr; }
#line 9518 "Parser/parser.cc"
    break;

  case 303:
#line 1511 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.fctl) = nullptr;
			//$$ = forCtrl( new ExpressionNode( build_varref( $3 ) ), $1, nullptr, OperKinds::Range, nullptr, nullptr );
		}
#line 9527 "Parser/parser.cc"
    break;

  case 304:
#line 1516 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LEThan || (yyvsp[-1].compop) == OperKinds::GEThan ) { SemanticError( yylloc, "All enumation ranges are equal (all values). Remove \"=~\"." ); (yyval.fctl) = nullptr; }
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.fctl) = nullptr;
		}
#line 9536 "Parser/parser.cc"
    break;

  case 305:
#line 1524 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GThan; }
#line 9542 "Parser/parser.cc"
    break;

  case 306:
#line 1526 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 9548 "Parser/parser.cc"
    break;

  case 307:
#line 1528 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GEThan; }
#line 9554 "Parser/parser.cc"
    break;

  case 308:
#line 1533 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LThan; }
#line 9560 "Parser/parser.cc"
    break;

  case 309:
#line 1535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GThan; }
#line 9566 "Parser/parser.cc"
    break;

  case 311:
#line 1541 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 9572 "Parser/parser.cc"
    break;

  case 312:
#line 1543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GEThan; }
#line 9578 "Parser/parser.cc"
    break;

  case 313:
#line 1548 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Goto ) ); }
#line 9584 "Parser/parser.cc"
    break;

  case 314:
#line 1552 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_computedgoto( (yyvsp[-1].en) ) ); }
#line 9590 "Parser/parser.cc"
    break;

  case 315:
#line 1555 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThrough ) ); }
#line 9596 "Parser/parser.cc"
    break;

  case 316:
#line 1557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::FallThrough ) ); }
#line 9602 "Parser/parser.cc"
    break;

  case 317:
#line 1559 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThroughDefault ) ); }
#line 9608 "Parser/parser.cc"
    break;

  case 318:
#line 1562 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Continue ) ); }
#line 9614 "Parser/parser.cc"
    break;

  case 319:
#line 1566 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Continue ) ); }
#line 9620 "Parser/parser.cc"
    break;

  case 320:
#line 1569 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Break ) ); }
#line 9626 "Parser/parser.cc"
    break;

  case 321:
#line 1573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Break ) ); }
#line 9632 "Parser/parser.cc"
    break;

  case 322:
#line 1575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_return( (yyvsp[-1].en) ) ); }
#line 9638 "Parser/parser.cc"
    break;

  case 323:
#line 1577 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 9644 "Parser/parser.cc"
    break;

  case 324:
#line 1579 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr ) ); }
#line 9650 "Parser/parser.cc"
    break;

  case 325:
#line 1581 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn) ) ); }
#line 9656 "Parser/parser.cc"
    break;

  case 326:
#line 1583 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Coroutine ) ); }
#line 9662 "Parser/parser.cc"
    break;

  case 327:
#line 1585 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Coroutine ) ); }
#line 9668 "Parser/parser.cc"
    break;

  case 328:
#line 1587 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Generator ) ); }
#line 9674 "Parser/parser.cc"
    break;

  case 329:
#line 1589 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Generator ) ); }
#line 9680 "Parser/parser.cc"
    break;

  case 330:
#line 1591 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_throw( (yyvsp[-1].en) ) ); }
#line 9686 "Parser/parser.cc"
    break;

  case 331:
#line 1593 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume( (yyvsp[-1].en) ) ); }
#line 9692 "Parser/parser.cc"
    break;

  case 332:
#line 1595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume_at( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 9698 "Parser/parser.cc"
    break;

  case 335:
#line 1605 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_with( (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 9704 "Parser/parser.cc"
    break;

  case 336:
#line 1611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-2].en) ) { SemanticError( yylloc, "mutex argument list cannot be empty." ); (yyval.sn) = nullptr; }
			(yyval.sn) = new StatementNode( build_mutex( (yyvsp[-2].en), (yyvsp[0].sn) ) );
		}
#line 9713 "Parser/parser.cc"
    break;

  case 337:
#line 1618 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 9719 "Parser/parser.cc"
    break;

  case 338:
#line 1623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9725 "Parser/parser.cc"
    break;

  case 341:
#line 1631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.en) = nullptr; }
#line 9731 "Parser/parser.cc"
    break;

  case 342:
#line 1635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 9737 "Parser/parser.cc"
    break;

  case 345:
#line 1644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 9743 "Parser/parser.cc"
    break;

  case 346:
#line 1646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) )); }
#line 9749 "Parser/parser.cc"
    break;

  case 347:
#line 1652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( new WaitForStmt(), (yyvsp[-2].en), (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ) ); }
#line 9755 "Parser/parser.cc"
    break;

  case 348:
#line 1654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-4].wfs), (yyvsp[-2].en), (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ) ); }
#line 9761 "Parser/parser.cc"
    break;

  case 349:
#line 1656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( (yyvsp[-4].wfs), (yyvsp[-2].en), maybe_build_compound( (yyvsp[0].sn) ) ); }
#line 9767 "Parser/parser.cc"
    break;

  case 350:
#line 1658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-4].wfs), (yyvsp[-2].en), (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ) ); }
#line 9773 "Parser/parser.cc"
    break;

  case 351:
#line 1661 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 9779 "Parser/parser.cc"
    break;

  case 352:
#line 1663 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_else( build_waitfor_timeout( (yyvsp[-8].wfs), (yyvsp[-6].en), (yyvsp[-5].en), maybe_build_compound( (yyvsp[-4].sn) ) ), (yyvsp[-2].en), maybe_build_compound( (yyvsp[0].sn) ) ); }
#line 9785 "Parser/parser.cc"
    break;

  case 353:
#line 1667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].wfs) ); }
#line 9791 "Parser/parser.cc"
    break;

  case 356:
#line 1677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 9797 "Parser/parser.cc"
    break;

  case 357:
#line 1682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { printf( "waituntil_clause 1\n" ); (yyval.wfs) = nullptr; }
#line 9803 "Parser/parser.cc"
    break;

  case 358:
#line 1684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { printf( "waituntil_clause 2\n" ); (yyval.wfs) = nullptr; }
#line 9809 "Parser/parser.cc"
    break;

  case 359:
#line 1689 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { printf( "wand_waituntil_clause 1\n" ); (yyval.wfs) = nullptr; }
#line 9815 "Parser/parser.cc"
    break;

  case 360:
#line 1691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { printf( "wand_waituntil_clause 2\n" ); (yyval.wfs) = nullptr; }
#line 9821 "Parser/parser.cc"
    break;

  case 361:
#line 1696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { printf( "wor_waituntil_clause 1\n" ); (yyval.wfs) = nullptr; }
#line 9827 "Parser/parser.cc"
    break;

  case 362:
#line 1698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { printf( "wor_waituntil_clause 2\n" ); (yyval.wfs) = nullptr; }
#line 9833 "Parser/parser.cc"
    break;

  case 363:
#line 1700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { printf( "wor_waituntil_clause 3\n" ); (yyval.wfs) = nullptr; }
#line 9839 "Parser/parser.cc"
    break;

  case 364:
#line 1702 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { printf( "wor_waituntil_clause 4\n" ); (yyval.wfs) = nullptr; }
#line 9845 "Parser/parser.cc"
    break;

  case 365:
#line 1705 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 9851 "Parser/parser.cc"
    break;

  case 366:
#line 1707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { printf( "wor_waituntil_clause 6\n" ); (yyval.wfs) = nullptr; }
#line 9857 "Parser/parser.cc"
    break;

  case 367:
#line 1713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (StatementNode *)0 ) ); }
#line 9863 "Parser/parser.cc"
    break;

  case 368:
#line 1718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), (yyvsp[0].sn), nullptr ) ); }
#line 9869 "Parser/parser.cc"
    break;

  case 369:
#line 1720 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), nullptr, (yyvsp[0].sn) ) ); }
#line 9875 "Parser/parser.cc"
    break;

  case 370:
#line 1722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-2].sn), (yyvsp[-1].sn), (yyvsp[0].sn) ) ); }
#line 9881 "Parser/parser.cc"
    break;

  case 371:
#line 1727 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 9887 "Parser/parser.cc"
    break;

  case 372:
#line 1729 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)(yyvsp[-8].sn)->set_last( new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ) ); }
#line 9893 "Parser/parser.cc"
    break;

  case 373:
#line 1734 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9899 "Parser/parser.cc"
    break;

  case 374:
#line 1735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[0].en); }
#line 9905 "Parser/parser.cc"
    break;

  case 375:
#line 1739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 9911 "Parser/parser.cc"
    break;

  case 376:
#line 1740 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 9917 "Parser/parser.cc"
    break;

  case 377:
#line 1741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.catch_kind) = CatchStmt::Resume; }
#line 9923 "Parser/parser.cc"
    break;

  case 378:
#line 1742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Resume; }
#line 9929 "Parser/parser.cc"
    break;

  case 379:
#line 1746 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = new StatementNode( build_finally( (yyvsp[0].sn) ) ); }
#line 9935 "Parser/parser.cc"
    break;

  case 381:
#line 1753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 9941 "Parser/parser.cc"
    break;

  case 382:
#line 1755 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 9947 "Parser/parser.cc"
    break;

  case 383:
#line 1757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 9953 "Parser/parser.cc"
    break;

  case 388:
#line 1772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-4].flag), (yyvsp[-2].constant), nullptr ) ); }
#line 9959 "Parser/parser.cc"
    break;

  case 389:
#line 1774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-6].flag), (yyvsp[-4].constant), (yyvsp[-2].en) ) ); }
#line 9965 "Parser/parser.cc"
    break;

  case 390:
#line 1776 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-8].flag), (yyvsp[-6].constant), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 9971 "Parser/parser.cc"
    break;

  case 391:
#line 1778 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-10].flag), (yyvsp[-8].constant), (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 9977 "Parser/parser.cc"
    break;

  case 392:
#line 1780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-12].flag), (yyvsp[-9].constant), nullptr, (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].label) ) ); }
#line 9983 "Parser/parser.cc"
    break;

  case 393:
#line 1785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = false; }
#line 9989 "Parser/parser.cc"
    break;

  case 394:
#line 1787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = true; }
#line 9995 "Parser/parser.cc"
    break;

  case 395:
#line 1792 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 10001 "Parser/parser.cc"
    break;

  case 398:
#line 1799 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 10007 "Parser/parser.cc"
    break;

  case 399:
#line 1804 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( nullptr, (yyvsp[-3].constant), maybeMoveBuild( (yyvsp[-1].en) ) ) ); }
#line 10013 "Parser/parser.cc"
    break;

  case 400:
#line 1806 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( (yyvsp[-5].tok), (yyvsp[-3].constant), maybeMoveBuild( (yyvsp[-1].en) ) ) ); }
#line 10019 "Parser/parser.cc"
    break;

  case 401:
#line 1811 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 10025 "Parser/parser.cc"
    break;

  case 402:
#line 1813 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 10031 "Parser/parser.cc"
    break;

  case 403:
#line 1815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( (yyvsp[0].constant) ) )); }
#line 10037 "Parser/parser.cc"
    break;

  case 404:
#line 1820 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = new LabelNode(); (yyval.label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 10046 "Parser/parser.cc"
    break;

  case 405:
#line 1825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = (yyvsp[-2].label); (yyvsp[-2].label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 10055 "Parser/parser.cc"
    break;

  case 406:
#line 1835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10061 "Parser/parser.cc"
    break;

  case 409:
#line 1842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 10067 "Parser/parser.cc"
    break;

  case 410:
#line 1847 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10073 "Parser/parser.cc"
    break;

  case 412:
#line 1853 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 10079 "Parser/parser.cc"
    break;

  case 413:
#line 1855 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-2].decl) ); }
#line 10085 "Parser/parser.cc"
    break;

  case 420:
#line 1875 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "C_DECLARATION1 %p %s\n", $$, $$->name ? $$->name->c_str() : "(nil)" );
			// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10096 "Parser/parser.cc"
    break;

  case 423:
#line 1887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-4].en), (yyvsp[-2].constant) ); }
#line 10102 "Parser/parser.cc"
    break;

  case 424:
#line 1889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-2].en), build_constantStr( *new string( "\"\"" ) ) ); }
#line 10108 "Parser/parser.cc"
    break;

  case 428:
#line 1907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10114 "Parser/parser.cc"
    break;

  case 430:
#line 1913 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].in) ); }
#line 10120 "Parser/parser.cc"
    break;

  case 431:
#line 1917 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 10126 "Parser/parser.cc"
    break;

  case 432:
#line 1919 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].in) ) ); }
#line 10132 "Parser/parser.cc"
    break;

  case 433:
#line 1926 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10138 "Parser/parser.cc"
    break;

  case 434:
#line 1928 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10144 "Parser/parser.cc"
    break;

  case 435:
#line 1930 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 10150 "Parser/parser.cc"
    break;

  case 437:
#line 1936 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10156 "Parser/parser.cc"
    break;

  case 438:
#line 1938 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10162 "Parser/parser.cc"
    break;

  case 439:
#line 1940 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 10168 "Parser/parser.cc"
    break;

  case 440:
#line 1942 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeClone( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->appendList( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 10179 "Parser/parser.cc"
    break;

  case 441:
#line 1975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10185 "Parser/parser.cc"
    break;

  case 442:
#line 1977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), nullptr )->addQualifiers( (yyvsp[0].decl) ); }
#line 10191 "Parser/parser.cc"
    break;

  case 443:
#line 1982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 10197 "Parser/parser.cc"
    break;

  case 444:
#line 1985 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-6].decl)->appendList( (yyvsp[-2].decl) ) ); }
#line 10203 "Parser/parser.cc"
    break;

  case 445:
#line 1990 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 10212 "Parser/parser.cc"
    break;

  case 446:
#line 1995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 10221 "Parser/parser.cc"
    break;

  case 447:
#line 2000 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "3" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 10230 "Parser/parser.cc"
    break;

  case 448:
#line 2011 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "4" );
			if ( (yyvsp[-1].decl)->type->forall || ((yyvsp[-1].decl)->type->kind == TypeData::Aggregate && (yyvsp[-1].decl)->type->aggregate.params) ) {
				SemanticError( yylloc, "forall qualifier in typedef is currently unimplemented." ); (yyval.decl) = nullptr;
			} else (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef(); // watchout frees $2 and $3
		}
#line 10241 "Parser/parser.cc"
    break;

  case 449:
#line 2018 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "5" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 10250 "Parser/parser.cc"
    break;

  case 450:
#line 2023 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10256 "Parser/parser.cc"
    break;

  case 451:
#line 2025 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10262 "Parser/parser.cc"
    break;

  case 452:
#line 2027 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type qualifiers/specifiers before TYPEDEF is deprecated, move after TYPEDEF." ); (yyval.decl) = nullptr; }
#line 10268 "Parser/parser.cc"
    break;

  case 453:
#line 2033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 10276 "Parser/parser.cc"
    break;

  case 454:
#line 2037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "TYPEDEF expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 10284 "Parser/parser.cc"
    break;

  case 455:
#line 2044 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 10290 "Parser/parser.cc"
    break;

  case 458:
#line 2048 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			assert( (yyvsp[0].decl)->type );
			if ( (yyvsp[0].decl)->type->qualifiers.any() ) {			// CV qualifiers ?
				SemanticError( yylloc, "Useless type qualifier(s) in empty declaration." ); (yyval.decl) = nullptr;
			}
			// enums are never empty declarations because there must have at least one enumeration.
			if ( (yyvsp[0].decl)->type->kind == TypeData::AggregateInst && (yyvsp[0].decl)->storageClasses.any() ) { // storage class ?
				SemanticError( yylloc, "Useless storage qualifier(s) in empty aggregate declaration." ); (yyval.decl) = nullptr;
			}
		}
#line 10305 "Parser/parser.cc"
    break;

  case 459:
#line 2064 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 10311 "Parser/parser.cc"
    break;

  case 460:
#line 2066 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 10317 "Parser/parser.cc"
    break;

  case 461:
#line 2069 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAsmName( (yyvsp[0].decl) )->addInitializer( nullptr ); }
#line 10323 "Parser/parser.cc"
    break;

  case 462:
#line 2071 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAsmName( (yyvsp[-2].decl) )->addInitializer( new InitializerNode( true ) ); }
#line 10329 "Parser/parser.cc"
    break;

  case 463:
#line 2074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ) ); }
#line 10335 "Parser/parser.cc"
    break;

  case 469:
#line 2087 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "Missing ';' after end of ",
				(yyvsp[-1].decl)->type->enumeration.name ? "enum" : AggregateDecl::aggrString( (yyvsp[-1].decl)->type->aggregate.kind ),
				" declaration" ) );
			(yyval.decl) = nullptr;
		}
#line 10346 "Parser/parser.cc"
    break;

  case 482:
#line 2131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10352 "Parser/parser.cc"
    break;

  case 485:
#line 2143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10358 "Parser/parser.cc"
    break;

  case 488:
#line 2153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Const ); }
#line 10364 "Parser/parser.cc"
    break;

  case 489:
#line 2155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Restrict ); }
#line 10370 "Parser/parser.cc"
    break;

  case 490:
#line 2157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Volatile ); }
#line 10376 "Parser/parser.cc"
    break;

  case 491:
#line 2159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Atomic ); }
#line 10382 "Parser/parser.cc"
    break;

  case 492:
#line 2161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newForall( (yyvsp[0].decl) ); }
#line 10388 "Parser/parser.cc"
    break;

  case 493:
#line 2166 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10394 "Parser/parser.cc"
    break;

  case 495:
#line 2172 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10400 "Parser/parser.cc"
    break;

  case 496:
#line 2174 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10406 "Parser/parser.cc"
    break;

  case 498:
#line 2185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10412 "Parser/parser.cc"
    break;

  case 499:
#line 2190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Extern ); }
#line 10418 "Parser/parser.cc"
    break;

  case 500:
#line 2192 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Static ); }
#line 10424 "Parser/parser.cc"
    break;

  case 501:
#line 2194 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Auto ); }
#line 10430 "Parser/parser.cc"
    break;

  case 502:
#line 2196 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Register ); }
#line 10436 "Parser/parser.cc"
    break;

  case 503:
#line 2198 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::ThreadlocalGcc ); }
#line 10442 "Parser/parser.cc"
    break;

  case 504:
#line 2200 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::ThreadlocalC11 ); }
#line 10448 "Parser/parser.cc"
    break;

  case 505:
#line 2203 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Inline ); }
#line 10454 "Parser/parser.cc"
    break;

  case 506:
#line 2205 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Fortran ); }
#line 10460 "Parser/parser.cc"
    break;

  case 507:
#line 2207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Noreturn ); }
#line 10466 "Parser/parser.cc"
    break;

  case 508:
#line 2212 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 10472 "Parser/parser.cc"
    break;

  case 509:
#line 2214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Bool ); }
#line 10478 "Parser/parser.cc"
    break;

  case 510:
#line 2216 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Char ); }
#line 10484 "Parser/parser.cc"
    break;

  case 511:
#line 2218 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int ); }
#line 10490 "Parser/parser.cc"
    break;

  case 512:
#line 2220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 ); }
#line 10496 "Parser/parser.cc"
    break;

  case 513:
#line 2222 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 )->addType( DeclarationNode::newSignedNess( DeclarationNode::Unsigned ) ); }
#line 10502 "Parser/parser.cc"
    break;

  case 514:
#line 2224 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Float ); }
#line 10508 "Parser/parser.cc"
    break;

  case 515:
#line 2226 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Double ); }
#line 10514 "Parser/parser.cc"
    break;

  case 516:
#line 2228 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat80 ); }
#line 10520 "Parser/parser.cc"
    break;

  case 517:
#line 2230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat128 ); }
#line 10526 "Parser/parser.cc"
    break;

  case 518:
#line 2232 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat16 ); }
#line 10532 "Parser/parser.cc"
    break;

  case 519:
#line 2234 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32 ); }
#line 10538 "Parser/parser.cc"
    break;

  case 520:
#line 2236 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32x ); }
#line 10544 "Parser/parser.cc"
    break;

  case 521:
#line 2238 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64 ); }
#line 10550 "Parser/parser.cc"
    break;

  case 522:
#line 2240 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64x ); }
#line 10556 "Parser/parser.cc"
    break;

  case 523:
#line 2242 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat128 ); }
#line 10562 "Parser/parser.cc"
    break;

  case 524:
#line 2244 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10568 "Parser/parser.cc"
    break;

  case 525:
#line 2246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10574 "Parser/parser.cc"
    break;

  case 526:
#line 2248 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10580 "Parser/parser.cc"
    break;

  case 527:
#line 2250 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Complex ); }
#line 10586 "Parser/parser.cc"
    break;

  case 528:
#line 2252 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Imaginary ); }
#line 10592 "Parser/parser.cc"
    break;

  case 529:
#line 2254 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Signed ); }
#line 10598 "Parser/parser.cc"
    break;

  case 530:
#line 2256 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Unsigned ); }
#line 10604 "Parser/parser.cc"
    break;

  case 531:
#line 2258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Short ); }
#line 10610 "Parser/parser.cc"
    break;

  case 532:
#line 2260 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Long ); }
#line 10616 "Parser/parser.cc"
    break;

  case 533:
#line 2262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Valist ); }
#line 10622 "Parser/parser.cc"
    break;

  case 534:
#line 2264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::AutoType ); }
#line 10628 "Parser/parser.cc"
    break;

  case 536:
#line 2270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10634 "Parser/parser.cc"
    break;

  case 538:
#line 2276 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVtableType( (yyvsp[-2].decl) ); }
#line 10640 "Parser/parser.cc"
    break;

  case 539:
#line 2282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10646 "Parser/parser.cc"
    break;

  case 540:
#line 2284 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10652 "Parser/parser.cc"
    break;

  case 542:
#line 2291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10658 "Parser/parser.cc"
    break;

  case 543:
#line 2293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10664 "Parser/parser.cc"
    break;

  case 544:
#line 2295 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10670 "Parser/parser.cc"
    break;

  case 545:
#line 2297 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 10676 "Parser/parser.cc"
    break;

  case 547:
#line 2304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10682 "Parser/parser.cc"
    break;

  case 549:
#line 2310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10688 "Parser/parser.cc"
    break;

  case 550:
#line 2312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10694 "Parser/parser.cc"
    break;

  case 551:
#line 2314 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 10700 "Parser/parser.cc"
    break;

  case 552:
#line 2319 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10706 "Parser/parser.cc"
    break;

  case 553:
#line 2321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en) ); }
#line 10712 "Parser/parser.cc"
    break;

  case 554:
#line 2323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 10718 "Parser/parser.cc"
    break;

  case 555:
#line 2325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en), true ); }
#line 10724 "Parser/parser.cc"
    break;

  case 556:
#line 2327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Zero ); }
#line 10730 "Parser/parser.cc"
    break;

  case 557:
#line 2329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::One ); }
#line 10736 "Parser/parser.cc"
    break;

  case 558:
#line 2334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_declaration_specifier %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10747 "Parser/parser.cc"
    break;

  case 559:
#line 2341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10753 "Parser/parser.cc"
    break;

  case 560:
#line 2343 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10759 "Parser/parser.cc"
    break;

  case 561:
#line 2345 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10765 "Parser/parser.cc"
    break;

  case 562:
#line 2350 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_type_specifier %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10776 "Parser/parser.cc"
    break;

  case 563:
#line 2357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 10782 "Parser/parser.cc"
    break;

  case 564:
#line 2359 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10788 "Parser/parser.cc"
    break;

  case 565:
#line 2361 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 10797 "Parser/parser.cc"
    break;

  case 567:
#line 2370 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10803 "Parser/parser.cc"
    break;

  case 568:
#line 2372 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10809 "Parser/parser.cc"
    break;

  case 569:
#line 2374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10815 "Parser/parser.cc"
    break;

  case 571:
#line 2380 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10821 "Parser/parser.cc"
    break;

  case 572:
#line 2382 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10827 "Parser/parser.cc"
    break;

  case 574:
#line 2388 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10833 "Parser/parser.cc"
    break;

  case 575:
#line 2390 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10839 "Parser/parser.cc"
    break;

  case 576:
#line 2392 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10845 "Parser/parser.cc"
    break;

  case 578:
#line 2398 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10851 "Parser/parser.cc"
    break;

  case 579:
#line 2400 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10857 "Parser/parser.cc"
    break;

  case 580:
#line 2405 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypedef( (yyvsp[0].tok) ); }
#line 10863 "Parser/parser.cc"
    break;

  case 581:
#line 2407 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 10869 "Parser/parser.cc"
    break;

  case 582:
#line 2409 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 10875 "Parser/parser.cc"
    break;

  case 584:
#line 2412 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), (yyvsp[0].decl) ); }
#line 10881 "Parser/parser.cc"
    break;

  case 585:
#line 2414 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), (yyvsp[0].decl) ); }
#line 10887 "Parser/parser.cc"
    break;

  case 586:
#line 2419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[0].tok), nullptr ); }
#line 10893 "Parser/parser.cc"
    break;

  case 587:
#line 2421 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-2].tok), nullptr ); }
#line 10899 "Parser/parser.cc"
    break;

  case 588:
#line 2423 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 10905 "Parser/parser.cc"
    break;

  case 589:
#line 2428 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "elaborated_type %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10916 "Parser/parser.cc"
    break;

  case 593:
#line 2444 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 10922 "Parser/parser.cc"
    break;

  case 594:
#line 2446 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 10928 "Parser/parser.cc"
    break;

  case 595:
#line 2448 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 10937 "Parser/parser.cc"
    break;

  case 596:
#line 2453 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 10945 "Parser/parser.cc"
    break;

  case 597:
#line 2457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 10954 "Parser/parser.cc"
    break;

  case 598:
#line 2462 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypedef( (yyvsp[-5].tok) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 10963 "Parser/parser.cc"
    break;

  case 599:
#line 2467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 10972 "Parser/parser.cc"
    break;

  case 600:
#line 2472 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode::newFromTypeGen( (yyvsp[-5].tok), nullptr );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 10981 "Parser/parser.cc"
    break;

  case 602:
#line 2481 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 10987 "Parser/parser.cc"
    break;

  case 603:
#line 2483 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 10993 "Parser/parser.cc"
    break;

  case 604:
#line 2488 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 11003 "Parser/parser.cc"
    break;

  case 605:
#line 2494 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			forall = false;								// reset
			// Create new generic declaration with same name as previous forward declaration, where the IDENTIFIER is
			// switched to a TYPEGENname. Link any generic arguments from typegen_name to new generic declaration and
			// delete newFromTypeGen.
			if ( (yyvsp[0].decl)->type->kind == TypeData::SymbolicInst && ! (yyvsp[0].decl)->type->symbolic.isTypedef ) {
				(yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) );
			} else {
				(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].decl)->type->symbolic.name, (yyvsp[0].decl)->type->symbolic.actuals, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
				(yyvsp[0].decl)->type->symbolic.name = nullptr;			// copied to $$
				(yyvsp[0].decl)->type->symbolic.actuals = nullptr;
				delete (yyvsp[0].decl);
			}
		}
#line 11022 "Parser/parser.cc"
    break;

  case 608:
#line 2517 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Struct; }
#line 11028 "Parser/parser.cc"
    break;

  case 609:
#line 2519 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Union; }
#line 11034 "Parser/parser.cc"
    break;

  case 610:
#line 2521 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Exception; }
#line 11040 "Parser/parser.cc"
    break;

  case 611:
#line 2527 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 11046 "Parser/parser.cc"
    break;

  case 612:
#line 2529 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 11052 "Parser/parser.cc"
    break;

  case 613:
#line 2531 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Generator; }
#line 11058 "Parser/parser.cc"
    break;

  case 614:
#line 2533 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor generator is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 11064 "Parser/parser.cc"
    break;

  case 615:
#line 2535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Coroutine; }
#line 11070 "Parser/parser.cc"
    break;

  case 616:
#line 2537 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor coroutine is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 11076 "Parser/parser.cc"
    break;

  case 617:
#line 2539 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Thread; }
#line 11082 "Parser/parser.cc"
    break;

  case 618:
#line 2541 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor thread is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 11088 "Parser/parser.cc"
    break;

  case 619:
#line 2546 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11094 "Parser/parser.cc"
    break;

  case 620:
#line 2548 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 11100 "Parser/parser.cc"
    break;

  case 621:
#line 2553 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "type_specifier1 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			(yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) );
			// printf( "type_specifier2 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 11113 "Parser/parser.cc"
    break;

  case 622:
#line 2562 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 11119 "Parser/parser.cc"
    break;

  case 623:
#line 2564 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "STATIC aggregate field qualifier currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11125 "Parser/parser.cc"
    break;

  case 624:
#line 2566 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-1].decl) ) {								// field declarator ?
				(yyvsp[-1].decl) = DeclarationNode::newName( nullptr );
			} // if
			(yyvsp[-1].decl)->inLine = true;
			(yyval.decl) = distAttr( (yyvsp[-2].decl), (yyvsp[-1].decl) );					// mark all fields in list
			distInl( (yyvsp[-1].decl) );
		}
#line 11138 "Parser/parser.cc"
    break;

  case 625:
#line 2575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 11144 "Parser/parser.cc"
    break;

  case 628:
#line 2579 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 11150 "Parser/parser.cc"
    break;

  case 629:
#line 2581 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11156 "Parser/parser.cc"
    break;

  case 632:
#line 2588 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11162 "Parser/parser.cc"
    break;

  case 634:
#line 2591 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 11168 "Parser/parser.cc"
    break;

  case 635:
#line 2596 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].en) ); }
#line 11174 "Parser/parser.cc"
    break;

  case 636:
#line 2599 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 11180 "Parser/parser.cc"
    break;

  case 637:
#line 2602 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 11186 "Parser/parser.cc"
    break;

  case 638:
#line 2605 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 11192 "Parser/parser.cc"
    break;

  case 639:
#line 2610 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11198 "Parser/parser.cc"
    break;

  case 641:
#line 2613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 11204 "Parser/parser.cc"
    break;

  case 643:
#line 2624 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 11210 "Parser/parser.cc"
    break;

  case 644:
#line 2626 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 11216 "Parser/parser.cc"
    break;

  case 646:
#line 2633 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 11222 "Parser/parser.cc"
    break;

  case 647:
#line 2638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 11228 "Parser/parser.cc"
    break;

  case 649:
#line 2644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 11234 "Parser/parser.cc"
    break;

  case 650:
#line 2649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, false )->addQualifiers( (yyvsp[-4].decl) ); }
#line 11240 "Parser/parser.cc"
    break;

  case 651:
#line 2651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); }
#line 11246 "Parser/parser.cc"
    break;

  case 652:
#line 2653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].hide) )->addQualifiers( (yyvsp[-7].decl) ); }
#line 11252 "Parser/parser.cc"
    break;

  case 653:
#line 2656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].decl)->name, (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].hide) )->addQualifiers( (yyvsp[-6].decl) ); }
#line 11258 "Parser/parser.cc"
    break;

  case 654:
#line 2658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-6].decl)->storageClasses.val != 0 || (yyvsp[-6].decl)->type->qualifiers.any() )
			{ SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }

			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true, (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 11269 "Parser/parser.cc"
    break;

  case 655:
#line 2665 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 11277 "Parser/parser.cc"
    break;

  case 656:
#line 2669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].decl)->storageClasses.any() || (yyvsp[-4].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			typedefTable.makeTypedef( *(yyvsp[-1].tok) );
		}
#line 11286 "Parser/parser.cc"
    break;

  case 657:
#line 2674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-7].tok), (yyvsp[-2].decl), true, true, (yyvsp[-10].decl), (yyvsp[-4].hide) )->addQualifiers( (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 11294 "Parser/parser.cc"
    break;

  case 658:
#line 2679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, true, nullptr, (yyvsp[-4].hide) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 11302 "Parser/parser.cc"
    break;

  case 659:
#line 2684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].decl)->name, (yyvsp[-2].decl), true, true, (yyvsp[-9].decl), (yyvsp[-4].hide) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 11310 "Parser/parser.cc"
    break;

  case 660:
#line 2689 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].decl)->name, (yyvsp[-2].decl), true, true, nullptr, (yyvsp[-4].hide) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 11318 "Parser/parser.cc"
    break;

  case 662:
#line 2697 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.hide) = EnumHiding::Visible; }
#line 11324 "Parser/parser.cc"
    break;

  case 663:
#line 2699 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.hide) = EnumHiding::Hide; }
#line 11330 "Parser/parser.cc"
    break;

  case 664:
#line 2704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); (yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 11336 "Parser/parser.cc"
    break;

  case 665:
#line 2706 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].decl)->type->symbolic.name );	(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].decl)->type->symbolic.name, nullptr, false, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 11342 "Parser/parser.cc"
    break;

  case 666:
#line 2711 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].in) ); }
#line 11348 "Parser/parser.cc"
    break;

  case 667:
#line 2713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumInLine( *(yyvsp[0].decl)->type->symbolic.name ); }
#line 11354 "Parser/parser.cc"
    break;

  case 668:
#line 2715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].in) ) ); }
#line 11360 "Parser/parser.cc"
    break;

  case 669:
#line 2717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( DeclarationNode::newEnumValueGeneric( new string("inline"), nullptr ) ); }
#line 11366 "Parser/parser.cc"
    break;

  case 671:
#line 2723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.hide) = EnumHiding::Visible; }
#line 11372 "Parser/parser.cc"
    break;

  case 672:
#line 2728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 11378 "Parser/parser.cc"
    break;

  case 673:
#line 2729 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = new InitializerNode( (yyvsp[0].en) ); }
#line 11384 "Parser/parser.cc"
    break;

  case 674:
#line 2730 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                     { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 11390 "Parser/parser.cc"
    break;

  case 675:
#line 2737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 11396 "Parser/parser.cc"
    break;

  case 676:
#line 2739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11402 "Parser/parser.cc"
    break;

  case 679:
#line 2743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11408 "Parser/parser.cc"
    break;

  case 680:
#line 2745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 11414 "Parser/parser.cc"
    break;

  case 681:
#line 2747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 11420 "Parser/parser.cc"
    break;

  case 683:
#line 2755 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11426 "Parser/parser.cc"
    break;

  case 684:
#line 2757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11432 "Parser/parser.cc"
    break;

  case 685:
#line 2759 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-8].decl)->appendList( (yyvsp[-4].decl) )->appendList( (yyvsp[0].decl) ); }
#line 11438 "Parser/parser.cc"
    break;

  case 687:
#line 2765 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11444 "Parser/parser.cc"
    break;

  case 688:
#line 2770 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11450 "Parser/parser.cc"
    break;

  case 689:
#line 2772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11456 "Parser/parser.cc"
    break;

  case 691:
#line 2775 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 11462 "Parser/parser.cc"
    break;

  case 694:
#line 2782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11468 "Parser/parser.cc"
    break;

  case 695:
#line 2784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11474 "Parser/parser.cc"
    break;

  case 697:
#line 2793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 11480 "Parser/parser.cc"
    break;

  case 698:
#line 2796 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 11486 "Parser/parser.cc"
    break;

  case 699:
#line 2798 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 11492 "Parser/parser.cc"
    break;

  case 704:
#line 2808 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11498 "Parser/parser.cc"
    break;

  case 706:
#line 2815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 11504 "Parser/parser.cc"
    break;

  case 707:
#line 2817 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 11510 "Parser/parser.cc"
    break;

  case 708:
#line 2822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 11516 "Parser/parser.cc"
    break;

  case 709:
#line 2824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 11522 "Parser/parser.cc"
    break;

  case 710:
#line 2833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11528 "Parser/parser.cc"
    break;

  case 711:
#line 2835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 11534 "Parser/parser.cc"
    break;

  case 717:
#line 2848 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 11540 "Parser/parser.cc"
    break;

  case 720:
#line 2858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 11546 "Parser/parser.cc"
    break;

  case 721:
#line 2859 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (yyvsp[-1].op) == OperKinds::Assign ? (yyvsp[0].in) : (yyvsp[0].in)->set_maybeConstructed( false ); }
#line 11552 "Parser/parser.cc"
    break;

  case 722:
#line 2860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.in) = new InitializerNode( true ); }
#line 11558 "Parser/parser.cc"
    break;

  case 723:
#line 2861 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 11564 "Parser/parser.cc"
    break;

  case 724:
#line 2865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = new InitializerNode( (yyvsp[0].en) ); }
#line 11570 "Parser/parser.cc"
    break;

  case 725:
#line 2866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 11576 "Parser/parser.cc"
    break;

  case 726:
#line 2871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 11582 "Parser/parser.cc"
    break;

  case 728:
#line 2873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ); }
#line 11588 "Parser/parser.cc"
    break;

  case 729:
#line 2874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (InitializerNode *)( (yyvsp[-2].in)->set_last( (yyvsp[0].in) ) ); }
#line 11594 "Parser/parser.cc"
    break;

  case 730:
#line 2875 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.in) = (InitializerNode *)((yyvsp[-3].in)->set_last( (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ) )); }
#line 11600 "Parser/parser.cc"
    break;

  case 732:
#line 2891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[-1].tok) ) ); }
#line 11606 "Parser/parser.cc"
    break;

  case 734:
#line 2897 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-1].en)->set_last( (yyvsp[0].en) )); }
#line 11612 "Parser/parser.cc"
    break;

  case 735:
#line 2903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 11618 "Parser/parser.cc"
    break;

  case 736:
#line 2906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 11624 "Parser/parser.cc"
    break;

  case 737:
#line 2908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 11630 "Parser/parser.cc"
    break;

  case 738:
#line 2910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild( (yyvsp[-4].en) ), maybeMoveBuild( (yyvsp[-2].en) ) ) ); }
#line 11636 "Parser/parser.cc"
    break;

  case 739:
#line 2912 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 11642 "Parser/parser.cc"
    break;

  case 741:
#line 2936 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl) ); }
#line 11648 "Parser/parser.cc"
    break;

  case 742:
#line 2941 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11654 "Parser/parser.cc"
    break;

  case 743:
#line 2943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 11660 "Parser/parser.cc"
    break;

  case 744:
#line 2948 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "9" );
			if ( (yyvsp[-1].tclass) == TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 11671 "Parser/parser.cc"
    break;

  case 745:
#line 2955 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 11677 "Parser/parser.cc"
    break;

  case 746:
#line 2957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "9" ); }
#line 11683 "Parser/parser.cc"
    break;

  case 747:
#line 2959 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 11689 "Parser/parser.cc"
    break;

  case 748:
#line 2961 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "9" );
			(yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 11698 "Parser/parser.cc"
    break;

  case 749:
#line 2967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 11704 "Parser/parser.cc"
    break;

  case 750:
#line 2972 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 11710 "Parser/parser.cc"
    break;

  case 751:
#line 2974 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 11716 "Parser/parser.cc"
    break;

  case 752:
#line 2976 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::DStype; }
#line 11722 "Parser/parser.cc"
    break;

  case 753:
#line 2980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 11728 "Parser/parser.cc"
    break;

  case 754:
#line 2985 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 11734 "Parser/parser.cc"
    break;

  case 755:
#line 2987 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 11740 "Parser/parser.cc"
    break;

  case 756:
#line 2989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ftype; }
#line 11746 "Parser/parser.cc"
    break;

  case 757:
#line 2991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 11752 "Parser/parser.cc"
    break;

  case 758:
#line 2996 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11758 "Parser/parser.cc"
    break;

  case 761:
#line 3003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 11764 "Parser/parser.cc"
    break;

  case 762:
#line 3008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 11770 "Parser/parser.cc"
    break;

  case 763:
#line 3010 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 11776 "Parser/parser.cc"
    break;

  case 764:
#line 3017 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 11782 "Parser/parser.cc"
    break;

  case 766:
#line 3020 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 11788 "Parser/parser.cc"
    break;

  case 767:
#line 3022 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)( (yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 11794 "Parser/parser.cc"
    break;

  case 768:
#line 3027 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 11800 "Parser/parser.cc"
    break;

  case 769:
#line 3029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11806 "Parser/parser.cc"
    break;

  case 770:
#line 3031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 11812 "Parser/parser.cc"
    break;

  case 771:
#line 3036 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 11818 "Parser/parser.cc"
    break;

  case 772:
#line 3038 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 11824 "Parser/parser.cc"
    break;

  case 773:
#line 3043 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "10" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), nullptr );
		}
#line 11833 "Parser/parser.cc"
    break;

  case 774:
#line 3048 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "11" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 11842 "Parser/parser.cc"
    break;

  case 775:
#line 3056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), nullptr );
		}
#line 11851 "Parser/parser.cc"
    break;

  case 776:
#line 3061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-2].tok), (yyvsp[-4].decl), nullptr ); }
#line 11857 "Parser/parser.cc"
    break;

  case 777:
#line 3063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticWarning( yylloc, Warning::DeprecTraitSyntax );
			(yyval.decl) = DeclarationNode::newTrait( (yyvsp[-8].tok), (yyvsp[-6].decl), (yyvsp[-2].decl) );
		}
#line 11866 "Parser/parser.cc"
    break;

  case 778:
#line 3068 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-7].decl), (yyvsp[-2].decl) ); }
#line 11872 "Parser/parser.cc"
    break;

  case 780:
#line 3074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl) ); }
#line 11878 "Parser/parser.cc"
    break;

  case 785:
#line 3086 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 11884 "Parser/parser.cc"
    break;

  case 786:
#line 3091 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 11890 "Parser/parser.cc"
    break;

  case 787:
#line 3093 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 11896 "Parser/parser.cc"
    break;

  case 789:
#line 3101 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl);	}
#line 11902 "Parser/parser.cc"
    break;

  case 790:
#line 3106 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11908 "Parser/parser.cc"
    break;

  case 791:
#line 3108 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->appendList( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 11914 "Parser/parser.cc"
    break;

  case 792:
#line 3113 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11920 "Parser/parser.cc"
    break;

  case 794:
#line 3118 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 11926 "Parser/parser.cc"
    break;

  case 795:
#line 3122 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 11932 "Parser/parser.cc"
    break;

  case 796:
#line 3127 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( (yyvsp[0].tok) ) ) ); }
#line 11938 "Parser/parser.cc"
    break;

  case 797:
#line 3129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Variable declarations of anonymous types requires creating a unique type-name across multiple translation
			// unit, which is a dubious task, especially because C uses name rather than structural typing; hence it is
			// disallowed at the moment.
			if ( (yyvsp[0].decl)->linkage == LinkageSpec::Cforall && ! (yyvsp[0].decl)->storageClasses.is_static && (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->kind == TypeData::AggregateInst ) {
				if ( (yyvsp[0].decl)->type->aggInst.aggregate->kind == TypeData::Enum && (yyvsp[0].decl)->type->aggInst.aggregate->enumeration.anon ) {
					SemanticError( yylloc, "extern anonymous enumeration is currently unimplemented." ); (yyval.decl) = nullptr;
				} else if ( (yyvsp[0].decl)->type->aggInst.aggregate->aggregate.anon ) { // handles struct or union
					SemanticError( yylloc, "extern anonymous struct/union is currently unimplemented." ); (yyval.decl) = nullptr;
				}
			}
		}
#line 11955 "Parser/parser.cc"
    break;

  case 798:
#line 3142 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, " declaration" ); (yyval.decl) = nullptr; }
#line 11961 "Parser/parser.cc"
    break;

  case 799:
#line 3144 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.decl) = nullptr; }
#line 11967 "Parser/parser.cc"
    break;

  case 800:
#line 3146 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.decl) = nullptr; }
#line 11973 "Parser/parser.cc"
    break;

  case 801:
#line 3148 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 11979 "Parser/parser.cc"
    break;

  case 802:
#line 3150 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 11985 "Parser/parser.cc"
    break;

  case 803:
#line 3152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 11991 "Parser/parser.cc"
    break;

  case 805:
#line 3155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 12000 "Parser/parser.cc"
    break;

  case 806:
#line 3160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( false, (yyvsp[-2].constant), nullptr ) ) ); }
#line 12006 "Parser/parser.cc"
    break;

  case 807:
#line 3162 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = LinkageSpec::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 12015 "Parser/parser.cc"
    break;

  case 808:
#line 3167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-1].decl);
		}
#line 12025 "Parser/parser.cc"
    break;

  case 809:
#line 3173 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = LinkageSpec::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 12034 "Parser/parser.cc"
    break;

  case 810:
#line 3178 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12044 "Parser/parser.cc"
    break;

  case 811:
#line 3185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.any() ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 12053 "Parser/parser.cc"
    break;

  case 812:
#line 3190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12063 "Parser/parser.cc"
    break;

  case 813:
#line 3196 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any() ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 12072 "Parser/parser.cc"
    break;

  case 814:
#line 3201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12082 "Parser/parser.cc"
    break;

  case 815:
#line 3207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.any()) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.any()) ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall) ) forall = true; // remember generic type
		}
#line 12091 "Parser/parser.cc"
    break;

  case 816:
#line 3212 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12101 "Parser/parser.cc"
    break;

  case 818:
#line 3227 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].sn) ); }
#line 12107 "Parser/parser.cc"
    break;

  case 819:
#line 3229 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].sn) ); }
#line 12113 "Parser/parser.cc"
    break;

  case 820:
#line 3234 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; forall = false; }
#line 12119 "Parser/parser.cc"
    break;

  case 821:
#line 3236 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.en) = (yyvsp[-2].en); forall = false;
			if ( (yyvsp[0].decl) ) {
				SemanticError( yylloc, "Attributes cannot be associated with function body. Move attribute(s) before \"with\" clause." );
				(yyval.en) = nullptr;
			} // if
		}
#line 12131 "Parser/parser.cc"
    break;

  case 822:
#line 3247 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 12142 "Parser/parser.cc"
    break;

  case 823:
#line 3254 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 12151 "Parser/parser.cc"
    break;

  case 824:
#line 3259 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 12160 "Parser/parser.cc"
    break;

  case 825:
#line 3265 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12166 "Parser/parser.cc"
    break;

  case 826:
#line 3268 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 12172 "Parser/parser.cc"
    break;

  case 827:
#line 3271 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12178 "Parser/parser.cc"
    break;

  case 828:
#line 3275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-4].decl) );
		}
#line 12187 "Parser/parser.cc"
    break;

  case 829:
#line 3281 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12193 "Parser/parser.cc"
    break;

  case 830:
#line 3284 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 12199 "Parser/parser.cc"
    break;

  case 831:
#line 3287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 12205 "Parser/parser.cc"
    break;

  case 836:
#line 3299 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild( (yyvsp[-2].en) ), maybeMoveBuild( (yyvsp[0].en) ) ) ); }
#line 12211 "Parser/parser.cc"
    break;

  case 837:
#line 3304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12217 "Parser/parser.cc"
    break;

  case 838:
#line 3306 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = (yyvsp[-2].constant);
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 12227 "Parser/parser.cc"
    break;

  case 839:
#line 3315 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12233 "Parser/parser.cc"
    break;

  case 842:
#line 3322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12239 "Parser/parser.cc"
    break;

  case 843:
#line 3327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 12245 "Parser/parser.cc"
    break;

  case 845:
#line 3333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12251 "Parser/parser.cc"
    break;

  case 846:
#line 3338 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12257 "Parser/parser.cc"
    break;

  case 847:
#line 3340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 12263 "Parser/parser.cc"
    break;

  case 848:
#line 3342 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 12269 "Parser/parser.cc"
    break;

  case 853:
#line 3351 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 12275 "Parser/parser.cc"
    break;

  case 854:
#line 3353 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 12281 "Parser/parser.cc"
    break;

  case 855:
#line 3388 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12287 "Parser/parser.cc"
    break;

  case 856:
#line 3390 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12293 "Parser/parser.cc"
    break;

  case 857:
#line 3395 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12299 "Parser/parser.cc"
    break;

  case 859:
#line 3398 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12305 "Parser/parser.cc"
    break;

  case 860:
#line 3400 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12311 "Parser/parser.cc"
    break;

  case 861:
#line 3405 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12317 "Parser/parser.cc"
    break;

  case 862:
#line 3407 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12323 "Parser/parser.cc"
    break;

  case 863:
#line 3409 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12329 "Parser/parser.cc"
    break;

  case 864:
#line 3411 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12335 "Parser/parser.cc"
    break;

  case 865:
#line 3416 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12341 "Parser/parser.cc"
    break;

  case 866:
#line 3418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12347 "Parser/parser.cc"
    break;

  case 867:
#line 3420 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12353 "Parser/parser.cc"
    break;

  case 868:
#line 3422 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12359 "Parser/parser.cc"
    break;

  case 869:
#line 3424 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12365 "Parser/parser.cc"
    break;

  case 870:
#line 3426 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12371 "Parser/parser.cc"
    break;

  case 871:
#line 3428 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12377 "Parser/parser.cc"
    break;

  case 872:
#line 3433 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12383 "Parser/parser.cc"
    break;

  case 873:
#line 3435 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 12389 "Parser/parser.cc"
    break;

  case 874:
#line 3437 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12395 "Parser/parser.cc"
    break;

  case 875:
#line 3439 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12401 "Parser/parser.cc"
    break;

  case 876:
#line 3448 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12407 "Parser/parser.cc"
    break;

  case 878:
#line 3451 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12413 "Parser/parser.cc"
    break;

  case 879:
#line 3456 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12419 "Parser/parser.cc"
    break;

  case 880:
#line 3458 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12425 "Parser/parser.cc"
    break;

  case 881:
#line 3460 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 12431 "Parser/parser.cc"
    break;

  case 882:
#line 3462 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12437 "Parser/parser.cc"
    break;

  case 883:
#line 3464 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12443 "Parser/parser.cc"
    break;

  case 884:
#line 3469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12449 "Parser/parser.cc"
    break;

  case 885:
#line 3471 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12455 "Parser/parser.cc"
    break;

  case 886:
#line 3473 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12461 "Parser/parser.cc"
    break;

  case 887:
#line 3475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12467 "Parser/parser.cc"
    break;

  case 888:
#line 3480 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12473 "Parser/parser.cc"
    break;

  case 889:
#line 3482 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12479 "Parser/parser.cc"
    break;

  case 890:
#line 3484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12485 "Parser/parser.cc"
    break;

  case 891:
#line 3486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12491 "Parser/parser.cc"
    break;

  case 892:
#line 3488 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12497 "Parser/parser.cc"
    break;

  case 893:
#line 3490 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12503 "Parser/parser.cc"
    break;

  case 897:
#line 3508 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 12509 "Parser/parser.cc"
    break;

  case 898:
#line 3510 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12515 "Parser/parser.cc"
    break;

  case 899:
#line 3512 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 12521 "Parser/parser.cc"
    break;

  case 900:
#line 3514 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12527 "Parser/parser.cc"
    break;

  case 901:
#line 3516 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12533 "Parser/parser.cc"
    break;

  case 902:
#line 3521 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12539 "Parser/parser.cc"
    break;

  case 903:
#line 3523 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12545 "Parser/parser.cc"
    break;

  case 904:
#line 3525 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12551 "Parser/parser.cc"
    break;

  case 905:
#line 3527 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12557 "Parser/parser.cc"
    break;

  case 906:
#line 3532 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12563 "Parser/parser.cc"
    break;

  case 907:
#line 3534 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12569 "Parser/parser.cc"
    break;

  case 908:
#line 3536 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12575 "Parser/parser.cc"
    break;

  case 909:
#line 3538 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12581 "Parser/parser.cc"
    break;

  case 910:
#line 3540 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12587 "Parser/parser.cc"
    break;

  case 911:
#line 3542 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12593 "Parser/parser.cc"
    break;

  case 912:
#line 3554 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "ID" );
		}
#line 12602 "Parser/parser.cc"
    break;

  case 913:
#line 3559 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12608 "Parser/parser.cc"
    break;

  case 914:
#line 3564 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12614 "Parser/parser.cc"
    break;

  case 916:
#line 3567 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12620 "Parser/parser.cc"
    break;

  case 917:
#line 3569 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12626 "Parser/parser.cc"
    break;

  case 918:
#line 3574 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12632 "Parser/parser.cc"
    break;

  case 919:
#line 3576 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12638 "Parser/parser.cc"
    break;

  case 920:
#line 3578 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12644 "Parser/parser.cc"
    break;

  case 921:
#line 3580 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12650 "Parser/parser.cc"
    break;

  case 922:
#line 3585 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12656 "Parser/parser.cc"
    break;

  case 923:
#line 3587 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12662 "Parser/parser.cc"
    break;

  case 924:
#line 3589 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12668 "Parser/parser.cc"
    break;

  case 925:
#line 3591 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12674 "Parser/parser.cc"
    break;

  case 926:
#line 3593 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12680 "Parser/parser.cc"
    break;

  case 927:
#line 3595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12686 "Parser/parser.cc"
    break;

  case 928:
#line 3597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12692 "Parser/parser.cc"
    break;

  case 929:
#line 3602 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12698 "Parser/parser.cc"
    break;

  case 930:
#line 3604 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 12704 "Parser/parser.cc"
    break;

  case 931:
#line 3606 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12710 "Parser/parser.cc"
    break;

  case 932:
#line 3608 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12716 "Parser/parser.cc"
    break;

  case 933:
#line 3617 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12722 "Parser/parser.cc"
    break;

  case 935:
#line 3620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12728 "Parser/parser.cc"
    break;

  case 936:
#line 3625 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12734 "Parser/parser.cc"
    break;

  case 937:
#line 3627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12740 "Parser/parser.cc"
    break;

  case 938:
#line 3629 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 12746 "Parser/parser.cc"
    break;

  case 939:
#line 3631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12752 "Parser/parser.cc"
    break;

  case 940:
#line 3633 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12758 "Parser/parser.cc"
    break;

  case 941:
#line 3638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12764 "Parser/parser.cc"
    break;

  case 942:
#line 3640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12770 "Parser/parser.cc"
    break;

  case 943:
#line 3642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12776 "Parser/parser.cc"
    break;

  case 944:
#line 3644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12782 "Parser/parser.cc"
    break;

  case 945:
#line 3649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12788 "Parser/parser.cc"
    break;

  case 946:
#line 3651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12794 "Parser/parser.cc"
    break;

  case 947:
#line 3653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12800 "Parser/parser.cc"
    break;

  case 948:
#line 3655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12806 "Parser/parser.cc"
    break;

  case 949:
#line 3657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12812 "Parser/parser.cc"
    break;

  case 950:
#line 3659 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12818 "Parser/parser.cc"
    break;

  case 951:
#line 3669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12824 "Parser/parser.cc"
    break;

  case 952:
#line 3671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12830 "Parser/parser.cc"
    break;

  case 954:
#line 3674 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12836 "Parser/parser.cc"
    break;

  case 955:
#line 3676 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12842 "Parser/parser.cc"
    break;

  case 956:
#line 3681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12848 "Parser/parser.cc"
    break;

  case 957:
#line 3683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12854 "Parser/parser.cc"
    break;

  case 958:
#line 3685 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12860 "Parser/parser.cc"
    break;

  case 959:
#line 3690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12866 "Parser/parser.cc"
    break;

  case 960:
#line 3692 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12872 "Parser/parser.cc"
    break;

  case 961:
#line 3694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12878 "Parser/parser.cc"
    break;

  case 962:
#line 3696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12884 "Parser/parser.cc"
    break;

  case 963:
#line 3701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12890 "Parser/parser.cc"
    break;

  case 964:
#line 3703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12896 "Parser/parser.cc"
    break;

  case 965:
#line 3705 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12902 "Parser/parser.cc"
    break;

  case 966:
#line 3719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12908 "Parser/parser.cc"
    break;

  case 967:
#line 3721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12914 "Parser/parser.cc"
    break;

  case 969:
#line 3724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12920 "Parser/parser.cc"
    break;

  case 970:
#line 3726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12926 "Parser/parser.cc"
    break;

  case 971:
#line 3731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12932 "Parser/parser.cc"
    break;

  case 972:
#line 3733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12938 "Parser/parser.cc"
    break;

  case 973:
#line 3738 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12944 "Parser/parser.cc"
    break;

  case 974:
#line 3740 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12950 "Parser/parser.cc"
    break;

  case 975:
#line 3742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12956 "Parser/parser.cc"
    break;

  case 976:
#line 3747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12962 "Parser/parser.cc"
    break;

  case 977:
#line 3749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12968 "Parser/parser.cc"
    break;

  case 978:
#line 3754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12974 "Parser/parser.cc"
    break;

  case 979:
#line 3756 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12980 "Parser/parser.cc"
    break;

  case 981:
#line 3774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12986 "Parser/parser.cc"
    break;

  case 982:
#line 3776 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12992 "Parser/parser.cc"
    break;

  case 983:
#line 3781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].op) ); }
#line 12998 "Parser/parser.cc"
    break;

  case 984:
#line 3783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 13004 "Parser/parser.cc"
    break;

  case 985:
#line 3785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 13010 "Parser/parser.cc"
    break;

  case 986:
#line 3787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 13016 "Parser/parser.cc"
    break;

  case 987:
#line 3789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13022 "Parser/parser.cc"
    break;

  case 989:
#line 3795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13028 "Parser/parser.cc"
    break;

  case 990:
#line 3797 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13034 "Parser/parser.cc"
    break;

  case 991:
#line 3799 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13040 "Parser/parser.cc"
    break;

  case 992:
#line 3804 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 13046 "Parser/parser.cc"
    break;

  case 993:
#line 3806 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 13052 "Parser/parser.cc"
    break;

  case 994:
#line 3808 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13058 "Parser/parser.cc"
    break;

  case 995:
#line 3814 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 13064 "Parser/parser.cc"
    break;

  case 996:
#line 3816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false )->addArray( (yyvsp[0].decl) ); }
#line 13070 "Parser/parser.cc"
    break;

  case 997:
#line 3819 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-4].en), nullptr, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].en), nullptr, false ) ); }
#line 13076 "Parser/parser.cc"
    break;

  case 998:
#line 3822 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type array dimension is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 13082 "Parser/parser.cc"
    break;

  case 1000:
#line 3828 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 13088 "Parser/parser.cc"
    break;

  case 1001:
#line 3830 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 13094 "Parser/parser.cc"
    break;

  case 1003:
#line 3833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 13100 "Parser/parser.cc"
    break;

  case 1004:
#line 3835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 13106 "Parser/parser.cc"
    break;

  case 1006:
#line 3841 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LThan; }
#line 13112 "Parser/parser.cc"
    break;

  case 1007:
#line 3843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 13118 "Parser/parser.cc"
    break;

  case 1008:
#line 3848 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), nullptr, false ); }
#line 13124 "Parser/parser.cc"
    break;

  case 1009:
#line 3850 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 13130 "Parser/parser.cc"
    break;

  case 1010:
#line 3852 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newArray( (yyvsp[-2].en), nullptr, false ) ); }
#line 13136 "Parser/parser.cc"
    break;

  case 1011:
#line 3854 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 13142 "Parser/parser.cc"
    break;

  case 1012:
#line 3888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 13148 "Parser/parser.cc"
    break;

  case 1015:
#line 3895 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 13154 "Parser/parser.cc"
    break;

  case 1016:
#line 3897 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13160 "Parser/parser.cc"
    break;

  case 1017:
#line 3899 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13166 "Parser/parser.cc"
    break;

  case 1018:
#line 3904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].op) ); }
#line 13172 "Parser/parser.cc"
    break;

  case 1019:
#line 3906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 13178 "Parser/parser.cc"
    break;

  case 1020:
#line 3908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 13184 "Parser/parser.cc"
    break;

  case 1021:
#line 3910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 13190 "Parser/parser.cc"
    break;

  case 1022:
#line 3912 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13196 "Parser/parser.cc"
    break;

  case 1024:
#line 3918 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13202 "Parser/parser.cc"
    break;

  case 1025:
#line 3920 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13208 "Parser/parser.cc"
    break;

  case 1026:
#line 3922 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13214 "Parser/parser.cc"
    break;

  case 1027:
#line 3927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 13220 "Parser/parser.cc"
    break;

  case 1028:
#line 3929 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 13226 "Parser/parser.cc"
    break;

  case 1029:
#line 3931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13232 "Parser/parser.cc"
    break;

  case 1031:
#line 3938 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 13238 "Parser/parser.cc"
    break;

  case 1033:
#line 3949 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, nullptr, false ); }
#line 13244 "Parser/parser.cc"
    break;

  case 1034:
#line 3952 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 13250 "Parser/parser.cc"
    break;

  case 1035:
#line 3954 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( nullptr, (yyvsp[-2].decl), false ); }
#line 13256 "Parser/parser.cc"
    break;

  case 1036:
#line 3957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 13262 "Parser/parser.cc"
    break;

  case 1037:
#line 3959 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 13268 "Parser/parser.cc"
    break;

  case 1038:
#line 3961 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-4].decl), true ); }
#line 13274 "Parser/parser.cc"
    break;

  case 1040:
#line 3976 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13280 "Parser/parser.cc"
    break;

  case 1041:
#line 3978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13286 "Parser/parser.cc"
    break;

  case 1042:
#line 3983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].op) ); }
#line 13292 "Parser/parser.cc"
    break;

  case 1043:
#line 3985 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 13298 "Parser/parser.cc"
    break;

  case 1044:
#line 3987 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 13304 "Parser/parser.cc"
    break;

  case 1045:
#line 3989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 13310 "Parser/parser.cc"
    break;

  case 1046:
#line 3991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 13316 "Parser/parser.cc"
    break;

  case 1048:
#line 3997 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13322 "Parser/parser.cc"
    break;

  case 1049:
#line 3999 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 13328 "Parser/parser.cc"
    break;

  case 1050:
#line 4001 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13334 "Parser/parser.cc"
    break;

  case 1051:
#line 4006 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 13340 "Parser/parser.cc"
    break;

  case 1052:
#line 4008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 13346 "Parser/parser.cc"
    break;

  case 1055:
#line 4018 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 13352 "Parser/parser.cc"
    break;

  case 1058:
#line 4029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 13358 "Parser/parser.cc"
    break;

  case 1059:
#line 4031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 13364 "Parser/parser.cc"
    break;

  case 1060:
#line 4033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 13370 "Parser/parser.cc"
    break;

  case 1061:
#line 4035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 13376 "Parser/parser.cc"
    break;

  case 1062:
#line 4037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 13382 "Parser/parser.cc"
    break;

  case 1063:
#line 4039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 13388 "Parser/parser.cc"
    break;

  case 1064:
#line 4046 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13394 "Parser/parser.cc"
    break;

  case 1065:
#line 4048 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13400 "Parser/parser.cc"
    break;

  case 1066:
#line 4050 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13406 "Parser/parser.cc"
    break;

  case 1067:
#line 4052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 13412 "Parser/parser.cc"
    break;

  case 1068:
#line 4054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13418 "Parser/parser.cc"
    break;

  case 1069:
#line 4057 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13424 "Parser/parser.cc"
    break;

  case 1070:
#line 4059 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13430 "Parser/parser.cc"
    break;

  case 1071:
#line 4061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13436 "Parser/parser.cc"
    break;

  case 1072:
#line 4063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 13442 "Parser/parser.cc"
    break;

  case 1073:
#line 4065 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13448 "Parser/parser.cc"
    break;

  case 1074:
#line 4070 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 13454 "Parser/parser.cc"
    break;

  case 1075:
#line 4072 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 13460 "Parser/parser.cc"
    break;

  case 1076:
#line 4077 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 13466 "Parser/parser.cc"
    break;

  case 1077:
#line 4079 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl)->addQualifiers( (yyvsp[-4].decl) ), true ); }
#line 13472 "Parser/parser.cc"
    break;

  case 1079:
#line 4106 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 13478 "Parser/parser.cc"
    break;

  case 1083:
#line 4117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 13484 "Parser/parser.cc"
    break;

  case 1084:
#line 4119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 13490 "Parser/parser.cc"
    break;

  case 1085:
#line 4121 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 13496 "Parser/parser.cc"
    break;

  case 1086:
#line 4123 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 13502 "Parser/parser.cc"
    break;

  case 1087:
#line 4125 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 13508 "Parser/parser.cc"
    break;

  case 1088:
#line 4127 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 13514 "Parser/parser.cc"
    break;

  case 1089:
#line 4134 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13520 "Parser/parser.cc"
    break;

  case 1090:
#line 4136 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13526 "Parser/parser.cc"
    break;

  case 1091:
#line 4138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13532 "Parser/parser.cc"
    break;

  case 1092:
#line 4140 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13538 "Parser/parser.cc"
    break;

  case 1093:
#line 4142 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13544 "Parser/parser.cc"
    break;

  case 1094:
#line 4144 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13550 "Parser/parser.cc"
    break;

  case 1095:
#line 4149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 13556 "Parser/parser.cc"
    break;

  case 1096:
#line 4151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 13562 "Parser/parser.cc"
    break;

  case 1097:
#line 4153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 13568 "Parser/parser.cc"
    break;

  case 1098:
#line 4160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 13574 "Parser/parser.cc"
    break;

  case 1099:
#line 4162 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 13580 "Parser/parser.cc"
    break;

  case 1102:
#line 4186 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 13586 "Parser/parser.cc"
    break;

  case 1103:
#line 4188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 13592 "Parser/parser.cc"
    break;


#line 13596 "Parser/parser.cc"

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
#line 4191 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "make install" //
// End: //
