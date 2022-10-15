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

#line 309 "Parser/parser.cc"

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
    DISABLE = 347,
    ENABLE = 348,
    TRY = 349,
    THROW = 350,
    THROWRESUME = 351,
    AT = 352,
    ASM = 353,
    ALIGNAS = 354,
    ALIGNOF = 355,
    GENERIC = 356,
    STATICASSERT = 357,
    IDENTIFIER = 358,
    QUOTED_IDENTIFIER = 359,
    TYPEDIMname = 360,
    TYPEDEFname = 361,
    TYPEGENname = 362,
    TIMEOUT = 363,
    WOR = 364,
    CATCH = 365,
    RECOVER = 366,
    CATCHRESUME = 367,
    FIXUP = 368,
    FINALLY = 369,
    INTEGERconstant = 370,
    CHARACTERconstant = 371,
    STRINGliteral = 372,
    DIRECTIVE = 373,
    FLOATING_DECIMALconstant = 374,
    FLOATING_FRACTIONconstant = 375,
    FLOATINGconstant = 376,
    ARROW = 377,
    ICR = 378,
    DECR = 379,
    LS = 380,
    RS = 381,
    LE = 382,
    GE = 383,
    EQ = 384,
    NE = 385,
    ANDAND = 386,
    OROR = 387,
    ELLIPSIS = 388,
    EXPassign = 389,
    MULTassign = 390,
    DIVassign = 391,
    MODassign = 392,
    PLUSassign = 393,
    MINUSassign = 394,
    LSassign = 395,
    RSassign = 396,
    ANDassign = 397,
    ERassign = 398,
    ORassign = 399,
    ErangeUpEq = 400,
    ErangeDown = 401,
    ErangeDownEq = 402,
    ATassign = 403,
    THEN = 404
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
#define DISABLE 347
#define ENABLE 348
#define TRY 349
#define THROW 350
#define THROWRESUME 351
#define AT 352
#define ASM 353
#define ALIGNAS 354
#define ALIGNOF 355
#define GENERIC 356
#define STATICASSERT 357
#define IDENTIFIER 358
#define QUOTED_IDENTIFIER 359
#define TYPEDIMname 360
#define TYPEDEFname 361
#define TYPEGENname 362
#define TIMEOUT 363
#define WOR 364
#define CATCH 365
#define RECOVER 366
#define CATCHRESUME 367
#define FIXUP 368
#define FINALLY 369
#define INTEGERconstant 370
#define CHARACTERconstant 371
#define STRINGliteral 372
#define DIRECTIVE 373
#define FLOATING_DECIMALconstant 374
#define FLOATING_FRACTIONconstant 375
#define FLOATINGconstant 376
#define ARROW 377
#define ICR 378
#define DECR 379
#define LS 380
#define RS 381
#define LE 382
#define GE 383
#define EQ 384
#define NE 385
#define ANDAND 386
#define OROR 387
#define ELLIPSIS 388
#define EXPassign 389
#define MULTassign 390
#define DIVassign 391
#define MODassign 392
#define PLUSassign 393
#define MINUSassign 394
#define LSassign 395
#define RSassign 396
#define ANDassign 397
#define ERassign 398
#define ORassign 399
#define ErangeUpEq 400
#define ErangeDown 401
#define ErangeDownEq 402
#define ATassign 403
#define THEN 404

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 280 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"

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

#line 681 "Parser/parser.cc"

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
#define YYLAST   22655

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  177
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  292
/* YYNRULES -- Number of rules.  */
#define YYNRULES  1043
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  2107

#define YYUNDEFTOK  2
#define YYMAXUTOK   404


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
       2,     2,     2,   166,     2,     2,     2,   170,   163,     2,
     151,   153,   162,   164,   157,   165,   154,   169,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,   158,   176,
     171,   175,   172,   174,   152,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,   155,   168,   156,   161,     2,   160,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,   159,   173,   150,   167,     2,     2,     2,
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
     145,   146,   147,   148,   149
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   579,   579,   583,   590,   591,   592,   593,   594,   598,
     599,   600,   601,   602,   603,   604,   608,   609,   613,   614,
     619,   623,   624,   635,   637,   639,   643,   644,   646,   648,
     650,   652,   662,   664,   666,   668,   670,   672,   677,   678,
     688,   693,   698,   699,   704,   710,   712,   714,   720,   722,
     726,   728,   730,   732,   734,   736,   738,   740,   742,   744,
     746,   748,   750,   752,   754,   756,   766,   767,   771,   772,
     777,   780,   784,   785,   789,   790,   792,   794,   796,   798,
     800,   805,   807,   809,   817,   818,   826,   829,   830,   832,
     837,   853,   855,   857,   859,   861,   863,   865,   867,   869,
     877,   878,   880,   884,   885,   886,   887,   891,   892,   894,
     896,   898,   900,   902,   904,   906,   913,   914,   915,   916,
     920,   921,   925,   926,   931,   932,   934,   936,   941,   942,
     944,   949,   950,   952,   957,   958,   960,   962,   964,   969,
     970,   972,   977,   978,   983,   984,   989,   990,   995,   996,
    1001,  1002,  1007,  1008,  1011,  1016,  1021,  1022,  1030,  1036,
    1037,  1041,  1042,  1046,  1047,  1051,  1052,  1053,  1054,  1055,
    1056,  1057,  1058,  1059,  1060,  1061,  1071,  1073,  1078,  1079,
    1081,  1083,  1088,  1089,  1095,  1096,  1102,  1103,  1104,  1105,
    1106,  1107,  1108,  1109,  1110,  1111,  1112,  1114,  1115,  1121,
    1123,  1133,  1135,  1143,  1144,  1149,  1151,  1153,  1155,  1157,
    1161,  1162,  1164,  1169,  1171,  1178,  1180,  1182,  1192,  1194,
    1196,  1201,  1206,  1209,  1214,  1216,  1218,  1220,  1228,  1229,
    1231,  1235,  1237,  1241,  1243,  1244,  1246,  1248,  1253,  1254,
    1258,  1263,  1264,  1268,  1270,  1275,  1277,  1282,  1284,  1286,
    1288,  1293,  1295,  1297,  1299,  1304,  1306,  1311,  1312,  1334,
    1336,  1338,  1341,  1343,  1346,  1348,  1351,  1353,  1358,  1363,
    1365,  1370,  1375,  1377,  1379,  1381,  1383,  1386,  1388,  1391,
    1393,  1398,  1404,  1407,  1409,  1414,  1420,  1422,  1427,  1433,
    1436,  1438,  1441,  1443,  1448,  1455,  1457,  1462,  1468,  1470,
    1475,  1481,  1484,  1489,  1497,  1499,  1501,  1506,  1508,  1513,
    1514,  1516,  1521,  1523,  1528,  1530,  1532,  1534,  1537,  1541,
    1544,  1548,  1550,  1552,  1554,  1556,  1558,  1560,  1562,  1564,
    1566,  1568,  1573,  1574,  1578,  1584,  1589,  1594,  1595,  1599,
    1603,  1608,  1609,  1615,  1619,  1621,  1623,  1625,  1628,  1630,
    1635,  1637,  1642,  1644,  1646,  1651,  1653,  1659,  1660,  1664,
    1665,  1666,  1667,  1671,  1676,  1677,  1679,  1681,  1683,  1687,
    1691,  1692,  1696,  1698,  1700,  1702,  1704,  1710,  1711,  1717,
    1718,  1722,  1723,  1728,  1730,  1736,  1737,  1739,  1744,  1749,
    1760,  1761,  1765,  1766,  1772,  1773,  1777,  1779,  1783,  1785,
    1789,  1790,  1794,  1795,  1799,  1806,  1807,  1811,  1813,  1828,
    1829,  1830,  1831,  1833,  1837,  1839,  1843,  1850,  1852,  1854,
    1859,  1860,  1862,  1864,  1866,  1898,  1901,  1906,  1908,  1914,
    1919,  1924,  1935,  1940,  1945,  1950,  1955,  1964,  1968,  1975,
    1977,  1978,  1979,  1985,  1987,  1992,  1993,  1994,  2003,  2004,
    2005,  2009,  2010,  2017,  2026,  2027,  2028,  2033,  2034,  2043,
    2044,  2049,  2050,  2054,  2056,  2058,  2060,  2062,  2066,  2071,
    2072,  2074,  2084,  2085,  2090,  2092,  2094,  2096,  2098,  2100,
    2103,  2105,  2107,  2112,  2114,  2116,  2118,  2120,  2122,  2124,
    2126,  2128,  2130,  2132,  2134,  2136,  2138,  2140,  2142,  2144,
    2146,  2148,  2150,  2152,  2154,  2156,  2158,  2160,  2162,  2164,
    2166,  2171,  2172,  2176,  2183,  2184,  2190,  2191,  2193,  2195,
    2197,  2202,  2204,  2209,  2210,  2212,  2214,  2219,  2221,  2223,
    2225,  2227,  2229,  2234,  2241,  2243,  2245,  2250,  2258,  2257,
    2261,  2269,  2270,  2272,  2274,  2279,  2280,  2282,  2287,  2288,
    2290,  2292,  2297,  2298,  2300,  2305,  2307,  2309,  2311,  2312,
    2314,  2319,  2321,  2323,  2328,  2335,  2339,  2340,  2345,  2344,
    2349,  2348,  2367,  2366,  2378,  2377,  2388,  2393,  2394,  2399,
    2405,  2419,  2420,  2424,  2426,  2428,  2434,  2436,  2438,  2440,
    2442,  2444,  2446,  2448,  2454,  2455,  2460,  2469,  2471,  2480,
    2482,  2483,  2484,  2486,  2488,  2489,  2494,  2495,  2496,  2501,
    2503,  2506,  2513,  2514,  2515,  2521,  2526,  2528,  2534,  2535,
    2541,  2542,  2546,  2551,  2554,  2553,  2557,  2560,  2567,  2572,
    2571,  2580,  2585,  2589,  2593,  2597,  2599,  2604,  2606,  2608,
    2610,  2616,  2617,  2618,  2625,  2626,  2628,  2629,  2630,  2632,
    2634,  2641,  2642,  2644,  2646,  2651,  2652,  2658,  2659,  2661,
    2662,  2667,  2668,  2669,  2671,  2679,  2680,  2682,  2685,  2687,
    2691,  2692,  2693,  2695,  2697,  2702,  2704,  2709,  2711,  2720,
    2722,  2727,  2728,  2729,  2733,  2734,  2735,  2740,  2741,  2746,
    2747,  2748,  2749,  2753,  2754,  2759,  2760,  2761,  2762,  2763,
    2777,  2778,  2783,  2784,  2790,  2792,  2795,  2797,  2799,  2822,
    2823,  2829,  2830,  2836,  2835,  2845,  2844,  2848,  2854,  2860,
    2861,  2863,  2867,  2872,  2874,  2876,  2878,  2884,  2885,  2889,
    2890,  2895,  2897,  2904,  2906,  2907,  2909,  2914,  2916,  2918,
    2923,  2925,  2930,  2935,  2943,  2945,  2950,  2951,  2956,  2957,
    2961,  2962,  2963,  2968,  2970,  2976,  2978,  2983,  2985,  2991,
    2992,  2996,  3000,  3004,  3006,  3007,  3009,  3011,  3013,  3015,
    3017,  3019,  3020,  3025,  3028,  3027,  3039,  3038,  3051,  3050,
    3062,  3061,  3073,  3072,  3086,  3092,  3094,  3100,  3101,  3112,
    3119,  3124,  3130,  3133,  3136,  3140,  3146,  3149,  3152,  3157,
    3158,  3159,  3163,  3169,  3170,  3180,  3181,  3185,  3186,  3191,
    3196,  3197,  3203,  3204,  3206,  3211,  3212,  3213,  3214,  3215,
    3217,  3252,  3254,  3259,  3261,  3262,  3264,  3269,  3271,  3273,
    3275,  3280,  3282,  3284,  3286,  3288,  3290,  3292,  3297,  3299,
    3301,  3303,  3312,  3314,  3315,  3320,  3322,  3324,  3326,  3328,
    3333,  3335,  3337,  3339,  3344,  3346,  3348,  3350,  3352,  3354,
    3366,  3367,  3368,  3372,  3374,  3376,  3378,  3380,  3385,  3387,
    3389,  3391,  3396,  3398,  3400,  3402,  3404,  3406,  3421,  3426,
    3431,  3433,  3434,  3436,  3441,  3443,  3445,  3447,  3452,  3454,
    3456,  3458,  3460,  3462,  3464,  3469,  3471,  3473,  3475,  3477,
    3487,  3489,  3491,  3492,  3494,  3499,  3501,  3503,  3508,  3510,
    3512,  3514,  3519,  3521,  3523,  3537,  3539,  3541,  3542,  3544,
    3549,  3551,  3556,  3558,  3560,  3565,  3567,  3572,  3574,  3591,
    3592,  3594,  3599,  3601,  3603,  3605,  3607,  3612,  3613,  3615,
    3617,  3622,  3624,  3626,  3632,  3634,  3636,  3639,  3643,  3645,
    3647,  3649,  3684,  3685,  3689,  3690,  3692,  3694,  3699,  3701,
    3703,  3705,  3707,  3712,  3713,  3715,  3717,  3722,  3724,  3726,
    3732,  3733,  3735,  3744,  3747,  3749,  3752,  3754,  3756,  3770,
    3771,  3773,  3778,  3780,  3782,  3784,  3786,  3791,  3792,  3794,
    3796,  3801,  3803,  3811,  3812,  3813,  3818,  3819,  3824,  3826,
    3828,  3830,  3832,  3834,  3841,  3843,  3845,  3847,  3849,  3852,
    3854,  3856,  3858,  3860,  3865,  3867,  3869,  3874,  3900,  3901,
    3903,  3907,  3908,  3912,  3914,  3916,  3918,  3920,  3922,  3929,
    3931,  3933,  3935,  3937,  3939,  3944,  3946,  3948,  3955,  3957,
    3975,  3977,  3982,  3983
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
  "multi_array_dimension", "abstract_parameter_declarator_opt",
  "abstract_parameter_declarator", "abstract_parameter_ptr",
  "abstract_parameter_array", "abstract_parameter_function",
  "array_parameter_dimension", "array_parameter_1st_dimension",
  "variable_abstract_declarator", "variable_abstract_ptr",
  "variable_abstract_array", "variable_abstract_function",
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
     125,    40,    64,    41,    46,    91,    93,    44,    58,   123,
      96,    94,    42,    38,    43,    45,    33,   126,    92,    47,
      37,    60,    62,   124,    63,    61,    59
};
# endif

#define YYPACT_NINF (-1745)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-922)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     133, 12319,   161,   250, 17057,   139, -1745, -1745, -1745, -1745,
   -1745, -1745, -1745, -1745, -1745, -1745, -1745, -1745,   117,   738,
     178, -1745, -1745, -1745, -1745, -1745, -1745, -1745, -1745, -1745,
   -1745, -1745, -1745, -1745, -1745, -1745, -1745, -1745, -1745, -1745,
   -1745, -1745, -1745, -1745, -1745, -1745, -1745, -1745,     5,   339,
   -1745, -1745, -1745, -1745, -1745, -1745,  5014,  5014,   230, 12319,
     243,   253, 22386, -1745,   372, -1745, -1745, -1745, -1745, -1745,
   -1745, -1745, -1745, -1745,  3876, -1745,   773,   383, -1745, -1745,
   -1745, -1745, -1745, 16906, -1745, -1745,   374,   419,   260,    27,
   -1745,  5014,   419,   419,   419,   406,  3418,   647,   873, 12480,
   -1745, -1745, -1745, 16755,  1663, -1745, -1745, -1745,  3161,   726,
    7454,  1077,   961,  3161,  1218,   487, -1745, -1745, -1745, -1745,
     665, -1745, -1745, -1745, -1745,   589, -1745, -1745, -1745, -1745,
   -1745,   606,   623,   665, -1745,   665,   635, -1745, -1745, -1745,
   18061,  5014, -1745, -1745,  5014, -1745, 12319, -1745,   630, 18212,
   -1745, -1745,  4439, 19624, -1745,   719,   719,   666,  2521, -1745,
   -1745, -1745, -1745,    42, 14363,  3979,   665, -1745, -1745, -1745,
   -1745, -1745, -1745,   682, -1745,   641,   698,   723, -1745,   775,
   21784, -1745, -1745, -1745, -1745, -1745, -1745, -1745, 15679,  3071,
    3876,   616,   744,   750,   782,   784,   791,   794, -1745, -1745,
   18363, 11336,   798, -1745, 17501, -1745, -1745, -1745, -1745,   809,
   -1745, -1745,   749, -1745,  6355,   930, 20029, -1745,   814,  5014,
     623,   817,   800,   816,   825, -1745, -1745, -1745,  2707,  3492,
     848,   915,    65, -1745, -1745,   665,   665,    50,    70,   119,
      50, -1745,   665,   665, -1745,  4372, -1745, -1745,   867,   883,
     719,  9735, -1745, 16906, -1745, -1745,  3161, -1745,  2823,   487,
     896,   971,    70,  5014,   260, -1745, 13883, -1745,   719,   719,
     903,   971,    70,  5014, -1745, 22532, -1745, -1745,   719, -1745,
     719, -1745,   818,  4820,  5014, -1745,  2011,   923, -1745, -1745,
   -1745, 16569,   623,    90, -1745, -1745, 19768, -1745,   915,    34,
   -1745, 21784, 19624,  3746,  4372, -1745,   122, -1745, -1745, -1745,
   18212,  5014, -1745,   917, -1745, -1745, -1745, -1745,  5014,  3993,
     476,   440, -1745,  5014,   641, -1745,   801,   665,   665,   939,
   18514,   485, 14843, 19819,  3161,  3161, -1745,  3161,   719,  3161,
     719, -1745, -1745,   665, -1745,   897, -1745, 18665, -1745, -1745,
   -1745, 18816,   809, -1745,   974,   346,  2255,   982,   487,   991,
   -1745,  2521,   977,   641,  2521,  2629, -1745,  1009,   997, 21857,
    1017,  1022,  1025, 21784, 21930,  1043, 22437, -1745, -1745, -1745,
   -1745, -1745, -1745, 22003, 22003, 15524,  1045,  4793, -1745, -1745,
   -1745, -1745,   144, -1745,   348, -1745,  1866, -1745, 21784, 21784,
   -1745,  1036,   590,   806,   859,   486,   862,  1056,  1047,  1033,
    1098,    93, -1745,   557, -1745,  1082, -1745,   854,  3671, 15989,
   -1745, -1745,   622,  1082, -1745, -1745,   655, -1745, -1745,  3071,
    1088,  1099,  1103,  1106,  1109,  1117, -1745, -1745,   197,  1119,
   -1745,   262,  1119, -1745, -1745, 18061, -1745,   864,  1121, 16144,
   -1745, -1745,  4952,  4059,  1152, 14843,  1156,   574,   815, -1745,
   -1745, -1745, -1745, -1745,  5014,  4967, -1745, -1745, -1745, -1745,
   -1745, -1745, 16463,  3617,  1045,  6355,  1133,  1135, -1745, -1745,
    1155, 20029,   625, -1745, -1745, -1745, 20105,  1158, -1745, -1745,
   -1745, -1745, -1745,  2707,   687,  1161,  1178,  1197,   693,  1203,
    1207,  1212,  3492, -1745, -1745,   665,  1167,   260,  1182, -1745,
   -1745,  1234, -1745, -1745,   623,   971, -1745, -1745, -1745,   623,
   -1745, -1745,  4372, -1745, 15989, 15989, -1745,   719,  4439, 20021,
   15003, -1745, -1745, -1745, -1745, -1745,   623,   971,    34, -1745,
   -1745,  3161,  1213,   971,    70, -1745,   623,   971, -1745, 22583,
   -1745,   719,   719, -1745, -1745,  1226,   403,  1233,   487,  1235,
   -1745, 17217, -1745,   729, -1745,  1326, 19919, -1745,  4439, 16657,
    9735, -1745, 16569, 22076, -1745, -1745, -1745, -1745, -1745,  3746,
     707,  4372, -1745, 15003,   915, 12319, -1745,  1242, -1745,  1251,
   -1745, -1745, -1745, -1745, -1745,  2521, -1745, -1745,  1342,  4858,
    4085, 18816, 11336, -1745, 18967, -1745,   719,   719, -1745, -1745,
     809, -1745,   658,  1266,  1415, 21784,   838,  1234,  1260, -1745,
     665,   665, -1745,  1119, -1745, 18514, -1745, -1745, 17661,   719,
     719, -1745,  4858,   665, -1745, 19480, -1745, -1745, 18665, -1745,
      42,  1282,  1267,  1285,  2255,   732, 18212,   742, -1745, -1745,
   -1745, -1745, -1745, -1745,   756, -1745,  1297,  1275, -1745, 15834,
   -1745,  4793, 19118, 19118, -1745, 15834, -1745, 21784, -1745, -1745,
   -1745, -1745, -1745, -1745, 15834, -1745, -1745, 17759, 19118, 19118,
     854,  1362,  1395,   364,  1785, -1745,   785,  1301,   905,  1315,
   -1745, 20105, 21784, 20178,  1310, 21784,  2011, 21784,  2011, -1745,
     916, -1745, -1745, 20251,  1102, 21784, 20251,  2011, -1745, -1745,
   21784, 21784, 21784, 21784, 21784, 21784, 21784, 21784, 21784, 21784,
   21784, 21784, 21784, 21784, 21784, 21784, 21784, 21784, 21784, 20324,
    1300,   775,  3809, 11336, -1745, -1745, -1745, -1745, -1745, -1745,
   -1745, -1745, -1745, -1745, -1745,  1318, 21784, -1745, -1745,   622,
    1283, -1745, -1745,   665,   665, -1745, -1745, 15989, -1745,   248,
    1119, -1745,   795,  1119, -1745, -1745, -1745,  1234, -1745, -1745,
    1234, 22149, -1745, -1745, 11336,  1323,  1324,  4472,  1467,  3222,
     330,  1260, -1745,   665,   665,  1260,   371, -1745,   665,   665,
   21784,  5014,   960,   970,  1260,    86, 14203, 14203,  5014, -1745,
   -1745, 21784,  1155, -1745,  6355,  1336, -1745,  2199, -1745, -1745,
   -1745, -1745, -1745,   810, -1745, 14203,  2011,  4439,  2011,   851,
    1338,  1339,  1340,   852,  1344,  1346,  1347,   387,  1119, -1745,
   -1745,   424,  1119, -1745, -1745, -1745,  4439,   775, -1745,  1119,
   22149, -1745,   623, 17217, -1745, -1745,   890,  1348,   891,  1351,
   -1745,  1357, -1745,   623, -1745, -1745,   623,   971,  1357, -1745,
     623,  1350,  1354,  1356, -1745, -1745, 17661, -1745,  1363, -1745,
   -1745, -1745,  2011,  5014, 10485,  1440,  1349, 19278, -1745,  1121,
   -1745, 14203,   894, -1745, -1745,  1357, -1745, 18212, 15989,  1343,
   -1745,  1343, -1745, -1745, -1745,  2255,   665,   665, -1745, 18665,
   -1745, 11500, 16299, -1745, 17217,  1367,  1370,  1371, -1745,  5783,
     665, -1745,   838, -1745, -1745, -1745, -1745,  1234, -1745, -1745,
   -1745,   719, -1745,  4212, -1745, -1745,   487,  2537,  1382, 20397,
   -1745,  2255,  1282, -1745, -1745,  1375,  1385,  2629, 20251, -1745,
    1386,  1390,   383,  1392,  1389,  1397,  1399,  1405, 21784,  1406,
    1407,  1409, 11336, 21784, -1745, -1745,  1874, -1745, -1745, -1745,
   21784, -1745,  1413,  1421,  8876,   979, -1745, 20251,  1419, -1745,
    1420, -1745, -1745,  4879, -1745, -1745,   899, -1745, -1745, -1745,
   -1745,  4879, -1745, -1745,  1014,   375, -1745, -1745,  1036,  1036,
    1036,   590,   590,   806,   806,   859,   859,   859,   859,   486,
     486,   862,  1056,  1047,  1033,  1098, 21784,  1069, -1745,  1425,
    4879, -1745, -1745,  6355, -1745, 17217,  1426,  1427,  1430,  1283,
   -1745, -1745, -1745, -1745, -1745, -1745, -1745, -1745,  1234, -1745,
   -1745,  1234, 17217, 17217, -1745, -1745,  4472,   712,  1431,  1434,
    1435,  1437,  2860,  3222, -1745, -1745, -1745, -1745, -1745, -1745,
   -1745, -1745, -1745, -1745, -1745, -1745, -1745, -1745,  1436, -1745,
    1260, -1745, -1745, -1745, -1745, -1745, -1745, -1745, -1745,  1438,
    1441, -1745,   260,  4879,  1092,    59, -1745, -1745,  1443, -1745,
   20029, -1745, 21784,   665, 20470, 14203, -1745, -1745, -1745,  1422,
     430,  1119, -1745,   479,  1119, -1745, -1745, -1745, -1745,  1234,
   -1745, -1745, -1745,  1234,   915,  1448,  1234, -1745, -1745, -1745,
   -1745, -1745, -1745, -1745,  1452, -1745, -1745,  1357, -1745,   623,
   -1745, -1745, -1745, -1745, -1745, 13117,  1451,  1450, -1745,    48,
   -1745,   426,   483, 11172,  1454, 15360,  1457,  1458,  2927,  3363,
    3201, 20543,  1459, -1745, -1745,  1462,  1463, -1745, -1745,   623,
   21784, 21784,  1602,  1460,   572, -1745,  1554,  1480,  1464, -1745,
   -1745, -1745, 10311, -1745, -1745, -1745, -1745, -1745,  1078, -1745,
   -1745, -1745,  1547, -1745, -1745, -1745,  2011, -1745, -1745, 12963,
   16906,  1481, -1745,  5014, -1745,  1465,  1487,  1489, -1745,  1108,
   -1745, -1745, -1745, -1745,  4439, -1745, -1745,  1471,  1472,   900,
   18212,   641,   641,  1282,  1490,  1491, -1745, -1745,  1045,  1121,
   16144, -1745,  1082, -1745, 11664, -1745,   500,  1119, -1745,   719,
    7775, -1745, -1745,  2255,   665,   665,    42,  1267, -1745,  6355,
   -1745,  1282,  1501,  1504, -1745, -1745,   912,   596, 17661, 11336,
    2011, -1745,   596, 17910,   596, -1745, 21784, 21784, 21784, -1745,
   -1745, -1745, -1745, 21784, 21784,  1496,  6355, -1745, -1745,  1500,
     638, -1745, -1745, -1745,  2375, -1745, -1745,  1111, -1745,    22,
   -1745, 20251,  1116, -1745, 20105, -1745, -1745, 21784,  1482,  1123,
    1145,  1155, -1745,   529,  1119, -1745, -1745, 17217, 17217, -1745,
   -1745,  1506,   532,  1119, -1745,   534,  2454,   665,   665, -1745,
   -1745, 17217, 17217, -1745,  1505, -1745, 15003, 15003,  1512,  1509,
    1520,  1525, -1745,  1522, 21784, 21784,  1151,  1526, -1745, -1745,
   -1745, -1745, -1745, -1745, -1745,  1528, 21784, -1745, -1745, -1745,
    1234, -1745, -1745, -1745,  1234, 17217, 17217,   260,   665,  1165,
    1530,  1536, -1745, -1745,  1538, 13271, 13425, 13579, 18212, 19118,
   19118,  1543, -1745,  1519,  1521,  2794, 13723, -1745,   193,  5014,
   -1745, -1745,  5014, -1745,  9373,   265,   306, -1745, -1745, -1745,
   -1745, 21784,  1545,  1618, 11007, 10659, -1745,  1524, -1745,  1539,
   21784,  1540,  6355,  1541, 21784, 20105, 21784,   885, -1745,  1542,
      78, -1745,    97,  1550, -1745, -1745,  1552, -1745,  1544, -1745,
    1546,  1553, 15360,   755, 14043,   665,   318, -1745, -1745, -1745,
    1561, -1745,  1556, -1745,  1574, -1745,  1572, -1745,  1573, -1745,
   -1745, -1745, -1745,  1582,  2255,  2255, 11828,  1578,  1580,  1581,
   -1745,  1585, -1745, -1745, -1745,  1234, 21784, 21784,  1121,  1583,
   -1745,  1282, -1745,  1584,   134, -1745,  1155,  1590, -1745, -1745,
   18212, -1745,   706,  1589,  1586,   940, -1745,  1587, -1745, -1745,
   -1745, -1745, -1745,  6355,  1155, 20105, -1745,  1626,  4879, -1745,
    1626,  1626, -1745,  4879,  2690,  4238, -1745, -1745,  1172, -1745,
   -1745, -1745,  1599,  1601, -1745, -1745, -1745,  1234, -1745, -1745,
    1603,  1605,   665, -1745, -1745, -1745,  1234, -1745, -1745, -1745,
    1606, -1745, -1745, -1745, -1745, -1745, -1745, -1745, -1745, -1745,
   -1745, -1745, -1745, -1745,  1596, -1745, -1745, -1745, -1745,  1604,
    1609,   665, -1745, 17217, 17217, -1745, -1745, -1745, -1745, 21784,
   -1745, -1745,  1614, -1745,  1543,  1543,  1543,   753,  1591,   336,
   -1745,  4149,   478, 15989, -1745, -1745, -1745,  3862, 21784,  4655,
     510, -1745, -1745,    46,  1607,  1607,  5014, -1745, -1745, 17368,
   -1745, 21784,  1615,  1613, -1745, -1745, -1745, -1745,   941,  1620,
   15360,  1480,  1622, 21784,   374,  1616,   406,  8751, 18212, -1745,
   -1745, -1745,   786, 15360, 21784,   731,   468, -1745, 21784,  6163,
   -1745, -1745,   517, -1745,  1155, -1745,   948,   950,   955, -1745,
   -1745, -1745, -1745,   623,   885,  1629, -1745, -1745, 21784, -1745,
    1632,   775, 11172, -1745, -1745, -1745, -1745, 21784,  1668, -1745,
   10137, -1745,   665, 15003, -1745, -1745, 18212, -1745, -1745, -1745,
    1282,  1282, -1745, -1745, -1745,  1628, -1745, 17217, -1745, -1745,
    1630, -1745,  1634,  1641,  1636,  2255, -1745,  1643, -1745, -1745,
    1652, -1745, -1745, 21784, -1745, 17910, 21784,  1155,  1656,  1185,
   -1745,  1187, -1745,  4879, -1745,  4879, -1745, -1745, -1745, -1745,
   17217,  1655,  1657, -1745, -1745, 17217, 17217,  1659,  1660,  1190,
   14523, 14683, -1745,  1653, -1745, -1745, -1745, -1745,  1661,  1664,
    1192, -1745, -1745, -1745, -1745,   753,  1619,   520, -1745, -1745,
   -1745, -1745,   665,   665, -1745, -1745, -1745,   545, -1745,   956,
    3862,   764, -1745,  4655,   665, -1745, -1745, -1745, -1745, -1745,
   -1745, -1745, -1745,   548, 15360,    43, 20616,  1743, 15360,  1480,
   15163, -1745, -1745, -1745, -1745, 21784, -1745, 20689,  1746,  1644,
    7049, 20762, 15360, 10833,  1480,   524,  1217,  1646, 21784, -1745,
    1674,   400, 15360, -1745, -1745,  1677, -1745, -1745,  1649,   775,
     608,  1676,  1678,  1230,  1742, -1745, -1745, -1745, -1745,  5014,
    4439,  1683,  1686, -1745, -1745,  1682,  1688, -1745, -1745, -1745,
    2255,  1282, -1745,  1695, -1745, -1745, -1745, -1745,  1696, -1745,
   -1745, -1745,  1249,  1259, -1745, -1745, -1745, -1745, -1745, -1745,
   -1745, -1745, -1745, -1745,  1698, -1745, -1745,  1700,  1701, -1745,
   -1745, -1745,  1702,  1704,  1707,  1619, -1745,   665, -1745, -1745,
   -1745, -1745, -1745,  1693,  4149, -1745, -1745,  5433,   108, 11995,
   -1745, 15256, -1745,     4,   965, 15360,  1786,   563,  1703,   448,
   15360, 21784,  1711,   524,  1217,  1692, 22222,  1705,   505,  1798,
   -1745, 20835, 20908, 21784,  1480,  1706, 12158, -1745, -1745, -1745,
   19329, -1745,  1718,  1720,    87, 15360, -1745, 21784, 20251,   445,
   -1745, -1745, -1745, -1745, -1745,  1726, -1745, -1745,  1282,  1730,
   -1745, -1745, -1745, -1745,  1728,  1747,  1752, 15003,  1751, -1745,
   -1745,   553,  1119, -1745, -1745,   753, -1745, -1745,   277, -1745,
     149, -1745, -1745, -1745,  1764, 12641, -1745, -1745, 15360, -1745,
      67, -1745, 15360, 21784,  1766, 20981, -1745, -1745, 21054, 21127,
   21784,  1711,  1480, 21200, 21273, 15360,  1749,   508,  1754,   521,
   -1745, -1745,  1769, 12641, 19329, -1745,  4406, 18967,  2011,  1767,
   -1745,  1819,  1776,   612,  1771, -1745,  1855, -1745,   966, 15360,
    1781, 15360, 15360, -1745,  1783, -1745, -1745, -1745, -1745, -1745,
   -1745, -1745, -1745,  1234, -1745, 21784, -1745, 21784, -1745, -1745,
    1341, 12802, -1745, -1745, 15360, -1745, -1745,  1480, -1745, -1745,
    1480,  1772,   559,  1773,   560, -1745, -1745,  1480, -1745,  1480,
   -1745,  1782, 21346, 21419, 21492, -1745,  1341, -1745,  1762,  3283,
    3773, -1745, -1745, -1745,    87,  1790, 21784,  1777,    87,    87,
   15360, -1745, -1745, 21784,  1845,  1848, -1745, 17217, -1745, -1745,
   15256, -1745,  1341, -1745, -1745,  1809, 21565, 21638, 21711, -1745,
   -1745,  1480, -1745,  1480, -1745,  1480, -1745,  1762, 21784,  1810,
    3773,  1804,   775,  1811, -1745,   640, -1745, -1745,   972,  1742,
     343, -1745, -1745,  9842,  1815, 15256, -1745, -1745,  1480, -1745,
    1480, -1745,  1480,  1816,  1817, -1745,   623,   775,  1820, -1745,
    1792,   775, -1745, -1745, 15360,  1897,  1822, -1745, -1745, -1745,
   10009, -1745,   623, -1745, -1745,  1268, 21784, -1745,   975, -1745,
   15360, -1745, -1745,   775,  2011,  1823,  1801, -1745, -1745, -1745,
     989, -1745, -1745,  1805,  2011, -1745, -1745
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,   457,     0,     2,   457,   474,   475,   476,   477,   478,
     479,   480,   481,   482,   463,   465,   464,   466,     0,     0,
       0,   483,   485,   506,   486,   507,   489,   490,   504,   505,
     484,   502,   503,   487,   488,   491,   492,   493,   494,   495,
     496,   497,   498,   499,   500,   501,   508,   509,   805,   511,
     584,   585,   588,   590,   586,   592,     0,     0,     0,   457,
       0,     0,    16,   555,   561,     9,    10,    11,    12,    13,
      14,    15,   763,   102,     0,    19,     0,     2,   100,   101,
      17,    18,   821,   457,   764,   406,     0,   409,   689,   411,
     420,     0,   410,   440,   441,     0,     0,     0,     0,   538,
     459,   461,   467,   457,   469,   472,   523,   510,   445,   516,
     521,   446,   533,   447,   548,   552,   558,   537,   564,   576,
     805,   581,   582,   565,   634,   412,   413,     3,   771,   784,
     462,     0,     0,   805,   843,   805,     2,   860,   861,   862,
     457,     0,  1021,  1022,     0,     1,   457,    16,     0,   457,
     429,   430,     0,   538,   451,   452,   453,   774,     0,   587,
     589,   591,   593,     0,   457,     0,   806,   807,   583,   512,
     682,   683,   681,   742,   737,   727,     0,     0,   772,     0,
       0,   474,   765,   769,   770,   766,   767,   768,   457,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   556,   559,
     457,   457,     0,  1023,   538,   850,   868,  1027,  1020,  1018,
    1025,   405,     0,   164,   695,   163,     0,   414,     0,     0,
       0,     0,     0,     0,     0,   404,   920,   921,     0,     0,
     439,   803,   805,   799,   824,   805,   805,   801,     2,   805,
     800,   881,   805,   805,   878,     0,   531,   532,     0,     0,
     457,   457,     2,   457,   421,   460,   470,   524,     0,   553,
       0,   787,     2,     0,   689,   422,   538,   517,   534,   549,
       0,   787,     2,     0,   473,   518,   525,   526,   535,   540,
     550,   554,     0,   568,     0,   757,     2,     2,   785,   842,
     844,   457,     0,     2,     2,  1031,   538,  1034,   803,   803,
       3,     0,   538,     0,     0,   432,   805,   801,   800,     2,
     457,     0,   761,     0,   723,   725,   724,   726,     0,     0,
     719,     0,   709,     0,   718,   729,     0,   805,   805,     2,
     457,  1042,   458,   457,   469,   448,   516,   449,   541,   450,
     548,   545,   566,   805,   567,     0,   670,   457,   671,   996,
     997,   457,   672,   674,   555,   561,     0,   635,   636,     0,
     808,     0,   740,   728,     0,   812,    21,     0,    20,     0,
       0,     0,     0,     0,     0,     0,    23,    25,     4,     8,
       5,     6,     7,     0,     0,   457,     2,     0,   103,   104,
     105,   106,    87,    24,    88,    42,    86,   107,     0,     0,
     122,   124,   128,   131,   134,   139,   142,   144,   146,   148,
     150,   152,   155,     0,    26,     0,   562,     2,   107,   457,
     156,   734,   685,   552,   687,   733,     0,   684,   688,     0,
       0,     0,     0,     0,     0,     0,   822,   848,   805,   858,
     866,   870,   876,     2,  1029,   457,  1032,     2,   100,   457,
       3,   669,     0,  1042,     0,   458,   516,   541,   548,     3,
       3,   651,   655,   665,   671,   672,     2,   851,   869,  1019,
       2,     2,    23,     0,     2,   695,    24,     0,   693,   696,
    1040,     0,     0,   702,   691,   690,     0,     0,   789,     2,
       2,     2,     2,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   827,   884,   805,     0,   689,     2,   823,
     831,   947,   825,   826,     0,   787,     2,   880,   888,     0,
     882,   883,     0,   435,   457,   457,   522,   458,     0,   538,
     457,  1024,  1028,  1026,   539,   761,     0,   787,   803,   415,
     423,   471,     0,   787,     2,   761,     0,   787,   738,   519,
     520,   536,   551,   557,   560,   555,   561,   579,   580,     0,
     739,   457,   679,     0,   201,   398,   457,     3,     0,   538,
     457,   786,   457,     0,   417,     2,   418,   758,   437,     0,
       0,     0,     2,   457,   803,   457,   761,     0,     2,     0,
     722,   721,   720,   715,   468,     0,   713,   730,   514,     0,
       0,   457,   457,   998,   458,   454,   455,   456,  1002,   993,
     994,  1000,     2,     2,   101,     0,   958,   972,  1042,   954,
     805,   805,   963,   970,   677,   457,   546,   673,   458,   542,
     543,   547,     0,   805,  1008,   458,  1013,  1005,   457,  1010,
       0,  1040,   641,     0,     0,     0,   457,     0,   820,   819,
     815,   817,   818,   816,     0,   810,   813,     0,    22,   457,
      94,     0,   457,   457,    89,   457,    96,     0,    32,    36,
      37,    33,    34,    35,   457,    92,    93,   457,   457,   457,
       2,   103,   104,     0,     0,   182,     0,     0,   582,     0,
    1018,     0,     0,     0,     0,     0,     0,     0,     0,    55,
       0,    61,    62,    66,     0,     0,    66,     0,    90,    91,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   457,   165,   166,   167,   168,   169,   170,
     171,   172,   173,   174,   175,   163,     0,   161,   162,     2,
     932,   686,   929,   805,   805,   937,   563,   457,   849,   805,
     859,   867,   871,   877,     2,   852,   854,   856,     2,   872,
     874,     0,  1030,  1033,   457,     0,     0,     2,   101,   958,
     805,  1042,   902,   805,   805,  1042,   805,   917,   805,   805,
       3,   673,     0,     0,  1042,  1042,   457,   457,     0,     2,
     704,     0,  1040,   701,  1041,     0,   697,     0,     2,   700,
     703,   179,   178,     0,     2,   457,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   805,   836,   840,
     879,   805,   893,   898,   828,   885,     0,     0,   443,   944,
       0,   790,     0,   457,   791,   436,     0,     0,     0,     0,
     434,     2,   792,     0,   419,   761,     0,   787,     2,   793,
       0,     0,     0,     0,   594,   658,   458,     3,     3,   662,
     661,   863,     0,     0,   457,   399,     0,   538,     3,   100,
       3,   457,     0,     3,   762,     2,   717,   457,   457,   711,
     710,   711,   515,   513,   635,     0,   805,   805,  1004,   457,
    1009,   458,   457,   995,   457,     0,     0,     0,   973,     0,
     805,  1043,   959,   960,   678,   956,   957,   971,   999,  1003,
    1001,   544,   579,     0,  1007,  1012,   638,  1041,     0,     0,
     637,     0,  1040,   743,   741,     0,     0,   812,    66,   773,
       0,     0,     2,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   457,     0,   121,   120,     0,   117,   116,    27,
       0,    28,     0,     0,     0,     0,     3,    66,     0,    51,
       0,    52,    59,     0,    58,    70,     0,    67,    68,    71,
      54,     0,    53,    57,     0,     0,    50,   123,   125,   126,
     127,   129,   130,   132,   133,   137,   138,   135,   136,   140,
     141,   143,   145,   147,   149,   151,     0,     0,   408,     0,
       0,    29,     3,   695,   157,   457,     0,     0,     0,   933,
     934,   930,   931,   736,   735,     2,   853,   855,   857,     2,
     873,   875,   457,   457,   949,   948,     2,     0,     0,     0,
       0,     0,   805,   959,   905,   922,     2,   900,   908,   675,
     903,   904,   676,     2,   915,   925,   918,   919,     0,     3,
    1042,   427,     2,  1035,     2,   666,   667,   645,     3,     3,
       3,     3,   689,     0,   155,     0,     3,     3,     0,   698,
       0,   692,     0,   805,     0,   457,     3,   431,   433,     0,
     805,   837,   841,   805,   894,   899,     2,   829,   832,   834,
       2,   886,   889,   891,   803,     0,   945,     3,   795,     3,
     528,   527,   530,   529,     2,   762,   796,     2,   794,     0,
     762,   797,   594,   594,   594,   457,     0,     0,   680,     0,
     402,     0,     0,   457,     0,     2,     0,     0,     0,     0,
       0,   184,     0,   332,   333,     0,     0,   371,   370,     0,
     159,   159,   377,   555,   561,   198,     0,   185,     0,   209,
     186,   187,   457,   203,   188,   189,   190,   191,     0,   192,
     193,   338,     0,   194,   195,   196,     0,   197,   205,   538,
     457,     0,   207,     0,   396,     0,     0,     0,     3,     0,
     775,   762,   750,   751,     0,     3,   746,     3,     3,     0,
     457,   727,   727,  1040,     0,     0,  1006,  1011,     2,   100,
     457,     3,   553,     3,   458,     3,   805,   966,   969,   457,
       3,   955,   961,     0,   805,   805,     0,   641,   623,   695,
     642,  1040,     0,     2,   809,   811,     0,    95,   457,   457,
       0,    99,    97,   457,     0,   111,     0,     0,     0,   115,
     119,   118,   183,     0,     0,     0,   695,   108,   176,     0,
       0,    45,    46,    84,     0,    84,    84,     0,    72,    74,
      48,     0,     0,    44,     0,    47,   154,     0,     0,     0,
       0,  1040,     3,   805,   940,   943,   935,   457,   457,     3,
       3,     0,   805,   911,   914,   805,     0,   805,   805,   906,
     923,   457,   457,  1036,     0,   668,   457,   457,     0,     0,
       0,     0,   416,     3,     0,     0,     0,     0,   694,   699,
       3,   788,   181,   180,     3,     0,     0,     2,   830,   833,
     835,     2,   887,   890,   892,   457,   457,   689,   805,     0,
       0,     0,   762,   798,     0,   457,   457,   457,   457,   457,
     457,   577,   605,     3,     3,   606,   538,   595,     0,     0,
     845,     2,     0,   400,    66,     0,     0,   323,   324,   206,
     208,     0,     0,     0,   457,   457,   319,     0,   317,     0,
       0,     0,   695,     0,     0,     0,     0,     0,   160,     0,
       0,   378,     0,     0,     3,   213,     0,   204,     0,   314,
       0,     0,     2,     0,   538,   805,     0,   397,   951,   950,
       0,     2,     0,   753,     2,   748,     0,   749,     0,   731,
     712,   716,   714,     0,     0,     0,   457,     0,     0,     0,
       3,     0,     2,   962,   964,   965,     0,     0,   100,     0,
       3,  1040,   629,     0,   641,   639,  1040,     0,   626,   744,
     457,   814,   952,     0,     0,     0,    38,     0,   112,   114,
     113,   110,   109,   695,  1040,     0,    65,    81,     0,    75,
      82,    83,    60,     0,     0,     0,    69,    56,     0,   153,
     407,    30,     0,     0,     2,   936,   938,   939,     3,     3,
       0,     0,   805,     2,   907,   909,   910,     2,   924,   926,
       0,   901,   916,     3,     3,  1037,     3,   653,   652,   656,
    1039,     2,     2,  1038,     0,     3,   802,   705,   706,     0,
       0,   805,   438,   457,   457,     3,     3,   444,   804,     0,
     895,   779,     0,   781,   577,   577,   577,   612,   582,     0,
     618,   606,     0,   457,   569,   604,   600,     0,     0,     0,
       0,   607,   609,   805,   620,   620,     0,   601,   616,   457,
     403,     0,     0,    67,   327,   328,   325,   326,     0,     0,
       2,   224,     0,     0,   226,   411,   225,   538,   457,   305,
     304,   306,     0,     2,   184,   264,     0,   257,     0,   184,
     320,   318,     0,   312,  1040,   321,     0,     0,     0,   359,
     360,   361,   362,     0,   352,     0,   353,   329,     0,   330,
       0,     0,   457,   215,   202,   316,   315,     0,   350,   369,
       0,   401,   805,   457,   777,   732,   457,     2,     2,   628,
    1040,  1040,  1014,  1015,  1016,     0,   967,   457,     3,     3,
       0,   975,     0,     0,     0,     0,   640,     0,   625,     3,
       0,   953,    98,     0,    31,   457,     0,  1040,     0,     0,
      85,     0,    73,     0,    79,     0,    77,    43,   158,   941,
     457,     0,     0,   846,   864,   457,   457,     0,     0,     0,
     457,   457,   708,     0,   424,   426,     3,     3,     0,     0,
       0,   783,   573,   575,   571,     0,   982,     0,   613,   987,
     615,   979,   805,   805,   599,   619,   603,     0,   602,     0,
       0,     0,   622,     0,   805,   596,   610,   621,   611,   617,
     660,   664,   663,     0,     2,     0,     0,   245,     2,   227,
     538,   310,   308,   311,   307,     0,   309,     0,   253,     0,
     184,     0,     2,   457,   265,     0,   290,     0,     0,   313,
       0,     0,     2,   336,   363,     0,   354,     2,     0,     0,
       0,     0,   341,     0,   337,   200,   199,   425,   747,     0,
       0,     0,     0,  1017,     3,     0,     0,   974,   976,   627,
       0,  1040,   643,     2,    49,    41,    39,    40,     0,    63,
     177,    76,     0,     0,     3,   847,   865,     3,     3,   912,
     927,   428,     2,   650,     3,   649,   707,     0,     0,   838,
     896,   946,     0,     0,     0,   983,   984,   805,   598,   980,
     981,   597,   578,     0,     0,   214,   335,     0,     0,     0,
     238,     2,   216,     0,     0,     2,   247,   262,   273,   267,
       2,   184,   302,     0,   277,     0,     0,   268,   266,   255,
     258,     0,     0,   184,   291,     0,     0,   219,   334,     2,
     457,   331,     0,     0,   379,     2,   339,     0,    66,     0,
     351,   752,   754,   631,   633,     0,   977,   978,  1040,     0,
     745,    64,    80,    78,     0,     0,     0,   457,     0,   839,
     897,   805,   990,   992,   985,     0,   608,   233,   228,   231,
       0,   230,   237,   236,     0,   457,   240,   239,     2,   249,
       0,   246,     2,     0,     0,     0,   254,   259,     0,     0,
     184,   303,   278,     0,     0,     2,     0,   293,   294,   292,
     261,   322,     0,   457,   457,     3,   364,   458,   368,     0,
     372,     0,     0,     0,   380,   381,   222,   342,     0,     2,
       0,     2,     2,   968,     0,   632,   942,   913,   928,   654,
       2,   986,   988,   989,   614,     0,   235,     0,   234,   218,
     241,   457,   392,   250,     2,   251,   248,   263,   276,   274,
     270,   282,   280,   281,   279,   260,   275,   271,   272,   269,
     256,     0,     0,     0,     0,   221,   241,     3,   357,     0,
     982,   365,   366,   367,   379,     0,     0,     0,   379,     0,
       2,   340,   347,     0,   344,   346,   630,   457,   229,   232,
       2,     3,   242,   393,   252,     0,     0,     0,     0,   301,
     299,   296,   300,   297,   298,   295,     3,   357,     0,     0,
     983,     0,     0,     0,   373,     0,   382,   223,     0,   337,
       0,     3,   210,     0,     0,     2,   289,   287,   284,   288,
     285,   286,   283,     0,     0,   358,     0,   385,     0,   383,
       0,   385,   343,   345,     2,     0,     0,   212,   211,   217,
       0,   220,     0,   355,   386,     0,     0,   374,     0,   348,
       2,   991,   356,     0,     0,     0,     0,   349,   387,   388,
       0,   384,   375,     0,     0,   376,   389
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1745,  6239,  5285, -1745,    -1,   143,  2572,    19, -1745,  -319,
   -1745,   324, -1745,  -691,   618,   713,  -950, -1025, -1745,   164,
    6187,  1803, -1745,  1745, -1745,  1304,   488,   717,   720,   439,
     718,  1270,  1271,  1269,  1273,  1274, -1745,   309,  -171,  8421,
     846, -1745,  1588, -1745, -1745,  -641,  7986, -1113,  1188, -1745,
     544, -1745,   839,   -50, -1745, -1745, -1745,   398,    49, -1745,
   -1662, -1487,   264,    17, -1745, -1745, -1745,   279, -1508, -1745,
   -1215, -1745, -1745, -1745, -1745,   -26, -1710,   160, -1745, -1745,
     -19, -1745, -1745, -1745,    -4,   432,   433,   100, -1745, -1745,
   -1745, -1745,  -855, -1745,    29,   -28, -1745,   111, -1745,  -168,
   -1745, -1745, -1745,   866,  -841,  -832, -1300, -1745,     3, -1322,
     107,  1991,  -798,  -776, -1745,  -278, -1745,     7,  -160,  1312,
    -254,  -231,  3493,   992,  -637, -1745,   109,   113,  2169,  2227,
   -1745,  1996, -1745,   333,  2918, -1745, -1745, -1745,   177, -1745,
   -1745,   421,   373,  4082,  3117,   -31,  1789,  -248, -1745, -1745,
   -1745, -1745, -1745,  -311,  4687,  5052, -1745,  -363,   121, -1745,
     509,   229, -1745,   162,   708, -1745,   504,   -98, -1745, -1745,
   -1745,  5432,  -606, -1149,  -740,  -684,  -483,  1055, -1745, -1243,
    -152,    30,   539,   884,  8043,  -122,  -389,  -262,  -184,  -455,
    1261, -1745,  1593,   302,  1173,  1477, -1745, -1745, -1745, -1745,
     258,  -165,    -9,  -864, -1745,  -111, -1745, -1745,   617,   451,
   -1745, -1745, -1745,  2066,  -751,  -494,  -933,   -22, -1745, -1745,
   -1745, -1745, -1745, -1745,   175,  -748,  -117, -1744,  -213,  7922,
     -57,  6626, -1745,  1146, -1745,  1124,  -224,  -214,  -212,  -207,
      71,   -73,   -68,   -63,   357,   -38,   -13,    -7,  -202,   -71,
    -188,  -157,  -140,  -692,  -708,  -658,  -648,  -654,  -142,  -639,
   -1745, -1745,  -688,  1352,  1353,  1359,  2001,  7198, -1745,  -586,
    -569,  -566,  -563,  -721, -1745, -1593, -1666, -1661, -1653,  -598,
     -59,  -303, -1745, -1745,   181,   237,  -101, -1745,  7929,   415,
     663,  -399
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,  1156,   222,   392,   393,    81,    82,   394,   368,   395,
    1455,  1456,   396,   976,   977,   978,  1267,  1268,  1269,  1467,
     418,   398,   399,   400,   683,   684,   401,   402,   403,   404,
     405,   406,   407,   408,   409,   410,   411,   420,  1075,   685,
    1389,   746,   216,   748,   414,   813,  1157,  1158,  1159,  1160,
    1161,  1162,  1163,  2053,  1164,  1165,  1394,  1572,  1899,  1900,
    1830,  1831,  1832,  2021,  2022,  1166,  1586,  1587,  1588,  1736,
    1737,  1167,  1168,  1169,  1170,  1171,  1172,  1402,  1763,  1952,
    1870,  1173,  1174,  1604,  2039,  1605,  1606,  1935,  1175,  1176,
    1177,  1392,  1943,  1944,  1945,  2085,  2100,  1970,  1971,   292,
     293,   874,   875,  1129,    84,    85,    86,    87,    88,    89,
     451,    91,    92,    93,    94,    95,   230,   568,   453,   422,
     454,    98,   302,   100,   101,   102,   333,   334,   105,   106,
     168,   107,   893,   335,   154,   110,   250,   111,   155,   258,
     337,   338,   339,   156,   415,   116,   117,   341,   118,   559,
     863,   861,   862,  1544,   342,   343,   121,   122,  1125,  1357,
    1550,  1551,  1697,  1698,  1358,  1539,  1716,  1552,   123,   643,
    1644,   344,   641,   930,  1068,   459,   460,   867,   868,   461,
     462,   869,   346,   563,  1181,   424,   425,   217,   479,   480,
     481,   482,   483,   321,  1201,   322,   891,   889,   593,   323,
     362,   324,   325,   426,   125,   174,   175,   126,  1195,  1196,
    1197,  1198,     2,  1114,  1115,   585,  1190,   127,   312,   313,
     260,   270,   542,   128,   220,   129,   231,  1077,   854,   509,
     166,   130,   654,   655,   656,   131,   233,   234,   235,   236,
     307,   133,   134,   135,   136,   137,   138,   139,   239,   308,
     241,   242,   243,   781,   782,   783,   784,   785,   244,   787,
     788,   789,   751,   752,   753,   754,   510,   140,  1650,   618,
     619,   620,   621,   622,   623,  1700,  1701,  1702,  1703,   608,
     464,   349,   350,   351,   427,   208,   142,   143,   144,   353,
     805,   624
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      80,   192,   539,    80,   331,   503,   193,   150,    96,   413,
     363,   194,   345,   567,   495,   985,   496,   190,   507,   526,
     802,   497,   688,   359,  1199,   240,   498,   919,  1383,  1812,
     913,  1272,   485,  1178,  1813,   305,   195,   178,   932,   297,
     499,   851,  1814,   905,   636,   199,   906,   958,   639,   907,
     965,   858,  1575,  1575,  1869,    80,    80,  1071,    80,  1048,
    1279,   196,  1020,  1507,  1508,  1055,    96,   197,   694,  1038,
     515,   500,   132,    80,  1574,  1086,   603,    58,  1445,  1908,
     503,  1748,    80,  1901,   626,   574,   576,  1044,   501,   495,
      80,   496,   885,   634,   537,    80,   497,   637,    80,   446,
     883,   498,    80,  1816,   547,   348,   264,  1120,    90,  1902,
     103,   151,  1069,  1069,   104,   499,   218,   430,    58,  1039,
    1827,  1828,   431,  1313,   300,  1045,  1182,   432,   523,  1040,
     132,  1069,   506,  -755,  1191,   847,   849,    58,  1041,   218,
      80,  1188,  1974,    80,  1474,    80,   500,   148,    63,    64,
    1966,    80,   433,    96,   205,   192,   164,    80,   504,  -394,
     193,   145,   548,   501,    80,   194,    90,   237,   103,  1907,
     261,   493,   104,   560,   271,  1608,  1475,   434,   112,  -395,
    1909,  1610,  1341,   435,   219,   575,  -787,  1344,    80,    80,
     195,    58,  1314,   913,    58,   603,    76,  1069,   367,   172,
     172,   508,  1829,    80,   366,  1362,   254,   626,   905,  -787,
     265,   906,   256,   476,   907,   196,   286,   132,    80,   914,
     508,   197,  1894,  1901,  1363,   728,  1315,    80,    80,  -394,
     192,   463,  1846,   504,   172,   193,   112,   575,   141,  1469,
     194,   141,  1941,  1975,    80,   838,   579,  1236,  1611,  -395,
    -756,   554,    80,    90,  1609,   103,   157,   934,  1412,   104,
     205,   615,    80,   689,   207,    80,  1903,   729,   158,    58,
     516,   609,    80,   582,   508,   467,  1260,   508,   834,   820,
     268,   821,    80,    80,   172,    80,   822,   172,   282,  1203,
    1575,   823,  1369,  1352,   878,  1646,   141,   806,   900,   695,
     205,   172,    80,    80,   696,   824,  1967,  1968,   357,   929,
      80,   786,  1574,  1070,  1070,   597,  1048,    80,    80,  1251,
      58,  1178,    80,   112,   205,  1231,  1222,  1353,  1038,   163,
     207,  1286,  1070,  1812,   108,   925,   825,   543,  1813,  1869,
     141,   983,   941,  2002,   773,  1324,  1814,   898,   764,  1354,
    1556,  1299,   508,   826,   597,    80,   626,   834,  1907,    20,
      80,  1117,   172,    80,   653,   820,  1342,   821,  1024,  1557,
    1741,   918,   822,   540,   113,   205,   857,   823,  1039,   541,
     626,   177,  1049,   141,   924,  1370,  1052,   626,  1040,  1300,
     609,   824,   108,  1907,   179,  1065,  1066,  1291,  1070,  1025,
     570,  1069,    58,   508,   180,   845,   172,  1816,   213,  1532,
    1965,   850,   514,   768,  1182,   519,   172,   508,  2074,   214,
     430,  1575,   825,   331,   287,   431,   557,   172,    80,   562,
     432,   835,   113,  1146,   532,   215,   536,  1507,  1508,   826,
     206,  1564,   199,    58,  1315,   256,   546,  1894,  1662,  1664,
    1666,    80,    80,   238,   172,   433,   262,  1222,   897,    58,
     272,   172,   172,    80,    80,   287,   172,   905,   112,   883,
     906,  1739,    80,   907,   476,  1362,  1747,  1827,  1828,   108,
     434,  1046,  1566,  2020,   159,   613,   435,   160,   161,   412,
     162,   532,    80,  1705,  1621,  1434,    58,   188,   210,   172,
     467,    80,    58,   697,   172,  -921,   430,   172,   698,  2020,
     835,   431,  1706,  1352,  1352,  1352,   432,   953,  1661,   113,
    1949,    80,  1053,   188,   348,  1275,   613,    80,   954,   955,
    1851,  1852,  1271,  1573,  1589,  2055,  1401,   940,  1096,   200,
     943,   944,   508,   945,  1365,  1366,   206,  1353,  1353,  1353,
     211,    58,   947,  1950,   188,   949,   950,   951,  1281,  1856,
     463,   468,  -574,   884,   210,    80,   609,    80,   256,  1354,
    1354,  1354,    58,   467,  1499,  1100,    -3,  1364,    80,   508,
      80,  1327,   225,  1575,    80,   508,   206,    14,    15,    16,
      17,    18,    96,   594,    80,   172,  1207,   595,    80,    80,
     207,    58,  1070,   428,    58,   960,    58,   172,   172,   590,
     206,  1575,   919,   718,   719,  1915,    73,  1441,   626,  1059,
    1079,  1742,   457,   544,   108,    58,  1743,  1845,  1918,  1919,
    1331,    80,   463,  1478,   508,  1556,   612,   786,   591,   592,
     613,   282,   287,    80,  1959,  1206,    58,    78,   614,  1575,
     245,  1432,   467,   626,  1708,   613,   132,   720,   721,  1367,
     615,  1305,   960,   645,   113,   960,   647,  1714,   533,  1731,
    1732,  1733,  1924,  1562,   960,  1992,   288,  1817,   960,  1709,
    1484,   570,   256,  1493,   508,  1497,  1715,   508,  1994,   613,
     842,  1734,    90,  1749,   103,    80,  1818,    80,   104,    80,
    1088,   554,  1714,    80,  1960,   960,    80,  -455,   508,   148,
     730,   902,   853,  1792,   731,  1793,   960,   960,   856,  1104,
     960,  1821,   860,   188,  1825,   533,  2026,  2028,  1917,  -451,
    -683,    80,    14,    15,    16,    17,    18,    58,   172,  1913,
    1930,   541,   894,   896,  1597,   611,   284,   268,  1255,   112,
    1009,   606,   711,    73,   629,  1256,   883,   286,  1230,   712,
     713,  1863,   112,   463,   488,  2007,  1864,   286,   606,   436,
    2008,  1573,   606,   749,  1446,   922,    80,   508,    80,   807,
     808,  1225,   287,   809,    78,    79,   468,   172,  1466,    73,
      80,    58,   331,  2070,  -394,  1271,   159,    80,  2071,   160,
     161,  1464,   162,   476,   463,   301,    80,  1985,   756,   612,
    1312,  1420,   757,   613,   319,    80,    80,    80,  1630,  1631,
      78,    79,   141,  1506,  1659,  -776,   463,   463,    14,    15,
      16,    17,    18,   361,   210,    80,   571,    73,   286,   969,
     436,   971,   508,   974,   516,   463,   830,   982,   508,   364,
     986,    14,    15,    16,    17,    18,  1105,   612,   582,   468,
     436,   613,   508,  1046,   611,   436,  1651,   613,    78,   614,
     606,    80,    80,   476,   365,  1011,  1731,  1732,  1733,   198,
      64,    96,   871,   902,    73,   933,   872,    58,   960,   595,
    1192,  1337,   366,   348,    80,   935,  1319,   437,  1734,   595,
    1298,   786,  1589,   438,  1695,   108,   471,  1740,   508,   936,
      58,   463,   872,   937,   287,    78,    79,   436,   108,   508,
     246,   247,    80,   248,   553,    64,    80,  1594,   249,   687,
      80,  1731,  1732,  1733,   172,   439,   653,   440,   959,   428,
     428,   172,   960,   883,   441,   113,  1029,   442,  -456,   466,
     508,   457,   484,  1734,   598,   282,   918,   490,   113,  1087,
     470,  1089,  1735,  1083,  -452,   486,   626,  1084,   489,    73,
     714,   715,    80,   491,    14,    15,    16,    17,    18,   256,
      80,    90,   492,  1180,   716,   717,  1538,   104,  1436,   612,
     541,   722,   723,   613,  1193,  1599,  1600,  1601,  1602,  1603,
      78,   614,   286,   516,   457,   505,   508,   508,  1657,    80,
     200,   691,   476,   506,   256,  1128,   172,   172,   524,   147,
     601,   691,   606,   457,    65,    66,    67,    68,    69,    70,
      71,   972,  1119,    58,   525,    80,   363,   363,   172,  1781,
     920,    80,    80,  1110,  1112,   582,   606,   960,   960,   508,
     633,   112,  1270,  1419,   185,   535,  1271,   757,   841,   606,
     218,   428,   545,   844,  1972,  1451,  1224,   962,   963,  1271,
     172,   973,    80,   564,   172,  1527,   586,  1413,  1452,   331,
     852,   181,     6,     7,     8,     9,    10,    11,    12,    13,
     859,   255,  1972,  1654,  1725,   601,  1576,  1655,   960,   412,
     870,  1751,   276,  1752,   279,   960,   281,  1084,  1753,  1822,
    1074,   141,   960,   757,   658,   463,  1061,  1062,  1910,  2011,
      19,  1457,   960,  1271,   141,  2072,  1063,  1064,  2096,   960,
    2023,  1554,  2093,  -920,   476,  1258,  1084,    80,    80,    80,
      96,  -624,  2103,  1515,  1516,   255,  2104,   279,   281,  2041,
     644,   609,   646,  2045,   457,  1509,  1398,   995,   996,   997,
     998,   476,   657,    52,    53,    54,    55,    80,   661,    96,
    1273,  1274,   428,   662,  1878,    80,   663,  1948,    80,    80,
     348,   147,    80,   264,   170,   171,    65,    66,    67,    68,
      69,    70,    71,    80,   667,   457,   255,  1804,   191,   988,
     989,   990,   691,   687,   710,   147,   726,   108,   725,   687,
      65,    66,    67,    68,    69,    70,    71,   980,   687,   724,
     232,  -453,    80,  1692,  1693,  1694,   960,  1277,   476,   727,
    -442,    14,    15,    16,    17,    18,   732,   687,   412,    80,
      90,   758,  1180,  1345,  1346,  1347,   104,   113,  -156,  -156,
     261,   271,   759,  -442,  1399,   476,   760,   981,   255,   761,
     279,   281,   762,    80,  1063,  1411,   331,  1472,  1473,    90,
     763,  1180,  1477,  1473,   443,   104,   306,    -3,   172,  1481,
    1473,   172,   172,   172,  1555,   790,   254,   265,   255,  -454,
      58,   -17,   256,   803,   255,    80,    14,    15,    16,    17,
      18,  1035,  1465,   428,   928,   172,  1359,  1517,  1465,   814,
     112,   172,   804,    97,   827,  1276,   152,  1554,   837,   562,
     606,  1035,  1529,   629,   255,   503,   172,   541,  1667,  1084,
     631,   828,   281,   495,  1576,   496,   463,   463,   839,   112,
     497,  1790,  1084,  1791,  1473,   498,  1801,  1802,  1811,   960,
     829,   150,   494,   232,    80,    58,   831,   268,    80,   499,
     832,    80,  1731,  1732,  1733,   833,   172,   348,   671,   306,
     141,    97,   855,   457,   960,  -121,  -121,  -121,  -121,  -121,
    -121,   476,   870,  1453,  1734,  -572,  1108,  1867,  1868,   294,
     500,  1074,  -570,  -185,   864,   203,   873,  1116,   886,   141,
    1118,   476,   888,    80,  1121,  1882,  1473,   501,  -120,  -120,
    -120,  -120,  -120,  -120,    73,  1883,  1473,   141,  1827,  1828,
     892,   255,   908,    80,    80,  2093,  2094,   580,   306,  1470,
    1471,   991,   992,   910,   749,   615,   993,   994,   508,   927,
     999,  1000,   929,   870,   931,    78,    79,   255,   938,   631,
     281,   939,   295,  1192,   961,   151,  1717,  1717,    97,  1421,
    1422,   203,   476,   331,   671,  1078,   108,    80,   964,   967,
    1555,  1509,    80,    80,    80,   543,  1008,  1013,   504,  1034,
    1035,    90,    90,  1578,  1578,  1042,  1081,   104,   104,   834,
    1710,  1090,  1091,  1092,   255,   108,   820,  1093,   821,  1094,
    1095,  1111,   172,   822,  1113,   172,   113,  -759,   823,  1122,
    1183,   540,   444,  1123,   255,  1124,  -659,   541,  1200,   255,
    1216,   255,   824,  1217,  1218,  1184,  1359,  1359,  1359,  1509,
    1540,  1359,  1228,  1457,  1233,   113,   262,   272,  1234,  1237,
      80,   255,  1241,   255,   255,   172,    80,  1238,    80,  1240,
    1242,   112,   112,   825,   870,    80,  1243,  1193,  1244,  1246,
    1247,   255,  1248,   528,   348,   531,  1253,   172,   172,   476,
     826,   870,   870,   255,  1254,  1261,  1262,   780,  1278,  1283,
    1284,   264,   476,  1285,  1292,   141,  1479,  1293,  1294,  1721,
    1295,  -647,  1303,  1318,  -646,  1232,   255,  1326,   631,   281,
    1554,  1338,  -760,    97,  1360,  1371,  1936,  1361,  1374,  1375,
    1384,   141,   141,  1385,  1386,   428,  1391,   819,  -682,   476,
     255,   631,   531,   412,   412,   920,   232,   255,  1393,  1192,
    1760,   606,    14,    15,    16,    17,    18,   960,  1401,  1405,
    1395,  1407,   835,  1408,    80,  1409,   306,  1415,  1417,  1424,
    1425,  1448,   306,  1872,  1449,  1463,  1898,  1465,  1480,  1492,
     457,  1505,    80,  1343,    80,  1510,  1511,   181,     6,     7,
       8,     9,    10,    11,    12,    13,  1368,  1512,  1513,  1473,
    1936,  1521,  1518,  1530,   254,   265,  1531,   141,  1533,   626,
     256,    58,   306,  1387,  1543,  1545,  1364,  1546,  1569,   172,
    1590,  1612,  1614,   882,  1617,   306,  1624,   108,   108,    80,
     463,   463,    80,   172,  1622,  1591,  1593,  1595,  1607,    90,
    1615,  1578,  1616,   476,  1625,   104,   172,   476,   273,  1627,
    1628,   203,  1629,  1193,  1632,  1509,  1633,  1634,  1636,  1641,
    1648,   476,  1652,  1645,  1653,  1656,  1660,   113,   113,  1668,
      73,   476,  1682,  1555,  1669,   268,  1673,   772,  1674,   436,
    1517,   544,  1684,   172,  1691,  1548,  1724,  1704,    80,    80,
    1695,  1726,  1271,   219,   508,  1728,   503,  1764,  1862,    80,
    1757,    78,    79,  1759,  1773,   495,  1777,   496,   172,   112,
    1778,  1779,   497,  1782,  2018,  1780,  1898,   498,    14,    15,
      16,    17,    18,   957,    83,  1784,  1789,   149,  1795,  1806,
    1796,   499,  1799,  1800,  1809,   141,   834,  1810,  1835,  2001,
    1841,  1840,  1853,    80,  1855,  1861,   870,   870,  1859,  1865,
     476,  1866,  1146,  1873,   476,  2043,  1874,   540,  1876,   476,
     870,   870,   500,   541,  1877,  1880,  1881,   457,   508,   141,
      90,  -648,  1578,  1889,  1890,  1891,   104,  1892,   255,   501,
    1893,  1912,    83,   141,   476,  -555,  1423,   172,  1920,   255,
    1914,   172,  1923,  1925,   870,   870,  1939,   189,   152,  1953,
    1955,  1956,  1931,  1942,    97,   172,    83,    14,    15,    16,
      17,    18,  1250,   255,  1447,   172,  1940,    97,  1109,   229,
    1957,  1037,   253,   780,   255,  1958,    83,   476,  1802,  1565,
    1567,   476,   172,   255,  1969,  2095,  1991,   463,  1978,  1995,
     112,  1993,  2005,   172,   476,  2004,   192,  2006,  2009,   504,
    2010,   193,  2013,  2016,  2029,    80,   194,    80,  2038,  2025,
    2027,   306,   579,   149,  1482,   108,  2042,  1619,   476,    83,
     476,   476,   149,  2044,  2049,   304,   310,  2050,   428,  1215,
     306,  2056,  2067,  2066,  2069,  2079,  2081,   330,  2087,   835,
    2082,  2086,  2090,   476,   172,  2091,  2101,  2102,   172,  1786,
     141,  2105,  1563,   172,  1476,   113,   699,   956,   700,   701,
     702,   419,   189,   189,  1001,  1003,  1002,  1390,    80,    80,
    1004,  1397,  1005,   149,   449,  2080,   747,   253,   172,   476,
    1761,   255,    90,  2036,  1578,  1857,  2019,   703,   104,   476,
     704,   705,  1850,  1942,  2075,   706,   707,  1942,  1942,  1951,
    2073,   229,   229,  2064,  1997,   255,  1755,  1756,  2046,    80,
      90,  1938,  1578,  2088,  1996,   169,   104,   534,   304,  1406,
    1707,   172,   476,  1896,   476,   172,    83,  1964,  1542,  1718,
    1403,  2068,   870,   870,  1202,  1080,     3,  1649,   172,   253,
    1282,   205,   890,   476,   202,   810,   108,  1768,    90,   476,
    1578,  2003,   112,  1235,   104,     0,  2084,  1289,  1290,   476,
    2084,     0,   172,    80,   172,   172,     0,     0,  1722,   310,
       0,  1016,  1017,    80,  1643,   310,   304,   304,  1018,  1647,
     112,   467,  2098,   149,   147,  1938,   113,   172,   664,    65,
      66,    67,    68,    69,    70,    71,     0,  1658,     0,     0,
       0,     0,     0,   330,   616,   625,   412,     0,     0,     0,
     202,     0,   141,   708,   709,     0,     0,  1754,   112,     0,
     330,     0,     0,   172,   330,   202,     0,     0,     0,     0,
    1037,     0,     0,   172,   708,     0,  1297,   780,     0,     0,
     141,   255,     0,     0,     0,     0,   870,     0,     0,   202,
       0,     0,     0,     0,     0,     0,    97,     0,   419,     0,
       0,     0,   452,     0,   708,     0,   172,     0,   172,  1194,
       0,     0,     0,     0,     0,     0,   255,     0,   141,   870,
       0,     0,   255,     0,   870,   870,     0,   172,     0,     0,
       0,     0,   419,   172,     0,   750,     0,     0,     0,     0,
       0,   186,   189,   172,     0,     0,     0,  2099,   108,     0,
     518,     0,     0,     0,   202,     0,     0,  2106,   149,     0,
       0,     0,   449,     0,     0,     0,   779,  1750,   625,     0,
       0,     0,     0,     0,     0,   640,   108,     0,     0,     0,
       0,     0,     0,   274,   412,     0,   412,   275,   113,     0,
     278,   606,   280,     0,     0,     0,     0,     0,     0,   187,
       0,     0,     0,  1771,  1772,     0,   229,     0,     0,     0,
       0,   202,   147,     0,   108,   229,   113,    65,    66,    67,
      68,    69,    70,    71,     0,   412,     0,     0,   306,     0,
    1788,   202,     0,  1373,     0,   304,   257,   419,   419,     0,
       0,   304,     0,   330,     0,     0,     0,   277,     0,     0,
       0,     0,  1488,  1489,   113,     0,     0,  2065,   255,     0,
       0,    75,     0,     0,   799,   606,  1503,  1504,   147,     0,
       0,   170,   171,    65,    66,    67,    68,    69,    70,    71,
       0,   304,     0,     0,     0,     0,     0,     0,     0,     0,
     257,     0,   304,     0,   304,     0,   330,     0,    83,     0,
    1525,  1526,     0,     0,     0,   412,   255,     0,     0,     0,
       0,     0,     0,     0,   330,   449,     0,   625,     0,     0,
     202,     0,     0,     0,     0,   616,     0,     0,     0,   616,
    1500,     0,     0,   755,     0,   274,     0,     0,   330,     0,
       0,   257,     0,     0,     0,     0,     0,  1355,   625,   766,
     202,   330,   769,     0,  1879,    97,     0,     0,     0,   149,
       0,     0,     0,     0,     0,   987,     0,     0,     0,     0,
       0,     0,   419,     0,     0,   149,   149,     0,   419,     0,
       0,     0,     0,     0,    97,     0,     0,   419,   147,  1553,
     149,   149,   149,    65,    66,    67,    68,    69,    70,    71,
    1263,     0,     0,   257,  1264,     0,  1265,     0,     0,   518,
       0,     0,     0,   274,   275,     0,   630,     0,   280,     0,
       0,     0,     0,     0,     0,   202,   202,     0,     0,     0,
       0,   452,     0,   257,     0,     0,     0,    75,     0,   257,
    1468,     0,     0,     0,     0,     0,   449,     0,     0,     0,
       0,  1954,     0,     0,     0,   672,     0,  1226,     0,     0,
       0,     0,   750,   750,     0,     0,   870,   147,     0,   257,
     419,     0,    65,    66,    67,    68,    69,    70,    71,   255,
       0,     0,     0,     0,   202,     0,     0,   449,  1686,  1687,
     779,     0,   779,     0,     0,     0,   314,   315,   316,   317,
    1618,     0,     0,   452,     0,     0,     0,     0,     0,   330,
     330,     0,     0,   673,     0,  1296,    75,     0,     0,     0,
    2083,     0,     0,     0,     0,     0,   202,     0,   330,     0,
     304,     0,     0,     0,   147,     0,  2092,   170,   171,    65,
      66,    67,    68,    69,    70,    71,     0,   202,     0,   304,
     147,   672,   648,   170,   171,    65,    66,    67,    68,    69,
      70,    71,     0,     0,     0,     0,     0,  1355,  1355,  1355,
     152,  1537,  1541,     0,     0,  1553,     0,     0,     0,     0,
       0,  1711,     0,  1553,     0,     0,   318,   419,     0,     0,
       0,     0,   257,     0,   330,     0,    97,    97,     0,     0,
     149,   419,  1774,  1245,   319,     0,     0,     0,  1249,   673,
       0,     0,   330,     0,  1210,   255,     0,     0,     0,  1257,
     274,     0,     0,     0,     0,   616,     0,   649,     0,     0,
       0,     0,   255,     0,   452,  1794,     0,     0,     0,     0,
    1797,  1798,   650,     0,     0,   651,   652,    65,    66,    67,
      68,    69,    70,    71,     0,     0,     0,     0,   202,     0,
     755,   755,     0,     0,     0,   449,   257,     0,  1727,     0,
    1027,     0,  1194,  1030,     0,   452,     0,     0,     0,     0,
       0,  1738,     0,     0,     0,     0,   257,     0,     0,    58,
       0,     0,     0,     0,     0,     0,   477,   452,   452,     0,
       0,     0,     0,   147,     0,     0,   257,   245,    65,    66,
      67,    68,    69,    70,    71,  1263,   452,   255,  1766,  1264,
     147,  1265,     0,   226,   227,    65,    66,    67,    68,    69,
      70,    71,   750,     0,   518,     0,     0,     0,  1098,     0,
       0,   257,  1102,     0,  1823,     0,     0,  1553,    73,   779,
       0,    19,    75,     0,     0,  1663,   779,     0,     0,     0,
       0,     0,     0,     0,     0,   257,     0,     0,   228,    75,
       0,     0,   257,     0,     0,     0,     0,     0,     0,    78,
      79,     0,   452,     0,     0,     0,     0,     0,   664,   202,
      48,    49,    50,    51,    52,    53,    54,    55,   330,     0,
       0,     0,     0,     0,   306,     0,     0,   147,     0,     0,
     226,   227,    65,    66,    67,    68,    69,    70,    71,     0,
       0,     0,  1826,     0,     0,     0,  1836,     0,     0,   109,
       0,     0,     0,     0,    97,    73,     0,     0,   149,   255,
    1849,     0,    58,     0,     0,     0,   419,     0,  1194,     0,
    1858,     0,     0,   202,     0,  1547,    75,     0,  1553,     0,
       0,     0,  1548,     0,     0,   708,    78,    79,     0,     0,
       0,     0,     0,   147,     0,   419,   226,   227,    65,    66,
      67,    68,    69,    70,    71,     0,     0,   109,     0,     0,
       0,     0,   253,    83,     0,     0,     0,     0,     0,     0,
       0,  1458,  1459,  1460,     0,     0,     0,   304,  1461,  1462,
       0,     0,     0,   149,     0,     0,     0,     0,     0,     0,
       0,  1296,    75,   449,     0,     0,     0,     0,     0,  1906,
     755,   267,     0,  1911,     0,     0,     0,     0,  1916,     0,
     147,     0,   255,   170,   171,    65,    66,    67,    68,    69,
      70,    71,   449,     0,     0,   800,   149,   477,     0,     0,
       0,     0,     0,  1946,     0,    97,     0,     0,     0,     0,
     306,     0,     0,     0,   109,     0,     0,     0,     0,     0,
       0,     0,  2051,     0,     0,     0,   452,     0,     0,     0,
       0,     0,   336,     0,    14,    15,    16,    17,    18,     0,
       0,  1329,     0,   257,  1333,     0,  1973,     0,     0,     0,
    1976,     0,     0,  1376,   257,     0,     0,     0,     0,   330,
     330,     0,     0,  1990,     0,     0,     0,     0,   115,   456,
       0,   115,     0,   580,   306,     0,     0,     0,   257,     0,
       0,     0,     0,     0,     0,     0,     0,  2012,     0,  2014,
    2015,     0,     0,    58,     0,     0,     0,     0,   149,   149,
     149,   149,   149,   149,     0,     0,     0,     0,  1549,   310,
       0,     0,  2024,     0,   306,   181,     6,     7,     8,     9,
      10,    11,    12,    13,   147,     0,   115,   419,   419,    65,
      66,    67,    68,    69,    70,    71,     0,     0,     0,     0,
       0,   202,     0,   550,     0,     0,     0,     0,  2047,     0,
     115,   202,    73,     0,     0,     0,     0,   253,  2052,   109,
       0,     0,     0,     0,     0,     0,   259,    97,     0,     0,
     115,     0,    74,    75,     0,     0,     0,     0,     0,   449,
     202,     0,     0,    78,    79,    14,    15,    16,    17,    18,
       0,  2078,     0,  2052,     0,    97,     0,     0,   605,     0,
       0,   267,     0,   149,     0,   616,     0,   115,     0,     0,
       0,     0,  2089,   115,     0,   605,   115,     0,  2078,   605,
     259,     0,     0,     0,     0,     0,     0,     0,  2097,     0,
     326,   115,   358,    97,  1486,     0,     0,     0,     0,     0,
       0,     0,     0,  1495,    58,     0,     0,   452,   452,     0,
       0,     0,     0,     0,   147,   423,     0,   170,   171,    65,
      66,    67,    68,    69,    70,    71,     0,   115,   423,     0,
       0,   259,     0,     0,     0,   147,     0,     0,   226,   227,
      65,    66,    67,    68,    69,    70,    71,     0,     0,     0,
    1696,     0,     0,     0,  1549,     0,   419,     0,     0,     0,
    1549,     0,  1549,    73,     0,    58,     0,     0,     0,     0,
       0,     0,  1762,  1380,     0,     0,     0,   605,   115,     0,
     115,     0,     0,   777,    75,     0,   477,   613,     0,   800,
     310,   149,     0,   259,    78,   778,   147,     0,     0,   226,
     227,    65,    66,    67,    68,    69,    70,    71,     0,     0,
     558,     0,     0,     0,     0,     0,   257,     0,   115,     0,
       0,     0,     0,   259,    73,   419,     0,   202,     0,   259,
       0,     0,     0,     0,     0,     0,   330,   115,     0,   149,
       0,     0,     0,     0,  1999,    75,     0,     0,   508,     0,
       0,   257,     0,     0,     0,    78,    79,   115,   456,   259,
     115,     0,     0,     0,     0,     0,     0,     0,   149,     0,
       0,     0,     0,     0,   115,     0,   147,     0,   115,   170,
     171,    65,    66,    67,    68,    69,    70,    71,     0,   336,
       0,     0,     0,   330,   330,     0,     0,     0,   267,     0,
     109,     0,     0,     0,    99,     0,     0,   153,  1696,  1696,
       0,   456,   423,   109,     0,    14,    15,    16,    17,    18,
       0,     0,     0,  1549,     0,     0,  1549,     0,     0,   605,
     456,   147,     0,     0,   226,   227,    65,    66,    67,    68,
      69,    70,    71,   310,   202,     0,   423,     0,  1699,  1378,
       0,     0,     0,   605,     0,  1266,   419,     0,     0,    73,
       0,     0,    99,  1266,     0,     0,   605,     0,     0,     0,
       0,     0,   115,     0,    58,     0,   423,     0,     0,   228,
      75,     0,   259,   304,     0,     0,   204,     0,     0,     0,
      78,    79,  1266,   257,     0,   477,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   147,   266,     0,   226,   227,
      65,    66,    67,    68,    69,    70,    71,     0,     0,     0,
       0,     0,  1947,     0,   202,     0,     0,     0,  1696,     0,
       0,     0,     0,    73,     0,     0,     0,  1549,     0,     0,
       0,   257,     0,   296,     0,     0,     0,     0,     0,    99,
       0,   423,   423,   228,    75,  1266,   259,   115,     0,     0,
       0,   456,     0,     0,    78,    79,     0,   332,     0,     0,
       0,     0,     0,   149,     0,     0,     0,     0,     0,     0,
       0,   452,   452,     0,     0,     0,     0,     0,   115,     0,
       0,     0,   429,   115,     0,     0,   259,   115,     0,   115,
     330,     0,   456,   296,   455,     0,  1699,  1699,  1696,     0,
     115,     0,   115,     0,     0,     0,     0,     0,   149,     0,
       0,     0,     0,     0,   336,   336,   358,     0,   115,   423,
     147,   259,   502,   198,    64,    65,    66,    67,    68,    69,
      70,    71,     0,   336,     0,     0,   149,   149,   522,  2000,
     310,     0,   115,   527,   529,   259,   204,     0,     0,   558,
       0,     0,   259,     0,     0,   115,     0,   926,     0,     0,
       0,   336,     0,   115,     0,     0,     0,     0,   549,    75,
       0,   551,   799,   552,   149,     0,   423,     0,     0,   115,
     115,     0,   423,     0,   569,     0,    14,    15,    16,    17,
      18,   423,   109,     0,   115,   115,   115,   581,     0,   336,
       0,   477,  2000,  2000,   257,   734,   735,   736,   737,   738,
     739,   740,   741,   742,   743,   744,  1699,   605,    58,   213,
     267,     0,   336,   604,     0,     0,   628,     0,   477,     0,
       0,     0,     0,     0,     0,     0,  1266,     0,     0,     0,
     635,     0,     0,  2000,   635,    58,   745,     0,     0,   147,
     423,     0,   226,   227,    65,    66,    67,    68,    69,    70,
      71,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     456,     0,     0,     0,   423,     0,   147,    73,   452,   226,
     227,    65,    66,    67,    68,    69,    70,    71,     0,     0,
       0,   423,  1962,     0,     0,     0,  1699,   303,    75,     0,
       0,     0,     0,     0,    73,     0,     0,     0,    78,    79,
       0,     0,   147,   115,   115,   553,    64,    65,    66,    67,
      68,    69,    70,    71,  1999,    75,     0,     0,   508,     0,
       0,     0,   115,   336,    58,    78,    79,  1699,   296,     0,
       0,     0,   604,     0,     0,     0,     0,     0,    58,     0,
     336,   336,     0,     0,   477,     0,     0,   257,     0,     0,
     115,     0,     0,     0,  1010,   147,     0,     0,   226,   227,
      65,    66,    67,    68,    69,    70,    71,     0,     0,   147,
       0,     0,     0,   259,    65,    66,    67,    68,    69,    70,
      71,   423,     0,    73,   259,     0,     0,     0,   115,     0,
    1699,  1699,     0,   336,   115,   423,     0,    73,     0,     0,
       0,     0,     0,  1547,    75,     0,   115,     0,  1212,   423,
       0,   115,     0,   455,    78,    79,     0,    74,    75,     0,
       0,     0,     0,     0,     0,   477,     0,     0,    78,    79,
    1266,  1699,     0,     0,     0,  1266,  1266,  1266,     0,     0,
       0,   109,     0,     0,   866,     0,     0,     0,     0,   529,
       0,     0,     0,   877,     0,   569,     0,     0,     0,   423,
       0,     0,     0,     0,     0,     0,   332,     0,    99,     0,
     109,     0,   147,   114,     0,   354,   355,    65,    66,    67,
      68,    69,    70,    71,   635,   901,   147,     0,   267,   170,
     171,    65,    66,    67,    68,    69,    70,    71,     0,   912,
       0,     0,     0,     0,     0,     0,     0,     0,   604,     0,
       0,     0,     0,   921,     0,     0,     0,     0,   605,     0,
       0,   635,   115,    76,     0,     0,     0,     0,   356,     0,
       0,   114,     0,     0,     0,     0,     0,     0,     0,   115,
     115,     0,   588,     0,     0,     0,   336,   456,     0,     0,
       0,     0,   147,     0,   257,   226,   227,    65,    66,    67,
      68,    69,    70,    71,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   269,     0,     0,   147,     0,
      73,   226,   227,    65,    66,    67,    68,    69,    70,    71,
       0,     0,   115,     0,     0,   336,   336,     0,     0,     0,
     777,    75,     0,     0,   613,     0,     0,     0,     0,   336,
     336,    78,   778,     0,   336,   336,   455,     0,   114,     0,
       0,     0,     0,     0,   615,  1266,     0,  1266,     0,     0,
       0,     0,   115,  1019,   895,     0,   340,     0,     0,     0,
     423,     0,   147,   336,   336,   226,   227,    65,    66,    67,
      68,    69,    70,    71,     0,     0,     0,   901,     0,     0,
       0,     0,  1043,     0,     0,     0,     0,     0,     0,   423,
      73,     0,     0,   458,     0,     0,     0,     0,     0,   455,
     455,     0,   109,   109,     0,     0,   259,   115,     0,     0,
    1547,    75,     0,     0,     0,     0,     0,  1548,   455,     0,
       0,    78,    79,     0,     0,   147,     0,   115,   226,   227,
      65,    66,    67,    68,    69,    70,    71,   423,     0,     0,
       0,  1212,     0,     0,     0,     0,   866,     0,     0,     0,
       0,   147,     0,  1444,   456,     0,    65,    66,    67,    68,
      69,    70,    71,  1263,     0,   115,   423,  1264,     0,  1265,
     115,     0,     0,     0,     0,     0,     0,  1179,     0,     0,
       0,  1223,     0,   114,   455,     0,     0,     0,     0,     0,
     153,     0,     0,     0,     0,    14,    15,    16,    17,    18,
      75,     0,   635,  1665,     0,  1214,     0,   866,     0,     0,
       0,     0,  1220,     0,   115,   115,     0,     0,     0,     0,
       0,     0,   607,     0,     0,   269,     0,     0,   115,   115,
       0,     0,     0,   115,   115,     0,     0,     0,     0,   607,
       0,     0,     0,   607,     0,     0,     0,     0,     0,     0,
       0,   336,   336,     0,    58,   332,     0,     0,     0,     0,
       0,     0,   115,   115,     0,     0,     0,     0,     0,     0,
       0,     0,   115,   115,   115,   115,   115,   115,     0,     0,
       0,     0,     0,   259,     0,   147,     0,   336,   226,   227,
      65,    66,    67,    68,    69,    70,    71,     0,     0,     0,
       0,   423,   423,     0,     0,     0,   267,     0,     0,     0,
       0,     0,     0,    73,     0,     0,     0,     0,   866,   147,
       0,     0,   226,   227,    65,    66,    67,    68,    69,    70,
      71,   259,     0,   303,    75,   866,   866,     0,     0,     0,
     109,   607,     0,     0,    78,    79,     0,    73,     0,     0,
       0,   336,   147,   423,     0,   226,   227,    65,    66,    67,
      68,    69,    70,    71,     0,   336,     0,  1999,    75,     0,
       0,   508,     0,     0,     0,     0,     0,   115,    78,    79,
      73,     0,     0,     0,     0,   147,     0,     0,   455,     0,
      65,    66,    67,    68,    69,    70,    71,     0,   336,     0,
     303,    75,     0,   336,   336,     0,     0,     0,   336,   336,
       0,    78,    79,    73,     0,     0,     0,     0,     0,     0,
       0,     0,   458,     0,     0,     0,     0,     0,  1356,     0,
       0,     0,     0,  1036,    75,     0,  1179,   613,     0,     0,
       0,     0,     0,     0,    78,    79,     0,     0,     0,     0,
     115,   115,     0,   340,     0,     0,     0,     0,     0,     0,
       0,     0,   269,     0,   114,  1179,     0,     0,     0,     0,
     423,   109,     0,     0,     0,   458,     0,   114,    14,    15,
      16,    17,    18,  1404,     0,     0,   115,     0,     0,     0,
       0,     0,     0,   607,   458,     0,     0,     0,   119,     0,
       0,   119,     0,     0,   259,   115,     0,     0,     0,     0,
       0,     0,     0,   604,     0,     0,     0,   607,     0,     0,
       0,     0,   527,     0,     0,     0,     0,     0,     0,     0,
     607,     0,     0,     0,     0,     0,     0,    58,     0,   423,
       0,   866,   332,     0,     0,     0,     0,     0,     0,     0,
     115,     0,     0,   115,     0,     0,   119,     0,     0,     0,
       0,     0,     0,     0,   115,     0,     0,     0,   147,     0,
       0,   226,   227,    65,    66,    67,    68,    69,    70,    71,
     119,     0,   115,     0,     0,     0,     0,     0,   605,     0,
     866,   866,     0,     0,     0,     0,    73,   115,     0,     0,
     119,     0,   115,   115,   866,   866,     0,   115,   115,   455,
     455,     0,     0,     0,     0,   336,  1547,    75,     0,     0,
       0,     0,     0,     0,     0,   458,     0,    78,    79,     0,
       0,     0,     0,   109,     0,     0,     0,   119,   866,   866,
       0,     0,     0,   119,     0,     0,   119,     0,  1356,  1356,
    1356,   153,     0,     0,     0,     0,     0,   259,     0,     0,
       0,   109,   605,     0,     0,     0,   458,     0,     0,     0,
     423,     0,     0,     0,     0,     0,     0,  1577,  1577,     0,
       0,     0,     0,     0,     0,   119,     0,     0,   340,   340,
       0,     0,     0,     0,     0,     0,     0,   119,     0,   109,
       0,     0,     0,     0,   375,     0,   376,   340,   377,    63,
      64,    65,    66,    67,    68,    69,    70,    71,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   332,
       0,     0,     0,   147,     0,   340,   555,   556,    65,    66,
      67,    68,    69,    70,    71,   336,     0,     0,   119,     0,
     119,     0,     0,   153,   693,   119,     0,    76,   386,     0,
       0,     0,     0,     0,     0,     0,   114,     0,     0,     0,
       0,   147,     0,   340,    63,    64,    65,    66,    67,    68,
      69,    70,    71,     0,    76,     0,     0,   115,   119,     0,
       0,   607,   147,     0,   269,     0,   340,    65,    66,    67,
      68,    69,    70,    71,  1263,     0,     0,   119,  1264,     0,
    1265,     0,     0,     0,   115,     0,     0,     0,     0,     0,
       0,     0,    76,     0,     0,     0,   866,   866,     0,     0,
       0,     0,   115,     0,     0,     0,     0,     0,     0,     0,
       0,    75,     0,     0,   458,     0,     0,     0,     0,     0,
       0,     0,  1713,     0,     0,     0,     0,     0,     0,     0,
     115,   115,   866,   120,   259,   147,   120,     0,   170,   171,
      65,    66,    67,    68,    69,    70,    71,     0,     0,     0,
     147,  1730,   119,   170,   171,    65,    66,    67,    68,    69,
      70,    71,     0,     0,     0,     0,     0,     0,   115,     0,
       0,     0,     0,     0,     0,     0,     0,   340,     0,     0,
       0,     0,     0,   466,     0,  1577,   119,     0,     0,     0,
       0,   120,     0,     0,   340,   340,   332,   147,   470,   153,
     170,   171,    65,    66,    67,    68,    69,    70,    71,     0,
     866,     0,   119,     0,   115,   120,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   120,     0,     0,     0,     0,
       0,     0,     0,   866,     0,     0,     0,   340,   866,   866,
       0,     0,     0,   455,   455,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1815,
       0,     0,   120,     0,     0,     0,     0,     0,   120,     0,
       0,   120,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   119,   119,     0,     0,   114,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1577,     0,     0,     0,
     120,     0,     0,     0,   114,     0,     0,     0,     0,     0,
       0,     0,   120,   119,     0,     0,     0,   119,     0,   119,
       0,     0,   269,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   119,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   607,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   120,     0,   120,     0,     0,     0,     0,
     120,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     340,   458,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   119,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   120,     0,     0,   119,     0,     0,   119,
     119,     0,   119,  1937,     0,     0,     0,     0,     0,     0,
       0,   119,   120,     0,   119,   119,   119,     0,     0,   340,
     340,     0,   212,     0,     0,     0,     0,     0,   223,   224,
     455,     0,     0,   340,   340,     0,     0,     0,   340,   340,
       0,     0,     0,     0,     0,     0,     0,     0,  1577,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   285,     0,     0,     0,     0,   340,   340,     0,
       0,     0,     0,     0,     0,     0,  1577,  1937,     0,     0,
       0,     0,     0,   124,  1897,     0,   124,   120,     0,     0,
       0,     0,     0,     0,   119,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   114,   114,     0,     0,
       0,     0,     0,     0,  1577,     0,     0,     0,     0,     0,
       0,   120,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   369,     0,     0,   370,     0,   371,     0,   372,
       0,   124,     0,  2040,     0,     0,     0,   120,     0,     0,
       0,     0,     0,     0,     0,     0,   373,     0,   458,     0,
     866,     0,     0,     0,     0,   124,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   374,   375,   124,   376,     0,   377,    63,
      64,    65,    66,    67,    68,    69,    70,    71,   378,   379,
     366,     0,   380,   381,   382,     0,   383,   384,     0,     0,
       0,   119,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,   124,     0,   119,   119,   120,   120,   124,     0,
       0,   124,     0,     0,   385,   577,     0,    76,   386,     0,
       0,     0,     0,     0,   387,    78,    79,   388,   389,   390,
     391,     0,     0,     0,     0,   340,   340,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   120,     0,
     124,     0,   120,     0,   120,     0,     0,     0,     0,     0,
       0,     0,   124,     0,     0,     0,     0,   120,     0,     0,
       0,   340,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     269,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   124,     0,   124,     0,     0,     0,     0,
     124,     0,     0,     0,   114,     0,     0,     0,   120,     0,
       0,     0,     0,     0,     0,   340,     0,     0,     0,     0,
       0,   120,     0,     0,   120,   120,     0,   120,     0,   340,
       0,     0,     0,   124,     0,     0,   120,     0,     0,   120,
     120,   120,     0,   775,     0,   776,     0,     0,     0,     0,
       0,     0,   124,     0,   792,   793,     0,     0,     0,     0,
       0,     0,   340,     0,     0,     0,     0,   340,   340,     0,
       0,     0,   340,   340,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1219,     0,
       0,     0,     0,     0,     0,     0,    14,    15,    16,    17,
      18,     0,     0,     0,     0,     0,     0,     0,     0,   120,
       0,     0,   119,     0,     0,     0,     0,   124,     0,     0,
     119,     0,     0,     0,     0,   114,     0,     0,     0,     0,
       0,     0,   369,     0,     0,   370,     0,   371,     0,   372,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   119,
       0,   124,   876,     0,     0,    58,   373,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   119,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   124,     0,     0,
       0,     0,     0,   374,   375,     0,   376,   119,   377,    63,
      64,    65,    66,    67,    68,    69,    70,    71,   378,   379,
     366,     0,   380,   381,   382,     0,   383,   384,     0,     0,
       0,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   120,     0,     0,     0,
     119,     0,     0,     0,   385,     0,     0,    76,   386,   120,
     120,     0,   607,     0,   387,   448,    79,   388,   389,   390,
     391,     0,     0,     0,     0,     0,   124,   124,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   340,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   114,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   124,     0,
       0,     0,   124,     0,   124,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   114,   607,   124,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   119,   119,   119,   119,   119,   119,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   114,     0,     0,     0,     0,     0,     0,
       0,   119,   119,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1058,     0,     0,   124,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   124,     0,     0,   124,   124,     0,   124,     0,   340,
       0,     0,     0,     0,     0,     0,   124,     0,     0,   124,
     124,   124,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   119,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1126,  1127,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1185,  1186,  1187,     0,     0,  1189,     0,
       0,     0,     0,     0,     0,     0,     0,   120,     0,     0,
       0,     0,     0,     0,     0,   120,     0,     0,     0,   124,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   369,     0,   120,   370,     0,   371,     0,   372,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     119,     0,   120,     0,     0,     0,   373,     0,     0,     1,
       0,     0,   146,     0,     0,     0,     0,     0,     0,     0,
       0,  1259,   120,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   374,   375,   119,   376,     0,   377,    63,
      64,    65,    66,    67,    68,    69,    70,    71,   378,   379,
     366,     0,   380,   381,   382,     0,   383,   384,     0,     0,
       0,     0,     0,     0,    73,   120,     0,  1280,     0,   119,
       0,     0,     0,     0,     0,     0,   124,     0,  1579,  1580,
    1581,     0,     0,   119,   385,  1745,   201,    76,   386,   124,
     124,     0,     0,     0,   387,    78,    79,   388,   389,   390,
     391,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   119,     0,  1304,     0,     0,     0,     0,     0,
       0,     0,     0,  1308,  1309,  1310,  1311,     0,     0,     0,
       0,  1316,  1317,     0,     0,     0,     0,   397,     0,     0,
       0,  1325,     0,     0,     0,   291,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1339,     0,  1340,     0,     0,   120,   120,   120,
     120,   120,   120,     0,   369,     0,     0,   370,     0,   371,
       0,   372,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   120,   120,   373,     0,
     119,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1396,     0,     0,
       0,     0,     0,     0,     0,   374,   375,     0,   472,     0,
     377,    63,    64,    65,    66,    67,    68,    69,    70,    71,
     378,   379,   366,  1410,   380,   381,   382,   291,   383,   384,
    1414,     0,  1416,  1418,     0,     0,    73,     0,     0,     0,
       0,   530,     0,     0,  1427,     0,  1428,     0,  1429,     0,
    1431,   291,   120,     0,     0,  1439,   385,    75,     0,   473,
     474,   291,     0,     0,   475,     0,   387,    78,    79,   388,
     389,   390,   391,     0,     0,   561,   565,     0,     0,     0,
       0,     0,   572,   573,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   583,     0,
       0,     0,     0,     0,     0,     0,   660,   124,     0,     0,
     397,   666,     0,     0,     0,   124,     0,  1483,   602,     0,
     675,   676,     0,     0,  1490,  1491,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   397,   397,     0,     0,     0,
       0,     0,   119,     0,   124,   120,     0,     0,  1514,     0,
       0,     0,     0,     0,     0,  1519,   397,     0,     0,  1520,
       0,     0,   124,     0,     0,     0,     0,     0,     0,     0,
     119,     0,     0,     0,     0,   692,     0,     0,     0,     0,
     120,     0,   124,     0,     0,     0,   397,     0,     0,   223,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   733,     0,   119,     0,
       0,     0,     0,     0,   120,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   167,   124,     0,     0,   120,  1613,
       0,     0,   771,     0,     0,     0,   774,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     167,     0,     0,     0,     0,   796,     0,   120,     0,   797,
     798,     0,     0,   801,     0,  1635,     0,     0,     0,     0,
       0,     0,     0,  1640,     0,  1642,     0,     0,   815,   816,
     817,   818,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   167,   840,     0,     0,
       0,     0,     0,     0,     0,   843,     0,     0,     0,   167,
       0,   167,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1671,  1672,     0,     0,   124,   124,   124,
     124,   124,   124,   291,     0,     0,     0,     0,  1677,  1678,
       0,  1679,   360,     0,     0,   120,     0,     0,     0,     0,
    1683,     0,     0,     0,     0,     0,   124,   124,     0,     0,
    1688,  1689,     0,     0,   881,     0,   360,     0,     0,     0,
       0,   561,     0,     0,     0,     0,     0,   887,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   904,   909,     0,   167,     0,     0,     0,   167,     0,
       0,   167,   167,     0,     0,   167,     0,     0,   167,   167,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   124,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   397,   397,   397,
     397,   397,   397,   397,   397,   397,   397,   397,   397,   397,
     397,   397,   397,   397,   397,   397,     0,     0,     0,   952,
       0,     0,     0,  1775,  1776,     0,     0,     0,     0,   167,
       0,     0,   167,     0,  1783,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   167,   167,     0,     0,   120,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   167,
       0,  1807,  1808,     0,     0,   124,     0,   397,     0,     0,
       0,     0,     0,     0,     0,   120,     0,     0,  1015,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1032,     0,     0,     0,  1033,     0,     0,
     124,     0,     0,     0,     0,     0,   904,     0,     0,     0,
       0,     0,     0,   120,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1073,     0,
       0,     0,     0,     0,   124,     0,     0,  1082,     0,     0,
       0,     0,     0,  1085,     0,     0,     0,     0,   124,  1875,
       0,     0,     0,     0,   167,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1884,
       0,     0,  1885,  1886,     0,     0,     0,   124,     0,  1888,
       1,     0,     0,     0,     0,     0,     0,     1,   369,     0,
       0,   370,     0,   371,     0,   372,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   397,     0,     0,   360,
       0,     0,   373,     0,     1,     0,     0,     0,     0,     0,
       0,   167,     0,     0,     0,   397,     0,     0,     0,     0,
     397,     0,     0,     0,     0,     0,     0,     0,     0,   374,
     375,   397,   376,     0,   377,  1842,    64,    65,    66,    67,
      68,    69,    70,    71,   378,   379,   366,     0,   380,   381,
     382,     0,   383,   384,     0,   124,     0,     0,     0,     0,
      73,  1239,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   397,  1579,  1580,  1581,     0,     0,     0,
     385,  1843,     0,    76,   386,   360,     0,     0,     0,     0,
     387,    78,    79,   388,   389,   390,   391,     0,     0,     0,
    1998,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   167,   167,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   167,
       0,     0,     0,     0,  1287,     0,     0,     0,  1288,     0,
       0,     0,     0,     0,     0,   904,     0,     0,     0,     0,
       0,     0,  2037,     0,     0,  1301,     0,     0,     0,     0,
       0,     0,  1302,     0,     0,     0,     0,     0,     0,     0,
       0,  1306,     0,  1307,     0,     0,  2054,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     397,  2063,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1335,  2076,   124,     0,  1336,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   146,     0,     0,     1,     0,     0,     0,
       0,     0,   347,     0,     0,   124,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   167,
     167,     0,     0,     0,     0,   167,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   397,   445,   347,
       0,     0,     0,   124,     0,     0,   167,     0,     0,   167,
     167,     0,   167,     0,   167,   167,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     511,     0,     0,   397,   397,   397,     0,   511,     0,     0,
     397,   397,     0,     0,     0,     0,     0,  1426,     0,     0,
       0,     0,     0,   167,     0,     0,     0,   167,     0,     0,
       0,     0,     0,     0,   397,     0,     0,    14,    15,    16,
      17,    18,  1450,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,   397,   397,     0,   511,    46,     0,    47,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   167,   167,     0,     0,    58,     0,   347,   617,
       0,     0,     0,     0,     0,     0,   167,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   638,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1523,     0,     0,     0,
    1524,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1559,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     511,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   511,   767,     0,   511,
     770,     0,     0,     0,     0,     0,     0,   347,     0,     0,
    1623,   617,     0,  1626,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   167,     0,
       0,  1637,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   511,     0,     0,     0,   511,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   167,
       0,     0,     0,     0,     0,     0,   167,     0,     0,   167,
       0,     0,     0,  1670,     0,     0,     0,     0,   347,     0,
       0,     0,  1675,     0,     0,     0,  1676,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1680,  1681,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   511,     0,
    1437,   347,     0,     0,     0,     0,     0,     0,    14,    15,
      16,    17,    18,     0,     0,     0,     0,     0,     0,   899,
     347,     0,     0,     0,   397,     0,     0,     0,     0,     0,
     617,     0,     0,     0,   617,     0,     0,     0,     0,     0,
       0,   917,     0,   347,   369,     0,     0,   370,     0,   371,
       0,   372,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   167,     0,     0,     0,     0,    58,   373,     0,
     167,   167,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1769,  1770,     0,     0,
       0,     0,     0,     0,     0,   374,   375,     0,   376,     0,
     377,    63,    64,    65,    66,    67,    68,    69,    70,    71,
     378,   379,   366,     0,   380,   381,   382,     0,   383,   384,
       0,     0,     0,     0,     0,     0,    73,     0,     0,   167,
       0,     0,     0,     0,     0,     0,     0,     0,   167,     0,
       0,   167,     0,   167,   167,     0,   385,     0,     0,    76,
     386,   347,     0,     0,     0,     0,   387,  1438,    79,   388,
     389,   390,   391,     0,     0,     0,     0,   511,   511,     0,
       0,     0,     0,     0,     0,     0,     0,   511,  1028,     0,
     511,  1031,     0,     0,   167,     0,     0,     0,     0,     0,
     165,     0,   347,     0,     0,   617,     0,   617,   617,     0,
       0,     0,     0,     0,   617,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   347,   347,  1860,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   209,   347,   397,     0,     0,   511,     0,     0,
       0,   511,  1626,     0,     0,   511,  1099,     0,   263,   511,
    1103,   167,     0,     0,     0,     0,     0,  1106,     0,     0,
       0,  1887,   283,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   397,   289,     0,   290,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1905,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   209,   347,
     511,     0,   311,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   352,     0,  1933,     0,     0,  1934,   173,
     176,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     617,     0,     0,     0,     0,     0,     0,   209,   167,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     465,     0,     0,   469,   221,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   167,     0,     0,
     347,     0,   397,     0,   397,     0,     0,   512,   513,     0,
       0,   517,     0,     0,   520,   521,     0,     0,     0,     0,
       0,     0,     0,   167,     0,     0,     0,     0,     0,   167,
       0,     0,   209,     0,   298,     0,     0,   299,     0,     0,
       0,     0,     0,   397,     0,   263,     0,     0,     0,  2017,
       0,   320,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   511,     0,     0,
       0,     0,     0,     0,     0,   397,     0,     0,     0,     0,
       0,   469,     0,     0,   617,   617,     0,     0,     0,   209,
       0,   617,     0,     0,     0,     0,     0,     0,   167,   599,
     600,     0,     0,     0,     0,     0,     0,     0,     0,   610,
       0,   627,   487,     0,     0,   632,     0,     0,     0,     0,
       0,     0,     0,   397,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   347,     0,     0,     0,     0,   511,  1330,
       0,   511,  1334,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   538,     0,     0,     0,
       0,     0,     0,     0,   690,     0,   173,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   173,   167,   167,
       0,     0,     0,     0,     0,     0,   360,     0,     0,     0,
     167,     0,     0,     0,     0,     0,     0,     0,   209,     0,
       0,     0,     0,     0,   584,     0,     0,     0,     0,     0,
     765,   587,   589,     0,     0,     0,   596,     0,     0,     0,
       0,   686,     0,     0,     0,     0,     0,     0,   610,     0,
       0,     0,     0,     0,   791,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   642,
       0,     0,     0,     0,   320,     0,     0,   320,   347,     0,
       0,     0,     0,     0,   617,  1435,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   836,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   347,     0,     0,
       0,     0,     0,   167,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   209,   209,     0,     0,     0,     0,   465,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   511,  1487,     0,     0,     0,     0,     0,     0,     0,
     511,  1496,     0,   617,     0,   221,     0,     0,     0,     0,
       0,     0,     0,     0,   347,   347,     0,   794,   795,     0,
     846,   848,   352,     0,     0,     0,     0,   167,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   465,     0,   903,     0,     0,     0,     0,     0,     0,
       0,     0,   915,   916,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   610,   923,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   209,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   690,     0,
       0,   690,   690,     0,   690,     0,     0,     0,     0,     0,
       0,     0,     0,   690,     0,     0,   690,   690,   690,   421,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   450,     0,   347,   167,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   478,     0,   478,   320,     0,
       0,     0,     0,     0,     0,   686,     0,     0,     0,     0,
     617,   686,     0,     0,     0,     0,     0,     0,     0,     0,
     686,     0,   465,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1021,  1022,     0,     0,   686,
       0,  1026,     0,     0,     0,     0,   209,   642,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1047,   465,     0,  1050,  1051,     0,  1054,     0,
    1056,  1057,     0,     0,     0,  1007,     0,     0,     0,     0,
       0,     0,   578,     0,     0,   465,   465,     0,     0,     0,
       0,     0,     0,     0,     0,   511,     0,     0,     0,     0,
       0,     0,     0,     0,   465,     0,     0,     0,     0,  1097,
       0,   511,     0,  1101,   251,   181,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,  -458,  -458,
       0,  -458,    46,     0,    47,     0,  -458,     0,     0,     0,
     465,     0,     0,     0,     0,     0,     0,   209,  1204,  1205,
       0,   347,     0,    58,     0,     0,     0,     0,     0,     0,
     791,     0,  1221,     0,  1060,     0,     0,     0,     0,     0,
       0,  1072,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   347,   347,
       0,   352,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   511,   511,     0,   478,     0,     0,     0,
       0,     0,   478,     0,     0,    76,   252,   812,     0,   511,
       0,     0,     0,    78,    79,     0,  1130,     0,     0,     0,
       0,     0,     0,     0,     0,   369,     0,     0,   370,     0,
     371,     0,   372,     0,     0,     0,     0,     0,   642,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   373,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1221,     0,     0,     0,     0,     0,
    1227,     0,     0,     0,   642,     0,   374,   375,     0,   376,
       0,   377,    63,    64,    65,    66,    67,    68,    69,    70,
      71,   378,   379,   366,   880,   380,   381,   382,     0,   383,
     384,     0,     0,     0,     0,  1321,     0,    73,     0,     0,
       0,     0,  1328,   511,   465,  1332,     0,     0,     0,     0,
       0,   511,     0,   450,     0,     0,     0,   385,  1255,     0,
      76,   386,     0,     0,     0,  1256,   911,   387,    78,    79,
     388,   389,   390,   391,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   690,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   347,     0,     0,   946,   511,
    1963,     0,     0,   511,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   263,     0,
       0,     0,   812,   966,     0,     0,   968,     0,   970,     0,
       0,     0,     0,     0,   979,     0,   984,   979,     0,   209,
       0,     0,     0,     0,   511,     0,     0,     0,  1433,   610,
       0,     0,     0,     0,     0,     0,  1442,  1443,     0,     0,
       0,     0,     0,     0,  1012,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1014,   352,     0,
       0,     0,   690,     0,     0,     0,     0,     0,  1023,     0,
       0,  1377,  1379,  1381,     0,     0,     0,     0,     0,     0,
       0,     0,   450,     0,     0,  1012,     0,   511,   511,     0,
       0,     0,     0,     0,     0,  1485,     0,     0,     0,     0,
       0,  1400,     0,     0,  1494,     0,     0,  1498,     0,  1501,
    1502,     0,  1076,     0,     0,   478,  1130,     0,     0,     0,
       0,     0,     0,     0,     0,   465,   465,     0,   511,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1528,  1107,     0,     0,     0,     0,   642,     0,     0,     0,
       0,     0,     0,     0,   690,   690,   690,     0,   690,   690,
       0,     0,     0,     0,     0,   469,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   421,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1211,  1213,     0,     0,     0,  1620,     0,     0,
     450,     0,     0,   263,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   352,     0,  1568,     0,   979,
    1571,  1585,     0,     0,     0,     0,  1592,     0,     0,     0,
    1596,     0,  1598,  1012,     0,     0,     0,     0,     0,     0,
       0,  1252,     0,     0,     0,     0,     0,     0,   979,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1558,     0,     0,  1560,     0,     0,     0,     0,
       0,     0,     0,     0,  1498,     0,     0,     0,     0,     0,
       0,     0,   369,     0,     0,   370,     0,   371,     0,   372,
       0,     0,     0,     0,   478,     0,     0,     0,     0,     0,
       0,     0,     0,  1685,     0,     0,   373,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   642,   642,     0,
       0,     0,   209,   374,   375,     0,   376,     0,   377,    63,
      64,    65,    66,    67,    68,    69,    70,    71,   378,   379,
     366,     0,   380,   381,   382,     0,   383,   384,     0,     0,
       0,   478,     0,  1320,    73,  1323,   263,     0,     0,     0,
       0,     0,     0,     0,     0,  1690,     0,     0,     0,     0,
       0,     0,     0,     0,   385,   975,  1561,    76,   386,     0,
       0,     0,     0,     0,   387,    78,    79,   388,   389,   390,
     391,     0,     0,     0,  1767,     0,     0,  1723,     0,     0,
       0,     0,   352,     0,     0,     0,     0,     0,     0,  1729,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1388,  1388,     0,  1744,  1746,     0,     0,     0,     0,
       0,     0,     0,     0,   690,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1571,  1719,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   465,
     465,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1819,  1820,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1430,  1824,     0,     0,     0,
       0,  1440,     0,     0,     0,     0,     0,     0,     0,     0,
     478,     0,     0,     0,     0,     0,     0,     0,     0,   263,
     450,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   478,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   642,     0,
       0,     0,   979,     0,     0,   812,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1834,     0,     0,     0,     0,     0,     0,     0,
       0,  1837,     0,  1839,     0,     0,  1844,  1848,     0,  1585,
       0,     0,     0,     0,  1854,     0,     0,     0,     0,  1895,
       0,     0,     0,     0,     0,     0,     0,  1522,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,   979,    46,     0,    47,   690,
       0,     0,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,   478,     0,     0,   812,    58,     0,     0,
       0,     0,  1871,  1961,     0,     0,   465,     0,     0,     0,
       0,     0,     0,   642,     0,     0,     0,     0,     0,     0,
       0,     0,  1922,     0,     0,     0,     0,  1927,  1929,     0,
       0,    63,    64,  2077,     0,     0,     0,   966,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1638,  1639,     0,
    1372,     0,     0,   690,     0,     0,   469,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   478,     0,   812,     0,     0,    76,
       0,   369,     0,     0,   370,     0,   371,     0,   372,  1977,
       0,  1980,     0,     0,  1982,  1984,     0,     0,     0,  1987,
    1989,     0,     0,  1132,     0,   373,    -2,     0,  1134,  -243,
    -243,  1135,  1136,  1137,  1138,  1139,  1140,  1141,  1142,  1143,
    1144,  1145,  1146,  -337,  1147,  1148,  1149,  1150,  1151,     0,
    1152,     0,   374,   375,     0,   472,     0,   377,  1153,  1154,
      65,    66,    67,    68,    69,    70,    71,   378,   379,   366,
    1155,   380,   381,   382,   421,   383,   384,     0,     0,  1712,
       0,     0,     0,    73,     0,     0,     0,     0,  2031,  2033,
    2035,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -243,   385,     0,     0,    76,   386,     0,  2048,
       0,   287,     0,   387,    78,    79,   388,   389,   390,   391,
    2077,     0,  2058,  2060,  2062,     0,     0,     0,  -184,     0,
       0,     0,     0,     0,     0,     0,     0,  1372,     0,  1758,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   369,     0,
       0,   370,     0,   371,     0,   372,     0,     0,     0,     0,
       0,     0,     0,     0,  1785,     0,     0,  1787,     0,     0,
    1132,     0,   373,    -2,     0,  1134,  -244,  -244,  1135,  1136,
    1137,  1138,  1139,  1140,  1141,  1142,  1143,  1144,  1145,  1146,
    -337,  1147,  1148,  1149,  1150,  1151,     0,  1152,     0,   374,
     375,     0,   472,     0,   377,  1153,  1154,    65,    66,    67,
      68,    69,    70,    71,   378,   379,   366,  1155,   380,   381,
     382,     0,   383,   384,     0,     0,     0,     0,  1765,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1372,     0,     0,     0,  -244,
     385,     0,     0,    76,   386,     0,     0,     0,   287,     0,
     387,    78,    79,   388,   389,   390,   391,     0,     0,     0,
       0,     0,     0,     0,     0,  -184,   369,     0,     0,   370,
       0,   371,     0,   372,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1132,     0,
     373,    -2,     0,  1134,     0,     0,  1135,  1136,  1137,  1138,
    1139,  1140,  1141,  1142,  1143,  1144,  1145,  1146,  -337,  1147,
    1148,  1149,  1150,  1151,     0,  1152,     0,   374,   375,     0,
     472,     0,   377,  1153,  1154,    65,    66,    67,    68,    69,
      70,    71,   378,   379,   366,  1155,   380,   381,   382,     0,
     383,   384,     0,     0,     0,     0,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   385,   979,
       0,    76,   386,     0,     0,     0,   287,     0,   387,    78,
      79,   388,   389,   390,   391,     0,     0,     0,     0,     0,
       0,     0,     0,  -184,     4,   181,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,  1131,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
     369,     0,    46,   370,    47,   371,     0,   372,    48,    49,
      50,    51,    52,    53,    54,    55,    56,     0,     0,     0,
      57,     0,  1132,    58,  1133,    -2,     0,  1134,     0,     0,
    1135,  1136,  1137,  1138,  1139,  1140,  1141,  1142,  1143,  1144,
    1145,  1146,  -337,  1147,  1148,  1149,  1150,  1151,     0,  1152,
       0,   374,   375,    61,   472,     0,   377,  1153,  1154,    65,
      66,    67,    68,    69,    70,    71,   378,   379,   366,  1155,
     380,   381,   382,     0,   383,   384,     0,     0,     0,     0,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    -3,   385,     0,     0,    76,   417,     0,     0,     0,
     287,     0,   387,    78,    79,   388,   389,   390,   391,     0,
       0,     0,     0,     0,     0,     0,     0,  -184,     4,   181,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,  1131,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,   369,     0,    46,   370,    47,   371,
       0,   372,    48,    49,    50,    51,    52,    53,    54,    55,
      56,     0,     0,     0,    57,     0,  1132,    58,  1133,    -2,
       0,  1134,     0,     0,  1135,  1136,  1137,  1138,  1139,  1140,
    1141,  1142,  1143,  1144,  1145,  1146,  -337,  1147,  1148,  1149,
    1150,  1151,     0,  1152,     0,   374,   375,    61,   472,     0,
     377,  1153,  1154,    65,    66,    67,    68,    69,    70,    71,
     378,   379,   366,  1155,   380,   381,   382,     0,   383,   384,
       0,     0,     0,     0,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   385,     0,     0,    76,
     417,     0,     0,     0,   287,     0,   387,    78,    79,   388,
     389,   390,   391,     0,     0,     0,     0,     0,     0,     0,
       0,  -184,     4,   181,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,   369,     0,
      46,   370,    47,   371,     0,   372,    48,    49,    50,    51,
      52,    53,    54,    55,    56,     0,     0,     0,    57,     0,
       0,    58,   373,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   374,
     375,    61,   376,     0,   377,    63,    64,    65,    66,    67,
      68,    69,    70,    71,   378,   379,   366,     0,   380,   381,
     382,     0,   383,   384,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1579,  1580,  1581,     0,     0,     0,
     385,  1582,  1583,    76,   417,     0,     0,     0,     0,     0,
     387,    78,    79,   388,   389,   390,   391,     0,     0,     0,
       0,     0,     0,     0,     0,  1584,     4,   181,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   369,     0,    46,   370,    47,   371,     0,   372,
      48,    49,    50,    51,    52,    53,    54,    55,    56,     0,
       0,     0,    57,     0,     0,    58,   373,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   374,   375,    61,   376,     0,   377,    63,
      64,    65,    66,    67,    68,    69,    70,    71,   378,   379,
     366,     0,   380,   381,   382,     0,   383,   384,     0,     0,
       0,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1579,  1580,
    1581,     0,     0,     0,   385,  1582,     0,    76,   417,     0,
       0,     0,     0,     0,   387,    78,    79,   388,   389,   390,
     391,     0,     0,     0,     0,     0,     0,     0,     0,  1584,
       4,   181,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,   369,     0,    46,   370,
      47,   371,     0,   372,    48,    49,    50,    51,    52,    53,
      54,    55,    56,     0,     0,     0,    57,     0,     0,    58,
     373,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   374,   375,    61,
     376,     0,   377,    63,    64,    65,    66,    67,    68,    69,
      70,    71,   378,   379,   366,     0,   380,   381,   382,     0,
     383,   384,     0,     0,     0,     0,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   385,     0,
    1570,    76,   417,     0,     0,     0,     0,     0,   387,    78,
      79,   388,   389,   390,   391,     4,   181,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   369,     0,    46,   370,    47,   371,     0,   372,    48,
      49,    50,    51,    52,    53,    54,    55,    56,     0,     0,
       0,    57,     0,     0,    58,   373,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   374,   375,    61,   376,     0,   377,    63,    64,
      65,    66,    67,    68,    69,    70,    71,   378,   379,   366,
       0,   380,   381,   382,     0,   383,   384,     0,     0,     0,
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   385,     0,     0,    76,   417,     0,     0,
       0,     0,     0,   387,    78,    79,   388,   389,   390,   391,
     181,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,   369,     0,    46,   370,    47,
     371,     0,   372,   327,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,   373,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   374,   375,     0,   376,
       0,   377,    63,    64,    65,    66,    67,    68,    69,    70,
      71,   378,   379,   366,     0,   380,   381,   382,     0,   383,
     384,     0,     0,     0,     0,     0,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   385,     0,     0,
      76,   447,     0,     0,     0,     0,     0,   387,   448,    79,
     388,   389,   390,   391,   181,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,   369,
       0,    46,   370,    47,   371,     0,   372,   327,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,   373,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     374,   375,     0,   376,     0,   377,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   378,   379,   366,     0,   380,
     381,   382,     0,   383,   384,     0,     0,     0,     0,     0,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   385,     0,     0,    76,  1208,     0,     0,     0,     0,
       0,   387,  1209,    79,   388,   389,   390,   391,   181,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   369,     0,    46,   370,    47,   371,     0,
     372,   327,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,   373,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   374,   375,     0,   376,     0,   377,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   378,
     379,   366,     0,   380,   381,   382,     0,   383,   384,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   385,     0,     0,    76,   386,
       0,     0,     0,     0,     0,   387,    78,    79,   388,   389,
     390,   391,   181,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,   369,     0,    46,
     370,    47,   371,     0,   372,   327,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,   373,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   374,   375,
       0,   376,     0,   377,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   378,   379,   366,     0,   380,   381,   382,
       0,   383,   384,     0,     0,     0,     0,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   385,
       0,     0,    76,   447,     0,     0,     0,     0,     0,   387,
      78,    79,   388,   389,   390,   391,  1904,     0,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,     0,    -2,     0,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,     0,    -2,    -2,     0,    -2,     0,
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
      -2,     0,     0,     0,     0,     0,     0,    -2,    -2,  1932,
       0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,    -2,     0,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,    -2,    -2,
       0,    -2,     0,    -2,     0,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,     0,     0,     0,    -2,     0,     0,
      -2,     0,     0,     0,     0,    -2,    -2,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      -2,     0,     0,     0,    -2,    -2,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    -2,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    -2,     0,
       0,     0,    -2,    -2,     0,     0,     0,     0,     0,     0,
      -2,    -2,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,    48,    49,    50,    51,
      52,    53,    54,    55,    56,     0,     0,     0,    57,     0,
       0,    58,    59,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    60,     0,     0,
       0,    61,    62,     0,     0,    63,    64,    65,    66,    67,
      68,    69,    70,    71,     0,     0,     0,    72,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,    75,     0,    76,    77,     0,     0,     0,     0,     0,
       0,    78,    79,   251,   181,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,     0,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,  -458,  -458,     0,
    -458,    46,     0,    47,     0,  -458,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   147,     0,     0,    63,    64,    65,    66,
      67,    68,    69,    70,    71,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,    75,     0,    76,   252,     0,     0,     0,  -778,
       0,     0,    78,    79,     4,   181,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,    48,    49,
      50,    51,    52,    53,    54,    55,    56,     0,     0,     0,
      57,     0,     0,    58,     0,     0,     0,     0,  -390,  -390,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    61,     0,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -390,     0,     0,     0,    76,    77,     0,     0,     0,
       0,     0,     0,    78,    79,     4,   181,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,    56,     0,     0,
       0,    57,     0,     0,    58,     0,     0,     0,     0,  -391,
    -391,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    61,     0,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -391,     0,     0,     0,    76,    77,     0,     0,
       0,     0,     0,     0,    78,    79,   251,   181,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,     0,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
    -458,  -458,     0,  -458,    46,     0,    47,     0,  -458,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   147,     0,     0,    63,
      64,    65,    66,    67,    68,    69,    70,    71,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,    75,     0,    76,   252,     0,
    1348,     0,     0,     0,     0,    78,    79,  1349,     0,     0,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
    1350,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    61,
       0,     0,     0,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1351,     0,     0,
       0,    76,   942,     0,  1348,     0,     0,     0,     0,    78,
      79,  1349,     0,     0,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,  1350,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    61,     0,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1534,     0,     0,     0,    76,   942,     0,  1348,     0,
       0,     0,     0,    78,    79,  1349,     0,     0,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
       0,     0,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,  1350,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    61,     0,     0,
       0,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1535,     0,     0,     0,    76,
     942,     0,  1348,     0,     0,     0,     0,    78,    79,  1349,
       0,     0,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,  1350,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    61,     0,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   251,     0,     0,  1536,
       0,     0,     0,    76,   942,     0,    14,    15,    16,    17,
      18,    78,    79,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
    -458,  -458,     0,  -458,    46,     0,    47,     0,  -458,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    76,   309,     0,
       0,     0,     0,     0,     0,    78,    79,   181,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,     0,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
    -458,  -458,     0,  -458,    46,     0,    47,     0,  -458,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   147,     0,     0,    63,
      64,    65,    66,    67,    68,    69,    70,    71,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,    75,     0,    76,   252,     0,
       0,     0,  -782,     0,     0,    78,    79,   181,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,     0,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
    -458,  -458,     0,  -458,    46,     0,    47,     0,  -458,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   147,     0,     0,    63,
      64,    65,    66,    67,    68,    69,    70,    71,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,    75,     0,    76,   252,     0,
       0,     0,     0,     0,     0,    78,    79,   181,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
     327,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    73,     0,  1067,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -644,    76,   329,     0,
       0,     0,     0,     0,     0,    78,    79,   181,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
     327,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   328,    76,   329,     0,
       0,     0,     0,     0,     0,    78,    79,   181,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
     327,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    73,     0,  1803,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    76,   329,     0,
       0,     0,     0,     0,     0,    78,    79,   181,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
     327,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    73,     0,  1805,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    76,   329,     0,
       0,     0,     0,     0,     0,    78,    79,   181,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
     327,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    76,   309,     0,
       0,     0,     0,     0,     0,    78,    79,   181,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
     327,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    76,   329,     0,
       0,     0,     0,     0,     0,    78,    79,   181,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,     0,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
    -458,  -458,     0,  -458,    46,     0,    47,     0,  -458,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    63,
      64,     0,     0,     0,  1372,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   369,     0,     0,   370,     0,
     371,     0,   372,     0,     0,     0,     0,    76,   252,     0,
       0,     0,     0,     0,     0,    78,    79,  1132,     0,   373,
       0,     0,  1134,  1827,  1828,  1135,  1136,  1137,  1138,  1139,
    1140,  1141,  1142,  1143,  1144,  1145,  1146,  -337,  1147,  1148,
    1149,  1150,  1151,     0,  1152,     0,   374,   375,     0,   472,
       0,   377,  1153,  1154,    65,    66,    67,    68,    69,    70,
      71,   378,   379,   366,  1155,   380,   381,   382,  1372,   383,
     384,     0,     0,     0,     0,     0,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   385,     0,   369,
      76,   386,   370,     0,   371,   287,   372,   387,    78,    79,
     388,   389,   390,   391,     0,     0,     0,     0,     0,     0,
       0,  1132,  -184,   373,     0,     0,  1134,     0,     0,  1135,
    1136,  1137,  1138,  1139,  1140,  1141,  1142,  1143,  1144,  1145,
    1146,  -337,  1147,  1148,  1149,  1150,  1151,     0,  1152,     0,
     374,   375,     0,   472,     0,   377,  1153,  1154,    65,    66,
      67,    68,    69,    70,    71,   378,   379,   366,  1155,   380,
     381,   382,     0,   383,   384,     0,     0,     0,     0,     0,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   385,     0,     0,    76,   386,     0,     0,     0,   287,
       0,   387,    78,    79,   388,   389,   390,   391,     0,     0,
       0,     0,     0,     0,     0,     0,  -184,    14,    15,    16,
      17,    18,    19,   677,    20,   678,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   369,     0,    46,   370,    47,   371,     0,
     372,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,   373,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   679,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   374,   375,     0,   376,     0,   377,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   378,
     379,   366,     0,   380,   381,   382,     0,   383,   384,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   385,     0,     0,    76,   680,
       0,     0,     0,   287,     0,   387,    78,    79,   681,   682,
     390,   391,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,   369,     0,
      46,   370,    47,   371,     0,   372,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,   373,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   374,
     375,     0,   376,     0,   377,    63,    64,    65,    66,    67,
      68,    69,    70,    71,   378,   379,   366,     0,   380,   381,
     382,     0,   383,   384,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     385,     0,   416,    76,   417,     0,     0,     0,     0,     0,
     387,    78,    79,   388,   389,   390,   391,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   369,     0,    46,   370,    47,   371,     0,
     372,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,   373,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   374,   375,     0,   376,     0,   377,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   378,
     379,   366,     0,   380,   381,   382,     0,   383,   384,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   385,     0,     0,    76,   680,
       0,     0,     0,   287,     0,   387,    78,    79,   388,   389,
     390,   391,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,   369,     0,
      46,   370,    47,   371,     0,   372,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,   373,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   374,
     375,     0,   376,     0,   377,    63,    64,    65,    66,    67,
      68,    69,    70,    71,   378,   379,   366,     0,   380,   381,
     382,     0,   383,   384,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     385,     0,     0,    76,   417,     0,     0,     0,     0,     0,
     387,    78,    79,   388,   389,   390,   391,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   369,     0,    46,   370,    47,   371,     0,
     372,   327,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,   373,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   374,   375,     0,   376,     0,   377,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   378,
     379,   366,     0,   380,   381,   382,     0,   383,   384,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   385,     0,     0,    76,   447,
       0,     0,     0,     0,     0,   387,    78,    79,   388,   389,
     390,   391,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,   369,     0,
      46,   370,    47,   371,     0,   372,   327,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,   373,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   374,
     375,     0,   376,     0,   377,    63,    64,    65,    66,    67,
      68,    69,    70,    71,   378,   379,   366,     0,   380,   381,
     382,     0,   383,   384,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     385,     0,     0,    76,   386,     0,     0,     0,     0,     0,
     387,    78,    79,   388,   389,   390,   391,   181,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,     0,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   668,     0,     0,   669,
     670,     0,   566,   181,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,   -16,    47,     0,     0,     0,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     251,   181,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    63,    64,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,  -458,  -458,     0,  -458,    46,     0,
      47,     0,  -458,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    76,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    63,    64,     0,     0,     0,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
       0,    76,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   147,     0,
       0,    63,    64,    65,    66,    67,    68,    69,    70,    71,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,    75,     0,    76,
      77,     0,     0,     0,  -780,     0,     0,    78,    79,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   147,
       0,     0,    63,    64,    65,    66,    67,    68,    69,    70,
      71,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,    75,     0,
      76,    77,     0,     0,     0,     0,     0,     0,    78,    79,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     147,     0,     0,    63,    64,    65,    66,    67,    68,    69,
      70,    71,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    76,    77,     0,     0,     0,     0,     0,     0,    78,
      79,   181,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,   327,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     865,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -657,    76,   181,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,   327,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1720,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    14,    15,    16,    17,    18,     0,
       0,    20,    76,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,  -458,  -458,
       0,  -458,    46,     0,    47,     0,  -458,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   147,     0,     0,    63,    64,    65,
      66,    67,    68,    69,    70,    71,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,    75,     0,    76,   309,     0,     0,     0,
       0,     0,     0,    78,    79,   181,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,   327,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    63,    64,     0,
       0,     0,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,    76,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   948,    76,   942,     0,     0,     0,     0,     0,
       0,    78,    79,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,  1454,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    76,   942,     0,     0,     0,     0,
       0,     0,    78,    79,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    76,   294,     0,     0,     0,
       0,     0,     0,    78,    79,    14,    15,    16,    17,    18,
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
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    76,    77,     0,     0,
       0,     0,     0,     0,    78,    79,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    76,   443,     0,
       0,     0,     0,     0,     0,    78,    79,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,   327,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    76,   329,
       0,     0,     0,     0,     0,     0,    78,    79,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
       0,     0,   327,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    76,
     294,     0,     0,     0,     0,     0,     0,    78,    79,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,   327,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      76,   443,     0,     0,     0,     0,     0,     0,    78,    79,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,   327,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    76,   309,     0,     0,     0,     0,     0,     0,    78,
      79,    14,    15,    16,    17,    18,    19,     0,    20,     0,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    76,   942,     0,     0,     0,     0,     0,     0,
      78,    79,   181,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,     0,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,  -458,  -458,     0,  -458,    46,
       0,    47,     0,  -458,     0,     0,     0,     0,     0,     0,
       0,     0,    14,    15,    16,    17,    18,    19,     0,    20,
      58,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,    63,    64,   327,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    76,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    76,   942,     0,     0,     0,     0,     0,
       0,    78,    79,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,   327,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    76,     0,     0,    14,    15,    16,
      17,    18,    78,    79,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -458,  -458,     0,  -458,    46,     0,    47,     0,  -458,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    76,   309,
       0,    14,    15,    16,    17,    18,    78,    79,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,  -458,  -458,     0,  -458,    46,
       0,    47,     0,  -458,     0,     0,     0,     0,     0,     0,
       0,     0,    14,    15,    16,    17,    18,    19,     0,    20,
      58,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,    63,    64,   327,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    76,     0,     0,    63,    64,     0,     0,     0,
      78,    79,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,    76,     0,     0,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   147,     0,     0,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    14,    15,    16,    17,    18,     0,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,  -458,  -458,
       0,  -458,    46,    76,    47,     0,  -458,     0,   369,     0,
       0,   370,     0,   371,     0,   372,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,   373,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    63,    64,   374,
     375,     0,   376,     0,   377,    63,    64,    65,    66,    67,
      68,    69,    70,    71,   378,   379,   366,     0,   380,   381,
     382,     0,   383,   384,   369,     0,     0,   370,     0,   371,
      73,   372,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    76,     0,     0,   373,     0,
     385,     0,     0,    76,   386,     0,     0,     0,   475,     0,
     387,    78,    79,   388,   389,   390,   391,     0,     0,     0,
       0,     0,     0,     0,     0,   374,   375,     0,   376,     0,
     377,    63,    64,    65,    66,    67,    68,    69,    70,    71,
     378,   379,   366,     0,   380,   381,   382,   369,   383,   384,
     370,     0,   371,     0,   372,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   373,     0,     0,     0,     0,   385,   811,     0,    76,
     386,     0,     0,     0,     0,     0,   387,    78,    79,   388,
     389,   390,   391,     0,     0,     0,     0,     0,   374,   375,
       0,   376,     0,   377,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   378,   379,   366,     0,   380,   381,   382,
     369,   383,   384,   370,     0,   371,     0,   372,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   373,     0,     0,     0,     0,   385,
       0,     0,    76,   386,     0,     0,     0,   287,     0,   387,
      78,    79,   388,   389,   390,   391,     0,     0,     0,     0,
       0,   374,   375,     0,   376,     0,   377,    63,    64,    65,
      66,    67,    68,    69,    70,    71,   378,   379,   366,     0,
     380,   381,   382,   369,   383,   384,   370,     0,   371,     0,
     372,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   373,     0,     0,
       0,     0,   385,   975,     0,    76,   386,     0,     0,     0,
       0,     0,   387,    78,    79,   388,   389,   390,   391,     0,
       0,     0,     0,     0,   374,   375,     0,   376,     0,   377,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   378,
     379,   366,     0,   380,   381,   382,   369,   383,   384,   370,
       0,   371,     0,   372,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     373,     0,     0,     0,     0,   385,     0,     0,    76,   386,
       0,     0,  1006,     0,     0,   387,    78,    79,   388,   389,
     390,   391,     0,     0,     0,     0,     0,   374,   375,     0,
     376,     0,   377,    63,    64,    65,    66,    67,    68,    69,
      70,    71,   378,   379,   366,     0,   380,   381,   382,   369,
     383,   384,   370,     0,   371,     0,   372,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   373,     0,     0,     0,     0,   385,     0,
       0,    76,   386,     0,     0,     0,  1229,     0,   387,    78,
      79,   388,   389,   390,   391,     0,     0,     0,     0,     0,
     374,   375,     0,   376,     0,   377,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   378,   379,   366,     0,   380,
     381,   382,   369,   383,   384,   370,     0,   371,     0,   372,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   373,     0,     0,     0,
       0,   385,  1322,     0,    76,   386,     0,     0,     0,     0,
       0,   387,    78,    79,   388,   389,   390,   391,     0,     0,
       0,     0,     0,   374,   375,     0,   376,     0,   377,    63,
      64,    65,    66,    67,    68,    69,    70,    71,   378,   379,
     366,     0,   380,   381,   382,   369,   383,   384,   370,     0,
     371,     0,   372,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   373,
       0,     0,     0,     0,   385,     0,     0,    76,   386,     0,
       0,     0,  1382,     0,   387,    78,    79,   388,   389,   390,
     391,     0,     0,     0,     0,     0,   374,   375,     0,   376,
       0,   377,    63,    64,    65,    66,    67,    68,    69,    70,
      71,   378,   379,   366,     0,   380,   381,   382,   369,   383,
     384,   370,     0,   371,     0,   372,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   373,     0,     0,     0,     0,   385,     0,  1833,
      76,   386,     0,     0,     0,     0,     0,   387,    78,    79,
     388,   389,   390,   391,     0,     0,     0,     0,     0,   374,
     375,     0,   376,     0,   377,    63,    64,    65,    66,    67,
      68,    69,    70,    71,   378,   379,   366,     0,   380,   381,
     382,   369,   383,   384,   370,     0,   371,     0,   372,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   373,     0,     0,     0,     0,
     385,  1838,     0,    76,   386,     0,     0,     0,     0,     0,
     387,    78,    79,   388,   389,   390,   391,     0,     0,     0,
       0,     0,   374,   375,     0,   376,     0,   377,    63,    64,
      65,    66,    67,    68,    69,    70,    71,   378,   379,   366,
       0,   380,   381,   382,   369,   383,   384,   370,     0,   371,
       0,   372,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   373,     0,
       0,     0,     0,   385,  1847,     0,    76,   386,     0,     0,
       0,     0,     0,   387,    78,    79,   388,   389,   390,   391,
       0,     0,     0,     0,     0,   374,   375,     0,   376,     0,
     377,    63,    64,    65,    66,    67,    68,    69,    70,    71,
     378,   379,   366,     0,   380,   381,   382,   369,   383,   384,
     370,     0,   371,     0,   372,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   373,     0,     0,     0,     0,   385,  1926,     0,    76,
     386,     0,     0,     0,     0,     0,   387,    78,    79,   388,
     389,   390,   391,     0,     0,     0,     0,     0,   374,   375,
       0,   376,     0,   377,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   378,   379,   366,     0,   380,   381,   382,
     369,   383,   384,   370,     0,   371,     0,   372,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   373,     0,     0,     0,     0,   385,
    1928,     0,    76,   386,     0,     0,     0,     0,     0,   387,
      78,    79,   388,   389,   390,   391,     0,     0,     0,     0,
       0,   374,   375,     0,   376,     0,   377,    63,    64,    65,
      66,    67,    68,    69,    70,    71,   378,   379,   366,     0,
     380,   381,   382,   369,   383,   384,   370,     0,   371,     0,
     372,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   373,     0,     0,
       0,     0,   385,  1979,     0,    76,   386,     0,     0,     0,
       0,     0,   387,    78,    79,   388,   389,   390,   391,     0,
       0,     0,     0,     0,   374,   375,     0,   376,     0,   377,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   378,
     379,   366,     0,   380,   381,   382,   369,   383,   384,   370,
       0,   371,     0,   372,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     373,     0,     0,     0,     0,   385,  1981,     0,    76,   386,
       0,     0,     0,     0,     0,   387,    78,    79,   388,   389,
     390,   391,     0,     0,     0,     0,     0,   374,   375,     0,
     376,     0,   377,    63,    64,    65,    66,    67,    68,    69,
      70,    71,   378,   379,   366,     0,   380,   381,   382,   369,
     383,   384,   370,     0,   371,     0,   372,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   373,     0,     0,     0,     0,   385,  1983,
       0,    76,   386,     0,     0,     0,     0,     0,   387,    78,
      79,   388,   389,   390,   391,     0,     0,     0,     0,     0,
     374,   375,     0,   376,     0,   377,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   378,   379,   366,     0,   380,
     381,   382,   369,   383,   384,   370,     0,   371,     0,   372,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   373,     0,     0,     0,
       0,   385,  1986,     0,    76,   386,     0,     0,     0,     0,
       0,   387,    78,    79,   388,   389,   390,   391,     0,     0,
       0,     0,     0,   374,   375,     0,   376,     0,   377,    63,
      64,    65,    66,    67,    68,    69,    70,    71,   378,   379,
     366,     0,   380,   381,   382,   369,   383,   384,   370,     0,
     371,     0,   372,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   373,
       0,     0,     0,     0,   385,  1988,     0,    76,   386,     0,
       0,     0,     0,     0,   387,    78,    79,   388,   389,   390,
     391,     0,     0,     0,     0,     0,   374,   375,     0,   376,
       0,   377,    63,    64,    65,    66,    67,    68,    69,    70,
      71,   378,   379,   366,     0,   380,   381,   382,   369,   383,
     384,   370,     0,   371,     0,   372,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   373,     0,     0,     0,     0,   385,  2030,     0,
      76,   386,     0,     0,     0,     0,     0,   387,    78,    79,
     388,   389,   390,   391,     0,     0,     0,     0,     0,   374,
     375,     0,   376,     0,   377,    63,    64,    65,    66,    67,
      68,    69,    70,    71,   378,   379,   366,     0,   380,   381,
     382,   369,   383,   384,   370,     0,   371,     0,   372,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   373,     0,     0,     0,     0,
     385,  2032,     0,    76,   386,     0,     0,     0,     0,     0,
     387,    78,    79,   388,   389,   390,   391,     0,     0,     0,
       0,     0,   374,   375,     0,   376,     0,   377,    63,    64,
      65,    66,    67,    68,    69,    70,    71,   378,   379,   366,
       0,   380,   381,   382,   369,   383,   384,   370,     0,   371,
       0,   372,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   373,     0,
       0,     0,     0,   385,  2034,     0,    76,   386,     0,     0,
       0,     0,     0,   387,    78,    79,   388,   389,   390,   391,
       0,     0,     0,     0,     0,   374,   375,     0,   376,     0,
     377,    63,    64,    65,    66,    67,    68,    69,    70,    71,
     378,   379,   366,     0,   380,   381,   382,   369,   383,   384,
     370,     0,   371,     0,   372,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   373,     0,     0,     0,     0,   385,  2057,     0,    76,
     386,     0,     0,     0,     0,     0,   387,    78,    79,   388,
     389,   390,   391,     0,     0,     0,     0,     0,   374,   375,
       0,   376,     0,   377,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   378,   379,   366,     0,   380,   381,   382,
     369,   383,   384,   370,     0,   371,     0,   372,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   373,     0,     0,     0,     0,   385,
    2059,     0,    76,   386,     0,     0,     0,     0,     0,   387,
      78,    79,   388,   389,   390,   391,     0,     0,     0,     0,
       0,   374,   375,     0,   376,     0,   377,    63,    64,    65,
      66,    67,    68,    69,    70,    71,   378,   379,   366,     0,
     380,   381,   382,   369,   383,   384,   370,     0,   371,     0,
     372,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   373,     0,     0,
       0,     0,   385,  2061,     0,    76,   386,     0,     0,     0,
       0,     0,   387,    78,    79,   388,   389,   390,   391,     0,
       0,     0,     0,     0,   374,   375,     0,   376,     0,   377,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   378,
     379,   366,     0,   380,   381,   382,   369,   383,   384,   370,
       0,   371,     0,   372,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     373,     0,     0,     0,     0,   385,     0,     0,    76,   386,
       0,     0,     0,     0,     0,   387,    78,    79,   388,   389,
     390,   391,     0,     0,     0,     0,     0,   374,   375,     0,
     376,     0,   377,    63,    64,    65,    66,    67,    68,    69,
      70,    71,   378,   379,   366,     0,   380,   381,   382,   369,
     383,   384,   370,     0,   371,     0,   372,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   373,     0,     0,     0,     0,   659,     0,
       0,    76,   386,     0,     0,     0,     0,     0,   387,    78,
      79,   388,   389,   390,   391,     0,     0,     0,     0,     0,
     374,   375,     0,   376,     0,   377,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   378,   379,   366,     0,   380,
     381,   382,   369,   383,   384,   370,     0,   371,     0,   372,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   373,     0,     0,     0,
       0,   665,     0,     0,    76,   386,     0,     0,     0,     0,
       0,   387,    78,    79,   388,   389,   390,   391,     0,     0,
       0,     0,     0,   374,   375,     0,   376,     0,   377,    63,
      64,    65,    66,    67,    68,    69,    70,    71,   378,   379,
     366,     0,   380,   381,   382,   369,   383,   384,   370,     0,
     371,     0,   372,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   373,
       0,     0,     0,     0,   674,     0,     0,    76,   386,     0,
       0,     0,     0,     0,   387,    78,    79,   388,   389,   390,
     391,     0,     0,     0,     0,     0,   374,   375,     0,   376,
       0,   377,    63,    64,    65,    66,    67,    68,    69,    70,
      71,   378,   379,   366,     0,   380,   381,   382,   369,   383,
     384,   370,     0,   371,     0,   372,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   373,     0,     0,     0,     0,   385,     0,     0,
      76,   386,     0,     0,     0,     0,     0,   387,   879,    79,
     388,   389,   390,   391,     0,     0,     0,     0,     0,   374,
     375,     0,   376,     0,   377,    63,    64,    65,    66,    67,
      68,    69,    70,    71,   378,   379,   366,     0,   380,   381,
     382,   369,   383,   384,   370,     0,   371,     0,   372,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   373,     0,     0,     0,     0,
     385,     0,     0,    76,   386,     0,     0,     0,     0,     0,
     387,   448,    79,   388,   389,   390,   391,     0,     0,     0,
       0,     0,   374,   375,     0,   376,     0,   377,  1921,    64,
      65,    66,    67,    68,    69,    70,    71,   378,   379,   366,
       0,   380,   381,   382,     0,   383,   384,     0,     0,     0,
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   385,     0,     0,    76,   386,     0,     0,
       0,     0,     0,   387,    78,    79,   388,   389,   390,   391,
     181,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,   181,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,     0,     0,    20,    58,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,   182,
      47,     0,   183,   184,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     668,     0,     0,   669,   670,    14,    15,    16,    17,    18,
       0,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,  -457,
    -457,     0,  -457,    46,     0,    47,     0,  -457,     0,     0,
       0,     0,     0,     0,     0,     0,    14,    15,    16,    17,
      18,     0,     0,    20,    58,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
    -458,  -458,     0,  -458,    46,     0,    47,     0,  -458,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    58
};

static const yytype_int16 yycheck[] =
{
       1,    74,   264,     4,   164,   229,    74,     4,     1,   180,
     175,    74,   164,   291,   228,   706,   228,    74,   231,   250,
     475,   228,   385,   165,   888,    96,   228,   625,  1141,  1695,
     616,   981,   216,   874,  1695,   152,    74,    59,   644,   140,
     228,   535,  1695,   612,   347,    76,   612,   684,   351,   612,
     691,   545,  1374,  1375,  1764,    56,    57,   797,    59,   780,
    1010,    74,   750,  1306,  1307,   786,    59,    74,   387,   777,
     238,   228,     1,    74,  1374,   815,   330,    72,  1227,    75,
     304,  1589,    83,  1827,   332,   298,   299,   779,   228,   303,
      91,   303,   586,   347,   262,    96,   303,   351,    99,   200,
     583,   303,   103,  1696,   272,   164,   103,   858,     1,     1,
       1,     4,   796,   797,     1,   303,    89,   190,    72,   777,
      77,    78,   190,  1073,   146,   779,   874,   190,   245,   777,
      59,   815,    98,     0,   885,   524,   525,    72,   777,    89,
     141,   881,    75,   144,   122,   146,   303,     4,   106,   107,
       1,   152,   190,   146,    83,   228,   151,   158,   229,    89,
     228,     0,   273,   303,   165,   228,    59,    96,    59,  1831,
      99,   228,    59,   284,   103,    97,   154,   190,     1,    89,
     176,    84,  1115,   190,   157,   151,   159,  1120,   189,   190,
     228,    72,   133,   779,    72,   449,   154,   881,   179,    56,
      57,   155,   159,   204,   117,   157,    99,   455,   777,   159,
     103,   777,    99,   214,   777,   228,   151,   146,   219,   618,
     155,   228,  1815,  1967,   176,   132,   167,   228,   229,   159,
     303,   201,  1740,   304,    91,   303,    59,   151,     1,  1264,
     303,     4,   155,   176,   245,   507,   303,   938,   151,   159,
       0,   282,   253,   146,   176,   146,   117,   646,  1191,   146,
     189,   175,   263,   385,    83,   266,   158,   174,   151,    72,
     151,   330,   273,   151,   155,   204,   967,   155,   502,   493,
     103,   493,   283,   284,   141,   286,   493,   144,   154,   895,
    1612,   493,  1133,  1125,   572,  1444,    59,   481,   601,   155,
     229,   158,   303,   304,   160,   493,   157,   158,   165,   175,
     311,   453,  1612,   796,   797,   324,  1037,   318,   319,   956,
      72,  1162,   323,   146,   253,   931,   912,  1125,  1036,   151,
     149,  1019,   815,  1999,     1,   638,   493,   266,  1999,  2049,
     103,   704,   661,  1936,   445,  1085,  1999,   601,   151,  1125,
     157,  1043,   155,   493,   363,   356,   604,   581,  2020,    20,
     361,   855,   219,   364,   365,   579,  1117,   579,   757,   176,
    1585,   625,   579,   266,     1,   304,   544,   579,  1036,   266,
     628,   151,   781,   146,   638,  1133,   785,   635,  1036,  1043,
     449,   579,    59,  2055,   151,   794,   795,  1036,   881,   151,
     291,  1085,    72,   155,   151,   522,   263,  2000,   148,  1342,
     133,   528,   237,   151,  1162,   240,   273,   155,    75,   159,
     493,  1743,   579,   583,   159,   493,   283,   284,   429,   286,
     493,   502,    59,    90,   253,   175,   261,  1680,  1681,   579,
      83,   176,   473,    72,   167,   332,   271,  2040,  1473,  1474,
    1475,   452,   453,    96,   311,   493,    99,  1043,   600,    72,
     103,   318,   319,   464,   465,   159,   323,  1036,   291,   952,
    1036,  1584,   473,  1036,   475,   157,  1589,    77,    78,   146,
     493,   151,   176,  1970,    58,   155,   493,    61,    62,   180,
      64,   310,   493,   157,   176,  1216,    72,   151,    83,   356,
     429,   502,    72,   155,   361,   159,   579,   364,   160,  1996,
     581,   579,   176,  1345,  1346,  1347,   579,   153,  1468,   146,
      75,   522,   151,   151,   583,   150,   155,   528,   164,   165,
    1745,  1746,   157,  1374,  1375,  2022,    91,   659,   151,   156,
     662,   663,   155,   665,    61,    62,   189,  1345,  1346,  1347,
     176,    72,   674,   108,   151,   677,   678,   679,  1013,   159,
     530,   204,   159,   585,   149,   566,   625,   568,   455,  1345,
    1346,  1347,    72,   502,  1295,   151,   157,   151,   579,   155,
     581,   151,   176,  1905,   585,   155,   229,    13,    14,    15,
      16,    17,   585,   153,   595,   452,   899,   157,   599,   600,
     419,    72,  1085,   188,    72,   157,    72,   464,   465,   133,
     253,  1933,  1210,   127,   128,   167,   131,  1223,   866,   790,
     804,   153,   201,   266,   291,    72,   158,  1740,  1843,  1844,
     151,   632,   602,  1274,   155,   157,   151,   779,   162,   163,
     155,   154,   159,   644,  1887,   899,    72,   162,   163,  1971,
       3,   151,   581,   901,   176,   155,   585,   171,   172,   176,
     175,  1060,   157,   361,   291,   157,   364,   157,   253,   145,
     146,   147,   167,  1364,   157,   167,   132,   157,   157,  1543,
     151,   572,   569,   151,   155,   151,   176,   155,   167,   155,
     515,   167,   585,   176,   585,   696,   176,   698,   585,   700,
     817,   732,   157,   704,   151,   157,   707,   133,   155,   566,
     153,   602,   537,  1663,   157,  1665,   157,   157,   543,   836,
     157,   176,   547,   151,   176,   310,   167,   167,  1841,     3,
     158,   732,    13,    14,    15,    16,    17,    72,   595,   176,
    1853,   628,   599,   600,  1385,   330,   157,   570,   152,   572,
     731,   330,   162,   131,   333,   159,  1239,   151,   929,   169,
     170,   153,   585,   733,   220,   153,   158,   151,   347,   153,
     158,  1612,   351,   151,  1229,   632,   777,   155,   779,   154,
     155,   923,   159,   158,   162,   163,   429,   644,   150,   131,
     791,    72,   952,   153,   159,   157,    58,   798,   158,    61,
      62,  1256,    64,   804,   774,   175,   807,  1920,   153,   151,
    1072,  1200,   157,   155,   173,   816,   817,   818,  1424,  1425,
     162,   163,   585,  1306,  1465,   159,   796,   797,    13,    14,
      15,    16,    17,   151,   419,   836,   292,   131,   151,   696,
     153,   698,   155,   700,   151,   815,   153,   704,   155,   151,
     707,    13,    14,    15,    16,    17,   837,   151,   151,   502,
     153,   155,   155,   151,   449,   153,  1452,   155,   162,   163,
     449,   872,   873,   874,   151,   732,   145,   146,   147,   106,
     107,   874,   153,   774,   131,   153,   157,    72,   157,   157,
     887,  1104,   117,   952,   895,   153,  1080,   153,   167,   157,
    1042,  1043,  1743,   153,   151,   572,   157,   176,   155,   153,
      72,   881,   157,   157,   159,   162,   163,   153,   585,   155,
      47,    48,   923,    50,   106,   107,   927,  1382,    55,   385,
     931,   145,   146,   147,   791,   153,   937,   153,   153,   524,
     525,   798,   157,  1426,   153,   572,   151,   153,   133,   151,
     155,   530,    22,   167,   153,   154,  1210,   157,   585,   816,
     151,   818,   176,   153,     3,   151,  1214,   157,   151,   131,
     164,   165,   973,   157,    13,    14,    15,    16,    17,   866,
     981,   874,   157,   874,   125,   126,  1349,   874,  1219,   151,
     877,   129,   130,   155,   887,   110,   111,   112,   113,   114,
     162,   163,   151,   151,   583,   157,   155,   155,  1463,  1010,
     156,   157,  1013,    98,   901,   872,   873,   874,   151,   103,
     156,   157,   601,   602,   108,   109,   110,   111,   112,   113,
     114,   115,   857,    72,   151,  1036,  1201,  1202,   895,  1645,
     625,  1042,  1043,   153,   153,   151,   625,   157,   157,   155,
     153,   874,   153,   153,    62,   159,   157,   157,   514,   638,
      89,   646,   159,   519,  1905,   153,   923,   162,   163,   157,
     927,   155,  1073,   150,   931,  1337,   159,  1194,  1238,  1239,
     536,     4,     5,     6,     7,     8,     9,    10,    11,    12,
     546,    99,  1933,   153,   153,   156,  1374,   157,   157,   790,
     561,   153,   110,   153,   112,   157,   114,   157,   153,   153,
     801,   874,   157,   157,   117,  1085,   156,   157,   153,   153,
      18,  1243,   157,   157,   887,   153,   156,   157,   153,   157,
    1971,  1355,   157,   159,  1135,   156,   157,  1138,  1139,  1140,
    1133,   159,   153,  1314,  1315,   153,   157,   155,   156,  2004,
     159,  1210,   175,  2008,   733,  1307,    78,   718,   719,   720,
     721,  1162,   153,    61,    62,    63,    64,  1168,   151,  1162,
     156,   157,   757,   151,  1780,  1176,   151,  1868,  1179,  1180,
    1239,   103,  1183,  1180,   106,   107,   108,   109,   110,   111,
     112,   113,   114,  1194,   151,   774,   204,  1680,    74,   711,
     712,   713,   157,   659,   168,   103,   173,   874,   161,   665,
     108,   109,   110,   111,   112,   113,   114,   115,   674,   163,
      96,     3,  1223,  1534,  1535,  1536,   157,   158,  1229,   131,
     153,    13,    14,    15,    16,    17,   154,   693,   929,  1240,
    1133,   153,  1133,  1122,  1123,  1124,  1133,   874,   156,   157,
    1179,  1180,   153,   176,   176,  1256,   153,   155,   266,   153,
     268,   269,   153,  1264,   156,   157,  1426,   156,   157,  1162,
     153,  1162,   156,   157,   155,  1162,   152,   156,  1135,   156,
     157,  1138,  1139,  1140,  1355,   133,  1179,  1180,   296,   133,
      72,   158,  1179,   158,   302,  1296,    13,    14,    15,    16,
      17,   156,   157,   888,   641,  1162,  1125,   156,   157,   151,
    1133,  1168,   157,     1,   153,  1006,     4,  1541,   151,  1176,
     899,   156,   157,   902,   332,  1549,  1183,  1214,   156,   157,
     338,   153,   340,  1547,  1612,  1547,  1306,  1307,   156,  1162,
    1547,   156,   157,   156,   157,  1547,   156,   157,   156,   157,
     153,  1348,   228,   229,  1355,    72,   153,  1180,  1359,  1547,
     153,  1362,   145,   146,   147,   153,  1223,  1426,   376,   245,
    1133,    59,   159,   952,   157,    13,    14,    15,    16,    17,
      18,  1382,   843,  1240,   167,   159,   842,   157,   158,   155,
    1547,  1082,   159,   176,   159,    83,    70,   853,   156,  1162,
     856,  1402,   151,  1404,   860,   156,   157,  1547,    13,    14,
      15,    16,    17,    18,   131,   156,   157,  1180,    77,    78,
      78,   429,   156,  1424,  1425,   157,   158,   303,   304,  1265,
    1266,   714,   715,    18,   151,   175,   716,   717,   155,   157,
     722,   723,   175,   904,   159,   162,   163,   455,   151,   457,
     458,   176,   140,  1450,   153,  1348,  1554,  1555,   146,  1201,
    1202,   149,  1463,  1623,   472,   802,  1133,  1468,   153,   159,
    1541,  1623,  1473,  1474,  1475,  1404,   176,   159,  1549,   156,
     156,  1374,  1375,  1374,  1375,    18,   150,  1374,  1375,  1713,
    1547,   153,   153,   153,   502,  1162,  1710,   153,  1710,   153,
     153,   153,  1359,  1710,   153,  1362,  1133,   150,  1710,   159,
      70,  1404,   200,   159,   522,   159,   153,  1404,   175,   527,
     153,   529,  1710,   153,   153,   176,  1345,  1346,  1347,  1681,
    1349,  1350,   150,  1655,   159,  1162,  1179,  1180,   153,   153,
    1541,   549,   153,   551,   552,  1402,  1547,   157,  1549,   157,
     153,  1374,  1375,  1710,  1015,  1556,   157,  1450,   153,   153,
     153,   569,   153,   251,  1623,   253,   153,  1424,  1425,  1570,
    1710,  1032,  1033,   581,   153,   156,   156,   453,   153,   153,
     153,  1578,  1583,   153,   153,  1348,  1277,   153,   153,  1559,
     153,   153,   156,   150,   153,   932,   604,   175,   606,   607,
    1824,   153,   150,   291,   153,   151,  1860,   157,   151,   151,
     151,  1374,  1375,   151,   151,  1200,    14,   493,   158,  1620,
     628,   629,   310,  1314,  1315,  1210,   502,   635,    74,  1626,
    1611,  1210,    13,    14,    15,    16,    17,   157,    91,   158,
     176,   176,  1713,   156,  1645,   156,   522,   176,   176,   159,
     159,   150,   528,  1770,   150,   159,  1827,   157,   176,   153,
    1239,   156,  1663,  1119,  1665,   153,   157,     4,     5,     6,
       7,     8,     9,    10,    11,    12,  1132,   157,   153,   157,
    1934,   153,   156,   153,  1577,  1578,   150,  1450,   150,  1937,
    1577,    72,   568,  1149,   151,   176,   151,   176,    80,  1556,
     176,   151,   150,   579,   151,   581,   150,  1374,  1375,  1710,
    1680,  1681,  1713,  1570,   153,   176,   176,   176,   176,  1612,
     176,  1612,   176,  1724,   150,  1612,  1583,  1728,    65,   157,
     157,   419,   150,  1626,   156,  1887,   156,   156,   153,   156,
     150,  1742,   153,   159,   158,   158,   120,  1374,  1375,   150,
     131,  1752,   156,  1824,   153,  1578,   153,   445,   153,   153,
     156,  1404,   153,  1620,   150,   158,   153,   176,  1769,  1770,
     151,   151,   157,   157,   155,   153,  2000,   109,  1759,  1780,
     151,   162,   163,   151,   156,  1999,   156,  1999,  1645,  1612,
     156,   150,  1999,   150,  1965,   159,  1967,  1999,    13,    14,
      15,    16,    17,    18,     1,   153,   150,     4,   153,   156,
     153,  1999,   153,   153,   153,  1578,  2040,   153,    75,  1936,
     176,    75,   176,  1824,   150,   176,  1287,  1288,   151,   153,
    1831,   153,    90,   150,  1835,  2006,   150,  1730,   156,  1840,
    1301,  1302,  1999,  1730,   156,   150,   150,  1426,   155,  1612,
    1743,   153,  1743,   153,   153,   153,  1743,   153,   866,  1999,
     153,    75,    59,  1626,  1865,   154,  1203,  1724,   176,   877,
     167,  1728,   167,    75,  1335,  1336,   158,    74,   566,   153,
     150,   153,   176,  1864,   572,  1742,    83,    13,    14,    15,
      16,    17,    18,   901,  1231,  1752,   176,   585,   843,    96,
     153,   777,    99,   779,   912,   153,   103,  1908,   157,  1365,
    1366,  1912,  1769,   921,   150,  2086,   167,  1887,   152,   150,
    1743,   167,   103,  1780,  1925,   158,  1999,   151,   157,  2000,
      75,  1999,   151,   150,   152,  1936,  1999,  1938,   176,   167,
     167,   817,  1999,   140,  1281,  1612,   156,  1403,  1949,   146,
    1951,  1952,   149,   176,   109,   152,   153,   109,  1543,   904,
     836,   152,   158,   153,   153,   150,   150,   164,   176,  2040,
     153,   151,    75,  1974,  1831,   153,   153,   176,  1835,  1655,
    1743,   176,  1364,  1840,  1271,  1612,   120,   683,   122,   123,
     124,   188,   189,   190,   724,   726,   725,  1151,  1999,  2000,
     727,  1162,   728,   200,   201,  2055,   418,   204,  1865,  2010,
    1612,  1019,  1905,  1996,  1905,  1751,  1967,   151,  1905,  2020,
     154,   155,  1743,  2004,  2050,   159,   160,  2008,  2009,  1869,
    2049,   228,   229,  2037,  1934,  1043,  1604,  1604,  2009,  2040,
    1933,  1860,  1933,  2071,  1933,    49,  1933,   258,   245,  1183,
    1541,  1908,  2053,  1824,  2055,  1912,   253,  1895,  1350,  1555,
    1176,  2042,  1523,  1524,   891,   804,     0,  1450,  1925,   266,
    1015,  2000,   595,  2074,    83,   482,  1743,  1626,  1971,  2080,
    1971,  1938,  1905,   937,  1971,    -1,  2067,  1032,  1033,  2090,
    2071,    -1,  1949,  2094,  1951,  1952,    -1,    -1,  1559,   296,
      -1,   749,   749,  2104,  1441,   302,   303,   304,   749,  1446,
    1933,  2040,  2093,   310,   103,  1934,  1743,  1974,   373,   108,
     109,   110,   111,   112,   113,   114,    -1,  1464,    -1,    -1,
      -1,    -1,    -1,   330,   331,   332,  1827,    -1,    -1,    -1,
     149,    -1,  1905,   398,   399,    -1,    -1,  1603,  1971,    -1,
     347,    -1,    -1,  2010,   351,   164,    -1,    -1,    -1,    -1,
    1036,    -1,    -1,  2020,   419,    -1,  1042,  1043,    -1,    -1,
    1933,  1179,    -1,    -1,    -1,    -1,  1637,    -1,    -1,   188,
      -1,    -1,    -1,    -1,    -1,    -1,   874,    -1,   385,    -1,
      -1,    -1,   201,    -1,   449,    -1,  2053,    -1,  2055,   887,
      -1,    -1,    -1,    -1,    -1,    -1,  1214,    -1,  1971,  1670,
      -1,    -1,  1220,    -1,  1675,  1676,    -1,  2074,    -1,    -1,
      -1,    -1,   419,  2080,    -1,   422,    -1,    -1,    -1,    -1,
      -1,    62,   429,  2090,    -1,    -1,    -1,  2094,  1905,    -1,
     239,    -1,    -1,    -1,   253,    -1,    -1,  2104,   445,    -1,
      -1,    -1,   449,    -1,    -1,    -1,   453,  1594,   455,    -1,
      -1,    -1,    -1,    -1,    -1,    10,  1933,    -1,    -1,    -1,
      -1,    -1,    -1,   104,  1965,    -1,  1967,   108,  1905,    -1,
     111,  1860,   113,    -1,    -1,    -1,    -1,    -1,    -1,    62,
      -1,    -1,    -1,  1630,  1631,    -1,   493,    -1,    -1,    -1,
      -1,   310,   103,    -1,  1971,   502,  1933,   108,   109,   110,
     111,   112,   113,   114,    -1,  2006,    -1,    -1,  1194,    -1,
    1657,   330,    -1,  1135,    -1,   522,    99,   524,   525,    -1,
      -1,   528,    -1,   530,    -1,    -1,    -1,   110,    -1,    -1,
      -1,    -1,  1287,  1288,  1971,    -1,    -1,  2038,  1356,    -1,
      -1,   152,    -1,    -1,   155,  1934,  1301,  1302,   103,    -1,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
      -1,   568,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     153,    -1,   579,    -1,   581,    -1,   583,    -1,   585,    -1,
    1335,  1336,    -1,    -1,    -1,  2086,  1404,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   601,   602,    -1,   604,    -1,    -1,
     419,    -1,    -1,    -1,    -1,   612,    -1,    -1,    -1,   616,
    1296,    -1,    -1,   422,    -1,   256,    -1,    -1,   625,    -1,
      -1,   204,    -1,    -1,    -1,    -1,    -1,  1125,   635,   438,
     449,   638,   441,    -1,  1781,  1133,    -1,    -1,    -1,   646,
      -1,    -1,    -1,    -1,    -1,   710,    -1,    -1,    -1,    -1,
      -1,    -1,   659,    -1,    -1,   662,   663,    -1,   665,    -1,
      -1,    -1,    -1,    -1,  1162,    -1,    -1,   674,   103,  1355,
     677,   678,   679,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,    -1,   266,   119,    -1,   121,    -1,    -1,   498,
      -1,    -1,    -1,   334,   335,    -1,   337,    -1,   339,    -1,
      -1,    -1,    -1,    -1,    -1,   524,   525,    -1,    -1,    -1,
      -1,   530,    -1,   296,    -1,    -1,    -1,   152,    -1,   302,
     155,    -1,    -1,    -1,    -1,    -1,   733,    -1,    -1,    -1,
      -1,  1878,    -1,    -1,    -1,   376,    -1,    10,    -1,    -1,
      -1,    -1,   749,   750,    -1,    -1,  2017,   103,    -1,   332,
     757,    -1,   108,   109,   110,   111,   112,   113,   114,  1577,
      -1,    -1,    -1,    -1,   583,    -1,    -1,   774,  1523,  1524,
     777,    -1,   779,    -1,    -1,    -1,    65,    66,    67,    68,
    1402,    -1,    -1,   602,    -1,    -1,    -1,    -1,    -1,   796,
     797,    -1,    -1,   376,    -1,   151,   152,    -1,    -1,    -1,
    2066,    -1,    -1,    -1,    -1,    -1,   625,    -1,   815,    -1,
     817,    -1,    -1,    -1,   103,    -1,  2082,   106,   107,   108,
     109,   110,   111,   112,   113,   114,    -1,   646,    -1,   836,
     103,   472,    13,   106,   107,   108,   109,   110,   111,   112,
     113,   114,    -1,    -1,    -1,    -1,    -1,  1345,  1346,  1347,
    1348,  1349,  1350,    -1,    -1,  1541,    -1,    -1,    -1,    -1,
      -1,  1547,    -1,  1549,    -1,    -1,   155,   874,    -1,    -1,
      -1,    -1,   455,    -1,   881,    -1,  1374,  1375,    -1,    -1,
     887,   888,  1637,   948,   173,    -1,    -1,    -1,   953,   472,
      -1,    -1,   899,    -1,   901,  1713,    -1,    -1,    -1,   964,
     541,    -1,    -1,    -1,    -1,   912,    -1,    88,    -1,    -1,
      -1,    -1,  1730,    -1,   733,  1670,    -1,    -1,    -1,    -1,
    1675,  1676,   103,    -1,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,    -1,    -1,    -1,    -1,   757,    -1,
     749,   750,    -1,    -1,    -1,   952,   529,    -1,  1570,    -1,
     759,    -1,  1450,   762,    -1,   774,    -1,    -1,    -1,    -1,
      -1,  1583,    -1,    -1,    -1,    -1,   549,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,    -1,   214,   796,   797,    -1,
      -1,    -1,    -1,   103,    -1,    -1,   569,     3,   108,   109,
     110,   111,   112,   113,   114,   115,   815,  1815,  1620,   119,
     103,   121,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,  1019,    -1,   823,    -1,    -1,    -1,   827,    -1,
      -1,   604,   831,    -1,  1710,    -1,    -1,  1713,   131,  1036,
      -1,    18,   152,    -1,    -1,   155,  1043,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   628,    -1,    -1,   151,   152,
      -1,    -1,   635,    -1,    -1,    -1,    -1,    -1,    -1,   162,
     163,    -1,   881,    -1,    -1,    -1,    -1,    -1,  1133,   888,
      57,    58,    59,    60,    61,    62,    63,    64,  1085,    -1,
      -1,    -1,    -1,    -1,  1770,    -1,    -1,   103,    -1,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,    -1,
      -1,    -1,  1724,    -1,    -1,    -1,  1728,    -1,    -1,     1,
      -1,    -1,    -1,    -1,  1612,   131,    -1,    -1,  1125,  1937,
    1742,    -1,    72,    -1,    -1,    -1,  1133,    -1,  1626,    -1,
    1752,    -1,    -1,   952,    -1,   151,   152,    -1,  1824,    -1,
      -1,    -1,   158,    -1,    -1,  1210,   162,   163,    -1,    -1,
      -1,    -1,    -1,   103,    -1,  1162,   106,   107,   108,   109,
     110,   111,   112,   113,   114,    -1,    -1,    59,    -1,    -1,
      -1,    -1,  1179,  1180,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1246,  1247,  1248,    -1,    -1,    -1,  1194,  1253,  1254,
      -1,    -1,    -1,  1200,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   151,   152,  1210,    -1,    -1,    -1,    -1,    -1,  1831,
    1019,   103,    -1,  1835,    -1,    -1,    -1,    -1,  1840,    -1,
     103,    -1,  2040,   106,   107,   108,   109,   110,   111,   112,
     113,   114,  1239,    -1,    -1,   473,  1243,   475,    -1,    -1,
      -1,    -1,    -1,  1865,    -1,  1743,    -1,    -1,    -1,    -1,
    1936,    -1,    -1,    -1,   146,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  2017,    -1,    -1,    -1,  1085,    -1,    -1,    -1,
      -1,    -1,   164,    -1,    13,    14,    15,    16,    17,    -1,
      -1,  1090,    -1,   866,  1093,    -1,  1908,    -1,    -1,    -1,
    1912,    -1,    -1,   176,   877,    -1,    -1,    -1,    -1,  1306,
    1307,    -1,    -1,  1925,    -1,    -1,    -1,    -1,     1,   201,
      -1,     4,    -1,  1999,  2000,    -1,    -1,    -1,   901,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1949,    -1,  1951,
    1952,    -1,    -1,    72,    -1,    -1,    -1,    -1,  1345,  1346,
    1347,  1348,  1349,  1350,    -1,    -1,    -1,    -1,  1355,  1356,
      -1,    -1,  1974,    -1,  2040,     4,     5,     6,     7,     8,
       9,    10,    11,    12,   103,    -1,    59,  1374,  1375,   108,
     109,   110,   111,   112,   113,   114,    -1,    -1,    -1,    -1,
      -1,  1200,    -1,   275,    -1,    -1,    -1,    -1,  2010,    -1,
      83,  1210,   131,    -1,    -1,    -1,    -1,  1404,  2020,   291,
      -1,    -1,    -1,    -1,    -1,    -1,    99,  1905,    -1,    -1,
     103,    -1,   151,   152,    -1,    -1,    -1,    -1,    -1,  1426,
    1239,    -1,    -1,   162,   163,    13,    14,    15,    16,    17,
      -1,  2053,    -1,  2055,    -1,  1933,    -1,    -1,   330,    -1,
      -1,   333,    -1,  1450,    -1,  1452,    -1,   140,    -1,    -1,
      -1,    -1,  2074,   146,    -1,   347,   149,    -1,  2080,   351,
     153,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2090,    -1,
     163,   164,   165,  1971,  1283,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1292,    72,    -1,    -1,  1306,  1307,    -1,
      -1,    -1,    -1,    -1,   103,   188,    -1,   106,   107,   108,
     109,   110,   111,   112,   113,   114,    -1,   200,   201,    -1,
      -1,   204,    -1,    -1,    -1,   103,    -1,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,    -1,    -1,    -1,
    1537,    -1,    -1,    -1,  1541,    -1,  1543,    -1,    -1,    -1,
    1547,    -1,  1549,   131,    -1,    72,    -1,    -1,    -1,    -1,
      -1,    -1,  1617,   162,    -1,    -1,    -1,   449,   251,    -1,
     253,    -1,    -1,   151,   152,    -1,   804,   155,    -1,   807,
    1577,  1578,    -1,   266,   162,   163,   103,    -1,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,    -1,    -1,
     283,    -1,    -1,    -1,    -1,    -1,  1179,    -1,   291,    -1,
      -1,    -1,    -1,   296,   131,  1612,    -1,  1426,    -1,   302,
      -1,    -1,    -1,    -1,    -1,    -1,  1623,   310,    -1,  1626,
      -1,    -1,    -1,    -1,   151,   152,    -1,    -1,   155,    -1,
      -1,  1214,    -1,    -1,    -1,   162,   163,   330,   530,   332,
     333,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1655,    -1,
      -1,    -1,    -1,    -1,   347,    -1,   103,    -1,   351,   106,
     107,   108,   109,   110,   111,   112,   113,   114,    -1,   561,
      -1,    -1,    -1,  1680,  1681,    -1,    -1,    -1,   570,    -1,
     572,    -1,    -1,    -1,     1,    -1,    -1,     4,  1695,  1696,
      -1,   583,   385,   585,    -1,    13,    14,    15,    16,    17,
      -1,    -1,    -1,  1710,    -1,    -1,  1713,    -1,    -1,   601,
     602,   103,    -1,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,   114,  1730,  1543,    -1,   419,    -1,  1537,   176,
      -1,    -1,    -1,   625,    -1,   973,  1743,    -1,    -1,   131,
      -1,    -1,    59,   981,    -1,    -1,   638,    -1,    -1,    -1,
      -1,    -1,   445,    -1,    72,    -1,   449,    -1,    -1,   151,
     152,    -1,   455,  1770,    -1,    -1,    83,    -1,    -1,    -1,
     162,   163,  1010,  1356,    -1,  1013,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   103,   103,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,    -1,    -1,    -1,
      -1,    -1,  1867,    -1,  1623,    -1,    -1,    -1,  1815,    -1,
      -1,    -1,    -1,   131,    -1,    -1,    -1,  1824,    -1,    -1,
      -1,  1404,    -1,   140,    -1,    -1,    -1,    -1,    -1,   146,
      -1,   524,   525,   151,   152,  1073,   529,   530,    -1,    -1,
      -1,   733,    -1,    -1,   162,   163,    -1,   164,    -1,    -1,
      -1,    -1,    -1,  1860,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1680,  1681,    -1,    -1,    -1,    -1,    -1,   561,    -1,
      -1,    -1,   189,   566,    -1,    -1,   569,   570,    -1,   572,
    1887,    -1,   774,   200,   201,    -1,  1695,  1696,  1895,    -1,
     583,    -1,   585,    -1,    -1,    -1,    -1,    -1,  1905,    -1,
      -1,    -1,    -1,    -1,   796,   797,   599,    -1,   601,   602,
     103,   604,   229,   106,   107,   108,   109,   110,   111,   112,
     113,   114,    -1,   815,    -1,    -1,  1933,  1934,   245,  1936,
    1937,    -1,   625,   250,   251,   628,   253,    -1,    -1,   632,
      -1,    -1,   635,    -1,    -1,   638,    -1,   640,    -1,    -1,
      -1,   843,    -1,   646,    -1,    -1,    -1,    -1,   275,   152,
      -1,   278,   155,   280,  1971,    -1,   659,    -1,    -1,   662,
     663,    -1,   665,    -1,   291,    -1,    13,    14,    15,    16,
      17,   674,   874,    -1,   677,   678,   679,   304,    -1,   881,
      -1,  1229,  1999,  2000,  1577,   134,   135,   136,   137,   138,
     139,   140,   141,   142,   143,   144,  1815,   899,    72,   148,
     902,    -1,   904,   330,    -1,    -1,   333,    -1,  1256,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1264,    -1,    -1,    -1,
     347,    -1,    -1,  2040,   351,    72,   175,    -1,    -1,   103,
     733,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     952,    -1,    -1,    -1,   757,    -1,   103,   131,  1887,   106,
     107,   108,   109,   110,   111,   112,   113,   114,    -1,    -1,
      -1,   774,  1891,    -1,    -1,    -1,  1895,   151,   152,    -1,
      -1,    -1,    -1,    -1,   131,    -1,    -1,    -1,   162,   163,
      -1,    -1,   103,   796,   797,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   151,   152,    -1,    -1,   155,    -1,
      -1,    -1,   815,  1015,    72,   162,   163,  1936,   445,    -1,
      -1,    -1,   449,    -1,    -1,    -1,    -1,    -1,    72,    -1,
    1032,  1033,    -1,    -1,  1382,    -1,    -1,  1730,    -1,    -1,
     843,    -1,    -1,    -1,   155,   103,    -1,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,    -1,    -1,   103,
      -1,    -1,    -1,   866,   108,   109,   110,   111,   112,   113,
     114,   874,    -1,   131,   877,    -1,    -1,    -1,   881,    -1,
    1999,  2000,    -1,  1085,   887,   888,    -1,   131,    -1,    -1,
      -1,    -1,    -1,   151,   152,    -1,   899,    -1,   901,   902,
      -1,   904,    -1,   530,   162,   163,    -1,   151,   152,    -1,
      -1,    -1,    -1,    -1,    -1,  1463,    -1,    -1,   162,   163,
    1468,  2040,    -1,    -1,    -1,  1473,  1474,  1475,    -1,    -1,
      -1,  1133,    -1,    -1,   561,    -1,    -1,    -1,    -1,   566,
      -1,    -1,    -1,   570,    -1,   572,    -1,    -1,    -1,   952,
      -1,    -1,    -1,    -1,    -1,    -1,   583,    -1,   585,    -1,
    1162,    -1,   103,     1,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   601,   602,   103,    -1,  1180,   106,
     107,   108,   109,   110,   111,   112,   113,   114,    -1,   616,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   625,    -1,
      -1,    -1,    -1,   630,    -1,    -1,    -1,    -1,  1210,    -1,
      -1,   638,  1015,   154,    -1,    -1,    -1,    -1,   159,    -1,
      -1,    59,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1032,
    1033,    -1,   159,    -1,    -1,    -1,  1238,  1239,    -1,    -1,
      -1,    -1,   103,    -1,  1937,   106,   107,   108,   109,   110,
     111,   112,   113,   114,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,   103,    -1,
     131,   106,   107,   108,   109,   110,   111,   112,   113,   114,
      -1,    -1,  1085,    -1,    -1,  1287,  1288,    -1,    -1,    -1,
     151,   152,    -1,    -1,   155,    -1,    -1,    -1,    -1,  1301,
    1302,   162,   163,    -1,  1306,  1307,   733,    -1,   146,    -1,
      -1,    -1,    -1,    -1,   175,  1663,    -1,  1665,    -1,    -1,
      -1,    -1,  1125,   750,   159,    -1,   164,    -1,    -1,    -1,
    1133,    -1,   103,  1335,  1336,   106,   107,   108,   109,   110,
     111,   112,   113,   114,    -1,    -1,    -1,   774,    -1,    -1,
      -1,    -1,   779,    -1,    -1,    -1,    -1,    -1,    -1,  1162,
     131,    -1,    -1,   201,    -1,    -1,    -1,    -1,    -1,   796,
     797,    -1,  1374,  1375,    -1,    -1,  1179,  1180,    -1,    -1,
     151,   152,    -1,    -1,    -1,    -1,    -1,   158,   815,    -1,
      -1,   162,   163,    -1,    -1,   103,    -1,  1200,   106,   107,
     108,   109,   110,   111,   112,   113,   114,  1210,    -1,    -1,
      -1,  1214,    -1,    -1,    -1,    -1,   843,    -1,    -1,    -1,
      -1,   103,    -1,  1226,  1426,    -1,   108,   109,   110,   111,
     112,   113,   114,   115,    -1,  1238,  1239,   119,    -1,   121,
    1243,    -1,    -1,    -1,    -1,    -1,    -1,   874,    -1,    -1,
      -1,   159,    -1,   291,   881,    -1,    -1,    -1,    -1,    -1,
     887,    -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,
     152,    -1,   899,   155,    -1,   902,    -1,   904,    -1,    -1,
      -1,    -1,   909,    -1,  1287,  1288,    -1,    -1,    -1,    -1,
      -1,    -1,   330,    -1,    -1,   333,    -1,    -1,  1301,  1302,
      -1,    -1,    -1,  1306,  1307,    -1,    -1,    -1,    -1,   347,
      -1,    -1,    -1,   351,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1523,  1524,    -1,    72,   952,    -1,    -1,    -1,    -1,
      -1,    -1,  1335,  1336,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1345,  1346,  1347,  1348,  1349,  1350,    -1,    -1,
      -1,    -1,    -1,  1356,    -1,   103,    -1,  1559,   106,   107,
     108,   109,   110,   111,   112,   113,   114,    -1,    -1,    -1,
      -1,  1374,  1375,    -1,    -1,    -1,  1578,    -1,    -1,    -1,
      -1,    -1,    -1,   131,    -1,    -1,    -1,    -1,  1015,   103,
      -1,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,  1404,    -1,   151,   152,  1032,  1033,    -1,    -1,    -1,
    1612,   449,    -1,    -1,   162,   163,    -1,   131,    -1,    -1,
      -1,  1623,   103,  1426,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,    -1,  1637,    -1,   151,   152,    -1,
      -1,   155,    -1,    -1,    -1,    -1,    -1,  1450,   162,   163,
     131,    -1,    -1,    -1,    -1,   103,    -1,    -1,  1085,    -1,
     108,   109,   110,   111,   112,   113,   114,    -1,  1670,    -1,
     151,   152,    -1,  1675,  1676,    -1,    -1,    -1,  1680,  1681,
      -1,   162,   163,   131,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   530,    -1,    -1,    -1,    -1,    -1,  1125,    -1,
      -1,    -1,    -1,   151,   152,    -1,  1133,   155,    -1,    -1,
      -1,    -1,    -1,    -1,   162,   163,    -1,    -1,    -1,    -1,
    1523,  1524,    -1,   561,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   570,    -1,   572,  1162,    -1,    -1,    -1,    -1,
    1543,  1743,    -1,    -1,    -1,   583,    -1,   585,    13,    14,
      15,    16,    17,  1180,    -1,    -1,  1559,    -1,    -1,    -1,
      -1,    -1,    -1,   601,   602,    -1,    -1,    -1,     1,    -1,
      -1,     4,    -1,    -1,  1577,  1578,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1210,    -1,    -1,    -1,   625,    -1,    -1,
      -1,    -1,  1219,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     638,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,  1612,
      -1,  1238,  1239,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1623,    -1,    -1,  1626,    -1,    -1,    59,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1637,    -1,    -1,    -1,   103,    -1,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
      83,    -1,  1655,    -1,    -1,    -1,    -1,    -1,  1860,    -1,
    1287,  1288,    -1,    -1,    -1,    -1,   131,  1670,    -1,    -1,
     103,    -1,  1675,  1676,  1301,  1302,    -1,  1680,  1681,  1306,
    1307,    -1,    -1,    -1,    -1,  1887,   151,   152,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   733,    -1,   162,   163,    -1,
      -1,    -1,    -1,  1905,    -1,    -1,    -1,   140,  1335,  1336,
      -1,    -1,    -1,   146,    -1,    -1,   149,    -1,  1345,  1346,
    1347,  1348,    -1,    -1,    -1,    -1,    -1,  1730,    -1,    -1,
      -1,  1933,  1934,    -1,    -1,    -1,   774,    -1,    -1,    -1,
    1743,    -1,    -1,    -1,    -1,    -1,    -1,  1374,  1375,    -1,
      -1,    -1,    -1,    -1,    -1,   188,    -1,    -1,   796,   797,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   200,    -1,  1971,
      -1,    -1,    -1,    -1,   101,    -1,   103,   815,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1426,
      -1,    -1,    -1,   103,    -1,   843,   106,   107,   108,   109,
     110,   111,   112,   113,   114,  2017,    -1,    -1,   251,    -1,
     253,    -1,    -1,  1450,   151,   258,    -1,   154,   155,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   874,    -1,    -1,    -1,
      -1,   103,    -1,   881,   106,   107,   108,   109,   110,   111,
     112,   113,   114,    -1,   154,    -1,    -1,  1860,   291,    -1,
      -1,   899,   103,    -1,   902,    -1,   904,   108,   109,   110,
     111,   112,   113,   114,   115,    -1,    -1,   310,   119,    -1,
     121,    -1,    -1,    -1,  1887,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   154,    -1,    -1,    -1,  1523,  1524,    -1,    -1,
      -1,    -1,  1905,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   152,    -1,    -1,   952,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1549,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1933,  1934,  1559,     1,  1937,   103,     4,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,    -1,    -1,    -1,
     103,  1578,   385,   106,   107,   108,   109,   110,   111,   112,
     113,   114,    -1,    -1,    -1,    -1,    -1,    -1,  1971,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1015,    -1,    -1,
      -1,    -1,    -1,   151,    -1,  1612,   419,    -1,    -1,    -1,
      -1,    59,    -1,    -1,  1032,  1033,  1623,   103,   151,  1626,
     106,   107,   108,   109,   110,   111,   112,   113,   114,    -1,
    1637,    -1,   445,    -1,  2017,    83,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1670,    -1,    -1,    -1,  1085,  1675,  1676,
      -1,    -1,    -1,  1680,  1681,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1696,
      -1,    -1,   140,    -1,    -1,    -1,    -1,    -1,   146,    -1,
      -1,   149,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   524,   525,    -1,    -1,  1133,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1743,    -1,    -1,    -1,
     188,    -1,    -1,    -1,  1162,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   200,   566,    -1,    -1,    -1,   570,    -1,   572,
      -1,    -1,  1180,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   585,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1210,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   251,    -1,   253,    -1,    -1,    -1,    -1,
     258,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1238,  1239,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   646,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   291,    -1,    -1,   659,    -1,    -1,   662,
     663,    -1,   665,  1860,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   674,   310,    -1,   677,   678,   679,    -1,    -1,  1287,
    1288,    -1,    87,    -1,    -1,    -1,    -1,    -1,    93,    94,
    1887,    -1,    -1,  1301,  1302,    -1,    -1,    -1,  1306,  1307,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1905,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   127,    -1,    -1,    -1,    -1,  1335,  1336,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1933,  1934,    -1,    -1,
      -1,    -1,    -1,     1,     1,    -1,     4,   385,    -1,    -1,
      -1,    -1,    -1,    -1,   757,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1374,  1375,    -1,    -1,
      -1,    -1,    -1,    -1,  1971,    -1,    -1,    -1,    -1,    -1,
      -1,   419,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    49,    -1,    -1,    52,    -1,    54,    -1,    56,
      -1,    59,    -1,  2000,    -1,    -1,    -1,   445,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,  1426,    -1,
    2017,    -1,    -1,    -1,    -1,    83,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   100,   101,   103,   103,    -1,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,    -1,   119,   120,   121,    -1,   123,   124,    -1,    -1,
      -1,   874,    -1,    -1,   131,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   140,    -1,   887,   888,   524,   525,   146,    -1,
      -1,   149,    -1,    -1,   151,   300,    -1,   154,   155,    -1,
      -1,    -1,    -1,    -1,   161,   162,   163,   164,   165,   166,
     167,    -1,    -1,    -1,    -1,  1523,  1524,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   566,    -1,
     188,    -1,   570,    -1,   572,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   200,    -1,    -1,    -1,    -1,   585,    -1,    -1,
      -1,  1559,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1578,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   251,    -1,   253,    -1,    -1,    -1,    -1,
     258,    -1,    -1,    -1,  1612,    -1,    -1,    -1,   646,    -1,
      -1,    -1,    -1,    -1,    -1,  1623,    -1,    -1,    -1,    -1,
      -1,   659,    -1,    -1,   662,   663,    -1,   665,    -1,  1637,
      -1,    -1,    -1,   291,    -1,    -1,   674,    -1,    -1,   677,
     678,   679,    -1,   448,    -1,   450,    -1,    -1,    -1,    -1,
      -1,    -1,   310,    -1,   459,   460,    -1,    -1,    -1,    -1,
      -1,    -1,  1670,    -1,    -1,    -1,    -1,  1675,  1676,    -1,
      -1,    -1,  1680,  1681,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     5,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    13,    14,    15,    16,
      17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   757,
      -1,    -1,  1125,    -1,    -1,    -1,    -1,   385,    -1,    -1,
    1133,    -1,    -1,    -1,    -1,  1743,    -1,    -1,    -1,    -1,
      -1,    -1,    49,    -1,    -1,    52,    -1,    54,    -1,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1162,
      -1,   419,   567,    -1,    -1,    72,    73,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1180,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   445,    -1,    -1,
      -1,    -1,    -1,   100,   101,    -1,   103,  1200,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,    -1,   119,   120,   121,    -1,   123,   124,    -1,    -1,
      -1,    -1,    -1,    -1,   131,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   874,    -1,    -1,    -1,
    1243,    -1,    -1,    -1,   151,    -1,    -1,   154,   155,   887,
     888,    -1,  1860,    -1,   161,   162,   163,   164,   165,   166,
     167,    -1,    -1,    -1,    -1,    -1,   524,   525,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1887,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1905,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   566,    -1,
      -1,    -1,   570,    -1,   572,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1933,  1934,   585,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1345,  1346,  1347,  1348,  1349,  1350,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1971,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1374,  1375,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   790,    -1,    -1,   646,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   659,    -1,    -1,   662,   663,    -1,   665,    -1,  2017,
      -1,    -1,    -1,    -1,    -1,    -1,   674,    -1,    -1,   677,
     678,   679,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1450,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   867,   868,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   878,   879,   880,    -1,    -1,   883,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1125,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1133,    -1,    -1,    -1,   757,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    49,    -1,  1162,    52,    -1,    54,    -1,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1543,    -1,  1180,    -1,    -1,    -1,    73,    -1,    -1,     0,
      -1,    -1,     3,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   966,  1200,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   100,   101,  1578,   103,    -1,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,    -1,   119,   120,   121,    -1,   123,   124,    -1,    -1,
      -1,    -1,    -1,    -1,   131,  1243,    -1,  1012,    -1,  1612,
      -1,    -1,    -1,    -1,    -1,    -1,   874,    -1,   145,   146,
     147,    -1,    -1,  1626,   151,   152,    77,   154,   155,   887,
     888,    -1,    -1,    -1,   161,   162,   163,   164,   165,   166,
     167,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1655,    -1,  1059,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1068,  1069,  1070,  1071,    -1,    -1,    -1,
      -1,  1076,  1077,    -1,    -1,    -1,    -1,   180,    -1,    -1,
      -1,  1086,    -1,    -1,    -1,   136,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1107,    -1,  1109,    -1,    -1,  1345,  1346,  1347,
    1348,  1349,  1350,    -1,    49,    -1,    -1,    52,    -1,    54,
      -1,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1374,  1375,    73,    -1,
    1743,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1162,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   100,   101,    -1,   103,    -1,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,  1188,   119,   120,   121,   238,   123,   124,
    1195,    -1,  1197,  1198,    -1,    -1,   131,    -1,    -1,    -1,
      -1,   252,    -1,    -1,  1209,    -1,  1211,    -1,  1213,    -1,
    1215,   262,  1450,    -1,    -1,  1220,   151,   152,    -1,   154,
     155,   272,    -1,    -1,   159,    -1,   161,   162,   163,   164,
     165,   166,   167,    -1,    -1,   286,   287,    -1,    -1,    -1,
      -1,    -1,   293,   294,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   309,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   369,  1125,    -1,    -1,
     373,   374,    -1,    -1,    -1,  1133,    -1,  1282,   329,    -1,
     383,   384,    -1,    -1,  1289,  1290,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   398,   399,    -1,    -1,    -1,
      -1,    -1,  1905,    -1,  1162,  1543,    -1,    -1,  1313,    -1,
      -1,    -1,    -1,    -1,    -1,  1320,   419,    -1,    -1,  1324,
      -1,    -1,  1180,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1933,    -1,    -1,    -1,    -1,   386,    -1,    -1,    -1,    -1,
    1578,    -1,  1200,    -1,    -1,    -1,   449,    -1,    -1,  1354,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   417,    -1,  1971,    -1,
      -1,    -1,    -1,    -1,  1612,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    48,  1243,    -1,    -1,  1626,  1394,
      -1,    -1,   443,    -1,    -1,    -1,   447,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      74,    -1,    -1,    -1,    -1,   466,    -1,  1655,    -1,   470,
     471,    -1,    -1,   474,    -1,  1430,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1438,    -1,  1440,    -1,    -1,   489,   490,
     491,   492,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   120,   508,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   516,    -1,    -1,    -1,   133,
      -1,   135,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1488,  1489,    -1,    -1,  1345,  1346,  1347,
    1348,  1349,  1350,   544,    -1,    -1,    -1,    -1,  1503,  1504,
      -1,  1506,   166,    -1,    -1,  1743,    -1,    -1,    -1,    -1,
    1515,    -1,    -1,    -1,    -1,    -1,  1374,  1375,    -1,    -1,
    1525,  1526,    -1,    -1,   575,    -1,   190,    -1,    -1,    -1,
      -1,   582,    -1,    -1,    -1,    -1,    -1,   588,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   612,   613,    -1,   228,    -1,    -1,    -1,   232,    -1,
      -1,   235,   236,    -1,    -1,   239,    -1,    -1,   242,   243,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1450,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   710,   711,   712,
     713,   714,   715,   716,   717,   718,   719,   720,   721,   722,
     723,   724,   725,   726,   727,   728,    -1,    -1,    -1,   680,
      -1,    -1,    -1,  1638,  1639,    -1,    -1,    -1,    -1,   303,
      -1,    -1,   306,    -1,  1649,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   327,   328,    -1,    -1,  1905,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   343,
      -1,  1686,  1687,    -1,    -1,  1543,    -1,   790,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1933,    -1,    -1,   749,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   764,    -1,    -1,    -1,   768,    -1,    -1,
    1578,    -1,    -1,    -1,    -1,    -1,   777,    -1,    -1,    -1,
      -1,    -1,    -1,  1971,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   799,    -1,
      -1,    -1,    -1,    -1,  1612,    -1,    -1,   808,    -1,    -1,
      -1,    -1,    -1,   814,    -1,    -1,    -1,    -1,  1626,  1774,
      -1,    -1,    -1,    -1,   438,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1794,
      -1,    -1,  1797,  1798,    -1,    -1,    -1,  1655,    -1,  1804,
     851,    -1,    -1,    -1,    -1,    -1,    -1,   858,    49,    -1,
      -1,    52,    -1,    54,    -1,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   929,    -1,    -1,   493,
      -1,    -1,    73,    -1,   885,    -1,    -1,    -1,    -1,    -1,
      -1,   505,    -1,    -1,    -1,   948,    -1,    -1,    -1,    -1,
     953,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,
     101,   964,   103,    -1,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,    -1,   119,   120,
     121,    -1,   123,   124,    -1,  1743,    -1,    -1,    -1,    -1,
     131,   942,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1006,   145,   146,   147,    -1,    -1,    -1,
     151,   152,    -1,   154,   155,   579,    -1,    -1,    -1,    -1,
     161,   162,   163,   164,   165,   166,   167,    -1,    -1,    -1,
    1935,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   620,   621,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   633,
      -1,    -1,    -1,    -1,  1025,    -1,    -1,    -1,  1029,    -1,
      -1,    -1,    -1,    -1,    -1,  1036,    -1,    -1,    -1,    -1,
      -1,    -1,  1997,    -1,    -1,  1046,    -1,    -1,    -1,    -1,
      -1,    -1,  1053,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1062,    -1,  1064,    -1,    -1,  2021,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1133,  2036,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1096,  2051,  1905,    -1,  1100,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1114,    -1,    -1,  1117,    -1,    -1,    -1,
      -1,    -1,   164,    -1,    -1,  1933,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   753,
     754,    -1,    -1,    -1,    -1,   759,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1210,   200,   201,
      -1,    -1,    -1,  1971,    -1,    -1,   780,    -1,    -1,   783,
     784,    -1,   786,    -1,   788,   789,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     232,    -1,    -1,  1246,  1247,  1248,    -1,   239,    -1,    -1,
    1253,  1254,    -1,    -1,    -1,    -1,    -1,  1208,    -1,    -1,
      -1,    -1,    -1,   827,    -1,    -1,    -1,   831,    -1,    -1,
      -1,    -1,    -1,    -1,  1277,    -1,    -1,    13,    14,    15,
      16,    17,  1233,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,  1314,  1315,    -1,   306,    51,    -1,    53,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   896,   897,    -1,    -1,    72,    -1,   330,   331,
      -1,    -1,    -1,    -1,    -1,    -1,   910,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   351,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1327,    -1,    -1,    -1,
    1331,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1361,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     422,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   438,   439,    -1,   441,
     442,    -1,    -1,    -1,    -1,    -1,    -1,   449,    -1,    -1,
    1411,   453,    -1,  1414,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1042,    -1,
      -1,  1432,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   494,    -1,    -1,    -1,   498,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1083,
      -1,    -1,    -1,    -1,    -1,    -1,  1090,    -1,    -1,  1093,
      -1,    -1,    -1,  1484,    -1,    -1,    -1,    -1,   530,    -1,
      -1,    -1,  1493,    -1,    -1,    -1,  1497,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1511,  1512,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   580,    -1,
       5,   583,    -1,    -1,    -1,    -1,    -1,    -1,    13,    14,
      15,    16,    17,    -1,    -1,    -1,    -1,    -1,    -1,   601,
     602,    -1,    -1,    -1,  1617,    -1,    -1,    -1,    -1,    -1,
     612,    -1,    -1,    -1,   616,    -1,    -1,    -1,    -1,    -1,
      -1,   623,    -1,   625,    49,    -1,    -1,    52,    -1,    54,
      -1,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1216,    -1,    -1,    -1,    -1,    72,    73,    -1,
    1224,  1225,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1627,  1628,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   100,   101,    -1,   103,    -1,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,    -1,   119,   120,   121,    -1,   123,   124,
      -1,    -1,    -1,    -1,    -1,    -1,   131,    -1,    -1,  1283,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1292,    -1,
      -1,  1295,    -1,  1297,  1298,    -1,   151,    -1,    -1,   154,
     155,   733,    -1,    -1,    -1,    -1,   161,   162,   163,   164,
     165,   166,   167,    -1,    -1,    -1,    -1,   749,   750,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   759,   760,    -1,
     762,   763,    -1,    -1,  1338,    -1,    -1,    -1,    -1,    -1,
      48,    -1,   774,    -1,    -1,   777,    -1,   779,   780,    -1,
      -1,    -1,    -1,    -1,   786,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   796,   797,  1757,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    83,   815,  1827,    -1,    -1,   819,    -1,    -1,
      -1,   823,  1783,    -1,    -1,   827,   828,    -1,    99,   831,
     832,  1405,    -1,    -1,    -1,    -1,    -1,   839,    -1,    -1,
      -1,  1802,   120,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1867,   133,    -1,   135,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1829,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,   881,
     882,    -1,   153,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   164,    -1,  1856,    -1,    -1,  1859,    56,
      57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     912,    -1,    -1,    -1,    -1,    -1,    -1,   188,  1492,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     201,    -1,    -1,   204,    91,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1521,    -1,    -1,
     952,    -1,  1965,    -1,  1967,    -1,    -1,   235,   236,    -1,
      -1,   239,    -1,    -1,   242,   243,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1547,    -1,    -1,    -1,    -1,    -1,  1553,
      -1,    -1,   253,    -1,   141,    -1,    -1,   144,    -1,    -1,
      -1,    -1,    -1,  2006,    -1,   266,    -1,    -1,    -1,  1960,
      -1,   158,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1019,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2038,    -1,    -1,    -1,    -1,
      -1,   302,    -1,    -1,  1036,  1037,    -1,    -1,    -1,   310,
      -1,  1043,    -1,    -1,    -1,    -1,    -1,    -1,  1622,   327,
     328,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   330,
      -1,   332,   219,    -1,    -1,   343,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2086,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1085,    -1,    -1,    -1,    -1,  1090,  1091,
      -1,  1093,  1094,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   263,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   385,    -1,   273,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   284,  1702,  1703,
      -1,    -1,    -1,    -1,    -1,    -1,  1710,    -1,    -1,    -1,
    1714,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   419,    -1,
      -1,    -1,    -1,    -1,   311,    -1,    -1,    -1,    -1,    -1,
     438,   318,   319,    -1,    -1,    -1,   323,    -1,    -1,    -1,
      -1,   385,    -1,    -1,    -1,    -1,    -1,    -1,   449,    -1,
      -1,    -1,    -1,    -1,   455,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   356,
      -1,    -1,    -1,    -1,   361,    -1,    -1,   364,  1210,    -1,
      -1,    -1,    -1,    -1,  1216,  1217,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   505,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1239,    -1,    -1,
      -1,    -1,    -1,  1817,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   524,   525,    -1,    -1,    -1,    -1,   530,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1283,  1284,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1292,  1293,    -1,  1295,    -1,   452,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1306,  1307,    -1,   464,   465,    -1,
     524,   525,   583,    -1,    -1,    -1,    -1,  1891,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   602,    -1,   604,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   620,   621,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   625,   633,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   646,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   659,    -1,
      -1,   662,   663,    -1,   665,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   674,    -1,    -1,   677,   678,   679,   188,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   201,    -1,  1426,  1999,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   214,    -1,   216,   595,    -1,
      -1,    -1,    -1,    -1,    -1,   659,    -1,    -1,    -1,    -1,
    1452,   665,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     674,    -1,   733,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   753,   754,    -1,    -1,   693,
      -1,   759,    -1,    -1,    -1,    -1,   757,   644,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   780,   774,    -1,   783,   784,    -1,   786,    -1,
     788,   789,    -1,    -1,    -1,   729,    -1,    -1,    -1,    -1,
      -1,    -1,   301,    -1,    -1,   796,   797,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1537,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   815,    -1,    -1,    -1,    -1,   827,
      -1,  1553,    -1,   831,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    -1,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      -1,    50,    51,    -1,    53,    -1,    55,    -1,    -1,    -1,
     881,    -1,    -1,    -1,    -1,    -1,    -1,   888,   896,   897,
      -1,  1623,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,
     901,    -1,   910,    -1,   791,    -1,    -1,    -1,    -1,    -1,
      -1,   798,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,   107,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1680,  1681,
      -1,   952,   131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1695,  1696,    -1,   475,    -1,    -1,    -1,
      -1,    -1,   481,    -1,    -1,   154,   155,   486,    -1,  1711,
      -1,    -1,    -1,   162,   163,    -1,   873,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    49,    -1,    -1,    52,    -1,
      54,    -1,    56,    -1,    -1,    -1,    -1,    -1,   895,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1042,    -1,    -1,    -1,    -1,    -1,
     927,    -1,    -1,    -1,   931,    -1,   100,   101,    -1,   103,
      -1,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   573,   119,   120,   121,    -1,   123,
     124,    -1,    -1,    -1,    -1,  1083,    -1,   131,    -1,    -1,
      -1,    -1,  1090,  1815,  1085,  1093,    -1,    -1,    -1,    -1,
      -1,  1823,    -1,   602,    -1,    -1,    -1,   151,   152,    -1,
     154,   155,    -1,    -1,    -1,   159,   615,   161,   162,   163,
     164,   165,   166,   167,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1125,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1887,    -1,    -1,   667,  1891,
    1892,    -1,    -1,  1895,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1179,    -1,
      -1,    -1,   691,   692,    -1,    -1,   695,    -1,   697,    -1,
      -1,    -1,    -1,    -1,   703,    -1,   705,   706,    -1,  1200,
      -1,    -1,    -1,    -1,  1936,    -1,    -1,    -1,  1216,  1210,
      -1,    -1,    -1,    -1,    -1,    -1,  1224,  1225,    -1,    -1,
      -1,    -1,    -1,    -1,   733,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   746,  1239,    -1,
      -1,    -1,  1243,    -1,    -1,    -1,    -1,    -1,   757,    -1,
      -1,  1138,  1139,  1140,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   771,    -1,    -1,   774,    -1,  1999,  2000,    -1,
      -1,    -1,    -1,    -1,    -1,  1283,    -1,    -1,    -1,    -1,
      -1,  1168,    -1,    -1,  1292,    -1,    -1,  1295,    -1,  1297,
    1298,    -1,   801,    -1,    -1,   804,  1183,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1306,  1307,    -1,  2040,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1338,   840,    -1,    -1,    -1,    -1,  1223,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1345,  1346,  1347,    -1,  1349,  1350,
      -1,    -1,    -1,    -1,    -1,  1356,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   888,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   901,   902,    -1,    -1,    -1,  1405,    -1,    -1,
     909,    -1,    -1,  1404,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1426,    -1,  1371,    -1,   938,
    1374,  1375,    -1,    -1,    -1,    -1,  1380,    -1,    -1,    -1,
    1384,    -1,  1386,   952,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   960,    -1,    -1,    -1,    -1,    -1,    -1,   967,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1359,    -1,    -1,  1362,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1492,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    49,    -1,    -1,    52,    -1,    54,    -1,    56,
      -1,    -1,    -1,    -1,  1013,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1521,    -1,    -1,    73,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1424,  1425,    -1,
      -1,    -1,  1543,   100,   101,    -1,   103,    -1,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,    -1,   119,   120,   121,    -1,   123,   124,    -1,    -1,
      -1,  1080,    -1,  1082,   131,  1084,  1577,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1529,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   151,   152,   153,   154,   155,    -1,
      -1,    -1,    -1,    -1,   161,   162,   163,   164,   165,   166,
     167,    -1,    -1,    -1,  1622,    -1,    -1,  1561,    -1,    -1,
      -1,    -1,  1623,    -1,    -1,    -1,    -1,    -1,    -1,  1573,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1150,  1151,    -1,  1588,  1589,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1655,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1612,  1556,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1680,
    1681,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1702,  1703,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1214,  1714,    -1,    -1,    -1,
      -1,  1220,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1229,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1730,
    1239,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1256,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1645,    -1,
      -1,    -1,  1271,    -1,    -1,  1274,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1726,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1735,    -1,  1737,    -1,    -1,  1740,  1741,    -1,  1743,
      -1,    -1,    -1,    -1,  1748,    -1,    -1,    -1,    -1,  1817,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1326,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,  1364,    51,    -1,    53,  1860,
      -1,    -1,    57,    58,    59,    60,    61,    62,    63,    64,
      -1,    -1,    -1,  1382,    -1,    -1,  1385,    72,    -1,    -1,
      -1,    -1,  1769,  1891,    -1,    -1,  1887,    -1,    -1,    -1,
      -1,    -1,    -1,  1780,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1846,    -1,    -1,    -1,    -1,  1851,  1852,    -1,
      -1,   106,   107,     1,    -1,    -1,    -1,  1426,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1436,  1437,    -1,
      18,    -1,    -1,  1934,    -1,    -1,  1937,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1463,    -1,  1465,    -1,    -1,   154,
      -1,    49,    -1,    -1,    52,    -1,    54,    -1,    56,  1913,
      -1,  1915,    -1,    -1,  1918,  1919,    -1,    -1,    -1,  1923,
    1924,    -1,    -1,    71,    -1,    73,    74,    -1,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    90,    91,    92,    93,    94,    95,    96,    -1,
      98,    -1,   100,   101,    -1,   103,    -1,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,  1543,   123,   124,    -1,    -1,  1548,
      -1,    -1,    -1,   131,    -1,    -1,    -1,    -1,  1992,  1993,
    1994,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   150,   151,    -1,    -1,   154,   155,    -1,  2013,
      -1,   159,    -1,   161,   162,   163,   164,   165,   166,   167,
       1,    -1,  2026,  2027,  2028,    -1,    -1,    -1,   176,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    18,    -1,  1608,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    49,    -1,
      -1,    52,    -1,    54,    -1,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1653,    -1,    -1,  1656,    -1,    -1,
      71,    -1,    73,    74,    -1,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    90,
      91,    92,    93,    94,    95,    96,    -1,    98,    -1,   100,
     101,    -1,   103,    -1,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,    -1,   123,   124,    -1,    -1,    -1,    -1,     1,    -1,
     131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    18,    -1,    -1,    -1,   150,
     151,    -1,    -1,   154,   155,    -1,    -1,    -1,   159,    -1,
     161,   162,   163,   164,   165,   166,   167,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   176,    49,    -1,    -1,    52,
      -1,    54,    -1,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,    -1,
      73,    74,    -1,    76,    -1,    -1,    79,    80,    81,    82,
      83,    84,    85,    86,    87,    88,    89,    90,    91,    92,
      93,    94,    95,    96,    -1,    98,    -1,   100,   101,    -1,
     103,    -1,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,    -1,
     123,   124,    -1,    -1,    -1,    -1,    -1,    -1,   131,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,  1868,
      -1,   154,   155,    -1,    -1,    -1,   159,    -1,   161,   162,
     163,   164,   165,   166,   167,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   176,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      49,    -1,    51,    52,    53,    54,    -1,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    -1,    -1,    -1,
      69,    -1,    71,    72,    73,    74,    -1,    76,    -1,    -1,
      79,    80,    81,    82,    83,    84,    85,    86,    87,    88,
      89,    90,    91,    92,    93,    94,    95,    96,    -1,    98,
      -1,   100,   101,   102,   103,    -1,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,   121,    -1,   123,   124,    -1,    -1,    -1,    -1,
      -1,    -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   150,   151,    -1,    -1,   154,   155,    -1,    -1,    -1,
     159,    -1,   161,   162,   163,   164,   165,   166,   167,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   176,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    49,    -1,    51,    52,    53,    54,
      -1,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    -1,    -1,    -1,    69,    -1,    71,    72,    73,    74,
      -1,    76,    -1,    -1,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    -1,    98,    -1,   100,   101,   102,   103,    -1,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,    -1,   123,   124,
      -1,    -1,    -1,    -1,    -1,    -1,   131,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   151,    -1,    -1,   154,
     155,    -1,    -1,    -1,   159,    -1,   161,   162,   163,   164,
     165,   166,   167,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   176,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    49,    -1,
      51,    52,    53,    54,    -1,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    -1,    -1,    -1,    69,    -1,
      -1,    72,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,
     101,   102,   103,    -1,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,    -1,   119,   120,
     121,    -1,   123,   124,    -1,    -1,    -1,    -1,    -1,    -1,
     131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   145,   146,   147,    -1,    -1,    -1,
     151,   152,   153,   154,   155,    -1,    -1,    -1,    -1,    -1,
     161,   162,   163,   164,   165,   166,   167,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   176,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    49,    -1,    51,    52,    53,    54,    -1,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    -1,
      -1,    -1,    69,    -1,    -1,    72,    73,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   100,   101,   102,   103,    -1,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,    -1,   119,   120,   121,    -1,   123,   124,    -1,    -1,
      -1,    -1,    -1,    -1,   131,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   145,   146,
     147,    -1,    -1,    -1,   151,   152,    -1,   154,   155,    -1,
      -1,    -1,    -1,    -1,   161,   162,   163,   164,   165,   166,
     167,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   176,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    49,    -1,    51,    52,
      53,    54,    -1,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    -1,    -1,    -1,    69,    -1,    -1,    72,
      73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,   101,   102,
     103,    -1,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,    -1,   119,   120,   121,    -1,
     123,   124,    -1,    -1,    -1,    -1,    -1,    -1,   131,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,    -1,
     153,   154,   155,    -1,    -1,    -1,    -1,    -1,   161,   162,
     163,   164,   165,   166,   167,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    49,    -1,    51,    52,    53,    54,    -1,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    65,    -1,    -1,
      -1,    69,    -1,    -1,    72,    73,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   100,   101,   102,   103,    -1,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
      -1,   119,   120,   121,    -1,   123,   124,    -1,    -1,    -1,
      -1,    -1,    -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   151,    -1,    -1,   154,   155,    -1,    -1,
      -1,    -1,    -1,   161,   162,   163,   164,   165,   166,   167,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    49,    -1,    51,    52,    53,
      54,    -1,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    73,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   100,   101,    -1,   103,
      -1,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,    -1,   119,   120,   121,    -1,   123,
     124,    -1,    -1,    -1,    -1,    -1,    -1,   131,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,    -1,    -1,
     154,   155,    -1,    -1,    -1,    -1,    -1,   161,   162,   163,
     164,   165,   166,   167,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    49,
      -1,    51,    52,    53,    54,    -1,    56,    57,    58,    59,
      60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    73,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     100,   101,    -1,   103,    -1,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,    -1,   119,
     120,   121,    -1,   123,   124,    -1,    -1,    -1,    -1,    -1,
      -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   151,    -1,    -1,   154,   155,    -1,    -1,    -1,    -1,
      -1,   161,   162,   163,   164,   165,   166,   167,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    49,    -1,    51,    52,    53,    54,    -1,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   100,   101,    -1,   103,    -1,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,    -1,   119,   120,   121,    -1,   123,   124,    -1,
      -1,    -1,    -1,    -1,    -1,   131,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   151,    -1,    -1,   154,   155,
      -1,    -1,    -1,    -1,    -1,   161,   162,   163,   164,   165,
     166,   167,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    49,    -1,    51,
      52,    53,    54,    -1,    56,    57,    58,    59,    60,    61,
      62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,   101,
      -1,   103,    -1,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,    -1,   119,   120,   121,
      -1,   123,   124,    -1,    -1,    -1,    -1,    -1,    -1,   131,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,
      -1,    -1,   154,   155,    -1,    -1,    -1,    -1,    -1,   161,
     162,   163,   164,   165,   166,   167,     1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    -1,    50,    51,    -1,    53,    -1,
      55,    -1,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    -1,    -1,    -1,    69,    -1,    -1,    72,    -1,    -1,
      -1,    -1,    77,    78,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   102,    -1,    -1,
      -1,   106,   107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   131,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   150,    -1,    -1,    -1,   154,
     155,    -1,    -1,    -1,    -1,    -1,    -1,   162,   163,     1,
      -1,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    -1,    50,    51,
      -1,    53,    -1,    55,    -1,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    -1,    -1,    -1,    69,    -1,    -1,
      72,    -1,    -1,    -1,    -1,    77,    78,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     102,    -1,    -1,    -1,   106,   107,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   131,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   150,    -1,
      -1,    -1,   154,   155,    -1,    -1,    -1,    -1,    -1,    -1,
     162,   163,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    -1,    -1,    -1,    69,    -1,
      -1,    72,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    98,    -1,    -1,
      -1,   102,   103,    -1,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,    -1,    -1,    -1,   118,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     151,   152,    -1,   154,   155,    -1,    -1,    -1,    -1,    -1,
      -1,   162,   163,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    -1,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    -1,
      50,    51,    -1,    53,    -1,    55,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   103,    -1,    -1,   106,   107,   108,   109,
     110,   111,   112,   113,   114,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   151,   152,    -1,   154,   155,    -1,    -1,    -1,   159,
      -1,    -1,   162,   163,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    -1,    -1,    -1,
      69,    -1,    -1,    72,    -1,    -1,    -1,    -1,    77,    78,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   102,    -1,    -1,    -1,   106,   107,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   150,    -1,    -1,    -1,   154,   155,    -1,    -1,    -1,
      -1,    -1,    -1,   162,   163,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    57,
      58,    59,    60,    61,    62,    63,    64,    65,    -1,    -1,
      -1,    69,    -1,    -1,    72,    -1,    -1,    -1,    -1,    77,
      78,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   102,    -1,    -1,    -1,   106,   107,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   150,    -1,    -1,    -1,   154,   155,    -1,    -1,
      -1,    -1,    -1,    -1,   162,   163,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    -1,    50,    51,    -1,    53,    -1,    55,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   131,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   151,   152,    -1,   154,   155,    -1,
       3,    -1,    -1,    -1,    -1,   162,   163,    10,    -1,    -1,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,    -1,    -1,    -1,    57,    58,    59,    60,    61,    62,
      63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   102,
      -1,    -1,    -1,   106,   107,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   131,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   150,    -1,    -1,
      -1,   154,   155,    -1,     3,    -1,    -1,    -1,    -1,   162,
     163,    10,    -1,    -1,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    57,    58,
      59,    60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    73,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   102,    -1,    -1,    -1,   106,   107,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   150,    -1,    -1,    -1,   154,   155,    -1,     3,    -1,
      -1,    -1,    -1,   162,   163,    10,    -1,    -1,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,
      -1,    -1,    57,    58,    59,    60,    61,    62,    63,    64,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    73,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   102,    -1,    -1,
      -1,   106,   107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   131,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   150,    -1,    -1,    -1,   154,
     155,    -1,     3,    -1,    -1,    -1,    -1,   162,   163,    10,
      -1,    -1,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,
      61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   102,    -1,    -1,    -1,   106,   107,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     3,    -1,    -1,   150,
      -1,    -1,    -1,   154,   155,    -1,    13,    14,    15,    16,
      17,   162,   163,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    -1,    50,    51,    -1,    53,    -1,    55,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,
     107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   131,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   154,   155,    -1,
      -1,    -1,    -1,    -1,    -1,   162,   163,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    -1,    50,    51,    -1,    53,    -1,    55,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   131,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   151,   152,    -1,   154,   155,    -1,
      -1,    -1,   159,    -1,    -1,   162,   163,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    -1,    50,    51,    -1,    53,    -1,    55,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   131,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   151,   152,    -1,   154,   155,    -1,
      -1,    -1,    -1,    -1,    -1,   162,   163,     4,     5,     6,
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
      -1,    -1,    -1,    -1,   131,    -1,   133,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   153,   154,   155,    -1,
      -1,    -1,    -1,    -1,    -1,   162,   163,     4,     5,     6,
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
      -1,    -1,    -1,    -1,   131,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   153,   154,   155,    -1,
      -1,    -1,    -1,    -1,    -1,   162,   163,     4,     5,     6,
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
      -1,    -1,    -1,    -1,   131,    -1,   133,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   154,   155,    -1,
      -1,    -1,    -1,    -1,    -1,   162,   163,     4,     5,     6,
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
      -1,    -1,    -1,    -1,   131,    -1,   133,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   154,   155,    -1,
      -1,    -1,    -1,    -1,    -1,   162,   163,     4,     5,     6,
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
      -1,    -1,    -1,    -1,   131,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   154,   155,    -1,
      -1,    -1,    -1,    -1,    -1,   162,   163,     4,     5,     6,
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
      -1,    -1,    -1,    -1,   131,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   154,   155,    -1,
      -1,    -1,    -1,    -1,    -1,   162,   163,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47,    48,    -1,    50,    51,    -1,    53,    -1,    55,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,
     107,    -1,    -1,    -1,    18,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   131,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    49,    -1,    -1,    52,    -1,
      54,    -1,    56,    -1,    -1,    -1,    -1,   154,   155,    -1,
      -1,    -1,    -1,    -1,    -1,   162,   163,    71,    -1,    73,
      -1,    -1,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    86,    87,    88,    89,    90,    91,    92,    93,
      94,    95,    96,    -1,    98,    -1,   100,   101,    -1,   103,
      -1,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,    18,   123,
     124,    -1,    -1,    -1,    -1,    -1,    -1,   131,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,    -1,    49,
     154,   155,    52,    -1,    54,   159,    56,   161,   162,   163,
     164,   165,   166,   167,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    71,   176,    73,    -1,    -1,    76,    -1,    -1,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    92,    93,    94,    95,    96,    -1,    98,    -1,
     100,   101,    -1,   103,    -1,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,    -1,   123,   124,    -1,    -1,    -1,    -1,    -1,
      -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   151,    -1,    -1,   154,   155,    -1,    -1,    -1,   159,
      -1,   161,   162,   163,   164,   165,   166,   167,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   176,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    49,    -1,    51,    52,    53,    54,    -1,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    85,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   100,   101,    -1,   103,    -1,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,    -1,   119,   120,   121,    -1,   123,   124,    -1,
      -1,    -1,    -1,    -1,    -1,   131,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   151,    -1,    -1,   154,   155,
      -1,    -1,    -1,   159,    -1,   161,   162,   163,   164,   165,
     166,   167,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    49,    -1,
      51,    52,    53,    54,    -1,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,
     101,    -1,   103,    -1,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,    -1,   119,   120,
     121,    -1,   123,   124,    -1,    -1,    -1,    -1,    -1,    -1,
     131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     151,    -1,   153,   154,   155,    -1,    -1,    -1,    -1,    -1,
     161,   162,   163,   164,   165,   166,   167,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    49,    -1,    51,    52,    53,    54,    -1,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   100,   101,    -1,   103,    -1,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,    -1,   119,   120,   121,    -1,   123,   124,    -1,
      -1,    -1,    -1,    -1,    -1,   131,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   151,    -1,    -1,   154,   155,
      -1,    -1,    -1,   159,    -1,   161,   162,   163,   164,   165,
     166,   167,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    49,    -1,
      51,    52,    53,    54,    -1,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,
     101,    -1,   103,    -1,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,    -1,   119,   120,
     121,    -1,   123,   124,    -1,    -1,    -1,    -1,    -1,    -1,
     131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     151,    -1,    -1,   154,   155,    -1,    -1,    -1,    -1,    -1,
     161,   162,   163,   164,   165,   166,   167,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    49,    -1,    51,    52,    53,    54,    -1,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   100,   101,    -1,   103,    -1,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,    -1,   119,   120,   121,    -1,   123,   124,    -1,
      -1,    -1,    -1,    -1,    -1,   131,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   151,    -1,    -1,   154,   155,
      -1,    -1,    -1,    -1,    -1,   161,   162,   163,   164,   165,
     166,   167,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    49,    -1,
      51,    52,    53,    54,    -1,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,
     101,    -1,   103,    -1,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,    -1,   119,   120,
     121,    -1,   123,   124,    -1,    -1,    -1,    -1,    -1,    -1,
     131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     151,    -1,    -1,   154,   155,    -1,    -1,    -1,    -1,    -1,
     161,   162,   163,   164,   165,   166,   167,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,   106,
     107,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,   158,    53,    -1,    -1,    -1,    57,    58,    59,    60,
      61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,   106,   107,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    -1,    50,    51,    -1,
      53,    -1,    55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   154,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   106,   107,    -1,    -1,    -1,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,
      -1,   154,    57,    58,    59,    60,    61,    62,    63,    64,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   131,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   151,   152,    -1,   154,
     155,    -1,    -1,    -1,   159,    -1,    -1,   162,   163,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,    -1,    -1,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,
      -1,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   131,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,   152,    -1,
     154,   155,    -1,    -1,    -1,    -1,    -1,    -1,   162,   163,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,    -1,    -1,    -1,    57,    58,    59,    60,    61,    62,
      63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     103,    -1,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   131,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   154,   155,    -1,    -1,    -1,    -1,    -1,    -1,   162,
     163,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,    -1,    -1,    -1,    57,    58,    59,    60,    61,    62,
      63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   106,   107,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     133,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     153,   154,     4,     5,     6,     7,     8,     9,    10,    11,
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
      -1,   133,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,    -1,
      -1,    20,   154,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      -1,    50,    51,    -1,    53,    -1,    55,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   103,    -1,    -1,   106,   107,   108,
     109,   110,   111,   112,   113,   114,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   151,   152,    -1,   154,   155,    -1,    -1,    -1,
      -1,    -1,    -1,   162,   163,     4,     5,     6,     7,     8,
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
      -1,    -1,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,    -1,   154,    57,    58,    59,    60,
      61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   106,   107,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   153,   154,   155,    -1,    -1,    -1,    -1,    -1,
      -1,   162,   163,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,
      60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    78,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   106,   107,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   154,   155,    -1,    -1,    -1,    -1,
      -1,    -1,   162,   163,    13,    14,    15,    16,    17,    18,
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
      -1,    -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   154,   155,    -1,    -1,    -1,
      -1,    -1,    -1,   162,   163,    13,    14,    15,    16,    17,
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
      -1,    -1,    -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   154,   155,    -1,    -1,
      -1,    -1,    -1,    -1,   162,   163,    13,    14,    15,    16,
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
      -1,    -1,    -1,    -1,   131,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   154,   155,    -1,
      -1,    -1,    -1,    -1,    -1,   162,   163,    13,    14,    15,
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
      -1,    -1,    -1,    -1,    -1,   131,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   154,   155,
      -1,    -1,    -1,    -1,    -1,    -1,   162,   163,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,
      -1,    -1,    57,    58,    59,    60,    61,    62,    63,    64,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   106,   107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   131,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   154,
     155,    -1,    -1,    -1,    -1,    -1,    -1,   162,   163,    13,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   131,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     154,   155,    -1,    -1,    -1,    -1,    -1,    -1,   162,   163,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,    -1,    -1,    -1,    57,    58,    59,    60,    61,    62,
      63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   106,   107,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   131,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   154,   155,    -1,    -1,    -1,    -1,    -1,    -1,   162,
     163,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   131,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   154,   155,    -1,    -1,    -1,    -1,    -1,    -1,
     162,   163,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    -1,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    -1,    50,    51,
      -1,    53,    -1,    55,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    13,    14,    15,    16,    17,    18,    -1,    20,
      72,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,   106,   107,    57,    58,    59,    60,
      61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   154,    -1,    -1,   106,   107,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   154,   155,    -1,    -1,    -1,    -1,    -1,
      -1,   162,   163,    13,    14,    15,    16,    17,    18,    -1,
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
      -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   154,    -1,    -1,    13,    14,    15,
      16,    17,   162,   163,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    -1,    50,    51,    -1,    53,    -1,    55,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     106,   107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   131,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   154,   155,
      -1,    13,    14,    15,    16,    17,   162,   163,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    -1,    50,    51,
      -1,    53,    -1,    55,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    13,    14,    15,    16,    17,    18,    -1,    20,
      72,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,    -1,   106,   107,    57,    58,    59,    60,
      61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   131,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   154,    -1,    -1,   106,   107,    -1,    -1,    -1,
     162,   163,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,    -1,    53,   154,    -1,    -1,    57,    58,    59,    60,
      61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   103,    -1,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,    13,    14,    15,    16,    17,    -1,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      -1,    50,    51,   154,    53,    -1,    55,    -1,    49,    -1,
      -1,    52,    -1,    54,    -1,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,   107,   100,
     101,    -1,   103,    -1,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,    -1,   119,   120,
     121,    -1,   123,   124,    49,    -1,    -1,    52,    -1,    54,
     131,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   154,    -1,    -1,    73,    -1,
     151,    -1,    -1,   154,   155,    -1,    -1,    -1,   159,    -1,
     161,   162,   163,   164,   165,   166,   167,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   100,   101,    -1,   103,    -1,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,    -1,   119,   120,   121,    49,   123,   124,
      52,    -1,    54,    -1,    56,    -1,   131,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,   151,   152,    -1,   154,
     155,    -1,    -1,    -1,    -1,    -1,   161,   162,   163,   164,
     165,   166,   167,    -1,    -1,    -1,    -1,    -1,   100,   101,
      -1,   103,    -1,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,    -1,   119,   120,   121,
      49,   123,   124,    52,    -1,    54,    -1,    56,    -1,   131,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,   151,
      -1,    -1,   154,   155,    -1,    -1,    -1,   159,    -1,   161,
     162,   163,   164,   165,   166,   167,    -1,    -1,    -1,    -1,
      -1,   100,   101,    -1,   103,    -1,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,    -1,
     119,   120,   121,    49,   123,   124,    52,    -1,    54,    -1,
      56,    -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,   151,   152,    -1,   154,   155,    -1,    -1,    -1,
      -1,    -1,   161,   162,   163,   164,   165,   166,   167,    -1,
      -1,    -1,    -1,    -1,   100,   101,    -1,   103,    -1,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,    -1,   119,   120,   121,    49,   123,   124,    52,
      -1,    54,    -1,    56,    -1,   131,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,   151,    -1,    -1,   154,   155,
      -1,    -1,   158,    -1,    -1,   161,   162,   163,   164,   165,
     166,   167,    -1,    -1,    -1,    -1,    -1,   100,   101,    -1,
     103,    -1,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,    -1,   119,   120,   121,    49,
     123,   124,    52,    -1,    54,    -1,    56,    -1,   131,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,   151,    -1,
      -1,   154,   155,    -1,    -1,    -1,   159,    -1,   161,   162,
     163,   164,   165,   166,   167,    -1,    -1,    -1,    -1,    -1,
     100,   101,    -1,   103,    -1,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,    -1,   119,
     120,   121,    49,   123,   124,    52,    -1,    54,    -1,    56,
      -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,
      -1,   151,   152,    -1,   154,   155,    -1,    -1,    -1,    -1,
      -1,   161,   162,   163,   164,   165,   166,   167,    -1,    -1,
      -1,    -1,    -1,   100,   101,    -1,   103,    -1,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,    -1,   119,   120,   121,    49,   123,   124,    52,    -1,
      54,    -1,    56,    -1,   131,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,   151,    -1,    -1,   154,   155,    -1,
      -1,    -1,   159,    -1,   161,   162,   163,   164,   165,   166,
     167,    -1,    -1,    -1,    -1,    -1,   100,   101,    -1,   103,
      -1,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,    -1,   119,   120,   121,    49,   123,
     124,    52,    -1,    54,    -1,    56,    -1,   131,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,   151,    -1,   153,
     154,   155,    -1,    -1,    -1,    -1,    -1,   161,   162,   163,
     164,   165,   166,   167,    -1,    -1,    -1,    -1,    -1,   100,
     101,    -1,   103,    -1,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,    -1,   119,   120,
     121,    49,   123,   124,    52,    -1,    54,    -1,    56,    -1,
     131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
     151,   152,    -1,   154,   155,    -1,    -1,    -1,    -1,    -1,
     161,   162,   163,   164,   165,   166,   167,    -1,    -1,    -1,
      -1,    -1,   100,   101,    -1,   103,    -1,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
      -1,   119,   120,   121,    49,   123,   124,    52,    -1,    54,
      -1,    56,    -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,
      -1,    -1,    -1,   151,   152,    -1,   154,   155,    -1,    -1,
      -1,    -1,    -1,   161,   162,   163,   164,   165,   166,   167,
      -1,    -1,    -1,    -1,    -1,   100,   101,    -1,   103,    -1,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,    -1,   119,   120,   121,    49,   123,   124,
      52,    -1,    54,    -1,    56,    -1,   131,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,   151,   152,    -1,   154,
     155,    -1,    -1,    -1,    -1,    -1,   161,   162,   163,   164,
     165,   166,   167,    -1,    -1,    -1,    -1,    -1,   100,   101,
      -1,   103,    -1,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,    -1,   119,   120,   121,
      49,   123,   124,    52,    -1,    54,    -1,    56,    -1,   131,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,   151,
     152,    -1,   154,   155,    -1,    -1,    -1,    -1,    -1,   161,
     162,   163,   164,   165,   166,   167,    -1,    -1,    -1,    -1,
      -1,   100,   101,    -1,   103,    -1,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,    -1,
     119,   120,   121,    49,   123,   124,    52,    -1,    54,    -1,
      56,    -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,   151,   152,    -1,   154,   155,    -1,    -1,    -1,
      -1,    -1,   161,   162,   163,   164,   165,   166,   167,    -1,
      -1,    -1,    -1,    -1,   100,   101,    -1,   103,    -1,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,    -1,   119,   120,   121,    49,   123,   124,    52,
      -1,    54,    -1,    56,    -1,   131,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,   151,   152,    -1,   154,   155,
      -1,    -1,    -1,    -1,    -1,   161,   162,   163,   164,   165,
     166,   167,    -1,    -1,    -1,    -1,    -1,   100,   101,    -1,
     103,    -1,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,    -1,   119,   120,   121,    49,
     123,   124,    52,    -1,    54,    -1,    56,    -1,   131,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,   151,   152,
      -1,   154,   155,    -1,    -1,    -1,    -1,    -1,   161,   162,
     163,   164,   165,   166,   167,    -1,    -1,    -1,    -1,    -1,
     100,   101,    -1,   103,    -1,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,    -1,   119,
     120,   121,    49,   123,   124,    52,    -1,    54,    -1,    56,
      -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,
      -1,   151,   152,    -1,   154,   155,    -1,    -1,    -1,    -1,
      -1,   161,   162,   163,   164,   165,   166,   167,    -1,    -1,
      -1,    -1,    -1,   100,   101,    -1,   103,    -1,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,    -1,   119,   120,   121,    49,   123,   124,    52,    -1,
      54,    -1,    56,    -1,   131,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,   151,   152,    -1,   154,   155,    -1,
      -1,    -1,    -1,    -1,   161,   162,   163,   164,   165,   166,
     167,    -1,    -1,    -1,    -1,    -1,   100,   101,    -1,   103,
      -1,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,    -1,   119,   120,   121,    49,   123,
     124,    52,    -1,    54,    -1,    56,    -1,   131,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,   151,   152,    -1,
     154,   155,    -1,    -1,    -1,    -1,    -1,   161,   162,   163,
     164,   165,   166,   167,    -1,    -1,    -1,    -1,    -1,   100,
     101,    -1,   103,    -1,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,    -1,   119,   120,
     121,    49,   123,   124,    52,    -1,    54,    -1,    56,    -1,
     131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
     151,   152,    -1,   154,   155,    -1,    -1,    -1,    -1,    -1,
     161,   162,   163,   164,   165,   166,   167,    -1,    -1,    -1,
      -1,    -1,   100,   101,    -1,   103,    -1,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
      -1,   119,   120,   121,    49,   123,   124,    52,    -1,    54,
      -1,    56,    -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,
      -1,    -1,    -1,   151,   152,    -1,   154,   155,    -1,    -1,
      -1,    -1,    -1,   161,   162,   163,   164,   165,   166,   167,
      -1,    -1,    -1,    -1,    -1,   100,   101,    -1,   103,    -1,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,    -1,   119,   120,   121,    49,   123,   124,
      52,    -1,    54,    -1,    56,    -1,   131,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,   151,   152,    -1,   154,
     155,    -1,    -1,    -1,    -1,    -1,   161,   162,   163,   164,
     165,   166,   167,    -1,    -1,    -1,    -1,    -1,   100,   101,
      -1,   103,    -1,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,    -1,   119,   120,   121,
      49,   123,   124,    52,    -1,    54,    -1,    56,    -1,   131,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,   151,
     152,    -1,   154,   155,    -1,    -1,    -1,    -1,    -1,   161,
     162,   163,   164,   165,   166,   167,    -1,    -1,    -1,    -1,
      -1,   100,   101,    -1,   103,    -1,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,    -1,
     119,   120,   121,    49,   123,   124,    52,    -1,    54,    -1,
      56,    -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,   151,   152,    -1,   154,   155,    -1,    -1,    -1,
      -1,    -1,   161,   162,   163,   164,   165,   166,   167,    -1,
      -1,    -1,    -1,    -1,   100,   101,    -1,   103,    -1,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,    -1,   119,   120,   121,    49,   123,   124,    52,
      -1,    54,    -1,    56,    -1,   131,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    -1,    -1,    -1,    -1,   151,    -1,    -1,   154,   155,
      -1,    -1,    -1,    -1,    -1,   161,   162,   163,   164,   165,
     166,   167,    -1,    -1,    -1,    -1,    -1,   100,   101,    -1,
     103,    -1,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,    -1,   119,   120,   121,    49,
     123,   124,    52,    -1,    54,    -1,    56,    -1,   131,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,   151,    -1,
      -1,   154,   155,    -1,    -1,    -1,    -1,    -1,   161,   162,
     163,   164,   165,   166,   167,    -1,    -1,    -1,    -1,    -1,
     100,   101,    -1,   103,    -1,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,    -1,   119,
     120,   121,    49,   123,   124,    52,    -1,    54,    -1,    56,
      -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,
      -1,   151,    -1,    -1,   154,   155,    -1,    -1,    -1,    -1,
      -1,   161,   162,   163,   164,   165,   166,   167,    -1,    -1,
      -1,    -1,    -1,   100,   101,    -1,   103,    -1,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,    -1,   119,   120,   121,    49,   123,   124,    52,    -1,
      54,    -1,    56,    -1,   131,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,   151,    -1,    -1,   154,   155,    -1,
      -1,    -1,    -1,    -1,   161,   162,   163,   164,   165,   166,
     167,    -1,    -1,    -1,    -1,    -1,   100,   101,    -1,   103,
      -1,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,    -1,   119,   120,   121,    49,   123,
     124,    52,    -1,    54,    -1,    56,    -1,   131,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    -1,    -1,    -1,    -1,   151,    -1,    -1,
     154,   155,    -1,    -1,    -1,    -1,    -1,   161,   162,   163,
     164,   165,   166,   167,    -1,    -1,    -1,    -1,    -1,   100,
     101,    -1,   103,    -1,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,    -1,   119,   120,
     121,    49,   123,   124,    52,    -1,    54,    -1,    56,    -1,
     131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
     151,    -1,    -1,   154,   155,    -1,    -1,    -1,    -1,    -1,
     161,   162,   163,   164,   165,   166,   167,    -1,    -1,    -1,
      -1,    -1,   100,   101,    -1,   103,    -1,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
      -1,   119,   120,   121,    -1,   123,   124,    -1,    -1,    -1,
      -1,    -1,    -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   151,    -1,    -1,   154,   155,    -1,    -1,
      -1,    -1,    -1,   161,   162,   163,   164,   165,   166,   167,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    -1,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    -1,    -1,    20,    72,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,   103,
      53,    -1,   106,   107,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     103,    -1,    -1,   106,   107,    13,    14,    15,    16,    17,
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
       0,   178,   389,   390,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      20,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    51,    53,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    69,    72,    73,
      98,   102,   103,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   118,   131,   151,   152,   154,   155,   162,   163,
     181,   182,   183,   198,   281,   282,   283,   284,   285,   286,
     287,   288,   289,   290,   291,   292,   294,   296,   298,   299,
     300,   301,   302,   303,   304,   305,   306,   308,   310,   311,
     312,   314,   315,   319,   320,   321,   322,   323,   325,   331,
     332,   333,   334,   345,   348,   381,   384,   394,   400,   402,
     408,   412,   417,   418,   419,   420,   421,   422,   423,   424,
     444,   462,   463,   464,   465,     0,   178,   103,   182,   198,
     285,   287,   296,   299,   311,   315,   320,   117,   151,    58,
      61,    62,    64,   151,   151,   406,   407,   408,   307,   308,
     106,   107,   182,   361,   382,   383,   361,   151,   394,   151,
     151,     4,   103,   106,   107,   300,   305,   306,   151,   198,
     407,   412,   418,   419,   420,   422,   423,   424,   106,   322,
     156,   178,   288,   296,   299,   417,   421,   461,   462,   465,
     466,   176,   179,   148,   159,   175,   219,   364,    89,   157,
     401,   361,   179,   179,   179,   176,   106,   107,   151,   198,
     293,   403,   412,   413,   414,   415,   416,   417,   421,   425,
     426,   427,   428,   429,   435,     3,    47,    48,    50,    55,
     313,     3,   155,   198,   287,   300,   304,   306,   316,   321,
     397,   417,   421,   465,   285,   287,   299,   311,   315,   320,
     398,   417,   421,    65,   305,   305,   300,   306,   305,   300,
     305,   300,   154,   406,   157,   179,   151,   159,   227,   406,
     406,   178,   276,   277,   155,   296,   299,   463,   361,   361,
     394,   175,   299,   151,   198,   403,   412,   417,   426,   155,
     198,   465,   395,   396,    65,    66,    67,    68,   155,   173,
     361,   370,   372,   376,   378,   379,   321,    57,   153,   155,
     198,   295,   299,   303,   304,   310,   311,   317,   318,   319,
     320,   324,   331,   332,   348,   357,   359,   444,   457,   458,
     459,   460,   465,   466,   106,   107,   159,   182,   321,   435,
     408,   151,   377,   378,   151,   151,   117,   184,   185,    49,
      52,    54,    56,    73,   100,   101,   103,   105,   115,   116,
     119,   120,   121,   123,   124,   151,   155,   161,   164,   165,
     166,   167,   180,   181,   184,   186,   189,   197,   198,   199,
     200,   203,   204,   205,   206,   207,   208,   209,   210,   211,
     212,   213,   214,   215,   221,   321,   153,   155,   197,   198,
     214,   216,   296,   321,   362,   363,   380,   461,   466,   299,
     418,   419,   420,   422,   423,   424,   153,   153,   153,   153,
     153,   153,   153,   155,   296,   444,   463,   155,   162,   198,
     216,   287,   288,   295,   297,   299,   311,   318,   320,   352,
     353,   356,   357,   358,   457,   465,   151,   417,   421,   465,
     151,   157,   103,   154,   155,   159,   181,   183,   216,   365,
     366,   367,   368,   369,    22,   365,   151,   361,   227,   151,
     157,   157,   157,   407,   412,   414,   415,   416,   425,   427,
     428,   429,   299,   413,   426,   157,    98,   405,   155,   406,
     443,   444,   406,   406,   401,   276,   151,   406,   443,   401,
     406,   406,   299,   403,   151,   151,   298,   299,   296,   299,
     178,   296,   461,   466,   323,   159,   401,   276,   361,   364,
     287,   304,   399,   417,   421,   159,   401,   276,   382,   299,
     311,   299,   299,   106,   322,   106,   107,   182,   321,   326,
     382,   178,   182,   360,   150,   178,     3,   292,   294,   299,
     303,   227,   178,   178,   405,   151,   405,   179,   216,   407,
     412,   299,   151,   178,   361,   392,   159,   361,   159,   361,
     133,   162,   163,   375,   153,   157,   361,   379,   153,   406,
     406,   156,   178,   297,   299,   311,   318,   320,   456,   457,
     465,   466,   151,   155,   163,   175,   198,   444,   446,   447,
     448,   449,   450,   451,   468,   198,   324,   465,   299,   318,
     305,   300,   406,   153,   297,   299,   458,   297,   444,   458,
      10,   349,   361,   346,   159,   370,   175,   370,    13,    88,
     103,   106,   107,   181,   409,   410,   411,   153,   117,   151,
     197,   151,   151,   151,   200,   151,   197,   151,   103,   106,
     107,   300,   305,   306,   151,   197,   197,    19,    21,    85,
     155,   164,   165,   201,   202,   216,   223,   227,   334,   362,
     465,   157,   178,   151,   186,   155,   160,   155,   160,   120,
     122,   123,   124,   151,   154,   155,   159,   160,   200,   200,
     168,   162,   169,   170,   164,   165,   125,   126,   127,   128,
     171,   172,   129,   130,   163,   161,   173,   131,   132,   174,
     153,   157,   154,   178,   134,   135,   136,   137,   138,   139,
     140,   141,   142,   143,   144,   175,   218,   219,   220,   151,
     198,   439,   440,   441,   442,   443,   153,   157,   153,   153,
     153,   153,   153,   153,   151,   406,   443,   444,   151,   443,
     444,   178,   296,   463,   178,   179,   179,   151,   163,   198,
     412,   430,   431,   432,   433,   434,   435,   436,   437,   438,
     133,   465,   179,   179,   361,   361,   178,   178,   178,   155,
     183,   178,   366,   158,   157,   467,   365,   154,   155,   158,
     369,   152,   216,   222,   151,   178,   178,   178,   178,   412,
     414,   415,   416,   425,   427,   428,   429,   153,   153,   153,
     153,   153,   153,   153,   413,   426,   406,   151,   364,   156,
     178,   227,   401,   178,   227,   403,   223,   363,   223,   363,
     403,   392,   227,   401,   405,   159,   401,   276,   392,   227,
     401,   328,   329,   327,   159,   133,   299,   354,   355,   358,
     359,   153,   157,    70,   278,   279,   179,   299,   292,   162,
     216,   178,   412,   353,   394,   392,   156,   178,   151,   374,
     372,   373,    78,   309,   182,   159,   182,   435,   297,   444,
     458,   299,   303,   465,   178,   447,   448,   449,   156,   178,
      18,   216,   299,   446,   468,   406,   406,   444,   297,   456,
     466,   299,   182,   406,   297,   458,   321,   157,   467,   175,
     350,   159,   349,   153,   363,   153,   153,   157,   151,   176,
     362,   186,   155,   362,   362,   362,   216,   362,   153,   362,
     362,   362,   178,   153,   164,   165,   202,    18,   301,   153,
     157,   153,   162,   163,   153,   222,   216,   159,   216,   182,
     216,   182,   115,   155,   182,   152,   190,   191,   192,   216,
     115,   155,   182,   334,   216,   190,   182,   200,   203,   203,
     203,   204,   204,   205,   205,   206,   206,   206,   206,   207,
     207,   208,   209,   210,   211,   212,   158,   223,   176,   184,
     155,   182,   216,   159,   216,   178,   440,   441,   442,   299,
     439,   406,   406,   216,   363,   151,   406,   443,   444,   151,
     443,   444,   178,   178,   156,   156,   151,   412,   431,   432,
     433,   436,    18,   299,   430,   434,   151,   406,   450,   468,
     406,   406,   468,   151,   406,   450,   406,   406,   179,   215,
     361,   156,   157,   156,   157,   468,   468,   133,   351,   352,
     353,   351,   361,   178,   214,   215,   216,   404,   467,   365,
     367,   150,   178,   153,   157,   178,   351,   182,   403,   182,
     153,   153,   153,   153,   153,   153,   151,   406,   443,   444,
     151,   406,   443,   444,   403,   184,   444,   216,   227,   354,
     153,   153,   153,   153,   390,   391,   227,   392,   227,   401,
     391,   227,   159,   159,   159,   335,   179,   179,   182,   280,
     361,    18,    71,    73,    76,    79,    80,    81,    82,    83,
      84,    85,    86,    87,    88,    89,    90,    92,    93,    94,
      95,    96,    98,   106,   107,   118,   178,   223,   224,   225,
     226,   227,   228,   229,   231,   232,   242,   248,   249,   250,
     251,   252,   253,   258,   259,   265,   266,   267,   281,   299,
     303,   361,   402,    70,   176,   179,   179,   179,   351,   179,
     393,   391,   285,   287,   296,   385,   386,   387,   388,   380,
     175,   371,   371,   349,   406,   406,   297,   458,   155,   162,
     198,   216,   321,   216,   299,   354,   153,   153,   153,     5,
     299,   406,   446,   159,   182,   435,    10,   361,   150,   159,
     215,   349,   467,   159,   153,   410,   190,   153,   157,   178,
     157,   153,   153,   157,   153,   200,   153,   153,   153,   200,
      18,   301,   216,   153,   153,   152,   159,   200,   156,   179,
     190,   156,   156,   115,   119,   121,   183,   193,   194,   195,
     153,   157,   193,   156,   157,   150,   214,   158,   153,   193,
     179,   366,   354,   153,   153,   153,   439,   178,   178,   354,
     354,   436,   153,   153,   153,   153,   151,   412,   435,   430,
     434,   178,   178,   156,   179,   468,   178,   178,   179,   179,
     179,   179,   364,   193,   133,   167,   179,   179,   150,   365,
     216,   406,   152,   216,   351,   179,   175,   151,   406,   443,
     444,   151,   406,   443,   444,   178,   178,   405,   153,   179,
     179,   393,   391,   227,   393,   335,   335,   335,     3,    10,
      73,   150,   282,   289,   290,   296,   299,   336,   341,   461,
     153,   157,   157,   176,   151,    61,    62,   176,   227,   281,
     402,   151,    18,   225,   151,   151,   176,   361,   176,   361,
     162,   361,   159,   224,   151,   151,   151,   227,   216,   217,
     217,    14,   268,    74,   233,   176,   179,   229,    78,   176,
     361,    91,   254,   360,   299,   158,   280,   176,   156,   156,
     179,   157,   393,   403,   179,   176,   179,   176,   179,   153,
     363,   377,   377,   467,   159,   159,   178,   179,   179,   179,
     216,   179,   151,   406,   450,   444,   298,     5,   162,   179,
     216,   349,   406,   406,   321,   350,   366,   467,   150,   150,
     178,   153,   295,   182,    78,   187,   188,   362,   200,   200,
     200,   200,   200,   159,   366,   157,   150,   196,   155,   194,
     196,   196,   156,   157,   122,   154,   192,   156,   222,   214,
     176,   156,   467,   179,   151,   406,   443,   444,   354,   354,
     179,   179,   153,   151,   406,   443,   444,   151,   406,   450,
     412,   406,   406,   354,   354,   156,   353,   356,   356,   357,
     153,   157,   157,   153,   179,   215,   215,   156,   156,   179,
     179,   153,   216,   178,   178,   354,   354,   364,   406,   157,
     153,   150,   393,   150,   150,   150,   150,   296,   334,   342,
     461,   296,   341,   151,   330,   176,   176,   151,   158,   198,
     337,   338,   344,   412,   413,   426,   157,   176,   361,   178,
     361,   153,   190,   191,   176,   227,   176,   227,   223,    80,
     153,   223,   234,   281,   283,   286,   292,   299,   303,   145,
     146,   147,   152,   153,   176,   223,   243,   244,   245,   281,
     176,   176,   223,   176,   366,   176,   223,   222,   223,   110,
     111,   112,   113,   114,   260,   262,   263,   176,    97,   176,
      84,   151,   151,   179,   150,   176,   176,   151,   225,   227,
     406,   176,   153,   178,   150,   150,   178,   157,   157,   150,
     349,   349,   156,   156,   156,   179,   153,   178,   216,   216,
     179,   156,   179,   467,   347,   159,   350,   467,   150,   385,
     445,   446,   153,   158,   153,   157,   158,   366,   467,   222,
     120,   193,   194,   155,   194,   155,   194,   156,   150,   153,
     178,   179,   179,   153,   153,   178,   178,   179,   179,   179,
     178,   178,   156,   179,   153,   406,   354,   354,   179,   179,
     223,   150,   330,   330,   330,   151,   198,   339,   340,   443,
     452,   453,   454,   455,   176,   157,   176,   337,   176,   380,
     407,   412,   216,   299,   157,   176,   343,   344,   343,   361,
     133,   358,   359,   223,   153,   153,   151,   225,   153,   223,
     299,   145,   146,   147,   167,   176,   246,   247,   225,   224,
     176,   247,   153,   158,   223,   152,   223,   224,   245,   176,
     467,   153,   153,   153,   227,   262,   263,   151,   216,   151,
     184,   234,   200,   255,   109,     1,   225,   406,   386,   178,
     178,   467,   467,   156,   354,   179,   179,   156,   156,   150,
     159,   349,   150,   179,   153,   216,   188,   216,   467,   150,
     156,   156,   193,   193,   354,   153,   153,   354,   354,   153,
     153,   156,   157,   133,   353,   133,   156,   179,   179,   153,
     153,   156,   453,   454,   455,   299,   452,   157,   176,   406,
     406,   176,   153,   412,   406,   176,   225,    77,    78,   159,
     237,   238,   239,   153,   223,    75,   225,   223,   152,   223,
      75,   176,   106,   152,   223,   224,   245,   152,   223,   225,
     244,   247,   247,   176,   223,   150,   159,   239,   225,   151,
     178,   176,   184,   153,   158,   153,   153,   157,   158,   253,
     257,   361,   403,   150,   150,   179,   156,   156,   349,   467,
     150,   150,   156,   156,   179,   179,   179,   178,   179,   153,
     153,   153,   153,   153,   452,   406,   338,     1,   215,   235,
     236,   404,     1,   158,     1,   178,   225,   237,    75,   176,
     153,   225,    75,   176,   167,   167,   225,   224,   247,   247,
     176,   106,   223,   167,   167,    75,   152,   223,   152,   223,
     224,   176,     1,   178,   178,   264,   297,   299,   461,   158,
     176,   155,   184,   269,   270,   271,   225,   200,   190,    75,
     108,   254,   256,   153,   467,   150,   153,   153,   153,   356,
     151,   406,   443,   444,   340,   133,     1,   157,   158,   150,
     274,   275,   281,   225,    75,   176,   225,   223,   152,   152,
     223,   152,   223,   152,   223,   224,   152,   223,   152,   223,
     225,   167,   167,   167,   167,   150,   274,   264,   179,   151,
     198,   403,   452,   182,   158,   103,   151,   153,   158,   157,
      75,   153,   225,   151,   225,   225,   150,   178,   215,   235,
     238,   240,   241,   281,   225,   167,   167,   167,   167,   152,
     152,   223,   152,   223,   152,   223,   240,   179,   176,   261,
     299,   269,   156,   215,   176,   269,   271,   225,   223,   109,
     109,   354,   225,   230,   179,   238,   152,   152,   223,   152,
     223,   152,   223,   179,   261,   214,   153,   158,   184,   153,
     153,   158,   153,   257,    75,   252,   179,     1,   225,   150,
     230,   150,   153,   227,   184,   272,   151,   176,   272,   225,
      75,   153,   227,   157,   158,   215,   153,   225,   184,   182,
     273,   153,   176,   153,   157,   176,   182
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_int16 yyr1[] =
{
       0,   177,   178,   179,   180,   180,   180,   180,   180,   181,
     181,   181,   181,   181,   181,   181,   182,   182,   183,   183,
     184,   185,   185,   186,   186,   186,   186,   186,   186,   186,
     186,   186,   186,   186,   186,   186,   186,   186,   187,   187,
     188,   188,   189,   189,   189,   189,   189,   189,   189,   189,
     189,   189,   189,   189,   189,   189,   189,   189,   189,   189,
     189,   189,   189,   189,   189,   189,   190,   190,   191,   191,
     192,   192,   193,   193,   194,   194,   194,   194,   194,   194,
     194,   195,   195,   195,   196,   196,   197,   197,   197,   197,
     197,   197,   197,   197,   197,   197,   197,   197,   197,   197,
     198,   198,   198,   199,   199,   199,   199,   200,   200,   200,
     200,   200,   200,   200,   200,   200,   201,   201,   201,   201,
     202,   202,   203,   203,   204,   204,   204,   204,   205,   205,
     205,   206,   206,   206,   207,   207,   207,   207,   207,   208,
     208,   208,   209,   209,   210,   210,   211,   211,   212,   212,
     213,   213,   214,   214,   214,   215,   216,   216,   216,   217,
     217,   218,   218,   219,   219,   220,   220,   220,   220,   220,
     220,   220,   220,   220,   220,   220,   221,   221,   222,   222,
     222,   222,   223,   223,   224,   224,   225,   225,   225,   225,
     225,   225,   225,   225,   225,   225,   225,   225,   225,   226,
     226,   227,   227,   228,   228,   229,   229,   229,   229,   229,
     230,   230,   230,   231,   231,   232,   232,   232,   232,   232,
     232,   232,   233,   233,   234,   234,   234,   234,   235,   235,
     235,   236,   236,   237,   237,   237,   237,   237,   238,   238,
     239,   240,   240,   241,   241,   242,   242,   242,   242,   242,
     242,   242,   242,   242,   242,   242,   242,   243,   243,   244,
     244,   244,   244,   244,   244,   244,   244,   244,   244,   244,
     244,   244,   244,   244,   244,   244,   244,   244,   244,   244,
     244,   244,   244,   244,   244,   244,   244,   244,   244,   244,
     244,   244,   244,   244,   244,   244,   244,   244,   244,   244,
     244,   244,   244,   244,   245,   245,   245,   246,   246,   247,
     247,   247,   248,   248,   248,   248,   248,   248,   248,   248,
     248,   248,   248,   248,   248,   248,   248,   248,   248,   248,
     248,   248,   249,   249,   250,   251,   252,   253,   253,   254,
     254,   255,   255,   256,   257,   257,   257,   257,   257,   257,
     258,   258,   259,   259,   259,   260,   260,   261,   261,   262,
     262,   262,   262,   263,   264,   264,   264,   264,   264,   265,
     266,   266,   267,   267,   267,   267,   267,   268,   268,   269,
     269,   270,   270,   271,   271,   272,   272,   272,   273,   273,
     274,   274,   275,   275,   276,   276,   277,   277,   278,   278,
     279,   279,   280,   280,   281,   281,   281,   282,   282,   283,
     283,   283,   283,   283,   284,   284,   284,   285,   285,   285,
     286,   286,   286,   286,   286,   287,   287,   288,   288,   289,
     289,   289,   290,   290,   290,   290,   290,   291,   291,   292,
     292,   292,   292,   293,   293,   294,   294,   294,   295,   295,
     295,   296,   296,   296,   297,   297,   297,   298,   298,   299,
     299,   300,   300,   301,   301,   301,   301,   301,   302,   303,
     303,   303,   304,   304,   305,   305,   305,   305,   305,   305,
     305,   305,   305,   306,   306,   306,   306,   306,   306,   306,
     306,   306,   306,   306,   306,   306,   306,   306,   306,   306,
     306,   306,   306,   306,   306,   306,   306,   306,   306,   306,
     306,   307,   307,   308,   309,   309,   310,   310,   310,   310,
     310,   311,   311,   312,   312,   312,   312,   313,   313,   313,
     313,   313,   313,   314,   314,   314,   314,   315,   316,   315,
     315,   317,   317,   317,   317,   318,   318,   318,   319,   319,
     319,   319,   320,   320,   320,   321,   321,   321,   321,   321,
     321,   322,   322,   322,   323,   323,   324,   324,   326,   325,
     327,   325,   328,   325,   329,   325,   325,   330,   330,   331,
     331,   332,   332,   333,   333,   333,   334,   334,   334,   334,
     334,   334,   334,   334,   335,   335,   336,   336,   336,   336,
     336,   336,   336,   336,   336,   336,   337,   337,   337,   338,
     338,   338,   339,   339,   339,   340,   341,   341,   342,   342,
     343,   343,   344,   345,   346,   345,   345,   345,   345,   347,
     345,   345,   345,   345,   345,   348,   348,   349,   349,   349,
     349,   350,   350,   350,   351,   351,   351,   351,   351,   351,
     351,   352,   352,   352,   352,   353,   353,   354,   354,   354,
     354,   355,   355,   355,   355,   356,   356,   356,   356,   356,
     357,   357,   357,   357,   357,   358,   358,   359,   359,   360,
     360,   361,   361,   361,   362,   362,   362,   363,   363,   364,
     364,   364,   364,   365,   365,   366,   366,   366,   366,   366,
     367,   367,   368,   368,   369,   369,   369,   369,   369,   370,
     370,   371,   371,   373,   372,   374,   372,   372,   372,   375,
     375,   375,   375,   376,   376,   376,   376,   377,   377,   378,
     378,   379,   379,   380,   380,   380,   380,   381,   381,   381,
     382,   382,   383,   383,   384,   384,   385,   385,   386,   386,
     387,   387,   387,   388,   388,   389,   389,   390,   390,   391,
     391,   392,   393,   394,   394,   394,   394,   394,   394,   394,
     394,   394,   394,   394,   395,   394,   396,   394,   397,   394,
     398,   394,   399,   394,   400,   400,   400,   401,   401,   402,
     402,   402,   402,   402,   402,   402,   402,   402,   402,   403,
     403,   403,   404,   405,   405,   406,   406,   407,   407,   408,
     409,   409,   410,   410,   410,   411,   411,   411,   411,   411,
     411,   412,   412,   413,   413,   413,   413,   414,   414,   414,
     414,   415,   415,   415,   415,   415,   415,   415,   416,   416,
     416,   416,   417,   417,   417,   418,   418,   418,   418,   418,
     419,   419,   419,   419,   420,   420,   420,   420,   420,   420,
     421,   421,   421,   422,   422,   422,   422,   422,   423,   423,
     423,   423,   424,   424,   424,   424,   424,   424,   425,   425,
     426,   426,   426,   426,   427,   427,   427,   427,   428,   428,
     428,   428,   428,   428,   428,   429,   429,   429,   429,   429,
     430,   430,   430,   430,   430,   431,   431,   431,   432,   432,
     432,   432,   433,   433,   433,   434,   434,   434,   434,   434,
     435,   435,   436,   436,   436,   437,   437,   438,   438,   439,
     439,   439,   440,   440,   440,   440,   440,   441,   441,   441,
     441,   442,   442,   442,   443,   443,   443,   443,   444,   444,
     444,   444,   445,   445,   446,   446,   446,   446,   447,   447,
     447,   447,   447,   448,   448,   448,   448,   449,   449,   449,
     450,   450,   450,   451,   451,   451,   451,   451,   451,   452,
     452,   452,   453,   453,   453,   453,   453,   454,   454,   454,
     454,   455,   455,   456,   456,   456,   457,   457,   458,   458,
     458,   458,   458,   458,   459,   459,   459,   459,   459,   459,
     459,   459,   459,   459,   460,   460,   460,   460,   461,   461,
     461,   462,   462,   463,   463,   463,   463,   463,   463,   464,
     464,   464,   464,   464,   464,   465,   465,   465,   466,   466,
     467,   467,   468,   468
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     0,     0,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     2,     1,     1,     1,     1,     3,     3,     3,
       5,     6,     2,     2,     2,     2,     2,     2,     1,     3,
       3,     3,     1,     6,     4,     4,     4,     4,     4,     7,
       3,     3,     3,     3,     3,     2,     5,     3,     3,     3,
       5,     2,     2,     7,     8,     5,     0,     1,     1,     3,
       1,     1,     1,     3,     1,     2,     4,     3,     5,     3,
       5,     2,     2,     2,     0,     2,     1,     1,     1,     2,
       2,     2,     2,     2,     2,     4,     2,     4,     6,     4,
       1,     1,     1,     1,     1,     1,     1,     1,     4,     5,
       5,     4,     5,     5,     5,     4,     2,     2,     3,     3,
       1,     1,     1,     3,     1,     3,     3,     3,     1,     3,
       3,     1,     3,     3,     1,     3,     3,     3,     3,     1,
       3,     3,     1,     3,     1,     3,     1,     3,     1,     3,
       1,     3,     1,     5,     4,     1,     1,     3,     6,     0,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     4,     7,     1,     1,
       3,     3,     1,     3,     0,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     4,
       4,     2,     6,     1,     2,     1,     2,     1,     2,     1,
       1,     2,     2,     2,     5,     3,     5,    10,     7,     5,
      10,     7,     5,     7,     1,     1,     1,     2,     1,     3,
       1,     1,     3,     2,     3,     3,     2,     2,     1,     2,
       2,     0,     1,     2,     3,     4,     6,     5,     7,     6,
       7,     7,     8,     4,     6,     5,     7,     1,     3,     4,
       5,     4,     3,     5,     1,     2,     3,     3,     3,     5,
       5,     5,     5,     3,     5,     5,     5,     3,     4,     5,
       5,     5,     5,     7,     7,     7,     7,     7,     7,     7,
       2,     3,     4,     4,     4,     6,     6,     6,     6,     6,
       6,     6,     3,     4,     1,     1,     1,     1,     1,     1,
       1,     1,     3,     4,     2,     3,     3,     2,     3,     2,
       3,     3,     6,     2,     2,     3,     3,     3,     3,     3,
       3,     5,     1,     1,     5,     5,     4,     0,     1,     4,
       6,     1,     3,     4,     3,     5,     3,     3,     6,     7,
       3,     5,     3,     3,     4,     8,     9,     0,     2,     1,
       1,     1,     1,     2,     1,     2,     2,     2,     1,     3,
       1,     1,     6,     8,    10,    12,    14,     0,     1,     0,
       1,     1,     3,     4,     7,     0,     1,     3,     1,     3,
       0,     1,     1,     2,     0,     1,     4,     5,     0,     1,
       3,     4,     1,     3,     2,     2,     1,     7,     5,     1,
       1,     1,     1,     1,     2,     3,     6,     3,     3,     4,
       1,     2,     2,     3,     8,     8,     8,     5,     9,     2,
       2,     5,     3,     5,     4,     3,     4,     4,     7,     2,
       1,     1,     1,     3,     6,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     0,     1,     1,
       2,     1,     1,     1,     1,     1,     1,     1,     4,     1,
       2,     3,     1,     2,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     0,     1,     5,     0,     1,     1,     2,     2,     3,
       3,     1,     3,     1,     2,     2,     2,     4,     4,     4,
       4,     1,     1,     1,     2,     2,     3,     1,     0,     3,
       2,     1,     2,     2,     3,     1,     2,     2,     1,     2,
       2,     3,     1,     2,     2,     1,     2,     3,     1,     2,
       3,     1,     3,     4,     1,     1,     1,     1,     0,     7,
       0,     8,     0,     8,     0,     8,     1,     0,     3,     3,
       3,     1,     1,     2,     1,     1,     1,     2,     1,     2,
       1,     2,     1,     2,     0,     2,     3,     4,     4,     3,
       2,     2,     3,     3,     2,     1,     0,     1,     4,     1,
       2,     2,     0,     1,     4,     1,     2,     3,     1,     2,
       0,     1,     2,     6,     0,     8,     7,     9,     8,     0,
      12,    10,    11,    10,     1,     3,     3,     2,     2,     4,
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
       1,     0,     0,     1,     1,     2,     2,     2,     2,     2,
       2,     1,     2,     5,     0,     6,     0,     8,     0,     7,
       0,     7,     0,     8,     1,     2,     3,     0,     5,     3,
       4,     4,     4,     4,     5,     5,     5,     5,     6,     1,
       1,     1,     3,     0,     5,     0,     1,     1,     2,     6,
       1,     3,     0,     1,     4,     1,     1,     1,     1,     1,
       1,     1,     3,     2,     1,     2,     2,     2,     3,     4,
       5,     2,     4,     5,     4,     5,     3,     4,     8,     9,
       3,     4,     2,     1,     2,     6,     8,     9,     3,     4,
       2,     3,     4,     5,     4,     5,     4,     5,     3,     4,
       1,     1,     1,     4,     8,     9,     3,     4,     2,     3,
       3,     4,     4,     5,     4,     5,     3,     4,     1,     3,
       2,     1,     2,     2,     2,     3,     4,     5,     2,     4,
       5,     4,     5,     3,     4,     6,     8,     9,     3,     4,
       2,     4,     1,     2,     2,     2,     3,     4,     2,     4,
       4,     3,     6,     8,     3,     2,     4,     1,     2,     2,
       1,     1,     2,     3,     4,     2,     4,     6,     8,     1,
       2,     2,     1,     2,     2,     3,     4,     1,     4,     4,
       3,     5,     8,     3,     2,     3,     7,     1,     5,     5,
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
#line 579 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.enterScope(); }
#line 7641 "Parser/parser.cc"
    break;

  case 3:
#line 583 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 7647 "Parser/parser.cc"
    break;

  case 4:
#line 590 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantInteger( *(yyvsp[0].tok) ) ); }
#line 7653 "Parser/parser.cc"
    break;

  case 5:
#line 591 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7659 "Parser/parser.cc"
    break;

  case 6:
#line 592 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7665 "Parser/parser.cc"
    break;

  case 7:
#line 593 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7671 "Parser/parser.cc"
    break;

  case 8:
#line 594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantChar( *(yyvsp[0].tok) ) ); }
#line 7677 "Parser/parser.cc"
    break;

  case 19:
#line 615 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 7683 "Parser/parser.cc"
    break;

  case 20:
#line 619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.constant) = build_constantStr( *(yyvsp[0].str) ); }
#line 7689 "Parser/parser.cc"
    break;

  case 21:
#line 623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 7695 "Parser/parser.cc"
    break;

  case 22:
#line 625 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 7705 "Parser/parser.cc"
    break;

  case 23:
#line 636 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 7711 "Parser/parser.cc"
    break;

  case 24:
#line 638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 7717 "Parser/parser.cc"
    break;

  case 25:
#line 642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_dimensionref( (yyvsp[0].tok) ) ); }
#line 7723 "Parser/parser.cc"
    break;

  case 27:
#line 645 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 7729 "Parser/parser.cc"
    break;

  case 28:
#line 647 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new StmtExpr( dynamic_cast<CompoundStmt *>(maybeMoveBuild<Statement>((yyvsp[-1].sn)) ) ) ); }
#line 7735 "Parser/parser.cc"
    break;

  case 29:
#line 649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_qualified_expr( (yyvsp[-2].decl), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7741 "Parser/parser.cc"
    break;

  case 30:
#line 651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7747 "Parser/parser.cc"
    break;

  case 31:
#line 653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild<Expression>( (yyvsp[-3].en) );
			(yyval.en) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 7757 "Parser/parser.cc"
    break;

  case 32:
#line 663 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, "n expression" ); (yyval.en) = nullptr; }
#line 7763 "Parser/parser.cc"
    break;

  case 33:
#line 665 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.en) = nullptr; }
#line 7769 "Parser/parser.cc"
    break;

  case 34:
#line 667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.en) = nullptr; }
#line 7775 "Parser/parser.cc"
    break;

  case 35:
#line 669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.en) = nullptr; }
#line 7781 "Parser/parser.cc"
    break;

  case 36:
#line 671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.en) = nullptr; }
#line 7787 "Parser/parser.cc"
    break;

  case 37:
#line 673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.en) = nullptr; }
#line 7793 "Parser/parser.cc"
    break;

  case 39:
#line 679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// steal the association node from the singleton and delete the wrapper
			(yyvsp[-2].genexpr)->associations.splice((yyvsp[-2].genexpr)->associations.end(), (yyvsp[0].genexpr)->associations);
			delete (yyvsp[0].genexpr);
			(yyval.genexpr) = (yyvsp[-2].genexpr);
		}
#line 7804 "Parser/parser.cc"
    break;

  case 40:
#line 689 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuildType((yyvsp[-2].decl)), maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } );
		}
#line 7813 "Parser/parser.cc"
    break;

  case 41:
#line 694 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } ); }
#line 7819 "Parser/parser.cc"
    break;

  case 43:
#line 703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-5].en), new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) ) ) )) ) ); }
#line 7825 "Parser/parser.cc"
    break;

  case 44:
#line 709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7831 "Parser/parser.cc"
    break;

  case 45:
#line 711 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7837 "Parser/parser.cc"
    break;

  case 46:
#line 713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, new ExpressionNode( (yyvsp[-3].constant) ), (yyvsp[-1].en) ) ); }
#line 7843 "Parser/parser.cc"
    break;

  case 47:
#line 715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.en) = new ExpressionNode( new ConstructorExpr( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) ) );
		}
#line 7853 "Parser/parser.cc"
    break;

  case 48:
#line 721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7859 "Parser/parser.cc"
    break;

  case 49:
#line 724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( new string( "__builtin_va_arg") ) ),
											   (ExpressionNode *)((yyvsp[-4].en)->set_last( (ExpressionNode *)((yyvsp[-1].decl) ? (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) ) : (yyvsp[-2].decl)) )) ) ); }
#line 7866 "Parser/parser.cc"
    break;

  case 50:
#line 727 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 7872 "Parser/parser.cc"
    break;

  case 51:
#line 729 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 7878 "Parser/parser.cc"
    break;

  case 52:
#line 731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), new ExpressionNode( (yyvsp[-2].constant) ) ) ); }
#line 7884 "Parser/parser.cc"
    break;

  case 53:
#line 733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7890 "Parser/parser.cc"
    break;

  case 54:
#line 735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 7896 "Parser/parser.cc"
    break;

  case 55:
#line 737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-1].en), build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) ) ) ); }
#line 7902 "Parser/parser.cc"
    break;

  case 56:
#line 739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7908 "Parser/parser.cc"
    break;

  case 57:
#line 741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[0].aggKey), (yyvsp[-2].en) ) ); }
#line 7914 "Parser/parser.cc"
    break;

  case 58:
#line 743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7920 "Parser/parser.cc"
    break;

  case 59:
#line 745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 7926 "Parser/parser.cc"
    break;

  case 60:
#line 747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7932 "Parser/parser.cc"
    break;

  case 61:
#line 749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::IncrPost, (yyvsp[-1].en) ) ); }
#line 7938 "Parser/parser.cc"
    break;

  case 62:
#line 751 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::DecrPost, (yyvsp[-1].en) ) ); }
#line 7944 "Parser/parser.cc"
    break;

  case 63:
#line 753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].in), true ) ) ); }
#line 7950 "Parser/parser.cc"
    break;

  case 64:
#line 755 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].in), true ))->set_maybeConstructed( false ) ) ); }
#line 7956 "Parser/parser.cc"
    break;

  case 65:
#line 757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) );
		}
#line 7966 "Parser/parser.cc"
    break;

  case 66:
#line 766 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 7972 "Parser/parser.cc"
    break;

  case 69:
#line 773 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7978 "Parser/parser.cc"
    break;

  case 70:
#line 778 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Default parameter for argument is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7984 "Parser/parser.cc"
    break;

  case 73:
#line 785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 7990 "Parser/parser.cc"
    break;

  case 75:
#line 791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-1].tok) ) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 7996 "Parser/parser.cc"
    break;

  case 76:
#line 793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-3].tok) ) ), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 8002 "Parser/parser.cc"
    break;

  case 77:
#line 795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 8008 "Parser/parser.cc"
    break;

  case 78:
#line 797 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 8014 "Parser/parser.cc"
    break;

  case 79:
#line 799 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 8020 "Parser/parser.cc"
    break;

  case 80:
#line 801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 8026 "Parser/parser.cc"
    break;

  case 81:
#line 806 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_constantInteger( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 8032 "Parser/parser.cc"
    break;

  case 82:
#line 808 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_field_name_FLOATINGconstant( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 8038 "Parser/parser.cc"
    break;

  case 83:
#line 810 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_varref( (yyvsp[-1].tok) ), (yyvsp[0].en) ) );
		}
#line 8046 "Parser/parser.cc"
    break;

  case 84:
#line 817 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8052 "Parser/parser.cc"
    break;

  case 85:
#line 819 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Expression * constant = build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) );
			(yyval.en) = (yyvsp[-1].en) != nullptr ? new ExpressionNode( build_fieldSel( (yyvsp[-1].en),  constant ) ) : new ExpressionNode( constant );
		}
#line 8061 "Parser/parser.cc"
    break;

  case 88:
#line 831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 8067 "Parser/parser.cc"
    break;

  case 89:
#line 833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en)->set_extension( true ); }
#line 8073 "Parser/parser.cc"
    break;

  case 90:
#line 838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 8093 "Parser/parser.cc"
    break;

  case 91:
#line 854 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_val( (yyvsp[-1].op), (yyvsp[0].en) ) ); }
#line 8099 "Parser/parser.cc"
    break;

  case 92:
#line 856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Incr, (yyvsp[0].en) ) ); }
#line 8105 "Parser/parser.cc"
    break;

  case 93:
#line 858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Decr, (yyvsp[0].en) ) ); }
#line 8111 "Parser/parser.cc"
    break;

  case 94:
#line 860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 8117 "Parser/parser.cc"
    break;

  case 95:
#line 862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8123 "Parser/parser.cc"
    break;

  case 96:
#line 864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 8129 "Parser/parser.cc"
    break;

  case 97:
#line 866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8135 "Parser/parser.cc"
    break;

  case 98:
#line 868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_offsetOf( (yyvsp[-3].decl), build_varref( (yyvsp[-1].tok) ) ) ); }
#line 8141 "Parser/parser.cc"
    break;

  case 99:
#line 870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.en) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 8150 "Parser/parser.cc"
    break;

  case 100:
#line 877 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::PointTo; }
#line 8156 "Parser/parser.cc"
    break;

  case 101:
#line 878 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AddressOf; }
#line 8162 "Parser/parser.cc"
    break;

  case 102:
#line 880 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::And; }
#line 8168 "Parser/parser.cc"
    break;

  case 103:
#line 884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::UnPlus; }
#line 8174 "Parser/parser.cc"
    break;

  case 104:
#line 885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::UnMinus; }
#line 8180 "Parser/parser.cc"
    break;

  case 105:
#line 886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::Neg; }
#line 8186 "Parser/parser.cc"
    break;

  case 106:
#line 887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::BitNeg; }
#line 8192 "Parser/parser.cc"
    break;

  case 108:
#line 893 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cast( (yyvsp[-2].decl), (yyvsp[0].en) ) ); }
#line 8198 "Parser/parser.cc"
    break;

  case 109:
#line 895 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 8204 "Parser/parser.cc"
    break;

  case 110:
#line 897 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 8210 "Parser/parser.cc"
    break;

  case 111:
#line 899 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( nullptr ) ) ); }
#line 8216 "Parser/parser.cc"
    break;

  case 112:
#line 901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 8222 "Parser/parser.cc"
    break;

  case 113:
#line 903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Return cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8228 "Parser/parser.cc"
    break;

  case 114:
#line 905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8234 "Parser/parser.cc"
    break;

  case 115:
#line 907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8240 "Parser/parser.cc"
    break;

  case 123:
#line 927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Exp, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8246 "Parser/parser.cc"
    break;

  case 125:
#line 933 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mul, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8252 "Parser/parser.cc"
    break;

  case 126:
#line 935 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Div, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8258 "Parser/parser.cc"
    break;

  case 127:
#line 937 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mod, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8264 "Parser/parser.cc"
    break;

  case 129:
#line 943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Plus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8270 "Parser/parser.cc"
    break;

  case 130:
#line 945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Minus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8276 "Parser/parser.cc"
    break;

  case 132:
#line 951 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8282 "Parser/parser.cc"
    break;

  case 133:
#line 953 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::RShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8288 "Parser/parser.cc"
    break;

  case 135:
#line 959 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8294 "Parser/parser.cc"
    break;

  case 136:
#line 961 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8300 "Parser/parser.cc"
    break;

  case 137:
#line 963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8306 "Parser/parser.cc"
    break;

  case 138:
#line 965 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8312 "Parser/parser.cc"
    break;

  case 140:
#line 971 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Eq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8318 "Parser/parser.cc"
    break;

  case 141:
#line 973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Neq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8324 "Parser/parser.cc"
    break;

  case 143:
#line 979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitAnd, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8330 "Parser/parser.cc"
    break;

  case 145:
#line 985 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Xor, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8336 "Parser/parser.cc"
    break;

  case 147:
#line 991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitOr, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8342 "Parser/parser.cc"
    break;

  case 149:
#line 997 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), true ) ); }
#line 8348 "Parser/parser.cc"
    break;

  case 151:
#line 1003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), false ) ); }
#line 8354 "Parser/parser.cc"
    break;

  case 153:
#line 1009 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8360 "Parser/parser.cc"
    break;

  case 154:
#line 1012 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-3].en), (yyvsp[-3].en), (yyvsp[0].en) ) ); }
#line 8366 "Parser/parser.cc"
    break;

  case 157:
#line 1023 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
//			if ( $2 == OperKinds::AtAssn ) {
//				SemanticError( yylloc, "C @= assignment is currently unimplemented." ); $$ = nullptr;
//			} else {
				(yyval.en) = new ExpressionNode( build_binary_val( (yyvsp[-1].op), (yyvsp[-2].en), (yyvsp[0].en) ) );
//			} // if
		}
#line 8378 "Parser/parser.cc"
    break;

  case 158:
#line 1031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8384 "Parser/parser.cc"
    break;

  case 159:
#line 1036 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8390 "Parser/parser.cc"
    break;

  case 163:
#line 1046 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::Assign; }
#line 8396 "Parser/parser.cc"
    break;

  case 164:
#line 1047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AtAssn; }
#line 8402 "Parser/parser.cc"
    break;

  case 165:
#line 1051 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ExpAssn; }
#line 8408 "Parser/parser.cc"
    break;

  case 166:
#line 1052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MulAssn; }
#line 8414 "Parser/parser.cc"
    break;

  case 167:
#line 1053 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::DivAssn; }
#line 8420 "Parser/parser.cc"
    break;

  case 168:
#line 1054 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ModAssn; }
#line 8426 "Parser/parser.cc"
    break;

  case 169:
#line 1055 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::PlusAssn; }
#line 8432 "Parser/parser.cc"
    break;

  case 170:
#line 1056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MinusAssn; }
#line 8438 "Parser/parser.cc"
    break;

  case 171:
#line 1057 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::LSAssn; }
#line 8444 "Parser/parser.cc"
    break;

  case 172:
#line 1058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::RSAssn; }
#line 8450 "Parser/parser.cc"
    break;

  case 173:
#line 1059 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AndAssn; }
#line 8456 "Parser/parser.cc"
    break;

  case 174:
#line 1060 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ERAssn; }
#line 8462 "Parser/parser.cc"
    break;

  case 175:
#line 1061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::OrAssn; }
#line 8468 "Parser/parser.cc"
    break;

  case 176:
#line 1072 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)(new ExpressionNode( nullptr ) )->set_last( (yyvsp[-1].en) ) ) ); }
#line 8474 "Parser/parser.cc"
    break;

  case 177:
#line 1074 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-4].en)->set_last( (yyvsp[-1].en) ) ) )); }
#line 8480 "Parser/parser.cc"
    break;

  case 179:
#line 1080 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8486 "Parser/parser.cc"
    break;

  case 180:
#line 1082 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 8492 "Parser/parser.cc"
    break;

  case 181:
#line 1084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8498 "Parser/parser.cc"
    break;

  case 183:
#line 1090 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new CommaExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 8504 "Parser/parser.cc"
    break;

  case 184:
#line 1095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8510 "Parser/parser.cc"
    break;

  case 196:
#line 1113 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 8516 "Parser/parser.cc"
    break;

  case 198:
#line 1116 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_directive( (yyvsp[0].tok) ) ); }
#line 8522 "Parser/parser.cc"
    break;

  case 199:
#line 1122 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[0].sn)->add_label( (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 8528 "Parser/parser.cc"
    break;

  case 200:
#line 1124 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "Label \"", *(yyvsp[-3].tok).str, "\" must be associated with a statement, "
											   "where a declaration, case, or default is not a statement. "
											   "Move the label or terminate with a semi-colon." ) );
			(yyval.sn) = nullptr;
		}
#line 8539 "Parser/parser.cc"
    break;

  case 201:
#line 1134 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (StatementNode *)0 ) ); }
#line 8545 "Parser/parser.cc"
    break;

  case 202:
#line 1139 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (yyvsp[-2].sn) ) ); }
#line 8551 "Parser/parser.cc"
    break;

  case 204:
#line 1145 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 8557 "Parser/parser.cc"
    break;

  case 205:
#line 1150 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8563 "Parser/parser.cc"
    break;

  case 206:
#line 1152 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8569 "Parser/parser.cc"
    break;

  case 207:
#line 1154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8575 "Parser/parser.cc"
    break;

  case 208:
#line 1156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8581 "Parser/parser.cc"
    break;

  case 211:
#line 1163 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 8587 "Parser/parser.cc"
    break;

  case 212:
#line 1165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Declarations only allowed at the start of the switch body, i.e., after the '{'." ); (yyval.sn) = nullptr; }
#line 8593 "Parser/parser.cc"
    break;

  case 213:
#line 1170 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_expr( (yyvsp[-1].en) ) ); }
#line 8599 "Parser/parser.cc"
    break;

  case 214:
#line 1172 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_mutex( nullptr, new StatementNode( build_expr( (yyvsp[-1].en) ) ) ) ); }
#line 8605 "Parser/parser.cc"
    break;

  case 215:
#line 1179 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn); }
#line 8611 "Parser/parser.cc"
    break;

  case 216:
#line 1181 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( true, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8617 "Parser/parser.cc"
    break;

  case 217:
#line 1183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( true, (yyvsp[-7].en), (yyvsp[-2].sn) ) );
			// The semantics of the declaration list is changed to include associated initialization, which is performed
			// *before* the transfer to the appropriate case clause by hoisting the declarations into a compound
			// statement around the switch.  Statements after the initial declaration list can never be executed, and
			// therefore, are removed from the grammar even though C allows it. The change also applies to choose
			// statement.
			(yyval.sn) = (yyvsp[-3].decl) ? new StatementNode( build_compound( (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 8631 "Parser/parser.cc"
    break;

  case 218:
#line 1193 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Only declarations can appear before the list of case clauses." ); (yyval.sn) = nullptr; }
#line 8637 "Parser/parser.cc"
    break;

  case 219:
#line 1195 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( false, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8643 "Parser/parser.cc"
    break;

  case 220:
#line 1197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( false, (yyvsp[-7].en), (yyvsp[-2].sn) ) );
			(yyval.sn) = (yyvsp[-3].decl) ? new StatementNode( build_compound( (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 8652 "Parser/parser.cc"
    break;

  case 221:
#line 1202 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Only declarations can appear before the list of case clauses." ); (yyval.sn) = nullptr; }
#line 8658 "Parser/parser.cc"
    break;

  case 222:
#line 1208 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-2].ifctl), maybe_build_compound( (yyvsp[0].sn) ), nullptr ) ); }
#line 8664 "Parser/parser.cc"
    break;

  case 223:
#line 1210 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-4].ifctl), maybe_build_compound( (yyvsp[-2].sn) ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8670 "Parser/parser.cc"
    break;

  case 224:
#line 1215 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( nullptr, (yyvsp[0].en) ); }
#line 8676 "Parser/parser.cc"
    break;

  case 225:
#line 1217 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 8682 "Parser/parser.cc"
    break;

  case 226:
#line 1219 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 8688 "Parser/parser.cc"
    break;

  case 227:
#line 1221 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[-1].decl), (yyvsp[0].en) ); }
#line 8694 "Parser/parser.cc"
    break;

  case 228:
#line 1228 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = (yyvsp[0].en); }
#line 8700 "Parser/parser.cc"
    break;

  case 229:
#line 1230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 8706 "Parser/parser.cc"
    break;

  case 231:
#line 1235 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.sn) = new StatementNode( build_case( (yyvsp[0].en) ) ); }
#line 8712 "Parser/parser.cc"
    break;

  case 232:
#line 1237 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)((yyvsp[-2].sn)->set_last( new StatementNode( build_case( (yyvsp[0].en) ) ) ) ); }
#line 8718 "Parser/parser.cc"
    break;

  case 233:
#line 1242 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing case list after case." ); (yyval.sn) = nullptr; }
#line 8724 "Parser/parser.cc"
    break;

  case 234:
#line 1243 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn); }
#line 8730 "Parser/parser.cc"
    break;

  case 235:
#line 1245 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing colon after case list." ); (yyval.sn) = nullptr; }
#line 8736 "Parser/parser.cc"
    break;

  case 236:
#line 1246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.sn) = new StatementNode( build_default() ); }
#line 8742 "Parser/parser.cc"
    break;

  case 237:
#line 1249 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing colon after default." ); (yyval.sn) = nullptr; }
#line 8748 "Parser/parser.cc"
    break;

  case 239:
#line 1254 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)( (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) )); }
#line 8754 "Parser/parser.cc"
    break;

  case 240:
#line 1258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( maybe_build_compound( (yyvsp[0].sn) ) ); }
#line 8760 "Parser/parser.cc"
    break;

  case 241:
#line 1263 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = nullptr; }
#line 8766 "Parser/parser.cc"
    break;

  case 243:
#line 1269 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ); }
#line 8772 "Parser/parser.cc"
    break;

  case 244:
#line 1271 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)( (yyvsp[-2].sn)->set_last( (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ) ) ); }
#line 8778 "Parser/parser.cc"
    break;

  case 245:
#line 1276 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8784 "Parser/parser.cc"
    break;

  case 246:
#line 1278 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_while( new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( (yyvsp[-2].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse, "" );
		}
#line 8793 "Parser/parser.cc"
    break;

  case 247:
#line 1283 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( (yyvsp[-2].ifctl), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8799 "Parser/parser.cc"
    break;

  case 248:
#line 1285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( (yyvsp[-4].ifctl), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[0].sn) ) ); }
#line 8805 "Parser/parser.cc"
    break;

  case 249:
#line 1287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( NEW_ONE, maybe_build_compound( (yyvsp[-4].sn) ) ) ); }
#line 8811 "Parser/parser.cc"
    break;

  case 250:
#line 1289 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_do_while( NEW_ONE, maybe_build_compound( (yyvsp[-5].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse, "" );
		}
#line 8820 "Parser/parser.cc"
    break;

  case 251:
#line 1294 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( (yyvsp[-2].en), maybe_build_compound( (yyvsp[-5].sn) ) ) ); }
#line 8826 "Parser/parser.cc"
    break;

  case 252:
#line 1296 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( (yyvsp[-3].en), maybe_build_compound( (yyvsp[-6].sn) ), (yyvsp[0].sn) ) ); }
#line 8832 "Parser/parser.cc"
    break;

  case 253:
#line 1298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( new ForCtrl( (ExpressionNode * )nullptr, (ExpressionNode * )nullptr, (ExpressionNode * )nullptr ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8838 "Parser/parser.cc"
    break;

  case 254:
#line 1300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_for( new ForCtrl( (ExpressionNode * )nullptr, (ExpressionNode * )nullptr, (ExpressionNode * )nullptr ), maybe_build_compound( (yyvsp[-2].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse, "" );
		}
#line 8847 "Parser/parser.cc"
    break;

  case 255:
#line 1305 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( (yyvsp[-2].fctl), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8853 "Parser/parser.cc"
    break;

  case 256:
#line 1307 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( (yyvsp[-4].fctl), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[0].sn) ) ); }
#line 8859 "Parser/parser.cc"
    break;

  case 258:
#line 1317 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 8878 "Parser/parser.cc"
    break;

  case 259:
#line 1335 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (ExpressionNode * )nullptr, (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8884 "Parser/parser.cc"
    break;

  case 260:
#line 1337 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8890 "Parser/parser.cc"
    break;

  case 261:
#line 1339 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-3].decl), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8896 "Parser/parser.cc"
    break;

  case 262:
#line 1342 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (ExpressionNode *)nullptr, (yyvsp[0].en), nullptr ); }
#line 8902 "Parser/parser.cc"
    break;

  case 263:
#line 1344 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (ExpressionNode *)nullptr, (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8908 "Parser/parser.cc"
    break;

  case 264:
#line 1347 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), NEW_ZERO, OperKinds::LThan, (yyvsp[0].en)->clone(), NEW_ONE ); }
#line 8914 "Parser/parser.cc"
    break;

  case 265:
#line 1349 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].compop), NEW_ZERO, (yyvsp[0].en)->clone() ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 8920 "Parser/parser.cc"
    break;

  case 266:
#line 1352 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].compop), (yyvsp[-2].en)->clone(), (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), (yyvsp[-2].en)->clone() ), NEW_ONE ); }
#line 8926 "Parser/parser.cc"
    break;

  case 267:
#line 1354 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[0].en)->clone(), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 8935 "Parser/parser.cc"
    break;

  case 268:
#line 1359 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
		}
#line 8944 "Parser/parser.cc"
    break;

  case 269:
#line 1364 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en)->clone(), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), (yyvsp[0].en) ); }
#line 8950 "Parser/parser.cc"
    break;

  case 270:
#line 1366 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-2].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].en)->clone(), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 8959 "Parser/parser.cc"
    break;

  case 271:
#line 1371 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
		}
#line 8968 "Parser/parser.cc"
    break;

  case 272:
#line 1376 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 8974 "Parser/parser.cc"
    break;

  case 273:
#line 1378 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 8980 "Parser/parser.cc"
    break;

  case 274:
#line 1380 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 8986 "Parser/parser.cc"
    break;

  case 275:
#line 1382 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 8992 "Parser/parser.cc"
    break;

  case 276:
#line 1384 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 8998 "Parser/parser.cc"
    break;

  case 277:
#line 1387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-2].en), NEW_ZERO, OperKinds::LThan, (yyvsp[0].en)->clone(), NEW_ONE ); }
#line 9004 "Parser/parser.cc"
    break;

  case 278:
#line 1389 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-3].en), UPDOWN( (yyvsp[-1].compop), NEW_ZERO, (yyvsp[0].en)->clone() ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9010 "Parser/parser.cc"
    break;

  case 279:
#line 1392 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), UPDOWN( (yyvsp[-1].compop), (yyvsp[-2].en)->clone(), (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), (yyvsp[-2].en)->clone() ), NEW_ONE ); }
#line 9016 "Parser/parser.cc"
    break;

  case 280:
#line 1394 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-4].en), (yyvsp[0].en)->clone(), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 9025 "Parser/parser.cc"
    break;

  case 281:
#line 1399 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::GThan || (yyvsp[-1].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 9035 "Parser/parser.cc"
    break;

  case 282:
#line 1405 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing low/high value for up/down-to range so index is uninitialized." ); (yyval.fctl) = nullptr; }
#line 9041 "Parser/parser.cc"
    break;

  case 283:
#line 1408 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en)->clone(), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), (yyvsp[0].en) ); }
#line 9047 "Parser/parser.cc"
    break;

  case 284:
#line 1410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-6].en), (yyvsp[-2].en)->clone(), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 9056 "Parser/parser.cc"
    break;

  case 285:
#line 1415 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 9066 "Parser/parser.cc"
    break;

  case 286:
#line 1421 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en)->clone(), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), nullptr ); }
#line 9072 "Parser/parser.cc"
    break;

  case 287:
#line 1423 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-6].en), (yyvsp[-2].en)->clone(), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 9081 "Parser/parser.cc"
    break;

  case 288:
#line 1428 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 9091 "Parser/parser.cc"
    break;

  case 289:
#line 1434 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing low/high value for up/down-to range so index is uninitialized." ); (yyval.fctl) = nullptr; }
#line 9097 "Parser/parser.cc"
    break;

  case 290:
#line 1437 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-1].decl), NEW_ZERO, OperKinds::LThan, (yyvsp[0].en), NEW_ONE ); }
#line 9103 "Parser/parser.cc"
    break;

  case 291:
#line 1439 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].decl), UPDOWN( (yyvsp[-1].compop), NEW_ZERO, (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9109 "Parser/parser.cc"
    break;

  case 292:
#line 1442 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-3].decl), UPDOWN( (yyvsp[-1].compop), (yyvsp[-2].en)->clone(), (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), (yyvsp[-2].en)->clone() ), NEW_ONE ); }
#line 9115 "Parser/parser.cc"
    break;

  case 293:
#line 1444 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-3].decl), (yyvsp[0].en), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 9124 "Parser/parser.cc"
    break;

  case 294:
#line 1449 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::GThan || (yyvsp[-1].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-3].decl), (yyvsp[-2].en), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 9134 "Parser/parser.cc"
    break;

  case 295:
#line 1456 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), (yyvsp[0].en) ); }
#line 9140 "Parser/parser.cc"
    break;

  case 296:
#line 1458 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-2].en), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 9149 "Parser/parser.cc"
    break;

  case 297:
#line 1463 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-4].en), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 9159 "Parser/parser.cc"
    break;

  case 298:
#line 1469 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), nullptr ); }
#line 9165 "Parser/parser.cc"
    break;

  case 299:
#line 1471 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-2].en), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 9174 "Parser/parser.cc"
    break;

  case 300:
#line 1476 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-4].en), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 9184 "Parser/parser.cc"
    break;

  case 301:
#line 1482 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing low/high value for up/down-to range so index is uninitialized." ); (yyval.fctl) = nullptr; }
#line 9190 "Parser/parser.cc"
    break;

  case 302:
#line 1485 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.fctl) = nullptr;
			//$$ = forCtrl( new ExpressionNode( build_varref( $3 ) ), $1, nullptr, OperKinds::Range, nullptr, nullptr );
		}
#line 9199 "Parser/parser.cc"
    break;

  case 303:
#line 1490 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LEThan || (yyvsp[-1].compop) == OperKinds::GEThan ) { SemanticError( yylloc, "All enumation ranges are equal (all values). Remove \"=~\"." ); (yyval.fctl) = nullptr; }
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.fctl) = nullptr;
		}
#line 9208 "Parser/parser.cc"
    break;

  case 304:
#line 1498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GThan; }
#line 9214 "Parser/parser.cc"
    break;

  case 305:
#line 1500 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 9220 "Parser/parser.cc"
    break;

  case 306:
#line 1502 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GEThan; }
#line 9226 "Parser/parser.cc"
    break;

  case 307:
#line 1507 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LThan; }
#line 9232 "Parser/parser.cc"
    break;

  case 308:
#line 1509 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GThan; }
#line 9238 "Parser/parser.cc"
    break;

  case 310:
#line 1515 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 9244 "Parser/parser.cc"
    break;

  case 311:
#line 1517 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GEThan; }
#line 9250 "Parser/parser.cc"
    break;

  case 312:
#line 1522 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Goto ) ); }
#line 9256 "Parser/parser.cc"
    break;

  case 313:
#line 1526 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_computedgoto( (yyvsp[-1].en) ) ); }
#line 9262 "Parser/parser.cc"
    break;

  case 314:
#line 1529 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThrough ) ); }
#line 9268 "Parser/parser.cc"
    break;

  case 315:
#line 1531 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::FallThrough ) ); }
#line 9274 "Parser/parser.cc"
    break;

  case 316:
#line 1533 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThroughDefault ) ); }
#line 9280 "Parser/parser.cc"
    break;

  case 317:
#line 1536 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Continue ) ); }
#line 9286 "Parser/parser.cc"
    break;

  case 318:
#line 1540 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Continue ) ); }
#line 9292 "Parser/parser.cc"
    break;

  case 319:
#line 1543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Break ) ); }
#line 9298 "Parser/parser.cc"
    break;

  case 320:
#line 1547 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Break ) ); }
#line 9304 "Parser/parser.cc"
    break;

  case 321:
#line 1549 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_return( (yyvsp[-1].en) ) ); }
#line 9310 "Parser/parser.cc"
    break;

  case 322:
#line 1551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 9316 "Parser/parser.cc"
    break;

  case 323:
#line 1553 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr ) ); }
#line 9322 "Parser/parser.cc"
    break;

  case 324:
#line 1555 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn) ) ); }
#line 9328 "Parser/parser.cc"
    break;

  case 325:
#line 1557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Coroutine ) ); }
#line 9334 "Parser/parser.cc"
    break;

  case 326:
#line 1559 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Coroutine ) ); }
#line 9340 "Parser/parser.cc"
    break;

  case 327:
#line 1561 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Generator ) ); }
#line 9346 "Parser/parser.cc"
    break;

  case 328:
#line 1563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Generator ) ); }
#line 9352 "Parser/parser.cc"
    break;

  case 329:
#line 1565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_throw( (yyvsp[-1].en) ) ); }
#line 9358 "Parser/parser.cc"
    break;

  case 330:
#line 1567 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume( (yyvsp[-1].en) ) ); }
#line 9364 "Parser/parser.cc"
    break;

  case 331:
#line 1569 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume_at( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 9370 "Parser/parser.cc"
    break;

  case 334:
#line 1579 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_with( (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 9376 "Parser/parser.cc"
    break;

  case 335:
#line 1585 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_mutex( (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 9382 "Parser/parser.cc"
    break;

  case 336:
#line 1589 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 9388 "Parser/parser.cc"
    break;

  case 337:
#line 1594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9394 "Parser/parser.cc"
    break;

  case 339:
#line 1600 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 9400 "Parser/parser.cc"
    break;

  case 340:
#line 1604 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) )); }
#line 9406 "Parser/parser.cc"
    break;

  case 342:
#line 1611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.en) = nullptr; }
#line 9412 "Parser/parser.cc"
    break;

  case 343:
#line 1615 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 9418 "Parser/parser.cc"
    break;

  case 344:
#line 1620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 9424 "Parser/parser.cc"
    break;

  case 345:
#line 1622 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-3].en), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[-4].en), (yyvsp[0].wfs) ); }
#line 9430 "Parser/parser.cc"
    break;

  case 346:
#line 1624 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 9436 "Parser/parser.cc"
    break;

  case 347:
#line 1626 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( nullptr, maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 9442 "Parser/parser.cc"
    break;

  case 348:
#line 1629 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 9448 "Parser/parser.cc"
    break;

  case 349:
#line 1631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-5].en), maybe_build_compound( (yyvsp[-4].sn) ), (yyvsp[-6].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 9454 "Parser/parser.cc"
    break;

  case 350:
#line 1636 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-1].en), (yyvsp[0].sn), (yyvsp[-2].en) ) ); }
#line 9460 "Parser/parser.cc"
    break;

  case 351:
#line 1638 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-3].en), (yyvsp[-2].sn), (yyvsp[-4].en), (yyvsp[0].wfs) ) ); }
#line 9466 "Parser/parser.cc"
    break;

  case 352:
#line 1643 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), (yyvsp[0].sn), 0 ) ); }
#line 9472 "Parser/parser.cc"
    break;

  case 353:
#line 1645 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), 0, (yyvsp[0].sn) ) ); }
#line 9478 "Parser/parser.cc"
    break;

  case 354:
#line 1647 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-2].sn), (yyvsp[-1].sn), (yyvsp[0].sn) ) ); }
#line 9484 "Parser/parser.cc"
    break;

  case 355:
#line 1652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 9490 "Parser/parser.cc"
    break;

  case 356:
#line 1654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)(yyvsp[-8].sn)->set_last( new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ) ); }
#line 9496 "Parser/parser.cc"
    break;

  case 357:
#line 1659 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9502 "Parser/parser.cc"
    break;

  case 358:
#line 1660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[0].en); }
#line 9508 "Parser/parser.cc"
    break;

  case 359:
#line 1664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 9514 "Parser/parser.cc"
    break;

  case 360:
#line 1665 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 9520 "Parser/parser.cc"
    break;

  case 361:
#line 1666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.catch_kind) = CatchStmt::Resume; }
#line 9526 "Parser/parser.cc"
    break;

  case 362:
#line 1667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Resume; }
#line 9532 "Parser/parser.cc"
    break;

  case 363:
#line 1671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = new StatementNode( build_finally( (yyvsp[0].sn) ) ); }
#line 9538 "Parser/parser.cc"
    break;

  case 365:
#line 1678 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 9544 "Parser/parser.cc"
    break;

  case 366:
#line 1680 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 9550 "Parser/parser.cc"
    break;

  case 367:
#line 1682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 9556 "Parser/parser.cc"
    break;

  case 372:
#line 1697 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-4].flag), (yyvsp[-2].constant), 0 ) ); }
#line 9562 "Parser/parser.cc"
    break;

  case 373:
#line 1699 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-6].flag), (yyvsp[-4].constant), (yyvsp[-2].en) ) ); }
#line 9568 "Parser/parser.cc"
    break;

  case 374:
#line 1701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-8].flag), (yyvsp[-6].constant), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 9574 "Parser/parser.cc"
    break;

  case 375:
#line 1703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-10].flag), (yyvsp[-8].constant), (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 9580 "Parser/parser.cc"
    break;

  case 376:
#line 1705 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-12].flag), (yyvsp[-9].constant), 0, (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].label) ) ); }
#line 9586 "Parser/parser.cc"
    break;

  case 377:
#line 1710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = false; }
#line 9592 "Parser/parser.cc"
    break;

  case 378:
#line 1712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = true; }
#line 9598 "Parser/parser.cc"
    break;

  case 379:
#line 1717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9604 "Parser/parser.cc"
    break;

  case 382:
#line 1724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 9610 "Parser/parser.cc"
    break;

  case 383:
#line 1729 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( nullptr, (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 9616 "Parser/parser.cc"
    break;

  case 384:
#line 1731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( (yyvsp[-5].tok), (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 9622 "Parser/parser.cc"
    break;

  case 385:
#line 1736 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9628 "Parser/parser.cc"
    break;

  case 386:
#line 1738 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 9634 "Parser/parser.cc"
    break;

  case 387:
#line 1740 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( (yyvsp[0].constant) ) )); }
#line 9640 "Parser/parser.cc"
    break;

  case 388:
#line 1745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = new LabelNode(); (yyval.label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 9649 "Parser/parser.cc"
    break;

  case 389:
#line 1750 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = (yyvsp[-2].label); (yyvsp[-2].label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 9658 "Parser/parser.cc"
    break;

  case 390:
#line 1760 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9664 "Parser/parser.cc"
    break;

  case 393:
#line 1767 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 9670 "Parser/parser.cc"
    break;

  case 394:
#line 1772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9676 "Parser/parser.cc"
    break;

  case 396:
#line 1778 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 9682 "Parser/parser.cc"
    break;

  case 397:
#line 1780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-2].decl) ); }
#line 9688 "Parser/parser.cc"
    break;

  case 404:
#line 1800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "C_DECLARATION1 %p %s\n", $$, $$->name ? $$->name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 9699 "Parser/parser.cc"
    break;

  case 407:
#line 1812 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-4].en), (yyvsp[-2].constant) ); }
#line 9705 "Parser/parser.cc"
    break;

  case 408:
#line 1814 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-2].en), build_constantStr( *new string( "\"\"" ) ) ); }
#line 9711 "Parser/parser.cc"
    break;

  case 412:
#line 1832 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9717 "Parser/parser.cc"
    break;

  case 414:
#line 1838 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].in) ); }
#line 9723 "Parser/parser.cc"
    break;

  case 415:
#line 1842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 9729 "Parser/parser.cc"
    break;

  case 416:
#line 1844 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].in) ) ); }
#line 9735 "Parser/parser.cc"
    break;

  case 417:
#line 1851 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 9741 "Parser/parser.cc"
    break;

  case 418:
#line 1853 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 9747 "Parser/parser.cc"
    break;

  case 419:
#line 1855 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 9753 "Parser/parser.cc"
    break;

  case 421:
#line 1861 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9759 "Parser/parser.cc"
    break;

  case 422:
#line 1863 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9765 "Parser/parser.cc"
    break;

  case 423:
#line 1865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 9771 "Parser/parser.cc"
    break;

  case 424:
#line 1867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeClone( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->appendList( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 9782 "Parser/parser.cc"
    break;

  case 425:
#line 1900 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), 0 )->addQualifiers( (yyvsp[0].decl) ); }
#line 9788 "Parser/parser.cc"
    break;

  case 426:
#line 1902 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), 0 )->addQualifiers( (yyvsp[0].decl) ); }
#line 9794 "Parser/parser.cc"
    break;

  case 427:
#line 1907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 9800 "Parser/parser.cc"
    break;

  case 428:
#line 1910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-6].decl)->appendList( (yyvsp[-2].decl) ) ); }
#line 9806 "Parser/parser.cc"
    break;

  case 429:
#line 1915 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 9815 "Parser/parser.cc"
    break;

  case 430:
#line 1920 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 9824 "Parser/parser.cc"
    break;

  case 431:
#line 1925 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "3" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 9833 "Parser/parser.cc"
    break;

  case 432:
#line 1936 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "4" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef();
		}
#line 9842 "Parser/parser.cc"
    break;

  case 433:
#line 1941 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "5" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 9851 "Parser/parser.cc"
    break;

  case 434:
#line 1946 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "6" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[-3].decl) )->addTypedef();
		}
#line 9860 "Parser/parser.cc"
    break;

  case 435:
#line 1951 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "7" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-2].decl) )->addTypedef();
		}
#line 9869 "Parser/parser.cc"
    break;

  case 436:
#line 1956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "8" );
			(yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-3].decl) )->addTypedef()->addType( (yyvsp[-3].decl) );
		}
#line 9878 "Parser/parser.cc"
    break;

  case 437:
#line 1965 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 9886 "Parser/parser.cc"
    break;

  case 438:
#line 1969 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 9894 "Parser/parser.cc"
    break;

  case 439:
#line 1976 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 9900 "Parser/parser.cc"
    break;

  case 443:
#line 1986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 9906 "Parser/parser.cc"
    break;

  case 444:
#line 1988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ) ); }
#line 9912 "Parser/parser.cc"
    break;

  case 452:
#line 2011 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_type_specifier2 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 9923 "Parser/parser.cc"
    break;

  case 457:
#line 2033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9929 "Parser/parser.cc"
    break;

  case 460:
#line 2045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9935 "Parser/parser.cc"
    break;

  case 463:
#line 2055 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Const ); }
#line 9941 "Parser/parser.cc"
    break;

  case 464:
#line 2057 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Restrict ); }
#line 9947 "Parser/parser.cc"
    break;

  case 465:
#line 2059 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Volatile ); }
#line 9953 "Parser/parser.cc"
    break;

  case 466:
#line 2061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Atomic ); }
#line 9959 "Parser/parser.cc"
    break;

  case 468:
#line 2067 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newForall( (yyvsp[-1].decl) ); }
#line 9965 "Parser/parser.cc"
    break;

  case 470:
#line 2073 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9971 "Parser/parser.cc"
    break;

  case 471:
#line 2075 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 9977 "Parser/parser.cc"
    break;

  case 473:
#line 2086 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9983 "Parser/parser.cc"
    break;

  case 474:
#line 2091 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Extern ); }
#line 9989 "Parser/parser.cc"
    break;

  case 475:
#line 2093 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Static ); }
#line 9995 "Parser/parser.cc"
    break;

  case 476:
#line 2095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Auto ); }
#line 10001 "Parser/parser.cc"
    break;

  case 477:
#line 2097 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Register ); }
#line 10007 "Parser/parser.cc"
    break;

  case 478:
#line 2099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::ThreadlocalGcc ); }
#line 10013 "Parser/parser.cc"
    break;

  case 479:
#line 2101 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::ThreadlocalC11 ); }
#line 10019 "Parser/parser.cc"
    break;

  case 480:
#line 2104 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Inline ); }
#line 10025 "Parser/parser.cc"
    break;

  case 481:
#line 2106 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Fortran ); }
#line 10031 "Parser/parser.cc"
    break;

  case 482:
#line 2108 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Noreturn ); }
#line 10037 "Parser/parser.cc"
    break;

  case 483:
#line 2113 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 10043 "Parser/parser.cc"
    break;

  case 484:
#line 2115 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Bool ); }
#line 10049 "Parser/parser.cc"
    break;

  case 485:
#line 2117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Char ); }
#line 10055 "Parser/parser.cc"
    break;

  case 486:
#line 2119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int ); }
#line 10061 "Parser/parser.cc"
    break;

  case 487:
#line 2121 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 ); }
#line 10067 "Parser/parser.cc"
    break;

  case 488:
#line 2123 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 )->addType( DeclarationNode::newSignedNess( DeclarationNode::Unsigned ) ); }
#line 10073 "Parser/parser.cc"
    break;

  case 489:
#line 2125 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Float ); }
#line 10079 "Parser/parser.cc"
    break;

  case 490:
#line 2127 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Double ); }
#line 10085 "Parser/parser.cc"
    break;

  case 491:
#line 2129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat80 ); }
#line 10091 "Parser/parser.cc"
    break;

  case 492:
#line 2131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat128 ); }
#line 10097 "Parser/parser.cc"
    break;

  case 493:
#line 2133 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat16 ); }
#line 10103 "Parser/parser.cc"
    break;

  case 494:
#line 2135 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32 ); }
#line 10109 "Parser/parser.cc"
    break;

  case 495:
#line 2137 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32x ); }
#line 10115 "Parser/parser.cc"
    break;

  case 496:
#line 2139 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64 ); }
#line 10121 "Parser/parser.cc"
    break;

  case 497:
#line 2141 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64x ); }
#line 10127 "Parser/parser.cc"
    break;

  case 498:
#line 2143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat128 ); }
#line 10133 "Parser/parser.cc"
    break;

  case 499:
#line 2145 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10139 "Parser/parser.cc"
    break;

  case 500:
#line 2147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10145 "Parser/parser.cc"
    break;

  case 501:
#line 2149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10151 "Parser/parser.cc"
    break;

  case 502:
#line 2151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Complex ); }
#line 10157 "Parser/parser.cc"
    break;

  case 503:
#line 2153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Imaginary ); }
#line 10163 "Parser/parser.cc"
    break;

  case 504:
#line 2155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Signed ); }
#line 10169 "Parser/parser.cc"
    break;

  case 505:
#line 2157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Unsigned ); }
#line 10175 "Parser/parser.cc"
    break;

  case 506:
#line 2159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Short ); }
#line 10181 "Parser/parser.cc"
    break;

  case 507:
#line 2161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Long ); }
#line 10187 "Parser/parser.cc"
    break;

  case 508:
#line 2163 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Valist ); }
#line 10193 "Parser/parser.cc"
    break;

  case 509:
#line 2165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::AutoType ); }
#line 10199 "Parser/parser.cc"
    break;

  case 511:
#line 2171 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10205 "Parser/parser.cc"
    break;

  case 513:
#line 2177 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVtableType( (yyvsp[-2].decl) ); }
#line 10211 "Parser/parser.cc"
    break;

  case 514:
#line 2183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10217 "Parser/parser.cc"
    break;

  case 515:
#line 2185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10223 "Parser/parser.cc"
    break;

  case 517:
#line 2192 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10229 "Parser/parser.cc"
    break;

  case 518:
#line 2194 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10235 "Parser/parser.cc"
    break;

  case 519:
#line 2196 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10241 "Parser/parser.cc"
    break;

  case 520:
#line 2198 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 10247 "Parser/parser.cc"
    break;

  case 522:
#line 2205 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10253 "Parser/parser.cc"
    break;

  case 524:
#line 2211 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10259 "Parser/parser.cc"
    break;

  case 525:
#line 2213 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10265 "Parser/parser.cc"
    break;

  case 526:
#line 2215 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 10271 "Parser/parser.cc"
    break;

  case 527:
#line 2220 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10277 "Parser/parser.cc"
    break;

  case 528:
#line 2222 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en) ); }
#line 10283 "Parser/parser.cc"
    break;

  case 529:
#line 2224 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 10289 "Parser/parser.cc"
    break;

  case 530:
#line 2226 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en), true ); }
#line 10295 "Parser/parser.cc"
    break;

  case 531:
#line 2228 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Zero ); }
#line 10301 "Parser/parser.cc"
    break;

  case 532:
#line 2230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::One ); }
#line 10307 "Parser/parser.cc"
    break;

  case 533:
#line 2235 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_declaration_specifier %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10318 "Parser/parser.cc"
    break;

  case 534:
#line 2242 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10324 "Parser/parser.cc"
    break;

  case 535:
#line 2244 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10330 "Parser/parser.cc"
    break;

  case 536:
#line 2246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10336 "Parser/parser.cc"
    break;

  case 537:
#line 2251 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_type_specifier %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10347 "Parser/parser.cc"
    break;

  case 538:
#line 2258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 10353 "Parser/parser.cc"
    break;

  case 539:
#line 2260 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10359 "Parser/parser.cc"
    break;

  case 540:
#line 2262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 10368 "Parser/parser.cc"
    break;

  case 542:
#line 2271 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10374 "Parser/parser.cc"
    break;

  case 543:
#line 2273 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10380 "Parser/parser.cc"
    break;

  case 544:
#line 2275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10386 "Parser/parser.cc"
    break;

  case 546:
#line 2281 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10392 "Parser/parser.cc"
    break;

  case 547:
#line 2283 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10398 "Parser/parser.cc"
    break;

  case 549:
#line 2289 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10404 "Parser/parser.cc"
    break;

  case 550:
#line 2291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10410 "Parser/parser.cc"
    break;

  case 551:
#line 2293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10416 "Parser/parser.cc"
    break;

  case 553:
#line 2299 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10422 "Parser/parser.cc"
    break;

  case 554:
#line 2301 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10428 "Parser/parser.cc"
    break;

  case 555:
#line 2306 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypedef( (yyvsp[0].tok) ); }
#line 10434 "Parser/parser.cc"
    break;

  case 556:
#line 2308 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 10440 "Parser/parser.cc"
    break;

  case 557:
#line 2310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 10446 "Parser/parser.cc"
    break;

  case 559:
#line 2313 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), (yyvsp[0].decl) ); }
#line 10452 "Parser/parser.cc"
    break;

  case 560:
#line 2315 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), (yyvsp[0].decl) ); }
#line 10458 "Parser/parser.cc"
    break;

  case 561:
#line 2320 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[0].tok), nullptr ); }
#line 10464 "Parser/parser.cc"
    break;

  case 562:
#line 2322 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-2].tok), nullptr ); }
#line 10470 "Parser/parser.cc"
    break;

  case 563:
#line 2324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 10476 "Parser/parser.cc"
    break;

  case 564:
#line 2329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "elaborated_type %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10487 "Parser/parser.cc"
    break;

  case 568:
#line 2345 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 10493 "Parser/parser.cc"
    break;

  case 569:
#line 2347 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 10499 "Parser/parser.cc"
    break;

  case 570:
#line 2349 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 10508 "Parser/parser.cc"
    break;

  case 571:
#line 2354 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 10525 "Parser/parser.cc"
    break;

  case 572:
#line 2367 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 10534 "Parser/parser.cc"
    break;

  case 573:
#line 2372 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "AGG3\n" );
			DeclarationNode::newFromTypedef( (yyvsp[-5].tok) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 10544 "Parser/parser.cc"
    break;

  case 574:
#line 2378 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 10553 "Parser/parser.cc"
    break;

  case 575:
#line 2383 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "AGG4\n" );
			DeclarationNode::newFromTypeGen( (yyvsp[-5].tok), nullptr );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 10563 "Parser/parser.cc"
    break;

  case 577:
#line 2393 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 10569 "Parser/parser.cc"
    break;

  case 578:
#line 2395 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 10575 "Parser/parser.cc"
    break;

  case 579:
#line 2400 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 10585 "Parser/parser.cc"
    break;

  case 580:
#line 2406 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 10600 "Parser/parser.cc"
    break;

  case 583:
#line 2425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Struct; }
#line 10606 "Parser/parser.cc"
    break;

  case 584:
#line 2427 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Union; }
#line 10612 "Parser/parser.cc"
    break;

  case 585:
#line 2429 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Exception; }
#line 10618 "Parser/parser.cc"
    break;

  case 586:
#line 2435 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 10624 "Parser/parser.cc"
    break;

  case 587:
#line 2437 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 10630 "Parser/parser.cc"
    break;

  case 588:
#line 2439 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Generator; }
#line 10636 "Parser/parser.cc"
    break;

  case 589:
#line 2441 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor generator is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 10642 "Parser/parser.cc"
    break;

  case 590:
#line 2443 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Coroutine; }
#line 10648 "Parser/parser.cc"
    break;

  case 591:
#line 2445 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor coroutine is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 10654 "Parser/parser.cc"
    break;

  case 592:
#line 2447 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Thread; }
#line 10660 "Parser/parser.cc"
    break;

  case 593:
#line 2449 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor thread is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 10666 "Parser/parser.cc"
    break;

  case 594:
#line 2454 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10672 "Parser/parser.cc"
    break;

  case 595:
#line 2456 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 10678 "Parser/parser.cc"
    break;

  case 596:
#line 2461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "type_specifier1 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			(yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) );
			// printf( "type_specifier2 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10691 "Parser/parser.cc"
    break;

  case 597:
#line 2470 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 10697 "Parser/parser.cc"
    break;

  case 598:
#line 2472 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-1].decl) ) {								// field declarator ?
				(yyvsp[-1].decl) = DeclarationNode::newName( nullptr );
			} // if
			(yyvsp[-1].decl)->inLine = true;
			(yyval.decl) = distAttr( (yyvsp[-2].decl), (yyvsp[-1].decl) );					// mark all fields in list
			distInl( (yyvsp[-1].decl) );
		}
#line 10710 "Parser/parser.cc"
    break;

  case 599:
#line 2481 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10716 "Parser/parser.cc"
    break;

  case 602:
#line 2485 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 10722 "Parser/parser.cc"
    break;

  case 603:
#line 2487 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10728 "Parser/parser.cc"
    break;

  case 606:
#line 2494 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10734 "Parser/parser.cc"
    break;

  case 608:
#line 2497 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 10740 "Parser/parser.cc"
    break;

  case 609:
#line 2502 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].en) ); }
#line 10746 "Parser/parser.cc"
    break;

  case 610:
#line 2505 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 10752 "Parser/parser.cc"
    break;

  case 611:
#line 2508 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 10758 "Parser/parser.cc"
    break;

  case 612:
#line 2513 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10764 "Parser/parser.cc"
    break;

  case 614:
#line 2516 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 10770 "Parser/parser.cc"
    break;

  case 616:
#line 2527 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 10776 "Parser/parser.cc"
    break;

  case 617:
#line 2529 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 10782 "Parser/parser.cc"
    break;

  case 619:
#line 2536 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 10788 "Parser/parser.cc"
    break;

  case 620:
#line 2541 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 10794 "Parser/parser.cc"
    break;

  case 622:
#line 2547 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 10800 "Parser/parser.cc"
    break;

  case 623:
#line 2552 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, false )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10806 "Parser/parser.cc"
    break;

  case 624:
#line 2554 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); }
#line 10812 "Parser/parser.cc"
    break;

  case 625:
#line 2556 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].tok), (yyvsp[-2].decl), true, false )->addQualifiers( (yyvsp[-6].decl) ); }
#line 10818 "Parser/parser.cc"
    break;

  case 626:
#line 2559 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-4].decl)->name, (yyvsp[-2].decl), true, false )->addQualifiers( (yyvsp[-5].decl) ); }
#line 10824 "Parser/parser.cc"
    break;

  case 627:
#line 2561 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-6].decl)->storageClasses.val != 0 || (yyvsp[-6].decl)->type->qualifiers.val != 0 )
			{ SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }

			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true, (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 10835 "Parser/parser.cc"
    break;

  case 628:
#line 2568 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 10843 "Parser/parser.cc"
    break;

  case 629:
#line 2572 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].decl)->storageClasses.val != 0 || (yyvsp[-4].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			typedefTable.makeTypedef( *(yyvsp[-1].tok) );
		}
#line 10852 "Parser/parser.cc"
    break;

  case 630:
#line 2577 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, true, (yyvsp[-9].decl) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 10860 "Parser/parser.cc"
    break;

  case 631:
#line 2582 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].tok), (yyvsp[-2].decl), true, true, nullptr )->addQualifiers( (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 10868 "Parser/parser.cc"
    break;

  case 632:
#line 2586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].decl)->name, (yyvsp[-2].decl), true, true, (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 10876 "Parser/parser.cc"
    break;

  case 633:
#line 2590 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].decl)->name, (yyvsp[-2].decl), true, true, nullptr )->addQualifiers( (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 10884 "Parser/parser.cc"
    break;

  case 635:
#line 2598 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); (yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), 0, false, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 10890 "Parser/parser.cc"
    break;

  case 636:
#line 2600 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].decl)->type->symbolic.name );	(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].decl)->type->symbolic.name, 0, false, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 10896 "Parser/parser.cc"
    break;

  case 637:
#line 2605 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].in) ); }
#line 10902 "Parser/parser.cc"
    break;

  case 638:
#line 2607 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( new string("inline"), nullptr ); }
#line 10908 "Parser/parser.cc"
    break;

  case 639:
#line 2609 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].in) ) ); }
#line 10914 "Parser/parser.cc"
    break;

  case 640:
#line 2611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( DeclarationNode::newEnumValueGeneric( new string("inline"), nullptr ) ); }
#line 10920 "Parser/parser.cc"
    break;

  case 641:
#line 2616 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 10926 "Parser/parser.cc"
    break;

  case 642:
#line 2617 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = new InitializerNode( (yyvsp[0].en) ); }
#line 10932 "Parser/parser.cc"
    break;

  case 643:
#line 2618 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                     { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 10938 "Parser/parser.cc"
    break;

  case 644:
#line 2625 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 10944 "Parser/parser.cc"
    break;

  case 645:
#line 2627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10950 "Parser/parser.cc"
    break;

  case 648:
#line 2631 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10956 "Parser/parser.cc"
    break;

  case 649:
#line 2633 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 10962 "Parser/parser.cc"
    break;

  case 650:
#line 2635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 10968 "Parser/parser.cc"
    break;

  case 652:
#line 2643 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10974 "Parser/parser.cc"
    break;

  case 653:
#line 2645 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10980 "Parser/parser.cc"
    break;

  case 654:
#line 2647 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-8].decl)->appendList( (yyvsp[-4].decl) )->appendList( (yyvsp[0].decl) ); }
#line 10986 "Parser/parser.cc"
    break;

  case 656:
#line 2653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 10992 "Parser/parser.cc"
    break;

  case 657:
#line 2658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10998 "Parser/parser.cc"
    break;

  case 658:
#line 2660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11004 "Parser/parser.cc"
    break;

  case 660:
#line 2663 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 11010 "Parser/parser.cc"
    break;

  case 663:
#line 2670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11016 "Parser/parser.cc"
    break;

  case 664:
#line 2672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11022 "Parser/parser.cc"
    break;

  case 666:
#line 2681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 11028 "Parser/parser.cc"
    break;

  case 667:
#line 2684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 11034 "Parser/parser.cc"
    break;

  case 668:
#line 2686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 11040 "Parser/parser.cc"
    break;

  case 673:
#line 2696 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11046 "Parser/parser.cc"
    break;

  case 675:
#line 2703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 11052 "Parser/parser.cc"
    break;

  case 676:
#line 2705 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 11058 "Parser/parser.cc"
    break;

  case 677:
#line 2710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 11064 "Parser/parser.cc"
    break;

  case 678:
#line 2712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 11070 "Parser/parser.cc"
    break;

  case 679:
#line 2721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11076 "Parser/parser.cc"
    break;

  case 680:
#line 2723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 11082 "Parser/parser.cc"
    break;

  case 686:
#line 2736 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 11088 "Parser/parser.cc"
    break;

  case 689:
#line 2746 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 11094 "Parser/parser.cc"
    break;

  case 690:
#line 2747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (yyvsp[-1].op) == OperKinds::Assign ? (yyvsp[0].in) : (yyvsp[0].in)->set_maybeConstructed( false ); }
#line 11100 "Parser/parser.cc"
    break;

  case 691:
#line 2748 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.in) = new InitializerNode( true ); }
#line 11106 "Parser/parser.cc"
    break;

  case 692:
#line 2749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 11112 "Parser/parser.cc"
    break;

  case 693:
#line 2753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = new InitializerNode( (yyvsp[0].en) ); }
#line 11118 "Parser/parser.cc"
    break;

  case 694:
#line 2754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 11124 "Parser/parser.cc"
    break;

  case 695:
#line 2759 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 11130 "Parser/parser.cc"
    break;

  case 697:
#line 2761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ); }
#line 11136 "Parser/parser.cc"
    break;

  case 698:
#line 2762 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (InitializerNode *)( (yyvsp[-2].in)->set_last( (yyvsp[0].in) ) ); }
#line 11142 "Parser/parser.cc"
    break;

  case 699:
#line 2763 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.in) = (InitializerNode *)((yyvsp[-3].in)->set_last( (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ) )); }
#line 11148 "Parser/parser.cc"
    break;

  case 701:
#line 2779 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[-1].tok) ) ); }
#line 11154 "Parser/parser.cc"
    break;

  case 703:
#line 2785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-1].en)->set_last( (yyvsp[0].en) )); }
#line 11160 "Parser/parser.cc"
    break;

  case 704:
#line 2791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 11166 "Parser/parser.cc"
    break;

  case 705:
#line 2794 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 11172 "Parser/parser.cc"
    break;

  case 706:
#line 2796 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 11178 "Parser/parser.cc"
    break;

  case 707:
#line 2798 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-4].en) ), maybeMoveBuild<Expression>( (yyvsp[-2].en) ) ) ); }
#line 11184 "Parser/parser.cc"
    break;

  case 708:
#line 2800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 11190 "Parser/parser.cc"
    break;

  case 710:
#line 2824 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl) ); }
#line 11196 "Parser/parser.cc"
    break;

  case 711:
#line 2829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11202 "Parser/parser.cc"
    break;

  case 712:
#line 2831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 11208 "Parser/parser.cc"
    break;

  case 713:
#line 2836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "9" );
			if ( (yyvsp[-1].tclass) == TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 11219 "Parser/parser.cc"
    break;

  case 714:
#line 2843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 11225 "Parser/parser.cc"
    break;

  case 715:
#line 2845 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "9" ); }
#line 11231 "Parser/parser.cc"
    break;

  case 716:
#line 2847 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 11237 "Parser/parser.cc"
    break;

  case 717:
#line 2849 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "9" );
			(yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 11246 "Parser/parser.cc"
    break;

  case 718:
#line 2855 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 11252 "Parser/parser.cc"
    break;

  case 719:
#line 2860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 11258 "Parser/parser.cc"
    break;

  case 720:
#line 2862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 11264 "Parser/parser.cc"
    break;

  case 721:
#line 2864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::DStype; }
#line 11270 "Parser/parser.cc"
    break;

  case 722:
#line 2868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 11276 "Parser/parser.cc"
    break;

  case 723:
#line 2873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 11282 "Parser/parser.cc"
    break;

  case 724:
#line 2875 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 11288 "Parser/parser.cc"
    break;

  case 725:
#line 2877 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ftype; }
#line 11294 "Parser/parser.cc"
    break;

  case 726:
#line 2879 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 11300 "Parser/parser.cc"
    break;

  case 727:
#line 2884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11306 "Parser/parser.cc"
    break;

  case 730:
#line 2891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 11312 "Parser/parser.cc"
    break;

  case 731:
#line 2896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 11318 "Parser/parser.cc"
    break;

  case 732:
#line 2898 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 11324 "Parser/parser.cc"
    break;

  case 733:
#line 2905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 11330 "Parser/parser.cc"
    break;

  case 735:
#line 2908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 11336 "Parser/parser.cc"
    break;

  case 736:
#line 2910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)( (yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 11342 "Parser/parser.cc"
    break;

  case 737:
#line 2915 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 11348 "Parser/parser.cc"
    break;

  case 738:
#line 2917 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11354 "Parser/parser.cc"
    break;

  case 739:
#line 2919 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 11360 "Parser/parser.cc"
    break;

  case 740:
#line 2924 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 11366 "Parser/parser.cc"
    break;

  case 741:
#line 2926 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 11372 "Parser/parser.cc"
    break;

  case 742:
#line 2931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "10" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), 0 );
		}
#line 11381 "Parser/parser.cc"
    break;

  case 743:
#line 2936 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "11" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 11390 "Parser/parser.cc"
    break;

  case 744:
#line 2944 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), 0 ); }
#line 11396 "Parser/parser.cc"
    break;

  case 745:
#line 2946 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-8].tok), (yyvsp[-6].decl), (yyvsp[-2].decl) ); }
#line 11402 "Parser/parser.cc"
    break;

  case 747:
#line 2952 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl) ); }
#line 11408 "Parser/parser.cc"
    break;

  case 752:
#line 2964 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 11414 "Parser/parser.cc"
    break;

  case 753:
#line 2969 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 11420 "Parser/parser.cc"
    break;

  case 754:
#line 2971 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 11426 "Parser/parser.cc"
    break;

  case 756:
#line 2979 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl);	}
#line 11432 "Parser/parser.cc"
    break;

  case 757:
#line 2984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11438 "Parser/parser.cc"
    break;

  case 758:
#line 2986 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->appendList( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 11444 "Parser/parser.cc"
    break;

  case 759:
#line 2991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11450 "Parser/parser.cc"
    break;

  case 761:
#line 2996 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 11456 "Parser/parser.cc"
    break;

  case 762:
#line 3000 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 11462 "Parser/parser.cc"
    break;

  case 763:
#line 3005 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( (yyvsp[0].tok) ) ) ); }
#line 11468 "Parser/parser.cc"
    break;

  case 765:
#line 3008 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, " declaration" ); (yyval.decl) = nullptr; }
#line 11474 "Parser/parser.cc"
    break;

  case 766:
#line 3010 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.decl) = nullptr; }
#line 11480 "Parser/parser.cc"
    break;

  case 767:
#line 3012 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.decl) = nullptr; }
#line 11486 "Parser/parser.cc"
    break;

  case 768:
#line 3014 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 11492 "Parser/parser.cc"
    break;

  case 769:
#line 3016 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 11498 "Parser/parser.cc"
    break;

  case 770:
#line 3018 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 11504 "Parser/parser.cc"
    break;

  case 772:
#line 3021 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 11513 "Parser/parser.cc"
    break;

  case 773:
#line 3026 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( false, (yyvsp[-2].constant), 0 ) ) ); }
#line 11519 "Parser/parser.cc"
    break;

  case 774:
#line 3028 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = LinkageSpec::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 11528 "Parser/parser.cc"
    break;

  case 775:
#line 3033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-1].decl);
		}
#line 11538 "Parser/parser.cc"
    break;

  case 776:
#line 3039 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = LinkageSpec::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 11547 "Parser/parser.cc"
    break;

  case 777:
#line 3044 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11557 "Parser/parser.cc"
    break;

  case 778:
#line 3051 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 11566 "Parser/parser.cc"
    break;

  case 779:
#line 3056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11576 "Parser/parser.cc"
    break;

  case 780:
#line 3062 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 11585 "Parser/parser.cc"
    break;

  case 781:
#line 3067 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11595 "Parser/parser.cc"
    break;

  case 782:
#line 3073 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.val) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.val) ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall) ) forall = true; // remember generic type
		}
#line 11604 "Parser/parser.cc"
    break;

  case 783:
#line 3078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11614 "Parser/parser.cc"
    break;

  case 785:
#line 3093 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].sn) ); }
#line 11620 "Parser/parser.cc"
    break;

  case 786:
#line 3095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].sn) ); }
#line 11626 "Parser/parser.cc"
    break;

  case 787:
#line 3100 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; forall = false; }
#line 11632 "Parser/parser.cc"
    break;

  case 788:
#line 3102 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.en) = (yyvsp[-2].en); forall = false;
			if ( (yyvsp[0].decl) ) {
				SemanticError( yylloc, "Attributes cannot be associated with function body. Move attribute(s) before \"with\" clause." );
				(yyval.en) = nullptr;
			} // if
		}
#line 11644 "Parser/parser.cc"
    break;

  case 789:
#line 3113 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11655 "Parser/parser.cc"
    break;

  case 790:
#line 3120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 11664 "Parser/parser.cc"
    break;

  case 791:
#line 3125 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 11673 "Parser/parser.cc"
    break;

  case 792:
#line 3131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 11679 "Parser/parser.cc"
    break;

  case 793:
#line 3134 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 11685 "Parser/parser.cc"
    break;

  case 794:
#line 3137 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 11691 "Parser/parser.cc"
    break;

  case 795:
#line 3141 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-4].decl) );
		}
#line 11700 "Parser/parser.cc"
    break;

  case 796:
#line 3147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 11706 "Parser/parser.cc"
    break;

  case 797:
#line 3150 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 11712 "Parser/parser.cc"
    break;

  case 798:
#line 3153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 11718 "Parser/parser.cc"
    break;

  case 802:
#line 3164 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 11724 "Parser/parser.cc"
    break;

  case 803:
#line 3169 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11730 "Parser/parser.cc"
    break;

  case 804:
#line 3171 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = (yyvsp[-2].constant);
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 11740 "Parser/parser.cc"
    break;

  case 805:
#line 3180 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11746 "Parser/parser.cc"
    break;

  case 808:
#line 3187 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11752 "Parser/parser.cc"
    break;

  case 809:
#line 3192 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 11758 "Parser/parser.cc"
    break;

  case 811:
#line 3198 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11764 "Parser/parser.cc"
    break;

  case 812:
#line 3203 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11770 "Parser/parser.cc"
    break;

  case 813:
#line 3205 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 11776 "Parser/parser.cc"
    break;

  case 814:
#line 3207 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 11782 "Parser/parser.cc"
    break;

  case 819:
#line 3216 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 11788 "Parser/parser.cc"
    break;

  case 820:
#line 3218 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 11794 "Parser/parser.cc"
    break;

  case 821:
#line 3253 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11800 "Parser/parser.cc"
    break;

  case 822:
#line 3255 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11806 "Parser/parser.cc"
    break;

  case 823:
#line 3260 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11812 "Parser/parser.cc"
    break;

  case 825:
#line 3263 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11818 "Parser/parser.cc"
    break;

  case 826:
#line 3265 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11824 "Parser/parser.cc"
    break;

  case 827:
#line 3270 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11830 "Parser/parser.cc"
    break;

  case 828:
#line 3272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11836 "Parser/parser.cc"
    break;

  case 829:
#line 3274 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11842 "Parser/parser.cc"
    break;

  case 830:
#line 3276 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11848 "Parser/parser.cc"
    break;

  case 831:
#line 3281 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11854 "Parser/parser.cc"
    break;

  case 832:
#line 3283 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11860 "Parser/parser.cc"
    break;

  case 833:
#line 3285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11866 "Parser/parser.cc"
    break;

  case 834:
#line 3287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11872 "Parser/parser.cc"
    break;

  case 835:
#line 3289 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11878 "Parser/parser.cc"
    break;

  case 836:
#line 3291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11884 "Parser/parser.cc"
    break;

  case 837:
#line 3293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11890 "Parser/parser.cc"
    break;

  case 838:
#line 3298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11896 "Parser/parser.cc"
    break;

  case 839:
#line 3300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 11902 "Parser/parser.cc"
    break;

  case 840:
#line 3302 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11908 "Parser/parser.cc"
    break;

  case 841:
#line 3304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11914 "Parser/parser.cc"
    break;

  case 842:
#line 3313 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11920 "Parser/parser.cc"
    break;

  case 844:
#line 3316 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11926 "Parser/parser.cc"
    break;

  case 845:
#line 3321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11932 "Parser/parser.cc"
    break;

  case 846:
#line 3323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11938 "Parser/parser.cc"
    break;

  case 847:
#line 3325 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 11944 "Parser/parser.cc"
    break;

  case 848:
#line 3327 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11950 "Parser/parser.cc"
    break;

  case 849:
#line 3329 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11956 "Parser/parser.cc"
    break;

  case 850:
#line 3334 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11962 "Parser/parser.cc"
    break;

  case 851:
#line 3336 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11968 "Parser/parser.cc"
    break;

  case 852:
#line 3338 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11974 "Parser/parser.cc"
    break;

  case 853:
#line 3340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11980 "Parser/parser.cc"
    break;

  case 854:
#line 3345 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11986 "Parser/parser.cc"
    break;

  case 855:
#line 3347 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11992 "Parser/parser.cc"
    break;

  case 856:
#line 3349 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11998 "Parser/parser.cc"
    break;

  case 857:
#line 3351 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12004 "Parser/parser.cc"
    break;

  case 858:
#line 3353 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12010 "Parser/parser.cc"
    break;

  case 859:
#line 3355 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12016 "Parser/parser.cc"
    break;

  case 863:
#line 3373 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 12022 "Parser/parser.cc"
    break;

  case 864:
#line 3375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12028 "Parser/parser.cc"
    break;

  case 865:
#line 3377 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 12034 "Parser/parser.cc"
    break;

  case 866:
#line 3379 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12040 "Parser/parser.cc"
    break;

  case 867:
#line 3381 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12046 "Parser/parser.cc"
    break;

  case 868:
#line 3386 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12052 "Parser/parser.cc"
    break;

  case 869:
#line 3388 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12058 "Parser/parser.cc"
    break;

  case 870:
#line 3390 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12064 "Parser/parser.cc"
    break;

  case 871:
#line 3392 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12070 "Parser/parser.cc"
    break;

  case 872:
#line 3397 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12076 "Parser/parser.cc"
    break;

  case 873:
#line 3399 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12082 "Parser/parser.cc"
    break;

  case 874:
#line 3401 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12088 "Parser/parser.cc"
    break;

  case 875:
#line 3403 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12094 "Parser/parser.cc"
    break;

  case 876:
#line 3405 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12100 "Parser/parser.cc"
    break;

  case 877:
#line 3407 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12106 "Parser/parser.cc"
    break;

  case 878:
#line 3422 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "ID" );
		}
#line 12115 "Parser/parser.cc"
    break;

  case 879:
#line 3427 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12121 "Parser/parser.cc"
    break;

  case 880:
#line 3432 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12127 "Parser/parser.cc"
    break;

  case 882:
#line 3435 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12133 "Parser/parser.cc"
    break;

  case 883:
#line 3437 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12139 "Parser/parser.cc"
    break;

  case 884:
#line 3442 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12145 "Parser/parser.cc"
    break;

  case 885:
#line 3444 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12151 "Parser/parser.cc"
    break;

  case 886:
#line 3446 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12157 "Parser/parser.cc"
    break;

  case 887:
#line 3448 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12163 "Parser/parser.cc"
    break;

  case 888:
#line 3453 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12169 "Parser/parser.cc"
    break;

  case 889:
#line 3455 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12175 "Parser/parser.cc"
    break;

  case 890:
#line 3457 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12181 "Parser/parser.cc"
    break;

  case 891:
#line 3459 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12187 "Parser/parser.cc"
    break;

  case 892:
#line 3461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12193 "Parser/parser.cc"
    break;

  case 893:
#line 3463 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12199 "Parser/parser.cc"
    break;

  case 894:
#line 3465 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12205 "Parser/parser.cc"
    break;

  case 895:
#line 3470 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12211 "Parser/parser.cc"
    break;

  case 896:
#line 3472 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12217 "Parser/parser.cc"
    break;

  case 897:
#line 3474 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 12223 "Parser/parser.cc"
    break;

  case 898:
#line 3476 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12229 "Parser/parser.cc"
    break;

  case 899:
#line 3478 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12235 "Parser/parser.cc"
    break;

  case 900:
#line 3488 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12241 "Parser/parser.cc"
    break;

  case 901:
#line 3490 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12247 "Parser/parser.cc"
    break;

  case 903:
#line 3493 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12253 "Parser/parser.cc"
    break;

  case 904:
#line 3495 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12259 "Parser/parser.cc"
    break;

  case 905:
#line 3500 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12265 "Parser/parser.cc"
    break;

  case 906:
#line 3502 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12271 "Parser/parser.cc"
    break;

  case 907:
#line 3504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12277 "Parser/parser.cc"
    break;

  case 908:
#line 3509 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12283 "Parser/parser.cc"
    break;

  case 909:
#line 3511 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12289 "Parser/parser.cc"
    break;

  case 910:
#line 3513 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12295 "Parser/parser.cc"
    break;

  case 911:
#line 3515 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12301 "Parser/parser.cc"
    break;

  case 912:
#line 3520 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12307 "Parser/parser.cc"
    break;

  case 913:
#line 3522 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12313 "Parser/parser.cc"
    break;

  case 914:
#line 3524 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12319 "Parser/parser.cc"
    break;

  case 915:
#line 3538 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12325 "Parser/parser.cc"
    break;

  case 916:
#line 3540 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12331 "Parser/parser.cc"
    break;

  case 918:
#line 3543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12337 "Parser/parser.cc"
    break;

  case 919:
#line 3545 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12343 "Parser/parser.cc"
    break;

  case 920:
#line 3550 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12349 "Parser/parser.cc"
    break;

  case 921:
#line 3552 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12355 "Parser/parser.cc"
    break;

  case 922:
#line 3557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12361 "Parser/parser.cc"
    break;

  case 923:
#line 3559 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12367 "Parser/parser.cc"
    break;

  case 924:
#line 3561 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12373 "Parser/parser.cc"
    break;

  case 925:
#line 3566 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12379 "Parser/parser.cc"
    break;

  case 926:
#line 3568 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12385 "Parser/parser.cc"
    break;

  case 927:
#line 3573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12391 "Parser/parser.cc"
    break;

  case 928:
#line 3575 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12397 "Parser/parser.cc"
    break;

  case 930:
#line 3593 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12403 "Parser/parser.cc"
    break;

  case 931:
#line 3595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12409 "Parser/parser.cc"
    break;

  case 932:
#line 3600 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( 0, (yyvsp[0].op) ); }
#line 12415 "Parser/parser.cc"
    break;

  case 933:
#line 3602 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 12421 "Parser/parser.cc"
    break;

  case 934:
#line 3604 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12427 "Parser/parser.cc"
    break;

  case 935:
#line 3606 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12433 "Parser/parser.cc"
    break;

  case 936:
#line 3608 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12439 "Parser/parser.cc"
    break;

  case 938:
#line 3614 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12445 "Parser/parser.cc"
    break;

  case 939:
#line 3616 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12451 "Parser/parser.cc"
    break;

  case 940:
#line 3618 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12457 "Parser/parser.cc"
    break;

  case 941:
#line 3623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 12463 "Parser/parser.cc"
    break;

  case 942:
#line 3625 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12469 "Parser/parser.cc"
    break;

  case 943:
#line 3627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12475 "Parser/parser.cc"
    break;

  case 944:
#line 3633 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false ); }
#line 12481 "Parser/parser.cc"
    break;

  case 945:
#line 3635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false )->addArray( (yyvsp[0].decl) ); }
#line 12487 "Parser/parser.cc"
    break;

  case 946:
#line 3637 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-4].en), 0, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].en), 0, false ) ); }
#line 12493 "Parser/parser.cc"
    break;

  case 948:
#line 3644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), 0, false ); }
#line 12499 "Parser/parser.cc"
    break;

  case 949:
#line 3646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 12505 "Parser/parser.cc"
    break;

  case 950:
#line 3648 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newArray( (yyvsp[-2].en), 0, false ) ); }
#line 12511 "Parser/parser.cc"
    break;

  case 951:
#line 3650 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 12517 "Parser/parser.cc"
    break;

  case 952:
#line 3684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12523 "Parser/parser.cc"
    break;

  case 955:
#line 3691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 12529 "Parser/parser.cc"
    break;

  case 956:
#line 3693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12535 "Parser/parser.cc"
    break;

  case 957:
#line 3695 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12541 "Parser/parser.cc"
    break;

  case 958:
#line 3700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].op) ); }
#line 12547 "Parser/parser.cc"
    break;

  case 959:
#line 3702 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 12553 "Parser/parser.cc"
    break;

  case 960:
#line 3704 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12559 "Parser/parser.cc"
    break;

  case 961:
#line 3706 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12565 "Parser/parser.cc"
    break;

  case 962:
#line 3708 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12571 "Parser/parser.cc"
    break;

  case 964:
#line 3714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12577 "Parser/parser.cc"
    break;

  case 965:
#line 3716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12583 "Parser/parser.cc"
    break;

  case 966:
#line 3718 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12589 "Parser/parser.cc"
    break;

  case 967:
#line 3723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 12595 "Parser/parser.cc"
    break;

  case 968:
#line 3725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12601 "Parser/parser.cc"
    break;

  case 969:
#line 3727 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12607 "Parser/parser.cc"
    break;

  case 971:
#line 3734 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12613 "Parser/parser.cc"
    break;

  case 973:
#line 3745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false ); }
#line 12619 "Parser/parser.cc"
    break;

  case 974:
#line 3748 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 12625 "Parser/parser.cc"
    break;

  case 975:
#line 3750 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, (yyvsp[-2].decl), false ); }
#line 12631 "Parser/parser.cc"
    break;

  case 976:
#line 3753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 12637 "Parser/parser.cc"
    break;

  case 977:
#line 3755 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 12643 "Parser/parser.cc"
    break;

  case 978:
#line 3757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-4].decl), true ); }
#line 12649 "Parser/parser.cc"
    break;

  case 980:
#line 3772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12655 "Parser/parser.cc"
    break;

  case 981:
#line 3774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12661 "Parser/parser.cc"
    break;

  case 982:
#line 3779 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( 0, (yyvsp[0].op) ); }
#line 12667 "Parser/parser.cc"
    break;

  case 983:
#line 3781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 12673 "Parser/parser.cc"
    break;

  case 984:
#line 3783 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12679 "Parser/parser.cc"
    break;

  case 985:
#line 3785 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12685 "Parser/parser.cc"
    break;

  case 986:
#line 3787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12691 "Parser/parser.cc"
    break;

  case 988:
#line 3793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12697 "Parser/parser.cc"
    break;

  case 989:
#line 3795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12703 "Parser/parser.cc"
    break;

  case 990:
#line 3797 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12709 "Parser/parser.cc"
    break;

  case 991:
#line 3802 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12715 "Parser/parser.cc"
    break;

  case 992:
#line 3804 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12721 "Parser/parser.cc"
    break;

  case 995:
#line 3814 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12727 "Parser/parser.cc"
    break;

  case 998:
#line 3825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12733 "Parser/parser.cc"
    break;

  case 999:
#line 3827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12739 "Parser/parser.cc"
    break;

  case 1000:
#line 3829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12745 "Parser/parser.cc"
    break;

  case 1001:
#line 3831 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12751 "Parser/parser.cc"
    break;

  case 1002:
#line 3833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12757 "Parser/parser.cc"
    break;

  case 1003:
#line 3835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12763 "Parser/parser.cc"
    break;

  case 1004:
#line 3842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 12769 "Parser/parser.cc"
    break;

  case 1005:
#line 3844 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12775 "Parser/parser.cc"
    break;

  case 1006:
#line 3846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 12781 "Parser/parser.cc"
    break;

  case 1007:
#line 3848 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 12787 "Parser/parser.cc"
    break;

  case 1008:
#line 3850 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12793 "Parser/parser.cc"
    break;

  case 1009:
#line 3853 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 12799 "Parser/parser.cc"
    break;

  case 1010:
#line 3855 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12805 "Parser/parser.cc"
    break;

  case 1011:
#line 3857 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 12811 "Parser/parser.cc"
    break;

  case 1012:
#line 3859 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 12817 "Parser/parser.cc"
    break;

  case 1013:
#line 3861 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12823 "Parser/parser.cc"
    break;

  case 1014:
#line 3866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 12829 "Parser/parser.cc"
    break;

  case 1015:
#line 3868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 12835 "Parser/parser.cc"
    break;

  case 1016:
#line 3873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 12841 "Parser/parser.cc"
    break;

  case 1017:
#line 3875 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl)->addQualifiers( (yyvsp[-4].decl) ), true ); }
#line 12847 "Parser/parser.cc"
    break;

  case 1019:
#line 3902 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12853 "Parser/parser.cc"
    break;

  case 1023:
#line 3913 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12859 "Parser/parser.cc"
    break;

  case 1024:
#line 3915 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12865 "Parser/parser.cc"
    break;

  case 1025:
#line 3917 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12871 "Parser/parser.cc"
    break;

  case 1026:
#line 3919 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12877 "Parser/parser.cc"
    break;

  case 1027:
#line 3921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12883 "Parser/parser.cc"
    break;

  case 1028:
#line 3923 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12889 "Parser/parser.cc"
    break;

  case 1029:
#line 3930 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12895 "Parser/parser.cc"
    break;

  case 1030:
#line 3932 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12901 "Parser/parser.cc"
    break;

  case 1031:
#line 3934 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12907 "Parser/parser.cc"
    break;

  case 1032:
#line 3936 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12913 "Parser/parser.cc"
    break;

  case 1033:
#line 3938 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12919 "Parser/parser.cc"
    break;

  case 1034:
#line 3940 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12925 "Parser/parser.cc"
    break;

  case 1035:
#line 3945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 12931 "Parser/parser.cc"
    break;

  case 1036:
#line 3947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 12937 "Parser/parser.cc"
    break;

  case 1037:
#line 3949 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 12943 "Parser/parser.cc"
    break;

  case 1038:
#line 3956 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 12949 "Parser/parser.cc"
    break;

  case 1039:
#line 3958 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 12955 "Parser/parser.cc"
    break;

  case 1042:
#line 3982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 12961 "Parser/parser.cc"
    break;

  case 1043:
#line 3984 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 12967 "Parser/parser.cc"
    break;


#line 12971 "Parser/parser.cc"

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
#line 3987 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "make install" //
// End: //
