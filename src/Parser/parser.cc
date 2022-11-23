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
#line 281 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"

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

#line 682 "Parser/parser.cc"

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
#define YYLAST   22801

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  177
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  296
/* YYNRULES -- Number of rules.  */
#define YYNRULES  1056
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  2133

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
       0,   583,   583,   587,   594,   595,   596,   597,   598,   602,
     603,   604,   605,   606,   607,   608,   612,   613,   617,   618,
     623,   627,   628,   639,   641,   643,   647,   648,   650,   652,
     654,   656,   666,   668,   670,   672,   674,   676,   681,   682,
     692,   697,   702,   703,   708,   714,   716,   718,   724,   726,
     730,   732,   734,   736,   738,   740,   742,   744,   746,   748,
     750,   752,   754,   756,   758,   760,   770,   771,   775,   776,
     781,   784,   788,   789,   793,   794,   796,   798,   800,   802,
     804,   809,   811,   813,   821,   822,   830,   833,   834,   836,
     841,   857,   859,   861,   863,   865,   867,   869,   871,   873,
     881,   882,   884,   888,   889,   890,   891,   895,   896,   898,
     900,   902,   904,   906,   908,   910,   917,   918,   919,   920,
     924,   925,   929,   930,   935,   936,   938,   940,   945,   946,
     948,   953,   954,   956,   961,   962,   964,   966,   968,   973,
     974,   976,   981,   982,   987,   988,   993,   994,   999,  1000,
    1005,  1006,  1011,  1012,  1015,  1020,  1025,  1026,  1034,  1040,
    1041,  1045,  1046,  1050,  1051,  1055,  1056,  1057,  1058,  1059,
    1060,  1061,  1062,  1063,  1064,  1065,  1075,  1077,  1082,  1083,
    1085,  1087,  1092,  1093,  1099,  1100,  1106,  1107,  1108,  1109,
    1110,  1111,  1112,  1113,  1114,  1115,  1116,  1118,  1119,  1125,
    1127,  1137,  1139,  1147,  1148,  1153,  1155,  1157,  1159,  1161,
    1165,  1166,  1168,  1173,  1175,  1182,  1184,  1186,  1196,  1198,
    1200,  1205,  1210,  1213,  1218,  1220,  1222,  1224,  1232,  1233,
    1235,  1239,  1241,  1245,  1247,  1248,  1250,  1252,  1257,  1258,
    1262,  1267,  1268,  1272,  1274,  1279,  1281,  1286,  1288,  1290,
    1292,  1297,  1299,  1301,  1303,  1308,  1310,  1315,  1316,  1338,
    1340,  1342,  1345,  1347,  1350,  1352,  1355,  1357,  1362,  1367,
    1369,  1374,  1379,  1381,  1383,  1385,  1387,  1390,  1392,  1395,
    1397,  1402,  1408,  1411,  1413,  1418,  1424,  1426,  1431,  1437,
    1440,  1442,  1445,  1447,  1452,  1459,  1461,  1466,  1472,  1474,
    1479,  1485,  1488,  1493,  1501,  1503,  1505,  1510,  1512,  1517,
    1518,  1520,  1525,  1527,  1532,  1534,  1536,  1538,  1541,  1545,
    1548,  1552,  1554,  1556,  1558,  1560,  1562,  1564,  1566,  1568,
    1570,  1572,  1577,  1578,  1582,  1588,  1593,  1598,  1599,  1603,
    1607,  1612,  1613,  1619,  1623,  1625,  1627,  1629,  1632,  1634,
    1639,  1641,  1646,  1648,  1650,  1655,  1657,  1663,  1664,  1668,
    1669,  1670,  1671,  1675,  1680,  1681,  1683,  1685,  1687,  1691,
    1695,  1696,  1700,  1702,  1704,  1706,  1708,  1714,  1715,  1721,
    1722,  1726,  1727,  1732,  1734,  1740,  1741,  1743,  1748,  1753,
    1764,  1765,  1769,  1770,  1776,  1777,  1781,  1783,  1787,  1789,
    1793,  1794,  1798,  1799,  1803,  1810,  1811,  1815,  1817,  1832,
    1833,  1834,  1835,  1837,  1841,  1843,  1847,  1854,  1856,  1858,
    1863,  1864,  1866,  1868,  1870,  1902,  1905,  1910,  1912,  1918,
    1923,  1928,  1939,  1944,  1949,  1954,  1959,  1968,  1972,  1979,
    1981,  1982,  1983,  1989,  1991,  1996,  1997,  1998,  2007,  2008,
    2009,  2013,  2014,  2021,  2030,  2031,  2032,  2037,  2038,  2047,
    2048,  2053,  2054,  2058,  2060,  2062,  2064,  2066,  2070,  2075,
    2076,  2078,  2088,  2089,  2094,  2096,  2098,  2100,  2102,  2104,
    2107,  2109,  2111,  2116,  2118,  2120,  2122,  2124,  2126,  2128,
    2130,  2132,  2134,  2136,  2138,  2140,  2142,  2144,  2146,  2148,
    2150,  2152,  2154,  2156,  2158,  2160,  2162,  2164,  2166,  2168,
    2170,  2175,  2176,  2180,  2187,  2188,  2194,  2195,  2197,  2199,
    2201,  2206,  2208,  2213,  2214,  2216,  2218,  2223,  2225,  2227,
    2229,  2231,  2233,  2238,  2245,  2247,  2249,  2254,  2262,  2261,
    2265,  2273,  2274,  2276,  2278,  2283,  2284,  2286,  2291,  2292,
    2294,  2296,  2301,  2302,  2304,  2309,  2311,  2313,  2315,  2316,
    2318,  2323,  2325,  2327,  2332,  2339,  2343,  2344,  2349,  2348,
    2353,  2352,  2371,  2370,  2382,  2381,  2392,  2397,  2398,  2403,
    2409,  2423,  2424,  2428,  2430,  2432,  2438,  2440,  2442,  2444,
    2446,  2448,  2450,  2452,  2458,  2459,  2464,  2473,  2475,  2484,
    2486,  2487,  2488,  2490,  2492,  2493,  2498,  2499,  2500,  2505,
    2507,  2510,  2517,  2518,  2519,  2525,  2530,  2532,  2538,  2539,
    2545,  2546,  2550,  2555,  2558,  2557,  2561,  2564,  2571,  2576,
    2575,  2584,  2589,  2594,  2599,  2604,  2605,  2610,  2612,  2617,
    2619,  2621,  2623,  2628,  2629,  2635,  2636,  2637,  2644,  2645,
    2647,  2648,  2649,  2651,  2653,  2660,  2661,  2663,  2665,  2670,
    2671,  2677,  2678,  2680,  2681,  2686,  2687,  2688,  2690,  2698,
    2699,  2701,  2704,  2706,  2710,  2711,  2712,  2714,  2716,  2721,
    2723,  2728,  2730,  2739,  2741,  2746,  2747,  2748,  2752,  2753,
    2754,  2759,  2760,  2765,  2766,  2767,  2768,  2772,  2773,  2778,
    2779,  2780,  2781,  2782,  2796,  2797,  2802,  2803,  2809,  2811,
    2814,  2816,  2818,  2841,  2842,  2848,  2849,  2855,  2854,  2864,
    2863,  2867,  2873,  2879,  2880,  2882,  2886,  2891,  2893,  2895,
    2897,  2903,  2904,  2908,  2909,  2914,  2916,  2923,  2925,  2926,
    2928,  2933,  2935,  2937,  2942,  2944,  2949,  2954,  2962,  2964,
    2969,  2970,  2975,  2976,  2980,  2981,  2982,  2987,  2989,  2995,
    2997,  3002,  3004,  3010,  3011,  3015,  3019,  3023,  3025,  3026,
    3028,  3030,  3032,  3034,  3036,  3038,  3039,  3044,  3047,  3046,
    3058,  3057,  3070,  3069,  3081,  3080,  3092,  3091,  3105,  3111,
    3113,  3119,  3120,  3131,  3138,  3143,  3149,  3152,  3155,  3159,
    3165,  3168,  3171,  3176,  3177,  3178,  3182,  3188,  3189,  3199,
    3200,  3204,  3205,  3210,  3215,  3216,  3222,  3223,  3225,  3230,
    3231,  3232,  3233,  3234,  3236,  3271,  3273,  3278,  3280,  3281,
    3283,  3288,  3290,  3292,  3294,  3299,  3301,  3303,  3305,  3307,
    3309,  3311,  3316,  3318,  3320,  3322,  3331,  3333,  3334,  3339,
    3341,  3343,  3345,  3347,  3352,  3354,  3356,  3358,  3363,  3365,
    3367,  3369,  3371,  3373,  3385,  3386,  3387,  3391,  3393,  3395,
    3397,  3399,  3404,  3406,  3408,  3410,  3415,  3417,  3419,  3421,
    3423,  3425,  3440,  3445,  3450,  3452,  3453,  3455,  3460,  3462,
    3464,  3466,  3471,  3473,  3475,  3477,  3479,  3481,  3483,  3488,
    3490,  3492,  3494,  3496,  3506,  3508,  3510,  3511,  3513,  3518,
    3520,  3522,  3527,  3529,  3531,  3533,  3538,  3540,  3542,  3556,
    3558,  3560,  3561,  3563,  3568,  3570,  3575,  3577,  3579,  3584,
    3586,  3591,  3593,  3610,  3611,  3613,  3618,  3620,  3622,  3624,
    3626,  3631,  3632,  3634,  3636,  3641,  3643,  3645,  3651,  3653,
    3656,  3659,  3661,  3665,  3667,  3669,  3670,  3672,  3674,  3678,
    3680,  3685,  3687,  3689,  3691,  3726,  3727,  3731,  3732,  3734,
    3736,  3741,  3743,  3745,  3747,  3749,  3754,  3755,  3757,  3759,
    3764,  3766,  3768,  3774,  3775,  3777,  3786,  3789,  3791,  3794,
    3796,  3798,  3812,  3813,  3815,  3820,  3822,  3824,  3826,  3828,
    3833,  3834,  3836,  3838,  3843,  3845,  3853,  3854,  3855,  3860,
    3861,  3866,  3868,  3870,  3872,  3874,  3876,  3883,  3885,  3887,
    3889,  3891,  3894,  3896,  3898,  3900,  3902,  3907,  3909,  3911,
    3916,  3942,  3943,  3945,  3949,  3950,  3954,  3956,  3958,  3960,
    3962,  3964,  3971,  3973,  3975,  3977,  3979,  3981,  3986,  3988,
    3990,  3997,  3999,  4017,  4019,  4024,  4025
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
  "type_ptr", "type_array", "type_function",
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
     125,    40,    64,    41,    46,    91,    93,    44,    58,   123,
      96,    94,    42,    38,    43,    45,    33,   126,    92,    47,
      37,    60,    62,   124,    63,    61,    59
};
# endif

#define YYPACT_NINF (-1775)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1055)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     212, 12111,   323,   372, 16705,    52, -1775, -1775, -1775, -1775,
   -1775, -1775, -1775, -1775, -1775, -1775, -1775, -1775,   252,   919,
     309, -1775, -1775, -1775, -1775, -1775, -1775, -1775, -1775, -1775,
   -1775, -1775, -1775, -1775, -1775, -1775, -1775, -1775, -1775, -1775,
   -1775, -1775, -1775, -1775, -1775, -1775, -1775, -1775,    36,   446,
   -1775, -1775, -1775, -1775, -1775, -1775,  3725,  3725,   338, 12111,
     349,   389, 22532, -1775,   395, -1775, -1775, -1775, -1775, -1775,
   -1775, -1775, -1775, -1775,  2652, -1775,   772,   358, -1775, -1775,
   -1775, -1775, -1775, 16554, -1775, -1775,   350,   417,   476,   399,
   -1775,  3725,   417,   417,   417,   457,  2349,   610,   787, 12272,
   -1775, -1775, -1775, 16403,  2127, -1775, -1775, -1775,  2763,   624,
    7875,  1265,   904,  2763,   945,   489, -1775, -1775, -1775, -1775,
     589, -1775, -1775, -1775, -1775,   510, -1775, -1775, -1775, -1775,
   -1775,   522,   540,   589, -1775,   589,   563, -1775, -1775, -1775,
   17709,  3725, -1775, -1775,  3725, -1775, 12111, -1775,   506, 17860,
   -1775, -1775,  4402, 19272, -1775,   869,   869,   637,  2830, -1775,
   -1775, -1775, -1775,   299, 14011,  2775,   589, -1775, -1775, -1775,
   -1775, -1775, -1775,   605, -1775,   640,   665,   692, -1775,   752,
   21930, -1775, -1775, -1775, -1775, -1775, -1775, -1775, 15327,  3302,
    2652,   436,   744,   749,   760,   778,   810,   843, -1775, -1775,
   18011, 11128,   765, -1775, 17149, -1775, -1775, -1775, -1775,   794,
   -1775, -1775,   847, -1775,  7283,   873, 20178, -1775,   861,  3725,
     540,   864,   876,   889,   891, -1775, -1775, -1775,  3116,  2038,
     899,   968,    66, -1775, -1775,   589,   589,    -9,    91,   136,
      -9, -1775,   589,   589, -1775,  4833, -1775, -1775,   970,   972,
     869,  5819, -1775, 16554, -1775, -1775,  2763, -1775,  2260,   489,
     916,  1005,    91,  3725,   476, -1775, 13531, -1775,   869,   869,
     971,  1005,    91,  3725, -1775, 22678, -1775, -1775,   869, -1775,
     869, -1775,   830,  3763,  3725, -1775,  1701,   950, -1775, -1775,
   -1775, 16217,   540,   262, -1775, -1775, 19416, -1775,   968,    95,
   -1775, 21930, 19272,  3618,  4833, -1775,   143, -1775, -1775, -1775,
   17860,  3725, -1775,   974, -1775, -1775, -1775, -1775,  3725,  3237,
     474,   588, -1775,  3725,   640, -1775,   799,   589,   589,   982,
   18162,   792, 14491, 19467,  2763,  2763, -1775,  2763,   869,  2763,
     869, -1775, -1775,   589, -1775,   993, -1775, 18313, -1775, -1775,
   -1775, 18464,   794, -1775,   296,   600,   274,   558,   489,  1042,
   -1775,  2830,   973,   640,  2830,  1044, -1775,  1006,  1113, 22003,
    1081,  1086,  1092, 21930, 22076,  1094, 22583, -1775, -1775, -1775,
   -1775, -1775, -1775, 22149, 22149, 15172,  1083,  4021, -1775, -1775,
   -1775, -1775,   100, -1775,   598, -1775,  1316, -1775, 21930, 21930,
   -1775,  1096,   622,   801,   853,   611,  1032,  1090,  1128,  1131,
    1136,   188, -1775,   675, -1775,  1147, -1775,   897,  3906, 15637,
   -1775, -1775,   565,  1147, -1775, -1775,   713, -1775, -1775,  3302,
    1169,  1171,  1173,  1175,  1181,  1183, -1775, -1775,   344,  1188,
   -1775,   854,  1188, -1775, -1775, 17709, -1775,  1022,  1182, 15792,
   -1775, -1775,  4518,  4121,  1221, 14491,  1225,   678,   697, -1775,
   -1775, -1775, -1775, -1775,  3725,  4624, -1775, -1775, -1775, -1775,
   -1775, -1775, 16111,  3853,  1083,  7283,  1194,  1212, -1775, -1775,
    1220, 20178,   865, -1775, -1775, -1775, 20251,  1236, -1775, -1775,
   -1775, -1775, -1775,  3116,   773,  1237,  1247,  1250,   791,  1252,
    1257,  1259,  2038, -1775, -1775,   589,  1297,   476,  1298, -1775,
   -1775,  1305, -1775, -1775,   540,  1005, -1775, -1775, -1775,   540,
   -1775, -1775,  4833, -1775, 15637, 15637, -1775,   869,  4402,  9705,
   14651, -1775, -1775, -1775, -1775, -1775,   540,  1005,    95, -1775,
   -1775,  2763,  1307,  1005,    91, -1775,   540,  1005, -1775, 22729,
   -1775,   869,   869, -1775, -1775,  1315,   217,  1318,   489,  1323,
   -1775, 16865, -1775,   736, -1775,  1426, 19863, -1775,  4402, 16305,
    5819, -1775, 16217, 22222, -1775, -1775, -1775, -1775, -1775,  3618,
     875,  4833, -1775, 14651,   968, 12111, -1775,  1341, -1775,  1348,
   -1775, -1775, -1775, -1775, -1775,  2830, -1775, -1775,  1422,  4460,
    3383, 18464, 11128, -1775, 18615, -1775,   869,   869, -1775, -1775,
     794, -1775,   844,  1347,  1486, 21930,   839,  1305,  1330, -1775,
     589,   589, -1775,  1188, -1775, 18162, -1775, -1775, 17309,   869,
     869, -1775,  4460,   589, -1775, 19128, -1775, -1775, 18313, -1775,
     299, -1775, -1775, -1775,  1350,  3725,  1042,  1356,   872, 17860,
     888, -1775, -1775, -1775, -1775, -1775, -1775,   898, -1775,  1373,
    1351, -1775, 15482, -1775,  4021, 18766, 18766, -1775, 15482, -1775,
   21930, -1775, -1775, -1775, -1775, -1775, -1775, 15482, -1775, -1775,
   17407, 18766, 18766,   897,  1266,  1429,   566,  1505, -1775,   915,
    1377,   943,  1388, -1775, 20251, 21930, 20324,  1367, 21930,  1701,
   21930,  1701, -1775,  1501, -1775, -1775, 20397,  2738, 21930, 20397,
    1701, -1775, -1775, 21930, 21930, 21930, 21930, 21930, 21930, 21930,
   21930, 21930, 21930, 21930, 21930, 21930, 21930, 21930, 21930, 21930,
   21930, 21930, 20470,  1366,   752,  4134, 11128, -1775, -1775, -1775,
   -1775, -1775, -1775, -1775, -1775, -1775, -1775, -1775,  1384, 21930,
   -1775, -1775,   565,  2267, -1775, -1775,   589,   589, -1775, -1775,
   15637, -1775,   351,  1188, -1775,   925,  1188, -1775, -1775, -1775,
    1305, -1775, -1775,  1305, 22295, -1775, -1775, 11128,  1389,  1394,
    3404,  1533,  3087,   362,  1330, -1775,   589,   589,  1330,   377,
   -1775,   589,   589, 21930,  3725,  1043,  1067,  1330,   178, 13851,
   13851,  3725, -1775, -1775, 21930,  1220, -1775,  7283,  1402, -1775,
    2467, -1775, -1775, -1775, -1775, -1775,   924, -1775, 13851,  1701,
    4402,  1701,   927,  1400,  1405,  1408,   941,  1416,  1421,  1423,
     424,  1188, -1775, -1775,   437,  1188, -1775, -1775, -1775,  4402,
     752, -1775,  1188, 19560, -1775,   540, 16865, -1775, -1775,   963,
    1424,   965,  1425, -1775,  1431, -1775,   540, -1775, -1775,   540,
    1005,  1431, -1775,   540,  1420,  1427,  1430, -1775, -1775, 17309,
   -1775,  1438, -1775, -1775, -1775,  1701,  3725, 10277,  1512,  1399,
   18926, -1775,  1182, -1775, 13851,   990, -1775, -1775,  1431, -1775,
   17860, 15637,  1417, -1775,  1417, -1775, -1775, -1775,   274,   589,
     589, -1775, 18313, -1775, 11292, 15947, -1775, 16865,  1444,  1447,
    1448, -1775,  5894,   589, -1775,   839, -1775, -1775, -1775, -1775,
    1305, -1775, -1775, -1775,   869, -1775,  3597, -1775, -1775,   489,
     282,  1452,  1432,  1461,   274, -1775, -1775,  1462,  1469,  1044,
   20397, -1775,  1470,  1472,   358,  1473,  1474,  1478,  1476,  1481,
   21930,  1484,  1485,  1487, 11128, 21930, -1775, -1775,  1521, -1775,
   -1775, -1775, 21930, -1775,  1488,  1489, 20032,  1139, -1775, 20397,
    1483, -1775,  1490, -1775, -1775,  4436, -1775, -1775,  1017, -1775,
   -1775, -1775, -1775,  4436, -1775, -1775,  1159,   621, -1775, -1775,
    1096,  1096,  1096,   622,   622,   801,   801,   853,   853,   853,
     853,   611,   611,  1032,  1090,  1128,  1131,  1136, 21930,  1160,
   -1775,  1492,  4436, -1775, -1775,  7283, -1775, 16865,  1497,  1499,
    1502,  2267, -1775, -1775, -1775, -1775, -1775, -1775, -1775, -1775,
    1305, -1775, -1775,  1305, 16865, 16865, -1775, -1775,  3404,   881,
    1506,  1507,  1508,  1511,  2294,  3087, -1775, -1775, -1775, -1775,
   -1775, -1775, -1775, -1775, -1775, -1775, -1775, -1775, -1775, -1775,
    1498, -1775,  1330, -1775, -1775, -1775, -1775, -1775, -1775, -1775,
   -1775,  1514,  1515, -1775,   476,  4436,  1174,   211, -1775, -1775,
    1493, -1775, 20178, -1775, 21930,   589, 20543, 13851, -1775, -1775,
   -1775,  1451,   450,  1188, -1775,   451,  1188, -1775, -1775, -1775,
   -1775,  1305, -1775, -1775, -1775,  1305,   968,  1519,  1305,    57,
   -1775,  1147,  1517, -1775, -1775, -1775, -1775, -1775, -1775,  1523,
   -1775, -1775,  1431, -1775,   540, -1775, -1775, -1775, -1775, -1775,
   12909,  1525,  1524, -1775,   281, -1775,   383,   105, 10964,  1518,
   15008,  1532,  1535,  2200,  2574,  1665, 20616,  1538, -1775, -1775,
    1539,  1542, -1775, -1775,   540, 21930, 21930,  1644,  1526,   644,
   -1775,  1620,  1541,  1520, -1775, -1775, -1775, 10103, -1775, -1775,
   -1775, -1775, -1775,  1609, -1775, -1775, -1775,  1608, -1775, -1775,
   -1775,  1701, -1775, -1775, 12755, 16554,  1543, -1775,  3725, -1775,
    1527,  1544,  1553, -1775,  1176, -1775, -1775, -1775, -1775,  4402,
   -1775, -1775,  1534,  1537,  1035, 17860,   640,   640,  1350,  1042,
    1042, -1775, -1775,  1083,  1182, 15792, -1775,  1147, -1775, 11456,
   -1775,   470,  1188, -1775,   869,  7115, -1775, -1775,   274,   589,
     589,   299,  3725, -1775, 20689, -1775,   274,  1350,  1561, -1775,
   -1775,  1037,   645, 17309, 11128,  1701, -1775,   645, 17558,   645,
   -1775, 21930, 21930, 21930, -1775, -1775, -1775, -1775, 21930, 21930,
    1555,  7283, -1775, -1775,  1568,   679, -1775, -1775, -1775,  2811,
   -1775, -1775,  1184, -1775,   125, -1775, 20397,  1189, -1775, 20251,
   -1775, -1775, 21930,  1550,  1191,  1200,  1220, -1775,   509,  1188,
   -1775, -1775, 16865, 16865, -1775, -1775,  1576,   519,  1188, -1775,
     528,   976,   589,   589, -1775, -1775, 16865, 16865, -1775,  1574,
   -1775, 14651, 14651,  1580,  1577,  1578,  1584, -1775,  1582, 21930,
   21930,  1205,  1587, -1775, -1775, -1775, -1775, -1775, -1775, -1775,
    1594, 21930, -1775, -1775, -1775,  1305, -1775, -1775, -1775,  1305,
   16865, 16865,   476,   589, -1775, -1775,  1210, 21930, 19708,  1596,
    1595,  1590, -1775, -1775,  1600, 13063, 13217, 13371, 17860, 18766,
   18766,  1602, -1775,  1579,  1583,  2411,  9290, -1775,   321,  3725,
   -1775, -1775,  3725, -1775, 20105,   232,   453, -1775, -1775, -1775,
   -1775, 21930,  1603,  1681, 10799, 10451, -1775,  1586, -1775,  1589,
   21930,  1591,  7283,  1607, 21930, 20251, 21930,   949, -1775,  1610,
     101, -1775,    98,  1612, -1775, -1775,  1616, -1775,  1614, -1775,
    1615,  1643, 15008,   616, 13691,   589,   386, -1775, -1775, -1775,
    1605, -1775,  1645, -1775,  1650, -1775,  1649, -1775,  1651, -1775,
   -1775, -1775, -1775,  1652,  1642,  1657, 11620,  1662,  1666,  1668,
   -1775,  1680, -1775, -1775, -1775,  1305, 21930, 21930,  1182,  1679,
   -1775,  1350, -1775,  1042,   121,  1432,  7283, -1775,  1350,  1693,
   -1775, 17860, -1775,   887,  1694,  1691,  1038, -1775,  1692, -1775,
   -1775, -1775, -1775, -1775,  7283,  1220, 20251, -1775,  1732,  4436,
   -1775,  1732,  1732, -1775,  4436,  3698,  3778, -1775, -1775,  1216,
   -1775, -1775, -1775,  1706,  1704, -1775, -1775, -1775,  1305, -1775,
   -1775,  1712,  1714,   589, -1775, -1775, -1775,  1305, -1775, -1775,
   -1775,  1717, -1775, -1775, -1775, -1775, -1775, -1775, -1775, -1775,
   -1775, -1775, -1775, -1775, -1775,  1703, -1775, -1775, -1775, -1775,
    1716,  1722,   589, -1775, 16865, 16865, -1775, -1775, -1775, -1775,
   21930, -1775,    57, -1775,  1147, -1775, -1775, -1775,  1727, -1775,
    1602,  1602,  1602,  1058,  1702,   407, -1775,  2985,   409, 15637,
   -1775, -1775, -1775,  3988, 21930,  5314,   414, -1775, -1775,   159,
    1723,  1723,  3725, -1775, -1775, 17016, -1775, 21930,  1729,  1731,
   -1775, -1775, -1775, -1775,  1053,  1738, 15008,  1541,  1737, 21930,
     350,  1734,   457,  7548, 17860, -1775, -1775, -1775,   846, 15008,
   21930,   714,   604, -1775, 21930, 19880, -1775, -1775,   439, -1775,
    1220, -1775,  1069,  1076,  1085, -1775, -1775, -1775, -1775,   540,
     949,  1741, -1775, -1775, 21930, -1775,  1742,   752, 10964, -1775,
   -1775, -1775, -1775, 21930,  1788, -1775,  9929, -1775,   589, 14651,
   -1775, -1775, 17860, -1775, -1775, -1775,   274,   274, -1775, -1775,
   -1775,  1743, -1775, 16865, -1775, -1775,  1744, -1775,  1745,  1748,
    1042,  1746, -1775, -1775,  1220,  1753, -1775, -1775,  1751, -1775,
   -1775, 21930, -1775, 17558, 21930,  1220,  1756,  1219, -1775,  1227,
   -1775,  4436, -1775,  4436, -1775, -1775, -1775, -1775, 16865,  1754,
    1755, -1775, -1775, 16865, 16865,  1757,  1758,  1229, 14171, 14331,
   -1775,  1759, -1775, -1775, -1775, -1775,  1760,  1761,  1242, 21930,
   -1775, -1775, -1775, -1775,  1058,  2285,   481, -1775, -1775, -1775,
   -1775,   589,   589, -1775, -1775, -1775,   514, -1775,  1105,  3988,
     691, -1775,  5314,   589, -1775, -1775, -1775, -1775, -1775, -1775,
   -1775, -1775,   527, 15008,   438, 20762,  1843, 15008,  1541, 14811,
   -1775, -1775, -1775, -1775, 21930, -1775, 20835,  1844,  1749, 19956,
   20908, 15008, 10625,  1541,   763,  1089,  1752, 21930, -1775,  1770,
     467, 15008, -1775, -1775,  1780, -1775, -1775,  1762,   752,   652,
    1768,  1782,  1251,  1846, -1775, -1775, -1775, -1775,  3725,  4402,
    1350,  1350, -1775, -1775,  1784,  1785, -1775, -1775, -1775,  1783,
     274,  1794, -1775,  1795, -1775, -1775, -1775, -1775,  1797, -1775,
   -1775, -1775,  1267,  1272, -1775, -1775, -1775, -1775, -1775, -1775,
   -1775, -1775, -1775, -1775,  1796, -1775, -1775,  1798,  1799, -1775,
   -1775, -1775, -1775,  1801,  1805,  1807,  2285, -1775,   589, -1775,
   -1775, -1775, -1775, -1775,  1793,  2985, -1775, -1775,  2539,   111,
   11787, -1775, 14904, -1775,    77,  1106, 15008,  1875,   532,  1804,
     189, 15008, 21930,  1812,   763,  1089,  1800, 22368,  1808,   363,
    1907, -1775, 20981, 21054, 21930,  1541,  1809, 11950, -1775, -1775,
   -1775, 18977, -1775,  1825,  1810,    19, 15008, -1775, 21930, 20397,
     390, -1775, -1775, -1775,  1834,  1837,  1835, -1775, -1775,   274,
    1350, -1775, -1775, -1775, -1775, -1775,  1836,  1838,  1840, 14651,
    1839, -1775, -1775,   546,  1188, -1775, -1775,  1058, -1775, -1775,
     215, -1775,   169, -1775, -1775, -1775,  1845, 12433, -1775, -1775,
   15008, -1775,    87, -1775, 15008, 21930,  1842, 21127, -1775, -1775,
   21200, 21273, 21930,  1812,  1541, 21346, 21419, 15008,  1831,   511,
    1832,   568, -1775, -1775,  1850, 12433, 18977, -1775,  4197, 18615,
    1701,  1847, -1775,  1900,  1855,   709,  1851, -1775,  1935, -1775,
    1135, 15008,  1861, 15008, 15008, -1775, -1775, -1775,  1350,  1865,
   -1775, -1775, -1775, -1775, -1775, -1775, -1775,  1305, -1775, 21930,
   -1775, 21930, -1775, -1775,  1357, 12594, -1775, -1775, 15008, -1775,
   -1775,  1541, -1775, -1775,  1541,  1849,   597,  1852,   632, -1775,
   -1775,  1541, -1775,  1541, -1775,  1866, 21492, 21565, 21638, -1775,
    1357, -1775,  1841,  3011,  3369, -1775, -1775, -1775,    19,  1864,
   21930,  1848,    19,    19, 15008, -1775, -1775, 21930,  1917,  1918,
    1878, -1775, 16865, -1775, -1775, 14904, -1775,  1357, -1775, -1775,
    1879, 21711, 21784, 21857, -1775, -1775,  1541, -1775,  1541, -1775,
    1541, -1775,  1841, 21930,  1881,  3369,  1884,   752,  1893, -1775,
     738, -1775, -1775,  1137,  1846,   135, -1775, -1775, -1775,  9550,
    1898, 14904, -1775, -1775,  1541, -1775,  1541, -1775,  1541,  1899,
    1897, -1775,   540,   752,  1905, -1775,  1883,   752, -1775, -1775,
   15008,  1985,  1908, -1775, -1775, -1775,  9804, -1775,   540, -1775,
   -1775,  1293, 21930, -1775,  1153, -1775, 15008, -1775, -1775,   752,
    1701,  1910,  1892, -1775, -1775, -1775,  1154, -1775, -1775,  1894,
    1701, -1775, -1775
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
     496,   497,   498,   499,   500,   501,   508,   509,   809,   511,
     584,   585,   588,   590,   586,   592,     0,     0,     0,   457,
       0,     0,    16,   555,   561,     9,    10,    11,    12,    13,
      14,    15,   767,   102,     0,    19,     0,     2,   100,   101,
      17,    18,   825,   457,   768,   406,     0,   409,   693,   411,
     420,     0,   410,   440,   441,     0,     0,     0,     0,   538,
     459,   461,   467,   457,   469,   472,   523,   510,   445,   516,
     521,   446,   533,   447,   548,   552,   558,   537,   564,   576,
     809,   581,   582,   565,   634,   412,   413,     3,   775,   788,
     462,     0,     0,   809,   847,   809,     2,   864,   865,   866,
     457,     0,  1034,  1035,     0,     1,   457,    16,     0,   457,
     429,   430,     0,   538,   451,   452,   453,   778,     0,   587,
     589,   591,   593,     0,   457,     0,   810,   811,   583,   512,
     686,   687,   685,   746,   741,   731,     0,     0,   776,     0,
       0,   474,   769,   773,   774,   770,   771,   772,   457,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   556,   559,
     457,   457,     0,  1036,   538,   854,   872,  1040,  1033,  1031,
    1038,   405,     0,   164,   699,   163,     0,   414,     0,     0,
       0,     0,     0,     0,     0,   404,   924,   925,     0,     0,
     439,   807,   809,   803,   828,   809,   809,   805,     2,   809,
     804,   885,   809,   809,   882,     0,   531,   532,     0,     0,
     457,   457,     2,   457,   421,   460,   470,   524,     0,   553,
       0,   791,     2,     0,   693,   422,   538,   517,   534,   549,
       0,   791,     2,     0,   473,   518,   525,   526,   535,   540,
     550,   554,     0,   568,     0,   761,     2,     2,   789,   846,
     848,   457,     0,     2,     2,  1044,   538,  1047,   807,   807,
       3,     0,   538,     0,     0,   432,   809,   805,   804,     2,
     457,     0,   765,     0,   727,   729,   728,   730,     0,     0,
     723,     0,   713,     0,   722,   733,     0,   809,   809,     2,
     457,  1055,   458,   457,   469,   448,   516,   449,   541,   450,
     548,   545,   566,   809,   567,     0,   674,   457,   675,  1009,
    1010,   457,   676,   678,   555,   561,   635,   637,   638,   635,
     812,     0,   744,   732,     0,   816,    21,     0,    20,     0,
       0,     0,     0,     0,     0,     0,    23,    25,     4,     8,
       5,     6,     7,     0,     0,   457,     2,     0,   103,   104,
     105,   106,    87,    24,    88,    42,    86,   107,     0,     0,
     122,   124,   128,   131,   134,   139,   142,   144,   146,   148,
     150,   152,   155,     0,    26,     0,   562,     2,   107,   457,
     156,   738,   689,   552,   691,   737,     0,   688,   692,     0,
       0,     0,     0,     0,     0,     0,   826,   852,   809,   862,
     870,   874,   880,     2,  1042,   457,  1045,     2,   100,   457,
       3,   673,     0,  1055,     0,   458,   516,   541,   548,     3,
       3,   655,   659,   669,   675,   676,     2,   855,   873,  1032,
       2,     2,    23,     0,     2,   699,    24,     0,   697,   700,
    1053,     0,     0,   706,   695,   694,     0,     0,   793,     2,
       2,     2,     2,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   831,   888,   809,     0,   693,     2,   827,
     835,   952,   829,   830,     0,   791,     2,   884,   892,     0,
     886,   887,     0,   435,   457,   457,   522,   458,     0,   538,
     457,  1037,  1041,  1039,   539,   765,     0,   791,   807,   415,
     423,   471,     0,   791,     2,   765,     0,   791,   742,   519,
     520,   536,   551,   557,   560,   555,   561,   579,   580,     0,
     743,   457,   683,     0,   201,   398,   457,     3,     0,   538,
     457,   790,   457,     0,   417,     2,   418,   762,   437,     0,
       0,     0,     2,   457,   807,   457,   765,     0,     2,     0,
     726,   725,   724,   719,   468,     0,   717,   734,   514,     0,
       0,   457,   457,  1011,   458,   454,   455,   456,  1015,  1006,
    1007,  1013,     2,     2,   101,     0,   971,   985,  1055,   967,
     809,   809,   976,   983,   681,   457,   546,   677,   458,   542,
     543,   547,     0,   809,  1021,   458,  1026,  1018,   457,  1023,
       0,   644,   636,   643,  1053,     0,   635,     0,     0,   457,
       0,   824,   823,   819,   821,   822,   820,     0,   814,   817,
       0,    22,   457,    94,     0,   457,   457,    89,   457,    96,
       0,    32,    36,    37,    33,    34,    35,   457,    92,    93,
     457,   457,   457,     2,   103,   104,     0,     0,   182,     0,
       0,   582,     0,  1031,     0,     0,     0,     0,     0,     0,
       0,     0,    55,     0,    61,    62,    66,     0,     0,    66,
       0,    90,    91,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   457,   165,   166,   167,
     168,   169,   170,   171,   172,   173,   174,   175,   163,     0,
     161,   162,     2,   936,   690,   933,   809,   809,   941,   563,
     457,   853,   809,   863,   871,   875,   881,     2,   856,   858,
     860,     2,   876,   878,     0,  1043,  1046,   457,     0,     0,
       2,   101,   971,   809,  1055,   906,   809,   809,  1055,   809,
     921,   809,   809,     3,   677,     0,     0,  1055,  1055,   457,
     457,     0,     2,   708,     0,  1053,   705,  1054,     0,   701,
       0,     2,   704,   707,   179,   178,     0,     2,   457,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     809,   840,   844,   883,   809,   897,   902,   832,   889,     0,
       0,   443,   948,     0,   794,     0,   457,   795,   436,     0,
       0,     0,     0,   434,     2,   796,     0,   419,   765,     0,
     791,     2,   797,     0,     0,     0,     0,   594,   662,   458,
       3,     3,   666,   665,   867,     0,     0,   457,   399,     0,
     538,     3,   100,     3,   457,     0,     3,   766,     2,   721,
     457,   457,   715,   714,   715,   515,   513,   637,   635,   809,
     809,  1017,   457,  1022,   458,   457,  1008,   457,     0,     0,
       0,   986,     0,   809,  1056,   972,   973,   682,   969,   970,
     984,  1012,  1016,  1014,   544,   579,     0,  1020,  1025,   640,
     635,     0,   645,     0,   635,   747,   745,     0,     0,   816,
      66,   777,     0,     0,     2,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   457,     0,   121,   120,     0,   117,
     116,    27,     0,    28,     0,     0,     0,     0,     3,    66,
       0,    51,     0,    52,    59,     0,    58,    70,     0,    67,
      68,    71,    54,     0,    53,    57,     0,     0,    50,   123,
     125,   126,   127,   129,   130,   132,   133,   137,   138,   135,
     136,   140,   141,   143,   145,   147,   149,   151,     0,     0,
     408,     0,     0,    29,     3,   699,   157,   457,     0,     0,
       0,   937,   938,   934,   935,   740,   739,     2,   857,   859,
     861,     2,   877,   879,   457,   457,   962,   961,     2,     0,
       0,     0,     0,     0,   809,   972,   909,   926,     2,   904,
     912,   679,   907,   908,   680,     2,   919,   929,   922,   923,
       0,     3,  1055,   427,     2,  1048,     2,   670,   671,   649,
       3,     3,     3,     3,   693,     0,   155,     0,     3,     3,
       0,   702,     0,   696,     0,   809,     0,   457,     3,   431,
     433,     0,   809,   841,   845,   809,   898,   903,     2,   833,
     836,   838,     2,   890,   893,   895,   807,     0,   949,     3,
     953,   954,     3,   799,     3,   528,   527,   530,   529,     2,
     766,   800,     2,   798,     0,   766,   801,   594,   594,   594,
     457,     0,     0,   684,     0,   402,     0,     0,   457,     0,
       2,     0,     0,     0,     0,     0,   184,     0,   332,   333,
       0,     0,   371,   370,     0,   159,   159,   377,   555,   561,
     198,     0,   185,     0,   209,   186,   187,   457,   203,   188,
     189,   190,   191,     0,   192,   193,   338,     0,   194,   195,
     196,     0,   197,   205,   538,   457,     0,   207,     0,   396,
       0,     0,     0,     3,     0,   779,   766,   754,   755,     0,
       3,   750,     3,     3,     0,   457,   731,   731,  1053,   635,
     635,  1019,  1024,     2,   100,   457,     3,   553,     3,   458,
       3,   809,   979,   982,   457,     3,   968,   974,   635,   809,
     809,     0,     0,   623,     0,   639,   635,  1053,     2,   813,
     815,     0,    95,   457,   457,     0,    99,    97,   457,     0,
     111,     0,     0,     0,   115,   119,   118,   183,     0,     0,
       0,   699,   108,   176,     0,     0,    45,    46,    84,     0,
      84,    84,     0,    72,    74,    48,     0,     0,    44,     0,
      47,   154,     0,     0,     0,     0,  1053,     3,   809,   944,
     947,   939,   457,   457,     3,     3,     0,   809,   915,   918,
     809,     0,   809,   809,   910,   927,   457,   457,  1049,     0,
     672,   457,   457,     0,     0,     0,     0,   416,     3,     0,
       0,     0,     0,   698,   703,     3,   792,   181,   180,     3,
       0,     0,     2,   834,   837,   839,     2,   891,   894,   896,
     457,   457,   693,   809,   960,   959,     0,     0,     0,     0,
       0,     0,   766,   802,     0,   457,   457,   457,   457,   457,
     457,   577,   605,     3,     3,   606,   538,   595,     0,     0,
     849,     2,     0,   400,    66,     0,     0,   323,   324,   206,
     208,     0,     0,     0,   457,   457,   319,     0,   317,     0,
       0,     0,   699,     0,     0,     0,     0,     0,   160,     0,
       0,   378,     0,     0,     3,   213,     0,   204,     0,   314,
       0,     0,     2,     0,   538,   809,     0,   397,   964,   963,
       0,     2,     0,   757,     2,   752,     0,   753,     0,   735,
     716,   720,   718,     0,     0,     0,   457,     0,     0,     0,
       3,     0,     2,   975,   977,   978,     0,     0,   100,     0,
       3,  1053,   629,   635,   645,   645,   699,   646,  1053,     0,
     748,   457,   818,   965,     0,     0,     0,    38,     0,   112,
     114,   113,   110,   109,   699,  1053,     0,    65,    81,     0,
      75,    82,    83,    60,     0,     0,     0,    69,    56,     0,
     153,   407,    30,     0,     0,     2,   940,   942,   943,     3,
       3,     0,     0,   809,     2,   911,   913,   914,     2,   928,
     930,     0,   905,   920,     3,     3,  1050,     3,   657,   656,
     660,  1052,     2,     2,  1051,     0,     3,   806,   709,   710,
       0,     0,   809,   438,   457,   457,     3,     3,   444,   808,
       0,   955,     0,   956,   957,   951,   899,   783,     0,   785,
     577,   577,   577,   612,   582,     0,   618,   606,     0,   457,
     569,   604,   600,     0,     0,     0,     0,   607,   609,   809,
     620,   620,     0,   601,   616,   457,   403,     0,     0,    67,
     327,   328,   325,   326,     0,     0,     2,   224,     0,     0,
     226,   411,   225,   538,   457,   305,   304,   306,     0,     2,
     184,   264,     0,   257,     0,   184,   320,   318,     0,   312,
    1053,   321,     0,     0,     0,   359,   360,   361,   362,     0,
     352,     0,   353,   329,     0,   330,     0,     0,   457,   215,
     202,   316,   315,     0,   350,   369,     0,   401,   809,   457,
     781,   736,   457,     2,     2,   628,   635,   635,  1027,  1028,
    1029,     0,   980,   457,     3,     3,     0,   988,     0,     0,
     635,     0,   642,   641,  1053,     0,   626,     3,     0,   966,
      98,     0,    31,   457,     0,  1053,     0,     0,    85,     0,
      73,     0,    79,     0,    77,    43,   158,   945,   457,     0,
       0,   850,   868,   457,   457,     0,     0,     0,   457,   457,
     712,     0,   424,   426,     3,     3,     0,     0,     0,     0,
     787,   573,   575,   571,     0,   995,     0,   613,  1000,   615,
     992,   809,   809,   599,   619,   603,     0,   602,     0,     0,
       0,   622,     0,   809,   596,   610,   621,   611,   617,   664,
     668,   667,     0,     2,     0,     0,   245,     2,   227,   538,
     310,   308,   311,   307,     0,   309,     0,   253,     0,   184,
       0,     2,   457,   265,     0,   290,     0,     0,   313,     0,
       0,     2,   336,   363,     0,   354,     2,     0,     0,     0,
       0,   341,     0,   337,   200,   199,   425,   751,     0,     0,
    1053,  1053,  1030,     3,     0,     0,   987,   989,   627,     0,
     635,     0,   625,     2,    49,    41,    39,    40,     0,    63,
     177,    76,     0,     0,     3,   851,   869,     3,     3,   916,
     931,   428,     2,   654,     3,   653,   711,     0,     0,   842,
     900,   950,   958,     0,     0,     0,   996,   997,   809,   598,
     993,   994,   597,   578,     0,     0,   214,   335,     0,     0,
       0,   238,     2,   216,     0,     0,     2,   247,   262,   273,
     267,     2,   184,   302,     0,   277,     0,     0,   268,   266,
     255,   258,     0,     0,   184,   291,     0,     0,   219,   334,
       2,   457,   331,     0,     0,   379,     2,   339,     0,    66,
       0,   351,   756,   758,     0,     0,     0,   990,   991,   635,
    1053,   647,   749,    64,    80,    78,     0,     0,     0,   457,
       0,   843,   901,   809,  1003,  1005,   998,     0,   608,   233,
     228,   231,     0,   230,   237,   236,     0,   457,   240,   239,
       2,   249,     0,   246,     2,     0,     0,     0,   254,   259,
       0,     0,   184,   303,   278,     0,     0,     2,     0,   293,
     294,   292,   261,   322,     0,   457,   457,     3,   364,   458,
     368,     0,   372,     0,     0,     0,   380,   381,   222,   342,
       0,     2,     0,     2,     2,   631,   633,   981,  1053,     0,
     946,   917,   932,   658,     2,   999,  1001,  1002,   614,     0,
     235,     0,   234,   218,   241,   457,   392,   250,     2,   251,
     248,   263,   276,   274,   270,   282,   280,   281,   279,   260,
     275,   271,   272,   269,   256,     0,     0,     0,     0,   221,
     241,     3,   357,     0,   995,   365,   366,   367,   379,     0,
       0,     0,   379,     0,     2,   340,   347,     0,   344,   346,
       0,   632,   457,   229,   232,     2,     3,   242,   393,   252,
       0,     0,     0,     0,   301,   299,   296,   300,   297,   298,
     295,     3,   357,     0,     0,   996,     0,     0,     0,   373,
       0,   382,   223,     0,   337,     0,   630,     3,   210,     0,
       0,     2,   289,   287,   284,   288,   285,   286,   283,     0,
       0,   358,     0,   385,     0,   383,     0,   385,   343,   345,
       2,     0,     0,   212,   211,   217,     0,   220,     0,   355,
     386,     0,     0,   374,     0,   348,     2,  1004,   356,     0,
       0,     0,     0,   349,   387,   388,     0,   384,   375,     0,
       0,   376,   389
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1775,  6307,  5660, -1775,    -1,   504,  -135,   -54, -1775,  -298,
   -1775,   394, -1775,  -708,   698,   795,  -920, -1084, -1775,   182,
    3270,  1592, -1775,   174, -1775,  1387,   257,   739,   743,   265,
     754,  1352,  1346,  1353,  1354,  1355, -1775,  -151,  -143,  8139,
     921, -1775,  1663, -1775, -1775,  -662,  5754, -1125,  1678, -1775,
    -127, -1775,   920,     7, -1775, -1775, -1775,   461,   102, -1775,
   -1774, -1273,   325,    71, -1775, -1775, -1775,   334, -1473, -1775,
   -1453, -1775, -1775, -1775, -1775,    23, -1731,   214, -1775, -1775,
      27, -1775, -1775, -1775,    44,   487,   488,   153, -1775, -1775,
   -1775, -1775,  -831, -1775,    78,    15, -1775,   163, -1775,  -155,
   -1775, -1775, -1775,   933,  -830,  -871, -1278, -1775,    11, -1269,
     742,  2711,  -716,  -701, -1775,  -275, -1775,    26,  -133,    37,
    -286,  -240,  3576,   959,  -633, -1775,    74,    35,  1495,  2092,
   -1775,  2074, -1775,    80,  3588, -1775, -1775, -1775,   170, -1775,
   -1775,  1891,   240,  4437,  2727,   -31,  1870,  -255, -1775, -1775,
   -1775, -1775, -1775,  -335,  4196,  5044, -1775,  -374,   123, -1775,
     567,   297, -1775,   236,   797, -1775,   584,  -102, -1775, -1775,
   -1775,  -346,  5054,  -797,  1228,    40,  -690,  -604,  -405,  1516,
   -1775, -1262,  -152,     8,  1023,   978,  5883,  -129,  -490,  -241,
    -177,  -458,  1359, -1775,  1682,  -208,  1268,  1566, -1775, -1775,
   -1775, -1775,   283,  -167,  -125,  -882, -1775,   131, -1775, -1775,
     702,   526, -1775, -1775, -1775,  2172,  -734,  -329,  -928,   -17,
   -1775, -1775, -1775, -1775, -1775, -1775,   233,  -768,  -132, -1717,
    -212,  8146,   -70,  6709, -1775,  1235, -1775,  4195,  -183,  -204,
    -200,  -198,     1,   -68,   -67,   -60,   669,    -6,    14,    17,
    -162,   -56,  -161,  -159,  -157,  -710,  -661,  -654,  -645,  -671,
     -95,  -631, -1775, -1775,  -691,  1428,  1433,  1434,  1987, -1775,
     634,  7433, -1775,  -591,  -579,  -569,  -561,  -730, -1775, -1597,
   -1666, -1640, -1638,  -607,  -107,  -325, -1775, -1775,   157,   175,
     -76, -1775,  7873,   375,  -173,  -443
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,  1161,   222,   392,   393,    81,    82,   394,   368,   395,
    1466,  1467,   396,   978,   979,   980,  1272,  1273,  1274,  1478,
     418,   398,   399,   400,   686,   687,   401,   402,   403,   404,
     405,   406,   407,   408,   409,   410,   411,   420,  1077,   688,
    1399,   749,   216,   751,   414,   816,  1162,  1163,  1164,  1165,
    1166,  1167,  1168,  2079,  1169,  1170,  1404,  1588,  1921,  1922,
    1851,  1852,  1853,  2046,  2047,  1171,  1602,  1603,  1604,  1755,
    1756,  1172,  1173,  1174,  1175,  1176,  1177,  1412,  1782,  1974,
    1891,  1178,  1179,  1620,  2064,  1621,  1622,  1957,  1180,  1181,
    1182,  1402,  1965,  1966,  1967,  2111,  2126,  1994,  1995,   292,
     293,   877,   878,  1134,    84,    85,    86,    87,    88,    89,
     451,    91,    92,    93,    94,    95,   230,   568,   453,   422,
     454,    98,   302,   100,   101,   102,   333,   334,   105,   106,
     168,   107,   896,   335,   154,   110,   250,   111,   155,   258,
     337,   338,   339,   156,   415,   116,   117,   341,   118,   559,
     866,   864,   865,  1560,   342,   343,   121,   122,  1130,  1367,
    1566,  1567,  1716,  1717,  1368,  1555,  1735,  1568,   123,   646,
    1660,   643,   344,   644,   645,  1235,  1070,   459,   460,   870,
     871,   461,   462,   872,   346,   563,  1186,   424,   425,   217,
     479,   480,   481,   482,   483,   321,  1206,   322,   894,   892,
     593,   323,   362,   324,   325,   426,   125,   174,   175,   126,
    1200,  1201,  1202,  1203,     2,  1119,  1120,   585,  1195,   127,
     312,   313,   260,   270,   542,   128,   220,   129,   231,  1079,
     857,   509,   166,   130,   657,   658,   659,   131,   233,   234,
     235,   236,   307,   133,   134,   135,   136,   137,   138,   139,
     239,   308,   241,   242,   243,   784,   785,   786,   787,   788,
     244,   790,   791,   792,   754,   755,   756,   757,   510,  1112,
    1347,   140,  1668,   618,   619,   620,   621,   622,   623,  1719,
    1720,  1721,  1722,   608,   464,   349,   350,   351,   427,   208,
     142,   143,   144,   353,   808,   624
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      80,   987,   132,    80,   190,   288,   192,   193,   363,  1204,
     526,   691,   345,   647,   194,   150,   567,   805,   922,   507,
     305,  1393,   636,   539,   495,   916,   639,    96,   496,   412,
     497,   331,   967,   908,   850,   852,   104,   413,    97,   485,
     240,   152,   178,   909,   603,   199,   503,  1183,  1833,  1518,
    1519,   910,  1890,  1050,   960,    80,    80,   348,    80,  1057,
     132,   634,  1022,  1277,   297,   637,   498,   499,   195,   500,
     359,   501,  1046,    80,  1834,   103,  1835,   626,  1929,   477,
     218,   108,    80,   515,   205,    96,   574,   576,   196,   697,
      80,   197,  1284,   488,   104,    80,    97,   237,    80,   495,
     261,  1208,    80,   496,   271,   497,  1590,   537,    58,  1187,
    1073,  1047,  1924,   523,   264,  1591,  1591,   547,  1837,  1040,
     203,   503,   430,   431,   446,   367,  1041,  1125,  1088,   300,
     432,  1923,  1767,   103,   256,  1042,   366,  1237,    58,   108,
      80,   498,   499,    80,   500,    80,   501,   132,  1760,  1043,
    -791,    80,  1930,   648,  1196,  1318,   650,    80,   493,   936,
     192,   193,  1998,   603,    80,   571,  1375,  1376,   194,   157,
    1990,   112,    96,   504,  1963,   917,   141,   295,   886,   141,
    -394,   104,  1626,    97,   433,  1480,   203,   164,    80,    80,
     205,   916,  1351,   506,  1193,  1071,  1071,  1354,  1624,   597,
     626,   908,  1344,    80,   434,   467,   854,   435,    58,   463,
    2100,   909,  -759,   476,  1071,    58,   861,   286,    80,   910,
     103,   508,   195,   609,  1345,  1151,   108,    80,    80,   112,
     205,    58,  1241,   579,   141,   192,   193,   444,   597,  1916,
     207,   113,   196,   194,    80,   197,   575,  1485,   504,  1627,
    -394,   554,    80,  1931,   205,   698,   692,   888,   690,  1362,
     699,  1265,    80,  1999,   287,    80,   841,   543,  1422,  1925,
    1026,  1929,    80,   268,  1923,   282,   903,  1625,   141,  1486,
    1071,  1377,    80,    80,   640,    80,  1867,   516,   528,   823,
     531,   508,  1231,   824,   582,   825,  1234,   881,   508,   113,
     933,   541,    80,    80,   809,   205,   207,  1929,  1379,  1050,
      80,  1872,  1873,   928,   508,   901,   112,    80,    80,   837,
     731,   141,    80,   145,  1227,  1256,  1991,  1992,    97,   575,
    1291,   826,   827,   985,   828,  1304,   829,  1183,   803,   921,
     477,  1051,   609,  1890,  1319,  1054,   962,   531,  1989,   626,
    1590,  -395,   927,   615,  1067,  1068,  1937,  1833,   789,  1591,
      80,  2026,   732,    80,   656,   570,   943,   256,   188,   776,
    1380,   108,  -760,   626,  1305,   823,  -574,  1040,  1320,   824,
     626,   825,  1320,  1834,  1041,  1835,   113,   844,  1352,   860,
     848,   287,   847,  1042,  1072,  1072,   853,  1329,   837,  1187,
    1680,  1682,  1684,   158,   548,    63,    64,  1296,  1580,   855,
     532,  1940,  1941,  1072,  1363,   560,    58,   826,   827,   862,
     828,  -395,   829,    58,  1548,   430,   431,  1837,    80,  1364,
     467,  1451, -1054,   432,    58,   641,  1518,  1519,  1372,  1458,
     642,   159,   199,   641,   160,   161,   838,   162,   642,    58,
     331,    80,    80,    76,  1227,  -924,   203,  1373,   210,   908,
     163,   112,  -924,    80,    80,  1971,    20,   532,  1916,   909,
     514,   931,    80,   519,   476,  1758,   348,   910,  1572,  1072,
    1766,  1411,   775,  1071,  1362,  1362,  1362,   433,   218,   177,
     256,  1444,    80,  1591,   536,   767,    58,  1573,  1972,   508,
     179,    80,  1027,   467,   546,   900,   508,   434,   148,    58,
     435,   430,   431,  1048,   200,  1848,  1849,   613,   609,   432,
     962,    80,    58,    58,   210,   838,   211,    80,  1055,  1122,
    1946,   113,   613,   942,  1374,   690,   945,   946,   463,   947,
     180,   690,    58,  1372,  1848,  1849,   188,   667,   949,   886,
     690,   951,   952,   953,  1589,  1605,   219,  1286,  -791,  1679,
     172,   172,  1637,   428,  1724,    80,  1572,    80,   887,   690,
    1510,  1733,   711,   712,    -3,  1098,   207,  1212,    80,   508,
      80,    58,   467,  1725,    80,  1727,   132,   286,  1102,   436,
    1734,    58,   508,   711,    80,   172,   962,  1850,    80,    80,
      58,  1332,  1336,   152,   256,   508,   508,   590,   922,    97,
     463,    96,   287,   245,   626,  1768,  1211,  1489,    58,  1310,
     104,  1442,    97,   711,   213,   613,  1877,  -451,   533,  1582,
    1081,    80,  1080,   225,  1866,   214,   591,   592,  1838,  1363,
    1363,  1363,   412,   282,    80,   172,   570,  1983,   172,   626,
    1061,   215,   108,  1076,  1364,  1364,  1364,  1839,  1591,   103,
    1495,    58,   172,   541,   508,   108,  1578,   284,   962,   357,
    1504,  1733,   477,   286,   508,   803,   905,  1728,  2016,  1508,
    1011,   301,  1072,   613,   962,   533,  1591,   789,  1090,   962,
    1842,    14,    15,    16,    17,    18,    73,  1984,    80,   287,
      80,   508,    80,  1846,   554,   611,    80,  1106,  1935,    80,
      14,    15,    16,    17,    18,  1430,   752,  -624,  1113,   955,
     508,  2045,  -394,   172,  -624,   962,  1591,    78,    79,  1121,
     956,   957,  1123,  1613,    80,  2018,  1126,  1939,   721,   722,
     268,   594,   112,    90,   463,   595,   151,  2045,   845,  1952,
      58,   188,   206,   700,   962,   112,   361,  1761,   701,  -925,
     141,  1812,  1762,  1813,  2051,   238,  -925,   172,   262,    58,
     856,  1280,   272,   875,  2081,   287,   859,   172,  1276,    80,
     863,    80,   723,   724,   714,   463,  1107,   557,   172,   962,
     562,   715,   716,    80,   210,   188,  -780,  1260,  1589,  2053,
      80,    90,  -687,  1475,  1261,  1884,   476,   463,   463,    80,
    1885,  -455,   113,   319,  1677,   172,   364,  2009,    80,    80,
      80,   331,   172,   172,   611,   113,   463,   172,   733,  1477,
    -456,  1230,   734,  1317,   246,   247,  1276,   248,    80,   886,
    1271,   254,   249,   365,   436,   265,   508,   348,  1271,  1790,
    1791,   905,    14,    15,    16,    17,    18,  1281,   206,  1750,
    1751,  1752,  2031,  1434,  1435,   172,   759,  2032,   172,   366,
     760,   962,  1669,   468,    80,    80,   476,  1271,   198,    64,
     477,  1753,    14,    15,    16,    17,    18,   989,    90,   874,
    1759,  2096,   463,   875,  1342,   484,  2097,   437,   206,   428,
     428,  1197,   438,    96,   256,  1324,  1517,  -452,  1750,  1751,
    1752,    58,   104,   439,    97,   541,   466,    14,    15,    16,
      17,    18,   206,    73,   286,    80,   436,  1199,   508,   921,
    1753,   440,  1605,  1076,  1610,   544,   553,    64,   656,   256,
    1271,    58,   516,   612,   833,   470,   508,   613,  -453,  1303,
     789,  1185,   598,   282,    78,   614,   172,   108,    14,    15,
      16,    17,    18,   441,   626,   717,   718,   615,   172,   172,
      73,   990,   991,   992,    80,    73,    58,   159,   719,   720,
     160,   161,    80,   162,  1446,  1554,   997,   998,   999,  1000,
     612,  1750,  1751,  1752,   613,   612,   442,  1353,  1664,   613,
     923,    78,   614,  1900,   471,   771,    78,    79,   540,   508,
    1378,    80,   486,  1753,   476,   489,  1675,    58,    73,   810,
     811,   185,  1754,   812,   428,   935,   582,  1397,   436,   595,
     508,   886,  1048,   490,   436,  1433,   613,    80,   612,   363,
     363,   937,   613,    80,    80,   595,   491,   112,   492,    78,
     614,   938,   141,   200,   694,   939,   505,   651,   255,  1615,
    1616,  1617,  1618,  1619,  1459,   141,   506,  1423,   961,   276,
     148,   279,   962,   281,    80,   535,  1031,  1085,   286,   147,
     508,  1086,   508,   412,    65,    66,    67,    68,    69,    70,
      71,  1457,   516,  1124,   218,   463,   508,  1996,   468,   172,
     564,  1538,  1978,   897,   899,   964,   965,  1661,   609,  1592,
    1463,   331,   255,  1493,   279,   281,  1115,   113,  1117,  1468,
     962,   524,   962,   525,  1250,  1996,   477,  1301,    75,  1254,
     545,  1490,   652,   586,  1271,   428,   925,   348,   601,   476,
    1262,   582,    80,    80,    80,   508,   633,   653,   649,   172,
     654,   655,    65,    66,    67,    68,    69,    70,    71,   660,
    1520,   725,   726,   255,    96,  2048,   476,  1365,   412,   412,
    1275,   468,    80,   104,  1276,    97,  1526,  1527,   601,   694,
      80,  1970,  1570,    80,    80,   261,   271,    80,  1429,    73,
    1462,  1672,   760,    96,  1276,  1673,   264,  2066,    80,  1063,
    1064,  2070,   104,   971,    97,   973,  1744,   976,   642,  1714,
     962,   984,  1185,   508,   988,  1711,  1712,  1713,   108,   256,
      78,    79,  1770,  1065,  1066,   255,   962,   279,   281,  1771,
     661,    80,   664,  1086,  1750,  1751,  1752,   665,  1772,  1013,
     694,  1185,   962,   666,    80,   670,   962,   108,  1581,  1583,
    1355,  1356,  1357,   727,   541,   255,  1753,   477,  1843,  1932,
     476,   255,   760,   962,   713,  -185,   428,   730,    80,   181,
       6,     7,     8,     9,    10,    11,    12,    13,  1659,  -121,
    -121,  -121,  -121,  -121,  -121,  1665,  1635,  1369,  2035,   728,
    2098,   255,  1276,  1824,   962,  1263,  1086,   631,   172,   281,
      80,   735,  1676,   331,   729,   172,  2122,  2129,   112,  1571,
    2119,  2130,   667,   141,  1799,  1278,  1279,   962,  1282,   463,
     463,   477,   761,  1089,   762,  1091,   763,    90,   764,   348,
    -156,  -156,  1065,  1421,   765,   674,   766,   112,    -3,   477,
    1483,  1484,   141,   443,  1271,  1488,  1484,  1492,  1484,  1271,
    1271,  1271,   -17,  1592,   793,   268,  1037,  1476,  -454,   495,
     141,  1528,  1476,   496,    80,   497,  1037,  1540,    80,   150,
     806,    80,  1685,  1086,  1570,  1810,  1086,   807,   113,  1133,
     172,   172,   503,  1811,  1484,  1821,  1822,   817,   255,   711,
     830,   476,  1365,  1365,  1365,   152,  1553,  1557,  1831,   962,
     831,   498,   499,   832,   500,   834,   501,   113,  1888,  1889,
     835,   476,   836,    80,   255,   543,   631,   281,  -442,   104,
     104,    97,    97,  1904,  1484,  1469,  1470,  1471,  1905,  1484,
    1229,   674,  1472,  1473,  1848,  1849,   702,  1769,   703,   704,
     705,  -442,  -120,  -120,  -120,  -120,  -120,  -120,   840,   541,
    2119,  2120,  1481,  1482,   842,   476,   993,   994,  1594,  1594,
     294,   255,   995,   996,   108,   108,   858,   706,  1736,  1736,
     707,   708,  1197,   476,  -572,   709,   710,  -570,    80,  1001,
    1002,   255,   867,    80,    80,    80,   255,  1520,   255,  1431,
    1432,  1801,  1773,  1729,  1662,  1663,   876,   889,  1199,   891,
     895,  1571,  1808,   911,   913,   615,   331,   930,   255,   504,
     255,   255,  1369,  1369,  1369,   934,  1556,  1369,    14,    15,
      16,    17,    18,   959,   940,   823,   969,   941,   255,   824,
     963,   825,   348,   141,    14,    15,    16,    17,    18,  1255,
     255,   966,  1010,  1015,  1468,  1036,  1271,  1520,  1271,   837,
    1037,  1044,  1083,  1092,   112,   112,    80,   186,  1093,   141,
     141,  1094,    80,   255,    80,   631,   281,   826,   827,  1095,
     828,    80,   829,  1779,  1096,  1189,  1097,  1116,  1118,  1127,
     428,  -763,  1188,  1740,   873,   476,  1128,   255,   631,  1129,
     923,  -663,  1205,    83,   255,  1958,   149,  1221,   476,   274,
    1222,  1223,  1233,   275,   147,   264,   278,  1234,   280,    65,
      66,    67,    68,    69,    70,    71,   974,  1894,  1895,    90,
    1236,  1238,  1239,  1242,   113,   113,  1331,  1246,   256,  1243,
    1245,  1247,  1198,  1248,  1249,   476,   141,  1251,  1252,  1266,
    1253,  1258,  1259,  1323,   172,  1283,  1267,   172,   172,   172,
    1288,    83,  1289,  1197,  1308,  1290,   975,  1893,  1401,  1297,
    1298,  1299,  1570,   104,  1300,    97,   189,  -651,  -650,  1381,
    1958,   172,  1343,  -764,  1348,    83,   838,   172,  1370,  1199,
      80,  1371,    80,  1384,  -686,   562,  1385,  1408,   229,  1394,
    1395,   253,   172,  1396,  1403,    83,  1405,   412,   962,  1411,
    1418,  1415,  1594,  1417,   626,  1920,   463,   463,   108,  1419,
    1425,  1460,   147,  1427,  1474,   170,   171,    65,    66,    67,
      68,    69,    70,    71,  1883,  1476,  1491,  1979,    80,  1503,
    1516,    80,   149,  1521,  1522,  1523,   172,  1524,    83,  1484,
    1547,   149,   476,  1529,   304,   310,   476,  1532,  1546,  1464,
    1549,   274,  1545,  1559,  1374,  1561,   330,  1520,  1638,  1562,
     476,  1585,  1606,  1628,   268,  1607,  1630,  1609,   147,   141,
     476,   170,   171,    65,    66,    67,    68,    69,    70,    71,
     419,   189,   189,  1611,   541,  1409,  1623,    80,    80,  1571,
    1631,  1632,   149,   449,  1633,  1640,   253,   104,   112,    97,
    1641,  1646,  1645,   141,   147,  2040,  1643,  1781,  1644,    65,
      66,    67,    68,    69,    70,    71,  1647,   141,  1648,   495,
     229,   229,  1649,   496,  1650,   497,  2025,  1390,   255,   274,
     275,  1964,   630,  1652,   280,  1657,  1594,   304,   412,   255,
     412,   503,   108,  1666,    80,    83,  2043,  1670,  1920,  1671,
    1674,   476,  1678,   262,   272,   476,  1686,  1687,   253,  1700,
     476,   498,   499,   255,   500,  1691,   501,  1692,   113,   873,
     436,   675,  1528,   172,   255,  1702,   172,  1710,  1723,   412,
      90,  1564,   837,   255,  1743,   476,  1276,  2068,   310,  1745,
    1747,   219,  1776,  1778,   310,   304,   304,  1783,  1798,  1792,
    1796,  1797,   149,  1802,  1804,  1800,  1809,  1815,  1816,    90,
    1819,  1820,  2091,  1829,  1830,  1826,   172,   463,  1856,  1861,
    1876,  1886,   330,   616,   625,  1862,   254,   265,  1874,   476,
     873,  1880,   112,   476,   428,  1887,  1151,   141,  1882,   330,
    1897,  1898,  1899,   330,  1901,  1902,   476,  1903,   508,  -652,
    1934,  1911,  1912,   579,  1913,   192,   193,    80,  1914,    80,
    1915,   412,   104,   194,    97,  2109,  -555,   675,   504,  2121,
     476,  1936,   476,   476,  1964,  1945,  1942,   419,  1964,  1964,
     255,  2118,  1947,  1961,  1975,  1953,  1962,  1976,  1977,  1980,
     104,  1981,    97,  1982,  2002,  1993,  1822,   476,  2015,  2017,
    2019,  1594,   113,  2029,   255,  2028,  2030,   108,  2033,   838,
    2034,   419,  2037,  2094,   753,  2041,  2050,  2063,  2054,  2052,
    2067,   189,    80,    80,  2069,   205,  2074,  2075,  2076,  1594,
     104,  2082,    97,   476,  2092,   108,   274,   149,  1960,  2110,
     873,   449,  2093,  2110,   476,   782,  2095,   625,  2105,  2107,
    2108,    14,    15,    16,    17,    18,  2112,   873,   873,  2113,
    2116,  2117,  1969,  2127,    80,  2124,   467,  1806,  2128,  1594,
    2131,  1487,  1579,   958,  1004,   108,   172,  1400,   476,  1003,
     476,   750,  1005,   544,  1006,   229,  1007,  1407,  2106,  1780,
     172,  2061,   457,  2044,   229,  1878,  1871,   112,  2101,   476,
     151,  2099,   141,   172,  1973,   476,  2090,  1774,  1775,  2021,
      58,  2071,  2114,  1960,   304,   476,   419,   419,  2020,    80,
     304,  1416,   330,   169,  1726,   112,    90,    90,   534,    80,
     141,   181,     6,     7,     8,     9,    10,    11,    12,    13,
     172,   147,  1918,   255,   226,   227,    65,    66,    67,    68,
      69,    70,    71,  1988,   187,  1737,   540,  1558,  1232,  1413,
     304,   893,  1207,  1667,   813,   112,  1082,   113,  1787,    73,
     141,   304,     3,   304,  1240,   330,  1709,    83,   255,     0,
    1018,     0,     0,     0,   255,  1019,  1020,     0,     0,   228,
      75,   257,   273,   330,   449,   113,   625,     0,     0,     0,
      78,    79,   277,  1198,   616,     0,     0,     0,   616,     0,
       0,     0,     0,     0,     0,     0,     0,   330,     0,     0,
       0,   606,     0,     0,   629,     0,   518,   625,     0,     0,
     330,     0,     0,     0,     0,   113,     0,     0,   606,     0,
       0,   149,   606,     0,     0,   257,     0,   172,     0,     0,
       0,   172,     0,     0,   419,     0,     0,   149,   149,     0,
     419,     0,     0,     0,     0,   172,     0,     0,     0,   419,
       0,     0,   149,   149,   149,   172,     0,     0,    19,     0,
      14,    15,    16,    17,    18,     0,     0,     0,     0,     0,
       0,     0,   172,     0,     0,     0,   257,     0,    14,    15,
      16,    17,    18,   147,     0,     0,   170,   171,    65,    66,
      67,    68,    69,    70,    71,   873,   873,    48,    49,    50,
      51,    52,    53,    54,    55,   255,     0,     0,   449,   873,
     873,     0,     0,     0,     0,   254,   265,     0,     0,    58,
     606,     0,     0,     0,   753,   753,     0,     0,     0,     0,
       0,     0,   419,     0,     0,     0,   172,    58,   257,     0,
     172,     0,  1114,   873,   873,   172,    58,     0,     0,   449,
      90,     0,   782,   255,   782,     0,  1386,     0,     0,     0,
       0,     0,     0,     0,  1198,     0,     0,     0,   257,     0,
     172,   330,   330,     0,   257,     0,     0,   147,    73,     0,
     226,   227,    65,    66,    67,    68,    69,    70,    71,   758,
     330,     0,   304,     0,   245,     0,    73,     0,   752,     0,
       0,   457,   508,  1220,   257,   769,     0,     0,   772,    78,
      79,   304,     0,     0,   172,     0,  1714,     0,   172,     0,
     508,     0,     0,     0,     0,  1301,    75,    78,    79,     0,
       0,   172,   147,     0,     0,   226,   227,    65,    66,    67,
      68,    69,    70,    71,  2027,     0,     0,     0,   676,   419,
       0,     0,     0,     0,   457,   172,   330,   172,   172,     0,
      73,     0,   149,   419,     0,   518,     0,     0,     0,     0,
       0,   540,   606,   457,   330,     0,  1215,     0,     0,     0,
     228,    75,   172,     0,    90,     0,     0,   616,     0,     0,
       0,    78,    79,     0,   147,     0,   606,   226,   227,    65,
      66,    67,    68,    69,    70,    71,     0,     0,     0,   606,
       0,     0,     0,  1287,     0,     0,     0,     0,   172,     0,
    1919,     0,    73,     0,     0,     0,   449,   257,     0,   172,
    1294,  1295,   255,     0,     0,     0,     0,   873,   873,     0,
       0,     0,  1563,    75,   676,     0,     0,     0,     0,  1564,
     147,     0,     0,    78,    79,    65,    66,    67,    68,    69,
      70,    71,     0,   172,     0,   172,     0,     0,   369,     0,
       0,   370,     0,   371,     0,   372,     0,     0,  1741,     0,
       0,     0,     0,     0,   172,     0,     0,     0,     0,     0,
     172,     0,   373,   753,     0,     0,     0,     0,     0,    75,
     172,   257,   802,     0,  2125,     0,     0,   457,     0,     0,
     782,     0,     0,     0,  2132,     0,     0,   782,     0,   374,
     375,   257,   376,     0,   377,    63,    64,    65,    66,    67,
      68,    69,    70,    71,   378,   379,   366,     0,   380,   381,
     382,   257,   383,   384,     0,     0,     0,     0,   457,    90,
      73,     0,     0,     0,     0,     0,   873,   147,     0,   330,
     170,   171,    65,    66,    67,    68,    69,    70,    71,     0,
     385,   255,     0,    76,   386,     0,   257,    90,     0,     0,
     387,    78,    79,   388,   389,   390,   391,     0,   255,     0,
       0,   873,     0,     0,     0,     0,   873,   873,     0,     0,
     257,     0,   149,     0,    58,     0,     0,   257,   115,     0,
     419,   115,     0,     0,     0,     0,     0,    90,     0,   758,
     758,     0,     0,     0,     0,     0,     0,     0,     0,  1029,
    1388,     0,  1032,     0,     0,   147,    19,     0,     0,   419,
      65,    66,    67,    68,    69,    70,    71,   181,     6,     7,
       8,     9,    10,    11,    12,    13,   253,    83,     0,     0,
       0,     0,     0,    73,     0,     0,   115,     0,     0,     0,
       0,   304,     0,   606,   202,   255,   629,   149,     0,    52,
      53,    54,    55,    74,    75,     0,     0,   449,  1499,  1500,
     115,     0,     0,   518,    78,    79,     0,  1100,  1383,     0,
       0,  1104,  1514,  1515,     0,     0,   259,     0,     0,     0,
     115,     0,     0,     0,     0,     0,   449,     0,     0,     0,
     149,   147,     0,     0,     0,   457,    65,    66,    67,    68,
      69,    70,    71,   982,     0,     0,  1536,  1537,     0,     0,
     202,     0,     0,     0,     0,     0,     0,   115,     0,     0,
       0,     0,     0,   115,     0,   202,   115,     0,   147,     0,
     259,   354,   355,    65,    66,    67,    68,    69,    70,    71,
     326,   115,   358,   983,     0,   314,   315,   316,   317,   202,
       0,     0,     0,   330,   330,     0,     0,     0,     0,     0,
       0,     0,   452,     0,   147,   423,     0,     0,   255,    65,
      66,    67,    68,    69,    70,    71,  1268,   115,   423,    76,
    1269,   259,  1270,   147,   356,  1110,   170,   171,    65,    66,
      67,    68,    69,    70,    71,     0,     0,   149,   149,   149,
     149,   149,   149,     0,     0,     0,     0,  1565,   310,     0,
       0,   257,     0,    75,   202,     0,  1479,     0,     0,     0,
       0,     0,   257,     0,     0,     0,   419,   419,   115,     0,
     115,     0,     0,     0,     0,   318,     0,     0,     0,     0,
       0,     0,     0,   259,     0,     0,   257,     0,     0,     0,
       0,     0,     0,   319,     0,     0,   253,     0,   758,     0,
     558,     0,     0,     0,     0,     0,     0,     0,   115,     0,
       0,   202,     0,   259,   255,     0,     0,     0,   449,   259,
       0,     0,     0,     0,     0,     0,     0,   115,     0,     0,
       0,   202,     0,     0,     0,     0,     0,     0,     0,     0,
    1704,  1705,     0,   149,     0,   616,     0,   115,     0,   259,
     115,     0,     0,     0,     0,   873,     0,     0,     0,     0,
       0,     0,     0,     0,   115,     0,     0,     0,   115,  1334,
       0,     0,  1338,    58,     0,     0,     0,     0,   147,     0,
    1634,   226,   227,    65,    66,    67,    68,    69,    70,    71,
      14,    15,    16,    17,    18,     0,   606,     0,     0,     0,
       0,     0,   423,     0,   147,     0,    73,   226,   227,    65,
      66,    67,    68,    69,    70,    71,     0,     0,     0,     0,
     202,     0,     0,     0,     0,   457,  1563,    75,     0,     0,
       0,     0,    73,  1564,     0,  1715,   423,    78,    79,  1565,
       0,   419,     0,     0,     0,  1565,     0,  1565,     0,    58,
     202,     0,  2023,    75,     0,     0,   508,     0,     0,  1793,
       0,     0,   115,    78,    79,     0,   423,     0,     0,     0,
       0,     0,   259,     0,     0,   310,   149,     0,    58,     0,
     147,     0,     0,   226,   227,    65,    66,    67,    68,    69,
      70,    71,     0,     0,  1814,     0,     0,     0,     0,  1817,
    1818,     0,     0,     0,     0,     0,     0,     0,    73,   147,
     419,     0,   226,   227,    65,    66,    67,    68,    69,    70,
      71,   330,     0,     0,   149,   202,   202,     0,   780,    75,
       0,   452,   613,     0,     0,     0,     0,    73,     0,    78,
     781,   423,   423,     0,     0,     0,   259,   115,     0,     0,
       0,     0,     0,     0,  1746,   149,     0,   228,    75,     0,
       0,     0,     0,     0,     0,  1497,   257,  1757,    78,    79,
       0,     0,     0,     0,  1506,     0,     0,     0,   115,     0,
     330,   330,     0,   115,   202,     0,   259,   115,     0,   115,
       0,     0,     0,     0,     0,     0,  1715,  1715,     0,     0,
     115,   257,   115,   452,  1785,    14,    15,    16,    17,    18,
       0,  1565,     0,     0,  1565,     0,   358,   457,   115,   423,
       0,   259,     0,     0,     0,     0,   202,     0,     0,     0,
     147,   310,     0,   170,   171,    65,    66,    67,    68,    69,
      70,    71,   115,     0,   419,   259,     0,     0,     0,   558,
     202,     0,   259,     0,     0,   115,     0,   929,     0,     0,
       0,     0,     0,     0,    58,     0,   115,     0,     0,     0,
       0,   304,    14,    15,    16,    17,    18,     0,     0,   423,
       0,     0,   115,   115,     0,   423,   588,     0,     0,     0,
       0,     0,     0,     0,   423,   147,     0,   115,   115,   115,
      65,    66,    67,    68,    69,    70,    71,     0,     0,     0,
       0,  1847,     0,     0,     0,  1857,     0,     0,  1715,     0,
       0,     0,     0,    73,     0,     0,     0,  1565,     0,  1870,
    1543,    58,     0,     0,     0,     0,     0,   452,     0,  1879,
     397,     0,     0,    74,    75,     0,     0,     0,   257,     0,
       0,     0,     0,   423,    78,    79,     0,     0,     0,     0,
       0,   202,   147,   149,     0,   226,   227,    65,    66,    67,
      68,    69,    70,    71,     0,     0,   147,   423,   452,   226,
     227,    65,    66,    67,    68,    69,    70,    71,     0,     0,
      73,   330,     0,     0,   423,     0,   257,   147,     0,  1715,
     452,   452,    65,    66,    67,    68,    69,    70,    71,   149,
    2023,    75,     0,     0,   508,     0,   115,   115,     0,   452,
    1928,    78,    79,     0,  1933,    73,     0,     0,     0,  1938,
    1718,     0,   898,     0,     0,   115,     0,   149,   149,     0,
    2024,   310,     0,     0,     0,  1038,    75,     0,  2077,   613,
       0,     0,     0,     0,  1968,     0,    78,    79,     0,     0,
    1111,     0,     0,   115,     0,     0,     0,    99,     0,     0,
     153,     0,     0,     0,     0,     0,     0,   149,     0,   109,
       0,     0,     0,     0,     0,   452,   259,     0,     0,     0,
       0,     0,   202,     0,   423,     0,     0,   259,  1997,     0,
       0,   115,  2000,     0,     0,  2024,  2024,   115,   423,     0,
       0,     0,     0,     0,     0,  2014,     0,     0,     0,   115,
       0,  1217,   423,     0,   115,    99,     0,     0,     0,   663,
       0,     0,     0,   397,   669,     0,     0,   109,     0,  2036,
       0,  2038,  2039,   678,   679,     0,     0,  2024,     0,   204,
       0,     0,     0,     0,     0,   202,     0,     0,   397,   397,
       0,     0,     0,     0,     0,     0,  2049,     0,     0,   266,
       0,   423,     0,     0,     0,   257,     0,     0,     0,   397,
      58,   267,     0,     0,     0,     0,     0,     0,     0,     0,
     147,  1718,  1718,   226,   227,    65,    66,    67,    68,    69,
      70,    71,  2072,     0,     0,     0,   296,     0,     0,   397,
       0,   147,    99,  2078,   226,   227,    65,    66,    67,    68,
      69,    70,    71,     0,   109,     0,     0,     0,     0,     0,
     332,     0,     0,     0,   115,     0,     0,     0,     0,    73,
       0,     0,   336,     0,     0,     0,  1228,  2104,     0,  2078,
       0,   115,   115,     0,     0,   429,     0,     0,     0,   303,
      75,     0,   606,     0,     0,     0,   296,   455,  2115,     0,
      78,    79,     0,     0,  2104,     0,     0,     0,     0,   456,
       0,     0,     0,     0,  2123,     0,     0,     0,   452,     0,
       0,   147,     0,     0,     0,   502,    65,    66,    67,    68,
      69,    70,    71,  1268,   115,     0,     0,  1269,     0,  1270,
       0,   522,     0,  1718,     0,     0,   527,   529,   147,   204,
       0,   170,   171,    65,    66,    67,    68,    69,    70,    71,
       0,   257,     0,     0,     0,     0,     0,   606,     0,     0,
      75,   549,     0,  1681,   551,     0,   552,   115,     0,     0,
       0,     0,     0,   550,     0,   423,   147,   569,     0,   555,
     556,    65,    66,    67,    68,    69,    70,    71,     0,   109,
     581,   147,     0,     0,     0,     0,    65,    66,    67,    68,
      69,    70,    71,  1268,   423,     0,     0,  1269,     0,  1270,
    1986,     0,     0,     0,  1718,     0,   604,     0,     0,   628,
       0,   259,   115,     0,     0,     0,   202,    76,   605,     0,
       0,   267,     0,   635,     0,     0,   202,   635,     0,     0,
      75,     0,   115,  1683,     0,   605,     0,     0,     0,   605,
       0,     0,   423,     0,     0,  1718,  1217,     0,     0,     0,
       0,     0,     0,     0,     0,   202,   147,     0,  1454,   198,
      64,    65,    66,    67,    68,    69,    70,    71,     0,     0,
     115,   423,     0,     0,     0,   115,     0,     0,     0,     0,
       0,     0,     0,   397,   397,   397,   397,   397,   397,   397,
     397,   397,   397,   397,   397,   397,   397,   397,   397,   397,
     397,   397,     0,     0,     0,    75,     0,     0,   802,     0,
    1718,  1718,     0,     0,     0,     0,     0,     0,     0,   115,
     115,   296,   452,   452,     0,   604,     0,     0,     0,     0,
       0,     0,     0,   115,   115,     0,     0,   605,   115,   115,
     737,   738,   739,   740,   741,   742,   743,   744,   745,   746,
     747,   257,  1718,     0,   213,     0,     0,     0,     0,     0,
      58,     0,     0,   397,     0,     0,     0,   115,   115,     0,
       0,     0,     0,     0,     0,  1544,     0,     0,     0,     0,
       0,   748,   115,   115,   115,   115,   115,   115,     0,     0,
       0,   147,     0,   259,   226,   227,    65,    66,    67,    68,
      69,    70,    71,     0,     0,     0,   455,     0,     0,     0,
       0,   423,   423,     0,     0,     0,     0,     0,   456,    73,
       0,     0,   375,     0,   376,     0,   377,    63,    64,    65,
      66,    67,    68,    69,    70,    71,     0,   869,     0,  1563,
      75,   259,   529,     0,     0,     0,   880,   202,   569,   336,
      78,    79,     0,     0,     0,     0,     0,     0,   267,   332,
     109,    99,     0,   423,     0,     0,     0,     0,     0,     0,
       0,   456,   696,   109,     0,    76,   386,   635,   904,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   115,   605,
     456,     0,   915,     0,     0,     0,     0,   119,     0,     0,
     119,   604,     0,     0,     0,     0,   924,     0,     0,     0,
       0,     0,     0,   605,   635,     0,     0,     0,     0,     0,
     397,     0,     0,     0,   147,   397,   605,   226,   227,    65,
      66,    67,    68,    69,    70,    71,   397,   147,     0,     0,
     553,    64,    65,    66,    67,    68,    69,    70,    71,     0,
       0,     0,    73,     0,     0,   119,     0,     0,     0,     0,
       0,   115,   115,     0,     0,     0,     0,     0,     0,   191,
     202,     0,   780,    75,     0,     0,   613,     0,   397,   119,
       0,     0,     0,    78,   781,     0,   423,     0,     0,  1012,
       0,   232,     0,     0,     0,     0,   615,     0,     0,   119,
     147,     0,   115,   226,   227,    65,    66,    67,    68,    69,
      70,    71,   455,     0,     0,     0,     0,     0,     0,     0,
     259,   115,     0,     0,   456,     0,     0,     0,    73,  1021,
       0,     0,     0,     0,     0,     0,   119,     0,     0,     0,
       0,     0,   119,     0,     0,   119,     0,   306,  2023,    75,
     202,     0,   508,   904,     0,   423,     0,     0,  1045,    78,
      79,     0,     0,     0,     0,   456,   115,     0,     0,   115,
       0,     0,     0,     0,     0,   455,   455,     0,     0,     0,
     115,     0,     0,     0,   119,     0,     0,   336,   336,     0,
       0,     0,     0,     0,   455,     0,   119,     0,     0,     0,
     115,     0,     0,     0,     0,     0,   336,     0,   397,   452,
     452,     0,     0,     0,     0,   115,     0,     0,     0,     0,
     115,   115,   869,   494,   232,   115,   115,     0,     0,     0,
       0,     0,     0,     0,   336,     0,     0,     0,   114,     0,
     306,     0,     0,     0,     0,     0,     0,   119,     0,   119,
       0,     0,     0,  1184,   119,     0,     0,     0,     0,     0,
     455,     0,     0,     0,     0,   109,   153,     0,     0,     0,
       0,     0,   336,     0,     0,     0,   259,     0,   635,     0,
       0,  1219,     0,   869,     0,   397,     0,   119,  1225,   423,
     605,     0,     0,   267,     0,   336,   114,     0,   580,   306,
       0,     0,     0,     0,   397,   147,   119,     0,   226,   227,
      65,    66,    67,    68,    69,    70,    71,     0,     0,     0,
       0,   397,   397,   397,     0,     0,     0,     0,   397,   397,
     332,     0,     0,    73,     0,     0,     0,     0,     0,   147,
     269,     0,   456,     0,    65,    66,    67,    68,    69,    70,
      71,  1268,   397,   303,    75,  1269,     0,  1270,     0,     0,
       0,     0,     0,   147,    78,    79,    63,    64,    65,    66,
      67,    68,    69,    70,    71,     0,     0,     0,     0,     0,
       0,   119,     0,   114,     0,     0,     0,     0,    75,   397,
     397,     0,     0,   869,     0,     0,     0,     0,     0,     0,
       0,   340,     0,     0,     0,   336,     0,     0,   115,     0,
     869,   869,     0,     0,    76,   119,     0,     0,     0,     0,
     452,   147,   336,   336,   170,   171,    65,    66,    67,    68,
      69,    70,    71,     0,     0,     0,   115,     0,   458,     0,
       0,   119,     0,     0,     0,     0,     0,     0,   783,     0,
       0,     0,     0,     0,   115,     0,     0,     0,     0,     0,
       0,     0,     0,   455,     0,     0,     0,     0,     0,   466,
       0,     0,     0,     0,     0,   336,     0,     0,     0,     0,
       0,     0,   115,   115,     0,     0,   259,     0,   822,     0,
       0,     0,     0,     0,     0,     0,     0,   232,     0,     0,
       0,     0,     0,     0,     0,     0,  1366,     0,     0,     0,
       0,     0,     0,     0,  1184,     0,     0,   306,     0,     0,
     119,   119,   115,   306,     0,     0,   109,   147,   114,     0,
     170,   171,    65,    66,    67,    68,    69,    70,    71,     0,
       0,     0,     0,  1184,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   109,     0,     0,     0,     0,
       0,  1414,   119,   306,     0,     0,   119,   607,   119,   115,
     269,     0,     0,   267,   885,   470,   306,     0,     0,     0,
       0,   119,     0,     0,   607,     0,     0,     0,   607,     0,
       0,   604,     0,     0,     0,     0,     0,     0,     0,     0,
     527,     0,     0,   605,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   869,
     332,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   336,   456,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   119,    14,    15,    16,    17,
      18,     0,     0,     0,     0,     0,     0,     0,   119,     0,
       0,   119,   119,     0,   119,     0,     0,     0,   869,   869,
       0,     0,     0,   119,     0,     0,   119,   119,   119,     0,
     336,   336,   869,   869,     0,     0,   607,   455,   455,     0,
       0,     0,     0,     0,   336,   336,     0,     0,     0,   336,
     336,     0,     0,   397,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   869,   869,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   336,   336,
       0,  1366,  1366,  1366,   153,     0,   147,     0,     0,   226,
     227,    65,    66,    67,    68,    69,    70,    71,     0,     0,
       0,     0,     0,     0,     0,     0,   119,     0,     0,     0,
    1593,  1593,     0,     0,    73,     0,     0,   458,     0,     0,
       0,     0,   109,   109,     0,  1039,     0,   783,     0,     0,
       0,     0,     0,     0,   303,    75,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    78,    79,     0,   340,     0,
       0,     0,     0,     0,     0,     0,     0,   269,     0,   114,
       0,     0,   332,     0,     0,   306,     0,     0,     0,     0,
     458,     0,   114,     0,   456,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   306,     0,     0,   153,   607,   458,
       0,     0,     0,     0,     0,   120,     0,     0,   120,     0,
       0,     0,     0,     0,     0,   124,     0,     0,   124,     0,
       0,     0,   607,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   119,     0,   607,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   119,   119,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   120,     0,     0,     0,     0,     0,     0,
     869,   869,     0,   124,     0,     0,     0,     0,   397,     0,
       0,     0,   336,   336,     0,     0,     0,   120,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   124,     0,     0,
       0,  1732,     0,     0,     0,     0,     0,   120,     0,     0,
       0,   869,     0,     0,     0,     0,     0,   124,   397,     0,
       0,     0,     0,   336,     0,     0,     0,     0,     0,     0,
    1749,     0,     0,   458,     0,     0,     0,     0,     0,     0,
       0,     0,   267,     0,   120,     0,     0,     0,     0,     0,
     120,     0,     0,   120,   124,     0,     0,     0,     0,     0,
     124,     0,     0,   124,  1593,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   458,   332,   109,     0,   153,     0,
       0,     0,     0,     0,     0,     0,     0,   336,     0,   869,
       0,     0,   120,  1039,     0,     0,   340,   340,     0,  1302,
     783,   336,   124,     0,   120,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   124,   340,     0,     0,     0,   397,
       0,   397,     0,     0,   869,     0,     0,     0,     0,   869,
     869,     0,     0,     0,   455,   455,   336,     0,     0,     0,
       0,   336,   336,   340,     0,     0,   336,   336,     0,     0,
       0,  1836,     0,     0,     0,   120,     0,   120,     0,     0,
     397,     0,   120,     0,     0,   124,     0,   124,     0,     0,
       0,     0,   124,     0,   114,     0,     0,     0,     0,     0,
       0,   340,     0,     0,     0,     0,   119,    14,    15,    16,
      17,    18,     0,   397,   119,   120,     0,     0,  1593,   607,
       0,     0,   269,     0,   340,   124,     0,     0,     0,     0,
     109,     0,     0,     0,   120,     0,     0,     0,     0,     0,
       0,     0,     0,   119,   124,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   119,   397,     0,     0,     0,    58,     0,     0,     0,
       0,   458,     0,     0,   306,     0,     0,     0,     0,     0,
       0,   119,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   147,     0,     0,
     226,   227,    65,    66,    67,    68,    69,    70,    71,   120,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   124,
       0,     0,     0,     0,   119,    73,     0,     0,     0,     0,
       0,     0,     0,     0,   340,     0,     0,  1959,     0,     0,
       0,     0,     0,   120,     0,  1563,    75,     0,     0,   605,
       0,   340,   340,   124,     0,     0,    78,    79,     0,     0,
       0,     0,     0,     0,     0,   455,     0,     0,     0,   120,
       0,     0,     0,     0,     0,     0,  1511,   336,     0,   124,
       0,     0,     0,  1593,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   109,     0,     0,     0,     0,
       0,     0,     0,     0,   340,     0,     0,     0,     0,     0,
       0,  1593,  1959,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   109,   605,     0,     0,     0,     0,     0,
       0,   119,   119,   119,   119,   119,   119,     0,     0,     0,
    1569,     0,     0,     0,     0,     0,     0,     0,   120,   120,
       0,  1593,     0,     0,     0,   114,     0,     0,   124,   124,
     119,   119,     0,   109,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    2065,     0,     0,     0,   114,     0,     0,     0,     0,     0,
     120,     0,     0,     0,   120,     0,   120,     0,   869,     0,
     124,     0,   269,     0,   124,     0,   124,     0,     0,   120,
     336,     0,     0,     0,     0,     0,     0,     0,     0,   124,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   607,     0,     0,     0,     0,   119,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     340,   458,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   120,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   124,     0,     0,   120,     0,     0,   120,
     120,     0,   120,     0,     0,     0,   124,     0,     0,   124,
     124,   120,   124,     0,   120,   120,   120,     0,     0,   340,
     340,   124,     0,     0,   124,   124,   124,     0,     0,     0,
       0,     0,     0,   340,   340,     0,     0,   212,   340,   340,
       0,     0,  1569,   223,   224,   119,     0,     0,  1730,     0,
    1569,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   340,   340,     0,
       0,     0,     0,     0,     0,     0,     0,   285,     0,     0,
     119,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   120,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   124,     0,     0,     0,     0,     0,
       0,   114,   114,     0,   119,     0,     0,     0,     0,     0,
       0,     0,    14,    15,    16,    17,    18,    19,   119,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,   119,
      46,     0,    47,   458,     0,     0,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,  1224,
       0,     0,     0,     0,     0,     0,     0,    14,    15,    16,
      17,    18,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   120,     0,     0,  1844,    63,    64,  1569,     0,     0,
       0,   124,     0,     0,   120,   120,     0,     0,     0,   173,
     176,     0,     0,   369,   124,   124,   370,     0,   371,     0,
     372,     0,     0,     0,     0,     0,     0,     0,   119,     0,
     577,     0,     0,     0,     0,     0,    58,   373,     0,     0,
       0,   340,   340,    76,   221,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   306,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   374,   375,     0,   376,     0,   377,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   378,
     379,   366,   340,   380,   381,   382,     0,   383,   384,     0,
       0,     0,     0,     0,   298,    73,     0,   299,     0,     0,
       0,   269,     0,     0,     0,     0,     0,     0,     0,     0,
    1569,   320,     0,     0,     0,   385,     0,     0,    76,   386,
       0,     0,     0,     0,     0,   387,   448,    79,   388,   389,
     390,   391,     0,     0,     0,   114,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   340,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     340,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   487,     0,     0,     0,     0,     0,   778,     0,
     779,     0,     0,     0,     0,     0,     0,     0,     0,   795,
     796,     0,     0,   119,     0,   340,     0,     0,     0,     0,
     340,   340,     0,     0,     0,   340,   340,     0,     0,   689,
       0,     0,     0,     0,     0,     0,   538,     0,     0,     0,
       0,   119,     0,   306,     0,     0,   173,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   173,     0,     0,
       0,     0,     0,     0,   120,     0,     0,     0,     0,     0,
       0,     0,   120,     0,   124,     0,     0,     0,     0,     0,
       0,   119,   124,     0,   584,     0,     0,     0,     0,   114,
       0,   587,   589,     0,     0,     0,   596,     0,     0,     0,
       0,   120,     0,     0,     0,     0,     0,     0,   580,   306,
       0,   124,     0,     0,     0,     0,     0,   879,     0,   120,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   124,
       0,     0,     0,     0,   320,     0,     0,   320,     0,   120,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   124,
     306,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   849,   851,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   120,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   124,     0,     0,     0,     0,     1,     0,     0,
     146,     0,     0,     0,     0,     0,     0,     0,   607,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   221,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   340,   797,   798,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   114,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   201,     0,     0,     0,     0,     0,
       0,     0,   114,   607,     0,     0,     0,     0,     0,   120,
     120,   120,   120,   120,   120,     0,     0,     0,     0,   124,
     124,   124,   124,   124,   124,     0,   689,     0,     0,     0,
       0,     0,   689,     0,     0,     0,     0,     0,   120,   120,
       0,   689,   114,     0,     0,     0,     0,     0,   124,   124,
       0,     0,     0,   291,     0,     0,     0,     0,     0,     0,
     689,     0,     0,  1060,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   320,   340,
       0,     0,     0,     0,     0,     0,  1009,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   120,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   124,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   932,     0,
    1131,  1132,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1190,  1191,  1192,     0,   291,  1194,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   530,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   291,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   291,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   561,   565,     0,     0,     0,     0,     0,
     572,   573,     0,   120,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   124,     0,     0,   583,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1264,     0,
       0,     0,     0,     0,     0,     0,   602,     0,   120,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   124,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   120,     0,  1285,     0,     0,  1062,     0,     0,
       0,     0,   124,     0,  1074,     0,   120,     0,     0,     0,
       0,     0,     0,   695,     0,     0,   124,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   120,     0,     0,
       0,  1309,     0,     0,   736,     0,     0,   124,     0,     0,
    1313,  1314,  1315,  1316,     0,     0,     0,     0,  1321,  1322,
       0,     0,     0,     0,     0,     0,     0,     0,  1330,     0,
     774,     0,     0,     0,   777,     0,     0,   167,     0,  1135,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1346,
       0,     0,  1349,   799,  1350,     0,     0,   800,   801,     0,
       0,   804,     0,   167,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   818,   819,   820,   821,
       0,     0,     0,     0,     0,     0,   120,     0,     0,     0,
       0,     0,     0,     0,     0,   843,   124,     0,     0,     0,
       0,     0,     0,   846,     0,     0,     0,  1406,     0,   167,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   167,     0,   167,     0,     0,     0,     0,     0,
       0,   291,     0,  1420,     0,     0,     0,     0,     0,     0,
    1424,     0,  1426,  1428,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1437,   360,  1438,     0,  1439,     0,
    1441,     0,   884,     0,     0,  1449,     0,     0,     0,   561,
       0,     0,     0,     0,     0,   890,     0,     0,     0,   360,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   907,
     912,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   167,     0,     0,
       0,   167,     0,     0,   167,   167,     0,  1494,   167,     0,
       0,   167,   167,     0,  1501,  1502,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   120,     0,     0,     0,     0,     0,     0,  1525,     0,
       0,   124,     0,     0,     0,  1530,     0,     0,     0,  1531,
     954,     0,     0,     0,     0,     0,     0,     0,     0,   120,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   124,
       0,     0,   167,     0,     0,   167,     0,     0,     0,     0,
       0,     0,     0,     0,   223,     0,  1387,  1389,  1391,     0,
       0,     0,     0,     0,     0,     0,   167,   167,     0,   120,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   124,
       0,     0,   167,     0,     0,     0,  1410,     0,     0,  1017,
       0,     0,     0,     0,  1629,     0,     0,     0,     0,     0,
       0,  1135,     0,     0,  1034,     0,     0,     0,  1035,     0,
       0,     0,     0,     0,     0,     0,     0,   907,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1651,     0,     0,     0,     0,     0,     0,     0,  1656,  1075,
    1658,     0,     0,     0,     0,  1455,     0,     0,  1084,     0,
    1447,     0,     0,     0,  1087,     0,     0,     0,    14,    15,
      16,    17,    18,     0,     0,  1584,     0,     0,  1587,  1601,
       0,     0,     0,     0,  1608,     0,     0,   167,  1612,     0,
    1614,     0,     0,     0,     0,     0,     0,     0,     0,  1689,
    1690,     1,     0,     0,   369,     0,     0,   370,     1,   371,
       0,   372,     0,     0,  1695,  1696,     0,  1697,     0,     0,
       0,     0,     0,     0,     0,     0,  1701,    58,   373,     0,
       0,     0,     0,     0,     0,     1,  1706,  1707,     0,     0,
       0,     0,   360,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   167,   374,   375,     0,   376,     0,
     377,    63,    64,    65,    66,    67,    68,    69,    70,    71,
     378,   379,   366,     0,   380,   381,   382,     0,   383,   384,
       0,     0,     0,     0,     0,     0,    73,     0,     0,     0,
       0,  1244,  1574,     0,     0,  1576,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   385,     0,     0,    76,
     386,     0,     0,     0,     0,     0,   387,  1448,    79,   388,
     389,   390,   391,     0,     0,     0,     0,     0,   360,     0,
       0,     0,     0,     0,  1708,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1794,  1795,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1803,     0,   167,
     167,  1742,   369,     0,  1292,   370,     0,   371,  1293,   372,
       0,     0,   167,  1748,     0,   907,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1306,   373,     0,  1763,  1765,
       0,     0,  1307,     0,  1827,  1828,     0,     0,     0,     0,
       0,  1311,     0,  1312,     0,     0,     0,     0,     0,     0,
       0,     0,  1587,   374,   375,     0,   472,     0,   377,    63,
      64,    65,    66,    67,    68,    69,    70,    71,   378,   379,
     366,     0,   380,   381,   382,  1340,   383,   384,     0,  1341,
       0,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   146,     0,     0,     1,
       0,     0,     0,     0,   385,    75,     0,   473,   474,     0,
       0,     0,   475,     0,   387,    78,    79,   388,   389,   390,
     391,     0,     0,  1896,     0,  1738,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   167,   167,     0,     0,     0,
       0,   167,     0,     0,  1906,     0,     0,  1907,  1908,     0,
       0,     0,     0,     0,  1910,     0,     0,     0,     0,     0,
       0,     0,   167,     0,     0,   167,   167,     0,   167,  1855,
     167,   167,     0,     0,     0,     0,     0,     0,  1858,     0,
    1860,     0,     0,  1865,  1869,     0,  1601,     0,     0,     0,
    1436,  1875,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   167,
       0,     0,     0,   167,     0,  1461,     0,     0,     0,     0,
       0,   251,   181,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,     0,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,  -458,  -458,   347,  -458,    46,
       0,    47,     0,  -458,     0,     0,     0,     0,   167,   167,
       0,     0,     0,     0,     0,     0,     0,  2022,     0,     0,
      58,  1944,   167,     0,     0,     0,  1949,  1951,     0,     0,
       0,     0,     0,   445,   347,     0,     0,     0,     0,  1534,
       0,     0,     0,  1535,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   511,     0,     0,     0,     0,
       0,  1892,   511,     0,     0,     0,     0,     0,  1575,    73,
       0,  2062,     0,     0,     0,     0,     0,     0,     0,  2001,
       0,  2004,     0,     0,  2006,  2008,     0,     0,     0,  2011,
    2013,     0,    76,   252,     0,     0,  2080,     0,     0,     0,
      78,    79,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  2089,     0,     0,     0,     0,     0,     0,  1639,     0,
       0,  1642,     0,     0,     0,     0,     0,  2102,     0,   511,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1653,
       0,     0,     0,   167,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   347,   617,     0,     0,     0,     0,     0,
    2056,  2058,  2060,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   638,     0,     0,     0,     0,     0,
       0,  2073,     0,     0,   167,     0,     0,     0,     0,     0,
       0,   167,  1688,     0,   167,  2084,  2086,  2088,     0,     0,
       0,  1693,     0,     0,     0,  1694,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1698,
    1699,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   511,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   511,   770,     0,   511,   773,     0,     0,     0,     0,
       0,     0,   347,     0,     0,     0,   617,     0,    14,    15,
      16,    17,    18,     0,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,   511,    47,     0,
     167,   511,     0,     0,     0,     0,     0,     0,   167,   167,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
    1788,  1789,     0,     0,     0,     0,   209,     0,     0,     0,
       0,     0,     0,   347,     0,     0,     0,     0,     0,     0,
       0,     0,   263,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   167,     0,     0,
       0,     0,     0,     0,     0,     0,   167,     0,     0,   167,
       0,   167,   167,   511,     0,     0,   347,     0,     0,     0,
       0,     0,   209,     0,     0,     0,   311,     0,     0,     0,
       0,     0,     0,     0,   902,   347,     0,   352,     0,     0,
       0,     0,     0,     0,     0,   617,     0,     0,     0,   617,
       0,     0,   167,     0,     0,     0,   920,     0,   347,     0,
       0,   209,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   465,     0,     0,   469,     0,     0,
       0,     0,     0,  1881,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1642,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   167,     0,   209,     0,     0,  1909,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   263,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1927,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   347,
       0,     0,     0,     0,     0,   469,     0,     0,     0,     0,
       0,     0,     0,   209,  1955,   511,   511,  1956,     0,     0,
       0,     0,     0,     0,   165,   511,  1030,     0,   511,  1033,
       0,     0,     0,   610,     0,   627,     0,     0,     0,     0,
     347,     0,   167,   617,     0,   617,   617,     0,     0,     0,
       0,     0,   617,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   347,   347,     0,     0,     0,     0,     0,     0,
       0,   167,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   347,     0,     0,     0,   511,     0,     0,   693,   511,
       0,     0,     0,   511,  1101,     0,   283,   511,  1105,     0,
       0,     0,   167,     0,     0,  1108,     0,     0,   167,   289,
       0,   290,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  2042,   209,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   347,   511,     0,
       0,     0,   610,     0,     0,     0,     0,   421,   794,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     450,     0,     0,     0,     0,     0,     0,   167,   617,     0,
       0,     0,     0,   478,     0,   478,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   512,   513,     0,     0,   517,     0,   347,   520,   521,
       0,     0,     0,     0,     0,     0,     0,   209,   209,     0,
       0,     0,     0,   465,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     167,   167,     0,     0,     0,     0,     0,     0,   360,     0,
     578,     0,   167,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   511,     0,   352,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   617,   617,   599,   600,   465,     0,   906,   617,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   632,
       0,     0,     0,     0,     0,     0,     0,     0,   610,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     347,     0,   209,     0,     0,   511,  1335,     0,   511,  1339,
       0,     0,     0,     0,     0,   693,     0,     0,   693,   693,
       0,   693,     0,     0,     0,     0,     0,   167,     0,     0,
     693,     0,     0,   693,   693,   693,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   768,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   465,
       0,     0,     0,     0,   478,     0,     0,     0,     0,     0,
     478,     0,   167,     0,     0,   815,     0,     0,     0,     0,
       0,     0,     0,   209,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   347,     0,
     465,   839,     0,     0,   617,  1445,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   465,   465,     0,     0,     0,   347,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   465,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   883,     0,     0,     0,     0,     0,     0,     0,
       0,   511,  1498,     0,     0,     0,     0,     0,     0,     0,
     511,  1507,   167,   617,     0,     0,     0,     0,     0,     0,
       0,   450,     0,     0,   347,   347,     0,     0,     0,     0,
       0,     0,     0,     0,   914,     0,     0,   465,     0,     0,
       0,     0,     0,     0,   209,     0,   918,   919,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   794,     0,   926,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   948,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   352,     0,     0,
       0,     0,     0,   815,   968,     0,     0,   970,     0,   972,
       0,     0,     0,     0,     0,   981,     0,   986,   981,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   347,
       0,     0,     0,     0,     0,  1014,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1016,     0,
       0,     0,     0,     0,     0,     0,   617,     0,     0,  1025,
       0,     0,  1023,  1024,     0,     0,     0,     0,  1028,     0,
       0,     0,     0,   450,     0,     0,  1014,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1049,
       0,     0,  1052,  1053,     0,  1056,     0,  1058,  1059,     0,
       0,     0,     0,  1078,     0,     0,   478,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     465,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1099,     0,     0,     0,
    1103,     0,  1109,     0,     0,     0,   511,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   511,   693,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     421,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1216,  1218,  1209,  1210,     0,     0,     0,
       0,   450,     0,     0,     0,     0,     0,   263,     0,  1226,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   347,     0,     0,     0,     0,     0,   209,   981,
       0,     0,     0,     0,     0,     0,     0,     0,   610,     0,
       0,     0,     0,  1014,     0,     0,     0,     0,     0,     0,
       0,  1257,     0,     0,     0,     0,     0,     0,   981,     0,
       0,     0,     0,     0,     0,     0,     0,   352,     0,     0,
       0,   693,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   347,   347,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   511,   511,     0,
       0,     0,     0,     0,   478,     0,     0,     0,     0,     0,
       0,     0,     0,   511,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   465,   465,     0,     0,     0,     0,
    1226,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   478,     0,  1325,     0,  1328,     0,     0,   693,   693,
     693,  1326,   693,   693,     0,     0,     0,     0,  1333,   469,
       0,  1337,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   511,
       0,     0,     0,     0,     0,     0,     0,   511,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   263,     0,     0,
       0,     0,     0,   251,  1398,  1398,     0,     0,     0,     0,
       0,     0,     0,    14,    15,    16,    17,    18,     0,   352,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,  -458,  -458,     0,
    -458,    46,   347,    47,     0,  -458,   511,  1987,     0,     0,
     511,     0,     0,     0,     0,     0,     0,     0,  1440,     0,
       0,     0,    58,     0,  1450,     0,     0,  1443,     0,     0,
       0,     0,     0,     0,     0,  1452,  1453,     0,     0,     0,
       0,     0,     0,   450,     0,     0,     0,     0,     0,     0,
       0,   511,     0,     0,     0,     0,    63,    64,     0,     0,
     478,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   981,     0,     0,   815,     0,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   209,     0,  1496,     0,     0,     0,     0,     0,
       0,     0,     0,  1505,    76,   309,  1509,     0,  1512,  1513,
       0,     0,    78,    79,     0,     0,   511,   511,     0,     0,
       0,     0,     0,     0,     0,     0,   263,     0,     0,     0,
    1533,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1541,  1542,     0,  1539,
       0,     0,     0,     0,     0,     0,     0,     0,   511,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   352,   981,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   478,     0,     0,   815,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   693,     0,     0,     0,
       0,  2103,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1636,     0,     0,     0,     0,     0,     0,  1382,     0,
       0,   465,   465,     0,     0,   968,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1654,  1655,     0,     0,     0,
       0,     0,     0,     0,     0,   478,     0,     0,     0,   369,
       0,     0,   370,     0,   371,     0,   372,     0,     0,     0,
       0,     0,     0,   478,     0,   815,     0,     0,     0,     0,
       0,  1137,   263,   373,    -2,     0,  1139,  -243,  -243,  1140,
    1141,  1142,  1143,  1144,  1145,  1146,  1147,  1148,  1149,  1150,
    1151,  -337,  1152,  1153,  1154,  1155,  1156,     0,  1157,  1509,
     374,   375,     0,   472,     0,   377,  1158,  1159,    65,    66,
      67,    68,    69,    70,    71,   378,   379,   366,  1160,   380,
     381,   382,     0,   383,   384,     0,     0,     0,  1703,     0,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   421,     0,
    -243,   385,     0,  1731,    76,   386,     0,     0,     0,   287,
       0,   387,    78,    79,   388,   389,   390,   391,    14,    15,
      16,    17,    18,     0,     0,    20,  -184,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,  -458,  -458,   693,  -458,    46,     0,    47,     0,
    -458,     0,     0,  1777,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,   465,     0,  1786,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  2103,     0,     0,     0,     0,
    1805,    63,    64,  1807,     0,     0,     0,     0,     0,     0,
       0,     0,  1382,     0,     0,     0,     0,     0,     0,   693,
       0,     0,   469,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1832,     0,
       0,     0,     0,   369,     0,     0,   370,     0,   371,    76,
     372,     0,     0,     0,     0,     0,     0,  1840,  1841,     0,
       0,     0,     0,     0,     0,  1137,     0,   373,    -2,  1845,
    1139,  -244,  -244,  1140,  1141,  1142,  1143,  1144,  1145,  1146,
    1147,  1148,  1149,  1150,  1151,  -337,  1152,  1153,  1154,  1155,
    1156,     0,  1157,     0,   374,   375,     0,   472,     0,   377,
    1158,  1159,    65,    66,    67,    68,    69,    70,    71,   378,
     379,   366,  1160,   380,   381,   382,     0,   383,   384,     0,
    1784,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1382,     0,     0,
       0,     0,     0,     0,  -244,   385,     0,     0,    76,   386,
       0,     0,     0,   287,     0,   387,    78,    79,   388,   389,
     390,   391,     0,     0,     0,     0,     0,     0,   369,     0,
    -184,   370,     0,   371,  1917,   372,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1137,     0,   373,    -2,     0,  1139,     0,     0,  1140,  1141,
    1142,  1143,  1144,  1145,  1146,  1147,  1148,  1149,  1150,  1151,
    -337,  1152,  1153,  1154,  1155,  1156,     0,  1157,   981,   374,
     375,     0,   472,     0,   377,  1158,  1159,    65,    66,    67,
      68,    69,    70,    71,   378,   379,   366,  1160,   380,   381,
     382,     0,   383,   384,     0,     0,     0,     0,     0,  1985,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     385,     0,     0,    76,   386,     0,     0,     0,   287,     0,
     387,    78,    79,   388,   389,   390,   391,     0,     0,     0,
       0,     0,     0,     0,     0,  -184,     4,   181,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,  1136,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,   369,     0,    46,   370,    47,   371,     0,   372,
      48,    49,    50,    51,    52,    53,    54,    55,    56,     0,
       0,     0,    57,     0,  1137,    58,  1138,    -2,     0,  1139,
       0,     0,  1140,  1141,  1142,  1143,  1144,  1145,  1146,  1147,
    1148,  1149,  1150,  1151,  -337,  1152,  1153,  1154,  1155,  1156,
       0,  1157,     0,   374,   375,    61,   472,     0,   377,  1158,
    1159,    65,    66,    67,    68,    69,    70,    71,   378,   379,
     366,  1160,   380,   381,   382,     0,   383,   384,     0,     0,
       0,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    -3,   385,     0,     0,    76,   417,     0,
       0,     0,   287,     0,   387,    78,    79,   388,   389,   390,
     391,     0,     0,     0,     0,     0,     0,     0,     0,  -184,
       4,   181,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,  1136,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,   369,     0,    46,   370,
      47,   371,     0,   372,    48,    49,    50,    51,    52,    53,
      54,    55,    56,     0,     0,     0,    57,     0,  1137,    58,
    1138,    -2,     0,  1139,     0,     0,  1140,  1141,  1142,  1143,
    1144,  1145,  1146,  1147,  1148,  1149,  1150,  1151,  -337,  1152,
    1153,  1154,  1155,  1156,     0,  1157,     0,   374,   375,    61,
     472,     0,   377,  1158,  1159,    65,    66,    67,    68,    69,
      70,    71,   378,   379,   366,  1160,   380,   381,   382,     0,
     383,   384,     0,     0,     0,     0,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   385,     0,
       0,    76,   417,     0,     0,     0,   287,     0,   387,    78,
      79,   388,   389,   390,   391,     0,     0,     0,     0,     0,
       0,     0,     0,  -184,     4,   181,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
     369,     0,    46,   370,    47,   371,     0,   372,    48,    49,
      50,    51,    52,    53,    54,    55,    56,     0,     0,     0,
      57,     0,     0,    58,   373,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   374,   375,    61,   376,     0,   377,    63,    64,    65,
      66,    67,    68,    69,    70,    71,   378,   379,   366,     0,
     380,   381,   382,     0,   383,   384,     0,     0,     0,     0,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1595,  1596,  1597,     0,
       0,     0,   385,  1598,  1599,    76,   417,     0,     0,     0,
       0,     0,   387,    78,    79,   388,   389,   390,   391,     0,
       0,     0,     0,     0,     0,     0,     0,  1600,     4,   181,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,   369,     0,    46,   370,    47,   371,
       0,   372,    48,    49,    50,    51,    52,    53,    54,    55,
      56,     0,     0,     0,    57,     0,     0,    58,   373,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   374,   375,    61,   376,     0,
     377,    63,    64,    65,    66,    67,    68,    69,    70,    71,
     378,   379,   366,     0,   380,   381,   382,     0,   383,   384,
       0,     0,     0,     0,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1595,  1596,  1597,     0,     0,     0,   385,  1598,     0,    76,
     417,     0,     0,     0,     0,     0,   387,    78,    79,   388,
     389,   390,   391,     0,     0,     0,     0,     0,     0,     0,
       0,  1600,     4,   181,     6,     7,     8,     9,    10,    11,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     385,     0,  1586,    76,   417,     0,     0,     0,     0,     0,
     387,    78,    79,   388,   389,   390,   391,     4,   181,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,   369,     0,    46,   370,    47,   371,     0,
     372,    48,    49,    50,    51,    52,    53,    54,    55,    56,
       0,     0,     0,    57,     0,     0,    58,   373,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   374,   375,    61,   376,     0,   377,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   378,
     379,   366,     0,   380,   381,   382,     0,   383,   384,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   385,     0,     0,    76,   417,
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
     448,    79,   388,   389,   390,   391,   181,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   369,     0,    46,   370,    47,   371,     0,   372,   327,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,   373,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   374,   375,     0,   376,     0,   377,    63,    64,
      65,    66,    67,    68,    69,    70,    71,   378,   379,   366,
       0,   380,   381,   382,     0,   383,   384,     0,     0,     0,
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   385,     0,     0,    76,  1213,     0,     0,
       0,     0,     0,   387,  1214,    79,   388,   389,   390,   391,
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
      76,   386,     0,     0,     0,     0,     0,   387,    78,    79,
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
       0,   385,     0,     0,    76,   447,     0,     0,     0,     0,
       0,   387,    78,    79,   388,   389,   390,   391,  1926,     0,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,     0,    -2,     0,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,     0,    -2,    -2,     0,
      -2,     0,    -2,     0,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,     0,     0,     0,    -2,     0,     0,    -2,
       0,     0,     0,     0,    -2,    -2,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    -2,
       0,     0,     0,    -2,    -2,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    -2,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    -2,     0,     0,
       0,    -2,    -2,     0,     0,     0,     0,     0,     0,    -2,
      -2,  1954,     0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,
      -2,     0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,
      -2,    -2,     0,    -2,     0,    -2,     0,    -2,    -2,    -2,
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
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,    48,    49,
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
       0,     0,     0,    78,    79,   251,   181,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,  -458,
    -458,     0,  -458,    46,     0,    47,     0,  -458,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   147,     0,     0,    63,    64,
      65,    66,    67,    68,    69,    70,    71,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,    75,     0,    76,   252,     0,     0,
       0,  -782,     0,     0,    78,    79,     4,   181,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
      48,    49,    50,    51,    52,    53,    54,    55,    56,     0,
       0,     0,    57,     0,     0,    58,     0,     0,     0,     0,
    -390,  -390,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    61,     0,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -390,     0,     0,     0,    76,    77,     0,
       0,     0,     0,     0,     0,    78,    79,     4,   181,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,    48,    49,    50,    51,    52,    53,    54,    55,    56,
       0,     0,     0,    57,     0,     0,    58,     0,     0,     0,
       0,  -391,  -391,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    61,     0,     0,     0,
      63,    64,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -391,     0,     0,     0,    76,    77,
       0,     0,     0,     0,     0,     0,    78,    79,   251,   181,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,     0,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,  -458,  -458,     0,  -458,    46,     0,    47,     0,
    -458,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   147,     0,
       0,    63,    64,    65,    66,    67,    68,    69,    70,    71,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,    75,     0,    76,
     252,     0,  1358,     0,     0,     0,     0,    78,    79,  1359,
       0,     0,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,  1360,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    61,     0,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1361,
       0,     0,     0,    76,   944,     0,  1358,     0,     0,     0,
       0,    78,    79,  1359,     0,     0,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,  1360,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    61,     0,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1550,     0,     0,     0,    76,   944,     0,
    1358,     0,     0,     0,     0,    78,    79,  1359,     0,     0,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,     0,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
    1360,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    61,
       0,     0,     0,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1551,     0,     0,
       0,    76,   944,     0,  1358,     0,     0,     0,     0,    78,
      79,  1359,     0,     0,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,  1360,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    61,     0,     0,     0,    63,    64,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1552,     0,     0,     0,    76,   944,     0,     0,     0,
       0,     0,     0,    78,    79,   181,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
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
       0,     0,    74,    75,     0,    76,   252,     0,     0,     0,
    -786,     0,     0,    78,    79,   181,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
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
       0,     0,    74,    75,     0,    76,   252,     0,     0,     0,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    73,     0,  1069,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -648,    76,   329,     0,     0,     0,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   328,    76,   329,     0,     0,     0,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    73,     0,  1823,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    76,   329,     0,     0,     0,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    73,     0,  1825,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    76,   329,     0,     0,     0,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    76,   309,     0,     0,     0,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    76,   329,     0,     0,     0,
       0,     0,     0,    78,    79,   181,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,  -458,  -458,
       0,  -458,    46,     0,    47,     0,  -458,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    63,    64,     0,
       0,     0,  1382,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   369,     0,     0,   370,     0,   371,     0,
     372,     0,     0,     0,     0,    76,   252,     0,     0,     0,
       0,     0,     0,    78,    79,  1137,     0,   373,     0,     0,
    1139,  1848,  1849,  1140,  1141,  1142,  1143,  1144,  1145,  1146,
    1147,  1148,  1149,  1150,  1151,  -337,  1152,  1153,  1154,  1155,
    1156,     0,  1157,     0,   374,   375,     0,   472,     0,   377,
    1158,  1159,    65,    66,    67,    68,    69,    70,    71,   378,
     379,   366,  1160,   380,   381,   382,  1382,   383,   384,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   385,     0,   369,    76,   386,
     370,     0,   371,   287,   372,   387,    78,    79,   388,   389,
     390,   391,     0,     0,     0,     0,     0,     0,     0,  1137,
    -184,   373,     0,     0,  1139,     0,     0,  1140,  1141,  1142,
    1143,  1144,  1145,  1146,  1147,  1148,  1149,  1150,  1151,  -337,
    1152,  1153,  1154,  1155,  1156,     0,  1157,     0,   374,   375,
       0,   472,     0,   377,  1158,  1159,    65,    66,    67,    68,
      69,    70,    71,   378,   379,   366,  1160,   380,   381,   382,
       0,   383,   384,     0,     0,     0,     0,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   385,
       0,     0,    76,   386,     0,     0,     0,   287,     0,   387,
      78,    79,   388,   389,   390,   391,     0,     0,     0,     0,
       0,     0,     0,     0,  -184,    14,    15,    16,    17,    18,
      19,   680,    20,   681,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   369,     0,    46,   370,    47,   371,     0,   372,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,   373,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   682,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   374,   375,     0,   376,     0,   377,    63,    64,
      65,    66,    67,    68,    69,    70,    71,   378,   379,   366,
       0,   380,   381,   382,     0,   383,   384,     0,     0,     0,
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   385,     0,     0,    76,   683,     0,     0,
       0,   287,     0,   387,    78,    79,   684,   685,   390,   391,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,   369,     0,    46,   370,
      47,   371,     0,   372,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
     373,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   374,   375,     0,
     376,     0,   377,    63,    64,    65,    66,    67,    68,    69,
      70,    71,   378,   379,   366,     0,   380,   381,   382,     0,
     383,   384,     0,     0,     0,     0,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   385,     0,
     416,    76,   417,     0,     0,     0,     0,     0,   387,    78,
      79,   388,   389,   390,   391,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   369,     0,    46,   370,    47,   371,     0,   372,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,   373,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   374,   375,     0,   376,     0,   377,    63,    64,
      65,    66,    67,    68,    69,    70,    71,   378,   379,   366,
       0,   380,   381,   382,     0,   383,   384,     0,     0,     0,
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   385,     0,     0,    76,   683,     0,     0,
       0,   287,     0,   387,    78,    79,   388,   389,   390,   391,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,   369,     0,    46,   370,
      47,   371,     0,   372,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
     373,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   374,   375,     0,
     376,     0,   377,    63,    64,    65,    66,    67,    68,    69,
      70,    71,   378,   379,   366,     0,   380,   381,   382,     0,
     383,   384,     0,     0,     0,     0,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   385,     0,
       0,    76,   417,     0,     0,     0,     0,     0,   387,    78,
      79,   388,   389,   390,   391,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   369,     0,    46,   370,    47,   371,     0,   372,   327,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,   373,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   374,   375,     0,   376,     0,   377,    63,    64,
      65,    66,    67,    68,    69,    70,    71,   378,   379,   366,
       0,   380,   381,   382,     0,   383,   384,     0,     0,     0,
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   385,     0,     0,    76,   447,     0,     0,
       0,     0,     0,   387,    78,    79,   388,   389,   390,   391,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,   369,     0,    46,   370,
      47,   371,     0,   372,   327,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
     373,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   374,   375,     0,
     376,     0,   377,    63,    64,    65,    66,    67,    68,    69,
      70,    71,   378,   379,   366,     0,   380,   381,   382,     0,
     383,   384,     0,     0,     0,     0,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   385,     0,
       0,    76,   386,     0,     0,     0,     0,     0,   387,    78,
      79,   388,   389,   390,   391,   181,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,     0,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
       0,     0,    46,     0,    47,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    58,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   671,     0,     0,   672,   673,     0,
     566,   181,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,   -16,
      47,     0,     0,     0,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   251,   181,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    63,    64,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,  -458,  -458,     0,  -458,    46,     0,    47,     0,
    -458,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    76,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    63,    64,     0,     0,     0,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,    76,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   147,     0,     0,    63,
      64,    65,    66,    67,    68,    69,    70,    71,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,    75,     0,    76,    77,     0,
       0,     0,  -784,     0,     0,    78,    79,    14,    15,    16,
      17,    18,    19,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,     0,    47,     0,     0,
       0,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   147,     0,     0,
      63,    64,    65,    66,    67,    68,    69,    70,    71,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,    75,     0,    76,    77,
       0,     0,     0,     0,     0,     0,    78,    79,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,     0,     0,    46,     0,    47,     0,
       0,     0,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   147,     0,
       0,    63,    64,    65,    66,    67,    68,    69,    70,    71,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    76,
      77,     0,     0,     0,     0,     0,     0,    78,    79,   181,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
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
       0,     0,     0,     0,     0,     0,     0,     0,   868,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -661,    76,
     181,     6,     7,     8,     9,    10,    11,    12,    13,    14,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1739,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    14,    15,    16,    17,    18,     0,     0,    20,
      76,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,  -458,  -458,     0,  -458,
      46,     0,    47,     0,  -458,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   147,     0,     0,    63,    64,    65,    66,    67,
      68,    69,    70,    71,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,    75,     0,    76,   309,     0,     0,     0,     0,     0,
       0,    78,    79,   181,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,   327,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    63,    64,     0,     0,     0,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,     0,    76,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     950,    76,   944,     0,     0,     0,     0,     0,     0,    78,
      79,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,     0,     0,     0,     0,     0,  1465,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    76,   944,     0,     0,     0,     0,     0,     0,
      78,    79,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,     0,    47,     0,     0,     0,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    76,   294,     0,     0,     0,     0,     0,
       0,    78,    79,    14,    15,    16,    17,    18,    19,     0,
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
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    76,    77,     0,     0,     0,     0,
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
       0,     0,     0,     0,     0,    76,   443,     0,     0,     0,
       0,     0,     0,    78,    79,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,   327,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    76,   329,     0,     0,
       0,     0,     0,     0,    78,    79,    14,    15,    16,    17,
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
       0,     0,     0,     0,     0,     0,     0,    76,   294,     0,
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
       0,     0,     0,     0,     0,     0,     0,     0,    76,   443,
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
     309,     0,     0,     0,     0,     0,     0,    78,    79,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      76,   944,     0,     0,     0,     0,     0,     0,    78,    79,
     181,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,     0,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,  -458,  -458,     0,  -458,    46,     0,    47,
       0,  -458,     0,     0,     0,     0,     0,     0,     0,     0,
      14,    15,    16,    17,    18,    19,     0,    20,    58,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,    63,    64,   327,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      76,     0,     0,    63,    64,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    76,   944,     0,     0,     0,     0,     0,     0,    78,
      79,    14,    15,    16,    17,    18,    19,     0,    20,     0,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    76,     0,     0,    14,    15,    16,    17,    18,
      78,    79,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,  -458,
    -458,     0,  -458,    46,     0,    47,     0,  -458,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    58,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    76,   309,     0,    14,
      15,    16,    17,    18,    78,    79,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,  -458,  -458,     0,  -458,    46,     0,    47,
       0,  -458,     0,     0,     0,     0,     0,     0,     0,     0,
      14,    15,    16,    17,    18,    19,     0,    20,    58,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,     0,    63,    64,   327,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      76,     0,     0,    63,    64,     0,     0,     0,    78,    79,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,   369,
       0,    46,   370,    47,   371,     0,   372,     0,     0,     0,
       0,    76,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   373,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     374,   375,     0,   376,     0,   377,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   378,   379,   366,     0,   380,
     381,   382,     0,   383,   384,     0,     0,     0,     0,     0,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   385,     0,     0,    76,   386,     0,     0,     0,     0,
       0,   387,   448,    79,   388,   389,   390,   391,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,   369,     0,    46,
     370,    47,   371,     0,   372,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   373,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   374,   375,
       0,   376,     0,   377,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   378,   379,   366,     0,   380,   381,   382,
       0,   383,   384,     0,     0,     0,     0,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   385,
       0,     0,    76,   386,     0,     0,     0,     0,     0,   387,
      78,    79,   388,   389,   390,   391,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
      48,    49,    50,    51,    52,    53,    54,    55,     0,   369,
       0,     0,   370,     0,   371,    58,   372,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   373,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   147,     0,     0,    63,
      64,    65,    66,    67,    68,    69,    70,    71,     0,     0,
     374,   375,     0,   376,     0,   377,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   378,   379,   366,     0,   380,
     381,   382,     0,   383,   384,   369,     0,     0,   370,     0,
     371,    73,   372,     0,     0,     0,     0,    76,     0,     0,
       0,     0,     0,     0,     0,  1595,  1596,  1597,     0,   373,
       0,   385,  1764,     0,    76,   386,     0,     0,     0,     0,
       0,   387,    78,    79,   388,   389,   390,   391,     0,     0,
       0,     0,     0,     0,     0,     0,   374,   375,     0,   376,
       0,   377,  1863,    64,    65,    66,    67,    68,    69,    70,
      71,   378,   379,   366,     0,   380,   381,   382,     0,   383,
     384,   369,     0,     0,   370,     0,   371,    73,   372,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1595,  1596,  1597,     0,   373,     0,   385,  1864,     0,
      76,   386,     0,     0,     0,     0,     0,   387,    78,    79,
     388,   389,   390,   391,     0,     0,     0,     0,     0,     0,
       0,     0,   374,   375,     0,   376,     0,   377,    63,    64,
      65,    66,    67,    68,    69,    70,    71,   378,   379,   366,
       0,   380,   381,   382,   369,   383,   384,   370,     0,   371,
       0,   372,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   373,     0,
       0,     0,     0,   385,  1260,     0,    76,   386,     0,     0,
       0,  1261,     0,   387,    78,    79,   388,   389,   390,   391,
       0,     0,     0,     0,     0,   374,   375,     0,   376,     0,
     377,    63,    64,    65,    66,    67,    68,    69,    70,    71,
     378,   379,   366,     0,   380,   381,   382,   369,   383,   384,
     370,     0,   371,     0,   372,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   373,     0,     0,     0,     0,   385,   977,  1577,    76,
     386,     0,     0,     0,     0,     0,   387,    78,    79,   388,
     389,   390,   391,     0,     0,     0,     0,     0,   374,   375,
       0,   376,     0,   377,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   378,   379,   366,     0,   380,   381,   382,
     369,   383,   384,   370,     0,   371,     0,   372,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   373,     0,     0,     0,     0,   385,
       0,     0,    76,   386,     0,     0,     0,   475,     0,   387,
      78,    79,   388,   389,   390,   391,     0,     0,     0,     0,
       0,   374,   375,     0,   376,     0,   377,    63,    64,    65,
      66,    67,    68,    69,    70,    71,   378,   379,   366,     0,
     380,   381,   382,   369,   383,   384,   370,     0,   371,     0,
     372,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   373,     0,     0,
       0,     0,   385,   814,     0,    76,   386,     0,     0,     0,
       0,     0,   387,    78,    79,   388,   389,   390,   391,     0,
       0,     0,     0,     0,   374,   375,     0,   376,     0,   377,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   378,
     379,   366,     0,   380,   381,   382,   369,   383,   384,   370,
       0,   371,     0,   372,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     373,     0,     0,     0,     0,   385,     0,     0,    76,   386,
       0,     0,     0,   287,     0,   387,    78,    79,   388,   389,
     390,   391,     0,     0,     0,     0,     0,   374,   375,     0,
     376,     0,   377,    63,    64,    65,    66,    67,    68,    69,
      70,    71,   378,   379,   366,     0,   380,   381,   382,   369,
     383,   384,   370,     0,   371,     0,   372,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   373,     0,     0,     0,     0,   385,   977,
       0,    76,   386,     0,     0,     0,     0,     0,   387,    78,
      79,   388,   389,   390,   391,     0,     0,     0,     0,     0,
     374,   375,     0,   376,     0,   377,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   378,   379,   366,     0,   380,
     381,   382,   369,   383,   384,   370,     0,   371,     0,   372,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   373,     0,     0,     0,
       0,   385,     0,     0,    76,   386,     0,     0,  1008,     0,
       0,   387,    78,    79,   388,   389,   390,   391,     0,     0,
       0,     0,     0,   374,   375,     0,   376,     0,   377,    63,
      64,    65,    66,    67,    68,    69,    70,    71,   378,   379,
     366,     0,   380,   381,   382,   369,   383,   384,   370,     0,
     371,     0,   372,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   373,
       0,     0,     0,     0,   385,  1327,     0,    76,   386,     0,
       0,     0,     0,     0,   387,    78,    79,   388,   389,   390,
     391,     0,     0,     0,     0,     0,   374,   375,     0,   376,
       0,   377,    63,    64,    65,    66,    67,    68,    69,    70,
      71,   378,   379,   366,     0,   380,   381,   382,   369,   383,
     384,   370,     0,   371,     0,   372,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   373,     0,     0,     0,     0,   385,     0,     0,
      76,   386,     0,     0,     0,  1392,     0,   387,    78,    79,
     388,   389,   390,   391,     0,     0,     0,     0,     0,   374,
     375,     0,   376,     0,   377,    63,    64,    65,    66,    67,
      68,    69,    70,    71,   378,   379,   366,     0,   380,   381,
     382,   369,   383,   384,   370,     0,   371,     0,   372,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   373,     0,     0,     0,     0,
     385,     0,     0,    76,   386,     0,     0,     0,  1456,     0,
     387,    78,    79,   388,   389,   390,   391,     0,     0,     0,
       0,     0,   374,   375,     0,   376,     0,   377,    63,    64,
      65,    66,    67,    68,    69,    70,    71,   378,   379,   366,
       0,   380,   381,   382,   369,   383,   384,   370,     0,   371,
       0,   372,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   373,     0,
       0,     0,     0,   385,     0,  1854,    76,   386,     0,     0,
       0,     0,     0,   387,    78,    79,   388,   389,   390,   391,
       0,     0,     0,     0,     0,   374,   375,     0,   376,     0,
     377,    63,    64,    65,    66,    67,    68,    69,    70,    71,
     378,   379,   366,     0,   380,   381,   382,   369,   383,   384,
     370,     0,   371,     0,   372,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   373,     0,     0,     0,     0,   385,  1859,     0,    76,
     386,     0,     0,     0,     0,     0,   387,    78,    79,   388,
     389,   390,   391,     0,     0,     0,     0,     0,   374,   375,
       0,   376,     0,   377,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   378,   379,   366,     0,   380,   381,   382,
     369,   383,   384,   370,     0,   371,     0,   372,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   373,     0,     0,     0,     0,   385,
    1868,     0,    76,   386,     0,     0,     0,     0,     0,   387,
      78,    79,   388,   389,   390,   391,     0,     0,     0,     0,
       0,   374,   375,     0,   376,     0,   377,    63,    64,    65,
      66,    67,    68,    69,    70,    71,   378,   379,   366,     0,
     380,   381,   382,   369,   383,   384,   370,     0,   371,     0,
     372,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   373,     0,     0,
       0,     0,   385,  1948,     0,    76,   386,     0,     0,     0,
       0,     0,   387,    78,    79,   388,   389,   390,   391,     0,
       0,     0,     0,     0,   374,   375,     0,   376,     0,   377,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   378,
     379,   366,     0,   380,   381,   382,   369,   383,   384,   370,
       0,   371,     0,   372,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     373,     0,     0,     0,     0,   385,  1950,     0,    76,   386,
       0,     0,     0,     0,     0,   387,    78,    79,   388,   389,
     390,   391,     0,     0,     0,     0,     0,   374,   375,     0,
     376,     0,   377,    63,    64,    65,    66,    67,    68,    69,
      70,    71,   378,   379,   366,     0,   380,   381,   382,   369,
     383,   384,   370,     0,   371,     0,   372,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   373,     0,     0,     0,     0,   385,  2003,
       0,    76,   386,     0,     0,     0,     0,     0,   387,    78,
      79,   388,   389,   390,   391,     0,     0,     0,     0,     0,
     374,   375,     0,   376,     0,   377,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   378,   379,   366,     0,   380,
     381,   382,   369,   383,   384,   370,     0,   371,     0,   372,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   373,     0,     0,     0,
       0,   385,  2005,     0,    76,   386,     0,     0,     0,     0,
       0,   387,    78,    79,   388,   389,   390,   391,     0,     0,
       0,     0,     0,   374,   375,     0,   376,     0,   377,    63,
      64,    65,    66,    67,    68,    69,    70,    71,   378,   379,
     366,     0,   380,   381,   382,   369,   383,   384,   370,     0,
     371,     0,   372,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   373,
       0,     0,     0,     0,   385,  2007,     0,    76,   386,     0,
       0,     0,     0,     0,   387,    78,    79,   388,   389,   390,
     391,     0,     0,     0,     0,     0,   374,   375,     0,   376,
       0,   377,    63,    64,    65,    66,    67,    68,    69,    70,
      71,   378,   379,   366,     0,   380,   381,   382,   369,   383,
     384,   370,     0,   371,     0,   372,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   373,     0,     0,     0,     0,   385,  2010,     0,
      76,   386,     0,     0,     0,     0,     0,   387,    78,    79,
     388,   389,   390,   391,     0,     0,     0,     0,     0,   374,
     375,     0,   376,     0,   377,    63,    64,    65,    66,    67,
      68,    69,    70,    71,   378,   379,   366,     0,   380,   381,
     382,   369,   383,   384,   370,     0,   371,     0,   372,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   373,     0,     0,     0,     0,
     385,  2012,     0,    76,   386,     0,     0,     0,     0,     0,
     387,    78,    79,   388,   389,   390,   391,     0,     0,     0,
       0,     0,   374,   375,     0,   376,     0,   377,    63,    64,
      65,    66,    67,    68,    69,    70,    71,   378,   379,   366,
       0,   380,   381,   382,   369,   383,   384,   370,     0,   371,
       0,   372,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   373,     0,
       0,     0,     0,   385,  2055,     0,    76,   386,     0,     0,
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
       0,     0,   385,  2083,     0,    76,   386,     0,     0,     0,
       0,     0,   387,    78,    79,   388,   389,   390,   391,     0,
       0,     0,     0,     0,   374,   375,     0,   376,     0,   377,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   378,
     379,   366,     0,   380,   381,   382,   369,   383,   384,   370,
       0,   371,     0,   372,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     373,     0,     0,     0,     0,   385,  2085,     0,    76,   386,
       0,     0,     0,     0,     0,   387,    78,    79,   388,   389,
     390,   391,     0,     0,     0,     0,     0,   374,   375,     0,
     376,     0,   377,    63,    64,    65,    66,    67,    68,    69,
      70,    71,   378,   379,   366,     0,   380,   381,   382,   369,
     383,   384,   370,     0,   371,     0,   372,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   373,     0,     0,     0,     0,   385,  2087,
       0,    76,   386,     0,     0,     0,     0,     0,   387,    78,
      79,   388,   389,   390,   391,     0,     0,     0,     0,     0,
     374,   375,     0,   376,     0,   377,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   378,   379,   366,     0,   380,
     381,   382,   369,   383,   384,   370,     0,   371,     0,   372,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   373,     0,     0,     0,
       0,   385,     0,     0,    76,   386,     0,     0,     0,     0,
       0,   387,    78,    79,   388,   389,   390,   391,     0,     0,
       0,     0,     0,   374,   375,     0,   376,     0,   377,    63,
      64,    65,    66,    67,    68,    69,    70,    71,   378,   379,
     366,     0,   380,   381,   382,   369,   383,   384,   370,     0,
     371,     0,   372,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   373,
       0,     0,     0,     0,   662,     0,     0,    76,   386,     0,
       0,     0,     0,     0,   387,    78,    79,   388,   389,   390,
     391,     0,     0,     0,     0,     0,   374,   375,     0,   376,
       0,   377,    63,    64,    65,    66,    67,    68,    69,    70,
      71,   378,   379,   366,     0,   380,   381,   382,   369,   383,
     384,   370,     0,   371,     0,   372,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   373,     0,     0,     0,     0,   668,     0,     0,
      76,   386,     0,     0,     0,     0,     0,   387,    78,    79,
     388,   389,   390,   391,     0,     0,     0,     0,     0,   374,
     375,     0,   376,     0,   377,    63,    64,    65,    66,    67,
      68,    69,    70,    71,   378,   379,   366,     0,   380,   381,
     382,   369,   383,   384,   370,     0,   371,     0,   372,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   373,     0,     0,     0,     0,
     677,     0,     0,    76,   386,     0,     0,     0,     0,     0,
     387,    78,    79,   388,   389,   390,   391,     0,     0,     0,
       0,     0,   374,   375,     0,   376,     0,   377,    63,    64,
      65,    66,    67,    68,    69,    70,    71,   378,   379,   366,
       0,   380,   381,   382,   369,   383,   384,   370,     0,   371,
       0,   372,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   373,     0,
       0,     0,     0,   385,     0,     0,    76,   386,     0,     0,
       0,     0,     0,   387,   882,    79,   388,   389,   390,   391,
       0,     0,     0,     0,     0,   374,   375,     0,   376,     0,
     377,    63,    64,    65,    66,    67,    68,    69,    70,    71,
     378,   379,   366,     0,   380,   381,   382,   369,   383,   384,
     370,     0,   371,     0,   372,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   373,     0,     0,     0,     0,   385,     0,     0,    76,
     386,     0,     0,     0,     0,     0,   387,   448,    79,   388,
     389,   390,   391,     0,     0,     0,     0,     0,   374,   375,
       0,   376,     0,   377,  1943,    64,    65,    66,    67,    68,
      69,    70,    71,   378,   379,   366,     0,   380,   381,   382,
       0,   383,   384,     0,     0,     0,     0,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   385,
       0,     0,    76,   386,     0,     0,     0,     0,     0,   387,
      78,    79,   388,   389,   390,   391,   181,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
       0,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,   181,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,     0,     0,    20,    58,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,   182,    47,     0,   183,   184,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    58,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   671,     0,     0,   672,
     673,    14,    15,    16,    17,    18,     0,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,  -457,  -457,     0,  -457,    46,
       0,    47,     0,  -457,     0,     0,     0,     0,     0,     0,
       0,     0,    14,    15,    16,    17,    18,     0,     0,    20,
      58,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,  -458,  -458,     0,  -458,
      46,     0,    47,     0,  -458,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    58
};

static const yytype_int16 yycheck[] =
{
       1,   709,     1,     4,    74,   132,    74,    74,   175,   891,
     250,   385,   164,   359,    74,     4,   291,   475,   625,   231,
     152,  1146,   347,   264,   228,   616,   351,     1,   228,   180,
     228,   164,   694,   612,   524,   525,     1,   180,     1,   216,
      96,     4,    59,   612,   330,    76,   229,   877,  1714,  1311,
    1312,   612,  1783,   783,   687,    56,    57,   164,    59,   789,
      59,   347,   753,   983,   140,   351,   228,   228,    74,   228,
     165,   228,   782,    74,  1714,     1,  1714,   332,  1852,   214,
      89,     1,    83,   238,    83,    59,   298,   299,    74,   387,
      91,    74,  1012,   220,    59,    96,    59,    96,    99,   303,
      99,   898,   103,   303,   103,   303,  1384,   262,    72,   877,
     800,   782,     1,   245,   103,  1384,  1385,   272,  1715,   780,
      83,   304,   190,   190,   200,   179,   780,   861,   818,   146,
     190,  1848,  1605,    59,    99,   780,   117,   934,    72,    59,
     141,   303,   303,   144,   303,   146,   303,   146,  1601,   780,
     159,   152,    75,   361,   888,  1075,   364,   158,   228,   649,
     228,   228,    75,   449,   165,   292,    61,    62,   228,   117,
       1,     1,   146,   229,   155,   618,     1,   140,   583,     4,
      89,   146,    84,   146,   190,  1269,   149,   151,   189,   190,
     189,   782,  1120,    98,   884,   799,   800,  1125,    97,   324,
     455,   780,   145,   204,   190,   204,   535,   190,    72,   201,
      75,   780,     0,   214,   818,    72,   545,   151,   219,   780,
     146,   155,   228,   330,   167,    90,   146,   228,   229,    59,
     229,    72,   940,   303,    59,   303,   303,   200,   363,  1836,
      83,     1,   228,   303,   245,   228,   151,   122,   304,   151,
     159,   282,   253,   176,   253,   155,   385,   586,   385,  1130,
     160,   969,   263,   176,   159,   266,   507,   266,  1196,   158,
     760,  2045,   273,   103,  1991,   154,   601,   176,   103,   154,
     884,   176,   283,   284,    10,   286,  1759,   151,   251,   493,
     253,   155,    10,   493,   151,   493,   175,   572,   155,    59,
     646,   266,   303,   304,   481,   304,   149,  2081,  1138,  1039,
     311,  1764,  1765,   638,   155,   601,   146,   318,   319,   502,
     132,   146,   323,     0,   915,   958,   157,   158,   291,   151,
    1021,   493,   493,   707,   493,  1045,   493,  1167,   473,   625,
     475,   784,   449,  2074,   133,   788,   157,   310,   133,   604,
    1628,    89,   638,   175,   797,   798,   167,  2023,   453,  1628,
     361,  1958,   174,   364,   365,   291,   664,   332,   151,   445,
    1138,   291,     0,   628,  1045,   579,   159,  1038,   167,   579,
     635,   579,   167,  2023,  1038,  2023,   146,   514,  1122,   544,
     522,   159,   519,  1038,   799,   800,   528,  1087,   581,  1167,
    1484,  1485,  1486,   151,   273,   106,   107,  1038,   176,   536,
     253,  1864,  1865,   818,  1130,   284,    72,   579,   579,   546,
     579,   159,   579,    72,  1352,   493,   493,  2024,   429,  1130,
     429,  1228,   150,   493,    72,   161,  1698,  1699,   157,  1236,
     166,    58,   473,   161,    61,    62,   502,    64,   166,    72,
     583,   452,   453,   154,  1045,   159,   419,   176,    83,  1038,
     151,   291,   166,   464,   465,    75,    20,   310,  2065,  1038,
     237,   644,   473,   240,   475,  1600,   583,  1038,   157,   884,
    1605,    91,   445,  1087,  1355,  1356,  1357,   493,    89,   151,
     455,  1221,   493,  1762,   261,   151,    72,   176,   108,   155,
     151,   502,   151,   502,   271,   600,   155,   493,     4,    72,
     493,   579,   579,   151,   156,    77,    78,   155,   625,   579,
     157,   522,    72,    72,   149,   581,   176,   528,   151,   858,
     167,   291,   155,   662,   151,   662,   665,   666,   530,   668,
     151,   668,    72,   157,    77,    78,   151,   373,   677,   954,
     677,   680,   681,   682,  1384,  1385,   157,  1015,   159,  1479,
      56,    57,   176,   188,   157,   566,   157,   568,   585,   696,
    1300,   157,   398,   399,   157,   151,   419,   902,   579,   155,
     581,    72,   581,   176,   585,   176,   585,   151,   151,   153,
     176,    72,   155,   419,   595,    91,   157,   159,   599,   600,
      72,   151,   151,   566,   569,   155,   155,   133,  1215,   572,
     602,   585,   159,     3,   869,   176,   902,  1279,    72,  1062,
     585,   151,   585,   449,   148,   155,   159,     3,   253,   176,
     807,   632,   805,   176,  1759,   159,   162,   163,   157,  1355,
    1356,  1357,   793,   154,   645,   141,   572,  1909,   144,   904,
     793,   175,   572,   804,  1355,  1356,  1357,   176,  1927,   585,
     151,    72,   158,   628,   155,   585,  1374,   157,   157,   165,
     151,   157,   807,   151,   155,   810,   602,  1559,   167,   151,
     734,   175,  1087,   155,   157,   310,  1955,   782,   820,   157,
     176,    13,    14,    15,    16,    17,   131,   151,   699,   159,
     701,   155,   703,   176,   735,   330,   707,   839,   176,   710,
      13,    14,    15,    16,    17,  1205,   151,   159,   845,   153,
     155,  1994,   159,   219,   166,   157,  1995,   162,   163,   856,
     164,   165,   859,  1395,   735,   167,   863,  1862,   127,   128,
     570,   153,   572,     1,   736,   157,     4,  2020,   515,  1874,
      72,   151,    83,   155,   157,   585,   151,   153,   160,   159,
     585,  1681,   158,  1683,   167,    96,   166,   263,    99,    72,
     537,   150,   103,   157,  2047,   159,   543,   273,   157,   780,
     547,   782,   171,   172,   162,   777,   840,   283,   284,   157,
     286,   169,   170,   794,   419,   151,   159,   152,  1628,   167,
     801,    59,   158,  1261,   159,   153,   807,   799,   800,   810,
     158,   133,   572,   173,  1476,   311,   151,  1942,   819,   820,
     821,   954,   318,   319,   449,   585,   818,   323,   153,   150,
     133,   926,   157,  1074,    47,    48,   157,    50,   839,  1244,
     975,    99,    55,   151,   153,   103,   155,   954,   983,  1646,
    1647,   777,    13,    14,    15,    16,    17,  1008,   189,   145,
     146,   147,   153,  1209,  1210,   361,   153,   158,   364,   117,
     157,   157,  1463,   204,   875,   876,   877,  1012,   106,   107,
    1015,   167,    13,    14,    15,    16,    17,   713,   146,   153,
     176,   153,   884,   157,  1106,    22,   158,   153,   229,   524,
     525,   890,   153,   877,   869,  1082,  1311,     3,   145,   146,
     147,    72,   877,   153,   877,   880,   151,    13,    14,    15,
      16,    17,   253,   131,   151,   926,   153,   890,   155,  1215,
     167,   153,  1762,  1084,  1392,   266,   106,   107,   939,   904,
    1075,    72,   151,   151,   153,   151,   155,   155,     3,  1044,
    1045,   877,   153,   154,   162,   163,   452,   877,    13,    14,
      15,    16,    17,   153,  1219,   164,   165,   175,   464,   465,
     131,   714,   715,   716,   975,   131,    72,    58,   125,   126,
      61,    62,   983,    64,  1224,  1359,   721,   722,   723,   724,
     151,   145,   146,   147,   155,   151,   153,  1124,  1456,   155,
     625,   162,   163,  1800,   157,   151,   162,   163,   266,   155,
    1137,  1012,   151,   167,  1015,   151,  1474,    72,   131,   154,
     155,    62,   176,   158,   649,   153,   151,  1154,   153,   157,
     155,  1436,   151,   157,   153,  1208,   155,  1038,   151,  1206,
    1207,   153,   155,  1044,  1045,   157,   157,   877,   157,   162,
     163,   153,   877,   156,   157,   157,   157,    13,    99,   110,
     111,   112,   113,   114,  1237,   890,    98,  1199,   153,   110,
     566,   112,   157,   114,  1075,   159,   151,   153,   151,   103,
     155,   157,   155,  1234,   108,   109,   110,   111,   112,   113,
     114,  1234,   151,   860,    89,  1087,   155,  1927,   429,   595,
     150,  1342,  1899,   599,   600,   162,   163,  1453,  1215,  1384,
    1243,  1244,   153,  1286,   155,   156,   153,   877,   153,  1248,
     157,   151,   157,   151,   950,  1955,  1261,   151,   152,   955,
     159,  1282,    88,   159,  1269,   760,   632,  1244,   156,  1140,
     966,   151,  1143,  1144,  1145,   155,   153,   103,   175,   645,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   153,
    1312,   129,   130,   204,  1138,  1995,  1167,  1130,  1319,  1320,
     153,   502,  1173,  1138,   157,  1138,  1319,  1320,   156,   157,
    1181,  1889,  1365,  1184,  1185,  1184,  1185,  1188,   153,   131,
     153,   153,   157,  1167,   157,   157,  1185,  2028,  1199,   156,
     157,  2032,  1167,   699,  1167,   701,   153,   703,   166,   151,
     157,   707,  1138,   155,   710,  1550,  1551,  1552,  1138,  1184,
     162,   163,   153,   156,   157,   266,   157,   268,   269,   153,
     117,  1232,   151,   157,   145,   146,   147,   151,   153,   735,
     157,  1167,   157,   151,  1245,   151,   157,  1167,  1375,  1376,
    1127,  1128,  1129,   163,  1219,   296,   167,  1392,   153,   153,
    1261,   302,   157,   157,   168,   176,   891,   131,  1269,     4,
       5,     6,     7,     8,     9,    10,    11,    12,  1451,    13,
      14,    15,    16,    17,    18,  1458,  1413,  1130,   153,   161,
     153,   332,   157,  1698,   157,   156,   157,   338,   794,   340,
    1301,   154,  1475,  1436,   173,   801,   153,   153,  1138,  1365,
     157,   157,  1138,  1138,  1660,   156,   157,   157,   158,  1311,
    1312,  1456,   153,   819,   153,   821,   153,   585,   153,  1436,
     156,   157,   156,   157,   153,   376,   153,  1167,   156,  1474,
     156,   157,  1167,   155,  1479,   156,   157,   156,   157,  1484,
    1485,  1486,   158,  1628,   133,  1185,   156,   157,   133,  1563,
    1185,   156,   157,  1563,  1365,  1563,   156,   157,  1369,  1358,
     158,  1372,   156,   157,  1557,   156,   157,   157,  1138,   875,
     876,   877,  1565,   156,   157,   156,   157,   151,   429,  1215,
     153,  1392,  1355,  1356,  1357,  1358,  1359,  1360,   156,   157,
     153,  1563,  1563,   153,  1563,   153,  1563,  1167,   157,   158,
     153,  1412,   153,  1414,   455,  1414,   457,   458,   153,  1384,
    1385,  1384,  1385,   156,   157,  1251,  1252,  1253,   156,   157,
     926,   472,  1258,  1259,    77,    78,   120,  1610,   122,   123,
     124,   176,    13,    14,    15,    16,    17,    18,   151,  1414,
     157,   158,  1270,  1271,   156,  1456,   717,   718,  1384,  1385,
     155,   502,   719,   720,  1384,  1385,   159,   151,  1570,  1571,
     154,   155,  1461,  1474,   159,   159,   160,   159,  1479,   725,
     726,   522,   159,  1484,  1485,  1486,   527,  1639,   529,  1206,
    1207,  1664,  1619,  1563,  1454,  1455,    70,   156,  1461,   151,
      78,  1557,  1675,   156,    18,   175,  1639,   157,   549,  1565,
     551,   552,  1355,  1356,  1357,   159,  1359,  1360,    13,    14,
      15,    16,    17,    18,   151,  1729,   159,   176,   569,  1729,
     153,  1729,  1639,  1358,    13,    14,    15,    16,    17,    18,
     581,   153,   176,   159,  1673,   156,  1681,  1699,  1683,  1732,
     156,    18,   150,   153,  1384,  1385,  1557,    62,   153,  1384,
    1385,   153,  1563,   604,  1565,   606,   607,  1729,  1729,   153,
    1729,  1572,  1729,  1627,   153,   176,   153,   153,   153,   159,
    1205,   150,    70,  1575,   561,  1586,   159,   628,   629,   159,
    1215,   153,   175,     1,   635,  1881,     4,   153,  1599,   104,
     153,   153,   150,   108,   103,  1594,   111,   175,   113,   108,
     109,   110,   111,   112,   113,   114,   115,  1790,  1791,   877,
     159,   159,   153,   153,  1384,  1385,   175,   153,  1593,   157,
     157,   153,   890,   157,   153,  1636,  1461,   153,   153,   156,
     153,   153,   153,   150,  1140,   153,   156,  1143,  1144,  1145,
     153,    59,   153,  1642,   156,   153,   155,  1789,    14,   153,
     153,   153,  1845,  1628,   153,  1628,    74,   153,   153,   151,
    1956,  1167,   153,   150,   157,    83,  1732,  1173,   153,  1642,
    1681,   157,  1683,   151,   158,  1181,   151,    78,    96,   151,
     151,    99,  1188,   151,    74,   103,   176,  1848,   157,    91,
     156,   158,  1628,   176,  1959,  1848,  1698,  1699,  1628,   156,
     176,   150,   103,   176,   159,   106,   107,   108,   109,   110,
     111,   112,   113,   114,  1778,   157,   176,  1900,  1729,   153,
     156,  1732,   140,   153,   157,   157,  1232,   153,   146,   157,
     150,   149,  1743,   156,   152,   153,  1747,   153,   153,  1245,
     150,   256,   156,   151,   151,   176,   164,  1909,   153,   176,
    1761,    80,   176,   151,  1594,   176,   150,   176,   103,  1594,
    1771,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     188,   189,   190,   176,  1749,   176,   176,  1788,  1789,  1845,
     176,   176,   200,   201,   151,   150,   204,  1762,  1628,  1762,
     150,   159,   150,  1628,   103,  1978,   157,  1633,   157,   108,
     109,   110,   111,   112,   113,   114,   159,  1642,   156,  2023,
     228,   229,   156,  2023,   156,  2023,  1958,   162,   869,   334,
     335,  1885,   337,   153,   339,   156,  1762,   245,  1989,   880,
    1991,  2024,  1762,   150,  1845,   253,  1989,   153,  1991,   158,
     158,  1852,   120,  1184,  1185,  1856,   150,   153,   266,   156,
    1861,  2023,  2023,   904,  2023,   153,  2023,   153,  1628,   846,
     153,   376,   156,  1369,   915,   153,  1372,   150,   176,  2030,
    1138,   158,  2065,   924,   153,  1886,   157,  2030,   296,   151,
     153,   157,   151,   151,   302,   303,   304,   109,   150,   156,
     156,   156,   310,   150,   153,   159,   150,   153,   153,  1167,
     153,   153,  2063,   153,   153,   156,  1412,  1909,    75,    75,
     150,   153,   330,   331,   332,   176,  1184,  1185,   176,  1930,
     907,   151,  1762,  1934,  1559,   153,    90,  1762,   176,   347,
     156,   156,   159,   351,   150,   150,  1947,   150,   155,   153,
      75,   153,   153,  2023,   153,  2023,  2023,  1958,   153,  1960,
     153,  2112,  1927,  2023,  1927,  2092,   154,   472,  2024,  2112,
    1971,   167,  1973,  1974,  2028,   167,   176,   385,  2032,  2033,
    1021,  2108,    75,   158,   150,   176,   176,   150,   153,   153,
    1955,   153,  1955,   153,   152,   150,   157,  1998,   167,   167,
     150,  1927,  1762,   103,  1045,   158,   151,  1927,   157,  2065,
      75,   419,   151,  2067,   422,   150,   167,   176,   152,   167,
     156,   429,  2023,  2024,   176,  2024,   109,   109,   150,  1955,
    1995,   152,  1995,  2034,   153,  1955,   541,   445,  1881,  2093,
    1017,   449,   158,  2097,  2045,   453,   153,   455,   150,   150,
     153,    13,    14,    15,    16,    17,   151,  1034,  1035,   176,
      75,   153,  1888,   153,  2065,  2119,  2065,  1673,   176,  1995,
     176,  1276,  1374,   686,   728,  1995,  1572,  1156,  2079,   727,
    2081,   418,   729,  1414,   730,   493,   731,  1167,  2081,  1628,
    1586,  2020,   201,  1991,   502,  1770,  1762,  1927,  2075,  2100,
    1358,  2074,  1927,  1599,  1890,  2106,  2062,  1620,  1620,  1956,
      72,  2033,  2097,  1956,   522,  2116,   524,   525,  1955,  2120,
     528,  1188,   530,    49,  1557,  1955,  1384,  1385,   258,  2130,
    1955,     4,     5,     6,     7,     8,     9,    10,    11,    12,
    1636,   103,  1845,  1184,   106,   107,   108,   109,   110,   111,
     112,   113,   114,  1917,    62,  1571,  1414,  1360,   930,  1181,
     568,   595,   894,  1461,   482,  1995,   807,  1927,  1642,   131,
    1995,   579,     0,   581,   939,   583,  1542,   585,  1219,    -1,
     752,    -1,    -1,    -1,  1225,   752,   752,    -1,    -1,   151,
     152,    99,    65,   601,   602,  1955,   604,    -1,    -1,    -1,
     162,   163,   110,  1461,   612,    -1,    -1,    -1,   616,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   625,    -1,    -1,
      -1,   330,    -1,    -1,   333,    -1,   239,   635,    -1,    -1,
     638,    -1,    -1,    -1,    -1,  1995,    -1,    -1,   347,    -1,
      -1,   649,   351,    -1,    -1,   153,    -1,  1743,    -1,    -1,
      -1,  1747,    -1,    -1,   662,    -1,    -1,   665,   666,    -1,
     668,    -1,    -1,    -1,    -1,  1761,    -1,    -1,    -1,   677,
      -1,    -1,   680,   681,   682,  1771,    -1,    -1,    18,    -1,
      13,    14,    15,    16,    17,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1788,    -1,    -1,    -1,   204,    -1,    13,    14,
      15,    16,    17,   103,    -1,    -1,   106,   107,   108,   109,
     110,   111,   112,   113,   114,  1292,  1293,    57,    58,    59,
      60,    61,    62,    63,    64,  1366,    -1,    -1,   736,  1306,
    1307,    -1,    -1,    -1,    -1,  1593,  1594,    -1,    -1,    72,
     449,    -1,    -1,    -1,   752,   753,    -1,    -1,    -1,    -1,
      -1,    -1,   760,    -1,    -1,    -1,  1852,    72,   266,    -1,
    1856,    -1,   846,  1340,  1341,  1861,    72,    -1,    -1,   777,
    1628,    -1,   780,  1414,   782,    -1,   176,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1642,    -1,    -1,    -1,   296,    -1,
    1886,   799,   800,    -1,   302,    -1,    -1,   103,   131,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   422,
     818,    -1,   820,    -1,     3,    -1,   131,    -1,   151,    -1,
      -1,   530,   155,   907,   332,   438,    -1,    -1,   441,   162,
     163,   839,    -1,    -1,  1930,    -1,   151,    -1,  1934,    -1,
     155,    -1,    -1,    -1,    -1,   151,   152,   162,   163,    -1,
      -1,  1947,   103,    -1,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,  1960,    -1,    -1,    -1,   376,   877,
      -1,    -1,    -1,    -1,   583,  1971,   884,  1973,  1974,    -1,
     131,    -1,   890,   891,    -1,   498,    -1,    -1,    -1,    -1,
      -1,  1749,   601,   602,   902,    -1,   904,    -1,    -1,    -1,
     151,   152,  1998,    -1,  1762,    -1,    -1,   915,    -1,    -1,
      -1,   162,   163,    -1,   103,    -1,   625,   106,   107,   108,
     109,   110,   111,   112,   113,   114,    -1,    -1,    -1,   638,
      -1,    -1,    -1,  1017,    -1,    -1,    -1,    -1,  2034,    -1,
       1,    -1,   131,    -1,    -1,    -1,   954,   455,    -1,  2045,
    1034,  1035,  1593,    -1,    -1,    -1,    -1,  1534,  1535,    -1,
      -1,    -1,   151,   152,   472,    -1,    -1,    -1,    -1,   158,
     103,    -1,    -1,   162,   163,   108,   109,   110,   111,   112,
     113,   114,    -1,  2079,    -1,  2081,    -1,    -1,    49,    -1,
      -1,    52,    -1,    54,    -1,    56,    -1,    -1,  1575,    -1,
      -1,    -1,    -1,    -1,  2100,    -1,    -1,    -1,    -1,    -1,
    2106,    -1,    73,  1021,    -1,    -1,    -1,    -1,    -1,   152,
    2116,   529,   155,    -1,  2120,    -1,    -1,   736,    -1,    -1,
    1038,    -1,    -1,    -1,  2130,    -1,    -1,  1045,    -1,   100,
     101,   549,   103,    -1,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,    -1,   119,   120,
     121,   569,   123,   124,    -1,    -1,    -1,    -1,   777,  1927,
     131,    -1,    -1,    -1,    -1,    -1,  1653,   103,    -1,  1087,
     106,   107,   108,   109,   110,   111,   112,   113,   114,    -1,
     151,  1732,    -1,   154,   155,    -1,   604,  1955,    -1,    -1,
     161,   162,   163,   164,   165,   166,   167,    -1,  1749,    -1,
      -1,  1688,    -1,    -1,    -1,    -1,  1693,  1694,    -1,    -1,
     628,    -1,  1130,    -1,    72,    -1,    -1,   635,     1,    -1,
    1138,     4,    -1,    -1,    -1,    -1,    -1,  1995,    -1,   752,
     753,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   762,
     176,    -1,   765,    -1,    -1,   103,    18,    -1,    -1,  1167,
     108,   109,   110,   111,   112,   113,   114,     4,     5,     6,
       7,     8,     9,    10,    11,    12,  1184,  1185,    -1,    -1,
      -1,    -1,    -1,   131,    -1,    -1,    59,    -1,    -1,    -1,
      -1,  1199,    -1,   902,    83,  1836,   905,  1205,    -1,    61,
      62,    63,    64,   151,   152,    -1,    -1,  1215,  1292,  1293,
      83,    -1,    -1,   826,   162,   163,    -1,   830,  1140,    -1,
      -1,   834,  1306,  1307,    -1,    -1,    99,    -1,    -1,    -1,
     103,    -1,    -1,    -1,    -1,    -1,  1244,    -1,    -1,    -1,
    1248,   103,    -1,    -1,    -1,   954,   108,   109,   110,   111,
     112,   113,   114,   115,    -1,    -1,  1340,  1341,    -1,    -1,
     149,    -1,    -1,    -1,    -1,    -1,    -1,   140,    -1,    -1,
      -1,    -1,    -1,   146,    -1,   164,   149,    -1,   103,    -1,
     153,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     163,   164,   165,   155,    -1,    65,    66,    67,    68,   188,
      -1,    -1,    -1,  1311,  1312,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   201,    -1,   103,   188,    -1,    -1,  1959,   108,
     109,   110,   111,   112,   113,   114,   115,   200,   201,   154,
     119,   204,   121,   103,   159,   843,   106,   107,   108,   109,
     110,   111,   112,   113,   114,    -1,    -1,  1355,  1356,  1357,
    1358,  1359,  1360,    -1,    -1,    -1,    -1,  1365,  1366,    -1,
      -1,   869,    -1,   152,   253,    -1,   155,    -1,    -1,    -1,
      -1,    -1,   880,    -1,    -1,    -1,  1384,  1385,   251,    -1,
     253,    -1,    -1,    -1,    -1,   155,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   266,    -1,    -1,   904,    -1,    -1,    -1,
      -1,    -1,    -1,   173,    -1,    -1,  1414,    -1,  1021,    -1,
     283,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   291,    -1,
      -1,   310,    -1,   296,  2065,    -1,    -1,    -1,  1436,   302,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   310,    -1,    -1,
      -1,   330,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1534,  1535,    -1,  1461,    -1,  1463,    -1,   330,    -1,   332,
     333,    -1,    -1,    -1,    -1,  2042,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   347,    -1,    -1,    -1,   351,  1092,
      -1,    -1,  1095,    72,    -1,    -1,    -1,    -1,   103,    -1,
    1412,   106,   107,   108,   109,   110,   111,   112,   113,   114,
      13,    14,    15,    16,    17,    -1,  1215,    -1,    -1,    -1,
      -1,    -1,   385,    -1,   103,    -1,   131,   106,   107,   108,
     109,   110,   111,   112,   113,   114,    -1,    -1,    -1,    -1,
     419,    -1,    -1,    -1,    -1,  1244,   151,   152,    -1,    -1,
      -1,    -1,   131,   158,    -1,  1553,   419,   162,   163,  1557,
      -1,  1559,    -1,    -1,    -1,  1563,    -1,  1565,    -1,    72,
     449,    -1,   151,   152,    -1,    -1,   155,    -1,    -1,  1653,
      -1,    -1,   445,   162,   163,    -1,   449,    -1,    -1,    -1,
      -1,    -1,   455,    -1,    -1,  1593,  1594,    -1,    72,    -1,
     103,    -1,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,    -1,    -1,  1688,    -1,    -1,    -1,    -1,  1693,
    1694,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   131,   103,
    1628,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,  1639,    -1,    -1,  1642,   524,   525,    -1,   151,   152,
      -1,   530,   155,    -1,    -1,    -1,    -1,   131,    -1,   162,
     163,   524,   525,    -1,    -1,    -1,   529,   530,    -1,    -1,
      -1,    -1,    -1,    -1,  1586,  1673,    -1,   151,   152,    -1,
      -1,    -1,    -1,    -1,    -1,  1288,  1184,  1599,   162,   163,
      -1,    -1,    -1,    -1,  1297,    -1,    -1,    -1,   561,    -1,
    1698,  1699,    -1,   566,   583,    -1,   569,   570,    -1,   572,
      -1,    -1,    -1,    -1,    -1,    -1,  1714,  1715,    -1,    -1,
     583,  1219,   585,   602,  1636,    13,    14,    15,    16,    17,
      -1,  1729,    -1,    -1,  1732,    -1,   599,  1436,   601,   602,
      -1,   604,    -1,    -1,    -1,    -1,   625,    -1,    -1,    -1,
     103,  1749,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   625,    -1,  1762,   628,    -1,    -1,    -1,   632,
     649,    -1,   635,    -1,    -1,   638,    -1,   640,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    -1,   649,    -1,    -1,    -1,
      -1,  1789,    13,    14,    15,    16,    17,    -1,    -1,   662,
      -1,    -1,   665,   666,    -1,   668,   159,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   677,   103,    -1,   680,   681,   682,
     108,   109,   110,   111,   112,   113,   114,    -1,    -1,    -1,
      -1,  1743,    -1,    -1,    -1,  1747,    -1,    -1,  1836,    -1,
      -1,    -1,    -1,   131,    -1,    -1,    -1,  1845,    -1,  1761,
    1348,    72,    -1,    -1,    -1,    -1,    -1,   736,    -1,  1771,
     180,    -1,    -1,   151,   152,    -1,    -1,    -1,  1366,    -1,
      -1,    -1,    -1,   736,   162,   163,    -1,    -1,    -1,    -1,
      -1,   760,   103,  1881,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,    -1,    -1,   103,   760,   777,   106,
     107,   108,   109,   110,   111,   112,   113,   114,    -1,    -1,
     131,  1909,    -1,    -1,   777,    -1,  1414,   103,    -1,  1917,
     799,   800,   108,   109,   110,   111,   112,   113,   114,  1927,
     151,   152,    -1,    -1,   155,    -1,   799,   800,    -1,   818,
    1852,   162,   163,    -1,  1856,   131,    -1,    -1,    -1,  1861,
    1553,    -1,   159,    -1,    -1,   818,    -1,  1955,  1956,    -1,
    1958,  1959,    -1,    -1,    -1,   151,   152,    -1,  2042,   155,
      -1,    -1,    -1,    -1,  1886,    -1,   162,   163,    -1,    -1,
     843,    -1,    -1,   846,    -1,    -1,    -1,     1,    -1,    -1,
       4,    -1,    -1,    -1,    -1,    -1,    -1,  1995,    -1,     1,
      -1,    -1,    -1,    -1,    -1,   884,   869,    -1,    -1,    -1,
      -1,    -1,   891,    -1,   877,    -1,    -1,   880,  1930,    -1,
      -1,   884,  1934,    -1,    -1,  2023,  2024,   890,   891,    -1,
      -1,    -1,    -1,    -1,    -1,  1947,    -1,    -1,    -1,   902,
      -1,   904,   905,    -1,   907,    59,    -1,    -1,    -1,   369,
      -1,    -1,    -1,   373,   374,    -1,    -1,    59,    -1,  1971,
      -1,  1973,  1974,   383,   384,    -1,    -1,  2065,    -1,    83,
      -1,    -1,    -1,    -1,    -1,   954,    -1,    -1,   398,   399,
      -1,    -1,    -1,    -1,    -1,    -1,  1998,    -1,    -1,   103,
      -1,   954,    -1,    -1,    -1,  1593,    -1,    -1,    -1,   419,
      72,   103,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     103,  1714,  1715,   106,   107,   108,   109,   110,   111,   112,
     113,   114,  2034,    -1,    -1,    -1,   140,    -1,    -1,   449,
      -1,   103,   146,  2045,   106,   107,   108,   109,   110,   111,
     112,   113,   114,    -1,   146,    -1,    -1,    -1,    -1,    -1,
     164,    -1,    -1,    -1,  1017,    -1,    -1,    -1,    -1,   131,
      -1,    -1,   164,    -1,    -1,    -1,   159,  2079,    -1,  2081,
      -1,  1034,  1035,    -1,    -1,   189,    -1,    -1,    -1,   151,
     152,    -1,  1881,    -1,    -1,    -1,   200,   201,  2100,    -1,
     162,   163,    -1,    -1,  2106,    -1,    -1,    -1,    -1,   201,
      -1,    -1,    -1,    -1,  2116,    -1,    -1,    -1,  1087,    -1,
      -1,   103,    -1,    -1,    -1,   229,   108,   109,   110,   111,
     112,   113,   114,   115,  1087,    -1,    -1,   119,    -1,   121,
      -1,   245,    -1,  1836,    -1,    -1,   250,   251,   103,   253,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
      -1,  1749,    -1,    -1,    -1,    -1,    -1,  1956,    -1,    -1,
     152,   275,    -1,   155,   278,    -1,   280,  1130,    -1,    -1,
      -1,    -1,    -1,   275,    -1,  1138,   103,   291,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,    -1,   291,
     304,   103,    -1,    -1,    -1,    -1,   108,   109,   110,   111,
     112,   113,   114,   115,  1167,    -1,    -1,   119,    -1,   121,
    1913,    -1,    -1,    -1,  1917,    -1,   330,    -1,    -1,   333,
      -1,  1184,  1185,    -1,    -1,    -1,  1205,   154,   330,    -1,
      -1,   333,    -1,   347,    -1,    -1,  1215,   351,    -1,    -1,
     152,    -1,  1205,   155,    -1,   347,    -1,    -1,    -1,   351,
      -1,    -1,  1215,    -1,    -1,  1958,  1219,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1244,   103,    -1,  1231,   106,
     107,   108,   109,   110,   111,   112,   113,   114,    -1,    -1,
    1243,  1244,    -1,    -1,    -1,  1248,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   713,   714,   715,   716,   717,   718,   719,
     720,   721,   722,   723,   724,   725,   726,   727,   728,   729,
     730,   731,    -1,    -1,    -1,   152,    -1,    -1,   155,    -1,
    2023,  2024,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1292,
    1293,   445,  1311,  1312,    -1,   449,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1306,  1307,    -1,    -1,   449,  1311,  1312,
     134,   135,   136,   137,   138,   139,   140,   141,   142,   143,
     144,  1959,  2065,    -1,   148,    -1,    -1,    -1,    -1,    -1,
      72,    -1,    -1,   793,    -1,    -1,    -1,  1340,  1341,    -1,
      -1,    -1,    -1,    -1,    -1,  1348,    -1,    -1,    -1,    -1,
      -1,   175,  1355,  1356,  1357,  1358,  1359,  1360,    -1,    -1,
      -1,   103,    -1,  1366,   106,   107,   108,   109,   110,   111,
     112,   113,   114,    -1,    -1,    -1,   530,    -1,    -1,    -1,
      -1,  1384,  1385,    -1,    -1,    -1,    -1,    -1,   530,   131,
      -1,    -1,   101,    -1,   103,    -1,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,    -1,   561,    -1,   151,
     152,  1414,   566,    -1,    -1,    -1,   570,  1436,   572,   561,
     162,   163,    -1,    -1,    -1,    -1,    -1,    -1,   570,   583,
     572,   585,    -1,  1436,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   583,   151,   585,    -1,   154,   155,   601,   602,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1461,   601,
     602,    -1,   616,    -1,    -1,    -1,    -1,     1,    -1,    -1,
       4,   625,    -1,    -1,    -1,    -1,   630,    -1,    -1,    -1,
      -1,    -1,    -1,   625,   638,    -1,    -1,    -1,    -1,    -1,
     950,    -1,    -1,    -1,   103,   955,   638,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   966,   103,    -1,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,    -1,
      -1,    -1,   131,    -1,    -1,    59,    -1,    -1,    -1,    -1,
      -1,  1534,  1535,    -1,    -1,    -1,    -1,    -1,    -1,    74,
    1559,    -1,   151,   152,    -1,    -1,   155,    -1,  1008,    83,
      -1,    -1,    -1,   162,   163,    -1,  1559,    -1,    -1,   155,
      -1,    96,    -1,    -1,    -1,    -1,   175,    -1,    -1,   103,
     103,    -1,  1575,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   736,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1593,  1594,    -1,    -1,   736,    -1,    -1,    -1,   131,   753,
      -1,    -1,    -1,    -1,    -1,    -1,   140,    -1,    -1,    -1,
      -1,    -1,   146,    -1,    -1,   149,    -1,   152,   151,   152,
    1639,    -1,   155,   777,    -1,  1628,    -1,    -1,   782,   162,
     163,    -1,    -1,    -1,    -1,   777,  1639,    -1,    -1,  1642,
      -1,    -1,    -1,    -1,    -1,   799,   800,    -1,    -1,    -1,
    1653,    -1,    -1,    -1,   188,    -1,    -1,   799,   800,    -1,
      -1,    -1,    -1,    -1,   818,    -1,   200,    -1,    -1,    -1,
    1673,    -1,    -1,    -1,    -1,    -1,   818,    -1,  1138,  1698,
    1699,    -1,    -1,    -1,    -1,  1688,    -1,    -1,    -1,    -1,
    1693,  1694,   846,   228,   229,  1698,  1699,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   846,    -1,    -1,    -1,     1,    -1,
     245,    -1,    -1,    -1,    -1,    -1,    -1,   251,    -1,   253,
      -1,    -1,    -1,   877,   258,    -1,    -1,    -1,    -1,    -1,
     884,    -1,    -1,    -1,    -1,   877,   890,    -1,    -1,    -1,
      -1,    -1,   884,    -1,    -1,    -1,  1749,    -1,   902,    -1,
      -1,   905,    -1,   907,    -1,  1215,    -1,   291,   912,  1762,
     902,    -1,    -1,   905,    -1,   907,    59,    -1,   303,   304,
      -1,    -1,    -1,    -1,  1234,   103,   310,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,    -1,    -1,    -1,
      -1,  1251,  1252,  1253,    -1,    -1,    -1,    -1,  1258,  1259,
     954,    -1,    -1,   131,    -1,    -1,    -1,    -1,    -1,   103,
     103,    -1,   954,    -1,   108,   109,   110,   111,   112,   113,
     114,   115,  1282,   151,   152,   119,    -1,   121,    -1,    -1,
      -1,    -1,    -1,   103,   162,   163,   106,   107,   108,   109,
     110,   111,   112,   113,   114,    -1,    -1,    -1,    -1,    -1,
      -1,   385,    -1,   146,    -1,    -1,    -1,    -1,   152,  1319,
    1320,    -1,    -1,  1017,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   164,    -1,    -1,    -1,  1017,    -1,    -1,  1881,    -1,
    1034,  1035,    -1,    -1,   154,   419,    -1,    -1,    -1,    -1,
    1909,   103,  1034,  1035,   106,   107,   108,   109,   110,   111,
     112,   113,   114,    -1,    -1,    -1,  1909,    -1,   201,    -1,
      -1,   445,    -1,    -1,    -1,    -1,    -1,    -1,   453,    -1,
      -1,    -1,    -1,    -1,  1927,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1087,    -1,    -1,    -1,    -1,    -1,   151,
      -1,    -1,    -1,    -1,    -1,  1087,    -1,    -1,    -1,    -1,
      -1,    -1,  1955,  1956,    -1,    -1,  1959,    -1,   493,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   502,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1130,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1138,    -1,    -1,   522,    -1,    -1,
     524,   525,  1995,   528,    -1,    -1,  1138,   103,   291,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,    -1,
      -1,    -1,    -1,  1167,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1167,    -1,    -1,    -1,    -1,
      -1,  1185,   566,   568,    -1,    -1,   570,   330,   572,  2042,
     333,    -1,    -1,  1185,   579,   151,   581,    -1,    -1,    -1,
      -1,   585,    -1,    -1,   347,    -1,    -1,    -1,   351,    -1,
      -1,  1215,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1224,    -1,    -1,  1215,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1243,
    1244,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1243,  1244,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   649,    13,    14,    15,    16,
      17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   662,    -1,
      -1,   665,   666,    -1,   668,    -1,    -1,    -1,  1292,  1293,
      -1,    -1,    -1,   677,    -1,    -1,   680,   681,   682,    -1,
    1292,  1293,  1306,  1307,    -1,    -1,   449,  1311,  1312,    -1,
      -1,    -1,    -1,    -1,  1306,  1307,    -1,    -1,    -1,  1311,
    1312,    -1,    -1,  1633,    -1,    72,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1340,  1341,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1340,  1341,
      -1,  1355,  1356,  1357,  1358,    -1,   103,    -1,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   760,    -1,    -1,    -1,
    1384,  1385,    -1,    -1,   131,    -1,    -1,   530,    -1,    -1,
      -1,    -1,  1384,  1385,    -1,   780,    -1,   782,    -1,    -1,
      -1,    -1,    -1,    -1,   151,   152,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   162,   163,    -1,   561,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   570,    -1,   572,
      -1,    -1,  1436,    -1,    -1,   820,    -1,    -1,    -1,    -1,
     583,    -1,   585,    -1,  1436,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   839,    -1,    -1,  1461,   601,   602,
      -1,    -1,    -1,    -1,    -1,     1,    -1,    -1,     4,    -1,
      -1,    -1,    -1,    -1,    -1,     1,    -1,    -1,     4,    -1,
      -1,    -1,   625,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   877,    -1,   638,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   890,   891,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    59,    -1,    -1,    -1,    -1,    -1,    -1,
    1534,  1535,    -1,    59,    -1,    -1,    -1,    -1,  1848,    -1,
      -1,    -1,  1534,  1535,    -1,    -1,    -1,    83,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    83,    -1,    -1,
      -1,  1565,    -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,
      -1,  1575,    -1,    -1,    -1,    -1,    -1,   103,  1888,    -1,
      -1,    -1,    -1,  1575,    -1,    -1,    -1,    -1,    -1,    -1,
    1594,    -1,    -1,   736,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1594,    -1,   140,    -1,    -1,    -1,    -1,    -1,
     146,    -1,    -1,   149,   140,    -1,    -1,    -1,    -1,    -1,
     146,    -1,    -1,   149,  1628,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   777,  1639,  1628,    -1,  1642,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1639,    -1,  1653,
      -1,    -1,   188,  1038,    -1,    -1,   799,   800,    -1,  1044,
    1045,  1653,   188,    -1,   200,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   200,   818,    -1,    -1,    -1,  1989,
      -1,  1991,    -1,    -1,  1688,    -1,    -1,    -1,    -1,  1693,
    1694,    -1,    -1,    -1,  1698,  1699,  1688,    -1,    -1,    -1,
      -1,  1693,  1694,   846,    -1,    -1,  1698,  1699,    -1,    -1,
      -1,  1715,    -1,    -1,    -1,   251,    -1,   253,    -1,    -1,
    2030,    -1,   258,    -1,    -1,   251,    -1,   253,    -1,    -1,
      -1,    -1,   258,    -1,   877,    -1,    -1,    -1,    -1,    -1,
      -1,   884,    -1,    -1,    -1,    -1,  1130,    13,    14,    15,
      16,    17,    -1,  2063,  1138,   291,    -1,    -1,  1762,   902,
      -1,    -1,   905,    -1,   907,   291,    -1,    -1,    -1,    -1,
    1762,    -1,    -1,    -1,   310,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1167,   310,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1185,  2112,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,   954,    -1,    -1,  1199,    -1,    -1,    -1,    -1,    -1,
      -1,  1205,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   385,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   385,
      -1,    -1,    -1,    -1,  1248,   131,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1017,    -1,    -1,  1881,    -1,    -1,
      -1,    -1,    -1,   419,    -1,   151,   152,    -1,    -1,  1881,
      -1,  1034,  1035,   419,    -1,    -1,   162,   163,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1909,    -1,    -1,    -1,   445,
      -1,    -1,    -1,    -1,    -1,    -1,  1301,  1909,    -1,   445,
      -1,    -1,    -1,  1927,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1927,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1087,    -1,    -1,    -1,    -1,    -1,
      -1,  1955,  1956,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1955,  1956,    -1,    -1,    -1,    -1,    -1,
      -1,  1355,  1356,  1357,  1358,  1359,  1360,    -1,    -1,    -1,
    1365,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   524,   525,
      -1,  1995,    -1,    -1,    -1,  1138,    -1,    -1,   524,   525,
    1384,  1385,    -1,  1995,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    2024,    -1,    -1,    -1,  1167,    -1,    -1,    -1,    -1,    -1,
     566,    -1,    -1,    -1,   570,    -1,   572,    -1,  2042,    -1,
     566,    -1,  1185,    -1,   570,    -1,   572,    -1,    -1,   585,
    2042,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   585,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1215,    -1,    -1,    -1,    -1,  1461,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1243,  1244,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   649,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   649,    -1,    -1,   662,    -1,    -1,   665,
     666,    -1,   668,    -1,    -1,    -1,   662,    -1,    -1,   665,
     666,   677,   668,    -1,   680,   681,   682,    -1,    -1,  1292,
    1293,   677,    -1,    -1,   680,   681,   682,    -1,    -1,    -1,
      -1,    -1,    -1,  1306,  1307,    -1,    -1,    87,  1311,  1312,
      -1,    -1,  1557,    93,    94,  1559,    -1,    -1,  1563,    -1,
    1565,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1340,  1341,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   127,    -1,    -1,
    1594,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   760,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   760,    -1,    -1,    -1,    -1,    -1,
      -1,  1384,  1385,    -1,  1628,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    13,    14,    15,    16,    17,    18,  1642,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,  1673,
      51,    -1,    53,  1436,    -1,    -1,    57,    58,    59,    60,
      61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     5,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,    14,    15,
      16,    17,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   877,    -1,    -1,  1729,   106,   107,  1732,    -1,    -1,
      -1,   877,    -1,    -1,   890,   891,    -1,    -1,    -1,    56,
      57,    -1,    -1,    49,   890,   891,    52,    -1,    54,    -1,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1762,    -1,
     300,    -1,    -1,    -1,    -1,    -1,    72,    73,    -1,    -1,
      -1,  1534,  1535,   154,    91,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1789,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   100,   101,    -1,   103,    -1,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,  1575,   119,   120,   121,    -1,   123,   124,    -1,
      -1,    -1,    -1,    -1,   141,   131,    -1,   144,    -1,    -1,
      -1,  1594,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1845,   158,    -1,    -1,    -1,   151,    -1,    -1,   154,   155,
      -1,    -1,    -1,    -1,    -1,   161,   162,   163,   164,   165,
     166,   167,    -1,    -1,    -1,  1628,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1639,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1653,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   219,    -1,    -1,    -1,    -1,    -1,   448,    -1,
     450,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   459,
     460,    -1,    -1,  1927,    -1,  1688,    -1,    -1,    -1,    -1,
    1693,  1694,    -1,    -1,    -1,  1698,  1699,    -1,    -1,   385,
      -1,    -1,    -1,    -1,    -1,    -1,   263,    -1,    -1,    -1,
      -1,  1955,    -1,  1958,    -1,    -1,   273,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   284,    -1,    -1,
      -1,    -1,    -1,    -1,  1130,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1138,    -1,  1130,    -1,    -1,    -1,    -1,    -1,
      -1,  1995,  1138,    -1,   311,    -1,    -1,    -1,    -1,  1762,
      -1,   318,   319,    -1,    -1,    -1,   323,    -1,    -1,    -1,
      -1,  1167,    -1,    -1,    -1,    -1,    -1,    -1,  2023,  2024,
      -1,  1167,    -1,    -1,    -1,    -1,    -1,   567,    -1,  1185,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1185,
      -1,    -1,    -1,    -1,   361,    -1,    -1,   364,    -1,  1205,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1205,
    2065,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   524,   525,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1248,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1248,    -1,    -1,    -1,    -1,     0,    -1,    -1,
       3,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1881,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   452,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1909,   464,   465,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1927,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    77,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1955,  1956,    -1,    -1,    -1,    -1,    -1,  1355,
    1356,  1357,  1358,  1359,  1360,    -1,    -1,    -1,    -1,  1355,
    1356,  1357,  1358,  1359,  1360,    -1,   662,    -1,    -1,    -1,
      -1,    -1,   668,    -1,    -1,    -1,    -1,    -1,  1384,  1385,
      -1,   677,  1995,    -1,    -1,    -1,    -1,    -1,  1384,  1385,
      -1,    -1,    -1,   136,    -1,    -1,    -1,    -1,    -1,    -1,
     696,    -1,    -1,   793,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   595,  2042,
      -1,    -1,    -1,    -1,    -1,    -1,   732,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1461,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1461,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   645,    -1,
     870,   871,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   881,   882,   883,    -1,   238,   886,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   252,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   262,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   272,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   286,   287,    -1,    -1,    -1,    -1,    -1,
     293,   294,    -1,  1559,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1559,    -1,    -1,   309,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   968,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   329,    -1,  1594,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1594,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1628,    -1,  1014,    -1,    -1,   794,    -1,    -1,
      -1,    -1,  1628,    -1,   801,    -1,  1642,    -1,    -1,    -1,
      -1,    -1,    -1,   386,    -1,    -1,  1642,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1673,    -1,    -1,
      -1,  1061,    -1,    -1,   417,    -1,    -1,  1673,    -1,    -1,
    1070,  1071,  1072,  1073,    -1,    -1,    -1,    -1,  1078,  1079,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1088,    -1,
     443,    -1,    -1,    -1,   447,    -1,    -1,    48,    -1,   876,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1109,
      -1,    -1,  1112,   466,  1114,    -1,    -1,   470,   471,    -1,
      -1,   474,    -1,    74,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   489,   490,   491,   492,
      -1,    -1,    -1,    -1,    -1,    -1,  1762,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   508,  1762,    -1,    -1,    -1,
      -1,    -1,    -1,   516,    -1,    -1,    -1,  1167,    -1,   120,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   133,    -1,   135,    -1,    -1,    -1,    -1,    -1,
      -1,   544,    -1,  1193,    -1,    -1,    -1,    -1,    -1,    -1,
    1200,    -1,  1202,  1203,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1214,   166,  1216,    -1,  1218,    -1,
    1220,    -1,   575,    -1,    -1,  1225,    -1,    -1,    -1,   582,
      -1,    -1,    -1,    -1,    -1,   588,    -1,    -1,    -1,   190,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   612,
     613,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   228,    -1,    -1,
      -1,   232,    -1,    -1,   235,   236,    -1,  1287,   239,    -1,
      -1,   242,   243,    -1,  1294,  1295,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1927,    -1,    -1,    -1,    -1,    -1,    -1,  1318,    -1,
      -1,  1927,    -1,    -1,    -1,  1325,    -1,    -1,    -1,  1329,
     683,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1955,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1955,
      -1,    -1,   303,    -1,    -1,   306,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1364,    -1,  1143,  1144,  1145,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   327,   328,    -1,  1995,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1995,
      -1,    -1,   343,    -1,    -1,    -1,  1173,    -1,    -1,   752,
      -1,    -1,    -1,    -1,  1404,    -1,    -1,    -1,    -1,    -1,
      -1,  1188,    -1,    -1,   767,    -1,    -1,    -1,   771,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   780,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1440,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1448,   802,
    1450,    -1,    -1,    -1,    -1,  1232,    -1,    -1,   811,    -1,
       5,    -1,    -1,    -1,   817,    -1,    -1,    -1,    13,    14,
      15,    16,    17,    -1,    -1,  1381,    -1,    -1,  1384,  1385,
      -1,    -1,    -1,    -1,  1390,    -1,    -1,   438,  1394,    -1,
    1396,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1499,
    1500,   854,    -1,    -1,    49,    -1,    -1,    52,   861,    54,
      -1,    56,    -1,    -1,  1514,  1515,    -1,  1517,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1526,    72,    73,    -1,
      -1,    -1,    -1,    -1,    -1,   888,  1536,  1537,    -1,    -1,
      -1,    -1,   493,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   505,   100,   101,    -1,   103,    -1,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,    -1,   119,   120,   121,    -1,   123,   124,
      -1,    -1,    -1,    -1,    -1,    -1,   131,    -1,    -1,    -1,
      -1,   944,  1369,    -1,    -1,  1372,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   151,    -1,    -1,   154,
     155,    -1,    -1,    -1,    -1,    -1,   161,   162,   163,   164,
     165,   166,   167,    -1,    -1,    -1,    -1,    -1,   579,    -1,
      -1,    -1,    -1,    -1,  1540,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1654,  1655,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1667,    -1,   620,
     621,  1577,    49,    -1,  1027,    52,    -1,    54,  1031,    56,
      -1,    -1,   633,  1589,    -1,  1038,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1048,    73,    -1,  1604,  1605,
      -1,    -1,  1055,    -1,  1704,  1705,    -1,    -1,    -1,    -1,
      -1,  1064,    -1,  1066,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1628,   100,   101,    -1,   103,    -1,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,    -1,   119,   120,   121,  1098,   123,   124,    -1,  1102,
      -1,    -1,    -1,    -1,   131,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1119,    -1,    -1,  1122,
      -1,    -1,    -1,    -1,   151,   152,    -1,   154,   155,    -1,
      -1,    -1,   159,    -1,   161,   162,   163,   164,   165,   166,
     167,    -1,    -1,  1793,    -1,  1572,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   756,   757,    -1,    -1,    -1,
      -1,   762,    -1,    -1,  1814,    -1,    -1,  1817,  1818,    -1,
      -1,    -1,    -1,    -1,  1824,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   783,    -1,    -1,   786,   787,    -1,   789,  1745,
     791,   792,    -1,    -1,    -1,    -1,    -1,    -1,  1754,    -1,
    1756,    -1,    -1,  1759,  1760,    -1,  1762,    -1,    -1,    -1,
    1213,  1767,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   830,
      -1,    -1,    -1,   834,    -1,  1238,    -1,    -1,    -1,    -1,
      -1,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    -1,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,   164,    50,    51,
      -1,    53,    -1,    55,    -1,    -1,    -1,    -1,   899,   900,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1957,    -1,    -1,
      72,  1867,   913,    -1,    -1,    -1,  1872,  1873,    -1,    -1,
      -1,    -1,    -1,   200,   201,    -1,    -1,    -1,    -1,  1332,
      -1,    -1,    -1,  1336,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   106,   107,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   232,    -1,    -1,    -1,    -1,
      -1,  1788,   239,    -1,    -1,    -1,    -1,    -1,  1371,   131,
      -1,  2021,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1935,
      -1,  1937,    -1,    -1,  1940,  1941,    -1,    -1,    -1,  1945,
    1946,    -1,   154,   155,    -1,    -1,  2046,    -1,    -1,    -1,
     162,   163,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2061,    -1,    -1,    -1,    -1,    -1,    -1,  1421,    -1,
      -1,  1424,    -1,    -1,    -1,    -1,    -1,  2077,    -1,   306,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1442,
      -1,    -1,    -1,  1044,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   330,   331,    -1,    -1,    -1,    -1,    -1,
    2016,  2017,  2018,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   351,    -1,    -1,    -1,    -1,    -1,
      -1,  2037,    -1,    -1,  1085,    -1,    -1,    -1,    -1,    -1,
      -1,  1092,  1495,    -1,  1095,  2051,  2052,  2053,    -1,    -1,
      -1,  1504,    -1,    -1,    -1,  1508,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1522,
    1523,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   422,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   438,   439,    -1,   441,   442,    -1,    -1,    -1,    -1,
      -1,    -1,   449,    -1,    -1,    -1,   453,    -1,    13,    14,
      15,    16,    17,    -1,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,   494,    53,    -1,
    1221,   498,    -1,    -1,    -1,    -1,    -1,    -1,  1229,  1230,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,
    1643,  1644,    -1,    -1,    -1,    -1,    83,    -1,    -1,    -1,
      -1,    -1,    -1,   530,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    99,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1288,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1297,    -1,    -1,  1300,
      -1,  1302,  1303,   580,    -1,    -1,   583,    -1,    -1,    -1,
      -1,    -1,   149,    -1,    -1,    -1,   153,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   601,   602,    -1,   164,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   612,    -1,    -1,    -1,   616,
      -1,    -1,  1343,    -1,    -1,    -1,   623,    -1,   625,    -1,
      -1,   188,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   201,    -1,    -1,   204,    -1,    -1,
      -1,    -1,    -1,  1776,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1803,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1415,    -1,   253,    -1,    -1,  1822,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   266,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1850,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   736,
      -1,    -1,    -1,    -1,    -1,   302,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   310,  1877,   752,   753,  1880,    -1,    -1,
      -1,    -1,    -1,    -1,    48,   762,   763,    -1,   765,   766,
      -1,    -1,    -1,   330,    -1,   332,    -1,    -1,    -1,    -1,
     777,    -1,  1503,   780,    -1,   782,   783,    -1,    -1,    -1,
      -1,    -1,   789,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   799,   800,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1532,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   818,    -1,    -1,    -1,   822,    -1,    -1,   385,   826,
      -1,    -1,    -1,   830,   831,    -1,   120,   834,   835,    -1,
      -1,    -1,  1563,    -1,    -1,   842,    -1,    -1,  1569,   133,
      -1,   135,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1984,   419,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   884,   885,    -1,
      -1,    -1,   449,    -1,    -1,    -1,    -1,   188,   455,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     201,    -1,    -1,    -1,    -1,    -1,    -1,  1638,   915,    -1,
      -1,    -1,    -1,   214,    -1,   216,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   235,   236,    -1,    -1,   239,    -1,   954,   242,   243,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   524,   525,    -1,
      -1,    -1,    -1,   530,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1721,  1722,    -1,    -1,    -1,    -1,    -1,    -1,  1729,    -1,
     301,    -1,  1733,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1021,    -1,   583,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1038,  1039,   327,   328,   602,    -1,   604,  1045,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   343,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   625,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1087,    -1,   649,    -1,    -1,  1092,  1093,    -1,  1095,  1096,
      -1,    -1,    -1,    -1,    -1,   662,    -1,    -1,   665,   666,
      -1,   668,    -1,    -1,    -1,    -1,    -1,  1838,    -1,    -1,
     677,    -1,    -1,   680,   681,   682,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   438,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   736,
      -1,    -1,    -1,    -1,   475,    -1,    -1,    -1,    -1,    -1,
     481,    -1,  1913,    -1,    -1,   486,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   760,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1215,    -1,
     777,   505,    -1,    -1,  1221,  1222,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   799,   800,    -1,    -1,    -1,  1244,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   818,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   573,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1288,  1289,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1297,  1298,  2023,  1300,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   602,    -1,    -1,  1311,  1312,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   615,    -1,    -1,   884,    -1,    -1,
      -1,    -1,    -1,    -1,   891,    -1,   620,   621,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   904,    -1,   633,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   670,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   954,    -1,    -1,
      -1,    -1,    -1,   694,   695,    -1,    -1,   698,    -1,   700,
      -1,    -1,    -1,    -1,    -1,   706,    -1,   708,   709,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1436,
      -1,    -1,    -1,    -1,    -1,   736,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   749,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1463,    -1,    -1,   760,
      -1,    -1,   756,   757,    -1,    -1,    -1,    -1,   762,    -1,
      -1,    -1,    -1,   774,    -1,    -1,   777,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   783,
      -1,    -1,   786,   787,    -1,   789,    -1,   791,   792,    -1,
      -1,    -1,    -1,   804,    -1,    -1,   807,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1087,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   830,    -1,    -1,    -1,
     834,    -1,   843,    -1,    -1,    -1,  1553,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1569,  1130,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     891,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   904,   905,   899,   900,    -1,    -1,    -1,
      -1,   912,    -1,    -1,    -1,    -1,    -1,  1184,    -1,   913,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1639,    -1,    -1,    -1,    -1,    -1,  1205,   940,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1215,    -1,
      -1,    -1,    -1,   954,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   962,    -1,    -1,    -1,    -1,    -1,    -1,   969,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1244,    -1,    -1,
      -1,  1248,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1698,  1699,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1714,  1715,    -1,
      -1,    -1,    -1,    -1,  1015,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1730,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1311,  1312,    -1,    -1,    -1,    -1,
    1044,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1082,    -1,  1084,    -1,  1086,    -1,    -1,  1355,  1356,
    1357,  1085,  1359,  1360,    -1,    -1,    -1,    -1,  1092,  1366,
      -1,  1095,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1836,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1844,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1414,    -1,    -1,
      -1,    -1,    -1,     3,  1155,  1156,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    13,    14,    15,    16,    17,    -1,  1436,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    -1,
      50,    51,  1909,    53,    -1,    55,  1913,  1914,    -1,    -1,
    1917,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1219,    -1,
      -1,    -1,    72,    -1,  1225,    -1,    -1,  1221,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1229,  1230,    -1,    -1,    -1,
      -1,    -1,    -1,  1244,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1958,    -1,    -1,    -1,    -1,   106,   107,    -1,    -1,
    1261,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1276,    -1,    -1,  1279,    -1,
      -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1559,    -1,  1288,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1297,   154,   155,  1300,    -1,  1302,  1303,
      -1,    -1,   162,   163,    -1,    -1,  2023,  2024,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1593,    -1,    -1,    -1,
    1331,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1347,  1348,    -1,  1343,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2065,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1639,  1374,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1392,    -1,    -1,  1395,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1673,    -1,    -1,    -1,
      -1,     1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1415,    -1,    -1,    -1,    -1,    -1,    -1,    18,    -1,
      -1,  1698,  1699,    -1,    -1,  1436,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1446,  1447,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1456,    -1,    -1,    -1,    49,
      -1,    -1,    52,    -1,    54,    -1,    56,    -1,    -1,    -1,
      -1,    -1,    -1,  1474,    -1,  1476,    -1,    -1,    -1,    -1,
      -1,    71,  1749,    73,    74,    -1,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    92,    93,    94,    95,    96,    -1,    98,  1503,
     100,   101,    -1,   103,    -1,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,    -1,   123,   124,    -1,    -1,    -1,  1532,    -1,
      -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1559,    -1,
     150,   151,    -1,  1564,   154,   155,    -1,    -1,    -1,   159,
      -1,   161,   162,   163,   164,   165,   166,   167,    13,    14,
      15,    16,    17,    -1,    -1,    20,   176,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,  1881,    50,    51,    -1,    53,    -1,
      55,    -1,    -1,  1624,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,
      -1,    -1,  1909,    -1,  1638,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     1,    -1,    -1,    -1,    -1,
    1671,   106,   107,  1674,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    18,    -1,    -1,    -1,    -1,    -1,    -1,  1956,
      -1,    -1,  1959,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1709,    -1,
      -1,    -1,    -1,    49,    -1,    -1,    52,    -1,    54,   154,
      56,    -1,    -1,    -1,    -1,    -1,    -1,  1721,  1722,    -1,
      -1,    -1,    -1,    -1,    -1,    71,    -1,    73,    74,  1733,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    91,    92,    93,    94,    95,
      96,    -1,    98,    -1,   100,   101,    -1,   103,    -1,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,    -1,   123,   124,    -1,
       1,    -1,    -1,    -1,    -1,   131,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    18,    -1,    -1,
      -1,    -1,    -1,    -1,   150,   151,    -1,    -1,   154,   155,
      -1,    -1,    -1,   159,    -1,   161,   162,   163,   164,   165,
     166,   167,    -1,    -1,    -1,    -1,    -1,    -1,    49,    -1,
     176,    52,    -1,    54,  1838,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      71,    -1,    73,    74,    -1,    76,    -1,    -1,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    90,
      91,    92,    93,    94,    95,    96,    -1,    98,  1889,   100,
     101,    -1,   103,    -1,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,    -1,   123,   124,    -1,    -1,    -1,    -1,    -1,  1913,
     131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     151,    -1,    -1,   154,   155,    -1,    -1,    -1,   159,    -1,
     161,   162,   163,   164,   165,   166,   167,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   176,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    49,    -1,    51,    52,    53,    54,    -1,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    -1,
      -1,    -1,    69,    -1,    71,    72,    73,    74,    -1,    76,
      -1,    -1,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    91,    92,    93,    94,    95,    96,
      -1,    98,    -1,   100,   101,   102,   103,    -1,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,    -1,   123,   124,    -1,    -1,
      -1,    -1,    -1,    -1,   131,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   150,   151,    -1,    -1,   154,   155,    -1,
      -1,    -1,   159,    -1,   161,   162,   163,   164,   165,   166,
     167,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   176,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    49,    -1,    51,    52,
      53,    54,    -1,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    -1,    -1,    -1,    69,    -1,    71,    72,
      73,    74,    -1,    76,    -1,    -1,    79,    80,    81,    82,
      83,    84,    85,    86,    87,    88,    89,    90,    91,    92,
      93,    94,    95,    96,    -1,    98,    -1,   100,   101,   102,
     103,    -1,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,    -1,
     123,   124,    -1,    -1,    -1,    -1,    -1,    -1,   131,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,    -1,
      -1,   154,   155,    -1,    -1,    -1,   159,    -1,   161,   162,
     163,   164,   165,   166,   167,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   176,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      49,    -1,    51,    52,    53,    54,    -1,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    -1,    -1,    -1,
      69,    -1,    -1,    72,    73,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   100,   101,   102,   103,    -1,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,    -1,
     119,   120,   121,    -1,   123,   124,    -1,    -1,    -1,    -1,
      -1,    -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   145,   146,   147,    -1,
      -1,    -1,   151,   152,   153,   154,   155,    -1,    -1,    -1,
      -1,    -1,   161,   162,   163,   164,   165,   166,   167,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   176,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    49,    -1,    51,    52,    53,    54,
      -1,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    -1,    -1,    -1,    69,    -1,    -1,    72,    73,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   100,   101,   102,   103,    -1,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,    -1,   119,   120,   121,    -1,   123,   124,
      -1,    -1,    -1,    -1,    -1,    -1,   131,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     145,   146,   147,    -1,    -1,    -1,   151,   152,    -1,   154,
     155,    -1,    -1,    -1,    -1,    -1,   161,   162,   163,   164,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     151,    -1,   153,   154,   155,    -1,    -1,    -1,    -1,    -1,
     161,   162,   163,   164,   165,   166,   167,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    49,    -1,    51,    52,    53,    54,    -1,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    65,
      -1,    -1,    -1,    69,    -1,    -1,    72,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   100,   101,   102,   103,    -1,   105,
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
     162,   163,   164,   165,   166,   167,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    49,    -1,    51,    52,    53,    54,    -1,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    73,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   100,   101,    -1,   103,    -1,   105,   106,   107,
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
      -1,   161,   162,   163,   164,   165,   166,   167,     1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    47,    48,    -1,    50,    51,    -1,
      53,    -1,    55,    -1,    57,    58,    59,    60,    61,    62,
      63,    64,    65,    -1,    -1,    -1,    69,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    77,    78,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   102,
      -1,    -1,    -1,   106,   107,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   131,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   150,    -1,    -1,
      -1,   154,   155,    -1,    -1,    -1,    -1,    -1,    -1,   162,
     163,     1,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    -1,
      50,    51,    -1,    53,    -1,    55,    -1,    57,    58,    59,
      60,    61,    62,    63,    64,    65,    -1,    -1,    -1,    69,
      -1,    -1,    72,    -1,    -1,    -1,    -1,    77,    78,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   102,    -1,    -1,    -1,   106,   107,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     150,    -1,    -1,    -1,   154,   155,    -1,    -1,    -1,    -1,
      -1,    -1,   162,   163,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    -1,    -1,    -1,
      69,    -1,    -1,    72,    73,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    98,
      -1,    -1,    -1,   102,   103,    -1,    -1,   106,   107,   108,
     109,   110,   111,   112,   113,   114,    -1,    -1,    -1,   118,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   151,   152,    -1,   154,   155,    -1,    -1,    -1,
      -1,    -1,    -1,   162,   163,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      -1,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    -1,    50,    51,    -1,    53,    -1,    55,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,   106,   107,
     108,   109,   110,   111,   112,   113,   114,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   151,   152,    -1,   154,   155,    -1,    -1,
      -1,   159,    -1,    -1,   162,   163,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    -1,
      -1,    -1,    69,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      77,    78,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   102,    -1,    -1,    -1,   106,
     107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   131,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   150,    -1,    -1,    -1,   154,   155,    -1,
      -1,    -1,    -1,    -1,    -1,   162,   163,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,
      -1,    57,    58,    59,    60,    61,    62,    63,    64,    65,
      -1,    -1,    -1,    69,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    77,    78,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   102,    -1,    -1,    -1,
     106,   107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   131,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   150,    -1,    -1,    -1,   154,   155,
      -1,    -1,    -1,    -1,    -1,    -1,   162,   163,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    -1,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    -1,    50,    51,    -1,    53,    -1,
      55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   131,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   151,   152,    -1,   154,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   150,
      -1,    -1,    -1,   154,   155,    -1,     3,    -1,    -1,    -1,
      -1,   162,   163,    10,    -1,    -1,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,
      57,    58,    59,    60,    61,    62,    63,    64,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    73,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   102,    -1,    -1,    -1,   106,
     107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   131,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   150,    -1,    -1,    -1,   154,   155,    -1,
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
      -1,   150,    -1,    -1,    -1,   154,   155,    -1,    -1,    -1,
      -1,    -1,    -1,   162,   163,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    -1,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
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
     159,    -1,    -1,   162,   163,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    -1,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   131,    -1,   133,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   153,   154,   155,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   153,   154,   155,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   131,    -1,   133,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   154,   155,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   131,    -1,   133,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   154,   155,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   154,   155,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   154,   155,    -1,    -1,    -1,
      -1,    -1,    -1,   162,   163,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    -1,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      -1,    50,    51,    -1,    53,    -1,    55,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,   107,    -1,
      -1,    -1,    18,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    49,    -1,    -1,    52,    -1,    54,    -1,
      56,    -1,    -1,    -1,    -1,   154,   155,    -1,    -1,    -1,
      -1,    -1,    -1,   162,   163,    71,    -1,    73,    -1,    -1,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    91,    92,    93,    94,    95,
      96,    -1,    98,    -1,   100,   101,    -1,   103,    -1,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,   118,   119,   120,   121,    18,   123,   124,    -1,
      -1,    -1,    -1,    -1,    -1,   131,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   151,    -1,    49,   154,   155,
      52,    -1,    54,   159,    56,   161,   162,   163,   164,   165,
     166,   167,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    71,
     176,    73,    -1,    -1,    76,    -1,    -1,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    91,
      92,    93,    94,    95,    96,    -1,    98,    -1,   100,   101,
      -1,   103,    -1,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,   118,   119,   120,   121,
      -1,   123,   124,    -1,    -1,    -1,    -1,    -1,    -1,   131,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,
      -1,    -1,   154,   155,    -1,    -1,    -1,   159,    -1,   161,
     162,   163,   164,   165,   166,   167,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   176,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    49,    -1,    51,    52,    53,    54,    -1,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    73,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    85,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   100,   101,    -1,   103,    -1,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
      -1,   119,   120,   121,    -1,   123,   124,    -1,    -1,    -1,
      -1,    -1,    -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   151,    -1,    -1,   154,   155,    -1,    -1,
      -1,   159,    -1,   161,   162,   163,   164,   165,   166,   167,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    49,    -1,    51,    52,
      53,    54,    -1,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,   101,    -1,
     103,    -1,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,    -1,   119,   120,   121,    -1,
     123,   124,    -1,    -1,    -1,    -1,    -1,    -1,   131,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,    -1,
     153,   154,   155,    -1,    -1,    -1,    -1,    -1,   161,   162,
     163,   164,   165,   166,   167,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    49,    -1,    51,    52,    53,    54,    -1,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    73,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   100,   101,    -1,   103,    -1,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
      -1,   119,   120,   121,    -1,   123,   124,    -1,    -1,    -1,
      -1,    -1,    -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   151,    -1,    -1,   154,   155,    -1,    -1,
      -1,   159,    -1,   161,   162,   163,   164,   165,   166,   167,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    49,    -1,    51,    52,
      53,    54,    -1,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,   101,    -1,
     103,    -1,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,    -1,   119,   120,   121,    -1,
     123,   124,    -1,    -1,    -1,    -1,    -1,    -1,   131,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,    -1,
      -1,   154,   155,    -1,    -1,    -1,    -1,    -1,   161,   162,
     163,   164,   165,   166,   167,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    49,    -1,    51,    52,    53,    54,    -1,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    73,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   100,   101,    -1,   103,    -1,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
      -1,   119,   120,   121,    -1,   123,   124,    -1,    -1,    -1,
      -1,    -1,    -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   151,    -1,    -1,   154,   155,    -1,    -1,
      -1,    -1,    -1,   161,   162,   163,   164,   165,   166,   167,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    49,    -1,    51,    52,
      53,    54,    -1,    56,    57,    58,    59,    60,    61,    62,
      63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,   101,    -1,
     103,    -1,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,    -1,   119,   120,   121,    -1,
     123,   124,    -1,    -1,    -1,    -1,    -1,    -1,   131,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,    -1,
      -1,   154,   155,    -1,    -1,    -1,    -1,    -1,   161,   162,
     163,   164,   165,   166,   167,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    -1,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   103,    -1,    -1,   106,   107,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,   158,
      53,    -1,    -1,    -1,    57,    58,    59,    60,    61,    62,
      63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,   106,   107,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    -1,    50,    51,    -1,    53,    -1,
      55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   154,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   106,   107,    -1,    -1,    -1,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,   154,
      57,    58,    59,    60,    61,    62,    63,    64,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   131,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   151,   152,    -1,   154,   155,    -1,
      -1,    -1,   159,    -1,    -1,   162,   163,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,
      -1,    57,    58,    59,    60,    61,    62,    63,    64,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   131,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   151,   152,    -1,   154,   155,
      -1,    -1,    -1,    -1,    -1,    -1,   162,   163,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,    -1,
      -1,    -1,    57,    58,    59,    60,    61,    62,    63,    64,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   131,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   154,
     155,    -1,    -1,    -1,    -1,    -1,    -1,   162,   163,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   133,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   153,   154,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   133,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    13,    14,    15,    16,    17,    -1,    -1,    20,
     154,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    -1,    50,
      51,    -1,    53,    -1,    55,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   103,    -1,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     151,   152,    -1,   154,   155,    -1,    -1,    -1,    -1,    -1,
      -1,   162,   163,     4,     5,     6,     7,     8,     9,    10,
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
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,    -1,    -1,   154,    57,    58,    59,    60,    61,    62,
      63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   106,   107,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   131,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     153,   154,   155,    -1,    -1,    -1,    -1,    -1,    -1,   162,
     163,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,
      -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,    61,
      62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    -1,    -1,    -1,    -1,    -1,    78,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   106,   107,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   131,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   154,   155,    -1,    -1,    -1,    -1,    -1,    -1,
     162,   163,    13,    14,    15,    16,    17,    18,    -1,    20,
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
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    -1,    -1,    20,    -1,    22,    23,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     154,    -1,    -1,   106,   107,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,   154,    -1,    -1,    13,    14,    15,    16,    17,
     162,   163,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    47,
      48,    -1,    50,    51,    -1,    53,    -1,    55,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   106,   107,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   154,   155,    -1,    13,
      14,    15,    16,    17,   162,   163,    20,    -1,    22,    23,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   131,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     154,    -1,    -1,   106,   107,    -1,    -1,    -1,   162,   163,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    49,
      -1,    51,    52,    53,    54,    -1,    56,    -1,    -1,    -1,
      -1,   154,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     100,   101,    -1,   103,    -1,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,    -1,   119,
     120,   121,    -1,   123,   124,    -1,    -1,    -1,    -1,    -1,
      -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   151,    -1,    -1,   154,   155,    -1,    -1,    -1,    -1,
      -1,   161,   162,   163,   164,   165,   166,   167,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    49,    -1,    51,
      52,    53,    54,    -1,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,   101,
      -1,   103,    -1,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,    -1,   119,   120,   121,
      -1,   123,   124,    -1,    -1,    -1,    -1,    -1,    -1,   131,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,
      -1,    -1,   154,   155,    -1,    -1,    -1,    -1,    -1,   161,
     162,   163,   164,   165,   166,   167,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,
      57,    58,    59,    60,    61,    62,    63,    64,    -1,    49,
      -1,    -1,    52,    -1,    54,    72,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,   106,
     107,   108,   109,   110,   111,   112,   113,   114,    -1,    -1,
     100,   101,    -1,   103,    -1,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,    -1,   119,
     120,   121,    -1,   123,   124,    49,    -1,    -1,    52,    -1,
      54,   131,    56,    -1,    -1,    -1,    -1,   154,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   145,   146,   147,    -1,    73,
      -1,   151,   152,    -1,   154,   155,    -1,    -1,    -1,    -1,
      -1,   161,   162,   163,   164,   165,   166,   167,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   100,   101,    -1,   103,
      -1,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,    -1,   119,   120,   121,    -1,   123,
     124,    49,    -1,    -1,    52,    -1,    54,   131,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   145,   146,   147,    -1,    73,    -1,   151,   152,    -1,
     154,   155,    -1,    -1,    -1,    -1,    -1,   161,   162,   163,
     164,   165,   166,   167,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   100,   101,    -1,   103,    -1,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
      -1,   119,   120,   121,    49,   123,   124,    52,    -1,    54,
      -1,    56,    -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,
      -1,    -1,    -1,   151,   152,    -1,   154,   155,    -1,    -1,
      -1,   159,    -1,   161,   162,   163,   164,   165,   166,   167,
      -1,    -1,    -1,    -1,    -1,   100,   101,    -1,   103,    -1,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,    -1,   119,   120,   121,    49,   123,   124,
      52,    -1,    54,    -1,    56,    -1,   131,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,   151,   152,   153,   154,
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
      -1,    -1,    -1,   159,    -1,   161,   162,   163,   164,   165,
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
      -1,   151,    -1,    -1,   154,   155,    -1,    -1,   158,    -1,
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
      -1,    -1,    73,    -1,    -1,    -1,    -1,   151,    -1,    -1,
     154,   155,    -1,    -1,    -1,   159,    -1,   161,   162,   163,
     164,   165,   166,   167,    -1,    -1,    -1,    -1,    -1,   100,
     101,    -1,   103,    -1,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,    -1,   119,   120,
     121,    49,   123,   124,    52,    -1,    54,    -1,    56,    -1,
     131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,
     151,    -1,    -1,   154,   155,    -1,    -1,    -1,   159,    -1,
     161,   162,   163,   164,   165,   166,   167,    -1,    -1,    -1,
      -1,    -1,   100,   101,    -1,   103,    -1,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
      -1,   119,   120,   121,    49,   123,   124,    52,    -1,    54,
      -1,    56,    -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,
      -1,    -1,    -1,   151,    -1,   153,   154,   155,    -1,    -1,
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
      -1,   119,   120,   121,    49,   123,   124,    52,    -1,    54,
      -1,    56,    -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,
      -1,    -1,    -1,   151,    -1,    -1,   154,   155,    -1,    -1,
      -1,    -1,    -1,   161,   162,   163,   164,   165,   166,   167,
      -1,    -1,    -1,    -1,    -1,   100,   101,    -1,   103,    -1,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,    -1,   119,   120,   121,    49,   123,   124,
      52,    -1,    54,    -1,    56,    -1,   131,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,   151,    -1,    -1,   154,
     155,    -1,    -1,    -1,    -1,    -1,   161,   162,   163,   164,
     165,   166,   167,    -1,    -1,    -1,    -1,    -1,   100,   101,
      -1,   103,    -1,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,    -1,   119,   120,   121,
      -1,   123,   124,    -1,    -1,    -1,    -1,    -1,    -1,   131,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,
      -1,    -1,   154,   155,    -1,    -1,    -1,    -1,    -1,   161,
     162,   163,   164,   165,   166,   167,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      -1,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,    -1,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,    -1,    20,    72,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,   103,    53,    -1,   106,   107,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,   106,
     107,    13,    14,    15,    16,    17,    -1,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    47,    48,    -1,    50,    51,
      -1,    53,    -1,    55,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    13,    14,    15,    16,    17,    -1,    -1,    20,
      72,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    -1,    50,
      51,    -1,    53,    -1,    55,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_int16 yystos[] =
{
       0,   178,   391,   392,     3,     4,     5,     6,     7,     8,
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
     332,   333,   334,   345,   349,   383,   386,   396,   402,   404,
     410,   414,   419,   420,   421,   422,   423,   424,   425,   426,
     448,   466,   467,   468,   469,     0,   178,   103,   182,   198,
     285,   287,   296,   299,   311,   315,   320,   117,   151,    58,
      61,    62,    64,   151,   151,   408,   409,   410,   307,   308,
     106,   107,   182,   363,   384,   385,   363,   151,   396,   151,
     151,     4,   103,   106,   107,   300,   305,   306,   151,   198,
     409,   414,   420,   421,   422,   424,   425,   426,   106,   322,
     156,   178,   288,   296,   299,   419,   423,   465,   466,   469,
     470,   176,   179,   148,   159,   175,   219,   366,    89,   157,
     403,   363,   179,   179,   179,   176,   106,   107,   151,   198,
     293,   405,   414,   415,   416,   417,   418,   419,   423,   427,
     428,   429,   430,   431,   437,     3,    47,    48,    50,    55,
     313,     3,   155,   198,   287,   300,   304,   306,   316,   321,
     399,   419,   423,   469,   285,   287,   299,   311,   315,   320,
     400,   419,   423,    65,   305,   305,   300,   306,   305,   300,
     305,   300,   154,   408,   157,   179,   151,   159,   227,   408,
     408,   178,   276,   277,   155,   296,   299,   467,   363,   363,
     396,   175,   299,   151,   198,   405,   414,   419,   428,   155,
     198,   469,   397,   398,    65,    66,    67,    68,   155,   173,
     363,   372,   374,   378,   380,   381,   321,    57,   153,   155,
     198,   295,   299,   303,   304,   310,   311,   317,   318,   319,
     320,   324,   331,   332,   349,   359,   361,   448,   461,   462,
     463,   464,   469,   470,   106,   107,   159,   182,   321,   437,
     410,   151,   379,   380,   151,   151,   117,   184,   185,    49,
      52,    54,    56,    73,   100,   101,   103,   105,   115,   116,
     119,   120,   121,   123,   124,   151,   155,   161,   164,   165,
     166,   167,   180,   181,   184,   186,   189,   197,   198,   199,
     200,   203,   204,   205,   206,   207,   208,   209,   210,   211,
     212,   213,   214,   215,   221,   321,   153,   155,   197,   198,
     214,   216,   296,   321,   364,   365,   382,   465,   470,   299,
     420,   421,   422,   424,   425,   426,   153,   153,   153,   153,
     153,   153,   153,   155,   296,   448,   467,   155,   162,   198,
     216,   287,   288,   295,   297,   299,   311,   318,   320,   354,
     355,   358,   359,   360,   461,   469,   151,   419,   423,   469,
     151,   157,   103,   154,   155,   159,   181,   183,   216,   367,
     368,   369,   370,   371,    22,   367,   151,   363,   227,   151,
     157,   157,   157,   409,   414,   416,   417,   418,   427,   429,
     430,   431,   299,   415,   428,   157,    98,   407,   155,   408,
     445,   448,   408,   408,   403,   276,   151,   408,   445,   403,
     408,   408,   299,   405,   151,   151,   298,   299,   296,   299,
     178,   296,   465,   470,   323,   159,   403,   276,   363,   366,
     287,   304,   401,   419,   423,   159,   403,   276,   384,   299,
     311,   299,   299,   106,   322,   106,   107,   182,   321,   326,
     384,   178,   182,   362,   150,   178,     3,   292,   294,   299,
     303,   227,   178,   178,   407,   151,   407,   179,   216,   409,
     414,   299,   151,   178,   363,   394,   159,   363,   159,   363,
     133,   162,   163,   377,   153,   157,   363,   381,   153,   408,
     408,   156,   178,   297,   299,   311,   318,   320,   460,   461,
     469,   470,   151,   155,   163,   175,   198,   448,   450,   451,
     452,   453,   454,   455,   472,   198,   324,   469,   299,   318,
     305,   300,   408,   153,   297,   299,   462,   297,   448,   462,
      10,   161,   166,   348,   350,   351,   346,   348,   372,   175,
     372,    13,    88,   103,   106,   107,   181,   411,   412,   413,
     153,   117,   151,   197,   151,   151,   151,   200,   151,   197,
     151,   103,   106,   107,   300,   305,   306,   151,   197,   197,
      19,    21,    85,   155,   164,   165,   201,   202,   216,   223,
     227,   334,   364,   469,   157,   178,   151,   186,   155,   160,
     155,   160,   120,   122,   123,   124,   151,   154,   155,   159,
     160,   200,   200,   168,   162,   169,   170,   164,   165,   125,
     126,   127,   128,   171,   172,   129,   130,   163,   161,   173,
     131,   132,   174,   153,   157,   154,   178,   134,   135,   136,
     137,   138,   139,   140,   141,   142,   143,   144,   175,   218,
     219,   220,   151,   198,   441,   442,   443,   444,   445,   153,
     157,   153,   153,   153,   153,   153,   153,   151,   408,   445,
     448,   151,   445,   448,   178,   296,   467,   178,   179,   179,
     151,   163,   198,   414,   432,   433,   434,   435,   436,   437,
     438,   439,   440,   133,   469,   179,   179,   363,   363,   178,
     178,   178,   155,   183,   178,   368,   158,   157,   471,   367,
     154,   155,   158,   371,   152,   216,   222,   151,   178,   178,
     178,   178,   414,   416,   417,   418,   427,   429,   430,   431,
     153,   153,   153,   153,   153,   153,   153,   415,   428,   408,
     151,   366,   156,   178,   227,   403,   178,   227,   405,   223,
     365,   223,   365,   405,   394,   227,   403,   407,   159,   403,
     276,   394,   227,   403,   328,   329,   327,   159,   133,   299,
     356,   357,   360,   361,   153,   157,    70,   278,   279,   179,
     299,   292,   162,   216,   178,   414,   355,   396,   394,   156,
     178,   151,   376,   374,   375,    78,   309,   182,   159,   182,
     437,   297,   448,   462,   299,   303,   469,   178,   451,   452,
     453,   156,   178,    18,   216,   299,   450,   472,   408,   408,
     448,   297,   460,   470,   299,   182,   408,   297,   462,   321,
     157,   471,   363,   348,   159,   153,   365,   153,   153,   157,
     151,   176,   364,   186,   155,   364,   364,   364,   216,   364,
     153,   364,   364,   364,   178,   153,   164,   165,   202,    18,
     301,   153,   157,   153,   162,   163,   153,   222,   216,   159,
     216,   182,   216,   182,   115,   155,   182,   152,   190,   191,
     192,   216,   115,   155,   182,   334,   216,   190,   182,   200,
     203,   203,   203,   204,   204,   205,   205,   206,   206,   206,
     206,   207,   207,   208,   209,   210,   211,   212,   158,   223,
     176,   184,   155,   182,   216,   159,   216,   178,   442,   443,
     444,   299,   441,   408,   408,   216,   365,   151,   408,   445,
     448,   151,   445,   448,   178,   178,   156,   156,   151,   414,
     433,   434,   435,   438,    18,   299,   432,   436,   151,   408,
     454,   472,   408,   408,   472,   151,   408,   454,   408,   408,
     179,   215,   363,   156,   157,   156,   157,   472,   472,   133,
     353,   354,   355,   353,   363,   178,   214,   215,   216,   406,
     471,   367,   369,   150,   178,   153,   157,   178,   353,   182,
     405,   182,   153,   153,   153,   153,   153,   153,   151,   408,
     445,   448,   151,   408,   445,   448,   405,   184,   448,   216,
     306,   321,   446,   227,   356,   153,   153,   153,   153,   392,
     393,   227,   394,   227,   403,   393,   227,   159,   159,   159,
     335,   179,   179,   182,   280,   363,    18,    71,    73,    76,
      79,    80,    81,    82,    83,    84,    85,    86,    87,    88,
      89,    90,    92,    93,    94,    95,    96,    98,   106,   107,
     118,   178,   223,   224,   225,   226,   227,   228,   229,   231,
     232,   242,   248,   249,   250,   251,   252,   253,   258,   259,
     265,   266,   267,   281,   299,   303,   363,   404,    70,   176,
     179,   179,   179,   353,   179,   395,   393,   285,   287,   296,
     387,   388,   389,   390,   382,   175,   373,   373,   350,   408,
     408,   297,   462,   155,   162,   198,   216,   321,   216,   299,
     356,   153,   153,   153,     5,   299,   408,   450,   159,   182,
     437,    10,   351,   150,   175,   352,   159,   350,   159,   153,
     412,   190,   153,   157,   178,   157,   153,   153,   157,   153,
     200,   153,   153,   153,   200,    18,   301,   216,   153,   153,
     152,   159,   200,   156,   179,   190,   156,   156,   115,   119,
     121,   183,   193,   194,   195,   153,   157,   193,   156,   157,
     150,   214,   158,   153,   193,   179,   368,   356,   153,   153,
     153,   441,   178,   178,   356,   356,   438,   153,   153,   153,
     153,   151,   414,   437,   432,   436,   178,   178,   156,   179,
     472,   178,   178,   179,   179,   179,   179,   366,   193,   133,
     167,   179,   179,   150,   367,   216,   408,   152,   216,   353,
     179,   175,   151,   408,   445,   448,   151,   408,   445,   448,
     178,   178,   407,   153,   145,   167,   179,   447,   157,   179,
     179,   395,   393,   227,   395,   335,   335,   335,     3,    10,
      73,   150,   282,   289,   290,   296,   299,   336,   341,   465,
     153,   157,   157,   176,   151,    61,    62,   176,   227,   281,
     404,   151,    18,   225,   151,   151,   176,   363,   176,   363,
     162,   363,   159,   224,   151,   151,   151,   227,   216,   217,
     217,    14,   268,    74,   233,   176,   179,   229,    78,   176,
     363,    91,   254,   362,   299,   158,   280,   176,   156,   156,
     179,   157,   395,   405,   179,   176,   179,   176,   179,   153,
     365,   379,   379,   471,   348,   348,   178,   179,   179,   179,
     216,   179,   151,   408,   454,   448,   298,     5,   162,   179,
     216,   350,   408,   408,   321,   363,   159,   215,   350,   471,
     150,   178,   153,   295,   182,    78,   187,   188,   364,   200,
     200,   200,   200,   200,   159,   368,   157,   150,   196,   155,
     194,   196,   196,   156,   157,   122,   154,   192,   156,   222,
     214,   176,   156,   471,   179,   151,   408,   445,   448,   356,
     356,   179,   179,   153,   151,   408,   445,   448,   151,   408,
     454,   414,   408,   408,   356,   356,   156,   355,   358,   358,
     359,   153,   157,   157,   153,   179,   215,   215,   156,   156,
     179,   179,   153,   216,   178,   178,   356,   356,   366,   408,
     157,   216,   216,   306,   321,   156,   153,   150,   395,   150,
     150,   150,   150,   296,   334,   342,   465,   296,   341,   151,
     330,   176,   176,   151,   158,   198,   337,   338,   344,   414,
     415,   428,   157,   176,   363,   178,   363,   153,   190,   191,
     176,   227,   176,   227,   223,    80,   153,   223,   234,   281,
     283,   286,   292,   299,   303,   145,   146,   147,   152,   153,
     176,   223,   243,   244,   245,   281,   176,   176,   223,   176,
     368,   176,   223,   222,   223,   110,   111,   112,   113,   114,
     260,   262,   263,   176,    97,   176,    84,   151,   151,   179,
     150,   176,   176,   151,   225,   227,   408,   176,   153,   178,
     150,   150,   178,   157,   157,   150,   159,   159,   156,   156,
     156,   179,   153,   178,   216,   216,   179,   156,   179,   471,
     347,   348,   352,   352,   368,   471,   150,   387,   449,   450,
     153,   158,   153,   157,   158,   368,   471,   222,   120,   193,
     194,   155,   194,   155,   194,   156,   150,   153,   178,   179,
     179,   153,   153,   178,   178,   179,   179,   179,   178,   178,
     156,   179,   153,   408,   356,   356,   179,   179,   223,   447,
     150,   330,   330,   330,   151,   198,   339,   340,   445,   456,
     457,   458,   459,   176,   157,   176,   337,   176,   382,   409,
     414,   216,   299,   157,   176,   343,   344,   343,   363,   133,
     360,   361,   223,   153,   153,   151,   225,   153,   223,   299,
     145,   146,   147,   167,   176,   246,   247,   225,   224,   176,
     247,   153,   158,   223,   152,   223,   224,   245,   176,   471,
     153,   153,   153,   227,   262,   263,   151,   216,   151,   184,
     234,   200,   255,   109,     1,   225,   408,   388,   178,   178,
     350,   350,   156,   356,   179,   179,   156,   156,   150,   348,
     159,   471,   150,   179,   153,   216,   188,   216,   471,   150,
     156,   156,   193,   193,   356,   153,   153,   356,   356,   153,
     153,   156,   157,   133,   355,   133,   156,   179,   179,   153,
     153,   156,   216,   457,   458,   459,   299,   456,   157,   176,
     408,   408,   176,   153,   414,   408,   176,   225,    77,    78,
     159,   237,   238,   239,   153,   223,    75,   225,   223,   152,
     223,    75,   176,   106,   152,   223,   224,   245,   152,   223,
     225,   244,   247,   247,   176,   223,   150,   159,   239,   225,
     151,   178,   176,   184,   153,   158,   153,   153,   157,   158,
     253,   257,   363,   405,   471,   471,   179,   156,   156,   159,
     350,   150,   150,   150,   156,   156,   179,   179,   179,   178,
     179,   153,   153,   153,   153,   153,   456,   408,   338,     1,
     215,   235,   236,   406,     1,   158,     1,   178,   225,   237,
      75,   176,   153,   225,    75,   176,   167,   167,   225,   224,
     247,   247,   176,   106,   223,   167,   167,    75,   152,   223,
     152,   223,   224,   176,     1,   178,   178,   264,   297,   299,
     465,   158,   176,   155,   184,   269,   270,   271,   225,   200,
     190,    75,   108,   254,   256,   150,   150,   153,   350,   471,
     153,   153,   153,   358,   151,   408,   445,   448,   340,   133,
       1,   157,   158,   150,   274,   275,   281,   225,    75,   176,
     225,   223,   152,   152,   223,   152,   223,   152,   223,   224,
     152,   223,   152,   223,   225,   167,   167,   167,   167,   150,
     274,   264,   179,   151,   198,   405,   456,   182,   158,   103,
     151,   153,   158,   157,    75,   153,   225,   151,   225,   225,
     471,   150,   178,   215,   235,   238,   240,   241,   281,   225,
     167,   167,   167,   167,   152,   152,   223,   152,   223,   152,
     223,   240,   179,   176,   261,   299,   269,   156,   215,   176,
     269,   271,   225,   223,   109,   109,   150,   356,   225,   230,
     179,   238,   152,   152,   223,   152,   223,   152,   223,   179,
     261,   214,   153,   158,   184,   153,   153,   158,   153,   257,
      75,   252,   179,     1,   225,   150,   230,   150,   153,   227,
     184,   272,   151,   176,   272,   225,    75,   153,   227,   157,
     158,   215,   153,   225,   184,   182,   273,   153,   176,   153,
     157,   176,   182
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
     345,   345,   345,   345,   345,   348,   348,   349,   349,   350,
     350,   350,   350,   351,   351,   352,   352,   352,   353,   353,
     353,   353,   353,   353,   353,   354,   354,   354,   354,   355,
     355,   356,   356,   356,   356,   357,   357,   357,   357,   358,
     358,   358,   358,   358,   359,   359,   359,   359,   359,   360,
     360,   361,   361,   362,   362,   363,   363,   363,   364,   364,
     364,   365,   365,   366,   366,   366,   366,   367,   367,   368,
     368,   368,   368,   368,   369,   369,   370,   370,   371,   371,
     371,   371,   371,   372,   372,   373,   373,   375,   374,   376,
     374,   374,   374,   377,   377,   377,   377,   378,   378,   378,
     378,   379,   379,   380,   380,   381,   381,   382,   382,   382,
     382,   383,   383,   383,   384,   384,   385,   385,   386,   386,
     387,   387,   388,   388,   389,   389,   389,   390,   390,   391,
     391,   392,   392,   393,   393,   394,   395,   396,   396,   396,
     396,   396,   396,   396,   396,   396,   396,   396,   397,   396,
     398,   396,   399,   396,   400,   396,   401,   396,   402,   402,
     402,   403,   403,   404,   404,   404,   404,   404,   404,   404,
     404,   404,   404,   405,   405,   405,   406,   407,   407,   408,
     408,   409,   409,   410,   411,   411,   412,   412,   412,   413,
     413,   413,   413,   413,   413,   414,   414,   415,   415,   415,
     415,   416,   416,   416,   416,   417,   417,   417,   417,   417,
     417,   417,   418,   418,   418,   418,   419,   419,   419,   420,
     420,   420,   420,   420,   421,   421,   421,   421,   422,   422,
     422,   422,   422,   422,   423,   423,   423,   424,   424,   424,
     424,   424,   425,   425,   425,   425,   426,   426,   426,   426,
     426,   426,   427,   427,   428,   428,   428,   428,   429,   429,
     429,   429,   430,   430,   430,   430,   430,   430,   430,   431,
     431,   431,   431,   431,   432,   432,   432,   432,   432,   433,
     433,   433,   434,   434,   434,   434,   435,   435,   435,   436,
     436,   436,   436,   436,   437,   437,   438,   438,   438,   439,
     439,   440,   440,   441,   441,   441,   442,   442,   442,   442,
     442,   443,   443,   443,   443,   444,   444,   444,   445,   445,
     445,   445,   445,   446,   446,   446,   446,   446,   446,   447,
     447,   448,   448,   448,   448,   449,   449,   450,   450,   450,
     450,   451,   451,   451,   451,   451,   452,   452,   452,   452,
     453,   453,   453,   454,   454,   454,   455,   455,   455,   455,
     455,   455,   456,   456,   456,   457,   457,   457,   457,   457,
     458,   458,   458,   458,   459,   459,   460,   460,   460,   461,
     461,   462,   462,   462,   462,   462,   462,   463,   463,   463,
     463,   463,   463,   463,   463,   463,   463,   464,   464,   464,
     464,   465,   465,   465,   466,   466,   467,   467,   467,   467,
     467,   467,   468,   468,   468,   468,   468,   468,   469,   469,
     469,   470,   470,   471,   471,   472,   472
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
       0,     1,     2,     6,     0,     9,     8,     9,     8,     0,
      13,    11,    12,    11,     1,     0,     1,     3,     3,     3,
       2,     5,     5,     1,     1,     0,     2,     5,     0,     1,
       1,     1,     5,     5,     5,     1,     5,     5,     9,     1,
       5,     0,     1,     1,     5,     1,     1,     5,     5,     1,
       3,     3,     4,     1,     1,     1,     1,     2,     1,     3,
       3,     2,     3,     1,     3,     1,     1,     1,     1,     1,
       2,     1,     1,     0,     2,     2,     4,     1,     4,     0,
       1,     2,     3,     4,     2,     2,     1,     2,     2,     5,
       5,     7,     6,     1,     3,     0,     2,     0,     5,     0,
       5,     3,     1,     0,     1,     1,     1,     1,     1,     1,
       1,     0,     1,     1,     2,     5,     6,     1,     1,     3,
       3,     2,     3,     3,     2,     4,     1,     4,     7,    10,
       1,     4,     2,     2,     1,     1,     5,     2,     5,     0,
       1,     3,     4,     0,     1,     0,     0,     1,     1,     2,
       2,     2,     2,     2,     2,     1,     2,     5,     0,     6,
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
       7,     5,     1,     1,     1,     3,     3,     3,     5,     1,
       1,     5,     5,     6,     6,     0,     1,     1,     3,     2,
       2,     1,     2,     2,     3,     4,     1,     4,     4,     3,
       5,     8,     3,     1,     2,     1,     2,     6,     5,     6,
       7,     7,     1,     2,     2,     1,     2,     2,     3,     4,
       1,     4,     4,     3,     8,     3,     1,     1,     2,     1,
       1,     2,     3,     2,     3,     2,     3,     3,     2,     4,
       3,     2,     3,     2,     4,     3,     2,     6,     6,     6,
       7,     1,     2,     1,     1,     1,     2,     3,     2,     3,
       2,     3,     3,     4,     2,     3,     4,     2,     5,     6,
       7,     6,     6,     0,     1,     0,     2
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
#line 583 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.enterScope(); }
#line 7686 "Parser/parser.cc"
    break;

  case 3:
#line 587 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 7692 "Parser/parser.cc"
    break;

  case 4:
#line 594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantInteger( *(yyvsp[0].tok) ) ); }
#line 7698 "Parser/parser.cc"
    break;

  case 5:
#line 595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7704 "Parser/parser.cc"
    break;

  case 6:
#line 596 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7710 "Parser/parser.cc"
    break;

  case 7:
#line 597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7716 "Parser/parser.cc"
    break;

  case 8:
#line 598 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantChar( *(yyvsp[0].tok) ) ); }
#line 7722 "Parser/parser.cc"
    break;

  case 19:
#line 619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 7728 "Parser/parser.cc"
    break;

  case 20:
#line 623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.constant) = build_constantStr( *(yyvsp[0].str) ); }
#line 7734 "Parser/parser.cc"
    break;

  case 21:
#line 627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 7740 "Parser/parser.cc"
    break;

  case 22:
#line 629 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 7750 "Parser/parser.cc"
    break;

  case 23:
#line 640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 7756 "Parser/parser.cc"
    break;

  case 24:
#line 642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 7762 "Parser/parser.cc"
    break;

  case 25:
#line 646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_dimensionref( (yyvsp[0].tok) ) ); }
#line 7768 "Parser/parser.cc"
    break;

  case 27:
#line 649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 7774 "Parser/parser.cc"
    break;

  case 28:
#line 651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new StmtExpr( dynamic_cast<CompoundStmt *>(maybeMoveBuild<Statement>((yyvsp[-1].sn)) ) ) ); }
#line 7780 "Parser/parser.cc"
    break;

  case 29:
#line 653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_qualified_expr( (yyvsp[-2].decl), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7786 "Parser/parser.cc"
    break;

  case 30:
#line 655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7792 "Parser/parser.cc"
    break;

  case 31:
#line 657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild<Expression>( (yyvsp[-3].en) );
			(yyval.en) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 7802 "Parser/parser.cc"
    break;

  case 32:
#line 667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, "n expression" ); (yyval.en) = nullptr; }
#line 7808 "Parser/parser.cc"
    break;

  case 33:
#line 669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.en) = nullptr; }
#line 7814 "Parser/parser.cc"
    break;

  case 34:
#line 671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.en) = nullptr; }
#line 7820 "Parser/parser.cc"
    break;

  case 35:
#line 673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.en) = nullptr; }
#line 7826 "Parser/parser.cc"
    break;

  case 36:
#line 675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.en) = nullptr; }
#line 7832 "Parser/parser.cc"
    break;

  case 37:
#line 677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.en) = nullptr; }
#line 7838 "Parser/parser.cc"
    break;

  case 39:
#line 683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// steal the association node from the singleton and delete the wrapper
			(yyvsp[-2].genexpr)->associations.splice((yyvsp[-2].genexpr)->associations.end(), (yyvsp[0].genexpr)->associations);
			delete (yyvsp[0].genexpr);
			(yyval.genexpr) = (yyvsp[-2].genexpr);
		}
#line 7849 "Parser/parser.cc"
    break;

  case 40:
#line 693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuildType((yyvsp[-2].decl)), maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } );
		}
#line 7858 "Parser/parser.cc"
    break;

  case 41:
#line 698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } ); }
#line 7864 "Parser/parser.cc"
    break;

  case 43:
#line 707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-5].en), new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) ) ) )) ) ); }
#line 7870 "Parser/parser.cc"
    break;

  case 44:
#line 713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7876 "Parser/parser.cc"
    break;

  case 45:
#line 715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7882 "Parser/parser.cc"
    break;

  case 46:
#line 717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, new ExpressionNode( (yyvsp[-3].constant) ), (yyvsp[-1].en) ) ); }
#line 7888 "Parser/parser.cc"
    break;

  case 47:
#line 719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.en) = new ExpressionNode( new ConstructorExpr( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) ) );
		}
#line 7898 "Parser/parser.cc"
    break;

  case 48:
#line 725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7904 "Parser/parser.cc"
    break;

  case 49:
#line 728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( new string( "__builtin_va_arg") ) ),
											   (ExpressionNode *)((yyvsp[-4].en)->set_last( (ExpressionNode *)((yyvsp[-1].decl) ? (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) ) : (yyvsp[-2].decl)) )) ) ); }
#line 7911 "Parser/parser.cc"
    break;

  case 50:
#line 731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 7917 "Parser/parser.cc"
    break;

  case 51:
#line 733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 7923 "Parser/parser.cc"
    break;

  case 52:
#line 735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), new ExpressionNode( (yyvsp[-2].constant) ) ) ); }
#line 7929 "Parser/parser.cc"
    break;

  case 53:
#line 737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7935 "Parser/parser.cc"
    break;

  case 54:
#line 739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 7941 "Parser/parser.cc"
    break;

  case 55:
#line 741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-1].en), build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) ) ) ); }
#line 7947 "Parser/parser.cc"
    break;

  case 56:
#line 743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7953 "Parser/parser.cc"
    break;

  case 57:
#line 745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[0].aggKey), (yyvsp[-2].en) ) ); }
#line 7959 "Parser/parser.cc"
    break;

  case 58:
#line 747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7965 "Parser/parser.cc"
    break;

  case 59:
#line 749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 7971 "Parser/parser.cc"
    break;

  case 60:
#line 751 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7977 "Parser/parser.cc"
    break;

  case 61:
#line 753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::IncrPost, (yyvsp[-1].en) ) ); }
#line 7983 "Parser/parser.cc"
    break;

  case 62:
#line 755 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::DecrPost, (yyvsp[-1].en) ) ); }
#line 7989 "Parser/parser.cc"
    break;

  case 63:
#line 757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].in), true ) ) ); }
#line 7995 "Parser/parser.cc"
    break;

  case 64:
#line 759 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].in), true ))->set_maybeConstructed( false ) ) ); }
#line 8001 "Parser/parser.cc"
    break;

  case 65:
#line 761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) );
		}
#line 8011 "Parser/parser.cc"
    break;

  case 66:
#line 770 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8017 "Parser/parser.cc"
    break;

  case 69:
#line 777 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 8023 "Parser/parser.cc"
    break;

  case 70:
#line 782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Default parameter for argument is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8029 "Parser/parser.cc"
    break;

  case 73:
#line 789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 8035 "Parser/parser.cc"
    break;

  case 75:
#line 795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-1].tok) ) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 8041 "Parser/parser.cc"
    break;

  case 76:
#line 797 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-3].tok) ) ), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 8047 "Parser/parser.cc"
    break;

  case 77:
#line 799 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 8053 "Parser/parser.cc"
    break;

  case 78:
#line 801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 8059 "Parser/parser.cc"
    break;

  case 79:
#line 803 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 8065 "Parser/parser.cc"
    break;

  case 80:
#line 805 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 8071 "Parser/parser.cc"
    break;

  case 81:
#line 810 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_constantInteger( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 8077 "Parser/parser.cc"
    break;

  case 82:
#line 812 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_field_name_FLOATINGconstant( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 8083 "Parser/parser.cc"
    break;

  case 83:
#line 814 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_varref( (yyvsp[-1].tok) ), (yyvsp[0].en) ) );
		}
#line 8091 "Parser/parser.cc"
    break;

  case 84:
#line 821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8097 "Parser/parser.cc"
    break;

  case 85:
#line 823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Expression * constant = build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) );
			(yyval.en) = (yyvsp[-1].en) != nullptr ? new ExpressionNode( build_fieldSel( (yyvsp[-1].en),  constant ) ) : new ExpressionNode( constant );
		}
#line 8106 "Parser/parser.cc"
    break;

  case 88:
#line 835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 8112 "Parser/parser.cc"
    break;

  case 89:
#line 837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en)->set_extension( true ); }
#line 8118 "Parser/parser.cc"
    break;

  case 90:
#line 842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 8138 "Parser/parser.cc"
    break;

  case 91:
#line 858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_val( (yyvsp[-1].op), (yyvsp[0].en) ) ); }
#line 8144 "Parser/parser.cc"
    break;

  case 92:
#line 860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Incr, (yyvsp[0].en) ) ); }
#line 8150 "Parser/parser.cc"
    break;

  case 93:
#line 862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Decr, (yyvsp[0].en) ) ); }
#line 8156 "Parser/parser.cc"
    break;

  case 94:
#line 864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 8162 "Parser/parser.cc"
    break;

  case 95:
#line 866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8168 "Parser/parser.cc"
    break;

  case 96:
#line 868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 8174 "Parser/parser.cc"
    break;

  case 97:
#line 870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8180 "Parser/parser.cc"
    break;

  case 98:
#line 872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_offsetOf( (yyvsp[-3].decl), build_varref( (yyvsp[-1].tok) ) ) ); }
#line 8186 "Parser/parser.cc"
    break;

  case 99:
#line 874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.en) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 8195 "Parser/parser.cc"
    break;

  case 100:
#line 881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::PointTo; }
#line 8201 "Parser/parser.cc"
    break;

  case 101:
#line 882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AddressOf; }
#line 8207 "Parser/parser.cc"
    break;

  case 102:
#line 884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::And; }
#line 8213 "Parser/parser.cc"
    break;

  case 103:
#line 888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::UnPlus; }
#line 8219 "Parser/parser.cc"
    break;

  case 104:
#line 889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::UnMinus; }
#line 8225 "Parser/parser.cc"
    break;

  case 105:
#line 890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::Neg; }
#line 8231 "Parser/parser.cc"
    break;

  case 106:
#line 891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::BitNeg; }
#line 8237 "Parser/parser.cc"
    break;

  case 108:
#line 897 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cast( (yyvsp[-2].decl), (yyvsp[0].en) ) ); }
#line 8243 "Parser/parser.cc"
    break;

  case 109:
#line 899 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 8249 "Parser/parser.cc"
    break;

  case 110:
#line 901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 8255 "Parser/parser.cc"
    break;

  case 111:
#line 903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( nullptr ) ) ); }
#line 8261 "Parser/parser.cc"
    break;

  case 112:
#line 905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 8267 "Parser/parser.cc"
    break;

  case 113:
#line 907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Return cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8273 "Parser/parser.cc"
    break;

  case 114:
#line 909 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8279 "Parser/parser.cc"
    break;

  case 115:
#line 911 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8285 "Parser/parser.cc"
    break;

  case 123:
#line 931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Exp, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8291 "Parser/parser.cc"
    break;

  case 125:
#line 937 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mul, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8297 "Parser/parser.cc"
    break;

  case 126:
#line 939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Div, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8303 "Parser/parser.cc"
    break;

  case 127:
#line 941 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mod, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8309 "Parser/parser.cc"
    break;

  case 129:
#line 947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Plus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8315 "Parser/parser.cc"
    break;

  case 130:
#line 949 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Minus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8321 "Parser/parser.cc"
    break;

  case 132:
#line 955 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8327 "Parser/parser.cc"
    break;

  case 133:
#line 957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::RShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8333 "Parser/parser.cc"
    break;

  case 135:
#line 963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8339 "Parser/parser.cc"
    break;

  case 136:
#line 965 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8345 "Parser/parser.cc"
    break;

  case 137:
#line 967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8351 "Parser/parser.cc"
    break;

  case 138:
#line 969 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8357 "Parser/parser.cc"
    break;

  case 140:
#line 975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Eq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8363 "Parser/parser.cc"
    break;

  case 141:
#line 977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Neq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8369 "Parser/parser.cc"
    break;

  case 143:
#line 983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitAnd, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8375 "Parser/parser.cc"
    break;

  case 145:
#line 989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Xor, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8381 "Parser/parser.cc"
    break;

  case 147:
#line 995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitOr, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8387 "Parser/parser.cc"
    break;

  case 149:
#line 1001 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), true ) ); }
#line 8393 "Parser/parser.cc"
    break;

  case 151:
#line 1007 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), false ) ); }
#line 8399 "Parser/parser.cc"
    break;

  case 153:
#line 1013 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8405 "Parser/parser.cc"
    break;

  case 154:
#line 1016 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-3].en), (yyvsp[-3].en), (yyvsp[0].en) ) ); }
#line 8411 "Parser/parser.cc"
    break;

  case 157:
#line 1027 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
//			if ( $2 == OperKinds::AtAssn ) {
//				SemanticError( yylloc, "C @= assignment is currently unimplemented." ); $$ = nullptr;
//			} else {
				(yyval.en) = new ExpressionNode( build_binary_val( (yyvsp[-1].op), (yyvsp[-2].en), (yyvsp[0].en) ) );
//			} // if
		}
#line 8423 "Parser/parser.cc"
    break;

  case 158:
#line 1035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8429 "Parser/parser.cc"
    break;

  case 159:
#line 1040 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8435 "Parser/parser.cc"
    break;

  case 163:
#line 1050 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::Assign; }
#line 8441 "Parser/parser.cc"
    break;

  case 164:
#line 1051 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AtAssn; }
#line 8447 "Parser/parser.cc"
    break;

  case 165:
#line 1055 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ExpAssn; }
#line 8453 "Parser/parser.cc"
    break;

  case 166:
#line 1056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MulAssn; }
#line 8459 "Parser/parser.cc"
    break;

  case 167:
#line 1057 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::DivAssn; }
#line 8465 "Parser/parser.cc"
    break;

  case 168:
#line 1058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ModAssn; }
#line 8471 "Parser/parser.cc"
    break;

  case 169:
#line 1059 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::PlusAssn; }
#line 8477 "Parser/parser.cc"
    break;

  case 170:
#line 1060 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MinusAssn; }
#line 8483 "Parser/parser.cc"
    break;

  case 171:
#line 1061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::LSAssn; }
#line 8489 "Parser/parser.cc"
    break;

  case 172:
#line 1062 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::RSAssn; }
#line 8495 "Parser/parser.cc"
    break;

  case 173:
#line 1063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AndAssn; }
#line 8501 "Parser/parser.cc"
    break;

  case 174:
#line 1064 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ERAssn; }
#line 8507 "Parser/parser.cc"
    break;

  case 175:
#line 1065 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::OrAssn; }
#line 8513 "Parser/parser.cc"
    break;

  case 176:
#line 1076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)(new ExpressionNode( nullptr ) )->set_last( (yyvsp[-1].en) ) ) ); }
#line 8519 "Parser/parser.cc"
    break;

  case 177:
#line 1078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-4].en)->set_last( (yyvsp[-1].en) ) ) )); }
#line 8525 "Parser/parser.cc"
    break;

  case 179:
#line 1084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8531 "Parser/parser.cc"
    break;

  case 180:
#line 1086 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 8537 "Parser/parser.cc"
    break;

  case 181:
#line 1088 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8543 "Parser/parser.cc"
    break;

  case 183:
#line 1094 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new CommaExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 8549 "Parser/parser.cc"
    break;

  case 184:
#line 1099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8555 "Parser/parser.cc"
    break;

  case 196:
#line 1117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 8561 "Parser/parser.cc"
    break;

  case 198:
#line 1120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_directive( (yyvsp[0].tok) ) ); }
#line 8567 "Parser/parser.cc"
    break;

  case 199:
#line 1126 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[0].sn)->add_label( (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 8573 "Parser/parser.cc"
    break;

  case 200:
#line 1128 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "Label \"", *(yyvsp[-3].tok).str, "\" must be associated with a statement, "
											   "where a declaration, case, or default is not a statement. "
											   "Move the label or terminate with a semi-colon." ) );
			(yyval.sn) = nullptr;
		}
#line 8584 "Parser/parser.cc"
    break;

  case 201:
#line 1138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (StatementNode *)0 ) ); }
#line 8590 "Parser/parser.cc"
    break;

  case 202:
#line 1143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (yyvsp[-2].sn) ) ); }
#line 8596 "Parser/parser.cc"
    break;

  case 204:
#line 1149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 8602 "Parser/parser.cc"
    break;

  case 205:
#line 1154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8608 "Parser/parser.cc"
    break;

  case 206:
#line 1156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8614 "Parser/parser.cc"
    break;

  case 207:
#line 1158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8620 "Parser/parser.cc"
    break;

  case 208:
#line 1160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8626 "Parser/parser.cc"
    break;

  case 211:
#line 1167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 8632 "Parser/parser.cc"
    break;

  case 212:
#line 1169 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Declarations only allowed at the start of the switch body, i.e., after the '{'." ); (yyval.sn) = nullptr; }
#line 8638 "Parser/parser.cc"
    break;

  case 213:
#line 1174 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_expr( (yyvsp[-1].en) ) ); }
#line 8644 "Parser/parser.cc"
    break;

  case 214:
#line 1176 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_mutex( nullptr, new StatementNode( build_expr( (yyvsp[-1].en) ) ) ) ); }
#line 8650 "Parser/parser.cc"
    break;

  case 215:
#line 1183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn); }
#line 8656 "Parser/parser.cc"
    break;

  case 216:
#line 1185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( true, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8662 "Parser/parser.cc"
    break;

  case 217:
#line 1187 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( true, (yyvsp[-7].en), (yyvsp[-2].sn) ) );
			// The semantics of the declaration list is changed to include associated initialization, which is performed
			// *before* the transfer to the appropriate case clause by hoisting the declarations into a compound
			// statement around the switch.  Statements after the initial declaration list can never be executed, and
			// therefore, are removed from the grammar even though C allows it. The change also applies to choose
			// statement.
			(yyval.sn) = (yyvsp[-3].decl) ? new StatementNode( build_compound( (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 8676 "Parser/parser.cc"
    break;

  case 218:
#line 1197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Only declarations can appear before the list of case clauses." ); (yyval.sn) = nullptr; }
#line 8682 "Parser/parser.cc"
    break;

  case 219:
#line 1199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( false, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8688 "Parser/parser.cc"
    break;

  case 220:
#line 1201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( false, (yyvsp[-7].en), (yyvsp[-2].sn) ) );
			(yyval.sn) = (yyvsp[-3].decl) ? new StatementNode( build_compound( (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 8697 "Parser/parser.cc"
    break;

  case 221:
#line 1206 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Only declarations can appear before the list of case clauses." ); (yyval.sn) = nullptr; }
#line 8703 "Parser/parser.cc"
    break;

  case 222:
#line 1212 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-2].ifctl), maybe_build_compound( (yyvsp[0].sn) ), nullptr ) ); }
#line 8709 "Parser/parser.cc"
    break;

  case 223:
#line 1214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-4].ifctl), maybe_build_compound( (yyvsp[-2].sn) ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8715 "Parser/parser.cc"
    break;

  case 224:
#line 1219 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( nullptr, (yyvsp[0].en) ); }
#line 8721 "Parser/parser.cc"
    break;

  case 225:
#line 1221 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 8727 "Parser/parser.cc"
    break;

  case 226:
#line 1223 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 8733 "Parser/parser.cc"
    break;

  case 227:
#line 1225 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[-1].decl), (yyvsp[0].en) ); }
#line 8739 "Parser/parser.cc"
    break;

  case 228:
#line 1232 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = (yyvsp[0].en); }
#line 8745 "Parser/parser.cc"
    break;

  case 229:
#line 1234 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 8751 "Parser/parser.cc"
    break;

  case 231:
#line 1239 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.sn) = new StatementNode( build_case( (yyvsp[0].en) ) ); }
#line 8757 "Parser/parser.cc"
    break;

  case 232:
#line 1241 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)((yyvsp[-2].sn)->set_last( new StatementNode( build_case( (yyvsp[0].en) ) ) ) ); }
#line 8763 "Parser/parser.cc"
    break;

  case 233:
#line 1246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing case list after case." ); (yyval.sn) = nullptr; }
#line 8769 "Parser/parser.cc"
    break;

  case 234:
#line 1247 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn); }
#line 8775 "Parser/parser.cc"
    break;

  case 235:
#line 1249 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing colon after case list." ); (yyval.sn) = nullptr; }
#line 8781 "Parser/parser.cc"
    break;

  case 236:
#line 1250 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.sn) = new StatementNode( build_default() ); }
#line 8787 "Parser/parser.cc"
    break;

  case 237:
#line 1253 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing colon after default." ); (yyval.sn) = nullptr; }
#line 8793 "Parser/parser.cc"
    break;

  case 239:
#line 1258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)( (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) )); }
#line 8799 "Parser/parser.cc"
    break;

  case 240:
#line 1262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( maybe_build_compound( (yyvsp[0].sn) ) ); }
#line 8805 "Parser/parser.cc"
    break;

  case 241:
#line 1267 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = nullptr; }
#line 8811 "Parser/parser.cc"
    break;

  case 243:
#line 1273 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ); }
#line 8817 "Parser/parser.cc"
    break;

  case 244:
#line 1275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)( (yyvsp[-2].sn)->set_last( (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ) ) ); }
#line 8823 "Parser/parser.cc"
    break;

  case 245:
#line 1280 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8829 "Parser/parser.cc"
    break;

  case 246:
#line 1282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_while( new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( (yyvsp[-2].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse, "" );
		}
#line 8838 "Parser/parser.cc"
    break;

  case 247:
#line 1287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( (yyvsp[-2].ifctl), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8844 "Parser/parser.cc"
    break;

  case 248:
#line 1289 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( (yyvsp[-4].ifctl), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[0].sn) ) ); }
#line 8850 "Parser/parser.cc"
    break;

  case 249:
#line 1291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( NEW_ONE, maybe_build_compound( (yyvsp[-4].sn) ) ) ); }
#line 8856 "Parser/parser.cc"
    break;

  case 250:
#line 1293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_do_while( NEW_ONE, maybe_build_compound( (yyvsp[-5].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse, "" );
		}
#line 8865 "Parser/parser.cc"
    break;

  case 251:
#line 1298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( (yyvsp[-2].en), maybe_build_compound( (yyvsp[-5].sn) ) ) ); }
#line 8871 "Parser/parser.cc"
    break;

  case 252:
#line 1300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( (yyvsp[-3].en), maybe_build_compound( (yyvsp[-6].sn) ), (yyvsp[0].sn) ) ); }
#line 8877 "Parser/parser.cc"
    break;

  case 253:
#line 1302 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( new ForCtrl( (ExpressionNode * )nullptr, (ExpressionNode * )nullptr, (ExpressionNode * )nullptr ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8883 "Parser/parser.cc"
    break;

  case 254:
#line 1304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_for( new ForCtrl( (ExpressionNode * )nullptr, (ExpressionNode * )nullptr, (ExpressionNode * )nullptr ), maybe_build_compound( (yyvsp[-2].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse, "" );
		}
#line 8892 "Parser/parser.cc"
    break;

  case 255:
#line 1309 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( (yyvsp[-2].fctl), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8898 "Parser/parser.cc"
    break;

  case 256:
#line 1311 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( (yyvsp[-4].fctl), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[0].sn) ) ); }
#line 8904 "Parser/parser.cc"
    break;

  case 258:
#line 1321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 8923 "Parser/parser.cc"
    break;

  case 259:
#line 1339 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (ExpressionNode * )nullptr, (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8929 "Parser/parser.cc"
    break;

  case 260:
#line 1341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8935 "Parser/parser.cc"
    break;

  case 261:
#line 1343 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-3].decl), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8941 "Parser/parser.cc"
    break;

  case 262:
#line 1346 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (ExpressionNode *)nullptr, (yyvsp[0].en), nullptr ); }
#line 8947 "Parser/parser.cc"
    break;

  case 263:
#line 1348 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (ExpressionNode *)nullptr, (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8953 "Parser/parser.cc"
    break;

  case 264:
#line 1351 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), NEW_ZERO, OperKinds::LThan, (yyvsp[0].en)->clone(), NEW_ONE ); }
#line 8959 "Parser/parser.cc"
    break;

  case 265:
#line 1353 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].compop), NEW_ZERO, (yyvsp[0].en)->clone() ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 8965 "Parser/parser.cc"
    break;

  case 266:
#line 1356 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].compop), (yyvsp[-2].en)->clone(), (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), (yyvsp[-2].en)->clone() ), NEW_ONE ); }
#line 8971 "Parser/parser.cc"
    break;

  case 267:
#line 1358 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[0].en)->clone(), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 8980 "Parser/parser.cc"
    break;

  case 268:
#line 1363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
		}
#line 8989 "Parser/parser.cc"
    break;

  case 269:
#line 1368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en)->clone(), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), (yyvsp[0].en) ); }
#line 8995 "Parser/parser.cc"
    break;

  case 270:
#line 1370 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-2].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].en)->clone(), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 9004 "Parser/parser.cc"
    break;

  case 271:
#line 1375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
		}
#line 9013 "Parser/parser.cc"
    break;

  case 272:
#line 1380 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 9019 "Parser/parser.cc"
    break;

  case 273:
#line 1382 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 9025 "Parser/parser.cc"
    break;

  case 274:
#line 1384 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 9031 "Parser/parser.cc"
    break;

  case 275:
#line 1386 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 9037 "Parser/parser.cc"
    break;

  case 276:
#line 1388 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 9043 "Parser/parser.cc"
    break;

  case 277:
#line 1391 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-2].en), NEW_ZERO, OperKinds::LThan, (yyvsp[0].en)->clone(), NEW_ONE ); }
#line 9049 "Parser/parser.cc"
    break;

  case 278:
#line 1393 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-3].en), UPDOWN( (yyvsp[-1].compop), NEW_ZERO, (yyvsp[0].en)->clone() ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9055 "Parser/parser.cc"
    break;

  case 279:
#line 1396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), UPDOWN( (yyvsp[-1].compop), (yyvsp[-2].en)->clone(), (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), (yyvsp[-2].en)->clone() ), NEW_ONE ); }
#line 9061 "Parser/parser.cc"
    break;

  case 280:
#line 1398 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-4].en), (yyvsp[0].en)->clone(), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 9070 "Parser/parser.cc"
    break;

  case 281:
#line 1403 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::GThan || (yyvsp[-1].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 9080 "Parser/parser.cc"
    break;

  case 282:
#line 1409 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing low/high value for up/down-to range so index is uninitialized." ); (yyval.fctl) = nullptr; }
#line 9086 "Parser/parser.cc"
    break;

  case 283:
#line 1412 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en)->clone(), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), (yyvsp[0].en) ); }
#line 9092 "Parser/parser.cc"
    break;

  case 284:
#line 1414 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-6].en), (yyvsp[-2].en)->clone(), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 9101 "Parser/parser.cc"
    break;

  case 285:
#line 1419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 9111 "Parser/parser.cc"
    break;

  case 286:
#line 1425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en)->clone(), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), nullptr ); }
#line 9117 "Parser/parser.cc"
    break;

  case 287:
#line 1427 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-6].en), (yyvsp[-2].en)->clone(), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 9126 "Parser/parser.cc"
    break;

  case 288:
#line 1432 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 9136 "Parser/parser.cc"
    break;

  case 289:
#line 1438 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing low/high value for up/down-to range so index is uninitialized." ); (yyval.fctl) = nullptr; }
#line 9142 "Parser/parser.cc"
    break;

  case 290:
#line 1441 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-1].decl), NEW_ZERO, OperKinds::LThan, (yyvsp[0].en), NEW_ONE ); }
#line 9148 "Parser/parser.cc"
    break;

  case 291:
#line 1443 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].decl), UPDOWN( (yyvsp[-1].compop), NEW_ZERO, (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9154 "Parser/parser.cc"
    break;

  case 292:
#line 1446 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-3].decl), UPDOWN( (yyvsp[-1].compop), (yyvsp[-2].en)->clone(), (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), (yyvsp[-2].en)->clone() ), NEW_ONE ); }
#line 9160 "Parser/parser.cc"
    break;

  case 293:
#line 1448 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-3].decl), (yyvsp[0].en), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 9169 "Parser/parser.cc"
    break;

  case 294:
#line 1453 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::GThan || (yyvsp[-1].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-3].decl), (yyvsp[-2].en), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 9179 "Parser/parser.cc"
    break;

  case 295:
#line 1460 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), (yyvsp[0].en) ); }
#line 9185 "Parser/parser.cc"
    break;

  case 296:
#line 1462 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-2].en), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 9194 "Parser/parser.cc"
    break;

  case 297:
#line 1467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-4].en), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 9204 "Parser/parser.cc"
    break;

  case 298:
#line 1473 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), nullptr ); }
#line 9210 "Parser/parser.cc"
    break;

  case 299:
#line 1475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-2].en), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 9219 "Parser/parser.cc"
    break;

  case 300:
#line 1480 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-4].en), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 9229 "Parser/parser.cc"
    break;

  case 301:
#line 1486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing low/high value for up/down-to range so index is uninitialized." ); (yyval.fctl) = nullptr; }
#line 9235 "Parser/parser.cc"
    break;

  case 302:
#line 1489 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.fctl) = nullptr;
			//$$ = forCtrl( new ExpressionNode( build_varref( $3 ) ), $1, nullptr, OperKinds::Range, nullptr, nullptr );
		}
#line 9244 "Parser/parser.cc"
    break;

  case 303:
#line 1494 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LEThan || (yyvsp[-1].compop) == OperKinds::GEThan ) { SemanticError( yylloc, "All enumation ranges are equal (all values). Remove \"=~\"." ); (yyval.fctl) = nullptr; }
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.fctl) = nullptr;
		}
#line 9253 "Parser/parser.cc"
    break;

  case 304:
#line 1502 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GThan; }
#line 9259 "Parser/parser.cc"
    break;

  case 305:
#line 1504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 9265 "Parser/parser.cc"
    break;

  case 306:
#line 1506 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GEThan; }
#line 9271 "Parser/parser.cc"
    break;

  case 307:
#line 1511 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LThan; }
#line 9277 "Parser/parser.cc"
    break;

  case 308:
#line 1513 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GThan; }
#line 9283 "Parser/parser.cc"
    break;

  case 310:
#line 1519 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 9289 "Parser/parser.cc"
    break;

  case 311:
#line 1521 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GEThan; }
#line 9295 "Parser/parser.cc"
    break;

  case 312:
#line 1526 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Goto ) ); }
#line 9301 "Parser/parser.cc"
    break;

  case 313:
#line 1530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_computedgoto( (yyvsp[-1].en) ) ); }
#line 9307 "Parser/parser.cc"
    break;

  case 314:
#line 1533 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThrough ) ); }
#line 9313 "Parser/parser.cc"
    break;

  case 315:
#line 1535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::FallThrough ) ); }
#line 9319 "Parser/parser.cc"
    break;

  case 316:
#line 1537 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThroughDefault ) ); }
#line 9325 "Parser/parser.cc"
    break;

  case 317:
#line 1540 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Continue ) ); }
#line 9331 "Parser/parser.cc"
    break;

  case 318:
#line 1544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Continue ) ); }
#line 9337 "Parser/parser.cc"
    break;

  case 319:
#line 1547 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Break ) ); }
#line 9343 "Parser/parser.cc"
    break;

  case 320:
#line 1551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Break ) ); }
#line 9349 "Parser/parser.cc"
    break;

  case 321:
#line 1553 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_return( (yyvsp[-1].en) ) ); }
#line 9355 "Parser/parser.cc"
    break;

  case 322:
#line 1555 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 9361 "Parser/parser.cc"
    break;

  case 323:
#line 1557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr ) ); }
#line 9367 "Parser/parser.cc"
    break;

  case 324:
#line 1559 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn) ) ); }
#line 9373 "Parser/parser.cc"
    break;

  case 325:
#line 1561 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Coroutine ) ); }
#line 9379 "Parser/parser.cc"
    break;

  case 326:
#line 1563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Coroutine ) ); }
#line 9385 "Parser/parser.cc"
    break;

  case 327:
#line 1565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Generator ) ); }
#line 9391 "Parser/parser.cc"
    break;

  case 328:
#line 1567 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Generator ) ); }
#line 9397 "Parser/parser.cc"
    break;

  case 329:
#line 1569 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_throw( (yyvsp[-1].en) ) ); }
#line 9403 "Parser/parser.cc"
    break;

  case 330:
#line 1571 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume( (yyvsp[-1].en) ) ); }
#line 9409 "Parser/parser.cc"
    break;

  case 331:
#line 1573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume_at( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 9415 "Parser/parser.cc"
    break;

  case 334:
#line 1583 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_with( (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 9421 "Parser/parser.cc"
    break;

  case 335:
#line 1589 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_mutex( (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 9427 "Parser/parser.cc"
    break;

  case 336:
#line 1593 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 9433 "Parser/parser.cc"
    break;

  case 337:
#line 1598 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9439 "Parser/parser.cc"
    break;

  case 339:
#line 1604 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 9445 "Parser/parser.cc"
    break;

  case 340:
#line 1608 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) )); }
#line 9451 "Parser/parser.cc"
    break;

  case 342:
#line 1615 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.en) = nullptr; }
#line 9457 "Parser/parser.cc"
    break;

  case 343:
#line 1619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 9463 "Parser/parser.cc"
    break;

  case 344:
#line 1624 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 9469 "Parser/parser.cc"
    break;

  case 345:
#line 1626 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-3].en), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[-4].en), (yyvsp[0].wfs) ); }
#line 9475 "Parser/parser.cc"
    break;

  case 346:
#line 1628 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 9481 "Parser/parser.cc"
    break;

  case 347:
#line 1630 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( nullptr, maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 9487 "Parser/parser.cc"
    break;

  case 348:
#line 1633 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 9493 "Parser/parser.cc"
    break;

  case 349:
#line 1635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-5].en), maybe_build_compound( (yyvsp[-4].sn) ), (yyvsp[-6].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 9499 "Parser/parser.cc"
    break;

  case 350:
#line 1640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-1].en), (yyvsp[0].sn), (yyvsp[-2].en) ) ); }
#line 9505 "Parser/parser.cc"
    break;

  case 351:
#line 1642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-3].en), (yyvsp[-2].sn), (yyvsp[-4].en), (yyvsp[0].wfs) ) ); }
#line 9511 "Parser/parser.cc"
    break;

  case 352:
#line 1647 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), (yyvsp[0].sn), 0 ) ); }
#line 9517 "Parser/parser.cc"
    break;

  case 353:
#line 1649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), 0, (yyvsp[0].sn) ) ); }
#line 9523 "Parser/parser.cc"
    break;

  case 354:
#line 1651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-2].sn), (yyvsp[-1].sn), (yyvsp[0].sn) ) ); }
#line 9529 "Parser/parser.cc"
    break;

  case 355:
#line 1656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 9535 "Parser/parser.cc"
    break;

  case 356:
#line 1658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)(yyvsp[-8].sn)->set_last( new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ) ); }
#line 9541 "Parser/parser.cc"
    break;

  case 357:
#line 1663 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9547 "Parser/parser.cc"
    break;

  case 358:
#line 1664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[0].en); }
#line 9553 "Parser/parser.cc"
    break;

  case 359:
#line 1668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 9559 "Parser/parser.cc"
    break;

  case 360:
#line 1669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 9565 "Parser/parser.cc"
    break;

  case 361:
#line 1670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.catch_kind) = CatchStmt::Resume; }
#line 9571 "Parser/parser.cc"
    break;

  case 362:
#line 1671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Resume; }
#line 9577 "Parser/parser.cc"
    break;

  case 363:
#line 1675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = new StatementNode( build_finally( (yyvsp[0].sn) ) ); }
#line 9583 "Parser/parser.cc"
    break;

  case 365:
#line 1682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 9589 "Parser/parser.cc"
    break;

  case 366:
#line 1684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 9595 "Parser/parser.cc"
    break;

  case 367:
#line 1686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 9601 "Parser/parser.cc"
    break;

  case 372:
#line 1701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-4].flag), (yyvsp[-2].constant), 0 ) ); }
#line 9607 "Parser/parser.cc"
    break;

  case 373:
#line 1703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-6].flag), (yyvsp[-4].constant), (yyvsp[-2].en) ) ); }
#line 9613 "Parser/parser.cc"
    break;

  case 374:
#line 1705 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-8].flag), (yyvsp[-6].constant), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 9619 "Parser/parser.cc"
    break;

  case 375:
#line 1707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-10].flag), (yyvsp[-8].constant), (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 9625 "Parser/parser.cc"
    break;

  case 376:
#line 1709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-12].flag), (yyvsp[-9].constant), 0, (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].label) ) ); }
#line 9631 "Parser/parser.cc"
    break;

  case 377:
#line 1714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = false; }
#line 9637 "Parser/parser.cc"
    break;

  case 378:
#line 1716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = true; }
#line 9643 "Parser/parser.cc"
    break;

  case 379:
#line 1721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9649 "Parser/parser.cc"
    break;

  case 382:
#line 1728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 9655 "Parser/parser.cc"
    break;

  case 383:
#line 1733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( nullptr, (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 9661 "Parser/parser.cc"
    break;

  case 384:
#line 1735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( (yyvsp[-5].tok), (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 9667 "Parser/parser.cc"
    break;

  case 385:
#line 1740 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9673 "Parser/parser.cc"
    break;

  case 386:
#line 1742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 9679 "Parser/parser.cc"
    break;

  case 387:
#line 1744 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( (yyvsp[0].constant) ) )); }
#line 9685 "Parser/parser.cc"
    break;

  case 388:
#line 1749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = new LabelNode(); (yyval.label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 9694 "Parser/parser.cc"
    break;

  case 389:
#line 1754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = (yyvsp[-2].label); (yyvsp[-2].label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 9703 "Parser/parser.cc"
    break;

  case 390:
#line 1764 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9709 "Parser/parser.cc"
    break;

  case 393:
#line 1771 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 9715 "Parser/parser.cc"
    break;

  case 394:
#line 1776 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9721 "Parser/parser.cc"
    break;

  case 396:
#line 1782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 9727 "Parser/parser.cc"
    break;

  case 397:
#line 1784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-2].decl) ); }
#line 9733 "Parser/parser.cc"
    break;

  case 404:
#line 1804 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "C_DECLARATION1 %p %s\n", $$, $$->name ? $$->name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 9744 "Parser/parser.cc"
    break;

  case 407:
#line 1816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-4].en), (yyvsp[-2].constant) ); }
#line 9750 "Parser/parser.cc"
    break;

  case 408:
#line 1818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-2].en), build_constantStr( *new string( "\"\"" ) ) ); }
#line 9756 "Parser/parser.cc"
    break;

  case 412:
#line 1836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9762 "Parser/parser.cc"
    break;

  case 414:
#line 1842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].in) ); }
#line 9768 "Parser/parser.cc"
    break;

  case 415:
#line 1846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 9774 "Parser/parser.cc"
    break;

  case 416:
#line 1848 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].in) ) ); }
#line 9780 "Parser/parser.cc"
    break;

  case 417:
#line 1855 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 9786 "Parser/parser.cc"
    break;

  case 418:
#line 1857 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 9792 "Parser/parser.cc"
    break;

  case 419:
#line 1859 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 9798 "Parser/parser.cc"
    break;

  case 421:
#line 1865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9804 "Parser/parser.cc"
    break;

  case 422:
#line 1867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9810 "Parser/parser.cc"
    break;

  case 423:
#line 1869 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 9816 "Parser/parser.cc"
    break;

  case 424:
#line 1871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeClone( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->appendList( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 9827 "Parser/parser.cc"
    break;

  case 425:
#line 1904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), 0 )->addQualifiers( (yyvsp[0].decl) ); }
#line 9833 "Parser/parser.cc"
    break;

  case 426:
#line 1906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), 0 )->addQualifiers( (yyvsp[0].decl) ); }
#line 9839 "Parser/parser.cc"
    break;

  case 427:
#line 1911 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 9845 "Parser/parser.cc"
    break;

  case 428:
#line 1914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-6].decl)->appendList( (yyvsp[-2].decl) ) ); }
#line 9851 "Parser/parser.cc"
    break;

  case 429:
#line 1919 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 9860 "Parser/parser.cc"
    break;

  case 430:
#line 1924 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 9869 "Parser/parser.cc"
    break;

  case 431:
#line 1929 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "3" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 9878 "Parser/parser.cc"
    break;

  case 432:
#line 1940 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "4" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef();
		}
#line 9887 "Parser/parser.cc"
    break;

  case 433:
#line 1945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "5" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 9896 "Parser/parser.cc"
    break;

  case 434:
#line 1950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "6" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[-3].decl) )->addTypedef();
		}
#line 9905 "Parser/parser.cc"
    break;

  case 435:
#line 1955 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "7" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-2].decl) )->addTypedef();
		}
#line 9914 "Parser/parser.cc"
    break;

  case 436:
#line 1960 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "8" );
			(yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-3].decl) )->addTypedef()->addType( (yyvsp[-3].decl) );
		}
#line 9923 "Parser/parser.cc"
    break;

  case 437:
#line 1969 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 9931 "Parser/parser.cc"
    break;

  case 438:
#line 1973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 9939 "Parser/parser.cc"
    break;

  case 439:
#line 1980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 9945 "Parser/parser.cc"
    break;

  case 443:
#line 1990 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 9951 "Parser/parser.cc"
    break;

  case 444:
#line 1992 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ) ); }
#line 9957 "Parser/parser.cc"
    break;

  case 452:
#line 2015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_type_specifier2 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 9968 "Parser/parser.cc"
    break;

  case 457:
#line 2037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9974 "Parser/parser.cc"
    break;

  case 460:
#line 2049 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9980 "Parser/parser.cc"
    break;

  case 463:
#line 2059 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Const ); }
#line 9986 "Parser/parser.cc"
    break;

  case 464:
#line 2061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Restrict ); }
#line 9992 "Parser/parser.cc"
    break;

  case 465:
#line 2063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Volatile ); }
#line 9998 "Parser/parser.cc"
    break;

  case 466:
#line 2065 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Atomic ); }
#line 10004 "Parser/parser.cc"
    break;

  case 468:
#line 2071 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newForall( (yyvsp[-1].decl) ); }
#line 10010 "Parser/parser.cc"
    break;

  case 470:
#line 2077 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10016 "Parser/parser.cc"
    break;

  case 471:
#line 2079 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10022 "Parser/parser.cc"
    break;

  case 473:
#line 2090 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10028 "Parser/parser.cc"
    break;

  case 474:
#line 2095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Extern ); }
#line 10034 "Parser/parser.cc"
    break;

  case 475:
#line 2097 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Static ); }
#line 10040 "Parser/parser.cc"
    break;

  case 476:
#line 2099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Auto ); }
#line 10046 "Parser/parser.cc"
    break;

  case 477:
#line 2101 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Register ); }
#line 10052 "Parser/parser.cc"
    break;

  case 478:
#line 2103 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::ThreadlocalGcc ); }
#line 10058 "Parser/parser.cc"
    break;

  case 479:
#line 2105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::ThreadlocalC11 ); }
#line 10064 "Parser/parser.cc"
    break;

  case 480:
#line 2108 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Inline ); }
#line 10070 "Parser/parser.cc"
    break;

  case 481:
#line 2110 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Fortran ); }
#line 10076 "Parser/parser.cc"
    break;

  case 482:
#line 2112 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Noreturn ); }
#line 10082 "Parser/parser.cc"
    break;

  case 483:
#line 2117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 10088 "Parser/parser.cc"
    break;

  case 484:
#line 2119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Bool ); }
#line 10094 "Parser/parser.cc"
    break;

  case 485:
#line 2121 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Char ); }
#line 10100 "Parser/parser.cc"
    break;

  case 486:
#line 2123 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int ); }
#line 10106 "Parser/parser.cc"
    break;

  case 487:
#line 2125 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 ); }
#line 10112 "Parser/parser.cc"
    break;

  case 488:
#line 2127 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 )->addType( DeclarationNode::newSignedNess( DeclarationNode::Unsigned ) ); }
#line 10118 "Parser/parser.cc"
    break;

  case 489:
#line 2129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Float ); }
#line 10124 "Parser/parser.cc"
    break;

  case 490:
#line 2131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Double ); }
#line 10130 "Parser/parser.cc"
    break;

  case 491:
#line 2133 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat80 ); }
#line 10136 "Parser/parser.cc"
    break;

  case 492:
#line 2135 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat128 ); }
#line 10142 "Parser/parser.cc"
    break;

  case 493:
#line 2137 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat16 ); }
#line 10148 "Parser/parser.cc"
    break;

  case 494:
#line 2139 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32 ); }
#line 10154 "Parser/parser.cc"
    break;

  case 495:
#line 2141 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32x ); }
#line 10160 "Parser/parser.cc"
    break;

  case 496:
#line 2143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64 ); }
#line 10166 "Parser/parser.cc"
    break;

  case 497:
#line 2145 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64x ); }
#line 10172 "Parser/parser.cc"
    break;

  case 498:
#line 2147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat128 ); }
#line 10178 "Parser/parser.cc"
    break;

  case 499:
#line 2149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10184 "Parser/parser.cc"
    break;

  case 500:
#line 2151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10190 "Parser/parser.cc"
    break;

  case 501:
#line 2153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10196 "Parser/parser.cc"
    break;

  case 502:
#line 2155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Complex ); }
#line 10202 "Parser/parser.cc"
    break;

  case 503:
#line 2157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Imaginary ); }
#line 10208 "Parser/parser.cc"
    break;

  case 504:
#line 2159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Signed ); }
#line 10214 "Parser/parser.cc"
    break;

  case 505:
#line 2161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Unsigned ); }
#line 10220 "Parser/parser.cc"
    break;

  case 506:
#line 2163 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Short ); }
#line 10226 "Parser/parser.cc"
    break;

  case 507:
#line 2165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Long ); }
#line 10232 "Parser/parser.cc"
    break;

  case 508:
#line 2167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Valist ); }
#line 10238 "Parser/parser.cc"
    break;

  case 509:
#line 2169 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::AutoType ); }
#line 10244 "Parser/parser.cc"
    break;

  case 511:
#line 2175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10250 "Parser/parser.cc"
    break;

  case 513:
#line 2181 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVtableType( (yyvsp[-2].decl) ); }
#line 10256 "Parser/parser.cc"
    break;

  case 514:
#line 2187 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10262 "Parser/parser.cc"
    break;

  case 515:
#line 2189 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10268 "Parser/parser.cc"
    break;

  case 517:
#line 2196 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10274 "Parser/parser.cc"
    break;

  case 518:
#line 2198 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10280 "Parser/parser.cc"
    break;

  case 519:
#line 2200 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10286 "Parser/parser.cc"
    break;

  case 520:
#line 2202 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 10292 "Parser/parser.cc"
    break;

  case 522:
#line 2209 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10298 "Parser/parser.cc"
    break;

  case 524:
#line 2215 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10304 "Parser/parser.cc"
    break;

  case 525:
#line 2217 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10310 "Parser/parser.cc"
    break;

  case 526:
#line 2219 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 10316 "Parser/parser.cc"
    break;

  case 527:
#line 2224 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10322 "Parser/parser.cc"
    break;

  case 528:
#line 2226 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en) ); }
#line 10328 "Parser/parser.cc"
    break;

  case 529:
#line 2228 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 10334 "Parser/parser.cc"
    break;

  case 530:
#line 2230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en), true ); }
#line 10340 "Parser/parser.cc"
    break;

  case 531:
#line 2232 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Zero ); }
#line 10346 "Parser/parser.cc"
    break;

  case 532:
#line 2234 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::One ); }
#line 10352 "Parser/parser.cc"
    break;

  case 533:
#line 2239 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_declaration_specifier %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10363 "Parser/parser.cc"
    break;

  case 534:
#line 2246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10369 "Parser/parser.cc"
    break;

  case 535:
#line 2248 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10375 "Parser/parser.cc"
    break;

  case 536:
#line 2250 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10381 "Parser/parser.cc"
    break;

  case 537:
#line 2255 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_type_specifier %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10392 "Parser/parser.cc"
    break;

  case 538:
#line 2262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 10398 "Parser/parser.cc"
    break;

  case 539:
#line 2264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10404 "Parser/parser.cc"
    break;

  case 540:
#line 2266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 10413 "Parser/parser.cc"
    break;

  case 542:
#line 2275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10419 "Parser/parser.cc"
    break;

  case 543:
#line 2277 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10425 "Parser/parser.cc"
    break;

  case 544:
#line 2279 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10431 "Parser/parser.cc"
    break;

  case 546:
#line 2285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10437 "Parser/parser.cc"
    break;

  case 547:
#line 2287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10443 "Parser/parser.cc"
    break;

  case 549:
#line 2293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10449 "Parser/parser.cc"
    break;

  case 550:
#line 2295 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10455 "Parser/parser.cc"
    break;

  case 551:
#line 2297 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10461 "Parser/parser.cc"
    break;

  case 553:
#line 2303 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10467 "Parser/parser.cc"
    break;

  case 554:
#line 2305 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10473 "Parser/parser.cc"
    break;

  case 555:
#line 2310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypedef( (yyvsp[0].tok) ); }
#line 10479 "Parser/parser.cc"
    break;

  case 556:
#line 2312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 10485 "Parser/parser.cc"
    break;

  case 557:
#line 2314 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 10491 "Parser/parser.cc"
    break;

  case 559:
#line 2317 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), (yyvsp[0].decl) ); }
#line 10497 "Parser/parser.cc"
    break;

  case 560:
#line 2319 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), (yyvsp[0].decl) ); }
#line 10503 "Parser/parser.cc"
    break;

  case 561:
#line 2324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[0].tok), nullptr ); }
#line 10509 "Parser/parser.cc"
    break;

  case 562:
#line 2326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-2].tok), nullptr ); }
#line 10515 "Parser/parser.cc"
    break;

  case 563:
#line 2328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 10521 "Parser/parser.cc"
    break;

  case 564:
#line 2333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "elaborated_type %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10532 "Parser/parser.cc"
    break;

  case 568:
#line 2349 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 10538 "Parser/parser.cc"
    break;

  case 569:
#line 2351 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 10544 "Parser/parser.cc"
    break;

  case 570:
#line 2353 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 10553 "Parser/parser.cc"
    break;

  case 571:
#line 2358 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 10570 "Parser/parser.cc"
    break;

  case 572:
#line 2371 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 10579 "Parser/parser.cc"
    break;

  case 573:
#line 2376 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "AGG3\n" );
			DeclarationNode::newFromTypedef( (yyvsp[-5].tok) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 10589 "Parser/parser.cc"
    break;

  case 574:
#line 2382 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 10598 "Parser/parser.cc"
    break;

  case 575:
#line 2387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "AGG4\n" );
			DeclarationNode::newFromTypeGen( (yyvsp[-5].tok), nullptr );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 10608 "Parser/parser.cc"
    break;

  case 577:
#line 2397 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 10614 "Parser/parser.cc"
    break;

  case 578:
#line 2399 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 10620 "Parser/parser.cc"
    break;

  case 579:
#line 2404 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 10630 "Parser/parser.cc"
    break;

  case 580:
#line 2410 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
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
#line 10645 "Parser/parser.cc"
    break;

  case 583:
#line 2429 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Struct; }
#line 10651 "Parser/parser.cc"
    break;

  case 584:
#line 2431 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Union; }
#line 10657 "Parser/parser.cc"
    break;

  case 585:
#line 2433 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Exception; }
#line 10663 "Parser/parser.cc"
    break;

  case 586:
#line 2439 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 10669 "Parser/parser.cc"
    break;

  case 587:
#line 2441 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 10675 "Parser/parser.cc"
    break;

  case 588:
#line 2443 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Generator; }
#line 10681 "Parser/parser.cc"
    break;

  case 589:
#line 2445 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor generator is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 10687 "Parser/parser.cc"
    break;

  case 590:
#line 2447 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Coroutine; }
#line 10693 "Parser/parser.cc"
    break;

  case 591:
#line 2449 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor coroutine is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 10699 "Parser/parser.cc"
    break;

  case 592:
#line 2451 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Thread; }
#line 10705 "Parser/parser.cc"
    break;

  case 593:
#line 2453 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor thread is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 10711 "Parser/parser.cc"
    break;

  case 594:
#line 2458 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10717 "Parser/parser.cc"
    break;

  case 595:
#line 2460 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 10723 "Parser/parser.cc"
    break;

  case 596:
#line 2465 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "type_specifier1 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
			(yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) );
			// printf( "type_specifier2 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10736 "Parser/parser.cc"
    break;

  case 597:
#line 2474 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 10742 "Parser/parser.cc"
    break;

  case 598:
#line 2476 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! (yyvsp[-1].decl) ) {								// field declarator ?
				(yyvsp[-1].decl) = DeclarationNode::newName( nullptr );
			} // if
			(yyvsp[-1].decl)->inLine = true;
			(yyval.decl) = distAttr( (yyvsp[-2].decl), (yyvsp[-1].decl) );					// mark all fields in list
			distInl( (yyvsp[-1].decl) );
		}
#line 10755 "Parser/parser.cc"
    break;

  case 599:
#line 2485 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10761 "Parser/parser.cc"
    break;

  case 602:
#line 2489 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 10767 "Parser/parser.cc"
    break;

  case 603:
#line 2491 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10773 "Parser/parser.cc"
    break;

  case 606:
#line 2498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10779 "Parser/parser.cc"
    break;

  case 608:
#line 2501 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 10785 "Parser/parser.cc"
    break;

  case 609:
#line 2506 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].en) ); }
#line 10791 "Parser/parser.cc"
    break;

  case 610:
#line 2509 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 10797 "Parser/parser.cc"
    break;

  case 611:
#line 2512 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 10803 "Parser/parser.cc"
    break;

  case 612:
#line 2517 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10809 "Parser/parser.cc"
    break;

  case 614:
#line 2520 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 10815 "Parser/parser.cc"
    break;

  case 616:
#line 2531 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 10821 "Parser/parser.cc"
    break;

  case 617:
#line 2533 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 10827 "Parser/parser.cc"
    break;

  case 619:
#line 2540 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 10833 "Parser/parser.cc"
    break;

  case 620:
#line 2545 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 10839 "Parser/parser.cc"
    break;

  case 622:
#line 2551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 10845 "Parser/parser.cc"
    break;

  case 623:
#line 2556 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, false )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10851 "Parser/parser.cc"
    break;

  case 624:
#line 2558 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); }
#line 10857 "Parser/parser.cc"
    break;

  case 625:
#line 2560 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].hide) )->addQualifiers( (yyvsp[-7].decl) ); }
#line 10863 "Parser/parser.cc"
    break;

  case 626:
#line 2563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].decl)->name, (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].hide) )->addQualifiers( (yyvsp[-6].decl) ); }
#line 10869 "Parser/parser.cc"
    break;

  case 627:
#line 2565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-6].decl)->storageClasses.val != 0 || (yyvsp[-6].decl)->type->qualifiers.val != 0 )
			{ SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }

			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true, (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 10880 "Parser/parser.cc"
    break;

  case 628:
#line 2572 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 10888 "Parser/parser.cc"
    break;

  case 629:
#line 2576 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].decl)->storageClasses.val != 0 || (yyvsp[-4].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			typedefTable.makeTypedef( *(yyvsp[-1].tok) );
		}
#line 10897 "Parser/parser.cc"
    break;

  case 630:
#line 2581 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-7].tok), (yyvsp[-2].decl), true, true, (yyvsp[-10].decl), (yyvsp[-4].hide) )->addQualifiers( (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 10905 "Parser/parser.cc"
    break;

  case 631:
#line 2586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, true, nullptr, (yyvsp[-4].hide) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 10913 "Parser/parser.cc"
    break;

  case 632:
#line 2591 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].decl)->name, (yyvsp[-2].decl), true, true, (yyvsp[-9].decl), (yyvsp[-4].hide) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 10921 "Parser/parser.cc"
    break;

  case 633:
#line 2596 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].decl)->name, (yyvsp[-2].decl), true, true, nullptr, (yyvsp[-4].hide) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 10929 "Parser/parser.cc"
    break;

  case 635:
#line 2604 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.hide) = EnumHiding::Visible; }
#line 10935 "Parser/parser.cc"
    break;

  case 636:
#line 2606 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.hide) = EnumHiding::Hide; }
#line 10941 "Parser/parser.cc"
    break;

  case 637:
#line 2611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); (yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), 0, false, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 10947 "Parser/parser.cc"
    break;

  case 638:
#line 2613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].decl)->type->symbolic.name );	(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].decl)->type->symbolic.name, 0, false, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 10953 "Parser/parser.cc"
    break;

  case 639:
#line 2618 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].in) ); }
#line 10959 "Parser/parser.cc"
    break;

  case 640:
#line 2620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumInLine( *(yyvsp[0].decl)->type->symbolic.name ); }
#line 10965 "Parser/parser.cc"
    break;

  case 641:
#line 2622 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].in) ) ); }
#line 10971 "Parser/parser.cc"
    break;

  case 642:
#line 2624 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( DeclarationNode::newEnumValueGeneric( new string("inline"), nullptr ) ); }
#line 10977 "Parser/parser.cc"
    break;

  case 644:
#line 2630 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.hide) = EnumHiding::Visible; }
#line 10983 "Parser/parser.cc"
    break;

  case 645:
#line 2635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 10989 "Parser/parser.cc"
    break;

  case 646:
#line 2636 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = new InitializerNode( (yyvsp[0].en) ); }
#line 10995 "Parser/parser.cc"
    break;

  case 647:
#line 2637 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                     { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 11001 "Parser/parser.cc"
    break;

  case 648:
#line 2644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 11007 "Parser/parser.cc"
    break;

  case 649:
#line 2646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11013 "Parser/parser.cc"
    break;

  case 652:
#line 2650 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11019 "Parser/parser.cc"
    break;

  case 653:
#line 2652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 11025 "Parser/parser.cc"
    break;

  case 654:
#line 2654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 11031 "Parser/parser.cc"
    break;

  case 656:
#line 2662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11037 "Parser/parser.cc"
    break;

  case 657:
#line 2664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11043 "Parser/parser.cc"
    break;

  case 658:
#line 2666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-8].decl)->appendList( (yyvsp[-4].decl) )->appendList( (yyvsp[0].decl) ); }
#line 11049 "Parser/parser.cc"
    break;

  case 660:
#line 2672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11055 "Parser/parser.cc"
    break;

  case 661:
#line 2677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11061 "Parser/parser.cc"
    break;

  case 662:
#line 2679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11067 "Parser/parser.cc"
    break;

  case 664:
#line 2682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 11073 "Parser/parser.cc"
    break;

  case 667:
#line 2689 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11079 "Parser/parser.cc"
    break;

  case 668:
#line 2691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11085 "Parser/parser.cc"
    break;

  case 670:
#line 2700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 11091 "Parser/parser.cc"
    break;

  case 671:
#line 2703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 11097 "Parser/parser.cc"
    break;

  case 672:
#line 2705 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 11103 "Parser/parser.cc"
    break;

  case 677:
#line 2715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11109 "Parser/parser.cc"
    break;

  case 679:
#line 2722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 11115 "Parser/parser.cc"
    break;

  case 680:
#line 2724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 11121 "Parser/parser.cc"
    break;

  case 681:
#line 2729 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 11127 "Parser/parser.cc"
    break;

  case 682:
#line 2731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 11133 "Parser/parser.cc"
    break;

  case 683:
#line 2740 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11139 "Parser/parser.cc"
    break;

  case 684:
#line 2742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 11145 "Parser/parser.cc"
    break;

  case 690:
#line 2755 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 11151 "Parser/parser.cc"
    break;

  case 693:
#line 2765 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 11157 "Parser/parser.cc"
    break;

  case 694:
#line 2766 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (yyvsp[-1].op) == OperKinds::Assign ? (yyvsp[0].in) : (yyvsp[0].in)->set_maybeConstructed( false ); }
#line 11163 "Parser/parser.cc"
    break;

  case 695:
#line 2767 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.in) = new InitializerNode( true ); }
#line 11169 "Parser/parser.cc"
    break;

  case 696:
#line 2768 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 11175 "Parser/parser.cc"
    break;

  case 697:
#line 2772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = new InitializerNode( (yyvsp[0].en) ); }
#line 11181 "Parser/parser.cc"
    break;

  case 698:
#line 2773 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 11187 "Parser/parser.cc"
    break;

  case 699:
#line 2778 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 11193 "Parser/parser.cc"
    break;

  case 701:
#line 2780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ); }
#line 11199 "Parser/parser.cc"
    break;

  case 702:
#line 2781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (InitializerNode *)( (yyvsp[-2].in)->set_last( (yyvsp[0].in) ) ); }
#line 11205 "Parser/parser.cc"
    break;

  case 703:
#line 2782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.in) = (InitializerNode *)((yyvsp[-3].in)->set_last( (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ) )); }
#line 11211 "Parser/parser.cc"
    break;

  case 705:
#line 2798 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[-1].tok) ) ); }
#line 11217 "Parser/parser.cc"
    break;

  case 707:
#line 2804 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-1].en)->set_last( (yyvsp[0].en) )); }
#line 11223 "Parser/parser.cc"
    break;

  case 708:
#line 2810 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 11229 "Parser/parser.cc"
    break;

  case 709:
#line 2813 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 11235 "Parser/parser.cc"
    break;

  case 710:
#line 2815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 11241 "Parser/parser.cc"
    break;

  case 711:
#line 2817 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-4].en) ), maybeMoveBuild<Expression>( (yyvsp[-2].en) ) ) ); }
#line 11247 "Parser/parser.cc"
    break;

  case 712:
#line 2819 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 11253 "Parser/parser.cc"
    break;

  case 714:
#line 2843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl) ); }
#line 11259 "Parser/parser.cc"
    break;

  case 715:
#line 2848 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11265 "Parser/parser.cc"
    break;

  case 716:
#line 2850 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 11271 "Parser/parser.cc"
    break;

  case 717:
#line 2855 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "9" );
			if ( (yyvsp[-1].tclass) == TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 11282 "Parser/parser.cc"
    break;

  case 718:
#line 2862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 11288 "Parser/parser.cc"
    break;

  case 719:
#line 2864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "9" ); }
#line 11294 "Parser/parser.cc"
    break;

  case 720:
#line 2866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 11300 "Parser/parser.cc"
    break;

  case 721:
#line 2868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "9" );
			(yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 11309 "Parser/parser.cc"
    break;

  case 722:
#line 2874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 11315 "Parser/parser.cc"
    break;

  case 723:
#line 2879 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 11321 "Parser/parser.cc"
    break;

  case 724:
#line 2881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 11327 "Parser/parser.cc"
    break;

  case 725:
#line 2883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::DStype; }
#line 11333 "Parser/parser.cc"
    break;

  case 726:
#line 2887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 11339 "Parser/parser.cc"
    break;

  case 727:
#line 2892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 11345 "Parser/parser.cc"
    break;

  case 728:
#line 2894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 11351 "Parser/parser.cc"
    break;

  case 729:
#line 2896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ftype; }
#line 11357 "Parser/parser.cc"
    break;

  case 730:
#line 2898 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 11363 "Parser/parser.cc"
    break;

  case 731:
#line 2903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11369 "Parser/parser.cc"
    break;

  case 734:
#line 2910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 11375 "Parser/parser.cc"
    break;

  case 735:
#line 2915 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 11381 "Parser/parser.cc"
    break;

  case 736:
#line 2917 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 11387 "Parser/parser.cc"
    break;

  case 737:
#line 2924 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 11393 "Parser/parser.cc"
    break;

  case 739:
#line 2927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 11399 "Parser/parser.cc"
    break;

  case 740:
#line 2929 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)( (yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 11405 "Parser/parser.cc"
    break;

  case 741:
#line 2934 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 11411 "Parser/parser.cc"
    break;

  case 742:
#line 2936 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11417 "Parser/parser.cc"
    break;

  case 743:
#line 2938 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 11423 "Parser/parser.cc"
    break;

  case 744:
#line 2943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 11429 "Parser/parser.cc"
    break;

  case 745:
#line 2945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 11435 "Parser/parser.cc"
    break;

  case 746:
#line 2950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "10" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), 0 );
		}
#line 11444 "Parser/parser.cc"
    break;

  case 747:
#line 2955 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "11" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 11453 "Parser/parser.cc"
    break;

  case 748:
#line 2963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), 0 ); }
#line 11459 "Parser/parser.cc"
    break;

  case 749:
#line 2965 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-8].tok), (yyvsp[-6].decl), (yyvsp[-2].decl) ); }
#line 11465 "Parser/parser.cc"
    break;

  case 751:
#line 2971 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl) ); }
#line 11471 "Parser/parser.cc"
    break;

  case 756:
#line 2983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 11477 "Parser/parser.cc"
    break;

  case 757:
#line 2988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 11483 "Parser/parser.cc"
    break;

  case 758:
#line 2990 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 11489 "Parser/parser.cc"
    break;

  case 760:
#line 2998 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl);	}
#line 11495 "Parser/parser.cc"
    break;

  case 761:
#line 3003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11501 "Parser/parser.cc"
    break;

  case 762:
#line 3005 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->appendList( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 11507 "Parser/parser.cc"
    break;

  case 763:
#line 3010 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11513 "Parser/parser.cc"
    break;

  case 765:
#line 3015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 11519 "Parser/parser.cc"
    break;

  case 766:
#line 3019 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 11525 "Parser/parser.cc"
    break;

  case 767:
#line 3024 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( (yyvsp[0].tok) ) ) ); }
#line 11531 "Parser/parser.cc"
    break;

  case 769:
#line 3027 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, " declaration" ); (yyval.decl) = nullptr; }
#line 11537 "Parser/parser.cc"
    break;

  case 770:
#line 3029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.decl) = nullptr; }
#line 11543 "Parser/parser.cc"
    break;

  case 771:
#line 3031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.decl) = nullptr; }
#line 11549 "Parser/parser.cc"
    break;

  case 772:
#line 3033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 11555 "Parser/parser.cc"
    break;

  case 773:
#line 3035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 11561 "Parser/parser.cc"
    break;

  case 774:
#line 3037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 11567 "Parser/parser.cc"
    break;

  case 776:
#line 3040 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 11576 "Parser/parser.cc"
    break;

  case 777:
#line 3045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( false, (yyvsp[-2].constant), 0 ) ) ); }
#line 11582 "Parser/parser.cc"
    break;

  case 778:
#line 3047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = LinkageSpec::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 11591 "Parser/parser.cc"
    break;

  case 779:
#line 3052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-1].decl);
		}
#line 11601 "Parser/parser.cc"
    break;

  case 780:
#line 3058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = LinkageSpec::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 11610 "Parser/parser.cc"
    break;

  case 781:
#line 3063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11620 "Parser/parser.cc"
    break;

  case 782:
#line 3070 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 11629 "Parser/parser.cc"
    break;

  case 783:
#line 3075 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11639 "Parser/parser.cc"
    break;

  case 784:
#line 3081 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 11648 "Parser/parser.cc"
    break;

  case 785:
#line 3086 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11658 "Parser/parser.cc"
    break;

  case 786:
#line 3092 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.val) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.val) ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall) ) forall = true; // remember generic type
		}
#line 11667 "Parser/parser.cc"
    break;

  case 787:
#line 3097 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11677 "Parser/parser.cc"
    break;

  case 789:
#line 3112 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].sn) ); }
#line 11683 "Parser/parser.cc"
    break;

  case 790:
#line 3114 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].sn) ); }
#line 11689 "Parser/parser.cc"
    break;

  case 791:
#line 3119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; forall = false; }
#line 11695 "Parser/parser.cc"
    break;

  case 792:
#line 3121 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.en) = (yyvsp[-2].en); forall = false;
			if ( (yyvsp[0].decl) ) {
				SemanticError( yylloc, "Attributes cannot be associated with function body. Move attribute(s) before \"with\" clause." );
				(yyval.en) = nullptr;
			} // if
		}
#line 11707 "Parser/parser.cc"
    break;

  case 793:
#line 3132 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11718 "Parser/parser.cc"
    break;

  case 794:
#line 3139 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 11727 "Parser/parser.cc"
    break;

  case 795:
#line 3144 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 11736 "Parser/parser.cc"
    break;

  case 796:
#line 3150 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 11742 "Parser/parser.cc"
    break;

  case 797:
#line 3153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 11748 "Parser/parser.cc"
    break;

  case 798:
#line 3156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 11754 "Parser/parser.cc"
    break;

  case 799:
#line 3160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-4].decl) );
		}
#line 11763 "Parser/parser.cc"
    break;

  case 800:
#line 3166 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 11769 "Parser/parser.cc"
    break;

  case 801:
#line 3169 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 11775 "Parser/parser.cc"
    break;

  case 802:
#line 3172 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 11781 "Parser/parser.cc"
    break;

  case 806:
#line 3183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 11787 "Parser/parser.cc"
    break;

  case 807:
#line 3188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11793 "Parser/parser.cc"
    break;

  case 808:
#line 3190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = (yyvsp[-2].constant);
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 11803 "Parser/parser.cc"
    break;

  case 809:
#line 3199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11809 "Parser/parser.cc"
    break;

  case 812:
#line 3206 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11815 "Parser/parser.cc"
    break;

  case 813:
#line 3211 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 11821 "Parser/parser.cc"
    break;

  case 815:
#line 3217 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11827 "Parser/parser.cc"
    break;

  case 816:
#line 3222 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11833 "Parser/parser.cc"
    break;

  case 817:
#line 3224 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 11839 "Parser/parser.cc"
    break;

  case 818:
#line 3226 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 11845 "Parser/parser.cc"
    break;

  case 823:
#line 3235 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 11851 "Parser/parser.cc"
    break;

  case 824:
#line 3237 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 11857 "Parser/parser.cc"
    break;

  case 825:
#line 3272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11863 "Parser/parser.cc"
    break;

  case 826:
#line 3274 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11869 "Parser/parser.cc"
    break;

  case 827:
#line 3279 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11875 "Parser/parser.cc"
    break;

  case 829:
#line 3282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11881 "Parser/parser.cc"
    break;

  case 830:
#line 3284 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11887 "Parser/parser.cc"
    break;

  case 831:
#line 3289 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11893 "Parser/parser.cc"
    break;

  case 832:
#line 3291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11899 "Parser/parser.cc"
    break;

  case 833:
#line 3293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11905 "Parser/parser.cc"
    break;

  case 834:
#line 3295 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11911 "Parser/parser.cc"
    break;

  case 835:
#line 3300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11917 "Parser/parser.cc"
    break;

  case 836:
#line 3302 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11923 "Parser/parser.cc"
    break;

  case 837:
#line 3304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11929 "Parser/parser.cc"
    break;

  case 838:
#line 3306 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11935 "Parser/parser.cc"
    break;

  case 839:
#line 3308 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11941 "Parser/parser.cc"
    break;

  case 840:
#line 3310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11947 "Parser/parser.cc"
    break;

  case 841:
#line 3312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11953 "Parser/parser.cc"
    break;

  case 842:
#line 3317 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11959 "Parser/parser.cc"
    break;

  case 843:
#line 3319 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 11965 "Parser/parser.cc"
    break;

  case 844:
#line 3321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11971 "Parser/parser.cc"
    break;

  case 845:
#line 3323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11977 "Parser/parser.cc"
    break;

  case 846:
#line 3332 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11983 "Parser/parser.cc"
    break;

  case 848:
#line 3335 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11989 "Parser/parser.cc"
    break;

  case 849:
#line 3340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11995 "Parser/parser.cc"
    break;

  case 850:
#line 3342 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12001 "Parser/parser.cc"
    break;

  case 851:
#line 3344 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 12007 "Parser/parser.cc"
    break;

  case 852:
#line 3346 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12013 "Parser/parser.cc"
    break;

  case 853:
#line 3348 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12019 "Parser/parser.cc"
    break;

  case 854:
#line 3353 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12025 "Parser/parser.cc"
    break;

  case 855:
#line 3355 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12031 "Parser/parser.cc"
    break;

  case 856:
#line 3357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12037 "Parser/parser.cc"
    break;

  case 857:
#line 3359 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12043 "Parser/parser.cc"
    break;

  case 858:
#line 3364 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12049 "Parser/parser.cc"
    break;

  case 859:
#line 3366 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12055 "Parser/parser.cc"
    break;

  case 860:
#line 3368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12061 "Parser/parser.cc"
    break;

  case 861:
#line 3370 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12067 "Parser/parser.cc"
    break;

  case 862:
#line 3372 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12073 "Parser/parser.cc"
    break;

  case 863:
#line 3374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12079 "Parser/parser.cc"
    break;

  case 867:
#line 3392 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 12085 "Parser/parser.cc"
    break;

  case 868:
#line 3394 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12091 "Parser/parser.cc"
    break;

  case 869:
#line 3396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 12097 "Parser/parser.cc"
    break;

  case 870:
#line 3398 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12103 "Parser/parser.cc"
    break;

  case 871:
#line 3400 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12109 "Parser/parser.cc"
    break;

  case 872:
#line 3405 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12115 "Parser/parser.cc"
    break;

  case 873:
#line 3407 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12121 "Parser/parser.cc"
    break;

  case 874:
#line 3409 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12127 "Parser/parser.cc"
    break;

  case 875:
#line 3411 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12133 "Parser/parser.cc"
    break;

  case 876:
#line 3416 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12139 "Parser/parser.cc"
    break;

  case 877:
#line 3418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12145 "Parser/parser.cc"
    break;

  case 878:
#line 3420 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12151 "Parser/parser.cc"
    break;

  case 879:
#line 3422 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12157 "Parser/parser.cc"
    break;

  case 880:
#line 3424 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12163 "Parser/parser.cc"
    break;

  case 881:
#line 3426 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12169 "Parser/parser.cc"
    break;

  case 882:
#line 3441 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "ID" );
		}
#line 12178 "Parser/parser.cc"
    break;

  case 883:
#line 3446 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12184 "Parser/parser.cc"
    break;

  case 884:
#line 3451 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12190 "Parser/parser.cc"
    break;

  case 886:
#line 3454 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12196 "Parser/parser.cc"
    break;

  case 887:
#line 3456 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12202 "Parser/parser.cc"
    break;

  case 888:
#line 3461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12208 "Parser/parser.cc"
    break;

  case 889:
#line 3463 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12214 "Parser/parser.cc"
    break;

  case 890:
#line 3465 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12220 "Parser/parser.cc"
    break;

  case 891:
#line 3467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12226 "Parser/parser.cc"
    break;

  case 892:
#line 3472 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12232 "Parser/parser.cc"
    break;

  case 893:
#line 3474 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12238 "Parser/parser.cc"
    break;

  case 894:
#line 3476 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12244 "Parser/parser.cc"
    break;

  case 895:
#line 3478 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12250 "Parser/parser.cc"
    break;

  case 896:
#line 3480 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12256 "Parser/parser.cc"
    break;

  case 897:
#line 3482 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12262 "Parser/parser.cc"
    break;

  case 898:
#line 3484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12268 "Parser/parser.cc"
    break;

  case 899:
#line 3489 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12274 "Parser/parser.cc"
    break;

  case 900:
#line 3491 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12280 "Parser/parser.cc"
    break;

  case 901:
#line 3493 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 12286 "Parser/parser.cc"
    break;

  case 902:
#line 3495 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12292 "Parser/parser.cc"
    break;

  case 903:
#line 3497 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12298 "Parser/parser.cc"
    break;

  case 904:
#line 3507 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12304 "Parser/parser.cc"
    break;

  case 905:
#line 3509 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12310 "Parser/parser.cc"
    break;

  case 907:
#line 3512 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12316 "Parser/parser.cc"
    break;

  case 908:
#line 3514 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12322 "Parser/parser.cc"
    break;

  case 909:
#line 3519 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12328 "Parser/parser.cc"
    break;

  case 910:
#line 3521 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12334 "Parser/parser.cc"
    break;

  case 911:
#line 3523 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12340 "Parser/parser.cc"
    break;

  case 912:
#line 3528 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12346 "Parser/parser.cc"
    break;

  case 913:
#line 3530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12352 "Parser/parser.cc"
    break;

  case 914:
#line 3532 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12358 "Parser/parser.cc"
    break;

  case 915:
#line 3534 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12364 "Parser/parser.cc"
    break;

  case 916:
#line 3539 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12370 "Parser/parser.cc"
    break;

  case 917:
#line 3541 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12376 "Parser/parser.cc"
    break;

  case 918:
#line 3543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12382 "Parser/parser.cc"
    break;

  case 919:
#line 3557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12388 "Parser/parser.cc"
    break;

  case 920:
#line 3559 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12394 "Parser/parser.cc"
    break;

  case 922:
#line 3562 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12400 "Parser/parser.cc"
    break;

  case 923:
#line 3564 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12406 "Parser/parser.cc"
    break;

  case 924:
#line 3569 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12412 "Parser/parser.cc"
    break;

  case 925:
#line 3571 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12418 "Parser/parser.cc"
    break;

  case 926:
#line 3576 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12424 "Parser/parser.cc"
    break;

  case 927:
#line 3578 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12430 "Parser/parser.cc"
    break;

  case 928:
#line 3580 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12436 "Parser/parser.cc"
    break;

  case 929:
#line 3585 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12442 "Parser/parser.cc"
    break;

  case 930:
#line 3587 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12448 "Parser/parser.cc"
    break;

  case 931:
#line 3592 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12454 "Parser/parser.cc"
    break;

  case 932:
#line 3594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12460 "Parser/parser.cc"
    break;

  case 934:
#line 3612 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12466 "Parser/parser.cc"
    break;

  case 935:
#line 3614 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12472 "Parser/parser.cc"
    break;

  case 936:
#line 3619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( 0, (yyvsp[0].op) ); }
#line 12478 "Parser/parser.cc"
    break;

  case 937:
#line 3621 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 12484 "Parser/parser.cc"
    break;

  case 938:
#line 3623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12490 "Parser/parser.cc"
    break;

  case 939:
#line 3625 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12496 "Parser/parser.cc"
    break;

  case 940:
#line 3627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12502 "Parser/parser.cc"
    break;

  case 942:
#line 3633 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12508 "Parser/parser.cc"
    break;

  case 943:
#line 3635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12514 "Parser/parser.cc"
    break;

  case 944:
#line 3637 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12520 "Parser/parser.cc"
    break;

  case 945:
#line 3642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 12526 "Parser/parser.cc"
    break;

  case 946:
#line 3644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12532 "Parser/parser.cc"
    break;

  case 947:
#line 3646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12538 "Parser/parser.cc"
    break;

  case 948:
#line 3652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false ); }
#line 12544 "Parser/parser.cc"
    break;

  case 949:
#line 3654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false )->addArray( (yyvsp[0].decl) ); }
#line 12550 "Parser/parser.cc"
    break;

  case 950:
#line 3657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-4].en), 0, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].en), 0, false ) ); }
#line 12556 "Parser/parser.cc"
    break;

  case 951:
#line 3660 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Type array dimension is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 12562 "Parser/parser.cc"
    break;

  case 953:
#line 3666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 12568 "Parser/parser.cc"
    break;

  case 954:
#line 3668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 12574 "Parser/parser.cc"
    break;

  case 956:
#line 3671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 12580 "Parser/parser.cc"
    break;

  case 957:
#line 3673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 12586 "Parser/parser.cc"
    break;

  case 959:
#line 3679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LThan; }
#line 12592 "Parser/parser.cc"
    break;

  case 960:
#line 3681 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 12598 "Parser/parser.cc"
    break;

  case 961:
#line 3686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), 0, false ); }
#line 12604 "Parser/parser.cc"
    break;

  case 962:
#line 3688 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 12610 "Parser/parser.cc"
    break;

  case 963:
#line 3690 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newArray( (yyvsp[-2].en), 0, false ) ); }
#line 12616 "Parser/parser.cc"
    break;

  case 964:
#line 3692 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 12622 "Parser/parser.cc"
    break;

  case 965:
#line 3726 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12628 "Parser/parser.cc"
    break;

  case 968:
#line 3733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 12634 "Parser/parser.cc"
    break;

  case 969:
#line 3735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12640 "Parser/parser.cc"
    break;

  case 970:
#line 3737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12646 "Parser/parser.cc"
    break;

  case 971:
#line 3742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].op) ); }
#line 12652 "Parser/parser.cc"
    break;

  case 972:
#line 3744 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 12658 "Parser/parser.cc"
    break;

  case 973:
#line 3746 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12664 "Parser/parser.cc"
    break;

  case 974:
#line 3748 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12670 "Parser/parser.cc"
    break;

  case 975:
#line 3750 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12676 "Parser/parser.cc"
    break;

  case 977:
#line 3756 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12682 "Parser/parser.cc"
    break;

  case 978:
#line 3758 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12688 "Parser/parser.cc"
    break;

  case 979:
#line 3760 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12694 "Parser/parser.cc"
    break;

  case 980:
#line 3765 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 12700 "Parser/parser.cc"
    break;

  case 981:
#line 3767 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12706 "Parser/parser.cc"
    break;

  case 982:
#line 3769 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12712 "Parser/parser.cc"
    break;

  case 984:
#line 3776 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12718 "Parser/parser.cc"
    break;

  case 986:
#line 3787 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false ); }
#line 12724 "Parser/parser.cc"
    break;

  case 987:
#line 3790 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 12730 "Parser/parser.cc"
    break;

  case 988:
#line 3792 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, (yyvsp[-2].decl), false ); }
#line 12736 "Parser/parser.cc"
    break;

  case 989:
#line 3795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 12742 "Parser/parser.cc"
    break;

  case 990:
#line 3797 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 12748 "Parser/parser.cc"
    break;

  case 991:
#line 3799 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-4].decl), true ); }
#line 12754 "Parser/parser.cc"
    break;

  case 993:
#line 3814 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12760 "Parser/parser.cc"
    break;

  case 994:
#line 3816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12766 "Parser/parser.cc"
    break;

  case 995:
#line 3821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( 0, (yyvsp[0].op) ); }
#line 12772 "Parser/parser.cc"
    break;

  case 996:
#line 3823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 12778 "Parser/parser.cc"
    break;

  case 997:
#line 3825 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12784 "Parser/parser.cc"
    break;

  case 998:
#line 3827 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12790 "Parser/parser.cc"
    break;

  case 999:
#line 3829 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12796 "Parser/parser.cc"
    break;

  case 1001:
#line 3835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12802 "Parser/parser.cc"
    break;

  case 1002:
#line 3837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12808 "Parser/parser.cc"
    break;

  case 1003:
#line 3839 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12814 "Parser/parser.cc"
    break;

  case 1004:
#line 3844 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12820 "Parser/parser.cc"
    break;

  case 1005:
#line 3846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12826 "Parser/parser.cc"
    break;

  case 1008:
#line 3856 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12832 "Parser/parser.cc"
    break;

  case 1011:
#line 3867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12838 "Parser/parser.cc"
    break;

  case 1012:
#line 3869 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12844 "Parser/parser.cc"
    break;

  case 1013:
#line 3871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12850 "Parser/parser.cc"
    break;

  case 1014:
#line 3873 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12856 "Parser/parser.cc"
    break;

  case 1015:
#line 3875 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12862 "Parser/parser.cc"
    break;

  case 1016:
#line 3877 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12868 "Parser/parser.cc"
    break;

  case 1017:
#line 3884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 12874 "Parser/parser.cc"
    break;

  case 1018:
#line 3886 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12880 "Parser/parser.cc"
    break;

  case 1019:
#line 3888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 12886 "Parser/parser.cc"
    break;

  case 1020:
#line 3890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 12892 "Parser/parser.cc"
    break;

  case 1021:
#line 3892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12898 "Parser/parser.cc"
    break;

  case 1022:
#line 3895 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 12904 "Parser/parser.cc"
    break;

  case 1023:
#line 3897 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12910 "Parser/parser.cc"
    break;

  case 1024:
#line 3899 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 12916 "Parser/parser.cc"
    break;

  case 1025:
#line 3901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 12922 "Parser/parser.cc"
    break;

  case 1026:
#line 3903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12928 "Parser/parser.cc"
    break;

  case 1027:
#line 3908 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 12934 "Parser/parser.cc"
    break;

  case 1028:
#line 3910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 12940 "Parser/parser.cc"
    break;

  case 1029:
#line 3915 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 12946 "Parser/parser.cc"
    break;

  case 1030:
#line 3917 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl)->addQualifiers( (yyvsp[-4].decl) ), true ); }
#line 12952 "Parser/parser.cc"
    break;

  case 1032:
#line 3944 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12958 "Parser/parser.cc"
    break;

  case 1036:
#line 3955 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12964 "Parser/parser.cc"
    break;

  case 1037:
#line 3957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12970 "Parser/parser.cc"
    break;

  case 1038:
#line 3959 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12976 "Parser/parser.cc"
    break;

  case 1039:
#line 3961 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12982 "Parser/parser.cc"
    break;

  case 1040:
#line 3963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12988 "Parser/parser.cc"
    break;

  case 1041:
#line 3965 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12994 "Parser/parser.cc"
    break;

  case 1042:
#line 3972 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13000 "Parser/parser.cc"
    break;

  case 1043:
#line 3974 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13006 "Parser/parser.cc"
    break;

  case 1044:
#line 3976 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13012 "Parser/parser.cc"
    break;

  case 1045:
#line 3978 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13018 "Parser/parser.cc"
    break;

  case 1046:
#line 3980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 13024 "Parser/parser.cc"
    break;

  case 1047:
#line 3982 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 13030 "Parser/parser.cc"
    break;

  case 1048:
#line 3987 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 13036 "Parser/parser.cc"
    break;

  case 1049:
#line 3989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 13042 "Parser/parser.cc"
    break;

  case 1050:
#line 3991 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 13048 "Parser/parser.cc"
    break;

  case 1051:
#line 3998 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 13054 "Parser/parser.cc"
    break;

  case 1052:
#line 4000 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 13060 "Parser/parser.cc"
    break;

  case 1055:
#line 4024 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 13066 "Parser/parser.cc"
    break;

  case 1056:
#line 4026 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 13072 "Parser/parser.cc"
    break;


#line 13076 "Parser/parser.cc"

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
#line 4029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "make install" //
// End: //
