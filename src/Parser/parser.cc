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
#define YYLAST   22902

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  177
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  294
/* YYNRULES -- Number of rules.  */
#define YYNRULES  1047
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  2118

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
    3655,  3658,  3662,  3664,  3666,  3668,  3703,  3704,  3708,  3709,
    3711,  3713,  3718,  3720,  3722,  3724,  3726,  3731,  3732,  3734,
    3736,  3741,  3743,  3745,  3751,  3752,  3754,  3763,  3766,  3768,
    3771,  3773,  3775,  3789,  3790,  3792,  3797,  3799,  3801,  3803,
    3805,  3810,  3811,  3813,  3815,  3820,  3822,  3830,  3831,  3832,
    3837,  3838,  3843,  3845,  3847,  3849,  3851,  3853,  3860,  3862,
    3864,  3866,  3868,  3871,  3873,  3875,  3877,  3879,  3884,  3886,
    3888,  3893,  3919,  3920,  3922,  3926,  3927,  3931,  3933,  3935,
    3937,  3939,  3941,  3948,  3950,  3952,  3954,  3956,  3958,  3963,
    3965,  3967,  3974,  3976,  3994,  3996,  4001,  4002
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

#define YYPACT_NINF (-1772)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1046)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     208, 12540,   271,   312, 17295,   130, -1772, -1772, -1772, -1772,
   -1772, -1772, -1772, -1772, -1772, -1772, -1772, -1772,   192,   881,
     309, -1772, -1772, -1772, -1772, -1772, -1772, -1772, -1772, -1772,
   -1772, -1772, -1772, -1772, -1772, -1772, -1772, -1772, -1772, -1772,
   -1772, -1772, -1772, -1772, -1772, -1772, -1772, -1772,   126,   466,
   -1772, -1772, -1772, -1772, -1772, -1772,  3575,  3575,   395, 12540,
     408,   420, 22633, -1772,   441, -1772, -1772, -1772, -1772, -1772,
   -1772, -1772, -1772, -1772,  2873, -1772,   510,   496, -1772, -1772,
   -1772, -1772, -1772, 17144, -1772, -1772,   485,   527,   322,   217,
   -1772,  3575,   527,   527,   527,   545,  4165,   742,   781, 12701,
   -1772, -1772, -1772, 16993,  2616, -1772, -1772, -1772,  2278,   748,
    7074,   849,  1014,  2278,  1216,   608, -1772, -1772, -1772, -1772,
     717, -1772, -1772, -1772, -1772,   683, -1772, -1772, -1772, -1772,
   -1772,   699,   704,   717, -1772,   717,   710, -1772, -1772, -1772,
   18299,  3575, -1772, -1772,  3575, -1772, 12540, -1772,   696, 18450,
   -1772, -1772,  4181, 19862, -1772,  1237,  1237,   759,  2163, -1772,
   -1772, -1772, -1772,   456, 14601,  2729,   717, -1772, -1772, -1772,
   -1772, -1772, -1772,   733, -1772,   747,   739,   783, -1772,   820,
   22031, -1772, -1772, -1772, -1772, -1772, -1772, -1772, 15917,  2908,
    2873,   270,   793,   798,   808,   822,   836,   845, -1772, -1772,
   18601, 11557,   818, -1772, 17739, -1772, -1772, -1772, -1772,   856,
   -1772, -1772,   802, -1772,  9910,   944,  9008, -1772,   859,  3575,
     704,   861,   858,   864,   903, -1772, -1772, -1772,  3619,  3545,
     906,   968,   100, -1772, -1772,   717,   717,    61,    78,   383,
      61, -1772,   717,   717, -1772,  3592, -1772, -1772,   917,   931,
    1237, 20057, -1772, 17144, -1772, -1772,  2278, -1772,  1661,   608,
     934,  1016,    78,  3575,   322, -1772, 14121, -1772,  1237,  1237,
     949,  1016,    78,  3575, -1772, 22779, -1772, -1772,  1237, -1772,
    1237, -1772,   669,  3439,  3575, -1772,  1457,   971, -1772, -1772,
   -1772, 16807,   704,    97, -1772, -1772, 20006, -1772,   968,   275,
   -1772, 22031, 19862,  3798,  3592, -1772,   386, -1772, -1772, -1772,
   18450,  3575, -1772,   959, -1772, -1772, -1772, -1772,  3575,  2797,
     523,   -61, -1772,  3575,   747, -1772,    31,   717,   717,   974,
   18752,   731, 15081, 20259,  2278,  2278, -1772,  2278,  1237,  2278,
    1237, -1772, -1772,   717, -1772,   979, -1772, 18903, -1772, -1772,
   -1772, 19054,   856, -1772,   389,   343,   214,   628,   608,   969,
   -1772,  2163,   976,   747,  2163,  1673, -1772,  1006,  1054, 22104,
    1025,  1036,  1041, 22031, 22177,  1048, 22684, -1772, -1772, -1772,
   -1772, -1772, -1772, 22250, 22250, 15762,  1044,  2992, -1772, -1772,
   -1772, -1772,   329, -1772,   544, -1772,   947, -1772, 22031, 22031,
   -1772,  1039,   604,   643,   852,   499,   866,  1050,  1060,  1042,
    1092,   223, -1772,   548, -1772,  1102, -1772,   876,  4373, 16227,
   -1772, -1772,   761,  1102, -1772, -1772,   724, -1772, -1772,  2908,
    1105,  1108,  1111,  1118,  1132,  1134, -1772, -1772,   399,  1149,
   -1772,   734,  1149, -1772, -1772, 18299, -1772,   943,  1154, 16382,
   -1772, -1772,  3389,  3309,  1189, 15081,  1198,   678,   755, -1772,
   -1772, -1772, -1772, -1772,  3575,  4327, -1772, -1772, -1772, -1772,
   -1772, -1772, 16701,  3607,  1044,  9910,  1206,  1213, -1772, -1772,
    1190,  9008,   743, -1772, -1772, -1772, 20498,  1231, -1772, -1772,
   -1772, -1772, -1772,  3619,   837,  1240,  1242,  1249,   853,  1251,
    1259,  1271,  3545, -1772, -1772,   717,  1233,   322,  1241, -1772,
   -1772,  1276, -1772, -1772,   704,  1016, -1772, -1772, -1772,   704,
   -1772, -1772,  3592, -1772, 16227, 16227, -1772,  1237,  4181,  4965,
   15241, -1772, -1772, -1772, -1772, -1772,   704,  1016,   275, -1772,
   -1772,  2278,  1247,  1016,    78, -1772,   704,  1016, -1772, 22830,
   -1772,  1237,  1237, -1772, -1772,  1289,   470,  1305,   608,  1309,
   -1772, 17455, -1772,   751, -1772,  1385, 20157, -1772,  4181, 16895,
   20057, -1772, 16807, 22323, -1772, -1772, -1772, -1772, -1772,  3798,
     865,  3592, -1772, 15241,   968, 12540, -1772,  1315, -1772,  1329,
   -1772, -1772, -1772, -1772, -1772,  2163, -1772, -1772,  1405,  4076,
    3175, 19054, 11557, -1772, 19205, -1772,  1237,  1237, -1772, -1772,
     856, -1772,  1034,  1331,  1467, 22031,   941,  1276,  1323, -1772,
     717,   717, -1772,  1149, -1772, 18752, -1772, -1772, 17899,  1237,
    1237, -1772,  4076,   717, -1772, 19718, -1772, -1772, 18903, -1772,
     456, -1772, -1772, -1772,  1349,  3575,   969,  1351,   754, 18450,
     764, -1772, -1772, -1772, -1772, -1772, -1772,   774, -1772,  1361,
    1356, -1772, 16072, -1772,  2992, 19356, 19356, -1772, 16072, -1772,
   22031, -1772, -1772, -1772, -1772, -1772, -1772, 16072, -1772, -1772,
   17997, 19356, 19356,   876,  1097,  1360,   583,  1749, -1772,   779,
    1375,   877,  1382, -1772, 20498, 22031, 20571,  1381, 22031,  1457,
   22031,  1457, -1772,  2261, -1772, -1772, 20644,  2447, 22031, 20644,
    1457, -1772, -1772, 22031, 22031, 22031, 22031, 22031, 22031, 22031,
   22031, 22031, 22031, 22031, 22031, 22031, 22031, 22031, 22031, 22031,
   22031, 22031,  9489,  1370,   820,  3697, 11557, -1772, -1772, -1772,
   -1772, -1772, -1772, -1772, -1772, -1772, -1772, -1772,  1394, 22031,
   -1772, -1772,   761,  1139, -1772, -1772,   717,   717, -1772, -1772,
   16227, -1772,   401,  1149, -1772,   809,  1149, -1772, -1772, -1772,
    1276, -1772, -1772,  1276, 22396, -1772, -1772, 11557,  1401,  1403,
    3079,  1545,  2596,   464,  1323, -1772,   717,   717,  1323,   511,
   -1772,   717,   717, 22031,  3575,   989,  1112,  1323,   -99, 14441,
   14441,  3575, -1772, -1772, 22031,  1190, -1772,  9910,  1424, -1772,
    1595, -1772, -1772, -1772, -1772, -1772,   815, -1772, 14441,  1457,
    4181,  1457,   846,  1422,  1423,  1433,   883,  1434,  1435,  1436,
     528,  1149, -1772, -1772,   568,  1149, -1772, -1772, -1772,  4181,
     820, -1772,  1149, 22396, -1772,   704, 17455, -1772, -1772,   904,
    1439,   920,  1441, -1772,  1427, -1772,   704, -1772, -1772,   704,
    1016,  1427, -1772,   704,  1438,  1444,  1445, -1772, -1772, 17899,
   -1772,  1447, -1772, -1772, -1772,  1457,  3575, 10706,  1528,  1429,
   19516, -1772,  1154, -1772, 14441,   924, -1772, -1772,  1427, -1772,
   18450, 16227,  1432, -1772,  1432, -1772, -1772, -1772,   214,   717,
     717, -1772, 18903, -1772, 11721, 16537, -1772, 17455,  1458,  1459,
    1460, -1772,  9393,   717, -1772,   941, -1772, -1772, -1772, -1772,
    1276, -1772, -1772, -1772,  1237, -1772,  3210, -1772, -1772,   608,
     379,  1464,  1442,  1451,   214, -1772, -1772,  1461,  1463,  1673,
   20644, -1772,  1466,  1469,   496,  1470,  1468,  1477,  1475,  1482,
   22031,  1483,  1485,  1486, 11557, 22031, -1772, -1772,  1888, -1772,
   -1772, -1772, 22031, -1772,  1487,  1489, 20352,  1117, -1772, 20644,
    1490, -1772,  1491, -1772, -1772,  2426, -1772, -1772,   923, -1772,
   -1772, -1772, -1772,  2426, -1772, -1772,  1121,   680, -1772, -1772,
    1039,  1039,  1039,   604,   604,   643,   643,   852,   852,   852,
     852,   499,   499,   866,  1050,  1060,  1042,  1092, 22031,  1138,
   -1772,  1492,  2426, -1772, -1772,  9910, -1772, 17455,  1495,  1498,
    1500,  1139, -1772, -1772, -1772, -1772, -1772, -1772, -1772, -1772,
    1276, -1772, -1772,  1276, 17455, 17455, -1772, -1772,  3079,   900,
    1506,  1508,  1509,  1510,  2584,  2596, -1772, -1772, -1772, -1772,
   -1772, -1772, -1772, -1772, -1772, -1772, -1772, -1772, -1772, -1772,
    1514, -1772,  1323, -1772, -1772, -1772, -1772, -1772, -1772, -1772,
   -1772,  1512,  1519, -1772,   322,  2426,  1151,   274, -1772, -1772,
    1473, -1772,  9008, -1772, 22031,   717, 20717, 14441, -1772, -1772,
   -1772,  1499,   573,  1149, -1772,   609,  1149, -1772, -1772, -1772,
   -1772,  1276, -1772, -1772, -1772,  1276,   968,  1524,  1276, -1772,
   -1772, -1772, -1772, -1772, -1772, -1772,  1530, -1772, -1772,  1427,
   -1772,   704, -1772, -1772, -1772, -1772, -1772, 13338,  1529,  1526,
   -1772,    55, -1772,   418,   388, 11393,  1533, 15598,  1536,  1538,
    2311,  2483,  2706, 20790,  1539, -1772, -1772,  1540,  1542, -1772,
   -1772,   704, 22031, 22031,  1667,  1537,   690, -1772,  1620,  1543,
    1521, -1772, -1772, -1772, 10532, -1772, -1772, -1772, -1772, -1772,
    1308, -1772, -1772, -1772,  1608, -1772, -1772, -1772,  1457, -1772,
   -1772, 13184, 17144,  1553, -1772,  3575, -1772,  1525,  1559,  1560,
   -1772,  1183, -1772, -1772, -1772, -1772,  4181, -1772, -1772,  1551,
    1554,   930, 18450,   747,   747,  1349,   969,   969, -1772, -1772,
    1044,  1154, 16382, -1772,  1102, -1772, 11885, -1772,   640,  1149,
   -1772,  1237,  9613, -1772, -1772,   214,   717,   717,   456,  3575,
   -1772, 20863, -1772,   214,  1349,  1579, -1772, -1772,   938,   692,
   17899, 11557,  1457, -1772,   692, 18148,   692, -1772, 22031, 22031,
   22031, -1772, -1772, -1772, -1772, 22031, 22031,  1572,  9910, -1772,
   -1772,  1576,   716, -1772, -1772, -1772,  2054, -1772, -1772,  1186,
   -1772,   103, -1772, 20644,  1205, -1772, 20498, -1772, -1772, 22031,
    1561,  1212,  1244,  1190, -1772,   648,  1149, -1772, -1772, 17455,
   17455, -1772, -1772,  1582,   661,  1149, -1772,   691,  2631,   717,
     717, -1772, -1772, 17455, 17455, -1772,  1583, -1772, 15241, 15241,
    1585,  1584,  1586,  1589, -1772,  1587, 22031, 22031,  1253,  1592,
   -1772, -1772, -1772, -1772, -1772, -1772, -1772,  1596, 22031, -1772,
   -1772, -1772,  1276, -1772, -1772, -1772,  1276, 17455, 17455,   322,
     717,  1270,  1599,  1609, -1772, -1772,  1621, 13492, 13646, 13800,
   18450, 19356, 19356,  1594, -1772,  1601,  1602,  2297,  9839, -1772,
     212,  3575, -1772, -1772,  3575, -1772, 20425,   340,   514, -1772,
   -1772, -1772, -1772, 22031,  1607,  1692, 11228, 10880, -1772,  1612,
   -1772,  1613, 22031,  1615,  9910,  1617, 22031, 20498, 22031,  1347,
   -1772,  1619,   145, -1772,     9,  1624, -1772, -1772,  1646, -1772,
    1623, -1772,  1634,  1649, 15598,   436, 14281,   717,   241, -1772,
   -1772, -1772,  1644, -1772,  1664, -1772,  1665, -1772,  1659, -1772,
    1660, -1772, -1772, -1772, -1772,  1670,  1663,  1668, 12049,  1674,
    1676,  1678, -1772,  1684, -1772, -1772, -1772,  1276, 22031, 22031,
    1154,  1682, -1772,  1349, -1772,   969,   187,  1442,  9910, -1772,
    1349,  1689, -1772, 18450, -1772,  1085,  1688,  1686,   966, -1772,
    1690, -1772, -1772, -1772, -1772, -1772,  9910,  1190, 20498, -1772,
    1706,  2426, -1772,  1706,  1706, -1772,  2426,  3719,  4295, -1772,
   -1772,  1280, -1772, -1772, -1772,  1699,  1697, -1772, -1772, -1772,
    1276, -1772, -1772,  1700,  1701,   717, -1772, -1772, -1772,  1276,
   -1772, -1772, -1772,  1711, -1772, -1772, -1772, -1772, -1772, -1772,
   -1772, -1772, -1772, -1772, -1772, -1772, -1772,  1696, -1772, -1772,
   -1772, -1772,  1712,  1714,   717, -1772, 17455, 17455, -1772, -1772,
   -1772, -1772, 22031, -1772, -1772,  1719, -1772,  1594,  1594,  1594,
    1129,  1666,   253, -1772,  4003,   289, 16227, -1772, -1772, -1772,
    3969, 22031,  3870,   299, -1772, -1772,    98,  1713,  1713,  3575,
   -1772, -1772, 17606, -1772, 22031,  1715,  1720, -1772, -1772, -1772,
   -1772,   980,  1724, 15598,  1543,  1723, 22031,   485,  1721,   545,
   13961, 18450, -1772, -1772, -1772,   982, 15598, 22031,  1136,   601,
   -1772, 22031,  6744, -1772, -1772,   354, -1772,  1190, -1772,  1007,
    1009,  1021, -1772, -1772, -1772, -1772,   704,  1347,  1726, -1772,
   -1772, 22031, -1772,  1728,   820, 11393, -1772, -1772, -1772, -1772,
   22031,  1772, -1772, 10340, -1772,   717, 15241, -1772, -1772, 18450,
   -1772, -1772, -1772,   214,   214, -1772, -1772, -1772,  1729, -1772,
   17455, -1772, -1772,  1731, -1772,  1732,  1734,   969,  1730, -1772,
   -1772,  1190,  1740, -1772, -1772,  1738, -1772, -1772, 22031, -1772,
   18148, 22031,  1190,  1743,  1288, -1772,  1295, -1772,  2426, -1772,
    2426, -1772, -1772, -1772, -1772, 17455,  1742,  1744, -1772, -1772,
   17455, 17455,  1745,  1746,  1310, 14761, 14921, -1772,  1751, -1772,
   -1772, -1772, -1772,  1756,  1757,  1322, -1772, -1772, -1772, -1772,
    1129,  1374,   382, -1772, -1772, -1772, -1772,   717,   717, -1772,
   -1772, -1772,   463, -1772,  1037,  3969,   577, -1772,  3870,   717,
   -1772, -1772, -1772, -1772, -1772, -1772, -1772, -1772,   500, 15598,
     186,  8051,  1807, 15598,  1543, 15401, -1772, -1772, -1772, -1772,
   22031, -1772, 20936,  1836,  1736, 20276, 21009, 15598, 11054,  1543,
     676,  1296,  1737, 22031, -1772,  1766,   356, 15598, -1772, -1772,
    1770, -1772, -1772,  1741,   820,   639,  1773,  1774,  1336,  1833,
   -1772, -1772, -1772, -1772,  3575,  4181,  1349,  1349, -1772, -1772,
    1780,  1781, -1772, -1772, -1772,  1771,   214,  1779, -1772,  1790,
   -1772, -1772, -1772, -1772,  1791, -1772, -1772, -1772,  1345,  1352,
   -1772, -1772, -1772, -1772, -1772, -1772, -1772, -1772, -1772, -1772,
    1792, -1772, -1772,  1795,  1796, -1772, -1772, -1772,  1797,  1799,
    1803,  1374, -1772,   717, -1772, -1772, -1772, -1772, -1772,  1804,
    4003, -1772, -1772,  3413,    77, 12216, -1772, 15494, -1772,    30,
    1047, 15598,  1886,   550,  1800,   376, 15598, 22031,  1809,   676,
    1296,  1788, 22469,  1802,   422,  1891, -1772, 21082, 21155, 22031,
    1543,  1798, 12379, -1772, -1772, -1772, 19567, -1772,  1813,  1801,
     235, 15598, -1772, 22031, 20644,   506, -1772, -1772, -1772,  1823,
    1826,  1825, -1772, -1772,   214,  1349, -1772, -1772, -1772, -1772,
   -1772,  1828,  1831,  1834, 15241,  1822, -1772, -1772,   713,  1149,
   -1772, -1772,  1129, -1772, -1772,   286, -1772,   225, -1772, -1772,
   -1772,  1838, 12862, -1772, -1772, 15598, -1772,    67, -1772, 15598,
   22031,  1837, 21228, -1772, -1772, 21301, 21374, 22031,  1809,  1543,
   21447, 21520, 15598,  1819,   492,  1827,   530, -1772, -1772,  1845,
   12862, 19567, -1772,  4098, 19205,  1457,  1839, -1772,  1897,  1850,
     712,  1847, -1772,  1930, -1772,  1053, 15598,  1855, 15598, 15598,
   -1772, -1772, -1772,  1349,  1861, -1772, -1772, -1772, -1772, -1772,
   -1772, -1772,  1276, -1772, 22031, -1772, 22031, -1772, -1772,  1437,
   13023, -1772, -1772, 15598, -1772, -1772,  1543, -1772, -1772,  1543,
    1849,   582,  1852,   621, -1772, -1772,  1543, -1772,  1543, -1772,
    1860, 21593, 21666, 21739, -1772,  1437, -1772,  1846,  3045,  3148,
   -1772, -1772, -1772,   235,  1858, 22031,  1848,   235,   235, 15598,
   -1772, -1772, 22031,  1912,  1914,  1875, -1772, 17455, -1772, -1772,
   15494, -1772,  1437, -1772, -1772,  1874, 21812, 21885, 21958, -1772,
   -1772,  1543, -1772,  1543, -1772,  1543, -1772,  1846, 22031,  1878,
    3148,  1876,   820,  1879, -1772,   725, -1772, -1772,  1061,  1833,
     311, -1772, -1772, -1772, 10077,  1885, 15494, -1772, -1772,  1543,
   -1772,  1543, -1772,  1543,  1887,  1889, -1772,   704,   820,  1890,
   -1772,  1863,   820, -1772, -1772, 15598,  1965,  1892, -1772, -1772,
   -1772, 10215, -1772,   704, -1772, -1772,  1364, 22031, -1772,  1086,
   -1772, 15598, -1772, -1772,   820,  1457,  1893,  1868, -1772, -1772,
   -1772,  1089, -1772, -1772,  1871,  1457, -1772, -1772
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
     457,     0,  1025,  1026,     0,     1,   457,    16,     0,   457,
     429,   430,     0,   538,   451,   452,   453,   778,     0,   587,
     589,   591,   593,     0,   457,     0,   810,   811,   583,   512,
     686,   687,   685,   746,   741,   731,     0,     0,   776,     0,
       0,   474,   769,   773,   774,   770,   771,   772,   457,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   556,   559,
     457,   457,     0,  1027,   538,   854,   872,  1031,  1024,  1022,
    1029,   405,     0,   164,   699,   163,     0,   414,     0,     0,
       0,     0,     0,     0,     0,   404,   924,   925,     0,     0,
     439,   807,   809,   803,   828,   809,   809,   805,     2,   809,
     804,   885,   809,   809,   882,     0,   531,   532,     0,     0,
     457,   457,     2,   457,   421,   460,   470,   524,     0,   553,
       0,   791,     2,     0,   693,   422,   538,   517,   534,   549,
       0,   791,     2,     0,   473,   518,   525,   526,   535,   540,
     550,   554,     0,   568,     0,   761,     2,     2,   789,   846,
     848,   457,     0,     2,     2,  1035,   538,  1038,   807,   807,
       3,     0,   538,     0,     0,   432,   809,   805,   804,     2,
     457,     0,   765,     0,   727,   729,   728,   730,     0,     0,
     723,     0,   713,     0,   722,   733,     0,   809,   809,     2,
     457,  1046,   458,   457,   469,   448,   516,   449,   541,   450,
     548,   545,   566,   809,   567,     0,   674,   457,   675,  1000,
    1001,   457,   676,   678,   555,   561,   635,   637,   638,   635,
     812,     0,   744,   732,     0,   816,    21,     0,    20,     0,
       0,     0,     0,     0,     0,     0,    23,    25,     4,     8,
       5,     6,     7,     0,     0,   457,     2,     0,   103,   104,
     105,   106,    87,    24,    88,    42,    86,   107,     0,     0,
     122,   124,   128,   131,   134,   139,   142,   144,   146,   148,
     150,   152,   155,     0,    26,     0,   562,     2,   107,   457,
     156,   738,   689,   552,   691,   737,     0,   688,   692,     0,
       0,     0,     0,     0,     0,     0,   826,   852,   809,   862,
     870,   874,   880,     2,  1033,   457,  1036,     2,   100,   457,
       3,   673,     0,  1046,     0,   458,   516,   541,   548,     3,
       3,   655,   659,   669,   675,   676,     2,   855,   873,  1023,
       2,     2,    23,     0,     2,   699,    24,     0,   697,   700,
    1044,     0,     0,   706,   695,   694,     0,     0,   793,     2,
       2,     2,     2,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   831,   888,   809,     0,   693,     2,   827,
     835,   951,   829,   830,     0,   791,     2,   884,   892,     0,
     886,   887,     0,   435,   457,   457,   522,   458,     0,   538,
     457,  1028,  1032,  1030,   539,   765,     0,   791,   807,   415,
     423,   471,     0,   791,     2,   765,     0,   791,   742,   519,
     520,   536,   551,   557,   560,   555,   561,   579,   580,     0,
     743,   457,   683,     0,   201,   398,   457,     3,     0,   538,
     457,   790,   457,     0,   417,     2,   418,   762,   437,     0,
       0,     0,     2,   457,   807,   457,   765,     0,     2,     0,
     726,   725,   724,   719,   468,     0,   717,   734,   514,     0,
       0,   457,   457,  1002,   458,   454,   455,   456,  1006,   997,
     998,  1004,     2,     2,   101,     0,   962,   976,  1046,   958,
     809,   809,   967,   974,   681,   457,   546,   677,   458,   542,
     543,   547,     0,   809,  1012,   458,  1017,  1009,   457,  1014,
       0,   644,   636,   643,  1044,     0,   635,     0,     0,   457,
       0,   824,   823,   819,   821,   822,   820,     0,   814,   817,
       0,    22,   457,    94,     0,   457,   457,    89,   457,    96,
       0,    32,    36,    37,    33,    34,    35,   457,    92,    93,
     457,   457,   457,     2,   103,   104,     0,     0,   182,     0,
       0,   582,     0,  1022,     0,     0,     0,     0,     0,     0,
       0,     0,    55,     0,    61,    62,    66,     0,     0,    66,
       0,    90,    91,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   457,   165,   166,   167,
     168,   169,   170,   171,   172,   173,   174,   175,   163,     0,
     161,   162,     2,   936,   690,   933,   809,   809,   941,   563,
     457,   853,   809,   863,   871,   875,   881,     2,   856,   858,
     860,     2,   876,   878,     0,  1034,  1037,   457,     0,     0,
       2,   101,   962,   809,  1046,   906,   809,   809,  1046,   809,
     921,   809,   809,     3,   677,     0,     0,  1046,  1046,   457,
     457,     0,     2,   708,     0,  1044,   705,  1045,     0,   701,
       0,     2,   704,   707,   179,   178,     0,     2,   457,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     809,   840,   844,   883,   809,   897,   902,   832,   889,     0,
       0,   443,   948,     0,   794,     0,   457,   795,   436,     0,
       0,     0,     0,   434,     2,   796,     0,   419,   765,     0,
     791,     2,   797,     0,     0,     0,     0,   594,   662,   458,
       3,     3,   666,   665,   867,     0,     0,   457,   399,     0,
     538,     3,   100,     3,   457,     0,     3,   766,     2,   721,
     457,   457,   715,   714,   715,   515,   513,   637,   635,   809,
     809,  1008,   457,  1013,   458,   457,   999,   457,     0,     0,
       0,   977,     0,   809,  1047,   963,   964,   682,   960,   961,
     975,  1003,  1007,  1005,   544,   579,     0,  1011,  1016,   640,
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
     861,     2,   877,   879,   457,   457,   953,   952,     2,     0,
       0,     0,     0,     0,   809,   963,   909,   926,     2,   904,
     912,   679,   907,   908,   680,     2,   919,   929,   922,   923,
       0,     3,  1046,   427,     2,  1039,     2,   670,   671,   649,
       3,     3,     3,     3,   693,     0,   155,     0,     3,     3,
       0,   702,     0,   696,     0,   809,     0,   457,     3,   431,
     433,     0,   809,   841,   845,   809,   898,   903,     2,   833,
     836,   838,     2,   890,   893,   895,   807,     0,   949,     3,
     799,     3,   528,   527,   530,   529,     2,   766,   800,     2,
     798,     0,   766,   801,   594,   594,   594,   457,     0,     0,
     684,     0,   402,     0,     0,   457,     0,     2,     0,     0,
       0,     0,     0,   184,     0,   332,   333,     0,     0,   371,
     370,     0,   159,   159,   377,   555,   561,   198,     0,   185,
       0,   209,   186,   187,   457,   203,   188,   189,   190,   191,
       0,   192,   193,   338,     0,   194,   195,   196,     0,   197,
     205,   538,   457,     0,   207,     0,   396,     0,     0,     0,
       3,     0,   779,   766,   754,   755,     0,     3,   750,     3,
       3,     0,   457,   731,   731,  1044,   635,   635,  1010,  1015,
       2,   100,   457,     3,   553,     3,   458,     3,   809,   970,
     973,   457,     3,   959,   965,   635,   809,   809,     0,     0,
     623,     0,   639,   635,  1044,     2,   813,   815,     0,    95,
     457,   457,     0,    99,    97,   457,     0,   111,     0,     0,
       0,   115,   119,   118,   183,     0,     0,     0,   699,   108,
     176,     0,     0,    45,    46,    84,     0,    84,    84,     0,
      72,    74,    48,     0,     0,    44,     0,    47,   154,     0,
       0,     0,     0,  1044,     3,   809,   944,   947,   939,   457,
     457,     3,     3,     0,   809,   915,   918,   809,     0,   809,
     809,   910,   927,   457,   457,  1040,     0,   672,   457,   457,
       0,     0,     0,     0,   416,     3,     0,     0,     0,     0,
     698,   703,     3,   792,   181,   180,     3,     0,     0,     2,
     834,   837,   839,     2,   891,   894,   896,   457,   457,   693,
     809,     0,     0,     0,   766,   802,     0,   457,   457,   457,
     457,   457,   457,   577,   605,     3,     3,   606,   538,   595,
       0,     0,   849,     2,     0,   400,    66,     0,     0,   323,
     324,   206,   208,     0,     0,     0,   457,   457,   319,     0,
     317,     0,     0,     0,   699,     0,     0,     0,     0,     0,
     160,     0,     0,   378,     0,     0,     3,   213,     0,   204,
       0,   314,     0,     0,     2,     0,   538,   809,     0,   397,
     955,   954,     0,     2,     0,   757,     2,   752,     0,   753,
       0,   735,   716,   720,   718,     0,     0,     0,   457,     0,
       0,     0,     3,     0,     2,   966,   968,   969,     0,     0,
     100,     0,     3,  1044,   629,   635,   645,   645,   699,   646,
    1044,     0,   748,   457,   818,   956,     0,     0,     0,    38,
       0,   112,   114,   113,   110,   109,   699,  1044,     0,    65,
      81,     0,    75,    82,    83,    60,     0,     0,     0,    69,
      56,     0,   153,   407,    30,     0,     0,     2,   940,   942,
     943,     3,     3,     0,     0,   809,     2,   911,   913,   914,
       2,   928,   930,     0,   905,   920,     3,     3,  1041,     3,
     657,   656,   660,  1043,     2,     2,  1042,     0,     3,   806,
     709,   710,     0,     0,   809,   438,   457,   457,     3,     3,
     444,   808,     0,   899,   783,     0,   785,   577,   577,   577,
     612,   582,     0,   618,   606,     0,   457,   569,   604,   600,
       0,     0,     0,     0,   607,   609,   809,   620,   620,     0,
     601,   616,   457,   403,     0,     0,    67,   327,   328,   325,
     326,     0,     0,     2,   224,     0,     0,   226,   411,   225,
     538,   457,   305,   304,   306,     0,     2,   184,   264,     0,
     257,     0,   184,   320,   318,     0,   312,  1044,   321,     0,
       0,     0,   359,   360,   361,   362,     0,   352,     0,   353,
     329,     0,   330,     0,     0,   457,   215,   202,   316,   315,
       0,   350,   369,     0,   401,   809,   457,   781,   736,   457,
       2,     2,   628,   635,   635,  1018,  1019,  1020,     0,   971,
     457,     3,     3,     0,   979,     0,     0,   635,     0,   642,
     641,  1044,     0,   626,     3,     0,   957,    98,     0,    31,
     457,     0,  1044,     0,     0,    85,     0,    73,     0,    79,
       0,    77,    43,   158,   945,   457,     0,     0,   850,   868,
     457,   457,     0,     0,     0,   457,   457,   712,     0,   424,
     426,     3,     3,     0,     0,     0,   787,   573,   575,   571,
       0,   986,     0,   613,   991,   615,   983,   809,   809,   599,
     619,   603,     0,   602,     0,     0,     0,   622,     0,   809,
     596,   610,   621,   611,   617,   664,   668,   667,     0,     2,
       0,     0,   245,     2,   227,   538,   310,   308,   311,   307,
       0,   309,     0,   253,     0,   184,     0,     2,   457,   265,
       0,   290,     0,     0,   313,     0,     0,     2,   336,   363,
       0,   354,     2,     0,     0,     0,     0,   341,     0,   337,
     200,   199,   425,   751,     0,     0,  1044,  1044,  1021,     3,
       0,     0,   978,   980,   627,     0,   635,     0,   625,     2,
      49,    41,    39,    40,     0,    63,   177,    76,     0,     0,
       3,   851,   869,     3,     3,   916,   931,   428,     2,   654,
       3,   653,   711,     0,     0,   842,   900,   950,     0,     0,
       0,   987,   988,   809,   598,   984,   985,   597,   578,     0,
       0,   214,   335,     0,     0,     0,   238,     2,   216,     0,
       0,     2,   247,   262,   273,   267,     2,   184,   302,     0,
     277,     0,     0,   268,   266,   255,   258,     0,     0,   184,
     291,     0,     0,   219,   334,     2,   457,   331,     0,     0,
     379,     2,   339,     0,    66,     0,   351,   756,   758,     0,
       0,     0,   981,   982,   635,  1044,   647,   749,    64,    80,
      78,     0,     0,     0,   457,     0,   843,   901,   809,   994,
     996,   989,     0,   608,   233,   228,   231,     0,   230,   237,
     236,     0,   457,   240,   239,     2,   249,     0,   246,     2,
       0,     0,     0,   254,   259,     0,     0,   184,   303,   278,
       0,     0,     2,     0,   293,   294,   292,   261,   322,     0,
     457,   457,     3,   364,   458,   368,     0,   372,     0,     0,
       0,   380,   381,   222,   342,     0,     2,     0,     2,     2,
     631,   633,   972,  1044,     0,   946,   917,   932,   658,     2,
     990,   992,   993,   614,     0,   235,     0,   234,   218,   241,
     457,   392,   250,     2,   251,   248,   263,   276,   274,   270,
     282,   280,   281,   279,   260,   275,   271,   272,   269,   256,
       0,     0,     0,     0,   221,   241,     3,   357,     0,   986,
     365,   366,   367,   379,     0,     0,     0,   379,     0,     2,
     340,   347,     0,   344,   346,     0,   632,   457,   229,   232,
       2,     3,   242,   393,   252,     0,     0,     0,     0,   301,
     299,   296,   300,   297,   298,   295,     3,   357,     0,     0,
     987,     0,     0,     0,   373,     0,   382,   223,     0,   337,
       0,   630,     3,   210,     0,     0,     2,   289,   287,   284,
     288,   285,   286,   283,     0,     0,   358,     0,   385,     0,
     383,     0,   385,   343,   345,     2,     0,     0,   212,   211,
     217,     0,   220,     0,   355,   386,     0,     0,   374,     0,
     348,     2,   995,   356,     0,     0,     0,     0,   349,   387,
     388,     0,   384,   375,     0,     0,   376,   389
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -1772,  6993,  5863, -1772,    -1,   349,   850,  -158, -1772,  -350,
   -1772,   390, -1772,  -701,   682,   785,  -884, -1074, -1772,   250,
    3636,  2051, -1772,  1277, -1772,  1368,   584,   806,   819,   629,
     816,  1332,  1333,  1335,  1330,  1337, -1772,    41,  -169,  8679,
     914, -1772,  1652, -1772, -1772,  -658,  7176, -1092,  1043, -1772,
     128, -1772,   908,     7, -1772, -1772, -1772,   461,   104, -1772,
   -1771, -1570,   323,    80, -1772, -1772, -1772,   333, -1472, -1772,
   -1458, -1772, -1772, -1772, -1772,    23, -1723,   211, -1772, -1772,
      29, -1772, -1772, -1772,    44,   486,   487,   151, -1772, -1772,
   -1772, -1772,  -754, -1772,    79,    13, -1772,   158, -1772,   -85,
   -1772, -1772, -1772,   916,  -768,  -904, -1311, -1772,    20, -1197,
      63,  3975,  -843,  -830, -1772,  -282, -1772,    70,  -152,   232,
    -312,  -228,  4140,  2746,  -642, -1772,     3,    22,  1958,   450,
   -1772,  2050, -1772,    56,  4621, -1772, -1772, -1772,    53, -1772,
   -1772,   384,    58,  4982,  3290,   -60,  1844,  -303, -1772, -1772,
   -1772, -1772, -1772,  -221,  5049,  6026, -1772,  -351,   202, -1772,
     559,   277, -1772,   207,   760, -1772,   555,   -13, -1772, -1772,
   -1772,  -311,  6075,  -705,  1185,   101,  -687,  -618,  -476,  1040,
   -1772, -1198,  -134,   247,   465,   939,  6248,    76,  -484,  -249,
    -203,  -443,  1311, -1772,  1637,  -154,  1226,  1527, -1772, -1772,
   -1772, -1772,   358,  -168,  -180,  -866, -1772,   438, -1772, -1772,
     668,   495, -1772, -1772, -1772,  2126,  -692,  -411,  -836,   -32,
   -1772, -1772, -1772, -1772, -1772, -1772,    59,  -824,  -151, -1707,
    -144,  2550,   -69,  7310, -1772,  1188, -1772,   813,  -215,  -218,
    -195,  -185,     1,   -57,   -55,   -54,   474,   -27,    42,    51,
    -184,   -90,  -165,  -142,  -125,  -714,  -719,  -679,  -653,  -707,
     -74,  -639, -1772, -1772,  -684,  1376,  1377,  1378,  1066,  7849,
   -1772,  -588,  -542,  -540,  -506,  -572, -1772, -1663, -1669, -1658,
   -1650,  -599,  -115,  -268, -1772, -1772,    -3,    73,   -52, -1772,
    8112,   112,   794,  -373
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,  1158,   222,   392,   393,    81,    82,   394,   368,   395,
    1458,  1459,   396,   978,   979,   980,  1269,  1270,  1271,  1470,
     418,   398,   399,   400,   686,   687,   401,   402,   403,   404,
     405,   406,   407,   408,   409,   410,   411,   420,  1077,   688,
    1391,   749,   216,   751,   414,   816,  1159,  1160,  1161,  1162,
    1163,  1164,  1165,  2064,  1166,  1167,  1396,  1575,  1906,  1907,
    1836,  1837,  1838,  2031,  2032,  1168,  1589,  1590,  1591,  1741,
    1742,  1169,  1170,  1171,  1172,  1173,  1174,  1404,  1768,  1959,
    1876,  1175,  1176,  1607,  2049,  1608,  1609,  1942,  1177,  1178,
    1179,  1394,  1950,  1951,  1952,  2096,  2111,  1979,  1980,   292,
     293,   877,   878,  1131,    84,    85,    86,    87,    88,    89,
     451,    91,    92,    93,    94,    95,   230,   568,   453,   422,
     454,    98,   302,   100,   101,   102,   333,   334,   105,   106,
     168,   107,   896,   335,   154,   110,   250,   111,   155,   258,
     337,   338,   339,   156,   415,   116,   117,   341,   118,   559,
     866,   864,   865,  1547,   342,   343,   121,   122,  1127,  1359,
    1553,  1554,  1702,  1703,  1360,  1542,  1721,  1555,   123,   646,
    1647,   643,   344,   644,   645,  1232,  1070,   459,   460,   870,
     871,   461,   462,   872,   346,   563,  1183,   424,   425,   217,
     479,   480,   481,   482,   483,   321,  1203,   322,   894,   892,
     593,   323,   362,   324,   325,   426,   125,   174,   175,   126,
    1197,  1198,  1199,  1200,     2,  1116,  1117,   585,  1192,   127,
     312,   313,   260,   270,   542,   128,   220,   129,   231,  1079,
     857,   509,   166,   130,   657,   658,   659,   131,   233,   234,
     235,   236,   307,   133,   134,   135,   136,   137,   138,   139,
     239,   308,   241,   242,   243,   784,   785,   786,   787,   788,
     244,   790,   791,   792,   754,   755,   756,   757,   510,   140,
    1655,   618,   619,   620,   621,   622,   623,  1705,  1706,  1707,
    1708,   608,   464,   349,   350,   351,   427,   208,   142,   143,
     144,   353,   808,   624
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      80,   305,   132,    80,   103,   190,   240,   363,   987,   567,
     495,   413,   331,   485,   503,   539,   199,   192,   603,   193,
     194,   367,   526,   104,   150,  1201,   922,   178,   916,   626,
     345,  1818,   805,   496,   691,   634,   967,   697,  1822,   637,
     850,   852,  1819,   497,   498,   960,  1875,   195,   647,   348,
    1820,  1385,   575,  1184,   112,    80,    80,   108,    80,   113,
     132,  1040,   103,   499,    90,  1577,  1914,   151,  1046,  1022,
     908,    96,   909,    80,   141,  1047,   615,   141,  1909,   636,
     207,   104,    80,   639,   205,   495,   500,   507,   297,   503,
      80,   359,   594,  1613,   523,    80,   595,   237,    80,  1274,
     261,  1041,    80,   501,   271,  1915,   910,   886,   496,  1180,
    1510,  1511,   112,  1073,   300,   108,   196,   113,   497,   498,
    1753,   256,    90,   264,   854,   197,  1908,  1042,  1281,    96,
    1746,  1088,   141,   430,   861,   431,   432,   603,   499,   504,
      80,  1043,  1983,    80,   597,    80,   207,   132,   446,   103,
     218,    80,   626,   515,   574,   576,   268,    80,  1901,   493,
    1614,   500,   254,   433,    80,   936,   265,  -394,   104,  1122,
      58,   192,    58,   193,   194,   888,   141,   537,   501,  1578,
    1578,  1071,  1071,   597,   598,   282,  -395,   547,    80,    80,
     205,  1315,  1472,  1205,   916,   210,  1193,  1190,    58,   112,
    1071,   195,   108,    80,   113,   467,  1916,   648,  -759,    90,
     650,  1050,  1364,   476,   504,   609,    96,  1057,    80,   141,
    -791,   412,   554,  1354,   640,  1477,  1975,    80,    80,  1234,
     205,  1365,   434,    97,   579,  1910,   152,  -394,   908,  1238,
     909,   435,  1611,  1984,    80,   917,   192,   157,   193,   194,
     532,   286,    80,   508,   205,   508,  -395,  1478,   841,  1914,
     288,   210,    80,  1833,  1834,    80,  1071,   543,  1262,  1908,
     196,   145,    80,  1852,   910,   823,  1026,   164,   809,   197,
    2011,  1343,    80,    80,  1355,    80,  1346,   837,   541,   901,
     881,    97,  1857,  1858,   570,  1914,   514,  1356,   824,   519,
     428,   626,    80,    80,  1577,   205,   218,   532,   825,   826,
      80,  1372,  -760,   921,   943,   203,  1253,    80,    80,  1040,
     536,  1612,    80,  1072,  1072,   626,   927,  1224,   827,   540,
     546,  1301,   626,   903,   609,   933,  1875,  1288,  1302,  1818,
    1184,   282,  1072,   158,   112,  1835,  1822,   108,   488,   113,
    1819,   828,   366,   148,   256,   731,   985,  1414,  1820,  1041,
      80,   823,  1231,    80,   656,   533,   837,  1371,   829,  1559,
     928,   848,   295,   506,   219,   641,  -791,   853,    97,   789,
     642,   203,  1976,  1977,   824,  1042,  2085,  1901,  1560,  1228,
    1948,  1925,  1926,   776,   825,   826,  1180,   732,  1364,  1293,
    1326,  1148,  1667,  1669,  1671,   172,   172,  1316,  1072,  2030,
    1710,  1051,   838,   199,   827,  1054,   207,  1624,  1578,  1974,
     571,   286,   533,   436,  1067,  1068,   575,  1344,    80,  1711,
     467,   331,   444,  1833,  1834,  2030,   430,   828,   431,   432,
     172,  1317,   611,  1354,  1354,  1354,  1559,  1119,   463,  1367,
    1368,    80,    80,  1317,   829,    58,  1719,  1224,    58,   860,
     163,   692,  2066,    80,    80,  1713,   433,  1050,   348,  1071,
     213,    58,    80,    58,   476,  1720,   159,   256,   886,   160,
     161,   214,   162,   528,   698,   531,    20,  1510,  1511,   699,
     172,   838,    80,   172,   188,  1744,   908,   215,   909,   287,
    1752,    80,  -925,   467,  1355,  1355,  1355,   172,  1535,  -925,
     609,   962,   187,   690,   357,  1862,  1567,  1356,  1356,  1356,
    1443,    80,   430,    97,   431,   432,   900,    80,  1450, -1045,
    1754,   210,   910,   962,   516,   434,    58,   582,   508,  1823,
     641,   508,   531,  1922,   435,   642,   177,   287,  -924,   257,
     767,  1578,  1027,   887,   508,  -924,   508,   206,  1824,   179,
     277,   611,    63,    64,  1369,    80,   626,    80,   172,  1366,
     238,   180,  1283,   262,   845,   570,  1011,   272,    80,   962,
      80,  1956,   467,    58,    80,   457,   132,  1666,   103,  1931,
    1208,   256,   188,   875,    80,   287,   856,  1403,    80,    80,
      58,   626,   859,   257,  1081,   905,   863,   104,  1576,  1592,
      76,  1072,   172,   922,  1957,  1048,   198,    64,  1481,   613,
    1719,   188,   172,   268,  1061,   112,   721,   722,   108,  -574,
     113,    80,   557,   172,  1209,   562,   428,   428,   112,  1827,
      58,   108,   844,   113,    80,    58,  1436,   847,    90,   962,
     541,   203,   200,  1851,   257,    96,   590,   962,   141,  2001,
     172,   211,  1055,   206,   855,  1565,   613,   172,   172,  1090,
     723,   724,   172,   287,   862,   554,  1831,   775,   468,  1098,
    1714,    58,  1107,   508,    -3,   591,   592,   962,  1106,  1307,
    1569,    14,    15,    16,    17,    18,  1968,  2003,    80,   700,
      80,   733,    80,   206,   701,   734,    80,   962,   789,    80,
     172,   548,    58,   172,   606,  1578,   257,   629,  1422,  1102,
      58,   225,   560,   508,  1329,  1502,  1920,   206,   508,  1600,
     436,   606,   508,    58,    80,   606,   955,   923,   942,   962,
     544,   945,   946,  1578,   947,   245,   257,   956,   957,  2036,
      58,  -451,   257,   949,  1747,  1924,   951,   952,   953,  1748,
    1333,   428,   282,    58,   508,   886,   714,  1937,    14,    15,
      16,    17,    18,   715,   716,   553,    64,   463,   962,    80,
     905,    80,   257,  1578,  1798,    58,  1799,  -624,  2038,    58,
     690,  1434,  1869,    80,  -624,   613,   690,  1870,   152,  1487,
      80,   172,   331,   508,    97,   690,   476,   717,   718,    80,
    1664,  -455,  1496,   172,   172,  1467,   508,    97,    80,    80,
      80,  1736,  1737,  1738,   690,  1314,   676,    58,   246,   247,
    1277,   248,  1509,   606,   412,  1994,   249,  1273,    80,   348,
     284,   188,  1500,  1739,  1257,  1076,   613,  1576,  -687,   463,
     286,  1258,  1227,   181,     6,     7,     8,     9,    10,    11,
      12,    13,    73,   287,  1969,  2016,  1469,  1656,   508,  -394,
    2017,   301,   428,  1273,    80,    80,   476,   759,  2081,  1321,
    1182,   760,   612,  2082,   361,   771,   613,   191,  -456,   508,
     364,   256,    73,    78,   614,  1426,  1427,   810,   811,   104,
     921,   812,   541,   468,   874,   257,   615,   935,   875,   232,
    1194,   595,   752,   626,   457,   148,   508,   937,  -780,  1121,
     319,   595,   676,    78,    79,    80,   256,   938,  1776,  1777,
     112,   939,   961,   108,   365,   113,   962,   366,   656,   159,
      90,  1597,   160,   161,   172,   162,   437,    96,   897,   899,
     141,   438,   886,  1195,    14,    15,    16,    17,    18,   471,
    1031,   439,  1339,   141,   508,   306,   484,   457,  1085,   466,
    1300,   789,  1086,  1110,    80,   440,   468,   719,   720,   257,
    1592,   925,    80,   463,  1118,   606,   457,  1120,   286,   441,
     436,  1123,   508,  1438,   172,   725,   726,   286,   442,   257,
    1541,   508,  -442,   428,   516,  1651,   833,   470,   508,   606,
     486,    80,   489,    58,   476,   490,   582,  -452,   436,   257,
     508,   491,   606,  1662,   463,  -442,   873,    14,    15,    16,
      17,    18,   200,   694,   516,   363,   363,    80,   508,   964,
     965,   494,   232,    80,    80,  1415,   463,   463,   971,  1278,
     973,  1048,   976,   436,   257,   613,   984,  1112,   306,   988,
     492,   962,  1449,   505,   477,   463,   506,   702,   524,   703,
     704,   705,    73,  1114,    80,   582,  1272,   962,   257,   508,
    1273,  1885,   525,  1421,  1013,   257,    58,   760,  1455,   331,
    1530,  1454,   612,   535,  1579,  1273,   613,   609,   706,   601,
     694,   707,   708,    78,   614,   218,   709,   710,   545,    97,
    -121,  -121,  -121,  -121,  -121,  -121,   580,   306,   586,  1659,
     457,   564,  1196,  1660,  1361,  1076,   348,  1736,  1737,  1738,
     601,   463,   633,  1730,  1648,   642,   476,   962,  1182,    80,
      80,    80,  1557,   172,  1981,  1063,  1064,  1518,  1519,  1739,
     172,   649,    14,    15,    16,    17,    18,   104,  1740,   660,
    1756,   457,  1757,   476,   962,    73,  1086,  1182,  1089,    80,
    1091,   661,  1981,  1955,  1758,  1512,   664,    80,   962,  1963,
      80,    80,   261,   271,    80,   612,   104,   665,   112,   613,
    1828,   108,   666,   113,   760,    80,    78,    79,    90,   670,
    1917,   694,   264,   256,   962,    96,  2020,   713,   141,  1810,
    1273,    58,  2033,   727,  2083,   729,    73,   112,   962,  -453,
     108,   728,   113,   730,  1130,   172,   172,    90,    80,    14,
      15,    16,    17,    18,    96,   268,   612,   141,   541,  2107,
     613,    80,  2114,  2104,   254,   265,  2115,    78,   614,  1345,
      14,    15,    16,    17,    18,   141,   735,   476,   761,  2051,
      73,   762,  1370,  2055,   763,    80,   783,  1558,  1065,  1066,
      73,   764,   412,  1260,  1086,  1226,   331,  1275,  1276,  1389,
    1700,  1736,  1737,  1738,   508,   765,   606,   766,    58,   629,
     752,    78,    79,   962,   508,   962,  1279,    80,   990,   991,
     992,    78,    79,  1739,   443,   518,   822,  -156,  -156,    58,
      -3,   873,  1745,   348,   428,   232,  1697,  1698,  1699,   257,
    1482,  1460,   793,   803,   923,   477,  1347,  1348,  1349,  1557,
     257,  -454,   495,  1579,   463,   306,  1785,   503,   457,  1065,
    1413,   306,  1475,  1476,  1361,  1361,  1361,   807,  1543,  1361,
     997,   998,   999,  1000,   257,   496,    80,   412,   412,  1357,
      80,  1480,  1476,    80,   -17,   497,   498,    97,  1484,  1476,
     150,   806,   873,  -120,  -120,  -120,  -120,  -120,  -120,  1581,
    1581,   306,   817,   476,   840,   499,  1400,    14,    15,    16,
      17,    18,   885,   830,   306,   831,    97,   842,   104,   104,
    1037,  1468,   832,   476,   834,    80,   858,   543,   500,  1520,
    1468,   147,   835,   151,   170,   171,    65,    66,    67,    68,
      69,    70,    71,   141,   836,   501,  1037,  1532,   541,   112,
     112,   294,   108,   108,   113,   113,  1672,  1086,   931,    90,
      90,  1736,  1737,  1738,  1796,  1086,    58,   476,  -572,   141,
     141,  1797,  1476,   962,  1558,   876,  1765,  1602,  1603,  1604,
    1605,  1606,   504,  1739,  -570,   476,  1807,  1808,   867,   540,
      80,   889,  -185,  1194,   331,    80,    80,    80,  1817,   962,
     891,  1715,   873,   895,  1401,   913,   172,   911,   758,   172,
     172,   172,  1512,  1873,  1874,  1568,  1570,   823,   615,   873,
     873,  1889,  1476,   837,   769,    73,   930,   772,  1890,  1476,
     934,   348,   940,   172,  1833,  1834,  1195,  1473,  1474,   172,
     824,  2104,  2105,   993,   994,  1700,   141,   562,   963,   508,
     825,   826,   941,  1622,   172,   966,    78,    79,   995,   996,
     969,  1001,  1002,    80,  1722,  1722,  1010,  1649,  1650,    80,
     827,    80,  1512,  1015,  1943,   463,   463,  1036,    80,  1037,
     147,  1423,  1424,  1044,   518,    65,    66,    67,    68,    69,
      70,    71,   476,   828,  1083,  1092,  1093,  -763,   172,  1357,
    1357,  1357,   152,  1540,  1544,   476,  1094,  1095,  1096,  1097,
     829,  1456,  1113,  1039,  1115,   783,   606,  1124,  1185,  1080,
    -663,   264,   256,  1125,  1126,  1186,  1868,  1202,    97,    97,
    1233,  1218,  1219,  1220,  1230,  1557,  1236,  1231,  1581,  1239,
    1235,  1243,   476,  1320,  1878,   457,  1240,  1242,   838,  1943,
    1244,   257,  1245,   306,   268,  1246,  1248,   104,  1249,  1250,
    1255,   626,  1256,   254,   265,  1280,  1263,  1264,  1285,  1194,
     667,  1286,   306,  1287,   141,   262,   272,   477,   428,  1294,
     803,  1295,  1296,  1297,  1905,  -651,   257,    80,   112,    80,
    1305,   108,  -650,   113,  1328,   711,   712,  1340,    90,    19,
    -764,  1393,  1362,  1363,  1373,  1196,   651,  1376,   141,  1377,
    1386,  1387,  1195,  1388,  1395,  -686,   711,  1397,   147,  1403,
     962,  1409,   141,    65,    66,    67,    68,    69,    70,    71,
     172,  1407,  1949,   172,    80,  1410,  1411,    80,    48,    49,
      50,    51,    52,    53,    54,    55,   711,  1417,   476,  1452,
    1419,  1466,   476,  1468,  1759,  1495,  1460,  1483,  1513,  1508,
    1558,  1514,  1516,  1515,  1476,  1546,   476,    75,  1521,  1524,
     802,  1581,  1533,   172,   873,   873,   476,   541,  1366,  1534,
    1512,   652,    14,    15,    16,    17,    18,   959,   873,   873,
     104,  1536,  1572,    80,    80,  1615,   653,  1548,  1549,   654,
     655,    65,    66,    67,    68,    69,    70,    71,  1593,  1594,
     495,  1596,  2010,  1598,   503,  1610,  1617,  1625,   540,  1618,
    1620,   112,   873,   873,   108,  2028,   113,  1905,   257,  1726,
    1619,    90,   457,   496,  1627,  1628,  1630,  1631,   758,   758,
    1632,   141,  1633,   497,   498,  1268,  1665,  1634,  1029,    80,
    1635,  1032,  1636,  1268,  1637,   837,   476,  1639,  1644,  1653,
     476,  1657,  1709,   499,  1658,   476,  2053,    97,  1661,  1673,
    1674,  1039,  1687,  1678,  1679,  1949,   257,  1299,   783,  1949,
    1949,  1196,  1268,  1945,   436,   477,   500,  1689,  1520,  1696,
     476,  1551,  1273,  1729,   412,  1731,  1733,  1762,   219,  1764,
     544,  1769,  1841,   501,  1784,  1778,  1111,  1782,  1783,  1786,
    1788,  1790,   518,  1795,  2079,  1801,  1100,  1802,  1805,  1806,
    1104,    14,    15,    16,    17,    18,  1252,  1812,   172,  1815,
    1816,  1846,  1847,  1859,   476,  1581,  1861,  1867,   476,   504,
    2095,  1865,   172,  1148,  2095,  1268,  1871,  1872,  2106,  1886,
    1884,   476,   463,   463,   104,   172,  1882,  1883,  1945,   579,
    1887,  1888,    80,  1581,    80,  -652,  2109,  1217,  1896,  1897,
    1898,   192,  1899,   193,   194,   476,  1900,   476,   476,   508,
     838,  1919,   104,  -555,  1927,   112,  1932,  1921,   108,  1930,
     113,  1946,   172,  1960,  1938,    90,  1961,  1947,  1962,  1808,
      97,  1965,   476,  1581,  1966,   141,  2000,  1967,  1978,  1987,
     989,   873,   873,   112,  2002,  2004,   108,  2013,   113,  1425,
    2014,  2015,   104,    90,  2018,  2019,  2022,    80,    80,   306,
     205,  2026,  2039,   141,  2052,   412,  2035,   412,   476,  2037,
     186,  2059,  2048,  2060,  2054,  2061,  2067,  1727,  1451,   476,
     257,  2077,  2080,   112,  2078,  2090,   108,  2092,   113,  2098,
    2101,  2097,  2093,    90,  2113,  2102,  2112,  2116,  1566,    80,
    1792,   467,    83,   141,   958,   149,   412,  1284,  1479,  1003,
    1006,  1004,   274,   476,  1005,   476,   275,  1392,  1007,   278,
     750,   280,  1399,  2091,  1291,  1292,  1766,  1485,   172,  1863,
    2029,  1856,   172,  2086,   476,  2046,  1958,   758,  2084,  2076,
     476,  2075,  2006,  1760,  1761,  2099,   172,  2056,  2005,   169,
     476,  1408,   534,  1712,    80,   873,   172,  1903,   477,  1973,
      83,  1503,  1545,  1723,    80,  1229,  1268,  1405,  1082,   813,
    1204,  1654,   893,   172,  1773,   189,     3,  1237,  1018,  1019,
    1020,     0,     0,     0,    83,     0,     0,     0,   412,     0,
     873,   463,     0,     0,    97,   873,   873,   229,     0,     0,
     253,     0,     0,     0,    83,     0,     0,   147,  1331,     0,
       0,  1335,    65,    66,    67,    68,    69,    70,    71,  1265,
    1556,     0,    97,  1266,     0,  1267,     0,     0,     0,     0,
    1375,     0,     0,     0,     0,   257,   172,     0,     0,     0,
     172,   149,     0,     0,     0,   172,     0,    83,     0,     0,
     149,     0,     0,   304,   310,  2094,    75,     0,     0,  1471,
       0,     0,    97,     0,   274,   330,     0,     0,     0,     0,
     172,  2103,     0,     0,     0,     0,     0,  1247,   314,   315,
     316,   317,  1251,     0,   477,     0,     0,  1646,     0,   419,
     189,   189,     0,  1259,  1652,     0,     0,     0,     0,     0,
     606,   149,   449,     0,     0,   253,     0,     0,     0,     0,
       0,  1663,     0,     0,   172,     0,   147,     0,   172,   170,
     171,    65,    66,    67,    68,    69,    70,    71,     0,   229,
     229,   172,   181,     6,     7,     8,     9,    10,    11,    12,
      13,     0,   274,   275,  2012,   630,   304,   280,   477,     0,
     245,     0,     0,     0,    83,   172,     0,   172,   172,     0,
       0,     0,     0,     0,     0,     0,   477,   253,   318,     0,
       0,  1268,     0,     0,     0,   606,  1268,  1268,  1268,  1491,
    1492,     0,   172,     0,   675,     0,   319,     0,     0,     0,
       0,     0,     0,  1506,  1507,     0,     0,   310,     0,     0,
       0,  1489,     0,   310,   304,   304,     0,  1556,     0,     0,
    1498,   149,     0,  1716,   147,  1556,     0,     0,   172,    65,
      66,    67,    68,    69,    70,    71,   974,  1528,  1529,   172,
       0,   330,   616,   625,     0,     0,     0,     0,     0,     0,
       0,  1755,     0,     0,   257,     0,     0,     0,   330,     0,
     147,     0,   330,   226,   227,    65,    66,    67,    68,    69,
      70,    71,   667,   172,   147,   172,   975,   170,   171,    65,
      66,    67,    68,    69,    70,    71,     0,     0,    73,     0,
     675,     0,     0,     0,   172,     0,   419,     0,     0,     0,
     172,     0,     0,     0,     0,  1787,     0,  1621,  1550,    75,
     172,     0,     0,     0,  2110,  1551,  1794,     0,     0,    78,
      79,     0,     0,     0,  2117,    19,     0,     0,     0,     0,
     419,     0,     0,   753,     0,     0,     0,     0,     0,     0,
     189,     0,     0,     0,     0,     0,     0,  1378,     0,   711,
       0,     0,   873,     0,     0,     0,   149,     0,     0,   274,
     449,     0,     0,     0,   782,     0,   625,     0,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,  1268,     0,
    1268,     0,     0,     0,     0,  1461,  1462,  1463,  1829,   147,
       0,  1556,  1464,  1465,    65,    66,    67,    68,    69,    70,
      71,  1265,     0,     0,   229,  1266,     0,  1267,     0,     0,
     147,     0,     0,   229,     0,    65,    66,    67,    68,    69,
      70,    71,   982,     0,     0,     0,  1691,  1692,     0,     0,
    1879,  1880,     0,   304,     0,   419,   419,     0,    75,   304,
       0,   330,     0,     0,     0,     0,   147,     0,   306,   170,
     171,    65,    66,    67,    68,    69,    70,    71,   165,     0,
       0,     0,   983,     0,     0,     0,  1704,     0,     0,    14,
      15,    16,    17,    18,     0,     0,  1732,     0,     0,   304,
     181,     6,     7,     8,     9,    10,    11,    12,    13,  1743,
     304,     0,   304,     0,   330,     0,    83,     0,     0,     0,
       0,     0,     0,  1556,     0,     0,     0,     0,     0,     0,
       0,     0,   330,   449,     0,   625,    58,     0,     0,  1380,
       0,     0,     0,   616,     0,     0,  1771,   616,    58,     0,
     283,     0,     0,     0,     0,     0,   330,     0,     0,  1964,
    1779,   273,     0,   289,     0,   290,   625,   147,     0,   330,
     226,   227,    65,    66,    67,    68,    69,    70,    71,   147,
     149,     0,   226,   227,    65,    66,    67,    68,    69,    70,
      71,     0,     0,   419,     0,  1800,   149,   149,     0,   419,
    1803,  1804,     0,     0,     0,     0,     0,    73,   419,     0,
       0,   149,   149,   149,   147,  1298,    75,     0,     0,    65,
      66,    67,    68,    69,    70,    71,     0,   780,    75,     0,
       0,   613,     0,     0,     0,     0,   306,  2025,    78,   781,
       0,     0,     0,     0,     0,     0,  1704,  1704,     0,     0,
       0,     0,  1832,     0,     0,     0,  1842,     0,     0,     0,
       0,     0,  1298,    75,     0,   512,   513,   449,     0,   517,
    1855,     0,   520,   521,     0,     0,     0,     0,     0,     0,
    1864,     0,     0,   753,   753,     0,     0,     0,   185,   147,
       0,   419,   170,   171,    65,    66,    67,    68,    69,    70,
      71,   580,   306,     0,     0,     0,     0,     0,   449,     0,
       0,   782,   147,   782,     0,   354,   355,    65,    66,    67,
      68,    69,    70,    71,     0,   255,     0,     0,     0,     0,
     330,   330,     0,     0,     0,     0,   276,     0,   279,     0,
     281,     0,     0,   306,     0,     0,     0,     0,  1382,   330,
       0,   304,     0,     0,     0,     0,     0,   599,   600,     0,
    1913,     0,     0,    76,  1918,     0,     0,  1704,   356,  1923,
     304,     0,     0,   632,     0,     0,     0,  1767,     0,   255,
     147,   279,   281,   170,   171,    65,    66,    67,    68,    69,
      70,    71,     0,     0,  1953,     0,     0,     0,     0,     0,
       0,    14,    15,    16,    17,    18,     0,     0,   419,     0,
       0,     0,     0,     0,     0,   330,     0,     0,     0,     0,
       0,   149,   419,     0,     0,    58,     0,     0,     0,     0,
     255,     0,     0,   330,     0,  1212,   588,     0,  1982,     0,
       0,     0,  1985,     0,  1971,     0,   616,     0,  1704,     0,
       0,     0,     0,     0,     0,  1999,   147,     0,     0,     0,
      58,    65,    66,    67,    68,    69,    70,    71,   768,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  2021,
       0,  2023,  2024,     0,    73,   449,     0,     0,     0,  1704,
       0,   147,   255,     0,   279,   281,    65,    66,    67,    68,
      69,    70,    71,     0,    74,    75,  2034,     0,     0,     0,
       0,     0,     0,     0,     0,    78,    79,     0,     0,    73,
       0,     0,   255,     0,     0,     0,     0,     0,   255,     0,
       0,     0,     0,     0,     0,   839,     0,     0,     0,    74,
      75,     0,  2057,     0,     0,     0,     0,  2062,     0,     0,
      78,    79,   753,  2063,  1704,  1704,     0,     0,   255,     0,
       0,     0,     0,     0,   631,     0,   281,     0,     0,   782,
       0,     0,     0,   375,     0,   376,   782,   377,    63,    64,
      65,    66,    67,    68,    69,    70,    71,  2089,     0,  2063,
       0,     0,     0,     0,     0,     0,  1704,    58,     0,     0,
       0,     0,   674,     0,     0,     0,     0,     0,  2100,     0,
       0,     0,     0,     0,  2089,     0,     0,     0,   330,     0,
       0,     0,     0,   696,  2108,     0,    76,   386,   147,     0,
    1954,   226,   227,    65,    66,    67,    68,    69,    70,    71,
       0,    14,    15,    16,    17,    18,     0,     0,     0,     0,
     918,   919,     0,     0,     0,   255,    73,     0,   149,     0,
       0,     0,   147,   926,     0,     0,   419,    65,    66,    67,
      68,    69,    70,    71,     0,     0,  2008,    75,     0,     0,
     508,   255,     0,   631,   281,     0,     0,    78,    79,     0,
      73,     0,     0,     0,     0,   419,     0,     0,   674,     0,
      58,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1038,    75,   253,    83,   613,     0,     0,     0,     0,     0,
       0,    78,    79,     0,     0,     0,     0,   304,   255,     0,
       0,   147,     0,   149,   226,   227,    65,    66,    67,    68,
      69,    70,    71,   449,     0,     0,     0,     0,   255,     0,
       0,     0,     0,   255,     0,   255,     0,     0,   147,    73,
       0,   226,   227,    65,    66,    67,    68,    69,    70,    71,
       0,   115,   449,     0,   115,   255,   149,   255,   255,  2008,
      75,     0,     0,   508,     0,     0,  1023,  1024,     0,     0,
      78,    79,  1028,   147,     0,   255,   226,   227,    65,    66,
      67,    68,    69,    70,    71,     0,     0,   255,     0,     0,
       0,     0,     0,  1049,   898,     0,  1052,  1053,     0,  1056,
       0,  1058,  1059,     0,     0,     0,     0,     0,     0,   115,
     255,     0,   631,   281,     0,     0,     0,     0,     0,   330,
     330,     0,     0,     0,     0,     0,     0,     0,     0,  1225,
       0,     0,     0,   115,   255,   631,     0,     0,     0,     0,
    1099,   255,     0,     0,  1103,     0,     0,     0,     0,   259,
       0,     0,     0,   115,     0,     0,     0,     0,   149,   149,
     149,   149,   149,   149,     0,     0,     0,     0,  1552,   310,
       0,     0,   147,     0,  1904,   226,   227,    65,    66,    67,
      68,    69,    70,    71,     0,     0,     0,   419,   419,     0,
     115,     0,     0,     0,     0,     0,   115,     0,     0,   115,
      73,     0,     0,   259,     0,     0,     0,     0,     0,  1206,
    1207,     0,     0,   326,   115,   358,     0,   253,     0,     0,
     780,    75,   369,  1223,   613,   370,     0,   371,     0,   372,
       0,    78,   781,     0,     0,     0,     0,     0,   423,   449,
       0,     0,     0,     0,   615,     0,   373,     0,     0,     0,
     115,   423,   147,     0,   259,   170,   171,    65,    66,    67,
      68,    69,    70,    71,   149,     0,   616,     0,     0,     0,
       0,     0,     0,   374,   375,     0,   376,     0,   377,    63,
      64,    65,    66,    67,    68,    69,    70,    71,   378,   379,
     366,     0,   380,   381,   382,     0,   383,   384,     0,     0,
     466,   115,   147,   115,    73,   555,   556,    65,    66,    67,
      68,    69,    70,    71,     0,     0,   259,     0,    14,    15,
      16,    17,    18,     0,   385,     0,     0,    76,   386,     0,
       0,     0,     0,   558,   387,    78,    79,   388,   389,   390,
     391,   115,     0,     0,     0,     0,   259,     0,     0,     0,
       0,  1701,   259,    76,  1223,  1552,     0,   419,     0,     0,
     115,  1552,     0,  1552,     0,    14,    15,    16,    17,    18,
       0,     0,     0,     0,     0,   255,     0,    58,     0,     0,
     115,     0,   259,   115,     0,     0,   255,     0,     0,     0,
       0,   310,   149,     0,     0,  1323,     0,   115,     0,     0,
       0,   115,  1330,     0,     0,  1334,     0,     0,   147,     0,
     255,   226,   227,    65,    66,    67,    68,    69,    70,    71,
       0,   255,     0,     0,    58,     0,   419,     0,     0,     0,
     255,     0,     0,     0,     0,   423,    73,   330,   147,     0,
     149,   170,   171,    65,    66,    67,    68,    69,    70,    71,
       0,    58,     0,     0,     0,   147,   228,    75,   226,   227,
      65,    66,    67,    68,    69,    70,    71,    78,    79,   423,
     147,   149,     0,   198,    64,    65,    66,    67,    68,    69,
      70,    71,   147,    73,     0,   226,   227,    65,    66,    67,
      68,    69,    70,    71,     0,   115,   330,   330,     0,   423,
       0,     0,     0,   303,    75,   259,     0,     0,     0,     0,
      73,  1701,  1701,     0,    78,    79,     0,     0,     0,    75,
       0,     0,   802,     0,     0,     0,  1552,   255,  1435,  1552,
     228,    75,     0,     0,     0,     0,  1444,  1445,     0,     0,
       0,    78,    79,     0,     0,     0,   310,     0,     0,     0,
       0,   255,     0,     0,     0,     0,     0,     0,     0,   419,
     147,     0,     0,   553,    64,    65,    66,    67,    68,    69,
      70,    71,     0,     0,   423,   423,   397,     0,     0,   259,
     115,     0,   147,     0,     0,     0,   304,    65,    66,    67,
      68,    69,    70,    71,  1265,  1488,     0,     0,  1266,     0,
    1267,     0,     0,     0,  1497,     0,     0,  1501,     0,  1504,
    1505,   115,  1012,     0,     0,     0,   115,     0,     0,   259,
     115,     0,   115,     0,     0,     0,     0,     0,     0,     0,
      58,    75,  1701,   115,  1668,   115,     0,     0,     0,     0,
       0,  1552,     0,    14,    15,    16,    17,    18,     0,   358,
    1531,   115,   423,     0,   259,     0,     0,     0,     0,     0,
       0,   147,     0,     0,   226,   227,    65,    66,    67,    68,
      69,    70,    71,     0,     0,   115,     0,   149,   259,     0,
       0,     0,   558,     0,     0,   259,     0,   255,   115,    73,
     929,     0,     0,     0,     0,     0,     0,     0,     0,   115,
       0,     0,    58,     0,     0,   330,     0,     0,     0,   303,
      75,     0,   423,  1701,     0,   115,   115,  1623,   423,     0,
      78,    79,   255,   149,     0,     0,     0,   423,   255,     0,
     115,   115,   115,   147,     0,     0,   226,   227,    65,    66,
      67,    68,    69,    70,    71,     0,     0,     0,     0,     0,
       0,   149,   149,     0,  2009,   310,     0,     0,     0,     0,
       0,    73,     0,     0,     0,   663,     0,     0,     0,   397,
     669,     0,     0,     0,     0,     0,     0,     0,     0,   678,
     679,  1550,    75,     0,     0,     0,   423,     0,     0,     0,
       0,   149,    78,    79,   397,   397,     0,     0,     0,     0,
       0,    58,     0,     0,     0,  1501,     0,     0,     0,     0,
     423,     0,     0,     0,     0,   397,     0,     0,   202,  2009,
    2009,     0,     0,     0,     0,     0,     0,   423,     0,     0,
       0,     0,   147,     0,  1690,   226,   227,    65,    66,    67,
      68,    69,    70,    71,     0,   397,     0,     0,     0,   115,
     115,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      73,  2009,     0,     0,   255,     0,   147,     0,   115,   226,
     227,    65,    66,    67,    68,    69,    70,    71,     0,     0,
    1550,    75,     0,     0,   202,     0,     0,     0,     0,     0,
       0,    78,    79,     0,    73,     0,   115,     0,     0,   202,
       0,    99,     0,     0,   153,     0,     0,     0,     0,     0,
       0,     0,   255,     0,  1550,    75,     0,     0,     0,   259,
       0,  1551,     0,   202,     0,    78,    79,   423,     0,     0,
     259,     0,     0,     0,   115,  1772,   452,     0,     0,   147,
     115,   423,    63,    64,    65,    66,    67,    68,    69,    70,
      71,     0,   115,     0,  1214,   423,     0,   115,     0,    99,
       0,   147,     0,     0,   226,   227,    65,    66,    67,    68,
      69,    70,    71,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   204,     0,     0,     0,     0,   202,    73,
      76,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   266,   423,     0,     0,     0,     0,  2008,
      75,     0,     0,   508,     0,     0,     0,  1825,  1826,     0,
      78,    79,     0,     0,     0,     0,     0,     0,   147,  1830,
       0,   226,   227,    65,    66,    67,    68,    69,    70,    71,
     296,     0,     0,     0,   147,   202,    99,   226,   227,    65,
      66,    67,    68,    69,    70,    71,    73,     0,     0,     0,
       0,     0,     0,     0,   332,   202,     0,   115,     0,     0,
       0,     0,    73,     0,     0,     0,   228,    75,     0,     0,
       0,     0,     0,     0,   115,   115,   255,    78,    79,   429,
       0,     0,   303,    75,     0,     0,     0,     0,     0,     0,
     296,   455,     0,    78,    79,     0,     0,     0,     0,   397,
     397,   397,   397,   397,   397,   397,   397,   397,   397,   397,
     397,   397,   397,   397,   397,   397,   397,   397,     0,   502,
       0,     0,     0,  1902,     0,     0,     0,   115,     0,     0,
       0,     0,     0,     0,     0,   522,     0,     0,     0,     0,
     527,   529,     0,   204,   202,     0,     0,     0,   147,     0,
       0,     0,     0,    65,    66,    67,    68,    69,    70,    71,
    1265,     0,     0,     0,  1266,   549,  1267,   115,   551,     0,
     552,     0,     0,     0,   202,   423,     0,     0,     0,   397,
     147,   569,     0,   170,   171,    65,    66,    67,    68,    69,
      70,    71,     0,     0,   581,     0,     0,    75,  1970,     0,
    1670,     0,     0,     0,   423,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   255,     0,     0,     0,     0,     0,
     604,   259,   115,   628,     0,     0,     0,     0,   470,     0,
       0,   255,     0,     0,     0,     0,     0,   635,     0,     0,
       0,   635,   115,     0,     0,     0,     0,     0,     0,   202,
     202,     0,   423,     0,     0,   452,  1214,   737,   738,   739,
     740,   741,   742,   743,   744,   745,   746,   747,  1446,     0,
       0,   213,     0,     0,     0,     0,     0,     0,     0,     0,
     115,   423,     0,     0,     0,   115,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   748,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   202,     0,
       0,     0,     0,     0,     0,     0,     0,   255,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   452,     0,   115,
     115,     0,     0,     0,     0,   296,   397,     0,     0,   604,
       0,   397,     0,   115,   115,     0,     0,     0,   115,   115,
     202,     0,   397,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   109,     0,   202,     0,     0,   115,   115,     0,
       0,     0,     0,     0,     0,     0,     0,   115,   115,   115,
     115,   115,   115,     0,   397,     0,     0,     0,   259,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   423,   423,     0,     0,
     455,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     109,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     255,     0,     0,     0,     0,     0,   259,     0,     0,     0,
       0,   869,     0,     0,     0,     0,   529,     0,     0,     0,
     880,   452,   569,     0,     0,     0,     0,     0,   423,     0,
       0,     0,     0,   332,   267,    99,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   202,     0,     0,     0,     0,
       0,   635,   904,   115,     0,     0,     0,     0,     0,     0,
       0,     0,   452,     0,     0,     0,   915,     0,     0,     0,
       0,     0,     0,     0,     0,   604,     0,   109,     0,     0,
     924,   397,     0,     0,   452,   452,     0,     0,   635,     0,
       0,     0,     0,     0,     0,   336,     0,     0,     0,     0,
       0,     0,     0,   452,     0,     0,   255,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   115,   115,     0,     0,
       0,     0,   456,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   423,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   397,     0,
       0,     0,   115,     0,     0,     0,     0,     0,     0,   452,
       0,     0,     0,     0,     0,     0,   202,   397,     0,     0,
     259,   115,     0,     0,     0,     0,   455,     0,     0,     0,
       0,     0,     0,     0,   397,   397,   397,     0,     0,     0,
       0,   397,   397,  1021,     0,     0,   550,     0,     0,     0,
       0,     0,     0,     0,     0,   423,     0,     0,     0,     0,
       0,     0,   109,     0,     0,   397,   115,   904,     0,   115,
       0,     0,  1045,     0,     0,     0,     0,     0,     0,   202,
     115,     0,     0,     0,     0,     0,     0,     0,     0,   455,
     455,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     115,   605,   397,   397,   267,     0,     0,     0,   455,     0,
       0,     0,     0,     0,     0,   115,     0,     0,   605,     0,
     115,   115,   605,     0,     0,   115,   115,     0,    14,    15,
      16,    17,    18,   114,     0,    20,   869,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,  -458,  -458,     0,  -458,    46,  1181,    47,     0,
    -458,     0,     0,     0,   455,   259,     0,     0,     0,     0,
     153,     0,     0,     0,     0,     0,     0,    58,   423,     0,
       0,   114,   635,     0,     0,  1216,     0,   869,     0,     0,
     119,     0,  1222,   119,     0,     0,     0,     0,     0,     0,
       0,     0,   452,     0,     0,     0,     0,     0,     0,     0,
     605,    63,    64,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   269,     0,     0,     0,     0,
       0,     0,     0,     0,   332,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   119,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    76,
       0,     0,     0,     0,     0,     0,     0,     0,   114,     0,
       0,     0,   119,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   340,     0,     0,     0,
       0,   456,   119,     0,     0,     0,   115,   869,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   869,   869,     0,   202,     0,     0,
       0,     0,   336,   458,   115,     0,     0,   202,     0,   119,
       0,   267,     0,   109,     0,   119,     0,     0,   119,     0,
       0,     0,   115,     0,   456,     0,   109,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   202,     0,     0,     0,
       0,     0,   605,   456,     0,     0,     0,   455,     0,     0,
     115,   115,     0,     0,   259,     0,     0,   119,     0,     0,
       0,     0,     0,     0,     0,     0,   605,     0,     0,   119,
       0,     0,     0,     0,     0,     0,   397,     0,     0,   605,
       0,     0,     0,     0,     0,     0,     0,  1358,     0,     0,
     115,     0,     0,   114,     0,  1181,     0,     0,     0,     0,
       0,     0,     0,   452,   452,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     119,     0,   119,     0,  1181,     0,     0,   119,     0,     0,
       0,     0,   607,     0,     0,   269,     0,   115,     0,     0,
       0,     0,  1406,     0,     0,     0,     0,     0,     0,   607,
       0,     0,     0,   607,     0,     0,     0,     0,     0,     0,
     119,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   604,     0,     0,     0,     0,   456,     0,   119,
       0,   527,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     869,   332,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   456,     0,
       0,     0,     0,   202,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     336,   336,     0,     0,     0,     0,     0,     0,     0,   869,
     869,   607,     0,     0,   119,     0,     0,     0,     0,   336,
       0,     0,     0,   869,   869,     0,     0,     0,   455,   455,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   336,   119,   397,
       0,     0,     0,     0,     0,     0,     0,   869,   869,     0,
       0,     0,     0,     0,     0,     0,     0,  1358,  1358,  1358,
     153,     0,     0,     0,   119,     0,     0,     0,   109,     0,
       0,     0,     0,     0,     0,   336,     0,     0,     0,   397,
       0,     0,   458,     0,     0,     0,  1580,  1580,     0,     0,
       0,   202,     0,   605,     0,     0,   267,     0,   336,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   340,     0,     0,     0,     0,     0,     0,
       0,     0,   269,     0,   114,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   458,     0,   114,   332,     0,
       0,     0,     0,   119,   119,   456,     0,     0,     0,     0,
       0,     0,     0,   607,   458,     0,     0,     0,     0,     0,
       0,     0,     0,   153,     0,     0,     0,     0,     0,     0,
       0,   202,     0,     0,     0,     0,     0,   607,     0,     0,
     397,     0,   397,     0,     0,   119,     0,     0,     0,   119,
     607,   119,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   119,     0,     0,     0,   336,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   397,     0,     0,     0,   336,   336,     0,     0,     0,
     452,   452,     0,     0,     0,     0,   869,   869,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   397,     0,     0,     0,     0,     0,
       0,     0,  1718,     0,     0,     0,     0,     0,   119,     0,
       0,     0,   869,     0,     0,     0,     0,     0,   336,     0,
       0,   119,     0,     0,   119,   119,     0,   119,   458,     0,
       0,  1735,     0,     0,     0,     0,   119,     0,     0,   119,
     119,   119,     0,   397,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1580,   109,     0,     0,   458,
       0,     0,     0,     0,     0,     0,   332,     0,     0,   153,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     869,   340,   340,     0,     0,   109,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     340,     0,     0,   267,     0,     0,     0,     0,     0,   119,
       0,     0,     0,     0,     0,   869,     0,     0,     0,     0,
     869,   869,     0,     0,     0,   455,   455,     0,   340,     0,
       0,     0,     0,   605,     0,     0,     0,     0,     0,     0,
       0,  1821,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   114,
       0,   336,   456,     0,     0,     0,   340,     0,     0,   452,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   607,     0,     0,   269,  1580,   340,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     336,   336,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   336,   336,   119,     0,     0,   336,
     336,     0,     0,     0,     0,     0,   458,     0,     0,   119,
     119,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     212,     0,     0,     0,     0,     0,   223,   224,   336,   336,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     285,     0,     0,     0,     0,     0,     0,   109,   109,   340,
       0,     0,     0,     0,     0,     0,  1944,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   340,   340,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   120,     0,     0,
     120,     0,     0,     0,   455,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   456,
       0,     0,  1580,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   340,
       0,     0,     0,     0,     0,     0,   124,     0,     0,   124,
    1580,  1944,     0,     0,     0,   120,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   120,
       0,     0,     0,     0,     0,     0,     0,   114,     0,     0,
    1580,     0,     0,     0,     0,     0,     0,     0,     0,   120,
       0,     0,     0,     0,   124,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   114,   336,   336,  2050,
       0,     0,     0,     0,     0,     0,     0,     0,   124,     0,
       0,     0,     0,   577,   269,     0,   120,   869,     0,     0,
       0,     0,   120,     0,     0,   120,   119,     0,   124,     0,
       0,     0,     0,   336,   119,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   607,     0,     0,     0,     0,     0,
       0,     0,   267,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   119,   120,   124,     0,     0,     0,     0,
       0,   124,   340,   458,   124,     0,   120,     0,     0,     0,
       0,   119,     0,     0,     0,     0,   109,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   336,     0,     0,
       0,   119,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   336,     0,   124,     0,     0,     0,     0,     0,     0,
       0,   340,   340,     0,     0,   124,     0,   120,     0,   120,
       0,     0,     0,     0,   120,   340,   340,     0,     0,     0,
     340,   340,     0,     0,   119,     0,   336,     0,     0,     0,
       0,   336,   336,     0,   173,   176,   336,   336,     0,     0,
       0,   778,     0,   779,     0,     0,     0,   120,     0,   340,
     340,     0,   795,   796,     0,     0,   124,     0,   124,     0,
       0,     0,     0,   124,     0,     0,   120,     0,     0,   221,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   114,   114,
       0,     0,     0,     0,     0,     0,   124,     0,     0,   109,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   124,     0,     0,     0,   298,
       0,     0,   299,     0,     0,     0,   119,   119,   119,   119,
     119,   119,     0,     0,     0,     0,   320,     0,     0,     0,
     458,   120,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   119,   119,     0,     0,     0,
     879,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   120,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     124,     0,     0,     0,     0,     0,     0,   487,     0,     0,
       0,   120,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   605,     0,     0,
       0,     0,     0,     0,   124,     0,     0,     0,     0,     0,
       0,     0,   119,     0,     0,     0,     0,     0,   340,   340,
       0,   538,     0,     0,     0,   336,     0,     0,     0,     0,
     124,   173,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   173,   109,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   340,     0,     0,     0,     0,     0,
     120,   120,     0,     0,     0,     0,     0,     0,     0,   584,
       0,   109,   605,   269,     0,     0,   587,   589,     0,     0,
       0,   596,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   120,     0,     0,   119,   120,   114,   120,   124,
     124,   109,     0,     0,     0,     0,     0,     0,   340,   320,
       0,   120,   320,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   340,     0,     0,     0,     0,     0,     0,     0,
     119,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   124,     0,     0,     0,   124,     0,   124,   336,     0,
       0,     0,     0,     0,     0,     0,  1060,   340,     0,     0,
     124,     0,   340,   340,   119,     0,     0,   340,   340,     0,
       0,     0,     0,     0,     0,   120,     0,     0,   119,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   120,     0,
       0,   120,   120,     0,   120,     0,     0,     0,     0,     0,
     221,     0,     0,   120,     0,     0,   120,   120,   120,   119,
       0,     0,   797,   798,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   124,     0,     0,     0,     0,     0,
     114,     0,     0,  1128,  1129,     0,     0,   124,     0,     0,
     124,   124,     0,   124,  1187,  1188,  1189,     0,     0,  1191,
       0,     0,   124,     0,     0,   124,   124,   124,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   120,     0,     0,     0,
       0,     0,     0,   369,     0,     0,   370,   119,   371,     0,
     372,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   373,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1261,     0,     0,     0,   124,     0,     0,     0,     0,
       0,     0,     0,   320,   374,   375,     0,   376,   607,   377,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   378,
     379,   366,     0,   380,   381,   382,     0,   383,   384,     0,
       0,     0,     0,     0,     0,    73,   340,  1282,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1582,
    1583,  1584,     0,   932,   114,   385,  1750,     0,    76,   386,
       0,     0,     0,   120,     0,   387,    78,    79,   388,   389,
     390,   391,     0,     0,     0,     0,   120,   120,     0,     0,
       0,     0,   114,   607,  1306,     0,     0,     0,     0,     0,
       0,     0,     0,  1310,  1311,  1312,  1313,     0,     0,     0,
       0,  1318,  1319,     0,     0,     0,     0,     0,     0,     0,
       0,  1327,   124,     0,     0,     0,     0,     0,     0,     0,
       0,   119,   114,     0,     0,   124,   124,     0,     0,     0,
       0,     0,  1341,     0,  1342,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   119,
       0,     0,     0,     1,     0,     0,   146,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   340,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1398,     0,   119,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1062,     0,     0,     0,     0,     0,     0,  1074,
       0,     0,     0,  1412,     0,     0,     0,     0,     0,     0,
    1416,     0,  1418,  1420,     0,     0,     0,     0,     0,     0,
     201,     0,     0,     0,  1429,     0,  1430,     0,  1431,     0,
    1433,     0,     0,     0,     0,  1441,     0,    14,    15,    16,
      17,    18,     0,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,  1132,    46,     0,    47,     0,   291,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    58,  1486,     0,     0,
       0,     0,     0,   120,  1493,  1494,     0,     0,     0,     0,
       0,   120,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1517,     0,
       0,     0,     0,     0,     0,  1522,     0,     0,     0,  1523,
     120,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   124,     0,     0,     0,     0,     0,   120,     0,
     124,     0,     0,     0,     0,     0,     0,     0,     0,   223,
       0,     0,     0,     0,     0,     0,     0,     0,   120,     0,
       0,   291,     0,     0,     0,     0,     0,     0,     0,   124,
       0,     0,     0,     0,     0,   530,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   291,     0,   124,     0,  1616,
       0,     0,     0,     0,     0,   291,     0,     0,     0,     0,
       0,   120,     0,     0,     0,     0,     0,   124,     0,   561,
     565,     0,     0,     0,     0,     0,   572,   573,     0,     0,
       0,     0,     0,     0,     0,  1638,     0,     0,     0,     0,
       0,     0,   583,  1643,     0,  1645,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     124,     0,   602,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1676,  1677,     0,     0,   167,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1682,
    1683,     0,  1684,   120,   120,   120,   120,   120,   120,   695,
       0,  1688,     0,     0,   167,     0,     0,     0,  1379,  1381,
    1383,  1693,  1694,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   120,   120,     0,     0,     0,     0,     0,     0,
     736,     0,     0,     0,     0,     0,     0,     0,  1402,     0,
       0,     0,   124,   124,   124,   124,   124,   124,     0,     0,
     167,     0,     0,  1132,     0,     0,   774,     0,     0,     0,
     777,     0,     0,   167,     0,   167,     0,     0,     0,     0,
       0,   124,   124,     0,     0,     0,     0,     0,     0,   799,
       0,     0,     0,   800,   801,     0,     0,   804,     0,     0,
       0,     0,     0,     0,     0,     0,   360,  1447,     0,   120,
       0,     0,   818,   819,   820,   821,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     360,   843,     0,     0,  1780,  1781,     0,     0,     0,   846,
       0,     0,     0,     0,     0,     0,     0,  1789,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   124,     0,
       0,     0,     0,     0,     0,     0,     0,   291,   167,     0,
       0,     0,   167,     0,     0,   167,   167,     0,     0,   167,
       0,     0,   167,   167,  1813,  1814,     0,     0,     0,     0,
       0,   689,     0,     0,     0,     0,     0,     0,   884,     0,
       0,     0,   120,     0,     0,   561,     0,     0,     0,     0,
       0,   890,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   907,   912,   120,     0,  1561,
       0,     0,  1563,   167,     0,     0,   167,     0,     0,     0,
       0,   124,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   167,   167,     0,
       0,   120,  1881,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   167,     0,   120,   124,     0,     0,     0,
       0,     0,     0,  1891,     0,     0,  1892,  1893,     0,     0,
       0,     0,     0,  1895,     0,     0,   954,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   120,     0,     0,     0,
     124,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     849,   851,     0,     0,   124,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   124,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1017,     0,     0,   167,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1034,     0,     0,     0,  1035,     0,     0,     0,     0,     0,
       0,     0,     0,   907,   120,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1075,     0,     0,     0,     0,
       0,     0,     0,   360,  1084,  2007,     0,  1724,     0,     0,
    1087,     0,     0,     0,     0,   167,     0,     0,     0,     0,
       0,     0,     0,   124,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   689,     0,
       0,     0,     0,     0,   689,     0,     0,     1,     0,     0,
       0,     0,     0,   689,     1,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  2047,
       0,     0,   689,     0,     0,     0,     0,     0,     0,     0,
       0,     1,     0,     0,     0,     0,     0,     0,     0,   360,
       0,     0,     0,     0,  2065,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1009,  2074,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  2087,     0,     0,     0,     0,
     167,   167,     0,     0,     0,     0,     0,  1241,   120,     0,
       0,     0,     0,   167,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   120,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   124,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   120,     0,     0,     0,
       0,     0,     0,   347,     0,   124,     0,     0,     0,     0,
    1289,     0,  1877,     0,  1290,     0,     0,     0,     0,     0,
       0,   907,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1303,     0,     0,     0,     0,     0,     0,  1304,   445,
     347,     0,     0,     0,     0,   124,     0,  1308,     0,  1309,
       0,     0,     0,     0,     0,     0,   167,   167,     0,     0,
       0,     0,   167,     0,     0,     0,     0,     0,     0,     0,
       0,   511,     0,     0,     0,     0,     0,     0,   511,     0,
       0,  1337,     0,   167,     0,  1338,   167,   167,     0,   167,
     369,   167,   167,   370,     0,   371,     0,   372,     0,   146,
       0,     0,     1,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   373,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     167,     0,     0,     0,   167,     0,     0,     0,     0,     0,
       0,   374,   375,     0,   376,   511,   377,    63,    64,    65,
      66,    67,    68,    69,    70,    71,   378,   379,   366,     0,
     380,   381,   382,     0,   383,   384,     0,     0,     0,   347,
     617,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   209,     0,     0,     0,     0,
     638,     0,   385,  1428,  1839,    76,   386,     0,     0,   167,
     167,   263,   387,    78,    79,   388,   389,   390,   391,     0,
       0,     0,     0,   167,     0,     0,     0,     0,  1453,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   209,     0,     0,     0,   311,     0,     0,     0,     0,
       0,   511,     0,     0,     0,     0,   352,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   511,   770,     0,
     511,   773,     0,     0,     0,     0,     0,     0,   347,     0,
     209,     0,   617,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   465,     0,     0,   469,     0,     0,     0,
       0,     0,  1526,     0,     0,     0,  1527,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   511,     0,     0,     0,   511,     0,     0,
       0,     0,     0,     0,   167,     0,  1562,     0,     0,     0,
       0,     0,     0,     0,     0,   209,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   263,   347,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   167,     0,     0,     0,     0,
       0,     0,   167,     0,     0,   167,  1626,     0,     0,  1629,
       0,     0,     0,     0,   469,     0,     0,     0,     0,     0,
       0,     0,   209,     0,     0,     0,     0,  1640,     0,   511,
       0,     0,   347,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   610,     0,   627,     0,     0,     0,     0,     0,
     902,   347,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   617,     0,     0,     0,   617,     0,     0,     0,     0,
       0,     0,   920,     0,   347,     0,     0,     0,     0,     0,
    1675,     0,     0,     0,     0,     0,     0,     0,     0,  1680,
       0,     0,     0,  1681,     0,     0,     0,   693,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1685,  1686,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   167,     0,
       0,   209,     0,     0,     0,     0,   167,   167,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1571,
       0,     0,  1574,  1588,     0,     0,     0,     0,  1595,     0,
       0,   610,  1599,     0,  1601,     0,     0,   794,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   347,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   167,     0,     0,     0,     0,
       0,   511,   511,     0,   167,     0,     0,   167,     0,   167,
     167,   511,  1030,     0,   511,  1033,     0,     0,     0,     0,
       0,     0,     0,  1774,  1775,     0,   347,     0,     0,   617,
       0,   617,   617,     0,     0,     0,   209,   209,   617,     0,
       0,     0,   465,     0,     0,     0,     0,     0,   347,   347,
     167,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   347,     0,     0,
       0,   511,     0,     0,     0,   511,     0,     0,     0,   511,
    1101,     0,     0,   511,  1105,     0,     0,     0,     0,     0,
       0,  1108,     0,     0,     0,   352,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1695,     0,
       0,     0,     0,     0,   465,     0,   906,   167,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   347,   511,     0,     0,   610,     0,     0,
    1728,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1734,     0,     0,  1866,     0,     0,     0,     0,
       0,   209,     0,     0,   617,     0,     0,  1749,  1751,     0,
       0,     0,     0,     0,   693,     0,     0,   693,   693,     0,
     693,     0,  1629,     0,     0,     0,     0,     0,     0,   693,
       0,  1574,   693,   693,   693,     0,     0,     0,     0,     0,
       0,  1894,     0,   347,     0,   167,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1912,     0,
       0,     0,     0,     0,   167,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   465,     0,
       0,     0,     0,     0,     0,  1940,     0,     0,  1941,     0,
     167,     0,     0,     0,     0,     0,   167,   421,     0,     0,
     511,     0,   209,     0,     0,     0,     0,     0,     0,     0,
     450,     0,     0,     0,     0,     0,     0,   617,   617,   465,
       0,     0,     0,   478,   617,   478,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1840,     0,     0,
       0,   465,   465,     0,     0,     0,  1843,     0,  1845,     0,
       0,  1850,  1854,     0,  1588,     0,     0,     0,     0,  1860,
     465,     0,     0,     0,     0,   167,   347,     0,     0,     0,
       0,   511,  1332,     0,   511,  1336,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  2027,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     578,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   465,     0,     0,     0,
       0,     0,     0,   209,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   794,   167,   167,     0,
       0,     0,     0,     0,     0,   360,     0,     0,  1929,   167,
       0,     0,     0,  1934,  1936,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   369,     0,     0,
     370,   347,   371,     0,   372,     0,   352,   617,  1437,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   373,     0,     0,     0,     0,     0,     0,     0,     0,
     347,     0,     0,     0,     0,     0,  1986,     0,  1989,     0,
       0,  1991,  1993,     0,     0,     0,  1996,  1998,   374,   375,
       0,   376,     0,   377,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   378,   379,   366,     0,   380,   381,   382,
       0,   383,   384,   167,   511,  1490,     0,     0,     0,    73,
       0,     0,     0,   511,  1499,     0,   617,     0,     0,     0,
       0,     0,     0,     0,   478,     0,     0,   347,   347,   385,
     478,     0,    76,   386,     0,   815,     0,   475,     0,   387,
      78,    79,   388,   389,   390,   391,     0,  2041,  2043,  2045,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  2058,   465,
       0,     0,     0,     0,     0,     0,     0,     0,   167,     0,
       0,     0,  2069,  2071,  2073,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   693,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   883,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   347,     0,     0,
       0,   450,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   263,   914,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   617,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   209,     0,     0,     0,   167,     0,
       0,     0,     0,     0,   610,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   948,
       0,     0,     0,   352,     0,     0,     0,   693,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   815,   968,     0,     0,   970,     0,   972,
       0,     0,     0,     0,     0,   981,     0,   986,   981,   511,
       0,     0,     0,     0,     0,     0,     0,     0,  1221,     0,
       0,     0,     0,     0,     0,   511,    14,    15,    16,    17,
      18,     0,     0,     0,     0,  1014,     0,     0,     0,     0,
     465,   465,     0,     0,     0,     0,     0,     0,  1016,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1025,
       0,     0,   369,     0,     0,   370,     0,   371,     0,   372,
       0,     0,     0,   450,     0,     0,  1014,     0,     0,   693,
     693,   693,     0,   693,   693,    58,   373,     0,     0,     0,
     469,     0,     0,     0,     0,   347,     0,     0,     0,     0,
       0,     0,     0,  1078,     0,     0,   478,     0,     0,     0,
       0,     0,     0,   374,   375,     0,   376,     0,   377,    63,
      64,    65,    66,    67,    68,    69,    70,    71,   378,   379,
     366,     0,   380,   381,   382,     0,   383,   384,   263,     0,
       0,     0,  1109,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   347,   347,     0,     0,   369,     0,
     352,   370,     0,   371,   385,   372,     0,    76,   386,   511,
     511,     0,     0,     0,   387,   448,    79,   388,   389,   390,
     391,     0,   373,     0,     0,   511,     0,     0,     0,     0,
     421,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1213,  1215,     0,     0,     0,     0,   374,
     375,   450,   376,     0,   377,    63,    64,    65,    66,    67,
      68,    69,    70,    71,   378,   379,   366,     0,   380,   381,
     382,     0,   383,   384,     0,     0,     0,     0,  1439,   981,
      73,     0,     0,     0,     0,     0,    14,    15,    16,    17,
      18,     0,     0,  1014,     0,     0,     0,     0,     0,     0,
     385,  1254,     0,    76,   386,     0,     0,  1008,   981,     0,
     387,    78,    79,   388,   389,   390,   391,     0,   209,     0,
       0,     0,   369,     0,     0,   370,     0,   371,     0,   372,
     511,     0,     0,     0,     0,     0,     0,     0,   511,     0,
       0,     0,     0,     0,     0,    58,   373,     0,     0,     0,
       0,     0,   263,     0,   478,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   374,   375,     0,   376,     0,   377,    63,
      64,    65,    66,    67,    68,    69,    70,    71,   378,   379,
     366,     0,   380,   381,   382,     0,   383,   384,   352,     0,
       0,     0,     0,   347,    73,     0,     0,   511,  1972,     0,
       0,   511,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   478,     0,  1322,   385,  1325,     0,    76,   386,     0,
       0,     0,   693,     0,   387,  1440,    79,   388,   389,   390,
     391,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   511,     0,     0,     0,     0,   465,   465,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1390,  1390,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   251,     0,     0,     0,     0,   263,     0,     0,
       0,     0,    14,    15,    16,    17,    18,   511,   511,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,  -458,  -458,     0,  -458,
      46,     0,    47,     0,  -458,  1432,     0,     0,     0,   511,
       0,  1442,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    58,     0,     0,     0,     0,     0,     0,     0,     0,
     450,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   478,     0,     0,
       0,     0,     0,     0,     0,    63,    64,     0,     0,     0,
       0,     0,   981,     0,     0,   815,     0,     0,     0,   369,
       0,     0,   370,     0,   371,     0,   372,     0,     0,     0,
      73,     0,     0,     0,     0,     0,     0,     0,   693,     0,
       0,     0,     0,   373,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    76,   309,     0,     0,     0,     0,     0,
       0,    78,    79,     0,     0,     0,   465,  1525,     0,     0,
     374,   375,     0,   472,     0,   377,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   378,   379,   366,     0,   380,
     381,   382,     0,   383,   384,     0,     0,     0,     0,     0,
       0,    73,     0,     0,     0,   981,     0,     0,     0,     0,
       0,     0,     0,   693,     0,     0,   469,     0,     0,     0,
       0,   385,    75,   478,   473,   474,   815,     0,     0,   475,
       0,   387,    78,    79,   388,   389,   390,   391,  2088,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1374,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   968,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1641,  1642,     0,
       0,     0,     0,     0,     0,     0,   369,   478,     0,   370,
       0,   371,     0,   372,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   478,     0,   815,  1134,     0,
     373,    -2,     0,  1136,  -243,  -243,  1137,  1138,  1139,  1140,
    1141,  1142,  1143,  1144,  1145,  1146,  1147,  1148,  -337,  1149,
    1150,  1151,  1152,  1153,     0,  1154,     0,   374,   375,     0,
     472,     0,   377,  1155,  1156,    65,    66,    67,    68,    69,
      70,    71,   378,   379,   366,  1157,   380,   381,   382,     0,
     383,   384,     0,     0,     0,     0,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,  2088,     0,     0,     0,
       0,     0,     0,     0,     0,   421,     0,  -243,   385,     0,
    1717,    76,   386,  1374,     0,     0,   287,     0,   387,    78,
      79,   388,   389,   390,   391,     0,     0,     0,     0,     0,
       0,     0,     0,  -184,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   369,     0,     0,   370,     0,   371,
       0,   372,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1134,     0,   373,    -2,
    1763,  1136,  -244,  -244,  1137,  1138,  1139,  1140,  1141,  1142,
    1143,  1144,  1145,  1146,  1147,  1148,  -337,  1149,  1150,  1151,
    1152,  1153,     0,  1154,     0,   374,   375,     0,   472,     0,
     377,  1155,  1156,    65,    66,    67,    68,    69,    70,    71,
     378,   379,   366,  1157,   380,   381,   382,  1791,   383,   384,
    1793,  1770,     0,     0,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1374,     0,
       0,     0,     0,     0,     0,  -244,   385,     0,     0,    76,
     386,     0,     0,     0,   287,     0,   387,    78,    79,   388,
     389,   390,   391,     0,     0,     0,     0,     0,     0,   369,
       0,  -184,   370,     0,   371,     0,   372,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1134,     0,   373,    -2,     0,  1136,     0,     0,  1137,
    1138,  1139,  1140,  1141,  1142,  1143,  1144,  1145,  1146,  1147,
    1148,  -337,  1149,  1150,  1151,  1152,  1153,     0,  1154,     0,
     374,   375,     0,   472,     0,   377,  1155,  1156,    65,    66,
      67,    68,    69,    70,    71,   378,   379,   366,  1157,   380,
     381,   382,     0,   383,   384,     0,     0,     0,     0,     0,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   385,     0,     0,    76,   386,     0,     0,     0,   287,
       0,   387,    78,    79,   388,   389,   390,   391,     0,     0,
       0,     0,     0,     0,     0,     0,  -184,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     4,   181,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
    1133,     0,    20,   981,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,   369,     0,    46,   370,    47,   371,     0,   372,    48,
      49,    50,    51,    52,    53,    54,    55,    56,     0,     0,
       0,    57,     0,  1134,    58,  1135,    -2,     0,  1136,     0,
       0,  1137,  1138,  1139,  1140,  1141,  1142,  1143,  1144,  1145,
    1146,  1147,  1148,  -337,  1149,  1150,  1151,  1152,  1153,     0,
    1154,     0,   374,   375,    61,   472,     0,   377,  1155,  1156,
      65,    66,    67,    68,    69,    70,    71,   378,   379,   366,
    1157,   380,   381,   382,     0,   383,   384,     0,     0,     0,
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    -3,   385,     0,     0,    76,   417,     0,     0,
       0,   287,     0,   387,    78,    79,   388,   389,   390,   391,
       0,     0,     0,     0,     0,     0,     0,     0,  -184,     4,
     181,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,  1133,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,   369,     0,    46,   370,    47,
     371,     0,   372,    48,    49,    50,    51,    52,    53,    54,
      55,    56,     0,     0,     0,    57,     0,  1134,    58,  1135,
      -2,     0,  1136,     0,     0,  1137,  1138,  1139,  1140,  1141,
    1142,  1143,  1144,  1145,  1146,  1147,  1148,  -337,  1149,  1150,
    1151,  1152,  1153,     0,  1154,     0,   374,   375,    61,   472,
       0,   377,  1155,  1156,    65,    66,    67,    68,    69,    70,
      71,   378,   379,   366,  1157,   380,   381,   382,     0,   383,
     384,     0,     0,     0,     0,     0,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   385,     0,     0,
      76,   417,     0,     0,     0,   287,     0,   387,    78,    79,
     388,   389,   390,   391,     0,     0,     0,     0,     0,     0,
       0,     0,  -184,     4,   181,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,   369,
       0,    46,   370,    47,   371,     0,   372,    48,    49,    50,
      51,    52,    53,    54,    55,    56,     0,     0,     0,    57,
       0,     0,    58,   373,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     374,   375,    61,   376,     0,   377,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   378,   379,   366,     0,   380,
     381,   382,     0,   383,   384,     0,     0,     0,     0,     0,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1582,  1583,  1584,     0,     0,
       0,   385,  1585,  1586,    76,   417,     0,     0,     0,     0,
       0,   387,    78,    79,   388,   389,   390,   391,     0,     0,
       0,     0,     0,     0,     0,     0,  1587,     4,   181,     6,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1582,
    1583,  1584,     0,     0,     0,   385,  1585,     0,    76,   417,
       0,     0,     0,     0,     0,   387,    78,    79,   388,   389,
     390,   391,     0,     0,     0,     0,     0,     0,     0,     0,
    1587,     4,   181,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,   369,     0,    46,
     370,    47,   371,     0,   372,    48,    49,    50,    51,    52,
      53,    54,    55,    56,     0,     0,     0,    57,     0,     0,
      58,   373,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   374,   375,
      61,   376,     0,   377,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   378,   379,   366,     0,   380,   381,   382,
       0,   383,   384,     0,     0,     0,     0,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   385,
       0,  1573,    76,   417,     0,     0,     0,     0,     0,   387,
      78,    79,   388,   389,   390,   391,     4,   181,     6,     7,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   385,     0,     0,    76,   417,     0,
       0,     0,     0,     0,   387,    78,    79,   388,   389,   390,
     391,   181,     6,     7,     8,     9,    10,    11,    12,    13,
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
       0,    76,   447,     0,     0,     0,     0,     0,   387,   448,
      79,   388,   389,   390,   391,   181,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
       0,    20,     0,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,     0,     0,
     369,     0,    46,   370,    47,   371,     0,   372,   327,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,     0,
       0,     0,     0,    58,   373,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   374,   375,     0,   376,     0,   377,    63,    64,    65,
      66,    67,    68,    69,    70,    71,   378,   379,   366,     0,
     380,   381,   382,     0,   383,   384,     0,     0,     0,     0,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   385,     0,     0,    76,  1210,     0,     0,     0,
       0,     0,   387,  1211,    79,   388,   389,   390,   391,   181,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,     0,    20,     0,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,     0,     0,   369,     0,    46,   370,    47,   371,
       0,   372,   327,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,     0,     0,     0,     0,    58,   373,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   374,   375,     0,   376,     0,
     377,    63,    64,    65,    66,    67,    68,    69,    70,    71,
     378,   379,   366,     0,   380,   381,   382,     0,   383,   384,
       0,     0,     0,     0,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   385,     0,     0,    76,
     386,     0,     0,     0,     0,     0,   387,    78,    79,   388,
     389,   390,   391,   181,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,     0,    20,
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
     385,     0,     0,    76,   447,     0,     0,     0,     0,     0,
     387,    78,    79,   388,   389,   390,   391,  1911,     0,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,     0,    -2,     0,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,     0,    -2,    -2,     0,    -2,
       0,    -2,     0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,     0,     0,     0,    -2,     0,     0,    -2,     0,
       0,     0,     0,    -2,    -2,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    -2,     0,
       0,     0,    -2,    -2,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    -2,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    -2,     0,     0,     0,
      -2,    -2,     0,     0,     0,     0,     0,     0,    -2,    -2,
    1939,     0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,    -2,
       0,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
      -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,     0,    -2,
      -2,     0,    -2,     0,    -2,     0,    -2,    -2,    -2,    -2,
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
       0,    -2,    -2,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,    48,    49,    50,
      51,    52,    53,    54,    55,    56,     0,     0,     0,    57,
       0,     0,    58,    59,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    60,     0,
       0,     0,    61,    62,     0,     0,    63,    64,    65,    66,
      67,    68,    69,    70,    71,     0,     0,     0,    72,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    74,    75,     0,    76,    77,     0,     0,     0,     0,
       0,     0,    78,    79,   251,   181,     6,     7,     8,     9,
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
    -782,     0,     0,    78,    79,     4,   181,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,    56,     0,     0,
       0,    57,     0,     0,    58,     0,     0,     0,     0,  -390,
    -390,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    61,     0,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -390,     0,     0,     0,    76,    77,     0,     0,
       0,     0,     0,     0,    78,    79,     4,   181,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,     0,    20,     0,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
       0,     0,     0,     0,    46,     0,    47,     0,     0,     0,
      48,    49,    50,    51,    52,    53,    54,    55,    56,     0,
       0,     0,    57,     0,     0,    58,     0,     0,     0,     0,
    -391,  -391,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    61,     0,     0,     0,    63,
      64,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -391,     0,     0,     0,    76,    77,     0,
       0,     0,     0,     0,     0,    78,    79,   251,   181,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,     0,    20,     0,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,  -458,  -458,     0,  -458,    46,     0,    47,     0,  -458,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   147,     0,     0,
      63,    64,    65,    66,    67,    68,    69,    70,    71,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,    75,     0,    76,   252,
       0,  1350,     0,     0,     0,     0,    78,    79,  1351,     0,
       0,    14,    15,    16,    17,    18,    19,     0,    20,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,     0,     0,     0,     0,    46,
       0,    47,     0,     0,     0,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,     0,     0,     0,     0,
      58,  1352,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      61,     0,     0,     0,    63,    64,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1353,     0,
       0,     0,    76,   944,     0,  1350,     0,     0,     0,     0,
      78,    79,  1351,     0,     0,    14,    15,    16,    17,    18,
      19,     0,    20,     0,    21,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     0,
       0,     0,     0,    46,     0,    47,     0,     0,     0,    48,
      49,    50,    51,    52,    53,    54,    55,     0,     0,     0,
       0,     0,     0,     0,    58,  1352,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    61,     0,     0,     0,    63,    64,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1537,     0,     0,     0,    76,   944,     0,  1350,
       0,     0,     0,     0,    78,    79,  1351,     0,     0,    14,
      15,    16,    17,    18,    19,     0,    20,     0,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,     0,     0,     0,     0,    46,     0,    47,
       0,     0,     0,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,     0,     0,     0,     0,    58,  1352,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    61,     0,
       0,     0,    63,    64,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1538,     0,     0,     0,
      76,   944,     0,  1350,     0,     0,     0,     0,    78,    79,
    1351,     0,     0,    14,    15,    16,    17,    18,    19,     0,
      20,     0,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,     0,     0,     0,
       0,    46,     0,    47,     0,     0,     0,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,     0,     0,
       0,     0,    58,  1352,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    61,     0,     0,     0,    63,    64,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1539,     0,     0,     0,    76,   944,     0,     0,     0,     0,
       0,     0,    78,    79,   251,   181,     6,     7,     8,     9,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    76,   252,     0,     0,     0,
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
       0,     0,    73,     0,  1809,     0,     0,     0,     0,     0,
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
       0,     0,    73,     0,  1811,     0,     0,     0,     0,     0,
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
       0,     0,  1374,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   369,     0,     0,   370,     0,   371,     0,
     372,     0,     0,     0,     0,    76,   252,     0,     0,     0,
       0,     0,     0,    78,    79,  1134,     0,   373,     0,     0,
    1136,  1833,  1834,  1137,  1138,  1139,  1140,  1141,  1142,  1143,
    1144,  1145,  1146,  1147,  1148,  -337,  1149,  1150,  1151,  1152,
    1153,     0,  1154,     0,   374,   375,     0,   472,     0,   377,
    1155,  1156,    65,    66,    67,    68,    69,    70,    71,   378,
     379,   366,  1157,   380,   381,   382,  1374,   383,   384,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   385,     0,   369,    76,   386,
     370,     0,   371,   287,   372,   387,    78,    79,   388,   389,
     390,   391,     0,     0,     0,     0,     0,     0,     0,  1134,
    -184,   373,     0,     0,  1136,     0,     0,  1137,  1138,  1139,
    1140,  1141,  1142,  1143,  1144,  1145,  1146,  1147,  1148,  -337,
    1149,  1150,  1151,  1152,  1153,     0,  1154,     0,   374,   375,
       0,   472,     0,   377,  1155,  1156,    65,    66,    67,    68,
      69,    70,    71,   378,   379,   366,  1157,   380,   381,   382,
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
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1725,
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
      58,     0,     0,     0,     0,     0,  1457,     0,     0,     0,
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
      47,     0,    63,    64,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      76,     0,     0,    63,    64,     0,     0,     0,    78,    79,
      14,    15,    16,    17,    18,    19,     0,    20,     0,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,     0,     0,     0,     0,    46,     0,
      47,    76,     0,     0,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,     0,     0,     0,     0,    58,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     147,     0,     0,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    14,    15,    16,    17,    18,    19,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,     0,     0,     0,     0,
      46,    76,    47,     0,     0,     0,   327,    49,    50,    51,
      52,    53,    54,    55,     0,   369,     0,     0,   370,     0,
     371,    58,   372,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   373,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   374,   375,     0,   376,
       0,   377,  1848,    64,    65,    66,    67,    68,    69,    70,
      71,   378,   379,   366,     0,   380,   381,   382,     0,   383,
     384,   369,     0,     0,   370,     0,   371,    73,   372,     0,
       0,     0,     0,    76,     0,     0,     0,     0,     0,     0,
       0,  1582,  1583,  1584,     0,   373,     0,   385,  1849,     0,
      76,   386,     0,     0,     0,     0,     0,   387,    78,    79,
     388,   389,   390,   391,     0,     0,     0,     0,     0,     0,
       0,     0,   374,   375,     0,   376,     0,   377,    63,    64,
      65,    66,    67,    68,    69,    70,    71,   378,   379,   366,
       0,   380,   381,   382,   369,   383,   384,   370,     0,   371,
       0,   372,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   373,     0,
       0,     0,     0,   385,  1257,     0,    76,   386,     0,     0,
       0,  1258,     0,   387,    78,    79,   388,   389,   390,   391,
       0,     0,     0,     0,     0,   374,   375,     0,   376,     0,
     377,    63,    64,    65,    66,    67,    68,    69,    70,    71,
     378,   379,   366,     0,   380,   381,   382,   369,   383,   384,
     370,     0,   371,     0,   372,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   373,     0,     0,     0,     0,   385,   977,  1564,    76,
     386,     0,     0,     0,     0,     0,   387,    78,    79,   388,
     389,   390,   391,     0,     0,     0,     0,     0,   374,   375,
       0,   376,     0,   377,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   378,   379,   366,     0,   380,   381,   382,
     369,   383,   384,   370,     0,   371,     0,   372,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   373,     0,     0,     0,     0,   385,
     814,     0,    76,   386,     0,     0,     0,     0,     0,   387,
      78,    79,   388,   389,   390,   391,     0,     0,     0,     0,
       0,   374,   375,     0,   376,     0,   377,    63,    64,    65,
      66,    67,    68,    69,    70,    71,   378,   379,   366,     0,
     380,   381,   382,   369,   383,   384,   370,     0,   371,     0,
     372,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   373,     0,     0,
       0,     0,   385,     0,     0,    76,   386,     0,     0,     0,
     287,     0,   387,    78,    79,   388,   389,   390,   391,     0,
       0,     0,     0,     0,   374,   375,     0,   376,     0,   377,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   378,
     379,   366,     0,   380,   381,   382,   369,   383,   384,   370,
       0,   371,     0,   372,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     373,     0,     0,     0,     0,   385,   977,     0,    76,   386,
       0,     0,     0,     0,     0,   387,    78,    79,   388,   389,
     390,   391,     0,     0,     0,     0,     0,   374,   375,     0,
     376,     0,   377,    63,    64,    65,    66,    67,    68,    69,
      70,    71,   378,   379,   366,     0,   380,   381,   382,   369,
     383,   384,   370,     0,   371,     0,   372,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   373,     0,     0,     0,     0,   385,  1324,
       0,    76,   386,     0,     0,     0,     0,     0,   387,    78,
      79,   388,   389,   390,   391,     0,     0,     0,     0,     0,
     374,   375,     0,   376,     0,   377,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   378,   379,   366,     0,   380,
     381,   382,   369,   383,   384,   370,     0,   371,     0,   372,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   373,     0,     0,     0,
       0,   385,     0,     0,    76,   386,     0,     0,     0,  1384,
       0,   387,    78,    79,   388,   389,   390,   391,     0,     0,
       0,     0,     0,   374,   375,     0,   376,     0,   377,    63,
      64,    65,    66,    67,    68,    69,    70,    71,   378,   379,
     366,     0,   380,   381,   382,   369,   383,   384,   370,     0,
     371,     0,   372,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   373,
       0,     0,     0,     0,   385,     0,     0,    76,   386,     0,
       0,     0,  1448,     0,   387,    78,    79,   388,   389,   390,
     391,     0,     0,     0,     0,     0,   374,   375,     0,   376,
       0,   377,    63,    64,    65,    66,    67,    68,    69,    70,
      71,   378,   379,   366,     0,   380,   381,   382,   369,   383,
     384,   370,     0,   371,     0,   372,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   373,     0,     0,     0,     0,   385,  1844,     0,
      76,   386,     0,     0,     0,     0,     0,   387,    78,    79,
     388,   389,   390,   391,     0,     0,     0,     0,     0,   374,
     375,     0,   376,     0,   377,    63,    64,    65,    66,    67,
      68,    69,    70,    71,   378,   379,   366,     0,   380,   381,
     382,   369,   383,   384,   370,     0,   371,     0,   372,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   373,     0,     0,     0,     0,
     385,  1853,     0,    76,   386,     0,     0,     0,     0,     0,
     387,    78,    79,   388,   389,   390,   391,     0,     0,     0,
       0,     0,   374,   375,     0,   376,     0,   377,    63,    64,
      65,    66,    67,    68,    69,    70,    71,   378,   379,   366,
       0,   380,   381,   382,   369,   383,   384,   370,     0,   371,
       0,   372,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   373,     0,
       0,     0,     0,   385,  1933,     0,    76,   386,     0,     0,
       0,     0,     0,   387,    78,    79,   388,   389,   390,   391,
       0,     0,     0,     0,     0,   374,   375,     0,   376,     0,
     377,    63,    64,    65,    66,    67,    68,    69,    70,    71,
     378,   379,   366,     0,   380,   381,   382,   369,   383,   384,
     370,     0,   371,     0,   372,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   373,     0,     0,     0,     0,   385,  1935,     0,    76,
     386,     0,     0,     0,     0,     0,   387,    78,    79,   388,
     389,   390,   391,     0,     0,     0,     0,     0,   374,   375,
       0,   376,     0,   377,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   378,   379,   366,     0,   380,   381,   382,
     369,   383,   384,   370,     0,   371,     0,   372,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   373,     0,     0,     0,     0,   385,
    1988,     0,    76,   386,     0,     0,     0,     0,     0,   387,
      78,    79,   388,   389,   390,   391,     0,     0,     0,     0,
       0,   374,   375,     0,   376,     0,   377,    63,    64,    65,
      66,    67,    68,    69,    70,    71,   378,   379,   366,     0,
     380,   381,   382,   369,   383,   384,   370,     0,   371,     0,
     372,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   373,     0,     0,
       0,     0,   385,  1990,     0,    76,   386,     0,     0,     0,
       0,     0,   387,    78,    79,   388,   389,   390,   391,     0,
       0,     0,     0,     0,   374,   375,     0,   376,     0,   377,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   378,
     379,   366,     0,   380,   381,   382,   369,   383,   384,   370,
       0,   371,     0,   372,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     373,     0,     0,     0,     0,   385,  1992,     0,    76,   386,
       0,     0,     0,     0,     0,   387,    78,    79,   388,   389,
     390,   391,     0,     0,     0,     0,     0,   374,   375,     0,
     376,     0,   377,    63,    64,    65,    66,    67,    68,    69,
      70,    71,   378,   379,   366,     0,   380,   381,   382,   369,
     383,   384,   370,     0,   371,     0,   372,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   373,     0,     0,     0,     0,   385,  1995,
       0,    76,   386,     0,     0,     0,     0,     0,   387,    78,
      79,   388,   389,   390,   391,     0,     0,     0,     0,     0,
     374,   375,     0,   376,     0,   377,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   378,   379,   366,     0,   380,
     381,   382,   369,   383,   384,   370,     0,   371,     0,   372,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   373,     0,     0,     0,
       0,   385,  1997,     0,    76,   386,     0,     0,     0,     0,
       0,   387,    78,    79,   388,   389,   390,   391,     0,     0,
       0,     0,     0,   374,   375,     0,   376,     0,   377,    63,
      64,    65,    66,    67,    68,    69,    70,    71,   378,   379,
     366,     0,   380,   381,   382,   369,   383,   384,   370,     0,
     371,     0,   372,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   373,
       0,     0,     0,     0,   385,  2040,     0,    76,   386,     0,
       0,     0,     0,     0,   387,    78,    79,   388,   389,   390,
     391,     0,     0,     0,     0,     0,   374,   375,     0,   376,
       0,   377,    63,    64,    65,    66,    67,    68,    69,    70,
      71,   378,   379,   366,     0,   380,   381,   382,   369,   383,
     384,   370,     0,   371,     0,   372,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   373,     0,     0,     0,     0,   385,  2042,     0,
      76,   386,     0,     0,     0,     0,     0,   387,    78,    79,
     388,   389,   390,   391,     0,     0,     0,     0,     0,   374,
     375,     0,   376,     0,   377,    63,    64,    65,    66,    67,
      68,    69,    70,    71,   378,   379,   366,     0,   380,   381,
     382,   369,   383,   384,   370,     0,   371,     0,   372,     0,
      73,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   373,     0,     0,     0,     0,
     385,  2044,     0,    76,   386,     0,     0,     0,     0,     0,
     387,    78,    79,   388,   389,   390,   391,     0,     0,     0,
       0,     0,   374,   375,     0,   376,     0,   377,    63,    64,
      65,    66,    67,    68,    69,    70,    71,   378,   379,   366,
       0,   380,   381,   382,   369,   383,   384,   370,     0,   371,
       0,   372,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   373,     0,
       0,     0,     0,   385,  2068,     0,    76,   386,     0,     0,
       0,     0,     0,   387,    78,    79,   388,   389,   390,   391,
       0,     0,     0,     0,     0,   374,   375,     0,   376,     0,
     377,    63,    64,    65,    66,    67,    68,    69,    70,    71,
     378,   379,   366,     0,   380,   381,   382,   369,   383,   384,
     370,     0,   371,     0,   372,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   373,     0,     0,     0,     0,   385,  2070,     0,    76,
     386,     0,     0,     0,     0,     0,   387,    78,    79,   388,
     389,   390,   391,     0,     0,     0,     0,     0,   374,   375,
       0,   376,     0,   377,    63,    64,    65,    66,    67,    68,
      69,    70,    71,   378,   379,   366,     0,   380,   381,   382,
     369,   383,   384,   370,     0,   371,     0,   372,     0,    73,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   373,     0,     0,     0,     0,   385,
    2072,     0,    76,   386,     0,     0,     0,     0,     0,   387,
      78,    79,   388,   389,   390,   391,     0,     0,     0,     0,
       0,   374,   375,     0,   376,     0,   377,    63,    64,    65,
      66,    67,    68,    69,    70,    71,   378,   379,   366,     0,
     380,   381,   382,   369,   383,   384,   370,     0,   371,     0,
     372,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   373,     0,     0,
       0,     0,   385,     0,     0,    76,   386,     0,     0,     0,
       0,     0,   387,    78,    79,   388,   389,   390,   391,     0,
       0,     0,     0,     0,   374,   375,     0,   376,     0,   377,
      63,    64,    65,    66,    67,    68,    69,    70,    71,   378,
     379,   366,     0,   380,   381,   382,   369,   383,   384,   370,
       0,   371,     0,   372,     0,    73,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     373,     0,     0,     0,     0,   662,     0,     0,    76,   386,
       0,     0,     0,     0,     0,   387,    78,    79,   388,   389,
     390,   391,     0,     0,     0,     0,     0,   374,   375,     0,
     376,     0,   377,    63,    64,    65,    66,    67,    68,    69,
      70,    71,   378,   379,   366,     0,   380,   381,   382,   369,
     383,   384,   370,     0,   371,     0,   372,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   373,     0,     0,     0,     0,   668,     0,
       0,    76,   386,     0,     0,     0,     0,     0,   387,    78,
      79,   388,   389,   390,   391,     0,     0,     0,     0,     0,
     374,   375,     0,   376,     0,   377,    63,    64,    65,    66,
      67,    68,    69,    70,    71,   378,   379,   366,     0,   380,
     381,   382,   369,   383,   384,   370,     0,   371,     0,   372,
       0,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   373,     0,     0,     0,
       0,   677,     0,     0,    76,   386,     0,     0,     0,     0,
       0,   387,    78,    79,   388,   389,   390,   391,     0,     0,
       0,     0,     0,   374,   375,     0,   376,     0,   377,    63,
      64,    65,    66,    67,    68,    69,    70,    71,   378,   379,
     366,     0,   380,   381,   382,   369,   383,   384,   370,     0,
     371,     0,   372,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   373,
       0,     0,     0,     0,   385,     0,     0,    76,   386,     0,
       0,     0,     0,     0,   387,   882,    79,   388,   389,   390,
     391,     0,     0,     0,     0,     0,   374,   375,     0,   376,
       0,   377,    63,    64,    65,    66,    67,    68,    69,    70,
      71,   378,   379,   366,     0,   380,   381,   382,   369,   383,
     384,   370,     0,   371,     0,   372,     0,    73,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   373,     0,     0,     0,     0,   385,     0,     0,
      76,   386,     0,     0,     0,     0,     0,   387,   448,    79,
     388,   389,   390,   391,     0,     0,     0,     0,     0,   374,
     375,     0,   376,     0,   377,  1928,    64,    65,    66,    67,
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
       0,     0,     0,     0,    46,     0,    47,     0,   181,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,     0,     0,    20,    58,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,     0,     0,     0,     0,    46,   182,    47,     0,   183,
     184,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    58,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   671,     0,     0,
     672,   673,    14,    15,    16,    17,    18,     0,     0,    20,
       0,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,  -457,  -457,     0,  -457,
      46,     0,    47,     0,  -457,     0,     0,     0,     0,     0,
       0,     0,     0,    14,    15,    16,    17,    18,     0,     0,
      20,    58,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,  -458,  -458,     0,
    -458,    46,     0,    47,     0,  -458,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    58
};

static const yytype_int16 yycheck[] =
{
       1,   152,     1,     4,     1,    74,    96,   175,   709,   291,
     228,   180,   164,   216,   229,   264,    76,    74,   330,    74,
      74,   179,   250,     1,     4,   891,   625,    59,   616,   332,
     164,  1700,   475,   228,   385,   347,   694,   387,  1701,   351,
     524,   525,  1700,   228,   228,   687,  1769,    74,   359,   164,
    1700,  1143,   151,   877,     1,    56,    57,     1,    59,     1,
      59,   780,    59,   228,     1,  1376,  1837,     4,   782,   753,
     612,     1,   612,    74,     1,   782,   175,     4,     1,   347,
      83,    59,    83,   351,    83,   303,   228,   231,   140,   304,
      91,   165,   153,    84,   245,    96,   157,    96,    99,   983,
      99,   780,   103,   228,   103,    75,   612,   583,   303,   877,
    1308,  1309,    59,   800,   146,    59,    74,    59,   303,   303,
    1592,    99,    59,   103,   535,    74,  1833,   780,  1012,    59,
    1588,   818,    59,   190,   545,   190,   190,   449,   303,   229,
     141,   780,    75,   144,   324,   146,   149,   146,   200,   146,
      89,   152,   455,   238,   298,   299,   103,   158,  1821,   228,
     151,   303,    99,   190,   165,   649,   103,    89,   146,   861,
      72,   228,    72,   228,   228,   586,   103,   262,   303,  1376,
    1377,   799,   800,   363,   153,   154,    89,   272,   189,   190,
     189,  1075,  1266,   898,   782,    83,   888,   884,    72,   146,
     818,   228,   146,   204,   146,   204,   176,   361,     0,   146,
     364,   783,   157,   214,   304,   330,   146,   789,   219,   146,
     159,   180,   282,  1127,    10,   122,     1,   228,   229,   934,
     229,   176,   190,     1,   303,   158,     4,   159,   780,   940,
     780,   190,    97,   176,   245,   618,   303,   117,   303,   303,
     253,   151,   253,   155,   253,   155,   159,   154,   507,  2030,
     132,   149,   263,    77,    78,   266,   884,   266,   969,  1976,
     228,     0,   273,  1745,   780,   493,   760,   151,   481,   228,
    1943,  1117,   283,   284,  1127,   286,  1122,   502,   266,   601,
     572,    59,  1750,  1751,   291,  2066,   237,  1127,   493,   240,
     188,   604,   303,   304,  1615,   304,    89,   310,   493,   493,
     311,  1135,     0,   625,   664,    83,   958,   318,   319,  1038,
     261,   176,   323,   799,   800,   628,   638,   915,   493,   266,
     271,  1045,   635,   601,   449,   646,  2059,  1021,  1045,  2008,
    1164,   154,   818,   151,   291,   159,  2009,   291,   220,   291,
    2008,   493,   117,     4,   332,   132,   707,  1193,  2008,  1038,
     361,   579,   175,   364,   365,   253,   581,  1135,   493,   157,
     638,   522,   140,    98,   157,   161,   159,   528,   146,   453,
     166,   149,   157,   158,   579,  1038,    75,  2050,   176,    10,
     155,  1849,  1850,   445,   579,   579,  1164,   174,   157,  1038,
    1087,    90,  1476,  1477,  1478,    56,    57,   133,   884,  1979,
     157,   784,   502,   473,   579,   788,   419,   176,  1615,   133,
     292,   151,   310,   153,   797,   798,   151,  1119,   429,   176,
     429,   583,   200,    77,    78,  2005,   493,   579,   493,   493,
      91,   167,   330,  1347,  1348,  1349,   157,   858,   201,    61,
      62,   452,   453,   167,   579,    72,   157,  1045,    72,   544,
     151,   385,  2032,   464,   465,   176,   493,  1039,   583,  1087,
     148,    72,   473,    72,   475,   176,    58,   455,   954,    61,
      62,   159,    64,   251,   155,   253,    20,  1685,  1686,   160,
     141,   581,   493,   144,   151,  1587,  1038,   175,  1038,   159,
    1592,   502,   159,   502,  1347,  1348,  1349,   158,  1344,   166,
     625,   157,    62,   385,   165,   159,   176,  1347,  1348,  1349,
    1225,   522,   579,   291,   579,   579,   600,   528,  1233,   150,
     176,   419,  1038,   157,   151,   493,    72,   151,   155,   157,
     161,   155,   310,   167,   493,   166,   151,   159,   159,    99,
     151,  1748,   151,   585,   155,   166,   155,    83,   176,   151,
     110,   449,   106,   107,   176,   566,   869,   568,   219,   151,
      96,   151,  1015,    99,   515,   572,   734,   103,   579,   157,
     581,    75,   581,    72,   585,   201,   585,  1471,   585,   167,
     902,   569,   151,   157,   595,   159,   537,    91,   599,   600,
      72,   904,   543,   153,   807,   602,   547,   585,  1376,  1377,
     154,  1087,   263,  1212,   108,   151,   106,   107,  1276,   155,
     157,   151,   273,   570,   793,   572,   127,   128,   572,   159,
     572,   632,   283,   284,   902,   286,   524,   525,   585,   176,
      72,   585,   514,   585,   645,    72,  1218,   519,   585,   157,
     628,   419,   156,  1745,   204,   585,   133,   157,   585,   167,
     311,   176,   151,   189,   536,  1366,   155,   318,   319,   820,
     171,   172,   323,   159,   546,   735,   176,   445,   204,   151,
    1546,    72,   840,   155,   157,   162,   163,   157,   839,  1062,
     176,    13,    14,    15,    16,    17,  1894,   167,   699,   155,
     701,   153,   703,   229,   160,   157,   707,   157,   782,   710,
     361,   273,    72,   364,   330,  1912,   266,   333,  1202,   151,
      72,   176,   284,   155,   151,  1297,   176,   253,   155,  1387,
     153,   347,   155,    72,   735,   351,   153,   625,   662,   157,
     266,   665,   666,  1940,   668,     3,   296,   164,   165,   167,
      72,     3,   302,   677,   153,  1847,   680,   681,   682,   158,
     151,   649,   154,    72,   155,  1241,   162,  1859,    13,    14,
      15,    16,    17,   169,   170,   106,   107,   530,   157,   780,
     777,   782,   332,  1980,  1668,    72,  1670,   159,   167,    72,
     662,   151,   153,   794,   166,   155,   668,   158,   566,   151,
     801,   452,   954,   155,   572,   677,   807,   164,   165,   810,
    1468,   133,   151,   464,   465,  1258,   155,   585,   819,   820,
     821,   145,   146,   147,   696,  1074,   376,    72,    47,    48,
     150,    50,  1308,   449,   793,  1927,    55,   157,   839,   954,
     157,   151,   151,   167,   152,   804,   155,  1615,   158,   602,
     151,   159,   926,     4,     5,     6,     7,     8,     9,    10,
      11,    12,   131,   159,   151,   153,   150,  1455,   155,   159,
     158,   175,   760,   157,   875,   876,   877,   153,   153,  1082,
     877,   157,   151,   158,   151,   151,   155,    74,   133,   155,
     151,   869,   131,   162,   163,  1206,  1207,   154,   155,   877,
    1212,   158,   880,   429,   153,   455,   175,   153,   157,    96,
     890,   157,   151,  1216,   530,   566,   155,   153,   159,   860,
     173,   157,   472,   162,   163,   926,   904,   153,  1633,  1634,
     877,   157,   153,   877,   151,   877,   157,   117,   939,    58,
     877,  1384,    61,    62,   595,    64,   153,   877,   599,   600,
     877,   153,  1428,   890,    13,    14,    15,    16,    17,   157,
     151,   153,  1106,   890,   155,   152,    22,   583,   153,   151,
    1044,  1045,   157,   845,   975,   153,   502,   125,   126,   529,
    1748,   632,   983,   736,   856,   601,   602,   859,   151,   153,
     153,   863,   155,  1221,   645,   129,   130,   151,   153,   549,
    1351,   155,   153,   891,   151,  1448,   153,   151,   155,   625,
     151,  1012,   151,    72,  1015,   157,   151,     3,   153,   569,
     155,   157,   638,  1466,   777,   176,   561,    13,    14,    15,
      16,    17,   156,   157,   151,  1203,  1204,  1038,   155,   162,
     163,   228,   229,  1044,  1045,  1196,   799,   800,   699,  1008,
     701,   151,   703,   153,   604,   155,   707,   153,   245,   710,
     157,   157,  1231,   157,   214,   818,    98,   120,   151,   122,
     123,   124,   131,   153,  1075,   151,   153,   157,   628,   155,
     157,  1786,   151,   153,   735,   635,    72,   157,  1240,  1241,
    1339,   153,   151,   159,  1376,   157,   155,  1212,   151,   156,
     157,   154,   155,   162,   163,    89,   159,   160,   159,   877,
      13,    14,    15,    16,    17,    18,   303,   304,   159,   153,
     736,   150,   890,   157,  1127,  1084,  1241,   145,   146,   147,
     156,   884,   153,   153,  1445,   166,  1137,   157,  1135,  1140,
    1141,  1142,  1357,   794,  1912,   156,   157,  1316,  1317,   167,
     801,   175,    13,    14,    15,    16,    17,  1135,   176,   153,
     153,   777,   153,  1164,   157,   131,   157,  1164,   819,  1170,
     821,   117,  1940,  1874,   153,  1309,   151,  1178,   157,  1884,
    1181,  1182,  1181,  1182,  1185,   151,  1164,   151,  1135,   155,
     153,  1135,   151,  1135,   157,  1196,   162,   163,  1135,   151,
     153,   157,  1182,  1181,   157,  1135,   153,   168,  1135,  1685,
     157,    72,  1980,   163,   153,   173,   131,  1164,   157,     3,
    1164,   161,  1164,   131,   875,   876,   877,  1164,  1229,    13,
      14,    15,    16,    17,  1164,  1182,   151,  1164,  1216,   153,
     155,  1242,   153,   157,  1181,  1182,   157,   162,   163,  1121,
      13,    14,    15,    16,    17,  1182,   154,  1258,   153,  2013,
     131,   153,  1134,  2017,   153,  1266,   453,  1357,   156,   157,
     131,   153,  1231,   156,   157,   926,  1428,   156,   157,  1151,
     151,   145,   146,   147,   155,   153,   902,   153,    72,   905,
     151,   162,   163,   157,   155,   157,   158,  1298,   714,   715,
     716,   162,   163,   167,   155,   239,   493,   156,   157,    72,
     156,   846,   176,  1428,  1202,   502,  1537,  1538,  1539,   869,
    1279,  1245,   133,   473,  1212,   475,  1124,  1125,  1126,  1544,
     880,   133,  1550,  1615,  1087,   522,  1647,  1552,   954,   156,
     157,   528,   156,   157,  1347,  1348,  1349,   157,  1351,  1352,
     721,   722,   723,   724,   904,  1550,  1357,  1316,  1317,  1127,
    1361,   156,   157,  1364,   158,  1550,  1550,  1135,   156,   157,
    1350,   158,   907,    13,    14,    15,    16,    17,    18,  1376,
    1377,   568,   151,  1384,   151,  1550,    78,    13,    14,    15,
      16,    17,   579,   153,   581,   153,  1164,   156,  1376,  1377,
     156,   157,   153,  1404,   153,  1406,   159,  1406,  1550,   156,
     157,   103,   153,  1350,   106,   107,   108,   109,   110,   111,
     112,   113,   114,  1350,   153,  1550,   156,   157,  1406,  1376,
    1377,   155,  1376,  1377,  1376,  1377,   156,   157,   644,  1376,
    1377,   145,   146,   147,   156,   157,    72,  1448,   159,  1376,
    1377,   156,   157,   157,  1544,    70,  1614,   110,   111,   112,
     113,   114,  1552,   167,   159,  1466,   156,   157,   159,  1406,
    1471,   156,   176,  1453,  1626,  1476,  1477,  1478,   156,   157,
     151,  1550,  1017,    78,   176,    18,  1137,   156,   422,  1140,
    1141,  1142,  1626,   157,   158,  1367,  1368,  1715,   175,  1034,
    1035,   156,   157,  1718,   438,   131,   157,   441,   156,   157,
     159,  1626,   151,  1164,    77,    78,  1453,  1267,  1268,  1170,
    1715,   157,   158,   717,   718,   151,  1453,  1178,   153,   155,
    1715,  1715,   176,  1405,  1185,   153,   162,   163,   719,   720,
     159,   725,   726,  1544,  1557,  1558,   176,  1446,  1447,  1550,
    1715,  1552,  1686,   159,  1866,  1308,  1309,   156,  1559,   156,
     103,  1203,  1204,    18,   498,   108,   109,   110,   111,   112,
     113,   114,  1573,  1715,   150,   153,   153,   150,  1229,  1347,
    1348,  1349,  1350,  1351,  1352,  1586,   153,   153,   153,   153,
    1715,  1242,   153,   780,   153,   782,  1212,   159,    70,   805,
     153,  1581,  1580,   159,   159,   176,  1764,   175,  1376,  1377,
     159,   153,   153,   153,   150,  1830,   153,   175,  1615,   153,
     159,   153,  1623,   150,  1775,  1241,   157,   157,  1718,  1941,
     153,  1181,   157,   820,  1581,   153,   153,  1615,   153,   153,
     153,  1944,   153,  1580,  1581,   153,   156,   156,   153,  1629,
     373,   153,   839,   153,  1581,  1181,  1182,   807,  1546,   153,
     810,   153,   153,   153,  1833,   153,  1216,  1668,  1615,  1670,
     156,  1615,   153,  1615,   175,   398,   399,   153,  1615,    18,
     150,    14,   153,   157,   151,  1453,    13,   151,  1615,   151,
     151,   151,  1629,   151,    74,   158,   419,   176,   103,    91,
     157,   176,  1629,   108,   109,   110,   111,   112,   113,   114,
    1361,   158,  1870,  1364,  1715,   156,   156,  1718,    57,    58,
      59,    60,    61,    62,    63,    64,   449,   176,  1729,   150,
     176,   159,  1733,   157,  1606,   153,  1660,   176,   153,   156,
    1830,   157,   153,   157,   157,   151,  1747,   152,   156,   153,
     155,  1748,   153,  1404,  1289,  1290,  1757,  1735,   151,   150,
    1894,    88,    13,    14,    15,    16,    17,    18,  1303,  1304,
    1748,   150,    80,  1774,  1775,   151,   103,   176,   176,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   176,   176,
    2008,   176,  1943,   176,  2009,   176,   150,   153,  1735,   176,
     151,  1748,  1337,  1338,  1748,  1974,  1748,  1976,  1358,  1562,
     176,  1748,  1428,  2008,   150,   150,   157,   157,   752,   753,
     150,  1748,   159,  2008,  2008,   975,   120,   159,   762,  1830,
     156,   765,   156,   983,   156,  2050,  1837,   153,   156,   150,
    1841,   153,   176,  2008,   158,  1846,  2015,  1615,   158,   150,
     153,  1038,   156,   153,   153,  2013,  1406,  1044,  1045,  2017,
    2018,  1629,  1012,  1866,   153,  1015,  2008,   153,   156,   150,
    1871,   158,   157,   153,  1833,   151,   153,   151,   157,   151,
    1406,   109,    75,  2008,   150,   156,   846,   156,   156,   159,
     150,   153,   826,   150,  2052,   153,   830,   153,   153,   153,
     834,    13,    14,    15,    16,    17,    18,   156,  1559,   153,
     153,    75,   176,   176,  1915,  1912,   150,   176,  1919,  2009,
    2078,   151,  1573,    90,  2082,  1075,   153,   153,  2097,   150,
     159,  1932,  1685,  1686,  1912,  1586,   156,   156,  1941,  2008,
     150,   150,  1943,  1940,  1945,   153,  2104,   907,   153,   153,
     153,  2008,   153,  2008,  2008,  1956,   153,  1958,  1959,   155,
    2050,    75,  1940,   154,   176,  1912,    75,   167,  1912,   167,
    1912,   158,  1623,   150,   176,  1912,   150,   176,   153,   157,
    1748,   153,  1983,  1980,   153,  1912,   167,   153,   150,   152,
     713,  1526,  1527,  1940,   167,   150,  1940,   158,  1940,  1205,
     103,   151,  1980,  1940,   157,    75,   151,  2008,  2009,  1196,
    2009,   150,   152,  1940,   156,  1974,   167,  1976,  2019,   167,
      62,   109,   176,   109,   176,   150,   152,  1562,  1234,  2030,
    1580,   153,   153,  1980,   158,   150,  1980,   150,  1980,   176,
      75,   151,   153,  1980,   176,   153,   153,   176,  1366,  2050,
    1660,  2050,     1,  1980,   686,     4,  2015,  1017,  1273,   727,
     730,   728,   104,  2064,   729,  2066,   108,  1153,   731,   111,
     418,   113,  1164,  2066,  1034,  1035,  1615,  1283,  1729,  1756,
    1976,  1748,  1733,  2060,  2085,  2005,  1875,  1021,  2059,  2048,
    2091,  2047,  1941,  1607,  1607,  2082,  1747,  2018,  1940,    49,
    2101,  1185,   258,  1544,  2105,  1640,  1757,  1830,  1258,  1902,
      59,  1298,  1352,  1558,  2115,   930,  1266,  1178,   807,   482,
     894,  1453,   595,  1774,  1629,    74,     0,   939,   752,   752,
     752,    -1,    -1,    -1,    83,    -1,    -1,    -1,  2097,    -1,
    1675,  1894,    -1,    -1,  1912,  1680,  1681,    96,    -1,    -1,
      99,    -1,    -1,    -1,   103,    -1,    -1,   103,  1092,    -1,
      -1,  1095,   108,   109,   110,   111,   112,   113,   114,   115,
    1357,    -1,  1940,   119,    -1,   121,    -1,    -1,    -1,    -1,
    1137,    -1,    -1,    -1,    -1,  1735,  1837,    -1,    -1,    -1,
    1841,   140,    -1,    -1,    -1,  1846,    -1,   146,    -1,    -1,
     149,    -1,    -1,   152,   153,  2077,   152,    -1,    -1,   155,
      -1,    -1,  1980,    -1,   256,   164,    -1,    -1,    -1,    -1,
    1871,  2093,    -1,    -1,    -1,    -1,    -1,   950,    65,    66,
      67,    68,   955,    -1,  1384,    -1,    -1,  1443,    -1,   188,
     189,   190,    -1,   966,  1450,    -1,    -1,    -1,    -1,    -1,
    1866,   200,   201,    -1,    -1,   204,    -1,    -1,    -1,    -1,
      -1,  1467,    -1,    -1,  1915,    -1,   103,    -1,  1919,   106,
     107,   108,   109,   110,   111,   112,   113,   114,    -1,   228,
     229,  1932,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    -1,   334,   335,  1945,   337,   245,   339,  1448,    -1,
       3,    -1,    -1,    -1,   253,  1956,    -1,  1958,  1959,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1466,   266,   155,    -1,
      -1,  1471,    -1,    -1,    -1,  1941,  1476,  1477,  1478,  1289,
    1290,    -1,  1983,    -1,   376,    -1,   173,    -1,    -1,    -1,
      -1,    -1,    -1,  1303,  1304,    -1,    -1,   296,    -1,    -1,
      -1,  1285,    -1,   302,   303,   304,    -1,  1544,    -1,    -1,
    1294,   310,    -1,  1550,   103,  1552,    -1,    -1,  2019,   108,
     109,   110,   111,   112,   113,   114,   115,  1337,  1338,  2030,
      -1,   330,   331,   332,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1597,    -1,    -1,  1944,    -1,    -1,    -1,   347,    -1,
     103,    -1,   351,   106,   107,   108,   109,   110,   111,   112,
     113,   114,  1135,  2064,   103,  2066,   155,   106,   107,   108,
     109,   110,   111,   112,   113,   114,    -1,    -1,   131,    -1,
     472,    -1,    -1,    -1,  2085,    -1,   385,    -1,    -1,    -1,
    2091,    -1,    -1,    -1,    -1,  1651,    -1,  1404,   151,   152,
    2101,    -1,    -1,    -1,  2105,   158,  1662,    -1,    -1,   162,
     163,    -1,    -1,    -1,  2115,    18,    -1,    -1,    -1,    -1,
     419,    -1,    -1,   422,    -1,    -1,    -1,    -1,    -1,    -1,
     429,    -1,    -1,    -1,    -1,    -1,    -1,   176,    -1,  1212,
      -1,    -1,  2027,    -1,    -1,    -1,   445,    -1,    -1,   541,
     449,    -1,    -1,    -1,   453,    -1,   455,    -1,    61,    62,
      63,    64,    -1,    -1,    -1,    -1,    -1,    -1,  1668,    -1,
    1670,    -1,    -1,    -1,    -1,  1248,  1249,  1250,  1715,   103,
      -1,  1718,  1255,  1256,   108,   109,   110,   111,   112,   113,
     114,   115,    -1,    -1,   493,   119,    -1,   121,    -1,    -1,
     103,    -1,    -1,   502,    -1,   108,   109,   110,   111,   112,
     113,   114,   115,    -1,    -1,    -1,  1526,  1527,    -1,    -1,
    1776,  1777,    -1,   522,    -1,   524,   525,    -1,   152,   528,
      -1,   530,    -1,    -1,    -1,    -1,   103,    -1,  1775,   106,
     107,   108,   109,   110,   111,   112,   113,   114,    48,    -1,
      -1,    -1,   155,    -1,    -1,    -1,  1540,    -1,    -1,    13,
      14,    15,    16,    17,    -1,    -1,  1573,    -1,    -1,   568,
       4,     5,     6,     7,     8,     9,    10,    11,    12,  1586,
     579,    -1,   581,    -1,   583,    -1,   585,    -1,    -1,    -1,
      -1,    -1,    -1,  1830,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   601,   602,    -1,   604,    72,    -1,    -1,   176,
      -1,    -1,    -1,   612,    -1,    -1,  1623,   616,    72,    -1,
     120,    -1,    -1,    -1,    -1,    -1,   625,    -1,    -1,  1885,
    1640,    65,    -1,   133,    -1,   135,   635,   103,    -1,   638,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   103,
     649,    -1,   106,   107,   108,   109,   110,   111,   112,   113,
     114,    -1,    -1,   662,    -1,  1675,   665,   666,    -1,   668,
    1680,  1681,    -1,    -1,    -1,    -1,    -1,   131,   677,    -1,
      -1,   680,   681,   682,   103,   151,   152,    -1,    -1,   108,
     109,   110,   111,   112,   113,   114,    -1,   151,   152,    -1,
      -1,   155,    -1,    -1,    -1,    -1,  1943,  1963,   162,   163,
      -1,    -1,    -1,    -1,    -1,    -1,  1700,  1701,    -1,    -1,
      -1,    -1,  1729,    -1,    -1,    -1,  1733,    -1,    -1,    -1,
      -1,    -1,   151,   152,    -1,   235,   236,   736,    -1,   239,
    1747,    -1,   242,   243,    -1,    -1,    -1,    -1,    -1,    -1,
    1757,    -1,    -1,   752,   753,    -1,    -1,    -1,    62,   103,
      -1,   760,   106,   107,   108,   109,   110,   111,   112,   113,
     114,  2008,  2009,    -1,    -1,    -1,    -1,    -1,   777,    -1,
      -1,   780,   103,   782,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,    -1,    99,    -1,    -1,    -1,    -1,
     799,   800,    -1,    -1,    -1,    -1,   110,    -1,   112,    -1,
     114,    -1,    -1,  2050,    -1,    -1,    -1,    -1,   162,   818,
      -1,   820,    -1,    -1,    -1,    -1,    -1,   327,   328,    -1,
    1837,    -1,    -1,   154,  1841,    -1,    -1,  1821,   159,  1846,
     839,    -1,    -1,   343,    -1,    -1,    -1,  1620,    -1,   153,
     103,   155,   156,   106,   107,   108,   109,   110,   111,   112,
     113,   114,    -1,    -1,  1871,    -1,    -1,    -1,    -1,    -1,
      -1,    13,    14,    15,    16,    17,    -1,    -1,   877,    -1,
      -1,    -1,    -1,    -1,    -1,   884,    -1,    -1,    -1,    -1,
      -1,   890,   891,    -1,    -1,    72,    -1,    -1,    -1,    -1,
     204,    -1,    -1,   902,    -1,   904,   159,    -1,  1915,    -1,
      -1,    -1,  1919,    -1,  1898,    -1,   915,    -1,  1902,    -1,
      -1,    -1,    -1,    -1,    -1,  1932,   103,    -1,    -1,    -1,
      72,   108,   109,   110,   111,   112,   113,   114,   438,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1956,
      -1,  1958,  1959,    -1,   131,   954,    -1,    -1,    -1,  1943,
      -1,   103,   266,    -1,   268,   269,   108,   109,   110,   111,
     112,   113,   114,    -1,   151,   152,  1983,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   162,   163,    -1,    -1,   131,
      -1,    -1,   296,    -1,    -1,    -1,    -1,    -1,   302,    -1,
      -1,    -1,    -1,    -1,    -1,   505,    -1,    -1,    -1,   151,
     152,    -1,  2019,    -1,    -1,    -1,    -1,  2027,    -1,    -1,
     162,   163,  1021,  2030,  2008,  2009,    -1,    -1,   332,    -1,
      -1,    -1,    -1,    -1,   338,    -1,   340,    -1,    -1,  1038,
      -1,    -1,    -1,   101,    -1,   103,  1045,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,  2064,    -1,  2066,
      -1,    -1,    -1,    -1,    -1,    -1,  2050,    72,    -1,    -1,
      -1,    -1,   376,    -1,    -1,    -1,    -1,    -1,  2085,    -1,
      -1,    -1,    -1,    -1,  2091,    -1,    -1,    -1,  1087,    -1,
      -1,    -1,    -1,   151,  2101,    -1,   154,   155,   103,    -1,
    1873,   106,   107,   108,   109,   110,   111,   112,   113,   114,
      -1,    13,    14,    15,    16,    17,    -1,    -1,    -1,    -1,
     620,   621,    -1,    -1,    -1,   429,   131,    -1,  1127,    -1,
      -1,    -1,   103,   633,    -1,    -1,  1135,   108,   109,   110,
     111,   112,   113,   114,    -1,    -1,   151,   152,    -1,    -1,
     155,   455,    -1,   457,   458,    -1,    -1,   162,   163,    -1,
     131,    -1,    -1,    -1,    -1,  1164,    -1,    -1,   472,    -1,
      72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     151,   152,  1181,  1182,   155,    -1,    -1,    -1,    -1,    -1,
      -1,   162,   163,    -1,    -1,    -1,    -1,  1196,   502,    -1,
      -1,   103,    -1,  1202,   106,   107,   108,   109,   110,   111,
     112,   113,   114,  1212,    -1,    -1,    -1,    -1,   522,    -1,
      -1,    -1,    -1,   527,    -1,   529,    -1,    -1,   103,   131,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
      -1,     1,  1241,    -1,     4,   549,  1245,   551,   552,   151,
     152,    -1,    -1,   155,    -1,    -1,   756,   757,    -1,    -1,
     162,   163,   762,   103,    -1,   569,   106,   107,   108,   109,
     110,   111,   112,   113,   114,    -1,    -1,   581,    -1,    -1,
      -1,    -1,    -1,   783,   159,    -1,   786,   787,    -1,   789,
      -1,   791,   792,    -1,    -1,    -1,    -1,    -1,    -1,    59,
     604,    -1,   606,   607,    -1,    -1,    -1,    -1,    -1,  1308,
    1309,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   159,
      -1,    -1,    -1,    83,   628,   629,    -1,    -1,    -1,    -1,
     830,   635,    -1,    -1,   834,    -1,    -1,    -1,    -1,    99,
      -1,    -1,    -1,   103,    -1,    -1,    -1,    -1,  1347,  1348,
    1349,  1350,  1351,  1352,    -1,    -1,    -1,    -1,  1357,  1358,
      -1,    -1,   103,    -1,     1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,    -1,    -1,    -1,  1376,  1377,    -1,
     140,    -1,    -1,    -1,    -1,    -1,   146,    -1,    -1,   149,
     131,    -1,    -1,   153,    -1,    -1,    -1,    -1,    -1,   899,
     900,    -1,    -1,   163,   164,   165,    -1,  1406,    -1,    -1,
     151,   152,    49,   913,   155,    52,    -1,    54,    -1,    56,
      -1,   162,   163,    -1,    -1,    -1,    -1,    -1,   188,  1428,
      -1,    -1,    -1,    -1,   175,    -1,    73,    -1,    -1,    -1,
     200,   201,   103,    -1,   204,   106,   107,   108,   109,   110,
     111,   112,   113,   114,  1453,    -1,  1455,    -1,    -1,    -1,
      -1,    -1,    -1,   100,   101,    -1,   103,    -1,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,    -1,   119,   120,   121,    -1,   123,   124,    -1,    -1,
     151,   251,   103,   253,   131,   106,   107,   108,   109,   110,
     111,   112,   113,   114,    -1,    -1,   266,    -1,    13,    14,
      15,    16,    17,    -1,   151,    -1,    -1,   154,   155,    -1,
      -1,    -1,    -1,   283,   161,   162,   163,   164,   165,   166,
     167,   291,    -1,    -1,    -1,    -1,   296,    -1,    -1,    -1,
      -1,  1540,   302,   154,  1044,  1544,    -1,  1546,    -1,    -1,
     310,  1550,    -1,  1552,    -1,    13,    14,    15,    16,    17,
      -1,    -1,    -1,    -1,    -1,   869,    -1,    72,    -1,    -1,
     330,    -1,   332,   333,    -1,    -1,   880,    -1,    -1,    -1,
      -1,  1580,  1581,    -1,    -1,  1085,    -1,   347,    -1,    -1,
      -1,   351,  1092,    -1,    -1,  1095,    -1,    -1,   103,    -1,
     904,   106,   107,   108,   109,   110,   111,   112,   113,   114,
      -1,   915,    -1,    -1,    72,    -1,  1615,    -1,    -1,    -1,
     924,    -1,    -1,    -1,    -1,   385,   131,  1626,   103,    -1,
    1629,   106,   107,   108,   109,   110,   111,   112,   113,   114,
      -1,    72,    -1,    -1,    -1,   103,   151,   152,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   162,   163,   419,
     103,  1660,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   103,   131,    -1,   106,   107,   108,   109,   110,
     111,   112,   113,   114,    -1,   445,  1685,  1686,    -1,   449,
      -1,    -1,    -1,   151,   152,   455,    -1,    -1,    -1,    -1,
     131,  1700,  1701,    -1,   162,   163,    -1,    -1,    -1,   152,
      -1,    -1,   155,    -1,    -1,    -1,  1715,  1021,  1218,  1718,
     151,   152,    -1,    -1,    -1,    -1,  1226,  1227,    -1,    -1,
      -1,   162,   163,    -1,    -1,    -1,  1735,    -1,    -1,    -1,
      -1,  1045,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1748,
     103,    -1,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,    -1,    -1,   524,   525,   180,    -1,    -1,   529,
     530,    -1,   103,    -1,    -1,    -1,  1775,   108,   109,   110,
     111,   112,   113,   114,   115,  1285,    -1,    -1,   119,    -1,
     121,    -1,    -1,    -1,  1294,    -1,    -1,  1297,    -1,  1299,
    1300,   561,   155,    -1,    -1,    -1,   566,    -1,    -1,   569,
     570,    -1,   572,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,   152,  1821,   583,   155,   585,    -1,    -1,    -1,    -1,
      -1,  1830,    -1,    13,    14,    15,    16,    17,    -1,   599,
    1340,   601,   602,    -1,   604,    -1,    -1,    -1,    -1,    -1,
      -1,   103,    -1,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,   114,    -1,    -1,   625,    -1,  1866,   628,    -1,
      -1,    -1,   632,    -1,    -1,   635,    -1,  1181,   638,   131,
     640,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   649,
      -1,    -1,    72,    -1,    -1,  1894,    -1,    -1,    -1,   151,
     152,    -1,   662,  1902,    -1,   665,   666,  1407,   668,    -1,
     162,   163,  1216,  1912,    -1,    -1,    -1,   677,  1222,    -1,
     680,   681,   682,   103,    -1,    -1,   106,   107,   108,   109,
     110,   111,   112,   113,   114,    -1,    -1,    -1,    -1,    -1,
      -1,  1940,  1941,    -1,  1943,  1944,    -1,    -1,    -1,    -1,
      -1,   131,    -1,    -1,    -1,   369,    -1,    -1,    -1,   373,
     374,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   383,
     384,   151,   152,    -1,    -1,    -1,   736,    -1,    -1,    -1,
      -1,  1980,   162,   163,   398,   399,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    -1,  1495,    -1,    -1,    -1,    -1,
     760,    -1,    -1,    -1,    -1,   419,    -1,    -1,    83,  2008,
    2009,    -1,    -1,    -1,    -1,    -1,    -1,   777,    -1,    -1,
      -1,    -1,   103,    -1,  1524,   106,   107,   108,   109,   110,
     111,   112,   113,   114,    -1,   449,    -1,    -1,    -1,   799,
     800,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     131,  2050,    -1,    -1,  1358,    -1,   103,    -1,   818,   106,
     107,   108,   109,   110,   111,   112,   113,   114,    -1,    -1,
     151,   152,    -1,    -1,   149,    -1,    -1,    -1,    -1,    -1,
      -1,   162,   163,    -1,   131,    -1,   846,    -1,    -1,   164,
      -1,     1,    -1,    -1,     4,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1406,    -1,   151,   152,    -1,    -1,    -1,   869,
      -1,   158,    -1,   188,    -1,   162,   163,   877,    -1,    -1,
     880,    -1,    -1,    -1,   884,  1625,   201,    -1,    -1,   103,
     890,   891,   106,   107,   108,   109,   110,   111,   112,   113,
     114,    -1,   902,    -1,   904,   905,    -1,   907,    -1,    59,
      -1,   103,    -1,    -1,   106,   107,   108,   109,   110,   111,
     112,   113,   114,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    83,    -1,    -1,    -1,    -1,   253,   131,
     154,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   103,   954,    -1,    -1,    -1,    -1,   151,
     152,    -1,    -1,   155,    -1,    -1,    -1,  1707,  1708,    -1,
     162,   163,    -1,    -1,    -1,    -1,    -1,    -1,   103,  1719,
      -1,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     140,    -1,    -1,    -1,   103,   310,   146,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   131,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   164,   330,    -1,  1017,    -1,    -1,
      -1,    -1,   131,    -1,    -1,    -1,   151,   152,    -1,    -1,
      -1,    -1,    -1,    -1,  1034,  1035,  1580,   162,   163,   189,
      -1,    -1,   151,   152,    -1,    -1,    -1,    -1,    -1,    -1,
     200,   201,    -1,   162,   163,    -1,    -1,    -1,    -1,   713,
     714,   715,   716,   717,   718,   719,   720,   721,   722,   723,
     724,   725,   726,   727,   728,   729,   730,   731,    -1,   229,
      -1,    -1,    -1,  1823,    -1,    -1,    -1,  1087,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   245,    -1,    -1,    -1,    -1,
     250,   251,    -1,   253,   419,    -1,    -1,    -1,   103,    -1,
      -1,    -1,    -1,   108,   109,   110,   111,   112,   113,   114,
     115,    -1,    -1,    -1,   119,   275,   121,  1127,   278,    -1,
     280,    -1,    -1,    -1,   449,  1135,    -1,    -1,    -1,   793,
     103,   291,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,    -1,    -1,   304,    -1,    -1,   152,  1898,    -1,
     155,    -1,    -1,    -1,  1164,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1718,    -1,    -1,    -1,    -1,    -1,
     330,  1181,  1182,   333,    -1,    -1,    -1,    -1,   151,    -1,
      -1,  1735,    -1,    -1,    -1,    -1,    -1,   347,    -1,    -1,
      -1,   351,  1202,    -1,    -1,    -1,    -1,    -1,    -1,   524,
     525,    -1,  1212,    -1,    -1,   530,  1216,   134,   135,   136,
     137,   138,   139,   140,   141,   142,   143,   144,  1228,    -1,
      -1,   148,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1240,  1241,    -1,    -1,    -1,  1245,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   175,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   583,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1821,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   602,    -1,  1289,
    1290,    -1,    -1,    -1,    -1,   445,   950,    -1,    -1,   449,
      -1,   955,    -1,  1303,  1304,    -1,    -1,    -1,  1308,  1309,
     625,    -1,   966,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     1,    -1,   649,    -1,    -1,  1337,  1338,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1347,  1348,  1349,
    1350,  1351,  1352,    -1,  1008,    -1,    -1,    -1,  1358,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1376,  1377,    -1,    -1,
     530,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      59,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1944,    -1,    -1,    -1,    -1,    -1,  1406,    -1,    -1,    -1,
      -1,   561,    -1,    -1,    -1,    -1,   566,    -1,    -1,    -1,
     570,   736,   572,    -1,    -1,    -1,    -1,    -1,  1428,    -1,
      -1,    -1,    -1,   583,   103,   585,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   760,    -1,    -1,    -1,    -1,
      -1,   601,   602,  1453,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   777,    -1,    -1,    -1,   616,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   625,    -1,   146,    -1,    -1,
     630,  1135,    -1,    -1,   799,   800,    -1,    -1,   638,    -1,
      -1,    -1,    -1,    -1,    -1,   164,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   818,    -1,    -1,  2050,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1526,  1527,    -1,    -1,
      -1,    -1,   201,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1546,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1212,    -1,
      -1,    -1,  1562,    -1,    -1,    -1,    -1,    -1,    -1,   884,
      -1,    -1,    -1,    -1,    -1,    -1,   891,  1231,    -1,    -1,
    1580,  1581,    -1,    -1,    -1,    -1,   736,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1248,  1249,  1250,    -1,    -1,    -1,
      -1,  1255,  1256,   753,    -1,    -1,   275,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1615,    -1,    -1,    -1,    -1,
      -1,    -1,   291,    -1,    -1,  1279,  1626,   777,    -1,  1629,
      -1,    -1,   782,    -1,    -1,    -1,    -1,    -1,    -1,   954,
    1640,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   799,
     800,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1660,   330,  1316,  1317,   333,    -1,    -1,    -1,   818,    -1,
      -1,    -1,    -1,    -1,    -1,  1675,    -1,    -1,   347,    -1,
    1680,  1681,   351,    -1,    -1,  1685,  1686,    -1,    13,    14,
      15,    16,    17,     1,    -1,    20,   846,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    -1,    50,    51,   877,    53,    -1,
      55,    -1,    -1,    -1,   884,  1735,    -1,    -1,    -1,    -1,
     890,    -1,    -1,    -1,    -1,    -1,    -1,    72,  1748,    -1,
      -1,    59,   902,    -1,    -1,   905,    -1,   907,    -1,    -1,
       1,    -1,   912,     4,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1087,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     449,   106,   107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   954,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    59,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   154,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   146,    -1,
      -1,    -1,    83,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   164,    -1,    -1,    -1,
      -1,   530,   103,    -1,    -1,    -1,  1866,  1017,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1034,  1035,    -1,  1202,    -1,    -1,
      -1,    -1,   561,   201,  1894,    -1,    -1,  1212,    -1,   140,
      -1,   570,    -1,   572,    -1,   146,    -1,    -1,   149,    -1,
      -1,    -1,  1912,    -1,   583,    -1,   585,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1241,    -1,    -1,    -1,
      -1,    -1,   601,   602,    -1,    -1,    -1,  1087,    -1,    -1,
    1940,  1941,    -1,    -1,  1944,    -1,    -1,   188,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   625,    -1,    -1,   200,
      -1,    -1,    -1,    -1,    -1,    -1,  1620,    -1,    -1,   638,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1127,    -1,    -1,
    1980,    -1,    -1,   291,    -1,  1135,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1308,  1309,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     251,    -1,   253,    -1,  1164,    -1,    -1,   258,    -1,    -1,
      -1,    -1,   330,    -1,    -1,   333,    -1,  2027,    -1,    -1,
      -1,    -1,  1182,    -1,    -1,    -1,    -1,    -1,    -1,   347,
      -1,    -1,    -1,   351,    -1,    -1,    -1,    -1,    -1,    -1,
     291,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1212,    -1,    -1,    -1,    -1,   736,    -1,   310,
      -1,  1221,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1240,  1241,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   777,    -1,
      -1,    -1,    -1,  1428,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     799,   800,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1289,
    1290,   449,    -1,    -1,   385,    -1,    -1,    -1,    -1,   818,
      -1,    -1,    -1,  1303,  1304,    -1,    -1,    -1,  1308,  1309,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   846,   419,  1833,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1337,  1338,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1347,  1348,  1349,
    1350,    -1,    -1,    -1,   445,    -1,    -1,    -1,   877,    -1,
      -1,    -1,    -1,    -1,    -1,   884,    -1,    -1,    -1,  1873,
      -1,    -1,   530,    -1,    -1,    -1,  1376,  1377,    -1,    -1,
      -1,  1546,    -1,   902,    -1,    -1,   905,    -1,   907,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   561,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   570,    -1,   572,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   583,    -1,   585,  1428,    -1,
      -1,    -1,    -1,   524,   525,   954,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   601,   602,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1453,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1626,    -1,    -1,    -1,    -1,    -1,   625,    -1,    -1,
    1974,    -1,  1976,    -1,    -1,   566,    -1,    -1,    -1,   570,
     638,   572,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   585,    -1,    -1,    -1,  1017,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2015,    -1,    -1,    -1,  1034,  1035,    -1,    -1,    -1,
    1685,  1686,    -1,    -1,    -1,    -1,  1526,  1527,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  2048,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1552,    -1,    -1,    -1,    -1,    -1,   649,    -1,
      -1,    -1,  1562,    -1,    -1,    -1,    -1,    -1,  1087,    -1,
      -1,   662,    -1,    -1,   665,   666,    -1,   668,   736,    -1,
      -1,  1581,    -1,    -1,    -1,    -1,   677,    -1,    -1,   680,
     681,   682,    -1,  2097,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1615,  1135,    -1,    -1,   777,
      -1,    -1,    -1,    -1,    -1,    -1,  1626,    -1,    -1,  1629,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1640,   799,   800,    -1,    -1,  1164,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     818,    -1,    -1,  1182,    -1,    -1,    -1,    -1,    -1,   760,
      -1,    -1,    -1,    -1,    -1,  1675,    -1,    -1,    -1,    -1,
    1680,  1681,    -1,    -1,    -1,  1685,  1686,    -1,   846,    -1,
      -1,    -1,    -1,  1212,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1701,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   877,
      -1,  1240,  1241,    -1,    -1,    -1,   884,    -1,    -1,  1894,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   902,    -1,    -1,   905,  1748,   907,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1289,  1290,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1303,  1304,   877,    -1,    -1,  1308,
    1309,    -1,    -1,    -1,    -1,    -1,   954,    -1,    -1,   890,
     891,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      87,    -1,    -1,    -1,    -1,    -1,    93,    94,  1337,  1338,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     127,    -1,    -1,    -1,    -1,    -1,    -1,  1376,  1377,  1017,
      -1,    -1,    -1,    -1,    -1,    -1,  1866,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1034,  1035,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,    -1,    -1,
       4,    -1,    -1,    -1,  1894,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1428,
      -1,    -1,  1912,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1087,
      -1,    -1,    -1,    -1,    -1,    -1,     1,    -1,    -1,     4,
    1940,  1941,    -1,    -1,    -1,    59,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    83,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1135,    -1,    -1,
    1980,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,
      -1,    -1,    -1,    -1,    59,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1164,  1526,  1527,  2009,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    83,    -1,
      -1,    -1,    -1,   300,  1182,    -1,   140,  2027,    -1,    -1,
      -1,    -1,   146,    -1,    -1,   149,  1127,    -1,   103,    -1,
      -1,    -1,    -1,  1562,  1135,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1212,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1581,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1164,   188,   140,    -1,    -1,    -1,    -1,
      -1,   146,  1240,  1241,   149,    -1,   200,    -1,    -1,    -1,
      -1,  1182,    -1,    -1,    -1,    -1,  1615,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1626,    -1,    -1,
      -1,  1202,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1640,    -1,   188,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1289,  1290,    -1,    -1,   200,    -1,   251,    -1,   253,
      -1,    -1,    -1,    -1,   258,  1303,  1304,    -1,    -1,    -1,
    1308,  1309,    -1,    -1,  1245,    -1,  1675,    -1,    -1,    -1,
      -1,  1680,  1681,    -1,    56,    57,  1685,  1686,    -1,    -1,
      -1,   448,    -1,   450,    -1,    -1,    -1,   291,    -1,  1337,
    1338,    -1,   459,   460,    -1,    -1,   251,    -1,   253,    -1,
      -1,    -1,    -1,   258,    -1,    -1,   310,    -1,    -1,    91,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1376,  1377,
      -1,    -1,    -1,    -1,    -1,    -1,   291,    -1,    -1,  1748,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   310,    -1,    -1,    -1,   141,
      -1,    -1,   144,    -1,    -1,    -1,  1347,  1348,  1349,  1350,
    1351,  1352,    -1,    -1,    -1,    -1,   158,    -1,    -1,    -1,
    1428,   385,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1376,  1377,    -1,    -1,    -1,
     567,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   419,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     385,    -1,    -1,    -1,    -1,    -1,    -1,   219,    -1,    -1,
      -1,   445,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1866,    -1,    -1,
      -1,    -1,    -1,    -1,   419,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1453,    -1,    -1,    -1,    -1,    -1,  1526,  1527,
      -1,   263,    -1,    -1,    -1,  1894,    -1,    -1,    -1,    -1,
     445,   273,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   284,  1912,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1562,    -1,    -1,    -1,    -1,    -1,
     524,   525,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   311,
      -1,  1940,  1941,  1581,    -1,    -1,   318,   319,    -1,    -1,
      -1,   323,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   566,    -1,    -1,  1546,   570,  1615,   572,   524,
     525,  1980,    -1,    -1,    -1,    -1,    -1,    -1,  1626,   361,
      -1,   585,   364,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1640,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1581,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   566,    -1,    -1,    -1,   570,    -1,   572,  2027,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   793,  1675,    -1,    -1,
     585,    -1,  1680,  1681,  1615,    -1,    -1,  1685,  1686,    -1,
      -1,    -1,    -1,    -1,    -1,   649,    -1,    -1,  1629,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   662,    -1,
      -1,   665,   666,    -1,   668,    -1,    -1,    -1,    -1,    -1,
     452,    -1,    -1,   677,    -1,    -1,   680,   681,   682,  1660,
      -1,    -1,   464,   465,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   649,    -1,    -1,    -1,    -1,    -1,
    1748,    -1,    -1,   870,   871,    -1,    -1,   662,    -1,    -1,
     665,   666,    -1,   668,   881,   882,   883,    -1,    -1,   886,
      -1,    -1,   677,    -1,    -1,   680,   681,   682,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   760,    -1,    -1,    -1,
      -1,    -1,    -1,    49,    -1,    -1,    52,  1748,    54,    -1,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   968,    -1,    -1,    -1,   760,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   595,   100,   101,    -1,   103,  1866,   105,
     106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
     116,   117,    -1,   119,   120,   121,    -1,   123,   124,    -1,
      -1,    -1,    -1,    -1,    -1,   131,  1894,  1014,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   145,
     146,   147,    -1,   645,  1912,   151,   152,    -1,   154,   155,
      -1,    -1,    -1,   877,    -1,   161,   162,   163,   164,   165,
     166,   167,    -1,    -1,    -1,    -1,   890,   891,    -1,    -1,
      -1,    -1,  1940,  1941,  1061,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1070,  1071,  1072,  1073,    -1,    -1,    -1,
      -1,  1078,  1079,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1088,   877,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1912,  1980,    -1,    -1,   890,   891,    -1,    -1,    -1,
      -1,    -1,  1109,    -1,  1111,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1940,
      -1,    -1,    -1,     0,    -1,    -1,     3,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2027,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1164,    -1,  1980,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   794,    -1,    -1,    -1,    -1,    -1,    -1,   801,
      -1,    -1,    -1,  1190,    -1,    -1,    -1,    -1,    -1,    -1,
    1197,    -1,  1199,  1200,    -1,    -1,    -1,    -1,    -1,    -1,
      77,    -1,    -1,    -1,  1211,    -1,  1213,    -1,  1215,    -1,
    1217,    -1,    -1,    -1,    -1,  1222,    -1,    13,    14,    15,
      16,    17,    -1,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,   876,    51,    -1,    53,    -1,   136,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,  1284,    -1,    -1,
      -1,    -1,    -1,  1127,  1291,  1292,    -1,    -1,    -1,    -1,
      -1,  1135,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1315,    -1,
      -1,    -1,    -1,    -1,    -1,  1322,    -1,    -1,    -1,  1326,
    1164,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1127,    -1,    -1,    -1,    -1,    -1,  1182,    -1,
    1135,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1356,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1202,    -1,
      -1,   238,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1164,
      -1,    -1,    -1,    -1,    -1,   252,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   262,    -1,  1182,    -1,  1396,
      -1,    -1,    -1,    -1,    -1,   272,    -1,    -1,    -1,    -1,
      -1,  1245,    -1,    -1,    -1,    -1,    -1,  1202,    -1,   286,
     287,    -1,    -1,    -1,    -1,    -1,   293,   294,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1432,    -1,    -1,    -1,    -1,
      -1,    -1,   309,  1440,    -1,  1442,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1245,    -1,   329,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1491,  1492,    -1,    -1,    48,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1506,
    1507,    -1,  1509,  1347,  1348,  1349,  1350,  1351,  1352,   386,
      -1,  1518,    -1,    -1,    74,    -1,    -1,    -1,  1140,  1141,
    1142,  1528,  1529,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1376,  1377,    -1,    -1,    -1,    -1,    -1,    -1,
     417,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1170,    -1,
      -1,    -1,  1347,  1348,  1349,  1350,  1351,  1352,    -1,    -1,
     120,    -1,    -1,  1185,    -1,    -1,   443,    -1,    -1,    -1,
     447,    -1,    -1,   133,    -1,   135,    -1,    -1,    -1,    -1,
      -1,  1376,  1377,    -1,    -1,    -1,    -1,    -1,    -1,   466,
      -1,    -1,    -1,   470,   471,    -1,    -1,   474,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   166,  1229,    -1,  1453,
      -1,    -1,   489,   490,   491,   492,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     190,   508,    -1,    -1,  1641,  1642,    -1,    -1,    -1,   516,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1654,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1453,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   544,   228,    -1,
      -1,    -1,   232,    -1,    -1,   235,   236,    -1,    -1,   239,
      -1,    -1,   242,   243,  1691,  1692,    -1,    -1,    -1,    -1,
      -1,   385,    -1,    -1,    -1,    -1,    -1,    -1,   575,    -1,
      -1,    -1,  1546,    -1,    -1,   582,    -1,    -1,    -1,    -1,
      -1,   588,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   612,   613,  1581,    -1,  1361,
      -1,    -1,  1364,   303,    -1,    -1,   306,    -1,    -1,    -1,
      -1,  1546,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   327,   328,    -1,
      -1,  1615,  1779,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   343,    -1,  1629,  1581,    -1,    -1,    -1,
      -1,    -1,    -1,  1800,    -1,    -1,  1803,  1804,    -1,    -1,
      -1,    -1,    -1,  1810,    -1,    -1,   683,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1660,    -1,    -1,    -1,
    1615,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     524,   525,    -1,    -1,  1629,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1660,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   752,    -1,    -1,   438,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     767,    -1,    -1,    -1,   771,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   780,  1748,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   802,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   493,   811,  1942,    -1,  1559,    -1,    -1,
     817,    -1,    -1,    -1,    -1,   505,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1748,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   662,    -1,
      -1,    -1,    -1,    -1,   668,    -1,    -1,   854,    -1,    -1,
      -1,    -1,    -1,   677,   861,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2006,
      -1,    -1,   696,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   888,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   579,
      -1,    -1,    -1,    -1,  2031,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   732,  2046,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  2062,    -1,    -1,    -1,    -1,
     620,   621,    -1,    -1,    -1,    -1,    -1,   944,  1912,    -1,
      -1,    -1,    -1,   633,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1940,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1912,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1980,    -1,    -1,    -1,
      -1,    -1,    -1,   164,    -1,  1940,    -1,    -1,    -1,    -1,
    1027,    -1,  1774,    -1,  1031,    -1,    -1,    -1,    -1,    -1,
      -1,  1038,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1048,    -1,    -1,    -1,    -1,    -1,    -1,  1055,   200,
     201,    -1,    -1,    -1,    -1,  1980,    -1,  1064,    -1,  1066,
      -1,    -1,    -1,    -1,    -1,    -1,   756,   757,    -1,    -1,
      -1,    -1,   762,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   232,    -1,    -1,    -1,    -1,    -1,    -1,   239,    -1,
      -1,  1098,    -1,   783,    -1,  1102,   786,   787,    -1,   789,
      49,   791,   792,    52,    -1,    54,    -1,    56,    -1,  1116,
      -1,    -1,  1119,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     830,    -1,    -1,    -1,   834,    -1,    -1,    -1,    -1,    -1,
      -1,   100,   101,    -1,   103,   306,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,    -1,
     119,   120,   121,    -1,   123,   124,    -1,    -1,    -1,   330,
     331,    -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    83,    -1,    -1,    -1,    -1,
     351,    -1,   151,  1210,   153,   154,   155,    -1,    -1,   899,
     900,    99,   161,   162,   163,   164,   165,   166,   167,    -1,
      -1,    -1,    -1,   913,    -1,    -1,    -1,    -1,  1235,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   149,    -1,    -1,    -1,   153,    -1,    -1,    -1,    -1,
      -1,   422,    -1,    -1,    -1,    -1,   164,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   438,   439,    -1,
     441,   442,    -1,    -1,    -1,    -1,    -1,    -1,   449,    -1,
     188,    -1,   453,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   201,    -1,    -1,   204,    -1,    -1,    -1,
      -1,    -1,  1329,    -1,    -1,    -1,  1333,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   494,    -1,    -1,    -1,   498,    -1,    -1,
      -1,    -1,    -1,    -1,  1044,    -1,  1363,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   253,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   266,   530,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1085,    -1,    -1,    -1,    -1,
      -1,    -1,  1092,    -1,    -1,  1095,  1413,    -1,    -1,  1416,
      -1,    -1,    -1,    -1,   302,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   310,    -1,    -1,    -1,    -1,  1434,    -1,   580,
      -1,    -1,   583,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   330,    -1,   332,    -1,    -1,    -1,    -1,    -1,
     601,   602,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   612,    -1,    -1,    -1,   616,    -1,    -1,    -1,    -1,
      -1,    -1,   623,    -1,   625,    -1,    -1,    -1,    -1,    -1,
    1487,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1496,
      -1,    -1,    -1,  1500,    -1,    -1,    -1,   385,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1514,  1515,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1218,    -1,
      -1,   419,    -1,    -1,    -1,    -1,  1226,  1227,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1373,
      -1,    -1,  1376,  1377,    -1,    -1,    -1,    -1,  1382,    -1,
      -1,   449,  1386,    -1,  1388,    -1,    -1,   455,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   736,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1285,    -1,    -1,    -1,    -1,
      -1,   752,   753,    -1,  1294,    -1,    -1,  1297,    -1,  1299,
    1300,   762,   763,    -1,   765,   766,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1630,  1631,    -1,   777,    -1,    -1,   780,
      -1,   782,   783,    -1,    -1,    -1,   524,   525,   789,    -1,
      -1,    -1,   530,    -1,    -1,    -1,    -1,    -1,   799,   800,
    1340,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   818,    -1,    -1,
      -1,   822,    -1,    -1,    -1,   826,    -1,    -1,    -1,   830,
     831,    -1,    -1,   834,   835,    -1,    -1,    -1,    -1,    -1,
      -1,   842,    -1,    -1,    -1,   583,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1532,    -1,
      -1,    -1,    -1,    -1,   602,    -1,   604,  1407,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   884,   885,    -1,    -1,   625,    -1,    -1,
    1564,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1576,    -1,    -1,  1762,    -1,    -1,    -1,    -1,
      -1,   649,    -1,    -1,   915,    -1,    -1,  1591,  1592,    -1,
      -1,    -1,    -1,    -1,   662,    -1,    -1,   665,   666,    -1,
     668,    -1,  1789,    -1,    -1,    -1,    -1,    -1,    -1,   677,
      -1,  1615,   680,   681,   682,    -1,    -1,    -1,    -1,    -1,
      -1,  1808,    -1,   954,    -1,  1495,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1835,    -1,
      -1,    -1,    -1,    -1,  1524,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   736,    -1,
      -1,    -1,    -1,    -1,    -1,  1862,    -1,    -1,  1865,    -1,
    1550,    -1,    -1,    -1,    -1,    -1,  1556,   188,    -1,    -1,
    1021,    -1,   760,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     201,    -1,    -1,    -1,    -1,    -1,    -1,  1038,  1039,   777,
      -1,    -1,    -1,   214,  1045,   216,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1731,    -1,    -1,
      -1,   799,   800,    -1,    -1,    -1,  1740,    -1,  1742,    -1,
      -1,  1745,  1746,    -1,  1748,    -1,    -1,    -1,    -1,  1753,
     818,    -1,    -1,    -1,    -1,  1625,  1087,    -1,    -1,    -1,
      -1,  1092,  1093,    -1,  1095,  1096,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1969,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     301,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   884,    -1,    -1,    -1,
      -1,    -1,    -1,   891,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   904,  1707,  1708,    -1,
      -1,    -1,    -1,    -1,    -1,  1715,    -1,    -1,  1852,  1719,
      -1,    -1,    -1,  1857,  1858,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    49,    -1,    -1,
      52,  1212,    54,    -1,    56,    -1,   954,  1218,  1219,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1241,    -1,    -1,    -1,    -1,    -1,  1920,    -1,  1922,    -1,
      -1,  1925,  1926,    -1,    -1,    -1,  1930,  1931,   100,   101,
      -1,   103,    -1,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,    -1,   119,   120,   121,
      -1,   123,   124,  1823,  1285,  1286,    -1,    -1,    -1,   131,
      -1,    -1,    -1,  1294,  1295,    -1,  1297,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   475,    -1,    -1,  1308,  1309,   151,
     481,    -1,   154,   155,    -1,   486,    -1,   159,    -1,   161,
     162,   163,   164,   165,   166,   167,    -1,  2001,  2002,  2003,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2022,  1087,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1898,    -1,
      -1,    -1,  2036,  2037,  2038,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1127,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   573,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1428,    -1,    -1,
      -1,   602,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1181,   615,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1455,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1202,    -1,    -1,    -1,  2008,    -1,
      -1,    -1,    -1,    -1,  1212,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   670,
      -1,    -1,    -1,  1241,    -1,    -1,    -1,  1245,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   694,   695,    -1,    -1,   698,    -1,   700,
      -1,    -1,    -1,    -1,    -1,   706,    -1,   708,   709,  1540,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     5,    -1,
      -1,    -1,    -1,    -1,    -1,  1556,    13,    14,    15,    16,
      17,    -1,    -1,    -1,    -1,   736,    -1,    -1,    -1,    -1,
    1308,  1309,    -1,    -1,    -1,    -1,    -1,    -1,   749,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   760,
      -1,    -1,    49,    -1,    -1,    52,    -1,    54,    -1,    56,
      -1,    -1,    -1,   774,    -1,    -1,   777,    -1,    -1,  1347,
    1348,  1349,    -1,  1351,  1352,    72,    73,    -1,    -1,    -1,
    1358,    -1,    -1,    -1,    -1,  1626,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   804,    -1,    -1,   807,    -1,    -1,    -1,
      -1,    -1,    -1,   100,   101,    -1,   103,    -1,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,    -1,   119,   120,   121,    -1,   123,   124,  1406,    -1,
      -1,    -1,   843,    -1,   131,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1685,  1686,    -1,    -1,    49,    -1,
    1428,    52,    -1,    54,   151,    56,    -1,   154,   155,  1700,
    1701,    -1,    -1,    -1,   161,   162,   163,   164,   165,   166,
     167,    -1,    73,    -1,    -1,  1716,    -1,    -1,    -1,    -1,
     891,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   904,   905,    -1,    -1,    -1,    -1,   100,
     101,   912,   103,    -1,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,    -1,   119,   120,
     121,    -1,   123,   124,    -1,    -1,    -1,    -1,     5,   940,
     131,    -1,    -1,    -1,    -1,    -1,    13,    14,    15,    16,
      17,    -1,    -1,   954,    -1,    -1,    -1,    -1,    -1,    -1,
     151,   962,    -1,   154,   155,    -1,    -1,   158,   969,    -1,
     161,   162,   163,   164,   165,   166,   167,    -1,  1546,    -1,
      -1,    -1,    49,    -1,    -1,    52,    -1,    54,    -1,    56,
    1821,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1829,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    73,    -1,    -1,    -1,
      -1,    -1,  1580,    -1,  1015,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   100,   101,    -1,   103,    -1,   105,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,    -1,   119,   120,   121,    -1,   123,   124,  1626,    -1,
      -1,    -1,    -1,  1894,   131,    -1,    -1,  1898,  1899,    -1,
      -1,  1902,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1082,    -1,  1084,   151,  1086,    -1,   154,   155,    -1,
      -1,    -1,  1660,    -1,   161,   162,   163,   164,   165,   166,
     167,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1943,    -1,    -1,    -1,    -1,  1685,  1686,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1152,  1153,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     3,    -1,    -1,    -1,    -1,  1735,    -1,    -1,
      -1,    -1,    13,    14,    15,    16,    17,  2008,  2009,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    -1,    50,
      51,    -1,    53,    -1,    55,  1216,    -1,    -1,    -1,  2050,
      -1,  1222,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    1241,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1258,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   106,   107,    -1,    -1,    -1,
      -1,    -1,  1273,    -1,    -1,  1276,    -1,    -1,    -1,    49,
      -1,    -1,    52,    -1,    54,    -1,    56,    -1,    -1,    -1,
     131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1866,    -1,
      -1,    -1,    -1,    73,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   154,   155,    -1,    -1,    -1,    -1,    -1,
      -1,   162,   163,    -1,    -1,    -1,  1894,  1328,    -1,    -1,
     100,   101,    -1,   103,    -1,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,    -1,   119,
     120,   121,    -1,   123,   124,    -1,    -1,    -1,    -1,    -1,
      -1,   131,    -1,    -1,    -1,  1366,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1941,    -1,    -1,  1944,    -1,    -1,    -1,
      -1,   151,   152,  1384,   154,   155,  1387,    -1,    -1,   159,
      -1,   161,   162,   163,   164,   165,   166,   167,     1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    18,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1428,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1438,  1439,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    49,  1448,    -1,    52,
      -1,    54,    -1,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1466,    -1,  1468,    71,    -1,
      73,    74,    -1,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    85,    86,    87,    88,    89,    90,    91,    92,
      93,    94,    95,    96,    -1,    98,    -1,   100,   101,    -1,
     103,    -1,   105,   106,   107,   108,   109,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,   121,    -1,
     123,   124,    -1,    -1,    -1,    -1,    -1,    -1,   131,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1546,    -1,   150,   151,    -1,
    1551,   154,   155,    18,    -1,    -1,   159,    -1,   161,   162,
     163,   164,   165,   166,   167,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   176,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    49,    -1,    -1,    52,    -1,    54,
      -1,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    71,    -1,    73,    74,
    1611,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    -1,    98,    -1,   100,   101,    -1,   103,    -1,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,  1658,   123,   124,
    1661,     1,    -1,    -1,    -1,    -1,   131,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    18,    -1,
      -1,    -1,    -1,    -1,    -1,   150,   151,    -1,    -1,   154,
     155,    -1,    -1,    -1,   159,    -1,   161,   162,   163,   164,
     165,   166,   167,    -1,    -1,    -1,    -1,    -1,    -1,    49,
      -1,   176,    52,    -1,    54,    -1,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    71,    -1,    73,    74,    -1,    76,    -1,    -1,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,    89,
      90,    91,    92,    93,    94,    95,    96,    -1,    98,    -1,
     100,   101,    -1,   103,    -1,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   118,   119,
     120,   121,    -1,   123,   124,    -1,    -1,    -1,    -1,    -1,
      -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   151,    -1,    -1,   154,   155,    -1,    -1,    -1,   159,
      -1,   161,   162,   163,   164,   165,   166,   167,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   176,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    -1,    20,  1874,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    49,    -1,    51,    52,    53,    54,    -1,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    65,    -1,    -1,
      -1,    69,    -1,    71,    72,    73,    74,    -1,    76,    -1,
      -1,    79,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    90,    91,    92,    93,    94,    95,    96,    -1,
      98,    -1,   100,   101,   102,   103,    -1,   105,   106,   107,
     108,   109,   110,   111,   112,   113,   114,   115,   116,   117,
     118,   119,   120,   121,    -1,   123,   124,    -1,    -1,    -1,
      -1,    -1,    -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   150,   151,    -1,    -1,   154,   155,    -1,    -1,
      -1,   159,    -1,   161,   162,   163,   164,   165,   166,   167,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   176,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    49,    -1,    51,    52,    53,
      54,    -1,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    -1,    -1,    -1,    69,    -1,    71,    72,    73,
      74,    -1,    76,    -1,    -1,    79,    80,    81,    82,    83,
      84,    85,    86,    87,    88,    89,    90,    91,    92,    93,
      94,    95,    96,    -1,    98,    -1,   100,   101,   102,   103,
      -1,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,   118,   119,   120,   121,    -1,   123,
     124,    -1,    -1,    -1,    -1,    -1,    -1,   131,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,    -1,    -1,
     154,   155,    -1,    -1,    -1,   159,    -1,   161,   162,   163,
     164,   165,   166,   167,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   176,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    49,
      -1,    51,    52,    53,    54,    -1,    56,    57,    58,    59,
      60,    61,    62,    63,    64,    65,    -1,    -1,    -1,    69,
      -1,    -1,    72,    73,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     100,   101,   102,   103,    -1,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,    -1,   119,
     120,   121,    -1,   123,   124,    -1,    -1,    -1,    -1,    -1,
      -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   145,   146,   147,    -1,    -1,
      -1,   151,   152,   153,   154,   155,    -1,    -1,    -1,    -1,
      -1,   161,   162,   163,   164,   165,   166,   167,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   176,     3,     4,     5,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   145,
     146,   147,    -1,    -1,    -1,   151,   152,    -1,   154,   155,
      -1,    -1,    -1,    -1,    -1,   161,   162,   163,   164,   165,
     166,   167,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     176,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    49,    -1,    51,
      52,    53,    54,    -1,    56,    57,    58,    59,    60,    61,
      62,    63,    64,    65,    -1,    -1,    -1,    69,    -1,    -1,
      72,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,   101,
     102,   103,    -1,   105,   106,   107,   108,   109,   110,   111,
     112,   113,   114,   115,   116,   117,    -1,   119,   120,   121,
      -1,   123,   124,    -1,    -1,    -1,    -1,    -1,    -1,   131,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,
      -1,   153,   154,   155,    -1,    -1,    -1,    -1,    -1,   161,
     162,   163,   164,   165,   166,   167,     3,     4,     5,     6,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   151,    -1,    -1,   154,   155,    -1,
      -1,    -1,    -1,    -1,   161,   162,   163,   164,   165,   166,
     167,     4,     5,     6,     7,     8,     9,    10,    11,    12,
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
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    -1,    -1,
      49,    -1,    51,    52,    53,    54,    -1,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    73,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   100,   101,    -1,   103,    -1,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,    -1,
     119,   120,   121,    -1,   123,   124,    -1,    -1,    -1,    -1,
      -1,    -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   151,    -1,    -1,   154,   155,    -1,    -1,    -1,
      -1,    -1,   161,   162,   163,   164,   165,   166,   167,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    -1,    -1,    49,    -1,    51,    52,    53,    54,
      -1,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    73,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   100,   101,    -1,   103,    -1,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,    -1,   119,   120,   121,    -1,   123,   124,
      -1,    -1,    -1,    -1,    -1,    -1,   131,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   151,    -1,    -1,   154,
     155,    -1,    -1,    -1,    -1,    -1,   161,   162,   163,   164,
     165,   166,   167,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
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
     161,   162,   163,   164,   165,   166,   167,     1,    -1,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    47,    48,    -1,    50,    51,    -1,    53,
      -1,    55,    -1,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    -1,    -1,    -1,    69,    -1,    -1,    72,    -1,
      -1,    -1,    -1,    77,    78,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   102,    -1,
      -1,    -1,   106,   107,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   131,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   150,    -1,    -1,    -1,
     154,   155,    -1,    -1,    -1,    -1,    -1,    -1,   162,   163,
       1,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    -1,    50,
      51,    -1,    53,    -1,    55,    -1,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    -1,    -1,    -1,    69,    -1,
      -1,    72,    -1,    -1,    -1,    -1,    77,    78,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   102,    -1,    -1,    -1,   106,   107,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   150,
      -1,    -1,    -1,   154,   155,    -1,    -1,    -1,    -1,    -1,
      -1,   162,   163,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,
      60,    61,    62,    63,    64,    65,    -1,    -1,    -1,    69,
      -1,    -1,    72,    73,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    98,    -1,
      -1,    -1,   102,   103,    -1,    -1,   106,   107,   108,   109,
     110,   111,   112,   113,   114,    -1,    -1,    -1,   118,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   151,   152,    -1,   154,   155,    -1,    -1,    -1,    -1,
      -1,    -1,   162,   163,     3,     4,     5,     6,     7,     8,
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
     159,    -1,    -1,   162,   163,     3,     4,     5,     6,     7,
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
      16,    17,    -1,    -1,    20,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    -1,    50,    51,    -1,    53,    -1,    55,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,
     106,   107,   108,   109,   110,   111,   112,   113,   114,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   131,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   151,   152,    -1,   154,   155,
      -1,     3,    -1,    -1,    -1,    -1,   162,   163,    10,    -1,
      -1,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,
      -1,    53,    -1,    -1,    -1,    57,    58,    59,    60,    61,
      62,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    73,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     102,    -1,    -1,    -1,   106,   107,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   131,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   150,    -1,
      -1,    -1,   154,   155,    -1,     3,    -1,    -1,    -1,    -1,
     162,   163,    10,    -1,    -1,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,    -1,
      -1,    -1,    -1,    51,    -1,    53,    -1,    -1,    -1,    57,
      58,    59,    60,    61,    62,    63,    64,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    73,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   102,    -1,    -1,    -1,   106,   107,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   150,    -1,    -1,    -1,   154,   155,    -1,     3,
      -1,    -1,    -1,    -1,   162,   163,    10,    -1,    -1,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,    53,
      -1,    -1,    -1,    57,    58,    59,    60,    61,    62,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    73,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   102,    -1,
      -1,    -1,   106,   107,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   131,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   150,    -1,    -1,    -1,
     154,   155,    -1,     3,    -1,    -1,    -1,    -1,   162,   163,
      10,    -1,    -1,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    -1,    -1,    -1,
      -1,    51,    -1,    53,    -1,    -1,    -1,    57,    58,    59,
      60,    61,    62,    63,    64,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    73,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   102,    -1,    -1,    -1,   106,   107,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     150,    -1,    -1,    -1,   154,   155,    -1,    -1,    -1,    -1,
      -1,    -1,   162,   163,     3,     4,     5,     6,     7,     8,
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
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,    -1,    -1,    -1,    -1,    51,    -1,
      53,   154,    -1,    -1,    57,    58,    59,    60,    61,    62,
      63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     103,    -1,    -1,   106,   107,   108,   109,   110,   111,   112,
     113,   114,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    -1,    -1,    -1,    -1,
      51,   154,    53,    -1,    -1,    -1,    57,    58,    59,    60,
      61,    62,    63,    64,    -1,    49,    -1,    -1,    52,    -1,
      54,    72,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   106,   107,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   100,   101,    -1,   103,
      -1,   105,   106,   107,   108,   109,   110,   111,   112,   113,
     114,   115,   116,   117,    -1,   119,   120,   121,    -1,   123,
     124,    49,    -1,    -1,    52,    -1,    54,   131,    56,    -1,
      -1,    -1,    -1,   154,    -1,    -1,    -1,    -1,    -1,    -1,
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
     152,    -1,   154,   155,    -1,    -1,    -1,    -1,    -1,   161,
     162,   163,   164,   165,   166,   167,    -1,    -1,    -1,    -1,
      -1,   100,   101,    -1,   103,    -1,   105,   106,   107,   108,
     109,   110,   111,   112,   113,   114,   115,   116,   117,    -1,
     119,   120,   121,    49,   123,   124,    52,    -1,    54,    -1,
      56,    -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    -1,
      -1,    -1,   151,    -1,    -1,   154,   155,    -1,    -1,    -1,
     159,    -1,   161,   162,   163,   164,   165,   166,   167,    -1,
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
      -1,   151,    -1,    -1,   154,   155,    -1,    -1,    -1,   159,
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
      -1,    -1,   151,    -1,    -1,   154,   155,    -1,    -1,    -1,
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
     121,    -1,   123,   124,    -1,    -1,    -1,    -1,    -1,    -1,
     131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     151,    -1,    -1,   154,   155,    -1,    -1,    -1,    -1,    -1,
     161,   162,   163,   164,   165,   166,   167,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,    -1,    20,    -1,    22,    23,    24,    25,    26,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      -1,    -1,    -1,    -1,    51,    -1,    53,    -1,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    -1,    -1,    20,    72,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    -1,    -1,    -1,    -1,    51,   103,    53,    -1,   106,
     107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,
     106,   107,    13,    14,    15,    16,    17,    -1,    -1,    20,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    -1,    50,
      51,    -1,    53,    -1,    55,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    13,    14,    15,    16,    17,    -1,    -1,
      20,    72,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    44,    45,    46,    47,    48,    -1,
      50,    51,    -1,    53,    -1,    55,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72
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
     446,   464,   465,   466,   467,     0,   178,   103,   182,   198,
     285,   287,   296,   299,   311,   315,   320,   117,   151,    58,
      61,    62,    64,   151,   151,   408,   409,   410,   307,   308,
     106,   107,   182,   363,   384,   385,   363,   151,   396,   151,
     151,     4,   103,   106,   107,   300,   305,   306,   151,   198,
     409,   414,   420,   421,   422,   424,   425,   426,   106,   322,
     156,   178,   288,   296,   299,   419,   423,   463,   464,   467,
     468,   176,   179,   148,   159,   175,   219,   366,    89,   157,
     403,   363,   179,   179,   179,   176,   106,   107,   151,   198,
     293,   405,   414,   415,   416,   417,   418,   419,   423,   427,
     428,   429,   430,   431,   437,     3,    47,    48,    50,    55,
     313,     3,   155,   198,   287,   300,   304,   306,   316,   321,
     399,   419,   423,   467,   285,   287,   299,   311,   315,   320,
     400,   419,   423,    65,   305,   305,   300,   306,   305,   300,
     305,   300,   154,   408,   157,   179,   151,   159,   227,   408,
     408,   178,   276,   277,   155,   296,   299,   465,   363,   363,
     396,   175,   299,   151,   198,   405,   414,   419,   428,   155,
     198,   467,   397,   398,    65,    66,    67,    68,   155,   173,
     363,   372,   374,   378,   380,   381,   321,    57,   153,   155,
     198,   295,   299,   303,   304,   310,   311,   317,   318,   319,
     320,   324,   331,   332,   349,   359,   361,   446,   459,   460,
     461,   462,   467,   468,   106,   107,   159,   182,   321,   437,
     410,   151,   379,   380,   151,   151,   117,   184,   185,    49,
      52,    54,    56,    73,   100,   101,   103,   105,   115,   116,
     119,   120,   121,   123,   124,   151,   155,   161,   164,   165,
     166,   167,   180,   181,   184,   186,   189,   197,   198,   199,
     200,   203,   204,   205,   206,   207,   208,   209,   210,   211,
     212,   213,   214,   215,   221,   321,   153,   155,   197,   198,
     214,   216,   296,   321,   364,   365,   382,   463,   468,   299,
     420,   421,   422,   424,   425,   426,   153,   153,   153,   153,
     153,   153,   153,   155,   296,   446,   465,   155,   162,   198,
     216,   287,   288,   295,   297,   299,   311,   318,   320,   354,
     355,   358,   359,   360,   459,   467,   151,   419,   423,   467,
     151,   157,   103,   154,   155,   159,   181,   183,   216,   367,
     368,   369,   370,   371,    22,   367,   151,   363,   227,   151,
     157,   157,   157,   409,   414,   416,   417,   418,   427,   429,
     430,   431,   299,   415,   428,   157,    98,   407,   155,   408,
     445,   446,   408,   408,   403,   276,   151,   408,   445,   403,
     408,   408,   299,   405,   151,   151,   298,   299,   296,   299,
     178,   296,   463,   468,   323,   159,   403,   276,   363,   366,
     287,   304,   401,   419,   423,   159,   403,   276,   384,   299,
     311,   299,   299,   106,   322,   106,   107,   182,   321,   326,
     384,   178,   182,   362,   150,   178,     3,   292,   294,   299,
     303,   227,   178,   178,   407,   151,   407,   179,   216,   409,
     414,   299,   151,   178,   363,   394,   159,   363,   159,   363,
     133,   162,   163,   377,   153,   157,   363,   381,   153,   408,
     408,   156,   178,   297,   299,   311,   318,   320,   458,   459,
     467,   468,   151,   155,   163,   175,   198,   446,   448,   449,
     450,   451,   452,   453,   470,   198,   324,   467,   299,   318,
     305,   300,   408,   153,   297,   299,   460,   297,   446,   460,
      10,   161,   166,   348,   350,   351,   346,   348,   372,   175,
     372,    13,    88,   103,   106,   107,   181,   411,   412,   413,
     153,   117,   151,   197,   151,   151,   151,   200,   151,   197,
     151,   103,   106,   107,   300,   305,   306,   151,   197,   197,
      19,    21,    85,   155,   164,   165,   201,   202,   216,   223,
     227,   334,   364,   467,   157,   178,   151,   186,   155,   160,
     155,   160,   120,   122,   123,   124,   151,   154,   155,   159,
     160,   200,   200,   168,   162,   169,   170,   164,   165,   125,
     126,   127,   128,   171,   172,   129,   130,   163,   161,   173,
     131,   132,   174,   153,   157,   154,   178,   134,   135,   136,
     137,   138,   139,   140,   141,   142,   143,   144,   175,   218,
     219,   220,   151,   198,   441,   442,   443,   444,   445,   153,
     157,   153,   153,   153,   153,   153,   153,   151,   408,   445,
     446,   151,   445,   446,   178,   296,   465,   178,   179,   179,
     151,   163,   198,   414,   432,   433,   434,   435,   436,   437,
     438,   439,   440,   133,   467,   179,   179,   363,   363,   178,
     178,   178,   155,   183,   178,   368,   158,   157,   469,   367,
     154,   155,   158,   371,   152,   216,   222,   151,   178,   178,
     178,   178,   414,   416,   417,   418,   427,   429,   430,   431,
     153,   153,   153,   153,   153,   153,   153,   415,   428,   408,
     151,   366,   156,   178,   227,   403,   178,   227,   405,   223,
     365,   223,   365,   405,   394,   227,   403,   407,   159,   403,
     276,   394,   227,   403,   328,   329,   327,   159,   133,   299,
     356,   357,   360,   361,   153,   157,    70,   278,   279,   179,
     299,   292,   162,   216,   178,   414,   355,   396,   394,   156,
     178,   151,   376,   374,   375,    78,   309,   182,   159,   182,
     437,   297,   446,   460,   299,   303,   467,   178,   449,   450,
     451,   156,   178,    18,   216,   299,   448,   470,   408,   408,
     446,   297,   458,   468,   299,   182,   408,   297,   460,   321,
     157,   469,   363,   348,   159,   153,   365,   153,   153,   157,
     151,   176,   364,   186,   155,   364,   364,   364,   216,   364,
     153,   364,   364,   364,   178,   153,   164,   165,   202,    18,
     301,   153,   157,   153,   162,   163,   153,   222,   216,   159,
     216,   182,   216,   182,   115,   155,   182,   152,   190,   191,
     192,   216,   115,   155,   182,   334,   216,   190,   182,   200,
     203,   203,   203,   204,   204,   205,   205,   206,   206,   206,
     206,   207,   207,   208,   209,   210,   211,   212,   158,   223,
     176,   184,   155,   182,   216,   159,   216,   178,   442,   443,
     444,   299,   441,   408,   408,   216,   365,   151,   408,   445,
     446,   151,   445,   446,   178,   178,   156,   156,   151,   414,
     433,   434,   435,   438,    18,   299,   432,   436,   151,   408,
     452,   470,   408,   408,   470,   151,   408,   452,   408,   408,
     179,   215,   363,   156,   157,   156,   157,   470,   470,   133,
     353,   354,   355,   353,   363,   178,   214,   215,   216,   406,
     469,   367,   369,   150,   178,   153,   157,   178,   353,   182,
     405,   182,   153,   153,   153,   153,   153,   153,   151,   408,
     445,   446,   151,   408,   445,   446,   405,   184,   446,   216,
     227,   356,   153,   153,   153,   153,   392,   393,   227,   394,
     227,   403,   393,   227,   159,   159,   159,   335,   179,   179,
     182,   280,   363,    18,    71,    73,    76,    79,    80,    81,
      82,    83,    84,    85,    86,    87,    88,    89,    90,    92,
      93,    94,    95,    96,    98,   106,   107,   118,   178,   223,
     224,   225,   226,   227,   228,   229,   231,   232,   242,   248,
     249,   250,   251,   252,   253,   258,   259,   265,   266,   267,
     281,   299,   303,   363,   404,    70,   176,   179,   179,   179,
     353,   179,   395,   393,   285,   287,   296,   387,   388,   389,
     390,   382,   175,   373,   373,   350,   408,   408,   297,   460,
     155,   162,   198,   216,   321,   216,   299,   356,   153,   153,
     153,     5,   299,   408,   448,   159,   182,   437,    10,   351,
     150,   175,   352,   159,   350,   159,   153,   412,   190,   153,
     157,   178,   157,   153,   153,   157,   153,   200,   153,   153,
     153,   200,    18,   301,   216,   153,   153,   152,   159,   200,
     156,   179,   190,   156,   156,   115,   119,   121,   183,   193,
     194,   195,   153,   157,   193,   156,   157,   150,   214,   158,
     153,   193,   179,   368,   356,   153,   153,   153,   441,   178,
     178,   356,   356,   438,   153,   153,   153,   153,   151,   414,
     437,   432,   436,   178,   178,   156,   179,   470,   178,   178,
     179,   179,   179,   179,   366,   193,   133,   167,   179,   179,
     150,   367,   216,   408,   152,   216,   353,   179,   175,   151,
     408,   445,   446,   151,   408,   445,   446,   178,   178,   407,
     153,   179,   179,   395,   393,   227,   395,   335,   335,   335,
       3,    10,    73,   150,   282,   289,   290,   296,   299,   336,
     341,   463,   153,   157,   157,   176,   151,    61,    62,   176,
     227,   281,   404,   151,    18,   225,   151,   151,   176,   363,
     176,   363,   162,   363,   159,   224,   151,   151,   151,   227,
     216,   217,   217,    14,   268,    74,   233,   176,   179,   229,
      78,   176,   363,    91,   254,   362,   299,   158,   280,   176,
     156,   156,   179,   157,   395,   405,   179,   176,   179,   176,
     179,   153,   365,   379,   379,   469,   348,   348,   178,   179,
     179,   179,   216,   179,   151,   408,   452,   446,   298,     5,
     162,   179,   216,   350,   408,   408,   321,   363,   159,   215,
     350,   469,   150,   178,   153,   295,   182,    78,   187,   188,
     364,   200,   200,   200,   200,   200,   159,   368,   157,   150,
     196,   155,   194,   196,   196,   156,   157,   122,   154,   192,
     156,   222,   214,   176,   156,   469,   179,   151,   408,   445,
     446,   356,   356,   179,   179,   153,   151,   408,   445,   446,
     151,   408,   452,   414,   408,   408,   356,   356,   156,   355,
     358,   358,   359,   153,   157,   157,   153,   179,   215,   215,
     156,   156,   179,   179,   153,   216,   178,   178,   356,   356,
     366,   408,   157,   153,   150,   395,   150,   150,   150,   150,
     296,   334,   342,   463,   296,   341,   151,   330,   176,   176,
     151,   158,   198,   337,   338,   344,   414,   415,   428,   157,
     176,   363,   178,   363,   153,   190,   191,   176,   227,   176,
     227,   223,    80,   153,   223,   234,   281,   283,   286,   292,
     299,   303,   145,   146,   147,   152,   153,   176,   223,   243,
     244,   245,   281,   176,   176,   223,   176,   368,   176,   223,
     222,   223,   110,   111,   112,   113,   114,   260,   262,   263,
     176,    97,   176,    84,   151,   151,   179,   150,   176,   176,
     151,   225,   227,   408,   176,   153,   178,   150,   150,   178,
     157,   157,   150,   159,   159,   156,   156,   156,   179,   153,
     178,   216,   216,   179,   156,   179,   469,   347,   348,   352,
     352,   368,   469,   150,   387,   447,   448,   153,   158,   153,
     157,   158,   368,   469,   222,   120,   193,   194,   155,   194,
     155,   194,   156,   150,   153,   178,   179,   179,   153,   153,
     178,   178,   179,   179,   179,   178,   178,   156,   179,   153,
     408,   356,   356,   179,   179,   223,   150,   330,   330,   330,
     151,   198,   339,   340,   445,   454,   455,   456,   457,   176,
     157,   176,   337,   176,   382,   409,   414,   216,   299,   157,
     176,   343,   344,   343,   363,   133,   360,   361,   223,   153,
     153,   151,   225,   153,   223,   299,   145,   146,   147,   167,
     176,   246,   247,   225,   224,   176,   247,   153,   158,   223,
     152,   223,   224,   245,   176,   469,   153,   153,   153,   227,
     262,   263,   151,   216,   151,   184,   234,   200,   255,   109,
       1,   225,   408,   388,   178,   178,   350,   350,   156,   356,
     179,   179,   156,   156,   150,   348,   159,   469,   150,   179,
     153,   216,   188,   216,   469,   150,   156,   156,   193,   193,
     356,   153,   153,   356,   356,   153,   153,   156,   157,   133,
     355,   133,   156,   179,   179,   153,   153,   156,   455,   456,
     457,   299,   454,   157,   176,   408,   408,   176,   153,   414,
     408,   176,   225,    77,    78,   159,   237,   238,   239,   153,
     223,    75,   225,   223,   152,   223,    75,   176,   106,   152,
     223,   224,   245,   152,   223,   225,   244,   247,   247,   176,
     223,   150,   159,   239,   225,   151,   178,   176,   184,   153,
     158,   153,   153,   157,   158,   253,   257,   363,   405,   469,
     469,   179,   156,   156,   159,   350,   150,   150,   150,   156,
     156,   179,   179,   179,   178,   179,   153,   153,   153,   153,
     153,   454,   408,   338,     1,   215,   235,   236,   406,     1,
     158,     1,   178,   225,   237,    75,   176,   153,   225,    75,
     176,   167,   167,   225,   224,   247,   247,   176,   106,   223,
     167,   167,    75,   152,   223,   152,   223,   224,   176,     1,
     178,   178,   264,   297,   299,   463,   158,   176,   155,   184,
     269,   270,   271,   225,   200,   190,    75,   108,   254,   256,
     150,   150,   153,   350,   469,   153,   153,   153,   358,   151,
     408,   445,   446,   340,   133,     1,   157,   158,   150,   274,
     275,   281,   225,    75,   176,   225,   223,   152,   152,   223,
     152,   223,   152,   223,   224,   152,   223,   152,   223,   225,
     167,   167,   167,   167,   150,   274,   264,   179,   151,   198,
     405,   454,   182,   158,   103,   151,   153,   158,   157,    75,
     153,   225,   151,   225,   225,   469,   150,   178,   215,   235,
     238,   240,   241,   281,   225,   167,   167,   167,   167,   152,
     152,   223,   152,   223,   152,   223,   240,   179,   176,   261,
     299,   269,   156,   215,   176,   269,   271,   225,   223,   109,
     109,   150,   356,   225,   230,   179,   238,   152,   152,   223,
     152,   223,   152,   223,   179,   261,   214,   153,   158,   184,
     153,   153,   158,   153,   257,    75,   252,   179,     1,   225,
     150,   230,   150,   153,   227,   184,   272,   151,   176,   272,
     225,    75,   153,   227,   157,   158,   215,   153,   225,   184,
     182,   273,   153,   176,   153,   157,   176,   182
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
     445,   445,   446,   446,   446,   446,   447,   447,   448,   448,
     448,   448,   449,   449,   449,   449,   449,   450,   450,   450,
     450,   451,   451,   451,   452,   452,   452,   453,   453,   453,
     453,   453,   453,   454,   454,   454,   455,   455,   455,   455,
     455,   456,   456,   456,   456,   457,   457,   458,   458,   458,
     459,   459,   460,   460,   460,   460,   460,   460,   461,   461,
     461,   461,   461,   461,   461,   461,   461,   461,   462,   462,
     462,   462,   463,   463,   463,   464,   464,   465,   465,   465,
     465,   465,   465,   466,   466,   466,   466,   466,   466,   467,
     467,   467,   468,   468,   469,   469,   470,   470
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
       7,     1,     5,     5,     6,     6,     0,     1,     1,     3,
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
#line 583 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.enterScope(); }
#line 7696 "Parser/parser.cc"
    break;

  case 3:
#line 587 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.leaveScope(); }
#line 7702 "Parser/parser.cc"
    break;

  case 4:
#line 594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantInteger( *(yyvsp[0].tok) ) ); }
#line 7708 "Parser/parser.cc"
    break;

  case 5:
#line 595 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7714 "Parser/parser.cc"
    break;

  case 6:
#line 596 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7720 "Parser/parser.cc"
    break;

  case 7:
#line 597 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantFloat( *(yyvsp[0].tok) ) ); }
#line 7726 "Parser/parser.cc"
    break;

  case 8:
#line 598 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = new ExpressionNode( build_constantChar( *(yyvsp[0].tok) ) ); }
#line 7732 "Parser/parser.cc"
    break;

  case 19:
#line 619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { Token tok = { new string( DeclarationNode::anonymous.newName() ), yylval.tok.loc }; (yyval.tok) = tok; }
#line 7738 "Parser/parser.cc"
    break;

  case 20:
#line 623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.constant) = build_constantStr( *(yyvsp[0].str) ); }
#line 7744 "Parser/parser.cc"
    break;

  case 21:
#line 627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.str) = (yyvsp[0].tok); }
#line 7750 "Parser/parser.cc"
    break;

  case 22:
#line 629 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ! appendStr( *(yyvsp[-1].str), *(yyvsp[0].tok) ) ) YYERROR;		// append 2nd juxtaposed string to 1st
			delete (yyvsp[0].tok);									// allocated by lexer
			(yyval.str) = (yyvsp[-1].str);									// conversion from tok to str
		}
#line 7760 "Parser/parser.cc"
    break;

  case 23:
#line 640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 7766 "Parser/parser.cc"
    break;

  case 24:
#line 642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 7772 "Parser/parser.cc"
    break;

  case 25:
#line 646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_dimensionref( (yyvsp[0].tok) ) ); }
#line 7778 "Parser/parser.cc"
    break;

  case 27:
#line 649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 7784 "Parser/parser.cc"
    break;

  case 28:
#line 651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new StmtExpr( dynamic_cast<CompoundStmt *>(maybeMoveBuild<Statement>((yyvsp[-1].sn)) ) ) ); }
#line 7790 "Parser/parser.cc"
    break;

  case 29:
#line 653 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_qualified_expr( (yyvsp[-2].decl), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7796 "Parser/parser.cc"
    break;

  case 30:
#line 655 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualified name is currently unimplemented." ); (yyval.en) = nullptr; }
#line 7802 "Parser/parser.cc"
    break;

  case 31:
#line 657 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// add the missing control expression to the GenericExpr and return it
			(yyvsp[-1].genexpr)->control = maybeMoveBuild<Expression>( (yyvsp[-3].en) );
			(yyval.en) = new ExpressionNode( (yyvsp[-1].genexpr) );
		}
#line 7812 "Parser/parser.cc"
    break;

  case 32:
#line 667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, "n expression" ); (yyval.en) = nullptr; }
#line 7818 "Parser/parser.cc"
    break;

  case 33:
#line 669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.en) = nullptr; }
#line 7824 "Parser/parser.cc"
    break;

  case 34:
#line 671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.en) = nullptr; }
#line 7830 "Parser/parser.cc"
    break;

  case 35:
#line 673 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.en) = nullptr; }
#line 7836 "Parser/parser.cc"
    break;

  case 36:
#line 675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.en) = nullptr; }
#line 7842 "Parser/parser.cc"
    break;

  case 37:
#line 677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.en) = nullptr; }
#line 7848 "Parser/parser.cc"
    break;

  case 39:
#line 683 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// steal the association node from the singleton and delete the wrapper
			(yyvsp[-2].genexpr)->associations.splice((yyvsp[-2].genexpr)->associations.end(), (yyvsp[0].genexpr)->associations);
			delete (yyvsp[0].genexpr);
			(yyval.genexpr) = (yyvsp[-2].genexpr);
		}
#line 7859 "Parser/parser.cc"
    break;

  case 40:
#line 693 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// create a GenericExpr wrapper with one association pair
			(yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuildType((yyvsp[-2].decl)), maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } );
		}
#line 7868 "Parser/parser.cc"
    break;

  case 41:
#line 698 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.genexpr) = new GenericExpr( nullptr, { { maybeMoveBuild<Expression>( (yyvsp[0].en) ) } } ); }
#line 7874 "Parser/parser.cc"
    break;

  case 43:
#line 707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-5].en), new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) ) ) )) ) ); }
#line 7880 "Parser/parser.cc"
    break;

  case 44:
#line 713 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7886 "Parser/parser.cc"
    break;

  case 45:
#line 715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7892 "Parser/parser.cc"
    break;

  case 46:
#line 717 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Index, new ExpressionNode( (yyvsp[-3].constant) ), (yyvsp[-1].en) ) ); }
#line 7898 "Parser/parser.cc"
    break;

  case 47:
#line 719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new std::string( "?{}" );			// location undefined - use location of '{'?
			(yyval.en) = new ExpressionNode( new ConstructorExpr( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) ) );
		}
#line 7908 "Parser/parser.cc"
    break;

  case 48:
#line 725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 7914 "Parser/parser.cc"
    break;

  case 49:
#line 728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( new string( "__builtin_va_arg") ) ),
											   (ExpressionNode *)((yyvsp[-4].en)->set_last( (ExpressionNode *)((yyvsp[-1].decl) ? (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) ) : (yyvsp[-2].decl)) )) ) ); }
#line 7921 "Parser/parser.cc"
    break;

  case 50:
#line 731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 7927 "Parser/parser.cc"
    break;

  case 51:
#line 733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), (yyvsp[-2].en) ) ); }
#line 7933 "Parser/parser.cc"
    break;

  case 52:
#line 735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( build_postfix_name( (yyvsp[0].tok) ) ) ), new ExpressionNode( (yyvsp[-2].constant) ) ) ); }
#line 7939 "Parser/parser.cc"
    break;

  case 53:
#line 737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7945 "Parser/parser.cc"
    break;

  case 54:
#line 739 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 7951 "Parser/parser.cc"
    break;

  case 55:
#line 741 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-1].en), build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) ) ) ); }
#line 7957 "Parser/parser.cc"
    break;

  case 56:
#line 743 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7963 "Parser/parser.cc"
    break;

  case 57:
#line 745 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[0].aggKey), (yyvsp[-2].en) ) ); }
#line 7969 "Parser/parser.cc"
    break;

  case 58:
#line 747 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_varref( (yyvsp[0].tok) ) ) ); }
#line 7975 "Parser/parser.cc"
    break;

  case 59:
#line 749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), build_constantInteger( *(yyvsp[0].tok) ) ) ); }
#line 7981 "Parser/parser.cc"
    break;

  case 60:
#line 751 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 7987 "Parser/parser.cc"
    break;

  case 61:
#line 753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::IncrPost, (yyvsp[-1].en) ) ); }
#line 7993 "Parser/parser.cc"
    break;

  case 62:
#line 755 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::DecrPost, (yyvsp[-1].en) ) ); }
#line 7999 "Parser/parser.cc"
    break;

  case 63:
#line 757 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-5].decl), new InitializerNode( (yyvsp[-2].in), true ) ) ); }
#line 8005 "Parser/parser.cc"
    break;

  case 64:
#line 759 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_compoundLiteral( (yyvsp[-6].decl), (new InitializerNode( (yyvsp[-2].in), true ))->set_maybeConstructed( false ) ) ); }
#line 8011 "Parser/parser.cc"
    break;

  case 65:
#line 761 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Token fn;
			fn.str = new string( "^?{}" );				// location undefined
			(yyval.en) = new ExpressionNode( build_func( new ExpressionNode( build_varref( fn ) ), (ExpressionNode *)( (yyvsp[-3].en) )->set_last( (yyvsp[-1].en) ) ) );
		}
#line 8021 "Parser/parser.cc"
    break;

  case 66:
#line 770 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8027 "Parser/parser.cc"
    break;

  case 69:
#line 777 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 8033 "Parser/parser.cc"
    break;

  case 70:
#line 782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Default parameter for argument is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8039 "Parser/parser.cc"
    break;

  case 73:
#line 789 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 8045 "Parser/parser.cc"
    break;

  case 75:
#line 795 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-1].tok) ) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 8051 "Parser/parser.cc"
    break;

  case 76:
#line 797 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( new ExpressionNode( build_field_name_FLOATING_DECIMALconstant( *(yyvsp[-3].tok) ) ), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 8057 "Parser/parser.cc"
    break;

  case 77:
#line 799 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 8063 "Parser/parser.cc"
    break;

  case 78:
#line 801 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_fieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 8069 "Parser/parser.cc"
    break;

  case 79:
#line 803 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-2].en), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 8075 "Parser/parser.cc"
    break;

  case 80:
#line 805 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_pfieldSel( (yyvsp[-4].en), build_tuple( (yyvsp[-1].en) ) ) ); }
#line 8081 "Parser/parser.cc"
    break;

  case 81:
#line 810 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_constantInteger( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 8087 "Parser/parser.cc"
    break;

  case 82:
#line 812 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_field_name_FLOATINGconstant( *(yyvsp[-1].tok) ), (yyvsp[0].en) ) ); }
#line 8093 "Parser/parser.cc"
    break;

  case 83:
#line 814 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.en) = new ExpressionNode( build_field_name_fraction_constants( build_varref( (yyvsp[-1].tok) ), (yyvsp[0].en) ) );
		}
#line 8101 "Parser/parser.cc"
    break;

  case 84:
#line 821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8107 "Parser/parser.cc"
    break;

  case 85:
#line 823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			Expression * constant = build_field_name_FLOATING_FRACTIONconstant( *(yyvsp[0].tok) );
			(yyval.en) = (yyvsp[-1].en) != nullptr ? new ExpressionNode( build_fieldSel( (yyvsp[-1].en),  constant ) ) : new ExpressionNode( constant );
		}
#line 8116 "Parser/parser.cc"
    break;

  case 88:
#line 835 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 8122 "Parser/parser.cc"
    break;

  case 89:
#line 837 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en)->set_extension( true ); }
#line 8128 "Parser/parser.cc"
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
#line 8148 "Parser/parser.cc"
    break;

  case 91:
#line 858 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_val( (yyvsp[-1].op), (yyvsp[0].en) ) ); }
#line 8154 "Parser/parser.cc"
    break;

  case 92:
#line 860 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Incr, (yyvsp[0].en) ) ); }
#line 8160 "Parser/parser.cc"
    break;

  case 93:
#line 862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_unary_ptr( OperKinds::Decr, (yyvsp[0].en) ) ); }
#line 8166 "Parser/parser.cc"
    break;

  case 94:
#line 864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 8172 "Parser/parser.cc"
    break;

  case 95:
#line 866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new SizeofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8178 "Parser/parser.cc"
    break;

  case 96:
#line 868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 8184 "Parser/parser.cc"
    break;

  case 97:
#line 870 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AlignofExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ); }
#line 8190 "Parser/parser.cc"
    break;

  case 98:
#line 872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_offsetOf( (yyvsp[-3].decl), build_varref( (yyvsp[-1].tok) ) ) ); }
#line 8196 "Parser/parser.cc"
    break;

  case 99:
#line 874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "typeid name is currently unimplemented." ); (yyval.en) = nullptr;
			// $$ = new ExpressionNode( build_offsetOf( $3, build_varref( $5 ) ) );
		}
#line 8205 "Parser/parser.cc"
    break;

  case 100:
#line 881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::PointTo; }
#line 8211 "Parser/parser.cc"
    break;

  case 101:
#line 882 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AddressOf; }
#line 8217 "Parser/parser.cc"
    break;

  case 102:
#line 884 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::And; }
#line 8223 "Parser/parser.cc"
    break;

  case 103:
#line 888 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::UnPlus; }
#line 8229 "Parser/parser.cc"
    break;

  case 104:
#line 889 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::UnMinus; }
#line 8235 "Parser/parser.cc"
    break;

  case 105:
#line 890 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::Neg; }
#line 8241 "Parser/parser.cc"
    break;

  case 106:
#line 891 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::BitNeg; }
#line 8247 "Parser/parser.cc"
    break;

  case 108:
#line 897 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cast( (yyvsp[-2].decl), (yyvsp[0].en) ) ); }
#line 8253 "Parser/parser.cc"
    break;

  case 109:
#line 899 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 8259 "Parser/parser.cc"
    break;

  case 110:
#line 901 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_keyword_cast( (yyvsp[-3].aggKey), (yyvsp[0].en) ) ); }
#line 8265 "Parser/parser.cc"
    break;

  case 111:
#line 903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( nullptr ) ) ); }
#line 8271 "Parser/parser.cc"
    break;

  case 112:
#line 905 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new VirtualCastExpr( maybeMoveBuild<Expression>( (yyvsp[0].en) ), maybeMoveBuildType( (yyvsp[-2].decl) ) ) ); }
#line 8277 "Parser/parser.cc"
    break;

  case 113:
#line 907 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Return cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8283 "Parser/parser.cc"
    break;

  case 114:
#line 909 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Coerce cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8289 "Parser/parser.cc"
    break;

  case 115:
#line 911 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Qualifier cast is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8295 "Parser/parser.cc"
    break;

  case 123:
#line 931 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Exp, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8301 "Parser/parser.cc"
    break;

  case 125:
#line 937 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mul, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8307 "Parser/parser.cc"
    break;

  case 126:
#line 939 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Div, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8313 "Parser/parser.cc"
    break;

  case 127:
#line 941 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Mod, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8319 "Parser/parser.cc"
    break;

  case 129:
#line 947 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Plus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8325 "Parser/parser.cc"
    break;

  case 130:
#line 949 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Minus, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8331 "Parser/parser.cc"
    break;

  case 132:
#line 955 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8337 "Parser/parser.cc"
    break;

  case 133:
#line 957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::RShift, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8343 "Parser/parser.cc"
    break;

  case 135:
#line 963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8349 "Parser/parser.cc"
    break;

  case 136:
#line 965 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8355 "Parser/parser.cc"
    break;

  case 137:
#line 967 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::LEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8361 "Parser/parser.cc"
    break;

  case 138:
#line 969 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::GEThan, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8367 "Parser/parser.cc"
    break;

  case 140:
#line 975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Eq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8373 "Parser/parser.cc"
    break;

  case 141:
#line 977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Neq, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8379 "Parser/parser.cc"
    break;

  case 143:
#line 983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitAnd, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8385 "Parser/parser.cc"
    break;

  case 145:
#line 989 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::Xor, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8391 "Parser/parser.cc"
    break;

  case 147:
#line 995 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_binary_val( OperKinds::BitOr, (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8397 "Parser/parser.cc"
    break;

  case 149:
#line 1001 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), true ) ); }
#line 8403 "Parser/parser.cc"
    break;

  case 151:
#line 1007 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_and_or( (yyvsp[-2].en), (yyvsp[0].en), false ) ); }
#line 8409 "Parser/parser.cc"
    break;

  case 153:
#line 1013 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ) ); }
#line 8415 "Parser/parser.cc"
    break;

  case 154:
#line 1016 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_cond( (yyvsp[-3].en), (yyvsp[-3].en), (yyvsp[0].en) ) ); }
#line 8421 "Parser/parser.cc"
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
#line 8433 "Parser/parser.cc"
    break;

  case 158:
#line 1035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer assignment is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8439 "Parser/parser.cc"
    break;

  case 159:
#line 1040 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8445 "Parser/parser.cc"
    break;

  case 163:
#line 1050 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                                { (yyval.op) = OperKinds::Assign; }
#line 8451 "Parser/parser.cc"
    break;

  case 164:
#line 1051 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AtAssn; }
#line 8457 "Parser/parser.cc"
    break;

  case 165:
#line 1055 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ExpAssn; }
#line 8463 "Parser/parser.cc"
    break;

  case 166:
#line 1056 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MulAssn; }
#line 8469 "Parser/parser.cc"
    break;

  case 167:
#line 1057 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::DivAssn; }
#line 8475 "Parser/parser.cc"
    break;

  case 168:
#line 1058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ModAssn; }
#line 8481 "Parser/parser.cc"
    break;

  case 169:
#line 1059 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::PlusAssn; }
#line 8487 "Parser/parser.cc"
    break;

  case 170:
#line 1060 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.op) = OperKinds::MinusAssn; }
#line 8493 "Parser/parser.cc"
    break;

  case 171:
#line 1061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::LSAssn; }
#line 8499 "Parser/parser.cc"
    break;

  case 172:
#line 1062 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::RSAssn; }
#line 8505 "Parser/parser.cc"
    break;

  case 173:
#line 1063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::AndAssn; }
#line 8511 "Parser/parser.cc"
    break;

  case 174:
#line 1064 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::ERAssn; }
#line 8517 "Parser/parser.cc"
    break;

  case 175:
#line 1065 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.op) = OperKinds::OrAssn; }
#line 8523 "Parser/parser.cc"
    break;

  case 176:
#line 1076 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)(new ExpressionNode( nullptr ) )->set_last( (yyvsp[-1].en) ) ) ); }
#line 8529 "Parser/parser.cc"
    break;

  case 177:
#line 1078 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_tuple( (ExpressionNode *)((yyvsp[-4].en)->set_last( (yyvsp[-1].en) ) ) )); }
#line 8535 "Parser/parser.cc"
    break;

  case 179:
#line 1084 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8541 "Parser/parser.cc"
    break;

  case 180:
#line 1086 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 8547 "Parser/parser.cc"
    break;

  case 181:
#line 1088 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Eliding tuple element with '@' is currently unimplemented." ); (yyval.en) = nullptr; }
#line 8553 "Parser/parser.cc"
    break;

  case 183:
#line 1094 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new CommaExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 8559 "Parser/parser.cc"
    break;

  case 184:
#line 1099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 8565 "Parser/parser.cc"
    break;

  case 196:
#line 1117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "enable/disable statement is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 8571 "Parser/parser.cc"
    break;

  case 198:
#line 1120 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_directive( (yyvsp[0].tok) ) ); }
#line 8577 "Parser/parser.cc"
    break;

  case 199:
#line 1126 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[0].sn)->add_label( (yyvsp[-3].tok), (yyvsp[-1].decl) ); }
#line 8583 "Parser/parser.cc"
    break;

  case 200:
#line 1128 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, ::toString( "Label \"", *(yyvsp[-3].tok).str, "\" must be associated with a statement, "
											   "where a declaration, case, or default is not a statement. "
											   "Move the label or terminate with a semi-colon." ) );
			(yyval.sn) = nullptr;
		}
#line 8594 "Parser/parser.cc"
    break;

  case 201:
#line 1138 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (StatementNode *)0 ) ); }
#line 8600 "Parser/parser.cc"
    break;

  case 202:
#line 1143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_compound( (yyvsp[-2].sn) ) ); }
#line 8606 "Parser/parser.cc"
    break;

  case 204:
#line 1149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 8612 "Parser/parser.cc"
    break;

  case 205:
#line 1154 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8618 "Parser/parser.cc"
    break;

  case 206:
#line 1156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8624 "Parser/parser.cc"
    break;

  case 207:
#line 1158 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8630 "Parser/parser.cc"
    break;

  case 208:
#line 1160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[0].decl) ); (yyval.sn) = new StatementNode( (yyvsp[0].decl) ); }
#line 8636 "Parser/parser.cc"
    break;

  case 211:
#line 1167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { assert( (yyvsp[-1].sn) ); (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) ); (yyval.sn) = (yyvsp[-1].sn); }
#line 8642 "Parser/parser.cc"
    break;

  case 212:
#line 1169 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Declarations only allowed at the start of the switch body, i.e., after the '{'." ); (yyval.sn) = nullptr; }
#line 8648 "Parser/parser.cc"
    break;

  case 213:
#line 1174 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_expr( (yyvsp[-1].en) ) ); }
#line 8654 "Parser/parser.cc"
    break;

  case 214:
#line 1176 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_mutex( nullptr, new StatementNode( build_expr( (yyvsp[-1].en) ) ) ) ); }
#line 8660 "Parser/parser.cc"
    break;

  case 215:
#line 1183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn); }
#line 8666 "Parser/parser.cc"
    break;

  case 216:
#line 1185 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( true, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8672 "Parser/parser.cc"
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
#line 8686 "Parser/parser.cc"
    break;

  case 218:
#line 1197 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Only declarations can appear before the list of case clauses." ); (yyval.sn) = nullptr; }
#line 8692 "Parser/parser.cc"
    break;

  case 219:
#line 1199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_switch( false, (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 8698 "Parser/parser.cc"
    break;

  case 220:
#line 1201 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			StatementNode *sw = new StatementNode( build_switch( false, (yyvsp[-7].en), (yyvsp[-2].sn) ) );
			(yyval.sn) = (yyvsp[-3].decl) ? new StatementNode( build_compound( (StatementNode *)((new StatementNode( (yyvsp[-3].decl) ))->set_last( sw )) ) ) : sw;
		}
#line 8707 "Parser/parser.cc"
    break;

  case 221:
#line 1206 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Only declarations can appear before the list of case clauses." ); (yyval.sn) = nullptr; }
#line 8713 "Parser/parser.cc"
    break;

  case 222:
#line 1212 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-2].ifctl), maybe_build_compound( (yyvsp[0].sn) ), nullptr ) ); }
#line 8719 "Parser/parser.cc"
    break;

  case 223:
#line 1214 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_if( (yyvsp[-4].ifctl), maybe_build_compound( (yyvsp[-2].sn) ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8725 "Parser/parser.cc"
    break;

  case 224:
#line 1219 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( nullptr, (yyvsp[0].en) ); }
#line 8731 "Parser/parser.cc"
    break;

  case 225:
#line 1221 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 8737 "Parser/parser.cc"
    break;

  case 226:
#line 1223 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[0].decl), nullptr ); }
#line 8743 "Parser/parser.cc"
    break;

  case 227:
#line 1225 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.ifctl) = new CondCtl( (yyvsp[-1].decl), (yyvsp[0].en) ); }
#line 8749 "Parser/parser.cc"
    break;

  case 228:
#line 1232 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.en) = (yyvsp[0].en); }
#line 8755 "Parser/parser.cc"
    break;

  case 229:
#line 1234 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 8761 "Parser/parser.cc"
    break;

  case 231:
#line 1239 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.sn) = new StatementNode( build_case( (yyvsp[0].en) ) ); }
#line 8767 "Parser/parser.cc"
    break;

  case 232:
#line 1241 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)((yyvsp[-2].sn)->set_last( new StatementNode( build_case( (yyvsp[0].en) ) ) ) ); }
#line 8773 "Parser/parser.cc"
    break;

  case 233:
#line 1246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing case list after case." ); (yyval.sn) = nullptr; }
#line 8779 "Parser/parser.cc"
    break;

  case 234:
#line 1247 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn); }
#line 8785 "Parser/parser.cc"
    break;

  case 235:
#line 1249 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing colon after case list." ); (yyval.sn) = nullptr; }
#line 8791 "Parser/parser.cc"
    break;

  case 236:
#line 1250 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.sn) = new StatementNode( build_default() ); }
#line 8797 "Parser/parser.cc"
    break;

  case 237:
#line 1253 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing colon after default." ); (yyval.sn) = nullptr; }
#line 8803 "Parser/parser.cc"
    break;

  case 239:
#line 1258 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.sn) = (StatementNode *)( (yyvsp[-1].sn)->set_last( (yyvsp[0].sn) )); }
#line 8809 "Parser/parser.cc"
    break;

  case 240:
#line 1262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( maybe_build_compound( (yyvsp[0].sn) ) ); }
#line 8815 "Parser/parser.cc"
    break;

  case 241:
#line 1267 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = nullptr; }
#line 8821 "Parser/parser.cc"
    break;

  case 243:
#line 1273 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ); }
#line 8827 "Parser/parser.cc"
    break;

  case 244:
#line 1275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)( (yyvsp[-2].sn)->set_last( (yyvsp[-1].sn)->append_last_case( new StatementNode( build_compound( (yyvsp[0].sn) ) ) ) ) ); }
#line 8833 "Parser/parser.cc"
    break;

  case 245:
#line 1280 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8839 "Parser/parser.cc"
    break;

  case 246:
#line 1282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_while( new CondCtl( nullptr, NEW_ONE ), maybe_build_compound( (yyvsp[-2].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse, "" );
		}
#line 8848 "Parser/parser.cc"
    break;

  case 247:
#line 1287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( (yyvsp[-2].ifctl), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8854 "Parser/parser.cc"
    break;

  case 248:
#line 1289 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_while( (yyvsp[-4].ifctl), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[0].sn) ) ); }
#line 8860 "Parser/parser.cc"
    break;

  case 249:
#line 1291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( NEW_ONE, maybe_build_compound( (yyvsp[-4].sn) ) ) ); }
#line 8866 "Parser/parser.cc"
    break;

  case 250:
#line 1293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_do_while( NEW_ONE, maybe_build_compound( (yyvsp[-5].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse, "" );
		}
#line 8875 "Parser/parser.cc"
    break;

  case 251:
#line 1298 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( (yyvsp[-2].en), maybe_build_compound( (yyvsp[-5].sn) ) ) ); }
#line 8881 "Parser/parser.cc"
    break;

  case 252:
#line 1300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_do_while( (yyvsp[-3].en), maybe_build_compound( (yyvsp[-6].sn) ), (yyvsp[0].sn) ) ); }
#line 8887 "Parser/parser.cc"
    break;

  case 253:
#line 1302 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( new ForCtrl( (ExpressionNode * )nullptr, (ExpressionNode * )nullptr, (ExpressionNode * )nullptr ), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8893 "Parser/parser.cc"
    break;

  case 254:
#line 1304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.sn) = new StatementNode( build_for( new ForCtrl( (ExpressionNode * )nullptr, (ExpressionNode * )nullptr, (ExpressionNode * )nullptr ), maybe_build_compound( (yyvsp[-2].sn) ) ) );
			SemanticWarning( yylloc, Warning::SuperfluousElse, "" );
		}
#line 8902 "Parser/parser.cc"
    break;

  case 255:
#line 1309 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( (yyvsp[-2].fctl), maybe_build_compound( (yyvsp[0].sn) ) ) ); }
#line 8908 "Parser/parser.cc"
    break;

  case 256:
#line 1311 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_for( (yyvsp[-4].fctl), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[0].sn) ) ); }
#line 8914 "Parser/parser.cc"
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
#line 8933 "Parser/parser.cc"
    break;

  case 259:
#line 1339 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (ExpressionNode * )nullptr, (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8939 "Parser/parser.cc"
    break;

  case 260:
#line 1341 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-4].en), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8945 "Parser/parser.cc"
    break;

  case 261:
#line 1343 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (yyvsp[-3].decl), (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8951 "Parser/parser.cc"
    break;

  case 262:
#line 1346 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (ExpressionNode *)nullptr, (yyvsp[0].en), nullptr ); }
#line 8957 "Parser/parser.cc"
    break;

  case 263:
#line 1348 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = new ForCtrl( (ExpressionNode *)nullptr, (yyvsp[-2].en), (yyvsp[0].en) ); }
#line 8963 "Parser/parser.cc"
    break;

  case 264:
#line 1351 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), NEW_ZERO, OperKinds::LThan, (yyvsp[0].en)->clone(), NEW_ONE ); }
#line 8969 "Parser/parser.cc"
    break;

  case 265:
#line 1353 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].compop), NEW_ZERO, (yyvsp[0].en)->clone() ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 8975 "Parser/parser.cc"
    break;

  case 266:
#line 1356 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-1].compop), (yyvsp[-2].en)->clone(), (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), (yyvsp[-2].en)->clone() ), NEW_ONE ); }
#line 8981 "Parser/parser.cc"
    break;

  case 267:
#line 1358 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[0].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[0].en)->clone(), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 8990 "Parser/parser.cc"
    break;

  case 268:
#line 1363 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
		}
#line 8999 "Parser/parser.cc"
    break;

  case 269:
#line 1368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), new string( DeclarationNode::anonymous.newName() ), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en)->clone(), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), (yyvsp[0].en) ); }
#line 9005 "Parser/parser.cc"
    break;

  case 270:
#line 1370 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-2].en), new string( DeclarationNode::anonymous.newName() ), (yyvsp[-2].en)->clone(), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 9014 "Parser/parser.cc"
    break;

  case 271:
#line 1375 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
			else { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
		}
#line 9023 "Parser/parser.cc"
    break;

  case 272:
#line 1380 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 9029 "Parser/parser.cc"
    break;

  case 273:
#line 1382 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 9035 "Parser/parser.cc"
    break;

  case 274:
#line 1384 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 9041 "Parser/parser.cc"
    break;

  case 275:
#line 1386 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 9047 "Parser/parser.cc"
    break;

  case 276:
#line 1388 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, MISSING_ANON_FIELD ); (yyval.fctl) = nullptr; }
#line 9053 "Parser/parser.cc"
    break;

  case 277:
#line 1391 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-2].en), NEW_ZERO, OperKinds::LThan, (yyvsp[0].en)->clone(), NEW_ONE ); }
#line 9059 "Parser/parser.cc"
    break;

  case 278:
#line 1393 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-3].en), UPDOWN( (yyvsp[-1].compop), NEW_ZERO, (yyvsp[0].en)->clone() ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9065 "Parser/parser.cc"
    break;

  case 279:
#line 1396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), UPDOWN( (yyvsp[-1].compop), (yyvsp[-2].en)->clone(), (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), (yyvsp[-2].en)->clone() ), NEW_ONE ); }
#line 9071 "Parser/parser.cc"
    break;

  case 280:
#line 1398 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[0].en), (yyvsp[-4].en), (yyvsp[0].en)->clone(), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 9080 "Parser/parser.cc"
    break;

  case 281:
#line 1403 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::GThan || (yyvsp[-1].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-4].en), (yyvsp[-2].en)->clone(), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 9090 "Parser/parser.cc"
    break;

  case 282:
#line 1409 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing low/high value for up/down-to range so index is uninitialized." ); (yyval.fctl) = nullptr; }
#line 9096 "Parser/parser.cc"
    break;

  case 283:
#line 1412 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en)->clone(), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), (yyvsp[0].en) ); }
#line 9102 "Parser/parser.cc"
    break;

  case 284:
#line 1414 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-6].en), (yyvsp[-2].en)->clone(), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 9111 "Parser/parser.cc"
    break;

  case 285:
#line 1419 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 9121 "Parser/parser.cc"
    break;

  case 286:
#line 1425 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en)->clone(), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), nullptr ); }
#line 9127 "Parser/parser.cc"
    break;

  case 287:
#line 1427 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-2].en), (yyvsp[-6].en), (yyvsp[-2].en)->clone(), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 9136 "Parser/parser.cc"
    break;

  case 288:
#line 1432 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-4].en), (yyvsp[-6].en), (yyvsp[-4].en)->clone(), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 9146 "Parser/parser.cc"
    break;

  case 289:
#line 1438 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing low/high value for up/down-to range so index is uninitialized." ); (yyval.fctl) = nullptr; }
#line 9152 "Parser/parser.cc"
    break;

  case 290:
#line 1441 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-1].decl), NEW_ZERO, OperKinds::LThan, (yyvsp[0].en), NEW_ONE ); }
#line 9158 "Parser/parser.cc"
    break;

  case 291:
#line 1443 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-2].decl), UPDOWN( (yyvsp[-1].compop), NEW_ZERO, (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), NEW_ZERO ), NEW_ONE ); }
#line 9164 "Parser/parser.cc"
    break;

  case 292:
#line 1446 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-3].decl), UPDOWN( (yyvsp[-1].compop), (yyvsp[-2].en)->clone(), (yyvsp[0].en) ), (yyvsp[-1].compop), UPDOWN( (yyvsp[-1].compop), (yyvsp[0].en)->clone(), (yyvsp[-2].en)->clone() ), NEW_ONE ); }
#line 9170 "Parser/parser.cc"
    break;

  case 293:
#line 1448 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LThan || (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-3].decl), (yyvsp[0].en), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 9179 "Parser/parser.cc"
    break;

  case 294:
#line 1453 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::GThan || (yyvsp[-1].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-1].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-3].decl), (yyvsp[-2].en), (yyvsp[-1].compop), nullptr, NEW_ONE );
		}
#line 9189 "Parser/parser.cc"
    break;

  case 295:
#line 1460 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), (yyvsp[0].en) ); }
#line 9195 "Parser/parser.cc"
    break;

  case 296:
#line 1462 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-2].en), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 9204 "Parser/parser.cc"
    break;

  case 297:
#line 1467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-4].en), (yyvsp[-3].compop), nullptr, (yyvsp[0].en) );
		}
#line 9214 "Parser/parser.cc"
    break;

  case 298:
#line 1473 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.fctl) = forCtrl( (yyvsp[-5].decl), UPDOWN( (yyvsp[-3].compop), (yyvsp[-4].en), (yyvsp[-2].en) ), (yyvsp[-3].compop), UPDOWN( (yyvsp[-3].compop), (yyvsp[-2].en)->clone(), (yyvsp[-4].en)->clone() ), nullptr ); }
#line 9220 "Parser/parser.cc"
    break;

  case 299:
#line 1475 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::LThan || (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, MISSING_LOW ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-2].en), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 9229 "Parser/parser.cc"
    break;

  case 300:
#line 1480 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-3].compop) == OperKinds::GThan || (yyvsp[-3].compop) == OperKinds::GEThan ) { SemanticError( yylloc, MISSING_HIGH ); (yyval.fctl) = nullptr; }
			else if ( (yyvsp[-3].compop) == OperKinds::LEThan ) { SemanticError( yylloc, "Equality with missing high value is meaningless. Use \"~\"." ); (yyval.fctl) = nullptr; }
			else (yyval.fctl) = forCtrl( (yyvsp[-5].decl), (yyvsp[-4].en), (yyvsp[-3].compop), nullptr, nullptr );
		}
#line 9239 "Parser/parser.cc"
    break;

  case 301:
#line 1486 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Missing low/high value for up/down-to range so index is uninitialized." ); (yyval.fctl) = nullptr; }
#line 9245 "Parser/parser.cc"
    break;

  case 302:
#line 1489 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.fctl) = nullptr;
			//$$ = forCtrl( new ExpressionNode( build_varref( $3 ) ), $1, nullptr, OperKinds::Range, nullptr, nullptr );
		}
#line 9254 "Parser/parser.cc"
    break;

  case 303:
#line 1494 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-1].compop) == OperKinds::LEThan || (yyvsp[-1].compop) == OperKinds::GEThan ) { SemanticError( yylloc, "All enumation ranges are equal (all values). Remove \"=~\"." ); (yyval.fctl) = nullptr; }
			SemanticError( yylloc, "Type iterator is currently unimplemented." ); (yyval.fctl) = nullptr;
		}
#line 9263 "Parser/parser.cc"
    break;

  case 304:
#line 1502 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GThan; }
#line 9269 "Parser/parser.cc"
    break;

  case 305:
#line 1504 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 9275 "Parser/parser.cc"
    break;

  case 306:
#line 1506 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GEThan; }
#line 9281 "Parser/parser.cc"
    break;

  case 307:
#line 1511 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LThan; }
#line 9287 "Parser/parser.cc"
    break;

  case 308:
#line 1513 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GThan; }
#line 9293 "Parser/parser.cc"
    break;

  case 310:
#line 1519 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::LEThan; }
#line 9299 "Parser/parser.cc"
    break;

  case 311:
#line 1521 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.compop) = OperKinds::GEThan; }
#line 9305 "Parser/parser.cc"
    break;

  case 312:
#line 1526 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Goto ) ); }
#line 9311 "Parser/parser.cc"
    break;

  case 313:
#line 1530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_computedgoto( (yyvsp[-1].en) ) ); }
#line 9317 "Parser/parser.cc"
    break;

  case 314:
#line 1533 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThrough ) ); }
#line 9323 "Parser/parser.cc"
    break;

  case 315:
#line 1535 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::FallThrough ) ); }
#line 9329 "Parser/parser.cc"
    break;

  case 316:
#line 1537 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::FallThroughDefault ) ); }
#line 9335 "Parser/parser.cc"
    break;

  case 317:
#line 1540 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Continue ) ); }
#line 9341 "Parser/parser.cc"
    break;

  case 318:
#line 1544 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Continue ) ); }
#line 9347 "Parser/parser.cc"
    break;

  case 319:
#line 1547 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( BranchStmt::Break ) ); }
#line 9353 "Parser/parser.cc"
    break;

  case 320:
#line 1551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_branch( (yyvsp[-1].tok), BranchStmt::Break ) ); }
#line 9359 "Parser/parser.cc"
    break;

  case 321:
#line 1553 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_return( (yyvsp[-1].en) ) ); }
#line 9365 "Parser/parser.cc"
    break;

  case 322:
#line 1555 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Initializer return is currently unimplemented." ); (yyval.sn) = nullptr; }
#line 9371 "Parser/parser.cc"
    break;

  case 323:
#line 1557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr ) ); }
#line 9377 "Parser/parser.cc"
    break;

  case 324:
#line 1559 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn) ) ); }
#line 9383 "Parser/parser.cc"
    break;

  case 325:
#line 1561 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Coroutine ) ); }
#line 9389 "Parser/parser.cc"
    break;

  case 326:
#line 1563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Coroutine ) ); }
#line 9395 "Parser/parser.cc"
    break;

  case 327:
#line 1565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( nullptr, SuspendStmt::Generator ) ); }
#line 9401 "Parser/parser.cc"
    break;

  case 328:
#line 1567 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_suspend( (yyvsp[0].sn), SuspendStmt::Generator ) ); }
#line 9407 "Parser/parser.cc"
    break;

  case 329:
#line 1569 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_throw( (yyvsp[-1].en) ) ); }
#line 9413 "Parser/parser.cc"
    break;

  case 330:
#line 1571 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume( (yyvsp[-1].en) ) ); }
#line 9419 "Parser/parser.cc"
    break;

  case 331:
#line 1573 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_resume_at( (yyvsp[-3].en), (yyvsp[-1].en) ) ); }
#line 9425 "Parser/parser.cc"
    break;

  case 334:
#line 1583 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_with( (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 9431 "Parser/parser.cc"
    break;

  case 335:
#line 1589 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_mutex( (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 9437 "Parser/parser.cc"
    break;

  case 336:
#line 1593 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 9443 "Parser/parser.cc"
    break;

  case 337:
#line 1598 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9449 "Parser/parser.cc"
    break;

  case 339:
#line 1604 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 9455 "Parser/parser.cc"
    break;

  case 340:
#line 1608 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-3].en)->set_last( (yyvsp[-1].en) )); }
#line 9461 "Parser/parser.cc"
    break;

  case 342:
#line 1615 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "List of mutex member is currently unimplemented." ); (yyval.en) = nullptr; }
#line 9467 "Parser/parser.cc"
    break;

  case 343:
#line 1619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[-1].en); }
#line 9473 "Parser/parser.cc"
    break;

  case 344:
#line 1624 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 9479 "Parser/parser.cc"
    break;

  case 345:
#line 1626 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor( (yyvsp[-3].en), maybe_build_compound( (yyvsp[-2].sn) ), (yyvsp[-4].en), (yyvsp[0].wfs) ); }
#line 9485 "Parser/parser.cc"
    break;

  case 346:
#line 1628 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-1].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 9491 "Parser/parser.cc"
    break;

  case 347:
#line 1630 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( nullptr, maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 9497 "Parser/parser.cc"
    break;

  case 348:
#line 1633 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "else clause must be conditional after timeout or timeout never triggered." ); (yyval.wfs) = nullptr; }
#line 9503 "Parser/parser.cc"
    break;

  case 349:
#line 1635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.wfs) = build_waitfor_timeout( (yyvsp[-5].en), maybe_build_compound( (yyvsp[-4].sn) ), (yyvsp[-6].en), maybe_build_compound( (yyvsp[0].sn) ), (yyvsp[-2].en) ); }
#line 9509 "Parser/parser.cc"
    break;

  case 350:
#line 1640 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-1].en), (yyvsp[0].sn), (yyvsp[-2].en) ) ); }
#line 9515 "Parser/parser.cc"
    break;

  case 351:
#line 1642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_waitfor( (yyvsp[-3].en), (yyvsp[-2].sn), (yyvsp[-4].en), (yyvsp[0].wfs) ) ); }
#line 9521 "Parser/parser.cc"
    break;

  case 352:
#line 1647 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), (yyvsp[0].sn), 0 ) ); }
#line 9527 "Parser/parser.cc"
    break;

  case 353:
#line 1649 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-1].sn), 0, (yyvsp[0].sn) ) ); }
#line 9533 "Parser/parser.cc"
    break;

  case 354:
#line 1651 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_try( (yyvsp[-2].sn), (yyvsp[-1].sn), (yyvsp[0].sn) ) ); }
#line 9539 "Parser/parser.cc"
    break;

  case 355:
#line 1656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ); }
#line 9545 "Parser/parser.cc"
    break;

  case 356:
#line 1658 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = (StatementNode *)(yyvsp[-8].sn)->set_last( new StatementNode( build_catch( (yyvsp[-7].catch_kind), (yyvsp[-4].decl), (yyvsp[-2].en), (yyvsp[0].sn) ) ) ); }
#line 9551 "Parser/parser.cc"
    break;

  case 357:
#line 1663 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9557 "Parser/parser.cc"
    break;

  case 358:
#line 1664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                { (yyval.en) = (yyvsp[0].en); }
#line 9563 "Parser/parser.cc"
    break;

  case 359:
#line 1668 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 9569 "Parser/parser.cc"
    break;

  case 360:
#line 1669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Terminate; }
#line 9575 "Parser/parser.cc"
    break;

  case 361:
#line 1670 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                { (yyval.catch_kind) = CatchStmt::Resume; }
#line 9581 "Parser/parser.cc"
    break;

  case 362:
#line 1671 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.catch_kind) = CatchStmt::Resume; }
#line 9587 "Parser/parser.cc"
    break;

  case 363:
#line 1675 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.sn) = new StatementNode( build_finally( (yyvsp[0].sn) ) ); }
#line 9593 "Parser/parser.cc"
    break;

  case 365:
#line 1682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 9599 "Parser/parser.cc"
    break;

  case 366:
#line 1684 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 9605 "Parser/parser.cc"
    break;

  case 367:
#line 1686 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 9611 "Parser/parser.cc"
    break;

  case 372:
#line 1701 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-4].flag), (yyvsp[-2].constant), 0 ) ); }
#line 9617 "Parser/parser.cc"
    break;

  case 373:
#line 1703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-6].flag), (yyvsp[-4].constant), (yyvsp[-2].en) ) ); }
#line 9623 "Parser/parser.cc"
    break;

  case 374:
#line 1705 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-8].flag), (yyvsp[-6].constant), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 9629 "Parser/parser.cc"
    break;

  case 375:
#line 1707 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-10].flag), (yyvsp[-8].constant), (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].en) ) ); }
#line 9635 "Parser/parser.cc"
    break;

  case 376:
#line 1709 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.sn) = new StatementNode( build_asm( (yyvsp[-12].flag), (yyvsp[-9].constant), 0, (yyvsp[-6].en), (yyvsp[-4].en), (yyvsp[-2].label) ) ); }
#line 9641 "Parser/parser.cc"
    break;

  case 377:
#line 1714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = false; }
#line 9647 "Parser/parser.cc"
    break;

  case 378:
#line 1716 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.flag) = true; }
#line 9653 "Parser/parser.cc"
    break;

  case 379:
#line 1721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9659 "Parser/parser.cc"
    break;

  case 382:
#line 1728 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 9665 "Parser/parser.cc"
    break;

  case 383:
#line 1733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( nullptr, (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 9671 "Parser/parser.cc"
    break;

  case 384:
#line 1735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new AsmExpr( (yyvsp[-5].tok), (yyvsp[-3].constant), maybeMoveBuild<Expression>( (yyvsp[-1].en) ) ) ); }
#line 9677 "Parser/parser.cc"
    break;

  case 385:
#line 1740 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 9683 "Parser/parser.cc"
    break;

  case 386:
#line 1742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( (yyvsp[0].constant) ); }
#line 9689 "Parser/parser.cc"
    break;

  case 387:
#line 1744 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( (yyvsp[0].constant) ) )); }
#line 9695 "Parser/parser.cc"
    break;

  case 388:
#line 1749 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = new LabelNode(); (yyval.label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 9704 "Parser/parser.cc"
    break;

  case 389:
#line 1754 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.label) = (yyvsp[-2].label); (yyvsp[-2].label)->labels.push_back( *(yyvsp[0].tok) );
			delete (yyvsp[0].tok);									// allocated by lexer
		}
#line 9713 "Parser/parser.cc"
    break;

  case 390:
#line 1764 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9719 "Parser/parser.cc"
    break;

  case 393:
#line 1771 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 9725 "Parser/parser.cc"
    break;

  case 394:
#line 1776 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9731 "Parser/parser.cc"
    break;

  case 396:
#line 1782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 9737 "Parser/parser.cc"
    break;

  case 397:
#line 1784 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-2].decl) ); }
#line 9743 "Parser/parser.cc"
    break;

  case 404:
#line 1804 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "C_DECLARATION1 %p %s\n", $$, $$->name ? $$->name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 9754 "Parser/parser.cc"
    break;

  case 407:
#line 1816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-4].en), (yyvsp[-2].constant) ); }
#line 9760 "Parser/parser.cc"
    break;

  case 408:
#line 1818 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStaticAssert( (yyvsp[-2].en), build_constantStr( *new string( "\"\"" ) ) ); }
#line 9766 "Parser/parser.cc"
    break;

  case 412:
#line 1836 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "otype declaration is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 9772 "Parser/parser.cc"
    break;

  case 414:
#line 1842 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].in) ); }
#line 9778 "Parser/parser.cc"
    break;

  case 415:
#line 1846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 9784 "Parser/parser.cc"
    break;

  case 416:
#line 1848 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-5].decl)->cloneType( (yyvsp[-1].tok) )->addInitializer( (yyvsp[0].in) ) ); }
#line 9790 "Parser/parser.cc"
    break;

  case 417:
#line 1855 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 9796 "Parser/parser.cc"
    break;

  case 418:
#line 1857 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 9802 "Parser/parser.cc"
    break;

  case 419:
#line 1859 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addName( (yyvsp[-1].tok) )->addAsmName( (yyvsp[0].decl) ); }
#line 9808 "Parser/parser.cc"
    break;

  case 421:
#line 1865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9814 "Parser/parser.cc"
    break;

  case 422:
#line 1867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 9820 "Parser/parser.cc"
    break;

  case 423:
#line 1869 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[-1].decl) ); }
#line 9826 "Parser/parser.cc"
    break;

  case 424:
#line 1871 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Append the return type at the start (left-hand-side) to each identifier in the list.
			DeclarationNode * ret = new DeclarationNode;
			ret->type = maybeClone( (yyvsp[-7].decl)->type->base );
			(yyval.decl) = (yyvsp[-7].decl)->appendList( DeclarationNode::newFunction( (yyvsp[-5].tok), ret, (yyvsp[-2].decl), nullptr ) );
		}
#line 9837 "Parser/parser.cc"
    break;

  case 425:
#line 1904 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), 0 )->addQualifiers( (yyvsp[0].decl) ); }
#line 9843 "Parser/parser.cc"
    break;

  case 426:
#line 1906 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( (yyvsp[-6].tok), (yyvsp[-7].decl), (yyvsp[-3].decl), 0 )->addQualifiers( (yyvsp[0].decl) ); }
#line 9849 "Parser/parser.cc"
    break;

  case 427:
#line 1911 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 9855 "Parser/parser.cc"
    break;

  case 428:
#line 1914 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-6].decl)->appendList( (yyvsp[-2].decl) ) ); }
#line 9861 "Parser/parser.cc"
    break;

  case 429:
#line 1919 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "1" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 9870 "Parser/parser.cc"
    break;

  case 430:
#line 1924 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "2" );
			(yyval.decl) = (yyvsp[0].decl)->addTypedef();
		}
#line 9879 "Parser/parser.cc"
    break;

  case 431:
#line 1929 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "3" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) );
		}
#line 9888 "Parser/parser.cc"
    break;

  case 432:
#line 1940 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "4" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addTypedef();
		}
#line 9897 "Parser/parser.cc"
    break;

  case 433:
#line 1945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "5" );
			(yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) )->addTypedef() );
		}
#line 9906 "Parser/parser.cc"
    break;

  case 434:
#line 1950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "6" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[-3].decl) )->addTypedef();
		}
#line 9915 "Parser/parser.cc"
    break;

  case 435:
#line 1955 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "7" );
			(yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-2].decl) )->addTypedef();
		}
#line 9924 "Parser/parser.cc"
    break;

  case 436:
#line 1960 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, TYPEDEFname, "8" );
			(yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-3].decl) )->addTypedef()->addType( (yyvsp[-3].decl) );
		}
#line 9933 "Parser/parser.cc"
    break;

  case 437:
#line 1969 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 9941 "Parser/parser.cc"
    break;

  case 438:
#line 1973 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			SemanticError( yylloc, "Typedef expression is deprecated, use typeof(...) instead." ); (yyval.decl) = nullptr;
		}
#line 9949 "Parser/parser.cc"
    break;

  case 439:
#line 1980 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = distAttr( (yyvsp[-1].decl), (yyvsp[0].decl) ); }
#line 9955 "Parser/parser.cc"
    break;

  case 443:
#line 1990 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ); }
#line 9961 "Parser/parser.cc"
    break;

  case 444:
#line 1992 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->appendList( (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addAsmName( (yyvsp[-1].decl) )->addInitializer( (yyvsp[0].in) ) ); }
#line 9967 "Parser/parser.cc"
    break;

  case 452:
#line 2015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_type_specifier2 %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 9978 "Parser/parser.cc"
    break;

  case 457:
#line 2037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 9984 "Parser/parser.cc"
    break;

  case 460:
#line 2049 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 9990 "Parser/parser.cc"
    break;

  case 463:
#line 2059 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Const ); }
#line 9996 "Parser/parser.cc"
    break;

  case 464:
#line 2061 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Restrict ); }
#line 10002 "Parser/parser.cc"
    break;

  case 465:
#line 2063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Volatile ); }
#line 10008 "Parser/parser.cc"
    break;

  case 466:
#line 2065 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeQualifier( Type::Atomic ); }
#line 10014 "Parser/parser.cc"
    break;

  case 468:
#line 2071 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newForall( (yyvsp[-1].decl) ); }
#line 10020 "Parser/parser.cc"
    break;

  case 470:
#line 2077 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10026 "Parser/parser.cc"
    break;

  case 471:
#line 2079 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10032 "Parser/parser.cc"
    break;

  case 473:
#line 2090 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10038 "Parser/parser.cc"
    break;

  case 474:
#line 2095 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Extern ); }
#line 10044 "Parser/parser.cc"
    break;

  case 475:
#line 2097 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Static ); }
#line 10050 "Parser/parser.cc"
    break;

  case 476:
#line 2099 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Auto ); }
#line 10056 "Parser/parser.cc"
    break;

  case 477:
#line 2101 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::Register ); }
#line 10062 "Parser/parser.cc"
    break;

  case 478:
#line 2103 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::ThreadlocalGcc ); }
#line 10068 "Parser/parser.cc"
    break;

  case 479:
#line 2105 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newStorageClass( Type::ThreadlocalC11 ); }
#line 10074 "Parser/parser.cc"
    break;

  case 480:
#line 2108 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Inline ); }
#line 10080 "Parser/parser.cc"
    break;

  case 481:
#line 2110 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Fortran ); }
#line 10086 "Parser/parser.cc"
    break;

  case 482:
#line 2112 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFuncSpecifier( Type::Noreturn ); }
#line 10092 "Parser/parser.cc"
    break;

  case 483:
#line 2117 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 10098 "Parser/parser.cc"
    break;

  case 484:
#line 2119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Bool ); }
#line 10104 "Parser/parser.cc"
    break;

  case 485:
#line 2121 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Char ); }
#line 10110 "Parser/parser.cc"
    break;

  case 486:
#line 2123 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int ); }
#line 10116 "Parser/parser.cc"
    break;

  case 487:
#line 2125 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 ); }
#line 10122 "Parser/parser.cc"
    break;

  case 488:
#line 2127 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Int128 )->addType( DeclarationNode::newSignedNess( DeclarationNode::Unsigned ) ); }
#line 10128 "Parser/parser.cc"
    break;

  case 489:
#line 2129 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Float ); }
#line 10134 "Parser/parser.cc"
    break;

  case 490:
#line 2131 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Double ); }
#line 10140 "Parser/parser.cc"
    break;

  case 491:
#line 2133 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat80 ); }
#line 10146 "Parser/parser.cc"
    break;

  case 492:
#line 2135 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uuFloat128 ); }
#line 10152 "Parser/parser.cc"
    break;

  case 493:
#line 2137 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat16 ); }
#line 10158 "Parser/parser.cc"
    break;

  case 494:
#line 2139 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32 ); }
#line 10164 "Parser/parser.cc"
    break;

  case 495:
#line 2141 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat32x ); }
#line 10170 "Parser/parser.cc"
    break;

  case 496:
#line 2143 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64 ); }
#line 10176 "Parser/parser.cc"
    break;

  case 497:
#line 2145 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat64x ); }
#line 10182 "Parser/parser.cc"
    break;

  case 498:
#line 2147 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::uFloat128 ); }
#line 10188 "Parser/parser.cc"
    break;

  case 499:
#line 2149 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal32 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10194 "Parser/parser.cc"
    break;

  case 500:
#line 2151 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal64 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10200 "Parser/parser.cc"
    break;

  case 501:
#line 2153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "_Decimal128 is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10206 "Parser/parser.cc"
    break;

  case 502:
#line 2155 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Complex ); }
#line 10212 "Parser/parser.cc"
    break;

  case 503:
#line 2157 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newComplexType( DeclarationNode::Imaginary ); }
#line 10218 "Parser/parser.cc"
    break;

  case 504:
#line 2159 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Signed ); }
#line 10224 "Parser/parser.cc"
    break;

  case 505:
#line 2161 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newSignedNess( DeclarationNode::Unsigned ); }
#line 10230 "Parser/parser.cc"
    break;

  case 506:
#line 2163 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Short ); }
#line 10236 "Parser/parser.cc"
    break;

  case 507:
#line 2165 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newLength( DeclarationNode::Long ); }
#line 10242 "Parser/parser.cc"
    break;

  case 508:
#line 2167 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Valist ); }
#line 10248 "Parser/parser.cc"
    break;

  case 509:
#line 2169 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::AutoType ); }
#line 10254 "Parser/parser.cc"
    break;

  case 511:
#line 2175 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10260 "Parser/parser.cc"
    break;

  case 513:
#line 2181 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVtableType( (yyvsp[-2].decl) ); }
#line 10266 "Parser/parser.cc"
    break;

  case 514:
#line 2187 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10272 "Parser/parser.cc"
    break;

  case 515:
#line 2189 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "vtable default is currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10278 "Parser/parser.cc"
    break;

  case 517:
#line 2196 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10284 "Parser/parser.cc"
    break;

  case 518:
#line 2198 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10290 "Parser/parser.cc"
    break;

  case 519:
#line 2200 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10296 "Parser/parser.cc"
    break;

  case 520:
#line 2202 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) )->addType( (yyvsp[-2].decl) ); }
#line 10302 "Parser/parser.cc"
    break;

  case 522:
#line 2209 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10308 "Parser/parser.cc"
    break;

  case 524:
#line 2215 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10314 "Parser/parser.cc"
    break;

  case 525:
#line 2217 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10320 "Parser/parser.cc"
    break;

  case 526:
#line 2219 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[0].decl) ); }
#line 10326 "Parser/parser.cc"
    break;

  case 527:
#line 2224 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10332 "Parser/parser.cc"
    break;

  case 528:
#line 2226 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en) ); }
#line 10338 "Parser/parser.cc"
    break;

  case 529:
#line 2228 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[-1].decl) ) ) ), true ); }
#line 10344 "Parser/parser.cc"
    break;

  case 530:
#line 2230 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeof( (yyvsp[-1].en), true ); }
#line 10350 "Parser/parser.cc"
    break;

  case 531:
#line 2232 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::Zero ); }
#line 10356 "Parser/parser.cc"
    break;

  case 532:
#line 2234 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBuiltinType( DeclarationNode::One ); }
#line 10362 "Parser/parser.cc"
    break;

  case 533:
#line 2239 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_declaration_specifier %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10373 "Parser/parser.cc"
    break;

  case 534:
#line 2246 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10379 "Parser/parser.cc"
    break;

  case 535:
#line 2248 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10385 "Parser/parser.cc"
    break;

  case 536:
#line 2250 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10391 "Parser/parser.cc"
    break;

  case 537:
#line 2255 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "sue_type_specifier %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10402 "Parser/parser.cc"
    break;

  case 538:
#line 2262 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; }
#line 10408 "Parser/parser.cc"
    break;

  case 539:
#line 2264 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 10414 "Parser/parser.cc"
    break;

  case 540:
#line 2266 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type != nullptr && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
			(yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) );
		}
#line 10423 "Parser/parser.cc"
    break;

  case 542:
#line 2275 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10429 "Parser/parser.cc"
    break;

  case 543:
#line 2277 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10435 "Parser/parser.cc"
    break;

  case 544:
#line 2279 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10441 "Parser/parser.cc"
    break;

  case 546:
#line 2285 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10447 "Parser/parser.cc"
    break;

  case 547:
#line 2287 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10453 "Parser/parser.cc"
    break;

  case 549:
#line 2293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10459 "Parser/parser.cc"
    break;

  case 550:
#line 2295 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10465 "Parser/parser.cc"
    break;

  case 551:
#line 2297 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-1].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 10471 "Parser/parser.cc"
    break;

  case 553:
#line 2303 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 10477 "Parser/parser.cc"
    break;

  case 554:
#line 2305 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 10483 "Parser/parser.cc"
    break;

  case 555:
#line 2310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypedef( (yyvsp[0].tok) ); }
#line 10489 "Parser/parser.cc"
    break;

  case 556:
#line 2312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 10495 "Parser/parser.cc"
    break;

  case 557:
#line 2314 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), DeclarationNode::newFromTypedef( (yyvsp[0].tok) ) ); }
#line 10501 "Parser/parser.cc"
    break;

  case 559:
#line 2317 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( DeclarationNode::newFromGlobalScope(), (yyvsp[0].decl) ); }
#line 10507 "Parser/parser.cc"
    break;

  case 560:
#line 2319 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newQualifiedType( (yyvsp[-2].decl), (yyvsp[0].decl) ); }
#line 10513 "Parser/parser.cc"
    break;

  case 561:
#line 2324 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[0].tok), nullptr ); }
#line 10519 "Parser/parser.cc"
    break;

  case 562:
#line 2326 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-2].tok), nullptr ); }
#line 10525 "Parser/parser.cc"
    break;

  case 563:
#line 2328 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFromTypeGen( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 10531 "Parser/parser.cc"
    break;

  case 564:
#line 2333 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "elaborated_type %p %s\n", $$, $$->type->aggregate.name ? $$->type->aggregate.name->c_str() : "(nil)" );
		  	// for ( Attribute * attr: reverseIterate( $$->attributes ) ) {
			//   printf( "\tattr %s\n", attr->name.c_str() );
			// } // for
		}
#line 10542 "Parser/parser.cc"
    break;

  case 568:
#line 2349 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { forall = false; }
#line 10548 "Parser/parser.cc"
    break;

  case 569:
#line 2351 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-6].aggKey), nullptr, (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-5].decl) ); }
#line 10554 "Parser/parser.cc"
    break;

  case 570:
#line 2353 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 10563 "Parser/parser.cc"
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
#line 10580 "Parser/parser.cc"
    break;

  case 572:
#line 2371 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 10589 "Parser/parser.cc"
    break;

  case 573:
#line 2376 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "AGG3\n" );
			DeclarationNode::newFromTypedef( (yyvsp[-5].tok) );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 10599 "Parser/parser.cc"
    break;

  case 574:
#line 2382 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname ); // create typedef
			forall = false;								// reset
		}
#line 10608 "Parser/parser.cc"
    break;

  case 575:
#line 2387 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// printf( "AGG4\n" );
			DeclarationNode::newFromTypeGen( (yyvsp[-5].tok), nullptr );
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-7].aggKey), (yyvsp[-5].tok), (yyvsp[0].en), (yyvsp[-2].decl), true )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 10618 "Parser/parser.cc"
    break;

  case 577:
#line 2397 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 10624 "Parser/parser.cc"
    break;

  case 578:
#line 2399 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-1].en); }
#line 10630 "Parser/parser.cc"
    break;

  case 579:
#line 2404 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.makeTypedef( *(yyvsp[0].tok), forall || typedefTable.getEnclForall() ? TYPEGENname : TYPEDEFname );
			forall = false;								// reset
			(yyval.decl) = DeclarationNode::newAggregate( (yyvsp[-2].aggKey), (yyvsp[0].tok), nullptr, nullptr, false )->addQualifiers( (yyvsp[-1].decl) );
		}
#line 10640 "Parser/parser.cc"
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
#line 10655 "Parser/parser.cc"
    break;

  case 583:
#line 2429 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Struct; }
#line 10661 "Parser/parser.cc"
    break;

  case 584:
#line 2431 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Union; }
#line 10667 "Parser/parser.cc"
    break;

  case 585:
#line 2433 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Exception; }
#line 10673 "Parser/parser.cc"
    break;

  case 586:
#line 2439 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 10679 "Parser/parser.cc"
    break;

  case 587:
#line 2441 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Monitor; }
#line 10685 "Parser/parser.cc"
    break;

  case 588:
#line 2443 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Generator; }
#line 10691 "Parser/parser.cc"
    break;

  case 589:
#line 2445 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor generator is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 10697 "Parser/parser.cc"
    break;

  case 590:
#line 2447 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Coroutine; }
#line 10703 "Parser/parser.cc"
    break;

  case 591:
#line 2449 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor coroutine is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 10709 "Parser/parser.cc"
    break;

  case 592:
#line 2451 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.aggKey) = AggregateDecl::Thread; }
#line 10715 "Parser/parser.cc"
    break;

  case 593:
#line 2453 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "monitor thread is currently unimplemented." ); (yyval.aggKey) = AggregateDecl::NoAggregate; }
#line 10721 "Parser/parser.cc"
    break;

  case 594:
#line 2458 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10727 "Parser/parser.cc"
    break;

  case 595:
#line 2460 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl) ? (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl); }
#line 10733 "Parser/parser.cc"
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
#line 10746 "Parser/parser.cc"
    break;

  case 597:
#line 2474 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = fieldDecl( (yyvsp[-2].decl), (yyvsp[-1].decl) ); distExt( (yyval.decl) ); }
#line 10752 "Parser/parser.cc"
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
#line 10765 "Parser/parser.cc"
    break;

  case 599:
#line 2485 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "INLINE aggregate control currently unimplemented." ); (yyval.decl) = nullptr; }
#line 10771 "Parser/parser.cc"
    break;

  case 602:
#line 2489 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { distExt( (yyvsp[-1].decl) ); (yyval.decl) = (yyvsp[-1].decl); }
#line 10777 "Parser/parser.cc"
    break;

  case 603:
#line 2491 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 10783 "Parser/parser.cc"
    break;

  case 606:
#line 2498 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10789 "Parser/parser.cc"
    break;

  case 608:
#line 2501 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 10795 "Parser/parser.cc"
    break;

  case 609:
#line 2506 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBitfield( (yyvsp[0].en) ); }
#line 10801 "Parser/parser.cc"
    break;

  case 610:
#line 2509 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 10807 "Parser/parser.cc"
    break;

  case 611:
#line 2512 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addBitfield( (yyvsp[0].en) ); }
#line 10813 "Parser/parser.cc"
    break;

  case 612:
#line 2517 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 10819 "Parser/parser.cc"
    break;

  case 614:
#line 2520 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ) ); }
#line 10825 "Parser/parser.cc"
    break;

  case 616:
#line 2531 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addName( (yyvsp[0].tok) ); }
#line 10831 "Parser/parser.cc"
    break;

  case 617:
#line 2533 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[-2].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 10837 "Parser/parser.cc"
    break;

  case 619:
#line 2540 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[-1].decl)->cloneType( 0 ) ); }
#line 10843 "Parser/parser.cc"
    break;

  case 620:
#line 2545 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 10849 "Parser/parser.cc"
    break;

  case 622:
#line 2551 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 10855 "Parser/parser.cc"
    break;

  case 623:
#line 2556 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, false )->addQualifiers( (yyvsp[-4].decl) ); }
#line 10861 "Parser/parser.cc"
    break;

  case 624:
#line 2558 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); }
#line 10867 "Parser/parser.cc"
    break;

  case 625:
#line 2560 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
          { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].hide) )->addQualifiers( (yyvsp[-7].decl) ); }
#line 10873 "Parser/parser.cc"
    break;

  case 626:
#line 2563 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnum( (yyvsp[-5].decl)->name, (yyvsp[-2].decl), true, false, nullptr, (yyvsp[-4].hide) )->addQualifiers( (yyvsp[-6].decl) ); }
#line 10879 "Parser/parser.cc"
    break;

  case 627:
#line 2565 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-6].decl)->storageClasses.val != 0 || (yyvsp[-6].decl)->type->qualifiers.val != 0 )
			{ SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }

			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true, (yyvsp[-6].decl) )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 10890 "Parser/parser.cc"
    break;

  case 628:
#line 2572 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( nullptr, (yyvsp[-2].decl), true, true )->addQualifiers( (yyvsp[-4].decl) );
		}
#line 10898 "Parser/parser.cc"
    break;

  case 629:
#line 2576 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[-4].decl)->storageClasses.val != 0 || (yyvsp[-4].decl)->type->qualifiers.val != 0 ) { SemanticError( yylloc, "storage-class and CV qualifiers are not meaningful for enumeration constants, which are const." ); }
			typedefTable.makeTypedef( *(yyvsp[-1].tok) );
		}
#line 10907 "Parser/parser.cc"
    break;

  case 630:
#line 2581 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-7].tok), (yyvsp[-2].decl), true, true, (yyvsp[-10].decl), (yyvsp[-4].hide) )->addQualifiers( (yyvsp[-8].decl) )->addQualifiers( (yyvsp[-6].decl) );
		}
#line 10915 "Parser/parser.cc"
    break;

  case 631:
#line 2586 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].tok), (yyvsp[-2].decl), true, true, nullptr, (yyvsp[-4].hide) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 10923 "Parser/parser.cc"
    break;

  case 632:
#line 2591 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].decl)->name, (yyvsp[-2].decl), true, true, (yyvsp[-9].decl), (yyvsp[-4].hide) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 10931 "Parser/parser.cc"
    break;

  case 633:
#line 2596 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			(yyval.decl) = DeclarationNode::newEnum( (yyvsp[-6].decl)->name, (yyvsp[-2].decl), true, true, nullptr, (yyvsp[-4].hide) )->addQualifiers( (yyvsp[-7].decl) )->addQualifiers( (yyvsp[-5].decl) );
		}
#line 10939 "Parser/parser.cc"
    break;

  case 635:
#line 2604 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.hide) = EnumHiding::Visible; }
#line 10945 "Parser/parser.cc"
    break;

  case 636:
#line 2606 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.hide) = EnumHiding::Hide; }
#line 10951 "Parser/parser.cc"
    break;

  case 637:
#line 2611 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].tok) ); (yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].tok), 0, false, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 10957 "Parser/parser.cc"
    break;

  case 638:
#line 2613 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.makeTypedef( *(yyvsp[0].decl)->type->symbolic.name );	(yyval.decl) = DeclarationNode::newEnum( (yyvsp[0].decl)->type->symbolic.name, 0, false, false )->addQualifiers( (yyvsp[-1].decl) ); }
#line 10963 "Parser/parser.cc"
    break;

  case 639:
#line 2618 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].in) ); }
#line 10969 "Parser/parser.cc"
    break;

  case 640:
#line 2620 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newEnumInLine( *(yyvsp[0].decl)->type->symbolic.name ); }
#line 10975 "Parser/parser.cc"
    break;

  case 641:
#line 2622 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( DeclarationNode::newEnumValueGeneric( (yyvsp[-1].tok), (yyvsp[0].in) ) ); }
#line 10981 "Parser/parser.cc"
    break;

  case 642:
#line 2624 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( DeclarationNode::newEnumValueGeneric( new string("inline"), nullptr ) ); }
#line 10987 "Parser/parser.cc"
    break;

  case 644:
#line 2630 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.hide) = EnumHiding::Visible; }
#line 10993 "Parser/parser.cc"
    break;

  case 645:
#line 2635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 10999 "Parser/parser.cc"
    break;

  case 646:
#line 2636 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = new InitializerNode( (yyvsp[0].en) ); }
#line 11005 "Parser/parser.cc"
    break;

  case 647:
#line 2637 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                     { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 11011 "Parser/parser.cc"
    break;

  case 648:
#line 2644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newBasicType( DeclarationNode::Void ); }
#line 11017 "Parser/parser.cc"
    break;

  case 649:
#line 2646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11023 "Parser/parser.cc"
    break;

  case 652:
#line 2650 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11029 "Parser/parser.cc"
    break;

  case 653:
#line 2652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 11035 "Parser/parser.cc"
    break;

  case 654:
#line 2654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 11041 "Parser/parser.cc"
    break;

  case 656:
#line 2662 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11047 "Parser/parser.cc"
    break;

  case 657:
#line 2664 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11053 "Parser/parser.cc"
    break;

  case 658:
#line 2666 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-8].decl)->appendList( (yyvsp[-4].decl) )->appendList( (yyvsp[0].decl) ); }
#line 11059 "Parser/parser.cc"
    break;

  case 660:
#line 2672 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11065 "Parser/parser.cc"
    break;

  case 661:
#line 2677 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11071 "Parser/parser.cc"
    break;

  case 662:
#line 2679 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11077 "Parser/parser.cc"
    break;

  case 664:
#line 2682 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->addVarArgs(); }
#line 11083 "Parser/parser.cc"
    break;

  case 667:
#line 2689 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11089 "Parser/parser.cc"
    break;

  case 668:
#line 2691 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[0].decl) ); }
#line 11095 "Parser/parser.cc"
    break;

  case 670:
#line 2700 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 11101 "Parser/parser.cc"
    break;

  case 671:
#line 2703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) ); }
#line 11107 "Parser/parser.cc"
    break;

  case 672:
#line 2705 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addName( (yyvsp[-1].tok) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 11113 "Parser/parser.cc"
    break;

  case 677:
#line 2715 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11119 "Parser/parser.cc"
    break;

  case 679:
#line 2722 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 11125 "Parser/parser.cc"
    break;

  case 680:
#line 2724 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 11131 "Parser/parser.cc"
    break;

  case 681:
#line 2729 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 11137 "Parser/parser.cc"
    break;

  case 682:
#line 2731 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addType( (yyvsp[-2].decl) )->addInitializer( (yyvsp[0].en) ? new InitializerNode( (yyvsp[0].en) ) : nullptr ); }
#line 11143 "Parser/parser.cc"
    break;

  case 683:
#line 2740 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11149 "Parser/parser.cc"
    break;

  case 684:
#line 2742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( DeclarationNode::newName( (yyvsp[0].tok) ) ); }
#line 11155 "Parser/parser.cc"
    break;

  case 690:
#line 2755 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 11161 "Parser/parser.cc"
    break;

  case 693:
#line 2765 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 11167 "Parser/parser.cc"
    break;

  case 694:
#line 2766 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (yyvsp[-1].op) == OperKinds::Assign ? (yyvsp[0].in) : (yyvsp[0].in)->set_maybeConstructed( false ); }
#line 11173 "Parser/parser.cc"
    break;

  case 695:
#line 2767 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                                        { (yyval.in) = new InitializerNode( true ); }
#line 11179 "Parser/parser.cc"
    break;

  case 696:
#line 2768 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 11185 "Parser/parser.cc"
    break;

  case 697:
#line 2772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = new InitializerNode( (yyvsp[0].en) ); }
#line 11191 "Parser/parser.cc"
    break;

  case 698:
#line 2773 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = new InitializerNode( (yyvsp[-2].in), true ); }
#line 11197 "Parser/parser.cc"
    break;

  case 699:
#line 2778 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.in) = nullptr; }
#line 11203 "Parser/parser.cc"
    break;

  case 701:
#line 2780 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                                        { (yyval.in) = (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ); }
#line 11209 "Parser/parser.cc"
    break;

  case 702:
#line 2781 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                        { (yyval.in) = (InitializerNode *)( (yyvsp[-2].in)->set_last( (yyvsp[0].in) ) ); }
#line 11215 "Parser/parser.cc"
    break;

  case 703:
#line 2782 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                                                           { (yyval.in) = (InitializerNode *)((yyvsp[-3].in)->set_last( (yyvsp[0].in)->set_designators( (yyvsp[-1].en) ) )); }
#line 11221 "Parser/parser.cc"
    break;

  case 705:
#line 2798 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[-1].tok) ) ); }
#line 11227 "Parser/parser.cc"
    break;

  case 707:
#line 2804 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-1].en)->set_last( (yyvsp[0].en) )); }
#line 11233 "Parser/parser.cc"
    break;

  case 708:
#line 2810 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( build_varref( (yyvsp[0].tok) ) ); }
#line 11239 "Parser/parser.cc"
    break;

  case 709:
#line 2813 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 11245 "Parser/parser.cc"
    break;

  case 710:
#line 2815 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 11251 "Parser/parser.cc"
    break;

  case 711:
#line 2817 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-4].en) ), maybeMoveBuild<Expression>( (yyvsp[-2].en) ) ) ); }
#line 11257 "Parser/parser.cc"
    break;

  case 712:
#line 2819 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[-2].en); }
#line 11263 "Parser/parser.cc"
    break;

  case 714:
#line 2843 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl) ); }
#line 11269 "Parser/parser.cc"
    break;

  case 715:
#line 2848 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11275 "Parser/parser.cc"
    break;

  case 716:
#line 2850 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 11281 "Parser/parser.cc"
    break;

  case 717:
#line 2855 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[0].tok), TYPEDEFname, "9" );
			if ( (yyvsp[-1].tclass) == TypeDecl::Otype ) { SemanticError( yylloc, "otype keyword is deprecated, use T " ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Dtype ) { SemanticError( yylloc, "dtype keyword is deprecated, use T &" ); }
			if ( (yyvsp[-1].tclass) == TypeDecl::Ttype ) { SemanticError( yylloc, "ttype keyword is deprecated, use T ..." ); }
		}
#line 11292 "Parser/parser.cc"
    break;

  case 718:
#line 2862 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-4].tclass), (yyvsp[-3].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 11298 "Parser/parser.cc"
    break;

  case 719:
#line 2864 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDEFname, "9" ); }
#line 11304 "Parser/parser.cc"
    break;

  case 720:
#line 2866 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( (yyvsp[-3].tclass), (yyvsp[-4].tok) )->addTypeInitializer( (yyvsp[-1].decl) )->addAssertions( (yyvsp[0].decl) ); }
#line 11310 "Parser/parser.cc"
    break;

  case 721:
#line 2868 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToScope( *(yyvsp[-1].tok), TYPEDIMname, "9" );
			(yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dimension, (yyvsp[-1].tok) );
		}
#line 11319 "Parser/parser.cc"
    break;

  case 722:
#line 2874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTypeParam( TypeDecl::Dtype, new string( DeclarationNode::anonymous.newName() ) )->addAssertions( (yyvsp[0].decl) ); }
#line 11325 "Parser/parser.cc"
    break;

  case 723:
#line 2879 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 11331 "Parser/parser.cc"
    break;

  case 724:
#line 2881 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 11337 "Parser/parser.cc"
    break;

  case 725:
#line 2883 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::DStype; }
#line 11343 "Parser/parser.cc"
    break;

  case 726:
#line 2887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 11349 "Parser/parser.cc"
    break;

  case 727:
#line 2892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Otype; }
#line 11355 "Parser/parser.cc"
    break;

  case 728:
#line 2894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Dtype; }
#line 11361 "Parser/parser.cc"
    break;

  case 729:
#line 2896 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ftype; }
#line 11367 "Parser/parser.cc"
    break;

  case 730:
#line 2898 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tclass) = TypeDecl::Ttype; }
#line 11373 "Parser/parser.cc"
    break;

  case 731:
#line 2903 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11379 "Parser/parser.cc"
    break;

  case 734:
#line 2910 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->appendList( (yyvsp[0].decl) ); }
#line 11385 "Parser/parser.cc"
    break;

  case 735:
#line 2915 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTraitUse( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 11391 "Parser/parser.cc"
    break;

  case 736:
#line 2917 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 11397 "Parser/parser.cc"
    break;

  case 737:
#line 2924 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ); }
#line 11403 "Parser/parser.cc"
    break;

  case 739:
#line 2927 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)((yyvsp[-2].en)->set_last( new ExpressionNode( new TypeExpr( maybeMoveBuildType( (yyvsp[0].decl) ) ) ) )); }
#line 11409 "Parser/parser.cc"
    break;

  case 740:
#line 2929 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (ExpressionNode *)( (yyvsp[-2].en)->set_last( (yyvsp[0].en) )); }
#line 11415 "Parser/parser.cc"
    break;

  case 741:
#line 2934 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl); }
#line 11421 "Parser/parser.cc"
    break;

  case 742:
#line 2936 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11427 "Parser/parser.cc"
    break;

  case 743:
#line 2938 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->appendList( (yyvsp[0].decl)->copySpecifiers( (yyvsp[-2].decl) ) ); }
#line 11433 "Parser/parser.cc"
    break;

  case 744:
#line 2943 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addAssertions( (yyvsp[0].decl) ); }
#line 11439 "Parser/parser.cc"
    break;

  case 745:
#line 2945 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addAssertions( (yyvsp[-2].decl) )->addType( (yyvsp[0].decl) ); }
#line 11445 "Parser/parser.cc"
    break;

  case 746:
#line 2950 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[0].tok), TYPEDEFname, "10" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[0].tok), 0 );
		}
#line 11454 "Parser/parser.cc"
    break;

  case 747:
#line 2955 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			typedefTable.addToEnclosingScope( *(yyvsp[-3].tok), TYPEGENname, "11" );
			(yyval.decl) = DeclarationNode::newTypeDecl( (yyvsp[-3].tok), (yyvsp[-1].decl) );
		}
#line 11463 "Parser/parser.cc"
    break;

  case 748:
#line 2963 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-5].tok), (yyvsp[-3].decl), 0 ); }
#line 11469 "Parser/parser.cc"
    break;

  case 749:
#line 2965 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTrait( (yyvsp[-8].tok), (yyvsp[-6].decl), (yyvsp[-2].decl) ); }
#line 11475 "Parser/parser.cc"
    break;

  case 751:
#line 2971 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->appendList( (yyvsp[0].decl) ); }
#line 11481 "Parser/parser.cc"
    break;

  case 756:
#line 2983 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneType( (yyvsp[0].tok) ) ); }
#line 11487 "Parser/parser.cc"
    break;

  case 757:
#line 2988 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addType( (yyvsp[-1].decl) ); }
#line 11493 "Parser/parser.cc"
    break;

  case 758:
#line 2990 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-4].decl)->appendList( (yyvsp[-4].decl)->cloneBaseType( (yyvsp[0].decl) ) ); }
#line 11499 "Parser/parser.cc"
    break;

  case 760:
#line 2998 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { parseTree = parseTree ? parseTree->appendList( (yyvsp[0].decl) ) : (yyvsp[0].decl);	}
#line 11505 "Parser/parser.cc"
    break;

  case 761:
#line 3003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11511 "Parser/parser.cc"
    break;

  case 762:
#line 3005 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl) ? (yyvsp[-3].decl)->appendList( (yyvsp[-1].decl) ) : (yyvsp[-1].decl); }
#line 11517 "Parser/parser.cc"
    break;

  case 763:
#line 3010 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11523 "Parser/parser.cc"
    break;

  case 765:
#line 3015 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.up( forall ); forall = false; }
#line 11529 "Parser/parser.cc"
    break;

  case 766:
#line 3019 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { typedefTable.down(); }
#line 11535 "Parser/parser.cc"
    break;

  case 767:
#line 3024 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newDirectiveStmt( new StatementNode( build_directive( (yyvsp[0].tok) ) ) ); }
#line 11541 "Parser/parser.cc"
    break;

  case 769:
#line 3027 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeIdentifier( *(yyvsp[-1].tok).str, *(yyvsp[0].tok).str, " declaration" ); (yyval.decl) = nullptr; }
#line 11547 "Parser/parser.cc"
    break;

  case 770:
#line 3029 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type qualifier" ); (yyval.decl) = nullptr; }
#line 11553 "Parser/parser.cc"
    break;

  case 771:
#line 3031 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "storage class" ); (yyval.decl) = nullptr; }
#line 11559 "Parser/parser.cc"
    break;

  case 772:
#line 3033 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 11565 "Parser/parser.cc"
    break;

  case 773:
#line 3035 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 11571 "Parser/parser.cc"
    break;

  case 774:
#line 3037 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { IdentifierBeforeType( *(yyvsp[-1].tok).str, "type" ); (yyval.decl) = nullptr; }
#line 11577 "Parser/parser.cc"
    break;

  case 776:
#line 3040 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distExt( (yyvsp[0].decl) );								// mark all fields in list
			(yyval.decl) = (yyvsp[0].decl);
		}
#line 11586 "Parser/parser.cc"
    break;

  case 777:
#line 3045 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAsmStmt( new StatementNode( build_asm( false, (yyvsp[-2].constant), 0 ) ) ); }
#line 11592 "Parser/parser.cc"
    break;

  case 778:
#line 3047 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = LinkageSpec::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 11601 "Parser/parser.cc"
    break;

  case 779:
#line 3052 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-1].decl);
		}
#line 11611 "Parser/parser.cc"
    break;

  case 780:
#line 3058 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkageStack.push( linkage );				// handle nested extern "C"/"Cforall"
			linkage = LinkageSpec::update( yylloc, linkage, (yyvsp[0].tok) );
		}
#line 11620 "Parser/parser.cc"
    break;

  case 781:
#line 3063 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			linkage = linkageStack.top();
			linkageStack.pop();
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11630 "Parser/parser.cc"
    break;

  case 782:
#line 3070 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type->forall ) forall = true;		// remember generic type
		}
#line 11639 "Parser/parser.cc"
    break;

  case 783:
#line 3075 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11649 "Parser/parser.cc"
    break;

  case 784:
#line 3081 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.val ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( (yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall ) forall = true; // remember generic type
		}
#line 11658 "Parser/parser.cc"
    break;

  case 785:
#line 3086 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-6].decl) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11668 "Parser/parser.cc"
    break;

  case 786:
#line 3092 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->qualifiers.val) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->qualifiers.val) ) { SemanticError( yylloc, "CV qualifiers cannot be distributed; only storage-class and forall qualifiers." ); }
			if ( ((yyvsp[-1].decl)->type && (yyvsp[-1].decl)->type->forall) || ((yyvsp[0].decl)->type && (yyvsp[0].decl)->type->forall) ) forall = true; // remember generic type
		}
#line 11677 "Parser/parser.cc"
    break;

  case 787:
#line 3097 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			distQual( (yyvsp[-2].decl), (yyvsp[-7].decl)->addQualifiers( (yyvsp[-6].decl) ) );
 			forall = false;
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11687 "Parser/parser.cc"
    break;

  case 789:
#line 3112 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addFunctionBody( (yyvsp[0].sn) ); }
#line 11693 "Parser/parser.cc"
    break;

  case 790:
#line 3114 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addOldDeclList( (yyvsp[-1].decl) )->addFunctionBody( (yyvsp[0].sn) ); }
#line 11699 "Parser/parser.cc"
    break;

  case 791:
#line 3119 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; forall = false; }
#line 11705 "Parser/parser.cc"
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
#line 11717 "Parser/parser.cc"
    break;

  case 793:
#line 3132 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// Add the function body to the last identifier in the function definition list, i.e., foo3:
			//   [const double] foo1(), foo2( int ), foo3( double ) { return 3.0; }
			(yyvsp[-2].decl)->get_last()->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) );
			(yyval.decl) = (yyvsp[-2].decl);
		}
#line 11728 "Parser/parser.cc"
    break;

  case 794:
#line 3139 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 11737 "Parser/parser.cc"
    break;

  case 795:
#line 3144 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-3].decl), (yyvsp[-2].decl) );
			(yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-3].decl) );
		}
#line 11746 "Parser/parser.cc"
    break;

  case 796:
#line 3150 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 11752 "Parser/parser.cc"
    break;

  case 797:
#line 3153 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) ); }
#line 11758 "Parser/parser.cc"
    break;

  case 798:
#line 3156 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 11764 "Parser/parser.cc"
    break;

  case 799:
#line 3160 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			rebindForall( (yyvsp[-4].decl), (yyvsp[-3].decl) );
			(yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addType( (yyvsp[-4].decl) );
		}
#line 11773 "Parser/parser.cc"
    break;

  case 800:
#line 3166 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 11779 "Parser/parser.cc"
    break;

  case 801:
#line 3169 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) ); }
#line 11785 "Parser/parser.cc"
    break;

  case 802:
#line 3172 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addOldDeclList( (yyvsp[-2].decl) )->addFunctionBody( (yyvsp[0].sn), (yyvsp[-1].en) )->addQualifiers( (yyvsp[-4].decl) )->addQualifiers( (yyvsp[-5].decl) ); }
#line 11791 "Parser/parser.cc"
    break;

  case 806:
#line 3183 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = new ExpressionNode( new RangeExpr( maybeMoveBuild<Expression>( (yyvsp[-2].en) ), maybeMoveBuild<Expression>( (yyvsp[0].en) ) ) ); }
#line 11797 "Parser/parser.cc"
    break;

  case 807:
#line 3188 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11803 "Parser/parser.cc"
    break;

  case 808:
#line 3190 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			DeclarationNode * name = new DeclarationNode();
			name->asmName = (yyvsp[-2].constant);
			(yyval.decl) = name->addQualifiers( (yyvsp[0].decl) );
		}
#line 11813 "Parser/parser.cc"
    break;

  case 809:
#line 3199 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11819 "Parser/parser.cc"
    break;

  case 812:
#line 3206 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 11825 "Parser/parser.cc"
    break;

  case 813:
#line 3211 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl); }
#line 11831 "Parser/parser.cc"
    break;

  case 815:
#line 3217 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11837 "Parser/parser.cc"
    break;

  case 816:
#line 3222 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 11843 "Parser/parser.cc"
    break;

  case 817:
#line 3224 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[0].tok) ); }
#line 11849 "Parser/parser.cc"
    break;

  case 818:
#line 3226 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newAttribute( (yyvsp[-3].tok), (yyvsp[-1].en) ); }
#line 11855 "Parser/parser.cc"
    break;

  case 823:
#line 3235 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "fallthrough" ), { nullptr, -1 } }; }
#line 11861 "Parser/parser.cc"
    break;

  case 824:
#line 3237 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.tok) = Token{ new string( "__const__" ), { nullptr, -1 } }; }
#line 11867 "Parser/parser.cc"
    break;

  case 825:
#line 3272 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 11873 "Parser/parser.cc"
    break;

  case 826:
#line 3274 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11879 "Parser/parser.cc"
    break;

  case 827:
#line 3279 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11885 "Parser/parser.cc"
    break;

  case 829:
#line 3282 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11891 "Parser/parser.cc"
    break;

  case 830:
#line 3284 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11897 "Parser/parser.cc"
    break;

  case 831:
#line 3289 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 11903 "Parser/parser.cc"
    break;

  case 832:
#line 3291 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 11909 "Parser/parser.cc"
    break;

  case 833:
#line 3293 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11915 "Parser/parser.cc"
    break;

  case 834:
#line 3295 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 11921 "Parser/parser.cc"
    break;

  case 835:
#line 3300 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 11927 "Parser/parser.cc"
    break;

  case 836:
#line 3302 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11933 "Parser/parser.cc"
    break;

  case 837:
#line 3304 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11939 "Parser/parser.cc"
    break;

  case 838:
#line 3306 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 11945 "Parser/parser.cc"
    break;

  case 839:
#line 3308 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 11951 "Parser/parser.cc"
    break;

  case 840:
#line 3310 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11957 "Parser/parser.cc"
    break;

  case 841:
#line 3312 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11963 "Parser/parser.cc"
    break;

  case 842:
#line 3317 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 11969 "Parser/parser.cc"
    break;

  case 843:
#line 3319 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 11975 "Parser/parser.cc"
    break;

  case 844:
#line 3321 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 11981 "Parser/parser.cc"
    break;

  case 845:
#line 3323 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 11987 "Parser/parser.cc"
    break;

  case 846:
#line 3332 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11993 "Parser/parser.cc"
    break;

  case 848:
#line 3335 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 11999 "Parser/parser.cc"
    break;

  case 849:
#line 3340 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12005 "Parser/parser.cc"
    break;

  case 850:
#line 3342 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12011 "Parser/parser.cc"
    break;

  case 851:
#line 3344 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 12017 "Parser/parser.cc"
    break;

  case 852:
#line 3346 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12023 "Parser/parser.cc"
    break;

  case 853:
#line 3348 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12029 "Parser/parser.cc"
    break;

  case 854:
#line 3353 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12035 "Parser/parser.cc"
    break;

  case 855:
#line 3355 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12041 "Parser/parser.cc"
    break;

  case 856:
#line 3357 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12047 "Parser/parser.cc"
    break;

  case 857:
#line 3359 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12053 "Parser/parser.cc"
    break;

  case 858:
#line 3364 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12059 "Parser/parser.cc"
    break;

  case 859:
#line 3366 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12065 "Parser/parser.cc"
    break;

  case 860:
#line 3368 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12071 "Parser/parser.cc"
    break;

  case 861:
#line 3370 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12077 "Parser/parser.cc"
    break;

  case 862:
#line 3372 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12083 "Parser/parser.cc"
    break;

  case 863:
#line 3374 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12089 "Parser/parser.cc"
    break;

  case 867:
#line 3392 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-3].decl)->addIdList( (yyvsp[-1].decl) ); }
#line 12095 "Parser/parser.cc"
    break;

  case 868:
#line 3394 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12101 "Parser/parser.cc"
    break;

  case 869:
#line 3396 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 12107 "Parser/parser.cc"
    break;

  case 870:
#line 3398 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12113 "Parser/parser.cc"
    break;

  case 871:
#line 3400 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12119 "Parser/parser.cc"
    break;

  case 872:
#line 3405 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12125 "Parser/parser.cc"
    break;

  case 873:
#line 3407 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12131 "Parser/parser.cc"
    break;

  case 874:
#line 3409 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12137 "Parser/parser.cc"
    break;

  case 875:
#line 3411 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12143 "Parser/parser.cc"
    break;

  case 876:
#line 3416 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12149 "Parser/parser.cc"
    break;

  case 877:
#line 3418 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12155 "Parser/parser.cc"
    break;

  case 878:
#line 3420 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12161 "Parser/parser.cc"
    break;

  case 879:
#line 3422 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12167 "Parser/parser.cc"
    break;

  case 880:
#line 3424 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12173 "Parser/parser.cc"
    break;

  case 881:
#line 3426 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12179 "Parser/parser.cc"
    break;

  case 882:
#line 3441 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                {
			// hide type name in enclosing scope by variable name
			typedefTable.addToEnclosingScope( *(yyvsp[0].decl)->name, IDENTIFIER, "ID" );
		}
#line 12188 "Parser/parser.cc"
    break;

  case 883:
#line 3446 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12194 "Parser/parser.cc"
    break;

  case 884:
#line 3451 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12200 "Parser/parser.cc"
    break;

  case 886:
#line 3454 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12206 "Parser/parser.cc"
    break;

  case 887:
#line 3456 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12212 "Parser/parser.cc"
    break;

  case 888:
#line 3461 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12218 "Parser/parser.cc"
    break;

  case 889:
#line 3463 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12224 "Parser/parser.cc"
    break;

  case 890:
#line 3465 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12230 "Parser/parser.cc"
    break;

  case 891:
#line 3467 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12236 "Parser/parser.cc"
    break;

  case 892:
#line 3472 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12242 "Parser/parser.cc"
    break;

  case 893:
#line 3474 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12248 "Parser/parser.cc"
    break;

  case 894:
#line 3476 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12254 "Parser/parser.cc"
    break;

  case 895:
#line 3478 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12260 "Parser/parser.cc"
    break;

  case 896:
#line 3480 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[-3].decl) )->addArray( (yyvsp[0].decl) ); }
#line 12266 "Parser/parser.cc"
    break;

  case 897:
#line 3482 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12272 "Parser/parser.cc"
    break;

  case 898:
#line 3484 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12278 "Parser/parser.cc"
    break;

  case 899:
#line 3489 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12284 "Parser/parser.cc"
    break;

  case 900:
#line 3491 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12290 "Parser/parser.cc"
    break;

  case 901:
#line 3493 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addQualifiers( (yyvsp[-7].decl) )->addParamList( (yyvsp[-2].decl) ); }
#line 12296 "Parser/parser.cc"
    break;

  case 902:
#line 3495 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12302 "Parser/parser.cc"
    break;

  case 903:
#line 3497 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[-2].decl) ); }
#line 12308 "Parser/parser.cc"
    break;

  case 904:
#line 3507 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12314 "Parser/parser.cc"
    break;

  case 905:
#line 3509 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12320 "Parser/parser.cc"
    break;

  case 907:
#line 3512 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12326 "Parser/parser.cc"
    break;

  case 908:
#line 3514 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12332 "Parser/parser.cc"
    break;

  case 909:
#line 3519 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12338 "Parser/parser.cc"
    break;

  case 910:
#line 3521 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12344 "Parser/parser.cc"
    break;

  case 911:
#line 3523 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12350 "Parser/parser.cc"
    break;

  case 912:
#line 3528 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12356 "Parser/parser.cc"
    break;

  case 913:
#line 3530 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12362 "Parser/parser.cc"
    break;

  case 914:
#line 3532 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12368 "Parser/parser.cc"
    break;

  case 915:
#line 3534 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12374 "Parser/parser.cc"
    break;

  case 916:
#line 3539 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12380 "Parser/parser.cc"
    break;

  case 917:
#line 3541 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12386 "Parser/parser.cc"
    break;

  case 918:
#line 3543 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12392 "Parser/parser.cc"
    break;

  case 919:
#line 3557 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12398 "Parser/parser.cc"
    break;

  case 920:
#line 3559 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addPointer( DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf ) )->addQualifiers( (yyvsp[0].decl) ); }
#line 12404 "Parser/parser.cc"
    break;

  case 922:
#line 3562 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12410 "Parser/parser.cc"
    break;

  case 923:
#line 3564 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12416 "Parser/parser.cc"
    break;

  case 924:
#line 3569 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12422 "Parser/parser.cc"
    break;

  case 925:
#line 3571 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newName( (yyvsp[0].tok) ); }
#line 12428 "Parser/parser.cc"
    break;

  case 926:
#line 3576 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12434 "Parser/parser.cc"
    break;

  case 927:
#line 3578 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12440 "Parser/parser.cc"
    break;

  case 928:
#line 3580 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12446 "Parser/parser.cc"
    break;

  case 929:
#line 3585 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12452 "Parser/parser.cc"
    break;

  case 930:
#line 3587 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12458 "Parser/parser.cc"
    break;

  case 931:
#line 3592 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12464 "Parser/parser.cc"
    break;

  case 932:
#line 3594 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12470 "Parser/parser.cc"
    break;

  case 934:
#line 3612 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12476 "Parser/parser.cc"
    break;

  case 935:
#line 3614 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12482 "Parser/parser.cc"
    break;

  case 936:
#line 3619 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( 0, (yyvsp[0].op) ); }
#line 12488 "Parser/parser.cc"
    break;

  case 937:
#line 3621 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 12494 "Parser/parser.cc"
    break;

  case 938:
#line 3623 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12500 "Parser/parser.cc"
    break;

  case 939:
#line 3625 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12506 "Parser/parser.cc"
    break;

  case 940:
#line 3627 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12512 "Parser/parser.cc"
    break;

  case 942:
#line 3633 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12518 "Parser/parser.cc"
    break;

  case 943:
#line 3635 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12524 "Parser/parser.cc"
    break;

  case 944:
#line 3637 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12530 "Parser/parser.cc"
    break;

  case 945:
#line 3642 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 12536 "Parser/parser.cc"
    break;

  case 946:
#line 3644 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12542 "Parser/parser.cc"
    break;

  case 947:
#line 3646 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12548 "Parser/parser.cc"
    break;

  case 948:
#line 3652 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false ); }
#line 12554 "Parser/parser.cc"
    break;

  case 949:
#line 3654 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false )->addArray( (yyvsp[0].decl) ); }
#line 12560 "Parser/parser.cc"
    break;

  case 950:
#line 3656 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-4].en), 0, false )->addArray( DeclarationNode::newArray( (yyvsp[-1].en), 0, false ) ); }
#line 12566 "Parser/parser.cc"
    break;

  case 952:
#line 3663 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), 0, false ); }
#line 12572 "Parser/parser.cc"
    break;

  case 953:
#line 3665 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( 0 ); }
#line 12578 "Parser/parser.cc"
    break;

  case 954:
#line 3667 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newArray( (yyvsp[-2].en), 0, false ) ); }
#line 12584 "Parser/parser.cc"
    break;

  case 955:
#line 3669 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-5].decl)->addArray( DeclarationNode::newVarArray( 0 ) ); }
#line 12590 "Parser/parser.cc"
    break;

  case 956:
#line 3703 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = nullptr; }
#line 12596 "Parser/parser.cc"
    break;

  case 959:
#line 3710 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( DeclarationNode::newTypeQualifier( Type::Mutex ), OperKinds::AddressOf )->addQualifiers( (yyvsp[0].decl) ); }
#line 12602 "Parser/parser.cc"
    break;

  case 960:
#line 3712 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12608 "Parser/parser.cc"
    break;

  case 961:
#line 3714 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12614 "Parser/parser.cc"
    break;

  case 962:
#line 3719 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( nullptr, (yyvsp[0].op) ); }
#line 12620 "Parser/parser.cc"
    break;

  case 963:
#line 3721 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 12626 "Parser/parser.cc"
    break;

  case 964:
#line 3723 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( nullptr, (yyvsp[-1].op) ) ); }
#line 12632 "Parser/parser.cc"
    break;

  case 965:
#line 3725 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12638 "Parser/parser.cc"
    break;

  case 966:
#line 3727 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12644 "Parser/parser.cc"
    break;

  case 968:
#line 3733 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12650 "Parser/parser.cc"
    break;

  case 969:
#line 3735 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12656 "Parser/parser.cc"
    break;

  case 970:
#line 3737 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12662 "Parser/parser.cc"
    break;

  case 971:
#line 3742 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, nullptr, (yyvsp[-2].decl), nullptr ); }
#line 12668 "Parser/parser.cc"
    break;

  case 972:
#line 3744 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12674 "Parser/parser.cc"
    break;

  case 973:
#line 3746 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12680 "Parser/parser.cc"
    break;

  case 975:
#line 3753 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addArray( (yyvsp[0].decl) ); }
#line 12686 "Parser/parser.cc"
    break;

  case 977:
#line 3764 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, 0, false ); }
#line 12692 "Parser/parser.cc"
    break;

  case 978:
#line 3767 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 12698 "Parser/parser.cc"
    break;

  case 979:
#line 3769 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( 0, (yyvsp[-2].decl), false ); }
#line 12704 "Parser/parser.cc"
    break;

  case 980:
#line 3772 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 12710 "Parser/parser.cc"
    break;

  case 981:
#line 3774 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 12716 "Parser/parser.cc"
    break;

  case 982:
#line 3776 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-4].decl), true ); }
#line 12722 "Parser/parser.cc"
    break;

  case 984:
#line 3791 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12728 "Parser/parser.cc"
    break;

  case 985:
#line 3793 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12734 "Parser/parser.cc"
    break;

  case 986:
#line 3798 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( 0, (yyvsp[0].op) ); }
#line 12740 "Parser/parser.cc"
    break;

  case 987:
#line 3800 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newPointer( (yyvsp[0].decl), (yyvsp[-1].op) ); }
#line 12746 "Parser/parser.cc"
    break;

  case 988:
#line 3802 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12752 "Parser/parser.cc"
    break;

  case 989:
#line 3804 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addPointer( DeclarationNode::newPointer( (yyvsp[-1].decl), (yyvsp[-2].op) ) ); }
#line 12758 "Parser/parser.cc"
    break;

  case 990:
#line 3806 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addQualifiers( (yyvsp[0].decl) ); }
#line 12764 "Parser/parser.cc"
    break;

  case 992:
#line 3812 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12770 "Parser/parser.cc"
    break;

  case 993:
#line 3814 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-2].decl)->addArray( (yyvsp[0].decl) ); }
#line 12776 "Parser/parser.cc"
    break;

  case 994:
#line 3816 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12782 "Parser/parser.cc"
    break;

  case 995:
#line 3821 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-6].decl)->addParamList( (yyvsp[-2].decl) ); }
#line 12788 "Parser/parser.cc"
    break;

  case 996:
#line 3823 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[-1].decl); }
#line 12794 "Parser/parser.cc"
    break;

  case 999:
#line 3833 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12800 "Parser/parser.cc"
    break;

  case 1002:
#line 3844 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12806 "Parser/parser.cc"
    break;

  case 1003:
#line 3846 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12812 "Parser/parser.cc"
    break;

  case 1004:
#line 3848 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12818 "Parser/parser.cc"
    break;

  case 1005:
#line 3850 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12824 "Parser/parser.cc"
    break;

  case 1006:
#line 3852 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12830 "Parser/parser.cc"
    break;

  case 1007:
#line 3854 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12836 "Parser/parser.cc"
    break;

  case 1008:
#line 3861 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 12842 "Parser/parser.cc"
    break;

  case 1009:
#line 3863 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12848 "Parser/parser.cc"
    break;

  case 1010:
#line 3865 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 12854 "Parser/parser.cc"
    break;

  case 1011:
#line 3867 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 12860 "Parser/parser.cc"
    break;

  case 1012:
#line 3869 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12866 "Parser/parser.cc"
    break;

  case 1013:
#line 3872 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 12872 "Parser/parser.cc"
    break;

  case 1014:
#line 3874 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12878 "Parser/parser.cc"
    break;

  case 1015:
#line 3876 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( 0, 0, false ) ); }
#line 12884 "Parser/parser.cc"
    break;

  case 1016:
#line 3878 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( (yyvsp[-2].decl) ); }
#line 12890 "Parser/parser.cc"
    break;

  case 1017:
#line 3880 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12896 "Parser/parser.cc"
    break;

  case 1018:
#line 3885 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newVarArray( (yyvsp[-3].decl) ); }
#line 12902 "Parser/parser.cc"
    break;

  case 1019:
#line 3887 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), false ); }
#line 12908 "Parser/parser.cc"
    break;

  case 1020:
#line 3892 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl), true ); }
#line 12914 "Parser/parser.cc"
    break;

  case 1021:
#line 3894 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newArray( (yyvsp[-2].en), (yyvsp[-3].decl)->addQualifiers( (yyvsp[-4].decl) ), true ); }
#line 12920 "Parser/parser.cc"
    break;

  case 1023:
#line 3921 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addQualifiers( (yyvsp[-1].decl) ); }
#line 12926 "Parser/parser.cc"
    break;

  case 1027:
#line 3932 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12932 "Parser/parser.cc"
    break;

  case 1028:
#line 3934 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12938 "Parser/parser.cc"
    break;

  case 1029:
#line 3936 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12944 "Parser/parser.cc"
    break;

  case 1030:
#line 3938 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12950 "Parser/parser.cc"
    break;

  case 1031:
#line 3940 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( 0, (yyvsp[-1].op) ) ); }
#line 12956 "Parser/parser.cc"
    break;

  case 1032:
#line 3942 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewPointer( DeclarationNode::newPointer( (yyvsp[-2].decl), (yyvsp[-1].op) ) ); }
#line 12962 "Parser/parser.cc"
    break;

  case 1033:
#line 3949 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12968 "Parser/parser.cc"
    break;

  case 1034:
#line 3951 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12974 "Parser/parser.cc"
    break;

  case 1035:
#line 3953 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12980 "Parser/parser.cc"
    break;

  case 1036:
#line 3955 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12986 "Parser/parser.cc"
    break;

  case 1037:
#line 3957 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) )->addNewArray( DeclarationNode::newArray( nullptr, nullptr, false ) ); }
#line 12992 "Parser/parser.cc"
    break;

  case 1038:
#line 3959 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = (yyvsp[0].decl)->addNewArray( (yyvsp[-1].decl) ); }
#line 12998 "Parser/parser.cc"
    break;

  case 1039:
#line 3964 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newTuple( (yyvsp[-2].decl) ); }
#line 13004 "Parser/parser.cc"
    break;

  case 1040:
#line 3966 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 13010 "Parser/parser.cc"
    break;

  case 1041:
#line 3968 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { SemanticError( yylloc, "Tuple array currently unimplemented." ); (yyval.decl) = nullptr; }
#line 13016 "Parser/parser.cc"
    break;

  case 1042:
#line 3975 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 13022 "Parser/parser.cc"
    break;

  case 1043:
#line 3977 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.decl) = DeclarationNode::newFunction( nullptr, (yyvsp[-5].decl), (yyvsp[-2].decl), nullptr ); }
#line 13028 "Parser/parser.cc"
    break;

  case 1046:
#line 4001 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = nullptr; }
#line 13034 "Parser/parser.cc"
    break;

  case 1047:
#line 4003 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"
                { (yyval.en) = (yyvsp[0].en); }
#line 13040 "Parser/parser.cc"
    break;


#line 13044 "Parser/parser.cc"

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
#line 4006 "/var/lib/jenkins/workspace/Cforall_Distribute_Ref/src/Parser/parser.yy"


// ----end of grammar----

// Local Variables: //
// mode: c++ //
// tab-width: 4 //
// compile-command: "make install" //
// End: //
